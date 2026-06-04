;;; flex-x.el --- Extended flex completion style -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nobu43
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/nobu43/flex-x

;;; Commentary:

;; flex-x adds a completion style based on the built-in `flex' style.
;;
;; Features:
;;
;; - Space-separated AND filtering.
;; - Sorting by minibuffer history and flex score.
;; - Whole-candidate highlighting for high-score matches.
;; - Optional regexp expanders for non-ASCII candidates, such as migemo or pyim.
;;
;; Add `flex-x' to `completion-styles' to enable it:
;;
;;   (add-to-list 'completion-styles 'flex-x)

;;; Code:

(require 'cl-lib)
(require 'minibuffer)
(require 'subr-x)

(declare-function completion--flex-cost "minibuffer"
                  (pat str &optional dont-error))
(declare-function completion--flex-score "minibuffer"
                  (str regexp &optional dont-error))
(declare-function completion-flex--make-flex-pattern "minibuffer"
                  (pattern))
(declare-function completion-lazy-hilit-p "minibuffer")
(declare-function completion-pcm--filename-try-filter "minibuffer"
                  (all))

(defvar completion-lazy-hilit)
(defvar completion-lazy-hilit-fn)
(defvar corfu-history)
(defvar corfu-history-decay)
(defvar corfu-history-duplicate)
(defvar corfu-history-mode)

(defgroup flex-x nil
  "Extended flex completion style."
  :group 'minibuffer
  :prefix "flex-x-")

(defface flex-x-highlight
  '((t :weight bold :underline t))
  "Face used to highlight high-score flex-x candidates.

This face intentionally does not set foreground or background colors."
  :group 'flex-x)

(defcustom flex-x-highlight-score-threshold 0.05
  "Minimum score required to highlight the whole candidate.

Candidates below this score keep their ordinary completion faces, but
still keep `completion-score' and `flex-x-score' text properties.

The default is intentionally low because built-in flex scores are
divided by candidate length, so good acronym-style matches often score
around 0.05."
  :type 'number
  :group 'flex-x)

(defcustom flex-x-split-regexp "[[:space:]]+"
  "Regexp used to split input into flex-x search terms."
  :type 'regexp
  :group 'flex-x)

(defcustom flex-x-sort-by-history t
  "Non-nil means prefer candidates found in completion history.

In minibuffer completion, this uses `minibuffer-history-variable' and
`minibuffer-completion-base', matching the behavior expected by
Emacs completion metadata.

When `corfu-history-mode' is enabled outside the minibuffer, this uses
`corfu-history' instead."
  :type 'boolean
  :group 'flex-x)

(defcustom flex-x-sort-by-score t
  "Non-nil means sort candidates by flex-x match quality.

When Emacs provides flex cost information, lower cost is preferred
before the normalized flex-x score."
  :type 'boolean
  :group 'flex-x)

(defcustom flex-x-extra-match-functions nil
  "Extra functions used when built-in flex does not match.

Each function is called with two arguments: the search term and the
candidate string.  It should return nil for no match, t for a match
with `flex-x-extra-match-score', a number to use as the score, or a
plist with `:score' and optional `:ranges'.

`:ranges' should be a list of (BEG . END) ranges in the candidate.
These ranges are highlighted with `completions-common-part'."
  :type '(repeat function)
  :group 'flex-x)

(defcustom flex-x-extra-pattern-function nil
  "Function or function names used to build extra regexps from search terms.

The value may be nil, a function object, a function name symbol, or a
list of those values.  Each function receives one argument, the search
term, and should return a regexp string or nil.  This is suitable for
functions like `migemo-get-pattern' or `pyim-cregexp-build'.

The regexp is used only when built-in flex does not match.  By default
this extra regexp matching is limited to non-ASCII candidates; see
`flex-x-extra-match-nonascii-only'."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Function")
                 (symbol :tag "Function name")
                 (repeat :tag "Functions"
                         (choice (function :tag "Function")
                                 (symbol :tag "Function name"))))
  :group 'flex-x)

(defcustom flex-x-extra-match-nonascii-only t
  "Non-nil means use extra matching only for non-ASCII candidates."
  :type 'boolean
  :group 'flex-x)

(defcustom flex-x-extra-match-score 0.1
  "Default score used when an extra matcher returns t."
  :type 'number
  :group 'flex-x)

(defcustom flex-x-extra-match-candidate-limit 5000
  "Maximum number of prefix candidates scanned by extra matchers.

When extra matchers are configured, flex-x may need to inspect
candidates that did not match built-in flex.  If the number of
candidates returned for the current completion prefix is greater than
this value, flex-x skips that full-table extra scan and keeps only
built-in flex matches.

Set this to nil to allow scanning every prefix candidate."
  :type '(choice (const :tag "No limit" nil)
                 (natnum :tag "Maximum candidates"))
  :group 'flex-x)

(defvar flex-x--last-match-context nil
  "Last match context used by `flex-x-all-completions'.")

(defconst flex-x--dedupe-ignored-properties
  '(completion-score
    flex-cost
    flex-matches
    flex-x--match
    flex-x--seed-term
    flex-x-cost
    flex-x-score)
  "Text properties ignored when detecting duplicate flex-x candidates.")

(defconst flex-x--metadata-properties
  '(display-sort-function
    cycle-sort-function
    flex-x--adjusted-metadata
    flex-x--match-context-cell
    flex-x--original-display-sort-function
    flex-x--original-cycle-sort-function)
  "Metadata properties managed by flex-x.")

(cl-defstruct flex-x--context
  prefix
  suffix
  field
  field-point
  terms)

(cl-defstruct flex-x--match-context
  terms
  prefix
  extra-pattern-cache
  flex-regexp-cache)

(defun flex-x--string-nonascii-p (string)
  "Return non-nil if STRING has at least one non-ASCII character."
  (cl-loop for char across string thereis (> char 127)))

(defun flex-x--split (string)
  "Split STRING into non-empty flex-x search terms."
  (split-string string flex-x-split-regexp t))

(defun flex-x--context (string table pred point)
  "Return completion context for STRING, TABLE, PRED and POINT."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (start (or (car-safe bounds) 0))
         (end (or (cdr-safe bounds) (length afterpoint)))
         (field-before (substring beforepoint start))
         (field-after (substring afterpoint 0 end)))
    (make-flex-x--context
     :prefix (substring beforepoint 0 start)
     :suffix (substring afterpoint end)
     :field (concat field-before field-after)
     :field-point (length field-before)
     :terms (flex-x--split (concat field-before field-after)))))

(defun flex-x--context-completion-string (context candidate)
  "Return full completion string for CANDIDATE in CONTEXT."
  (concat (flex-x--context-prefix context)
          (substring-no-properties candidate)
          (flex-x--context-suffix context)))

(defun flex-x--context-completion-point (context candidate)
  "Return point position after completing CANDIDATE in CONTEXT."
  (+ (length (flex-x--context-prefix context))
     (length candidate)))

(defun flex-x--candidate-target (candidate)
  "Return the unquoted string to match for CANDIDATE."
  (or (and (> (length candidate) 0)
           (get-text-property 0 'completion--unquoted candidate))
      (substring-no-properties candidate)))

(defun flex-x--completion-list-parts (completions)
  "Return (ITEMS . BASE-SIZE) from dotted completion list COMPLETIONS."
  (let ((items nil)
        (cell completions))
    (while (consp cell)
      (push (car cell) items)
      (setq cell (cdr cell)))
    (cons (nreverse items) cell)))

(defun flex-x--candidate-dedupe-copy (candidate)
  "Return a copy of CANDIDATE normalized for duplicate detection."
  (let ((copy (copy-sequence candidate)))
    (remove-list-of-text-properties 0 (length copy)
                                    flex-x--dedupe-ignored-properties
                                    copy)
    copy))

(defun flex-x--same-candidate-p (a b)
  "Return non-nil if A and B represent the same completion candidate."
  (and (equal (flex-x--candidate-target a)
              (flex-x--candidate-target b))
       (equal-including-properties (flex-x--candidate-dedupe-copy a)
                                   (flex-x--candidate-dedupe-copy b))))

(defun flex-x--delete-duplicate-candidates (candidates)
  "Remove duplicate CANDIDATES while preserving order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (candidate candidates)
      (let ((key (flex-x--candidate-target candidate)))
        (unless (cl-some (lambda (seen-candidate)
                           (flex-x--same-candidate-p candidate
                                                     seen-candidate))
                         (gethash key seen))
          (push candidate (gethash key seen))
          (push candidate result))))
    (nreverse result)))

(defun flex-x--flex-pattern (term)
  "Return a PCM flex pattern for TERM."
  (completion-pcm--optimize-pattern
   (completion-flex--make-flex-pattern
    (list 'prefix term))))

(defun flex-x--flex-regexp (term)
  "Return a grouped flex regexp for TERM."
  (completion-pcm--pattern->regex (flex-x--flex-pattern term) 'group))

(defun flex-x--make-flex-regexp-cache ()
  "Return a cache for flex regexps."
  (make-hash-table :test #'equal))

(defun flex-x--cached-flex-regexp (term cache)
  "Return cached flex regexp for TERM using CACHE."
  (if cache
      (let ((cached (gethash term cache 'flex-x--missing)))
        (if (not (eq cached 'flex-x--missing))
            cached
          (puthash term (flex-x--flex-regexp term) cache)))
    (flex-x--flex-regexp term)))

(defun flex-x--flex-cost-p ()
  "Return non-nil when this Emacs provides flex cost matching."
  (fboundp 'completion--flex-cost))

(defun flex-x--lazy-hilit-p ()
  "Return non-nil if completion frontend supports lazy highlighting."
  (if (fboundp 'completion-lazy-hilit-p)
      (completion-lazy-hilit-p)
    (and (boundp 'completion-lazy-hilit)
         completion-lazy-hilit)))

(defun flex-x--score-from-cost (cost candidate)
  "Return a score-like quality value from COST for CANDIDATE."
  (/ 1.0 (+ 1.0 (/ (float cost) (max 1 (length candidate))))))

(defun flex-x--remember-seed-flex-match (candidate term)
  "Return CANDIDATE annotated as a built-in flex match for TERM."
  (if (and (> (length candidate) 0)
           (get-text-property 0 'flex-cost candidate))
      (let ((copy (copy-sequence candidate)))
        (put-text-property 0 1 'flex-x--seed-term term copy)
        copy)
    candidate))

(defun flex-x--precomputed-flex-match (term candidate target)
  "Return precomputed flex match for TERM, CANDIDATE and TARGET."
  (when (and (> (length candidate) 0)
             (equal (get-text-property 0 'flex-x--seed-term candidate)
                    term))
    (let ((cost (get-text-property 0 'flex-cost candidate))
          (matches (get-text-property 0 'flex-matches candidate)))
      (when (numberp cost)
        (list :score (flex-x--score-from-cost cost target)
              :cost cost
              :matches matches)))))

(defun flex-x--normalize-extra-match (value)
  "Normalize an extra matcher return VALUE to a plist."
  (cond
   ((null value) nil)
   ((numberp value) (list :score value))
   ((and (listp value) (plist-member value :score)) value)
   (t (list :score flex-x-extra-match-score))))

(defun flex-x--extra-pattern-functions ()
  "Return configured extra pattern functions as a list."
  (cond
   ((null flex-x-extra-pattern-function) nil)
   ((functionp flex-x-extra-pattern-function)
    (list flex-x-extra-pattern-function))
   ((symbolp flex-x-extra-pattern-function)
    (list flex-x-extra-pattern-function))
   ((listp flex-x-extra-pattern-function)
    flex-x-extra-pattern-function)))

(defun flex-x--extra-matchers-p ()
  "Return non-nil if any extra matcher is configured."
  (or flex-x-extra-match-functions
      (flex-x--extra-pattern-functions)))

(defun flex-x--extra-match-allowed-p (candidate)
  "Return non-nil when extra matchers may run for CANDIDATE."
  (and (flex-x--extra-matchers-p)
       (or (not flex-x-extra-match-nonascii-only)
           (flex-x--string-nonascii-p candidate))))

(defun flex-x--extra-function-match (term candidate)
  "Return custom function match information for TERM and CANDIDATE."
  (when flex-x-extra-match-functions
    (cl-loop for fn in flex-x-extra-match-functions
             for value = (ignore-errors (funcall fn term candidate))
             for match = (flex-x--normalize-extra-match value)
             when match return match)))

(defun flex-x--extra-pattern (function term)
  "Return regexp from FUNCTION for TERM, or nil."
  (when (functionp function)
    (let ((regexp (ignore-errors (funcall function term))))
      (when (and (stringp regexp)
                 (not (string-empty-p regexp)))
        (condition-case nil
            (progn
              (string-match-p regexp "")
              regexp)
          (invalid-regexp nil))))))

(defun flex-x--make-extra-pattern-cache ()
  "Return a cache for extra regexp patterns, or nil."
  (when (flex-x--extra-pattern-functions)
    (make-hash-table :test #'equal)))

(defun flex-x--make-match-context (terms prefix)
  "Return a match context for TERMS and PREFIX."
  (make-flex-x--match-context
   :terms terms
   :prefix prefix
   :extra-pattern-cache (flex-x--make-extra-pattern-cache)
   :flex-regexp-cache (flex-x--make-flex-regexp-cache)))

(defun flex-x--extra-patterns (term)
  "Return valid extra regexps for TERM."
  (cl-loop for function in (flex-x--extra-pattern-functions)
           for regexp = (flex-x--extra-pattern function term)
           when regexp collect regexp))

(defun flex-x--cached-extra-patterns (term cache)
  "Return cached extra regexps for TERM using CACHE."
  (if cache
      (let ((cached (gethash term cache 'flex-x--missing)))
        (if (not (eq cached 'flex-x--missing))
            cached
          (puthash term (flex-x--extra-patterns term) cache)))
    (flex-x--extra-patterns term)))

(defun flex-x--extra-pattern-match-1 (regexp candidate)
  "Return regexp match information for REGEXP against CANDIDATE."
  (let ((case-fold-search completion-ignore-case))
    (when (string-match regexp candidate)
      (list :score flex-x-extra-match-score
            :ranges (list (cons (match-beginning 0) (match-end 0)))))))

(defun flex-x--extra-pattern-match (term candidate pattern-cache)
  "Return extra regexp match information for TERM and CANDIDATE.

PATTERN-CACHE stores generated regexps by search term."
  (cl-loop for regexp in (flex-x--cached-extra-patterns term pattern-cache)
           for match = (flex-x--extra-pattern-match-1 regexp candidate)
           when match return match))

(defun flex-x--extra-match (term candidate pattern-cache)
  "Return extra match information for TERM and CANDIDATE."
  (when (flex-x--extra-match-allowed-p candidate)
    (or (flex-x--extra-function-match term candidate)
        (flex-x--extra-pattern-match term candidate pattern-cache))))

(defun flex-x--match-term (term candidate &optional pattern-cache flex-regexp-cache)
  "Return match information when TERM matches CANDIDATE."
  (let ((target (flex-x--candidate-target candidate)))
    (if (flex-x--flex-cost-p)
        (or (flex-x--precomputed-flex-match term candidate target)
            (if-let* ((cost-match (funcall 'completion--flex-cost
                                           term target t)))
                (let ((cost (car cost-match))
                      (matches (cdr cost-match)))
                  (list :score (flex-x--score-from-cost cost target)
                        :cost cost
                        :matches matches))
              (flex-x--extra-match term target pattern-cache)))
      (let* ((regexp (flex-x--cached-flex-regexp term flex-regexp-cache))
             (score (completion--flex-score target regexp t)))
        (if score
            (list :score score :regexp regexp)
          (flex-x--extra-match term target pattern-cache))))))

(defun flex-x--match-candidate (candidate terms &optional
                                          pattern-cache flex-regexp-cache)
  "Return aggregate match information for CANDIDATE and TERMS."
  (when terms
    (let ((term-count (length terms))
          (score 0.0)
          (cost 0.0)
          (cost-count 0)
          (matches nil))
      (catch 'failed
        (dolist (term terms)
          (let ((match (flex-x--match-term term candidate pattern-cache
                                           flex-regexp-cache)))
            (unless match
              (throw 'failed nil))
            (cl-incf score (or (plist-get match :score) 0.0))
            (when-let ((match-cost (plist-get match :cost)))
              (cl-incf cost match-cost)
              (cl-incf cost-count))
            (push match matches)))
        (list :score (/ score term-count)
              :cost (and (= cost-count term-count)
                         (> cost-count 0)
                         (/ cost cost-count))
              :matches (nreverse matches))))))

(defun flex-x--highlight-ranges (candidate ranges)
  "Highlight RANGES in CANDIDATE."
  (dolist (range ranges)
    (let ((beg (car range))
          (end (cdr range)))
      (when (and (integerp beg)
                 (integerp end)
                 (<= 0 beg)
                 (<= beg end)
                 (<= end (length candidate)))
        (add-face-text-property beg end 'completions-common-part nil
                                candidate)))))

(defun flex-x--highlight-matches (candidate matches)
  "Highlight flex MATCHES in CANDIDATE."
  (let (last-match)
    (dolist (pos matches)
      (when (and (integerp pos)
                 (<= 0 pos)
                 (< pos (length candidate)))
        (setq last-match pos)
        (add-face-text-property pos (1+ pos) 'completions-common-part
                                nil candidate)))
    (when (and last-match
               (< (1+ last-match) (length candidate)))
      (add-face-text-property (1+ last-match) (+ 2 last-match)
                              'completions-first-difference
                              nil candidate))))

(defun flex-x--common-candidate-prefix (candidates)
  "Return the common plain-string prefix of CANDIDATES."
  (let ((strings (mapcar #'substring-no-properties candidates)))
    (and strings
         (try-completion "" strings))))

(defun flex-x--try-completion-from-candidates (string point context candidates)
  "Return a try-completion result for CANDIDATES in CONTEXT."
  (cond
   ((null candidates) nil)
   ((= (length candidates) 1)
    (let* ((candidate (car candidates))
           (completion
            (flex-x--context-completion-string context candidate)))
      (if (string-equal completion string)
          t
        (cons completion
              (flex-x--context-completion-point context candidate)))))
   (t
    (let ((common (flex-x--common-candidate-prefix candidates)))
      (if (and (stringp common)
               (> (length common)
                  (length (flex-x--context-field context))))
          (cons (flex-x--context-completion-string context common)
                (flex-x--context-completion-point context common))
        (cons string point))))))

(defun flex-x--highlight-candidate-p (score)
  "Return non-nil if SCORE should highlight the whole candidate."
  (>= score flex-x-highlight-score-threshold))

(defun flex-x--apply-match-faces (candidate match)
  "Destructively apply faces to CANDIDATE using MATCH."
  (let ((score (or (plist-get match :score) 0.0)))
    (dolist (term-match (plist-get match :matches))
      (when-let ((regexp (plist-get term-match :regexp)))
        (completion--hilit-from-re candidate regexp))
      (when-let ((matches (plist-get term-match :matches)))
        (flex-x--highlight-matches candidate matches))
      (when-let ((ranges (plist-get term-match :ranges)))
        (flex-x--highlight-ranges candidate ranges)))
    (when (and (> (length candidate) 0)
               (flex-x--highlight-candidate-p score))
      (add-face-text-property 0 (length candidate) 'flex-x-highlight
                              t candidate))
    candidate))

(defun flex-x--lazy-hilit-candidate (candidate)
  "Destructively highlight CANDIDATE using its stored flex-x match."
  (if-let ((match (and (> (length candidate) 0)
                       (get-text-property 0 'flex-x--match candidate))))
      (flex-x--apply-match-faces candidate match)
    candidate))

(defun flex-x--propertize-candidate (candidate match)
  "Return CANDIDATE propertized using MATCH."
  (let* ((copy (copy-sequence candidate))
         (score (or (plist-get match :score) 0.0))
         (lazy-hilit (flex-x--lazy-hilit-p))
         flex-matches)
    (dolist (term-match (plist-get match :matches))
      (when-let ((matches (plist-get term-match :matches)))
        (setq flex-matches (append flex-matches matches))))
    (when (> (length copy) 0)
      (when lazy-hilit
        (put-text-property 0 1 'flex-x--match match copy))
      (put-text-property 0 1 'completion-score score copy)
      (put-text-property 0 1 'flex-x-score score copy)
      (when flex-matches
        (put-text-property 0 1 'flex-matches flex-matches copy))
      (when-let ((cost (plist-get match :cost)))
        (put-text-property 0 1 'flex-cost cost copy)
        (put-text-property 0 1 'flex-x-cost cost copy)))
    (if lazy-hilit
        copy
      (flex-x--apply-match-faces copy match))))

(defun flex-x--all-table-candidates (context table pred)
  "Return all candidates in TABLE using CONTEXT and PRED."
  (let ((candidates (all-completions (flex-x--context-prefix context)
                                     table
                                     pred)))
    (if minibuffer-completing-file-name
        (completion-pcm--filename-try-filter candidates)
      candidates)))

(defun flex-x--limit-candidate-key (candidate)
  "Return key used for extra scan candidate-limit accounting."
  (cond
   ((stringp candidate)
    (substring-no-properties candidate))
   ((and (consp candidate)
         (stringp (car candidate)))
    (substring-no-properties (car candidate)))
   (t candidate)))

(defun flex-x--unique-candidate-count (candidates)
  "Return the number of unique CANDIDATES for candidate-limit accounting."
  (let ((seen (make-hash-table :test #'equal))
        (count 0))
    (dolist (candidate candidates count)
      (let ((key (flex-x--limit-candidate-key candidate)))
        (unless (gethash key seen)
          (puthash key t seen)
          (cl-incf count))))))

(defun flex-x--extra-candidate-scan-allowed-p (candidates)
  "Return non-nil if extra matchers may scan CANDIDATES."
  (let ((limit flex-x-extra-match-candidate-limit))
    (or (null limit)
        (and (integerp limit)
             (>= limit 0)
             (<= (flex-x--unique-candidate-count candidates) limit)))))

(defun flex-x--candidate-limit-predicate (pred limit too-many)
  "Return predicate combining PRED with LIMIT.

TOO-MANY is a cons cell whose car is set to non-nil once more than
LIMIT unique candidates have been accepted."
  (let ((count 0)
        (seen (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (and (or (null pred) (apply pred args))
           (let* ((candidate (car args))
                  (key (flex-x--limit-candidate-key candidate)))
             (unless (gethash key seen)
               (puthash key t seen)
               (cl-incf count))
             (if (> count limit)
                 (progn
                   (setcar too-many t)
                   nil)
               t))))))

(defun flex-x--extra-table-candidates (context table pred)
  "Return candidates for full-table extra matching, or nil if too many."
  (let ((limit flex-x-extra-match-candidate-limit))
    (cond
     ((null limit)
      (flex-x--all-table-candidates context table pred))
     (minibuffer-completing-file-name
      (let ((candidates (flex-x--all-table-candidates context table pred)))
        (when (flex-x--extra-candidate-scan-allowed-p candidates)
          candidates)))
     ((and (integerp limit) (>= limit 0))
      (let* ((too-many (cons nil nil))
             (limited-pred (flex-x--candidate-limit-predicate
                            pred limit too-many))
             (candidates (flex-x--all-table-candidates
                          context table limited-pred)))
        (when (and (not (car too-many))
                   (flex-x--extra-candidate-scan-allowed-p candidates))
          candidates))))))

(defun flex-x--builtin-flex-candidates (context table pred term)
  "Return built-in flex candidates for TERM using CONTEXT, TABLE and PRED."
  (let* ((input (concat (flex-x--context-prefix context) term))
         (result (completion-flex-all-completions
                  input table pred (length input)))
         (parts (flex-x--completion-list-parts result)))
    (cons (mapcar (lambda (candidate)
                    (flex-x--remember-seed-flex-match candidate term))
                  (car parts))
          (or (cdr parts) (length (flex-x--context-prefix context))))))

(defun flex-x--matching-candidates (string table pred point)
  "Return (CANDIDATES . BASE-SIZE) for STRING, TABLE, PRED and POINT."
  (let* ((context (flex-x--context string table pred point))
         (terms (flex-x--context-terms context))
         (prefix (flex-x--context-prefix context))
         (match-context (flex-x--make-match-context terms prefix))
         (pattern-cache
          (flex-x--match-context-extra-pattern-cache match-context))
         (flex-regexp-cache
          (flex-x--match-context-flex-regexp-cache match-context)))
    (setq flex-x--last-match-context match-context)
    (cond
     ((null terms)
      (flex-x--completion-list-parts
       (completion-flex-all-completions string table pred point)))
     (t
      (let* ((seed (flex-x--builtin-flex-candidates
                    context table pred (car terms)))
             (base-size (or (cdr seed) (length prefix)))
             (candidates (car seed)))
        (when (flex-x--extra-matchers-p)
          (when-let ((extra-candidates (flex-x--extra-table-candidates
                                        context table pred)))
            (setq candidates (nconc candidates extra-candidates))))
        (setq candidates
              (cl-loop for candidate in (flex-x--delete-duplicate-candidates
                                         candidates)
                       for match = (flex-x--match-candidate
                                    candidate terms pattern-cache
                                    flex-regexp-cache)
                       when match collect (flex-x--propertize-candidate
                                           candidate match)))
        (when (and (flex-x--lazy-hilit-p)
                   (boundp 'completion-lazy-hilit-fn))
          (setq completion-lazy-hilit-fn #'flex-x--lazy-hilit-candidate))
        (cons candidates base-size))))))

(defun flex-x--completion-list (string table pred point)
  "Return completion list for STRING, TABLE, PRED and POINT."
  (pcase-let ((`(,candidates . ,base-size)
               (flex-x--matching-candidates string table pred point)))
    (when candidates
      (nconc candidates base-size))))

;;;###autoload
(defun flex-x-all-completions (string table pred point)
  "Return flex-x completions for STRING in TABLE obeying PRED at POINT."
  (flex-x--completion-list string table pred point))

;;;###autoload
(defun flex-x-try-completion (string table pred point)
  "Try to complete STRING in TABLE obeying PRED at POINT using flex-x."
  (let* ((context (flex-x--context string table pred point))
         (terms (flex-x--context-terms context))
         (match-context (flex-x--make-match-context
                         terms
                         (flex-x--context-prefix context))))
    (setq flex-x--last-match-context match-context)
    (cond
     ((or (null terms)
          (and (= (length terms) 1)
               (not (flex-x--extra-matchers-p))))
      (completion-flex-try-completion string table pred point))
     (t
      (pcase-let ((`(,candidates . ,_base-size)
                   (flex-x--matching-candidates string table pred point)))
        (flex-x--try-completion-from-candidates
         string point context candidates))))))

(defun flex-x--candidate-stored-score (candidate)
  "Return stored score for CANDIDATE, or nil."
  (or (and (> (length candidate) 0)
           (get-text-property 0 'completion-score candidate))
      (and (> (length candidate) 0)
           (get-text-property 0 'flex-x-score candidate))))

(defun flex-x--candidate-stored-cost (candidate)
  "Return stored flex cost for CANDIDATE, or nil."
  (or (and (> (length candidate) 0)
           (get-text-property 0 'flex-cost candidate))
      (and (> (length candidate) 0)
           (get-text-property 0 'flex-x-cost candidate))))

(defun flex-x--candidate-context-match (candidate match-context)
  "Return computed match for CANDIDATE using MATCH-CONTEXT."
  (when-let* ((terms (and match-context
                          (flex-x--match-context-terms match-context))))
    (flex-x--match-candidate
     candidate terms
     (flex-x--match-context-extra-pattern-cache match-context)
     (flex-x--match-context-flex-regexp-cache match-context))))

(defun flex-x--candidate-score (candidate &optional match-context)
  "Return score for CANDIDATE."
  (or (flex-x--candidate-stored-score candidate)
      (when-let ((match (flex-x--candidate-context-match candidate
                                                         match-context)))
        (plist-get match :score))
      0.0))

(defun flex-x--candidate-cost (candidate &optional match-context)
  "Return flex cost for CANDIDATE, or nil."
  (or (flex-x--candidate-stored-cost candidate)
      (when-let ((match (flex-x--candidate-context-match candidate
                                                         match-context)))
        (plist-get match :cost))))

(defun flex-x--rank-table-from-history (history)
  "Return a hash table mapping HISTORY candidates to ranks."
  (let ((rank 0)
        (table (make-hash-table :test #'equal)))
    (dolist (candidate history)
      (when (and (stringp candidate)
                 (not (gethash candidate table)))
        (puthash candidate rank table))
      (cl-incf rank))
    table))

(defun flex-x--corfu-history-active-p ()
  "Return non-nil when Corfu history should sort candidates."
  (and (not (minibufferp))
       (bound-and-true-p corfu-history-mode)
       (boundp 'corfu-history)
       (listp corfu-history)))

(defun flex-x--corfu-history-number (symbol default)
  "Return numeric value of SYMBOL, or DEFAULT."
  (let ((value (and (boundp symbol) (symbol-value symbol))))
    (if (numberp value) value default)))

(defun flex-x--corfu-history-rank-table ()
  "Return a rank table from `corfu-history'."
  (let* ((duplicate (flex-x--corfu-history-number
                     'corfu-history-duplicate 10))
         (decay-value (flex-x--corfu-history-number
                       'corfu-history-decay 10))
         (decay (and (> duplicate 0)
                     (> decay-value 0)
                     (/ -1.0 (* duplicate decay-value))))
         (table (make-hash-table :test #'equal)))
    (cl-loop for candidate in corfu-history
             for index from 0
             when (stringp candidate)
             do (puthash
                 candidate
                 (if-let* ((rank (gethash candidate table)))
                     (if decay
                         (- rank
                            (round (* duplicate
                                      (exp (* decay index)))))
                       rank)
                   (if (= index 0)
                       (/ most-negative-fixnum 2)
                     index))
                 table))
    table))

(defun flex-x--minibuffer-history-rank-table (&optional match-context)
  "Return a rank table from minibuffer history."
  (when (and flex-x-sort-by-history
             (boundp 'minibuffer-history-variable)
             (symbolp minibuffer-history-variable)
             (not (eq minibuffer-history-variable t))
             (boundp minibuffer-history-variable))
    (flex-x--rank-table-from-history
     (minibuffer--sort-preprocess-history
      (or minibuffer-completion-base
          (and match-context
               (flex-x--match-context-prefix match-context))
          "")))))

(defun flex-x--history-rank-table (&optional match-context)
  "Return a hash table mapping history candidates to ranks."
  (when flex-x-sort-by-history
    (if (flex-x--corfu-history-active-p)
        (flex-x--corfu-history-rank-table)
      (flex-x--minibuffer-history-rank-table match-context))))

(defun flex-x--candidate-history-rank (candidate rank-table)
  "Return CANDIDATE rank from RANK-TABLE."
  (if rank-table
      (gethash (substring-no-properties candidate)
               rank-table
               most-positive-fixnum)
    most-positive-fixnum))

(defun flex-x--candidate-sort-record (candidate rank-table match-context index)
  "Return a sort record for CANDIDATE at INDEX."
  (let* ((stored-score (flex-x--candidate-stored-score candidate))
         (stored-cost (flex-x--candidate-stored-cost candidate))
         (match (and (or (null stored-score)
                         (and (flex-x--flex-cost-p)
                              (null stored-cost)))
                     (flex-x--candidate-context-match candidate
                                                      match-context))))
    (vector candidate
            (flex-x--candidate-history-rank candidate rank-table)
            (or stored-cost (plist-get match :cost))
            (or stored-score (plist-get match :score) 0.0)
            index)))

(defun flex-x--sort-record< (a b)
  "Return non-nil if sort record A should come before B."
  (let ((history-a (aref a 1))
        (history-b (aref b 1))
        (cost-a (aref a 2))
        (cost-b (aref b 2))
        (score-a (aref a 3))
        (score-b (aref b 3)))
    (cond
     ((/= history-a history-b)
      (< history-a history-b))
     ((and flex-x-sort-by-score
           (flex-x--flex-cost-p)
           (numberp cost-a)
           (numberp cost-b)
           (/= cost-a cost-b))
      (< cost-a cost-b))
     ((and flex-x-sort-by-score
           (/= score-a score-b))
      (> score-a score-b))
     (t
      (< (aref a 4) (aref b 4))))))

(defun flex-x--sort-candidates (candidates &optional match-context)
  "Sort CANDIDATES by history and score."
  (let ((rank-table (flex-x--history-rank-table match-context))
        records)
    (cl-loop for candidate in candidates
             for index from 0
             do (push (flex-x--candidate-sort-record
                       candidate rank-table match-context index)
                      records))
    (mapcar (lambda (record)
              (aref record 0))
            (sort (nreverse records) #'flex-x--sort-record<))))

(defun flex-x--metadata-without (metadata properties)
  "Return METADATA alist without entries whose cars are in PROPERTIES."
  (cl-remove-if (lambda (entry)
                  (memq (car-safe entry) properties))
                (cdr metadata)))

(defun flex-x--active-match-context-p (match-context)
  "Return non-nil if MATCH-CONTEXT represents active flex-x filtering."
  (and match-context
       (flex-x--match-context-terms match-context)))

(defun flex-x--metadata-property-value (metadata property fallback)
  "Return PROPERTY value in METADATA, or FALLBACK if PROPERTY is missing."
  (if-let ((entry (assq property (cdr metadata))))
      (cdr entry)
    fallback))

(defun flex-x--compose-sort-function (existing-sort-function &optional
                                                             match-context-cell)
  "Return a sort function composed with EXISTING-SORT-FUNCTION."
  (lambda (candidates)
    (let ((sorted (if existing-sort-function
                      (funcall existing-sort-function candidates)
                    candidates)))
      (flex-x--sort-candidates sorted (car match-context-cell)))))

(defun flex-x--adjust-metadata (metadata)
  "Adjust completion METADATA for flex-x sorting."
  (let* ((display-sort-function
          (completion-metadata-get metadata 'display-sort-function))
         (cycle-sort-function
          (completion-metadata-get metadata 'cycle-sort-function))
         (match-context flex-x--last-match-context)
         (match-context-cell
          (or (alist-get 'flex-x--match-context-cell (cdr metadata))
              (cons match-context nil)))
         (already-adjusted (assq 'flex-x--adjusted-metadata (cdr metadata)))
         (original-display-sort-function
          (flex-x--metadata-property-value
           metadata
           'flex-x--original-display-sort-function
           display-sort-function))
         (original-cycle-sort-function
          (flex-x--metadata-property-value
           metadata
           'flex-x--original-cycle-sort-function
           cycle-sort-function))
         (rest (flex-x--metadata-without
                metadata
                flex-x--metadata-properties)))
    (setcar match-context-cell match-context)
    (if (not (flex-x--active-match-context-p match-context))
        `(metadata
          ,@(and original-display-sort-function
                 `((display-sort-function
                    . ,original-display-sort-function)))
          ,@(and original-cycle-sort-function
                 `((cycle-sort-function . ,original-cycle-sort-function)))
          ,@rest)
      `(metadata
        (display-sort-function
         . ,(if already-adjusted
                display-sort-function
              (flex-x--compose-sort-function
               original-display-sort-function
               match-context-cell)))
        (cycle-sort-function
         . ,(if already-adjusted
                cycle-sort-function
              (flex-x--compose-sort-function
               original-cycle-sort-function
               match-context-cell)))
        (flex-x--adjusted-metadata . t)
        (flex-x--match-context-cell . ,match-context-cell)
        (flex-x--original-display-sort-function
         . ,original-display-sort-function)
        (flex-x--original-cycle-sort-function
         . ,original-cycle-sort-function)
        ,@rest))))

;;;###autoload
(defun flex-x-migemo-match (term candidate)
  "Return non-nil when TERM matches CANDIDATE using migemo.

This function does not require migemo at load time.  Add it to
`flex-x-extra-match-functions' after loading migemo.

For simple migemo integration, prefer setting
`flex-x-extra-pattern-function' to `migemo-get-pattern'."
  (when (fboundp 'migemo-get-pattern)
    (and (string-match-p (migemo-get-pattern term) candidate)
         t)))

;;;###autoload
(defun flex-x-register-style ()
  "Register the `flex-x' completion style."
  (unless (assq 'flex-x completion-styles-alist)
    (add-to-list
     'completion-styles-alist
     '(flex-x
       flex-x-try-completion
       flex-x-all-completions
       "Extended flex completion with space-separated terms.")))
  (put 'flex-x 'completion--adjust-metadata #'flex-x--adjust-metadata))

(flex-x-register-style)

(provide 'flex-x)

;;; flex-x.el ends here
