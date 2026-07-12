;;; flex-x.el --- Extended flex completion style -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: kn66 <https://github.com/kn66>
;; Assisted-by: Codex:GPT-5
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/kn66/flex-x

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; flex-x adds a completion style based on the built-in `flex' style.
;;
;; Features:
;;
;; - Space-separated AND filtering.
;; - Sorting by minibuffer or Corfu history and flex score.
;; - Standard completion highlighting and whole-candidate highlighting for
;;   literal and word-prefix-sequence matches, including lazy highlighting.
;; - Optional regexp expanders for non-ASCII candidates, such as migemo or pyim.
;;
;; Add `flex-x' to `completion-styles' to enable it:
;;
;;   (add-to-list 'completion-styles 'flex-x)

;;; Code:

(require 'cl-lib)
(require 'minibuffer)
(require 'subr-x)

;; These functions are available only in newer Emacs versions.  Their calls
;; are guarded by `fboundp' so flex-x can still support Emacs 30.
(declare-function completion--flex-cost "minibuffer"
                  (pat str &optional dont-error) t)
(declare-function completion-lazy-hilit-p "minibuffer" () t)

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
  "Face used to highlight strong flex-x matches.

This face intentionally does not set foreground or background colors."
  :group 'flex-x)

(defcustom flex-x-split-regexp "[[:space:]]+"
  "Regexp used to split input into flex-x search terms."
  :type 'regexp
  :group 'flex-x)

(defcustom flex-x-sort-by-history t
  "Non-nil means prefer candidates found in completion history.

In minibuffer completion, this uses `minibuffer-history-variable' and
`minibuffer-completion-base'.  Outside the minibuffer, this uses
`corfu-history' when `corfu-history-mode' is enabled."
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
this value, flex-x skips that extra matching pass and keeps only
built-in flex matches.

Set this to nil to allow scanning every prefix candidate."
  :type '(choice (const :tag "No limit" nil)
                 (natnum :tag "Maximum candidates"))
  :group 'flex-x)

(defvar flex-x--last-match-context nil
  "Last match context used by `flex-x-all-completions'.")

(defconst flex-x--metadata-properties
  '(display-sort-function
    cycle-sort-function
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
  flex-regexp-cache
  highlight-patterns)

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
         (field-after (substring afterpoint 0 end))
         (field (concat field-before field-after)))
    (make-flex-x--context
     :prefix (substring beforepoint 0 start)
     :suffix (substring afterpoint end)
     :field field
     :field-point (length field-before)
     :terms (flex-x--split field))))

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

(defun flex-x--delete-duplicate-candidates (candidates)
  "Remove duplicate CANDIDATES while preserving order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (candidate candidates)
      (let ((key (flex-x--candidate-target candidate)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push candidate result))))
    (nreverse result)))

(defun flex-x--flex-regexp (term)
  "Return a grouped flex regexp for TERM."
  (completion-pcm--pattern->regex
   (completion-pcm--optimize-pattern
    (completion-flex--make-flex-pattern
     (list 'prefix term)))
   'group))

(defun flex-x--cached (cache key thunk)
  "Return CACHE value for KEY, computing it with THUNK when missing."
  (let ((cached (gethash key cache 'flex-x--missing)))
    (if (not (eq cached 'flex-x--missing))
        cached
      (puthash key (funcall thunk) cache))))

(defun flex-x--cached-flex-regexp (term match-context)
  "Return cached flex regexp for TERM in MATCH-CONTEXT."
  (flex-x--cached
   (flex-x--match-context-flex-regexp-cache match-context)
   term
   (lambda () (flex-x--flex-regexp term))))

(defun flex-x--highlight-regexp (term)
  "Return regexp matching TERM literally."
  (regexp-quote term))

(defun flex-x--prefix-sequence-regexp (term)
  "Return regexp matching TERM across consecutive word prefixes."
  (unless (string-match-p "[[:space:][:punct:]]" term)
    (concat
     "\\(?:\\`\\|[[:space:][:punct:]]\\)"
     (regexp-quote (substring term 0 1))
     (mapconcat
      (lambda (index)
        (let ((character (regexp-quote (substring term index (1+ index)))))
          (concat "\\(?:" character
                  "\\|[^[:space:][:punct:]]*[[:space:][:punct:]]+"
                  character "\\)")))
      (number-sequence 1 (1- (length term)))
      ""))))

(defun flex-x--whole-candidate-highlight-p (candidate match-context)
  "Return non-nil when every search term strongly matches CANDIDATE."
  (let ((case-fold-search completion-ignore-case)
        (target (flex-x--candidate-target candidate)))
    (cl-loop for (literal-regexp . prefix-sequence-regexp)
             in (flex-x--match-context-highlight-patterns match-context)
             always
             (or (string-match-p literal-regexp target)
                 (and prefix-sequence-regexp
                      (string-match-p prefix-sequence-regexp target))))))

(defun flex-x--lazy-hilit-p ()
  "Return non-nil if completion frontend supports lazy highlighting."
  (if (fboundp 'completion-lazy-hilit-p)
      (completion-lazy-hilit-p)
    (and (boundp 'completion-lazy-hilit)
         completion-lazy-hilit)))

(defun flex-x--flex-cost-p ()
  "Return non-nil when this Emacs provides flex cost matching."
  (fboundp 'completion--flex-cost))

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

(defun flex-x--string-nonascii-p (string)
  "Return non-nil if STRING has at least one non-ASCII character."
  (cl-loop for char across string thereis (> char 127)))

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

(defun flex-x--make-match-context (terms prefix)
  "Return a match context for TERMS and PREFIX."
  (make-flex-x--match-context
   :terms terms
   :prefix prefix
   :extra-pattern-cache (make-hash-table :test #'equal)
   :flex-regexp-cache (make-hash-table :test #'equal)
   :highlight-patterns
   (mapcar (lambda (term)
             (cons (flex-x--highlight-regexp term)
                   (flex-x--prefix-sequence-regexp term)))
           terms)))

(defun flex-x--normalize-extra-match (value)
  "Normalize an extra matcher return VALUE to a plist."
  (cond
   ((null value) nil)
   ((numberp value) (list :score value))
   ((and (listp value) (plist-member value :score)) value)
   (t (list :score flex-x-extra-match-score))))

(defun flex-x--extra-match-allowed-p (candidate)
  "Return non-nil when extra matchers may run for CANDIDATE."
  (and (flex-x--extra-matchers-p)
       (or (not flex-x-extra-match-nonascii-only)
           (flex-x--string-nonascii-p candidate))))

(defun flex-x--extra-function-match (term candidate)
  "Return custom function match information for TERM and CANDIDATE."
  (cl-loop for fn in flex-x-extra-match-functions
           for value = (ignore-errors (funcall fn term candidate))
           for match = (flex-x--normalize-extra-match value)
           when match return match))

(defun flex-x--valid-regexp (regexp)
  "Return REGEXP when it is a valid non-empty regexp string."
  (when (and (stringp regexp)
             (not (string-empty-p regexp)))
    (condition-case nil
        (progn
          (string-match-p regexp "")
          regexp)
      (invalid-regexp nil))))

(defun flex-x--extra-pattern (function term)
  "Return regexp from FUNCTION for TERM, or nil."
  (when (functionp function)
    (flex-x--valid-regexp
     (ignore-errors (funcall function term)))))

(defun flex-x--extra-patterns (term)
  "Return valid extra regexps for TERM."
  (cl-loop for function in (flex-x--extra-pattern-functions)
           for regexp = (flex-x--extra-pattern function term)
           when regexp collect regexp))

(defun flex-x--cached-extra-patterns (term match-context)
  "Return cached extra regexps for TERM in MATCH-CONTEXT."
  (flex-x--cached
   (flex-x--match-context-extra-pattern-cache match-context)
   term
   (lambda () (flex-x--extra-patterns term))))

(defun flex-x--extra-pattern-match (term candidate match-context)
  "Return extra regexp match information for TERM and CANDIDATE."
  (let ((case-fold-search completion-ignore-case))
    (cl-loop for regexp in (flex-x--cached-extra-patterns term match-context)
             when (string-match regexp candidate)
             return (list :score flex-x-extra-match-score
                          :ranges (list (cons (match-beginning 0)
                                              (match-end 0)))))))

(defun flex-x--extra-match (term candidate match-context)
  "Return extra match information for TERM and CANDIDATE."
  (when (flex-x--extra-match-allowed-p candidate)
    (or (flex-x--extra-function-match term candidate)
        (flex-x--extra-pattern-match term candidate match-context))))

(defun flex-x--match-term (term candidate match-context)
  "Return match information when TERM matches CANDIDATE."
  (let ((target (flex-x--candidate-target candidate)))
    (if (flex-x--flex-cost-p)
        (or (flex-x--precomputed-flex-match term candidate target)
            (if-let* ((cost-match (funcall #'completion--flex-cost
                                           term target t)))
                (let ((cost (car cost-match))
                      (matches (cdr cost-match)))
                  (list :score (flex-x--score-from-cost cost target)
                        :cost cost
                        :matches matches))
              (flex-x--extra-match term target match-context)))
      (let* ((regexp (flex-x--cached-flex-regexp term match-context))
             (score (completion--flex-score target regexp t)))
        (if score
            (list :score score :regexp regexp)
          (flex-x--extra-match term target match-context))))))

(defun flex-x--match-candidate (candidate match-context)
  "Return aggregate match information for CANDIDATE and MATCH-CONTEXT."
  (let* ((terms (flex-x--match-context-terms match-context))
         (term-count (length terms))
         (score 0.0)
         (cost 0.0)
         (cost-count 0)
         (matches nil))
    (when terms
      (catch 'failed
        (dolist (term terms)
          (let ((match (flex-x--match-term term candidate match-context)))
            (unless match
              (throw 'failed nil))
            (cl-incf score (or (plist-get match :score) 0.0))
            (when-let* ((match-cost (plist-get match :cost)))
              (cl-incf cost match-cost)
              (cl-incf cost-count))
            (push match matches)))
        (list :score (/ score term-count)
              :cost (and (= cost-count term-count)
                         (> cost-count 0)
                         (/ cost cost-count))
              :match-context match-context
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

(defun flex-x--apply-match-faces (candidate match)
  "Destructively apply completion faces to CANDIDATE using MATCH."
  (dolist (term-match (plist-get match :matches))
    (when-let* ((regexp (plist-get term-match :regexp)))
      (completion--hilit-from-re candidate regexp))
    (when-let* ((matches (plist-get term-match :matches)))
      (flex-x--highlight-matches candidate matches))
    (when-let* ((ranges (plist-get term-match :ranges)))
      (flex-x--highlight-ranges candidate ranges)))
  (when (and (> (length candidate) 0)
             (flex-x--whole-candidate-highlight-p
              candidate (plist-get match :match-context)))
    (add-face-text-property 0 (length candidate) 'flex-x-highlight
                            t candidate))
  candidate)

(defun flex-x--lazy-hilit-candidate (candidate)
  "Destructively highlight CANDIDATE using its stored flex-x match."
  (if-let* ((match (and (> (length candidate) 0)
                        (get-text-property 0 'flex-x--match candidate))))
      (flex-x--apply-match-faces candidate match)
    candidate))

(defun flex-x--propertize-candidate (candidate match)
  "Return CANDIDATE propertized using MATCH."
  (let* ((copy (copy-sequence candidate))
         (score (or (plist-get match :score) 0.0))
         (lazy-hilit (flex-x--lazy-hilit-p))
         (flex-matches (cl-loop for term-match in (plist-get match :matches)
                                append (plist-get term-match :matches))))
    (when (> (length copy) 0)
      (put-text-property 0 1 'completion-score score copy)
      (put-text-property 0 1 'flex-x-score score copy)
      (when flex-matches
        (put-text-property 0 1 'flex-matches flex-matches copy))
      (when-let* ((cost (plist-get match :cost)))
        (put-text-property 0 1 'flex-cost cost copy)
        (put-text-property 0 1 'flex-x-cost cost copy))
      (when lazy-hilit
        (put-text-property 0 1 'flex-x--match match copy)))
    (if lazy-hilit
        copy
      (flex-x--apply-match-faces copy match))))

(defun flex-x--all-table-candidates (context table pred)
  "Return all prefix candidates in TABLE using CONTEXT and PRED."
  (all-completions (flex-x--context-prefix context) table pred))

(defun flex-x--candidate-key (candidate)
  "Return plain key for CANDIDATE."
  (cond
   ((stringp candidate)
    (substring-no-properties candidate))
   ((and (consp candidate)
         (stringp (car candidate)))
    (substring-no-properties (car candidate)))
   (t candidate)))

(defun flex-x--first-candidate-p (candidate seen)
  "Record CANDIDATE in SEEN and return non-nil if it was new."
  (let ((key (flex-x--candidate-key candidate)))
    (unless (gethash key seen)
      (puthash key t seen))))

(defun flex-x--within-candidate-limit-p (candidates)
  "Return non-nil if CANDIDATES may be scanned by extra matchers."
  (let ((limit flex-x-extra-match-candidate-limit))
    (or (null limit)
        (and
         (integerp limit)
         (>= limit 0)
         (let ((seen (make-hash-table :test #'equal))
               (count 0))
           (catch 'too-many
             (dolist (candidate candidates t)
               (when (flex-x--first-candidate-p candidate seen)
                 (cl-incf count)
                 (when (> count limit)
                   (throw 'too-many nil))))))))))

(defun flex-x--candidate-limit-predicate (pred limit tag marker)
  "Return predicate combining PRED with LIMIT.

Throw MARKER to TAG once more than LIMIT unique candidates have been
accepted."
  (let ((seen (make-hash-table :test #'equal))
        (count 0))
    (lambda (&rest args)
      (and (or (null pred) (apply pred args))
           (progn
             (when (flex-x--first-candidate-p (car args) seen)
               (cl-incf count)
               (when (> count limit)
                 (throw tag marker)))
             t)))))

(defun flex-x--extra-table-candidates (context table pred)
  "Return candidates for extra matching when the scan is bounded."
  (let ((limit flex-x-extra-match-candidate-limit))
    (cond
     ((null limit)
      (flex-x--all-table-candidates context table pred))
     ((and (integerp limit) (>= limit 0))
      (let* ((tag (make-symbol "flex-x--candidate-limit"))
             (marker (cons nil nil))
             (limited-pred (flex-x--candidate-limit-predicate
                            pred limit tag marker))
             (candidates
              (catch tag
                (flex-x--all-table-candidates
                 context table limited-pred))))
        (unless (eq candidates marker)
          ;; A custom table may ignore PRED, so enforce the limit on its
          ;; returned candidates as a fallback.
          (when (flex-x--within-candidate-limit-p candidates)
            candidates)))))))

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

(defun flex-x--matched-candidates (candidates match-context)
  "Return CANDIDATES that match MATCH-CONTEXT, with flex-x properties."
  (cl-loop for candidate in (flex-x--delete-duplicate-candidates candidates)
           for match = (flex-x--match-candidate candidate match-context)
           when match collect (flex-x--propertize-candidate candidate match)))

(defun flex-x--matching-candidates (string table pred point)
  "Return (CANDIDATES . BASE-SIZE) for STRING, TABLE, PRED and POINT."
  (let* ((context (flex-x--context string table pred point))
         (terms (flex-x--context-terms context))
         (match-context (flex-x--make-match-context
                         terms
                         (flex-x--context-prefix context)))
         (extra-matchers (flex-x--extra-matchers-p)))
    (setq flex-x--last-match-context match-context)
    (if (null terms)
        (flex-x--completion-list-parts
         (completion-flex-all-completions string table pred point))
      (let* ((seed (flex-x--builtin-flex-candidates
                    context table pred (car terms)))
             (base-size (cdr seed))
             (candidates (car seed)))
        (when extra-matchers
          (when-let* ((extra-candidates (flex-x--extra-table-candidates
                                         context table pred)))
            (setq candidates (append candidates extra-candidates))))
        (setq candidates
              (flex-x--matched-candidates candidates match-context))
        (when (and extra-matchers minibuffer-completing-file-name)
          (setq candidates
                (completion-pcm--filename-try-filter candidates)))
        (when (and (flex-x--lazy-hilit-p)
                   (boundp 'completion-lazy-hilit-fn))
          (setq completion-lazy-hilit-fn #'flex-x--lazy-hilit-candidate))
        (cons candidates base-size)))))

;;;###autoload
(defun flex-x-all-completions (string table pred point)
  "Return flex-x completions for STRING in TABLE obeying PRED at POINT."
  (pcase-let ((`(,candidates . ,base-size)
               (flex-x--matching-candidates string table pred point)))
    (when candidates
      (nconc candidates base-size))))

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
           (completion (flex-x--context-completion-string context candidate)))
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

;;;###autoload
(defun flex-x-try-completion (string table pred point)
  "Try to complete STRING in TABLE obeying PRED at POINT using flex-x."
  (let* ((context (flex-x--context string table pred point))
         (terms (flex-x--context-terms context))
         (match-context (flex-x--make-match-context
                         terms
                         (flex-x--context-prefix context))))
    (setq flex-x--last-match-context match-context)
    (if (or (null terms)
            (and (= (length terms) 1)
                 (not (flex-x--extra-matchers-p))))
        (completion-flex-try-completion string table pred point)
      (pcase-let ((`(,candidates . ,_base-size)
                   (flex-x--matching-candidates string table pred point)))
        (flex-x--try-completion-from-candidates
         string point context candidates)))))

(defun flex-x--candidate-property (candidate properties)
  "Return the first non-nil text property in PROPERTIES for CANDIDATE."
  (when (> (length candidate) 0)
    (cl-loop for property in properties
             thereis (get-text-property 0 property candidate))))

(defun flex-x--candidate-stored-score (candidate)
  "Return stored score for CANDIDATE, or nil."
  (flex-x--candidate-property candidate '(completion-score flex-x-score)))

(defun flex-x--candidate-stored-cost (candidate)
  "Return stored flex cost for CANDIDATE, or nil."
  (flex-x--candidate-property candidate '(flex-cost flex-x-cost)))

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

(defun flex-x--minibuffer-history-rank-table (match-context)
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

(defun flex-x--history-rank-table (match-context)
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

(defun flex-x--sort-candidates (candidates &optional match-context)
  "Sort CANDIDATES by history first, then flex quality."
  (let ((rank-table (flex-x--history-rank-table match-context))
        (terms (and match-context
                    (flex-x--match-context-terms match-context)))
        (use-cost (flex-x--flex-cost-p))
        records)
    (cl-loop for candidate in candidates
             for index from 0
             do (let* ((score (flex-x--candidate-stored-score candidate))
                       (cost (flex-x--candidate-stored-cost candidate))
                       (match (and terms
                                   (or (null score)
                                       (and use-cost (null cost)))
                                   (flex-x--match-candidate
                                    candidate match-context))))
                  (push (vector candidate
                                (flex-x--candidate-history-rank
                                 candidate rank-table)
                                (or cost (plist-get match :cost))
                                (or score (plist-get match :score) 0.0)
                                index)
                        records)))
    (mapcar
     (lambda (record) (aref record 0))
     (sort
      (nreverse records)
      (lambda (a b)
        (cond
         ((/= (aref a 1) (aref b 1))
          (< (aref a 1) (aref b 1)))
         ((and use-cost
               (numberp (aref a 2))
               (numberp (aref b 2))
               (/= (aref a 2) (aref b 2)))
          (< (aref a 2) (aref b 2)))
         ((/= (aref a 3) (aref b 3))
          (> (aref a 3) (aref b 3)))
         (t
          (< (aref a 4) (aref b 4)))))))))

(defun flex-x--metadata-without-flex-x (metadata)
  "Return METADATA alist without entries managed by flex-x."
  (cl-remove-if (lambda (entry)
                  (memq (car-safe entry) flex-x--metadata-properties))
                (cdr metadata)))

(defun flex-x--compose-sort-function (existing-sort-function match-context-cell)
  "Return a sort function composed with EXISTING-SORT-FUNCTION."
  (lambda (candidates)
    (let ((sorted (if existing-sort-function
                      (funcall existing-sort-function candidates)
                    candidates)))
      (flex-x--sort-candidates sorted (car match-context-cell)))))

(defun flex-x--adjust-metadata (metadata)
  "Adjust completion METADATA for flex-x sorting."
  (let* ((match-context flex-x--last-match-context)
         (match-context-cell
          (or (alist-get 'flex-x--match-context-cell (cdr metadata))
              (cons match-context nil)))
         (original-display-sort-function
          (alist-get
           'flex-x--original-display-sort-function
           (cdr metadata)
           (completion-metadata-get metadata 'display-sort-function)))
         (original-cycle-sort-function
          (alist-get
           'flex-x--original-cycle-sort-function
           (cdr metadata)
           (completion-metadata-get metadata 'cycle-sort-function)))
         (rest (flex-x--metadata-without-flex-x metadata)))
    (setcar match-context-cell match-context)
    (if (not (and match-context
                  (flex-x--match-context-terms match-context)))
        `(metadata
          ,@(and original-display-sort-function
                 `((display-sort-function
                    . ,original-display-sort-function)))
          ,@(and original-cycle-sort-function
                 `((cycle-sort-function . ,original-cycle-sort-function)))
          ,@rest)
      `(metadata
        (display-sort-function
         . ,(flex-x--compose-sort-function
             original-display-sort-function
             match-context-cell))
        (cycle-sort-function
         . ,(flex-x--compose-sort-function
             original-cycle-sort-function
             match-context-cell))
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
    (let ((regexp (ignore-errors (migemo-get-pattern term))))
      (and (flex-x--valid-regexp regexp)
           (string-match-p regexp candidate)
           t))))

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
