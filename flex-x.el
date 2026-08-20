;;; flex-x.el --- Extended flex completion style -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: kn66 <https://github.com/kn66>
;; Assisted-by: Codex:GPT-5
;; Version: 0.2.0
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
;; - Fuzzy matching before whitespace and literal AND filtering after it.
;; - Sorting by minibuffer history and flex score.
;; - Standard completion highlighting, including lazy highlighting.
;; - An optional regexp expander for non-ASCII candidates, such as migemo or
;;   pyim.
;;
;; Add `flex-x' to `completion-styles' to enable it:
;;
;;   (add-to-list 'completion-styles 'flex-x)

;;; Code:

(require 'cl-lib)
(require 'minibuffer)
(require 'subr-x)

(defvar completion-lazy-hilit)
(defvar completion-lazy-hilit-fn)

(defgroup flex-x nil
  "Extended flex completion style."
  :group 'minibuffer
  :prefix "flex-x-")

(defconst flex-x--whitespace-regexp "[[:space:]]+"
  "Regexp separating flex-x search terms.")

(defcustom flex-x-extra-pattern-function nil
  "Function or function name used to build an extra regexp from a search term.

The function receives one argument, the search term, and should return
a regexp string or nil.  This is suitable for functions like
`migemo-get-pattern' or `pyim-cregexp-build'.

The regexp is used only when built-in flex does not match.  By default
this extra regexp matching is limited to non-ASCII candidates; see
`flex-x-extra-match-nonascii-only'."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Function")
                 (symbol :tag "Function name"))
  :group 'flex-x)

(defcustom flex-x-extra-match-nonascii-only t
  "Non-nil means use expanded regexp matching only for non-ASCII candidates."
  :type 'boolean
  :group 'flex-x)

(defcustom flex-x-extra-match-candidate-limit 5000
  "Maximum number of prefix candidates scanned for regexp expansion.

When a regexp expander is configured, flex-x may need to inspect
candidates that did not match built-in flex.  If the number of
candidates returned for the current completion prefix is greater than
this value, flex-x skips expanded regexp matching and keeps only
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
  terms
  literal-terms-p)

(cl-defstruct flex-x--match-context
  terms
  literal-terms-p
  extra-pattern-cache
  flex-regexp-cache)

(defun flex-x--split (string)
  "Split STRING into non-empty flex-x search terms."
  (split-string string flex-x--whitespace-regexp t))

(defun flex-x--context (string table pred point)
  "Return completion context for STRING, TABLE, PRED and POINT."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (start (or (car-safe bounds) 0))
         (end (or (cdr-safe bounds) (length afterpoint)))
         (field-before (substring beforepoint start))
         (field-after (substring afterpoint 0 end))
         (field (concat field-before field-after))
         (literal-terms-p
          (string-match-p flex-x--whitespace-regexp field)))
    (make-flex-x--context
     :prefix (substring beforepoint 0 start)
     :suffix (substring afterpoint end)
     :field field
     :terms (flex-x--split field)
     :literal-terms-p literal-terms-p)))

(defun flex-x--context-completion-string (context candidate)
  "Return full completion string for CANDIDATE in CONTEXT."
  (concat (flex-x--context-prefix context)
          (substring-no-properties candidate)
          (flex-x--context-suffix context)))

(defun flex-x--context-completion-point (context candidate)
  "Return point position after completing CANDIDATE in CONTEXT."
  (+ (length (flex-x--context-prefix context))
     (length candidate)))

(defun flex-x--whitespace-only-p (context)
  "Return non-nil when CONTEXT's field contains only whitespace."
  (let ((field (flex-x--context-field context)))
    (and (null (flex-x--context-terms context))
         (not (string-empty-p field))
         (string-match-p flex-x--whitespace-regexp field))))

(defun flex-x--empty-field-completions (context table pred)
  "Return flex completions for CONTEXT with an empty completion field."
  (let ((prefix (flex-x--context-prefix context))
        (suffix (flex-x--context-suffix context)))
    (completion-flex-all-completions
     (concat prefix suffix) table pred (length prefix))))

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

(defun flex-x--visible-string-nonascii-p (string)
  "Return non-nil if STRING has a visible non-ASCII character."
  (cl-loop for index below (length string)
           thereis (and (> (aref string index) 127)
                        (not (get-text-property index 'invisible string)))))

(defun flex-x--extra-pattern-p ()
  "Return non-nil if an extra regexp expander is configured."
  (functionp flex-x-extra-pattern-function))

(defun flex-x--make-match-context (terms literal-terms-p)
  "Return a match context for TERMS and LITERAL-TERMS-P."
  (make-flex-x--match-context
   :terms terms
   :literal-terms-p literal-terms-p
   :extra-pattern-cache (make-hash-table :test #'equal)
   :flex-regexp-cache (make-hash-table :test #'equal)))

(defun flex-x--extra-pattern-match-allowed-p (candidate)
  "Return non-nil when extra regexp matching may run for CANDIDATE."
  (and (flex-x--extra-pattern-p)
       (or (not flex-x-extra-match-nonascii-only)
           (flex-x--visible-string-nonascii-p candidate))))

(defun flex-x--valid-regexp (regexp)
  "Return REGEXP when it is a valid non-empty regexp string."
  (when (and (stringp regexp)
             (not (string-empty-p regexp)))
    (condition-case nil
        (progn
          (string-match-p regexp "")
          regexp)
      (invalid-regexp nil))))

(defun flex-x--extra-pattern (term)
  "Return the configured extra regexp for TERM, or nil."
  (when (functionp flex-x-extra-pattern-function)
    (flex-x--valid-regexp
     (ignore-errors (funcall flex-x-extra-pattern-function term)))))

(defun flex-x--cached-extra-pattern (term match-context)
  "Return the cached extra regexp for TERM in MATCH-CONTEXT."
  (flex-x--cached
   (flex-x--match-context-extra-pattern-cache match-context)
   term
   (lambda () (flex-x--extra-pattern term))))

(defun flex-x--extra-pattern-match (term candidate match-context)
  "Return extra regexp match information for TERM and CANDIDATE."
  (let ((case-fold-search completion-ignore-case))
    (when-let* ((regexp (flex-x--cached-extra-pattern term match-context))
                ((string-match regexp candidate)))
      (list :score 0.1
            :ranges (list (cons (match-beginning 0)
                                (match-end 0)))))))

(defun flex-x--builtin-flex-match (term candidate target match-context)
  "Return built-in flex match information for TERM and CANDIDATE's TARGET."
  (if (fboundp 'completion--flex-cost)
      (or (flex-x--precomputed-flex-match term candidate target)
          (when-let* ((cost-match (funcall #'completion--flex-cost
                                           term target t)))
            (let ((cost (car cost-match))
                  (matches (cdr cost-match)))
              (list :score (flex-x--score-from-cost cost target)
                    :cost cost
                    :matches matches))))
    (let* ((regexp (flex-x--cached-flex-regexp term match-context))
           (score (completion--flex-score target regexp t)))
      (when score
        (list :score score :regexp regexp)))))

(defun flex-x--literal-match (term candidate target match-context)
  "Return literal match information for TERM in CANDIDATE's TARGET."
  (let ((case-fold-search completion-ignore-case))
    (when (string-match (regexp-quote term) target)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (match (flex-x--builtin-flex-match
                    term candidate target match-context)))
        (list :score (plist-get match :score)
              :cost (plist-get match :cost)
              :ranges (list (cons beg end)))))))

(defun flex-x--match-term (term candidate match-context)
  "Return match information when TERM matches CANDIDATE."
  (let ((target (flex-x--candidate-target candidate)))
    (or (if (flex-x--match-context-literal-terms-p match-context)
            (flex-x--literal-match term candidate target match-context)
          (flex-x--builtin-flex-match term candidate target match-context))
        (and (flex-x--extra-pattern-match-allowed-p candidate)
             (flex-x--extra-pattern-match term target match-context)))))

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
      (when flex-matches
        (put-text-property 0 1 'flex-matches flex-matches copy))
      (when-let* ((cost (plist-get match :cost)))
        (put-text-property 0 1 'flex-cost cost copy))
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
  "Return non-nil if CANDIDATES may be scanned for regexp expansion."
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
  "Return candidates for expanded regexp matching when the scan is bounded."
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
                         (flex-x--context-literal-terms-p context)))
         (extra-pattern-p (flex-x--extra-pattern-p)))
    (setq flex-x--last-match-context match-context)
    (if (null terms)
        (flex-x--completion-list-parts
         (if (flex-x--whitespace-only-p context)
             (flex-x--empty-field-completions context table pred)
           (completion-flex-all-completions string table pred point)))
      (let* ((seed (flex-x--builtin-flex-candidates
                    context table pred (car terms)))
             (base-size (cdr seed))
             (candidates (car seed)))
        (when extra-pattern-p
          (when-let* ((extra-candidates (flex-x--extra-table-candidates
                                         context table pred)))
            (setq candidates (append candidates extra-candidates))))
        (setq candidates
              (flex-x--matched-candidates candidates match-context))
        (when (and extra-pattern-p minibuffer-completing-file-name)
          (setq candidates
                (completion-pcm--filename-try-filter candidates)))
        (when (boundp 'completion-lazy-hilit-fn)
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
                  (if (flex-x--whitespace-only-p context)
                      0
                    (length (flex-x--context-field context)))))
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
                         (flex-x--context-literal-terms-p context))))
    (setq flex-x--last-match-context match-context)
    (if (or (and (null terms)
                 (not (flex-x--whitespace-only-p context)))
            (and (not (flex-x--context-literal-terms-p context))
                 (= (length terms) 1)
                 (not (string-match-p
                       flex-x--whitespace-regexp
                       (flex-x--context-field context)))
                 (not (flex-x--extra-pattern-p))))
        (completion-flex-try-completion string table pred point)
      (pcase-let ((`(,candidates . ,_base-size)
                   (flex-x--matching-candidates string table pred point)))
        (when minibuffer-completing-file-name
          (setq candidates
                (completion-pcm--filename-try-filter candidates)))
        (flex-x--try-completion-from-candidates
         string point context candidates)))))

(defun flex-x--candidate-stored-score (candidate)
  "Return stored score for CANDIDATE, or nil."
  (and (> (length candidate) 0)
       (get-text-property 0 'completion-score candidate)))

(defun flex-x--candidate-stored-cost (candidate)
  "Return stored flex cost for CANDIDATE, or nil."
  (and (> (length candidate) 0)
       (get-text-property 0 'flex-cost candidate)))

(defun flex-x--sort-candidates (candidates &optional match-context)
  "Sort CANDIDATES by flex quality."
  (let ((terms (and match-context
                    (flex-x--match-context-terms match-context)))
        (use-cost (fboundp 'completion--flex-cost))
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
         ((and use-cost
               (numberp (aref a 1))
               (numberp (aref b 1))
               (/= (aref a 1) (aref b 1)))
          (< (aref a 1) (aref b 1)))
         ((/= (aref a 2) (aref b 2))
          (> (aref a 2) (aref b 2)))
         (t
          (< (aref a 3) (aref b 3)))))))))

(defun flex-x--minibuffer-history-rank-table ()
  "Return ranks for the active minibuffer history, or nil."
  (when (and (minibufferp)
             (boundp 'minibuffer-history-variable)
             (symbolp minibuffer-history-variable)
             (not (eq minibuffer-history-variable t))
             (boundp minibuffer-history-variable))
    (let ((history
           (minibuffer--sort-preprocess-history
            (or minibuffer-completion-base "")))
          (rank 0)
          (table (make-hash-table :test #'equal)))
      (dolist (candidate history)
        (when (and (stringp candidate)
                   (not (gethash candidate table)))
          (puthash candidate rank table))
        (cl-incf rank))
      table)))

(defun flex-x--promote-minibuffer-history (candidates)
  "Stably promote history entries in CANDIDATES."
  (if-let* ((rank-table (flex-x--minibuffer-history-rank-table)))
      (cl-stable-sort
       candidates
       (lambda (a b)
         (< (gethash (substring-no-properties a)
                     rank-table most-positive-fixnum)
            (gethash (substring-no-properties b)
                     rank-table most-positive-fixnum))))
    candidates))

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
      (setq sorted
            (flex-x--sort-candidates sorted (car match-context-cell)))
      (if existing-sort-function
          sorted
        (flex-x--promote-minibuffer-history sorted)))))

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
    (if (or (not (and match-context
                      (flex-x--match-context-terms match-context)))
            (flex-x--match-context-literal-terms-p match-context))
        `(metadata
          ,@(and original-display-sort-function
                 `((display-sort-function . ,original-display-sort-function)))
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
(defun flex-x-register-style ()
  "Register the `flex-x' completion style."
  (unless (assq 'flex-x completion-styles-alist)
    (add-to-list
     'completion-styles-alist
     '(flex-x
       flex-x-try-completion
       flex-x-all-completions
       "Extended flex completion with space-separated terms.")))
  (put 'flex-x 'completion--adjust-metadata #'flex-x--adjust-metadata)
  (remove-hook 'minibuffer-setup-hook 'flex-x--minibuffer-setup))

(flex-x-register-style)

(provide 'flex-x)

;;; flex-x.el ends here
