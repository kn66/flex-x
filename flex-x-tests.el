;;; flex-x-tests.el --- Tests for flex-x -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:

(require 'ert)
(require 'flex-x)

(defvar corfu-history)
(defvar corfu-history-decay)
(defvar corfu-history-duplicate)
(defvar corfu-history-mode)

(defvar flex-x-tests-history nil
  "History variable used by flex-x tests.")

(defun flex-x-tests--items (completions)
  "Return plain strings from COMPLETIONS."
  (let ((items nil)
        (cell completions))
    (while (consp cell)
      (push (substring-no-properties (car cell)) items)
      (setq cell (cdr cell)))
    (nreverse items)))

(defun flex-x-tests--faces-at (candidate pos)
  "Return face list at POS in CANDIDATE."
  (let ((face (get-text-property pos 'face candidate)))
    (cond
     ((null face) nil)
     ((and (listp face) (not (keywordp (car face)))) face)
     (t (list face)))))

(defun flex-x-tests--face-at-p (candidate pos face)
  "Return non-nil if CANDIDATE has FACE at POS."
  (memq face (flex-x-tests--faces-at candidate pos)))

(defun flex-x-tests--any-face-p (candidate face)
  "Return non-nil if CANDIDATE has FACE anywhere."
  (cl-loop for pos below (length candidate)
           thereis (flex-x-tests--face-at-p candidate pos face)))

(defun flex-x-tests--all-face-p (candidate face)
  "Return non-nil if CANDIDATE has FACE everywhere."
  (cl-loop for pos below (length candidate)
           always (flex-x-tests--face-at-p candidate pos face)))

(defun flex-x-tests--slash-boundary-table (candidates)
  "Return a completion table for slash-delimited CANDIDATES."
  (lambda (string pred action)
    (let* ((slash (cl-position ?/ string :from-end t))
           (start (if slash (1+ slash) 0))
           (field (substring string start)))
      (cond
       ((eq action 'metadata) nil)
       ((eq (car-safe action) 'boundaries)
        (let* ((suffix (cdr action))
               (end (or (cl-position ?/ suffix)
                        (length suffix))))
          `(boundaries ,start . ,end)))
       (t
        (complete-with-action action candidates field pred))))))

(ert-deftest flex-x-registers-completion-style ()
  (should (assq 'flex-x completion-styles-alist))
  (should (eq (get 'flex-x 'completion--adjust-metadata)
              #'flex-x--adjust-metadata)))

(ert-deftest flex-x-space-separated-terms-filter-with-and ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (should (equal (flex-x-tests--items
                    (completion-all-completions
                     "ff pr"
                     '("find-file" "project-find-file" "switch-to-buffer")
                     nil 5))
                   '("project-find-file")))))

(ert-deftest flex-x-skips-extra-candidate-scan-without-extra-matchers ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil)
        (calls 0))
    (cl-letf (((symbol-function 'flex-x--extra-table-candidates)
               (lambda (&rest _)
                 (cl-incf calls)
                 (error "extra candidate scan should not run"))))
      (should (equal (flex-x-tests--items
                      (completion-all-completions
                       "fb" '("foo-bar") nil 2))
                     '("foo-bar")))
      (should (= calls 0)))))

(ert-deftest flex-x-respects-completion-boundaries ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table (flex-x-tests--slash-boundary-table
                 '("foo-bar" "foo-baz")))
         (input "dir/f ba/qux")
         (point (length "dir/f ba")))
    (should (equal (flex-x-try-completion input table nil point)
                   '("dir/foo-ba/qux" . 10)))))

(ert-deftest flex-x-score-property-and-standard-face-are-added ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "foo"
                         '("foo")
                         nil 3))
           (candidate (car completions)))
      (should (numberp (get-text-property 0 'completion-score candidate)))
      (should (get-text-property 0 'flex-x-score candidate))
      (should (flex-x-tests--face-at-p candidate 0
                                       'completions-common-part))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-long-candidate-starting-with-term-is-highlighted ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "agent"
                         '("agent-shell-new-downloads-shell")
                         nil 5))
           (candidate (car completions)))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-term-after-separator-highlights-whole-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "agent" '("gnus-agentize") nil 5))
           (candidate (car completions)))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-contiguous-term-inside-word-highlights-whole-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "load" '("org-reload") nil 4))
           (candidate (car completions)))
      (should candidate)
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-word-prefix-highlight-obeys-completion-ignore-case ()
  (let ((completion-styles '(flex-x))
        (completion-ignore-case t)
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "agent" '("Gnus-Agentize") nil 5))
           (candidate (car completions)))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-all-terms-at-word-prefixes-highlight-whole-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "sw bu" '("switch-to-buffer") nil 5))
           (candidate (car completions)))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-concatenated-word-prefixes-highlight-whole-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (dolist (input '("loadfile" "lofile"))
      (let* ((completions (completion-all-completions
                           input '("load-file") nil (length input)))
             (candidate (car completions)))
        (should candidate)
        (should (flex-x-tests--all-face-p candidate 'flex-x-highlight))))))

(ert-deftest flex-x-word-prefix-sequence-may-start-after-earlier-words ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "lofile" '("org-babel-load-file") nil 6))
           (candidate (car completions)))
      (should candidate)
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-word-prefix-sequence-does-not-skip-words ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "lofile" '("htmlfontify-load-rgb-file") nil 6))
           (candidate (car completions)))
      (should candidate)
      (should-not (flex-x-tests--any-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-word-prefix-sequence-requires-word-prefixes ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "loadfile" '("package-upload-file") nil 8))
           (candidate (car completions)))
      (should candidate)
      (should-not (flex-x-tests--any-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-noncontiguous-flex-match-is-not-highlighted-as-a-whole ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "agt"
                         '("agent-shell")
                         nil 3))
           (candidate (car completions)))
      (should (numberp (get-text-property 0 'completion-score candidate)))
      (should (get-text-property 0 'flex-x-score candidate))
      (should-not (flex-x-tests--any-face-p candidate 'flex-x-highlight))
      (should (flex-x-tests--face-at-p candidate 0
                                       'completions-common-part)))))

(ert-deftest flex-x-all-terms-must-strongly-match-for-whole-highlight ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "sw agt" '("switch-to-buffer-agent") nil 6))
           (candidate (car completions)))
      (should candidate)
      (should-not (flex-x-tests--any-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-lazy-hilit-defers-faces ()
  (let ((completion-styles '(flex-x))
        (completion-lazy-hilit t)
        completion-lazy-hilit-fn
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "foo" '("foo") nil 3))
           (candidate (car completions))
           (highlighted (completion-lazy-hilit candidate)))
      (should (numberp (get-text-property 0 'completion-score candidate)))
      (should-not (get-text-property 0 'face candidate))
      (should (eq completion-lazy-hilit-fn
                  #'flex-x--lazy-hilit-candidate))
      (should (flex-x-tests--face-at-p highlighted 0
                                       'completions-common-part))
      (should (flex-x-tests--all-face-p highlighted 'flex-x-highlight)))))

(ert-deftest flex-x-extra-matcher-adds-nonascii-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions
         (list (lambda (term candidate)
                 (and (string= term "tokyo")
                      (string= candidate "東京")))))
        (flex-x-extra-pattern-function nil)
        (flex-x-extra-match-nonascii-only t))
    (should (member "東京"
                    (flex-x-tests--items
                     (completion-all-completions
                      "tokyo" '("東京" "touch") nil 5))))))

(ert-deftest flex-x-extra-matcher-skips-ascii-candidates-by-default ()
  (let* ((completion-styles '(flex-x))
         (calls 0)
         (flex-x-extra-match-functions
          (list (lambda (_term _candidate)
                  (cl-incf calls)
                  t)))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t))
    (completion-all-completions "zzz" '("ascii") nil 3)
    (should (= calls 0))))

(ert-deftest flex-x-extra-matcher-ranges-are-highlighted ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions
         (list (lambda (term candidate)
                 (and (string= term "tokyo")
                      (string= candidate "東京")
                      '(:score 0.2 :ranges ((0 . 2)))))))
        (flex-x-extra-pattern-function nil)
        (flex-x-extra-match-nonascii-only t))
    (let ((candidate (car (completion-all-completions
                           "tokyo" '("東京") nil 5))))
      (should (flex-x-tests--face-at-p candidate 0
                                       'completions-common-part)))))

(ert-deftest flex-x-extra-pattern-function-adds-nonascii-candidate ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function
         (lambda (term)
           (and (string= term "tokyo")
                (regexp-quote "東京"))))
        (flex-x-extra-match-nonascii-only t))
    (should (member "東京"
                    (flex-x-tests--items
                     (completion-all-completions
                      "tokyo" '("東京" "touch") nil 5))))))

(ert-deftest flex-x-extra-pattern-function-is-cached-per-term ()
  (let* ((completion-styles '(flex-x))
         (calls 0)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function
          (lambda (term)
            (cl-incf calls)
            (and (string= term "tokyo")
                 (regexp-quote "東京"))))
         (flex-x-extra-match-nonascii-only t))
    (let ((items (flex-x-tests--items
                  (completion-all-completions
                   "tokyo" '("東京" "東京都" "touch") nil 5))))
      (should (member "東京" items))
      (should (member "東京都" items))
      (should (= calls 1)))))

(ert-deftest flex-x-extra-pattern-function-type-accepts-symbols ()
  (require 'cus-edit)
  (let ((widget (widget-convert
                 (get 'flex-x-extra-pattern-function 'custom-type))))
    (should (widget-apply widget :match 'migemo-get-pattern))
    (should (widget-apply widget :match
                          '(migemo-get-pattern pyim-cregexp-build)))))

(ert-deftest flex-x-extra-matcher-respects-candidate-limit ()
  (let* ((completion-styles '(flex-x))
         (calls 0)
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (cl-incf calls)
                  (and (string= term "tokyo")
                       (string= candidate "東京")))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit 1))
    (should-not (member "東京"
                        (flex-x-tests--items
                         (completion-all-completions
                          "tokyo" '("東京" "東京都") nil 5))))
    (should (= calls 0))))

(ert-deftest flex-x-extra-matcher-stops-enumerating-at-candidate-limit ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions (list (lambda (&rest _) t)))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only nil)
         (flex-x-extra-match-candidate-limit 5)
         (candidates (mapcar #'number-to-string (number-sequence 1 100)))
         (calls 0)
         (original-candidate-key (symbol-function 'flex-x--candidate-key)))
    (cl-letf (((symbol-function 'flex-x--candidate-key)
               (lambda (candidate)
                 (cl-incf calls)
                 (funcall original-candidate-key candidate))))
      (completion-all-completions "zzz" candidates nil 3)
      (should (= calls 6)))))

(ert-deftest flex-x-extra-matcher-obeys-file-name-ignored-extensions ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "tokyo")
                       (string-match-p "東京" candidate)))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit nil)
         (minibuffer-completing-file-name t)
         (completion-ignored-extensions '(".o")))
    (should (equal (flex-x-tests--items
                    (completion-all-completions
                     "tokyo" '("東京.el" "東京.o") nil 5))
                   '("東京.el")))))

(ert-deftest flex-x-extra-matcher-keeps-sole-matching-ignored-file ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "zzz")
                       (string= candidate "special.o")))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only nil)
         (flex-x-extra-match-candidate-limit nil)
         (minibuffer-completing-file-name t)
         (completion-ignored-extensions '(".o")))
    (should (equal (flex-x-tests--items
                    (completion-all-completions
                     "zzz" '("special.o" "unrelated.el") nil 3))
                   '("special.o")))))

(ert-deftest flex-x-migemo-match-does-not-require-migemo ()
  (should-not (flex-x-migemo-match "tokyo" "東京"))
  (cl-letf (((symbol-function 'migemo-get-pattern)
             (lambda (_term) (regexp-quote "東京"))))
    (should (flex-x-migemo-match "tokyo" "東京"))))

(ert-deftest flex-x-sort-prefers-history-then-score ()
  (let ((flex-x-tests-history '("far-baz")))
    (let* ((completion-styles '(flex-x))
           (flex-x-extra-match-functions nil)
           (flex-x-extra-pattern-function nil)
           (minibuffer-history-variable 'flex-x-tests-history)
           (metadata (completion-metadata "" '("foo-bar" "far-baz") nil))
           (completions (completion-all-completions
                         "fb" '("foo-bar" "far-baz") nil 2 metadata))
           (sort-function (completion-metadata-get metadata
                                                   'display-sort-function)))
      (should sort-function)
      (should (equal (flex-x-tests--items
                      (funcall sort-function
                               (flex-x-tests--items completions)))
                     '("far-baz" "foo-bar"))))))

(ert-deftest flex-x-sort-prefers-corfu-history-then-score ()
  (let ((flex-x-tests-history '("foo-bar")))
    (let* ((completion-styles '(flex-x))
           (flex-x-extra-match-functions nil)
           (flex-x-extra-pattern-function nil)
           (corfu-history-mode t)
           (corfu-history '("far-baz"))
           (minibuffer-history-variable 'flex-x-tests-history)
           (metadata (completion-metadata "" '("foo-bar" "far-baz") nil))
           (completions (completion-all-completions
                         "fb" '("foo-bar" "far-baz") nil 2 metadata))
           (sort-function (completion-metadata-get metadata
                                                   'display-sort-function)))
      (should sort-function)
      (should (equal (flex-x-tests--items
                      (funcall sort-function
                               (flex-x-tests--items completions)))
                     '("far-baz" "foo-bar"))))))

(ert-deftest flex-x-sort-keeps-existing-metadata-sort-function ()
  (let* ((completion-styles '(flex-x))
         (flex-x-sort-by-history nil)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (metadata '(metadata (display-sort-function . reverse))))
    (completion-all-completions "a" '("ab" "ac") nil 1 metadata)
    (let ((sort-function (completion-metadata-get metadata
                                                  'display-sort-function)))
      (should sort-function)
      (should (equal (funcall sort-function '("ab" "ac"))
                     '("ac" "ab"))))))

(ert-deftest flex-x-sort-function-keeps-match-context ()
  (let* ((completion-styles '(flex-x))
         (flex-x-sort-by-history nil)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table '("foo-bar" "fbar" "far-baz"))
         (metadata (completion-metadata "" table nil)))
    (completion-all-completions "fb" table nil 2 metadata)
    (let ((sort-function (completion-metadata-get metadata
                                                  'display-sort-function)))
      (should sort-function)
      (completion-all-completions "zz" '("zzz") nil 2)
      (should (equal (funcall sort-function
                              '("foo-bar" "fbar" "far-baz"))
                     '("fbar" "foo-bar" "far-baz"))))))

(ert-deftest flex-x-sort-computes-match-once-per-candidate ()
  (let* ((flex-x-sort-by-history nil)
         (calls 0)
         (match-context (flex-x--make-match-context '("fb") "")))
    (cl-letf (((symbol-function 'flex-x--match-candidate)
               (lambda (candidate _match-context)
                 (cl-incf calls)
                 (list :score (if (string= candidate "far-baz")
                                  1.0
                                0.1)))))
      (should (equal (flex-x--sort-candidates
                      '("foo-bar" "far-baz")
                      match-context)
                     '("far-baz" "foo-bar")))
      (should (= calls 2)))))

(ert-deftest flex-x-try-completion-returns-nil-for-no-multi-term-match ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (should-not (flex-x-try-completion
                 "zz aa" '("foo" "bar") nil 5))))

(ert-deftest flex-x-try-completion-completes-single-multi-term-match ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (should (equal (flex-x-try-completion
                    "ff pr"
                    '("find-file" "project-find-file" "switch-to-buffer")
                    nil 5)
                   '("project-find-file" . 17)))))

(ert-deftest flex-x-try-completion-extends-common-multi-term-prefix ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (should (equal (flex-x-try-completion
                    "fb ba" '("foo-bar" "foo-baz" "far-qux") nil 5)
                   '("foo-ba" . 6)))))

(ert-deftest flex-x-emacs31-cost-matching-adds-cost-and-matches ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (term candidate &optional _dont-error)
               (and (string= term "foo")
                    (string= candidate "foo")
                    (cons 0 '(0 1 2))))))
    (let ((completion-styles '(flex-x))
          (flex-x-extra-match-functions nil)
          (flex-x-extra-pattern-function nil))
      (let* ((completions (completion-all-completions
                           "foo" '("foo") nil 3))
             (candidate (car completions)))
        (should (= (get-text-property 0 'flex-x-score candidate) 1.0))
        (should (= (get-text-property 0 'flex-x-cost candidate) 0))
        (should (= (get-text-property 0 'flex-cost candidate) 0))
        (should (equal (get-text-property 0 'flex-matches candidate)
                       '(0 1 2)))
        (should (flex-x-tests--all-face-p candidate 'flex-x-highlight))
        (should (flex-x-tests--face-at-p candidate 0
                                         'completions-common-part))))))

(ert-deftest flex-x-emacs31-cost-matching-highlights-first-difference ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (term candidate &optional _dont-error)
               (and (string= term "fb")
                    (string= candidate "foo-bar")
                    (cons 1 '(0 4))))))
    (let ((completion-styles '(flex-x))
          (flex-x-extra-match-functions nil)
          (flex-x-extra-pattern-function nil))
      (let* ((completions (completion-all-completions
                           "fb" '("foo-bar") nil 2))
             (candidate (car completions)))
        (should (flex-x-tests--face-at-p candidate 0
                                         'completions-common-part))
        (should (flex-x-tests--face-at-p candidate 4
                                         'completions-common-part))
        (should (flex-x-tests--face-at-p candidate 5
                                         'completions-first-difference))))))

(ert-deftest flex-x-emacs31-cost-sort-prefers-lower-cost ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (&rest _) (cons 0 '(0)))))
    (let* ((flex-x-sort-by-history nil)
           (high-score (propertize "high-score"
                                   'flex-x-score 1.0
                                   'completion-score 1.0
                                   'flex-x-cost 10
                                   'flex-cost 10))
           (low-cost (propertize "low-cost"
                                 'flex-x-score 0.1
                                 'completion-score 0.1
                                 'flex-x-cost 1
                                 'flex-cost 1)))
      (should (equal (mapcar #'substring-no-properties
                             (flex-x--sort-candidates
                              (list high-score low-cost)))
                     '("low-cost" "high-score"))))))

(ert-deftest flex-x-emacs31-reuses-seed-flex-cost ()
  (let ((cost-calls 0))
    (cl-letf (((symbol-function 'completion-flex-all-completions)
               (lambda (_string _table _pred _point)
                 (cons (propertize "foo"
                                   'flex-cost 0
                                   'flex-matches '(0 1 2))
                       0)))
              ((symbol-function 'completion--flex-cost)
               (lambda (&rest _)
                 (cl-incf cost-calls)
                 (error "seed flex cost should be reused"))))
      (let ((completion-styles '(flex-x))
            (flex-x-extra-match-functions nil)
            (flex-x-extra-pattern-function nil))
        (let* ((completions (completion-all-completions
                             "foo" '("foo") nil 3))
               (candidate (car completions)))
          (should candidate)
          (should (= cost-calls 0))
          (should (= (get-text-property 0 'flex-x-cost candidate) 0))
          (should (equal (get-text-property 0 'flex-matches candidate)
                         '(0 1 2))))))))

(provide 'flex-x-tests)

;;; flex-x-tests.el ends here
