;;; flex-x-tests.el --- Tests for flex-x -*- lexical-binding: t; -*-

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

(ert-deftest flex-x-score-properties-are-added ()
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
      (should (>= (get-text-property 0 'flex-x-score candidate)
                  flex-x-highlight-score-threshold))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight))
      (should (flex-x-tests--face-at-p candidate 0
                                       'completions-common-part)))))

(ert-deftest flex-x-low-score-candidates-are-not-highlighted ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "stb"
                         '("switch-to-buffer")
                         nil 3))
           (candidate (car completions)))
      (should (numberp (get-text-property 0 'completion-score candidate)))
      (should (get-text-property 0 'flex-x-score candidate))
      (should (< (get-text-property 0 'flex-x-score candidate)
                 flex-x-highlight-score-threshold))
      (should-not (flex-x-tests--any-face-p candidate 'flex-x-highlight))
      (should (flex-x-tests--face-at-p candidate 0
                                       'completions-common-part)))))

(ert-deftest flex-x-default-threshold-highlights-practical-flex-score ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (let* ((completions (completion-all-completions
                         "ff pr"
                         '("project-find-file")
                         nil 5))
           (candidate (car completions)))
      (should (>= (get-text-property 0 'flex-x-score candidate)
                  flex-x-highlight-score-threshold))
      (should (flex-x-tests--all-face-p candidate 'flex-x-highlight)))))

(ert-deftest flex-x-highlight-face-does-not-set-colors ()
  (should (eq (face-attribute 'flex-x-highlight :foreground nil nil)
              'unspecified))
  (should (eq (face-attribute 'flex-x-highlight :background nil nil)
              'unspecified))
  (should (eq (face-attribute 'flex-x-highlight :weight nil nil)
              'bold))
  (should (eq (face-attribute 'flex-x-highlight :underline nil nil)
              t)))

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
      (should (flex-x-tests--all-face-p highlighted 'flex-x-highlight))
      (should (flex-x-tests--face-at-p highlighted 0
                                       'completions-common-part)))))

(ert-deftest flex-x-lazy-hilit-handles-extra-match-ranges ()
  (let ((completion-styles '(flex-x))
        (completion-lazy-hilit t)
        completion-lazy-hilit-fn
        (flex-x-extra-match-functions
         (list (lambda (term candidate)
                 (and (string= term "tokyo")
                      (string= candidate "foo-bar東京")
                      '(:score 0.2 :ranges ((7 . 9)))))))
        (flex-x-extra-pattern-function nil)
        (flex-x-extra-match-nonascii-only t))
    (let* ((completions (completion-all-completions
                         "fb tokyo" '("foo-bar東京") nil 8))
           (candidate (car completions))
           (highlighted (completion-lazy-hilit candidate)))
      (should candidate)
      (should-not (get-text-property 7 'face candidate))
      (should (eq completion-lazy-hilit-fn
                  #'flex-x--lazy-hilit-candidate))
      (should (flex-x-tests--face-at-p highlighted 7
                                       'completions-common-part)))))

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

(ert-deftest flex-x-extra-pattern-function-skips-ascii-candidates-by-default ()
  (let* ((completion-styles '(flex-x))
         (calls 0)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function
          (lambda (_term)
            (cl-incf calls)
            "ascii"))
         (flex-x-extra-match-nonascii-only t))
    (completion-all-completions "zzz" '("ascii") nil 3)
    (should (= calls 0))))

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

(ert-deftest flex-x-flex-regexp-is-cached-per-term ()
  (skip-unless (not (flex-x--flex-cost-p)))
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (calls 0)
         (original (symbol-function 'flex-x--flex-regexp)))
    (cl-letf (((symbol-function 'flex-x--flex-regexp)
               (lambda (term)
                 (cl-incf calls)
                 (funcall original term))))
      (let ((items (flex-x-tests--items
                    (completion-all-completions
                     "fb" '("foo-bar" "fbar" "far-baz") nil 2))))
        (should (equal items '("foo-bar" "fbar" "far-baz")))
        (should (= calls 1))))))

(ert-deftest flex-x-duplicate-candidates-preserve-distinct-properties ()
  (let* ((first (propertize "foo" 'flex-x-tests-id 1))
         (second (propertize "foo" 'flex-x-tests-id 2))
         (deduped (flex-x--delete-duplicate-candidates
                   (list first second))))
    (should (= (length deduped) 2))
    (should (equal (mapcar (lambda (candidate)
                             (get-text-property 0 'flex-x-tests-id
                                                candidate))
                           deduped)
                   '(1 2)))))

(ert-deftest flex-x-duplicate-candidates-ignore-flex-derived-properties ()
  (let* ((seed (propertize "foo"
                           'completion-score 1.0
                           'flex-cost 0
                           'flex-matches '(0 1 2)
                           'flex-x--seed-term "foo"))
         (plain "foo")
         (deduped (flex-x--delete-duplicate-candidates
                   (list seed plain))))
    (should (= (length deduped) 1))
    (should (equal (car deduped) seed))))

(ert-deftest flex-x-extra-matcher-error-does-not-abort-completion ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions
         (list (lambda (_term _candidate)
                 (error "broken matcher"))
               (lambda (term candidate)
                 (and (string= term "tokyo")
                      (string= candidate "東京")))))
        (flex-x-extra-pattern-function nil)
        (flex-x-extra-match-nonascii-only t))
    (should (member "東京"
                    (flex-x-tests--items
                     (completion-all-completions
                      "tokyo" '("東京") nil 5))))))

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

(ert-deftest flex-x-extra-matcher-runs-within-candidate-limit ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "tokyo")
                       (string= candidate "東京")))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit 2))
    (should (member "東京"
                    (flex-x-tests--items
                     (completion-all-completions
                      "tokyo" '("東京" "東京都") nil 5))))))

(ert-deftest flex-x-extra-matcher-limit-counts-predicate-matches ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "tokyo")
                       (string= candidate "東京")))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit 1)
         (items (flex-x-tests--items
                 (completion-all-completions
                  "tokyo" '("東京" "東京都")
                  (lambda (candidate)
                    (string= candidate "東京"))
                  5))))
    (should (member "東京" items))
    (should-not (member "東京都" items))))

(ert-deftest flex-x-extra-matcher-limit-supports-hash-table ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "東京" 'city table)
    (let* ((completion-styles '(flex-x))
           (flex-x-extra-match-functions
            (list (lambda (term candidate)
                    (and (string= term "tokyo")
                         (string= candidate "東京")))))
           (flex-x-extra-pattern-function nil)
           (flex-x-extra-match-nonascii-only t)
           (flex-x-extra-match-candidate-limit 1))
      (should (member "東京"
                      (flex-x-tests--items
                       (completion-all-completions
                        "tokyo" table nil 5)))))))

(ert-deftest flex-x-extra-matcher-limit-supports-hash-table-predicate ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "東京" 'city table)
    (puthash "東京都" 'prefecture table)
    (let* ((completion-styles '(flex-x))
           (flex-x-extra-match-functions
            (list (lambda (term candidate)
                    (and (string= term "tokyo")
                         (string= candidate "東京")))))
           (flex-x-extra-pattern-function nil)
           (flex-x-extra-match-nonascii-only t)
           (flex-x-extra-match-candidate-limit 1)
           (items (flex-x-tests--items
                   (completion-all-completions
                    "tokyo" table
                    (lambda (_candidate value)
                      (eq value 'city))
                    5))))
      (should (member "東京" items))
      (should-not (member "東京都" items)))))

(ert-deftest flex-x-extra-matcher-limit-counts-alist-string-keys ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "tokyo")
                       (string= candidate "東京")))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit 1))
    (should (member "東京"
                    (flex-x-tests--items
                     (completion-all-completions
                      "tokyo" '(("東京" . 1) ("東京" . 2)) nil 5))))))

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

(ert-deftest flex-x-extra-matcher-file-name-limit-counts-filtered-candidates ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions
          (list (lambda (term candidate)
                  (and (string= term "tokyo")
                       (string-match-p "東京" candidate)))))
         (flex-x-extra-pattern-function nil)
         (flex-x-extra-match-nonascii-only t)
         (flex-x-extra-match-candidate-limit 1)
         (minibuffer-completing-file-name t)
         (completion-ignored-extensions '(".o")))
    (should (equal (flex-x-tests--items
                    (completion-all-completions
                     "tokyo" '("東京.o" "東京.el") nil 5))
                   '("東京.el")))))

(ert-deftest flex-x-migemo-match-uses-default-extra-match-score ()
  (cl-letf (((symbol-function 'migemo-get-pattern)
             (lambda (_term) (regexp-quote "東京"))))
    (let ((completion-styles '(flex-x))
          (flex-x-extra-match-functions '(flex-x-migemo-match))
          (flex-x-extra-pattern-function nil)
          (flex-x-extra-match-nonascii-only t))
      (let ((candidate (car (completion-all-completions
                             "tokyo" '("東京") nil 5))))
        (should (= (get-text-property 0 'flex-x-score candidate)
                   flex-x-extra-match-score))
        (should (flex-x-tests--all-face-p candidate
                                          'flex-x-highlight))))))

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

(ert-deftest flex-x-sort-corfu-history-duplicates-raise-rank ()
  (let* ((corfu-history-mode t)
         (corfu-history-duplicate 10)
         (corfu-history-decay 10)
         (corfu-history '("unused" "foo-bar" "far-baz" "far-baz"))
         (high-score (propertize "foo-bar"
                                 'completion-score 1.0
                                 'flex-x-score 1.0))
         (low-score (propertize "far-baz"
                                'completion-score 0.1
                                'flex-x-score 0.1)))
    (should (equal (mapcar #'substring-no-properties
                           (flex-x--sort-candidates
                            (list high-score low-score)))
                   '("far-baz" "foo-bar")))))

(ert-deftest flex-x-sort-computes-match-once-per-candidate ()
  (let* ((flex-x-sort-by-history nil)
         (calls 0)
         (match-context (flex-x--make-match-context '("fb") "")))
    (cl-letf (((symbol-function 'flex-x--match-candidate)
               (lambda (candidate _terms _pattern-cache _flex-regexp-cache)
                 (cl-incf calls)
                 (list :score (if (string= candidate "far-baz")
                                  1.0
                                0.1)))))
      (should (equal (flex-x--sort-candidates
                      '("foo-bar" "far-baz")
                      match-context)
                     '("far-baz" "foo-bar")))
      (should (= calls 2)))))

(ert-deftest flex-x-sort-by-history-toggle-disables-corfu-history ()
  (let* ((flex-x-sort-by-history nil)
         (corfu-history-mode t)
         (corfu-history '("far-baz"))
         (high-score (propertize "foo-bar"
                                 'completion-score 1.0
                                 'flex-x-score 1.0))
         (low-score (propertize "far-baz"
                                'completion-score 0.1
                                'flex-x-score 0.1)))
    (should (equal (mapcar #'substring-no-properties
                           (flex-x--sort-candidates
                            (list low-score high-score)))
                   '("foo-bar" "far-baz")))))

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

(ert-deftest flex-x-adjust-metadata-does-not-wrap-sort-function-repeatedly ()
  (let* ((completion-styles '(flex-x))
         (flex-x-sort-by-history nil)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table '("foo-bar" "fbar" "far-baz"))
         (metadata (completion-metadata "" table nil)))
    (completion-all-completions "fb" table nil 2 metadata)
    (let ((sort-function (completion-metadata-get metadata
                                                  'display-sort-function))
          (metadata-length (length (cdr metadata))))
      (completion-all-completions "fa" table nil 2 metadata)
      (should (eq sort-function
                  (completion-metadata-get metadata
                                           'display-sort-function)))
      (should (= metadata-length (length (cdr metadata))))
      (should (= 1 (cl-count 'display-sort-function
                             (cdr metadata)
                             :key #'car-safe))))))

(ert-deftest flex-x-empty-input-does-not-install-sort-functions ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table '("b" "a"))
         (metadata (completion-metadata "" table nil)))
    (completion-all-completions "" table nil 0 metadata)
    (should-not (completion-metadata-get metadata 'display-sort-function))
    (should-not (completion-metadata-get metadata 'cycle-sort-function))
    (should-not (assq 'flex-x--adjusted-metadata (cdr metadata)))))

(ert-deftest flex-x-empty-input-restores-original-sort-functions ()
  (let* ((completion-styles '(flex-x))
         (flex-x-sort-by-history nil)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table '("foo-bar" "fbar" "far-baz"))
         (metadata '(metadata
                     (display-sort-function . reverse)
                     (cycle-sort-function . nreverse))))
    (completion-all-completions "fb" table nil 2 metadata)
    (should-not (eq (completion-metadata-get metadata 'display-sort-function)
                    'reverse))
    (completion-all-completions "" table nil 0 metadata)
    (should (eq (completion-metadata-get metadata 'display-sort-function)
                'reverse))
    (should (eq (completion-metadata-get metadata 'cycle-sort-function)
                'nreverse))
    (should-not (assq 'flex-x--adjusted-metadata (cdr metadata)))))

(ert-deftest flex-x-try-completion-updates-single-term-sort-context ()
  (let* ((completion-styles '(flex-x))
         (flex-x-sort-by-history nil)
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (flex-x--last-match-context (flex-x--make-match-context '("zz") ""))
         (table '("foo-bar" "fbar" "far-baz"))
         (metadata (completion-metadata "" table nil)))
    (completion-try-completion "fb" table nil 2 metadata)
    (let ((sort-function (completion-metadata-get metadata
                                                  'display-sort-function)))
      (should sort-function)
      (should (equal (funcall sort-function
                              '("foo-bar" "fbar" "far-baz"))
                     '("fbar" "foo-bar" "far-baz"))))))

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

(ert-deftest flex-x-try-completion-preserves-boundary-prefix ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table (flex-x-tests--slash-boundary-table '("foo-bar")))
         (input "dir/foo bar"))
    (should (equal (flex-x-try-completion input table nil (length input))
                   '("dir/foo-bar" . 11)))))

(ert-deftest flex-x-try-completion-preserves-boundary-suffix ()
  (let* ((completion-styles '(flex-x))
         (flex-x-extra-match-functions nil)
         (flex-x-extra-pattern-function nil)
         (table (flex-x-tests--slash-boundary-table '("foo-bar")))
         (input "dir/foo bar/qux")
         (point (length "dir/foo bar")))
    (should (equal (flex-x-try-completion input table nil point)
                   '("dir/foo-bar/qux" . 11)))))

(ert-deftest flex-x-try-completion-keeps-input-for-multiple-matches ()
  (let ((completion-styles '(flex-x))
        (flex-x-extra-match-functions nil)
        (flex-x-extra-pattern-function nil))
    (should (equal (flex-x-try-completion
                    "f b" '("foo-bar" "far-baz") nil 3)
                   '("f b" . 3)))))

(ert-deftest flex-x-emacs31-cost-matching-adds-cost-and-matches ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (term candidate &optional _dont-error)
               (and (string= term "foo")
                    (string= candidate "foo")
                    (cons 0 '(0 1 2)))))
            ((symbol-function 'completion--flex-score)
             (lambda (&rest _)
               (error "old flex score should not be used"))))
    (let ((completion-styles '(flex-x))
          (flex-x-extra-match-functions nil)
          (flex-x-extra-pattern-function nil))
      (let* ((completions (completion-all-completions
                           "foo" '("foo") nil 3))
             (candidate (car completions)))
        (should (= (get-text-property 0 'flex-x-score candidate) 1.0))
        (should (= (get-text-property 0 'flex-x-cost candidate) 0))
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

(ert-deftest flex-x-emacs31-reuses-seed-flex-cost-for-first-term ()
  (let ((cost-calls nil))
    (cl-letf (((symbol-function 'completion-flex-all-completions)
               (lambda (_string _table _pred _point)
                 (cons (propertize "foo-bar"
                                    'flex-cost 1
                                    'flex-matches '(0 1 2))
                       0)))
              ((symbol-function 'completion--flex-cost)
               (lambda (term candidate &optional _dont-error)
                 (push (list term candidate) cost-calls)
                 (cond
                  ((string= term "foo")
                   (error "seed flex cost should be reused"))
                  ((and (string= term "bar")
                        (string= candidate "foo-bar"))
                   (cons 2 '(4 5 6)))))))
      (let ((completion-styles '(flex-x))
            (flex-x-extra-match-functions nil)
            (flex-x-extra-pattern-function nil))
        (let* ((completions (completion-all-completions
                             "foo bar" '("foo-bar") nil 7))
               (candidate (car completions)))
          (should candidate)
          (should (equal (nreverse cost-calls)
                         '(("bar" "foo-bar"))))
          (should (= (get-text-property 0 'flex-x-cost candidate) 1.5))
          (should (equal (get-text-property 0 'flex-matches candidate)
                         '(0 1 2 4 5 6))))))))

(ert-deftest flex-x-emacs31-mixed-extra-match-does-not-add-cost ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (term candidate &optional _dont-error)
               (and (string= term "fb")
                    (string= candidate "foo-bar東京")
                    (cons 1 '(0 4))))))
    (let ((completion-styles '(flex-x))
          (flex-x-extra-match-functions
           (list (lambda (term candidate)
                   (and (string= term "tokyo")
                        (string= candidate "foo-bar東京")
                        '(:score 0.2 :ranges ((7 . 9)))))))
          (flex-x-extra-pattern-function nil)
          (flex-x-extra-match-nonascii-only t))
      (let* ((completions (completion-all-completions
                           "fb tokyo" '("foo-bar東京") nil 8))
             (candidate (car completions)))
        (should candidate)
        (should (get-text-property 0 'flex-x-score candidate))
        (should-not (get-text-property 0 'flex-x-cost candidate))
        (should-not (get-text-property 0 'flex-cost candidate))
        (should (flex-x-tests--face-at-p candidate 7
                                         'completions-common-part))))))

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

(ert-deftest flex-x-emacs31-cost-sort-obeys-score-toggle ()
  (cl-letf (((symbol-function 'completion--flex-cost)
             (lambda (&rest _) (cons 0 '(0)))))
    (let* ((flex-x-sort-by-history nil)
           (flex-x-sort-by-score nil)
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
                     '("high-score" "low-cost"))))))

(provide 'flex-x-tests)

;;; flex-x-tests.el ends here
