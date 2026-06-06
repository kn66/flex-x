EMACS ?= emacs
BYTE_COMPILE_DIR ?= /tmp/flex-x-byte-compile

BATCH = $(EMACS) --batch -Q \
	--eval "(setq load-prefer-newer t)" \
	--eval "(setq native-comp-jit-compilation nil)" \
	--eval "(setq native-comp-enable-subr-trampolines nil)" \
	--eval "(progn (require 'package) (package-initialize))" \
	--eval "(add-to-list 'load-path default-directory)" \
	-L .

SOURCES = flex-x.el

TESTS = flex-x-tests.el

.PHONY: check test compile load package-lint checkdoc check-declare clean-elc

check: test compile package-lint checkdoc check-declare

test:
	$(BATCH) -l $(TESTS) -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		--eval "(make-directory \"$(BYTE_COMPILE_DIR)\" t)" \
		--eval "(setq byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") \"$(BYTE_COMPILE_DIR)\")))" \
		-f batch-byte-compile $(SOURCES) $(TESTS)

load:
	$(BATCH) --eval "(require 'flex-x)"

package-lint:
	$(BATCH) -l package-lint -f package-lint-batch-and-exit flex-x.el

checkdoc:
	$(BATCH) \
		--eval "(progn (require 'checkdoc) (dolist (file (mapcar #'symbol-name '($(SOURCES) $(TESTS)))) (with-current-buffer (find-file-noselect file) (checkdoc-current-buffer t))))"

check-declare:
	$(BATCH) \
		--eval "(progn (require 'check-declare) (check-declare-directory default-directory))"

clean-elc:
	rm -f *.elc
	rm -rf -- "$(BYTE_COMPILE_DIR)"
