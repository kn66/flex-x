# flex-x

`flex-x` is an Emacs completion style built on the built-in `flex`
completion style.

## Features

- Space-separated AND filtering.
- Sort by minibuffer history, then flex match quality.
- Sort by Corfu history outside the minibuffer when `corfu-history-mode` is
  active.
- Highlight matches with standard completion faces, and highlight high-score
  candidates as a whole with `flex-x-highlight`.
- Add extra matchers or regexp expanders for non-ASCII candidates, for example
  migemo or pyim.

## Setup

```elisp
(use-package flex-x
  :vc ( :url "https://github.com/kn66/flex-x"
        :rev :newest)
  :config
  (setopt completion-styles '(flex-x basic)))
```

Some completion categories have built-in `completion-category-overrides`
which can prepend or replace styles for that category.  If you want
`flex-x` to be preferred for those categories too, configure category
overrides explicitly:

```elisp
(setopt completion-category-overrides
        '((buffer (styles . (flex-x basic substring)))
          (project-file (styles . (flex-x substring)))))
```

For migemo-style matching, pass `migemo-get-pattern` directly:

```elisp
(with-eval-after-load 'migemo
  (setq flex-x-extra-pattern-function #'migemo-get-pattern))
```

For pyim-style matching:

```elisp
(with-eval-after-load 'pyim
  (require 'pyim-cregexp-utils nil t)
  (setq flex-x-extra-pattern-function #'pyim-cregexp-build))
```

By default, extra matching is called only for candidates containing
non-ASCII characters.  Customize `flex-x-extra-match-nonascii-only` if
you want it to run for every candidate.

Extra matching scans up to `flex-x-extra-match-candidate-limit`
candidates for the current completion prefix.  The default limit is
`5000`; set it to nil if you want migemo or pyim expansion to scan every
candidate even in very large completion tables.

With Corfu, enable `corfu-history-mode` to sort flex-x candidates by the
recently selected Corfu candidates before flex match quality:

```elisp
(with-eval-after-load 'corfu
  (corfu-history-mode 1))
```

High-score candidates are highlighted as a whole with `flex-x-highlight`.
The default threshold is `0.05`; customize
`flex-x-highlight-score-threshold` to change it.

## Regexp Expander

`flex-x-extra-pattern-function` can be nil, one function, or a list of
functions.  Each function receives a search term and should return a
regexp string or nil.

Examples:

```elisp
(setq flex-x-extra-pattern-function #'migemo-get-pattern)

(setq flex-x-extra-pattern-function
      '(migemo-get-pattern pyim-cregexp-build))
```

## Custom Matcher Protocol

For lower-level control, use `flex-x-extra-match-functions`.

Each function in `flex-x-extra-match-functions` receives `(TERM
CANDIDATE)`.

It can return:

- `nil`: no match.
- `t`: match with `flex-x-extra-match-score`.
- a number: match with that score.
- a plist: `(:score SCORE :ranges ((BEG . END) ...))`.

`ranges` are optional candidate character ranges highlighted with
`completions-common-part`.  Matchers that signal an error are ignored so
that one broken matcher does not abort completion.

## Development

`FEATURES.md` defines the package behavior boundary.

```sh
make test
make compile
make check
```
