# Recommended title

`flex-x: built-in flex completion with AND filtering, history-aware sorting, and migemo/pyim support`

# Post

Hi r/emacs,

I made a small completion-style package called **flex-x**. It extends Emacs's built-in `flex` style rather than replacing it with a new matching engine.

For me, `orderless` is still the gold standard among completion styles. In particular, I like how space-separated components make it easy to narrow a large candidate set, and how leaving sorting to the completion table or frontend often produces a very natural order—for example, preserving the useful order of `find-file` candidates. Its matching-style dispatch also makes integrations such as migemo and pyim possible.

At the same time, I also like one property of built-in `flex`: it scores matches, so candidates that are more closely related to the input can rise to the top. The downside is that loose flex matching can leave many candidates in the list, and those weaker matches can become noise.

`flex-x` is my attempt to combine the parts of those experiences that I find useful:

- space-separated **AND filtering**: every input term must match;
- **history-aware sorting**: minibuffer history first, then flex match quality;
- optional **Corfu history** sorting outside the minibuffer;
- customizable extra matchers or regexp expanders for non-ASCII candidates, including **migemo** and **pyim**;
- standard match highlighting, plus whole-candidate highlighting for strong literal or consecutive word-prefix matches.

That last point is meant to make a noisy flex result set easier to scan. A loose subsequence match keeps the normal per-character highlighting, while a stronger match—such as a contiguous term or a sequence of word prefixes—is emphasized as a whole.

The package is frontend-agnostic: it uses normal completion metadata and is intended to work with the completion UI you already use. It does not require Vertico, Corfu, migemo, or pyim. Optional non-ASCII matching is also bounded by default so that it does not perform an unlimited scan of very large completion tables.

Basic setup:

```emacs-lisp
(use-package flex-x
  :ensure t
  :config
  (setopt completion-styles '(flex-x basic)))
```

Migemo integration can be as small as:

```emacs-lisp
(with-eval-after-load 'migemo
  (setq flex-x-extra-pattern-function #'migemo-get-pattern))
```

For Chinese candidates, pyim can provide pinyin-based regexp expansion:

```emacs-lisp
(with-eval-after-load 'pyim
  (require 'pyim-cregexp-utils nil t)
  (setq flex-x-extra-pattern-function #'pyim-cregexp-build))
```

`flex-x` requires Emacs 30.1 or newer and is available from MELPA:

- GitHub: https://github.com/kn66/flex-x
- MELPA: https://melpa.org/#/flex-x

I would especially appreciate feedback on the matching and sorting behavior: whether history-first ranking feels natural, whether the stronger highlighting helps when flex produces many candidates, and whether the extra-matcher API works well for other non-ASCII search tools.

# Shorter title alternatives

- `flex-x: an extended version of Emacs's built-in flex completion style`
- `I made flex-x: space-separated AND search and history ranking for built-in flex`
