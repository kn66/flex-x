# Repository Guidelines

## Project Structure & Module Organization

This repository contains a small Emacs Lisp completion package. The main
implementation is `flex-x.el`, and ERT tests live in `flex-x-tests.el`.
`README.md` documents user-facing setup and behavior. Compiled `.elc` files may
exist in the tree, but contributors should edit the `.el` sources and regenerate
bytecode only when intentionally updating compiled artifacts.

## Build, Test, and Development Commands

- `emacs -Q -L . -batch -l flex-x-tests.el -f ert-run-tests-batch-and-exit`
  runs the full ERT test suite with only this directory on `load-path`.
- `emacs -Q -L . -batch -f batch-byte-compile flex-x.el flex-x-tests.el`
  byte-compiles the package and tests, surfacing compiler warnings.
- `emacs -Q -L . --eval "(require 'flex-x)"`
  performs a minimal load check in a clean Emacs session.

Use Emacs 30.1 or newer, matching the `Package-Requires` header in
`flex-x.el`.

## Coding Style & Naming Conventions

Use standard Emacs Lisp formatting and keep `lexical-binding: t` in source
headers. Public package symbols should use the `flex-x-` prefix; private helpers
should use `flex-x--`. Test helpers should use `flex-x-tests--`, and ERT test
names should begin with `flex-x-` and describe behavior, for example
`flex-x-extra-pattern-function-is-cached-per-term`.

Prefer clear docstrings for public custom variables and functions. Keep comments
focused on non-obvious completion behavior, scoring, caching, or compatibility
with Emacs internals.

## Testing Guidelines

Tests use the built-in `ert` framework. Add or update tests in
`flex-x-tests.el` for every behavior change, especially matching semantics,
sorting, highlighting, text properties, and compatibility paths for different
Emacs completion internals. Keep tests deterministic by binding package
customization variables locally with `let`.

Before submitting a change, run the ERT command and the byte-compile command.

## Commit & Pull Request Guidelines

Recent history uses short, direct Japanese commit summaries such as
`vertico-buffer-frameの設定を調整` and `:afterキーワードを追加`. Follow that concise
style unless the project adopts a new convention. Keep commits scoped to one
logical change.

Pull requests should include a brief description, the affected completion
behavior, and the exact commands run for verification. Link related issues when
available, and include screenshots only for UI-visible highlighting changes.

## Agent-Specific Instructions

Do not rewrite unrelated Emacs configuration files outside this package while
working here. Preserve existing user changes in the wider `.emacs.d` worktree.
