# Features

This file defines the feature boundary for `flex-x`. Keep it short: list
user-visible completion behavior, compatibility commitments, performance
requirements, and non-goals. Put implementation details in tests, code, or
`README.org`.

## Required Behavior

- Register `flex-x` as an Emacs completion style that extends the built-in
  `flex` style.
- Match every non-empty whitespace-separated input term with built-in flex
  matching and require every term to match, independent of term order, while
  respecting completion boundaries and filename completion filtering.  Input
  containing only whitespace does not filter candidates.
- When completion metadata explicitly preserves candidate order with an
  `identity` sort function, match every input term literally instead of
  returning unsorted fuzzy matches.
- In the minibuffer, display `[Fuzzy]` or `[Literal]` immediately after the
  prompt to identify the current flex-x matching mode without changing the
  completion input.
- Sort candidates by flex match quality without discarding existing metadata
  sort functions.
- When minibuffer completion has no explicit metadata sort function, stably
  promote matching candidates from the active minibuffer history after flex
  sorting, while preserving flex order among candidates outside the history.
- Preserve an explicit `identity` metadata sort function independently for its
  display or cycle sort channel.
- Annotate and highlight matches with standard completion properties and faces,
  including lazy highlighting when available, and preserve lazy highlighting
  across nested eager completion requests.
- Use Emacs flex cost information when available, including match positions for
  highlighting and lower-cost sorting.
- Support an optional regexp expander such as migemo or pyim without requiring
  a third-party package at load time.  Keep expanded regexp matching available
  as an alternative when built-in flex does not match a term.
- Keep expanded regexp matching bounded by default by stopping predicate-driven
  candidate enumeration when the configured limit is exceeded, and cache
  generated patterns per completion request.
- Apply ignored-extension filtering after expanded regexp matching so an
  ignored file is retained when it is the only matching candidate.
- Apply ignored-extension filtering to multi-term `try-completion` whether or
  not regexp expansion is configured.

## Non-Goals

- Replacing Emacs completion frontends, completion tables, or the built-in flex
  matching algorithm.
- Requiring migemo, pyim, Vertico, or another third-party package.
- Adding UI behavior specific to Vertico, Corfu, Icomplete, or another
  completion frontend.
- Performing unbounded full-table scans by default.
- Adding speculative options, compatibility paths, refactors, or optimizations
  without a listed behavior, clear bug, or measured target.
