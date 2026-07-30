# Features

This file defines the feature boundary for `flex-x`. Keep it short: list
user-visible completion behavior, compatibility commitments, performance
requirements, and non-goals. Put implementation details in tests, code, or
`README.org`.

## Required Behavior

- Register `flex-x` as an Emacs completion style that extends the built-in
  `flex` style.
- Match a single input term with built-in flex matching.  Once whitespace
  occurs in the input, match every non-empty term as an
  order-independent literal substring while respecting completion boundaries
  and filename completion filtering.  Input containing only whitespace does not
  filter candidates.
- When completion metadata explicitly preserves candidate order with an
  `identity` sort function, match a single input term literally instead of
  returning unsorted fuzzy matches.
- Sort candidates by history first, then flex match quality, without discarding
  existing metadata sort functions.
- Preserve an explicit `identity` metadata sort function, which marks
  candidates as already sorted and disables additional display or cycle
  sorting.  If either sort channel uses `identity`, do not add flex-x sorting to
  either channel; preserve another explicitly configured sort function and use
  `identity` for an unspecified channel.
- Annotate and highlight matches with standard completion properties and faces,
  including lazy highlighting when available, and preserve lazy highlighting
  across nested eager completion requests.
- Highlight a candidate as a whole with the package face `flex-x-highlight`
  when every search term occurs contiguously in the candidate or matches a
  concatenation of prefixes from consecutive words delimited by separators or
  CamelCase boundaries.
- Use Corfu history for history ranking when `corfu-history-mode` is active
  outside the minibuffer, without requiring Corfu at load time.
- Use Emacs flex cost information when available, including match positions for
  highlighting and lower-cost sorting.
- Support optional regexp expanders such as migemo or pyim without requiring
  third-party packages at load time.  Keep expanded regexp matching available
  as an alternative to literal matching after whitespace is entered.
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
- Requiring migemo, pyim, Corfu, Vertico, or another third-party package.
- Adding frontend-specific UI behavior beyond standard completion metadata and
  optional history integration.
- Performing unbounded full-table scans by default.
- Adding speculative options, compatibility paths, refactors, or optimizations
  without a listed behavior, clear bug, or measured target.
