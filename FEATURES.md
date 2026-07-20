# Features

This file defines the feature boundary for `flex-x`. Keep it short: list
user-visible completion behavior, compatibility commitments, performance
requirements, and non-goals. Put implementation details in tests, code, or
`README.org`.

## Required Behavior

- Register `flex-x` as an Emacs completion style that extends the built-in
  `flex` style.
- Match every space-separated input term while respecting completion boundaries
  and filename completion filtering.
- Sort candidates by history first, then flex match quality, without discarding
  existing metadata sort functions.
- Annotate and highlight matches with standard completion properties and faces,
  including lazy highlighting when available.
- Highlight a candidate as a whole with the package face `flex-x-highlight`
  when every search term occurs contiguously in the candidate or matches a
  concatenation of prefixes from consecutive words delimited by separators or
  CamelCase boundaries.
- Use Corfu history for history ranking when `corfu-history-mode` is active
  outside the minibuffer, without requiring Corfu at load time.
- Use Emacs flex cost information when available, including match positions for
  highlighting and lower-cost sorting.
- Support optional extra matchers and regexp expanders such as migemo or pyim
  without requiring third-party packages at load time.
- Keep extra matching bounded by default by stopping predicate-driven candidate
  enumeration when the configured limit is exceeded, and cache generated
  patterns per completion request.
- Apply ignored-extension filtering after extra matching so an ignored file is
  retained when it is the only matching candidate.

## Non-Goals

- Replacing Emacs completion frontends, completion tables, or the built-in flex
  matching algorithm.
- Requiring migemo, pyim, Corfu, Vertico, or another third-party package.
- Adding frontend-specific UI behavior beyond standard completion metadata and
  optional history integration.
- Performing unbounded full-table scans by default.
- Adding speculative options, compatibility paths, refactors, or optimizations
  without a listed behavior, clear bug, or measured target.
