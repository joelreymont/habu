---
title: Package checked-boundary lint core
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:10:09.049031+02:00"
blocks:
  - habu-finish-boundary-lint-07b5dd3b
---

Metadata umbrella for the checked-boundary lint migration. Ordered child dots
package the command, cut over the six-operation provider API after package
`CHECK` owns its caller, replace name-only hook allowlists with `HOOK-SITES`
identity, shorten private state/scanner/diagnostic/policy/runner names, and
remove the `UB`-prefixed surface. This dot owns no source.

Acceptance on one exact combined tree: package `CHECKED-BOUNDARY-LINT` exposes
exactly `RESET`, `JSON!`, `STRICT!`, `OUT-FD!`, `FILE`, and `FINISH`; package
`BOUNDARY-LINT-CLI` exposes no command word; no `UB`-prefixed declaration,
reference, or alias remains; and `HOOK-SITES` is the sole hook authority. Run
the real command, CBLT suite, CHECK suite, structural hostile fixtures, exact
ownership/type checks, package gate, and host lint.
