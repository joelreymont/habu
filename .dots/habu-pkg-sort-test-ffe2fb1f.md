---
title: Package sort-test legacy fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T16:45:49.792286+02:00"
---

lib/sort-test.f (47 lines) has no package opener and defines eight global words: the two arrays created at lines 8 and 9 (ST-A, IT-A) and the colon words ST!, ST@, IT!, IT@, ST-ASC? and SORT-RUN at lines 11 to 20. These are exactly the raw global stems the package-first rule forbids.

Measured wall, reproduced independently on master ed3465d3 rather than inherited from the earlier lane report: a whitespace-only edit inside the body of ST@, then running bin/hb --load tools/package-diff-lint.f on the resulting jj diff --git artifact, reds with "E-PACKAGE-OWNERSHIP lib/sort-test.f:12:3: `ST@` defines a changed module word outside a package" and exit status 1. Any future body edit anywhere in this file is blocked until the file is packaged.

Exact-token external caller sweep, whitespace-delimited, whole tree, .f and .fs and .md, excluding the file itself: zero external callers for all eight names. ST-A returns 16 hits but not one is a caller of this file's array. lib/source-test.f:59 defines its own unrelated word `: ST-A ( -- ptr u8 n )` and is itself unpackaged, and maki/examples/nanogpt/from-scratch-train-test.f:23 defines `variable ST-A` inside package MAKI. That collision is a further reason to package this file: two unpackaged files currently publish a same-named global ST-A carrying different roles and effects, while the third instance already shows the correct owned shape.

This is therefore a self-contained single-file wall with no caller migration cascade. Owned result: give lib/sort-test.f its own package with short package-local tails, keeping the SORT: qualified calls across the package boundary. Run a case-insensitive collision check over the stripped tails before choosing them; the repl-lint lesson is that stripped tails shadowed c!, I and J, so reserved and core names need a role prefix.

Acceptance: the whitespace-edit probe described above reds E-PACKAGE-OWNERSHIP before the change and passes after it; the suite runs green through its exact owning path, cat lib/errors.f lib/test.f lib/sort.f lib/sort-test.f piped into bin/hb; both diff lints pass.
