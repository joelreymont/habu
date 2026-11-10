---
title: Check a sealed reopen in check-all-errors BUF and FILE
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T21:33:20.762487+02:00"
closed-at: "2026-11-10T21:50:20.791456+02:00"
close-reason: merged into habu-check-src-through-1198f4ed
---

Problem: tools/check-all-errors.f certifies a program that check.f and --load refuse: `bin/hb --load tools/check-all-errors.f -- --label x s4.f` (and s1.f, s6.f in /tmp/jerry-sealedrv-tmp, each reopening a sealed engine package) answers rc 0 with no output, on the public hb and lane sealed's candidate alike. src/lib/verify-source.f SEALED-LOAD? asks the seal only when composing (VERIFY:SOURCE-COMPOSE*), and CHECK-ALL-ERRORS:BUF / FILE do not compose; the build certifies through VERIFY:SOURCE-BUF, which does ask. check-all-errors.f is a command, not only a harness. Acceptance: BUF and FILE refuse a sealed reopen with E-SEALED-PACKAGE at the reopen's token, as check.f does; no row relies on the skip (the BUF/FILE harness sources use packages they create); s1/s4/s6 become rows in tools/check-all-errors-test.f failing first on the installed hb. Files: src/lib/verify-source.f (SEALED-LOAD? and its callers), tools/check-all-errors-core.f, tools/check-all-errors-test.f. Verify: the probes; rows check-all-errors-test, check-verify. Depends: lane sealed landed. Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
