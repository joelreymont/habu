---
title: "Give capture children the parent's HB_TMP"
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:39:00.862853+03:00"
---

Problem: spawn-argv-io starts its child with an empty environment (docs/x86-64.md:1216), so a child run through RUN-ARGV-STDIN-CAPTURE / RUN-ARGV-CAPTURE sees neither HB_TMP nor TMPDIR. Measured: HB_TMP=<private> bin/hb --load test/checker-unit-capture-run.f prints 'checker prepared artifact: /tmp/checker-unit-capture-e2e-*/prepared.checker-unit'. The child (test/checker-unit-capture-e2e.f:37 HB-TMP-MKDIR) falls back to /tmp, outside the gate row's private scratch. Acceptance: test/checker-unit-capture-run.f spawns its child with an environment that carries HB_TMP (RUN-ARGV-ENV-STDIN-CAPTURE with PROC-ENV-INHERIT-MISSING, docs/stdlib.md:2740-2764), and the artifact path lies under HB_TMP. Every other test whose empty-environment child makes scratch through HB-TMP-MKDIR or TMPDIR-MKDIR is found by census and fixed the same way, each listed in the commit message. Files: test/checker-unit-capture-run.f and the census list. Verify: each touched row, with the child's scratch under HB_TMP. Depends: none. Ownership: those test files. Claim: unassigned.
