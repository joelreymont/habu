---
title: "Refuse a NUL inside a child's argument"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T12:09:33.870479+02:00"
---

Problem: src/lib/process-command.f:248-253 CMD:ARG+ and src/lib/process-argv.f:96-100 PROC-ARGV+ append an argument's bytes and a terminating NUL without checking the bytes, so an argument holding a NUL reaches the child cut at it: 'a\0b' given to /bin/echo prints 'a' and the run exits 0 (probe .jj-ws.noindex/tim-nul/.out/review/probe-arg.f, found by the review of tim's NUL env-row lane). Same cause as habu-refuse-a-nul-4bfde9f3 (environment rows), different writers. Acceptance: both writers refuse an argument holding a NUL with E-PROC-OUTPUT at the point they accept the span, before any byte is appended, in the shape of their existing length refusals; rows in src/lib/process-command-test.f (a NUL argument through CMD:ARG+) and src/lib/process-argv-test.f or process-test.f (a NUL argument through PROC-ARGV+) that fail without the change and pass with it. Files: src/lib/process-command.f, src/lib/process-argv.f, their tests. Verify: hb --load src/lib/process-command-test.f </dev/null; hb --load src/lib/process-test.f </dev/null (needs bin/hb in the workspace); hb --load tools/check.f src/lib/process-command.f </dev/null; hb --load tools/check.f src/lib/process-argv.f </dev/null. Depends: none. Ownership: src/lib/process-command.f ARG+, src/lib/process-argv.f PROC-ARGV+, their tests. Claim: unassigned (tim takes it after habu-refuse-a-nul-4bfde9f3, hand-in through review).
