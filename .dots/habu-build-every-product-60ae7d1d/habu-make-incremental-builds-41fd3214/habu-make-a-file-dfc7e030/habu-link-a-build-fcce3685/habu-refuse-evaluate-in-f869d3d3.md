---
title: "Refuse evaluate in a stripped program's closure"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T01:44:16.278633+02:00"
closed-at: "2026-11-06T02:30:11.602411+02:00"
close-reason: "landed: the link refuses evaluate, evaluate-range, evaluate-closed (link-band.f UNSAFE-NAME?); case eval in arm64-link-data; reviewed twice; arm64-link-data, arm64-place, arm64-link-run pass"
---

Problem: a stripped arm64 program whose closure calls evaluate reaches B-EVAL (src/habu/habu1.f:1913), which branches through LMAINP-CELL; hb writes that cell at start (src/habu/habu2.f:8980) but a stripped product's image holds 0, so the call jumps to address 0. src/habu/link-band.f UNSAFE-NAME? refuses other words a stripped product cannot run but not evaluate (the L5b2b review, read only, not run). Acceptance: the link refuses a closure that reaches evaluate (or any word needing the interpreter) with a named message, as UNSAFE-NAME? does today for its words; a test case in test/compiler/arm64-link-data-bad.f proves the refusal. Files: src/habu/link-band.f, test/compiler/arm64-link-data-bad.f. Verify: bin/hb --load test/run.f -- arm64-link-data. Depends: habu-s6-l5b2b-linker-b34bfda1. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-eval; brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-eval.md.
