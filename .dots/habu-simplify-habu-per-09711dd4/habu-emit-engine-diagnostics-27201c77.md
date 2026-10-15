---
title: Emit engine diagnostics through one word
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.814323+03:00"
---

Problem: habu2.f writes the same three steps (message, offending token, newline) out by hand 27 times. Acceptance: one emitter word takes the message; all 27 sites call it; diagnostic output is byte-identical (checked by the existing diagnostic tests). Files: src/habu/habu2.f. Verify: rebuild bin/hb with tools/native-build.f (docs/bootstrap.md); bin/hb --load test/run.f; tools/two-generation-build.f convergence (engine output changes). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: src/habu/habu2.f. Claim: unassigned.
