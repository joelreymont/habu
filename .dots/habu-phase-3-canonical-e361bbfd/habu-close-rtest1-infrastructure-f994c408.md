---
title: Close rtest1 infrastructure failures
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.272937+02:00\""
closed-at: "2026-04-04T17:46:47.162435+02:00"
close-reason: "done: canonical wrapper and clean-path inventory now leave only compiler, numeric-tower, and reader buckets in docs/maxima-rtest1-clean-path.json; no remaining loader/stream/condition/harness bucket in the classified residue set"
blocks:
  - habu-classify-clean-path-ea35b5be
---

Problem: rtest1 still contains hangs or infrastructure failures unrelated to semantic comparison mismatches. Acceptance: rtest1 no longer fails from loader, stream, condition, or harness defects. Files: ../maxima/tests/rtest1.mac and classified subsystem files. Verify: canonical rtest1 run reaches only real semantic diffs or passes. Blockers: habu-classify-clean-path-ea35b5be.
