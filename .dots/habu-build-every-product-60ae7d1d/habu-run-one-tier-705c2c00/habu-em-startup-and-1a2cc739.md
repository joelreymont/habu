---
title: EM-STARTUP and the cold baseline are laid in every engine and never run
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.213034+02:00"
---

Problem: found by the JIT reach census (jit-census-notes.md item 4, a probe engine whose JIT entry exits 99: engine-suite, --load and stdin all rc 0 with the probe silent): habu2.f EM-STARTUP's JIT state and the cold baseline are emitted into every engine and no entry runs them. Acceptance: the engine no longer carries them, or the census shows the entry that runs them. Files: src/habu/habu2.f. Verify: an engine built without them passes the engine build's fixpoint and test/engine-suite.f. Depends: habu-delete-the-jit-d9fdaf67 (cut with its lanes). Ownership: those lanes. Claim: unassigned.
