---
title: Name the nested quotation refusal
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T11:16:27.816516+03:00"
---

Problem: a quotation opened inside a quotation, '[: [: 1 ;] execute ;] execute', exits 75 with the bare token '[:' on stderr and nothing else (measured 2026-09-18 by the forth-card lane on release 5a3d9f82): no code, no word name, no file or line, no reason; docs/forth.md's rule (only one '[:' may be open at a time) is right but the diagnostic is unusable, the same naked-exit class the capacity lane just closed for BODYBUF-CAP and BEGIN nesting. Acceptance: the refusal is named on fd 2 with the definition and the reason (a quotation may not open inside a quotation) through the labeled tail the other compile refusals use (LCOMPILEDIE, catchable inside evaluate, exit 75 at top level unchanged), with a JSON code and repair class if the checker is the layer that sees it (E-BAD-LOCAL-SHAPE's neighbour) or the engine's prose line if the engine refuses before the checker; a case in test/runtime-regression-test.f or test/compiler/ pinning the message red-first; docs/forth.md's quotation rule cites the message. Files: src/habu/habu2.f or src/core/checker.f (measure which layer refuses), test/, docs/forth.md. Verify: the fixture; three generations with cmp if a baked file changes; test/run.f. Depends: none. Ownership: engine or checker, by measurement. Claim: unassigned.
