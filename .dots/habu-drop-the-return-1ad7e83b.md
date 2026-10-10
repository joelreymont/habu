---
title: Drop the return-stack neutrality remnants
status: open
priority: 3
issue-type: task
created-at: "2026-10-10T10:11:04.258645+03:00"
blocks:
  - habu-compile-a-declared-21cc269d
---

Problem: once habu-compile-a-declared-21cc269d ("Refuse a declared return-stack cell") lands, two remnants of return-stack neutrality stay. The owner field BOUND-NEUTRAL (src/core/checker-owner-abi.f, constant 11; BOUND-CELLS 16) is written 1 for every resolved effect and read by nothing, but an engine built before the rule builds the tree with owner rows of 16 cells: removing it stopped the master-host native build at src/core/check-hook.f with E-NCOMP-OWNER -8574 (measured in that lane). NDICT:SPELL-CALL still returns a return-stack bool that both compiler callers drop.
Acceptance: BOUND-NEUTRAL is gone and BOUND-CELLS shrinks to fit, with the build host an engine that includes the rule; NDICT:SPELL-CALL returns no return-stack bool and test/ndict-spell-call.f follows; nothing else changes behavior.
Files: src/core/checker-owner-abi.f and its readers, src/compiler/native/ (NDICT:SPELL-CALL and its callers), test/ndict-spell-call.f.
Verify: native build per docs/gate.md with a post-rule host; `bin/hb --load test/run.f`; two-generation build converges.
Depends: habu-compile-a-declared-21cc269d landed. Worker: worker.
