---
title: Measure the type checks in emitted code
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.084825+03:00"
---

Problem: Joel (2026-09-22): 'why are there type checks in compiled code? type checking is a property of the compiler, not generated machine code' - unproved either way. Probe: a ten-word program of checked typed words built stripped (hb-build, no --repl, as Tender's scripts/build.f drives it), objdump -d, the instruction count per word against the same words written untyped; the difference is what types cost at runtime. Acceptance: both counts recorded with the engine sha, and either the emitted checks named (word, instruction pattern) with the compiler change that erases them, or the finding that no type check is emitted. Verification: the two stripped images and their disassembly kept under ~/.cache/tender/habu-gaps/emitted-type-checks/. Ownership: alder. Claim: unassigned.
