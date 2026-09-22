---
title: Measure the type checks in emitted code
status: active
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.084825+03:00"
---

Problem: Joel (2026-09-22): 'why are there type checks in compiled code? type checking is a property of the compiler, not generated machine code' - unproved either way. Probe: a ten-word program of checked typed words built stripped (hb-build, no --repl, as Tender's scripts/build.f drives it), objdump -d, the instruction count per word against the same words written untyped; the difference is what types cost at runtime. Acceptance: both counts recorded with the engine sha, and either the emitted checks named (word, instruction pattern) with the compiler change that erases them, or the finding that no type check is emitted. Verification: the two stripped images and their disassembly kept under ~/.cache/tender/habu-gaps/emitted-type-checks/. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-size-probes.


Initial measurement on engine 6dc6cd7bd526eb5a02d8812a8912da9af8a620773ea081715a3a7fef5fc0bde3:
ten checked scalar words produced 3,260 code bytes; the same words declared
through `TRUSTED:` produced the same 3,260 bytes and the two stripped images
were byte-identical. No scalar type-check instruction was found in that probe.
This rules out scalar effect checking as the explanation for the Tender size;
runtime checks for spans and other data-bearing contracts remain a separate
question.
