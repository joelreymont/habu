---
title: "C2b3: read a primitive's flags and operand counts from facts text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T22:04:00.354206+02:00"
blocks:
  - habu-c2b-read-facts-f698adb6
---

Problem: design r2 D2: facts a caller's check reads kept outside the control word: the primitive table's flags (checker.f PE-TRUSTED-ONLY 2, a trust boundary refused in checked code, PRIM-TRUSTED-SYM?; PE-TOP-ONLY 32, a top-level keyword whose row a body token never takes, PE-TAKES?; and whether PE-FIXED-ABI 4, PE-C2-INIT 8 and PE-C2-RECORDS 16 are read by a caller's check), the parse-imm payload count (PIMM-IX, PIMM-CNT@ :22180-22187, read by REJECT-IMMEDIATE) and the parses: token count (only in src/habu/verify-source.f PRS-* :2945-2970). Acceptance: tags trusted-only, top-only, parse-imm N and parses N (decimal), plus one per other PE flag a caller's check reads, written by a render word after REND-CTL and read by SIG-TAGS into scratch cells beside SIG-CTL; a probe over every PE row, PIMM row and PRS row finds each read back equal. Files: src/core/checker.f, src/core/render.f (verify-source.f only if its count must be read there). Verify: the probe; bin/hb --load test/run.f -- engine checker-soundness. Depends: C2b1 (parallel with C2b2). Ownership: those files. Claim: unassigned.
