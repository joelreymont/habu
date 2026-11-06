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
Research (jerry, 2026-11-06, master ed11b635): PE rows are per primitive row (PRIM-ADD :11085, PRIM-FIRST-IDX finds a symbol's first); their flags beside PE-ACTIVE are each read by a caller's check: PE-TRUSTED-ONLY (PRIM-TRUSTED-ONLY! :11093; read :11157, :18001 a checked body's call refused, :19156), PE-TOP-ONLY (:11096; :11166 PE-TAKES? path), PE-FIXED-ABI (:11099; SELECTED-PRIM-ABI? :14690), PE-C2-INIT and PE-C2-RECORDS (PRIM-C2-STOW! :11102; C2-STOW-PRIM-KIND :19158). PIMM: PARSE-IMM (:22286) stores sym and count in PIMM-SYMS/PIMM-NS, read through PIMM-IX/PIMM-CNT@ (REJECT-IMMEDIATE). `parses:` and `parses-through: W n ( E1 E2 )` rows live only in verify-source.f's scan (PRS-OWNER/SYM/EFF/COUNT/AT/LEN/TERMS, keyed by binding; 'the load checks them and keeps nothing'), so on an entry they become the word's own fact, terminators included. Spelling to settle in the brief: C2b2's tag `parses` is the CTL-PARSES flag; the rows' tags could take their declarer's spelling (`parses: 2`, `parses-through: 2 ( E1 E2 )`, `parse-imm 2`).
