---
title: Compile records over 63 cells at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T03:30:36.644053+03:00"
---

Problem: ~/.cache/tmp/carl-gfrest/c/c6-wide-make.f (a 65-field STRUCTURE, never used), c6-make-64.f, c6-make-65.f and c6-make-255.f (a MAKE/UNMAKE roundtrip summing the fields) declare and run at tier 0 (1, 2080, 2145, 32640; rc 0). Tier 1 refuses the declaration itself, `ncomp: cannot compile W64:MAKE`, `habu: bad structure declaration 'w64': declaration failed at 'w64'`, rc 70, under the Habu loop and the engine loop alike (master 9751f482): 64 fields throw -8611 E-A64RAV-DKEEP (src/compiler/native/regalloc-verify.f:1761-1765), 65 and 255 fields -8303 E-NELAB-ARITY from src/compiler/native/elaborate.f:4079-4082 ARITY-CK (VMAX 64, :116); 63 fields compile and run. Behind them the IR signature list stops at 64 (src/compiler/ir/type.f:710-721, E-IR-TYPE-ARITY -6688). The language admits an input row of 255 cells (docs/forth.md:1245-1257, EFFECT-MIN-IN-MAX): tools/check.f certifies c6-make-65.f, and c6-make-256.f is refused at its declaration at both tiers (rc 70). docs/forth.md:2171-2178 records the native 64-cell ceilings as current behavior, not as a rule.
Acceptance: at tier 1 a STRUCTURE of up to 255 fields declares, c6-make-64.f, c6-make-65.f and c6-make-255.f print what tier 0 prints, rc 0, and c6-wide-make.f prints 1; c6-make-256.f is still refused at its declaration, rc 70; docs/forth.md:2171-2178 states the tier-1 width. The reproducers join test/compiler/native-generated-constructor.f, where docs/forth.md places the 34-cell record roundtrip.
Files: src/compiler/native/elaborate.f, src/compiler/ir/type.f, src/compiler/native/regalloc-verify.f and the native call ABI they bound, docs/forth.md, test/compiler/native-generated-constructor.f.
Verify: rebuild bin/hb per docs/gate.md; each reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; `bin/hb --load test/compiler/native-generated-constructor.f`; `bin/hb --load test/run.f`; two-generation build converges.
Host cases: the Gforth host's r67 (~/.cache/tmp/carl-gfrest/c/waiting/) joins test/gforth/cases/ and matches native.
Depends: none.
Worker: worker-max.
