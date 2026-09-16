---
title: "Sweep TRUSTED: out of the core and compiler"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T15:49:57.931374+03:00"
---

Problem: 298 TRUSTED: definition sites remain under src/ (src/core 146: enum-decl.f 39, structure-decl.f 35, generated-declaration.f 24, others; src/habu 68; src/compiler 84, checker-owner.f 67; src/os 4). Each opens a body the checker does not see. Joel (2026-09-16): TRUSTED: is to be retired entirely; agents keep reaching for it because the tree still carries it. Acceptance: every site under src/ is a checked `:` definition; where a body genuinely needs a primitive the checker cannot model, that primitive gets a package-owned PRIM axiom declared inside the owning package, never a TRUSTED: body; `rg -c "^\\s*TRUSTED:" src/` = 0; engine rebuilds to a byte fixpoint; full gate green. Work per file family (declaration machinery, checker-owner, engine emitters, compiler), one commit each. Files: src/core/*.f, src/habu/*.f, src/compiler/**/*.f, src/os/**/*.f. Verify: rg count; tools/native-build.f fixpoint; bin/hb --load test/run.f. Depends: none for a body that calls no trust-boundary primitive (the majority; start there). A body that calls a PE-TRUSTED-ONLY primitive (checker.f 6925-6945 code-injection prims, ffi-call-bounded) waits for the mechanism decision recorded in habu-delete-the-trusted-42b30edd: hazel recommends dropping the bit and making those primitives private to their owning packages, so package privacy, not a checker flag, bounds who may call them; the FFI consumers go through aspen FFI declarer. Ownership: src/. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.
