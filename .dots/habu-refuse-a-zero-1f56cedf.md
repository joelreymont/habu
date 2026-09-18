---
title: Refuse a zero divisor by name in tier-1 code
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T08:58:57.438052+03:00"
---

Problem: src/compiler/native/emit.f PUT-SDIV emits its own 'CBNZ +2 / BRK / SDIV' guard (INSNS-OF hard-codes the three-instruction form), so once the engine primitives / mod /mod throw E-DIV-ZERO (habu-refuse-int-division-639af5fa, landing 2026-09-18) a tier-0 program refuses by name while a tier-1 compiled word or an AOT-built executable still dies with a SIGTRAP register dump (measured: '1 set-tier : DZ ( n n -- n ) / ; 7 0 DZ' traps with the operands in x0/x1); src/compiler/hir.f:159 and :720 and src/compiler/native/elaborate.f:3389 assert in comments that div carries 'the refusal the engine's mod makes', which is now false for tier 1. The native compiler has no throw lowering at all (its trap ABI NTRAP dies with a message and an rc). Acceptance: the emitter gains a throw-raising trap form (materialise the error code, push it, tail-call throw through NDICT:CALL-TARGET, with relocation and INSNS-OF re-derived), PUT-SDIV uses it so a tier-1 zero divisor throws the same E-DIV-ZERO catchable in checked code, the three comments corrected, a tier-1 and an AOT-executable regression beside the tier-0 one in test/prim-parity.f or test/compiler/, docs/forth.md's tier-1 caveat removed; the design records whether other NTRAP dies (bounds, stack) should become catchable the same way. Files: src/compiler/native/emit.f, src/compiler/hir.f, src/compiler/native/elaborate.f, test/compiler/, docs/forth.md. Verify: the regressions at tier 1 and in a stripped executable; byte fixpoint; test/run.f. Depends: habu-refuse-int-division-639af5fa. Ownership: native compiler. Parent: habu-campaign-c1-finish-1f129a00. Claim: unassigned.
