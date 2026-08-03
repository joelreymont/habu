---
title: Repair the codegen-compare inline red
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T23:16:18.815943+02:00"
---

tools/codegen-compare-test.f:1495 (assertion 238, 'and the four the refusal left staged make a definition that compiles') fails on proofs: SPEND-FOUR throws -8559 E-NELAB-INLINE where 0 is expected. SPEND-FOUR migrates ': FAN-CEILING-N ( n -- n ) CODEGEN-CORPUS4:C-ADD1-N CODEGEN-CORPUS4:C-MUL2-N CODEGEN-CORPUS4:C-AND7-N CODEGEN-CORPUS4:C-XOR5-N ;' through NMIGRATE:DEFINE-CALLING after STAGE-FIVE was refused, so the four staged callees have to compile. Bisected by running the suite standalone on each native landing after the assertion was introduced (509e308d4f62): 23e3cc8c7ae2, 26bf6b8f35c8, 46bcb08c059e, e8f7f2355045, bc1fc84defab and 082b7847b879 are all green; f7eb936dc665 'Name-check the splice key, unify the meaning table' is the first red, and it is the only difference (src/compiler/native/elaborate.f, inline.f, migrate.f). Reproduce with 'bin/hb --load tools/codegen-compare-test.f' - exit 1, one TFAIL. E-NELAB-INLINE is 'a callee's recorded body the elaborator cannot splice into its caller: a recorded arity that is not the effect the caller declared for that callee, a token the caller's own word model admits with a meaning the splice has no rule for, a spliced body that reaches below the values its caller was holding, or one that does not leave the vector as the callee's declared effect says' - the unified meaning table is the suspect. Found by the ratchet reconciliation lane (habu-reconcile-the-drifted-48eefbd9) while running the full test/gate-stdlib.f: this is the gate's only remaining red and it is a real regression, not a pin drift, so no ratchet was refreshed over it.
