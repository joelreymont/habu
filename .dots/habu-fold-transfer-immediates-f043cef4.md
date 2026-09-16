---
title: Fold transfer immediates into emitted words
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T11:24:03.727750+03:00\""
---

Problem: the guard-era compiler (dcd369bf..6739c962) replaced folded immediates with JIT-STACK:LITERAL-REG plus a register-form instruction so the guard call could share the operand register, and 2dbab303 removed the guards but kept that shape and the dead operand setups (8 0 MOVZ, etc.). Every compiled wide transfer bound and every MATCH group pop is one instruction longer than before the guards: movz x10,#16 / sub x10,x19,x10 where the pre-guard compiler emitted sub x10,x19,#16; movz x16,#8 / sub x19,x19,x16 for a group pop. test/type-layout-lower-pending.f and test/match-factor-pin.f pin the pre-guard shape and fail. Acceptance: every JIT-STACK:LITERAL-REG and CELL-BYTES site in src/habu/habu2.f restored to the pre-guard fold (LIT64 base opcode, LSLI the operand into the immediate field, ORR, LCEMIT), dead operand setups removed, LITERAL-REG and CELL-BYTES deleted from src/habu/jit.f, tools/lint/clobber-lint.f and its test and fixture no longer special-case them; both pins pass unchanged; engine smaller than 6226112 bytes; byte fixpoint. Files: src/habu/habu2.f, src/habu/jit.f, tools/lint/clobber-lint.f, tools/lint/clobber-lint-test.f, tools/lint/clobber-stack-fixture.f. Verify: build from the workspace on /tmp/hazel-fG6, run test/type-layout-lower-pending.f and test/match-factor-pin.f with the new engine, rebuild on itself and cmp. Depends: none. Ownership: hazel line. Claim: agent=hazel workspace=.jj-ws/hazel-fold-imm
