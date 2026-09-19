---
title: Copy tied x86-64 operands by liveness, not use count
status: open
priority: 2
issue-type: task
created-at: "2026-09-20T00:13:38.380281+03:00"
---

Problem (audit of 78043b90): src/compiler/native/select-x64.f:750-753 TIED-OPERAND copies operand 0 of a two-address form only when VALUE-USES > 1, and VALUE-USES sums textual operands over the function (edge arguments count only because EMIT-BR treats a br's operands as edge values). A value defined in a dominating block and read once inside a loop is not an operand of the backedge br, so it gets no copy although it is live around the backedge: B0(x,c0): br B1(c0); B1(c): t = add x c; brz t -> B2/B3; B2: ret t; B3: br B1(t) selects x64.add with operand 0 = x itself and no x64.mov (probe /tmp/audit-cu/tree/probe/x64-loop.f, a copy of test/compiler/x64-select.f with that fixture; the verifier admits the IR). The allocator's MB-TIE1 then refuses E-A64RA-TIE (OVERLAP? on the hull intervals): a refusal of a valid module, not a miscompile. Not reachable from elaborated source today (OPEN-ARGS-H makes every live stack cell a block argument) but reachable from any pass that reads a value across a backedge by dominance. Acceptance: the copy decision is 'operand 0 is live after this op' (liveness over the CFG, or at least live-out of the defining block counted as a use); that loop fixture selects mov + add with the copy as operand 0; DIFF and LOGIC still copy nothing; the allocator lane's x64 binding accepts the module. Files: src/compiler/native/select-x64.f, test/compiler/x64-select.f. Verify: test/compiler/x64-select.f; native-select; test/run.f. Depends: habu-bind-the-register-623e83ff. Ownership: hazel. Claim: unassigned.
