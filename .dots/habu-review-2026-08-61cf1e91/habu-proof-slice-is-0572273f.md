---
title: proof slice is outside the commit gate and no prover here
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.051233+02:00"
---

Problem: formal/ is compiled only by the seven test/compiler/*-proof.f gates (rocq-run.f:29), selected only by the 'proof' slice (test/gate-stdlib-lib.f:374-382, phase 40 of test/run-lib.f:99); the commit gate in docs/forth.md:1077-1085 and CLAUDE.md never names it; no CI; rocq/coqc absent on this host, so no commit made here can re-check the proofs while docs/bootstrap.md:20 says they stand behind the gate. MEASURED 2026-08-23: Rocq 9.2 installed in a user-space opam switch (~/.local/bin/rocq wrapper; Arch ships 9.1.1, below the documented minimum); `bin/hb --load test/gate-stdlib.f -- proof` on master 3e698efe: PASS compiler-ir-intern, ir-id, ir-structure, checker-model, reloc, ir-storage, codegen-spill-probe (5.6-49 s each) - the committed manifests match 9.2's output; compiler-insn-proof RED kind=TIMEOUT-UNDER-LOAD with six lanes building on the box (the known slowest proof, 99.5 s on macOS; the uncalibrated host budget class, habu-runner-budgets-uncalibrated-cb11c328) - re-run solo owed. Acceptance: the commit gate names the proof slice for diffs touching formal/, src/core/checker.f, src/compiler/ir/, src/arch/arm64/, src/habu/{habu2,snap-lib,layout}.f; rocq >= 9.2 installed here (user action) and the slice run once to confirm the committed manifests match. Files: docs/forth.md, CLAUDE.md. Verify: the proof slice green on this host. Depends: prover install. Ownership: proofs. Claim: unassigned.
