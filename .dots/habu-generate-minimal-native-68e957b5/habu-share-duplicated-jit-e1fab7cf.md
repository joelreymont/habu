---
title: Share duplicated JIT helper bodies
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T19:53:44.749601+02:00\""
---

Measured structural duplication at master 3909bbac. LVBINIPREP emits the complete LVBINPREP base path again after its immediate-mode probe: 58 duplicated target instructions, exactly 232 bytes. LVPUSHC and LVPUSHF emit 21 identical target instructions, exactly 84 bytes, differing only in the constant tag value 1 versus 3. Source helpers C-VBIN-* make the Forth look factored but each call expands another target body. Root cause: metacompiler factoring is being mistaken for generated-code sharing. Fix: make the non-immediate LVBINIPREP path tail-branch into the single LVBINPREP body with a compatible frame/result ABI; emit one constant-push body parameterized by tag and use tiny integer/float entry wrappers. Preserve allocation-failure atomicity and all register/free-mask invariants. Acceptance: before/after label-span attribution pins 232 and 84 duplicated bytes; final disassembly contains one binary-prep base and one constant-push body; wrappers and branches yield a measured net shrink; modes 0/1/2/3, imm12 boundaries, integer/float tags, spill-on-full, allocation failure, fold/register/fallback paths, representative JIT output, clobber lint, AOT, snapshot, bootstrap mirror, fixpoint x2, both targets, full gates, and exact ratchets pass. Files: src/habu/jit.f, bootstrap/cg equivalents, JIT/regalloc tests, engine-size attribution, and size gates.

Claim: agent=jitshare workspace=.jj-ws/fable-jitshare machine=spark (owns src/habu/jit.f + bootstrap/cg mirror + JIT/regalloc tests; THE engine-size lane this round - holds the CODELEN rows)
