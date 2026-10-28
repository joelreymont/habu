---
title: Delete the arm64 JIT, tier cells and Gforth seed
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.648925+02:00"
blocks:
  - habu-build-the-arm64-a2975141
---

Problem: once L7 meets the bar, the JIT, tier selection and Gforth recovery are dead weight. Acceptance: delete jit.f, regalloc.f, CF JIT helpers + keyword handlers, J-*/C-COMPILE-*, EM-COMPILE-* except the SEMI capture tail, LCOMPILE, tier cells (offsets stay reserved), set-tier/tier@ rows, the tier checks in def-open/def-close and native-definition-cache.f OBJECT-OPEN (file objects install at the one tier; habu-install-file-objects-2d6c9b17 depends on this lane), TIER:SELECT, src/lib/tier.f, test/tier.f, set-tier lines; habu1.f JIT helpers after a census; delete bootstrap/, tools/bootstrap.sh, the ddc-verify.f Gforth leg; recovery = trusted native seed or cross-build; DDC = cross-host double compile; update CLAUDE.md/docs naming Gforth recovery. Files: src/habu/habu1.f, habu2.f, jit.f, regalloc.f, layout.f, prims.f, src/lib/tier.f, bootstrap/, tools/, docs. Verify: full suite + fixpoint. Depends: L7 with bar met. Ownership: those files. Claim: unassigned.
