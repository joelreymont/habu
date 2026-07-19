---
title: Relocate absolute helper calls in AOT images
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T11:12:08.655412+02:00\""
---

Found by the size-guard slice-1 worker (2026-07-19) while building the destruction-review end-to-end test: an AOT image whose MAIN performs a layout-bundle store builds successfully but crashes (SIGSEGV) at runtime. Cause: the store's guard path (LP2STORE) reaches the (PROT-SPAN) helper through an absolute movz/movk + blr call (C-CALL-EMIT-ABSOLUTE, src/habu/habu2.f:100), and the AOT code-literal relocation class (src/habu/habu2.f:3692) only rebases data/blob address ranges by a fixed delta — it never maps engine-text helper addresses through the closure, so the call ships pointing at the build-time engine address (worker evidence: crash PC 0x10447c4d4, outside the stripped image __text at 0x100001000; a declaration-only layout image runs clean, so inclusion is fine and only the absolute-call relocation is missing). Fix options: (a) map engine-text targets through the closure in the code-literal relocation class, or (b) convert LP2STORE's LPROTSPAN call to a direct BL, which the landed slice-1 direct-branch machinery already relocates — option (b) is expected to arrive with the PROT-GUARD:CALL rollout (size-guard slice 3, preserved commit 687a469a in .jj-ws/habu-recover-size-guard-31d26b61). Whichever fix lands, this dot also owns: the end-to-end positive gate (an AOT image whose MAIN does a layout-bundle store, proving helper inclusion + relocation + execution — the test the destruction review demanded, currently impossible without the fix), and an audit of every other C-CALL-EMIT-ABSOLUTE site reachable from image code for the same un-relocated-engine-address hazard. Depends on the size-guard slice-1 stack (42853755..5172bf5a) merging first.
Claim: agent=prot-guard workspace=.jj-ws/habu-relocate-absolute-helper-dbb53aef
