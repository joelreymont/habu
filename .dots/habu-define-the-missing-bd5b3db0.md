---
title: Define the missing AOT names storage reserve
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:29:07.035686+03:00"
---

Problem: src/habu/aot-decl.f:139 AOT-NAMES-RESERVE calls AOT-NAMES-STORAGE-RESERVE, which is defined nowhere in the tree, so tools/build-fixpoint-refresh.f -- snap (the driver of the REPL snapshot writer) fails at certify with 'stage2-src rejected rc 70 ... undefined word AOT-NAMES-STORAGE-RESERVE'. Found by the independent review of the tier stack on 2026-09-12; identical on 495dea80 and on the stack, so pre-existing. Acceptance: the word is defined by its owning storage package (or AOT-NAMES-RESERVE repointed to the word that replaced it, with the rename explained), build-fixpoint-refresh.f -- snap runs to completion on the root, and a gate case loads the aot-decl path that reaches it so an undefined word there is red before certification. Files: src/habu/aot-decl.f, tools/build-fixpoint-refresh.f, test/. Verify: bin/hb --load tools/build-fixpoint-refresh.f -- snap; the gate. Depends: none. Ownership: rowan (src/habu). Claim: unassigned
