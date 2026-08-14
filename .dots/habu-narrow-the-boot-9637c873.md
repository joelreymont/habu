---
title: "Narrow the boot's protection flips to pages"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T10:03:27.285977+02:00"
---

The seed-the-chain leaf's LAND-NOW item (e98b03d4, stageb measurement 2026-08-11): the source-prefix boot toggles mprotect over the whole 8MB region twice per definition (8.6us/call, 119ms measured, ~12% of the 1.27s/boot checking-dominated cost; with PROT-SPAN guard cost the pair is ~20% of every boot) - and the gate runs 323 engine boots, so this pays ~every battery. The narrow-flip precedent exists: LPROTREC habu2.f:2179. Narrow the code-region flips to the touched pages; acceptance: boot-time delta measured on the real gate (before/after battery wall), no protection semantics change (the trap tests still fire), fixpoint x2. Files: src/habu/habu2.f. Depends: none - independent of the bake sequence, pays immediately.
