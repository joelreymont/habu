---
title: Delete ARM-WARM-ORIGIN, dead in the native definition cache
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T03:11:57.399951+02:00"
---

Problem: tools/native-definition-cache.f:680 ARM-WARM-ORIGIN ( -- ) has no caller: rg over src/, tools/, test/ and docs/ finds only its definition (fasl point 1 worker, 2026-10-30). Acceptance: the definition is removed with any word only it used; the tool's rows pass. Files: tools/native-definition-cache.f. Verify: rg ARM-WARM-ORIGIN over the tree is empty; test/native-file-types-e2e.f and test/prefix-provided-unfilled.f with the public hb. Depends: none; the fasl load model (habu-spike-the-fasl-16b76e28) reworks the warm origin path and may remove it wholesale: close this with that lane if so. Ownership: tools/native-definition-cache.f. Claim: unassigned.
