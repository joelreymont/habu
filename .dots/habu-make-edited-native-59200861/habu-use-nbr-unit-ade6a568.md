---
title: Use NBR unit in native rebuild
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:29:28.520928+02:00"
blocks:
  - habu-import-checked-nbr-107045f9
  - habu-own-native-build-0aa191d6
  - habu-prove-reusable-native-bdd18a51
---

Use the NBR package artifact at its normal native target source-load position on an exact owned-input hit; compile from those same owned bytes on a miss. Continue to freshly check later packages. Automatic hits wait for saved-builder byte parity. Relevant files: tools/native-build-core.f and package driver. Acceptance: cold and imported edited-callee engines/.names are exactly equal, cache-off matches, target/checker dependencies invalidate, and whole invocation plus phase timing is measured on the qualified host in a cleared CPU window.
