---
title: Prove staged compiler fixpoint
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.528767+02:00"
blocks:
  - habu-build-staged-candidate-3ad0825a
---

Full context: design sections 11 and 14.9 require a byte-identical new-compiler fixpoint and linked bootstrap proof. Acceptance: two consecutive staged rebuilds are byte identical with matching manifests/digests; assumptions report is complete; recovery/native paths agree; any hidden legacy dependency fails the proof.
