---
title: Share the tree-copy helper of builder e2e tests
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T10:40:51.444190+02:00"
---

Problem (review 267): test/compiler/native-hookless-reject.f:395-413 PARENT-U/COPY-MEMBER/COPY-TREE is the third copy of the helper in test/host-checker-row-e2e.f:72-103 and test/native-builder-image-e2e.f:57-90. Also native-hookless-reject's window cases assert `RC @ 0 T<>` where the stated contract is BUILD-RC 74. Acceptance: one shared test helper (lib or test library, real package, typed effects) used by all three; the window cases assert `74 T=`; the three tests rc 0 unchanged otherwise. Base: after keeparity b4977a65 lands.
