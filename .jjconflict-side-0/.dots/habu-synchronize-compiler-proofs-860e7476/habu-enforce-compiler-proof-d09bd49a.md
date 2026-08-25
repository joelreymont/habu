---
title: Enforce compiler proof gates
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.850149+02:00"
blocks:
  - habu-compose-gpu-compiler-308b9a57
  - habu-compose-native-compiler-cce6c85c
---

Full context: make schema parity, Rocq build, no-Admitted, assumptions report, shared vectors, corrupt rejection, and composed native/GPU theorems owning integration gates. Acceptance: a schema drift, admitted theorem, unexpected axiom, or witness mismatch fails the exact master gate before cutover.
