---
title: Delete workspace.f and the CWORK words
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:55.613446+02:00"
blocks:
  - habu-own-ir-build-6d3e6cf0
  - habu-pass-native-pass-71eae056
  - habu-own-driver-and-66b15325
---

Lane 5 of /Users/joel/.cache/tmp/carl-file/wstate-recommend.md: delete src/compiler/workspace.f, every CWORK word and the record machinery (IR-CTX:RECORD, module keys, tolerant ABANDON); dict.f, tools, checker/verify-source rows, internal-word-gate rows; delete test/native-file-cwork-e2e.f and its gate row; the session closes before both ends of IMAGE-TAKE/IMAGE-DIFF at the edge lane's seam. Proof: define -> require -> define with compiler declarations inside the observed DATA range; cold/warm file objects show no compiler writes while a real earlier program write still refuses; full native suite; convergence. Claim: unassigned.
