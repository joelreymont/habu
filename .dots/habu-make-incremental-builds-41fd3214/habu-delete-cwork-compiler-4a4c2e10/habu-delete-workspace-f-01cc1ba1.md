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

Landing review of 9f905e31: CWORK's file-event machinery in src/compiler/workspace.f (FILE-IDENTITY-SET, FILE-MARK, FILE-EXPORT, FILE-PREPARE, FILE-INSTALL, FILE-PLAN-RESET, the ALLOC/DEFAULT EVENT rows, PROJECT-*/PREP-*, CHECK-FILE) has no caller in src/, tools/ or test/; compiler working state is never carried (tskmosow run-writes-cwork), so it goes with this deletion.
