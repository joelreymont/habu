---
title: Fill the x86-64 kernel digest
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T15:30:14.152635+02:00"
blocks:
  - habu-make-an-app-8bd0fa30
---

Problem (Fable feature review of the B1/B1b stack, finding F3, 2026-11-02): src/habu/kernel-x64.f kernel-producer$ (~:4354) pushes zeros, so once the B1/B1b stack keys stored files by kernel role, every Linux x86-64 build has an unknown kernel role and stores nothing (master stored under the binary hash): x86-64 engine and app builds compile every file every time. Acceptance: the x86-64 kernel fills the same kernel digest arm64 fills, by the definition the built-in-files dot settles; on joel@pulsar-ua-106 a warm engine build after a cold one reuses every file and a one-file edit recompiles only its dependents, with warm == fresh cold. No public word added or changed. Files: src/habu/kernel-x64.f. Verify: the gate row's engine cases on pulsar-ua-106. Depends: the 'Make an app file depend on the built-in files it requires' dot (same digest). Arm64 first (LESSONS 2026-10-29/31): starts with the x86-64 T0 work. Ownership: carl. Claim: unassigned.
