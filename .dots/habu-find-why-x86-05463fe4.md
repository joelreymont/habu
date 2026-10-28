---
title: Find why x86 booted images fail EXPECT-BALANCED
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:38:46.785738+02:00"
---

Problem: every booted x86-64 kernel test image (test/x86-64-boot-harness.f X64HARNESS:EXPECT-BALANCED, used by test/x86-64-kernel-*.f) fails its balance check when run on joel@pulsar-ua-106: an image with an empty case exits 22 (its balance check), qsort in test/x86-64-kernel-callback.f exits 29. The gate rows only build these ELFs; nothing runs them. Found by L4-C1 (habu-answer-the-cb-d1fbe53d). Acceptance: root cause at the responsible layer (harness, kernel boot, or the check), fixed; the booted kernel images that should exit 0 do so on the peer. Files: test/x86-64-boot-harness.f and what the cause names. Verify: build the ELFs here, run them on the peer, before/after exit codes. Depends: none. Ownership: the harness. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-bal.
