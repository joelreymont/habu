---
title: Run the booted x86 harness images on the peer
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T20:54:10.620959+02:00"
---

Problem: the gate rows for the 17 test files using test/x86-64-boot-harness.f only build their ELFs; nothing runs them, so a wrong EXPECT-BALANCED failed 129 positive images unnoticed (fixed with habu-find-why-x86-05463fe4). Acceptance: one x86-peer row runs every booted-harness image against its header's expected exit code and stderr line. Files: test/ x86 peer row, the 17 headers. Verify: the row on joel@pulsar-ua-106. Depends: none. Ownership: the new row. Claim: unassigned. Also: test/x86-64-kernel-policy.f never calls T-REPORT, so it prints no 'test: ok'.
