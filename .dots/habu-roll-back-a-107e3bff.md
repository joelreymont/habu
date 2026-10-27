---
title: Roll back a failed parser operand on arm64
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T21:12:44.074933+02:00"
---

On arm64 the tier-1 capture (kernel reader, src/habu/habu2.f EM-COMPILE) does not roll back a nested parser operand that fails: in test/compiler/native-lexical-recovery.f RECOVERY-CASE the failed operand joins LEXWORD's body, which is then refused for NEVER-BOUND. x86-64's Habu loop restores the opener before capture resumes, so RECOVERY-CASE runs on x86-64 only.
Acceptance: on arm64 a caught parser-operand refusal leaves the enclosing body as x86-64 does (RECOVERY-CASE output "70\n7\n"); RECOVERY-CASE runs on every target, the RUN gate and comment removed; no new test file.
Verification: test/compiler/native-lexical-recovery.f on an arm64 engine built from the tip; full suite on that engine in a private tree; tools/two-generation-build.f.
