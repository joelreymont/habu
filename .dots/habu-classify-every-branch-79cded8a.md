---
title: Classify every branch in BRANCH-CK
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T21:14:07.567022+02:00"
---

Destruction review of NCLOB, low. NPUB:BRANCH-CK (publish.f:284-316) decodes only the Bl form ($94000000/$FC000000). Complete today — no Blr/Svc/cross-routine B is emitted, A64EFF reserves BIT-INDIRECT/BIT-SYSCALL for forms that do not exist yet — but nothing refuses an emission containing another control-transfer form, so adding Blr or a tail-call B would silently escape this second derivation of the callee clobber union. Add a positive completeness check: every branch-class instruction in the emission must be one the decoder classifies; refuse the rest with a named error.
