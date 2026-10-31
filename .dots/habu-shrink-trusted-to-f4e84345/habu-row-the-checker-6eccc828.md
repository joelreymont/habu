---
title: Row the checker words the owner calls
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:28:26.199753+02:00"
closed-at: "2026-10-31T14:16:13.380507+02:00"
close-reason: "Landed e4afb1f7: MULTI-ERROR?, RECOVERY-USED?, DOES-BEGIN, DOES-FINISH and DOES-COMMIT are : definitions over PRIM: rows for MULTI-ERR?, RECOVERY-USED? and the three does> frame writers. Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: checker-owner.f MULTI-ERROR?, RECOVERY-USED?, DOES-FINISH, DOES-BEGIN, DOES-COMMIT are TRUSTED: because their callees have rows without authority (deliberate: checker.f CALL-AUTHORITY; docs/architecture.md Kernel). Two are safe reads, three owner-only writers (/tmp/jerry-big/mpre-answer.md). Acceptance: rows for MULTI-ERR?, RECOVERY-USED? and CHECKER-NATIVE-DOES-* (plain PRIM: rows, no owner rows: a kernel word checked code calls carries a trusted type at its definition, docs/architecture.md "Kernel"), the five callers :, engine build passes, user calls of RECOVERY-USED! still refused; the new rows reach nothing CHECKER-OWNER's public DOES-BEGIN, DOES-FINISH and DOES-COMMIT do not (sealing those is habu-seal-the-checker-a751051f). DEF-PREFIX-BIND and BIND-OWNER stay TRUSTED: (boundaries). Files: src/core/checker.f, src/compiler/native/checker-owner.f. Verify: engine build; rows compiler-native-create-does, program-diagnostics, tool-boundary-check-repair, check-verify-batch, check-verify. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tp1
