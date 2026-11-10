---
title: "An uncheckable token after a pinned failure overwrites the failure's reason"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.184221+02:00"
---

Problem: src/core/checker.f UNFIT-UNCHECKABLE stores UNFIT-WHY into MDIAG even when a failure is pinned, so a pinned refusal of kind RF-REASON can lose its reason. The same cause turns a mismatch followed by a covered name into a deferral with no packet: CALL-UNDEFINED, DO-TOK-BODY, IS-TOK and the tick raise UNSEEN whatever is pinned, and CHECK-VERDICT answers deferred for UNSEEN first. Cases: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/unck/cases/def (mark-probe.f M1 and M2, d1, d2). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a pinned failure keeps its reason and its rejected verdict whatever follows; an unseen name raises UNSEEN only while the pin is open. Files: src/core/checker.f, test/signature-nesting.f. Verify: the reviewer's case. Depends: none; a second commit on the lane of habu-unck-is-set-88095289, landing with it. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-unck-is-set-88095289
