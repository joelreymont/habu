---
title: UNCK is set after the token already failed
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:11:05.652695+02:00"
---

Problem: the checker's uncheckable sites set UNCK even when the token has already failed and the pin is closed (CF-PUSH src/core/checker.f:21014, CF-RECURSE :21254, and the other UNCK stores). REFUSAL-OF ranks UNCK above DEXP (:6434-6435), so the token's own mismatch is lost: 32 `dup IF` then `1 IF` prints the bare `habu: in h8: at 'IF'`, E-UNCHECKABLE with no reason, where the same `1 IF` at frame 32 prints E-MISMATCH with rows (cases /tmp/bob-review-unck/cases/c2-deep-badcond.f, c2-badcond-at32.f). CHECK-VERDICT tests UNCK (:26199), so CHECK-CANDIDATE! answers 1 (uncheckable) for a mismatch followed by 33 IFs or by RECURSE while the packet says rejected, E-MISMATCH (verdict.f there). Both engines. Found by the review of habu-tokens-refused-as-e0ec22ab. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: UNCK is set only while no failure is pinned, at every site that stores it; the first failure's code, rows and verdict stand in prose, packet and CHECK-CANDIDATE!'s answer alike. Files: src/core/checker.f. Verify: the cases; a census of every UNCK store with what each does when the pin is closed; test/signature-nesting.f gains the same-token case; the full suite, since the numeric verdict of some definitions changes from uncheckable to rejected. Depends: habu-tokens-refused-as-e0ec22ab landed. Ownership: the checker's verdict. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-unck-is-set-88095289
