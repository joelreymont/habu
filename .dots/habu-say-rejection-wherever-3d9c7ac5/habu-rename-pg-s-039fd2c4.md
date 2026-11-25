---
title: "Rename PG's public refused words to rejected"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T15:38:21.194760+01:00"
---

Problem: PG:REFUSED$ and the refused variants of PG's connect-result and progress (src/lib/pg.f) say refused; Zen, Maki and Tender call them, and all three already change their PG calls at the next install. Acceptance: PG:REJECTED$ and the rejected variants replace them, no old name left in Habu, the pg suites pass, and the install notice names each renamed word. Depends: habu-pg-refused-answers-863744ed. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rej-pg.
