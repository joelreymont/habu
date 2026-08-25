---
title: Migrate transaction result sum
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T14:22:45.962555+02:00"
close-reason: "Landed as dde711d0e26b."
---

Wave C12 of the unified-type migration program (plan file, rules R1-R7). maki/db/transaction.f:107 tx-result 1 (ok a + 4 payloadless; consumed by commit-store.f and the keywire child - run both consumer suites, the xproc child standalone too) becomes a full-mode payload ENUM, FIELD name from source (A1 recipe). Spellings byte-identical via calibrated tables; REFLECT pins per R7; FIELD-removal kill; non-zero discipline; name-cliff precheck; pins below T-RESET. Acceptance: transaction suite + consumer suites + maki/test.f; both diff lints; census verify identical.

Contract correction 2026-07-26 (C12 baseline finding): the tx-result consumer child is maki/db/keywire-xproc-env-child.f and its owning parent suite is keywire-xproc-env-test.f (not keywire-xproc-test.f; two unrelated children exist and only the -env- one touches TX:tx-result at :114,:126-127). Judgement call approved: the shape twin goes in a NEW appended TX-TEST section (own T-RESET/T-REPORT) after line 262, not inside the production TX reopen. Payload instantiated at raw n, so negatives lean on instantiation identity and mandatory payload, not raw-cell rejection - measured and accepted.
