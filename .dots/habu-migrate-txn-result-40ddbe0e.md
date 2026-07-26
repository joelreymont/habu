---
title: Migrate transaction result sum
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T14:22:45.962555+02:00"
---

Wave C12 of the unified-type migration program (plan file, rules R1-R7). maki/db/transaction.f:107 tx-result 1 (ok a + 4 payloadless; consumed by commit-store.f and the keywire child - run both consumer suites, the xproc child standalone too) becomes a full-mode payload ENUM, FIELD name from source (A1 recipe). Spellings byte-identical via calibrated tables; REFLECT pins per R7; FIELD-removal kill; non-zero discipline; name-cliff precheck; pins below T-RESET. Acceptance: transaction suite + consumer suites + maki/test.f; both diff lints; census verify identical. Claim: agent=mig-c12 workspace=.jj-ws/habu-mig-c12
