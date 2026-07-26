---
title: Migrate promotion authority record and sum
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T14:22:45.956650+02:00"
---

Wave C11 of the unified-type migration program (plan file, rules R1-R7). maki/db/promotion-authority.f:42 PRODUCT authority 0 (slot n, tok auth-proof - a PROOF-CARRYING record: DERIVE eq impossible per the engine limit, keep hand semantics; copy the C13 granted pattern including proof checker negatives) becomes STRUCTURE with byte-identical FIELD lines; :51 authz-result 0 (ok OBLIG:evidence / not-discharged / unauthorized; consumed cross-package by commit-store.f) becomes full-mode payload ENUM with source-justified FIELD name. Spellings byte-identical; consumers untouched, commit-store suites run; REFLECT pins per R7; kills; non-zero discipline; pins below T-RESET. Acceptance: promotion-authority suite + consumers + maki/test.f; both diff lints; census verify identical. Claim: agent=mig-c11 workspace=.jj-ws/habu-mig-c11
