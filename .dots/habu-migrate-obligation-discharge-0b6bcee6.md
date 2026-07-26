---
title: Migrate obligation discharge and decode sums
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T12:42:17.672879+02:00"
---

Wave C10 of the unified-type migration program. maki/db/obligation.f:170 discharge-result 0 (ok evidence + 6 payloadless; consumed cross-package by commit-store.f, promotion-authority.f, evidence-applicability.f), :180 decode-result 0 (ok obligation + 5), :192 id-result 1 (A1 recipe). Full-mode payload ENUMs; spellings byte-identical (cross-package consumers make this load-bearing - run all three consumer suites); FIELD names from source. A1 pattern + kills + non-zero discipline. STOP conditions per plan; name cliff check (OBLIG package + discharge tails). Acceptance: obligation suite, three consumer suites, maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-c10 workspace=.jj-ws/habu-mig-c10
