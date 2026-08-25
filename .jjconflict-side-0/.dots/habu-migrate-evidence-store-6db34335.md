---
title: Migrate evidence-store load results
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T13:55:54.599405+02:00"
close-reason: "Landed as 236086693f35."
---

Waves C6+D3 of the unified-type migration program, one lane, two files, same shape: maki/db/diff-case-store.f:74 SUMTYPE load-result 1 (ok a / absent / malformed / mismatch; consumed by competitive-evidence-store.f) and maki/competitive-evidence-store.f:83 SUMTYPE load-result 0 (ok evidence / absent / malformed). Both become full-mode payload ENUMs, FIELD names from source; note the shared tail load-result across two packages - the R7 pair-keying in REFLECT pins is load-bearing here, and the cross-package twin negative (one store's constructor cannot build the other's result) is mandatory. Full recipe as D2. Acceptance: both store suites + consumers + maki/test.f green; both diff lints; census verify identical.
