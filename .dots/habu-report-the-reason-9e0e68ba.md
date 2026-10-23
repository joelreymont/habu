---
title: Report the reason for a refused does> clause at tier 0
status: open
priority: 3
issue-type: task
created-at: "2026-10-24T01:29:44.213043+03:00"
---

When the checker refuses a does> clause at tier 0, habu2.f C-DIE-DOES prints only 'does> at FILE:LINE' and exits 70: no diagnostic says why. Example: ': MKW ( n -- ) create , does> ( -- n n ) @ ;'. Tier 1 reports the checker's diagnostic naming '<definer>;does' after dot habu-report-tier-1-48a9264c. Acceptance: tier 0 prints the same diagnostic as tier 1 for a refused clause, with rc 70; a test/tier.f row. Found by jerry, 2026-10-24. Not yet worked.
