---
title: snap-rebase straddling-range gap past endpoint guards
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:46:16.026032+02:00"
---

From 2b-v slice (e) (2026-07-04): BSNAPREBASE guards only the ENDPOINTS (base x8, end x16) via PROT-GUARD, so a rebase region that STARTS below a protected band and ENDS above it (straddles) walks through the band unguarded — endpoint checks miss it. The legit snapshot builder (snap-lib.f SNC-CANON) never straddles (high scratch mmap), so this is a hardening gap, not a live bug. Fix: range-overlap check — reject when [base,end) intersects [FRIEND-ARENA, FRIEND-ARENA+latch) or the second protected-WID band, not just when an endpoint lands inside; add a negative fixture with a straddling range (metabuild probe pattern from slice c). Files: src/habu/habu2.f BSNAPREBASE (~:2787) + test/seal.f. SEQUENCE: after 2b-v chain merges.
