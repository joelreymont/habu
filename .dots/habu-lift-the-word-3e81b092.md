---
title: Lift the word cap in tools/tier-census-join.f
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T19:20:43.957214+03:00"
---

tools/tier-census-join.f throws E-JOIN-CAPACITY (-7730) above a fixed 8192 words; maki alone has 15094 (tier census, habu-census-tier-1-aed47e19). Size its table from the input instead of a fixed limit. Acceptance: joining maki's tier-0 and tier-1 census files succeeds and pairs every row. Claim: jerry, worker in .jj-ws.noindex/jerry-census.
