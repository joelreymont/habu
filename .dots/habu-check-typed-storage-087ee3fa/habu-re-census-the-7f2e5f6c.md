---
title: Re-census the rule with every case run past failures
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T19:23:06.891755+02:00"
---

outer-interpret stops at its first failure, so the census saw one root per suite; the fail-continue run (/Users/joel/.cache/tmp/carl-mcoi.ld2D/ka.out) found ~20 more cases. Before the rule lands, rerun the census on the final candidate with GE-FAIL's die removed in a scratch tree.
