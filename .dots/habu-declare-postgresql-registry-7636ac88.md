---
title: Declare PostgreSQL registry capacities
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T19:00:07.521133+03:00"
---

Owner: Cedar. Problem: fixed CONN-CAP=8 prevents Tender pools from starting; witnessed by ~/.cache/tender/habu-gaps/db-conn-cap/conn-cap-repro.f. Acceptance: the application declares connection/result capacities before use, allocations match those capacities, live handles prevent reconfiguration, stale ownership and result lifetimes stay enforced. No larger hidden constant. Verification: real connections at the requested capacity, capacity refusal and reuse, existing PG live suite, Tender DB and job pools integration.
