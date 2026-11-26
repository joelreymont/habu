---
title: "Use PostgreSQL's planner as a tool for SPARQL"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:41:18.721647+01:00"
---

Problem: Joel 2026-11-26: "integrating with [PostgreSQL's cost-based optimizer] and using it as a tool". On Zen's 35-query fixture plan cost ranks queries poorly against wall time (Spearman 0.54; a 20-pattern star costs 2.7e17 and runs in 15 ms) because of Zen's view estimates, and another join order runs one chain 10 times faster. Acceptance: once cost ranks the fixture's queries as their wall times do (Spearman at least 0.9), a query over the host's cost limit is rejected before it runs, naming its costliest pattern and how to narrow it, and none of the fixture's queries under 1 s is. Depends: Zen's direct-triple estimates.
