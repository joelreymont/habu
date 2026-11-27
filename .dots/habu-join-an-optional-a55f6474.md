---
title: Join an optional SPARQL union branch by branch
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T19:45:15.952304+01:00"
---

Problem: an OPTIONAL over a UNION whose branches each bind one of the outer keys meets the union with IS NULL OR per key, so PostgreSQL nests loops and runs the union once per outer row (3.6 s against 254 ms written branch by branch). Acceptance: such an OPTIONAL is each branch joined on its always-bound keys with =, plus the outer rows no branch matches; same rows; hash or merge joins in the plan. Depends: none. Claim: unassigned.
