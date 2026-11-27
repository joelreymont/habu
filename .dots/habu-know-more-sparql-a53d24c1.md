---
title: Know more SPARQL expressions always bound
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T19:45:15.921754+01:00"
---

Problem: emit.f SURE? counts STR(e) and IF(BOUND(?v), a, b) maybe bound when e, a and b are always bound, so a LEFT JOIN on such a variable tests IS NULL OR and PostgreSQL cannot hash or merge it (Tender's survey-path query). Acceptance: such a join's ON is a plain equality where the values are always bound; same rows. Depends: none. Claim: unassigned.
