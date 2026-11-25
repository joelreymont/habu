---
title: Claim every header cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:28:51.626542+01:00"
---

Problem: src/habu/data-claims.f lists only some of layout.f's header cells, so CLAIMS-ASSERT lets a new cell overlap one without a row. Acceptance: every header cell layout.f names has a claim, and a duplicate offset stops the build. Depends: none. Claim: unassigned.
