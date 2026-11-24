---
title: "Read model.f's JSON by value"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.502687+01:00"
---

Problem: src/lib/net/model.f has 15 READ-INIT sites and a reader field in its record (:158). Acceptance: three commits by region (:1768-1801, :2326-3173, :3521-3632) read by value; the last deletes the reader field. Depends: none. Claim: unassigned.
