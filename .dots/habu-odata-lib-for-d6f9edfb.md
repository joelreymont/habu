---
title: OData library for sapd
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.754371+01:00"
---

Goal: package ODATA (src/lib/odata.f) reads a service's $metadata (V2 and V4), V2/V4 JSON response pages (records, next link, count) and writes key predicates by Edm type, for sapd (mark's request, /tmp/mark-odata-request.md items 1-3). Acceptance: the odata suite passes on V2 and V4 fixtures; sapd reads the production order service through it. Depends: Joel's names. Claim: unassigned.
