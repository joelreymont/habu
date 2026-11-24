---
title: Read OData responses by value
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.464759+01:00"
blocks:
  - habu-walk-a-json-da90ebe1
---

Problem: ODATA:PAGE, RECORD$ and COPY-ENTITY take cursors, scratch pairs and text. Acceptance: PAGE ( ^metadata JSON:value ^VEC:vector<u8> -- JSON:value option<n> ), RECORD ( ^metadata JSON:value -- JSON:value ), COPY-ENTITY ( JSON:value ^JSON:writer ^entity-type -- ^JSON:writer ) walking by JSON:WALK, its own repeat check deleted, a deep expanded record as its test. Depends: one-pass walk lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-jodata.
