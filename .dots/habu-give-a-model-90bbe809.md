---
title: Give a model call and an MCP peer one record
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:44:54.564745+01:00"
blocks:
  - habu-buf-buf-a-48c31f14
---

Problem: src/lib/net/model.f lays a call's state out as one byte region with hand-summed offsets (:360-375, the BUF header -HDR-AT chain, then COUNTS-AT through HASH-AT) and reaches its records by casting region bytes (>TOKENS :215, >LEDGER :237, >STREAM :250, >RECEIPT :315, >VEC-CELL :394; TOKENS..RECEIPT :571-578); src/lib/net/mcp.f's peer keeps a region span laid out the same way (URL-OFF..REGION-BYTES, :1685-1693). Acceptance: a call and a peer are each one STRUCTURE holding their vectors as ^VEC:vector<u8> fields and their records by value; no offset sum over a region and no cast from region bytes remains in either file; the model and mcp tests pass. Depends: habu-buf-buf-a-48c31f14 (its hand-laid-record holders become vector fields). Claim: unassigned.
