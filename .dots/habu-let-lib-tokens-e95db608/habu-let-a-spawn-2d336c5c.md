---
title: Let a spawn request carry its record
status: open
priority: 2
issue-type: task
blocks:
  - habu-structure-s-buf-cb3240cc
created-at: "2026-11-22T12:32:58.588135+01:00"
---

Problem: src/lib/spawn.f:41-77 lay a request out as SPAWN-ABI cells (src/habu/spawn-abi.f, read by spawn-fds), PROC-CHUNKS cells and a vectors record at offsets (>VECTORS :47); MINT and ERASE (:31-32) carry ^n. Acceptance: the request is one STRUCTURE whose leading fields are the SPAWN-ABI record, which MINT and ERASE carry; no cast or offset reaches it. Depends: habu-structure-s-buf-cb3240cc. Claim: unassigned.
