---
title: Let a CMD command carry its record
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:32:58.577773+01:00"
---

Problem: src/lib/process-command.f:66-123 lay a command out as 14 cells, a vectors record at an offset (>VECTORS :62) and two PATHZ-CAP byte regions; MINT and ERASE (:129-130) carry ^n. Acceptance: the command is one STRUCTURE with BUFFER fields for the two paths, which MINT and ERASE carry; no cast or offset reaches it. Depends: habu-structure-s-buf-cb3240cc. Claim: unassigned.
