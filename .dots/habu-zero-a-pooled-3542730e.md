---
title: Zero a pooled WITH-BYTES slot by cells, not bytes
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T23:10:38.692502+02:00"
---

Problem: src/lib/memory.f WB-ENTER (:268) zeroes every pooled slot with SPAN:FILL one byte at a time, so a word that takes a 1 KB scope buffer per call pays for it in user time: a worker's timing of F64-TEXT:PARSE with a 1024-byte pooled buffer showed 600000 calls costing about 2 s more than the same calls with a mapped buffer (the attribution to the fill is read from the source, not measured apart). Acceptance: first measure an empty WITH-BYTES body at 64, 512 and 1024 bytes, a million calls each, and say where the time goes; if the fill is the cost, the slot is zeroed a cell at a time (slot bodies are 16-aligned) and the same measure shows it. Files: src/lib/memory.f. Verify: the measure before and after; memory's own test; the engine provides memory.f, so the engine is rebuilt and the full suite run. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: every WITH-BYTES caller; no consumer reported it.
