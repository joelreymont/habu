---
title: Model byte-offset cell access in heterogeneous records
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:50:42.428695+02:00"
---

Why: docs/forth.md already names the missing piece ('a modeled byte-offset primitive'): a record stored in a byte arena that holds cells AND byte spans (the aot-closure.f 48-byte dict record, the retired perf-watch block) cannot be typed without per-view minting today. Behavior: a checker capability that types cell fetch/store at a byte offset inside a byte-addressed record, so heterogeneous records need no trusted view pairs. Owner: checker type model + primitive effects. Dependencies: none. Acceptance: a checked fixture reads a cell field and a byte span from one record with no TRUSTED view; the aot-closure per-view mints become checked accessors. First consumer: src/habu/aot-closure.f AOT-REC views. Claim: unassigned.
