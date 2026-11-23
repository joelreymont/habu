---
title: Replace the SB string builder with vectors
status: open
priority: 2
issue-type: task
blocks:
  - habu-reject-a-vector-0feb8b8f
created-at: "2026-11-23T10:59:30.577142+01:00"
---

Problem: the SB builder (src/lib/string.f SB-RESET, SB-APPEND, SB-APPEND-C, SB$) is a task-local string buffer with a runtime capacity check (E-STR-CAPACITY); Joel 2026-11-19 'drop buffers', AGENTS.md no runtime checks. 8648 call sites (Habu 4378, Zen 1464, Tender 2259, Maki 547). Acceptance: no SB word remains; each caller writes into a VEC:vector<u8> with an owner; no capacity check runs. Depends: habu-one-str-pkg-03f714ea. Claim: unassigned; needs its own design.
