---
title: Write no zeroes into fresh checker storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:08:36.705460+01:00"
---

Problem: the checker stores zero into pages it has not written since mapping them, which makes them resident: a grow copies an old map's whole capacity, zero cells included, into the fresh one (ARENA-CELLS-COPY from ARENA-BYTES-GROW, src/core/arena-alloc.f:13, and TV-GROW's planes, src/core/checker.f:1003), and four regions mapped through map-anon hold 401,408 resident zero bytes for the program `1 2 + drop` with no named owner. Acceptance: no checker code stores zero into a page it has not written since mapping it; each site is named with its resident zero bytes before and after. Depends: none.
