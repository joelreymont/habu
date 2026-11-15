---
title: "Keep private words out of a program's reach"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T02:09:12.705299+01:00"
---

Problem: XREF-PRIVATE-WL (src/habu/xref.f:103) is global and answers any package's private wordlist, search-wl is public, and a private CAST: turns any n into a callable: with 'CAST: >ROW ( n -- [ ^u8 n ^u8 n -- n ] )', ': TWO ( -- n ) 2 ;' then 's" TWO" s" -- n n" s" TRUST-DECL-ROW" s" CHECKER-REG" XREF-PRIVATE-WL search-wl >ROW execute drop' then ': USE ( -- n ) TWO + ;' loads and USE underflows at run time, rc 67. Its callers: src/lib/arena.f, src/habu/verify-source.f, interpreter.f, link-map.f, link-data.f, linker.f, tools/perf-map-core.f. Acceptance: no program reaches a private word through the xref readers or a cast of an n; the probe is rejected at load, rc 70. Depends: none. Claim: unassigned.
