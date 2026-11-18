---
title: "Keep private words out of a program's reach"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T02:09:12.705299+01:00"
---

Problem: XREF-PRIVATE-WL (src/habu/xref.f:103) is global and answers any package's private wordlist, search-wl is public, and a private CAST: turns any n into a callable: with 'CAST: >ROW ( n -- [ ^u8 n ^u8 n -- n ] )', ': TWO ( -- n ) 2 ;' then 's" TWO" s" -- n n" s" TRUST-DECL-ROW" s" CHECKER-REG" XREF-PRIVATE-WL search-wl >ROW execute drop' then ': USE ( -- n ) TWO + ;' loads and USE underflows at run time, rc 67. Its callers: src/lib/arena.f, src/habu/verify-source.f, interpreter.f, link-map.f, link-data.f, linker.f, tools/perf-map-core.f. Problem, also: a program that reads the dictionary by position at build time (ndict@ and dbase@, src/habu/prims.f :573 and :582; xref.f's global readers XREF-REC :60, XREF-ADDR>REC :68, LATEST :70, the cell readers :79-157, SEAL-NDICT@ :628) bakes an answer no object's dependencies cover, so a warm build can differ from a cold one (dot habu-make-a-build-6931d6ea). Acceptance: no program reaches a private word through the xref readers or a cast of an n, and none reaches those position readers; the probe is rejected at load, rc 70. Depends: none. Claim: unassigned.
