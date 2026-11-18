---
title: Reject a does> clause name in the forget words
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T15:36:48.856444+01:00"
---

Problem: a does> definer's clause record is published one slot above the definer under NAME;does (src/habu/xref.f ~574-592), and XREF:FORGET-DEFS-FROM and XREF:HIDE-DEFS-FROM find it by that name. On the installed hb, `: PR-DEF ( n -- ) create , does> ( -- n ) @ ;` `5 PR-DEF PR-FIVE` then `s" PR-DEF;does" XREF:FORGET-DEFS-FROM` (or HIDE-DEFS-FROM) dies `checker: missing signature truncation mark`, rc 76 (EXIT-STATUS:INTERNAL, src/core/checker.f CHECKER-USIGS-TRUNCATE-FROM-RAW). Were the truncation to pass, CODE-RECLAIM:FLOOR-FROM would take the clause's start, inside the surviving definer's body, as the floor and hand back the definer's tail. Acceptance: either word given a clause name is rejected with its place and a help line naming the definer (as XREF-REQUIRE-DEFINED rejects an unknown name), before it changes anything; no input reaches the internal die from these words. Depends: none. Claim: unassigned.
