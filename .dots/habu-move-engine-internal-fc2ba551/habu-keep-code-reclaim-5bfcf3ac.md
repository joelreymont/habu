---
title: "Keep CODE-RECLAIM:TRUNCATE from programs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:43:11.070621+01:00"
---

Problem: CODE-RECLAIM:TRUNCATE (src/habu/xref.f:852-857, public) is the public form of cp!, which is private to engine packages. Its E-LIVE check (:855) compares the floor with the highest live routine's start only, so a floor inside the last live routine passes: on the installed hb, `: PRB-A ( -- n ) 0 ndict@ max cp@ max ndict@ max cp@ max ndict@ max cp@ max ndict@ max cp@ max 1 and ;` then `s" PRB-A" XREF-FIND XREF-START 8 + CODE-RECLAIM:TRUNCATE`, `: PRB-B ( -- n ) 99 ;`, `PRB-A . cr` compiles PRB-B over PRB-A's body; PRB-A prints 0, leaves a cell, and the run exits 70 E-EVAL-RESIDUE. Its callers are engine packages: src/habu/interpreter.f:3102 and :3111, xref.f:873, src/habu/linker.f:5764, src/core/generated-declaration-dictionary.f:25; test/code-reclaim.f:91 and :100 call it as a program. It is also the only public route to cp!'s task guard (src/habu/habu1.f:1365). Acceptance: a program's call of TRUNCATE is E-UNDEFINED under tools/check.f --verify-only and when it runs; the engine callers build and work; the reproducer is rejected. Depends: none. Claim: unassigned.
