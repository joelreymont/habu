---
title: A failed evaluate brings back forgotten words
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:43:11.082298+01:00"
---

Problem: a failed evaluate restores NDICT and CP from its frame even when the evaluate lowered them, so words it forgot come back over code it reused. On the installed hb: `require src/lib/test.f`, `: PRB-K ( -- n ) 41 ;`, `: PRB-L ( -- n ) 42 ;`, then `S\" s\" PRB-K\" XREF:FORGET-DEFS-FROM : PRB-M ( -- n ) 0 ndict@ max cp@ max ndict@ max cp@ max ndict@ max cp@ max ndict@ max cp@ max ndict@ max cp@ max ; 1 throw" TEST-EVAL:RC . cr`, then `PRB-L . cr`: inside the evaluate NDICT drops by two and PRB-M is written over PRB-K and PRB-L's code; the recovery restores NDICT and CP, PRB-L is visible again and runs PRB-M's bytes, E-UNDERFLOW rc 67. On arm64 the kernel's LEVALREC stores the frame's CP and NDICT directly (src/habu/habu2.f:2151-2152); FRAME-ROLLBACK-CODE (src/habu/interpreter.f:3101-3102) does the same through ndict!. Acceptance: no word a failed evaluate forgot becomes callable again: the reproducer's PRB-L is undefined after the evaluate, or the forget is rejected inside the frame; the rollback never raises NDICT or CP above a lowering made inside its frame. Depends: none. Claim: unassigned.
