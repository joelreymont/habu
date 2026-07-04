---
title: Fix 0 USIGS ! byte-ptr cell store rejected by certify
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:28:23.818485+02:00"
---

Live certify rejection: fixpoint install prints 'certify: stdin-src rejected rc 70 / habu: in usigs-clear: at !'. Cause: revert 4d669312 (Revert 'Type mmap results through checker') replaced typed USIGS-HEAD ( -- ptr a ) with 0 USIGS ! at src/core/checker.f:2336 where USIGS ( -- ptr u8 ) — cell store through byte ptr, correctly rejected by the checker. Invisible on the self-host load path (checker.f loads in the bootstrap window before the checker can check itself); only VERIFY:SOURCE-BUF certify catches it, and certify is non-blocking so gates stay green. Runtime behavior is correct (zeroes pool head cell); only typing is wrong. Fix: restore a minimal typed head accessor (TRUSTED: USIGS-CELL-AT ( n -- ptr a ) / USIGS-HEAD, with TRUST manifest row + ratchet bump) without re-landing the reverted typed-mmap work; then force a fixpoint rerun (HABU_FIXPOINT_STAMP override) and clear ALL remaining certify rejections from the three reverts (cbae427b, 03acaef3, 2b86d5f3) until stage2-src certifies rc 0. Repro probe: scratchpad certify-probe.f pattern — require src/habu/verify-source.f, VERIFY:SOURCE-BUF on /tmp/stage2-src. Minimal fixture proving the miss class: variable P : BASE ( -- ptr u8 ) P @ ; : CLR ( -- ) 0 BASE ! ; rejected rc 70 on plain load.
