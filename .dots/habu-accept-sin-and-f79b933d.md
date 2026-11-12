---
title: "Accept SIN and COS up to fdlibm's medium range"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T12:01:20.510089+02:00"
---

Problem: src/lib/fmath.f:141 MEDIUM-BITS $413921FB00000000 makes TRIG-DOMAIN throw E-DOMAIN for |x| >= 1647099.0, while fdlibm's medium-size test (e_rem_pio2.c:123, ix<=0x413921fb) reduces every |x| < 1647100.0 with its three-piece pi/2; repro: a file with `require src/lib/fmath.f` and `1647099.0 FMATH:SIN drop` ends 'uncaught throw code -9020', rc 67 (1647098.9 rc 0). The file spells fdlibm's other <= tests by raising the high word (PIO4-BITS $3FE921FC00000000), not this one. The SIN/COS comment (fmath.f:311-313, '|x| below 2^20 * pi/2 (1647099.32...)') states neither. Found by the FMATH:ACOS review. Acceptance: TRIG-DOMAIN's bound is fdlibm's (SIN and COS answer every finite |x| < 1647100.0 as fdlibm does, and E-DOMAIN from 1647100.0), a test at both sides of the bound, and the E-DOMAIN comment (which says '1647099 or more (TRIG-DOMAIN's MEDIUM-BITS)' once ACOS lands) and the SIN/COS comment state it. Depends: none. Claim: unassigned.
