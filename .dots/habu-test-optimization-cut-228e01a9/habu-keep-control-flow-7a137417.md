---
title: Keep control-flow depth per definition and quotation
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:33:57.627182+02:00"
---

Problem (r4-ifsemi lane, 6e3e8f2a, dot 3cf9a606): (1) src/habu/habu2.f EM-RESET-COMPILE-STATE (~:10305) zeroes PEND-CELL, QPATCH-CELL, JIT-SNAP:SP-CELL and JIT-QUOT:SP-CELL but not the control-flow stack depth (DBASE+CFSTK-OFF cell 0), so after a compile refusal caught inside an open if/do/case the depth stays non-zero until the next definition head; whatever reads it in between (the new ';' refusal, snapshot quiescence if it reads it) sees a stale structure. (2) J-SEMIQUOT (~:3108) does not check that the depth at ';]' equals the depth at '[:', so '0 if [: then ;]' patches the outer if from inside the quotation and reaches ';' with depth 0. Acceptance: a caught refusal leaves the control-flow depth zero (the write goes through the band's owner, PROT-EMIT:LCF, as the worker found); ';]' refuses a quotation that closes a structure it did not open, or opens one it does not close, with the quotation's name/location, before publishing; cases for both through the real load path seen failing first (runtime-regression-test GE-RAWEXIT-RESIDUAL is where the sibling rows live); baked: rebuild, g1 == g2, two-generation build. Base: after the master merge and 6e3e8f2a land.
