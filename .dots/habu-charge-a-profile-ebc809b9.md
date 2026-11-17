---
title: "Charge a profile's callers by the frame chain"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:49:34.560957+01:00"
---

Problem: the sampler's inclusive counts and caller edges name words that are not on the stack. C-PROF-WALK (src/habu/prof.f:224-263) treats every cell from SP to the stack top whose value minus 4 falls in the code index as a frame (:240-259): a late-declared local's slot holding an earlier call's return address, an unwritten alignment pad (FPRIM frames' +8 cell, habu1.f:80-85), an xt held in a local (minus 4 names the word before it), and a stale [sp] at a prologue all count, and a tail call drops its caller. Reproducer (Maki hunt habprof, 2026-11-17): /tmp/tim-habprof/stale.f run as `hb stale.f` from ~/.local/lib/habu, output stale.out: G2 1609 inclusive of 3218 samples though it ran 17 ms, BEFORE-VICTIM 1600 though it never ran, the variable T0 100%, realpath 100%, SPIN's callers G2 and MAIN through tail calls. In Maki this put 60.7 to 68.9 s on STEP-SOURCE:JOB-SLOT, which ran none of it. Acceptance: stale.f's profile charges each word only the samples taken while it is on the stack and names SPIN's true callers; a likely fix is an exact frame chain (an AAPCS64 x29/x30 record per frame, followed by C-PROF-WALK and C-PROF-FCALLER), the owner's to settle. Depends: none. Claim: unassigned.
