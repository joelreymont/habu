---
title: Find why a reached does> part adds 198 KB to a stripped program
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:32:34.129498+02:00"
---

Problem: in the L4-1 fold (carl-l4t, commit 93dccbec), making fixture A's stripped arm64 program reach only the does> part of test/fixtures/file-objects/reopen.f:18 MAKER (`: MAKER ( n -- ) create , does> ( -- n ) @ ;`, its does> word MAKER;does, xt stored into a private variable at the file's top level as mark.f:11-13 does) grew A's program from 33276 to 231420 bytes (+198144), past PROG-CAP 65536, so A2's read threw E-FS-CAPACITY -2106. A body of one `@` should add tens of bytes. Hypothesis, untested: the kernel cut (A64ICODE:REFS) keeps a kernel routine a does> part's entry references whose label closure reaches most of the kernel (the interpreter or create's runtime). Prediction: the kept labels with and without the does> reference differ by a routine that references the interpreter. Acceptance: the cause named as file:line with the label chain that pulls the 198 KB in; the fix a second brief, so that a program reaching a does> part whose body is `@` grows by under 1 KB; a fixture case in test/file-objects.f (L4-1's A) reaching MAKER;does once the cause is fixed (L4-1 left it unreached for this reason). Files: src/habu/linker.f (kernel cut), src/arch/arm64/ (A64ICODE refs). Verify: build A with and without the MAKER;does reference and compare program sizes and kept-label lists. Depends: L4 landing (habu-make-a-file-dfc7e030) for the fixture. Ownership: carl. Claim: unassigned.
