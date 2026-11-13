---
title: "Release what a build space's code maps outside the space"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T20:24:43.490647+02:00"
---

Problem: after pomtnsyp (A64ICODE:ASM-RELEASE, kernel-object.f WRITE), each kernel build in one process still leaves three 16K private regions (48K resident), 16K more resident in a 1024K region and 528K of reserved address space mapped after LINKER:WITH closes (vmmap s1->s3 deltas over three kernel builds in one process; the probe's file is lost). The close (src/habu/linker.f UNDO, UNMAP :120) unmaps only the space; mappings its code took with map-anon or DYNAMIC-BUFFER stay. Owners not traced: candidates are the space's DYNAMIC-STORAGE registry (src/core/dynamic-storage.f REG-MAP), checker arenas, include frames, the intern table. Acceptance: name each region's owner by size and source, then have each owner release it when its run ends (as kernel-object.f WRITE does), so repeated kernel builds in one process grow by 0 regions. Files: the owners found. Verify: a probe that runs three kernel builds in one process shows no per-build region in its mappings from the first to the third. Depends: the arm64 landing. Ownership: unassigned. Claim: unassigned.
