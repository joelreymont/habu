---
title: "Give the kernel's refusals the class shape"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T12:59:53.819090+02:00"
---

Problem: the kernel's own refusal texts do not match the interpreter's E-<class> habu: <path>:<line>: <prose> shape. src/habu/habu2.f LCOMPILEDIE (EM-COMPILE-DIE) prints E-OVERLAY-OPEN with the location after the prose (LSRCAT appends ' at <path>:<line>'), and its LDPBAD and BGROW dies still print 'hb:'. LFLOORREC's E-UNDERFLOW (MINMSG$) has no location, while the interpreter's GATE prints one. Acceptance: every kernel refusal that prints prints its class first, then the location when one exists, then the prose, as the interpreter's REFUSAL does; one shape per refusal across kernel and interpreter. The silent task-thread exits are habu-name-the-primitive-ff0806a0. Fix needs a leaf that writes '<path>:<line>: ' after the class head and an LCOMPILEDIE entry past 'LSRCAT LABEL@ BL,'. Depends: none. Claim: unassigned.
