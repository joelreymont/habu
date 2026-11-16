---
title: "Declare the library's error codes so logs name them"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:11:00.740900+01:00"
---

Problem: src/lib/error.f declares the library's codes outside the file-system block with plain constant (417 'constant E-' lines, each block's FIRST and LAST among them; -2000 constant E-A-EMPTY at error.f:20), not with ERROR:CODE or ERROR:FAULT (error.f:845-851), so ERROR:NAME$ answers empty and LOG:CODE+ (src/lib/log.f:255-261) prints the number alone. Acceptance: every code src/lib/error.f defines outside the file-system block (not a block's FIRST or LAST) is declared with a name and text, and LOG:CODE+ of E-A-EMPTY prints '-2000 E-A-EMPTY: <its text>'. Depends: none. Claim: unassigned.

Acceptance case: the codes src/core/engine-error.f mints before error.f loads (E-MEM-SIZE..E-ALLOC-STATE, DEF-OCC:E-STALE and the rest), which ERROR:CODE cannot declare because their names exist (error.f DECLARE, E-DUPLICATE-DEFINITION), also get a name and text: an uncaught throw of E-MEM-UNMAP (src/habu/address-cells.f INDEX-RELEASE) prints its text, not only "nothing caught a throw of code -3203".
