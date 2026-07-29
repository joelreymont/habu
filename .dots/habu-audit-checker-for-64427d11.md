---
title: Audit checker for by-value byte loops
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:32:14.795227+02:00"
---

Full context: VPKG-SAVE and VPKG-RESTORE in src/core/checker.f wrote each byte at an offset equal to its own character code — an out-of-bounds write — for as long as they existed, and only a REPEATED in-package replay exposed it. The shape was a hand-written loop keeping its index on the data stack:
"0 BEGIN dup n < WHILE  src over + c@  dst over + c!  1+ REPEAT drop".
The second "over" reaches the FETCHED BYTE rather than the loop index, because
by then the destination address sits above it on the stack. The fix was a named
VPKG-COPY over an explicit cursor variable. Sweep src/core/checker.f (especially the pre-USIGS-COPY region) for the same stack-discipline shape, and consider hoisting a byte-copy helper above line 550 so no future loop in that region has to be hand-written. Acceptance: every remaining hand-written byte loop in the file is either converted to the helper or shown by inspection to keep its index correctly; add a regression for any further defect found.
