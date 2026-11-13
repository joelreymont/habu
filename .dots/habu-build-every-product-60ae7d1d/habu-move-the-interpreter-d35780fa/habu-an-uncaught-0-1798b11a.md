---
title: An uncaught 0 throw exits 109 with no message
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:41.739282+02:00"
closed-at: "2026-11-13T20:59:27.211407+01:00"
close-reason: A literal 0 throw is refused at compile time (E-THROW-ZERO); the 109 conversion is gone, and a computed 0 unwinds unchecked as the card states.
---

Problem: an uncaught `0 throw` under --load exits 109 with no output at all: `: ZT ( -- ) 0 throw ; ZT` prints nothing, rc 109. The throw primitive turns a zero code into ENGINE-ERROR:THROW-ZERO, 109 (src/habu/habu1.f:3497-3507 BTHROWPRIM; x86-64 src/habu/kernel-x64.f:1713-1720), and the uncaught reporter passes a code in 1..255 through as the exit status without a line (src/habu/habu2.f:2994-2996). habu-uncaught-throws-name-1a43b055 keeps that silent 1..255 exit unchanged, so its work does not cover this. Whether an uncaught engine-error code in 1..255 prints a line naming it is a design choice not made. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the choice is made and written beside the reporter: either an uncaught 1..255 code prints a line naming the engine error with its exit status unchanged, or the silent exit is kept and documented as intended for THROW-ZERO. The reproducer under bin/hb --load shows the chosen output and exits 109.
Depends: none.
Claim: unassigned.
