---
title: "Declare the library's error codes so logs name them"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:11:00.740900+01:00"
---

Problem: src/lib/error.f declares the library's codes with plain constant (436 'constant E-' lines, each block's FIRST and LAST among them; -2102 constant E-FS-OPEN at error.f:28), not with ERROR:CODE or ERROR:FAULT (error.f:873-883), so ERROR:NAME$ answers empty and LOG:CODE+ (src/lib/log.f:250-254) prints the number alone. Seen 2026-11-13 in Tender's zen-client log: 'could not copy the upload ... (-2102)', where a declared code prints '-110956 ZEN-CLIENT:E-INTAKE: a file could not be placed in Zen's intake folder'. Acceptance: every code src/lib/error.f defines (not a block's FIRST or LAST) is declared with a name and text, and LOG:CODE+ of E-FS-OPEN prints '-2102 E-FS-OPEN: <its text>'. Depends: none. Claim: unassigned.
