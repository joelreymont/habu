---
title: Checked source reads and writes any address through a base plus an offset
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T18:43:12.993249+02:00"
---

Problem: ': T ( -- n ) data-base $100000000000 + @ ;' passes the checker: T reads an unmapped address and stops with E-MEMORY-FAULT (110), and the same offset to a mapped address reads it silently. The null-read hunt also reports, and the installed engine shows: with 'variable V 7 V !', ': R ( -- n ) V BYTE-VIEW NULL-PTR BYTE-VIEW - NULL-PTR swap + @ ;' reads V's cell (R prints 7) and the same route with '!' writes it ('9 W' leaves V at 9), rc 0; T stops 'E-MEMORY-FAULT habu: memory fault at 0000100340000000', rc 110. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which bases admit an offset with no bound, what the checker knows of the offset at the read or write (the kinds, the offset mark), and which source in the tree uses each form. Acceptance: no checked program reads or writes outside the object its base names: it is refused with a reason or stops with a catchable code. Files: named by the hunt. Verify: the reproducers, refused or caught. Cases: /tmp/carl-spark/rf/pc-plan/keep/raw-cur-foreign.f (--load rc 70, --verify-only rc 0), raw-cur-own.f (0 against 70), raw-pkg-pub.f (70 against 0) agree once fixed. Depends: none. Ownership: bounds of a base plus an offset. Settled 2026-11-12 by the long-term rule: unbounded pointer arithmetic leaves program source, restricted by privacy (private sections and the build strip), not owner gates; this is the null-reach design's rule 2 (rule 1 is habu-take-the-null-3e98f000's): the pointer step rows leave ordinary words and a package steps only through its own private CAST:; its lanes 4-6 and the strings design are this dot's children. Claim: agent=jerry (lanes as children).

Acceptance case (2026-11-12): a private `CAST: ( n -- ptr [ -- n ] )` over a neighbour cell's address minus 5 cells writes the cell of `defer D ( -- Q:tok )`, and D then yields a token the package never minted; refused once no checked program writes outside the object its base names.

Acceptance case: `TASK:MAIN-BASE` (src/lib/task.f:1800) answers the main DATA region's base as a raw `^n`, so `TASK:MAIN-BASE $2000 + @` reads an engine cell once programs lose `data-base` (dot habu-reject-a-cell-3499c957); refused once no checked program reads outside the object its base names.
