---
title: A build refuses a program that declares a VALUE-RECORD
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:52:43.532352+02:00"
---

Problem: reproduced on master 554c7ac1 (installed hb) and on the linear-entry lane's engine. One file 'VALUE-RECORD point x n y n END-VALUE-RECORD' then ': ->POINT ( n n -- point ) ;' built with tools/hb-build.f is refused rc 74: "in ->point: unknown type 'point' in signature", "ncomp: cannot compile ->POINT at point", threw 70; hb --load of the same file with ': POINT> ( point -- n n ) ;', ': SUM ( point -- n ) POINT> + ;' and '40 2 ->POINT SUM .' prints 42. The same shape DEFLINEAR had before dot habu-make-a-linear-953febee: VALUE-RECORD is missing from DECLARATION? (src/habu/interpret.f:97-127) and a value record has no type entry, so a file that names one declared in another file's object cannot read it. No file under src/ declares a value record; tests do. Acceptance: a three-file program (owner declares the record, a second file names it in an effect, main calls both) builds cold, builds again over the same objects after main gains a word naming the record, and runs. Files: src/habu/interpret.f, src/core/checker.f, src/habu/linker.f, a test beside test/linear-suite.f's LIN-FILES. Verify: that test; lin-style engine generations equal. Depends: habu-make-a-linear-953febee landed. Ownership: those words. Claim: unassigned.
