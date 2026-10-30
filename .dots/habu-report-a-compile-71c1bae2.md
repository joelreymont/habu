---
title: "Report a compile failure in the engine's checked prefix"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.685575+02:00"
---

Problem: in an engine build any compile failure inside the checked prefix (e.g. ': ZZ-BAD ( -- n ) ;' inserted before src/core/checker.f's first TRUSTED: ~2857) prints the checker refusal ('ncomp: cannot compile ZZ-BAD') and then dies 'hb: stack bounds exceeded (machine)', rc 102, instead of reporting the error. Reproduced on the carried-rows host with lane 11 tools, with the tip's dc/dep, and on the tip host (EXT reviewer, /tmp/carl-fext-rev o1-o4.log; scratch trees there). Not tried on master. Cause untraced: an unbounded recursion on the engine build's failure path (evaluate-throw recovery with a half-loaded checker, or the prefix finally handlers). Acceptance: reproduce on master first; the build reports the compile error and exits with the build's error rc; a regression row. Verify: the reproducer on master red/green. Depends: none. Ownership: unassigned. Claim: unassigned.
