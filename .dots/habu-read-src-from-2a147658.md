---
title: Read source from stdin as the usage line says
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T16:49:56.747009+02:00"
---

Problem: `echo "1 . cr" | bin/hb --load` prints `hb: source prefix buffer full`, rc 74, while the usage line (src/habu/main.f:48 USAGE, habu2.f:384 USAGE2$) says `[file.f]  (source on stdin)`. src/habu/main.f:51-53 says `--load` with no file is the assembly's arena-overflow exit (SRC-SFAIL) and NO-FILES names it; the measured run reaches FULL (main.f:62-63) instead, so either stdin is not a supported route under --load and the usage line is wrong, or the stdin read overflows the arena (layout.f:292-299 SOURCE-ARENA-CAP, habu2.f:858-866 SOURCE-ARENA-LEN). Found during the ndict! raise census (carl, 2026-11-03). Acceptance: reduce which route runs for `hb --load` with no file and stdin input, and make the program and its usage line agree: either stdin source runs, or the usage line and the error name the real requirement; a row in test/main-argv.f holds it. Files: src/habu/main.f, src/habu/habu2.f, test/main-argv.f. Verify: the reproducer on a rebuilt hb; test/main-argv.f; tools/build-fixpoint-test.f SOURCE-BOUNDARY. Depends: none. Ownership: carl. Claim: unassigned.
