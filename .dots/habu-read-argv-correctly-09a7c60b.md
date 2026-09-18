---
title: Read argv correctly in a stripped image
status: open
priority: 1
issue-type: task
created-at: "2026-09-18T13:13:59.171952+03:00"
---

Problem (stripped-image cells lane, 2026-09-18): src/os/script-argv.f SCRIPT-ARG-START reads data-base APP-ENTRY:XT-CELL + @, which is zero in a stripped image, so it takes the source-list branch and starts at argv[2]: 'argvprog one two' gives SCRIPT-ARGC 1 and arg0 'two'. Pre-existing and reachable now that a stripped image can read argv at all (habu-give-a-stripped-c1a6664a). Every Tender entry reads its arguments. Acceptance: SCRIPT-ARG-START decides by the image kind it is in, not by a cell that is zero in a stripped image (the stripped entry sets a declared image-kind or entry cell the way it sets S0-CELL/DP-CELL, or the branch keys on APP-ENTRY's own claim), so a stripped image's argv starts at argv[1] and SCRIPT-ARGC counts them all; the --repl and engine paths unchanged; a fixture in tools/hb-build-test.f runs a stripped image with two arguments and asserts both; docs/native-applications.md's SCRIPT-ARGV$ paragraph states it. Files: src/os/script-argv.f (baked: three generations), src/habu/aot-lib.f, tools/hb-build-test.f, docs/native-applications.md. Verify: the fixture; three generations with cmp; bootstrap check; test/run.f. Depends: habu-give-a-stripped-c1a6664a. Ownership: AOT entry. Claim: unassigned.
