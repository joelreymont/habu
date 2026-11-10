---
title: Process argv and env rows carry process state in objects
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.442078+02:00"
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/process-argv.f:41 (REGISTERED :26, PROC-ARGV-N :15, CONTROL :25); src/lib/process-env.f:55 (REGISTERED :37, PROC-ENV-N :23, PROC-ENV-DEF-N :26, CONTROL :33, DEF-CONTROL :36); a top level reaches both through the public declaration words SLOT, DEF-SLOT and BYTES. Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-o4.
