---
title: maker program buffer PMAX caps builds at 142 KiB
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:25:43.152182+02:00"
---

Problem: '$40000 constant PMAX' (262,144) in src/habu/aot-lib.f:30 and src/habu/build.f:21 is the maker drivers' program buffer ('here PB ! PMAX allot', a dictionary allot); after the hb-build 64 KiB cliff was removed (lane habu-hb-build-cannot-d09df17e, 2026-08-23) it is the binding limit: a 141,943-byte source (257,895 commented) builds AOT rc 0, 145,243 bytes (263,995 commented) dies 'aot: source exceeds buffer' rc 74, --repl at 170,056 dies 'hb-build: source exceeds buffer' rc 74. Named and loud, but a number on a growing input in two files. Acceptance: the program buffer sized from the commented source's length (FILE-SIZE of what the driver reads) through lib/memory.f (docs/forth.md: dictionary allot is not the place for input-sized storage), one definition shared by both drivers; a build of a 300 KiB program through tools/hb-build.f succeeds and runs; the death on a real allocation failure stays named; both drivers compiled fresh by hb-build (no engine reinstall). Files: src/habu/aot-lib.f, src/habu/build.f, tools/hb-build-test.f. Verify: the 300 KiB build; hb-build tests. Depends: habu-hb-build-cannot-d09df17e (landing). Ownership: maker drivers. Claim: unassigned.
