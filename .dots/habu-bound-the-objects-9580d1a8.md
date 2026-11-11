---
title: Bound the objects an out keeps for old engines
status: open
priority: 3
issue-type: task
created-at: "2026-11-11T15:03:31.549160+02:00"
---

Problem: src/lib/build-out.f OBJ$ keys each out's objects by engine identity (<out>/obj/<ENGINE-ID:KEY$>/), and nothing ever removes another engine's directory. The installed tree's .out holds 4.7 GB (obj 3.6 GB, 940 engine directories, 20064 files, 2026-11-11), filled by engine and --repl builds run with the installed tree as the working directory and no HABU_OUT; every workspace's and consumer's .out grows the same way with each engine built or installed. An old engine's objects are never read by a new one. Acceptance: an out keeps a bounded set of engine directories, and a build never removes a directory another running build is using. Files: src/lib/build-out.f, src/habu/linker.f (OBJECT$, OBJECT-DIR). Verify: build twice with two engines into one out; the first engine's directory goes once its builds are done, and a concurrent build of either engine never fails. Depends: none. Ownership: none. Claim: unassigned. Needs a design (concurrency: two engines building into one out at once).
