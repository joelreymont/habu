---
title: Model layout buffers in the check.f preverify of lib/aio.f
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T15:20:00.392155+03:00"
---

tools/check.f lib/aio.f (script argument, engine fa2fe032 on 548facd2) fails the source preverify with throw 7121 E-CHECKER-LAYOUT-BUFFER (check.f: source preverify failed before run; label lib/aio.f), rc 67. The engine load path accepts lib/aio.f (the suite is green), so the pre-pass (src/habu/verify-source.f) diverges from the checker on a layout-buffer or type-family shape in that file. Reduce to the definition that throws, name whether the pre-pass or the declaration is wrong, fix that layer and pin the reduced shape in a certify row. Depends: none. Ownership: hazel. Claim: unassigned.
