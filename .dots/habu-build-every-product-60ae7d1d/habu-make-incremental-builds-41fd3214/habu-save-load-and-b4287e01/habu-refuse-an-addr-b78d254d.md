---
title: "Refuse an address cell marked past its file's end"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T03:31:15.663817+02:00"
closed-at: "2026-11-07T04:58:35.841264+02:00"
close-reason: "Landed: DATA+ refuses an address-cell row at or above FILE-DP that ends past the file's D bytes (E-COMP-STATE) before the object is written; fixture past.f and STOP-PAST in test/fixtures/file-objects/build.f prove it (before: the row dropped from every object; after: refused). Review ACCEPT on all four claims."
---

Problem (L3 re-review, carried in the L5 dot until 2026-11-07): an address-cell row a file's compile marks at or past its own FILE-END (src/habu/linker.f FILE-END :172, IN-FILE? :654) is seen by neither its DATA+ nor FIND-STORES, and the next file's DATA+ takes every row in its own range, so the row lands in another file's object. Acceptance: the build refuses such a row E-COMP-STATE, as it refuses an address cell off a cell boundary; a rejected-program case in test/file-objects.f or test/linker.f shows the refusal. Files: src/habu/linker.f, one test file. Verify: linker and file-objects suites rc 0. Depends: none. Ownership: carl (lead), worker lane. Claim: agent=worker workspace=.jj-ws.noindex/carl-addr.
