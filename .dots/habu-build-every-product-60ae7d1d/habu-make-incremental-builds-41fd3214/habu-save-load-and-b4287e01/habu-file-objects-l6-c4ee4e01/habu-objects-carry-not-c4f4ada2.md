---
title: Objects carry not-performed requires and provided
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:20.012459+02:00"
closed-at: "2026-11-10T21:56:49.429570+02:00"
close-reason: merged into habu-find-a-compile-3bbd4bb2
---

Problem: a 'require' that performs nothing (the file is already loaded) and a 'provided' mark leave no trace in a file's object, so a warm link cannot replay what the loader's table learned; and COPY-BELOW in src/habu/linker.f costs one copy of the data below per piece. Acceptance: a warm build whose file requires an already-loaded file, or marks one provided, links with the loader table equal to the cold build's (census cold == warm); COPY-BELOW once per file. Files: src/habu/linker.f, src/habu/link-data.f, src/core/include.f. Verify: census cold/warm byte compare. Depends: step B of requests (piecewise link). Ownership: src/habu/linker.f. Claim: unassigned.
