---
title: "Place a kept-cell rejection at its storage's line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:20:19.806169+01:00"
---

Problem: the linker's kept-address rejection (src/habu/linker.f REFUSE-CELL, from CELL-REF+) names the file and the storage but no source line: the linker's emission records carry none. Deleting the scratch family mark (dot habu-delete-the-scratch-7d11ca2e) moves kept-aio.f's rejection from the checker, which names kept-aio.f:8 today (test/file-objects.f KEPT-AIO-REJECTED), to this linker rejection, so the line would be lost. Acceptance: a kept cell's rejection places the storage's declaration line (file:line and the source line, as the checker's records do), cold and warm; test/file-objects.f ARENA-REFUSED asserts it. Depends: habu-give-the-kept-0ed67e30. Claim: unassigned.
