---
title: Answer a reserve that fits without the extent division
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:12:00.862190+02:00"
---

Problem: src/core/dynamic-storage.f RESERVE computes EXTENT (a division) on each of 3.16M calls in a warm engine build before seeing the record already has room. Acceptance: a reserve whose width matches the record's tag and whose count is within the cached bound returns before EXTENT; refusals and growth unchanged. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-reserve
