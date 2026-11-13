---
title: "Grow and copy the linker's tables in bulk"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.040786+02:00"
---

Problem: a warm engine build makes 63.6M guarded stores ((PROT-SPAN), src/habu/habu1.f:5256; 9189 of them walk the bands, the rest are the call itself, about 130 ms) and 41.7M DYNAMIC-STORAGE:ADDRESS calls; src/habu/linker.f PLACES-ROOM (:863/:865) copies and zeroes its table a cell at a time when it grows (4.8M ADDRESS calls), and DYNAMIC-STORAGE:COPY copies cell by cell. A byte span copy would break src/core/dynamic-storage.f's rule that no word views a pointer cell as bytes (:3-5, COPY :49-52) and reach past the accessor's bounds check, so the bulk move needs a design. Acceptance: those copies and zeroings take no accessor call per cell, by a design that keeps that rule and the bounds check; guarded stores and ADDRESS calls per warm build measured before and after; warm equals cold. Depends: none. Claim: unassigned.
