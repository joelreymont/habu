---
title: "Find what LINKER:WRITE's first call allots"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:05:04.115667+02:00"
---

Problem: in one LINKER:WITH build, the first LINKER:WRITE allots 344 bytes in the build's data space before it records DP; a second write does not, and the written image holds the bytes (found while rewriting test/compiler/arm64-link-data.f, which takes its expected values after the first link). A write that changes what it writes is a defect until explained. Acceptance: the word that allots them, as path:line with evidence, and whether the program should hold them; the fix is a second brief. Depends: none. Claim: unassigned.
