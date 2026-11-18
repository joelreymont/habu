---
title: "Dump an x86 image's dictionary with imgdump"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:43:43.609198+01:00"
---

Problem: hb --load tools/imgdump.f -- IMAGE on an x86-64 cross-built hb exits 65 'imgdump: no file-backed x86 dictionary' (tools/imgdump.f:294), on master as well: the x86 path finds no dictionary offset in the file. Acceptance: imgdump prints an x86-64 image's words as it does an arm64 image's. Depends: none. Claim: unassigned.
