---
title: Compare files in one FS word
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:26:07.127355+02:00"
---

Problem: two copies compare two files' bytes: tools/hb-build-core.f:68 SAME-FILE?, tools/hb-build-lib.f:252-267 HBB-READ/HBB-SAME-FILE?. Acceptance: one public word in src/lib/fs.f compares two files' bytes and both call it (fs.f is engine-provided: a stage commit adds the word first). Depends: none. Claim: unassigned.
