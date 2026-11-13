---
title: Compare files in one FS word
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:26:07.127355+02:00"
---

Problem: three copies compare two files' bytes: tools/hb-build-core.f:68 SAME-FILE?, tools/install.f:178-191 READ-BOTH/SAME-FILE?/SAME-HB?, tools/hb-build-lib.f:250-266 HBB-READ/HBB-SAME-FILE?. Acceptance: one public word in src/lib/fs.f compares two files' bytes and all three call it (fs.f is engine-provided: a stage commit adds the word first); SAME-HB? keeps only its signature comparator. Depends: none. Claim: unassigned.
