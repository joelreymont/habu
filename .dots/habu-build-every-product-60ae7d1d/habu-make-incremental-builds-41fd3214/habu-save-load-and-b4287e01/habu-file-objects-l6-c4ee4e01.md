---
title: "File objects L6: compile after linked files"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.570993+02:00"
blocks:
  - habu-file-objects-l5-c2dba0e3
---

Problem: until the checker's facts are stored on the dictionary entries (DECISIONS 2026-11-05 'where the checker's facts live'; jerry's habu-design-the-checker-b77c228b and its lanes), a file cannot compile after a linked one and the build restarts. Acceptance: with the facts on the records, a stale file compiles after linked ones and the restart transition goes. Design: .out/carl-file/design-fobj.md section 2 item 9, section 5 L6. Files: src/habu/linker.f (or its object file), test/file-objects.f. Verify: edit b.f, only b.f compiles (LINKER:COMPILED 1), program cmp-identical to a cold build. Depends: L5 and jerry's facts lanes. Ownership: carl (lead), worker lane. Claim: unassigned.
