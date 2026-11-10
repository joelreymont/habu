---
title: "Name a facts row's record by its identity"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:13:32.661559+02:00"
blocks:
  - habu-name-an-entry-147c1a88
---

Problem: a facts row names a record by number (src/habu/linker.f FACT+: a retirement and an earlier piece's amended text through REF+, a text of the piece's own record by its number in the piece), so a word added mid-file changes the text bytes of every later word; each gets a new time and its users recompile. Acceptance: the three rows save the file, the record's name sum and its count (ID-REF+); FACTS-CK checks them through ID-REF-CK; a link finds the record through ID-REC and a compile through PLACE (FILE-KEY or the REFS row's key); REF+, REC-REF-CK, REC-INDEX and REFS-ROW are removed; ev.sh as before (every rc 0, warm rewritten 0, cold = warm unsigned, restored = cold, probe 3, restore 3, file-objects PASS). Files: src/habu/linker.f. Verify: ev.sh. Depends: habu-name-an-entry-147c1a88. Ownership: carl. Brief: carl-layout/.out/briefs/lane-facts-refs-by-identity.md. Claim: agent=carl; merged into the stack as okntvqyk 84d0c122 (workspace retired), lands with the feature.
