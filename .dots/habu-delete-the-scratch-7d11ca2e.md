---
title: Delete the scratch family mark
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:00:56.337856+01:00"
---

Problem: Joel 2026-11-19: "did you request the SCRATCH-TYPE that we now have as a separate type, or something? because that smells and needs to be revisited". The NEWTYPE SCRATCH-ONLY clause and E-CAST-SCRATCH make a program repeat what a reference type already tells the linker; no consumer uses them. Design: .out/designs/design-scratch-values.md items 1 and 2 (the storage clause on TYPED-VARIABLE and TYPED-BUFFER stays). Acceptance: rg -i 'scratch-only' src tools finds only storage lines and the two storage-clause parsers; rg -i 'cast-scratch|STG-SCRATCH|TF.SCRATCH' finds nothing; docs/forth-card.md and docs/forth.md say nothing of a scratch family; test/file-objects.f KEPT-AIO-REJECTED expects kept-aio.f's build to fail with the linker's LINKER:E-ADDRESS rejection naming OLD at kept-aio.f's line, cold and warm; dot habu-reject-a-scratch-35da950a is closed in the same commit. Depends: none. Claim: unassigned.
