---
title: "Re-land zen's typed VEC CFB reader after Habu master installs"
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T21:47:05.424507+02:00"
---

Re-apply zen bca6a811 + 151f4147 (reverted in db4b3d12) once the installed Habu includes master c4657b44 (VEC:N>ITEM/N>INDEX public). Blocked until after the demo. Prove with the installed hb: test/document-prepare.f, test/reader.f, tender test/c2-office-scale.f. Claim: jerry.
Land on zen master (zen's default branch since 2026-10-26; main is frozen at 3a0e2a6e).

Also tender (found 2026-10-27, jerry-jmeasure): src/fields.f does not load on Habu master: E-UNDEFINED VEC-IDX at tender src/table.f:5313, the same typed VEC break as zen cfb.f. tender test/c2-office-scale.f still loads. Fix it with the zen re-land, after the Habu install (tender runs on the installed hb).
