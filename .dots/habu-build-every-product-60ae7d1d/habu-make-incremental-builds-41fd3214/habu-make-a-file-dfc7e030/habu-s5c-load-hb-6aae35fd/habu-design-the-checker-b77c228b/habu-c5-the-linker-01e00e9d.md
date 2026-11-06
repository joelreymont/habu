---
title: "C5: the linker carries facts; T0 ships"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:49.575281+02:00"
blocks:
  - habu-c4-a-build-84efe577
---

Problem: design r2 D7 (/tmp/jerry-big/design-facts-r2.md): file objects and the cross-built hb must carry each record's facts text; today the next generation gets its effects from the signature pool. Acceptance: a file object holds its records' texts by name and linking interns each into the product's DATA and writes [32]; the cross-built hb copies DATA raw, so [32] is a plain number on no address list (S6 D3, D7); the linker writes each primitive record's text from prims.f's row; the cross-built hb boots with no signature pool, no registry section and no replay and checks a new file; T0 ships here (DECISIONS 'the T0 release'). Files: src/habu/linker.f, src/habu/link-band.f, src/habu/prims.f readers, src/core/checker.f. Verify: the cross-built hb checks a new file; bin/hb --load test/run.f. Depends: C4; carl's S6 linker. Ownership: those files, coordinated with carl (S6 owner). Claim: unassigned.
