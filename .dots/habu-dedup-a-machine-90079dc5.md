---
title: Dedup a machine description before the table-full check
status: open
priority: 3
issue-type: task
created-at: "2026-09-19T18:27:54.094905+03:00"
---

Problem (adversarial review, minor): src/compiler/native/machine.f MACHINE checks 'row MACH-MAX >= if E-NMACH throw' before the DUP-ROW scan, which needs the scratch row, so once MACH-MAX (16) distinct machines exist a RE-description of an existing machine throws although the header promises two descriptions of one machine are the same value. Not reachable with the two rows the chain declares. Acceptance: compare against the existing rows before claiming a scratch row (or claim the scratch row only when there is room and otherwise compare without writing), keeping 'a duplicate leaves MACH-N unchanged'; a case in test/compiler/native-effect.f or x64ir.f that fills the table with 16 variants then re-describes the first and gets the same value. Files: src/compiler/native/machine.f, its suite. Verify: the suite; three generations; test/run.f. Depends: none. Ownership: native chain. Claim: unassigned.
