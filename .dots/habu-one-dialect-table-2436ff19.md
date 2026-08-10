---
title: One dialect table, three owners
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:21:39.106284+02:00"
---

combine.f and spill.f carry byte-identical SLOT-OF opcode tables (md5 4fd0841... both) and emit.f a near-identical third; every A64IR:opcode addition costs three synchronized edits (the exhaustive-MATCH discipline catches misses, but the duplication is real). Factor the slot table into one owner the three passes read - likely a64ir.f itself, which already owns the opcode family - without weakening the fail-to-compile-until-armed property each pass's MATCH gives. Measured basis: loops lane 2026-08-10. Acceptance: one definition of the table; the three passes read it; adding a test opcode still fails to compile in each pass until its arm exists. Files: src/compiler/native/{a64ir,combine,spill,emit}.f. Depends: none.
