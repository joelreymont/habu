---
title: Remove stale test helper metadata
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.982411+03:00"
---

Delete unused FORM-ARITY/classification helpers in compiler/insn-schema, unread roles in reloc-schema, GE-FILES DSL and obsolete gate-common execution helpers plus lint entry. Preserve live vectors/readers and behavior tests. Reconfirm all call sites; focused affected instruction/relocation/gate tests.
