---
title: Add integer nominal-role dimension
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T20:56:48.014625+02:00"
---

Full context: design section 6.3 names integer(width, signedness, role); the HIR checker-type snapshot (section 7.2) needs the role linked to the symbol table. Add a role reference (ir-symbol-id or an explicit none) to the IR-TYPE int record with cross-store validation (the symbol must belong to the same module), participating in interning identity. Depends on: IR-SYM (landed) and the schema-coordination window before the canonical encoder pins layouts.
