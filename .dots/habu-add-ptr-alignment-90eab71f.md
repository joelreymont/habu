---
title: Add pointer alignment to type table
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T20:56:47.987242+02:00"
---

Full context: design section 6.3 names pointer(address-space, pointee, alignment) but the landed IR-TYPE row (src/compiler/ir/type.f, TYR1 4-cell rows) stores space+pointee only. Add the alignment field to the row and the interning key with a schema-coordinated change BEFORE the canonical encoder (section 6.6) pins the type-table layout. Fixture: same space/pointee with different alignment must not intern to one id.
