---
title: "Checker capability: typed ADT arrays + buffer store/load"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:25:56.150571+02:00"
---

Gap found reviewing TFAM coverage for Model CAD (docs/model-cad.md typed backbone). TFAM plan item 16 ships packed-tag/niche-null/boxed as layout POLICIES with stack-level tests, and docs/type-families.md:1276-1282 specifies the packed memory ABI descriptor (tag width, payload offsets, alignment, size) noting it matters for arrays of ADTs - but no TFAM item or dot implements the CONSUMER capability: checked store/load words for layout-family values in buffers, and a typed array-of-ADT container (alloc/store/load/iterate with checked family+args, no hidden-field exposure, no trust rows). Needed by maki: cad-0a report tables, cad-4 schedule measurement history + cache rows, cad-7 artifact cache (serialization builds on this). Until it lands, tables stay parallel-column records per the cad staging rule. Depends: TFAM 15, TFAM 16 packed-tag. Related: habu-epic-adopt-adts, habu-checker-capability-derive.
