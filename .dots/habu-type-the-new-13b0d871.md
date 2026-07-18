---
title: Type the new definer registries
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T22:41:24.682153+02:00"
---

The extent, tensor, and spec definers merged 2026-07-18 (maki/extent.f, maki/extent-tensor.f, maki/spec.f) carry fully checked stack effects and typed locals, but their internal data layer uses the old core-definer idiom the migration program is draining: parallel create arrays with hand-written cells-offset math (XR-SURF/XR-SLEN/XR-TAIL/XR-TLEN/XR-VAL, TR-NAMES/TR-NLEN/TR-RANK-A/TR-KIND-A, SP-FAC-* and SP-FI-*), lookup words returning ( n bool ) pairs (XR-FIND, TR-FIND, SP-FREE-POS, SP-CT-POS), and kind flags as bare integer constants (TR-TENSOR/TR-GATHER). None of that is checker-visible: swapping a rank for a kind, or a surface-length column for a tail-length column, type-checks. Migrate in two steps. Step 1 (unblocked today): the ( n bool ) lookups become option<n> consumed with MATCH, the pattern the migrated core finders and tools/size-report.f already use; update every caller. Step 2 (after the ENUM capability chain lands - named constructors, construct/match typing): TR-TENSOR/TR-GATHER becomes a real ENUM, and the parallel arrays become typed record rows (STRUCTURE or layout-buffer, whichever the unified type DSL blesses) so a column swap is a checker reject. Also apply the same standard to the VNOM registry-free codegen buffers only if they grow state. Review standard updated alongside: a new registry or lookup in reviewed code must use option/ENUM/typed records where those capabilities exist, or the review sends it back.
