---
title: "M4a: trusted span/matrix constructors"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.457716+02:00"
---

Decomposes M4 (habu-ptx-m4-tile-6a825f56). M2 types are BUILT (all of ptr/span/gridctx/tile/uniform parse+check; mask/space/extent mismatch rejects - verified). Define the trusted boundary constructors with checked parametric signatures: MK-SPAN ( ptr<space-global,f32> u32 -- span<space-global,f32,extent-n> ), MK-SPAN= (SHARED extent for two spans), MK-MATRIX ( ptr u32 u32 -- matrix<...,extent-r,extent-c> ). These assert runtime extent (the from_raw_parts boundary). TRUSTED: bodies cannot use locals (docs/forth.md); each needs a TRUSTED.md audit row in the same change.
- Files: new lib/ptx/tile.f; TRUSTED.md rows.
- Verify: a lone MK-SPAN mints a fresh extent token unifying with nothing; MK-SPAN= shares N; T{ }T + a negative (two independent MK-SPAN spans NOT assumed equal length).
- Dep: M2 (done). Ready now (no device needed).
