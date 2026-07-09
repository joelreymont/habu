---
title: "docs/forth.md: 0<> guidance is stale"
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:49:52.627206+02:00"
---

docs/forth.md 'Habu Native Tooling Gotchas' says Habu lacks pick, within, and 0<> and suggests 0 = 0= workarounds. lib/prelude.f has provided 0<> (plus true/false/fdup/f<=/f>=) for a while and maki/cad.f + maki/fusion-plan.f already use it. Fix the sentence: core lacks pick/within; 0<> and the bool/float conveniences come from require lib/prelude.f. One-line doc edit; found while building maki/onnx/proto.f (first load failed on 0<> until the prelude require was added).

## CLOSE-READY

Verified against native bin/hb and lib/prelude.f:

- `pick` and `within` are genuinely absent from native core (both `E-UNDEFINED`
  on `bin/hb`). The bootstrap Gforth recovery seed has PICK/ROLL check-folds and
  a WITHIN prim, but that is the frozen `bootstrap/` corpus, not the native
  engine this gotcha describes.
- `0<>` is `E-UNDEFINED` in core and resolves after `require lib/prelude.f`.
  lib/prelude.f provides exactly: `true`, `false`, `0<>`, `fdrop`, `fdup`,
  `fover`, `f<=`, `f>=` (all `export`ed).

Fixed the docs/forth.md sentence: core lacks `pick`/`within`; `0<>` and the
boolean/float conveniences come from `require lib/prelude.f` rather than
re-deriving `0 0=` / `0 0= 0=` by hand. One focused doc edit.
