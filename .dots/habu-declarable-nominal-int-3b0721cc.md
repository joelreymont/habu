---
title: Declarable nominal integer types
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:32:21.889800+02:00"
---

Roles (idx/len/fd/...) are baked into the engine: src/core/roles.f conversion words are TRUSTED empty bodies pinned to checker-known role names; consumers cannot declare their own nominal ints (camera serial, frame index, exposure-us, GMSL channel for the odin-habu port - MISSING.md:57-89) without editing engine+checker and a fixpoint rebuild. This is the #1 ergonomics want from the in-progress odin-habu port. Fix: DEFTYPE-style user-declarable nominal scalar types registered at load time in the checker (CHECKER-DEFTYPE exists, roles.f:80-82 - extend to full role semantics: no widening, cast words auto-derived with TRUST rows), plus tests + docs/effects.md update.

## Progress (DONE)

`deftype NAME` now delivers full role semantics for user-declared nominal
integers. Finding: no-widening was ALREADY correct (CHECKER-DEFTYPE registers a
distinct CT-ROLE entry via CT-ADD-NOMINAL; `foo -- n`, `n -- foo`, `foo -- bar`
already rejected). Two real gaps remained: (1) no converter words, so a user
could never construct/erase a value; (2) user nominals rendered as `?` in
diagnostics.

src/core/roles.f — DEFTYPE auto-derives the explicit converter pair:
- `DEFTYPE` now, after CHECKER-DEFTYPE, builds and evaluates
  `TRUSTED: >NAME ( n -- NAME ) ;` and `TRUSTED: NAME>N ( NAME -- n ) ;`
  (constructed strings in DTC-BUF, run via one audited `evaluate` wrapper
  DTC-EVAL). Converters are no-op identity casts, matching >IDX/IDX>N. Because
  the TRUSTED: text lives in string literals, no per-declaration static trust
  site is added; the single DTC-EVAL boundary is manifested + classified.

src/core/render.f — CON-OUT rendered names only for codes < CC-MAX (built-ins);
widened to `< CTN @` so any registered type (built-in OR user deftype) renders by
name. `frame-idx -- n` now reports `expected: n actual: frame-idx`, not `?`.

TRUSTED base delta: +1 TRUSTED (roles.f:DTC-EVAL, the evaluate wrapper) — the one
irreducible metaprogramming boundary for dynamic word creation. Manifested in
TRUSTED.md (name-keyed row), classified `prim-axiom` under this dot, baseline
TRUSTED 218 -> 219. All other kinds unchanged (TRUST 348 / SETCHECK 10 /
TRUST-BARE 1 / HOOK-INSTALL 12). Runtime-generated per-type converters are not
source sites, so they add no static count; they are correct-by-construction
identity casts covered by the DTC-EVAL boundary. No existing boundary was
dischargeable by this capability (the built-in role casts stay baked; they are
used pervasively and are not user types).

REGRESSION (test/engine-suite.f): deftype frame-idx / exposure-us then
T-CHECK-PASSES on >frame-idx / frame-idx>N / round-trip / identity;
T-CHECK-REJECTS on no-widen (`frame-idx -- n`), no-cast (`n -- frame-idx`),
distinctness (`frame-idx -- exposure-us`), and cross-cast
(`n -- exposure-us` via >frame-idx); plus a captured-diagnostic assertion that the
JSON names `frame-idx` (render-by-name).

VALIDATION: install --force byte-for-byte compiler fixpoint; full gate
(`bin/hb --load test/run.f`) PASS; typed-local-diff-lint 0; trusted-inventory
ratchet ok (219); trust-lint 0; host-lint 0; filemap-lint 0. docs/effects.md
updated (DEFTYPE converter pair + escape-hatch note).
