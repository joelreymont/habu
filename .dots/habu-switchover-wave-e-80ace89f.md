---
title: "Switchover wave E: self-hosting resolvers + trust discharges"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.015932+02:00"
---

Wave E is the last bootstrap-sensitive switchover. (1) Migrate the type-family.f find/resolve family (TFAM-FIND-IN, TFAM-RESOLVE, SUMV-FIND, PF-FIND, LAY-FIND, TFAM-QUAL/SIG-RESOLVE) from '-- id true | false' to option<id>/result<id,ambig> and remove TFAM-SIG-RESOLVE's catch-on-E-TFAM-AMBIG throw-as-signal. The registry migrates onto itself, so prove each step through full fixpoint and bootstrap checks. (2) Migrate persisted checker tags (T-*/VR-*/SC-*/TK-*/TL-*) only when the AOT-image encoding change is proven fixpoint-safe; otherwise retain an explicit source-local self-hosting rationale, retirement owner, and focused production test. (3) Retire BP-NULL, TASK-NULL, c-defer-find-unset/c-defer-cell, and NULL$/ENV-FALSE source TRUST boundaries one dot at a time; each surviving boundary keeps only source-local rationale, a retirement owner, and focused production proof. DEPENDS: waves A-D landed and stable.

## AUDIT (2026-07-11) — item (1) resolver migration: BLOCKED, self-hosting boundary

### Enumeration (all in src/core/type-family.f; current lines)
- `TFAM-FIND-IN :270 ( ptr u8 n ptr u8 n -- n bool )` — callers: type-family.f
  x7, sumtype.f x3, five type suites; ZERO lib/tools callers.
- `TFAM-FIND-PUBLIC :285 ( ptr u8 n -- n bool )` — 5 hits, same population.
- `TFAM-RESOLVE :302 ( ptr u8 n ptr u8 n -- n bool )` — 14 hits incl. the
  checker.f:372 `TFAM-RESOLVE-XT` install cell (xt contract `-- id true |
  false`) and its :486 `TFAM-RESOLVE*` wrapper (CONTESTED file).
- `SUMV-FIND :459`, `PF-FIND :614`, `LAY-FIND :671` — 7 hits each, all
  src/core + suites.
- `TFAM-QUAL-RESOLVE :916` (1), `TFAM-SIG-RESOLVE :933` (4; plus the
  E-TFAM-AMBIG throw-as-signal).
- `TFAM-CONSTRUCT-FAM :1001`, `TFAM-MATCH-FAM :1029` — value+flag, zero
  external callers (construct/MATCH keyword dispatch internals).
Convention everywhere: `id true | 0/junk false` value+flag; no -1-index forms.

### Verdict: BLOCKED TWICE OVER — record as an explicit self-hosting boundary
A. BOOTSTRAP CIRCULARITY (fundamental, not a load-order accident). The
   resolvers are the substrate SUMTYPE executes on: TFAM-FIND-IN/SUMV-FIND run
   DURING every family declaration — including any declaration of `option`
   itself — so option<id> cannot exist until the resolvers have already run.
   Compile-order seconds it: the prefix (and the gforth concat, bootstrap.sh
   :168-182) loads type-family.f BEFORE sumtype.f, so an `option<id>` sig on a
   resolver is an unknown-family reject at its own compile. "The registry
   migrating onto itself" (this dot's original caveat) is the exact wall.
B. BOOTSTRAP-MIRROR TRIPWIRE (enforced red gate). tools/bootstrap-mirror-lint.f
   fails on any live SUMTYPE/ENUM/PRODUCT/TYPEFAMILY token in src/ non-test
   source, because the Gforth stage-0 emitter compiles src/core/type-family.f +
   sumtype.f DIRECTLY and has NO width-aware pass-2 mirror — a wide family
   declared in src/ would be miscompiled by the no-binary recovery. Landing any
   src/ ADT declaration first requires the mirror parity dot
   (habu-bootstrap-mirror-pass-f1714953); and even with it, blocker A remains.

### Escape hatches assessed and rejected
- Post-boot option-typed WRAPPERS over sentinel internals (the STR>NUMBER-UNWRAP
  pattern): the caller distribution kills it — resolver callers are 100%
  src/core + type suites; there is NO lib/tools consumer population to serve.
  A wrapper layer would be API surface with zero users.
- A checker-core PRIMITIVE option kind (built-in, pre-family): duplicates the
  family machinery as a special case in contested checker.f — the exact
  anti-pattern the ADT campaign exists to remove. Rejected.
- Reordering resolvers after sumtype.f: impossible; sumtype.f's declaration
  processing calls them.

### What wave E actually still contains (the plan)
- E-1 ACTIONABLE NOW (tools/lib layer, unaffected by the blockers): the
  remaining source TRUST discharges — BP-NULL, TASK-NULL, c-defer-find-unset /
  c-defer-cell, NULL$/ENV-FALSE — one dot each when reached. Until retired,
  each keeps only source-local rationale, a retirement owner, and a focused
  production-path test.
- E-2 CLOSED AS BOUNDARY (this audit): resolver option/result migration. The
  value+flag convention in type-family.f is the floor the ADT tower stands on;
  it stays sentinel WITH THIS DOCUMENTED REASON. Reopen conditions (both
  engine-lane): (i) bootstrap mirror parity lands (retires blocker B), AND
  (ii) either a pre-family option representation exists in the checker core or
  the boot order is restructured so a minimal family layer precedes the
  resolvers (retires blocker A) — and only if a real consumer population
  appears; none exists today.
- E-3 SAME BOUNDARY: TFAM-SIG-RESOLVE's E-TFAM-AMBIG throw-as-signal would
  become result<id,ambig> only under E-2's conditions; the throw is named and
  fail-closed (not a silent sentinel), an acceptable boundary meanwhile.
- Item (2) persisted checker tags (T-*/VR-*/SC-*/TK-*/TL-*): unchanged —
  migrate only with a proven fixpoint-safe encoding, else they join this same
  documented boundary.

NOTE 2026-07-17 (wave-A closure 6040c0ec): this wave also owns
ACAP-POOL-FIND (src/habu/aot-capture.f:110, -1 sentinel, one direct caller
at :119 with a mechanical MATCH shape). Blocker is bootstrap: option.f
must enter the AOT-capture metabuild/stdin closure (load-order change)
and the real source-load self-test/bootstrap recovery owning path, fixpoint x2,
run.f verdict, and touched current gates are owed. Bootstrap-sensitive items
stay in this wave.

NOTE 2026-07-18 (from wave-B closure): the bare-n raw scan kernel
SPLIT-NEXT (lib/string.f:185) is deliberately retained under the typed
STR:SPLIT-NEXT layer (wave-B batch 2 record in the closed
habu-switchover-wave-b-08482d5b). Its retirement/typed-view discharge is
TVK-RAW-class source-boundary discharge work owned by this wave when its remaining
direct readers (gate-stats.f, bootstrap-codegen-test.f, string-test
kernel test) are resolved.
