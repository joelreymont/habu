---
title: "Switchover wave E: self-hosting resolvers + trust discharges"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.015932+02:00"
---

docs/census-switchover.md sections 1a/1c/4+5 wave E, LAST and bootstrap-sensitive. (1) type-family.f find/resolve family (TFAM-FIND-IN :248, TFAM-RESOLVE :280, SUMV-FIND :370, PF-FIND :432, LAY-FIND :489, TFAM-QUAL/SIG-RESOLVE :696/:704) from '-- id true | false' to option<id>/result<id,ambig>; TFAM-SIG-RESOLVE catch-on-E-TFAM-AMBIG throw-as-signal removed. Fixpoint-sensitive: the registry migrating onto itself — prove each step through the full fixpoint + bootstrap check. (2) Persisted checker tags (T-*/VR-*/SC-*/TK-*/TL-*) migrate ONLY if AOT-image encoding change proven fixpoint-safe; else record as explicit self-hosting boundary with rationale. (3) Trust-row discharges, one dot each when reached: BP-NULL, TASK-NULL, c-defer-find-unset/c-defer-cell, NULL$/ENV-FALSE (census section 4 list) — the campaign itself adds zero trust rows. DEPENDS: waves A-D landed and stable.

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
- E-1 ACTIONABLE NOW (tools/lib layer, unaffected by the blockers): the census
  section-4 trust-row discharges — BP-NULL, TASK-NULL, c-defer-find-unset /
  c-defer-cell, NULL$/ENV-FALSE — one dot each when reached.
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
