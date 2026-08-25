# Value-nominal substrate — decision record

Decision for dot `habu-value-nominal-declaration-9947031d`. What does the
`NOMINAL:` declaration surface (`lib/type/value-nominal.f`) mint a value-nominal
integer type on — a package-scoped **arity-0 type family** (TFAM), or an extended
**CT-role** (the `DEFTYPE` table in `src/core/roles.f`, given package scoping)?

A value nominal is a distinct integer type that carries a runtime value: a camera
serial, a frame index, an exposure time. It is the flat-value counterpart of an
extent (`docs/extent-substrate.md`), which is a *phantom* type argument with no
value. That record chose TFAM for extents because of the product/factorization
axis (BTC-7) and left value nominals open: "CT roles remain correct for flat
value nominals... where A1's contract stands", and it deferred the CT-role
package-scoping restructure (`habu-foundation-a1b-pkg-6692f4e3`) unless "a
concrete consumer needs a package-private value nominal that a TFAM arity-0
nominal scalar cannot serve". This dot is that consumer, and the probes below
show a TFAM arity-0 nominal *does* serve it — so A1b is obviated for value
nominals.

## Decision

**Value nominals are package-scoped arity-0 TFAM cell families with generated
converter words**, the same substrate `maki/extent.f` uses for a flat extent:

- `NOMINAL: NAME` folds the UPPER-CASE surface name to a lowercase family tail
  (`SERIAL` → `serial`, `FRAME-INDEX` → `frame-index`; internal hyphens survive,
  `TF-CANON?` allows them) and mints an arity-0 cell family with
  `CHECKER-DEFFAMILY` in the caller's active package.
- it derives the explicit converter pair `>NAME ( n -- tail )` and
  `NAME>N ( tail -- n )` as no-op identity casts through one audited `evaluate`
  boundary (`NG-EVAL`, the `roles.f` `DTC-EVAL` / `extent.f` `XG-EVAL` pattern).

## Why the loser lost — extend the CT-role table (DEFTYPE / A1b)

The rejected alternative is to keep value nominals on CT-roles (`DEFTYPE`) and
build the package-scoped resolution the surface needs (the `CON-OF` / `CT-FIND`
restructure tracked by `habu-foundation-a1b-pkg-6692f4e3`).

1. **Package scoping — the dot's hard requirement — is free on TFAM and a large
   engine change on CT-roles.** TFAM records key on (package, tail), so two
   packages may declare the same tail without aliasing, with no engine edit. The
   CT-role table is global and flat; a second same-named declaration dies. To
   give CT-roles package scoping means rebuilding `CT-FIND`/`CON-OF` around a
   package-keyed registry with package-aware resolution, snapshot, and rollback —
   the highest-cost path, in the shared checker resolution region, while another
   lane is already editing `checker.f`. The TFAM path needs **zero** `checker.f`
   edits.

2. **Everything else the value-nominal contract needs, TFAM already has.** The
   converters CT-roles auto-derive are reproduced by the codegen boundary the
   extent surface already proves (`XG-EVAL`); an arity-0 family resolves directly
   as a scalar signature type; and the strictness is identical.

3. **The extent decision already anticipated this.** It kept A1b only as a
   contingency for a value-nominal consumer TFAM could not serve. TFAM serves it,
   so A1b is obviated for value nominals (CT-roles remain the substrate for the
   built-in global roles `idx`/`len`/`fd`/... and their locked
   `test/type-nominal-suite.f`).

Both substrates give the **same strictness** (probed below), so nothing is lost
by choosing TFAM; TFAM adds package scoping and rides the proven extent codegen.

## Evidence (probed on `bin/hb`, fcc90057)

An arity-0 TFAM family used as a standalone value-nominal scalar type:

| probe | verdict |
|---|---|
| `serial -- serial` (same nominal) | ACCEPT |
| `serial -- n` / `n -- serial` (vs generic int, both directions) | REJECT |
| `serial -- frame` / `frame -- serial` (distinct nominals) | REJECT |
| `n -- serial` with `>SERIAL` (explicit inject) | ACCEPT |
| `serial -- n` with `SERIAL>N` (explicit project) | ACCEPT |
| `n -- n` with `>SERIAL SERIAL>N` (round-trip identity) | ACCEPT |
| `n -- frame` with `>SERIAL` (converter no-launder) | REJECT |
| demanded `serial` input fed a plain `n` | REJECT |

Package scoping (the deciding axis), `NOMINAL: SERIAL` in two packages:

| probe | verdict |
|---|---|
| package CAMERA: `serial -- serial` (own) | ACCEPT |
| package CAMERA: `serial -- FRAME:serial` (cross-package, both directions) | REJECT |
| two packages both declare `NOMINAL: SERIAL` | no collision (each distinct) |
| CT-role comparison: two packages both `DEFTYPE SERIAL` | dies exit 70 |

Fail-closed on hazards (`CHECKER-DEFFAMILY`, named throws, exit 67):

| probe | verdict |
|---|---|
| same-package `NOMINAL: SERIAL` twice | E-TFAM-DUP "duplicate family" |
| `NOMINAL: N` (tail collides with builtin `n`) | reserved-name reject |
| `NOMINAL: IDX` (tail collides with CT-role `idx`) | reserved-name reject |

The strictness matches `test/type-nominal-suite.f` (the DEFTYPE contract) exactly;
`test/value-nominal-suite.f` locks it for the `NOMINAL:` surface, and adds the
two-package distinctness the CT-role table cannot express.

## Consequence for `habu-foundation-a1b-pkg-6692f4e3`

A1b (package-scoped CT-role resolution) is **obviated for value nominals**: the
`NOMINAL:` surface delivers package-scoped value nominals on TFAM without it.
A1b stays relevant only if a future consumer needs a package-private value
nominal that must live in the CT-role table specifically (none exists today).
Recommend the orchestrator close or de-prioritize A1b accordingly.
