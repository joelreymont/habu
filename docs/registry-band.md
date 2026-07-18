# Registry write-protection

How the type-registry control cells are protected from bare mutation. Dot
`habu-protect-type-field-04d91409` (PF registry) and the sibling rollout
`habu-protect-sibling-type-44eec932`.

**Layer 1 (internal-word marking) is shipped and closes bare naming and direct
checked references. It does NOT close every checked-source route.** A destruction
review demonstrated a laundered write from fully checked source (below), and
analysis shows a PROT-GUARD memory band would NOT close it either — the band's own
guard-bypass writer is itself launderable through the same checker gap. The single
root cause is the `RSEXEC` `T-VAR` `execute` laundering (`src/core/checker.f:1796-1804`,
documented `docs/effects.md:311-313`, dot `habu-checker-exec-of-5923c543`); closing
it closes the whole class. This document records what each layer does and does not
close.

## Problem

`src/core/type-family.f` holds the PF (shared-field registry) control cells —
`PF-CAP-V PF-A-BOOT PF-A-P PF-N PF-COMMIT-N PF-TX-CAP-V PF-TX-BOOT PF-TX-P
PF-TX-DEPTH PF-TX-SERIAL` — as `variable`/`create` data records. A din=0 data
record is EXEMPT from the seal-time internal-word marking pass
(`src/core/internal-mark.f:28-34`, exemption comment: "data records
(create/variable/constant, does>-instances) are exempt"), because its push-only
body cannot consume interpret-stack garbage and it is auto-trusted. So its bare
name stays executable at top level and a bare write mutates the registry past the
public API. Confirmed exploit on the pre-fix engine (exit 0):

```
TYPE-FIELD:COUNT . cr     \ 0
99 PF-COMMIT-N !          \ bare store, no reject
TYPE-FIELD:COUNT . cr     \ 99   — COUNT corrupted
```

Scope of the actual hole (measured, not assumed):

- **Bare top-level interpret / tick** (`--load` and stdin): OPEN. `99
  PF-COMMIT-N !` runs because `PF-COMMIT-N` is interpreted and the data-record
  exemption leaves it executable.
- **Checked user code, direct name** (`: FOO … PF-COMMIT-N … ;`): CLOSED. The PF
  cells are defined before the checker hook installs, so they are never certified;
  the checker rejects the reference with `E-UNDEFINED` / `non-certified
  definition` at check time (verified: a checked body reading or writing
  `PF-COMMIT-N` fails closed before runtime).
- **Checked user code, laundered xt** (`variable V  ' PF-COMMIT-N V !  : F V @
  execute 99 swap ! ;`): OPEN — and NOT closed by Layer 1 or by a memory band.
  A destruction review wrote a PF cell this way from fully checked source (COUNT
  0 → 99, exit 0). `[']` resolves the marked word (the mark gates interpret/tick,
  not compile-time `[']`), and the `RSEXEC` `T-VAR` branch (`checker.f:1796-1804`)
  models `V @ execute` as a pure `( -- )` while the real xt runs — dot
  `habu-checker-exec-of-5923c543`. This route is provenance-blind: it launders any
  xt, including a would-be guard-bypass writer.
- **Unchecked user code** (`0 set-check …`) and `TRUSTED:` bodies: out of the
  stated seal threat model (checked habu only).

Layer 1 closes the bare-interpret hole and the direct-name checked reference. The
laundered-xt route is the checker gap `habu-checker-exec-of-5923c543`; only fixing
that closes it (see Layer 2).

## Layer 1 — internal-word marking (shipped)

The seal-time pass already fails interpret/tick closed on any record carrying the
`DNAME-INT` flag: `EM-INTERPRET-FIND` (`src/habu/habu2.f:4604`,
`14 13 16 ANDI, 14 LINTERNAL … CBNZ`) and interpret-`'` (`habu2.f:2801`) reject
`hb: internal engine word: <token>` + rc 70 BEFORE the body runs — uniformly,
with no data-record fast path. The exemption is only that the pass does not *set*
`DNAME-INT` on data records; setting it on a specific cell makes interpret reject
that cell. That is the whole fix.

Core-vs-user split (why marking the cells does not break the engine):

- **Core compiled callers** (`type-family.f`, `sumtype.f`) reference the cells
  inside `:` bodies. Those references are resolved and baked at COMPILE time,
  before the marking pass (`internal-mark.f` is the LAST cold-prefix source), and
  the `DNAME-INT` gate lives in the INTERPRET dispatch, not the compiler. So a
  compiled `PF-N @` / `PF-COMMIT-N !` keeps working after the mark.
- **Bare interpret / tick** (user top level): rejected by the gate above.
- **Checked user compilation**: separately closed by `E-UNDEFINED` (above).

Implementation:

- `type-family.f` `REG-PROTECT ( -- )` records the just-defined record's
  dictionary index (`ndict@ 1 -`) in `REG-PROT-IDX[0, REG-PROT-N)`; each of the
  ten PF cells calls it on its definition line. Marking is DEFERRED to the pass,
  so the load-time inits (`0 PF-COMMIT-N !` etc., which interpret at cold load)
  run before the cell is marked.
- `internal-mark.f` `IMK-SEAL-REGISTRY` int-marks every recorded index; it runs
  in `IMK-PASS` before `IMK-SEAL-PRIM` (so `int-mark` is still callable).
- Uses only existing prims (`ndict@`, `int-mark`) — no new primitive, no layout
  change, so no fixpoint bootstrap concern.

Proof (temp engine + installed byte-fixpoint):

- Bare `99 PF-COMMIT-N !` → `hb: internal engine word: PF-COMMIT-N`, rc **70**,
  on both `--load` and stdin. All ten cells reject bare (`PF-N`, `PF-A-P`,
  `PF-TX-DEPTH`, …).
- A post-seal `PRODUCT p 0 FIELD x n FIELD y n ;PRODUCT` still advances
  `TYPE-FIELD:COUNT` 0 → 2; the reflection API is intact (declarations admitted).
- Checked-body reference still `E-UNDEFINED` (unchanged).
- `install --force` twice → byte-identical shasum. `test/run.f`
  `perf-verdict: performance=pass` (Layer 1 adds NO per-store cost — it only marks
  ten records at seal time and calls `REG-PROTECT` at load).
- Negatives in `test/internal-word-gate.f` (`IWG-REGISTRY-CASES`): bare cell
  names fail closed; the `99 PF-COMMIT-N !` exploit fails closed on `--load` and
  stdin; the existing colon-builder rows (`PF-BEGIN`, `PF-FIND`) stay.

Arena rows: the arena BASE names (`PF-A-P`, `PF-A-BOOT`, `PF-TX-P`, `PF-TX-BOOT`)
are among the marked cells, so a bare row-address computation
(`PF-A-P @ <off> + !`) dies at the base name at interpret — but the same laundered
`[']`→`execute` route reaches the arena from checked source.

## Layer 2 — why a memory band does NOT close the laundered route

A PROT-GUARD write-trap band was considered (relocate the ten cells into a guarded
DATA band `[REG-BAND-OFF, +REG-BAND-LEN)` added to `GUARD-SPAN`/`PROT-GUARD`,
`habu1.f:211-241`, so `!`/`c!`/`+!` trap a post-seal write with
`ENGINE-ERROR:SEAL-VIOLATION`). It does not close the checked-source threat model,
for a structural reason:

- A guarded band traps ordinary stores, so the LEGIT declaration-time writes
  (`PF-ADD` advancing `PF-N`, `PF-COMMIT` advancing `PF-COMMIT-N`, all post-seal)
  need a guard-BYPASS writer — a prim `reg-cell! ( n off -- )` doing a raw store.
  Because PF is a Forth-level registry, that bypass must be a Forth-callable word.
- A bypass word is a no-effect internal prim — exactly the shape that the `RSEXEC`
  `T-VAR` gap laundered above. Checked source runs `' reg-cell! V !  V @ execute`
  and writes any band cell through the bypass, unguarded. Proven analogue:
  `execute` of a laundered no-effect internal word (`PF-FIND`) runs from a checked
  def (exit 0). So the band only trades the demonstrated `['] PF-COMMIT-N →
  execute → !` route for an equivalent `['] reg-cell! → execute` route — no net
  closure.
- (The engine's own `TXN-STATE` band is safe only because its writers are inline
  machine code inside larger compiler prims, never a standalone Forth word. PF's
  Forth-level writers cannot be made non-launderable that way without rewriting
  `PF-ADD`/`PF-COMMIT`/… into prim bodies.)

The pointer-leak sub-audit (does a public reflection word hand back a PF pointer?)
is moot under this finding but recorded: `COUNT/NO-VARIANT/FAMILY@/…/FLAGS@` return
values; `FIND`'s `ptr` is an input; `NAME$` (`type-family.f:88-89`, `TF-OFF$` =
`TF-STR off +`) returns a STRING-POOL pointer (sibling `TF-STR`), not PF storage.
So no public word leaks a PF pointer — but that does not matter, because the
laundered-xt route reaches the cells directly regardless.

**Root cause and correct fix.** Both the demonstrated PF write and the band's
defeat are the ONE checker gap `habu-checker-exec-of-5923c543` (`RSEXEC` `T-VAR`,
`checker.f:1796-1804`; `docs/effects.md:311-313`): checked `execute` of a laundered
raw xt is modeled as pure `( -- )` while the real xt runs. Closing it — reject (or
require a statically-known effect / closed quotation for) `execute` of an
uncertified laundered xt — closes the PF write, the `reg-cell!` bypass, and the
whole laundered-mutation class at check time, provenance-blind, with no memory
band and no new primitive. Layer 1 stays as the author-time reject for bare naming
and direct checked references; the laundered route is the checker's to close.

## TDECL-MARK/RESTORE PF-snapshot redundancy (done)

`TDECL-MARK`/`TDECL-RESTORE` (`src/core/sumtype.f`) no longer snapshot
`PF-N`/`PF-COMMIT-N`. Only `PRODUCT` mutates PF, and only inside
`TDECL-PRODUCT-TX`'s own `PF-BEGIN … PF-ADD … PF-ROLLBACK/PF-COMMIT` transaction,
which restores `PF-N` on any field failure and advances `PF-COMMIT-N` only on the
outer commit; the sole later step (`TDECL-FAM-REG !`) never throws, so a rejected
`PRODUCT` leaves both marks at baseline without a second snapshot. `SUMTYPE`/
`ENUM`/`TYPEFAMILY` never touch PF. Regression: `test/type-decl-suite.f` `tdpdup`
(a two-field product whose second field duplicates the first throws E-TFAM-DUP
after one field is added; `TDT-NEG` asserts `TYPE-FIELD:COUNT` is restored). The
checker-scope frame's own PF marks (`type-family.f` `TFAM-ROLLBACK-SAVE/RESTORE`)
are load-bearing for rejected families and are unchanged.

## Per-sibling rollout recipe (`habu-protect-sibling-type-44eec932`)

The pattern is Layer 1 — marking, shared across all seven registries (`TFAM-N`,
`SUMV-N`, `TF-STR-U`, `TF-PK-N`, `LAY-N`, `SCH-N`, `SCH-ROOT-N` + their
cap/pointer/arena cells):

1. Call `REG-PROTECT` on each registry control cell's definition line (the
   infrastructure already exists in `type-family.f`; move it earlier in the file
   if a sibling is defined before it).
2. `IMK-SEAL-REGISTRY` already marks everything tagged — no change needed.
3. Add per-registry bare-name + write-exploit negatives to
   `test/internal-word-gate.f`.
4. Re-run the fixpoint (byte-identical) and the owning gates.

Layer 1 (marking) is the shared per-sibling pattern for bare naming and direct
checked references, and needs no new primitive. The laundered `[']`→`execute`
route is common to every registry and is NOT a per-sibling memory band (a band's
bypass writer is itself launderable, above); it is closed once, centrally, by the
checker fix `habu-checker-exec-of-5923c543`.
