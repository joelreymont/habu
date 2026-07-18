# Registry write-protection

How the type-registry control cells are protected from bare mutation. Dot
`habu-protect-type-field-04d91409` (PF registry) and the sibling rollout
`habu-protect-sibling-type-44eec932`.

Two layers were considered. **Layer 1 (internal-word marking) is the shipped
enforcement and closes the stated threat model.** Layer 2 (a PROT-GUARD memory
band) is an audited, *conditional* defense-in-depth option that is **not built** —
the Layer-2 audit found nothing that requires it. Both are recorded here.

## Problem

`src/core/type-family.f` holds the PF (shared-field registry) control cells —
`PF-CAP-V PF-A-BOOT PF-A-P PF-N PF-COMMIT-N PF-TX-CAP-V PF-TX-BOOT PF-TX-P
PF-TX-DEPTH PF-TX-SERIAL` — as `variable`/`create` data records. A din=0 data
record is EXEMPT from the seal-time internal-word marking pass
(`src/core/internal-mark.f:54-65`, `DNAME-INT` doc: "data records
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
- **Checked user code** (`: FOO … PF-COMMIT-N … ;`): already CLOSED. The PF cells
  are defined before the checker hook installs, so they are never certified;
  the checker rejects the reference with `E-UNDEFINED` / `non-certified
  definition` at check time (verified: a checked body reading or writing
  `PF-COMMIT-N` fails closed before runtime).
- **Unchecked user code** (`0 set-check …`) and `TRUSTED:` bodies: out of the
  stated seal threat model (checked habu only).

So only the bare-interpret hole needed closing.

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
(`PF-A-P @ <off> + !`) dies at the base name. The remaining row-mutation question
is the same pointer-leak audit as Layer 2.

## Layer 2 — pointer-leak audit + verdict

Layer 1 closes bare interpret and (via `E-UNDEFINED`) checked references BY NAME.
The only way checked code could still reach PF storage is a public word that
RETURNS a pointer into it — then `<ptr> !` is type-legal and Layer 1's name gate
is bypassed. Audit of the retained public surface
(`type-family.f` `package TYPE-FIELD public`):

| public word | output | pointer into PF storage? |
|-------------|--------|--------------------------|
| `COUNT NO-VARIANT FAMILY@ VARIANT@ SCHEMA@ SLOT@ CELLS@ BYTE-OFF@ BYTES@ ALIGN@ FLAGS@` | value `n` | no |
| `FIND ( n n ptr u8 n -- n bool )` | `n bool` (id + found) | no — the `ptr` is an INPUT |
| `EACH ( n n n -- n bool )` | `n bool` | no |
| `NAME$ ( n -- ptr u8 n )` | ptr | into the STRING POOL, not PF storage |

`NAME$` → `PF-NAME$` → `TF-OFF$` (`type-family.f:88-89`, `TF-STR off + u`): the
pointer is `TF-STR + offset`, into the interned-name arena (the `TF-STR-U`
registry), NOT the PF record array or the PF counters. Writing through it would
corrupt a field-name byte string, a separate sibling registry, never the PF
count/rows.

**Verdict: no public word leaks a pointer into PF storage.** Layer 1 alone closes
the checked-source threat model for the PF registry. The `NAME$` string-pool
pointer is a sibling (`TF-STR`) concern, tracked with the sibling rollout, not a
PF-storage leak. **The band is therefore NOT built.**

## Layer 2 (optional) — the PROT-GUARD band, if depth is ever wanted

Recorded for completeness. It would add coverage ONLY for out-of-model paths
(a laundered address obtained via unchecked `0 set-check` code, or a `TRUSTED:`
body computing `data-base + REG-BAND-OFF + off`). Cost: one interval compare on
every guarded store's check chain. Benefit: the write traps regardless of how the
address was obtained.

Mechanism (matching the `TXN-STATE`/`PROT-REG` precedent): relocate the PF scalar
cells into a reserved DATA band `[REG-BAND-OFF, +REG-BAND-LEN)` appended below
`DATA-START` (`src/habu/layout.f`), add it to `GUARD-SPAN`/`PROT-GUARD`
(`habu1.f:211-241`) so `!`/`c!`/`+!`/`atomic*` trap a post-seal write there with
`ENGINE-ERROR:SEAL-VIOLATION` (= 83, uncatchable exit), and route the legit
declaration-time writes through ONE new guard-bypassing primitive:

```
reg-cell! ( n off -- )   \ raw store into REG-BAND; off band-relative, hard-bounded
                         \ to [0, REG-BAND-LEN) (out-of-range = SEAL-VIOLATION);
                         \ internal-marked (bare rejects); no checker effect.
```

plus `reg-cell-addr ( off -- a )` for the address-returning reader so the exploit
`99 PF-COMMIT-N !` stays type-legal and is enforced by the runtime trap. A new
primitive is REQUIRED (the friend-latch seal is one-way, so a guarded band is
writable post-seal only by a raw prim-body store); this is why the band is a
larger, separately-reviewed change. One generic bounded `reg-cell!` serves all
seven sibling registries.

Boot cost note: the band is a software compare (no mprotect), so it is NOT in the
LPROT `+41ms` class (`LESSONS.md:776`); measure the store-guard delta against the
fixpoint baseline before adopting.

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
4. Run the pointer-leak audit for that registry's public surface (e.g. `TF-STR`:
   `NAME$`'s string-pool pointer becomes in-scope there — decide return-a-copy vs
   accept, or adopt the optional band).
5. Re-run the fixpoint (byte-identical) and the owning gates.

No new primitive is needed for Layer 1. The optional band above is the only piece
that would introduce one, and only if defense-in-depth against out-of-model
laundering is ever wanted.
