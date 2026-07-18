# Registry Band — write-protecting type-registry control state

Design record for dot `habu-protect-type-field-04d91409` (Design C, friend-band
memory protection) and the sibling rollout `habu-protect-sibling-type-44eec932`.

## Problem

`src/core/type-family.f:781-919` holds the PF (shared-field registry) control
cells as ordinary `variable`/`create` data records. `src/core/internal-mark.f:28-34`
exempts data records from internal-word marking by design, so their bare names
stay resolvable at top level and a bare write corrupts registry state through the
public API. Confirmed live exploit (current engine, exit 0):

```
TYPE-FIELD:COUNT . cr     \ 0
99 PF-COMMIT-N !          \ bare store, no reject
TYPE-FIELD:COUNT . cr     \ 99   — COUNT corrupted
```

The name-visibility seam (move PF into a package, reopen for callers) is proven
UNIMPLEMENTABLE (dot RE-BLESS paragraph, rule M1): a word may name a private word
only inside an open package block, and the PF writers are reached from globally
pinned cross-file API (`CHECKER-DEF*` via `src/habu/verify-source.f:448-505`,
`tools/check-core.f:718-798`, PRIM rows `src/core/checker.f:4772-4776`), so no
reopen boundary terminates.

Design C instead follows the engine's own stated architecture
(`internal-mark.f:34`: "the truly dangerous engine cells are owned by the
PROT-GUARD friend bands, not by name visibility"): **names may stay global; the
WRITE is what is guarded.**

## Machinery the band rides

The engine already runs a software write-guard, distinct from the LPROT mprotect
toggle:

- **Store guards** `GUARD-SPAN ( addr len -- )` (`src/habu/habu1.f:211-226`) and
  `PROT-GUARD ( addr -- )` (`habu1.f:228-241`) are emitted into every guarded
  store primitive body. Each reads `FRIEND-LATCH-CELL`; if 0 (cold prefix owns
  the open latch) the guard is inert, otherwise it interval-tests the target
  against each protected band and, on intersection, writes
  `ENGINE-ERROR:SEAL-VIOLATION` (= 83, `src/core/engine-error.f:5`) and calls
  `NR-EXIT-GROUP` — a hard, uncatchable process exit.
- **Interval emitters** `GUARD-BAND ( addr off len trap -- )` (`habu1.f:164-170`)
  and `GUARD-ADDR-BAND` (`habu1.f:172-176`) are the half-open overlap tests a
  band adds itself to.
- **Guarded sinks** are exactly `! c! +! atomic*`, `patch32`, snapshot rebase,
  and syscall write buffers (`src/habu/layout.f:108-110`). There is **no**
  guard-bypassing Forth store: every Forth-level write to a computed address
  carries the range check.
- **Existing bands** guarded by the same path: `FRIEND-ARENA`/`-LEN`
  (`layout.f:125-126`), `PROT-REG-OFF`/`-LEN` (`layout.f:281-282`),
  `OWNER-REG-OFF`/`-LEN` (`layout.f:310-311`), `TXN-STATE-OFF`/`-LEN`
  (`layout.f:432-433`), the `GUARD:SPAN`/`GUARD:ADDR` TXN-BLOB band
  (`habu1.f:178-204`), `ENGINE-HOOK`, `BODYBUF`.
- **The one-way seal** `EMIT-SEAL-FRIEND` sets `FRIEND-LATCH-CELL :=
  FRIEND-ARENA-LEN` at the end of the cold prefix (`src/habu/habu2.f:713-728`).
  The latch cell sits inside the guarded `FRIEND-ARENA` band, so post-seal it
  cannot be cleared — the seal is monotonic. **There is no post-seal RW window
  via the latch.**
- **Guard-bypassing writes** are raw prim-body stores `<reg> DATA <off> STR`
  (AArch64), never the `!` prim. `TXN-STATE` — a *mutable* band written by the
  checker after seal — is written exclusively this way (e.g.
  `habu2.f:4156-4161, 5169, 5397`). This is the model the PF band copies.

`LPROT` (`habu1.f:2460-2461`, `NR-MPROTECT` over the DICT/CODE region) is a
SEPARATE mechanism for the RX code region and is **not** used here; the band is a
software compare with no mprotect (see Boot cost).

## Why a new primitive is required (STOP gate)

The PF control cells must be written by legit code **after** the seal:

- The friend latch seals BEFORE the engine's own checker/type-family/sumtype/
  stdlib source runs; that source writes DATA "post-latch via guard-bypassing
  DATA stores" (`habu2.f:720-721`). So even `type-family.f:787`'s definition-time
  `0 PF-COMMIT-N !` and any baked/stdlib `PRODUCT` run post-seal.
- Empirically confirmed: a post-seal `PRODUCT p 0 FIELD x n FIELD y n ;PRODUCT`
  advances `TYPE-FIELD:COUNT` 0 → 2 (PF-COMMIT-N written post-seal); `ENUM` and
  `SUMTYPE` leave it unchanged (only products touch PF).

Once `PF-COMMIT-N` is in a guarded band, its legit `!` writers trap exactly like
the exploit. The seal latch cannot reopen, and no guard-bypassing Forth store
exists. Therefore Design C **requires one new privileged store primitive** — the
"narrow internal-marked bracket" the declaration path uses. This is the
STOP-for-review gate in the dispatch: the primitive is specified below and must
be reviewed before it is added to the engine.

### Proposed primitive `reg-cell!`

```
reg-cell! ( n off -- )
```

- Raw-stores `n` to `data-base + REG-BAND-OFF + off`.
- Hard-bounds `off` unsigned to `[0, REG-BAND-LEN)`; out-of-range traps
  `ENGINE-ERROR:SEAL-VIOLATION` (so the prim can never be an arbitrary-write
  gadget outside the band).
- Emitted as a prim body with a raw `STR` — it does NOT consult the friend latch,
  so it is the sole guard-bypassing writer of the band.
- **Internal-marked** via the `internal-mark.f` self-seal (extend `IMK-PRIM?`
  / `IMK-SEAL-PRIM`, `internal-mark.f:108-121`) so a bare/top-level `reg-cell!`
  rejects with `hb: internal engine word` (rc 70), exactly as bare `PF-ADD`
  already does (proven). It carries **no** checker effect, so checked code sees
  `E-UNDEFINED`. Compiled callers reach it because compiled calls bypass the
  interpret-dispatch internal check.

Reads are unaffected: only writes carry the guard, so `PF-N @` / `PF-COMMIT-N @`
readers stay plain `@`. Only WRITE sites convert `<cell> !` → `<off> reg-cell!`.

One generic bounded prim serves all seven sibling registries (they share one
band), keeping the new-primitive surface at exactly one word. A per-cell prim
family (no `off` argument) is the alternative; it avoids the in-band offset but
costs ~8-16 prims and does not scale to the siblings — rejected in favor of the
single bounded prim.

## Band placement

Cells to protect (dot list, `type-family.f:781-919`):

| cell | kind | site |
|------|------|------|
| `PF-CAP-V` | scalar | 781 |
| `PF-A-P` | scalar ptr | 784 |
| `PF-N` | scalar | 786 |
| `PF-COMMIT-N` | scalar | 787 |
| `PF-TX-CAP-V` | scalar | 915 |
| `PF-TX-P` | scalar ptr | 917 |
| `PF-TX-DEPTH` | scalar | 918 |
| `PF-TX-SERIAL` | scalar | 919 |
| `PF-A-BOOT` | grown arena | 783 |
| `PF-TX-BOOT` | grown arena | 916 |

The eight scalars are fixed-size cells and relocate cleanly into a reserved
`REG-BAND` at fixed DATA offsets, following the DATA-region growth precedent:
new reserved bands are appended at the top of `[0, DATA-START)` and bump
`DATA-START` so no existing offset moves (`layout.f:504-521`,
`PD-TABLE`/`PKGSNAP` precedent). Each becomes a constant returning
`data-base + REG-BAND-OFF + <cell-off>`; `@` reads it, `<off> reg-cell!` writes.

The two `create … allot` **boot arenas** hold row storage that `REG-GROW1`
reallocates onto the DP heap; after growth `PF-A-P`/`PF-TX-P` point outside any
band. Banding the row storage is therefore a Phase-2 sub-design (guard the boot
arena AND route grown-arena row writes through `reg-cell!`, or box the arena
base). The direct COUNT-corruption vector is the scalar counters, so Phase 1
bands the eight scalars; the arena-row protection is tracked separately. The
band obeys the Friend-Arena adjacency rule (`LESSONS.md:1456-1459`): contiguous,
includes any adjacent control cell, never extends an existing band whose end is a
public boundary.

## Trap semantics

A bare or checked `!`/`c!`/`+!`/`atomic*` into the band, post-seal, hits
`GUARD-SPAN`, which writes `ENGINE-ERROR:SEAL-VIOLATION` (83) to fd 2 and calls
`NR-EXIT-GROUP` — the process exits 83. This is stronger than the colon-builder
`internal engine word` rc-70 reject: it is a hard, **uncatchable** exit (not a
`throw`), identical to a FRIEND-ARENA crown-jewel violation. It surfaces the same
on every path: `--load file` aborts the load with exit 83; a bare `stdin` line
exits the REPL process 83. The declaration path never trips it because its
writes go through `reg-cell!` (guard-bypassing), not `!`.

Negative fixtures (add to `test/internal-word-gate.f`): bare `99 PF-COMMIT-N !`
on `--load` and on `stdin` must exit 83; bare `reg-cell!` must reject
`internal engine word`.

## Boot / fixpoint sequencing

1. `REG-BAND` is a fixed DATA offset range appended below `DATA-START` — no
   runtime allocation.
2. The band's `GUARD-BAND` entry is compiled into the store prims during
   cold-prefix prim emission (`habu1.f`), present from engine build.
3. `FRIEND-LATCH` seals at the end of the cold prefix (`habu2.f:713`), BEFORE
   `type-family.f`/`sumtype.f`/stdlib evaluate. So `type-family.f`'s init writes
   and every baked/stdlib `PRODUCT` run post-seal and MUST already use
   `reg-cell!` — this is why the primitive is load-bearing for the engine's own
   self-build, not only for user code.
4. Snapshot / AOT re-entry: `REG-BAND` sits in `[0, DATA-START)`, which the
   snapshot captures and restores wholesale (`layout.f:516-521`); restore runs in
   the boot cold prefix with the latch open (`MODE-BUILD` leaves the latch open
   for the compiler prefix, `habu2.f:726-728`), so restore writes the band
   without trapping. (Verify in Phase 2 that the snapshot DATA range upper bound
   is `DATA-START` and moves with the bump.)

## Boot cost

The band adds ONE `GUARD-BAND` (a few compare/branch instructions) to each
guarded store prim body — a software compare, **no** mprotect syscall. It is NOT
in the LPROT `+41ms` boot-cost class (`LESSONS.md:776`), which was mprotect
full-region bracket growth. Per-store runtime cost is a couple of instructions
only while the latch is sealed. Phase 2 measures the delta against the fixpoint
build baseline; if it is structurally in the `+41ms` class, stop and report.

## TDECL-MARK/RESTORE redundancy (investigated)

`TDECL-MARK`/`TDECL-RESTORE` (`src/core/sumtype.f:61-70`) snapshot and restore
`PF-N`/`PF-COMMIT-N` for every declaration. Investigation result: **the PF part
of that snapshot is redundant.** PF is mutated only by `PRODUCT` (empirically:
ENUM/SUMTYPE do not change COUNT), and only inside `TDECL-PRODUCT-TX`'s own
`PF-BEGIN … PF-ADD … PF-ROLLBACK/PF-COMMIT` transaction (`sumtype.f:689-694`),
which self-restores `PF-N` on failure and advances `PF-COMMIT-N` only on the
outer commit. No PF write occurs outside that transaction, and the only step
after it in `CHECKER-DEFPRODUCT-BODY` is a non-throwing `TDECL-FAM-REG !`, so no
post-commit rollback path exists. Dropping the two PF lines from
`TDECL-MARK`/`RESTORE` therefore removes two would-be-guarded writes from the hot
declaration path and means `TDECL-RESTORE` needs no `reg-cell!` at all. This must
be proven by a negative regression before removal: a `PRODUCT` that throws after
adding one field must leave `COUNT` and provisional state correct with the
snapshot removed.

Note the SEPARATE `TFAM-ROLLBACK-SAVE`/`-RESTORE` frame
(`type-family.f:1408-1427`) is checker-scope rollback for a *rejected family*
declaration; it restores `PF-COMMIT-N` after a committed product inside a
later-rejected family and IS load-bearing — its PF writes DO convert to
`reg-cell!`.

## Per-sibling rollout recipe (`habu-protect-sibling-type-44eec932`)

The pattern is the BAND, shared across all seven registries (`TFAM-N`, `SUMV-N`,
`TF-STR-U`, `TF-PK-N`, `LAY-N`, `SCH-N`, `SCH-ROOT-N` + their cap/pointer cells):

1. Relocate each registry's scalar counter/pointer cells into `REG-BAND` (extend
   the band; bump `DATA-START`).
2. Convert every `<cell> !` write site to `<off> reg-cell!`; leave `@` readers
   unchanged.
3. Add per-registry bare-write negatives to `test/internal-word-gate.f`.
4. No new primitive per sibling — `reg-cell!` is generic and shared.
5. Re-run the fixpoint (byte-identical) and the owning gates.

The grown-arena row storage protection (see Band placement) is the one piece the
recipe defers per registry; track it as an explicit sub-item.
