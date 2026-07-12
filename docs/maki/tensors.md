# Maki tensors — storage, metadata, the tensor value, broadcast classes

The landed tensor layer, checked Habu throughout, one concern per file. A tensor
is storage (`array.f`) plus recorded facts (`tensor.f` dtype, `tensor-value.f`
layout/alignment/shape); the facts travel as **checked type families**, never as
raw integers, so a swapped semantic role (dtype where layout belongs) is a
checker reject, not a runtime surprise.

## `maki/array.f` — tensor-scale storage + whole-tensor ops

Real float tensors as contiguous cell buffers (base `ptr a` + length). Element
access (`T-AT` / `T-GET` / `T-SET`), whole-tensor ops the Habu-PTX kernels run
on device, here on the host: `T-FILL`, `T-SUM ( ptr a n -- r )` (reduction),
in-place `T-ADD! ( ptr a ptr a n -- )`, and the in-place tensor SGD step
`T-SGD! ( r ptr a ptr a n -- )` (`w -= lr*g` over a whole parameter array).
Comparison metrics for goldens and parity tests: `T-DIST2`, `T-NORM2`,
`T-REL-L2 ( ptr a ptr a n -- r )` (relative L2 vs a reference tensor), and the
scalar `T-REL1 ( r r -- r )`. Tests: `maki/array-test.f`.

## `maki/tensor.f` — shape arithmetic + the dtype family

2-D shape arithmetic and the sm_87 dtype set. The `dtype` **ENUM family**
(`ENUM dtype DERIVE eq` — variants `df32 df16 dbf16 du32 di32`; `f32` is a
reserved variant tail, hence the `df` prefix) is the semantic type carried
through construction, Model IR storage, and every consumer. The `DT-*` integer
codes survive **only** as wire/hash vocabulary at two named render boundaries:
`DTYPE>N ( dtype -- n )` and `DT-KEY ( dtype -- ptr u8 n )` (renders
`"f32"`..`"i32"`); both are exhaustive `MATCH`es, so a bad dtype is
unrepresentable and `E-MK-DTYPE` is retired (code reserved). `DERIVE eq`
generates `MAKI-DTYPE:EQ ( dtype dtype -- bool )` so dtype can be an enum field
of the `DERIVE eq` SKEY product (`maki/sched-key.f`).

- `DT-SIZE ( dtype -- n )` — element bytes (f32/u32/i32 = 4, f16/bf16 = 2).
- `SHAPE-ELEMS ( n n -- n )`, `SHAPE-EQUAL? ( n n n n -- bool )`,
  `TENSOR-BYTES ( n n dtype -- n )`.
- Broadcast (NumPy rule, dims equal or either 1): `DIM-BCAST? ( n n -- bool )`,
  `SHAPE-BCAST? ( n n n n -- bool )`, result shape via `DIM-MAX` /
  `BCAST-SHAPE ( n n n n -- n n )`.

Maki owns error range -5000..-5099 (one-way fence: never extends
`lib/errors.f`). Tests: `maki/tensor-test.f`.

## `maki/tensor-value.f` — layout/align families + the single-slot tensor value

Two more fact families in `package MAKI`:

- `ENUM layout DERIVE eq` (`row col`), wire boundaries `LAYOUT>N` / `LAY-KEY`,
  predicate `LAYOUT-ROW? ( layout -- bool )`. `LAY-*` codes are wire-only.
- `ENUM align DERIVE eq` (`unknown byte a4 a8 a16`), order-preserving render
  `ALIGN>N ( align -- n )` (sched-key's ordinal min-fold), predicate
  `ALIGN-UNKNOWN?`, and the fail-closed parse boundary `>ALIGN ( n -- align )`
  (`E-TV-ALIGN` on an out-of-domain code). Alignment is **recorded from the
  real pointer** at construction (`P>N` low bits), never assumed; a descriptor
  with no buffer records `unknown`.

The tensor value itself lives in `package TENSOR`: `TYPEFAMILY tensor 0` is an
opaque nominal one-cell handle indexing a fixed-capacity record table
(`TV-CAP` = 256, free counter `TV-U`); handles carry the store GENERATION, so a
handle from before a `TV-RESET` fails closed (`E-TV-HANDLE`), and no raw-`n`
public conversion exists (identity is `TV-EQUAL?`). The record layout never
leaks (representation hiding per `maki/report.f`), and the family-typed columns
are reachable only through typed slot accessors, so a raw `n` or a foreign
family can never enter a descriptor cell. Shape extents are the nominal
`CAD-KIND:rows` / `CAD-KIND:cols` kinds (`maki/tensor.f SHAPE` constructs);
the address-space fact is `CAD-KIND:address-space` (Model-CAD V2 R3).

- Constructors: `TV-NEW-HOST ( ptr a CAD-KIND:rows CAD-KIND:cols dtype layout -- tensor )`
  (host space, measured alignment),
  `TV-NEW ( ptr a CAD-KIND:rows CAD-KIND:cols -- tensor )` (defaults f32 + row-major),
  `TV-DESC ( CAD-KIND:rows CAD-KIND:cols dtype layout CAD-KIND:address-space -- tensor )`
  (planning descriptor: no buffer, align `unknown`, `TV-DATA@` fails closed
  with `E-TV-NODATA`).
- Accessors, one per recorded fact: `TV-ROWS@` `TV-COLS@` `TV-SPACE@` `TV-ELEMS`
  `TV-DTYPE@ ( tensor -- dtype )` `TV-LAYOUT@ ( tensor -- layout )`
  `TV-ALIGN@ ( tensor -- align )` `TV-HAS-DATA?` `TV-DATA@ ( tensor -- ptr a )`.
  The settable dtype/layout mutators are gone with R3 (descriptor facts are
  fixed at construction).
- Lifecycle: `TV-RESET` (invalidates outstanding handles), `TV-COUNT`;
  bad handle = `E-TV-HANDLE`, full store = `E-TV-FULL`.
- **Plan mode** (descriptor-vocabulary base): `PLINEAR ( tensor tensor tensor
  -- tensor )` and `PGELU ( tensor -- tensor )` do not compute — they append IR
  records (op-kind family, input tensors, output descriptor) to a bounded plan
  store (`PLAN-CAP` = 64) staged by `PLAN-OP-BEGIN ( opkind -- )` /
  `PLAN-IN+` / `PLAN-ATTR!` and committed by `PLAN-OP+`; readers
  `PLAN-OP@ ( n -- opkind )` `PLAN-OUT@` `PLAN-IN@` `PLAN-IN-COUNT@`
  `PLAN-ATTR@`. Builder misuse and capacities are named throws
  (`E-TV-PLAN-STATE` / `E-TV-PLAN-FULL` / `E-TV-PLAN-IDX`).
- **Eager interop proof**: `TV-LINEAR ( tensor tensor tensor tensor -- tensor )`
  unpacks tensor values into the cells eager `MAKI:LINEAR` wants, runs it, and
  wraps the result back; inner-dim mismatch throws `E-TV-SHAPE`.

tensor-value owns -5040..-5049; `E-TV-LAYOUT` and `E-TV-OPKIND` are retired —
the families make out-of-range tags checker rejects. Tests:
`maki/tensor-value-test.f`, including the swapped-family negatives (dtype into
a layout slot and vice versa reject with cited diagnostics).

## `maki/bcast.f` — broadcast-operand classification for lowering

The single source of the broadcast element mapping shared by the host executor
(`EX-BC@` in `maki/executor.f`) and the compute lowerings (`maki/lower-ew.f`,
`maki/lower-red.f`), so device output matches the host golden by construction.
`BC-CLASS ( n n n n -- n )` classifies an operand shape `br x bc` against a
target `R x C` into the four legal flat load-index classes — `BC-FULL` (e),
`BC-ROW` (1xC: `e mod C`), `BC-COL` (Rx1: `e / C`), `BC-SCALAR` (1x1: 0) — or
the fail-closed sentinel `BC-ILLEGAL`, which each caller maps to its own owned
throw (`E-LEW-BCAST` / `E-LRED-BCAST`). Capture-time legality
(`maki/cad.f SHP-LEGAL?`) restricts which class an op may broadcast; this
classifier is the fail-closed re-derivation at lowering. Tests:
`maki/bcast-test.f` (all classes plus the degenerate R=1/C=1 targets where
FULL must win).

## Device-side constructors — `lib/ptx/tile.f` extent tokens

On the kernel side a tensor arrives as a span/matrix whose extent is a checker
**token**, minted by the trusted constructor boundary (the `from_raw_parts` of
the system, per `docs/inference.md`): `MK-SPAN ( ptr<space-global,t> u32 --
span<space-global,t,fresh-extent-n> )`, `MK-SPAN-ONCE`, `MK-SPAN=` (two
pointers, one shared fresh extent), and `MK-MATRIX ( ptr<space-global,t> u32
u32 -- matrix<space-global,t,fresh-extent-r,fresh-extent-c> )` /
`MK-MATRIX-ONCE`. Each call mints rigid fresh tokens, so two independently
constructed spans never unify by accident; agreement must be constructed
(`MK-SPAN=`) or declared in the kernel ABI. The checked SAXPY in
`lib/ptx/tile-test.f` certifies the vocabulary end to end.

## Where the facts flow

The dtype/layout/align (and op-kind) families travel as family values from
construction into the Model IR (`maki/model-ir.f` — `MIR-INPUT+` /
`MIR-OP+ ( CAD-KIND:rows CAD-KIND:cols dtype layout n n -- CAD-KIND:node-id )`)
and out to every consumer; integer codes appear only at the named wire/hash
boundaries above. Graph handles are nominal too: `CAD-KIND:node-id`,
`MIR:input-slot`, `MIR:operand-ref`, and `MIR:input-index`. The field-swap
holes are closed by the checker: a dtype<->layout swap at `MIR-INPUT+` /
`MIR-OP+` / `TV-NEW-HOST` rejects in both directions, a rows<->cols swap and a
raw-`n` extent reject at the same boundaries (pinned in `maki/model-ir-test.f`
and `maki/tensor-value-test.f`), and the schedule key is a `DERIVE eq`
`PRODUCT` over dimclass/dtype/layout/align enum fields (`maki/sched-key.f`),
so a role-swapped key cannot be constructed.

## Design intent (unchanged)

A tensor value = storage (`array.f`) + recorded facts (`tensor.f` /
`tensor-value.f`). The element/scalar rules in `optim.f` / `loss.f` /
`autograd.f` apply per element; the **tensor-level apply** (one
update/reduction/op over the whole tensor) is what lowers onto a checked
Habu-PTX kernel. Roofline: elementwise/reduction tensor ops are memory-bound
(see `docs/kernel-principles.md`), so the device path **fuses** them — never
one kernel per op.

## Verification status

Construction, recorded facts, broadcast classification, plan mode, eager
interop, and the swapped-family negatives are proven in the maki suite
(`bin/hb --load maki/test.f`). The M4 tile-lowering **device** verify legs
(device goldens for the generated kernels over this metadata) are **parked,
pending-zed indefinitely** — the host side proves the emitted PTX
(`maki/lower-ew.f`, `maki/lower-mv-test.f` note the pending device goldens);
nothing here claims a device measurement.
