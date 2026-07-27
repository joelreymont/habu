# Compiler IR implementation plan

Status: scope-locked after three adversarial review rounds and distillation

## Task anchor

Read the new compiler-IR design, write and adversarially review the implementation
plan, split the retained work into dots, and start implementation; use only
`NEWTYPE`, `ENUM`, and `STRUCTURE`, do not use blackboard coordination, and do not
interfere with the independent Spark vLLM-replacement work.

## Pins

- Design: `docs/compiler-ir-design.md`
- Design SHA-256:
  `78b3fa8b2c9290d9416c94e556e2b0977b8d4b83e403c4abc30f199f5348d7c3`
- Design audit commit: `eb5742e916978d5c9067218737ce9c62a1af25a4`
- Reviewed remote base:
  `197fb07d55b3395cdf9bfd007aac999eb6895473`
- Re-fetch before every claim and integration. The Spark agents publish through
  `master`; this campaign does not use `.blackboard`.

## Required result

`CORE` — Replace both direct-emission pipelines with immutable validated stages:

```text
source tape -> HIR -> SIR -> LIR -> A64IR -> allocation/layout -> HBOBJ 2
model IR -> RIR -> KIR -> GIR -> PTXIR2 -> PTX
```

The pipelines share identity, storage, source, type, attribute, schema, freeze,
codec, digest, pass-result, and witness infrastructure, but not dialect
operations. The current compiler remains the sole publisher while the new native
path runs in isolation; there is no hidden fallback. Cutover requires zero
unsupported capabilities for the covered production path, then the old path is
deleted.

## Binding decisions

### Identity

`DERIVED` — Bounds-only IDs alias across modules, so every referential ID carries
an owner.

- A process-wide allocator issues nonzero module serials monotonically and never
  reuses them.
- A referential ID is `(module-serial << 32) | local-u32-index`.
- `IR-ID:ir-module-id` carries a module serial. `IR-ID:ir-count` and
  `IR-ID:ir-pool-offset` are scalar roles and are never packed IDs.
- A dereference checks owner equality before the kind-specific committed bound.
- Runtime serials are not serialized. Canonical encoding remaps references to
  module-local unsigned indices, so construction history cannot change bytes or
  digests.
- The ID owner allocates module serials through one process-wide aligned atomic
  CAS cell. Allocation is monotonic, nonzero, fail-closed at exhaustion, and is
  not reset by replaying `require`.
- Source bytes may be cached by the context, but every frozen module owns its
  source table and `IR-ID:ir-source-id` values. Import remaps equal source
  digests into module-local source IDs.

### Package boundary

`DERIVED` — Raw conversion authority must have one owner.

- `src/compiler/ir/id.f` owns one package, `IR-ID`. Its public window declares
  the ID families and semantic operations; its private window owns every
  checked `CAST:` mint/projection. Both wordlists are protected after
  definition, so later source cannot reopen either authority.
- A cast into a resolved scalar-cell family, including a parametric `NEWTYPE`
  instance, is legal only in the destination family's declaring package.
  Projections out remain unrestricted. This structural owner rule is what lets
  `IR-ID` contain its casts without a second authority package.
- Other substrate concerns use `IR-SOURCE`, `IR-TYPE`, `IR-ATTR`, `IR-SCHEMA`,
  `IR-BUILD`, `IR-VERIFY`, `IR-CODEC`, and `IR-PASS`.
- `src/compiler/ir.f` may open `IR` only to publish semantic facade words over
  public substrate operations. The facade has no `IR-ID` mint authority.
- `refine-lint` inventories and confines every mint. The complete assembly
  protects package wordlists only after a closed schema has passed through the
  real builder/freeze path without reopening `IR`.
- Dialects are separate closed packages: `HIR`, `SIR`, `LIR`, `A64IR`,
  `GPU-RIR`, `GPU-KIR`, `GPU-GIR`, and `GPU-PTXIR2`. The `GPU-` names avoid the
  existing `RIR` and `KIR` packages in `maki/typestate.f`; the architectural
  stage names remain RIR, KIR, GIR, and PTXIR2.

### Storage and lifecycle

`DERIVED` — Table-bearing leaves require ownership and storage before source,
symbol, type, or operation records exist.

- The arena reuses `VEC`/`MEM` allocation and growth and the proven `NOM`
  seal/truncate invariants without copying `NOM`.
- Each builder owns disposable append-only vectors. Abort disposes the complete
  provisional module; freeze commits ceilings and removes every mutation
  surface. Frozen readers accept nominal IDs only.
- If dynamic typed-pointee access is not expressible, one narrow arena boundary
  is audited and tracked for retirement. Generic `n -> a`, public raw converters,
  and per-table casts are forbidden.
- Context owns target policy, numeric policy, source cache, diagnostics, module
  registry, frozen modules, witnesses, and metrics.
- Before context code, focused checker probes must prove the exact linear result
  shapes. If the checker cannot preserve an owner on every refusal arm, the
  missing checker capability is a prerequisite; the IR must not hide ownership
  in globals or add a leaking throw path.

The lifecycle surface is:

```text
CONTEXT-NEW       ( target-contract numeric-policy -- compiler-context )
CONTEXT-DISPOSE   ( compiler-context -- )
NEW-BUILDER       ( compiler-context dialect-schema -- ir-builder )
ABORT             ( ir-builder -- compiler-context )
FREEZE            ( ir-builder -- freeze-result )
MODULE-RETIRE     ( compiler-context IR-ID:ir-module-id -- compiler-context )
PASS-VALIDATE     ( compiler-context pass-result -- compiler-context pass-validation-result )
PASS-ACCEPT       ( compiler-context validated-pass-result -- compiler-context IR-ID:ir-module-id )
PASS-RELEASE      ( compiler-context pass-result -- compiler-context )
```

`freeze-result` and `pass-validation-result` return the context on every arm.
Freeze adds a module only on success. `PASS-VALIDATE` uses a separate validator
package and mints `validated-pass-result` only after every header binding and
pass-specific witness check succeeds; `PASS-ACCEPT` is total only for that
typestate. Disposal uses the repository's owned-release contract: a successful
whole-range release consumes the owner, while an operating-system refusal is a
fatal violated invariant and is never catchable. Handle invalidation occurs
only with successful release. Double release and stale use reject before the
release sink. Context disposal releases all remaining modules and witnesses.

### Determinism and validation

`DERIVED` — The design requires independent validation and stable proof subjects.

- Every dialect uses a closed `ENUM` opcode set and exhaustive data schema.
- Target, numeric policy, schema, pass configuration, source, frozen checker
  environment, compiler/checker identity, input, output, and witness digests are
  explicit.
- Freeze validates owners, bounds, windows, parents, definitions, terminators,
  successor arguments, visibility/dominance, schema, attributes, effects,
  symbols, spans, target legality, and absence of placeholders.
- Canonicalization sorts strings first, then remaps dependency-ordered symbols,
  types, attributes, and sources before rewriting every downstream reference.
  Function, block, operation, operand, and result order remains semantic and is
  not sorted.
- The wire format fixes magic, major/minor version, little-endian scalar widths,
  table order, counts, lengths, decoder limits, and full-input consumption.
- Rendered text is diagnostic-only and is never parsed by compiler code.
- Transformation producers and witness validators are separate packages.

## Ordered implementation

### 0. Baseline evidence

`CORE` — Before changing either backend, record source-pinned inventories and
representative native/GPU measurements required by design section 14, Wave 0.

- Reuse `habu-add-pinned-engine-90090800` only for its checker, fixpoint,
  interpreter, and future inference benchmarks.
- Reuse `habu-adjudicate-dormant-ptx-482310bc` for the current PTX optimizer
  evidence.
- Add the missing native baseline for JIT/AOT latency, emitted bytes, dynamic
  instructions, stack traffic, spills, calls, branches, runtime, binary size,
  and peak temporary memory.
- Add the missing GPU baseline for PTX bytes/instructions, `ptxas` time, cubin
  size, registers, shared memory, spills, occupancy, memory traffic, device time,
  effective throughput, and roofline class.
- Inventory every raw instruction, machine-code scan, branch patch, PTX string
  emitter, `opt-ir` path, production entry, and unsupported capability.
- Add a disabled `new-compiler` capability record and comparison-only native/GPU
  shadow harness plumbing. Wave 0 does not change generated code or publication.
- Missing live Spark measurements block GPU code-generation changes, not the
  shared substrate or native isolated shadow work. Committed evidence is valid
  only when target, toolchain, source digest, and protocol are pinned.

### 1. Shared substrate

`CORE` — Implement in this order:

1. `IR-0.1` ID families, monotonic module authority, and
   pack/project/owner/bound checks.
2. Target contract and numeric policy records plus canonical digests.
3. Context registry, module serial allocation, stale-handle state, and total
   teardown.
4. Disposable typed arena, geometric growth, marks, committed ceilings, and
   module ownership.
5. Module-local source registry, source digests, spans, origin chains, and cycle
   checks.
6. Deterministic string and symbol interning.
7. Canonical scalar, pointer, and token types.
8. Canonical attributes.
9. Closed dialect schema records and exhaustive schema validation.
10. Operation/value/operand/result/successor pools.
11. Function/block parents and windows.
12. Builder, abort, and freeze lifecycle.
13. Structural freeze verifier and hostile mutation fixtures.
14. Canonical string-first table reindexing and reference remap.
15. Deterministic renderer and structural diff.
16. Canonical codec, validating decoder, and digest.
17. Pass-result and witness header, accept/release lifecycle, and corrupt-binding
    fixtures.
18. One closed schema through the real generic builder/freeze path, then facade
    assembly and package protection.

Implementation/proof synchronization is stage-local: each stable schema leaf
may start its proof at the provider instead of waiting for the sealed facade.
Immediately after IR-0.1, `habu-prove-compiler-id-399232c5` owns
`formal/Common/Ids.v`, the ID manifest/digest, parity checks, and shared
valid/hostile vectors while substrate items 2-18 continue.

Memory proof is also stage-local and does not delay the ID proof leaves:
`habu-model-compiler-heaps-dfef07ae` starts after target policy and compiler
types, `habu-prove-compiler-separation-db458ea0` starts after that heap model and
the dialect effect schema, and `habu-prove-compiler-arena-59a8a885` starts after
separation plus the executable IR arena and freeze lifecycle. Later shared,
native, and GPU proofs consume those exact memory, frame, ownership, and
race-freedom obligations.

`habu-type-dsl-prove-93da83c4` is a prerequisite for items 2-18 and every
dialect leaf. IR-0.1 uses `NEWTYPE`, checked `CAST:`, and one atomic allocator;
it may proceed before that hard-cutover proof.
`habu-make-owned-release-79de2b5c` is a prerequisite for context/arena disposal
in items 3-4.

Exit: equivalent modules built in different insertion orders have identical
bytes and digests when only unordered intern-table insertion differs; every
malformed invariant has a named location-aware diagnostic; abort/release leave
no live owner; frozen mutation and raw casts are unresolvable.

### 2. First native vertical slice

`CORE` — Implement design section 14, Wave 2 without broadening its operation set:

1. checker-bound source tape;
2. canonical frozen checker-environment manifest plus compiler/checker identity
   and digest, bound to the source tape and every resulting artifact;
3. HIR literal, modeled arithmetic call, and return;
4. straight-line stack SSA;
5. SIR verification, constant folding, and dead pure elimination;
6. integer LIR and A64IR;
7. table-driven selection, no-call linear scan, and independent allocation
   validation;
8. typed AArch64 encoder adapter and isolated object runner;
9. old/new shadow execution, named coverage, dumps, source maps, and metrics.

Exit: a real checked `SQUARE`-shaped definition reaches executable AArch64 bytes,
matches the old path, and emits no instruction for pure stack renames. The old
path still publishes.

Existing native dots are reconciled before new native leaves:

| Existing dot | Retained owner |
|---|---|
| `habu-idx-arm64-operands-98280863` | A64IR schema, indexed operands, fixups, frame slots, and control effects |
| `habu-lower-native-emission-cbc7f99b` | validated A64IR-to-`A64ENC` adapter and migrated emitter entry; not a parallel old-emitter IR |
| `habu-canonicalize-typed-native-7d698b51` | post-slice SIR/LIR structural optimization |
| `habu-emit-proof-carrying-058f43b6` | allocation witness and independent validator |

### 3. Remaining native, GPU, and proof waves

`CORE` — Follow the exact operation sets and acceptance gates in
`docs/compiler-ir-design.md`:

- Native Waves 3-8: sections 14.4-14.9.
- GPU Waves A-E: sections 14.10-14.14.
- Formal synchronization and gates: sections 10-12 and 16.6.

The decisive exits are structured control, calls/exceptions, defining semantics,
wide `ENUM`/`STRUCTURE` values, `HBOBJ 2` AOT, and self-host cutover for native;
SAXPY PTXIR2, elementwise KIR, softmax, MMA, and planner/tuner cutover for GPU.
Each stable schema ships its manifest/digest, valid and hostile canonical
fixtures, witness vectors, and the assumptions report for any covered theorem.

GPU Wave A routes a bounded SAXPY operation set from the existing checked DSL
boundary into a PTXIR2 builder and renders only at the final sink. It never
parses generated text. Existing GPU ownership is reconciled first:

| Existing dot | Retained owner |
|---|---|
| `habu-verify-ptx-virtual-50281017` | sole PTXIR2 instruction schema, verifier, state, and renderer owner |
| `habu-ptx-opt-layer-325b9507` | split into KIR/GIR structure and Wave B-E pass/tuner outcomes; no PTXIR2 duplicate |
| `habu-ptx-register-pressure-ed521b40` | virtual-register liveness/pressure scheduling, dense naming, and exact declarations; physical assignment remains `ptxas` work |
| `habu-v2-resource-model-985a0b0e` | target resource facts and promotion evidence consumed by GPU validators |

GPU work does not edit `maki/infer/` while the Spark agents are active.

## Specification coverage

`CORE` — Every normative design section has an implementation owner:

| Design sections | Plan owner |
|---|---|
| 5-6 | shared substrate items 1-18 |
| 7, 14.3-14.9 | native first slice and Waves 3-8 |
| 8, 14.10-14.14 | GPU Waves A-E |
| 9 | stable pass implementations, independent validator leaves, and explicit numeric policy |
| 10 | per-stage Rocq semantics, schema parity, witness validators, and composed proof gates |
| 11-12 | Wave 8 self-host/bootstrap proof and wave-owned retain/replace/retire cuts |
| 13 | dot-frozen one-concern package/file ownership |
| 14.2, 15 | Wave 0 capability record plus explicit shadow coverage and no silent fallback |
| 16 | structural, correctness, performance, and formal gates attached to their owning leaves |
| 17 | every dot's frozen interface, mutation test, destruction review, and green integration proof |
| 18-20 | IR-0/NATIVE-1 leaves plus native/GPU lowering fixtures |
| 21 | final native, GPU, and trust campaign exits |

## First implementation leaf: IR-0.1

`CORE` — This leaf owns ID representation and process-wide module serial
allocation. It does not create contexts, dereference arenas, or add
sources/builders/codecs.

Files:

```text
src/compiler/ir/id.f
test/compiler/ir-id-concurrency.f
test/compiler/ir-id.f
lib/errors.f
FILEMAP.md
LESSONS.md
docs/compiler-ir-design.md
docs/forth.md
src/core/checker.f
src/core/type-family.f
src/habu/xref.f
test/cast-negative-suite.f
tools/refine-lint-core.f
tools/refine-lint-test.f
```

No task, manifest, loader, fixpoint, AOT, public-signature, or shared structural
lint source is changed by this leaf.

It declares public `NEWTYPE` identities in package `IR-ID` with exact tails
`ir-module-key`, `ir-module-id`, `ir-source-id`, `ir-fun-id`, `ir-block-id`,
`ir-op-id`, `ir-value-id`, `ir-type-id`, `ir-attr-id`,
`ir-symbol-id`, `ir-span-id`, `ir-pool-offset`, and `ir-count`. Private checked
`CAST:` words in that same owner package implement module capabilities, packed
referential IDs, and scalar count/offset roles. `NEW-MODULE` is the sole public
module-key constructor. No raw `n -> ir-module-key` path is public.

The frozen representation contract is:

```text
module serial:        1 .. 0x7fffffff
local unsigned index: 0 .. 0xffffffff
packed referential:   (serial << 32) | local
count/pool offset:    0 .. 0x7fffffffffffffff, never packed
```

The exact 26 private `IR-ID` representation casts are:

```text
MINT-KEY           ( n -- IR-ID:ir-module-key )
KEY>N              ( IR-ID:ir-module-key -- n )
MINT-MODULE        ( n -- IR-ID:ir-module-id )
MODULE>N           ( IR-ID:ir-module-id -- n )
MINT-COUNT         ( n -- IR-ID:ir-count )
COUNT>N            ( IR-ID:ir-count -- n )
MINT-POOL-OFF      ( n -- IR-ID:ir-pool-offset )
POOL-OFF>N         ( IR-ID:ir-pool-offset -- n )
MINT-{KIND}        ( n -- IR-ID:ir-{kind}-id )
{KIND}>N           ( IR-ID:ir-{kind}-id -- n )
```

Packing, projection, and validation are public checked `IR-ID` words that
compose only those private identity-shaped casts:

```text
PACK-{KIND}        ( IR-ID:ir-module-key n -- IR-ID:ir-{kind}-id )
{KIND}-OWNER       ( IR-ID:ir-{kind}-id -- IR-ID:ir-module-id )
{KIND}-LOCAL       ( IR-ID:ir-{kind}-id -- n )
{KIND}-CHECK       ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-{kind}-id
                     -- IR-ID:ir-{kind}-id )
```

`{KIND}` expands exactly to `SOURCE`, `FUN`, `BLOCK`, `OP`, `VALUE`,
`TYPE`, `ATTR`, `SYMBOL`, and `SPAN`. The later arena/builder is the only
semantic caller of `PACK-*`; the codec is the only semantic caller of `*-LOCAL`.
`refine-lint` confines private raw definitions to `id.f`'s private `IR-ID`
window and rejects direct definitions, qualified references, and `EXPORT`
aliases across the complete 19-package compiler API set.

The exact block and named errors reserved in `lib/errors.f` are:

```text
-6600 E-IR-FIRST
-6699 E-IR-LAST
-6600 E-IR-MODULE-ZERO
-6601 E-IR-MODULE-RANGE
-6602 E-IR-INDEX-RANGE
-6603 E-IR-INDEX-BOUND
-6604 E-IR-OWNER
-6605 E-IR-SCALAR-RANGE
-6606 E-IR-MODULE-EXHAUSTED
```

Acceptance:

- `NEW-MODULE` yields a nonforgeable key plus matching public module ID;
- an outcome-bounded fresh child starts every task over a disjoint slice; each
  worker atomically publishes `READY`, waits without a scheduler-dependent spin
  limit for the parent `GO`, allocates its slice, validates its typed key/owner
  pair, and publishes completion;
- one private test-only erase-only `CAST:` projects a validated module owner to
  its raw serial for process-shared `create` storage; it is outside `IR-ID`,
  cannot mint a nominal, is absent from every compiler API and production load
  path, and does not change the exact 26-cast production inventory;
- the parent releases `GO` only after all workers are live, verifies every
  stored serial is nonzero and globally unique, and attempts to kill/release
  every prepared task on success or caught failure;
- an activation-failure case catches exact `E-TASK-STATE`, then reruns the
  normal case with the same four task objects in the same child; deleting
  cleanup makes reuse fail;
- deleting the `READY`/`GO` protocol makes the overlap witness fail, while the
  process outcome timeout owns genuine stalls;
- require replay in a fresh child does not reset the serial;
- explicit valid key/local index inputs round-trip each referential ID;
- scalar count/offset roles never acquire module bits;
- negative/equal-to-bound/overflow index and foreign owner throw named errors
  before memory access;
- `E-IR-MODULE-ZERO`, `E-IR-MODULE-RANGE`, and
  `E-IR-MODULE-EXHAUSTED` guard private states unreachable through the sole
  public key/allocator path; the allocator transition and law proof leaves own
  executable defensive-state coverage plus monotonic, nonzero, unique, and
  exhaustion-before-wrap unreachability proofs;
- checker fixtures reject wrong-family substitutions;
- the CAST owner gate binds the destination package to the engine's live
  namespace record and actual public/private definition WID; callable
  `CHECKER-PACKAGE` and direct checker-mirror mutation cannot authorize a mint;
- canonical local projection excludes the runtime owner;
- no public raw converter or dialect-specific cast resolves;
- all 26 raw tails belong only to the private `IR-ID` dictionary window;
- `refine-lint` rejects direct definitions and `EXPORT` aliases of raw tails in
  all 19 compiler API packages;
- both `IR-ID` wordlists reject reopen, qualified publication, direct WID
  publication, and source replay;
- every cast is checker-certified and confined by `refine-lint`;
- `bin/hb --load test/compiler/ir-id.f` passes;
- the existing `compiler-ir-id` suite schedule remains intact; error, refine,
  file-map, package, typed-local, and suite coverage gates pass.

## Dot and worker rules

`DERIVED` — Decomposition must reuse the existing dots named above and create
only uncovered outcomes. Every retained item appears once; no review-only dot is
created.

Before dispatch, follow the parallel-work and dot-dispatch rules in `AGENTS.md`: establish an active claim
on an immutable base and use an isolated `.jj-ws/<dot-id>`. Separate workspaces
may edit overlapping files concurrently; never assign overlapping files to
concurrent editors in the same workspace. Each leaf gets its focused test,
required diff/lint gates, independent destruction review, and exact owning
integration gate before closure.

Before IR-0.1 integrates or pushes, fetch, rebase, and reconcile every
overlapping change; verify remotely valid ownership; then run the exact focused
and publication gates on the reconciled tree. Items 2-18 remain blocked on
`habu-type-dsl-prove-93da83c4`.

## Cut from the previous plan

Pre-distillation version: `PLAN.md` SHA-256
`bf375f936846fbd0df6695fad6e14c747dee1e260ce9e3e89103df8fb673fdf7`,
324 lines, planning base `b8e46224929f6d65062610fec55e2f2f0140f318`.

| Cut | Why it was cut |
|---|---|
| Review matrix | Review machinery, not implementation scope |
| Repeated full acceptance lists for later waves | The design already owns them; the plan retains decisive exits and exact section links |
| Fixed first-version file layout for every later dialect | Premature detail not required to start the dependency-ordered substrate |
| Generic per-leaf gate checklist | Repository workflow already owns it; only the first leaf needs exact routing now |
| Blackboard lifecycle text | Explicitly disabled by the user for the remote Spark agents |
| Automated Wave 0 semantic rediscovery | The design requires a source-pinned inventory, not a new call-graph/provenance analyzer; audit hashes plus structural review force re-audit without delaying compiler work. Rejected split: `b776cdd872ce25da8dd0c0fc0e8cb09f5c4ca564`. |
