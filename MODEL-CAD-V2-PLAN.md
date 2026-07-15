# Model CAD V2 Plan

**Status:** proposed architecture  
**Date:** 2026-07-11  
**Scope:** Model CAD kernel, Maki compiler stack, required checker capabilities,
artifact database, incremental build/test architecture, migration, and retirement
of V1 singleton state.

This plan supersedes the implementation architecture in CAD-PLAN.md and
docs/model-cad.md. Their product goals, device measurements, and validated
workloads remain inputs unless explicitly replaced here.

## 1. Mission

Model CAD V2 makes the immutable design database, typed artifact graph, and
verified pass engine the center of Habu. The REPL becomes a client of that kernel
instead of the owner of mutable model state.

The primary client is an autonomous LLM model engineer. After a human supplies
a signed objective, dataset authority, deployment constraints, and safety
policy, the agent must be able to construct, train, diagnose, optimize,
validate, deploy, monitor, and roll back a model without unstructured human
intervention. Habu owns the legal action space and evidence requirements; the
LLM proposes bounded transactions inside that space.

The core loop is:

~~~text
edit model
  -> create a new immutable model revision
  -> invalidate only dependent analyses and artifacts
  -> generate legal implementation alternatives
  -> select layouts, schedules, and lowering plans
  -> certify and measure exact candidates
  -> promote one reproducible artifact
~~~

A model revision, every derived plan, every generated kernel, and every promotion
verdict must be immutable, content-addressed, inspectable, replayable, and
independent of hidden process state.

## 2. Why V2 Is A Redesign

The current system is a successful vertical prototype, but its architecture is
still singleton-oriented:

- maki/model-ir.f owns one fixed-capacity global model table.
- Shape binding and planning mutate shared tables in place.
- maki/cad.f combines parsing, generated-source compilation, shape propagation,
  planning, validation, persistence, reporting, and public commands.
- CAD commands reconstruct reports from implicit global state.
- Op and schedule extension requires synchronized tables and dispatch ladders.
- The store persists rendered rows rather than a typed revision/artifact graph.
- Content-addressed incremental replanning is documented but is not the
  organizing implementation abstraction.

V2 does not wrap these structures in more commands. It replaces them with
explicit typed artifacts and pass contracts.

## 3. Architectural Invariants

1. Semantic IR is immutable.
2. Every derived fact names its exact source revision.
3. Every pass declares inputs, outputs, target/config dependencies, effects,
   invalidation, diagnostics, and verifier.
4. Every artifact is content-addressed from canonical content.
5. Public semantics never depend on a process-global current model.
6. Legality, equivalence, and profitability are separate decisions.
7. Lowering consumes a complete plan and makes no hidden scheduling decisions.
8. Promotion requires typed evidence for the exact artifact.
9. Diagnostics are structured artifacts.
10. Runtime design storage grows with input size.
11. Parser, IR, analysis, rewrite, plan, backend, evidence, persistence, and UI
    remain separate concerns.
12. Equal canonical inputs and toolchain facts produce equal artifact hashes.
13. Cached results are validated against the complete dependency set.
14. V2 does not normalize missing checker capability with raw n conversions,
    unchecked tables, or local TRUSTED boundaries.

### 3.1 Unified Type Declaration Hard Cutover

Model CAD V2 has exactly two public composite/type-family declaration blocks:

```forth
STRUCTURE name arity [ POLICY policy ] [ DERIVE feature ... ]
  FIELD field-name type
;STRUCTURE

ENUM name arity [ POLICY policy ] [ DERIVE feature ... ]
  VARIANT variant-name
    FIELD field-name type
  ;VARIANT
;ENUM
```

The compact `ENUM name variant ... ;ENUM` form omits the arity and header
clauses and is legal only for payloadless variants. The first token after the
name selects the form: a decimal arity selects block mode; a bare variant
selects compact mode. The modes cannot mix. Type, field, and variant tails are
lowercase. Project packages and generated words remain uppercase. Arity is
mandatory for `STRUCTURE` and block `ENUM`, while compact `ENUM` is
implicitly arity zero.

`STRUCTURE` unifies opaque cell families, pointer-layout records, value
records, and products. Zero fields mean an opaque one-cell family with no
generic raw constructor. One or more fields mean a nominal product with one
checker-owned schema driving stack width, storage layout, typed address
projection, reflection, codecs, snapshots, and AOT metadata. Public
field-bearing structures generate a closed package containing checked
`MAKE`/`UNMAKE` and typed field operations. Field-address words use exactly
`FAMILY:FIELD ( ptr family<a,...> -- ptr field-type )`.

`ENUM` unifies payloadless enums and payload-bearing sums. Each variant payload
uses the same named `FIELD` schema as `STRUCTURE`; the compiler selects the
payloadless enum kind only when every variant has no fields. Public enums
generate one checked constructor per variant in a closed package. `MATCH`
remains the checked eliminator. Constructor inputs and `MATCH` payload bindings
use field declaration order, deepest first.

Both blocks write one field registry keyed by `(family-id,
optional-variant-id, field-tail)`. A row owns slot, schema root, width,
alignment, byte offset, visibility, and source span. Structure fields use no
variant id; enum payload fields use their variant id. Field-name reflection,
constructors, `UNMAKE`, `MATCH`, codecs, snapshots, and AOT all consume those
same ordered rows. Existing constructor-package spelling is preserved through
the current injective uppercase escape/join algorithm (`point` -> `POINT`,
package `PX-PROBE` plus `pxevid` -> `PX--PROBE-PXEVID`).

The raw-structure bootstrap cycle is removed instead of generalized. Internal
records needed before the checker hook use explicit named cell or byte offsets,
record strides, ordinary accessor words, and load-time assertions for every
offset, size, alignment, and pointer-field role. These implementation layouts
have no family ids, reflection, constructors, parser, definer, descriptor
arena, adoption phase, snapshot rows, or AOT rows. Native and Gforth recovery
sources carry exactly the same layouts and parity tests. The sole executable
`STRUCTURE`/`ENUM` parser loads only after the checker, type registries, render
support, and checker hook.

This is a hard cutover. The implementation removes `TYPEFAMILY`, `PRODUCT`,
`;PRODUCT`, `VALUE-RECORD`, `END-VALUE-RECORD`, `BEGIN-STRUCTURE`,
`END-STRUCTURE`, `+FIELD`, `PTR-FIELD:`, `CFIELD:`, `SUMTYPE`, `;SUMTYPE`,
`ENUM+`, and `ENUM4+`. They have no aliases, shims, desugaring path, or second
registry. Error-only compiler tombstones report `E-REMOVED-TYPE-SYNTAX`; they
cannot execute or mutate metadata. Mixed compact/block enums, anonymous variant
payloads, legacy field words/closers inside new blocks, and a missing arity on
`STRUCTURE` or block `ENUM` reject at the exact token.

The implementation dependency chain is:

1. `habu-type-dsl-specify-db2bf883` — freeze grammar, census, and migration ownership.
2. `habu-migration-core-records-77182600` — replace every pre-checker raw record
   with explicit asserted implementation layouts and establish native/recovery
   load-order parity.
3. `habu-type-dsl-unify-b65d46c1` — one transaction and field/variant schema.
4. `habu-type-dsl-implement-50f8dc15` — the sole post-hook typed `STRUCTURE`
   parser and generated operations.
5. `habu-type-dsl-implement-a762cfaf` — payload-aware `ENUM` and compact payloadless form.
6. `habu-checker-certify-unified-5d56fe73` — certification, diagnostics, replay, and rollback.
7. `habu-compiler-lower-unified-5f599080` — compiler capture, lowering, snapshot,
   and AOT parity for the post-hook language only.
8. `habu-migration-core-variants-af8e09b4` — core enum migration and bounded
   declaration-census proof.
9. `habu-migration-libs-to-4e798110`,
   `habu-migration-tools-to-d4e8fcf8`,
   `habu-migration-tests-and-51d00332`, and
   `habu-migration-maki-models-c965e65d` — consumer migration.
10. `habu-type-dsl-delete-8bd73b41` — delete every old definer, registry path,
   generated operation, snapshot row, and AOT row.
11. `habu-type-dsl-enforce-19a93c1a` — zero-occurrence lint and retired-token diagnostics.
12. `habu-type-dsl-prove-93da83c4` — fixpoint, recovery, snapshot/AOT, full gate, and final census proof.

No dependent V2 schema is complete while it uses a removed declaration. The
exact baseline and file-to-owner routing live in
`docs/census-type-dsl-cutover.md`.

## 4. Architecture

~~~text
MODEL DSL / ONNX
        |
        v
  MODEL-IR                 semantic ops, attrs, symbolic descriptors
        |
        v
  TENSOR-IR                solved shapes, layouts, aliases, effects
        |
        v
  REGION-IR                legal rewrite and fusion alternatives
        |
        v
  PLAN-IR                  memory, schedule, precision, save/recompute
        |
        v
  KERNEL-IR                target-independent executable structure
        |
        v
  PTX-IR -> PTX -> CUBIN   target lowering and executable module
        |
        v
  EVIDENCE -> PROMOTION    exact proof and measurement bundle

All objects live in the DESIGN DATABASE.
All arrows are registered passes in the PASS ENGINE.
~~~

Cross-cutting services:

- canonical schema encoding and hashing;
- dependency recording and invalidation;
- target descriptions and calibrated cost models;
- diagnostics and provenance;
- artifact/evidence persistence;
- deterministic replay;
- build, test, and performance telemetry.

## 5. Type-System Capability Tiers

The capability tiers are architectural commitments, not priority adjectives.

- **Required:** V2 cannot satisfy its core invariants or retire V1 without it.
- **Beneficial:** the first narrow V2 vertical slice can run without it, but
  scaling, extensibility, or proof quality remains materially compromised.
- **Awesome to have:** high-leverage research capability that is deliberately
  outside the V2 critical path.

V2 does not require unrestricted dependent types, higher-kinded types, a general
effect calculus, or full lifetime inference. It requires bounded, decidable
capabilities tied to concrete CAD artifacts.

## 6. Required Type-System Improvements

### R1. Complete Layout-Family Storage

Finish the existing typed ADT memory campaign:

- width-aware non-linear layout store/fetch for sums and products;
- typed layout arrays such as LAYOUT-BUFFER;
- checked indexing returning ptr family;
- iteration without exposing hidden fields;
- growable OS-backed backing storage;
- rollback and snapshot persistence;
- canonical serialization driven by family metadata;
- explicit rejection of unsupported linear-in-memory cases.

Why required:

The design database, IR nodes, pass descriptors, plans, and evidence are nested
typed records. Parallel raw-cell columns would recreate the V1 architecture and
erase family identity at every storage boundary.

Acceptance:

- wide sum and product round-trip through memory;
- nested product/enum arrays;
- family-mismatch store/fetch rejects;
- bad index, stride, and capacity reject;
- rejected declarations restore registry state;
- no hidden-field public signature;
- no new trust boundary.

Tracked foundation: habu-checker-capability-typed-a480c423. Its width-1 tier is
not the full V2 requirement; the wide and typed-array tiers are required.

### R2. Derived Equality, Hashing, Ordering, And Canonical Codecs

Generate family-specific operations from checker-owned metadata:

- equality;
- stable hash;
- deterministic order;
- canonical encode/decode;
- schema-aware visit/fold where needed by the database.

Why required:

Content-addressed objects and typed indexes cannot key on rendered strings or
hand-maintained raw comparisons. Canonical persistence cannot depend on a
separate handwritten encoder for every record.

Acceptance:

- equal values encode and hash identically;
- unequal schema-significant values change the hash;
- canonical map/set ordering is deterministic;
- decode/encode round-trips;
- malformed input returns typed diagnostics;
- policy-dependent layouts either derive correctly or reject;
- linear fields reject illegal copy/inspection derivations.

Tracked foundation: habu-checker-capability-derive-23788e95.

### R3. Nominal Artifact And Index Kinds

Add distinct kinds or nominal families for:

- design-id;
- rev-id;
- obj-id;
- node-id;
- analysis-id;
- plan-id;
- artifact-id;
- evidence-id;
- target-id;
- toolchain-id;
- pass-id;
- schema-id.

Also add domain index kinds:

- dim;
- shape;
- rows;
- cols;
- dtype;
- layout;
- address-space;
- stage;
- effect;
- region.

Landed deviation: `dtype` and `layout` shipped as maki ENUM families
(`maki/tensor.f`, `maki/tensor-value.f`) rather than CAD-KIND rows — strictly
stronger (real layout values with MATCH/derived eq), per merge-policy dot
habu-merge-policy-master-961bb2b7.

Why required:

All of these are one-cell values physically. Treating them as n permits
semantically catastrophic swaps that ordinary stack checking cannot detect.
`rows` and `cols` are distinct even though both are dimensions: a generic
`dim` cannot reject a transposed constructor call, swapped matrix descriptor,
or row/column table access. Shape algebra owns the only checked role-changing
operations, such as transpose and contraction.

Acceptance:

- every cross-role swap rejects;
- storage preserves the nominal family;
- public CAD APIs contain no raw n handles;
- renderer and diagnostics preserve role names;
- rollback, snapshots, source replay, and derived operations retain kinds.

Design decision:

- use public zero-field `STRUCTURE` declarations in package `CAD-KIND`, not
  `DEFTYPE` and not new checker tokens;
- inside `CAD-KIND`, signatures use the unqualified lowercase family tail;
  outside it, signatures use the qualified form
  `CAD-KIND:design-id`, `CAD-KIND:node-id`, and so on;
- do not publish universal `n` conversion words. The module that allocates,
  decodes, or indexes a value owns a private audited refinement from its raw
  representation after validating range, generation, schema, or provenance;
- keep public Forth words uppercase. Lowercase spelling is reserved for the
  type-family tokens required by the checker grammar;
- reject `DEFTYPE` for this surface because its nominal registry is global and
  its generated `>NAME` / `NAME>N` pair makes raw conversion generally
  available. Those properties are useful for low-level scalar roles but do not
  provide package-owned CAD authority.

No checker extension is required for the kind declarations themselves. The
existing family-id representation supplies nominal identity. Ordinary
`ptr CAD-KIND:node-id` fetch/store preserves that identity; a node value cannot
enter `ptr CAD-KIND:design-id`. Qualified diagnostics render the package and
tail outside the owner package, while owner-local diagnostics render the tail.

Registry and transaction contract:

- `TFAM` stores `(package, visibility, lowercase tail, arity=0, TK-CELL)` and
  signatures store the resolved family id rather than spelling;
- `CHECKER-SCOPE-*` and `CHECK-CANDIDATE-*` restore the `TFAM` high-water,
  interned-string, and package state through the installed rollback hooks;
- `CHECKER-SNAPSHOT-PREPARE` persists grown `TFAM` stores and rejects snapshots
  inside a live rollback frame;
- native load, verify-source, check-core, and all-errors support replay all
  recognize `STRUCTURE`, so a declaration has one source-replay meaning;
- derived products/sums refer to the same family ids; encoders render a kind
  only at the canonical wire boundary and decoders validate before refining.

Required negative fixtures:

~~~forth
: BAD-ID ( CAD-KIND:design-id -- CAD-KIND:node-id ) ;
: BAD-STORE ( CAD-KIND:node-id ptr CAD-KIND:design-id -- ) ! ;
: BAD-STAGE ( CAD-KIND:artifact-id CAD-KIND:stage -- CAD-KIND:evidence-id ) ... ;
~~~

The first two already reject in direct probes with qualified expected/actual
types. Implementation keeps those probes, adds package-local render assertions,
and adds rollback, snapshot, source-replay, and canonical round-trip coverage.

Migration order:

1. declare and gate `CAD-KIND` without changing existing APIs;
2. migrate graph identity and tensor descriptor kinds;
3. migrate stage/effect/region and design/revision/pass identities;
4. migrate artifact/evidence/target/toolchain identities;
5. migrate storage and canonical encoders, then remove raw-handle conversions;
6. fail the gate on any remaining public CAD handle or index spelled `n`.

Tracked implementation slices:

- package declarations and checker fixtures:
  `habu-v2-r3-declare-3fcdeebb`;
- model-IR node/reference identity: `habu-v2-r3-type-dfe5609e`;
- tensor dimension/rows/cols/dtype/layout/address-space identity:
  `habu-v2-r3-type-9f89d1e9`;
- stage/effect/region identity: `habu-v2-r3-type-5809bec6`;
- design/revision/object/analysis/plan/pass/schema identity:
  `habu-v2-r3-type-5a20bd12`;
- artifact/evidence/target/toolchain identity:
  `habu-v2-r3-type-2f60c17c`;
- persistent storage and canonical codec preservation:
  `habu-v2-r3-preserve-f081f2c9`;
- final public-signature audit and raw-`n` handle lint:
  `habu-v2-r3-forbid-23051b46`.

### R4. Constraint-Indexed Tensor Types

Introduce the conceptual family:

~~~text
tensor<shape,dtype,layout,address-space,region>
~~~

The bounded constraint language supports:

- dimension constants;
- fresh symbolic dimensions;
- equality;
- multiplication by a known constant;
- exact division and ceiling division by a known constant;
- ranked shape construction and projection;
- broadcast relations;
- matmul inner-dimension equality;
- reshape element-count equality;
- alignment divisibility;
- layout/view compatibility.

Why required:

Without it, MODEL composition can prove only tensor-versus-nontensor and stack
arity. Shape, dtype, and layout legality remains runtime mutation logic, contrary
to Model CAD's author-time checking goal.

Acceptance:

- same-shape elementwise composition certifies;
- scalar and row broadcast certify with evidence;
- incompatible broadcast rejects during checking;
- matmul inner mismatch rejects during checking;
- reshape count mismatch rejects during checking;
- independently fresh dimensions do not unify;
- shared dimension tokens survive locals and quotations;
- diagnostics identify the op, operand, failed constraint, and provenance.

Tracked foundation: habu-checker-shape-kind-4c6a3f4c.

### R5. Existential Shapes And Runtime Refinement

Support runtime-known shapes without erasing type information.

Required semantic forms include:

~~~text
some<shape,tensor<shape,dt,lay,space,region>>
shape-eq<a,b>
broadcastable<a,b,out>
aligned<ptr,align>
bounded<idx,len>
~~~

A validator consumes raw metadata and returns a refined value plus evidence.
Opening an existential introduces a fresh rigid token. It cannot escape except
inside another existential package.

Why required:

ONNX imports, runtime input binding, dynamic batches, and externally supplied
buffers cannot all have compile-time concrete dimensions. Opaque tensor handles
would discard the checker benefit at the system boundary.

Acceptance:

- validated runtime dimensions produce typed existential tensors;
- independently opened shapes remain distinct;
- equality evidence permits safe unification;
- raw integer metadata cannot forge evidence;
- branch-local refinement cannot leak after MATCH;
- invalid runtime metadata produces a structured diagnostic.

### R6. Region Ownership And Borrowing

Extend linear types with a narrow checker-generated region model:

- linear arena-owner<region>;
- copyable immutable arena-ref<region,t>;
- unique arena-mut<region,t>;
- lexical transaction/borrow scope;
- no reference escape beyond its owner;
- no simultaneous illegal mutable/immutable access.

Apply the same model to device buffers, modules, contexts, and mapped host
buffers where appropriate.

Why required:

Immutable graphs need shared reads. Builders and device resources need unique
ownership. Pure raw pointers cannot prove either property.

Acceptance:

- immutable graph references copy safely;
- owner destruction with an escaping reference rejects;
- double mutable borrow rejects;
- mutation through immutable reference rejects;
- transaction commit consumes mutable authority;
- device allocation/free and module/context lifetimes balance.

This is not general lifetime inference. Region tokens are lexical or
transaction-scoped and checker-generated.

### R7. Typestate And Evidence Families

Represent pipeline state in types:

~~~text
model<elaborated>
tensor-ir<constraints-solved>
region-ir<legal>
plan<complete>
kernel-ir<verified>
candidate<emitted>
evidence<certified>
evidence<golden>
evidence<gradchecked>
artifact<promoted>
~~~

Evidence is indexed by the artifact hash or nominal artifact id.

Why required:

A report tag checked at runtime does not make promotion structurally safe.
Pass ordering and gate ownership must be impossible to bypass through the public
API.

Acceptance:

- unconstrained Model IR cannot enter region planning;
- incomplete Plan IR cannot enter lowering;
- unverified Kernel IR cannot enter target emission;
- promotion without policy-required evidence is untypeable;
- evidence from one artifact cannot satisfy another artifact;
- profile can be mandatory-to-record but non-blocking through explicit policy.

### R8. Explicit Capability Effects

Use explicit capability tokens and op-schema effect rows for:

- pure;
- parameter read;
- state write;
- random;
- host IO;
- device launch;
- atomic/reduction;
- collective/barrier;
- allocation/free;
- persistent publication.

R8 has two related but deliberately separate representations.

The **static effect row** is package-owned by sealed `CAD-EFFECT`. `PURE` is
the unique empty row. Every non-pure entry names an atom, a stable site path, a
slot kind, and a slot index relative to an op schema, word signature, declared
capability, or quotation capture. Local declarations start with an empty path;
`CAD-EFFECT:REMAP` prefixes caller/call-site path segments while preserving the
original resolvable slot. One atom may have several bindings, so weight and bias reads
do not collapse. Different atoms may bind the same slot, so an atomic state
write can express both facts. Direct duplicate insertion rejects; canonical
`CAD-EFFECT:UNION` is associative, commutative, and idempotent. It imposes no
small semantic composition bound on stored words or quotations. Versioned wire
counts and checked allocation/resource budgets are explicit protocol and policy
boundaries, and their overflow or exhaustion returns a typed diagnostic.

The public row value is a one-cell opaque nominal handle backed by a sealed
immutable layout arena, not a wide by-value `PRODUCT`. Handle numbers, arena
offsets, pointers, allocation order, and internal hashes are implementation
references only. Canonical sorted binding contents define equality,
serialization, diagnostics, replay, AOT, fixpoint bytes, and cache identity.
Persistent site paths and chunked rows have no small fixed composition or path
depth bound; a versioned wire count plus checked allocation/resource failures
provide the protocol boundary. A private linear transactional builder streams
sorted union/remap results and freezes once, so transitive composition is not
quadratic. Rollback restores every high-water/index before a handle escapes,
and published arena spans are permanently immutable and protected from raw
stores, atomics, FFI/syscall outputs, remapping, and writable reprotection.

Static rows contain no artifact digest, pointer, process-local address, mutable
generation, RNG position, or authority instance. Stored-word and quotation
metadata keep bindings relative to declared inputs and captures. At a call or
quotation boundary the checker must apply capture-avoiding
`CAD-EFFECT:REMAP` into a stable caller/call-site namespace before union. Raw
union of two callee-local slot numbers is unsound because both callees may call
their first resource `slot 0`. Primitive declarations, stored effects,
quotations, registry rows, replay records, and snapshots validate through the
sealed owner before any permissive decision.

The **resolved semantic binding set** is execution-specific. One checked
resolver combines a static row with typed invocation operands, attributes,
capability tokens, canonical artifact metadata, and a stable semantic site
derived from revision/node/call structure. It produces sorted entries of the
form `(atom, site-path, slot-kind, slot-index, semantic-fact)` or a typed
uncacheable/unresolved reason. Parameter reads resolve immutable payload
digests; state and random effects bind owner plus generation or sequence; IO,
device, allocation, atomic, collective, and publication bind the exact
authority facts permitted by policy. Addresses and insertion order never enter
the result. Only an exact repeated full tuple is intentionally idempotent. The
same atom/site/slot resolving to different semantic facts is a conflict, while
different sites or slots remain distinct even when their local slot numbers or
payload digests match.

Fusion and recompute legality consume sealed rows plus successfully resolved
bindings. Only `PURE` and immutable digest-bound parameter reads may duplicate;
unresolved reads and every mutable or externally observable effect are
conservative barriers. Schedule, compile, result, replay, evidence, and
promotion caches do not all depend on the same subset. A sealed versioned
projection policy derives a domain-specific digest plus completeness evidence
from the full resolved set. Each owner consumes that projection rather than
filtering bindings privately. Every omitted fact has an explicit tested
irrelevance rule; an unknown atom, domain, or unproved omission falls back to
the full digest or a typed uncacheable result. The policy version participates
in the key. This R8 model covers
semantic effects of the generated operation; generated register, flag, frame,
and control clobbers remain the separate R9 machine-state contract.

Why required:

Rewrite, fusion, recomputation, caching, and pass scheduling are unsound if
stateful/random/IO operations look pure. A general effect calculus is not
required; the finite CAD effect vocabulary is.

Acceptance:

- random/stateful ops cannot be duplicated for recompute;
- writes and atomics cannot cross illegal reorder/fusion boundaries;
- pure passes run without IO/device authority;
- analysis-only contexts cannot publish persistent artifacts;
- two callees or quotations that each bind local slot zero remain distinct after
  checker remapping, while replaying the same remapped binding is idempotent;
- weight, bias, mutable generation, RNG position, target/device authority, and
  publication scope mutations change the exact affected key or make it
  explicitly uncacheable;
- each cache key includes every capability-controlled semantic input relevant
  to its artifact class, every omission has completeness evidence, and no key
  depends on addresses or traversal order;
- package sealing, every metadata/registry ingress, snapshot, replay, and
  fixpoint preserve canonical row authority.

Tracked implementation slices:

- `habu-fix-wide-product-5c81dada` landed the independent checker correction
  that counts lowered wide PRODUCT cells once instead of quadratically;
- `habu-protect-dynamic-immutable-eccd0489` owns permanent dynamic protected
  spans and the complete raw-mutation/remap sink boundary;
- `habu-add-immutable-nominal-9290a81f` owns opaque handles, canonical immutable
  row storage, transactional builders, and rollback/snapshot/AOT/replay scale;
- `habu-define-finite-cad-0bdf52ad` owns static row vocabulary, remap, union,
  and legality tables;
- `habu-seal-cad-effect-49cac404` owns the final authority boundary;
- `habu-persist-cad-semantic-028c0881` owns checker metadata and call-site
  substitution;
- `habu-require-maki-op-b14ccc89` owns mandatory static Maki schema rows;
- `habu-infer-linear-kinds-1f77b4c4` and
  `habu-add-explicit-cad-58a05453` close polymorphic capability laundering and
  expose explicit authority tokens;
- `habu-resolve-runtime-cad-2864336f` owns execution-specific resolution;
- `habu-enforce-effect-aware-cf9181b8` owns fusion/recompute legality;
- `habu-census-cad-effect-3240237b` freezes cache dependency domains and splits
  disjoint owners; `habu-define-complete-cad-90a9945c` owns sealed projection
  policies before `habu-key-caches-by-fddcea19` integrates the key migration.

### R9. Generated Machine-State Integrity

The checked host word that emits an instruction and the generated instruction
operate on different machines. A correct Forth stack effect proves only the
emitter's data stack; it does not prove the generated GPR, SIMD, NZCV, SP,
frame-slot, label, fixup, call, or return state. Generated code therefore needs
its own typed instruction and routine-effect layer.

Required forms:

- package-scoped instruction operands indexed by register class, address
  space, bounded immediate, frame slot, label, fixup kind, and control effect;
- one inferred or declared routine contract for every callable emitted label,
  including live-ins, reads, writes, returns, preserves, flags, SP delta and
  alignment, no-return paths, syscalls, direct calls, and indirect calls;
- path-sensitive liveness and frame verification over the actual emitted CFG
  and resolved fixups, not a linear source-token approximation;
- linear region ownership and phase capabilities for scratch buffers, capture
  artifacts, frame storage, and one-shot hooks;
- an independently checkable allocation certificate bound to the exact IR,
  target, CFG, machine code, frame, and verifier version.

Why required:

Register clobbers, branch-only restores, frame overlap, scratch-buffer aliasing,
and same-cell operand swaps can all preserve the host stack effect. Handwritten
name-to-mask tables detect only enumerated cases and drift when emitter syntax
or call graphs change. These failures must reject before generated code or an
artifact can be promoted.

Acceptance:

- missing or contradictory callable contracts reject;
- wrong register class, fixup kind, frame slot, or control effect rejects;
- live-in through call to later read, one-branch restore, loop-carried clobber,
  unsaved LR, SP imbalance/misalignment, callee-save destruction, NZCV loss,
  SIMD clobber, and untyped indirect calls reject on the emitted CFG;
- nested scratch reuse, escaped borrows, overlapping fixed DATA ranges, and
  out-of-order capture phases reject;
- bounded differential execution agrees with every machine-effect row;
- allocation certificates reject any code, CFG, assignment, spill, frame,
  call-effect, or content-hash mutation.

#### R9 PTX Generated-State Contract

PTX is not an exception to R9 merely because it is virtual. Habu does not own
the final NVIDIA physical-register allocation or instruction schedule, but it
does own the generated PTX virtual machine, the exact assembler boundary, and
the identity carried into launch and promotion. The proof chain is therefore
split at the proprietary boundary instead of pretending that a host stack
effect or a successful `ptxas` process proves more than it does.

The generated PTX state is target-indexed. A `PTX-STATE<TARGET>` contains:

- the exact target and feature set used to admit every instruction;
- nominal virtual-register identities with register class, scalar/vector
  element type, definition site, and one-definition state;
- predicate definitions and the control region in which each predicate is
  valid;
- labels, edges, dominance, reconvergence regions, and path-join live state;
- pointer value type, address space, alignment fact, and access width;
- parameter, register, shared, local, and constant resource declarations and
  their observed use;
- collective identity, mask/uniformity fact, and barrier phase supplied by the
  M5 owner;
- canonical instruction, CFG, target, and verifier-version digests.

The instruction verifier is a separate checked consumer of the generated
instruction graph. It does not trust emitter-local counters, text spelling,
paths, traversal order, or a prior compiler pass. PTX text may be rendered only
from a verified state. Any compatibility renderer that still accepts raw text
is an explicit boundary, cannot produce promotable evidence, and must be
retired by the PTX-state implementation leaf.

Its static invariants are:

1. Every virtual register has exactly one definition and every use is dominated
   by that definition on every incoming path.
2. Definition and use agree on register class, value type, vector width, and
   target capability; spelling equality is never type authority.
3. Every predicate is defined before use, has predicate class, and is used only
   in a compatible control region.
4. Branch joins agree on live register, predicate, address-space, resource, and
   collective phase state; an absent or contradictory join rejects.
5. Memory instructions agree with pointer address space, value type, access
   width, alignment evidence, and the enclosing extent/mask authority.
6. Resource declarations are complete and agree with instruction use; undeclared
   shared/local/parameter state or conflicting declarations reject.
7. Barrier and reconvergence legality comes from the M5 uniformity model and is
   checked against the actual instruction CFG, not inferred from PTX text.
8. The verification evidence names the exact instruction graph, CFG, target,
   and verifier version; any mutation requires re-verification.

#### PTX Boundary Layers

The layers intentionally prove different facts:

| Layer | Subject | Proved fact | Not proved |
|---|---|---|---|
| Habu checker | emitter definition | The host Forth stack effect and package/type-family contracts compose. | Generated virtual-register or CFG state. |
| `PTX-STATE:VERIFY` | target-indexed PTX instruction graph | Virtual def/use, type, predicate/control, address-space, resource, collective, and CFG legality. | NVIDIA physical allocation, scheduling, or SASS semantics. |
| `PTXAS-ATTEST:ASSEMBLE` | verified PTX plus target/toolchain/config | Exact input provenance, assembler success, typed resource report, exact cubin digest, and opaque-backend status. | Correctness of proprietary allocation or scheduling. |
| `CUBIN-INTEGRITY:VERIFY` | cubin/SASS artifact | Payload identity, target/toolchain/PTX/attestation binding, symbol/ABI binding, and optional policy-required disassembly evidence. | Kernel semantics for arbitrary inputs. |
| device semantic verifier | exact cubin launch subject | Golden, numeric, safety, or performance evidence for the named environment, inputs, launch, and policy. | Evidence for another cubin, environment, or policy. |
| promotion verifier | artifact plus required evidence set | All required evidence has the same subject and satisfies the promotion policy. | Any omitted or mismatched claim. |

`ptxas` allocation and scheduling are proprietary. Habu must not emit a
proof-carrying allocation certificate for those decisions unless an
independent verifier can actually reconstruct them. The accepted boundary is
an opaque-backend attestation bound to canonical verified PTX, immutable target
and toolchain descriptors, exact invocation policy, parsed resource facts,
diagnostics, cubin bytes, and attestation version. This proves provenance and
observable facts. Device evidence supplies bounded semantic evidence. The
proof-carrying allocation-certificate dot remains limited to allocations Habu
owns and can independently verify.

Unsupported backends are policy outcomes, not implicit fallbacks. If a target
policy requires a resource field, disassembly verifier, or independent subject
binding that the backend cannot provide, compilation may produce a typed
non-promotable diagnostic artifact, but promotion rejects.

#### Fixed PTX Call-Graph Census

This is the fixed implementation census for the three PTX leaves. Files outside
it require a new reviewed census; they are not silently absorbed into a leaf.

1. Maki lowering originates in `maki/lower-ew.f`, `maki/lower-red.f`,
   `maki/lower-mm.f`, and `maki/lower-move.f`.
2. Expression construction and checked kernel semantics pass through
   `lib/ptx/ir.f`, `lib/ptx/cg.f`, `lib/ptx/header.f`,
   `lib/ptx/collective.f`, and the collective renderer
   `lib/ptx/cg-collective.f`; the local baseline encoder is
   `src/arch/ptx/emit.f`.
3. Assembly crosses `lib/ptx/toolchain.f` and the independent smoke path
   `tools/ptx/ptxas-smoke.f`. Device assembly fixtures are consumers of the
   same attestation package, not alternate proof paths.
4. Cubin registration, module loading, ABI binding, and launch cross
   `maki/lower-launch.f`.
5. Device semantic evidence is produced by `maki/lower-golden.f` and exercised
   end to end by `maki/lower-model-device-test.f`.
6. Evidence presentation, promotion policy, and durable rows cross
   `maki/report.f`, `maki/cad.f`, and `maki/store.f`.

The fixed path is therefore:

~~~text
maki/lower-*
  -> lib/ptx/{ir,cg,header,collective} + src/arch/ptx/emit
  -> lib/ptx/toolchain + tools/ptx/ptxas-smoke
  -> maki/lower-launch
  -> maki/lower-golden + maki/lower-model-device-test
  -> maki/report + maki/cad + maki/store
~~~

#### Fail-Closed Ownership

| Failure | Sole owner |
|---|---|
| Undefined use, duplicate definition, wrong register class, incompatible join, predicate/control mismatch, address-space/type mismatch, or declaration/use mismatch | `habu-verify-ptx-virtual-50281017` |
| Divergent barrier or invalid collective reachability | `habu-ptx-m5-mask-eb0716f1` |
| Lost kernel phantom through checked emitters | `habu-ptx-phantom-preserving-3df9db92` |
| Unknown, incomplete, or stale target/toolchain identity | `habu-v2-r3-define-987402c7`, consumed by the `ptxas` leaf |
| Missing assembler, failed process, input/config mismatch, malformed resource report, report drift, or unverifiable opaque backend | `habu-attest-proprietary-ptxas-6ce9fda2` |
| Predicted-versus-attested resource error or invalid occupancy | `habu-v2-resource-model-985a0b0e` |
| Cubin mutation, wrong region/symbol/ABI/launch subject, stale device evidence, or wrong-subject promotion | `habu-bind-cubin-and-c1103e74` |
| Generic artifact-envelope mismatch | `habu-v2-canonical-artifact-ee5121b4` |
| Proof-domain, independence-policy, or required-environment mismatch | `habu-v2-proof-obligation-6cf70b4f` |
| CUDA rc, cleanup, readback, or launch failure | `habu-make-ptx-device-c0eb12a3` |
| Committed kernel device-golden regression | `habu-committed-device-correctness-9ca4cbc6` |
| Independently certified allocation owned by Habu | `habu-emit-proof-carrying-058f43b6` |

No failure has two implementation owners. Consumers depend on the named owner
and preserve its typed evidence rather than restating the check.

#### PTX Implementation Leaves

- `habu-verify-ptx-virtual-50281017` adds the target-indexed instruction/state
  ADTs, actual-CFG verifier, structured diagnostics, and render gate. It follows
  the M5 uniformity/barrier and phantom-preservation owners.
- `habu-attest-proprietary-ptxas-6ce9fda2` adds exact `ptxas` provenance,
  typed resource-report parsing, opaque-backend attestation, and deterministic
  replay. It follows PTX state verification, R3 toolchain identity, and generic
  proof obligations. The resource-model dot consumes its attested facts.
- `habu-bind-cubin-and-c1103e74` adds immutable cubin/SASS identity through
  typed registration, pre-load verification, launch, device evidence, durable
  replay, and promotion. It follows the `ptxas` attestation and fail-closed
  device-proof infrastructure; committed device goldens consume it.

Tracked slices:

- `habu-specify-ptx-generated-5a3a902d` freezes the PTX boundary census and
  delegates implementation to `habu-verify-ptx-virtual-50281017`,
  `habu-attest-proprietary-ptxas-6ce9fda2`, and
  `habu-bind-cubin-and-c1103e74`;
- `habu-define-typed-arm64-4ab8894f`;
- `habu-idx-arm64-operands-98280863`;
- `habu-verify-emitted-arm64-efd5eb61`;
- `habu-add-bounded-host-b40b048f`;
- `habu-add-lexical-mutable-725b49eb`;
- `habu-derive-fixed-data-853cb615`;
- `habu-add-linear-capture-172b29da`;
- `habu-differentially-test-arm64-7b6e4269`;
- `habu-emit-proof-carrying-058f43b6`.

### R10. Type-System Dependency Order

~~~text
R1  wide layout storage and typed arrays
R2  derived eq/hash/order/codecs
R3  nominal artifact and domain index kinds
R4  dimension/shape constraints
R5  existential packaging and runtime refinement
R6  region owner/reference capabilities
R7  typestate and artifact-indexed evidence
R8  explicit CAD effect capabilities
R9  generated instruction, machine-state, and resource effects
~~~

R1-R3 unblock the design database. R4-R5 unblock a genuinely typed Model/Tensor
IR. R6-R8 unblock safe transactions, transformation, caching, and promotion.
R6 and the audited primitive-effect table unblock R9; R9 makes native and
device code generation independently verifiable.

Do not implement the V2 database using parallel untyped columns while waiting
for R1-R3. That would make the migration itself architectural debt.

## 7. Beneficial Type-System Improvements

### B1. Rewrite Equivalence Witnesses

A verified rewrite produces:

~~~text
equiv<input,output,semantic-domain>
~~~

Witnesses are constructed only by audited rewrite axioms and proof combinators:
composition, symmetry, transitivity, and congruence.

Benefit:

The optimizer can replay why two graphs are equivalent independently of the
search that found the rewrite. Cache invalidation can include axiom versions.

This is beneficial rather than required because the first V2 slice can use
independently verified, deterministic hand-written passes plus golden tests.

### B2. Declarative Schema Reflection

Expose checked family metadata for:

- field/variant traversal;
- canonical schema descriptors;
- generic visitors;
- migration tooling;
- diagnostic rendering;
- registry completeness checks.

Benefit:

Op schemas, IR schemas, persistence, docs, and tooling stop maintaining parallel
knowledge.

Constraint:

Reflection returns typed metadata views. It never exposes writable checker
registries or hidden physical fields.

### B3. Policy-Aware Boxed Families

Complete boxed layouts for recursive diagnostic/schema values:

- region-owned allocation;
- non-null or niche policy;
- derived traversal/serialization;
- cycle handling;
- policy-correct equality/hash;
- no raw-pointer semantic equality unless declared.

Benefit:

Recursive diagnostics, external schemas, and proof DAGs become easier to model.
Core model graphs still use typed ids and immutable arenas, so boxed recursion is
not required for the first database.

### B4. Shape-Polymorphic Declared Quotations

Allow a quotation to declare indexed input/output relations and safely open
existentials within lexical scope.

Benefit:

Generic passes, visitors, and schedule filters can remain checked without
expanding bespoke dispatch words.

The existing checked higher-order capability remains the base. This addition
must preserve quotation/local capture rules and effect visibility.

### B5. Refined Numeric Roles

This section is the canonical design anchor for the refined-numeric design dot;
older line-number references to this requirement refer here.

All of these values occupy one cell, but they do not have interchangeable
meaning. A byte length is not a cell count, an index is not an offset, and an
alignment is not a divisor merely because all can currently pass through `n`.
Global `>LEN`, `>COUNT`, `>IDX`, and `>OFF` casts establish nominal identity
only: they do not validate a predicate and remain compatibility boundaries, not
the V2 authority model.

#### B5.1 Package And Scalar Roles

The owner is package `CAD-NUM`. Its constituent files are assembled while the
package is reopenable; the dedicated CAD-NUM final-assembly file seals it only
after every scalar constructor, arithmetic word, and owner API has been defined.
A partially assembled `CAD-NUM` is not an authority boundary and must not be
loaded by a production V2 entry point.

`CAD-NUM` declares public arity-zero nominal families for the reusable scalar
facts:

~~~text
CAD-NUM:byte-len    nonnegative extent measured in bytes
CAD-NUM:item-count  nonnegative logical element count
CAD-NUM:cell-count  nonnegative machine-cell count
CAD-NUM:index       nonnegative ordinal, not yet bounded
CAD-NUM:byte-off    nonnegative byte offset
CAD-NUM:cell-off    nonnegative cell offset
CAD-NUM:alignment   positive power-of-two alignment
CAD-NUM:positive-divisor positive divisor for unit-preserving extent arithmetic
CAD-NUM:alloc-byte-len positive byte extent accepted by an allocator
CAD-NUM:alloc-cell-count positive cell count accepted by a cell allocator
~~~

The lowercase tails are checker type-family vocabulary. Public Forth words are
uppercase and package-qualified, for example `CAD-NUM:BYTE-LEN` and
`CAD-NUM:CELLS>BYTES`. Do not introduce global `CAD-NUM-*` prefix words or new
checker tokens.

The existing shared `result<a,b>` accepts cell-kinded parameters today. A
closed `CAD-NUM:error` layout would be a layout value, so
`result<CAD-NUM:byte-len,CAD-NUM:error>` is not currently expressible; that
nesting would depend on `habu-checker-capability-layout-9b8540bd`. B5 does not
need to wait for that capability. `CAD-NUM` instead owns the target
payload-bearing `ENUM` `CAD-NUM:numeric-result<a>`, whose success variant carries
one cell-kinded `a` and whose error variants are payloadless:

- ok `a`;
- negative;
- zero;
- overflow;
- underflow;
- bad-alignment;
- misaligned.

The hard-cutover representation is zero-field `STRUCTURE` roles plus the full
payload-bearing `ENUM`. Unified declaration lowering has not landed;
`habu-migrate-cad-num-cf178e59` owns the current source migration after
`habu-compiler-lower-unified-5f599080` lands. The design needs no nested-layout
capability; a closed instantiation can already be stored through the landed
`LAYOUT-BUFFER` when needed. Expected validation failures return
`CAD-NUM:numeric-result<a>`; they do not throw and they do not collapse
different failures into a flag. I/O, allocation, or corrupted owner-state
failures remain ordinary propagated errors at their owning boundary.

The checker cannot prove that an `n` satisfying a runtime predicate has become
an arity-zero nominal family. Every raw-to-role mint is therefore a private,
audited `TRUSTED:` representation boundary, not a checked conversion. Each
public constructor validates the raw value first and calls its corresponding
private mint only on the success path:

~~~forth
CAD-NUM:BYTE-LEN   ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
CAD-NUM:ITEM-COUNT ( n -- CAD-NUM:numeric-result<CAD-NUM:item-count> )
CAD-NUM:CELL-COUNT ( n -- CAD-NUM:numeric-result<CAD-NUM:cell-count> )
CAD-NUM:INDEX      ( n -- CAD-NUM:numeric-result<CAD-NUM:index> )
CAD-NUM:BYTE-OFF   ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-off> )
CAD-NUM:CELL-OFF   ( n -- CAD-NUM:numeric-result<CAD-NUM:cell-off> )
CAD-NUM:ALIGNMENT  ( n -- CAD-NUM:numeric-result<CAD-NUM:alignment> )
CAD-NUM:POSITIVE-DIVISOR
  ( n -- CAD-NUM:numeric-result<CAD-NUM:positive-divisor> )
CAD-NUM:AS-ALLOC-BYTE-LEN
  ( CAD-NUM:byte-len
    -- CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> )
CAD-NUM:AS-ALLOC-CELL-COUNT
  ( CAD-NUM:cell-count
    -- CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> )
~~~

Zero is valid for `byte-len`, `item-count`, `cell-count`, `index`, and both
offsets: empty tensors, strings, vectors, and zero-distance ranges are ordinary
values. Zero is invalid only where the role says so: `alignment`,
`positive-divisor`, `alloc-byte-len`, and `alloc-cell-count`. Allocation callers
first construct a zero-admitting extent/count, then explicitly pass through the
corresponding `AS-ALLOC-*` validator. The validator returns the `zero` variant;
it does not throw and no allocator accepts a zero-admitting scalar role.
`AS-ALLOC-CELL-COUNT` succeeds only for `1 <= count <= MAX-N / CELL-BYTES`;
zero returns `zero`, and the first count whose `cells` conversion would exceed
`MAX-N` returns `overflow` before an allocation primitive is reachable.

Every mint and any unavoidable role-to-`n` primitive adapter receives a
`TRUSTED.md` row, a trusted-inventory classification, a focused validation test,
and a removal condition. No public unchecked raw mint or conversion exists. A
role-specific refined-to-`n` projection may exist only in private owner code
where an existing primitive cannot consume the role directly. It is explicit
proof erasure, never paired with a public inverse, and must not appear in a V2
public signature.

Ordinary reopenable packages cannot protect these private mints. `CAD-NUM` is
not unforgeable while its constituent files are being assembled. Permanent
sealing belongs to the open `habu-seal-cad-num-36dbeec6` implementation dot. It
may dispatch only after package sealing, unified declaration migration, and the
native/REPL definer-registration residual have landed; only the fully assembled
and sealed load path may claim authority. Any scalar/result storage uses the
landed typed `LAYOUT-BUFFER`; an untyped `create ... cells allot` table plus
trusted casts is not an acceptable substitute.

#### B5.2 Checked Arithmetic

Role-changing arithmetic is owned and named. This table is the complete B5
algebra; an omitted pair is statically unavailable. Operand order is canonical
and no reversed overload is implied. `R<t>` below abbreviates
`CAD-NUM:numeric-result<t>` only inside the B5 design tables.
Unqualified lowercase type tails in those tables are all `CAD-NUM` families;
for example, `byte-len` means `CAD-NUM:byte-len`, not a global checker token.

| Word | Exact stack effect | Required boundary classes |
|------|--------------------|---------------------------|
| `CAD-NUM:ADD-BYTES` | `( byte-len byte-len -- R<byte-len> )` | zero identity; maximum-safe sum; first overflow |
| `CAD-NUM:ADD-ITEMS` | `( item-count item-count -- R<item-count> )` | zero identity; maximum-safe sum; first overflow |
| `CAD-NUM:ADD-CELLS` | `( cell-count cell-count -- R<cell-count> )` | zero identity; maximum-safe sum; first overflow |
| `CAD-NUM:ADVANCE-BYTE-OFF` | `( byte-off byte-len -- R<byte-off> )` | zero distance; maximum-safe advance; first overflow |
| `CAD-NUM:ADVANCE-CELL-OFF` | `( cell-off cell-count -- R<cell-off> )` | zero distance; maximum-safe advance; first overflow |
| `CAD-NUM:ADVANCE-INDEX` | `( index item-count -- R<index> )` | zero distance; maximum-safe advance; first overflow |
| `CAD-NUM:SUB-BYTES` | `( byte-len byte-len -- R<byte-len> )` | equal gives zero; positive difference; first underflow |
| `CAD-NUM:SUB-ITEMS` | `( item-count item-count -- R<item-count> )` | equal gives zero; positive difference; first underflow |
| `CAD-NUM:SUB-CELLS` | `( cell-count cell-count -- R<cell-count> )` | equal gives zero; positive difference; first underflow |
| `CAD-NUM:RETREAT-BYTE-OFF` | `( byte-off byte-len -- R<byte-off> )` | zero distance; exact-to-zero; first underflow |
| `CAD-NUM:RETREAT-CELL-OFF` | `( cell-off cell-count -- R<cell-off> )` | zero distance; exact-to-zero; first underflow |
| `CAD-NUM:RETREAT-INDEX` | `( index item-count -- R<index> )` | zero distance; exact-to-zero; first underflow |
| `CAD-NUM:BYTE-DISTANCE` | `( byte-off byte-off -- R<byte-len> )` | equal gives zero; positive distance; reversed underflow |
| `CAD-NUM:CELL-DISTANCE` | `( cell-off cell-off -- R<cell-count> )` | equal gives zero; positive distance; reversed underflow |
| `CAD-NUM:INDEX-DISTANCE` | `( index index -- R<item-count> )` | equal gives zero; positive distance; reversed underflow |
| `CAD-NUM:MUL-ITEMS` | `( item-count item-count -- R<item-count> )` | zero in either position; maximum-safe product; first overflow |
| `CAD-NUM:SCALE-BYTES` | `( byte-len item-count -- R<byte-len> )` | zero in either position; maximum-safe product; first overflow |
| `CAD-NUM:SCALE-CELLS` | `( cell-count item-count -- R<cell-count> )` | zero in either position; maximum-safe product; first overflow |
| `CAD-NUM:DIV-BYTES` | `( byte-len positive-divisor -- byte-len )` | zero dividend; divisor one; maximum dividend; total, so no error variant |
| `CAD-NUM:REM-BYTES` | `( byte-len positive-divisor -- byte-len )` | zero dividend; exact zero remainder; nonzero remainder; total, so no error variant |
| `CAD-NUM:DIV-ITEMS` | `( item-count positive-divisor -- item-count )` | zero dividend; divisor one; maximum dividend; total, so no error variant |
| `CAD-NUM:REM-ITEMS` | `( item-count positive-divisor -- item-count )` | zero dividend; exact zero remainder; nonzero remainder; total, so no error variant |
| `CAD-NUM:DIV-CELLS` | `( cell-count positive-divisor -- cell-count )` | zero dividend; divisor one; maximum dividend; total, so no error variant |
| `CAD-NUM:REM-CELLS` | `( cell-count positive-divisor -- cell-count )` | zero dividend; exact zero remainder; nonzero remainder; total, so no error variant |
| `CAD-NUM:CELLS>BYTES` | `( cell-count -- R<byte-len> )` | zero; maximum-safe cell count; first overflow |
| `CAD-NUM:CELL-OFF>BYTE-OFF` | `( cell-off -- R<byte-off> )` | zero; maximum-safe cell offset; first overflow |
| `CAD-NUM:BYTES>CELLS` | `( byte-len -- R<cell-count> )` | zero; exact maximum; first misaligned byte length; no overflow class |
| `CAD-NUM:BYTE-OFF>CELL-OFF` | `( byte-off -- R<cell-off> )` | zero; exact maximum; first misaligned byte offset; no overflow class |
| `CAD-NUM:ALIGN-UP-BYTES` | `( byte-len alignment -- R<byte-len> )` | zero; alignment one; already aligned maximum; first overflowing round-up |
| `CAD-NUM:ALIGN-UP-BYTE-OFF` | `( byte-off alignment -- R<byte-off> )` | zero; alignment one; already aligned maximum; first overflowing round-up |
| `CAD-NUM:BYTES-ALIGNED?` | `( byte-len alignment -- bool )` | zero is aligned; exact and non-exact values; no overflow/underflow class |
| `CAD-NUM:BYTE-OFF-ALIGNED?` | `( byte-off alignment -- bool )` | zero is aligned; exact and non-exact offsets; no overflow/underflow class |

The table intentionally rejects `index + index`, `offset + offset`,
`length + offset` in reversed order, alignment arithmetic, cross-unit add/sub,
and multiplication or division of indexes or offsets. It also rejects generic
raw-`n` arithmetic. New combinations require a consumer, unit proof, boundary
matrix, and a plan amendment; they are not inferred from representation size.

A successful typed result proves that this operation completed without the
named numeric failure. It does not prove a general value equation between
arbitrary future operands. Shape element-count equalities, symbolic products,
and rewrite algebra remain R4/R5 constraints rather than an accidental general
dependent-arithmetic system in B5.

#### B5.3 Relational Evidence Is Out Of Scope

B5 is scalar-only. It does not define, store, mint, or migrate
`bounded-index`, `aligned-size`, owner, region, extent, or generation evidence.
Bounds and alignment are relations over identities, so their parameterized
evidence remains owned by R5 and `habu-add-bounded-host-b40b048f`, including
runtime reification and container/span integration. `CAD-NUM:index`,
`CAD-NUM:item-count`, `CAD-NUM:byte-len`, and `CAD-NUM:alignment` are merely
validated scalar inputs to those owners; none proves a relation by itself.

The R5 and bounded-host implementations must ensure that owner/region identities
and mutable generations are never reused or allowed to wrap into a previously
observable identity. Monotonic allocation either succeeds with a fresh identity
or fails closed on exhaustion; clear, shrink, reallocation, and owner replacement
invalidate prior evidence. Their owning dots must add wrap/exhaustion,
cross-container same-size, owner-recreation, and stale-after-clear/shrink
regressions before implementation is assigned. B5 neither duplicates those
records nor claims their runtime or checker guarantees.

#### B5.4 Static And Runtime Proof Matrix

`CHECK!` proves role flow, not numeric predicates over literal values. Candidate
fixtures therefore separate checker rejection from runtime result tests.

Positive checker candidates:

~~~forth
: GOOD-OFF-SWAP ( CAD-NUM:byte-off CAD-NUM:cell-off
  -- CAD-NUM:cell-off CAD-NUM:byte-off ) swap ;
: GOOD-DIV-COUNT ( CAD-NUM:item-count CAD-NUM:positive-divisor
  -- CAD-NUM:item-count ) CAD-NUM:DIV-ITEMS ;
~~~

Negative checker candidates:

~~~forth
: BAD-OFF-SWAP ( CAD-NUM:byte-off CAD-NUM:cell-off
  -- CAD-NUM:byte-off CAD-NUM:cell-off ) swap ;
: BAD-RAW-OFF ( n -- CAD-NUM:byte-off ) ;
: BAD-DIV-ROLE ( CAD-NUM:item-count CAD-NUM:alignment
  -- CAD-NUM:item-count ) CAD-NUM:DIV-ITEMS ;
: BAD-MUL-ROLE ( CAD-NUM:item-count CAD-NUM:byte-len
  -- CAD-NUM:numeric-result<CAD-NUM:item-count> ) CAD-NUM:MUL-ITEMS ;
: BAD-INDEX-PLUS-INDEX ( CAD-NUM:index CAD-NUM:index
  -- CAD-NUM:numeric-result<CAD-NUM:index> ) CAD-NUM:ADVANCE-INDEX ;
~~~

The positive candidates must return accepted and every negative candidate must
return rejected through the standard quiet candidate harness. R5 and
bounded-host maintain their own relational-evidence checker/runtime matrices;
B5 does not restate placeholder signatures for capabilities it does not own.

The memory owner adds the following candidates only after a real public package
`MEM` exists and the allocator has its final checked signature; a placeholder
word cannot make this negative pass:

~~~forth
: GOOD-ALLOC ( CAD-NUM:alloc-byte-len
  -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES ;
: BAD-ZEROABLE-ALLOC ( CAD-NUM:byte-len
  -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES ;
~~~

The owner first creates/reopens `MEM`, lands and tests `MEM:ALLOC-BYTES`, then
migrates every legacy direct caller and removes or tightens the old global
`MEM-ALLOC-BYTES` signature. Only that exact post-migration tree may claim the
positive/negative allocator candidates.

Runtime `T{ ... -> ... }T` fixtures then prove the value predicates:

- negative length/count/index/offset returns negative; zero length, count,
  index, and offset constructors succeed;
- `CAD-NUM:POSITIVE-DIVISOR` accepts positive input, returns zero for zero, and
  returns negative for negative input;
- each `AS-ALLOC-*` validator accepts one and its maximum representation,
  returns zero for zero, and cannot be called with the other allocation unit;
- every algebra row runs exactly the zero, overflow, underflow, and
  misalignment classes named in its table row; the total positive-divisor
  operations return their role directly and expose no impossible error variant;
- alignment construction accepts one and the largest positive power of two,
  while zero, negative, `MAX-N`, and other non-powers-of-two return
  bad-alignment;
- division tests zero dividend, divisor one, exact division, and maximum
  dividend; remainder tests zero dividend, exact zero remainder, and a nonzero
  remainder, separately for bytes, items, and cells. Relational bound/alignment,
  identity wrap/exhaustion, and stale-evidence cases remain in their
  R5/bounded-host owning suites.

No test may claim that `CHECK!` rejects `-1`, zero, or an overflowing literal
unless value refinement has separately landed. Those are runtime validator
outcomes in this bounded design.

#### B5.5 Dependencies, Owners, And Implementation Slices

Dependency order:

1. package-scoped zero-field `STRUCTURE` roles and a full `ENUM` result are the
   hard-cutover target; `CAD-NUM:numeric-result<a>` remains limited to
   cell-kinded payloads without nested-layout support. The current CAD-NUM
   sources still use historical pre-cutover declarations, and
   `habu-migrate-cad-num-cf178e59` owns their migration after the unified DSL
   lands;
2. the landed `LAYOUT-BUFFER` supports closed non-linear layouts and arity-zero
   nominal scalars with checked fixed-capacity indexing, typed store/fetch,
   extent overflow checks, rollback, and zero initialization. This is sufficient
   for any private fixed-capacity scalar/result storage B5 actually needs;
3. `habu-nominal-storage-raw-a3430ef2` landed at `085cf242`: TVK-RAW now fences
   the baked `here` effect and verifier-registered
   `variable`/`create`/`constant` effects against nominal-family value laundering
   while retaining scalar storage. The closed dot explicitly leaves plain
   `--load`/REPL definer registration to the open
   `habu-register-native-repl-f12807aa`; production B5 authority cannot claim
   that path sealed until the follow-up lands;
4. scalar declarations, audited private `TRUSTED:` mints, public validators,
   and checked arithmetic are assembled across focused `CAD-NUM` constituent
   files, but remain an unsealed non-authority during assembly;
5. the open `habu-seal-cad-num-36dbeec6` dot owns final assembly of every
   constituent and permanently seals `CAD-NUM`; no later file adds owner
   behavior. It depends on the package-seal syntax/capability, unified
   declaration migration, the landed gate-path TVK-RAW seal, and the native/REPL
   registration follow-up; it does not overlap existing owner migrations;
6. consumers migrate at their existing ownership seams only from the complete,
   sealed entry point.

Existing owners remain authoritative:

- `habu-checker-seal-owner-f7de26ff` owns permanent owner-package sealing;
- `habu-seal-owners-migrate-2dda16df` owns migration of its already named
  TARGET, TOOLCHAIN, fusion-region, artifact, evidence, and store owners. It
  does not own CAD-NUM and must not be silently expanded;
- `habu-nominal-storage-raw-a3430ef2` closed at `085cf242` and owns the landed
  checker/verify-source TVK-RAW seal; `habu-register-native-repl-f12807aa` owns
  the still-open plain-load/REPL definer-registration residual;
- `habu-checker-shape-kind-4c6a3f4c` and
  `habu-v2-types-existential-cce4a41a` own value-indexed shape and existential
  evidence;
- `habu-add-bounded-host-b40b048f` owns pointer region, extent, alignment,
  lifetime, and borrow safety.

B5 does not duplicate those capabilities and requires no checker arithmetic
extension for its scalar nominal wrappers. It deliberately avoids placing the
layout-valued error family inside shared `result<a,b>`; choosing that alternate
representation would add an explicit dependency on
`habu-checker-capability-layout-9b8540bd`. If a minimal candidate exposes an
actual checker defect, that defect receives its own negative regression and
checker-owned dot rather than expanding a library migration.

The current storage facts are pinned by `docs/type-families.md` and
`test/layout-buffer.f`: `LAYOUT-BUFFER` already admits arity-zero nominal
families and closed composite layouts. Before `085cf242`, the historical checker
probe `variable V : X ( n -- CAD-KIND:region ) V ! V @ ;` certified. The landed
verifier/gate-path TVK-RAW regressions now reject that laundering shape; only the
separately tracked plain `--load`/REPL registration path remains open. No
`TYPED-VARIABLE` or `TYPED-BUFFER` definer exists in the current source. B5 does
not require those generic convenience definers;
`habu-nominal-storage-typed-c5f44d66` already describes only the residual
convenience-definer work and is not a B5 prerequisite.

The implementation is split into these disjoint core owners:

1. `lib/cad-num-types.f` and `lib/cad-num-types-test.f` own only scalar family
   declarations, `numeric-result<a>`, constructors, `AS-ALLOC-*`, and audited
   mints. Their hard-cutover target is zero-field `STRUCTURE` roles plus a full
   payload-aware `ENUM`; the unified declaration DSL has not landed, so the
   current pre-cutover implementation remains migration-owned. They depend on
   package support and the landed gate-path TVK-RAW seal and do not seal the
   package.
2. `lib/cad-num-arithmetic.f` and `lib/cad-num-arithmetic-test.f` own exactly the
   B5.2 table. They depend on slice 1 and add no roles or overloads.
3. The open `habu-seal-cad-num-36dbeec6` dot owns `lib/cad-num.f` and
   `lib/cad-num-seal-test.f`: final assembly, permanent sealing,
   trusted-inventory integration, and hostile reopen/qualified-publication
   probes only. It depends on slices 1-2, the unified declaration migration, the
   landed gate-path TVK-RAW seal, the native/REPL registration follow-up, and
   package sealing; it is the first production authority.

Consumer work uses the following bounded contracts. A consumer dot copies its
row verbatim before dispatch; files in another row are not edited in the same
workspace.

| Owner | Owned files | Exact API and callers | Focused proof | Dependencies |
|-------|-------------|-----------------------|---------------|--------------|
| Memory | `lib/memory.f`, `lib/memory-test.f` | Create/reopen public package `MEM` and add exactly `MEM:CELLS>BYTES ( cell-count -- R<byte-len> )`, `MEM:64K-BYTES ( item-count -- R<byte-len> )`, `MEM:64K-COUNT-FOR ( byte-len -- R<item-count> )`, `MEM:64K-SPAN-BYTES ( byte-len -- R<byte-len> )`, `MEM:ALLOC-BYTES ( alloc-byte-len -- ptr u8 alloc-byte-len )`, `MEM:ALLOC-CELLS ( alloc-cell-count -- ptr a )`, and `MEM:ALLOC-64K ( -- ptr u8 alloc-byte-len )`. Allocation sinks have no zero-admitting overload. The existing global `MEM-ALLOC-BYTES` is removed or tightened only after the exact caller waves below migrate; `MEM-ALLOC-CELLS` and the multi-64K legacy conveniences remain explicitly out of this B5 wave. | `bin/hb --load lib/memory-test.f`; package-first positive signature, zero scalar calculation succeeds, zero allocation conversion returns `zero`, maximum-safe conversions pass, and first overflow/over-allocation fails before `mmap` | sealed CAD-NUM; B5.2 conversions; existing `mmap` boundary |
| String | `lib/string.f`, `lib/string-test.f` | Add packaged typed `STR:LENGTH`, `STR:OFFSET`, `STR:COUNT`, `STR:FIND-SUB`, `STR:INDEX-OF`, `STR:SPLIT-NEXT`, `STR:BUF-RESET`, `STR:BUF-LEN@`, and `STR:BUF-APPEND`. Only owner-internal calls and `lib/string-test.f` move in this row; the existing `ptr u8 n` global surface remains a named legacy boundary for separate caller dots. Empty strings and empty needles retain zero lengths/offsets. | `bin/hb --load lib/string-test.f`; empty, first/last/not-found substring cases; offset advance overflow; swapped length/offset checker negatives | sealed CAD-NUM; B5.2 byte/index algebra; existing cell-polymorphic `option<a>` carries `CAD-NUM:index` without an option-owner change |
| Vector | `lib/vector.f`, `lib/vector-test.f` | Add packaged `VEC:INIT`, `VEC:CLEAR`, `VEC:LEN@`, `VEC:CAP@`, `VEC:RESIZE`, `VEC:ENSURE`, `VEC:@`, `VEC:!`, `VEC:PUSH`, and `VEC:EACH`. Length/capacity are `item-count`, access/push uses `index`, and only the private one-cell-per-item adapter produces `cell-count` then `alloc-cell-count`. Assigned direct callers are exactly `maki/sched-key.f`, `tools/lint/intern.f`, and `tools/lint/source-lex.f`; the first is one combined sched-key vector/Model-IR caller commit, and the latter pair belong to the combined memory/vector tool caller commit described below. | `bin/hb --load lib/vector-test.f`, then `bin/hb --load maki/sched-key-test.f` and `bin/hb --load tools/lint/text-foundation-test.f`; zero length remains valid, zero capacity allocation rejects, growth overflow and index/count swaps reject | sealed CAD-NUM; packaged MEM API; bounded-host owns relational bounds/generations, not this row |
| Model IR | `maki/model-ir.f`, `maki/model-ir-test.f` | Reopen package `MIR` for `MIR:NODE-COUNT@`, `MIR:SLOT-COUNT@`, `MIR:OPERAND-COUNT@`, and `MIR:MATERIALIZED-COUNT`, all returning `item-count`. Keep `MIR:input-index`, `MIR:ref-pos`, and `MIR:operand-ref`; never replace them with scalar `index`. Operand-count callers are exactly `maki/backward-test.f`, `maki/backward.f`, `maki/cad.f`, `maki/checkpoint.f`, `maki/fusion-plan.f`, `maki/lower-ew.f`, `maki/lower-mm.f`, `maki/lower-move.f`, `maki/lower-red.f`, `maki/mem-plan.f`, `maki/saved.f`, `maki/sched-key.f`, and `maki/traffic.f`. Count accessors are added first; those caller files move in separately owned commits, and the old MAKI-prefixed count accessors are removed only after the listed set is empty. | `bin/hb --load maki/model-ir-test.f`, then `bin/hb --load maki/test.f`; zero-node/zero-operand state, maximum capacities, count-versus-index checker negatives, rollback counts | sealed CAD-NUM; existing MIR nominal handles; no typed-storage-definer dependency and no ownership of MI-* storage migration |
| Shape census | Read-only census of `maki/tensor.f`, `maki/tensor-value.f`, `maki/cad.f`, `maki/executor.f`, `maki/golden-artifact.f`, `maki/gradcheck.f`, `maki/lower-ew.f`, `maki/lower-launch.f`, `maki/lower-mm.f`, `maki/lower-move.f`, `maki/lower-red.f`, `maki/move-view.f`, `maki/plan-ops.f`, `maki/saved.f`, and `maki/traffic.f`; only the census result is added to this plan | Classify every current product as already owned by `MAKI:DIM*`, `MAKI:SHAPE-ELEMS`, or `MAKI:TENSOR-BYTES`, or name an exact residual file/word for a new dot. This row edits no Maki source and adds no CAD-NUM multiplication. | `bin/hb --load maki/tensor-test.f` and `bin/hb --load maki/tensor-value-test.f`; the census records zero/overflow semantics of each owner | read-only after B5.2; any residual becomes a separate owner dot |
| Final integration | `src/habu/habu2.f`, `FILEMAP.md`, `TRUSTED.md`, `MODEL-CAD-V2-PLAN.md`, `STATUS.md`, and `tools/public-signatures-test.f` only | Load the sealed `lib/cad-num.f`, register the three new owner/test files, audit trusted mints and packaged public signatures, and prove the V2 production entry point uses no legacy global numeric cast or allocation boundary. It does not migrate consumers or change arithmetic. | exact public-signature/trust/filemap/status gates, native fixpoint, then the full owning gates on the rebased tree | every dispatched owner/caller dot above green and integrated |

The memory owner contains exactly two private representation projections:

~~~forth
package MEM
private
TRUSTED: ALLOC-BYTES>N
  ( CAD-NUM:alloc-byte-len -- n ) ;
TRUSTED: ALLOC-CELLS>N
  ( CAD-NUM:alloc-cell-count -- n ) ;
;package
~~~

Source definitions and calls use the bare names inside `MEM`; documentation may
render them as `MEM:ALLOC-BYTES>N` and `MEM:ALLOC-CELLS>N` only to identify the
owner. `ALLOC-BYTES>N` appears solely at the `mmap` size operand, and
`ALLOC-CELLS>N` solely before the `cells` primitive used by the cell-allocation
sink. They are never public conversions or general arithmetic adapters. Each
has a `TRUSTED.md` row, refine-lint classification, trust-inventory ownership,
and focused tests proving qualified lookup/export is unavailable and byte/cell
roles cannot swap. Their removal condition is that the corresponding primitive
accepts the nominal allocation role directly.

The string, vector, and Model IR rows mean these exact public effects; no
additional overload is implicit:

~~~forth
STR:LENGTH ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
STR:OFFSET ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-off> )
STR:COUNT ( n -- CAD-NUM:numeric-result<CAD-NUM:item-count> )
STR:FIND-SUB
  ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len
    -- option<CAD-NUM:index> )
STR:INDEX-OF
  ( ptr u8 CAD-NUM:byte-len n -- option<CAD-NUM:index> )
STR:SPLIT-NEXT
  ( ptr u8 CAD-NUM:byte-len n CAD-NUM:index
    -- ptr u8 CAD-NUM:byte-len CAD-NUM:index bool )
STR:BUF-RESET ( ptr CAD-NUM:byte-len -- )
STR:BUF-LEN@ ( ptr CAD-NUM:byte-len -- CAD-NUM:byte-len )
STR:BUF-APPEND
  ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len
    ptr CAD-NUM:byte-len -- )

VEC:INIT ( ptr a CAD-NUM:item-count -- )
VEC:CLEAR ( ptr a -- )
VEC:LEN@ ( ptr a -- CAD-NUM:item-count )
VEC:CAP@ ( ptr a -- CAD-NUM:item-count )
VEC:RESIZE ( ptr a CAD-NUM:item-count -- )
VEC:ENSURE ( ptr a CAD-NUM:item-count -- )
VEC:@ ( ptr a CAD-NUM:index -- a )
VEC:! ( a ptr a CAD-NUM:index -- )
VEC:PUSH ( a ptr a -- CAD-NUM:index )
VEC:EACH
  ( R ptr a [ R CAD-NUM:index a -- R ] -- R )

MIR:NODE-COUNT@ ( -- CAD-NUM:item-count )
MIR:SLOT-COUNT@ ( -- CAD-NUM:item-count )
MIR:OPERAND-COUNT@ ( CAD-KIND:node-id -- CAD-NUM:item-count )
MIR:MATERIALIZED-COUNT ( -- CAD-NUM:item-count )
~~~

The Model IR caller census is frozen per replaced accessor:

- node count: `maki/backward.f`, `maki/cad.f`, `maki/checkpoint.f`,
  `maki/executor.f`, `maki/from-scratch-model-test.f`,
  `maki/fusion-mout-test.f`, `maki/fusion-plan.f`,
  `maki/golden-artifact-test.f`, `maki/golden-artifact.f`, `maki/golden.f`,
  `maki/gradcheck-test.f`, `maki/gradcheck.f`, `maki/lower-ew.f`,
  `maki/lower-golden.f`, `maki/lower-launch.f`, `maki/lower-mm.f`,
  `maki/lower-move.f`, `maki/lower-red.f`, `maki/mem-plan.f`,
  `maki/mlp-bwd-test.f`, `maki/onnx/import-test.f`,
  `maki/onnx/ort-ref-test.f`, `maki/saved.f`, `maki/sched-key.f`, and
  `maki/traffic.f`;
- slot count: `maki/backward.f`, `maki/cad.f`, `maki/executor.f`,
  `maki/from-scratch-model-test.f`, `maki/golden-artifact.f`,
  `maki/gradcheck-test.f`, `maki/gradcheck.f`, `maki/mem-plan.f`,
  `maki/onnx/import-test.f`, and `maki/traffic.f`;
- operand count: the thirteen files named in the Model IR contract row;
- materialized count: `maki/cad.f` and `maki/lower-model-test.f`.

Each owning Maki caller commit migrates all count accessors in its file. The old
MAKI-prefixed accessors are removed only after a fresh fixed-string `rg` census
for all four names is empty outside `maki/model-ir.f` and its focused test.

`VEC:INIT`, `VEC:RESIZE`, and `VEC:ENSURE` accept zero-admitting logical counts
because empty state and no-op ensure are domain operations. Immediately before
allocation, the private vector representation adapter converts the positive
item count to `cell-count`, checks cell scaling, and requires
`AS-ALLOC-CELL-COUNT`; zero or overflow maps explicitly to the existing named
vector capacity result/throw at that owner boundary. No zero-admitting value
reaches `MEM:ALLOC-CELLS`.

The frozen direct-caller set for the one legacy memory sink changed by this B5
wave, `MEM-ALLOC-BYTES`, is split into disjoint caller dots:

- library callers: `lib/codesign.f`, `lib/content-key.f`, `lib/object-cache.f`,
  `lib/process-argv.f`, `lib/process-env.f`, and `lib/source.f`;
- Maki callers: `maki/eval-repair-mech.f`, `maki/eval-transcript.f`, and
  `maki/onnx/import.f`;
- test callers: `test/gate-build-common.f`, `test/gate-engine-lib.f`,
  `test/gate-pool.f`, `test/gate-stdlib-inline-lib.f`,
  `test/run-result-cache-test.f`, and `test/seal-absence.f`;
- tool callers: `tools/aot-call-report-test.f`, `tools/build-fixpoint-test.f`,
  `tools/build-fixpoint.f`, `tools/check-core.f`, `tools/check-test-lib.f`,
  `tools/codegen-role-test.f`, `tools/codegen-role.f`,
  `tools/diag-origin-core.f`, `tools/diagnose-hb-test.f`,
  `tools/examples-test.f`, `tools/hb-build-lib.f`,
  `tools/json-only-test-lib.f`, `tools/json-only.f`, `tools/lint/intern.f`,
  `tools/lint/text-foundation-test.f`, `tools/refine-lint-core.f`,
  `tools/repair-packet-core.f`, `tools/repair-packet-test.f`,
  `tools/repair-schema-doc-test.f`, `tools/signature-lint-core.f`,
  `tools/stdlib-manifest-test.f`, `tools/trust-lint.f`, and
  `tools/trusted-inventory.f`.

Each caller dot owns its listed files plus their already-associated focused
tests, converts a raw size through `CAD-NUM:BYTE-LEN` and
`CAD-NUM:AS-ALLOC-BYTE-LEN`, and calls the real packaged allocator. The memory
owner changes the legacy signature and activates `BAD-ZEROABLE-ALLOC` only after
all four caller dots are green; a fresh `rg` census must equal this list before
the change.

Overlap is resolved by ownership, not concurrent edits: the tool caller dot also
migrates `VEC` use in `tools/lint/intern.f` and `tools/lint/source-lex.f` after
both packaged APIs land, while the sched-key caller dot migrates both its `VEC`
and Model IR count use. No other caller dot may edit those three files.

#### B5.5a Legacy Global String Caller Census (habu-census-legacy-str-b84390fe)

B5.5 landed the typed `STR:` surface inside `lib/string.f` but deliberately left
the global `ptr u8 n` raw string words as a named legacy boundary for separate
caller dots. This is the frozen, exhaustive census of every live external caller
of those raw words and the disjoint migration leaves that discharge them. It
edits no consumer source; it is a census + dot-decomposition only.

**Census targets (raw global words the typed `STR:` words wrap, read from
`lib/string.f`), plus their length-bearing BUF helpers:** `STR-LEN`, `STR-OFF`,
`STR-COUNT`, `FIND-SUB`, `INDEX-OF`, `SPLIT-NEXT`, `BUF-RESET`, `BUF-LEN@`,
`BUF-APPEND`, `BUF-APPEND-LEN`, `BUF-APPEND-C`, `BUF-CHECK-LEN`.

**Explicitly out of scope:** the global single-buffer string builder `SB-RESET`,
`SB-APPEND`, `SB-APPEND-C`, `SB-APPEND-LEN`, `SB-CHECK-ROOM`, `SB-CHECK-LEN-ROOM`,
`SB$` (100+ files, ~2600 sites). These are a separate global surface with **no**
typed `STR:` target; the B5.5 row wrapped none of them, so they are not
migratable in this wave and belong to a future STR string-builder owner dot, not
a caller census. The Gforth-world `bootstrap/src/render.fs` defines its own
unrelated `BUF-RESET ( -- )`; `.fs`/`bootstrap/` are excluded.

**Search method (reproducible; a fresh census must equal these lists):** for each
target word `W`, whitespace-delimited raw-call match
`rg -n "(^|[[:space:]])W([[:space:]]|$)" -g '*.f'`, excluding `lib/string.f`,
`lib/string-test.f`, `bootstrap/**`, `.dots/**`, `.jj/**`. The pattern isolates
raw calls: a `:`-prefixed `STR:W` typed call and a `-`-prefixed `RAW-W` alias are
not matched. A broader boundary sweep `(^|[^A-Za-z0-9-])W([^A-Za-z0-9-]|$)`
surfaced no additional call site (only comment/`require`/alias lines in the owner
file), confirming exhaustiveness. `require ... \ ... consumer` comment lines are
not calls.

**Per-word typed target and zero/not-found semantics** (positive behavior is
byte-identical to the raw word):

| Raw global word | Typed target | Zero / not-found semantics |
|-----------------|--------------|----------------------------|
| `STR-LEN` | `STR:LENGTH ( n -- CAD-NUM:byte-len )` | 0 → byte-len 0; negative → `E-STR-BOUNDS`. **No external callers.** |
| `STR-OFF` | `STR:OFFSET ( n -- CAD-NUM:byte-off )` | 0 → byte-off 0; negative → `E-STR-BOUNDS`. **No external callers.** |
| `STR-COUNT` | `STR:COUNT ( n -- CAD-NUM:item-count )` | 0 → item-count 0; negative → `E-STR-BOUNDS`. **No external callers.** |
| `FIND-SUB` | `STR:FIND-SUB ( ptr u8 byte-len ptr u8 byte-len -- option<CAD-NUM:index> )` | empty needle → `SOME` index 0; needle longer than haystack or absent → `NONE`. |
| `INDEX-OF` | `STR:INDEX-OF ( ptr u8 byte-len n -- option<CAD-NUM:index> )` | byte found → `SOME` index; empty/absent → `NONE`. |
| `SPLIT-NEXT` | `STR:SPLIT-NEXT ( ptr u8 byte-len n byte-off -- ptr u8 byte-len byte-off bool )` | `start<0`/`start>len` → `(a, len 0, start, false)`; adds one-past-end offset-advance overflow → `E-STR-BOUNDS` (raw silently wrapped). |
| `BUF-RESET` | `STR:BUF-RESET ( ptr len -- )` | sets length cell to 0. |
| `BUF-LEN@` | `STR:BUF-LEN@ ( ptr len -- CAD-NUM:byte-len )` | empty → byte-len 0. |
| `BUF-APPEND` | `STR:BUF-APPEND ( ptr u8 byte-len ptr u8 byte-len ptr len -- )` | `E-STR-CAPACITY` on overflow; negative → `E-STR-BOUNDS`. |
| `BUF-APPEND-LEN` | `STR:BUF-APPEND` (drop the caller's `>LEN` pre-conversion; pass raw counts — `BUF-APPEND(n) ≡ BUF-APPEND-LEN(>LEN n)`) | as `BUF-APPEND`. |
| `BUF-APPEND-C` | **none in the current B5.5 surface** → owner-extension leaf **D0** adds `STR:BUF-APPEND-C ( n ptr u8 byte-len ptr len -- )` | byte-range `c<0`/`c>255` → `E-STR-BOUNDS`; overflow → `E-STR-CAPACITY`. |
| `BUF-CHECK-LEN` | owner-internal helper | **No external callers.** |

**Census result: 170 live external calls across 41 files.** Per word:
`FIND-SUB` 37 / `INDEX-OF` 23 / `SPLIT-NEXT` 13 / `BUF-RESET` 15 / `BUF-LEN@` 8 /
`BUF-APPEND` 35 / `BUF-APPEND-LEN` 1 / `BUF-APPEND-C` 38.
`STR-LEN`/`STR-OFF`/`STR-COUNT`/`BUF-CHECK-LEN` have zero external callers, so
`STR:LENGTH`/`STR:OFFSET`/`STR:COUNT` need no caller migration.

**Owner-surface gap (census finding).** The B5.5 String row enumerated nine typed
words and omitted a typed equivalent for the raw single-byte appender
`BUF-APPEND-C`, on which 5 files depend. Leaf **D0** adds `STR:BUF-APPEND-C` to
the owner (`lib/string.f` + `lib/string-test.f`) before those callers migrate.
`BUF-APPEND-LEN` (one caller, `lib/content-key.f`) needs no owner extension: it is
`BUF-APPEND` with the length pre-converted, so it migrates to `STR:BUF-APPEND`.

**Disjoint migration leaves** (each file owned by exactly one leaf; each live call
owned exactly once). Ownership is per file — a leaf migrates every in-scope raw
STR call in each file it owns. `(C)` marks a file that also uses `BUF-APPEND-C`
and therefore blocks on D0.

- **D0 — owner extension** `Add STR:BUF-APPEND-C typed byte append`. Owns
  `lib/string.f`, `lib/string-test.f`. Blocks D3 and D4.
- **D1 — library callers** `Migrate library string callers to STR:`
  (7 files): `examples/string-regex.f` (INDEX-OF), `lib/json-read.f` (INDEX-OF),
  `lib/float.f` (INDEX-OF), `lib/process-env.f` (INDEX-OF, SPLIT-NEXT),
  `lib/object.f` (SPLIT-NEXT), `lib/ptx/ad.f` (SPLIT-NEXT),
  `lib/content-key.f` (BUF-APPEND-LEN → STR:BUF-APPEND). No D0 dependency.
- **D2 — Maki callers** `Migrate Maki string callers to STR:` (13 files):
  `maki/store.f` (INDEX-OF), `maki/store-rehydrate.f` (INDEX-OF, SPLIT-NEXT),
  `maki/competitive-store.f` (INDEX-OF), `maki/cad.f` (INDEX-OF),
  `maki/eval-transcript.f` (INDEX-OF, SPLIT-NEXT),
  `maki/golden-artifact.f` (INDEX-OF, SPLIT-NEXT),
  `maki/eval-repair-loop.f` (SPLIT-NEXT), `maki/ablate-ptx.f` (FIND-SUB),
  `maki/lower-red-test.f`, `maki/lower-mm-test.f`, `maki/lower-ew-test.f`,
  `maki/lower-mv-test.f`, `maki/onnx/deploy-test.f` (all FIND-SUB). No BUF use; no
  D0 dependency.
- **D3 — test callers** `Migrate test string callers to STR:` (7 files):
  `test/boot-pin-test.f` (FIND-SUB), `test/gate-engine-lib.f` (FIND-SUB),
  `test/owner-wid-doctor.f` (FIND-SUB),
  `test/gate-pool-test.f` (FIND-SUB, INDEX-OF),
  `test/seal-absence.f` (FIND-SUB, SPLIT-NEXT),
  `test/run-lib.f` (SPLIT-NEXT, BUF-RESET, BUF-APPEND, BUF-APPEND-C `(C)`),
  `test/run-rerun-failed-test.f` (BUF-RESET). Blocks on D0.
- **D4 — tool callers** `Migrate tool string callers to STR:` (14 files):
  `tools/codegen-role.f` (FIND-SUB, INDEX-OF, BUF-APPEND, BUF-APPEND-C `(C)`),
  `tools/codegen-role-test.f` (FIND-SUB, BUF-APPEND),
  `tools/hb-build-lib.f` (INDEX-OF),
  `tools/bootstrap-codegen-test.f` (FIND-SUB), `tools/build-fixpoint.f` (FIND-SUB),
  `tools/build-fixpoint-test.f` (FIND-SUB), `tools/ptx/saxpy-test.f` (FIND-SUB),
  `tools/ptx/perf-registry.f` (SPLIT-NEXT),
  `tools/lint/text.f` (BUF-RESET, BUF-APPEND),
  `tools/lint/text-foundation-test.f` (BUF-RESET, BUF-LEN@, BUF-APPEND, BUF-APPEND-C `(C)`),
  `tools/public-signatures-core.f` (BUF-APPEND),
  `tools/stale-status-lint-core.f` (BUF-APPEND),
  `tools/suite-coverage-lint-test.f` (BUF-RESET, BUF-APPEND, BUF-APPEND-C `(C)`),
  `tools/typed-local-diff-lint-test.f` (BUF-RESET, BUF-LEN@, BUF-APPEND, BUF-APPEND-C `(C)`).
  Blocks on D0.

All four caller leaves depend on the already-landed `STR:` owner surface. Leaf
lanes are disjoint by path prefix (`lib/`+`examples/` / `maki/` / `test/` /
`tools/`), so no file is owned twice and no leaf edits `lib/string.f` except D0.

**Overlap with the already-landed numeric caller waves** (file-touch history
only — those waves merged, so this is sequential, not a concurrent-edit hazard;
dispatch STR leaves on a base with the MEM/VEC/Model-IR waves merged): the
following census files were also touched by a landed numeric wave —
`lib/content-key.f`, `lib/process-env.f` (MEM library); `maki/eval-transcript.f`
(MEM Maki); `maki/cad.f`, `maki/golden-artifact.f` (Model IR count);
`test/gate-engine-lib.f`, `test/seal-absence.f` (MEM test);
`tools/build-fixpoint.f`, `tools/build-fixpoint-test.f`, `tools/codegen-role.f`,
`tools/codegen-role-test.f`, `tools/hb-build-lib.f`,
`tools/lint/text-foundation-test.f` (MEM/VEC tool). The `maki/lower-*-test.f`
files are distinct from the non-test `maki/lower-*.f` files in the Model IR wave.

**Ready-to-mint leaf dots** (orchestrator mints; census records verbatim):

~~~
dot add "Add STR:BUF-APPEND-C typed byte append" -d "Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census. The typed STR: surface wraps BUF-RESET/BUF-LEN@/BUF-APPEND but omits a typed equivalent for the raw single-byte appender BUF-APPEND-C ( n ptr u8 n ptr len -- ), which 5 files depend on. Add packaged STR:BUF-APPEND-C ( n ptr u8 CAD-NUM:byte-len ptr len -- ) to lib/string.f mirroring STR:BUF-APPEND: bind RAW-BUF-APPEND-C to the global before the package word shadows it, take cap as CAD-NUM:byte-len via BYTE-LEN>N, keep the same c<0/c>255 E-STR-BOUNDS and E-STR-CAPACITY throws. Acceptance: STR:BUF-APPEND-C lands with a T{ }T test covering append, capacity overflow throw, and byte-range reject; lib/string-test.f green. Files: lib/string.f, lib/string-test.f. Verify: bin/hb --load lib/string-test.f. Depends: landed STR owner. Ownership: lib/string.f, lib/string-test.f. Claim: unassigned."

dot add "Migrate library string callers to STR:" -d "Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, library lane. Migrate every raw STR call in these files to the typed STR: surface: examples/string-regex.f (INDEX-OF), lib/json-read.f (INDEX-OF), lib/float.f (INDEX-OF), lib/process-env.f (INDEX-OF, SPLIT-NEXT), lib/object.f (SPLIT-NEXT), lib/ptx/ad.f (SPLIT-NEXT), lib/content-key.f (BUF-APPEND-LEN -> STR:BUF-APPEND, dropping the >LEN pre-conversion). INDEX-OF/FIND-SUB return option<CAD-NUM:index>; SPLIT-NEXT gains the one-past-end offset-advance overflow guard; behavior otherwise byte-identical. Acceptance: no raw STR-LEN/STR-OFF/STR-COUNT/FIND-SUB/INDEX-OF/SPLIT-NEXT/BUF-* call remains in these files (fresh rg census empty); each file's focused test green. Files: examples/string-regex.f, lib/json-read.f, lib/float.f, lib/process-env.f, lib/object.f, lib/ptx/ad.f, lib/content-key.f, plus their focused tests. Verify: bin/hb --load each owned file's focused test; library gate slice. Depends: landed STR owner. Ownership: the 7 listed library files. Claim: unassigned."

dot add "Migrate Maki string callers to STR:" -d "Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, Maki lane. Migrate every raw STR call to the typed STR: surface in: maki/store.f (INDEX-OF), maki/store-rehydrate.f (INDEX-OF, SPLIT-NEXT), maki/competitive-store.f (INDEX-OF), maki/cad.f (INDEX-OF), maki/eval-transcript.f (INDEX-OF, SPLIT-NEXT), maki/golden-artifact.f (INDEX-OF, SPLIT-NEXT), maki/eval-repair-loop.f (SPLIT-NEXT), maki/ablate-ptx.f (FIND-SUB), maki/lower-red-test.f, maki/lower-mm-test.f, maki/lower-ew-test.f, maki/lower-mv-test.f, maki/onnx/deploy-test.f (FIND-SUB). No BUF use, no D0 dependency. INDEX-OF/FIND-SUB return option<CAD-NUM:index>; SPLIT-NEXT gains offset-advance overflow guard; behavior byte-identical. Overlap note: maki/cad.f and maki/golden-artifact.f were touched by the landed Model IR count wave (sequential). Acceptance: fresh rg census empty in these files; maki/test.f and each focused test green. Files: the 13 listed Maki files plus their focused tests. Verify: bin/hb --load maki/test.f and the owned focused tests. Depends: landed STR owner. Ownership: the 13 listed Maki files. Claim: unassigned."

dot add "Migrate test string callers to STR:" -d "Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, test lane. Migrate every raw STR call to the typed STR: surface in: test/boot-pin-test.f (FIND-SUB), test/gate-engine-lib.f (FIND-SUB), test/owner-wid-doctor.f (FIND-SUB), test/gate-pool-test.f (FIND-SUB, INDEX-OF), test/seal-absence.f (FIND-SUB, SPLIT-NEXT), test/run-lib.f (SPLIT-NEXT, BUF-RESET, BUF-APPEND, BUF-APPEND-C), test/run-rerun-failed-test.f (BUF-RESET). test/run-lib.f uses BUF-APPEND-C so this leaf blocks on the D0 STR:BUF-APPEND-C owner extension. Overlap note: test/gate-engine-lib.f and test/seal-absence.f were touched by the landed MEM test wave (sequential). Acceptance: fresh rg census empty in these files; each focused test/gate slice green. Files: the 7 listed test files plus their focused tests. Verify: bin/hb --load the owned focused tests / gate slices. Depends: landed STR owner; D0 (STR:BUF-APPEND-C). Ownership: the 7 listed test files. Claim: unassigned."

dot add "Migrate tool string callers to STR:" -d "Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, tool lane. Migrate every raw STR call to the typed STR: surface in: tools/codegen-role.f (FIND-SUB, INDEX-OF, BUF-APPEND, BUF-APPEND-C), tools/codegen-role-test.f (FIND-SUB, BUF-APPEND), tools/hb-build-lib.f (INDEX-OF), tools/bootstrap-codegen-test.f (FIND-SUB), tools/build-fixpoint.f (FIND-SUB), tools/build-fixpoint-test.f (FIND-SUB), tools/ptx/saxpy-test.f (FIND-SUB), tools/ptx/perf-registry.f (SPLIT-NEXT), tools/lint/text.f (BUF-RESET, BUF-APPEND), tools/lint/text-foundation-test.f (BUF-RESET, BUF-LEN@, BUF-APPEND, BUF-APPEND-C), tools/public-signatures-core.f (BUF-APPEND), tools/stale-status-lint-core.f (BUF-APPEND), tools/suite-coverage-lint-test.f (BUF-RESET, BUF-APPEND, BUF-APPEND-C), tools/typed-local-diff-lint-test.f (BUF-RESET, BUF-LEN@, BUF-APPEND, BUF-APPEND-C). Four files use BUF-APPEND-C so this leaf blocks on the D0 STR:BUF-APPEND-C owner extension. Overlap note: build-fixpoint(.f/-test.f), codegen-role(.f/-test.f), hb-build-lib.f, lint/text-foundation-test.f were touched by the landed MEM/VEC tool wave (sequential). Acceptance: fresh rg census empty in these files; each focused test/lint slice green. Files: the 14 listed tool files plus their focused tests. Verify: bin/hb --load the owned focused tests / lint slices. Depends: landed STR owner; D0 (STR:BUF-APPEND-C). Ownership: the 14 listed tool files. Claim: unassigned."
~~~

**`habu-integrate-sealed-cad-ba510e2e` amendment.** Before it closes, its
`blocks:` list must add all five minted leaf ids (the orchestrator substitutes
the minted ids for these titles): D0 `Add STR:BUF-APPEND-C typed byte append`,
D1 `Migrate library string callers to STR:`, D2 `Migrate Maki string callers to
STR:`, D3 `Migrate test string callers to STR:`, D4 `Migrate tool string callers
to STR:`. Additionally D3 and D4 must list D0 in their own `blocks:`. The census
dot `habu-census-legacy-str-b84390fe` closes once these five leaves are minted and
recorded on ba510e2e.

The open `habu-v2-types-refined-519fd2d1` design dot now states this scalar-only
algebra and allocation contract and awaits its named censuses, plan-state
reconciliation, and TRUSTED retirement audit. The active
`habu-nominal-storage-typed-c5f44d66` dot already records the landed
`LAYOUT-BUFFER` capability and owns residual generic convenience definers only;
CAD-NUM does not depend on it. This plan reconciliation changes no tracker
entry.

Each implementation slice owns disjoint source and focused tests, runs the
exact native load path for those files, and commits one verified change. Shared
manifest/file-map edits are a final integration slice so parallel workers do
not overlap.

Benefit:

Object encoding, bufferization, shape arithmetic, and file parsing reject more
same-cell semantic-role bugs before memory access without committing V2 to
unrestricted dependent arithmetic.

## 8. Awesome-To-Have Type-System Improvements

### A1. Proof-Producing Equality Saturation

A bounded e-graph stores proof edges and extracts a graph plus a compact
equivalence witness.

Value:

Fusion and algebraic exploration become both broad and replayably justified.

Not required because deterministic rewrite passes can establish V2 first.

### A2. Solver-Generated Schedule Legality Evidence

The constraint solver produces evidence such as:

~~~text
vectorizable<layout,align,width>
fits-smem<plan,target>
occupancy-at-least<plan,target,threshold>
mma-compatible<shape,dtype,target>
~~~

Value:

Lowering signatures can require exact legality evidence instead of rechecking
integer fields.

Not required because Plan IR verification can initially produce ordinary typed
diagnostic results.

### A3. Approximation Proof Domains

Model exact, ULP-bounded, relative-error, and empirically licensed equivalence
as distinct proof domains.

Value:

Approximate GELU, TF32, reduced precision, and recomputation choices become
first-class rewrite evidence rather than policy flags.

Device golden evidence remains required regardless.

### A4. Staged Generics Over Targets And Dialects

Permit one checked lowering template to quantify over a bounded target
capability family.

Value:

Backends can share verified structure without raw conditional ladders.

Not required until a second serious target/backend exists.

### A5. Proof-Carrying External Imports

Importers produce schema, shape, provenance, and semantic mapping evidence that
can be replayed independently of parsing.

Value:

ONNX and future formats become auditable frontends with exact mapping proofs.

Not required for the first V2 import vertical slice, which can use canonical IR
round-trips and external goldens.

## R7 Design Addendum: Stage, Evidence, And Promotion Families

Design deliverable of dot `habu-v2-types-artifact-6ee556f8` for the
"### R7. Typestate And Evidence Families" section above (cited as
MODEL-CAD-V2-PLAN.md:335-368 at claim time; anchor by heading, not line).
This addendum is append-only; it changes no other section.

### Census: proven V1 mixing/bypass paths

How report tags flow today: gates write verdicts through the public
`REPORT:GATE! ( report ptr u8 n n n -- report )` (maki/report.f:592-599) into a
singleton typed verdict column; `PROMOTE` gates on `PROMOTE-OK?`
(maki/cad.f:1026-1030) reading `REPORT:GATE-TAG@`; on pass, `PROMOTE-EVIDENCE`
(maki/cad.f:1046-1054) re-reads the four tags as raw `n` and writes evidence
and replay rows through `EVID-PUT-G` (maki/store.f:345-354) and
`SK-PUT-DURABLE`, keyed by a raw key string.

Probe census, all green through
`HB_TMP=/private/tmp/claude-501/fable-arttype-hbtmp bin/hb --load
promote-census-probe.f` on 2026-07-13 (fixture in the worker scratch; every
probe is a CHECK!-certified program — the census point is that the checker
accepts these today):

1. Verdict/gate-id slot mixing. The two trailing `REPORT:GATE!` arguments are
   both raw `n`, so the swapped call `s" swapped" G-CERTIFY V-NOTRUN
   REPORT:GATE!` certifies (`CHECK-CANDIDATE!` verdict -1) and at runtime
   records `V-PASS` on `G-GRADCHECK` (`V-NOTRUN` = 2 = `G-GRADCHECK`,
   `G-CERTIFY` = 0 = `V-PASS`) while the intended CERTIFY slot silently keeps
   its `REPORT:NEW` default. The internal `verdict` ENUM column
   (maki/report.f:162) protects the storage cell, not the accessor arguments.
2. Forgeable promotion readout. The exact reads `PROMOTE-OK?` performs are
   public-surface reproducible and satisfiable by two `REPORT:GATE!` writes
   (`REPORT:NEW` defaults already satisfy the gradcheck/profile reads)
   with no gate machinery behind them. Promotion-worthiness is a runtime tag
   readout, not proof that any gate ran.
3. Store bypass and wrong-artifact evidence. `EVID-PUT` (maki/store.f:355) and
   `SCHED-PUT` (maki/store.f:293) are public `MAKI` words over raw key strings
   plus raw verdict ints. Any call path can plant
   `certify=pass|golden=pass|gradcheck=pass|profile=pass` under any artifact
   key, and can plant a replay selection PROMOTE never recorded; `TILE-REPLAY`
   (maki/cad.f:943-946) then replays it as "the selection PROMOTE recorded".
   Nothing ties a verdict to the artifact it was measured on.

Additionally, device-golden provenance is ambient process state:
`GOLDEN-DEV-FLAG`/`GOLDEN-PREC-V` (maki/golden.f:105-111) are globals set by
one leg and read back later by `PROMOTE-EVIDENCE`, not fields of an evidence
value.

### Reused machinery (no parallel system)

Everything below describes the landed pre-cutover machinery the unified DSL
must preserve; the cited spellings are migration sites, not V2 syntax:

- planned zero-field `STRUCTURE` package kinds: historical pre-cutover evidence
  uses the `TYPEFAMILY` spelling and `CHECKER-DEFPRODUCT`/`TDECL-FAMILY`
  internals in src/core/sumtype.f; the migration site is
  maki/cad-kinds.f:7-33 (`CAD-KIND`), R3 above.
- planned `STRUCTURE`/`ENUM`/`MATCH` declarations: historical pre-cutover
  evidence uses `SUMTYPE`/`PRODUCT`/`ENUM` in src/core/sumtype.f:1-8 and
  docs/type-families.md §9, §13-14; migration sites include
  maki/model-ir.f:70-74 (`PRODUCT mark 0` inside `package MIR`, ctor
  `MIR-MARK:MAKE` used at maki/model-ir.f:295), maki/report.f:53-96
  (`verdict`/`roofline`/`costatus` enums), lib/adt/result.f:35
  (`SUMTYPE result 2`), and lib/adt/option.f:26.
- Cell-family product fields: `TDECL-PAY-FAM?` (src/core/sumtype.f) admits
  TK-CELL and layout families at arity 0. Probe-verified: `FIELD art
  CAD-KIND:artifact-id` compiles; `MAKE` is born-typed; a same-width foreign
  id family (`CAD-KIND:evidence-id`) and raw `n` both reject (verdict 0).
- Products as variant payloads and sums as product fields: `TFAM-LAYOUT?`
  (src/core/type-family.f:221-222) includes TK-PRODUCT. Probe-verified: an
  evidence-slot sum (`VARIANT got certified ;VARIANT VARIANT absent ;VARIANT`),
  a bundle product holding that sum, and `UNMAKE`+`MATCH` elimination all
  certify; a payload product where the slot sum is expected rejects.
- Sealed construction: docs/type-families.md §12 — public families construct
  only through the generated ctor package (derived escaped spelling: package
  `MODEL` + family `elab` publishes `MODEL-ELAB:MAKE`; a hyphenated segment
  escapes, e.g. `PX-PROBE`+`pxevid` derives `PX--PROBE-PXEVID`). Private sums
  construct only via owner-scoped `construct family variant` (cross-package or
  qualified operands never resolve); private products have no construction
  surface at all (§9.4, fail-closed).
- Private audited raw mints: `TRUSTED: RAW>TARGET-ID` kept private in
  maki/target/target.f:54-55 — the R3 refinement pattern for proof tokens.
- Derived equality for policy fields: `DERIVE eq` (docs §9.3.1;
  src/core/sumtype.f:429).
- Candidate verdict convention for fixtures: `CHECK-CANDIDATE!` returns
  -1 certified / 0 rejected / 1 uncheckable (src/core/checker.f:7551); an
  unresolvable word (e.g. a sealed private mint referenced cross-package)
  reads as 1, a type mismatch as 0. Fixtures assert the exact verdict and pair
  every negative with a resolving positive control.

### Stage families

One zero-field `STRUCTURE` per (IR level, stage), declared in the stage owner's
package — not one `stage` enum value: distinct stages must be distinct
TYPES so wrong order is a signature mismatch. The existing generic
`CAD-KIND:stage` kind (maki/cad-kinds.f:31) remains for rendering/diagnostic
data and is never an ordering authority. Parameterized nominal application
(`plan<complete>`) is not yet in the declaration grammar (docs §9.4), so the
plan's `model<elaborated>` notation is realized as flat per-stage families; a
later parametric-application capability can migrate them mechanically.

~~~text
package MODEL  (maki/ir/model)    decl-proof, elab-proof;  decl, elab
package TIR    (maki/ir/tensor)   solved-proof;            drafted, solved
package RIR    (maki/ir/region)   legal-proof;             drafted, legal
package PLAN   (maki/ir/plan)     complete-proof;          draft, complete
package KIR    (maki/ir/kernel)   verified-proof;          drafted, verified
package CAND   (maki/backend/ptx) emitted-proof;           emitted
package ART    (maki/evidence)    built-proof, promoted-proof; built, promoted
~~~

Each staged value is a `STRUCTURE` pairing the persistent identity with a
package-sealed proof token:

~~~forth
package PLAN
public
STRUCTURE complete-proof 0 ;STRUCTURE

STRUCTURE complete 0
   FIELD plan CAD-KIND:plan-id
   FIELD rev  CAD-KIND:rev-id
   FIELD tok  complete-proof
;STRUCTURE
~~~

Sealing decision: the generated `PLAN-COMPLETE:MAKE` is public
(closed-but-callable ctor package), so the seal is the proof-token FIELD. The
only producer of a `complete-proof` cell is a private
`TRUSTED: MINT-COMPLETE-PROOF ( -- complete-proof )` inside the owning
package, invoked by the transition word after its independent verifier
succeeds — the maki/target/target.f:54 pattern, with a TRUSTED.md row and a
focused test per mint. Outside code cannot produce the token, so it cannot
forge the staged product even though `MAKE` resolves. Private products were
considered and rejected: they have no construction surface at all (docs §9.4),
including for the owner. Staged values are NOT linear: revisions are immutable
(§9.3), so re-consuming a solved handle is legal; linearity stays reserved for
R6 resource ownership.

### Artifact-indexed evidence families

One `STRUCTURE` per evidence class in package `EVID` (maki/evidence/), each
carrying the exact artifact id plus a class-private proof token; the class is
the FAMILY, not a tag value:

~~~forth
package EVID
public
STRUCTURE certify-proof 0 ;STRUCTURE
STRUCTURE golden-proof 0 ;STRUCTURE
STRUCTURE gradcheck-proof 0 ;STRUCTURE
STRUCTURE profile-proof 0 ;STRUCTURE

ENUM golden-leg 0 DERIVE eq
   VARIANT host ;VARIANT
   VARIANT external ;VARIANT
   VARIANT device ;VARIANT
;ENUM
ENUM prec-class 0 DERIVE eq
   VARIANT f32 ;VARIANT
   VARIANT tf32 ;VARIANT
;ENUM

STRUCTURE certified 0
   FIELD art CAD-KIND:artifact-id
   FIELD tok certify-proof
;STRUCTURE

STRUCTURE golden 0
   FIELD art  CAD-KIND:artifact-id
   FIELD leg  golden-leg
   FIELD prec prec-class
   FIELD tok  golden-proof
;STRUCTURE

STRUCTURE gradchecked 0
   FIELD art CAD-KIND:artifact-id
   FIELD tok gradcheck-proof
;STRUCTURE

STRUCTURE profiled 0
   FIELD art CAD-KIND:artifact-id
   FIELD us  n
   FIELD tok profile-proof
;STRUCTURE
~~~

Consequences: probe 1's verdict/gate-id swap becomes unrepresentable (there is
no `(tag, gate-id)` raw pair anywhere); the ambient
`GOLDEN-DEV-FLAG`/`GOLDEN-PREC-V` globals become the `leg`/`prec` fields of
the golden value. A failing gate produces `err diag-set` through
`result<_,_>` (lib/adt/result.f:35) — there is no pass-shaped failure object;
"not-run" is the absence of the evidence value under the policy check. The V1
`verdict` ENUM survives only at the report render boundary.

Evidence presence is typed with per-class slot sums plus one bundle product
(probe-verified shape; parametric `option<...>` fields are not admissible
product fields at arity 0, so the slots are flat sums):

~~~forth
ENUM certify-slot 0
   VARIANT got FIELD evidence certified ;VARIANT
   VARIANT absent ;VARIANT
;ENUM
\ golden-slot / gradcheck-slot / profile-slot identically

STRUCTURE bundle 0
   FIELD cert  certify-slot
   FIELD gold  golden-slot
   FIELD grad  gradcheck-slot
   FIELD prof  profile-slot
;STRUCTURE
~~~

### Promotion-policy products

Policies are ordinary public data (inputs, not proofs); the grant is sealed.
Per §16, a different policy is a different typed object and hash: `pol` is the
policy's canonical schema identity and participates in the release key.

~~~forth
package POLICY
public
STRUCTURE grant-proof 0 ;STRUCTURE

ENUM req-class 0 DERIVE eq
   VARIANT required-blocking ;VARIANT       \ evidence must exist and carry this artifact
   VARIANT required-when-supported ;VARIANT \ required unless a typed unsupported reason exists
   VARIANT required-recorded ;VARIANT       \ must exist; content never blocks (profile)
   VARIANT informational ;VARIANT           \ may be absent
;ENUM

STRUCTURE promote-policy 0
   FIELD cert req-class
   FIELD gold req-class
   FIELD grad req-class
   FIELD prof req-class
   FIELD pol  CAD-KIND:schema-id
;STRUCTURE

STRUCTURE granted 0
   FIELD art CAD-KIND:artifact-id
   FIELD pol CAD-KIND:schema-id
   FIELD tok grant-proof
;STRUCTURE
~~~

The V1 inference gate set (maki/cad.f:1019-1030) is exactly the default
policy `cert=required-blocking gold=required-blocking
grad=required-when-supported prof=required-recorded`.

`POLICY:CHECK` is the single sealed site of the value-level artifact binding:
for each slot the policy marks required, the evidence value must be `got` and
its `art` field must equal the artifact under promotion (typed diagnostic
`E-EVID-ARTIFACT` on mismatch). This boundary is explicit and honest (§24
Proof Theater): the type system guarantees provenance (proof tokens only
exist downstream of a real gate run), class separation (distinct families),
and stage order; same-family artifact-id equality is a value fact checked
once, inside the only word that can mint `POLICY:granted`.

### Transition words

Exact conceptual signatures (each wraps §12 `PASS:RUN`/`PASS:VERIFY` plus the
owner's independent verifier; every success mints its proof token privately):

~~~forth
MODEL:ELABORATE ( MODEL:decl -- result<MODEL:elab,diag-set> )
TIR:SOLVE       ( MODEL:elab -- result<TIR:solved,diag-set> )
RIR:LEGALIZE    ( TIR:solved -- result<RIR:legal,diag-set> )
PLAN:FINISH     ( RIR:legal PLAN:draft -- result<PLAN:complete,diag-set> )
KIR:VERIFY      ( PLAN:complete KIR:drafted -- result<KIR:verified,diag-set> )
CAND:EMIT       ( KIR:verified CAD-KIND:target-id -- result<CAND:emitted,diag-set> )
ART:BUILD       ( CAND:emitted CAD-KIND:toolchain-id -- result<ART:built,diag-set> )
EVID:CERTIFY    ( ART:built -- result<EVID:certified,diag-set> )
EVID:GOLDEN     ( ART:built golden-ctx -- result<EVID:golden,diag-set> )
EVID:GRADCHECK  ( ART:built -- result<EVID:gradchecked,diag-set> )
EVID:PROFILE    ( ART:built device-ctx -- result<EVID:profiled,diag-set> )
POLICY:CHECK    ( ART:built EVID:bundle POLICY:promote-policy
                  -- result<POLICY:granted,diag-set> )
ART:PROMOTE     ( ART:built POLICY:granted -- result<ART:promoted,diag-set> )
~~~

`ART:PROMOTE` owns the §16 atomic publication and is the ONLY writer of
evidence/schedule store rows: the V1 public raw row writers (`EVID-PUT`,
`EVID-PUT-G`, `SCHED-PUT` reachable as generic `MAKI` words) become private to
the promotion/store owner or take the typed products, closing probe 3.
Decoders validate before refining (R2 canonical codecs; a rehydrated row never
mints a proof token — replay rows feed schedule defaults only after key
validation, and promoted status is re-derived from stored evidence plus policy
identity by a validator, never by a raw trust cast). The report becomes a
RENDERING of typed evidence: `REPORT:GATE!` is demoted to the render boundary
and fed only from evidence values, retiring the raw `V-*`/`G-*` pair from the
promotion path (closing probes 1 and 2).

### Acceptance fixtures (must fail to certify)

Concrete checked sketches for the v2 typestate suite
(`maki/typestate-test.f` style: `CHECK-QUIET-CANDIDATE!` per
test/checker-assert.f, exact-verdict asserts, each negative paired with a
resolving positive control per LESSONS). Verdict 0 = type reject; verdict 1 =
unresolvable (sealed word not visible); both are "fails to certify".

~~~forth
\ positive controls: the staged pipeline in the right order certifies
s" TS-OK1 ( TIR:solved -- ) RIR:LEGALIZE RESULT-DROP"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK2 ( PLAN:complete KIR:drafted -- ) KIR:VERIFY RESULT-DROP"
   CHECK-QUIET-CANDIDATE! -1 T=

\ 1. untypeable-wrong-order: unconstrained Model IR cannot enter region
\    planning (R7 acceptance 1); expected/actual name qualified stage families.
s" TS-BAD-ORDER ( MODEL:elab -- ) RIR:LEGALIZE RESULT-DROP"
   CHECK-QUIET-CANDIDATE! 0 T=

\ 2. incomplete-plan: a draft plan cannot enter lowering (R7 acceptance 2).
s" TS-BAD-PLAN ( PLAN:draft KIR:drafted -- ) KIR:VERIFY RESULT-DROP"
   CHECK-QUIET-CANDIDATE! 0 T=

\ 3. wrong-artifact-evidence, type layer: evidence cannot be forged for an
\    artifact because no public word produces the proof token; a raw cell or a
\    foreign id family in the token/id slot rejects, and the private mint does
\    not resolve outside its owner.
s" TS-BAD-EVID-RAW ( CAD-KIND:artifact-id n -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" TS-BAD-EVID-ID ( CAD-KIND:evidence-id EVID:certify-proof -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" TS-BAD-EVID-MINT ( CAD-KIND:artifact-id -- EVID:certified ) MINT-CERTIFY-PROOF EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 1 T=
\    value layer (runtime negative, sealed boundary): a bundle whose golden
\    evidence names artifact B refuses to grant for artifact A with
\    E-EVID-ARTIFACT — asserted as an err result in an executed test, since
\    same-family id equality is a value fact by design.

\ 4. missing-gate: promotion without the policy grant is untypeable, and the
\    grant cannot be minted around POLICY:CHECK.
s" TS-BAD-PROMOTE ( ART:built -- ) ART:PROMOTE RESULT-DROP"
   CHECK-QUIET-CANDIDATE! 0 T=
s" TS-BAD-GRANT ( CAD-KIND:artifact-id CAD-KIND:schema-id n -- POLICY:granted ) POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=

\ regression pinning the retired V1 surface: the raw (verdict, gate-id) writer
\ is no longer reachable from the promotion path.
s" TS-OLD-GATE ( report ptr u8 n n n -- report ) REPORT:GATE!"
   CHECK-QUIET-CANDIDATE! 1 T=
~~~

The analogous shapes were executed against landed machinery in the census
probe (product field family mismatch 0, raw `n` 0, unresolvable ctor 1,
correct construction -1), so these fixtures compile conceptually today; the
implementation sub-dots make the named words real and wire the suite into the
maki gate.

### Implementation sub-dots

Bounded decomposition (children of §21 `v2-type-typestate`,
`v2-evidence-schema`, `v2-promotion`; the orchestrator mints the actual dots).

1. Title: v2-typestate-stage-kinds.
   Problem: pipeline order is enforced by runtime guards (`NEED-MODEL`,
   `NEED-CAPTURE`, maki/cad.f) and by each command re-running earlier phases;
   no type distinguishes a drafted from a verified object.
   Acceptance: stage proof-token families and staged products declared per
   this addendum for MODEL/TIR/RIR/PLAN/KIR/CAND/ART; private mints with
   TRUSTED.md rows; positive/negative candidate matrix TS-OK1/2,
   TS-BAD-ORDER, TS-BAD-PLAN green; wrong-stage diagnostics name qualified
   families.
   Files: maki/ir/*/stage.f (new, one per level), maki/typestate-test.f (new),
   TRUSTED.md.
   Verify: `HB_TMP=<tmp> bin/hb --load maki/typestate-test.f`;
   `bin/hb --load maki/test.f`.
2. Title: v2-evidence-schema-products.
   Problem: evidence is four raw `n` tags in a singleton report plus ambient
   golden-leg globals (census probes 1-2); nothing binds a verdict to an
   artifact or to the gate that produced it.
   Acceptance: `EVID` products/slots/bundle per this addendum; gate transition
   words return `result<_,diag-set>`; golden leg/precision are fields;
   TS-BAD-EVID-RAW/ID/MINT green; report render consumes evidence values.
   Files: maki/evidence/schema.f (new), maki/golden.f, maki/gradcheck.f,
   maki/report.f (render boundary only), maki/typestate-test.f.
   Verify: `HB_TMP=<tmp> bin/hb --load maki/typestate-test.f` and the golden/
   gradcheck suites through `bin/hb --load maki/test.f`.
3. Title: v2-promotion-policy-products.
   Problem: the promotion gate set is a hard-coded word body
   (`PROMOTE-OK?`, maki/cad.f:1026-1030); policy variation would be flags.
   Acceptance: `req-class`/`promote-policy`/`granted` per this addendum;
   `POLICY:CHECK` performs the sealed artifact-id binding with
   `E-EVID-ARTIFACT`; default inference policy reproduces V1 semantics
   exactly (pinned by an executed matrix); TS-BAD-GRANT green; policy id
   participates in the release key.
   Files: maki/evidence/policy.f (new), maki/typestate-test.f, src/config.fs
   (named code).
   Verify: `HB_TMP=<tmp> bin/hb --load maki/typestate-test.f`.
4. Title: v2-promotion-transition-store-seal.
   Problem: `EVID-PUT`/`EVID-PUT-G`/`SCHED-PUT` are public raw-string/raw-tag
   writers; any call path plants evidence and replay rows (census probe 3).
   Acceptance: `ART:PROMOTE` is the only evidence/schedule row writer; raw
   writers private to the store owner or retyped over evidence products;
   rehydration validates rows and never mints proof tokens; the probe-3
   plant-and-replay program no longer compiles against the public surface
   (negative candidates pinned).
   Files: maki/store.f, maki/store-replay.f, maki/cad.f, maki/store-test.f.
   Verify: `HB_TMP=<tmp> bin/hb --load maki/store-test.f`;
   `bin/hb --load maki/test.f`.
5. Title: v2-report-render-demotion.
   Problem: `REPORT:GATE!` raw `(verdict, gate-id)` pair certifies swapped
   arguments (census probe 1) and is the source of promotion truth.
   Acceptance: report gate cells are fed only from typed evidence at the
   render boundary; the public raw pair is retired (TS-OLD-GATE verdict 1) or
   retyped `( report verdict gate -- report )` over the ENUM plus a gate
   family; report-test.f candidates updated; EXPLAIN/packet output unchanged
   for passing paths.
   Files: maki/report.f, maki/report-test.f, maki/cad.f, maki/golden.f,
   maki/gradcheck.f.
   Verify: `HB_TMP=<tmp> bin/hb --load maki/report-test.f`;
   `bin/hb --load maki/test.f`.
6. Title: v2-typestate-store-rehydrate.
   Problem: stored evidence/schedule rows are trusted on read (latest-wins
   string scan); a planted or stale row replays as PROMOTE's decision.
   Acceptance: rehydration validates key shape, engine build, and policy
   identity before feeding replay; invalid rows produce typed diagnostics,
   never silent defaults; stale-evidence and cross-key negative tests green.
   Files: maki/store-replay.f, maki/sched-key.f, maki/store-test.f.
   Verify: `HB_TMP=<tmp> bin/hb --load maki/store-test.f` plus a fresh-process
   replay test through `bin/hb --load maki/test.f`.

Dependency order: 1 → 2 → 3 → 4 → (5, 6). Sub-dots 1-3 are pure additions;
4-6 migrate V1 surfaces and must keep `maki/test.f` green per commit.

## 9. Design Database

### 9.1 Object Identity

Every stored object is addressed by a digest over:

~~~text
object kind
schema version
canonical payload
ordered child hashes
producer/pass version when derived
target/toolchain facts when target-dependent
~~~

Paths, addresses, timestamps, process ids, temporary names, and table indexes
never enter semantic keys.

### 9.2 Core Objects

- design;
- revision;
- model node;
- tensor constraint set;
- analysis;
- region candidate;
- plan;
- Kernel IR module;
- target module;
- artifact;
- evidence bundle;
- promotion/release manifest;
- target;
- toolchain;
- pass configuration.

### 9.3 Revisions

A revision contains:

- logical design id;
- schema version;
- input/output ports;
- root node ids;
- canonical node set;
- parameter/weight references;
- import/source provenance;
- parent revision ids;
- non-semantic author metadata outside the semantic hash.

Edits structurally share unchanged nodes. Multiple candidate revisions coexist.

### 9.4 Linear Transactions

Conceptual API:

~~~forth
DESIGN:BEGIN   ( design-id -- design-txn )
DESIGN:NODE+   ( design-txn op operands attrs -- design-txn node-id )
DESIGN:COMMIT  ( design-txn roots -- result<rev-id,diag-set> )
DESIGN:ABORT   ( design-txn -- )
~~~

A transaction cannot be copied, discarded, committed twice, or published
partially.

### 9.5 Memory And Persistent Stores

The same typed interface has:

1. an OS-backed memory arena/hash index for interactive work;
2. an atomic content-addressed file store for persistence.

Publication is payload-first and manifest-last. Recovery ignores unreachable
objects and rejects incomplete manifests.

### 9.6 Canonical Encoding

Requirements:

- explicit schema version;
- fixed endianness and widths;
- length-delimited strings/vectors;
- deterministic map/set ordering;
- family-qualified variant tags;
- checked length/offset arithmetic;
- fail-closed unknown required fields;
- explicit optional-field evolution policy.

Rendered text is diagnostic/UI output, never the identity source.

## 10. IR Ladder

### 10.1 Model IR

Contains semantics only:

- op schema id;
- typed operands;
- typed attributes;
- symbolic result descriptors;
- source/import provenance;
- semantic effects;
- parameter/constant references.

It excludes fusion ids, materialization flags, schedules, target registers,
measurements, and gate verdicts.

### 10.2 Tensor IR

Adds solved or explicitly deferred facts:

- rank/dimensions;
- dtype;
- logical layout;
- physical strides;
- alignment;
- alias/view relation;
- address space/device placement;
- ownership/mutability;
- numeric policy;
- runtime obligations.

Construction returns a complete artifact or a diagnostic set. No consumer reads
partially propagated facts.

### 10.3 Region IR

Represents legal implementation alternatives:

- region membership and iteration domain;
- boundaries;
- rewrite provenance;
- fusion/split decisions;
- explicit movement/materialization;
- resource upper bounds;
- legal schedule-family set;
- backward save/recompute alternatives.

Region candidates are immutable children of one Tensor IR.

### 10.4 Plan IR

Contains every implementation decision:

- selected region graph;
- target;
- layouts/conversions;
- allocation/lifetime plan;
- schedule family/parameters;
- vector/tail policy;
- block/warp/tile/stage configuration;
- shared-memory mapping;
- precision license;
- save/recompute policy;
- resource/traffic estimates;
- required runtime guards.

Lowering has no hidden defaults after Plan IR exists.

### 10.5 Kernel IR

Contains:

- typed values and blocks;
- explicit address spaces and memory operations;
- structured control flow;
- barriers/uniformity;
- vector/matrix operations;
- masks/bounds;
- collectives/reductions;
- ABI parameters;
- Plan IR provenance.

Kernel verification checks plan/code coherence before target emission.

### 10.6 PTX IR And Module

PTX text is rendered from a typed target IR. The module artifact includes ABI,
target/toolchain ids, resource facts, source maps, and certification evidence.

## 11. Declarative Op Schemas

One op schema owns:

- qualified identity/version/dialect;
- operand and result schemas;
- attribute product;
- shape/layout constraints;
- effects;
- numeric/tolerance policy;
- reference evaluator;
- VJP/JVP;
- canonicalization and rewrite rules;
- cost hooks;
- lowering capabilities;
- import aliases;
- canonical encoding.

Parsing, rendering, completeness checks, dispatch, and documentation derive from
the schema.

Initial dialects:

- MODEL;
- TENSOR;
- REGION;
- KERNEL;
- PTX.

A new op is incomplete until it has schema, constraints, effects, numeric policy,
reference or explicit non-executable reason, lowering/fail-closed diagnostic,
VJP policy, encoding, and positive/negative/round-trip tests.

## 12. Pass Engine

Each pass descriptor contains:

- stable id and semantic version;
- input/output artifact stages;
- required analyses;
- target/config dependencies;
- capability effects;
- invalidation rules;
- deterministic configuration;
- diagnostic schema;
- independent verifier;
- implementation quotation.

Conceptual API:

~~~forth
PASS:RUN     ( pass-id obj-id pass-ctx -- result<obj-id,diag-set> )
PASS:VERIFY  ( pass-id obj-id pass-ctx -- result<evidence-id,diag-set> )
~~~

The explicit pass context contains store, target, cost model, configuration, and
capability handles. Ambient variables are not semantic inputs.

Every output records dependency edges to input objects, pass version,
configuration, target/toolchain, and analyses. Cache lookup validates the full
edge set.

Analyses are immutable objects:

- shape solution;
- use/def and liveness;
- alias/view sets;
- effect summary;
- iteration domains;
- traffic/FLOP estimates;
- resource bounds;
- target legality;
- proof/equivalence graph.

Transformation control flow never self-certifies; an independent verifier checks
the output.

## 13. Rewrite And Fusion

### 13.1 Typed Rewrite DSL

A rule declares:

- typed pattern;
- required constraints/evidence;
- replacement;
- exact or approximate equivalence class;
- produced evidence;
- target-independent legality;
- optional target profitability hook.

Rules operate on schemas and typed operands, not source strings.

### 13.2 Canonicalization

Deterministic convergent rules include:

- identity elimination;
- constant normalization;
- commutative operand ordering;
- view composition;
- no-op reshape/cast removal;
- canonical broadcast;
- dead pure-node elimination.

Canonicalization is idempotent.

### 13.3 Fusion Extraction

Fusion selects legal alternatives rather than mutating node flags. It considers:

- traffic and launches removed;
- recompute cost;
- register/shared-memory pressure;
- occupancy;
- barrier/uniformity legality;
- effects;
- numeric policy;
- target capability;
- cached negative-profitability evidence;
- downstream layout/schedule compatibility.

Every split records the controlling reason.

A bounded e-graph may replace deterministic rewrite enumeration after A1 lands.
The whole model never enters an unbounded e-graph.

## 14. Shape, Layout, Alias, And Memory Planning

The constraint solver owns propagation and diagnostics. Op implementations do
not manually rewrite rows and columns.

Views represent:

- base tensor;
- offset expression;
- shape;
- strides/index map;
- alias set;
- read/write effects;
- materialization requirement.

Reshape, transpose, slice, gather, and broadcast remain views when legal.
Materialization becomes an explicit Plan IR operation when required or measured
profitable.

Bufferization produces:

- allocation ids;
- size/alignment;
- lifetimes;
- alias assignments;
- reuse decisions;
- host/device placement;
- transfers;
- weight-layout transforms;
- peak-memory estimate.

Reuse requires non-overlapping lifetimes and compatible type/layout/space.

Coalescing analysis reads address expressions and reports lane mapping,
transactions, vector legality, alignment/tails, shared-memory banks, and selected
remediation.

## 15. Targets, Cost, Schedules, And Tuning

### 15.1 Target Objects

A target records:

- architecture/capability version;
- warp size;
- register/shared-memory limits;
- launch limits;
- supported dtypes/instructions;
- tensor-core modes;
- alignment/vector rules;
- barrier/collective capabilities;
- driver/PTX/assembler versions;
- measured bandwidth/compute roofs;
- launch/transfer calibration;
- target-specific errata.

Planner policy reads target objects. sm_87 constants do not remain scattered.

The R3 identity owner is `maki/target/target.f`: immutable semantic descriptors
intern to validated `CAD-KIND:target-id` values, with labels kept outside the
identity facts. Schedule keys now require that nominal identity and project its
stable label only at the rendering boundary. Rich driver/toolchain/calibration
facts remain V2-7 schema work rather than raw fields scattered through planners.

### 15.2 Unified Cost Model

One model supplies fusion extraction, materialization, schedule pruning/ranking,
save/recompute, roofline classification, and recommendations.

Each fact names its source:

- exact static;
- target-table estimate;
- calibrated estimate;
- direct measurement.

Measurements override estimates only for an exact canonical key.

### 15.3 Declarative Schedule Families

A schedule schema contains:

- applicability constraints;
- parameter axes;
- derived fields;
- legality predicates;
- resource formulas;
- deterministic default;
- enumeration order;
- lowerer capability;
- canonical encoding.

Adding a family must not require a new central case ladder.

### 15.4 Tuner

1. Enumerate canonical candidates.
2. Prune illegal candidates with reasons.
3. Rank by estimated cost.
4. Emit and certify.
5. Run golden before timing.
6. Warm and sample under an explicit protocol.
7. reject invalid/unstable measurements.
8. Record every attempt.
9. Select deterministically.
10. Publish winner and negative-profitability facts atomically.

Search policy cannot bypass legality or evidence.

## 16. Evidence And Promotion

Evidence classes:

- checker certification;
- IR/pass invariant verification;
- rewrite proof;
- host golden;
- external-artifact golden;
- device golden;
- gradcheck;
- determinism;
- profile/roofline;
- regression comparison;
- toolchain/module validation.

Each evidence object names the exact artifact, inputs, target, policy, and
protocol.

Promotion policies explicitly classify evidence as required, optional,
blocking, or informational for inference, training, approximate precision,
stateful models, and device availability.

There is no force flag. A different policy is a different typed object and hash.

Promotion atomically publishes:

- revision;
- Plan IR;
- executable modules;
- weight/layout artifacts;
- evidence bundle;
- selected schedules;
- runtime manifest;
- rollback predecessor;
- canonical report.

## 17. Incremental Compilation And Test Performance

### 17.1 Package Compilation Cache

Compile checked Forth packages by:

- ordered source closure;
- compiler/checker/core hashes;
- target;
- public signature schema;
- build options.

Cache relocatable validated package artifacts, never live process pointers.

### 17.2 Pass-Level Cache

Every pass output uses the complete dependency key. A one-node edit reuses
unchanged node analyses, unaffected regions, schedules, modules, and evidence.

### 17.3 Gate DAG

- Build the candidate once.
- Validate it regardless of cache source.
- Execute semantic tests in resident family workers.
- Keep CLI/crash/process boundaries isolated.
- Share package/pass artifacts by content hash.
- Make device gates explicit.
- Keep full certification as the merge oracle.

Measure uncontended end-to-end critical-path wall time. Do not retain an isolated
optimization that regresses the full DAG.

Track separate budgets for clean build, hot package compilation, one-node
edit-to-report, one-region rebuild, cached candidate validation, cold/hot full
gate, and device tuning.

## 18. Public API

~~~forth
CAD:OPEN      ( store-ctx target-id -- cad-ctx )
CAD:IMPORT    ( cad-ctx source -- result<rev-id,diag-set> )
CAD:EDIT      ( cad-ctx rev-id edit -- result<rev-id,diag-set> )
CAD:ANALYZE   ( cad-ctx rev-id -- result<analysis-id,diag-set> )
CAD:EXPLORE   ( cad-ctx rev-id policy-id -- result<candidate-set,diag-set> )
CAD:BUILD     ( cad-ctx plan-id -- result<artifact-id,diag-set> )
CAD:VALIDATE  ( cad-ctx artifact-id policy-id -- result<evidence-id,diag-set> )
CAD:PROMOTE   ( cad-ctx artifact-id evidence-id -- result<release-id,diag-set> )
CAD:EXPLAIN   ( cad-ctx obj-id -- report )
CAD:DIFF      ( cad-ctx obj-id obj-id -- report )
~~~

The REPL UI may maintain a current selection. The kernel API never depends on it.

## 19. Repository Structure

~~~text
maki/model/          MODEL dialect, DSL elaboration, import bridge
maki/db/             objects, stores, revisions, transactions
maki/ir/model/       Model IR
maki/ir/tensor/      Tensor IR
maki/ir/region/      Region IR
maki/ir/plan/        Plan IR
maki/ir/kernel/      Kernel IR
maki/schema/         op, schedule, target, evidence schemas
maki/constraint/     dimension/shape/layout solver
maki/analysis/       alias, effects, traffic, resources, liveness
maki/rewrite/        rules, proof graph, extraction
maki/pass/           registry, manager, cache, verification
maki/plan/           fusion, memory, schedule, save/recompute
maki/target/         target descriptions and calibration
maki/backend/ptx/    Kernel IR to PTX IR/module
maki/evidence/       golden, gradcheck, profile, promotion
maki/runtime/        module loading, launch, buffers
maki/ui/             REPL commands and reports
maki/cad.f           curated public re-exports only
~~~

Migrate responsibilities without maintaining indefinite parallel V1/V2
implementations.

## 20. Migration Phases

Each phase produces one coherent artifact. Review, dot decomposition,
implementation, and merge remain separate workflow phases.

### V2-0: Baseline And Contract Freeze

Deliver:

- public API and ownership inventory;
- canonical V1 model/report/device fixtures;
- compile, gate, edit-to-report, build, and device timings;
- V1/V2 compatibility matrix;
- schema/version policy.

Exit:

- preserved behavior has checked fixtures;
- retired behavior has explicit decisions;
- performance work starts from measurements.

### V2-1: Required Type Substrate

Deliver R1-R3:

- wide typed layout storage;
- typed arrays;
- derived equality/hash/order/codecs;
- nominal ids and domain kinds.

Exit:

A nested typed revision object stores, loads, hashes, encodes, decodes, and
compares without raw conversion or trust.

### V2-2: Object Store And Revisions

Deliver:

- canonical object envelope;
- memory/persistent stores;
- linear transactions;
- immutable revisions;
- structural sharing;
- object/revision diff;
- atomic recovery tests.

Exit:

- identical inputs hash identically across processes;
- one-node edit reuses unchanged node ids;
- crash injection cannot publish partial state;
- multiple revisions coexist.

### V2-3: Declarative Model IR

Deliver:

- op-schema registry;
- Model IR;
- DSL elaboration into transactions;
- ONNX builder into the same IR;
- V1 fixture migration;
- retirement of generated-source capture from semantics.

Exit:

- DSL and ONNX produce equal IR for equivalent models;
- a test op is added through one schema and declared hooks;
- maki/cad.f owns no semantic model globals.

### V2-4: Required Shape And Ownership Types

Deliver R4-R6:

- index kinds;
- shape constraint terms/solver;
- existential refinement;
- Tensor IR;
- view/alias model;
- region ownership.

Exit:

- current elementwise/reduction/matmul/movement fixtures solve;
- static mismatches reject before runtime;
- dynamic imported shapes remain typed;
- manual REPROP logic retires;
- transactions and references satisfy ownership gates.

### V2-5: Pass Engine And Typestate

Deliver R7-R8:

- pass/analysis schemas;
- explicit contexts;
- dependency recording;
- result cache;
- verifier hooks;
- effect capabilities;
- typestate/evidence transitions.

Exit:

- one-node edits recompute only dependent objects;
- pass/target/config changes invalidate exactly their outputs;
- cached/uncached artifact hashes match;
- illegal pass order is untypeable.

### V2-6: Region Rewrites And Fusion

Deliver:

- typed rewrite DSL;
- deterministic canonicalization;
- Region IR;
- extraction;
- split reasons;
- migration of fusion/movement rules;
- B1 equivalence witnesses if ready.

Exit:

- V1 fusion fixtures reproduce or improve;
- illegal multi-use/effect/layout/reduction/resource cases reject;
- Model IR is never mutated by fusion;
- selected transformations pass independent verification.

### V2-7: Plans, Targets, And Tuner

Deliver:

- target schema and sm_87 instance;
- unified cost model;
- memory/bufferization plan;
- declarative schedules;
- candidate pruning;
- tuner;
- Plan IR validator.

Exit:

- Plan IR contains every lowering decision;
- lowering has no defaults;
- replay is deterministic;
- measurements are exact-keyed;
- schedule extension has no central ladder.

### V2-8: Kernel IR And PTX Backend

Deliver:

- Kernel IR;
- indexed machine operands and control effects;
- callable routine-effect contracts;
- emitted-CFG liveness and frame verification;
- plan-to-kernel lowering;
- kernel verifier;
- PTX IR/module;
- renderer/assembler bridge;
- provenance maps;
- proof-carrying register-allocation evidence;
- migration of elementwise, reduction, movement, GEMM, and backward paths.

Exit:

- host/device goldens pass;
- plan/code divergence rejects before ptxas;
- generated register, flag, stack, frame, call, and fixup violations reject
  before artifact publication;
- equal keys produce equal artifacts;
- raw text emitters no longer own compiler semantics.

### V2-9: Evidence, Promotion, And Training

Deliver:

- evidence schemas;
- promotion policies;
- atomic releases;
- model-level backward rewrites;
- save/recompute planning;
- training-step regions;
- convergence/profile gates.

Exit:

- missing evidence makes promotion untypeable;
- inference/training policies are explicit;
- backward/training use the same artifact pipeline;
- the from-scratch flagship trains and promotes.

### V2-10: V1 Retirement

Deliver:

- remove singleton Model IR/report/planner state;
- reduce maki/cad.f to public composition/re-exports;
- migrate/retire V1 store rows;
- delete duplicate dispatch and lowering paths;
- update docs/status/paper;
- archive superseded dots.

Exit:

- no V2 command depends on V1 globals;
- no dual semantic implementation remains;
- all native, Maki, PTX, bootstrap, lint, device, and performance gates pass;
- V2 meets ratcheted edit-to-report and hot-gate budgets.

## 21. Proposed Dot Families

After review, decompose into bounded dots:

~~~text
v2-type-layout-store
v2-type-derived-codecs
v2-type-index-kinds
v2-type-shape-constraints
v2-type-existentials
v2-type-region-owner
v2-type-typestate
v2-type-effects
v2-type-machine-effects
v2-type-machine-operands
v2-verify-emitted-cfg
v2-type-scratch-regions
v2-data-layout-proof
v2-machine-effect-differential
v2-register-allocation-certificate
v2-db-object-envelope
v2-db-memory-store
v2-db-persistent-store
v2-db-transactions
v2-db-revisions
v2-schema-op
v2-ir-model
v2-model-dsl
v2-onnx-builder
v2-ir-tensor
v2-pass-registry
v2-pass-cache
v2-analysis-shape
v2-analysis-alias
v2-rewrite-dsl
v2-rewrite-proof
v2-region-extract
v2-target-schema
v2-target-sm87
v2-cost-model
v2-memory-plan
v2-schedule-schema
v2-tuner
v2-ir-plan
v2-ir-kernel
v2-backend-ptx
v2-evidence-schema
v2-promotion
v2-training
v2-package-cache
v2-gate-dag
v2-retire-v1
~~~

Dependencies and exact acceptance commands belong in each dot. Each
implementation dot owns one significant verified change and one commit.

## 22. Competitive Execution Program

Model CAD V2 does not claim universal superiority over Triton, PyTorch,
Inductor, cuBLAS, or cuDNN. It wins only when a fixed workload, device,
numeric policy, and measurement protocol prove one or more of:

- lower end-to-end latency;
- higher throughput at equal output quality;
- fewer global-memory bytes or launches;
- lower peak device memory or energy;
- lower clean or incremental compilation latency;
- earlier rejection with a more actionable diagnostic;
- deterministic, reusable compilation and tuning evidence.

Single-kernel bandwidth parity is a floor, not the objective. The primary
performance target is a repeated multi-operation region where graph-level
fusion, memory planning, persistent tuning, and launch amortization can beat a
piecewise incumbent execution while preserving independently checked numeric
quality. Vendor libraries remain legal candidates: the planner chooses among a
library call, a custom kernel, and a fused region rather than forcing custom
code when the library is faster.

### 22.1 Competitive baseline and claim discipline

Every performance claim records:

- exact model or region revision;
- input shapes, dynamic bounds, dtype, layout, sparsity, and numeric policy;
- target identity, clocks/power mode, software versions, and compiler digests;
- cold compile, warm compile, first-run, and steady-state measurements;
- candidate count, tuning time, cache state, and selected schedule;
- latency distribution, throughput, bytes, launches, peak memory, and energy
  where available;
- host and device correctness evidence against an independent reference;
- equivalent Triton-autotuned, `torch.compile`/Inductor, and vendor-library
  baselines when each applies.

No result may mix arithmetic domains silently. FP32 FMA, TF32 tensor-core,
FP16/BF16 accumulation, and quantized results are separate comparison rows
unless an explicit approximation policy licenses the conversion. The report
distinguishes parity, workload-local win, and broad coverage; one workload can
never support a universal claim.

### 22.2 Compute backend parity

Required before a compute-bound competitive claim:

1. Typed `mma.sync` families for sm_87 tensor-core shapes and accumulator
   domains.
2. Checked `cp.async` staging with pipeline-depth, barrier, and shared-memory
   lifetime legality.
3. Vectorized global and shared-memory transfers with alignment evidence.
4. Bank-conflict-aware shared layouts and coalescing verification.
5. Register-pressure, occupancy, shared-memory, and launch-bound estimates
   validated against device measurements.
6. Schedule-controlled BK, warps, stages, grouping, micro-tiles, and persistent
   variants with no lowering defaults.
7. Shape-keyed persistent autotuning whose complete measurement history is
   replayable across processes.

Exit: GEMM and attention families cover representative small, medium, and large
shapes; the selected kernel is correct under its declared numeric policy; the
report explains every rejected schedule; end-to-end fused regions are compared
against Triton, Inductor, and vendor libraries on the same device session.

### 22.3 Asynchronous execution runtime

The runtime must model streams, events, dependencies, and allocations as typed
resources rather than hidden driver state. Deliver:

- stream and event ownership with deterministic cleanup;
- an async dependency DAG covering kernels, copies, memset, and host fences;
- stream-ordered allocation, lifetime-based buffer reuse, and a zero-allocation
  steady state;
- overlap planning for host/device copies and independent compute;
- CUDA Graph definition, instantiation, update, and replay;
- graph-key invalidation on executable, shape-bound, address, or target changes;
- launch-amortization choices between ordinary streams, graph replay, and a
  persistent region queue.

Exit: a repeated multi-kernel region performs no steady-state allocation,
replays deterministically, preserves dependency ordering under stress, and
beats the equivalent per-launch path where launch overhead is material.

### 22.4 End-to-end fusion targets

The first competitive workloads are region-level compositions where Model CAD
can remove work rather than merely emit the same kernel faster:

- GEMM + bias + activation;
- attention score + mask + softmax + value projection;
- residual + RMSNorm;
- quantize/dequantize and layout-conversion chains;
- forward + backward + optimizer regions.

Each target owns unfused, hand-fused Triton, Inductor, vendor-library, and Model
CAD rows where representable. Acceptance requires equal licensed accuracy,
strictly fewer bytes or launches than the unfused graph, and a measured
end-to-end win for at least one flagship shape distribution.

**Landed evidence (2026-07-15, orin-nx-25w; dot habu-automatic-op-fusion).** The
first end-to-end win through the AUTOMATIC pipeline: a same-shape Add->Mul->Relu
elementwise chain lowered by the real path (`FP-BUILD` -> per-region cubins ->
`LOWER-MODEL-RUN`) and run on device twice. Fusion ON plans ONE elementwise
region / ONE kernel (intermediates register-resident); ablated (`FP-FUSE-OFF!`)
plans THREE per-op regions / THREE kernels with a global round-trip between each.
Both are device-correct against the SAME host golden (`LOWER-MODEL-GOLDEN`
V-PASS at 1 and 3 regions), and the fused kernel finishes the chain 2.07x faster
(77.7 ms vs 160.8 ms / 200 iters at 1,048,576 elems), moving 16 B/elem vs
32 B/elem at the same ~42 GB/s 1-elem/thread memory roof - the win is 2x fewer
global round-trips, not a higher peak GB/s. Rows `FUSE-CHAIN-ON` /
`FUSE-CHAIN-OFF` (`tools/ptx/perf-rows.tsv`, orin-nx-25w). Magnitude-independent
corruption probe: perturbing the fused kernel (ADD->MUL) drives the golden to
V-FAIL; the committed proof stays the clean PASS. Harness:
`maki/fusion-bench-device-test.f` (plan-shape assertions run everywhere; the
golden + bandwidth legs are Orin-only, CUDA-probe-gated SKIP off-device).

### 22.5 Dynamic-shape multiversioning

Existential shape refinement supplies runtime type identity; the execution
program adds:

- symbolic dimension intervals, divisibility, equality, and ordering guards;
- guarded fast paths and a correct generic plan;
- bounded specialization and code-size budgets;
- artifact selection by shape region rather than exact shape only;
- profiling-guided widening or splitting of regions;
- explicit recompile and cache-miss diagnostics;
- data-dependent-shape policy, including operations that remain unsupported.

Exit: variable batch and sequence lengths reuse bounded artifacts without
uncontrolled recompilation; guard failure selects a valid alternate artifact or
returns a typed not-supported result; independent shape opens never alias.

### 22.6 Numeric domains, mixed precision, and quantization

Precision is part of the plan and artifact key. Implement exact FP32, TF32,
FP16, BF16, INT8, INT4/weight-only, and accumulator-domain schemas as target
support permits. A conversion or approximate rewrite requires evidence in one
of the exact, ULP, relative-error, or empirically licensed domains. Quantized
plans record scale/zero-point ownership, calibration provenance, saturation,
rounding, packing, and dequantization placement.

Exit: an approximate candidate cannot satisfy an exact policy; composed error
bounds are deterministic; every empirical license names its independent
golden dataset; tuning may optimize precision only within the requested policy.

### 22.7 Training completeness

To compete with PyTorch rather than only Triton, V2 must provide:

- tensor-level forward and reverse rules for the flagship operator set;
- generated backward regions using the same rewrite/plan/kernel pipeline;
- save/recompute selection with memory and latency evidence;
- fused optimizer steps, gradient accumulation, and mixed-precision scaling;
- deterministic checkpoint and resume;
- seeded convergence and gradient-parity gates;
- a later distributed phase covering collective effects, sharding, DDP-style
  execution, and topology-aware checkpointing.

Exit: the temporal-training flagship matches the independent reference per-step
gradients and convergence tolerance, then shows a fused profiled training step.
Distributed work is not required for the first Orin claim but is required for
general PyTorch-framework parity.

### 22.8 Deployment and ecosystem boundary

Deliver reliable ONNX import, stable AOT packages, versioned runtime ABI,
external tensor/DLPack interoperability, weight loading, promotion-time weight
layout transforms, schema migrations, and artifact compatibility predicates.
The compiler/runtime remains Habu-native; foreign-language bindings are thin
invocation boundaries, never semantic compiler owners.

Exit: a promoted artifact starts without compilation or tuning, rejects an
incompatible target or schema with a typed diagnostic, and consumes/produces
external tensors without copies when ownership and layout permit.

### 22.9 Compile-time and usability advantage

The competitive loop must be faster to operate, not only faster to execute:

- sub-second incremental edit-to-report for bounded edits;
- dependency-cone recomputation and persistent package/kernel/tuning caches;
- zero duplicate parse/check/lower work within one transaction;
- located legality, resource, and numeric diagnostics;
- schedule explanations and ranked counterfactual next moves;
- source-to-Model/Plan/Kernel/PTX/device-profile provenance;
- deterministic replay of every compiler, search, and promotion decision.

Budgets are ratchets in the evidence store. A change that regresses a hot gate,
compile phase, cache-hit path, or report latency fails unless an explicit policy
change updates the budget with evidence.

### 22.10 Non-cherry-picked evaluation matrix

The standing matrix covers:

- Triton autotuned kernels, `torch.compile`/Inductor, and cuBLAS/cuDNN where
  applicable;
- cold/warm compilation and cold/warm artifact caches;
- static and dynamic shape distributions;
- inference and training;
- memory-bound, compute-bound, launch-bound, and capacity-bound regions;
- latency percentiles, throughput, peak memory, energy, compile time, tuning
  time, correctness, and numerical quality;
- success, fail-closed rejection, and diagnostic-guided repair.

The flagship is a fused transformer or temporal-model block. SAXPY remains a
bandwidth-floor regression, never the headline.

### 22.11 Delivery order

1. Tensor-core and async-pipeline backend.
2. Autotuning plus validated resource model.
3. Async graph runtime and stream-ordered allocator.
4. Fused end-to-end transformer/temporal block.
5. Dynamic-shape multiversioning.
6. Mixed precision and quantization.
7. Training/autograd completeness.
8. AOT packaging and interoperability.
9. Proof-producing equality saturation, solver evidence, target generics, and
   proof-carrying imports after the execution substrate is competitive.

## 23. Autonomous Model Engineering Program

Habu is not LLM-native merely because an LLM can emit Forth. It is LLM-native
when every engineering action is discoverable, transactional, bounded,
machine-readable, independently verifiable, and replayable. Natural-language
conversation may select an objective, but compiler semantics, safety policy,
promotion, and rollback never depend on prose interpretation.

The autonomy boundary begins after a human or trusted upstream system provides:

- a versioned task objective and success metrics;
- authority to use named datasets, pretrained weights, and licenses;
- target devices and deployment topology;
- hard latency, throughput, memory, power, accuracy, and startup limits;
- permitted numeric and approximation policies;
- safety constraints and actions that require external authorization;
- experiment, compute, storage, and wall-time budgets.

The system may optimize within that contract. It may not invent the objective,
expand data authority, weaken safety constraints, or self-certify evidence.

### 23.1 Agent protocol and transactional action surface

Expose one typed protocol over the design database and pass engine. Every agent
request names its input revision, expected artifact kind, capability set,
budget, and acceptance policy. Core actions include:

- inspect schemas, revisions, dependencies, diagnostics, plans, evidence, and
  target capabilities;
- begin, validate, commit, abort, diff, replay, and revert a transaction;
- import or construct a model revision;
- request analyses, rewrites, schedules, compilation, training, evaluation,
  profiling, packaging, deployment, monitoring, and rollback;
- enumerate legal next actions and their required evidence;
- query why a candidate was rejected and which facts would change legality;
- compare revisions or artifacts without mutating either;
- promote only through a policy-owned evidence transition.

Requests and responses are typed artifacts, not rendered terminal text. Text and
JSON views are projections of the same canonical values. Idempotency keys,
revision preconditions, capability tokens, and resource budgets prevent stale or
duplicated agent actions. A failed transaction publishes nothing.

Exit: an agent can complete a multi-step edit/compile/evaluate cycle using only
the protocol; replay produces the same revision and artifact hashes; a stale
revision, unauthorized effect, exhausted budget, or invalid transition returns
a structured diagnostic without partial state.

### 23.2 Diagnostic and repair contract

Every failure returns a diagnostic artifact containing:

- violated invariant and owning checker, verifier, runtime, or policy boundary;
- exact source revision, artifact, pass, token/node/region, target, and inputs;
- expected and observed typed facts;
- minimized counterexample or smallest failing dependency cone where possible;
- legal repair classes and the evidence each repair would invalidate;
- related prior failures and whether the attempted repair made progress;
- deterministic reproduction command and environment digest.

The repair loop is explicit:

~~~text
failure artifact
  -> minimize and classify
  -> enumerate legal repair actions
  -> create child revision
  -> run focused verifier
  -> run affected gates
  -> compare objective and resource evidence
  -> promote, continue, or revert
~~~

The search component cannot certify its own repair. Checker/compiler repairs
require negative regressions. Numeric repairs require an independent golden.
Performance repairs require same-session measurements. Repeated non-progress
terminates with a typed blocked result instead of an unbounded conversational
loop.

### 23.3 Complete autonomous ML workbench

The agent-facing system must cover the whole model lifecycle, not only kernel
generation:

- declarative model construction and versioned component schemas;
- broad ONNX import with initializers, control/data dependencies, dynamic
  shapes, provenance, and fail-closed unsupported features;
- native components for convolutions, attention, normalization, embeddings,
  recurrent/temporal state, detection heads, segmentation heads, geometric
  transforms, and common vision preprocessing;
- tensor-level autograd, losses, optimizers, schedules, gradient accumulation,
  mixed-precision scaling, clipping, and regularization;
- deterministic data ingestion, decoding, augmentation, shuffling, batching,
  sampling, and split ownership;
- checkpoint/resume including model, optimizer, scaler, RNG, data cursor, and
  schema versions;
- transfer learning, freezing, adapter/fine-tuning, pruning, distillation, and
  quantization-aware or post-training quantization;
- evaluation, calibration, robustness, drift, and failure-slice analysis;
- reproducible AOT packaging and target deployment.

Exit: a fresh agent process can resume a stopped experiment, explain every
input and transformation, reproduce the next batch and update, and promote only
an artifact whose full lineage and independent evaluation are present.

### 23.4 Experiment, dataset, and model registry

Make datasets, splits, examples, augmentations, pretrained weights, experiments,
checkpoints, metrics, and evaluations immutable content-addressed objects.
Record licenses and authority alongside content. Every run key includes model,
data/split, preprocessing, seed/RNG algorithm, optimizer, numeric policy,
target, compiler, runtime, and environment facts.

The experiment controller provides bounded search, early stopping, successive
halving, retry policy, failure attribution, checkpoint retention, and explicit
compute/storage accounting. Metrics are typed by population, aggregation,
units, and direction; an agent cannot compare unlike populations or optimize a
held-out test set as if it were training evidence.

Exit: equal run keys reproduce; changed data or preprocessing invalidates the
right descendants; interrupted training resumes exactly; leakage, unauthorized
data, missing licenses, and incompatible metric populations reject before
promotion.

### 23.5 DGX Spark to Jetson Orin deployment architecture

The owner workflow is development, training, search, and tuning on a DGX Spark,
followed by deployment to Jetson Orin NX and comparable edge targets. One
immutable semantic model revision fans out into target-specific plans:

~~~text
semantic model revision + weights
    |-- DGX Spark training/tuning plan -> artifacts + evidence
    `-- Jetson Orin NX deployment plan -> artifacts + evidence
~~~

Semantic model identity, weight lineage, dataset evidence, equivalence proofs,
and numeric licenses may transfer. Schedules, precision selections, layouts,
memory plans, binaries, latency/energy measurements, driver/toolchain facts,
and deployment evidence are target-specific and may never be reused by target
name alone.

Deliver:

- explicit Spark and Orin target descriptions and calibrated cost models;
- cross-target compatibility predicates for operators, dtypes, quantization,
  layouts, memory capacity, and runtime ABI;
- training-to-deployment transforms such as freezing, folding, pruning,
  distillation, calibration, quantization, and weight repacking, each with
  lineage and accuracy evidence;
- remote execution agents with bounded capabilities, artifact transfer by
  digest, environment attestation, resumable jobs, and structured telemetry;
- separate Spark search evidence and on-Orin final latency, power, thermal,
  memory, correctness, and endurance evidence;
- reusable target-family schemas for future Orin-class devices without
  conflating their measured capabilities.

The Jetson Orin NX is expected to become available on 2026-07-15. Device-only
acceptance remains not-run until then and must be executed on the exact promoted
tree once access is restored. The DGX Spark environment is provisioned when the
device arrives; no result from another GPU substitutes for its baseline.

Exit: the same semantic revision trains or tunes on Spark, produces an explicit
deployment child revision, compiles separately for Orin, passes independent
goldens, meets Orin constraints, packages without runtime compilation/tuning,
and can be rolled back by artifact digest.

### 23.6 Perception and autonomous-navigation workload ladder

Use a staged ladder so autonomy and safety evidence grow with system scope:

1. Image classification and object recognition: deterministic ingest,
   transfer/fine-tuning, quantized Orin inference, latency/power evidence.
2. Object detection and segmentation: variable image sizes, preprocessing,
   postprocessing/NMS, calibration, per-class and failure-slice metrics.
3. Temporal perception and tracking: stateful regions, bounded history,
   dropped-frame and timing behavior, sequence-level metrics.
4. Navigation perception: depth/pose/occupancy or waypoint outputs with sensor
   synchronization and uncertainty evidence.
5. Closed-loop autonomous navigation: simulator scenarios, deterministic
   replay, safety envelope, fault injection, hardware-in-the-loop, and only
   then supervised real-world deployment.

Object recognition/detection is the first deployment flagship. Navigation may
reuse the same compiler and experiment substrate, but it cannot reuse a static
vision golden as control-safety evidence.

### 23.7 Edge runtime, observability, and safety

The Orin runtime owns sensor/input schemas, timestamp and frame identity,
pre/postprocessing, bounded queues, deadlines, memory pools, thermal/power
policy, watchdogs, health state, telemetry, and atomic artifact activation.
Steady-state inference performs no allocation or compilation. Overload,
missing/stale input, shape violations, device errors, deadline misses, thermal
throttling, and confidence-policy failures produce typed outcomes with explicit
safe actions.

Deployment requires signed manifests, compatibility checks, canary policy,
health-gated activation, previous-artifact retention, and automatic rollback.
For navigation, simulation and hardware-in-the-loop evidence are distinct from
model accuracy and device performance evidence; no one category can satisfy
another.

Exit: fault injection covers corrupt artifacts, incompatible targets, missing
sensors, stale frames, allocation exhaustion, device loss, deadline misses,
thermal throttling, process restart, failed canary, and rollback.

### 23.8 Reference frameworks and environment matrix

Provision isolated, version-pinned reference environments for PyTorch,
`torch.compile`/Inductor, Triton, ONNX Runtime, TensorRT, CUDA libraries, and
profiling tools where supported. They are independent goldens and baselines,
not semantic owners of Habu. Record package lockfiles, container or environment
digests, driver/runtime/compiler versions, target identity, and installation
verification.

Required rows:

- macOS host: source/check/build orchestration only;
- DGX Spark: reference training, Habu training/tuning, compile/profile, and
  cross-target artifact production;
- Jetson Orin NX: reference inference, Habu inference, final correctness,
  latency, throughput, memory, power, thermal, and endurance measurements.

Exit: each framework runs a scalar/tensor smoke, one shared model golden, and a
profiling smoke; unsupported combinations are explicit typed not-supported
facts; no baseline claim mixes environments or device sessions.

### 23.9 Trusted autonomy kernel architecture

The autonomy kernel is the smallest authority-bearing layer. It owns only:

- canonical artifact/schema identity and hashing;
- immutable revision and transaction commit semantics;
- capability and budget enforcement;
- proof-obligation creation and discharge rules;
- verifier identity and independence constraints;
- evidence applicability and typestate transitions;
- promotion, activation, and rollback authorization;
- append-only audit records and deterministic replay.

It does not own model search, rewrite selection, schedule ranking, repair choice,
benchmark interpretation, prose generation, or UI. Those are replaceable agent
or pass implementations operating through the kernel.

#### Canonical typed artifacts

Every stored value uses a canonical envelope:

~~~text
Artifact<Kind> = {
  schema-id, schema-version, kind,
  content-digest, canonical-payload,
  producer-id, producer-version,
  source-revisions[], dependencies[],
  target/config/numeric-policy facts,
  capabilities-used[], created-event
}
~~~

The digest covers every semantic field and dependency version. Provenance and
evidence are first-class linked artifacts, never comments attached to a mutable
row. Decoders reject non-canonical representations, unknown required fields,
kind/schema disagreement, digest mismatch, and unsupported migrations.

#### Deterministic transactions

Transactions use snapshot isolation with explicit read and write sets. Each
action reads one base revision and produces immutable candidate objects plus a
commit proposal. Commit succeeds only when:

- the base/head precondition still holds or an explicit merge proof exists;
- every read dependency and negative lookup is recorded;
- capability and resource budgets cover all effects;
- schemas and artifact digests validate;
- all required proof obligations are discharged;
- no policy-owned evidence is forged by the proposing component.

Commit atomically publishes objects, dependency edges, revision, and audit
event. Crash injection before the commit marker publishes nothing; recovery
either observes the old revision or the complete new revision. Idempotency keys
make retries return the original result. Rebase and merge are explicit passes,
not hidden transaction behavior.

#### Proof obligations and independent verifiers

An action that changes semantics, representation, numeric domain, target,
deployment state, or promotion state emits typed obligations. An obligation
names subject, claimed relation, proof domain, policy, verifier class, and
required environment. Discharge produces evidence linked to the exact subject
digest. Rules include:

- the producer/search pass cannot be the sole verifier for its own claim;
- exact, approximate, empirical, device, safety, and performance domains are
  distinct and non-coercible without a policy constructor;
- changed dependencies, schema, target, numeric policy, verifier version, or
  environment invalidate exactly the affected evidence;
- static proof never substitutes for required execution evidence, and a device
  measurement never proves semantic equivalence;
- missing or stale obligations make promotion unconstructible.

The kernel checks obligation closure; domain-specific verifiers remain outside
the trusted core and are trusted only for their declared evidence constructor.

#### Structured failure IR

All checker, compiler, pass, runtime, benchmark, deployment, and policy failures
lower to one diagnostic IR with typed variants for invariant violation,
unsupported capability, invalid input, resource exhaustion, external failure,
numeric mismatch, performance regression, stale evidence, and authorization
denial. Common fields are:

~~~text
Diagnostic = {
  code, class, severity, owner,
  subject-digest, revision, phase, location,
  expected-facts[], observed-facts[],
  dependency-cone[], counterexample?,
  legal-repairs[], invalidated-evidence[],
  reproduction, environment-digest,
  parent-diagnostic?, progress-measure?
}
~~~

Renderers produce human text and canonical JSON from the same value. Repair
actions reference diagnostic and repair-schema ids, so an LLM selects a legal
typed action rather than editing prose. Diagnostic minimization is a pass with
its own budget and evidence; it cannot alter the original failure.

#### Automatic differential verification

Differential suites are declared artifacts containing input generators/corpora,
reference implementations, normalization, comparison domain/tolerance,
metamorphic properties, target requirements, failure minimizer, and budget.
The runner:

1. derives cases deterministically from suite digest and seed;
2. executes Habu and independent references in isolated environments;
3. compares semantic outputs, gradients, failures, and selected performance
   counters under the declared domain;
4. minimizes discrepancies while retaining the original case;
5. emits evidence on success or a structured diagnostic/counterexample on
   failure;
6. stores every case and environment digest needed for replay.

The standing suites cover checker negative programs, IR/pass equivalence,
operator forward/backward values, ONNX import parity, PyTorch eager and compiled
models, Triton/vendor kernels where applicable, quantization/calibration, and
Spark/Orin cross-target model semantics.

#### Evidence-gated promotion typestate

Artifacts move through explicit immutable states:

~~~text
Candidate
  -> Verified
  -> Measured
  -> PolicySatisfied
  -> Promoted
  -> CanaryActive
  -> Active
  -> Retired | RolledBack
~~~

Transitions consume a complete obligation set and produce a new state artifact;
they never mutate the candidate. Promotion policy binds model, weights,
target, numeric policy, data populations, required verifiers, thresholds,
expiration, and rollback artifact. Activation requires target attestation and
deployment health evidence. Rollback is always available without recompilation
and emits its own audit/evidence event.

#### Machine-facing action registry

Every callable action is a registered schema with input/output kinds,
preconditions, effects, capabilities, deterministic/cacheable flags, budget
dimensions, produced obligations, verifier, diagnostics, and invalidation
rules. The LLM discovers actions and enum values from the registry; it never
constructs raw command strings as the semantic interface. CLI/JSON/MCP-style
adapters are thin codecs over the same transaction protocol.

Initial protocol operations are `SCHEMA:LIST`, `ARTIFACT:GET`, `REVISION:DIFF`,
`TX:BEGIN`, `TX:APPLY`, `TX:VALIDATE`, `TX:COMMIT`, `TX:ABORT`, `PASS:RUN`,
`OBLIGATION:LIST`, `VERIFY:RUN`, `DIAGNOSTIC:EXPLAIN`, `REPAIR:LIST`,
`EVIDENCE:COMPARE`, `PROMOTION:PROPOSE`, `PROMOTION:ACTIVATE`, and
`PROMOTION:ROLLBACK`. Each operation has a checked Habu API and canonical
request/response codec.

#### Implementation order

1. Canonical artifact envelope, schema ids, codecs, and content digests.
2. Immutable object store, revisions, dependency index, and crash-safe
   transactions.
3. Action registry, capability/budget vocabulary, and typed protocol codecs.
4. Obligation/evidence schemas and independence/applicability checker.
5. Diagnostic IR, renderers, minimizer protocol, and legal-repair registry.
6. Differential-suite schema, isolated runner, reference adapters, and
   counterexample store.
7. Promotion typestate, policy evaluator, activation, audit, and rollback.
8. Agent-loop controller with bounded progress and deterministic replay.
9. Object-recognition flagship through the complete interface.

Exit: deleting the LLM and replaying its recorded action requests produces the
same revisions, artifacts, evidence decisions, deployment state, and rollback;
replacing the LLM changes search choices but cannot bypass legality, authority,
budget, verification, or promotion policy.

### 23.10 Autonomous flagship acceptance

Starting from only the signed task contract and authorized inputs, a fresh agent
must:

1. inspect targets, schemas, budgets, datasets, and prior evidence;
2. import or construct the object-recognition model;
3. train/fine-tune and resume after an injected interruption;
4. diagnose and repair at least one injected type/shape/numeric failure;
5. search legal Spark candidates within budget;
6. derive a separate Orin deployment revision;
7. calibrate/quantize only if licensed by policy;
8. compile, package, transfer, validate, and profile on Orin;
9. compare PyTorch/Triton/TensorRT/ONNX Runtime baselines where applicable;
10. promote through independent evidence, deploy a canary, detect an injected
    health failure, and roll back;
11. reproduce the complete decision and artifact history from the database.

No step may require a human to reinterpret an error, edit hidden state, choose
an undocumented compiler default, reconstruct provenance, or manually decide
whether evidence applies. External authorization remains required only for
actions explicitly named by the signed safety policy.

## 24. Risks

### Type-System Scope Explosion

Control: implement the required bounded kinds, dimension language,
existentials, lexical regions, typestate, and finite effect vocabulary. General
dependent types and lifetime inference remain out.

### Dual-System Drift

Control: migrate vertical fixtures and retire each V1 owner immediately after
parity. Never add a new op to both systems.

### Cache Unsoundness

Control: record dependencies at reads, include every producer/schema/target
version, verify cache-hit artifacts, and maintain mutation fixtures for key
invalidation.

### Proof Theater

Control: proof constructors are checker-owned; verifiers are independent of
search; device golden remains mandatory where static proof is insufficient.

### Serialization Lock-In

Control: explicit versions, required/optional fields, migration tools, and
golden bytes from the first store slice.

### Forth Image Growth

Control: runtime schemas/data live in OS-backed stores; metadata is compact and
growable; exact code/data/dictionary growth is measured; compiled support is
package-cached.

### Search Explosion

Control: regional bounds, deterministic fuel, legality pruning, cost ranking,
negative facts, and explicit budgets.

### Device Availability

Control: typed not-run evidence names the cause; policy decides whether it
blocks; device-required policies cannot be satisfied off-device.

## 25. Flagship Acceptance

### LocateAnything-Derived Inference

- real ONNX import;
- static/dynamic shape refinement;
- movement/view planning;
- RMSNorm, RoPE, attention/KV-cache, linear/GEMM regions;
- external golden;
- sm_87 schedule search;
- promoted runtime artifact;
- measured incremental reuse after a model edit.

### From-Scratch Temporal Training

- forward Model IR;
- generated model-level backward;
- save/recompute planning;
- fused optimizer/training regions;
- numeric gradcheck;
- seeded convergence gate;
- GPU profile;
- promoted training artifact.

### Comparative Evidence

- edit-to-report latency;
- clean/incremental compile latency;
- traffic and launch counts;
- tuned/default schedule deltas;
- device correctness/numerics;
- end-to-end external-baseline latency;
- replay determinism across restarts;
- candidate rejection stage and repair quality.

## 26. Definition Of Done

1. Multiple immutable revisions coexist and share nodes.
2. Every IR level is typed, encoded, hashed, and independently verified.
3. Static shape/layout/kind errors reject before runtime.
4. Runtime shapes remain typed through existential refinement.
5. Passes have explicit contexts, dependencies, invalidation, effects, and
   diagnostics.
6. Fusion/search emits replayable transformation and split evidence.
7. Plans completely determine lowering.
8. Targets and schedules are declarative.
9. Promotion requires artifact-indexed typed evidence.
10. One-node edits recompute only the affected dependency cone.
11. Inference and training flagships pass host/device gates.
12. V1 singleton semantics and duplicate planner/lowering paths are removed.
13. Native fixpoint, bootstrap, Maki, PTX, lints, cold/hot gates, and device gates
    pass on the exact merge tree.
14. Edit-to-report, compilation, and gate performance meet ratcheted V2 budgets.
15. Compute-bound kernels use checked tensor-core/async pipelines where selected.
16. Repeated regions have a typed async plan and zero-allocation steady state.
17. Dynamic-shape distributions reuse bounded guarded artifacts.
18. Numeric-policy and quantization evidence are part of every approximate key.
19. Competitive claims come from the standing external-baseline matrix.
20. Every agent action is a typed, budgeted, idempotent transaction over an
    explicit revision and capability set.
21. Diagnostics expose violated invariants, exact ownership, reproduction,
    legal repair classes, and invalidated evidence as structured artifacts.
22. Experiments, datasets, checkpoints, metrics, and promotions are immutable,
    authorized, content-addressed, and exactly resumable.
23. One semantic revision produces separately planned, compiled, measured, and
    promoted DGX Spark and Jetson Orin artifacts.
24. The Orin runtime is allocation-free in steady state and has health-gated
    activation, telemetry, fault injection, and automatic rollback.
25. A fresh agent completes the object-recognition flagship from signed task to
    deployed canary without unstructured human intervention.
26. Autonomous-navigation deployment remains gated on simulator,
    hardware-in-the-loop, timing, uncertainty, and safety-envelope evidence.
27. Every emitted callable, CFG edge, machine operand, register/flag/frame
    effect, scratch region, and allocation is typed or independently certified;
    generated-state clobbers reject before artifact publication.
28. Every promoted PTX artifact binds one verified virtual instruction/CFG
    subject to the exact target, toolchain, `ptxas` attestation, cubin/SASS
    identity, launch ABI/configuration, device evidence, and promotion policy;
    proprietary allocation remains explicitly opaque and every mutation,
    stale report, unsupported verifier, or wrong-subject evidence rejects.
