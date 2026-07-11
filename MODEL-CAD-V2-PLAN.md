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
- dtype;
- layout;
- address-space;
- stage;
- effect;
- region.

Why required:

All of these are one-cell values physically. Treating them as n permits
semantically catastrophic swaps that ordinary stack checking cannot detect.

Acceptance:

- every cross-role swap rejects;
- storage preserves the nominal family;
- public CAD APIs contain no raw n handles;
- renderer and diagnostics preserve role names;
- rollback, snapshots, source replay, and derived operations retain kinds.

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

Why required:

Rewrite, fusion, recomputation, caching, and pass scheduling are unsound if
stateful/random/IO operations look pure. A general effect calculus is not
required; the finite CAD effect vocabulary is.

Acceptance:

- random/stateful ops cannot be duplicated for recompute;
- writes and atomics cannot cross illegal reorder/fusion boundaries;
- pure passes run without IO/device authority;
- analysis-only contexts cannot publish persistent artifacts;
- cache keys include every capability-controlled semantic input.

### R9. Type-System Dependency Order

~~~text
R1  wide layout storage and typed arrays
R2  derived eq/hash/order/codecs
R3  nominal artifact and domain index kinds
R4  dimension/shape constraints
R5  existential packaging and runtime refinement
R6  region owner/reference capabilities
R7  typestate and artifact-indexed evidence
R8  explicit CAD effect capabilities
~~~

R1-R3 unblock the design database. R4-R5 unblock a genuinely typed Model/Tensor
IR. R6-R8 unblock safe transactions, transformation, caching, and promotion.

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

Add checked constructors/evidence for:

- nonnegative lengths/counts;
- bounded indexes;
- byte/cell offsets;
- aligned sizes;
- nonzero divisors;
- overflow-checked products.

Benefit:

Object encoding, bufferization, shape arithmetic, and file parsing reject more
same-cell semantic-role bugs before memory access.

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
- plan-to-kernel lowering;
- kernel verifier;
- PTX IR/module;
- renderer/assembler bridge;
- provenance maps;
- migration of elementwise, reduction, movement, GEMM, and backward paths.

Exit:

- host/device goldens pass;
- plan/code divergence rejects before ptxas;
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

## 23. Risks

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

## 24. Flagship Acceptance

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

## 25. Definition Of Done

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
