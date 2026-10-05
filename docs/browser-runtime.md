# Habu Browser Runtime — consolidated design, revision 2

Revision 2 of 2 October 2026. Adopted 2026-10-03 for Maki's viewer; its server-durable SYNC profile is habu-add-a-srv-845f35e8.

**Status:** replacement implementation specification; not an implemented Habu browser runtime or a browser-qualification report.  
**Replaces:** the earlier runtime architecture, proposed protocol v1, and their conflicting contract language. The old documents are historical evidence only. No v1 wire-compatibility promise is retained.  
**Scope:** the full Habu-authored browser application platform: declarative UI, native controls, reactive state, tools, GPU viewport, capabilities, persistence, collaboration, development tools, and independently admitted compiler/plugin/fallback profiles.

The revision addresses R01–R24 in `habu-browser-runtime-audit.md` and the supplementary A01–A24 findings in `habu-browser-runtime-design-audit.md`. Section 29 maps every finding to its replacement contract and tests. “Specified” means a decision and algorithm exist here; it does not mean the implementation or browser tests have passed. Python reference models in the external reference package test selected temporal invariants independently of Habu. Actual checker, browser, IME, accessibility, and GPU qualification remain implementation gates.

HBR2 was delivered in an external reference package. Habu holds only this document. The package's other files are provenance: the R01–R24 and A01–A24 audits, the Python tools `tools/*.py` and models `tests/reference_models.py`, their recorded results `reference-test-results.json` and `registry-checks.json`, the registry JSON, the fixture specification and `archive/`. §28 names the oracles Habu uses instead.

## Habu decisions

A Fable design review decided these on 2026-10-04, and Habu adopted them. The
table maps HBR2's packages to Habu's.

| Package | Holds | Error codes |
|---|---|---|
| `RT-ID` | `Id128` and `ErrorClass` | -9500..-9509 |
| `RT-HANDLE` | `Handle` and handle tables | -9510..-9519 |
| `RT-POOL` | Memory pools, grown in 64 KiB chunks to §26.1's ceilings | -9520..-9529 |
| `RT-RECORD` | Immutable typed records (§3.1) | -9530..-9539 |
| `RT-TREE` | The persistent B+ tree (§3.1) | -9540..-9549 |
| `RT-ROOTS` | RootSet and snapshot leases (§3.2): HBR2's `RT:` and `SNAPSHOT:` words | -9550..-9559 |
| `RT-TXN` | Transactions (§3.3) | -9560..-9569 |
| `RT-JOB` | Jobs (§4.1) | -9570..-9579 |
| `RT-SCHED` | The scheduler step (§4.1, §24.5) | -9580..-9589 |
| `RT-SCOPE` | Scopes (§5.1) and the host port: `SUBMIT`, `WAKE`, `DELIVER`, `TIME!`, `TIME@` and `submit-result` | -9590..-9599 |
| `RT-REQUEST` | Requests (§5.2) | -9600..-9609 |
| `RT-CMD` | Registered commands (§18.1) | -9610..-9619 |
| `RT-QUERY` | Queries (§3.1, §6) | -9760..-9769 |
| `RT-COST` | §4.1's cost units and the declared weights of RUNTIME read words | not assigned |
| `UI` | The §7.2 builder: HBR2's `UI:` words | -9690..-9699 |
| `UI-TOKEN` | UI tokens (§7.1) | -9650..-9659 |
| `UI-ACTION` | Actions (§7.4) | -9660..-9669 |
| `UI-BINDING` | Bindings and `ControlValue` (§7.4, §9.1) | -9670..-9679 |
| `UI-LAYOUT` | Layout records (§11.1) | -9680..-9689 |
| `UI-REACT` | The reactive graph and retry (§6) | -9700..-9709 |
| `UI-COMP` | Components (§7.3); HBR2's `STATE:READ` is `UI-COMP:STATE@` | -9710..-9719 |
| `UI-RECON` | Reconciliation (§8.1) | -9720..-9729 |
| `UI-RESOURCE` | Resources (§7.4) | -9730..-9739 |
| `UI-COLLECTION` | Collections (§11.4) | -9740..-9749 |
| `UI-ADMIT` | Callback admission (§7.5) | -9750..-9759 |

**Habu decisions (2026-10-04).** Names: HBR2's `RT:`/`SNAPSHOT:` words live in `RT-ROOTS`; `UI:` keeps the §7.2 builder; other packages are `RT-*` and `UI-*` (table above). Port: `RT-SCOPE:SUBMIT` and `WAKE` are `defer` words the embedding binds once; `DELIVER` and `TIME!` are its inputs; `submit-result` is §24.5's eight results, defined once. Memory: pools grow by 64 KiB chunks from lib/memory.f to §26.1 ceilings; slot IDs are handles; a refused growth is RecoverableOOM. RootSet: a stamp header and six slots indexed by `rt-store` (§3.2's list); component state lives in `session` keyed (instance, schema key). Queries: key-range scans with a declared field-equality filter; query revision is the store slot's revision. Mutation: handlers return Actions; every write is a registered command's PROJECT. Admission: `UI-ADMIT` reads frozen HIR through NFROZEN in the engine's post-freeze observer cell and enforces at registration; admitted packages compile at tier 1; fuel is the Wasm path's. Cost: 1 per HIR op, callee cost + 1, bound × body, ⌈bytes/4096⌉; `RT-COST` holds the units. G1 package: Workspace, AssemblyTree, PartEditor, MaterialChooser, ConstraintPanel; trace to Apply's command. Guards: one recorded removal proof per guard, no mutation harness. Cycles: the package-DAG lint; compile units follow package-build §4.5. Tokens: `DEFTYPE` with retired public converters and private `CAST:`; linears are `DEFLINEAR` with one private `LINEAR:` pair each, the checked way (docs/forth.md). Errors: each package mints its decade in its owning file (RUNTIME -9500.., UI -9650..); lib/errors.f carries one comment line per package. Deferred without a G0-G4 caller: compaction, JOIN/RACE/BOUNDED-MAP, semantics records, Boolean/OptionIds bindings, secondary indexes.

Counters (a lead decision of 2026-10-04 on the oracle's recommendation): the
never-reused ComponentInstanceId and PlacementId counters are each an opaque
`DEFTYPE` reference to private state in cells the scope owns exclusively, not a
linear token. Copying a reference aliases its state, so no typed route rewinds a
counter; the epoch's scope holds both across host calls; `CLOSE` marks a counter
closed for every alias and never resets it.

Oracles (§28.3): Habu does not hold the independent Python codec. The
wire's independent oracle is the golden vectors of
habu-pin-hbr2-wire-0b340032 (`lib/browser/hbr-v2-registry.json`,
`test/wasm/hbr2-fixtures.f`) with hand-built malformed vectors, in place of
§28.3's Python codec.

## Contents

- [0. Reading and precedence](#0-reading-and-precedence)
- [1. Architecture and product contracts](#1-architecture-and-product-contracts)
- [2. Identities, stamps, authority, and validity](#2-identities-stamps-authority-and-validity)
- [3. Immutable state, snapshots, transactions, and memory reclamation](#3-immutable-state-snapshots-transactions-and-memory-reclamation)
- [4. Scheduling, bounded execution, clocks, and backpressure](#4-scheduling-bounded-execution-clocks-and-backpressure)
- [5. Scopes, asynchronous tasks, and durable ownership](#5-scopes-asynchronous-tasks-and-durable-ownership)
- [6. Reactive graph, consistency, and retry](#6-reactive-graph-consistency-and-retry)
- [7. Concrete Habu UI API and admission](#7-concrete-habu-ui-api-and-admission)
- [8. Retained UI, bounded DOM publication, and relocation](#8-retained-ui-bounded-dom-publication-and-relocation)
- [9. Fields, native controls, composition, and form snapshots](#9-fields-native-controls-composition-and-form-snapshots)
- [10. Native interaction adapter, focus, and user activation](#10-native-interaction-adapter-focus-and-user-activation)
- [11. Layout, docking, portals, virtualization, and routes](#11-layout-docking-portals-virtualization-and-routes)
- [12. Widget machines and host/Habu division](#12-widget-machines-and-hosthabu-division)
- [13. Accessibility, localization, themes, and text](#13-accessibility-localization-themes-and-text)
- [14. Partial document residency, scene bundles, and interaction tools](#14-partial-document-residency-scene-bundles-and-interaction-tools)
- [15. GPU content ownership, frame leases, and reliable jobs](#15-gpu-content-ownership-frame-leases-and-reliable-jobs)
- [16. Render mathematics, graph, shaders, and visual policy](#16-render-mathematics-graph-shaders-and-visual-policy)
- [17. WebGL2, unavailable graphics, and backend parity](#17-webgl2-unavailable-graphics-and-backend-parity)
- [18. Semantic commands, collaboration, and ordered outcomes](#18-semantic-commands-collaboration-and-ordered-outcomes)
- [19. Indexed storage, blobs, migrations, and multiple tabs](#19-indexed-storage-blobs-migrations-and-multiple-tabs)
- [20. Closed browser capabilities](#20-closed-browser-capabilities)
- [21. Boot, deployment, developer tools, and optional compilation](#21-boot-deployment-developer-tools-and-optional-compilation)
- [22. Authorization, privacy, resource isolation, and recovery security](#22-authorization-privacy-resource-isolation-and-recovery-security)
- [23. Integrated behavior and failure traces](#23-integrated-behavior-and-failure-traces)
- [24. Binary ABI and boundary ownership](#24-binary-abi-and-boundary-ownership)
- [25. Complete operation/type registry](#25-complete-operationtype-registry)
- [26. Resource accounting, workloads, and performance contracts](#26-resource-accounting-workloads-and-performance-contracts)
- [27. Implementation dependency graph and deliverable gates](#27-implementation-dependency-graph-and-deliverable-gates)
- [28. Verification and evidence](#28-verification-and-evidence)
- [29. Audit closure and replacement decisions](#29-audit-closure-and-replacement-decisions)
- [30. Sources, provenance, and trust boundary](#30-sources-provenance-and-trust-boundary)
- [Appendix A — protocol registry](#appendix-a--generated-hbr2-registry)

## 0. Reading and precedence

This is a consolidated design, not an amendment that requires reconciling old files. Sections 1–23 define behavior; sections 24–26 define encoding, operation schemas, budgets, and release construction; sections 27–29 define implementation gates, evidence, and issue closure. The generated protocol registry appended to this document is part of the specification. Sources are in section 30.

Normative terms **must**, **reject**, and **only** describe implementation obligations. Numbers labelled *initial budget* are configurable profile defaults, not measured speed claims. No optional feature is activated by detecting an API alone: the application manifest requests it, the host grants it, and a qualification profile admits it.

There are three evidence levels:

| Level | Meaning |
|---|---|
| Specified | Representation, transitions, ordering, cleanup, limits, and test obligation are defined |
| Reference-model checked | The external reference package's independent executable model exercised the stated invariant |
| Qualified implementation | Real Habu and the exact declared browser/device combination passed the gate |

Do not substitute one level for another. The manifest of a deployed application records the third level, not this document's existence.

## 1. Architecture and product contracts

### 1.1 Responsibilities retained

Habu owns document/session state, semantic commands, component state, reactive derivations, UI construction and reconciliation, interaction tools, scene derivations, render planning, journal/reconnect policy, and application validation. The host owns DOM objects, CSS execution, native editor buffers and IME, synchronous input mechanics, browser API objects, GPU objects, and platform call execution.

The host is a **closed native interaction adapter**, not a promise of a few hundred lines of JavaScript. It executes finite, typed policies described below, not application callbacks or engineering rules. Application authors write Habu. React, arbitrary HTML, arbitrary CSS, reflection over DOM objects, and application-specific JavaScript are not prerequisites.

The production compiler remains the direct checked-Habu-to-Wasm design: memory32 addressing, 64-bit Habu cells, dedicated Wasm emission, narrow imports, no production LLVM/MLIR/Emscripten dependency. AOT is sufficient for the application. Browser-hosted compilation is a separate profile; no second unchecked interpreter is introduced. The pinned source guide says quotations do not capture enclosing locals and genuine primitive boundaries cannot be fabricated to bypass the checker [H1, H2]. This design uses owned data environments instead.

### 1.2 Independent packages

```text
RUNTIME: IDs, immutable stores, snapshots, transactions, jobs, scopes
UI:      components, bindings, reactivity, native-interaction descriptions
SCENE:   assets, instances, queries, scene bundles, tools
RENDER:  immutable render inputs, graph, shader interfaces, execution plans
SYNC:    optional durable operations, receipts, subscriptions, reconnect
BROWSER: byte transport, DOM/input/editing, GPU executor, capabilities
MAKI:    engineering model, solver and geometry contracts, topology semantics
```

`RUNTIME` cannot import `UI`, `SCENE`, `BROWSER`, or `SYNC`. `UI` imports `RUNTIME`; a noncollaborative form does not link `SYNC` or a renderer. `RENDER` does not import Maki or an exact kernel. Native/headless embeddings implement the same portable boundaries. Package loading and dependency digests use Habu's package build system, not a new browser-only package manager.

For the initial Maki deployment, exact modeling is server-authoritative and the browser has meshes and semantic metadata. The geometry service is kernel-neutral. OCCT, another server kernel, or a future qualified local kernel is an application/deployment choice, not a type baked into this runtime.

### 1.3 Profiles and feature retention

| Profile | Required | Explicit limitations |
|---|---|---|
| DOM | worker AOT, controls, forms, capabilities selected by app | no viewport required |
| CAD-WebGPU-worker | DOM plus worker WebGPU and OffscreenCanvas, scene/picking | capability-probed with disposable canvas before real surface transfer |
| CAD-WebGPU-main | same application/scene semantics, main GPU executor | tighter encoding budgets; no semantic code moves into JS |
| CAD-WebGL2 | separately qualified shaders and executor described in §17 | no general compute; CPU semantic picking; declared render differences |
| Developer | inspector, trace replay, AOT worker replacement | disabled by default in production |
| Browser compiler | existing compiler/publication profile | independent compiler closure and self-build gates |
| Plugin | isolated Wasm instances and restricted schema | no same-heap extension for untrusted code |

Every platform feature has Available, Denied, Unsupported, or TemporarilyUnavailable state and a typed fallback. Detection is not universal browser support. A failed canvas transfer/render qualification may require replacing the actual DOM canvas while preserving a logical viewport ID [S10].

### 1.4 Four lifetime contracts

| Contract | Owner and retained object | Termination |
|---|---|---|
| Snapshot lease | job/reader retains immutable RootSet and projection stamp | finish/cancel; bounded reference release |
| Edit lease | host editor retains stable field target, native node, draft and edit sequence | explicit end, approved relocation/handoff, or target revocation |
| Frame lease | host owns exact sealed GPU input versions, maps and transient allocations | skipped/failed before submit, or safe queue/readback completion |
| Durable operation | document service owns journal identity and outcome discovery | persisted terminal receipt and retention/compaction policy |

They share identity tables but do not become one universal transaction. State publication, DOM activation, GPU submission, IDB completion, and server acceptance are separate events.

## 2. Identities, stamps, authority, and validity

### 2.1 Identity spaces

Persistent application identities are opaque 128-bit byte sequences. Temporary host resources use a typed `Handle {slot:u32, generation:u32}`; both nonzero or both zero for null. A context plus allocator kind owns each handle table. JavaScript never converts U64 identities through Number. Serialized u64 uses BigInt or two u32 words. Memory offsets are a separate u32 type; pointers, execution tokens, document IDs, and host handles are never interchangeable.

Allocator authority is fixed:

| Identity | Allocator | Reuse rule |
|---|---|---|
| PageSessionId | main bootstrap, random Id128 | new page boot |
| RuntimeEpoch | main host, u64 | increment for each worker incarnation |
| AuthEpoch / NamespaceId | host authority service | increment on any privilege/tenant identity transition |
| Scope handle | host on SCOPE-OPEN | generation increments; tombstone through outstanding results |
| ComponentInstanceId, PlacementId | Habu, u64 in runtime | never reused in that runtime |
| Node / policy handle | Habu-reserved namespace, host validates CREATE/INSTALL | disjoint from host resource slots; generation increments |
| TreeEpoch | host-root controller | assigned at initial mount and recovery activation |
| Surface/device handles and epochs | host GPU service | only host allocates; replacement never revives old handles |
| EditSession / EditLease | native edit host | retained only in its page/runtime binding epoch |
| File/blob/stream/image/GPU version | relevant host service | typed table, owner scope/namespace, generation |
| OperationId | document service, cryptographic Id128 | never changed on retry |
| Server sequence and entity revision | authoritative server | monotone within DocumentEpoch |

Generation exhaustion retires the slot. Sequence/epoch exhaustion starts a new owning session, never wraps. A raw generation value is not an authorization capability. Ports bind PageSessionId, RuntimeEpoch, producer and host grants out of band; a packet cannot change those bindings.

### 2.2 Coherent state stamps

```text
AuthorityStamp = NamespaceId + AuthEpoch
DocumentStamp  = DocumentId + DocumentEpoch
ProjectionStamp = DocumentStamp + ConfirmedSequence
                + PendingGeneration + PreviewGeneration
EntityVersion = EntityId + EntityRevision
UIStamp = RootHandle + TreeEpoch + TreeRevision
FrameStamp = SurfaceHandle + SurfaceEpoch + DeviceEpoch
           + FrameId + SceneBundleId + CameraVersion + InputWatermark
```

`PendingGeneration` increments whenever membership, order, canonical arguments, or projection outcomes of local operations change. `PreviewGeneration` increments for provisional tool changes. They do not advance server sequence. A content-addressed immutable asset may remain reusable after unrelated changes, provided namespace policy and its exact dependency manifest still match.

### 2.3 Per-message dependency matrix

Every packet matches page/runtime/channel/producer. Additional dependencies are explicit in the record, not implied by a coincidentally equal counter:

| Record class | Required match | Stale handling |
|---|---|---|
| UI native input | authority, root/tree epoch, node generation, binding generation | old action resolves through retirement table; revalidate domain preconditions; never rebind |
| Edit snapshot | authority, stable FieldKey, lease/session, edit sequence | vault old draft if appropriate; reject retarget; release payload |
| Editor correction | exact session/edit sequence plus binding generation | Applied, Stale, Composing, Missing, or Ended result |
| Job output | owner scope generation and its recorded snapshot/dependencies | adopt only under declared compare/readset rule; otherwise discard/restart |
| Geometry result | document epoch and declared entity dependencies, geometry/bundle versions | reuse immutable cache by hash only; do not attach to wrong projection |
| Display frame | device/surface epochs plus exact version handles and SceneBundleId | skip before submit; terminal outcome; retain submitted leases |
| Pick | task ID, retained scene/camera/layout and mapping versions | finish original snapshot or StaleTarget/Unavailable; no silent retarget |
| Network/storage completion | authority and namespace of original request | never route into current tenant; finish original durable record or quarantine |
| Server event | document epoch and next authoritative sequence | buffer bounded gaps, resync checkpoint; deduplicate operation ID |
| Credit/terminal/cancel | page/runtime/channel generation | discard old-port messages without re-execution |

A valid stale result is not necessarily a malformed packet. Distinguish StaleTarget from ProtocolViolation. Stale owned results still require release under their original authority context.

## 3. Immutable state, snapshots, transactions, and memory reclamation

### 3.1 Selected baseline representation

Use immutable typed records indexed by a persistent B+ tree. This revision selects that structure; implementers do not choose independently between whole-heap copying and arbitrary mutable objects.

Keys are 16 bytes, compared lexicographically; composite field indexes have generated canonical 16-byte IDs with a collision-resolving full-key side table, or use a separate tree per entity keyed by declared field ordinal. Hash equality alone never establishes identity. The simpler first implementation uses one entity tree and a fixed-schema immutable field record per entity.

Each tree page is 4 KiB, has a 64-byte header, and a bounded layout. Internal pages hold at most 64 child references and 63 separator keys. Leaf pages hold at most 96 `(Id128 key, RecordRef:u64, revision:u64)` entries. Unused bytes are zero. Values larger than a record page are chunked immutable objects. Internal and leaf layouts use the same fixed page pool but different schema tags. Page IDs are generation-checked references, not exposed guest pointers.

Lookup is binary search within each node, O(log n) page visits. Insertion clones the root-to-leaf path; overflow splits a leaf 48/49 and an internal page 32/33, propagating separators upward. A transaction's clone map ensures a page is cloned at most once within that transaction. Deletion writes a tombstone, removed by bounded rebuild/compaction; lookup skips tombstones. A root with more than 25% tombstones or more than twice live payload bytes queues compaction. Tombstones may temporarily reduce occupancy; this does not change key-range routing or increase tree height. Compaction builds a new tree and publishes only when complete.

Records contain schema/version, bounded payload length, and references to immutable children. Domain cycles (assembly graph, dependency edges) use stable IDs, not cycles of owning references. All owning memory edges form a DAG. Cycles detected in imported owning-object manifests are rejected.

### 3.2 RootSet and SnapshotLease

A RootSet holds the document base, pending-op projection, session state, native-edit mirrors, resource metadata, and UI derivation roots with their stamps. Publishing one RootSet pointer is the serialized semantic commit point. Scene/GPU residency may lag and is labelled separately.

`SNAPSHOT:ACQUIRE` increments a checked root reference and returns an owned lease. `SNAPSHOT:READ` returns a value valid only while that lease remains live; APIs return copied small scalars or owned record references for values escaping a call. A yielded job owns its lease. A new publication never mutates pages reachable from any published root, even if its refcount appears to be one. Only transaction-private pages are writable.

A shared-read token is cloneable by an explicit retain operation. The owning lease, candidate, and builder are linear nominal types in the public interface. The implementation uses opaque one-cell handles so multi-cell linear-local restrictions do not need to be worked around. The checker and admission verifier must validate these interfaces; the proposal does not assert that such new words already exist.

### 3.3 Transaction state machine

```text
Open(base lease, private allocations)
 -> Validating(readset, writeset, effects)
 -> Ready(reserved publication metadata)
 -> Published(new RootSet)
 or Aborting -> Aborted
```

A command validates input and permissions, records entity/field read revisions, and writes candidate pages. It reserves changed-key, operation, and effect metadata before publication. A failure before publication exposes neither state nor effects. Publication transfers ownership of the new roots and enqueues effect descriptions exactly once under CommitId. Effect execution occurs after publication; pure replay never executes that queue.

A suspended transaction keeps its base snapshot. Before publication, compare the current RootSet: if identical, publish; if changed, either restart the transaction or validate its complete readset and reapply its *semantic writes* into a new candidate. The baseline restarts general transactions; operation-specific rebase helpers may use the latter path with tests. Never splice an old candidate tree over an unrelated new root.

Undo of unpublished work destroys candidate ownership, not arbitrary memory rollback. A fatal trap discards the entire instance; a caught domain error cannot resume after an invariant failure.

### 3.4 Reclamation and OOM

Each immutable object has a checked u64 reference count. Overflow is fatal to that candidate/admission, not wraparound. Dropping the last reference places the object on an intrusive retirement queue; it does not recursively free descendants on the caller's stack. A reclaim job visits at most the declared edge budget per step, decrements children, then returns the slot to its pool after incrementing generation. The queue record is embedded in the object's header so freeing memory does not require allocating another unbounded list.

Use separate pools for immutable pages, transient transaction/build storage, job states, packets, and emergency diagnostics. Arenas reset only after owned values have moved or been copied. Diagnostic failure must not allocate arbitrary strings. Quotas are checked before cloning/reserving. Under pressure: cancel obsolete disposable jobs, evict unpinned caches, compact if headroom exists, or reject a new candidate with RecoverableOOM. Never evict a lease to satisfy a new command.

A paused reader test must retain old values across many publications and incremental collection. A failed allocation at every clone/split/publication step must leave the old root reachable. Peak accounting includes all retained root versions (§26), not just the live current document.

## 4. Scheduling, bounded execution, clocks, and backpressure

### 4.1 One entry, explicit jobs

The adapter serializes startup, ingress publication, STEP, and stop. A synchronous import may copy bytes or schedule browser work but must never call an export recursively. Promise completions become later events.

Jobs have this exact logical representation:

```text
JobHeader: Id, Scope, ScopeGeneration, Kind, Phase, Status,
           SnapshotLease?, Cursor, WorkRemainingEstimate,
           CancelRequested, OwnedInputs, OwnedPartialOutput
StepResult: Yield | Wait(RequestId) | Done(OwnedResult) | Failed(Error)
```

Kinds are concrete state machines, not saved arbitrary Wasm stacks. Required jobs include packet validation, record decoding, transaction/rebase, tree reconciliation, DOM snapshot planning, semantic page query, asset decode/hash, BVH construction/refit, path tessellation, checkpoint, migration, reclamation, and cleanup. Each kind lists bounded inner operations in its implementation table.

A step budget counts primitive work: one record/edge comparison, one bounded arithmetic operation, one byte-block hash/decode of at most 4 KiB, one B+ page search/clone, or one bounded callback. Costs are conservative declared weights. An operation checks sufficient credit *before* performing its bounded unit. Cancellation moves to a cleanup phase, which yields while releasing many children. Library parsing, sorting, hashing, and destructors cannot hide unbounded work inside one unit.

### 4.2 Callback admission, not fictional preemption

A ViewCallback profile allows an acyclic transitive call graph, statically bounded loops (upper bound at most 64 per loop in the base profile), closed callback tables, bounded string/record primitives, and at most 2,048 weighted operations per invocation. Unbounded recursion, arbitrary `execute`, raw global writes, clock/random imports, and host I/O are refused in that profile. A verifier calculates a conservative maximum cost from frozen HIR and the primitive cost contracts. An unknown bound is a compile diagnostic, not “probably fast.”

Collections use declarative EACH/QUERY descriptors whose enumeration is a runtime job. A component callback constructs at most 256 direct nodes and copies at most 16 KiB; larger static subtrees are compiled into chunked template descriptors. A giant user loop is not silently transformed into a resumable job. Application authors supply a job step for genuinely large custom computation. The same purity/cost admission applies to derived callbacks, key selectors, custom widget handlers and reducers' bounded phases.

Runtime fuel is a hard safety boundary, not a resumable yield. Exhausting it in an admitted callback aborts its unpublished candidate and indicates a cost-contract defect; repeated violation disables the module/profile. Ordinary budget yield occurs only at explicit job boundaries. Browser native calls, JIT pauses and GPU driver work do not have a strict wall-time guarantee; measure them and constrain the amount of admitted work.

### 4.3 Ingress and causal scheduling

The host validates fixed envelopes before copying. Bulk packets are at most 1 MiB, semantic packets 64 KiB, control packets 4 KiB in the initial profile. Ingress does not synchronously traverse a million nested elements: it stores a bounded owned packet and schedules a validator cursor. Records become visible only when that packet's required validation is complete. Emergency records use fixed bounded decoding. A per-channel head stays blocked behind its earlier unvalidated packet; different channels may proceed when their explicit dependencies permit.

The runtime assigns a total ingress ordinal to accepted events for replay. Per-producer event order is preserved. Class scheduling: emergency/terminal, semantic input and operations, interactive, frame planning, background/reclamation. Use weighted deficit round-robin with quanta 8/8/4/4/2, and promote a waiting nonterminal class after 32 scheduler rounds. Each round executes bounded units. Cancellation does not jump a pointer release before the motion needed to interpret it.

Coalesce only before sealing a packet. Hover is latest-wins; drag motion can be latest absolute sample; relative deltas aggregate only when the tool declares equivalence. Stroke tools preserve coalesced sample arrays. Never coalesce across pointer ID, capture, buttons, composition, tool generation, binding or layout epoch. Pointer-up contains final position and the gesture sequence.

### 4.4 Credits and liveness

Each directional transport lane has initial byte/message windows `Wb,Wm`; sender totals `Sb,Sm`; receiver cumulative release totals `Rb,Rm`. A new packet of size b may be sent iff `Sb+b <= Wb+Rb` and `Sm+1 <= Wm+Rm`. Counters increase only on accepted transport ownership. CREDIT reports absolute released totals and a lane generation; duplicate/older totals are ignored, inconsistent totals exceeding admitted sends are protocol errors. No additive grants.

Transport credit returns when packet storage is freed or ownership is transferred to a separately charged, reserved application allocation. Retained heap/resource bytes remain charged there. Copying cannot remove their memory cost. Cancelling a queued packet returns its transport credit exactly once.

A reserved control lane is not blocked by ordinary byte windows. It contains fixed-size credit, cancellation, terminal, stop and epoch records. Before accepting a request, reserve one terminal-result slot; large result bodies use ordinary credit and an owned result handle. If ordinary result transfer is blocked, a terminal record can report a fetchable result handle. CONTROL capacity covers the maximum live requests plus fixed emergency allowance. CREDIT coalesces, each cancellation has one outstanding bit, duplicate errors do not multiply records. Unsolicited terminal/control flooding closes the offending port, not unbounded queue growth. Local ports use reliable ordered delivery; corruption/gaps reset a scoped runtime/channel. We do not build a retransmission protocol inside a page.

Network receive credit is separate and negotiated with the server. Stop granting asset chunks before buffers fill; send queues check browser buffered amount. A browser socket alone does not provide the application's desired receive backpressure [S11].

### 4.5 Clock domain and frame demand

Bootstrap selects `ClockId=PageSessionId` and root time origin T0. Each agent converts a native relative timestamp t with its own origin Ti into `(Ti+t)-T0`. The host performs this conversion before emitting records; values remain f64 milliseconds in the root domain. This follows the platform's distinct time-origin model [S4]. Unknown-origin timestamps are diagnostic only. User event sequences, not timestamps, establish causality.

Timeout means elapsed host-clock time; it does not promise work executes while suspended. On resume, expired ordinary requests receive Timeout; durable operations enter outcome discovery instead. Replayed clocks are recorded inputs. Offline authorization is not secured by a manipulable wall clock (§22).

Frame pacing uses `FRAME-DEMAND(surface, continuous, demandGeneration)` and host `FRAME-TICK(surface, tickId, rootTime, deadlineHint, sizeVersion)`. Only one unconsumed tick per surface. Worker replies TICK-CONSUMED(tickId, Planned|Idle|Superseded). A new demand replaces older demand; hidden/zero-sized surfaces receive SURFACE-PACING(Hidden|ZeroSize) and no ordinary ticks. Reliable pick/export jobs do not depend on visible-surface animation ticks. They have independent fair service or explicit Unavailable outcomes. A main-thread rAF supplies the root-domain tick even when rendering executes in a worker, avoiding a mandatory worker-rAF feature.


## 5. Scopes, asynchronous tasks, and durable ownership

### 5.1 Scope lifecycle

There are Application, Document, Component, Gesture, Job, and Plugin scopes. Habu creates a local scope record first, requests `SCOPE-OPEN(parentHandle, kind, localId, authority)`, and receives a host scope handle. Until accepted, dependent host requests remain locally queued; rejection closes the local scope without sending them. Scope parents cannot form cycles. Component scopes descend from an application/document scope; durable operations belong to the document service, never a component.

```text
Opening -> Open -> Closing -> Draining -> Closed
              \-> Revoked -> Draining -> Closed
```

Closing refuses new requests, cancels cancellable children, detaches observers and drains late results. The host returns SCOPE-CLOSED only after owned handles are released or transferred to an explicitly live parent scope. A scope tombstone remains while completion tokens reference it. Generation reuse waits for the tombstone's retirement. Runtime death revokes the whole incarnation; persistent records remain discoverable under the document journal, not the dead handles.

### 5.2 Task contract

Each request has Id, scope/generation, authority, operation type, immutable arguments, deadline and result schema. States are New, Submitted, Pending, Terminal, Draining, Released. One terminal *task outcome* is delivered to its observer; that does not promise exactly-once remote execution. Cancel/complete races are serialized at the request owner. Once a terminal outcome is chosen, later results are drained and their handles released. A lost consumer never strands a newly produced host object.

JOIN waits for all children and owns their results; empty JOIN returns an empty result. RACE accepts the first declared-success or first terminal according to its explicit mode, cancels losers, and drains them. TIMEOUT detaches at expiry but preserves durable operation discovery. BOUNDED-MAP starts at most k jobs, k in 1..32, and yields between admissions. Every composite owns a small explicit result vector; large collections stream results through a bounded sink. Cancellation does not recursively traverse all children in one synchronous call.

Errors are Domain, Validation, Conflict, Permission, Unsupported, Cancelled, Timeout, Unresolved, Quota, OOM, Stale, Platform, Protocol, or Fatal. Habu catchable throws retain the full i64 throw code in the diagnostic record; wire ErrorClass is separate. Fatal/protocol invariant violations discard the affected instance/port; they are not caught as ordinary “invalid input.”

### 5.3 Durable operation versus UI observation

A click creates a document operation record before attaching an observer. The component gets `ObservationId`, which it may cancel on unmount. The document service keeps OperationId, journal transition state, receipt discovery and reconciliation. After worker failure, the replacement document service reconstructs them from storage. UI timeout or panel close never deletes the journal or prevents eventual outcome processing.

General computation and reliable GPU jobs also outlive an individual frame, but not necessarily their owner scope. They are not durable across a whole-page crash unless their application defines a journaled operation. The distinction is explicit in each command schema.

## 6. Reactive graph, consistency, and retry

A SourceKey is `(store, entity/id, field-or-query key)`. Source revisions are monotone within their owner epoch. Derived nodes contain descriptor/code-generation ID, owning component scope, cached value, status, successful-read edges, retry-read edges, and output revision. Status is Clean, Dirty, Running, Ready, FailedWithPrevious, or FailedEmpty. Reading a failure never returns the old value as silently current: the caller receives its stale/error state or chooses an explicit previous-value view.

An evaluation owns one SnapshotLease and a dependency scratch set. Reading a source records its revision. Reading another derived value computes/reads that value for the same snapshot identity; cache entries are keyed by relevant revision/readset, not only node ID. A yielded job resumes the same snapshot. On completion it may publish only if all captured dependencies remain equal; otherwise its result is cached for that snapshot or discarded and rerun. No mixed-revision derived result is published.

On successful evaluation, atomically install the new value and successful reads, clear retry reads, remove obsolete subscriptions, then notify downstream only when typed equality changes value or validity state. Scalar equality is schema-specific (floating NaN is disallowed in domain values); immutable objects use identity plus revision or a declared bounded comparator. A large array's equality is not hidden inside one callback.

On failure, preserve the last successful value and its successful dependencies, but replace retry dependencies with the union of reads performed during the failed attempt and explicit repair keys supplied by a typed error. Subscribe to both sets. Retry on a change to either set, explicit RETRY, descriptor replacement, or owning resource-generation change. Do not retry merely because a frame tick occurred. Each set has a 4,096-edge initial cap; exceed it with a TooManyDependencies error and explicit coarse query revision subscription rather than an incomplete readset. Repeated failures replace, not accumulate, retry edges.

Grey DFS visitation detects dynamic cycles and returns a cycle path. Every member in the detected strongly dependent running path gets a stable diagnostic and retry subscription to relevant external source changes; no self-triggered retry loop. Explicit RETRY can re-evaluate after code/data repair. Disposal unlinks both edge sets and drains running jobs. Reclamation and unsubscribe walks are jobs.

Incremental queries use indexes with query-revision keys. A filtered million-row list is not one view callback reading one million cells. A query job scans/paginates an index under a snapshot, returns a stable ordered logical collection, and the UI renders a bounded window.

## 7. Concrete Habu UI API and admission

### 7.1 Physical and nominal representation

All public owning runtime tokens are opaque one-cell nominal handles. Their internal records are private to the owning package. The source API does not expose a raw pointer that could mutate published state. Generated nominal types include `ui-read`, `ui-build`, `ui-props`, `ui-state`, `ui-description`, `ui-action`, `ui-binding`, `ui-collection`, `ui-resource`, `ui-scope`, and `rt-job`.

`ui-read` is a retained read-context handle owning a SnapshotLease and dependency recorder. `ui-build` exclusively owns scratch node records, copied strings, action payloads and an open-scope stack. `ui-props` and `ui-state` are immutable schema-tagged records. Read-only records may be explicitly retained; a linear builder cannot be duplicated. `Binding<T>` is implemented as generated monomorphic wrappers (e.g. `ui-text-binding`, `ui-quantity-binding`) around a private validated binding record, not runtime unchecked casts from an untyped pointer.

The exact Habu declarations for new nominal types must be generated using the repository's admitted type-definition mechanism. The following are specified stack interfaces for those new library words, not a claim that the library has been compiled. This is deliberate: a design can fix the representation without falsely labelling unimplemented words runnable.

### 7.2 Public stack contracts

In the following contracts, `ptr len` is a borrowed span valid only for the call. `n` is the existing Habu cell numeric type. `key` is a declared nominal UI key represented by a cell. `status` is a checked domain result, not the backend throw status.

```forth
RT:READ-ACQUIRE  ( root-set -- ui-read )
RT:READ-RETAIN   ( ui-read -- ui-read ui-read )
RT:READ-RELEASE  ( ui-read -- )
UI:BUILD         ( ui-scope -- ui-build )
UI:ROW           ( ui-build key -- ui-build )
UI:;ROW          ( ui-build -- ui-build )
UI:COLUMN        ( ui-build key -- ui-build )
UI:;COLUMN       ( ui-build -- ui-build )
UI:TEXT          ( ui-build key ptr n -- ui-build )
UI:BUTTON        ( ui-build key ptr n ui-action -- ui-build )
UI:FIELD         ( ui-build key ui-binding -- ui-build )
UI:CHILD         ( ui-build key component-id ui-props -- ui-build )
UI:EACH          ( ui-build key ui-collection component-id -- ui-build )
UI:WHEN          ( ui-build key flag component-id ui-props -- ui-build )
UI:VIEWPORT      ( ui-build key viewport-id -- ui-build )
UI:LAYOUT        ( ui-build layout-record -- ui-build )
UI:SEMANTICS     ( ui-build semantics-record -- ui-build )
UI:FINISH        ( ui-build -- ui-description status )
UI:ABORT         ( ui-build -- )
UI:DESCRIPTION-RELEASE ( ui-description -- )
```

ROW/COLUMN scope closers are specific checked library operations: a runtime scope stack records expected kind and sibling-key set. Static lint rejects obviously unbalanced constant scopes; dynamic conditions are checked at FINISH. A wrongly nested closer marks the candidate invalid. The builder remains owned for cleanup and later calls become bounded no-ops. FINISH consumes it and returns either a valid description plus Success, or a null description plus a typed BuildError. No half-tree is published. Closing a scope is not permitted to drop a linear child/resource silently.

TEXT/BUTTON string spans have a 4 KiB call limit. Static long content uses compiled assets or resource bindings. Strings and action payloads are copied/owned before returning; no scratch pointer survives. Direct children are capped at 256. EACH stores a collection descriptor and row-component ID; it does not execute an unbounded iteration in this call. Row components receive owned immutable props containing semantic item identity, field revisions, and view flags.

### 7.3 Component descriptor and lifecycle

```text
ComponentDescriptor:
  stable type Id128, version:u32, codeGeneration:u64
  propsSchema, stateSchema
  initHandler, viewHandler, eventHandler, disposeHandler
  admittedEffects, maximumCallbackCost, migrationHandler?
```

Handlers are typed noncapturing functions. `view` has logical contract `(read, props, state, build) -> (read, build)`; props/state borrows are consumed/released by the dispatcher after the call. `event` receives explicit immutable environment and a transaction builder, never access to the native DOM. Init/dispose return declarative effect/cleanup descriptors processed after state publication. No synchronous host effects occur while a view is built.

ComponentInstanceId is allocated once. Normal keyed children are found by `(logical owner instance, semantic key, compatible type)`. PlacementId separately names where the instance is mounted. RELOCATE explicitly transfers placement; it does not create a new logical parent or destroy state. Changing owner scope requires explicit TRANSFER with both owners live and no unauthorized data crossing; ordinary docking changes only placement.

Local state fields use declared schema keys, not call-order hooks. `STATE:READ` records a dependency; mutation occurs through a component/session command. A hidden conditional branch defaults to Suspend (preserve state, pause view/resources), with a declared Dispose option. WHEN's branch key distinguishes unrelated panels. A suspended component has a bounded residency lease; eviction is an explicit remount policy, not silent state reuse.

### 7.4 Actions, bindings, and resources

An Action is `(commandId, schemaVersion, immutable payload, owner, bindingGeneration)`. It is data, not a closure. A domain target is resolved during binding and copied as EntityId/FieldId. A later selection change cannot retarget the action. Dynamic availability is checked again when dispatching.

A Binding is `(FieldKey, acceptedRevision, typed Value, FormatId, ParserId, ValidatorId, CommitCommandFactoryId, EditPolicy, authority, formMembership)`. Commit factory is a closed typed descriptor receiving explicit owned data. It cannot mutate the store directly. Formats/parsers are versioned application data; native host formatting never defines engineering truth.

Resources have Idle, Loading, Ready, Refreshing(previous), Failed states. Starting work is a command/effect, not a view side effect. The resource descriptor owns its request generation and optional previous value. Component disposal cancels observation and cancellable work; shared content loads may be retained by other consumers. Late results match scope/generation before use.

### 7.5 Purity and cost verifier

Add a verifier after checked HIR freeze, before backend lowering. Each operation/callee carries one of ReadSnapshot, WriteFresh(region), WriteTransaction, EmitCommittedEffect, HostCapability, NondeterministicInput, or RawGlobalAccess. Region identity is nominal and flows through the checked builder/transaction descriptor. A view admits ReadSnapshot and WriteFresh for its own builder only. It cannot obtain a transaction token, call clock/random, evaluate source, or use a raw store. Unknown call targets are rejected; dynamic descriptors list closed candidate tables with identical effect/cost bounds.

The verifier checks constructor provenance of opaque tokens, prevents integer-to-token casts, and disallows exports of private mutation primitives. Whole-program symbol/primitive admission catches a transitive global write even if the public stack effect looks read-only. It is an additional verifier with an explicit trusted primitive table, not a claim that existing Habu stack checking already proves purity. Do not add TRUST wrappers. Negative tests must fail at the actual checker/verifier boundary.

### 7.6 Reference application with nontrivial data flow

The reference workbench consists of these registered descriptors, all using the interfaces above:

| Component | Props/state | View and events |
|---|---|---|
| Workspace | document ref; active panel ID | toolbar, tree, viewport, properties, status; changes placements |
| AssemblyTree | subscription ID; filter draft, expanded ID set | QUERY resource + EACH virtual window; SELECT action copies EntityId |
| PartEditor | PartId + entity revision; FormId, conflict state | text/material/quantity bindings, conditional constraint fields, Apply/Cancel |
| MaterialChooser | catalog resource; query/active option | asynchronous closed-schema results, stale generation guards |
| OperationStatus | OperationId | observes document-owned operation; unmount only detaches observer |
| ConstraintPanel | PartId; expanded state | conditional CHILD with persistent key, no accidental state transfer |
| Viewport | logical viewport, document | tool state + frame demand; emits selection/preview commands |

Example composition (new API; no implicit captured locals):

```forth
package WORKSPACE
public
: CHROME ( ui-build -- ui-build )
    K-ROOT UI:COLUMN
      K-TOOLS UI:ROW
        K-OPEN s" Open" ACTION:OPEN UI:BUTTON
        K-UNDO s" Undo" ACTION:UNDO UI:BUTTON
        K-REDO s" Redo" ACTION:REDO UI:BUTTON
      UI:;ROW
    UI:;COLUMN ;
;package
```

`ACTION:OPEN` constructs/returns an owned zero-payload Action; it is not a quotation referring to local variables. For a row, the generated adapter reads explicit row props, constructs `SelectEntity{entityId,expectedRevision}`, and hands that Action to BUTTON. The row adapter releases props and no borrowed pointer escapes. Descriptions use locale message assets instead of these literal English strings in production.

A full editing trace, not merely a pretty static DSL:

```text
AssemblyTree QUERY completes for query generation 12 under snapshot S.
EACH schedules visible rows; each row props includes EntityId, not row index.
User activates row E -> SelectEntity(E) session transaction.
PartEditor receives immutable props for E; three FIELD bindings are installed.
MaterialChooser emits SearchMaterials(query, generation 3) as a committed effect.
Generation 2 result arrives -> drain; generation 3 result -> Resource Ready.
User edits width/offset; host retains native drafts.
Apply -> one FORM-SNAPSHOT containing both current binding/session sequences.
Parser validates units, validator checks coordinated geometry constraints.
One SetPartitionParameters operation is constructed and journaled.
PartEditor may unmount; OperationStatus/document service retains the operation.
Server canonicalizes values; ordered event updates accepted projection.
Host receives FIELD-STATE for idle fields; newer active drafts become conflicts.
```

Compile gates cover props mismatch, escaped builder pointer, duplicate keys, unbalanced scope, raw global mutation, hidden nondeterminism, arbitrary callback, use-after-dispose and linear-token duplication. The external reference package includes a fixture specification; claiming actual Habu compilation requires running those fixtures after implementation.

## 8. Retained UI, bounded DOM publication, and relocation

### 8.1 Trees and reconciliation

Keep desired UI, acknowledged UI, and at most one ordinary patch in flight per commit region. A commit region is a bounded physical subtree controlled by one root controller; logical components may reference portals by PlacementId. Desired changes arriving during a patch update the next target, not the patch's base.

A reconciler job traverses dirty component descriptions with an explicit cursor. Match stable keys and compatible types; duplicate sibling keys reject the candidate. Use hash maps with full-key comparisons. First implementation emits correct linear keyed moves; optional LIS minimizes moves without changing identity. An operation touching k children costs O(k) plus bounded map work; an index maps node ID to parent/position for changes. Do not rescan the whole application for a label.

Create nodes off-tree, set properties, bind behavior, then attach. Retire actions only after the host event fence is acknowledged and consumed. DOM inputs and patch acknowledgements from the main host share one ordered port. Input carries explicit TreeEpoch. Old descriptors remain resolvable long enough to reject/revalidate an old action, not reinterpret it as the current row.

### 8.2 Ordinary bounded patches

A patch carries RegionHandle, expected UIStamp, next revision, operation list and optional InteractionFence. Defaults: at most 256 DOM mutations, 64 KiB payload, 1,024 properties. Host prevalidation is incremental against a shadow copy; side effects begin only when complete. Apply one admitted bounded region without yielding inside its mutation list. Browser work is measured, not promised to be constant-time.

If a logical update spans regions, publish desired state once but allow presentation to converge region by region. Commands always revalidate current domain state. Features requiring coordinated form bindings use an InteractionFence that temporarily disables only those related actions until every named region ACK arrives. This is not global atomicity over DOM/GPU.

Failure after touching DOM marks only the affected region Desynchronized, disarms its policies, records recoverable drafts, and enters staged recovery. Do not emit Applied for a partial mutation. The static shell remains usable even if the application root is unusable.

### 8.3 Staged snapshot protocol

```text
SNAPSHOT-BEGIN(region, expected active stamp, candidateId, quotas)
 -> SNAPSHOT-CHUNK(candidateId, sequence, detached node/property records)*
 -> SNAPSHOT-SEAL(candidateId, rootId, completeHash)
 -> SNAPSHOT-READY(candidateId, measured counts)
 -> SNAPSHOT-ACTIVATE(candidateId, expected active stamp, editHandoffs)
 -> SNAPSHOT-ACTIVATED(new TreeEpoch, revision=1, eventFence)
 or SNAPSHOT-ABORT / SNAPSHOT-FAILED
```

Staging holds disconnected nodes, a candidate shadow table, bind/policy descriptors not yet armed, bytes and node charges, and a deadline. Chunks are individually bounded and idempotent only for the same sequence/hash within the live candidate. Full candidate validation verifies connectivity, unique identities, no cycles, allowed properties and placements, and compatible edit handoffs. Failure/timeout queues incremental destruction; no candidate action can fire before activation.

Active content remains unchanged during staging. Activation is capped at one bounded commit region (initial 2,048 live nodes; virtualized content uses smaller regions) and swaps only after edit leases are resolved. A larger workspace rebuild uses multiple explicit regions with a recovery/loading presentation and an interaction fence. No unbounded root replacement is advertised as a 2 ms operation. CSS/layout latency is measured separately.

An active editor absent from the candidate must receive an approved lease end/handoff or block activation with EditBusy. A region in an irrecoverable tree can preserve the old native input in a host recovery island until adoption; raw original node identity does not authorize new application actions.

### 8.4 Editor-safe relocation

A RELOCATE names ComponentInstanceId, old/new PlacementId, expected placement revision and an edit policy. If the native node has no edit lease, move normally. With an edit lease, use a browser-qualified state-preserving move only when its exact source/destination constraints are satisfied; otherwise defer relocation or explicitly commit/cancel the edit before moving. Ordinary remove/reinsert plus text/caret restoration is not claimed equivalent to native IME/undo [S5].

Docking a live editor therefore may show a pending dock preview until composition ends. Filtering a focused row pins the instance in an edit island or ends editing under a visible policy; it never recycles that input into another entity. A target's actual deletion terminates its lease with TargetDeleted and preserves a recovery draft unless privacy policy forbids retention.

## 9. Fields, native controls, composition, and form snapshots

### 9.1 Stable field and accepted-state messages

`FieldKey` consists of NamespaceId, DocumentId (zero for session-only fields), EntityId, FieldOrdinal and SchemaVersion. A BindingId is runtime-local and immutable for one FieldKey; generation changes for a new binding contract. Never change its FieldKey in place.

`FIELD-INSTALL` associates node, BindingId/generation, FieldKey, ControlKind, accepted revision/value, parser/format IDs, permissions, edit policy and FormId. `FIELD-STATE` updates accepted revision/value on that same binding. There is no generic `initial-value` property in v2.

Host states: Idle, FocusedClean, Editing, Composing, AwaitingSubmit, Conflict, Ending, DetachedRecovery. An Idle field accepts a newer FIELD-STATE and updates native value even after previous sessions have ended. A clean focused field may accept the update only after ending its old lease and starting a fresh clean lease, recording the transition. A dirty/composing field retains draft, stores new accepted state separately and emits Conflict. Field labels/disabled presentation may change without replacing its draft; permission revocation explicitly terminates editing.

Typed ControlValue is Text, Boolean, Mixed, OptionId, OptionIds, NumberWithUnit, or Empty. Options are stable IDs, not DOM indexes. Radio groups share an explicit GroupId; select/radio/checkbox/range observation uses CONTROL-OBSERVED with observed value and edit sequence. Browser input/change events are deduplicated by actual value/state comparison; click and keydown do not issue a second semantic toggle. The runtime accepts/corrects the observed value with CONTROL-ACK/CONTROL-CORRECT and explicit sequence matching.

### 9.2 Lossless edit snapshots

Native draft strings are encoded as **UTF-16LE code units** in `DraftText`, including temporary unpaired surrogates. The protocol validates even byte length and selection bounds but does not silently replace code units. This is separate from strict UTF-8 `String` used for accepted text, labels and identifiers. Commit conversion rejects ill-formed surrogate sequences with a field error while retaining the original draft. No automatic Unicode normalization is applied to engineering identifiers or source text; search may have a separate normalized index.

```text
EditSnapshot:
  BindingId, BindingGeneration, FieldKey
  SessionHandle, EditLeaseHandle, BaseAcceptedRevision
  EditSequence:u64, DraftText, SelectionStart:u32, SelectionEnd:u32
  SelectionDirection:{None,Forward,Backward}
  Composition:{Inactive,Started,Updating,Ended}
  Dirty:bool, NativeControlValue:ControlValue?
```

EditSequence increments whenever text, selection, composition or typed control state actually changes. Selection-only changes are observed through selectionchange/select/click/key effects as appropriate; duplicate identical observations do not increment it. Composition start/end are barriers even if final text is unchanged. Browser composition and input are not interchangeable event classes [S2, S3]. Native editing remains the immediate authority for the draft.

Snapshots may coalesce within one session between barriers, but a commit/cancel/blur capture includes the latest entire snapshot. Enter with `isComposing` or an active native composition session does not submit; it remains an IME event. Capture actual control state after input, not reconstructed key presses.

### 9.3 Correction and end outcomes

`EDIT-ACK(session, sequence, validity, errorId)` changes validation presentation only. `EDIT-CORRECT` carries expected exact binding/session/sequence and a replacement snapshot. Host returns one EDIT-CORRECTION-RESULT: Applied(newSequence), Stale(currentSequence), Composing(currentSequence), Missing, or Ended. A composing correction is **not** stored for automatic later application; the worker re-evaluates after composition ends and sends a new exact-sequence request. This eliminates a delayed destructive correction queue.

`EDIT-END` similarly returns Ended or Stale/Composing/Missing. Success names outcome CommittedLocally, Cancelled, AdoptedRemote, TargetDeleted, Revoked or HandedOff, and the accepted value revision. A newer draft is never erased by an old end. Assignment of corrected text can change native undo behavior; baseline automatic corrections occur only on explicit commit/accept-remote, not every keystroke. During a session Ctrl/Cmd+Z remains native draft undo. Document Undo is separate outside the native edit scope.

### 9.4 Whole-form capture

Forms have FormId/generation, stable member BindingIds, a membership revision, baseline accepted revisions, validation rules, and a named commit command. The host records association using a real form or managed equivalent and suppresses accidental browser navigation.

On an admitted submit gesture, first drain pending editor observations, then synchronously capture every member's actual state into one FORM-SNAPSHOT with SubmissionId, form/membership generation, edit-sequence vector and full drafts. Maximum 128 fields and 64 KiB raw draft bytes in the base coordinated-form profile. Larger workflows use explicit subforms or a separately admitted larger capture budget; no implementation silently truncates a form.

If any member is composing, return FORM-BLOCKED(Composing, binding list); do not infer that blur committed composition. The user submits again after composition. Snapshot captures only native state; expensive parsing/validation runs in the worker. Additional typing may continue after capture. The submitted vector is immutable.

The worker verifies membership/bindings/authority, parses all fields, validates cross-field invariants under one document snapshot, and constructs one command. A FORM-RESULT names the captured SubmissionId and sequence vector. For a field whose current sequence still equals the submitted sequence, the host can end/mark accepted. Newer drafts remain dirty based on their original target and receive updated accepted state/conflict indication. No “Apply succeeded” response clears subsequent typing.

Reset restores the form's latest accepted values using exact-session corrections; composing fields block reset unless the user explicitly chooses terminate composition and discard. Cancel of a form cancels drafts/observations, not a document operation already journaled. Async field/form validation includes submission/edit generation; old validity cannot clear newer errors.

### 9.5 Native draft vault and worker recovery

The main thread stores a bounded draft vault keyed by FieldKey, schema, base value revision, authority, last snapshot and optional live native-node lease. Initial ceiling 2 MiB/256 drafts; active inputs are charged before editing. Password/secret fields are never vaulted or persisted. Entries are marked Dirty, Adoptable, Orphaned, or Revoked. On size pressure, block new recoverable edits or prompt export/discard; never silently evict an active dirty draft.

After worker failure, revoke old bindings/gesture policies, keep live editing inert but recoverable, and advertise DRAFT-OFFER entries to the new worker. It validates permissions and target existence, then DRAFT-ADOPT maps the old stable target to a new BindingId/session and returns a new lease; DRAFT-REJECT gives a visible reason. Live-node adoption preserves native state when valid; reconstruction preserves only text/selection and is explicitly not native undo/IME restoration. Deleted-target drafts remain exportable only under policy. Old runtime handles never become valid again.

Optional encrypted draft journaling protects recovery across page loss under §22's offline policy. Otherwise vault survival is page-local only; the UI does not label it durably saved. Main-thread DOM desynchronization does not automatically delete the vault.


## 10. Native interaction adapter, focus, and user activation

### 10.1 Closed host machine

Habu installs `InteractionModel` records. Each contains scope, node/placement binding, policy version, allowed event classes, focus graph, bounded local transition kind, default-action policy, and invalidation guards. Supported transition kinds are NativeControl, RovingFocus, MenuChain, DialogScope, PointerCapture, SplitterPreview, VirtualExtent, and PreparedGesture. There is no host bytecode, arbitrary expression evaluation, callback string, or “execute JS” record.

The host may change only provisional platform state declared by the model: focus, active-descendant, open/dismiss presentation, pointer capture, a clamped splitter variable, native editor values, and validity of prepared actions. Every such change emits a normalized outcome. Habu accepts/reconciles it; it remains the semantic authority for commands and document changes. A policy update with the wrong binding/scope generation is rejected before changing behavior.

### 10.2 Event ordering and canonical activation

For a native event: identify current scope/model; increment relevant intent counters; disarm guards affected by those counters; perform permitted immediate platform transitions; capture latest editor/control state if required; enqueue normalized events in observed causal order. Native `click` is the canonical button activation. Keyboard policies either let the native click occur or suppress it and emit one semantic shortcut activation, never both. Checkbox/radio/select/range use observed value transitions, not synthetic double toggles.

Pointerdown on a popup option marks a bounded selection intent; the associated blur is held until that activation resolves or pointercancel ends it. The host emits the option selection plus final field snapshot as one ordered interaction bundle. This is not arbitrary delayed blur: one pending pointer intent per popup, cancelled on capture loss, timeout, removal or authority change. Form/member actions can request ResolveDraftBeforeAction; Habu validates the captured draft and either commits that change plus the intended session action or refuses the action. A destructive document action never runs merely because blur happened first.

### 10.3 Focus and dismissal

Focus models enumerate focusable semantic IDs, current roving member, orientation, wrapping, Home/End behavior, typeahead policy, active-descendant target and restoration PlacementId. Arrow traversal among mounted elements happens synchronously on main. Habu receives FocusChanged with FocusEpoch and IntentGeneration. An asynchronous focus request includes expected FocusEpoch; a newer user action causes a Stale outcome.

A focus scope has ParentScope, Modal flag, InitialTarget, RestoreTarget, DismissPolicy and EscapePriority. Escape goes first to composition/native editing, then innermost open popup, then active tool cancellation, then modal scope. Browser/system shortcuts remain native unless a declared nonconflicting app shortcut is installed. Default platform-reserved navigation/reload/devtools shortcuts are not captured by the base profile. Key versus code matching is explicit; printable text always comes from native input.

Native dialog show/close/cancel events are observed. The host applies modal/inert behavior to the managed siblings, not arbitrary outside-page elements; nested modals form a stack. Opening/closing emits a semantic Dismiss/Open outcome with model generation. Restoration targets must still exist and be eligible; otherwise use the nearest surviving focus scope root. Portal PlacementIds preserve logical owner scope and focus relationships across physical DOM roots.

### 10.4 Pointer/touch arbitration

Each viewport declares touch-action before gesture start. Base CAD policy uses `none` only on its canvas; surrounding panels keep native scrolling. Pointerdown chooses one tool/capture owner; additional touches either join that tool's declared gesture or cancel the first gesture and begin an explicit multitouch navigation state. No silent dual ownership. Capture is acquired/released synchronously where permitted and reported. Pointercancel, lostpointercapture, node retirement, visibility loss, and authority revocation reset pressed/capture assumptions. Pen pressure/tilt and optional sample arrays remain typed inputs; a stroke sampler declares maximum samples/second and resampling error.

Pointer lock/raw unaccelerated input is not part of baseline controls. It is an optional capability with its own activation/result/revocation rules; camera navigation works without it. Synthetic automation may submit permitted semantic tool commands but cannot fabricate native capture or user activation [S6, S7].

### 10.5 Prepared privileged actions

Host maintains intent counters per scope: SelectionIntent, EditIntent, NavigationIntent, FocusIntent and AuthorityIntent. A Habu prepared action binds an expected vector and a declared invalidation mask. The node model says which local native events increment which counters. Any potentially selection-changing viewport/tree pointerdown invalidates SelectionIntent immediately, before a worker reply. Text change invalidates EditIntent. Tenant/logout invalidates all.

`GESTURE-PREPARE` carries action ID, preallocated request ID, operation, owned payload, authority, binding generation, expected intent vector, invalidation mask and expiry in root-clock units. The host copies payload and reserves terminal capacity. On a matching actual gesture, it checks native trust/activation and grants, consumes the policy one-shot **before** invoking the platform operation, then emits GESTURE-STARTED. Habu adopts that request; it never launches a duplicate. Failure emits GESTURE-RESULT with Denied, ActivationRequired, StaleIntent, Cancelled or PlatformError.

An action disarmed by local selection cannot copy/export the previously selected entity. It displays Busy/PrepareAndClickAgain until refreshed. Save may open a picker immediately and stream generated data later using its authorized writer. Copy requires a current prepared payload; it does not perform an asynchronous worker round-trip and assume activation survives. Browser activation cannot be minted by a packet flag [S6].

## 11. Layout, docking, portals, virtualization, and routes

### 11.1 Layout representation

Layout is typed intent: normal/flex-row/flex-column/grid/overlay, logical inline/block size with Auto/Px/Percent, min/max, grow/shrink, gaps, padding, align/justify, overflow, grid tracks and placement, and visibility. Grid tracks use Fixed, Fraction or MinMax; children have row/column/span/alignment. Lengths must be finite; negative sizes/gaps are rejected; grow/shrink are nonnegative. The host maps to reviewed CSS classes/custom properties. No arbitrary selectors, CSS text, external URLs, or font injection.

Habu owns intent; browser CSS owns actual layout. Measurements carry UIStamp, LayoutEpoch, ScrollEpoch, FontEpoch, CSS content rect, backing size, direction and visual viewport. ResizeObserver reports are inputs, not permission to write layout recursively in the observer callback [S12]. Apply changes in a later task; suppress differences below 0.25 CSS px; after eight alternating layout states in one cause chain, freeze the last stable intent and report LayoutOscillation. This is a diagnostic fallback, not a proof CSS always converges.

Zero-sized/hidden surfaces retain scene state but do not allocate zero-sized GPU attachments or request ordinary frames. Backing size is round(CSS size × admitted render scale), clamped to device dimension and pixel limits. Picking uses actual backingWidth/CSSWidth, not an assumed DPR. v2 baseline surfaces are axis-aligned; arbitrary transformed canvases require an explicit inverse-transform profile.

### 11.2 Docking and splitters

Dock tree node is Split(axis, ratio, children) or Tabs(activePlacement, ordered placements). Ratios are clamped against child minimums; impossible layouts use overflow with a visible reset action, not negative dimensions. Dock state belongs to user/session preferences, not collaborative engineering history. Dragging creates provisional placement geometry, then one layout command on drop. Keyboard commands move panel, reorder tab, split region and reset layout.

The host splitter model receives axis, start coordinate, initial ratio, min/max, layout epoch and CSS variable ID from a closed registry. It updates only that variable during native motion; emits absolute provisional ratio and final commit/cancel. Habu accepts/corrects it under matching gesture generation. Escape restores baseline ratio. An intervening layout epoch change cancels/restarts rather than applying an old pixel delta. Editor-safe placement rules in §8.4 govern final docking.

### 11.3 Anchored portals

Popup anchor includes target PlacementId, preferred side, align, offset, collision padding, flip/shift flags and size limit. Host computes candidate rect from current measured anchor: try preferred side; flip to opposite if it fits more; clamp within visual viewport with padding; cap popup dimensions and enable scrolling. Ties choose preferred side. Recompute after anchor/viewport/font changes in a coalesced task. The host reports the actual placement epoch; Habu does not synchronously query DOM.

Escape/outside-pointer/focus-loss dismissal is a closed policy. A pointer event originating in any registered descendant portal counts as inside. Destroying the anchor dismisses its popups and resolves focus restoration. Active input overlays use axis-aligned projected anchors; if their 3D anchor goes offscreen, retain/freeze the native input with an explicit offscreen indicator or end the edit, not retarget to a different label.

### 11.4 Large collections and scroll authority

Logical collection stores stable IDs, order revision, total-known flag, estimated heights, loaded pages and a query subscription. Fixed rows use arithmetic. Variable rows use a Fenwick/prefix-sum tree over measured/estimated heights; insert/delete uses a chunked order tree, with a Fenwick tree per 256-row chunk and a prefix tree over chunk totals. Index-to-offset and offset-to-index are logarithmic in chunks plus bounded local work. Heights are measured under FontEpoch/locale/density; invalidating them changes estimates incrementally.

Choose a visible window and overscan from estimated scroll velocity × recent p95 worker service latency plus one viewport, capped by the node budget. Native host knows the current virtual extent and mounted interval. A jump outside it immediately shows a fixed neutral placeholder/extent surface and requests a new window; it does not recycle stale rows into new logical indices. The host keeps native scrolling smooth while Habu computes actual rows.

The virtualizer owns anchoring; set `overflow-anchor:none` on managed virtual areas to avoid competing adjustments. Anchor is `(ItemId, offsetInsideItem, ScrollEpoch)`. A height/order update preserves that item when still present, otherwise the next surviving neighbor. Apply an anchor correction only if no later native scroll changed ScrollEpoch. User input wins over stale corrections.

Focused/edited rows are pinned outside the recycled window, initially at most four per collection. Offscreen navigation is a task: resolve ItemId/index, request/load page, mount new window, wait for DOM ACK, then conditional focus. Unknown total maps to explicit unknown semantics; an empty known collection is distinct. Built-in search/filter covers unmounted content; find-in-page is not advertised as a whole-collection search.

### 11.5 Routing, closing, and exports

Routes are typed `(routeId, documentId?, entityId?, view, query)` decoded from untrusted input. Encode/decode round-trips canonical paths; history push is only for an app navigation, never in response to the same popstate event. A deep link to an unloaded entity creates a resolve task and placeholder; NotFound/Denied remain explicit.

Navigation intent first resolves dirty drafts under Stay, DiscardDrafts, or CommitAndNavigate. Pending *durable* operations continue in their document service or restore journal; leaving a panel is not cancellation. On browser Back/Forward while blocked, the host restores the current history index using a suppression token and shows the guard; after decision it applies the saved target exactly once. Whole-page unload warning is best effort; journaling happens during normal use, not at unload.

Screenshot export is a reliable render/export job with pinned scene inputs, size and pixel budget, then an image/download capability. It is labelled an approximate viewport image. Exact drawing/print/CAD export goes through an application command with canonical geometry, fonts and units; it is not generated by pretending the display tessellation is exact. Printing ordinary UI uses a prepared print-layout descriptor and native print action; confidential content obeys export policy.

## 12. Widget machines and host/Habu division

All widgets share scoped identity, disabled/read-only policy, FocusEpoch, native-event deduplication, error boundaries, and explicit cleanup. The tables below are the behavioral specification; a widget is not complete because its CSS looks correct. Accessible states are updated with the same transition as the visible state. Label strings come from the locale catalogue.

### 12.1 Basic controls and editing

| Widget / states | Event and immediate host transition | Habu transition / terminal rule |
|---|---|---|
| Button: Idle, Pressed, Busy, Disabled | pointer/key native activation gives one click; native focus/pressed feedback | revalidate Action, create command/task; Busy may block repeats under explicit action policy; unmount only detaches observer |
| Toggle/checkbox: Off, On, Mixed, Disabled | observe actual native checked/indeterminate after input; Mixed first toggle becomes On; deduplicate change | CONTROL-OBSERVED carries typed value/seq; accept/correct exact seq; conflicting remote update does not target new binding |
| Radio group: None/Selected(id), Disabled members | arrows/Space follow installed group model; only one eligible selection; preserve stable option IDs | submit one group value; no per-radio duplicate commands; removal chooses no selection until policy validates a replacement |
| Select: Closed, NativeOpen, Pending, Disabled | native choice maps option value to registered Id128; multi-select returns sorted-by-option-order IDs | validate selected IDs still in option set; stale option -> conflict/rebuild, not reinterpret index |
| Slider: Idle, Dragging, KeyboardPreview, Pending | native absolute value clamped min/max/step; repeated keys update preview; pointer capture | one grouped commit on release/Enter/blur according to policy; Escape restores accepted value; domain quantity still parsed/validated |
| Text/quantity/textarea | §9 edit/composition states; native undo and soft keyboard | explicit parser and edit policy; multiline Enter inserts newline unless a declared modifier submit; no document commit during composition |
| Progress: Idle, Running, Cancelling, Done, Failed | accessible progress min/max/current or indeterminate; cancel activation once | task cancellation may be pending; completion shown only on actual typed terminal result, not a sent request |

Buttons default `type=button`. A form's submit button explicitly has submit semantics and is routed only through FORM-SNAPSHOT. Disabled controls cannot install a privileged gesture even if a stale action remains cached. Read-only allows focus/copy but not value change.

### 12.2 Combobox/autocomplete

State is `(Closed|Open, Idle|Loading|Ready|Error, queryGeneration, activeOptionId?, selectedId?, dirtyDraft, composing, popupIntent?)`.

| Event | Defined transition |
|---|---|
| Focus | preserve draft; open only if policy says open-on-focus; request current generation if needed |
| Text edit | increment queryGeneration after native snapshot; invalidate prior option action bindings; schedule debounced search in component scope |
| ArrowDown while Loading | open popup, set desired first-option navigation; no fabricated option; when matching results arrive activate first eligible option |
| ArrowUp/Down/Home/End when Ready | host moves active descendant among mounted eligible options; reports intent; offscreen option mounts via virtual task |
| Enter while Composing | handled by IME, no option acceptance |
| Enter with valid active option | capture latest draft, choose that exact option ID/generation; keep newer draft protected by seq check |
| Enter without valid option | free-text commit only if binding explicitly allows it; otherwise validation feedback |
| Pointerdown/click option | bounded popup intent prevents blur from separately committing old text; ordered choice bundle resolves once |
| Escape | close popup first and retain dirty query; second Escape follows field cancel policy |
| Blur outside registered popup | close; commit/cancel according to binding policy using final snapshot |
| Old search result | release result handles; never change active option of a newer query |
| Current search error | show error/retry row not a selectable material; retain draft |
| Unmount/target deleted | cancel search observation, close popup, follow edit-lease termination policy |

### 12.3 Menus, toolbars, palette, tabs and disclosures

Menus form an owned chain. States Closed, Open(activeItem), SubmenuPending, Invoking, Dismissing. Opening takes a placement/focus lease and snapshots available action bindings. Arrow navigation/typeahead is host-synchronous over eligible items; Right/Left open/close submenus according to direction. Escape closes only the innermost menu and restores focus. Outside pointer dismisses after classifying descendant portals. Activation dispatches exact Action; availability revalidation may refuse with feedback. Async action completion is not required to keep a menu open; its durable task belongs elsewhere. Pointer hover submenu open delay is a host timer (initial 150 ms), cancelled on focus/intent generation change.

Toolbar is roving focus without popup ownership. Command palette combines a modal scope and generation-tagged searchable command collection. Search results carry command IDs and fixed arguments, not source strings. Enter activates only the current generation; Escape closes; unavailable commands remain labelled or omitted consistently.

Tabs use SelectedTabId and focus tab separately. Base policy is manual activation: arrows move focus; Space/Enter activates. Fast fully local tab panels may declare automatic activation. Selection is never inferred from array index. Removing an active tab chooses the nearest surviving tab in old order and emits a session command; panel state survives hiding unless Dispose was declared. Accordion/disclosure toggles expanded state; its button exposes expanded/controls. Collapsing a subtree containing a dirty editor requires the explicit keep-hidden-edit or resolve-edit policy.

### 12.4 Dialogs, popovers, tooltip and toast

Dialog states Closed, Opening, Open, Closing, BlockedByDraft, Failed. Open creates native scope and chooses initial focus after mount ACK. Native cancel/outside policy emits DismissIntent; Habu may block close for unresolved drafts. While closing is blocked, native state remains open. Success releases scope and restores focus conditionally. Nested dialogs use the focus stack; removing a parent closes descendants in reverse order, draining their tasks.

Popover is anchored but not necessarily modal; its dismissal policy explicitly names outside-pointer, focus-leave and Escape. Tooltip has Idle, Delay, Visible states; pointer/focus starts a timer, Escape dismisses, and it never contains the only way to access an essential action. Hover loss cancels delay. Toast has queued/showing/dismissed states, bounded queue (initial 8 visible/pending), severity, announcement policy and optional action; it never steals focus. Progress/error state persists elsewhere if a toast expires.

### 12.5 Tree, list, data grid and property grid

Tree state stores focused ItemId, selected ID set, expanded set, anchor ID and load generations. Right expands or enters first child; Left collapses or moves to parent; Up/Down traverse currently logical visible order; Home/End navigate boundary with an explicit unknown-total behavior. Expanding unloaded children yields Loading; collapse cancels the observation, not shared cached data. Shift range selection resolves IDs under an order revision; a changed order forces revalidation. Select-all is an application command over the declared scope/filter, not selection of just mounted DOM rows.

Grid state stores active `(RowId, ColumnId)`, selection model, edit mode and sort/filter generations. Navigation mode arrows move cells. Enter/F2 begins cell editing and native input owns text keys. Escape ends edit before returning to navigation. Tab commits/resolves draft then moves to the next eligible cell; validation failure keeps focus and announces the field error. Copy of a range is a prepared export task with explicit selected ID/order revision; native input copy remains native. Pasting a tabular range is parsed under bounded dimensions and validated as one multi-cell command or returns per-cell errors without partial undocumented acceptance.

Property grid uses schema field IDs; mixed multi-selection values have Mixed state, not the empty string. Editing a mixed value creates a command listing explicit target entities, a permission/precondition check for each, and an all-or-nothing application policy unless a separately named partial-apply operation was chosen.

### 12.6 Virtual focus and accessibility synchronization

Never point aria-activedescendant to an unmounted/recycled node. While mounting a requested offscreen active item, keep the previous active descendant or focus the collection container with `aria-busy=true`; after ACK install the new descendant and announce the logical label/position. Editing/focus pins prevent ID reassignment. Portal and dialog focus updates happen after target existence validation, before action-policy arming.

## 13. Accessibility, localization, themes, and text

### 13.1 Semantic schema

Every node kind has a closed allowed-property matrix. Core semantics include accessible name/description, role, disabled/read-only/required/invalid/busy, checked (false/true/mixed), expanded/selected, value-min/max/now/text, set-size/position with known/unknown distinction, hierarchy level, row/column count/index, label/description/error/controls/active-descendant relationships, live mode and atomic announcement. Relationships are node handles scoped to the managed root/portal; raw arbitrary DOM IDs are not admitted.

Native controls keep their native roles. Composite widgets implement the interaction behavior, not just a role name. The WAI-ARIA patterns are qualification references, not a substitute for the concrete machines above [S8]. A viewport exposes searchable engineering entities, selection and manipulation commands, properties and textual results; it does not create an accessibility node per triangle. Announcements coalesce hover/progress; explicit selection and validation have meaningful bounded messages. Keyboard-only, screen reader, magnification and touch input are first-class gates.

### 13.2 Catalogue and formatting

The application catalogue is immutable data keyed by `(LocaleId, MessageId, version)`. Message body is a bounded tree: Literal, Argument(name,type), Select(enum key, cases, other), Plural(number key, category cases, other), and Sequence. Argument types are Text, Integer, Decimal, Quantity, DateDisplay or EntityLabel; interpolated text is inserted as text, not markup. A build gate checks all required message IDs, type agreement, mandatory other cases, recursion depth ≤16 and total expansion ≤16 KiB.

Locale fallback is exact locale -> configured language default -> application default. The application manifest includes every admitted locale and its pinned plural/format data version; it cannot silently use a host's different plural rules for semantic decisions. Host Intl may format presentation through a versioned result capability when chosen, but those strings are nondeterministic presentation inputs; canonical domain values and parser decisions remain Habu-owned. Base locale data packages include English, Ukrainian and German rules and fixtures; adding other locales requires their data and fixtures, not code changes to controls.

Quantity parsing has an ASCII-canonical internal representation `(finite f64 magnitude, DimensionId, UnitId)` or application decimal/rational type for values that require it. Display uses locale separators; input accepts the configured decimal separator and explicit unit aliases, rejects ambiguous grouping, and preserves the original draft. Multi-field validation checks dimensions, finite ranges and exact application tolerances. Locale change preserves draft text and its parsing locale until explicit reformat/commit; it does not reinterpret `1,234` silently.

### 13.3 Theme and asset invalidation

Theme token sets define colors, type scales, spacing, density, focus rings and contrast variants. Native prefers-color-scheme/reduced-motion/forced-colors and text scaling are environment inputs. The host owns native hover/pressed/focus appearance; Habu supplies semantic states and tokens. CSS transitions are nonsemantic and can be cancelled without changing document state.

Font assets are packaged or approved content hashes. Font completion increments FontEpoch, invalidates measured text/virtual-row heights, and schedules reflow through the measurement protocol. Text rasterization uses whole strings or declared shaped runs with font, direction, language, size, scale, width/wrap and baseline. The host returns bitmap plus advance/ascent/descent/ink rect. Habu's atlas owns immutable versioned rectangles and leases them for frames; eviction cannot overwrite a glyph/run rectangle still referenced by a frame. Editable labels use a real native input overlay; rendering a string into an atlas is not editing it.

Rich content is a parsed closed semantic tree of paragraphs, headings, lists, emphasis, inline code and validated links. Imported HTML/SVG is not injected. A future rich-text editor is a separately admitted component, not an assumption that textarea provides document editing. Canonical CAD drawing typography and exact export use an application-controlled font/shaping pipeline, not platform-dependent raster snapshots.


## 14. Partial document residency, scene bundles, and interaction tools

### 14.1 Semantic subscriptions, not merely mesh LOD

The browser does not have to load every entity, feature and constraint in a large assembly. A DocumentSubscription consists of DocumentStamp, confirmed sequence, root entity manifest, subscribed entity ranges/IDs, loaded page versions, and unresolved placeholders. Entity states are Unknown, Requested, SummaryReady, DetailReady, Deleted or Denied. A summary contains stable ID, type, label, bounds, child-count-known flag, dependency revision and permitted summary actions. It is not evidence that all exact properties are locally present.

A command declares required entity fields/dependency classes. Local validation returns Valid, Invalid, or NeedsData(dependency list). A command with missing prerequisites either fetches them, submits for authoritative validation with no invented local result, or remains a draft offline. Absence from the loaded subset never means an entity does not exist. Server deletions/tombstones are explicit. Subscription updates are revision-tagged and sequence-consistent; entity detail can be evicted while stable IDs and summaries remain.

Content cache entries use `(NamespaceId, contentHash, schema, dependency fingerprint)`. Matching immutable assets remain reusable after unrelated global revisions. Attaching them to a projection still validates their exact entity/geometry dependencies. Server metadata pages expose a canonical page/version manifest and bounded cursors; loaded pages are charged to semantic memory independently of mesh caches.

### 14.2 SceneBundle publication

A SceneBundle manifest contains BundleId, DocumentStamp, source ProjectionStamp, accepted operation/evaluation IDs, GeometryRevision, unit/frame conversion, entity dependency versions, MeshAssetRefs, instance records, mesh-to-topology maps, edge-curve refs, LOD/error bounds and optional BVH refs. Content hashes identify each asset; compressed wire bytes and decoded canonical content have distinct hashes/lengths. The topology map names the exact mesh index order it maps.

Publication states:

```text
ManifestReceived -> Validating -> LoadingMinimum -> ReadyCandidate
 -> ScenePublished | Superseded | Failed
```

Minimum displayable set is an admitted coarse mesh (or explicitly a bounding-box placeholder), its matching semantic/topology mapping if selectable, finite bounds, units/frame and an instance transform. CPU triangle picking additionally requires a valid BVH or a bounded fallback query job. A placeholder is not reported as an exact face hit. Publish the bundle's coherent root in one SceneStore update. Later LOD swaps are new immutable bundle versions, not mutation of a mapping next to old triangles.

An operation may be accepted while its exact evaluation is Pending or Failed; a previously successful geometry bundle can remain visibly labelled StaleGeometry. `OperationAccepted`, `EvaluationSucceeded`, and `DisplayReady` are separate status fields. An obsolete geometry result may populate immutable cache only if its original namespace still allows it; it may not replace the active bundle merely because download finished last.

### 14.3 Scene storage and streaming

Mesh assets are immutable and shared across instances. Store vertices/indices/normals, material groups, bounds, topology map and mesh-local BVH once per content version. Instances use chunked structure-of-arrays tables for transforms, bounds, visibility, material override and semantic identity. A top-level BVH indexes instances; transform changes refit leaves and ancestors without rebuilding shared triangle BVHs.

LOD selection compares projected error against a CSS-pixel target (initial 1 px for normal display, 0.5 px selected); hysteresis upgrades above 1.2× threshold and downgrades below 0.7×. Selection/edit pin leases prevent evicting the required mapping/geometry. Priority is visible selected/editing, visible ordinary, nearby coarse, background prefetch. Cancel obsolete observations but deduplicate shared asset loads. Requests have byte/triangle/decompressed-ratio limits from manifest before allocation. Hash, decompression and validation are incremental jobs; malformed indices never reach GPU upload.

Eviction order: unused fine LODs, unreferenced BVHs, unreferenced decoded meshes, old compressed cache by policy. Leased bundles/frames/readers are not eviction candidates. Retain coarse or box representations within budget; inability to retain them produces an explicit resource-limited presentation, not silent selection identity changes.

### 14.4 Tool machines and previews

Tools are explicit `(ToolId, Generation, DocumentStamp, start dependencies, state, pointers, preview root, scope)`. Manipulation states Idle, Armed, Dragging, ResolvingDraft, AwaitingDurability, AwaitingAuthority, Finished or Cancelled. Escape/capture loss cancels a provisional preview before command construction. After durability, Cancel means an explicit semantic cancellation/compensation request, not erasing its journal entry.

Camera navigation updates session state and per-frame camera only. Dragging a partition starts from immutable world transform and pointer ray; each sample computes a transform from that baseline, not accumulated rounded deltas. Snapping candidates come from the retained semantic/mesh query snapshot and carry Approximate or Exact authority. Crossing a remote constraint/entity change triggers RebasePreview if the specific tool supports it, otherwise cancels and preserves numeric draft parameters. A solver hint never becomes an exact engineering guarantee merely because it ran locally.

Release captures final position and emits one SetTransform/SetParameter/constraint operation with start dependencies. A rejected result keeps recoverable intent parameters. UI, keyboard and AI invoke the same tool/command registry with explicit units and coordinate frames.

### 14.5 Reliable pick tasks and input causality

The host publishes an InteractionViewToken whenever it learns that a frame was submitted: FrameId, BundleId, CameraVersion, SurfaceEpoch, LayoutEpoch and InputWatermark. A pointer event captures the latest token known to that host at input time. This is **not** a claim that the exact frame was physically visible on the monitor. The token identifies an unambiguous interaction basis; acknowledgement latency is measured.

A PickTask contains TaskId, owner scope, authority, ViewToken, retained bundle/camera lease, normalized CSS/backing coordinates, policy, purpose Hover|Click|Tool|Measurement and deadline. Hover may supersede the preceding hover. Click/tool tasks must return one terminal Result|StaleTarget|Unavailable|Cancelled|Timeout. They do not live in disposable frame packets.

CPU picking transforms the ray into each candidate mesh's local coordinates, traverses top-level/mesh BVHs under a work budget, computes nearest valid triangle hit, then resolves its semantic face/instance using the bundle's exact map. For nonuniform scale, compare reconstructed world ray distance, not incomparable local t values. Mirrored transforms retain the configured backface policy. Clip, visibility, suppression, transparency alpha threshold and pickability filters match rendering.

GPU picking is a reliable job against leased bundle/camera data and dedicated transient targets. It may share a display frame's compatible depth/ID work, but if that carrier is skipped the task returns to Ready. After three skipped opportunities it is scheduled as a dedicated offscreen job ahead of discretionary display frames. Under finite available resources and a functioning device it progresses; otherwise the deadline/device result terminates it explicitly. Continuous motion cannot postpone it forever. Device loss can trigger one retry under the same logical snapshot if reconstructible; otherwise Unavailable. A retry never switches to a newer topology silently.

A pick result includes request ID, bundle/mapping, camera/surface/layout stamps, semantic reference, approximate point/normal, and authority class. Hover stale results are discarded. Click results are revalidated against current entity existence and persistent-name policy; they either select the same semantic target or report StaleTarget/Ambiguous. Exact measurement is a separate geometry-service task.

## 15. GPU content ownership, frame leases, and reliable jobs

### 15.1 Immutable logical resource versions

The GPU host separates Allocation, Writer, ContentVersion and Lease. A writable allocation is never a valid frame input. Buffer/texture creation returns a WriterHandle scoped to size, usage, format and device epoch. Upload writes require that writer and bounded in-range chunks. SEAL validates complete required ranges, content/layout metadata and closes the writer irreversibly; it returns a ContentVersionHandle. A logical content version cannot be modified in place.

```text
Reserved -> Writing -> Sealing -> SealedReady -> Retiring -> Released
                       \-> Failed
any device-owned state -> Lost
```

For a large buffer, the manifest declares initialized ranges; all bytes observable by an admitted shader must be initialized (zero-fill omitted regions or reject). Padding is zero. Immutable asset uploads verify their content hash. Frame-local uniform data may use a per-plan generation instead of a cryptographic digest, but the storage remains sealed and leased.

Changing transforms/materials allocates a new logical chunk/version, optionally GPU-copying unchanged portions into a new allocation before patching/sealing. Geometry remains shared. The base implementation uses copy-on-write instance chunks of 256 records (32 KiB at 128 bytes/instance), not whole-scene rewrites. Retiring the old chunk waits for all frame and pick readers. A physical ring allocator may reuse a slice **only** after every older lease ends; its allocation generation then increments. Merely adding a version label without retaining bytes is invalid.

Bindings reference the sealed ContentVersion plus exact offset/length. The first WebGPU profile uses fixed-offset bind groups per sealed slice; it does not assume unadmitted dynamic offsets. An optional dynamic-offset profile includes aligned offsets in the schema and tests the same lease rules.

### 15.2 FramePlan and lease transitions

A complete FramePlan contains FrameStamp, resource-version refs, graph instance, pass/draw lists, sealed camera/instance/material refs, transient-output requirements, and no durable side effects. No BUFFER-WRITE, persistent COMPUTE or PICK task is hidden inside it.

On transport acceptance, the host validates and retains referenced version/bundle-map leases before acknowledging FRAME-ACCEPTED. If validation cannot acquire every lease, return FRAME-REJECTED and acquire none. A pending frame may be replaced only after emitting its one terminal pre-submit outcome and releasing its leases. Submitted frames retain inputs until queue completion and any shared readbacks finish.

```text
Received -> Validated/Leased -> Pending -> Submitted(serial)
                        \-> Skipped          -> Completed
                        \-> Failed           -> Lost
```

FRAME-SUBMITTED is progress, not the end of resource lifetime. FRAME-COMPLETED or Lost retires submitted leases. There are initially at most two submitted frame leases and one pending plan per surface. Once Submitted, a frame cannot be relabelled Skipped. Resource-release requests remove the application's owner reference but cannot destroy storage with remaining host leases. Release is idempotent only for the same kind/identity.

A newer upload cannot change an older pending frame's uniform contents: they are different versions/allocations or protected slices. Resource processed-revision counters are scheduling diagnostics, not content snapshots. This explicitly fixes the audit's camera-A/camera-B counterexample. Browser GPU queue and resource rules remain platform constraints; the application supplies the version semantics [S1].

### 15.3 Reliable GPU jobs

Pick, readback, screenshot/export, and optional general compute are GPUJob records accepted independently of frames. A job has JobId, scope/authority, input ContentVersionRefs, output writer descriptors, shader/graph descriptor, priority, deadline and terminal reservation. Inputs are immutable. Outputs remain private until completion and validation; only then are new sealed versions published. Partial outputs on cancel/device failure are destroyed.

General compute is feature-gated and only accepts reviewed shader assets with fixed layouts and bounded dispatch metadata. It cannot write another job's published inputs. A dispatch count limit alone does not prove shader termination; approved shader admission and host watchdog/device-failure policy are required. A cancelled job may finish on GPU, but its output is drained instead of published. Frame-local compute is represented only inside a graph pass with transient outputs whose scope is that frame; skipping the frame discards all such work safely.

Readback creates an internal staging buffer, submits copy after producer work, maps asynchronously, strips row padding into a typed bounded stream, then unmaps/releases. It never blocks the Habu event loop. Retain source version and mapping table until terminal data transfer or discard. Completion does not imply physical screen presentation.

### 15.4 Resource creation and lifecycle errors

The v2 base uses the individually specified creation requests. Independent requests may share one packet, but a descriptor may reference only an already-Ready global handle; there are no undocumented packet-local IDs. Habu groups independent shaders/layouts, awaits their readiness, then requests dependent pipelines/bind groups. Startup measurements include these dependency round trips. A descriptor-DAG/local-ID batching extension is not admitted in this schema and is not required for any promised feature. It can be added only with its own complete negotiated schema. On partial batch failure, successfully created objects remain owned by their request scopes; scope disposal releases them.

Shader/pipeline compilation is asynchronous where supported; states Requested, Compiling, Ready, Failed. Pipeline/frame descriptors include expected bind/vertex/output-layout digests. Uncaptured validation errors are associated with the narrowest known job/frame; fatal device loss starts a new DeviceEpoch. Old resources are invalidated, outstanding tasks terminate/retry under their own policies, new descriptors rebuild from retained assets, and only then does the latest scene resume. Bounded retries (initial two attempts) lead to another admitted graphics profile or DOM-only mode. Surface replacement creates a new epoch and reports it to input/picking before arming interactions.

## 16. Render mathematics, graph, shaders, and visual policy

### 16.1 Coordinate contract and precision

The portable display space is right-handed, +Y up, meters, column vectors and column-major matrices. Application assets declare their original units/handedness and an explicit conversion matrix into display space; canonical engineering values remain in the application model. Camera looks along view -Z. Model transforms are `world = M * local`; clip is `P * V * M * local`. CPU transforms/rays/bounds use f64; GPU values are rounded explicitly to f32 at packing.

Use a stable world render origin O. Initial O is camera position rounded to a 1,024 m grid; rebase when camera distance from O exceeds 4,096 m or the scene's precision policy demands it. Instance translations are relative to O, and camera V is computed in that same frame. Ordinary orbit changes only camera data. Rebase is an explicit SceneOriginVersion job: build replacement instance chunks and camera data, then publish a coherent new scene bundle; old frames keep the old origin versions. Do not update all live translations in place.

Very large local meshes have a mesh-local origin and bounded coordinate chunks, admitted against a maximum f32 display error derived from the requested LOD. If the error exceeds policy, subdivide/re-tessellate via an asset job/service or show an explicit lower-accuracy representation. Floating-point display is not an exact modeling kernel.

Camera basis: z = normalize(eye-target), x = normalize(cross(up,z)), y = cross(z,x). Reject zero eye-target and nearly parallel up; a user camera command may choose an explicit alternate up vector, not produce NaNs. View rows are x,y,z with translation `-dot(axis, eye-O)`.

Perspective near n > 0, far F > n, vertical field angle θ in (0,π), aspect a > 0:

```text
s = 1 / tan(θ/2)
P = [ s/a  0   0              0          ]
    [ 0    s   0              0          ]
    [ 0    0   F/(n-F)        F*n/(n-F)  ]
    [ 0    0  -1              0          ]
```

It maps view z=-n to depth 0 and z=-F to depth 1 after division. Orthographic bounds l,r,b,t use rows `(2/(r-l),0,0,-(r+l)/(r-l))`, `(0,2/(t-b),0,-(t+b)/(t-b))`, `(0,0,1/(n-F),n/(n-F))`, `(0,0,0,1)`. Clear depth=1; compare LessEqual; normal geometry writes depth. Far/near are finite scene-derived settings with hysteresis and user overrides; warn when depth resolution is inadequate. Reversed-Z/log-depth are independent profiles, not silently mixed conventions.

NDC x maps to `(x+1)*width/2`; y maps to `(1-y)*height/2` in top-left pixel coordinates. Pixel-center rays use x+0.5,y+0.5. Near/far unprojection uses clip z=0/1. WebGL2 vertex shaders convert WebGPU depth with `clip.z = 2*clip.z - clip.w` for its [-1,1] clip range.

Normals use inverse-transpose of the model's 3×3 linear part and normalize afterward. Singular/nonfinite transforms are rejected for shaded/pickable geometry. Check condition/scale according to the asset numeric policy, not a hard determinant epsilon that rejects all small valid scales. A negative determinant selects the mirrored front-face pipeline (CW instead of CCW). Winding and normal orientation fixtures cover reflections and nonuniform scaling.

### 16.2 GPU layouts

Wire structs are packed separately from GPU structs. WGSL alignment/layout rules are tested independently [S9]. Base layouts:

| Buffer | Layout |
|---|---|
| Vertex, stride 32 | position vec3f @0, normal vec3f @12, uv vec2f @24 |
| Instance, stride 128 | model mat4f @0; normal columns vec4f @64,80,96; drawId u32 @112; materialIndex u32 @116; flags u32 @120; zero @124 |
| Material, stride 64 | baseColor linear straight rgba @0; parameters vec4f @16 (alphaThreshold, reserved×3); emissive vec4f @32; flags vec4u @48 |
| Camera, size 320 | viewProjection mat4f @0; view mat4f @64; eyeRelative vec4f @128; viewport vec4f @144; 6 relative-origin clip planes vec4f @160..255; lightDirection @256; lightColor @272; ambient @288; params @304 |

Unused lanes are zero. Camera params includes active clip-plane count as an exactly representable bounded value and render scale. Texture UVs/color conversions are declared by material, never guessed. Base bind groups: 0 camera uniform; 1 sealed instance storage; 2 sealed material storage plus optional reviewed albedo texture/sampler. Without a texture use a 1×1 white asset. Shader interface digests bind these layouts to pipeline declarations.

Illustrative core vertex shader for this layout (shader source is design material, not browser-compiled evidence):

```wgsl
struct Camera {
  vp: mat4x4<f32>, view: mat4x4<f32>, eye: vec4<f32>, viewport: vec4<f32>,
  planes: array<vec4<f32>, 6>, light: vec4<f32>, lightColor: vec4<f32>,
  ambient: vec4<f32>, params: vec4<f32>,
};
struct Instance {
  model: mat4x4<f32>, n0: vec4<f32>, n1: vec4<f32>, n2: vec4<f32>,
  drawId: u32, materialId: u32, flags: u32, padding: u32,
};
@group(0) @binding(0) var<uniform> camera: Camera;
@group(1) @binding(0) var<storage, read> instances: array<Instance>;
struct VertexIn {
  @location(0) p: vec3<f32>, @location(1) n: vec3<f32>,
  @location(2) uv: vec2<f32>, @builtin(instance_index) ix: u32,
};
struct VertexOut {
  @builtin(position) clip: vec4<f32>, @location(0) position: vec3<f32>,
  @location(1) normal: vec3<f32>, @location(2) uv: vec2<f32>,
  @location(3) @interpolate(flat) drawId: u32,
  @location(4) @interpolate(flat) materialId: u32,
};
@vertex fn vs_main(v: VertexIn) -> VertexOut {
  let i = instances[v.ix];
  let p = i.model * vec4<f32>(v.p, 1.0);
  var o: VertexOut;
  o.clip = camera.vp * p;
  o.position = p.xyz;
  o.normal = normalize(mat3x3<f32>(i.n0.xyz, i.n1.xyz, i.n2.xyz) * v.n);
  o.uv = v.uv; o.drawId = i.drawId; o.materialId = i.materialId;
  return o;
}
```

Fragment contracts below specify the remaining algorithms; production shaders are generated/reviewed assets and validated against numerical/pixel fixtures. No assertion of shader qualification follows from this listing.

### 16.3 Graph and transient lifetime

The render graph is an acyclic ordered pass DAG. Every pass declares named inputs, output attachments, size/format/sample count, load/store behavior, resource-version dependencies and whether it is required. A validator checks producer-before-consumer, compatible sampling, no simultaneous illegal read/write, initialized load, bounds and output ownership. Transient targets belong to one FrameLease/GPUJobLease. The base allocator does not alias targets across live frames. Within a frame it may alias only identical compatible formats/sizes whose nonoverlapping lifetimes are proven by the graph; start without aliasing.

| Pass | Reads | Writes / order and policy |
|---|---|---|
| Opaque | camera/instances/materials/mesh | linear sceneColor + depth; clear then depth-write |
| Transparent | same + opaque depth | sceneColor blended, no depth-write, sorted back-to-front |
| Edges/sketch | edge/path assets + depth | sceneColor, visible segments; optional dashed hidden segments |
| SelectionMask | selected geometry + depth | mask, visible-only by default |
| SelectionComposite | mask + sceneColor | outlinedColor; separate target prevents read/write alias |
| Labels/gizmos | raster atlas/overlay geometry | outlinedColor, explicit depth-tested or always-on-top layer |
| Presentation | final linear color | surface; linear-to-sRGB encode once, opaque alpha |
| ReliablePick | exact selected bundle inputs | private ID/depth targets under GPUJob, optional sharing only with matching leased pass |

Base sceneColor is RGBA8-unorm **linear storage**, depth32float, single sample. RGBA16-float is an optional higher-quality intermediate. Canvas uses negotiated RGBA8/BGRA8 unorm, opaque alpha. Albedo sRGB textures are decoded to linear by the sampler; vertex/material colors are already linear. Final encode uses `12.92*x` when x≤0.0031308, otherwise `1.055*x^(1/2.4)-0.055`, clamped to [0,1]. Do not use both a final explicit encode and an sRGB view conversion. A different surface profile declares its color/alpha policy separately.

### 16.4 Shading, visibility, and picking consistency

Opaque shading is intentionally modest: `base.rgb * (ambient.rgb + lightColor.rgb * max(dot(normal, -lightDirection),0)) + emissive.rgb`. Normalize normals/light. Material alpha=1 is opaque; cutout materials discard below their declared threshold; transparent materials use premultiplied source rgb=`linearRgb*alpha`, source factor One, destination OneMinusSrcAlpha. No tone mapping is required for base LDR; clamp in final output. Flat/unlit mode omits lighting.

Transparent instances sort descending by view-space far bound, then stable InstanceId for ties. This is approximate for intersecting translucent objects and is labelled so; no exact order-independent compositing is promised. Opaque depth remains authoritative for occlusion. Picking renders nearest selectable fragments with depth-write and no blend, including transparent fragments with alpha≥0.1 unless material declares another threshold. Zero-alpha/cutout holes, suppression and clipping use identical tests. Tool gizmos have a separate priority pick layer; selecting through objects is an explicit alternate query policy.

Clipping uses up to six planes in render-origin coordinates: keep fragment when every active `dot(plane.xyz,position)+plane.w >= 0`. The same predicate applies to ID passes and CPU hit filtering. Base display shows cut boundaries/plane indicators but does not fabricate exact capped sections. Exact section curves/caps arrive from a geometry operation and carry authority metadata.

### 16.5 Edges, strokes, overlays and antialiasing

Geometric edges come from explicit semantic edge polylines/curves, not every triangle boundary. Optional silhouette extraction uses adjacency and view-facing classification in a bounded job; absent adjacency, do not label tessellation seams as CAD edges. Screen-width strokes expand clipped line segments into triangles. Clip in homogeneous space before perspective divide to avoid near-plane explosions. Width is CSS pixels × actual render scale; endpoint positions carry accumulated dash length.

Join policy: miter until miter length exceeds 4×half-width, then bevel; round joins/caps use adaptive arc segments with at most 0.25 physical-pixel chord error and a 64-segment cap. Zero-length segments use a round point or are omitted under the path policy. Visible edges test depth with a configurable small clip-depth bias toward camera, limited to the declared edge tolerance; pickable topology is not altered by that visual bias. Hidden-line mode, when enabled, draws a separate depth-failing dashed pass in a subdued color. It is a display mode, not an exact hidden-line export.

Path flattening uses adaptive subdivision in screen space with ≤0.25 px error and depth ≤20; a capped failure returns PathTooComplex rather than allocating indefinitely. For filled paths, v2 uses an incremental sweep/trapezoid algorithm: compute segment intersections; split at crossings; sort y event levels; evaluate ordered intersections in each open slab; track winding/even-odd coverage; triangulate covered trapezoids. Horizontal edges contribute boundaries but not crossing count. Degenerate/near-coincident events use one declared display epsilon and deterministic tie ordering; inconsistent topology fails the display asset rather than influencing engineering data. Initial cap 4,096 segments and 65,536 intersection events, all processed by resumable jobs. This handles self-intersecting display paths under the specified fill rule; exact CAD curves remain separate.

Base antialiasing uses analytic stroke coverage and a final optional FXAA-like luma-edge smoothing pass with fixed kernel/threshold assets; it does not blur ID/depth picking. A multisample profile uses sample count 4 only after format support qualification, includes explicit resolve attachments, and never references a multisampled texture as an ordinary sampled texture. Indexed strips are **not admitted** in base pipelines: all strips/lines/points are expanded to triangle-list topology before upload, closing the missing strip-index-format issue without a hidden WebGPU field.

Selection outline is a mask dilation-minus-mask operation with radius 2 CSS px converted to backing pixels and capped at 8 physical pixels. Visible-only mask compares scene depth; optional XRay mode is a separate labelled policy. Labels have anchor, priority, max width, depth policy, baseline/ink metrics and atlas version. Occluded labels can hide/fade only under a declared rule; editing a label always uses the native overlay lease. Whole-run raster cache keys include font/version, text, locale, direction, size, scale, wrap and policy. Frame leases protect atlas rectangles from overwrite.

## 17. WebGL2, unavailable graphics, and backend parity

WebGL2 is a separate execution profile, not WGSL translation. Habu selects the compatible plan; GLSL ES shaders share generated mathematical/layout constants but are independently tested. Supported base geometry is indexed triangle lists with U16/U32 indices; instancing uses vertex divisors or bounded draw loops. Since there is no `firstInstance` equivalent assumed by this profile, the planner binds an offset instance slice or sets a baseInstance uniform and indexes a declared data texture. Storage buffers are lowered to uniform blocks for small values or RGBA32F/integer data textures with bounded index arithmetic; the planner rejects data exceeding qualified texture/uniform limits rather than mispacking it.

Pipeline descriptors map fixed state explicitly: depth compare/write, mirrored front face, cull, premultiplied blending, vertex stride/offset, scissor, viewport and texture/sampler formats. WebGPU clip z is converted in the shader. Dynamic uniform offsets and general compute are unsupported. Textures use explicit decoded color policy; output gamma handling matches §16.3. Baseline WebGL2 semantic picking is CPU BVH; GPU integer/depth readback is optional and separately qualified. Context loss invalidates all GL-owned handles under a new DeviceEpoch, like WebGPU device loss.

If neither GPU profile qualifies, DOM-only mode still supports semantic tree, properties, operation/conflict status and file/command flows. It shows a clear disabled-viewport explanation. It does not claim an invisible face can be selected by pixels. Keyboard entity selection and server-render/export actions may remain available under application policy. Browser/renderer capability admission is a product feature matrix generated from passing tests, not hard-coded version assumptions.


## 18. Semantic commands, collaboration, and ordered outcomes

### 18.1 Registry and application hooks

Every command descriptor has stable Id128, schema version, argument/result schemas, permission class, preview policy, required entity/dependency classes, admission profile, inverse policy and automation exposure. Execution is mediated by the same registry for widgets, shortcuts, tests and AI. An enabled button is never authorization.

The optional synchronization library calls these **pure** application hooks:

```text
VALIDATE(snapshot, command) -> Valid(readset) | Invalid(errors) | NeedsData(ids)
PROJECT(snapshot, command, logicalIds) -> CandidateWrites | Deferred(reason)
REBASE(newBase, pendingCommand, oldReadset) -> Keep(rewrittenCommand, readset)
                                        | Conflict(detail) | Blocked(dependencies)
CANONICALIZE(orderedEvent, pendingCommand?) -> canonical writes + ID mapping
INVERSE(currentSnapshot, receipt, retainedEvidence) -> command | unavailable/conflict
REDACT(command/result, authority) -> permitted public/automation view
```

PROJECT/REBASE/CANONICALIZE do not submit effects, read a live clock, generate fresh random IDs, or contact a server. IDs and nondeterministic inputs are fixed when the operation is constructed. Replaying the pending queue is a pure derivation. Effect dispatch belongs to a durable outbox transition with its own idempotency key, not the projection function.

### 18.2 Operation and event envelopes

Operation contains OperationId, ActorSessionId (server authenticated), DocumentStamp, base confirmed sequence, command ID/version, canonical arguments, dependency OperationIds, entity preconditions, client journal sequence and any approval token/reference. The server verifies identity/permissions; packet actor bytes alone are not trusted. IDs for new entities are client-generated persistent Id128 from the outset when accepted by the domain. If a service assigns canonical IDs instead, receipt includes a full provisional-to-canonical mapping applied to pending arguments and selection in one projection transaction; never patch arbitrary serialized bytes by search/replace.

OrderedEvent contains DocumentStamp, ServerSequence, OperationId, canonical command/result, entity deltas/revisions, evaluation status/ticket and receipt digest. The server publishes a gap-free per-document order. A Receipt can arrive separately and says Accepted(sequence, digest), Rejected(detail), Pending(ticket), or Unknown/Expired; it does not replace an absent ordered event.

### 18.3 Durable operation machine

```text
Constructed -> VolatileProjected -> Journaled -> Sent
  -> AcceptedAwaitingEvent -> AppliedAwaitingReceiptPersistence -> Applied
  -> RejectedAwaitingPersistence -> Rejected
  -> Unresolved -> outcome discovery
```

OperationId and immutable command body are stored before transmission. The UI may display VolatileProjected immediately, explicitly Unsaved; no offline-saved label precedes the IDB transaction completion. Journal failure retains an unsaved draft and blocks dependent sends. Document service owns these transitions and an operation observer is detachable.

Receipt-before-event: persist acceptance and expected sequence, request the gap/event, keep projection provisional until event applies. Event-before-receipt: apply that operation once in sequence, synthesize a local receipt from the authenticated event, atomically persist receipt/pending disposition, and treat later matching receipt as duplicate. A mismatched digest is a synchronization fault. Never apply both the optimistic command and its canonical event to the accepted base.

On a server event, build a candidate accepted base, remove matching accepted pending operation, rebase remaining operations in dependency order, and publish one new projection stamp. Rejected parents block dependent commands; preserve them as drafts with an actionable dependency conflict. Rebase failure does not drop user's intent invisibly. Commands with no safe local PROJECT remain AwaitingAuthority without invented geometry.

### 18.4 Persistence boundary and exactly-once limits

The receipt and pending-state disposition are written in one local IDB transaction alongside the new accepted sequence/checkpoint pointer when applicable. Applying an event in volatile memory before this transaction completes is allowed, but recovery replays from the persisted checkpoint and receipt set, not a half-written pointer.

Server deduplication keys include authenticated tenant/document and OperationId. A retry with the same ID but different canonical body hash is rejected. Deduplication must retain receipts at least the negotiated horizon; the client stores that horizon. After expiry, Unknown does not authorize blind resubmission. Query current state, request a retained operation status/history proof, or require a newly approved semantic operation. Network timeout and local cancellation never prove server rollback.

Crash matrix:

| Crash point | Recovery |
|---|---|
| Before journal commit | only volatile draft; no server send was allowed |
| After journal, before send | discover Journaled and send same OperationId |
| After server commit, before reply | query/retry same ID within receipt horizon |
| After reply, before receipt persistence | reconcile authenticated event/receipt again; deduplicate |
| After receipt persistence, before UI update | rebuild accepted/pending projection from durable records |
| During rebase/checkpoint | old committed pointer remains; unpublished candidate discarded |

### 18.5 Undo, long operations, presence, and reconnect

Draft undo is native/session-local. An unsent journaled operation may be withdrawn by a durable local state transition only if no dependent sent operation relies on it. Once sent/ambiguous, Undo is a compensating command or a server cancellation request, not erasure of evidence. Accepted operations declare ExactInverse, ConditionalInverse or Unavailable. Inverse evidence is retained with a bounded history policy; once compacted, UI explicitly says the action cannot be undone locally. Redo is revalidated and gets a new OperationId.

Long exact modeling returns an operation/evaluation ticket. Progress is advisory and distinct from acceptance; cancelling evaluation has AcceptedCancellation, AlreadyCommitted, NotCancellable or Unresolved outcome. Changes already accepted are compensated through ordinary commands. Result geometry attaches only under its dependency manifest (§14).

Presence is lossy/coalesced, includes session/document, selection/camera/cursor and expiry, and never enters document history. Reconnect negotiates DocumentEpoch, last durable server sequence, schema support, authority and receipt horizon. Apply missing ordered events or an immutable checkpoint, query unresolved operations, then rebase pending commands before sending. A new document epoch prevents old events applying to a replaced document even if sequence numbers restart.

## 19. Indexed storage, blobs, migrations, and multiple tabs

### 19.1 Database schema

Browser storage is a recovery mechanism subject to its actual persistence policy, not a guarantee against device loss or user-cleared storage [S13]. The host reports IDB transaction completion, not request issuance; transactions cannot await arbitrary outside work and upgrades can be blocked by other connections [S14].

One application database uses these logical stores, all keys start with NamespaceId:

| Store | Key / value |
|---|---|
| namespace | namespace -> auth fence, active data generation, schema versions, policy |
| documents | namespace/doc -> DocumentEpoch, committed checkpoint, server sequence |
| operations | namespace/doc/OperationId -> immutable command body/hash, construction schema |
| op-state | namespace/doc/journalSequence -> append-only transition, OperationId, receipt ref |
| receipts | namespace/doc/OperationId -> authenticated result, server sequence/digest |
| journal-head | namespace/doc/session -> next local sequence, high-water, owner/fence |
| checkpoints | namespace/doc/generation/chunk -> immutable data/hash/schema |
| checkpoint-head | namespace/doc -> active generation, verified manifest |
| blobs | namespace/hash -> descriptor, byte length, state, storage generation, ref metadata |
| blob-chunks | namespace/hash/chunk -> bytes (IDB fallback) |
| leases | namespace/kind/id -> owner, fencing generation, pinned refs, heartbeat hint |
| migrations | namespace/target-generation -> phase, cursor, source/target versions, hashes |
| preferences | namespace/user/key -> versioned bounded data |
| draft-vault | namespace/FieldKey -> encrypted optional draft, policy/key version |

Recovery reads journal-head high-water and scans op-state up to that watermark; new transitions afterward belong to the next pass. Immutable operation bodies are looked up by ID. Checkpoint manifest contains exact retained pending operation IDs and receipt boundary, so discovery does not depend on guessing keys.

### 19.2 Transactions and scans

`STORE-TX` contains a finite list of Get, Put, Delete, RequireAbsent or RequireVersion operations. Every stored value has a u64 version and validated schema/length. Compare-and-set uses this version inside the IDB request callback sequence, not an asynchronous hash operation awaited mid-transaction. Hashing/encoding/encryption occurs before opening the transaction. Failure of a predicate aborts the whole transaction. Mutation increments the value version under the transaction. Read/write permission, namespace auth fence and journal fencing owner are checked in the same transaction where relevant.

`STORE-SCAN` has store/index ID, prefix/range, limit and continuation `{dataGeneration,lastKey,highWater}`. Results are bounded rows plus next token or End. Checkpoint/operation-body generations are immutable and can be scanned consistently. Mutable metadata scans return observed version stamps and WeakSnapshot status; consumers revalidate before mutation. Recovery uses the immutable op-state watermark route, not a weak scan to infer completeness. A token from a different migration/data generation returns StaleCursor. Cursors are data tokens, not a browser cursor object held across tasks.

### 19.3 Blob writer, publication, reading and deletion

Blob operations are Begin(expected content hash, size), WriteChunk(sequence/offset), Finish, Abort, Open, Read, Stat, List, Retain, ReleaseHandle and DeletePersistent. This closes the earlier open-without-read gap. Chunks must be sequential in the base writer; repeated identical last chunk is acknowledged by sequence/hash, conflicting retries abort. A complete decoded-content hash is verified incrementally before Ready.

OPFS writer uses a staging object; Finish closes/flushes under the actual API contract and verifies length/hash. Then one IDB transaction publishes Ready metadata. IDB and OPFS are not one atomic transaction. Recovery detects staged orphan files and missing referenced files; cache assets can be refetched. The sole copy of a pending command body must be inside its committed journal transaction or an explicitly durable referenced publication, never just a temporary file.

Open returns a typed BlobRead handle and generation; Read yields bounded bytes plus EOF. It can be consumed by image/mesh decode or exported through file writing. Generic release closes the live handle; it does not delete persistent content. DeletePersistent requires GC authority and a CAS on blob metadata version, zero durable references and no live reader/lease under the same namespace fencing policy. Metadata first transitions Ready -> Deleting. New opens refuse Deleting. Physical deletion follows, then metadata is removed; recovery resumes either stage. Missing physical data is a recoverable cache miss, not falsely Ready.

GC snapshots durable checkpoint/pending references and live lease records, marks candidates, then rechecks before deletion. Frozen tabs cannot keep a lease forever solely by a stale heartbeat; takeover increments the persistent fence, and every resumed reader/writer validates it before access. Old buffers already in a tab cannot be forcibly erased by database leases; they are revoked for further application use. New references created after a mark pass prevent deletion at the recheck.

### 19.4 Migration and releases

ReleaseManifest binds host, Wasm, protocol feature schemas, shaders/layouts, locale/template assets, document schema range, journal schema range and migration descriptor hashes. Pin an active manifest for the page. Do not fetch “latest host” with an old cached Wasm module. Service worker caches only immutable hashed public assets; authenticated customer data uses the scoped storage adapter, not a generic public cache rule.

Application data migration uses shadow generations:

```text
Idle -> Announced -> OwnershipAcquired -> Copying(cursor)
 -> Verifying -> ReadyToActivate -> Activated -> OldGenerationRetained
 or Failed/AbortedBeforeActivation
```

Acquire migration fence; old tabs enter read-only or close their DB connection on versionchange. Physical IDB schema upgrades create only needed stores/indexes quickly; bulk data transformation is a resumable ordinary job into a new logical generation. A blocked upgrade reports UpgradeBlocked with an explicit close/reload action; it does not spin or delete the database. Source generation remains readable until atomic head-pointer activation. On crash, cursor/hash state resumes or abandons the candidate. Rollback is allowed before activation; after activation only if no incompatible new writes occurred and an explicitly supported reverse migration exists. Never promise rollback after destructive transformation.

Pending operation migrations preserve OperationId and its already-submitted body hash. A sent command's protocol identity cannot be rewritten and retried under the same ID with different bytes. Keep an old transport encoder for supported pending schemas or mark operations NeedsCompatibleClient/ReadOnlyExport. Unsent drafts may be migrated into a *new* command under explicit semantic policy while preserving the original evidence.

### 19.5 Multiple tabs and ownership

Each tab has independent session IDs, observers, view state and a journal partition. A single document service owns a given journal partition under a durable lease/fence; takeover is a compare-and-set transaction. Every append/state update checks that fence. BroadcastChannel is only a hint for logout/release/invalidations; correctness does not depend on receiving it. Server order reconciles operations from distinct tabs. Shared cache GC and migration use persistent fencing. There is no mandatory leader tab or SharedWorker dependency for ordinary operation.

## 20. Closed browser capabilities

All requests include registered ScopeHandle, authority/namespace, request ID, deadline, typed operation and limits. Success creates no unowned object: each returned handle has a consuming operation and release path. Error/late completion still drains owned results. Ordinary requests cannot borrow a Wasm span across await. Specific operation records and their result types are in the registry (§25/appendix).

### 20.1 Time, environment, networking

ClockNow returns root-domain time; Timer returns actual firing time; Random returns bounded cryptographic bytes. EnvironmentGet/Subscribe returns theme, motion/contrast, locale list, visual viewport and connectivity hints, each with EnvironmentRevision. Connectivity is a hint; actual network failures are authoritative for task results.

HTTP Request takes configured ServiceId/RouteId/Method, typed route parameters, allowed headers, optional bounded body/read stream and max response size. The host resolves URLs and credentials from deployment policy; arbitrary URLs/Authorization-cookie reflection are not inputs. Success returns status/allowed headers and a typed ReadStream, including empty-body streams. HTTP 4xx/5xx remain HTTP outcomes, not fabricated transport failures.

SocketOpen returns **both** SocketHandle and ReceiveStreamHandle, negotiated protocol and message limit. Receive stream emits `STREAM-MESSAGE` with monotonically increasing MessageId, chunk number, Start/End flags, total-known/length and data; one logical WebSocket message may be split into bounded internal packets but retains boundaries. ReceiveCredit grants application bytes/messages, separate from local lane credit. Send returns AcceptedByHost; a server command receipt is separate. Closing returns Closed and terminates the receive stream exactly once with reason. Reconnect creates new handles and cannot reuse old message IDs as new stream state.

The baseline uses push streams: STREAM-CREDIT enables delivery, STREAM-MESSAGE carries chunks, and STREAM-CANCEL terminates the subscription. STREAM-CLOSED reports EOF/failure/cancellation exactly once. There is no implied unregistered StreamRead or StreamClose request. Streams have owner, authority, next sequence, total-known, byte/message ceilings, state Open/EOF/Failed/Closed. A consumer can drain or cancel; abandoned scopes close streams. On failure mid-message, the incomplete message is discarded or exposed as a typed IncompleteMessage error, never concatenated with the next message.

### 20.2 Files, clipboard, navigation and printing

FilePick returns typed file read handles and metadata; file-input fallback has the same result schema but no path/writable privilege claim. ReadBytes accepts a closed SourceRef(File, Blob, ResultBuffer) and offset/max count, returning bytes/EOF. Names/MIME are untrusted hints; parsers inspect contents and quotas.

WriteBegin consumes an authorized destination or selects a declared download fallback, returning a Writer. WriteChunk is sequential with byte/hash checks; WriteClose returns BrowserStreamClosed or DownloadInitiated. The latter is not proof of a completed disk write. WriteAbort is explicit; a partially written external file may require platform cleanup and is reported, not assumed rolled back. The host does not fabricate arbitrary filesystem access from a filename.

ClipboardRead/Write accept approved formats and require actual policy/activation when the platform does. Internal rich clipboard data has an application schema/version plus plain text fallback. Arbitrary HTML is not trusted. Secret fields do not prepare persistent copy payloads. Navigation/external-link/print/fullscreen use the prepared gesture policy as required; no synthetic click circumvents it. Fullscreen exit is observed, not assumed because a request was issued.

PRINT-PREPARE accepts a closed PrintDocument of title, paragraph, table and immutable-image blocks, locale/direction, paper size and margins in millimeters. Maximums are 2,048 blocks, 256 columns per table, 10,000 total cells, 128 retained images, 64 KiB per semantic packet and 200 requested pages. Oversized documents return Quota or use an application export service rather than hidden unbounded DOM construction. The host builds a read-only detached print tree incrementally, retains image/font leases, and returns a PrintLayoutHandle only after preparation. No script, raw HTML, dynamic native inputs or arbitrary CSS is permitted. PRINT uses that handle under a valid gesture/authority policy; completion means the native print dialog was requested/returned under the platform contract, never that paper was printed. The layout is released by RESOURCE-RELEASE or scope disposal. It is an ordinary UI/report print path, not an exact CAD drawing generator.

### 20.3 Storage, assets and text

Storage operations are those in §19, not unrestricted IndexedDB object access. Quota estimate/persistence request returns actual browser results and cannot upgrade policy into an unconditional durability promise. Blob read/list/delete are distinct from handle release.

ImageDecode accepts a File/Blob/ResultBuffer byte source, an allowed codec policy, maximum decoded dimensions/pixels and orientation policy. Base policy allows PNG/JPEG/WebP through qualified browser decoders; animated images use the first frame unless an animation profile is selected. SVG goes through a separate sanitized vector parser, not image-to-DOM injection. The decoder returns an immutable ImageHandle, normalized orientation, dimensions and color/alpha metadata. PixelRead returns bounded canonical RGBA chunks. Host-to-GPU copy is an optional capability that produces a sealed GPU version with the same ownership checks.

FontLoad accepts only a packaged/approved hash and font policy. TextRaster accepts a FontHandle, UTF-8 text, language/direction, size/scale, width/wrap/alignment and pixel budget; returns ImageHandle and metrics. Font readiness changes FontEpoch and invalidates measurements as §13 specifies. ResultBuffer handles from large operations can be read using ReadBytes and released; they cannot be confused with files or GPU buffers.

### 20.4 Capability completion and revocation

Every request has exactly one task terminal outcome: Success, DomainError, Denied, Unsupported, Cancelled, Timeout, Unresolved, Quota, StaleScope, ActivationRequired, OOM, PlatformError or ProtocolError. Progress does not free a terminal reservation. Success with a handle transfers its ownership to the receiving live scope only after validation; otherwise host releases it. Request cancellation is acknowledged even if the platform cannot stop the underlying work.

CapabilityChanged increments the relevant authority/policy generation. Old grants never upgrade automatically. ResourceRead/Release remain permitted for narrowly defined cleanup under the old owner; new privileged work is blocked. Durable operation receipt reconciliation may continue under a reauthenticated authorized document service; it cannot send new unauthorized edits.

## 21. Boot, deployment, developer tools, and optional compilation

### 21.1 Boot and lifecycle

Static HTML supplies a managed root, loading status, error/retry and recovery export before Wasm starts. Fetch pinned ReleaseManifest, validate supported schemas/policies, instantiate the AOT module with only declared imports, negotiate clock/credits/scopes and call START. Mount minimal DOM, restore the journal/checkpoint under authority, then load semantic subscription/coarse scene assets. Panels do not wait for exact geometry or every shader.

Runtime states Created, Negotiating, Starting, Running, Suspending, Suspended, Resuming, Recovering, Stopping, Stopped, Failed. Visibility loss pauses animation and speculative gestures according to tool policy; it does not depend on unload to persist. Resume rechecks auth, transport, DB fence, environment/layout and device/surface. A fatal Wasm trap revokes old ports/epochs, preserves permitted host drafts, restarts a fresh instance and recovers durable state. Rendering-only failure does not roll back the document.

Shutdown disarms policies, stops accepting commands, detaches observers, flushes already-reserved journal work when possible, closes scopes/streams, releases resources and terminates workers. Crash recovery never assumes shutdown completed.

### 21.2 Build outputs and package increments

An application build emits `app.wasm`, ABI/feature schema manifest, immutable public asset manifest, generic host modules, CSS/widget templates, locale catalogues, shader/layout assets, source/definition map and optional offline cache manifest. All are content-hashed and tied to one release identity. Server delivery uses suitable MIME for Wasm, HTTPS and tested CSP/worker/connect/img/font policies; do not weaken policy by adding eval when startup fails.

Habu package digests include source, dependencies, target profile, schema and layout versions. A changed application component rebuilds its Habu package and affected AOT module/link plan; it does not force rewriting generic host files. The wire schema generator is a checked Habu tool in the production toolchain. The JSON registry in the external reference package is language-neutral design data and its Python generator/validator is an independent reference utility, not a new required production compiler dependency.

### 21.3 Inspector, replay, automation, and reload

The inspector exposes component/placement/key path, props/state schemas, dependency and retry edges, desired/acknowledged UIStamp, edit leases/drafts under redaction, pending jobs/scopes, operation states, scene maps, GPU content versions, leases, copied bytes and frame causal watermarks. It can explain why a result was stale and why a component reran. Sensitive text/geometry is omitted from routine traces; explicit permitted diagnostic export records its privacy classification.

Replay captures accepted normalized events, total ingress order, nondeterministic capability results, clocks/random, layout/environment epochs and scheduler cut points. Pure semantic hashes can be exact. Browser layout/raster results are replay inputs, not claimed cross-platform deterministic outputs. Trace records carry command/task/frame parent IDs; bounded ring buffers overwrite only diagnostic history, never the durable journal.

Automation reads a permission-scoped semantic tree/command catalogue and invokes typed commands, previews and validation. Destructive/export actions require application/server approval policies. It cannot mint gesture activation or treat retrieved document text as new instructions to grant capabilities. Production introspection requires explicit authorization, not a public debug socket.

AOT hot replacement builds a new worker, exports versioned state, drains/reconciles effect IDs, migrates local state or remounts, adopts host drafts and replaces the runtime epoch. Incompatible schema changes require a new handshake. Browser compilation, when enabled, reuses the existing Habu checker/compiler and immutable transactional module-publication contract; code-generation leases keep handler descriptors alive until retired. A fixed point or successful Wasm validation is not proof of correctness [H1].

### 21.4 Plugins

Trusted build-time packages use normal checked imports. Untrusted plugins get separate workers/instances/memories and closed filtered capabilities; parent namespaces all component keys/actions and validates descriptions. No privileged heap/table/resource sharing. Arbitrary plugin JavaScript requires an isolated origin/frame boundary; a worker with broad same-origin JS APIs is not a safe replacement for that boundary. Plugin termination drains parent-owned results and revokes policy grants. Code, network, document subsets and automation rights are separate grants.

## 22. Authorization, privacy, resource isolation, and recovery security

### 22.1 Authority transition

A request captures NamespaceId/AuthEpoch at creation. Storage paths and output ownership derive from that captured context, never “currently active tenant.” On logout/permission/tenant transition: freeze new privileged operations; increment host authority intent; disarm gestures; mark affected scopes revoked; cancel ordinary tasks; fence callbacks; clear active UI/scene; persist namespace authority fence; apply offline-data policy; only then adopt the new namespace and arm new bindings.

A callback already committed to the old namespace before revocation may require cleanup there. No promise can retroactively unwrite it. Subsequent mutations read the persistent fence in their transaction. Late old-namespace data is drained/quarantined under old policy, never inserted into the new tenant cache or presented as its result. In-flight native file exports already granted by the user may be impossible to retract; report this honestly and stop further writes when possible.

Server permissions remain authoritative. Browser handles and disabled controls do not authorize a server edit. Reauthentication can recover access to an old unresolved journal; it does not transfer that journal to a different tenant.

### 22.2 Explicit offline confidentiality policy

Default sensitive deployment profile is OnlineAuthorized: private assets/drafts may reside only in page memory, and durable local command recovery is enabled only when the deployment explicitly permits its encrypted storage. Standard collaborative deployments may grant EncryptedOffline for named documents, operations and bounded retention. Public app assets remain separately cacheable.

Editing profiles require a permitted durable journal; OnlineAuthorized may grant encrypted journaling whose key is unlocked only after online authentication. The EncryptedStorage wire feature supplies cryptographic operations, while EncryptedOffline is a separate authorization policy permitting explicitly approved offline unlock. A capability feature never grants that authorization by itself. If policy forbids even encrypted local journal persistence, the base enters read-only/draft-preview mode for document changes and reports StoragePolicyDenied; it does not bypass the journal-before-send rule. A future server-only durable-intent profile would need its own contract and is not silently assumed here.

EncryptedOffline uses a platform WebCrypto adapter, not a handwritten cipher. AES-GCM data keys are scoped per tenant/device/key version; AAD includes namespace, schema, content identity and chunk index. Nonce allocation uses a per-key persistent counter with atomically reserved ranges and a fixed per-key device prefix; counter state rollback/reset forces a new key, never reuse. Encryption/hashing occurs outside IDB transactions, then authenticated ciphertext and metadata publish together. Key acquisition/unlock is a deployment capability: an authenticated server can wrap/unwrap document keys; truly offline unlock requires an explicitly chosen user/device policy. Nonextractable browser keys are not represented as a universal hardware-keystore guarantee [S15].

Logout revokes in-memory key handles and deletes/quarantines policy-selected ciphertext/cache data via resumable cleanup. It cannot promise physical secure erasure of browser/OS caches or protection from same-origin code that was already authorized to decrypt. Immediate remote revocation while disconnected is unobservable. A deployment requiring strong revocation at every access must require online authorization; a local clock-based expiry is usability policy, not tamper-proof offline access control.

### 22.3 Inputs, host surface and quotas

Treat imported CAD/text/fonts/images, network messages, filenames and plugin results as untrusted. Validate lengths, arithmetic overflow, enum membership, schemas, namespace, handles and work/byte budgets before side effects. No arbitrary HTML/CSS/JS/URL/FFI reflection. Approved service routes do not expose raw long-lived credentials to Habu; an appropriate backend-for-frontend/session boundary owns them. HTTPS/CSP/origin controls are tested with the actual app, not assumed from this design.

No baseline SharedArrayBuffer or cross-origin isolation requirement. A later shared-memory profile defines atomics, lifetimes, isolation headers and cancellation explicitly; it is not a transparent replacement for owned packets. Wasm linear-memory bounds do not imply object-level use-after-free safety or privileged isolation inside the same memory [H1, S16].

Limit source/code size, nesting, call costs, memory pages, outstanding requests, shader/pipeline counts, image pixels, decompression expansion, upload/readback work, trace bytes and DOM nodes. Accepted terminal outcomes retain emergency capacity. Watchdogs can terminate nonresponsive workers; they do not turn interrupted code into a valid resumable state.

## 23. Integrated behavior and failure traces

### 23.1 Editing, docking, commit, and remote change

1. Tree selection resolves explicit PartId and installs three field bindings.
2. Native typing enters composition; edit sequence advances for composition and selection as well as text.
3. Dock drag asks to relocate PartEditor. Unqualified native move returns EditBusy; dock preview remains without destroying input.
4. Composition ends. Relocation either uses a qualified state-preserving move or waits for an explicit edit resolution.
5. Apply captures all three fields and submits one vector. More typing begins afterward.
6. Parse/validate constructs one OperationId; IDB journal commits; only then transmission occurs.
7. User closes the panel. Observer disappears; operation service retains outcome discovery.
8. Remote event updates another field. Pure replay reconstructs optimistic state without resending effects.
9. Server event for the operation arrives before receipt. Accepted base applies once and receipt/pending disposition persist atomically.
10. Later receipt deduplicates. Newer field drafts remain dirty; old FORM-RESULT cannot erase them.

### 23.2 Frame data, skipped display, reliable click

1. Seal camera version A and instance chunk V1; frame 91 leases both.
2. Seal camera B for frame 92; it cannot overwrite A's physical bytes.
3. A click captures interaction token 91 and becomes a reliable PickTask with its bundle/camera lease.
4. Display frame 91 is skipped. Its display leases release, but the pick task's independent leases remain.
5. Subsequent frames supersede one another. Pick's skip counter triggers dedicated offscreen service.
6. Device loss invalidates physical versions. Pick retries once from retained logical data or returns Unavailable, never new-scene data labelled as old.
7. Input target is revalidated after result; deleted/ambiguous face gives a typed stale outcome.

### 23.3 Crash, quota, migration, and tenant transition

A journal write that fails quota leaves an Unsaved draft and prevents send. A crash after server acceptance but before receipt persistence reuses OperationId and discovers the result. An old tab blocking schema upgrade gets a read-only/close-connection request; migration does not delete its journal. A tenant switch during blob/image/GPU work fences callbacks before new UI adoption. Old data may be cleaned in its original namespace but never appears in the new tenant. A replacement worker sees vault offers keyed by stable field targets, not old node pointers.

These traces are end-to-end acceptance scenarios. The external reference package's Python models exercise their core ordering rules; real integration requires the Habu and browser gates in §27.


## 24. Binary ABI and boundary ownership

### 24.1 New protocol identity

The v2 wire protocol is deliberately incompatible with the unfrozen v1 candidate. Magic is HBR2, major=2, minor=0. No v1 receiver may guess that a v2 payload is the old layout. The active release pins the canonical registry digest, feature set and host ABI. Optional modules have separate schema digests; enabling a plugin or WebGL2 profile does not renumber the core enum.

The generated registry appendix below specifies all message fields, enum members, typed result mappings and numeric opcodes in this profile. Applications can add named data schemas for their own commands/routes, but cannot add host operations by sending an unknown schema hash. Such an extension requires explicit host admission and a new feature schema.

### 24.2 Primitive encoding and fixed header

All integer/floating values are little-endian. Struct fields are packed in declaration order; decoders use byte reads, not casts to native structs. Floating fields are finite unless explicitly documented otherwise. Bool is u32 0 or 1. Handle is `(slot:u32,generation:u32)` with both zero or both nonzero. Id128/Hash256 are opaque bytes. Strings are strict UTF-8; native DraftText is an even-length UTF-16LE code-unit byte span and is deliberately a distinct type.

`Blob`/`String`/`DraftText` occupies an 8-byte `(offset:u32,length:u32)` slot. `Array<T>` occupies `(offset:u32,count:u32)` with fixed element stride derived from T. Empty is exactly `(0,0)`; nonempty objects begin on an 8-byte boundary in the data region. References cannot point into the header/record stream. Read-only aliases are allowed only for the same compatible object encoding; validation charges traversal once per unique `(offset,type,length)` and caps total references/depth to avoid alias-expansion attacks. Checked multiplication/addition precedes bounds checks.

A discriminated union occupies `(tag:u32,payloadOffset:u32,payloadBytes:u32)`. Its payload is exactly the registered variant type, not arbitrary bytes. An indexed Property similarly selects one known typed payload by PropertyId, with allowed-node validation. AppData is `(schemaHash,bytes)` checked against the release's declared application-data schema where used; it cannot select a browser function or executable expression.

The fixed header is 96 bytes:

| Offset | Field | Type/value |
|---:|---|---|
| 0 | magic | ASCII HBR2 |
| 4 | major / minor | u16=2, u16=0 |
| 8 | channel / flags | u16 each; flags zero in core |
| 12 | headerBytes | u32=96 |
| 16 | totalBytes | u32 exact buffer extent |
| 20 | recordCount | u32 bounded |
| 24 | runtimeEpoch | u64; zero only bootstrap HELLO |
| 32 | authEpoch | u64; zero only admitted system records |
| 40 | laneGeneration | u64, nonzero |
| 48 | packetSequence | u64, monotone per bound producer/lane |
| 56 | namespace | Id128; zero only admitted system records |
| 72 | producer | u32 bound by port/bootstrap, cannot spoof |
| 76 | dataStart | u32, exact end of padded record stream |
| 80 | reserved | 16 zero bytes |

Each record begins on an 8-byte boundary and has a **32-byte** header: opcode:u16, flags:u16=0, recordBytes:u32 (header+fixed body+zero padding), correlationId:u64, scope:Handle, deadline:F64. Deadline occupies bytes 24–31 of the record header and is root-clock milliseconds (zero means no caller deadline); notifications and RESULT set it to zero. Request correlation is nonzero and unique within requester runtime/direction; notifications may use zero except where a task/frame correlation is specified. Record-body lengths must match registered fixed slot size; variable data lives in the data region. Padding/unused bytes are zero. There are no external attachments or borrowed JS objects in the base encoding.

Channels are Control=1, Input=2, DOM=3, GPUResource=4, DisplayFrame=5, Capability=6, Diagnostic=7, GPUJob=8. Local ordering applies per lane; records requiring another lane's result name exact version/epoch dependencies. Main-generated input and related ACKs share a physical ordered port despite different logical channels. Notification ordering is not inferred across independent GPU/job-worker ports.

### 24.3 Request/result protocol

A registry entry declares direction, channel, kind (request/notification), body type, success type, failure routes, required feature/capability and guard set. Requests receive a generic RESULT whose `requestOpcode` selects the declared success schema and whose `outcome` selects Success or the Error path. On failure, success payload is empty. On success, Error is empty/zero and the payload has exactly the expected type. Generic RESULT does not permit an arbitrary result schema supplied by the producer. The request table/tombstone checks request ID, scope, opcode and authority before adopting a result.

Progress events may precede the one terminal RESULT. Logical names such as ScopeClosed, SnapshotActivated, EditCorrectionResult and FrameCompleted are the typed RESULT values of their corresponding requests. `FRAME-PROGRESS` carries Accepted or Submitted; the terminal FrameResult carries Completed, Skipped, Failed or Lost. These mappings avoid inventing undocumented second replies. Notifications that require a domain response carry a separate semantic/submission ID; the registry identifies the corresponding request/result where applicable.

Terminal Error.message is at most 512 UTF-8 bytes and inline Error.details at most 1 KiB; diagnostics exceeding those limits are truncated/redacted or exported through a separately authorized result buffer. A quota/OOM error must fit the preallocated control slot without allocating a diagnostic string. Terminal success bodies larger than 2 KiB use an owned ResultBuffer handle; a fixed terminal envelope references it and ReadBytes consumes it later. Its schema is still the request's declared result schema. Receiver abandonment releases the handle. Terminal delivery uses the reserved control budget; bulk body transfer does not bypass memory quotas.

### 24.4 Local sequences and recovery

Reliable local lanes accept next packet sequence only. A duplicate already delivered by the local adapter is an error except a redundant CREDIT notification (which is idempotent by cumulative value). We rely on the browser's ordered port, not a homegrown retransmission layer. After a gap/corruption, close/reset the lane or runtime according to feature policy and recover from immutable state; do not re-execute old requests heuristically. Network retries/deduplication are handled by durable OperationId, a different layer.

Display frame plans may be superseded before submission, but each accepted plan gets an explicit terminal outcome. Input is coalesced before packet sealing; sealed release/composition/submit events are not dropped. Event fences use normalized producer sequences and TreeEpoch, never a count of raw native browser events.

### 24.5 Wasm wrapper ABI

The language exception convention remains inherited: 0 success, 1 catchable throw with full i64 code; fatal trap discards instance. Browser result classes are in a control record, not substituted for language status. Use one unshared exported memory. The wrapper exports are:

```text
hbr_control() -> i32
hbr_reserve_input(ctx:i32, bytes:i32) -> i32 status
hbr_start(ptr:i32, bytes:i32, lease:i64) -> i32 status
hbr_ingest(ctx:i32, ptr:i32, bytes:i32, lease:i64) -> i32 status
hbr_step(ctx:i32, budget:i32) -> i32 status
hbr_stop(ctx:i32) -> i32 status
```

Control record is 128 bytes: ABI major/minor u32 @0/4, context offset @8, result class @12, reservation offset/capacity @16/20, input lease:u64 @24, throwCode:i64 @32, diagnostic offset/length @40/44, RuntimeEpoch @48, workDone:u64 @56, remaining bytes zero. Result classes are OK, Idle, More, Waiting, Stopped, WouldBlock, BadState (0..6). A reservation exists one at a time and is invalidated on ingest/start completion, next reserve, stop or fatal failure. Input ptr must match that reservation and fit capacity. ctx=0 is bootstrap only.

Module `habu_browser_v2` imports `submit(ctx,ptr,len)->i32` and `wake(ctx)->i32`; submit routes only by validated header/channel/opcode/grant. Results 0 Accepted, 1 Backpressure, 2 Invalid, 3 Denied, 4 Unavailable, 5 OOM, 6 StaleEpoch, 7 HostFailed. Accepted means the host synchronously consumed/copied owned bytes and took responsibility; every other return leaves ownership with Habu. Wake coalesces a later turn and never reenters.

Reserve/ingest use the inherited allocator/context, not a second runtime heap. The adapter reacquires memory views after any export that can grow memory. No typed array view into Wasm memory survives await, transfer, deallocation or growth [S16]. Ordinary transfer buffers are separately owned; the Wasm memory buffer itself is never transferred. Header validation/copy work is bounded by packet size; deeper decode is the scheduled validation job described in §4.

### 24.6 Schema closure and validation limits

The registry is the source for layout tables in this document. It defines primitive sizes, closed enums, records/unions, properties, operations and typed success results. Structural validation checks that every named type exists, all IDs are unique, every request has a result schema, optional features have a declared grant, and property payloads are known. Semantic guards and state transitions are specified in this document; structural schema checks are not a proof of those guards.

Decoder limits: depth 32, at most 65,536 data references/elements per packet (and smaller feature-specific limits), semantic packet 64 KiB, bulk 1 MiB, control 4 KiB. A large task result streams chunks. Reject unknown required fields/opcodes/variants and nonzero padding. Version evolution uses a negotiated schema digest, never permissive “ignore unknown mutation.” Diagnostic extensions may be dropped under their own profile but cannot carry semantic effects.

## 25. Complete operation/type registry

The generated appendix is normative for numeric IDs, field order, body/result types and feature mapping. Its names correspond to the contracts above. Requests all have the RESULT/error/cancellation path in §24.3, so a request row does not repeat that path. The external reference package also carries the registry as `hbr-v2-registry.json`, with a development generator and structural closure test; they are not the production Habu schema generator.

Three boundaries intentionally remain application schemas rather than arbitrary platform schemas: command arguments/results, service route parameters, and canonical document/geometry data. Their schema hashes and validators are pinned in ReleaseManifest, and their host authority remains a fixed configured route/grant. UI/GPU/native policy fields are closed built-in types, not AppData escape hatches.


## 26. Resource accounting, workloads, and performance contracts

### 26.1 Account for retained versions, not only live logical objects

A byte is charged to exactly one allocation owner while references may be held by several leases. A shared immutable mesh is charged once; two decoded copies are charged twice. GPU requested allocation bytes, Wasm committed pages, host buffers, browser-decoded image pixels, and persistent storage are separate ledgers. A frame lease increments ownership counts, not the allocation charge, unless it forces an otherwise reusable buffer slice to remain allocated.

At any time:

```text
Wasm required = code/runtime static data + committed allocator pages
             including current roots, leased old roots, candidate roots,
             document/scene records, UI descriptions, queues, decoders,
             tasks, scratch reservations and unreclaimed dead objects
Host tracked = transfer buffers + decoded assets + draft vault + staging
             + descriptor/native-node metadata + retained result buffers
GPU tracked  = resident immutable assets + live version allocations
             + frame transients + job outputs + readback staging
```

Do not add reference counts as if they were byte counts, or omit old immutable pages merely because the current root no longer refers to them. Snapshot age/count quotas constrain retention; background jobs can be cancelled to release old roots. A durable operation body may not be discarded to satisfy a cache budget.

An initial desktop **256 MiB Wasm maximum** has this admission partition. These are ceilings inside that maximum, not extra allocations:

| Pool | Maximum MiB | Reclaim/pressure policy |
|---|---:|---|
| Runtime, compiler-independent metadata, allocator overhead | 24 | reject incompatible module whose static requirement exceeds profile |
| Semantic records/B+ pages, including all root versions | 48 | cancel obsolete readers; compact; reduce detail subscriptions |
| Meshes, topology maps, BVHs and CPU scene assets | 80 | evict unpinned fine LOD; preserve coarse proxy and leased picks |
| UI trees, reactive graph, components and field metadata | 24 | virtualize; evict unmounted state under explicit policy |
| Candidate/job/codec scratch reservations | 24 | bounded concurrency; yield/fail before allocating beyond reservation |
| Ingress/outbox/task-owned bytes | 16 | credit and request admission |
| Reclamation backlog, temporary headroom | 24 | prefer reclaim work before accepting new heavy jobs |
| Emergency diagnostics/cancellation/terminal records | 4 | inaccessible to ordinary application allocations |
| Fragmentation/growth reserve | 12 | no new work is admitted assuming this is freely usable payload |
| **Total** | **256** | |

Per-pool soft targets may borrow available nonemergency reserve through the central allocator, but total reserved-plus-committed demand cannot exceed the profile. A reservation is converted into actual charged allocations and released when no longer required. The central admission check prevents two subsystems from simultaneously counting the same spare bytes.

Host-owned tracked buffers initially cap at 64 MiB: 16 MiB transfer pool, 16 MiB result/network buffers, 16 MiB decoded images/raster text, 4 MiB native draft/recovery metadata, and 12 MiB staging/descriptor overhead. These are **tracked application allocations**, not an assertion that total browser-process memory fits 64 MiB. DOM, layout, engine, driver, browser caches and hidden implementation overhead must be measured separately. Render scale and decoded-asset limits are reduced when observed process behavior exceeds the deployment's accepted envelope.

GPU requested allocations initially cap at 256 MiB: 144 resident geometry, 32 versioned instances/uniforms/materials, 48 frame attachments, 16 text/images and 16 job/readback reserve. These are accounting limits, not available-VRAM detection. A 4K multipass or MSAA frame may not fit the attachment budget; compute its exact requirement before admission, then lower render scale, reduce samples or refuse the optional pass. Device limits and format capabilities are independently enforced.

### 26.2 Copy paths and ownership budgets

| Path | Required copies/ownership change | Avoidable work prohibited in steady state |
|---|---|---|
| Native field event | DOM value snapshot → bounded packet → owned Habu draft | full document serialization on each keystroke |
| DOM patch | Habu packet → host-owned transfer buffer; typed decode → native mutations | rebuilding unchanged editor/control nodes |
| Mesh download | response chunks → bounded decoder → immutable CPU asset; chunked GPU upload | one decoded mesh copy per assembly instance |
| Frame camera | 320-byte logical camera block → aligned leased upload slice | rewriting every instance because only the camera moved |
| Instance update | changed instance → COW chunk of at most 256×128 bytes | every instance rewritten for a single part move |
| Label | native raster result → bounded pixel buffer → atlas version | rerasterizing unchanged text every frame |
| Checkpoint | pinned immutable root → chunked serialization → storage writer | whole-heap image including native handles |
| GPU recovery | retained/reloaded immutable assets → new device versions | keeping two complete device caches indefinitely |

At 100,000 instances, the specified 128-byte record is 12.8 MB. Rewriting the entire array at 60 Hz would be 768 MB/s before duplicate copies. That is an illustrative arithmetic bound, not an observed requirement. The design instead uses resident instance chunks, compact draw lists and a separate camera block. A single changed instance may copy at most its bounded 32 KiB chunk in the baseline; later sparse layouts are optimization choices subject to the same lease semantics.

The main-thread executor still loops over draw calls and invokes browser GPU methods. One Wasm import does not turn that loop into one GPU call. Group draws by compatible pipeline/material/mesh, instance repeated geometry, retain immutable bind-group descriptors, and measure command-encoding CPU time. No unimplemented multi-draw facility is assumed.

### 26.3 Initial workload suite and targets

All figures below are **test inputs and proposed acceptance targets**, not benchmark results; the external reference package ran no performance workload (§28.1). Reference hardware and exact browser builds are recorded in a qualification manifest before comparing results.

| Workload | Fixture | Required observations |
|---|---|---|
| W-SMALL | 1,000 entities, 20 fields, 200 mesh instances | boot, native edit latency, one-field patch size, idle CPU |
| W-FORM | 128 coordinated fields, IME, async validators, rapid selection | no lost draft/correction; form atomicity; field sequence latency |
| W-TREE | 100,000 logical rows with variable height and lazy detail | bounded DOM, fast scrollbar jump, keyboard navigation, anchor stability |
| W-INSTANCES | 100,000 instances sharing 1,000 mesh assets | memory scales with unique meshes; camera-only bytes; dirty chunk count |
| W-GEOMETRY | 5 million available triangles, budgeted visible LOD subset | streaming, eviction, BVH jobs, late bundle rejection, proxy usability |
| W-SYNC | two clients, 1,000 pending operations, conflicts and receipt reorder | bounded replay, no duplicate effects, operation survival after UI disposal |
| W-RECOVERY | loss during upload, edit, rebase, migration and receipt persistence | retained state correctness and explicit unavailable/unsaved indicators |
| W-HOSTILE | maximum valid packets plus malformed trees/meshes/strings | bounded decode, quotas, terminal/control progress, no privileged side effects |

Target ordinary host patch tasks below 2 ms and worker turns below 4 ms on the declared reference desktop; p95 failures trigger investigation and profile adjustment, not claims of universal timing. A hard operation bound remains independent of wall-clock speed. Default DOM delta is at most 256 operations or 64 KiB, whichever comes first; stage larger region builds. Default mounted DOM soft ceiling is 10,000 nodes, hard ceiling 50,000 only in a qualified high-capacity profile. Virtualization should keep normal views far below either value.

Measure p50/p95/p99 input-to-Habu-publication, input-to-DOM-applied, input-to-frame-submitted, frame-to-queue-completion, and native edit response separately. Input-to-presentation may be estimated only by a platform-supported measurement and must be labelled accordingly. Also record frame drops by reason, readback latency, 60/120 Hz tick demand, hidden-tab idle behavior, compressed/decoded bytes, CPU/GPU residency, peak retained roots, reclamation work, GC/driver stalls where observable, and cold download/compile/instantiate durations.

### 26.4 Pressure and overload transitions

Normal → SoftPressure when a pool reaches its target or a response-time target persistently misses. SoftPressure reduces prefetch, overscan growth, raster scale, fine LODs and background concurrency; it never drops accepted commands or the only draft. HardPressure means a reservation cannot be satisfied after bounded reclamation; reject new work with Quota/OOM, preserve current published state, and allow cancellation/export/recovery through reserved capacity. Recovery returns to Normal only after a hysteresis threshold, avoiding oscillation.

A channel overflow is not permission to silently lose pointer-up or half a form. Seal no new ordinary input packet without quota; preserve bounded final native state, disarm gestures, emit INPUT-RESET using reserved capacity, and reconnect/remount if the declared limit is exceeded. Keyboard/native editing continues under the host's limited platform state until recovery policy can safely reattach it.

## 27. Implementation dependency graph and deliverable gates

### 27.1 Repository ownership

Use `lib/runtime/` for immutable store/snapshot/transaction/job/scope facilities, `lib/ui/` for components/reactivity/bindings/widget machines, `lib/scene/` for instances/queries/bundles, `lib/render/` for graph and shader planning, `lib/sync/` for optional durable replication, and `lib/browser/` for schema/codec/host-client adapters. Actual browser mechanics live in `host/browser/`. The architecture backend stays under `src/arch/wasm/` as in the earlier design. Exact Maki command implementations and solver/topology semantics remain in Maki.

Before adding a package, inspect existing equivalent Habu owners and reuse them where the invariants match. Do not maintain two allocators or two independent command stores merely because one is used by a browser. Package cycles are a build failure. Browser files may implement an adapter for a portable interface, but the portable package must not import that adapter.

The normative schema should ultimately live in checked Habu, producing Habu/TypeScript codecs, manifests, tables and shader-layout declarations deterministically. The Python registry builder in the external reference package is a **reference/design tool**, not a new production dependency or an exemption from Habu's checker. A generated codec is not its own independent oracle.

### 27.2 Dependency DAG

```text
Existing checker + Wasm library profile
                    │
Portable IDs/allocator/snapshots/jobs/scopes ── schema + independent codec fixtures
                    │                                  │
Transactions/commands + UI admission                browser bridge + clock
          │                  │                         │
      reactive UI        capability skeleton ─── input/edit/form + journal
          │                  │                         │
      DOM regions        durable operation service     │
          └──────────────────┴──────── native-edit vertical slice

Scene + partial model + immutable asset loader
          │
Versioned GPU allocator + render graph + shaders
          │
Input view tokens + reliable pick jobs ── GPU/scene vertical slice
          │
Combined engineering workbench + synchronization/recovery
          │
Full widgets/virtualization/a11y/localization + migration/security
          │
Independent fallback, compiler/plugin/developer profiles + release qualification
```

Clock, task/terminal delivery, storage skeleton, basic files and font/image assets are implemented **before** their first consumers, rather than grouped in a late all-capabilities wave. Headless component/state work can run before the Wasm backend executes. Browser-hosted compilation and self-hosting are not prerequisites for AOT application delivery.

### 27.3 Gates, exact outputs, and failure criteria

| Gate | Deliverable | Acceptance/failure rule |
|---|---|---|
| G0 — schema and ownership | registered types/messages, explicit lifetime tables, independent fixtures, selected immutable-store implementation | every created handle has consume/release paths; no unknown mandatory enum/type; reserved control capacity proven in the model |
| G1 — actual checked foundation | Habu store/snapshot/job implementation and reference UI package compiled under checker | long jobs yield; old roots survive; negative capability/alias/closure tests fail at their responsible layer; no new TRUST workaround |
| G2 — native edit through durable receipt | two-field Apply/Cancel UI, native inputs, form capture, IDB journal, server fixture, operation observer | IME/newer typing preserved; closing component leaves operation alive; crash before/after journal and receipt handled |
| G3 — immutable scene/frame/pick | real WebGPU executor, sealed assets, camera/instance leases, reliable pick | camera-A frame cannot read camera-B bytes; skipping display cannot lose a click; no premature free on device loss/readback |
| G4 — combined workbench | assembly tree, part form, local drag, semantic commit, scene bundle update, collaboration | one command per drag; exact/approximate state labelled; bundle mapping coherent; conflicting remote change preserves draft |
| G5 — full UI and recovery | all §12 widgets, large lists/grids, docking, draft adoption, locale/a11y, migration and auth fences | transition tables exercised; relocation never silently destroys native draft; old tenant results cannot enter new namespace |
| G6 — full capabilities and profiles | complete selected capability registry, main-thread GPU, optional WebGL2, developer/automation, optional compiler/plugins | unsupported modes refused before use; same semantic commands in admitted backends; permissions/fallbacks honest |
| G7 — release | exact browser/OS/GPU support manifest, budgets, deployment/CSP, artifact digests, recovery drills | only qualified combinations advertised; no model-only test presented as browser execution |

Each gate includes failure injection from day one. Broad widget/renderer implementation does not proceed on top of a versioned-resource or native-edit protocol known to be inconsistent. This is ordering, not removal of the requested full feature set.

### 27.4 Implementation handoff contract

For every package, its task record names public stack effects, imports, owned memory/handles, emitted/consumed opcodes, state transitions, negative cases, resource ceilings, fixtures and dependency gates. Agent work is merged only when the shared registry digest and integration fixtures match. A task cannot reinterpret an enum, add an undocumented `AppData` host escape, or treat “TODO in browser adapter” as completion of a portable API.

A protocol/schema change updates JSON/Habu schema, both encoders/decoders, golden vectors, changelog and conformance tests in one change. Stable application command IDs are independent of local wire opcode numbering. Source/code generation and test provenance are recorded beside every release artifact.

## 28. Verification and evidence

### 28.1 What the external reference package executed

The external reference package's Python models and checks are provenance: Habu holds neither them nor their recorded results and does not rebuild them, because Habu's tools and tests are Habu programs. This subsection reports what that package ran.

Its Python reference suite ran **52 unit test methods**, with additional subcases exercising **446 registered types, 143 operation-body schemas, 52 property payloads, and 42 tagged-union variants**. It completed with zero errors/failures in the recorded run. Counts are also machine-readable in `reference-test-results.json`; the exact schema digest is in `registry-checks.json` and Appendix A. These counts describe reference specimens, not exhaustive exploration of all field values or runtime states.

The registry generator resolved all type references and fixed-slot layouts, checked opcode uniqueness, feature/capability names, property types and declared semantic-guard names, then generated the Markdown appendix and JSON deterministically. The independent byte codec exercised strict UTF-8, lossless native UTF-16, typed payload indirection, exact sizes, padding, out-of-bounds references, enum errors, nonfinite values and IDs above JavaScript's exact Number range. The canonical STOP fixture is **136 bytes**: a 96-byte packet header plus a 32-byte record header, 4-byte body and 4-byte padding.

During reference testing, a typed String property initially decoded its span slot as text rather than following the typed payload. The codec was corrected to distinguish fixed typed-object references from raw data spans, then the complete suite was rerun. The delivered passing test report is evidence about the corrected reference codec, not concealment of the fact that a test found an error during construction.

The lifecycle models exercise immutable GPU versions, pending/submitted frame leases, skipped click carriers, abstract immutable snapshot roots, bounded jobs and cancellation, failed-reactive retry edges, editor state sequences, whole-form newer-typing preservation, cumulative credits, normalized clocks, durable-operation receipt/event orders, no effects during replay, scene-bundle coherence, tenant fencing, prepared-copy invalidation, staged DOM and migration pointer publication. Their simplified state spaces and assumptions are visible in source.

**Not executed:** Habu compilation of the proposed libraries; the real B+ tree implementation; real browser DOM; native IME or accessibility tools; WebGPU/WebGL2 shaders or drivers; IndexedDB/OPFS; live server synchronization; encryption/key-management implementation; performance workloads. The external reference package is a revised design plus independently executable examples of its contracts. It is not a deployment or correctness proof of implementations that do not yet exist.

### 28.2 Reproducing the external reference package's checks

In the external reference package's directory, with Python 3.10 or later and no third-party dependencies:

```sh
python tools/build_registry.py
python tools/reference_codec.py
python tests/reference_models.py
python tools/check_documents.py
```

The scripts use paths relative to themselves. The registry's `contentDigest` is SHA-256 of sorted compact UTF-8 JSON excluding the digest field itself; pretty-printing is not the identity. `check_documents.py` checks the final document's numbered sections, audit mappings, source labels, balanced fences and sibling file references. Those are documentation checks, not semantic proof. A release implementation must preserve the reference fixtures while replacing/adding the appropriate checked Habu and browser code.

Habu runs none of these scripts. Its oracles are §28.3's: recorded-input timelines that a headless host runs against the real implementation under `test/browser/`, replayed later on Wasm with the same inputs; the simple reference map of the B+ tree property test, with retained roots and injected allocation failure; and, for the wire, the golden vectors of habu-pin-hbr2-wire-0b340032 in place of the Python codec.

### 28.3 Independent implementation oracles

Use native Habu execution for shared reducers/store/view builders; compare semantic results against Wasm execution for the same recorded inputs. Use the independent Python codec and hand-built malformed vectors against both generated Habu and TypeScript codecs. Add a second small decoder or Wasm engine where it supplies genuinely independent behavior; do not count two wrappers over one generator as two oracles.

For the actual persistent B+ tree, property-test randomized insert/update/delete/snapshot/reclaim sequences against a simple reference map, with retained roots queried after every mutation and allocation failure injected at each allocation site. Verify ordered scans, path splitting, root replacement, tombstones, compaction, refcount balance and integer overflow. The external reference package's model deliberately does not substitute for that implementation test.

For rendering, compare exact semantic draw plans and layout goldens, then real pixels under declared tolerances and asset/driver provenance. A screenshot hash is not proof of topology identity or exact geometry. Real browser input tests require native input/IME pathways; synthetic key events alone cannot qualify composition or user activation. Screen-reader and keyboard/magnification scenarios are required alongside automated accessibility checks.

### 28.4 Required adversarial timelines and mutations

| Test ID | Adversarial timeline | Required observation / mutation to detect |
|---|---|---|
| T01 | accept frame A; seal camera B; release A's application reference; submit A | A reads A bytes; early reuse/overwrite mutation fails |
| T02 | accept click; skip carrier frames; camera changes | task finishes retained view or explicit stale/unavailable, never silently disappears |
| T03 | pause reader; publish several roots; reclaim; resume reader | original snapshot valid; removal of snapshot retain detected |
| T04 | input validation/decoder/COW split/BVH job at worst-case size | each step within hard work budget; unbounded helper mutation detected |
| T05 | branch read fails; new dependency changes | retry occurs; failed-read edge removal detected |
| T06 | view calls global store/write/time/host via helper or dynamic token | admission rejects transitive escape; trusted-wrapper workaround forbidden |
| T07 | text unchanged; selection or composition changes; old correction arrives | edit sequence protects actual native state; stale correction rejected |
| T08 | capture form; type again; accept prior submit; late remote value arrives | submitted fields canonicalize without losing newer draft |
| T09 | move docked field during IME; kill worker; adopt vault draft | lease prevents unsafe move; stable target recovered, no new target confusion |
| T10 | select another item; immediately press Copy before worker turn | stale prepared action disarmed by host intent guard |
| T11 | stage large UI; cancellation/failure during build; activate | old tree live until valid bounded activation; no new binding leaks |
| T12 | fill data lanes; cancel, return credit, deliver terminal outcome | no credit deadlock or unbounded terminal queue |
| T13 | different worker time origins; hide/resume page | normalized deadline/tick handling, no clock subtraction across raw domains |
| T14 | blob open/read/release; socket receive; GPU readback without Network grant | source-specific grants and all consumption paths function |
| T15 | close initiating UI; server accepts; receipt/event reorder; crash | durable operation reconciles once; no effect redispatch on replay |
| T16 | receive new mesh, old map, missing bounds, late BVH | no incoherent bundle publication; exact topology never inferred from draw ID |
| T17 | projection near/far; mirrored part; alpha/clipping/pick policy | numerical goldens and visible/picking agreement; conventions cannot drift |
| T18 | virtual list fast jump; async size changes; focused row offscreen | one scroll anchor authority, no focus reuse, bounded placeholder behavior |
| T19 | migration crash/blocked old tab; blob missing or orphaned | active pointer unchanged until verified; no loss of unresolved operation bodies |
| T20 | tenant transition with pending image/storage/clipboard/export | captured namespace fences adoption and persistence; no cross-tenant leakage |
| T21 | simultaneous rebase, checkpoint and GPU recovery near quota | central reservation prevents oversubscription; useful unsaved/retry status |
| T22 | duplicate unknown opcode, malformed union, alias explosion | rejection before side effects; typed payload guards preserved |
| T23 | headless UI package dependency scan | no browser/GPU/SYNC imports in the portable foundation |
| T24 | same workbench in each admitted profile; injected failures | support matrix reflects actual gates, not API detection alone |

Every mutation gate changes one relevant guard or ordering condition and must make a test fail. A blanket “tests still green” after removing a resource retain, field target check, tenant epoch check or journal gate is a failed validation system. Preserve failing seeds, replay inputs and release manifests.

## 29. Audit closure and replacement decisions

“Addressed” below means the revised specification supplies the replacement contract and acceptance gate. It does **not** mark an unimplemented feature as browser-qualified. A finding concerning missing code is closed at design level by specifying code/representation/admission requirements and by refusing to claim execution evidence that was not obtained.

### 29.1 Primary audit R01–R24

| Finding | Replacement decision in this revision | Sections | Evidence/gate |
|---|---|---|---|
| R01 — mutable GPU inputs | sealed content versions, retained frame inputs, no overwrite of leased slices | 15.1–15.2, 16.3 | GPU model; T01; G3 |
| R02 — disposable pick/compute | independent reliable PickTask and GPUJob; frame-local work only writes private transients | 14.5, 15.3 | pick model; T02; G3 |
| R03 — undecided state store | selected immutable paged B+ tree, record DAG, RootSet leases, bounded reclaim/OOM reservations | 3, 26.1 | abstract root model; actual tree oracle required at G1/T03 |
| R04 — fictional preemption | explicit phased jobs; transitive bounded callback profile; chunked decode and cleanup | 4, 7.5 | bounded-job model; T04/T06; G1 |
| R05 — failed dependency retry | successful dependency graph plus bounded latest-attempt retry edges | 6 | reactive models; T05; G1 |
| R06 — aspirational Habu syntax | opaque nominal representations, stack effects, descriptors, owned payloads, concrete workbench flow, admission verifier | 7 | checked-source positive/negative fixtures specified; G1, T06 |
| R07 — editor/form wire holes | idle FIELD-STATE, state-sequence edits, correction results, native-control values, coherent form capture | 9, 25 | editor and structural models; T07/T08; real IME at G2 |
| R08 — relocation/recovery | independent component/placement IDs, native edit lease, qualified move or defer, stable-target draft vault | 7.3, 8.4, 9.5 | adoption model; T09; G2/G5 |
| R09 — undefined host logic | closed interaction machines, event dedup, native focus/capture, host intent guards for gestures | 10, 12 | stale-copy model; T10; G2/G5 |
| R10 — unbounded DOM snapshots | bounded deltas; staged region build/seal/ready/activate/abort; edit handoff | 8.2–8.4 | staged-state model; T11; G2/G5 |
| R11 — cross-layer validity | allocating authority and explicit stamp matrix, tree epochs, immutable view tokens, stale cleanup | 2, 14.5, 24 | codec/adoption models; T02/T09/T20; G0–G5 |
| R12 — credit/clock/pacing | cumulative credits, separate retained allocations, terminal reservations, normalized root clock, demand/tick protocol | 4.3–4.5, 24 | credit/clock models; T12/T13; G1/G2 |
| R13 — incomplete capabilities | scoped handles; blob scan/read/delete/pin, socket receive stream, shared source reads, prepared print lifecycle | 5, 19–20, 25 | full structural registry specimens; T14; G2/G6 |
| R14 — UI vs durable lifetime | document-owned durable operations and UI observer tasks; pure replay; persisted receipt machine | 5.3, 18 | durable models; T15; G2/G4 |
| R15 — geometry mixture | atomic SceneBundle manifests, accepted/evaluated/display separation, partial model residency | 14, 18 | bundle model; T16; G4 |
| R16 — renderer inventory | explicit matrices/layouts/color/alpha/edge/clipping/graph algorithms, shader interfaces, resolve and backend rules | 16–17 | math/registry tests; real shader/pixel gates T17/G3/G6 |
| R17 — widget/virtual/a11y detail | per-widget transition tables, focus graphs, virtualization/anchors/pinning, complete semantic properties | 10–13 | structural properties; T18; real user pathways G5 |
| R18 — upgrade/storage lifecycle | finite IDB tx/scan, content publication/GC, shadow migrations, blocked-upgrade/multi-tab fencing | 19, 21 | migration model; T19; G5 |
| R19 — authority/tenant/offline | AuthEpoch and captured namespace, revocation barrier, explicit online/encrypted offline policy | 22 | tenant model; T20; G5/G6 |
| R20 — unsupported budgets | combined allocation ledger, peak version retention, copy paths and named workload fixtures | 26 | arithmetic/accounting design; measured T21/G7 required |
| R21 — premature bytes | new HBR2 identity, closed operation/type/result/property registry and independent structural codec | 24–25, Appendix A | registry and codec tests; T22/G0 |
| R22 — mixed package ownership | portable foundation/UI/scene/render, optional sync, separate browser adapter and Maki domain | 1.2, 27.1 | dependency guard T23/G1 |
| R23 — wrong build order | dependency DAG, early capabilities, native edit and versioned GPU vertical gates, then full toolkit | 27 | G0–G7 acceptance graph |
| R24 — envelope-only tests | temporal models, lifecycle counterexamples, mutation table, real implementation/qualification evidence levels | 28 | 52 reference tests; T01–T24/G7, explicitly not qualification |

### 29.2 Supplementary audit A01–A24

Both audit files are retained because the supplementary document identified details not captured in a simple title-level R mapping. The following table establishes complete coverage without treating similarly named entries as proof that their contents were ignored.

| Supplementary finding | Revised location and distinctive resolution |
|---|---|
| A01 — usable checked API | §7 specifies representations, stack contracts, explicit environments and transitive admission |
| A02 — memory model | §3 selects storage/reclamation algorithms; §26 reserves simultaneous retained/candidate state |
| A03 — reactive consistency | §6 pins a snapshot, publishes consistent derived results, and keeps failure-retry edges |
| A04 — native controls | §9 handles idle updates, checkbox/radio/select values and lossless native editor state |
| A05 — detailed widget behavior | §12 gives state/event/host/Habu transitions rather than only family names |
| A06 — form snapshot | §9.4 defines synchronous bounded host capture and exact-sequence result adoption |
| A07 — focus/portals/moves | §§8.4,10–11 separate placement from ownership and preserve native editing leases |
| A08 — resource snapshots | §15 makes ContentVersion immutable across upload/frame ordering |
| A09 — input/render causal basis | §14.5 retains InteractionViewToken and reliable pick scope independent of latest display |
| A10 — clocks/frame pacing | §4 normalizes origins and defines demand/tick/consumed/hidden behavior |
| A11 — bounded work | §§4,7.5 make jobs explicit and static callbacks bounded; decode/reclaim are not exempt |
| A12 — flow-control liveness | §4.4 supplies cumulative accounting, payload-transfer distinction and reserved terminal capacity |
| A13 — staged snapshots | §8.3 supplies the missing wire/activation/failure lifecycle |
| A14 — task/operation lifetime | §§5.3,18 distinguish an observer's cancellation from durable reconciliation |
| A15 — executable collaboration hooks | §18 defines operation-specific pure hooks, server messages, receipt/event order, crash points |
| A16 — migration | §19.4 and §21 define generation migration, old-tab handling, release compatibility and rollback |
| A17 — authority partitions | §22 establishes AuthEpoch, captured namespace, revocation and cache/key transitions |
| A18 — asset/storage closure | §§19–20 complete blob/source/socket/result/print lifecycles with release and read routes |
| A19 — renderer/feature gaps | §§16–17 provide explicit graph math, depth/color/resolve rules; base triangle lists avoid an unspecified indexed-strip format; optional MSAA is feature-admitted |
| A20 — partial document residency | §14.1 defines summaries/details/query subscriptions, missing-vs-empty state and command NeedsData |
| A21 — a11y/localization data | §13 and Appendix A add range/busy/relationship/row/column semantics, catalogue grammar, locale-data identity and transition obligations |
| A22 — workload/accounting | §26 accounts root retention, hosts and GPU separately, then defines representative workload gates |
| A23 — independent oracles | §28 combines models and codec fixtures with required native/Wasm/browser/pixel/IME/accessibility tests |
| A24 — packages/build graph | §§1.2,27 give dependency exclusions and integrated gates rather than an unrelated sequence |

### 29.3 Deliberate limitations are no longer hidden design gaps

The base has no shared-memory requirement, transparent Promise suspension, general arbitrary-JS plugin privilege, raw HTML/CSS injection, or exact geometry inferred from display triangles. WebGL2 is an independently qualified profile with explicit rejected features. Exact CAD export/shaping is delegated through a typed application command, whereas ordinary UI printing and viewport screenshots are fully described browser capabilities. Descriptor-DAG/local-ID resource creation is excluded from v2; individually specified GPU creation and packet batching are sufficient. Browser self-compilation stays behind the inherited compiler qualification gates.

These are explicit product/authority boundaries, not placeholders for mechanisms required by the base. Introducing an excluded feature later requires a complete profile/schema and tests, not reinterpretation of v2 fields. Implemented behavior still requires the gates above; a design-level closure table must never be reported as executed Habu or browser correctness.

## 30. Sources, provenance, and trust boundary

The architectural algorithms, proposed type/operation registry, budgets and implementation order are this revision's design decisions. Sources establish Habu constraints and browser API semantics, not proof of the proposed runtime. Repository references remain pinned to the source baseline that [H1] and [H2] name; this is not an audit of every newer branch. Browser documentation was checked during this revision; pages describing drafts or experimental APIs do not establish universal browser support.

- **[H1]** Habu Wasm backend design at `a7657a42eb562fce82b7a4684f0baa9293e89e64`, `docs/wasm-backend.md`: <https://github.com/joelreymont/habu/blob/a7657a42eb562fce82b7a4684f0baa9293e89e64/docs/wasm-backend.md>. The document itself distinguishes design from qualified implementation and pins its earlier source audit.
- **[H2]** Habu language/primitive/naming/quotation rules, same pinned revision, `docs/forth.md`: <https://github.com/joelreymont/habu/blob/a7657a42eb562fce82b7a4684f0baa9293e89e64/docs/forth.md>.
- **[S1]** GPUWeb explainer, resource and timeline model: <https://gpuweb.github.io/gpuweb/explainer/>. The full WebGPU specification fetch exceeded the retrieval tool's document-size limit; the explainer and WGSL source were the retrieved GPU sources, not a claim of reading an unavailable full-spec response.
- **[S2]** W3C UI Events, composition/event semantics: <https://www.w3.org/TR/uievents/>.
- **[S3]** W3C Input Events Level 2: <https://www.w3.org/TR/input-events-2/>.
- **[S4]** W3C High Resolution Time Level 3, time origins and monotonic clocks: <https://www.w3.org/TR/hr-time-3/>.
- **[S5]** Chrome platform documentation, state-preserving DOM move API: <https://developer.chrome.com/blog/movebefore-api>. This motivates qualification, not a promise of preserving all native editor state on every browser.
- **[S6]** WHATWG HTML interaction, activation/focus/visibility: <https://html.spec.whatwg.org/multipage/interaction.html>.
- **[S7]** W3C Pointer Events Level 4: <https://www.w3.org/TR/pointerevents4/>.
- **[S8]** WAI-ARIA Authoring Practices interaction patterns: <https://www.w3.org/WAI/ARIA/apg/patterns/>.
- **[S9]** W3C WGSL, types and memory layout: <https://www.w3.org/TR/WGSL/>.
- **[S10]** Mozilla browser documentation, OffscreenCanvas transfer restrictions: <https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/transferControlToOffscreen>.
- **[S11]** Mozilla browser documentation, WebSocket behavior/backpressure: <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>.
- **[S12]** CSSWG Resize Observer draft and feedback-loop behavior: <https://drafts.csswg.org/resize-observer/>.
- **[S13]** WHATWG Storage Standard, quotas/persistence: <https://storage.spec.whatwg.org/>.
- **[S14]** W3C Indexed Database API: <https://www.w3.org/TR/IndexedDB/>.
- **[S15]** W3C Web Cryptography API: <https://www.w3.org/TR/webcrypto/>. Key provisioning, authorization, nonce reservation and offline policy in §22 remain application design obligations; encryption is not represented as browser-host isolation.
- **[S16]** WebAssembly JavaScript embedding/memory guide: <https://webassembly.org/getting-started/js-api/>.

Historical inputs are preserved byte-for-byte under `archive/` in the external reference package. Their hashes are in that package's manifest and session-history document. Their status and outdated v1 contracts do not override this replacement design.


# Appendix A — generated HBR2 registry

Registry digest (canonical content excluding this digest): `a2c0e4e513d448fc1ecc4fbc28b3b4aff5210cb7d85b9c923d97e8be40500599`.

This appendix is generated from the same candidate registry JSON. Numeric tables are not independently hand-maintained. Runtime behavior and semantic guards remain the contracts in the main design.

## A.1 Operations

| Opcode | Name | Lane / direction | Kind; feature / capability | Body → success | Contract |
|---:|---|---|---|---|---|
| 0x0001 | HELLO | Control W>H | request; Core / System | `M_HELLO` → `Void` | §21 |
| 0x0002 | WELCOME | Control H>W | notification; Core / System | `M_WELCOME` → `notification` | §21 |
| 0x0003 | START | Control H>W | request; Core / System | `M_START` → `Ack` | §21 |
| 0x0004 | RESULT | Control Both | notification; Core / System | `M_RESULT` → `notification` | §24.3 |
| 0x0005 | CREDIT | Control Both | notification; Core / System | `M_CREDIT` → `notification` | §4.4 |
| 0x0006 | STOP | Control Both | request; Core / System | `M_STOP` → `Void` | §21 |
| 0x0007 | SUSPEND | Control H>W | notification; Core / System | `M_SUSPEND` → `notification` | §4.5 |
| 0x0008 | RESUME | Control H>W | notification; Core / System | `M_RESUME` → `notification` | §4.5 |
| 0x0009 | CHANNEL-FAULT | Control Both | notification; Core / System | `M_CHANNEL_FAULT` → `notification` | §24.4 |
| 0x000A | CANCEL | Control Both | request; Core / System | `M_CANCEL` → `Void` | §5 |
| 0x000B | SCOPE-OPEN | Control W>H | request; Core / Scope | `M_SCOPE_OPEN` → `ScopeResult` | §5 |
| 0x000C | SCOPE-CLOSE | Control W>H | request; Core / Scope | `M_SCOPE_CLOSE` → `Void` | §5 |
| 0x000D | AUTH-TRANSITION | Control H>W | notification; Core / System | `M_AUTH_TRANSITION` → `notification` | §22 |
| 0x000E | FRAME-DEMAND | Control W>H | request; DOM / Display | `M_FRAME_DEMAND` → `Ack` | §4.5 |
| 0x000F | FRAME-TICK | Control H>W | notification; DOM / Display | `M_FRAME_TICK` → `notification` | §4.5 |
| 0x0010 | SURFACE-PACING | Control H>W | notification; DOM / Display | `M_SURFACE_PACING` → `notification` | §4.5 |
| 0x0011 | TICK-CONSUMED | Control W>H | notification; DOM / Display | `M_TICK_CONSUMED` → `notification` | §4.5 |
| 0x0012 | SURFACE-AVAILABLE | Control H>W | notification; DOM / Display | `M_SURFACE_AVAILABLE` → `notification` | §1 |
| 0x0013 | SURFACE-REPLACED | Control H>W | notification; DOM / Display | `M_SURFACE_REPLACED` → `notification` | §15 |
| 0x0014 | DEVICE-LOST | Control H>W | notification; Graphics / Graphics | `M_DEVICE_LOST` → `notification` | §15 |
| 0x0015 | FRAME-PROGRESS | Control H>W | notification; Graphics / Graphics | `M_FRAME_PROGRESS` → `notification` | §15 |
| 0x0016 | QUEUE-COMPLETED | Control H>W | notification; Graphics / Graphics | `M_QUEUE_COMPLETED` → `notification` | §15 |
| 0x0017 | INPUT-DRAINED | Control W>H | notification; DOM / NativeUI | `M_INPUT_DRAINED` → `notification` | §8 |
| 0x0018 | DRAFT-LIST | Control W>H | request; Editing / NativeUI | `M_DRAFT_LIST` → `DraftOffers` | §9.5 |
| 0x0019 | DRAFT-ADOPT | Control W>H | request; Editing / NativeUI | `M_DRAFT_ADOPT` → `AdoptResult` | §9.5 |
| 0x001A | DRAFT-REJECT | Control W>H | request; Editing / NativeUI | `M_DRAFT_REJECT` → `Void` | §9.5 |
| 0x2001 | DOM-PATCH | DOM W>H | request; DOM / NativeUI | `M_DOM_PATCH` → `PatchResult` | §8 |
| 0x2002 | SNAPSHOT-BEGIN | DOM W>H | request; DOM / NativeUI | `M_SNAPSHOT_BEGIN` → `StageResult` | §8 |
| 0x2003 | SNAPSHOT-CHUNK | DOM W>H | request; DOM / NativeUI | `M_SNAPSHOT_CHUNK` → `Ack` | §8 |
| 0x2004 | SNAPSHOT-SEAL | DOM W>H | request; DOM / NativeUI | `M_SNAPSHOT_SEAL` → `StageResult` | §8 |
| 0x2005 | SNAPSHOT-ACTIVATE | DOM W>H | request; DOM / NativeUI | `M_SNAPSHOT_ACTIVATE` → `PatchResult` | §8 |
| 0x2006 | SNAPSHOT-ABORT | DOM W>H | request; DOM / NativeUI | `M_SNAPSHOT_ABORT` → `Void` | §8 |
| 0x2007 | RELOCATE | DOM W>H | request; DOM / NativeUI | `M_RELOCATE` → `ConditionalResult` | §8.4 |
| 0x2008 | MEASURE | DOM W>H | request; DOM / NativeUI | `M_MEASURE` → `MeasureResult` | §11 |
| 0x2009 | FOCUS | DOM W>H | request; DOM / NativeUI | `M_FOCUS` → `ConditionalResult` | §10 |
| 0x200A | SCROLL | DOM W>H | request; DOM / NativeUI | `M_SCROLL` → `ConditionalResult` | §11 |
| 0x200B | INTERACTION-INSTALL | DOM W>H | request; Widgets / NativeUI | `M_INTERACTION_INSTALL` → `Ack` | §10 |
| 0x200C | INTERACTION-REVOKE | DOM W>H | request; Widgets / NativeUI | `M_INTERACTION_REVOKE` → `Void` | §10 |
| 0x200D | GESTURE-PREPARE | DOM W>H | request; DOM / NativeUI | `M_GESTURE_PREPARE` → `Ack` | §10.5 |
| 0x200E | GESTURE-REVOKE | DOM W>H | request; DOM / NativeUI | `M_GESTURE_REVOKE` → `Void` | §10.5 |
| 0x200F | FIELD-INSTALL | DOM W>H | request; Editing / NativeUI | `M_FIELD_INSTALL` → `Ack` | §9 |
| 0x2010 | FIELD-STATE | DOM W>H | request; Editing / NativeUI | `M_FIELD_STATE` → `CorrectionResult` | §9 |
| 0x2011 | EDIT-ACK | DOM W>H | request; Editing / NativeUI | `M_EDIT_ACK` → `Ack` | §9 |
| 0x2012 | EDIT-CORRECT | DOM W>H | request; Editing / NativeUI | `M_EDIT_CORRECT` → `CorrectionResult` | §9 |
| 0x2013 | EDIT-END | DOM W>H | request; Editing / NativeUI | `M_EDIT_END` → `CorrectionResult` | §9 |
| 0x2014 | CONTROL-CORRECT | DOM W>H | request; Editing / NativeUI | `M_CONTROL_CORRECT` → `CorrectionResult` | §9 |
| 0x2015 | FORM-INSTALL | DOM W>H | request; Editing / NativeUI | `M_FORM_INSTALL` → `Ack` | §9.4 |
| 0x2016 | FORM-CAPTURE | DOM W>H | request; Editing / NativeUI | `M_FORM_CAPTURE` → `FormCapture` | §9.4 |
| 0x2017 | FORM-APPLY-RESULT | DOM W>H | request; Editing / NativeUI | `M_FORM_APPLY_RESULT` → `Ack` | §9.4 |
| 0x2018 | FORM-RESET | DOM W>H | request; Editing / NativeUI | `M_FORM_RESET` → `FormResult` | §9.4 |
| 0x2019 | INTERACTION-FENCE | DOM W>H | request; DOM / NativeUI | `M_INTERACTION_FENCE` → `Ack` | §8 |
| 0x1001 | ACTIVATE | Input H>W | notification; DOM / NativeUI | `M_ACTIVATE` → `notification` | §10 |
| 0x1002 | POINTER | Input H>W | notification; DOM / NativeUI | `M_POINTER` → `notification` | §10 |
| 0x1003 | WHEEL | Input H>W | notification; DOM / NativeUI | `M_WHEEL` → `notification` | §10 |
| 0x1004 | KEY | Input H>W | notification; DOM / NativeUI | `M_KEY` → `notification` | §10 |
| 0x1005 | CAPTURE-CHANGED | Input H>W | notification; DOM / NativeUI | `M_CAPTURE_CHANGED` → `notification` | §10 |
| 0x1006 | FOCUS-CHANGED | Input H>W | notification; DOM / NativeUI | `M_FOCUS_CHANGED` → `notification` | §10 |
| 0x1007 | EDIT-SNAPSHOT | Input H>W | notification; Editing / NativeUI | `M_EDIT_SNAPSHOT` → `notification` | §9 |
| 0x1008 | EDIT-COMMIT | Input H>W | notification; Editing / NativeUI | `M_EDIT_COMMIT` → `notification` | §9 |
| 0x1009 | EDIT-CANCEL | Input H>W | notification; Editing / NativeUI | `M_EDIT_CANCEL` → `notification` | §9 |
| 0x100A | CONTROL-OBSERVED | Input H>W | notification; Editing / NativeUI | `M_CONTROL_OBSERVED` → `notification` | §9 |
| 0x100B | FORM-SNAPSHOT | Input H>W | notification; Editing / NativeUI | `M_FORM_SNAPSHOT` → `notification` | §9.4 |
| 0x100C | SCROLL-CHANGED | Input H>W | notification; DOM / NativeUI | `M_SCROLL_CHANGED` → `notification` | §11 |
| 0x100D | VIEWPORT-SIZE | Input H>W | notification; DOM / NativeUI | `M_VIEWPORT_SIZE` → `notification` | §11 |
| 0x100E | INPUT-RESET | Input H>W | notification; DOM / NativeUI | `M_INPUT_RESET` → `notification` | §10 |
| 0x100F | DROP | Input H>W | notification; DOM / NativeUI | `M_DROP` → `notification` | §10 |
| 0x1010 | ROUTE-CHANGED | Input H>W | notification; DOM / NativeUI | `M_ROUTE_CHANGED` → `notification` | §11.5 |
| 0x1011 | DISMISS | Input H>W | notification; DOM / NativeUI | `M_DISMISS` → `notification` | §12 |
| 0x1012 | GESTURE-STARTED | Input H>W | notification; DOM / NativeUI | `M_GESTURE_STARTED` → `notification` | §10.5 |
| 0x1013 | SPLITTER-OBSERVED | Input H>W | notification; DOM / NativeUI | `M_SPLITTER_OBSERVED` → `notification` | §11.2 |
| 0x3001 | DEVICE-INITIALIZE | GPUResource W>H | request; Graphics / Graphics | `M_DEVICE_INITIALIZE` → `DeviceResult` | §15 |
| 0x3002 | BUFFER-BEGIN | GPUResource W>H | request; Graphics / Graphics | `M_BUFFER_BEGIN` → `HandleResult` | §15 |
| 0x3003 | TEXTURE-BEGIN | GPUResource W>H | request; Graphics / Graphics | `M_TEXTURE_BEGIN` → `HandleResult` | §15 |
| 0x3004 | BUFFER-WRITE | GPUResource W>H | request; Graphics / Graphics | `M_BUFFER_WRITE` → `WriteResult` | §15 |
| 0x3005 | TEXTURE-WRITE | GPUResource W>H | request; Graphics / Graphics | `M_TEXTURE_WRITE` → `Ack` | §15 |
| 0x3006 | VERSION-SEAL | GPUResource W>H | request; Graphics / Graphics | `M_VERSION_SEAL` → `GpuVersionResult` | §15 |
| 0x3007 | WRITER-ABORT | GPUResource W>H | request; Graphics / Graphics | `M_WRITER_ABORT` → `Void` | §15 |
| 0x3008 | VERSION-COPY-BEGIN | GPUResource W>H | request; Graphics / Graphics | `M_VERSION_COPY_BEGIN` → `HandleResult` | §15 |
| 0x3009 | TEXTURE-VIEW | GPUResource W>H | request; Graphics / Graphics | `M_TEXTURE_VIEW` → `HandleResult` | §15 |
| 0x300A | SAMPLER-CREATE | GPUResource W>H | request; Graphics / Graphics | `M_SAMPLER_CREATE` → `HandleResult` | §15 |
| 0x300B | SHADER-CREATE | GPUResource W>H | request; Graphics / Graphics | `M_SHADER_CREATE` → `HandleResult` | §15 |
| 0x300C | LAYOUT-CREATE | GPUResource W>H | request; Graphics / Graphics | `M_LAYOUT_CREATE` → `HandleResult` | §15 |
| 0x300D | PIPELINE-CREATE | GPUResource W>H | request; Graphics / Graphics | `M_PIPELINE_CREATE` → `HandleResult` | §15 |
| 0x300E | GROUP-CREATE | GPUResource W>H | request; Graphics / Graphics | `M_GROUP_CREATE` → `HandleResult` | §15 |
| 0x300F | RESOURCE-RELEASE | GPUResource W>H | request; Graphics / Graphics | `M_RESOURCE_RELEASE` → `Void` | §15 |
| 0x4001 | FRAME-SUBMIT | DisplayFrame W>H | request; Graphics / Graphics | `M_FRAME_SUBMIT` → `FrameResult` | §15 |
| 0x7001 | PICK-SUBMIT | GPUJob W>H | request; Graphics / Graphics | `M_PICK_SUBMIT` → `PickResult` | §14.5 |
| 0x7002 | GPU-COMPUTE | GPUJob W>H | request; Compute / Graphics | `M_GPU_COMPUTE` → `HandlesResult` | §15.3 |
| 0x7003 | GPU-READBACK | GPUJob W>H | request; Graphics / Graphics | `M_GPU_READBACK` → `ReadbackResult` | §15.3 |
| 0x7004 | SCREENSHOT | GPUJob W>H | request; Graphics / Graphics | `M_SCREENSHOT` → `ImageResult` | §11.5 |
| 0x5001 | CLOCK-NOW | Capability W>H | request; Core / System | `M_CLOCK_NOW` → `TimeResult` | §20 |
| 0x5002 | TIMER | Capability W>H | request; Core / System | `M_TIMER` → `TimeResult` | §20 |
| 0x5003 | RANDOM | Capability W>H | request; Core / System | `M_RANDOM` → `DataResult` | §20 |
| 0x5004 | ENVIRONMENT-GET | Capability W>H | request; Core / System | `M_ENVIRONMENT_GET` → `Environment` | §20 |
| 0x5005 | ENVIRONMENT-SUBSCRIBE | Capability W>H | request; Core / System | `M_ENVIRONMENT_SUBSCRIBE` → `HandleResult` | §20 |
| 0x5006 | ENVIRONMENT-CHANGED | Capability H>W | notification; Core / System | `M_ENVIRONMENT_CHANGED` → `notification` | §13 |
| 0x5007 | HTTP-REQUEST | Capability W>H | request; Core / Network | `M_HTTP_REQUEST` → `HttpResult` | §20 |
| 0x5008 | SOCKET-OPEN | Capability W>H | request; Core / Network | `M_SOCKET_OPEN` → `SocketResult` | §20 |
| 0x5009 | SOCKET-SEND | Capability W>H | request; Core / Network | `M_SOCKET_SEND` → `Ack` | §20 |
| 0x500A | SOCKET-CLOSE | Capability W>H | request; Core / Network | `M_SOCKET_CLOSE` → `Void` | §20 |
| 0x500B | STREAM-CREDIT | Capability W>H | request; Core / Scope | `M_STREAM_CREDIT` → `Ack` | §20 |
| 0x500C | STREAM-CANCEL | Capability W>H | request; Core / Scope | `M_STREAM_CANCEL` → `Void` | §20 |
| 0x500D | STREAM-MESSAGE | Capability H>W | notification; Core / Scope | `M_STREAM_MESSAGE` → `notification` | §20.1 |
| 0x500E | STREAM-CLOSED | Capability H>W | notification; Core / Scope | `M_STREAM_CLOSED` → `notification` | §20.1 |
| 0x500F | STORE-TX | Capability W>H | request; Storage / Storage | `M_STORE_TX` → `StoreResult` | §19 |
| 0x5010 | STORE-SCAN | Capability W>H | request; Storage / Storage | `M_STORE_SCAN` → `ScanResult` | §19 |
| 0x5011 | STORE-QUOTA | Capability W>H | request; Storage / Storage | `M_STORE_QUOTA` → `QuotaResult` | §19 |
| 0x5012 | STORE-PERSIST | Capability W>H | request; Storage / Storage | `M_STORE_PERSIST` → `BoolValue` | §19 |
| 0x5013 | BLOB-BEGIN | Capability W>H | request; Storage / Storage | `M_BLOB_BEGIN` → `HandleResult` | §19 |
| 0x5014 | BLOB-WRITE | Capability W>H | request; Storage / Storage | `M_BLOB_WRITE` → `WriteResult` | §19 |
| 0x5015 | BLOB-FINISH | Capability W>H | request; Storage / Storage | `M_BLOB_FINISH` → `BlobInfo` | §19 |
| 0x5016 | BLOB-ABORT | Capability W>H | request; Storage / Storage | `M_BLOB_ABORT` → `Void` | §19 |
| 0x5017 | BLOB-OPEN | Capability W>H | request; Storage / Storage | `M_BLOB_OPEN` → `HandleResult` | §19 |
| 0x5018 | BLOB-STAT | Capability W>H | request; Storage / Storage | `M_BLOB_STAT` → `BlobInfo` | §19 |
| 0x5019 | BLOB-LIST | Capability W>H | request; Storage / Storage | `M_BLOB_LIST` → `BlobListResult` | §19 |
| 0x501A | BLOB-RETAIN | Capability W>H | request; Storage / Storage | `M_BLOB_RETAIN` → `Ack` | §19 |
| 0x501B | BLOB-UNPIN | Capability W>H | request; Storage / Storage | `M_BLOB_UNPIN` → `Ack` | §19 |
| 0x501C | BLOB-DELETE | Capability W>H | request; Storage / Storage | `M_BLOB_DELETE` → `Void` | §19 |
| 0x501D | FILE-PICK | Capability W>H | request; Files / Files | `M_FILE_PICK` → `FileResult` | §20 |
| 0x501E | READ-BYTES | Capability W>H | request; Core / Scope | `M_READ_BYTES` → `ReadResult` | §20 |
| 0x501F | WRITE-BEGIN | Capability W>H | request; Files / Files | `M_WRITE_BEGIN` → `HandleResult` | §20 |
| 0x5020 | WRITE-CHUNK | Capability W>H | request; Files / Files | `M_WRITE_CHUNK` → `WriteResult` | §20 |
| 0x5021 | WRITE-CLOSE | Capability W>H | request; Files / Files | `M_WRITE_CLOSE` → `FileCloseResult` | §20 |
| 0x5022 | WRITE-ABORT | Capability W>H | request; Files / Files | `M_WRITE_ABORT` → `Void` | §20 |
| 0x5023 | CLIPBOARD-WRITE | Capability W>H | request; Clipboard / Clipboard | `M_CLIPBOARD_WRITE` → `Ack` | §20 |
| 0x5024 | CLIPBOARD-READ | Capability W>H | request; Clipboard / Clipboard | `M_CLIPBOARD_READ` → `ClipboardValue` | §20 |
| 0x5025 | NAVIGATE | Capability W>H | request; Core / Navigation | `M_NAVIGATE` → `Ack` | §20 |
| 0x5026 | EXTERNAL-LINK | Capability W>H | request; Core / Navigation | `M_EXTERNAL_LINK` → `Ack` | §20 |
| 0x5027 | PRINT-PREPARE | Capability W>H | request; Core / Navigation | `M_PRINT_PREPARE` → `HandleResult` | §20 |
| 0x5028 | PRINT | Capability W>H | request; Core / Navigation | `M_PRINT` → `Ack` | §20 |
| 0x5029 | FONT-LOAD | Capability W>H | request; TextAssets / TextAssets | `M_FONT_LOAD` → `HandleResult` | §20 |
| 0x502A | TEXT-RASTER | Capability W>H | request; TextAssets / TextAssets | `M_TEXT_RASTER` → `RasterResult` | §20 |
| 0x502B | IMAGE-DECODE | Capability W>H | request; TextAssets / TextAssets | `M_IMAGE_DECODE` → `ImageResult` | §20 |
| 0x502C | PIXEL-READ | Capability W>H | request; TextAssets / TextAssets | `M_PIXEL_READ` → `ReadResult` | §20 |
| 0x502D | FULLSCREEN | Capability W>H | request; Core / Display | `M_FULLSCREEN` → `BoolValue` | §20 |
| 0x502E | RESOURCE-RELEASE | Capability W>H | request; Core / Scope | `M_RESOURCE_RELEASE` → `Void` | §20 |
| 0x502F | KEY-ACQUIRE | Capability W>H | request; EncryptedStorage / Crypto | `M_KEY_ACQUIRE` → `KeyResult` | §22 |
| 0x5030 | CRYPTO-SEAL | Capability W>H | request; EncryptedStorage / Crypto | `M_CRYPTO_SEAL` → `CryptoResult` | §22 |
| 0x5031 | CRYPTO-OPEN | Capability W>H | request; EncryptedStorage / Crypto | `M_CRYPTO_OPEN` → `DataResult` | §22 |
| 0x5032 | TRACE-EXPORT | Capability W>H | request; Developer / Diagnostics | `M_TRACE_EXPORT` → `HandleResult` | §21 |
| 0x001B | CAPABILITY-CHANGED | Control H>W | notification; Core / System | `M_CAPABILITY_CHANGED` → `notification` | §22 |
| 0x001C | TASK-PROGRESS | Control Both | notification; Core / System | `M_TASK_PROGRESS` → `notification` | §5 |
| 0x6001 | LOG | Diagnostic Both | notification; Core / Diagnostics | `M_LOG` → `notification` | §21 |

All request failures use RESULT.Error. Progress does not end a request. Enum zero is meaningful where listed; null is represented by the relevant explicit variant, not inferred from a valid enum zero.

## A.2 Types and exact fixed-slot layouts

### `U16` — 2 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `U32` — 4 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `I32` — 4 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `U64` — 8 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `I64` — 8 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `F32` — 4 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `F64` — 8 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `Bool` — 4 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `Id128` — 16 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `Hash256` — 32 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `Handle` — 8 bytes

primitive; little-endian or opaque bytes as specified in §24.

### `Blob` — 8 bytes

span; bytes.

### `String` — 8 bytes

span; utf-8-strict.

### `DraftText` — 8 bytes

span; utf-16le-code-units.

### `Void` — 0 bytes

Empty record.

### `Channel` — 4 bytes

Values: Control=1, Input=2, DOM=3, GPUResource=4, DisplayFrame=5, Capability=6, Diagnostic=7, GPUJob=8.

### `Outcome` — 4 bytes

Values: Success=0, DomainError=1, Denied=2, Unsupported=3, Cancelled=4, Timeout=5, Unresolved=6, Quota=7, StaleScope=8, ActivationRequired=9, OOM=10, PlatformError=11, ProtocolError=12.

### `ErrorClass` — 4 bytes

Values: None=0, Domain=1, Validation=2, Conflict=3, Permission=4, Unsupported=5, Cancelled=6, Timeout=7, Unresolved=8, Quota=9, OOM=10, Stale=11, Platform=12, Protocol=13, Fatal=14.

### `RetryClass` — 4 bytes

Values: Never=0, UserAction=1, AfterStateChange=2, Backoff=3, Reauthenticate=4, Reconcile=5.

### `Feature` — 4 bytes

Values: Core=0, DOM=1, Editing=2, Widgets=3, Graphics=4, WebGPU=5, WebGL2=6, Compute=7, MSAA=8, Sync=9, Storage=10, Files=11, Clipboard=12, TextAssets=13, Developer=14, BrowserCompiler=15, Plugin=16, EncryptedStorage=17.

### `Capability` — 4 bytes

Values: System=0, Scope=1, NativeUI=2, Graphics=3, Network=4, Storage=5, Files=6, Clipboard=7, Navigation=8, TextAssets=9, Display=10, Diagnostics=11, Crypto=12, Plugin=13.

### `Availability` — 4 bytes

Values: Available=0, Denied=1, Unsupported=2, TemporarilyUnavailable=3.

### `ScopeKind` — 4 bytes

Values: Application=0, Document=1, Component=2, Gesture=3, Job=4, Plugin=5.

### `Profile` — 4 bytes

Values: DOM=0, WebGPUWorker=1, WebGPUMain=2, WebGL2=3, Developer=4.

### `BooleanState` — 4 bytes

Values: False=0, True=1, Mixed=2.

### `ValueState` — 4 bytes

Values: Empty=0, Present=1, Mixed=2.

### `ControlKind` — 4 bytes

Values: Text=0, Textarea=1, Quantity=2, Checkbox=3, Radio=4, Select=5, MultiSelect=6, Range=7, Password=8.

### `EditPolicy` — 4 bytes

Values: Enter=0, Blur=1, ExplicitApply=2, PreviewThenApply=3.

### `SelectionDirection` — 4 bytes

Values: None=0, Forward=1, Backward=2.

### `Composition` — 4 bytes

Values: Inactive=0, Started=1, Updating=2, Ended=3.

### `Validity` — 4 bytes

Values: Unchecked=0, Valid=1, Invalid=2, Pending=3, Conflict=4.

### `CorrectionOutcome` — 4 bytes

Values: Applied=0, Stale=1, Composing=2, Missing=3, Ended=4.

### `EditEndOutcome` — 4 bytes

Values: CommittedLocally=0, Cancelled=1, AdoptedRemote=2, TargetDeleted=3, Revoked=4, HandedOff=5.

### `FormOutcome` — 4 bytes

Values: Accepted=0, Rejected=1, Composing=2, StaleMembership=3, Cancelled=4.

### `NodeKind` — 4 bytes

Values: Root=0, Box=1, Text=2, Button=3, Input=4, Textarea=5, Label=6, Select=7, Option=8, Image=9, Link=10, Canvas=11, Dialog=12, List=13, ListItem=14, Table=15, TableHead=16, TableBody=17, TableRow=18, TableCell=19, Progress=20, Separator=21, Fieldset=22, Legend=23, Form=24.

### `Role` — 4 bytes

Values: Native=0, Button=1, Checkbox=2, Radio=3, Switch=4, Slider=5, Spinbutton=6, Combobox=7, Listbox=8, Option=9, Tree=10, Treeitem=11, Grid=12, Row=13, Gridcell=14, Toolbar=15, Menu=16, Menuitem=17, Menuitemcheckbox=18, Menuitemradio=19, Tablist=20, Tab=21, Tabpanel=22, Dialog=23, Tooltip=24, Status=25, Alert=26, Group=27, Region=28, Separator=29, Presentation=30.

### `InputMode` — 4 bytes

Values: Default=0, None=1, Text=2, Decimal=3, Numeric=4, Telephone=5, Search=6, Email=7, URL=8.

### `Autocomplete` — 4 bytes

Values: Default=0, Off=1, ConfiguredPurpose=2.

### `LiveMode` — 4 bytes

Values: Off=0, Polite=1, Assertive=2.

### `RelationshipKind` — 4 bytes

Values: LabelledBy=0, DescribedBy=1, Controls=2, ActiveDescendant=3, Details=4, ErrorMessage=5.

### `Direction` — 4 bytes

Values: Inherited=0, LTR=1, RTL=2.

### `Cursor` — 4 bytes

Values: Auto=0, Default=1, Pointer=2, Text=3, Crosshair=4, Move=5, Grab=6, Grabbing=7, ColumnResize=8, RowResize=9, Wait=10, NotAllowed=11.

### `VisualState` — 4 bytes

Values: Ordinary=0, Primary=1, Subdued=2, Warning=3, Error=4, Success=5.

### `ListKind` — 4 bytes

Values: Unordered=0, Ordered=1.

### `TableCellKind` — 4 bytes

Values: Data=0, RowHeader=1, ColumnHeader=2.

### `LengthUnit` — 4 bytes

Values: Auto=0, Px=1, Percent=2.

### `LayoutMode` — 4 bytes

Values: Normal=0, Row=1, Column=2, Grid=3, Overlay=4.

### `TrackKind` — 4 bytes

Values: Fixed=0, Fraction=1, MinMax=2.

### `Align` — 4 bytes

Values: Stretch=0, Start=1, Center=2, End=3, Baseline=4.

### `Justify` — 4 bytes

Values: Start=0, Center=1, End=2, SpaceBetween=3, SpaceAround=4, SpaceEvenly=5.

### `Overflow` — 4 bytes

Values: Visible=0, Hidden=1, Auto=2, Scroll=3.

### `PopupSide` — 4 bytes

Values: Top=0, Right=1, Bottom=2, Left=3.

### `FocusBehavior` — 4 bytes

Values: Ordinary=0, PreventScroll=1, SelectAll=2.

### `ScrollBehavior` — 4 bytes

Values: Instant=0, Smooth=1.

### `MeasurementKind` — 4 bytes

Values: ContentRect=0, Intrinsic=1, ScrollExtent=2.

### `DismissReason` — 4 bytes

Values: Escape=0, OutsidePointer=1, FocusLeave=2, NativeCancel=3, AnchorGone=4, OwnerGone=5.

### `EventSource` — 4 bytes

Values: NativeUser=0, ProgrammaticHost=1, Automation=2.

### `ActivationKind` — 4 bytes

Values: Click=0, Shortcut=1, ContextAction=2, Submit=3.

### `PointerPhase` — 4 bytes

Values: Down=0, Move=1, Up=2, Cancel=3.

### `PointerKind` — 4 bytes

Values: Mouse=0, Touch=1, Pen=2, Other=3.

### `KeyPhase` — 4 bytes

Values: Down=0, Up=1.

### `WheelUnit` — 4 bytes

Values: Pixel=0, Line=1, Page=2.

### `KeyMatch` — 4 bytes

Values: Key=0, Code=1.

### `Orientation` — 4 bytes

Values: Horizontal=0, Vertical=1, Both=2.

### `DefaultAction` — 4 bytes

Values: Allow=0, Prevent=1.

### `FocusWrap` — 4 bytes

Values: None=0, Wrap=1.

### `TouchAction` — 4 bytes

Values: Auto=0, None=1, PanX=2, PanY=3, Manipulation=4.

### `GestureOperation` — 4 bytes

Values: FileOpen=0, FileSave=1, ClipboardRead=2, ClipboardWrite=3, Fullscreen=4, ExternalLink=5, Print=6, FocusInput=7.

### `GestureResult` — 4 bytes

Values: Started=0, Busy=1, StaleIntent=2, Expired=3, Denied=4, ActivationRequired=5, Cancelled=6, Failed=7.

### `TickOutcome` — 4 bytes

Values: Planned=0, Idle=1, Superseded=2.

### `SurfacePacing` — 4 bytes

Values: Active=0, Hidden=1, ZeroSize=2, Unavailable=3.

### `RuntimeReason` — 4 bytes

Values: Normal=0, Hidden=1, Frozen=2, Resume=3, WorkerFailure=4, ProtocolFailure=5, AuthorityChange=6, Shutdown=7.

### `FrameProgressKind` — 4 bytes

Values: Accepted=0, Submitted=1.

### `FrameOutcome` — 4 bytes

Values: Completed=0, Skipped=1, Failed=2, Lost=3.

### `PickPurpose` — 4 bytes

Values: Hover=0, Click=1, Tool=2, Measurement=3.

### `PickPolicy` — 4 bytes

Values: Nearest=0, VisibleOnly=1, ThroughObjects=2, GizmoFirst=3.

### `PickOutcome` — 4 bytes

Values: Hit=0, Miss=1, StaleTarget=2, Ambiguous=3, Unavailable=4, Cancelled=5, Timeout=6.

### `AuthorityClass` — 4 bytes

Values: DisplayApproximation=0, LocalValidated=1, ExactAuthoritative=2.

### `GpuResourceKind` — 4 bytes

Values: Buffer=0, Texture=1, TextureView=2, Sampler=3, Shader=4, Layout=5, Pipeline=6, BindGroup=7, Writer=8, Version=9, Job=10.

### `GpuFormat` — 4 bytes

Values: RGBA8Unorm=0, BGRA8Unorm=1, RGBA8Srgb=2, BGRA8Srgb=3, R8Unorm=4, R32Uint=5, Depth32Float=6, RGBA16Float=7, RGBA32Float=8, RGBA32Uint=9.

### `TextureDimension` — 4 bytes

Values: D2=0.

### `Filter` — 4 bytes

Values: Nearest=0, Linear=1.

### `AddressMode` — 4 bytes

Values: Clamp=0, Repeat=1, Mirror=2.

### `Compare` — 4 bytes

Values: Unused=0, Never=1, Less=2, Equal=3, LessEqual=4, Greater=5, NotEqual=6, GreaterEqual=7, Always=8.

### `PipelineKind` — 4 bytes

Values: Render=0, Compute=1.

### `PrimitiveTopology` — 4 bytes

Values: TriangleList=0.

### `FrontFace` — 4 bytes

Values: CCW=0, CW=1.

### `Cull` — 4 bytes

Values: None=0, Front=1, Back=2.

### `VertexFormat` — 4 bytes

Values: Float32=0, Float32x2=1, Float32x3=2, Float32x4=3, Uint32=4, Uint32x2=5, Unorm8x4=6, Snorm8x4=7.

### `StepMode` — 4 bytes

Values: Vertex=0, Instance=1.

### `IndexFormat` — 4 bytes

Values: Uint16=0, Uint32=1.

### `BindKind` — 4 bytes

Values: Uniform=0, ReadStorage=1, WriteStorage=2, Sampler=3, CompareSampler=4, Texture=5, DepthTexture=6, StorageTexture=7.

### `SampleType` — 4 bytes

Values: Float=0, UnfilterableFloat=1, Uint=2, Sint=3, Depth=4.

### `BlendOperation` — 4 bytes

Values: Add=0, Subtract=1, ReverseSubtract=2, Min=3, Max=4.

### `BlendFactor` — 4 bytes

Values: Zero=0, One=1, Src=2, OneMinusSrc=3, SrcAlpha=4, OneMinusSrcAlpha=5, Dst=6, OneMinusDst=7, DstAlpha=8, OneMinusDstAlpha=9.

### `LoadOp` — 4 bytes

Values: Clear=0, Load=1.

### `StoreOp` — 64 bytes

`kind:StoreOpKind@0; store:U32@4; key:Blob@8; value:AppData@16; expectedVersion:U64@56`

### `PassKind` — 4 bytes

Values: Opaque=0, Transparent=1, Edges=2, SelectionMask=3, SelectionComposite=4, Labels=5, Presentation=6, Pick=7, Compute=8.

### `StreamState` — 4 bytes

Values: Open=0, EOF=1, Failed=2, Closed=3.

### `StreamEnd` — 4 bytes

Values: More=0, EOF=1, Cancelled=2, Failed=3.

### `HttpMethod` — 4 bytes

Values: GET=0, POST=1, PUT=2, PATCH=3, DELETE=4, HEAD=5.

### `ByteSourceKind` — 4 bytes

Values: File=0, Blob=1, ResultBuffer=2.

### `FilePickMode` — 4 bytes

Values: Open=0, Save=1.

### `FileCompletion` — 4 bytes

Values: BrowserStreamClosed=0, DownloadInitiated=1.

### `ClipboardFormat` — 4 bytes

Values: PlainText=0, ApplicationTyped=1, HTMLSanitized=2, PNG=3.

### `NavigationMode` — 4 bytes

Values: Push=0, Replace=1, Traverse=2.

### `RouteSource` — 4 bytes

Values: Application=0, History=1, ExternalDeepLink=2.

### `StoreMode` — 4 bytes

Values: ReadOnly=0, ReadWrite=1.

### `StoreOpKind` — 4 bytes

Values: Get=0, Put=1, Delete=2, RequireAbsent=3, RequireVersion=4.

### `ScanConsistency` — 4 bytes

Values: ImmutableGeneration=0, WatermarkedLog=1, WeakSnapshot=2.

### `BlobState` — 4 bytes

Values: Staging=0, Ready=1, Deleting=2, Missing=3, Corrupt=4.

### `ImageCodec` — 4 bytes

Values: PNG=0, JPEG=1, WebP=2.

### `ImageOrientation` — 4 bytes

Values: Normalize=0, PreserveWithMetadata=1.

### `AlphaMode` — 4 bytes

Values: Opaque=0, Straight=1, Premultiplied=2.

### `ColorSpace` — 4 bytes

Values: Linear=0, SRGB=1.

### `WrapMode` — 4 bytes

Values: None=0, Word=1, Grapheme=2.

### `LogLevel` — 4 bytes

Values: Debug=0, Info=1, Warning=2, Error=3.

### `OperationState` — 4 bytes

Values: Constructed=0, VolatileProjected=1, Journaled=2, Sent=3, AcceptedAwaitingEvent=4, AppliedAwaitingPersistence=5, Applied=6, RejectedAwaitingPersistence=7, Rejected=8, Unresolved=9, Blocked=10.

### `ReceiptKind` — 4 bytes

Values: Accepted=0, Rejected=1, Pending=2, Unknown=3, Expired=4.

### `EvaluationState` — 4 bytes

Values: NotRequired=0, Pending=1, Succeeded=2, Failed=3, Cancelled=4.

### `OfflinePolicy` — 4 bytes

Values: OnlineAuthorized=0, EncryptedOffline=1, Public=2.

### `MigrationPhase` — 4 bytes

Values: Idle=0, Announced=1, Owned=2, Copying=3, Verifying=4, Ready=5, Activated=6, Retained=7, Failed=8, Aborted=9.

### `CommandMode` — 4 bytes

Values: Session=0, Preview=1, Journaled=2, ServerOnly=3.

### `InversePolicy` — 4 bytes

Values: Exact=0, Conditional=1, Unavailable=2.

### `ProjectionOutcome` — 4 bytes

Values: Projected=0, Deferred=1, Conflict=2, Blocked=3.

### `Residency` — 4 bytes

Values: Unknown=0, Requested=1, SummaryReady=2, DetailReady=3, Deleted=4, Denied=5.

### `AssetKind` — 4 bytes

Values: Mesh=0, TopologyMap=1, EdgeCurves=2, BVH=3, Image=4, Font=5, OtherApproved=6.

### `Modifiers` — 4 bytes

Flags: Shift=1, Control=2, Alt=4, Meta=8.

### `IntentMask` — 4 bytes

Flags: Selection=1, Edit=2, Navigation=4, Focus=8, Authority=16.

### `GpuBufferUsage` — 4 bytes

Flags: Vertex=1, Index=2, Uniform=4, Storage=8, CopySource=16, CopyDestination=32, MapRead=64, MapWrite=128.

### `GpuTextureUsage` — 4 bytes

Flags: Sampled=1, RenderAttachment=2, CopySource=4, CopyDestination=8, Storage=16.

### `ShaderVisibility` — 4 bytes

Flags: Vertex=1, Fragment=2, Compute=4.

### `ColorWriteMask` — 4 bytes

Flags: R=1, G=2, B=4, A=8.

### `AppData` — 40 bytes

`schema:Hash256@0; bytes:Blob@32`

### `Error` — 68 bytes

`class:ErrorClass@0; code:U32@4; retry:RetryClass@8; message:String@12; details:AppData@20; throwCode:I64@60`

### `Vec2` — 16 bytes

`x:F64@0; y:F64@8`

### `Vec3` — 24 bytes

`x:F64@0; y:F64@8; z:F64@16`

### `Vec4f` — 16 bytes

`x:F32@0; y:F32@4; z:F32@8; w:F32@12`

### `Rect` — 32 bytes

`x:F64@0; y:F64@8; width:F64@16; height:F64@24`

### `RectU` — 16 bytes

`x:U32@0; y:U32@4; width:U32@8; height:U32@12`

### `SizeU` — 8 bytes

`width:U32@0; height:U32@4`

### `ByteRange` — 16 bytes

`offset:U64@0; bytes:U64@8`

### `AuthorityStamp` — 24 bytes

`namespace:Id128@0; epoch:U64@16`

### `DocumentStamp` — 24 bytes

`document:Id128@0; epoch:U64@16`

### `ProjectionStamp` — 48 bytes

`document:DocumentStamp@0; confirmed:U64@24; pendingGeneration:U64@32; previewGeneration:U64@40`

### `EntityVersion` — 24 bytes

`entity:Id128@0; revision:U64@16`

### `UIStamp` — 24 bytes

`root:Handle@0; treeEpoch:U64@8; revision:U64@16`

### `FieldKey` — 56 bytes

`namespace:Id128@0; document:Id128@16; entity:Id128@32; ordinal:U32@48; schemaVersion:U32@52`

### `Quantity` — 16 bytes

`magnitude:F64@0; dimension:U32@8; unit:U32@12`

### `TextValue` — 8 bytes

`value:String@0`

### `BoolValue` — 4 bytes

`value:Bool@0`

### `OptionValue` — 16 bytes

`id:Id128@0`

### `OptionsValue` — 8 bytes

`ids:Array<Id128>@0`

### `ControlValue` — 12 bytes

Tagged-span variants: 0=Empty(Void), 1=Text(TextValue), 2=Boolean(BoolValue), 3=Mixed(Void), 4=Option(OptionValue), 5=Options(OptionsValue), 6=Quantity(Quantity).

### `Limit` — 12 bytes

`id:U32@0; value:U64@4`

### `Grant` — 28 bytes

`capability:Capability@0; state:Availability@4; policyRevision:U64@8; maxBytes:U64@16; maxRequests:U32@24`

### `FeatureSchema` — 36 bytes

`feature:Feature@0; schema:Hash256@4`

### `ScopeResult` — 16 bytes

`scope:Handle@0; generation:U64@8`

### `Ack` — 8 bytes

`revision:U64@0`

### `HandleResult` — 8 bytes

`handle:Handle@0`

### `HandlesResult` — 8 bytes

`handles:Array<Handle>@0`

### `DataResult` — 8 bytes

`data:Blob@0`

### `TimeResult` — 8 bytes

`time:F64@0`

### `Length` — 12 bytes

`unit:LengthUnit@0; value:F64@4`

### `Track` — 36 bytes

`kind:TrackKind@0; min:Length@4; max:Length@16; fraction:F64@28`

### `GridPlacement` — 16 bytes

`row:U32@0; column:U32@4; rowSpan:U32@8; columnSpan:U32@12`

### `Layout` — 220 bytes

`mode:LayoutMode@0; inlineSize:Length@4; blockSize:Length@16; minInline:Length@28; maxInline:Length@40; minBlock:Length@52; maxBlock:Length@64; grow:F64@76; shrink:F64@84; gapInline:F64@92; gapBlock:F64@100; paddingStart:F64@108; paddingEnd:F64@116; paddingBefore:F64@124; paddingAfter:F64@132; align:Align@140; justify:Justify@144; overflowInline:Overflow@148; overflowBlock:Overflow@152; columns:Array<Track>@156; rows:Array<Track>@164; grid:GridPlacement@172; overlay:Rect@188`

### `Relation` — 12 bytes

`kind:RelationshipKind@0; target:Handle@4`

### `Relations` — 8 bytes

`items:Array<Relation>@0`

### `NumericRange` — 32 bytes

`min:F64@0; max:F64@8; current:F64@16; step:F64@24`

### `CountState` — 12 bytes

`known:Bool@0; count:U64@4`

### `PopupAnchor` — 64 bytes

`placement:U64@0; side:PopupSide@8; align:Align@12; offset:Vec2@16; padding:F64@32; flip:Bool@40; shift:Bool@44; maxSize:Vec2@48`

### `Link` — 20 bytes

`routeId:U32@0; mode:NavigationMode@4; target:String@8; policy:U32@16`

### `Property` — 12 bytes

`id:U32@0; payload:Blob@4`

### `NodeSpec` — 28 bytes

`node:Handle@0; kind:NodeKind@8; properties:Array<Property>@12; children:Array<Handle>@20`

### `CreateOp` — 28 bytes

`spec:NodeSpec@0`

### `TextOp` — 16 bytes

`node:Handle@0; text:String@8`

### `PropsOp` — 16 bytes

`node:Handle@0; properties:Array<Property>@8`

### `InsertOp` — 24 bytes

`parent:Handle@0; child:Handle@8; before:Handle@16`

### `NodeOp` — 8 bytes

`node:Handle@0`

### `DestroyOp` — 12 bytes

`node:Handle@0; recursive:Bool@8`

### `ChildrenOp` — 16 bytes

`parent:Handle@0; children:Array<Handle>@8`

### `BindOp` — 36 bytes

`node:Handle@0; eventClass:U32@8; bindingGeneration:U64@12; action:U64@20; policy:Handle@28`

### `UnbindOp` — 20 bytes

`node:Handle@0; eventClass:U32@8; bindingGeneration:U64@12`

### `DomOp` — 12 bytes

Tagged-span variants: 0=Create(CreateOp), 1=SetText(TextOp), 2=SetProperties(PropsOp), 3=Insert(InsertOp), 4=Detach(NodeOp), 5=Destroy(DestroyOp), 6=SetChildren(ChildrenOp), 7=Bind(BindOp), 8=Unbind(UnbindOp).

### `PatchResult` — 32 bytes

`stamp:UIStamp@0; eventFence:U64@24`

### `StageResult` — 20 bytes

`candidate:Handle@0; nodes:U32@8; bytes:U64@12`

### `EditHandoff` — 28 bytes

`lease:Handle@0; oldNode:Handle@8; newNode:Handle@16; preserveNative:Bool@24`

### `MeasureResult` — 120 bytes

`node:Handle@0; stamp:UIStamp@8; layoutEpoch:U64@32; scrollEpoch:U64@40; fontEpoch:U64@48; content:Rect@56; scroll:Vec2@88; backing:SizeU@104; dpr:F64@112`

### `ConditionalResult` — 12 bytes

`applied:Bool@0; currentEpoch:U64@4`

### `FieldState` — 140 bytes

`node:Handle@0; binding:U64@8; bindingGeneration:U64@16; field:FieldKey@24; kind:ControlKind@80; acceptedRevision:U64@84; value:ControlValue@92; parserId:U32@104; formatId:U32@108; validatorId:U32@112; policy:EditPolicy@116; form:U64@120; formGeneration:U64@128; writable:Bool@136`

### `EditSnapshot` — 144 bytes

`binding:U64@0; bindingGeneration:U64@8; field:FieldKey@16; session:Handle@72; lease:Handle@80; baseRevision:U64@88; sequence:U64@96; draft:DraftText@104; selectionStart:U32@112; selectionEnd:U32@116; direction:SelectionDirection@120; composition:Composition@124; dirty:Bool@128; control:ControlValue@132`

### `CorrectionResult` — 12 bytes

`outcome:CorrectionOutcome@0; currentSequence:U64@4`

### `BindingSequence` — 16 bytes

`binding:U64@0; sequence:U64@8`

### `FieldError` — 20 bytes

`binding:U64@0; validity:Validity@8; message:String@12`

### `FormResult` — 44 bytes

`submission:U64@0; outcome:FormOutcome@8; sequences:Array<BindingSequence>@12; errors:Array<FieldError>@20; operation:Id128@28`

### `DraftOffer` — 212 bytes

`vaultId:U64@0; field:FieldKey@8; snapshot:EditSnapshot@64; liveNative:Bool@208`

### `DraftOffers` — 8 bytes

`drafts:Array<DraftOffer>@0`

### `AdoptResult` — 164 bytes

`session:Handle@0; lease:Handle@8; restoredNative:Bool@16; snapshot:EditSnapshot@20`

### `IntentVector` — 40 bytes

`selection:U64@0; edit:U64@8; navigation:U64@16; focus:U64@24; authority:U64@32`

### `FocusModel` — 52 bytes

`members:Array<Handle>@0; current:Handle@8; orientation:Orientation@16; wrap:FocusWrap@20; initial:Handle@24; restore:Handle@32; modal:Bool@40; parent:Handle@44`

### `NativeRule` — 12 bytes

`binding:U64@0; kind:ControlKind@8`

### `RovingRule` — 56 bytes

`focus:FocusModel@0; typeahead:Bool@52`

### `MenuRule` — 72 bytes

`focus:FocusModel@0; parentMenu:Handle@52; closeOutside:Bool@60; hoverDelay:F64@64`

### `DialogRule` — 60 bytes

`focus:FocusModel@0; outsideDismiss:Bool@52; escapeDismiss:Bool@56`

### `CaptureRule` — 20 bytes

`target:Handle@0; buttons:U32@8; touchAction:TouchAction@12; prevent:DefaultAction@16`

### `SplitterRule` — 40 bytes

`axis:Orientation@0; min:F64@4; max:F64@12; ratio:F64@20; variableId:U32@28; layoutEpoch:U64@32`

### `VirtualRule` — 44 bytes

`collection:U64@0; orderRevision:U64@8; totalExtent:F64@16; windowStart:F64@24; windowEnd:F64@32; placeholder:Bool@40`

### `ShortcutRule` — 40 bytes

`match:KeyMatch@0; key:String@4; modifiers:Modifiers@12; action:U64@16; allowRepeat:Bool@24; excludeText:Bool@28; excludeComposition:Bool@32; prevent:DefaultAction@36`

### `InteractionRule` — 12 bytes

Tagged-span variants: 0=Native(NativeRule), 1=Roving(RovingRule), 2=Menu(MenuRule), 3=Dialog(DialogRule), 4=Capture(CaptureRule), 5=Splitter(SplitterRule), 6=Virtual(VirtualRule), 7=Shortcut(ShortcutRule).

### `InteractionModel` — 36 bytes

`node:Handle@0; bindingGeneration:U64@8; modelGeneration:U64@16; rules:Array<InteractionRule>@24; invalidates:IntentMask@32`

### `PreparedGesture` — 100 bytes

`node:Handle@0; bindingGeneration:U64@8; action:U64@16; requestId:U64@24; operation:GestureOperation@32; arguments:GestureArguments@36; expected:IntentVector@48; invalidates:IntentMask@88; expiry:F64@92`

### `ViewToken` — 64 bytes

`frame:U64@0; bundle:Id128@8; camera:U64@24; surface:Handle@32; surfaceEpoch:U64@40; layoutEpoch:U64@48; inputWatermark:U64@56`

### `EventHead` — 112 bytes

`producer:U32@0; sequence:U64@4; time:F64@12; stamp:UIStamp@20; node:Handle@44; bindingGeneration:U64@52; layoutEpoch:U64@60; source:EventSource@68; intent:IntentVector@72`

### `PointerData` — 136 bytes

`phase:PointerPhase@0; id:I32@4; kind:PointerKind@8; buttons:U32@12; changedButton:I32@16; modifiers:Modifiers@20; position:Vec2@24; pressure:F64@40; tilt:Vec2@48; toolGeneration:U64@64; view:ViewToken@72`

### `PointerSample` — 32 bytes

`time:F64@0; position:Vec2@8; pressure:F64@24`

### `FrameTick` — 48 bytes

`surface:Handle@0; surfaceEpoch:U64@8; tick:U64@16; time:F64@24; deadline:F64@32; sizeVersion:U64@40`

### `SurfaceResult` — 20 bytes

`surface:Handle@0; epoch:U64@8; profile:Profile@16`

### `FrameStamp` — 64 bytes

`surface:Handle@0; surfaceEpoch:U64@8; deviceEpoch:U64@16; frame:U64@24; bundle:Id128@32; camera:U64@48; inputWatermark:U64@56`

### `FrameResult` — 20 bytes

`frame:U64@0; outcome:FrameOutcome@8; queueSerial:U64@12`

### `VersionRef` — 56 bytes

`version:Handle@0; offset:U64@8; bytes:U64@16; layout:Hash256@24`

### `BufferDesc` — 60 bytes

`bytes:U64@0; usage:GpuBufferUsage@8; layout:Hash256@12; ranges:Array<ByteRange>@44; label:String@52`

### `TextureDesc` — 40 bytes

`size:SizeU@0; layers:U32@8; mips:U32@12; samples:U32@16; dimension:TextureDimension@20; format:GpuFormat@24; usage:GpuTextureUsage@28; label:String@32`

### `TextureViewDesc` — 20 bytes

`format:GpuFormat@0; baseMip:U32@4; mipCount:U32@8; baseLayer:U32@12; layerCount:U32@16`

### `SamplerDesc` — 32 bytes

`addressU:AddressMode@0; addressV:AddressMode@4; mag:Filter@8; min:Filter@12; mip:Filter@16; lodMin:F32@20; lodMax:F32@24; compare:Compare@28`

### `BindLayoutEntry` — 36 bytes

`binding:U32@0; visibility:ShaderVisibility@4; kind:BindKind@8; dynamicOffset:Bool@12; minBufferBytes:U64@16; textureDimension:TextureDimension@24; sampleType:SampleType@28; storageFormat:GpuFormat@32`

### `VertexAttribute` — 12 bytes

`location:U32@0; format:VertexFormat@4; offset:U32@8`

### `VertexLayout` — 16 bytes

`stride:U32@0; step:StepMode@4; attributes:Array<VertexAttribute>@8`

### `Blend` — 28 bytes

`enabled:Bool@0; colorOp:BlendOperation@4; colorSource:BlendFactor@8; colorDest:BlendFactor@12; alphaOp:BlendOperation@16; alphaSource:BlendFactor@20; alphaDest:BlendFactor@24`

### `ColorTarget` — 36 bytes

`format:GpuFormat@0; writeMask:ColorWriteMask@4; blend:Blend@8`

### `PipelineDesc` — 140 bytes

`kind:PipelineKind@0; shader:Handle@4; vertexEntry:String@12; fragmentEntry:String@20; computeEntry:String@28; layouts:Array<Handle>@36; vertices:Array<VertexLayout>@44; primitive:PrimitiveTopology@52; front:FrontFace@56; cull:Cull@60; depthFormat:GpuFormat@64; depthWrite:Bool@68; depthCompare:Compare@72; depthBias:I32@76; slopeBias:F32@80; clampBias:F32@84; samples:U32@88; colors:Array<ColorTarget>@92; interface:Hash256@100; label:String@132`

### `ResidentRef` — 8 bytes

`handle:Handle@0`

### `TransientRef` — 4 bytes

`id:U32@0`

### `TargetRef` — 12 bytes

Tagged-span variants: 0=None(Void), 1=Surface(Void), 2=Resident(ResidentRef), 3=Transient(TransientRef).

### `BindEntry` — 36 bytes

`binding:U32@0; kind:BindKind@4; resource:TargetRef@8; offset:U64@20; bytes:U64@28`

### `InlineGroup` — 16 bytes

`layout:Handle@0; entries:Array<BindEntry>@8`

### `GroupRef` — 12 bytes

Tagged-span variants: 0=Resident(ResidentRef), 1=Inline(InlineGroup).

### `BufferSlice` — 24 bytes

`version:Handle@0; offset:U64@8; bytes:U64@16`

### `Draw` — 84 bytes

`pipeline:Handle@0; groups:Array<GroupRef>@8; vertices:Array<BufferSlice>@16; index:BufferSlice@24; indexed:Bool@48; indexFormat:IndexFormat@52; count:U32@56; first:U32@60; baseVertex:I32@64; instances:U32@68; firstInstance:U32@72; dynamicOffsets:Array<U32>@76`

### `Attachment` — 108 bytes

`target:TargetRef@0; resolve:TargetRef@12; load:LoadOp@24; store:StoreOp@28; clear:Vec4f@92`

### `DepthAttachment` — 92 bytes

`enabled:Bool@0; target:TargetRef@4; load:LoadOp@16; store:StoreOp@20; clear:F32@84; readOnly:Bool@88`

### `RenderPass` — 172 bytes

`id:U32@0; kind:PassKind@4; reads:Array<TargetRef>@8; colors:Array<Attachment>@16; depth:DepthAttachment@24; viewport:Rect@116; scissor:RectU@148; draws:Array<Draw>@164`

### `ComputePass` — 48 bytes

`id:U32@0; pipeline:Handle@4; groups:Array<GroupRef>@12; readVersions:Array<Handle>@20; transientWrites:Array<U32>@28; x:U32@36; y:U32@40; z:U32@44`

### `GraphPass` — 12 bytes

Tagged-span variants: 0=Render(RenderPass), 1=Compute(ComputePass).

### `TransientDesc` — 44 bytes

`id:U32@0; descriptor:TextureDesc@4`

### `FramePlan` — 104 bytes

`stamp:FrameStamp@0; versions:Array<Handle>@64; transients:Array<TransientDesc>@72; passes:Array<GraphPass>@80; width:U32@88; height:U32@92; originVersion:U64@96`

### `GpuVersionResult` — 24 bytes

`version:Handle@0; deviceEpoch:U64@8; bytes:U64@16`

### `DeviceResult` — 36 bytes

`device:Handle@0; epoch:U64@8; profile:Profile@16; features:Array<Feature>@20; limits:Array<Limit>@28`

### `PickRequest` — 152 bytes

`view:ViewToken@0; projection:ProjectionStamp@64; purpose:PickPurpose@112; policy:PickPolicy@116; css:Vec2@120; backing:SizeU@136; deadline:F64@144`

### `SemanticHit` — 108 bytes

`entity:Id128@0; instancePath:Array<Id128>@16; topology:Id128@24; geometryRevision:U64@40; mappingRevision:U64@48; point:Vec3@56; normal:Vec3@80; authority:AuthorityClass@104`

### `PickResult` — 176 bytes

`view:ViewToken@0; outcome:PickOutcome@64; hit:SemanticHit@68`

### `ComputeJob` — 52 bytes

`pipeline:Handle@0; inputs:Array<VersionRef>@8; outputs:Array<BufferDesc>@16; groups:Array<InlineGroup>@24; x:U32@32; y:U32@36; z:U32@40; deadline:F64@44`

### `ReadbackResult` — 48 bytes

`stream:Handle@0; schema:Hash256@8; bytes:U64@40`

### `TextMetrics` — 56 bytes

`advance:F64@0; ascent:F64@8; descent:F64@16; ink:Rect@24`

### `ImageResult` — 24 bytes

`image:Handle@0; size:SizeU@8; color:ColorSpace@16; alpha:AlphaMode@20`

### `RasterResult` — 88 bytes

`image:Handle@0; size:SizeU@8; metrics:TextMetrics@16; fontEpoch:U64@72; color:ColorSpace@80; alpha:AlphaMode@84`

### `FileInfo` — 40 bytes

`file:Handle@0; name:String@8; mime:String@16; bytes:U64@24; modified:F64@32`

### `FileResult` — 8 bytes

`files:Array<FileInfo>@0`

### `ByteSource` — 12 bytes

`kind:ByteSourceKind@0; handle:Handle@4`

### `ReadResult` — 20 bytes

`offset:U64@0; data:Blob@8; eof:Bool@16`

### `WriteResult` — 8 bytes

`nextOffset:U64@0`

### `FileCloseResult` — 4 bytes

`completion:FileCompletion@0`

### `NamedString` — 12 bytes

`key:U32@0; value:String@4`

### `HttpRequest` — 84 bytes

`service:U32@0; route:U32@4; method:HttpMethod@8; parameters:AppData@12; headers:Array<NamedString>@52; body:Blob@60; uploadStream:Handle@68; maxResponse:U64@76`

### `HttpResult` — 20 bytes

`status:U32@0; headers:Array<NamedString>@4; stream:Handle@12`

### `SocketResult` — 28 bytes

`socket:Handle@0; receiveStream:Handle@8; protocol:U32@16; maxMessage:U64@20`

### `StreamChunk` — 48 bytes

`stream:Handle@0; message:U64@8; chunk:U32@16; start:Bool@20; end:Bool@24; totalKnown:Bool@28; total:U64@32; data:Blob@40`

### `StoreRow` — 60 bytes

`key:Blob@0; value:AppData@8; version:U64@48; found:Bool@56`

### `StoreResult` — 16 bytes

`rows:Array<StoreRow>@0; transactionSequence:U64@8`

### `ScanToken` — 24 bytes

`generation:U64@0; lastKey:Blob@8; highWater:U64@16`

### `ScanResult` — 40 bytes

`rows:Array<StoreRow>@0; next:ScanToken@8; end:Bool@32; consistency:ScanConsistency@36`

### `BlobInfo` — 52 bytes

`hash:Hash256@0; bytes:U64@32; version:U64@40; state:BlobState@48`

### `BlobListResult` — 36 bytes

`items:Array<BlobInfo>@0; next:ScanToken@8; end:Bool@32`

### `QuotaResult` — 20 bytes

`usage:U64@0; quota:U64@8; known:Bool@16`

### `ClipboardValue` — 44 bytes

`format:ClipboardFormat@0; schema:Hash256@4; bytes:Blob@36`

### `Environment` — 76 bytes

`revision:U64@0; locales:Array<String>@8; direction:Direction@16; dark:Bool@20; reducedMotion:Bool@24; forcedColors:Bool@28; viewport:Rect@32; dpr:F64@64; onlineHint:Bool@72`

### `FieldSequenceSet` — 8 bytes

`items:Array<BindingSequence>@0`

### `GestureFile` — 12 bytes

`mode:FilePickMode@0; filterPolicy:U32@4; multiple:Bool@8`

### `GestureClipboard` — 48 bytes

`value:ClipboardValue@0; readPolicy:U32@44`

### `GestureFullscreen` — 12 bytes

`surface:Handle@0; enter:Bool@8`

### `GestureLink` — 12 bytes

`policy:U32@0; target:String@4`

### `PrintRow` — 8 bytes

`cells:Array<String>@0`

### `PrintTable` — 16 bytes

`headers:Array<String>@0; rows:Array<PrintRow>@8`

### `PrintImage` — 32 bytes

`image:Handle@0; width:F64@8; height:F64@16; caption:String@24`

### `PrintBlock` — 12 bytes

Tagged-span variants: 0=Title(TextValue), 1=Paragraph(TextValue), 2=Table(PrintTable), 3=Image(PrintImage).

### `PrintDocument` — 56 bytes

`blocks:Array<PrintBlock>@0; language:String@8; direction:Direction@16; paper:Vec2@20; margins:Vec4f@36; maxPages:U32@52`

### `GesturePrint` — 8 bytes

`layout:Handle@0`

### `GestureFocus` — 12 bytes

`node:Handle@0; behavior:FocusBehavior@8`

### `GestureArguments` — 12 bytes

Tagged-span variants: 0=File(GestureFile), 1=Clipboard(GestureClipboard), 2=Fullscreen(GestureFullscreen), 3=Link(GestureLink), 4=Print(GesturePrint), 5=Focus(GestureFocus).

### `GestureOutcome` — 12 bytes

`request:U64@0; outcome:GestureResult@8`

### `LogField` — 44 bytes

`key:U32@0; value:AppData@4`

### `GeometryAsset` — 116 bytes

`kind:AssetKind@0; hash:Hash256@4; compressedHash:Hash256@36; bytes:U64@68; compressedBytes:U64@76; schema:Hash256@84`

### `MeshLod` — 140 bytes

`asset:Hash256@0; topologyMap:Hash256@32; bvh:Hash256@64; edgeAsset:Hash256@96; error:F64@128; lod:U32@136`

### `EntitySummary` — 100 bytes

`entity:Id128@0; revision:U64@16; type:U32@24; label:String@28; boundsMin:Vec3@36; boundsMax:Vec3@60; childCount:CountState@84; residency:Residency@96`

### `SceneBundle` — 228 bytes

`id:Id128@0; projection:ProjectionStamp@16; geometryRevision:U64@64; evaluation:Id128@72; dependencies:Array<EntityVersion>@88; assets:Array<GeometryAsset>@96; lods:Array<MeshLod>@104; semanticSchema:Hash256@112; instances:AppData@144; units:U32@184; frame:AppData@188`

### `Operation` — 220 bytes

`id:Id128@0; actor:Id128@16; document:DocumentStamp@32; base:U64@56; command:Id128@64; version:U32@80; arguments:AppData@84; dependencies:Array<Id128>@124; preconditions:Array<EntityVersion>@132; journalSequence:U64@140; approval:AppData@148; bodyHash:Hash256@188`

### `Receipt` — 192 bytes

`operation:Id128@0; kind:ReceiptKind@16; sequence:U64@20; digest:Hash256@28; result:AppData@60; error:Error@100; ticket:Id128@168; horizon:F64@184`

### `OrderedEvent` — 188 bytes

`document:DocumentStamp@0; sequence:U64@24; operation:Id128@32; canonical:AppData@48; deltas:AppData@88; entities:Array<EntityVersion>@128; evaluation:EvaluationState@136; ticket:Id128@140; digest:Hash256@156`

### `SyncHello` — 72 bytes

`document:DocumentStamp@0; sequence:U64@24; journalSchema:Hash256@32; unresolved:Array<Id128>@64`

### `SyncWelcome` — 80 bytes

`document:DocumentStamp@0; head:U64@24; horizon:F64@32; checkpoint:Hash256@40; schemas:Array<Hash256>@72`

### `KeyResult` — 16 bytes

`key:Handle@0; version:U64@8`

### `CryptoResult` — 24 bytes

`bytes:Blob@0; keyVersion:U64@8; nonce:Blob@16`

### `MigrationStatus` — 112 bytes

`phase:MigrationPhase@0; source:U64@4; target:U64@12; cursor:ScanToken@20; error:Error@44`

### `M_HELLO` — 68 bytes

`app:Id128@0; release:Hash256@16; abiMajor:U32@48; abiMinor:U32@52; schemas:Array<FeatureSchema>@56; profile:Profile@64`

### `M_WELCOME` — 76 bytes

`runtimeEpoch:U64@0; root:Handle@8; rootScope:Handle@16; clock:Id128@24; origin:F64@40; profile:Profile@48; schemas:Array<FeatureSchema>@52; limits:Array<Limit>@60; grants:Array<Grant>@68`

### `M_START` — 180 bytes

`app:Id128@0; release:Hash256@16; root:Handle@48; rootScope:Handle@56; environment:Environment@64; config:AppData@140`

### `M_RESULT` — 92 bytes

`requestOpcode:U32@0; outcome:Outcome@4; result:Blob@8; resultBuffer:Handle@16; error:Error@24`

### `M_CREDIT` — 28 bytes

`lane:Channel@0; generation:U64@4; releasedBytes:U64@12; releasedMessages:U64@20`

### `M_STOP` — 4 bytes

`reason:RuntimeReason@0`

### `M_SUSPEND` — 12 bytes

`reason:RuntimeReason@0; time:F64@4`

### `M_RESUME` — 84 bytes

`time:F64@0; environment:Environment@8`

### `M_CHANNEL_FAULT` — 80 bytes

`lane:Channel@0; sequence:U64@4; error:Error@12`

### `M_CANCEL` — 12 bytes

`request:U64@0; reason:RuntimeReason@8`

### `M_SCOPE_OPEN` — 12 bytes

`kind:ScopeKind@0; localId:U64@4`

### `M_SCOPE_CLOSE` — 8 bytes

`scopeToClose:Handle@0`

### `M_AUTH_TRANSITION` — 52 bytes

`old:AuthorityStamp@0; new:AuthorityStamp@24; policy:OfflinePolicy@48`

### `M_FRAME_DEMAND` — 28 bytes

`surface:Handle@0; surfaceEpoch:U64@8; generation:U64@16; continuous:Bool@24`

### `M_FRAME_TICK` — 48 bytes

`tick:FrameTick@0`

### `M_SURFACE_PACING` — 20 bytes

`surface:Handle@0; surfaceEpoch:U64@8; state:SurfacePacing@16`

### `M_TICK_CONSUMED` — 20 bytes

`surface:Handle@0; tick:U64@8; outcome:TickOutcome@16`

### `M_SURFACE_AVAILABLE` — 28 bytes

`node:Handle@0; surface:Handle@8; epoch:U64@16; profile:Profile@24`

### `M_SURFACE_REPLACED` — 32 bytes

`node:Handle@0; old:Handle@8; new:Handle@16; epoch:U64@24`

### `M_DEVICE_LOST` — 76 bytes

`deviceEpoch:U64@0; error:Error@8`

### `M_FRAME_PROGRESS` — 20 bytes

`frame:U64@0; state:FrameProgressKind@8; queueSerial:U64@12`

### `M_QUEUE_COMPLETED` — 16 bytes

`deviceEpoch:U64@0; serial:U64@8`

### `M_INPUT_DRAINED` — 20 bytes

`producer:U32@0; sequence:U64@4; treeEpoch:U64@12`

### `M_DRAFT_LIST` — 0 bytes

Empty record.

### `M_DRAFT_ADOPT` — 148 bytes

`vaultId:U64@0; field:FieldState@8`

### `M_DRAFT_REJECT` — 80 bytes

`vaultId:U64@0; reason:Error@8; discard:Bool@76`

### `M_DOM_PATCH` — 56 bytes

`stamp:UIStamp@0; next:U64@24; region:Handle@32; operations:Array<DomOp>@40; interactionFence:U64@48`

### `M_SNAPSHOT_BEGIN` — 52 bytes

`stamp:UIStamp@0; region:Handle@24; nodes:U32@32; bytes:U64@36; deadline:F64@44`

### `M_SNAPSHOT_CHUNK` — 24 bytes

`candidate:Handle@0; sequence:U64@8; nodes:Array<NodeSpec>@16`

### `M_SNAPSHOT_SEAL` — 48 bytes

`candidate:Handle@0; root:Handle@8; hash:Hash256@16`

### `M_SNAPSHOT_ACTIVATE` — 40 bytes

`candidate:Handle@0; expected:UIStamp@8; edits:Array<EditHandoff>@32`

### `M_SNAPSHOT_ABORT` — 8 bytes

`candidate:Handle@0`

### `M_RELOCATE` — 36 bytes

`component:U64@0; oldPlacement:U64@8; newPlacement:U64@16; expectedRevision:U64@24; preserveEdit:Bool@32`

### `M_MEASURE` — 20 bytes

`node:Handle@0; kind:MeasurementKind@8; token:U64@12`

### `M_FOCUS` — 20 bytes

`node:Handle@0; expectedEpoch:U64@8; behavior:FocusBehavior@16`

### `M_SCROLL` — 36 bytes

`node:Handle@0; expectedEpoch:U64@8; offset:Vec2@16; behavior:ScrollBehavior@32`

### `M_INTERACTION_INSTALL` — 44 bytes

`policy:Handle@0; model:InteractionModel@8`

### `M_INTERACTION_REVOKE` — 8 bytes

`policy:Handle@0`

### `M_GESTURE_PREPARE` — 108 bytes

`policy:Handle@0; gesture:PreparedGesture@8`

### `M_GESTURE_REVOKE` — 8 bytes

`policy:Handle@0`

### `M_FIELD_INSTALL` — 140 bytes

`state:FieldState@0`

### `M_FIELD_STATE` — 140 bytes

`state:FieldState@0`

### `M_EDIT_ACK` — 28 bytes

`session:Handle@0; sequence:U64@8; validity:Validity@16; message:String@20`

### `M_EDIT_CORRECT` — 160 bytes

`session:Handle@0; expectedSequence:U64@8; replacement:EditSnapshot@16`

### `M_EDIT_END` — 28 bytes

`session:Handle@0; expectedSequence:U64@8; outcome:EditEndOutcome@16; acceptedRevision:U64@20`

### `M_CONTROL_CORRECT` — 36 bytes

`binding:U64@0; generation:U64@8; expectedSequence:U64@16; value:ControlValue@24`

### `M_FORM_INSTALL` — 48 bytes

`form:U64@0; generation:U64@8; membershipRevision:U64@16; members:Array<U64>@24; command:Id128@32`

### `M_FORM_CAPTURE` — 24 bytes

`form:U64@0; generation:U64@8; membershipRevision:U64@16`

### `FormCapture` — 40 bytes

`submission:U64@0; form:U64@8; generation:U64@16; membershipRevision:U64@24; fields:Array<EditSnapshot>@32`

### `M_FORM_APPLY_RESULT` — 44 bytes

`result:FormResult@0`

### `M_FORM_RESET` — 24 bytes

`form:U64@0; generation:U64@8; expected:Array<BindingSequence>@16`

### `M_INTERACTION_FENCE` — 20 bytes

`fence:U64@0; regions:Array<Handle>@8; release:Bool@16`

### `M_ACTIVATE` — 124 bytes

`head:EventHead@0; action:U64@112; kind:ActivationKind@120`

### `M_POINTER` — 256 bytes

`head:EventHead@0; pointer:PointerData@112; samples:Array<PointerSample>@248`

### `M_WHEEL` — 144 bytes

`head:EventHead@0; delta:Vec3@112; unit:WheelUnit@136; modifiers:Modifiers@140`

### `M_KEY` — 144 bytes

`head:EventHead@0; phase:KeyPhase@112; key:String@116; code:String@124; modifiers:Modifiers@132; repeat:Bool@136; composing:Bool@140`

### `M_CAPTURE_CHANGED` — 124 bytes

`head:EventHead@0; pointer:I32@112; captured:Bool@116; reason:RuntimeReason@120`

### `M_FOCUS_CHANGED` — 124 bytes

`head:EventHead@0; focused:Bool@112; focusEpoch:U64@116`

### `M_EDIT_SNAPSHOT` — 256 bytes

`head:EventHead@0; snapshot:EditSnapshot@112`

### `M_EDIT_COMMIT` — 260 bytes

`head:EventHead@0; snapshot:EditSnapshot@112; reason:U32@256`

### `M_EDIT_CANCEL` — 260 bytes

`head:EventHead@0; snapshot:EditSnapshot@112; reason:EditEndOutcome@256`

### `M_CONTROL_OBSERVED` — 256 bytes

`head:EventHead@0; snapshot:EditSnapshot@112`

### `M_FORM_SNAPSHOT` — 152 bytes

`head:EventHead@0; capture:FormCapture@112`

### `M_SCROLL_CHANGED` — 136 bytes

`head:EventHead@0; offset:Vec2@112; scrollEpoch:U64@128`

### `M_VIEWPORT_SIZE` — 176 bytes

`head:EventHead@0; surface:Handle@112; surfaceEpoch:U64@120; content:Rect@128; backing:SizeU@160; sizeVersion:U64@168`

### `M_INPUT_RESET` — 124 bytes

`head:EventHead@0; reason:RuntimeReason@112; pointers:Array<I32>@116`

### `M_DROP` — 148 bytes

`head:EventHead@0; files:Array<FileInfo>@112; text:String@120; position:Vec2@128; policy:U32@144`

### `M_ROUTE_CHANGED` — 136 bytes

`head:EventHead@0; route:U32@112; canonical:String@116; historySequence:U64@124; source:RouteSource@132`

### `M_DISMISS` — 124 bytes

`head:EventHead@0; policy:Handle@112; reason:DismissReason@120`

### `M_GESTURE_STARTED` — 128 bytes

`head:EventHead@0; request:U64@112; action:U64@120`

### `M_SPLITTER_OBSERVED` — 144 bytes

`head:EventHead@0; placement:U64@112; gesture:U64@120; ratio:F64@128; final:Bool@136; cancelled:Bool@140`

### `M_DEVICE_INITIALIZE` — 20 bytes

`surface:Handle@0; profile:Profile@8; limits:Array<Limit>@12`

### `M_BUFFER_BEGIN` — 68 bytes

`deviceEpoch:U64@0; descriptor:BufferDesc@8`

### `M_TEXTURE_BEGIN` — 48 bytes

`deviceEpoch:U64@0; descriptor:TextureDesc@8`

### `M_BUFFER_WRITE` — 24 bytes

`writer:Handle@0; offset:U64@8; data:Blob@16`

### `M_TEXTURE_WRITE` — 48 bytes

`writer:Handle@0; mip:U32@8; x:U32@12; y:U32@16; layer:U32@20; size:SizeU@24; rowsPerImage:U32@32; bytesPerRow:U32@36; data:Blob@40`

### `M_VERSION_SEAL` — 76 bytes

`writer:Handle@0; expectedHash:Hash256@8; layout:Hash256@40; verifyHash:Bool@72`

### `M_WRITER_ABORT` — 8 bytes

`writer:Handle@0`

### `M_VERSION_COPY_BEGIN` — 116 bytes

`source:VersionRef@0; descriptor:BufferDesc@56`

### `M_TEXTURE_VIEW` — 28 bytes

`version:Handle@0; descriptor:TextureViewDesc@8`

### `M_SAMPLER_CREATE` — 32 bytes

`descriptor:SamplerDesc@0`

### `M_SHADER_CREATE` — 72 bytes

`asset:Hash256@0; source:String@32; interface:Hash256@40`

### `M_LAYOUT_CREATE` — 40 bytes

`entries:Array<BindLayoutEntry>@0; interface:Hash256@8`

### `M_PIPELINE_CREATE` — 140 bytes

`descriptor:PipelineDesc@0`

### `M_GROUP_CREATE` — 16 bytes

`group:InlineGroup@0`

### `M_RESOURCE_RELEASE` — 12 bytes

`resource:Handle@0; capability:Capability@8`

### `M_FRAME_SUBMIT` — 104 bytes

`plan:FramePlan@0`

### `M_PICK_SUBMIT` — 152 bytes

`pick:PickRequest@0`

### `M_GPU_COMPUTE` — 52 bytes

`job:ComputeJob@0`

### `M_GPU_READBACK` — 56 bytes

`source:VersionRef@0`

### `M_SCREENSHOT` — 112 bytes

`plan:FramePlan@0; size:SizeU@104`

### `M_CLOCK_NOW` — 0 bytes

Empty record.

### `M_TIMER` — 8 bytes

`due:F64@0`

### `M_RANDOM` — 4 bytes

`bytes:U32@0`

### `M_ENVIRONMENT_GET` — 0 bytes

Empty record.

### `M_ENVIRONMENT_SUBSCRIBE` — 0 bytes

Empty record.

### `M_ENVIRONMENT_CHANGED` — 76 bytes

`environment:Environment@0`

### `M_HTTP_REQUEST` — 84 bytes

`request:HttpRequest@0`

### `M_SOCKET_OPEN` — 8 bytes

`service:U32@0; protocol:U32@4`

### `M_SOCKET_SEND` — 24 bytes

`socket:Handle@0; message:U64@8; bytes:Blob@16`

### `M_SOCKET_CLOSE` — 16 bytes

`socket:Handle@0; reason:String@8`

### `M_STREAM_CREDIT` — 24 bytes

`stream:Handle@0; releasedBytes:U64@8; releasedMessages:U64@16`

### `M_STREAM_CANCEL` — 8 bytes

`stream:Handle@0`

### `M_STREAM_MESSAGE` — 48 bytes

`chunk:StreamChunk@0`

### `M_STREAM_CLOSED` — 80 bytes

`stream:Handle@0; end:StreamEnd@8; error:Error@12`

### `M_STORE_TX` — 20 bytes

`mode:StoreMode@0; operations:Array<StoreOp>@4; fence:U64@12`

### `M_STORE_SCAN` — 48 bytes

`store:U32@0; index:U32@4; prefix:Blob@8; token:ScanToken@16; limit:U32@40; consistency:ScanConsistency@44`

### `M_STORE_QUOTA` — 0 bytes

Empty record.

### `M_STORE_PERSIST` — 0 bytes

Empty record.

### `M_BLOB_BEGIN` — 40 bytes

`hash:Hash256@0; bytes:U64@32`

### `M_BLOB_WRITE` — 32 bytes

`writer:Handle@0; sequence:U64@8; offset:U64@16; bytes:Blob@24`

### `M_BLOB_FINISH` — 40 bytes

`writer:Handle@0; hash:Hash256@8`

### `M_BLOB_ABORT` — 8 bytes

`writer:Handle@0`

### `M_BLOB_OPEN` — 32 bytes

`hash:Hash256@0`

### `M_BLOB_STAT` — 32 bytes

`hash:Hash256@0`

### `M_BLOB_LIST` — 28 bytes

`token:ScanToken@0; limit:U32@24`

### `M_BLOB_RETAIN` — 56 bytes

`hash:Hash256@0; owner:Id128@32; expectedVersion:U64@48`

### `M_BLOB_UNPIN` — 56 bytes

`hash:Hash256@0; owner:Id128@32; expectedVersion:U64@48`

### `M_BLOB_DELETE` — 48 bytes

`hash:Hash256@0; expectedVersion:U64@32; fence:U64@40`

### `M_FILE_PICK` — 12 bytes

`mode:FilePickMode@0; filterPolicy:U32@4; multiple:Bool@8`

### `M_READ_BYTES` — 24 bytes

`source:ByteSource@0; offset:U64@12; maxBytes:U32@20`

### `M_WRITE_BEGIN` — 24 bytes

`destination:Handle@0; name:String@8; mime:String@16`

### `M_WRITE_CHUNK` — 32 bytes

`writer:Handle@0; sequence:U64@8; offset:U64@16; bytes:Blob@24`

### `M_WRITE_CLOSE` — 8 bytes

`writer:Handle@0`

### `M_WRITE_ABORT` — 8 bytes

`writer:Handle@0`

### `M_CLIPBOARD_WRITE` — 44 bytes

`value:ClipboardValue@0`

### `M_CLIPBOARD_READ` — 4 bytes

`policy:U32@0`

### `M_NAVIGATE` — 24 bytes

`route:U32@0; path:String@4; mode:NavigationMode@12; historySequence:U64@16`

### `M_EXTERNAL_LINK` — 12 bytes

`policy:U32@0; target:String@4`

### `M_PRINT_PREPARE` — 56 bytes

`document:PrintDocument@0`

### `M_PRINT` — 8 bytes

`layout:Handle@0`

### `M_FONT_LOAD` — 36 bytes

`hash:Hash256@0; policy:U32@32`

### `M_TEXT_RASTER` — 64 bytes

`font:Handle@0; text:String@8; language:String@16; direction:Direction@24; size:F64@28; scale:F64@36; maxWidth:U32@44; wrap:WrapMode@48; align:Align@52; maxPixels:U64@56`

### `M_IMAGE_DECODE` — 28 bytes

`source:ByteSource@0; codec:ImageCodec@12; orientation:ImageOrientation@16; maxPixels:U64@20`

### `M_PIXEL_READ` — 24 bytes

`image:Handle@0; rect:RectU@8`

### `M_FULLSCREEN` — 12 bytes

`surface:Handle@0; enter:Bool@8`

### `M_KEY_ACQUIRE` — 28 bytes

`document:Id128@0; keyVersion:U64@16; policy:U32@24`

### `M_CRYPTO_SEAL` — 32 bytes

`key:Handle@0; counter:U64@8; aad:Blob@16; plaintext:Blob@24`

### `M_CRYPTO_OPEN` — 32 bytes

`key:Handle@0; nonce:Blob@8; aad:Blob@16; ciphertext:Blob@24`

### `M_TRACE_EXPORT` — 12 bytes

`policy:U32@0; maximum:U64@4`

### `M_CAPABILITY_CHANGED` — 52 bytes

`grant:Grant@0; authority:AuthorityStamp@28`

### `M_TASK_PROGRESS` — 28 bytes

`request:U64@0; completed:U64@8; total:U64@16; known:Bool@24`

### `M_LOG` — 16 bytes

`category:U32@0; level:LogLevel@4; fields:Array<LogField>@8`

## A.3 Property payloads

Property is `(id:u32, payload:Span)`; the following table selects the exact payload type. Generic arbitrary HTML/style/value objects are not admitted. TabIndex is restricted to -1 or 0, progress to [0,1], spans/counts to their feature limits; relationships must resolve inside the authorized managed scope. Native-kind role/control constraints and field leases override generic presentation changes.

| ID | Property | Payload type | Node kinds |
|---:|---|---|---|
| 1 | disabled | Bool | * |
| 2 | hidden | Bool | * |
| 3 | readOnly | Bool | * |
| 4 | required | Bool | * |
| 5 | invalid | Bool | * |
| 6 | busy | Bool | * |
| 7 | checked | BooleanState | Input |
| 8 | selected | Bool | * |
| 9 | multiple | Bool | * |
| 10 | expanded | Bool | * |
| 11 | ariaSelected | Bool | * |
| 12 | placeholder | String | * |
| 13 | title | String | * |
| 14 | accessibleName | String | * |
| 15 | description | String | * |
| 16 | language | String | * |
| 17 | imageAlt | String | * |
| 18 | valueText | String | * |
| 19 | controlKind | ControlKind | Input, Textarea, Select |
| 20 | inputMode | InputMode | Input, Textarea |
| 21 | autocomplete | Autocomplete | Input, Textarea |
| 22 | labelTarget | Handle | Label |
| 23 | role | Role | * |
| 24 | tabIndex | I32 | * |
| 25 | liveMode | LiveMode | * |
| 26 | relationships | Relations | * |
| 27 | positionInSet | U64 | * |
| 28 | hierarchyLevel | U64 | * |
| 29 | rowIndex | U64 | * |
| 30 | columnIndex | U64 | * |
| 31 | setSize | CountState | * |
| 32 | rowCount | CountState | * |
| 33 | columnCount | CountState | * |
| 34 | numericRange | NumericRange | Input, Box |
| 35 | layout | Layout | * |
| 36 | themeTokens | U64 | * |
| 37 | visualState | VisualState | * |
| 38 | direction | Direction | * |
| 39 | cursor | Cursor | * |
| 40 | asset | Handle | Image |
| 41 | link | Link | Link |
| 42 | progress | F64 | Progress |
| 43 | indeterminate | Bool | Progress, Input |
| 44 | listKind | ListKind | List |
| 45 | tableCellKind | TableCellKind | TableCell |
| 46 | columnSpan | U32 | TableCell |
| 47 | rowSpan | U32 | TableCell |
| 48 | viewportId | U64 | Canvas |
| 49 | viewportDescription | String | Canvas |
| 50 | modalOpen | Bool | Dialog |
| 51 | textSelectable | Bool | * |
| 52 | popupAnchor | PopupAnchor | Box, Dialog |

## A.4 Manifest IDs and guards

**limitIds:** MemoryBytes=1, SemanticPacketBytes=2, BulkPacketBytes=3, ControlPacketBytes=4, Requests=5, DOMNodes=6, DOMPatchOperations=7, DOMStagedNodes=8, GPUBytes=9, GPUTextureDimension=10, GPUWorkgroups=11, InputSamples=12, DraftBytes=13, TraceBytes=14, SemanticPages=15, AssetDecodedBytes=16, ShaderBytes=17, PipelineCount=18, DataReferences=19, SnapshotRoots=20.

**eventClassIds:** Activate=1, Pointer=2, Wheel=3, Key=4, Edit=5, Control=6, Focus=7, Capture=8, Scroll=9, Drop=10, Dismiss=11, Submit=12, Layout=13.

**editCommitReasonIds:** Enter=0, Blur=1, Apply=2, Command=3, NativeSubmit=4.

**epoch:** Bound port PageSession/Runtime/lane identity matches; section 2.

**scope:** Registered live scope or admitted system/root operation; section 5.

**authority:** Captured namespace/AuthEpoch/grant matches operation owner; section 22.

**quota:** Reserve bytes/work/request-terminal slot before accepting ownership; sections 4/26.

**schema:** Closed field types, lengths, enum members and payload schemas validate.

**bootstrap:** Only bootstrap state and bound host may use null scope/authority.

**request-match:** Request/correlation, opcode, direction, scope and tombstone match.

**result-schema:** Success payload type is selected by registered original request opcode.

**credit-monotone:** Cumulative released counts never exceed admitted sends or move backwards.

**reserved-control:** Fixed bounded control/terminal capacity reserved at request acceptance.

**clock:** Timestamp is converted to the negotiated root time domain.

**authority-controller:** Only the bound host authorization controller can change authority.

**ui-stamp:** TreeEpoch/root/revision and region base match; retired input follows event fence.

**node-kind:** Property/operation valid for this node kind and managed placement.

**field-target:** FieldKey immutable; target/schema/permissions validated before adoption.

**edit-lease:** Native lease is owned and its relocation/adoption policy permits transition.

**binding-current:** Correct node/binding generation or explicitly retained old action.

**edit-sequence:** Exact expected session and state sequence; composition policy applies.

**device:** DeviceEpoch valid and profile permits the descriptor.

**resource-kind:** Handle kind, owner, version state and source-specific grant match.

**surface:** SurfaceEpoch/layout/size and surface ownership match.

**sealed-versions:** Every input is immutable and host retains the exact version before admission.

**graph:** Acyclic producer/consumer, load/store, attachment/format and private-transient validation.

**scene-lease:** Retained bundle/mapping/dependencies match the pick/geometry request.

**input-view:** ViewToken identifies the retained input-time interaction basis.

**private-outputs:** GPU writes are restricted to unpublished job/frame-owned outputs.

**shader-admission:** Reviewed shader/layout hash and bounded profile, not arbitrary shader text.

**capability:** Configured capability/route/resource-kind policy permits the call.
