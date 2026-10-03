# Package-based incremental builds

Baseline 37b2c1b7. This design was reviewed only at its interface with
[portability.md](portability.md), which builds its §8 and §23 on it; the review
dot habu-review-and-schedule-dbdba551 reviews it in full and schedules its work
(P4 in portability.md §27). Its file paths and facts about existing code are as
of 37b2c1b7 and have not been re-audited against master.

The scope is native self-builds and application builds: one
architecture-independent package contract, with backend-owned relocation
implementations. ARM64 is the first executing target; Linux x86-64 integration
is coordinated with the Intel lane. New file names, types, APIs, CLI switches,
wire profiles, counters and tests below are **proposed**, not claims that those
facilities exist. Existing code is identified in the evidence ledger (§26). No
native performance result is claimed.

## 1. Decision

Implement incremental builds as **reusable, unstripped, checked package contributions installed at their original source-load positions**.

A contribution is not a saved whole-process heap, a final stripped image, or necessarily one file. It contains the package's code, persistent data, private declarations, verified checker facts, explicit imports, symbolic references, and the finite owner-managed state changes its source would publish. A fresh build session reconstructs the same semantic state by importing unchanged contributions and compiling changed ones. Final layout, reachability, stripping, image writing, and signing happen after assembly of the current program.

There are three separately keyed products:

1. **Builder image:** the reusable compiler-driving and writer tooling. Its key covers the actual tool producer and tool dependencies, not every target source.
2. **Package contribution:** a checked compilation result, independent of process addresses and unrelated preceding packages.
3. **Final build generation:** the linked executable, name map, diagnostics/validation receipt, and input identity. An exact repeat may restore this without importing all packages.

Keep the implementation in checked Habu and existing compiler owners. Do not add LLVM, MLIR, a second compiler, a general query framework, or an arbitrary event-replay interpreter. Reuse the AOT artifact machinery, source-view hooks, native emission boundary, and declaration-transaction coordinator; extend their missing contracts rather than bypass them.

### 1.1 What this solves

The recorded native-selfbuild baseline is 124.390 s: 90.386 s in target source loading/compilation, 21.218 s in writer source loading/compilation, and 0.153 s in actual writer emission. The receipt itself was not rerun here. Avoiding repeated compilation is the principal opportunity, not optimizing the final byte write. [H1]

The existing application cache wraps the whole generated application in a closure-keyed object; a changed closure misses it. The new package layer must preserve independent later packages after an early edit. [H2]

### 1.2 Completion, not merely the first demonstration

Completion means the ordinary native and application build commands use the same mechanism; unchanged units do no source evaluation, checking, or code generation on a hit; semantically edited builds equal a cache-off build of the exact same frozen inputs; stale or malformed artifacts never publish; and externally measured edit latency meets an agreed budget on the qualified host.

One NBR hit is an integration milestone, not completion. All expensive target-load families, bootstrap ownership transfers, generated declarations, literals, protected wordlists, runtime initialization, and final publication have work packages below. Unsupported source remains compilable through the ordinary path, but a large uncached suffix prevents claiming the overall performance goal.

## 2. Invariants

| ID | Invariant |
|---|---|
| I1 | Discovery, keying, compilation, diagnostics, and publication refer to the same owned source/input bytes and resolution outcomes. |
| I2 | Import preserves source order, package reopening, `using` ambiguity, public/private visibility, `require` deduplication, and generated declaration order. |
| I3 | Persisted identities never use process addresses, transient dictionary record numbers, or globally assigned wordlist/type IDs as semantic identity. |
| I4 | A reusable result records every input that influenced it, including negative lookups, relevant state, constants, layouts, and compile-time execution. Unsupported observation means non-cacheable, not an omitted edge. |
| I5 | A cached checked interface contains the graph and control facts that were verified, not reconstructed signature prose. |
| I6 | Every placement-dependent encoding has a symbolic fixup with full owner-local bounds, or the contribution is explicitly placement-sensitive and misses when those coordinates change. |
| I7 | Import rejection leaves the live dictionary, checker, data, literal owner, relocation tables, loader registry, and persistent callback ownership unchanged. |
| I8 | No cache hit suppresses required observable source-load side effects. |
| I9 | Cold and cached paths use the same deterministic target allocation/publication and final-link policy. |
| I10 | Artifact integrity, producer compatibility, and authority to trust compiled code are separate checks. |
| I11 | Compiling target source never silently changes which retained compiler implementation is the producer for an active action. |
| I12 | Engine, names, and receipt are selected as one immutable generation by participating consumers. |

## 3. Architecture and owners

```text
ordinary build entry / minimal dispatcher
        |
        v
owned invocation inputs + ordered load plan
        |
        +--> qualified builder image (or source-bound tooling fallback)
        |
        +--> exact final-generation hit? --> validated generation selection
        |
        v
fresh target build session, retained host compiler still authoritative
        |
        +--> contribution U0: validate dependencies -> import or compile
        +--> contribution U1: validate dependencies -> import or compile
        +--> ... in semantic publication order ...
        |
        v
owned complete program / current contribution graph
        |
        v
reachability -> layout/relocation -> writer -> sign -> smoke
        |
        v
immutable generation -> atomic selector publication
```

The graph decides which work is reusable. It does not grant permission to change evaluation order. Start with one target-installation lane; parallelize only isolated producers with explicit environments after correctness and actual package reuse are established.

Existing `NEMIT` supplies sealed bytes and call/address rows, but its call targets are currently absolute and rows expire at `CLEAR`. Existing `NPUB` publishes one pending native definition. These are extension points, not an already finished relocatable package interface. [H3, H4]

Proposed shared components:

| Component | Responsibility | Must not do |
|---|---|---|
| `BUILD-INPUTS` | Own bytes, path queries, environment/configuration and declared generated inputs. | Re-read live disk while assigning an artifact key. |
| `BUILD-PLAN` | Produce ordered load occurrences, contribution boundaries, ownership, and barriers. | Reorder package blocks by name or infer source semantics with regexes. |
| `PKG-DEPS` | Record and validate compiler observations against current providers. | Resolve names differently from dictionary/checker owners. |
| `PKG-CAPTURE` | Own an unstripped contribution and canonicalize identities. | Tree-shake a reusable unit using today's final entry point. |
| `PKG-ARTIFACT` | A package profile of the AOT envelope, staged reader/writer and cache. | Treat a checksum as a certificate of semantic correctness. |
| `PKG-IMPORT` | Coordinate owner preflight, remapping, installation, rollback. | Mutate registry internals through another unchecked implementation. |
| `PKG-LINK` | Plan final symbol/section placement and dispatch target fixups. | Decode ARM64 instructions in architecture-neutral code. |
| `BUILD-GENERATION` | Stage, validate, publish, select and retain output generations. | Overwrite the working output before a replacement is ready. |

## 4. What exactly is a compilation unit?

### 4.1 Logical package, contribution, and occurrence are different

A **package** is the existing Habu namespace with its public/private wordlists and owner identity. A **contribution** is a contiguous, replay-free installation result from one or more ordered source regions. A **load occurrence** is a particular invocation of `include`, `require`, or a declared generated input, in its parent context.

A package may have several contributions. A file may contain several contributions belonging to different packages, plus global declarations. Repeated `include` is not automatically the same occurrence; repeated `require` retains existing load-once behavior.

For example, source order:

```text
A/core        declares A:X
B/core        compiles a use of A:X
A/extension   reopens A and declares A:Y
C/core        uses A:Y
```

must install as `A/core, B/core, A/extension, C/core`. Do not collect both A blocks into one early installation. B must not see a declaration that did not yet exist.

When a file contains `package A ... ;package`, then `package B ... ;package`, the loader can produce two contributions only if the ordinary lexer/parser identifies complete top-level boundaries and the boundary-state contract is satisfied. A quotation containing the text `package` is not a boundary. Unknown dynamic evaluation makes that region source-only.

### 4.2 Boundary contract

A cacheable boundary is outside a definition, quotation, active definer expansion, recovery handler, or declaration transaction. Top-level value and return stacks must have the entry/exit shapes prescribed by the ordinary loader; v1 requires no additional live cross-boundary values. Pending declarations, provisional checker facts, and borrowed scratch slices cannot escape.

Ambient package/search-order state is recorded as logical identities. If a source intentionally leaves `using` active, that is an explicit boundary-state change, not discarded state. A larger contribution may encompass several blocks to close otherwise unrepresentable temporary state, but it may not move interleaved external work.

### 4.3 Explicit effective manifests

Every admitted unit has a small effective manifest:

```text
unit_id             = (root_namespace, package_key, contribution_label)
inputs              = ordered (source_id, region_id, load_mode)
entry_context       = package/search context + declared state preconditions
provided_packages   = package namespaces whose state this unit owns or extends
exports             = owner-derived, not manually trusted
required_units      = candidate dependencies, refined/validated by observations
effect_profile      = declarative | owner-managed | opaque
role                = host-tool | target-package | bootstrap-bundle
```

For NBR the initial explicit input is `src/compiler/native/branch.f`. That file defines public branch helpers, private `TARGET`, constants, and both protected wordlist registrations. [H5]

Use a proposed checked configuration file `build/package-units.json` for explicit unit hints and bootstrap boundaries; the existing source loader/discovery remains authoritative about the actual ordered closure. A hint cannot exclude a source that the program loads. The resulting effective manifest is compiler-owned. JSON is a data carrier here, not a new Habu source grammar.

Illustrative configuration, not an existing file:

```json
{
  "schema": 1,
  "units": [{
    "id": "habu/NBR/core",
    "package": "NBR",
    "inputs": [{"path": "src/compiler/native/branch.f", "region": "whole-file"}],
    "role": "target-package",
    "effect_profile": "declarative"
  }]
}
```

Whole-file units are supported first. Parser-derived regions then support mixed-package files without text rewriting. A region identity uses its owner and a stable local label; offsets locate it in this invocation, but do not become a globally unstable symbol identity.

### 4.4 Shared files, guards, and load completion

Hash a shared file once per invocation; multiple units may refer to its owned bytes. Initially, a changed byte invalidates all units selecting that file. Region-level source hashing can later reduce that cost while retaining directive and source-location dependencies.

Mark a file `required` only after all of its scheduled contributions and load-time work have completed successfully. A hit for its first package must not suppress a later package or a top-level expression in the same file. Preserve separate retained-host and target-loader registries, as the current loader already requires. [H6]

### 4.5 Cycles

Do not confuse a runtime call cycle with a compile-time dependency cycle. Already-declared runtime functions can be linked cyclically without executing anything during import. Recursive groups continue using the ordinary checker’s rules.

Where two units genuinely require one another's not-yet-published compilation-time state, merge the smallest legal contiguous source interval into a compilation bundle, preserving every interleaved contribution. Do not invent forward declarations or expose future names to break the cycle. If the normal source rejects the cycle, incremental mode rejects it too.

## 5. Own one input world

Extend the existing source-view mechanism rather than adding a parallel file cache. It already owns source bytes and canonicalization answers and can freeze subsequent lookup; the normal build entries must establish it before cache decisions. [H7]

### 5.1 Input lifecycle

```text
NEW -> COLLECTING -> FROZEN -> CONSUMED -> RELEASED
```

During collection, read each resolved source into owned immutable storage. Record the requested path, owner root, candidate order, canonical target, and success/missing outcome. Capture declared environment variables with present/absent distinctions, target flags, generator inputs, sysroot/toolchain identities, and build-mode parameters.

Discovery scans those same bytes. Freezing prevents fallback to live disk. Both the retained loader and the target's freshly installed loader use the same input owner through their own provider bindings. A worker receives the frozen input bundle or immutable content-store references whose content digests are verified; it never independently hashes a mutable checkout and assumes equivalence.

This is a coherent **build-owned view**, not a claim that reading a directory magically creates an atomic filesystem snapshot. A mixed set captured while files are being edited is still the exact set compiled and keyed. Atomic repository-snapshot semantics require an explicit filesystem/VCS snapshot supplied as input.

### 5.2 Missing candidates matter

For `require helper.f`, record an absent preferred candidate as well as the selected fallback. After freeze, creating the preferred file must not change this invocation. The next invocation must discover it and invalidate the affected unit.

The identity must include source-root interpretation and lookup policy, not just the final file contents. Do not key the entire build by absolute checkout path unless source semantics observe that path. Begin with conservative physical path identity; portable logical-root remapping is an explicit later mode with tests for path-sensitive source and diagnostics.

### 5.3 Dynamic inputs and generators

A cacheable unit may request only inputs in the frozen manifest. Support declared generators as upstream actions with their own executable, arguments, environment, input digests, and output bundle. Generated bytes are ordinary owned inputs.

An unforeseen input during compilation is not silently added to an already keyed action. If it is discovered before any observable action, abort that unit transaction, collect a new view, compute a new key, and retry under a bounded policy. Otherwise run source-only without cache publication, or refuse in `--incremental-required` mode. Do not restart an arbitrary effectful build and perform its external actions twice.

Static discovery is not allowed to execute source once for discovery and then again for compilation. Dynamic source-order behavior not representable by the declared-input contract remains opaque.

### 5.4 Metadata is not the correctness test

File timestamps, sizes, filesystem watchers, and VCS change lists may prioritize reads. They do not authorize a hit. A touched but byte-identical input can hit; a same-size, same-timestamp changed input must miss. Cache each input digest and framing once within the immutable invocation.

## 6. Identities, interfaces, and keys

### 6.1 Separate identity from revision

| Identity | Definition |
|---|---|
| `PackageKey` | Logical source root/dependency namespace plus canonical package identity from the existing namespace owner. |
| `UnitKey` | Package key plus stable contribution label and role. |
| `SymbolKey` | Defining unit, lexical owner, normalized name, definition kind and local generated discriminator. |
| `ExportKey` | Public package name/alias resolving to a symbol key; aliases need not be new definitions. |
| `TypeKey` | Defining nominal owner, canonical package/tail, arity and nominal form. Layout/version is a separate fingerprint. |
| `WordlistKey` | Package key plus public/private role; reopening refers to the existing logical wordlist. |
| `ObjectKey` | Unit-local persistent object identity, distinct from its future DATA address. |
| `FragmentKey` | Owning symbol plus local fragment role/ordinal. |

Use the existing owners' name normalization, not a new case-folding rule. Namespace roots distinguish identical package spellings supplied by different dependencies. Duplicate/ambiguous names remain subject to existing resolution rules; the cache cannot accept two providers simply because their effects match.

Private generated objects use stable owner-local discriminators, not a global declaration counter. Changing a unit may legitimately change its private IDs; unrelated changes must not renumber public symbol identity elsewhere. Stable identity plus revision avoids treating an edited declaration as both a wholly new namespace and the old verified definition.

### 6.2 Three different hashes

All hashes use domain-separated SHA-256 and an explicitly encoded byte stream: lengths and integers are unsigned little-endian u64; booleans are 0 or 1; missing values have explicit tags; text is length-prefixed. No host-memory struct hashing, pointer hashing, or unframed concatenation.

```text
BuilderKey = H("habu.builder", producer identity, ordered tooling inputs,
               host/target execution profile, writer/layout/checker ABI,
               relevant configuration)

BaseKey(U) = H("habu.pkg.base", package profile version, UnitKey,
               ordered owned source identities, boundary contract,
               actual producer identity, host/target tuple, compiler policy)

ActionKey(U) = H("habu.pkg.action", BaseKey(U), validated ordered observations)

ArtifactID = H(exact serialized artifact bytes)
```

`ActionKey` names the computation. `ArtifactID` names its result. Do not make every consumer depend on the full producer artifact bytes merely because it is convenient to locate them.

A candidate index maps `BaseKey` to a bounded list of `(observation manifest, ActionKey, ArtifactID)` entries. On a hit attempt, validate observations against the current environment at the proper source position. Recompute the action key from those current validated observations, then verify the referenced artifact digest and headers. A stale candidate never supplies the new truth. Keep variants for branch switching, with configurable bounded eviction; eviction costs recompilation, never correctness.

This avoids a circular request to know every dependency before selecting a previous result. A changed branch condition invalidates the recorded controlling observation; the unit is then recompiled and discovers its new observations. Ordered observations are not evaluated against state that would exist only after a stale branch's work.

### 6.3 Actual producer identity

Pin the binary/checker/backend actually performing compilation, not merely the source revision it claims to implement. Include code ABI, checker/effect schema, relocation schema, target word size/endianness/features, optimization/inline policy, and trust policy. Include native extension/FFI tool identities where execution can influence compilation.

Merely compiling an edited target definition does not change the retained producer. However, an explicit checker/definer/backend owner transfer can change which compiled implementation executes subsequent work. Model the producer as an **effective producer vector per action**: retained engine identity plus the current executable checker/backend/definer owner revisions and relevant ABI contracts. At every deliberate transfer, update that vector and invalidate definition-local observations before the next action. Hashing only the seed binary would miss a source-installed checker change.

When the freshly built compiler becomes the next invocation's producer, its new identity legitimately invalidates old producer-bound results. Cross-producer reuse is a separate, explicit compatibility qualification, not an automatic version-string match.

The saved builder may depend on broad resident tooling through its donor identity. It must not also include unrelated target sources in its key. Compute its actual dependency closure through the same input machinery instead of maintaining another handwritten hash list.

### 6.4 Interface fingerprint

Export a canonical, deterministic descriptor derived by the checker and dictionary owners:

- public export-to-symbol bindings and their visibility/compilation behavior;
- verified effect graph roots, quantified kinds, row sharing and constraints;
- input/output widths, value-boundary/glue facts, minimum-input facts, ownership and control properties;
- nominal type identities, constructors, arities, public layouts and representation contracts;
- no-return/throw and ABI facts used by code generation;
- immediate/definer/callback surfaces that consumers can invoke during compilation.

Alpha-equivalent effect variables are numbered by a deterministic owner traversal; shared and cyclic nodes retain identity. Nominal types are never merged because two strings or layouts happen to match. Rendered signature text is diagnostic material, not a replacement for the checked graph. Current AOT state already carries verified graphs, and type-family code explicitly ties IDs to canonical identities; extend those owners for partial export/remapping. [H8]

Do not hash transient registry numbering, diagnostic counters, source-load timing, or code addresses into the semantic interface. Hash source-location metadata separately where relevant to diagnostics or debug policy.

## 7. Dependency observations and invalidation

### 7.1 Record at the owner operation that observes the fact

Source imports alone are insufficient. Record observations at dictionary lookup, checker query, constant evaluation, layout query, inliner consumption, compile-time execution, and owner-state reads. Cover the host compiler and any source-loaded checker/definer instances. A missing hook in one instance is a reason to disable reuse for the affected profile.

The existing dictionary path has private/public/global/used-public and qualified-name rules, including ambiguity and trusted visibility. It also executes stamped fixed-value words to obtain constants/addresses. Use those operations; do not invent a parallel resolver or read constant instruction bytes in the package cache. [H9]

A lookup witness records:

```text
(query kind, source-local step, normalized spelling, logical search context,
 authority mode, outcome: missing | ambiguous | resolved,
 resolved SymbolKey and definition revision where required)
```

Validation re-runs owner lookup in a **staged namespace overlay** that includes the contribution's earlier declarations at that recorded step. Validating every lookup only against the unit's entry environment is wrong: later definitions can shadow earlier ones within the unit. The overlay applies a finite declaration schedule; it does not execute source or arbitrary bytecode.

For qualified lookup, revalidate both the namespace binding and the member. For absence or ambiguity, retain enough of the original query to detect a new candidate. A new name in an already-used package can change resolution even when the previously selected provider did not change.

### 7.2 Dependency classes

| Observation | Fingerprint required for a hit | Typical body-only provider edit |
|---|---|---|
| `BINDING` | Exact current resolution outcome at source position | Can hit if binding remains the same. |
| `CALL_CONTRACT` | Verified effect/control/ABI and semantic call properties | Can hit and relink to new body. |
| `IMPLEMENTATION` | Canonical implementation dependency revision | Miss. |
| `INLINE_BODY` | Body plus assumptions used by the inliner | Miss. |
| `CONSTANT_VALUE` | Typed constant value, including symbolic-address identity | Miss if value changed. |
| `TYPE_LAYOUT` | Nominal identity and physical/logical layout | Miss if layout changed. |
| `COMPILE_EXEC` | Invoked implementation plus its observed input/state closure | Miss when any consumed fact changes. |
| `STATE_READ` | Owner-state channel/value revision | Miss on relevant state change. |
| `SOURCE_RESOLUTION` | Path query/candidates, including absence, owned bytes | Miss when the answer changes. |
| `REFLECTION` | Exact bounded query result, or entire queried namespace snapshot | Usually wider invalidation. |

These are design kinds, not new inferred claims about today's checker effect vocabulary.

### 7.3 Rollout policy

**Conservative mode first:** retain lookup/source/state witnesses, but depend on full canonical provider contribution revision for every recorded provider dependency. Do not include every preceding unit. This is safe only once observations are complete for the admitted effect profile; "conservative" does not excuse untracked inputs.

**Precise mode next:** permit `CALL_CONTRACT` reuse and smaller projections only for observations with an independently tested classifier. Keep the complete provider content as the fallback for unknown use kinds. Compiler, checker, generated-code and inliner consumers may remain conservative longer than ordinary application calls.

Recompute a changed provider and compare the relevant output fingerprints. A source change with unchanged public contract need not propagate through contract-only edges. Keep the previous immutable dependency records and copy the dependencies of a reused unit into the new build receipt; otherwise repeated hits would gradually forget why they were valid.

The principles of stable IDs and comparing observed result fingerprints have a useful precedent in rustc's incremental design. Habu needs only package-level owners and observations here, not a wholesale port of rustc's query engine. [E1]

### 7.4 Invalidation examples

```text
edit NBR:TARGET implementation
  conservative: recompile NBR and units depending on its full revision
  precise:      recompile NBR; relink ordinary contract-only consumers
                recompile consumers that inlined/executed/inspected it

edit exported INSN-BYTES
  recompile NBR and consumers that folded/used the constant

add a public name to a used package
  revalidate lookup witnesses; formerly unambiguous consumers may now fail

change provider type layout but retain its name
  invalidate layout consumers and reject mismatched nominal descriptors

edit an unrelated early contribution
  later independent units hit; physical placement changes are handled by relocations
```

### 7.5 No false dirtiness from placement or bookkeeping

Provider revisions use canonical unplaced content and dependencies, never relocated bytes. Logical predecessor version is relevant when reopening that same package, not merely because another package was installed first. Counter increments, artifact timestamps, and LRU state are not semantic dependencies.

If a definition observes raw dictionary counts, physical addresses, or the complete namespace, the actual observation is wide or placement-sensitive. Do not pretend it is a normal narrow dependency. The cache can still serve other closed units outside that observation's dependency cone.

### 7.6 Promote dependencies when a function executes at compile time

Suppose P calls Q only through a runtime contract, and unit R later executes P while compiling a constant. A body edit in Q can leave P's canonical code and call contract unchanged while changing the constant computed by R. Therefore R must depend on the **executed implementation closure**, including Q, not only on P's code bytes or public contract.

Track actual callable targets through compiler-execution authority, including indirect/deferred calls and their state. For the initial conservative mode, include the full current provider closure admitted for execution. In precise mode, compose implementation and input/state observations across executed calls. Cyclic call graphs use a canonical SCC descriptor rather than recursively hashing hashes until they stabilize. If execution can reach an unknown target or bypass observation, R is not cacheable under the precise profile. A runtime linkage edge becomes an implementation dependency when the compiler executes through it.

## 8. Compile-time effects: the admission boundary

Caching source evaluation is sound only if its observable results and inputs are represented. Typed stack effects alone do not establish that property. Compiling a function that performs I/O at runtime is perfectly compatible with package caching; **executing** that I/O while loading the package is the relevant distinction.

### 8.1 Three admission profiles

**Declarative:** definitions, package bindings, types, typed constants/data, literal allocation, and recognized registry/protection operations. Source evaluation produces owned persistent state and compiler metadata without uncontrolled observations. Capture their semantic results; a hit installs them without executing the original definitions or definers again.

**Owner-managed:** additional compile-time reads and writes go through named, audited owner adapters. Each adapter exports typed inputs, a deterministic result or state delta, relocation/ownership information, validation, installation, and rollback. Examples include a generated-declaration registry, a declared deferred binding, and a build-owned resource table. A channel's expected prior state is a precondition on import.

**Opaque:** source can observe or modify unaccounted process state, raw external memory, environment, files, time, random state, network, or arbitrary native callbacks. Compile it through the normal source path. If its unknown writes can affect later compilation, disable reuse for the remainder of that build region/session. A mere counter saying "barrier happened" is not a fingerprint of the unknown resulting state.

The first implementation admits a compiler-derived set of declarative forms and exact-version owner adapters. A source author cannot make a package cacheable by writing a Boolean annotation. Unsupported dynamically called code, arbitrary `execute`, or unaudited trusted code at compile time denies eligibility until the compiler establishes its execution/read/write closure.

### 8.2 Do not rely on observing file reads alone

An existing machine-code word can read a global cell or call a syscall without passing through the new source loader. Hooking `READ` does not make all compile-time execution pure. Eligibility requires compiler enforcement of the allowed operation/call closure, plus owner adapters for trusted internals. Runtime tracing is useful evidence, but not a completeness proof when unchecked/native operations can bypass it.

An exact-version audited primitive/definer may have a declared effect adapter. The adapter is part of the producer/trust fingerprint. Changing its implementation invalidates its consumers unless explicitly requalified. No ordinary `TRUSTED:` wrapper around package import algorithms is introduced.

### 8.3 Captured local state versus rerun actions

For `create TABLE ...` followed by deterministic initialization of TABLE owned by this contribution, serialize TABLE's initialized bytes and typed references. On a hit, allocate fresh TABLE storage and install that state. Do not run the initializer again and duplicate its effects.

For a change to an existing provider-owned registry, capture an owner-authored delta with a precondition and inverse/savepoint support. Do not save the provider's entire heap and overwrite its current state during import.

Compiler diagnostics can be retained as structured source-addressed records and replayed in source order, with diagnostic policy in the key. Arbitrary top-level `type`, logging to files, spawning processes, and similar user-visible effects are not silently replayed or suppressed. They remain source-only unless promoted to a specifically declared action whose execution semantics are explicit.

### 8.4 Runtime initialization

Separate three different things:

| Phase | Example | Behavior on package hit |
|---|---|---|
| Compile-time evaluation | A definer builds declarations and fills a local constant table. | Install captured semantic result; do not execute the definer again. |
| Build-session owner initialization | Rebind a registry owner or reset a process-local callback adapter. | Run only the finite audited owner install operation, transactionally. |
| Program startup | Register/open a runtime resource when the emitted program boots. | Preserve an ordered runtime-init entry in the final program; do not run it during linking. |

Persistent target state must not retain file descriptors, mutex handles, anonymous mappings, or source-view callbacks from the producer. Represent a reconstruction operation at the appropriate runtime phase or mark the contribution unsupported.

### 8.5 Address-observing source

`here`, a created-word address, or a type/wordlist ID can be used as a typed reference. That reference must become symbolic through the owning compiler operation. Arithmetic over a typed object reference is legal when represented as a validated object/addend relation.

If source converts a process address to an ordinary number, branches on it, emits its decimal digits, or mixes it into an opaque computation, the system cannot recover provenance by scanning for pointer-looking integers. Mark the unit placement-sensitive or opaque. Reuse then requires the actual observed placement/state or remains disabled. The design does not redefine those existing source semantics.

## 9. The contribution object

### 9.1 Core logical data model

The object is immutable after sealing. Every indexed collection belongs to it; no slice borrows scratch storage in `NEMIT`, source input, an earlier live checker, or a transient interner.

```text
Contribution {
    descriptor, effective_manifest, action_identity,
    dependency_observations,
    symbol_table, export_bindings, wordlist_descriptors,
    ordered_install_records,
    code_fragments, data_objects, typed_references, relocations,
    checker_owned_payload, literal_descriptors,
    runtime_init_entries, owner_state_deltas,
    diagnostics, source_map, validation_summary
}
```

A symbol may be a function, constant, created word, deferred word, quotation, generated accessor/constructor, or other existing declared kind. Preserve the exact kind/flags needed by the checker and native compiler. A body offset and rendered effect are not sufficient for an imported constant or immediate.

A `CodeFragment` has a stable owner, exact byte length, alignment, native ABI entry kind, immutable canonical bytes, and complete relocation membership. Do not infer the last return from the following definition. Preserve existing exact-span semantics and distinguish ordinary entries from any separately introduced private register-ABI entries.

A `DataObject` has logical size, alignment, zero-fill policy, initialized extents, type/ownership classification, mutability, and typed reference slots. Zero bytes may still contain a declared nullable pointer slot; a null initial value does not remove the relocation declaration.

### 9.2 Explicit reference types

```text
Ref = Null
    | LocalSymbol(SymbolID, addend)
    | ImportedSymbol(ImportID, addend)
    | LocalObject(ObjectID, addend)
    | ImportedObject(ImportID, addend)
    | LocalType(TypeID)
    | ImportedType(TypeKey)
    | Wordlist(WordlistKey)
    | Literal(LiteralID, addend)
    | EngineAnchor(AnchorKey, addend)
```

The tag is authoritative. Zero is a valid local index, so do not overload it to mean null. Named engine anchors identify stable semantic primitives/slots through a versioned host/target layout contract; an unknown anchor is rejected, never mapped by an old numeric offset. Typed target IDs embedded in code require fixups just as addresses do.

### 9.3 Capture at publication, not by disassembling the final image

Open a contribution collector around ordinary checked source compilation. Dictionary/checker/definer owners attach declarations and verified facts as they become final. The backend attaches emission facts before `NEMIT:CLEAR` retires them. DATA/literal owners report persistent allocations and reference slots where their kinds are known. The contribution closes only after all provisional state has committed.

Resolve absolute targets reported by today's emission rows to authenticated local/imported symbol identities while the live producer still knows those targets. Add missing target/width/addend semantics at the backend boundary. Unknown target provenance denies admission. A post-hoc scan for instruction-looking or pointer-looking values is not the completeness mechanism.

Capture all code/data/private definitions belonging to the contribution, including support needed by future compile-time consumers. Do not apply final entry-point tree shaking. Exclude compiler scratch according to owner lifecycle, not by guessing that unused pages must be temporary.

### 9.4 Literal and object ownership

Serialize literal content and logical owner/offset relationships. Import allocates or interns through the existing literal owner and records the resulting remap. Pointer equality or identity-sensitive constants need an explicit shared-object identity contract; equal bytes alone do not authorize merging distinct mutable or identity-observable objects.

A literal shared across contributions is either owned by an explicit provider or interned by a deterministic owner whose canonicalization policy participates in the producer key. It is never a pointer into another process's discarded source buffer.

## 10. Persistence: one AOT family, a package profile

Do not extend the legacy `HBOBJ` text-blob path into another compiler metadata format. Use a **package-contribution profile of the existing AOT artifact family**, with `.hbp` as a descriptive extension. Keep the existing captured-image profile and the package profile distinguishable and non-interchangeable. Today's `AOT-FILE:READ/MERGE` remains a staging/image operation, not the live importer. [H10]

### 10.1 Version policy

Reserve the next available AOT family version at integration time, coordinated with pending code-generation format work. Do not hard-code a conflicting version increment from another unmerged design. Old readers reject the new version. New readers may read existing image captures only through their explicit legacy profile; they must never interpret an old stripped image as a package contribution.

Retain the existing 136-byte header layout, producer/input/payload digests, and bounded section framing where possible. The new profile's required first descriptor section identifies `IMAGE_CAPTURE` or `PACKAGE_CONTRIBUTION`, its profile revision, target ABI, checker/relocation schema requirements and feature bits. The header target discriminator and the descriptor must agree: allocate distinct target IDs for each supported architecture/OS ABI instead of treating every Linux target as the same target. Section interpretation is selected by that descriptor after payload integrity and bounds validation. Existing fixed section indices are adapted by profile; no caller gets to use image indices on a package object.

The source-view/action digest is the package profile's meaning of the input digest field. Re-hashing arbitrary live files during package artifact admission would violate I1. Existing image-chain semantics remain explicit in the image profile.

### 10.2 Package sections

The profile fixes section IDs/order and requires empty sections to be represented consistently. Reuse the checker/type owner's encoding inside its payload rather than inventing a second graph serializer.

| ID | Section | Content |
|---:|---|---|
| 0 | Descriptor | Artifact kind/profile, required features, host/target/ABI/schema identities. |
| 1 | String pool | Length-prefixed source, package, symbol and diagnostic strings. |
| 2 | Manifest | Unit/action/base identities, ordered inputs, effect/role/boundary policy. |
| 3 | Input observations | Owned file, environment and resolution identities, including missing results. |
| 4 | Dependency observations | Ordered lookup/state/value/interface/implementation witnesses. |
| 5 | Symbols and exports | Stable keys, declared kind/flags, owner IDs, fragment/object/fact roots. |
| 6 | Wordlists | Logical public/private identities, reopen/protection requirements. |
| 7 | Installation order | Finite owner operation descriptors in source order. |
| 8 | Code descriptors | Fragment owner, exact span, alignment, ABI, relocation range. |
| 9 | Code bytes | Canonical unplaced bytes. |
| 10 | DATA descriptors | Owned object sizes/alignment/initialized extents and reference shape. |
| 11 | DATA bytes | Initialized extents; zero-fill sizes remain descriptor facts. |
| 12 | Relocations | Typed source/target references, kind, addend and patch-site sets. |
| 13 | Checker payload | Owner-versioned verified graph, nominal types and exported roots. |
| 14 | Literals | Content and logical ownership/import descriptors. |
| 15 | Initializers/state deltas | Audited owner install operations and ordered program-startup entries. |
| 16 | Diagnostics/source map | Source-owned diagnostic records and definition/source associations. |
| 17 | Requirements/evidence | Native provenance, validation profile, required anchors/FFI contracts. |

This is a wire-profile specification, not a demand to split code into 18 public modules. Section counts are derived from validated lengths where fixed-width rows permit it; variable rows are length-framed. No arbitrary 32-symbol or 64-relocation ceilings. Enforce policy budgets and arithmetic limits before allocation.

### 10.3 Canonical wire rules

All administrative integers and local IDs are LE u64. Signed relocation addends use specified two's-complement i64 and are checked before address arithmetic. Target machine bytes keep their target encoding; wire endianness does not choose target endianness. IDs and byte extents are distinct types even when both use u64.

Lengths, offsets and sections must not overlap or exceed the verified payload. Gaps/padding are canonical zeroes. Validate multiplication with division bounds and ranges by subtraction. Strings need not be interned in hash-table iteration order; write them in deterministic first-owner traversal order. Graph encoding and IDs are canonical within the artifact, never leaked live IDs.

The full file digest used by the content store covers the header as well as payload. Payload-only checks alone do not authenticate producer/header metadata. Unknown required features, malformed IDs, unsupported target profiles and inconsistent declared counts reject before live state is touched.

### 10.4 Indexed readers and memory bounds

Validate the envelope and rows once, construct bounded indexes once, and retain owner-held slices. Iteration uses stored validated row counts. Use indexed symbol and graph lookup; exact key comparison resolves hash collisions. Do not call a full-buffer row counter inside each iteration as the old object codec does. [H11]

Begin with uncompressed binary code/data sections and existing proven sparse DATA techniques where compatible. Compression is not a prerequisite; later formats must declare uncompressed bounds and verify them before allocation. Cache indexes are hints and disposable. Immutable artifacts and input manifests are the authoritative records.

## 11. Relocation and placement contract

### 11.1 Relocation descriptor

```text
Relocation {
    owner_fragment_or_object,
    patch_sites: nonempty list of (local_offset, width, encoding_component),
    kind, target_ref, signed_addend,
    alignment_requirement,
    expected_encoding_class,
    abi_contract_id,
    overlap_group_or_none
}
```

Some instructions encode one logical address across multiple instruction sites. One logical relocation can own several component patches. Independently overlapping writes are rejected; a composite encoding must be described and validated as one supported group.

For every component, before translating any coordinate:

```text
0 <= offset <= owner_length
0 <  width <= owner_length - offset
```

Only then validate final placement, target/addend arithmetic, alignment, expected opcode/encoding, reserved bits, reach, and calling convention. A code relocation may not cross from the last bytes of one fragment into the next merely because the merged allocation has room. This fixes the reviewed cross-object `abs64` case. [H11]

### 11.2 Two relocation applications, one canonical input

**Live import placement** makes imported words callable by the current compiler while processing later source. **Final image placement** produces the target executable after compaction/link layout. Both start from canonical bytes plus symbolic references and current maps.

Never feed already-relocated bytes back as if they were canonical. Never infer whether a cell has "already been rebased." Preserve an immutable source object and distinct staged outputs for live and final placements. Import-time fixups must also register the appropriate relocation/provenance rows with current owners so later capture is correct.

### 11.3 Backend interface

Proposed interface, schematic types:

```text
RELOC:CAPABILITIES(target) -> supported kinds / ABI constraints
RELOC:VALIDATE(fragment, relocation, placement, symbols) -> PatchPlan | Error
RELOC:APPLY(private_bytes, PatchPlan) -> accepted private bytes
```

Validation may reserve a veneer/trampoline through a deterministic layout plan. It cannot opportunistically append code after other placements have been published. Layout repeats only when a monotone, bounded veneer/relaxation decision requires it; all affected ranges are revalidated. A target lacking the necessary veneer form rejects or requests source recompilation under an explicitly supported alternate lowering profile.

ARM64 owns branch encodings, address-materialization sequences and their site components. x86-64 owns relative call/jump/RIP-relative and absolute encodings. Other backends share object identity and checker remapping, not native patch code. Wasm can later implement symbol/index relocations under a different target profile; it must not consume native pointers.

Record every inter-contribution call, not only a call outside the current dictionary region. Two packages sharing today's region still move independently in later builds. Current `NPUB`'s external-region call registration is not sufficient to infer this set. [H4]

### 11.4 ABI and optimizer dependencies

Public calls initially use Habu's canonical stack ABI. Any private register ABI or specialized entry introduced by code-generation work has a distinct entry kind, contract fingerprint, and relocation requirement. Linkers never treat two entry conventions as interchangeable because the source word is the same.

An optimizer that copies instructions from an imported body records an implementation edge. An optimizer using no-return, clobber, width, exception or stack-motion facts records those facts in the call contract. Unknown summarized effects invalidate reuse conservatively.

Placement-dependent instruction selection is a separate hazard from patching addresses. Either use a fixed canonical emission form whose fields can be repatched without changing size, represent explicit layout alternatives, or classify the unit placement-sensitive. A relocation table cannot repair missing instructions selected under a different placement.

## 12. Verified graph and nominal-type remapping

The checker owns serialization, validation, graph canonicalization, and installation. `PKG-IMPORT` supplies mappings and coordinates; it does not implement an alternative type checker.

### 12.1 Required maps

Before publication prepare maps for:

```text
SymbolID     -> staged/live definition handle and final symbol identity
WordlistKey  -> existing or newly reserved public/private WID
TypeID       -> current nominal-family handle
GraphNodeID  -> current verified effect/type graph node
ObjectID     -> staged/live DATA allocation and final logical object
LiteralID    -> current literal-owner handle
FragmentID   -> code placement and exact span
AnchorKey    -> compatible retained/target engine slot or primitive
```

Repeated package contributions must map public/private wordlists to the same package owner, not allocate a fresh WID pair per artifact. Type declarations introduced locally reserve identities; imported types resolve by owner identity and required revision. Distinct nominal families with equal names in different roots stay distinct.

### 12.2 Validation order

Validate owner/schema compatibility, all references and allowed tags, graph bounds and sharing, nominal constructor ownership/arity/form, effect constraints and width/control facts, then graph roots associated with each definition. Reserve all required identities before wiring recursive edges. Use explicit visited states/worklists; a recursive graph must not recurse unboundedly on the C/native stack.

Do not patch integers inside an opaque checker blob by an arbitrary numeric delta. Existing owner routines must export local/imported identity distinctions and import them through the current registry base. A partial package graph is not a full-registry replacement.

### 12.3 Authority

A matching graph checksum proves byte integrity, not that source was verified. Import requires trusted producer provenance plus the expected checker/schema contract and a successfully validated artifact. Only the build coordinator can obtain the privileged installation capability. Ordinary Habu source cannot construct a `VerifiedContribution` by filling a public struct or supplying an arbitrary serialized effect.

After import, a fresh wrong-effect client must still fail. After importing the same graph and then performing another capture/restore, the error must still fail. Preserve row relationships, linear/borrow facts, constructor identity and no-return behavior across that second boundary.

## 13. Transactional live installation

Use the existing `DECLARATION-TRANSACTION` design: ordered participants, snapshots retained through prepare and reversible commit, reverse rollback, non-failing final release, poisoning on rollback failure. It already has a deliberately sealed participant table; do not reintroduce a heap-growing callback table whose addresses escape DATA relocation. [H12]

Create a package-install coordinator instance with a known participant set/capacity sized for its registered owners. Reuse compatible participants, adding narrow owner APIs where the current single-declaration operation is insufficient.

### 13.1 State machine

```text
RAW -> ENVELOPE_CHECKED -> DEPENDENCIES_CURRENT -> OWNERS_VALIDATED
    -> RESERVED -> STAGED -> PREPARED -> COMMITTED -> RELEASED

Any pre-commit error -> reverse ROLLBACK -> RELEASED
Rollback failure    -> POISONED_SESSION -> terminate session; no final output
```

Proposed handle types distinguish raw bytes from validated, prepared, and committed objects. A stage handle cannot be used as a callable public word. All newly callable native code must have target-specific executable-memory/cache-maintenance preparation complete before publication.

### 13.2 Participants and savepoints

| Participant | State that must be prepared or restorable |
|---|---|
| Dictionary/package owner | Namespace records, pending records, live tails, public/private WIDs, aliases, flags. |
| Checker/type owners | Verified graph roots, nominal registrations, effect indexes, declaration ownership. |
| Code owner | Reserved bytes, code cursor, callable-region status, exact spans and provenance. |
| DATA owner | New allocations, initialized bytes, typed reference registrations; journals for approved changes to old state. |
| Relocation owners | Call/address/reference tables, target identity mappings and per-object site coverage. |
| Literal owner | Owned pool entries, content/reference mappings and refcounts/lifetimes. |
| Protection owner | Protected-WID rows and seal state in the correct logical package. |
| Loader/source owner | File-completion marks, occurrence state, source-map/diagnostic ordering. |
| Initialization/registry adapters | Prepared typed deltas, expected old channel state and rollback handles. |

Local reservations may allocate memory during preflight, but they cannot become discoverable definitions. If an owner mutates an existing slot during reversible commit, its snapshot/journal must remain live until every participant succeeds. New output-only APIs should reserve first and perform nonallocating commit where possible.

### 13.3 Installation algorithm

```text
install(current_session, validated_artifact):
    revalidate context stamp and dependency witnesses at this source position
    begin coordinator transaction
    reserve stable identities and code/DATA capacity
    build staged namespace/type/graph maps
    validate all owner payloads and all relocation plans
    copy canonical bytes into private reserved regions
    apply all relocation plans to those regions
    prepare literal/protection/loader/initializer deltas
    prepare executable code for invocation
    prepare every participant
    commit all participants under the installation exclusion boundary
    expose callable dictionary/checker view only after the coordinated commit
    release savepoints without failure
    return InstalledContribution
```

The exclusion boundary forbids concurrent REPL lookup/execution into a partially installed package. V1 has a single owner lane and does not promise lock-free concurrent publication. Recovery/debugger/profiler hooks must not observe half-committed definitions through an interrupt callback.

If a validated contribution becomes inapplicable because a provider or placement changes, discard its plan and retry validation/compilation; do not apply the stale plan to new coordinates. Ordinary edited builds start fresh sessions rather than replacing code in a live application with outstanding frames. Hot reload is a different feature.

### 13.4 Error classes

| Class | Response |
|---|---|
| Candidate not found, producer mismatch, changed dependency | Normal miss; compile source in the owned view. |
| Valid artifact cannot fit current relocation/placement profile | Explicit placement miss or supported layout alternative. |
| Corrupt/truncated artifact or malformed graph | Reject before install, quarantine/remove index hint; automatic mode may compile from trusted source and reports corruption. |
| Invalid source/check failure | Report source error; no success artifact or output generation. |
| Unexpected owner install error | Roll back and report internal failure; do not hide it as a routine cache miss. |
| Failed rollback | Poison/terminate build session; previous external generation remains untouched. |
| Opaque compile-time behavior | Run source-only with the appropriate barrier, or fail `--incremental-required`. |

A corrupt cache is not permission to accept corrupted source, ignore diagnostics, or silently claim a hit. A reported fallback must include its reason and cannot count toward hit acceptance.

## 14. Native selfhosting and bootstrap bundles

Selfbuilding requires more than importing ordinary library packages: the build resets the target dictionary, compiles/replaces checker owners, installs a new source loader, and finally prepares/captures target state. The retained host remains callable through continuations. Preserve that distinction in the incremental design. [H13]

### 14.1 Two roles, not an accidental producer switch

A `host-tool` contribution runs the build. A `target-package` contribution belongs to the executable under construction. They have different role keys even on the same CPU. The actual running compiler and its paired checker determine the producer identity for an action; target source becoming present does not automatically authorize using it as a replacement producer.

The build owns explicit host and target owner capabilities. All import APIs receive the intended owner capability; do not locate a checker or registry by an ambient private name and hope it is the right instance. During a deliberate checker transfer, flush definition-local binding caches and establish the new authority/epoch before continuing source evaluation.

### 14.2 Small explicit bootstrap bundle

Some early core files precede the target checker/loader services needed by an ordinary package importer. Define `habu/bootstrap-target` as the smallest **ordered** bundle enclosing that transition. Its manifest comes from the actual `LOAD-TARGET` prefix and the dependencies required to finish owner/loader initialization; do not invent a second source list that gradually diverges.

On a cold path, the retained qualified compiler compiles that prefix exactly as today while the contribution owner captures its code, DATA, primitive-to-declaration relationships, registry roots, and named engine-anchor writes. On a hit, the retained qualified importer validates and installs the whole prefix, then executes the finite source-checker transfer and loader-provider binding protocol at the same logical boundary.

The bundle's schema must explicitly represent persistent checker/type registry roots and inherited primitive facts. Do not import a second complete copy of host registry state on top of an already installed target registry. Owner adapters identify target-created records, required inherited anchors, and the handoff operation. Bundle installation either completes that transition or rolls back the target session.

A changed checker/layout schema incompatible with the retained importer is a bundle miss and source-bound bootstrap, not a weakened validation path. Keep the ordinary recovery route available. Once the target owner is active, subsequent contributions use the normal per-package importer.

### 14.3 The minimal dispatcher problem

A cache key cannot certify tooling source already loaded from different mutable bytes. Ship the minimal cache dispatcher in the qualified seed/builder image, or load it through an input-owning seed entry that records its exact source bytes before compiling it. A recovery source entry that cannot establish that condition must remain cache-off until it constructs qualified tooling from frozen inputs.

The ordinary dispatcher checks builder compatibility before loading the expensive source-bound driver. A builder miss collects and compiles the tooling from the same owned view, saves an immutable builder image/receipt, and proceeds. Its source-loaded and saved paths share typed writer invocation and owned captures.

### 14.4 Why the builder key is not the target key

Editing `NBR`, application logic, or another target package may reuse the builder. Editing its writer, host-layout adapters, actual compiler, or checker implementation invalidates the relevant builder. A source file serving both roles can legitimately invalidate both; role separation must not omit real tool dependencies.

Builder qualification compares actual producer binary digest, host-tool ordered inputs, ABI/schema/anchor requirements, and relevant environment. It does not accept a directory named after a commit as proof.

### 14.5 Convergence gates

Use an identified qualified compiler C0 and immutable source/input view S:

```text
C0 --cache-off--> C1
C1 --cache-off--> C2
C2 --cache-off--> C3
```

Require the existing selfhosting convergence and runtime gates appropriate to that source/engine pair. Do not count importing C0's cached objects as a proof that C1 can compile the source.

Then, with the **same producer C1**, compare cache-off and cache-on builds of S and of edited view S'. Those products must match under the same layout/signing policy. Cross-producer package reuse remains disabled by default. Producer identity/receipts stay outside the target program's semantic payload so they cannot by themselves prevent compiler fixed-point convergence.

### 14.6 Architecture progression

ARM64 integration uses the same native-publication and AOT ownership boundaries already present. The Intel x86-64 lane implements/qualifies its relocation adapter and supplies the target capability descriptor; it does not fork package graph, cache, type remapping, or loader semantics.

Cross-compilation is not implied by native package import. A target contribution whose code must execute during compilation needs a compatible host-execution form or explicit execution backend. Until supplied, reject that cross-target action; never execute target instructions on the host by assuming all compilation is data transformation.

## 15. Final linking and deterministic equivalence

### 15.1 One cold/hit assembly path

Cache-off means compile all units from the owned view without reading or writing reusable package/builder/final-result caches selected by the requested policy. It should still produce the same in-memory contribution model and use the same installation/layout/finalization logic as cache-on. This makes comparing the two meaningful rather than comparing two different optimizers.

Keep a separate source-bound/legacy differential gate during migration. It catches bugs shared by the new cold and cached paths. An intentional change in canonical allocation/layout must be reviewed as such, not disguised by comparing only new-cache-off against new-cache-on.

### 15.2 Deterministic allocation and publication tape

Give semantic target objects a deterministic allocation/publication order derived from source occurrences and owner-local declaration order. Both newly compiled and imported units reserve code, DATA, dictionary records, wordlists and graph roots by this policy. Canonical symbols are stable identities; numeric handles may differ internally but final serialization derives from the canonical plan.

Compiler scratch must not perturb target layout. Move or detach transient build allocations through existing lifecycle owners before capture. Where existing source semantics genuinely observe layout, retain the declared footprint/placement dependency or mark the unit placement-sensitive. Do not delete padding or allocations merely to make a cache file smaller if source can observe them.

A package hit must not create additional hidden target declarations, debug rows, or literals that a cold compilation would not create. Source maps and complete `.names` rows remain deterministic; scheduling/completion order never determines their order.

### 15.3 Reachability and export roots

Compute final reachability from the current program's entry, retained runtime/REPL/debug requirements, runtime initialization, callbacks, address-taken definitions and exported interfaces. Private symbols can remain reachable and must be kept when required. A package artifact was deliberately unstripped so today's consumer and later compile-time use do not depend on the previous program's root set.

Final stripped application output may omit checker/private naming payload not needed at runtime, according to existing product policy. A compiler/REPL product must preserve its checked loading requirements. That final distinction is separate from whether a package artifact is reusable.

### 15.4 Final build key

```text
FinalKey = H("habu.final-generation", ordered current contribution identities,
             root/entry set, all required load-time action outcomes,
             complete validated input-view identity, builder/linker/writer identity,
             target + layout + code ABI + runtime-init policy,
             product/whitebox/debug/optimization/diagnostic policy,
             external linkage/sysroot contract, signing/smoke policy)
```

A no-op final-result hit is allowed only when every required build-time side effect is either absent or explicitly satisfied by the action contract. An opaque top-level logging/process action prevents simply returning a prior executable and suppressing source evaluation.

For cacheable deterministic builds, validate the final generation receipt and its executable/name-map digests. No recompile, relink or repeated successful smoke is necessary merely to rediscover bytes whose identical inputs and validation policy are already established. A changed smoke/validation policy invalidates that receipt. Diagnostic settings belong in the appropriate lint/diagnostic action key, so warnings-as-errors cannot reuse an incompatible success.

### 15.5 Equality requirement

For the same producer, frozen inputs, target, mode and layout/signing policy, cache-on and cache-off must emit identical executable and `.names` bytes on the presently qualified deterministic native path. Also compare canonical contribution graph/interface digests and behavior, because equal broken outputs are possible.

Where a future signing mechanism introduces external nondeterminism, compare the unsigned executable and semantic metadata exactly, then separately verify the signature and bind the signed artifact to those digests. Do not apply that exception to current deterministic output just to hide a mismatch.

## 16. Cache storage, trust, races and eviction

### 16.1 Store layout

Illustrative versioned directory layout:

```text
<cache>/pkg-v1/
    blobs/sha256/<prefix>/<artifact-id>        immutable AOT-family objects
    candidates/<base-key>                    bounded candidate metadata
    actions/<action-key>                     receipt -> artifact-id
    builders/<builder-key>                   builder generation receipt
    finals/<final-key>                       final generation receipt
    staging/<invocation-id>/                 unique uncommitted work
    leases/<invocation-id>                   references retained during use
```

Use existing private build-cache root resolution and checked filesystem primitives. Temporary names are invocation-unique, not a global `.tmp` appended to a key. A malicious path/key cannot escape the cache root.

Write and verify the immutable blob first; publish its action receipt next; update candidate hints last. Concurrent identical actions can both compile, but only verified complete results become visible. If the same ActionKey produces different ArtifactIDs under a deterministic profile, record a reproducibility failure and retain evidence. Do not choose one silently and call the conflict harmless.

This action-result/CAS distinction follows the useful storage separation documented by Bazel, without adding a Bazel dependency. Its sandboxing guidance also illustrates why undeclared inputs matter; Habu still needs compiler-specific state/authority enforcement, not just a filesystem sandbox. [E2, E3]

### 16.2 Trust boundary

V1 accepts artifacts produced within a private local cache by the qualified toolchain under the current trust policy. Check directory ownership/permissions through the platform facilities, producer compatibility, full artifact digest, envelope/owner schema, and current input witnesses.

A SHA-256 digest detects changed bytes but does not prove that an attacker-supplied machine-code artifact implements its claimed checked source. Do not enable arbitrary shared/remote native artifacts simply because their checksums match their filenames. Future shared caches require authenticated trusted producer attestations or local recompilation from trusted source. There is no new promise that structural graph validation proves arbitrary machine code correct.

Invalidation is not revocation: a changed trust policy/producer allowlist must participate in admission even for otherwise matching keys.

### 16.3 In-flight source and publication races

Input views are immutable once owned, so a newer checkout cannot change an active computation's key. However, an older invocation can finish after a newer one and otherwise overwrite its output. At invocation registration assign a monotonic per-destination request ticket. Under the destination publication lock, publish only when policy permits that ticket: default latest-registered successful request wins; an older request is reported as superseded and its valid generation remains addressable by ID.

If the newer request fails, keep the last published successful output; do not silently promote the stale older request unless the user explicitly selects that build generation. This avoids a successful old build unexpectedly becoming the result of a newer failed edit.

Advisory locks serialize publication, not all compilation. Locks never authorize trusting unvalidated bytes. Handle abandoned staging safely after crashes; do not steal a live lock based only on a PID that could have been reused.

### 16.4 Eviction and cleanup

Readers retain an open immutable artifact or lease before a garbage collector removes candidates/blobs. Losing an index entry causes a miss, not corruption. Roots include active build leases, selected output generations, explicit pins and retained recent action variants. Byte/age limits are configurable; no graph node becomes a dangling reference silently counted as a hit.

Build-unit observers and temporary maps are process-local or lifecycle-registered exactly as their owners require. Capture must detach/reset source-view and build-only callbacks before target serialization. A hit cannot persist an observer identity or pointer from a prior build process.

## 17. Atomic output generations

Stage all members before selecting the new output:

```text
generation/
    program
    program.names
    receipt
    diagnostics
```

The receipt binds the executable and name map digests, FinalKey, producer/target/mode, validation policy, and source/input manifest. Write names before publication; sign and smoke the staged program; verify all members; then select the generation.

Avoid a digest cycle: finalize/sign the program, compute its digest, write the canonical name map with that executable digest if a cross-check field is needed, compute the map digest, and then write the canonical receipt. Derive the generation directory ID from the two output digests and the canonical receipt (which does not contain its own generation ID). Invocation tickets, timings and cache-hit statistics live in a separate operational report. Do not put producer-dependent generation IDs into executable/name-map bytes, where they could destroy otherwise valid selfhosting convergence. Multiple request receipts can refer to an equivalent output pair without changing the program.

### 17.1 POSIX native output contract

For the first macOS/Linux implementation, use an immutable destination-side generation directory and an invocation-unique temporary symlink for the requested executable path. Atomically rename that symlink over the output selector after the generation is complete. Running the requested path still executes a native binary; no launch daemon or VM is introduced.

Update participating tools to resolve the executable selector **once**, then read `program`, `program.names` and `receipt` from that resolved generation. Following separate `out` and `out.names` links independently across a publication is not a consistent read. A compatibility `out.names` alias may be provided for older tooling, but it is not the atomic-pair authority; readers must validate its embedded generation/executable digest or migrate to the resolver.

Exactly one selector is authoritative. Do not invent a second `current` file whose update can disagree with the executable symlink. Generation directories live on the destination filesystem; cache blobs can be copied into them before selection. Durable publication additionally requires flushing member files and the relevant directories through the OS adapter; atomic visibility and power-loss durability are distinct properties.

### 17.2 Other platforms and flat export

The platform interface is `STAGE`, `VALIDATE`, `SELECT`, `RESOLVE-GENERATION`, `RETAIN`, and `RELEASE`. A future Windows implementation must supply equivalent selection through managed launch/tool consumers or a platform-qualified indirection. Do not assume replacing a mapped executable or creating privileged symlinks works like POSIX.

An explicitly requested flat-file export is a deployment format, not an atomic multi-file selection protocol. Stage all members, serialize export, include digest cross-checks, and document that unaware external readers cannot obtain a mathematically atomic engine/name-map pair by opening two independently replaced files. Do not make that false guarantee.

### 17.3 Failure contract

Any failure before selector replacement leaves the prior selected generation untouched. Failure after replacement is only nonessential cleanup/reporting; essential name-map generation cannot occur after commit. Retain previous generations until active readers release them or conservative retention permits cleanup.

## 18. Build driver and proposed API contracts

### 18.1 End-to-end algorithm

```text
build(request):
    ticket = register_destination_request(request.output)
    inputs = own_and_freeze_inputs(request)
    tools = select_or_build_qualified_builder(inputs, request.tool_profile)
    plan = derive_ordered_plan(inputs, request)

    if plan.all_build_time_effects_are_cacheable:
        candidate = find_final_generation(plan, inputs, tools, request)
        if candidate.valid_and_current:
            replay_compatible_diagnostics(candidate)
            select_generation(candidate, ticket)
            return exact_final_hit_receipt

    session = create_target_session(tools, inputs)
    for occurrence in plan.semantic_order:
        if occurrence.is_require_already_satisfied(session):
            record_require_skip(occurrence)
            continue
        if occurrence.is_opaque:
            compile_original_source(occurrence, inputs, session)
            apply_unknown_state_barrier_when_required(session)
            continue
        candidates = lookup_candidates(base_key(occurrence))
        hit = validate_candidate_in_current_overlay(candidates, session)
        if hit:
            install_transactionally(hit, session)
            retain_current_dependency_manifest(hit)
        else:
            contribution = compile_with_capture(occurrence, inputs, session)
            if contribution.is_admissible:
                validate_and_store(contribution)
            else:
                record_source_only_reason_and_barrier(contribution)
        complete_source_occurrence_only_when_all_parts_finish()

    program = freeze_complete_target(session)
    staged = link_write_names_sign_and_smoke(program, inputs, tools, request)
    store_generation_receipt(staged)
    select_generation(staged, ticket)
    return full_phase_and_reuse_receipt
```

A successful compiled contribution can be stored before the whole application succeeds, provided its own transaction and producer/source evidence are complete. A failed later client does not invalidate a correctly checked earlier package. Never store provisional declarations or a partially captured unit.

### 18.2 Proposed typed operations

Schematic contracts are given below instead of pretending these are existing Forth words. Final spellings must follow Habu's current checked ownership conventions; owned handles cannot be silently duplicated or discarded.

| Operation | Input | Output and contract |
|---|---|---|
| `INPUTS:FREEZE` | Owned collecting view | Immutable view or input error; no disk fallback thereafter. |
| `PLAN:DERIVE` | Frozen view + request | Ordered plan and explicit source-only reasons. |
| `DEPS:BEGIN` | Unit + current owner capabilities | Scoped observer; nested compiler instances inherit it deliberately. |
| `DEPS:SEAL` | Completed observer | Immutable complete observation set or non-cacheable result. |
| `PKG:CAPTURE-BEGIN` | Unit + owner savepoints | Scoped collector with no public declarations of its own. |
| `PKG:CAPTURE-SEAL` | Completed source transaction | Owned unstripped contribution; no borrowed emission buffers. |
| `ARTIFACT:READ-STAGED` | Owned bytes + required profile | Structurally validated immutable object, no live mutation. |
| `PKG:VALIDATE` | Object + session + input view | Current dependency/owner-validated handle or typed miss/error. |
| `PKG:PREPARE` | Validated handle + session | Reserved staged install plan with rollback ownership. |
| `PKG:COMMIT` | Prepared plan | Installed handle, or rollback/internal failure; dictionary publication last. |
| `PKG:ABORT` | Uncommitted plan | Restored owner state; a failure poisons the session. |
| `GENERATION:SELECT` | Complete verified generation + request ticket | Selected or superseded, with old generation preserved on failure. |

Do not add these as unchecked wrappers around arbitrary pointers. Begin with existing opaque/protected owner handles and checked result types; add the small engine-bound primitives only where current memory publication requires them.

### 18.3 CLI

Preserve current source and output arguments. Add proposed switches consistently to both native selfbuild and application build drivers:

```text
--incremental=auto|off|required
--cache=read-write|read-only|off
--explain-rebuild
--build-report <path>
--verify-incremental
--unit-plan <path>
```

`required` fails when a required planned unit is not eligible or an opaque barrier prevents intended reuse; it is a test/developer mode, not a way to bypass checks. It need not require a hit on an empty cache: distinguish eligibility from hit policy in the report, and use an explicit acceptance assertion for a mandatory warm hit.

`verify-incremental` runs a separate cache-off comparison in fresh processes using the same immutable input bundle. It compares executable/names/contribution interfaces and retains divergences; it does not re-read a moving checkout and call that a valid comparison. `cache=off` must disable all reusable layers chosen by the request, not just final executables while silently using packages.

### 18.4 Diagnostics and reports

Every unit reports `compiled`, `imported`, `source-only`, or `skipped-require`, with a typed reason. Record the actual producer, required/current observation values, dependency edge kind, source position and artifact identity for a miss. Do not print secret environment values; retain their presence and digests with controlled diagnostic disclosure.

Illustrative reasons:

```text
NBR/core: compile — SOURCE_BYTES_CHANGED(branch.f)
USER/core: import — observations current; 0 definitions compiled
FORMAT/core: compile — CONSTANT_VALUE_CHANGED(PROVIDER:WIDTH)
CLI/core: compile — LOOKUP_BECAME_AMBIGUOUS("RUN")
legacy/setup: source-only — UNTRACKED_COMPILE_TIME_STORE
build: superseded — newer destination request registered
```

These are examples, not observed results from this design session.

## 19. Performance contract and instrumentation

The external invocation is the timing authority. Start before launching the build command and stop after required reporting/publication. Existing internal timings omit some tool loading/report work; do not base acceptance on that partial timer. [H2]

Record at least:

```text
process/startup, dispatcher, input reads/hashes/discovery,
builder selection/restore/compile, candidate lookup,
artifact verification/decode, dependency validation,
source compile (checker + backend) by unit,
owner graph remap, relocation plan/apply, install,
bootstrap transfer, target preparation/capture,
final reachability/layout, writer, names, signing, smoke,
publication, required diagnostics/reporting, cleanup
```

Count source bytes evaluated, definitions checked/compiled, packages imported, cache entries inspected, lookup witnesses, graph nodes/edges remapped, relocations planned/applied, copied bytes, anonymous mappings, subprocesses, and peak resident memory. `source-hash-bytes` is not `source-evaluated-bytes`; a genuine hit can read/hash source without compiling it.

### 19.1 Acceptance budgets

The following are engineering targets to qualify on the identified host, not predicted measurements:

| Case | Target |
|---|---|
| Unchanged deterministic build, final-generation hit | Roughly <=1 s median whole invocation for the current native tree; tighten/adjust only from actual input-size/platform evidence. |
| Small late or early independent source edit | <=5 s median, with <=10 s p95 as an initial "handful of seconds" acceptance budget. |
| Hot package import | No definition compilation/source evaluation for that package; near-linear work in its actual metadata/bytes. |
| Cold cache population | Report overhead against source-bound/cache-off build; target <=10% steady-state overhead once instrumentation/codec are optimized. |
| Uncached compiler | Preserve its separate correctness and throughput goals; incremental hits do not satisfy them. |

If the qualified source's uncached bootstrap or opaque region alone exceeds the edit budget, the package campaign is not complete. The remedy is finishing the owner adapter/bundle or attributing a remaining cost, not removing the phase from the timer.

Use fresh processes, repeated trials, fixed producer/input bundles, a quiet machine, and explicit CPU/memory conditions. Report medians, p95, CPU time, variation, and raw receipts. Avoid asserting a measured percentile from an insufficient sample. Record cold filesystem, warm filesystem, empty artifact cache, warm artifact cache and producer-change cases separately.

### 19.2 Avoid replacing compilation with quadratic import

Use one content digest per owned input and one validated parse/index per artifact in the session. Hash only observations actually needed by admitted units; reuse verified provider fingerprints in the session. Share imported dependency identity/graph references through owner maps rather than reimporting the entire dependency graph for every consumer.

Target complexity is approximately O(input bytes + artifact bytes + declarations + graph edges + observations + relocation sites), plus bounded indexed lookup and intentional layout work. A test with a large text blob and a handful of metadata rows is insufficient; scale rows, graph size, lookup count and fan-out independently.

## 20. Parallelism and persistent processes

V1 is deliberately useful without a daemon: a precompiled builder image starts a fresh process, imports unchanged contributions, compiles the affected units, and exits. This removes substantial repeated work without a long-lived mutable compiler becoming the correctness authority.

Later, closed units may compile in worker processes with a frozen input view and explicit imported environment. They return immutable contributions; a coordinator validates and publishes in semantic order. Worker completion order cannot choose numeric target handles, source diagnostics, literal placement, or `.names` order.

Do not distribute a unit that depends on unknown ambient state. Do not parallelize two owner-managed actions that mutate the same state channel unless commutativity/isolation is explicitly proved. Source declaration order and valid recursion groups constrain scheduling even when their codegen jobs look independent.

A future persistent process is only a disposable acceleration layer over these immutable artifacts and verified action identities. A restart must reproduce results and hit eligibility. It must not become necessary to retrieve authoritative dependency state.

## 21. First executing acceptance: NBR

NBR is an intentionally small real package, not a synthetic stand-in for the whole compiler. The baseline source has public `BL-TARGET`/`B-TARGET` wrappers, private `TARGET`, typed constants and public/private protected-WID registrations. [H5]

Build this test before implementing the importer:

1. A qualified producer compiles declared NBR inputs normally and writes an owned package artifact.
2. A new process starts the current compiler; additional independent declarations, DATA and nominal types deliberately shift placement and live IDs.
3. At NBR's ordinary load position, the coordinator imports NBR. The execution loader is configured to throw if it attempts to evaluate NBR source; reading/hash verification of source inputs remains allowed.
4. Fresh client source calls the public functions and gets the same values as a cold load. For example, `BL-TARGET(4096, 0x94000002)` is 4104 and `B-TARGET(4096, 0x17ffffff)` is 4092, by the arithmetic in the source.
5. A wrong-effect/type client is rejected by the current checker, and external lookup cannot reach private `TARGET`.
6. Public/private WIDs retain the required protection. Reopening resolves the same wordlists, not new copies.
7. Emit the complete product and `.names`, run it, capture it again and verify the same properties.
8. Compare with a cache-off build using the same producer, frozen inputs and placement policy.

Ensure the test actually exercises an out-of-line private reference. Check emitted relocation rows; if the default optimizer inlines the tiny wrapper, add a controlled no-inline test compilation profile or an additional private-call fixture under the same importer contract. Inlined arithmetic alone does not prove private-symbol relocation.

Then edit NBR's private callee semantically, rebuild, and verify changed behavior, expected invalidation reasons, and equality against a fresh cache-off build. Repeat with the exported constant, a newly introduced private data reference, and an imported provider change.

The second real package must contain a type family/constructor and a typed DATA/code-pointer field; NBR alone cannot qualify graph or persistent pointer remapping. The third must exercise an audited definer/registration operation. These are mandatory next coverage, not optional polish.

## 22. Acceptance matrix

All tests are proposed requirements. This document does not report that they have been executed against Habu.

| Test ID | Scenario | Required evidence |
|---|---|---|
| U01 | Single-file NBR contribution | Real artifact, fresh-process hit, 0 NBR definitions compiled. |
| U02 | One file with two packages | Both contributions run/import in order; file completion only after both. |
| U03 | One package reopened around another | Original visibility/order preserved; no early publication of later names. |
| U04 | `package` text inside quotation/string/comment | No false unit boundary. |
| U05 | Repeated `require` | One completed source load per loader context; hit does not duplicate state. |
| U06 | Repeated `include` | Ordinary repeated-load behavior, including legitimate duplicate errors/effects. |
| U07 | Nonempty cross-boundary stack | Enlarge legal unit or source-only classification; never silently discard values. |
| U08 | Genuine compile-time cycle | Legal ordered bundle or same rejection as uncached source. |
| U09 | Runtime recursive call group | Existing checker rules and symbolic linking preserved without premature declarations. |
| I01 | Mutate input after freeze | Compilation and ActionKey both use original owned bytes. |
| I02 | Create missing preferred include | Current invocation uses original fallback; next invocation re-resolves and misses. |
| I03 | Same-size/same-timestamp content mutation | Miss based on content. |
| I04 | Timestamp-only change | Hit when content and observed metadata semantics are unchanged. |
| I05 | Worker starts after checkout changes | Worker consumes the same immutable input bundle, not new disk contents. |
| I06 | Undeclared generated/dynamic input | Controlled source-only/retry/refusal; never publish under an old key. |
| I07 | Missing versus empty environment variable | Correctly distinct when compilation observes presence. |
| I08 | Symlink/root resolution change | Current frozen answer stable; next invocation invalidates relevant observations. |
| D01 | Runtime-only provider body edit | Conservative mode invalidates as specified; precise mode relinks valid contract-only consumer. |
| D02 | Constant value edit | Consumers that folded it recompile even with unchanged public stack effect. |
| D03 | Type layout edit | Layout-dependent consumers miss; wrong nominal references reject. |
| D04 | Inlined body edit | Every affected inlining consumer misses. |
| D05 | Compile-time macro/definer edit | Consuming units recompile with implementation/state closure. |
| D06 | Runtime helper of a compile-time-executed function changes | Compile-time consumer misses despite unchanged intermediary contract/canonical code. |
| D07 | Global/private/used-public shadowing changes | Resolve by existing precedence; stale bindings cannot survive. |
| D08 | Added public name creates ambiguity | Fresh ambiguity failure; old cached resolution not reused. |
| D09 | Previously absent optional name appears | Negative witness invalidates. |
| D10 | Internal earlier declaration shadows later lookup | Overlay revalidation uses source-local steps, not only entry state. |
| D11 | Conditional changes dependency branch | Stop validating stale branch after changed control observation; compile current branch. |
| D12 | Repeated all-hit builds | Dependency manifests remain complete across generations. |
| D13 | Early independent edit | At least one later independent real package remains a hit. |
| D14 | Same spelling from different source roots | No mistaken provider/type identity merge. |
| E01 | Local deterministic DATA initialization | New storage with captured values; initializer not run twice. |
| E02 | Owner-managed registry mutation | Preconditions checked and full rollback works. |
| E03 | Raw untracked compile-time store | Unit non-cacheable; required unknown-state barrier applied. |
| E04 | Arbitrary top-level I/O | Not skipped on unchanged final-result hit; hit is disallowed where necessary. |
| E05 | Runtime I/O function not executed during build | Package may remain cacheable; no overbroad side-effect rejection. |
| E06 | Address cast to ordinary number and observed | Placement-sensitive/source-only handling, not numeric pointer guessing. |
| E07 | Compiler warning and warnings-as-errors mode | Correct diagnostics and invalidation/success policy. |
| E08 | Runtime initializer order | Executes at program startup exactly as cold output, not during artifact decoding. |
| R01 | Object A has 8 bytes, abs64 at 4; B follows | Reject before writes, even though merged 16-byte allocation fits. |
| R02 | Patch exactly ends at owner boundary | Accept if kind/alignment/target valid. |
| R03 | Valid first relocation, invalid later one | No partial public buffer/counter/registry mutation. |
| R04 | Overlapping patches | Reject unless one validated composite group owns them. |
| R05 | Changed code and DATA placement | Public/private calls, quotations, literals and typed addresses remain correct. |
| R06 | Changed live WID/type/record IDs | Owner remaps code immediates, graph references and DATA correctly. |
| R07 | Near/far reach limits | Correct supported veneer or named range miss/refusal, never truncation. |
| R08 | Wrong instruction/encoding at patch site | Reject before patching. |
| R09 | Null typed code/DATA field | Null remains null; declaration remains registered for subsequent capture. |
| R10 | Import then final link then recapture | No double rebasing or stale producer address. |
| R11 | Local/imported alias and addend limits | Preserve ownership; reject out-of-bounds/overflow addends. |
| R12 | Inter-package call within one old region | Relocation is still recorded and moves correctly. |
| T01 | Shared/cyclic verified graph | Preserved sharing, terminating bounded validation, correct current IDs. |
| T02 | Corrupt graph edge/constructor identity | Rejection before registry publication. |
| T03 | Textually equal but different nominal types | Remain distinct; wrong client rejected. |
| T04 | Linear/borrow/row-tail/width facts | Fresh clients obey original verified constraints after import and recapture. |
| T05 | Package reopen/protection state | Same WIDs and authority; external private lookup still fails. |
| X01 | Fail at every owner prepare/commit step | Complete state digest equals pre-import state after rollback. |
| X02 | Allocation/capacity failure | No half-published definitions; old state usable. |
| X03 | Rollback failure injection | Session poisoned/terminated, not reused; output generation unchanged. |
| X04 | Observer/debugger callback during commit | No partial dictionary/checker view exposed. |
| C01 | Corrupt/truncated artifact with executable payload | Reject; auto fallback reported, not a hit. |
| C02 | Producer/target/schema/mode mismatch | Miss/refusal through explicit compatibility rules. |
| C03 | Candidate points to wrong but valid artifact | Full header/action/digest checks reject mismatch. |
| C04 | Same ActionKey yields different results | Reproducibility failure with evidence retained. |
| C05 | Concurrent cache writers | Only complete validated artifacts/receipts visible. |
| C06 | GC during read/import | Lease/open immutable object remains usable; no dangling hit. |
| C07 | Untrusted shared native artifact | Admission denied without appropriate trusted provenance. |
| P01 | Names/sign/smoke failure | Previous selected engine+names generation unchanged. |
| P02 | Kill at each publication boundary | Resolver selects complete old or new generation, never a partial pair. |
| P03 | Older build finishes after newer request | Ticket policy prevents stale output takeover. |
| P04 | Two outputs share cache | No fixed temporary file collisions. |
| P05 | Consumer holds a generation while publishing next | It reads a consistent retained engine/name-map pair. |
| B01 | Builder reuse after target-only edit | Actual ordinary-command builder hit, not manually bypassed qualification. |
| B02 | Writer/donor/layout change | Builder miss or explicit source-bound path. |
| B03 | Bootstrap bundle hit | Correct target checker transfer, fresh loader binding and core facts. |
| B04 | Unsupported bootstrap schema transition or changed executable checker owner | Safe source-bound rebuild; new effective producer vector for later actions; no relaxed graph validation. |
| B05 | Cache-off C0→C1→C2→C3 | Existing convergence criteria and tests, independent of cached objects. |
| B06 | Same-producer cached/uncached edited build | Exact engine and `.names` equality plus behavior. |
| B07 | Product versus whitebox and entry/preseed options | No cache cross-contamination; policy preserved. |
| A01 | ARM64 and Linux x86-64 profiles | Same semantic package tests plus target-specific relocation/execution gates. |
| S01 | Metadata rows scale at fixed payload size | Indexed near-linear admission/import; no repeated full scans. |
| S02 | Payload bytes scale at fixed metadata size | Byte-copy/hash cost measured separately. |
| S03 | Large fan-out dependency graph | Reuse maps/fingerprints; no import of complete provider graph per edge. |
| S04 | New process with warm caches | External timings include startup, validation, signing policy and publication. |

### 22.1 Mutation testing

For each important refusal, remove or weaken that check in a private test build and require a relevant acceptance test to fail: owner-local relocation bounds, graph-reference validation, source-view freezing, negative lookup tracking, dependency-edge classification, producer qualification, transactional rollback, and generation selection. A suite that still passes after those checks are removed does not establish their protection.

### 22.2 Differential edit corpus

Create a small automatically generated corpus of legal package arrangements, declarations, constants, aliases, private/public changes, and source-order edits. For each edit, run cached and cold builds from the same frozen inputs, compare the canonical contributions/diagnostics/executables, then run public entry behavior and negative type clients. Include removal/rename/addition, not only changed integer literals.

Randomized physical placement and registry padding are test inputs with fixed recorded seeds. They must not affect semantic action identity unless a tested source genuinely observes placement. Record both the generated source and placement seed for reproduction.

## 23. Implementation work packages and dependencies

This is a full delivery plan. The milestones are ordered integration gates, not permission to stop after the first leaf.

| ID | Work package | Main existing/proposed locations | Depends on | Exit evidence |
|---|---|---|---|---|
| W00 | Pin source/host and establish external phase baseline | `tools/build-profile.f`, native/application entry wrappers, new report schema | — | Immutable receipts for cold/repeat/early/late edits. |
| W01 | Own ordinary invocation inputs, including minimal entry and workers | `tools/native-source-view.f`, `src/core/include.f`, native and hb-build entry/core | W00 | I01–I08; normal command uses frozen view. |
| W02 | Ordered unit plan and boundary validation | `tools/source-discovery.f`, `tools/event-closure-lib.f`, proposed `tools/package-build/plan.f` | W01 | U01–U09; effective manifests tied to actual loader events. |
| W03 | Stable owner/symbol/type/wordlist identities | `src/compiler/native/dict.f`, dictionary owner, `src/core/type-family.f`, proposed `src/compiler/package/identity.f` | W02 | Identity tests across reordered unrelated definitions and roots. |
| W04 | Complete dependency observers and eligibility profiles | Dictionary/checker/constant/layout/inliner/compile-execution owners; proposed `src/compiler/package/dependencies.f` | W03 | D07–D12, E03–E06; unsupported paths cannot claim cacheability. |
| W05 | Extend sealed emission with symbolic relocation coverage | `src/compiler/native/emission.f`, backend emission owners, `src/compiler/native/publish.f` | W03 | R01–R12 and explicit coverage for intra-region inter-package references. |
| W06 | Partial verified graph export/import owner API | `src/core/checker.f`, `src/core/type-family.f`, owner graph codecs | W03 | T01–T04; no rendered-text substitution. |
| W07 | Contribution DATA/literal/definer capture | DATA/literal/generated-declaration/address-cell owners; proposed `src/compiler/package/capture.f` | W02–W06 | E01–E02, E08, R09; private/typed state complete. |
| W08 | Package profile of AOT envelope and indexed codec | `src/habu/aot-file.f`, `src/habu/aot-owned.f`, profile adapters, proposed `src/compiler/package/artifact.f` | W03, W05–W07 | Malformed/large artifact tests; owned lifetimes; deterministic bytes. |
| W09 | Transactional live package importer | `src/core/declaration-transaction.f`, `src/compiler/native/publish.f`, package/checker/literal/loader owners | W04–W08 | NBR real-load acceptance plus X01–X04. |
| W10 | Content/action/candidate cache and admission | `lib/build-cache.f`, existing hash/fs helpers, proposed `tools/package-build/cache.f` | W01, W08–W09 | C01–C07 and branch-switch variants. |
| W11 | Qualified saved builder dispatch through ordinary entries | `tools/native-builder-image.f`, `tools/native-build-core.f`, hb-build tooling | W01, W10 | B01–B02; ordinary-command hit with correct tool identity. |
| W12 | Bootstrap target bundle and owner transfer | Native driver `LOAD-TARGET`, checker/loader owner adapters | W06–W11 | B03–B06; no target/host authority confusion. |
| W13 | Full expensive package-family coverage | Runtime/compiler/checker support manifests and missing owner adapters | W09–W12 | Every major target-load phase classified; no hidden opaque suffix masking goal. |
| W14 | Unified deterministic final link and source-bound differential | Native capture/layout/writer, proposed package linker, image-name tooling | W05, W07, W09 | Exact hit/cold output and independent source-bound comparisons. |
| W15 | Immutable generation publication and output resolver | Native/application output installation; platform fs/sign/smoke adapters; name-map consumers | W01, W14 | P01–P05; corruption/failure preserves selected generation. |
| W16 | Final-generation cache, diagnostic/lint cache policy, explain CLI | Native and hb-build core/report code | W04, W10–W15 | No-op hits with no suppressed required effects; full report. |
| W17 | Precise dependency projections | Checker/control/inliner/constant/layout/execution dependency owners | W04, W09, W13 | D01–D06 with conservative/precise expected differences. |
| W18 | Linux x86-64 adapter and gate integration | Intel lane backend emission/relocation, shared package tests | W05, W09, W14 | A01 on an identified native x86-64 engine; no duplicated graph semantics. |
| W19 | Scalability, fuzz/mutation/differential gates and performance qualification | New package suites and profiler/report tooling | W13–W18 | Entire matrix, target/application gates, external edit budget. |
| W20 | Optional isolated parallel producers | Existing process/input facilities, new scheduler only if measured useful | W19 | Same outputs/diagnostics across worker counts; demonstrated benefit. |

### 23.1 Milestones

**M1: correctness substrate** — W00–W06 and failing-first NBR/relocation/input tests. Existing builds remain cache-off/source-bound where profiles are incomplete. Existing safety fixes are integrated rather than hidden by the new path.

**M2: a real checked package hit** — W07–W10, NBR plus graph/DATA/definer fixtures. Import without source evaluation works in a fresh process and after shifted placements. This is the first cache claim, with counters and receipts.

**M3: native selfbuild and normal-command integration** — W11–W16, including bootstrap bundle, major runtime contributions, both build commands, generation output, and exact-repeat hits. Measure the actual ordinary edit path, not a standalone importer benchmark.

**M4: useful invalidation and multi-target qualification** — W17–W19. Contract-only body edits stop unnecessary downstream compilation where safe; compiler/definer consumers keep stronger dependencies. ARM64 and qualified x86-64 evidence remain separate.

**M5: optional throughput work** — W20, prefix checkpoints or backend-only caches only when phase measurements justify them. None is prerequisite for the immutable contribution model or a fresh-process hit.

### 23.2 Proposed file organization

Keep package artifact algorithms outside architecture directories; backend fixup logic stays with its backend. Reuse existing source/transaction/checker owners rather than duplicating them.

```text
src/compiler/package/
    identity.f          stable logical identities and local handles
    dependencies.f      observations, witnesses, classification
    contribution.f      owned semantic data model
    capture.f           compiler-owner contribution collection
    artifact.f          package AOT-profile adapter
    import.f            coordinated live install
    link.f              target-independent layout/reference plan

src/arch/<target>/
    package-reloc.f     target-owned validation/patch planning

tools/package-build/
    plan.f              effective manifests and ordered occurrences
    cache.f             action/candidate/blob resolution
    generation.f        complete output generation and selection
    report.f            phase counters and explain reasons

build/package-units.json
test/package-build/
    ... focused and real-load acceptance suites ...
docs/package-build.md
```

Existing reusable functions stay in their current owning files. The module split is a responsibility map; combine adjacent files when doing so avoids needless API forwarding, but do not merge checker authority into generic cache code.

## 24. Proof obligations and limits

These are implementation obligations, not claimed completed formal proofs.

### 24.1 Substitution property

Let S be the ordinary semantic compiler state at a contribution boundary, I the frozen inputs, and `compile(U,S,I)` the checked source operation. Let A be the artifact captured by that operation. For an admitted artifact and current state S', if all recorded observations, entry/boundary preconditions, producer/profile requirements and placement constraints match, then:

```text
observable(install(A,S',I)) = observable(compile(U,S',I))
```

"Observable" includes subsequent name resolution, fresh checking, constants/compile-time behavior, persistent data relationships, initialization effects and final program output—not just the bytes of U's own functions.

The argument requires complete dependency/effect coverage and correct owner adapters. It does not follow from SHA-256 or a valid stack signature alone.

### 24.2 Composition property

Apply the substitution property in original source order. Each successful import establishes the same next boundary state as source compilation. Therefore later units observe the same state, and deterministic final layout produces the same program as cache-off compilation. An opaque unit invalidates the substitution premise unless its effects are represented, which is why it introduces a barrier rather than a guessed cache key.

### 24.3 Failure property

For any rejection before coordinated commit, all observable live owner state equals the pre-import savepoint. Reservations/scratch may be released, but no callable partial definition or changed persistent reference remains. If restoring this property fails, the session is poisoned and cannot produce a new selected output.

### 24.4 Relocation property

Each patch changes only its declared component bytes inside its owner, and computes the target according to a backend-validated reference/ABI contract. Staged patch plans are pure with respect to public state. Applying live and final placement plans to the same canonical input is repeatable and does not accumulate offsets.

### 24.5 Known engineering risks and controls

| Risk | Required control |
|---|---|
| An indirect compile-time read bypasses dependency tracking | Restricted admission profiles, audited adapters, exact producer keys, mutation tests and opaque barriers. |
| Most runtime packages are not initially closed | Coverage inventory and dedicated adapter work; no speed completion claim with a large uncached tail. |
| Native relocation metadata omits typed IDs or interior references | Capture at semantic emission/publication; coverage tests; placement-sensitive fallback. |
| Owner rollback is incomplete | Failure injection at every stage, state digests, poison-on-failure and external generation isolation. |
| Cache-on and cache-off share a bug | Independent source-bound differential and behavior/negative-checker tests. |
| Fine invalidation forgets transitive compile execution | Execution-closure dependency promotion and D06. |
| Builder invalidates on all target edits | Separate actual host-tool inputs from target contribution inputs. |
| Package serialization dominates new warm path | Indexed binary records, shared provider maps, whole-invocation phase evidence. |
| Output selector semantics break existing map readers | Migrate readers to one-time generation resolution; compatibility aliases carry validation, not false atomicity. |

## 25. Implementation order

Start by landing the owned-input ordinary-entry integration and the NBR E2E test that forbids source evaluation on a hit. In parallel, extend sealed emissions with authenticated symbolic target records and implement owner-local transactional relocation tests. Next add checker/type partial export and the coordinated importer, then store the first real package artifact.

Keep the saved builder qualification moving alongside those owners; do not wait for every package before removing repeated writer/tool compilation. However, the first meaningful edited-selfbuild release also needs the bootstrap bundle and the large runtime package groups, not only the saved writer and NBR.

The decisive release receipt should show: a fresh ordinary invocation; an early independent semantic edit; the exact producer and frozen input view; which units compiled and which imported; zero evaluated definitions for hits; correct changed behavior; fresh invalid-client rejection; identical engine and `.names` versus the cache-off run; safe generation publication; and externally measured elapsed time.

That receipt demonstrates package incrementality. A warm whole-application cache, a saved prefix, or a lower internal compiler timer does not.

## 26. Evidence ledger

### Habu source

All repository paths refer to `37b2c1b75773fc269682f1869f16107660671fd6` unless explicitly stated. These sources ground existing behavior and extension points; the proposed APIs/profile above are new design.

- **H1:** `.dots/habu-make-edited-native-59200861/habu-make-edited-native-59200861.md` — recorded native timing, builder/package direction and retained experiments.
- **H2:** `tools/hb-build-lib.f`, `tools/hb-build-core.f` (removed on master by 2dcfccf1), `tools/hb-build.f` — whole-closure object/executable cache, producer keys, source rereading, cache lookup and timing boundaries.
- **H3:** `src/compiler/native/emission.f` — sealed emission rows, absolute call targets and lifetime ending at clear.
- **H4:** `src/compiler/native/publish.f` — single pending-definition publication, external-region call handling, exact spans, checker facts and engine authority boundaries.
- **H5:** `src/compiler/native/branch.f` — real NBR contents, private helper, public wrappers, constants and protected wordlists.
- **H6:** `src/core/include.f` — source composition, separate require state, retained/target loader-local source provider pairs.
- **H7:** `tools/native-source-view.f`; incremental input-ownership task — owned bytes, negative resolution answers, frozen lookup and integration requirement.
- **H8:** `src/habu/aot-decl.f` verified-graph payload; `src/habu/aot-file.f` signature remapping; `src/core/type-family.f` nominal graph-ID identity requirements; `PLAN.md` verified graph preservation contract. This design does not claim every current graph-owner routine was exhaustively audited.
- **H9:** `src/compiler/native/dict.f` — source-visible resolution order, ambiguity, trusted visibility, constant/address classification and evaluation.
- **H10:** `src/habu/aot-file.f`; NBR capture/import tasks — existing image artifact envelope and staging merge, need for unstripped contribution and live import.
- **H11:** `lib/object.f`, `lib/object-link.f`, `tools/object-image.f` — old codec/linker limits, repeated scans, owner-boundary and transactional patch issues.
- **H12:** `src/core/declaration-transaction.f` — ordered reversible transactions, closed participant table, savepoint lifetime, total release and poison policy.
- **H13:** `tools/native-build-core.f`, `tools/native-build.f`, `tools/native-builder-image.f`, `test/native-builder-image-e2e.f` — host/target transitions, typed writer/owned capture, explicit saved builder and its acceptance test.

### External primary references

- **E1:** Rust Compiler Development Guide, “Incremental compilation in detail,” https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html — stable cross-session IDs, dependency/result fingerprints and persistence concerns. Used as a conceptual precedent, not a new Habu dependency.
- **E2:** Bazel documentation, “Remote Caching,” https://bazel.build/remote/caching — action result metadata separated from content-addressed output storage and explicit action inputs.
- **E3:** Bazel documentation, “Sandboxing,” https://bazel.build/docs/sandboxing — undeclared dependencies undermine reuse. Filesystem sandboxing alone does not account for Habu's in-process compiler state.

External references were consulted on 2026-09-30. All performance budgets in this specification are proposed acceptance targets; no new native execution results are asserted.
