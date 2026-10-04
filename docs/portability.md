# Portability architecture

Adopted 2026-10-03; reviewed against master 67a66d28.

How Habu builds programs, and Habu itself, for more than one target: five
qualified native compiler-host products, reusable ISA backends, separately
selected ABI, runtime, image and toolchain profiles, package contributions
installed in source order, and a Wasm path bound to Browser Runtime v2. The
compiler's execution environment never silently defines the program it builds.

This page specifies contracts and a migration plan. It claims no compiler
implementation, browser qualification, TI board test or native self-build.
Wasm code generation and the browser binding (sections 17 to 21) live in
[wasm-backend.md](wasm-backend.md); the package contribution design this page
builds on is [package-build.md](package-build.md). The adoption review checked
every claim about master against 67a66d28; the commits and file:line references
below are master's.

## 0. Failures the design closes

Each row names a failure an ambiguous portability design allows and the
contract that closes it.

| Finding | Failure an ambiguous design allows | Contract that closes it |
|---|---|---|
| F01 — build/host/target roles | Building a cross-compiler could select the output backend while compiling the compiler itself | Two explicit actions and separate product host and enabled-emitter set; §2 |
| F02 — target identity versus compatibility | Either unsafe object mixing or rejection of otherwise compatible objects | Separate reproducibility, link-ABI, runtime, and execution predicates; §4 |
| F03 — one “target-dependence barrier” | Target-dependent source selection could contaminate supposedly portable HIR | Dependencies recorded from source evaluation onward; §6 |
| F04 — ambient compiler state | An ARM compilation, nested x86 compilation, or failed Wasm compilation could overwrite another's scratch state | Session-owned contexts and artifact ownership; §5 |
| F05 — target objects described only as IDs | Existing `create`, `here`, `,`, `!`, `does>` and immediate behavior had no realizable cross-compile path | Typed target references, mapped build-time views, owner adapters and explicit refusal cases; §7 |
| F06 — host code versus target code | A target-only import could be called during compilation | Separate callable implementations and execution grants; §§7–8 |
| F07 — package-order semantics | Generic dependency sorting could expose future declarations or rerun effects on cache hits | Original-position contribution installation from the existing package design; §8 |
| F08 — producer identity | Different host binaries or source-installed checker owners could reuse incompatible cache results | Effective producer vector and conservative reuse policy; §23 |
| F09 — 64-bit cells / 32-bit pointers | Packed foreign pointers, guest pointers and Habu pointer slots could be confused | Explicit storage domains and checked conversion; §9 |
| F10 — arithmetic equivalence | Wasm traps, C helper behavior, NaN differences or FMA could change Habu semantics | Operation-level legalization and numeric-policy gates; §10 |
| F11 — generic pipeline order | C66x spills could introduce unscheduled hazards; Wasm could inherit fake registers | Backend-owned pass graphs and terminal validators; §11 |
| F12 — emission versus object format | A function buffer could be mistaken for a finished object or executable | Distinct emission, contribution, link-plan and packaged-artifact types; §12 |
| F13 — relocation underspecification | Wrong PC bias, truncated pointers, broken paired relocations, or changing LEB lengths | Relocation expressions and monotone layout/encoding; §13 |
| F14 — image/OS conflation | ELF support could be overstated as Linux or firmware support | Separate link formats, execution policy, startup and deployment; §§13–16 |
| F15 — native publication | Executable bytes could become visible before permissions/unwind/cache work completed | Prepare/install/commit/retire protocol; §14 |
| F16 — snapshots and stripping | Host helpers, process handles or necessary reflection roots could leak/disappear | Explicit roots and separate live, image and portable-state profiles; §§8, 14, 22 |
| F17 — host OS abstraction | Native page size, library search, paths or CPU detection could affect foreign output | Host service interface and pinned target environment; §15 |
| F18 — freestanding completeness | Firmware might link but lack valid startup, load/run addresses, interrupt or DMA rules | Board/BSP contract and artifact-specific gates; §16 |
| F19 — Windows ABI completeness | Internal register conventions could escape into C/COM, or exceptions cross foreign frames | ABI call plans, entry thunks, unwind and callback ownership; §15 |
| F20 — Wasm linking | Native byte patching could corrupt function/type indices and variable-length encodings | Symbolic Wasm contribution plans and final index assignment; §17 |
| F21 — actual HBR2 surface | A new collection of imports could replace the existing v2 protocol | Exact two imports, six exports, control record and packet framing; §18 |
| F22 — three result domains | Scheduler outcomes could be treated as Habu throws or submission ownership | Separate language status, control-record result and host submission codes; §18 |
| F23 — browser admission | Correct Wasm could bypass v2 effect, cost or capability restrictions | Artifact inspection plus callback certificates and host grants; §§19–20 |
| F24 — publication “rollback” | Candidate initialization could mutate shared memory or trigger irreversible browser work | No start/active shared segments; declarative initialization; single root commit; §21 |
| F25 — code lifetime | Redefinition could invalidate callbacks, suspended jobs or old execution tokens | Definition generations and code-generation leases; §§14, 21 |
| F26 — browser release construction | Wasm, generic host, shader layouts and schema registry could come from different releases | ReleaseManifest integration and deterministic asset graph; §20 |
| F27 — build-cache invalidation | Negative lookups, compile-time transitive calls and undeclared effects could be missed | Observation classes, closure dependencies and source-only fallback; §23 |
| F28 — self-build circularity | Producer hashes embedded in outputs could prevent convergence | Content/provenance separation and explicit B0/B1/B2 receipts; §22 |
| F29 — capability/qualification claims | “Backend available” could imply linking, deployment or execution support | Independent stage capability and evidence records; §§3, 24 |
| F30 — migration ordering | New portability work could block or overwrite the Intel lane's work | Additive interfaces, per-lane ownership and concrete exit gates; §§26–27 |

Browser Runtime v2 (HBR2) replaces the old browser runtime and protocol v1. Its
main body and its generated registry are normative; it is a specification and
reference model, not a qualified browser implementation. [B2 §§0–1, 24–28](browser-runtime.md#0-reading-and-precedence)

### 0.1 Precedence

Existing Habu language semantics and checker ownership rules remain
authoritative. This design changes compiler architecture and adds target
profiles; it does not change arithmetic, quotation capture, package visibility
or `include`/`require` behavior. Three HIR contracts in particular bind every
target, Wasm included:

- Canonical NaN: a NaN made from numbers is `$7FF8000000000000` on every
  target, and a quiet NaN operand passes through, the left of two
  (`src/compiler/native/hir-word.f:1269-1272`).
- Integer arithmetic: `+`, `-` and `*` wrap; `/`, `mod` and `/mod` throw
  `E-DIV-ZERO` (-6400) on a zero divisor (`src/habu/arith-abi.f:4-9`, `:29`).
- Counted loops: `?do … loop` enters only while start < limit, signed;
  `?do … +loop` skips only equal bounds (`docs/forth.md:939-955`).

The precedence for this work is:

1. Existing language and primitive contracts, for language behavior.
2. Browser Runtime v2, for browser semantics, HBR2 wire fields and the wrapper
   ABI.
3. The package incremental-build design ([package-build.md](package-build.md)),
   for contribution boundaries, source-order installation and the AOT
   package-profile family.
4. This page, for portability composition, context ownership, cross-target
   data, backend results and integration.
5. [wasm-backend.md](wasm-backend.md), for Wasm-specific details this page does
   not refine.

A genuine conflict is an explicit integration change with new versioning and
tests, not an implementer's local choice. No v1 browser compatibility is
promised. [B1; B2 §0; P1 §§1, 4, 10]

The Windows/COM design [WN] this design cites does not exist yet and is open
future work. The HBR2 registry exists as `lib/browser/hbr-v2-registry.json`,
generated from HBR2's tables and pinned by its own digest, but no build imports
it yet ([wasm-backend.md](wasm-backend.md) §12.1). Until they land, the sections
that defer to them (§15.4 here, and the registry parts of §18.1 and §20.4 in
[wasm-backend.md](wasm-backend.md) §12.1 and §11.1)
state requirements, not bindings.

### 0.2 Notation and evidence

Record definitions and algorithms below are implementation pseudocode, not
compilable Habu. Implement them with Habu's admitted nominal types, checked
buffers, records and owner APIs. Public linear objects use opaque one-cell
handles where that avoids relying on unqualified multi-cell linear storage. This
design authorizes no new `TRUSTED:` escape hatch.

A cited source establishes an existing fact or external ABI rule. New records,
algorithms and paths are design decisions. §29 lists the sources. The numeric
wire fields in [wasm-backend.md](wasm-backend.md)'s HBR2 binding come from the
HBR2 text; they are not new portability conventions.

## 1. Required scope and support claims

### 1.1 Compiler execution platforms

The five required native Habu compiler hosts are:

| Host identifier, proposed canonical spelling | OS | CPU |
|---|---|---|
| `aarch64-apple-darwin` | macOS | ARM64 |
| `aarch64-unknown-linux-gnu` | Linux | ARM64 |
| `x86_64-unknown-linux-gnu` | Linux | x86-64 |
| `aarch64-pc-windows-msvc` | Windows | ARM64 |
| `x86_64-pc-windows-msvc` | Windows | x86-64 |

These are **required configurations**, not assertions that all five currently work. The two Windows hosts stay "required, unqualified" until a Windows lane exists (P9 is parked). Linux libc policy must be explicit: the table uses GNU profiles as defaults, while musl and syscall-only runtimes are separately resolved variants. A target label is a front-door alias, never the whole semantic target definition.

There is no required macOS/x86-64 host in this scope. TI C66x and Cortex-R4F/R5F are target-only, not compiler hosts. Browser-hosted Habu is an additional independently gated compiler profile, not a prerequisite of ordinary Wasm application compilation.

### 1.2 Output profiles

Every native host should be able to **generate** every implemented output profile without executing that output. Initial profile families are native hosted ARM64/x86-64; ARM32 and C66x freestanding; core Wasm memory32; and browser applications using HBR2. Native objects, executables, static/shared libraries, firmware and browser releases are separate output kinds.

Support is reported as `Declared`, `CodegenTested`, `ObjectTested`, `Linked`, `Executed`, and `Qualified(profile, environment, evidence)`, with independent fields rather than an assumed linear progression. A code-generation-only backend may be useful. It must not be advertised as a working compiler host, bootable board image, or COM server.

### 1.3 Non-goals that do not block the architecture

No binary backend-plugin ABI, LLVM/Emscripten dependency, TI self-hosting, transparent Promise suspension, universal browser support, automatic translation of Wasm to GPU shaders, or universal native-object interoperability is required. WGSL/WebGL shaders remain runtime/application assets under the existing ownership split. ARM64EC, memory64, Wasm GC, threads, additional WASI versions, unusual foreign calling conventions and additional DSPs are explicit profiles with full gates, not aliases that silently activate.

## 2. Build platform, compiler product and output target

P1b resolves named profiles into fixed `RTARGET:resolved-target` values and captures the executing process profile before a native target window opens. `BUILD-TARGET:WITH` scopes a `build-action` around the common native build entry and restores pending selection on return or throw. Compiler product composition records enabled emitter families separately from the executable target; it does not assert backend availability. The action graph below describes later expansion beyond this bounded native build adapter.

```text
ExecutionPlatform { os, arch, process_abi, environment_id }
CompilerProduct {
    executable_target: ResolvedTarget,
    enabled_emitters: [BackendId],
    default_output_profile: TargetProfileId,
    supported_build_features: FeatureSet
}
ApplicationProduct {
    output_target: ResolvedTarget,
    entry_profile, package_roots, output_kind
}
BuildAction {
    execution_platform, effective_producer, input_world,
    product: CompilerProduct | ApplicationProduct | GeneratorProduct,
    compile_options, publication_policy
}
```

`execution_platform` describes the executing **process ABI**, not just the physical machine. A native x86 process under an emulator is not an ARM compiler merely because hardware is ARM. Detect the running binary's ABI from its embedded host descriptor; use OS/CPU probing only to validate actual execution capabilities.

### 2.1 Ordinary application cross-build

An ARM64/macOS `hb` building a Linux/x86-64 application executes compile-time helpers on ARM64/macOS, emits application bodies for Linux/x86-64, resolves Linux target libraries, writes ELF under the Linux image policy, and does not run the ELF unless an explicit runner is selected.

### 2.2 Cross-building a cross-compiler

Suppose macOS/ARM64 builds a Windows/ARM64 Habu compiler that can emit C66x and Wasm:

```text
Action A, runs on macOS/ARM64:
  compile compiler source -> Windows/ARM64 machine code
  include emitter implementations {ARM64, x86-64, ARM32, C66x, Wasm}
  compile every included emitter implementation FOR Windows/ARM64
  embed default output profile Windows/ARM64

Later Action B, runs the produced compiler on Windows/ARM64:
  execute that C66x emitter implementation on Windows/ARM64
  produce C66x application code
```

An emitter's implementation target and the instructions that emitter writes are distinct. Code that constructs a C66x instruction is ordinary Habu code compiled for the compiler product's host. The currently running compiler still supplies host-callable implementations for Action A's own compile-time work. Product-host definitions must never replace those implementations halfway through Action A by ambient source selection.

`enabled_emitters` belongs to compiler product composition. It is not an application runtime dependency: compiling a browser app with five enabled backends does not embed those five backends in the app.

### 2.3 Execution is a separate action

`compile`, `link`, `package`, `run`, `test`, `flash`, and `sign` are distinct nodes. Only `run/test/flash` require a runner. `sign` may need an authorized external finalizer. Cross-compilation never tests executability by trying to launch an unknown output through a host process API.

A `RunnerSpec` describes accepted image profiles, target environment, transport, timeout, cleanup and evidence capture. It is not part of a target object's code identity unless runner-related instrumentation changes the code. Runner and device identity remain part of execution evidence.

## 3. Resolved target and profile resolution

### 3.1 Record ownership

```text
ResolvedTarget {
    machine: MachineSpec,
    layout: DataLayoutSpec,
    habu_abi: HabuAbiSpec,
    foreign_abis: [ForeignAbiSpec],
    environment: EnvironmentSpec,
    runtime: RuntimeComposition,
    image: ImageSpec,
    required_services: ServiceRequirementSet,
    effective_features: CanonicalFeatureSet
}

MachineSpec {
    architecture, instruction_state, cpu,
    enabled_isa_features, tuning_profile,
    code_endian, data_endian, address_spaces
}
EnvironmentSpec {
    kind: Darwin | Linux | Windows | Freestanding | Browser | Wasi,
    deployment_floor, libc_or_service_profile, platform_constraints
}
```

Wasm is one backend family. Memory32 versus memory64 is an address-model/profile distinction; browser versus WASI is an embedding distinction. JSPI is an embedding feature, not a machine instruction feature. C66x CPU rules, ARM32 instruction state, and x86 CPU feature sets belong in `MachineSpec`; DOM availability does not.

`HabuAbiSpec` owns internal calls, stack/value conventions, runtime context and execution-token representation. `ForeignAbiSpec` owns C/platform/native-entry marshalling. `ImageSpec` owns output format and image layout. Do not let one overloaded `abi` field decide all three.

### 3.2 Data layout is generated, not guessed

```text
DataLayoutSpec {
    habu_cell: { bits:64, storage_bytes:8, alignment },
    habu_pointer_slots: [{space, storage_bytes, canonicalization}],
    scalar_storage: [ScalarType -> {size, alignment, endian}],
    foreign_models: [AbiId -> ForeignLayoutRules],
    address_spaces: [{id, address_bits, index_bits, unit_bits,
                     null_rule, code_or_data, access_constraints}],
    aggregate_rules, layout_schema
}
```

`address_bits`, `index_bits`, and storage width are not interchangeable. A memory32 region can have an exclusive bound of 2^32 even though its last address is 2^32−1. Use a wide extent type for layout calculations. Address-space unit size is explicit; current requested profiles use byte addressing, but the shared object model must not hard-code that all future DSPs do.

### 3.3 Profile resolver algorithm

The command interface accepts a target profile plus explicit overrides. Precedence is command-line override, project configuration, selected profile defaults, then the compiler's configured native default **only when no output target was requested**. Conflicts are errors, not silent last-writer behavior between unrelated descriptors.

Resolution performs:

1. Parse canonical names and documented aliases. Reject unknown spellings with suggestions that do not change the chosen target automatically.
2. Load immutable profile and dependency records from pinned inputs.
3. Expand CPU feature implications and check incompatible/required feature combinations.
4. Derive data layout, Habu ABI and platform constraints. Validate architecture/state/endian/ABI coherence.
5. Resolve runtime services and exact foreign libraries for the target, not for the host.
6. Resolve output-kind/linker/finalizer capabilities and host executability of external tools.
7. Select backend provider and produce an implementation support report for this build mode.
8. Seal descriptors, ordered input records and digests before target-dependent compilation.

A structurally coherent target may be unsupported by the installed backend. These errors remain distinct. Object-only compilation must not require a final linker, signing credential, browser, or board runner.

### 3.4 Capability intersection

Do not replace the four-row backend registry (`BACKEND-ROWS`, `src/compiler/target.f:441`) with a large collection of optimistic booleans. Resolution asks stage-specific implementations about the exact tuple:

```text
(machine, layout, HabuAbi, foreignABI, runtime, imageKind, options)
```

A diagnostic includes the stage, unsatisfied requirement, responsible provider and dependency path. Examples: `foreignABI=win64` unsupported by call lowering; `TLS=dynamic` unsupported by image builder; `Graphics` not admitted by the browser release; required C66x helper library absent; external linker cannot execute on this host.

## 4. Identity, compatibility and feature versioning

### 4.1 Four different questions

`SameBuildIdentity` asks whether an action has identical declared inputs and code-generation policy. `LinkCompatible` asks whether two objects can coexist. `RuntimeAdmissible` asks whether an image fits a runtime/embedding's actual requirements and grants. `ExecutableHere` asks whether a runner can execute it.

P1b exposes `RTARGET:SAME-BUILD-IDENTITY?` over a resolved target and the existing action-input digest, `LINK-COMPATIBLE?` over declared profile ABI/layout/runtime facts, `RUNTIME-ADMISSIBLE?` over selected semantic features, and `EXECUTABLE-HERE?` over the captured process ABI/image/runtime. Contribution, relocation and runner checks remain at their later owners.

These must not be one digest equality test. Objects built with different optimization levels can link when their call/data contracts agree. Objects for the same ISA can still be incompatible because of foreign ABI, float ABI, pointer layout, TLS or internal Habu ABI. A CPU with more features can execute an object requiring fewer features; that does not authorize changing code-generation features during a supposedly reproducible build.

### 4.2 Canonical identity rules

Use the existing domain-separated SHA-256 facilities and package encoding conventions. New administrative encodings use explicitly ordered tagged fields, length-prefixed bytes and checked LE u64 counts, not native struct dumps or delimiter-free concatenation. Sets sort by stable wire ID; sequences preserve semantic order. Include schema versions. Never hash host pointers, randomized interner IDs, hash-table iteration order, transient registry slot numbers or final code placement into unplaced content identity. [R1; P1 §§6, 10]

Keep existing CTARGET wire meanings and native digests readable under their existing schema. Add a composite target descriptor around the old core contract during migration; bump the composite schema when adding new semantic fields. Do not make an old digest mean a newly widened ABI. Cache namespaces move with the relevant schema.

### 4.3 Compatibility descriptor

Every target contribution carries `CompatibilityRequirements` with machine family/state, endian, address/layout ABI, Habu ABI, foreign-call contracts actually used, relocation schema, runtime interfaces actually referenced, and required features. The linker checks these per object, unions compatible requirements, and rejects conflicts. A backend's encoding support is not a promise that every ABI named by the architecture works.

A runtime requirement is an interface/version constraint; a release pin is an exact content identity. Ordinary Habu linking can accept a compatible replacement runtime implementation, while a reproducible release pins the exact implementation. HBR2 is stricter at the protocol boundary: its selected canonical registry digest and feature schemas must match the release. A label containing “v2” alone is insufficient. [B2 §24.1](browser-runtime.md#241-new-protocol-identity)

### 4.4 Existing feature repairs

Separate scalar floating-point availability from fused multiply-add. Preserve old feature wire values and give any old stronger feature its documented implication into the new scalar capability query. Update consumers that currently demand the stronger feature for ordinary FP. Unsupported fused semantics require a qualified helper or a diagnostic, never an unfused multiply followed by add. Also separate hardware atomics from runtime thread support, CPU tuning from legal instructions, and optional browser features from Wasm opcodes. [B1 §4; R1]

The sites that read `CTARGET:F-FP` as the one floating-point capability on master are `src/compiler/native/hir.f:208-213` (`FP-TARGET`), `src/compiler/ir/type.f:329-335` (`FMT-FEATURE`), `src/compiler/binding.f:50-53` (`FMA-CK`, the fused-contraction check), `src/compiler/ir/schema.f:903-913` (the schema's feature checks) and the ABI contract builders at `src/compiler/native/abi.f:56`, `src/arch/x86-64/abi.f:64` and `src/compiler/native/a64ir.f:197`.

## 5. Compiler sessions, owner state and reentrancy

Adding immutable descriptors does not make a compiler with mutable global scratch reentrant. `NCOMP` (`src/compiler/native/compiler.f`) stores compilation state in module variables and single-row buffers; `NEMIT` (`src/compiler/native/emission.f`) and `NSHADOW` (`src/compiler/native/shadow.f`) likewise have ambient mutable state. The existing nested shadow path is a controlled special case, not a general multi-session guarantee. [R2; R3; R4]

### 5.1 Session model

```text
CompilerSession {
    execution_context: HostExecutionContext,
    input_world: OwnedInputWorld,
    target: ResolvedTarget,
    producer: EffectiveProducerVector,
    declaration_owner, checker_owner, source_cursor,
    host_callable_dictionary, target_symbol_graph,
    contribution_transaction,
    backend_instances: map<BackendId, BackendSession>,
    diagnostics, limits, cancellation, publication_policy
}
BackendSession {
    target, machine_context, owned_arenas,
    function_work_state, emission_builder,
    temporary_symbols, pass_metrics
}
```

Immutable opcode schemas and read-only backend descriptions can be shared. Pass scratch, pending definitions, label allocators, literal maps, buffers, private checker state, dynamic registry deltas and emission results belong to the appropriate session/transaction.

Source-language globals used by compile-time helpers belong to a host execution realm associated with the session. They are not made thread-safe merely by moving compiler buffers. Initially serialize execution within a realm; parallelize independent builds in isolated processes. Later in-process parallelism requires audited isolation for the complete callable closure, not only the backend.

### 5.2 Handle ownership and lifecycle

Use nominal handles for `Session`, `Target`, `ContributionBuilder`, `CheckedModule`, `Emission`, `PreparedPublication`, and `CodeLease`. Their lifetimes are:

```text
session: Created -> Configured -> Building -> Finalizing -> Closed
                                      \-> Failing -> Closed
emission: Building -> Sealed -> Consumed/Released
publication: Prepared -> InstalledPrivate -> Committed -> Retired
                         \-> Aborted
```

A sealed emission owns its bytes/rows; it cannot borrow a backend scratch buffer that `RETIRE` clears. Native emission adaptation initially copies existing NEMIT bytes and all metadata before retirement. Later the backend transfers ownership of its buffer directly.

Cleanup must be safe after partial initialization, cancellation or exceptions. It cannot allocate in the emergency error path. A failed target compilation must restore the enclosing host callable owner, source cursor, selected target and arena marks exactly. A scope token enforces LIFO restoration where legacy APIs still require dynamic binding.

### 5.3 Transitional adapters

Do not switch `HB-TARGET-*` meanings globally. Preserve legacy predicates for the engine/source window that owns them, while new code receives explicit `HostContext` or `TargetContext`. Prohibit new portable callers of the legacy predicates with a dependency/lint rule. Move each source selector individually behind a resolver adapter, retaining native fixtures.

Master seals every package the native capture ships (bbcac900; `docs/forth.md:198-205`). Every edit to `CTARGET`, `CBIND`, `HIR`, `NBACK`, `NELAB` or `NCOMP` is therefore an engine rebuild under the full gate (the engine requires the compiler at `src/habu/native-runtime.f:114`; [gate.md](gate.md)), and a backend loaded at run time uses public words only. An adapter cannot be loaded into a sealed package on a product engine. All the sealed edits the Wasm path needs go into one commit, P1a (§27), developed on the whitebox image.

A transitional non-reentrant backend is marked `ExclusiveSession`. The driver takes an explicit lease, saves only documented state, and prevents unsupported nested entry. This is preferable to claiming reentrancy before globals have been removed.

### 5.4 Required tests

Compile A64, then x86, then A64 in the same long-lived process; compare both A64 artifacts. Repeat with failure midway through x86 emission, nested host helper compilation, different numeric policies, and two independently owned sessions. Once true parallel sessions are advertised, race those tests and inject allocation failures. Passing the sequential tests does not qualify parallel execution.

## 6. Frontend phases and dependency-sensitive reuse

A single target-dependence barrier is too strong. Source conditionals, type definitions, foreign layouts and immediates can observe target facts **before** HIR exists. The implementation uses a dependency-aware frontend, not an assumption of universal target-free HIR.

```text
owned source + source-order environment
    -> resolution and compile-time execution
    -> checker-owned declarations, effects and observed facts
    -> semantic HIR with logical references
    -> target-layout/ABI legalization
    -> backend-specific representations
```

### 6.1 Reuse classes

A parse tree can be shared when tokenization/source-policy inputs agree. A resolved definition can be shared only when its binding observations and compile-time inputs agree. A checked semantic module can be shared only when all target facts it consumed agree. Selected code requires its full code-generation identity.

Represent each consumed fact as `(owner, query-kind, logical-key, returned-value-digest)`. Target queries include language layout, foreign layout, target feature, platform condition, primitive effect variant and internal ABI rule. A missing observation hook makes the contribution ineligible for the more precise reuse class.

### 6.2 Shared-HIR dual emission

Retain the existing frozen-HIR fan-out when host and output-target versions genuinely share the observed semantic facts. When they differ, elaborate each required semantic variant from the same owned source at the same source-order environment. Do not re-resolve it against a later, mutated dictionary.

A target-only constant derived from `target.sizeof(T)` is not automatically suitable for executing a host helper that treats that constant as its own native structure size. The helper must either operate through the explicit target-data API or use a separately elaborated host-layout variant. This distinction is enforced at the helper's effect/layout boundary.

### 6.3 Common semantic operations

The shared representation must preserve ordered value lanes, nominal/linear ownership, memory effects, exceptional edges, call effects, tail-call intent, source spans and logical identities. Do not lower pointers into indistinguishable integers before provenance-dependent target construction completes.

Memory operations carry width, alignment, address space, signed extension and volatility/ordering. Calls name a logical function and call-contract identity. Inline assembly and machine primitives are explicitly target-specific leaves with declared clobbers/effects; they cannot enter a supposedly portable helper closure without an implementation for the execution target.

The frontend remains the owner of Forth loop and stack semantics. Backend code must not recreate `+LOOP`, `LEAVE`, wide locals or linear duplication rules from mnemonic intuition. [B1 §§5–8]

On master, `NBACK:FREEZE` folds the definition's loops (`NLOOP`) before any selector runs, and every selector of the definition binds that one folded module (`src/compiler/native/backend.f:27-35`, `:159-185`). A new selector, Wasm's included, reads folded HIR. The `?do` entry rule is the frontend's (§0.1, `docs/forth.md:939-955`); P6 pins its three rows (`-1 0 ?do`, `MIN-N 0 ?do` and a counting-down `+loop`, §25.5).

## 7. Compile-time execution and logical target data

This is the largest structural change. It must work for native cross-compilers, address32 targets and browser-hosted compilation without replacing Habu's language with an unrelated evaluator.

### 7.1 Distinguish the callable implementation from the declared word

```text
DefinitionIdentity = {package_owner, logical_symbol, generation}
CallableBinding {
    definition_identity,
    checked_effect,
    host_implementation?,
    target_implementation?,
    compile_time_admission,
    implementation_dependencies
}
```

Use the existing dictionary/checker owners to issue identities and generations. Do not add a second name resolver. A host-callable body can execute during compilation; a target-only body cannot. A declaration may have both bodies, but their raw code addresses are never their shared identity.

A build-time call records the actual implementation closure it executes, including deferred/indirect targets and source-installed helper owners. A plain runtime call usually needs only the callee's stable contract for checking/linking; once executed at compile time, implementation changes become dependencies. [P1 §7.6]

### 7.2 Target reference algebra

```text
TargetRef = Null(space)
          | Object(object_id, byte_addend)
          | Function(definition_id, entry_kind)
          | ImportedSymbol(import_id, byte_addend)
          | ApprovedAbsolute(space, unsigned_value, board_contract)
          | RuntimeSlot(slot_identity)
```

Approved absolute addresses are necessary for MMIO and boot vectors; they require an explicit target/board declaration. A process pointer is never auto-converted into that variant. Function state, such as a Thumb entry tag, belongs to the function-address lowering rule and is not blindly applied to object pointers.

`HostPointer`, `TargetRef`, `TargetAddress`, `GuestOffset`, `BrowserHandle`, and `ExecutionToken` are distinct roles. Offset arithmetic remains symbolic until placement. Arithmetic that loses provenance cannot be recovered by scanning integers for pointer-looking bit patterns.

### 7.3 Target objects

```text
TargetObject {
    id, owner_contribution, schema, address_space,
    size:u64, alignment:u64, storage_class,
    initialized_ranges, byte_storage,
    reference_slots: [{offset, width, encoding, TargetRef}],
    identity_observable, mutable_at_runtime, retention_policy
}
```

Storage class is static read-only, static mutable, zero-fill, TLS, runtime-created, or board-defined no-init. A reference slot has a declared target encoding, so an eight-byte Habu pointer slot and a four-byte foreign pointer field are distinguishable. Reject overlapping incompatible writes/reference slots. All padding is initialized canonically where it is part of stored artifact data.

### 7.4 Realizable `create` / `here` / stores

The target-data builder offers checked operations to allocate, align, append scalar/bytes/reference, read/write a typed field, and obtain a symbolic cursor. `here` in an admitted target-construction scope yields that cursor, not the current host heap address. `allot` changes the logical target extent with checked bounds. `,` stores a language cell under the target layout; a pointer-valued cell records a reference slot instead of copying native bits.

There are two implementation paths, with explicit admission:

**Owner-instrumented construction.** The checker/elaborator recognizes target-building operations and routes them through the builder while the helper executes on the host. This is the preferred portable path.

**Audited legacy owner adapter.** Existing declarers may operate on host-accessible staging storage. The adapter maps a bounded object into a host view, records every typed reference store and target-endian scalar access, then seals that object. It may not expose an arbitrary raw pointer to an unaudited helper and still claim complete tracking. Copying a pointer-bearing region requires copying its reference metadata, not just its bytes.

Master already has this adapter for native cross-builds. `NSHADOW` compiles each definition a second time for the target from the HIR module the engine's selector bound, and keeps the result as a sealed unplaced emission whose every call, branch and address is a row (`src/compiler/native/shadow.f:1-30`). The capture reads that map (`src/habu/aot-shadow.f`): it resolves every host address through the xt -> record index and refuses an unresolved one by name. The x86-64 linker then lays the records out at fixed addresses (`src/habu/link-x64.f`). P6 rides this adapter; §7.2's `TargetRef` replaces it later.

A read/write view is a temporary host implementation detail associated with an object ID. It is invalid after unmap, growth or seal and cannot be stored in target data. General raw native stores that bypass tracking make a cross-target contribution unsupported or opaque under the declared mode. Native legacy builds can retain their existing behavior; the cross compiler reports the exact unsupported operation instead of silently changing its meaning.

### 7.5 Definers, execution tokens and generated code

`does>` creates the same checked definition/companion relationships as the existing language, represented by logical function entries plus the created object's reference. Quotations preserve existing capture rules; they do not acquire implicit lexical closures. A stored quotation/xt is a logical callable reference during building, lowered later to the target's execution-token representation. [R4; B1 §9]

A mutable `defer` binding is owner-managed state with a prior-generation precondition and new target reference. Generated names, type registrations, protection tables and dispatch tables are collected from the actual publishing owners, not reconstructed from a memory image after the fact.

### 7.6 Compile-time effects and failure

Portable compile-time execution admits deterministic declared input reads and owner-managed state construction. File generation and external tools are separate declared actions. Arbitrary network/process/FFI activity while loading source is either forbidden under reproducible/cross-required mode or explicitly source-only and uncacheable. A type signature is not proof of purity.

Transactions can undo unpublished declarations and private target allocations. They cannot undo an already issued external operation. The driver must not automatically retry an effectful build after discovering an undeclared input. It reports an effect boundary and preserves a receipt of any observed external work. [P1 §8]

### 7.7 Cross-width example

A helper constructs a record containing a language integer and a pointer to a static string. It allocates object A, string object B, writes the integer in target byte order, and records `Object(B,0)` at A's pointer field with its schema width. The final linker places B and writes the checked target address. On macOS, Linux and Windows the result is independent of B's temporary host allocation address. Changing target field alignment or foreign pointer width changes the layout identity and forces the necessary elaboration/rebuild.

## 8. Packages, source order, contributions and retention

This section is conditional on P1, the package incremental-build design. P1 is in [package-build.md](package-build.md), reviewed so far only at its interface with this page; its own review (dot habu-review-and-schedule-dbdba551) gates P4.

This design extends the package incremental-build design instead of inventing another object cache. Its unit is a **reusable, unstripped package contribution installed at its original source-load position**. A contribution can cover several source regions; it is neither an arbitrary file nor a complete process snapshot. [P1 §§1, 4]

### 8.1 Dependency edge classes

The build graph records `BuildExec`, `TargetRuntime`, `LinkOnly`, `SchemaGeneration`, `Asset`, `InitOrder`, and `Observation` edges. A compiler used to build an application is a BuildExec dependency, not a runtime root. A schema generator executes for the build platform; its generated binding library is compiled for the output target.

A generic topological sort may schedule independent upstream generators, but cannot reorder source-load contributions or expose declarations before the ordinary loader would. Runtime call cycles are legal where normal checking admits them. Compile-time cycles require the smallest legal **contiguous** contribution bundle preserving interleaved source effects; unsupported source is not fixed by invented forward visibility. [P1 §4.5]

### 8.2 Contribution installation

On a cache hit: decode and validate the contribution privately; resolve logical references; check source-position binding witnesses and prior owner-state conditions; reserve dictionary/checker/type/registry storage; install owned data and metadata privately; then publish the contribution's complete semantic result. Do not rerun its original immediate or static initializer.

Maintain separate host-helper and target-runtime contributions where their execution ABIs differ. Runtime initialization is a later declared phase, not compile-time replay. A file becomes `required` only when all of its contributions and required top-level work have completed. Reopening a package preserves its logical owner identity; a failed public/private declaration leaves no recordless shadow symbol behind. [P1 §§4.4, 8–10; R8]

Installation reproduces master's loader, which these contracts must pin rather than restate: source that ends inside a definition is refused (136425ef), and a `require` expands in its loader's scope (dba92d50).

### 8.3 Persistence format

Use the package-contribution profile of the existing AOT family, with `.hbp` as its descriptive extension, as specified by P1. Do not invent a competing `CodeArtifact` disk container. `CodeArtifact` below is an in-memory sum type; its persisted payload resides in the coordinated AOT profile or a standard external object format.

Reserve actual format/version codes at integration against the current writer/reader registry. Old readers reject new required descriptors. The descriptor identifies image capture versus package contribution, target compatibility, checker/IR/relocation schemas and required sections. Preserve bounded framing and integrity validation. [P1 §10]

### 8.4 Reachability and initialization roots

Final linking starts from explicit exports/entry points, target initialization, registered callbacks, interrupt vectors, dynamic dispatch tables and selected reflection/runtime/compiler services. Edges from address-taking, stored xts and registered handlers retain their code and metadata.

A normal AOT application excludes compiler/checker/REPL packages unless its profile actually requires them. A compiler product retains the checker and enabled backends even when ordinary program reachability would miss dynamically selected services. A `does>` companion, callback thunk or future dynamic binding is not dead merely because no direct CALL references it.

Persist unstripped contributions; perform profile-specific stripping only when assembling the final image. Report bytes by code, static data, zero-fill extent, relocations, checker/definition metadata, embedded assets and runtime initialization. Fixed native image assumptions remain a separate compatibility profile.

## 9. Language values, target layout and foreign storage

### 9.1 Baseline representation

Preserve 64-bit Habu language cells for the new profiles. This is consistent with [wasm-backend.md](wasm-backend.md) and master's eight-byte cell (`src/core/cell.f`). Target address width remains independent. [R5; B1 §6]

| Meaning | Language representation | Storage/conversion rule |
|---|---|---|
| Habu integer `n` | 64-bit cell | Defined wrapping/checked operations, not host C `long` |
| Habu real | Declared 64-bit real/cell semantics | Preserve bits or use the admitted FP representation |
| Habu pointer on memory32/target32 | Canonical zero-extended address in an 8-byte language slot | Check before narrowing; no lossy high-bit truncation |
| Foreign pointer field | Target ABI pointer width | Four bytes on admitted address32 foreign layouts; eight on corresponding address64 layouts |
| Habu true flag | Existing language flag representation | Convert explicitly to protocol Bool or branch predicate |
| HBR2 Bool | u32 0 or 1 | Never serialize an eight-byte Forth mask as the wire field |
| Execution token | Nominal callable descriptor/reference | Not interchangeable with C function pointers or browser handles |
| Browser handle | Two u32 fields under HBR2 | Nominal owner/type and generation validation |
| Native layout object | ABI-specific fields | Not a wire packet and not a host struct when cross-compiling |

### 9.2 Layout calculation

For an ordinary unpacked foreign record, classify each field under the exact ABI, align the current offset, assign the field, advance by its target size, and round final size to the required aggregate alignment. Packed, vector, bitfield, union, flexible-array and unusual aggregate-return rules are explicitly provided by ABI-specific classifiers. Unsupported declarations are rejected when imported, not approximated.

A Habu aggregate uses the Habu schema's lane order and slot layout, including tag/payload placement. Do not apply foreign C padding to a Habu multi-cell value. Generate all offsets and codecs from their owning schema. Three independent layouts may coexist: host internal compiler data, target language/foreign data, and canonical wire data.

### 9.3 Bounds and arithmetic

Administrative lengths, address expressions and region bounds use unsigned wide arithmetic with explicit overflow checks. Avoid unchecked `base + size` comparisons. For a buffer extent L:

```text
valid_span(p,n,L): p <= L and n <= L-p
valid_array(p,count,stride,L):
    p <= L and (stride == 0 ? count == 0 : count <= (L-p)/stride)
```

The owning API additionally enforces null/sentinel, alignment, object-lifetime and address-space constraints. A pointer is representable only when `p < 2^address_bits`; the separate exclusive extent can equal that power of two. Thus an extent at 2^32 is not a valid memory32 pointer even for a zero-byte operation. APIs that need that mathematical one-past marker keep it as an extent/cursor, not a narrowed pointer.

For alignment A, require a supported nonzero power of two, check `x <= MAX-(A-1)`, then compute aligned value. For signed relocation addends use a checked mathematical signed expression or multi-limb intermediate before field range validation. Two's-complement host wrap is not an overflow proof.

### 9.4 Memory policies

Keep the existing intentionally bounded native compiler-region policy as the native legacy default. Do not silently replace it with unbounded allocation merely to hold more backends. Build artifact buffers, target arenas and browser memory have separate explicit budgets. Streaming/spooling a large owned artifact is allowed under a checked storage abstraction; it does not change the compiler's semantic dictionary capacity.

Runtime allocations cannot contain host process handles in portable snapshots. Address32 narrowing occurs only at validated target operations. DMA/device addresses need an address-space-specific translation; virtual pointer bits are not automatically physical addresses.

## 10. Numeric, effect and exception contracts

A backend must implement the actual language semantics, not its machine's convenient result. Master's arithmetic contract defines wrapping `+`, `-`, `*`, division-by-zero throw code −6400, and wrapping `MIN-N / -1` to `MIN-N` (`src/habu/arith-abi.f:4-9`, `:23-24`). [R6]

### 10.1 Operation admission table

Each language/HIR operation has `Native`, `Helper(symbol,abi)`, or `Unsupported(reason)` for the resolved target and numeric policy. Helper dependencies are target-runtime dependencies, independently versioned and tested. A compiler-host helper library does not satisfy a target-runtime helper requirement.

| Operation family | Required handling |
|---|---|
| i64 add/sub/multiply | Correct modulo-2^64 result, including on register-pair targets |
| Signed divide/remainder | Guard zero and signed-min/−1 before a hardware/Wasm instruction that would trap differently |
| Shifts and rotates | Use the existing Habu shift-count contract; pin 0/63/64/65 and negative-input cases before selecting machine masking behavior |
| Comparisons | Preserve signed/unsigned intent; materialize whole-width language flags when observable |
| Integer/FP conversions | Check range and rounding under the selected policy; no accidental host conversion during constant folding |
| FP multiply/add/FMA | Respect contraction policy and instruction/helper availability |
| NaN, signed zero, denormals | An operation that makes a NaN answers `$7FF8000000000000` on every target, and a quiet NaN operand passes through, the left of two (`src/compiler/native/hir-word.f` `DEF-FLOAT`, 0fe69607). Signed zero and denormals follow `CNUM`'s FP model (`src/compiler/numeric-policy.f`). No profile selects away the NaN rule |
| Atomic operations | Check width/alignment/order, hardware lowering and runtime context together |
| Volatile/MMIO | Preserve access count, width and order; no speculative/fused accesses |

Constant evaluation must use target numeric semantics, independent of the build host's rounding mode, locale, integer width or accidental FP contraction. A target whose instructions make a different NaN canonicalises it: `X64SEL` clears the sign of SSE's default NaN after `addsd subsd mulsd divsd sqrtsd` ([x86-64.md](x86-64.md), `docs/x86-64.md:2361-2365`), and the Wasm selector clears the sign after f64 add, sub, mul, div and sqrt when the result is a NaN and no operand was one ([wasm-backend.md](wasm-backend.md)).

### 10.2 Throws, traps and foreign errors

Catchable Habu exceptions retain the full 64-bit code. Wasm uses explicit status edges and context storage. Native implementations may use their qualified internal mechanism, but no Habu unwind/nonlocal jump may escape through arbitrary C/COM/browser frames. Foreign entry wrappers catch expected Habu failures and translate them according to the actual foreign function's return contract. Native foreign entry exists on master as C callbacks: `FFI-CB` catches a throw in the callback body and answers C the declared `FALLBACK` value (`lib/ffi-callback.f`, `docs/ffi-callback.md:1-9`). [B1 §8; WN]

A fatal invariant failure poisons/discards the affected runtime or process. It is not converted into a recoverable domain error. Restoring a stack depth on catch does not restore overwritten linear resources, heap data, browser requests, file writes or remote changes. Cleanup precedence follows the existing Habu contract; tests cover body throw, cleanup throw, nested catch and fatal termination.

### 10.3 Memory model and concurrency

A single-worker browser profile does not promise threads. Native thread support requires per-thread/context state, allocator synchronization, foreign-call reentrancy and actual atomic/fence lowering. Do not treat C66x multicore shared memory as ordinary cache-coherent CPU memory without a board/runtime contract. Volatile, atomics, interrupts and DMA coherency are separate concepts.

## 11. Backend providers, pass graphs and machine constraints

### 11.1 Registry versus instance

An immutable `BackendDescriptor` identifies implementation code, accepted machine families/states, supported representations and stage factories. A `BackendSession` contains its mutable state. Registration publishes a complete validated descriptor once; it must not expose a row before its mandatory function tables are installed.

Replace the four-row ceiling (`BACKEND-ROWS`, `src/compiler/target.f:441`) with a manifest-derived capacity or checked storage sized to the loaded descriptors. This change is P2; Wasm alone fits in the existing rows and does not need it. Keep stable `BackendId` separate from runtime row index. Sort manifest entries deterministically. Duplicate provider identity or ambiguous providers for the same resolution is an error; an explicit backend selection can disambiguate experimental providers.

The old A32 and Thumb2 target variants may map to one ARM32 provider with different instruction-state configurations. Do not renumber old target wire codes to achieve that. A new backend may require extending the centrally owned target vocabulary and validators; the goal is one owner for that extension, not a false promise of adding arbitrary architectures without any schema change.

### 11.2 Stage contract

```text
BackendDescriptor {
    id, implementation_revision,
    target_match, capability_query,
    create_session, build_pass_graph,
    validate_machine_module, seal_emission,
    relocation_provider, debug_provider
}
PassNode {
    input_dialect, output_dialect,
    requires_facts, invalidates_facts,
    execute(session, owned_or_borrowed_input),
    validator, metrics_name
}
```

The driver enforces ownership and phase transitions. A pass cannot retain a borrowed arena after its owner retires. Required validators run after transformations that invalidate their facts. The common driver does not impose a single “schedule then allocate” ordering on every architecture.

### 11.3 Native pass families

A practical baseline is semantic legalization; ABI call/return lowering; machine selection; liveness; constrained allocation/spill insertion; target scheduling/fixups; frame/prologue/epilogue construction; branch/layout relaxation; encoding; final machine/relocation validation. Targets can reorder or iterate stages where necessary, provided the graph states which facts are recomputed.

Machine descriptions include register classes and alias units, reserved registers by runtime/platform, fixed/tied/early-clobber operands, call clobbers, condition-code effects, stack alignment, legal addressing modes, instruction lengths and branch ranges. Runtime register reservations are target-ABI facts, not “all registers except SP” or a host-derived pool.

On x86, two-address operations, subregister aliasing, shift/divide fixed registers and flags dependencies need explicit constraints. On ARM64, platform reservations, immediate/addressing legality, literal reach and frame conventions are explicit. Preserve the Intel lane's actual accepted register contract; an older design's preferred register assignment is not authority to change it.

### 11.4 C66x scheduling

Use the existing C66x assembler/facts/simulator as substrate, but recognize its admitted instruction subset and shared-oracle risk. The inspected facts include functional-unit use, cross-path reads, registers read/written, delayed load writes and branch/idle information. Both scheduler and simulator consuming the same incorrect facts would agree incorrectly. Add independently specified instruction fixtures and vendor/hardware checks. [R7]

The conservative correctness scheduler maintains a cycle scoreboard: pending register writes and availability, functional-unit occupancy, cross-path and register-bank usage, memory ordering, predicate dependencies and delayed branch state. For each selected instruction, compute the earliest legal issue cycle, insert explicit delay/NOP operations when necessary, reserve resources, and schedule its writes. Reject unsupported encodings rather than assuming one-cycle behavior.

A serial first implementation still respects every load/branch delay and resource constraint. “One instruction per cycle” is not automatically safe. Initially drain pending effects at conservative block boundaries and emit explicitly safe branch-delay sequences. An optimizing packetizer then groups ready instructions only when complete packet constraints hold. Allocation or spill insertion creates new dependencies; rerun scheduling and final hazard validation afterward.

Software pipelining is a later optional pass with dependency-distance/resource proofs and modulo-schedule validation, not a prerequisite for a correct baseline. A backend capability report must distinguish scalar correctness from packetization and pipelining optimization.

### 11.5 Primitive implementations

Keep the shared primitive specification as the source of names/effects. Separate those declarations from host and target implementations. A backend/runtime provider supplies a body or helper under the same checked contract. `complete` checks the requested primitive closure for the current product profile, not all conceivable hosted services for every firmware object.

`src/habu/kernel-hir-x64.f`, which builds x86-64 primitive bodies from HIR, is master's precedent for a separate body provider.

A missing target implementation produces a dependency diagnostic. It never falls back to a host-callable primitive during target execution, and a target runtime's syscall emitter must not be loaded as a host service implementation.

## 12. Owned artifacts and standard object boundaries

One artifact type covering native code, objects and Wasm modules would conflate compilation phases. Separate them:

```text
EmissionUnit = NativeFragment | WasmFunctionPlan
NativeFragment {
    target_contract, symbol_definitions, byte_sections,
    relocations, frame_unwind_facts, source_ranges,
    runtime_requirements, validation_evidence
}
WasmFunctionPlan {
    target_contract, function_identity, signature,
    typed_locals, structured_operations, symbolic_operands,
    source_ranges, admission_facts
}
Contribution {
    declarations, checker_facts, target_objects,
    emission_units, imports, owner_deltas, init_descriptors,
    observed_dependencies, compatibility_requirements
}
LinkPlan { input_contributions, roots, symbol_resolution,
           placements_or_indices, imports, finalizers }
PackagedArtifact = NativeObject | NativeImage | Firmware | WasmModule | ReleaseBundle
```

An emission unit is not a finished `.o`, `.exe` or `.wasm`. A contribution is not ready for public dictionary installation until all of its ownership and checker metadata validate. A packaged image is not evidence that it executes correctly.

Master's nearest result to an emission unit is the unplaced emission: `NBACK:EMIT-UNPLACED` writes a routine measured from no slot, so every site that leaves it is a row for a linker to write (`src/compiler/native/backend.f:36-40`); `NEMIT` holds the sealed bytes and rows in no machine's terms (`src/compiler/native/emission.f:1-24`); and `NSHADOW` keeps one per published record for a second target (§7.4).

### 12.1 Native sections and symbols

A section has owner, kind, byte contents or zero-fill extent, size, alignment, permissions, placement constraints, address space, retention group and compression policy. Symbols have stable logical identity, linkage/visibility, type/call contract, defining section/offset, size and optional import/version requirement.

BSS/no-init have extents without gratuitous serialized zero blocks. Debug and unwind sections are distinct; stripping debug must not remove unwind data required for execution. Foreign names/ordinals remain foreign identities attached to typed imports; they do not replace Habu package symbol identity.

### 12.2 Encoding and ownership checks

Before seal, validate all extents, alignment, reference widths, symbol definitions, entry offsets, relocation groups and source-map spans. Canonicalize padding and order. Seal transfers ownership from the builder. Publication, linker and serializers consume only sealed units.

Malformed input objects are treated as untrusted bytes. Verify declared sizes and integrity before allocating from their counts, then validate indexed references and schema compatibility. Reject cyclic or overlapping ownership descriptions not admitted by the artifact profile. Compression, when used, requires a declared and enforced expansion bound.

### 12.3 Interoperability support levels

A Habu-native linker supports a documented subset of ELF/Mach-O/COFF and target relocations. Import of a general C object may require TLS, COMDAT, weak aliases, unwind, constructor arrays or relocation variants outside that subset. The object reader reports unsupported constructs precisely and can route the action to a pinned external linker when the profile permits it. It does not silently drop unknown sections carrying execution semantics.

Standard object-format interoperability and Habu package reuse are different: a C `.o` normally lacks Habu checker/private declaration metadata and cannot masquerade as a Habu contribution.

## 13. Link planning, relocations, image writing and finalization

### 13.1 Link algorithm

1. Validate input contribution/object compatibility and selected target libraries.
2. Resolve Habu and foreign symbols using separate name/identity rules. Reject duplicate strong definitions. Apply only explicitly supported weak/COMDAT/version policies.
3. Expand demanded archive members and runtime-helper dependencies deterministically; iterate to closure with indexed worklists. Preserve defined archive-order behavior for foreign tool compatibility.
4. Compute reachability from product roots, retaining initialization, exception/unwind and callback groups as required.
5. Allocate semantic sections and memory regions under image constraints, or assign Wasm indices under §17 ([wasm-backend.md](wasm-backend.md) §10.1).
6. Run target relocation/layout relaxation to a checked fixed point.
7. Encode all final structures and regenerate layout-dependent debug/unwind offsets.
8. Validate the completed object/image independently of the writer's construction state.
9. Finalize checksums/signatures/deployment containers in the specified order and write atomically.

For a symbol cycle, compute a canonical strongly connected component descriptor when hashing dependencies; do not recursively hash hashes until they happen to stabilize. Algorithms use stable traversal order. Library discovery uses the target toolchain/sysroot only.

### 13.2 Relocation model

```text
Relocation {
    source_section, byte_offset, field_encoding,
    expression: S_plus_A | S_plus_A_minus_P | PageDelta |
                GOT | PLT | TLS | FunctionIndex | TableIndex | other_registered,
    target_ref, signed_addend,
    pc_origin_rule, scale, signed_range, alignment,
    pair_or_group_id?, relaxation_class
}
```

A relocation provider defines exactly which instruction bits carry the value, target endian handling, PC bias, required alignment, overflow range and pairing rules. Validate the full patched field span, not just the first byte. Paired high/low relocations share one target expression and are checked together.

Useful explicit examples:

- An x86 `rel32` uses the address after the relevant instruction's displacement field according to that encoding, not a generic “instruction start.” The computed signed displacement must fit 32 bits.
- An ARM64 page-relative sequence uses its specified page basis, independent of the build host's operating-system page size. Instruction page arithmetic is not a host memory-map operation.
- A Thumb function reference includes the instruction-state convention; a data reference does not. Thumb instruction serialization follows its halfword encoding, not a blanket native 32-bit store.
- A Wasm function/type/table reference is an index, not a native address. Its variable-length encoding is handled by the Wasm encoder.

Exact relocation constants/bitfields come from target-owned schema tables and qualified object-format specifications. The portable linker does not carry an expanding architecture switch for every field patch.

### 13.3 Relaxation and termination

Use a monotone expansion algorithm for the initial native linker. Start with allowed short/small forms under a deterministic layout; when a reference is out of range, promote its encoding or allocate a declared thunk/island. Do not demote during the same run. Recompute affected placements and ranges until stable.

Each site has a finite promotion chain. Thunks have bounded counts and deterministic placement constraints. Exceeding the configured code model or thunk budget fails explicitly. Final validation rechecks all references after the last size/alignment change, including data, literal and unwind relationships. Optional shrink optimization is a separate pass with its own convergence and byte-identity tests.

Avoid O(n²) full rescans when practical by indexing references to moved sections and maintaining affected worklists. Correctness is more important than premature incremental link patching; the first package-incremental implementation can relink the entire unplaced contribution set deterministically. That whole-set relink is what lets the browser application binding (P7) proceed without the package cache (P4); see §27.

### 13.4 Format and execution policy

| Format layer | What it owns | What remains outside it |
|---|---|---|
| ELF | Object/header/section/program-header encoding, symbols and supported relocations | Linux loader policy, libc/interpreter choice, bare-metal memory map |
| Mach-O | Mach-O structures, supported relocations and link metadata | macOS deployment/signing/runtime policy |
| COFF/PE | COFF objects; PE image sections, imports/exports/base relocations | Win32 runtime services, COM lifecycle and process policy |
| Firmware container | Verified image regions and required wrapper/checksums | Board boot state, flashing authorization and device execution |
| Core Wasm | Module sections/types/functions/data/table structure | Browser HBR2, WASI interfaces, host grants and application assets |

Windows image RVAs, file offsets, section alignment and base relocations have distinct meanings; do not conflate them when sharing layout utilities. [PE-COFF]

### 13.5 Finalization and reproducibility

Unsigned canonical image bytes are produced before signing. A finalizer declares the exact input format, target environment, executable tool host, credentials/grants, output rules and determinism policy. Signing credentials do not belong in cache keys or public manifests; the approved signer identity/policy and resulting receipt do.

Deterministic code/object comparison must not silently exclude arbitrary differences. Report separately canonical content identity and exact distributable bytes. Signature/timestamp containers can be nondeterministic while the underlying code content is reproducible; compare using an explicitly specified profile, not an ad hoc “strip until equal” script.

## 14. Native publication, image capture and code retirement

Native in-process code publication is allowed only into a runtime with a compatible **execution** ISA/ABI and enabled CPU features. Cross-generated target code goes to an artifact, never into the current host's executable region merely because a numeric address was supplied.

Staged file publication has landed on master: every `-o` and engine publication is staged (b394e3d3), a failed snapshot write leaves no partial image (3ad91bad, `src/habu/snap-lib.f`), and `hb-build` install stages through `RESERVE-SIBLING` (318155ae, `lib/fs-mutate.f`).

### 14.1 Publisher contract

```text
prepare(emission, runtime) -> PreparedPublication
install_private(prepared) -> InstalledPublication
commit(installed, dictionary_transaction) -> PublishedCodeLease
abort(prepared_or_installed)
retire(code_lease) -> RetireTicket
```

Preparation validates target compatibility, reserves writable code/data ranges and metadata, resolves references, constructs frame/unwind records, and allocates all publication bookkeeping. Installation copies/patches bytes and performs required executable-memory transitions, instruction-cache synchronization and platform unwind/indirect-call registration. None of that makes the dictionary binding visible yet.

Master's native publisher already has this order: a pending routine is proven, committed to the code region, filed with `NSHADOW` and only then appended to the dictionary (`PUBLISH-PENDING`, `src/compiler/native/publish.f:161-180`). The contract above generalizes that pending -> commit -> `APPEND-PENDING` sequence.

Commit publishes the prepared dictionary/checker/entry metadata as one logical transaction, with no fallible allocation or external callback after the visibility point. A late OS failure before commit aborts private installation and unwinds registrations. Report failure without leaving a callable half-word.

On Windows and macOS, use their documented executable-memory/signing/mitigation requirements and reject a disallowed dynamic-code profile. Do not weaken process policy to preserve an old JIT assumption. AOT-only compiler/application profiles remain valid alternatives where dynamic code is unavailable. [WN §§5–6]

### 14.2 Retirement and generations

A logical symbol has stable identity and a definition generation. Redefinition publishes a new generation under the existing language rules; old closures, callbacks and active frames keep their exact generation's code lease. Retire code only after all owning references and activations are gone, then unregister platform metadata and release memory.

Raw external callback addresses cannot be recalled safely just by replacing a dictionary entry. The callback owner must unregister/drain the foreign registration first. Initial implementations can deliberately retain old code until runtime shutdown within a bounded code budget; they must not claim safe eager unloading.

### 14.3 Capture versus portability

Distinguish three products:

- **Native image capture:** qualified snapshot of a particular native engine/layout/ABI, including its supported relocation/fixed-mapping convention.
- **Package contribution:** portable logical declarations/data plus target-specific code under the AOT package profile.
- **Application state snapshot:** versioned application data without process addresses, OS handles or executable-memory assumptions.

Keep retained-host capture readers bound to the host's actual layout. Target emission uses the target layout. Source-installed definitions changing fixed engine bands do not authorize interpreting a live old heap with new offsets. Retain the existing source/engine compatibility checks until the new object model replaces each use with a tested equivalent. [R9; P1 §§9–12]

A source-free compiler product must retain exactly the code, checker facts and source-independent metadata necessary for its declared capabilities. It must not accidentally capture build-host FFI addresses, source buffers, open files, pending transactions or unselected backend scratch.

## 15. Hosted services, native ABI and foreign interoperability

### 15.1 Host services

`HostServices` serves the executing compiler only. It owns file/input acquisition, path resolution, process invocation, monotonic/wall clocks, entropy, executable memory, terminal, native library loading and temporary output publication. Target runtime providers implement analogous language services for generated applications without pretending to be the same live resource.

A shared low-level library can implement both interfaces, but every call receives its appropriate context. The compiler's current directory, host page size, host errno values, signal structures and loaded DLL addresses never become target facts by default.

### 15.2 Filesystem and process portability

Use logical source-root identities and explicit path observation. Master already has source roots: a named `--load` entry's directory, then the working directory, then the engine's source root found from the executable (2c073d3c, adc041b8; `docs/forth.md:264-282`). Preserve byte-exact source content and appropriate platform filename handling. Do not blindly lowercase paths, collapse case-sensitive names, normalize Unicode names or replace separators inside source strings. Detect portability collisions under the selected source-root policy and report them.

External tools are invoked with structured executable/argument/environment/cwd records. The host adapter owns Windows command-line quoting or POSIX argv construction; target OS does not choose the quoting. Use response files only under the selected tool's specified encoding/escaping rules. No shell string concatenation from untrusted paths.

Writes use staging files plus the platform's qualified atomic-replacement/durability operations (on master, §14's staged publication). Atomic replacement and crash durability are different service capabilities. Capture stdout/stderr and exit/timeout/termination distinctly. Library discovery for a target action never falls back to the build host's default library path.

### 15.3 Native foreign call plan

```text
ForeignCallPlan {
    abi_identity, signature_identity,
    argument_classification, result_classification,
    hidden_parameters, register_assignments,
    stack_arguments, alignment, home_area,
    caller_saves, aggregate_temporaries,
    ownership_transfers, error_capture, callback_policy
}
```

Create the plan from exact foreign types before emission. Validate hidden aggregate-return pointers, varargs, sign/zero extension, floating/integer positions and by-reference parameters. An unsupported signature is refused at declaration. Use independent C/SDK caller and callee fixtures, not two Habu stubs generated from the same potentially wrong classifier.

C callbacks, the reverse direction, exist on master: `lib/ffi-callback.f` (01bc2a66, 1e9951ab) gives C 16 entry stubs into checked Habu words, and only macOS on arm64 has run them ([ffi-callback.md](ffi-callback.md)).

Windows x64 uses positional argument registers and caller-provided home space; Windows ARM64 has its own platform variations, including reserved-register constraints. Internal Habu register pools cannot be exported as those ABIs. ARM64EC is a distinct ABI and is not included merely by implementing normal Windows ARM64. [MS-X64; MS-ARM64; MS-ARM64EC]

### 15.4 Windows runtime details

This section waits for the Windows/COM design [WN], which does not exist yet; until then it states requirements only.

Use documented Win32 APIs rather than guessed native syscall numbers. Model pointers/handles separately from signed 32-bit return values such as HRESULT or C `int`; sign/zero extension is part of the return conversion. Generate PE imports/exports, required base relocations, TLS and unwind metadata under their own providers.

Thread entry and callbacks obtain or attach a runtime context, maintain activation-local foreign scratch, and obey the declared reentrancy policy. Stack probing, nonvolatile registers, unwind registration and process mitigations require native execution tests. `DllMain` remains minimal; complex runtime initialization and COM work occur through explicit safe entry points.

COM/OLE and the SWAI design MCP/PDM roles remain above the compiler ABI. Both COM client calls and server callbacks/thunks need exact layout and lifetime handling. No Habu throw crosses arbitrary COM frames. DLL unloading waits for all class factories, objects, callbacks, active calls and runtime leases. This portability document retains the separate Windows/COM design rather than reducing Windows completion to writing PE headers. [WN]

### 15.5 Signals, time and threads

Target signal/exception frame decoders are architecture/OS-specific. Shared code receives a normalized crash record, not raw `ucontext` or SEH pointers. Fatal signal reporting uses constrained safe operations; it does not promise ordinary Habu recovery from arbitrary memory corruption.

Native threads use per-context data/return stacks and declared TLS contracts. Compile-time execution remains serialized in the initial host realm even if the target program supports threads. A target's runtime clocks/entropy are imports/services; host build-time observations are separately declared inputs.

## 16. Freestanding, kernel and TI profiles

Freestanding applies to ARM64 and x86-64 as well as ARM32/C66x. It is not simply the hosted runtime with filesystem calls removed.

This section is specified only; no package implements it yet. Its package, P10, overlaps the [roadmap](roadmap.md)'s C6 microcontroller dot (habu-design-the-first-61f718f0), which owns the TI and ARM32 work and takes this section as design input.

### 16.1 Board and boot contract

```text
BoardProfile {
    machine_and_silicon_revision,
    memory_regions: [{space, origin, size, permissions, cache_policy}],
    boot_protocol, reset_entry, execution_mode,
    load_run_mappings, stacks, heap_policy,
    vector_tables, interrupt_policy,
    clock_and_FPU_setup, DMA_translation_and_coherency,
    console_or_panic, external_libraries,
    firmware_packaging, runner_profile
}
```

A memory region's exclusive end and all overlaps are checked. Load memory address and runtime virtual/physical address are separate. A raw binary, ELF, Intel HEX/S-record or vendor container is selected explicitly; “bare metal” does not imply one file format.

### 16.2 Startup algorithm

The board-owned startup establishes its required execution mode and stack, performs prescribed memory/FPU/platform setup, copies initialized data from load to run regions, zeroes BSS while preserving explicit no-init areas, establishes runtime context/TLS if selected, installs required vectors, runs ordered runtime initializers, and calls the Habu entry. Failure/panic behavior is board-defined and requires no implicit host console.

Boot-protocol inputs are typed records with documented lifetime. Kernel entry on x86-64 selects its boot protocol, stack/red-zone policy, enabled vector state and interrupt frame contract. ARM64 kernels select required exception level, system-register and translation regime; these are not imported from an ordinary user-space ABI. The compiler may emit privileged primitives only under an admitted target profile.

### 16.3 ARM32 R4F/R5F

Use one ARM32 backend with CPU, A32/Thumb2 state, instruction features and external float ABI selected independently. A core's FPU presence does not automatically select hard-float argument passing. Preserve instruction-state metadata in function entries and interworking relocations. Instruction endian and data endian are separately validated where a profile distinguishes them.

The Habu64 baseline uses register-pair/helper lowering as needed. Helpers and foreign calls obey selected AAPCS32 variants, including aggregate alignment and FP rules. Interrupt handlers have generated target-specific entry/exit wrappers and a restricted effect profile; they cannot allocate, block, throw across the interrupt boundary or call non-interrupt-safe services by accident. [AAPCS32]

### 16.4 C66x

Reuse `src/arch/tic6x/{asm,facts,eabi,sim}.f` under the new provider/session interface. Keep codegen correctness, C6000 EABI interoperability, target runtime helpers and board boot qualification as separate tasks. The instruction facts table must be versioned with its accepted ISA subset; unsupported instructions cannot receive default resource/latency facts. [R7]

A minimal product can use conservative serial scheduling and helper-based wide arithmetic. It still must obey register-pair layout, delayed results, call-clobber rules, stack alignment and relocation ranges. A correct simulator fixture is evidence for its subset, not proof of vendor library or silicon compatibility.

### 16.5 Memory ordering and DMA

MMIO operations specify width/alignment/ordering and cannot be coalesced or eliminated as ordinary loads/stores. DMA buffers have device-address mapping and cache maintenance ownership. Before a device reads CPU-written data, perform the board's required publication/flush/fence operations; before the CPU consumes device writes, apply the declared completion/invalidate rules. Shared-memory multicore access requires a qualified runtime protocol, not an assumption of desktop-style coherence.

### 16.6 Toolchains and qualification

No unverified physical memory addresses or silicon errata tables are supplied here. Those are mandatory pinned board inputs, not compiler defaults. The profile resolver can emit standalone code/object fixtures without them, but refuses a claim of bootable firmware until a complete board/boot/link manifest is present.

A vendor linker or packaging tool is an external action with explicit host compatibility. A remote tool runner is allowed only as a declared, authorized mode; it is not reported as a local cross-toolchain. The architectural goal remains local Habu code/object generation from every required native host and qualified finalization paths for each advertised image profile.

## 17–21. Wasm and Browser Runtime v2

These sections live in [wasm-backend.md](wasm-backend.md), beside the Wasm
detail they refine:

| Section | Subject | In wasm-backend.md |
|---|---|---|
| 17 | Wasm code generation, module linking and admission: profiles, lowering, calls with the pinned 16/16 arity rule, memory, symbolic linking (padded LEB immediates in the first slice) and the binary validator | §3, §7.3, §10, §17 |
| 18 | The exact HBR2 binding: two imports, six exports, the 128-byte control record, packet framing and lanes | §12.1 |
| 19 | Compiler obligations HBR2 imposes: the bounded callback verifier, admission evidence, ownership types, code retention and package boundaries | §12.2 |
| 20 | Browser release graph and runtime admission | §11.1 |
| 21 | Optional browser compilation and transactional module installation | §11.2 |

Section 21 is P12. It is adopted as specified and deferred until a caller
exists; Maki, the first browser caller, does not need it.

## 22. Compiler products, bootstrap, self-build and persistent state

### 22.1 Build the compiler for its host, not for its emission targets

A compiler product is a normal application targeting the platform on which that compiler will execute, plus an enabled-emitter manifest. Every backend implementation included in a Windows/ARM64 compiler is compiled into Windows/ARM64 machine code—even the implementation that emits C66x instructions or Wasm bytes.

```text
Build A, executed by macOS/ARM64 hb:
    compiler executable target = Windows/ARM64
    enabled emitters = ARM64, x86-64, ARM32, C66x, Wasm
    compile-time helper implementation = macOS/ARM64
    resulting backend implementation code = Windows/ARM64

Build B, executed by the resulting Windows/ARM64 hb:
    application output target = C66x freestanding
    compile-time helper implementation = Windows/ARM64
    resulting application code = C66x
```

This requires dependency roles, not a single global target switch. A package may have build-executed helpers, compiler-product runtime code, and ordinary target application code. Their artifacts have distinct execution/target identities even when their source bytes coincide.

Target SDK import metadata must be parseable on any build host without loading that target's libraries. A macOS compiler constructing a Windows compiler does not call Windows DLL entry points during the build. Code generators that execute as tools are separate host actions; their generated declarations/data enter the frozen target input world.

### 22.2 Bootstrap graph and recovery

Keep accepted seed/compiler pins and the existing recovery path. Do not turn this refactor into a requirement to implement independent cold bootstraps for every platform. The initial x86 recovery route can remain a cross-build from an identified working ARM64 compiler, as the Intel lane specifies. [R12]

Record a directed bootstrap graph whose nodes are executable compiler artifacts and whose edges are identified build actions. Each edge states source/input identities, execution platform, produced compiler platform, enabled emitters, runtime/toolchain profile and qualification receipt. A foreign produced compiler is not executable evidence until a matching runner actually executes it.

For each of the five native hosts, require an obtainable qualified seed path. For TI, require a path to a working cross-compiler and deployable target artifacts—not a compiler executing on TI. Browser self-hosting is a separate graph branch and must not become a dependency of native-to-browser AOT application builds.

### 22.3 Self-build qualification

For a host H:

```text
B0 = identified seed capable of producing H
B1 = build compiler from frozen source/input set S, target H
run B1 on H
B2 = build the same compiler from the same S, target H
run B2 on H
compare canonical B1/B2 products and run independent behavior gates
```

Where the retained seed/source-owner transition requires an additional bridge generation, identify it explicitly. Do not silently normalize an invalid first-generation layout because a later generation converges. Test source-free startup, checker/type information, host services, enabled-emitter registration, runtime code publication and another build from the produced compiler.

For browser self-build, B0 is generated by a native compiler; B1/B2 are generated by the actual Wasm-hosted compiler under the selected compiler embedding. Producing a small Wasm arithmetic module from a browser is not qualification of the compiler's whole dependency closure. Test the real source loader, checker, definers, target data construction and installation pump.

### 22.4 Content identity versus provenance identity

A self-build can never reach a literal byte fixed point if its executable embeds the complete hash of its immediate producer executable and that producer changes each generation. Separate canonical executable content from the detached build receipt.

The canonical product contains semantic versions and identities needed at runtime: language/runtime ABI, emitted backend contracts, source/application identity where semantically intended, image policy and required feature metadata. The detached receipt contains actual producer binary hashes, source-installed owner revisions, tool binaries, execution environment, logs and timing. Any producer identity that **changes semantics or generated bytes** remains a build-cache input; it need not be recursively embedded in executable bytes.

Compare unsigned canonical content, or another precisely documented signing-independent representation, and compare behavior. List any excluded nonsemantic section by schema and reason. Do not strip arbitrary mismatches until the hashes agree. Digital signatures, timestamping and vendor packaging have their own reproducibility constraints and receipts. A fixed point is useful evidence, not proof of correct language compilation.

### 22.5 Three persistence domains

Keep these artifacts non-interchangeable:

| Domain | Contains | Restore/install rule |
|---|---|---|
| Native image capture/snapshot | Native code and layout-bound runtime/checker state | Exact admitted architecture, OS/runtime ABI and layout/relocation profile |
| Portable package contribution | Unplaced code or module plans, declarations, typed symbolic data and owner deltas | Validate producer/input/target compatibility; install at its source occurrence |
| Portable application state | Versioned application records, document IDs and semantic data | Reconstruct through application/runtime migration APIs |

Open files, OS handles, thread IDs, live host pointers, browser object handles and arbitrary FFI callback addresses do not become portable application state. Persist a capability-specific reconstruction description only when the owning service defines one; reauthorization may be necessary.

Native image captures still need host-bound readers that understand the actual live source layout, then explicit target-layout writers. A target layout declaration cannot retroactively change the shape of the executing compiler's memory. Preserve the source-owner and capture-lifetime protections already present in the native build work. [R9]

### 22.6 Retention and source-free products

Compiler images retain the checker, compiler, selected backends, necessary reflection/type data and interactive facilities according to their declared profile. Ordinary application images root only runtime-reachable code/data, exported entries, registered callbacks, initializers and explicitly retained reflection metadata. Compile-time-only helpers do not become application roots merely because they ran during construction.

Stripping follows whole-program roots after contribution assembly. A package cache stores unstripped contributions so future consumers can use private helper dependencies, checker facts or newly selected exports. Test hidden quotation/defer references, callback tables and dynamically selected registered operations; name-based lookup requires a declared retained namespace rather than keeping everything accidentally.

## 23. Incremental builds, observations and cache soundness

This section extends the package incremental-build design ([package-build.md](package-build.md)) rather than replacing its contribution format or source-order importer. In particular, a cache hit installs an already checked contribution and owner-managed state; it does not rerun the source initializer. [P1 §§1, 4, 8–10]

### 23.1 Host identity reaches target objects

Build-host identity does not enter only host-executable artifacts. A target object can depend on the actual host-executed compiler, checker, definer, foreign generator or platform-observable compile-time result. Those facts must participate in its cache admission.

Separate:

- **Target compatibility:** whether the object is legal to link/load for a resolved target.
- **Build-result identity:** whether this producer and its observed inputs are entitled to reuse that object as the result of this action.
- **Cross-host equivalence evidence:** whether independently produced canonical objects match under equivalent inputs.

Initial cache mode pins the actual effective producer and execution-policy identity conservatively. Cross-host byte comparison is a test, not automatic permission to omit producer facts. A later qualified producer-equivalence class may permit wider reuse, but must identify exact producer binaries and semantic conditions. It is not inferred from a common Git commit, compiler version string or similar executable name.

### 23.2 Effective producer vector

```text
EffectiveProducer {
    executing_engine_content_id,
    active_checker_owner_revision,
    active_definer_and_primitive_adapter_revisions,
    active_backend_implementation_revision,
    pass_graph_and_validator_versions,
    runtime_abi_and_helper_versions,
    trusted_execution_policy_id,
    observed_host_environment_projection
}
```

A deliberate source-installed checker/backend/definer owner transfer changes this vector before subsequent work is keyed. The source tree claiming to contain a new compiler is not evidence that the currently executing compiler has changed. Conversely, hashing only the seed ignores a source-loaded owner replacement. [P1 §6.3]

Part of this vector exists outside master: the Intel lane's unmerged 8b05f5d9 keys the `hb-build` producer by its load closures.

For link/finalization actions, include actual linker/finalizer implementation, target libraries and relevant SDK data. For browser wrappers, include registry, wrapper ABI, runtime package and generator identities. Numeric policy, bounds/fuel instrumentation and internal call-ABI thresholds are code-generation inputs.

### 23.3 Observation classes

Record observations at the existing authoritative owner operations, not in a second approximate resolver. On master those owners are the `lib/policy.f` seal on source lookup (880a6e43, adf9bee0; [policy.md](policy.md)) and the source roots (2c073d3c, adc041b8; `docs/forth.md:264-282`):

| Observation | Required identity/witness |
|---|---|
| Name lookup | Source-position search environment, selected definition generation, absence/ambiguity witnesses |
| Type/effect query | Exact admitted contract/type-layout projection |
| Runtime direct call | Selected provider binding and call contract; body revision when inlined/optimized across boundary |
| Compile-time call | Executed implementation closure and state/input observations, including indirect/deferred targets |
| Constant or target-layout evaluation | Value, producing implementation and target facts consumed |
| Source acquisition | Owned bytes, resolution policy, selected path/root and absent preferred candidates |
| Generator | Executable/tool identity, argv, environment, declared input bundle and output bytes |
| Owner state | Owner identity, prior-state precondition and declared projection/delta |
| Capability query | Whether observed during compilation, exact answer or target service declaration |

Calling Q only at runtime may allow a caller to reuse code after Q's body changes. If a compile-time computation later executes through that caller into Q, the computation depends on Q's implementation. Promote the executed dependency closure; do not infer this from static runtime link edges alone. Cycles use a canonical strongly connected component descriptor, not endlessly recursive hashes. [P1 §7.6]

Conservative mode records full provider contribution revisions for observed dependencies. Precise contract-only reuse is enabled per independently tested observation class. Neither mode may ignore raw/native operations capable of bypassing the observation system. Such execution is opaque/source-only or rejected under required-incremental/cross-target restrictions.

### 23.4 Immutable input world and dynamic discovery

Own source bytes before hashing/compiling them. Record the source-root interpretation, include/require resolution and missing higher-priority candidates. Freeze the world used by discovery, diagnostics, checking, emission and cache keys.

A filesystem watcher, timestamp, file size or VCS change list may accelerate discovery; none proves content equality. A same-size/same-timestamp edit must invalidate. A touched but byte-identical source can hit.

An unforeseen input discovered before any externally visible action permits aborting the local action, extending the input world and retrying under a bounded policy. After uncontrolled effects, do not rerun the action silently: compile source-only without publishing a reusable result, or refuse the required-hermetic profile. The execution receipt reports the reason.

Source-root remapping is an explicit reproducibility feature. Do not erase path-sensitive semantics while normalizing debug paths. Case-sensitive file identity and Unicode spelling remain distinct unless the source resolver itself specifies equivalence.

### 23.5 Cache keys and invalidation

A conceptual key is:

```text
ActionKey = H(domain, action_schema,
              product_role, EffectiveProducer,
              owned_input_manifest, source_occurrence_contract,
              consumed_target_projections, compile_options,
              observed_dependency_descriptors, owner_preconditions)
```

The descriptor must use canonical framed encoding from §4. Keying is not the only defense: import validates the stored artifact, target/schema identities, owner preconditions and observation witnesses again against the current load position.

A runtime-only provider edit need not rebuild every source predecessor. A build-time owner transfer can invalidate a much larger dependency cone. A browser application edit need not regenerate unchanged generic host modules; a registry-layout change must rebuild the affected Habu codec, host validator, wrappers and release manifest together.

Do not include final virtual placement in an unplaced contribution's semantic revision. Include it only when source truly observes placement and the admitted profile preserves that dependency. Final layout can then change without falsely recompiling unrelated contributions.

### 23.6 Storage, integrity and concurrent access

Treat cache files/indexes as untrusted inputs. Validate framing, bounds, section uniqueness, schemas, digests, target facts, references and resource ceilings before allocation or publication. A digest detects corruption relative to expected content; it is not an authorization signature.

Write an immutable candidate to a temporary path, flush under the selected durability policy, atomically publish the completed artifact, then update the disposable index. Master's staged writes (§14, `RESERVE-SIBLING` in `lib/fs-mutate.f`) are the base. Concurrent writers of identical content may converge; a reader never sees a half-written mutable artifact. Locking protects index publication or use an equivalent checked compare-and-publish protocol. Filesystem behavior differs across hosts, so the host-service implementation owns the actual primitives.

Eviction removes only unleased cache objects and rebuildable indexes. Live contribution/install handles retain their backing owned storage or an immutable mapped file lease. Corruption is a miss with a diagnostic or a hard error under strict verification, never permission to skip validation. Quotas cover compressed and declared uncompressed sizes before decompression.

### 23.7 Acceptance examples

Changing Q's implementation while R computes a compile-time constant by executing P→Q invalidates R, even if P's public signature stays unchanged. Adding a previously absent preferred include file invalidates the affected lookup. Replacing the active checker owner invalidates later actions using that checker. Reordering unrelated physical cache rows changes no semantic key. A failed contribution leaves no live symbol or `required` completion mark. Building one target after another must not reuse the first target's layout-dependent constant.

These are required cache-on versus cache-off equivalence fixtures, not optional optimization benchmarks.

## 24. Toolchains, runners, command surface and qualification records

### 24.1 Explicit toolchain actions

```text
ExternalToolAction {
    executable_content_id, tool_version_metadata,
    compatible_execution_platforms,
    argv_vector, environment_allowlist,
    owned_input_bundle, target_sysroot_and_libraries,
    declared_output_bundle, working_directory_policy,
    timeout_and_resource_limits, finalization_stage
}
```

The tool executes on the **build action's execution platform**. Its input/output target may differ. A Windows target linker is not necessarily a Windows-hosted executable; an installed vendor tool is usable only when its own execution constraints are met.

Resolve target library/header/import metadata solely from the selected toolchain/sysroot. Do not fall through to `/usr/lib`, the host's dynamic loader path, or an arbitrary Windows SDK installation when the target manifest names another version. SDK path discovery may suggest candidates, but selecting/pinning them is a resolved build input.

A remote tool adapter, when explicitly selected, exchanges a content-addressed input bundle and receipt. It records tool identity, target/profile, returned content hashes and execution location. It does not send undeclared secrets, assume a shared checkout, or label the result “locally built.” Remote finalization is a bring-up option, not satisfaction of the user's fully local cross-toolchain goal.

### 24.2 Runners are separate from producers

A runner declares accepted artifact kinds, target ABI/features, environment identity, launch/deploy mechanism, timeout, output/exit normalization and cleanup. Examples are native process execution, an explicit emulator, a browser test host, a board debugger/loader or a remote test machine.

Builds do not execute output by default. A test request lacking a compatible runner reports `NoRunner`, not `CodegenUnsupported`. A successful linker does not count as board boot; successful emulator execution does not count as physical device qualification. Native self-build requires a runner for the produced compiler host.

Flashing/deployment operations are distinct from read-only compilation. They require an explicitly selected device and deployment policy, rather than treating an attached device as implicit permission to overwrite firmware.

### 24.3 Proposed CLI, not currently implemented flags

These examples specify the intended command model. They are **not claims that current `hb` accepts these commands**:

```text
hb build app.f --target x86_64-unknown-linux-gnu --emit object -o app.o
hb build app.f --target aarch64-pc-windows-msvc --emit executable -o app.exe
hb build firmware.f --target c66x-none-eabi --board <pinned-board-profile>
                   --emit firmware --toolchain <pinned-toolchain> -o firmware.out
hb build ui.f --target wasm32-habu --runtime browser-runtime-v2
              --browser-profile DOM --emit browser-release -o release/
hb build-compiler --compiler-host aarch64-pc-windows-msvc
                  --enable-backends arm64,x86-64,arm32,tic6x,wasm -o hb.exe
hb test-artifact app.wasm --runner <qualified-browser-runner>
hb explain-target wasm32-habu --runtime browser-runtime-v2
hb explain-cache <action-receipt>
```

Master's `tools/native-build.f` takes `--target linux-aarch64`, `macos-aarch64` or `linux-x86-64` (`tools/native-build-args.f:58-70` and `tools/native-build-args.f:92`). These labels and the existing entry scripts remain aliases until P13 retires target selection on the building engine. The resolver rejects ambiguous aliases and prints the resolved semantic profile. `--target native`, if admitted, means an explicitly requested host-derived default; generic backend code never performs that detection itself.

`--emit object` must not load a final-image signer or demand a board runner. `--emit browser-release` includes the complete v2 release graph rather than only a `.wasm` file. `--build-only`/absence of a runner never implies foreign execution.

### 24.4 Diagnostics

Diagnostics use master's one namespace: each new code is an `E-` throw code registered in `tools/diag-code.f`, and a rejection is emitted in the checker diagnostic JSON shape of [repair-diagnostics.md](repair-diagnostics.md), which the language server already publishes (e965e75b, b1d62b4b, adc041b8). There is no second, string-coded namespace. The record below lists the fields a portability rejection carries; its `code` is such an `E-` name.

Every rejection has a stable code, action/stage, source span where applicable, logical package/contribution occurrence, execution platform, output target identity, requirement, observed capability/identity and dependency path.

```text
Diagnostic {
    code: E-TARGET-ABI-UNSUPPORTED,
    stage: "foreign-call-classification",
    action_id, source_span, definition_id,
    output_target, required_abi_variant,
    available_provider, dependency_path,
    explanation, evidence_kind
}
```

Distinguish invalid configuration, coherent-but-unimplemented feature, missing backend module, missing runtime service, missing target library, unavailable tool host, no runner, malformed artifact, stale generation and capability denial. A message saying only “unknown target” is insufficient once resolution succeeded.

A strict profile fails rather than silently selecting another ABI, lowering unsupported atomics to non-atomic operations, truncating addresses, invoking JIT fallback in a promised AOT build, or switching browser profiles without authorization.

### 24.5 Machine-readable qualification matrix

For each `(compiler execution platform, output profile, artifact kind)` record separate capabilities and evidence:

```text
SupportRecord {
    configuration_identity,
    declarable, sources_resolvable, backend_loaded,
    lowering_coverage, encoding_coverage,
    object_writer_coverage, linker_coverage,
    runtime_and_toolchain_available,
    generation_receipts, execution_receipts,
    qualified_environment_constraints,
    unsupported_operations_and_reason
}
```

The required matrix includes every implemented output profile generated from all five native hosts. It does not require every host to physically contain every target device. Generation and independent target execution can be separate receipts linked by artifact hash. All five native compiler products additionally require their own executable/self-build gates.

The repository does not gain qualification simply because this design lists a configuration. Exact browser versions, OS builds, CPU features, GPU/device drivers, SDKs, boards and tool versions are recorded only after tests. Profile tables must visibly distinguish “required” from “passed.”

### 24.6 Metrics without unsupported performance claims

Measure externally visible elapsed build time and per-stage time: input freeze, source resolution, checking, compile-time execution, legalization, selection, scheduling, allocation, encoding, contribution import, linking, signing/finalization and browser module preparation. Also record cache hits/misses with reasons, code/data/debug bytes, retained compiler roots, peak temporary memory, branch relaxations/thunks, Wasm locals/types/table slots, helper calls and target runtime footprint.

Performance regressions must not be hidden by stripping required checker facts or weakening tests. Set release budgets from measured baseline and product needs. No universal compilation-time or binary-size improvement is asserted by these structural changes alone.

## 25. Verification, fault injection and acceptance

Tests are organized around invariants and independently observable behavior. A backend accepting its own output, a schema generator validating its own table, or a simulator sharing the encoder's mistaken facts is insufficient alone. Combine source/IR validation, independent decoders or foreign callers, cross-host artifact comparisons, actual execution and deliberate mutations.

### 25.1 Target, session and staging tests

| Test ID | Fixture | Required result |
|---|---|---|
| T01 | Resolve every native-host alias and explicit output target | Exact machine/ABI/runtime profile; no host-default substitution after resolution |
| T02 | Windows ARM64 compiler product with C66x and Wasm emitters | Compiler implementation bodies are Windows ARM64, not C66x/Wasm |
| T03 | Coherent target with absent emitter | `BackendNotLoaded`, distinct from invalid target |
| T04 | Same ISA with incompatible internal or external ABI | Reject wrong call/object contract before publication |
| T05 | Compatible objects with different optimization settings | Link permitted if ABI/runtime predicates agree; build identities remain distinct |
| T06 | Feature union exceeds selected runtime/CPU profile | Refuse or require explicit stronger profile; never silently use host CPU |
| T07 | ARM64 → x86-64 → ARM64 compilation in one session process | First/last semantic output match, scratch/registry state isolated |
| T08 | Nested compiler invocation fails after allocating scratch | Parent remains valid; every child-owned resource released or accounted |
| T09 | Concurrent use of legacy-global provider | Explicit serialization/refusal; no claimed thread safety |
| T10 | Target-layout constant differs between host and target | Host helper computes using explicit target facts; correct target constant |
| T11 | Target-only browser/Windows/TI operation executed at compile time | Fail with execution-domain dependency path |
| T12 | `create`, `here`, typed stores, quotations and `does>` | Target data contains only admitted typed references and correct target bytes |
| T13 | Cast host pointer to scalar and hide it in initializer bytes | No pointer-looking scan “repair”; reject cross-target capture/mark opaque |
| T14 | Failed generated definition or recordless tombstone | No newly visible checker/dictionary binding or poisoned future lookup |
| T15 | Cross-target change during an active action | Refuse mutation; a new resolved action/context is required |

T14 pins master's refusal of recordless checker symbols (16a4d57b). The test must exercise the real loader/checker, not only a mock symbol table. [R8]

### 25.2 Packages and caches

| Test ID | Fixture | Required result |
|---|---|---|
| C01 | Cache-on versus cache-off build of frozen inputs | Equivalent current semantic state and canonical artifacts |
| C02 | Repeated `include`, repeated `require`, multi-package file | Original occurrence/load-once semantics preserved |
| C03 | Failure in a file after its first contribution hits | File not prematurely marked fully required |
| C04 | New preferred include path appears | Next action invalidates negative lookup witness |
| C05 | Byte edit with unchanged size/timestamp | Miss; metadata cannot authorize hit |
| C06 | Byte-identical touch | Hit when all other inputs match |
| C07 | Q changes; R computes constant through P→Q | R invalidates even if P's call signature stays fixed |
| C08 | Active checker/definer/backend owner changes | Subsequent action producer identity changes |
| C09 | Runtime-only call provider body changes | Conservative mode misses affected dependency; precise mode hits only with tested contract classification |
| C10 | Source-owned initialized data imported from cache | State installed once; original initializer not re-executed |
| C11 | Owner-managed delta prior state changed | Import refused before live mutation |
| C12 | Unknown raw/native observation | Source-only/strict refusal, no supposedly hermetic hit |
| C13 | Corrupt/truncated/overlapping contribution sections | Rejected before allocation/publication of live state |
| C14 | Two writers publish one content-addressed artifact | Readers observe only complete validated artifact |
| C15 | Evict artifact during a live install lease | Backing bytes remain available until release |
| C16 | Raw absolute placement changes | Unplaced artifact unchanged unless source actually observed placement |
| C17 | Cross-host producers with no equivalence admission | No automatic reuse based only on source/version label |
| C18 | Cache package then strip final application | Cached unstripped contribution remains reusable for another root set |

Include state hashes, owned-data values and executed behavior—not only final file hashes—where layout or permitted metadata makes raw byte comparison inappropriate.

### 25.3 Numeric and layout tests

| Test ID | Fixture | Required result |
|---|---|---|
| N01 | Signed/unsigned limits, wrap add/sub/mul | Match existing Habu cell semantics |
| N02 | Zero divisor and signed minimum / −1 | Catchable −6400 and specified wrapped result, respectively |
| N03 | Condition with only a high bit set | True according to Habu; no accidental low-i32 truncation |
| N04 | Observable Boolean/mask conversion | Habu mask and HBR2 u32 Bool stay distinct |
| N05 | Shifts, remainder sign, integer/real conversions | Match pinned primitive contract; no host-language accidental semantics |
| N06 | NaNs, signed zero, infinities, subnormals, contraction; print rows for `-1 fsqrt`, `0 0 f/` and `inf inf f-` | Every made NaN is `$7FF8000000000000` and a quiet NaN operand passes through, the left of two (§10.1); the three print rows print the same on every target; protocol finite fields validated separately |
| N07 | Memory32 maximum offset and exclusive extent 2^32 | Last byte and legal empty spans handled without narrowing the extent |
| N08 | Pointer+length or count×stride overflow | Refuse before access; checked subtraction/division tests |
| N09 | 32-bit foreign pointer field beside 64-bit Habu pointer cell | Exact independent sizes/alignment/loads/stores |
| N10 | Little-/big-endian target serialization | Target bytes follow target; administrative artifact bytes follow their own schema |
| N11 | Wide product/tagged value and linear local transfer | All lanes preserved; no ownership duplication through spill/copy/error |
| N12 | Habu exception inside a foreign callback | Converted at boundary; no nonlocal transfer through foreign frames |

Compare math helpers against independent arithmetic/specification oracles. Random tests supplement explicit boundaries; random generation alone is unlikely to hit the most important overflow/NaN/layout cases. A trapping Wasm instruction must not be used as an implementation of a catchable Habu error.

### 25.4 Native code, linking and TI

| Test ID | Fixture | Required result |
|---|---|---|
| L01 | Native relocation at signed minimum/maximum displacement | Exact limits accepted; one-step overflow rejected/promoted |
| L02 | Paired high/low relocation with nonzero addend | Consistent symbol/addend/pair handling; malformed pairs rejected |
| L03 | Branch relaxation inserts thunk and shifts later sites | Iterate to stable legal layout; all ranges revalidated |
| L04 | Imported weak/strong/COMDAT symbols in supported subset | Documented resolution; unsupported semantics explicit |
| L05 | BSS, TLS, debug and unwind sections | Correct size/alignment/ownership; debug data cannot become runtime roots accidentally |
| L06 | ELF object versus Linux executable versus board ELF | Format validation alone does not substitute for loader/startup policy |
| L07 | Windows mixed positional int/float arguments | Independent native caller observes exact argument/result values |
| L08 | Signed 32-bit error/result in 64-bit return register | Correct sign/zero extension by declared type |
| L09 | Nested foreign callback and concurrent runtime contexts | Separate activation scratch, correct thread/context ownership |
| L10 | Executable publication fails before permission/unwind/cache completion | No callable entry becomes visible |
| L11 | Retire native code with live callback/frame lease | Code remains valid until lease ends |
| L12 | ARM32 soft/base versus VFP argument convention | Explicit compatibility decision and oracle-call test |
| L13 | ARM32 instruction-state/function-pointer interworking | Correct bit/state encoding and branch behavior |
| L14 | C66x dependent load, cross-path conflict, delayed write | Scheduler or terminal validator rejects illegal schedule |
| L15 | Spill inserted after C66x scheduling | Rescheduling/validation catches new hazards |
| L16 | C66x serial issue across basic-block/control boundary | Required delay/drain/control rules remain satisfied |
| L17 | Firmware load address differs from execution address | Copy/zero tables and initialized target data correct at startup |
| L18 | IRQ/FPU/DMA/cache policy not supplied by board profile | No bootable qualification; precise missing-profile diagnostic |
| L19 | Bare-metal x86-64/ARM64 kernel profile | Privilege/entry/stack/red-zone/interrupt assumptions explicit |
| L20 | Target library/tool missing on build host | Object-only build remains possible when independent; finalization fails at its own stage |

Use independent disassembly, ABI callers and target execution. Existing C66x assembler/facts/simulator sharing can validate consistency, but their common implementation facts can share a defect; supplement them with manual-derived golden encodings and an independently implemented/vendor execution oracle where available. Record the exact tested instruction subset and board/tool profile. [R7]

### 25.5 Wasm and HBR2

| Test ID | Fixture | Required result |
|---|---|---|
| W01 | Structured loops/branches with cyclic edge copies | Correct parallel-copy semantics and enclosing-label depths |
| W02 | Unsupported irreducible control graph | Explicit refusal, or qualified dispatcher lowering; no incorrect nesting |
| W03 | Function/type index crosses a LEB-width boundary | Re-encoded body/section sizes remain correct |
| W04 | Imported function has correct name but wrong signature | Admission rejects actual type mismatch |
| W05 | Required Wasm feature unavailable | Profile rejection/fallback only when explicitly authorized |
| W06 | Wide arity crosses fast-call/frame ABI threshold | Deterministic descriptor and correct direct/dynamic adaptation |
| W07 | Full-width throw, fatal trap and browser Idle | Three different behaviors; no status-domain conflation |
| W08 | HBR2 header/record/control layout | Exact 96/32/128-byte layouts, zero padding and registered types |
| W09 | Canonical STOP packet | 136 bytes under the inherited reference shape |
| W10 | IDs/epochs greater than 2^53 | No JavaScript Number precision loss |
| W11 | Strict UTF-8 versus unpaired native UTF-16 draft unit | Distinct codecs; valid draft code units not destructively normalized |
| W12 | Input lease replaced, stopped, consumed or from old epoch | Stale use refused; ownership and reservations settled exactly once |
| W13 | Memory grows in an export between host accesses | Host view reacquired; no detached/stale view read |
| W14 | submit returns Backpressure/OOM/Denied | Habu retains packet; no accepted-send credit increment |
| W15 | submit returns Accepted then async operation fails | Host owns copied request; one typed terminal outcome plus proper cleanup |
| W16 | Ordinary lane exhausted while request must terminate | Reserved control/terminal path remains usable |
| W17 | Synchronous import tries to call an export recursively | Reentry blocked; completion scheduled later |
| W18 | Callback bound unknown or cost exceeds 2,048 | Compile/admission rejection; not “probably fast” |
| W19 | Callback within operation count but exceeds node/byte budget | Relevant separate budget rejection |
| W20 | Candidate module has start/active shared segment | Incremental admission rejects before instantiation |
| W21 | Prepare fails after memory/table capacity grew | Old semantic root remains; retained capacity accounted, not claimed undone |
| W22 | Arbitrary initializer tries writing existing shared object | Not admitted as declarative candidate initialization |
| W23 | Epoch/provider generation changes during browser compilation | Prepared ticket discarded/revalidated, never committed stale |
| W24 | Old callback/job keeps a superseded module alive | Generation lease prevents retirement |
| W25 | New AOT worker adopts state/draft | Versioned migration, new epoch; no copied raw memory/resource handles |
| W26 | Untrusted plugin requests shared privileged memory/table | Refused; isolated plugin profile used |
| W27 | Registry/schema mismatch in host/module/sidecar | Reject before bootstrap; no best-effort v1 reinterpretation |
| W28 | Module valid but grants do not authorize operation | Runtime Denied; valid bytes do not confer authority |
| W29 | Device/frame skipped while reliable pick is pending | Pick follows its retained scene/lease contract, not frame lifetime |
| W30 | Old namespace completion arrives after authority transition | Drain/quarantine under original namespace; never adopt into new authority |
| W31 | The same checked program compiled native and to Wasm | Differential rows: identical printed output, throw codes and final stacks |
| W32 | `-1 0 ?do … loop`, `MIN-N 0 ?do … loop` and a counting-down `?do … +loop` | The first two take no turn; the third runs to its limit, as on native (`docs/forth.md:939-955`) |

W29/W30 are integration tests of **existing v2** semantics. Their implementation remains in v2 owners, not the Wasm instruction selector. Browser tests must include the actual supported Safari/WebKit, Firefox and Chromium products/profiles when those are claimed; one engine's validator is not universal-browser evidence.

### 25.6 Failure injection discipline

For every preparing operation, enumerate allocation, buffer-growth, symbol-resolution, reference-validation, external compilation, instantiation, permission-change, import-installation and finalization failure points. Inject a failure at each point and verify: no illegal publication, no source-order change, no leaked ownership, no duplicate external action, no cache success receipt, and a reusable old state or an explicitly poisoned/discarded runtime.

After the declared commit point, verify that the commit path contains no operation classified as fallible. Faults outside that assumption are fatal recovery cases, not an invitation to roll back half a root update. Record retained capacities separately from leaked live objects.

Mutation testing should deliberately alter ABI identity, type indices, relocation widths, source witnesses, schema hashes, HBR2 result codes, generation fields, C66x delay constraints, callback costs and ownership transfer points. A test suite that remains green after such changes does not adequately defend the contract.

### 25.7 Evidence levels and review checks

Keep `Specified`, `ReferenceModelChecked`, `GeneratedByHabu`, `ExecutedOnTarget`, and `QualifiedConfiguration` distinct. Store evidence receipts with source/compiler/artifact hashes and tool/environment identities.

HBR2 reports its own reference-model results. Those are inherited evidence about its supplied specimens, not tests this design reran. Likewise, the worked-example checks done while writing this design do not establish a Habu implementation or real browser execution. [B2 §§0, 28](browser-runtime.md#0-reading-and-precedence)

## 26. File-by-file integration map

Paths in the “destination/contract” column are proposed ownership destinations, not claims that those files already exist. The contracts are adopted now; the physical moves are deferred to P13. Introduce interfaces at current locations first, and keep a directory move and a semantic change in separate commits. A row whose current file is in a sealed engine package (§5.3) changes only through an engine rebuild.

| Current file/area | Problem or retained value | Destination/contract | First acceptance gate |
|---|---|---|---|
| `src/compiler/target.f` | Good immutable identity; incomplete resolved profile and four-row registry | Keep legacy schema; add composed target/layout/ABI/compatibility modules and manifest-sized registry | T01–T06; legacy digest goldens |
| `src/compiler/binding.f` | Existing target/numeric-policy binding | Retain; explicitly compose layout/internal ABI and consumed policies | N01–N06; no duplicated numeric policy |
| `tools/build-target.f` | Ambient three-way selection | Resolve aliases once into action-owned target context; temporary read-only compatibility view | T07/T15 |
| `src/os/*/target.f` | Host/target predicate ambiguity: `HB-TARGET-*` appears on 415 lines in 104 files; 74 uses select target semantics on the building engine, the rest select host services | Host identity from executing image; source selection from resolved manifest. P13 retires the 74 target-selecting uses, with a lint; the host-service uses stay | T01/T11 |
| `src/compiler/native/compiler.f` | Shared orchestration but native dependencies and module globals | Frontend orchestration + session-owned state + artifact/publication provider | T07–T09/W20–W24 |
| `src/compiler/native/backend.f` | Useful dispatch; global lifecycle/pass rows | Typed provider registry and session-owned provider contexts/pass graph | T08/T09/L15 |
| `src/compiler/native/emission.f` | Good byte-based facts; borrowed ambient result | Owned `NativeFragment` adapter and explicit lifetime; no instruction-count regression | L01/L10/C13 |
| `src/compiler/native/shadow.f` | Real cross-generation bridge | Adapt to per-target contexts and owned contributions; authenticate host refs during capture | T10–T13/C16 |
| `src/compiler/native/abi.f` | Host identity mixed with AArch64 routine construction | Split execution-host descriptor from ARM64 Habu routine/foreign ABI construction | T04/L07/L12 |
| `native/a64ir.f`, ARM64 selector/emitter files | Architecture-specific implementation in shared area | `src/arch/arm64/` ownership after interfaces stabilize | Existing ARM64 gate and byte parity |
| `native/x64ir.f`, `select-x64.f`, `emit-x64.f` | Existing Intel implementation | The Intel lane retains ownership; move by agreement after shared adapters land | Existing Intel gate; L07/L08 later Windows lane |
| `src/compiler/ir/{attr,context,schema,type}.f` | Decoders that mirror every CTARGET architecture, ABI and feature code | Each new target code is mirrored in the decoders in the same sealed commit (P1a) | T01; HIR schema version bumped |
| `src/arch/arm32/` | ISA construction available; complete target pipeline needed | Shared ARM32 provider with CPU/state/FP/ABI profiles | L12/L13/N09 |
| `src/arch/tic6x/` | Existing assembler, ABI/facts/simulator assets | Context-owned target provider; constraint scheduling and independent validators | L14–L18 |
| `src/core/cell.f` | Existing 64-bit Habu cell semantics | Keep language cell invariant; target pointer/foreign layout moves to explicit layout owner | N07–N11 |
| `src/habu/prims.f`, primitive registry | One language-effect authority | Preserve; separate body providers and runtime-capability catalog | Primitive parity + T11/N12 |
| `src/compiler/native/publish.f` | Native placement/record publication | Native publisher behind generic candidate interface | L10/L11 |
| `src/habu/aot-*`, native layout/capture tools | Existing owned capture and persistence machinery | Keep AOT family; add package profile per P1 and format-specific target adapters | C13/C18; self-build/capture gates |
| `src/habu/aot-shadow.f`, `src/habu/link-x64.f` | The audited legacy owner adapter (§7.4): capture of a shadow target's routines and the fixed-address x86-64 layout | P6 rides it for Wasm; §7.2's `TargetRef` replaces it in P3 | W01–W07; existing x86-64 cross-build gate |
| `tools/diag-code.f` | The one diagnostic code registry, with the repair-diagnostics JSON shape | Portability rejections register `E-` codes here (§24.4) | Rejection rows emit the registered shape |
| `lib/ffi-callback.f` | C callbacks into checked Habu, catch -> `FALLBACK` | Base for the foreign entry rule (§10.2) and Windows callbacks | L09/N12 |
| `lib/policy.f` | Sealed source lookup, the owner of source admission | Records source-acquisition observations (§23.3) | C04/C12 |
| `tools/native-build-core.f` | Host live readers and target source/window logic intertwined | Explicit build action, retained host reader, target builder and manifest composition | T02/T10; changed-layout generation-1 gate |
| `tools/hb-build-lib.f`, `tools/object-image.f` | Host-derived target naming/selection | Consume resolved target/producer/input identities and link plan | C07/C08/C17 |
| `tools/build-fixpoint.f`, `native-emit.f`, bootstrap/runtime source lists | Several selection owners | One source-manifest authority with generated bootstrap representation | C02/C03; native self-build |
| Existing object/cache/build libraries | Existing package design should be reused | Contribution storage/import and observation hooks from P1, not parallel format | C01–C18 |
| New `src/arch/wasm/` provider | Must not inherit native code-address assumptions | WCFG/WSTRUCT, module contribution plans and binary encoder | W01–W07 |
| Browser runtime packages/host adapter | Existing v2 owns UI/GPU/capabilities | Pin HBR2 registry and exact wrappers; portability sidecar only | W08–W19/W27–W30 |
| Tool and test entry points | Must remain portable and inspectable | Thin Habu commands; external tools as adapters, independent tests as oracles | Same commands on all qualified hosts |

### 26.1 Minimal practical source layout

Use existing `src/arch` rather than a competing `src/targets` hierarchy. Target *profiles* live separately from backend source. The destination can be introduced gradually:

```text
src/compiler/target/     resolved contracts, layouts, compatibility
src/compiler/session/    contexts, ownership, diagnostics, observations
src/compiler/backend/    provider interfaces and pass planning
src/compiler/artifact/   owned contributions and publication plans
src/arch/{arm64,x86-64,arm32,tic6x,wasm}/
src/abi/                 external call classifiers and Habu ABI descriptors
src/host/                executing-compiler OS services
src/object/              standard object codecs
src/link/                symbols, layout, relocations, image policies
src/runtime/             portable primitives plus environment providers
src/build/               actions, input worlds, manifests, caches, tools, runners
profiles/                targets, runtimes, boards, toolchains, qualification
host/browser/            existing v2 closed adapter and workers
```

`RUNTIME`, `UI`, `SCENE`, `RENDER`, `SYNC`, `BROWSER` and `MAKI` retain v2's package boundaries regardless of physical directory placement. `src/runtime` does not mean all those packages become mandatory dependencies of a small native application.

### 26.2 Dependency lint rules

Compiler HIR and generic artifact modules may depend on target contracts but not import a particular OS seam or machine emitter. ISA construction cannot import a browser/COM/UI package. Host services cannot select target image policy. The link format codec cannot discover libraries through the host's default loader. A runtime profile cannot cause application roots to retain every compiler backend.

Lint generated source manifests as well as textual `require` edges: dynamic source selection is exactly where the old architecture could conceal coupling. Bootstrap's generated static list is checked against the same manifest authority. Fail closed when an unclassified dependency is introduced into a cross-build closure.

## 27. Work packages, dependencies and landing gates

The order below is an implementation dependency graph, not a demand to stop Intel work until a repository-wide redesign finishes. Shared changes land with compatibility adapters and native regression gates. New Windows, TI and Wasm work can proceed behind those interfaces.

P0 and P1 are split in two. P0n pins the native contracts and P0w the HBR2 wire and Wasm numeric goldens. P1a is the one commit that makes every sealed edit the Wasm path needs (§5.3); P1b is the rest of the action and target model. The Wasm path is P1a -> P6 -> P7, with P0w alongside; it never waits on P2-P5. Each package names its dot. A bracketed [P1] elsewhere on this page cites the package-build design, not the P1 work package.

| Work package | Concrete deliverable | Depends on | Exit gate |
|---|---|---|---|
| P0n — pin native contracts | Literal CTARGET digests, primitive and layout goldens (habu-pin-native-target-12e4fbb7) | None | Old behavior reproducible on currently qualified hosts; unknown results labelled untested |
| P0w — pin HBR2 wire and Wasm numerics | HBR2 wire fixtures, the registry and its digest, Wasm numeric goldens (habu-pin-hbr2-wire-0b340032) | This page | Wire goldens; N01–N06 with the NaN print rows and the three `?do` rows, run natively |
| P1a — Wasm target row | Wasm architecture and ABI codes, the scalar-FP capability split (§4.4) and the decoders, in one sealed commit (habu-add-the-wasm-4c32353e) | P0n | T01; legacy digests unchanged |
| P1b — action/target model | ExecutionPlatform, CompilerProduct, ResolvedTarget, alias resolver, separate compatibility predicates (habu-resolve-build-targets-ae8e65c1) | P1a | T01–T06, T15; no changed legacy digest meanings |
| P2 — provider/session adapter | Backend manifest capacity, context-owned artifact adapter, exclusive legacy-provider guard (habu-give-each-backend-b6f7ea4f) | P1b | T07–T09; ARM64/Intel failures leave parent state valid |
| P3 — target data/staging | Symbolic target refs, object builder, host/helper binding split, adapter admission (habu-make-cross-target-934166cc) | P2 | T10–T14/N07–N11; no foreign artifact host pointers |
| P4 — contribution integration | Existing P1 package profile, source-position importer, observation/producer hooks; scheduled by the package-build review (habu-review-and-schedule-dbdba551) | P3; that review | C01–C18 including transitive compile-time closure |
| P5 — link/publication | Native fragments, relocation providers, layout fixpoint, native prepare/commit/retire (habu-link-native-fragments-5949ec30) | P2–P4 | L01–L06/L10/L11; existing image/capture qualification retained |
| P6 — Wasm vertical slice | Genuine Habu-generated scalar/control/memory module through the shadow route (habu-emit-a-wasm-05443776; [wasm-backend.md](wasm-backend.md)) | P1a, P0w | W01–W07 and numeric parity; proves non-native result model early |
| P7 — HBR2 AOT integration | Exact wrapper/codecs, callback certificates, ReleaseManifest sidecar, actual browser host (habu-bind-the-wasm-5e9830c7) | P6; P3 only for target data the capture path cannot carry; the HBR2 runtime packages | W08–W19/W27–W30 on selected real browser profiles |
| P8 — host/toolchain services | Structured paths/processes/files/memory/tool/runner APIs for native hosts (habu-own-host-svcs-b1368bbe) | P1b; grows with P5 | Cross-generation on each brought-up host; no host library contamination |
| P9 — Windows native profiles | x64/ARM64 foreign ABI, PE/DLL/unwind/TLS/callback and host runtime (habu-specify-windows-x64-3cd4cef3, parked until [WN] and a Windows lane exist) | P5/P8; [WN]; existing ISA lane output | L07–L11/N12, native compiler self-build per Windows host |
| P10 — TI/freestanding profiles | ARM32/C66x complete lowered subset, startup/BSP, object/finalizer adapters (design input to C6's habu-design-the-first-61f718f0) | P3/P5/P8 | L12–L20 and device/independent execution evidence |
| P11 — compiler product qualification | Native cross-bootstrap graph, source-free/self-build, cache equivalence and size gates (habu-qualify-the-five-a49d428e) | Relevant P4/P5/P8/P9 | All five required native compiler-host profiles qualified |
| P12 — optional browser compiler | Installer provider, restricted transaction, leases, explicit compiler pump; deferred until a caller exists | P7/P11 where reused; v2 compiler profile | W20–W26 and real compiler B0/B1/B2 closure |
| P13 — cleanup/optimization | Retire compatibility predicates, move files, optimize measured bottlenecks (habu-retire-target-selecting-affc4d65) | Affected profiles have replacement gates | No hidden fallback; ownership lint and performance receipts |

P4 gates incremental browser builds only. P7 needs no package cache, because the first linker relinks the whole unplaced set (§13.3).

P11 does not require P10 or P12 to prove that a host compiler executes correctly. A release claiming TI emission additionally needs P10. A release claiming browser self-hosting needs P12. Keep capability/evidence records granular.

### 27.1 Preserve the Intel lane

The Intel lane retains the Linux x86-64 backend and native self-build work. Its pin 7554c01d is unmerged: it carries 14 commits master lacks, among them 5c8459cb "Compile definer bodies through NCOMP", 9751ac70 "Publish definer bodies without checker facts" and 8b05f5d9 "Key the hb-build producer by its load closures", which touch the sealed compiler and the build producer that P1a, P2 and P4 also edit. Shared-interface owners provide small adapters and reviewed receipts; they do not replace the lane's selected register convention with an older design's alternative. Windows ABI work is a separate classifier/runtime effort over the same x86 backend.

For a change that affects files loaded by macOS, obtain the existing macOS integration gate rather than calling a Linux-only run universal qualification. Track exactly which host/source/compiler pair ran. No new CI infrastructure is presumed to exist; native local/remote runners can provide receipts under the same test contract.

Avoid simultaneous mechanical moves and behavior changes across active worktrees. Land new context types and adapters first, move architecture files separately, then remove deprecated globals after the last supported caller is migrated.

### 27.2 Required handoff receipt

Each work package records public stack interfaces, imported contracts, owned resources, state machine, target/profile coverage, failure paths, quotas, test IDs, baseline/source commit, effective producer identities and exact outcomes. A source-only design patch says “specified”; generated Habu output says “generated”; actual target execution is attached separately.

Schema changes include all readers/writers, validators, fixtures and version-policy updates. No worker independently assigns a conflicting AOT profile version or HBR2 opcode. A task requiring a board-specific register map or external ABI table cannot close based on a guessed value.

### 27.3 Definition of done

Completion for a claimed profile means the ordinary build command uses the new model; no hidden host predicate selects target semantics; no fallback native interpreter/JIT rescues an advertised AOT output; package hits and misses preserve source semantics; required target runtime/ABI/image behaviors execute correctly; and artifacts carry reproducible inputs plus honest qualification receipts.

Moving files, registering a backend row, producing a valid ELF/Wasm header, or achieving a self-build hash match is individually insufficient.

## 28. Decisions fixed here and integration inputs still required

This section separates fixed architectural decisions from deployment facts that cannot safely be invented.

### 28.1 Decisions fixed by this design

The action model separates executing platform, compiler-product host and emission target. Habu cells remain 64-bit in the initial target family. The generic result is an owned emission/contribution, not a native code address. Shared HIR reuse is dependency-sensitive. Cross-target persistent references are symbolic. Native and Wasm publishers have different preparation mechanisms but explicit commit/retirement contracts. Package contributions preserve source order and reuse the existing AOT-family package design.

Browser integration uses **HBR2 2.0**, its registry digest once the registry exists, two imports, six wrapper exports, one unshared memory and the exact status/ownership rules of [wasm-backend.md](wasm-backend.md)'s HBR2 binding. No new portability-specific browser runtime replaces v2. Optional compiler installation is separately admitted; it does not assign new core operations in this document.

The Wasm fast-call/frame threshold, 16 input and 16 output lanes ([wasm-backend.md](wasm-backend.md)), is a **pinned internal ABI parameter**, not an inherited browser wire value. It must enter the internal ABI descriptor and implementation fixtures. Changing it later requires compatible adapters or an ABI revision; it is not an untracked optimization heuristic.

### 28.2 Facts to pin during implementation—not opportunities to guess

| Required integration input | Authority and treatment |
|---|---|
| Current exact shift/conversion/cleanup precedence semantics | Read current primitive/source contracts and freeze execution fixtures before changing lowering |
| AOT family/profile version number | Allocate through the existing artifact owner at integration; old readers reject unknown profiles |
| Backend/ABI wire codes | Append through the authoritative registry; preserve legacy meanings |
| ARM32 CPU/FPU/PCS and C66x device variant | Selected target profile backed by official ABI/device documentation |
| Board memory map, reset/boot state, interrupt/cache/DMA policy | Explicit BSP/board manifest; refuse firmware qualification without it |
| SDK libraries, import metadata and vendor finalizer support | Pinned content/tool profiles and actual host execution evidence |
| Optional compiler-service transport schema | Separate reviewed extension of the existing compiler embedding, not invented HBR2 core IDs |
| Browser support set, CSP, graphics fallback and budgets | Deployment manifest backed by actual browser/device tests |
| Producer equivalence across different hosts | Explicit qualified equivalence policy; conservative exact-producer cache mode until then |
| Runtime state migrations and offline authority | Existing v2/application owners; never inferred from pointer or schema similarity |

This is not permission to leave ordinary implementation behavior undefined. Each missing fact has a designated owner, a consuming descriptor and a fail-closed gate. Application/library code generation can proceed for narrower admitted profiles without overstating full platform qualification.

### 28.3 Principal residual engineering risks

The largest risk is hidden compile-time access to live dictionary memory or untracked native operations. Address it through owner adapters and enforced call/effect admission, not retrospective pointer scanning. The next is cross-owner publication and cleanup: dictionary, checker, registry and code-generation state must share a real commit boundary.

C66x scheduling/ABI correctness, native unwinding and Windows callbacks, and complete Habu browser self-hosting are substantial implementation efforts even with clean interfaces. They must retain independent validators and staged execution gates. The architecture reduces duplication; it does not eliminate target-specific engineering.

Reproducibility can also fail through unordered tables, floating compile-time evaluation, path-sensitive source, generated timestamps, nondeterministic IDs or toolchain metadata. Record and test each observed dependency. Do not weaken numeric/source semantics or discard mismatches to make an equality test pass.

### 28.4 Review boundary

This design checks its integration contracts against [wasm-backend.md](wasm-backend.md), HBR2 and [package-build.md](package-build.md). It does not claim that HBR2's runtime is reimplemented, that its state machines are independently reproved, or that every repository branch is audited. The Windows/COM design [WN] does not exist, so the Windows rows state requirements only.

Habu generates the HBR2 registry, `lib/browser/hbr-v2-registry.json`, from HBR2's tables and pins its own digest (P0w; [wasm-backend.md](wasm-backend.md) §12.1). The digest HBR2 reports cannot be reproduced, because HBR2 does not publish the registry's JSON, so it stays attributed to HBR2. A qualified release imports the registry and checks its digest.

## 29. Sources and provenance

A cited source establishes an existing fact. Every algorithm, new type or path,
proposed command and work package not attributed to one is a decision of this
design.

### 29.1 Design documents

| Label | Document | Status |
|---|---|---|
| [B1] | [wasm-backend.md](wasm-backend.md), the Wasm backend design, first written against aed8416b | In Habu, reconciled with this page |
| [B2] | Habu Browser Runtime, consolidated design, revision 2 (HBR2), 2 October 2026 | It is [docs/browser-runtime.md](browser-runtime.md). HBR2 reports the digest of its registry, `hbr-v2-registry.json`, as `a2c0e4e513d448fc1ecc4fbc28b3b4aff5210cb7d85b9c923d97e8be40500599`; Habu generates the registry from its tables and cannot reproduce that digest ([wasm-backend.md](wasm-backend.md) §12.1) |
| [P1] | [package-build.md](package-build.md), the package incremental-build design, baseline 37b2c1b7 | In Habu, reviewed only at its interface with this page; its full review is habu-review-and-schedule-dbdba551 |
| [WN] | A Windows/COM design | Does not exist; open future work |

### 29.2 Repository references

| Label | Source | Observation |
|---|---|---|
| [R1] | `src/compiler/target.f` | Immutable five-field contract, stable identity, backend registry of four rows (`:428`) |
| [R2] | `src/compiler/native/compiler.f` | Per-definition orchestration, source-selected passes, native imports, mutable session scratch |
| [R3] | `src/compiler/native/emission.f` | Byte-oriented sealed emission facts and architecture identity (`:1-24`) |
| [R4] | `src/compiler/native/shadow.f` | Second-target emission, copied rows, host-address target mapping (`:1-30`) |
| [R5] | `src/core/cell.f` | Eight-byte Habu cell and live-engine width check |
| [R6] | `src/habu/arith-abi.f` | Wrapping arithmetic, divide-by-zero −6400, `MIN-N -1 /` (`:4-9`, `:23-24`, `:29`) |
| [R7] | `src/arch/tic6x/facts.f` | Admitted instruction-resource/dependence subset; shared scheduler/simulator facts are not independent proof |
| [R8] | 16a4d57b "Refuse recordless checker symbols" | Live-record lookup and refusal/publication |
| [R9] | `tools/native-build-core.f`, `src/compiler/native/abi.f` | Retained host layout reader; machine-specific ABI and source selection |
| [R10] | `tools/build-target.f`, `tools/hb-build-lib.f` | The build-target cell and alias selection; artifact naming |
| [R11] | [porting.md](porting.md) | Source-list owners, OS/image seams, primitive and native-port gates |
| [R12] | [INTEL.md](../INTEL.md) and the unmerged Intel pin 7554c01d | Linux x86-64 lane ownership, cross-build, self-build and recovery direction |

### 29.3 External references

| Label | Primary source | Used for |
|---|---|---|
| [MS-X64] | [Microsoft x64 calling convention](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170) | Positional argument registers, shadow space and unwind boundary facts |
| [MS-ARM64] | [Microsoft ARM64 ABI conventions](https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170) | Windows ARM64 platform/ABI distinctions |
| [MS-ARM64EC] | [Microsoft ARM64EC ABI conventions](https://learn.microsoft.com/en-us/cpp/build/arm64ec-windows-abi-conventions?view=msvc-170) | ARM64EC is a distinct ABI profile, not native ARM64 by alias |
| [PE-COFF] | [Microsoft PE/COFF format](https://learn.microsoft.com/en-us/windows/win32/debug/pe-format) | Object/image, relocation, import and metadata format boundaries |
| [AAPCS32] | [Arm AAPCS32](https://github.com/ARM-software/abi-aa/blob/main/aapcs32/aapcs32.rst) | ARM32 procedure-call convention and variant ownership |
| [WW1] | [WebAssembly module structure](https://webassembly.github.io/spec/core/syntax/modules.html) | Core module import/export/memory/table/initialization structure |
| [WW2] | [WebAssembly JavaScript interface](https://webassembly.github.io/spec/js-api/) | i64/BigInt embedding and memory/view/host-call semantics |
| [WW3] | [WebAssembly binary module format](https://webassembly.github.io/spec/core/binary/modules.html) | Section/body framing and variable-length index encoding |
| [WW4] | [WebAssembly tool conventions, linking](https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md), and [LLD Wasm documentation](https://lld.llvm.org/WebAssembly.html) | External relocatable-object conventions; not a production dependency choice |
| [WW5] | [WebAssembly module execution/instantiation](https://webassembly.github.io/spec/core/exec/modules.html) | Active initialization and start-function effects relevant to safe publication |

External pages were consulted on 2 October 2026. Editor's drafts describe their
specified behavior, not universal browser availability. Implementations pin the
exact versions and subsets and verify them in their qualification environment.
No external source is cited as proof that a proposed Habu algorithm is
implemented.

### 29.4 Provenance

This page is PA-r2, the portability architecture revision of 2 October 2026,
written against master 16a4d57b and adopted on 2026-10-03 after a review against
67a66d28 that corrected its claims about master. PA-r2 says it replaced an
outline of 16,928 bytes; that outline is not the predecessor design imported
for the review (139,590 bytes), so §0's failure list was written against the
short outline. The worked-example checks made with PA-r2 (field layouts,
checked bounds, integer boundaries, relocation range, canonical encoding and
publication ownership) ran as reference models outside Habu: they execute no
Habu, compile no proposed API and test no emitted code. Qualification is the
gates of §§25–27.

## Appendix A. Worked end-to-end build traces

### A.1 Windows ARM64 compiler built on macOS, then used for TI

The current macOS compiler resolves a `CompilerProduct` whose executable target is Windows ARM64. Compiler packages—including C66x instruction selection/encoding source—are built as Windows ARM64 program code. Native compile-time helpers remain macOS code or execute as separately admitted host tools. Windows target foreign metadata is read, not loaded as host DLLs. Contributions assemble into PE, with Windows host services and compiler roots retained.

A Windows runner executes that exact PE and records startup/self-build results. That executable then resolves a C66x application target, board profile and target library/finalizer plan. Its C66x backend executes as Windows ARM64 code and emits C66x objects. Missing local vendor finalization does not invalidate object generation, but it prevents claiming a fully local final firmware workflow until a qualified finalizer exists.

No step requires a compiler executing on C66x. No x86/Linux register or syscall convention is selected because Intel was the first completed non-ARM64 lane.

### A.2 Windows x86-64 compiler builds an HBR2 DOM application

Resolve `wasm32-habu`, cell64/address32, browser environment, HBR2 DOM runtime and AOT browser-release output. The application package closure uses v2's portable runtime/UI code without pulling in scene/GPU/SYNC packages unless referenced. The production schema generator emits codecs/wrappers from the pinned HBR2 registry. Host-executed compile-time code constructs symbolic guest data and never touches DOM APIs.

Wasm contributions are linked by symbol/type/function identity; final indices and LEB sizes are assigned before module encoding. Admission checks exactly the selected wrapper/import/memory/profile requirements. The release binds `app.wasm`, host adapter, assets, schema, callback evidence and source map. A browser boot validates that release, instantiates without a core start function, reserves/copies bootstrap input and drives the six wrapper exports serially.

If `submit` returns Backpressure, Habu retains the packet and does not advance accepted-send credit. If an asynchronous host request later fails after Accepted, the host still owns the request and returns its typed terminal outcome. Neither result becomes an arbitrary Habu throw code.

### A.3 Interrupted incremental browser compilation

A compiler job records a new definition against root D7 and epoch E3. The host compiles its sealed bytes asynchronously. Before preparation resumes, an AOT reload creates epoch E4. The old ticket is rejected/drained under E3; it cannot publish into E4 even if the numeric table slot is free.

In a separate run without epoch change, allocation grows capacity but a later validation fails. The candidate never becomes dictionary-visible, its reservations are cleared/tombstoned, and D7 remains the semantic root. Increased capacity stays charged. The receipt does not claim that browser memory/engine caches shrank back to their original physical state.

### A.4 Package cache and compile-time transitive execution

P contains a runtime call to Q. R executes P while constructing target data. The first build records Q's implementation in R's compile-time dependency closure. A later body edit in Q may leave P's call contract unchanged, but it invalidates R's cached data. R recompiles at its original source occurrence; unaffected later contributions may still hit if their own observations remain valid.

Adding a new higher-priority include candidate similarly invalidates only the relevant resolution witnesses and dependent units. No cache importer exposes a future private declaration early just because it is convenient to topologically sort all packages.

## Appendix B. Adding one more platform without duplicating everything

For a new **ISA**, define machine/features, internal ABI/layout, legalization, native/Wasm-like emission kind, relocations, primitive bodies, validators and generation/execution gates. Do not add an OS merely to carry an encoder.

For a new **OS on an existing ISA**, implement its foreign ABI variants, execution-host services, target runtime/entry/loader policy and image finalization. Reuse the ISA encoder/lowering where constraints genuinely match. Qualify callbacks, TLS, unwinding, memory permissions and process/path behavior; a hello-world executable alone is insufficient.

For a new **runtime embedding**, specify capabilities, import/export ABI, owned data/handles, concurrency, errors, startup/shutdown, resource limits and admission. Reuse the target backend. Browser Runtime v2 is the existing example; a versioned WASI adapter is a separate embedding, not a rewrite of Wasm instruction selection.

For a new **board**, supply a reviewed BSP/memory/boot/interrupt/cache/DMA profile and deploy/runner evidence. Reuse ISA/ABI/object layers unless the board actually introduces a new constraint.

For a new **object/image format**, add a codec/finalizer and map the relevant relocation/metadata subset. Do not make the frontend understand binary-file headers. Unsupported imported sections remain explicit errors until implemented.

In every case, update the resolved profile, source manifest, capability record, identity schema and independent tests. Existing hosts should need no target-specific execution changes merely to generate the new target's bytes.
