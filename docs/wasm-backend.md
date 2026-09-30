# Habu WebAssembly Backend

**Status:** proposed implementation design, not an implemented or qualified backend.  
**Date:** 29 September 2026.  
**Source baseline:** `joelreymont/habu`, `master` at `aed8416b42d4e3bfd95edad708c30702a8071145` (28 September 2026, 20:40:59 UTC).  
**Scope:** direct checked-Habu-to-core-WebAssembly compilation, browser embedding, a path to the interactive runtime and self-hosting.  
**Primary decision:** memory32 with 64-bit Habu cells; reuse the checker, recorded tape, elaborator and frozen HIR; emit a dedicated Wasm IR and final binary in checked Habu. No LLVM, MLIR, Emscripten or Binaryen dependency in the production compiler.

The browser / NG architecture motivates the first application, but the compiler must remain application-independent. A browser library is the first delivery, not a permanent restricted replacement for Habu.

## 1. Decisions at a glance

| Concern | Decision |
|---|---|
| Initial product | Ahead-of-time compiled browser libraries, callable through a small JavaScript host adapter |
| Long-term product | Checked interactive Habu, including browser-hosted compilation and a measured self-hosting fixed point |
| Architecture row | Add `wasm`; use address width and an explicit feature profile to distinguish memory32 and future memory64 |
| External target label | Proposed `wasm32-habu`; this is not an existing command-line option |
| Language cell | 64 bits; `CELL = 8`; do not shrink `n`, products, masks or dictionary cells |
| Linear-memory addressing | 32-bit offsets, not native process pointers |
| Cell-resident pointer | Canonical zero-extended offset in an eight-byte cell; narrowed only after validation |
| First backend representation | Existing HIR values mapped to Wasm locals, with explicit typed structured control |
| Internal calls | Typed parameters and multi-value results, including a status class; hidden context parameter |
| Dynamic word calls | Checked execution-token descriptors and uniform context-taking adapters |
| Catchable exceptions | Explicit status propagation and full 64-bit throw code in context |
| Fatal failures | Diagnostic plus Wasm trap / discarded execution instance, not a catchable language error |
| Host interop | Versioned narrow imports; byte spans and generation-checked resource handles |
| Browser async | Explicit request/completion and resumable top-level execution; no transparent promise blocking |
| JIT-style compilation | New immutable Wasm modules published transactionally by the host |
| Optimizer | Habu-owned; engine native optimization is the embedding's job |
| First deployment | Single worker, one memory, scalar instructions plus multi-value; no mandatory threads, GC, memory64 or JSPI |
| Proof posture | Preserve checker and ownership obligations; validate every stage; do not equate valid Wasm with semantic correctness |

## 2. What actually exists in the audited source

These are observations of the pinned revision, not assumptions inherited from older design discussions.

* `src/compiler/target.f` owns an immutable architecture/ABI/endian/address-width/features contract, stable field encodings and the backend registry. It separates a coherent target description from a loaded implementation. `wasm` is not among the inspected architecture variants. [R2]
* `src/compiler/native/backend.f` (`NBACK`) dispatches declaration, selection, rewriting, emission and lifecycle stages through the target row. The emission interface still accepts a numeric native code location. [R3]
* `src/compiler/native/compiler.f` (`NCOMP`) consumes checker-observed source tape and publishes a pending definition only after the compilation chain succeeds. Its imports still load native ABI/publication machinery and ARM64 passes. [R4]
* `src/compiler/native/elaborate.f` holds compile-time cell vectors with grouping information for wide values. Stack renaming and the admitted return-stack operations move value identities rather than implementing a runtime stack. Quotation bodies become functions. [R5]
* `src/compiler/native/hir.f` has 47 opcode ordinals in the inspected section, including arithmetic, floating point, memory, branches, calls, quotations, traps and termination. Header comments saying “straight-line” or “aarch64” are not a reliable description of all current code: the operative `TARGET` word reads the immutable binding. [R6]
* `src/compiler/native/frozen.f` reads frozen functions, blocks, values, operands and predecessor edges. This is a usable backend input boundary. [R7]
* `src/compiler/native/publish.f` explicitly consumes A64 emission, four-byte instructions, code windows, native relocation discovery and dictionary retargeting. It is not a portable publisher. [R8]
* `src/core/quotation-storage.f` distinguishes native image DATA addresses and records quotation relocations. This must acquire a target-specific storage strategy rather than being reused unchanged. [R9]
* `src/habu/arith-abi.f` says addition/subtraction/multiplication wrap, division by zero throws `E-DIV-ZERO = -6400`, and `MIN-N -1 /` wraps to `MIN-N`. These are settled contracts; old Intel handoff text reporting crashes is not the current authority. [R10]
* The source guide says quotations do not capture surrounding locals, package redefinition requires explicit `undefine`, and new unchecked escape hatches are not an acceptable substitute for missing checker support. Genuine engine/host boundaries belong in the current primitive mechanism. [R11]

The correct assessment is therefore: **a meaningful shared frontend already exists; the native runtime, representation and publication boundaries remain substantial work.** This is not “start a new compiler,” and it is not “add an instruction encoder.”

## 3. Product profiles and release boundaries

Use one compiler implementation with three admitted product profiles.

### 3.1 Library profile

Compile a closed set of checked entry points and their reachable dependencies. No REPL, filesystem, arbitrary `evaluate`, native FFI or live dictionary mutation is required in the guest. Ship a normal `.wasm`, ABI manifest and small generated or handwritten JavaScript/TypeScript adapter.

Initial NG candidates are protocol decoding, deterministic identifier handling, transforms, mesh operations, selection, bounding-volume queries and local validation. Exact solid modeling stays on the server initially. These are proposed application boundaries, not prerequisites of the compiler.

### 3.2 Runtime profile

Add the portable dictionary, checked execution tokens, supported defining words, exception behavior, explicit source evaluation and a resumable top-level driver. A browser host installs compiled modules; a non-browser embedding supplies equivalent capabilities.

### 3.3 Compiler profile

Compile Habu's checker and Wasm compiler themselves to Wasm, execute compile-time operations through the portable runtime, and rebuild the compiler under a pinned environment. This profile is only declared complete after the actual source dependency closure and fixed-point gates pass.

Maintain a machine-readable admission matrix for source constructs, HIR operations, primitive contracts and runtime services. Every row says `supported`, `lowered via helper`, `host capability required`, or `refused`, with a test. No silent native fallback is possible in a browser.

## 4. Target identity and numeric capabilities

### 4.1 Separate cell width from address width

The first target preserves Habu64 source semantics while addressing memory with `i32`:

```text
architecture        wasm
ABI                  habu-wasm-cell64-v1
address width        32
cell width           64, fixed by the ABI/layout identity
byte order           little
pointer cell slot    8 bytes, upper 32 bits zero
memory count         1
shared memory        false
feature baseline     scalar core + multi-value
```

A 64-bit integer does not require memory64. WebAssembly has distinct numeric value types and memory address types. [W1, W2]

Keep `ptr-width` about the actual target address width. Define the eight-byte Habu pointer *storage slot* in the ABI layout; never let generic code infer `CELL` or pointer-slot storage solely from address width. A future packed four-byte pointer-field ABI is a separate layout, not a silent optimization.

Append architecture, ABI and feature wire codes without renumbering existing values. Update all exhaustive matches, validation, decoding, registry capacities and golden identities. Put cell layout in the new ABI's versioned identity. Preserve existing native contract digests when their meanings have not changed.

### 4.2 Repair the floating-point capability boundary

Current `F-FP` includes fused multiply-add, while `HIR:FP-TARGET` requires it for ordinary floating-point operations. [R2, R6] Do not set that bit just to get Wasm float schemas admitted.

Add a capability for ordinary scalar IEEE operations. Have the old stronger feature imply the new capability in a documented capability query, without changing old wire encodings. Make ordinary float HIR require scalar floating point; a fused operation must require a separately justified fused capability. Bump the affected HIR schema version and update native tests deliberately.

Base Wasm arithmetic has no scalar fused multiply-add instruction. Do not substitute multiply followed by add where one rounding is required. A correctly rounded software helper is an explicit later implementation; otherwise reject the operation. [W2]

### 4.3 Feature and deployment identity

Create a closed `WPROFILE` describing permitted Wasm features, import ABI, maximum arities/locals/module bytes, and deployment resource limits. Bind code generation to a composite identity including:

```text
existing CBIND digest
Wasm feature-profile digest
Habu ABI-layout digest
runtime ABI/version digest
host-import signature digest
source/IR identities and compiler pass versions
```

Semantic settings must never hide in mutable globals. Operational quotas may be separately signed/recorded deployment policy, but any quota or instrumentation choice that changes generated bytes enters artifact identity.

The current upstream specification identifies itself as WebAssembly 3.0, dated 21 September 2026. That does not imply every requested browser supports every instruction. Qualify the exact selected subset with feature probes and browser execution tests. [W1, W3]

## 5. Compiler pipeline

```text
Habu source + permitted compile-time environment
  -> existing checker and owned source tape
  -> existing elaborator
  -> frozen HIR with target-symbolic addresses
  -> Wasm legalization and call/effect lowering
  -> WCFG: typed Wasm-level control-flow graph
  -> WSTRUCT: typed structured control
  -> local assignment and conservative stack scheduling
  -> sealed Wasm function/module plans
  -> Habu binary encoder
  -> independent validation
  -> artifact publication
```

Keep `WCFG` and `WSTRUCT` small closed dialects on the existing IR substrate. They do not become a universal instruction IR and do not contain A64 registers. Reuse immutable builders, nominal IDs, source spans, schemas, digests and ownership validation.

### 5.1 WCFG content

The lowered graph must make these explicit: scalar type and lane grouping, block parameters, direct symbol calls, dynamic adapters, memory operations and ordering, checked address conversion, error status propagation, cleanup edges, terminal failures, tail-call intent and source provenance.

Preserve nominal and linear type facts in proof/validation metadata even when multiple Habu types share `i64` as their physical representation. Physical equality is not source-type substitutability.

### 5.2 Compile stack code into values

`dup`, `swap`, `over` and checked local references should normally rearrange compiler value IDs. They must not imply pushes and pops to linear memory. The inspected elaborator already does this. [R5, R6]

Start with one Wasm local per lowered live value. Emit `local.get`, the operation, and `local.set`. Then add safe local reuse and straight-line expression scheduling. This deliberately chooses an easy-to-check first representation over a difficult optimal operand-stack scheduler.

There is no physical register allocator in this backend. The Wasm engine owns native register allocation. Habu still owns local lifetime, excessive local counts and code size.

### 5.3 Structured control and joins

Retain source region hints where available, but validate them against the frozen graph; do not trust lexical annotations after graph transformations. Lower reducible graphs using dominance, loop headers and an explicit control tree. Track nominal label IDs until final emission; only then compute branch depths.

Map `if`/`else`, loop exits and loop backedges to typed `if`, `block`, `loop` and branches. Wasm branches target enclosing labels; loop branches and block branches use different continuation points. [W4]

Lower block-argument transfers as parallel copies. For an edge swapping two live values, copy both old values into temporaries before assigning destination locals. Sequential copies without cycle handling are a miscompile.

Reuse the frontend's meaning of `DO`, `?DO`, `+LOOP`, `LEAVE` and `UNLOOP`; do not recreate Forth loop arithmetic as a naive `index < limit` test.

Initially refuse irreducible graphs with source diagnostics. A later checked dispatcher-loop lowering can admit genuinely irreducible regions, but must be explicit in optimization reports. Do not use a program-counter dispatcher for every ordinary function as the permanent main strategy.

### 5.4 Return-stack and tail-call behavior

The admitted balanced return-stack operations already lower into compile-time value movements. Preserve that path. Wasm's operand stack and protected call stack are not Habu's source-visible return stack. Any source construct requiring additional runtime return-row behavior needs an explicit model or a refusal.

Self tail recursion becomes a Wasm loop. Before claiming full native-runtime parity, implement bounded-space mutual tail calls through a checked trampoline or qualify a tail-call feature profile. A normal `call` followed by `return` is not a bounded-space implementation of arbitrary tail recursion.

## 6. Representation and memory

### 6.1 Values

| Habu meaning | Wasm execution representation | Stored / dynamic-stack representation |
|---|---|---|
| `n`, `i64`, `u64`, one-cell integer nominal | `i64` | 8 bytes |
| Default one-cell real | `f64` where HIR proves it; otherwise preserved cell bits | 8 IEEE bytes |
| Proven narrow numeric value | `i32`/`f32` only through explicit representation lowering | Existing declared layout |
| Boolean/mask | `i32` predicate locally; canonical Habu mask when observable | 8-byte Habu flag |
| `ptr T` | Validated memory offset; may stay `i64` until an access | 8-byte canonical offset cell |
| Product / wide value | Fixed ordered tuple of lanes | Existing target-layout cell sequence |
| Tagged value | Discriminant and fixed payload layout | Existing family layout; initialized padding |
| Execution token | Nominal 64-bit descriptor handle | 8 bytes, never a code address |

Do not infer signedness from Wasm types: integer operations select signed or unsigned behavior explicitly. Retain source roles until those operations are selected.

### 6.2 Offset conversion

A load through a pointer cell must establish:

```text
p is a canonical target offset
0 <= p <= UINT32_MAX
n is nonnegative
p <= memory_length
n <= memory_length - p
required application region / capability permits the access
```

Perform size and overflow checks in sufficiently wide arithmetic before `i32.wrap_i64`. `memory.size` is in pages; widen before converting pages to bytes. Pointer addition must not wrap back into the start of memory unnoticed.

Distinguish compiler-host pointers, guest offsets, external host-resource handles, source symbol IDs and execution-token IDs. A native address appearing as an integer literal is not a target relocation.

### 6.3 Allocation and runtime regions

Use one non-shared memory with explicit regions for static data, runtime metadata, contexts, dynamic stacks, arenas and heap blocks. Make arenas and the allocator obey configurable ceilings. An illustrative deployment can start with 1 MiB and cap at 256 MiB; these are configuration examples, not universal Habu limits.

Reserve a low null/sentinel region, but recognize that this is not an unmapped guard page. Raw Wasm accesses to address zero can be in bounds. Enforce the reservation through admitted pointer operations / runtime checks where Habu's contract requires it.

Reuse checked allocation algorithms after replacing `mmap` and native growth seams with a Wasm memory provider. Define allocation failure and failed growth paths before implementing resize. Reacquire JavaScript memory views whenever the buffer identity or extent changes. Fixed-length unshared ArrayBuffer views can be detached by growth. [W5, W6]

Neither Wasm memory bounds nor the checker alone proves absence of use-after-free or an overwrite of another object in the same linear memory. Do not expand Habu's safety claims on the strength of Wasm validation. [W7]

## 7. Calling conventions

### 7.1 Fast internal calls

Use typed functions for fixed checked effects:

```text
(ctx: i32, flattened inputs...) -> (status: i32, flattened outputs...)
```

`status = 0` means success; `status = 1` means a catchable Habu throw. The actual throw code lives in `ctx.throw-code: i64`; it must not be truncated into the status class. Output lanes on failure are defined zero placeholders and are semantically invalid. Generated callers test status before using output values.

The untouched row-polymorphic prefix stays in the caller. Monomorphize only the concrete shape/layout needed by each call, not every possible deeper data-stack prefix. Preserve wide-value groupings and linear moves across the ABI.

For the first implementation, retain existing HIR lane representations rather than inventing aggressive cross-function type recovery. Fast type-specialized helpers can use native `f64` lanes when justified. Large arities use an explicit call-frame variant selected by a deterministic ABI rule and encoded in the function's signature descriptor; never silently choose a different ABI based on a compiler heuristic.

Proven non-throwing internal functions may later omit status through an explicitly distinguished internal ABI variant. This is not necessary for the first correct backend.

### 7.2 Stable dynamic adapters

Dynamic calls use a uniform adapter:

```text
(ctx: i32) -> status: i32
```

A per-context data stack carries eight-byte cells. An adapter validates the required depth and room for results, reads arguments, invokes the typed implementation and commits the resulting stack shape on success. This is the ABI for `execute`, stored quotations, deferred words and evaluator dispatch; it is not the normal cost paid by every direct arithmetic call.

The context includes an ABI version, stack region and top, throw code, diagnostic record, call-frame scratch ownership, execution generation and entry/reentrancy state. Security-sensitive fuel can be held outside guest-writable memory.

### 7.3 Browser-facing exports

Export named library entry points through generated wrappers, not the entire dictionary. A small bootstrap ABI can provide version, context creation/destruction, buffer allocation/free and explicit entry points returning the status class.

Use JavaScript `BigInt` for direct `i64` parameters/results. Use `Number` for `f64`, and validate all pointer/length conversions. Reinterpret exported `i32` address bits as unsigned before indexing views. Prefer batched buffer calls over one import/export call per tiny operation. The standardized JS interface defines BigInt conversion for `i64`. [W5]

No function, product or pointer layout is presumed to match the wasm32 C ABI. A future C-compiled module interop adapter must describe the memory, allocator and aggregate layouts explicitly.

## 8. Exceptions, traps and arithmetic parity

### 8.1 No native unwinder inside Wasm

Native Habu exception primitives can rely on native control transfer. That mechanism must not be ported as pointer arithmetic on a fabricated call stack.

The portable backend gives potentially throwing calls explicit status edges. A `throw` of zero returns normally. A nonzero throw stores its full Habu cell in the context and propagates status through generated cleanup/return paths. An unknown dynamic callee is conservatively potentially throwing; a no-throw certificate is validated against its dependency closure.

`catch` records the caller's stack depth and appropriate runtime marks, executes the adapter, and follows the existing checked catch contract on either result. **Depth restoration is not restoration of overwritten argument values, object ownership, heap contents or host side effects.** Current Habu source and issue records explicitly warn against recovering ownership from overwritten caught arguments. [R12]

Represent exceptional placeholder cells in the checker/IR as specified by the existing catch model. Do not turn zero placeholders into usable copies of a linear resource. Cleanup owners stay in safe outer scopes. Compiler publication transactions and application state transactions provide rollback where required; `catch` does not imply that rollback.

`finally` needs tests for normal return, body throw, cleanup throw and fatal exit; preserve existing cleanup precedence rather than selecting one incidentally.

### 8.2 Fatal failures are different

An impossible family tag, a false no-return certificate, unexpected engine failure or exhausted hard execution budget must not become an ordinary catchable domain error. Habu already distinguishes native terminal traps. [R13]

Write the available diagnostic, trap, and discard/poison the execution instance until explicitly rebuilt. Wasm traps do not roll back linear-memory writes or host side effects. Host JavaScript exceptions from allowed imports must be caught and translated by the adapter where the import contract declares a recoverable failure; unexpected exceptions poison execution.

### 8.3 Arithmetic table

| Operation | Required initial lowering |
|---|---|
| Addition/subtraction/multiplication | `i64` wrapping operations |
| Signed division by zero | Store `-6400` and return Habu-throw status before any Wasm divide |
| `MIN-N / -1` | Return `MIN-N`, not a Wasm integer-overflow trap |
| `mod`, `/mod` | Match truncation toward zero and Habu result order; special boundary remains remainder zero |
| Logical right shift | Unsigned right shift; preserve modulo-64 shift count |
| Comparisons | Wasm predicate -> canonical Habu mask at an observable cell boundary |
| Cell bit reinterpretation | Bit-preserving reinterpret, not numeric conversion |
| Real-to-integer | Match the actual Habu rounding and invalid-input behavior; no blind use of trapping or saturating Wasm conversion |

Wasm signed division traps on zero and the signed minimum divided by minus one. Its comparison instructions return 0/1. These require explicit Habu adaptation. [W2, W4, R10]

Lower a Habu observable true mask as `0 - extend_u(predicate)` where the language representation is all bits set. A raw 64-bit condition must be tested against zero before becoming an `i32` predicate; truncating to the low 32 bits can turn a nonzero condition into false.

### 8.4 Floating point and determinism

Do not claim arbitrary native/Wasm bit identity. Wasm permits more than one NaN result payload for some arithmetic. [W2]

Define a new explicit numeric-policy mode admitting Wasm's IEEE value semantics and permitted NaN outcomes, and bind it into artifacts. Do not silently reinterpret the existing `BIT-EXACT` policy. For a requested stronger bit-exact policy, either prove a restricted operation/input set, supply bit-preserving software semantics, or refuse the unsupported case.

Disable contraction, reassociation and relaxed SIMD by default. A deterministic application protocol may normalize NaNs and reject nonfinite geometry values at its own documented serialization boundary, but that does not change Habu's bit-observation semantics behind the user's back. Transcendentals should use qualified Habu helpers or explicitly named host imports, not unexamined substitution with JavaScript math functions.

## 9. Execution tokens, definitions and reflection

Represent a stored execution token as a nominal handle, for example a 32-bit descriptor slot plus a 32-bit generation in a 64-bit cell. Its descriptor records the adapter's table slot, module/definition identity, full checked effect/layout identity and allowed lifetime. Do not publish raw function indices as stable Habu addresses.

All generic table adapters can share `(i32) -> i32`; consequently Wasm's dynamic signature check alone cannot distinguish Habu word effects, nominal types or linear ownership. Validate these Habu identities before dispatch. Wasm indirect calls provide only the Wasm-level signature check. [W7]

Initial policy: never reuse a published execution-token descriptor/table slot within an execution epoch. Cap growth; fail or rebuild the epoch at the ceiling. A later reclaiming scheme must pin active calls and account for every reachable stored token. Do not guess that a word is unused merely because its name was removed.

Preserve package `undefine` semantics: existing checked references bind definition identity, not whichever word later gets the same spelling. Existing stored tokens must retain their intended old definition or be explicitly invalidated under a documented retirement contract; never silently retarget them. `DEFER` is a deliberately mutable binding and validates replacement effect identity separately. [R11]

Quotations in the inspected language do not capture outer locals. Compile these as noncapturing functions. Do not introduce implicit closures in the Wasm backend. Supported `CREATE ... DOES>` behavior can be represented by a word descriptor with a data object and a behavior identity; this is defining-word implementation, not new lexical-capture semantics.

Code-memory inspection, native CFA arithmetic and code patching are refused or replaced by explicit reflection/debug metadata APIs. User-visible source bodies, names and disassembly can come from retained metadata and original Wasm bytes, not readable engine-native instruction memory.

## 10. Linking, data and binary emission

### 10.1 Link from symbols, not native images

Before lowering, every reachable definition is classified as guest code, guest data, portable primitive, declared host import or unsupported native dependency. Reject native syscalls, `dlopen`, arbitrary host addresses and code pointers with a dependency path to the export that required them.

Use typed symbolic relocations such as function identity, data object plus offset, and import identity. A target data builder produces static bytes from layouts; do not copy a native dictionary, AOT code buffer or snapshot containing native addresses into Wasm.

First link a whole program from frozen HIR objects and target-specific symbolic data descriptions. This avoids inventing a general Wasm object linker before a working backend. Keep final type/function/global/table/data indices deterministic, with imports accounted for in their index spaces. Link-time renumbering re-encodes affected LEBs and body sizes; never patch variable-length immediates as though they were fixed native words.

### 10.2 Encoder contract

The production binary encoder is checked Habu. It emits owned bounded buffers using checked unsigned and signed LEB128 routines, typed opcode constructors and measured function/section sizes. The WAT renderer is a diagnostic view of the same sealed IR, not an intermediate compiler dependency.

Validate exact counts, immediates, block signatures, type indices, local declarations, memory alignment fields, section order and body sizes. Section order follows the specification, not a naive ascending numeric sort of section IDs. The binary module format defines these constraints. [W8]

Emit a small stable core and optional custom sections for ABI identity, target/features, source maps, provenance and Habu definition metadata. Bind diagnostics to `(function index, instruction byte range, source span, definition identity)`.

Custom sections carry evidence; they are not enforced by ordinary Wasm validation and are not a security boundary by themselves. A loader must compare actual imports/memory/table declarations and code properties to the expected manifest.

## 11. Publication and incremental compilation

### 11.1 Replace the native publisher interface

Do not implement `NBACK:EMIT(..., at)` by pretending `at` is a Wasm function address. Introduce an owned artifact result and a publication strategy. Native code can retain a native-emission/native-entry variant; Wasm returns a module plan/bytes and a host-installed function-handle identity.

Reuse the registry and selection boundary. Do not add a second backend-discovery mechanism. Split frontend orchestration from the native ABI and publisher imports only as much as required by a second backend.

### 11.2 AOT publication

Encode and validate the whole library, construct its manifest, then instantiate it into a fresh instance. Publish the library handle only when initialization succeeds. Initialization that needs external services runs through explicit entry points after admission, not an unrestricted start function.

### 11.3 Interactive publication protocol

Wasm does not expose writable executable memory to the guest; new compiled code is a new module which the host compiles and installs. [W7]

Proposed state machine:

```text
recorded -> checked -> lowered -> encoded -> validated
         -> host-instantiated -> initialized -> committed
                                    \-> aborted
```

For an incremental module, forbid a start section and active data/element segments that could modify shared execution state during instantiation. Produce symbolic initialization work instead. Reserve staging memory and unused table slots, validate all resources, and instantiate without user-code execution. Existing execution is paused at a safe point.

Initialize only the candidate's reserved data, install adapters in previously unused slots, and publish its dictionary binding last. The commit path must have no fallible allocation or host callbacks after visibility changes begin. On failure, release/tombstone staged resources and preserve the prior searchable dictionary and callable entries. Leaked host compilation caches do not constitute publication, but are accounted for separately.

Trusted modules within one runtime epoch may share the epoch's memory and table. Untrusted plugins must not share them with the compiler or privileged runtime; give those plugins separate instances and narrow host interfaces.

Batch related definitions. One module per tiny word is a useful smoke test, not an assumed performance strategy. Record compilation time, installation time, retained module/table growth and bytes per batch.

## 12. Browser host, async and compile-time execution

### 12.1 Deliberately small host surface

Use named, versioned imports for diagnostics, application transport, permitted source acquisition, storage and resource release. All byte ranges are validated against current memory bounds. External objects use generation-checked handles, not pointers. Cap request bytes and quotas before allocating host objects.

Do not offer a general `eval-JavaScript` import, a generic DOM-object escape, unrestricted filesystem access, native library loading or raw database credentials. Host permissions are deployment grants, not automatically conferred by a source declaration.

Habu owns algorithms and the checked compiler. A small JavaScript/TypeScript adapter owns browser-only integration. It is not a compiler implementation dependency.

### 12.2 Async boundary

Run the Habu instance in a worker and serialize entry to each context. No same-context reentry from a synchronous import. Host callbacks queue events and execute after the current turn returns. Multiple contexts still share instance-level state unless proven otherwise; initial implementation serializes the entire instance.

For application I/O, submit a request carrying a handle/ID, return a pending application value, and resume through a completion entry. Outstanding host operations must own copied input or explicitly pinned memory; they cannot retain a borrowed view through growth, deallocation or an `await`.

Do not assume a JavaScript Promise can transparently suspend a synchronous core-Wasm stack. Transparent suspension requires an explicitly supported integration or compiler transformation. The baseline uses resumable application/top-level state. JSPI may be a later independently qualified optimization. [W5, W9]

### 12.3 Compile-time evaluation is a real milestone

Cross compilation must separate:

```text
compiler execution environment
compile-time dictionary and evaluator
frozen source/checker facts
construction of target data and target code
```

A source `create`, constant evaluation, immediate operation or generated declaration must not accidentally allocate a native object and emit its host address as guest data. The evaluator uses logical target data objects and explicit portable compile-time primitives where target layout matters.

For browser interaction, a newly compiled definition may need to be installed before evaluation can continue. Use a top-level pump that can return `need-install`, let the host compile/instantiate the module, and then resume its saved evaluator state. That saved state is not a captured arbitrary engine call stack.

A small evaluator over already checked/resolved operations is permitted for compile-time execution or a no-dynamic-compilation environment. It must use the same parser/checker facts and declared primitive semantics. Do not create a second permissive Habu language or silently interpret a library which was promised native Wasm code generation.

Qualify WebAssembly compilation policy, worker loading, MIME type, host restrictions and deployment content policy with real browsers. A browser that prohibits dynamic Wasm compilation needs a documented AOT/evaluator deployment, not a pretend JIT.

### 12.4 WASI and GPU boundaries

The core backend is not tied to WASI. A dedicated runner can provide the narrow test ABI first; versioned WASI adapters and component wrapping follow as separate packaging work. WASI's current documentation describes 0.1, 0.2 and 0.3 milestones; do not build a design that assumes 0.2 is perpetually the newest release. [W10]

Wasm is the CPU execution target in this design, not the GPU kernel target. Browser GPU work goes through a host GPU interface and separately generated shaders; this backend does not translate Wasm functions into GPU kernels. The inspected Habu README already places Loom's PTX/GPU machinery in the sibling repository. Keep that ownership separation. [R1]

## 13. Security, validation and proof obligations

Keep three different properties separate:

1. The Habu checker validates declared source effects and the ownership/type rules it actually implements.
2. Backend validators check representation, lowering and publication invariants.
3. The Wasm engine validates and isolates core-Wasm execution according to its embedding.

None is a replacement for the other two. A syntactically valid Wasm module can compute the wrong answer, misuse a nominal handle, overwrite another object in its memory or exhaust CPU.

Use existing genuine primitive mechanisms for raw memory providers, host calls and publication admission. Do not add `TRUSTED:` wrappers around ordinary compiler algorithms to get them through the checker. Record every remaining trusted assumption and the engine/host in the execution trusted base. The compiler can remain self-hosted without claiming the browser engine is proved Habu. [R11, W7]

### 13.1 Validator subjects

* HIR admission: owned input, known schemas, supported semantics, symbolic targets, source provenance.
* Representation: cell widths, lane grouping, pointer roles, valid conversions, nominal effect evidence.
* Control: branch destinations and signatures, parallel copies, loop intent, cleanup/error edges.
* Locals/stack: type-correct use, initialization, preserved value identities and evaluation ordering.
* Artifact: binary decoding, section lengths and order, imports, memory/table limits, metadata binding.
* Publication: no observable half-definition, no side-effecting instantiate path, generation and lifetime discipline.

A witness bound to input/output hashes is only useful when the validator checks its claimed relation. Structural validation is not a semantic-preservation proof.

### 13.2 Resource isolation

Place maximums on source bytes, graph nodes, nesting, function arity/locals, type count, code bytes, table entries, memory and outstanding host requests. Compilation of untrusted source needs limits too.

Instrument recursive entries and loop backedges with a hard fuel mechanism whose exhaustion cannot be swallowed by language `catch`. A worker host also provides termination/cancellation. Do not depend only on a JavaScript timer inside the worker that is executing an infinite synchronous Wasm loop.

For adversarial source, separately isolate compile-time execution, generated program instances and any native server host. Imports are the authority boundary. Review whether a plugin can access or manufacture internal runtime data; coarse linear-memory bounds do not protect privileged data placed in the same memory.

## 14. Repository layout and integration changes

Use the current `src/arch/` convention. Do not force the earlier proposed `src/targets/` reorganization into this change.

```text
src/compiler/
  target.f                         extend stable target vocabulary
  binding.f                        retain source of semantic binding
  numeric-policy.f                 explicit Wasm numeric policy
  backend.f                        extract genuinely shared dispatch as needed
  driver.f                         target-neutral definition/artifact orchestration
  native/
    hir.f                          existing shared HIR; minimal capability fixes
    elaborate.f                    existing shared elaboration; symbolic hooks
    ...                            keep native-only code native

src/arch/wasm/
  target.f                         target/profile construction and capabilities
  passes.f                         registry/pass installation
  abi.f                            scalar, frame and dynamic call contracts
  layout.f                         cell, pointer, family and data layout
  legalize.f                       memory, numeric, error and call legalization
  cfg.f                            WCFG schema and builders
  structure.f                      CFG -> WSTRUCT and region verification
  locals.f                         local assignment / safe reuse
  stackify.f                       conservative expression scheduling
  leb.f                            bounded signed/unsigned LEB codecs
  encode.f                         typed instruction and body encoding
  module.f                         section construction and whole-module checks
  link.f                           symbolic whole-program link plan
  validate.f                       backend-specific validators
  debug.f                          source/definition/provenance mappings
  publish.f                        owned artifact and host installation protocol
  runtime/
    context.f                      contexts and entry invariants
    memory.f                       memory provider, arenas and quotas
    error.f                        throw propagation / fatal diagnostics
    xt.f                           descriptors, adapters and lifetime checks
    dictionary.f                   portable definition identity/publication
    eval.f                         explicit evaluator/top-level pump
    host.f                         narrow import declarations

host/browser/
  habu-host.ts                     browser capability adapters only
  habu-worker.ts                   worker lifecycle, serialized calls, events

test/wasm/
  target.f  semantics.f  control.f  calls.f  exceptions.f
  memory.f  families.f  imports.f  publication.f  selfbuild.f
```

Names are proposed files, not claims that they exist today. Move existing shared code incrementally with compatibility includes only where useful. Preserve current build/gate ownership; a Wasm fixture generator belongs in Habu, with external engines used as independent test tools.

## 15. Implementation waves and acceptance

### W0 — Portability contracts and negative tests

Add target/ABI/profile identity, separate ordinary scalar floating point from the current stronger feature, establish the pointer/cell layout, artifact result boundary and guest-symbol classification. Audit the reachable native dependencies of a small library export.

**Acceptance:** existing target digest goldens remain stable; unsupported imports and host pointers are rejected with dependency paths; malformed feature combinations fail; no native test regressions. No claim of execution yet.

### W1 — Honest scalar backend

Implement integer/real constants, basic arithmetic, bit operations, comparison masks, pure locals, direct functions, returns, bounded LEBs, binary sections and explicit status results. Implement division boundary handling immediately.

**Acceptance:** generated bytes validate independently and execute in a Wasm engine; arithmetic fixtures match the admitted Habu contract, including `MIN-N/-1`, zero division and full-width conditions. Habu generates the bytes in the implementation gate; hand-authored fixtures alone are not a backend gate.

### W2 — Control flow and checked data

Implement structured control, edge copies, normal loops, self recursion/tail loops, memory, typed products, tagged families, static target data and source diagnostics.

**Acceptance:** nested loops, cyclic phi-copy cases, whole-width masks, overlapping memory moves, malformed tags and wide-value preservation pass; no native addresses appear in target data. Unsupported control shapes are explicit refusals.

### W3 — Useful browser library

Add allocator/context entry wrappers, JavaScript buffer/BigInt adapters, worker integration and a real application slice such as decode -> transform -> BVH/selection -> returned result buffer.

**Acceptance:** run with no native Habu process; test Chromium, Firefox and Safari/WebKit on the actual supported products; exercise growth, OOM, buffer lifetime, reentry rejection and cancellation. Measure code size, cold compile/instantiate time, resident memory and task performance. Do not promise a speedup before measurement.

### W4 — Dynamic runtime correctness

Implement typed execution-token descriptors, generic adapters, defer identity, catch/throw/finally, supported defining words, portable dictionary and bounded-space recursion paths needed by the runtime.

**Acceptance:** exception and linear-owner negative tests, stale-token tests, retirement/redefinition tests and native-vs-Wasm semantic differentials pass. Native-only operations fail by name, never reach fabricated pointers.

### W5 — Browser compilation and evaluation

Compile the checker/frontend/backend into Wasm; implement capability-controlled compile-time execution and the install/resume pump. Add transactional module batches and rollback/failure injection.

**Acceptance:** source is received, checked, compiled and run in the browser without a native compiler; compilation/instantiation/init failures publish nothing; duplicate/retired definition rules remain correct; evaluator and code quotas cannot be bypassed.

### W6 — Self-hosting and qualification

Build compiler B0 with the native Habu compiler. Run B0 under Wasm to produce B1. Run B1 under the same pinned source/runtime/profile to produce B2. Compare deterministic B1/B2 artifacts with only a narrowly specified nonsemantic build-note exclusion, if any, and test behavior independently.

**Acceptance:** the actual compiler source closure builds; B1/B2 generation identity, provenance, corruption recovery and test gates pass. Cross-compiling B0 is not self-hosting. A fixed point is not proof of correctness. Exercise the existing recovery path building a native compiler that can regenerate B0 rather than assuming that provenance transfers automatically.

### W7 — Optimization and additional embeddings

Only after the above: local reuse, expression stack scheduling, inlining with code-growth budgets, SIMD, optional tail-call/EH/JSPI profiles, component/WASI packaging, additional memory models and incremental compaction.

Each optimization must preserve the numeric policy and pass mutation/differential tests. An external optimizer may be an independent experiment, never a required production compiler stage unless the architectural decision is explicitly revisited.

## 16. Test matrix

### Semantic edge cases

Zero; minus one; signed min/max; high unsigned bits; carry/borrow; overflow multiplication; zero divisor; signed-min divided by minus-one; positive/negative remainder order; shift counts 0/63/64/65; a condition with only bit 40 set; boolean-mask bitwise use; NaN/signed-zero comparisons; subnormal/overflow floats; numeric conversions; literal boundary parsing.

### Control and type invariants

Nested `IF`/`ELSE`; loop-carried values; parallel-copy cycles; `DO` versus `?DO`; negative `+LOOP`; early exit and leave; balanced return-stack operations; wide locals; ADT payload variants and invalid tags; typed quotation mismatch; unreachable results; effectful loads/calls not reordered across stores or throws.

### Memory and host boundaries

Pointer upper bits; truncated pointers; multiplication in array bounds; length overflow; null reservation; one-past-end and zero-length spans; cross-object overwrites; resize preservation; failed `memory.grow`; stale views; asynchronous use of borrowed input; stale host handles; same-context reentry; malicious/malformed input manifests.

### Lifecycle and publication

Missing imports; wrong import type; unexpected extra authority; forbidden start/active segments; failed compile/instantiate/initialize; rollback after staged writes; table limit; retired word still referenced; generation exhaustion; no slot reuse; traps after partial writes; hard fuel exhaustion; host worker termination; compiler resource exhaustion.

### Independent oracles

Use official Wasm validation through at least two independent execution engines where practical, plus WABT or another independent decoder/validator in development. WABT supplies tools such as `wasm-validate`, `wasm2wat` and `wat2wasm`. [W11] The production backend emits binaries itself.

Generate well-typed Habu programs for differential execution against the pinned native/reference semantics. Compare observable outputs and explicit memory effects, not unspecified stack contents after throw or unspecified NaN payloads. Preserve failing seeds and input profiles.

Mutation tests must detect wrong signedness, missing zero guard, missing signed-overflow guard, unnormalized masks, pointer wrap, sequential phi copy, wrong branch depth, ignored status, lost full-width throw code, wrong effect descriptor, missing cleanup, native pointer literals and early dictionary publication.

## 17. Immediate recommended implementation slice

The first end-to-end slice is:

```text
existing checked source tape
  -> frozen HIR
  -> Wasm locals + typed calls
  -> status-aware i64 arithmetic
  -> encoded module
  -> browser/engine execution
```

Then make one actual NG browser library work. This gives Habu a valuable deployment target before porting every interactive runtime service, without choosing a throwaway compiler architecture.

The two highest-risk gates are **host/target execution and address separation** and **runtime semantic parity around exceptions, execution tokens and publication**. Treat both as first-class design work, not details to patch after instruction emission.

## Sources

Repository links are pinned to the audited commit. Public specification pages were consulted on 29 September 2026; implementation should pin exact revisions in its own test/tool manifests.

[R1] Habu README: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/README.md`

[R2] Target contract: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/target.f`

[R3] Backend dispatch: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/backend.f`

[R4] Compiler driver: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/compiler.f`

[R5] Elaborator: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/elaborate.f`

[R6] HIR: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/hir.f`

[R7] Frozen readers: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/frozen.f`

[R8] Native publisher: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/publish.f`

[R9] Quotation storage: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/core/quotation-storage.f`

[R10] Arithmetic ABI: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/habu/arith-abi.f`

[R11] Current source-language guide: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/docs/forth.md`

[R12] Catch behavior evidence: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/lib/unicode.f` and `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/.dots/habu-checker-linear-scope-6218899c/habu-prove-catch-restores-2f368434.md`

[R13] Native fatal trap semantics: `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/src/compiler/native/trap.f`

[R14] Existing compiler architecture design (a design, not proof of implementation): `https://github.com/joelreymont/habu/blob/aed8416b42d4e3bfd95edad708c30702a8071145/docs/compiler-ir-design.md`

[W1] WebAssembly Core specification: `https://webassembly.github.io/spec/core/`

[W2] Numeric semantics: `https://webassembly.github.io/spec/core/exec/numerics.html`; instruction inventory: `https://webassembly.github.io/spec/core/syntax/instructions.html`

[W3] Official feature status and runtime detection guidance: `https://webassembly.org/features/`

[W4] Instruction execution, including structured control and traps: `https://webassembly.github.io/spec/core/exec/instructions.html`

[W5] WebAssembly JavaScript Interface: `https://www.w3.org/TR/wasm-js-api-2/`

[W6] Official JavaScript embedding guide, including memory growth: `https://webassembly.org/getting-started/js-api/`

[W7] WebAssembly security model and limitations: `https://webassembly.org/docs/security/`

[W8] Binary modules: `https://webassembly.github.io/spec/core/binary/modules.html`

[W9] JavaScript promise integration specification tree: `https://webassembly.github.io/js-promise-integration/`

[W10] WASI introduction and current releases: `https://wasi.dev/`

[W11] WebAssembly Binary Toolkit: `https://github.com/WebAssembly/wabt`
