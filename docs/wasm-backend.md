# Habu WebAssembly backend

The design of Habu's WebAssembly backend: checked Habu compiled directly to
core Wasm, its binding to Browser Runtime v2 (HBR2), and the first
implementation slice. It is a design, not an implemented or qualified backend.

[portability.md](portability.md) is the parent design. It owns target
resolution, compiler sessions, compile-time execution and target data, package
contributions, linking, publication and the work packages. This page holds its
Wasm sections (17 to 21) and the Wasm detail beneath them; where the two
differ, portability.md wins (its §0.1). HBR2 is
[docs/browser-runtime.md](browser-runtime.md), and "HBR2 §n" cites its
sections. Its registry is `lib/browser/hbr-v2-registry.json` (§12.1).

The backend uses memory32 with 64-bit Habu
cells, reuses the checker, recorded tape, elaborator and frozen HIR, and emits a
dedicated Wasm IR and the final binary in checked Habu. The production compiler
depends on no LLVM, MLIR, Emscripten or Binaryen.

## 1. Decisions at a glance

| Concern | Decision |
|---|---|
| Product profiles | Pure callable library, HBR2 AOT application, HBR2 developer replacement, optional browser compiler and isolated plugin (§3). The scheduled product is the HBR2 AOT application (P7); browser compilation (P12) is deferred until a caller exists |
| Architecture row | Add a `wasm` architecture and ABI in one sealed commit (P1a); memory32 and a future memory64 are profiles, told apart by address width and features |
| External target label | Proposed `wasm32-habu`; not an existing command-line option |
| Language cell | 64 bits; `CELL = 8`; do not shrink `n`, products, masks or dictionary cells |
| Linear-memory addressing | 32-bit offsets, not native process pointers |
| Cell-resident pointer | Canonical zero-extended offset in an eight-byte cell; narrowed only after validation |
| First backend representation | HIR values mapped to Wasm locals with typed structured control, built as a WSTRUCT dialect module frozen with the full `IR-BUILD:FREEZE` before encoding (§17.2) |
| Internal calls | `(ctx, inputs) -> (status, outputs)` up to 16 input and 16 output lanes, then an aligned call frame; 16/16 is a pinned internal ABI parameter (§7.3) |
| Dynamic word calls | Checked execution-token descriptors and uniform context-taking adapters |
| Catchable exceptions | Explicit status propagation and full 64-bit throw code in context |
| Fatal failures | Diagnostic plus Wasm trap / discarded execution instance, not a catchable language error |
| Host interop | Exactly HBR2's two imports and six wrapper exports; every capability is a typed packet through `submit` (§12.1) |
| Browser async | HBR2's serialized turns: `wake` schedules a later turn; no transparent promise blocking |
| NaN | Every NaN an operation makes is `$7FF8000000000000`, as on native targets; the selector clears the sign (§17.3) |
| First slice | P6 through `NSHADOW`: unplaced emissions whose call and address immediates are fixed-width padded LEBs, patched by the linker as `NEMIT` rows (§17.1) |
| JIT-style compilation | New immutable modules installed transactionally by the host (§11.2), deferred |
| Optimizer | Habu-owned; engine native optimization is the embedding's job |
| First deployment | Single worker, one unshared memory, scalar instructions plus multi-value; no mandatory threads, GC, memory64 or JSPI |
| Proof posture | Preserve checker and ownership obligations; validate every stage; do not equate valid Wasm with semantic correctness |

## 2. Existing compiler boundaries

* `src/compiler/target.f` owns the immutable architecture/ABI/endian/address-width/features contract and its stable wire codes and digest. `src/compiler/native/backend.f` owns one private complete descriptor/pass row per loaded provider, registered by its `passes.f` module. `wasm` is a contract architecture, but an unloaded Wasm provider refuses with `E-CTGT-UNLOADED`.
* `src/compiler/native/backend.f` (`NBACK`) dispatches declaration, selection, emission and lifecycle through the target row (`:97-107`, `:187-234`). `NBACK:FREEZE` freezes the definition's HIR module and folds its loops before any selector runs, so a shadow target selects from the very module the engine's selector did (`:27-35`, `:159-185`). `EMIT` still takes a numeric code location; its sibling `EMIT-UNPLACED` writes a routine measured from no slot (`:36-40`).
* `src/compiler/native/compiler.f` (`NCOMP`) consumes checker-observed source tape and publishes a pending definition only after the compilation chain succeeds (`:15-16`, `:515-527`). `LOAD-PASSES` loads the ARM64 or x86-64 passes by target and refuses any other with `E-CTGT-ABI` (`:18-24`, `:54-71`). NCOMP also compiles every definition for an open shadow binding (`:26-33`, `:482-506`).
* `src/compiler/native/elaborate.f` holds compile-time cell vectors with grouping for wide values; stack renaming and the admitted return-stack operations move value identities rather than implementing a runtime stack; quotation bodies become functions (`:4-13`). Native `does>` and the `?do` entry rule (ae15d38a) are newer than the first audit.
* `src/compiler/native/hir.f` has 47 opcodes, covering arithmetic, floating point, memory, branches, calls, quotations, traps and termination (`:51-99`); `TARGET` reads the immutable binding (`:203-206`).
* `src/compiler/native/frozen.f` reads frozen functions, blocks, values, operands and predecessor edges (`:98-196`), a usable backend input. Its one producer, `NBACK:FREEZE`, uses `IR-BUILD:FREEZE-INTERIM` (`src/compiler/native/backend.f:184`), which skips the schema, dominance, terminator, single-definition, successor-argument and span checks (`src/compiler/ir/build.f:1476-1494`); native code relies on the verify of the emitted machine module.
* `src/compiler/native/publish.f` reads the owned `NART` artifact copied from `NEMIT`, decodes no instruction, refuses an emission for another machine, and places code in the engine's code region through its own writers. It is not a portable publisher.
* `src/compiler/native/shadow.f` (`NSHADOW`) compiles each definition a second time for an open shadow binding and keeps the result as a sealed unplaced emission per published record (`:1-30`); `src/habu/aot-shadow.f` carries it into the capture.
* `src/core/quotation-storage.f` distinguishes native image DATA addresses and records quotation relocations (`:7-17`); it needs a target-specific storage strategy.
* `src/habu/arith-abi.f`: `+ - *` wrap (`:4-5`), division by zero throws `E-DIV-ZERO` = -6400 (`:5-9`, `:29`), and `MIN-N -1 /` wraps to `MIN-N` (`:23-25`).
* `docs/forth.md`: quotations capture no locals, and declaring or referencing a local inside one is `E-BAD-LOCAL-SHAPE` (`:899-906`); package redefinition needs an explicit `undefine` (`:311-316`); no unchecked escape hatch replaces checker support (`:15-17`); engine and host boundaries use `PRIM:`/`PPRIM:` (`:18-29`).

A shared frontend exists, and a second backend (x86-64) already selects from the
same HIR. The native runtime, representation and publication boundaries remain
substantial work: this is neither a new compiler nor only an instruction
encoder.

## 3. Product profiles and release boundaries

The required browser base is `wasm` + memory32 + Habu64 + one unshared memory +
the HBR2 wrapper. Scalar core and multi-value support are the baseline.
Additional instruction features are individually listed and validated; the
version number of an evolving WebAssembly specification is not a feature set.
[WW1]

The product profiles are the pure callable library, the HBR2 AOT application,
the HBR2 developer replacement, the optional browser compiler and the isolated
plugin. The scheduled product is the HBR2 AOT application (P7). The browser
compiler (P12) is specified in §11.2 and deferred until a caller exists; Maki,
the first browser caller, does not need it. The pure callable library has no
work package. A future WASI profile pins its exact interface/world and adapter
versions; it does not supply browser DOM services. Component-model packaging
and wasm32 C interop require explicit ABI/memory adapters, not an assumption
that Habu's internal cell convention is the standard C or component canonical
ABI.

Maintain a machine-readable admission matrix for source constructs, HIR
operations, primitive contracts and runtime services. Every row says
`supported`, `lowered via helper`, `host capability required`, or `refused`,
with a test. No silent native fallback is possible in a browser.

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

A 64-bit integer does not require memory64. WebAssembly has distinct numeric value types and memory address types. [WS1, WS2]

Keep `ptr-width` about the actual target address width. Define the eight-byte Habu pointer *storage slot* in the ABI layout; never let generic code infer `CELL` or pointer-slot storage solely from address width. A future packed four-byte pointer-field ABI is a separate layout, not a silent optimization.

Append architecture, ABI and feature wire codes without renumbering existing values. Update all exhaustive matches, validation, decoding, registry capacities and golden identities. Put cell layout in the new ABI's versioned identity. Preserve existing native contract digests when their meanings have not changed.

### 4.2 Repair the floating-point capability boundary

Current `F-FP` includes fused multiply-add, while `HIR:FP-TARGET` requires it for ordinary floating-point operations. [R2, R6] Do not set that bit just to get Wasm float schemas admitted.

Add a capability for ordinary scalar IEEE operations. Have the old stronger feature imply the new capability in a documented capability query, without changing old wire encodings. Make ordinary float HIR require scalar floating point; a fused operation must require a separately justified fused capability. Bump the affected HIR schema version and update native tests deliberately.

Base Wasm arithmetic has no scalar fused multiply-add instruction. Do not substitute multiply followed by add where one rounding is required. A correctly rounded software helper is an explicit later implementation; otherwise reject the operation. [WS2]

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

The current upstream specification identifies itself as WebAssembly 3.0, dated 21 September 2026. That does not imply every requested browser supports every instruction. Qualify the exact selected subset with feature probes and browser execution tests. [WS1, WS3]

## 5. Compiler pipeline

```text
Habu source + permitted compile-time environment
  -> existing checker and owned source tape
  -> existing elaborator
  -> frozen HIR with target-symbolic addresses
  -> Wasm legalization and call/effect lowering
  -> WSTRUCT: typed Wasm-level control-flow graph (the first slice's one
     dialect; a separate WCFG legalisation dialect when a pass needs it)
  -> structuring, local assignment and conservative stack scheduling at encoding
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

Map `if`/`else`, loop exits and loop backedges to typed `if`, `block`, `loop` and branches. Wasm branches target enclosing labels; loop branches and block branches use different continuation points. [WS4]

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

Reuse checked allocation algorithms after replacing `mmap` and native growth seams with a Wasm memory provider. Define allocation failure and failed growth paths before implementing resize. Reacquire JavaScript memory views whenever the buffer identity or extent changes. Fixed-length unshared ArrayBuffer views can be detached by growth. [WS5, WS6]

Neither Wasm memory bounds nor the checker alone proves absence of use-after-free or an overwrite of another object in the same linear memory. Do not expand Habu's safety claims on the strength of Wasm validation. [WS7]

## 7. Calling conventions

### 7.1 Fast internal calls

Use typed functions for fixed checked effects:

```text
(ctx: i32, flattened inputs...) -> (status: i32, flattened outputs...)
```

`status = 0` means success; `status = 1` means a catchable Habu throw. The actual throw code lives in `ctx.throw-code: i64`; it must not be truncated into the status class. Output lanes on failure are defined zero placeholders and are semantically invalid. Generated callers test status before using output values.

The untouched row-polymorphic prefix stays in the caller. Monomorphize only the concrete shape/layout needed by each call, not every possible deeper data-stack prefix. Preserve wide-value groupings and linear moves across the ABI.

For the first implementation, retain existing HIR lane representations rather than inventing aggressive cross-function type recovery. Fast type-specialized helpers can use native `f64` lanes when justified. Large arities use an explicit call-frame variant selected by a deterministic ABI rule (16/16, §7.3) and encoded in the function's signature descriptor; never silently choose a different ABI based on a compiler heuristic.

Proven non-throwing internal functions may later omit status through an explicitly distinguished internal ABI variant. This is not necessary for the first correct backend.

### 7.2 Stable dynamic adapters

Dynamic calls use a uniform adapter:

```text
(ctx: i32) -> status: i32
```

A per-context data stack carries eight-byte cells. An adapter validates the required depth and room for results, reads arguments, invokes the typed implementation and commits the resulting stack shape on success. This is the ABI for `execute`, stored quotations, deferred words and evaluator dispatch; it is not the normal cost paid by every direct arithmetic call.

The context includes an ABI version, stack region and top, throw code, diagnostic record, call-frame scratch ownership, execution generation and entry/reentrancy state. Security-sensitive fuel can be held outside guest-writable memory.

### 7.3 Wide arity and browser-facing calls

The internal ABI uses one deterministic arity rule: up to 16 flattened input lanes and 16 output lanes, excluding context/status, use the fast signature; larger shapes use an explicit aligned call-frame variant. This threshold is a **pinned internal ABI parameter**, included in the Habu call-contract identity and shared by every caller/callee. It is not selected by an optimization heuristic and does not change HBR2's external wrapper signatures. Changing it is an ABI revision.

Dynamic `execute`, quotation and defer adapters use `(ctx:i32)->i32 language_status` and canonical eight-byte Habu stack slots. An xt descriptor records definition generation, expected Habu signature/effect and adapter-table identity. Wasm function-type equality alone is insufficient to prove Habu nominal/linear compatibility. A stale xt is rejected; it never starts calling whichever unrelated function later occupies a slot.

An HBR2 application's browser-facing calls are exactly HBR2's six wrapper
exports (§12.1). Use JavaScript `BigInt` for `i64` and never `Number`; reinterpret
exported `i32` address bits as unsigned before indexing views. [WS5]

No function, product or pointer layout is presumed to match the wasm32 C ABI. A
future C-compiled module interop adapter must describe the memory, allocator and
aggregate layouts explicitly.

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

Wasm signed division traps on zero and the signed minimum divided by minus one. Its comparison instructions return 0/1. These require explicit Habu adaptation. [WS2, WS4, R10]

Lower a Habu observable true mask as `0 - extend_u(predicate)` where the language representation is all bits set. A raw 64-bit condition must be tested against zero before becoming an `i32` predicate; truncating to the low 32 bits can turn a nonzero condition into false.

### 8.4 Floating point and determinism

Wasm permits more than one NaN result payload for some arithmetic [WS2], but Habu's NaN rule binds Wasm as it binds every target: an operation that makes a NaN answers `$7FF8000000000000`, and a quiet NaN operand passes through, the left of two ([portability.md](portability.md) §0.1, §10.1). The selector clears the sign (§17.3), and no profile selects the rule away. For a requested bit-exact policy beyond that rule, either prove a restricted operation/input set, supply bit-preserving software semantics, or refuse the unsupported case.

Disable contraction, reassociation and relaxed SIMD by default. A deterministic application protocol may normalize NaNs and reject nonfinite geometry values at its own documented serialization boundary, but that does not change Habu's bit-observation semantics behind the user's back. Transcendentals should use qualified Habu helpers or explicitly named host imports, not unexamined substitution with JavaScript math functions.

## 9. Execution tokens, definitions and reflection

Represent a stored execution token as a nominal handle, for example a 32-bit descriptor slot plus a 32-bit generation in a 64-bit cell. Its descriptor records the adapter's table slot, module/definition identity, full checked effect/layout identity and allowed lifetime. Do not publish raw function indices as stable Habu addresses.

All generic table adapters can share `(i32) -> i32`; consequently Wasm's dynamic signature check alone cannot distinguish Habu word effects, nominal types or linear ownership. Validate these Habu identities before dispatch. Wasm indirect calls provide only the Wasm-level signature check. [WS7]

Initial policy: never reuse a published execution-token descriptor/table slot within an execution epoch. Cap growth; fail or rebuild the epoch at the ceiling. A later reclaiming scheme must pin active calls and account for every reachable stored token. Do not guess that a word is unused merely because its name was removed.

Preserve package `undefine` semantics: existing checked references bind definition identity, not whichever word later gets the same spelling. Existing stored tokens must retain their intended old definition or be explicitly invalidated under a documented retirement contract; never silently retarget them. `DEFER` is a deliberately mutable binding and validates replacement effect identity separately. [R11]

Quotations in the inspected language do not capture outer locals. Compile these as noncapturing functions. Do not introduce implicit closures in the Wasm backend. Supported `CREATE ... DOES>` behavior can be represented by a word descriptor with a data object and a behavior identity; this is defining-word implementation, not new lexical-capture semantics.

Code-memory inspection, native CFA arithmetic and code patching are refused or replaced by explicit reflection/debug metadata APIs. User-visible source bodies, names and disassembly can come from retained metadata and original Wasm bytes, not readable engine-native instruction memory.

## 10. Linking, data and binary emission

### 10.1 Link from symbols, not native images

Before lowering, every reachable definition is classified as guest code, guest
data, portable primitive, declared host import or unsupported native
dependency. Reject native syscalls, `dlopen`, arbitrary host addresses and code
pointers with a dependency path to the export that required them. A target data
builder produces static bytes from layouts; do not copy a native dictionary, AOT
code buffer or snapshot containing native addresses into Wasm.

The first slice links the whole set of routines from the capture (§17.1); no package cache is needed for that. Once package contributions exist (P4), persist Wasm function plans/symbolic operands inside the package contribution, not native-style machine addresses. Final assembly:

1. Resolve reachable functions, imports, data objects and dynamic adapters.
2. Deduplicate identical function types deterministically by structural signature, retaining Habu effect identities separately.
3. Sort/import/type/function/table/global/data identities under stable rules and assign final indices.
4. Lay out static memory and validate offsets against the memory32 profile.
5. Encode bodies and all affected section lengths from the final indices using bounded ULEB/SLEB routines.
6. Rebuild source offsets after final encoding, then validate the entire module.

Changing an index can change LEB byte length. The first slice therefore emits every call and address immediate at fixed width, as a padded LEB: 5 bytes for a u32, 10 for an s64. Final index assignment then patches each as an `NEMIT` row in place without changing any length (§17.1), and the padded code size is accepted. Re-encoding to minimal LEBs is the optional shrink pass of [portability.md](portability.md) §13.3, with its own convergence and byte-identity tests; it re-encodes affected function plans and section sizes and never patches variable-length bytes as if they were a fixed native displacement. A future standard Wasm object adapter follows the published tool conventions, including its relocation padding rules; it is not claimed to be the core Wasm specification. [WW3; WW4]

Cache code-generation plans to avoid repeating checking/selection on a package hit. Relinking/re-encoding is allowed and measured. One module per tiny function is not the intended incremental performance strategy.

### 10.2 Encoder contract

The production binary encoder is checked Habu. It emits owned bounded buffers using checked unsigned and signed LEB128 routines, typed opcode constructors and measured function/section sizes. The WAT renderer is a diagnostic view of the same sealed IR, not an intermediate compiler dependency.

Validate exact counts, immediates, block signatures, type indices, local declarations, memory alignment fields, section order and body sizes. Section order follows the specification, not a naive ascending numeric sort of section IDs. The binary module format defines these constraints. [WS8]

Emit a small stable core and optional custom sections for ABI identity, target/features, source maps, provenance and Habu definition metadata. Bind diagnostics to `(function index, instruction byte range, source span, definition identity)`.

Custom sections carry evidence; they are not enforced by ordinary Wasm validation and are not a security boundary by themselves. A loader must compare actual imports/memory/table declarations and code properties to the expected manifest.

### 10.3 Binary validator and admission

Validate types, locals, block signatures, index bounds, data/table/memory declarations, section ordering, body sizes, immediates, admitted opcodes, imports and exports. Wasm section order follows the specification, not a naive sort by section ID. Run an independent engine validator during qualification. Engine validity is necessary, not a semantic equivalence proof. [WW1; WW3]

Module admission parses the actual binary to compare exact function signatures, memory limits, table policy, initialization behavior and features with the manifest. A custom section is metadata, not authority. JavaScript module introspection can supplement inspection, but a list of import names/kinds alone is not a complete signature or side-effect validator.

## 11. Publication and release

### 11.1 Browser release graph and runtime admission

#### ReleaseManifest integration

The build produces the v2 release set: `app.wasm`, ABI/feature schema manifest, immutable asset manifest, generic host modules, CSS/widget templates, locale catalogues, shader/layout assets, source/definition map and optional offline manifest. All are bound to one release identity. A component-only source edit need not regenerate unchanged generic host modules. [HBR2 §21.2](browser-runtime.md#212-build-outputs-and-package-increments)

Add a **portability build descriptor** referenced by the existing release construction, rather than another competing browser manifest:

```text
PortabilityBuildDescriptor {
    schema: "habu.portability-build/1",       # proposed new sidecar schema
    target_contract_digest,
    HabuAbi_and_layout_digests,
    wasm_feature_requirements,
    wrapper_profile: "HBR2/2.0",
    core_registry_digest,
    optional_schema_digests,
    actual_import_export_shape_digest,
    callback_admission_bundle_digest,
    app_wasm_content_digest,
    debug_map_digest?, producer_receipt_digest
}
```

The sidecar carries no package contribution root digest until the package cache (P4) exists; that field is added with P4. The HBR2 protocol remains unchanged by this sidecar. Fields duplicating existing ReleaseManifest facts must agree; do not allow two independently selected registry or module hashes.

#### Admission sequence

Fetch the pinned release manifest through the existing v2 boot policy. Verify the release's content identities and host-authorized provenance. Check requested feature schemas and host grants. Inspect actual Wasm imports, export signatures, memory/table policy, required opcodes and forbidden initialization behavior. Verify callback-admission evidence under the trusted producer policy. Probe the exact browser feature subset independently of manifest claims. Only then instantiate and enter the serialized bootstrap/handshake.

For the base AOT module, forbid a core Wasm start function so application/browser effects begin only through explicit wrapper entry. Static data or element initialization of a **fresh private** instance can be admitted when fully validated; this is distinct from shared-memory incremental publication, which has stricter rules in §11.2. WebAssembly instantiation can apply active segments and invoke a start function, so neither behavior may be ignored by a transaction design. [WW1; WW5]

A code digest is an integrity identifier, not authorization. An application requesting HTTP, clipboard, persistent storage or Graphics does not grant itself those capabilities. V2's authority/namespace/grant policy remains the runtime owner. No compiler-generated import can bypass it.

#### Profile selection and fallback

Preserve the independently admitted DOM, worker WebGPU, main-thread GPU, WebGL2, developer, browser compiler and plugin profiles. The Wasm backend is not reimplemented for each. Their package/runtime/asset composition and qualified feature contracts differ. Probe disposable resources before irreversible canvas transfer where required by v2.

No SharedArrayBuffer, JSPI, memory64 or Wasm GC is silently required by the base profile. If enabled later, each changes the relevant machine/embedding/runtime constraints and testing. A selected strict profile fails clearly when unavailable; only explicitly configured fallback profiles are chosen.

#### Schemas and generated bindings

`lib/browser/hbr-v2-registry.json` is the authority for numeric operations, field layouts and typed results. Generate Habu codecs, host-side validation tables, TypeScript declarations, interface metadata and golden vectors from its pinned content. Production generation is a checked Habu build action; independent reference tools remain tests, not a new compiler implementation dependency. [HBR2 §§21.2, 25](browser-runtime.md#212-build-outputs-and-package-increments)

Artifact admission compares generated signatures/limits and actual emitted bytes, not just matching filenames. A schema change updates both sides, fixtures, feature digest and changelog together. AppData can select only approved application data schemas, never new host operations.

#### Browser deployment and recovery

HTTP MIME, worker URL/module loading, CSP, assets, connect/img/font policies and offline service-worker behavior are deployment inputs and qualification cases. Do not add unrestricted eval to bypass a failure. Public release assets and private document/draft state have separate cache and authorization policies.

AOT hot replacement creates a new worker/runtime epoch and follows v2 state migration, effect reconciliation and draft adoption. It is the baseline developer reload path. It does not copy raw Wasm heap pointers or browser resource handles into a new instance, and it does not pretend DOM/GPU/IDB/server state share one rollback point. [HBR2 §§21–22](browser-runtime.md#21-boot-deployment-developer-tools-and-optional-compilation)

### 11.2 Optional browser compilation and transactional installation

Adopted as specified and deferred: this is P12, which waits for a caller. No
Maki path needs it.

This is an **optional compiler profile**, not an extra obligation imposed on every HBR2 application. The base application retains exactly the wrapper/import surface in §12.1. Browser Runtime v2 already delegates browser compilation to the checked compiler and immutable publication design; it does not authorize arbitrary new core opcodes or a generic JavaScript execution import. [HBR2 §21.3](browser-runtime.md#213-inspector-replay-automation-and-reload)

#### Installer provider, not a second browser protocol

Define a compiler-service interface independently of transport:

```text
CompilerInstallProvider {
    prepare(OwnedCandidate, ExpectedRuntimeIdentity) -> InstallTicket,
    poll(InstallTicket) -> Pending | Prepared(PreparedInstall) | Failed(Error),
    commit(PreparedInstall, ExpectedDefinitionRoot) -> PublicationReceipt,
    abort(InstallTicket or PreparedInstall) -> CleanupReceipt
}
```

These are proposed internal interfaces, **not existing HBR2 messages or assigned numeric opcodes**. An optional compiler embedding implements them through an explicitly admitted extension schema or a private trusted host service. Its signatures, grants, quotas and schema digest are pinned in the optional compiler profile. It cannot smuggle code installation through ordinary `AppData` or alter the base `submit` interpretation without versioned admission.

The browser owns `WebAssembly.compile`/instantiation and callable installation. The Habu compiler owns source checking, generated code, symbolic data, candidate metadata and the decision to publish a definition. A Promise resolves a prepared ticket; it never recursively resumes the live compiler while a Wasm export is active.

#### Candidate representation and invariants

```text
InstallCandidate {
    candidate_id, owner_runtime_epoch,
    base_definition_root, producer_and_target_identities,
    owned_wasm_bytes, declared_imports_and_signatures,
    symbolic_data_objects, declarative_initialization_plan,
    function_adapters_and_effect_descriptors,
    private_dictionary_and_checker_delta,
    required_capacity, dependency_generations,
    code_generation_lease_plan
}
```

Every byte/row is owned by the candidate or by an explicitly retained immutable dependency. A candidate holds no borrowed slice of the compiler's reusable emission buffer. Module bytes, initializer plan and metadata are mutually bound by content identities. An import resolved during preparation must still denote the expected provider generation at commit.

For incremental modules sharing a trusted runtime's memory/table:

- Reject a core start function.
- Reject active data or active element segments that can write shared state at instantiation. The initial implementation simply rejects all active segments in this profile.
- Do not invoke an arbitrary guest initializer before commit. Initialize data through a checked declarative plan applied only to candidate-owned reservations.
- Require the exact shared memory/table identities and compatible limits. Imported mutable globals and other authorities are individually allowlisted.
- Forbid user-code entry during preparation. Instantiation is not “harmless” unless its entire admitted behavior is constrained.

Core Wasm instantiation has initialization effects, so a failed `instantiate` alone does not provide rollback. The restrictions above are the mechanism that makes preparation nonpublishing. [WW5]

Untrusted modules must not use this shared-memory compiler lane. They receive separate instances/memories and restricted capabilities, following v2's plugin profile. A nominal generation handle is not a security barrier against code permitted to overwrite the same privileged heap. [HBR2 §21.4](browser-runtime.md#214-plugins)

#### Prepare, install and commit algorithm

Use the following ordered procedure:

1. **Seal and inspect.** Check the owned artifact, target/runtime ABI, exact import types, feature subset, no-start/no-active-segment policy, data-reference provenance, callback admission, and quotas. Reserve error/cleanup bookkeeping and the eventual fixed-size publication receipt before external compilation begins.
2. **Compile privately.** Submit immutable bytes to the browser engine. While it compiles, ordinary runtime work may continue. The ticket carries the runtime epoch and dependency generations observed when submitted.
3. **Revalidate at a safe point.** Before any shared-state installation, compare the current runtime epoch, definition root and provider generations. A mismatch either restarts a bounded preparation step with fresh dependencies or aborts; it never silently rebases stale executable assumptions.
4. **Reserve candidate resources.** Allocate private data ranges, definition records, type/effect rows, table slots, cleanup bookkeeping and code-generation leases. All potential allocation failure belongs here. Reservations are inaccessible through published dictionary roots and normal adapters.
5. **Instantiate without user execution.** Bind only approved imports and instantiate the restricted module. Its exported function objects remain private. Any unexpected exception aborts; an invariant breach affecting the live runtime poisons that runtime rather than pretending the old state is safe.
6. **Apply declarative initialization.** Copy candidate bytes, zero padding, fix symbolic references to approved local/imported targets, and install adapters only into reserved unpublished slots. Validate every destination and reference. Initializers do not execute I/O or call arbitrary Habu code.
7. **Prepare one publication root.** Construct a root encompassing dictionary visibility, checker facts, effect/type records and generation metadata. If existing owners use several tables, introduce a coordinator generation/root that prevents readers from seeing a partial mixture. Do not publish the dictionary first and “finish the checker later.”
8. **Commit without failure.** Recheck expected root/epoch; atomically replace the serialized runtime's authoritative definition root and mark the candidate committed. No allocation, fallible foreign call, user callback or asynchronous wait occurs after the visibility switch begins.
9. **Return a receipt and retire old resources.** The receipt identifies the new definition generation and semantic root. Release preparation-only resources. Queue bounded reclamation of superseded code after all leases end.

Large validation/copy jobs may yield while their storage remains private. They cannot yield halfway through the visibility switch. Single-threaded serialization is sufficient for the initial implementation; adding shared-memory threads requires an explicit publication memory-order and reader-lifetime protocol.

#### What rollback means—and does not mean

Before commit, abort makes the candidate unreachable, releases owned allocations where possible, clears/tombstones reserved table entries and abandons prepared metadata. The prior **semantic definition root** remains usable if no invariant breach occurred.

Do not claim byte-for-byte rollback of the whole process. `memory.grow`, table growth, browser compilation caches and reserved allocator high-water marks may be irreversible. Charge that retained capacity and reclaim/reuse it under quota rules. Repeated failed installations must not evade memory limits by leaving every growth operation uncharged.

Post-commit application initialization and browser effects are ordinary runtime operations with their own outcomes. A file write, network request, DOM change or durable operation is not rolled back by removing a dictionary entry. If a definition requires a fallible startup action, publish an explicit `NotStarted/Starting/Ready/Failed` application state rather than describing the definition publication as one transaction with the external world.

If failure occurs after an unexpected shared write, trap or violated isolation invariant, discard the affected runtime and recover through v2. “Caught exception” is not evidence that shared state was restored.

#### Code-generation leases and redefinition

An execution token identifies a definition generation and effect descriptor; it is not merely a table index. Direct calls in an older module remain bound to their originally selected provider unless the source explicitly uses a deferred/dynamic dispatch slot. Redefining a name changes future lookup; it must not silently rewrite the meaning of already bound ordinary calls.

Retain a code generation while referenced by active calls, installed modules, stored quotations/deferred targets, callback descriptors, suspended compiler work, UI handlers, jobs or debugger records requiring execution. A prepared candidate also retains its providers until commit/abort. Generation exhaustion retires the slot rather than wrapping.

The initial implementation may retain committed module generations for the whole runtime epoch, with explicit ceilings and new-worker compaction. That is a valid bounded retention policy. Claiming prompt unloading requires actual lease accounting and reclamation tests, not relying on the browser eventually collecting a function object.

#### Resumable compiler driver

The compiler driver exposes `Continue`, `NeedInput`, `NeedInstall`, `Completed` and `Failed`. These are internal compiler states, not replacements for HBR2's language status or `STEP` result classes.

A suspended driver owns its source cursor, resolved dictionary snapshot, compilation session, candidate, input manifest and temporary resources. It stores typed continuation state at declared boundaries. It cannot retain an arbitrary native/Wasm stack, borrowed `NEMIT` storage, stale browser memory view, or an implicit callback into a now-retired definition.

Native build hosts can fulfill installation synchronously while preserving the same logical contract. Browser hosts queue completion and resume on a later serialized turn. User-provided unbounded compile-time loops are subject to compiler-job admission/resource policy; a timer cannot turn an arbitrary interrupted loop into a valid continuation.

## 12. Browser host and compile-time execution

### 12.1 Exact HBR2 binding

This is a binding to HBR2, not another proposed browser ABI. Its details come from [HBR2 §24](browser-runtime.md#24-binary-abi-and-boundary-ownership) and [Appendix A](browser-runtime.md#appendix-a--generated-hbr2-registry) and were checked exactly against [HBR2 §24.5](browser-runtime.md#245-wasm-wrapper-abi): two imports, six exports, a 128-byte control record, result classes 0 to 6 and submit codes 0 to 7.

#### Identity and registry

The wire magic is `HBR2`, major 2, minor 0. The registry digest HBR2's appendix reports is:

```text
a2c0e4e513d448fc1ecc4fbc28b3b4aff5210cb7d85b9c923d97e8be40500599
```

Habu cannot reproduce this digest: HBR2 publishes the tables but not the registry's JSON, and the member names and nesting of Habu's file are Habu's. It stays attributed to HBR2 and is not a fixture. Habu generates the registry from HBR2's normative tables (Appendix A.1–A.4, §4.2, §24.1–§24.6) into `lib/browser/hbr-v2-registry.json`, whose `sources` member names the table behind each member. Its own digest, `fdd071a5d0df682af409a9d4c2c7ae409b75554fa0602e3f06f790982dcc9677`, is SHA-256 of its sorted compact UTF-8 JSON without `contentDigest`. The file stores that canonical form itself (keys in byte order, no escapes, no floats), and test/wasm/hbr2-fixtures.f recomputes the digest and pins it. The production build imports `hbr-v2-registry.json` from the selected v2 package and recomputes its specified sorted compact UTF-8 JSON digest excluding `contentDigest`. It fails if that digest differs. Optional feature schemas have separate digests and must not renumber the core registry. [HBR2 §§24–25, 28.2, Appendix A](browser-runtime.md#24-binary-abi-and-boundary-ownership)

#### Imports

The base runtime has precisely these function imports under module name `habu_browser_v2`:

```text
submit(ctx:i32, ptr:i32, len:i32) -> i32
wake(ctx:i32) -> i32
```

DOM, storage, network, rendering, forms and other capabilities are **typed protocol operations carried through submit**, not separate new Wasm imports invented by each library. Generated Habu bindings depend on the authoritative opcode/type registry. A package asking for Graphics does not cause the compiler to create an arbitrary `webgpu.*` import namespace.

`submit` results are 0 Accepted, 1 Backpressure, 2 Invalid, 3 Denied, 4 Unavailable, 5 OOM, 6 StaleEpoch, 7 HostFailed. Accepted transfers responsibility for the submitted packet to the host after synchronous consumption/copy. Every non-Accepted result leaves packet responsibility with Habu. No awaited operation may retain a borrowed view of the Wasm memory. `wake` schedules/coalesces a later turn and never recursively enters exports. [HBR2 §24.5](browser-runtime.md#245-wasm-wrapper-abi)

#### Exports

The base wrapper exports exactly these six functions, plus the declared unshared linear memory:

```text
hbr_control() -> i32
hbr_reserve_input(ctx:i32, bytes:i32) -> i32 language_status
hbr_start(ptr:i32, bytes:i32, lease:i64) -> i32 language_status
hbr_ingest(ctx:i32, ptr:i32, bytes:i32, lease:i64) -> i32 language_status
hbr_step(ctx:i32, budget:i32) -> i32 language_status
hbr_stop(ctx:i32) -> i32 language_status
```

`hbr_control` returns a memory offset, not a status. The other functions return the inherited language exception status. Do not replace `hbr_start` with an export taking a host object, change lease to Number/u32, or return scheduler states from these functions. [HBR2 §24.5](browser-runtime.md#245-wasm-wrapper-abi)

The generated wrapper's control record has stable instance-local storage; allocator movement must not invalidate it. Bootstrap context zero is permitted only as specified by the wrapper. Wrapper startup and the HBR2 START request are separate layers; the adapter follows the v2 handshake/fixtures rather than replaying a second semantic START merely because a wrapper function was called.

#### Control record

The record is **128 bytes**, laid out by explicit byte offsets:

| Offset | Field | Representation |
|---:|---|---|
| 0 | ABI major | u32 |
| 4 | ABI minor | u32 |
| 8 | context offset | u32 |
| 12 | browser result class | u32 |
| 16 | input reservation offset | u32 |
| 20 | input reservation capacity | u32 |
| 24 | input lease | u64 |
| 32 | throw code | i64 |
| 40 | diagnostic offset | u32 |
| 44 | diagnostic length | u32 |
| 48 | RuntimeEpoch | u64 |
| 56 | workDone | u64 |
| 64–127 | reserved | zero bytes |

Browser result classes are OK=0, Idle=1, More=2, Waiting=3, Stopped=4, WouldBlock=5, BadState=6. Those are not Habu throw codes and not host submission outcomes. A normal Waiting result is not a failed function call. A language throw status does not mean the control record's result class contains the throw code. [HBR2 §24.5](browser-runtime.md#245-wasm-wrapper-abi)

#### Ingress ownership algorithm

The host serializes all instance entries. To submit input it requests a reservation, checks language status, reacquires memory views, reads the control record, verifies capacity/lease, copies the packet, and invokes start or ingest with the exact reservation pointer and lease. It reacquires views again before reading resulting control/diagnostic data.

Only one reservation is live. It is invalidated by ingest/start completion, next reserve, stop or fatal failure. A stale pointer/lease cannot be retried after another reservation. Input lengths and returned offsets are interpreted as unsigned bit patterns only after range validation. An i64 lease crosses JavaScript as BigInt without Number conversion.

The receiving runtime takes responsibility for accepted input under its allocator/context; this is not a second hidden heap. Ingress performs bounded envelope work and schedules deeper decode/semantic validation as jobs. It does not synchronously traverse an unbounded object graph. [HBR2 §§4.1–4.3, 24.5](browser-runtime.md#41-one-entry-explicit-jobs)

#### Egress and backpressure algorithm

Before calling submit, Habu owns a sealed packet and a prepared send record. On Accepted, it records the send exactly once, advances accepted transport accounting, and can release its source packet storage after the host's synchronous ownership transfer. On Backpressure or other rejection it keeps ownership, leaves accepted-send counters unchanged and follows the typed outcome policy. A queued retry references owned bytes, not a scratch pointer.

The host reserves capacity and required terminal-result bookkeeping before returning Accepted. It must not accept a request and later discover there is no storage for any terminal outcome. Browser work is scheduled after admission; same-context reentry is prohibited even for an immediately resolved Promise. Acknowledged transport acceptance is not GPU completion, DOM activation, IDB commit or server acceptance. [HBR2 §§4.4, 24.3–24.5](browser-runtime.md#44-credits-and-liveness)

#### Packet framing and codecs

HBR2 has a **96-byte packet header** and **32-byte record header**, with 8-byte record/data alignment. Integers and floats are explicitly little-endian; structures are packed in registry order, not cast from native or Habu records. Header magic, major/minor, exact total extent, record count, epochs, lane generation, packet sequence, namespace, producer, dataStart and reserved zero bytes must all validate. [HBR2 §24.2](browser-runtime.md#242-primitive-encoding-and-fixed-header)

`Blob`, `String`, and `DraftText` use eight-byte `(offset:u32,length:u32)` slots, but are different types. String is strict UTF-8; native DraftText is even-length UTF-16LE code-unit data and must preserve native draft contents losslessly, including cases not representable as ordinary normalized Unicode strings. Bool is u32 0/1. `Handle` is `(slot:u32,generation:u32)` with both zero or both nonzero; persistent Id128/Hash256 are opaque bytes.

Arrays use `(offset,count)` and a registry-derived fixed stride. Empty references are exactly `(0,0)`. Referenced data cannot point into headers or the record stream. Union/property payloads must have their registered type and exact size. Checked extent calculations, a visited-reference accounting key `(offset,type,length)`, depth limits and total traversal budgets prevent overflow/alias-expansion attacks.

The initial decoder limits are depth 32, at most 65,536 references/elements per packet, and packet limits of 64 KiB semantic, 1 MiB bulk and 4 KiB control, with stricter feature limits where specified. The v2 reference STOP packet is 136 bytes: 96-byte packet header, 32-byte record header, four-byte body and four-byte padding. These values should become binding-generation golden tests, not new handwritten numeric tables. [HBR2 §§24, 28](browser-runtime.md#24-binary-abi-and-boundary-ownership)

#### Exact packet field offsets

The generated decoder/encoder must agree with these inherited header offsets. This table is a cross-check against [HBR2 §24.2](browser-runtime.md#242-primitive-encoding-and-fixed-header); production constants still come from the registry, not an independently maintained table.

| Packet offset | Field | Width/encoding |
|---:|---|---|
| 0 | magic | 4 bytes `HBR2` |
| 4 / 6 | major / minor | u16 / u16 |
| 8 / 10 | channel / flags | u16 / u16; core flags zero |
| 12 | headerBytes | u32 = 96 |
| 16 | totalBytes | u32, exact extent |
| 20 | recordCount | u32 |
| 24 | runtimeEpoch | u64 |
| 32 | authEpoch | u64 |
| 40 | laneGeneration | u64 |
| 48 | packetSequence | u64 |
| 56 | namespace | 16 opaque bytes |
| 72 | producer | u32, bound by host/port |
| 76 | dataStart | u32, exact padded-record-stream end |
| 80–95 | reserved | 16 zero bytes |

Each record's offsets are opcode @0:u16, flags @2:u16, recordBytes @4:u32, correlationId @8:u64, scope slot/generation @16/20:u32 each, and deadline @24:f64. Deadline is root-clock milliseconds, with zero/no-deadline and notification/result rules inherited from v2. Record bytes include the header, fixed registered body and zero padding; referenced variable payload belongs in the packet data region.

Validation of a syntactically correct header is only the first phase. Exact operation direction, channel, kind, feature/capability grant, body/result schema and semantic guards must also match the registry. An attacker cannot change a bound producer/namespace by supplying numerically valid packet fields.

#### Lane and terminal semantics

Retain Control, Input, DOM, GPUResource, DisplayFrame, Capability, Diagnostic and GPUJob channels from the registry. Ordered local lanes do not become a new retry transport. Local duplicates/gaps follow v2's error/reset behavior, except its explicitly idempotent cumulative CREDIT semantics. Network durable-operation deduplication is a different layer.

Each accepted request resolves through the registered RESULT success/error schema. Progress can precede terminal outcome. Terminal ownership must survive observer abandonment and late completion. The compiler may generate typed wrappers and state helpers, but cannot reinterpret requests as fire-and-forget calls or assume every terminal result fits a small inline struct. [HBR2 §§4.4, 24.3–24.4](browser-runtime.md#44-credits-and-liveness)

### 12.2 Compiler obligations HBR2 imposes

HBR2 is more than an import manifest: it requires **compile-time effect and cost admission** and lifetime-aware generated application code. The limits below were checked against [HBR2 §4.2](browser-runtime.md#42-callback-admission-not-fictional-preemption): loop bound 64, 2,048 weighted operations, 256 direct nodes and 16 KiB copied.

#### Bounded callback verifier

Implement an admission pass over frozen HIR and its exact callable closure. For the base ViewCallback profile, verify the acyclic transitive call graph, statically bounded loops with each bound at most 64, closed indirect callback sets, allowed bounded primitives, and the 2,048 weighted-operation limit. Also enforce v2's 256 direct-node and 16 KiB copy bounds for a component callback. These are v2 initial profile limits, not measured wall-clock promises. [HBR2 §4.2](browser-runtime.md#42-callback-admission-not-fictional-preemption)

The verifier computes worst-case cost using checked/saturating arithmetic: sequence costs sum; alternatives take the maximum; loops multiply the bound by worst body cost plus their control overhead; calls include the certified callee cost; bounded byte operations include their maximum byte-block count. Nested loops multiply, so two individually small bounds do not excuse an excessive total. Unknown bounds or recursive SCCs fail this callback profile. A helper called “copy” does not cost one operation when it can copy an unbounded buffer.

Effect closure rejects arbitrary `execute`, raw global writes, clock/random calls and host I/O in restricted callbacks. A generated action descriptor is data; it is not permission for a view callback to mutate the document or issue an unbounded request. Large enumeration, parsing, sorting and scene work use v2's explicit job state machines.

#### Admission evidence

```text
CallbackAdmission {
    profile_and_revision,
    definition_generation,
    HIR_content_identity,
    executed_or_reachable_implementation_closure,
    allowed_effects, primitive_cost_table_id,
    maximum_cost, node_bound, copied_byte_bound,
    verifier_identity
}
```

Bind evidence to the exact code/implementation closure. If optimization, inlining, helper selection or a deferred target changes relevant behavior, revalidate or regenerate the certificate. Metadata can be forged; a loader trusts a qualified producer/proof-verifier policy, not a self-asserted custom section in arbitrary Wasm. Untrusted opaque modules remain isolated plugins unless independently admitted.

Hard fuel catches a cost-contract violation; exhaustion aborts unpublished work or terminates the isolated runtime as specified. It is **not** resumable preemption of an arbitrary Wasm stack. Cooperative Yield/Wait occurs only at explicit job boundaries. A watchdog on a different agent can terminate a stuck worker, but that does not recover an arbitrary valid continuation. [HBR2 §§4, 22](browser-runtime.md#4-scheduling-bounded-execution-clocks-and-backpressure)

#### Generated ownership types

Generate nominal library interfaces for v2's snapshot leases, builders, read contexts, scope/task handles, immutable records and code-generation leases. Preserve explicit retain/release for shared reads and linear ownership for candidates/builders. Do not expose mutable raw pointers into published immutable trees as a convenience API.

The runtime package owns its concrete persistent-store representation. The portability layer does not replace it with a new generic transaction system. In particular, v2's semantic snapshot lease, native edit lease, GPU frame lease and durable operation have different commit/termination events. A compiler optimization must not merge their release paths simply because each is represented by one handle.

#### Code retention across UI and GPU work

A component descriptor, queued event/action, job continuation or retired binding that can invoke code holds its definition generation's code lease. V2 frame leases retain sealed GPU input versions until safe completion; reliable picks have their own ownership and cannot be cancelled merely because a display frame was superseded.

Hot replacement may detach old observers while old durable operations continue outcome discovery. The host drains old-epoch resources under their original ownership/authority; it must not route their results into the replacement epoch merely because a slot number matches. [HBR2 §§2, 5, 15, 21](browser-runtime.md#2-identities-stamps-authority-and-validity)

#### Package boundaries

Preserve v2's package dependency direction: RUNTIME does not import UI, SCENE, BROWSER or SYNC; UI depends on RUNTIME; rendering remains independent of Maki/exact-kernel choices. A DOM form build must not pull in GPU or collaborative journal code unless selected by its feature/dependency closure. Browser bindings use Habu's package system, not a separate browser-only package manager. [HBR2 §1.2](browser-runtime.md#12-independent-packages)

### 12.3 Compile-time evaluation

Compile-time execution and target data are [portability.md](portability.md) §7
in full: `TargetRef`, `TargetObject`, the target-data builder and the audited
legacy owner adapter. The first slice uses that adapter, `NSHADOW` (§17.1);
browser-hosted compile-time evaluation belongs to the deferred browser compiler
(§11.2).

### 12.4 WASI and GPU boundaries

The core backend is not tied to WASI. A dedicated runner can provide the narrow test ABI first; versioned WASI adapters and component wrapping follow as separate packaging work. WASI's current documentation describes 0.1, 0.2 and 0.3 milestones; do not build a design that assumes 0.2 is perpetually the newest release. [WS10]

Wasm is the CPU execution target in this design, not the GPU kernel target. Browser GPU work goes through a host GPU interface and separately generated shaders; this backend does not translate Wasm functions into GPU kernels. The inspected Habu README already places Loom's PTX/GPU machinery in the sibling repository. Keep that ownership separation. [R1]

## 13. Security, validation and proof obligations

Keep three different properties separate:

1. The Habu checker validates declared source effects and the ownership/type rules it actually implements.
2. Backend validators check representation, lowering and publication invariants.
3. The Wasm engine validates and isolates core-Wasm execution according to its embedding.

None is a replacement for the other two. A syntactically valid Wasm module can compute the wrong answer, misuse a nominal handle, overwrite another object in its memory or exhaust CPU.

Use existing genuine primitive mechanisms for raw memory providers, host calls and publication admission. Do not add `TRUSTED:` wrappers around ordinary compiler algorithms to get them through the checker. Record every remaining trusted assumption and the engine/host in the execution trusted base. The compiler can remain self-hosted without claiming the browser engine is proved Habu. [R11, WS7]

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

Physical layout follows [portability.md](portability.md) §26.1, and its moves
wait for P13. The Wasm path adds only new files:

| Path | Holds |
|---|---|
| `src/arch/wasm/` | The Wasm backend: selector, WSTRUCT dialect, encoder, LEB routines and module linker, registered as a complete row through its `passes.f` module |
| `test/wasm/` | Wasm tests and fixtures |
| `host/browser/` | HBR2's closed host adapter and workers |
| `lib/browser/`, `lib/runtime/`, `lib/ui/` | HBR2's BROWSER, RUNTIME and UI packages, built as caller-driven Habu libraries |

The sealed engine edits the path needs (the `wasm` target row, the scalar-FP
capability bit and the IR decoders that mirror them) land in one commit, P1a
([portability.md](portability.md) §5.3). HBR2's package boundaries hold
whatever the directory (§12.2, Package boundaries).

## 15. Work packages

The Wasm path is P1a -> P6 -> P7, with P0w alongside; it never waits on P2-P5
([portability.md](portability.md) §27).

| Package | Dot | Delivers |
|---|---|---|
| P0w | habu-pin-hbr2-wire-0b340032 | HBR2 wire fixtures, the registry and its digest, Wasm numeric goldens N01-N06 with the NaN print rows and the three `?do` rows, run natively |
| P1a | habu-add-the-wasm-4c32353e | The `wasm` architecture and ABI rows, the scalar-FP bit and the decoders, in one sealed commit |
| P6 | habu-emit-a-wasm-05443776 | The first slice (§17) |
| P7 | habu-bind-the-wasm-5e9830c7 | The HBR2 wrapper, codecs, callback verifier, release sidecar and a real browser host (§11.1, §12) |
| P12 | none; deferred | Browser compilation (§11.2) |
| P13 | habu-retire-target-selecting-affc4d65 | Retires target-selecting host predicates and does the moves |

The HBR2 runtime packages P7 needs are built by habu-build-hbr2-runtime-731d5ddd,
habu-build-hbr2-browser-84328e34 and habu-build-hbr2-scene-972b5283. Earlier
revisions of this page planned waves W0-W7, which map to the packages as W0 ->
P1a and P0w, W1 and W2 -> P6, W3 -> P7, W4 -> the adapters in P6 and P3, W5
and W6 -> P12, and W7 -> P13.

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

Use official Wasm validation through at least two independent execution engines where practical, plus WABT or another independent decoder/validator in development. WABT supplies tools such as `wasm-validate`, `wasm2wat` and `wat2wasm`. [WS11] The production backend emits binaries itself.

Generate well-typed Habu programs for differential execution against the pinned native/reference semantics. Compare observable outputs and explicit memory effects, not unspecified stack contents after throw or unspecified NaN payloads. Preserve failing seeds and input profiles.

Mutation tests must detect wrong signedness, missing zero guard, missing signed-overflow guard, unnormalized masks, pointer wrap, sequential phi copy, wrong branch depth, ignored status, lost full-width throw code, wrong effect descriptor, missing cleanup, native pointer literals and early dictionary publication.

## 17. The first slice (P6)

### 17.1 The route through NSHADOW

P6 emits a genuine Habu-generated module by riding master's audited legacy owner
adapter ([portability.md](portability.md) §7.4) instead of waiting for
symbolic target data (P3). A Wasm shadow binding is opened, and `NCOMP`
compiles each definition for it from the module `NBACK:FREEZE` froze for the
engine's own selector (§2). The Wasm backend's `EMIT-UNPLACED` row writes a
sealed `NEMIT` emission measured from no slot. Every call and address immediate
in it is a fixed-width padded LEB (5 bytes for a u32, 10 for an s64), so each is
a row whose field the linker writes in place (§10.1). The capture
(`src/habu/aot-shadow.f`) collects the routines, resolving every host address
through the xt -> record index and refusing an unresolved one by name, and the
Wasm linker assigns final indices and writes the module. The `TargetRef` of
[portability.md](portability.md) §7.2 replaces this route later.

A Wasm emission's bytes begin with a function table the encoder writes and
`src/habu/link-wasm.f` reads: the count, then per function its body offset,
size, inputs, outputs and frame variant. `NEMIT`'s function rows name the
bodies. The call field is the five bytes after the `call` opcode, and the
address field is the ten after `i64.const`. The capture's Wasm reader is
`src/arch/wasm/capture.f`, which reads `NSHADOW`'s public readers into
`AOT-SHADOW`'s tables (`src/habu/aot-decl.f`), because the native reader's
MOVABS carrier cannot rewrite an LEB.

### 17.2 Verify HIR before lowering

`NBACK:FREEZE` freezes with `IR-BUILD:FREEZE-INTERIM` (`src/compiler/native/backend.f:184`),
which skips the schema, dominance, terminator, single-definition,
successor-argument and span checks (`src/compiler/ir/build.f:1470-1498`). Native
code is safe because the emitted machine module is verified whole; a Wasm
module has no such machine module. The Wasm selector therefore reads the folded
interim HIR, builds a WSTRUCT dialect module with its own schema, and freezes
it with the full `IR-BUILD:FREEZE` (`src/compiler/ir/build.f:1465`) before
encoding. The binary validator (§10.3) checks the bytes, not the SSA and
dominance facts.

WSTRUCT is a control-flow graph. It has blocks with typed i32, i64 and f64
arguments, Wasm instructions as operations, and `br`, `brz`, `return`,
`unreachable`, `call` and `call_indirect`. It has no block, loop or if
operation: the substrate records a region count
(`src/compiler/ir/schema.f:324`), but `IR-BUILD` has no region builder. The
full FREEZE therefore checks dominance, single definition and successor
arguments meaningfully. The encoder (`src/arch/wasm/encode.f` with
`structure.f`) derives dominators, loops and the control tree from the frozen
graph, refuses an irreducible one by name, and assigns label depths as it
writes.

### 17.3 NaN canonicalisation

Wasm leaves a NaN result's sign nondeterministic, and a non-canonical NaN
operand can come out as any arithmetic NaN (the `nans_N` rule), so clearing
the sign does not meet the rule. After f64 add, sub, mul, div and sqrt, the
selector answers without a branch: the left operand when it is a NaN, else the
right when it is, else `$7FF8000000000000` when the result is a NaN, else the
result (`f64.ne x x` and `select`). Made NaNs are then `$7FF8000000000000`, and
a quiet NaN operand passes through unchanged, the left of two
(`src/compiler/native/hir-word.f:1261-1266`). A signalling NaN operand is
outside the rule and passes through unquieted. fneg, fabs, the comparisons and
the conversions add nothing; `f>s` is exactly `i64.trunc_sat_f64_s`.

### 17.4 Lowering algorithm

Use the existing checked semantic HIR, legalize representations/calls/errors, construct a typed Wasm CFG, structure it, assign locals, and emit a module plan. Begin with one local per lowered value and conservative evaluation ordering. Stack shuffles normally rearrange value identities, not linear-memory stack slots. Retain wide-value grouping and source ownership metadata even where physical lanes are all i64 (§5).

For reducible CFGs, use dominators, natural loop membership and a verified region tree. Translate conditional regions to `if`, exits to enclosing blocks and backedges to loops. Keep symbolic label identities until branch-depth assignment. For edge arguments, schedule parallel copies with temporary locals to break cycles; sequentially implementing `a <- b; b <- a` is a miscompile.

The compiler must account for irreducible control that later transformations can create. The initial profile can explicitly refuse it with a source/IR diagnostic; the complete profile includes a checked dispatcher-loop lowering for the affected region, not every function. This is a release capability, not silent fallback to native execution or an unverified structure heuristic.

The first slice lowers every tail call, self calls included, as call then return, and its admission matrix says so: not bounded-space. The self-tail loop, the trampoline and the Wasm tail-call feature are later profiles. `call; return` is not bounded-space tail recursion. Unknown dynamic calls carry the necessary checked effect and exceptional edges.

At every call and wordcall, the live row the HIR operation carries through is stored to the context stack and reloaded afterwards, as the native data-stack boundary stores it (`src/compiler/native/select-x64.f:685-693`). Arguments and results are typed lanes (§7.1), so `depth` and `.s` answer as they do natively.

### 17.5 Memory and constants

Logical pointers lower to canonical memory offsets, checked before narrowing. Reserve null/control/runtime/stack/static/heap regions under one target layout. The null reservation is not a Wasm guard page: address zero can otherwise be in bounds. Pointer operations enforce the language's intended null rule.

Wasm memory growth failure follows an explicit allocator error path. View lifetime across host calls is specified by HBR2, not guessed from a fixed initial buffer. The memory page count must be widened before converting it to bytes. Numeric i64 values do not require memory64, and JavaScript i64 interchange uses BigInt rather than Number (§6). [WW2]

P6 pins this layout:
- null [0,$10000);
- ctx [$10000,$11000);
- output region [$11000,$21000);
- data stack [$21000,$31000);
- static DATA from $31000.

The memory minimum equals its maximum, since the slice has no allocator. A failed pointer check records the fault in ctx and traps (§8.2).

### 17.6 Acceptance

The slice is done when a module generated by Habu, not written by hand,
validates in an independent engine and passes W01-W07 and N01-N06 of
[portability.md](portability.md) §25, including the NaN print rows and the
three `?do` rows, with differential rows against native output (W31, W32).
Hand-authored fixtures alone are not a backend gate.

The oracles: wasm-tools validates with exactly the profile's features, and
node (V8) and bun (JavaScriptCore) run `test/wasm/run.mjs`. Together they form
the `wasm` device check ([bootstrap.md](bootstrap.md)), not the ordinary gate.
The module exports memory, run, throw-code, out-base and out-len and imports
nothing; HBR2's imports are P7's. The driver, `tools/wasm-build.f`, loads
`src/arch/wasm/backend.f` and `passes.f`; `NCOMP`'s `LOAD-PASSES` loads native
passes only.

## Sources

Repository references are paths on master, re-audited at 67a66d28. Public specification pages were consulted on 29 September 2026; implementation should pin exact revisions in its own test/tool manifests.

[R1] Habu README: `README.md`

[R2] Target contract: `src/compiler/target.f`

[R3] Backend dispatch: `src/compiler/native/backend.f`

[R4] Compiler driver: `src/compiler/native/compiler.f`

[R5] Elaborator: `src/compiler/native/elaborate.f`

[R6] HIR: `src/compiler/native/hir.f`

[R7] Frozen readers: `src/compiler/native/frozen.f`

[R8] Native publisher: `src/compiler/native/publish.f`

[R9] Quotation storage: `src/core/quotation-storage.f`

[R10] Arithmetic ABI: `src/habu/arith-abi.f`

[R11] Current source-language guide: `docs/forth.md`

[R12] Catch behavior evidence: `lib/unicode.f` and `.dots/habu-checker-linear-scope-6218899c/habu-prove-catch-restores-2f368434.md`

[R13] Native fatal trap semantics: `src/compiler/native/trap.f`

[R14] Existing compiler architecture design (a design, not proof of implementation): `docs/compiler-ir-design.md`

[WS1] WebAssembly Core specification: `https://webassembly.github.io/spec/core/`

[WS2] Numeric semantics: `https://webassembly.github.io/spec/core/exec/numerics.html`; instruction inventory: `https://webassembly.github.io/spec/core/syntax/instructions.html`

[WS3] Official feature status and runtime detection guidance: `https://webassembly.org/features/`

[WS4] Instruction execution, including structured control and traps: `https://webassembly.github.io/spec/core/exec/instructions.html`

[WS5] WebAssembly JavaScript Interface: `https://www.w3.org/TR/wasm-js-api-2/`

[WS6] Official JavaScript embedding guide, including memory growth: `https://webassembly.org/getting-started/js-api/`

[WS7] WebAssembly security model and limitations: `https://webassembly.org/docs/security/`

[WS8] Binary modules: `https://webassembly.github.io/spec/core/binary/modules.html`

[WS9] JavaScript promise integration specification tree: `https://webassembly.github.io/js-promise-integration/`

[WS10] WASI introduction and current releases: `https://wasi.dev/`

[WS11] WebAssembly Binary Toolkit: `https://github.com/WebAssembly/wabt`

[WW1] WebAssembly module structure: `https://webassembly.github.io/spec/core/syntax/modules.html`

[WW2] WebAssembly JavaScript interface: `https://webassembly.github.io/spec/js-api/`

[WW3] WebAssembly binary module format: `https://webassembly.github.io/spec/core/binary/modules.html`

[WW4] WebAssembly tool conventions, linking: `https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md`, and LLD's Wasm documentation: `https://lld.llvm.org/WebAssembly.html`

[WW5] WebAssembly module instantiation: `https://webassembly.github.io/spec/core/exec/modules.html`

[HBR2] Habu Browser Runtime, consolidated design, revision 2 (2 October 2026). It is [docs/browser-runtime.md](browser-runtime.md).
