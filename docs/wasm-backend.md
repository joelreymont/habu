# Habu WebAssembly backend

The design of Habu's WebAssembly backend: checked Habu compiled directly to
core Wasm, and the first implementation slice. It is a design, not an
implemented or qualified backend.

[portability.md](portability.md) is the parent design. It owns target
resolution, compiler sessions, compile-time execution and target data, package
contributions, linking, publication and the work packages. This page holds its
Wasm code generation section (17) and the Wasm detail beneath it; where the two
differ, portability.md wins (its §0.1).

The backend uses memory32 with 64-bit Habu
cells, reuses the checker, recorded tape, elaborator and frozen HIR, and emits a
dedicated Wasm IR and the final binary in checked Habu. The production compiler
depends on no LLVM, MLIR, Emscripten or Binaryen.

## 1. Decisions at a glance

| Concern | Decision |
|---|---|
| Product profiles | The core module P6 builds (§3, §17) |
| Architecture row | Add a `wasm` architecture and ABI in one commit (P1a); memory32 and a future memory64 are profiles, told apart by address width and features |
| External target label | Proposed `wasm32-habu`; not an existing command-line option |
| Language cell | 64 bits; `CELL = 8`; do not shrink `n`, products, masks or dictionary cells |
| Linear-memory addressing | 32-bit offsets, not native process pointers |
| Cell-resident pointer | Canonical zero-extended offset in an eight-byte cell; narrowed only after validation |
| First backend representation | HIR values mapped to Wasm locals with typed structured control, built as a WSTRUCT dialect module frozen with the full `IR-BUILD:FREEZE` before encoding (§17.2) |
| Internal calls | `(ctx, inputs) -> (status, outputs)` up to 16 input and 16 output lanes, then an aligned call frame; 16/16 is a pinned internal ABI parameter (§7.3) |
| Dynamic word calls | Checked execution-token descriptors and uniform context-taking adapters |
| Catchable exceptions | Explicit status propagation and full 64-bit throw code in context |
| Fatal failures | Diagnostic plus Wasm trap / discarded execution instance, not a catchable language error |
| Host interop | The first slice's module imports nothing (§17.6); the browser binding is designed when the viewer needs it |
| NaN | Every NaN an operation makes is `$7FF8000000000000`, as on native targets; the selector implements §17.3 without branches |
| First slice | P6 through `NSHADOW`: unplaced emissions whose call and address immediates are fixed-width padded LEBs, patched by the linker as `NEMIT` rows (§17.1) |
| Optimizer | Habu-owned; engine native optimization is the embedding's job |
| First deployment | Single worker, one unshared memory, scalar instructions plus multi-value; no mandatory threads, GC, memory64 or JSPI |
| Proof posture | Preserve checker and ownership obligations; validate every stage; do not equate valid Wasm with semantic correctness |

## 2. Existing compiler boundaries

* `src/compiler/target.f` owns the immutable architecture/ABI/endian/address-width/features contract and its stable wire codes and digest. `src/compiler/native/backend.f` owns one private complete descriptor/pass row per loaded provider, registered by its `passes.f` module. `wasm` is a contract architecture, but an unloaded Wasm provider refuses with `E-CTGT-UNLOADED`.
* `src/compiler/native/backend.f` (`NBACK`) dispatches declaration, selection, emission and lifecycle through the target row. `NBACK:FREEZE` freezes the definition's HIR module and folds its loops before any selector runs, so a shadow target selects from the very module the engine's selector did. `EMIT` still takes a numeric code location; its sibling `EMIT-UNPLACED` writes a routine measured from no slot.
* `src/compiler/native/compiler.f` (`NCOMP`) consumes checker-observed source tape and publishes a pending definition only after the compilation chain succeeds (`:15-16`, `:515-527`). `LOAD-PASSES` loads the ARM64 or x86-64 passes by target and refuses any other with `E-CTGT-ABI` (`:18-24`, `:54-71`). NCOMP also compiles every definition for an open shadow binding (`:26-33`, `:482-506`).
* `src/compiler/native/elaborate.f` holds compile-time cell vectors with grouping for wide values; stack renaming and the admitted return-stack operations move value identities rather than implementing a runtime stack; quotation bodies become functions (`:4-13`). Native `does>` and the `?do` entry rule (ae15d38a) are newer than the first audit.
* `src/compiler/native/hir.f` has 47 opcodes, covering arithmetic, floating point, memory, branches, calls, quotations, traps and termination (`:51-99`); `TARGET` reads the immutable binding (`:203-206`).
* `src/compiler/native/frozen.f` reads frozen functions, blocks, values, operands and predecessor edges (`:98-196`), a usable backend input. Its one producer, `NBACK:FREEZE`, uses `IR-BUILD:FREEZE-INTERIM`, which skips the schema, dominance, terminator, single-definition, successor-argument and span checks (`src/compiler/ir/build.f:1476-1494`); native code relies on the verify of the emitted machine module.
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

The required base is `wasm` + memory32 + Habu64 + one unshared memory. Scalar
core and multi-value support are the baseline.
Additional instruction features are individually listed and validated; the
version number of an evolving WebAssembly specification is not a feature set.
[WW1]

The one product is the core module P6 builds (§17). No silent native fallback
is possible in a browser.

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

WPROF (src/arch/wasm/profile.f) states the one feature set the backend emits (core Wasm with multi-value and saturating float-to-int), the 16/16 lane ABI and the memory layout, as constants.

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

The context includes an ABI version, stack region and top, throw code, diagnostic record, call-frame scratch ownership and entry/reentrancy state.

### 7.3 Wide arity and browser-facing calls

The internal ABI uses one deterministic arity rule: up to 16 flattened input lanes and 16 output lanes, excluding context/status, use the fast signature; larger shapes use an explicit aligned call-frame variant. This threshold is a **pinned internal ABI parameter**, included in the Habu call-contract identity and shared by every caller/callee. It is not selected by an optimization heuristic. Changing it is an ABI revision.

Dynamic `execute`, quotation and defer adapters use `(ctx:i32)->i32 language_status` and canonical eight-byte Habu stack slots. An xt is its adapter's table slot; dispatch checks the stack depth against the slot's arity.

Browser-facing calls use JavaScript `BigInt` for `i64` and never `Number`, and
reinterpret exported `i32` address bits as unsigned before indexing views. [WS5]

No function, product or pointer layout is presumed to match the wasm32 C ABI. A
future C-compiled module interop adapter must describe the memory, allocator and
aggregate layouts explicitly.

## 8. Exceptions, traps and arithmetic parity

### 8.1 No native unwinder inside Wasm

Native Habu exception primitives can rely on native control transfer. That mechanism must not be ported as pointer arithmetic on a fabricated call stack.

The portable backend gives potentially throwing calls explicit status edges. A `throw` of zero returns normally. A nonzero throw stores its full Habu cell in the context and propagates status through generated cleanup/return paths. An unknown dynamic callee is conservatively potentially throwing; a no-throw certificate is validated against its dependency closure.

`catch` records the caller's stack depth and appropriate runtime marks, executes the adapter, and follows the existing checked catch contract on either result. **Depth restoration is not restoration of overwritten argument values, object ownership, heap contents or host side effects.** Current Habu source and issue records explicitly warn against recovering ownership from overwritten caught arguments. [R12]

Represent exceptional placeholder cells in the checker/IR as specified by the existing catch model. Do not turn zero placeholders into usable copies of a linear resource. Cleanup owners stay in safe outer scopes. `catch` does not imply rollback.

`finally` needs tests for normal return, body throw, cleanup throw and fatal exit; preserve existing cleanup precedence rather than selecting one incidentally.

### 8.2 Fatal failures are different

An impossible family tag, a false no-return certificate or unexpected engine failure must not become an ordinary catchable domain error. Habu already distinguishes native terminal traps. [R13]

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

Wasm permits more than one NaN result payload for some arithmetic [WS2], but Habu's NaN rule binds Wasm as it binds every target: an operation that makes a NaN answers `$7FF8000000000000`, and a quiet NaN operand passes through, the left of two ([portability.md](portability.md) §0.1, §10.1). The selector implements §17.3 without branches, preserving quiet NaN operands unchanged (the left of two) and replacing a NaN made from non-NaN operands with `$7FF8000000000000`; no profile selects the rule away. For a requested bit-exact policy beyond that rule, either prove a restricted operation/input set, supply bit-preserving software semantics, or refuse the unsupported case.

Disable contraction, reassociation and relaxed SIMD by default. A deterministic application protocol may normalize NaNs and reject nonfinite geometry values at its own documented serialization boundary, but that does not change Habu's bit-observation semantics behind the user's back. Transcendentals should use qualified Habu helpers or explicitly named host imports, not unexamined substitution with JavaScript math functions.

## 9. Execution tokens, definitions and reflection

A stored execution token is the typed table slot of its adapter, fixed for the module's life. Do not publish raw function indices.

All generic table adapters can share `(i32) -> i32`; consequently Wasm's dynamic signature check alone cannot distinguish Habu word effects, nominal types or linear ownership. Validate these Habu identities before dispatch. Wasm indirect calls provide only the Wasm-level signature check. [WS7]



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

## 12. Compile-time execution and host boundaries

### 12.3 Compile-time evaluation

Compile-time execution and target data are [portability.md](portability.md) §7
in full: `TargetRef`, `TargetObject`, the target-data builder and the audited
legacy owner adapter. The first slice uses that adapter, `NSHADOW` (§17.1).

### 12.4 WASI and GPU boundaries

The core backend is not tied to WASI. A dedicated runner provides the narrow test ABI (§17.6).

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

A witness bound to input/output hashes is only useful when the validator checks its claimed relation. Structural validation is not a semantic-preservation proof.

### 13.2 Resource isolation

Place maximums on source bytes, graph nodes, nesting, function arity/locals, type count, code bytes, table entries and memory.

## 14. Repository layout and integration changes

Physical layout follows [portability.md](portability.md) §26.1, and its moves
wait for P13. The Wasm path adds:

| Path | Holds |
|---|---|
| `src/arch/wasm/` | The Wasm backend: selector, WSTRUCT dialect, encoder, LEB routines and module linker, registered as a complete row through its `passes.f` module |
| `test/wasm/` | Wasm tests and fixtures |

When the backend needs more from an engine package (one compiled into bin/hb), that package gains the word and the engine is rebuilt.

## 15. Work packages

The Wasm path is P1a -> P6, with P0w alongside; it never waits on P2-P5
([portability.md](portability.md) §27).

| Package | Dot | Delivers |
|---|---|---|
| P0w | habu-pin-hbr2-wire-0b340032 | Wasm numeric goldens N01-N06 with the NaN print rows and the three `?do` rows, run natively |
| P1a | habu-add-the-wasm-4c32353e | The `wasm` architecture and ABI rows, the scalar-FP bit and the decoders, in one commit |
| P6 | habu-emit-a-wasm-05443776 | The first slice (§17) |
| P13 | habu-retire-target-selecting-affc4d65 | Retires target-selecting host predicates and does the moves |

## 16. Test matrix

### Semantic edge cases

Zero; minus one; signed min/max; high unsigned bits; carry/borrow; overflow multiplication; zero divisor; signed-min divided by minus-one; positive/negative remainder order; shift counts 0/63/64/65; a condition with only bit 40 set; boolean-mask bitwise use; NaN/signed-zero comparisons; subnormal/overflow floats; numeric conversions; literal boundary parsing.

### Control and type invariants

Nested `IF`/`ELSE`; loop-carried values; parallel-copy cycles; `DO` versus `?DO`; negative `+LOOP`; early exit and leave; balanced return-stack operations; wide locals; ADT payload variants and invalid tags; typed quotation mismatch; unreachable results; effectful loads/calls not reordered across stores or throws.

### Memory and host boundaries

Pointer upper bits; truncated pointers; multiplication in array bounds; length overflow; null reservation; one-past-end and zero-length spans; cross-object overwrites; resize preservation; failed `memory.grow`; stale views.

### Lifecycle and publication

Missing imports; wrong import type; unexpected extra authority; failed compile/instantiate/initialize; table limit; traps after partial writes; compiler resource exhaustion.

### Independent oracles

Use official Wasm validation through at least two independent execution engines where practical, plus WABT or another independent decoder/validator in development. WABT supplies tools such as `wasm-validate`, `wasm2wat` and `wat2wasm`. [WS11] The production backend emits binaries itself.

Generate well-typed Habu programs for differential execution against the pinned native/reference semantics. Compare observable outputs and explicit memory effects, not unspecified stack contents after throw or unspecified NaN payloads. Preserve failing seeds and input profiles.

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

The first slice lowers every tail call, self calls included, as call then return: not bounded-space. The self-tail loop, the trampoline and the Wasm tail-call feature are later profiles. `call; return` is not bounded-space tail recursion. Unknown dynamic calls carry the necessary checked effect and exceptional edges.

At every call and wordcall, the live row the HIR operation carries through is stored to the context stack and reloaded afterwards, as the native data-stack boundary stores it (`src/compiler/native/select-x64.f:685-693`). Arguments and results are typed lanes (§7.1), so `depth` and `.s` answer as they do natively.

### 17.5 Memory and constants

Logical pointers lower to canonical memory offsets, checked before narrowing. Reserve null/control/runtime/stack/static/heap regions under one target layout. The null reservation is not a Wasm guard page: address zero can otherwise be in bounds. Pointer operations enforce the language's intended null rule.

Wasm memory growth failure follows an explicit allocator error path. The memory page count must be widened before converting it to bytes. Numeric i64 values do not require memory64, and JavaScript i64 interchange uses BigInt rather than Number (§6). [WW2]

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

wasm-tools validates with the backend's features, and node (V8, the engine
Chromium uses) runs `test/wasm/run.mjs`. Together they form
the `wasm` device check ([bootstrap.md](bootstrap.md)), not the ordinary gate.
The module exports memory, run, throw-code, out-base and out-len and imports
nothing. The driver, `tools/wasm-build.f`, loads
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

[WS11] WebAssembly Binary Toolkit: `https://github.com/WebAssembly/wabt`

[WW1] WebAssembly module structure: `https://webassembly.github.io/spec/core/syntax/modules.html`

[WW2] WebAssembly JavaScript interface: `https://webassembly.github.io/spec/js-api/`

[WW3] WebAssembly binary module format: `https://webassembly.github.io/spec/core/binary/modules.html`

[WW4] WebAssembly tool conventions, linking: `https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md`, and LLD's Wasm documentation: `https://lld.llvm.org/WebAssembly.html`
