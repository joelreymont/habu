# Habu Compiler and GPU Code-Generation Redesign

**Status:** implementation design  
**Audience:** the implementation orchestrator and its coding agents  
**Repository audited:** `joelreymont/habu`  
**Audit commit:** `eb5742e916978d5c9067218737ce9c62a1af25a4`  
**Date:** 2026-07-26  
**Primary proof target:** Rocq, while keeping all IR contracts proof-system-independent

---

## 1. Executive decision

Habu should replace both of its current code-generation architectures with staged, typed, immutable intermediate representations.

This is not a recommendation to add one or two peepholes to the existing emitters. The current native compiler and the current PTX generator are organized around direct emission:

- the native path parses tokens, maintains an abstract stack cache, chooses registers, emits ARM instructions, patches branches, recognizes relocatable instruction stencils, and performs machine-code-based inlining during one tightly coupled process;
- the PTX path represents kernel values as PTX register numbers, renders instruction text immediately, declares fixed register arrays, and later reparses that text into a deliberately narrow optimization table;
- wide Habu values trigger a second compilation pass over source text rather than a lowering pass over an already captured, typed program;
- AOT capture recovers calls and relocations by decoding emitted machine words;
- schedule and MMA configuration are distributed across mutable globals and hand-written emitter paths.

That architecture creates three linked problems:

1. **Optimization is local and accidental.** Once semantics have been collapsed into instruction bytes or strings, most useful transformations are no longer available.
2. **Correctness is difficult to state.** There is no stable object between source semantics and target instructions on which to state pass invariants or semantic-preservation theorems.
3. **The implementation is brittle.** A semantic change, ABI change, instruction encoding, relocation convention, optimization, and proof obligation frequently meet in the same code.

The replacement architecture is:

```text
Native Habu
───────────
source bytes
  → resolved Habu IR
  → stack SSA
  → target-neutral low-level IR
  → AArch64 virtual-register IR
  → register allocation and block layout
  → typed AArch64 instructions
  → encoded bytes and relocatable object
  → ELF or Mach-O image

GPU / Maki
──────────
model IR
  → explicit region and fusion IR
  → logical kernel IR
  → scheduled GPU IR
  → structured PTX IR
  → PTX text
  → ptxas
```

There should be **one shared IR substrate**, but not one universal operation set. Habu source, tensor graphs, scheduled GPU kernels, PTX, and AArch64 are different languages and should remain different dialects with different invariants and semantics.

The optimizer or autotuner may remain untrusted. Every search-oriented transformation must produce an output and a witness accepted by an independent validator. Stable, deterministic lowering passes may later receive direct Rocq proofs.

The migration must preserve the current compiler as a shadow oracle until the new path covers the relevant production behavior. It must then retire the old path. Permanent dual compilation is explicitly forbidden.

---

## 2. Audit judgment

### 2.1 Native compiler

The native compiler is currently a direct machine-code generator.

`src/habu/habu2.f` combines the runtime compiler, control-flow handling, source handling, literal handling, quotations, locals, defining words, exception paths, branch patching, AOT support, and direct ARM emission. Its own header describes it as the JIT compiler and notes that emission order must remain stable for the self-rebuild fixpoint.

Several mechanisms demonstrate that machine code is being used as an intermediate representation even though it has no explicit type:

- relocatable addresses use a fixed four-instruction `MOVZ`/`MOVK` stencil so a later pass can recognize and rewrite them;
- call inlining scans already emitted instruction words, rejects selected branch, call, return, and ADR encodings, then copies the remaining machine words;
- control-flow words such as `IF`, `ELSE`, `LOOP`, `+LOOP`, `EXIT`, `DOES>`, and quotations directly construct instruction words and branch placeholders;
- some instruction sequences appear as decimal or hexadecimal instruction constants rather than typed AArch64 operations;
- `EXIT` and `LEAVE` maintain patch chains inside the code stream;
- the AOT capture path decodes direct branch instructions and fixed literal stencils to rediscover symbols and relocations.

This is compact, but it prevents global reasoning. By the time the compiler can inspect a sequence, it has lost:

- source operation identity;
- logical stack values;
- type and width information;
- block structure;
- dominance;
- def-use relationships;
- alias and effect information;
- symbolic call and address identity.

The current JIT stack cache is a useful local optimization, but it is not a general optimizer. It tracks constants and live physical registers in a small abstract value stack, spills the whole stack when it cannot continue, and uses a first-free bitmask allocator. Register ownership is modified in several files. There are no explicit live intervals, call-clobber constraints, interference checks, or block-wide allocation decisions.

The width-aware second pass is especially strong evidence that a typed IR is missing. The compiler records lowering facts in a source-bound certificate, then reparses and recompiles the definition from a sealed source copy. An explicit typed IR would retain value width and layout through normal lowering and eliminate this second source compilation.

**Judgment:** the native compiler should be restructured, not incrementally patched. The existing ARM encoder and label/fixup machinery are worth retaining behind a typed AArch64 IR. The direct token-to-instruction compiler, byte-scanning inliner, whole-stack spill model, pass-2 source recompile, and AOT machine-code scanner should be retired.

### 2.2 PTX generator

The PTX generator has a similar direct-emission architecture.

In `lib/ptx/cg.f`, the runtime representation of a `span`, `gridctx`, `tile`, or `uniform` is a PTX register number. Each operation allocates another monotonically increasing register number, renders a line through a string builder, and appends it to the PTX sink. The checked kernel body therefore doubles as an imperative text emitter.

This was a productive bootstrap mechanism. It is no longer an adequate backend architecture.

Consequences include:

- target register identity is conflated with semantic kernel value identity;
- register declarations are fixed-size arrays rather than derived from the actual program;
- address calculations, loads, stores, predicates, barriers, and arithmetic become text before optimization;
- the optimizer must parse Habu's own generated text back into records;
- unfamiliar, predicated, memory-touching, barrier, atomic, branch, and MMA instructions are intentionally opaque to that optimizer;
- optimization is limited to straight-line copy propagation, identical constants, common-subexpression elimination, dead pure operations, and self-moves;
- the real schedule is encoded in emitter control flow and mutable configuration variables;
- large GEMM and MMA emitters manually manage register-number ranges, shared-memory layouts, pipelines, fragment mappings, and special-case paths.

`lib/ptx/ir.f` is not the missing backend IR. It is a small expression DAG with a handful of operations and a 64-node cap. It can supply implementation patterns for interning and liveness, but it cannot represent real PTX control flow, memory effects, address spaces, barriers, asynchronous copies, reductions, MMA fragments, or complete kernels.

**Judgment:** kernel DSL words should build a logical kernel IR value, not emit PTX. Scheduling should transform that logical kernel into an explicit GPU schedule IR. PTX selection should produce structured instructions. Rendering should be the last step. The line-oriented PTX parser may remain as an external import and round-trip test tool, but it must leave the optimization-critical path.

### 2.3 Maki model IR

Maki already has a real graph IR. It should not be discarded.

`maki/model-ir.f` records a DAG of operations, operand windows, shape, dtype, layout, attributes, materialization, and autograd metadata. The fusion planner, memory classifier, schedule families, and lowerers already operate on meaningful model facts.

The problem is the boundary below that graph:

- the fusion planner mutates materialization facts back into the model table;
- region identity and schedule identity are spread across side tables and mutable globals;
- resource feasibility is incomplete;
- the lowerers transition directly from model/region facts to PTX text;
- schedule configurations become emitter-specific globals;
- there is no stable logical kernel between a fused tensor region and a target instruction stream.

**Judgment:** keep Maki model IR as the initial tensor front end. Introduce immutable region, kernel, and schedule IRs below it. Later, Maki's tables may migrate onto the common IR substrate, but that migration is not a prerequisite for replacing the PTX backend.

---

## 3. Goals

The redesign must achieve all of the following.

### 3.1 Native goals

- Preserve Habu's interactive compiler and REPL.
- Compile each definition atomically: no partial executable code becomes visible before validation and publication succeed.
- Represent structured control flow before machine-code selection.
- Convert stack operations into SSA value flow.
- Eliminate `dup`, `swap`, `over`, local references, and similar bookkeeping from generated code when they are compile-time value rearrangements.
- Track value type, cell width, representation, source span, and effect through lowering.
- Replace whole-stack spill decisions with liveness-driven materialization and individual spills.
- Replace machine-code scanning with symbolic calls, addresses, labels, and relocations.
- Support both fast JIT compilation and stronger AOT optimization.
- Produce canonical, content-addressed objects suitable for caching and linking.
- Preserve source maps for diagnostics, disassembly, profiling, and breakpoints.
- Expose stable proof subjects from resolved source through encoded AArch64 bytes and executable images.

### 3.2 GPU goals

- Keep model semantics separate from schedule and target lowering.
- Make fusion, movement dissolution, layout selection, vectorization, tiling, shared staging, pipelining, and tensorization explicit transformations.
- Represent memory references, index maps, masks, address spaces, alignment, alias classes, and numerical policy structurally.
- Derive PTX register declarations from the real program.
- Optimize structured PTX, not text.
- Compute resource estimates before emission and record actual `ptxas` resource results afterward.
- Permit untrusted heuristics, autotuners, and LLM proposals only behind verified validators.
- Distinguish exact IEEE preservation from explicitly bounded numerical refinement.
- Provide stable proof stages from model semantics through PTX semantics.

### 3.3 Formal-proof goals

- Give every IR a syntax, well-formedness predicate, operational or denotational semantics, and canonical serialization.
- Make every pass either:
  - directly proved correct; or
  - dependent on a proved validator that checks a pass-specific witness.
- Bind every witness to input digest, output digest, target contract, numeric policy, schema version, and pass identity.
- Eliminate hidden mutable configuration from proof-critical behavior.
- Make every remaining external assumption visible in the final theorem and in the live trust manifest.
- Support a CompCert-like composition theorem from concrete Habu source to AArch64 bytes, and a separate model-to-PTX theorem.

---

## 4. Non-goals

The first implementation must not attempt any of these.

- Reimplement MLIR inside Habu.
- Introduce a dynamically registered, open-ended dialect ecosystem.
- Build one universal IR whose operations mix tensor algebra, Habu quotations, AArch64, and PTX.
- Prove every pass before the executable pipeline exists.
- Replace the checker, parser, native compiler, Maki planner, PTX backend, linker, and image writers in one change.
- Implement an advanced graph-coloring register allocator before a correct linear-scan allocator exists.
- Build a general e-graph optimizer before ordinary SSA simplification and pass validation work.
- Claim bit-exact equality for reductions, tensor-core operations, approximations, or precision changes that are not bit exact.
- Keep both old and new compilers as permanent production options.
- Add Python, Rust, C++, shell logic, LLVM, or MLIR as implementation dependencies. Repository automation and implementation remain checked Habu, except for the existing audited recovery path and the separate Rocq proof tree.
- Preserve old emitted bytes as an architectural constraint. Byte parity is a useful migration oracle for selected slices, not the target design.

---

## 5. Architectural rules

These rules are mandatory because they simultaneously improve optimization, implementation clarity, and proof tractability.

### 5.1 Immutable pass inputs

A pass never mutates its input module.

It consumes a frozen module and builds a new module:

```text
PASS(input module, configuration)
  → output module, witness, metrics
```

Mutation is allowed only inside an unsealed builder that owns private append-only arenas. `FREEZE` consumes the builder, validates the complete module, computes its canonical digest, and returns an immutable module handle.

This makes pass composition explicit and prevents an optimizer from leaving half-updated side tables after an error.

### 5.2 IDs, not internal pointers

IR records refer to functions, blocks, operations, values, types, attributes, symbols, and source spans through nominal integer IDs.

No record contains a pointer to another record. Arena growth therefore cannot invalidate references, canonical serialization is straightforward, and Rocq semantics can use finite maps or vectors without modeling host pointer aliasing.

### 5.3 Closed-world operation schemas

Each dialect has an exhaustive operation family and one schema table.

The schema records:

- dialect and opcode;
- operand and result arity rules;
- type constraints;
- control-flow shape;
- terminator status;
- effect class;
- memory space and alias behavior;
- whether the operation may trap;
- legal target capabilities;
- semantic rule identifier;
- renderer identifier.

Do not implement operation behavior through arbitrary execution tokens in proof-critical code. Use exhaustive `MATCH` or table dispatch whose full domain is validated.

### 5.4 Explicit target contracts

No pass reads target facts from scattered global variables.

A target contract is an immutable value containing, as appropriate:

- architecture and feature set;
- ABI;
- register file and reserved registers;
- caller/callee-save masks;
- legal instruction forms;
- branch reach;
- endianness;
- pointer and cell width;
- PTX version and SM target;
- block, warp, shared-memory, register, and asynchronous-copy limits;
- supported dtype and MMA forms;
- external tool identity.

Its digest is part of every cached artifact and proof witness.

### 5.5 Explicit numerical policy

Floating-point transformations are never justified by a generic `commutative` or `associative` flag.

A compilation unit carries a numerical policy such as:

- `exact-ieee`;
- `allow-fma`;
- `ulp-bound N`;
- `absolute-bound E`;
- `relative-bound E`;
- a named mixed-precision/tensor-core policy.

Every optimization rule declares which policies admit it. The default low-level optimizer is bit exact.

### 5.6 No semantic reconstruction from bytes or text

The following are prohibited in the new pipeline:

- decoding emitted ARM words to find calls;
- recognizing literal stencils to infer relocations;
- copying machine-code ranges to inline;
- reparsing generated PTX in order to optimize it;
- deriving source-level value identity from register spelling;
- deriving schedule facts from rendered text.

All such facts must exist before rendering.

### 5.7 Deterministic construction

Given identical source bytes, environment manifests, target contract, numeric policy, and pass configuration, every stage must produce the same canonical module and digest.

Do not rely on hash-table iteration order, allocation addresses, process IDs, temporary paths, or target timing while constructing semantic artifacts. Autotuning measurements select among already identified candidates and are stored separately from candidate identity.

### 5.8 Validation before publication

A definition, object, kernel, or optimized candidate is not visible until:

1. all stage validators pass;
2. all required witnesses pass;
3. encoding or PTX rendering succeeds;
4. target assembly succeeds where required;
5. required golden/device checks succeed for promoted GPU artifacts;
6. the enclosing transaction commits.

---

## 6. Shared IR substrate

The shared substrate provides storage, identity, validation, serialization, and pass plumbing. It does not define the meaning of dialect-specific operations.

### 6.1 Core nominal IDs

Define sealed nominal single-cell families:

```text
ir-module-id
ir-fun-id
ir-block-id
ir-op-id
ir-value-id
ir-type-id
ir-attr-id
ir-symbol-id
ir-span-id
ir-pool-offset
ir-count
```

Raw index conversion is private to the owning package. The conversion authority should be concentrated in one generic indexed-arena implementation rather than repeated as trusted casts in every dialect.

A frozen module validates every ID against the corresponding table bound. Public readers accept only nominal IDs.

### 6.2 Module ownership

Use an explicit compilation context.

Conceptually:

```text
compiler-context
  target contract
  numeric policy
  source registry
  diagnostic sink
  scratch allocator
  module allocator
  witness allocator
  metrics
```

Builders are uniquely owned:

```text
NEW-BUILDER  ( compiler-context dialect -- ir-builder )
FREEZE       ( ir-builder -- ir-module )
ABORT        ( ir-builder -- )
```

A frozen module is immutable and may be shared by later read-only passes. A pass result owns its new module and witness until the caller either promotes or releases them.

Habu's linear type facilities should ultimately enforce builder and context ownership. During the earliest implementation, an explicit generation token and fail-closed lifecycle checks are acceptable, but the public API must already have the ownership shape above.

### 6.3 Tables

Use append-only cell tables with geometric growth and committed ceilings. In-memory indices may use native cells; canonical serialization uses bounded unsigned integers.

#### Function table

Each function record contains:

```text
symbol-id
signature-type-id
first-block
block-count
attr-window
source-span
linkage
calling-convention
flags
```

#### Block table

Each block record contains:

```text
parent-fun
argument-window
op-window
terminator-op
predecessor-count
successor-count
source-span
flags
```

Predecessor and successor tables are derived at freeze time rather than maintained through every builder mutation.

#### Operation table

Each operation record contains:

```text
dialect-opcode
parent-block
operand-window
result-window
attr-window
successor-window
source-span
effect-class
flags
```

#### Value table

Each value record contains:

```text
type-id
definition-kind       # block argument or operation result
definition-id
definition-position
flags
```

Use lists are derived after freeze. A first implementation may compute use counts only; later passes can build a compact use index.

#### Type table

Types are canonical and interned. Required kinds include:

```text
integer(width, signedness, role)
float(format)
pointer(address-space, pointee, alignment)
tuple/layout(family, fields, cell-width)
quotation(effect)
code-reference(effect)
memory-token(effect-domain)
target-register-class
tensor/memref(element, rank, extents, strides, layout, address-space)
mask/domain token
opaque external handle
```

HIR should snapshot the checker's semantic type graph into this table. It must not retain pointers into the checker's scratch arenas.

#### Attribute table

Attributes are canonical immutable values:

```text
integer
boolean
string
symbol
type
value list
integer list
enum
nested record
digest
```

Attribute keys are sorted canonically at freeze time. Unknown attributes fail unless the dialect schema explicitly permits an extension set.

#### Source-span table

A span contains:

```text
source-file-id
byte-start
byte-end
line
column
origin-kind
parent-span
```

`origin-kind` distinguishes source text, generated declaration, inlining, lowering expansion, and synthesized control flow. Parent spans form an origin chain.

### 6.4 Builder API

The common builder API should be small:

```text
BEGIN-FUN
END-FUN

BEGIN-BLOCK
ADD-BLOCK-ARG
END-BLOCK

ADD-OP
ADD-OPERAND
ADD-RESULT
ADD-SUCCESSOR
ADD-ATTR

INTERN-TYPE
INTERN-SYMBOL
INTERN-ATTR
ADD-SPAN
```

Dialect packages wrap these generic words with typed constructors. Ordinary compiler code should not manually write record fields.

### 6.5 Freeze validation

`IR:FREEZE` must perform all structural checks:

- every referenced ID exists and has the expected kind;
- every operation belongs to exactly one block;
- every block belongs to exactly one function;
- operation windows and pool windows are in range and non-overlapping where required;
- every block ends in exactly one terminator;
- successor argument counts and types match destination block arguments;
- every value has exactly one definition;
- operands refer only to visible values;
- dominance holds for SSA dialects;
- operation schema arity and type rules hold;
- attributes are canonical and legal for the opcode;
- effect and memory tokens are well formed;
- symbols obey uniqueness and linkage rules;
- source spans are valid slices of registered source;
- target-specific operations are legal for the target contract;
- the module contains no builder-only placeholder.

Each failure has a named code and reports stage, function, block, operation, source span, expected invariant, and actual value.

### 6.6 Canonical serialization

A frozen module serializes in this order:

```text
header
target and numeric-policy digests
dialect/schema versions
string and symbol tables
type table
attribute table
source table
function table
block table
operation table
value table
operand/result/successor pools
optional derived indices
```

The encoding includes explicit counts and lengths; it does not serialize host addresses or arena capacities.

Provide:

```text
IR:ENCODE
IR:DECODE
IR:DIGEST
IR:RENDER
IR:DIFF
```

`IR:RENDER` is diagnostic text. It is not parsed by the compiler.

### 6.7 Pass result and witness header

Every pass returns:

```text
pass-result
  output-module
  witness
  metrics
```

Every witness begins with:

```text
magic
format-version
pass-id
pass-version
input-module-digest
output-module-digest
target-contract-digest
numeric-policy-digest
schema-digest
payload-length
payload-digest
```

A witness is rejected before its pass-specific payload is read if any binding differs.

---

## 7. Native compiler pipeline

### 7.1 Stage N0: source tape

Before HIR, capture the exact token stream consumed by the compiler.

A source-tape token contains:

```text
token kind
byte span
resolved spelling slice
literal value where applicable
parser mode
origin
```

The source tape is not a full syntax tree. It exists to guarantee that checking, elaboration, diagnostics, and compilation refer to the same bytes.

During migration, the existing checker may continue to run over source text, but its result and lowering certificate must bind to the source-tape digest. Long term, the checker and HIR elaborator consume the same tape.

Compile-time immediate words divide into three classes:

1. **Front-end intrinsics.** Registered compiler words such as `IF`, `LOOP`, locals, quotations, `POSTPONE`, and defining forms call HIR builder operations.
2. **Compile-time computation.** A checked immediate may execute during elaboration but may affect the generated program only through a sealed HIR-builder capability.
3. **Unmodeled boundary.** An immediate without a registered elaboration contract is rejected in checked source. A temporary legacy boundary must be named, tested, inventoried, and scheduled for retirement.

No immediate word receives access to AArch64 emission in the new path.

### 7.2 Stage N1: HIR — resolved Habu IR

HIR preserves Habu's structured source semantics.

Required operations include:

```text
constant
string/counted-string literal
resolved call
primitive call
local bind
local reference
quotation
execute
if
loop
do-loop
leave
exit
return
recurse
throw
catch/evaluate boundary
create/variable/constant metadata
defer/is
does>
type constructor
match
raw external operation
```

HIR has structured regions for `if`, loops, and quotations. It retains:

- declared and inferred effects;
- checker type IDs;
- exact cell widths and layouts;
- word and package identity;
- primitive-effect identity;
- source spans;
- trusted/external contract identity;
- compilation-mode facts.

HIR validation checks the structured language rules but does not repeat the full type checker. Instead it verifies that the elaborated operations correspond to an accepted, source-bound checker certificate and that every referenced effect/type identity exists in the frozen environment manifest.

#### Definition publication

A colon definition is provisional while HIR is built. Its symbol may be referenced by `RECURSE`, but no dictionary record points to executable code until all later stages succeed.

### 7.3 Stage N2: SIR — stack SSA

SIR is the principal optimization IR for native Habu.

It is conventional block-parameter SSA, but its construction is driven by Habu's two logical stacks.

The converter maintains compile-time vectors:

```text
data-stack-values
return-stack-values
local-bindings
active-loop-frames
```

Operations such as `DUP`, `DROP`, `SWAP`, `OVER`, `NIP`, `ROT`, and local reference normally change these vectors only. They produce no SIR operation and therefore no runtime instruction.

#### Basic blocks

Each SIR block has explicit arguments for every live value entering it. At a branch, the predecessor passes values to the successor. This replaces implicit stack-state reconciliation.

Example:

```forth
: MAX2 ( a b -- m )
   2dup < if swap then drop ;
```

Conceptual SIR:

```text
entry(%a:i64, %b:i64):
  %lt = icmp.slt %a, %b
  cond-branch %lt, then(%a, %b), join(%a, %b)

then(%a, %b):
  branch join(%b, %a)

join(%top, %drop):
  return %top
```

There is no generated `2dup`, `swap`, or `drop`.

#### Loops

Loop headers receive block arguments for every loop-carried value and explicit loop-frame values. Back edges pass the next values. `LEAVE` branches to the loop-exit block with the required exit stack. `I` and `J` read explicit loop-frame values.

#### `EXIT`

Every function has one canonical exit block. An `EXIT` branches to it with the declared output values. The final return sequence is emitted once.

#### Quotations

A quotation becomes a nested or module-local function with a known effect. Because Habu forbids capturing enclosing locals, it requires no environment object. The quotation value is a typed code reference.

`EXECUTE` becomes an indirect call whose input/output effect is already known. Unknown-effect execution remains rejected.

#### Return stack

The return stack is a distinct SSA value vector. Operations such as `>R`, `R>`, and `R@` manipulate it structurally. At call or external boundaries where the runtime ABI exposes a physical return stack, lowering materializes the required portion.

#### Effects and memory tokens

Side-effecting SIR operations take and return explicit effect tokens. At minimum define domains for:

```text
data memory
dictionary state
code publication
I/O and syscalls
process state
external FFI
```

Pure arithmetic has no token. The token chain prevents illegal reordering and makes dead-operation elimination sound.

### 7.4 SIR optimizations

Implement a small, exact default pipeline:

```text
canonicalize block arguments
constant propagation and folding
copy/identity elimination
dead pure operation elimination
unreachable block removal
conditional branch folding
block merge and jump threading
local common-subexpression elimination
load forwarding within a proven alias class
redundant stack-home store/load removal
```

Do not mark floating operations algebraically commutative or associative under `exact-ieee`.

Add inlining only after the basic pipeline is stable. The inliner operates on SIR, uses an explicit cost model, clones blocks and values, and emits a substitution witness. It never inspects machine bytes.

### 7.5 Stage N3: LIR — target-neutral low-level IR

LIR removes language-specific abstractions while retaining SSA and explicit control flow.

Required scalar and memory types:

```text
i1 i8 i16 i32 i64
f16 bf16 f32 f64
pointer(address-space)
code-pointer
```

Required operations:

```text
integer arithmetic and comparisons
floating operations with explicit rounding/contract mode
extend/truncate/bitcast
address calculation
load/store with width, alignment, volatility, and alias class
direct and indirect call
syscall/external call
memory fence
trap
branch/conditional branch/switch
return
```

Wide Habu values lower once according to an explicit representation descriptor. This is where products, sums, tags, padding, and multi-cell values become scalar SSA values or memory aggregates. Because HIR and SIR retain the checker layout identity, there is no second source compilation.

The representation-lowering witness records, per source value:

```text
source type/layout
ordered LIR components
tag and payload mapping
padding policy
memory representation where materialized
```

### 7.6 Calling convention and stack homes

The first implementation preserves the current public Habu word ABI:

- the Habu data-stack pointer remains in its current dedicated register;
- externally callable words receive inputs and publish outputs through canonical data-stack slots;
- the runtime return stack and exception machinery retain their existing ABI;
- reserved engine registers remain target-contract facts.

Inside a function, values are SSA and register allocated.

Before a non-inlined call:

1. materialize only the caller values required by the callee ABI;
2. preserve live values according to call-clobber rules;
3. emit a symbolic direct or indirect call;
4. define returned SSA values from the canonical output locations;
5. leave unrelated values in registers when the ABI and liveness permit.

This is already superior to unconditional whole-stack spilling. A later internal fast calling convention may pass short known effects in registers, but it must be a separate, validated ABI transformation rather than an implicit stack-cache convention.

### 7.7 Stage N4: A64IR — AArch64 virtual-register IR

A64IR contains target instructions and pseudos, but no encoded words.

Virtual register classes:

```text
gpr32
gpr64
fpr16
fpr32
fpr64
predicate/flags dependency
```

Pseudos include:

```text
CONST64
SYMBOL-ADDRESS
DATA-ADDRESS
CODE-ADDRESS
CALL-SYMBOL
CALL-INDIRECT
BRANCH
COND-BRANCH
PROLOGUE
EPILOGUE
SPILL
RELOAD
PARALLEL-COPY
STACK-HOME-LOAD
STACK-HOME-STORE
```

Concrete operations correspond to the subset supported by `src/arch/arm64/asm.f`.

Each instruction records:

- operand and result virtual registers;
- fixed-register constraints;
- use/def masks;
- call-clobber mask;
- condition-code use/def;
- memory effect;
- source span;
- relocation, if any.

No compiler stage above encoding may contain a raw ARM instruction integer.

### 7.8 Instruction selection

Instruction selection lowers each LIR operation to one or more A64IR operations.

Use table-driven patterns for common cases:

- immediate versus register arithmetic;
- shifted operands;
- compare-and-branch;
- load/store addressing modes;
- constant materialization;
- address calculation;
- direct versus indirect call;
- boolean materialization;
- floating operations.

Selection must preserve symbolic addresses. `CONST64` is for numeric bits only; code and data addresses use distinct pseudos and relocation kinds.

### 7.9 Liveness and register allocation

Start with linear scan.

The allocator computes:

- block order;
- instruction positions;
- value live intervals;
- fixed/precolored intervals;
- call-crossing intervals;
- register-class demand;
- spill costs;
- rematerializable values.

Register files and restrictions come from the target contract. Reserved engine registers are never allocatable. Darwin's reserved `x18` remains forbidden by the target contract, not by scattered emitter checks.

Allocation rules:

- separate GPR and FPR pools;
- detect interval interference;
- respect fixed-register constraints;
- spill or assign callee-saved locations for values live across calls;
- allocate machine-stack spill slots, never Habu data-stack slots;
- coalesce copies where intervals permit;
- rematerialize constants and symbolic addresses where cheaper than spilling;
- validate the final assignment independently.

The allocation witness contains:

```text
virtual register
live interval or interval pieces
assigned physical register or spill slot
fixed constraints
call-crossing classification
```

The validator rejects overlapping intervals in one physical register, illegal register classes, reserved registers, insufficient spill slots, and unmet fixed constraints.

Graph coloring is a later option, not a prerequisite.

### 7.10 Block layout, branches, and relocations

After allocation:

1. choose deterministic block order;
2. remove branches to the immediately following block where legal;
3. lower block-argument parallel copies;
4. select short conditional forms;
5. create typed labels and relocations;
6. check branch reach;
7. insert veneers only through an explicit, validated transformation if required.

A relocation record contains:

```text
section
byte offset
relocation kind
symbol
addend
width
range contract
```

No later stage scans instruction bits to infer it.

### 7.11 Encoding and object construction

`A64ENC` consumes physical A64IR and calls the existing encoder words. The existing label/fixup implementation may be reused after its input is converted from ad hoc emitter calls to typed instructions and relocations.

The encoding validator checks:

- instruction schema versus operands;
- encoder result versus expected decoded instruction;
- label/fixup completeness;
- reach and alignment;
- little-endian byte placement;
- relocation field boundaries;
- instruction-to-byte source map.

The compiler emits an in-memory relocatable object containing:

```text
text
rodata
data
bss requirements
symbols
relocations
function metadata
source map
effect/signature metadata
compiler and checker digests
proof/witness references
```

Introduce canonical `HBOBJ 2`. Its proof-critical representation is structured data and canonical bytes. A separate renderer provides readable text. Do not make a textual object parser part of code generation.

The ELF and Mach-O writers consume this object. AOT capture no longer decodes `BL` instructions or literal stencils.

### 7.12 Definition transaction

The complete compile transaction is:

```text
capture source tape
check and elaborate HIR
freeze HIR
lower to SIR
optimize SIR
lower to LIR
select A64IR
allocate registers
layout and encode object
validate every stage
make code executable
publish dictionary record and metadata
commit
```

On any failure:

```text
release builders/modules
discard object bytes
restore dictionary/data/code marks
emit a stage-specific diagnostic
leave no published word
```

### 7.13 JIT and AOT modes

#### JIT O0

- build one definition;
- stack-to-SSA;
- minimal canonicalization;
- instruction selection;
- linear-scan allocation;
- encode and publish.

#### JIT O1 — default target

Adds:

- exact constant folding;
- CFG simplification;
- local CSE;
- dead code elimination;
- stack-home forwarding;
- small IR inlining where compile-latency budget permits.

#### AOT O2

Adds:

- module summaries;
- broader inlining;
- interprocedural dead function elimination;
- constant argument specialization;
- repeated address/load hoisting under proven effects;
- object-level tree shaking.

O-level behavior is part of the compiler configuration digest.

---

## 8. GPU pipeline

### 8.1 Stage G0: model IR

Keep current Maki model IR initially.

Create a frozen adapter view that snapshots:

```text
model name and provenance
input descriptors
nodes and ordered operand references
op identity and attributes
shape, dtype, and layout
alignment facts
autograd metadata
model outputs
```

The adapter computes a digest. Downstream passes consume the frozen view and do not mutate `maki/model-ir.f`.

Long term, Maki may write directly to the common IR substrate. Do not block backend replacement on that migration.

### 8.2 Stage G1: RIR — region and fusion IR

The fusion planner produces a new RIR module rather than writing region and materialization facts back into model IR.

A region record contains:

```text
region id
ordered member nodes
external input values
materialized outputs
region class
movement/view folds
numeric policy
backend capability requirements
split reasons
source model digest
```

RIR validation proves structural facts:

- every compute node belongs to exactly one region;
- region membership respects dependency order;
- each internal use resolves within the region;
- every cross-region use is an explicit region input/output;
- every model output remains observable;
- movement dissolution has a valid index-map interpretation;
- duplicated computation is explicit and policy permitted;
- effectful operations are neither dropped nor duplicated;
- region class and backend requirements are consistent.

The fusion witness maps each model node and value to its RIR location and boundary behavior.

The existing greedy planner may remain the producer. It becomes untrusted relative to the validator.

### 8.3 Stage G2: KIR — logical kernel IR

Each RIR region lowers to a target-independent logical kernel.

KIR is SSA with explicit index domains and memory references.

#### KIR values

```text
scalar
vector
tile
tensor fragment
index
mask
uniform
memref
memory token
```

#### Memref

A memref records:

```text
base parameter or allocation
address space
element type
rank
extents
strides
layout
alignment
alias group
mutability
lifetime
```

#### Index-map DAG

Addressing is represented structurally:

```text
constant
logical axis
add/subtract/multiply
floor-divide
remainder
min/max
select
affine map
validated non-affine map
```

Broadcast, transpose, reshape, slice, gather, and row selection are index maps, not hand-emitted PTX arithmetic strings.

#### KIR operations

Required initial set:

```text
parameter
logical-axis
mask-from-domain
load
store
atomic/reduction store
elementwise unary/binary/ternary
cast
reduce
matmul/contraction
view
barrier requirement
return
```

`reduce` records operator, identity, domain, accumulation type, and numeric policy. `matmul` records contraction axes, operand layouts, accumulation policy, and output mapping.

The checked kernel DSL should build KIR value IDs. The runtime cell carried by `span`, `tile`, `uniform`, and related phantom types becomes a KIR value handle, not a PTX register number.

This preserves the attractive user surface:

```forth
x g LOAD  a SCALE  y g LOAD  +.  y g STORE
```

but the implementation becomes:

```text
LOAD  → add KIR load op, return KIR value id
SCALE → add KIR multiply op, return KIR value id
+.    → add KIR add op, return KIR value id
STORE → add KIR store op
```

The existing `PTXREP` pattern may temporarily concentrate the value-handle representation boundary. The long-term goal is a checker capability for type-indexed IR handles so these operations become fully checked and the current trusted register/phantom coercions disappear.

### 8.4 KIR canonicalization

Before scheduling:

```text
constant folding under numeric policy
view/index-map composition
common index-expression elimination
common load elimination under alias/effect proof
dead pure value elimination
broadcast hoisting
redundant cast elimination
store-to-load forwarding where legal
region output pruning
```

KIR remains independent of block size, warp layout, shared memory, PTX registers, and target instruction spelling.

### 8.5 Stage G3: GIR — scheduled GPU IR

GIR makes execution mapping explicit.

It contains:

```text
loop nests and bounds
tile partitions
thread/block/warp/lane mappings
vector lanes
predication
shared-memory allocations
global-to-shared copies
asynchronous-copy groups
barriers
pipeline stages
unroll factors
MMA fragment layouts
epilogues
```

A schedule is a value, not mutable emitter globals.

For example, a GEMM schedule record includes:

```text
BM BN BK
warps
warp-grid shape
stages
vector widths
A/B shared layouts and padding
async-copy mode
MMA opcode and fragment map
accumulator layout
epilogue plan
dynamic shared-memory requirement
```

This replaces global variables such as MMA tile width, K depth, stage count, padding, warp count, fragment count, and feed mode.

#### Schedule transformations

Represent each schedule decision as an explicit operation over domains:

```text
split
tile
reorder
fuse-loop
map-to-block
map-to-warp
map-to-lane
vectorize
predicate-tail
stage-in-shared
pipeline
unroll
tensorize
materialize
```

The scheduler or autotuner may generate these operations. A validator checks their composition.

#### Schedule witness

The witness records:

- original logical domains;
- transformed domains;
- forward and inverse index maps where required;
- coverage and disjointness facts;
- tail predicates;
- memory-space substitutions;
- synchronization points;
- producer/consumer stage ordering;
- tensor-fragment mapping;
- resource-accounting inputs.

Validation must establish:

- every required logical output element is produced;
- no output is produced twice unless the operation is an allowed reduction/atomic;
- every active memory access is in bounds;
- vector lanes cover exactly the scalar domain under the tail mask;
- shared copies provide the values later loaded;
- no read occurs before the producing async group/barrier;
- every barrier is convergent for participating threads;
- tensorized fragments correspond to the logical contraction;
- numerical policy admits any changed operation order or precision.

### 8.6 Resource model

Before PTX selection, compute:

```text
threads per block
warps per block
shared bytes per allocation and total
dynamic shared bytes
barrier count
async-copy groups
estimated live scalar values by class
estimated accumulator registers
estimated predicate pressure
target feature requirements
```

Reject an impossible schedule before text is emitted.

After `ptxas`, record actual:

```text
registers per thread
shared and constant memory
spill stores/loads
stack frame
cubin size
warnings
```

The target registry stores both estimate and actual result. A persistent discrepancy beyond policy is a regression or a model-calibration task, not silently ignored.

### 8.7 Stage G4: PTXIR — structured PTX

PTXIR is a typed CFG with virtual values.

Instruction records contain:

```text
opcode
type and width
rounding/saturation/flush modifiers
cache and memory-order modifiers
destination values
source values/immediates
predicate
address expression or memory operand
target capability requirement
source span
effect
```

Required groups:

```text
integer and floating arithmetic
conversion
predicate operations
special-register reads
address-space conversion
loads and stores
atomics and reductions
branches and calls
barriers
cp.async and commit/wait
shuffle/vote
MMA and ldmatrix
return
```

Unknown PTX is not representable in generated modules unless carried as an explicit `external-ptx` boundary with a named semantic contract. The compiler's own generated instructions are always structured.

### 8.8 PTX optimization

Port the existing exact rules to structured PTXIR:

```text
copy propagation
identical constant reuse
common-subexpression elimination
dead pure instruction elimination
self-move elimination
predicate constant folding
branch simplification
address-expression CSE
redundant cvta elimination
load reuse under explicit alias/effect conditions
```

The exact optimizer never:

- reorders side effects;
- changes rounding;
- contracts multiply/add into FMA;
- reassociates floating reductions;
- crosses barriers, atomics, volatile memory, or unknown effects.

Numerically changing transformations belong to named higher-level passes admitted by the numeric policy.

### 8.9 PTX register naming and rendering

Initially assign one dense virtual register number per live PTXIR value, separated by PTX class:

```text
%p
%r
%rd
%f
%fd
fragment register classes
```

Render exact declarations from the maximum assigned IDs. Do not declare fixed `<32>` arrays.

PTXAS performs physical register allocation, so custom virtual-name reuse is not initially required. The compiler's resource estimate comes from IR liveness; actual occupancy facts come from `ptxas`.

The renderer is a pure final pass:

```text
PTXIR module → PTX bytes
```

Provide a parser only for external PTX tests, differential checks, and optional future import. The generated optimizer does not invoke it.

### 8.10 GPU artifact promotion

A GPU candidate is promotable only when:

```text
RIR validates
KIR validates
GIR schedule validates
PTXIR validates
PTX renders
ptxas accepts
launch contract validates
device golden passes
required numerical or gradient checks pass
profile is admissible
```

The artifact record binds:

```text
model digest
region digest
schedule digest
target digest
numeric policy
PTX digest
cubin digest
validation witness digests
device evidence
measurement evidence
```

---

## 9. Optimization strategy

### 9.1 Optimization levels are contracts

Each optimization level has an explicit pass list. A cache key includes the ordered pass IDs and versions.

No hidden peephole runs merely because an emitter happens to notice a pattern.

### 9.2 Stable passes versus search passes

#### Direct-proof candidates

These are deterministic and stable enough to prove as functions:

- stack-to-SSA conversion;
- basic constant folding;
- dead pure operation elimination;
- CFG simplification;
- representation lowering;
- AArch64 instruction encoding;
- branch/fixup encoding;
- PTX exact copy propagation/CSE/DCE;
- canonical rendering and serialization.

#### Validator candidates

These are better treated as untrusted producers:

- inlining selection;
- fusion;
- layout selection;
- tiling;
- loop reordering;
- vectorization;
- shared staging;
- pipeline depth;
- tensorization;
- register allocation;
- block layout;
- autotuning.

The corresponding validators are small enough to prove and remain stable while heuristics evolve.

### 9.3 No generic floating algebra

Integer rules must state overflow semantics. Floating rules must state IEEE format, rounding, NaN behavior, signed-zero behavior, and contraction.

Examples:

```text
x + 0 → x
```

is not universally bit exact for IEEE floating point because of signed zero and NaN behavior. It may be admitted only under a policy or a proved restricted condition.

Likewise, operand canonicalization for floating addition/multiplication is not automatically bit exact when NaN payload behavior matters.

The optimizer schema therefore must not inherit the current expression IR's integer-style commutative canonicalization for arbitrary floating operations.

### 9.4 Performance facts are not semantic facts

Timing may select a validated schedule. Timing never validates a schedule.

Resource estimates and `ptxas` results are optimization facts. Bounds, domain coverage, alias legality, barrier convergence, and semantic preservation are correctness facts.

---

## 10. Formal-verification integration

### 10.1 Proof architecture

The Rocq tree should mirror the executable stages:

```text
formal/
  Common/
    Ids.v
    Tables.v
    Digest.v
    Trace.v
  Habu/
    Source.v
    HIR.v
    SIR.v
    LIR.v
    TypeSoundness.v
  Native/
    A64IR.v
    RegallocValidator.v
    LayoutValidator.v
    Encoding.v
    Image.v
  GPU/
    Model.v
    RIR.v
    FusionValidator.v
    KIR.v
    GIR.v
    ScheduleValidator.v
    PTX.v
    PTXOpt.v
  EndToEnd/
    Native.v
    GPU.v
    Assumptions.v
```

The executable repository does not depend on Rocq at runtime.

### 10.2 Per-stage proof obligations

Every stage supplies:

1. syntax;
2. well-formedness;
3. semantics;
4. executable validator;
5. canonical encoding;
6. pass theorem or validator-soundness theorem.

Conceptually:

```text
validate-pass input output witness = true
  ⇒ semantics(output) refines semantics(input)
```

The end-to-end theorem composes these refinements.

### 10.3 Refinement direction

Use target-behavior inclusion:

```text
behaviors(target) ⊆ behaviors(source)
```

For deterministic, well-defined exact programs this often yields equality. Inclusion remains appropriate for source nondeterminism, external calls, traps, and refined undefined behavior.

### 10.4 Numeric refinement

GPU semantics are indexed by numerical policy.

Define separate relations for:

```text
bit-exact
ULP bounded
absolute bounded
relative bounded
named mixed-precision policy
```

A pass theorem names the relation it preserves. The final GPU theorem cannot silently strengthen a bounded policy into exact equality.

### 10.5 Trust and assumptions

Every production trust row eventually becomes one of:

```text
proved theorem
instance of a proved generic representation theorem
validated certificate
external axiom
```

The proof gate emits its complete assumptions. CI compares that set to the committed external-assumption manifest and fails on unexpected growth, `Admitted`, or an unbound theorem reference.

### 10.6 Implementation/proof synchronization

Every dialect and witness format has:

```text
schema name
major/minor version
canonical schema manifest
schema digest
```

A semantics-changing schema edit increments the major version and requires proof-owner review. A rendering-only extension may increment the minor version if canonical semantics are unchanged.

The implementation branch must expose stable canonical fixtures for the proof branch:

- valid modules;
- one mutation per rejected invariant;
- pass input/output/witness triplets;
- encoding vectors;
- source-to-output traces.

---

## 11. Self-hosting and bootstrap

### 11.1 New compiler as ordinary checked Habu

Most of the new compiler should be ordinary checked Habu modules, not hand-written ARM-emitter bodies in `habu2.f`.

Migration path:

1. the existing compiler compiles the new compiler modules;
2. the AOT seed or object path installs those compiled words into a candidate engine;
3. the new compiler compiles representative source in shadow mode;
4. the new compiler compiles its own modules;
5. the resulting candidate rebuilds itself to a byte-identical fixpoint under the new compiler;
6. the old direct compiler is removed from the production image except for the minimal audited recovery seed required by bootstrap policy.

This can substantially shrink the special builder surface even if the final binary grows temporarily during migration.

### 11.2 Bootstrap theorem

The ultimate release proof should connect:

```text
proved compiler model
  → verified compiler artifact
  → concrete bin/hb bytes
```

The current fixpoint remains valuable:

```text
verified/reference compiler output
  = native new compiler output
  = next-stage output
```

But fixpoint equality is not substituted for semantic preservation.

### 11.3 Runtime JIT validation

The final JIT path should validate every produced object before making it executable. At minimum:

- all IR freeze validators;
- register-allocation validator;
- branch/layout validator;
- encoder/fixup validator;
- source/effect digest binding.

As proofs land, these validators become the executable checkers justified by Rocq.

---

## 12. What is retained, replaced, and retired

### 12.1 Retain and adapt

- the checker and source-bound certificate concepts;
- Maki model IR as the initial tensor front end;
- operation registry, shape, dtype, layout, alignment, and target registries;
- bounded schedule-family enumeration as a candidate producer;
- CUDA driver and launch-contract layers;
- device golden, sentinel, gradcheck, and profiling harnesses;
- ARM64 instruction encoders;
- label/fixup range checks;
- ELF and Mach-O image writers;
- content-addressed cache and object-store concepts;
- PTX target/header/toolchain handling;
- current exact PTX optimization rules as specifications.

### 12.2 Replace

- direct token-to-ARM emission with HIR/SIR/LIR/A64IR;
- virtual-stack physical-register tags with SSA values and liveness;
- bitmask first-free allocation with validated linear scan;
- raw instruction constants in compiler logic with typed A64IR operations;
- machine-code inlining with SIR inlining;
- pass-2 source recompile with representation lowering;
- AOT code scanning with emitted symbols and relocations;
- PTX register-number semantic values with KIR value IDs;
- immediate PTX string emission with structured KIR/GIR/PTXIR;
- line-oriented PTX optimization with PTXIR optimization;
- mutable MMA configuration globals with immutable schedule values;
- fixed PTX register declarations with exact declarations;
- fusion/materialization mutation of model IR with explicit RIR.

### 12.3 Retire after cutover

The production responsibilities of these files should disappear or shrink to compatibility/bootstrap shims:

```text
src/habu/jit.f
src/habu/regalloc.f
the direct user-definition compiler portions of src/habu/habu2.f
src/habu/aot-capture.f machine-code scanning
lib/ptx/opt-ir.f from the generated-code path
the string-emitting operation bodies in lib/ptx/cg*.f
lib/ptx/ir.f as the claimed optimizer/backend IR
```

Do not delete a file merely to satisfy this list. Retire each responsibility only after the new owner and production tests are live.

---

## 13. Proposed package and file layout

Names are architectural guidance. The implementing agent may adjust a path before code exists, but one concern per file and package-first ownership are mandatory.

### 13.1 Common compiler substrate

```text
src/compiler/ir/id.f
src/compiler/ir/context.f
src/compiler/ir/arena.f
src/compiler/ir/source.f
src/compiler/ir/type.f
src/compiler/ir/attr.f
src/compiler/ir/schema.f
src/compiler/ir/builder.f
src/compiler/ir/freeze.f
src/compiler/ir/verify.f
src/compiler/ir/serialize.f
src/compiler/ir/render.f
src/compiler/ir/diff.f
src/compiler/pass/result.f
src/compiler/pass/witness.f
src/compiler/target.f
src/compiler/numeric-policy.f
```

Packages:

```text
IR-ID
IR-CONTEXT
IR-SOURCE
IR-TYPE
IR-ATTR
IR-SCHEMA
IR-BUILD
IR-VERIFY
IR-CODEC
IR-PASS
COMPILER-TARGET
NUMERIC-POLICY
```

### 13.2 Native pipeline

```text
src/compiler/hir/op.f
src/compiler/hir/builder.f
src/compiler/hir/elaborate.f
src/compiler/hir/verify.f

src/compiler/sir/op.f
src/compiler/sir/from-hir.f
src/compiler/sir/verify.f
src/compiler/sir/canonicalize.f
src/compiler/sir/const.f
src/compiler/sir/cfg.f
src/compiler/sir/dce.f
src/compiler/sir/cse.f
src/compiler/sir/inline.f

src/compiler/lir/op.f
src/compiler/lir/from-sir.f
src/compiler/lir/representation.f
src/compiler/lir/verify.f

src/compiler/a64/op.f
src/compiler/a64/select.f
src/compiler/a64/verify.f
src/compiler/a64/liveness.f
src/compiler/a64/regalloc.f
src/compiler/a64/regalloc-verify.f
src/compiler/a64/layout.f
src/compiler/a64/layout-verify.f
src/compiler/a64/encode.f
src/compiler/a64/object.f

src/compiler/native/config.f
src/compiler/native/compile.f
src/compiler/native/transaction.f
src/compiler/native/shadow.f
src/compiler/native/metrics.f
```

### 13.3 GPU pipeline

```text
maki/ir/model-adapter.f
maki/ir/region.f
maki/ir/region-verify.f
maki/ir/fusion-witness.f

maki/ir/kernel-op.f
maki/ir/index-map.f
maki/ir/memref.f
maki/ir/kernel-builder.f
maki/ir/kernel-verify.f

maki/ir/gpu-op.f
maki/ir/schedule.f
maki/ir/schedule-verify.f
maki/ir/resource.f

maki/lower/model-region.f
maki/lower/region-kernel.f
maki/lower/kernel-gpu.f

lib/ptx/ir2/op.f
lib/ptx/ir2/builder.f
lib/ptx/ir2/verify.f
lib/ptx/ir2/from-gpu.f
lib/ptx/ir2/opt.f
lib/ptx/ir2/liveness.f
lib/ptx/ir2/name.f
lib/ptx/ir2/render.f
lib/ptx/ir2/parse-external.f
```

Use a new `PTXIR2` package during migration to avoid ambiguous calls into the old `PTXIR` code. Rename only after the old path is retired.

### 13.4 Tests

Every implementation file has a focused test. Add integration suites:

```text
test/compiler/ir-structure.f
test/compiler/ir-mutations.f
test/compiler/native-straight.f
test/compiler/native-control.f
test/compiler/native-wide.f
test/compiler/native-shadow.f
test/compiler/native-regalloc.f
test/compiler/native-object.f
test/compiler/native-selfhost.f

maki/ir/region-verify-test.f
maki/ir/kernel-verify-test.f
maki/ir/schedule-verify-test.f
lib/ptx/ir2/verify-test.f
lib/ptx/ir2/opt-test.f
lib/ptx/ir2/render-test.f
tools/ptx/ir2-device-test.f
```

---

## 14. Migration plan

The migration is deliberately vertical. Each wave must compile and run a real production-shaped program before broadening the language.

### Wave 0: freeze measurements and inventory

Deliverables:

- pin the audit commit;
- record native JIT latency, emitted code bytes, stack loads/stores, spills, calls, and dynamic runtime for a representative corpus;
- record PTX text size, `ptxas` registers, shared memory, spills, cubin size, compile time, and device time for current kernels;
- inventory every raw ARM instruction constant outside the encoder;
- inventory every machine-code scan;
- inventory every PTX string emitter and every path through `opt-ir`;
- add a disabled `new-compiler` feature/capability record;
- add shadow-comparison harness plumbing.

No code generation changes land in this wave.

### Wave 1: common IR substrate

Implement, in order:

1. nominal IDs and bounds tests;
2. source spans;
3. string/symbol interning;
4. type interning;
5. attributes;
6. function/block/operation/value tables;
7. append-only builder;
8. freeze lifecycle;
9. structural verifier;
10. canonical renderer;
11. canonical serialization and digest;
12. hostile mutation fixtures.

Acceptance:

- malformed IDs, windows, arities, blocks, terminators, value definitions, types, and attributes all fail with named diagnostics;
- encode/decode is byte identical;
- two independently built equivalent modules have the same digest;
- no new unchecked boundary is introduced without an owned retirement task.

### Wave 2: native straight-line slice

Support:

```text
integer literals
resolved direct calls to modeled primitives
DUP/DROP/SWAP/OVER as compile-time vector operations
integer add/sub/mul/bitwise
return
```

Pipeline:

```text
source tape → HIR → SIR → LIR → A64IR → allocation → bytes
```

Shadow compile a real definition through old and new paths. Execute both in isolated candidate processes and compare output, exit status, and stack result.

Acceptance:

- no raw ARM words above `A64ENC`;
- the new `SQUARE`, arithmetic chains, and stack-shuffle examples run correctly;
- generated stack shuffles produce no instructions when they only rename SSA values;
- new path has complete source maps and pass dumps;
- current path remains default.

### Wave 3: native control-flow slice

Add:

```text
IF/ELSE/THEN
BEGIN/UNTIL/AGAIN
BEGIN/WHILE/REPEAT
EXIT
RECURSE
```

Acceptance:

- block arguments encode every join;
- no branch path relies on a hidden virtual-stack snapshot;
- every function has one exit block;
- loop-carried values validate;
- differential behavior covers zero-trip, back edge, early exit, and nested control flow.

### Wave 4: locals, return stack, calls, and exceptions

Add:

```text
typed locals
>R R> R@
direct calls
indirect typed execute
throw/catch/evaluate edges
```

Acceptance:

- immutable local reference is an SSA alias;
- values live across calls obey call-clobber validation;
- only required stack homes are materialized;
- exceptions leave no half-published definition;
- process-boundary error tests move in-process where the new API makes errors catchable.

### Wave 5: quotations, loops, and defining semantics

Add:

```text
[: ;]
DO/?DO/LOOP/+LOOP/LEAVE/UNLOOP/I/J
DEFER/IS
CREATE/DOES>
string literals
POSTPONE and modeled compile-time words
```

Acceptance:

- quotations are typed nested functions/code references;
- loop frames are explicit;
- `DOES>` produces explicit function/symbol relationships and relocations;
- no code-stream patch chain is used as semantic state.

### Wave 6: wide values and type families

Add full representation lowering for:

```text
products
sums/enums
multi-cell layouts
construct/MATCH
wide locals
wide fetch/store
linear values
```

Acceptance:

- one source parse and one HIR build;
- no pass-2 source recompile;
- representation witness mutation tests;
- all existing wide-layout tests run through the new compiler.

### Wave 7: object and AOT cutover

Add:

```text
HBOBJ 2
symbols
relocations
source map
linking
AOT tree shaking
ELF/Mach-O consumption
```

Acceptance:

- AOT capture no longer scans `BL` encodings;
- code/data addresses are explicit relocations;
- object cache keys include source, checker, compiler, target, policy, and pass pipeline;
- current AOT and REPL build tests pass.

### Wave 8: native default and self-hosting

- compile the new compiler modules with the new compiler;
- build a candidate engine;
- reach a byte-identical new-compiler fixpoint;
- run the complete candidate suite;
- make the new compiler default;
- remove old direct compilation paths after one release-quality green checkpoint;
- lower trust and size ratchets with the same change.

### GPU Wave A: structured PTXIR

Before changing Maki lowering:

1. implement PTXIR2 structural types;
2. port the current PTX instruction subset;
3. render structured PTX;
4. port current exact optimizations;
5. compare rendered modules and device behavior for SAXPY and selected kernels;
6. remove generated-code dependence on line parsing.

Acceptance:

- every generated instruction is structured;
- unfamiliar generated instructions are impossible, not opaque;
- external parser round-trips separately;
- exact optimizer has pass-specific mutation tests.

### GPU Wave B: KIR elementwise slice

- make `LOAD`, elementwise operations, and `STORE` build KIR;
- lower KIR to simple flat GIR;
- lower GIR to PTXIR2;
- port Maki elementwise region lowering.

Acceptance:

- current elementwise device goldens pass;
- broadcast index maps validate;
- address CSE and load reuse operate on structure;
- no operation emitter writes text directly.

### GPU Wave C: row reductions and softmax

Add:

```text
row domains
mask identities
reduction identity
barrier/convergence model
two-pass and online schedules
```

Acceptance:

- inactive-lane behavior is explicit;
- reduction-domain coverage validates;
- barrier convergence validates;
- exact or bounded numeric policy is recorded;
- current softmax golden and gradcheck pass.

### GPU Wave D: GEMM and MMA

Add:

```text
logical contraction
shared staging
async-copy pipeline
warp/lane mapping
fragment layouts
MMA tensorization
epilogue fusion
```

Port one known-good configuration first. Then move current knobs into immutable schedule records and port the search space.

Acceptance:

- fragment map has a dedicated validator;
- shared-memory producer/consumer ordering validates;
- resource estimates precede emission;
- current device correctness and performance baselines are met or exceeded;
- no manual PTX register ranges remain in semantic lowering code.

### GPU Wave E: planner and tuner cutover

- RIR becomes the fusion output;
- schedule candidates produce GIR and witnesses;
- the autotuner accepts only validated candidates;
- artifact promotion binds all evidence;
- retire string-first `cg` operation emitters and old `opt-ir` from production.

---

## 15. Shadow-mode design

Shadow mode is a migration tool, not a user feature.

For each supported definition or kernel:

```text
old path produces artifact A
new path produces artifact B
```

Comparison levels:

### Native

- both compile or both reject;
- diagnostics identify the same source contract where possible;
- both execute under isolated candidate processes;
- stdout, stderr, exit status, and observable stack results agree;
- object metadata and relocations validate;
- code size and metrics are reported, not required to match.

### GPU

- both PTX modules assemble;
- device outputs agree under the declared numerical policy;
- sentinel and launch checks pass;
- gradients agree where applicable;
- performance/resource deltas are reported.

Shadow mode must have an explicit coverage count. Unsupported inputs fall back only while a named migration capability remains open. A silent fallback is forbidden.

After default cutover, remove shadow fallback. Keep differential fixtures as regression tests where useful.

---

## 16. Verification and performance gates

### 16.1 Structural gates

Every IR has a hostile fixture for:

- out-of-range ID;
- wrong ID kind;
- invalid pool window;
- missing terminator;
- operation after terminator;
- wrong operand/result arity;
- type mismatch;
- use before definition;
- dominance violation;
- bad successor arguments;
- illegal attribute;
- illegal target feature;
- builder use after freeze;
- module mutation after freeze;
- digest mismatch;
- witness input/output mismatch.

### 16.2 Native correctness gates

- checker acceptance remains required;
- source-tape/checker/HIR digest binding;
- stage verifier after every pass in debug/gate builds;
- old/new differential corpus during migration;
- full `test/run.f` candidate validation;
- AOT, REPL, snapshot, debugger, profiler, and image tests;
- cross-target macOS and Linux AArch64 fixtures;
- encoder/fixup golden vectors;
- register-allocation mutation tests;
- self-hosted fixpoint.

### 16.3 Native performance metrics

Pin individually timed benchmarks for:

```text
JIT compile latency
AOT compile latency
emitted code bytes
dynamic instructions
data-stack loads/stores
machine-stack spills/reloads
direct and indirect calls
branch count
runtime
compiler binary size
compiler peak temporary memory
```

Require attribution for every deliberate regression. Do not gate only on whole-suite time.

### 16.4 GPU correctness gates

- RIR/KIR/GIR/PTXIR validators;
- PTX renderer round-trip fixtures;
- `ptxas` acceptance;
- launch contract;
- device sentinel;
- CPU/device golden;
- ULP/error policy;
- finite-difference gradcheck;
- backward/forward consistency;
- target capability rejects;
- schedule-witness mutation tests.

### 16.5 GPU performance metrics

Record per candidate:

```text
PTX bytes and instruction count
PTXAS compile time
cubin size
registers/thread
shared memory/block
spill stores/loads
estimated and achieved occupancy
global load/store traffic
kernel time
effective bandwidth or FLOP/s
roofline classification
```

A new backend must at least match the old path on the committed correctness corpus before default cutover. Performance cutover requires representative kernels to improve or a documented target-specific reason why a slower kernel is retained.

### 16.6 Formal gates

As proofs land:

- no `Admitted`;
- complete assumptions report;
- expected external-axiom manifest;
- schema digest parity;
- witness vectors accepted by both executable and Rocq validators;
- corrupted witnesses rejected;
- composed native theorem for each covered language slice;
- composed GPU theorem for each covered operation/schedule slice.

---

## 17. Agent execution contract

The implementing agent must follow the repository's orchestrator rules.

### 17.1 Branching

- Work on a dedicated bookmark/branch.
- Never move `master` until the exact rebased tree is green.
- Use one semantic concern per commit.
- Treat each commit as a proof checkpoint, not a stash.

### 17.2 First checkpoint for every leaf

Before implementation, record:

1. package owner;
2. exact production entry point;
3. green baseline;
4. failing structural or behavioral proof through that production path;
5. exact interface to add or change;
6. forbidden shortcuts;
7. focused acceptance command;
8. broader gates owed.

A leaf that reveals an unplanned interface or dependency stops and is redesigned.

### 17.3 Code rules

- New implementation is checked Habu.
- Every module has a package owner.
- Every public word has an exact stack effect.
- Builders and validators are separate concerns.
- Do not add `TRUSTED:` merely to make an IR table compile.
- Concentrate any temporary representation boundary behind one generic arena/index owner.
- No raw instruction literal outside the architecture encoder.
- No text parser in a generated-code optimization path.
- No hidden fallback from new to old compiler.
- No pass mutates its input module.
- No optimization merges with its validator in the same opaque word.
- Update `FILEMAP.md` for every new owning file.
- Record durable lessons in `LESSONS.md`, not API descriptions.

### 17.4 Review rules

Review every diff hunk for:

- ownership;
- lifecycle;
- bounds;
- canonical ordering;
- deterministic output;
- named failure;
- test through real production entry;
- proof-surface impact;
- trust-surface impact;
- size and timing attribution.

A green test suite does not excuse an architecture violation.

---

## 18. First implementation backlog

These are the first bounded leaves. They deliberately stop before any source is compiled through the new backend.

### Epic IR-0: substrate

#### IR-0.1 — ID families

Add nominal IDs and private raw conversion in one package.

Acceptance:

- valid refinement/projection round trip;
- wrong-family use rejected by the checker;
- out-of-bound refinement throws a named code;
- no per-dialect trusted cast.

#### IR-0.2 — source registry and spans

Add source-file IDs and byte spans.

Acceptance:

- invalid range rejected;
- stable source digest;
- origin-parent cycle rejected.

#### IR-0.3 — symbol interning

Add deterministic symbol table.

Acceptance:

- duplicate bytes return one ID;
- insertion order does not alter canonical encoded symbol table;
- capacity grows geometrically.

#### IR-0.4 — canonical scalar types

Add integer, float, pointer, and token type records.

Acceptance:

- identical types intern;
- malformed width/address space rejected;
- canonical render and encoding.

#### IR-0.5 — operation/value pools

Add append-only operation, value, operand, and result pools.

Acceptance:

- builder-only read API;
- bounds and overflow fixtures;
- no freeze yet.

#### IR-0.6 — functions and blocks

Add parent and window records.

Acceptance:

- ownership checks;
- duplicate insertion and cross-function block misuse rejected.

#### IR-0.7 — builder lifecycle

Add create/reset/abort/freeze states.

Acceptance:

- use-after-freeze and double-freeze reject;
- abort releases private state;
- no frozen mutation API is exported.

#### IR-0.8 — structural freeze verifier

Validate windows, ownership, definitions, and terminators.

Acceptance:

- one hostile fixture per invariant;
- diagnostic includes module/function/block/op/span.

#### IR-0.9 — canonical renderer

Render a frozen module.

Acceptance:

- renderer has no mutation;
- golden text is deterministic;
- renderer output is never parsed by compiler code.

#### IR-0.10 — canonical codec and digest

Encode/decode and SHA-256.

Acceptance:

- byte-identical round trip;
- a one-field mutation changes digest;
- malformed count/length rejects before allocation overrun.

### Epic NATIVE-1: first vertical slice

Only after IR-0 is complete:

1. HIR literal/call/return schema.
2. Source-tape builder for a colon body.
3. HIR elaborator for literals and selected arithmetic primitives.
4. Straight-line stack-to-SSA.
5. SIR verifier.
6. SIR constant fold and DCE.
7. LIR integer operations.
8. A64IR integer operations and return.
9. Physical A64 encoder adapter.
10. Minimal linear scan without calls.
11. Register-allocation validator.
12. Isolated executable-object runner.
13. Old/new shadow runner.
14. Metrics row.

The vertical-slice done criterion is a real checked Habu definition compiled by the new path into AArch64 bytes and executed successfully.

Do not broaden the IR schema before that vertical slice runs.

---

## 19. Example: native lowering

Source:

```forth
: SQUARE-ADD ( x y -- z )
   swap dup * swap + ;
```

HIR:

```text
function SQUARE-ADD (i64, i64) -> i64
  stack.swap
  stack.dup
  call primitive:*
  stack.swap
  call primitive:+
  return
```

SIR:

```text
bb0(%x:i64, %y:i64):
  %sq = mul.i64 %x, %x
  %z  = add.i64 %sq, %y
  return %z
```

LIR remains similar, with explicit overflow semantics.

A64IR before allocation:

```text
v2 = MUL v0, v0
v3 = ADD v2, v1
STACK-RESULT v3
RET
```

The source stack operations have disappeared. The allocator chooses physical registers or spills based on liveness rather than on the order in which stack words happened to execute.

---

## 20. Example: GPU lowering

Model region:

```text
y = relu(a * x + y)
```

KIR:

```text
%e = logical-axis 0 .. N
%m = domain-mask %e < N
%xv = load %x map=%e mask=%m
%yv = load %y map=%e mask=%m
%p  = mul.rn.f32 %a, %xv
%s  = add.rn.f32 %p, %yv
%r  = max.f32 %s, +0
store %y map=%e value=%r mask=%m
```

GIR schedule candidate:

```text
split %e by block=256 vector=4
map outer to block.x
map inner to thread.x/vector-lane
predicate residual vector
```

PTXIR:

```text
%cta  = sreg.ctaid.x
%tid  = sreg.tid.x
%base = mad.lo.u32 %cta, 1024, %tid*4
%mask = vector-domain-mask %base, N, 4
...
```

Rendering assigns PTX register names only after optimization. Fusion and schedule validators operate before PTX spelling exists.

---

## 21. Completion criteria

The campaign is complete only when all of these are true.

### Native

- Concrete Habu source is captured once.
- Checked/elaborated HIR is the only source of native code generation.
- Stack SSA is the optimization representation.
- Wide values lower without source recompilation.
- Inlining operates on IR.
- Register allocation is liveness driven and independently validated.
- Calls, labels, addresses, and relocations are symbolic until encoding.
- AOT does not inspect machine bytes.
- `bin/hb` rebuilds itself to a byte-identical fixpoint using the new compiler.
- Full candidate, AOT, REPL, image, debug, and cross-target gates pass.
- Direct old compilation is removed.
- The final native proof chain reaches encoded AArch64 bytes and loaded image semantics for the covered language.

### GPU

- Kernel DSL words build KIR values.
- Fusion produces immutable RIR.
- Scheduling produces immutable GIR plus a witness.
- PTX is structured before rendering.
- Generated PTX is never reparsed for optimization.
- Register declarations are exact.
- Current elementwise, reduction, softmax, GEMM, and MMA correctness gates pass through the new path.
- Autotuning promotes only validated candidates.
- Old string-first operation emitters are removed from production.
- The final GPU proof chain reaches PTX semantics under an explicit numerical policy.

### Trust

- No new unowned trust surface was introduced.
- Retired direct-emission boundaries lower the trust ratchets in the same commits.
- Every remaining production assumption is either proved, validated by a proved checker, or named as an external axiom.

---

## 22. Final direction

The key design decision is not merely “add an IR.” It is:

> Preserve semantic information until the last responsible stage, and make every information-losing step explicit, typed, validated, and bindable to a proof.

For native Habu, stack SSA is the center of that design. It turns concatenative source into ordinary value flow, allowing stack shuffles and local references to disappear, control-flow joins to become explicit, and register allocation to become a real pass.

For GPU models, logical kernel IR and scheduled GPU IR are the center. They separate what the kernel computes from how a target executes it, allowing fusion, indexing, tiling, staging, pipelining, and tensorization to be optimized and verified independently.

The existing direct emitters proved that Habu can own its complete stack. The staged redesign is what lets Habu optimize that stack aggressively and prove that it has not changed the program.
