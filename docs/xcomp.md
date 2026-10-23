# Cross-compilation and targets

How Habu builds for a machine other than the one running it: the roles of the
build host, the compiler product and the output target, the target contract,
the rules every backend keeps, the design of each non-host target (x86-64,
Wasm, freestanding ARM32 and C66x), and the procedure for a new port. The
ARM64 host engine is [architecture.md](architecture.md).

## Build host, product and target

Three roles stay separate, and `src/compiler/target/model.f` (`RTARGET`) gives
each its own type:

- The **execution platform** is the process ABI of the running `hb`
  (`RTARGET:HOST`, from `DETECT-NATIVE-PROFILE`), not the physical machine: an
  x86-64 process under an emulator on ARM hardware is an x86-64 compiler.
- A **compiler product** records its executable target, its enabled emitter
  families and its default output profile (`RTARGET:PRODUCT`). Enabled
  emitters are product composition, not an application dependency: an
  application built by a product with five backends carries none of them.
- A **build action** pairs the execution platform with the target its bodies
  are compiled for (`FOR-COMPILER`, `FOR-OUTPUT`). `BUILD-TARGET:WITH`
  (`tools/build-target.f`) scopes one action around the native build entry
  and restores the pending selection on return or throw.

An emitter's implementation target and the instructions it writes are
distinct: code that builds an x86-64 instruction is ordinary Habu compiled for
the product's host. The running compiler supplies every body that executes
during a build; product-host definitions never replace them mid-build.

Execution is a separate action. A cross-build never tests an output by
launching it through a host process API: `RTARGET:EXECUTABLE-HERE?` admits
only a link-compatible target with the host's image format and the native
runtime, and a Linux x86-64 product built on ARM64 is run on an x86-64 machine.

## Target contract

`src/compiler/target.f` (`CTARGET`) is the immutable contract every compilation
carries: architecture, ABI, byte order, pointer width and features, with stable
wire codes and a digest. It refuses only incoherent combinations, so a contract
can be declared before any backend for it exists; an architecture with no
loaded backend is refused at the first compiler stage with `E-CTGT-UNLOADED`.

`RTARGET` resolves a named profile (`aarch64-apple-darwin`,
`aarch64-unknown-linux-gnu`, `x86-64-unknown-linux-gnu`,
`wasm32-unknown-unknown`, `wasm64-unknown-unknown`, and the two Windows
profiles) into a `resolved-target`: the profile plus its feature set. A profile
fixes the architecture, pointer width, foreign ABI, runtime, image format and
Habu ABI (`cell64-v1`, the only one). `RTARGET:CORE` answers `unsupported` for
the Windows profiles: they are declarable products whose process and image
services do not exist. Unknown spellings refuse with `RTARGET:E-UNKNOWN`.

Three questions have three answers and must not be merged:

- `LINK-COMPATIBLE?`: same architecture, byte order, pointer width, Habu ABI,
  foreign ABI and runtime;
- `RUNTIME-ADMISSIBLE?`: link-compatible, and the selected features include
  every required one;
- `SAME-BUILD-IDENTITY?`: same resolved target and same action-input digest.

The Habu ABI (internal calls, value conventions, execution-token
representation), the foreign ABI (C marshalling) and the image format are
separate fields, because one overloaded `abi` field would decide all three. A
structurally coherent target can still be unsupported by the installed
backend; the two errors stay distinct. `test/compiler/target-resolve.f` and
`test/compiler/target-registry.f` cover resolution and the registry.

## Compile-time execution and target data

A cross-build runs the target's whole source prefix on the host: definers,
`create`, `here` and stores execute against host memory. Target data reaches
the output only through tracked owners, never by copying a host image:

- `NSHADOW` (`src/compiler/native/shadow.f`) compiles each definition a second
  time for the target from the same frozen HIR module and keeps it as a sealed
  unplaced emission whose every call and address is a row (x86-64 "Dual
  emission").
- The capture (`src/habu/aot-shadow.f`, `src/arch/wasm/capture.f`) resolves
  each host address through its record and refuses an unresolved one by name.
- The target linker places records and DATA at target addresses
  (`src/habu/link-x64.f`, `src/arch/wasm/link.f`).

A host pointer is never converted into a target address by value: arithmetic
that lost provenance cannot be recovered by scanning integers for
pointer-looking bits. A host-callable body may execute during compilation; a
target-only body may not, and a declaration's code addresses are never its
identity.

## Layout

The Habu cell is 64 bits on every target (`src/core/cell.f`); target address
width is independent of it.

- A Habu pointer on a 32-bit address space is a canonical zero-extended offset
  in an eight-byte slot, narrowed only after its bounds check.
- A foreign pointer field takes the foreign ABI's width; a Habu multi-cell
  value keeps its schema's lane order and never takes C padding.
- An execution token is a nominal callable reference, not a C function pointer
  or a browser handle.
- Host compiler data, target data and wire data are three layouts that may
  coexist; offsets come from the owning schema.

Bounds use unsigned wide arithmetic: `p <= L and n <= L - p`, never an
unchecked `base + size`. A pointer is representable only when
`p < 2^address_bits`; an extent may equal that power, so an extent at 2^32 is
not a valid memory32 pointer even for a zero-byte access. Raw storage never
holds an address ([forth.md](forth.md) "Raw storage never holds an address").

## Numeric admission

A backend implements the language's semantics, not its machine's convenient
answer. The contract is `src/habu/arith-abi.f` and
`src/compiler/numeric-policy.f` (`CNUM`):

- `+ - *` wrap modulo 2^64, including on register-pair targets;
- a zero divisor throws `E-DIV-ZERO` (-6400) and `MIN-N -1 /` wraps to
  `MIN-N`, both guarded before a machine divide that would trap
  (x86-64 `idiv`, Wasm `i64.div_s`);
- `/`, `mod` and `/mod` truncate toward zero;
- `lshift`/`rshift` take the count modulo 64 and `rshift` is logical;
- a comparison observed as a cell is the all-ones flag, never 0/1;
- `f>s` truncates, saturates and answers 0 for a NaN;
- an operation that makes a NaN answers `$7FF8000000000000` and a quiet NaN
  operand passes through, the left of two (`src/compiler/native/hir-word.f`
  `DEF-FLOAT`); no profile selects the rule away (x86-64 "The NaN an operation
  makes").

Constant folding uses target semantics, never the build host's rounding or
integer width. Catchable throws keep the full 64-bit code; no Habu unwind
escapes through C frames, and a C callback catches a throw and answers its
declared `FALLBACK` (`src/lib/ffi-callback.f`). A fatal invariant failure ends
or poisons the runtime; it is never turned into a catchable error.

## Linking and publication

- Native in-process publication only enters a runtime with the same execution
  machine: `NEMIT:OPEN` takes the machine an emission is for, and
  `src/compiler/native/publish.f` refuses any other with `E-NPUB-TARGET`.
  Cross-generated code goes to an artifact.
- Publication is ordered: a pending routine is proven, committed to the code
  region, filed with `NSHADOW` and only then appended to the dictionary
  (`NPUB:PUBLISH-PENDING`), so a failure leaves no callable half-word.
- File publication is staged: every `-o` and engine write lands whole or not at
  all (`RESERVE-SIBLING`, `src/lib/fs-mutate.f`).
- A relocation patches the exact field its encoding defines and checks the
  whole field's range: an x86-64 `rel32` counts from the end of the
  instruction; a Wasm function, type or table reference is an index, written as
  a fixed-width padded LEB so linking never changes a length (Wasm "Linking").
- A linker refuses before writing its first byte; nothing leaves the process
  ahead of a refusal.

## x86-64

The x86-64 backend shares the target-free compiler with ARM64 and forks
nothing else by accident. Linux x86-64 is the target; the ARM64 engine
cross-builds it.

### What a "backend" is in this tree

A second architecture touches five layers:

| Layer | arm64 | x86-64 |
| --- | --- | --- |
| Assembler | `src/arch/arm64/asm.f` | `src/arch/x86-64/asm.f`, variable-length encoding |
| Engine primitives | `habu1.f` bodies | `src/habu/kernel-x64.f` bodies under the same `src/habu/prims.f` rows |
| Compiler | `select.f`, `regalloc.f`, `spill.f`, `emit.f`, `a64ir.f` | `x64ir.f`, `select-x64.f`, `emit-x64.f`; allocator and spill shared, parameterised by a register file |
| OS seam | `src/os/linux/` | `src/os/linux-x86-64/`, same file set and word names |
| Runtime ABI | AAPCS64 FFI, task entry, guard pages, signal frames | SysV AMD64 FFI, task entry, the same guard-page model |

`hir.f`, `hir-word.f`, `elaborate.f` and `loop.f` are target-free.

A backend is one sorted row in `src/compiler/native/backend.f` (`NBACK`),
reached by architecture (`NBACK:ROW`) or stable provider ID
(`NBACK:ID-ROW`). The row holds the immutable `CTARGET` descriptor and the
complete typed pass record, published under an `NLEASE:WITH` lease after both
are stored. `src/compiler/native/compiler.f` names no backend package: it runs
each stage through the row its compilation's contract resolves to. A backend
registers itself when its `passes.f` loads, so `bin/hb` for one host does not
carry the other backend, and the driver needs no edit to reach a new one.
`src/arch/x86-64/backend.f` (`X64BACK`) requires only `src/lib/prelude.f` and
`src/compiler/target.f`, so no engine prefix grows a compiler dependency.

### Machine model

- **VM registers.** x86-64 has 16 general registers. Seven are reserved:
  `rbp` user area, `r12` data stack, `r13` data base (the code region holding
  the dictionary records), `r14` record count, `r15` code pointer, `rbx`
  interpreter, and `rsp`, because `call` pushes the return address through it.
  The nine allocatable (`rax rcx rdx rsi rdi r8 r9 r10 r11`) are all
  caller-saved internally. SysV's callee-saved set is exactly the reserved set
  less `rsp`, so a foreign call preserves the VM with no save block.
- The engine's six are declared once in `src/habu/layout.f`
  (`ENGINE-GPR:X64-*`, with `X64-MASK` derived). `ENGINE-GPR:DSTACK` and `MASK`
  select the running target's pair, so x86-64 compiler code running on the
  ARM64 engine names the `X64-` pair. `X64IR:RESERVED-MASK` adds `rsp`.
- **Calls between compiled words** use the ARM64 internal convention: stack
  cells in memory, live values in registers within a routine.
- **Literals.** `mov r64, imm64` is the one relocatable literal, one site to
  patch (`src/habu/address-carrier.f` `MOVABS`).
- **Traps** use the same mmap protections; the handler reads `RIP` and `RSP`.

### The assembler

`src/arch/x86-64/asm.f` (`X64ASM`) holds the `ENC-*` encoders; its header
lists the operand types and forms. The rules a caller relies on:

- Every encoder appends to a `src/lib/byte-buffer.f` `BUF` given as its last
  operand; there is no instruction value, because x86-64 instructions have no
  fixed width.
- Sizes are deterministic per word and displacement; to size an instruction,
  encode it into a scratch buffer.
- A `rel` operand counts from the end of the instruction; `call rel32` and
  `jmp rel32` carry it at `CALL-REL32-OFF` (1), and `ENC-MOV-RI64` carries its
  imm64 at `MOV-RI64-IMM-OFF` (2).
- An `r8` of 4..7 is `spl bpl sil dil`: REX is forced and the legacy high-byte
  registers do not exist.
- `E-X64ASM-OPERAND` is raised before any byte is written.
- Forms are added when a caller needs them.

### The emitter's sink

The x86-64 emitter is its own file, `src/compiler/native/emit-x64.f`
(`X64EMIT`), writing through the same `BUF` sink the encoders take;
`src/compiler/native/emit.f` stays the ARM64 four-byte word sink. Its layout,
branch reach and source map assume fixed-width instructions and its word
builders are ARM64-only, so parameterising it would rewrite most of it, share
almost nothing and put the ARM64 product's byte identity at risk.

The layout therefore works in bytes: an operation's size is measured by
encoding it, block starts are byte offsets, and every branch is laid out as
`rel32`, so one pass suffices. Shortening to `rel8` would be a separate
relaxation, a size question and not a correctness one.

### The machine dialect

`src/compiler/native/x64ir.f` (`X64IR`, dialect `x64`) is the closed set of
operations that stand for real x86-64 forms. Three machine facts shape it, and
a lowering that forgets one is wrong in a way no later pass catches:

- **Most forms are two-address.** Each arithmetic form declares a schema tie
  naming the operand it destroys. The selector makes the tie satisfiable by
  copying operand 0 through `x64.mov` whenever that value is live after the
  form, and the allocator coalesces what it can. Liveness, not use count: a
  value defined above a loop and read once inside it is live round the
  backedge. A value live in any block laid after this one counts too, because
  the allocator's range is a hull over blocks in ordinal order
  (`select-x64.f` `LATER-LIVE?`).
- **The flags are not a value.** Every use of `RFLAGS` is fused into one
  operation (`cmpset`, `cmpsel`, `cmpbr`, `brz`), because no SSA value may
  stand for a single architectural resource.
- **No link register, no write-back addressing.** `call` pushes the return
  address, so there is no link save; a data-stack store and its pointer move
  are two operations where ARM64 fuses them.

Fixed registers: `shl`/`shr` read their count from `rcx`, and `idiv` divides
`rdx:rax`. The selector copies the count or dividend with `x64.mov` just before
the form; the allocator pins that copy and forbids the register to every other
value the form reads or lives across, and to every value whose range spans the
pin's opening (`regalloc.f` `MB-FORBID-PINS`). `idiv` carries the runtime
`throw` entry as an attribute and renders behind a divisor screen: zero throws
`E-DIV-ZERO`, minus one negates, anything else is `cqo; idiv`.

`cmpsel` and `selz` tie their result to the answer for a failed condition, so
the move of the other answer is the only write and any aliasing is safe.

Doubles are SSE2 scalar forms over the sixteen XMM registers. Every contract
destroys the XMM file at a call, so a double live across one goes to the frame.
`fcmpset` compares with `ucomisd`; `equal` joins `sete` and `setnp` because
`ucomisd` sets ZF on an unordered pair and every HIR float comparison answers
false there. `cvttsd2si` answers MIN-CELL for a NaN or an out-of-range double,
so `realint` corrects it without a branch (Selection).

Error ranges: -8740..-8759 `X64IR`, -8760..-8779 `X64SEL`, -8780..-8799
`X64EMIT`. `test/x86-64-peer-routines.f` runs every form's routine image,
including each aliasing identity of `cmpsel`/`selz` and doubles live across a
call that writes every XMM register.

### The machine description a contract is written against

The register allocator and the routine-effect contract it reads are
target-neutral: `src/compiler/native-effect.f` (`NEFF`) states the vocabulary,
and every contract carries the machine it is about, an `NMACH:mach`
(`src/compiler/native/machine.f`) the backend supplies.
`src/arch/x86-64/machine.f` (`X64M`) reads every fact off the dialect. Its one
different reading is the frame offset field, counted in bytes rather than
scaled by access width, which is why `NMACH:SLOT-REACH` takes a width.

A contract here says `NEFF-LINK:ABSENT`; ARM64's `preserved`/`clobbered` are
refused on it, and ARM64 refuses `absent`. A register the schema can encode is
still refused unless this machine lets a routine hold state in it, so `rsp`
and anything past `r15` are refused at `NEFF:ROUTINE`.

Nothing allocates without a description: the allocator binds the machine and
the dialect vocabulary together (`A64RA:BIND-DIALECT`), and the register file
(`src/compiler/native/regfile.f`, `NREGFILE:FILE`) is derived from the machine,
storing the reserved and clobbered sets and deriving the allocatable and
callee-saved ones. `test/compiler/x64-regalloc.f` allocates real x86-64 leaves.

### Selection

`src/compiler/native/select-x64.f` (`X64SEL`) lowers HIR to X64IR in a separate
pass with A64SEL's shape.

- A literal right operand that fits imm32 selects the immediate form; its
  `movi` is dropped only when every use folds.
- `realint` (`f>s`) is branchless: `cvttsd2si`, xored with the `gt` flag
  against `$43DFFFFFFFFFFFFF` (the largest double below 2^63) to turn the
  top's MIN-CELL into MAX-CELL, then anded with the self-equality flag to
  answer 0 for a NaN.
- The data-stack boundary is `dtake`, one `dload` per argument, one `dstore`
  per result, then `dpublish`; in a routine that leaves through its callee the
  pointer never moves, so a tail call reads its arguments at negative
  displacements.
- A comparison feeding a branch selects `cmpset` then `brz`; the fused
  `cmpbr` and `cmpsel` forms are declared but not selected.
- HIR has no `negate`, so `neg` has no source form.
- The word convention (`src/arch/x86-64/abi.f`, `X64ABI`): link `absent`, no
  prologue slot because the return address is on the machine stack, the nine
  allocatable registers as the pool, frames holding spills alone.
- `src/compiler/ir/fun.f` `TARGET-CK` ties a calling convention to a kind of
  architecture, not to AArch64 by name.

`test/compiler/x64-select.f` is the suite. Not covered: edge splitting of a
permuting back edge, and the internal shape refusals.

### The pass chain

`src/arch/x86-64/passes.f` (`X64PASS`) fills this backend's `NBACK` row as it
loads:

- **select** reads the frozen HIR module `NBACK:FREEZE` hands every selector,
  the target-free loop fold already applied;
- **fixpoint** allocates and, while the plan is not empty, lowers it through
  the shared `src/compiler/native/spill.f`, bound to this dialect's forms;
  there is no x86-64 spill file;
- **emit** places the routine at the slot the driver names and resolves every
  displacement against it. A slot must be a multiple of `X64IR:SP-ALIGN`. The
  emission returns a sealed byte image with its site lists, every offset in
  bytes, where the ARM64 emitter answers instruction indices. The span is
  exact whatever the routine ends in;
- **unplaced** emits for no slot: every external call field is zero, a
  `codeaddr` holds its function's offset in the emission, and the rows are what
  a linker writes the fields from. A shadow target ends here;
- **retire** is nonthrowing and runs on both the accepting and the refusing
  path, so a second call is harmless;
- the native shadow's MOVABS rewrite is installed through
  `NSHADOW:NATIVE-ADDRS!`; `NSHADOW:OPEN-NATIVE` refuses until it is, so the
  ARM64 product carries none of it.

Emission is stated as `NEMIT` rows (`src/compiler/native/emission.f`) and
copied into the owned artifact `src/compiler/native/publish.f` reads.
Publication decodes nothing: it records the span and hands each site's first
byte to `callmap-set` or `addrmap-set` ("Live-region sites"). A refusal inside
the emitter, such as `E-X64EMIT-REACH`, comes before any row is stated. Still
refused by name with `E-X64EMIT-FORM`: the negate, which no selection reaches.
`test/compiler/x64-chain.f` drives the chain through `NBACK` as the driver does.

#### The row installers

`X64PASS:INSTALL` gives `NBACK:REGISTER` the stable provider descriptor and
one complete typed pass row; the provider becomes visible only after all ten
callbacks exist.

| pass field | obligation |
| --- | --- |
| `declare` | record in-cells, out-cells, linkage; reset per-definition state |
| `select` | frozen HIR module in, machine module out; the driver retires the HIR module |
| `fixpoint` | lower until the allocation seals empty |
| `emit` | write at the code slot the driver passes (`NPUB:NEXT-SLOT`) |
| `unplaced` | write for no slot and state the rows unplaced |
| `release` | give back every pass binding still held (failure path) |
| `retire` | end-of-definition emitter reset (success and failure) |
| `prototype` | intern the dialect vocabulary into the session prototype |
| `forget` | clear that prototype flag at session stand-down |
| `prepare` | scrub code buffers and pass scratch before an image capture |

The quotation effects are in `backend.f`. `NBACK:linkage` (`L-NONE L-DEAD
L-CALLED L-TAIL L-BACK`) carries how control reaches and leaves a routine.
`test/compiler/target-registry.f` drives a fake backend through these rows.

### Dual emission

A cross-build runs the target's whole prefix on the host, so each definition
must be the host's routine and the target's at once; its names are bound as it
compiles, so the target's routine cannot be lowered later from the tape.
`src/compiler/native/shadow.f` (`NSHADOW`) is that second target:

- `NSHADOW:OPEN` opens a shadow on a binding (`X64ABI:BINDING`); `CLOSE` is
  nonthrowing and `NCOMP:CAPTURE-PREPARE` runs it, so no capture carries an
  open shadow.
- While one is open, `NCOMP` runs declare, select, fixpoint and
  `NBACK:EMIT-UNPLACED` for the shadow's binding in a context nested inside the
  definition's, from the same frozen HIR module, which it only reads.
  `NSHADOW:TAKE` copies the sealed rows; a shadow refusal refuses the
  definition.
- `NPUB:PUBLISH-PENDING` files the copy under the record after its commit
  (nonthrowing, because `TAKE` already refused what could be refused);
  `NSHADOW:ABANDON` drops a copy no publication claimed.

With no shadow open the driver runs exactly its old stages, so an engine
compiles every definition to the same bytes. `test/compiler/shadow.f`
compiles with an x86-64 shadow open on the ARM64 host.

The capture carries the map: `src/habu/aot-shadow.f` walks it into the four
`AOT-SHADOW` tables of `src/habu/aot-decl.f` (records, code, sites, code
cells), and `src/habu/aot-file.f` states the format version. Code bytes carry
no host address: call fields stay zero, a code literal holds 0, a DATA literal
holds its window coordinate.

Two reaches decide what ships, because x86-64 and ARM64 optimise differently
and ARM64 reach cannot answer which x86 routines are needed:

- the capture's reach (`ACAP-SHADOW-REACH`) decides which stripped or retired
  records are live; a live emitted private record is retained by name with its
  private wordlist before stripping (`SH-KEEP-PRIVATE`);
- the shadow's reach (`SH-REACH`) roots named rows and declared code cells and
  follows the x86 shadow's own call and code-address sites. A reached routine
  with no shipped row ships anonymously; an `EXPORT` alias attaches by exact
  host xt identity to its source's emission.

Every record index is a shipped row, never a window index, and a capture table
is read only through `ACAP-DICT>CAP`, because stripping and `undefine`
renumber records. A function entry without a record, such as a quotation, is
found by its ordinal in the frozen HIR emission; an interior address has no
function identity and is refused. Every refusal is by name, exit 74. MERGE
refuses a shadow on either side, because the merged engine is the ARM64 seed.

The reader is its own file because `aot-capture.f` also loads in the stdin
metabuild and the recovery bootstrap, which have no `NSHADOW`, and because the
map must be read through the host's `NSHADOW`: after a native build's logical
reset a name looked up at capture time finds the target window's unopened
instance. `test/aot-shadow-capture.f` round-trips every kind and requires a
named refusal for each forged row.

### The dictionary layout

`src/habu/link-x64.f` (`X64LINK`) lays a capture out where an x86-64 image
holds it. It is the write-time twin of the ARM64 seed's boot passes
(`habu2.f` `EM-SEED-DICT`, `EM-AOT-REGISTER-RECS`): the region and DATA are at
fixed addresses, so what that seed computes at every boot is computed once.
`LAYOUT` runs after the kernel's rows are in the `X64CODE` stream and before
`elf.f` `ASM-CODE` links it.

- `DICT$`: the kernel's `ENGINE-PRIMS` rows, then the shipped records in
  capture order.
- `CODE$`: the long names, then each shadow emission from a `CODE-SLOT`, int3
  to the next.
- Wordlist IDs are rebased to `FIRST-DYNAMIC-WID`; the protection bitmap and
  the hash index are built by the writer, because they are functions of the
  records alone.
- The window's DATA moves up to `AOT-DATA-D0`'s 8-residue, as the seed's DP
  moves.

Two passes precede the first laid byte: one places each emission, one resolves
every site and code cell. `CALL`/`TAIL` sites get a rel32 from the end of the
instruction, `CODE` and `FUN` an absolute entry, `DATA` the target DATA address.
A name of the engine's own prefix resolves to the kernel body the index finds
in wordlist 0. `DATA-BAND` also publishes the placed site rows and declared
pointer cells, so a linked engine captures a later image without the ARM host's
coordinates.

Every refusal is by name, exit 74, before the first byte leaves the process.
The fixed segments keep every band byte within `$3400000` of the text, so no
capture this tree writes reaches the rel32 refusal. `test/x86-64-link-records.f`
checks the layout is independent of the capturing host (same digest from a
host with extra records ahead of the window), probes the booted index in
`hb-x64-link-index`, and runs a stripped image through a private helper.

Load order: the x86-64 files bind `using X64CODE`, whose tails
`src/arch/arm64/icode.f` defines as globals, so `link-x64.f` requires the
x86-64 side first, then `src/arch/arm64/icode.f` and `aot-decl.f`.

### The OS seam

- **Target.** `linux-x86-64`, predicate `HB-TARGET-LINUX-X86-64?`. Every
  `src/os/<target>/target.f` defines all three predicates and
  `HB-TARGET-KNOWN?` is closed over them; every selector has an explicit
  x86-64 arm or a named refusal (Porting "Source Selection").
- **Layout.** Both Linux targets load `src/os/linux/layout.f` and share
  `src/os/linux/layout-constants.f` (`DATA-VA $340000000`, a lazy
  `DATA-SIZE` reservation), so captured DATA pointers need no translation
  between the two Linux hosts.
- **ELF.** `elf.f` writes ELF64 `EM_X86_64` at `VMBASE $400000` with six
  program headers ("Fixed segments"); `dlopen`/`dlsym` bind through
  `R_X86_64_GLOB_DAT`.
- **Syscalls.** `sys.f` uses the x86-64 numbers of the *at* family, as aarch64
  does. `SYS,` sets CF on error (Porting "Syscalls And Signals");
  `X64RT:SYS-PUSH` pushes the result or -1. Seam register arguments are x86-64
  register numbers and move through an upward-growing data stack: push is
  `mov [r12], r` then `add r12, 8`.
- **Stencils.** `SYS-EMIT-*` are byte strings of whole instructions that
  `X64RT:EMIT-STENCIL` appends as they stand, the twin of `jit.f`
  `C-EMIT-STENCIL`.
- **The stream.** `X64CODE:ASM-SINK` is the byte buffer the code stream appends
  into, linked at the text address by `ASM-CODE`. `tools/native-emit.f` sends
  an x86-64 target to `tools/native-emit-x64.f`, which loads the x86 stream
  before the window installs its compiler, so the ARM64 assembler's global
  `CODE` and `LBL` cannot bind it.
- **Contract.** arch `x86-64`, ABI `sysv-amd64`, little-endian, 64-bit
  pointers.
- **Relocation site.** `MOVABS` (`src/habu/address-carrier.f`) is 10 bytes,
  REX.W `B8+r` imm64 patched at offset 2; `test/x86-64-seam.f` pins
  `MOVABS-IMM-OFF` equal to `X64ASM:MOV-RI64-IMM-OFF`.
- **Tests.** `test/x86-64-seam.f` (headers by field, `MOVABS`, contract),
  `test/x86-64-emit.f` (byte strings against `llvm-mc`), and
  `test/x86-64-peer-image.f` (`hb-x64-peer` exits 0 and
  `hb-x64-peer-negative` 21 on an x86-64 host).

### Fixed segments

An x86-64 image places its code region by `PT_LOAD`, where the ARM64 boot maps
it near a hint. DATA is no segment on either target: both boots map it at
`DATA-VA` (on Linux a lazy `MAP_FIXED_NOREPLACE` reservation, `sys.f`
`MAP-DATA-RESERVE`) and copy in the bytes the image carries, which an x86-64
image holds in a read-only carrier `LOAD` at the region's end.

`src/os/linux-x86-64/elf.f` writes the six program headers: the text, the
read-write tail, the interpreter and the dynamic table it shares with the
aarch64 writer `src/os/linux/elf.f`, then the code region (header 4) and the
DATA carrier (header 5). `test/x86-64-seam.f` pins each one.

- **The carrier.** No `LOAD` reserves DATA, so the kernel neither commit-charges
  it nor starts the brk inside it: the brk starts past the carrier, below
  `DATA-VA`. The carrier must fit `DATA-SIZE` and end below `DATA-VA`
  (`ELF-CARRIER-CAP`). A stripped image passes an empty DATA span and decodes
  its sparse runs at startup.
- **Boot copies bytes; nothing rebases.** `LINKED-START,` protects the loaded
  code and copies the carrier into DATA (`DATA-LOAD,`); `SNAP-START,` does the
  same on its cold path and restores a validated heap trailer. Snapshot restore
  checks that header 5 is the carrier with exactly the `DATA-START` prefix
  before it maps DATA (exit 79 otherwise, 80 for an unsupported version).
  REGION and DATA addresses are fixed, so neither code nor DATA pointers are
  rebased on restart.
- **The target's layout is not the host's.** A macOS host places its own DATA
  elsewhere. `src/os/linux-x86-64/target-layout.f` replays
  `layout-constants.f` privately in `X64LAYOUT` and publishes the target's
  `DATA-VA`, `DATA-SIZE`, `CODE-OFF`, `IMAGE-TEXT-SIZE-OFF` and
  `LINUX-DLSYM-SLOT-OFF`; x86 code reads them qualified. Files that read the
  layout open `using X64LAYOUT` after their last load, and under that guard a
  bare read of a shadowed global refuses the load by name
  (`E-USING-SHADOW-GLOBAL`). Packages do not nest, so `target-layout.f` and
  `elf.f` load at top level.
- **Order.** `ELF-FIXED-CHECK` refuses at load (rc 73) a layout where the text
  window reaches the region or the region reaches DATA.
- **Metadata** starts at `$190`, past six program headers, where the ARM64
  writer's four leave it at `$120`. `M-PAD-OFF` only pads forward, so an offset
  left inside the headers would misplace its table silently;
  `test/x86-64-seam.f` pins each offset.

### Live-region sites

The live code region records its sites (calls and address literals a later
pass rewrites) where code is built, and never finds them by decoding bytes. On
ARM64 the record is two bitmaps, one bit per four-byte word. An x86-64 site
starts at any byte, so on this target the same bytes,
`[CALLMAP-OFF, ADDRMAP-END)`, hold a band of five-byte rows (u32 offset, u8
kind) declared in `src/habu/layout.f` (`SNAP-RELOC:SITE-*`); a full or corrupt
band is `SITE-RC`.

- A site's offset is its instruction's first byte on both targets.
- Rows are strictly ascending, one per offset; `callmap-set` and
  `addrmap-set` insert in place, so callers record sites in any order.
  `code-publish` removes the rows over the bytes it writes.
- The kind byte says call or address, nothing more; the address class lives in
  the emission's site list.
- Both views are declared on every target, because `src/` has no load-time
  target selection; the running engine picks one by
  `HB-TARGET-LINUX-X86-64?`.
- One reader, `src/habu/sites.f` `SITES:EACH-IN-SPAN`, serves both and checks
  every row before the first yield.
- Snapshot relocation does not apply: the region and DATA are fixed, so the
  rows serve publication and the AOT capture only.

### The primitive table

`src/habu/prims.f` (`PRIM-SPEC`) is the one machine-independent list of engine
primitives, loaded before `src/core/checker.f` as pure data; the checker
replays its effects, so each effect is stated only there. Each backend supplies
bodies under exactly these names; the ARM64 bodies in `habu1.f` and the x86-64
bodies in `src/habu/kernel-x64.f` are implementations of the table. Its header
states the row forms and atoms.

- Two gates close the fork: `ENGINE-PRIMS:SPEC-CHECK`
  (`src/habu/primitive-registry.f`) refuses a body whose name has no row, and
  `ENGINE-PRIMS:COMPLETE` refuses a row no section registered;
  `X64KERNEL:KERNEL,` calls it after its last section.
- `MIN-IN` is folded into the seed record, so the interpret band refuses a bare
  primitive on a shallower stack before its body runs.
- **The parity gate** is `test/prim-parity.f`, run unchanged on both engines.
  Its cases are data (`test/prim-cases.f`, `test/prim-float-cases.f`) that a
  kernel test includes without copying; a row with a `REF PRIM-REF:<X>`
  (`src/habu/prim-ref.f`, checked Habu that never uses the primitive it stands
  for) runs beside the body, and a row with neither a case nor a reference is
  printed by name on every run.

### Build and bootstrap

Cross-build first, self-host second.

1. **The engine is tier-1 only.** A small hand-written kernel is bound to the
   `prims.f` names; the outer interpreter, definers, packages, source loading
   and `MAIN` are checked Habu captured into both products. There is no x86
   copy of `habu2.f` or `jit.f`.
2. **The ARM64 engine cross-builds by dual emission.** Loading the prefix
   executes on the host, so the host cannot switch its contract; each
   definition's one HIR is emitted twice and the capture carries the shadow.
3. **A host-side Habu linker writes the image.** It resolves every record,
   call, `MOVABS` literal and DATA cell at write time; no relocation runs at
   boot or snapshot restore. The source window ends at `SEAL-CAPTURE`; the
   linker publishes the linked record count as `SEAL-NDICT-CELL` and
   translates MAIN's reporter from `ENGINE-MAIN:REPORT-CELL` into the protected
   `UNCGH-CELL`.
4. **The cross-built engine self-hosts** to a byte fixpoint on an x86-64 host.
5. **Recovery.** The Gforth chain (`bootstrap/cg/forth.fs`) emits ARM64 only
   and refuses x86-64 by name; x86-64 recovery is a cross-build from a working
   ARM64 engine, and `tools/native-build.f` is its only route.

### Gates and acceptance

- The primitive parity gate, green on both backends.
- `test/run.f` green on an x86-64 host on a cross-built engine, then on its
  own fixpoint engine.
- The FFI suite under SysV, the task suite, the guard-page suite, and an
  AOT-stripped application running.
- A native x86-64 build records the emission it published and reaches the same
  byte fixpoint, engine and names sidecar, across three generations before a
  product is promoted.

The x86-64 kernel refuses the six `source-exec-*` words by name
(`src/habu/kernel-x64.f`, the `REFUSE-WID` rows `source-exec-run` ..
`source-exec-handler`), so no source range is observed there.

### Build target

`tools/native-build.f -- <out> [whitebox] [--target <target>]` builds for
`linux-aarch64`, `macos-aarch64` or `linux-x86-64`, by default for the engine
running it. The target is one cell, `tools/build-target.f`; the window gets
that target's `target.f` and `layout.f` (`NB-TARGET-CORE-FILES`), and inside
it the window's own predicates answer, so `NATIVE-RUNTIME:PROVIDE-TARGET`,
`LOAD-REPL-TERM` and `compiler.f` `LOAD-PASSES` follow the target with no read
of their own. The building engine's predicates are not touched.

The entry refuses a target whose architecture has no backend registered in the
building engine (`NBACK:REGISTERED?`) and loads none itself, so a cross-build
loads the backend ahead of the driver:

```sh
bin/hb --load src/arch/x86-64/passes.f tools/native-build.f -- <out> --target linux-x86-64
```

`src/arch/arm64/machine.f` loads in that window because the shared compiler
requires `a64ir.f` on every target; it names the `A64-` pair and reserves x18
on x86-64, where no AArch64 platform releases it.

### The NaN an operation makes

One rule holds on every target (`src/compiler/native/hir-word.f`
`DEF-FLOAT`): an operation that makes a NaN from numbers answers
`$7FF8000000000000`, the positive quiet NaN, and a quiet NaN operand passes
through unchanged, the left of two. `f.` prints `-` exactly when bit 63 is set,
so `-1 fsqrt f.` and `0 0 f/ f.` print `0.000000` everywhere.

ARM64 answers the rule as it stands. SSE answers `$FFF8000000000000`, so
`X64SEL` clears that sign after `addsd subsd mulsd divsd sqrtsd`
(`EMIT-FARITH`, `EMIT-FSQRT`). It cannot test the answer alone, because an
operand NaN passes through with its own sign: `cmpunordsd` masks the operands
before the operation and the answer after it, and the masks differ exactly when
the operation made the NaN. Nothing branches; the cost is nine instructions on
each of `f+ f- f* f/ fsqrt`, and none on conversions, `fnegate`, `fabs` or
comparisons.

The rule was chosen over a sign-blind `f.`, which costs nothing per operation
but changes output both engines pin, and over making ARM64 answer SSE's NaN,
which would cost ARM64 the same test on every operation.
`test/prim-float-cases.f` pins the rule on both targets. It does not cover a
signalling NaN beside a quiet one, which only a bit cast or memory supplies:
ARM64 answers the signalling one quieted, SSE the left one.

## Wasm

Checked Habu compiles directly to a core Wasm module through
`src/arch/wasm/` and `tools/wasm-build.f`; the production path depends on no
LLVM, Emscripten or Binaryen. The backend reuses the checker, the recorded
tape, the elaborator and the frozen HIR; each file's header states its part.
A browser host takes turns with the module through its memory
(`src/lib/browser/`).

### Decisions

| Concern | Decision |
|---|---|
| Profile | `wasm32-unknown-unknown`: ABI `habu-wasm-cell64-v1`, memory32, one unshared memory, little-endian; the contract carries `MASK-WASM` |
| Features | scalar core plus multi-value, saturating float-to-int and bulk memory, stated in `src/arch/wasm/profile.f` (`WPROF`) |
| Cell | 64 bits; `CELL = 8`; no cell, product or mask shrinks |
| Pointer | a canonical zero-extended 32-bit offset in an eight-byte cell, narrowed only after its check |
| Representation | a `WSTRUCT` control-flow graph frozen with the full `IR-BUILD:FREEZE` before encoding |
| Route | an `NSHADOW` binding; no Wasm routine is placed in the engine's code region (`src/arch/wasm/passes.f`) |
| Imports | none; no start function |
| Registers | none to allocate: one Wasm local per value, and the engine owns native register allocation |

A 64-bit integer does not need memory64: Wasm's value types and memory address
types are independent. Generic code never infers `CELL` or pointer-slot width
from address width.

### Control

The selector reads the folded interim HIR and builds a `WSTRUCT` module; the
full freeze restores the checks `IR-BUILD:FREEZE-INTERIM` skipped, because a
Wasm backend has no machine module to verify instead
(`src/arch/wasm/wstruct.f`). `src/arch/wasm/structure.f` derives Wasm's
`block`, `loop` and `if` from dominators and refuses an irreducible graph with
`E-WCTL-IRREDUCIBLE`; a block-argument transfer is a parallel copy, because
sequential copies on a swapping edge miscompile. Stack shuffles and the
admitted return-stack operations rename values at compile time; Wasm's operand
stack is not Habu's return stack. A tail call is a call then a return, which is
not bounded space for mutual recursion.

### Calls

- A direct call is `(ctx:i32, inputs x i64) -> (status:i32, outputs x i64)`.
  Past 16 input or 16 output lanes a function takes the aligned frame,
  `(ctx:i32) -> (status:i32)`, its cells on the context stack. 16/16 is a
  pinned ABI parameter in `WPROF`, never an optimisation choice; changing it
  is an ABI revision.
- Status 0 is success; status 1 is a catchable throw whose full 64-bit code is
  in `ctx.throw-code`, never truncated into the status. Callers test status
  before reading outputs.
- At every call the live row is stored to the context stack and reloaded
  after, as the native boundary stores it, so `depth`, `.s` and `catch` see
  what they see natively.
- A dynamic call (`execute`, a stored quotation, a deferred word) goes through
  a uniform `(ctx:i32) -> i32` adapter that checks stack depth before
  dispatch (`src/arch/wasm/dynamic.f`).
- Browser-facing calls pass `i64` as `BigInt`, never `Number`, and read
  exported `i32` addresses as unsigned. No layout is presumed to match the
  wasm32 C ABI.

### Exceptions and fatal failures

There is no native unwinder: a throw stores its code in the context and
propagates status through generated return paths. `catch` restores stack depth
only; it does not restore overwritten arguments, ownership, heap contents or
host effects. A fatal failure (a stack store past its region, a pointer that
fails its check, an impossible tag) records a diagnostic and executes
`unreachable`; it is never a catchable error, and a trap rolls back no memory
write.

### Arithmetic

The selector keeps the language rules ("Numeric admission"): a zero divisor
stores `E-DIV-ZERO` and returns status 1 before any divide, and `-1` answers
`0 - n`, so `MIN-N -1 /` is `MIN-N` where `i64.div_s` would trap. An observable
flag is `0 - extend_u(p)`, because Wasm comparisons answer 0/1. A raw 64-bit
condition is tested against zero before it becomes an `i32`; truncating it can
turn nonzero into false.

### Floating point

Wasm permits several NaN payloads; Habu's NaN rule binds it anyway, and the
selector applies it without a branch. `f>s` is `i64.trunc_sat_f64_s`, which
already truncates, saturates and answers 0 for a NaN. Contraction,
reassociation and relaxed SIMD stay off. Base Wasm has no fused multiply-add,
and a multiply followed by an add is not a substitute where one rounding is
required.

### Execution tokens

An xt is its adapter's table slot, fixed for the module's life. Slots number
from 1: slot 0 is a null funcref, so `0 execute` traps as native execute of 0
fails, and an xt with an upper bit set traps before dispatch. Every adapter
shares one Wasm type, so Wasm's signature check cannot see Habu effects or
types; the adapter checks depth itself. Quotations capture nothing, as
natively; code inspection and patching have no Wasm form.

### Linking

Before lowering, every reachable definition is guest code, guest data, a
kernel row or refused: native syscalls, `dlopen`, host addresses and code
pointers have no Wasm form. A target data builder lays out static bytes; no
native dictionary or snapshot is copied in. `src/arch/wasm/link.f` numbers
functions by origin (kernel, captured, adapter, then the four wrappers),
deduplicates types by encoding and patches each site in place. Changing an
index can change an LEB's length, so every call and address immediate is a
padded LEB (5 bytes for a u32, 10 for an s64, `src/arch/wasm/leb.f`) and the
padded size is accepted. The engine primitives a module calls are
`src/arch/wasm/kernel.f` and `kernel-words.f`. Memory regions are fixed in
`WPROF`: a null page, the context, output, the data stack, the DATA region and
the captured image; address zero is in bounds in Wasm, so null is enforced by
the checked access, not by a guard page.

### Encoder

`src/arch/wasm/encode.f` writes the binary in checked Habu with checked LEB128
routines; section order follows the specification, not a sort by section ID.
A custom section would be evidence, not authority: a loader compares the actual
imports, memory and table declarations.

### Validation and proof

Three properties stay separate, and none replaces another: the checker
validates declared effects and the ownership rules it implements; backend
validators check representation, lowering and publication; the Wasm engine
validates and isolates execution. A valid module can still compute the wrong
answer or overwrite another object in its memory, so Wasm validation never
widens Habu's safety claims. No `TRUSTED:` wrapper is added around a compiler
algorithm to get it through the checker. An independent engine validator runs
during qualification ([gate.md](gate.md)); engine validity is necessary, not a
semantic proof.

## Freestanding targets

Freestanding is not the hosted runtime with the filesystem removed. A board
target needs its memory regions, boot protocol, vectors, stacks and image
format as pinned board inputs, never compiler defaults; load and run addresses
are separate; MMIO keeps access count, width and order; DMA buffers need the
board's flush and invalidate rules. No board target is built yet: what exists
is the instruction constructors (`src/arch/arm32/asm.f`, `src/arch/tic6x/`)
and their tests ("Embedded encoders").

### Cortex-M

The first microcontroller target. `src/arch/arm32/asm.f` constructs A32 and
Thumb instructions; its Thumb subset is ARMv7E-M. Nothing yet lowers to it,
lays out an image, loads it or talks to a board.

#### Decisions

- **Board and core.** Cortex-M4 with a USB serial console, because the encoder
  subset is ARMv7E-M; a Cortex-M0+ would need the Thumb-1 subset. The gate runs
  on QEMU's `mps2-an386` (a Cortex-M4), so no hardware is needed for green.
- **Host-resident compiler, target-resident monitor.** The checker and compiler
  stay on the host; the target runs a monitor of a few kilobytes speaking one
  protocol over serial: read bytes, write bytes, call address, report
  registers, reset. Generated code lands in a running target without a
  target-side interpreter (SwiftX's model).
- **Image.** One flat image: vector table, reset handler setting the stacks and
  user area, the primitive set of `src/habu/prims.f`, the cooperative kernel,
  vectored terminal I/O over the UART, the monitor, then the application. No
  heap unless a program links the allocator.
- **Kernel.** The cooperative round robin of `src/lib/task.f`, with the same
  public names; interrupt handlers do the time-critical work and wake the
  waiting task.
- **I/O.** `EMIT`, `KEY`, `KEY?`, `TYPE`, `ACCEPT` dispatch through per-task
  user vectors; the UART driver is interrupt-driven with a byte ring, and its
  blocking words sleep until the interrupt wakes them.
- **Loading.** Development uses the monitor over serial, with XMODEM for whole
  images through the existing peer (`test/xmodem.py`). Production flashing is
  the board's tool, not Habu's.

#### The lowering

The target-free front (`hir.f`, `elaborate.f`, `loop.f`) is reused. The
backend registers with the target registry as x86-64 does: a Thumb-2 machine
IR, selection onto the encoder constructors, the shared allocator
parameterised for `r0`..`r12` with the VM registers fixed (`r7` user area,
`r9` data stack, `r10` data base, `r11` code pointer), the shared `spill.f`,
and an emitter producing a position-fixed image. One ARM32 backend selects CPU,
instruction state and float ABI independently: FPU presence does not select
hard-float argument passing.

#### Gate

- The encoder tests already in the tree.
- A QEMU device peer under `test/`, started like the serial peer
  (`test/serial.py`), booting the image on `mps2-an386` with its UART as the
  serial link.
- The monitor protocol test: write bytes, read them back, call a word that
  toggles a memory cell, reset.
- The kernel test: two tasks and an interrupt-driven UART echo on QEMU.
- The demo: a background task driving a GPIO pattern while the console
  answers.

### TI C66x

`src/arch/tic6x/` holds the constructors (`asm.f`), the per-instruction
resource facts (`facts.f`), an execute-packet simulator (`sim.f`) and a
scheduled helper writer (`eabi.f`). A correct simulator fixture is evidence for
its subset, not for vendor libraries or silicon. A product must still obey
register-pair layout, delay slots, call-clobber rules, stack alignment and
relocation ranges.

## Embedded encoders

`src/arch/arm32/asm.f` (`A32ASM`), `src/arch/tic6x/asm.f` (`C6XASM`) and
`src/arch/x86-64/asm.f` (`X64ASM`) construct instructions in checked Habu and
stop there: none lowers IR, allocates, schedules, relocates or writes an
executable.

### Verification

Run the focused tests through the native engine:

```sh
bin/hb --load test/compiler/tic6x-asm.f </dev/null
bin/hb --load test/compiler/arm32-asm.f </dev/null
bin/hb --load test/compiler/x86-64-asm.f </dev/null
python3 test/compiler/embedded-asm.py --tic6x-prefix /path/to/tic6x-elf-
```

The x86-64 suite needs no assembler at test time: each form is pinned to a
byte string, with the `llvm-mc -triple=x86_64 -show-encoding` line that
produced it in a comment above the case. A relative branch's bytes come from
`llvm-mc -filetype=obj` and `llvm-objdump`, because `llvm-mc` leaves a fixup
for a symbolic target.

`test/compiler/embedded-asm.py` is a device peer that compares checked Habu
output with independent assemblers: LLVM `llvm-mc`/`ld.lld`/`llvm-objcopy` for
ARM/Thumb and GNU `tic6x-elf-as`/`ld`/`objcopy` for the C674x/C66x subset.
`--family arm` or `--family c6x` selects one family; generated files stay under
the ignored `tmp/embedded-asm/`. A passing comparison proves encoding only: no
encoder's output has run on its hardware.

## Porting

A new port adds one target seam and proves it with native refresh and the port
gate; it adds no host-language build logic. The directory structure of a seam
is `src/os/AGENTS.md`.

### Source Selection

The target is wired in exactly these source-list owners:

- `tools/bootstrap.sh` for no-binary recovery;
- `tools/build-fixpoint.f` for native refresh, AOT and REPL builds;
- `tools/native-emit.f` for the emitter's own seam load;
- `src/habu/habu2.f` and `bootstrap/cg/forth.fs` for the `--load` prefix;
- `src/habu/stdin.f` and `tools/hb-build-lib.f` for baked REPL sources;
- `tools/lint/shadow-lint.f` for lint coverage.

No caller passes a target prelude for normal tests: `bin/hb --load` selects the
host prefix from the running binary. Selection fails closed: no branch means
"Linux, otherwise macOS"; every selector handles each supported target
explicitly and raises a named target-unknown error when none matches.

### Engine Primitives

A new backend adds bodies under the `src/habu/prims.f` row names and declares
no effects of its own; the gates of x86-64 "The primitive table" refuse a body
with no row and a row with no body. An `EPREFIX-PROVIDED!` row keeps its
dictionary record: a seeded backend jumps through its registered DATA cell, and
the gate refuses an empty cell. Rows of kind `ELAB` (effect computed at the
call site) and `UNROWED` name the primitives that deliberately have no row.

### Native Compiler ABI

`src/compiler/native/abi.f` maps the target predicates to `aapcs64-linux`,
`aapcs64-darwin` or `sysv-amd64`; x18 is allocatable only on Linux AArch64.
Habu-to-Habu calls use Habu's internal contract on every target: arguments and
results occupy data-stack slots, the register pool is scratch, frames are
16-byte aligned. No compiler consumer uses platform variadic or stack-argument
rules.

### Syscalls And Signals

All kernel entry goes through `sys.f`; the number register and trap
instruction are the target's (`svc #0x80` on macOS, `svc #0` on Linux
aarch64, `mov eax, imm32` then `syscall` on x86-64). Numbers are chosen for the
argument shapes the engine's primitives build: the engine passes `AT_FDCWD`
and a flags register, so both Linux seams use the *at* family (`faccessat`,
`unlinkat`, `renameat`, `fchmodat`, `newfstatat`, `openat`, `mkdirat`), with
`unlinkat` serving rmdir and `newfstatat` serving lstat.

The x86-64 seam maps the engine's x0..x5 onto `rdi rsi rdx r10 r8 r9` in that
order. Its error test reverses the operands: aarch64 reconciles `-errno` into
the carry with `cmp x0, #-4095`, where ARM sets C on no borrow, but x86 sets CF
on borrow, so `cmp rax, -4095` would flag success. `mov rcx, -4096` then
`cmp rcx, rax` (rcx is free because `syscall` clobbers it) sets CF on the same
range, so `SYS-PUSH`'s consumers read the bit the same way on both.

Signal handlers are target ABI boundaries: crash and profiler handlers use the
target's `sigaction` frame, ucontext PC offset, `sigreturn` convention and
signal list, and Linux `rt_sigaction` needs the sigset size.
`src/habu/prof.f` models the two aarch64 hosts and refuses any other target at
load rather than emit an aarch64 frame; `src/habu/prof-x64.f` is the x86-64
twin, walking from `RSP` because x86-64 has no link register.

### Executable Images

Drivers use the checked phase chain `ASM-CODE BUILD-IMAGE SET-SIGID CODESIG2
DRV-WRITE-IMAGE`, whose nominal phase cells exist so the checker enforces the
order; the target image file implements the format. `src/os/image-bytes.f`
owns the shared byte buffer, endian stores and patch helpers; target image
files own only format layout. Raw byte counts convert with `M-LEN` and `M-OFF`
at the format boundary before they reach the typed helpers.

- macOS writes Mach-O and signs it (`src/os/macos/macho.f`).
- Linux writes ELF64 whose executable `PT_LOAD` is read+execute and not write.
  Both Linux seams write the same header kinds, the same `PROT-PAGE-MAX`
  boundary and the same dynamic tags; the architecture owns `e_machine`, the
  interpreter and the GOT relocation type.

Headers are rebuilt from constants and native refresh reaches a byte-for-byte
fixpoint on every target. Snapshot trailer offsets come only from
`src/habu/layout.f` (`SNAP-TRL-*`); a port never spells them, and a port
without fixed mappings must implement full relocation before it passes refresh.

A Windows port needs a PE/COFF writer, Windows host services in place of the
syscall seam, and foreign calls under the Microsoft x64 convention (positional
argument registers and caller-provided home space); generic COM belongs in a
library.

### Port Gate

After `bin/hb` exists on the target, run only native validation on the target
machine:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force

bin/hb --load test/run.f
```

Never run a port's tests from another host by injecting its source prelude:
bring up `bin/hb` on the target machine and let it select its own sources.

### Future ports

The Gforth seed emits ARM64 only. Bring up another host by cross-building with
an existing Habu compiler and running the product there; extend the no-binary
seed only when native recovery on that host is needed.
