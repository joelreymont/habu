# Cross-compilation and targets

How Habu builds for a machine other than the one running it: the roles of the
build host, the compiler product and the output target, the rules every backend
keeps, the shape of each non-host target (x86-64, Wasm, freestanding ARM32 and
C66x), and the procedure for a new port. Each file's header states its own
part; this page holds what spans files. The ARM64 host engine is
[architecture.md](architecture.md).

## Build host, product and target

Three roles stay separate, and `src/compiler/target/model.f` gives each its own
type:

- The **execution platform** is the process ABI of the running `hb`, not the
  physical machine: an x86-64 process under an emulator on ARM hardware is an
  x86-64 compiler.
- A **compiler product** records its executable target, its enabled emitter
  families and its default output profile. Enabled emitters are product
  composition, not an application dependency: an application built by a
  product with five backends carries none of them.
- A **build action** pairs the execution platform with the target its bodies
  are compiled for; `tools/build-target.f` scopes one action around the native
  build entry and restores the pending selection on return or throw.

An emitter's implementation target and the instructions it writes are
distinct: code that builds an x86-64 instruction is ordinary Habu compiled for
the product's host. The running compiler supplies every body that executes
during a build; product-host definitions never replace them mid-build.

Execution is a separate action. A cross-build never tests an output by
launching it through a host process API: only a link-compatible target with the
host's image format and the native runtime runs here, and a Linux x86-64
product built on ARM64 is run on an x86-64 machine.

## Target contract

`src/compiler/target.f` is the immutable contract every compilation carries:
architecture, ABI, byte order, pointer width and features. It refuses only
incoherent combinations, so a contract can be declared before any backend for
it exists; an architecture with no loaded backend is refused at the first
compiler stage. A named profile fixes the architecture, pointer width, foreign
ABI, runtime, image format and Habu ABI; the Windows profiles are declarable
products whose process and image services do not exist.

Three questions have three answers and must not be merged: whether two targets
link together (architecture, byte order, pointer width, Habu ABI, foreign ABI
and runtime), whether a runtime admits a target (link-compatible, with every
required feature), and whether two builds are the same (same resolved target
and same action inputs). The Habu ABI (internal calls, value conventions,
execution-token representation), the foreign ABI (C marshalling) and the image
format are separate fields, because one overloaded `abi` field would decide all
three. A structurally coherent target can still be unsupported by the installed
backend; the two errors stay distinct.

## Compile-time execution and target data

A cross-build runs the target's whole source prefix on the host: definers,
`create`, `here` and stores execute against host memory. Target data reaches
the output only through tracked owners, never by copying a host image:

- `src/compiler/native/shadow.f` compiles each definition a second time for the
  target from the same frozen HIR module and keeps it as a sealed unplaced
  emission whose every call and address is a row (x86-64 "Dual emission").
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

The Habu cell is 64 bits on every target; target address width is independent
of it.

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
answer (`src/habu/arith-abi.f`, `src/compiler/numeric-policy.f`):

- `+ - *` wrap modulo 2^64, including on register-pair targets;
- a zero divisor throws `E-DIV-ZERO` and `MIN-N -1 /` wraps to `MIN-N`, both
  guarded before a machine divide that would trap (x86-64 `idiv`, Wasm
  `i64.div_s`);
- `/`, `mod` and `/mod` truncate toward zero;
- `lshift`/`rshift` take the count modulo 64 and `rshift` is logical;
- a comparison observed as a cell is the all-ones flag, never 0/1;
- `f>s` truncates, saturates and answers 0 for a NaN;
- an operation that makes a NaN answers the positive quiet NaN and a quiet NaN
  operand passes through, the left of two; no profile selects the rule away
  ("The NaN an operation makes").

Constant folding uses target semantics, never the build host's rounding or
integer width. Catchable throws keep the full 64-bit code; no Habu unwind
escapes through C frames, and a C callback catches a throw and answers its
declared fallback. A fatal invariant failure ends or poisons the runtime; it is
never turned into a catchable error.

## Linking and publication

- Native in-process publication only enters a runtime with the same execution
  machine (`src/compiler/native/publish.f`); cross-generated code goes to an
  artifact.
- Publication is ordered: a pending routine is proven, committed to the code
  region, filed with the shadow and only then appended to the dictionary, so a
  failure leaves no callable half-word.
- File publication is staged: every `-o` and engine write lands whole or not at
  all (`src/lib/fs-mutate.f`).
- A relocation patches the exact field its encoding defines and checks the
  whole field's range: an x86-64 `rel32` counts from the end of the
  instruction; a Wasm function, type or table reference is an index, written as
  a fixed-width padded LEB so linking never changes a length (Wasm "Linking").
- A linker refuses before writing its first byte; nothing leaves the process
  ahead of a refusal.

## The NaN an operation makes

One rule holds on every target (`src/compiler/native/hir-word.f`
`DEF-FLOAT`): an operation that makes a NaN from numbers answers
`$7FF8000000000000`, the positive quiet NaN, and a quiet NaN operand passes
through unchanged, the left of two. `f.` prints `-` exactly when bit 63 is set,
so `-1 fsqrt f.` and `0 0 f/ f.` print `0.000000` everywhere.

ARM64 answers the rule as it stands; SSE makes the negative NaN, so the x86-64
selector corrects the sign without a branch, and Wasm's selector does the same
for its permitted payloads. The rule was chosen over a sign-blind `f.`, which
costs nothing per operation but changes output both engines pin, and over
making ARM64 answer SSE's NaN, which would cost ARM64 the same test on every
operation. `test/prim-float-cases.f` pins the rule on both native targets. A
signalling NaN beside a quiet one, which only a bit cast or memory supplies,
is not covered: ARM64 answers the signalling one quieted, SSE the left one.

## x86-64

The x86-64 backend shares the target-free compiler with ARM64 and forks
nothing else by accident. Linux x86-64 is the target; the ARM64 engine
cross-builds it.

A second architecture touches five layers: the assembler
(`src/arch/x86-64/`), the engine primitive bodies (`src/habu/kernel-x64.f`,
under the rows of `src/habu/prims.f`), the compiler's dialect, selector and
emitter (`src/compiler/native/*-x64.f`, `x64ir.f`; the allocator and spill pass
are shared, parameterised by a register file), the OS seam
(`src/os/linux-x86-64/`, the same file set and word names as `src/os/linux/`)
and the runtime ABI (SysV FFI, task entry, the same guard-page model). The HIR,
the elaborator and the loop pass are target-free. A backend is one row in
`src/compiler/native/backend.f`, registered when its `passes.f` loads: the
driver names no backend, and `bin/hb` for one host does not carry the other.

### Machine model

- **VM registers.** SysV's callee-saved set is exactly the reserved VM set
  less `rsp`, so a foreign call preserves the VM with no save block.
  `src/habu/layout.f` declares the reserved set once; compiler code running on
  the ARM64 engine names the x86-64 set from there.
- **Calls between compiled words** use the ARM64 internal convention: stack
  cells in memory, live values in registers within a routine.
- **Literals.** `mov r64, imm64` is the one relocatable literal, one site to
  patch (`src/habu/address-carrier.f`).
- **Traps** use the same mmap protections; the handler reads `RIP` and `RSP`.

### Dual emission

A cross-build runs the target's whole prefix on the host, so each definition
must be the host's routine and the target's at once; its names are bound as it
compiles, so the target's routine cannot be lowered later from the tape.
`src/compiler/native/shadow.f` is that second target, compiled from the same
frozen HIR module in a nested context; a shadow refusal refuses the definition.
With no shadow open the driver runs exactly its old stages, so an engine
compiles every definition to the same bytes.

The capture carries the shadow's map (`src/habu/aot-shadow.f`). Code bytes
carry no host address: call fields stay zero, a code literal holds 0, a DATA
literal holds its window coordinate. Two reaches decide what ships, because
x86-64 and ARM64 optimise differently and ARM64 reach cannot answer which x86
routines are needed: the capture's reach decides which stripped or retired
records are live, and the shadow's reach follows the x86 shadow's own call and
code-address sites. Every record index is a shipped row, never a window index,
because stripping and `undefine` renumber records. A merge refuses a shadow on
either side, because the merged engine is the ARM64 seed.

### The OS seam

`src/os/linux-x86-64/` is a seam of the same shape as `src/os/linux/`
(Porting). Both Linux targets share `src/os/linux/layout-constants.f`, so
captured DATA pointers need no translation between the two Linux hosts. Seam
register arguments are x86-64 register numbers moving through an
upward-growing data stack, and the seam's instruction stencils are byte strings
of whole instructions appended as they stand. `tools/native-emit.f` sends an
x86-64 target to `tools/native-emit-x64.f`, which loads the x86 code stream
before the window installs its compiler, so the ARM64 assembler cannot bind
it.

### Fixed segments

An x86-64 image places its code region by `PT_LOAD`, where the ARM64 boot maps
it near a hint. DATA is no segment on either target: both boots map it at its
fixed address and copy in the bytes the image carries, which an x86-64 image
holds in a read-only carrier `LOAD` at the region's end
(`src/os/linux-x86-64/elf.f`).

- No `LOAD` reserves DATA, so the kernel neither commit-charges it nor starts
  the brk inside it.
- Boot copies bytes; nothing rebases. The region and DATA addresses are fixed,
  so neither code nor DATA pointers are rebased on restart, and a snapshot
  restore checks the carrier before it maps DATA.
- The target's layout is not the host's: a macOS host places its own DATA
  elsewhere, so x86 code reads the target's layout from
  `src/os/linux-x86-64/target-layout.f`, qualified.

### Live-region sites

The live code region records its sites (calls and address literals a later
pass rewrites) where code is built, and never finds them by decoding bytes. On
ARM64 the record is two bitmaps, one bit per four-byte word; an x86-64 site
starts at any byte, so the same bytes hold a band of ascending rows. A site's
offset is its instruction's first byte on both targets. Both views are declared
on every target, because `src/` has no load-time target selection; the running
engine picks one, and one reader (`src/habu/sites.f`) serves both. Snapshot
relocation does not apply: the region and DATA are fixed, so the rows serve
publication and the AOT capture only.

### Build and bootstrap

Cross-build first, self-host second.

1. **The engine is tier-1 only.** A small hand-written kernel is bound to the
   `src/habu/prims.f` names; the outer interpreter, definers, packages, source
   loading and `MAIN` are checked Habu captured into both products. There is no
   x86 copy of `habu2.f` or `jit.f`.
2. **The ARM64 engine cross-builds by dual emission.** Loading the prefix
   executes on the host, so the host cannot switch its contract; each
   definition's one HIR is emitted twice and the capture carries the shadow.
3. **A host-side Habu linker writes the image** (`src/habu/link-x64.f`). It
   resolves every record, call, literal and DATA cell at write time; no
   relocation runs at boot or snapshot restore.
4. **The cross-built engine self-hosts** to a byte fixpoint on an x86-64 host.
5. **Recovery.** The Gforth chain emits ARM64 only and refuses x86-64 by name;
   x86-64 recovery is a cross-build from a working ARM64 engine, and
   `tools/native-build.f` is its only route.

### Gates and acceptance

- The primitive parity gate (`test/prim-parity.f`), green on both backends.
- `test/run.f` green on an x86-64 host on a cross-built engine, then on its
  own fixpoint engine.
- The FFI suite under SysV, the task suite, the guard-page suite, and an
  AOT-stripped application running.
- A native x86-64 build records the emission it published and reaches the same
  byte fixpoint, engine and names sidecar, across three generations before a
  product is promoted.

### Build target

`tools/native-build.f -- <out> [whitebox] [--target <target>]` builds for
`linux-aarch64`, `macos-aarch64` or `linux-x86-64`, by default for the engine
running it. The window gets that target's `target.f` and `layout.f`, and inside
it the window's own predicates answer; the building engine's predicates are not
touched. The entry refuses a target whose architecture has no backend
registered in the building engine and loads none itself, so a cross-build
loads the backend ahead of the driver:

```sh
bin/hb --load src/arch/x86-64/passes.f tools/native-build.f -- <out> --target linux-x86-64
```

The building engine's own checker checks the backend, so the engine must
know every rule the backend's source uses: an engine without the `xt` type
refuses `select-x64.f`'s `XT>N` cast with E-CAST-FAM. Cross-build with an
engine built from the same tree.

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
| Profile | `wasm32-unknown-unknown`: memory32, one unshared memory, little-endian |
| Features | scalar core plus multi-value, saturating float-to-int and bulk memory (`src/arch/wasm/profile.f`) |
| Cell | 64 bits; no cell, product or mask shrinks |
| Pointer | a canonical zero-extended 32-bit offset in an eight-byte cell, narrowed only after its check |
| Representation | a structured control-flow graph, fully frozen before encoding |
| Route | a shadow binding; no Wasm routine is placed in the engine's code region |
| Imports | none; no start function |
| Registers | none to allocate: one Wasm local per value |

A 64-bit integer does not need memory64: Wasm's value types and memory address
types are independent. Generic code never infers the cell or pointer-slot width
from address width.

### Control

The selector builds a structured module from the folded HIR; the full freeze
restores the checks the interim freeze skipped, because a Wasm backend has no
machine module to verify instead. Wasm's `block`, `loop` and `if` derive from
dominators, and an irreducible graph is refused; a block-argument transfer is a
parallel copy, because sequential copies on a swapping edge miscompile. Stack
shuffles and the admitted return-stack operations rename values at compile
time; Wasm's operand stack is not Habu's return stack. A tail call is a call
then a return, which is not bounded space for mutual recursion.

### Calls

- A direct call is `(ctx:i32, inputs x i64) -> (status:i32, outputs x i64)`.
  Past 16 input or 16 output lanes a function takes the aligned frame,
  `(ctx:i32) -> (status:i32)`, its cells on the context stack. 16/16 is a
  pinned ABI parameter, never an optimisation choice; changing it is an ABI
  revision.
- Status 0 is success; status 1 is a catchable throw whose full 64-bit code is
  in the context, never truncated into the status. Callers test status before
  reading outputs.
- At every call the live row is stored to the context stack and reloaded
  after, as the native boundary stores it, so `depth`, `.s` and `catch` see
  what they see natively.
- A dynamic call (`execute`, a stored quotation, a deferred word) goes through
  a uniform `(ctx:i32) -> i32` adapter that checks stack depth before
  dispatch.
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
throws before any divide, and `-1` answers `0 - n`, so `MIN-N -1 /` is `MIN-N`
where `i64.div_s` would trap. An observable flag is `0 - extend_u(p)`, because
Wasm comparisons answer 0/1. A raw 64-bit condition is tested against zero
before it becomes an `i32`; truncating it can turn nonzero into false.

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
native dictionary or snapshot is copied in. Changing an index can change an
LEB's length, so every call and address immediate is a padded LEB and the
padded size is accepted (`src/arch/wasm/link.f`, `leb.f`). Memory regions are
fixed by the profile; address zero is in bounds in Wasm, so null is enforced by
the checked access, not by a guard page.

### Encoder

`src/arch/wasm/encode.f` writes the binary in checked Habu; section order
follows the specification, not a sort by section ID. A custom section would be
evidence, not authority: a loader compares the actual imports, memory and table
declarations.

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

The first microcontroller target. Nothing yet lowers to the ARM32 encoder, lays
out an image, loads it or talks to a board. The decisions:

- **Board and core.** Cortex-M4 with a USB serial console, because the encoder
  subset is ARMv7E-M; a Cortex-M0+ would need the Thumb-1 subset. The gate runs
  on QEMU's `mps2-an386` (a Cortex-M4), so no hardware is needed for green.
- **Host-resident compiler, target-resident monitor.** The checker and compiler
  stay on the host; the target runs a monitor of a few kilobytes speaking one
  protocol over serial: read bytes, write bytes, call address, report
  registers, reset. Generated code lands in a running target without a
  target-side interpreter (SwiftX's model).
- **Image.** One flat image: vector table, reset handler setting the stacks and
  user area, the primitive set of `src/habu/prims.f`, the cooperative kernel, vectored terminal I/O
  over the UART, the monitor, then the application. No heap unless a program
  links the allocator.
- **Kernel.** The cooperative round robin of `src/lib/task.f`, with the same
  public names; interrupt handlers do the time-critical work and wake the
  waiting task.
- **I/O.** `EMIT`, `KEY`, `KEY?`, `TYPE`, `ACCEPT` dispatch through per-task
  user vectors; the UART driver is interrupt-driven with a byte ring, and its
  blocking words sleep until the interrupt wakes them.
- **Loading.** Development uses the monitor over serial, with XMODEM for whole
  images through the existing peer (`test/xmodem.py`). Production flashing is the board's tool,
  not Habu's.
- **Lowering.** The target-free front is reused and the backend registers as
  x86-64 does: a Thumb-2 machine IR, selection onto the encoder constructors,
  the shared allocator over `r0`..`r12` with the VM registers fixed (`r7` user
  area, `r9` data stack, `r10` data base, `r11` code pointer), the shared spill
  pass, and an emitter producing a position-fixed image. One ARM32 backend selects CPU,
  instruction state and float ABI independently: FPU presence does not select
  hard-float argument passing.
- **Gate.** The encoder tests; a QEMU device peer under `test/`, started like
  `test/serial.py`, booting the image on `mps2-an386` with its UART as the serial link; a monitor protocol
  test (write bytes, read them back, call a word that toggles a memory cell,
  reset); a kernel test with two tasks and an interrupt-driven UART echo; and
  a demo with a background task driving a GPIO pattern while the console
  answers.

### TI C66x

`src/arch/tic6x/` holds the constructors, the per-instruction resource facts,
an execute-packet simulator and a scheduled helper writer. A correct simulator
fixture is evidence for its subset, not for vendor libraries or silicon. A
product must still obey register-pair layout, delay slots, call-clobber rules,
stack alignment and relocation ranges.

## Embedded encoders

`src/arch/arm32/asm.f`, `src/arch/tic6x/asm.f` and `src/arch/x86-64/asm.f`
construct instructions in checked Habu; on ARM32 and C66x nothing lowers IR,
allocates, schedules, relocates or writes an executable yet. Run the focused
tests through the native engine:

```sh
bin/hb --load test/compiler/tic6x-asm.f </dev/null
bin/hb --load test/compiler/arm32-asm.f </dev/null
bin/hb --load test/compiler/x86-64-asm.f </dev/null
python3 test/compiler/embedded-asm.py --tic6x-prefix /path/to/tic6x-elf-
```

The x86-64 suite needs no assembler at test time: each form is pinned to the
byte string `llvm-mc` produced. `test/compiler/embedded-asm.py` is a device
peer that compares checked Habu output with independent assemblers (LLVM for
ARM/Thumb, GNU `tic6x-elf-*` for the C674x/C66x subset); `--family arm` or
`--family c6x` selects one family. A passing comparison proves encoding only:
no encoder's output has run on its hardware.

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
no effects of its own; that file's header names the gates that refuse a body
with no row and a row with no body.

### Native Compiler ABI

Habu-to-Habu calls use Habu's internal contract on every target: arguments and
results occupy data-stack slots, the register pool is scratch, frames are
16-byte aligned. No compiler consumer uses platform variadic or stack-argument
rules; `src/compiler/native/abi.f` maps each target to its foreign convention.

### Syscalls And Signals

All kernel entry goes through the seam's `sys.f`, with the target's number
register and trap instruction. Numbers are chosen for the argument shapes the
engine's primitives build, so both Linux seams use the *at* family. A seam
reconciles the kernel's error return into the carry bit the engine's
primitives read, whatever the machine's own flag convention.

Signal handlers are target ABI boundaries: crash and profiler handlers use the
target's `sigaction` frame, ucontext PC offset, `sigreturn` convention and
signal list, and Linux `rt_sigaction` needs the sigset size. A handler written
for one target refuses another at load rather than emit the wrong frame
(`src/habu/prof.f`, `src/habu/prof-x64.f`).

### Executable Images

Drivers run a checked phase chain whose nominal phase cells make the checker
enforce the order; the target image file implements the format, and
`src/os/image-bytes.f` owns the shared byte buffer and patch helpers.

- macOS writes Mach-O and signs it (`src/os/macos/macho.f`).
- Linux writes ELF64 whose executable `PT_LOAD` is read+execute and not write;
  both Linux seams write the same header kinds and dynamic tags, and the
  architecture owns `e_machine`, the interpreter and the GOT relocation type.

Headers are rebuilt from constants and native refresh reaches a byte-for-byte
fixpoint on every target. Snapshot trailer offsets come only from
`src/habu/layout.f`; a port never spells them, and a port without fixed
mappings must implement full relocation before it passes refresh.

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
