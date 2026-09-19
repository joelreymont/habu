# INTEL.md — the x86_64 backend from the Intel machine

For the agent that continues campaign C6 (`habu-campaign-c6-targets-86bb56bb`)
on the Intel (x86_64 Linux) machine. Read `docs/x86-64.md` (the design) and
`docs/porting.md` (the seam rules) first; this file is the state and the entry
point. Hazel, who runs Habu on the arm64 host, keeps the State section current
and rewrites the Landed section as each arm64 lane lands.

## Experiment reconciliation

The `experiment/wide-typed-locals`, `experiment/intel-bootstrap-20260919` and
`experiment/x86-native-20260919` branches start at `51546316`; the integration
line at `39d9a399` supersedes their x86 architecture work. Keep the current
`src/arch/x86-64/` encoder and backend, X64IR, NMACH/NEFF, ELF64/OS seam and
`SYSV-AMD64` identity. HIR already reads the architecture from its binding.
The older task status below predates this reconciliation: Hazel owns the
compiler pipeline; Alder owns recovery, cross-build and the ThinkPad peer.

The four recovery repairs were checked against the line:

| Experiment repair | Disposition |
| --- | --- |
| Atomics | Salvage `atomic@`, `atomic-add` and `fence`; `atomic!` and `atomic-cas` already exist. The stage0 runtime fixture checks returned old values, final values and protected-write refusal. The removed bootstrap optimizer needs no barrier classification. |
| libc realpath bridge | Keep the static Linux seed's existing lexical normalization for the symlink-free source paths used by recovery. This is not libc realpath: it does not resolve symlinks, and the macOS seed still refuses. The experiment's dynamic loader is not imported. |
| Cold-image alignment | Not applicable to the static seed: both its ELF writer and cold-size check use 4 KiB rounding. The experiment changed its ELF to 64 KiB alignment and a dynamic segment. |
| Provided rows | Already covered: prelude/errors in `PFX-PROVIDE-STDLIB-FILES`, dynamic-storage in `PFX-PROVIDE-CORE-FILES`. |

The encoder comparison found three missing families, recorded with measured
llvm-mc vectors: scalar SSE2 (`65313206`), immediate memory stores (`45775daf`),
and NOP/UD2 (`8c377734`). RIP-relative addressing, CL shifts, SETCC, CMOVCC and
relative/register call and jump forms are already present. Do not import the
experimental instruction-value representation or its competing encoder.

Typed locals are salvaged through the wide (`bc67d207`), parametric
(`50be4d43`) and linear-local checker lanes, then adoption (`12aa121b`). The
latest experimental CI at `bba129e0` failed on the existing `prior:ptr`
annotation; its linear draft also loses branch-local ownership obligations.
Its successful earlier recovery run built an ARM64 engine under QEMU, not a
native Intel engine. Archive the experiment branches after these salvage
dots land; do not merge them.

## Earlier integration state

Updated 2026-09-18 01:20 by hazel. Integrated line head: the commit that
closes `habu-add-the-x86-56726659`; arm64 engine `bin/hb` sha256
`fa4980358806e3ca` (3,997,888 bytes). **All four arm64-side dots are closed;
the Intel agent starts at `habu-lower-hir-to-6bf80d33`.**

| Dot | Work | Runs on | State |
| --- | --- | --- | --- |
| `habu-write-the-x86-fbaf3086` | `src/arch/x86-64/asm.f` encoder + byte tests | arm64 host (hazel) | landed 07a7e90c |
| `habu-specify-the-engine-fcbcee25` | `src/habu/prims.f` primitive table + parity gate | arm64 host (hazel) | landed fb4f2392 + 383a18c7 (closed) |
| `habu-add-the-x86-56726659` | `src/os/linux-x86-64/` seam, ELF64, target contract, emitters | arm64 host (hazel) | landed d55021af + 5e05cbfd + d1961798 (closed) |
| `habu-parameterise-the-alloc-7efbe7a1` | register-file description for regalloc/spill/prune | arm64 host (hazel) | landed 50ee6a3c |
| `habu-bind-compiler-targets-ff970b99` | backend registry + pass dispatch | arm64 host (hazel) | landed 0901e61c + ddc1412d (closed) |
| `habu-lower-hir-to-6bf80d33` | `x64ir.f`, `select-x64.f`, `emit-x64.f` | arm64 host (hazel) | `x64ir.f` + backend row and selection slice A landed (see Landed); fusing, the allocator's fixed-register operands (`habu-place-the-fixed-3347ae15`), emission and the pass rows open |
| `habu-cross-build-the-d25a959d` | cross-build entry + device-peer gate | alder | open |
| `habu-port-the-ffi-676f745d` | SysV FFI, task entry, traps | alder | open |
| `habu-self-host-the-ccc31e78` | fixpoint on the Intel machine, release artifact | Intel agent | open |

Do not start the lowering dot until every row above it says landed; its
inputs (the operand types of the assembler, the primitive table, the
register-file description and the registry's registration form) are what it
compiles against, and the Landed section names them exactly.

## Where to start

1. `habu-lower-hir-to-6bf80d33`, then `habu-cross-build-the-d25a959d`, then
   `habu-port-the-ffi-676f745d`, then `habu-self-host-the-ccc31e78`. Each
   dot's text is the contract: `dot show <id>`, or read `.dots/<slug>.md`.
   Claim a dot before working it (`Claim: agent=<name> workspace=…`, status
   active) and close it only when it has landed with its gates green.
2. There is no x86_64 engine until the cross-build dot lands. `bin/hb` in
   this repository is an arm64 ELF. Until then every Habu program (tests,
   tools, `hb-build`, the compiler itself) runs on the arm64 host `neubau`,
   and the Intel machine is the device peer that runs a cross-built image and
   reports. `qemu-user` is not installed on neubau and has never been tried
   with `bin/hb`; do not plan on it.
3. So the lowering dot is written on either machine and tested on neubau:
   `bin/hb --load test/compiler/<suite>.f` there, against the arm64 engine,
   compiling for the x86_64 contract and pinning the emitted bytes. Only the
   cross-build dot puts an image on the Intel machine.
4. The peer gate follows the device-peer scripts under `test/` (serial,
   XMODEM, UDP: the only non-Habu files CLAUDE.md allows there). The x86_64
   peer is the same shape: neubau writes the image, ships it over ssh, runs
   `bin/hb --load test/run.f` on the peer and reads the result back.

## What the Intel machine needs

- `jj` and `git`, the repository cloned from `git@github.com:joelreymont/habu.git`
  (jj colocated or `jj git clone`), ssh keys accepted by neubau and by GitHub.
- `binutils` (`objdump`, `readelf`) and `llvm` (`llvm-mc`, `llvm-objdump`):
  `readelf -l` checks the cross-built ELF's program headers before anything
  runs; `llvm-mc -triple=x86_64 -show-encoding` is how the encoder tests'
  expected bytes were produced.
- `python3` for the peer scripts; the `dot` CLI (built from `~/Work/dots` with
  `zig build`). The dot CLI re-quotes `created-at` on `dot on`/`dot add`;
  normalise afterwards with
  `sed -i 's/^created-at: "\\"\(.*\)\\""$/created-at: "\1"/' .dots/*.md .dots/*/*.md`.
- ssh reachability both ways: neubau ships images to the Intel machine and
  the peer gate reports back.

## Ground rules

- VCS is `jj`, never git commands. One commit per dot with its dot update;
  50-character imperative subject, no attribution, no emoji.
- Read `docs/forth.md` before writing Habu: packages, small factored words,
  typed effects that keep meaningful types. No new `TRUSTED:` or unchecked
  seams; an OS or ABI boundary that must exist is explicit, small and tested.
- Only the compiler, JIT and REPL belong in the engine binary. The x86_64
  backend registers through the target registry when loaded; the arm64
  engine does not carry it and the x86_64 engine does not carry arm64.
- Test through the real load path; include rejected programs for type rules.
  Report actual results and untested boundaries.
- Baked modules (`lib/errors.f`, `lib/string.f`, `lib/fmt.f`,
  `src/habu/layout.f`, the `PRIM:` rows in `src/core/checker.f`) are
  answered by the host engine's baked copy until the engine is rebuilt; an
  edit there shows up only after a fixpoint rebuild.
- Landing: hazel reviews every change and runs the batch chain (two engine
  generations compared byte for byte, a third when they differ, then the
  full gate) before a commit reaches the integration line
  (`hazel/integration`). Push your work as a bookmark `intel/<dot-id>` to
  origin and tell Joel or hazel the bookmark name; do not move
  `hazel/integration` or `cedar/compiler-integration` yourself.
- Library public-surface changes are announced to the consumer repositories
  (loom, maki, kiba, radar, Tender) before they land.

## Design facts that must not drift

From `docs/x86-64.md`; the arm64 lanes are built on them.

- VM registers: `rbp` user area, `r12` data stack pointer, `r13` data base,
  `r14` dictionary, `r15` code pointer, `rbx` interpreter register. The rest
  are allocatable and caller-saved under the internal convention. The SysV
  callee-saved set (`rbx rbp r12-r15`) is exactly the VM set, so a foreign
  call preserves the VM with no save block.
- Calls between compiled words use the internal convention: stack cells in
  memory, live values in registers within a routine.
- `mov r64, imm64` is the one relocatable literal form: one patch site per
  literal, one new site kind in the relocation model.
- Guard pages and traps use the same mmap model as arm64; the signal frame
  decoding reads `RIP` and `RSP`.
- Recovery for x86_64 is "cross-build from a working arm64 engine"; the
  Gforth chain is not mirrored.

## Landed

### Assembler (`habu-write-the-x86-fbaf3086`, line 07a7e90c, engine 2afd2c72)

- `src/arch/x86-64/asm.f`, package `X64ASM`, 107 public `ENC-*` encoders.
  Operand nominals: `r64 r32 r16 r8 imm8 imm32 imm64 condition rel mem`;
  the sixteen `RAX`..`R15` words are `r64`, the sixteen `C-O`..`C-G` words
  are `condition`; `r32`/`r16`/`r8` are made with the generated `>R32` casts
  and `r8` 4..7 mean `spl bpl sil dil` (a REX is forced; the legacy high-byte
  registers do not exist).
- Memory operands are one packed cell built only by `MEM-AT ( r64 -- mem )`,
  `MEM-OFF ( r64 n -- mem )`, `MEM-IDX ( r64 r64 n n -- mem )` (base, index,
  scale 1/2/4/8, disp; index never rsp) and `MEM-RIP ( n -- mem )`; fields are
  re-screened inside every encoder.
- Every encoder takes a `lib/byte-buffer.f` `BUF` header as its LAST operand
  and appends bytes; there is no instruction value. Operand order: destination
  first, then sources; memory forms take the data register first (`-RR`,
  `-RM`, `-MR`, `-RI8`, `-RI32`, `-RI64` suffixes).
- Sizes are deterministic (the word chosen plus the displacement magnitude);
  size an instruction by encoding it into a scratch buffer. `rel` is measured
  from the END of the instruction; `ENC-JCC-REL8`/`ENC-JCC-REL32`,
  `ENC-JMP-REL8`/`ENC-JMP-REL32`, `ENC-CALL-REL32` are separate words.
- `ENC-MOV-RI64` is the one relocatable literal: 10 bytes, imm64 at
  `MOV-RI64-IMM-OFF` (2). The OS-seam lane names the relocation site kind
  `MOVABS` with that offset and width 8.
- Refusals: `E-X64ASM-OPERAND` (-8830), screened before any byte is emitted.
- Tests: `test/compiler/x86-64-asm.f` (152 byte-string cases from
  `llvm-mc -triple=x86_64 -show-encoding`, each with its llvm-mc line; 27
  runtime refusals; 11 checker refusals), `SUITE compiler-x86-64-asm`.
  Doc: `docs/embedded-encoders.md`, section x86_64.
- Deliberately absent until the selector asks: 32-bit ALU beyond
  `ENC-XOR32-RR`, 8/16-bit ALU, memory-destination immediates, `lock`
  prefixes, `nop`, the accumulator and `D1` short forms.
- Open decision for the lowering dot: `src/compiler/native/emit.f` is a
  fixed 4-byte word sink; the x86_64 emitter is either a byte-sized layout
  pass in emit.f or a separate emitter over `BUF` (see the lowering dot).

### Primitive table (`habu-specify-the-engine-fcbcee25` worker 1, line fb4f2392, engine 7f70b47e)

- `src/habu/prims.f`, package `PRIM-SPEC`, loads BEFORE `src/core/checker.f`
  as pure data: 222 rows (210 global, 3 `FFI` package rows, 7 `ELAB:`
  elaborated by the checker, 2 `UNROWED:` with no declaration). Row forms:
  `EPRIM: name atoms [REF word] EPRIM;`, `EPPRIM: pkg name atoms
  ECLOSE-PRIVATE`, `ETRUSTED-ONLY!` (flags the row before it), `ELAB: name`,
  `UNROWED: name`. Atoms are the checker's `PE-*` spellings encoding codes
  `A-IN 1 A-OUT 2 A-VAR-A..E 3..7 A-NUM 8 A-BOOL 9 A-REAL 10 A-U8 11 A-PTR 12
  A-RAW 13 A-QUOT 14 A-QUOT-END 15 A-FINALLY 16` (composites are sequences:
  `PE-PTR-A` = `A-VAR-A A-PTR`). checker.f replays the stream at its table
  position, so the effects exist only in prims.f (proved by a byte-identical
  textual round trip and a PES differential).
- Two gates close the fork: `habu1.f FP-ARGS` refuses a machine body whose
  name has no row; `habu2.f ENGINE-EMIT:PRIM-TABLE-COMPLETE` refuses a
  `KEEP?`-kept row that no section registered. **The x86_64 engine
  registers its bodies under exactly these names** and passes the same two
  gates; the table is the contract, the arm64 bodies in `habu1.f` are one
  implementation of it.
- Readers (with checker rows, callable from checked Habu): `COUNT`, `KIND@`,
  `NAME$`, `PKG$`, `REF$`, `TRUSTED-ONLY?`, `CODE-LEN@`, `CODE@`,
  `FIND ( ptr u8 n -- n )`, `K-PRIM K-PKG-PRIVATE K-ELAB K-UNROWED`.
- Docs: `docs/porting.md` "Engine Primitives".
- **The parity gate** (`test/prim-parity.f`, `SUITE prim-parity`, line
  383a18c7) is the file the x86_64 engine runs unchanged: 64 case sets over
  81 effect rows (416 assertions), each `CASES <name> … ;CASES` block a
  column of inputs and expected outputs (`3 4 7 NN-N`; shufflers get the
  sentinels 1 2 3 4 and expect the digit spelling of the window they leave,
  `swap` = 1243), reached through name-keyed dispatcher arms that are the
  only place a primitive is spelled. 46 rows carry `REF PRIM-REF:<X>`
  (`src/habu/prim-ref.f`, checked Habu, never baked, never using the
  primitive it stands for); the gate runs those beside the body. The 132
  rows with neither (I/O, syscalls, process, code publication, profiler,
  engine state, FFI) are printed by name every run. On x86_64 the first
  green run of this file is the primitive-parity acceptance of
  `docs/x86-64.md`.
- Known engine facts the parity lane measured and the x86_64 bodies must
  match or the dots must settle first: `/` and `mod` truncate toward zero;
  `lshift`/`rshift` mask the count to 6 bits, `rshift` is logical; division
  by zero crashes with a register dump (dot `habu-refuse-int-division-639af5fa`);
  `MIN-INT -1 /` wraps on arm64 and traps on x86 (dot
  `habu-define-min-int-50dc15ef`).

### Register file (`habu-parameterise-the-alloc-7efbe7a1`, line 50ee6a3c, engine 7f70b47e)

- `src/compiler/native/regfile.f`, package `NREGFILE`: `regs` (a one-cell
  set, bit i = register i) and the seven-field `file` record (`gpr-size`,
  `gpr-reserved`, `gpr-clobbered`, `fpr-size`, `fpr-reserved`,
  `fpr-clobbered`, `slot-width`); reserved is stored and allocatable
  derived, clobbered stored and callee-saved derived; one checked
  constructor `FILE ( n regs regs n regs regs n -- file )` refusing an
  incoherent description (`E-NREGFILE`); `REG-MAX` 63; builders
  `REGS-NONE`, `REGS-SET`, `REGS-REG`, `REGS-WITH`; readers `GPR-SIZE`,
  `GPR-RESERVED`, `GPR-ALLOCATABLE`, `GPR-CLOBBERED`, `GPR-CALLEE-SAVED`,
  the `FPR-` four, `SLOT-WIDTH`, `ALLOCATABLE-MASK`.
- The allocator takes the MACHINE with the dialect:
  `A64RA:BIND-DIALECT ( ctx builder NMACH:mach -- )`; nothing allocates
  without a description, and the register file is derived from it
  (`NMACH:REGFILE`). `A64IR:MACHINE` is the arm64 description
  (`src/arch/arm64/machine.f`, Darwin's x18 included); `regalloc-verify.f`
  reads `A64RA:MACHINE`. Frame rounding is the machine's
  (`NMACH:FRAME-ROUND`, `NMACH:FRAME-MAX`).
- **For x86_64:** the description for the design's machine is 16 general
  registers with `rbp r12 r13 r14 r15 rbx` reserved (ten allocatable), the
  SysV callee-saved set equal to the reserved set, so `clobbered` = the
  allocatable set; `test/compiler/native-regalloc.f` already drives the
  allocator with that shape. The pool type is target-neutral now
  (`NEFF:gprs` over an `NMACH:mach`, `src/arch/x86-64/machine.f` for this
  machine); what is NOT is `spill.f`/`prune.f`, which are instruction-form
  code over the A64 dialect, so the lowering dot supplies its own dialect
  before `A64RA` can be handed a real x86_64 pool.

### OS seam, ELF64, contract, site kind, emitters (`habu-add-the-x86-56726659`, lines d55021af + 5e05cbfd + d1961798, engine fa498035)

- Target name `linux-x86-64`, predicate `HB-TARGET-LINUX-X86-64?`; every
  `src/os/<target>/target.f` defines all three predicates and
  `HB-TARGET-KNOWN?` is closed over the three. Every selector in the tree
  has an explicit linux-x86-64 arm or a named refusal (`src/habu/prof.f`
  refuses at load: the signal frame is not modelled; `src/compiler/native/abi.f`
  answers `sysv-amd64`). Source-list owners (`tools/bootstrap.sh`,
  `tools/build-fixpoint.f`, `src/habu/habu2.f` + `bootstrap/cg/forth.fs`,
  `src/habu/stdin.f`, `tools/hb-build-lib.f`, `tools/lint/shadow-lint.f`,
  `tools/native-emit.f`) know the target; the recovery chain stays
  aarch64-only (x86_64 recovery is a cross-build, `docs/bootstrap.md`).
- `src/os/linux-x86-64/`: `target.f`, `layout.f` (`DATA-VA $340000000`,
  `DATA-SIZE $2000000`, `CODE-OFF $1000`, same guard-page model), `elf.f`
  (ELF64 `EM_X86_64`, `VMBASE $400000`, entry `$401000`, four phdrs:
  LOAD RX, LOAD RW `$C0`, INTERP `/lib64/ld-linux-x86-64.so.2`, DYNAMIC
  `$B0`; `R_X86_64_GLOB_DAT` for `dlopen`/`dlsym`; validated with
  `readelf`), `sys.f` (the x86_64 numbers, the *at* family as aarch64 uses;
  `SYS, ( n -- )` = `mov eax, NR / syscall / mov rcx, -4096 / cmp rcx, rax`
  so **CF set means error** and the x86_64 `SYS-PUSH` is a `setc`;
  `OS-OPEN-RD` (openat, `AT_FDCWD` in rdi), branchless `OS-OPEN-FLAGS` /
  `OS-MMAP-FLAGS` (test/cmov; they clobber rax, rcx, r11); the stencils
  `SYS-EMIT-WRITE/EXIT/SVC ( -- ptr u8 n )` as byte strings, consumed by
  `src/habu/jit.f C-EMIT-STENCIL`), `repl-term.f`, `sign.f`,
  `proc-watch.f` (`BPROCWATCHOPEN` = pidfd_open), `proc-control.f`
  (`BKILLERRNO`, `BEXECVE`, publishing rax). `G-POP`/`G-PUSH` in this seam
  take x86_64 register numbers: 7 rdi, 6 rsi, 2 rdx, 0 rax.
- **`ASM-SINK ( -- ptr u8 )`** is the seam's one forward reference: the
  byte buffer the current code stream appends into, referenced unrequired
  the way the aarch64 seam references `mnem.f`. The cross-build dot's x86-64
  code layer defines it. It cannot be a `lib/byte-buffer.f` BUF in the
  engine payload (BUF needs `lib/memory.f`, which needs the mmap primitives
  `habu1.f` defines, and the seam loads before `habu1.f`): give `X64ASM` an
  append seam the payload can satisfy, or reorder the payload.
- Contract: `CTARGET` arch `x86-64` (wire code 5), ABI `sysv-amd64` (code
  5), little-endian, 64-bit pointers, `MASK-X86-64` = BASE|FP|SIMD|FP16|
  BF16|ATOMIC (no AMX); the exhaustive `MATCH` arms in `ir/schema.f`,
  `attr.f`, `type.f`; `test/compiler/target-policy.f` domain 484 → 516.
- Relocation site kind `MOVABS` in `src/habu/aot-decl.f` (package
  `SNAP-RELOC`): 10 bytes, REX.W `B8+r` imm64, patch at offset 2 width 8;
  `MOVABS-SITE?` admits exactly `$48`/`$49` and `B8..BF`; `MOVABSV`,
  `SET-MOVABS`. `formal/Common/Reloc.v` models it with three theorems
  (`test/compiler/reloc-axioms.txt` rows). `SNAP-RELOC:MOVABS-IMM-OFF` and
  `X64ASM:MOV-RI64-IMM-OFF` both say 2, pinned equal by
  `test/x86-64-seam.f`.
- Tests: `test/x86-64-seam.f` (ELF header by field, MOVABS fixtures,
  contract rows), `test/x86-64-emit.f` (every emitted byte string pinned
  against `llvm-mc`, and the stencils against the encoders), suites
  `x86-64-seam`, `x86-64-emit`. Docs: `docs/porting.md` (seam row,
  syscall paragraph), `docs/bootstrap.md` (recovery rule).
- Residuals for the Intel dots: `src/habu/habu1.f` (25 two-arm
  `HB-TARGET-LINUX?` forms, the arm64 primitive bodies) and
  `bootstrap/cg/*.fs` → cross-build dot; `src/habu/crash.f` frames and the
  `lib/` platform gates (`pq`, `evp`, `serial`, `net/*`, `task`, `fs`,
  `process`, `genio`, `codesign`, `pty`) refuse an x86_64 host until the
  FFI is SysV → FFI dot; `tools/engine-size.f` and `tools/imgdump.f` read
  only `EM_AARCH64` images. **No x86_64 instruction has executed anywhere;
  the carry polarity is argued and byte-pinned, not observed.**

### Target registry, rows (`habu-bind-compiler-targets-ff970b99` worker 1, line 0901e61c, engine 343ef770)

- Package `CTARGET` (`src/compiler/target.f`) gained the backend registry:
  `BACKEND-ROWS` (4), `REGISTER ( arch [ contract -- bool ] [ contract -- bool ] -- )`
  (lowering acceptance, emission acceptance; a second claim of one arch is
  `E-CTGT-REGISTERED`, a full table `E-CTGT-ROW`), `REGISTERED? ( arch -- bool )`,
  `ROW ( arch -- n )` (throws `E-CTGT-UNLOADED`), `LOWERS?` and
  `EMITS? ( contract -- bool )`. Rows are keyed by `ARCH-CODE`, because the
  native-build window's dialect cannot fetch a nominal ENUM from a
  `TYPED-BUFFER` (dot `habu-model-a-nominal-05d89f50`); quotation buffers work.
- `src/arch/arm64/backend.f` (package `A64BACK`) registers aarch64 at load
  with `SERVES?` = aarch64 + little + bits64 (the ABI is `abi.f`'s answer);
  it is required by `a64ir.f` and `emit.f`, so the row exists exactly when
  arm64 backend code is loaded. `A64IR:CHECK-TARGET` and `A64EMIT:TARGET-CK`
  resolve through the registry: an arch with no module is `E-CTGT-UNLOADED`,
  a loaded backend that declines the machine keeps its own refusal.
- The x86_64 backend does the same: `src/arch/x86-64/backend.f` requiring
  only `src/compiler/target.f`, registering the `x86-64` arch variant (which
  the OS-seam lane adds to the contract tables) with its two predicates; its
  `x64ir.f`/`emit-x64.f` require that file.
- Tests: `test/compiler/target-registry.f` (`SUITE compiler-target-registry`);
  `backend-boundary.f` now expects `E-CTGT-UNLOADED` for A32/THUMB2/C66X.
- There is no `--target` flag: the registry is the single resolution point.

### Target registry, pass dispatch (worker 2, line ddc1412d, chain AP)

- `src/compiler/native/backend.f`, package `NBACK`, above `ir/build.f`,
  nothing arm64 in its requires: ten `CTARGET:BACKEND-ROWS TYPED-BUFFER`
  rows indexed by `<arch> CTARGET:ROW`, one dispatch word and one `!`
  installer per row, refusing defaults (`E-CTGT-UNLOADED`) for the
  per-definition stages and no-ops for the lifecycle rows. The one-field
  nominal `NBACK:linkage` (`L-NONE L-DEAD L-CALLED L-TAIL L-BACK`, `WITH`,
  `HAS?`) carries how control reaches and leaves a routine, because a
  multi-field value cannot be bound to a local (docs/type-system.md §10.2).
- `src/compiler/native/compiler.f` names no backend package: `EMITTED` is
  `NBACK:DECLARE → SELECT → PRUNE → FIXPOINT → EMIT`, retire uses
  `RELEASE`/`RETIRE`, the session `PROTOTYPE`/`FORGET`, capture `PREPARE`.
  `NABI:BINDING` remains the one target-naming word (which machine this
  engine compiles for; where a host binding for x86_64 lands).
- `src/arch/arm64/passes.f`, package `A64PASS`, installs the arm64 rows at
  load (required by `compiler.f`); it wraps the A64* passes without moving
  their state and passes `A64IR:REGFILE` to `A64RA:BIND-DIALECT`.
- **What the x86_64 lowering dot provides, in require order:**
  1. `src/arch/x86-64/backend.f` (package `X64BACK`): requires only
     `lib/prelude.f` and `src/compiler/target.f`; defines
     `SERVES? ( CTARGET:contract -- bool )` and ends with
     `CTARGET-ARCH:X86-64 [: SERVES? ;] [: SERVES? ;] CTARGET:REGISTER`.
     The `x86-64` arch variant (wire code 5) and `sysv-amd64` ABI are in
     `target.f` from the OS-seam lane.
  2. `x64ir.f` and the x86_64 emitter require that file, so the row exists
     whenever any backend code is loaded.
  3. `src/arch/x86-64/passes.f` (package `X64PASS`): requires
     `src/compiler/native/backend.f`, `src/arch/x86-64/backend.f` and the
     pass modules it wraps; ends with `X64PASS:INSTALL`, ten calls, each
     `arch quotation NBACK:<STAGE>!`; an installer throws
     `E-CTGT-UNLOADED` if the arch never registered, so step 1 precedes it.

  | installer | quotation effect | obligation |
  | --- | --- | --- |
  | `NBACK:DECLARE!` | `[ n n NBACK:linkage -- ]` | record in-cells, out-cells, linkage; reset per-definition state |
  | `NBACK:SELECT!` | `[ IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module ]` | HIR builder in, machine module out |
  | `NBACK:PRUNE!` | `[ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]` | may hand the module back untouched |
  | `NBACK:FIXPOINT!` | `[ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]` | lower until the allocation seals empty |
  | `NBACK:EMIT!` | `[ IR-CTX:ctx IR-BUILD:module n -- ]` | write at the code slot the driver passes (`NPUB:NEXT-SLOT`) |
  | `NBACK:RELEASE!` | `[ -- ]` | give back every pass binding still held (failure path) |
  | `NBACK:RETIRE!` | `[ -- ]` | end-of-definition emitter reset (success and failure) |
  | `NBACK:PROTOTYPE!` | `[ IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ]` | intern the dialect vocabulary into the session prototype |
  | `NBACK:FORGET!` | `[ -- ]` | clear that prototype flag at session stand-down |
  | `NBACK:PREPARE!` | `[ -- ]` | scrub code buffers and pass scratch before an image capture |

  Beyond the rows the backend composes its own `ROUTINE` equivalent from
  `NBACK:linkage`, the declared arity and its own function/spill readings.
  No edit to `compiler.f` is needed to reach a second backend. The
  register allocator's pool type is target-neutral now (`NEFF:gprs` over the
  machine a contract carries), so the x86_64 backend can hand `A64RA` a real
  pool once it lowers into its own dialect.
- Tests: `test/compiler/target-registry.f` property 5 (a fake PTX backend
  with pass rows, dispatch through `DECLARE`/`RELEASE`/`RETIRE`, an
  unregistered arch refused, a registered-but-passless arch refused through
  the default row).

### Machine dialect and backend row (`habu-lower-hir-to-6bf80d33`, first slice)

- `src/arch/x86-64/backend.f` (`X64BACK`): the registry row, `SERVES?` =
  x86-64 + little-endian + 64-bit, installed at load; requires only the
  registry, nothing under `src/os/`, so no engine prefix grows a compiler
  dependency.
- `src/compiler/native/x64ir.f` (`X64IR`, dialect `x64` 0.1): 46 opcodes, 12
  attribute keys, six signed conditions taken from `X64ASM:C-L/LE/G/GE/E/NE`.
  Two-address forms declare a schema tie on the operand they overwrite; every
  flags user is fused (`cmpset`, `cmpseti`, `cmpsel`, `selz`, `cmpbr`,
  `cmpbri`, `brz`); the literal is `mov r64, imm64` (site kind `MOVABS`); no
  `movk`, `linksave`/`linkload` or `dpush`/`dpop`. No float forms yet (they
  raise MINOR when they land). Two fixed-register obligations are stated in
  the file for the selector and emitter: `shl`/`shr` count in `rcx`, `idiv`
  over `rdx:rax` (quotient `rax`, remainder `rdx`); `idiv` also traps on
  `MIN-N / -1`, which the selector must answer as `(MIN-N, 0)` by contract.
- `rsp` is reserved (call pushes through it): seven reserved, nine allocatable
  (`rax rcx rdx rsi rdi r8..r11`). `X64IR:REGFILE` states it;
  `test/compiler/x64ir.f` asserts it.
- The emitter's sink is decided in `docs/x86-64.md`: a separate
  `src/compiler/native/emit-x64.f` over the `BUF` byte sink, layout in bytes,
  branches laid out as `rel32`; `emit.f` stays the ARM64 word sink.
- `src/compiler/ir/context.f` decodes wire code 5 (`x86-64`, `sysv-amd64`);
  before this the first x86-64 context threw `E-IR-CTX-STATE`.
- Errors `-8740..-8759` (`X64IR`); `-8760..-8779` and `-8780..-8799` reserved
  for `X64SEL` and `X64EMIT`.
- Still open in the dot: the fusing slice of `select-x64.f`, `emit-x64.f`,
  `src/arch/x86-64/passes.f`, the pinned-bytes suite
  `test/compiler/x64-emit.f`.
- Decision (hazel, 2026-09-18), LANDED: the allocator's effect schema is
  GENERALISED, not duplicated. `src/compiler/a64-effect.f` (`A64EFF`) is now
  `src/compiler/native-effect.f` (`NEFF`), and the ARM64 facts it used to hold
  as constants are a MACHINE DESCRIPTION the backend supplies:
  `src/compiler/native/machine.f` (`NMACH`, a nominal one-cell `mach` over a
  deduplicated private table - register files, link set, sp operand and
  alignment, frame bound, offset limit and how the offset field is counted,
  access widths, slot reach back) with `src/arch/arm64/machine.f` (`A64M`) and
  `src/arch/x86-64/machine.f` (`X64M`) as its two instances. A contract
  carries its machine as a fourteenth field, `ROUTINE` is where a register is
  held to `NMACH:GPR-ALLOCATABLE`, and register sets and place lists are
  machine-free values. The `link` field gained `absent`, which is the only
  answer a machine with no link register may give - x86-64's contracts use it
  (`test/compiler/x64ir.f`). Digest SCHEMA 4 -> 5, SLOTS 15 -> 16
  (`SLOT-MACH` = `NMACH:MARK`). The arm64 engine is byte-identical across the
  generation chain (gen2 == gen3; gen1 differs in move-wide immediates and
  data pointers only).

### Selection, slice A (`habu-lower-hir-to-6bf80d33`, second slice)

- `src/compiler/native/select-x64.f` (package `X64SEL`): HIR -> X64IR for the
  straight-line and branching core, a separate pass with A64SEL's shape
  (`BIND-SOURCE` / `RELEASE` / `SELECT ( ctx module builder routine -- module )`).
  CORRECT AND UNFUSED: a compare feeding a branch is `x64.cmpset` then
  `x64.brz`; `cmpbr`/`cmpbri`/`cmpsel`/`selz` are the fusing slice. The table:
  `const`->`movi` (with `x64.addr`), `quot`->`codeaddr`, `+ - and or xor`->
  the tied form or its `*i` immediate form when the right operand is a literal
  the imm32 admits (the literal's own `movi` is dropped only when EVERY use
  folds, counted over the function), `*`->`imul`, literal-count shifts->
  `shli`/`shri`, `invert`->`not`, the six signed comparisons->`cmpset`/
  `cmpseti`, `@ ! c@ c!`->`aload/astore/abload/abstore`, `br`/`brz` with edge
  splitting through `x64.mov`, the data-stack boundary (`dtake`, `dload` per
  argument, `dstore` per result, `dpublish`; the pointer stands at the entry
  base, or never moves in a routine that leaves through its callee, so a tail
  call reads its arguments at negative displacements), `call`/`wordcall`/
  `tailcall`, `trap`, `reserve`/`release`. No copy is ever inserted for a
  two-address tie: the allocator owns that.
- Refused by name, each with a case: `E-X64SEL-FLOAT` (no SSE form declared),
  `E-X64SEL-TRAP` (trapping-overflow unit), `E-X64SEL-FIXED` (a shift whose
  count is not a literal reads rcx; a divide reads rdx:rax - the allocator has
  no fixed-register operand yet, `habu-place-the-fixed-3347ae15`),
  `E-X64SEL-MACHINE` (a contract of another machine), `E-X64SEL-OPCODE`,
  `-BIND`, `-SOURCE`, `-MEM`; `-8760..-8774` in lib/errors.f. HIR has no
  `negate`, so `neg` has no source form in this slice.
- `src/arch/x86-64/abi.f` (`X64ABI`): the Habu word convention on this
  machine - link `absent`, no prologue slot (the return address is on the
  machine stack), the nine-register pool from `X64M:MACHINE NEFF:GPR-ALL`,
  frames holding spills alone.
- `src/compiler/ir/fun.f` `TARGET-CK`: a calling convention belongs to a KIND
  of architecture (kernel <-> PTX, everything else native), not to AArch64 by
  name; that is what lets an x86-64 module verify at all.
- Suite `test/compiler/x64-select.f` (`compiler-x64-select`): opcode per
  source operation and value identity per operand, the operation COUNT for the
  immediate forms, both successors of the unfused branch, the negative
  displacement of the tail case, the ABI contracts, and every refusal above.
- Untested: `trap` has a rule and no positive case (no selection fixture
  builds a target dictionary with `die`, ARM64's suite has the same gap);
  edge splitting is covered for one carried forward edge, not the permuting
  back edge; the seven internal shape refusals (`-SHAPE`, `-ATTR`, `-CAP`,
  `-PLACE`, `-CALL`, `-TAIL`, `-ORDER`) have no case.
