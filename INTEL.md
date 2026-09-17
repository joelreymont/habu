# INTEL.md — the x86_64 backend from the Intel machine

For the agent that continues campaign C6 (`habu-campaign-c6-targets-86bb56bb`)
on the Intel (x86_64 Linux) machine. Read `docs/x86-64.md` (the design) and
`docs/porting.md` (the seam rules) first; this file is the state and the entry
point. Hazel, who runs Habu on the arm64 host, keeps the State section current
and rewrites the Landed section as each arm64 lane lands.

## State

Updated 2026-09-17 19:20 by hazel. Integrated line head `1a3cba18`; arm64
engine `bin/hb` sha256 `343ef7705f0ea45a` (3,932,352 bytes).

| Dot | Work | Runs on | State |
| --- | --- | --- | --- |
| `habu-write-the-x86-fbaf3086` | `src/arch/x86-64/asm.f` encoder + byte tests | arm64 host (hazel) | landed 07a7e90c |
| `habu-specify-the-engine-fcbcee25` | `src/habu/prims.f` primitive table + parity gate | arm64 host (hazel) | in progress |
| `habu-add-the-x86-56726659` | `src/os/linux-x86-64/` seam, ELF64, target contract | arm64 host (hazel) | in progress |
| `habu-parameterise-the-alloc-7efbe7a1` | register-file description for regalloc/spill/prune | arm64 host (hazel) | in progress |
| `habu-bind-compiler-targets-ff970b99` | backend registry in `src/compiler/target.f` | arm64 host (hazel) | half landed 0901e61c (registry rows); pass dispatch in progress |
| `habu-lower-hir-to-6bf80d33` | `x64ir.f`, `select-x64.f`, `emit-x64.f` | Intel agent | open; depends on the five above |
| `habu-cross-build-the-d25a959d` | cross-build entry + device-peer gate | Intel agent | open |
| `habu-port-the-ffi-676f745d` | SysV FFI, task entry, traps | Intel agent | open |
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
- Worker 2 (in progress) adds the pass rows above `ir/build.f` (`NBACK`,
  ten typed quotation rows indexed by `<arch> CTARGET:ROW`, refusing
  defaults) so `compiler.f` names no backend; its report will list the exact
  registration sequence a second backend performs. There is no `--target`
  flag: the registry is the single resolution point.
