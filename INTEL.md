# INTEL.md — the x86_64 backend from the Intel machine

For the agent that continues campaign C6 (`habu-campaign-c6-targets-86bb56bb`)
on the Intel (x86_64 Linux) machine. Read `docs/x86-64.md` (the design) and
`docs/porting.md` (the seam rules) first; this file is the state and the entry
point. Hazel, who runs Habu on the arm64 host, keeps the State section current
and rewrites the Landed section as each arm64 lane lands.

## State

Updated 2026-09-17 by hazel. Line head `dd3f5044`; arm64 engine `bin/hb`
sha256 `c28d1c9314aefbd5` (3,932,352 bytes).

| Dot | Work | Runs on | State |
| --- | --- | --- | --- |
| `habu-write-the-x86-fbaf3086` | `src/arch/x86-64/asm.f` encoder + byte tests | arm64 host (hazel) | in progress |
| `habu-specify-the-engine-fcbcee25` | `src/habu/prims.f` primitive table + parity gate | arm64 host (hazel) | in progress |
| `habu-add-the-x86-56726659` | `src/os/linux-x86-64/` seam, ELF64, target contract | arm64 host (hazel) | in progress |
| `habu-parameterise-the-alloc-7efbe7a1` | register-file description for regalloc/spill/prune | arm64 host (hazel) | in progress |
| `habu-bind-compiler-targets-ff970b99` | backend registry in `src/compiler/target.f` | arm64 host (hazel) | in progress |
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

Nothing yet. Hazel fills this section per lane with the package names, the
entry points and operand types the lowering compiles against, the tests that
pin them, and the sha256 of the arm64 engine that carries them.
