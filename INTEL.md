# INTEL.md — the Linux x86-64 backend

The entry point for the lane that finishes the Linux x86-64 backend. The
design reference is [docs/x86-64.md](docs/x86-64.md). The seam rules are in
[docs/porting.md](docs/porting.md). The dot tree starts at
`habu-finish-the-linux-57cb3952` (`dot show habu-finish-the-linux-57cb3952`),
under the umbrella `habu-campaign-c6-targets-86bb56bb`.

## Mission

The goal is a tier-1-only Linux x86-64 engine:

- **Kernel.** Each target has a small hand-written kernel that binds the
  names in `src/habu/prims.f`.
- **Interpreter in Habu.** The outer interpreter, the definers, packages,
  source loading and a `MAIN` entry move out of ARM64 assembly into checked
  Habu. They are captured into both the ARM64 and the x86-64 product. The
  seeded boot enters them through the DATA cell `ENGINE-MAIN:XT-CELL`, so
  `habu2.f` and `jit.f` get no x86 twin.
- **Dual-emission cross-build.** spark cross-builds the first x86 engine by
  dual emission: each definition's single HIR is lowered and emitted twice,
  ARM64 into the live region and x86 into a shadow keyed by record, and the
  capture carries the shadow.
- **Write-time link.** A host-side Habu linker writes the x86 image with the
  code region and DATA as fixed `PT_LOAD`s.
- **Self-host.** The cross-built engine rebuilds itself on the ThinkPad to a
  byte fixpoint, which is the release artefact. A native build records the
  already published Intel emission; it does not lower or emit it a second time.

The ARM64 product keeps tier 0 through the B1 hook
(`habu-hook-tier-0-96e33c29`). A tier-1-only product on both architectures
is a follow-on, opened once tier-1 latency is measured on spark
(`habu-measure-tier-1-f7425ab1`, G4a) and on the ThinkPad
(`habu-measure-tier-1-0faadb01`, G4b).

## State

Landed on `master` (the facts are in [docs/x86-64.md](docs/x86-64.md)):

- the assembler (`X64ASM`);
- the primitive table and its parity gate;
- the OS seam, the ELF64 writer and the `MOVABS` site kind;
- the register-file description;
- the backend registry and pass rows;
- `x64ir.f`, `select-x64.f`, the partial `emit-x64.f` and `X64PASS`.

The implementation lane also carries checked Habu startup and interpretation,
the fixed-segment image linker, recorded anonymous-function targets, SysV FFI,
Linux runtime facts, and native capture. Emitted Intel executables exercise
these boundaries on the ThinkPad; ARM products are built and tested on spark.

A complete cross-built engine enters captured `MAIN`, evaluates `cr` from
stdin, and reports a missing load file with exit 74. Completion still requires
the remaining kernel interpreter boundaries, full native suites, saved images
and snapshots, and native byte convergence. A successful cross-build alone
does not establish those outcomes.

## Hosts

- **ThinkPad** (x86-64 Arch Linux) is the lead's machine. It runs cross-built
  x86 images until `habu-run-bin-hb-6378f297` (X6) lands. After that it
  builds and gates natively.
- **spark** (aarch64 Ubuntu 24.04) builds, cross-builds and runs the ARM64 gate.
  Use an isolated source copy and build directory under `~/.cache/habu/`;
  `~/Work/habu/krait` belongs to other active work. Reach it
  with `ssh -o BatchMode=yes spark`. Every command there that
  loads libzip needs `LD_LIBRARY_PATH=$HOME/.local/opt/libzip/lib` (zlib 1.3.1
  and libzip 1.11.4 are built into `~/.local/opt/libzip`).
- **No CI.** macOS is untested by this lane. Alder pulls `master` and fixes
  macOS. Every shared-file landing keeps the macOS arms correct by
  construction and reports macOS as untested.

## Gate commands

On spark, from a tree whose `bin/hb` is the candidate
([docs/gate.md](docs/gate.md)):

```sh
bin/hb --load test/run.f                                          # gate
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  $HOST --load tools/native-build.f -- $OUT                       # rebuild
HB_TMP=$PWD/build/tmp bin/hb --load tools/two-generation-build.f -- <seed>  # chain
```

If an engine is too old to build current `master`, recover through Gforth
([docs/bootstrap.md](docs/bootstrap.md)) and rebuild from that:

```sh
HABU_ALLOW_BOOTSTRAP=1 HABU_TARGET=linux-aarch64 HB_TMP=$TMP \
  GFORTH=$(command -v gforth) tools/bootstrap.sh
```

Cross-build from a working ARM64 engine whose baked layout matches the source:

```sh
bin/hb --load src/arch/x86-64/passes.f tools/native-build.f -- <out> --target linux-x86-64
```

Copy the output and its matching `.names` sidecar to the ThinkPad. Run the
Intel native gate with that candidate installed in the isolated tree's
`bin/hb`, and rebuild there through `tools/native-build.f`.

## Ownership and landing

Follow [AGENTS.md](AGENTS.md): use isolated jj workspaces, preserve unrelated
work, independently review each significant feature, run the required gates,
and push `master`. Shared-file changes preserve the macOS arms; this lane
reports macOS as untested. Retire integrated task bookmarks and workspaces only
after verifying their changes are accounted for. Existing campaign dots close
when their stated acceptance is met.

## Design facts that must not drift

- VM registers: `rbp` is DATA, `r12` the data stack pointer, `r13`
  the dictionary base, `r14` its record count, `r15` the code pointer and `rbx` the
  interpreter register. `rsp` is reserved too, and nine registers are
  allocatable. The SysV callee-saved set is exactly the VM set, so a foreign
  call needs no save block.
- `mov r64, imm64` is the one relocatable literal, site kind `MOVABS`: 10
  bytes, with the imm64 at offset 2.
- Guard pages and traps use the ARM64 mmap model. The signal frame decoding
  reads `RIP` and `RSP`.
- x86 recovery is a cross-build from a working ARM64 engine. The Gforth chain
  emits ARM64 only and refuses x86 by name.
- x86 has no cold route: `tools/native-build.f` is its only build route.
- Every code-bearing record comes from the compiler. No assembly chain is
  decoded.
- The capture reads recorded sites. It does not decode instructions.
- On x86 the code region and DATA are fixed `PT_LOAD`s. No relocation runs at
  boot or at snapshot restore.
- `MAIN` is a DATA cell the kernel calls (`ENGINE-MAIN:XT-CELL`).

## Earlier work

The 2026-09 experiment branches and the earlier multi-agent integration line
are superseded and archived. What survived is on `master` and is described in
[docs/x86-64.md](docs/x86-64.md).
