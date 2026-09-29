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
  byte fixpoint, which is the release artefact.

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

Executed natively on the ThinkPad: `test/x86-64-peer-image.f`, run on spark,
writes `hb-x64-peer` and `hb-x64-peer-negative`. They exit 0 and 21. They are
the only x86 code that has run so far.

The spark baseline at `afd626da` is 488/491:

- `zip` and `native-resource-image` fail only because Ubuntu ships no
  `libzip.so.5`. This is host setup outside the repository (below).
- `fs-mutate` is a real defect: a C `int` result is not sign-extended, so
  `-1` reads as `0xFFFFFFFF`. The fix is `habu-sign-extend-c-c7f55f0e`.

## Hosts

- **ThinkPad** (x86-64 Arch Linux) is the lead's machine. It runs cross-built
  x86 images until `habu-run-bin-hb-6378f297` (X6) lands. After that it
  builds and gates natively.
- **spark** (aarch64 Ubuntu 24.04, 20 cores; the clone is
  `~/Work/habu/krait`) builds, cross-builds and runs the ARM64 gate. Reach it
  with `ssh -o BatchMode=yes spark bash -lc '…'`. Every command there that
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

On the ThinkPad, until X6 lands: copy the images spark writes (`scp`) and run
them, comparing exit statuses. After X6: `bin/hb --load test/run.f`
natively.

## Ownership and landing

- Every x86 dot belongs to this lane (`Ownership: krait (Intel lane)`),
  including `habu-cross-build-the-d25a959d`.
- `habu-make-build-fixpoint-eeaf6c00` stays Alder's. It is ARM64 recovery
  work, and x86 has no cold route.
- Each leaf's `Route:` field says how it lands:
  - **`Route: direct`**: every file the leaf changes is x86-only
    (`src/arch/x86-64/`, `src/os/linux-x86-64/`,
    `src/compiler/native/{x64ir,select-x64,emit-x64}.f`, new x86-only files,
    `test/x86-64-*`, `test/compiler/x64-*`, `docs/x86-64.md`, `INTEL.md`,
    `.dots/`). The lane lands it on `master` after the Linux gates.
  - **`Route: Alder`**: anything a macOS build loads. The lane pushes the
    leaf as bookmark `intel/<dot-id>` to `origin`, rebased on
    `master@origin`, reviewed and Linux-gated. It lists the bookmark, commit,
    base, paths and Linux results in its receipt
    (`~/.cache/tmp/habu-intel-krait-current.md` on the ThinkPad), which Alder
    pulls. Alder gates macOS and moves `master`. The lane never moves
    `master` for these leaves.
- A dot closes after its commit is on `master`.

Workflow rules from [CLAUDE.md](CLAUDE.md):

- one commit per leaf, made in its own `.jj-ws/<dot-id>` workspace created
  from the repository root;
- an independent review of every delegated change before it lands;
- the gate and the push are never one command chain;
- no shell or Python logic under `tools/` or `test/`: the x86 peer runs
  images, and Habu programs do the checking.

## Design facts that must not drift

- VM registers: `rbp` is the user area, `r12` the data stack pointer, `r13`
  the data base, `r14` the dictionary, `r15` the code pointer and `rbx` the
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
