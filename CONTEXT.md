# Handoff Context

Date: 2026-06-26

This file is the committed pickup point. The local `.dots/` store is ignored, so
this file is authoritative for a fresh checkout.

## Current State

- Repository: `/Users/joel/Work/habu` locally; `/home/user/Work/habu` on zed
- VCS: `jj`
- Remote branch: `origin/master`
- Expected local state before continuing: clean working copy on top of `master`
- Current focus: PTX/CUDA work on zed
- Only ready dot in the local dot store:
  `habu-m1a-dynamic-linux-1ff8d288`

## What Is Done

- LLM benchmark infrastructure is retired. The old cross-language and scorecard
  dots were closed as not planned; current work is PTX/CUDA.
- `src/arch/ptx/emit.f` emits a minimal SAXPY PTX kernel.
- `tools/ptx/saxpy.f` uses the shared emitter.
- `tools/ptx/saxpy-test.f` validates emitted PTX text shape locally.
- `tools/ptx/ptxas-smoke.f` assembles the PTX with `ptxas` on zed.
- `docs/ptx.md`, `docs/ptx-sketch.md`, `FILEMAP.md`, and the gate map know
  about the PTX emitter/smoke files.
- `bin/hb` was rebuilt and the full native gate passed locally after the PTX
  emitter work.
- Zed validation already passed for emit+assemble:
  - `saxpy-test: ok`
  - `ptxas-smoke: ok`

## Restart Point

Start on zed at:

```sh
cd /home/user/Work/habu
```

Sync the checkout to `origin/master`, verify `bin/hb` exists, then continue the
single ready dot:

```sh
dot show habu-m1a-dynamic-linux-1ff8d288
```

The work is M1a: make the Linux image dynamic enough to import `dlopen` and
`dlsym`, without breaking the self-host snapshot path.

## M1a Requirements

Implement this in typed Habu wherever the checker can express it. Any unchecked
code must stay as a thin, named, tested boundary with `TRUST` signatures only for
the words crossed by checked code.

Target file:

- `src/os/linux/elf.f`

Both Linux image paths must become dynamic:

1. `BUILD-ELF` / `BUILD-IMAGE`
2. `BUILD-SNAP-HDR` in `src/habu/snap.f`

The dynamic ELF plan already proved out on zed as a tiny native experiment:

- `PT_INTERP=/lib/ld-linux-aarch64.so.1`
- `DT_NEEDED=libc.so.6`
- `R_AARCH64_GLOB_DAT` relocations for `dlopen` and `dlsym`
- `DT_FLAGS=DF_BIND_NOW`
- fixed RW segment at `VMBASE + 0x100000`
- fixed GOT slots for checked code to read after loader relocation

Do not stop after an AOT/maker-only image. The self-host refresh path is the
hard requirement.

## Acceptance

On zed, in order:

1. Emit a dynamic Linux ELF and prove it structurally with `readelf -hl`.
2. Prove ld.so loads and runs it.
3. Rebuild through the fixpoint install path and prove the produced `bin/hb`
   still self-hosts.
4. Prove `DLOPEN-SLOT @` and `DLSYM-SLOT @` are non-zero after startup.
5. Use the existing FFI call path to `dlopen` Tegra `libcuda.so.1`, `dlsym`
   `cuInit`, and call `cuInit(0)`.
6. Only after M1a passes, continue the blocked M1b/M1c/M1d CUDA harness dots.

## Known Commands

Local PTX tests:

```sh
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f \
  lib/process-argv.f lib/process-env.f src/arch/ptx/emit.f \
  tools/ptx/saxpy-test.f

bin/hb --load lib/errors.f lib/test.f lib/ptx.f lib/ptx-test.f
```

Native gate:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-pool.f \
  test/run.f
```

If `bin/hb` is missing, recover only through:

```sh
HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh
```
