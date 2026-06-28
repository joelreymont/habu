# habu — Checked Forth

Habu is a self-hosted, checked Forth for macOS ARM64 and Linux AArch64.
`bin/hb` is the native engine: it type-checks Forth stack effects, JIT-compiles
words to ARM64 code, rebuilds itself to a byte-for-byte fixpoint, and can AOT
build standalone binaries.

Core pieces:

- **Typechecker** — row-polymorphic stack-effect checking for data and return
  stacks. Checked definitions fail before runtime unless the inferred body effect
  unifies with the declared `( in -- out )` signature.
- **JIT** — the native engine compiles Forth words directly to ARM64 machine
  code; accepted checked code has no runtime type tags or GC.
- **AOT** — `tools/hb-build.f` emits signed standalone binaries for the host
  target.
- **Tree shaker** — AOT builds keep only code reachable from `MAIN` or top-level
  roots, so generated binaries do not carry the whole interactive engine.

```forth
: SQUARE ( i64 -- i64 ) dup * ;   \ accepted
: BAD    ( i64 -- i64 ) dup ;     \ rejected: leaves an extra i64
```

## Quick Start

If `bin/hb` is missing, recover it with Gforth once:

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth-fast tools/bootstrap.sh
```

After `bin/hb` exists, normal work is Habu-native:

```sh
bin/hb                                  # checked REPL, stepper, debugger
echo ': SQ ( i64 -- i64 ) dup * ; 7 SQ .' | bin/hb
bin/hb script.f arg...
```

Refresh the self-hosted engine:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
```

Build an AOT binary:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/source.f lib/build.f \
  lib/codesign.f lib/content-key.f tools/build-fixpoint.f tools/warm-run.f \
  tools/hb-build-lib.f tools/hb-build.f -- prog.f -o prog
```

Run the native gate:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f \
  test/gate-pool.f \
  test/run.f
```

## Checked Forth

Checked definitions use ordinary Forth plus a typed stack comment. The checker
supports concrete types (`i64`, `u8`, `u32`, `cell`, `bool`, `char`, `str`,
`addr`), nominal roles (`idx`, `len`, `fd`, `reg`, `label`, `va`, `symidx`,
`asm`, `img`, `snap`), `ptr a`, row variables, quotations, return-stack effects,
recursion, loops, control flow, locals, and `CREATE ... DOES>`.

Words that cross compiler/runtime boundaries are explicit `TRUSTED:` or `TRUST`
sites and are tracked in [`TRUSTED.md`](TRUSTED.md). New Forth should be checked
unless the boundary is deliberately documented and tested.

## Maki — an ML framework on a checked GPU kernel DSL

[`maki/`](maki/README.md) is an ML framework layered on Habu and a **checked PTX
kernel backend**. The thesis: *checked kernels + checked AD transforms* are a better
target for LLM-authored ML — a GPU kernel DSL whose type system shifts stack,
address-space, and mask/extent discipline bugs to author-time diagnostics. Fresh
per-call extent/mask identity is still an open checker soundness item, tracked in
dot `habu-add-per-call`.

- **Habu-PTX kernel DSL** (`lib/ptx/`) — `tile<T,B,M>`/`span`/`matrix` parametric
  types; checked `KERNEL:` definitions (SAXPY, numerically-stable SOFTMAX-ROWS) that
  emit PTX, assemble with `ptxas -arch=sm_87`, and **run correct-vs-golden on the
  NVIDIA Orin GPU** via a Habu FFI to the CUDA Driver API.
- **Reverse-mode autograd v0** (`lib/ptx/ad.f`) — AD as a syntactic reversal of
  the concatenative IR (no runtime tape); the VJP table/reverse pass and checked
  backward fixtures exist. Device finite-difference gradcheck remains the hard
  gate before claiming derivative correctness for generated PTX gradients.
- **Maki framework** (`maki/`) — tensor/array types, autograd orchestration,
  optimizers (SGD family) + losses, ONNX op import, and a training loop that
  converges at tensor scale; plus the LLM-target **eval harness** (the checker as
  the correctness judge for candidate kernels). Strictly one-way dependency
  (`maki → habu`), fenced out of the trust root (not in `TRUSTED.md`/fixpoint/gate),
  extractable to its own repo.

See [`maki/README.md`](maki/README.md), [`PLAN.md`](PLAN.md), and the design docs
`docs/ptx.md` / `docs/ptx-sketch.md` / `docs/inference.md` / `docs/autograd.md`.

## Docs

- [`docs/bootstrap.md`](docs/bootstrap.md) — bootstrap, refresh, and porting.
- [`docs/forth.md`](docs/forth.md) — mandatory Forth style and checker rules.
- [`docs/debugging.md`](docs/debugging.md) — stepper, debugger, breakpoints,
  watchpoints, image dumpers, JIT dumpers, and native fallback boundaries.
- [`STATUS.md`](STATUS.md) — current gate status.
- [`LESSONS.md`](LESSONS.md) — concise project memory.
- `.dots/` — active implementation tasks.

## Source Layout

- `src/core/` — checker, renderer, roles, combinators, hashes.
- `src/arch/arm64/` — assembler, encoders, disassembler, mnemonics.
- `src/habu/` — engine builder, JIT, AOT drivers, tree shaker, profiler,
  debugger, stepper, snapshot/fixpoint drivers.
- `src/os/` — Linux ELF and macOS Mach-O target seams.
- `tools/` — checked Habu automation and build tools.
- `test/` — native Habu gate and focused suites.
