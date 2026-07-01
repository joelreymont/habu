# habu — Checked Forth

Habu is a self-hosted, checked Forth for macOS ARM64 and Linux AArch64.
`bin/hb` is the small native engine: it type-checks Forth stack effects,
JIT-compiles words to ARM64 code, rebuilds itself to a byte-for-byte fixpoint,
and can AOT build standalone binaries. It is not a snapshot launcher; the large
dictionary/checker state is loaded from source into runtime memory.

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

Use the repo skills for current commands:

- [`skills/habu-bootstrap/SKILL.md`](skills/habu-bootstrap/SKILL.md) — recover
  missing `bin/hb` with Gforth 0.7.9+, refresh the self-hosted engine, and port
  bootstrap work to Linux/aarch64.
- [`skills/habu-gate/SKILL.md`](skills/habu-gate/SKILL.md) — run focused and
  full native gates with explicit pool and budget arguments.
- [`skills/habu-host-profiles/SKILL.md`](skills/habu-host-profiles/SKILL.md) —
  run host-class macOS and Jetson/Orin timing profiles.
- [`skills/habu-build/SKILL.md`](skills/habu-build/SKILL.md) — build AOT
  binaries and REPL images.

After `bin/hb` exists, normal work is Habu-native: the checked REPL, source
loading, self-refresh, AOT builds, and gates all run through `bin/hb`. Run from
the repo root, or from a tree where `src/`, `lib/`, `tools/`, and `test/` are
available. Generated images are regenerable build artifacts.

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
per-call extent/mask identities are now checker-level constructor templates
(`fresh-extent-*`, `fresh-mask-*`), so independent contexts/spans do not silently
unify.

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
- [`skills/habu-bootstrap/SKILL.md`](skills/habu-bootstrap/SKILL.md) — bootstrap
  and engine refresh commands.
- [`skills/habu-gate/SKILL.md`](skills/habu-gate/SKILL.md) — native gate
  commands.
- [`skills/habu-host-profiles/SKILL.md`](skills/habu-host-profiles/SKILL.md) —
  host-class timing profiles.
- [`skills/habu-build/SKILL.md`](skills/habu-build/SKILL.md) — AOT and REPL
  build commands.
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
