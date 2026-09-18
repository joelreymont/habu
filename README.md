# Habu — Checked Forth

Habu is a self-hosted Forth with a static stack-effect checker. Every checked
word declares the values it consumes and produces, and the checker verifies that
the implementation matches that contract before the word can run.

```forth
: SQUARE ( i64 -- i64 ) dup * ;   \ accepted
: BAD    ( i64 -- i64 ) dup ;     \ rejected: leaves an extra i64
```

The native `bin/hb` engine provides the REPL, checker, ARM64 JIT and AOT
compiler, debugger, profiler, source loader, and image builder.

Loom, the ML framework and model-CAD layer formerly kept in this tree, now lives
in the sibling [`loom`](../loom/README.md) repository, together with the PTX
backend, the checked GPU-kernel vocabulary and the CUDA driver bindings. Loom
depends on Habu. Habu's language, compiler, runtime, standard libraries, numeric
types and FFI remain here, including the substrate the moved code still needs:
the GPU type families and barrier hooks in the checker, the PTX target row in
the compiler, and the PTX error codes in `lib/errors.f`.

## Quick start

Start the checked REPL:

```sh
bin/hb
```

Load a source file or run the native suite from the repository root:

```sh
bin/hb --load path/to/program.f
bin/hb --load test/run.f
```

If `bin/hb` is missing or broken, read the current recovery status and procedure
in [docs/bootstrap.md](docs/bootstrap.md). The build and
gate recipes live in [`skills/habu-build/SKILL.md`](skills/habu-build/SKILL.md)
and [`skills/habu-gate/SKILL.md`](skills/habu-gate/SKILL.md).

## What the checker covers

Checked definitions use ordinary Forth with typed stack comments. The checker
supports concrete and nominal types, row-polymorphic effects, quotations,
locals, control flow, loops, recursion, return-stack effects, packages, and
algebraic data types. Explicit `TRUSTED:` and `TRUST` sites mark the small set of
compiler or runtime boundaries the checker cannot express directly.

## Repository layout

```text
src/core/         checker, type families, source loading, core words
src/compiler/     compiler policy and shared compiler support
src/arch/arm64/   ARM64 assembler and native code generation
src/habu/         native engine, JIT, AOT, debugger, profiler, image tools
src/os/           Linux and macOS target seams
lib/              checked standard, numeric, FFI, and runtime libraries
tools/            build, lint, inspection, and backend smoke tools
test/             native Habu suite and focused compiler/runtime tests
bench/            benchmarks
docs/             language, compiler, runtime, and backend documentation
skills/           operational recipes
```

Everything PTX and GPU lives in the sibling Loom repository: the emitter, the
typed kernel vocabulary, the CUDA bindings, application kernel producers,
benchmarking, autotuning, device goldens and the ML expression IR. Loom's README
documents native scoped-root loading against Habu's shared libraries.

## Documentation

- [`docs/forth.md`](docs/forth.md) — Forth conventions and checker rules.
- [`docs/type-system.md`](docs/type-system.md) — the checked effect system.
- [`docs/type-families.md`](docs/type-families.md) — nominal and algebraic types.
- [`docs/bootstrap.md`](docs/bootstrap.md) — bootstrap and self-hosting.
- [`docs/debugging.md`](docs/debugging.md) — debugger and inspection tools.
- [`docs/stdlib.md`](docs/stdlib.md) — standard library reference.
- [`docs/gate.md`](docs/gate.md) — the native test suite and what it demands of a test file.
