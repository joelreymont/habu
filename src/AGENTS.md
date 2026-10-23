# src

Everything `hb` is made of, in checked Habu; each module has a real package and
states its contract in its header comment. Every program starts from the
`--load` prefix `PFX-FILES` in `habu/habu2.f`.

- `habu/` — the engine: primitive table, engine builders, interpreter, images.
- `core/` — the checker, type families, typed definers and source loading; the
  kernel ends where the checker is switched on (`docs/architecture.md` "Kernel").
- `compiler/` — the tier-1 native compiler: target contract, IR, pipeline.
- `arch/` — one directory per instruction set; `os/` — one per target seam.
- `lib/` — the library, one concern per file.
- `examples/llm/good.f` — a fixture the public-signatures tests scan.
