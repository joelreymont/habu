# src

Everything `hb` is made of, in checked Habu; each module has a real package and
states its contract in its header comment. An engine starts in
`arch/arm64/boot.f` ENGINE-START, which calls the MAIN `habu/main.f` installs.

- `habu/` — the engine: primitive table, engine builders, interpreter, images.
- `core/` — the checker, type families, typed definers and source loading; the
  kernel ends where the checker is switched on (`docs/architecture.md` "Kernel").
- `compiler/` — the compiler's shared half: target contract, definition, emission, shadow.
- `arch/` — one directory per instruction set; `os/` — one per target seam.
- `lib/` — the library, one concern per file.
