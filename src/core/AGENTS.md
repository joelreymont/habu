# src/core — the checked core

Load order is `PFX-FILES` in `src/habu/habu2.f`.

- `checker.f` — the stack-effect checker: effect grammar, the primitive rows it
  replays from `src/habu/prims.f`, unification; `check-hook.f` installs it.
- `type-schema.f`, `type-family.f` — the schema arena and the family, variant,
  field and layout registries (package TFAM).
- `sumtype.f`, `structure-decl.f`, `enum-decl.f` — typed declaration definers,
  over `decl-event.f`, `structure-make.f` and `generated-declaration*.f`.
- `structures.f`, `enums.f`, `layout-buffer*.f`, `pointer-storage.f`,
  `dynamic-storage.f`, `exec-vector.f` — storage and vector definers.
- `include.f` — `include` and `require`; `top-row.f` — the typed top-level row;
  `render.f` — diagnostics; `engine-error.f` — the engine failure ABI.
