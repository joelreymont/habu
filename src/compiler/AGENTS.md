# src/compiler — the tier-1 native compiler

- `target.f` — the immutable target contract; `numeric-policy.f`; `binding.f`
  binds the two; `target/` — build target and native binding values.
- `session/` — the lease on the compiler's shared state, and kept emissions.
- `ir/` — the IR substrate; `schema.f` is the opcode schema, `verify.f` the
  structural verifier.
- `native/compiler.f` compiles one pending definition: `feed.f` fills the tape
  (`tape.f`), `elaborate.f` builds HIR (`hir.f`), `loop.f` folds counted loops,
  then the backend selects (`select.f`, `select-x64.f`), allocates
  (`regalloc.f`, `spill.f`) and emits (`emit.f`, `emit-x64.f`) one sealed
  `emission.f`, which `publish.f` commits.
- `native/backend.f` — the backend registry; no stage names a backend.
  `native/shadow.f` compiles each published definition for a second machine.
