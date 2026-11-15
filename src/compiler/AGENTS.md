# src/compiler — the compiler's shared half

- `target.f` — the immutable target contract; `numeric-policy.f`; `binding.f`
  binds the two; `target/` — build target and native binding values.
- `session/` — the lease on the compiler's shared state.
- `native/definition.f` stages, checks and publishes one pending definition
  for src/habu/t0.f; `emission.f` holds one sealed emission, which
  `publish.f` commits.
- `native/shadow.f` keeps each published definition's routine for a second
  machine.
