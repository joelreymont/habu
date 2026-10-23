# test — the native test suite

`bin/hb --load test/run.f` runs the full native registry: `gate-stdlib.f` loads
the rows of `gate-stdlib-cases.f`, which adds `gate-arm-cases.f` or
`gate-x64-cases.f` for the host, and runs them in `gate-pool.f`'s process pool.
`gate-images.f` builds the keyed images rows share. When to run what, and what
a suite file must satisfy, is in `docs/gate.md`.

- `compiler/` — compiler, capture and image tests; `wasm/`, `browser/` — Wasm
  and browser-host tests; `db/` — the private PostgreSQL cluster;
  `policy/` and `fixtures/` — subjects other tests load; `golden/` — packets.
- Non-Habu files are device peers (`*.py`, `wasm/*.mjs`, `browser/*.mjs`) and
  the Gforth recovery checks (`bootstrap-*.fs`, `nf.fs`) `tools/bootstrap.sh`
  runs.
