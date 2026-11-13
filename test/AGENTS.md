# test — the native test suite

`caffeinate -i bin/hb --load test/run.f </dev/null` runs the full native test
suite; `bin/hb --load test/run.f -- name ... </dev/null` runs only the named
tests. `gate-stdlib.f` loads
the tests of `gate-stdlib-cases.f`, which adds `gate-arm-cases.f` on an arm64
host, and runs them in `gate-pool.f`'s process pool.
`load-graph.f` reads the tests' load graph, which `gate-entry-guard.f` walks.
Keep stdin at end of file: the tests' process groups inherit it, and a read
from a terminal stops them with SIGTTIN. Give a run its own `TMPDIR` outside
the checkout: jj snapshots a file a run leaves there that `.gitignore` does not
name into the working-copy commit.

- `compiler/` — compiler, capture and image tests; `wasm/`, `browser/` — Wasm
  and browser-host tests; `db/` — the private PostgreSQL cluster;
  `policy/` and `fixtures/` — subjects other tests load.
- Non-Habu files are device peers (`*.py`, `wasm/*.mjs`, `browser/*.mjs`).

## Writing tests

A test earns its place by a user-visible failure no other test catches. The
suite runs in two to three minutes, five at most.

- Test end to end, through the real load path, each test ending in a
  repeatable artifact. Never write unit tests after the code.
- The suite keeps the self-build fixpoint, one rejected program per type rule,
  programs at boundaries hb's own build never reaches, a capacity limit a real
  program can reach and a refusal of a malformed saved file, made by
  corrupting the file on disk. It drops tests of what the self-build proves, of
  machinery being replaced, of one tool agreeing with another, of the harness,
  of in-memory state no real input reaches (a hand-built table, a forged token,
  a patched code byte), and lints; redundant assertions, implementation
  mirrors and change detectors go too.
- A failing test of a target whose turn has not come is switched off with
  `[if]`, and a platform's tests are switched by platform; a test is deleted
  only when it is not load-bearing.
- Run compatible cases in one loaded tool or built program; start a fresh
  process or build only when that boundary is under test, and say why beside
  it. Never split a slow test to meet a limit: remove repeated work and share
  setup.
- The files of one suite entry load into one image in order: each is
  package-scoped, safe to load twice and closes its package by its last line,
  and its fixture names carry its own tag.
- A CLI tool that reads argv is spawned, never included; its assertions live in
  an argv-free `-test.f`. `SUBJECT:RUN` forks the test process: call it with no
  package open.
- A spawned `bin/hb` is given what it runs: with no argument it reads its
  program from stdin, and a pipe left open blocks it.
- A hang is named by its WAIT line, never ended by a clock. A test that sets a
  deadline of its own and expects it asserts it; a failing test is never fixed
  by raising a deadline, retry count or sleep.
- A test that needs a server starts a private one as its own child on a
  Unix-domain socket in its `HB_SOCK_TMP`, never through a launcher that
  daemonizes it, and stops it whatever its cases do (test/db/pg-cluster.f).
- No test creates a tree or path Finder cannot delete.
