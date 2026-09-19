# Restart

Start with [AGENTS.md](AGENTS.md) and [the Forth card](docs/forth-card.md).
This file points to current state; it does not keep a second release snapshot.

The accepted source head is the `hazel/integration` bookmark, also named
`cedar/compiler-integration`. Read it without snapshotting a shared working copy:

```sh
jj --ignore-working-copy log -r hazel/integration --no-graph
dot ready
```

A dot's original report may predate its fix. Check its named code and fixtures
against the current head before implementing it or proposing closure. Coordinate
file ownership and the serial full-gate slot with the integration owner. Create
new workspaces under `.jj-ws/` from the current accepted head, as AGENTS.md says.

- [Bootstrap and native builds](docs/bootstrap.md) owns recovery, private host
  copies, generation checks and promotion. Use the native build route for a
  release candidate; a recovery engine passing its bootstrap check has narrower
  acceptance and is not automatically an interchangeable release engine.
- [The gate](docs/gate.md) owns candidate qualification and test execution.
  Read the result for the exact source/engine pair being considered; a historical
  green gate does not qualify a later change.
- [Debugging](docs/debugging.md) owns crash diagnosis, guard-page faults,
  disassembly and profiling.
- [The language reference](docs/forth.md) and [the card](docs/forth-card.md)
  describe the current language surface, including wide and parametric locals.
- [The roadmap](docs/roadmap.md), [compiler plan](PLAN.md) and open dots own the
  remaining work. A green integration gate does not complete every campaign
  or establish every downstream application's acceptance.

Engine hashes, sizes, timings, lane assignments and pending commit ids belong
with the measured candidate or its dot, rather than in this restart index.
