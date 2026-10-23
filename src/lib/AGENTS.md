# src/lib — the library

One concern per file and one package per module; a module `require`s its own
dependencies. Its header states purpose, contract and storage class
(process-wide, task-local or caller-owned). A module's focused test sits beside
it as `<name>-test.f`.

- `test.f` is the test framework's interface; `test/` holds its runner, suites,
  assertions and fork helpers.
- `net/` (TCP, UDP, HTTP, WebSocket, curl, event streams), `crypto/`, `db/`
  (connections and row readers over `pg.f`), `xml/`, `unicode/`, `adt/` (option,
  result), `type/` (`deftype.f`), `secret/` (per-OS backends of `secret.f`),
  `browser/` (the browser host and its JavaScript peers), `c2-memory/`.
- A module the engine bakes or its build loads takes a declared band in
  `src/habu/layout.f`; any other task-local module takes `TASK:+USER` rows.
