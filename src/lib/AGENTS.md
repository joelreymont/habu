# src/lib — the library

One concern per file and one package per module; a module `require`s its own
dependencies. Its header states purpose, contract and storage class
(process-wide, task-local or caller-owned). A module's focused test sits beside
it as `<name>-test.f`.

- `test/` holds the test framework: its assertions, suites, evaluated text,
  runner and fork helpers.
- `net/` (TCP, UDP, TLS, HTTP, WebSocket, curl, event streams, model chat
  and embeddings), `crypto/`, `json/` (the reader, writer, pointers, copy,
  equality and values of `json.f`), `xml/`, `unicode/`, `adt/` (option, result,
  nullable), `type/` (`deftype.f`), `secret/` (per-OS backends of `secret.f`),
  `heap/` (per-target backends of `heap.f`), `power/` (per-OS backends of
  `power.f`), and `browser/` (the browser host and its JavaScript peers).
- Task-local storage follows `task.f`'s header: a module the engine bakes takes
  a band in `src/habu/layout.f`, any other takes rows from `task.f`.
