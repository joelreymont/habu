---
title: Builder TRUST rows to CHECKED
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.827175+02:00"
---

Convert redundant builder TRUST effects in `src/habu/habu1.f`,
`src/habu/habu2.f`, and `src/habu/jit.f` into checked definitions. The exact
surviving boundaries are `STDIN?` and `FP-EMIT` in `habu1.f`,
`EM-HXT-EXECUTE` in `habu2.f`, and `JIT-XT-EXECUTE` in `jit.f`. `STDIN?` is a
variable whose generic definer publishes `-- ptr a`; convert it through the
existing typed-variable declaration path. The other three boundaries execute
opaque execution tokens, so the checker cannot infer their effects. Preserve
those three source assertions with their local checker-gap rationale; the
redundant `VERIFY-SOURCE` cleanup must exclude them.

The completed `habu1.f` cleanup established that production `VERIFY-SOURCE`
runs `CHECK!` on each ordinary `:` body and rejects both reject and uncheckable
verdicts. Remove only assertions made redundant by that production proof. Work
file-by-file and keep the byte-for-byte fixpoint green after each batch.
Conflict: active `habu-own-engine-emitter-42db38aa`, claimed by
`agent=engine_emitter_impl`, owns `src/habu/habu1.f` and
`src/habu/habu2.f`; begin after that owner lands. Owning checks are
`test/run.f`, `test/aot-wid-build.f`, bootstrap recovery through
`tools/bootstrap.sh`, and the native build fixpoint through
`tools/build-fixpoint-refresh.f`, as applicable to the changed source.
