---
title: "TFAM 5: one shared stdin-driver-closure manifest (gate 17e)"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-04T10:40:00.000000+02:00\""
closed-at: "2026-07-04T17:50:35.484174+02:00"
---

Gate 17e (PLAN.md :973-978): reconcile the stdin driver closure through ONE exact
shared manifest consumed by `tools/build-fixpoint.f`, `tools/bootstrap.sh`,
`tools/srclist.f`, `tools/hb-build-lib.f`, and `test/run-files.f`, so
`src/core/include.f`, `src/habu/aot-capture.f`, and `src/habu/stdin.f` are either
all in the keyed closure where they load, or explicitly proven outside it.

Deferred from habu-tfam-5-public-3a692040 (that dot fully landed the hb-build
AOT/REPL/object cache-key closure via tools/event-closure-lib.f — user-source
closure. The stdin/engine metabuild closure below is a separable gate-17e refactor
touching the engine build assembly + the audited bootstrap launcher, so it is NOT
half-wired here.)

Current state (verified by symbol):
- `tools/build-fixpoint.f` BF-EMIT-STDIN-RUN-SOURCE (:696-703) hardcodes the stdin
  driver closure `src/core/include.f` + `src/habu/aot-capture.f` + `src/habu/stdin.f`
  as concatenated source. BF-RECORD-STDIN (:886-888) records a SHA256 of the
  GENERATED stage2-src (BF-STAGE2-DIGEST :875-876), so their content is keyed
  transitively via the generated-source digest, but not via a named per-file manifest.
- `tools/hb-build-lib.f` HBB-KEY-COMMON-SOURCES / HBB-KEY-DRIVER-SOURCES (:453-519)
  do NOT list any of the three; hb-build's maker uses aot.f/build.f drivers, and
  include.f is captured via `BF-ENGINE$ HBB-KEY-FILE+` (HBB-MAKER-KEY! :524). So they
  are legitimately "outside" hb-build's keyed closure — but gate 17e wants that
  proven, not incidental.
- `test/run-files.f` TR-FILES (:103-104) lists include.f + stdin.f in a canonical
  source ordering; aot-capture.f is absent (it is host build-time only).
- `tools/srclist.f` (SL-PREFIX / SRCLIST-MAIN :66-84) emits a hardcoded canonical
  order verbatim.
- `tools/bootstrap.sh` is the audited no-binary launcher (host glue); any manifest
  it consumes must keep behavior `exec bin/hb ...`.

Work: introduce ONE canonical Habu-native manifest (checked/typed, e.g. a shared
`tools/stdin-closure-lib.f` word list) naming the stdin driver closure files in
load order; make build-fixpoint (stdin source assembly + digest), hb-build-lib
(maker/artifact keys — either fold the three or carry an explicit proven-outside
assertion), srclist (canonical order), run-files (TR-FILES), and the bootstrap
launcher all consume it so the set cannot drift. Add a lint/fixture that fails if
any of the three files is present in some consumers' closure but missing from
others. Because build source assembly changes, rebuild `bin/hb` via
docs/bootstrap.md native path and prove the checked fixpoint. Acceptance
(PLAN.md :1001-1004): result-cache closure tests cover the aot-capture.f stdin
source closure across every stdin builder/cache/list entry; the three files are
all-in-or-explicitly-out with a fixture proving it; fixpoint holds.
