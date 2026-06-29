---
title: Collapse stdlib inner hb spawns
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-29T05:53:20.746031+02:00\""
---

Problem: after AOT warm runner, hot full gate still ~44.6s. AOT-positive is ~4.5s and AOT-negative ~15s under contention, but stdlib check-cli/tool-boundary remain ~27s and gate counters still show inner-hb=55, inner-hb-stdin=41, helper-spawn=106. Cause: runner-backed stdlib slices still launch many nested bin/hb checks instead of using in-process checked helpers or batched warm runner paths. Fix: inspect test/gate-stdlib-lib.f and tool/check helpers; replace repeated inner hb spawns with in-process/batched checked Habu runners while preserving at least one real CLI boundary per tool contract. Acceptance: focused stdlib check-cli/tool-boundary materially lower; hot full native gate drops below the prior ~43.3s and toward 30s; diagnostics unchanged; host/filemap/typed diff gates green.

Progress 2026-06-29: split `tools/check.f` into `tools/check-core.f` plus thin
entries, added direct capture/source-list helpers, moved check-test die,
source-list success, and source-list reserved-name semantics in-process while
keeping usage/file-label CLI smokes. Fixed warm-image entry shape with
`tools/check-main.f` for checker-warm and `WR-TOOLS-LOAD2` for tools-warm. Hot
full gate now passes at 42104ms internal / 45.42s wall, down from the prior
43273ms internal baseline; `check-cli` hot full slice is 23633ms and
`tool-boundary` is still 27494ms. Next work: cut `tool-boundary-lints` and
engine fixture tails without adding more top-level contention.

Progress 2026-06-29: routed `tools/check-test.f` CLI smokes through the
checker-warm image (`HABU_WARM_CHECK`) so `tools/check-core.f` is baked once
instead of reloaded through tools-warm for each check wrapper boundary. Moved
`check-repair-hints-test` repair-class checks in-process through
`CHECK-ALL-ERRORS-FILE`; engine repair now loads the core explicitly instead of
hiding it in a child process. RCA on a failed full run found the AOT maker cache
lock was the wrong invariant: concurrent AOT phases share one maker key, and
the waiter timed out while the holder was still building under contention.
Removed the maker lock and kept atomic publish by private temp build +
`rename`. Proof: `hb-build-test` passed, focused AOT-positive cold passed in
39.18s, focused AOT-negative hot passed in 20.15s, and hot full gate passed at
40935ms internal / 44.21s wall with `candidate-hit=1`, `runner-build=0`,
`warm-miss=0`, `maker-hit=1`, `helper-spawn=106`. Remaining blocker:
`tool-boundary` is still 26559ms and engine fixtures are 25108ms under full
contention.

Rejected experiment 2026-06-29: moving persistent tools/checker warm cache-hit
validation in-process into `test/run.f` removed the two visible warm child
phases, but serialized content-key hashing before any real phase could start.
The hot full gate regressed to 46577ms internal / 50.54s wall. Keep warm
validation parallel until the content-key check itself is made cheaper.

Progress 2026-06-29: split hot lint fixtures into load-only `*-test-lib.f`
libraries plus thin entry wrappers, added `test/gate-stdlib-inline-lib.f`, and
made the warm runner's `tool` entry skip the spawned `tool-boundary-lints`
suite while running those lints in-process immediately before `GT-POOL-DRAIN`.
This keeps the remaining tool-boundary child suites overlapped instead of
serializing the inline work. Proof on the exact tree: direct warm-runner `tool`
slice passed at 15.05s wall with no `RUN: tool-boundary-lints`; hot full native
gate passed at 28139ms internal / 31.47s wall with `candidate-hit=1`,
`runner-build=0`, `warm-miss=0`, and budget `28139ms <= 90000ms`. Commit gates:
typed-local diff lint passed, `filemap-lint: 247 path(s), 0 finding(s)`, and
`host-lint: 0 finding(s)`. Remaining debt: aggregate counters still report
`inner-hb=55`, `inner-hb-stdin=41`, `helper-spawn=106`; those are now overlapped
enough for the hot internal gate target, but further wall-clock cuts require
batching the remaining CLI contract smokes rather than adding more top-level
suite splits.
