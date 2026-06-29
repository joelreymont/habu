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
