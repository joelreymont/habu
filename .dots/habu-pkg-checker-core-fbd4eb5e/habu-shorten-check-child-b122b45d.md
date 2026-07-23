---
title: Shorten CHECK child runner
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:13:22.553053+02:00"
blocks:
  - habu-shorten-check-declaration-0a28d9d9
---

Why: nominal-pass ordering and native child invocation share the private run buffer and capture boundary, but remain legacy CHK-prefixed after declaration scanning is package-owned. Owner: package CHECK. Files: tools/check-core.f and tools/check-test-lib.f. Rename only the private words and storage from RUN-NOMINAL-FILE through REPLAY: nominal file/as/id/order execution, label validation, run-buffer reset/append/number formatting, native command prefix and argument construction, captured child launch, and exact stdout/stderr replay. Use short package-local tails and update only direct callers. Preserve dependency preload order, source labels, quoting rejection, command arguments, child result, captured bytes, replay order, and allocation/capacity failures exactly. Acceptance: zero executable CHK-prefixed name remains in this concern; direct source, one file, dependency list, label quoting, child success, checker rejection, engine absence, captured stdout/stderr, capacity, and repeated-run fixtures execute the production CHECK entry points byte-for-byte. Forbidden: aliases, shell command construction, duplicated process launcher, swallowed child errors, public buffers/helpers, changed replay timing, or behavior changes. Pre-change proof: a representative short runner helper fails package ownership outside CHECK and passes only as CHECK-private. Verify through tools/check-test.f direct/list/capture/engine fixtures, nominal dependency fixtures, exact diff ownership/type, host, file-map, and gate diagnostics.
