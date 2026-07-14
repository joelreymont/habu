---
title: Track DOES invocation ownership
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T20:31:02.335189+02:00\""
---

Full context: src/habu/habu1.f BCREATE and src/habu/habu2.f EMIT-DOESPATCH use global LASTC. EVALUATE rollback exposes stale CREATE ownership: static caller-PC/DREC matching is unsound because it rejects factored CREATE helpers and accepts a stale CREATE from a prior invocation of the same conditional definer. Implement a protected invocation-scoped definer token stack: runtime entry for a definition containing DOES> establishes a fresh token, factored CREATE records the active token, DOES> accepts and consumes only the exact current invocation's CREATE, and normal return/CATCH/THROW/EVALUATE/task transitions restore depth without leaks. Add factored positive, same-definer prior-invocation negative, nested, recursive, throw, evaluate rollback, task, bootstrap parity, protection, snapshot, and performance regressions. Remove static DREC/caller-PC ownership.
