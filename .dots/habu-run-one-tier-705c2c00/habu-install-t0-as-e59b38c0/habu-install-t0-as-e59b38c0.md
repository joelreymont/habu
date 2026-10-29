---
title: Install T0 as the arm64 runtime compiler
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:20:39.934072+02:00"
---

Lanes A1-A6 per umbrella lane order and census jerry-t0-l1/.out/a64-census.md sec. 5. A1 shared unit step; A2 emitter rows; A3 T0:COMPILE/COMPILE-FIXED/callback/DISPATCH!/INSTALL + committed suite; A4 borrowed-dispatch gate subsets; A5 measurement vs the JIT (go/no-go: load <= 1.2x JIT insn, run <= 1.05x, outputs identical); A6 engine build carrying T0 (L7). Integration workspace .jj-ws.noindex/jerry-a64 (merge of styvunzx and ktmkppmt; rebase onto C3 when final). Stack lands whole after a Fable high feature review.

A4 cut (lead, 2026-10-29): a borrowed-dispatch gate runner must rebuild the gate's private hooks, image grants, row spawning and an exclusion census over 142 set-tier files, only to preview what A6's full arm64 suite on T0 proves directly. Not a goal prerequisite. Kept: the T0 driver (require T0, select aarch64, T0:INSTALL, 1 set-tier, XT-CELL body counter printed through the exit hook chained as src/lib/fs-mutate.f does) moves into A5, where the client loads need it (census sec. 4 item 2-3). Per-lane evidence stays as now: tier 1's own tests for the feature run under T0 (insert the install after their requires). Notes: jerry-a4/.out/a4/a4-handoff.md.

Roadmap (Joel 2026-10-29, "a working tier-0 compiler, then performance in small well-defined steps"): 1 land the 55-commit T0 line (Fable feature review running) and S2B-1 (fold-ins running, re-review next); 2 habu-run-the-full-suite... (child); 3 this dot: bake T0 into the engine as COMP (needs the one-process build, habu-build-a-tree-d00185b1, which continues alongside; T0 compile speed becomes build speed after cold-load: measure); 4 habu-delete-the-jit (child); then habu-t0-performance-steps (child). Landing note: the line rebases onto master with one trivial conflict in test/compiler/session.f (master renamed fd-io.f -> fd.f where the line adds a require; keep both); the whole line moves from oylsqluo, never from worxlvkp alone.
