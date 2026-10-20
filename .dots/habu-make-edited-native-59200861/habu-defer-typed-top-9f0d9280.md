---
title: Defer typed top-level release
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T12:11:11.216473+03:00"
closed-at: "2026-10-20T18:19:57.335467+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

The private typed top-level tracker is unrelated to incremental FILE reuse or self-building, but its per-token graph copying and pending refusals block qualification. Astra traced 2D50/58/60 and typed row scratch: none is consumed by caches, TASK state or observed inputs. Restore current public warning/tier behavior while retaining shared checked-body correctness, raw/typed capture checks, native body hooks, file execution span and the complete incremental line. Merge current public master first, preserve its null/layout-pointee fix. Keep typed-top source/history and its own future dot; do not weaken public behavior or revert mixed prerequisite commits. Acceptance: independent adversarial review, public top behavior and real incremental E2Es, freshly matched native product, cold-prefix boot, convergence and full native gate.

Source35e00c85 merges publicbe25f70b and removes199net production lines, preserves checked opaque-tick store refusal and restores public set-top-check PE-N. Independent Astra review and focused follow-up are clear. Matching native4653470c/f953d81d passes public top-row warning/hook, xt-effect, typed-storage including public null/layout checks, and checked raw/opaque storage refusals. Its cold-prefix aot-wid-build passes. Source-only Gforth recovery check-only passes all launcher regressions, including exact missing-preflight refusal; no native replacement was installed. Required generation chain and full native registry remain before landing/promotion.

The existing five-generation native chain now exits0: all five engines are a5f93a787ae17205144206b369b8b3f5cab5d463a83f31059b410287304a0a84, all five maps9cfc53caa102c269fc56da246021f5ea4084dd48f048d30c992e417ec158763a. Every adjacent map matches; the driver reports zero differing bytes for engine generations2/3,3/4,4/5. The active root owns the final pair; completed generation workspace and products are retired. The full registry on that pair is running; its source-root-exe failure is separately tracked620caf2d. No public promotion yet.
