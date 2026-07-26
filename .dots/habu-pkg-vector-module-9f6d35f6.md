---
title: Package vector module surface
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:22:48.835637+02:00"
---

Fourth instance of the unpackaged-legacy wall, exposed by the MEM:RELEASE rename (its diff touches VEC-RELEASE-STORAGE at lib/vector.f:149, outside package VEC which opens at line 244, and the exact package gate correctly rejects any edit to an unpackaged word's body - proven by a whitespace-only control edit tripping the same finding). Own the WHOLE module per the partial-ownership-bands ruling: package VEC absorbs the legacy raw VEC-* surface above line 244 with short package-local tails; callers migrated in the same commit (measured: 353 references across 15 files including maki/schedule.f, maki/model-ir.f, lib/ptx/cg-vec.f, tools/lint files); load-position rule respected (requires above the package line, any load-time dispatch below ;package via captured token). Behavior identity proven by the consumer suites green plus a boundary-aware rename-map audit with zero collisions (the harness-packaging machinery and precedent apply). Acceptance: package-diff-lint rc=0 on a representative body edit inside the module (the exact probe that fails today); vector, schedule, model-ir, and touched lint suites green; both diff lints on the full artifact; no alias, no forwarder, no exemption. Real pre-change failure: E-PACKAGE-OWNERSHIP lib/vector.f:178 VEC-DISPOSE from a whitespace-only hunk, measured.
