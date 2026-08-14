---
title: Give baked records checker rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:56:13.392604+02:00"
---

Found by the arm-seed lane (5234727b checkpoint): EM-SEED-AOT registers ENGINE dict records only - the checker's declaration dictionary has no rows for baked words, so a baked-only word resolves at interpret level but a CHECKED definition naming it refuses E-UNDEFINED (measured on a pty: : ASF ( -- ) BPW-CLEAR ; refused). Interpret-level + patched call sites is sufficient for the bake payoff (the engine calls baked code via EM-AOT-PATCH-SITES, not checked user calls), but user batch programs are checked by default - baked-only words are not CALLABLE from them until the checker learns the records. Design: rows derived from the baked record surface at seed time (the record carries name + entry; the effect needs a source - decide whether the capture bakes certified effects alongside records, which is the honest shape). Files: src/habu/aot-capture.f (bake effects), src/core/checker.f (register at seed), habu2.f (seed path). Depends: 5234727b (the two-stream boot landing).
