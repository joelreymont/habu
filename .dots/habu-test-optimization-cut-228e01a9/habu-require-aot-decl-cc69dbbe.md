---
title: Require aot-decl.f and aot-arm.f in aot-capture.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:17:49.768602+02:00"
---

Problem: src/habu/aot-capture.f uses words from src/habu/aot-decl.f and src/habu/aot-arm.f without requiring them (forth-card section 7: a file requires what it uses); it works only because tools/build-fixpoint.f loads both before it (rows near :1110 and :1153). Acceptance: aot-capture.f states both requires; the two build-fixpoint rows become BF-APPEND-MODULE so the require is a no-op inside the fixpoint build; tools/build-fixpoint.f, tools/bootstrap.sh check-only and tools/native-build.f still build; g1 == g2 (review 164). Files: src/habu/aot-capture.f, tools/build-fixpoint.f. Verify: native build rc 0, g1 == g2 and .names, two-generation build rc 0.
