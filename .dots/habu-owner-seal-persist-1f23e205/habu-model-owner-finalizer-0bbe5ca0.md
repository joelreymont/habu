---
title: Model owner finalizer primitive
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T13:53:44.487634+02:00\""
---

Context: generated AOT prefix now executes OWNER-WID:FINALIZE, a public checked zero-stack-effect finalizer. Cause: without an exact checker primitive model, generated source certification fails rc 70 before owner restore tests. Fix: package the runtime finalizer under OWNER-WID; add PRIM: OWNER-WID:FINALIZE PRIM; to every exact checker/generated/recovery inventory; update emitted token, seal census, and parity regressions. Files: src/habu/habu1.f, src/habu/habu2.f, src/habu/owner-wid-emit-seal.f, checker primitive inventory files located by census, bootstrap/cg/forth.fs if mirrored, focused checker/generated parity tests. Acceptance: generated source certifies with effect ( -- ); exact primitive inventory parity passes; owner child and build-fixpoint owner tests green.
