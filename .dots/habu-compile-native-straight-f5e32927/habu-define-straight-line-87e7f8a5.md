---
title: Define straight-line HIR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:03.005233+02:00\""
blocks:
  - habu-bind-checker-env-ed4f9f87
---

Full context: design section 7.2 and Wave 2 require one closed HIR schema for integer literals, modeled arithmetic calls, compile-time DUP/DROP/SWAP/OVER effects, and return. Acceptance: schema/effect/target/source bindings validate exhaustively; unknown or unmodeled words reject with named capabilities. Dependency: checker environment manifest.

Claim: agent=hirschema workspace=.jj-ws/habu-define-straight-line-87e7f8a5
