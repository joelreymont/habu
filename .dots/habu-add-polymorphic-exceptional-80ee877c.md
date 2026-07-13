---
title: Add polymorphic exceptional quotation effects
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T13:37:31.695884+02:00\""
---

Problem: quotation terms carry XHAS/XDEAD and exceptional G/H rows, but public signature grammar declares only normal A-D rows; exact quotation unification now correctly rejects effect erasure, exposing higher-order APIs such as TEST:RUNNER! that must accept callbacks which may either return or throw. Fix: add canonical exceptional-effect syntax and effect-row variables/subeffect constraints; preserve exception inputs/outputs; infer may-throw/no-normal-continuation without conflating die; extend stored EFF/VREC/schema rendering and diagnostics; remove trusted callback-setter boundaries. Acceptance: normal callback and declared may-throw callback both satisfy a polymorphic higher-order contract; undeclared throwing callbacks reject; G/H cycles/linearity/wide/persistence round trip; catch restores declared exceptional rows; lib/test and all combinators compile checked with no erasure. Files: src/core/checker.f, src/core/render.f, docs/effects.md, lib/test/suite.f, focused engine/type suites. Depends: habu-preserve-exceptional-quotation-48be3da0. Ownership: checker effect grammar/unification/call semantics; no AOT envelope/materializer edits.
