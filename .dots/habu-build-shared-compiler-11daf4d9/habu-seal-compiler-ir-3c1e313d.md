---
title: Seal compiler IR facade
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.469277+02:00"
blocks:
  - habu-validate-compiler-pass-79e0660c
---

Full context: PLAN.md shared-substrate exit requires one real closed schema through the generic builder/freeze/verify/codec path before public assembly. Add src/compiler/ir.f semantic IR facade over public substrate APIs, protect package wordlists, and prove IR-RAW is inaccessible from the facade/dialects. Acceptance: closed-schema positive and hostile fixtures pass; public surface is exact; raw casts and frozen mutation are unresolvable; abort/release leak no owner. Dependency: validated pass results.
