---
title: "Universal enum: parametric + quotation payloads"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:09:20.861386+02:00"
---

Phase 0 (Joel: type families need the universal enum). SUMTYPE v1 payload grammar (src/core/sumtype.f:13-17) rejects parametric family applications (option<a>, result<x,y>) and quotations as variant payloads (E-TDECL-PAYLOAD) — while slice 5 (94bdaf7) landed the nested-ADT + linear payload LAYOUT capability, so representation exists but declaration cannot reach it. Extend the declaration grammar (via the unified type DSL, not a sumtype-only patch): payload elements admit closed parametric applications resolved in signature scope (nesting depth per the slice-5 layout rules) and quotation types for xt-carrying variants; constructors/MATCH/layout ride the landed capability; negative fixtures per new reject path; fixpoint + full gate. Sequenced in the fields/DSL lane after factor-field-schema; feeds the typed planner IR (result<plan,reject>, option<tensor>) both flagships author against.
