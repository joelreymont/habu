---
title: Factor the frozen-module reader plumbing
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T21:20:20.170286+02:00"
---

Five native passes carry near-identical scaffolding: view-slot tables (V-OPP/V-OPR/...), SAME-SYM?/SAME-TYPE? identity comparisons, OP-AT/OPERAND-AT/RESULT-AT accessors, VIEWS! wiring, and the BIND-DIALECT/BND-TAKE spent-binding state machine - in src/compiler/native/regalloc.f, regalloc-verify.f, emit.f, spill.f and select.f. The independence argument that justifies each pass re-deriving its CHECKS does not cover this accessor plumbing, which is copy-paste that will drift (a change to the view API means five edits). Factor the accessors and the binding state machine into one substrate file the passes require, keeping each pass's checks where they are. Acceptance: the five passes shrink by their shared plumbing, behavior identical (all suites green with the same exact expectations), and a view-API change becomes one edit. No new abstraction beyond what the five files already spell out five times.
