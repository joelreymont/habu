---
title: Typed depth introspection capability
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.841471+02:00"
---

Checker capability discharging the largest TRUSTED test-metaprogramming class: T{/->/}T (lib/array-test.f:37-52), prop-test depth checks, and assert-layer helpers are TRUSTED only because 'depth' + arbitrary-stack capture cannot be typed. Design: a checked combinator whose effect says 'captures the row delta since a marker' - e.g. SNAP{ ... }SNAP with row-polymorphic effect ( R marker -- R ) internally witnessing the captured row - or a restricted typed API (depth-of-marker, cell-at-capture idx) with roles. Deliverable: capability in src/core/checker.f + primitive model, shared lib/test/snap.f rewritten checked (feeds habu-shared-t-t-470833e6), TRUSTED count drop measured. Needs design doc first (docs/effects.md extension).
