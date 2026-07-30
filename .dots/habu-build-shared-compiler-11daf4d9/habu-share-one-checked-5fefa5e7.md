---
title: Share one checked IR module test fixture
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T02:10:02.442026+02:00"
---

Full context: test/compiler/ir-canon.f and test/compiler/ir-encode.f each build a frozen module along two admissible intern insertion orders through the real IR-BUILD API, and each keeps its own private copy of that fixture (bindings, plan, types, symbols, attributes, sources, dialect schema, one function, its block and its operations). The renderer and diff stage (habu-render-and-diff-3d249719) and any decoder will want the same fixture. Work: extract one checked test-support module - a real package, for example IR-FIX in test/compiler/ir-module-fixture.f - that publishes the binding, the builder plan, and a BUILD word taking the insertion-order and operation-order flags, then migrate both existing tests to consume it and delete their private copies. Acceptance: test/compiler/ir-canon.f and test/compiler/ir-encode.f both stay green through their exact owning bin/hb --load paths with no fixture words of their own; the fixture module has a package owner and typed effects; package-diff-lint and typed-local-diff-lint pass on the diff; the mutation matrices of both tests still go red under the same mutations they went red under before the move. Dependency: both tests as landed.
