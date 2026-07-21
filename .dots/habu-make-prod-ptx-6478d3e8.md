---
title: Make production PTX modules dependency closed
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:48.200628+02:00"
---

Twenty-two production files under lib/ptx fail when loaded directly because their direct requirements are supplied only by ambient aggregate order. Make every production PTX module declare its complete direct dependency set and add a bare-load regression for every nested module, not only flat standard-library files. Derive dependency closure from the existing source-composition event stream and compare declared direct edges with observed loads; unknown, cyclic, missing, duplicate, or order-dependent dependencies fail with the exact module and word. Package ownership work may shorten public names, but it must not replace explicit module closure, and a larger shared prelude is forbidden. Preserve all current PTX bytes, public APIs, test order, and device behavior. Add isolated-process loads in varied valid order, a mutation that removes each direct require, aggregate-versus-bare parity, cycle detection, and cache-key invalidation when any transitive dependency changes. Files: lib/ptx module require rows, a checked nested-module closure gate and focused tests, source-composition adapter, manifests/FILEMAP. Verify all 22 bare loads, PTX standard library, co-loaded device tests, Maki, package/host/filemap/dot lints, and full native gate.
