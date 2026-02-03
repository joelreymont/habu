---
title: Port VM GC
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-03T22:43:50.602909+01:00\""
closed-at: "2026-02-03T22:55:32.755571+01:00"
close-reason: Port VM GC to RootSet; zig build test
---

Context: src/interp/vm.zig:901; cause: Vm.collectGarbage copies roots into ArrayList(Value) and writes back; fix: build RootSet (stack/global ranges + scalar slots + ext_roots range) and call heap.collectGarbageRootSet; remove write-back path; add regression tests; deps: habu-add-heap-rootset-7bdae262; verification: zig build test.
