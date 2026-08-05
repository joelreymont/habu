---
title: Delete the canonical codec stack
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.522482+02:00"
---

CG-10 + CG-31. canon.f, encode.f, render.f, diff.f: ~4,560 source lines plus ~1,750 test lines of speculative major/minor version compatibility with no product object, cache, compiler, or inference consumer. Also unsound: encode.f:190-194,226-229,345-352 accepts a caller context independently of the module and writes that context's binding policy into the frame (a module built under TRAP encoded under WRAP and returned success). Delete the stack and its tests. If a concrete object/cache consumer appears later, build the smallest exact current format for that consumer, no backwards compatibility. Reconcile with the campaign dots that minted it (habu-encode-compiler-ir-545ee6d1, habu-render-and-diff-3d249719, habu-canonicalize-compiler-tables-e0c7f8f1 in .dots/habu-build-shared-compiler-11daf4d9/).
