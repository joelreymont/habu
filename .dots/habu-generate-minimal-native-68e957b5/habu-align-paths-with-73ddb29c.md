---
title: Align paths with package owners
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T22:40:43.846755+02:00"
---

Invariant: a subsystem package and its physical directory express the same ownership boundary; broad flat directories do not mix unrelated GPU, CUDA, store, autodiff, optimizer, and model concerns. Current flat roots contain more than two hundred Maki Forth modules and eighty-five PTX modules, including coherent clusters that already have or need distinct package owners.

After each real package boundary is settled, move that subsystem atomically into a matching directory in small disjoint leaves. Initial leaves are Maki GPU and CUDA, Maki stores, PTX autodiff, and the PTX optimizer only if its adjudication proves it should survive. Delete thin legacy re-export modules after callers use the canonical owner instead of moving aliases. Reuse the exact package dots for API and representation changes; this controller owns physical paths, path consumers, and integrated proof only. Do not create directories as cosmetic substitutes for packages.

For each leaf update all requires, suite inventories, manifests, build and generated-source closures, cache keys, tool routes, documentation commands, and file-map rows with no compatibility paths or fallback lookup. Prove standalone loads, dependency order, exact public package names, stale-path rejection, cache invalidation, suite membership, snapshots, ahead-of-time compilation, bootstrap, fixpoint, Maki, PTX standard library, package, host, file-map, and full native gates. Measure path duplication, manifest and closure entries, source and loaded size, build time, and gate wall time before and after; require no unexplained growth. The optimizer leaf is blocked on the measured delete-or-integrate verdict.
