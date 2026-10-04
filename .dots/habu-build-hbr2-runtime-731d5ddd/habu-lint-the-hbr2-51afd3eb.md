---
title: Lint the HBR2 package DAG
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.336073+03:00"
---

Problem: HBR2 §1.2 forbids RUNTIME to import UI, SCENE, BROWSER or SYNC and keeps UI off browser, GPU and SYNC code; §27.1 makes a package cycle a build failure and T23 (§28.4) scans the headless packages. Habu walks require graphs (tools/manifest-lint-core.f) but has no rule for these layers, and a qualified call into a package someone else loaded earlier resolves with no require edge at all, so a require scan alone misses it; measured on 7c03e870, a two-file require cycle loads with rc 0 unless a body names a not-yet-defined word. Acceptance: a Habu lint over a declared layer table (RUNTIME = lib/runtime/; UI = lib/ui/, which may import RUNTIME; SCENE, RENDER, SYNC, BROWSER and host/browser/ declared for later layers) that walks the require graph from every file of each layer and refuses a forbidden edge or a cycle, printing the path; each layer's closure also loads alone in a fresh process, so an unrequired qualified reference fails as E-UNDEFINED; fixtures for a RUNTIME file requiring a UI file, a two-file require cycle and a RUNTIME word calling a UI word without a require, each refused. The rule governs the HBR2 package DAG; docs/package-build.md §4.5 keeps governing compile units. Files: tools/package-dag-lint-core.f (new), tools/package-dag-lint.f (new), tools/package-dag-lint-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load tools/package-dag-lint-test.f; bin/hb --load test/run.f. Depends: habu-land-hbr2-as-9b56de1a. Ownership: tools/package-dag-lint*.f. Lane: tim. Claim: unassigned.
