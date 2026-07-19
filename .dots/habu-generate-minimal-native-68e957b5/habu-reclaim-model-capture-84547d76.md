---
title: Reclaim model capture words
status: open
priority: 1
issue-type: task
blocks:
  - habu-check-records-go-4f62cd2e
created-at: "2026-07-19T22:53:00.455721+02:00"
---

maki/cad.f:265-269,664-676,683-691 mints one unique %MDLCAP<n> checked definition per MODEL:, compiles it only to execute once for IR capture, then retains the definition and JIT code forever. Exact census finds no later %MDLCAP consumer. The suite's dictionary-wall comments and new process slicing hide this monotonic leak instead of removing it. Provide a checked anonymous or temporary capture-compilation abstraction that performs the same checker/compiler work, executes once, and transactionally reclaims dictionary, JIT, type, package, and checker registry state after capture; preserve intentionally published model IR. Do not use raw cp/ndict FORGET while stale CHECK! records survive. Depend on habu-check-records-go-4f62cd2e unless an anonymous checked primitive avoids registry publication by construction. Add identical MIR and diagnostics goldens, invalid-body rejection, nested capture, throw rollback, repeated model declaration, registry integrity, and exact definitions/JIT/DATA/CODELEN per model before/after. Require flat retained compiler state across repeated captures and measure compile/capture time. Files: maki/cad.f, focused compiler/checker abstraction and tests only.
