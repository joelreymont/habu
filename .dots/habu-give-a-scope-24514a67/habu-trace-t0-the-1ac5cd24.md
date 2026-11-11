---
title: Trace T0, the arm64 kernel and tier 1 for quotations that read locals
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T07:54:59.000827+02:00"
---

Read-only census for the approved design (DECISIONS.md 'quotations read locals'): how T0 compiles a quotation and lays out locals, the emitter interface, the arm64 frame and the registers free at the kernel's indirect calls, how a kernel edit reaches a built engine, who else calls an xt, where the other compilers would refuse the form. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/quot-census-t0.md. Acceptance: a report answering its seven questions with path:line and the probes run; no tracked file changes. Verify: the report. Depends: none. Ownership: none (read-only). Claim: agent=jerry workspace=.jj-ws.noindex/quot-census-t0.
