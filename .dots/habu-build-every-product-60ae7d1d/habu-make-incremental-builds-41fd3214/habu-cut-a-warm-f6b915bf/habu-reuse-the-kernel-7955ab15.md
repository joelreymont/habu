---
title: Reuse the kernel object while its inputs hold
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T06:37:44.754853+02:00"
closed-at: "2026-11-13T09:20:10.784634+02:00"
---

Problem: every engine, REPL and native app build runs a whole kernel build (tools/hb-build-core.f KERNEL-BUILD :318, called at :338, :348, :358) though its files are unchanged: 374 ms of a warm engine build, and the same in every Zen, Tender and Maki warm build. Design: .jj-ws.noindex/jerry-krn/.out/design.md (carl's, refreshed against master). Acceptance: a build whose kernel inputs all hold adopts the last kernel object and runs no kernel build; an edit to a kernel file or the tool reruns it; warm equals cold; warm build time measured before and after. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-krn, then jerry-krn2.
