---
title: LINKER place tests go through written programs
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:39.028481+02:00"
---

Problem: test/compiler/arm64-place.f:40 and arm64-place-unsafe-bad.f:24 open package LINKER to call its privates (ROUTINES, ROOT, REACH-ALL, LINK-ADDRESS, PLACE-ROUTINES, OFFSET@, KERNEL-END); under the boundary seal both exit 84. Acceptance: neither opens LINKER; their facts are asserted through LINKER:WITH/LINKER:WRITE on written programs, refusals as rejected programs, assertions with no observable twin deleted and listed. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-place.
