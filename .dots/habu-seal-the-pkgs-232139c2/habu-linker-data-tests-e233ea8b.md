---
title: LINKER data tests go through written programs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:39.018035+02:00"
closed-at: "2026-11-12T18:03:36.879198+02:00"
close-reason: landed
---

Problem: test/compiler/arm64-link-data.f:42 and arm64-link-data-bad.f:22 open package LINKER to call private LINK-STRIPPED and read its buffers; under the boundary seal both exit 84. Acceptance: neither opens LINKER; their facts are asserted through LINKER:WITH/LINKER:WRITE on written programs (as test/compiler/arm64-link-run.f), refusals as rejected programs, assertions with no observable twin deleted and listed. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-link-data.
