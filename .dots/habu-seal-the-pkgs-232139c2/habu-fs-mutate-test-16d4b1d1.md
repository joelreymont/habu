---
title: fs-mutate-test.f and fs-test.f stop reopening FS
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:39.007360+02:00"
closed-at: "2026-11-12T18:03:36.849243+02:00"
close-reason: landed
---

Problem: src/lib/fs-mutate-test.f:20-45 and src/lib/fs-test.f:23-29 reopen baked package FS to forward 29 privates; under the boundary seal both exit 84. Acceptance: neither opens FS; each forwarded word is replaced by a stated OS constant, the public word that does it, or an assertion through public words with real files, or goes as unreachable state; both pass. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-fs-b.
