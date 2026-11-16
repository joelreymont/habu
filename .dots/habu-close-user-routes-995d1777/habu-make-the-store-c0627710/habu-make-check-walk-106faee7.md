---
title: Make check-walk.f private to package CHECK
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:04:25.502408+01:00"
---

Problem: src/core/check-walk.f's global and CHECK-public words are callable by programs (store-feature2 S4). Acceptance: every check-walk.f word is private to CHECK except the S5/S5a/S5b surface; CHECK!, CHECK-CANDIDATE!, CHECK-QUIET-CANDIDATE! and generates: keep global forwarders; the engine builds. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-pub.
