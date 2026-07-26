---
title: Retire raw vector surface
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:39:40.870543+02:00"
blocks:
  - habu-migrate-raw-vector-259d513e
---

Leaf 2, blocked on leaf 1: with zero external raw callers left, delete the legacy raw VEC-* surface above package VEC in lib/vector.f (the file's own comment schedules exactly this); every surviving internal word lands inside package VEC with short tails; VEC-RELEASE-STORAGE becomes a packaged private word calling the owned release (the held MEM:RELEASE rename artifact edits this exact line - coordinate: whichever lands second rebases, resolving toward BOTH changes). lib/vector-test.f: port the legacy sections that test still-existing behavior onto the typed API, delete the sections that tested deleted raw words - each deleted test named in the report, never silently dropped. Acceptance: the whitespace-body-edit probe on any word in lib/vector.f passes package-diff-lint (the wall probe from the parent dot); vector suite green on the typed surface; boundary-aware sweep proves zero raw VEC-* references tree-wide; both diff lints.
