---
title: "Tracker GC: close provably-landed dots"
status: active
priority: 2
issue-type: task
created-at: "2026-08-21T09:27:55.441027+02:00"
---

Problem: the tracker holds 2045 dots / 1120 blocker edges (dot-dep-lint baseline on master cd7d96c0), including work that has provably landed - so dot ready and the blocker graph no longer describe real open work. Acceptance: every closure carries a proof in its close-reason - either a commit id verified in master ancestry, or a re-run command plus rc showing the leaf's demanded behavior now holds, or an exact-duplicate cross-reference to the surviving leaf. No design questions, unlanded capability dots, actively-claimed dots, or dots blocked by an open dot are closed. Files: .dots only, no .f changes. Verify: HB_TMP=<private> bin/hb --load tools/dot-dep-lint.f exit 0 and 0 finding(s) per batch. Depends: none. Ownership: .dots leaf frontmatter.

Claim: agent=gc-1 workspace=.jj-ws/habu-effstore
