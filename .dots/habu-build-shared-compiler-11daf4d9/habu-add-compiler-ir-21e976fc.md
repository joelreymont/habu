---
title: Add compiler IR IDs
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:38.085129+02:00"
---

Full context: PLAN.md IR-0.1 freezes the exact IR-RAW representation, CAST, pack/project/check APIs, -6600 error block, suite routing, and confinement rules. Implement public NEWTYPE ID families plus private checked representation authority; no context, arena, source, builder, codec, STRUCTURE, ENUM, or TRUSTED work. Acceptance: round trips and owner/bound negatives pass, wrong-family programs fail checking, no public raw converter resolves, refine/package/error/trust/filemap/suite gates pass. Files are exactly PLAN.md IR-0.1. Overlapping edits may proceed concurrently only in separate jj workspaces; before integration or push, fetch, rebase, reconcile every overlapping change, verify remotely valid ownership, and run the exact focused and publication gates on the reconciled tree.

Claim: agent=ir0 workspace=.jj-ws/habu-add-compiler-ir-21e976fc
