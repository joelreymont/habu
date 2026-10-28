---
title: "Give CALLBACK: a parses-through row"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T01:12:38.023272+02:00"
---

A3a2 finding: CALLBACK: (src/lib/ffi-callback.f ~440) reads tokens itself and has no parses-through: row, so tools/check.f --verify-only defers the rest of any file after a CALLBACK: (W-CHECK-DEFERRED at test/t0-install.f:79 in jerry-a3a sqrotpvq); nothing after it is verified. Candidate fix: a row like 'parses-through: CALLBACK: 0 ( ;CALLBACK )' in ffi-callback.f, checked against docs/forth-card.md. Workspace: none yet.
