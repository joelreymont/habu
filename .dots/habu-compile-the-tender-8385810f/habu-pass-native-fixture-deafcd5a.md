---
title: Pass native fixture paths as complete arguments
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-13T14:51:13.081324+03:00\""
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own test/nf.fs HB_TMP paths/process invocation. Quote/escape complete shell arguments including redirections and size encoded combined command, or reuse structured argv if available. Space path fails today;97-byte root fits individual128 but exceeds command256. Preserve per-run isolation. Verify spaces/metacharacters/quotes, long valid root, clear overflow refusal and concurrent real gforth/native fixtures. No new runner framework.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Claim: Sol native-fixture-argv lane; Cedar independent review/integration. Scope: named fixture argv/path handling and buffer sizing, preserving real build behavior.
