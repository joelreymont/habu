---
title: "Check past a CALLBACK: definition"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T13:12:30.498186+01:00"
---

Problem: src/lib/net/curl.f:920 `CALLBACK: PIECE-WRITE ... ;CALLBACK` has no parses: row, so tools/check.f defers it (W-CHECK-DEFERRED) and leaves every later stretch to the run; check.f --verify-only answers 0 for src/lib/net/model.f while the model suite's load rejects a stale read there (E-STALE-READ, model.f EMBED-PARSE typed bind). Acceptance: check.f --verify-only checks CALLBACK: definitions and what follows them, and rejects that typed bind. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-cbchk.
