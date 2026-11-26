---
title: "Give a model call's output and reasoning tokens"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T08:51:43.782185+01:00"
---

Problem: src/lib/net/model.f reads output_tokens and reasoning_tokens into the private TOKENS record but gives no public word for them; Tender's cost lines need them (kevin's request). Acceptance: MODEL:OUTPUT-TOKENS and MODEL:REASONING-TOKENS ( ^call -- n ) give an answered chat's or compaction's counts as INPUT-TOKENS does, 0 for a call stopped before its request; model-test cases show it. Depends: habu-zero-a-model-b49d73ce. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mtok
