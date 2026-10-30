---
title: "fix pd-latch: abandon a counted TRUSTED: refusal by a checker latch"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.805780+02:00"
---

Problem: definition.f VERDICT-CK abandons a TRUSTED: definition when NAME$ NDICT:SPELL-ARITY = ARITY-NONE, a stand-in for 'the multi-error checker reported and counted its signature' (checker.f USIG-ADD-BAD, TRUST-STALE); SIG-UNRES returns with no report or count, so the comment is false for it. Acceptance: the checker sets a latch where it counts the refusal; VERDICT-CK reads it; comment states it. Workspace: .jj-ws.noindex/jerry-pdlatch, brief /tmp/jerry-big/fix-pdlatch-brief.md. Claim: jerry.

Worker launched 2026-10-30 23:00 in .jj-ws.noindex/jerry-pdlatch on lqwprrlr.

First worker handed off before building (handoff /tmp/jerry-big/pdlatch-handoff.md); a fresh worker continues it in the same workspace from 2026-10-30 23:45.

Finished: poomoouv ab9a0f39 in jerry-pdlatch, rows program-diagnostics, native-file-replay, native-file-state, check-cli-boundary, check-verify green; under review with vktznxps.
