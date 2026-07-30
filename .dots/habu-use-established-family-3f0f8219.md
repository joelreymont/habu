---
title: Use established family in payload token test
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T21:14:09.412420+02:00"
closed-at: "2026-07-30T04:58:35+02:00"
close-reason: Absorbed into habu-guard-declaration-event-2b0f3e79 because the test becomes red only when that same-generation ownership invariant lands.
---

Why: test/enum-decl-suite.f TEST-PAYLOAD-VIEW opens a second declaration-event frame with the still-provisional outer family only to prove that the outer token is not current. The declaration ownership guard correctly rejects that unrelated duplicate-family bind before the token-scope assertions run. Owner: test/enum-decl-suite.f only. Exact result: bind the nested frame to the already-published epwide family stored in PFB, and use that same family for the nested frame payload query. Keep the outer RC/FID query unchanged so it still proves E-DEV-TX 7161 for a non-current token; keep the nested query at E-DEV-PAYLOAD 7172 for a current token without a variant. Acceptance: the test still fails if token validation is removed or if the current empty frame exposes payload; it no longer requires two live declarations to claim one provisional family; every other payload view assertion is byte-for-byte unchanged. Forbidden: production edits, new family or event seam, raw state, changed error codes, reduced assertions, or legacy declaration changes. Smallest check: bin/hb --load test/enum-decl-suite.f. Depends: none. Claim: agent=enum_payload_scope workspace=.jj-ws/habu-use-established-family-3f0f8219.
