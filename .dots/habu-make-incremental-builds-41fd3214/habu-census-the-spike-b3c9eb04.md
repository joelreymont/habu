---
title: "Census the spike's print-only refusals"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:12:49.677782+02:00"
---

Problem: FILE-REFUSE is print-only in the lane 11 spike (o ~278-289): files print spike-refuse reason=<r> and still store objects (frame-triage.md (other)). Each reason must either refuse (reuse differs from a fresh cold build) or be deleted with its frame assert. Brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-census-refuse.md: part A (scope-at-load, scope-exit, scope-wordlist-unknown, tier, ambient-changed, capture-profile-unsupported) in carl-b1, part B (run-* reasons) in carl-b2, both on zxztprpo; U1 owns checker-ref-retired, ordered-dependency, checker-ref-unowned, cut-below-segment. Acceptance: census-A.md and census-B.md with a verdict and case per reason; verdicts feed the frame test-edit lane. Depends: none. Claim: agent=carl workers workspace=.jj-ws.noindex/carl-b1, .jj-ws.noindex/carl-b2.
