---
title: Make TRUST-DECL-ROW private
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:46:57.197751+01:00"
closed-at: "2026-11-16T04:07:35.510869+01:00"
---

Problem: src/core/checker.f TRUST-DECL-ROW, TRUST-DECL? and TRUST-DECL-REJECTED? are global; with C4's BYTE-SPAN? guard gone a program calling TRUST-DECL-ROW with a bad span crashes (E-MEMORY-FAULT in TOKFOLD, or rc 76) (re-review 9b finding 3). Acceptance: user source naming them is undefined, pinned in test/engine-private-test.f. Depends: none. Claim: agent=W147 workspace=.jj-ws.noindex/jerry-w147
