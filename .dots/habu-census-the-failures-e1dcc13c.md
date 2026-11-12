---
title: Census the failures that print no name, values or place
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T13:24:11.128063+02:00"
---

Problem: the null stack's suite printed 'hb: uncaught throw code -8283' for 17 tests: E-HIR-CAP, with no name, table, count or place. Joel wants errors as complete as Rust's and asked for a check of the codebase for similar issues. Acceptance: every failure report that prints a bare code, drops the values its check compared, or prints nothing is listed with path:line, its output and its reaching path, marked covered by an existing dot or new; new ones become dots. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-errs.
