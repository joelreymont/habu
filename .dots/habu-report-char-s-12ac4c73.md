---
title: "Report [char]'s operand once under --all-errors"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:07:10.299970+01:00"
---

Problem: under `tools/check.f --all-errors`, a top-level `[char] a` gives the body-keyword record for `[char]` and then a second record for `a`, with help "`a` is public in `BODY-ITEMS`": the walk goes on to the keyword's operand as a top-level token. Acceptance: --all-errors gives one record for `[char] a` (and for each body keyword that reads an operand) at top level; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w199.
