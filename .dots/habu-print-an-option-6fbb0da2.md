---
title: "Print an option's type as c:? shows it"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.800205+02:00"
---

Problem: 'c:?' on an option<n> local prints '@option.tag<n>' instead of the option's type (stuart, hbind fix, 2026-11-07). Acceptance: c:? prints the declared type option<n>; a case where c:? is tested. Files: src/core/checker.f. Verify: that case. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-chk3.
