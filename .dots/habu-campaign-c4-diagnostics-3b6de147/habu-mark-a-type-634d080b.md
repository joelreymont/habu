---
title: Mark a type written across lines within its line
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T12:54:17.404282+01:00"
closed-at: "2026-11-16T13:54:13.872318+01:00"
---

Problem: an application type written across lines (STRUCTURE field `option<n,⏎  n>`, wrong arity) prints line 1 with carets past its text: src/core/rejection.f:716 SHOW-CARETS clips the caret count only when the line is longer than WIDTH, never at the line's end. The load prints 14 carets, check.f 12: check.f's token omits line 2's leading blanks (cause not found). Probes /tmp/jerry-w202/rr/p/, log /tmp/jerry-w202/rr/probes-ml.log. Acceptance: the record's carets end at the marked line's end, and the runner and check.f mark the same span. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w230 (carets), .jj-ws.noindex/jerry-w231 (span cause).
