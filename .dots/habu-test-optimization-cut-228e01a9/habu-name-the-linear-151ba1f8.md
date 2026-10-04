---
title: Name the linear refusal at a call
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T03:33:41.521940+03:00"
---

Problem: a linear (DEFLINEAR) value dropped through a type variable at a call is refused with the generic E-REJECTED / unknown_rejection under --json-errors, on the d37500ab engine and after lane 562 (its MO6 case in test/type-match-suite.f, an eliminator over a linear payload). The refusal is right; it carries no code or reason a reader can act on. Fix: the call-site refusal names a linear-drop code and reason at the call token, rendered in prose and JSON like the cell-rule reasons RS-CAPTURE names. Acceptance: MO6 and a plain call that drops a linear value through `a` render the named code in prose, --json-errors and --all-errors, seen generic first; no other refusal's code changes. After: 7ce778f8.
