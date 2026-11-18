---
title: "Show a parameter's declared type in its record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T07:38:49.525258+01:00"
closed-at: "2026-11-18T11:24:41.997721+01:00"
---

Problem: a quotation parameter whose input facts the check widens by union prints a type its signature does not state: /tmp/jerry-nested/q3.f declares ( [ [ -- ] -- ] -- ) but its caller's record shows expected [ [ -- renders ] -- ]. Acceptance: the record names the declared type and says which fact the body's use adds, or the widening is shown as the help; a program-diagnostics test fails first. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-qdiag.
