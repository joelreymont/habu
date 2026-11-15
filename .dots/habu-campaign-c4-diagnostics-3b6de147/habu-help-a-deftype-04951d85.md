---
title: Help a DEFTYPE family base by a form it takes
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:01:16.022861+01:00"
closed-at: "2026-11-16T04:07:35.970291+01:00"
---

Problem: a parametric family inside a longer DEFTYPE base (DEFTYPE dx ^option) gets the help 'give the family its type arguments, as in box<n>', but its edit DEFTYPE dx ^option<n> is rejected as 'a deftype base is one name' (src/core/structure-decl.f SD-ARGS-FIX$): the help names a fix the loader rejects. Acceptance: the help names an edit the loader accepts, pinned by deftype-suite. Depends: none. Claim: agent=W162 workspace=.jj-ws.noindex/jerry-w162.
