---
title: Help a width in a DEFTYPE base to a NEWTYPE
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T02:53:30.098914+01:00"
closed-at: "2026-11-16T04:07:35.864725+01:00"
---

Problem: DEFTYPE dx ^width is rejected (src/core/structure-decl.f FIELD-FAM?, UNSTORED-REASON) with help 'declare it `n`', whose edit DEFTYPE dx ^n is rejected again as 'a deftype base is one name'. Acceptance: the help names an edit the loader accepts (NEWTYPE dx ^n), pinned by deftype-suite. Depends: none. Claim: agent=W163 workspace=.jj-ws.noindex/jerry-w163.
