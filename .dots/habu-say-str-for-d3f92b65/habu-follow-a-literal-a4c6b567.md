---
title: Follow a literal through >str to its loader
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:20:02.489231+01:00"
---

Problem: included and required take str, so unmarked text can only write s" a.f" >str included; verify-source.f's loader discovery and SRCLOC's place record drop the literal after one token, so the check rejects E-LOADER-FORM and a missing file is placed at included. Acceptance: >str right after a literal keeps it as the loader's operand, statically and at run time. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p6fog
