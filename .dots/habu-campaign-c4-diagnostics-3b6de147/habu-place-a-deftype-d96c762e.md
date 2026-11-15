---
title: "Place a DEFTYPE's name records at its name"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T02:38:47.465212+01:00"
closed-at: "2026-11-16T04:07:35.100582+01:00"
---

Problem: src/lib/type/deftype.f MINT-OVER and src/habu/verify-source.f RECORD-DEFTYPE hand the wrap (TYPE-DECL:SD-WRAP) the folded tail from a scratch buffer, so a record of a DEFTYPE's name is placed on its base with a word note: a second 'DEFTYPE DUPX str' is marked under 'str' with '= word: dupx'. Acceptance: the load and check.f place each record of a DEFTYPE's name at the name as written, with no word note. Depends: none. Claim: agent=W153 workspace=.jj-ws.noindex/jerry-w153
