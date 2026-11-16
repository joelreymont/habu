---
title: Place a declaration record from stdin as from a file
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:37:39.110213+01:00"
---

Problem: a declaration record is placed at its token when the program comes from a file, but at the running token with a '= word:' note when it comes from stdin: on the installed hb /tmp/jerry-stdin/a11.f gives 1:11 as 'hb a11.f' and 1:64 as 'hb < a11.f'; r01.f 1:26 against 1:28. src/core/rejection.f REJECTION:HELD? does not accept the root stream's text, so the record falls back to its copy. Acceptance: each record from stdin is placed and noted as the same program's record from a file; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w198.
