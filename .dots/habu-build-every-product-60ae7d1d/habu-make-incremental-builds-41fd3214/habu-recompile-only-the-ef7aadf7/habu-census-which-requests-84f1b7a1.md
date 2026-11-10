---
title: "Census: which requests of an engine build are clean cuts"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T15:16:08.774364+02:00"
closed-at: "2026-11-10T15:26:33.835758+02:00"
close-reason: "measured: all 168 requests of a cold engine build are clean cuts; hb's own require records each and NEST reads hb's log; report .out/reports/census-requests-clean-cut.md, answers in .out/design-requester-by-words.md"
---

Problem: the requester design (carl-layout/.out/design-requester-by-words.md) compiles a file from one of its requests onward, sound only at a clean cut (top scope, empty stack, no open definition, cursor in the requester's source, a loader word dispatched); it could not determine which definition of require a build's file runs nor whose event log NEST reads, and no count of clean and unclean requests exists. Acceptance: a report with the path:line chain for who records a build file's request and a table of every NEST call of a cold engine build with the five facts. Files: none committed (temporary prints). Verify: the report's table. Depends: none. Ownership: carl. Brief: carl-layout/.out/briefs/census-requests-clean-cut.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-req.
