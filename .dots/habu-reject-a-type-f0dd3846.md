---
title: Reject a type too deep to walk
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.521106+01:00"
---

Problem: 4097 nested quotation locals make the checker exit rc 76 with 'checker: term walk too deep (cyclic term)' (src/core/checker.f:3131-3132), though the type is not cyclic; 4094 pass. A self-imposed depth limit stops a sound program and its message is wrong. Acceptance: the program is checked (no fixed walk depth) or rejected as a rejection naming the place; a test pins it and fails first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-deep.
