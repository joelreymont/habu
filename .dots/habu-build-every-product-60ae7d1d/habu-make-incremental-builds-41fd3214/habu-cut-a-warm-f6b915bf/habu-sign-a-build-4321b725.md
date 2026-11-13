---
title: "Sign a build's output only when it changed"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.174786+02:00"
closed-at: "2026-11-13T05:34:49.422657+02:00"
---

Problem: src/lib/codesign.f:348 looks up the signing identity and runs codesign three times per build (check the team signature, sign, verify), 111 ms of a warm engine build, also when -o already holds a team-signed image of the same code; the team check always fails there, as LINKER:WRITE has just signed the build ad hoc. Acceptance: a build whose code and map equal a team-signed -o leaves -o untouched and runs only the team check on it; a changed one is signed, verified and installed as now. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-cs.
