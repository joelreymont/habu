---
title: Delete the executable-build scope
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.314895+01:00"
---

Problem: EXECUTABLE-BUILD:WITH (src/lib/executable-build.f) has no caller in the tree or the consumers; tools/hb-build-core.f only bakes the file in, so BUILD-DEPTH-CELL never leaves 0 and the enter and leave primitives, the tick-query bypasses in interpreter.f and habu1.f and the x86 BUILD-SCOPE test serve nothing; src/habu/prims.f calls WITH the checked caller every other scope calls, which is false. Acceptance: the scope, its primitives, cell, bypasses and tests are deleted after rg finds no other reader, and the engine builds cold and warm-equal. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xbuild.
