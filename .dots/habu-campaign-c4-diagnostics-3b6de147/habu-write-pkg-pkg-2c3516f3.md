---
title: "Write PKG-PACKAGE's colon rejection once"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.197086+01:00"
closed-at: "2026-11-16T04:07:33.662905+01:00"
---

Problem: src/habu/interpreter.f PKG-PACKAGE (~:1033) checks a package name's colon with its own generic writer, duplicating the E-PKG-CONTEXT writers. Acceptance: one writer per cause. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
