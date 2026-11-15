---
title: "Make check-hook.f and top-row.f's publics private"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T21:50:03.589368+01:00"
---

Problem: src/core/check-hook.f and top-row.f are package CHECK, but 20 of their words are public (CHECK:TYPE-OP, CHECK:PRIMITIVE-FACTS, CHECK:TOP-TAKE, the ROWS-* words), so a program can call them. Acceptance: only CHECK:DEFINITION stays public in the two files; engine callers EXPORT what they call; the engine builds. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-w-hook.
