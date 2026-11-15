---
title: Give the right help for three rejections
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.591260+01:00"
closed-at: "2026-11-16T04:07:36.010443+01:00"
---

Problem: a stray `;` gets help that does not name `:` (interpreter.f:2218); a word that calls itself gets E-UNDEFINED with no hint of `recurse`; a top-level underflow gets the definition's help (render MD-REASON$/MD-SUGGEST$ under TOP-STEP). Acceptance: each record's help says how to fix that case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
