---
title: "Name a code by ERROR:NAME$ alone"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:12.268837+01:00"
---

Problem: src/lib/net/model.f:2890 CODE$ maps each MODEL code to its name by hand (called at model.f:3822), and tools/lint/text.f:298-327 keeps a code-name table of its own (LINT-CODE-NAME+, filled by tools/lint/token.f:8 and tools/lint/intern.f:14, read by tools/lint/lib.f:45-49); once those codes are ERROR:CODE rows, src/lib/error.f:854 ERROR:NAME$ answers the same names. Acceptance: CODE$ and the lint table are deleted and their callers ask ERROR:NAME$. Depends: none. Claim: unassigned.
