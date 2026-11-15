---
title: Report a definition rejection once, at its place
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.342457+01:00"
---

Problem: a rejected body prints src/core/render.f DIAG-PRINT's line with no place or source line, then src/core/check-hook.f's 'hook: non-certified definition' and src/compiler/native/definition.f REPORT-REFUSAL's 'compiler: cannot compile'; E-UNDEFINED prints twice; the suggestion is generic. Acceptance: every definition rejection prints one diagnostic through REJECTION (src/core/rejection.f) with expected/found/declared/inferred notes and a specific help (E-UNDEFINED names the packages holding the name); the hook: and compiler: lines are gone. Depends: none. Claim: unassigned.
