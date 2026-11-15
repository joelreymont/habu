---
title: Help a top-level take with its type and name
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:58.158172+01:00"
closed-at: "2026-11-16T04:07:35.874390+01:00"
---

Problem: re-review 7b of tmpkwxml. src/core/check-walk.f:3220-3229 TOP-OPEN-DROP-REJECT's help always says '`option<n>` in place of `option<a>`' and 'the effect that leaves the value', wrong for another family (myopt<a>) and for a library word's effect (OPTION:NONE drop); test/export-package.f:378 XPK-OPEN-DROP pins option only. check-walk.f:3189-3198's constant help uses ': NAME ( -- TYPE )' though DEF-CONSTANT (src/habu/interpreter.f:2460) has read the name and the row holds the type. Acceptance: both records render the value's type, the constant's help names the constant, the drop help holds whoever wrote the effect, and tests pin a non-option family. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
