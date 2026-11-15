---
title: Label a --verify-only record by the file that holds its token
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.787448+01:00"
closed-at: "2026-11-16T04:07:35.648887+01:00"
---

Problem: tools/check-verify-child-lib.f VERIFY-ALL (:453) sets the label to the subject's path while the engine still reads tools/check-verify-child.f, so rejection.f PLACE-IN labels a token in OWNED$ (the child's own bytes) with the subject's path. Acceptance: a token in the child's bytes is labelled with the child's path or with none; the subject's tokens keep the subject's path. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
