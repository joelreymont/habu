---
title: Retire compiler context state on every exit path
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.504512+02:00"
---

CG-08. src/compiler/ir/context.f:337-351 retires registry state only after the callback returns normally; a caught top-level throw unmaps storage but leaves the context live, and after 64 caught failures WITH-CONTEXT returns E-IR-CTX-DEPTH instead of the body error. formal/Common/Storage.v records this leak as FINDING 1 instead of proving it impossible. Fix: the ownership primitive catches the body, unconditionally retires/truncates/releases all context state, then rethrows. Existing tests only catch inside a normally-returning outer context — add the top-level-throw path. Same rule applies to migrate RUN cleanup (dies with NMIGRATE) and TFX/SVX rewind (own dot).

Claim: agent=retire-ir workspace=.jj-ws/habu-make-ir-builder-bce0ba4a
