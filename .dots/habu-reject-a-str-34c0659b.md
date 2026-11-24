---
title: Reject a str bound to two locals with a diagnostic, not a render death
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:09:51.137546+01:00"
---

Problem: /tmp/jerry-p1/strlocal-repro.f (': S ( -- str ) s" ab" ; : T ( -- ) S {: a:^ u :} ;') makes the engine die with 'render: a rejection with no code' (src/core/render.f:1633 DCODE), rc 76; tools/check.f reports E-VERIFIER-ENDED, rc 69. Acceptance: the program is rejected with a coded Rust-style diagnostic at the locals, on the load and in check.f, and a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-strlocal.
