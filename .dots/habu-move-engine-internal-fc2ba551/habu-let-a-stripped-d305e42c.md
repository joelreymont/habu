---
title: Let a stripped program use genio-core.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:34:34.262754+01:00"
closed-at: "2026-11-18T17:40:34.912166+01:00"
---

Problem: src/lib/genio-core.f:211,214 calls KEY1 and RD-LINE, which only src/habu/repl.f defines, and the stripped-program build (tools/hb-build-core.f APP-BODY) loads no repl.f, so a program that requires genio-core.f and calls GENIO:KEY is rejected E-UNDEFINED KEY1 at genio-core.f:211:4 (test/fixtures/file-objects/kept-genio.f is built with --repl for this reason). Acceptance: such a program builds as a stripped program and runs. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-le.
