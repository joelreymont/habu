---
title: Reject a target no linker part places at the command line
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:39:33.862062+01:00"
closed-at: "2026-11-16T13:54:13.899141+01:00"
---

Problem: tools/hb-build-lib.f:153 accepts --target linux-x86-64, but no linker part places x86-64 code, so such a build stops late at src/habu/link-band.f NO-PART (E-LINK-MACHINE). Acceptance: hb-build rejects the target where it reads it, as REFUSE-WASM does, with one record, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w211.
