---
title: "itick: refuse internal ticks in unchecked native bodies"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:27.501911+02:00"
---

Problem: under 0 set-check, T0 compiles ['] int-mark (t0.f DO-TICK lacks the INT-CALL? check NAME-TOKEN has) and hands unchecked code an internal engine xt, rc 0; tier 1 exits 67 (uncaught -8651); the JIT exits 70. Acceptance: both native tiers exit 70 naming the word; TRUSTED: behaviour unchanged; case beside test/owner-access.f INTERNAL-ONLY. Repros /tmp/jerry-big/uc/itick.f, itick1.f. Brief /tmp/jerry-big/t0-itick-brief.md. Base pvovztnu (uref). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-itick
