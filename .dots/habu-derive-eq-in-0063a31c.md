---
title: "Derive eq in time linear in a sum's arms"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T17:48:43.742031+01:00"
---

Problem: loading a SUMTYPE with DERIVE eq takes about n^4 time in its variant count (one n per VARIANT: 70 variants 1.73 s, 100 variants 6.82 s on the engine of 'Build derived bodies from items'; DERIVE hash at 100 takes 0.04 s). src/core/sumtype.f TDGEN-EQ-DIAG emits an outer MATCH whose every arm holds an inner MATCH over every variant, n^2 arms, and the cost per arm grows with n. Reproducers: /tmp/carl-dcap/gen/e50.f, e70.f, e100.f. Acceptance: a 200-variant SUMTYPE with DERIVE eq hash loads in about the time DERIVE hash alone takes and answers EQ right (true for a value and itself, false across variants), as a type-decl test. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-eqh.
