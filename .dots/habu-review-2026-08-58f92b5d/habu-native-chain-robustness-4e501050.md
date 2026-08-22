---
title: native chain robustness defects
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.911900+02:00"
---

Problem: (a) migrate.f:536-546 RUN sets M-OPEN before IN-CONTEXT and only WORK is caught, so a throw from NABI:BINDING or IR-CTX:WITH-CONTEXT leaves M-OPEN=1 and every later migration dies E-NMIGRATE-STATE; (b) elaborate.f:3543-3560 TAIL-SCAN lacks the MOPERAND? filter BACK-CALL? applies, so a trailing 'is FOO' sets TAIL-NEED and TAIL-CK refuses with the wrong code; (c) migrate.f:449-452 EMITTED re-allocates without checking A64RA:PLAN-N 0= so a residual spill surfaces as E-A64RAV-REGISTER; (d) emit.f:1684-1719 SCAN-ADDR-SITES re-scans for runs of exactly four same-register lanes and two adjacent address chains sharing a register (legal per regalloc.f:691-697) give E-A64EMIT-ADDR - record the site in APPEND instead; (e) tools/judge/chain.f:154-157 PUBLISH answers 0 for a name that already resolves against its own comment; (f) judge/traffic.f:49-64 masks admit pre/post-index and register-offset forms as stack traffic; (g) a64-effect.f:43-48 header says no float class. Acceptance: each fixed with a negative test or refuted. Files: as listed. Verify: test/compiler suites. Depends: none. Ownership: native chain. Claim: unassigned.
