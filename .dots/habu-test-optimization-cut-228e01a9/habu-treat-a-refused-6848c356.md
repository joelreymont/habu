---
title: Treat a refused does> clause like a refused body after the pre-pass
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T13:24:10.225863+02:00"
---

Problem (lane 308 r4-doesname, 8d6da143): (a) under tools/check.f --all-errors a refused does> clause leaves the created word unknown, so each caller adds a follow-on E-UNDEFINED for it (fixtures m1g/m1f in $HOME/.cache/tmp/kestrel-r4-doesname/); a refused definer body does the same. (b) The clause's JSON record has no declared_effect: DIAG-JSON prints it only when SGSEEN is set and the clause signature is parsed outside the name/signature scan. (c) The engine's own --load path still reports a refused clause only as 'does> at file:line' (CHECK-DOES! is called from habu2.f C-CALL-CHECK-DOES with no name). Acceptance: --all-errors reports the refusal once and no follow-on E-UNDEFINED for the created word or the definer (recover with the declared effect as a refused colon body's callers are handled, or state the measured reason that is wrong); the clause record carries declared_effect like a body's; the load path names the clause <definer>;does as check.f now does; each seen failing first. Files: src/core/checker.f, src/habu/verify-source.f, src/habu/habu2.f, tools/check-test-lib.f.

Review 326 (of 8d6da143) adds: (a) the cascade's cause: check-all-errors-core.f:493-498 keeps a reject's declared signature so later callers do not cascade, but src/habu/verify-source.f:609-625 VERIFY-DOES withholds DEFINER-RECORD when the clause is refused; edit: bind the clause verdict and record when `v 0<>` or (`v 0=` and MULTI-ERR-MODE?), i.e. `sig sigu DEF-NAME-A @ DEF-NAME-U @ VERIFY-DOES-BODY {: v:n :} v 0<> v 0= MULTI-ERR-MODE? and or def 0<> and IF sig sigu DEFINER-RECORD THEN EXIT` (fixtures \$HOME/.cache/tmp/kestrel-r4-rev326/fx/followed-clean.f, plain.f). (b) DOES-REPORT's `DIAG-QUIET @ 0= IF DIAGXT THEN` (src/core/checker.f ~:20095) duplicates CHECKER-CHECK-REPORT (~:19850): call it. (c) DOES-REPORT's comment "CHECK leaves that verdict to its callers outside JSON" is true of the engine, not of check.f's pre-pass (DIAG-JSON on there): state which path.
