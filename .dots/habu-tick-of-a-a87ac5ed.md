---
title: Tick of a using-imported bare name crashes
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T11:59:43.485457+03:00"
---

Problem: ['] W on a bare name that a `using PKG` import resolved yields a token that SIGSEGVs at run time when it is executed through catch, while the qualified ['] PKG:W and a local wrapper (: W-ONE ( XML:reader -- XML:reader ) W ;) run. Measured by the catch-stale lane on its engine and on 3da80b23, before the lane: under `using XML`, on a reader opened on <a q='x'/>, ['] ATTR-RESET catch crashes and ['] XML:ATTR-RESET catch answers E-STATE. Reproducer: /home/joel/.cache/tmp/hazel-catch-stale/scratch/probe-xml-tick.f (STEP2). lib/xml-test.f CAPACITY-AND-STATE carries the wrapper ATTR-RESET-ONE with a comment naming the crash. Acceptance: the bare and the qualified tick of an imported word are the same xt, or the checker refuses the tick of an import alias by name - never a crash; the reduction names the layer that answered the wrong token (the kernel tick, the using resolver, or the checker's BTICK-TOK at src/core/checker.f:13183 trusting it); a regression row ticks an imported bare name and runs it through catch in the suite that owns tick; the probe passes, and ATTR-RESET-ONE and its comment go. Files: the kernel ['] (src/habu/), the using import resolver (src/core), src/core/checker.f BTICK-TOK, lib/xml-test.f, the tick suite. Verify: the probe with the lane env line, the tick suite, lib/xml-test.f. Depends: none. Ownership: src/core/checker.f BTICK-TOK, the kernel tick. Claim: unassigned.
