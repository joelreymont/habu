---
title: "Carry the captured REPL's defer rows into the product engine"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T02:57:55.417406+03:00"
---

Problem: on the stdin/AOT-seeded product lineage (the engine the recovery chain installs and tools/build-fixpoint.f's product) `require src/habu/debug.f` dies in src/habu/stepper.f S-INSTALL with `hook: non-certified definition: s-install at 'is'`: the AOT capture carries the captured REPL's checker SIGNATURES but not its DEFER rows, so `: T ( -- ptr u8 n ) RD-LINE ;` certifies while `[: RD-LINE ;] is REPL-READ` is refused, whereas a self-defined defer and a prefix defer (TYPE-DECL:TDECL-EVAL-XT) bind normally (double-load lane, 2026-09-17, LESSONS.md entry). A native-runtime engine has the rows and loads the debugger. Exposed, not caused, by ad21c4d4 (the engine no longer carries the trio, so a session must require it). Acceptance: the capture carries the window's defer rows through CHECKER-OWNER-ABI the way signatures travel, so `is` against a captured defer certifies on the product; test/debugger-resume.f, test/engine-stack-debugger.f and test/proc-pty.f pass on the chain-installed product and on the build-fixpoint product as well as on a native-runtime engine; a fixture pins `[: X ;] is <captured-defer>` on a product engine. Files: src/habu/aot-capture.f, src/core/checker-owner-abi.f, src/habu/habu2.f (EM-SEED-AOT), test/. Verify: the three debugger suites on /tmp/hz-dload/eng/hb-fx3-class engines; tools/bootstrap.sh full; fixpoint. Depends: none. Ownership: AOT capture. Claim: unassigned.
