---
title: Restore package state on a caught throw
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T10:29:25.659987+03:00"
closed-at: "2026-10-01T12:01:48.649492+03:00"
close-reason: "done: product 7d0ce3a7 (gen1 = gen2 from host 5c32d0dd) passes test/using-test.f (with the unauthenticated-scope case), test/outer-interpret.f (133 cases agree, PACKAGE-RECOVERY on both loops), proc-pty PTY-PKGFLOOR-RECOVERY, checker-verify-pkg-scope, whitebox checker-replay-pkg-state and the Gforth bootstrap check; base 5c32d0dd fails every new restore case (7136, 104, Habu loop 75 and 70, REPL USING-OUTER)."
---

Problem: throw recovery and the checker's package mirror disagree after a caught throw inside an evaluated buffer. (1) Measured on engine e11cab55 with `test/using-test.f`'s `UCE-CATCH` (INCLUDE-EVALUATE under catch) and `VS-CATCH` (VERIFY:SOURCE-BUF under catch): `s" package UQF" UCE-CATCH` gives 0, `s" ;package AW drop" UCE-CATCH` gives 70, and from then on every `s" 1 drop" VS-CATCH` throws 7136 (E-PKG-CONTEXT), also after a clean `UCE-CATCH` in between. Controls on the same engine: `;package` alone in the second buffer (VS 0); a throw inside the package without `;package` (VS 0); no package (VS 0). The trigger is a buffer that closes a package an earlier buffer opened and then throws. (2) From the UC lane (`habu-refuse-closing-an-2bfefd76`): recovery after a throw (`LEVALREC`, REPL recover) does not save or restore the package's using floor (`USE-PKG-SAVE-CELL`); evaluated code that closes package P, opens Q and throws leaves P with Q's floor. (3) The Habu loop leaves a package opened by a throwing file open, where the engine closes it.
Acceptance: after a caught throw the engine's package state (open package, using depth and floor) is the one the buffer entered with, the checker's package mirror matches it, and the Habu loop agrees; the three measured sequences above give the engine's answers on all routes, with no 7136.
Files: `src/habu/habu2.f` (eval frame PKGSNAP, `LEVALREC`, REPL recover), `src/core/checker.f` (package mirror on recovery), `src/habu/interpret.f`/`src/habu/packages.f`, `test/using-test.f`, `test/outer-interpret.f`.
Verify: the sequences above on the engine route and through the Habu loop; rebuild, chain, gate.
Ownership: krait (Intel lane).
