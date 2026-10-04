---
title: "Name the compiler's empty failure report"
status: open
priority: 4
issue-type: task
created-at: "2026-10-03T16:39:51.771214+03:00"
---

Found by lane 455 (e81287d4, c5fd9f2c): src/compiler/native REPORT-FAILURE prints 'ncomp: cannot compile ' with an empty name for a refusal raised before the tape name is kept (E-NFEED-CUT, and any feed refusal before the name), and lib/errors.f:519's compiler-region map lists -8520..-8599 as unassigned although -855x (A64RAV/HIR/A64IR/A64EMIT) and the native publication block -8560..-8579 are minted. Acceptance: the report names the definition (or says it has none yet) for every refusal path, seen failing first; the region map matches the minted codes; error-code-lint 0.

Widened (review 485 refusalexit): src/compiler/native/compiler.f:697 `REPORT-FAILURE RETRACT rc throw` renders `ncomp: cannot compile ...` and then rethrows the native compiler's code bare, so a rendered compiler refusal exits 67 (uncaught) while some compiler refusals already exit 70 (census 7b3f85de: s1c 'ncomp: cannot compile P1' rc 70). Acceptance adds: a rendered native-compiler refusal exits 70 at --load, consistently with rendered checker refusals (4d copy 6 CHECKER-REFUSE / REFUSAL-CELL), with a test pinning the rc.
