---
title: Delete the capture-prepare chain when x86-64 and Wasm use the linker
status: open
priority: 4
issue-type: task
created-at: "2026-11-09T17:59:25.302424+02:00"
---

Problem: only the deleted native-runtime.f called NCOMP:CAPTURE-PREPARE (compiler.f:820) -> NBACK:PREPARE -> each backend's PREPARE (x86 passes.f:233), LBUF-CAPTURE-PREPARE (cell-effects.f:40), XREF-SNAPSHOT-PREPARE, ENV-SNAPSHOT-PREPARE and ENGINE-ORIGIN:CAPTURE-PREPARE/-END; they stay while x86-64 and Wasm capture. REG-PERSIST-MOVE, REG-POOL-PERSIST and the *-SNAPSHOT-PERSIST words stay for CHECKER-CAPTURE-PREPARE (Wasm capture). x86-64 notes for its move onto the linker: kernel-x64.f:4010 still carries the seed-ndict! body and the x86 kernel tests still call it (arm64 deleted the word; test/prof-rewind-child.f uses ndict!); aot-x64*.f cite deleted arm64 aot files; test/x86-64-kernel-ffi.f:5 cites the deleted test/ffi-stub-child.f SUM10. Acceptance: when x86-64 and Wasm build through the linker (docs/design.md "Machinery being replaced"), the chain, seed-ndict! on x86-64 and the persist words go, with their citations. Files: those named. Verify: rg each name over src tools test; each target's build rebuilds itself byte for byte. Depends: x86-64 and Wasm on the linker (no work before arm64 lands: Joel 2026-11-09, "forget about x86, get arm64 done."). Ownership: those files. Claim: unassigned.

Note 2026-11-10 (bob): src/habu/aot-x64.f and src/habu/patch-x64.f are deleted (habu-nothing-loads-src-66ea20d9); nothing loaded them.

Moved from habu-delete-the-dead-c212d5a2 (words only the x86-64/Wasm capture path keeps): AOT-RUNTIME:COMPLETE?'s NATIVE-RUNTIME clause (src/habu/aot-runtime.f:49; a capture holding CHECKER-REG now dies 'incomplete captured runtime', :10); AOT-CAPTURE MAP-N, MAP-NAMED, MAP-NAME$, DECLARED-IN, SIG-KNOWN, SIG-EXEMPT and CAPTURE-WITH-NAMES (src/habu/aot-capture.f); AOT-SPAN PUBLISH (src/habu/aot-decl.f:343, habu2.f:7635, called at habu2.f:7726, no arm64 reader). Each goes with the capture path, after an rg for other callers.
