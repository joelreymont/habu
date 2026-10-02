---
title: Emit a second target through NSHADOW
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.706966+03:00"
closed-at: "2026-10-01T08:30:00+03:00"
close-reason: test/compiler/shadow.f passes on the rebuilt engine and it rebuilds master to b4e05778 (host swap)
blocks:
  - habu-bind-x86-host-4485acdd
---

Problem: a cross-build must execute the prefix on the host while emitting for the target: `LOAD-TARGET` (`tools/native-build-core.f:186-225`) runs every declarer and immediate on the host, and the host cannot run x86 bytes; re-lowering from the tape later is rejected because `BIND-PRIOR` (`compiler.f:381-419`) resolves names against the dictionary at compile time. Two rules bind the second chain: `NBACK:ROW@` resolves the row from `IR-CTX:BINDING@` (`src/compiler/native/backend.f:131-132`) and `X64SEL:MACHINE-CK` refuses another machine's contract (`select-x64.f:2068-2069`).
Acceptance: `src/compiler/native/shadow.f`: `OPEN ( CBIND:binding -- )`/`CLOSE`; the driver freezes once (`FREEZE ( ctx builder -- module )`: `IR-BUILD:FREEZE-INTERIM` plus the target-free loop fold now inside each row's `SELECT`, `src/arch/arm64/passes.f:120-137`, `src/arch/x86-64/passes.f:156-168`); rows take `SELECT ( ctx module -- module )` with `BIND-SOURCE` over the frozen module (both `passes.f` files); the driver retires the interim after the last selector; the shadow chain runs in a context nested inside the definition context and bound to the shadow binding (`IR-CTX:WITH-CONTEXT`, `context.f:599`); builders are minted in the context that mutates them (`OWN-CK`, `build.f:413-422`; frozen reads cross contexts, `build.f:407-411`); the shadow's bytes and rows are copied into `NSHADOW`'s `DYNAMIC-BUFFER` before its context leaves; each row's `RETIRE`/`RELEASE` runs in its own context; `NPUB:PUBLISH-PENDING(-DOES)` calls `NSHADOW:PUBLISH ( idx -- )`, recording the shadow entry per pending record (the `;does` companion records through function offsets); no shadow open gives a byte-identical engine (chain); `docs/x86-64.md` gains the dual-emission section. First step: check whether `A64SEL:BIND-SOURCE` can read opcode identities from a frozen module without a builder (not determined by the design).
Files: `src/compiler/native/shadow.f`, `src/compiler/native/compiler.f`, `src/compiler/native/publish.f`, `src/arch/arm64/passes.f`, `src/arch/x86-64/passes.f`, `test/compiler/shadow.f` (host-side: compile with an x86 shadow, read the map), `docs/x86-64.md`.
Verify: spark `bin/hb --load test/compiler/shadow.f`; rebuild; chain gen2==gen3; gate.
Depends: habu-fill-nemit-from-d8c030e4 (P3), habu-bind-x86-host-4485acdd (K12), habu-record-symbolic-x86-10037f07 (C6). Serialise on both `passes.f` files, `compiler.f` and `publish.f`.
Route: Alder (shared: src/compiler/native/shadow.f (the ARM64 host compiler loads it), src/compiler/native/compiler.f, src/compiler/native/publish.f, src/arch/arm64/passes.f, test/compiler/shadow.f).
Ownership: krait (Intel lane).
Claim: unassigned.
- The shadow emission compiles every definition through the x86 rows, so it needs schema-fixed operands pinned (habu-pin-schema-fixed-1983d191).

Lead note (2026-09-30, from P3's design): `NEMIT` holds one emission (`emission.f:122` refuses an open over sealed rows), and after P3 the x86 row fills it in `EMIT`, so the shadow's rows must be copied and retired before the primary's `NEMIT:OPEN`. `X64PASS:EMIT` always places, so X1 adds the unplaced path. X1 takes the wrong-target refusal: `NEMIT:OPEN` takes the emitting `CTARGET:arch`, and NPUB refuses any arch other than `NABI:BINDING`'s (K12) with a new `E-NPUB-TARGET` before the window.
