---
title: Audit aot-closure legacy chain recognizers
status: active
priority: 3
issue-type: task
created-at: "\"\\\"2026-07-21T07:45:16.839799+02:00\\\"\""
---

Loose end from the FINDPTR retirement (stack d2c2be29): src/habu/aot-closure.f still carries top-level CALL?/TGT/CALL-AT? (lines ~27-36) recognizing the movz/movk/movk/blr absolute-call chain for AOT closure analysis. Post-direct-BL (1e9a3926) no native emitter produces that chain - only the gforth seed does, and seed output never reaches AOT closure analysis. Verify dead, then retire with the same discipline (test rework against the surviving surface, honest delta measurement); if NOT dead (some path still analyzes seed-built artifacts), record exactly which and keep.

Claim: agent=aotretire workspace=.jj-ws/fable-aotretire machine=spark (owns the RE-SCOPED co-retirement: src/habu/aot-closure.f + src/habu/aot-lib.f + test/gate-aot-positive-lib.f)

Audit complete 2026-07-21 (aotchain lane, zero edits - the wall was real): the abs-chain surface (CALL?/TGT/CALL-AT?/CALL-IN-CLO?/SCAN-TARGET/FINDADDR) is DEAD-IN-EFFECT - proven four ways: (1) the movz/movk/movk/blr chain is emitted ONLY by the gforth seed (forth.fs BCOMPILE + lcall; the movk-x16 middle opcodes appear nowhere in src/); (2) mechanical scan of the post-BL seed engine bin/hb: ZERO full chains (9 bare blr-x16 are ldr-x16-preceded indirect calls CALL? cannot match); (3) the maker is built by the NATIVE engine (HBB-BUILD-MAKER-FRESH spawns BF-ENGINE$), so everything AOT-LINK walks is direct-BL; (4) AOT-UNSAFE? fail-closes on compile,/create/patch32 so the seed's runtime chain path can never land in an image. BUT structurally load-bearing at five aot-lib.f linker-core sites (COMPACT-LEN :214, MAP-IN-BLOB :288, COPY-COMPACT-BLOB :337, RELOCATE :361, SCAN-REC aot-closure.f:244) - single-file retirement breaks compilation.

DECISION (orchestrator, 2026-07-21): narrow the linker contract to DIRECT-BL-ONLY input. Retire the abs-chain arms from all five sites + the recognizers + the stale collapse comment in gate-aot-positive-lib.f, and replace the dead arm with a FAIL-CLOSED reject: the copier/relocator dies with a named error on any full abs-chain match (the safety the dead arm nominally provided, made loud). Proof shape: all AOT gates stay green; NEW negative - a hand-constructed blob containing one chain is rejected with the named error (red-first: the old code silently collapsed it); census/CODELEN delta 0 measured (hb-build tools, not engine text, per the FINDPTR precedent).
