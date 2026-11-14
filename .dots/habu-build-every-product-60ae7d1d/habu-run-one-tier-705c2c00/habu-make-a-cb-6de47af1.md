---
title: "Make a callback's C entry in the step that defines it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:54:25.237579+02:00"
---

Problem. A C callback's entry is not made by the step that defines it. The engine lays 16 ready-made stubs (src/habu/layout.f CB-POOL, laid by src/habu/habu1.f:2377 BCALLBACK-ENTRY, registered as `callback-entry` at :4245; src/habu/prims.f:874-878), answered by FFI:CALLBACK-ENTRY (src/lib/ffi-abi.f:572). src/lib/ffi-callback.f EMIT (:324-348) arms a stub across two definitions: ARM-STUB (:26) calls FFI:CALLBACK-ENTRY at :339 through NCOMP-DISPATCH:CALLBACK-STUB-CELL (:27), STUB-ENTRY reads NCOMP-DISPATCH:CALLBACK-ENTRY-CELL, and INSTALL-ENTRY (:181, public) stores it for the generated NAME-ENTRY, which calls FFI-CB:ENTRY-ADDRESS (:177, :342). The two cells are src/habu/layout.f:586-587 (claims src/habu/data-claims.f:161-165). T0 stores NDEFINITION's callback group (src/compiler/native/definition.f:89-106, CALLBACK-STUB! to STUB-END) in them at src/habu/t0.f:2147-2148, calls it at :2011-2136 and reads NCBSTUB (src/compiler/native/callback-stub.f:5) in src/habu/t0-emit.f:550-581, src/habu/t0-arm64.f:413-414 and src/habu/t0-x64.f:172-173. The machines differ: arm64 emits its thunk as a separate (CALLBACK-THUNK) record (src/habu/habu1.f:2395-2406); x86-64 keeps it inside its `callback-entry` primitive (src/habu/kernel-x64.f CALLBACK-ENTRY-BODY, :4281). Callbacks live in their own package FFI-CB (src/lib/ffi-callback.f:14).

Joel's rulings.
- 2026-11-03, not yet settled as a design: a callback is defined for the program's life to bridge one Habu word; no request is left between calls, so nothing to end; the engine's 16 ready-made stubs go; he asked why FFI-CB is a separate package.
- 2026-11-03: T0 takes on no N* package.
- 2026-11-03: "i was wrong. callbacks should live in the FFI package since they are FFI callbacks"
- 2026-11-03: "there's no point i can see in a 2-pass approach to setting callbacks, prove me wrong"
- 2026-11-03, on names and shapes: the engine word `callback-stub`; the shape `CALLBACK: NAME WORD ( in -- out ) clauses ;CALLBACK`; COMP-BODY:callback; FFI:BIND and a prefix rule whose words are lost, so the design asks Joel for it; RESULT!; CLEAR-FAULT.
- 2026-11-04: "we decided to unify the frontend and just have different code-generation backends. why the fuck do arm64 and x86 have different interpreters? why does arm64 not use the typed one if it's better? why are we patching and working around instead of doing what's best for long term?"

What the design must do.
- Make a callback's C entry in the one step that defines the callback, once, at top level after its word, as every body store in Habu is made: no request armed in one definition and read in another. Both cells and their claims, the NDEFINITION callback group, T0's calls to it and NCBSTUB go.
- Remove the 16 stubs, `callback-entry`, FFI:CALLBACK-ENTRY, FFI-CB:ENTRY-ADDRESS and the public INSTALL-ENTRY; test/x86-64-kernel-callback.f (SUITE at test/gate-stdlib-cases.f:741), which tests `callback-entry`'s slot range, goes with the primitive.
- Write `callback-stub` once in the typed definers (src/habu/interpreter.f, which every machine runs: src/habu/runtime.f:9), with COMP-BODY:callback and the ruled names and shape above. Each machine differs only in the code it generates, so arm64 and x86-64 reach the thunk the same way.
- Fold FFI-CB into FFI. FFI is sealed in hb (src/lib/ffi-abi.f:1552 seal-package; `hb --load tools/check.f -- src/lib/ffi-abi.f` answers E-ENGINE-PROVIDED) and src/lib/ffi-callback.f is not built into hb (the same check runs it), so the fold must build the callback code into hb or open no definition into FFI after its seal.
- Find every generated word by name, never by the latest definition: a definition the compiler refuses in a multi-error load is abandoned and the load reads on (src/habu/interpreter.f:1896-1901 DEF-COMPILE).

Acceptance: a CALLBACK: program built for arm64 calls its Habu word from C through the entry its definition made; `rg -i --no-ignore` finds no CB-POOL, callback-entry, CALLBACK-STUB-CELL, CALLBACK-ENTRY-CELL, NCBSTUB or package FFI-CB in src/ or test/; the ffi-callback and ffi-callback-growth suite tests (test/gate-stdlib-cases.f:434, :440) pass.

Depends: none.

Claim: unassigned.
