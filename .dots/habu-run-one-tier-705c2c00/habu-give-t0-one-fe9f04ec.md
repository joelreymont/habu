---
title: Give T0 one emitter seam with structured control
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.540462+02:00"
---

Problem: t0.f calls t0-x64.f words directly, and its control interface is native jumps (JMP/JZ/JNZ/JMP-BACK/JZ-BACK/JNE*/LABEL/PATCH/LOOP-ENTER/SKIP-EQUAL), which Wasm cannot emit and which ties the front end to one arch. Acceptance: new src/habu/t0-emit.f (package T0-EMIT) holds the emitter DEFTYPEs/constants (moved from t0-x64.f:63-111), one TYPED-VARIABLE xt cell per op with a same-named wrapper (publish.f:29-30 pattern), VREGS as a cell, registry PROVIDE ( CTARGET:arch [ -- ] -- ) / SELECT ( arch -- ), E-T0-TARGET when no emitter is provided. Control interface: IF ( vreg -- ) ELSE THEN; BEGIN; WHILE ( vreg -- ) REPEAT; UNTIL ( vreg -- ); AGAIN; LOOP-OPEN ( entry:bool -- ) LEAVE LOOP-NEXT LOOP-STEP ( vreg -- ) LOOP-CLOSE; CASE OF ( vreg -- ) OF-LIT ( vreg n -- ) ENDOF ENDCASE. t0.f CS keeps kinds + local scopes, loses CS-VAL sites, DO-HEAD/DO-ENTRY/LEAVES; sites/marks are private to the emitter. t0-x64.f ends 'ARCH [: INSTALL ;] T0-EMIT:PROVIDE'. Files: src/habu/t0-emit.f (new), src/habu/t0.f, src/habu/t0-x64.f. Verify: the commit 1-2 harness rows (jerry-t0-kw/.out/l1.f, t0-time.f, t0-dis.f) pass on arm64; corpus unsupported-token counts unchanged (MATCH 48, construct 46, is 2, does> 2). Depends: T0 stack tip sqnysqkt. Ownership: those three files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-l1
