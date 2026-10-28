---
title: Run one tier 0 on every target
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:18.214120+02:00"
---

Joel 2026-10-27/28: iteration speed; one T0 front end (src/habu/t0.f), one neutral emission record, one publisher and capture/shadow path, per-arch emitters only (arm64, x86-64, Wasm); any host cross-compiles any target; arm64 JIT replaced by T0-arm64 and deleted after section 5's measurement; tier 1 and the Gforth seed retired. Lanes are children; the T0 stack oylsqluo..sqnysqkt (habu-start-the-habu-3dac4a39) lands first or with L4/L5, carrying a committed test.

Claim: jerry, 2026-10-28.

Design (Fable Plan, read at sqnysqkt; lead complexity pass applied):

# One T0 everywhere (Fable Plan, read at jerry-t0-kw @ sqnysqkt, 2026-10-28)

Joel 2026-10-28: one T0 front end, one neutral emission record, one publisher + capture/shadow path, per-arch
emitters only (arm64, x86-64, Wasm); any host cross-compiles any target; arm64 JIT replaced by a T0 arm64
emitter, deleted after measurement; tier 1 deleted. Gforth seed: RETIRE (Joel chose).
Invariants: codegen emits unplaced rows naming records (architecture.md:160-167); publication proves then commits
(xcomp.md:121-134); no load-time target selection in src/ (xcomp.md:242); a host bin/hb carries no other backend
(xcomp.md:166-167, 279-285); image-ABI cells stay (hard cutover).

## 1. Emitter seam
Shared in t0.f: tokenizer, VS, folding/inline ops, locals, control/loops/case/strings/quotations, dispatch. New in
t0.f: T0:COMPILE ( ptr u8 n -- ) lifted from compiler.f STAGE/RUN/RECORD (check step 337-457, NFETCH:CAPTURE,
RETRACT/report); T0:COMPILE-FIXED ( n n -- ) (constant/create/defer); does> split; callback entry; dual emission:
host walk + publish; if NSHADOW:OPEN? and not NATIVE?, select shadow arch, walk again, NSHADOW:TAKE (TAKE-DOES).
Per emitter (t0-arm64.f, t0-x64.f, t0-wasm.f): today's t0-x64.f words + FIXED ( val kind -- ), DOES-TAIL,
CB-ENTRY ( slot thunk -- ) (extra function + ADDR-CODE row, as emit.f:2068-2078), SEAL (fills NEMIT).
Several emitters per process (arm64 host building x86/Wasm): new src/habu/t0-emit.f (package T0-EMIT) holds the
DEFTYPEs/constants (t0-x64.f:63-111 move), one TYPED-VARIABLE xt cell per op + same-named wrapper (pattern
publish.f:29-30), VREGS as a cell, registry T0-EMIT:PROVIDE ( CTARGET:arch [ -- ] -- ) / SELECT ( arch -- ),
E-T0-TARGET if none. Each emitter ends `ARCH [: INSTALL ;] T0-EMIT:PROVIDE`. Each emitter owns its sink/rows.
T0:COMPILE selects host arch (NABI:BINDING abi.f:25-26) at install, shadow arch (NSHADOW:BINDING) per definition;
native-build-core.f loads the target emitter (as native-emit-x64.f:6-15 today); wasm-build.f requires t0-wasm.f
(replacing src/arch/wasm/backend.f, line 29).
Structured control (Wasm forces it): replace JMP/JZ/JNZ/JMP-BACK/JZ-BACK/JNE*/LABEL/PATCH/LOOP-ENTER/SKIP-EQUAL
in the interface with IF ( vreg -- ) ELSE THEN; BEGIN; WHILE ( vreg -- ) REPEAT; UNTIL ( vreg -- ); AGAIN;
LOOP-OPEN ( entry:bool -- ) LEAVE LOOP-NEXT LOOP-STEP ( vreg -- ) LOOP-CLOSE; CASE OF ( vreg -- )
OF-LIT ( vreg n -- ) ENDOF ENDCASE. Native emitters keep sites/marks private; t0.f CS keeps kinds + local scopes,
loses CS-VAL sites, DO-HEAD/DO-ENTRY/LEAVES.

## 2. Emission record: NEMIT (src/compiler/native/emission.f), lifetime SEAL..CLEAR
Emitter SEAL: NEMIT:OPEN ( bytes size ret arch ), PLACE (host walk: NPUB:NEXT-SLOT at OPEN, rel32/BL written;
shadow walk unplaced), FUNCTION+ per unit (unit 0 at 0, each QUOT-OPEN offset, CB-ENTRY fn), CALL-SITE+ (impl 0,
loc = token byte offset), ADDR-SITE+ kinds NEMIT:ADDR-DATA=1 / ADDR-CODE=2 (move hir.f:569-572 into NEMIT; a
quotation address files as ADDR-CODE with the absolute unit address, as x86 passes.f:275-300), PATCH-SLOT! for a
created word, SEAL. T0:COMPILE publishes and shadows between SEAL and CLEAR (finally).
publish.f: NART: reads -> NEMIT words without handle (92-141, 227-262); HELD-E goes (162); PLACE-CK, RELOC-CALLS
stay. NHOST (host-publish.f, host.f) only serves the tier-1 unit cache -> COMMIT-OWNED (200-213) collapses to
COMMIT + UNIT-NOTIFY; NHOST TAKE/PUBLISH-TAKEN/ABANDON-TAKEN go; impl/loc columns go once unread (keep loc if
PUBLISHED-ROWS diagnostics compiler.f:193-200 survive: verify in B1). shadow.f: TARGET-CK (117-125) on NEMIT:ARCH;
COPY-* read NEMIT; NATIVE-ADDRS-XT (55) loses emission param. aot-shadow.f, wasm/capture.f, link-x64.f,
link-wasm.f unchanged except HIR:ADDR-* -> NEMIT:ADDR-* (aot-shadow.f:293,411,417). Delete
src/compiler/session/emission.f and lease.f with tier 1.
Public word changes (follow from Joel's choice): NPUB:PUBLISH-PENDING/RAW ( NART:emission -- ) -> ( -- );
PUBLISH-PENDING-DEFER/DOES ( e n -- ) -> ( n -- ); WITH-UNIT observer deleted with the unit cache; NSHADOW:NATIVE-ADDRS! likewise; NSHADOW:TAKE/TAKE-DOES lose emission; NEMIT gains
ADDR-DATA/CODE; CALL-SITE+ may lose impl/loc; NCOMP:COMPILE/COMPILE-FIXED/INSTALL/DISPATCH! -> T0:COMPILE,
COMPILE-FIXED, INSTALL, DISPATCH! (same effects); set-tier/tier@ rows (prims.f:778, 812), TIER:SELECT,
src/lib/tier.f deleted; WBACK:INSTALL/BINDING -> T0-EMIT:PROVIDE + WASM binding in t0-wasm.f; tools/
native-unit-capture.f, native-unit-object.f STAY: they are whole-file reuse (file objects), rebound to NEMIT in L3.

## 3. arm64
Entry: EM-COMPILE-SEMI already hands the captured body to NCOMP-DISPATCH:XT-CELL (habu2.f:12414-12454; TIER-CELL
3349; LCOMPILE branch 10261). T0:INSTALL stores T0:COMPILE in XT-CELL and fixed/does/callback entries in
FIXED-SHADOW-CELL, DOES-SHADOW-CELL, CALLBACK-STUB/ENTRY-CELL (compiler.f:1259-1264, layout.f:1237-1244,
1775-1776), run by CHECKER-REG:SEAL (native-runtime.f:205-209) and at boot where NCOMP:INSTALL runs. Native build
already compiles the prefix at tier 1 through publish.f (native-build-support.f:2, aot-build.f:4, hb-build-lib.f
671-676): JIT is not on the product path; it serves --load/REPL.
Fixed bodies: machine-code definers stop emitting stencils and call FIXED-SHADOW-CELL with (value kind) =
T0:COMPILE-FIXED (habu2.f:4949-4957, 5378, 5603, 5785; does> 3944), as x86 definers.f DEF-ADDR-TAIL/
DEF-CONSTANT/DEF-DEFER already do. does> child patch stays a kernel row.
t0-arm64.f over src/arch/arm64/asm.f: data stack x19 up (jit.f:20-23), frame str x30,[sp,#-n]! / ldr x30,[sp],#n
(frame.f:30-36), BL + callmap only outside region (sites.f:11-14), DATA addr MOVZ/MOVK chain (emit.f:1660-1666),
code addr ADR in reach else four-lane chain, VREGS from scratch set (abi.f:40-48), loop frames on DATA loop stack.
Never the four-lane carrier form for a scalar literal (habu2.f:45-52).
habu2.f keeps prims, boot, snapshot/AOT relocation, seed dictionary, machine-code interpreter; loses jit.f,
regalloc.f, CF JIT helpers + keyword handlers (habu2.f:2164-3145, J-*/C-COMPILE-*, EM-COMPILE-* 11196-14091 minus
SEMI capture tail, LCOMPILE 10261), tier cells (layout.f:1248-1273; offsets stay reserved), habu1.f JIT helpers
(boundary: worker census in L8). Prefix gains t0-emit.f, t0.f, host emitter (habu2.f EMIT-HOST-LOAD-PREFIX:1885,
native-runtime.f manifest, tools/build-fixpoint.f; mirror list xcomp.md:497-504). Convergence: gen1 by old tier 1,
gen2 by T0-arm64, fixpoint gen2->3 (tools/two-generation-build.f). No snapshot/capture file references the JIT.
Machine code stays: primitive bodies, interpreter loop, boot, snapshot relocation, seed dictionary writers.
Switching arm64 to the Habu loop is enabled, not required (separate decision).
Gforth seed RETIRED (Joel 2026-10-28): delete bootstrap/, tools/bootstrap.sh, ddc-verify.f Gforth leg; recovery =
trusted native seed or cross-build from another host; DDC = cross-host double compile (x86 host builds arm64
product vs arm64 self-build, byte compare). Update CLAUDE.md/docs that name Gforth recovery.

## 4. Wasm (host-side only; no Wasm runtime compiler)
t0-wasm.f writes WENC's emission format (encode.f:48-57; CALL row = padded 5-byte index; ADDR row = padded 10-byte
SLEB, kinds DATA/CODE/FUN) so capture.f, link.f, link-wasm.f, dynamic.f, kernel.f, kernel-words.f, leb.f,
profile.f survive; delete backend.f, passes.f, select.f, structure.f, wstruct.f, encode.f (SLEB/field helpers and
lane rule encode.f:584-592 move into t0-wasm.f). Vregs = i64 locals; SPILL-ALL at calls; IF/ELSE/THEN ->
if/else/end, BEGIN..UNTIL -> loop/br_if, WHILE..REPEAT -> block/loop/br_if/br, LEAVE -> br, multiple whiles -> one
block per exit, EXIT -> return. Lanes: definition in/out from the certificate; callee arity needs a record->arity
reader (checker-owner; word not determined). does>/create stay refused in a Wasm link.

## 5. Measure before deleting the JIT (jerry-aot/.out/bench, benchlib.sh)
(a) load instructions+wall zz and tender, JIT vs T0-arm64; (b) run instructions same; (c) per-definition compile
cost tools/compile-floor.f; (d) engine code size. Bar: load <= 1.2x JIT instructions, run <= 1.05x JIT on both,
outputs identical. A miss is a design finding reported with numbers.

## 6. Lanes (unpushed T0 commits oylsqluo..sqnysqkt stay)
(L0 x86 prims: DONE, landed 454f776c.)
L1: t0-emit.f seam + registry; structured control; t0-x64.f adapted; t0.f requires t0-emit.f. Proof: harness rows
  of commits 1-2 on arm64, unsupported count unchanged on corpus.
L2 (after L1): x86 SEAL into NEMIT; NEMIT:ADDR-* constants, every HIR:/x64ir/a64ir ADDR reader moved to them in this lane (no alias).
L3 (after L2): publish.f + shadow.f on NEMIT; compiler.f publishes inside HOST-WORK before RETIRE; NHOST/
  COMMIT-OWNED removed with unit-cache readers. Proof: compiler-session, native-file-* e2e, full arm64 suite.
L4 (after L1, L3): T0:COMPILE entry, COMPILE-FIXED (PATCH-SLOT, defer trailer), does> clause unit, CB-ENTRY,
  T0:INSTALL; runtime-x64.f installs T0; definers.f tier rules dropped. Proof: peer test/run.f.
L5 (parallel with L4): t0-arm64.f emitter + harness rows; borrowed-dispatch run of gate-stdlib-cases subsets.
L6 (after L4, L5): dual-emission loop; native-build-core.f loads target emitter; native-emit-x64.f requires
  t0-x64.f. Proof: x86 engine cross-built by T0 on arm64, peer test/run.f, aot-shadow-capture, x86-64-link-records.
L7 (after L5): arm64 engine on T0 (fixed-body sites call cell, XT-CELL always, prefix rows, manifest); fixpoint;
  full arm64 suite; section 5 measurements in the commit.
L8 (after L7, bar met): delete JIT, tier cells, set-tier/tier@, src/lib/tier.f, test/tier.f, ~40 set-tier lines;
  retire Gforth seed; full suite + fixpoint.
L9 (after L6): t0-wasm.f; wasm-build.f on it; test/wasm rows under bun, maki viewer build; size/run vs tier 1.
L10 (after L6, L8, L9): delete src/compiler/** (keep dict.f, string.f, workspace.f, checker-owner.f, fetch*.f,
  emission.f moved to src/habu/), src/arch/*/passes.f, select*, test/compiler/native-* per t0-open-items.md 6, docs.
Parallel: L1 now; L2->L3; {L4, L5} after L3; L6; {L7, L9} after L6; L8 after L7; L10 last.

## 7. Risks / open
- T0-arm64 spill-everything vs run bar on loop-heavy zz/tender: measurement decides.
- Exact habu1.f/habu2.f JIT deletion boundary: worker census in L8.
- BODYBUF parity between arm64 machine-code capture (LBCAP) and definers.f (quotation/does> signatures dropped,
  strings one byte past opener): L5 harness proves.
- x86 callbacks unsupported at tier 1 today (kernel-x64.f:4686-4697): CB-ENTRY on x86 is new kernel work.
- Not determined: record->arity reader for Wasm lanes; PUBLISHED-ROWS need for CALL-LOC after NHOST goes.

Lane order, Joel 2026-10-28 (supersedes the section 6 parallel plan): shared structure first (L1, L2, L3 incl. C3, L5c label layer), then arm64 end to end: shared front end T0:COMPILE + unit step extracted from compiler.f (proved on arm64), arm64 fixed/does>/callback emitter rows, T0 installed as the arm64 runtime compiler, section 5 measurement vs the JIT (go/no-go), then L8 JIT deletion. Only then x86 (L4 C2-C4 on the peer; L4-C1 done and parked in the stack), L6 cross/dual emission, L9 Wasm, L10 tier 1.
