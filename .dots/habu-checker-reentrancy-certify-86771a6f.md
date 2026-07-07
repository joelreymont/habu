---
title: "Checker reentrancy: certify defs compiled during word execution"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T15:14:54.739645+02:00"
---

BLOCKER for habu-compiler-pkg-scoped-1a4d29bd full static composition. When an executing word triggers compilation of a new colon definition WHILE the check hook (set-check HOOK / CHECK!) is active, the native engine CRASHES (SIGBUS / EXC_BAD_ACCESS, jump to PC=0x1 via blr x9 from clobbered interp state [x20,#0x1b0]). Reproduced with BOTH evaluate and included. Minimal reproducer (bin/hb --load): "0 set-check : W ( -- ) s\" : ZZ ( -- n ) 5 ;\" evaluate ; 1 set-check  W" -> EXC_BAD_ACCESS. Also crashes with included of a file defining a colon word. Top-level evaluate of the same colon def (not nested inside a word execution) is fine, so the gap is checker/compiler NON-REENTRANCY across a word-execution boundary: the check hook and interpreter/compile state are not saved/restored for a nested definition. Capability needed: make definition compilation + certification reentrant (save/restore checker arenas + interp compile pointers), OR provide a supported native primitive to compile-and-certify a definition programmatically from within an executing word. Until this lands, MODEL: (an executing word) cannot compile its body as a checker-verified colon definition. Owner: src/core/checker.f + native colon compiler + src/core/check-hook.f. Evidence gathered on fable host 2026-07-06 (lldb).

RCA SESSION 2026-07-06 (no fix landed; workspace clean; exhaustive evidence):
FAITHFUL REPRODUCER (the dot's original was partly a red herring - `1
set-check` installs literal 1 as the hook, crashing even top-level):
  dbase@ $1B0 + @  0 set-check
  : W ( -- ) s" : ZZ ( -- n ) 5 ;" evaluate ;
  set-check  W  ZZ .
-> exit 70 printing "does>" (C-DIE-DOES, habu2.f:1197). Discriminators:
top-level checked evaluate OK; reentrant unchecked OK; reentrant checked FAILS
(does>/hang/SIGBUS, layout-sensitive across rebuilds).
PINNED: die site = C-CALL-CHECK-DEFINER (habu2.f:1219) at EVALD=1: the 10
G-POP after the hook BLR reads flag 0. PARADOX: HOOK (check-hook.f) can only
return -1 or throw - never 0; BTHROW instrumentation shows NO throw fires and
XDS is balanced. So either CHECK! mis-executes mid-publish at EVALD>0 or the
return never reaches G-POP. Secondary: x20 (XREG-RBASE, reserved DATA base)
intermittently corrupted to 0x0000044000000000 = a checker T-CON typed cell -
corrupted during the hook window, valid before/after.
RULED OUT with evidence: throw-misroute via LEVALREC; RSTK collision (RSP=0);
stack-gap collision (256KB gap no change); EVAL-FRAME overlap; duplicate-def
throw; FFI x20-parking (no FFI at EVALD>0); mmap; register allocator (x20 not
pooled). Core files load at EVALD=0, so the hook had NEVER run at EVALD>0.
BLOCKER: warm-snapshot builds are layout-unstable (same source -> die vs hang
vs SIGBUS) and any store added to HOOK breaks the fixpoint - could not read
past the paradox. NEXT STEP (concrete): lldb single-step the stable `install`
bin/hb - persistent BP at C-CALL-CHECK-DEFINER's hook BLR, step INTO CHECK!,
watch x19/x20 + data-stack top to catch the instruction where the return is
lost / x20 written; then save/restore that state across the nested evaluate
frame or a reentrant-safe wrapper in check-hook.f.
Key sites: habu2.f 1219/3068-3110/2799/3358/3303/2376; habu1.f B-EVAL 1036,
BTHROW 1631; check-hook.f HOOK/CHECK!; checker.f 5113.
habu-compiler-pkg-scoped stays BLOCKED on this.

SEQUENCING 2026-07-07 (user): fix this LAST - after the remaining host lanes
(codegen-role, named-descriptor) land. Then attempt the lldb single-step
session agent-driven per the banked next-step plan.

RESOLUTION 2026-07-07 (lldb-lane finding, verified independently on the stock
engine): the NATURAL nested-compile path WORKS - a `TRUSTED: W ( -- ) s" : ZZ
( -- n ) 5 ;" evaluate ;` definer with the hook active certifies+publishes+runs
the nested checked def (ZZ -> 5, rc 0) and a bad-effect nested def still
rejects (rc 70 at 'drop'). Both landed as engine-gate regressions
(GE-NESTED-CHECKED-DEF / GE-NESTED-BAD-DEF in test/gate-engine-lib.f). The
does> failure REMAINS but only under the hook-swap idiom (save hook ->
0 set-check -> define W unchecked -> set-check restore -> W) - the prior RCA's
reproducer idiom, not the natural path. The defect narrows to hook-swap state
(likely the same stale-registry class as habu-check-records-go-4f62cd2e).
CONSEQUENCE: habu-compiler-pkg-scoped (MODEL:-driven static composition) is
UNBLOCKED - a TRUSTED:/checked definer wiring package PLAN is the natural
idiom and it works today. This dot stays open ONLY for the hook-swap-idiom
defect, deprioritized (p3): no natural code path uses that idiom.
