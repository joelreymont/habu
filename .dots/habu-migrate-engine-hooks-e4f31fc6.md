---
title: Migrate engine hooks to typed xt cells
status: open
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-16T08:27:22.049121+02:00\\\"\""
---

Stage 2 of the stored-xt soundness program (RCA + sequencing in habu-checker-exec-of-5923c543; stage 1 landed: typed xt<effect> cells, f369f37c). Migrate the ~36 engine hook cells that use the raw-var @ execute shape - the RCA's measured inventory: checker.f/type-family.f/sumtype.f/layout-buffer.f plugin dispatch (tfam-resolve*/arity*/layout?*/cell?*/width@*/concrete-linear?, match-fam-tok/match-variant-tok/match-of-tok/construct-tok, checker-usig-cert-add/checker-package/checker-undefine-guard/checker-lbuf-name-guard/checker-snapshot-prepare, rbf-push/rbf-pop/report-uncheckable/usig-add-bad/sig-type/loc-show-one/check, lbuf-eval-run/layout-maybe-linear?/layout-linear-count, tdecl-gen-eval/tdecl-ctor-prot-wid/tf-ctor-build-hash) and habu1.f hooks (source-hook/prefix-hook/cold-hook/restore-hook/proof-hook) - to typed xt<E> cells (TYPED-VARIABLE with the hook's true effect) or defer/is where install semantics fit; fprim/fprim-l/fprim-wid stay TRUSTED machine-code boundaries (classify explicitly). Each migrated hook: declared effect matches the real callers (derive from the current install sites), store sites become first-class quotation stores or typed installs, fetch+execute fit-checks. Acceptance: zero raw-var @ execute sites remain in the inventory (rg proof), all suites + self-check certify (Certified count may move - STATUS.md updated honestly), fixpoint x2, full run.f, maki 0 FAIL; the stage-3 pin (xt-cell-test L1) still shows plain user variable laundering (the flip is stage 3, not this dot). COORDINATE: broad checker.f surface - tfam's sealed-packages lane is active; keep hunks per-hook surgical; integrator rebases by hash. Files: src/core/checker.f, type-family.f, sumtype.f, layout-buffer.f, render.f, src/habu/habu1.f, STATUS.md, tests. Ownership: checker/engine hook typing.


RE-SCOPED 2026-07-16 (hookmig lane; enabler landed 7e93b439 "engine: load
exec-vector before checker prefix" - the prefix order lives in SEVEN
synchronized mirrors + pinned codegen-test counts, all updated). The migration
itself was BUILT AND NATIVE-GREEN (defer conversions + default-is guard
restructuring + installer words; fixpoint x2; full run.f correctness-green)
then honestly REVERTED on two proven blockers:

- BLOCKER 1 (hard): RESOLVED 2026-07-16 - habu-mirror-defer-is-4461fe23 landed
  (stage0 forth.fs now mirrors C-DEFER/J-IS; wide-memory defer/is round-trip
  green; full recovery CHECK_ONLY chain OK). Scope boundary that constrains
  stage 2: the stage0 defer/is works on the UNCHECKED load path only (files
  before check-hook INSTALL, TRUSTED: bodies); a defer/is inside a CHECKED
  definition in a stage0-loaded prefix file is rejected fail-closed because the
  mirror lacks the C-CALL-CHECKER-DEFER registration bridge. Either keep
  stage-2 defer shapes in unchecked-region files, or land
  habu-mirror-checker-defer-6a8a366e first.
- BLOCKER 2 (ordering): checker.f-internal hooks (rbf-push/rbf-pop/sig-type/
  check/...) are installed BEFORE the checker prefix is live, so TYPED-VARIABLE
  xt<E> cells cannot type their own bootstrap installs without a trust seam;
  the enabler (exec-vector before checker prefix) fixes the habu1.f hook class
  only. The checker-internal class needs either stage0 defer/is (blocker 1) or
  a typed pre-prefix install primitive - decide when blocker 1 lands.

Per-hook inventory is the measured list in this dot's opening paragraph; the
per-hook effects were derived once by the hookmig lane (workspace retired,
revert not preserved) and are re-derivable from the current install sites as
the acceptance text specifies.
Claim released (agent=hookmig done; enabler integrated by orchestrator).
Blocked-by: none (habu-mirror-defer-is-4461fe23 landed and closed 2026-07-16);
blocker 2 (install-ordering) remains a design constraint documented above.
STAGE-2A LANDED 2026-07-16 (stage2a lane; claim released): all five habu1.f
hooks (source/prefix/cold/restore/proof) converted from raw-var @ execute to
defer/is with declared effects and preserved default semantics (explicit
default-is: SOURCE-DEFAULT for source-hook, HOOK-NOOP for the 0-guard class;
habu2's restore install is a colon-wrapped typed quotation since [: is
compile-only). rg proof: zero *-XT sites remain in habu1.f (FP-XT/fprim
excluded by scope - already checked since the btrust batch). Stage-3 pin
intact (xt-cell-test L1 still launders). Proven on BOTH paths after the
checker-defer bridge: native fixpoint x2 0a71f23d; full recovery chain
'bootstrap check OK' with the mirror-built seed certifying the checked
defer/is hooks. Zero new TRUSTED sites; owner-wid-emit-seal absence rows
follow the new words.

REMAINING = STAGE-2B (checker.f/type-family/sumtype/layout-buffer/render
hooks, the ~31-cell balance of the opening inventory): the bridge removed the
mirror-side rejection; the open constraints are (1) the install-ordering
design - those hooks install BEFORE the checker prefix is live, so either
stage0-compatible defer placement or a typed pre-prefix install primitive,
decide at dispatch; (2) tfam sealed-packages coordination on checker.f hunks
(surgical per-hook hunks, integrator rebases by hash). Corrected premise for
2b (proven by stage-2a): the engine prefix files are in the CHECKED region of
the generated stage sources - 2b conversions will be checker-certified code,
not unchecked-region code.


STAGE-2A STATUS 2026-07-16 (stage2a lane, implementation COMPLETE and
native-green, held uncommitted in .jj-ws/fable-stage2a pending the bridge):
PREMISE CORRECTION - the habu1.f hooks are NOT unchecked-region code: in the
assembled stage2-src, LOWER-CERT-HOOK:INSTALL (checking ON) precedes
SOURCE-HOOK! with no 0 set-check between, so the owner-wid hooks compile
CHECKED. Native certifies checked defer+is fine (fixpoint x2 5a7638a6);
the gforth-mirror stage0 rejects it - exit 70 'hook: non-certified definition:
source-hook! at is' - because mirror C-DEFER (forth.fs:4344) omits
C-CALL-CHECKER-DEFER. Option A (unchecked-region placement) would require
adding a forbidden 0 set-check boundary; therefore ALL of stage 2 gates on
habu-mirror-checker-defer-6a8a366e (blocks: edge added). Minimal blocker
fixture: any 'defer FOO (E)' + ': FOO! ([E] --) is FOO ;' after
LOWER-CERT-HOOK:INSTALL on the mirror-built stage. Per-hook conversion table,
rg proof (zero raw *-XT sites), stage-3 pin intact, and full gate tails are in
the stage2a lane report. When the bridge lands: rerun recovery in the
workspace, commit 'engine: habu1 hooks to defer/is (stage 2a)', integrate.

BRIDGE LANDED 2026-07-16 (habu-mirror-checker-defer-6a8a366e closed, 5d2f6d29):
the mirror-built seed now certifies checked defer/is. Stage-2a proceeds from
the held implementation in .jj-ws/fable-stage2a (rerun recovery, commit,
integrate). Stage-2b's defer approach is likewise unblocked on the stage0 side;
its remaining constraint is the checker.f-internal install-ordering design +
tfam coordination.

STAGE-2B DESIGN (read-only scout, 2026-07-16, master 16bb3cc8; full table with
every file:line in the scout report - essentials preserved here):

LOAD-ORDER SPINE: engine prefix loads checker.f(6) -> lower-cert-base.f(8,
installs PRODUCER-XT) -> type-family.f(10) -> render.f(11) -> sumtype.f(12) ->
layout-buffer.f(13, TYPED-VARIABLE definer :217) -> check-hook.f(15, set-check
re-point) -> ... include.f(~25, TDECL-EVAL-XT install :343) ->
type-family-sha.f(~28, TF-SHA16-XT install :13). Two AOT-driver installers
outside the prefix: xref.f:235 (TDECL-PROT-WID-XT), aot.f:13 (TDECL-EVAL-XT
alt). Checking is ON throughout (parent checker live; check-hook only
re-points), so all 2b conversions compile CHECKED and post-bridge defer/is
works everywhere.

KEY DESIGN VERDICTS:
- defer/is is the UNIVERSAL shape. TYPED-VARIABLE (pos 13) loads after every
  hook-cell file (6/10/11/12) - stage-1 typed cells unavailable without
  relocating the definer (not required). NO typed pre-prefix install primitive
  needed - blocker 2 dissolves post-bridge.
- Install-twice semantics preserved everywhere (plain ! and is both silently
  overwrite); the one die-on-double cell (PRODUCER-XT, checker.f:4855) is a
  deliberately sealed boundary (installed lower-cert-base.f:129, INSTALL word
  undefined + axiom sealed) - OUT of scope, classify like fprim.
- Default/unset semantics MUST be preserved via explicit default-is
  (stage-2a SOURCE-DEFAULT/HOOK-NOOP precedent) in four classes:
  (1) fail-open VALUE defaults, live during the pos6->pos10 window:
      TFAM-RESOLVE(0,false)/ARITY(0)/LAYOUT?(f)/CELL?(f)/WIDTH(1);
  (2) fail-open NO-OP: DIAGXT RECXT BADSIG-XT LOCSHOWXT REG-EXT-PERSIST
      REG-SCRATCH-SNAP REG-EXT-RB-SAVE/RESTORE CTOR-PKG?/WORD?/EXTEND?;
  (3) fail-closed REJECT gates: MATCH-FAM-XT (MATCH-BEGIN->0 OK!),
      CONSTRUCT-FAM-XT - default-is returns the reject value;
  (4) fail-closed DIE/THROW: TDECL-EVAL-XT (E-LAYOUT-BUFFER throws + die),
      TDECL-PROT-WID-XT, TF-SHA16-XT - keep guards or default-is that dies
      with the SAME message.
  Gate-protected unguarded cells (MATCH-VTAG/VCOUNT/PAY, CONSTRUCT-STEP,
  TFAM-CON-LIN, SIG-QUOT, TFCL-NODE) need no default-is (unreachable unset).
- BOUNDARY FLAG: TDECL-PROT-WID-XT's ONLY installer is xref.f:235 (AOT driver,
  outside the checked engine prefix) - classify as boundary or verify the AOT
  driver hosts a checked is before converting. The one inventory cell without
  a checked-prefix installer.

BATCH PLAN (LOW-risk first; each its own commit):
  B1 render-diagnostic (LOW): DIAGXT/RECXT/BADSIG-XT/LOCSHOWXT/
     REG-SCRATCH-SNAP-XT; cells checker.f 3990/6502/7486-7487/5520, installs
     render.f 91/328/773/808/879.
  B2 snapshot/rollback (LOW-MED): REG-EXT-PERSIST/RB-SAVE/RB-RESTORE;
     checker.f 5519/8351-8352 + type-family.f 1038-1039/1063 (3-line hunk,
     near tfam's live 1093 hunk - keep surgical).
  B3 eval-crossing (LOW-MED): TDECL-EVAL-XT + TF-SHA16-XT; sumtype.f 732/
     823-825, layout-buffer.f 104-221 consumers, include.f 343,
     type-family.f 624/671-675, type-family-sha.f 13.
  B4 internal trampolines (LOW): SIG-QUOT-XT (checker.f 2497/2518/2525/2644),
     TFCL-NODE-XT (type-family.f 780/785/794/813) - plain defer, same-file.
  B5 TFAM/MATCH/CONSTRUCT core (HIGH - tfam epicenter): Groups A+B+C, decls
     checker.f:418-430 + guards 4824-4942/5233-5240, installs the contiguous
     type-family.f:1358-1372 block. ONE tightly-scoped commit in a coordinated
     window; integrator rebases by hash.
  B6 TDECL-PROT-WID-XT: separate boundary decision.

TFAM COLLISION MAP: HIGH = type-family.f 1358-1372 / 518-560 / 1030-1093 /
780-814 / 620-680, checker.f 418-431 / 4824-4942 / 5233-5240 / 7315-7445 /
7955-7973. LOW = render.f everywhere, checker.f 3990/6502/7486-7487/
5519-5534/8351-8494/2497-2644, sumtype.f + layout-buffer.f + include.f +
type-family-sha.f. Lane markers: recent tfam commits e596b0f1 (ctor-package
limit), c217293a + the option-family/field-schema cluster; live hunks
type-family.f @-1093, checker.f @-2337.

CLEAN-4 PARTIAL LANDED 2026-07-16 (stage2b lane; claim released; B1-B4 scope
exhausted - everything else in stage 2b now gates on the pre-trust defer
capability or a recorded design decision): REG-EXT-RB-SAVE/RESTORE-XT (no-op
default-is via REG-EXT-RB-DEFAULTS), TF-SHA16-XT (die default-is, verbatim
message/code from the deleted guard), TFCL-NODE-XT (plain defer, :813 install
only) - rg proof zero raw sites for all four repo-wide; stage-3 pin intact;
proven native (fixpoint x2) AND through the full recovery chain. Certify
constraint discovered (recorded in 77410827 + LESSONS): die-class default-is
bodies must locals-consume their declared inputs or the fixpoint certify pass
rejects (E-BUILD-CERTIFY at the die site) - certify is the authority, boot
alone does not exercise it.

STAGE-2B DESIGN CORRECTIONS (2026-07-16, stage2b lane BLOCKED report - the
scout's central premise was empirically REFUTED; the scout overlooked the
pre-existing LESSONS entry documenting the constraint):
- Native C-DEFER LFINDs 'trust' (checker.f:7685-7687) unconditionally; ANY
  defer declared before that line kills boot (exit 70 printing 'trust').
  Blocker 2 did NOT dissolve - the bridge fixed only the mirror side. The
  pre-7687 class (BADSIG-XT, REG-SCRATCH-SNAP-XT, LOCSHOWXT,
  REG-EXT-PERSIST-XT, SIG-QUOT-XT, and ALL of B5 at checker.f:418-431) now
  gates on habu-engine-pre-trust-77410827 (blocks: edge below).
- Second blocker, independent: the test harness's raw-cell save/disable/
  restore idiom (checker-assert.f:4-7 'DIAGXT @ >r 0 DIAGXT ! ... r> !',
  engine-suite.f:301-305/1197-1203/1679/1695) is defer-incompatible - DIAGXT
  and LOCSHOWXT need a defer-compatible enable/disable or save/restore
  mechanism designed WITH the harness before conversion. The scout's B1 table
  omitted these 6 test sites plus DIAGXT executes in check-hook.f:8 and
  verify-source.f:294.
- Third: TDECL-EVAL-XT has a DUAL failure shape (layout-buffer.f consumers
  throw recoverable E-LAYOUT-BUFFER inside a catch; sumtype.f:824 dies 76
  fatal) - one default-is cannot preserve both; either unify the consumer
  contract first or keep the guarded raw cell. Design decision, not a swap.
- CLEAN-4 partial authorized and in flight (regrouped commit 'engine:
  post-trust hook cells to defer/is (2b partial)'): REG-EXT-RB-SAVE/RESTORE
  (no-op default-is), TF-SHA16-XT (die default-is, exact current message),
  TFCL-NODE-XT (plain defer, install :813 only).
- Process note for future scouts: rg LESSONS.md for the target surface before
  designing - this constraint was already recorded there.

PRE-TRUST CAPABILITY LANDED 2026-07-17 (habu-engine-pre-trust-77410827 closed,
318c748e): defers now declarable before checker.f's ': TRUST' via the pending
registration table (capture -> drain at 7690). TWO OBLIGATIONS bound to the
FIRST pre-7687 conversion landing from this dot: (1) it carries the inherent
ecosystem transition (old binaries cannot boot a tree whose prefix declares a
pre-trust defer - sequence after sol/tfam have refreshed past 318c748e; the
old-binary-boots gate flips from required-pass to expected-fail-with-named-
diagnostic at that landing); (2) it REVERTS the TRUSTED: DRAIN-PRETRUST-COMPAT
shim at checker.f's drain point to the bare DRAIN-PRETRUST token and deletes
its TRUSTED.md rows (owner habu-checker-exec-of-5923c543, discharge-candidate
class). The pre-7687 class (BADSIG-XT, REG-SCRATCH-SNAP-XT, REG-EXT-PERSIST-XT,
SIG-QUOT-XT) is otherwise ready; LOCSHOWXT/DIAGXT still need the harness
save/restore design; TDECL-EVAL-XT still needs the dual-shape decision; B5
still needs the tfam window.
