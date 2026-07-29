---
title: Fix stage0 pre-trust defer replay
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-28T10:38:57.712053+02:00\""
---

Claim released 2026-07-28 during the anonymous-head harvest (dot
habu-harvest-unmerged-anonymous-adffdf99): the claiming workspace
.jj-ws/habu-fix-stage0-pre-88a4297e (agent stage0_mirror) no longer exists in
the repository's workspace list, so the dot returns to open. The stage0_mirror
lane's code in progress (bootstrap/cg/forth.fs plus src/habu/habu1.f) is
preserved on unmerged commit 35bf3f752c16; reconcile against it before
re-dispatch.

Full context: the gforth-built stage0 mirror bootstrap/cg/forth.fs does not register captured pre-trust defers with the checker, so any CHECKED 'is NAME' on a defer declared before ': TRUST' in src/core/checker.f fails the boot with 'hook: non-certified definition: <word> at is' and exit 70. This reproduces on master@origin with a two-file patch taken verbatim from an existing test, so it is an engine defect and not lane code. Reproducer: append 'defer ZZ-PRETRUST-XT ( -- n )' to src/core/exec-vector.f and the checked selftest that is-installs [: 42 ;] to src/core/check-hook.f, exactly as test/pre-trust-defer.f APPEND-POS-DEFER and APPEND-POS-SELFTEST do at lines 109-118. Under the NATIVE engine that patch passes (test/pre-trust-defer.f exits 0, verified on a clean master workspace with a freshly bootstrapped bin/hb). Under the MIRROR the same sources fail. tools/bootstrap.sh:315 builds hb-stage0 with gforth from the mirror and :318 runs it; the failure happens there, before the native engine exists, which is why the existing test cannot see it - it boots native children. Evidence gathered: capture works (forcing C-PD-DIE-FULL at capture entry dies naming TFAM-RESOLVE-XT); the drain runs at the bare token with a non-empty table and iterates all 31 captured slots with intact names; but the replayed trust and checker-defer calls do not enter ': TRUST' (checker.f:8595) or ': CHECKER-DEFER' (checker.f:5913) for most slots - instrumented, those words execute 2353 times across the boot and zero times between markers bracketing DRAIN-PRETRUST. Immediately after the drain the checker knows diagxt (=-1) but not tfam-resolve-xt or pkg-live-xt (=0, symbol never interned), so it is a partial-effect failure inside one loop rather than an ordering rule. BDRAINPRETRUST is byte-faithful to native except C-FIND-GLOBAL vs C-P2-FIND-GLOBAL; C-PD-CAPTURE differs only by the documented '12 DATA PEND-CELL LDR,' the mirror's record-reading C-PUSH-DREC-NAME needs. The faulty instruction is NOT yet pinned - C-FIND-TRUST, C-CALL-X11-SAVED, G-PUSH/XDS and the slot arithmetic all look identical or equivalent, and the same C-FIND-TRUST + blr x11 works on the mirror's direct path. Required result: the stage0 mirror's pre-trust replay registers every captured defer, so the mirror matches the native engine that test/pre-trust-defer.f already certifies. Acceptance: the reproducer seed prints ZZ-OK and exits 74 where it exits 70 today; test/pre-trust-defer.f stays green under native; a negative regression covers the mirror path so this cannot regress unseen; full bootstrap completes with 'bootstrap OK: bin/hb'. Forbidden: special-casing any defer by name, relaxing what 'is' accepts, wrapping installs in TRUSTED: or 0 set-check, or routing callers around the defect. Oracle, about 40 seconds per iteration, no lane and no bootstrap needed: build the seed as tools/bootstrap.sh emit_src does in seed mode, then HABU_TARGET=macos-aarch64 gforth -e 'require test/nf.fs s" SEED" slurp-file s" OUT" FORTH-BUILD-EXE bye' and HB_TMP=DIR OUT -- DIR. Blocks habu-certify-pkg-auth-af2af4fe and the whole compiler-ID proofs lane.

Confirmation 2026-07-30 (agent stage0using, independent): the same failure
reds the no-binary recovery path at its base with no test patch at all -
HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh exits 70
with "hook: non-certified definition: install at 'is'" on the unmodified
parent tree, stopping at src/habu/xref.f:208 (: INSTALL ... is PKG-LIVE-XT).
So the defect now blocks the END-TO-END recovery proof for every change, not
only the reproducer seed. Note the native-engine red that shares this
diagnostic is a DIFFERENT defect (stale test assumption, dot
habu-reprove-the-undrained-16d55d36) - the matching text is not evidence
either way; this dot is specifically the stage0 mirror's replay loop. Also:
the stage0 using implementation (commit b9d5fca5 in
.jj-ws/habu-add-using-to-d815f0ab) added a checker-using scope mirror whose
real-checker leg cannot be proven until this dot closes, because a recovery
build cannot yet compile a checked region at all.

Claim: agent=stage0replay workspace=.jj-ws/habu-add-using-to-d815f0ab
