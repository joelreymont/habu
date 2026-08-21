---
title: Fix stage0 pre-trust defer replay
status: open
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

Claim: agent=stage0replay workspace=.jj-ws/habu-add-using-to-d815f0ab (RELEASED 2026-08-21: workspace gone, no live lane - gc)

## MEASURED report 2026-07-30 (agent stage0replay, workspace .jj-ws/habu-add-using-to-d815f0ab)

### What the earlier lane's preserved commit contributed

Commit 35bf3f752c16 turned out to contain no work to reconcile: its whole diff
is two identical one-line comments, `\ package-lint probe`, appended to
bootstrap/cg/forth.fs and src/habu/habu1.f. Nothing kept, nothing discarded,
no code recovered. The earlier lane's real contribution is the oracle recipe
already written up in docs/debugging.md, which this lane used unchanged.

### The dot's prior diagnosis was wrong

The dot recorded that "the replayed trust and checker-defer calls do not enter
`: TRUST` or `: CHECKER-DEFER` for most slots". They do, for every slot. Two
independent probes agree:

  * An engine-side probe in BDRAINPRETRUST (write the slot name to a stream
    before the replay, one byte after each `blr x11`) printed all 31 slots with
    intact names and both return markers.
  * A checker-side probe (`type` the name at the head of `: TRUST` and
    `: CHECKER-DEFER` in src/core/checker.f) interleaved on the same stream
    printed `[NAME><tr:NAME>T<cd:NAME>D]` for all 31 slots, PKG-LIVE-XT
    included.

So the mirror's replay loop is not the defect. It is byte-faithful to native
and it works.

### The pinned mechanism

The Gforth-built recovery engine loads the boot prefix TWICE in one process,
and the second load has to shadow the first:

  * Load 1 is the startup prefix read from disk, emitted by the mirror's
    `PFX-LOAD-CHECKER-FILES` / `PFX-LOAD-CORE-FILES` (bootstrap/cg/forth.fs
    1927-1976) as `LSRCRD` calls on baked path strings. Its file list is
    provably distinct from the seed concatenation: it contains
    src/core/include.f (whose `is` site shows up only in load 1) and it does
    not contain src/habu/habu1.f (whose `is SOURCE-HOOK` shows up only in
    load 2).
  * Load 2 is the engine interpreting its own baked program, which is exactly
    the file `emit_src` wrote: the whole prefix again, plus the driver.

In load 2, `trust` and `checker-defer` from load 1 are still resolvable while
src/core/checker.f is being re-read. `C-PRETRUST-READY?` therefore answers
"ready" at src/core/checker.f:465, `C-DEFER` takes the direct branch, and
`PKG-LIVE-XT` publishes its effect row and its defer row into LOAD 1's
checker -- the one load 2 is in the middle of replacing. Load 2's checker.f
then creates fresh DFERS/USIGS storage, so those rows are invisible; nothing
was captured, so DRAIN-PRETRUST has an empty table and replays nothing; and
the first checked `is` on a pre-trust defer, src/habu/xref.f:209
`[: LIVE ;] is PKG-LIVE-XT`, cannot certify. Measured in one trace: the same
`is pkg-live-xt` succeeds in load 1 and reports "not a defer" in load 2.

The precondition that makes load 2 safe already exists: the boot-hide prologue
`emit_boot_hide` in tools/bootstrap.sh, which hides load 1's dictionary
(BOOT-HIDE-DICT-FROM-EARLIEST) and clears its recorded effects
(BOOT-USIGS-RESET). `emit_src` emitted it only when its third argument was
`native`, so the recovery seed that Gforth compiles into hb-stage0 went
without it. That is the whole defect.

### The fix

tools/bootstrap.sh: `emit_src` now emits the prologue unconditionally and the
`mode` parameter is gone, so the recovery seed and the stage sources are one
text. The two call sites that asked for `native` no longer need to, and the
second, now-identical emission of $T/stage2-src is gone.

No engine or mirror code changed. No defer is special-cased, `is` was not
relaxed, nothing was wrapped in TRUSTED: or `0 set-check`, and no caller was
routed around anything.

### Falsification

Oracle: seed built the way `emit_src` builds it, then
`HABU_TARGET=macos-aarch64 gforth -e 'require test/nf.fs s" SEED" slurp-file
s" OUT" FORTH-BUILD-EXE bye'` and `HB_TMP=DIR OUT -- DIR`, about 3 seconds a
turn.

  * Before, with the ZZ reproducer (`defer ZZ-PRETRUST-XT ( -- n )` appended to
    src/core/exec-vector.f plus the checked `is`-installing selftest appended to
    src/core/check-hook.f, exactly as test/pre-trust-defer.f writes them):
    exit 70, `hook: non-certified definition: zz-pretrust-selftest at 'is'`.
  * Before, unpatched: exit 70 at `install`, matching the reported
    src/habu/xref.f red.
  * After: exit 74, `stage2: cannot open source`, and the reproducer prints
    ZZ-OK. docs/debugging.md already names that exit as "got through the whole
    prefix".
  * Mutation: delete the `emit_boot_hide "$out"` line again and the exit-70
    `is` diagnostic comes straight back.

### Regression

tools/bootstrap-codegen-test.f gains BCG-TEST-BOOTSTRAP-PROLOGUE-UNCONDITIONAL,
which checks the structure of `emit_src` rather than counting words: the
prologue call must sit between the head of `emit_src` and its first emitted
line, that span must contain no `if [[` that could gate it again, no
`local mode=` may survive anywhere, the prologue must precede
`cat src/core/util.f`, and no call site may pass a mode. Four mutations were
run against it and each one reds it: dropping the prologue, re-gating it
behind `if [[ "$3" == native ]]`, moving it after the first `cat`, and adding
`native` back to one call site. The clean tree is green.

### Gates

  * tools/bootstrap-codegen-test.f: exit 0, `bootstrap-codegen-test: ok`.
  * tools/typed-local-diff-lint.f on `jj diff --git -r @`: exit 0.
  * tools/error-code-lint.f: exit 0, `1327 file(s), 844 claim(s),
    39 reservation(s), 0 finding(s)`.
  * tools/bootstrap-mirror-lint.f: exit 0.
  * tools/package-diff-lint.f on the same artifact: exit 1, 2 findings, both
    E-PACKAGE-OWNERSHIP against tools/bootstrap-codegen-test.f -- the new test
    word and the pre-existing BCG-MAIN that calls it. Every one of that file's
    thirty-odd sibling BCG-TEST-* words is defined at global scope the same
    way, so this is the known missing category for the bootstrap gate corpus,
    not a finding about this change. No exemption was added.

### How far the full recovery run now gets

`HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` used to
stop at the hb-stage0 boot with exit 70. It now clears the wide-memory gate,
the preflight-recovery gate, the `using` gate, the hb-stage0 seed build and
boot, the stage fixpoint loop (hb-stage and hb-stdin-mk and hb-stdin all
produced), and stops later, in the post-build regression suite the
recovery-built engine runs against itself:
`test/engine-error-package.f` assert 5, "post-seal missing checker fails
closed", expected exit 70 and got 67. That failure reproduces identically
under the installed native bin/hb and is already owned by open dot
habu-restore-fail-closed-4f1d6375, which describes exactly this exit 67
(UNCAUGHT-RC). It was not chased.

### Still red, separately owned

test/pre-trust-defer.f under native bin/hb: asserts 1-4 green (the positive
capture -> drain -> trust row -> checker-defer row -> checked `is` -> runtime
dispatch case, and the table-overflow case), asserts 5-7 red (the UNDRAINED
case expects exit 73 and gets 70). That is the native-engine red the dispatch
message flagged as a different defect; this change touches only
tools/bootstrap.sh and tools/bootstrap-codegen-test.f, neither of which
participates in that test, so it cannot be affected by it.

### Residual soundness gap, dotted

`C-PRETRUST-READY?` still decides "is this load's checker live" by asking "is a
word named `trust` resolvable", which is an existence proxy that a stale
checker satisfies. The fix above removes the situation in which a stale
checker is resolvable, but not the proxy. The structural replacement is a
handshake in which the checker tells the engine that a new checker instance has
begun, so that "ready" means this load's checker. That needs the same primitive
in bootstrap/cg/forth.fs and src/habu/habu2.f plus a token in
src/core/checker.f, which is outside this lane and overlaps the native
undrained-backstop work, so it is recorded as its own dot rather than done here.
