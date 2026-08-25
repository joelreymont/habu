---
title: Fix gforth recovery broken at current tip
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T16:31:12.700314+02:00"
closed-at: "2026-08-22T23:58:57.612143+02:00"
close-reason: implemented, reviewed, merged, gates green: the stage0 mirror publishes created-word effects from the definer through trust-raw under the qualified body-buffer name (a69a1909), integrated as 1a49d55f and landed on master 85fac4cb; recovery run HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh on that tree printed 'bootstrap OK: bin/hb' rc 0 on the Linux host; four stage0 negatives red on their mutants and reproduced byte-for-byte by the native engine; maki suite, lint-libs slice, both diff lints, error-code-lint, dot-dep-lint green on the landed tree.
---

Discovered 2026-07-21 during the boot-compat healing: HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh FAILS at current master - gforth aborts in test/bootstrap-wide-memory.fs:12 (c-abort backtrace through the 0.7.9 kernel). The no-binary recovery path is therefore broken exactly when it would be needed. The gforth install itself is healthy (verified end-to-end green this morning at an older commit), so this is a source regression in the recovery leg - most likely from the recent structure-make/certify-cycle landings touching bootstrap/cg or the wide-memory bootstrap test. Bisect the recovery run between the last-known-good commit and tip, fix at the root (the recovery leg must track the boot-file additions), and re-prove bootstrap.sh end-to-end to the byte-fixpoint. This is disaster-recovery infrastructure: treat as P1.

## CONTRACT (2026-08-22, re-bisected on the Linux host; the July finding above was this same defect)

Problem: the Gforth-hosted stage0 mirror bootstrap/cg/forth.fs never mirrors the
native definer-side effect publication. Native src/habu/habu2.f publishes a
created word's effect FROM ITS DEFINER: package LASTC-TRUST (habu2.f:2329-2375)
PUBLISH-PTR-A registers `-- ptr a` through the checker's `trust-raw` registrar
(checker.f TRUST-RAW) at the tail of C-CREATE (habu2.f:3162-3165, inherited by
C-VARIABLE) and PUBLISH-A registers `-- a` at the tail of C-CONSTANT
(habu2.f:3195). The mirror's C-CREATE (forth.fs:3391), C-VARIABLE (3393) and
C-CONSTANT (3398-3412) publish no trust row at all; their words' effects came
only from the check hook inferring the seeded body "NAME create" / "NAME
constant", i.e. from the `PRIM: create` / `PRIM: constant` axiom rows. Commit
760e9c90 (2026-08-14, "checker: state the create axiom's real effect") corrected
the create row to `( -- )` - the true effect of CALLING create, see
test/create-axiom-test.f header - so under the mirror every `create`d word and
every `variable` now certifies as `( -- )`.
Evidence: bisect over 155 commits, first bad 760e9c90 (one line); reverting
only that line at master makes the probe pass; a probe `: ZZ-A ( -- n ) ARMED @ ;`
inserted in src/core/generated-declaration.f fails at '@'. Sandbox is not the
variable (identical unsandboxed). Observable at master: `HABU_TARGET=linux-aarch64
gforth test/bootstrap-wide-memory.fs` aborts at :12 and /tmp/nf-bin exits 70
"hook: non-certified definition: pick-reason at 'CODE-REASON'"; tools/bootstrap.sh
dies in bootstrap_wide_gate. No scheduled gate runs the recovery leg, so the
create-axiom lane (760e9c90 + test 8bb57941) landed without it.

Fix (design, mirror the native seam exactly, nothing else):
1. bootstrap/cg/forth.fs gains the `trust-raw` keyword bytes + label (native
   KWDATA:LKWTRUSTRAW, habu2.f:1900/1950/8843) and the `-- ptr a` / `-- a`
   signature byte labels (native LSIGPTRA / LSIGA, habu2.f:1847/1950/8854).
2. A FIND-RAW resolver shaped like the mirror's C-FIND-TRUST-DECL
   (forth.fs:2865) that resolves `trust-raw` and exits 70 naming it on fd 2 when
   missing (never publishes unsealed).
3. PUBLISH-PTR-A and PUBLISH-A mirroring habu2.f:2356-2375 byte-for-byte in
   shape: HOOK-CELL guard, FIND-RAW, x12 = the just-published record,
   C-PUSH-DREC-NAME, push sig ptr + len, C-CALL-X11-SAVED.
4. Call PUBLISH-PTR-A at the tail of the mirror's C-CREATE (so C-VARIABLE
   inherits it, as native) and PUBLISH-A at the tail of C-CONSTANT after its
   C-DEFHOOK, in native order. Verify the mirror's create and constant paths
   leave the record address where the publish expects it (native stores
   LASTC-CELL before publishing: habu2.f:3150, 3191; the mirror's C-CONSTANT
   tail has no such store - mirror it if the publish needs it).
Forbidden: touching the `PRIM: create` row or any axiom row; TRUSTED: / 0
set-check wrappers; name special-casing; edits to src/habu/habu2.f (native is
correct); shrinking or bypassing any recovery gate.

Acceptance:
A. `HABU_TARGET=linux-aarch64 gforth test/bootstrap-wide-memory.fs` exits 0 with
   /tmp/nf-out = "ok" on the fixed tree, axiom rows untouched.
B. A stage0 recovery regression, through the real recovery load path (a stage0
   source in test/ wired into tools/bootstrap.sh as bootstrap_wide_gate is, or
   cases added to test/bootstrap-wide-memory-src.f): a `create`d word, a
   `variable` and a `constant` certify in CHECKED callers as `-- ptr a`,
   `-- ptr a`, `-- a`; a checked caller declaring the wrong effect for one of
   them is refused. Shown red on the unfixed tree (the wide gate already dies
   there; the new cases must be shown to discriminate, e.g. on the tree with
   only the axiom line reverted) and green on the fixed tree.
C. Full `HABU_ALLOW_BOOTSTRAP=1 GFORTH=$HOME/.local/bin/gforth tools/bootstrap.sh`
   runs to `bootstrap OK: bin/hb` with the install fixpoint byte-identical. If
   it stops LATER at a different, separately owned failure (the July report
   named test/engine-error-package.f assert 5, dot
   habu-restore-fail-closed-4f1d6375), STOP and report the exact failure; do not
   chase it.
D. With the recovered bin/hb: tools/bootstrap-mirror-lint.f and
   tools/bootstrap-codegen-test.f exit 0; the typed-local and package diff
   lints exit 0 on the diff.
E. Report answers the Checker-Miss question: which structural gate would have
   refused 760e9c90 without its mirror half (e.g. a bootstrap-codegen-test
   invariant: every LASTC-TRUST:PUBLISH* call site in habu2.f has a mirror
   counterpart, read through the real lexer). Proposal only; becomes a
   follow-up dot.
Files: bootstrap/cg/forth.fs; test/bootstrap-wide-memory-src.f or a new stage0
fixture + tools/bootstrap.sh gate; docs/bootstrap.md only if the procedure
changes.
Verify: the commands in A, C, D. /tmp/nf-bin and /tmp/nf-out are fixed shared
paths: one recovery probe at a time on the machine.
Depends: none. Blocks: every lane on this host (no engine exists without it).
Ownership: bootstrap/cg/forth.fs (mirror), the recovery fixtures.
Claim: agent=recovery-mirror workspace=.jj-ws/habu-fix-gforth-recovery-9269e3a3
