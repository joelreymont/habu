---
title: "Engine: BTHROW no-handler exit masks throw code to 8 bits"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-05T00:00:25.900757+02:00"
closed-at: "2026-07-08T07:30:00+02:00"
close-reason: "completed: BTHROW no-handler + eval-frame LEVLR share one baked reporter (habu2.f EM-EVAL-THROW-RECOVER tail via UNCGH-CELL $3D00); codes in [1,255] exit byte-identically (argv usage 64 / check hook 70 / lint findings 1 contracts preserved), all other codes print 'hb: uncaught throw code N' to fd 2 and exit UNCAUGHT-RC 67; die primitive maps the same way ([0,255] passthrough) closing the DRV-FAIL second layer; stage0 forth.fs mirrors both; regression GE-UNCAUGHT-THROW in the engine runtime slice; proof: full native gate PASS 27582ms, build-fixpoint-test ok, install --force fixpoint green"
---

src/habu/habu1.f BTHROW THROW-NOREC path ends '0 9 0 ADDI, NR-EXIT-GROUP SYS,' so an uncaught top-level throw exits with the RAW code masked by the kernel to 8 bits and prints NOTHING. Proven on current bin/hb: '-2802 throw' at top level exits 14 silently; '-2816 throw' (multiple of 256) exits 0 silently - a fail-open class for any tool relying on throw propagation for its exit status. Fix in the engine emitter: before NR-EXIT-GROUP, report the code on stderr like src/habu/driver-io.f DRV-FAIL ('driver: uncaught throw code N') and clamp the exit status to a deterministic nonzero rc when code & 0xFF is 0 (or always map to a fixed uncaught-throw rc). Needs fixpoint rebuild plus a regression that loads a file throwing -2816 and asserts nonzero exit + diagnostic. Context: found while fixing habu-install-force-exits-09c3c981; tools/build-fixpoint.f now has its own BF-CLI catch+die boundary, but every other --load tool is still exposed.

## Analysis + exact fix spec (2026-07-06, head 1468793a) - engine-side, ROUTED to item-12 after 3b

Re-proven on the head engine: `--load f` with `-2802 throw` exits 14 silent;
`-2816 throw` exits 0 silent (one-line fixture files, spawn + rc check).

MECHANISM (src/habu/habu1.f:1686-1703 BTHROW): x9 = code; inside evaluate ->
LEVALREC; else HND-CELL handler; else THROW-NOH -> REPLH-CELL (tty REPL
recovers via RRECP-CELL); else THROW-NOREC:1703 `0 9 0 ADDI, NR-EXIT-GROUP
SYS,` = exit_group(raw code), kernel truncates to 8 bits, no output. Stage0
mirror has the identical path (bootstrap/cg/forth.fs:636-638 `lnorec LBL, 0 9
0 ADDI, NR-EXIT-GROUP SYS,`).

SECOND MASKED LAYER (same class): driver-io.f DRV-FAIL:107 prints the right
diagnostic but exits `s" " rc die` with the RAW code, so the build-driver
catch boundaries that call it (maker.f:53, build.f:66, stage2.f:67 - comments
even say "exit code stays the throw code") report -2816 as rc 0 WITH a
diagnostic. Both layers need the deterministic mapping.

NO SOUND LIB-SIDE FIX: THROW-NOREC is reached exactly when no handler exists;
no library can interpose without the emitter consulting a cell. Per-tool
BF-CLI-style catch boundaries are the anti-pattern this dot exists to end.

FIX SPEC (one routed engine unit, byte-for-byte fixpoint green):
1. layout.f: `UNCGH-CELL` - new DATA cell (next free slot) holding the
   uncaught-throw reporter entry; `67 constant UNCAUGHT-RC` - deterministic
   uncaught-throw exit status. 67 verified free repo-wide as an exit rc
   (engine fixed exits in use: 64/70/71/74/76/78/83/84/127; 69 collides with
   CHK-E-UNAVAILABLE/FL-E-UPPER throw codes; 77 taken by dup-def-lint die).
2. habu1.f BTHROW THROW-NOREC: load UNCGH-CELL; CBZ -> fallback; else branch
   to the stored entry with the code passed per the RRECP precedent
   (forth.fs:637 branches with x9 = code; habu2.f:2933 stores an ADR label -
   implementer picks the same ABI). Fallback (cell 0, early boot):
   `0 UNCAUGHT-RC MOVZ, NR-EXIT-GROUP SYS,` - silent but deterministic
   nonzero, never 0, never aliased to a real rc.
3. driver-io.f: refactor DRV-FAIL into DRV-UNCAUGHT ( n -- ): keep the
   `driver: uncaught throw code N` fd-2 line (DRV-W2/DRV-FAIL-CODE machinery
   already prints signed decimal correctly), but exit UNCAUGHT-RC, not the
   raw code. Install its entry into UNCGH-CELL at boot (beside the RRECP
   install, habu2.f:2933 pattern). Update maker.f/build.f/stage2.f callers +
   their "exit code stays the throw code" comments.
4. Stage0 mirror forth.fs:638: at minimum the deterministic fallback
   (`0 UNCAUGHT-RC MOVZ,`); full diagnostic optional (no driver-io baked in
   stage0). CHECK_ONLY parity must stay green.
5. REGRESSION (gate engine slice, spawned-hb probes per gate-engine-lib GE-*
   pattern): (a) `--load` file `-2816 throw` -> rc = UNCAUGHT-RC and stderr
   contains `uncaught throw code -2816`; (b) `-2802 throw` -> rc =
   UNCAUGHT-RC (not 14) + `code -2802`; (c) positive control:
   `[: -2816 throw ;] catch` still handled in-process, rc 0; (d) a
   build-driver boundary failure exits UNCAUGHT-RC with the diagnostic.
6. Census: no new PES row (`throw`/`die` already noexec axioms).

Cross-links: the rc-0-after-error `--load` REPL-drop seen in
habu-standalone-support-load-7c3d9f16 is a DIFFERENT path (load-error
recovery prints a diagnostic and continues; nothing reaches THROW-NOREC) -
fixing this dot does not close that one. wait-rc masking
(habu-wait-rc-masks-9ae37cd0) is the same fail-open family on the parent
side; both mappings agree a crashed/thrown child is never rc 0.

## Implementation record (2026-07-08, engine lane)

Landed with these deviations from the 2026-07-06 spec, each proven:

1. RC MAPPING (the spec offered clamp-when-`code&0xFF`-is-0 or always-fixed;
   neither was taken verbatim). Existing callers depend on small positive codes
   passing through to the exit status: lib/argv.f ARGV-FAIL-DONE (`ARGV-E-USAGE
   throw` = 64, the shared usage convention), the standard check hook (`70
   throw`; docs/bootstrap.md documents "a bad definition exits 70"), lint
   finding exits (`1 throw`, tools/trust-lint.f TL-ARGV-BAD-TODAY), and gate
   tests assert those rc's with EMPTY/exact stderr (tools/trust-lint-test.f
   TLT-EXPECT-BAD-TODAY-CLI `erru 0 T=`; tools/json-only usage exact stderr;
   dictionary/checker `package rejects private checked call` rc 70). These
   throws must stay throws (CHECK-CANDIDATE!/PARSE-RC catch them in-process).
   Chosen mapping: exit = code when code is in [1,255] - byte-identical to the
   old behavior, those codes were never masked - else report + UNCAUGHT-RC 67.
   Always-fixed-67 would have broken the documented E-CHECK=70 contract
   repo-wide; clamp-only-when-0 would keep -2802 exiting an aliased 14.
   `0 throw` (previously a silent rc-0 exit, the same fail-open shape) now also
   maps to 67 + report; no in-repo bare `catch throw` passthrough exists.
2. DIAGNOSTIC: printed exactly for the remapped (out-of-range) codes.
   In-range deliberate exits keep their empty-stderr contracts; every exit that
   would otherwise lie is named: `hb: uncaught throw code N` on fd 2.
3. REPORTER PLACEMENT: baked into the engine (tail of habu2.f
   EM-EVAL-THROW-RECOVER; LEVLR and LUNCAUGHT share the entry) instead of the
   spec's driver-io.f-installed hook - covers every tool, not just driver
   contexts, and adds NO new TRUST row (folded into the existing trusted
   emitter). This also fixed the eval-frame LEVLR no-handler exit, which had
   the identical raw-exit bug.
4. UNCGH-CELL is $3D00, not a "next free slot" low hole: $36B0 looked free in
   layout.f but is FRFREE-CELL (src/habu/regalloc.f, JIT float-pool bitmask) -
   proven by a PC=0xff jump under lldb. lib/task.f TASK-USER-BASE moved
   $3D00->$3D08 (task suite green). layout.f is NOT the only owner of fixed
   DATA cells; regalloc.f/debug.f define more.
5. SECOND MASKED LAYER: closed at the `die` primitive itself (habu1.f BDIE maps
   [0,255] passthrough, 0 stays the deliberate success exit, else 67) rather
   than refactoring DRV-FAIL - driver-io.f is also loaded by
   tools/object-image.f WITHOUT src/habu/layout.f, so it cannot reference
   UNCAUGHT-RC; the primitive covers DRV-FAIL, GE-EVAL-FORK-EXIT, and every
   future `rc die` site. DRV-FAIL diagnostic + rc contract unchanged for
   representable codes (maker/build/stage2 comments updated).
6. Stage0 bootstrap/cg/forth.fs mirrors both BTHROW lnorec and BDIE with the
   same mapping (no reporter in stage0, per spec item 4).
7. Regression: test/gate-engine-lib.f GE-UNCAUGHT-THROW (engine runtime slice,
   GE-RUNTIME-CHECKS): -2816 -> 67 + diagnostic (was rc 0 silent), -2802 -> 67
   + diagnostic (was rc 14 silent), 70 -> 70 silent passthrough, caught -2816
   stays in-process rc 0. Proven RED on the old engine (exit 0/14, silent),
   GREEN after the fixpoint rebuild.
8. Census: no new PES rows; no new TRUSTED:/TRUST/0 set-check sites.
