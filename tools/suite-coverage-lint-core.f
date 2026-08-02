\ suite-coverage-lint-core.f - derive & check the stdlib gate suite lists.
\
\ Problem this guards (dot habu-derive-inprocess-spawned-a54e760d): the spawned
\ TEST:SUITE inventory (test/gate-stdlib-cases.f) and the resident in-process GSI
\ groups (test/gate-stdlib-inline-lib.f / test/gate-stdlib-lint-tools.f) are
\ hand-synced copies that have silently drifted - a cases suite whose members are
\ mirrored into no scheduled GSI or candidate group runs in no automatic gate, and the
\ inprocess ptx-toolchain list can diverge from its spawned superset.
\
\ Policy is DERIVED, not transcribed: the lint parses the three gate files each
\ run and enforces -
\   (a) every TEST:SUITE member file is either scheduled (mirrored into a run.f
\       in-process GSI include/require group or candidate validation), documented spawn-only, or
\       documented manual-gate - otherwise SUITE-ORPHAN.
\   (b) the in-process GSI-LINT-LIBS-PTX-TOOL list == the spawned ptx-toolchain
\       member list MINUS the documented spawn-only set - any other divergence is
\       PTX-TOOL-MISSING (spawned unit absent inprocess) or PTX-TOOL-EXTRA
\       (inprocess entry not a spawned unit, or a spawn-only file run inprocess).
\   (c) the documented MANUAL-GATE and SPAWN-ONLY sets live in ONE checked table
\       here, cross-checked for staleness, so adding a suite/file forces a
\       conscious decision (a new unrouted member goes red).
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/intern.f, tools/lint/token.f, tools/lint/lib.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f

package SUITE-COVERAGE-LINT

$8000 constant SC-CASES-CAP        \ test/gate-stdlib-cases.f source (resident)
$20000 constant SC-SCAN-CAP        \ reused GSI-file scan buffer; grown 64K->128K as test/gate-engine-lib.f (the authoritatively scanned engine gate) passed the old cap
$800 constant SC-PTX-BUF-CAP       \ copied inprocess ptx-toolchain file names
320 constant SC-CASE-MAX           \ (label,file) member rows; 167 live + headroom
48 constant SC-PTX-MAX             \ inprocess ptx-toolchain files
34 constant SC-DQUOTE
16 constant SC-USE-MAX              \ mirror engine USE-MAX: concurrent `using` scopes tracked while linting

create SC-CASES-BUF SC-CASES-CAP allot
create SC-SCAN-BUF  SC-SCAN-CAP  allot
create SC-PTX-BUF   SC-PTX-BUF-CAP allot
create SC-NUM 32 allot

\ CASES rows: (label,file) pairs, pointers into SC-CASES-BUF (or a fixture string)
create SC-CL-A SC-CASE-MAX cells allot    \ label ptr
create SC-CL-U SC-CASE-MAX cells allot    \ label len
create SC-CF-A SC-CASE-MAX cells allot    \ file ptr
create SC-CF-U SC-CASE-MAX cells allot    \ file len
create SC-PTX-OFF SC-PTX-MAX cells allot  \ inprocess ptx file offset into SC-PTX-BUF
create SC-PTX-LEN SC-PTX-MAX cells allot  \ inprocess ptx file len

variable SC-CASE#
variable SC-SUITE#
variable SC-PTX#
variable SC-PTX-USED
variable SC-FIND
variable SC-REPORT?
variable SC-INBLOCK
variable SC-INDEF
variable SC-CUR-LABEL          \ ptr slot for current suite label
variable SC-CUR-LABEL-U
variable SC-QF                 \ ptr slot for a table-membership query target
variable SC-QF-U
variable SC-QF-HIT
variable SC-NUM-L

\ `using TEST` scope state (see SC-USE-* below): file-local, reset per scan.
create SC-USE-STK SC-USE-MAX cells allot    \ per-open `using`: 1 if it imported TEST
variable SC-USE-SP                            \ open-using stack depth
variable SC-USE-TEST#                         \ count of currently-open usings that imported TEST

: SC-NL ( -- ) 10 emit ;

: SC-U. ( n -- )
   0 SC-NUM-L !
   dup 0= if drop 48 emit exit then
   begin dup 0 > while
      dup 10 mod 48 + SC-NUM SC-NUM-L @ + c!
      10 / SC-NUM-L @ 1+ SC-NUM-L !
   repeat drop
   begin SC-NUM-L @ 0 > while
      SC-NUM-L @ 1- SC-NUM-L !
      SC-NUM SC-NUM-L @ + c@ emit
   repeat ;

\ ---- CASES row store -------------------------------------------------------
: SC-CL-A-FIELD ( n -- ptr ptr u8 ) cells SC-CL-A + 0 ptr-field ;
: SC-CF-A-FIELD ( n -- ptr ptr u8 ) cells SC-CF-A + 0 ptr-field ;

: SC-CASE+ ( ptr u8 n ptr u8 n -- ) {: lp:ptr lu:n fp:ptr fu:n :}
   SC-CASE# @ SC-CASE-MAX >= if E-TBL-BOUNDS throw then
   lp SC-CASE# @ SC-CL-A-FIELD !
   lu SC-CASE# @ cells SC-CL-U + !
   fp SC-CASE# @ SC-CF-A-FIELD !
   fu SC-CASE# @ cells SC-CF-U + !
   SC-CASE# @ 1+ SC-CASE# ! ;

: SC-CASE-LABEL$ ( n -- ptr u8 n ) {: k:n :}
   k SC-CL-A-FIELD @  k cells SC-CL-U + @ ;

: SC-CASE-FILE$ ( n -- ptr u8 n ) {: k:n :}
   k SC-CF-A-FIELD @  k cells SC-CF-U + @ ;

\ ---- current suite label ---------------------------------------------------
: SC-CUR-LABEL-A-FIELD ( -- ptr ptr u8 ) SC-CUR-LABEL 0 ptr-field ;

: SC-CUR-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SC-CUR-LABEL-A-FIELD !  u SC-CUR-LABEL-U ! ;

: SC-CUR-LABEL$ ( -- ptr u8 n )
   SC-CUR-LABEL-A-FIELD @ SC-CUR-LABEL-U @ ;

\ ---- inprocess ptx-toolchain store -----------------------------------------
: SC-PTX-INPROC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SC-PTX# @ SC-PTX-MAX >= if E-TBL-BOUNDS throw then
   SC-PTX-USED @ u + SC-PTX-BUF-CAP > if E-TBL-BOUNDS throw then
   a  SC-PTX-BUF SC-PTX-USED @ +  u LINT-BMOVE
   SC-PTX-USED @ SC-PTX-OFF SC-PTX# @ cells + !
   u SC-PTX-LEN SC-PTX# @ cells + !
   SC-PTX-USED @ u + SC-PTX-USED !
   SC-PTX# @ 1+ SC-PTX# ! ;

: SC-PTX-INPROC$ ( n -- ptr u8 n ) {: k:n :}
   SC-PTX-BUF SC-PTX-OFF k cells + @ +  SC-PTX-LEN k cells + @ ;

: SC-PTX-INPROC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SC-PTX# @ < while
      dup SC-PTX-INPROC$ a u LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

\ ---- scheduled set (the interner) ------------------------------------------
: SC-SCHED+ ( ptr u8 n -- ) INTERN drop ;
: SC-SCHEDULED? ( ptr u8 n -- bool ) INTERN? ;

\ ============================================================================
\ (c) THE ONE CHECKED TABLE: documented MANUAL-GATE and SPAWN-ONLY member files.
\ ----------------------------------------------------------------------------
\ MANUAL-GATE: cases members that run only in the standalone `test/gate-stdlib.f`
\ full/slice merge gate (SUITE-ALL or a spawned -- <slice>), NOT in the resident
\ automatic test/run.f fast tier. Each is here on purpose; a NEW unrouted member
\ is an orphan, not a silent skip.
\ typed-local-lint: allow-bare-local - q carries the quotation effect from the
\ stack signature; it names each documented file to the caller-supplied action.
: SC-MANUAL-TABLE ( [ ptr u8 n -- ] -- ) {: q :}
   \ pure library/unit suites not mirrored into the resident fast tier
   s" lib/adt/option-test.f" q execute
   s" lib/adt/result-test.f" q execute
   s" lib/cad-num-arithmetic-test.f" q execute
   s" lib/engine-id-test.f" q execute
   s" lib/ffi-test.f" q execute
   s" lib/float-test.f" q execute
   s" lib/fmath-test.f" q execute
   s" lib/fmt-test.f" q execute
   s" lib/hashmap-test.f" q execute
   s" lib/object-cache-test.f" q execute
   s" lib/object-index-test.f" q execute
   s" lib/object-link-test.f" q execute
   s" lib/object-resolve-test.f" q execute
   s" lib/prelude-test.f" q execute
   s" lib/sort-test.f" q execute
   s" lib/stats-test.f" q execute
   s" lib/test/src-shape-test.f" q execute
   s" lib/layout/box-test.f" q execute
   \ stdin/argv/subprocess suites needing a real spawned image
   s" lib/argv-test.f" q execute
   s" lib/source-test.f" q execute
   s" tools/standalone-load-test.f" q execute
   s" test/lint-cli-standalone-load.f" q execute
   \ pthread-live tasking suites isolated to the spawned gate
   s" lib/task-test.f" q execute
   s" test/atomics-smoke.f" q execute
   s" test/run-in-stack-smoke.f" q execute
   s" test/getpid-smoke.f" q execute
   s" test/proc-watch-smoke.f" q execute
   s" test/proc-signal-smoke.f" q execute
   \ checked-fork wrapper suite: a real fork probe (0 in child / pid in
   \ parent) plus SET-PGID; spawn-only like the sibling proc smokes.
   s" lib/process-fork-test.f" q execute
   s" test/process-pty-io-smoke.f" q execute
   s" test/engine-candidate-test.f" q execute
   \ device/codegen-weight PTX suites excluded from the resident inprocess groups
   s" lib/ptx/ad-gen-test.f" q execute
   s" lib/ptx/autograd-neg-test.f" q execute
   s" lib/ptx/uniform-barrier-test.f" q execute
   s" lib/ptx/cuda-driver-test.f" q execute
   s" lib/ptx/cuda-scope-test.f" q execute
   s" lib/ptx/sentinel-test.f" q execute
   s" src/arch/ptx/vjp-test.f" q execute
   \ heavy build/image suites too costly for the run.f fast-tier budget
   \ (build-fixpoint-fixtures: multiple full engine builds - dot: manual/slow)
   s" tools/build-fixpoint-test.f" q execute
   s" tools/hb-build-test.f" q execute
   s" lib/build-cache-test.f" q execute
   s" lib/codesign-test.f" q execute
   s" test/boot-pin-test.f" q execute
   \ pre-trust defer regressions (dot habu-engine-pre-trust-77410827): four
   \ spawned child-engine boots against a patched src-tree copy — engine-boot
   \ weight (~1s, boot-pin-test class), and the pinned properties only move
   \ when the engine defer machinery / reserved layout changes. Runs in the
   \ spawned stdlib-cases suite (label-selected) and standalone
   \ (bin/hb --load test/pre-trust-defer.f); measured too costly for the
   \ fast-tier fork groups.
   s" test/pre-trust-defer.f" q execute
   s" tools/object-image-test.f" q execute
   s" tools/imgdump-test.f" q execute
   s" tools/imagedisasm-test.f" q execute
   \ shadow-lint fixtures: shadow-lint.f is GSI-INCLUDEd in the scheduled
   \ lint-tools/repo fork, but its -test.f fixtures run only in the standalone
   \ `lint`/`lint-tools` slice (candidate to wire later; documented for now)
   s" tools/lint/shadow-lint-test.f" q execute
   \ misc test/-tree suites not mirrored into resident groups
   s" test/load-reject-diag-test.f" q execute
   \ snapshot-writer adversarial suite: builds two full snapshots in child
   \ processes (return-stack zeroing + fail-closed on a failed close), too
   \ costly for the resident fast tier - runs in the standalone stdlib gate.
   s" test/snapshot-writer.f" q execute
   \ flat-lib standalone-load gate: spawns ~50 child engines to bare-load every
   \ flat lib/<module>.f (dot habu-pkg-stdlib-modules-3197dbbc). ~20s of process
   \ spawns, too costly for the resident fast tier - runs in the standalone
   \ stdlib gate.
   s" test/stdlib-standalone-load.f" q execute
   \ protected-WID boot-integration suite: spawns test/aot-wid-build.f to build a
   \ full variant engine (~12s), too costly for the fast tail-process fork tier
   \ (its 11.1s pool ratchet) - runs in the standalone stdlib gate like the
   \ snapshot-writer suite above.
   s" test/aot-wid-suite.f" q execute
   \ framed-diff codec fixtures: a self-contained in-memory round-trip/reject
   \ suite (diff-frame-codec). It runs spawned in the standalone stdlib gate via
   \ the SUITE-ALL selection, and is not mirrored into the resident fast tier.
   s" tools/lint/diff-frame-test.f" q execute
   \ shared unified-diff parser fixtures (diff-parser): a self-contained in-memory
   \ event/fail-closed suite over tools/lint/diff.f. Runs spawned in the standalone
   \ stdlib gate via SUITE-ALL; not mirrored into the resident fast tier.
   s" tools/lint/diff-test.f" q execute
   \ lint interner set fixtures (lint-intern-set): in-memory coverage of the
   \ tools/lint/intern.f set helpers and the LINT-MAIN attributed-failure seam.
   \ Runs spawned in the standalone stdlib gate via SUITE-ALL; not resident.
   s" tools/lint/set-test.f" q execute ;

\ SPAWN-ONLY: ptx-toolchain cases members that must run only in a fresh spawned
\ image, never in the resident full-runner image. Two reasons appear here: tools
\ that SIGBUS when loaded into the resident image (device/bench compile-checks,
\ device-SKIP off-device), and the perf-regress CLI, whose registry path is
\ resolved from ambient SCRIPT-ARGV - clean only in a fresh spawn, misread as a
\ path in the resident image (its argv-free scan runs inprocess via
\ perf-regress-test.f). Acceptance (b) subtracts exactly this set from the
\ spawned ptx-toolchain list.
\ typed-local-lint: allow-bare-local - q carries the quotation effect.
: SC-SPAWN-ONLY-TABLE ( [ ptr u8 n -- ] -- ) {: q :}
   \ perf-regress CLI: reads SCRIPT-ARGV for its registry path, so resident-image
   \ loading would mis-read the harness argv; the inprocess scan is perf-regress-test.f
   s" tools/ptx/perf-regress.f" q execute
   s" tools/ptx/bandwidth-lib-test.f" q execute
   s" tools/ptx/mma-exact-lib-test.f" q execute
   s" tools/ptx/autotune-sweep-test.f" q execute
   s" tools/ptx/fusion-compare.f" q execute
   s" tools/ptx/gemm-bench.f" q execute
   s" tools/ptx/attention-bench.f" q execute
   \ device-leg tools: emit+ptxas+launch gated on CUDA:OPEN? (recorded off-device
   \ SKIP); they compile-check in the fresh spawned image only, device-run on the
   \ Orin via tools/ptx/zed-device-suite.f - never in the resident run.f image.
   s" tools/ptx/acc-device-test.f" q execute
   s" tools/ptx/redadd-device-test.f" q execute
   s" tools/ptx/saxpy-v4-tail-device-test.f" q execute
   s" tools/ptx/device-gold.f" q execute
   s" tools/ptx/sum-launch.f" q execute
   s" tools/ptx/softmax-launch.f" q execute
   s" tools/ptx/softmax-gradcheck.f" q execute
   s" tools/ptx/rmsnorm-device-test.f" q execute
   s" tools/ptx/rope-device-test.f" q execute
   s" tools/ptx/layernorm-device-test.f" q execute
   s" tools/ptx/swiglu-device-test.f" q execute
   s" tools/ptx/cuda-launch.f" q execute ;

\ closure-run lint cores: their suite member is the CLI tool file, exercised by a
\ scheduled `[: XXX-LINT ;] GSI-RUN` in the lint-tools group (the file itself is
\ not GSI-INCLUDEd). Count them scheduled.
: SC-SCHED-CLOSURE-CORES ( -- )
   s" tools/repl-lint.f" SC-SCHED+
   s" tools/dot-dep-lint.f" SC-SCHED+
   s" tools/maki-dep-lint.f" SC-SCHED+
   s" tools/suite-coverage-lint.f" SC-SCHED+
   s" tools/namespace-lint.f" SC-SCHED+
   s" tools/error-code-lint.f" SC-SCHED+ ;

\ ---- table membership ------------------------------------------------------
: SC-QF! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SC-QF 0 ptr-field !  u SC-QF-U !  0 SC-QF-HIT ! ;

: SC-QF$ ( -- ptr u8 n )
   SC-QF 0 ptr-field @  SC-QF-U @ ;

: SC-QF-MATCH ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SC-QF$ LINT-STR= if -1 SC-QF-HIT ! then ;

: SC-MANUAL? ( ptr u8 n -- bool )
   SC-QF!
   [: SC-QF-MATCH ;] SC-MANUAL-TABLE
   SC-QF-HIT @ 0 <> ;

: SC-SPAWN-ONLY? ( ptr u8 n -- bool )
   SC-QF!
   [: SC-QF-MATCH ;] SC-SPAWN-ONLY-TABLE
   SC-QF-HIT @ 0 <> ;

\ ---- CASES membership queries ----------------------------------------------
: SC-CASE-MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SC-CASE# @ < while
      dup SC-CASE-FILE$ a u LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: SC-PTX-SPAWNED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SC-CASE# @ < while
      dup SC-CASE-LABEL$ s" ptx-toolchain" LINT-STR= if
         dup SC-CASE-FILE$ a u LINT-STR= if drop LINT-TRUE exit then
      then
      1+
   repeat drop LINT-FALSE ;

\ ============================================================================
\ Parsers - policy derived from the live gate files each run.
\ ----------------------------------------------------------------------------
: SC-STRIP-Q ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 > a u 1- + c@ SC-DQUOTE = and if a u 1- exit then
   a u ;

\ ---- `using TEST` scope tracker --------------------------------------------
\ A migrated manifest reads `using TEST … SUITE foo … ;SUITE … ;using`: the
\ loader resolves a bare SUITE/;SUITE/SUITE-STDIN as the TEST verb ONLY while a
\ `using TEST` scope is open. The lint walks manifests textually, so it must read
\ them the same way and recognize BOTH the qualified `TEST:SUITE` spelling and the
\ bare-in-using-scope spelling - going blind to neither. This mirrors the engine's
\ using/;using/;package scoping (file-local, reset per scan, bounded by USE-MAX).
: SC-USE-RESET ( -- )  0 SC-USE-SP !  0 SC-USE-TEST# ! ;

: SC-USE-TEST? ( -- bool )  SC-USE-TEST# @ 0 > ;

: SC-USE-PUSH ( n -- ) {: t:n :}            \ t nonzero if this `using` imported TEST
   SC-USE-SP @ SC-USE-MAX >= if exit then    \ past USE-MAX the engine rejects; drop the overflow frame
   t SC-USE-STK SC-USE-SP @ cells + !
   t 0 <> if SC-USE-TEST# @ 1+ SC-USE-TEST# ! then
   SC-USE-SP @ 1+ SC-USE-SP ! ;

: SC-USE-POP ( -- )                          \ `;using` closes the most recent `using`
   SC-USE-SP @ 0= if exit then
   SC-USE-SP @ 1- SC-USE-SP !
   SC-USE-STK SC-USE-SP @ cells + @ 0 <> if SC-USE-TEST# @ 1- SC-USE-TEST# ! then ;

: SC-USE-NEXT-TEST? ( n -- n ) {: k:n :}     \ 1 if the token after `using` at k names TEST
   k 1+ TN# @ >= if 0 exit then
   k 1+ TOK s" TEST" LINT-STR= if 1 else 0 then ;

: SC-USE-TOK ( n -- ) {: k:n :}              \ maintain the using-scope stack across token k
   k TOK s" using" LINT-STR= if k SC-USE-NEXT-TEST? SC-USE-PUSH exit then
   k TOK s" ;using" LINT-STR= if SC-USE-POP exit then
   k TOK s" ;package" LINT-STR= if SC-USE-RESET then ;

\ suite header / terminator recognizers: qualified TEST:… always; bare … only
\ while `using TEST` is open.
: SC-SUITE-OPEN? ( n -- bool ) {: k:n :}
   k TOK s" TEST:SUITE" LINT-STR= if LINT-TRUE exit then
   k TOK s" TEST:SUITE-STDIN" LINT-STR= if LINT-TRUE exit then
   SC-USE-TEST? if
      k TOK s" SUITE" LINT-STR= if LINT-TRUE exit then
      k TOK s" SUITE-STDIN" LINT-STR= if LINT-TRUE exit then
   then
   LINT-FALSE ;

: SC-SUITE-CLOSE? ( n -- bool ) {: k:n :}
   k TOK s" TEST:;SUITE" LINT-STR= if LINT-TRUE exit then
   SC-USE-TEST? if k TOK s" ;SUITE" LINT-STR= if LINT-TRUE exit then then
   LINT-FALSE ;

: SC-MK-SUITE? ( n -- bool ) {: k:n :}       \ maki header: qualified TEST:SUITE or bare SUITE in TEST scope
   k TOK s" TEST:SUITE" LINT-STR= if LINT-TRUE exit then
   SC-USE-TEST? if k TOK s" SUITE" LINT-STR= if LINT-TRUE exit then then
   LINT-FALSE ;

\ ---- cases file: SUITE label + beginning-of-line .f members (qualified or bare)
: SC-OPEN-SUITE ( n -- ) {: k:n :}
   k 1+ dup TN# @ >= if drop exit then
   TOK SC-CUR-LABEL!
   -1 SC-INBLOCK !
   SC-SUITE# @ 1+ SC-SUITE# ! ;

: SC-ADD-MEMBER ( n -- ) {: k:n :}
   SC-CUR-LABEL$ k TOK SC-CASE+ ;

: SC-CASES-TOK ( n -- ) {: k:n :}
   k SC-USE-TOK
   k SC-SUITE-OPEN? if k SC-OPEN-SUITE exit then
   k SC-SUITE-CLOSE? if 0 SC-INBLOCK ! exit then
   SC-INBLOCK @ 0= if exit then
   k TOK0? 0= if exit then                 \ members are the first token on a line
   k TOK s" .f" HAS-EXT? if k SC-ADD-MEMBER then ;

: SC-CASES-SCAN$ ( ptr u8 n -- )
   LINT-TRUE PARENS? ! TOKENIZE
   0 SC-INBLOCK !
   SC-USE-RESET
   0 begin dup TN# @ < while
      dup SC-CASES-TOK
      1+
   repeat drop ;

\ ---- gate files: files run through a resident or candidate gate --------------
: SC-SCHED-VERB? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" GSI-INCLUDE" LINT-STR= if LINT-TRUE exit then
   a u s" GSI-FORK-INCLUDE" LINT-STR= if LINT-TRUE exit then
   a u s" GSI-REQUIRE" LINT-STR= if LINT-TRUE exit then
   a u s" GE-SRC-FILE+" LINT-STR= ;

: SC-SCHED-TOK ( n -- ) {: k:n :}
   k TOK SC-SCHED-VERB? 0= if exit then
   k 1- TOK SC-STRIP-Q 2dup s" .f" HAS-EXT? if SC-SCHED+ else 2drop then ;

: SC-SCHED-SCAN$ ( ptr u8 n -- )
   LINT-TRUE PARENS? ! TOKENIZE
   1 begin dup TN# @ < while
      dup SC-SCHED-TOK
      1+
   repeat drop ;

\ ---- candidate validation manifest: nearest .f token before each RUN-CASE ------
: SC-CAND-PATH ( n -- ) {: k:n :}
   k 1- begin dup 0 >= while
      dup TOK s" RUN-CASE" LINT-STR= if drop exit then
      dup TOK SC-STRIP-Q 2dup s" .f" HAS-EXT? if
         SC-SCHED+
         drop exit
      then
      2drop 1-
   repeat drop ;

: SC-CAND-TOK ( n -- ) {: k:n :}
   k TOK s" RUN-CASE" LINT-STR= if k SC-CAND-PATH then ;

: SC-CAND-SCAN$ ( ptr u8 n -- )
   LINT-TRUE PARENS? ! TOKENIZE
   1 begin dup TN# @ < while
      dup SC-CAND-TOK
      1+
   repeat drop ;

\ ---- inprocess ptx-toolchain list: the GSI-LINT-LIBS-PTX-TOOL definition body -
: SC-PTX-DEF-START? ( n -- bool ) {: k:n :}
   k TOK s" GSI-LINT-LIBS-PTX-TOOL" LINT-STR=
   k 1- TOK s" :" LINT-STR= and ;

: SC-PTX-TOK ( n -- ) {: k:n :}
   k SC-PTX-DEF-START? if -1 SC-INDEF ! exit then
   SC-INDEF @ 0= if exit then
   k TOK s" ;" LINT-STR= if 0 SC-INDEF ! exit then
   k TOK s" GSI-INCLUDE" LINT-STR= if
      k 1- TOK SC-STRIP-Q 2dup s" .f" HAS-EXT? if SC-PTX-INPROC+ else 2drop then
   then ;

: SC-PTX-INPROC-SCAN$ ( ptr u8 n -- )
   LINT-TRUE PARENS? ! TOKENIZE
   0 SC-INDEF !
   1 begin dup TN# @ < while
      dup SC-PTX-TOK
      1+
   repeat drop ;

\ ============================================================================
\ Findings + checks
\ ----------------------------------------------------------------------------
: SC-FIND+ ( -- ) SC-FIND @ 1+ SC-FIND ! ;

: SC-ORPHAN ( ptr u8 n ptr u8 n -- ) {: lp:ptr lu:n fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: SUITE-ORPHAN " type lp lu type
      s"  member " type fp fu type
      s"  runs in no scheduled group and is not documented manual/spawn-only" type SC-NL
   then SC-FIND+ ;

: SC-PTX-MISSING ( ptr u8 n -- ) {: fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: PTX-TOOL-MISSING " type fp fu type
      s"  is a spawned ptx-toolchain unit but absent from inprocess GSI-LINT-LIBS-PTX-TOOL" type SC-NL
   then SC-FIND+ ;

: SC-PTX-EXTRA ( ptr u8 n -- ) {: fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: PTX-TOOL-EXTRA " type fp fu type
      s"  is in inprocess GSI-LINT-LIBS-PTX-TOOL but not a non-spawn-only ptx-toolchain unit" type SC-NL
   then SC-FIND+ ;

: SC-MANUAL-STALE ( ptr u8 n ptr u8 n -- ) {: fp:ptr fu:n rp:ptr ru:n :}
   SC-REPORT? @ if
      s" suite-coverage: MANUAL-STALE " type fp fu type
      s"  " type rp ru type SC-NL
   then SC-FIND+ ;

: SC-SPAWN-STALE ( ptr u8 n ptr u8 n -- ) {: fp:ptr fu:n rp:ptr ru:n :}
   SC-REPORT? @ if
      s" suite-coverage: SPAWN-ONLY-STALE " type fp fu type
      s"  " type rp ru type SC-NL
   then SC-FIND+ ;

\ (a) every cases member file is scheduled / spawn-only / manual, else orphan
: SC-CHECK-ORPHAN-I ( n -- ) {: k:n :}
   k SC-CASE-FILE$ {: fp:ptr fu:n :}
   fp fu SC-SCHEDULED? if exit then
   fp fu SC-SPAWN-ONLY? if exit then
   fp fu SC-MANUAL? if exit then
   k SC-CASE-LABEL$ fp fu SC-ORPHAN ;

: SC-CHECK-ORPHANS ( -- )
   0 begin dup SC-CASE# @ < while dup SC-CHECK-ORPHAN-I 1+ repeat drop ;

\ (b) inprocess ptx == spawned ptx minus spawn-only
: SC-CHECK-PTX-MISSING-I ( n -- ) {: k:n :}
   k SC-CASE-LABEL$ s" ptx-toolchain" LINT-STR= 0= if exit then
   k SC-CASE-FILE$ {: fp:ptr fu:n :}
   fp fu SC-SPAWN-ONLY? if exit then
   fp fu SC-PTX-INPROC? if exit then
   fp fu SC-PTX-MISSING ;

: SC-CHECK-PTX-EXTRA-I ( n -- ) {: k:n :}
   k SC-PTX-INPROC$ {: fp:ptr fu:n :}
   fp fu SC-PTX-SPAWNED? 0= if fp fu SC-PTX-EXTRA exit then
   fp fu SC-SPAWN-ONLY? if fp fu SC-PTX-EXTRA then ;

: SC-CHECK-PTX ( -- )
   0 begin dup SC-CASE# @ < while dup SC-CHECK-PTX-MISSING-I 1+ repeat drop
   0 begin dup SC-PTX# @ < while dup SC-CHECK-PTX-EXTRA-I 1+ repeat drop ;

\ (c) keep the tables honest
: SC-MANUAL-STALE-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SC-SCHEDULED? if a u s" is documented manual-gate but is actually scheduled" SC-MANUAL-STALE exit then
   a u SC-CASE-MEMBER? 0= if a u s" is documented manual-gate but no TEST:SUITE lists it" SC-MANUAL-STALE then ;

: SC-SPAWN-STALE-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SC-SCHEDULED? if a u s" is documented spawn-only but is actually scheduled inprocess" SC-SPAWN-STALE exit then
   a u SC-PTX-SPAWNED? 0= if a u s" is documented spawn-only but not a spawned ptx-toolchain member" SC-SPAWN-STALE then ;

: SC-CHECK-TABLES ( -- )
   [: SC-MANUAL-STALE-CHECK ;] SC-MANUAL-TABLE
   [: SC-SPAWN-STALE-CHECK ;] SC-SPAWN-ONLY-TABLE ;

\ ============================================================================
\ (d) lint-suite registration: fail closed on an unregistered lint test file
\ ----------------------------------------------------------------------------
\ Every tools/lint/*-test.f that EXISTS on disk must be a TEST:SUITE member of
\ test/gate-stdlib-cases.f, otherwise it runs in no gate and regressions land
\ green (the shared-parser diff-test.f and interner set-test.f both drifted in
\ unregistered before this rule). A file documented manual-gate or spawn-only is
\ already a cases member - SC-MANUAL-STALE-CHECK / SC-SPAWN-STALE-CHECK reject a
\ documented entry that no TEST:SUITE lists - so the cases-membership test also
\ honours those documented exceptions and no separate exempt list is needed. The
\ live tools/lint directory is walked each run, so a NEW *-test.f file is a
\ LINT-UNREGISTERED finding until it is registered.
: SC-LINT-UNREGISTERED ( ptr u8 n -- ) {: fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: LINT-UNREGISTERED " type fp fu type
      s"  is a tools/lint test file that no TEST:SUITE in test/gate-stdlib-cases.f lists" type SC-NL
   then SC-FIND+ ;

: SC-LINT-REG-CHECK-FILE ( ptr u8 n -- ) {: fp:ptr fu:n :}
   fp fu SC-CASE-MEMBER? if exit then
   fp fu SC-LINT-UNREGISTERED ;

\ Walk callback: only *-test.f files are lint suites; the path is consumed in place.
: SC-LINT-REG-VISIT ( ptr u8 n -- )
   2dup s" -test.f" LINT-ENDS-WITH? if SC-LINT-REG-CHECK-FILE else 2drop then ;

: SC-CHECK-LINT-REG ( -- )
   s" tools/lint" [: SC-LINT-REG-VISIT ;] WALK-FILES ;

\ ============================================================================
\ maki slice partition (dot habu-split-monolithic-maki-fccca4ea)
\ ----------------------------------------------------------------------------
\ The monolithic maki/test.f was split into four parallel gate slices
\ (maki/test-<slice>.f, spawned by test/run-lib.f). maki/test.f stays as the full
\ run-all inventory / master; each of its `TEST:SUITE <file>` members must run in
\ EXACTLY ONE slice loader. An unaccounted member (0 slices) would silently stop
\ running under the gate; a double-accounted member (>=2 slices) runs twice and
\ breaks the one-file-per-image guarantee; a slice member with no master entry is an
\ extra. All three feed the same SC-FIND+ tally, so any break exits the lint red.
\ maki files use `SUITE <file>` under `using TEST` (or legacy `TEST:SUITE <file>`);
\ either way the file is the token AFTER the header, unlike the gate-stdlib cases
\ format (label on the header, member on its own BOL line).
$8000 constant SC-MAKI-CAP
create SC-MAKI-BUF SC-MAKI-CAP allot        \ maki/test.f master source (pointers held)
create SC-MK-A SC-CASE-MAX cells allot      \ master file ptr
create SC-MK-U SC-CASE-MAX cells allot      \ master file len
create SC-MK-CNT SC-CASE-MAX cells allot    \ per-file slice occurrence count
variable SC-MK#
variable SC-MK-MODE                          \ 0 collect master, nonzero bump slice
variable SC-MK-AFTER-SUITE                   \ previous token was TEST:SUITE

: SC-MK-A-FIELD ( n -- ptr ptr u8 ) cells SC-MK-A + 0 ptr-field ;
: SC-MK$ ( n -- ptr u8 n ) {: k:n :} k SC-MK-A-FIELD @  k cells SC-MK-U + @ ;

: SC-MK-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup SC-MK# @ < while
      dup SC-MK$ a u LINT-STR= if exit then
      1+
   repeat drop -1 ;

: SC-MK+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SC-MK# @ SC-CASE-MAX >= if E-TBL-BOUNDS throw then
   a  SC-MK# @ SC-MK-A-FIELD !
   u  SC-MK# @ cells SC-MK-U + !
   0  SC-MK# @ cells SC-MK-CNT + !
   SC-MK# @ 1+ SC-MK# ! ;

: SC-MK-UNACCOUNTED ( ptr u8 n -- ) {: fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: MAKI-UNACCOUNTED " type fp fu type
      s"  is a maki/test.f suite member that runs in no maki/test-<slice>.f loader" type SC-NL
   then SC-FIND+ ;

: SC-MK-DOUBLE ( ptr u8 n n -- ) {: fp:ptr fu:n c:n :}
   SC-REPORT? @ if
      s" suite-coverage: MAKI-DOUBLE " type fp fu type
      s"  is accounted to " type c SC-U. s"  maki slices (must be exactly one)" type SC-NL
   then SC-FIND+ ;

: SC-MK-EXTRA ( ptr u8 n -- ) {: fp:ptr fu:n :}
   SC-REPORT? @ if
      s" suite-coverage: MAKI-EXTRA " type fp fu type
      s"  is a maki slice member with no maki/test.f master entry" type SC-NL
   then SC-FIND+ ;

: SC-MK-BUMP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SC-MK-FIND dup 0< if drop a u SC-MK-EXTRA exit then
   cells SC-MK-CNT + 1 swap +! ;

: SC-MK-APPLY ( ptr u8 n -- )
   SC-MK-MODE @ 0= if SC-MK+ else SC-MK-BUMP then ;

\ pick the token right after each SUITE header (the maki suite/member file)
: SC-MK-TOK ( n -- ) {: k:n :}
   SC-MK-AFTER-SUITE @ if
      0 SC-MK-AFTER-SUITE !
      k TOK SC-STRIP-Q SC-MK-APPLY exit
   then
   k SC-USE-TOK
   k SC-MK-SUITE? if -1 SC-MK-AFTER-SUITE ! then ;

: SC-MK-SCAN$ ( ptr u8 n -- )
   LINT-TRUE PARENS? ! TOKENIZE
   0 SC-MK-AFTER-SUITE !
   SC-USE-RESET
   0 begin dup TN# @ < while
      dup SC-MK-TOK
      1+
   repeat drop ;

: SC-CHECK-MAKI-I ( n -- ) {: k:n :}
   k cells SC-MK-CNT + @ {: c:n :}
   c 1 = if exit then
   c 0= if k SC-MK$ SC-MK-UNACCOUNTED exit then
   k SC-MK$ c SC-MK-DOUBLE ;

: SC-CHECK-MAKI ( -- )
   0 begin dup SC-MK# @ < while dup SC-CHECK-MAKI-I 1+ repeat drop ;

\ ---- state + report --------------------------------------------------------
: SC-RESET ( -- )
   0 SC-CASE# !  0 SC-SUITE# !  0 SC-PTX# !  0 SC-PTX-USED !
   0 SC-FIND !  0 SC-INBLOCK !  0 SC-INDEF !
   0 SC-MK# !  0 SC-MK-MODE !  0 SC-MK-AFTER-SUITE !
   SC-USE-RESET
   INTERN-RESET ;

: SC-CHECK-ALL ( -- )
   SC-CHECK-ORPHANS
   SC-CHECK-PTX
   SC-CHECK-TABLES
   SC-CHECK-LINT-REG
   SC-CHECK-MAKI ;

: SC-SUMMARY ( -- )
   s" suite-coverage-lint: " type SC-SUITE# @ SC-U.
   s"  suite(s), " type SC-FIND @ SC-U.
   s"  finding(s)" type SC-NL ;

\ ---- live load: derive every list from the real gate files -----------------
: SC-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   READ-FILE ;

: SC-LOAD-CASES ( -- )
   s" test/gate-stdlib-cases.f" SC-CASES-BUF SC-CASES-CAP SC-READ SC-CASES-SCAN$ ;

: SC-LOAD-SCHED-FILE ( ptr u8 n -- )
   SC-SCAN-BUF SC-SCAN-CAP SC-READ SC-SCHED-SCAN$ ;

: SC-LOAD-SCHED ( -- )
   s" test/gate-stdlib-inline-lib.f" SC-LOAD-SCHED-FILE
   s" test/gate-stdlib-lint-tools.f" SC-LOAD-SCHED-FILE
   s" test/run-worker-stdlib.f" SC-LOAD-SCHED-FILE
   s" test/gate-engine-lib.f" SC-LOAD-SCHED-FILE
   s" test/candidate-validation.f" SC-SCAN-BUF SC-SCAN-CAP SC-READ SC-CAND-SCAN$
   SC-SCHED-CLOSURE-CORES ;

: SC-LOAD-PTX ( -- )
   s" test/gate-stdlib-inline-lib.f" SC-SCAN-BUF SC-SCAN-CAP SC-READ SC-PTX-INPROC-SCAN$ ;

: SC-LOAD-MAKI-MASTER ( -- )
   0 SC-MK-MODE !
   s" maki/test.f" SC-MAKI-BUF SC-MAKI-CAP SC-READ SC-MK-SCAN$ ;

: SC-BUMP-SLICE ( ptr u8 n -- )
   SC-SCAN-BUF SC-SCAN-CAP SC-READ SC-MK-SCAN$ ;

\ The one checked list of maki slice loaders: adding a slice means adding a row.
: SC-LOAD-MAKI-SLICES ( -- )
   1 SC-MK-MODE !
   s" maki/test-core.f"      SC-BUMP-SLICE
   s" maki/test-db.f"        SC-BUMP-SLICE
   s" maki/test-eval-emit.f" SC-BUMP-SLICE
   s" maki/test-eval.f"      SC-BUMP-SLICE ;

: SC-LOAD-MAKI ( -- )
   SC-LOAD-MAKI-MASTER
   SC-LOAD-MAKI-SLICES ;

: SC-LOAD ( -- )
   SC-RESET
   SC-LOAD-CASES
   SC-LOAD-SCHED
   SC-LOAD-PTX
   SC-LOAD-MAKI ;

public

: RUN ( -- )
   LINT-TRUE SC-REPORT? !
   SC-LOAD
   SC-CHECK-ALL
   SC-SUMMARY
   SC-FIND @ 0 > if 1 throw then ;

;package
