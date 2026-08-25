\ lint-cli-standalone-load.f - prove every standalone lint entry point still loads.
\
\ Regression guard for habu-restore-dead-standalone-a362cb77. A lint entry point is
\ a COMMAND: `bin/hb --load tools/<name>-lint.f` is how a person or a script runs it.
\ Such an entry must `require` its own dependency closure. When it only lists the
\ closure in a header comment and relies on the caller having loaded it first, the
\ command is dead. Reserved-name-lint and aot-lint were dead that way when this
\ guard was written.
\
\ Each entry is spawned in a fresh child engine with empty stdin. The verdict is
\ deliberately NOT "exit 0": these entries RUN when loaded, so a healthy one may
\ exit 0 (it did its work), or exit with its usage code (it loaded, then refused the
\ argument list), or exit with a findings code. What a healthy entry never does is
\ die while loading. A dead entry exits REJECT-RC with an E-UNDEFINED diagnostic, so
\ the leg asserts the child exited on its own and did NOT exit REJECT-RC. That is
\ exactly the distinction the lane required: an entry that throws while parsing
\ arguments still passes, an entry that never finished loading does not.
\
\ The three ways a child can finish are kept apart. A child that merely ran out of
\ wall clock is not a child that died, and folding the two into one boolean is how
\ a saturated box came to be reported as a dead lint entry: the failure read
\ "expected true got false" and named neither the deadline nor the fact that one
\ was hit (dot habu-un-flake-lint-2535cef6). Exit, signal and timeout each get
\ their own verdict below, and a timeout is recorded as a timeout naming the entry
\ and the budget it burned.
\
\ The entry list is DERIVED each run by walking tools/ and keeping the flat
\ tools/<name>-lint.f entries. Nested tools/lint/<name>-lint.f helpers are library
\ pieces their parents load, and -lint-core.f / -lint-test.f are not entry points;
\ all three drop out by structure alone. There is no exclusion table: a new lint
\ entry is covered the moment it is added, so no reserved name can suppress one, and
\ coverage fails closed on any load-order regression.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/argv.f
require lib/test.f

package LINT-CLI-STANDALONE-LOAD

2048 constant CAP
$4000 constant PATHS-CAP                     \ collected entry path bytes
64 constant PATHS-MAX                        \ collected entry slots
47 constant SLASH
\ The engine's load/compile reject status (src/habu/habu2.f RC-REJECT). An entry
\ that dies of an unrequired dependency leaves exactly this code behind.
70 constant REJECT-RC
create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot                         \ zero-length stdin
create PATHS-BUF PATHS-CAP allot
create PATH-OFF PATHS-MAX cells allot
create PATH-LEN PATHS-MAX cells allot
variable PATHS-N
variable PATHS-USED

36000 constant TIMEOUT-MS

: ENGINE$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0 > if exit then
   2drop s" bin/hb" ;

: TRUE ( -- bool ) 0 0= ;
: FALSE ( -- bool ) 0 0= 0= ;

\ ---- path predicates -------------------------------------------------------
: SLASHES ( ptr u8 n -- n ) {: a:ptr u:n :}   \ count '/' bytes in a path
   0 0 begin dup u < while
      dup a + c@ SLASH = if swap 1+ swap then
      1+
   repeat drop ;

: SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n sa:ptr su:n :}
   u su < if FALSE exit then
   a u su - +  su  sa su  STR= ;

\ A standalone lint entry is tools/<name>-lint.f: the -lint.f suffix carries it, and
\ a single '/' keeps the nested tools/lint/ helpers out. -lint-core.f and
\ -lint-test.f fail the suffix test, so no separate exclusion is needed.
: LINT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -lint.f" SUFFIX? 0= if FALSE exit then
   a u SLASHES 1 = ;

\ ---- collect entries during the walk ---------------------------------------
: PATH$ ( n -- ptr u8 n ) {: k:n :}
   PATHS-BUF PATH-OFF k cells + @ +  PATH-LEN k cells + @ ;

: COLLECT ( ptr u8 n -- ) {: a:ptr u:n :}     \ WALK-FILES callback
   a u LINT-ENTRY? 0= if exit then
   PATHS-N @ PATHS-MAX >= if E-TBL-BOUNDS throw then
   PATHS-USED @ u + PATHS-CAP > if E-TBL-BOUNDS throw then
   a  PATHS-BUF PATHS-USED @ +  u BYTE-COPY
   PATHS-USED @ PATH-OFF PATHS-N @ cells + !
   u PATH-LEN PATHS-N @ cells + !
   PATHS-USED @ u + PATHS-USED !
   PATHS-N @ 1+ PATHS-N ! ;

: COLLECTED? ( ptr u8 n -- bool ) {: qa:ptr qu:n :}
   0 begin dup PATHS-N @ < while
      dup PATH$ qa qu STR= if drop TRUE exit then
      1+
   repeat drop FALSE ;

\ ---- spawn one entry -------------------------------------------------------
: SPAWN ( ptr u8 n n -- len len outcome ) {: p:ptr u:n ms:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   ENGINE$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  ms >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: DROP-LENS ( len len -- )
   LEN>N drop LEN>N drop ;

\ Its own failure kind, not an assertion that happened to go false: the entry and
\ the budget it burned are the two facts needed to tell a slow box from a hang, so
\ both go into the label and therefore into the machine-readable TFAIL record a
\ gate log is read with, not only into an adjacent printed line.
: OVER-BUDGET$ ( ptr u8 n n -- ptr u8 n ) {: p:ptr u:n ms:n :}
   SB-RESET
   p u SB-APPEND
   s"  over its per-child budget of " SB-APPEND
   ms FMT:SB-U
   s" ms" SB-APPEND
   SB$ ;

: ON-SIGNAL$ ( ptr u8 n n -- ptr u8 n ) {: p:ptr u:n sig:n :}
   SB-RESET
   p u SB-APPEND
   s"  died on signal " SB-APPEND
   sig FMT:SB-U
   SB$ ;

: TIMED-OUT ( ptr u8 n n -- )
   T-NEXT
   OVER-BUDGET$ T-LABEL
   s" timeout" T-FAIL-AS
   T-LABEL-CLEAR ;

: SIGNALLED ( ptr u8 n n -- )
   T-NEXT
   ON-SIGNAL$ T-LABEL
   s" signal" T-FAIL-AS
   T-LABEL-CLEAR ;

\ The budget is a parameter so the suite can drive this exact word - the one the
\ real legs run - with a budget no child can meet and check the verdict it makes.
: LOAD-VERDICT ( ptr u8 n n -- ) {: p:ptr u:n ms:n :}
   p u ms SPAWN
   MATCH outcome
     exited   OF {: rc:n :} p u T-LABEL rc REJECT-RC <> TTRUE ENDOF
     signaled OF {: sig:n :} p u sig SIGNALLED ENDOF
     timeout  OF p u ms TIMED-OUT ENDOF
   ;MATCH
   DROP-LENS ;

: LOADS ( ptr u8 n -- )
   TIMEOUT-MS LOAD-VERDICT ;

: LOAD-ALL ( -- )
   0 begin dup PATHS-N @ < while
      dup PATH$ LOADS
      1+
   repeat drop ;

\ ---- fixture: a timeout is its own verdict ---------------------------------
\ The distinction this suite exists to make is between an entry that never
\ finished loading and one that merely ran out of wall clock, so the timeout path
\ is exercised rather than assumed. A discovered entry is spawned through the same
\ LOAD-VERDICT the real legs use, with a budget nothing can meet, and the record
\ it produces is checked: kind `timeout`, and a label naming that entry and the
\ budget it burned. It records one deliberate failure and clears it, which is the
\ lib/test/assert-test.f convention for testing a failure path; it runs before
\ anything else so the reset cannot erase a real one.
1 constant UNMEETABLE-MS
256 constant GOT-CAP
create GOT-BUF GOT-CAP allot
variable GOT-U

: GOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u GOT-CAP > if E-TBL-BOUNDS throw then
   a GOT-BUF u BYTE-COPY
   u GOT-U ! ;

: GOT$ ( -- ptr u8 n )
   GOT-BUF GOT-U @ ;

\ Spelled out here rather than taken from OVER-BUDGET$: a label that stopped
\ naming the entry, or stopped naming the budget, has to fail this.
: WANT-LABEL$ ( ptr u8 n n -- ptr u8 n ) {: p:ptr u:n ms:n :}
   SB-RESET
   p u SB-APPEND
   s"  over its per-child budget of " SB-APPEND
   ms FMT:SB-U
   s" ms" SB-APPEND
   SB$ ;

: TIMEOUT-VERDICT ( -- )
   0 PATHS-N !  0 PATHS-USED !
   s" tools" [: COLLECT ;] WALK-FILES
   PATHS-N @ 0 <= if E-TBL-BOUNDS throw then
   T-RESET
   0 PATH$ UNMEETABLE-MS LOAD-VERDICT
   TREC$ GOT!
   T-CASES {: id:n :}
   T-FAILURES {: fails:n :}
   T-RESET                                   \ drop the deliberate failure, THEN judge it,
   s" timeout verdict record" T-LABEL        \ so a verdict that came out wrong survives
   GOT$  s" timeout" id  0 PATH$ UNMEETABLE-MS WANT-LABEL$  TREC-FAIL$  T$=
   s" timeout counts as exactly one failure" T-LABEL
   fails 1 T= ;

\ ---- fixture: scheduling is structural, no exclusion table -----------------
\ Inject synthetic paths straight into COLLECT and assert each verdict without
\ spawning anything. This proves discovery keeps exactly the flat tools/<name>-lint.f
\ entries: every entry this lane repaired is scheduled, a brand-new lint entry is
\ scheduled the moment it appears, and the shapes that are NOT entry points - a lint
\ core, a lint suite, a nested tools/lint/ helper, and a non-lint tool - drop out by
\ structure alone. No reserved name can suppress an entry.
: SCHEDULES? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ inject one path; collected?
   0 PATHS-N !  0 PATHS-USED !
   a u COLLECT
   a u COLLECTED? ;

: FIXTURE ( -- )
   s" reserved-name-lint entry scheduled" T-LABEL
      s" tools/reserved-name-lint.f" SCHEDULES? TTRUE
   s" aot-lint entry scheduled" T-LABEL
      s" tools/aot-lint.f" SCHEDULES? TTRUE
   s" newly added lint entry scheduled" T-LABEL
      s" tools/newly-added-lint.f" SCHEDULES? TTRUE
   s" lint core is not an entry" T-LABEL
      s" tools/reserved-name-lint-core.f" SCHEDULES? TFALSE
   s" lint suite is not an entry" T-LABEL
      s" tools/reserved-name-lint-test.f" SCHEDULES? TFALSE
   s" nested lint helper is not an entry" T-LABEL
      s" tools/lint/shadow-lint.f" SCHEDULES? TFALSE
   \ A usage failure is not a dead load: the two statuses must stay distinct, or the
   \ verdict above would accept an entry that never finished loading.
   s" usage status differs from a dead load" T-LABEL
      ARGV:E-USAGE REJECT-RC <> TTRUE ;

public

: RUN ( -- )
   TIMEOUT-VERDICT                           \ owns the suite's T-RESET; see its comment
   FIXTURE
   0 PATHS-N !  0 PATHS-USED !
   s" tools" [: COLLECT ;] WALK-FILES
   s" tools/reserved-name-lint.f discovered" T-LABEL
      s" tools/reserved-name-lint.f" COLLECTED? TTRUE
   LOAD-ALL
   T-REPORT
   s" lint-cli-standalone-load-test: ok" type cr ;

;package

LINT-CLI-STANDALONE-LOAD:RUN
