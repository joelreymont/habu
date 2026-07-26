\ lint-cli-standalone-load.f - prove every standalone lint entry point still loads.
\
\ Regression guard for habu-restore-dead-standalone-a362cb77. A lint entry point is
\ a COMMAND: `bin/hb --load tools/<name>-lint.f` is how a person or a script runs it.
\ Such an entry must `require` its own dependency closure. When it only lists the
\ closure in a header comment and relies on the caller having loaded it first, the
\ command is dead - and stays silently dead, because the resident test/run.f DAG
\ loads those same modules in order before the entry's unit tests run, so the suite
\ never exercises the command path. Five entries were dead that way when this guard
\ was written: signature-lint, reserved-name-lint, duplicate-definition-lint,
\ aot-lint and bootstrap-mirror-lint.
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
\ The entry list is DERIVED each run by walking tools/ and keeping the flat
\ tools/<name>-lint.f entries. Nested tools/lint/<name>-lint.f helpers are library
\ pieces their parents load, and -lint-core.f / -lint-test.f are not entry points;
\ all three drop out by structure alone. There is no exclusion table: a new lint
\ entry is covered the moment it is added, so no reserved name can suppress one, and
\ coverage fails closed on any load-order regression.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/argv.f
require lib/test.f

package LINT-CLI-STANDALONE-LOAD

2048 constant CAP
20000 constant TIMEOUT-MS
$4000 constant PATHS-CAP                     \ collected entry path bytes
64 constant PATHS-MAX                        \ collected entry slots
20 constant ENTRY-FLOOR                      \ min entries; guards a broken walk
47 constant SLASH
\ The engine's load/compile reject status (src/habu/habu2.f RC-REJECT). An entry
\ that dies of an unrequired dependency leaves exactly this code behind.
70 constant REJECT-RC
\ The engine's status for an uncaught throw. A file that is deliberately NOT a
\ standalone entry refuses with a throw, so it exits here instead.
67 constant THROW-RC

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot                         \ zero-length stdin
create PATHS-BUF PATHS-CAP allot
create PATH-OFF PATHS-MAX cells allot
create PATH-LEN PATHS-MAX cells allot
variable PATHS-N
variable PATHS-USED
variable RC
variable EXITED

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
: STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF RC ! TRUE EXITED ! ENDOF
     signaled OF RC ! FALSE EXITED ! ENDOF
     timeout  OF 0 RC ! FALSE EXITED ! ENDOF
   ;MATCH
   LEN>N drop LEN>N drop ;

: LOADS ( ptr u8 n -- ) {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   ENGINE$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE!
   p u T-LABEL  EXITED @ TTRUE
   p u T-LABEL  RC @ REJECT-RC <> TTRUE ;

: LOAD-ALL ( -- )
   0 begin dup PATHS-N @ < while
      dup PATH$ LOADS
      1+
   repeat drop ;

\ ---- a harness body must refuse, not die opaquely --------------------------
\ test/gate-stdlib-lint-tools.f is deliberately NOT a standalone entry: its header
\ says "Load after GSI-LINT-TOOLS-SETUP", and the harness supplies the lint cores and
\ GSI-* words its bodies compile against, so there is nothing it could require to
\ stand alone. Direct invocation must therefore REFUSE by name rather than die on
\ whichever harness word is reached first, because an opaque E-UNDEFINED there reads
\ like a missing require and sent one earlier investigation down that path. The two
\ statuses must stay distinct for this row to mean anything.
: REFUSES ( ptr u8 n -- ) {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   ENGINE$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE!
   p u T-LABEL  EXITED @ TTRUE
   p u T-LABEL  RC @ THROW-RC T= ;

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
   s" signature-lint entry scheduled" T-LABEL
      s" tools/signature-lint.f" SCHEDULES? TTRUE
   s" reserved-name-lint entry scheduled" T-LABEL
      s" tools/reserved-name-lint.f" SCHEDULES? TTRUE
   s" duplicate-definition-lint entry scheduled" T-LABEL
      s" tools/duplicate-definition-lint.f" SCHEDULES? TTRUE
   s" aot-lint entry scheduled" T-LABEL
      s" tools/aot-lint.f" SCHEDULES? TTRUE
   s" bootstrap-mirror-lint entry scheduled" T-LABEL
      s" tools/bootstrap-mirror-lint.f" SCHEDULES? TTRUE
   s" newly added lint entry scheduled" T-LABEL
      s" tools/newly-added-lint.f" SCHEDULES? TTRUE
   s" lint core is not an entry" T-LABEL
      s" tools/signature-lint-core.f" SCHEDULES? TFALSE
   s" lint suite is not an entry" T-LABEL
      s" tools/signature-lint-test.f" SCHEDULES? TFALSE
   s" nested lint helper is not an entry" T-LABEL
      s" tools/lint/shadow-lint.f" SCHEDULES? TFALSE
   s" non-lint tool is not an entry" T-LABEL
      s" tools/enum-census.f" SCHEDULES? TFALSE
   \ A usage failure is not a dead load: the two statuses must stay distinct, or the
   \ verdict above would accept an entry that never finished loading.
   s" usage status differs from a dead load" T-LABEL
      ARGV:E-USAGE REJECT-RC <> TTRUE
   s" refusal status differs from a dead load" T-LABEL
      THROW-RC REJECT-RC <> TTRUE ;

public

: RUN ( -- )
   T-RESET
   FIXTURE
   0 PATHS-N !  0 PATHS-USED !
   s" tools" [: COLLECT ;] WALK-FILES
   s" lint entries discovered" T-LABEL  PATHS-N @ ENTRY-FLOOR >= TTRUE
   s" tools/signature-lint.f discovered" T-LABEL
      s" tools/signature-lint.f" COLLECTED? TTRUE
   LOAD-ALL
   s" test/gate-stdlib-lint-tools.f" REFUSES
   T-REPORT
   s" lint-cli-standalone-load-test: ok" type cr ;

;package

LINT-CLI-STANDALONE-LOAD:RUN
