\ stdlib-standalone-load.f - prove every flat production lib module bare-loads.
\
\ Regression guard for habu-pkg-stdlib-modules-3197dbbc: a flat lib/<module>.f
\ must `require` its own dependency closure, so `bin/hb --load lib/<module>.f`
\ works in a fresh engine with no caller-supplied prelude. Hidden load-order
\ coupling (a module using another module's words with no require line) is masked
\ by the resident gate DAG order and surfaces only here. Each module is spawned
\ in a fresh child engine with empty stdin; a clean exit 0 proves the module
\ pulled its whole dependency closure itself.
\
\ The module list is DERIVED each run by walking lib/ and keeping the flat
\ lib/<name>.f modules. Nested lib/<sub>/ research sub-libraries and *-test.f
\ files are out of scope by structure alone (their suites cover them). There is
\ no exclusion table: every flat module is covered automatically, so no path
\ prefix or reserved future name can suppress a production module, and coverage
\ fails closed on any load-order regression.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/test.f

package STDLIB-STANDALONE-LOAD

2048 constant CAP
20000 constant TIMEOUT-MS
$4000 constant PATHS-CAP                     \ collected flat-module path bytes
128 constant PATHS-MAX                       \ collected flat-module slots
40 constant MODULE-FLOOR                     \ min flat modules; guards a broken walk
47 constant SLASH

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

\ A flat production module is lib/<name>.f: a `.f` file, not a `-test.f`, with a
\ single '/' (nested lib/<sub>/<name>.f has two and is out of scope).
: FLAT-MODULE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" SUFFIX? 0= if FALSE exit then
   a u s" -test.f" SUFFIX? if FALSE exit then
   a u SLASHES 1 = ;

\ ---- collect flat modules during the walk ----------------------------------
: PATH$ ( n -- ptr u8 n ) {: k:n :}
   PATHS-BUF PATH-OFF k cells + @ +  PATH-LEN k cells + @ ;

: COLLECT ( ptr u8 n -- ) {: a:ptr u:n :}     \ WALK-FILES callback
   a u FLAT-MODULE? 0= if exit then
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

\ ---- spawn one bare load ---------------------------------------------------
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
   p u T-LABEL  RC @ 0 T= ;

: LOAD-ALL ( -- )
   0 begin dup PATHS-N @ < while
      dup PATH$ LOADS
      1+
   repeat drop ;

\ ---- fixture: scheduling is structural, no exclusion table -----------------
\ Inject synthetic paths straight into COLLECT and assert each verdict without
\ spawning a load. This proves discovery keeps exactly the flat lib/<name>.f
\ modules: the two spellings the deleted speculative table reserved
\ (lib/engine-candidate.f and the lib/process-pty family) are now scheduled like
\ any other flat module, a brand-new flat name is scheduled the moment it is
\ discovered, and nested lib/<sub>/<name>.f and *-test.f drop out by FLAT-MODULE?
\ structure alone. No path prefix or reserved future name can suppress a
\ production module.
: SCHEDULES? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ inject one path; collected?
   0 PATHS-N !  0 PATHS-USED !
   a u COLLECT
   a u COLLECTED? ;

: FIXTURE ( -- )
   s" lib/engine-candidate.f scheduled" T-LABEL
      s" lib/engine-candidate.f" SCHEDULES? TTRUE
   s" lib/process-pty-handle.f scheduled" T-LABEL
      s" lib/process-pty-handle.f" SCHEDULES? TTRUE
   s" lib/process-pty-io.f scheduled" T-LABEL
      s" lib/process-pty-io.f" SCHEDULES? TTRUE
   s" newly discovered flat module scheduled" T-LABEL
      s" lib/newly-discovered.f" SCHEDULES? TTRUE
   s" nested sub-library excluded by scope" T-LABEL
      s" lib/sub/nested.f" SCHEDULES? TFALSE
   s" -test.f excluded by scope" T-LABEL
      s" lib/newly-discovered-test.f" SCHEDULES? TFALSE ;

public

: RUN ( -- )
   T-RESET
   FIXTURE
   0 PATHS-N !  0 PATHS-USED !
   s" lib" [: COLLECT ;] WALK-FILES
   s" flat lib modules discovered" T-LABEL  PATHS-N @ MODULE-FLOOR >= TTRUE
   s" lib/json-read.f discovered" T-LABEL   s" lib/json-read.f" COLLECTED? TTRUE
   LOAD-ALL
   T-REPORT
   s" stdlib-standalone-load-test: ok" type cr ;

;package

STDLIB-STANDALONE-LOAD:RUN
