\ process-env-test.f - focused tests for lib/process-env.f.
\ Run: bin/hb --load lib/process-env-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$8000 constant PET-CAP
$20000 constant PET-EARLY-IN-CAP
$1388 constant PET-HB-TIMEOUT-MS
$3E8 constant PET-CMD-TIMEOUT-MS
$32 constant PET-SHORT-TIMEOUT-MS
$2 constant PET-ENOENT

create PET-OUT PET-CAP allot
create PET-ERR PET-CAP allot
create PET-PATH FS-PATH-CAP allot
create PET-EARLY-IN PET-EARLY-IN-CAP allot
variable PET-I
variable PET-START-NS

: PET-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-DEFAULT-RESET ;

: PET-EARLY-IN! ( -- )
   0 PET-I !
   begin PET-I @ PET-EARLY-IN-CAP < while
      $61 PET-EARLY-IN PET-I @ + c!
      PET-I @ 1+ PET-I !
   repeat ;

: PET-U-TYPE ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: PET-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PET-OUTCOME>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: PET-FIND>N ( len bool -- n bool ) {: gotu found :}
   gotu LEN>N found ;

: PET-ENV+ ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu val:ptr valu :}
   name nameu >LEN val valu >LEN PROC-ENV+ ;

: PET-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-ENV-CAPTURE
   PET-CAPTURE>N ;

: PET-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   PET-OUTCOME>N ;

: PET-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PET-CAPTURE>N ;

: PET-STDIN-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   PET-OUTCOME>N ;

: PET-FIND-IN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n bool )
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu >LEN path pathu >LEN dst FIND-EXECUTABLE-IN-PATH
   PET-FIND>N ;

: PET-RESOLVE ( ptr u8 n ptr u8 -- n )
   {: cmd:ptr cmdu dst:ptr :}
   cmd cmdu >LEN dst RESOLVE-EXECUTABLE LEN>N ;

: PET-ENV-CMD$ ( -- ptr u8 n )
   s" /usr/bin/env" ;

: PET-ALPHA-LINE$ ( -- ptr u8 n )
   SB-RESET
   s" HABU_PROC_ENV_TEST=alpha" SB-APPEND
   $0A SB-APPEND-C
   SB$ ;

\ typed-local-lint: allow-bare-local - q is the test action quotation.
: PET-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns PET-START-NS !
   q execute
   s" PASS: " type label labelu type
   s"  (" type mono-ns PET-START-NS @ - PROC-NS-PER-MS / PET-U-TYPE s" ms)" type cr ;

: PET-RUN-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu PET-ALPHA-LINE$ T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PET-RUN-EMPTY-ENV-CHILD ( -- )
   PET-RESET
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= 0 T= ;

: PET-EXPECT-INHERITED ( n ptr u8 n -- ) {: outu:n name:ptr nameu:n :}
   name nameu GETENV {: val:ptr valu:n :}
   valu 0= if exit then
   SB-RESET
   name nameu SB-APPEND
   $3D SB-APPEND-C
   val valu SB-APPEND
   $0A SB-APPEND-C
   PET-OUT outu SB$ CONTAINS? TTRUE ;

: PET-INHERIT-ENV-OUT ( n -- ) {: outu:n :}
   PET-OUT outu PET-ALPHA-LINE$ CONTAINS? TTRUE
   outu s" HOME" PET-EXPECT-INHERITED
   outu s" PATH" PET-EXPECT-INHERITED ;

: PET-RUN-INHERIT-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-DEFAULT-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-DEFAULT+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-DEFAULT-LOOKUP ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-DEFAULT+
   s" HABU_PROC_ENV_TEST" >LEN PROC-ENV-DEFAULT$? TTRUE
   LEN>N s" alpha" T$=
   s" HABU_PROC_ENV_MISSING" >LEN PROC-ENV-DEFAULT$? TFALSE
   2drop ;

: PET-EXPLICIT-BEATS-DEFAULT ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" wrong" >LEN PROC-ENV-DEFAULT+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu s" HABU_PROC_ENV_TEST=wrong" CONTAINS? TFALSE
   outu PET-INHERIT-ENV-OUT ;

\ PROC-ENV-SET vs PROC-ENV+: SET replaces an existing row in place (one row
\ survives, the child sees only the last value), while PROC-ENV+ appends and
\ never deduplicates. SET on an absent name appends like PROC-ENV+.
: PET-SET-REPLACES ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" wrong" PET-ENV+
   PROC-ENV-N @ COUNT>N 1 T=
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-SET
   PROC-ENV-N @ COUNT>N 1 T=
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu s" HABU_PROC_ENV_TEST=wrong" CONTAINS? TFALSE
   outu PET-INHERIT-ENV-OUT ;

: PET-SET-APPENDS-NEW ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-SET
   PROC-ENV-N @ COUNT>N 1 T=
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-APPEND-KEEPS-DUP ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" wrong" PET-ENV+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-N @ COUNT>N 2 T= ;

: PET-RUN-ENV-OUTCOME-FALSE ( -- )
   PET-RESET
   s" /usr/bin/false" PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-OUTCOME
   1 T= PROC-OUTCOME-EXIT T= 0 T= 0 T= ;

: PET-RUN-ENV-OUTCOME-TIMEOUT ( -- )
   PET-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" PET-OUT PET-CAP PET-ERR PET-CAP PET-SHORT-TIMEOUT-MS PET-OUTCOME
   SIGKILL T= PROC-OUTCOME-TIMEOUT T= 0 T= 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME ( -- )
   PET-RESET
   s" /bin/cat" s" env-stdin" PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-OUTCOME
   0 T= PROC-OUTCOME-EXIT T= 0 T= 9 T=
   PET-OUT 9 s" env-stdin" T$= ;

: PET-RUN-ENV-STDIN-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

\ Budget env carrier (habu-concurrent-multi-workspace-5341c7f4): a spawned hb
\ child inherits HB_LOAD_PCT through the env rows and scales its budgets by it
\ end-to-end - proving the gate's exported cal-factor actually reaches
\ worker-spawned suites. Inherit-missing keeps HOME/PATH so bin/hb boots.
: TEST-BUDGET-ENV ( -- )
   PET-RESET
   s" HB_LOAD_PCT" s" 250" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" s" require lib/test/budget.f 100 T-BUDGET-MS . "
   PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-STDIN-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu s" 250" CONTAINS? TTRUE ;

: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-OUTCOME
   1 T= PROC-OUTCOME-EXIT T= 0 T= 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME-TIMEOUT ( -- )
   PET-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" s" " PET-OUT PET-CAP PET-ERR PET-CAP PET-SHORT-TIMEOUT-MS PET-STDIN-OUTCOME
   SIGKILL T= PROC-OUTCOME-TIMEOUT T= 0 T= 0 T= ;

: PET-SPAWN-RAW-MISSING ( -- )
   PET-RESET
   s" /no/such/habu-process-env-test" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-RAW PID>N {: code:n :}
   PROC-ARGV-ENV-RESET
   code 0 < TTRUE
   HB-TARGET-MACOS? if code PET-ENOENT negate T= then ;

: PET-SPAWN-RAW-TRUE-ONE ( -- )
   PET-RESET
   s" /usr/bin/true" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 > TTRUE
   pid PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: PET-SPAWN-RAW-TRUE ( -- )
   0 begin dup 16 < while
      PET-SPAWN-RAW-TRUE-ONE
      1+
   repeat drop ;

: PET-BAD-ENV-NAME ( -- )
   PET-RESET
   s" BAD=NAME" s" x" PET-ENV+ ;

: PET-BAD-ENV-ENTRY ( -- )
   PET-RESET
   s" MISSING_EQUALS"  >LEN PROC-ENV-ENTRY+ ;

: PET-BAD-ENV-EMPTY ( -- )
   PET-RESET
   s" " s" x" PET-ENV+ ;

: PET-PATH-FIND-HB ( -- )
   s" hb" s" bin" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-DIRECT-HB ( -- )
   s" bin/hb" s" nowhere" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-MISSING ( -- )
   s" no-habu-process-env-test" s" bin" PET-PATH PET-FIND-IN-PATH TFALSE
   drop ;

: PET-RESOLVE-MISSING ( -- )
   s" no-habu-process-env-test" PET-PATH PET-RESOLVE drop ;

: PET-BAD-ENV-NAME-THROWS ( -- )
   [: PET-BAD-ENV-NAME ;] E-PROC-ENV TTHROWSQ ;

: PET-BAD-ENV-ENTRY-THROWS ( -- )
   [: PET-BAD-ENV-ENTRY ;] E-PROC-ENV TTHROWSQ ;

: PET-BAD-ENV-EMPTY-THROWS ( -- )
   [: PET-BAD-ENV-EMPTY ;] E-PROC-ENV TTHROWSQ ;

: PET-RESOLVE-MISSING-THROWS ( -- )
   [: PET-RESOLVE-MISSING ;] E-PROC-PATH TTHROWSQ ;

: PROCESS-ENV-TEST-MAIN ( -- )
   T-RESET
   s" env-child" [: PET-RUN-ENV-CHILD ;] PET-CASE
   s" empty-env-child" [: PET-RUN-EMPTY-ENV-CHILD ;] PET-CASE
   s" inherit-env-child" [: PET-RUN-INHERIT-ENV-CHILD ;] PET-CASE
   s" default-env-child" [: PET-DEFAULT-ENV-CHILD ;] PET-CASE
   s" default-lookup" [: PET-DEFAULT-LOOKUP ;] PET-CASE
   s" explicit-beats-default" [: PET-EXPLICIT-BEATS-DEFAULT ;] PET-CASE
   s" set-replaces" [: PET-SET-REPLACES ;] PET-CASE
   s" set-appends-new" [: PET-SET-APPENDS-NEW ;] PET-CASE
   s" append-keeps-dup" [: PET-APPEND-KEEPS-DUP ;] PET-CASE
   s" env-outcome-false" [: PET-RUN-ENV-OUTCOME-FALSE ;] PET-CASE
   s" env-outcome-timeout" [: PET-RUN-ENV-OUTCOME-TIMEOUT ;] PET-CASE
   s" env-stdin-outcome" [: PET-RUN-ENV-STDIN-OUTCOME ;] PET-CASE
   s" env-stdin-false-large" [: PET-RUN-ENV-STDIN-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-false-large" [: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-timeout" [: PET-RUN-ENV-STDIN-OUTCOME-TIMEOUT ;] PET-CASE
   s" budget-env" [: TEST-BUDGET-ENV ;] PET-CASE
   s" spawn-raw-missing" [: PET-SPAWN-RAW-MISSING ;] PET-CASE
   s" spawn-raw-true" [: PET-SPAWN-RAW-TRUE ;] PET-CASE
   s" bad-env-name" [: PET-BAD-ENV-NAME-THROWS ;] PET-CASE
   s" bad-env-entry" [: PET-BAD-ENV-ENTRY-THROWS ;] PET-CASE
   s" bad-env-empty" [: PET-BAD-ENV-EMPTY-THROWS ;] PET-CASE
   s" path-find-hb" [: PET-PATH-FIND-HB ;] PET-CASE
   s" path-direct-hb" [: PET-PATH-DIRECT-HB ;] PET-CASE
   s" path-missing" [: PET-PATH-MISSING ;] PET-CASE
   s" resolve-missing" [: PET-RESOLVE-MISSING-THROWS ;] PET-CASE
   T-REPORT
   s" process-env-test: ok" type cr ;

PROCESS-ENV-TEST-MAIN
