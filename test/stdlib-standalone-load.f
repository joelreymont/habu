\ Every flat src/lib/<name>.f module bare-loads (`bin/hb --load` in a fresh
\ child, empty stdin, exit 0): it requires its own dependency closure. A missing
\ require is masked by the gate's resident load order and surfaces only here.
\ The list is walked from src/lib each run with no exclusion table; nested
\ src/lib/<sub>/ and *-test.f files are out of scope, covered by their own
\ suites.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/engine-candidate.f

package STDLIB-STANDALONE-LOAD

2048 constant CAP
20000 constant TIMEOUT-MS
$4000 constant PATHS-CAP
128 constant PATHS-MAX
40 constant MODULE-FLOOR                     \ min flat modules; guards a broken walk
47 constant SLASH

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot
create PATHS-BUF PATHS-CAP allot
create PATH-OFF PATHS-MAX cells allot
create PATH-LEN PATHS-MAX cells allot
variable PATHS-N
variable PATHS-USED

: TRUE ( -- bool ) 0 0= ;
: FALSE ( -- bool ) 0 0= 0= ;

: SLASHES ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 0 begin dup u < while
      dup a + c@ SLASH = if swap 1+ swap then
      1+
   repeat drop ;

: SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n sa:ptr su:n :}
   u su < if FALSE exit then
   a u su - +  su  sa su  STR= ;

\ A flat module is src/lib/<name>.f and not a -test.f: two '/' bytes; a nested
\ src/lib/<sub>/<name>.f has three.
: FLAT-MODULE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" SUFFIX? 0= if FALSE exit then
   a u s" -test.f" SUFFIX? if FALSE exit then
   a u SLASHES 2 = ;

: PATH$ ( n -- ptr u8 n ) {: k:n :}
   PATHS-BUF PATH-OFF k cells + @ +  PATH-LEN k cells + @ ;

: COLLECT ( ptr u8 n -- ) {: a:ptr u:n :}     \ FS:WALK-FILES callback
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

: LOADS ( ptr u8 n -- )
   {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   p u T-LABEL
   p u OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED= ;

: LOAD-ALL ( -- )
   0 begin dup PATHS-N @ < while
      dup PATH$ LOADS
      1+
   repeat drop ;

public

: RUN ( -- )
   T-RESET
   0 PATHS-N !  0 PATHS-USED !
   s" src/lib" [: COLLECT ;] FS:WALK-FILES
   s" flat lib modules discovered" T-LABEL  PATHS-N @ MODULE-FLOOR >= TTRUE
   s" src/lib/json-read.f discovered" T-LABEL   s" src/lib/json-read.f" COLLECTED? TTRUE
   LOAD-ALL
   T-REPORT
   s" stdlib-standalone-load-test: ok" type cr ;

;package

STDLIB-STANDALONE-LOAD:RUN
