\ Every flat src/lib/<name>.f module bare-loads (`bin/hb --load` in a fresh
\ child, empty stdin, exit 0): it requires its own dependency closure. A missing
\ require is masked by the gate's resident load order and surfaces only here.
\ Each module loads as the walk of src/lib finds it, with no list or exclusion
\ table; nested src/lib/<sub>/ and *-test.f files are out of scope, covered by
\ their own suites.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package STDLIB-STANDALONE-LOAD

2048 constant CAP
40 constant MODULE-FLOOR                     \ min flat modules; guards a broken walk
47 constant SLASH

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot
variable MODULES-N
TYPED-VARIABLE JSON-SEEN bool

: TRUE ( -- bool ) 0 0= ;
: FALSE ( -- bool ) 0 0= 0= ;

: SLASHES ( ^u8 n -- n ) {: a:^ u:n :}
   0 0 begin dup u < while
      dup a + c@ SLASH = if swap 1+ swap then
      1+
   repeat drop ;

: SUFFIX? ( ^u8 n ^u8 n -- bool ) {: a:^ u:n sa:^ su:n :}
   u su < if FALSE exit then
   a u su - +  su  sa su  STR= ;

\ A flat module is src/lib/<name>.f and not a -test.f: two '/' bytes; a nested
\ src/lib/<sub>/<name>.f has three.
: FLAT-MODULE? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u s" .f" SUFFIX? 0= if FALSE exit then
   a u s" -test.f" SUFFIX? if FALSE exit then
   a u SLASHES 2 = ;

: LOADS ( ^u8 n -- )
   {: p:^ u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   p u T-LABEL
   p u OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED= ;

: COLLECT ( ^u8 n -- ) {: a:^ u:n :}     \ FS:WALK-FILES callback
   a u FLAT-MODULE? 0= if exit then
   MODULES-N @ 1+ MODULES-N !
   a u s" src/lib/json.f" STR= if TRUE JSON-SEEN ! then
   a u LOADS ;

public

: RUN ( -- )
   T-RESET
   0 MODULES-N !  FALSE JSON-SEEN !
   s" src/lib" [: COLLECT ;] FS:WALK-FILES
   s" flat lib modules discovered" T-LABEL  MODULES-N @ MODULE-FLOOR >= TTRUE
   s" src/lib/json.f discovered" T-LABEL   JSON-SEEN @ TTRUE
   T-REPORT
   s" stdlib-standalone-load-test: ok" type cr ;

;package

STDLIB-STANDALONE-LOAD:RUN
