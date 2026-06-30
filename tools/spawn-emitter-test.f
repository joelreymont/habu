\ spawn-emitter-test.f - source-shape regression for Darwin spawn emitters.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/spawn-emitter-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f

$20000 constant SET-CAP

create SET-BUF SET-CAP allot
variable SET-LEN

: SET-SOURCE ( -- ptr u8 n )
   SET-BUF SET-LEN @ ;

: SET-LOAD ( -- )
   s" src/habu/habu1.f" SET-BUF SET-CAP READ-ALL SET-LEN ! ;

: SET-HAS? ( ptr u8 n -- bool )
   SET-SOURCE 2swap CONTAINS? ;

: SET-MUST-HAVE ( ptr u8 n -- )
   SET-HAS? TTRUE ;

: SET-MUST-LACK ( ptr u8 n -- )
   SET-HAS? 0= TTRUE ;

: SET-COUNT ( ptr u8 n -- n ) {: needle:ptr needleu :}
   needleu 0= if 0 exit then
   SET-LEN @ needleu < if 0 exit then
   0 0 begin dup SET-LEN @ needleu - <= while
      SET-BUF over + needleu needle needleu STR= if swap 1+ swap then
      1+
   repeat drop ;

: SET-COUNT= ( ptr u8 n n -- ) {: needle:ptr needleu want :}
   needle needleu SET-COUNT want T= ;

: SET-TEST-HELPERS ( -- )
   s" : SPAWN-DUP2-ACTION ( reg fd -- )" SET-MUST-HAVE
   s" : SPAWN-CHDIR-ACTION ( reg label -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-FRAME3-ENTER ( -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-FRAME4-ENTER ( -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-ACTIONS-RESET ( count -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-STDIO-ACTIONS ( -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-ZERO-ADESC ( -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-FILL-ADESC ( -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-NULLABLE-ADESC ( label -- )" SET-MUST-HAVE
   s" : SPAWN-DARWIN-FINISH ( label label -- )" SET-MUST-HAVE ;

: SET-TEST-HELPER-USES ( -- )
   s" SPAWN-DARWIN-FRAME3-ENTER" 4 SET-COUNT=
   s" SPAWN-DARWIN-FRAME3-LEAVE" 4 SET-COUNT=
   s" SPAWN-DARWIN-FRAME4-ENTER" 2 SET-COUNT=
   s" SPAWN-DARWIN-FRAME4-LEAVE" 2 SET-COUNT=
   s" SPAWN-DARWIN-ACTIONS-RESET" 5 SET-COUNT=
   s" SPAWN-DARWIN-STDIO-ACTIONS" 5 SET-COUNT=
   s" SPAWN-DARWIN-ZERO-ADESC" 5 SET-COUNT=
   s" SPAWN-DARWIN-FILL-ADESC" 5 SET-COUNT=
   s" SPAWN-DARWIN-NULLABLE-ADESC" 4 SET-COUNT=
   s" SPAWN-DARWIN-FINISH" 5 SET-COUNT= ;

: SET-TEST-DARWIN-WRAPPERS ( -- )
   s" 3 >COUNT SPAWN-DARWIN-ACTIONS-RESET" 3 SET-COUNT=
   s" 4 >COUNT SPAWN-DARWIN-ACTIONS-RESET" 1 SET-COUNT=
   s" BSP-SAD @ >LABEL SPAWN-DARWIN-NULLABLE-ADESC" 3 SET-COUNT=
   s" 6 >REG BSP-DN @ >LABEL SPAWN-CHDIR-ACTION" SET-MUST-HAVE
   s" SPAWN-DARWIN-USE-ADESC" 2 SET-COUNT=
   s" SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP" 2 SET-COUNT=
   s" SPAWN-DARWIN-ARGV-DEFAULT-ENVP" 2 SET-COUNT= ;

: SET-TEST-REMOVED-DUPLICATION ( -- )
   s" SP SP 3584 SUBI," SET-MUST-LACK
   s" SP SP 3584 ADDI," SET-MUST-LACK
   s" 13 SP 176 ADDI," SET-MUST-LACK
   s" 15 1040 MOVZ," SET-MUST-LACK
   s" 14 SP 48 STR,  14 SP 56 STR" SET-MUST-LACK ;

: SET-MAIN ( -- )
   T-RESET
   SET-LOAD
   SET-TEST-HELPERS
   SET-TEST-HELPER-USES
   SET-TEST-DARWIN-WRAPPERS
   SET-TEST-REMOVED-DUPLICATION
   T-REPORT
   s" spawn-emitter-test: ok" type cr ;

SET-MAIN
