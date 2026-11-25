\ code-owner-main.f - CLI entry.
\ The argument is a REGION OFFSET, not an address: the region moves every boot, so
\ an absolute address caught in one process is meaningless in this one.

require src/lib/error.f
require src/lib/string.f
require src/lib/adt/option.f
require tools/code-owner.f
require src/core/exit-status.f

package CODE-OWNER-CLI

: ?ARGS ( -- )
   SCRIPT-ARGC 0 > if exit then
   s" usage: --load tools/code-owner-main.f -- <region offset>"
   EXIT-STATUS:USAGE die ;

: HEX-BODY ( str -- str bool ) {: s:str :}
   s STR:LEN 1 > if
      s STR:DATA c@ 36 = if
         s STR:DATA 1 + s STR:LEN 1 - >str 0 0= exit
      then
   then

   s 0 0= 0= ;

: DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c STR:ASCII-LOWER {: l:n :}
   l 97 >= l 102 <= and if l 87 - exit then
   -1 ;

variable ACC
: HEX>N ( str -- n ) {: s:str :}
   0 ACC !
   s STR:LEN 0 ?do
      s STR:DATA i + c@ DIGIT {: d:n :}
      d 0 < if s" code-owner: offset is not a number" EXIT-STATUS:USAGE die then
      ACC @ 16 * d + ACC !
   loop
   ACC @ ;

\ `$1a2b` or plain decimal, the two spellings a debugger prints.
: BAD ( -- ) s" code-owner: offset is not a number" EXIT-STATUS:USAGE die ;

: OFFSET ( str -- n )
   HEX-BODY if HEX>N exit then
   STR:>NUMBER? MATCH option
     none OF BAD ENDOF
     some OF ENDOF
   ;MATCH ;

public

: MAIN ( -- )
   ?ARGS
   0 SCRIPT-ARGV$ OFFSET
   CODE-OWNER:REGION-OWNER. ;

;package

CODE-OWNER-CLI:MAIN
