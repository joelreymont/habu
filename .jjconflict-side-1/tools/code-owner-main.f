\ code-owner-main.f - CLI entry for tools/code-owner.f.
\ Run: <engine> --load tools/code-owner.f tools/code-owner-main.f -- <region offset>
\ The argument is a REGION OFFSET, not an address: the region moves every boot, so
\ an absolute address caught in one process is meaningless in this one.

require lib/errors.f
require lib/string.f
require lib/adt/option.f

package CODE-OWNER-CLI

74 constant RC

: ?ARGS ( -- )
   SCRIPT-ARGC 0 > if exit then
   s" usage: --load tools/code-owner.f tools/code-owner-main.f -- <region offset>"
   RC die ;

: HEX-BODY ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}
   u 1 > a c@ 36 = and if a 1 + u 1 - 0 0= exit then
   a u 0 0= 0= ;

: DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c ASCII-LOWER {: l:n :}
   l 97 >= l 102 <= and if l 87 - exit then
   -1 ;

variable ACC
: HEX>N ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 ACC !
   u 0= if s" code-owner: empty offset" RC die then
   u 0 ?do
      a i + c@ DIGIT {: d:n :}
      d 0 < if s" code-owner: offset is not a number" RC die then
      ACC @ 16 * d + ACC !
   loop
   ACC @ ;

\ `$1a2b` or plain decimal, the two spellings a debugger prints.
: BAD ( -- ) s" code-owner: offset is not a number" RC die ;

: OFFSET ( ptr u8 n -- n )
   HEX-BODY if HEX>N exit then
   STR>NUMBER? MATCH option
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
