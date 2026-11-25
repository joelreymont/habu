\ import-names.f - an image's imports, which a target writer's BUILD-IMAGE is
\ given: the C names its GOT binds, in row order, each ending in a NUL.

package IMPORT-NAMES
PTR-VARIABLE NAMES
variable NAMES-U
variable NAMES-N

public
: SPAN! ( str -- )
   {: s:str :}
   s STR:DATA NAMES !  s STR:LEN NAMES-U !
   0  s STR:LEN 0 ?do s STR:DATA i + c@ 0= if 1 + then loop  NAMES-N ! ;

: SPAN@ ( -- str ) NAMES @ NAMES-U @ >str ;

: COUNT ( -- n ) NAMES-N @ ;

\ The name at a byte offset in the span.
: NAME-AT ( n -- str )
   {: at:n :}
   NAMES @ at +  0
   begin 2dup + c@ 0<> while 1 + repeat >str ;

\ The byte offset of the name after the one at this offset.
: NEXT-AT ( n -- n ) dup NAME-AT STR:LEN + 1 + ;

\ Whether a walk by NEXT-AT has passed the last name.
: END? ( n -- bool ) NAMES-U @ >= ;
;package
