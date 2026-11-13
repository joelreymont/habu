\ import-names.f - an image's imports, which a target writer's BUILD-IMAGE is
\ given: the C names its GOT binds, in row order, each ending in a NUL.

package IMPORT-NAMES
PTR-VARIABLE NAMES
variable NAMES-U
variable NAMES-N

public
: SPAN! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a NAMES !  u NAMES-U !
   0  u 0 ?do a i + c@ 0= if 1 + then loop  NAMES-N ! ;

: SPAN@ ( -- ptr u8 n ) NAMES @ NAMES-U @ ;

: COUNT ( -- n ) NAMES-N @ ;

\ The name at a byte offset in the span.
: NAME-AT ( n -- ptr u8 n )
   {: at:n :}
   NAMES @ at +  0
   begin 2dup + c@ 0<> while 1 + repeat ;

\ The byte offset of the name after the one at this offset.
: NEXT-AT ( n -- n ) dup NAME-AT nip + 1 + ;

\ Whether a walk by NEXT-AT has passed the last name.
: END? ( n -- bool ) NAMES-U @ >= ;
;package
