\ kept-genio.f - a program built by tools/hb-build.f with --repl, since
\ genio-core.f calls the REPL's KEY1: its top level builds a device and keeps
\ its handle in OLD; MAIN builds another device and closes OLD, which
\ E-GENIO-STATE refuses.

require src/lib/error.f
require src/lib/genio-core.f

package FILE-OBJECTS-KEPT-GENIO
private
: NOTHING ( -- ) ;
: DROP-CHAR ( n -- ) drop ;
: NO-KEY ( -- n ) GENIO:E-IO throw ;
: READY? ( -- bool ) 0 0= ;
: NO-READ ( ptr u8 n -- n ) {: a:ptr u:n :} 0 ;
: DROP-BYTES ( ptr u8 n -- ) {: a:ptr u:n :} ;
: NO-ACCEPT ( ptr u8 n -- n ) {: a:ptr u:n :} 0 ;

: BUILD ( -- GENIO:device )
   [: DROP-CHAR ;] [: NO-KEY ;] [: READY? ;] [: NO-READ ;]
   [: DROP-BYTES ;] [: NO-ACCEPT ;] [: NOTHING ;] [: NOTHING ;]
   0 GENIO:DEVICE ;

TYPED-VARIABLE OLD GENIO:device
BUILD OLD !

public
: CHECK ( -- )
   BUILD {: own:GENIO:device :}
   [: OLD @ GENIO:CLOSE ;] catch {: code:n :}
   code E-GENIO-STATE = if ." genio refused" cr else ." genio " code . cr then
   code 0<> if own GENIO:CLOSE then ;
;package

: MAIN ( -- ) FILE-OBJECTS-KEPT-GENIO:CHECK ;
