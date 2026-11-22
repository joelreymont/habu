split-str
\ genio-repl-device.f - makes the current input a device that sends four lines,
\ then -1: one whose READ-LINE takes the next, 300 x bytes, and prints its length
\ before DEVICE-REPL; an empty line; one that prints DEVICE-AFTER.
require src/lib/genio-core.f

package GENIO-REPL-DEVICE
private

4 constant LINES
300 constant XS-LEN
$78 constant CH-X
create XS XS-LEN allot

variable LINE-AT                \ the line the next accept sends
variable BYTE-AT                \ its first byte not yet sent

: LINE$ ( n -- str ) {: k:n :}
   k 0= if s\" GENIO:READ-LINE str> nip . .\" DEVICE-REPL\" cr" exit then
   k 1 = if XS XS-LEN exit then
   k 2 = if XS 0 exit then
   s\" .\" DEVICE-AFTER\" cr" ;

\ An unfilled buffer ends the line, so a line that fills one ends at the next.
: SENT ( n n -- ) {: len:n cap:n :}
   len cap < if 1 LINE-AT +!  0 BYTE-AT ! exit then
   len BYTE-AT +! ;

: LINE-ACCEPT ( ^u8 n -- n ) {: bytes:^ cap:n :}
   LINE-AT @ LINES >= if -1 exit then
   LINE-AT @ LINE$ {: a:^ u:n :}
   u BYTE-AT @ - cap min {: len:n :}
   len 0 ?do a BYTE-AT @ + i + c@ bytes i + c! loop
   len cap SENT
   len ;

: NO-EMIT ( n -- ) drop ;
: NO-KEY ( -- n ) GENIO:E-IO throw ;
: NO-READY? ( -- bool ) false ;
: NO-READ ( ^u8 n -- n ) {: a:^ cap:n :} 0 ;
: NO-WRITE ( str -- ) {: a:^ u:n :} ;
: NOTHING ( -- ) ;

public

: INSTALL ( -- )
   0 LINE-AT !  0 BYTE-AT !
   XS-LEN 0 ?do CH-X XS i + c! loop
   [: NO-EMIT ;] [: NO-KEY ;] [: NO-READY? ;] [: NO-READ ;]
   [: NO-WRITE ;] [: LINE-ACCEPT ;] [: NOTHING ;] [: NOTHING ;]
   0 GENIO:DEVICE GENIO:INPUT! ;

;package

GENIO-REPL-DEVICE:INSTALL
