\ driver-io.f - shared fail-closed I/O for internal build drivers. It reaches
\ MBUF, so a booted engine cannot load it; what a booted engine needs lives in
\ src/habu/fdio.f (FDIO:WALL) and src/habu/sign-id.f.

require src/habu/fdio.f
require src/habu/sign-id.f
require src/os/image-bytes.f
using IMAGE-BYTES

package DRIVER
variable DRV-WFD

public
: DRV-RETIRE-RELOADS ( -- )
   s" include" UNDEFINE-IF-DEFINED
   s" included" UNDEFINE-IF-DEFINED
   s" require" UNDEFINE-IF-DEFINED
   s" required" UNDEFINE-IF-DEFINED ;

private
: DRV-WRITE-IMAGE-PATH ( ptr u8 n -- )
   PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" driver: cannot open output" 74 die THEN
   DRV-WFD @ MBUF MLEN@ FDIO:WALL
   DRV-WFD @ close ;

public
: DRV-WRITE-IMAGE ( img ptr u8 n -- ) {: phase path:ptr pathu :}
   phase IMG-DROP
   path pathu DRV-WRITE-IMAGE-PATH ;

: DRV-EXIT-OK ( -- )
   s" " 0 die ;

private
\ DRV-FAIL prints the throw code: die exits UNCAUGHT-RC for one outside [0,255]
\ (src/habu/habu1.f BDIE), so the exit status alone can lose it.
create DRV-FC 1 allot
create DRV-FB 24 allot
variable DRV-FV
variable DRV-FN

: DRV-B2 ( n -- ) {: c:n :}
   c DRV-FC c!
   DRV-FC 1 ERROR-TEXT:TYPE ;

: DRV-FAIL-DIGITS ( -- )
   0 DRV-FN !
   DRV-FV @ 0 = IF 48 DRV-B2 EXIT THEN
   BEGIN DRV-FV @ 0 > WHILE
      DRV-FV @ 10 mod 48 +  DRV-FB DRV-FN @ + c!
      DRV-FN @ 1 + DRV-FN !
      DRV-FV @ 10 / DRV-FV !
   REPEAT
   BEGIN DRV-FN @ 0 > WHILE
      DRV-FN @ 1 - DRV-FN !
      DRV-FB DRV-FN @ + 1 ERROR-TEXT:TYPE
   REPEAT ;

: DRV-FAIL-CODE ( n -- ) {: rc:n :}
   rc 0 < IF 45 DRV-B2 THEN
   rc 0 < IF 0 rc - ELSE rc THEN DRV-FV !
   DRV-FAIL-DIGITS ;

public
: DRV-FAIL ( n -- ) {: rc:n :}
   s" driver: uncaught throw code " ERROR-TEXT:TYPE
   rc DRV-FAIL-CODE
   10 DRV-B2
   s" " rc die ;
;package
;using
