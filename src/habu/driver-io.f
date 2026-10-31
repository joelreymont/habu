\ driver-io.f - shared fail-closed I/O for internal build drivers. It reaches
\ MBUF, BUILD-IMAGE and the target's signer, so a booted engine cannot load it;
\ what a booted engine needs lives in src/habu/fdio.f (FDIO:WALL, loaded just
\ before this file) and src/habu/sign-id.f.

require src/habu/sign-id.f

variable DRV-WFD

: DRV-RETIRE-RELOADS ( -- )
   s" include" UNDEFINE-IF-DEFINED
   s" included" UNDEFINE-IF-DEFINED
   s" require" UNDEFINE-IF-DEFINED
   s" required" UNDEFINE-IF-DEFINED ;

: DRV-WRITE-IMAGE-PATH ( ptr u8 n -- )
   PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" driver: cannot open output" 74 die THEN
   DRV-WFD @ MBUF MLEN@ FDIO:WALL
   DRV-WFD @ close ;

: DRV-WRITE-IMAGE ( img ptr u8 n -- ) {: phase path:ptr pathu :}
   phase IMG-DROP
   path pathu DRV-WRITE-IMAGE-PATH ;

\ The one BUILD-IMAGE, sign and write path: every engine driver and
\ tools/native-emit.f route through it, naming SIGN-ID:PROG$ or SIGN-ID:ENGINE$.
\ Loads after the target image writer (macho/elf + sign).
: DRV-EMIT-IMAGE ( ptr u8 n ptr u8 n -- ) {: sig:ptr sigu:n path:ptr pathu:n :}
   ASM-CODE BUILD-IMAGE
   sig sigu SET-SIGID CODESIG2
   path pathu DRV-WRITE-IMAGE ;

: DRV-EXIT-OK ( -- )
   s" " 0 die ;

\ Uncaught-throw reporting at the driver boundary, so a build failure names its
\ throw code (an image-buffer overrun once exited 75 with no output). The exit
\ code stays the raw throw code when the kernel can represent it (check reject
\ 70, image bounds 75); die maps any other to UNCAUGHT-RC, never `code & 0xFF`
\ (which turned -2816 into rc 0).
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

: DRV-FAIL ( n -- ) {: rc:n :}
   s" driver: uncaught throw code " ERROR-TEXT:TYPE
   rc DRV-FAIL-CODE
   10 DRV-B2
   s" " rc die ;
