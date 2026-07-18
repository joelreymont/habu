\ driver-io.f - shared fail-closed I/O for internal build drivers.

variable DRV-WFD
variable DRV-WR
variable DRV-WOFF
variable DRV-WALL-FD
variable DRV-WALL-A
variable DRV-WALL-U

: DRV-RETIRE-RELOADS ( -- )
   s" include" UNDEFINE-IF-DEFINED
   s" included" UNDEFINE-IF-DEFINED
   s" require" UNDEFINE-IF-DEFINED
   s" required" UNDEFINE-IF-DEFINED ;

: DRV-WALL-A-FIELD ( -- ptr ptr u8 )
   DRV-WALL-A 0 ptr-field ;

: DRV-WALL-A@ ( -- ptr u8 )
   DRV-WALL-A-FIELD @ ;

: DRV-WALL-A! ( ptr u8 -- )
   DRV-WALL-A-FIELD ! ;

: DRV-WALL-LEFT ( -- n )
   DRV-WALL-U @ DRV-WOFF @ - ;

: DRV-WALL-DST ( -- ptr u8 )
   DRV-WALL-A@ DRV-WOFF @ + ;

: DRV-WALL-MORE? ( -- bool )
   DRV-WOFF @ DRV-WALL-U @ < ;

: DRV-WALL-STEP ( -- )
   DRV-WALL-FD @ DRV-WALL-DST DRV-WALL-LEFT write DRV-WR !
   DRV-WR @ 0 <= IF s" driver: write failed" 74 die THEN
   DRV-WOFF @ DRV-WR @ + DRV-WOFF ! ;

: DRV-WALL ( n ptr u8 n -- )
   DRV-WALL-U !
   DRV-WALL-A!
   DRV-WALL-FD !
   0 DRV-WOFF !
   BEGIN DRV-WALL-MORE? WHILE DRV-WALL-STEP REPEAT ;

: DRV-WRITE-IMAGE-PATH ( ptr u8 n -- )
   PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" driver: cannot open output" 74 die THEN
   DRV-WFD @ MBUF MLEN@ DRV-WALL
   DRV-WFD @ close ;

: DRV-WRITE-IMAGE ( img ptr u8 n -- ) {: phase path:ptr pathu :}
   phase IMG-DROP
   path pathu DRV-WRITE-IMAGE-PATH ;

74 constant DRV-SIZE-RC

\ Size attribution tail. EMIT-FORTH records one row per emitter phase, but those
\ rows only cover the assembled __text; the built and signed image also holds the
\ header/load commands padded to CODE-OFF, the zero fill to the __TEXT page
\ boundary, and the target tail (Mach-O: __DATA_CONST + __LINKEDIT + code
\ signature; ELF: the read-write segment). These words attribute those bytes onto
\ the same table so every byte of the file lands on a named region.
: DRV-SIZE-TEXTPAD ( -- n )
   TEXTSZ CODE-OFF CODELEN @ + - ;

: DRV-SIZE-TAIL-MARK ( n -- ) {: i:n :}
   i IMG-TAIL-NAME i IMG-TAIL-BYTES ENGINE-SIZE:MARK-BYTES ;

: DRV-SIZE-TAIL ( -- )
   0 begin dup IMG-TAIL-N < while
      dup DRV-SIZE-TAIL-MARK
      1+
   repeat drop ;

: DRV-SIZE-MARKS ( -- )
   s" container/header" CODE-OFF ENGINE-SIZE:MARK-BYTES
   s" container/text-pad" DRV-SIZE-TEXTPAD ENGINE-SIZE:MARK-BYTES
   DRV-SIZE-TAIL ;

\ Fail closed: the sum of every recorded region must equal the exact image
\ length. A residue means an emitted region has no named row - the build stops
\ rather than ship an engine whose size the map cannot fully explain.
: DRV-SIZE-RECONCILE ( -- )
   ENGINE-SIZE:TOTAL MLEN@ <> if
      s" driver: size map does not reconcile to image length" DRV-SIZE-RC die
   then ;

\ Runs only for a full engine emission - EMIT-FORTH left the phase rows in the
\ table; object images leave it empty and are skipped. Must run after CODESIG2 so
\ the signature row (SB-SIZE) and MLEN@ are final. HABU_ENGINE_SIZE_MAP prints the
\ reconciled rows for capture by tools/size-report.f.
: DRV-SIZE-MAP ( -- )
   ENGINE-SIZE:COUNT 0= if exit then
   DRV-SIZE-MARKS
   DRV-SIZE-RECONCILE
   s" HABU_ENGINE_SIZE_MAP" GETENV nip 0 > if ENGINE-SIZE:REPORT then ;

\ The single high-level image-emission tail: assemble the current CODE into the
\ target image, sign it with the caller's sigid, and write it to path. Every
\ engine driver (stage2/build/stdin/maker/aot-lib) and the object image writer
\ (tools/object-image.f OBJIMG:WRITE) route through this one word, so exactly one
\ BUILD-IMAGE+sign+write implementation exists. Loads after the target image
\ writer (macho/elf + sign) in every context that includes driver-io.f.
: DRV-EMIT-IMAGE ( ptr u8 n ptr u8 n -- ) {: sig:ptr sigu:n path:ptr pathu:n :}
   ASM-CODE BUILD-IMAGE
   sig sigu SET-SIGID CODESIG2
   DRV-SIZE-MAP
   path pathu DRV-WRITE-IMAGE ;

: DRV-EXIT-OK ( -- )
   s" " 0 die ;

\ Uncaught-throw boundary reporting. Driver runners catch at top level and
\ report here so a build failure always names its throw code (proven need: an
\ image-buffer overrun once exited 75 with no output). The exit code stays the
\ raw throw code when kernel-representable so existing rc contracts hold (check
\ reject 70, image-bytes bounds 75); for any other code the die primitive maps
\ the exit status to the deterministic UNCAUGHT-RC instead of the silently
\ masked `code & 0xFF` (which turned -2816 into rc 0).
create DRV-FC 1 allot
create DRV-FB 24 allot
variable DRV-FV
variable DRV-FN

\ Diagnostic-path writes: a failed stderr write has nowhere further to report.
: DRV-W2 ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

: DRV-B2 ( n -- ) {: c:n :}
   c DRV-FC c!
   DRV-FC 1 DRV-W2 ;

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
      DRV-FB DRV-FN @ + 1 DRV-W2
   REPEAT ;

: DRV-FAIL-CODE ( n -- ) {: rc:n :}
   rc 0 < IF 45 DRV-B2 THEN
   rc 0 < IF 0 rc - ELSE rc THEN DRV-FV !
   DRV-FAIL-DIGITS ;

: DRV-FAIL ( n -- ) {: rc:n :}
   s" driver: uncaught throw code " DRV-W2
   rc DRV-FAIL-CODE
   10 DRV-B2
   s" " rc die ;
