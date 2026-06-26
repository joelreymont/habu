\ driver-io.f - shared fail-closed I/O for internal build drivers.

variable DRV-WFD
variable DRV-WR
variable DRV-WOFF
variable DRV-WALL-FD
variable DRV-WALL-A
variable DRV-WALL-U

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
   DRV-WFD @ MBUF MLEN @ DRV-WALL
   DRV-WFD @ close ;

TRUSTED: DRV-WRITE-IMAGE ( img ptr u8 n -- )
   DRV-WRITE-IMAGE-PATH ;

: DRV-EXIT-OK ( -- )
   s" " 0 die ;
