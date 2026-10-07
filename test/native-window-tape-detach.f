\ Exercise the real fresh checker's final capture seam and later tape reuse.
\ test/native-window-capture.f runs RUN after payload validation's preparations
\ and the prefix-boundary mark, so no PREPARE here is the window's first. None
\ needs to be: DETACH heads every CHECKER-CAPTURE-PREPARE, and each PREPARE
\ below follows its own OBSERVE or a checked detach.
package CHECKER-TAPE

variable SEEN
: CHECK-SCAN ( ptr u8 n -- ) 2drop 1 SEEN +! ;
: CHECK-TOKEN ( ptr u8 n n n n n -- ) 2drop 2drop 2drop 10 SEEN +! ;
: CHECK-DONE ( ptr u8 n n -- ) 2drop drop 100 SEEN +! ;
: EQ! ( n n -- ) <> if 79 throw then ;

defer PREPARE ( -- )
\ The owner record holds its prepare entry as a code address integer.
CAST: AS-PREPARE ( n -- [ -- ] )
: BIND-PREPARE ( -- )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   NCOMP-DISPATCH:DECL-CAPTURE-OFF + CELL-VIEW @ AS-PREPARE is PREPARE ;
BIND-PREPARE

\ Inject the checker's own events at this whitebox boundary. Ordinary source
\ owns the declared observer API, not SCAN/TOKEN/DONE's event-production path.
: EVENTS ( -- )
   s" name" SCAN
   s" name" 0 0 TOKEN
   s" name" -1 DONE ;

: OBSERVE ( -- )
   91 ['] CHECK-SCAN ['] CHECK-TOKEN ['] CHECK-DONE INSTALL ;

: DETACHED! ( -- )
   INSTALLED-BY 0 EQ!  SET @ 0 EQ!  ARMED @ 0 EQ!
   0 SEEN ! EVENTS SEEN @ 0 EQ! ;

: RUN ( -- )
   OBSERVE EVENTS SEEN @ 111 EQ!
   PREPARE DETACHED!
   PREPARE DETACHED!
   OBSERVE ARM DISARM
   INSTALLED-BY 91 EQ!
   0 SEEN ! EVENTS SEEN @ 111 EQ!
   PREPARE DETACHED! ;

;package
