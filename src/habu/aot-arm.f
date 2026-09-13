\ Capture cursors and the checker payload share one opening/closing operation.
\ Load this before the captured source so the capture tool's own dependencies
\ cannot become unintended call targets in the artifact.

require src/core/checker-owner-guard.f

package AOT-ARM

\ Raw cell boundary, the same shape aot-capture.f uses for the same two cells:
\ the live DATA base is a `ptr n` and storing a cell through it is what the
\ checker cannot state on its own. Retirement: habu-builder-trust-rows-c5d41af6.
: LIVE ( -- ptr n ) data-base ;
: CELL! ( n ptr n -- ) ! ;

public

\ Retain the existing engine DATA coordinates; call emission no longer uses
\ these slots to decide whether to copy a body.
: OPEN ( n n -- ) {: b0:n d0:n :}
   d0 LIVE AOT-WINDOW:D0-CELL + CELL!
   b0 LIVE AOT-WINDOW:B0-CELL + CELL! ;

\ The engine's next wordlist id. It is read here, beside the window's other base
\ cursors, because every producer needs it at the same two moments they need those
\ - when the window opens and when it closes.
: WIDN ( -- n ) LIVE WIDN-CELL + @ ;

\ ---- the window's four coordinates -------------------------------------------
\
\ A CAPTURE WINDOW IS FOUR SPANS, NOT ONE, and every producer needs all four at
\ the same two moments. Four of them wrote the pair of latch lines out by hand -
\ the stdin driver, the chain capture tool, the band fixture and the merge
\ fixture - and a fifth generated the text. Latching three of the four, or
\ reading one of them at some other moment, is a silent wrong capture rather than
\ a refusal: the wordlist counter especially, because a capture tool's own
\ tooling opens packages AFTER the window closes, so a WIDN read at capture time
\ answers for the tool and not for the window.
variable B0  variable B1      \ the window's code span
variable R0  variable R1      \ its dictionary record span
variable D0  variable D1      \ its DATA span
variable W0  variable W1      \ its wordlist span

\ A payload belongs to one concrete checker instance for the whole window.
PTR-VARIABLE PAYLOAD-OWNER
variable PAYLOAD-MODE                       \ 0 pending, 1 partial, 2 complete runtime
variable PAYLOAD-FROZEN
variable PAYLOAD-EXPORTED

private

: PAYLOAD-BAD ( -- )
   s" aot-arm: missing, changed or unfrozen checker payload owner" 74 die ;

: SOURCE-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

: OWNER! ( ptr u8 -- )
   CHECKER-OWNER-ABI:BYTES CHECKER-OWNER-GUARD:VALIDATE
   dup SOURCE-OWNER <> if drop PAYLOAD-BAD then
   PAYLOAD-OWNER ! ;

: PAYLOAD-FIELD ( n -- n )
   PAYLOAD-OWNER @ over CELL + CHECKER-OWNER-GUARD:VALIDATE
   swap + CELL-VIEW @ dup 0= if drop PAYLOAD-BAD then ;

TRUSTED: AS-ACTION ( n -- [ -- ] ) ;
TRUSTED: AS-LOOKUP ( n -- [ ptr u8 n bool ptr u8 n -- n bool ] ) ;
TRUSTED: AS-SPANS ( n -- [ -- ptr u8 n ptr u8 n ] ) ;
TRUSTED: AS-SAVE ( n -- [ ptr u8 n -- n ] ) ;

: RUN-ACTION ( n -- ) PAYLOAD-FIELD AS-ACTION execute ;

: CANCEL ( -- )
   PAYLOAD-MODE @ 1 = if
      CHECKER-OWNER-ABI:PAYLOAD-DISARM-OFF RUN-ACTION then
   0 PAYLOAD-MODE !  0 PAYLOAD-FROZEN ! 0 PAYLOAD-EXPORTED !
   NULL-PTR PAYLOAD-OWNER ! ;

public

: PAYLOAD-ARM ( ptr u8 -- )
   CANCEL OWNER!
   1 PAYLOAD-MODE !
   CHECKER-OWNER-ABI:PAYLOAD-ARM-OFF RUN-ACTION ;

: PAYLOAD-PERSISTENT ( ptr u8 -- )
   CANCEL OWNER!
   2 PAYLOAD-MODE ! ;

: SIG-CLOSE ( -- )
   PAYLOAD-FROZEN @ 0= 0= if exit then
   PAYLOAD-MODE @ 0= if PAYLOAD-BAD then
   PAYLOAD-OWNER @ SOURCE-OWNER <> if PAYLOAD-BAD then
   PAYLOAD-MODE @ 1 = if
      CHECKER-OWNER-ABI:PAYLOAD-FREEZE-OFF RUN-ACTION then
   -1 PAYLOAD-FROZEN ! ;

: ?FROZEN ( -- )
   PAYLOAD-FROZEN @ 0= if PAYLOAD-BAD then
   PAYLOAD-OWNER @ SOURCE-OWNER <> if PAYLOAD-BAD then ;

: PAYLOAD-LOOKUP ( ptr u8 n bool ptr u8 n -- n bool )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-LOOKUP-OFF PAYLOAD-FIELD AS-LOOKUP execute ;

: PAYLOAD-SPANS ( -- ptr u8 n ptr u8 n )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-SPANS-OFF PAYLOAD-FIELD AS-SPANS execute ;

: PAYLOAD-REG-SAVE ( ptr u8 n -- n )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-REG-SAVE-OFF PAYLOAD-FIELD AS-SAVE execute ;

private

: LATCH-OPEN ( -- )
   CANCEL
   cp@ B0 ! ndict@ R0 ! here D0 ! WIDN W0 ! ;

public

\ Latch all four spans and bind the payload to this checker owner.
: WINDOW-OPEN ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

\ Compatibility entry with zero engine coordinates and the same captured spans.
: WINDOW-OPEN-UNARMED ( -- )
   LATCH-OPEN
   0 0 OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

: WINDOW-OPEN-PERSISTENT ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN ;

\ Where the window's definitions end. The wordlist counter is latched HERE for
\ the reason above, and never read again at capture time.
: WINDOW-CLOSE ( -- )
   cp@ B1 ! ndict@ R1 ! here D1 ! WIDN W1 !
   SIG-CLOSE ;

\ The window as aot-capture.f CAPTURE takes it. One reader, so a caller cannot
\ hand the six in a different order than the next caller does.
: WINDOW$ ( -- n n n n n n )
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @ ;

;package
