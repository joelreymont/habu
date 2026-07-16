\ hidx-tombstone-test.f - wrapped collision and stale-slot fixture.

package HIDX-TOMB

public

\ These three package-owned folded names share base slot HIDX-SLOTS-1. The
\ fixture makes the first entry stale, inserts the third through the wrapped
\ cluster, and exposes the slots for positive and duplicate assertions.
: WRAP-4606 ( -- n ) 4606 ;
: WRAP-18438 ( -- n ) 18438 ;

private

variable FIRST-SLOT
variable SECOND-SLOT

$7749109108877FEC constant FIRST-HASH
$5F37B8D5056AFFEC constant SECOND-HASH
$3E5E90DE828DFFEC constant THIRD-HASH
1 DGEN-SHIFT lshift constant GEN-BIT

: INDEX@ ( -- ptr u8 )
   data-base HIDXP-CELL + @ ;

: ENTRY ( n -- ptr u8 )
   HIDX-ENTRY-SHIFT lshift INDEX@ + ;

\ typed-local-lint: allow-bare-local - p preserves the ptr-u8 element role.
: CELL@ ( ptr u8 -- n ) {: p :}
   p c@
   p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or
   p 4 + c@ 32 lshift or
   p 5 + c@ 40 lshift or
   p 6 + c@ 48 lshift or
   p 7 + c@ 56 lshift or ;

\ typed-local-lint: allow-bare-local - p preserves the ptr-u8 element role.
: CELL! ( n ptr u8 -- ) {: x:n p :}
   x p c!
   x 8 rshift p 1+ c!
   x 16 rshift p 2 + c!
   x 24 rshift p 3 + c!
   x 32 rshift p 4 + c!
   x 40 rshift p 5 + c!
   x 48 rshift p 6 + c!
   x 56 rshift p 7 + c! ;

: SLOT-OF ( n -- n )
   {: hash:n :}
   HIDX-SLOTS 0 ?do
      i ENTRY CELL@ hash = if i unloop exit then
   loop
   -1 ;

: STALE-SLOT ( n -- )
   ENTRY cell+ dup CELL@ GEN-BIT xor swap CELL! ;

public

: FIRST-SLOT@ ( -- n )
   FIRST-SLOT @ ;

: SECOND-SLOT@ ( -- n )
   SECOND-SLOT @ ;

: STALE-FIRST ( -- )
   FIRST-HASH SLOT-OF FIRST-SLOT !
   SECOND-HASH SLOT-OF SECOND-SLOT !
   FIRST-SLOT @ STALE-SLOT ;

;package

package HIDX-TOMB

public

STALE-FIRST
: WRAP-28125 ( -- n ) 28125 ;

: THIRD-SLOT@ ( -- n )
   THIRD-HASH SLOT-OF ;

: STALE-THIRD ( -- )
   THIRD-SLOT@ STALE-SLOT ;

;package
