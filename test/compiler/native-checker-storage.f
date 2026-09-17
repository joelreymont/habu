\ Loaded after the checker prefix; exercise real pointer-pool clearing.
package CLEAR-RANGE-TEST

\ Three DECLARED pointer cells, laid out back to back so REG-POINTERS-CLEAR can
\ be handed a sub-range of them. PTR-VARIABLE is the only declared form this
\ window has: TYPED-BUFFER needs the TDECL-EVAL-ARMED that src/core/include.f
\ sets, which the rebuilt checker prefix never reaches, and throws
\ E-LAYOUT-BUFFER here. So the adjacency the range walk depends on is asserted
\ rather than assumed.
PTR-VARIABLE SLOT0
PTR-VARIABLE SLOT1
PTR-VARIABLE SLOT2

: LIVE ( -- ptr u8 ) SLOT0 BYTE-VIEW ;
: ADJACENT ( -- )
   SLOT0 cell+ SLOT1 <> SLOT1 cell+ SLOT2 <> or if
      s" pointer slots are not contiguous" 76 die then ;
: INIT ( -- ) LIVE SLOT0 ! LIVE SLOT1 ! LIVE SLOT2 ! ;
TRUSTED: CLEAR ( ptr ptr u8 n n -- ) REG-POINTERS-CLEAR ;
: CHECK ( -- )
   ADJACENT
   INIT
   SLOT0 1 1 CLEAR
   SLOT0 2 1 CLEAR
   SLOT0 @ LIVE <> SLOT1 @ LIVE <> or SLOT2 @ LIVE <> or if
      s" empty pointer clear changed storage" 76 die then
   SLOT0 1 2 CLEAR
   SLOT0 @ LIVE <> SLOT1 @ NULL-PTR <> or SLOT2 @ LIVE <> or if
      s" pointer clear changed the wrong range" 76 die then ;
CHECK
;package
