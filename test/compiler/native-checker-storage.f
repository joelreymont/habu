\ Loaded after the checker prefix; exercise real pointer-pool clearing.
package CLEAR-RANGE-TEST
create SLOTS 3 cells allot
: SLOT ( n -- ptr ptr u8 ) SLOTS swap ptr-field ;
: LIVE ( -- ptr u8 ) SLOTS BYTE-VIEW ;
: INIT ( -- ) LIVE 0 SLOT ! LIVE 1 SLOT ! LIVE 2 SLOT ! ;
TRUSTED: CLEAR ( ptr ptr u8 n n -- ) REG-POINTERS-CLEAR ;
: CHECK ( -- )
   INIT
   0 SLOT 1 1 CLEAR
   0 SLOT 2 1 CLEAR
   0 SLOT @ LIVE <> 1 SLOT @ LIVE <> or 2 SLOT @ LIVE <> or if
      s" empty pointer clear changed storage" 76 die then
   0 SLOT 1 2 CLEAR
   0 SLOT @ LIVE <> 1 SLOT @ NULL-PTR <> or 2 SLOT @ LIVE <> or if
      s" pointer clear changed the wrong range" 76 die then ;
CHECK
;package
