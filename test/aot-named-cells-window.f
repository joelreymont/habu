\ Captured declarations only. The producer installs prefix values after closing
\ this window, so no existing named instruction-chain behavior is involved.
package NAMED-CELLS-WINDOW
public
TYPED-VARIABLE GLOBAL-SLOT [ n -- bool ]
TYPED-VARIABLE PUBLIC-SLOT [ n -- n ]
TYPED-VARIABLE LOCAL-SLOT [ n -- n ]
TYPED-VARIABLE NULL-SLOT [ n -- n ]
PERSISTED-PTR-VARIABLE DATA-SLOT
PERSISTED-PTR-VARIABLE NULL-DATA-SLOT
variable VALUE
defer UNASSIGNED ( -- )

\ The null pointer cells are declared from the high address down. Every
\ intervening scalar must still be copied through the sparse DATA bitmap.
16384 constant GAP-COUNT
create GAP-CELLS GAP-COUNT 2 * cells allot
: GAP-SCALAR ( n -- n ) $5A000000 + ;
: GAP-INIT ( -- )
   GAP-COUNT 0 ?do
      GAP-COUNT i - 1- {: idx:n :}
      GAP-CELLS idx 2 * cells + dup ptr-cell-mark 0 swap !
      idx GAP-SCALAR GAP-CELLS idx 2 * 1+ cells + !
   loop ;
: GAP-CHECK ( -- )
   GAP-COUNT 0 ?do
      GAP-CELLS i 2 * cells + @ 0<> if 79 throw then
      GAP-CELLS i 2 * 1+ cells + @ i GAP-SCALAR <> if 79 throw then
   loop ;

: LOCAL ( n -- n ) 9 + ;
: GLOBAL ( n -- bool ) GLOBAL-SLOT @ execute ;
: PUBLIC-VALUE ( n -- n ) PUBLIC-SLOT @ execute ;
: INTERNAL ( n -- n ) LOCAL-SLOT @ execute ;
: DATA-VALUE ( -- n ) DATA-SLOT @ @ ;
: CALL-UNASSIGNED ( -- ) UNASSIGNED ;

\ Reading a null quotation as data checks preservation without executing it.
: NULL-CODE? ( -- bool ) NULL-SLOT byte-view cell-view @ 0= ;
: NULL-DATA? ( -- bool ) NULL-DATA-SLOT @ 0= ;
: CHECK ( -- )
   65 GLOBAL 0= if 79 throw then
   0 PUBLIC-VALUE 0 CODE-RECLAIM:FLOOR-FROM <> if 79 throw then
   10 INTERNAL 19 <> if 79 throw then
   DATA-VALUE $5A5A <> if 79 throw then
   NULL-CODE? NULL-DATA? and 0= if 79 throw then
   GAP-CHECK
   s" named-cells: live" type cr ;
;package
