\ The fixed address-bearing engine slots that a native build may carry.
\ This module loads once against the host layout and again in the target writer.
require src/habu/layout.f
require src/habu/native-observer-cells.f
require src/habu/aot-decl.f

package NATIVE-LAYOUT
private

\ Each row has a semantic identity by position, a DATA offset and a target kind
\ (0 code, 1 DATA). Aliases of a slot must not appear as separate identities.
create SLOTS
   HOOK-CELL ,                         0 ,
   COMPILE-PREFLIGHT-CELL ,             0 ,
   TOP-HOOK-CELL ,                      0 ,
   EXIT-HOOK-CELL ,                     0 ,
   UNCGH-CELL ,                         0 ,
   NATIVE-OBS-CELLS:OBSERVE ,           0 ,
   NATIVE-OBS-CELLS:PUBLISHED ,         0 ,
   NATIVE-OBS-CELLS:INVALIDATE ,        0 ,
   NCOMP-DISPATCH:XT-CELL ,              0 ,
   NCOMP-DISPATCH:FIXED-SHADOW-CELL ,    0 ,
   NCOMP-DISPATCH:DOES-SHADOW-CELL ,     0 ,
   TASK-CHAIN-CELL ,                     1 ,
   NCOMP-DISPATCH:DECL-CELL ,            1 ,
   NCOMP-DISPATCH:TARGET-DECL-CELL ,     1 ,
   PROVIDED-XT:EVALUATE-CELL ,          0 ,
   POLICY-ABI:KEYWORD-CELL ,            0 ,
   ENGINE-MAIN:XT-CELL ,                0 ,
   ENGINE-MAIN:REPORT-CELL ,            0 ,
   APP-ENTRY:XT-CELL ,                  0 ,
   REPLH-CELL ,                         0 ,
   BPWBASE-CELL ,                       1 ,
   LASTC-CELL ,                         0 ,
   CREATEP-CELL ,                       0 ,
here SLOTS - 2 cells / constant ROWS

: SLOT ( ptr n n -- ptr n ) 2 * cells + ;
: OFFSET ( ptr n n -- n ) SLOT @ ;
: KIND ( ptr n n -- n ) SLOT cell+ @ ;

: REFUSE ( -- )
   s" native-build: incompatible fixed engine layout" 74 die ;

: COUNT-CHECK ( n -- ) ROWS <> if REFUSE then ;

public

\ The caller retains this read-only table through reset and passes it explicitly
\ to the writer. It is build metadata and never enters the captured image.
: CURRENT ( -- ptr n n ) SLOTS ROWS ;

: CHECK ( ptr n n n -- ) {: table:ptr count:n floor:n :}
   count COUNT-CHECK
   floor CELL < if REFUSE then
   count 0 ?do
      table i OFFSET {: off:n :}
      off 0 < off floor CELL - > or if REFUSE then
      table i KIND SLOTS i KIND <> if REFUSE then
      i 0 ?do table i OFFSET off = if REFUSE then loop
   loop ;

\ Find a semantic slot using its host location, then read that same slot from
\ the source-bound table. Numeric offsets alone do not identify engine hooks.
: TRANSLATE ( ptr n n n bool -- n ) {: host:ptr count:n off:n data?:bool :}
   count COUNT-CHECK
   count 0 ?do
      host i OFFSET off = if
         data? if 1 else 0 then SLOTS i KIND <> if REFUSE then
         SLOTS i OFFSET unloop exit
      then
   loop
   s" native-build: unknown fixed cell offset " type off .
   REFUSE ;

: TRANSLATE-ROWS ( ptr n n -- ) {: host:ptr count:n :}
   CURRENT DATA-START CHECK
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + CELL-VIEW {: row:ptr :}
      row @ {: pair:n :}
      pair $FFFFFFFF and {: loc:n :}
      loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if
         host count loc pair 32 rshift AOT-WINDOW:XTOFF-DATA-TAG and 0<>
         TRANSLATE pair $FFFFFFFF00000000 and or row !
      then
   loop ;

;package
