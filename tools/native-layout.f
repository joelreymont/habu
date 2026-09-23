\ The fixed address-bearing engine slots that a native build may carry.
\ This module loads once against the host layout and again in the target writer.
require src/habu/layout.f

package NATIVE-LAYOUT
private

\ Each row has a semantic identity by position, a DATA offset and a target kind
\ (0 code, 1 DATA). Aliases of a slot must not appear as separate identities.
create SLOTS
   HOOK-CELL ,                         0 ,
   COMPILE-PREFLIGHT-CELL ,             0 ,
   TOP-HOOK-CELL ,                      0 ,
   ENGINE-SNAP-XT-CELL ,                0 ,
   EXIT-HOOK-CELL ,                     0 ,
   NCOMP-DISPATCH:XT-CELL ,              0 ,
   NCOMP-DISPATCH:DECL-CELL ,            1 ,
   NCOMP-DISPATCH:TARGET-DECL-CELL ,     1 ,
   APP-ENTRY:XT-CELL ,                  0 ,
   REPLH-CELL ,                         0 ,
   BPWBASE-CELL ,                       1 ,
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
   REFUSE ;

;package
