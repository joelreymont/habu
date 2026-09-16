\ pointer-storage.f - pointer-valued global storage definer.

: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

\ Typed reset code needs a null value without pretending numeric zero inhabits
\ every pointer family. A reserved DATA header cell, not a `create`d body whose
\ absolute address chain the stripped AOT linker refuses; src/habu/layout.f
\ NULL-PTR-CELL-OFF reserves the offset, says why, and internal-word-gate.f
\ refuses a drift from this copy. Keep this file inside pointer-storage-test.f's
\ SOURCE-CAP: it reads the whole file in.
$3800 constant NULL-PTR-OFF
: NULL-PTR-CELL ( -- ptr n )
   data-base NULL-PTR-OFF + ;
REG-PROTECT
: NULL-PTR ( -- ptr a )
   NULL-PTR-CELL 0 ptr-field @ ;

\ Persistence is explicit: scratch pointer slots use PTR-VARIABLE and never join
\ the relocation table; only a slot whose value must survive an image/capture uses
\ this definer.
: PERSISTED-PTR-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) 0 ptr-field ;
