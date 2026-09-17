\ pointer-storage.f - pointer-valued global storage definer.

\ THE CLAUSES ARE EMPTY ON PURPOSE. `create` already leaves a word that pushes
\ its own data address, and all a pointer definer adds is the type to read that
\ address at: `0 ptr-field` was the identity on it, so spelling it cost every
\ read a call, a branch and a frame. An empty clause declares the effect and
\ nothing else, and the engine answers by leaving the word the body and the
\ DKIND:ADDR stamp `create` gave it (habu2.f DOES-REC:ELIDE-EMPTY), so a read
\ costs the one load a bare cell costs. test/does-empty-clause.f pins it.
: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) ;

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
   create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) ;
