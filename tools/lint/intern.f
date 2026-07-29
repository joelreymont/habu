\ intern.f - checked growable string interner for native lint tools.

require lib/errors.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f

76 constant E-LINT-INTERN-CAP
s" E-LINT-INTERN-CAP" E-LINT-INTERN-CAP LINT-CODE-NAME+
\ Shared by repository-scale lint tools; retain capacity for their largest
\ interned path and token sets plus growth headroom.
$800 constant INTERN-MAX   \ crossed $400 on 2026-07-21 as FILEMAP grew past 1024 interned paths (structure-decl/make, field-proj, enum suites)
$1000 constant INTERN-CHUNK-MIN
$100 constant INTERN-FOLD-CAP
8 constant INTERN-VEC-CAP
2 constant INTERN-CHUNK-VEC-CAP

create INTERN-ADDR-V VEC-HEADER-CELLS cells allot
create INTERN-LEN-V VEC-HEADER-CELLS cells allot
create INTERN-CHUNK-A-V VEC-HEADER-CELLS cells allot
create INTERN-CHUNK-CAP-V VEC-HEADER-CELLS cells allot
create INTERN-CHUNK-USED-V VEC-HEADER-CELLS cells allot
create INTERN-FOLD-BUF INTERN-FOLD-CAP allot
variable INTERN-READY
variable INTERN-CHUNK-I

\ ---- raw table cell -> CAD-NUM role bridges for the typed VEC surface ---------
\ The interner's parallel vectors store raw cells (string / chunk addresses,
\ lengths, caps, used counts). The typed VEC surface (package VEC) reads a
\ validated CAD-NUM role - a capacity is a `CAD-NUM:item-count`, an entry position
\ is a `CAD-NUM:index` - so a count/index role swap at a VEC call is a checker
\ reject. These lift a nonnegative cell to its role through the PUBLIC CAD-NUM
\ validators (no laundering back to n, no reopened package); the refusal arms are
\ unreachable invariants (a boot capacity and a bounds-checked id are nonnegative),
\ an impossible negative surfaces the vector's own capacity / bounds code. This is
\ the maki/sched-key.f SK>ITEM / SK>INDEX idiom, kept intern-local.
: INTERN>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: INTERN>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: INTERN-READY? ( -- bool )
   INTERN-READY @ 0 = IF LINT-FALSE ELSE LINT-TRUE THEN ;

: INTERN-INIT-ONCE ( -- )
   INTERN-READY? IF exit THEN
   INTERN-ADDR-V INTERN-VEC-CAP INTERN>ITEM VEC:INIT
   INTERN-LEN-V INTERN-VEC-CAP INTERN>ITEM VEC:INIT
   INTERN-CHUNK-A-V INTERN-CHUNK-VEC-CAP INTERN>ITEM VEC:INIT
   INTERN-CHUNK-CAP-V INTERN-CHUNK-VEC-CAP INTERN>ITEM VEC:INIT
   INTERN-CHUNK-USED-V INTERN-CHUNK-VEC-CAP INTERN>ITEM VEC:INIT
   1 INTERN-READY ! ;

\ live interned count. RAW residual (maki/sched-key.f SK-N precedent): VEC:LEN@
\ yields a CAD-NUM:item-count and the checker correctly refuses to launder a count
\ back to n, but INTERN# is pinned to a raw n that drives the INTERN$/INTERN-FIND
\ bounds and the INTERN-CHECK-NEW-ID compare, so the count is read through the raw
\ VEC-LEN@ accessor for this word alone (no new projection). A typed
\ item-count-bounded VEC iterator would retire it.
: INTERN# ( -- n )
   INTERN-INIT-ONCE
   INTERN-ADDR-V VEC-LEN@ LEN>N ;

\ live chunk count. RAW residual (same pin as INTERN#): drives the raw begin/while
\ bounds in INTERN-RESET-CHUNKS / INTERN-ENSURE-CHUNK, so it stays on VEC-LEN@.
: INTERN-CHUNK# ( -- n )
   INTERN-CHUNK-A-V VEC-LEN@ LEN>N ;

: INTERN-CHUNK-A@ ( n -- ptr u8 )
   INTERN-CHUNK-A-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-CAP@ ( n -- n )
   INTERN-CHUNK-CAP-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-USED@ ( n -- n )
   INTERN-CHUNK-USED-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-USED! ( n n -- ) {: used k :}
   used INTERN-CHUNK-USED-V k INTERN>INDEX VEC:! ;

: INTERN-RESET-CHUNKS ( -- )
   0 begin dup INTERN-CHUNK# < while
      0 over INTERN-CHUNK-USED!
      1+
   repeat drop ;

: INTERN-RESET ( -- )
   INTERN-INIT-ONCE
   INTERN-ADDR-V VEC:CLEAR
   INTERN-LEN-V VEC:CLEAR
   0 INTERN-CHUNK-I !
   INTERN-RESET-CHUNKS ;

: INTERN-INIT ( -- )
   INTERN-RESET ;

: INTERN$ ( n -- ptr u8 n ) {: id :}
   INTERN-INIT-ONCE
   id 0 < IF E-LINT-INTERN-CAP throw THEN
   id INTERN# >= IF E-LINT-INTERN-CAP throw THEN
   INTERN-ADDR-V id INTERN>INDEX VEC:@
   INTERN-LEN-V id INTERN>INDEX VEC:@ ;

: INTERN-FIND ( ptr u8 n -- n ) {: a:ptr u :}
   INTERN-INIT-ONCE
   0 begin dup INTERN# < while
      dup INTERN$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: INTERN? ( ptr u8 n -- bool )
   INTERN-FIND 0 >= ;

: INTERN-CHUNK-SIZE ( n -- n )
   dup INTERN-CHUNK-MIN < IF drop INTERN-CHUNK-MIN THEN ;

: INTERN-STORE-CHUNK ( ptr u8 n -- ) {: a:ptr cap :}
   a INTERN-CHUNK-A-V VEC:PUSH drop
   cap INTERN-CHUNK-CAP-V VEC:PUSH drop
   0 INTERN-CHUNK-USED-V VEC:PUSH drop ;

: INTERN-ADD-CHUNK ( n -- )
   INTERN-CHUNK-SIZE {: cap:n :}
   cap MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop cap INTERN-STORE-CHUNK ;

: INTERN-CHUNK-FREE ( n -- n ) {: k :}
   k INTERN-CHUNK-CAP@ k INTERN-CHUNK-USED@ - ;

: INTERN-ADVANCE-CHUNK ( -- )
   INTERN-CHUNK-I @ 1+ dup INTERN-CHUNK# < IF
      INTERN-CHUNK-I !
   ELSE
      drop INTERN-CHUNK# INTERN-CHUNK-I !
   THEN ;

: INTERN-ENSURE-CHUNK ( n -- ) {: need :}
   INTERN-CHUNK# 0= IF need INTERN-ADD-CHUNK THEN
   begin need INTERN-CHUNK-I @ INTERN-CHUNK-FREE > while
      INTERN-ADVANCE-CHUNK
      INTERN-CHUNK-I @ INTERN-CHUNK# >= IF need INTERN-ADD-CHUNK THEN
   repeat ;

: INTERN-ALLOC-IN-CHUNK ( n n -- ptr u8 ) {: u k :}
   k INTERN-CHUNK-A@ k INTERN-CHUNK-USED@ +
   k INTERN-CHUNK-USED@ u + k INTERN-CHUNK-USED! ;

: INTERN-ALLOC-SPAN ( n -- ptr u8 ) {: u :}
   u 0 < IF E-LINT-INTERN-CAP throw THEN
   u INTERN-ENSURE-CHUNK
   u INTERN-CHUNK-I @ INTERN-ALLOC-IN-CHUNK ;

: INTERN-COPY-TO ( ptr u8 n ptr u8 -- ptr u8 ) {: a:ptr u dst:ptr :}
   a dst u LINT-BMOVE
   dst ;

: INTERN-COPY$ ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u u INTERN-ALLOC-SPAN INTERN-COPY-TO ;

: INTERN-CHECK-NEW-ID ( -- n )
   INTERN# dup INTERN-MAX >= IF drop E-LINT-INTERN-CAP throw THEN ;

: INTERN-STORE-NEW ( ptr u8 n n -- n ) {: a:ptr u id :}
   a u INTERN-COPY$ INTERN-ADDR-V VEC:PUSH drop
   u INTERN-LEN-V VEC:PUSH drop
   id ;

: INTERN ( ptr u8 n -- n ) {: a:ptr u :}
   a u INTERN-FIND dup 0 >= IF exit THEN drop
   a u INTERN-CHECK-NEW-ID INTERN-STORE-NEW ;

: INTERN-FOLD ( ptr u8 n -- n ) {: a:ptr u :}
   u INTERN-FOLD-CAP > IF E-LINT-INTERN-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u INTERN ;

: INTERN-FOLD? ( ptr u8 n -- bool ) {: a:ptr u :}
   u INTERN-FOLD-CAP > IF E-LINT-INTERN-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u INTERN? ;
