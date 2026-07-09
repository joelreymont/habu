\ intern.f - checked growable string interner for native lint tools.

require lib/errors.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f

76 constant E-LINT-INTERN-CAP
s" E-LINT-INTERN-CAP" E-LINT-INTERN-CAP LINT-CODE-NAME+
\ Sized by the largest interned set: filemap-lint interns every FILEMAP.md
\ path (513 as of 2026-07, past the old $200); keep growth headroom.
$400 constant INTERN-MAX
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

: INTERN-READY? ( -- bool )
   INTERN-READY @ 0 = IF LINT-FALSE ELSE LINT-TRUE THEN ;

: INTERN-INIT-ONCE ( -- )
   INTERN-READY? IF exit THEN
   INTERN-ADDR-V INTERN-VEC-CAP >COUNT VEC-INIT
   INTERN-LEN-V INTERN-VEC-CAP >COUNT VEC-INIT
   INTERN-CHUNK-A-V INTERN-CHUNK-VEC-CAP >COUNT VEC-INIT
   INTERN-CHUNK-CAP-V INTERN-CHUNK-VEC-CAP >COUNT VEC-INIT
   INTERN-CHUNK-USED-V INTERN-CHUNK-VEC-CAP >COUNT VEC-INIT
   1 INTERN-READY ! ;

: INTERN# ( -- n )
   INTERN-INIT-ONCE
   INTERN-ADDR-V VEC-LEN@ LEN>N ;

: INTERN-CHUNK# ( -- n )
   INTERN-CHUNK-A-V VEC-LEN@ LEN>N ;

: INTERN-CHUNK-A@ ( n -- ptr u8 )
   >IDX INTERN-CHUNK-A-V swap VEC-A@ ;

: INTERN-CHUNK-CAP@ ( n -- n )
   >IDX INTERN-CHUNK-CAP-V swap VEC-N@ ;

: INTERN-CHUNK-USED@ ( n -- n )
   >IDX INTERN-CHUNK-USED-V swap VEC-N@ ;

: INTERN-CHUNK-USED! ( n n -- ) {: used k :}
   used INTERN-CHUNK-USED-V k >IDX VEC-N! ;

: INTERN-RESET-CHUNKS ( -- )
   0 begin dup INTERN-CHUNK# < while
      0 over INTERN-CHUNK-USED!
      1+
   repeat drop ;

: INTERN-RESET ( -- )
   INTERN-INIT-ONCE
   INTERN-ADDR-V VEC-CLEAR
   INTERN-LEN-V VEC-CLEAR
   0 INTERN-CHUNK-I !
   INTERN-RESET-CHUNKS ;

: INTERN-INIT ( -- )
   INTERN-RESET ;

: INTERN$ ( n -- ptr u8 n ) {: id :}
   INTERN-INIT-ONCE
   id 0 < IF E-LINT-INTERN-CAP throw THEN
   id INTERN# >= IF E-LINT-INTERN-CAP throw THEN
   INTERN-ADDR-V id >IDX VEC-A@
   INTERN-LEN-V id >IDX VEC-N@ ;

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
   a INTERN-CHUNK-A-V VEC-PUSH-A drop
   cap INTERN-CHUNK-CAP-V VEC-PUSH-N drop
   0 INTERN-CHUNK-USED-V VEC-PUSH-N drop ;

: INTERN-ADD-CHUNK ( n -- )
   INTERN-CHUNK-SIZE MEM-ALLOC-BYTES INTERN-STORE-CHUNK ;

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
   a u INTERN-COPY$ INTERN-ADDR-V VEC-PUSH-A drop
   u INTERN-LEN-V VEC-PUSH-N drop
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
