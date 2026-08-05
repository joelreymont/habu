\ intern.f - checked growable string interner for native lint tools.

require lib/errors.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f

package LINT-INTERN
private

public
76 constant E-CAP
private
s" E-LINT-INTERN-CAP" E-CAP LINT-CODE-NAME+
\ Shared by repository-scale lint tools; retain capacity for their largest
\ interned path and token sets plus growth headroom.
$800 constant MAX   \ crossed $400 on 2026-07-21 as FILEMAP grew past 1024 interned paths (structure-decl/make, field-proj, enum suites)
$1000 constant INTERN-CHUNK-MIN
$100 constant INTERN-FOLD-CAP
8 constant INTERN-VEC-CAP
2 constant INTERN-CHUNK-VEC-CAP

create ADDR-V VEC:HEADER-CELLS cells allot
create LEN-V VEC:HEADER-CELLS cells allot
create CHUNK-A-V VEC:HEADER-CELLS cells allot
create CHUNK-CAP-V VEC:HEADER-CELLS cells allot
create CHUNK-USED-V VEC:HEADER-CELLS cells allot
create INTERN-FOLD-BUF INTERN-FOLD-CAP allot
variable INTERN-READY
variable INTERN-CHUNK-I
variable N
variable CHUNK-N

\ Lift validated interner counts and indexes into VEC roles.
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
   INTERN-READY @ 5 = IF LINT-TRUE ELSE LINT-FALSE THEN ;

: INIT1 ( ptr h n n -- ) {: vec:ptr cap:n step:n :}
   step INTERN-READY @ < IF exit THEN
   vec cap INTERN>ITEM VEC:INIT
   step 1+ INTERN-READY ! ;

: INTERN-INIT-ONCE ( -- )
   INTERN-READY? IF exit THEN
   ADDR-V       INTERN-VEC-CAP       0 INIT1
   LEN-V        INTERN-VEC-CAP       1 INIT1
   CHUNK-A-V    INTERN-CHUNK-VEC-CAP 2 INIT1
   CHUNK-CAP-V  INTERN-CHUNK-VEC-CAP 3 INIT1
   CHUNK-USED-V INTERN-CHUNK-VEC-CAP 4 INIT1 ;

public
: COUNT ( -- n )
   INTERN-INIT-ONCE
   N @ ;
private

: INTERN-CHUNK# ( -- n )
   CHUNK-N @ ;

: INTERN-CHUNK-A@ ( n -- ptr u8 )
   CHUNK-A-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-CAP@ ( n -- n )
   CHUNK-CAP-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-USED@ ( n -- n )
   CHUNK-USED-V swap INTERN>INDEX VEC:@ ;

: INTERN-CHUNK-USED! ( n n -- ) {: used k :}
   used CHUNK-USED-V k INTERN>INDEX VEC:! ;

: INTERN-RESET-CHUNKS ( -- )
   0 begin dup INTERN-CHUNK# < while
      0 over INTERN-CHUNK-USED!
      1+
   repeat drop ;

public
: RESET ( -- )
   INTERN-INIT-ONCE
   ADDR-V VEC:CLEAR
   LEN-V VEC:CLEAR
   0 N !
   0 INTERN-CHUNK-I !
   INTERN-RESET-CHUNKS ;
private

public
: TEXT ( n -- ptr u8 n ) {: id:n :}
   INTERN-INIT-ONCE
   id 0 < IF E-CAP throw THEN
   id COUNT >= IF E-CAP throw THEN
   ADDR-V id INTERN>INDEX VEC:@
   LEN-V id INTERN>INDEX VEC:@ ;

: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   INTERN-INIT-ONCE
   0 begin dup COUNT < while
      dup TEXT a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: HAS? ( ptr u8 n -- bool )
   FIND 0 >= ;
private

: INTERN-CHUNK-SIZE ( n -- n )
   dup INTERN-CHUNK-MIN < IF drop INTERN-CHUNK-MIN THEN ;

: RESERVE1 ( ptr h n -- )  INTERN>ITEM VEC:ENSURE ;

: RESERVE-CHUNK ( n -- ) {: need:n :}
   CHUNK-A-V    need RESERVE1
   CHUNK-CAP-V  need RESERVE1
   CHUNK-USED-V need RESERVE1 ;

: INTERN-STORE-CHUNK ( ptr u8 n -- ) {: a:ptr cap :}
   a CHUNK-A-V VEC:PUSH drop
   cap CHUNK-CAP-V VEC:PUSH drop
   0 CHUNK-USED-V VEC:PUSH drop
   CHUNK-N @ 1+ CHUNK-N ! ;

: INTERN-ADD-CHUNK ( n -- )
   INTERN-CHUNK-SIZE {: cap:n :}
   CHUNK-N @ 1+ RESERVE-CHUNK
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
   u 0 < IF E-CAP throw THEN
   u INTERN-ENSURE-CHUNK
   u INTERN-CHUNK-I @ INTERN-ALLOC-IN-CHUNK ;

: INTERN-COPY-TO ( ptr u8 n ptr u8 -- ptr u8 ) {: a:ptr u dst:ptr :}
   a dst u LINT-BMOVE
   dst ;

: INTERN-COPY$ ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u u INTERN-ALLOC-SPAN INTERN-COPY-TO ;

: INTERN-CHECK-NEW-ID ( -- n )
   COUNT dup MAX >= IF drop E-CAP throw THEN ;

: RESERVE-ENTRY ( n -- ) {: need:n :}
   ADDR-V need RESERVE1
   LEN-V  need RESERVE1 ;

: STORE-NEW ( ptr u8 n n -- n ) {: a:ptr u:n id:n :}
   id 1+ RESERVE-ENTRY
   a u INTERN-COPY$ ADDR-V VEC:PUSH drop
   u LEN-V VEC:PUSH drop
   N @ 1+ N !
   id ;

public
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIND dup 0 >= IF exit THEN drop
   a u INTERN-CHECK-NEW-ID STORE-NEW ;

: ADD-FOLD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u INTERN-FOLD-CAP > IF E-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u ADD ;

: HAS-FOLD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u INTERN-FOLD-CAP > IF E-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u HAS? ;

;package
