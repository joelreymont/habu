\ intern.f - checked growable string interner for native lint tools.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/vector.f
require tools/lint/text.f
require src/core/exit-status.f

package LINT-INTERN

\ The set has no entry ceiling: ids live in growable vectors and their bytes in
\ chunks allocated on demand, so a repository-scale input is bounded by memory
\ alone and a failed chunk allocation reports as a memory error. The bound
\ below is a real resource: the chunk granularity.
$1000 constant INTERN-CHUNK-MIN

\ NULL-PTR until INTERN-INIT-ONCE makes them; every public word runs it first.
TYPED-VARIABLE INTERN-ADDR-V ^VEC:vector<^u8>
TYPED-VARIABLE INTERN-LEN-V ^VEC:vector<n>
TYPED-VARIABLE INTERN-CHUNK-A-V ^VEC:vector<^u8>
TYPED-VARIABLE INTERN-CHUNK-CAP-V ^VEC:vector<n>
TYPED-VARIABLE INTERN-CHUNK-USED-V ^VEC:vector<n>
variable INTERN-READY
variable INTERN-CHUNK-I

: INTERN-READY? ( -- bool )
   INTERN-READY @ 0 = IF LINT-FALSE ELSE LINT-TRUE THEN ;

: INTERN-INIT-ONCE ( -- )
   INTERN-READY? IF exit THEN
   HEAP:ALLOCATOR 0 [type] ^u8 VEC:NEW INTERN-ADDR-V !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW INTERN-LEN-V !
   HEAP:ALLOCATOR 0 [type] ^u8 VEC:NEW INTERN-CHUNK-A-V !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW INTERN-CHUNK-CAP-V !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW INTERN-CHUNK-USED-V !
   1 INTERN-READY ! ;

: INTERN# ( -- n )
   INTERN-INIT-ONCE
   INTERN-ADDR-V @ VEC:COUNT ;

: INTERN-CHUNK# ( -- n )
   INTERN-CHUNK-A-V @ VEC:COUNT ;

: INTERN-CHUNK-A@ ( n -- ^u8 )
   INTERN-CHUNK-A-V @ swap VEC:AT @ ;

: INTERN-CHUNK-CAP@ ( n -- n )
   INTERN-CHUNK-CAP-V @ swap VEC:AT @ ;

: INTERN-CHUNK-USED@ ( n -- n )
   INTERN-CHUNK-USED-V @ swap VEC:AT @ ;

: INTERN-CHUNK-USED! ( n n -- ) {: used k :}
   used INTERN-CHUNK-USED-V @ k VEC:AT ! ;

: INTERN-RESET-CHUNKS ( -- )
   0 begin dup INTERN-CHUNK# < while
      0 over INTERN-CHUNK-USED!
      1+
   repeat drop ;

public

: INTERN-RESET ( -- )
   INTERN-INIT-ONCE
   INTERN-ADDR-V @ 0 VEC:RESIZE
   INTERN-LEN-V @ 0 VEC:RESIZE
   0 INTERN-CHUNK-I !
   INTERN-RESET-CHUNKS ;

: INTERN$ ( n -- ^u8 n ) {: id :}
   INTERN-INIT-ONCE
   id 0 < IF EXIT-STATUS:FULL throw THEN
   id INTERN# >= IF EXIT-STATUS:FULL throw THEN
   INTERN-ADDR-V @ id VEC:AT @
   INTERN-LEN-V @ id VEC:AT @ ;

: INTERN-FIND ( ^u8 n -- n ) {: a:^ u :}
   INTERN-INIT-ONCE
   0 begin dup INTERN# < while
      dup INTERN$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

private

: INTERN-CHUNK-SIZE ( n -- n )
   dup INTERN-CHUNK-MIN < IF drop INTERN-CHUNK-MIN THEN ;

: INTERN-STORE-CHUNK ( ^u8 n -- ) {: a:^ cap :}
   a INTERN-CHUNK-A-V @ VEC:PUSH !
   cap INTERN-CHUNK-CAP-V @ VEC:PUSH !
   0 INTERN-CHUNK-USED-V @ VEC:PUSH ! ;

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

: INTERN-ALLOC-IN-CHUNK ( n n -- ^u8 ) {: u k :}
   k INTERN-CHUNK-A@ k INTERN-CHUNK-USED@ +
   k INTERN-CHUNK-USED@ u + k INTERN-CHUNK-USED! ;

: INTERN-ALLOC-SPAN ( n -- ^u8 ) {: u :}
   u 0 < IF EXIT-STATUS:FULL throw THEN
   u INTERN-ENSURE-CHUNK
   u INTERN-CHUNK-I @ INTERN-ALLOC-IN-CHUNK ;

: INTERN-COPY-TO ( ^u8 n ^u8 -- ^u8 ) {: a:^ u dst:^ :}
   a dst u LINT-BMOVE
   dst ;

: INTERN-COPY$ ( ^u8 n -- ^u8 ) {: a:^ u :}
   a u u INTERN-ALLOC-SPAN INTERN-COPY-TO ;

: INTERN-STORE-NEW ( ^u8 n n -- n ) {: a:^ u id :}
   a u INTERN-COPY$ INTERN-ADDR-V @ VEC:PUSH !
   u INTERN-LEN-V @ VEC:PUSH !
   id ;

public

: INTERN ( ^u8 n -- n ) {: a:^ u :}
   a u INTERN-FIND dup 0 >= IF exit THEN drop
   a u INTERN# INTERN-STORE-NEW ;

;package
