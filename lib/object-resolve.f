\ object-resolve.f - checked source+ABI resolver over OBJIDX and OBJSTORE.
\
\ Load after lib/object-index.f and lib/object-cache.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f

package OBJRES

64 constant KEY-U

create SRC-KEY 80 allot

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: SOURCE-KEY! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n :}
   src srcu target targetu checker checkeru compiler compileru SRC-KEY OBJIDX:SOURCE-KEY-HEX ;

: CHECK-HEADER ( ptr u8 n ptr u8 n -- )
   STR= 0= if E-OBJ-SCHEMA throw then ;

: CHECK-HEADERS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n :}
   OBJ:SOURCE$ src srcu CHECK-HEADER
   OBJ:TARGET$ target targetu CHECK-HEADER
   OBJ:CHECKER$ checker checkeru CHECK-HEADER
   OBJ:COMPILER$ compiler compileru CHECK-HEADER ;

public

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u OBJSTORE:ROOT!
   a u OBJIDX:ROOT! ;

: ROOT$ ( -- ptr u8 n )
   OBJSTORE:ROOT$ ;

: STORE ( -- ptr u8 n )
   OBJ:SOURCE$ {: src:ptr srcu:n :}
   OBJ:TARGET$ {: target:ptr targetu:n :}
   OBJ:CHECKER$ {: checker:ptr checkeru:n :}
   OBJ:COMPILER$ {: compiler:ptr compileru:n :}
   OBJSTORE:STORE {: obj:ptr obju:n :}
   src srcu target targetu checker checkeru compiler compileru SOURCE-KEY!
   SRC-KEY KEY-U obj obju OBJIDX:STORE
   obj obju ;

: LOAD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n :}
   src srcu target targetu checker checkeru compiler compileru SOURCE-KEY!
   SRC-KEY KEY-U OBJIDX:LOAD {: obj:ptr obju:n hit:bool :}
   hit 0= if FALSE exit then
   obj obju OBJSTORE:LOAD
   src srcu target targetu checker checkeru compiler compileru CHECK-HEADERS
   TRUE ;

;package
