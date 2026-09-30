\ object-resolve.f - checked source+ABI resolver over OBJIDX and OBJSTORE.
\
\ The store is a build-cache family (lib/build-cache.f): objects <key>.hbo and
\ index records <key>.idx under the root ROOT! names. STORE prunes both after
\ it publishes, and LOAD dates the index record and the object it names as used
\ before reading either. The two age apart - one record's object can be another
\ source's too, and a prune can take one and not the other - so a source whose
\ index record or object is gone is a miss, and the build that follows stores
\ both again.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f
require lib/build-cache.f

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

\ Prune a family that has no work directories around the entry just stored.
: PRUNE-FAMILY ( ptr u8 n ptr u8 n -- ) {: suffix:ptr suffixu:n kept:ptr keptu:n :}
   s" " suffix suffixu s" " kept keptu BUILD-CACHE:PRUNE ;

\ The object an index record names, dated as used, loaded and its headers
\ checked. FALSE when the object is gone, and a load that fails while it is gone
\ is a miss too: a prune can take the object after USED dated it.
: LOAD-OBJECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n :}
   key keyu OBJSTORE:PATH$ BUILD-CACHE:USED 0= if FALSE exit then
   key keyu [: 2dup OBJSTORE:LOAD ;] catch {: code:n :} 2drop
   code 0<> if
      key keyu OBJSTORE:PATH$ FS-TRY-LSTAT if code throw then
      FALSE exit
   then
   src srcu target targetu checker checkeru compiler compileru CHECK-HEADERS
   TRUE ;

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
   OBJSTORE:SUFFIX$ obj obju OBJSTORE:PATH$ PRUNE-FAMILY
   OBJIDX:SUFFIX$ SRC-KEY KEY-U OBJIDX:PATH$ PRUNE-FAMILY
   obj obju ;

: LOAD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n :}
   src srcu target targetu checker checkeru compiler compileru SOURCE-KEY!
   SRC-KEY KEY-U OBJIDX:PATH$ BUILD-CACHE:USED 0= if FALSE exit then
   SRC-KEY KEY-U OBJIDX:LOAD MATCH option
     none OF FALSE ENDOF
     some OF OBJIDX-REC:UNMAKE
        src srcu target targetu checker checkeru compiler compileru LOAD-OBJECT ENDOF
   ;MATCH ;

;package
