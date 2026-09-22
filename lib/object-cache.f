\ object-cache.f - checked content-addressed object file store.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object.f

package OBJSTORE
using OBJ

64 constant KEY-U
4 constant SUFFIX-U
46 constant DOT
65 constant UP-A
71 constant UP-G
97 constant LOW-A
103 constant LOW-G

create ROOT-BUF FS-PATH-CAP allot
create NAME-BUF 80 allot
create PATH-BUF FS-PATH-CAP allot
create KEY-BUF 80 allot

variable ROOT-U
variable PATH-U
DYNAMIC-BUFFER READ-STORAGE n

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: HEX? ( n -- bool ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if TRUE exit then
   c UP-A >= c UP-G < and if TRUE exit then
   c LOW-A >= c LOW-G < and ;

: KEY-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u KEY-U <> if E-OBJ-FIELD throw then
   0 begin dup u < while
      dup a + c@ HEX? 0= if E-OBJ-FIELD throw then
      1+
   repeat drop ;

: ROOT-CHECK ( -- )
   ROOT-U @ 0 <= if E-FS-PATH throw then ;

: SUFFIX! ( -- )
   DOT NAME-BUF KEY-U + c!
   104 NAME-BUF KEY-U 1 + + c!
   98 NAME-BUF KEY-U 2 + + c!
   111 NAME-BUF KEY-U 3 + + c! ;

: NAME! ( ptr u8 n -- ptr u8 n ) {: key:ptr keyu:n :}
   key keyu KEY-CHECK
   key NAME-BUF KEY-U BYTE-COPY
   SUFFIX!
   NAME-BUF KEY-U SUFFIX-U + ;

: PATH! ( ptr u8 n -- )
   ROOT-CHECK
   NAME! {: name:ptr nameu:n :}
   ROOT-BUF ROOT-U @ name nameu PATH-BUF JOIN-PATH PATH-U ! ;

: READ-ROOM ( n -- ) {: bytes:n :}
   bytes 0 < bytes MAX-BYTES > or if E-OBJ-CAPACITY throw then
   bytes CELL / bytes CELL mod 0 > if 1+ then
   1 max READ-STORAGE-RESERVE ;

: READ-BUF ( -- ptr u8 )
   0 READ-STORAGE byte-view ;

public

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-CHECK
   ROOT-BUF ROOT-U @ ;

: PATH$ ( ptr u8 n -- ptr u8 n )
   PATH!
   PATH-BUF PATH-U @ ;

: EXISTS? ( ptr u8 n -- bool )
   PATH$ FILE? ;

: STORE ( -- ptr u8 n )
   BYTES$ {: obj:ptr obju:n :}
   KEY-BUF KEY-HEX
   ROOT$ MAKE-DIRS
   KEY-BUF KEY-U PATH!
   PATH-BUF PATH-U @ obj obju ATOMIC-WRITE-FILE
   KEY-BUF KEY-U ;

: LOAD ( ptr u8 n -- ) {: key:ptr keyu:n :}
   key keyu PATH$ 2dup FILE? 0= if 2drop E-FS-OPEN throw then
   FILE-SIZE {: size:n :}
   size READ-ROOM
   PATH-BUF PATH-U @ READ-BUF size READ-ALL {: u:n :}
   READ-BUF u OBJ:LOAD
   KEY-BUF KEY-HEX
   \ PATH! preserved the requested key; the caller may hold STORE's KEY-BUF.
   KEY-BUF KEY-U NAME-BUF KEY-U STR= 0= if E-OBJ-SCHEMA throw then ;

;using
;package
