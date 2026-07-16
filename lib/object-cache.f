\ object-cache.f - checked content-addressed object file store.
\
\ Load after lib/object.f and lib/fs-mutate.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-atomic.f
require lib/content-key.f
require lib/object.f

package OBJSTORE

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
create ATOMIC-CTX FS-ATOMIC:CONTEXT-CELLS cells allot

variable ROOT-U
variable PATH-U
variable READ-BUF-A

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

: READ-BUF-FIELD ( -- ptr ptr u8 )
   READ-BUF-A 0 ptr-field ;

: READ-BUF@ ( -- ptr u8 )
   READ-BUF-FIELD @ ;

: READ-BUF! ( ptr u8 -- )
   READ-BUF-FIELD ! ;

\ OBJ:MAX-BYTES is a positive library constant: MEM:BYTES-ALLOC-LEN narrows the raw
\ size to the validated alloc role before MEM:ALLOC-BYTES, throwing E-MEM-SIZE on
\ any refusal (unreachable for the constant).
: READ-BUF ( -- ptr u8 )
   READ-BUF@ 0= if
      OBJ:MAX-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop READ-BUF!
   then
   READ-BUF@ ;

: REQUIRE-ATOMIC ( FS-ATOMIC:result -- )
   MATCH FS-ATOMIC:result
      committed OF ENDOF
      committed-unsynced OF 2drop drop E-FS-IO throw ENDOF
      committed-close-failed OF 2drop E-FS-IO throw ENDOF
      aborted OF 2drop 2drop throw ENDOF
   ;MATCH ;

\ typed-local-lint: allow-bare-local - path and src preserve ptr u8 through the transaction.
: ATOMIC-WRITE ( ptr u8 n ptr u8 n -- ) {: path pathu:n src srcu:n :}
   ATOMIC-CTX FS-ATOMIC:CONTEXT-INIT
   ATOMIC-CTX path pathu src srcu FS-ATOMIC:TRY-WRITE-FILE REQUIRE-ATOMIC ;

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
   OBJ:BYTES$ {: obj:ptr obju:n :}
   KEY-BUF OBJ:KEY-HEX
   ROOT$ MAKE-DIRS
   KEY-BUF KEY-U PATH!
   PATH-BUF PATH-U @ obj obju ATOMIC-WRITE
   KEY-BUF KEY-U ;

: LOAD ( ptr u8 n -- ) {: key:ptr keyu:n :}
   key keyu PATH$
   READ-BUF OBJ:MAX-BYTES READ-ALL {: u:n :}
   READ-BUF u OBJ:LOAD
   KEY-BUF OBJ:KEY-HEX
   KEY-BUF KEY-U key keyu STR= 0= if E-OBJ-SCHEMA throw then ;

;package
