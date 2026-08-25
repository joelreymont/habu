\ object-index.f - checked source-to-object key index.
\
\ Load after lib/content-key.f and lib/fs-mutate.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/adt/option.f                  \ option<objidx:rec> for LOAD

package OBJIDX

64 constant KEY-U
4 constant SUFFIX-U
10 constant LF
46 constant DOT
65 constant UP-A
71 constant UP-G
97 constant LOW-A
103 constant LOW-G

create ROOT-BUF FS-PATH-CAP allot
create NAME-BUF 80 allot
create PATH-BUF FS-PATH-CAP allot
create REC-BUF 80 allot
create KEY-BUF 80 allot

variable ROOT-U
variable PATH-U

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
   105 NAME-BUF KEY-U 1 + + c!
   100 NAME-BUF KEY-U 2 + + c!
   120 NAME-BUF KEY-U 3 + + c! ;

: NAME! ( ptr u8 n -- ptr u8 n ) {: key:ptr keyu:n :}
   key keyu KEY-CHECK
   key NAME-BUF KEY-U BYTE-COPY
   SUFFIX!
   NAME-BUF KEY-U SUFFIX-U + ;

: PATH! ( ptr u8 n -- )
   ROOT-CHECK
   NAME! {: name:ptr nameu:n :}
   ROOT-BUF ROOT-U @ name nameu PATH-BUF JOIN-PATH PATH-U ! ;

: RECORD! ( ptr u8 n -- ) {: key:ptr keyu:n :}
   key keyu KEY-CHECK
   key REC-BUF KEY-U BYTE-COPY
   LF REC-BUF KEY-U + c! ;

: RECORD-CHECK ( n -- ) {: u:n :}
   u KEY-U 1 + <> if E-OBJ-FIELD throw then
   REC-BUF KEY-U + c@ LF <> if E-OBJ-FIELD throw then
   REC-BUF KEY-U KEY-CHECK ;

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

: SOURCE-KEY-HEX ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 -- )
   {: src:ptr srcu:n target:ptr targetu:n checker:ptr checkeru:n compiler:ptr compileru:n dst:ptr :}
   src srcu KEY-CHECK
   CONTENT-KEY:OPEN
   s" obj-source-index-v1" CONTENT-KEY:TEXT+
   src srcu CONTENT-KEY:TEXT+
   target targetu CONTENT-KEY:TEXT+
   checker checkeru CONTENT-KEY:TEXT+
   compiler compileru CONTENT-KEY:TEXT+
   dst CONTENT-KEY:FINAL-HEX ;

: EXISTS? ( ptr u8 n -- bool )
   PATH$ FILE? ;

: STORE ( ptr u8 n ptr u8 n -- ) {: skey:ptr skeyu:n okey:ptr okeyu:n :}
   skey skeyu PATH!
   okey okeyu RECORD!
   ROOT$ MAKE-DIRS
   PATH-BUF PATH-U @ REC-BUF KEY-U 1 + ATOMIC-WRITE-FILE ;

\ A loaded index record is the stored object-key bytes for a source key.
\ LOAD returns option<objidx:rec>: SOME(key-bytes) when the index file exists,
\ NONE when it is absent.
STRUCTURE rec 0
  FIELD ptr ptr u8
  FIELD len n
;STRUCTURE

: LOAD ( ptr u8 n -- option<objidx:rec> )   \ SOME object-key bytes when present, else NONE
   PATH!
   PATH-BUF PATH-U @ FILE? 0= if OPTION:NONE exit then
   PATH-BUF PATH-U @ REC-BUF KEY-U 1 + READ-ALL {: u:n :}
   u RECORD-CHECK
   REC-BUF KEY-BUF KEY-U BYTE-COPY
   KEY-BUF KEY-U OBJIDX-REC:MAKE OPTION:SOME ;

;package
