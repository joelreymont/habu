\ aot-runtime.f - identify a complete captured runtime from its records.
\ Both native writers use this query after importing a capture; a partial
\ image may contain only a REPL or a fragment of the core prefix.
require src/habu/aot-decl.f
using AOT-BUF

package AOT-RUNTIME
private

: BAD ( -- ) s" hb: incomplete captured runtime" 74 die ;

: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;

: REC ( n -- ptr u8 )
   AOT-CREC-ROW * AOT-REC-MAX 48 * + AOT-REC-BUF@ + ;

: NAME= ( ptr u8 ptr u8 n -- bool ) {: rec:ptr a:ptr u:n :}
   rec 8 + W32@ {: off:n :}
   off AOT-NAMES-LEN @ >= if BAD then
   AOT-NAMES-BUF@ off + {: name:ptr :}
   name c@ {: len:n :}
   len AOT-NAMES-LEN @ off - 1- > if BAD then
   name 1+ len a u CORE-STR=CI ;

: FIND ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   AOT-REC-N @ 0 ?do
      i REC {: rec:ptr :}
      rec 16 + W32@ wid = if
         rec a u NAME= if i unloop exit then
      then
   loop
   -1 ;

: MEMBER? ( ptr u8 n n -- bool ) {: a:ptr u:n pkg:n :}
   pkg 0 < if 0 0= 0= exit then
   pkg REC {: rec:ptr :}
   a u rec W32@ FIND 0 >= if 0 0= exit then
   rec 4 + W32@ {: wid:n :}
   wid 0= if 0 0= 0= exit then
   a u wid FIND 0 >= ;

public

: COMPLETE? ( -- bool )
   AOT-REC-N @ dup 0 < swap AOT-REC-MAX > or if BAD then
   AOT-REC-N @ 0= if 0 0= 0= exit then
   s" CHECKER-REG" $FFFFFFFF FIND {: owner:n :}
   s" PREFIX-MARK" $FFFFFFFF FIND {: mark:n :}
   s" NATIVE-RUNTIME" $FFFFFFFF FIND {: runtime:n :}
   owner 0 < mark 0 < and runtime 0 < and if 0 0= 0= exit then
   owner 0 < if BAD then
   s" CAPTURE-PREPARE" runtime MEMBER? 0= if BAD then
   s" CURSORS" mark MEMBER? 0= if BAD then
   0 0= ;

;package
;using
