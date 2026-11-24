split-str
\ test-fsm.f - an FSM:DEFINE table in a source file as the text of its TLA+
\ model's Table module, for tests only; nothing in a program calls it. A test
\ compares TABLE-MODULE's text with the model's <Model>Table.tla, so the table
\ and the model the model checker reads stay one. TABLE-MODULE fails the
\ running test when the file holds no such table, and for a PICK line, whose
\ next state no tuple can hold.
\ STORAGE CLASS. CALLER-OWNED: the file and the module text lie in the arena
\ the caller passes. It builds lines in the shared string builder and reads
\ the table with FSM:LINES, so it runs on the main thread.

require src/lib/adt/option.f
require src/lib/arena.f
require src/lib/span.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fs.f
require src/lib/fsm.f
require src/lib/test.f
require src/core/bytes.f

package TEST-FSM

\ A text being rendered: each ADD lays a longer copy and keeps its span here.
ARENA RENDERED SPAN:span<u8>

\ The bytes laid after the text at h, in the arena.
: ADD ( ^SPAN:span<u8> str ^ARENA:arena -- )
   {: h a:^ u:n ar :}
   h @ SPAN:$ {: s:^ su:n :}
   ar su u + ARENA:ALLOC drop {: b:^ :}
   s b su BYTE-COPY
   a b su + u BYTE-COPY
   b su u + SPAN:MAKE h ! ;

\ The file at the path, whole, in the arena.
: FILE$ ( str ^ARENA:arena -- str )
   {: p:^ pu:n ar :}
   p pu FS:FILE-SIZE {: u:n :}
   ar u ARENA:ALLOC drop {: b:^ :}
   b  p pu b u FS:READ-ALL ;

\ The first offset at or after from of k in t, else -1.
: FIND-AT ( n str str -- n )
   {: from:n t:^ tu:n k:^ ku:n :}
   t tu k ku from STR:FIND-SUB-FROM MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Whether ku bytes at offset k open a line of t and end at a
\ byte <= 32 or at t's end.
: WHOLE? ( n str n -- bool )
   {: k:n t:^ tu:n ku:n :}
   k ku + {: e:n :}
   k 0= if true else t k + 1- c@ 10 = then
   e tu = if true else t e + c@ 33 < then and ;

\ The offset of the first whole k in t, else -1.
: KEY-LINE ( str str -- n )
   {: t:^ tu:n k:^ ku:n :}
   0 begin  t tu k ku FIND-AT  dup 0< 0= while
      dup t tu ku WHOLE? if exit then
      1+
   repeat ;

\ The text from n where a line opens FSM:DEFINE n, n whole; else none.
: TABLE-AT ( str str -- str )
   {: t:^ tu:n n:^ nu:n :}
   SB-RESET s" FSM:DEFINE " SB-APPEND n nu SB-APPEND
   t tu SB$ KEY-LINE {: k:n :}
   k 0< if t 0 exit then
   k s" FSM:DEFINE " nip + {: a:n :}
   t a + tu a - ;

: SB-LINE ( str -- ) SB-APPEND 10 SB-APPEND-C ;

\ The model's Table module's lines before the tuples.
: HEAD$ ( str str str -- str )
   {: f:^ fu:n n:^ nu:n m:^ mu:n :}
   SB-RESET
   s" ---- MODULE " SB-APPEND m mu SB-APPEND s" Table ----" SB-LINE
   s" \* The lines of " SB-APPEND n nu SB-APPEND
   s" , " SB-APPEND f fu SB-APPEND s" , as <<state, input," SB-LINE
   s" \* next state>>. A test fails unless this file is that table." SB-LINE
   s" Lines == {" SB-LINE SB$ ;

\ A line state input >next as a tuple after the text at h; a PICK
\ line has no fixed next state, which the check cannot hold.
: TUPLE+ ( str str str ^SPAN:span<u8> ^ARENA:arena -- )
   {: s:^ su:n i:^ iu:n x:^ xu:n h ar :}
   x c@ 62 = TTRUE
   SB-RESET h @ SPAN:LEN 0 > if S\" ,\n" SB-APPEND then
   S\"     <<\q" SB-APPEND s su SB-APPEND
   S\" \q, \q" SB-APPEND i iu SB-APPEND
   S\" \q, \q" SB-APPEND x 1+ xu 1- SB-APPEND
   S\" \q>>" SB-APPEND h SB$ ar ADD ;

\ The table at t as the model's Table module holds it.
: TABLE-TLA ( str str str str ^ARENA:arena -- str )
   {: t:^ tu:n f:^ fu:n n:^ nu:n m:^ mu:n ar :}
   ar RENDERED-NEW {: h :}
   t tu [: h ar TUPLE+ ;] FSM:LINES
   ar RENDERED-NEW {: o :}
   o f fu n nu m mu HEAD$ ar ADD  o h @ SPAN:$ ar ADD
   o S\" \n}\n====\n" ar ADD  o @ SPAN:$ ;

public

\ The table named n in the file at f as model m's Table module, in the
\ arena; empty, and the running test failed, when the file holds no table n.
: TABLE-MODULE ( str str str ^ARENA:arena -- str )
   {: f:^ fu:n n:^ nu:n m:^ mu:n ar :}
   f fu ar FILE$ n nu TABLE-AT {: t:^ tu:n :}
   tu 0 > TTRUE  tu 0= if t 0 exit then
   t tu f fu n nu m mu ar TABLE-TLA ;

;package
