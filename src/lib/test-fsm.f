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
   {: h more:str ar :}
   h @ SPAN:$ {: old:str :}
   ar old STR:LEN more STR:LEN + ARENA:ALLOC drop {: b:^ :}
   old STR:DATA b old STR:LEN BYTE-COPY
   more STR:DATA b old STR:LEN + more STR:LEN BYTE-COPY
   b old STR:LEN more STR:LEN + SPAN:MAKE h ! ;

\ The file at the path, whole, in the arena.
: FILE$ ( str ^ARENA:arena -- str )
   {: p:str ar :}
   p FS:FILE-SIZE {: u:n :}
   ar u ARENA:ALLOC drop {: b:^ :}
   b  p b u FS:READ-ALL >str ;

\ The first offset at or after from of k in t, else -1.
: FIND-AT ( n str str -- n )
   {: from:n t:str k:str :}
   t k from STR:FIND-SUB-FROM MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Whether ku bytes at offset k open a line of t and end at a
\ byte <= 32 or at t's end.
: WHOLE? ( n str n -- bool )
   {: k:n t:str ku:n :}
   k ku + {: e:n :}
   k 0= if true else t STR:DATA k + 1- c@ 10 = then
   e t STR:LEN = if true else t STR:DATA e + c@ 33 < then and ;

\ The offset of the first whole k in t, else -1.
: KEY-LINE ( str str -- n )
   {: t:str k:str :}
   0 begin  t k FIND-AT  dup 0< 0= while
      dup t k STR:LEN WHOLE? if exit then
      1+
   repeat ;

\ The text from n where a line opens FSM:DEFINE n, n whole; else none.
: TABLE-AT ( str str -- str )
   {: t:str n:str :}
   SB-RESET s" FSM:DEFINE " SB-APPEND n SB-APPEND
   t SB$ KEY-LINE {: k:n :}
   k 0< if t STR:DATA 0 >str exit then
   k s" FSM:DEFINE " STR:LEN + {: a:n :}
   t STR:DATA a + t STR:LEN a - >str ;

: SB-LINE ( str -- ) SB-APPEND 10 SB-APPEND-C ;

\ The model's Table module's lines before the tuples.
: HEAD$ ( str str str -- str )
   {: f:str n:str m:str :}
   SB-RESET
   s" ---- MODULE " SB-APPEND m SB-APPEND s" Table ----" SB-LINE
   s" \* The lines of " SB-APPEND n SB-APPEND
   s" , " SB-APPEND f SB-APPEND s" , as <<state, input," SB-LINE
   s" \* next state>>. A test fails unless this file is that table." SB-LINE
   s" Lines == {" SB-LINE SB$ ;

\ A line state input >next as a tuple after the text at h; a PICK
\ line has no fixed next state, which the check cannot hold.
: TUPLE+ ( str str str ^SPAN:span<u8> ^ARENA:arena -- )
   {: st:str inp:str nx:str h ar :}
   nx STR:DATA c@ 62 = TTRUE
   SB-RESET h @ SPAN:LEN 0 > if S\" ,\n" SB-APPEND then
   S\"     <<\q" SB-APPEND st SB-APPEND
   S\" \q, \q" SB-APPEND inp SB-APPEND
   S\" \q, \q" SB-APPEND nx STR:DATA 1+ nx STR:LEN 1- >str SB-APPEND
   S\" \q>>" SB-APPEND h SB$ ar ADD ;

\ The table at t as the model's Table module holds it.
: TABLE-TLA ( str str str str ^ARENA:arena -- str )
   {: t:str f:str n:str m:str ar :}
   ar RENDERED-NEW {: h :}
   t [: h ar TUPLE+ ;] FSM:LINES
   ar RENDERED-NEW {: o :}
   o f n m HEAD$ ar ADD  o h @ SPAN:$ ar ADD
   o S\" \n}\n====\n" ar ADD  o @ SPAN:$ ;

public

\ The table named n in the file at f as model m's Table module, in the
\ arena; empty, and the running test failed, when the file holds no table n.
: TABLE-MODULE ( str str str ^ARENA:arena -- str )
   {: f:str n:str m:str ar :}
   f ar FILE$ n TABLE-AT {: t:str :}
   t STR:LEN 0 > TTRUE  t STR:LEN 0= if t STR:DATA 0 >str exit then
   t f n m ar TABLE-TLA ;

;package
