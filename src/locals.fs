\ locals.fs — typed locals { … } / {: … :}. Fills the CHECK-LOCAL hook.
\ A locals opener pops one data value per named input and binds the name to that
\ value's type for the rest of the definition; a later use of a known name pushes
\ its bound type. A typed name `a:u8` asserts the popped type unifies with u8.
\
\ Reset choice: checker.fs cannot be edited and never calls a locals reset, so we
\ detect a new definition ourselves. CHECK-DEF sets CUR-WORD@ once per definition
\ (CUR-TOKEN@ changes per token, so it is the wrong signal). On the first time the
\ hook is consulted in a new definition — i.e. CUR-WORD@ differs from the last one
\ we saw — we clear the table. This stops a prior definition's names leaking into
\ a definition that declares no locals of its own (a bare-name def would otherwise
\ resolve a stale name). v0 allows at most one locals clause per definition.

\ --- name -> type table -----------------------------------------------------
\ Names are short. Store each name's bytes in NAME-BUF (length-prefixed via a
\ parallel offset/len array) and its bound type term in a parallel cell array.
32 constant MAX-LOCALS        \ locals per definition (v0 bound)
64 constant MAX-NAME          \ bytes per local name

create LC-NAME  MAX-LOCALS MAX-NAME chars * allot   \ name bytes, MAX-NAME per slot
create LC-LEN   MAX-LOCALS cells allot               \ name length per slot
create LC-TYPE  MAX-LOCALS cells allot               \ bound type term per slot
variable LC-N                                        \ count of bound locals

create LC-WORD  MAX-NAME chars allot   \ name of the def whose table we hold
variable LC-WORD-LEN

: LOCALS-RESET ( -- )  0 LC-N ! ;

\ Note the current definition's word so we can tell when a new one begins.
: LC-NOTE-WORD ( -- )
   CUR-WORD@ MAX-NAME min dup LC-WORD-LEN !  LC-WORD swap move ;
: LC-NEW-DEF? ( -- f )   CUR-WORD@ LC-WORD LC-WORD-LEN @ CI= 0= ;

\ At the first hook consultation of a new definition, clear the stale table.
: LC-GUARD ( -- )  LC-NEW-DEF? if LOCALS-RESET LC-NOTE-WORD then ;

: LC-NAME@ ( i -- a u )  dup MAX-NAME chars * LC-NAME +  swap cells LC-LEN + @ ;
: LC-TYPE@ ( i -- t )    cells LC-TYPE + @ ;

\ Bind a fresh local: copy name bytes, store length and type.
: LC-BIND ( a u t -- )
   LC-N @ MAX-LOCALS >= if E-LOCAL throw then
   LC-N @ {: a u t i :}
   t       i cells LC-TYPE + !
   u       i cells LC-LEN  + !
   a  i MAX-NAME chars * LC-NAME +  u chars  move
   i 1+ LC-N ! ;

\ Find a local by name (case-insensitive, like Forth). ( a u -- t true | false )
: LC-FIND ( a u -- t true | false )
   LC-N @ 0 ?do
      2dup i LC-NAME@ CI= if  2drop i LC-TYPE@ true unloop exit  then
   loop  2drop false ;

\ --- opener / closer tokens -------------------------------------------------
: BRACE-OPEN?  ( a u -- f )
   2dup s" {"  CI= if 2drop true exit then  s" {:" CI= ;
: BRACE-CLOSE? ( a u -- f )   \ `}` ends `{ … }`; `:}` ends `{: … :}`
   2dup s" }"  CI= if 2drop true exit then  s" :}" CI= ;
: BRACE-SEP?   ( a u -- f )   s" --" CI= ;   \ inside `{ … }`, names end at --

\ --- popping a typed value off the data stack -------------------------------
\ Current top type term (resolve the row first). Underflow surfaces later.
: TOP-TYPE ( -- t )  DCUR @ RESOLVE-ROW STACK-TOP ;
: POP-ONE  ( -- )    s" R a -- R" APPLY-SCHEME ;   \ remove the top data value

\ Split a name token on a single `:` into name + type-name. The colon must be
\ neither first nor last byte. ( a u -- na nu ta tu true | a u false )
: SPLIT-COLON ( a u -- na nu ta tu f | a u f )
   {: a u :}
   u 0 ?do
      a i chars + c@ [char] : = if
         i 0= i u 1- = or if leave then           \ leading/trailing colon: no split
         a i  a i 1+ chars +  u i 1+ -  true unloop exit
      then
   loop  a u false ;

\ Bind one name token, popping one value. A typed `a:u8` asserts the popped
\ top unifies with the named concrete type before popping.
: BIND-NAME ( a u -- )
   SPLIT-COLON if                                ( na nu ta tu )
      CON-CODE dup UNBOUND = if E-BADTYPE throw then
      MK-CON {: na nu ct :}
      TOP-TYPE ct UNIFY-TYPE                     \ assert popped type = declared
      na nu TOP-TYPE LC-BIND  POP-ONE
   else                                          ( a u )
      TOP-TYPE LC-BIND  POP-ONE
   then ;

\ --- declaration drive ------------------------------------------------------
\ After an opener, names run until the closer. In `{ a b -- c d }` everything
\ after `--` is a comment (output names): bind only the inputs, then skip to the
\ closer. In `{: a b :}` there is no `--`: every name is an input.
\ Forth binds right-to-left (last name -> top of stack); collect names then bind
\ in reverse so the last name pops the current top.
create DECL-A  MAX-LOCALS cells allot   \ pending name addrs
create DECL-U  MAX-LOCALS cells allot   \ pending name lens
variable DECL-N

: DECL-PUSH ( a u -- )
   DECL-N @ MAX-LOCALS >= if E-LOCAL throw then
   DECL-N @ {: a u i :}
   a i cells DECL-A + !  u i cells DECL-U + !  i 1+ DECL-N ! ;

\ Bind the collected input names right-to-left.
: DECL-BIND ( -- )
   DECL-N @ 0 ?do
      DECL-N @ 1- i -  {: j :}
      j cells DECL-A + @  j cells DECL-U + @  BIND-NAME
   loop ;

\ Drain remaining tokens (the output-name comment) up to and incl. the closer.
: SKIP-TO-CLOSE ( -- )
   begin B-NEXT 2dup BRACE-CLOSE? if 2drop exit then 2drop again ;

: DO-LOCALS ( -- )
   LOCALS-RESET  0 DECL-N !
   begin
      B-NEXT
      2dup BRACE-CLOSE? if 2drop DECL-BIND exit then
      2dup BRACE-SEP?   if 2drop DECL-BIND SKIP-TO-CLOSE exit then
      DECL-PUSH
   again ;

\ --- hook -------------------------------------------------------------------
: (CHECK-LOCAL) ( a u -- f )
   LC-GUARD
   2dup BRACE-OPEN? if 2drop DO-LOCALS true exit then
   LC-FIND if PUSH-DTYPE true exit then           \ known name -> push its type
   false ;
' (CHECK-LOCAL) is CHECK-LOCAL
