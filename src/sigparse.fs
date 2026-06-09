\ sigparse.fs — PARSE-SIG: parse a signature STRING into a 4-stack effect,
\ allocating FRESH type/row vars by name per call (so it doubles as INST).
\ Builds terms in the ARENA. Caller (or test) does ARENA-RESET.
\ Depends on: config types rows effects-repr.
\
\ Grammar (whitespace-delimited tokens):
\   sig    = stack '--' stack ( '|' stack '--' stack )?
\   stack  = rowvar? type*
\   type   = conname | tyvar | 'ptr' type | '[' stack '--' stack ']'
\   conname= i64 u8 u32 cell bool char str addr
\   tyvar  = a..z (same letter -> same id per parse)
\   rowvar = A..Z (same letter -> same id per parse; leading-only = tail)
\ Stacks with no leading rowvar share one implicit data row (and one implicit
\ return row); each quotation gets its OWN fresh implicit rows. Return clause is
\ optional; default = one fresh row used for both rin and rout (untouched).

\ --- tokenizer state (cursor over the source string) ---
variable SRC-A          \ source c-addr
variable SRC-U          \ source length
variable SRC-P          \ current read position (0..SRC-U)

: SRC!  ( c-addr u -- )  SRC-U !  SRC-A !  0 SRC-P ! ;
: SRC-CHAR  ( -- c )     SRC-A @ SRC-P @ chars + c@ ;
: SRC-END?  ( -- f )     SRC-P @ SRC-U @ >= ;
: BLANK?    ( c -- f )   bl = ;

\ Advance over leading blanks.
: SKIP-WS  ( -- )
   begin
     SRC-END? if exit then
     SRC-CHAR BLANK? 0= if exit then
     1 SRC-P +!
   again ;

\ Current scan address minus the token start = token byte length
\ (one byte per char in gforth, so byte count = char count).
: TOK-LEN  ( start -- start u )  SRC-A @ SRC-P @ chars +  over - ;

\ NEXT-TOK: next whitespace token as ( a u ); ( a 0 ) at end of string.
: NEXT-TOK  ( -- a u )
   SKIP-WS
   SRC-END? if  SRC-A @ 0  exit  then
   SRC-A @ SRC-P @ chars +        ( start )
   begin
     SRC-END?              if TOK-LEN exit then
     SRC-CHAR BLANK?       if TOK-LEN exit then
     1 SRC-P +!
   again ;

\ --- one-token lookahead (push-back) ---
variable PEEK-A
variable PEEK-U
variable HAVE-PEEK

: PEEK-RESET ( -- )      0 HAVE-PEEK ! ;
: TAKE-TOK   ( -- a u )
   HAVE-PEEK @ if  0 HAVE-PEEK !  PEEK-A @ PEEK-U @ exit  then  NEXT-TOK ;
: PUSH-BACK  ( a u -- )  PEEK-U !  PEEK-A !  -1 HAVE-PEEK ! ;

\ --- name-keyed var tables (reset per parse) ---
\ TVNAME[letter-'a'] = var id+1 (0 = unseen); RVNAME[letter-'A'] likewise.
26 constant N-LETTER
create TVNAME  N-LETTER cells allot
create RVNAME  N-LETTER cells allot
variable IMP-DROW       \ implicit data-row id+1 for the current scope (0=none)
variable IMP-RROW       \ implicit return-row id+1 for the current scope (0=none)

: SIG-NAMES-RESET  ( -- )
   N-LETTER 0 ?do  0 TVNAME i cells + !  0 RVNAME i cells + !  loop
   0 IMP-DROW !  0 IMP-RROW ! ;

\ Type var by letter index: same letter -> same id within one parse.
: TVAR-OF  ( idx -- t )
   dup cells TVNAME + @ ?dup if  nip 1- MK-VAR exit  then  ( idx )
   1 TV-ALLOC                     ( idx id )
   dup 1+ rot cells TVNAME + !    ( id )
   MK-VAR ;

\ Row var by letter index: same letter -> same id within one parse.
: RVAR-OF  ( idx -- s )
   dup cells RVNAME + @ ?dup if  nip 1- MK-ROW exit  then  ( idx )
   1 RV-ALLOC                     ( idx id )
   dup 1+ rot cells RVNAME + !    ( id )
   MK-ROW ;

\ Shared implicit data row for the current scope (one fresh row var, reused).
: IMP-DROW-OF  ( -- s )
   IMP-DROW @ ?dup if  1- MK-ROW exit  then
   1 RV-ALLOC  dup 1+ IMP-DROW !  MK-ROW ;

\ Shared implicit return row for the current scope (one fresh row var, reused).
: IMP-RROW-OF  ( -- s )
   IMP-RROW @ ?dup if  1- MK-ROW exit  then
   1 RV-ALLOC  dup 1+ IMP-RROW !  MK-ROW ;

\ --- token classification ---
: SINGLE?  ( a u -- f )   nip 1 = ;
: TOK-CHAR ( a u -- c )   drop c@ ;
: LOWER?   ( c -- f )     dup [char] a >=  swap [char] z <=  and ;
: UPPER?   ( c -- f )     dup [char] A >=  swap [char] Z <=  and ;

\ caf is case-insensitive (like Forth) for keywords/type names. Single-letter
\ vars stay case-distinguished (lc=type, uc=row) — unambiguous since type names
\ are >=2 chars.
: UPC  ( c -- C )  dup [char] a [char] z 1+ within if [char] a - [char] A + then ;
: CI=  ( a1 u1 a2 u2 -- f )
   {: a1 u1 a2 u2 :}
   u1 u2 <> if false exit then
   u1 0 ?do  a1 i + c@ UPC  a2 i + c@ UPC  <> if false unloop exit then  loop
   true ;

\ Concrete type-name -> type code, or UNBOUND if unknown.
: CON-CODE  ( a u -- code )
   2dup s" i64"  CI= if 2drop TC-I64  exit then
   2dup s" u8"   CI= if 2drop TC-U8   exit then
   2dup s" u32"  CI= if 2drop TC-U32  exit then
   2dup s" cell" CI= if 2drop TC-CELL exit then
   2dup s" bool" CI= if 2drop TC-BOOL exit then
   2dup s" char" CI= if 2drop TC-CHAR exit then
   2dup s" str"  CI= if 2drop TC-STR  exit then
   2dup s" addr" CI= if 2drop TC-ADDR exit then
   2drop UNBOUND ;

\ Stack delimiters: end-of-string, ], --, |
: DELIM?  ( a u -- f )
   dup 0= if 2drop true exit then
   2dup s" ]"  CI= if 2drop true exit then
   2dup s" --" CI= if 2drop true exit then
   2dup s" |"  CI= >r 2drop r> ;

\ A token is a leading row var iff it is a single upper-case letter.
: ROW-LEAD?  ( a u -- f )   2dup SINGLE? if TOK-CHAR UPPER? else 2drop false then ;

\ --- type parsing (mutually recursive with quotation parsing) ---
defer PARSE-TYPE  ( a u -- t )
defer PARSE-QUOT  ( -- t )

\ Scratch buffer of element type-terms for one stack (bounded by MAX-DEPTH).
create ELEMS  MAX-DEPTH cells allot

\ Fold n element terms above the tail into pushes bottom->top.
\ Elements arrive on the data stack as tn (top) .. t1, tail underneath. Spill
\ them into ELEMS so t1 ends at index 0, then push tail,t1,..,tn.
: MK-COUNT  ( tail t1 .. tn n -- s )
   dup MAX-DEPTH > if E-DEPTH throw then
   {: n :}
   n 0 ?do  ELEMS  n 1- i -  cells +  !  loop   ( tail )  \ store tn..t1 -> idx n-1..0
   n 0 ?do  ELEMS i cells + @  MK-PUSH  loop     ( s )    \ push t1..tn bottom->top
   ;

\ Parse one stack onto the chosen tail. tail-xt has ( -- s ): the implicit tail
\ used when no leading row var is present.
\ A leading single upper-case token IS the row tail; remaining tokens up to the
\ next delimiter are element types, folded bottom->top with MK-PUSH.
: PARSE-STACK  ( tail-xt -- s )
   {: tail-xt :}
   TAKE-TOK  2dup ROW-LEAD? if           ( a u )
     TOK-CHAR [char] A -  RVAR-OF        ( tail = explicit row var )
   else
     PUSH-BACK  tail-xt execute          ( tail = implicit row )
   then
   0 {: n :}                             ( tail )
   begin                                 ( tail t1 .. tk )
     TAKE-TOK  2dup DELIM? if PUSH-BACK n MK-COUNT exit then
     PARSE-TYPE  n 1+ to n
   again ;

\ --- type body ---
: (PARSE-TYPE)  ( a u -- t )
   2dup s" ptr" CI= if  2drop  NEXT-TOK PARSE-TYPE MK-PTR  exit  then
   2dup s" ["   CI= if  2drop  PARSE-QUOT             exit  then
   2dup SINGLE? if
     2dup TOK-CHAR LOWER? if  TOK-CHAR [char] a -  TVAR-OF   exit  then
     2dup TOK-CHAR UPPER? if  2drop  E-BADTYPE throw  then
   then
   2dup CON-CODE dup UNBOUND = if  drop 2drop E-BADTYPE throw  then
   nip nip MK-CON ;
' (PARSE-TYPE) IS PARSE-TYPE

\ --- quotation: '[' din '--' dout ']' ---
\ Own fresh implicit data rows (save/restore the scope's implicit-row state);
\ return rows untouched = one fresh row shared rin/rout.
: WANT-TOK  ( c-addr len -- )         \ next token must equal a literal
   {: ca cu :}  TAKE-TOK ca cu CI= 0= if E-BADTYPE throw then ;
: WANT-SEP   ( -- )  s" --" WANT-TOK ;
: WANT-CLOSE ( -- )  s" ]"  WANT-TOK ;

: (PARSE-QUOT)  ( -- t )
   IMP-DROW @ IMP-RROW @ {: sd sr :}    \ save outer scope implicit rows
   0 IMP-DROW !  0 IMP-RROW !           \ quotation gets its own fresh rows
   ['] IMP-DROW-OF PARSE-STACK          ( din )
   WANT-SEP
   ['] IMP-DROW-OF PARSE-STACK          ( din dout )
   WANT-CLOSE
   IMP-RROW-OF dup                      ( din dout rin rout=rin: untouched )
   MK-EFFECT MK-QUOT                    ( t )
   sd IMP-DROW !  sr IMP-RROW !         \ restore outer scope
   ;
' (PARSE-QUOT) IS PARSE-QUOT

\ --- top level ---
\ ( Din Rin -- Dout Rout ); return clause optional.
: PARSE-SIG  ( c-addr u -- eff )
   SRC!  PEEK-RESET  SIG-NAMES-RESET
   ['] IMP-DROW-OF PARSE-STACK          ( din )
   WANT-SEP
   ['] IMP-DROW-OF PARSE-STACK          ( din dout )
   TAKE-TOK  2dup s" |" CI= if   ( din dout a u )
     2drop                              \ explicit return clause
     ['] IMP-RROW-OF PARSE-STACK        ( din dout rin )
     WANT-SEP
     ['] IMP-RROW-OF PARSE-STACK        ( din dout rin rout )
   else                                 ( din dout a u )
     dup if  2drop  E-BADTYPE throw  then  \ nonempty token that is not | = junk
     2drop                              \ end of string: return untouched
     IMP-RROW-OF dup                    ( din dout rin rout )
   then
   MK-EFFECT ;
