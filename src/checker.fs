\ checker.fs — four-row composition + token classification + CHECK-DEF.
\ State is the current effect ( declared-in | declared-Rin -- DCUR | RCUR ).

variable DCUR    \ current data stack term
variable RCUR    \ current return stack term
variable DECL    \ declared effect of the definition in progress (EXIT/final)

\ Declared signature string of the definition in progress — control.fs re-parses
\ it for RECURSE (a fresh instantiation of this word's own effect).
create CUR-SIG-BUF 256 chars allot   variable CUR-SIG-LEN
: CUR-SIG! ( c-addr u -- )  256 min dup CUR-SIG-LEN !  CUR-SIG-BUF swap move ;
: CUR-SIG@ ( -- c-addr u )  CUR-SIG-BUF CUR-SIG-LEN @ ;

\ Apply an already-instantiated effect to the current state (compose all 4 rows).
: APPLY-EFFECT  ( eff -- )
   {: e :}
   DCUR @ e EFF>DIN UNIFY-ROW
   RCUR @ e EFF>RIN UNIFY-ROW
   e EFF>DOUT DCUR !
   e EFF>ROUT RCUR ! ;

: APPLY-SCHEME  ( sa su -- )   PARSE-SIG APPLY-EFFECT ;   \ INST (fresh) + apply
: PUSH-DTYPE    ( t -- )       DCUR @ swap MK-PUSH DCUR ! ;

\ --- classification helpers ---
: NUMBER?    ( c-addr u -- f )   s>number? nip nip ;       \ integer under BASE
: DEFINED?   ( c-addr u -- f )   find-name 0<> ;

\ Forbidden = compiler-manipulating words (need TRUSTED:). Compared by name
\ token so matching is case-insensitive (find-name is CI), like Forth itself.
s" EVALUATE" find-name constant NT-EVALUATE
s" POSTPONE" find-name constant NT-POSTPONE
s" COMPILE," find-name constant NT-COMPILE,
s" [" find-name constant NT-LBRACK
s" ]" find-name constant NT-RBRACK
: FORBIDDEN? ( c-addr u -- f )
   find-name {: nt :}
   nt 0= if false exit then
   nt NT-EVALUATE =  nt NT-POSTPONE = or  nt NT-COMPILE, = or
   nt NT-LBRACK = or  nt NT-RBRACK = or ;

\ Check one body token, in classification order.
: CHECK-WORD  ( c-addr u -- )
   2dup CUR-TOKEN!
   2dup CHECK-CONTROL if 2drop exit then
   2dup CHECK-QUOT    if 2drop exit then
   2dup CHECK-PARSE   if 2drop exit then
   2dup CHECK-LOCAL   if 2drop exit then
   2dup NUMBER?       if 2drop TC-I64 MK-CON PUSH-DTYPE exit then
   2dup EFFECT-OF dup 0= if              \ not charted
      drop
      2dup FORBIDDEN? if 2drop E-UNSAFE throw then
      DEFINED? if E-UNCHECKED throw then
      E-UNKNOWN throw
   else                                 ( c-addr u sa su )
      APPLY-SCHEME 2drop
   then ;

\ --- body tokenizer (own cursor — PARSE-SIG inside CHECK-WORD uses its own) ---
variable B>A   variable B>U
: BODY-INIT ( c-addr u -- )  B>U !  B>A ! ;
: B-SKIP ( -- )  begin B>U @ 0> B>A @ c@ bl = and while 1 B>A +! -1 B>U +! repeat ;
: B-NEXT ( -- a u )
   B-SKIP  B>A @  0
   begin B>U @ 0> B>A @ c@ bl <> and while  1+ 1 B>A +! -1 B>U +!  repeat ;
: WALK-BODY ( c-addr u -- )
   BODY-INIT  begin B-NEXT dup 0> while CHECK-WORD repeat 2drop ;

\ Check a definition: seed current from declared inputs, walk the body, then the
\ inferred output must unify with the declared output. Charts the scheme on
\ success. Throws (E-MISMATCH/E-ARITY/E-OCCURS/E-UNKNOWN/…) on failure.
: CHECK-DEF  ( name-a name-u sig-a sig-u body-a body-u -- )
   {: na nu sa su ba bu :}
   na nu CUR-WORD!
   sa su CUR-SIG!
   ARENA-RESET  TV-RESET  RV-RESET  CHECK-RESET
   sa su PARSE-SIG {: deff :}
   deff DECL !
   deff EFF>DIN DCUR !
   deff EFF>RIN RCUR !
   deff EFF>DIN TAIL-ROW RV-RIGID!     \ declared prefix is rigid: the body may not
   deff EFF>RIN TAIL-ROW RV-RIGID!     \ extend it (underflow/over-arity rejected)
   ba bu WALK-BODY
   DCUR @ STACK-ARITY  deff EFF>DOUT STACK-ARITY  <> if E-ARITY throw then
   RCUR @ STACK-ARITY  deff EFF>ROUT STACK-ARITY  <> if E-ARITY throw then
   DCUR @ deff EFF>DOUT UNIFY-ROW
   RCUR @ deff EFF>ROUT UNIFY-ROW
   deff na nu CHART ;
