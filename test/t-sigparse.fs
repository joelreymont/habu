\ t-sigparse.fs — PARSE-SIG: signature string -> 4-stack effect, fresh vars
\ by name per call.  Assumes config+arena+types+rows+effects-repr+sigparse.
\ Words below have ( -- ) effect so ' WORD catch yields a clean ( code ).

\ --- concrete types: i64 i64 -- i64, dout top is i64 ---
T{ ARENA-RESET TV-RESET RV-RESET
   s" i64 i64 -- i64" PARSE-SIG
   EFF>DOUT STACK-TOP  -> TC-I64 MK-CON }T

\ --- R a -- R a a : dout two SAME var, din one var ---
: SAME-IDS  ( -- top2 top )
   ARENA-RESET TV-RESET RV-RESET
   s" R a -- R a a" PARSE-SIG  >r
   r@ EFF>DOUT STACK-TOP TERM>PAYLOAD                 \ dout top id
   r> EFF>DOUT STACK-REST STACK-TOP TERM>PAYLOAD ;    \ dout 2nd id
T{ SAME-IDS = -> true }T

: DIN-ONE  ( -- f )                       \ din has exactly one element
   ARENA-RESET TV-RESET RV-RESET
   s" R a -- R a a" PARSE-SIG EFF>DIN  >r
   r@ STACK-TOP TYVAR?                    \ top is a var
   r> STACK-REST SROW? and ;             \ rest is the bare row tail
T{ DIN-ONE -> true }T

\ --- same letter => same id; two letters => different ids ---
: SAME-LETTER  ( -- f )
   ARENA-RESET TV-RESET RV-RESET
   s" a -- a" PARSE-SIG  >r
   r@ EFF>DIN  STACK-TOP TERM>PAYLOAD
   r> EFF>DOUT STACK-TOP TERM>PAYLOAD = ;
T{ SAME-LETTER -> true }T

: DIFF-LETTER  ( -- f )
   ARENA-RESET TV-RESET RV-RESET
   s" a b -- a" PARSE-SIG EFF>DIN  >r
   r@ STACK-TOP TERM>PAYLOAD
   r> STACK-REST STACK-TOP TERM>PAYLOAD <> ;
T{ DIFF-LETTER -> true }T

\ --- ptr i64 : din top is a T-PTR term ---
: PTR-TAG  ( -- tag )
   ARENA-RESET TV-RESET RV-RESET
   s" ptr i64 -- i64" PARSE-SIG EFF>DIN STACK-TOP TERM>TAG ;
T{ PTR-TAG -> T-PTR }T

\ --- [ R -- R i64 ] quotation : din top is a T-QUOT term ---
: QUOT-TAG  ( -- tag )
   ARENA-RESET TV-RESET RV-RESET
   s" [ R -- R i64 ] -- i64" PARSE-SIG EFF>DIN STACK-TOP TERM>TAG ;
T{ QUOT-TAG -> T-QUOT }T

\ --- fresh instantiation: two parses of "a -- a" give different ids ---
: TWICE  ( -- f )
   ARENA-RESET TV-RESET RV-RESET
   s" a -- a" PARSE-SIG EFF>DIN STACK-TOP TERM>PAYLOAD
   s" a -- a" PARSE-SIG EFF>DIN STACK-TOP TERM>PAYLOAD <> ;
T{ TWICE -> true }T

\ --- unknown type name throws E-BADTYPE (operands built inside W) ---
: BADTYPE  ( -- )
   ARENA-RESET TV-RESET RV-RESET
   s" xyz -- i64" PARSE-SIG drop ;
T{ ' BADTYPE catch -> E-BADTYPE }T
