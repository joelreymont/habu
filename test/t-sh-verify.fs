\ t-sh-verify.fs — the native checker's VERIFY mode (CHECK!): a typed definition
\ is checked body-against-its-own-declared ( in -- out ), rejecting a mismatch.
\ Covers named rows, quotation params + combinator round-trip, and the
\ concrete-type distinctions the native grammar makes. Mirrors t-sh-check's
\ CHK2 but installs CHECK! (verify) as the hook. Run: gforth test/t-sh-verify.fs -e bye
require sh-driver.fs
: V ( a u -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   s" : HOOK CHECK! dup . ; ' HOOK set-check " +B  +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;

\ --- body verified against declared sig: good certifies (-1), wrong rejects (0)
T{ s" : SQ ( i64 -- i64 ) dup * ;"        V s\" -1\n" compare 0= -> true }T
T{ s" : BAD ( i64 -- i64 ) dup ;"         V s\" 0\n"  compare 0= -> true }T
T{ s" : DR ( i64 i64 -- i64 ) + ;"        V s\" -1\n" compare 0= -> true }T
T{ s" : XS ( i64 -- i64 i64 ) dup ;"      V s\" -1\n" compare 0= -> true }T

\ --- polymorphic + named row vars
T{ s" : SW ( a b -- b a ) swap ;"         V s\" -1\n" compare 0= -> true }T
T{ s" : PSH ( R -- R i64 ) 5 ;"           V s\" -1\n" compare 0= -> true }T
T{ s" : BR ( R -- R i64 ) 5 5 ;"          V s\" 0\n"  compare 0= -> true }T
T{ s" : OV ( a b -- a b a ) over ;"       V s\" -1\n" compare 0= -> true }T

\ --- quotation parameters + execute (the combinator surface)
T{ s" : AP ( i64 [ i64 -- i64 ] -- i64 ) execute ;"  V s\" -1\n" compare 0= -> true }T
T{ s" : BQ ( i64 [ i64 -- i64 ] -- i64 ) 2drop ;"    V s\" 0\n"  compare 0= -> true }T
T{ s" : IDQ ( [ i64 -- i64 ] -- [ i64 -- i64 ] ) ;"  V s\" -1\n" compare 0= -> true }T

\ --- distinct concrete widths: a pure mismatch rejects (n subsumes via prims)
T{ s" : WW ( u8 -- u32 ) ;"               V s\" 0\n"  compare 0= -> true }T
T{ s" : WK ( u8 -- u8 ) ;"                V s\" -1\n" compare 0= -> true }T

\ --- nested quotations verify (the recursive parser handles depth)
T{ s" : NQ ( [ [ i64 -- i64 ] -- i64 ] -- ) drop ;"  V s\" -1\n" compare 0= -> true }T
\ --- return stack: balance is inferred; >r without r> rejects
T{ s" : RB ( i64 -- ) >r ;"               V s\" 0\n"  compare 0= -> true }T
T{ s" : RK ( i64 -- i64 ) >r r> ;"        V s\" -1\n" compare 0= -> true }T
\ --- ptr typed as an address (int-family)
T{ s" : PP ( ptr -- ptr ) ;"              V s\" -1\n" compare 0= -> true }T

\ --- return-stack clause ( ... | rin -- rout ): >R / R> declarations verify,
\ a wrong return declaration rejects
T{ s" : TOR ( R a | S -- R | S a ) >r ;"      V s\" -1\n" compare 0= -> true }T
T{ s" : BAL ( R a | S -- R a | S ) >r ;"      V s\" 0\n"  compare 0= -> true }T
T{ s" : FR ( R | S a -- R a | S ) r> ;"       V s\" -1\n" compare 0= -> true }T

\ --- distinct concrete widths reject pairwise; n (generic int) subsumes them
T{ s" : C1 ( str -- i64 ) ;"              V s\" 0\n"  compare 0= -> true }T
T{ s" : C2 ( bool -- char ) ;"            V s\" 0\n"  compare 0= -> true }T
T{ s" : C3 ( cell -- i64 ) ;"             V s\" 0\n"  compare 0= -> true }T
T{ s" : C4 ( u8 -- i64 ) 0= ;"            V s\" 0\n"  compare 0= -> true }T   \ 0= : n -- bool; bool is NOT i64
T{ s" : C5 ( char -- char ) 1 + ;"        V s\" -1\n" compare 0= -> true }T
\ bool is a distinct flag type: a comparison result must be declared bool, not
\ folded into i64; IF/UNTIL/WHILE consume a flag, so a concrete non-flag rejects.
T{ s" : CF2 ( i64 -- bool ) 0= ;"         V s\" -1\n" compare 0= -> true }T
T{ s" : CF3 ( char -- i64 ) if 1 else 2 then ;" V s\" 0\n"  compare 0= -> true }T   \ IF on char -> reject
T{ s" : CF4 ( bool -- i64 ) if 1 else 2 then ;" V s\" -1\n" compare 0= -> true }T
T{ s" : CF5 ( i64 i64 -- bool ) < ;"      V s\" -1\n" compare 0= -> true }T

\ --- robustness: a malformed declared signature must REJECT (a missing/wrong
\ '--' or ']' delimiter can't be silently reparsed as some other effect), and
\ never crash the engine (V builds + runs an engine; a crash/hang fails here).
T{ s" : M1 ( [ -- ) drop ;"               V s\" 0\n"  compare 0= -> true }T   \ unclosed quot
T{ s" : M2 ( a | b | c -- a ) drop ;"     V s\" 0\n"  compare 0= -> true }T   \ triple pipe
T{ s" : M3 ( i64 ) drop ;"                V s\" 0\n"  compare 0= -> true }T   \ no top-level --
T{ s" : M4 ( [ i64 -- i64 -- i64 ) ;"     V s\" 0\n"  compare 0= -> true }T   \ quot missing ]
T{ s" : M5 ( [ i64 i64 ] -- ) drop ;"     V s\" 0\n"  compare 0= -> true }T   \ quot missing inner --
T{ s" : M6 ( i64 i64 -- i64 ) + ;"        V s\" -1\n" compare 0= -> true }T   \ well-formed still certifies
T{ s" : M3 ( i64 -- i64 | S ) ;"          V s\" 0\n"  compare 0= -> true }T   \ asymmetric return

\ --- exit inside a [: ;] quotation is scoped to the quote, NOT the colon def.
\ Verify catches the wrong effect: declaring ( -- i64 ) for a body that actually
\ leaves six values must REJECT (this was a false-certify before the fix).
T{ s" : QXB ( -- i64 ) 5 [: exit ;] execute 1 2 3 4 5 ;"  V s\" 0\n"  compare 0= -> true }T
T{ s" : QXG ( -- i64 i64 i64 ) [: 1 2 3 exit ;] execute ;" V s\" -1\n" compare 0= -> true }T

\ --- regression: a (...)-named word in a body is a CALL, not a declared sig.
\ The sig scanner requires '( ' (paren+space); before that, '(CMP)' was eaten as
\ a sig, emptying LT's body so it rejected against its declared ( a b -- i64 ).
T{ s" : (CMP) 2drop -1 ; : LT ( a b -- i64 ) (CMP) ;"  V s\" -1\n-1\n" compare 0= -> true }T

\ --- deep + polymorphic: spill past inference, nested rows, shared tyvars
T{ s" : D1 ( a b c d e f -- f e d c b a ) >r >r >r >r >r >r r> r> r> r> r> r> ;"  V s\" -1\n" compare 0= -> true }T
T{ s" : D2 ( a a a -- a ) drop drop ;"    V s\" -1\n" compare 0= -> true }T
T{ s" : D3 ( a -- b ) ;"                  V s\" -1\n" compare 0= -> true }T   \ a unifies with b (no-op)

#ERRORS @ 0<> negate (bye)
