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

#ERRORS @ 0<> negate (bye)
