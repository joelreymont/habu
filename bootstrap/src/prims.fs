\ prims.fs — the closed v1 primitive effect table. Each primitive's signature
\ is parsed by PARSE-SIG and stored (canonical) under its name by CHART.
\ Depends on: config arena types rows effects-repr sigparse db.
\
\ PRIM resets the per-check arena, parses the signature, and charts it under the
\ given name. ARENA-RESET wipes the term arena before each parse so successive
\ primitives never collide on var ids.

: PRIM  ( sig-a sig-u name-a name-u -- )
   2>r  ARENA-RESET  PARSE-SIG  2r>  CHART ;

\ --- stack shuffles ---
s" R a -- R a a"       s" DUP"   PRIM
s" R a -- R"           s" DROP"  PRIM
s" R a b -- R b a"     s" SWAP"  PRIM
s" R a b -- R a b a"   s" OVER"  PRIM
s" R a b c -- R b c a" s" ROT"   PRIM
s" R a b c -- R c a b" s" -ROT"  PRIM
s" R a b -- R b"       s" NIP"   PRIM
s" R a b -- R b a b"   s" TUCK"  PRIM

\ --- binary arithmetic / bitwise ( R i64 i64 -- R i64 ) ---
s" R i64 i64 -- R i64" s" +"      PRIM
s" R i64 i64 -- R i64" s" -"      PRIM
s" R i64 i64 -- R i64" s" *"      PRIM
s" R i64 i64 -- R i64" s" /"      PRIM
s" R i64 i64 -- R i64" s" MOD"    PRIM
s" R i64 i64 -- R i64" s" AND"    PRIM
s" R i64 i64 -- R i64" s" OR"     PRIM
s" R i64 i64 -- R i64" s" XOR"    PRIM
s" R i64 i64 -- R i64" s" LSHIFT" PRIM
s" R i64 i64 -- R i64" s" RSHIFT" PRIM

\ --- unary arithmetic ( R i64 -- R i64 ) ---
s" R i64 -- R i64" s" NEGATE" PRIM
s" R i64 -- R i64" s" INVERT" PRIM
s" R i64 -- R i64" s" ABS"    PRIM
s" R i64 -- R i64" s" 1+"     PRIM
s" R i64 -- R i64" s" 1-"     PRIM
s" R i64 -- R i64" s" 2*"     PRIM
s" R i64 -- R i64" s" 2/"     PRIM

\ --- comparison ( -> bool ) ---
s" R i64 -- R bool"     s" 0="  PRIM
s" R a a -- R bool"     s" ="   PRIM
s" R a a -- R bool"     s" <>"  PRIM
s" R i64 i64 -- R bool" s" <"   PRIM
s" R i64 i64 -- R bool" s" >"   PRIM
s" R i64 i64 -- R bool" s" <="  PRIM
s" R i64 i64 -- R bool" s" >="  PRIM
s" R i64 -- R bool"     s" 0<"  PRIM
s" R i64 -- R bool"     s" 0>"  PRIM
s" R i64 -- R bool"     s" 0<>" PRIM
s" R i64 i64 -- R bool" s" U<"  PRIM
s" R i64 i64 -- R bool" s" U>"  PRIM
s" R i64 i64 i64 -- R bool" s" WITHIN" PRIM   \ ( n lo hi -- f )

\ --- min/max, /mod, double-cell stack (codegen subset) ---
s" R i64 i64 -- R i64"     s" MIN"   PRIM
s" R i64 i64 -- R i64"     s" MAX"   PRIM
s" R i64 i64 -- R i64 i64" s" /MOD"  PRIM    \ ( a b -- rem quot )
s" R a b -- R a b a b"     s" 2DUP"  PRIM
s" R a b -- R"             s" 2DROP" PRIM
s" R a b c d -- R c d a b" s" 2SWAP" PRIM

\ --- memory ---
s" R ptr a -- R a"        s" @"   PRIM
s" R a ptr a -- R"        s" !"   PRIM
s" R ptr u8 -- R u8"      s" c@"  PRIM
s" R u8 ptr u8 -- R"      s" c!"  PRIM
s" R i64 ptr i64 -- R"    s" +!"  PRIM

\ --- floating point (f64 on the data stack; one cell, FP ops use D-regs) ---
s" R f64 f64 -- R f64"  s" F+"  PRIM
s" R f64 f64 -- R f64"  s" F-"  PRIM
s" R f64 f64 -- R f64"  s" F*"  PRIM
s" R f64 f64 -- R f64"  s" F/"  PRIM
s" R f64 -- R f64"      s" FNEGATE" PRIM
s" R f64 -- R f64"      s" FABS"    PRIM
s" R f64 -- R f64"      s" FSQRT"   PRIM
s" R f64 f64 -- R bool" s" F<"  PRIM
s" R f64 f64 -- R bool" s" F>"  PRIM
s" R f64 f64 -- R bool" s" F="  PRIM
s" R f64 -- R bool"     s" F0<" PRIM
s" R f64 -- R bool"     s" F0=" PRIM
s" R i64 -- R f64"      s" S>F" PRIM    \ int -> float (SCVTF)
s" R f64 -- R i64"      s" F>S" PRIM    \ float -> int, truncate (FCVTZS)

\ --- bump heap (codegen: mmap'd arena; HERE returns a ptr you can @/!) ---
s" R -- R ptr i64"  s" HERE"  PRIM
s" R i64 -- R"      s" ALLOT" PRIM
s" R i64 -- R"      s" ,"      PRIM
s" R u8 -- R"       s" C,"     PRIM

\ --- pointer arithmetic ---
s" R ptr a -- R ptr a" s" CELL+" PRIM
s" R ptr a -- R ptr a" s" CHAR+" PRIM
s" R i64 -- R i64"     s" CELLS" PRIM
s" R i64 -- R i64"     s" CHARS" PRIM

\ --- return stack moves ( data <-> return ) ---
s" R a -- R | S -- S a" s" >R" PRIM
s" R -- R a | S a -- S" s" R>" PRIM
s" R -- R a | S a -- S a" s" R@" PRIM

\ --- loop indices ---

\ --- higher order ---
s" R [ R -- S ] -- S"     s" EXECUTE" PRIM
s" R a [ R -- S ] -- S a" s" DIP"     PRIM
s" R a [ R a -- S ] -- S a" s" KEEP"  PRIM
\ ?DUP-IF — the typeable fused form of `?DUP IF … THEN`. Naked ?DUP is untypeable
\ (value-dependent 0/2 arity); this consumes the value and a quotation over it,
\ both the run (nonzero) and skip (zero) paths converging to R. See docs/effects.md.
s" R a [ R a -- R ] -- R" s" ?DUP-IF" PRIM

\ --- number parsing (the double result is modeled as one i64; honest enough to
\ catch the branch-imbalance bug class that bit the codegen — see LESSONS.md) ---
s" R str -- R i64 bool" s" S>NUMBER?" PRIM   \ ( c-addr u -- d flag ), d as i64

\ --- char literals ---
s" R -- R char" s" CHAR"    PRIM
s" R -- R char" s" [CHAR]"  PRIM

\ --- output ---
s" R i64 -- R" s" ."     PRIM
s" R i64 -- R" s" U."    PRIM
s" R i64 -- R" s" EMIT"  PRIM
s" R -- R"     s" CR"    PRIM
s" R -- R"     s" SPACE" PRIM

\ combinators / iterators (runtime in runtime.fs)
s" R i64 [ R -- R ] -- R"                              s" TIMES" PRIM
s" R a [ R a -- R b ] [ R a -- R c ] -- R b c"         s" BI"    PRIM
s" R a [ R a -- R b ] [ R a -- R c ] [ R a -- R d ] -- R b c d" s" TRI" PRIM
s" R ptr a i64 [ R a -- R ] -- R"                      s" EACH"  PRIM
s" R ptr a i64 [ R a -- R a ] -- R"                    s" MAP"   PRIM
s" R ptr a i64 b [ R b a -- R b ] -- R b"              s" FOLD"  PRIM

\ double return-stack + more output (completes the checklist)
s" R a b -- R | S -- S a b"    s" 2>R"   PRIM
s" R -- R a b | S a b -- S"    s" 2R>"   PRIM
s" R -- R a b | S a b -- S a b" s" 2R@"  PRIM
s" R i64 -- R"                 s" U."    PRIM
s" R -- R"                     s" SPACE" PRIM
s" R addr u32 -- R"            s" TYPE"  PRIM
