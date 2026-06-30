\ ad-ir.f - bridge AD op lists into the PTX expression IR and optimized emit path.
\
\ The general AD DAG emitter recomputes a literal backward from a forward op list.
\ This file recognizes the supported softmax-row op list, builds the canonical
\ closed-form IR dx = y * (dy - BLOCK-SUM(dy * y)), and lowers that same form to
\ PTX emit registers. Load after lib/ptx/ir.f, lib/ptx/ad-dag.f, and
\ lib/ptx/cg-collective.f. Checked Habu.

7 constant ADIR-SOFTMAX-LEN

: ADIR-OP@ ( ptr a n -- n ) {: ops:ptr idx:n :}
   idx 0 < if E-PTX-AD-UNKNOWN throw then
   ops idx cells + @ ;

: ADIR-EXPECT ( ptr a n n -- ) {: ops:ptr idx:n want:n :}
   ops idx ADIR-OP@ want <> if E-PTX-AD-UNKNOWN throw then ;

: ADIR-SOFTMAX-OPS-CHECK ( ptr a n -- ) {: ops:ptr len:n :}
   len ADIR-SOFTMAX-LEN <> if E-PTX-AD-UNKNOWN throw then
   ops 0 OP-DUP  ADIR-EXPECT
   ops 1 OP-BMAX ADIR-EXPECT
   ops 2 OP-BSUB ADIR-EXPECT
   ops 3 OP-EXP  ADIR-EXPECT
   ops 4 OP-DUP  ADIR-EXPECT
   ops 5 OP-BSUM ADIR-EXPECT
   ops 6 OP-BDIV ADIR-EXPECT ;

: ADIR-SOFTMAX-BWD-IR ( ptr a n -- n )
   ADIR-SOFTMAX-OPS-CHECK
   PTXIR-RESET
   0 PTXIR-INPUT# {: y:n :}
   1 PTXIR-INPUT# {: dy:n :}
   dy y PTXIR-MUL PTXIR-BSUM {: s:n :}
   dy s PTXIR-BSUB y PTXIR-MUL ;

: ADIR-SOFTMAX-BWD$ ( ptr a n -- ptr u8 n )
   ADIR-SOFTMAX-BWD-IR PTXIR-RENDER ;

: ADIR-EMIT-SOFTMAX-BWD ( n n -- n ) {: y:n dy:n :}
   dy y EMIT-MUL {: dyy:n :}
   dyy EMIT-BLOCK-SUM {: s:n :}
   dy s EMIT-B- {: dms:n :}
   dms y EMIT-MUL ;

: ADIR-EMIT-SOFTMAX-BWD-FROM ( ptr a n n n -- n ) {: ops:ptr len:n y:n dy:n :}
   ops len ADIR-SOFTMAX-OPS-CHECK
   y dy ADIR-EMIT-SOFTMAX-BWD ;
