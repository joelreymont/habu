\ ptx-ir-test.f - static value fixtures for the PTX IR optimizer seed.

require lib/ptx/test-prelude.f
require lib/ptx/ad-ir.f

create PTXIRT-OPS 7 cells allot

: PTXIRT-SOFTMAX-OPS ( -- )
   OP-DUP  PTXIRT-OPS 0 cells + !  OP-BMAX PTXIRT-OPS 1 cells + !  OP-BSUB PTXIRT-OPS 2 cells + !
   OP-EXP  PTXIRT-OPS 3 cells + !  OP-DUP  PTXIRT-OPS 4 cells + !  OP-BSUM PTXIRT-OPS 5 cells + !
   OP-BDIV PTXIRT-OPS 6 cells + ! ;

T-RESET

: PTXIRT-FOLD ( -- )
   PTXIR-RESET
   2 PTXIR-CONST 3 PTXIR-CONST PTXIR-ADD {: sum:n :}
   sum PTXIR-CONST? TTRUE
   sum PTXIR-CONST-VAL 5 T=
   4 PTXIR-CONST 3 PTXIR-CONST PTXIR-MUL {: prod:n :}
   prod PTXIR-CONST-VAL 12 T= ;

: PTXIRT-PEEPHOLE ( -- )
   PTXIR-RESET
   PTXIR-INPUT {: x:n :}
   x PTXIR-NEG PTXIR-NEG x T=
   x 0 PTXIR-CONST PTXIR-ADD x T=
   x 1 PTXIR-CONST PTXIR-MUL x T=
   x PTXIR-RENDER s" y" STR= TTRUE ;

: PTXIRT-CSE ( -- )
   PTXIR-RESET
   PTXIR-INPUT {: x:n :}
   1 PTXIR-CONST {: one:n :}
   x one PTXIR-ADD {: a:n :}
   one x PTXIR-ADD {: b:n :}
   a b T=
   PTXIR-COUNT 3 T= ;

: PTXIRT-INPUT-SYMS ( -- )
   PTXIR-RESET
   0 PTXIR-INPUT# {: x:n :}
   1 PTXIR-INPUT# {: y:n :}
   0 PTXIR-INPUT# x T=
   x y <> TTRUE
   PTXIR-COUNT 2 T= ;

\ wave D: ptxir-node is a PRODUCT — by-value construct/destructure roundtrip,
\ one-layout-dup and one-layout-drop shapes (dot acceptance).
: PTXIRT-NODE-SUM ( ptxir-node -- n ) PTXIR-NODE> + + + + ;
: PTXIRT-NODE-PRODUCT ( -- )
   1 2 3 4 5 >PTXIR-NODE PTXIR-NODE> {: op:n a:n b:n val:n live:n :}
   op 1 T=  a 2 T=  b 3 T=  val 4 T=  live 5 T=
   1 2 3 4 5 >PTXIR-NODE PTXIR-NODE-DUP PTXIRT-NODE-SUM 15 T= PTXIRT-NODE-SUM 15 T=
   1 2 3 4 5 PTXIR-NODE-DUP-RAW PTXIRT-NODE-SUM 15 T= PTXIRT-NODE-SUM 15 T=
   1 2 3 4 5 >PTXIR-NODE PTXIR-NODE-DROP ;

\ switchover wave A: PTXIR-FIND-RAW / PTXIR-FIND return option<n> (SOME matching
\ node id, else NONE). Both branches, via the raw finder (interned consts are
\ written with live 0).
: PTXIRT-FIND-OPTION ( -- )
   PTXIR-RESET
   5 PTXIR-CONST {: five:n :}
   PTXIR-K-CONST PTXIR-NONE PTXIR-NONE 5 0 PTXIR-FIND-RAW MATCH option
     none OF 0 0= 0= ENDOF                          \ none -> fail (const 5 exists)
     some OF five = ENDOF                            \ some(id) -> the interned const
   ;MATCH TTRUE
   PTXIR-K-CONST PTXIR-NONE PTXIR-NONE 99 0 PTXIR-FIND-RAW MATCH option
     none OF 0 0= ENDOF                             \ none -> pass (no const 99)
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE ;

: PTXIRT-DCE ( -- )
   PTXIR-RESET
   PTXIR-INPUT {: x:n :}
   x 2 PTXIR-CONST PTXIR-ADD {: root:n :}
   x 3 PTXIR-CONST PTXIR-MUL {: dead:n :}
   root PTXIR-LIVE-COUNT 3 T=
   root PTXIR-LIVE@ TTRUE
   dead PTXIR-LIVE@ TFALSE ;

: PTXIRT-SOFTMAX-BWD ( -- )
   PTXIR-RESET
   0 PTXIR-INPUT# {: y:n :}
   1 PTXIR-INPUT# {: dy:n :}
   dy y PTXIR-MUL PTXIR-BSUM {: s:n :}
   dy s PTXIR-BSUB y PTXIR-MUL {: dx:n :}
   dx PTXIR-OP@ PTXIR-K-MUL T=
   dx PTXIR-A@ y T=
   dx PTXIR-B@ PTXIR-OP@ PTXIR-K-BSUB T=
   dx PTXIR-LIVE-COUNT 6 T=
   dx PTXIR-RENDER s" y dy y dy *. BLOCK-SUM PTX:B- *." STR= TTRUE ;

: PTXIRT-ADIR-SOFTMAX-BWD ( -- )
   PTXIRT-SOFTMAX-OPS
   PTXIRT-OPS 7 ADIR-SOFTMAX-BWD$ s" y dy y dy *. BLOCK-SUM PTX:B- *." STR= TTRUE
   PTXIR-COUNT 6 T= ;

: PTXIRT-ADIR-SOFTMAX-BODY ( -- )
   s" DUP BLOCK-MAX PTX:B- EXP. DUP BLOCK-SUM PTX:B/" ADIR-SOFTMAX-BWD-BODY$
   s" y dy y dy *. BLOCK-SUM PTX:B- *." STR= TTRUE
   PTXIR-COUNT 6 T= ;

: PTXIRT-RENDER ( -- )
   PTXIR-RESET
   2 PTXIR-CONST 3 PTXIR-CONST PTXIR-ADD {: folded:n :}
   folded PTXIR-RENDER s" 5" STR= TTRUE
   2 PTXIR-INPUT# 1 PTXIR-CONST PTXIR-ADD
   PTXIR-RENDER s" i2 1 +." STR= TTRUE ;

: PTXIRT-BAD-ADIR ( -- )
   PTXIRT-SOFTMAX-OPS
   OP-BSUM PTXIRT-OPS 6 cells + !
   PTXIRT-OPS 7 ADIR-SOFTMAX-BWD-IR drop ;

: PTXIRT-BAD-ADIR-BODY ( -- )
   s" DUP BLOCK-MAX B- EXP. DUP BLOCK-SUM B/" ADIR-SOFTMAX-BWD-BODY$ 2drop ;

: PTXIRT-OVERFLOW ( -- )
   PTXIR-RESET
   PTXIR-MAX 0 ?do i PTXIR-CONST drop loop
   PTXIR-MAX PTXIR-CONST drop ;

PTXIRT-FOLD
PTXIRT-PEEPHOLE
PTXIRT-CSE
PTXIRT-INPUT-SYMS
PTXIRT-NODE-PRODUCT
PTXIRT-FIND-OPTION
PTXIRT-DCE
PTXIRT-SOFTMAX-BWD
PTXIRT-ADIR-SOFTMAX-BWD
PTXIRT-ADIR-SOFTMAX-BODY
PTXIRT-RENDER
' PTXIRT-BAD-ADIR E-PTX-AD-UNKNOWN TTHROWS
' PTXIRT-BAD-ADIR-BODY E-PTX-AD-UNKNOWN TTHROWS
' PTXIRT-OVERFLOW E-PTX-IR-OVERFLOW TTHROWS

T-REPORT
