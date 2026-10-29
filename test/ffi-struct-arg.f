\ ffi-struct-arg.f - C structs passed by value through FUNCTION:.
\ Loaded by src/lib/ffi-test.f on macOS arm64, inside package FFI-TEST, after
\ test/ffi-struct-result.f, whose canaried destination it reuses.
\
\ The callee is test/ffi-struct-fixture.f's library, compiled here: AAPCS64
\ passes a record of up to 16 bytes in consecutive integer registers, or
\ wholly on the stack once they run short, and a larger one as the address of
\ a copy the caller owns. Every checksum weighs each member by position, so a
\ member read from the wrong offset, register or stack slot fails its case.

: FFI-T-SA-LIBRARY ( -- )
   s" habu-ffi-struct" FS:MKTEMP-DIR {: dir:ptr diru :}
   dir diru FS:CLEANUP-TREE+
   dir diru STRUCT-FIXTURE:BUILD FFI:SELECT-LIBRARY ;
FFI-T-SA-LIBRARY

FUNCTION: FFI-T-SUM8 hb_sum8 ( ptr u8 -- n ) 0 8 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SUM12 hb_sum12 ( ptr u8 -- i32 ) 0 12 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SUM16 hb_sum16 ( ptr u8 -- n ) 0 16 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SUM24 hb_sum24 ( ptr u8 -- n ) 0 24 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SUM32 hb_sum32 ( ptr u8 -- n ) 0 32 STRUCT-ARG ;FUNCTION
\ The whole 16-cell staging buffer: the copy alone fills it.
FUNCTION: FFI-T-SUM128 hb_sum128 ( ptr u8 -- n ) 0 128 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SPILL16 hb_spill16 ( n n n n n n n ptr u8 n -- n )
   7 16 STRUCT-ARG
;FUNCTION
FUNCTION: FFI-T-BIG2 hb_big2 ( n ptr u8 n ptr u8 n -- n )
   1 24 STRUCT-ARG 3 32 STRUCT-ARG
;FUNCTION
FUNCTION: FFI-T-BIGSTACK hb_bigstack ( n n n n n n n n ptr u8 n -- n )
   8 32 STRUCT-ARG
;FUNCTION
FUNCTION: FFI-T-FMIX hb_fmix ( r ptr u8 f32 -- n ) 1 16 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-MUTATE hb_mutate ( ptr u8 -- n ) 0 32 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-HOLD hb_hold ( ptr u8 n -- n ) 0 32 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-KEEP24 hb_keep24 ( ptr u8 -- ) 0 24 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-KEPT hb_kept ( -- n ) ;FUNCTION
FUNCTION: FFI-T-PARK16 hb_park16 ( ptr u8 -- ptr u8 ) 0 16 STRUCT-ARG ;FUNCTION
FUNCTION: FFI-T-SWAP16 hb_swap16 ( ptr u8 ptr u8 -- )
   0 16 STRUCT-ARG 16 STRUCT-RESULT
;FUNCTION
FUNCTION: FFI-T-ECHO32 hb_echo32 ( n ptr u8 ptr u8 -- )
   1 32 STRUCT-ARG 32 STRUCT-RESULT
;FUNCTION
\ Nine C arguments put the ninth on the stack beside the x8 destination.
FUNCTION: FFI-T-RET9 hb_ret9 ( n n n n n n n n n ptr u8 -- ) 32 STRUCT-RESULT ;FUNCTION

create FFI-T-S8 8 allot
create FFI-T-S16 16 allot
create FFI-T-S24 24 allot
create FFI-T-S32 32 allot
create FFI-T-S32B 32 allot
create FFI-T-S128 128 allot

: FFI-T-MIX ( n n -- n ) swap 31 * + ;
: FFI-T-H2 ( n n -- n ) {: a:n b:n :} 1 a FFI-T-MIX b FFI-T-MIX ;
: FFI-T-H4 ( n n n n -- n ) {: a:n b:n c:n d:n :}
   1 a FFI-T-MIX b FFI-T-MIX c FFI-T-MIX d FFI-T-MIX ;
\ MIX folded over 1..count from a seed of 1, as the fixture folds its longs.
: FFI-T-FOLD ( n -- n ) {: count:n :}
   1 count 0 ?do i 1+ FFI-T-MIX loop ;

: FFI-T-S16! ( n n ptr u8 -- ) {: a:n b:n p :}
   a p LE:U64!  b p 8 + LE:U64! ;
\ s24 is { int64 a; int32 b; uint32 c; int64 d }.
: FFI-T-S24! ( n n n n ptr u8 -- ) {: a:n b:n c:n d:n p :}
   a p LE:U64!  b p 8 + LE:U32!  c p 12 + LE:U32!  d p 16 + LE:U64! ;
: FFI-T-S32! ( n n n n ptr u8 -- ) {: a:n b:n c:n d:n p :}
   a p LE:U64!  b p 8 + LE:U64!  c p 16 + LE:U64!  d p 24 + LE:U64! ;
: FFI-T-S32= ( n n n n ptr u8 -- ) {: a:n b:n c:n d:n p :}
   p LE:U64@ a T=  p 8 + LE:U64@ b T=  p 16 + LE:U64@ c T=  p 24 + LE:U64@ d T= ;

: FFI-T-S128-SUM ( -- n )
   1 16 0 ?do i 3 * 20 - FFI-T-MIX loop ;

\ The callback runs while C holds its parameter's address: it rewrites the
\ source record and makes its own struct call through the same staging.
CALLBACK: FFI-T-SA-CB ( -- n ) 0 FALLBACK ;CALLBACK
: FFI-T-SA-CB-IMPL ( -- n )
   91 92 93 94 FFI-T-S32 FFI-T-S32!
   50 60 70 80 FFI-T-S32B FFI-T-S32!
   FFI-T-S32B FFI-T-SUM32 ;
' FFI-T-SA-CB-IMPL FFI-T-SA-CB-BODY !

: FFI-T-HOLD-CASE ( -- )
   1 2 3 4 FFI-T-S32 FFI-T-S32!
   FFI-T-SA-CB TASK:SELF-CONTEXT FFI-CB:ENTRY {: entry:n :}
   FFI-T-S32 entry FFI-T-HOLD
   FFI-T-SA-CB FFI-CB:UNBIND
   1 2 3 4 FFI-T-H4 50 60 70 80 FFI-T-H4 FFI-T-MIX T= ;

: FFI-T-REGISTER-ARGS ( -- )
   s" an 8-byte struct argument rides one register" T-LABEL
   -3 FFI-T-S8 LE:U32!  9 FFI-T-S8 4 + LE:U32!
   FFI-T-S8 FFI-T-SUM8 -3 9 FFI-T-H2 T=

   s" a 12-byte struct argument reads exactly its bytes" T-LABEL
   12 0 GUARD-PAGE:TAIL {: tail:ptr :}
   -1 tail LE:U32!  2 tail 4 + LE:U32!  3 tail 8 + LE:U32!
   tail FFI-T-SUM12 -77 T=

   s" a 16-byte struct argument rides two registers" T-LABEL
   11 -12 FFI-T-S16 FFI-T-S16!
   FFI-T-S16 FFI-T-SUM16 11 -12 FFI-T-H2 T=

   s" a struct argument beside a double and a float" T-LABEL
   3 4 FFI-T-S16 FFI-T-S16!
   2.5 FFI-T-S16 1.25 FFI-T-FMIX 3 4 FFI-T-H2 10 FFI-T-MIX 5 FFI-T-MIX T=

   s" a 16-byte struct one register short spills whole with the next long" T-LABEL
   11 -12 FFI-T-S16 FFI-T-S16!
   1 2 3 4 5 6 7 FFI-T-S16 13 FFI-T-SPILL16
   7 FFI-T-FOLD 0 FFI-T-MIX 11 -12 FFI-T-H2 FFI-T-MIX 13 FFI-T-MIX T= ;

: FFI-T-COPY-ARGS ( -- )
   s" larger struct arguments pass private copies" T-LABEL
   7 -5 $FFFFFFF0 -9 FFI-T-S24 FFI-T-S24!
   FFI-T-S24 FFI-T-SUM24 7 -5 $FFFFFFF0 -9 FFI-T-H4 T=
   1 -2 3 -4 FFI-T-S32 FFI-T-S32!
   FFI-T-S32 FFI-T-SUM32 1 -2 3 -4 FFI-T-H4 T=
   16 0 ?do i 3 * 20 - FFI-T-S128 i 8 * + LE:U64! loop
   FFI-T-S128 FFI-T-SUM128 FFI-T-S128-SUM T=

   s" two copies between scalars" T-LABEL
   5 FFI-T-S24 6 FFI-T-S32 7 FFI-T-BIG2
   1 5 FFI-T-MIX 7 -5 $FFFFFFF0 -9 FFI-T-H4 FFI-T-MIX 6 FFI-T-MIX
   1 -2 3 -4 FFI-T-H4 FFI-T-MIX 7 FFI-T-MIX T=

   s" a copy's address on the stack after eight longs" T-LABEL
   1 2 3 4 5 6 7 8 FFI-T-S32 9 FFI-T-BIGSTACK
   8 FFI-T-FOLD 1 -2 3 -4 FFI-T-H4 FFI-T-MIX 9 FFI-T-MIX T=

   s" a callee writing its parameter leaves the source intact" T-LABEL
   1 2 3 4 FFI-T-S32 FFI-T-S32!
   FFI-T-S32 FFI-T-MUTATE 1001 2 3 -4 FFI-T-H4 T=
   1 2 3 4 FFI-T-S32 FFI-T-S32=

   s" a parameter address held across a struct-calling callback" T-LABEL
   FFI-T-HOLD-CASE ;

: FFI-T-RESULT-SHAPES ( -- )
   s" a struct argument with no result and with a pointer result" T-LABEL
   7 -5 $FFFFFFF0 -9 FFI-T-S24 FFI-T-S24!
   FFI-T-S24 FFI-T-KEEP24
   FFI-T-KEPT 7 -5 $FFFFFFF0 -9 FFI-T-H4 T=
   21 22 FFI-T-S16 FFI-T-S16!
   FFI-T-S16 FFI-T-PARK16 {: parked:ptr :}
   parked LE:U64@ 21 T=  parked 8 + LE:U64@ 22 T=

   s" a 16-byte struct in and out by value" T-LABEL
   FFI-T-SR-RESET
   31 32 FFI-T-S16 FFI-T-S16!
   FFI-T-S16 FFI-T-SR-DST FFI-T-SWAP16
   FFI-T-SR-DST LE:U64@ 32 T=  FFI-T-SR-DST 8 + LE:U64@ 31 T=
   16 FFI-T-SR-CANARIES

   s" a copied struct argument beside an x8 result" T-LABEL
   FFI-T-SR-RESET
   1 -2 3 -4 FFI-T-S32 FFI-T-S32!
   100 FFI-T-S32 FFI-T-SR-DST FFI-T-ECHO32
   101 98 103 96 FFI-T-SR-DST FFI-T-S32=
   32 FFI-T-SR-CANARIES

   s" nine C arguments beside the destination spill the ninth" T-LABEL
   FFI-T-SR-RESET
   1 2 3 4 5 6 7 8 9 FFI-T-SR-DST FFI-T-RET9
   8 FFI-T-FOLD 9 1 8 FFI-T-SR-DST FFI-T-S32=
   32 FFI-T-SR-CANARIES ;

\ Sixteen record inputs whose payloads sum, modulo 2^64, to almost nothing.
: FFI-T-SA-WRAP$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s" FUNCTION: FFI-T-SA-WRAP hb_none ( " FFI-T-SRC+
   16 0 ?do s" ptr u8 " FFI-T-SRC+ loop
   s" -- )" FFI-T-SRC+
   15 0 ?do
      s"  " FFI-T-SRC+ i FFI-T-SRC CODEGEN:APPEND-DECIMAL
      s"  $7FFFFFFFFFFFFFFF STRUCT-ARG" FFI-T-SRC+
   loop
   s"  15 $7FFFFFFFFFFFFF7F STRUCT-ARG ;FUNCTION" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;

\ Declarations the declarer must refuse, each through the loader's evaluate.
: FFI-T-SA-REFUSALS ( -- )
   s" malformed struct-argument declarations are refused" T-LABEL
   [: s" 0 8 STRUCT-ARG" INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) 1 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) -1 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( n -- n ) 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) 0 0 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) 0 8 STRUCT-ARG 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) 0 8 WRITES-BYTES 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- n ) 0 8 STRUCT-ARG 0 8 WRITES-BYTES ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 n -- n ) 0 8 STRUCT-ARG 0 1 WRITES-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- ) 16 STRUCT-RESULT 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- ) 0 8 STRUCT-ARG 16 STRUCT-RESULT ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 n -- n ) 1 VARIADIC 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 n -- n ) 0 8 STRUCT-ARG 1 VARIADIC ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- r ) 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ
   [: s" FUNCTION: FFI-T-SAX hb_sum8 ( ptr u8 -- f32 ) 0 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-SYNTAX TTHROWSQ

   s" struct staging past sixteen cells is refused before publication" T-LABEL
   [: s" FUNCTION: FFI-T-SA-CAP hb_sum128 ( ptr u8 -- n ) 0 129 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ
   s" FUNCTION: FFI-T-SA-CAP hb_sum128 ( ptr u8 -- n ) 0 128 STRUCT-ARG ;FUNCTION"
   INCLUDE-EVALUATE
   [: s" FUNCTION: FFI-T-SA-CAP2 hb_none ( n n n n n n n n ptr u8 -- n ) 8 113 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ
   s" FUNCTION: FFI-T-SA-CAP2 hb_none ( n n n n n n n n ptr u8 -- n ) 8 112 STRUCT-ARG ;FUNCTION"
   INCLUDE-EVALUATE
   [: s" FUNCTION: FFI-T-SA-CAP3 hb_none ( r r r r r r r r r ptr u8 -- n ) 9 8 STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ
   [: s" FUNCTION: FFI-T-SA-CAP6 hb_none ( ptr u8 -- n ) 0 $7FFFFFFFFFFFFFFF STRUCT-ARG ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ
   [: FFI-T-SA-WRAP$ INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ
   s" a row's relocation mask names only that row's cells" T-LABEL
   [: s" getpid" FFI:PROCESS 0 0 $4000 FFI:DECLARE-ROW drop ;] E-FFI-ARITY TTHROWSQ
   [: s" getpid" FFI:PROCESS 0 0 1 FFI:DECLARE-ROW drop ;] E-FFI-ARITY TTHROWSQ
   s" sixteen inputs are the limit, a struct destination included" T-LABEL
   s" FUNCTION: FFI-T-SA-CAP4 hb_none ( n n n n n n n n n n n n n n n ptr u8 -- ) 32 STRUCT-RESULT ;FUNCTION"
   INCLUDE-EVALUATE
   [: s" FUNCTION: FFI-T-SA-CAP5 hb_none ( n n n n n n n n n n n n n n n n ptr u8 -- ) 32 STRUCT-RESULT ;FUNCTION"
      INCLUDE-EVALUATE ;] E-FFI-ARITY TTHROWSQ ;

: FFI-T-STRUCT-ARGS ( -- )
   FFI-T-REGISTER-ARGS
   FFI-T-COPY-ARGS
   FFI-T-RESULT-SHAPES
   FFI-T-SA-REFUSALS ;

FFI-T-STRUCT-ARGS
