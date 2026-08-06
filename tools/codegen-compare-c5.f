\ codegen-compare-c5.f - the clang reference column of the FIFTH comparison.
\ One concern: which C twin stands for which of the pinned six, and on which
\ inputs.
\
\ THIS IS THE CORPUS ABOUT ONE DECISION - what a compiler emits for a call in
\ tail position - so the reference column is the answer to "is this a hard thing
\ to do, or only one we have not done". Every row here is a placement, clang is
\ handed the same placement in C, and what it emits for each is the third
\ column. Nothing forces or forbids a tail branch: -O2 is what the build passes,
\ and whether clang takes each of these is the measurement.
\
\ TAIL-PAIR IS THE ONE ROW WHOSE ANSWER DOES NOT FIT IN A RETURN VALUE. The habu
\ word leaves two values; the twin computes the pair once, returns the top one
\ and leaves the other where hc5_pair_deep reads it, so this column records the
\ same two values in the same order as the other two. It is the discipline
\ tools/codegen-compare-c4.f's STORE-LOAD row already uses for an answer that
\ lives somewhere other than the return register, and the head of
\ tools/clang/twins.c says it beside the twin.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-clang.f

package CODEGEN-C5

private

\ The deeper of the pair row's two results, read back the way the fourth
\ corpus's step cell is: through an accessor of the twins' own.
: PAIR-DEEP ( -- n )
   s" hc5_pair_deep" CODEGEN-CABI:FN CODEGEN-CABI:I0 ;

: TAIL-BIG-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-BIG" s" hc5_tail_big" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: TAIL-WORK-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-WORK" s" hc5_tail_work" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: NONTAIL-CASE ( -- )
   s" CODEGEN-CORPUS5:NONTAIL" s" hc5_nontail" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: TAIL-MID-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-MID" s" hc5_tail_mid" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: TAIL-CHAIN-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-CHAIN" s" hc5_tail_chain" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

\ Two values a call, recorded in the order the habu columns leave them: the
\ returned one first, then the one the accessor reads back.
: TAIL-PAIR-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-PAIR" s" hc5_tail_pair" s" hf_i2"
   [: 7 3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 drop ;]
   [: 7 3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      PAIR-DEEP CODEGEN-COMPARE:VECTOR
      0 0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      PAIR-DEEP CODEGEN-COMPARE:VECTOR
      -1 5 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      PAIR-DEEP CODEGEN-COMPARE:VECTOR
      255 -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      PAIR-DEEP CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: TAIL-AFTER-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-AFTER" s" hc5_tail_after" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

public

: RUN ( -- )
   CODEGEN-CLANG:PRESENT? 0= if exit then
   CODEGEN-CLANG:CALIBRATE
   TAIL-BIG-CASE
   TAIL-WORK-CASE
   NONTAIL-CASE
   TAIL-MID-CASE
   TAIL-CHAIN-CASE
   TAIL-PAIR-CASE
   TAIL-AFTER-CASE ;

;package
