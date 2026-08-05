\ codegen-compare-c2.f - the clang reference column of the SECOND comparison.
\ One concern: which C twin stands for which of the pinned seven, and on which
\ inputs.
\
\ The inputs are tools/codegen-compare-cases2.f's, value for value, and the two
\ analogue rows are twinned as analogues: the walk over a binding table and the
\ copy between two buffers are the same programs over the twins' own storage,
\ filled from the same constants by hc2_setup.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-clang.f

package CODEGEN-C2

private

variable SUBJ                        \ the twins' copy of the byte span COUNT-CHAR scans
variable COPY-SRC
variable COPY-DST

11 constant SUBJECT-LEN              \ "aha aha aha"
0 constant EMPTY-LEN
97 constant LETTER-A                 \ present at both ends and between
122 constant LETTER-Z                \ absent

32 constant WS-SPACE
9 constant WS-TAB
10 constant WS-LF
13 constant WS-CR
97 constant NOT-WS

64 constant BELOW-A                  \ one under the fold's lower bound
65 constant EXACTLY-A
90 constant EXACTLY-Z
91 constant ABOVE-Z                  \ one over its upper bound
97 constant ALREADY-LOWER

\ The three terms the walk is measured on, spelled as the numbers the corpus's
\ own CHAIN-HEAD, NOT-A-VAR and UNBOUND-VAR construct: a payload shifted up three
\ bits with a tag in the low three.
9 constant CHAIN-HEAD                \ payload 1, tag 1
24 constant NOT-A-VAR                \ payload 3, tag 0
33 constant UNBOUND-VAR              \ payload 4, tag 1

4 constant COPY-LEN

: PREPARE ( -- )
   s" hc2_subject_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 SUBJ !
   s" hc2_copy_src_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 COPY-SRC !
   s" hc2_copy_dst_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 COPY-DST ! ;

: DST@ ( n -- n )
   s" hc2_copy_dst_get" CODEGEN-CABI:FN CODEGEN-CABI:I1 ;

: TAG-CASE ( -- )
   s" CODEGEN-CORPUS2:TAG" s" hc2_tag" s" hf_i1"
   [: 9 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 9 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      24 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

\ The twin answers 1 and 0 where the corpus word answers a habu flag, and the
\ engine's row records that flag through VECTOR-FLAG, which turns it into 1 and
\ 0. The two rows therefore hold the same numbers by construction rather than by
\ a coincidence of representations.
: WS-CASE ( -- )
   s" CODEGEN-CORPUS2:WS?" s" hc2_ws" s" hf_i1"
   [: WS-SPACE CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: WS-SPACE CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      WS-TAB CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      WS-LF CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      WS-CR CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      NOT-WS CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: SYM-FOLD-CASE ( -- )
   s" CODEGEN-CORPUS2:SYM-FOLD-C" s" hc2_sym_fold_c" s" hf_i1"
   [: EXACTLY-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: BELOW-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      EXACTLY-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      EXACTLY-Z CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      ABOVE-Z CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      ALREADY-LOWER CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: MAX-DIM-CASE ( -- )
   s" CODEGEN-CORPUS2:MAX-DIM" s" hc2_max_dim" s" hf_i2"
   [: 3 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 drop ;]
   [: 3 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      7 3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      5 5 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: COUNT-CHAR-CASE ( -- )
   s" CODEGEN-CORPUS2:COUNT-CHAR" s" hc2_count_char" s" hf_i3"
   [: SUBJ @ SUBJECT-LEN LETTER-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop ;]
   [: SUBJ @ SUBJECT-LEN LETTER-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I3
      CODEGEN-COMPARE:VECTOR
      SUBJ @ SUBJECT-LEN LETTER-Z CODEGEN-CLANG:FN@ CODEGEN-CABI:I3
      CODEGEN-COMPARE:VECTOR
      SUBJ @ EMPTY-LEN LETTER-A CODEGEN-CLANG:FN@ CODEGEN-CABI:I3
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: T-RES-WALK-CASE ( -- )
   s" CODEGEN-CORPUS2:T-RES-WALK" s" hc2_t_res_walk" s" hf_i1"
   [: CHAIN-HEAD CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: CHAIN-HEAD CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      NOT-A-VAR CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      UNBOUND-VAR CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

\ The row whose point is a side effect: what the copy returns is nothing, so the
\ four cells it moved and the fifth it must not have are what the row records.
: VEC-COPY-CASE ( -- )
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" s" hc2_vec_copy_cells" s" hf_i3"
   [: COPY-SRC @ COPY-DST @ COPY-LEN CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop ;]
   [: COPY-SRC @ COPY-DST @ COPY-LEN CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop
      0 DST@ CODEGEN-COMPARE:VECTOR
      1 DST@ CODEGEN-COMPARE:VECTOR
      2 DST@ CODEGEN-COMPARE:VECTOR
      3 DST@ CODEGEN-COMPARE:VECTOR
      4 DST@ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

public

: RUN ( -- )
   CODEGEN-CLANG:PRESENT? 0= if exit then
   PREPARE
   CODEGEN-CLANG:CALIBRATE
   TAG-CASE
   WS-CASE
   SYM-FOLD-CASE
   MAX-DIM-CASE
   COUNT-CHAR-CASE
   T-RES-WALK-CASE
   VEC-COPY-CASE ;

;package
