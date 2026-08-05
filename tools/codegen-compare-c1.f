\ codegen-compare-c1.f - the clang reference column of the FIRST comparison.
\ One concern: which C twin stands for which of the pinned eleven, and on which
\ inputs.
\
\ EVERY ROW OF THE CORPUS HAS A TWIN. There is no gap list here and there cannot
\ be one: a C compiler expresses every shape this corpus has, so a missing row
\ would mean the reference was not written rather than that it could not be. The
\ scheduled test pins the count.
\
\ THE INPUTS ARE THE ENGINE COLUMN'S, VALUE FOR VALUE. Each case below hands the
\ twin the same numbers tools/codegen-compare-cases.f hands the corpus word, and
\ records the answers with the same VECTOR discipline, so the comparison of the
\ two rows' outputs is a statement about the two programs and not about two
\ different questions. Where the corpus word is measured over a byte span, the
\ twin is measured over its own copy of the same bytes - see the head of
\ tools/codegen-compare-clang.f for why the twins own their data - and the span's
\ address is fetched once, at PREPARE, because fetching it inside a timed body
\ would be timing a second foreign call.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-clang.f

package CODEGEN-C1

private

variable SUBJ                        \ where the twins' copy of the subject text lives

21 constant SUBJECT-LEN              \ "habu codegen baseline"
0 constant EMPTY-LEN
103 constant LETTER-G                \ present in the subject text
122 constant LETTER-Z                \ absent from it

: PREPARE ( -- )
   s" hc1_subject_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 SUBJ ! ;

: ADD3-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" hc1_add3" s" hf_i3"
   [: 1 2 3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop ;]
   [: 1 2 3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 CODEGEN-COMPARE:VECTOR
      -5 5 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: SQUARE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:SQUARE-SUM" s" hc1_square_sum" s" hf_i2"
   [: 3 4 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 drop ;]
   [: 3 4 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      -2 5 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: MAX2-CASE ( -- )
   s" CODEGEN-CORPUS:MAX2" s" hc1_max2" s" hf_i2"
   [: 3 4 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 drop ;]
   [: 3 4 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      9 -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: LERP-CASE ( -- )
   s" CODEGEN-CORPUS:LERP" s" hc1_lerp" s" hf_i3"
   [: 10 20 50 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop ;]
   [: 10 20 50 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 CODEGEN-COMPARE:VECTOR
      0 100 25 CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: SUM-TO-CASE ( -- )
   s" CODEGEN-CORPUS:SUM-TO" s" hc1_sum_to" s" hf_i1"
   [: 16 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 16 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: COUNT-DOWN-CASE ( -- )
   s" CODEGEN-CORPUS:COUNT-DOWN" s" hc1_count_down" s" hf_i1"
   [: 16 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 16 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: FACT-CASE ( -- )
   s" CODEGEN-CORPUS:FACT" s" hc1_fact" s" hf_i1"
   [: 10 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 10 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

\ The row whose point is a side effect. The twin bumps its own cell and the cell
\ is read back through the twin's own reader, exactly as the engine's row reads
\ the corpus's cell through the corpus's reader, so what the four recorded
\ numbers compare is a store and a load against a store and a load.
: CELL-BUMP-CASE ( -- )
   s" CODEGEN-CORPUS:CELL-BUMP" s" hc1_cell_bump" s" hf_i1"
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 drop ;]
   [: 7 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      s" hc1_bump_get" CODEGEN-CABI:FN CODEGEN-CABI:I0 CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CLANG:FN@ CODEGEN-CABI:I1 CODEGEN-COMPARE:VECTOR
      s" hc1_bump_get" CODEGEN-CABI:FN CODEGEN-CABI:I0 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: BYTE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-SUM" s" hc1_byte_sum" s" hf_i2"
   [: SUBJ @ SUBJECT-LEN CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 drop ;]
   [: SUBJ @ SUBJECT-LEN CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR
      SUBJ @ EMPTY-LEN CODEGEN-CLANG:FN@ CODEGEN-CABI:I2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

: BYTE-FIND-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-FIND" s" hc1_byte_find" s" hf_i3"
   [: SUBJ @ SUBJECT-LEN LETTER-G CODEGEN-CLANG:FN@ CODEGEN-CABI:I3 drop ;]
   [: SUBJ @ SUBJECT-LEN LETTER-G CODEGEN-CLANG:FN@ CODEGEN-CABI:I3
      CODEGEN-COMPARE:VECTOR
      SUBJ @ SUBJECT-LEN LETTER-Z CODEGEN-CLANG:FN@ CODEGEN-CABI:I3
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-CLANG:MEASURE ;

public

\ Every twin of this corpus, measured after the engine's column and the chain's.
\ A run without a reference column does nothing here and the report says why.
: RUN ( -- )
   CODEGEN-CLANG:PRESENT? 0= if exit then
   PREPARE
   CODEGEN-CLANG:CALIBRATE
   ADD3-CASE
   SQUARE-SUM-CASE
   MAX2-CASE
   LERP-CASE
   SUM-TO-CASE
   COUNT-DOWN-CASE
   FACT-CASE
   CELL-BUMP-CASE
   BYTE-SUM-CASE
   BYTE-FIND-CASE ;

;package
