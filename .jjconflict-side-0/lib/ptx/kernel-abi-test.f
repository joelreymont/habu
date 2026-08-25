\ kernel-abi-test.f - focused tests for the structured kernel-ABI record.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require lib/ptx/kernel-abi.f

package KABI-TEST

: SAXPY-ABI! ( -- )                     \ the default scaffolding record, declared explicitly
   KABI:RESET
   s" SAXPY" KABI:NAME!
   s" ceil-n-256" KABI:GRID!
   256 KABI:BLOCK!
   s" x" s" n" KABI:SPAN+
   s" y" s" n" KABI:SPAN+
   s" a" KABI:UNIFORM+ ;

: ROWS-ABI! ( -- )                      \ row-kernel matrices: rows launch-derived, stride dense
   KABI:RESET
   s" SOFTMAX_ROWS" KABI:NAME!
   s" extent-r" KABI:GRID!
   256 KABI:BLOCK!
   s" in" s" r" s" c" KABI:MATRIX+
   s" out" s" r" s" c" KABI:MATRIX+ ;

: INDEXED-ABI! ( -- )                   \ u32 index span + two data spans, two extents
   KABI:RESET
   s" INDEXED" KABI:NAME!
   s" ceil-n-256" KABI:GRID!
   256 KABI:BLOCK!
   s" idx" s" i" KABI:U32-SPAN+
   s" vals" s" i" KABI:SPAN+
   s" out" s" d" KABI:SPAN+ ;

\ --- negatives (each a named word for TTHROWS) ---

: KAT-UNKNOWN-FIELD ( -- )
   SAXPY-ABI!  s" zz" KABI:OFFSET-OF drop ;

: KAT-DERIVED-OFFSET ( -- )             \ launch-derived rows have no .param offset
   ROWS-ABI!  s" r" KABI:OFFSET-OF drop ;

: KAT-DUP-NAME ( -- )                   \ duplicate logical param name -> duplicate field
   KABI:RESET  s" K" KABI:NAME!
   s" x" s" n" KABI:SPAN+
   s" x" s" m" KABI:SPAN+
   KABI:TOTAL drop ;

: KAT-EXT-CONFLICT ( -- )               \ span extent (param) vs matrix rows (launch) on one token
   KABI:RESET  s" K" KABI:NAME!
   s" v" s" r" KABI:SPAN+
   s" m" s" r" s" c" KABI:MATRIX+
   KABI:TOTAL drop ;

: KAT-EMPTY-EXT ( -- )
   KABI:RESET  s" K" KABI:NAME!
   s" x" s" " KABI:SPAN+ ;

: KAT-EMPTY-NAME ( -- )
   KABI:RESET  s" " KABI:NAME! ;

: KAT-IDX-RANGE ( -- )
   SAXPY-ABI!  9 KABI:FIELD-OFF drop ;

create KAT-CAPN 1 allot

: KAT-OVERFLOW ( -- )                   \ MAX-LOGICAL is 16: the 17th param must throw
   KABI:RESET  s" K" KABI:NAME!
   17 0 ?do
      i 65 + KAT-CAPN c!
      KAT-CAPN 1 KABI:UNIFORM+
   loop ;

T-RESET

\ --- SAXPY record: fields, offsets, dedup, renders (the pinned historical ABI) ---
SAXPY-ABI!
KABI:NAME$ s" SAXPY" T$=
KABI:GRID$ s" ceil-n-256" T$=
KABI:BLOCK@ 256 T=
KABI:N-PARAMS 3 T=
0 KABI:PARAM-KIND KABI:KIND-SPAN T=
2 KABI:PARAM-KIND KABI:KIND-UNIFORM T=
0 KABI:PARAM-NAME$ s" x" T$=
0 KABI:PARAM-ELEM$ s" f32" T$=
0 KABI:PARAM-EXT$ s" n" T$=
0 KABI:PARAM-ALIGN 0 T=
KABI:N-FIELDS 4 T=                      \ x y a + ONE dedup'd n
0 KABI:FIELD-NAME$ s" x" T$=
1 KABI:FIELD-NAME$ s" y" T$=
2 KABI:FIELD-NAME$ s" a" T$=
3 KABI:FIELD-NAME$ s" n" T$=
0 KABI:FIELD-OFF 0 T=
1 KABI:FIELD-OFF 8 T=
2 KABI:FIELD-OFF 16 T=
3 KABI:FIELD-OFF 20 T=
0 KABI:FIELD-SIZE 8 T=
2 KABI:FIELD-SIZE 4 T=
0 KABI:FIELD-PTX$ s" .u64" T$=
2 KABI:FIELD-PTX$ s" .f32" T$=
3 KABI:FIELD-PTX$ s" .u32" T$=
0 KABI:FIELD-ROLE KABI:ROLE-BASE T=
2 KABI:FIELD-ROLE KABI:ROLE-SCALAR T=
3 KABI:FIELD-ROLE KABI:ROLE-LEN T=
3 KABI:FIELD-SRC KABI:SRC-PARAM T=
3 KABI:FIELD-EXT$ s" n" T$=
0 KABI:FIELD-PARAM? TTRUE
KABI:TOTAL 24 T=
KABI:U64-N 2 T=
KABI:F32-N 1 T=
KABI:U32-N 1 T=
0 KABI:FIELD-REG 1 T=                   \ x -> %rd1
1 KABI:FIELD-REG 2 T=                   \ y -> %rd2
2 KABI:FIELD-REG 1 T=                   \ a -> %f1
3 KABI:FIELD-REG 1 T=                   \ n -> %r1
s" x" KABI:OFFSET-OF 0 T=
s" n" KABI:OFFSET-OF 20 T=
s" y" KABI:SIZE-OF 8 T=
s" n" KABI:SIZE-OF 4 T=
s" x" KABI:FIELD-INDEX 0 T=
s" n" KABI:FIELD-INDEX 3 T=

\ renders are byte-equal to the historical hardcoded lines
KABI:ENTRY$ s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" T$=
0 KABI:LD-LINE$ s" ld.param.u64 %rd1, [p_x];" T$=
1 KABI:LD-LINE$ s" ld.param.u64 %rd2, [p_y];" T$=
2 KABI:LD-LINE$ s" ld.param.f32 %f1, [p_a];" T$=
3 KABI:LD-LINE$ s" ld.param.u32 %r1, [p_n];" T$=

\ determinism: redeclaring the same record derives the same layout
SAXPY-ABI!
KABI:TOTAL 24 T=
KABI:N-FIELDS 4 T=
KABI:ENTRY$ s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" T$=

\ --- matrix record: cols param'd + dedup'd, rows launch-derived, stride dense ---
ROWS-ABI!
KABI:N-PARAMS 2 T=
KABI:N-FIELDS 6 T=                      \ in out c | r | in-stride out-stride
0 KABI:FIELD-NAME$ s" in" T$=
1 KABI:FIELD-NAME$ s" out" T$=
2 KABI:FIELD-NAME$ s" c" T$=
3 KABI:FIELD-NAME$ s" r" T$=
4 KABI:FIELD-NAME$ s" in-stride" T$=
5 KABI:FIELD-NAME$ s" out-stride" T$=
2 KABI:FIELD-ROLE KABI:ROLE-COLS T=
3 KABI:FIELD-ROLE KABI:ROLE-ROWS T=
4 KABI:FIELD-ROLE KABI:ROLE-STRIDE T=
3 KABI:FIELD-SRC KABI:SRC-LAUNCH T=
4 KABI:FIELD-SRC KABI:SRC-DENSE T=
3 KABI:FIELD-PARAM? TFALSE
3 KABI:FIELD-OFF -1 T=                  \ launch-derived: no .param slot
4 KABI:FIELD-EXT$ s" c" T$=             \ dense stride equals the cols extent
s" in" KABI:OFFSET-OF 0 T=
s" out" KABI:OFFSET-OF 8 T=
s" c" KABI:OFFSET-OF 16 T=
KABI:TOTAL 20 T=
0 KABI:PARAM-EXT$ s" r" T$=
0 KABI:PARAM-EXT2$ s" c" T$=
KABI:ENTRY$ s" .visible .entry SOFTMAX_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_c)" T$=

\ --- u32 index span + two extents; ALIGN! refinement ---
INDEXED-ABI!
0 KABI:PARAM-ELEM$ s" u32" T$=
1 KABI:PARAM-ELEM$ s" f32" T$=
KABI:N-FIELDS 5 T=                      \ idx vals out + i d
s" idx" KABI:OFFSET-OF 0 T=
s" vals" KABI:OFFSET-OF 8 T=
s" out" KABI:OFFSET-OF 16 T=
s" i" KABI:OFFSET-OF 24 T=
s" d" KABI:OFFSET-OF 28 T=
KABI:TOTAL 32 T=
16 KABI:ALIGN!                          \ refine the LAST logical param (out)
2 KABI:PARAM-ALIGN 16 T=
0 KABI:PARAM-ALIGN 0 T=

\ --- negatives ---
' KAT-UNKNOWN-FIELD E-KABI-FIELD TTHROWS
' KAT-DERIVED-OFFSET E-KABI-FIELD TTHROWS
' KAT-DUP-NAME E-KABI-DUP TTHROWS
' KAT-EXT-CONFLICT E-KABI-DUP TTHROWS
' KAT-EMPTY-EXT E-KABI-TOKEN TTHROWS
' KAT-EMPTY-NAME E-KABI-TOKEN TTHROWS
' KAT-IDX-RANGE E-KABI-FIELD TTHROWS
' KAT-OVERFLOW E-KABI-CAP TTHROWS

\ leave the default scaffolding record installed for any later consumer
SAXPY-ABI!

T-REPORT

;package
