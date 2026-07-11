\ maki/eval-matrix.f - the durable eval-matrix report (package EVAL).
\
\ Assembles per-(task,target) rows from replayed/recorded transcript tallies
\ (maki/eval-transcript.f) and renders the eval matrix (a superset of the docs/eval-triton.md recorded table):
\ samples, first-attempt green, unbiased pass@1..3 (per-mille, exact via
\ maki/eval-passk.f), repair rounds, tokens-to-green, GB/s, and the device
\ verdict. The GB/s and device columns are HOST-HONEST: they default to
\ `not-run` and only carry values when an on-device run (tools/ptx/bandwidth.f
\ for GB/s; MAKI:GRADE-AUTHOR device goldens - Orin only) sets them through
\ MATRIX-GBS! / MATRIX-DEVICE!. Nothing on the host path fakes a device number.
\ Duplicate (task,target) rows and out-of-range writes fail closed.

require lib/errors.f
require lib/string.f
require maki/report.f          \ MAKI:V-PASS/V-FAIL/V-NOTRUN verdict vocabulary
require maki/eval-passk.f
require maki/eval-transcript.f

\ eval-matrix owns error codes -5268..-5271.
-5268 constant E-MX-CAP   \ row table or render buffer capacity exceeded
-5269 constant E-MX-IDX   \ row index out of range
-5270 constant E-MX-DUP   \ duplicate (task,target) row
-5271 constant E-MX-ARG   \ bad GB/s value or device verdict tag

package EVAL

16 constant MX-ROW-MAX
64 constant MX-NAME-CAP
$2000 constant MX-OUT-CAP

\ ---- row table -----------------------------------------------------------------
create MX-TASKS   MX-ROW-MAX MX-NAME-CAP * allot
create MX-TASK-US MX-ROW-MAX cells allot
create MX-TGTS    MX-ROW-MAX MX-NAME-CAP * allot
create MX-TGT-US  MX-ROW-MAX cells allot
8 constant MX-SLOTS
0 constant MXL-N        \ samples graded
1 constant MXL-GREEN    \ first-attempt passes (pass@k c)
2 constant MXL-REPAIRED \ green after >=1 repair round
3 constant MXL-ROUNDS   \ repair rounds over green replayed samples
4 constant MXL-TOKENS   \ tokens-to-green over green replayed samples
5 constant MXL-REC      \ externally recorded samples
6 constant MXL-GBS      \ measured GB/s x10, -1 = not-run
7 constant MXL-DEV      \ device verdict tag (MAKI:V-*), default not-run
create MX-TAB MX-ROW-MAX MX-SLOTS * cells allot
variable MX-ROW#

create MX-OUT MX-OUT-CAP allot
variable MX-OUT-U

: MX-SLOT ( n n -- ptr a ) {: row:n slot:n :}
   row MX-SLOTS * slot + cells MX-TAB + ;

: MX-CK-ROW ( n -- n ) {: row:n :}
   row 0 < row MX-ROW# @ >= or if E-MX-IDX throw then
   row ;

: MX-TASK-AT ( n -- ptr u8 n ) {: row:n :}
   row MX-NAME-CAP * MX-TASKS +  row cells MX-TASK-US + @ ;

: MX-TGT-AT ( n -- ptr u8 n ) {: row:n :}
   row MX-NAME-CAP * MX-TGTS +  row cells MX-TGT-US + @ ;

\ reject a duplicate (task,target) row
: MX-CK-DUP ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   0 begin dup MX-ROW# @ < while
      dup MX-TASK-AT a u STR=
      over MX-TGT-AT b v STR= and if E-MX-DUP throw then
      1+
   repeat drop ;

: MX-NAME! ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u MX-NAME-CAP > if E-MX-CAP throw then
   u 0= if E-MX-ARG throw then
   a dst u BYTE-COPY
   u lenp ! ;

\ append an empty row; returns its index
: MX-ADD ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   MX-ROW# @ MX-ROW-MAX >= if E-MX-CAP throw then
   a u b v MX-CK-DUP
   a u  MX-ROW# @ MX-NAME-CAP * MX-TASKS +  MX-ROW# @ cells MX-TASK-US +  MX-NAME!
   b v  MX-ROW# @ MX-NAME-CAP * MX-TGTS  +  MX-ROW# @ cells MX-TGT-US  +  MX-NAME!
   MX-SLOTS 0 ?do 0 MX-ROW# @ i MX-SLOT ! loop
   -1 MX-ROW# @ MXL-GBS MX-SLOT !
   MAKI:V-NOTRUN MX-ROW# @ MXL-DEV MX-SLOT !
   MX-ROW# @  MX-ROW# @ 1+ MX-ROW# ! ;

\ ---- render buffer (report.f OUT- pattern) ---------------------------------------
: MXO-RESET ( -- )  0 MX-OUT-U ! ;

: MXO-C ( n -- ) {: c:n :}
   MX-OUT-U @ MX-OUT-CAP >= if E-MX-CAP throw then
   c MX-OUT MX-OUT-U @ + c!
   MX-OUT-U @ 1+ MX-OUT-U ! ;

: MXO+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@ MXO-C  1+  repeat drop ;

: MXO-INT ( n -- )  SB-RESET SB-INT SB$ MXO+ ;
: MXO-NL ( -- )  $0A MXO-C ;
: MXO-SEP ( -- )  s"  | " MXO+ ;
: MXO$ ( -- ptr u8 n )  MX-OUT MX-OUT-U @ ;

\ ---- cells ---------------------------------------------------------------------
: MXR-PASSK ( n n -- )  \ row k: per-mille pass@k, `-` when k > n samples
   {: row:n k:n :}
   k row MXL-N MX-SLOT @ > if s" -" MXO+ exit then
   row MXL-N MX-SLOT @  row MXL-GREEN MX-SLOT @  k PASSK-X1000 MXO-INT ;

: MXR-GBS ( n -- ) {: row:n :}
   row MXL-GBS MX-SLOT @ dup 0 < if drop s" not-run" MXO+ exit then
   MXO-INT ;

: MXR-DEV ( n -- ) {: row:n :}
   row MXL-DEV MX-SLOT @
   dup MAKI:V-PASS = if drop s" pass" MXO+ exit then
   dup MAKI:V-FAIL = if drop s" fail" MXO+ exit then
   drop s" not-run" MXO+ ;

\ how the row was graded: live checker replay, recorded external verdicts, or both
: MXR-SRC ( n -- ) {: row:n :}
   row MXL-N MX-SLOT @ 0= if s" -" MXO+ exit then
   row MXL-REC MX-SLOT @ 0= if s" checker" MXO+ exit then
   row MXL-REC MX-SLOT @ row MXL-N MX-SLOT @ = if s" recorded" MXO+ exit then
   s" mixed" MXO+ ;

: MXR-HEADER ( -- )
   s" | task | target | n | green | pass@1-x1000 | pass@2-x1000 | pass@3-x1000 | repaired | repair-rounds | tokens-to-green | GB/s-x10 | device | graded |"
      MXO+ MXO-NL ;

: MXR-ROW ( n -- ) {: row:n :}
   s" | " MXO+
   row MX-TASK-AT MXO+  MXO-SEP
   row MX-TGT-AT MXO+   MXO-SEP
   row MXL-N MX-SLOT @ MXO-INT  MXO-SEP
   row MXL-GREEN MX-SLOT @ MXO-INT  MXO-SEP
   row 1 MXR-PASSK  MXO-SEP
   row 2 MXR-PASSK  MXO-SEP
   row 3 MXR-PASSK  MXO-SEP
   row MXL-REPAIRED MX-SLOT @ MXO-INT  MXO-SEP
   row MXL-ROUNDS MX-SLOT @ MXO-INT  MXO-SEP
   row MXL-TOKENS MX-SLOT @ MXO-INT  MXO-SEP
   row MXR-GBS  MXO-SEP
   row MXR-DEV  MXO-SEP
   row MXR-SRC
   s"  |" MXO+ MXO-NL ;

public

: MATRIX-RESET ( -- )  0 MX-ROW# ! ;

\ append one matrix row per replayed-transcript task, tagged with its target
: MATRIX-FROM-TS ( -- )
   TS-TASKS@ 0 ?do
      i TS-TASK$ TS-TARGET$ MX-ADD {: row:n :}
      i TS-N@        row MXL-N        MX-SLOT !
      i TS-GREEN@    row MXL-GREEN    MX-SLOT !
      i TS-REPAIRED@ row MXL-REPAIRED MX-SLOT !
      i TS-ROUNDS@   row MXL-ROUNDS   MX-SLOT !
      i TS-TOKENS@   row MXL-TOKENS   MX-SLOT !
      i TS-REC@      row MXL-REC      MX-SLOT !
   loop ;

: MATRIX-ROWS@ ( -- n )  MX-ROW# @ ;
: MATRIX-TASK$   ( n -- ptr u8 n )  MX-CK-ROW MX-TASK-AT ;
: MATRIX-TARGET$ ( n -- ptr u8 n )  MX-CK-ROW MX-TGT-AT ;
: MATRIX-N@      ( n -- n )  MX-CK-ROW MXL-N     MX-SLOT @ ;
: MATRIX-GREEN@  ( n -- n )  MX-CK-ROW MXL-GREEN MX-SLOT @ ;

\ device-run wiring (Orin): GB/s x10 from the bandwidth bench
: MATRIX-GBS! ( n n -- ) {: row:n gbs:n :}
   row MX-CK-ROW drop
   gbs 0 < if E-MX-ARG throw then
   gbs row MXL-GBS MX-SLOT ! ;

\ device-run wiring (Orin): golden verdict from the device grader
: MATRIX-DEVICE! ( n n -- ) {: row:n tag:n :}
   row MX-CK-ROW drop
   tag 0 < tag MAKI:V-N >= or if E-MX-ARG throw then
   tag row MXL-DEV MX-SLOT ! ;

\ the docs/eval-triton.md matrix schema, one table
: MATRIX-RENDER ( -- ptr u8 n )
   MXO-RESET
   MXR-HEADER
   MX-ROW# @ 0 ?do i MXR-ROW loop
   MXO$ ;

end-package
