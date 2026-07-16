\ diff-report.f - structural failure-report delivery.

require lib/errors.f
require lib/prelude.f
require tools/lint/diff-error.f

package DIFF-REPORT
public

ENUM delivery
   unattempted
   delivered
   render-failed
   write-failed
;ENUM

private

create LF-BYTE $0A c,

variable CAPTURE-CODE-N
variable REPORT-CODE-N
variable RESULT-READY
1 LAYOUT-BUFFER OUTCOME-V delivery

variable WRITE-FD
PTR-VARIABLE WRITE-A
variable WRITE-U
variable WRITE-OFF
PTR-VARIABLE PENDING-A
variable PENDING-U

: OUTCOME-AT ( -- ptr delivery )
   0 OUTCOME-V ;

: OUTCOME! ( delivery -- )
   OUTCOME-AT ! ;

: SINK-DEFAULT ( n ptr u8 n -- n ) {: fd:n a:ptr u:n :}
   fd a u write ;

defer SINK ( n ptr u8 n -- n )

: RESET-SINK ( -- )
   [: SINK-DEFAULT ;] is SINK ;
RESET-SINK

: REQUIRE-RESULT ( -- )
   RESULT-READY @ 0= if E-DIFF-CAPTURE throw then ;

: REQUIRE-PENDING ( -- )
   REQUIRE-RESULT
   OUTCOME-AT @ MATCH delivery
      unattempted OF ENDOF
      delivered   OF E-DIFF-CAPTURE throw ENDOF
      render-failed OF E-DIFF-CAPTURE throw ENDOF
      write-failed  OF E-DIFF-CAPTURE throw ENDOF
   ;MATCH ;

: WRITE-LEFT ( -- n )
   WRITE-U @ WRITE-OFF @ - ;

: WRITE-DST ( -- ptr u8 )
   WRITE-A @ WRITE-OFF @ + ;

: WRITE-STEP ( -- )
   WRITE-LEFT {: left:n :}
   WRITE-FD @ WRITE-DST left SINK {: wrote:n :}
   wrote 0 <= wrote left > or if E-FS-IO throw then
   WRITE-OFF @ wrote + WRITE-OFF @ < if E-FS-IO throw then
   WRITE-OFF @ wrote + WRITE-OFF ! ;

: WRITE-FULL ( n ptr u8 n -- )
   {: fd:n a:ptr u:n :}
   u 0 < if E-FS-CAPACITY throw then
   fd WRITE-FD ! a WRITE-A ! u WRITE-U ! 0 WRITE-OFF !
   begin WRITE-OFF @ WRITE-U @ < while WRITE-STEP repeat ;

: WRITE-REPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u WRITE-FULL
   2 LF-BYTE 1 WRITE-FULL ;

: WRITE-PENDING ( -- )
   PENDING-A @ PENDING-U @ WRITE-REPORT ;

: SET-REPORT-CODE ( n -- ) {: code:n :}
   code 0= if E-DIFF-CAPTURE throw then
   code REPORT-CODE-N ! ;

public

: START ( n -- ) {: code:n :}
   false RESULT-READY !
   code 0= if E-DIFF-CAPTURE throw then
   code CAPTURE-CODE-N !
   0 REPORT-CODE-N !
   construct delivery unattempted OUTCOME!
   true RESULT-READY ! ;

: RECORD-FAILURE ( n -- n )
   REQUIRE-PENDING
   SET-REPORT-CODE
   construct delivery render-failed OUTCOME!
   CAPTURE-CODE-N @ ;

: DELIVER ( ptr u8 n -- n ) {: a:ptr u:n :}
   REQUIRE-PENDING
   a PENDING-A ! u PENDING-U !
   [: WRITE-PENDING ;] catch {: code:n :}
   code 0<> if
      code SET-REPORT-CODE
      construct delivery write-failed OUTCOME!
   else
      construct delivery delivered OUTCOME!
   then
   CAPTURE-CODE-N @ ;

: CAPTURE-CODE ( -- n )
   REQUIRE-RESULT
   CAPTURE-CODE-N @ ;

: REPORT-CODE ( -- n )
   REQUIRE-RESULT
   REPORT-CODE-N @ ;

: LAST-OUTCOME ( -- delivery )
   REQUIRE-RESULT
   OUTCOME-AT @ ;

;package
