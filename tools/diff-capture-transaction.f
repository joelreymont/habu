\ diff-capture-transaction.f - capture transaction and publication.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require tools/diff-capture-types.f
require tools/diff-capture-command.f
require tools/diff-capture-metadata.f
require tools/diff-capture-content.f
require tools/diff-capture-frame.f

package DIFF-TXN
private

create ROOT FS-PATH-CAP allot
variable ROOT-U
PTR-VARIABLE OUT-A
variable OUT-U
PTR-VARIABLE REPO-A
variable REPO-U
PTR-VARIABLE FROM-A
variable FROM-U
PTR-VARIABLE TO-A
variable TO-U
PTR-VARIABLE FRAME-A
variable FRAME-U
variable ROOT-READY
variable REPORT-READY
1 LAYOUT-BUFFER LAST-CAPTURE-V DIFF-CAPTURE:capture-outcome
variable LAST-PRIMARY-N
variable LAST-CLEANUP-N

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT u BYTE-COPY
   u ROOT-U ! ;

: LAST-CAPTURE-AT ( -- ptr DIFF-CAPTURE:capture-outcome )
   0 LAST-CAPTURE-V ;

: LAST-CAPTURE! ( DIFF-CAPTURE:capture-outcome -- )
   LAST-CAPTURE-AT ! ;

defer CAPTURE-JJ ( -- )

: RESET-CAPTURE-JJ ( -- )
   [: DIFF-CMD:JJ! ;] is CAPTURE-JJ ;

RESET-CAPTURE-JJ

defer CAPTURE-CLEAN ( -- )

: CAPTURE-CLEAN-DEFAULT ( -- )
   ROOT$ REMOVE-TREE ;

: RESET-CAPTURE-CLEAN ( -- )
   [: CAPTURE-CLEAN-DEFAULT ;] is CAPTURE-CLEAN ;

RESET-CAPTURE-CLEAN

defer CAPTURE-PUBLISH ( -- )

: CAPTURE-PUBLISH-DEFAULT ( -- )
   OUT-A @ OUT-U @ FRAME-A @ FRAME-U @ ATOMIC-WRITE-FILE ;

: RESET-CAPTURE-PUBLISH ( -- )
   [: CAPTURE-PUBLISH-DEFAULT ;] is CAPTURE-PUBLISH ;

RESET-CAPTURE-PUBLISH

: RESET-CAPTURE-RESULT ( -- )
   DIFF--CAPTURE-CAPTURE--OUTCOME:OK LAST-CAPTURE!
   0 LAST-PRIMARY-N !
   0 LAST-CLEANUP-N ! ;

: RESET-REPORT ( -- )
   true REPORT-READY !
   DIFF-CMD:RESET-REPORT
   RESET-CAPTURE-RESULT ;

: SET-CAPTURE-RESULT ( n n -- ) {: primary:n cleanup:n :}
   primary LAST-PRIMARY-N !
   cleanup LAST-CLEANUP-N !
   primary 0<> if
      cleanup 0<> if
         DIFF--CAPTURE-CAPTURE--OUTCOME:COMBINED-FAILED
      else
         DIFF--CAPTURE-CAPTURE--OUTCOME:PRIMARY-FAILED
      then
   else
      cleanup 0<> if
         DIFF--CAPTURE-CAPTURE--OUTCOME:CLEANUP-FAILED
      else
         DIFF--CAPTURE-CAPTURE--OUTCOME:OK
      then
   then
   LAST-CAPTURE! ;

: PREPARE-ROOT ( -- )
   s" habu-diff-capture" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu ROOT!
   true ROOT-READY !
   ROOT$ REPO-A @ REPO-U @ DIFF-CMD:CONFIGURE ;

: LOAD-METADATA ( -- )
   DIFF-CMD:META$ DIFF-META:LOAD
   DIFF-CMD:META$ DIFF-CMD:META-PATH$ DIFF-CONTENT:CONFIGURE ;

: BUILD-FRAME ( -- )
   DIFF-CMD:RAW$ DIFF-CMD:FROM$ DIFF-CMD:TO$ DIFF-FRAME:BUILD
   FRAME-U ! FRAME-A ! ;

: CAPTURE-BODY ( -- )
   CAPTURE-JJ
   DIFF-CMD:SNAPSHOT
   DIFF-CMD:BARRIER
   FROM-A @ FROM-U @ TO-A @ TO-U @ DIFF-CMD:RESOLVE-REVISIONS
   DIFF-CMD:CAPTURE-METADATA
   DIFF-CMD:CAPTURE-RAW
   DIFF-CMD:LOAD-CAPTURES
   LOAD-METADATA
   DIFF-CONTENT:PROVIDE
   BUILD-FRAME ;

: CLEAN-CODE ( -- n )
   ROOT-READY @ 0= if 0 exit then
   [: CAPTURE-CLEAN ;] catch dup 0= if false ROOT-READY ! then ;

: CAPTURE-PRIMARY ( -- )
   PREPARE-ROOT
   CAPTURE-BODY ;

: THROW-RESULT ( n n -- ) {: primary:n cleanup:n :}
   primary cleanup SET-CAPTURE-RESULT
   primary 0<> if primary throw then
   cleanup 0<> if cleanup throw then ;

: PUBLISH ( -- )
   [: CAPTURE-PUBLISH ;] catch {: code:n :}
   code 0<> if code 0 THROW-RESULT then ;

public

: CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: repo:ptr repou:n output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   false ROOT-READY !
   RESET-REPORT
   repo REPO-A ! repou REPO-U !
   output OUT-A ! outputu OUT-U !
   from FROM-A ! fromu FROM-U !
   to TO-A ! tou TO-U !
   [: CAPTURE-PRIMARY ;] catch {: primary:n :}
   CLEAN-CODE {: cleanup:n :}
   primary cleanup THROW-RESULT
   PUBLISH ;

: REPORT? ( -- bool )
   REPORT-READY @ if true else false then ;

: LAST-CAPTURE-OUTCOME ( -- DIFF-CAPTURE:capture-outcome )
   LAST-CAPTURE-AT @ ;

: LAST-PRIMARY ( -- n )
   LAST-PRIMARY-N @ ;

: LAST-CLEANUP ( -- n )
   LAST-CLEANUP-N @ ;

;package
