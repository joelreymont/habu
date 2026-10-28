\ Actual captured cells, then byte-exact file/owned and malformed rows. The
\ harness loads before the tier selection: only the subject below it is compiled
\ at the requested tier (docs/gate.md
\ "How a suite runs, and what that demands of its files").
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/fs-mutate.f
require src/lib/engine-id.f
package NAMED-CELLS-TIER
: REQUESTED ( -- n )
   SCRIPT-ARGC 0= if tier@ exit then
   SCRIPT-ARGC 1 <> if 64 throw then
   0 SCRIPT-ARGV$ s" native" STR= 0= if 64 throw then
   1 ;
REQUESTED
;package
set-tier

package NAMED-CELLS-PREFIX
: ALIAS ( n -- n ) 3 + ;
public
EXPORT ALIAS
;package
package NAMED-CELLS-TEST
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require src/compiler/native/string.f
require src/lib/le.f

AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN
include test/aot-named-cells-window.f
package NAMED-CELLS-WINDOW
public
TYPED-VARIABLE ALIAS-SLOT [ n -- n ]
;package
AOT-ARM:WINDOW-CLOSE
include test/aot-named-cells-init.f
' NAMED-CELLS-PREFIX:ALIAS NAMED-CELLS-WINDOW:ALIAS-SLOT xt!

package NAMED-CELLS-TEST
using AOT-BUF
using AOT-WINDOW
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context
create ROOT FS-PATH-CAP allot variable ROOT-U
create ART FS-PATH-CAP allot variable ART-U
create BAD-ART FS-PATH-CAP allot variable BAD-ART-U
create OUT 4096 allot create ERR 4096 allot
DYNAMIC-BUFFER SAVED n
variable SAVED-N
DYNAMIC-BUFFER SAVED-NAMES n
variable SAVED-NAMES-U
variable NAME-ROW
variable ORIGINAL-META
variable BAD-META

: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: BAD-ART$ ( -- ptr u8 n ) BAD-ART BAD-ART-U @ ;
: ROW ( n -- ptr u8 ) XTOFF-ROW * XTOFF-BUF@ + ;
: META! ( n -- ) NAME-ROW @ ROW 4 + LE:U32! ;
: SAVE ( -- )
   XTOFF-N @ dup SAVED-N ! SAVED-RESERVE
   XTOFF-BUF@ 0 SAVED byte-view SAVED-N @ XTOFF-ROW * BYTE-COPY
   AOT-NAMES-LEN @ dup SAVED-NAMES-U ! CELL 1- + CELL / SAVED-NAMES-RESERVE
   AOT-NAMES-BUF@ 0 SAVED-NAMES byte-view SAVED-NAMES-U @ BYTE-COPY ;
: CHECK-ROWS ( -- )
   XTOFF-N @ SAVED-N @ T=
   XTOFF-BUF@ XTOFF-N @ XTOFF-ROW * 0 SAVED byte-view SAVED-N @ XTOFF-ROW * STR= TTRUE
   AOT-NAMES-BUF@ AOT-NAMES-LEN @ 0 SAVED-NAMES byte-view SAVED-NAMES-U @ STR= TTRUE ;
: RELEASE-SOURCE ( -- )
   0 XTOFF-N ! XTOFF-STORAGE-RELEASE
   0 AOT-NAMES-LEN ! AOT-NAMES-STORAGE-RELEASE ;
: READ ( -- ) KEY ART$ AOT-FILE:READ ;
: TRANSFER ( AOT-OWNED:capture -- )
   RELEASE-SOURCE
   dup AOT-FILE:IMPORT CHECK-ROWS AOT-OWNED:CLOSE ;
: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE ;

: FIND-NAMES ( -- )
   -1 NAME-ROW ! 0
   XTOFF-N @ 0 ?do
      i ROW 4 + LE:U32@ {: meta:n :}
      meta XTOFF-KIND-MASK and XTOFF-NAME-TAG = if
         i NAME-ROW ! meta ORIGINAL-META ! 1+
      then
   loop
   4 T= NAME-ROW @ 0 >= TTRUE ;

: HAS-NAME? ( ptr u8 n -- bool ) {: text:ptr size:n :}
   XTOFF-N @ 0 ?do
      i ROW 4 + LE:U32@ {: meta:n :}
      meta XTOFF-KIND-MASK and XTOFF-NAME-TAG = if
         meta XTOFF-VALUE-MASK and 1- AOT-NAMES-BUF@ +
         dup 1+ swap c@ text size STR= if true unloop exit then
      then
   loop false ;

: NAMES ( -- )
   FIND-NAMES
   s" 0<>" HAS-NAME? TTRUE
   s" CODE-RECLAIM:FLOOR-FROM" HAS-NAME? TTRUE
   s" DEFER-UNSET" HAS-NAME? TTRUE
   s" NAMED-CELLS-PREFIX:ALIAS" HAS-NAME? TTRUE
   \ An unused empty entry is legal pool content but cannot name a target.
   AOT-NAMES-LEN @ 1+ AOT-NAMES-RESERVE
   0 AOT-NAMES-BUF@ AOT-NAMES-LEN @ + c! 1 AOT-NAMES-LEN +! ;

public
: BAD-READ ( -- ) BAD-META @ META! KEY BAD-ART$ AOT-FILE:WRITE KEY BAD-ART$ AOT-FILE:READ ;
: BAD-MERGE ( -- )
   BAD-META @ META! KEY BAD-ART$ AOT-FILE:WRITE
   ORIGINAL-META @ META! 0 AOT-REG-LEN ! KEY BAD-ART$ AOT-FILE:MERGE ;
: TRUNCATE-POOL ( -- ) 255 AOT-NAMES-BUF@ AOT-NAMES-LEN @ + 1- c! ;
: TRUNCATED-READ ( -- )
   TRUNCATE-POOL KEY BAD-ART$ AOT-FILE:WRITE KEY BAD-ART$ AOT-FILE:READ ;
: TRUNCATED-MERGE ( -- )
   TRUNCATE-POOL KEY BAD-ART$ AOT-FILE:WRITE
   0 AOT-NAMES-BUF@ AOT-NAMES-LEN @ + 1- c!
   0 AOT-REG-LEN ! KEY BAD-ART$ AOT-FILE:MERGE ;

private
: REFUSE ( ptr u8 n n ptr u8 n -- )
   {: source:ptr sourceu:n code:n text:ptr size:n :}
   source sourceu T-LABEL
   source sourceu OUT 4096 >LEN ERR 4096 >LEN 5000 >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   source sourceu OUT outu LEN>N ERR erru LEN>N oc code T-OUTCOME-EXITED=
   OUT outu LEN>N text size CONTAINS? ERR erru LEN>N text size CONTAINS? or
   dup 0= if
      s" named-cells: refused source:" type cr source sourceu type cr
      s" stdout:" type cr OUT outu LEN>N type cr
      s" stderr:" type cr ERR erru LEN>N type cr
   then TTRUE
   CHECK-ROWS ;
: REFUSE-IO ( n ptr u8 n -- ) {: meta:n text:ptr size:n :}
   meta BAD-META !
   s" NAMED-CELLS-TEST:BAD-READ" 75 text size REFUSE
   s" NAMED-CELLS-TEST:BAD-MERGE" 75 text size REFUSE ;
: REFUSALS ( -- )
   XTOFF-KIND-MASK 1+ s" invalid target kind" REFUSE-IO
   XTOFF-NAME-TAG s" name is not a pool entry" REFUSE-IO
   XTOFF-NAME-TAG AOT-NAMES-LEN @ + 1+ s" name is not a pool entry" REFUSE-IO
   XTOFF-NAME-TAG AOT-NAMES-LEN @ + s" name is not a pool entry" REFUSE-IO
   ORIGINAL-META @ 1+ s" name is not a pool entry" REFUSE-IO
   s" NAMED-CELLS-TEST:TRUNCATED-READ" 75 s" name pool ends inside an entry" REFUSE
   s" NAMED-CELLS-TEST:TRUNCATED-MERGE" 75 s" name pool ends inside an entry" REFUSE ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   s" habu-named-cells" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT size BYTE-COPY size ROOT-U ! path size CLEANUP-TREE+
   path size s" window.aot" ART JOIN-PATH ART-U !
   path size s" malformed.aot" BAD-ART JOIN-PATH BAD-ART-U !
   FSHA-CTX ENGINE-ID:PATH$ KEY SHA256-FILE-IN 0 T=
   NAMED-CELLS-WINDOW:CHECK
   7 NAMED-CELLS-WINDOW:ALIAS-SLOT @ execute 10 T=
   CAPTURE NAMES SAVE
   AOT-IDENT:RESET s" test/aot-named-cells-window.f" AOT-IDENT:PATH+
   AOT-FILE:OWN TRANSFER
   KEY ART$ AOT-FILE:WRITE
   RELEASE-SOURCE READ CHECK-ROWS
   REFUSALS
   SAVED-RELEASE SAVED-NAMES-RELEASE CLEANUP-RUN T-REPORT ;
' RUN
;using
;using
;package
execute
