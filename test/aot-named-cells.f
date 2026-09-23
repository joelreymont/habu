\ Actual captured cells, then byte-exact file/owned/merge and malformed rows.
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
: HIDDEN ( n -- n ) 4 + ;
public
EXPORT ALIAS
: HIDDEN-XT ( -- [ n -- n ] ) ['] HIDDEN ;
;package
package NAMED-CELLS-TEST
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/fs-mutate.f
require lib/engine-id.f
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

package NAMED-CELLS-PRELUDE
public
: VALUE ( n -- n ) 5 + ;
;package
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

package AOT-FILE
public
: NAMED-TEST-META! ( n n AOT-OWNED:capture -- AOT-OWNED:capture )
   {: index:n meta:n capture :}
   capture AOT-OWNED:BYTES$ drop {: bytes:ptr :}
   bytes S-XTOFFS ROW-BYTES * + U64@ bytes + index AOT-WINDOW:XTOFF-ROW * + 4 +
   meta swap U32!
   capture ;

: NAMED-TEST-TRUNCATE ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup AOT-OWNED:BYTES$ drop {: bytes:ptr :}
   bytes S-NAMES ROW-BYTES * + {: row:ptr :}
   bytes row U64@ + row 8 + U64@ + 1- 255 swap c! ;
;package

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
: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: U32! ( n ptr u8 -- ) {: value:n p:ptr :}
   4 0 ?do value i 8 * rshift p i + c! loop ;
: META! ( n -- ) NAME-ROW @ ROW 4 + U32! ;
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
      i ROW 4 + U32@ {: meta:n :}
      meta XTOFF-KIND-MASK and XTOFF-NAME-TAG = if
         i NAME-ROW ! meta ORIGINAL-META ! 1+
      then
   loop
   4 T= NAME-ROW @ 0 >= TTRUE ;

: HAS-NAME? ( ptr u8 n -- bool ) {: text:ptr size:n :}
   XTOFF-N @ 0 ?do
      i ROW 4 + U32@ {: meta:n :}
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
: RECAPTURE ( -- ) CAPTURE ;
: TARGET! ( n -- ) NAMED-CELLS-WINDOW:GLOBAL-SLOT byte-view cell-view ! ;
: UNKNOWN ( -- ) $123 TARGET! CAPTURE ;
: NONENTRY ( -- ) s" 0<>" XREF-FIND XREF-START 1+ TARGET! CAPTURE ;
: PRIVATE-TARGET ( -- )
   NAMED-CELLS-PREFIX:HIDDEN-XT NAMED-CELLS-WINDOW:LOCAL-SLOT xt! CAPTURE ;
: PRELUDE-TARGET ( -- )
   ['] NAMED-CELLS-PRELUDE:VALUE NAMED-CELLS-WINDOW:LOCAL-SLOT xt! CAPTURE ;
: BAD-OWN ( -- ) BAD-META @ META! AOT-FILE:OWN AOT-OWNED:CLOSE ;
: BAD-IMPORT ( -- )
   NAME-ROW @ BAD-META @ AOT-FILE:OWN AOT-FILE:NAMED-TEST-META!
   dup AOT-FILE:IMPORT AOT-OWNED:CLOSE ;
: BAD-READ ( -- ) BAD-META @ META! KEY BAD-ART$ AOT-FILE:WRITE KEY BAD-ART$ AOT-FILE:READ ;
: BAD-MERGE ( -- )
   BAD-META @ META! KEY BAD-ART$ AOT-FILE:WRITE
   ORIGINAL-META @ META! 0 AOT-REG-LEN ! KEY BAD-ART$ AOT-FILE:MERGE ;
: TRUNCATE-POOL ( -- ) 255 AOT-NAMES-BUF@ AOT-NAMES-LEN @ + 1- c! ;
: TRUNCATED-OWN ( -- ) TRUNCATE-POOL AOT-FILE:OWN AOT-OWNED:CLOSE ;
: TRUNCATED-IMPORT ( -- )
   AOT-FILE:OWN AOT-FILE:NAMED-TEST-TRUNCATE dup AOT-FILE:IMPORT AOT-OWNED:CLOSE ;
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
   code T-OUTCOME-EXITED= {: outu:len erru:len :}
   OUT outu LEN>N text size CONTAINS? ERR erru LEN>N text size CONTAINS? or
   dup 0= if
      s" named-cells: refused source:" type cr source sourceu type cr
      s" stdout:" type cr OUT outu LEN>N type cr
      s" stderr:" type cr ERR erru LEN>N type cr
   then TTRUE
   CHECK-ROWS ;
: REFUSE-CAPTURE ( ptr u8 n -- )
   74 s" aot-capture: declared address target is not self-contained" REFUSE ;
: REFUSE-IO ( n ptr u8 n -- ) {: meta:n text:ptr size:n :}
   meta BAD-META !
   s" NAMED-CELLS-TEST:BAD-OWN" 75 text size REFUSE
   s" NAMED-CELLS-TEST:BAD-IMPORT" 75 text size REFUSE
   s" NAMED-CELLS-TEST:BAD-READ" 75 text size REFUSE
   s" NAMED-CELLS-TEST:BAD-MERGE" 75 text size REFUSE ;
: REFUSALS ( -- )
   s" NAMED-CELLS-TEST:UNKNOWN" REFUSE-CAPTURE
   s" NAMED-CELLS-TEST:NONENTRY" REFUSE-CAPTURE
   s" NAMED-CELLS-TEST:PRIVATE-TARGET" REFUSE-CAPTURE
   s" NAMED-CELLS-TEST:PRELUDE-TARGET" REFUSE-CAPTURE
   S\" package NAMED-CELLS-PREFIX\npublic\nundefine ALIAS\n: ALIAS ( n -- n ) 30 + ;\n;package\nNAMED-CELLS-TEST:RECAPTURE\n" REFUSE-CAPTURE
   XTOFF-KIND-MASK 1+ s" invalid target kind" REFUSE-IO
   XTOFF-NAME-TAG s" name is not a pool entry" REFUSE-IO
   XTOFF-NAME-TAG AOT-NAMES-LEN @ + 1+ s" name is not a pool entry" REFUSE-IO
   XTOFF-NAME-TAG AOT-NAMES-LEN @ + s" name is not a pool entry" REFUSE-IO
   ORIGINAL-META @ 1+ s" name is not a pool entry" REFUSE-IO
   s" NAMED-CELLS-TEST:TRUNCATED-OWN" 75 s" name pool ends inside an entry" REFUSE
   s" NAMED-CELLS-TEST:TRUNCATED-IMPORT" 75 s" name pool ends inside an entry" REFUSE
   s" NAMED-CELLS-TEST:TRUNCATED-READ" 75 s" name pool ends inside an entry" REFUSE
   s" NAMED-CELLS-TEST:TRUNCATED-MERGE" 75 s" name pool ends inside an entry" REFUSE ;

: MERGE ( -- )
   \ Keep one copy of the identical registry, then append behind a nonempty
   \ name pool. This is a byte courier check, not execution of duplicate words.
   AOT-NAMES-LEN @ {: names:n :}
   AOT-BLOB-LEN @ {: blob:n :}
   0 AOT-REG-LEN ! KEY ART$ AOT-FILE:MERGE
   XTOFF-N @ SAVED-N @ 2 * T=
   AOT-NAMES-LEN @ names 2 * T=
   AOT-NAMES-BUF@ names 0 SAVED-NAMES byte-view names STR= TTRUE
   AOT-NAMES-BUF@ names + names 0 SAVED-NAMES byte-view names STR= TTRUE
   XTOFF-BUF@ SAVED-N @ XTOFF-ROW * 0 SAVED byte-view SAVED-N @ XTOFF-ROW * STR= TTRUE
   SAVED-N @ 0 ?do
      0 SAVED byte-view i XTOFF-ROW * + {: old:ptr :}
      old 4 + U32@ {: meta:n :}
      meta XTOFF-VALUE-MASK and 0= if meta else
         meta XTOFF-NAME-TAG and 0<> if meta names + else
            meta XTOFF-DATA-TAG and 0<> if meta AOT-FILE:WDATA-BASE + else meta blob + then
         then
      then {: want:n :}
      SAVED-N @ i + ROW 4 + U32@ want T=
      old U32@ {: loc:n :}
      loc XTOFF-WINDOW-TAG and 0<> if loc AOT-FILE:WDATA-BASE + else loc then
      SAVED-N @ i + ROW U32@ T=
   loop ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   s" habu-named-cells" HB-TMP-MKDIR {: path:ptr size:n :}
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
   REFUSALS MERGE
   SAVE AOT-FILE:OWN TRANSFER
   KEY ART$ AOT-FILE:WRITE RELEASE-SOURCE READ CHECK-ROWS
   SAVED-RELEASE SAVED-NAMES-RELEASE CLEANUP-RUN T-REPORT ;
' RUN
;using
;using
;package
execute
