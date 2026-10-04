\ Two real native x64 capture windows compose through a verified v15 artifact.
\ Run on a native x86-64 engine: bin/hb --load test/aot-native-merge.f

package AOT-NATIVE-MERGE
public
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

require lib/test.f
require lib/string.f
require lib/le.f
require lib/fs.f
require lib/fs-mutate.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/habu/aot-owned.f
require src/habu/aot-ident.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/native/string.f
require src/arch/x86-64/passes.f

package AOT-NATIVE-MERGE
using AOT-BUF
using AOT-WINDOW

create KEY 32 allot
create DIR FS-PATH-CAP allot variable DIR-U
create HOST FS-PATH-CAP allot variable HOST-U
create PART FS-PATH-CAP allot variable PART-U
create DONE FS-PATH-CAP allot variable DONE-U
create FIRST 32 allot

variable H-RECS variable H-SHRECS variable H-SHCODE
variable H-SHSITES variable H-SHXTS variable H-XTOFFS variable H-WIDS
variable P-RECS variable P-SHRECS variable P-SHCODE
variable P-SHSITES variable P-SHXTS variable P-XTOFFS
variable P-CALLSITE variable P-CALL-OFF variable P-CODESITE variable P-NAMEDCELL

CAST: DATA-N ( ptr u8 -- n )

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * +
   8 + LE:U32@ AOT-NAMES-BUF@ + {: p:ptr :}
   p 1+ p c@ ;

: SHIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   AOT-REC-N @ 0 ?do
      i NAME$ a u STR= if i unloop exit then
   loop -1 ;

: SITE ( n -- ptr u8 ) AOT-SHADOW:SITE-ROW * AOT-SHADOW:SITE-BUF@ + ;
: XT-ROW ( n -- ptr u8 ) AOT-WINDOW:XTOFF-ROW * XTOFF-BUF@ + ;

: TARGET-NAME$ ( n -- ptr u8 n ) {: t:n :}
   AOT-NAMES-BUF@ t SITE-TARGET-MASK and + {: p:ptr :}
   p 1+ p c@ ;

: SITE-NAMED ( n ptr u8 n -- n ) {: kind:n a:ptr u:n :}
   AOT-SHADOW:SITE-N @ 0 ?do
      i SITE 4 + LE:U32@ kind = if
         i SITE 8 + LE:U32@ {: t:n :}
         t SITE-TARGET-MASK invert and SITE-NAME-TAG = if
            t TARGET-NAME$ a u STR= if i unloop exit then
         then
      then
   loop -1 ;

: CELL-NAMED ( ptr u8 n -- n ) {: a:ptr u:n :}
   XTOFF-N @ 0 ?do
      i XT-ROW 4 + LE:U32@ {: meta:n :}
      meta XTOFF-KIND-MASK and XTOFF-NAME-TAG = if
         AOT-NAMES-BUF@ meta XTOFF-VALUE-MASK and 1- + {: p:ptr :}
         p 1+ p c@ a u STR= if i unloop exit then
      then
   loop -1 ;

public
: OPEN ( -- ) NABI:BINDING NSHADOW:OPEN-NATIVE AOT-ARM:WINDOW-OPEN NSTR:WINDOW-OPEN ;
private

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$
   ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
   AOT-CAPTURE:CAPTURE-NATIVE
   AOT-CAPTURE:SHADOW-CAPTURE
   NSHADOW:CLOSE ;

: IDENT! ( -- )
   32 0 ?do 0 KEY i + c! loop
   AOT-IDENT:RESET s" test/aot-native-merge.f" AOT-IDENT:PATH+ ;

: FILES! ( -- )
   s" habu-aot-native-merge" HB-TMP-MKDIR {: p:ptr u:n :}
   p u CLEANUP-TREE+
   p DIR u BYTE-COPY u DIR-U !
   DIR DIR-U @ s" host.aot" HOST JOIN-PATH HOST-U !
   DIR DIR-U @ s" partial.aot" PART JOIN-PATH PART-U !
   DIR DIR-U @ s" merged.aot" DONE JOIN-PATH DONE-U ! ;

public
: PREPARE ( -- ) T-RESET CLEANUP-RESET FILES! ;

: HOST-CAPTURE ( -- )
   CAPTURE
   AOT-REC-N @ H-RECS !
   AOT-SHADOW:REC-N @ H-SHRECS !
   AOT-SHADOW:CODE-LEN @ H-SHCODE !
   AOT-SHADOW:SITE-N @ H-SHSITES !
   AOT-SHADOW:XT-N @ H-SHXTS !
   XTOFF-N @ H-XTOFFS !
   AOT-WID-SPAN @ H-WIDS !
   IDENT! KEY HOST HOST-U @ AOT-FILE:WRITE
   ndict@ PRE-R ! here DATA-N PRE-D ! ;

: PART-CAPTURE ( -- )
   CAPTURE
   AOT-REC-N @ P-RECS !
   AOT-SHADOW:REC-N @ P-SHRECS !
   AOT-SHADOW:CODE-LEN @ P-SHCODE !
   AOT-SHADOW:SITE-N @ P-SHSITES !
   AOT-SHADOW:XT-N @ P-SHXTS !
   XTOFF-N @ P-XTOFFS !
   AOT-SHADOW:CALL s" AOT-MERGE-BASE:LEAF" SITE-NAMED P-CALLSITE !
   P-CALLSITE @ 0 >= if P-CALLSITE @ SITE LE:U32@ P-CALL-OFF ! then
   AOT-SHADOW:CODE s" AOT-MERGE-BASE:LEAF" SITE-NAMED P-CODESITE !
   s" AOT-MERGE-BASE:LEAF" CELL-NAMED P-NAMEDCELL !
   P-CALLSITE @ 0 >= TTRUE
   P-CODESITE @ 0 >= TTRUE
   P-NAMEDCELL @ 0 >= TTRUE
   IDENT! KEY PART PART-U @ AOT-FILE:WRITE ;

private

: CHECK ( -- )
   AOT-REC-N @ H-RECS @ P-RECS @ + T=
   AOT-SHADOW:REC-N @ H-SHRECS @ P-SHRECS @ + T=
   AOT-SHADOW:CODE-LEN @ H-SHCODE @ P-SHCODE @ + T=
   AOT-SHADOW:SITE-N @ H-SHSITES @ P-SHSITES @ + T=
   AOT-WID-SPAN @ H-WIDS @ > TTRUE
   H-SHSITES @ P-CALLSITE @ + SITE 0 + LE:U32@
      H-SHCODE @ P-CALL-OFF @ + T=
   H-SHSITES @ P-CALLSITE @ + SITE 8 + LE:U32@
      SITE-REC-TAG s" LEAF" SHIPPED or T=
   H-SHSITES @ P-CODESITE @ + SITE 8 + LE:U32@
      SITE-REC-TAG s" LEAF" SHIPPED or T=
   H-XTOFFS @ P-NAMEDCELL @ + XT-ROW 4 + LE:U32@
      XTOFF-KIND-MASK and 0 T=
   AOT-SHADOW:XT-N @ H-SHXTS @ P-SHXTS @ + > TTRUE ;

public
: RUN ( -- )
   KEY HOST HOST-U @ AOT-FILE:READ
   KEY PART PART-U @ AOT-FILE:MERGE
   CHECK
   AOT-FILE:OWN dup AOT-FILE:IMPORT AOT-OWNED:CLOSE
   CHECK
   KEY DONE DONE-U @ AOT-FILE:WRITE
   AOT-FILE:SHA$ drop FIRST 32 BYTE-COPY
   KEY DONE DONE-U @ AOT-FILE:READ
   KEY DONE DONE-U @ AOT-FILE:WRITE
   AOT-FILE:SHA$ FIRST 32 T$=
   CLEANUP-RUN T-REPORT ;
;using
;using
;package

1 set-tier
AOT-NATIVE-MERGE:PREPARE
AOT-NATIVE-MERGE:OPEN
package AOT-MERGE-BASE
public
: LEAF ( n -- n ) 7 + ;
create HOST-PAD 8192 allot
TYPED-VARIABLE HOST-XT [ n -- n ]
;package
' AOT-MERGE-BASE:LEAF AOT-MERGE-BASE:HOST-XT xt!
AOT-ARM:WINDOW-CLOSE
AOT-NATIVE-MERGE:HOST-CAPTURE

AOT-NATIVE-MERGE:OPEN
package AOT-MERGE-PART
public
create DATA-CELL 8 allot
5 DATA-CELL !
TYPED-VARIABLE BASE-XT [ n -- n ]
TYPED-VARIABLE QUOTE-XT [ n -- n ]
private
: HELPER ( n -- n ) 1+ ;
public
: CALLER ( n -- n ) HELPER AOT-MERGE-BASE:LEAF 1+ ;
: TICK ( -- [ n -- n ] ) ['] AOT-MERGE-BASE:LEAF ;
: DATA-USER ( -- n ) DATA-CELL @ ;
: SET-QUOTE ( -- ) [: 2 + ;] QUOTE-XT xt! ;
defer HOOK ( n -- n )
: SET-HOOK ( -- ) ['] CALLER is HOOK ;
: MAKER ( n -- ) create , does> ( -- n ) @ ;
: USE-MAKER ( n -- ) MAKER ;
;package
AOT-ARM:WINDOW-CLOSE
' AOT-MERGE-BASE:LEAF AOT-MERGE-PART:BASE-XT xt!
AOT-MERGE-PART:SET-QUOTE
AOT-MERGE-PART:SET-HOOK
AOT-NATIVE-MERGE:PART-CAPTURE
AOT-NATIVE-MERGE:RUN
