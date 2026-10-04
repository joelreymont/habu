\ A constant exported before native recording must remain a fixed value to a
\ later checked caller. Its original body has no recorded x86 shadow routine.
\ Run: bin/hb --load test/aot-native-export-fixed.f -- <artifact.aot>

package AOTXF
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require lib/string.f
require lib/le.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/habu/aot-file.f
require src/habu/aot-ident.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/native/string.f
require src/arch/x86-64/passes.f

package AOTXF
private
using AOT-BUF
create KEY 32 allot

: IDENT! ( -- )
   32 0 ?do 0 KEY i + c! loop
   AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   s" src/habu/aot-shadow.f" AOT-IDENT:PATH+ ;

: CREC ( n -- ptr u8 ) {: k:n :}
   AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;

: SHIPPED ( ptr u8 n -- bool ) {: a:ptr u:n :}
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR= if true unloop exit then
   loop false ;

public
: OPEN ( -- ) NABI:BINDING NSHADOW:OPEN-NATIVE ;

: RUN ( -- )
   SCRIPT-ARGC 1 <> if s" aot-native-export-fixed: artifact path required" 64 die then
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$
   ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
   AOT-CAPTURE:CAPTURE-NATIVE
   AOT-CAPTURE:SHADOW-CAPTURE
   NSHADOW:CLOSE
   s" USE-FIX" SHIPPED 0= if
      s" aot-native-export-fixed: public caller was lost" 74 die then
   IDENT!
   KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE
   AOT-SHADOW:RESET
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   s" aot-native-export-fixed: captured" type cr ;
;package

1 set-tier
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package AOTXF-WINDOW
private
7 constant HIDDEN
public
EXPORT HIDDEN
;package

AOTXF:OPEN
package AOTXF-WINDOW
public
: USE-FIX ( -- n ) AOTXF-WINDOW:HIDDEN ;
;package

AOT-ARM:WINDOW-CLOSE
AOTXF:RUN
