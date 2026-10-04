\ Native x86 capture of a private does> definer reached through a public call.
\ The clause has no created child to root it. Capture must retain its exact
\ entry record and a shadow row while omitting the private parent's name.
\ Run: bin/hb --load test/aot-native-does.f -- <artifact.aot>

package AOTN
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

package AOTN
public
: OPEN ( -- ) NABI:BINDING NSHADOW:OPEN-NATIVE ;
;package

package AOTN
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

: SHIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR= if drop i leave then
   loop ;

: SHADOWED? ( n -- bool ) {: k:n :}
   AOT-SHADOW:REC-N @ 0 ?do
      AOT-SHADOW:REC-BUF@ i AOT-SHADOW:REC-ROW * + LE:U32@ k =
         if true unloop exit then
   loop false ;

public
: RUN ( -- )
   SCRIPT-ARGC 1 <> if s" aot-native-does: artifact path required" 64 die then
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$
   ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
   AOT-CAPTURE:CAPTURE-NATIVE
   AOT-CAPTURE:SHADOW-CAPTURE
   NSHADOW:CLOSE
   s" MAKER" SHIPPED -1 <> if
      s" aot-native-does: private parent shipped" 74 die then
   s" USE-MAKER" SHIPPED 0 < if
      s" aot-native-does: public wrapper was lost" 74 die then
   s" MAKER;does" SHIPPED {: clause:n :}
   clause 0 < if s" aot-native-does: clause record was lost" 74 die then
   clause SHADOWED? 0= if
      s" aot-native-does: clause has no shadow routine" 74 die then
   IDENT!
   KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE
   AOT-SHADOW:RESET
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   clause SHADOWED? 0= if
      s" aot-native-does: clause lost in artifact" 74 die then
   s" aot-native-does: captured" type cr ;
;package

1 set-tier
AOTN:OPEN
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package AOTN-WINDOW
private
: MAKER ( n -- ) create , does> ( -- n ) @ 1+ ;
public
: USE-MAKER ( n -- ) MAKER ;
;package

AOT-ARM:WINDOW-CLOSE
AOTN:RUN
