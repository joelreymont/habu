\ A postcommit shadow callback runs inside the real compiler publication path.
\ Refusing its checker write must also release the scope for the next compile.
require src/lib/test.f
require src/lib/errors.f
require src/compiler/native/publish.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/habu/xref.f

package NATIVE-SHADOW-CALLBACK-TEST
private

: TAKE ( NART:emission n n n -- ) 2drop drop drop ;

: WRITE ( -- ) s" NATIVE-SHADOW-CALLBACK-ROW ( -- )" CHECK! drop ;

: COMMITTED ( n -- ) drop WRITE ;

variable WRITE-RC

: COMMITTED-CAUGHT ( n -- )
   drop [: WRITE ;] catch WRITE-RC ! ;

: MIN-IN ( ptr u8 n -- n )
   XREF-FIND XREF-FLAGS DNAME-MIN-IN-MASK and 52 rshift ;

: DEFINE-KEPT ( -- )
   s" : NATIVE-SHADOW-CALLBACK-KEPT ( n -- n ) 1+ ;" evaluate-closed ;

: DEFINE ( -- )
   s" : NATIVE-SHADOW-CALLBACK-REJECT ( n -- n ) 1+ ;" evaluate-closed ;

: ATTEMPT ( -- )
   ['] TAKE ['] COMMITTED ['] DEFINE NPUB:WITH-SHADOW ;

: THROW-COMMITTED ( n -- ) drop E-DIV-ZERO throw ;

: DEFINE-THROWN ( -- )
   s" : NATIVE-SHADOW-CALLBACK-THROWN ( n -- n ) 1+ ;" evaluate-closed ;

: THROWING-ATTEMPT ( -- )
   ['] TAKE ['] THROW-COMMITTED ['] DEFINE-THROWN NPUB:WITH-SHADOW ;

: REUSE ( -- )
   NABI:BINDING NSHADOW:OPEN-NATIVE
   ndict@ {: slot:n :}
   s" a rejected publication claims no local shadow row" T-LABEL
   [: THROWING-ATTEMPT ;] E-DIV-ZERO TTHROWSQ
   NSHADOW:RECORDS 0 T=
   s" the reused slot has exactly one shadow row for its live definition" T-LABEL
   s" : NATIVE-SHADOW-CALLBACK-REUSED ( n -- n ) 1+ ;" evaluate-closed
   NSHADOW:RECORDS 1 T=
   0 NSHADOW:RECORD@ slot T=
   NSHADOW:CLOSE ;

public

: RUN ( -- )
   T-RESET
   \ Native shadow byte normalization is the x86 path; checker callback
   \ mutation and recovery below exercise the shared publisher on every host.
   HB-TARGET-LINUX-X86-64? if REUSE then
   s" a caught callback write cannot change the pending word's minimum input" T-LABEL
   ['] TAKE ['] COMMITTED-CAUGHT ['] DEFINE-KEPT NPUB:WITH-SHADOW
   WRITE-RC @ E-NCOMP-STATE T=
   s" NATIVE-SHADOW-CALLBACK-KEPT" MIN-IN 1 T=
   s" postcommit callback cannot mutate the checker" T-LABEL
   [: ATTEMPT ;] E-NCOMP-STATE TTHROWSQ
   s" a throwing callback leaves no underchecked publication visible" T-LABEL
   s" NATIVE-SHADOW-CALLBACK-REJECT" XREF-FIND XREF-FOUND? TFALSE
   s" a refused callback restores the next compilation's write window" T-LABEL
   s" : NATIVE-SHADOW-CALLBACK-NEXT ( n -- n ) 1+ ;" evaluate-closed
   s" NATIVE-SHADOW-CALLBACK-NEXT" MIN-IN 1 T=
   T-REPORT ;

;package

NATIVE-SHADOW-CALLBACK-TEST:RUN
