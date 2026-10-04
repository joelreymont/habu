\ Run in a matching whitebox engine with two paths after --: the intended
\ foreign output and a retained AOT artifact. The source policy runs before
\ native-runtime.f, then the supplied writer preserves the capture before the
\ ordinary foreign-writer refusal.

1 set-tier
require lib/test.f
require lib/le.f
require tools/native-build-core.f
require src/habu/aot-file.f
require src/compiler/native/host.f
require src/compiler/native/publish.f

package NATIVE-BUILD
private

create PRODUCT-KEY 32 allot
PTR-VARIABLE TEST-NAME
variable TEST-NAME-U
variable OBSERVED
variable OLD-SLOT
variable OLD-OCC

TRUSTED: RUN-N ( n -- [ -- n ] ) ;

: HANDLE ( ptr u8 n -- n n )
   XREF-FIND DEF-OCC:SELECT ;

: SELECT ( ptr u8 n -- n )
   HANDLE BUILD-TARGET:ACTION@ RTARGET:EXECUTION@ NHOST:SELECT-ENTRY ;

: SELECTED-N ( ptr u8 n -- n ) SELECT RUN-N execute ;

: VALUE-N ( ptr u8 n -- n )
   XREF-FIND XREF-START RUN-N execute ;

: SELECT-STORED ( -- )
   TEST-NAME @ TEST-NAME-U @ SELECT drop ;

: REFUSES ( ptr u8 n -- )
   TEST-NAME-U ! TEST-NAME !
   [: SELECT-STORED ;] NHOST:E-UNSAFE TTHROWSQ ;

: OBSERVE ( NART:emission n n n -- )
   2drop 2drop 1 OBSERVED +! ;

: CUSTOM-SOURCE ( -- )
   s" test/compiler/native-host-custom.f" included ;

: INITIALIZER-SOURCE ( -- )
   s" test/compiler/native-host-initializer.f" included ;

: STALE-SELECT ( -- )
   OLD-SLOT @ OLD-OCC @ BUILD-TARGET:ACTION@ RTARGET:EXECUTION@
   NHOST:SELECT-ENTRY drop ;

: RECLAIM-CASE ( -- )
   s" test/compiler/native-host-reclaim.f" included
   s" NATIVE-HOST-RECLAIM:OLD" HANDLE OLD-OCC ! OLD-SLOT !
   s" NATIVE-HOST-RECLAIM:OLD" SELECTED-N 11 T=
   s" NATIVE-HOST-RECLAIM:MARK" FORGET-DEFS-FROM
   [: STALE-SELECT ;] DEF-OCC:E-STALE TTHROWSQ
   s" package NATIVE-HOST-RECLAIM public : NEW ( -- n ) 12 ; ;package"
      evaluate-closed
   [: STALE-SELECT ;] DEF-OCC:E-STALE TTHROWSQ
   s" NATIVE-HOST-RECLAIM:NEW" SELECTED-N 12 T= ;

: SOURCE-CHECKS ( -- )
   s" selected host helper and native abs yield 42" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" SELECTED-N 42 T=
   s" NATIVE-HOST-SOURCE:LARGE-SCALAR" SELECTED-N
      $7FFF000000001234 T=
   s" custom publication retains its owned host row" T-LABEL
   s" NATIVE-HOST-CUSTOM:ANSWER" SELECTED-N 42 T=
   OBSERVED @ 1 T=
   s" source casts, stores and pointer views refuse before entry" T-LABEL
   s" NATIVE-HOST-SOURCE:BEFORE-BAD" REFUSES
   s" NATIVE-HOST-SOURCE:NESTED-STORE" REFUSES
   s" NATIVE-HOST-SOURCE:CAST-BODY" REFUSES
   s" NATIVE-HOST-SOURCE:COMMA-BODY" REFUSES
   s" NATIVE-HOST-SOURCE:POINTER-BODY" REFUSES
   s" NATIVE-HOST-SOURCE:WRITES@" VALUE-N 0 T=
   s" unknown native and division cold throw lack contracts" T-LABEL
   s" NATIVE-HOST-SOURCE:DIRECT-NATIVE" REFUSES
   s" NATIVE-HOST-SOURCE:DIVIDE-COLD" REFUSES
   s" NATIVE-HOST-SOURCE:TRUSTED-ROOT" REFUSES
   s" untaken indirect, catch, finally, defer and callback refuse" T-LABEL
   s" NATIVE-HOST-SOURCE:INDIRECT" REFUSES
   s" NATIVE-HOST-SOURCE:CAUGHT" REFUSES
   s" NATIVE-HOST-SOURCE:FINALIZED" REFUSES
   s" NATIVE-HOST-SOURCE:DEFERRED" REFUSES
   s" NATIVE-HOST-SOURCE:CALLBACK-ROOT" REFUSES
   s" NATIVE-HOST-SOURCE:WRITES@" VALUE-N 0 T=
   s" old bindings and aliases retain their original bodies" T-LABEL
   s" NATIVE-HOST-SOURCE:OLD-CALLER" SELECTED-N 42 T=
   s" NATIVE-HOST-ALIAS:HOST42" SELECTED-N 42 T=
   s" NATIVE-HOST-SOURCE:NEW-CALLER" SELECTED-N 99 T=
   s" NATIVE-HOST-SOURCE:HOST42" SELECTED-N 99 T=
   s" reclamation invalidates old occurrence before reuse" T-LABEL
   RECLAIM-CASE
   s" a refused closure leaves the next valid call usable" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" SELECTED-N 42 T= ;

: BIND-CONSTRUCTION ( -- )
   DEFAULT-SOURCE-BIND
   T-RESET
   0 OBSERVED !
   s" test/compiler/native-host-source.f" included
   ['] OBSERVE ['] CUSTOM-SOURCE NPUB:WITH-UNIT
   s" NATIVE-HOST-SOURCE:TARGET99" HANDLE
   s" NATIVE-HOST-SOURCE:HOST42" HANDLE NHOST:ASSOCIATE
   s" test/compiler/native-host-redefine.f" included
   SOURCE-CHECKS
   BUILD-TARGET:ACTION@ RTARGET:EXECUTION@
      ['] INITIALIZER-SOURCE NHOST:WITH-REQUIRED
   s" NATIVE-HOST-RESULT:ONE42" VALUE-N 42 T=
   T-REPORT ;

: CHECK-CONSTRUCTION ( -- )
   s" NATIVE-HOST-RESULT:ONE42" VALUE-N 42 T= ;

: CAPTURE-WRITER ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   {: owned:AOT-OWNED:capture host:ptr count:n path:ptr size:n :}
   owned dup AOT-FILE:IMPORT
   PRODUCT-KEY 1 SCRIPT-ARGV$ AOT-FILE:WRITE
   host count path size SOURCE-WRITER-DISPATCH ;

: ORIGIN ( n n -- n ) code-origin ;

: ARTIFACT-NAME? ( n ptr u8 n -- bool )
   {: row:n name:ptr size:n :}
   AOT-NAMES-BUF@ AOT-REC-BUF@ AOT-REC-MAX 48 * +
      row AOT-CREC-ROW * + 8 + LE:U32@ + {: e:ptr :}
   e 1+ e c@ name size CORE-STR= ;

: ONE42-COUNT ( -- n )
   0
   AOT-REC-N @ 0 ?do
      i s" ONE42" ARTIFACT-NAME? if 1+ then
   loop ;

public

: RUN-HOST-CONSTRUCTION ( -- )
   s" aarch64-unknown-linux-gnu" BUILD-TARGET:SELECT? 0= if 76 throw then
   ['] BIND-CONSTRUCTION ['] CHECK-CONSTRUCTION ['] SOURCE-NOOP SOURCE-POLICY!
   0 SCRIPT-ARGV$ OUTPUT!
   ['] ORIGIN false ['] CAPTURE-WRITER RUN-READY-RC BUILD-RC T=
   PRODUCT-KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
   ONE42-COUNT 1 T=
   T-REPORT
   s" native host construction: artifact " type
   1 SCRIPT-ARGV$ type cr ;

;package

NATIVE-BUILD:RUN-HOST-CONSTRUCTION
