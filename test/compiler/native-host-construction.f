\ Run in a matching ARM64 whitebox engine with two paths after --: the intended
\ foreign output and a retained AOT artifact. The source policy runs before
\ native-runtime.f; the supplied writer saves the capture before the normal
\ cross-OS launch refusal returns from native-build.

1 set-tier
require lib/test.f
require lib/le.f
require lib/engine-id.f
require tools/native-build-core.f
require src/habu/aot-file.f
require src/compiler/native/host.f
require src/compiler/native/publish.f

package NATIVE-BUILD
private

create PRODUCT-KEY 32 allot
create FILE-HASH SHA256-FILE-CTX-BYTES allot
PTR-VARIABLE TEST-NAME
variable TEST-NAME-U
variable OBSERVED
variable EARLY-ARMED
variable EARLY-OBSERVED
variable OLD-SLOT
variable OLD-OCC

TRUSTED: RUN-N ( n -- [ -- n ] ) ;
TRUSTED: PATH-XT ( n -- [ ptr u8 n -- ] ) ;
TRUSTED: SLOT-XT ( n -- [ n -- ptr u8 ] ) ;
TRUSTED: LEN-XT ( n -- [ n -- n ] ) ;
CAST: CODE-BYTES ( n -- ptr u8 )

TRUSTED: REWRITE-FIRST ( n -- )
   {: entry:n :}
   entry CODE-BYTES LE:U32@ entry patch32 ;

: LOAD-SOURCE ( ptr u8 n -- )
   s" script-required" OPEN-TARGET-XT PATH-XT execute ;

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
   2drop 2drop
   NHOST:UNKNOWN 0 NHOST:SOURCE-REFUSE
   1 OBSERVED +! ;

: OBSERVE-EARLY ( n IR-CTX:ctx IR-BUILD:module -- )
   2drop drop
   EARLY-ARMED @ 0= if exit then
   EARLY-OBSERVED @ 0= if
      NHOST:UNKNOWN 0 NHOST:SOURCE-REFUSE
   else
      NHOST:SOURCE-CONTRACT-START
      NHOST:SOURCE-CONTRACT-DONE
   then
   1 EARLY-OBSERVED +! ;

: CUSTOM-SOURCE ( -- )
   s" test/compiler/native-host-custom.f" LOAD-SOURCE ;

: EARLY-SOURCE ( -- )
   1 EARLY-ARMED !
   s" test/compiler/native-host-early.f" LOAD-SOURCE
   0 EARLY-ARMED ! ;

: FOREIGN-SOURCE ( -- )
   s" test/compiler/native-host-foreign.f" LOAD-SOURCE ;

: INITIALIZER-SOURCE ( -- )
   s" test/compiler/native-host-initializer.f" LOAD-SOURCE ;

: STALE-SELECT ( -- )
   OLD-SLOT @ OLD-OCC @ BUILD-TARGET:ACTION@ RTARGET:EXECUTION@
   NHOST:SELECT-ENTRY drop ;

: RECLAIM-CASE ( -- )
   s" test/compiler/native-host-reclaim.f" LOAD-SOURCE
   s" NATIVE-HOST-RECLAIM:OLD" HANDLE OLD-OCC ! OLD-SLOT !
   s" NATIVE-HOST-RECLAIM:OLD" SELECTED-N 11 T=
   s" NATIVE-HOST-RECLAIM:EARLY" HANDLE
   s" NATIVE-HOST-RECLAIM:OLD" HANDLE NHOST:ASSOCIATE
   s" NATIVE-HOST-RECLAIM:EARLY" SELECTED-N 11 T=
   \ The mark belongs to the fresh target checker, like the loaded source.
   s" NATIVE-HOST-RECLAIM:MARK"
      s" FORGET-DEFS-FROM" OPEN-TARGET-XT PATH-XT execute
   [: STALE-SELECT ;] DEF-OCC:E-STALE TTHROWSQ
   s" NATIVE-HOST-RECLAIM:EARLY" REFUSES
   s" package NATIVE-HOST-RECLAIM public : NEW ( -- n ) 12 ; ;package"
      evaluate-closed
   [: STALE-SELECT ;] DEF-OCC:E-STALE TTHROWSQ
   s" NATIVE-HOST-RECLAIM:EARLY" REFUSES
   s" NATIVE-HOST-RECLAIM:NEW" SELECTED-N 12 T= ;

: PATCH-CASE ( -- )
   s" NATIVE-HOST-SOURCE:PATCH-TARGET" HANDLE
   s" NATIVE-HOST-SOURCE:PATCH-HOST" HANDLE NHOST:ASSOCIATE
   s" NATIVE-HOST-SOURCE:PATCH-TARGET" SELECTED-N 32 T=
   s" NATIVE-HOST-SOURCE:PATCH-HOST" XREF-FIND XREF-START REWRITE-FIRST
   s" NATIVE-HOST-SOURCE:PATCH-TARGET" REFUSES
   s" NATIVE-HOST-SOURCE:PATCH-HOST" REFUSES
   s" NATIVE-HOST-SOURCE:PATCH-NEIGHBOR" SELECTED-N 33 T= ;

: CONTRACT-MISMATCH ( -- )
   s" NATIVE-HOST-SOURCE:BOOL-TARGET" HANDLE
   s" NATIVE-HOST-SOURCE:HOST42" HANDLE NHOST:ASSOCIATE ;

: SOURCE-CHECKS ( -- )
   s" ordinary native execution retains target 99" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" VALUE-N 99 T=
   s" selected host helper and native abs yield 42" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" SELECTED-N 42 T=
   s" NATIVE-HOST-SOURCE:LARGE-SCALAR" SELECTED-N
      $7FFF000000001234 T=
   s" custom publication retains its owned host row" T-LABEL
   s" NATIVE-HOST-CUSTOM:ANSWER" SELECTED-N 42 T=
   OBSERVED @ 1 T=
   s" early frozen-HIR observer cannot change producer facts" T-LABEL
   EARLY-OBSERVED @ 2 T=
   s" NATIVE-HOST-EARLY:ANSWER" SELECTED-N 43 T=
   s" NATIVE-HOST-SOURCE:PATCH-TARGET" HANDLE
   s" NATIVE-HOST-EARLY:CONTRACT" HANDLE NHOST:ASSOCIATE
   s" NATIVE-HOST-SOURCE:PATCH-TARGET" SELECTED-N 44 T=
   s" source casts, stores and pointer views refuse before entry" T-LABEL
   s" NATIVE-HOST-SOURCE:BEFORE-BAD" REFUSES
   s" NATIVE-HOST-SOURCE:NESTED-STORE" REFUSES
   s" NATIVE-HOST-SOURCE:CAST-BODY" REFUSES
   s" NATIVE-HOST-SOURCE:COMMA-BODY" REFUSES
   s" NATIVE-HOST-SOURCE:BYTE-BODY" REFUSES
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
   s" instruction writes retire only affected implementation facts" T-LABEL
   PATCH-CASE
   s" a refused closure leaves the next valid call usable" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" SELECTED-N 42 T= ;

: BIND-CONSTRUCTION ( -- )
   DEFAULT-SOURCE-BIND
   T-RESET
   0 OBSERVED !
   s" test/compiler/native-host-source.f" LOAD-SOURCE
   ['] OBSERVE ['] CUSTOM-SOURCE NPUB:WITH-UNIT
   ['] OBSERVE-EARLY NBACK:OBSERVE!
   EARLY-SOURCE
   s" NATIVE-HOST-SOURCE:TARGET99" HANDLE
   s" NATIVE-HOST-SOURCE:HOST42" HANDLE NHOST:ASSOCIATE
   s" distinct checked output constructors cannot attach" T-LABEL
   [: CONTRACT-MISMATCH ;] NHOST:E-UNSAFE TTHROWSQ
   s" test/compiler/native-host-redefine.f" LOAD-SOURCE
   SOURCE-CHECKS
   BUILD-TARGET:ACTION@ RTARGET:EXECUTION@
      ['] INITIALIZER-SOURCE NHOST:WITH-REQUIRED
   s" NATIVE-HOST-RESULT:ONE42" VALUE-N 42 T=
   T-REPORT ;

: CHECK-CONSTRUCTION ( -- )
   s" foreign target body associates with retained host implementation" T-LABEL
   s" NATIVE-HOST-FOREIGN:TARGET" VALUE-N 99 T=
   s" NATIVE-HOST-FOREIGN:TARGET" HANDLE
   s" NATIVE-HOST-SOURCE:HOST42" HANDLE NHOST:ASSOCIATE
   s" NATIVE-HOST-FOREIGN:TARGET" SELECTED-N 42 T=
   s" foreign body cannot serve as a host implementation" T-LABEL
   s" NATIVE-HOST-SOURCE:TARGET99" HANDLE
   s" NATIVE-HOST-FOREIGN:TARGET" HANDLE NHOST:ASSOCIATE
   s" NATIVE-HOST-SOURCE:TARGET99" REFUSES
   s" NATIVE-HOST-RESULT:ONE42" VALUE-N 42 T=
   T-REPORT
   \ The artifact carries the fresh target loader's real recorded closure.
   AOT-IDENT:RESET
   s" REQUIRE-REG:COUNT" OPEN-TARGET-XT RUN-N execute 0 ?do
      i s" REQUIRE-SLOT" OPEN-TARGET-XT SLOT-XT execute
      i s" REQUIRE-LEN@" OPEN-TARGET-XT LEN-XT execute
      AOT-IDENT:PATH+
   loop ;

: CAPTURE-WRITER ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   {: owned:AOT-OWNED:capture host:ptr count:n path:ptr size:n :}
   owned dup AOT-FILE:IMPORT
   FILE-HASH ENGINE-ID:PATH$ PRODUCT-KEY SHA256-FILE-IN
      0<> if BUILD-RC throw then
   PRODUCT-KEY 1 SCRIPT-ARGV$ AOT-FILE:WRITE
   host count path size SOURCE-WRITER-DISPATCH ;

: ORIGIN ( n n -- n ) code-origin ;

: ARTIFACT-NAME? ( n ptr u8 n -- bool )
   {: row:n name:ptr size:n :}
   AOT-BUF:AOT-NAMES-BUF@ AOT-BUF:AOT-REC-BUF@
      AOT-BUF:AOT-REC-MAX 48 * +
      row AOT-CREC-ROW * + 8 + LE:U32@ + {: e:ptr :}
   e 1+ e c@ name size CORE-STR= ;

: ONE42-COUNT ( -- n )
   0
   AOT-BUF:AOT-REC-N @ 0 ?do
      i s" ONE42" ARTIFACT-NAME? if 1+ then
   loop ;

: FOREIGN-TARGET? ( -- bool )
   HB-TARGET-MACOS? if
      s" aarch64-unknown-linux-gnu" BUILD-TARGET:SELECT? exit
   then
   HB-TARGET-LINUX? if
      s" aarch64-apple-darwin" BUILD-TARGET:SELECT? exit
   then
   false ;

public

: RUN-HOST-CONSTRUCTION ( -- )
   FOREIGN-TARGET? 0= if 76 throw then
   ['] BIND-CONSTRUCTION ['] CHECK-CONSTRUCTION ['] SOURCE-NOOP SOURCE-POLICY!
   ['] FOREIGN-SOURCE SOURCE-TAIL!
   0 SCRIPT-ARGV$ OUTPUT!
   ['] ORIGIN false ['] CAPTURE-WRITER RUN-READY-RC BUILD-RC T=
   PRODUCT-KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
   ONE42-COUNT 1 T=
   T-REPORT
   s" native host construction: artifact " type
   1 SCRIPT-ARGV$ type cr ;

;package

NATIVE-BUILD:RUN-HOST-CONSTRUCTION
