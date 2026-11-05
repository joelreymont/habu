\ build-fixpoint-test-lib.f - the fixture tools/build-fixpoint-source-test.f
\ loads: the scratch tree, BFT-PREPARE and the step and finish words. No MAIN.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/build.f
require src/lib/codesign.f
require tools/build-fixpoint.f

\ The fixture drives the tool's internals, so it reopens package BUILD-FIXPOINT
\ instead of widening the tool's interface for its own test.
package BUILD-FIXPOINT

variable BFT-ROOT-U
variable BFT-PREFIX-U
variable BFT-CERT-U
TYPED-VARIABLE BFT-READ-A ptr u8
variable BFT-READ-CAP

create BFT-ROOT-BUF PATH-CAP allot
create BFT-PREFIX-BUF PATH-CAP allot
create BFT-CERT-BUF PATH-CAP allot

: BFT-READ-BUF! ( ptr u8 -- )
   BFT-READ-A ! ;

: BFT-READ-BUF ( -- ptr u8 )
   BFT-READ-A @ ;

: BFT-ALLOC-READ ( n -- )
   dup BFT-READ-CAP @ <= if drop exit then
   dup MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-READ-BUF!
   BFT-READ-CAP ! ;

: BFT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: BFT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: BFT-ROOT ( -- ptr u8 n )
   BFT-ROOT-BUF BFT-ROOT-U @ ;

: BFT-PREFIX ( -- ptr u8 n )
   BFT-PREFIX-BUF BFT-PREFIX-U @ ;

: BFT-CERT ( -- ptr u8 n )
   BFT-CERT-BUF BFT-CERT-U @ ;

: BFT-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-build-fixpoint" FS:MKTEMP-DIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ROOT FS:CLEANUP-TREE+
   BFT-ROOT s" prefix-src" BFT-PREFIX-BUF BFT-PREFIX-U BFT-PATH!
   BFT-ROOT s" cert-source.f" BFT-CERT-BUF BFT-CERT-U BFT-PATH! ;

: BFT-READ ( ptr u8 n -- n ) {: pa:ptr pu:n :}
   pa pu FS:FILE-SIZE BFT-ALLOC-READ
   pa pu BFT-READ-BUF BFT-READ-CAP @ FS:READ-ALL ;

\ E-PROC-TIMEOUT leaves the row uncaught once its step is named: the engine's
\ report is then the row's last stderr line. Every other throw ends the row here
\ as a failed step.
: BFT-STEP ( ptr u8 n [ -- ] -- ) {: a:ptr u:n q :}
   a u T-LABEL
   q catch {: rc:n :}
   rc 0= if exit then
   a u type s" : throw " type rc . cr
   rc E-PROC-TIMEOUT = if rc throw then
   s" build-fixpoint-test-lib: subtest threw" T-EX-FAIL die ;

: BFT-FINISH ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   FS:CLEANUP-RUN
   BFT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   msg msgu type cr ;

;package
