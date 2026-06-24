\ bundle-lib-test.f - checked fixtures for tools/bundle-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/bundle-lib-test.f

8192 constant BLTT-BUF-CAP
$20000 constant BLTT-BUNDLE-CAP

variable BLTT-ROOT-U
variable BLTT-DRIVER-U
variable BLTT-BUNDLE-U
variable BLTT-MISSING-U
variable BLTT-PUBLIC-BIN-N

create BLTT-ROOT-BUF FS-PATH-CAP allot
create BLTT-DRIVER-BUF FS-PATH-CAP allot
create BLTT-BUNDLE-BUF FS-PATH-CAP allot
create BLTT-MISSING-BUF FS-PATH-CAP allot
create BLTT-OUT BLTT-BUF-CAP allot
create BLTT-ERR BLTT-BUF-CAP allot
create BLTT-BUNDLE-READ BLTT-BUNDLE-CAP allot

: BLTT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: BLTT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: BLTT-ROOT ( -- ptr u8 n )
   BLTT-ROOT-BUF BLTT-ROOT-U @ ;

: BLTT-DRIVER ( -- ptr u8 n )
   BLTT-DRIVER-BUF BLTT-DRIVER-U @ ;

: BLTT-BUNDLE ( -- ptr u8 n )
   BLTT-BUNDLE-BUF BLTT-BUNDLE-U @ ;

: BLTT-MISSING ( -- ptr u8 n )
   BLTT-MISSING-BUF BLTT-MISSING-U @ ;

: BLTT-LF ( -- )
   10 SB-APPEND-C ;

: BLTT-DQ ( -- )
   34 SB-APPEND-C ;

: BLTT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BLTT-OK$ ( -- ptr u8 n )
   s" bundle-lib-test: ok" ;

: BLTT-DRIVER$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  bundle-lib smoke driver; loaded after errors, combinators, and array." SB-APPEND BLTT-LF
   BLTT-LF
   s" 100 constant BLT-FAIL" SB-APPEND BLTT-LF
   BLTT-LF
   s" create BLT-DATA 3 , 1 , 4 ," SB-APPEND BLTT-LF
   BLTT-LF
   s" : BLT= ( n n -- ) {: got want :}" SB-APPEND BLTT-LF
   s"    got want <> if s" SB-APPEND BLTT-DQ s"  bundle-lib-test: mismatch" SB-APPEND BLTT-DQ
   s"  BLT-FAIL die then ;" SB-APPEND BLTT-LF
   BLTT-LF
   s" : BLT-SUM ( -- n )" SB-APPEND BLTT-LF
   s"    BLT-DATA 3 >LEN A-SUM ;" SB-APPEND BLTT-LF
   BLTT-LF
   s" : BLT-ERROR-CODE ( -- n )" SB-APPEND BLTT-LF
   s"    E-A-BOUNDS ;" SB-APPEND BLTT-LF
   BLTT-LF
   s" : BLT-MAIN ( -- )" SB-APPEND BLTT-LF
   s"    BLT-SUM 8 BLT=" SB-APPEND BLTT-LF
   s"    BLT-ERROR-CODE E-A-BOUNDS BLT=" SB-APPEND BLTT-LF
   s"    s" SB-APPEND BLTT-DQ s"  bundle-lib-test: ok" SB-APPEND BLTT-DQ
   s"  type cr ;" SB-APPEND BLTT-LF
   BLTT-LF
   s" BLT-MAIN" SB-APPEND BLTT-LF
   SB$ ;

: BLTT-PREPARE ( -- )
   CLEANUP-RESET
   s" hb-bundle-lib" TMPDIR-MKDIR {: a:ptr u :}
   a u BLTT-ROOT-BUF BLTT-ROOT-U BLTT-COPY!
   BLTT-ROOT CLEANUP-DIR+
   BLTT-ROOT s" driver.f" BLTT-DRIVER-BUF BLTT-DRIVER-U BLTT-PATH!
   BLTT-ROOT s" bundle.f" BLTT-BUNDLE-BUF BLTT-BUNDLE-U BLTT-PATH!
   BLTT-ROOT s" no-such-script.f" BLTT-MISSING-BUF BLTT-MISSING-U BLTT-PATH!
   BLTT-DRIVER CLEANUP+
   BLTT-BUNDLE CLEANUP+
   BLTT-DRIVER BLTT-DRIVER$ WRITE-ALL ;

: BLTT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BLTT-ARGV-TOOL ( -- )
   PROC-ARGV-RESET
   s" --load" BLTT-ARG+
   s" lib/errors.f" BLTT-ARG+
   s" lib/string.f" BLTT-ARG+
   s" lib/fs.f" BLTT-ARG+
   s" lib/fs-mutate.f" BLTT-ARG+
   s" tools/bundle-lib.f" BLTT-ARG+
   s" --" BLTT-ARG+ ;

: BLTT-ARGV-BASE ( -- )
   BLTT-ARGV-TOOL
   s" -o" BLTT-ARG+
   BLTT-BUNDLE BLTT-ARG+ ;

: BLTT-ARGV-ERRORS ( -- )
   BLTT-ARGV-BASE
   s" errors"  >LEN PROC-ARGV+ ;

: BLTT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: BLTT-HB-CAPTURE ( -- n n n )
   s" bin/hb"  >LEN BLTT-OUT BLTT-BUF-CAP >LEN
   BLTT-ERR BLTT-BUF-CAP >LEN 1000 >MS
   RUN-ARGV-CAPTURE BLTT-CAPTURE>N ;

: BLTT-RUN-MISSING-MODULE ( -- n n n )
   BLTT-ARGV-ERRORS
   s" missing-module" BLTT-ARG+
   s" --" BLTT-ARG+
   BLTT-DRIVER BLTT-ARG+
   BLTT-HB-CAPTURE ;

: BLTT-RUN-MISSING-SCRIPT ( -- n n n )
   BLTT-ARGV-ERRORS
   s" array" BLTT-ARG+
   s" --" BLTT-ARG+
   BLTT-MISSING BLTT-ARG+
   BLTT-HB-CAPTURE ;

: BLTT-RUN-BUNDLE-LIB ( -- n n n )
   BLTT-ARGV-ERRORS
   s" array" BLTT-ARG+
   s" --" BLTT-ARG+
   BLTT-DRIVER BLTT-ARG+
   BLTT-HB-CAPTURE ;

: BLTT-RUN-BUNDLE ( -- n n n )
   PROC-ARGV-RESET
   s" --load" BLTT-ARG+
   BLTT-BUNDLE BLTT-ARG+
   s" --" BLTT-ARG+
   s" unused" BLTT-ARG+
   s" args" BLTT-ARG+
   BLTT-HB-CAPTURE ;

: BLTT-TEST-MISSING-MODULE ( -- )
   BLTT-RUN-MISSING-MODULE 0 T<>
   {: outu erru :}
   outu 0 T=
   BLTT-ERR erru s" missing module" CONTAINS? TTRUE ;

: BLTT-TEST-MISSING-SCRIPT ( -- )
   BLTT-RUN-MISSING-SCRIPT 0 T<>
   {: outu erru :}
   outu 0 T=
   BLTT-ERR erru s" missing script" CONTAINS? TTRUE ;

: BLTT-TEST-BUILD-BUNDLE ( -- )
   BLTT-RUN-BUNDLE-LIB 0 T=
   {: outu erru :}
   outu 0 T=
   BLTT-ERR erru BLTT-EMPTY$ T$=
   BLTT-BUNDLE BLTT-BUNDLE-READ BLTT-BUNDLE-CAP READ-ALL {: bundleu :}
   BLTT-BUNDLE-READ bundleu s" lib/errors.f" CONTAINS? TTRUE
   BLTT-BUNDLE-READ bundleu s" src/core/combinators.f" CONTAINS? TTRUE
   BLTT-BUNDLE-READ bundleu s" lib/array.f" CONTAINS? TTRUE
   BLTT-BUNDLE-READ bundleu s" BLT-MAIN" CONTAINS? TTRUE ;

: BLTT-TEST-RUN-BUNDLE ( -- )
   BLTT-RUN-BUNDLE 0 T=
   {: outu erru :}
   BLTT-ERR erru BLTT-EMPTY$ T$=
   BLTT-OUT outu BLTT-OK$ CONTAINS? TTRUE ;

: BLTT-CHECK-BIN ( ptr u8 n -- ) {: a:ptr u :}
   a u FILE? if
      BLTT-PUBLIC-BIN-N @ 1 + BLTT-PUBLIC-BIN-N !
      a u BASENAME s" hb" T$=
   then ;

: BLTT-TEST-PUBLIC-BINS ( -- )
   s" bin" DIR? if
      0 BLTT-PUBLIC-BIN-N !
      s" bin" [: BLTT-CHECK-BIN ;] WALK-FILES
      BLTT-PUBLIC-BIN-N @ 1 T=
   then ;

: BLTT-MAIN ( -- )
   T-RESET
   BLTT-PREPARE
   BLTT-TEST-MISSING-MODULE
   BLTT-TEST-MISSING-SCRIPT
   BLTT-TEST-BUILD-BUNDLE
   BLTT-TEST-RUN-BUNDLE
   BLTT-TEST-PUBLIC-BINS
   CLEANUP-RUN
   BLTT-ROOT EXISTS? TFALSE
   T-REPORT
   BLTT-OK$ type cr ;

BLTT-MAIN
