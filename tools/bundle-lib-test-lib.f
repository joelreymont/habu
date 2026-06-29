\ bundle-lib-test.f - checked fixtures for tools/bundle-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/bundle-lib-core.f tools/bundle-lib-test.f

8192 constant BLTT-BUF-CAP
$20000 constant BLTT-BUNDLE-CAP
10000 constant BLTT-TIMEOUT-MS

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

: BLTT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: BLTT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
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
   92 SB-APPEND-C s"  bundle-lib smoke driver; loaded after errors and array." SB-APPEND BLTT-LF
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
   s" hb-bundle-lib" TMPDIR-MKDIR {: a:ptr u:n :}
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
   s" tools/bundle-lib-core.f" s" tools/bundle-lib.f" WR-TOOLS-LOAD2 if exit then
   s" --load" BLTT-ARG+
   s" lib/errors.f" BLTT-ARG+
   s" lib/string.f" BLTT-ARG+
   s" lib/fs.f" BLTT-ARG+
   s" lib/fs-mutate.f" BLTT-ARG+
   s" tools/bundle-lib-core.f" BLTT-ARG+
   s" tools/bundle-lib.f" BLTT-ARG+
   s" --" BLTT-ARG+ ;

: BLTT-ARGV-BASE ( -- )
   BLTT-ARGV-TOOL
   s" -o" BLTT-ARG+
   BLTT-BUNDLE BLTT-ARG+ ;

: BLTT-ARGV-ERRORS ( -- )
   BLTT-ARGV-BASE
   s" errors"  >LEN PROC-ARGV+ ;

: BLTT-CAPTURE>N ( len len n n -- n n n n ) {: outu:len erru:len kind:n code:n :}
   outu LEN>N erru LEN>N kind code ;

: BLTT-HB-CAPTURE ( -- n n n n )
   s" bin/hb"  >LEN BLTT-OUT BLTT-BUF-CAP >LEN
   BLTT-ERR BLTT-BUF-CAP >LEN BLTT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME BLTT-CAPTURE>N ;

: BLTT-TOOL-CAPTURE ( -- n n n n )
   WR-TOOLS$  >LEN BLTT-OUT BLTT-BUF-CAP >LEN
   BLTT-ERR BLTT-BUF-CAP >LEN BLTT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME BLTT-CAPTURE>N ;

: BLTT-RUN-MISSING-MODULE ( -- n n n n )
   BLTT-ARGV-ERRORS
   s" missing-module" BLTT-ARG+
   s" --" BLTT-ARG+
   BLTT-DRIVER BLTT-ARG+
   BLTT-TOOL-CAPTURE ;

: BLTT-RUN-MISSING-SCRIPT ( -- n n n n )
   BLTT-ARGV-ERRORS
   s" array" BLTT-ARG+
   s" --" BLTT-ARG+
   BLTT-MISSING BLTT-ARG+
   BLTT-TOOL-CAPTURE ;

: BLTT-RUN-BUNDLE-LIB ( -- n n n n )
   BL-RESET
   BLTT-BUNDLE BL-OUT!
   s" errors" BL-MOD+
   s" array" BL-MOD+
   BLTT-DRIVER BL-SCRIPT!
   BL-VERIFY
   BL-EMIT-BUNDLE
   0 0 PROC-OUTCOME-EXIT 0 ;

: BLTT-RUN-BUNDLE ( -- n n n n )
   PROC-ARGV-RESET
   s" --load" BLTT-ARG+
   BLTT-BUNDLE BLTT-ARG+
   s" --" BLTT-ARG+
   s" unused" BLTT-ARG+
   s" args" BLTT-ARG+
   BLTT-HB-CAPTURE ;

: BLTT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n expect:n :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: BLTT-EXPECT-EXIT-NZ ( n n n n -- n n ) {: outu:n erru:n kind:n code:n :}
   kind PROC-OUTCOME-EXIT T=
   code 0 T<>
   outu erru ;

: BLTT-TEST-MISSING-MODULE ( -- )
   BLTT-RUN-MISSING-MODULE BLTT-EXPECT-EXIT-NZ {: outu:n erru:n :}
   outu 0 T=
   BLTT-ERR erru s" missing module" CONTAINS? TTRUE ;

: BLTT-TEST-MISSING-SCRIPT ( -- )
   BLTT-RUN-MISSING-SCRIPT BLTT-EXPECT-EXIT-NZ {: outu:n erru:n :}
   outu 0 T=
   BLTT-ERR erru s" missing script" CONTAINS? TTRUE ;

: BLTT-TEST-BUILD-BUNDLE ( -- )
   BLTT-RUN-BUNDLE-LIB 0 BLTT-EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   BLTT-ERR erru BLTT-EMPTY$ T$=
   BLTT-BUNDLE BLTT-BUNDLE-READ BLTT-BUNDLE-CAP READ-ALL {: bundleu:n :}
   BLTT-BUNDLE-READ bundleu s" lib/errors.f" CONTAINS? TTRUE
   BLTT-BUNDLE-READ bundleu s" src/core/combinators.f" CONTAINS? TFALSE
   BLTT-BUNDLE-READ bundleu s" lib/array.f" CONTAINS? TTRUE
   BLTT-BUNDLE-READ bundleu s" BLT-MAIN" CONTAINS? TTRUE ;

: BLTT-TEST-RUN-BUNDLE ( -- )
   BLTT-RUN-BUNDLE 0 BLTT-EXPECT-EXIT {: outu:n erru:n :}
   BLTT-ERR erru BLTT-EMPTY$ T$=
   BLTT-OUT outu BLTT-OK$ CONTAINS? TTRUE ;

: BLTT-CHECK-BIN ( ptr u8 n -- ) {: a:ptr u:n :}
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
