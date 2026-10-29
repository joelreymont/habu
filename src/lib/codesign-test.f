\ codesign-test.f - focused tests for src/lib/codesign.f.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f src/lib/codesign.f src/lib/codesign-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/codesign.f

variable CST-ROOT-U
variable CST-SCRIPT-SRC-U
variable CST-SCRIPT-DST-U
variable CST-SIGN-U
variable CST-SIGNED-SRC-U
variable CST-SIGNED-DST-U
variable CST-ENSURE-U

create CST-ROOT-BUF PATH-CAP allot
create CST-SCRIPT-SRC-BUF PATH-CAP allot
create CST-SCRIPT-DST-BUF PATH-CAP allot
create CST-SIGN-BUF PATH-CAP allot
create CST-SIGNED-SRC-BUF PATH-CAP allot
create CST-SIGNED-DST-BUF PATH-CAP allot
create CST-ENSURE-BUF PATH-CAP allot

: CST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: CST-ROOT ( -- ptr u8 n )
   CST-ROOT-BUF CST-ROOT-U @ ;

: CST-SCRIPT-SRC ( -- ptr u8 n )
   CST-SCRIPT-SRC-BUF CST-SCRIPT-SRC-U @ ;

: CST-SCRIPT-DST ( -- ptr u8 n )
   CST-SCRIPT-DST-BUF CST-SCRIPT-DST-U @ ;

: CST-SIGN ( -- ptr u8 n )
   CST-SIGN-BUF CST-SIGN-U @ ;

: CST-SIGNED-SRC ( -- ptr u8 n )
   CST-SIGNED-SRC-BUF CST-SIGNED-SRC-U @ ;

: CST-SIGNED-DST ( -- ptr u8 n )
   CST-SIGNED-DST-BUF CST-SIGNED-DST-U @ ;

: CST-ENSURE ( -- ptr u8 n )
   CST-ENSURE-BUF CST-ENSURE-U @ ;

: CST-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-codesign" FS:MKTEMP-DIR {: a:ptr u :}
   a u CST-ROOT-BUF CST-ROOT-U CST-COPY!
   CST-ROOT FS:CLEANUP-TREE+
   CST-ROOT s" promote-src.sh" CST-SCRIPT-SRC-BUF CST-SCRIPT-SRC-U CST-PATH!
   CST-ROOT s" promote-dst.sh" CST-SCRIPT-DST-BUF CST-SCRIPT-DST-U CST-PATH!
   CST-ROOT s" sign-echo" CST-SIGN-BUF CST-SIGN-U CST-PATH!
   CST-ROOT s" signed-src" CST-SIGNED-SRC-BUF CST-SIGNED-SRC-U CST-PATH!
   CST-ROOT s" signed-dst" CST-SIGNED-DST-BUF CST-SIGNED-DST-U CST-PATH!
   CST-ROOT s" ensure-script" CST-ENSURE-BUF CST-ENSURE-U CST-PATH! ;

73 constant CST-EXEC-BITS   \ octal 111: every execute bit

: CST-X? ( ptr u8 n -- bool )
   FS:STAT-MODE CST-EXEC-BITS and CST-EXEC-BITS = ;

: CST-CP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   PROC-ARGV-RESET
   src srcu  >LEN PROC-ARGV+
   dst dstu  >LEN PROC-ARGV+
   s" /bin/cp" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: CST-WRITE-SCRIPT ( -- )
   CST-SCRIPT-SRC s" #!/bin/sh\nexit 0\n" FS:WRITE-ALL ;

: CST-TEST-PROMOTE ( -- )
   CST-WRITE-SCRIPT
   CST-SCRIPT-SRC FS:FILE? TTRUE
   CST-SCRIPT-DST FS:EXISTS? TFALSE
   CST-SCRIPT-SRC CST-SCRIPT-DST CODESIGN:PROMOTE-EXECUTABLE
   CST-SCRIPT-SRC FS:EXISTS? TFALSE
   CST-SCRIPT-DST FS:FILE? TTRUE
   CST-SCRIPT-DST CST-X? TTRUE ;

: CST-TEST-CODESIGN ( -- )
   s" /bin/echo" CST-SIGN CST-CP
   CST-SIGN FS:CHMOD-X
   CST-SIGN CODESIGN:FORCE
   CST-SIGN CODESIGN:VERIFY ;

: CST-TEST-SIGNED-PROMOTE ( -- )
   s" /bin/echo" CST-SIGNED-SRC CST-CP
   CST-SIGNED-SRC CST-SIGNED-DST CODESIGN:PROMOTE-SIGNED-EXECUTABLE
   CST-SIGNED-SRC FS:EXISTS? TFALSE
   CST-SIGNED-DST FS:FILE? TTRUE
   CST-SIGNED-DST CST-X? TTRUE
   CST-SIGNED-DST CODESIGN:VERIFY ;

: CST-EXPECT-UNSIGNED-VERIFY-RC ( -- )
   HB-TARGET-LINUX-KERNEL? if
      CST-ENSURE CODESIGN:VERIFY-RC 0 T=
      exit
   then
   HB-TARGET-MACOS? if
      CST-ENSURE CODESIGN:VERIFY-RC 0 T<>
      exit
   then
   E-BUILD-SOURCE throw ;

: CST-TEST-ENSURE ( -- )
   CST-ENSURE s" #!/bin/sh\nexit 0\n" FS:WRITE-ALL
   CST-ENSURE FS:CHMOD-X
   CST-EXPECT-UNSIGNED-VERIFY-RC
   CST-ENSURE CODESIGN:ENSURE
   CST-ENSURE CODESIGN:VERIFY ;

: CST-CODESIGN-MISSING ( -- )
   s" /no/such/habu-codesign-test" CODESIGN:VERIFY ;

: CST-PROMOTE-MISSING ( -- )
   s" /no/such/habu-promote-test" CST-SCRIPT-DST CODESIGN:PROMOTE-EXECUTABLE ;

: CST-TEST-TOOL ( -- )
   HB-TARGET-LINUX-KERNEL? if
      CODESIGN:TOOL FS:FILE? TFALSE
      exit
   then
   HB-TARGET-MACOS? if
      CODESIGN:TOOL FS:FILE? TTRUE
      exit
   then
   E-BUILD-SOURCE throw ;

: CODESIGN-TEST-MAIN ( -- )
   T-RESET
   CST-PREPARE
   CST-TEST-TOOL
   CST-TEST-PROMOTE
   CST-TEST-CODESIGN
   CST-TEST-SIGNED-PROMOTE
   CST-TEST-ENSURE
   [: CST-CODESIGN-MISSING ;] E-BUILD-PATH TTHROWSQ
   [: CST-PROMOTE-MISSING ;] E-BUILD-PATH TTHROWSQ
   FS:CLEANUP-RUN
   CST-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" codesign-test: ok" type cr ;

CODESIGN-TEST-MAIN
