\ codesign-test.f - focused tests for lib/codesign.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/codesign.f lib/codesign-test.f

variable CST-ROOT-U
variable CST-SCRIPT-SRC-U
variable CST-SCRIPT-DST-U
variable CST-SIGN-U
variable CST-SIGNED-SRC-U
variable CST-SIGNED-DST-U
variable CST-ENSURE-U

create CST-ROOT-BUF FS-PATH-CAP allot
create CST-SCRIPT-SRC-BUF FS-PATH-CAP allot
create CST-SCRIPT-DST-BUF FS-PATH-CAP allot
create CST-SIGN-BUF FS-PATH-CAP allot
create CST-SIGNED-SRC-BUF FS-PATH-CAP allot
create CST-SIGNED-DST-BUF FS-PATH-CAP allot
create CST-ENSURE-BUF FS-PATH-CAP allot

: CST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

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
   CLEANUP-RESET
   s" habu-codesign" TMPDIR-MKDIR {: a:ptr u :}
   a u CST-ROOT-BUF CST-ROOT-U CST-COPY!
   CST-ROOT CLEANUP-TREE+
   CST-ROOT s" promote-src.sh" CST-SCRIPT-SRC-BUF CST-SCRIPT-SRC-U CST-PATH!
   CST-ROOT s" promote-dst.sh" CST-SCRIPT-DST-BUF CST-SCRIPT-DST-U CST-PATH!
   CST-ROOT s" sign-echo" CST-SIGN-BUF CST-SIGN-U CST-PATH!
   CST-ROOT s" signed-src" CST-SIGNED-SRC-BUF CST-SIGNED-SRC-U CST-PATH!
   CST-ROOT s" signed-dst" CST-SIGNED-DST-BUF CST-SIGNED-DST-U CST-PATH!
   CST-ROOT s" ensure-script" CST-ENSURE-BUF CST-ENSURE-U CST-PATH! ;

: CST-X? ( ptr u8 n -- bool )
   STAT-MODE FS-MUT-MODE-EXEC and FS-MUT-MODE-EXEC = ;

: CST-CP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   PROC-ARGV-RESET
   src srcu  >LEN PROC-ARGV+
   dst dstu  >LEN PROC-ARGV+
   s" /bin/cp" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC RC>N 0 T= ;

: CST-WRITE-SCRIPT ( -- )
   CST-SCRIPT-SRC s" #!/bin/sh\nexit 0\n" WRITE-ALL ;

: CST-TEST-PROMOTE ( -- )
   CST-WRITE-SCRIPT
   CST-SCRIPT-SRC FILE? TTRUE
   CST-SCRIPT-DST EXISTS? TFALSE
   CST-SCRIPT-SRC CST-SCRIPT-DST PROMOTE-EXECUTABLE
   CST-SCRIPT-SRC EXISTS? TFALSE
   CST-SCRIPT-DST FILE? TTRUE
   CST-SCRIPT-DST CST-X? TTRUE ;

: CST-TEST-CODESIGN ( -- )
   s" /bin/echo" CST-SIGN CST-CP
   CST-SIGN CHMOD-X
   CST-SIGN CODESIGN-FORCE
   CST-SIGN CODESIGN-VERIFY ;

: CST-TEST-SIGNED-PROMOTE ( -- )
   s" /bin/echo" CST-SIGNED-SRC CST-CP
   CST-SIGNED-SRC CST-SIGNED-DST PROMOTE-SIGNED-EXECUTABLE
   CST-SIGNED-SRC EXISTS? TFALSE
   CST-SIGNED-DST FILE? TTRUE
   CST-SIGNED-DST CST-X? TTRUE
   CST-SIGNED-DST CODESIGN-VERIFY ;

: CST-EXPECT-UNSIGNED-VERIFY-RC ( -- )
   HB-TARGET-LINUX? if
      CST-ENSURE CODESIGN-VERIFY-RC 0 T=
      exit
   then
   HB-TARGET-MACOS? if
      CST-ENSURE CODESIGN-VERIFY-RC 0 T<>
      exit
   then
   E-BUILD-SOURCE throw ;

: CST-TEST-ENSURE ( -- )
   CST-ENSURE s" #!/bin/sh\nexit 0\n" WRITE-ALL
   CST-ENSURE CHMOD-X
   CST-EXPECT-UNSIGNED-VERIFY-RC
   CST-ENSURE CODESIGN-ENSURE
   CST-ENSURE CODESIGN-VERIFY ;

: CST-CODESIGN-MISSING ( -- )
   s" /no/such/habu-codesign-test" CODESIGN-VERIFY ;

: CST-PROMOTE-MISSING ( -- )
   s" /no/such/habu-promote-test" CST-SCRIPT-DST PROMOTE-EXECUTABLE ;

: CST-TEST-TOOL ( -- )
   HB-TARGET-LINUX? if
      CODESIGN-TOOL FILE? TFALSE
      exit
   then
   HB-TARGET-MACOS? if
      CODESIGN-TOOL FILE? TTRUE
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
   CLEANUP-RUN
   CST-ROOT EXISTS? TFALSE
   T-REPORT
   s" codesign-test: ok" type cr ;

CODESIGN-TEST-MAIN
