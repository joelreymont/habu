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
require src/lib/le.f
require src/lib/be.f
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

\ Two images shaped as a 64-bit Mach-O: one load command, code bytes, and an
\ embedded-signature superblob indexing a CodeDirectory and a CMS blob.
$FEEDFACF constant CST-MACHO-MAGIC
$1D constant CST-LC-SIGNATURE
$19 constant CST-LC-SEGMENT
124 constant CST-IMG-BYTES
64 constant CST-SIG-OFF
60 constant CST-SIG-BYTES
48 constant CST-CODE-OFF
92 constant CST-CD-OFF
108 constant CST-CMS-OFF
8 constant CST-BLOB-HEAD
create CST-IMG-A 128 allot
create CST-IMG-B 128 allot

: CST-BLOB ( n n ptr u8 n -- ) {: magic:n fill:n a:ptr off:n :}
   magic a off + BE:U32!
   16 a off + 4 + BE:U32!
   8 0 ?do fill i + a off CST-BLOB-HEAD + i + + c! loop ;

: CST-SUPERBLOB ( ptr u8 -- ) {: a:ptr :}
   $FADE0CC0 a CST-SIG-OFF + BE:U32!
   CST-SIG-BYTES a CST-SIG-OFF + 4 + BE:U32!
   2 a CST-SIG-OFF + 8 + BE:U32!
   0 a CST-SIG-OFF + 12 + BE:U32!
   CST-CD-OFF CST-SIG-OFF - a CST-SIG-OFF + 16 + BE:U32!
   $10000 a CST-SIG-OFF + 20 + BE:U32!
   CST-CMS-OFF CST-SIG-OFF - a CST-SIG-OFF + 24 + BE:U32!
   $FADE0C02 $40 a CST-CD-OFF CST-BLOB
   $FADE0B01 $60 a CST-CMS-OFF CST-BLOB ;

\ The command is CST-LC-SIGNATURE for a signed image, another for an unsigned
\ one; the signature offset is the command's, so it may lie past the image.
: CST-IMAGE ( ptr u8 n n -- ) {: a:ptr cmd:n sig:n :}
   128 0 ?do 0 a i + c! loop
   CST-MACHO-MAGIC a LE:U32!
   1 a 16 + LE:U32!
   16 a 20 + LE:U32!
   cmd a 32 + LE:U32!
   16 a 36 + LE:U32!
   sig a 40 + LE:U32!
   CST-SIG-BYTES a 44 + LE:U32!
   CST-SIG-OFF CST-CODE-OFF - 0 ?do i 1 + a CST-CODE-OFF + i + c! loop
   a CST-SUPERBLOB ;

\ A header and one 8-byte LC_CODE_SIGNATURE that ends the image; the bytes
\ after it, where a 16-byte command's dataoff and datasize would sit, name an
\ empty superblob inside the header.
40 constant CST-SHORT-BYTES
: CST-SHORT-SIG ( ptr u8 -- ) {: a:ptr :}
   128 0 ?do 0 a i + c! loop
   CST-MACHO-MAGIC a LE:U32!
   1 a 16 + LE:U32!
   8 a 20 + LE:U32!
   CST-LC-SIGNATURE a 32 + LE:U32!
   8 a 36 + LE:U32!
   4 a 40 + LE:U32!
   12 a 44 + LE:U32!
   $FADE0CC0 a 4 + BE:U32!
   12 a 8 + BE:U32! ;

: CST-PAIR ( n n -- ) {: cmd:n sig:n :}
   CST-IMG-A cmd sig CST-IMAGE
   CST-IMG-B cmd sig CST-IMAGE ;

: CST-FLIP ( n -- ) {: off:n :}
   CST-IMG-B off + c@ 1 xor CST-IMG-B off + c! ;

: CST-SAME-IMAGE? ( n -- bool ) {: bu:n :}
   CST-IMG-A CST-IMG-BYTES CST-IMG-B bu CODESIGN:SAME-IMAGE? ;

: CST-TEST-SAME-IMAGE ( -- )
   s" images that differ only in the CMS blob are the same image" T-LABEL
   CST-LC-SIGNATURE CST-SIG-OFF CST-PAIR
   CST-IMG-BYTES CST-SAME-IMAGE? TTRUE
   CST-CMS-OFF CST-BLOB-HEAD + 3 + CST-FLIP
   CST-IMG-BYTES CST-SAME-IMAGE? TTRUE
   s" a CodeDirectory byte differs" T-LABEL
   CST-CD-OFF CST-BLOB-HEAD + 3 + CST-FLIP
   CST-IMG-BYTES CST-SAME-IMAGE? TFALSE
   s" a code byte differs" T-LABEL
   CST-LC-SIGNATURE CST-SIG-OFF CST-PAIR
   CST-CODE-OFF 2 + CST-FLIP
   CST-IMG-BYTES CST-SAME-IMAGE? TFALSE
   s" the lengths differ" T-LABEL
   CST-LC-SIGNATURE CST-SIG-OFF CST-PAIR
   CST-IMG-BYTES 1 + CST-SAME-IMAGE? TFALSE
   s" an image without LC_CODE_SIGNATURE is compared whole" T-LABEL
   CST-LC-SEGMENT CST-SIG-OFF CST-PAIR
   CST-IMG-BYTES CST-SAME-IMAGE? TTRUE
   CST-CMS-OFF CST-BLOB-HEAD + 3 + CST-FLIP
   CST-IMG-BYTES CST-SAME-IMAGE? TFALSE
   s" a signature past the image end is not the same image" T-LABEL
   CST-LC-SIGNATURE 200 CST-PAIR
   CST-IMG-BYTES CST-SAME-IMAGE? TFALSE
   s" a short LC_CODE_SIGNATURE at the image end is not the same image" T-LABEL
   CST-IMG-A CST-SHORT-SIG  CST-IMG-B CST-SHORT-SIG
   CST-IMG-A CST-SHORT-BYTES CST-IMG-B CST-SHORT-BYTES CODESIGN:SAME-IMAGE? TFALSE ;

: CODESIGN-TEST-MAIN ( -- )
   T-RESET
   CST-PREPARE
   CST-TEST-TOOL
   CST-TEST-PROMOTE
   CST-TEST-CODESIGN
   CST-TEST-SIGNED-PROMOTE
   CST-TEST-ENSURE
   CST-TEST-SAME-IMAGE
   [: CST-CODESIGN-MISSING ;] E-BUILD-PATH TTHROWSQ
   [: CST-PROMOTE-MISSING ;] E-BUILD-PATH TTHROWSQ
   FS:CLEANUP-RUN
   CST-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" codesign-test: ok" type cr ;

CODESIGN-TEST-MAIN
