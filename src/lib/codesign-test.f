\ codesign-test.f - focused tests for src/lib/codesign.f.
\ Run: bin/hb --load src/lib/codesign-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/le.f
require src/lib/be.f
require src/lib/process-env.f
require src/lib/engine-id.f
require src/lib/codesign.f

package CODESIGN-TEST

variable CST-ROOT-U
variable CST-SIGN-U
variable CST-ENSURE-U
variable CST-RESIGN-U
variable CST-KEEP-U
variable CST-ELF-U
variable CST-UNSIGNED-U
variable CST-IN-U

create CST-ROOT-BUF PATH-CAP allot
create CST-SIGN-BUF PATH-CAP allot
create CST-ENSURE-BUF PATH-CAP allot
create CST-RESIGN-BUF PATH-CAP allot
create CST-KEEP-BUF PATH-CAP allot
create CST-ELF-BUF PATH-CAP allot
create CST-UNSIGNED-BUF PATH-CAP allot
create CST-IN-BUF PATH-CAP allot

: CST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: CST-ROOT ( -- ptr u8 n )
   CST-ROOT-BUF CST-ROOT-U @ ;

: CST-SIGN ( -- ptr u8 n )
   CST-SIGN-BUF CST-SIGN-U @ ;

: CST-ENSURE ( -- ptr u8 n )
   CST-ENSURE-BUF CST-ENSURE-U @ ;

: CST-RESIGN ( -- ptr u8 n )
   CST-RESIGN-BUF CST-RESIGN-U @ ;

: CST-KEEP ( -- ptr u8 n )
   CST-KEEP-BUF CST-KEEP-U @ ;

: CST-ELF ( -- ptr u8 n )
   CST-ELF-BUF CST-ELF-U @ ;

: CST-UNSIGNED ( -- ptr u8 n )
   CST-UNSIGNED-BUF CST-UNSIGNED-U @ ;

: CST-IN ( -- ptr u8 n )
   CST-IN-BUF CST-IN-U @ ;

: CST-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-codesign" FS:MKTEMP-DIR {: a:ptr u :}
   a u CST-ROOT-BUF CST-ROOT-U CST-COPY!
   CST-ROOT FS:CLEANUP-TREE+
   CST-ROOT s" sign-echo" CST-SIGN-BUF CST-SIGN-U CST-PATH!
   CST-ROOT s" ensure-script" CST-ENSURE-BUF CST-ENSURE-U CST-PATH!
   CST-ROOT s" ensure-resign" CST-RESIGN-BUF CST-RESIGN-U CST-PATH!
   CST-ROOT s" ensure-keep" CST-KEEP-BUF CST-KEEP-U CST-PATH!
   CST-ROOT s" elf" CST-ELF-BUF CST-ELF-U CST-PATH!
   CST-ROOT s" unsigned-script" CST-UNSIGNED-BUF CST-UNSIGNED-U CST-PATH! ;

: CST-CP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   PROC-ARGV-RESET
   src srcu  >LEN PROC-ARGV+
   dst dstu  >LEN PROC-ARGV+
   s" /bin/cp" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: CST-TOOL-RC ( -- n )
   CODESIGN:TOOL >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: CST-TEAM ( -- ptr u8 n )
   s" 8TW82D98K7" ;

DYNAMIC-BUFFER CST-OUT u8
DYNAMIC-BUFFER CST-ERR u8
variable CST-ERR-U

: CST-OUT-ROOM ( n -- ptr u8 )
   CST-OUT-RESERVE 0 CST-OUT ;

: CST-ERR-ROOM ( n -- ptr u8 )
   CST-ERR-RESERVE 0 CST-ERR ;

: CST-RELEASE ( -- )
   CST-OUT-RELEASE
   CST-ERR-RELEASE ;

\ Runs tool with PROC-ARGV and stdin in; its stderr stays in CST-ERR.
: CST-CAPTURE ( ptr u8 n ptr u8 n -- n ) {: tool:ptr toolu:n in:ptr inu:n :}
   PROC-ENV-RESET
   tool toolu >LEN in inu >LEN
   [: CST-OUT-ROOM ;] [: CST-ERR-ROOM ;]
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME
   {: o:len e:len oc :}
   e LEN>N CST-ERR-U !
   oc PROCESS:ENDING>RC RC>N ;

: CST-TEAM-REQ ( ptr u8 n -- ptr u8 n ) {: id:ptr idu :}
   SB-RESET
   s\" =anchor apple generic and identifier \"" SB-APPEND
   id idu SB-APPEND
   s\" \" and certificate leaf[subject.OU] = \"" SB-APPEND
   CST-TEAM SB-APPEND
   s\" \"" SB-APPEND
   SB-BUF SB-LEN @ ;

\ codesign -v exits 3 for a valid signature of another signer, ad hoc included.
: CST-EXPECT-TEAM ( ptr u8 n ptr u8 n -- ) {: a:ptr u id:ptr idu :}
   HB-TARGET-MACOS? 0= if exit then
   id idu CST-TEAM-REQ CST-IN-BUF CST-IN-U CST-COPY!
   PROC-ARGV-RESET
   s" -v" >LEN PROC-ARGV+
   s" -R" >LEN PROC-ARGV+
   CST-IN >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   CST-TOOL-RC 0 T= ;

: CST-TEST-CODESIGN ( -- )
   s" /bin/echo" CST-SIGN CST-CP
   CST-SIGN FS:CHMOD-X
   CST-SIGN CODESIGN:FORCE
   CST-SIGN CODESIGN:VERIFY
   CST-SIGN s" com.apple.echo" CST-EXPECT-TEAM ;

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
   CST-ENSURE CODESIGN:VERIFY
   CST-ENSURE s" ensure-script" CST-EXPECT-TEAM ;

\ A copy of /bin/echo carries Apple's valid signature, as a written image carries
\ its ad-hoc one: ENSURE still signs it for the team.
: CST-TEST-ENSURE-RESIGNS ( -- )
   s" /bin/echo" CST-RESIGN CST-CP
   CST-RESIGN CODESIGN:VERIFY
   CST-RESIGN CODESIGN:ENSURE
   CST-RESIGN s" com.apple.echo" CST-EXPECT-TEAM ;

\ ENSURE keeps a valid team signature: signing again would drop its runtime
\ flag, which `codesign -dv` names on stderr.
: CST-TEST-ENSURE-KEEPS ( -- )
   HB-TARGET-MACOS? 0= if exit then
   s" /bin/echo" CST-KEEP CST-CP
   PROC-ARGV-RESET
   s" -s" >LEN PROC-ARGV+
   CODESIGN:IDENTITY$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   s" runtime" >LEN PROC-ARGV+
   s" --timestamp=none" >LEN PROC-ARGV+
   s" --force" >LEN PROC-ARGV+
   CST-KEEP >LEN PROC-ARGV+
   CST-TOOL-RC 0 T=
   CST-KEEP CODESIGN:ENSURE
   PROC-ARGV-RESET
   s" -dv" >LEN PROC-ARGV+
   CST-KEEP >LEN PROC-ARGV+
   CODESIGN:TOOL s" " CST-CAPTURE 0 T=
   0 CST-ERR CST-ERR-U @ s" (runtime)" CONTAINS? TTRUE
   CST-RELEASE ;

\ A Linux executable built on a Mac (--target) is an ELF file: ENSURE leaves it
\ unsigned, as a Linux host does.
: CST-TEST-ELF ( -- )
   CST-ELF s\" \x7FELF\x02\x01\x01\z" FS:WRITE-ALL
   CST-ELF FS:CHMOD-X
   CST-ELF CODESIGN:ENSURE
   HB-TARGET-MACOS? 0= if exit then
   PROC-ARGV-RESET
   s" -v" >LEN PROC-ARGV+
   CST-ELF >LEN PROC-ARGV+
   CST-TOOL-RC 0 T<> ;

\ What a session prints is under test, so a child engine runs CST-IN from stdin;
\ its exit status is returned.
: CST-CHILD ( -- n )
   SB-BUF SB-LEN @ CST-IN-BUF CST-IN-U CST-COPY!
   PROC-ARGV-RESET
   ENGINE-ID:PATH$ CST-IN CST-CAPTURE ;

: CST-ERR-HAS? ( ptr u8 n -- bool )
   {: a:ptr u :}
   0 CST-ERR CST-ERR-U @ a u CONTAINS? ;

: CST-UNSIGNED-CHILD ( ptr u8 n -- n ) {: w:ptr wu :}
   SB-RESET
   s\" require src/lib/codesign.f\ns\" " SB-APPEND
   CST-UNSIGNED SB-APPEND
   s\" \" CODESIGN:" SB-APPEND
   w wu SB-APPEND
   s\" \n" SB-APPEND
   CST-CHILD ;

\ codesign's own reason for a nonzero status reaches stderr when VERIFY throws;
\ VERIFY-RC returns the status alone and prints nothing.
: CST-TEST-STATUS-STDERR ( -- )
   HB-TARGET-MACOS? 0= if exit then
   CST-UNSIGNED s\" #!/bin/sh\nexit 0\n" FS:WRITE-ALL
   CST-UNSIGNED FS:CHMOD-X
   s" VERIFY" CST-UNSIGNED-CHILD drop
   s" not signed at all" CST-ERR-HAS? TTRUE
   CST-RELEASE
   s" VERIFY-RC drop" CST-UNSIGNED-CHILD 0 T=
   CST-ERR-U @ 0 T=
   CST-RELEASE ;

: CST-CODESIGN-MISSING ( -- )
   s" /no/such/habu-codesign-test" CODESIGN:VERIFY ;

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
   CST-TEST-CODESIGN
   CST-TEST-ENSURE
   CST-TEST-SAME-IMAGE
   CST-TEST-ENSURE-RESIGNS
   CST-TEST-ENSURE-KEEPS
   CST-TEST-ELF
   CST-TEST-STATUS-STDERR
   [: CST-CODESIGN-MISSING ;] E-BUILD-PATH TTHROWSQ
   FS:CLEANUP-RUN
   CST-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" codesign-test: ok" type cr ;

CODESIGN-TEST-MAIN

;package
