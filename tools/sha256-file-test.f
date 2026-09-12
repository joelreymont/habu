\ sha256-file-test.f - checked fixture coverage for streaming SHA-256 helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f tools/sha256-file-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f

5000 constant SFT-BIG-LEN

create SFT-DG-A 32 allot
create SFT-DG-B 32 allot
create SFT-HEX-A 64 allot
create SFT-HEX-B 64 allot
create SFT-BIG SFT-BIG-LEN allot
create SFT-A100 100 allot
create SFT-ROOT FS-PATH-CAP allot
create SFT-FILE FS-PATH-CAP allot
create SFT-MISSING FS-PATH-CAP allot
create SFT-DEEP-DIR FS-PATH-CAP allot
create SFT-DEEP FS-PATH-CAP allot
create SFT-DEEP-NAME 230 allot
create SFT-EDGE-A FS-PATH-CAP allot
create SFT-EDGE-B FS-PATH-CAP allot
create SFT-EDGE-NAME 255 allot

variable SFT-ROOT-U
variable SFT-FILE-U
variable SFT-MISSING-U
variable SFT-DEEP-DIR-U
variable SFT-DEEP-U
variable SFT-EDGE-U

: SFT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: SFT-ROOT$ ( -- ptr u8 n )
   SFT-ROOT SFT-ROOT-U @ ;

: SFT-FILE$ ( -- ptr u8 n )
   SFT-FILE SFT-FILE-U @ ;

: SFT-MISSING$ ( -- ptr u8 n )
   SFT-MISSING SFT-MISSING-U @ ;

: SFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-sha256-file-test" TMPDIR-MKDIR SFT-ROOT SFT-ROOT-U SFT-COPY!
   SFT-ROOT$ CLEANUP-TREE+
   SFT-ROOT$ s" big.bin" SFT-FILE JOIN-PATH SFT-FILE-U !
   SFT-ROOT$ s" missing.bin" SFT-MISSING JOIN-PATH SFT-MISSING-U ! ;

: SFT-FILL-BIG ( -- )
   SFT-BIG-LEN 0 ?do
      i 251 mod SFT-BIG i + c!
   loop ;

: SFT-FILL-A100 ( -- )
   100 0 ?do
      97 SFT-A100 i + c!
   loop ;

: SFT-SHA-HEX= ( ptr u8 n ptr u8 n -- ) {: a:ptr u expect:ptr eu :}
   a u SFT-DG-A SHA256
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-HEX-A 64 expect eu T$= ;

: SFT-TEST-FIPS-HEX ( -- )
   SFT-BIG 0
   s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" SFT-SHA-HEX=
   s" abc"
   s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" SFT-SHA-HEX=
   s" abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   s" 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" SFT-SHA-HEX=
   SFT-FILL-A100
   SFT-A100 100
   s" 2816597888e4a0d3a36b82b83316ab32680eb8f00f8cd3b904d681246d285a0e" SFT-SHA-HEX= ;

: SFT-TEST-INCREMENTAL ( -- )
   SHA256-RESET
   s" a" SHA256-UPDATE
   s" bc" SHA256-UPDATE
   SFT-DG-A SHA256-FINAL
   s" abc" SFT-DG-B SHA256
   SFT-DG-A 32 SFT-DG-B 32 T$= ;

: SFT-TEST-FILE ( -- )
   SFT-FILL-BIG
   SFT-FILE$ SFT-BIG SFT-BIG-LEN WRITE-ALL
   SFT-BIG SFT-BIG-LEN SFT-DG-A SHA256
   SFT-FILE$ SFT-DG-B SHA256-FILE 0 T=
   SFT-DG-A 32 SFT-DG-B 32 T$=
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-FILE$ SFT-HEX-B SHA256-FILE-HEX 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-TEST-MISSING ( -- )
   SFT-MISSING$ SFT-DG-A SHA256-FILE SHA-E-OPEN T= ;

\ A file whose path is longer than the 256 bytes the core's path buffer once
\ held: the digest words take every path lib/fs.f takes (dot
\ habu-digest-files-through-308dc561). One 230-byte component keeps the name
\ under NAME_MAX while the whole path passes 256.
: SFT-DEEP$ ( -- ptr u8 n )
   SFT-DEEP SFT-DEEP-U @ ;

: SFT-TEST-DEEP-PATH ( -- )
   230 0 ?do 100 SFT-DEEP-NAME i + c! loop
   SFT-ROOT$ SFT-DEEP-NAME 230 SFT-DEEP-DIR JOIN-PATH SFT-DEEP-DIR-U !
   SFT-DEEP-DIR SFT-DEEP-DIR-U @ MAKE-DIRS
   SFT-DEEP-DIR SFT-DEEP-DIR-U @ s" deep.bin" SFT-DEEP JOIN-PATH SFT-DEEP-U !
   SFT-DEEP-U @ 256 > TTRUE
   SFT-FILL-A100
   SFT-DEEP$ SFT-A100 100 WRITE-ALL
   SFT-A100 100 SFT-DG-A SHA256
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-DEEP$ SFT-HEX-B SHA256-FILE-HEX 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

\ The top of the range: a path of exactly FS-PATH-CAP bytes, built from
\ 100-byte directory components and a final name that lands on the cap. fs.f
\ creates and writes it, and the digest words take it too; one byte more is
\ refused by both layers alike.
: SFT-EDGE-NAME! ( n -- ) {: n:n :}
   n 0 ?do 101 SFT-EDGE-NAME i + c! loop ;

: SFT-EDGE-DIRS ( -- )   \ extend SFT-EDGE-A by 100-byte components while more than 201 bytes remain
   SFT-ROOT$ SFT-EDGE-A swap BYTE-COPY SFT-ROOT-U @ SFT-EDGE-U !
   begin FS-PATH-CAP SFT-EDGE-U @ - 201 > while
      100 SFT-EDGE-NAME!
      SFT-EDGE-A SFT-EDGE-U @ SFT-EDGE-NAME 100 SFT-EDGE-B JOIN-PATH {: u:n :}
      SFT-EDGE-B SFT-EDGE-A u BYTE-COPY u SFT-EDGE-U !
   repeat
   SFT-EDGE-A SFT-EDGE-U @ MAKE-DIRS ;

: SFT-TEST-EDGE-PATH ( -- )
   SFT-EDGE-DIRS
   FS-PATH-CAP SFT-EDGE-U @ - 1- {: last:n :}
   last SFT-EDGE-NAME!
   SFT-EDGE-A SFT-EDGE-U @ SFT-EDGE-NAME last SFT-EDGE-B JOIN-PATH SFT-EDGE-U !
   SFT-EDGE-U @ FS-PATH-CAP T=
   SFT-FILL-A100
   SFT-EDGE-B SFT-EDGE-U @ SFT-A100 100 WRITE-ALL
   SFT-A100 100 SFT-DG-A SHA256
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-EDGE-B SFT-EDGE-U @ SFT-HEX-B SHA256-FILE-HEX 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-MAIN ( -- )
   T-RESET
   SFT-PREPARE
   SFT-TEST-FIPS-HEX
   SFT-TEST-INCREMENTAL
   SFT-TEST-FILE
   SFT-TEST-MISSING
   SFT-TEST-DEEP-PATH
   SFT-TEST-EDGE-PATH
   CLEANUP-RUN
   T-REPORT
   s" sha256-file-test: ok" type cr ;

SFT-MAIN
