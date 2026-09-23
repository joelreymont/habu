\ sha256-file-test.f - checked fixture coverage for streaming SHA-256 helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f tools/sha256-file-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f                \ the two-task row: a context per thread
require lib/adt/result.f          \ TASK:JOIN's answer is MATCHed here

5000 constant SFT-BIG-LEN
20 constant SFT-TASK-ROUNDS       \ a race need not lose the first round

create SFT-DG-A 32 allot
create SFT-DG-B 32 allot
create SFT-DG-C 32 allot
create SFT-DG-D 32 allot
create SFT-DG-E 32 allot
create SFT-HEX-A 64 allot
create SFT-HEX-B 64 allot
create SFT-CTX-A SHA256-CTX-BYTES allot
create SFT-CTX-B SHA256-CTX-BYTES allot
create SFT-CTX-C SHA256-CTX-BYTES allot
create SFT-ODD 1 allot            \ R6: one byte, so the next `create` has to align
create SFT-CTX-D SHA256-CTX-BYTES allot
create SFT-FCTX-A SHA256-FILE-CTX-BYTES allot
create SFT-FCTX-B SHA256-FILE-CTX-BYTES allot
create SFT-BIG SFT-BIG-LEN allot
create SFT-BIG1 SFT-BIG-LEN allot
create SFT-NIB 32 allot
create SFT-A100 100 allot
create SFT-ROOT FS-PATH-CAP allot
create SFT-FILE FS-PATH-CAP allot
create SFT-FILE1 FS-PATH-CAP allot
create SFT-MISSING FS-PATH-CAP allot
create SFT-DEEP-DIR FS-PATH-CAP allot
create SFT-DEEP FS-PATH-CAP allot
create SFT-DEEP-NAME 230 allot
create SFT-EDGE-A FS-PATH-CAP allot
create SFT-EDGE-B FS-PATH-CAP allot
create SFT-EDGE-NAME 255 allot

variable SFT-ROOT-U
variable SFT-FILE-U
variable SFT-FILE1-U
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

: SFT-FILE1$ ( -- ptr u8 n )
   SFT-FILE1 SFT-FILE1-U @ ;

: SFT-MISSING$ ( -- ptr u8 n )
   SFT-MISSING SFT-MISSING-U @ ;

: SFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-sha256-file-test" HB-TMP-MKDIR SFT-ROOT SFT-ROOT-U SFT-COPY!
   SFT-ROOT$ CLEANUP-TREE+
   SFT-ROOT$ s" big.bin" SFT-FILE JOIN-PATH SFT-FILE-U !
   SFT-ROOT$ s" big1.bin" SFT-FILE1 JOIN-PATH SFT-FILE1-U !
   SFT-ROOT$ s" missing.bin" SFT-MISSING JOIN-PATH SFT-MISSING-U ! ;

: SFT-FILL-BIG ( -- )
   SFT-BIG-LEN 0 ?do
      i 251 mod SFT-BIG i + c!
   loop ;

: SFT-FILL-BIG1 ( -- )            \ the same pattern, every byte flipped
   SFT-BIG-LEN 0 ?do
      SFT-BIG i + c@ 1 xor $FF and SFT-BIG1 i + c!
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

\ ---- contexts -----------------------------------------------------------------
\ A digest in progress lives in the caller's span, so these rows hold two and
\ three of them at once and each one answers for its own bytes. Every digest is
\ compared against the one-shot SHA256 of the same bytes.

: SFT-DIGEST= ( ptr u8 ptr u8 -- ) {: got want :}   \ two $20-byte digests, as hex
   got SFT-HEX-A SHA256>HEX
   want SFT-HEX-B SHA256>HEX
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-ONE-SHOT= ( ptr u8 ptr u8 n -- ) {: got a u:n :}
   a u SFT-DG-E SHA256
   got SFT-DG-E SFT-DIGEST= ;

: SFT-FEED-PIECES ( ptr u8 ptr u8 n n -- ) {: ctx a u:n piece:n :}
   u 0 ?do
      ctx  a i ZPTR+  u i - piece min  SHA256-FEED
   piece +loop ;

\ Both contexts over the same bytes, alternating: one byte to the first for every
\ $40 bytes to the second, so the two digests are interleaved inside each block.
: SFT-FEED-BOTH ( ptr u8 ptr u8 ptr u8 n -- ) {: ca cb a u:n :}
   u 0 ?do
      ca  a i ZPTR+  1  SHA256-FEED
      i $40 mod 0= if
         cb  a i ZPTR+  u i - $40 min  SHA256-FEED
      then
   loop ;

\ R1. The interleaving of /home/joel/.cache/tender/habu-gaps/sha-call-state:
\ one context is opened and left open across another's whole digest. The first
\ pair is the FIPS "abc" vector split "a" + "bc" against "abc" whole; the second
\ runs the 5000-byte pattern through both in different piece sizes and ends them
\ in the opposite order.
: SFT-TEST-INTERLEAVED ( -- )
   SFT-CTX-A SHA256-BEGIN
   SFT-CTX-A s" a" SHA256-FEED
   SFT-CTX-B SHA256-BEGIN
   SFT-CTX-B s" abc" SHA256-FEED
   SFT-CTX-A s" bc" SHA256-FEED
   SFT-CTX-B SFT-DG-B SHA256-END
   SFT-CTX-A SFT-DG-A SHA256-END
   SFT-DG-A s" abc" SFT-ONE-SHOT=
   SFT-DG-B s" abc" SFT-ONE-SHOT=
   SFT-FILL-BIG
   SFT-CTX-A SHA256-BEGIN
   SFT-CTX-B SHA256-BEGIN
   SFT-CTX-A SFT-CTX-B SFT-BIG SFT-BIG-LEN SFT-FEED-BOTH
   SFT-CTX-B SFT-DG-B SHA256-END
   SFT-CTX-A SFT-DG-A SHA256-END
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT=
   SFT-DG-B SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT= ;

\ R2. A context is a plain span and nothing else: copying its bytes copies the
\ digest in progress, and the copy finishes on its own bytes. The original is fed
\ again after the copy is taken and the copy is ended first.
: SFT-TEST-CONTEXT-COPY ( -- )
   SFT-FILL-BIG
   SFT-CTX-A SHA256-BEGIN
   SFT-CTX-A SFT-BIG 100 SHA256-FEED
   SFT-CTX-A SFT-CTX-C SHA256-CTX-BYTES BYTE-COPY
   SFT-CTX-A  SFT-BIG 100 ZPTR+  SFT-BIG-LEN 100 -  7 SFT-FEED-PIECES
   SFT-CTX-C  SFT-BIG 100 ZPTR+  SFT-BIG-LEN 100 -  $40 SFT-FEED-PIECES
   SFT-CTX-C SFT-DG-C SHA256-END
   SFT-CTX-A SFT-DG-A SHA256-END
   SFT-DG-C SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT=
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT= ;

TASK:MIN-STACK TASK:TASK SFT-TASK-A
TASK:MIN-STACK TASK:TASK SFT-TASK-B

: SFT-WORK-A ( -- )
   SFT-CTX-A SHA256-BEGIN
   SFT-CTX-A SFT-BIG SFT-BIG-LEN 1 SFT-FEED-PIECES
   SFT-CTX-A SFT-DG-A SHA256-END
   0 TASK:RETURN ;

: SFT-WORK-B ( -- )
   SFT-CTX-B SHA256-BEGIN
   SFT-CTX-B SFT-BIG1 SFT-BIG-LEN 1 SFT-FEED-PIECES
   SFT-CTX-B SFT-DG-B SHA256-END
   0 TASK:RETURN ;

: SFT-JOIN-OK ( result<n,n> -- )   \ ok, and the worker's own answer
   MATCH result ok OF 0 ENDOF err OF 1 ENDOF ;MATCH
   0 T=
   0 T= ;

\ R3. Two tasks are two pthreads (docs/threads.md): each hashes its own
\ 5000-byte pattern a byte at a time through its own context, so the two digests
\ are interleaved inside every block the scheduler lets them share. A cell shared
\ anywhere on the streaming path shows up as a digest that is not its bytes'.
: SFT-TEST-TASKS ( -- )
   SFT-FILL-BIG
   SFT-FILL-BIG1
   SFT-BIG SFT-BIG-LEN SFT-DG-C SHA256
   SFT-BIG1 SFT-BIG-LEN SFT-DG-D SHA256
   SFT-TASK-ROUNDS 0 ?do
      ['] SFT-WORK-A SFT-TASK-A TASK:ACTIVATE
      ['] SFT-WORK-B SFT-TASK-B TASK:ACTIVATE
      SFT-TASK-A TASK:JOIN SFT-JOIN-OK
      SFT-TASK-B TASK:JOIN SFT-JOIN-OK
      SFT-DG-A SFT-DG-C SFT-DIGEST=
      SFT-DG-B SFT-DG-D SFT-DIGEST=
   loop ;

\ R4. The layout is a promise to every caller that allocates a context, so the
\ size is pinned here and a change to it is deliberate.
: SFT-TEST-CTX-BYTES ( -- )
   SHA256-CTX-BYTES 784 T= ;

\ R6. The hash words are cells, so a context has to be cell-aligned, and every
\ caller gets that from `create` - including this one, defined right after a
\ one-byte allot. AArch64 would not fault on the misaligned case, so the address
\ itself is the assertion; the digest then shows the aligned span works.
: SFT-TEST-ALIGNED-CREATE ( -- )
   SFT-CTX-D BYTE-VIEW NULL-PTR BYTE-VIEW - CELL mod 0 T=
   SFT-FILL-BIG
   SFT-CTX-D SHA256-BEGIN
   SFT-CTX-D SFT-BIG SFT-BIG-LEN 7 SFT-FEED-PIECES
   SFT-CTX-D SFT-DG-A SHA256-END
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT= ;

\ R5. SHA256>HEX reads and writes only the caller's two spans now; every nibble
\ value proves it still renders each half of a byte in the right place.
: SFT-FILL-NIB ( -- )
   32 0 ?do
      i 8 mod 2 * {: hi:n :}
      hi 16 * hi 1 + + $FF and SFT-NIB i + c!
   loop ;

: SFT-TEST-HEX ( -- )
   SFT-FILL-NIB
   SFT-NIB SFT-HEX-A SHA256>HEX
   SFT-HEX-A 64
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" T$= ;

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

\ R7. A file context is the caller's span too: the -IN words answer exactly what
\ the one-shot wrappers answer over the same file, and a missing file answers
\ SHA-E-OPEN through the context word as well.
: SFT-TEST-FILE-CTX ( -- )
   SHA256-FILE-CTX-BYTES 5944 T=
   SFT-FILL-BIG
   SFT-FILE$ SFT-BIG SFT-BIG-LEN WRITE-ALL
   SFT-FCTX-A SFT-FILE$ SFT-DG-A SHA256-FILE-IN 0 T=
   SFT-FILE$ SFT-DG-B SHA256-FILE 0 T=
   SFT-DG-A SFT-DG-B SFT-DIGEST=
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT=
   SFT-FCTX-A SFT-FILE$ SFT-HEX-A SHA256-FILE-HEX-IN 0 T=
   SFT-FILE$ SFT-HEX-B SHA256-FILE-HEX 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$=
   SFT-FCTX-A SFT-MISSING$ SFT-DG-A SHA256-FILE-IN SHA-E-OPEN T= ;

: SFT-FILE-WORK-A ( -- )
   SFT-FCTX-A SFT-FILE$ SFT-DG-A SHA256-FILE-IN TASK:RETURN ;

: SFT-FILE-WORK-B ( -- )
   SFT-FCTX-B SFT-FILE1$ SFT-DG-B SHA256-FILE-IN TASK:RETURN ;

\ R8. Two tasks hash two files at once, each through its own file context. The
\ descriptor and the byte count are locals and the read buffer, the path handed
\ to the open and the digest state are the context's, so neither task can be
\ handed the other's bytes - a shared one shows up as a digest that is not its
\ file's.
: SFT-TEST-FILE-TASKS ( -- )
   SFT-FILL-BIG
   SFT-FILL-BIG1
   SFT-FILE$ SFT-BIG SFT-BIG-LEN WRITE-ALL
   SFT-FILE1$ SFT-BIG1 SFT-BIG-LEN WRITE-ALL
   SFT-BIG SFT-BIG-LEN SFT-DG-C SHA256
   SFT-BIG1 SFT-BIG-LEN SFT-DG-D SHA256
   SFT-TASK-ROUNDS 0 ?do
      ['] SFT-FILE-WORK-A SFT-TASK-A TASK:ACTIVATE
      ['] SFT-FILE-WORK-B SFT-TASK-B TASK:ACTIVATE
      SFT-TASK-A TASK:JOIN SFT-JOIN-OK
      SFT-TASK-B TASK:JOIN SFT-JOIN-OK
      SFT-DG-A SFT-DG-C SFT-DIGEST=
      SFT-DG-B SFT-DG-D SFT-DIGEST=
   loop ;

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
   SFT-TEST-CTX-BYTES
   SFT-TEST-INTERLEAVED
   SFT-TEST-CONTEXT-COPY
   SFT-TEST-TASKS
   SFT-TEST-ALIGNED-CREATE
   SFT-TEST-HEX
   SFT-TEST-FILE
   SFT-TEST-MISSING
   SFT-TEST-FILE-CTX
   SFT-TEST-FILE-TASKS
   SFT-TEST-DEEP-PATH
   SFT-TEST-EDGE-PATH
   CLEANUP-RUN
   T-REPORT
   s" sha256-file-test: ok" type cr ;

SFT-MAIN
