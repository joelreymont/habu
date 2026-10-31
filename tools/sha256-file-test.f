\ sha256-file-test.f - checked fixture coverage for streaming SHA-256 helpers.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f
\ src/lib/fs-mutate.f tools/sha256-file-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f                \ the two-task row: a context per thread
require src/lib/adt/result.f          \ TASK:JOIN's answer is MATCHed here

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
create SFT-CTX-ONE SHA256-CTX-BYTES allot   \ the row's own one-shot context
create SFT-CTX-S1 SHA256-CTX-BYTES allot    \ S1: one context per task
create SFT-CTX-S2 SHA256-CTX-BYTES allot
create SFT-DG-S1 32 allot
create SFT-DG-S2 32 allot
create SFT-FCTX-A SHA256-FILE-CTX-BYTES allot
create SFT-FCTX-B SHA256-FILE-CTX-BYTES allot
create SFT-BIG SFT-BIG-LEN allot
create SFT-BIG1 SFT-BIG-LEN allot
create SFT-NIB 32 allot
create SFT-A100 100 allot
create SFT-ROOT PATH-CAP allot
create SFT-FILE PATH-CAP allot
create SFT-FILE1 PATH-CAP allot
create SFT-MISSING PATH-CAP allot
create SFT-DEEP-DIR PATH-CAP allot
create SFT-DEEP PATH-CAP allot
create SFT-DEEP-NAME 230 allot
create SFT-EDGE PATH-CAP allot

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
   FS:CLEANUP-RESET
   s" habu-sha256-file-test" FS:MKTEMP-DIR SOURCE-ROOT:CANONICAL drop SFT-ROOT SFT-ROOT-U SFT-COPY!
   SFT-ROOT$ FS:CLEANUP-TREE+
   SFT-ROOT$ s" big.bin" SFT-FILE FS:JOIN-PATH SFT-FILE-U !
   SFT-ROOT$ s" big1.bin" SFT-FILE1 FS:JOIN-PATH SFT-FILE1-U !
   SFT-ROOT$ s" missing.bin" SFT-MISSING FS:JOIN-PATH SFT-MISSING-U ! ;

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
   SFT-CTX-ONE a u SFT-DG-A SHA256-IN
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
   SFT-CTX-A SHA256-BEGIN
   SFT-CTX-A s" a" SHA256-FEED
   SFT-CTX-A s" bc" SHA256-FEED
   SFT-CTX-A SFT-DG-A SHA256-END
   SFT-CTX-ONE s" abc" SFT-DG-B SHA256-IN
   SFT-DG-A 32 SFT-DG-B 32 T$= ;

\ ---- contexts -----------------------------------------------------------------
\ A digest in progress lives in the caller's span. Every digest is compared
\ against the one-shot SHA256-IN of the same bytes.

: SFT-DIGEST= ( ptr u8 ptr u8 -- ) {: got want :}   \ two $20-byte digests, as hex
   got SFT-HEX-A SHA256>HEX
   want SFT-HEX-B SHA256>HEX
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-ONE-SHOT= ( ptr u8 ptr u8 n -- ) {: got a u:n :}
   SFT-CTX-ONE a u SFT-DG-E SHA256-IN
   got SFT-DG-E SFT-DIGEST= ;

: SFT-FEED-PIECES ( ptr u8 ptr u8 n n -- ) {: ctx a u:n piece:n :}
   u 0 ?do
      ctx  a i ZPTR+  u i - piece min  SHA256-FEED
   piece +loop ;

\ One byte to the first context for every $40 to the second, so the two digests
\ interleave inside each block.
: SFT-FEED-BOTH ( ptr u8 ptr u8 ptr u8 n -- ) {: ca cb a u:n :}
   u 0 ?do
      ca  a i ZPTR+  1  SHA256-FEED
      i $40 mod 0= if
         cb  a i ZPTR+  u i - $40 min  SHA256-FEED
      then
   loop ;

\ R1. One context is left open across another's whole digest.
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

\ R2. A context is a plain span: a copy of its bytes finishes the digest in
\ progress on its own bytes.
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

\ R3. Two tasks (pthreads, src/lib/task.f) each hash a pattern a byte at a time
\ through their own context: a cell shared anywhere on the streaming path shows
\ up as a digest that is not its bytes'.
: SFT-TEST-TASKS ( -- )
   SFT-FILL-BIG
   SFT-FILL-BIG1
   SFT-CTX-ONE SFT-BIG SFT-BIG-LEN SFT-DG-C SHA256-IN
   SFT-CTX-ONE SFT-BIG1 SFT-BIG-LEN SFT-DG-D SHA256-IN
   SFT-TASK-ROUNDS 0 ?do
      ['] SFT-WORK-A SFT-TASK-A TASK:ACTIVATE
      ['] SFT-WORK-B SFT-TASK-B TASK:ACTIVATE
      SFT-TASK-A TASK:JOIN SFT-JOIN-OK
      SFT-TASK-B TASK:JOIN SFT-JOIN-OK
      SFT-DG-A SFT-DG-C SFT-DIGEST=
      SFT-DG-B SFT-DG-D SFT-DIGEST=
   loop ;

: SFT-S1-WORK-A ( -- )
   SFT-CTX-S1 SFT-BIG SFT-BIG-LEN SFT-DG-S1 SHA256-IN
   0 TASK:RETURN ;

: SFT-S1-WORK-B ( -- )
   SFT-CTX-S2 SFT-BIG1 SFT-BIG-LEN SFT-DG-S2 SHA256-IN
   0 TASK:RETURN ;

\ S1. Two tasks run whole one-shot digests of different bytes at once, each on
\ its own context. Both tasks on one context fails this row (measured: the
\ second task's BEGIN throws the first task's digest away).
: SFT-TEST-TASKS-ONE-SHOT ( -- )
   SFT-FILL-BIG
   SFT-FILL-BIG1
   SFT-CTX-ONE SFT-BIG SFT-BIG-LEN SFT-DG-C SHA256-IN
   SFT-CTX-ONE SFT-BIG1 SFT-BIG-LEN SFT-DG-D SHA256-IN
   SFT-TASK-ROUNDS 0 ?do
      ['] SFT-S1-WORK-A SFT-TASK-A TASK:ACTIVATE
      ['] SFT-S1-WORK-B SFT-TASK-B TASK:ACTIVATE
      SFT-TASK-A TASK:JOIN SFT-JOIN-OK
      SFT-TASK-B TASK:JOIN SFT-JOIN-OK
      SFT-DG-S1 SFT-DG-C SFT-DIGEST=
      SFT-DG-S2 SFT-DG-D SFT-DIGEST=
   loop ;

\ R6. The hash words are cells, so a context must be cell-aligned, which
\ `create` gives even right after a one-byte allot. AArch64 does not fault on a
\ misaligned context, so the address itself is the assertion.
: SFT-TEST-ALIGNED-CREATE ( -- )
   SFT-CTX-D BYTE-VIEW NULL-PTR BYTE-VIEW - CELL mod 0 T=
   SFT-FILL-BIG
   SFT-CTX-D SHA256-BEGIN
   SFT-CTX-D SFT-BIG SFT-BIG-LEN 7 SFT-FEED-PIECES
   SFT-CTX-D SFT-DG-A SHA256-END
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT= ;

\ R5. SHA256>HEX reads and writes only the caller's two spans; every nibble
\ value pins where each half of a byte renders.
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
   SFT-FILE$ SFT-BIG SFT-BIG-LEN FS:WRITE-ALL
   SFT-CTX-ONE SFT-BIG SFT-BIG-LEN SFT-DG-A SHA256-IN
   SFT-FCTX-A SFT-FILE$ SFT-DG-B SHA256-FILE-IN 0 T=
   SFT-DG-A 32 SFT-DG-B 32 T$=
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-FCTX-A SFT-FILE$ SFT-HEX-B SHA256-FILE-HEX-IN 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-TEST-MISSING ( -- )
   SFT-FCTX-A SFT-MISSING$ SFT-DG-A SHA256-FILE-IN SHA-E-OPEN T= ;

\ R7. A file context is the caller's span too: the -IN words answer exactly what
\ the one-shot wrappers answer over the same file, and a missing file answers
\ SHA-E-OPEN through the context word as well.
: SFT-TEST-FILE-CTX ( -- )
   SFT-FILL-BIG
   SFT-FILE$ SFT-BIG SFT-BIG-LEN FS:WRITE-ALL
   SFT-FCTX-A SFT-FILE$ SFT-DG-A SHA256-FILE-IN 0 T=
   SFT-FCTX-B SFT-FILE$ SFT-DG-B SHA256-FILE-IN 0 T=
   SFT-DG-A SFT-DG-B SFT-DIGEST=
   SFT-DG-A SFT-BIG SFT-BIG-LEN SFT-ONE-SHOT=
   SFT-FCTX-A SFT-FILE$ SFT-HEX-A SHA256-FILE-HEX-IN 0 T=
   SFT-FCTX-B SFT-FILE$ SFT-HEX-B SHA256-FILE-HEX-IN 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$=
   SFT-FCTX-A SFT-MISSING$ SFT-DG-A SHA256-FILE-IN SHA-E-OPEN T= ;

: SFT-FILE-WORK-A ( -- )
   SFT-FCTX-A SFT-FILE$ SFT-DG-A SHA256-FILE-IN TASK:RETURN ;

: SFT-FILE-WORK-B ( -- )
   SFT-FCTX-B SFT-FILE1$ SFT-DG-B SHA256-FILE-IN TASK:RETURN ;

\ R8. Two tasks hash two files at once, each through its own file context
\ (descriptor and count are locals; buffer, path and state are the context's):
\ a shared one shows up as a digest that is not its file's.
: SFT-TEST-FILE-TASKS ( -- )
   SFT-FILL-BIG
   SFT-FILL-BIG1
   SFT-FILE$ SFT-BIG SFT-BIG-LEN FS:WRITE-ALL
   SFT-FILE1$ SFT-BIG1 SFT-BIG-LEN FS:WRITE-ALL
   SFT-CTX-ONE SFT-BIG SFT-BIG-LEN SFT-DG-C SHA256-IN
   SFT-CTX-ONE SFT-BIG1 SFT-BIG-LEN SFT-DG-D SHA256-IN
   SFT-TASK-ROUNDS 0 ?do
      ['] SFT-FILE-WORK-A SFT-TASK-A TASK:ACTIVATE
      ['] SFT-FILE-WORK-B SFT-TASK-B TASK:ACTIVATE
      SFT-TASK-A TASK:JOIN SFT-JOIN-OK
      SFT-TASK-B TASK:JOIN SFT-JOIN-OK
      SFT-DG-A SFT-DG-C SFT-DIGEST=
      SFT-DG-B SFT-DG-D SFT-DIGEST=
   loop ;

\ A path longer than 256 bytes: the digest words take every path src/lib/fs.f
\ takes. One 230-byte component keeps the name under NAME_MAX.
: SFT-DEEP$ ( -- ptr u8 n )
   SFT-DEEP SFT-DEEP-U @ ;

: SFT-TEST-DEEP-PATH ( -- )
   230 0 ?do 100 SFT-DEEP-NAME i + c! loop
   SFT-ROOT$ SFT-DEEP-NAME 230 SFT-DEEP-DIR FS:JOIN-PATH SFT-DEEP-DIR-U !
   SFT-DEEP-DIR SFT-DEEP-DIR-U @ FS:MAKE-DIRS
   SFT-DEEP-DIR SFT-DEEP-DIR-U @ s" deep.bin" SFT-DEEP FS:JOIN-PATH SFT-DEEP-U !
   SFT-DEEP-U @ 256 > TTRUE
   SFT-FILL-A100
   SFT-DEEP$ SFT-A100 100 FS:WRITE-ALL
   SFT-CTX-ONE SFT-A100 100 SFT-DG-A SHA256-IN
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-FCTX-A SFT-DEEP$ SFT-HEX-B SHA256-FILE-HEX-IN 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

\ A path string of exactly PATH-CAP bytes from the physical root, since a
\ symlink's target also counts against the kernel limit.
: SFT-EDGE$ ( -- ptr u8 n )
   SFT-EDGE SFT-EDGE-U @ ;

: SFT-EDGE+ ( ptr u8 n -- ) {: a:ptr u :}
   a SFT-EDGE SFT-EDGE-U @ + u BYTE-COPY
   SFT-EDGE-U @ u + SFT-EDGE-U ! ;

\ `/.` pairs carry the length, so the file is in the root itself; its name of
\ `e` takes 1 or 2 bytes, so the pairs get an even number of bytes.
: SFT-EDGE-BUILD ( -- )
   SFT-ROOT$ SFT-EDGE swap BYTE-COPY SFT-ROOT-U @ SFT-EDGE-U !
   PATH-CAP SFT-EDGE-U @ - 1- {: m:n :}
   2 m 1 and - {: r:n :}
   m r - 2 / 0 ?do s" /." SFT-EDGE+ loop
   s" /ee" drop r 1+ SFT-EDGE+ ;

: SFT-TEST-EDGE-PATH ( -- )
   SFT-EDGE-BUILD
   SFT-EDGE-U @ PATH-CAP T=
   SFT-FILL-A100
   SFT-EDGE$ SFT-A100 100 FS:WRITE-ALL
   SFT-CTX-ONE SFT-A100 100 SFT-DG-A SHA256-IN
   SFT-DG-A SFT-HEX-A SHA256>HEX
   SFT-FCTX-A SFT-EDGE$ SFT-HEX-B SHA256-FILE-HEX-IN 0 T=
   SFT-HEX-A 64 SFT-HEX-B 64 T$= ;

: SFT-MAIN ( -- )
   T-RESET
   SFT-PREPARE
   SFT-TEST-FIPS-HEX
   SFT-TEST-INCREMENTAL
   SFT-TEST-INTERLEAVED
   SFT-TEST-CONTEXT-COPY
   SFT-TEST-TASKS
   SFT-TEST-TASKS-ONE-SHOT
   SFT-TEST-ALIGNED-CREATE
   SFT-TEST-HEX
   SFT-TEST-FILE
   SFT-TEST-MISSING
   SFT-TEST-FILE-CTX
   SFT-TEST-FILE-TASKS
   SFT-TEST-DEEP-PATH
   SFT-TEST-EDGE-PATH
   FS:CLEANUP-RUN
   T-REPORT
   s" sha256-file-test: ok" type cr ;

SFT-MAIN
