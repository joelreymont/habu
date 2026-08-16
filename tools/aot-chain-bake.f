\ aot-chain-bake.f - bake a captured chain artifact into an engine.
\
\ The metabuild that ships a chain differs from the one that ships today's bin/hb
\ by exactly one call: src/habu/stdin.f STDIN-DRIVER:ARTIFACT! names the artifact
\ to merge and the engine that produced it. This builds the driver that makes that
\ call and runs the production stdin build with it, so the engine it writes is
\ seeded from the REPL capture PLUS the merged chain.
\
\ Run:  bin/hb --load tools/aot-chain-bake.f -- <artifact> <producer-engine>
\ Leaves <HB_TMP>/hb-chain and prints `aot-chain-bake: hb-chain ready`.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/vector.f
require lib/fmt.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/date.f
require tools/build-fixpoint.f

package CHAIN-BAKE
using BUILD-FIXPOINT

: STDIN-SRC-PATH ( -- ptr u8 n ) s" src/habu/stdin.f" ;

$4000 constant SRC-CAP
$5000 constant DRV-CAP

create SRC-BUF SRC-CAP allot   variable SRC-U
create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U

: DRV-PATH$ ( -- ptr u8 n ) DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )
   BF-TMP$ s" chain-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: READ-STDIN-SRC ( -- )
   STDIN-SRC-PATH SRC-BUF SRC-CAP READ-ALL SRC-U !
   SRC-U @ 0 <= if s" aot-chain-bake: cannot read src/habu/stdin.f" BF-BUILD-RC die then ;

: WS? ( n -- bool ) {: c:n :}
   c 32 = c 9 = or c 13 = or c 10 = or ;

: SRC-LAST ( -- n )
   SRC-U @ 1-
   begin dup 0 >= while
      dup SRC-BUF + c@ WS? 0= if exit then
      1-
   repeat ;

: TAIL-BAD ( -- )
   s" aot-chain-bake: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

: RUN-TAIL$ ( -- ptr u8 n ) s" STDIN-DRIVER:RUN" ;

\ Everything up to the trailing `STDIN-DRIVER:RUN`, which this file replaces with
\ the artifact declaration plus the same call. Fail closed if the tail moved.
: RUN-KEEP ( -- n )
   SRC-LAST {: l:n :}
   RUN-TAIL$ {: t:ptr tu:n :}
   l 1+ tu < if TAIL-BAD then
   l 1+ tu - SRC-BUF + tu t tu STR= 0= if TAIL-BAD then
   l 1+ tu > if l tu - SRC-BUF + c@ WS? 0= if TAIL-BAD then then
   l 1+ tu - ;

: DRV-RESET ( -- ) 0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-chain-bake: driver buffer overflow" BF-BUILD-RC die then
   a DRV-BUF DRV-U @ + u BYTE-COPY
   DRV-U @ u + DRV-U ! ;

: DRV-NL ( -- )
   10 DRV-BUF DRV-U @ + c!
   DRV-U @ 1+ DRV-U ! ;

: DRV-LINE ( ptr u8 n -- ) DRV+ DRV-NL ;

create ART-BUF FS-PATH-CAP allot   variable ART-U
create ENG-BUF FS-PATH-CAP allot   variable ENG-U

: ?ARGS ( -- )
   SCRIPT-ARGC 2 >= if exit then
   s" aot-chain-bake: usage: --load tools/aot-chain-bake.f -- <artifact> <producer-engine>"
   BF-BUILD-RC die ;

: PATH-COPY ( ptr u8 n ptr u8 -- ) {: a:ptr u:n d:ptr :}
   u FS-PATH-CAP > if s" aot-chain-bake: path exceeds the buffer" BF-BUILD-RC die then
   a d u BYTE-COPY ;

: ARGS! ( -- )
   ?ARGS
   0 SCRIPT-ARGV$ dup ART-U !  ART-BUF PATH-COPY
   1 SCRIPT-ARGV$ dup ENG-U !  ENG-BUF PATH-COPY ;

\ A path with a `"` in it would end the generated string literal early, and the
\ driver would then compile some other program. Refuse rather than escape.
: ?PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ 34 = if
         s" aot-chain-bake: a path with a quote cannot be spliced into the driver"
         BF-BUILD-RC die
      then
   loop ;

: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;
: ENG$ ( -- ptr u8 n ) ENG-BUF ENG-U @ ;

\ THE TWO PATHS GO IN A COLON BODY, never on a top-level line. An interpret-mode
\ `s"` ALLOTS its bytes at HERE, and src/habu/stdin.f CAPTURE-REPL latches HERE as
\ the capture window's DATA base - so a top-level literal made the emitted engine
\ depend on the LENGTH of the paths spliced into it, and two bakes of one tree from
\ artifact paths of different lengths wrote engines differing in 12081 bytes.
\ Compiled into a body the same literal lands in code below the window, where the
\ seed's canonical CODE-B0 absorbs it, and neither path reaches the product's
\ bytes. stdin.f's DP-MARK refuses the top-level shape by name.
: DECL-NAME$ ( -- ptr u8 n ) s" ART-DECL" ;

: INJECT ( -- )
   ART$ ?PATH  ENG$ ?PATH
   S\" : " DRV+  DECL-NAME$ DRV+  S\"  ( -- ) s\" " DRV+  ART$ DRV+
   S\" \" s\" " DRV+  ENG$ DRV+
   S\" \" STDIN-DRIVER:ARTIFACT! ;" DRV-LINE
   DECL-NAME$ DRV-LINE
   RUN-TAIL$ DRV-LINE ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   READ-STDIN-SRC
   RUN-KEEP {: keep:n :}
   DRV-RESET
   SRC-BUF keep DRV+
   INJECT
   DRV-PATH$ DRV-BUF DRV-U @ WRITE-ALL ;

: EMIT-CHAIN-STDIN ( -- )
   s" stage2-src" DRV-PATH$ BF-EMIT-STDIN-RUN-SOURCE ;

\ Build the maker from the stage engine, run it, and keep the engine it writes.
: RUN-MAKER ( -- )
   s" stage2-got" s" hb-chain-mk" BF-RENAME-TMP
   s" hb-chain-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-chain-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-chain" BF-RENAME-TMP
   s" hb-chain" BF-CHMOD-X-TMP
   s" hb-chain" BF-CODESIGN-VERIFY-TMP ;

public

: BUILD ( -- )
   ARGS!
   BF-STAGE-FIXPOINT
   GEN-DRIVER
   EMIT-CHAIN-STDIN
   BF-CERTIFY-STDIN
   BF-RUN-STAGE
   RUN-MAKER
   s" aot-chain-bake: hb-chain ready" type cr ;

;package

CHAIN-BAKE:BUILD
