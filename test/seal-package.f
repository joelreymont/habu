\ seal-package.f - sealed system-package regressions (TFAM 2b-ii).
\
\ Proves that once the friend latch is sealed (every user-source entry), the
\ compiler rejects opening or reopening a reserved system package
\ (`package TFAM`/`TYPE`/`MATCH`) and rejects a qualified definition into one
\ (`: TFAM:tail ...`), case-insensitively, fail-closed with exit E-SEAL-PACKAGE.
\ Ordinary (non-reserved) packages and qualified definitions still compile, and a
\ trailing-colon ordinary name (`PRIM:`-shaped) is never treated as qualified.
\
\ Each program is a standalone forge run in a fresh child engine, so it cannot
\ include layout.f; the reserved names are written literally in the forge source
\ (they are the sealed spelling under test). The child engine is HABU_UNDER_TEST
\ when the gate sets it, else bin/hb, so the assertions run against the freshly
\ built sealed candidate. Both cold-prefix entry paths are covered: `--load` file
\ and piped stdin.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f test/seal-package.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

2048 constant SPK-CAP
10000 constant SPK-TIMEOUT-MS
84 constant SPK-SEAL-RC             \ E-SEAL-PACKAGE child exit status

variable SPK-ROOT-U
variable SPK-CHILD-U
variable SPK-IN-U
variable SPK-OUT-U
variable SPK-ERR-U
variable SPK-KIND
variable SPK-RC

create SPK-ROOT-BUF FS-PATH-CAP allot
create SPK-CHILD-BUF FS-PATH-CAP allot
create SPK-IN SPK-CAP allot          \ stdin-piped forge source
create SPK-OUT SPK-CAP allot
create SPK-ERR SPK-CAP allot
create SPK-EMPTY 1 allot             \ zero-length stdin

: SPK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SPK-ROOT ( -- ptr u8 n )   SPK-ROOT-BUF SPK-ROOT-U @ ;
: SPK-CHILD ( -- ptr u8 n )  SPK-CHILD-BUF SPK-CHILD-U @ ;
: SPK-IN$ ( -- ptr u8 n )    SPK-IN SPK-IN-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the sealed
\ candidate; standalone runs fall back to bin/hb.
: SPK-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: SPK-LF ( -- )   10 SB-APPEND-C ;

: SPK-LINE ( ptr u8 n -- )   SB-APPEND SPK-LF ;

\ --- forge programs: each names its own reserved spelling under test. ---

: SPK-PKG-FORGE$ ( ptr u8 n -- ptr u8 n )    \ `package <NAME>` + `end-package`
   SB-RESET
   s" package " SB-APPEND SB-APPEND SPK-LF
   s" end-package" SPK-LINE
   SB$ ;

: SPK-QUAL-FORGE$ ( ptr u8 n -- ptr u8 n )   \ `: <NAME>:W ( -- n ) 0 ;`
   SB-RESET
   s" : " SB-APPEND SB-APPEND s" :W ( -- n ) 0 ;" SB-APPEND SPK-LF
   SB$ ;

: SPK-OK-PKG-FORGE$ ( -- ptr u8 n )          \ non-reserved package with a public word
   SB-RESET
   s" package NOTSEALED" SPK-LINE
   s" public" SPK-LINE
   s" : W ( -- n ) 5 ;" SPK-LINE
   s" end-package" SPK-LINE
   s" NOTSEALED:W . cr" SPK-LINE
   SB$ ;

: SPK-OK-QUAL-FORGE$ ( -- ptr u8 n )         \ non-reserved qualified definition
   SB-RESET
   s" : MYPKG:W ( -- n ) 3 ;" SPK-LINE
   s" MYPKG:W . cr" SPK-LINE
   SB$ ;

: SPK-OK-EDGE-FORGE$ ( -- ptr u8 n )         \ trailing-colon ordinary name (PRIM:-shaped)
   SB-RESET
   s" : TFAM: ( -- n ) 7 ;" SPK-LINE
   s" TFAM: . cr" SPK-LINE
   SB$ ;

: SPK-OK-PREFIX-FORGE$ ( -- ptr u8 n )       \ reserved-prefix-but-longer package name
   SB-RESET
   s" package TFAMX" SPK-LINE
   s" end-package" SPK-LINE
   SB$ ;

\ --- child spawn + outcome capture ---

: SPK-STORE! ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
   kind SPK-KIND !  code SPK-RC !
   erru LEN>N SPK-ERR-U !  outu LEN>N SPK-OUT-U ! ;

: SPK-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SPK-CAP > if E-FS-CAPACITY throw then
   a SPK-IN u BYTE-COPY
   u SPK-IN-U ! ;

\ Run the forge as a --load file with empty stdin.
: SPK-RUN-LOAD ( ptr u8 n -- )
   SPK-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SPK-CHILD >LEN PROC-ARGV+
   SPK-HB$ >LEN  SPK-EMPTY 0 >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  SPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SPK-STORE! ;

\ Run the forge as a piped stdin program (no --load), the other cold-prefix path.
: SPK-RUN-STDIN ( ptr u8 n -- )
   SPK-IN!
   PROC-ARGV-RESET
   SPK-HB$ >LEN  SPK-IN$ >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  SPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SPK-STORE! ;

: SPK-ASSERT-SEAL ( -- )                     \ child died with the sealed-package exit
   SPK-KIND @ PROC-OUTCOME-EXIT T=
   SPK-RC @ SPK-SEAL-RC T= ;

: SPK-ASSERT-OK ( -- )                       \ child exited cleanly
   SPK-KIND @ PROC-OUTCOME-EXIT T=
   SPK-RC @ 0 T= ;

: SPK-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-seal-pkg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u SPK-ROOT-BUF SPK-ROOT-U SPK-COPY!
   SPK-ROOT CLEANUP-TREE+
   SPK-ROOT s" forge.f" SPK-CHILD-BUF JOIN-PATH SPK-CHILD-U ! ;

: SPK-CLEANUP ( -- )
   CLEANUP-RUN
   SPK-ROOT EXISTS? TFALSE ;

\ --- one reserved spelling: `package NAME` traps on both entry paths. ---
: SPK-PKG-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPK-PKG-FORGE$ SPK-RUN-LOAD SPK-ASSERT-SEAL
   a u SPK-PKG-FORGE$ SPK-RUN-STDIN SPK-ASSERT-SEAL ;

: SPK-QUAL-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPK-QUAL-FORGE$ SPK-RUN-LOAD SPK-ASSERT-SEAL
   a u SPK-QUAL-FORGE$ SPK-RUN-STDIN SPK-ASSERT-SEAL ;

: SPK-NEGATIVES ( -- )
   s" package TFAM (canonical) traps" T-LABEL   s" TFAM"  SPK-PKG-NEG
   s" package tfam (lower alias) traps" T-LABEL  s" tfam"  SPK-PKG-NEG
   s" package Tfam (mixed alias) traps" T-LABEL  s" Tfam"  SPK-PKG-NEG
   s" package TYPE traps" T-LABEL                s" TYPE"  SPK-PKG-NEG
   s" package MATCH traps" T-LABEL               s" MATCH" SPK-PKG-NEG
   s" package mAtCh (mixed alias) traps" T-LABEL s" mAtCh" SPK-PKG-NEG ;

: SPK-NEGATIVES-QUAL ( -- )
   s" qualified def TFAM:tail traps" T-LABEL     s" TFAM"  SPK-QUAL-NEG
   s" qualified def type:tail traps" T-LABEL     s" type"  SPK-QUAL-NEG
   s" qualified def MATCH:tail traps" T-LABEL    s" MATCH" SPK-QUAL-NEG ;

: SPK-POSITIVES ( -- )
   s" non-reserved package still compiles" T-LABEL
   SPK-OK-PKG-FORGE$ SPK-RUN-LOAD SPK-ASSERT-OK
   s" non-reserved qualified def still compiles" T-LABEL
   SPK-OK-QUAL-FORGE$ SPK-RUN-LOAD SPK-ASSERT-OK
   s" trailing-colon ordinary name is not qualified" T-LABEL
   SPK-OK-EDGE-FORGE$ SPK-RUN-LOAD SPK-ASSERT-OK
   s" reserved-prefix-but-longer package name allowed" T-LABEL
   SPK-OK-PREFIX-FORGE$ SPK-RUN-LOAD SPK-ASSERT-OK ;

: SPK-MAIN ( -- )
   T-RESET
   SPK-PREPARE
   SPK-NEGATIVES
   SPK-NEGATIVES-QUAL
   SPK-POSITIVES
   SPK-CLEANUP
   T-REPORT
   s" seal-package-test: ok" type cr ;

SPK-MAIN
