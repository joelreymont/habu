\ build-test.f - focused tests for checked build helpers.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/build.f src/lib/build-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/process.f
require src/lib/build.f

\ Reopen the module for BUILD-EXPECT and the package-local public builders.
package BUILD

create BUILD-TEST-PATH PATH-CAP allot

variable BT-ROOT-U
variable BT-SRC-U
variable BT-MISSING-U
variable BT-CMD-OK-U
variable BT-CMD-NOART-U
variable BT-CMD-FAIL-U
variable BT-BAD-SRC-U
variable BT-UNCHECKABLE-SRC-U
variable BT-TOP-DIE-SRC-U
variable BT-BIG-SRC-U
variable BT-BIG-BAD-SRC-U

create BT-ROOT-BUF PATH-CAP allot
create BT-SRC-BUF PATH-CAP allot
create BT-MISSING-BUF PATH-CAP allot
create BT-CMD-OK-BUF PATH-CAP allot
create BT-CMD-NOART-BUF PATH-CAP allot
create BT-CMD-FAIL-BUF PATH-CAP allot
create BT-BAD-SRC-BUF PATH-CAP allot
create BT-UNCHECKABLE-SRC-BUF PATH-CAP allot
create BT-TOP-DIE-SRC-BUF PATH-CAP allot
create BT-BIG-SRC-BUF PATH-CAP allot
create BT-BIG-BAD-SRC-BUF PATH-CAP allot

\ CHECK reads a source of any size: 70000 leading spaces put the definition past
\ the 64 KiB a fixed source buffer would hold.
70000 constant BT-BIG-PAD
BT-BIG-PAD PATH-CAP + constant BT-BIG-CAP
create BT-BIG-TEXT BT-BIG-CAP allot

: BT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: BT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: BT-ROOT ( -- ptr u8 n )
   BT-ROOT-BUF BT-ROOT-U @ ;

: BT-SRC ( -- ptr u8 n )
   BT-SRC-BUF BT-SRC-U @ ;

: BT-MISSING ( -- ptr u8 n )
   BT-MISSING-BUF BT-MISSING-U @ ;

: BT-CMD-OK ( -- ptr u8 n )
   BT-CMD-OK-BUF BT-CMD-OK-U @ ;

: BT-CMD-NOART ( -- ptr u8 n )
   BT-CMD-NOART-BUF BT-CMD-NOART-U @ ;

: BT-CMD-FAIL ( -- ptr u8 n )
   BT-CMD-FAIL-BUF BT-CMD-FAIL-U @ ;

: BT-BAD-SRC ( -- ptr u8 n )
   BT-BAD-SRC-BUF BT-BAD-SRC-U @ ;

: BT-UNCHECKABLE-SRC ( -- ptr u8 n )
   BT-UNCHECKABLE-SRC-BUF BT-UNCHECKABLE-SRC-U @ ;

: BT-TOP-DIE-SRC ( -- ptr u8 n )
   BT-TOP-DIE-SRC-BUF BT-TOP-DIE-SRC-U @ ;

: BT-BIG-SRC ( -- ptr u8 n )
   BT-BIG-SRC-BUF BT-BIG-SRC-U @ ;

: BT-BIG-BAD-SRC ( -- ptr u8 n )
   BT-BIG-BAD-SRC-BUF BT-BIG-BAD-SRC-U @ ;

: BT-ROOT! ( -- )
   s" habu-build" FS:MKTEMP-DIR {: a:ptr u :}
   a u BT-ROOT-BUF BT-ROOT-U BT-COPY! ;

: BT-PATHS! ( -- )
   BT-ROOT s" source.f" BT-SRC-BUF BT-SRC-U BT-PATH!
   BT-ROOT s" missing.f" BT-MISSING-BUF BT-MISSING-U BT-PATH!
   BT-ROOT s" cmd-ok.f" BT-CMD-OK-BUF BT-CMD-OK-U BT-PATH!
   BT-ROOT s" cmd-noart.f" BT-CMD-NOART-BUF BT-CMD-NOART-U BT-PATH!
   BT-ROOT s" cmd-fail.f" BT-CMD-FAIL-BUF BT-CMD-FAIL-U BT-PATH!
   BT-ROOT s" bad.f" BT-BAD-SRC-BUF BT-BAD-SRC-U BT-PATH!
   BT-ROOT s" uncheckable.f" BT-UNCHECKABLE-SRC-BUF BT-UNCHECKABLE-SRC-U BT-PATH!
   BT-ROOT s" top-die.f" BT-TOP-DIE-SRC-BUF BT-TOP-DIE-SRC-U BT-PATH!
   BT-ROOT s" big.f" BT-BIG-SRC-BUF BT-BIG-SRC-U BT-PATH!
   BT-ROOT s" big-bad.f" BT-BIG-BAD-SRC-BUF BT-BIG-BAD-SRC-U BT-PATH! ;

: BT-ART ( -- ptr u8 n )
   BT-ROOT s" artifact.bin" ARTIFACT ;

: BT-NOART ( -- ptr u8 n )
   BT-ROOT s" noart.bin" ARTIFACT ;

: BT-SHEBANG$ ( -- ptr u8 n )
   S\" #!/usr/bin/env bin/hb\n" ;

: BT-SCRIPT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   BT-SHEBANG$ SB-APPEND
   a u SB-APPEND
   SB$ ;

\ The artifact-writing command embeds its output path one `c,` per byte, up
\ to seven bytes of source per path byte, so its buffer is sized by the path
\ capacity and not by the shared string builder: the scratch root lives under
\ TMPDIR, and the gate pool's 98-byte TMPDIR overflowed SB-CAP (E-STR-CAPACITY)
\ where a 94-byte one fit. The 256 covers the script's fixed text.
PATH-CAP 7 * 256 + constant BT-SCRIPT-CAP
create BT-SCRIPT-BUF BT-SCRIPT-CAP allot
TYPED-VARIABLE BT-SCRIPT-LEN len

: BT-SCRIPT+ ( ptr u8 n -- )
   BT-SCRIPT-BUF BT-SCRIPT-CAP BT-SCRIPT-LEN BUF-APPEND ;

: BT-SCRIPT-C, ( n -- )
   SB-RESET
   FMT:SB-U
   s"  c, " SB-APPEND
   SB$ BT-SCRIPT+ ;

: BT-SCRIPT-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ BT-SCRIPT-C,
      1+
   repeat drop ;

: BT-SCRIPT-PATH ( ptr u8 n -- )
   s" create P " BT-SCRIPT+
   BT-SCRIPT-BYTES
   0 BT-SCRIPT-C, ;

: BT-OK-SCRIPT$ ( -- ptr u8 n )
   BT-SCRIPT-LEN BUF-RESET
   BT-SHEBANG$ BT-SCRIPT+
   BT-ART BT-SCRIPT-PATH
   s" create BT-ART-BUF 97 c, 114 c, 116 c, 105 c, 102 c, 97 c, 99 c, 116 c, " BT-SCRIPT+
   s" P 1537 420 open dup BT-ART-BUF 8 write drop close" BT-SCRIPT+
   BT-SCRIPT-BUF BT-SCRIPT-LEN BUF-LEN@ ;

: BT-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu FS:WRITE-ALL
   path pathu FS:CLEANUP+ ;

: BT-WRITE-CMD ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   src srcu BT-SCRIPT$ {: code:ptr codeu :}
   path pathu code codeu FS:WRITE-ALL
   path pathu FS:CHMOD-X
   path pathu FS:CLEANUP+ ;

: BT-WRITE-CMD-OK ( -- )
   BT-CMD-OK BT-OK-SCRIPT$ FS:WRITE-ALL
   BT-CMD-OK FS:CHMOD-X
   BT-CMD-OK FS:CLEANUP+ ;

: BT-WRITE-BIG ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu def:ptr defu :}
   BT-BIG-PAD 0 do 32 BT-BIG-TEXT i + c! loop
   def BT-BIG-TEXT BT-BIG-PAD + defu BYTE-COPY
   path pathu BT-BIG-TEXT BT-BIG-PAD defu + BT-WRITE-FILE ;

: BT-WRITE-FIXTURES ( -- )
   BT-BIG-SRC s" : BIG ( i64 -- i64 ) 1 + ;" BT-WRITE-BIG
   BT-BIG-BAD-SRC s" : BIG-BAD ( i64 -- i64 ) 0= ;" BT-WRITE-BIG
   BT-SRC s" : MAIN ( -- ) ; : INC ( i64 -- i64 ) 1 + ;" BT-WRITE-FILE
   BT-BAD-SRC s" : BAD ( i64 -- i64 ) 0= ;" BT-WRITE-FILE
   BT-UNCHECKABLE-SRC s" : UNCHECKABLE ( i64 -- i64 ) evaluate ;" BT-WRITE-FILE
   BT-TOP-DIE-SRC s" NULL-PTR 0 1 die : SAFE ( i64 -- i64 ) 1 + ;" BT-WRITE-FILE
   BT-WRITE-CMD-OK
   BT-CMD-NOART s" : NOART ( -- ) ; NOART" BT-WRITE-CMD
   BT-CMD-FAIL s" NULL-PTR 0 7 die" BT-WRITE-CMD
   BT-ART FS:CLEANUP+
   BT-NOART FS:CLEANUP+ ;

: BT-PREPARE ( -- )
   FS:CLEANUP-RESET
   BT-ROOT!
   BT-ROOT FS:CLEANUP-DIR+
   BT-PATHS!
   BT-WRITE-FIXTURES ;

: BT-CLEANUP ( -- )
   FS:CLEANUP-RUN
   BT-ROOT FS:EXISTS? TFALSE ;

: BT-CHECK-ARTIFACT-PATH ( -- )
   BT-ROOT s" artifact.bin" ARTIFACT {: got:ptr gotu:n :}
   BT-ROOT s" artifact.bin" BUILD-TEST-PATH FS:JOIN-PATH {: wantu :}
   got gotu BUILD-TEST-PATH wantu T$= ;

: BT-OK-STEP ( -- n )
   0 ;

: BT-BAD-STEP ( -- n )
   7 ;

: BT-MISSING-SOURCE ( -- )
   BT-MISSING CHECK ;

: BT-BAD-SOURCE ( -- )
   BT-BAD-SRC CHECK ;

: BT-BIG-BAD-SOURCE ( -- )
   BT-BIG-BAD-SRC CHECK ;

: BT-UNCHECKABLE-SOURCE ( -- )
   BT-UNCHECKABLE-SRC CHECK ;

: BT-MISSING-EXPECT ( -- )
   BT-NOART BUILD-EXPECT ;

: BT-EMPTY-ARTIFACT ( -- )
   BT-ROOT s" " ARTIFACT 2drop ;

\ An artifact path is refused by its own rule when root, separator and name do
\ not fit PATH-CAP. The name is compared with the room the root leaves, so a
\ length near the maximum cell cannot wrap the sum back into range.
create BT-LONG PATH-CAP allot

: BT-LONG! ( -- )
   PATH-CAP 0 ?do 122 BT-LONG i + c! loop ;

: BT-ARTIFACT-EXACT ( -- )
   BT-ROOT BT-LONG PATH-CAP 1- BT-ROOT-U @ - ARTIFACT nip PATH-CAP T= ;

: BT-ARTIFACT-PAST ( -- )
   BT-ROOT BT-LONG PATH-CAP BT-ROOT-U @ - ARTIFACT 2drop ;

: BT-ARTIFACT-HUGE-NAME ( -- )
   BT-ROOT BT-LONG MEM-MAX-N ARTIFACT 2drop ;

: BT-ARTIFACT-HUGE-ROOT ( -- )
   BT-LONG MEM-MAX-N s" artifact.bin" ARTIFACT 2drop ;

: BT-BAD-STEP-RUN ( -- )
   s" bad-step" [: BT-BAD-STEP ;] STEP ;

: BT-EMPTY-STEP ( -- )
   s" " [: BT-OK-STEP ;] STEP ;

: BT-MISSING-COMMAND ( -- )
   BT-MISSING BT-ART RUN drop ;

: BT-NO-ARTIFACT ( -- )
   BT-CMD-NOART BT-NOART RUN drop ;

: BT-FAIL-COMMAND ( -- )
   BT-CMD-FAIL BT-ART RUN drop ;

: BUILD-TEST-PATHS ( -- )
   BT-SRC CHECK
   BT-TOP-DIE-SRC CHECK
   BT-BIG-SRC CHECK
   [: BT-BIG-BAD-SOURCE ;] E-BUILD-SOURCE TTHROWSQ
   [: BT-MISSING-SOURCE ;] E-BUILD-SOURCE TTHROWSQ
   [: BT-BAD-SOURCE ;] E-BUILD-SOURCE TTHROWSQ
   [: BT-UNCHECKABLE-SOURCE ;] E-BUILD-SOURCE TTHROWSQ
   BT-CHECK-ARTIFACT-PATH
   [: BT-MISSING-EXPECT ;] E-BUILD-PATH TTHROWSQ
   [: BT-EMPTY-ARTIFACT ;] E-BUILD-PATH TTHROWSQ
   BT-LONG!
   BT-ARTIFACT-EXACT
   [: BT-ARTIFACT-PAST ;] E-BUILD-PATH TTHROWSQ
   [: BT-ARTIFACT-HUGE-NAME ;] E-BUILD-PATH TTHROWSQ
   [: BT-ARTIFACT-HUGE-ROOT ;] E-BUILD-PATH TTHROWSQ ;

: BUILD-TEST-STEPS ( -- )
   s" ok-step" [: BT-OK-STEP ;] STEP
   [: BT-BAD-STEP-RUN ;] E-BUILD-STATUS TTHROWSQ
   [: BT-EMPTY-STEP ;] E-BUILD-COMMAND TTHROWSQ ;

: BUILD-TEST-RUNS ( -- )
   BT-CMD-OK BT-ART RUN 0 T=
   BT-ART FS:FILE? TTRUE
   [: BT-MISSING-COMMAND ;] E-BUILD-COMMAND TTHROWSQ
   [: BT-NO-ARTIFACT ;] E-BUILD-PATH TTHROWSQ
   [: BT-FAIL-COMMAND ;] E-BUILD-STATUS TTHROWSQ ;

: BUILD-TEST-MAIN ( -- )
   T-RESET
   BT-PREPARE
   BUILD-TEST-PATHS
   BUILD-TEST-STEPS
   BUILD-TEST-RUNS
   BT-CLEANUP
   T-REPORT
   s" build-test: ok" type cr ;

BUILD-TEST-MAIN

;package
