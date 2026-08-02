\ seal-package.f - sealed system-package regressions (TFAM 2b-ii, 2b-iii).
\
\ Proves that once the friend latch is sealed (every user-source entry), the
\ compiler rejects opening or reopening a reserved system package
\ (`package TFAM`/`TYPE`/`MATCH`/`LOWER-CERT`/`ENGINE-ERROR`) and rejects a qualified definition into one
\ (`: TFAM:tail ...`), case-insensitively, fail-closed with exit ENGINE-ERROR:SEAL-PACKAGE.
\ Ordinary (non-reserved) packages and qualified definitions still compile, and a
\ trailing-colon ordinary name (`PRIM:`-shaped) is never treated as qualified.
\
\ TFAM 2b-iii extends the same reserved-name seal (C-QUALIFY-SEAL-GUARD) to the
\ token sinks that resolve a qualified name without defining it: `'` (tick),
\ `[']` (bracket-tick), and `postpone`. A user `' TFAM:tail`, `['] TYPE:tail`, or
\ `postpone MATCH:tail` rejects fail-closed (ENGINE-ERROR:SEAL-PACKAGE) exactly like the
\ define path, case-insensitively, while
\ non-reserved, non-qualified, trailing-colon, and reserved-prefix-but-longer
\ names resolve normally. `[']`/`postpone` are checker-rejected in checked code
\ independently of the seal, so their inert positives run under `0 set-check`.
\
\ Semantic cases run in disposable SUBJECT forks of the sealed candidate. Exact
\ outcome/stdout/stderr parity against direct `--load` and stdin executions is
\ pinned below; those direct representatives retain both cold-prefix entry paths.
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
require lib/test/subject.f
require test/tail-ratchet.f

package SEAL-TEST

2048 constant SPK-CAP
ENGINE-ERROR:SEAL-PACKAGE constant SPK-SEAL-RC

variable SPK-ROOT-U
variable SPK-CHILD-U
variable SPK-IN-U
variable SPK-OUT-U
variable SPK-ERR-U
variable SPK-EXITED                 \ bool: child completed by exit
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

: SPK-PKG-FORGE$ ( ptr u8 n -- ptr u8 n )    \ `package <NAME>` + `;package`
   SB-RESET
   s" package " SB-APPEND SB-APPEND SPK-LF
   s" ;package" SPK-LINE
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
   s" ;package" SPK-LINE
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
   s" ;package" SPK-LINE
   SB$ ;

: SPK-CTOR-WID-FORGE$ ( -- ptr u8 n )       \ generated ctor WID closes to later tails
   SB-RESET
   s" SUMTYPE spkw 0" SPK-LINE
   s"   VARIANT ok n ;VARIANT" SPK-LINE
   s"   VARIANT err n ;VARIANT" SPK-LINE
   s" ;SUMTYPE" SPK-LINE
   S\" s\" ctor-ready\" type cr" SPK-LINE
   s" 0 set-check" SPK-LINE
   s" : SPKW:EVIL ( -- n ) 7 ;" SPK-LINE
   SB$ ;

: SPK-FFI-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/ffi-abi.f" SPK-LINE
   s" package FFI" SPK-LINE
   s" ;package" SPK-LINE
   SB$ ;

: SPK-FFI-REDIRECT$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/ffi-abi.f" SPK-LINE
   s" : FFI:EVIL ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-CUDA-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/ptx/cuda-driver.f" SPK-LINE
   s" package CUDA" SPK-LINE
   s" ;package" SPK-LINE
   SB$ ;

: SPK-CUDA-REDIRECT$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/ptx/cuda-driver.f" SPK-LINE
   s" : CUDA:EVIL ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-TASK-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SPK-LINE
   s" package TASK" SPK-LINE
   s" ;package" SPK-LINE
   SB$ ;

: SPK-TASK-REDIRECT$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SPK-LINE
   s" : TASK:EVIL ( -- n ) 0 ;" SPK-LINE
   SB$ ;

\ --- tick / bracket-tick / postpone reserved-name forges (TFAM 2b-iii) ---

: SPK-TICK-FORGE$ ( ptr u8 n -- ptr u8 n )   \ `' <NAME>:X drop` at top level (tick is checker-legal)
   SB-RESET
   s" ' " SB-APPEND SB-APPEND s" :X drop" SB-APPEND SPK-LF
   SB$ ;

: SPK-BTICK-FORGE$ ( ptr u8 n -- ptr u8 n )  \ `: F ( -- ) ['] <NAME>:X drop ;` (seal fires before the checker reject)
   SB-RESET
   s" : F ( -- ) ['] " SB-APPEND SB-APPEND s" :X drop ;" SB-APPEND SPK-LF
   SB$ ;

: SPK-POST-FORGE$ ( ptr u8 n -- ptr u8 n )   \ `: F ( -- ) postpone <NAME>:X ;` (seal fires before the checker reject)
   SB-RESET
   s" : F ( -- ) postpone " SB-APPEND SB-APPEND s" :X ;" SB-APPEND SPK-LF
   SB$ ;

: SPK-OK-TICK-BARE-FORGE$ ( -- ptr u8 n )    \ non-qualified tick still works
   SB-RESET
   s" ' dup drop" SPK-LINE
   SB$ ;

: SPK-OK-TICK-QUAL-FORGE$ ( -- ptr u8 n )    \ non-reserved qualified tick still works
   SB-RESET
   s" : MYPKG:W ( -- n ) 5 ;" SPK-LINE
   s" ' MYPKG:W drop" SPK-LINE
   SB$ ;

: SPK-OK-TICK-EDGE-FORGE$ ( -- ptr u8 n )    \ trailing-colon ordinary name is not qualified (tick)
   SB-RESET
   s" : TFAM: ( -- n ) 7 ;" SPK-LINE
   s" ' TFAM: drop" SPK-LINE
   SB$ ;

: SPK-OK-TICK-PREFIX-FORGE$ ( -- ptr u8 n )  \ reserved-prefix-but-longer qualifier ticks fine
   SB-RESET
   s" : TFAMX:W ( -- n ) 9 ;" SPK-LINE
   s" ' TFAMX:W drop" SPK-LINE
   SB$ ;

: SPK-OK-BTICK-FORGE$ ( -- ptr u8 n )        \ unchecked ['] on a non-reserved name is inert
   SB-RESET
   s" 0 set-check" SPK-LINE
   s" : G ( -- ) ['] dup drop ;" SPK-LINE
   s" G" SPK-LINE
   SB$ ;

: SPK-OK-POST-FORGE$ ( -- ptr u8 n )         \ unchecked postpone of a non-reserved name is inert
   \ Stack-neutral body: `postpone MYW` makes F a compiling word (COMPILE, MYW), so
   \ running F leaves the data stack UNCHANGED. The old `( -- n )`/`F drop` shape
   \ relied on the engine silently reading below an empty stack; the merged engine's
   \ runtime arity guard (E-UNDERFLOW) correctly rejects that, so keep F balanced —
   \ the point here is only that a non-reserved postpone compiles + runs unsealed.
   SB-RESET
   s" 0 set-check" SPK-LINE
   s" : MYW ( -- ) ;" SPK-LINE
   s" : F ( -- ) postpone MYW ;" SPK-LINE
   s" F" SPK-LINE
   SB$ ;

\ --- child spawn + outcome capture ---

: SPK-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF SPK-RC ! 0 0= SPK-EXITED ! ENDOF
     signaled OF SPK-RC ! 0 0= 0= SPK-EXITED ! ENDOF
     timeout OF 0 SPK-RC ! 0 0= 0= SPK-EXITED ! ENDOF
   ;MATCH
   LEN>N SPK-ERR-U !  LEN>N SPK-OUT-U ! ;

: SPK-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SPK-CAP > if E-FS-CAPACITY throw then
   a SPK-IN u BYTE-COPY
   u SPK-IN-U ! ;

\ Run the forge as a --load file with empty stdin.
: SPK-RUN-LOAD ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   SPK-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SPK-CHILD >LEN PROC-ARGV+
   SPK-HB$ >LEN  SPK-EMPTY 0 >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SPK-STORE! ;

\ Run the forge as a piped stdin program (no --load), the other cold-prefix path.
: SPK-RUN-STDIN ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   SPK-IN!
   PROC-ARGV-RESET
   SPK-HB$ >LEN  SPK-IN$ >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SPK-STORE! ;

: SPK-SUBJECT ( ptr u8 n -- )
   TAIL-RATCHET:SUBJECT
   SPK-OUT SPK-CAP >LEN SPK-ERR SPK-CAP >LEN
   TAIL-BUDGET:TIMEOUT-MS >MS SUBJECT:RUN SPK-STORE! ;

: SPK-ASSERT-SEAL ( -- )                     \ child died with the sealed-package exit
   SPK-EXITED @ TTRUE
   SPK-RC @ SPK-SEAL-RC T= ;

: SPK-ASSERT-OK ( -- )                       \ child exited cleanly
   SPK-EXITED @ TTRUE
   SPK-RC @ 0 T= ;

: SPK-ASSERT-CTOR-MARK ( -- )
   SPK-OUT SPK-OUT-U @ s" ctor-ready" CONTAINS? TTRUE ;

3 constant SPK-DIRECT-N
50 constant SPK-SUBJECT-N
: SPK-RESULT ( -- ptr u8 n ptr u8 n n )
   SPK-OUT SPK-OUT-U @ SPK-ERR SPK-ERR-U @ SPK-RC @ ;

: NEG-LOAD ( ptr u8 n -- )
   2dup SPK-RUN-LOAD SPK-ASSERT-SEAL
   SPK-RESULT TAIL-RATCHET:SNAPSHOT
   SPK-SUBJECT SPK-ASSERT-SEAL
   SPK-RESULT TAIL-RATCHET:SAME ;

: NEG-STDIN ( ptr u8 n -- )
   2dup SPK-RUN-STDIN SPK-ASSERT-SEAL
   SPK-RESULT TAIL-RATCHET:SNAPSHOT
   SPK-SUBJECT SPK-ASSERT-SEAL
   SPK-RESULT TAIL-RATCHET:SAME ;

: POS-LOAD ( ptr u8 n -- )
   2dup SPK-RUN-LOAD SPK-ASSERT-OK
   SPK-RESULT TAIL-RATCHET:SNAPSHOT
   SPK-SUBJECT SPK-ASSERT-OK
   SPK-RESULT TAIL-RATCHET:SAME ;

: SPK-PARITY-TEST ( -- )
   s" direct --load and subject preserve raw sealed-package results" T-LABEL
   s" TFAM" SPK-PKG-FORGE$ NEG-LOAD
   s" direct stdin and subject preserve raw sealed-package results" T-LABEL
   s" TFAM" SPK-PKG-FORGE$ NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   SPK-OK-PKG-FORGE$ POS-LOAD ;

: SPK-PARITY-CHECK ( -- )
   SPK-DIRECT-N SPK-SUBJECT-N TAIL-RATCHET:CHECK ;

: SPK-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-seal-pkg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u SPK-ROOT-BUF SPK-ROOT-U SPK-COPY!
   SPK-ROOT CLEANUP-TREE+
   SPK-ROOT s" forge.f" SPK-CHILD-BUF JOIN-PATH SPK-CHILD-U ! ;

: SPK-CLEANUP ( -- )
   CLEANUP-RUN
   SPK-ROOT EXISTS? TFALSE ;

: SPK-TYPE-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" package TYPE-NAME public : HACK ( -- n ) 0 ; ;package" SPK-LINE
   SB$ ;

: SPK-TYPE-PRIVATE-WID$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" TYPE-NAME\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-LEN set-current" SPK-LINE
   s" : HACK-PRIVATE ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-TYPE-QUALIFIED$ ( -- ptr u8 n )
   SB-RESET
   s" : TYPE-NAME:HACK ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-TYPE-TEST ( -- )
   s" TYPE-NAME private wordlist rejects direct publication" T-LABEL
   SPK-TYPE-PRIVATE-WID$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" TYPE-NAME public wordlist rejects package reopen" T-LABEL
   SPK-TYPE-REOPEN$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" TYPE-NAME public wordlist rejects qualified publish" T-LABEL
   SPK-TYPE-QUALIFIED$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-TYPE-RUN ( -- )
   T-RESET
   TAIL-RATCHET:START
   SPK-PREPARE
   SPK-TYPE-TEST
   0 3 TAIL-RATCHET:CHECK
   SPK-CLEANUP
   T-REPORT
   s" seal-package-type-name-test: ok" type cr ;

\ --- one reserved spelling: `package NAME` traps after the sealed entry. ---
: SPK-PKG-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPK-PKG-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-QUAL-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPK-QUAL-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-NEGATIVES ( -- )
   s" package TFAM (canonical) traps" T-LABEL   s" TFAM"  SPK-PKG-NEG
   s" package tfam (lower alias) traps" T-LABEL  s" tfam"  SPK-PKG-NEG
   s" package Tfam (mixed alias) traps" T-LABEL  s" Tfam"  SPK-PKG-NEG
   s" package TYPE traps" T-LABEL                s" TYPE"  SPK-PKG-NEG
   s" package MATCH traps" T-LABEL               s" MATCH" SPK-PKG-NEG
   s" package mAtCh (mixed alias) traps" T-LABEL s" mAtCh" SPK-PKG-NEG
   s" package CHECKER-CERT traps" T-LABEL        s" CHECKER-CERT" SPK-PKG-NEG
   s" package LOWER-CERT traps" T-LABEL          s" LOWER-CERT" SPK-PKG-NEG
   s" package LOWER-CERT-HOOK traps" T-LABEL     s" LOWER-CERT-HOOK" SPK-PKG-NEG
   s" package ENGINE-ERROR traps" T-LABEL        s" ENGINE-ERROR" SPK-PKG-NEG ;

: SPK-NEGATIVES-QUAL ( -- )
   s" qualified def TFAM:tail traps" T-LABEL     s" TFAM"  SPK-QUAL-NEG
   s" qualified def type:tail traps" T-LABEL     s" type"  SPK-QUAL-NEG
   s" qualified def MATCH:tail traps" T-LABEL    s" MATCH" SPK-QUAL-NEG
   s" qualified def CHECKER-CERT:tail traps" T-LABEL
      s" CHECKER-CERT" SPK-QUAL-NEG
   s" qualified def LOWER-CERT:tail traps" T-LABEL
      s" LOWER-CERT" SPK-QUAL-NEG
   s" qualified def LOWER-CERT-HOOK:tail traps" T-LABEL
      s" LOWER-CERT-HOOK" SPK-QUAL-NEG
   s" qualified def ENGINE-ERROR:tail traps" T-LABEL
      s" ENGINE-ERROR" SPK-QUAL-NEG ;

: SPK-NEGATIVE-CTOR-WID ( -- )
   s" generated constructor WID rejects new tail" T-LABEL
   SPK-CTOR-WID-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL SPK-ASSERT-CTOR-MARK ;

: SPK-NEGATIVE-FFI-WIDS ( -- )
   s" sealed FFI rejects reopen" T-LABEL
   SPK-FFI-REOPEN$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" sealed FFI rejects redirect" T-LABEL
   SPK-FFI-REDIRECT$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" sealed CUDA rejects reopen" T-LABEL
   SPK-CUDA-REOPEN$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" sealed CUDA rejects redirect" T-LABEL
   SPK-CUDA-REDIRECT$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" sealed TASK rejects reopen" T-LABEL
   SPK-TASK-REOPEN$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" sealed TASK rejects redirect" T-LABEL
   SPK-TASK-REDIRECT$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-POSITIVES ( -- )
   s" non-reserved package still compiles" T-LABEL
   SPK-OK-PKG-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" non-reserved qualified def still compiles" T-LABEL
   SPK-OK-QUAL-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" trailing-colon ordinary name is not qualified" T-LABEL
   SPK-OK-EDGE-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" reserved-prefix-but-longer package name allowed" T-LABEL
   SPK-OK-PREFIX-FORGE$ SPK-SUBJECT SPK-ASSERT-OK ;

\ --- TFAM 2b-iii: tick / bracket-tick / postpone token sinks ---

: SPK-TICK-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPK-TICK-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-NEGATIVES-TICK ( -- )
   s" ' TFAM:tail (canonical) traps" T-LABEL      s" TFAM"  SPK-TICK-NEG
   s" ' tfam:tail (lower alias) traps" T-LABEL    s" tfam"  SPK-TICK-NEG
   s" ' Tfam:tail (mixed alias) traps" T-LABEL    s" Tfam"  SPK-TICK-NEG
   s" ' TYPE:tail traps" T-LABEL                  s" TYPE"  SPK-TICK-NEG
   s" ' MATCH:tail traps" T-LABEL                 s" MATCH" SPK-TICK-NEG
   s" ' CHECKER-CERT:private traps" T-LABEL       s" CHECKER-CERT" SPK-TICK-NEG
   s" ' LOWER-CERT:private traps" T-LABEL         s" LOWER-CERT" SPK-TICK-NEG
   s" ' LOWER-CERT-HOOK:private traps" T-LABEL    s" LOWER-CERT-HOOK" SPK-TICK-NEG
   s" ' ENGINE-ERROR:private traps" T-LABEL       s" ENGINE-ERROR" SPK-TICK-NEG ;

: SPK-NEGATIVES-BTICK-POST ( -- )
   s" ['] TFAM:tail traps" T-LABEL
   s" TFAM" SPK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" ['] match:tail alias traps" T-LABEL
   s" match" SPK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" postpone TFAM:tail traps" T-LABEL
   s" TFAM" SPK-POST-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" postpone type:tail alias traps" T-LABEL
   s" type" SPK-POST-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-POSITIVES-TICK ( -- )
   s" non-qualified tick still works" T-LABEL
   SPK-OK-TICK-BARE-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" non-reserved qualified tick still works" T-LABEL
   SPK-OK-TICK-QUAL-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" trailing-colon ordinary name is not qualified (tick)" T-LABEL
   SPK-OK-TICK-EDGE-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" reserved-prefix-but-longer qualifier ticks fine" T-LABEL
   SPK-OK-TICK-PREFIX-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" unchecked ['] on a non-reserved name is inert" T-LABEL
   SPK-OK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" unchecked postpone of a non-reserved name is inert" T-LABEL
   SPK-OK-POST-FORGE$ SPK-SUBJECT SPK-ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   TAIL-RATCHET:START
   SPK-PREPARE
   SPK-PARITY-TEST
   SPK-NEGATIVES
   SPK-NEGATIVES-QUAL
   SPK-NEGATIVE-CTOR-WID
   SPK-NEGATIVE-FFI-WIDS
   SPK-NEGATIVES-TICK
   SPK-NEGATIVES-BTICK-POST
   SPK-POSITIVES
   SPK-POSITIVES-TICK
   SPK-PARITY-CHECK
   SPK-CLEANUP
   T-REPORT
   s" seal-package-test: ok" type cr
   SPK-TYPE-RUN ;

;package

SEAL-TEST:RUN
