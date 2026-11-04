\ seal-package.f - once the friend latch is sealed (every user-source entry),
\ opening, reopening or defining a qualified word into a reserved system package
\ (TFAM TYPE MATCH LOWER-CERT ENGINE-ERROR) exits ENGINE-ERROR:SEAL-PACKAGE,
\ case-insensitively, and so do ' and ['] on a reserved qualified name
\ (C-QUALIFY-SEAL-GUARD). The guard fires before the checker, so every case
\ runs checked; ['] on a non-reserved name stays inert.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/subject.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require test/tail-parity.f

package SEAL-TEST

2048 constant SPK-CAP
30000 constant SPK-TIMEOUT-MS
ENGINE-ERROR:SEAL-PACKAGE constant SPK-SEAL-RC

variable SPK-ROOT-U
variable SPK-CHILD-U
variable SPK-IN-U
variable SPK-OUT-U
variable SPK-ERR-U
variable SPK-EXITED
variable SPK-RC

create SPK-ROOT-BUF PATH-CAP allot
create SPK-CHILD-BUF PATH-CAP allot
create SPK-IN SPK-CAP allot          \ stdin-piped forge source
create SPK-OUT SPK-CAP allot
create SPK-ERR SPK-CAP allot
create SPK-EMPTY 1 allot             \ zero-length stdin
create SPK-WANT SPK-CAP allot        \ the stderr line a parity run must print
variable SPK-WANT-U

: SPK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SPK-ROOT ( -- ptr u8 n )   SPK-ROOT-BUF SPK-ROOT-U @ ;
: SPK-CHILD ( -- ptr u8 n )  SPK-CHILD-BUF SPK-CHILD-U @ ;
: SPK-IN$ ( -- ptr u8 n )    SPK-IN SPK-IN-U @ ;

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

: SPK-OK-PKG-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" package NOTSEALED" SPK-LINE
   s" public" SPK-LINE
   s" : W ( -- n ) 5 ;" SPK-LINE
   s" ;package" SPK-LINE
   s" NOTSEALED:W . cr" SPK-LINE
   SB$ ;

: SPK-OK-QUAL-FORGE$ ( -- ptr u8 n )
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
   s" SUMTYPE spkw" SPK-LINE
   s"   VARIANT ok n ;VARIANT" SPK-LINE
   s"   VARIANT err n ;VARIANT" SPK-LINE
   s" ;SUMTYPE" SPK-LINE
   S\" s\" ctor-ready\" type cr" SPK-LINE
   s" : SPKW:EVIL ( -- n ) 7 ;" SPK-LINE
   SB$ ;

: SPK-FFI-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" require src/lib/ffi-abi.f" SPK-LINE
   s" package FFI" SPK-LINE
   s" ;package" SPK-LINE
   SB$ ;

: SPK-FFI-REDIRECT$ ( -- ptr u8 n )
   SB-RESET
   s" require src/lib/ffi-abi.f" SPK-LINE
   s" : FFI:EVIL ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-TASK-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" require src/lib/task.f" SPK-LINE
   s" package TASK" SPK-LINE
   s" ;package" SPK-LINE
   SB$ ;

: SPK-TASK-REDIRECT$ ( -- ptr u8 n )
   SB-RESET
   s" require src/lib/task.f" SPK-LINE
   s" : TASK:EVIL ( -- n ) 0 ;" SPK-LINE
   SB$ ;

: SPK-TICK-FORGE$ ( ptr u8 n -- ptr u8 n )   \ `' <NAME>:X drop` at top level (tick is checker-legal)
   SB-RESET
   s" ' " SB-APPEND SB-APPEND s" :X drop" SB-APPEND SPK-LF
   SB$ ;

: SPK-BTICK-FORGE$ ( ptr u8 n -- ptr u8 n )  \ `: F ( -- ) ['] <NAME>:X drop ;` (seal fires before the checker reject)
   SB-RESET
   s" : F ( -- ) ['] " SB-APPEND SB-APPEND s" :X drop ;" SB-APPEND SPK-LF
   SB$ ;

: SPK-OK-TICK-BARE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" ' dup drop" SPK-LINE
   SB$ ;

: SPK-OK-TICK-QUAL-FORGE$ ( -- ptr u8 n )
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

: SPK-OK-BTICK-FORGE$ ( -- ptr u8 n )        \ ['] on a non-reserved name is inert
   SB-RESET
   s" : G ( -- ) ['] dup drop ;" SPK-LINE
   s" G" SPK-LINE
   SB$ ;

: SPK-STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N SPK-ERR-U !  outu LEN>N SPK-OUT-U !
   oc MATCH outcome
     exited OF SPK-RC ! 0 0= SPK-EXITED ! ENDOF
     signaled OF SPK-RC ! 0 0= 0= SPK-EXITED ! ENDOF
     timeout OF src u SPK-OUT SPK-OUT-U @ SPK-ERR SPK-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: SPK-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SPK-CAP > if E-FS-CAPACITY throw then
   a SPK-IN u BYTE-COPY
   u SPK-IN-U ! ;

\ The parity cases run each program three ways: a direct --load, piped to
\ stdin (the two cold-prefix entry paths), and in a SUBJECT fork that
\ evaluates it while this file loads. A refusal's stdout and rc are the same
\ all three ways and its stderr line names each way's own place: the --load
\ file and its line, nothing on stdin, and this file alone for the evaluated
\ text. A program that runs leaves the same stdout, stderr and rc each way.
: SPK-RUN-LOAD ( ptr u8 n -- ) {: src:ptr u:n :}
   SPK-CHILD src u FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SPK-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  SPK-EMPTY 0 >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  SPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u SPK-STORE! ;

\ The other cold-prefix path: the forge piped to stdin, no --load.
: SPK-RUN-STDIN ( ptr u8 n -- )
   SPK-IN!
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  SPK-IN$ >LEN  SPK-OUT SPK-CAP >LEN
   SPK-ERR SPK-CAP >LEN  SPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SPK-IN$ SPK-STORE! ;

: SPK-SUBJECT ( ptr u8 n -- ) {: src:ptr u:n :}
   src u SPK-OUT SPK-CAP >LEN SPK-ERR SPK-CAP >LEN
   SPK-TIMEOUT-MS >MS SUBJECT:RUN src u SPK-STORE! ;

: SPK-ASSERT-SEAL ( -- )
   SPK-EXITED @ TTRUE
   SPK-RC @ SPK-SEAL-RC T= ;

: SPK-ASSERT-OK ( -- )
   SPK-EXITED @ TTRUE
   SPK-RC @ 0 T= ;

: SPK-ASSERT-CTOR-MARK ( -- )
   SPK-OUT SPK-OUT-U @ s" ctor-ready" CONTAINS? TTRUE ;

: SPK-RESULT ( -- ptr u8 n ptr u8 n n )
   SPK-OUT SPK-OUT-U @ SPK-ERR SPK-ERR-U @ SPK-RC @ ;

\ This file's path as the engine prints it while the file loads.
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL SPK-LOADING
: SPK-LOADING$ ( -- ptr u8 n )
   SPK-LOADING @ data-base SRCLOC:PATHLEN-CELL + @ ;

: SPK-WANT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SPK-WANT-U @ u + SPK-CAP > if E-STR-CAPACITY throw then
   a SPK-WANT SPK-WANT-U @ + u BYTE-COPY
   SPK-WANT-U @ u + SPK-WANT-U ! ;

\ Starts the refusal line of `package TFAM`; SPK-ASSERT-WANT ends it.
: SPK-WANT-OPEN ( -- )
   0 SPK-WANT-U !  s" E-SEALED-PACKAGE: TFAM" SPK-WANT+ ;

: SPK-ASSERT-WANT ( -- )
   S\" \n" SPK-WANT+
   SPK-ERR SPK-ERR-U @ SPK-WANT SPK-WANT-U @ T$= ;

\ The result with an empty stderr, for parity of stdout and rc alone.
: SPK-OUT-RC ( -- ptr u8 n ptr u8 n n )
   SPK-OUT SPK-OUT-U @ SPK-EMPTY 0 SPK-RC @ ;

: NEG-PLACES ( ptr u8 n -- ) {: src:ptr u:n :}
   src u SPK-RUN-LOAD SPK-ASSERT-SEAL
   SPK-WANT-OPEN  s"  at " SPK-WANT+  SPK-CHILD SPK-WANT+  s" :1" SPK-WANT+
   SPK-ASSERT-WANT
   SPK-OUT-RC TAIL-PARITY:SNAPSHOT
   src u SPK-RUN-STDIN SPK-ASSERT-SEAL
   SPK-WANT-OPEN SPK-ASSERT-WANT
   SPK-OUT-RC TAIL-PARITY:SAME
   src u SPK-SUBJECT SPK-ASSERT-SEAL
   SPK-WANT-OPEN  s"  at " SPK-WANT+  SPK-LOADING$ SPK-WANT+
   SPK-ASSERT-WANT
   SPK-OUT-RC TAIL-PARITY:SAME ;

: POS-LOAD ( ptr u8 n -- )
   2dup SPK-RUN-LOAD SPK-ASSERT-OK
   SPK-RESULT TAIL-PARITY:SNAPSHOT
   SPK-SUBJECT SPK-ASSERT-OK
   SPK-RESULT TAIL-PARITY:SAME ;

: SPK-PARITY-TEST ( -- )
   s" --load, stdin and subject name each one's place for a sealed package" T-LABEL
   s" TFAM" SPK-PKG-FORGE$ NEG-PLACES
   s" direct --load and subject preserve raw successful results" T-LABEL
   SPK-OK-PKG-FORGE$ POS-LOAD ;

: SPK-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-seal-pkg" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u SPK-ROOT-BUF SPK-ROOT-U SPK-COPY!
   SPK-ROOT FS:CLEANUP-TREE+
   SPK-ROOT s" forge.f" SPK-CHILD-BUF FS:JOIN-PATH SPK-CHILD-U ! ;

: SPK-CLEANUP ( -- )
   FS:CLEANUP-RUN
   SPK-ROOT FS:EXISTS? TFALSE ;

: SPK-TYPE-REOPEN$ ( -- ptr u8 n )
   SB-RESET
   s" package TYPE-NAME public : HACK ( -- n ) 0 ; ;package" SPK-LINE
   SB$ ;

: SPK-TYPE-PRIVATE-WID$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" TYPE-NAME\" BOOT:NAMESPACE-WL XREF-FIND-WL XREF-PKG-PRIVATE set-current" SPK-LINE
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
   SPK-PREPARE
   SPK-TYPE-TEST
   SPK-CLEANUP
   T-REPORT
   s" seal-package-type-name-test: ok" type cr ;

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

: SPK-NEGATIVES-BTICK ( -- )
   s" ['] TFAM:tail traps" T-LABEL
   s" TFAM" SPK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL
   s" ['] match:tail alias traps" T-LABEL
   s" match" SPK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-SEAL ;

: SPK-POSITIVES-TICK ( -- )
   s" non-qualified tick still works" T-LABEL
   SPK-OK-TICK-BARE-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" non-reserved qualified tick still works" T-LABEL
   SPK-OK-TICK-QUAL-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" trailing-colon ordinary name is not qualified (tick)" T-LABEL
   SPK-OK-TICK-EDGE-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" reserved-prefix-but-longer qualifier ticks fine" T-LABEL
   SPK-OK-TICK-PREFIX-FORGE$ SPK-SUBJECT SPK-ASSERT-OK
   s" ['] on a non-reserved name is inert" T-LABEL
   SPK-OK-BTICK-FORGE$ SPK-SUBJECT SPK-ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   SPK-PREPARE
   SPK-PARITY-TEST
   SPK-NEGATIVES
   SPK-NEGATIVES-QUAL
   SPK-NEGATIVE-CTOR-WID
   SPK-NEGATIVE-FFI-WIDS
   SPK-NEGATIVES-TICK
   SPK-NEGATIVES-BTICK
   SPK-POSITIVES
   SPK-POSITIVES-TICK
   SPK-CLEANUP
   T-REPORT
   s" seal-package-test: ok" type cr
   SPK-TYPE-RUN ;

;package

SEAL-TEST:RUN
