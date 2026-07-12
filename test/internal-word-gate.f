\ internal-word-gate.f - engine-internal word execution gate regressions (dot
\ habu-hb-crash-bare-c5be6634).
\
\ A word defined by the engine prefix with no checker-known effect (no
\ certified/trusted signature and no primitive axiom) carries DNAME-INT after
\ the seal-time marking pass (src/core/internal-mark.f). Interpret-mode
\ execution AND tick of such a word must fail closed with
\ `hb: internal engine word: <token>` + rc 70 on both cold-prefix source paths
\ (--load file and stdin pipe). Previously a bare `U-TYPE` in a load file
\ consumed below-base garbage as type-term handles and corrupted the process
\ (wild loads/stores, SIGSEGV at pc=0), so the user-facing top-level name
\ universe now equals the checker's. Positives prove the public surface is
\ untouched: undefined words still report E-UNDEFINED, underflow still reports
\ E-UNDERFLOW, user unchecked words stay executable, top-level TRUST rows /
\ TRUSTED: / structures + type-family DSLs still work, and XREF introspection
\ of internal words survives.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/internal-word-gate.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

2048 constant IWG-CAP
10000 constant IWG-TIMEOUT-MS
70 constant IWG-REJECT-RC           \ interpret-level reject exit (RC-REJECT)

variable IWG-ROOT-U
variable IWG-CHILD-U
variable IWG-IN-U
variable IWG-OUT-U
variable IWG-ERR-U
variable IWG-EXITED                 \ bool: child completed by exit
variable IWG-RC

create IWG-ROOT-BUF FS-PATH-CAP allot
create IWG-CHILD-BUF FS-PATH-CAP allot
create IWG-IN IWG-CAP allot         \ stdin-piped program
create IWG-OUT IWG-CAP allot
create IWG-ERR IWG-CAP allot
create IWG-EMPTY 1 allot            \ zero-length stdin

: IWG-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: IWG-ROOT ( -- ptr u8 n )
   IWG-ROOT-BUF IWG-ROOT-U @ ;

: IWG-CHILD ( -- ptr u8 n )
   IWG-CHILD-BUF IWG-CHILD-U @ ;

: IWG-IN$ ( -- ptr u8 n )
   IWG-IN IWG-IN-U @ ;

: IWG-ERR$ ( -- ptr u8 n )
   IWG-ERR IWG-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the candidate;
\ standalone runs fall back to bin/hb.
: IWG-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: IWG-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF IWG-RC ! 0 0= IWG-EXITED ! ENDOF
     signaled OF IWG-RC ! 0 0= 0= IWG-EXITED ! ENDOF
     timeout OF 0 IWG-RC ! 0 0= 0= IWG-EXITED ! ENDOF
   ;MATCH
   LEN>N IWG-ERR-U !  LEN>N IWG-OUT-U ! ;

: IWG-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u IWG-CAP > if E-FS-CAPACITY throw then
   a IWG-IN u BYTE-COPY
   u IWG-IN-U ! ;

\ Run the program as a --load file with empty stdin.
: IWG-RUN-LOAD ( ptr u8 n -- )
   IWG-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   IWG-CHILD >LEN PROC-ARGV+
   IWG-HB$ >LEN  IWG-EMPTY 0 >LEN  IWG-OUT IWG-CAP >LEN
   IWG-ERR IWG-CAP >LEN  IWG-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   IWG-STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: IWG-RUN-STDIN ( ptr u8 n -- )
   IWG-IN!
   PROC-ARGV-RESET
   IWG-HB$ >LEN  IWG-IN$ >LEN  IWG-OUT IWG-CAP >LEN
   IWG-ERR IWG-CAP >LEN  IWG-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   IWG-STORE! ;

: IWG-LF ( -- )
   10 SB-APPEND-C ;

: IWG-TOKEN$ ( ptr u8 n -- ptr u8 n )    \ program = the bare token on one line
   SB-RESET
   SB-APPEND IWG-LF
   SB$ ;

: IWG-ASSERT-INTERNAL ( ptr u8 n -- ) {: a:ptr u:n :}   \ fail-closed reject naming the word
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ s" hb: internal engine word: " CONTAINS? TTRUE
   IWG-ERR$ a u CONTAINS? TTRUE ;

: IWG-ASSERT-OK ( -- )
   IWG-EXITED @ TTRUE
   IWG-RC @ 0 T= ;

: IWG-NEG-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}   \ bare token via --load must reject
   a u IWG-TOKEN$ IWG-RUN-LOAD
   a u IWG-ASSERT-INTERNAL ;

\ --- negatives: internal checker words fail closed before their body runs ---

: IWG-NEG-BARE ( -- )
   s" bare U-TYPE via --load fails closed (was SIGSEGV)" T-LABEL
   s" U-TYPE" IWG-NEG-LOAD
   s" bare U-TYPE via stdin pipe fails closed" T-LABEL
   s" U-TYPE" IWG-TOKEN$ IWG-RUN-STDIN
   s" U-TYPE" IWG-ASSERT-INTERNAL
   s" bare T-RES fails closed" T-LABEL
   s" T-RES" IWG-NEG-LOAD
   s" bare PAIR fails closed" T-LABEL
   s" PAIR" IWG-NEG-LOAD
   s" bare CHECKER-FIND-ACTIVE-SIG fails closed" T-LABEL
   s" CHECKER-FIND-ACTIVE-SIG" IWG-NEG-LOAD
   s" bare E-INST fails closed" T-LABEL
   s" E-INST" IWG-NEG-LOAD ;

: IWG-ARGS-FORGE$ ( -- ptr u8 n )        \ args present: the gate is not depth-keyed
   SB-RESET
   s" 1 2 U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-TICK-FORGE$ ( -- ptr u8 n )        \ tick would launder the xt to execute
   SB-RESET
   s" ' U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-PRIM-FORGE$ ( -- ptr u8 n )        \ the marking prim itself is sealed
   SB-RESET
   s" 0 int-mark" SB-APPEND IWG-LF
   SB$ ;

: IWG-NEG-SHAPES ( -- )
   s" 1 2 U-TYPE (satisfied depth) still fails closed" T-LABEL
   IWG-ARGS-FORGE$ IWG-RUN-LOAD
   s" U-TYPE" IWG-ASSERT-INTERNAL
   s" ' U-TYPE (tick laundering) fails closed" T-LABEL
   IWG-TICK-FORGE$ IWG-RUN-LOAD
   s" U-TYPE" IWG-ASSERT-INTERNAL
   s" 0 int-mark: the marking prim is itself internal" T-LABEL
   IWG-PRIM-FORGE$ IWG-RUN-LOAD
   s" int-mark" IWG-ASSERT-INTERNAL ;

\ --- positives: the public top-level surface is untouched -------------------

: IWG-ASSERT-DIAG ( ptr u8 n -- ) {: a:ptr u:n :}   \ child rejected rc 70 with the given diagnostic
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ a u CONTAINS? TTRUE ;

: IWG-UNDEF-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" IWG-NO-SUCH-WORD" SB-APPEND IWG-LF
   SB$ ;

: IWG-RAW-FORGE$ ( -- ptr u8 n )         \ user unchecked word stays executable
   SB-RESET
   s" 0 set-check" SB-APPEND IWG-LF
   s" : IWG-RAW 42 . cr ;" SB-APPEND IWG-LF
   s" IWG-RAW" SB-APPEND IWG-LF
   SB$ ;

: IWG-TRUST-FORGE$ ( -- ptr u8 n )       \ top-level TRUST row still works
   SB-RESET
   s" variable IWG-V" SB-APPEND IWG-LF
   S\" s\" IWG-V\" s\" -- ptr a\" TRUST" SB-APPEND IWG-LF
   SB$ ;

: IWG-TRUSTED-FORGE$ ( -- ptr u8 n )     \ TRUSTED: definition + bare call still work
   SB-RESET
   s" TRUSTED: IWG-T ( -- n ) 3 ;" SB-APPEND IWG-LF
   s" IWG-T . cr" SB-APPEND IWG-LF
   SB$ ;

: IWG-STRUCT-FORGE$ ( -- ptr u8 n )      \ structures DSL still works at top level
   SB-RESET
   s" BEGIN-STRUCTURE IWG-PT" SB-APPEND IWG-LF
   s"    CELL +FIELD IWG-PT.X" SB-APPEND IWG-LF
   s" END-STRUCTURE" SB-APPEND IWG-LF
   SB$ ;

: IWG-SUMTYPE-FORGE$ ( -- ptr u8 n )     \ type-family DSL still works at top level
   SB-RESET
   s" SUMTYPE iwgfoo 1 VARIANT iwgbar a ;VARIANT ;SUMTYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-XREF-FORGE$ ( -- ptr u8 n )        \ introspection of internal words survives
   SB-RESET
   s" XREF U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-LBUF-FORGE$ ( -- ptr u8 n )        \ LAYOUT-BUFFER DSL still works at top level
   SB-RESET
   s" ENUM iwgclr iwgred iwgblue ;ENUM" SB-APPEND IWG-LF
   s" 4 LAYOUT-BUFFER IWG-LB iwgclr" SB-APPEND IWG-LF
   s" 3 IWG-LB drop" SB-APPEND IWG-LF
   SB$ ;

: IWG-LBUF-BODY-FORGE$ ( -- ptr u8 n )   \ layout-buffer inside a checked body is unsafe
   SB-RESET
   s" : IWG-LBAD ( n -- ) LAYOUT-BUFFER ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-POSITIVES ( -- )
   s" undefined word still reports E-UNDEFINED" T-LABEL
   IWG-UNDEF-FORGE$ IWG-RUN-LOAD
   s" E-UNDEFINED" IWG-ASSERT-DIAG
   s" bare drop still reports E-UNDERFLOW" T-LABEL
   s" drop" IWG-TOKEN$ IWG-RUN-LOAD
   s" E-UNDERFLOW" IWG-ASSERT-DIAG
   s" user unchecked word stays executable at top level" T-LABEL
   IWG-RAW-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" top-level TRUST row still works" T-LABEL
   IWG-TRUST-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" TRUSTED: + bare call still work" T-LABEL
   IWG-TRUSTED-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" structures DSL still works" T-LABEL
   IWG-STRUCT-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" SUMTYPE DSL still works" T-LABEL
   IWG-SUMTYPE-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" LAYOUT-BUFFER DSL still works" T-LABEL
   IWG-LBUF-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" layout-buffer in a checked body is rejected unsafe" T-LABEL
   IWG-LBUF-BODY-FORGE$ IWG-RUN-LOAD
   s" at 'LAYOUT-BUFFER'" IWG-ASSERT-DIAG
   s" XREF of an internal word still works" T-LABEL
   IWG-XREF-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK ;

: IWG-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-iwg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u IWG-ROOT-BUF IWG-ROOT-U IWG-COPY!
   IWG-ROOT CLEANUP-TREE+
   IWG-ROOT s" forge.f" IWG-CHILD-BUF JOIN-PATH IWG-CHILD-U ! ;

: IWG-CLEANUP ( -- )
   CLEANUP-RUN
   IWG-ROOT EXISTS? TFALSE ;

: IWG-MAIN ( -- )
   T-RESET
   IWG-PREPARE
   IWG-NEG-BARE
   IWG-NEG-SHAPES
   IWG-POSITIVES
   IWG-CLEANUP
   T-REPORT
   s" internal-word-gate: ok" type cr ;

IWG-MAIN
