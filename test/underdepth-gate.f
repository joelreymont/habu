\ underdepth-gate.f - a bare top-level call on a shallower stack than its
\ word's effect states (a certified `:`, TRUSTED:, defer, does> clause,
\ engine word or primitive) is rejected by the checker before the word
\ runs: rc 70 and an E-INPUT-UNDERFLOW record at the word. The checker
\ measures the input of `execute`, `catch` and `finally` in a body as any
\ word's.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f
\   test/underdepth-gate.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/subject.f
require src/lib/engine-candidate.f
require test/tail-parity.f

2048 constant UDG-CAP

variable UDG-ROOT-U
variable UDG-CHILD-U
variable UDG-IN-U
variable UDG-OUT-U
variable UDG-ERR-U
variable UDG-EXITED                 \ bool: child completed by exit
variable UDG-RC

create UDG-ROOT-BUF PATH-CAP allot
create UDG-CHILD-BUF PATH-CAP allot
create UDG-IN UDG-CAP allot         \ stdin-piped program
create UDG-OUT UDG-CAP allot
create UDG-ERR UDG-CAP allot
create UDG-EMPTY 1 allot            \ zero-length stdin
create UDG-AT PATH-CAP allot
TYPED-VARIABLE UDG-AT-U len

: UDG-COPY! ( str ^u8 ^n -- ) {: src:str dst:^ lenp:^ :}
   src STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   src STR:DATA dst src STR:LEN BYTE-COPY
   src STR:LEN lenp ! ;

: UDG-ROOT ( -- str )
   UDG-ROOT-BUF UDG-ROOT-U @ >str ;

: UDG-CHILD ( -- str )
   UDG-CHILD-BUF UDG-CHILD-U @ >str ;

: UDG-IN$ ( -- str )
   UDG-IN UDG-IN-U @ >str ;

: UDG-OUT$ ( -- str )
   UDG-OUT UDG-OUT-U @ >str ;

: UDG-ERR$ ( -- str )
   UDG-ERR UDG-ERR-U @ >str ;

: UDG-STORE! ( len len PROCESS:ending str -- ) {: outu:len erru:len oc src:str :}
   erru LEN>N UDG-ERR-U !  outu LEN>N UDG-OUT-U !
   oc MATCH PROCESS:ending
     exited OF UDG-RC ! 0 0= UDG-EXITED ! ENDOF
     signaled OF UDG-RC ! 0 0= 0= UDG-EXITED ! ENDOF
     timeout OF src UDG-OUT$ UDG-ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: UDG-IN! ( str -- ) {: s:str :}
   s STR:LEN UDG-CAP > if E-FS-CAPACITY throw then
   s STR:DATA UDG-IN s STR:LEN BYTE-COPY
   s STR:LEN UDG-IN-U ! ;

\ Cases run in SUBJECT forks; output parity keeps direct --load and stdin
\ representatives.
: UDG-RUN-LOAD ( str -- ) {: src:str :}
   UDG-CHILD src FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   UDG-CHILD PROC-ARGV+
   ENGINE-CANDIDATE:PATH$  UDG-EMPTY 0 >str  UDG-OUT UDG-CAP >LEN
   UDG-ERR UDG-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src UDG-STORE! ;

\ The other cold-prefix path: the program on stdin, no --load.
: UDG-RUN-STDIN ( str -- )
   UDG-IN!
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$  UDG-IN$  UDG-OUT UDG-CAP >LEN
   UDG-ERR UDG-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   UDG-IN$ UDG-STORE! ;

package UDG-EXEC

public

: SUBJECT ( str -- ) {: src:str :}
   src UDG-OUT UDG-CAP >LEN UDG-ERR UDG-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN src UDG-STORE! ;

;package

: UDG-LF ( -- )
   10 SB-APPEND-C ;

: UDG-LINE ( str -- )
   SB-APPEND UDG-LF ;

: UDG-FOO2$ ( str -- str )     \ program = FOO2 definition + the given tail line
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   UDG-LINE
   SB$ ;

: UDG-AT+ ( str -- )
   UDG-AT PATH-CAP UDG-AT-U BUF-APPEND ;

\ The caret run under a word of u bytes, then its label l.
: UDG-CARETS$ ( n str -- str ) {: u:n l:str :}
   UDG-AT-U BUF-RESET
   s"  " UDG-AT+  u 0 ?do s" ^" UDG-AT+ loop
   s"  " UDG-AT+  l UDG-AT+
   UDG-AT UDG-AT-U @ LEN>N >str ;

\ The rejection opens with h, and carets labelled l stand under the word s;
\ in a subject run they stand in its `= evaluated:` note, whose primary
\ place is the test's own line.
: UDG-ASSERT-AT ( str str str -- ) {: h:str s:str l:str :}
   UDG-EXITED @ TTRUE
   UDG-RC @ 70 T=
   UDG-ERR$ h STR:CONTAINS? TTRUE
   UDG-ERR$ s STR:CONTAINS? TTRUE
   UDG-ERR$ s STR:LEN l UDG-CARETS$ STR:CONTAINS? TTRUE ;

: UDG-UNDERFLOW$ ( -- str )
   s" error[E-INPUT-UNDERFLOW]: input underflow" ;

\ The checker's rejection of the word s, in a direct or subject run: an input
\ underflow at it, below the stack or below the inputs.
: UDG-ASSERT-UNDERDEPTH ( str -- ) {: s:str :}
   UDG-UNDERFLOW$ s s" reaches below the" UDG-ASSERT-AT ;

: UDG-ASSERT-OK ( -- )
   UDG-EXITED @ TTRUE
   UDG-RC @ 0 T= ;

: UDG-ASSERT-OUT ( str -- ) {: s:str :}
   UDG-OUT$ s STR:CONTAINS? TTRUE ;

package UDG-MINIMUM

: MIN-SIG$ ( n -- str ) {: width:n :}
   SB-RESET
   s" defer MINIMUM-PROBE ( " SB-APPEND
   0 begin dup width < while
      s" n " SB-APPEND
      1 +
   repeat drop
   s" -- )" UDG-LINE
   SB$ ;

public

: BOUNDARY ( -- )
   s" 255-cell defer still refuses an empty interpret stack" T-LABEL
   255 MIN-SIG$ drop
   s" MINIMUM-PROBE" UDG-LINE
   SB$ UDG-EXEC:SUBJECT
   s" MINIMUM-PROBE" UDG-ASSERT-UNDERDEPTH ;

;package

\ --- negatives: certified words fail closed before their body reads garbage --

: UDG-NEG-BARE ( -- )
   s" bare FOO2 fails closed (was silent below-base read)" T-LABEL
   s" FOO2" UDG-FOO2$ UDG-EXEC:SUBJECT
   s" FOO2" UDG-ASSERT-UNDERDEPTH ;

: UDG-PARTIAL$ ( -- str )           \ depth 1 < declared 2: still underdepth
   SB-RESET
   s" : TP2 ( n n -- n ) + ;" UDG-LINE
   s" 1 TP2" UDG-LINE
   SB$ ;

: UDG-TRUSTED$ ( -- str )           \ TRUSTED: declared sig is guarded too
   SB-RESET
   s" TRUSTED: UDG-T ( n -- n n ) dup ;" UDG-LINE
   s" UDG-T" UDG-LINE
   SB$ ;

: UDG-DEFER$ ( -- str )             \ defer's required sig is guarded too
   SB-RESET
   s" defer UDG-D ( n -- n n )" UDG-LINE
   s" UDG-D" UDG-LINE
   SB$ ;

: UDG-SIGLESS$ ( -- str )           \ certified sig-less definition: inferred effect guards
   SB-RESET
   s" : UDG-SQ dup * ;" UDG-LINE
   s" UDG-SQ" UDG-LINE
   SB$ ;

: UDG-DOES$ ( -- str )              \ a does> clause's declared inputs are guarded too: the
   SB-RESET                              \ created record takes the same publish tail a `:` word takes
   s" : UDG-MQ: ( -- ) create 5 , does> ( n -- n ) nip @ ;" UDG-LINE
   s" UDG-MQ: UDG-MQA" UDG-LINE
   s" UDG-MQA" UDG-LINE
   SB$ ;

: UDG-ENGINE$ ( -- str )            \ an engine word
   SB-RESET
   s" STR:=" UDG-LINE
   SB$ ;

: UDG-NEG-SHAPES ( -- )
   s" 1 TP2 (partial depth) fails closed" T-LABEL
   UDG-PARTIAL$ UDG-EXEC:SUBJECT
   s" TP2" UDG-ASSERT-UNDERDEPTH
   s" TRUSTED: declared sig fails closed" T-LABEL
   UDG-TRUSTED$ UDG-EXEC:SUBJECT
   s" UDG-T" UDG-ASSERT-UNDERDEPTH
   s" defer declared sig fails closed" T-LABEL
   UDG-DEFER$ UDG-EXEC:SUBJECT
   s" UDG-D" UDG-ASSERT-UNDERDEPTH
   s" certified sig-less definition fails closed" T-LABEL
   UDG-SIGLESS$ UDG-EXEC:SUBJECT
   s" UDG-SQ" UDG-ASSERT-UNDERDEPTH
   s" does>-created word's clause inputs fail closed" T-LABEL
   UDG-DOES$ UDG-EXEC:SUBJECT
   s" UDG-MQA" UDG-ASSERT-UNDERDEPTH
   s" engine word (STR:=) fails closed" T-LABEL
   UDG-ENGINE$ UDG-EXEC:SUBJECT
   s" STR:=" UDG-ASSERT-UNDERDEPTH ;

\ --- primitive negatives: the checker measures a primitive by its row ------
\ A bare primitive checked code may call takes the same rejection on a shallow
\ stack, a row with no input atoms (2>r) included.

: UDG-BARE$ ( str -- str )     \ program = the bare token on one line
   SB-RESET
   UDG-LINE
   SB$ ;

: UDG-PRIM-ROW ( str -- ) {: s:str :}
   s UDG-BARE$ UDG-EXEC:SUBJECT
   s UDG-ASSERT-UNDERDEPTH ;

: UDG-NEG-PRIMS ( -- )
   s" bare catch fails closed (was SIGSEGV)" T-LABEL
   s" catch" UDG-PRIM-ROW
   s" bare evaluate fails closed pre-execution" T-LABEL
   s" evaluate" UDG-PRIM-ROW
   s" evaluate on one cell fails closed pre-execution" T-LABEL
   SB-RESET s" 1 evaluate" UDG-LINE SB$ UDG-EXEC:SUBJECT
   s" evaluate" UDG-ASSERT-UNDERDEPTH
   s" bare 2>r fails closed (was a guard-page read, rc 102)" T-LABEL
   s" 2>r" UDG-PRIM-ROW ;

\ --- body negatives: a primitive's input is measured as any word's ----------

: UDG-BODY-UNDER ( str str -- ) {: src:str at:str :}
   src UDG-BARE$ UDG-EXEC:SUBJECT
   at UDG-ASSERT-UNDERDEPTH ;

: UDG-NEG-BODY ( -- )
   s" execute on no cells in a body is an input underflow" T-LABEL
   s" : f ( -- ) execute ;" s" execute" UDG-BODY-UNDER
   s" catch on no cells in a body is an input underflow" T-LABEL
   s" : f ( -- ) catch ;" s" catch" UDG-BODY-UNDER
   s" finally on no cells in a body is an input underflow" T-LABEL
   s" : f ( -- ) finally ;" s" finally" UDG-BODY-UNDER
   s" finally over no quotation in a body names its cleanup" T-LABEL
   s" : f ( -- ) 1 2 finally ;" UDG-BARE$ UDG-EXEC:SUBJECT
   s" error[E-FINALLY-CLEANUP]: the cleanup `finally` runs must take and leave nothing"
   s" finally" s" cleanup not stack-neutral" UDG-ASSERT-AT ;

\ --- combinator negatives: cells reached through a row var -----------------
\ The cells a combinator's quotation takes beyond its fixed inputs arrive
\ through R and count as needed: RUNQ's call needs 2 cells over the 1 there.

: UDG-COMB$ ( str str -- str ) {: comb:str v:str :}
   SB-RESET
   s" : STEPN ( n -- n ) ;" UDG-LINE
   comb UDG-LINE
   v UDG-LINE
   SB$ ;

: UDG-COMB-UNDER ( str str str -- ) {: comb:str v:str at:str :}
   comb v UDG-COMB$ UDG-EXEC:SUBJECT
   at UDG-ASSERT-UNDERDEPTH
   UDG-ERR$ s" (needs 2, has 1)" STR:CONTAINS? TTRUE ;

: UDG-RUNQ$ ( -- str )
   s" : RUNQ ( R [ R -- R ] -- R ) execute ;" ;

: UDG-NEG-COMB ( -- )
   s" a ticked word through RUNQ on no cells is an input underflow" T-LABEL
   UDG-RUNQ$ s" : V ( -- ) ['] STEPN RUNQ ;" s" RUNQ" UDG-COMB-UNDER
   s" a quotation literal through RUNQ on no cells is an input underflow" T-LABEL
   UDG-RUNQ$ s" : V ( -- ) [: STEPN ;] RUNQ ;" s" RUNQ" UDG-COMB-UNDER
   s" a ticked RUNQ executed on no cells is an input underflow" T-LABEL
   UDG-RUNQ$ s" : V ( -- ) ['] STEPN ['] RUNQ execute ;" s" execute" UDG-COMB-UNDER
   s" a quotation calling RUNQ executed on no cells is an input underflow" T-LABEL
   UDG-RUNQ$ s" : V ( -- ) ['] STEPN [: RUNQ ;] execute ;" s" execute" UDG-COMB-UNDER
   s" a row var carrying a cell counts it as needed" T-LABEL
   s" : STEP3 ( n n -- n ) + ;  : APPLY2 ( R n [ R n -- S ] -- S ) execute ;"
   s" : V ( -- ) ['] STEP3 APPLY2 ;" UDG-COMB$ UDG-EXEC:SUBJECT
   s" APPLY2" UDG-ASSERT-UNDERDEPTH
   UDG-ERR$ s" (needs 3, has 1)" STR:CONTAINS? TTRUE ;

\ --- recursion negatives: RECURSE takes its inputs as a call does ----------

: UDG-NEG-RECURSE ( -- )
   s" RECURSE on no cells is an input underflow" T-LABEL
   s" : X ( n -- ) drop RECURSE ;" s" RECURSE" UDG-BODY-UNDER
   UDG-ERR$ s" (needs 1, has 0)" STR:CONTAINS? TTRUE ;

\ --- positives: the public top-level surface is untouched -------------------

: UDG-EXACT$ ( -- str )
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   s" 5 FOO2 + . cr" UDG-LINE
   SB$ ;

: UDG-OVER$ ( -- str )
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   s" 1 2 3 FOO2 + + + . cr" UDG-LINE
   SB$ ;

: UDG-COMPILED$ ( -- str )          \ checked calling checked: checker-proven, no guard
   SB-RESET
   s" : UDG-A ( n -- n ) 1 + ;" UDG-LINE
   s" : UDG-B ( n -- n ) UDG-A UDG-A ;" UDG-LINE
   s" 7 UDG-B . cr" UDG-LINE
   SB$ ;

: UDG-RAW$ ( -- str )               \ an ordinary inferred word remains executable
   SB-RESET
   s" : UDG-RAW 42 . cr ;" UDG-LINE
   s" UDG-RAW" UDG-LINE
   SB$ ;

: UDG-CATCH$ ( -- str )             \ evaluate delivers the reject as a catchable rc-70 throw
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   S\" TRUSTED: UDG-TRY ( -- ) s\" FOO2\" dup STR:DATA swap STR:LEN evaluate ;" UDG-LINE
   s" ' UDG-TRY catch . cr" UDG-LINE
   S\" s\" after\" type cr" UDG-LINE
   SB$ ;

: UDG-PROBE$ ( -- str )             \ satisfied-depth CHECK! probing idiom still works
   SB-RESET
   S\" s\" : UDG-G ( n -- n ) 1 + ;\" CHECK! . cr" UDG-LINE
   SB$ ;

: UDG-POSITIVES ( -- )
   s" exact declared depth still runs" T-LABEL
   UDG-EXACT$ UDG-EXEC:SUBJECT UDG-ASSERT-OK
   s" 10" UDG-ASSERT-OUT
   s" surplus depth still runs" T-LABEL
   UDG-OVER$ UDG-EXEC:SUBJECT UDG-ASSERT-OK
   s" 9" UDG-ASSERT-OUT
   s" compiled checked-to-checked calls carry no guard" T-LABEL
   UDG-COMPILED$ UDG-EXEC:SUBJECT UDG-ASSERT-OK
   s" 9" UDG-ASSERT-OUT
   s" inferred user word stays executable" T-LABEL
   UDG-RAW$ UDG-EXEC:SUBJECT UDG-ASSERT-OK
   s" 42" UDG-ASSERT-OUT
   s" evaluate delivers a catchable rc-70 reject and execution continues" T-LABEL
   UDG-CATCH$ UDG-EXEC:SUBJECT UDG-ASSERT-OK
   s" 70" UDG-ASSERT-OUT
   s" after" UDG-ASSERT-OUT
   s" satisfied-depth CHECK! probe still works" T-LABEL
   UDG-PROBE$ UDG-EXEC:SUBJECT UDG-ASSERT-OK ;

package UDG-PARITY

: RESULT ( -- str str n )
   UDG-OUT$ UDG-ERR$ UDG-RC @ ;

: NEG-LOAD ( str -- )
   dup UDG-RUN-LOAD
   s" FOO2" UDG-ASSERT-UNDERDEPTH
   RESULT TAIL-PARITY:SNAPSHOT
   UDG-EXEC:SUBJECT
   s" FOO2" UDG-ASSERT-UNDERDEPTH
   RESULT TAIL-PARITY:SAME-UNLOCATED ;

: NEG-STDIN ( str -- )
   dup UDG-RUN-STDIN
   s" FOO2" UDG-ASSERT-UNDERDEPTH
   RESULT TAIL-PARITY:SNAPSHOT
   UDG-EXEC:SUBJECT
   s" FOO2" UDG-ASSERT-UNDERDEPTH
   RESULT TAIL-PARITY:SAME-UNLOCATED ;

: POS-LOAD ( str -- )
   dup UDG-RUN-LOAD UDG-ASSERT-OK
   RESULT TAIL-PARITY:SNAPSHOT
   UDG-EXEC:SUBJECT UDG-ASSERT-OK
   RESULT TAIL-PARITY:SAME ;

public

: TEST ( -- )
   s" direct --load and subject agree on underdepth results but the location" T-LABEL
   s" FOO2" UDG-FOO2$ NEG-LOAD
   s" direct stdin and subject agree on underdepth results but the location" T-LABEL
   s" FOO2" UDG-FOO2$ NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   UDG-EXACT$ POS-LOAD ;

;package

: UDG-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-udg" FS:MKTEMP-DIR {: d:str :}
   d UDG-ROOT-BUF UDG-ROOT-U UDG-COPY!
   UDG-ROOT FS:CLEANUP-TREE+
   UDG-ROOT s" forge.f" UDG-CHILD-BUF FS:JOIN-PATH UDG-CHILD-U ! ;

: UDG-CLEANUP ( -- )
   FS:CLEANUP-RUN
   UDG-ROOT FS:EXISTS? TFALSE ;

: UDG-MAIN ( -- )
   T-RESET
   UDG-PREPARE
   UDG-PARITY:TEST
   UDG-NEG-BARE
   UDG-NEG-SHAPES
   UDG-NEG-PRIMS
   UDG-NEG-BODY
   UDG-NEG-COMB
   UDG-NEG-RECURSE
   UDG-POSITIVES
   UDG-MINIMUM:BOUNDARY
   UDG-CLEANUP
   T-REPORT
   s" underdepth-gate: ok" type cr ;

UDG-MAIN
