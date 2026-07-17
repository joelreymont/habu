\ underdepth-gate.f - certified-word interpret underdepth gate regressions (dot
\ habu-habu-certified-words-84e84eaf).
\
\ A word with a checker-known effect (certified `:`, TRUSTED:, defer, or an
\ axiom'd engine-prefix word) carries its declared input arity in the dict
\ record (DNAME-MIN-IN, flags bits 52-59), poked at certification time by the
\ publish tails and at seal time by src/core/internal-mark.f. Executing it at
\ bare top level with fewer interpret-stack cells must fail closed with
\ `hb: interpret stack underdepth: <token>` + rc 70 BEFORE the body runs, on
\ both cold-prefix source paths (--load file and stdin pipe). Previously
\ `: FOO2 ( n -- n n ) dup ; FOO2` on an empty stack ran rc 0 silently reading
\ below the stack base whenever net depth stayed >= 0 (the LMAIN depth floor
\ never tripped). Positives prove the public surface is untouched: exact and
\ surplus depth still run, compiled calls inside checked words carry no guard,
\ unchecked user words stay a documented boundary, evaluate delivers the
\ reject as a catchable rc-70 throw, and the satisfied-depth CHECK! probing
\ idiom still works. The REPL recovery smoke lives in test/proc-pty.f
\ (PTY-UNDERDEPTH).
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/underdepth-gate.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/src-shape.f

2048 constant UDG-CAP
10000 constant UDG-TIMEOUT-MS
70 constant UDG-REJECT-RC           \ interpret-level reject exit (RC-REJECT)
ENGINE-ERROR:SEAL-PACKAGE constant UDG-SEAL-RC

variable UDG-ROOT-U
variable UDG-CHILD-U
variable UDG-IN-U
variable UDG-OUT-U
variable UDG-ERR-U
variable UDG-EXITED                 \ bool: child completed by exit
variable UDG-RC

create UDG-ROOT-BUF FS-PATH-CAP allot
create UDG-CHILD-BUF FS-PATH-CAP allot
create UDG-IN UDG-CAP allot         \ stdin-piped program
create UDG-OUT UDG-CAP allot
create UDG-ERR UDG-CAP allot
create UDG-EMPTY 1 allot            \ zero-length stdin

: UDG-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: UDG-ROOT ( -- ptr u8 n )
   UDG-ROOT-BUF UDG-ROOT-U @ ;

: UDG-CHILD ( -- ptr u8 n )
   UDG-CHILD-BUF UDG-CHILD-U @ ;

: UDG-IN$ ( -- ptr u8 n )
   UDG-IN UDG-IN-U @ ;

: UDG-OUT$ ( -- ptr u8 n )
   UDG-OUT UDG-OUT-U @ ;

: UDG-ERR$ ( -- ptr u8 n )
   UDG-ERR UDG-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the candidate;
\ standalone runs fall back to bin/hb.
: UDG-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: UDG-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF UDG-RC ! 0 0= UDG-EXITED ! ENDOF
     signaled OF UDG-RC ! 0 0= 0= UDG-EXITED ! ENDOF
     timeout OF 0 UDG-RC ! 0 0= 0= UDG-EXITED ! ENDOF
   ;MATCH
   LEN>N UDG-ERR-U !  LEN>N UDG-OUT-U ! ;

: UDG-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u UDG-CAP > if E-FS-CAPACITY throw then
   a UDG-IN u BYTE-COPY
   u UDG-IN-U ! ;

\ Run the program as a --load file with empty stdin.
: UDG-RUN-LOAD ( ptr u8 n -- )
   UDG-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   UDG-CHILD >LEN PROC-ARGV+
   UDG-HB$ >LEN  UDG-EMPTY 0 >LEN  UDG-OUT UDG-CAP >LEN
   UDG-ERR UDG-CAP >LEN  UDG-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   UDG-STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: UDG-RUN-STDIN ( ptr u8 n -- )
   UDG-IN!
   PROC-ARGV-RESET
   UDG-HB$ >LEN  UDG-IN$ >LEN  UDG-OUT UDG-CAP >LEN
   UDG-ERR UDG-CAP >LEN  UDG-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   UDG-STORE! ;

: UDG-LF ( -- )
   10 SB-APPEND-C ;

: UDG-LINE ( ptr u8 n -- )
   SB-APPEND UDG-LF ;

: UDG-FOO2$ ( ptr u8 n -- ptr u8 n )     \ program = FOO2 definition + the given tail line
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   UDG-LINE
   SB$ ;

: UDG-ASSERT-UNDERDEPTH ( ptr u8 n -- ) {: a:ptr u:n :}   \ fail-closed reject naming the word
   UDG-EXITED @ TTRUE
   UDG-RC @ UDG-REJECT-RC T=
   UDG-ERR$ s" hb: interpret stack underdepth: " CONTAINS? TTRUE
   UDG-ERR$ a u CONTAINS? TTRUE ;

: UDG-ASSERT-OK ( -- )
   UDG-EXITED @ TTRUE
   UDG-RC @ 0 T= ;

: UDG-ASSERT-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   UDG-OUT$ a u CONTAINS? TTRUE ;

package UDG-MINIMUM

76 constant CAPACITY-RC             \ checker representation capacity reject

\ `die` exits through the kernel and cannot be caught for an in-process state
\ comparison.  Lock the nearest direct invariant instead: every effect-record
\ entry path must cross the physical-minimum guard before touching its arena,
\ cache, recurse state, or publish latches.
: SHAPE-FIND-AFTER ( ptr u8 n n -- n )
   {: needle:ptr needleu:n start:n :}
   SHAPE:TEXT {: hay:ptr hayu:n :}
   start 0 < if -1 exit then
   needleu 0= if start exit then
   hayu needleu < if -1 exit then
   start begin dup hayu needleu - <= while
      hay over + needleu needle needleu STR= if exit then
      1 +
   repeat drop -1 ;

variable SHAPE-DEF
variable SHAPE-GUARD
variable SHAPE-MUTATE

: SHAPE-ORDER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: def:ptr defu:n guard:ptr guardu:n mutate:ptr mutateu:n :}
   def defu 0 SHAPE-FIND-AFTER SHAPE-DEF !
   SHAPE-DEF @ 0 >= TTRUE
   guard guardu SHAPE-DEF @ SHAPE-FIND-AFTER SHAPE-GUARD !
   SHAPE-GUARD @ 0 >= TTRUE
   mutate mutateu SHAPE-DEF @ SHAPE-FIND-AFTER SHAPE-MUTATE !
   SHAPE-MUTATE @ 0 >= TTRUE
   SHAPE-GUARD @ SHAPE-MUTATE @ < TTRUE ;

public

: ATOMIC-SHAPE ( -- )
   s" minimum overflow guards every mutation boundary" T-LABEL
   s" src/core/checker.f" SHAPE:LOAD
   s" : E-BUILD-EFFECT (" s" din EFFECT-MIN-IN" s" E-REC-START E-OFF >r" SHAPE-ORDER
   s" : E-ADD-EFFECT (" s" din EFFECT-MIN-IN drop" s" RECW !" SHAPE-ORDER
   s" : PE-CLOSE-SYM (" s" PE-DIN @ EFFECT-MIN-IN drop" s" PE-SYM-ID !" SHAPE-ORDER
   s" : SIG-EFF-CACHE!" s" SGIN @ EFFECT-MIN-IN drop" s" RECEFF-UEND !" SHAPE-ORDER ;

private

\ Build a definition whose fixed input row contains exactly `width` physical
\ cells.  The encoded DNAME-MIN-IN byte accepts 255 and rejects 256 before the
\ effect arena/cache can be changed.
: MIN-SIG$ ( n -- ptr u8 n ) {: width:n :}
   SB-RESET
   s" TRUSTED: MINIMUM-PROBE ( " SB-APPEND
   0 begin dup width < while
      s" n " SB-APPEND
      1 +
   repeat drop
   s" -- ) ;" UDG-LINE
   SB$ ;

: ASSERT-CAPACITY ( -- )
   UDG-EXITED @ TTRUE
   UDG-RC @ CAPACITY-RC T=
   UDG-ERR$ s" checker: min-in exceeds record field" CONTAINS? TTRUE ;

public

: BOUNDARY ( -- )
   s" 255-cell physical minimum is representable" T-LABEL
   255 MIN-SIG$ UDG-RUN-LOAD
   UDG-ASSERT-OK
   s" 256-cell physical minimum rejects deterministically via --load" T-LABEL
   256 MIN-SIG$ UDG-RUN-LOAD
   ASSERT-CAPACITY
   s" 256-cell physical minimum rejects deterministically via stdin" T-LABEL
   256 MIN-SIG$ UDG-RUN-STDIN
   ASSERT-CAPACITY ;

;package

\ --- negatives: certified words fail closed before their body reads garbage --

: UDG-NEG-BARE ( -- )
   s" bare FOO2 via --load fails closed (was silent below-base read)" T-LABEL
   s" FOO2" UDG-FOO2$ UDG-RUN-LOAD
   s" FOO2" UDG-ASSERT-UNDERDEPTH
   s" bare FOO2 via stdin pipe fails closed" T-LABEL
   s" FOO2" UDG-FOO2$ UDG-RUN-STDIN
   s" FOO2" UDG-ASSERT-UNDERDEPTH ;

: UDG-PARTIAL$ ( -- ptr u8 n )           \ depth 1 < declared 2: still underdepth
   SB-RESET
   s" : TP2 ( n n -- n ) + ;" UDG-LINE
   s" 1 TP2" UDG-LINE
   SB$ ;

: UDG-TRUSTED$ ( -- ptr u8 n )           \ TRUSTED: declared sig is guarded too
   SB-RESET
   s" TRUSTED: UDG-T ( n -- n n ) dup ;" UDG-LINE
   s" UDG-T" UDG-LINE
   SB$ ;

: UDG-DEFER$ ( -- ptr u8 n )             \ defer's required sig is guarded too
   SB-RESET
   s" defer UDG-D ( n -- n n )" UDG-LINE
   s" UDG-D" UDG-LINE
   SB$ ;

: UDG-SIGLESS$ ( -- ptr u8 n )           \ certified sig-less definition: inferred effect guards
   SB-RESET
   s" : UDG-SQ dup * ;" UDG-LINE
   s" UDG-SQ" UDG-LINE
   SB$ ;

: UDG-ENGINE$ ( -- ptr u8 n )            \ axiom'd engine-prefix word (seal-time min-in poke)
   SB-RESET
   s" CORE-STR=" UDG-LINE
   SB$ ;

: UDG-NEG-SHAPES ( -- )
   s" 1 TP2 (partial depth) fails closed" T-LABEL
   UDG-PARTIAL$ UDG-RUN-LOAD
   s" TP2" UDG-ASSERT-UNDERDEPTH
   s" TRUSTED: declared sig fails closed" T-LABEL
   UDG-TRUSTED$ UDG-RUN-LOAD
   s" UDG-T" UDG-ASSERT-UNDERDEPTH
   s" defer declared sig fails closed" T-LABEL
   UDG-DEFER$ UDG-RUN-LOAD
   s" UDG-D" UDG-ASSERT-UNDERDEPTH
   s" certified sig-less definition fails closed" T-LABEL
   UDG-SIGLESS$ UDG-RUN-LOAD
   s" UDG-SQ" UDG-ASSERT-UNDERDEPTH
   s" axiom'd engine word (CORE-STR=) fails closed via seal-time poke" T-LABEL
   UDG-ENGINE$ UDG-RUN-LOAD
   s" CORE-STR=" UDG-ASSERT-UNDERDEPTH ;

\ --- p4 compile-path: compile-mode immediate underdepth (docs §5 sub-dot 5) --
\ An IMMEDIATE word executes at COMPILE time on the interpret stack, via the
\ EM-COMPILE-CALL BLR. The checked path is fail-closed by the p5 checker model
\ (test/immediate-model-test.f rejects the immediate as a body step); this is the
\ native floor beneath it, reached inside a `0 set-check` window where the
\ checker/hook is suspended. The compile-path BLR now carries the same
\ DNAME-MIN-IN depth gate the interpret path has (EM-INTERPRET-FIND), so an
\ immediate at compile-time underdepth diverts to the shared LMININ leg and fails
\ closed BEFORE the below-base read - rc 70, `hb: interpret stack underdepth:
\ <token>`, on both cold-prefix source paths (--load and stdin), matching the
\ FOO2 regression pattern. IMM2/IMM-DROP are defined checked so their
\ DNAME-MIN-IN byte is poked before the window opens.

: UDG-IMM-NEG$ ( -- ptr u8 n )           \ IMM2 wants 1 cell; empty interpret stack -> compile-time underdepth
   SB-RESET
   s" : IMM2 ( n -- n n ) dup ; immediate" UDG-LINE
   s" 0 set-check" UDG-LINE
   s" : USER IMM2 ;" UDG-LINE
   SB$ ;

: UDG-IMM-POS$ ( -- ptr u8 n )           \ IMM-DROP wants 1 cell; 42 supplied before `:` -> depth ok, compiles
   SB-RESET
   s" : IMM-DROP ( n -- ) drop ; immediate" UDG-LINE
   s" 0 set-check" UDG-LINE
   s" 42 : USER IMM-DROP ;" UDG-LINE
   s" 777 . cr" UDG-LINE
   SB$ ;

: UDG-COMPILE-IMM ( -- )
   s" compile-time immediate underdepth fails closed via --load" T-LABEL
   UDG-IMM-NEG$ UDG-RUN-LOAD
   s" IMM2" UDG-ASSERT-UNDERDEPTH
   s" compile-time immediate underdepth fails closed via stdin pipe" T-LABEL
   UDG-IMM-NEG$ UDG-RUN-STDIN
   s" IMM2" UDG-ASSERT-UNDERDEPTH
   s" compile-time immediate at satisfied depth still compiles" T-LABEL
   UDG-IMM-POS$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 777" UDG-ASSERT-OUT ;

\ --- census negatives: unguarded-prim gaps closed via the LARITY table ------
\ (dot census: bare catch/ffi-call/patch32 crashed with SIGSEGV exit 134;
\ evaluate/search-wl silently deref'd below-base garbage when it happened to
\ be mapped. All now divert to a clean pre-execution E-UNDERFLOW.)

: UDG-ASSERT-UNDERFLOW ( ptr u8 n -- ) {: a:ptr u:n :}   \ clean named reject, never a signal
   UDG-EXITED @ TTRUE
   UDG-RC @ UDG-REJECT-RC T=
   UDG-ERR$ s" E-UNDERFLOW: " CONTAINS? TTRUE
   UDG-ERR$ a u CONTAINS? TTRUE ;

: UDG-BARE$ ( ptr u8 n -- ptr u8 n )     \ program = the bare token on one line
   SB-RESET
   UDG-LINE
   SB$ ;

: UDG-PRIM-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u UDG-BARE$ UDG-RUN-LOAD
   a u UDG-ASSERT-UNDERFLOW ;

: UDG-NEG-PRIMS ( -- )
   s" bare catch fails closed (was SIGSEGV)" T-LABEL
   s" catch" UDG-PRIM-ROW
   s" bare ffi-call fails closed (was SIGSEGV)" T-LABEL
   s" ffi-call" UDG-PRIM-ROW
   s" bare patch32 fails closed (was SIGSEGV)" T-LABEL
   s" patch32" UDG-PRIM-ROW
   s" bare evaluate fails closed pre-execution" T-LABEL
   s" evaluate" UDG-PRIM-ROW
   s" bare set-check fails closed pre-execution" T-LABEL
   s" set-check" UDG-PRIM-ROW
   s" partial-depth ffi-call fails closed" T-LABEL
   SB-RESET s" 0 ffi-call" UDG-LINE SB$ UDG-RUN-LOAD
   s" ffi-call" UDG-ASSERT-UNDERFLOW ;

\ Package COLON records are classified through their owning package record,
\ so public wrappers receive the same pre-body minimum-input gate as globals.
: UDG-PF-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u UDG-BARE$ UDG-RUN-LOAD
   a u UDG-ASSERT-UNDERDEPTH ;

: UDG-PF-ZERO$ ( -- ptr u8 n )
   SB-RESET
   s" TYPE-FIELD:COUNT drop" UDG-LINE
   s" TYPE-FIELD:NO-VARIANT drop" UDG-LINE
   SB$ ;

: UDG-PF-REOPEN$ ( -- ptr u8 n )
   s" package TYPE-FIELD" UDG-BARE$ ;

: UDG-PF-PUBLISH$ ( -- ptr u8 n )
   s" : TYPE-FIELD:EVIL ( -- ) ;" UDG-BARE$ ;

: UDG-PF-ASSERT-SEALED ( -- )
   UDG-EXITED @ TTRUE
   UDG-RC @ UDG-SEAL-RC T= ;

: UDG-PF-PACKAGE ( -- )
   s" zero-input TYPE-FIELD wrappers remain callable" T-LABEL
   UDG-PF-ZERO$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" protected TYPE-FIELD package rejects reopen" T-LABEL
   UDG-PF-REOPEN$ UDG-RUN-LOAD UDG-PF-ASSERT-SEALED
   s" protected TYPE-FIELD package rejects qualified publish" T-LABEL
   UDG-PF-PUBLISH$ UDG-RUN-LOAD UDG-PF-ASSERT-SEALED
   s" required-input TYPE-FIELD wrappers reject before their bodies" T-LABEL
   s" TYPE-FIELD:FIND" UDG-PF-ROW
   s" TYPE-FIELD:EACH" UDG-PF-ROW
   s" TYPE-FIELD:FAMILY@" UDG-PF-ROW
   s" TYPE-FIELD:VARIANT@" UDG-PF-ROW
   s" TYPE-FIELD:NAME$" UDG-PF-ROW
   s" TYPE-FIELD:SCHEMA@" UDG-PF-ROW
   s" TYPE-FIELD:SLOT@" UDG-PF-ROW
   s" TYPE-FIELD:CELLS@" UDG-PF-ROW
   s" TYPE-FIELD:BYTE-OFF@" UDG-PF-ROW
   s" TYPE-FIELD:BYTES@" UDG-PF-ROW
   s" TYPE-FIELD:ALIGN@" UDG-PF-ROW
   s" TYPE-FIELD:FLAGS@" UDG-PF-ROW ;

\ --- positives: the public top-level surface is untouched -------------------

: UDG-EXACT$ ( -- ptr u8 n )
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   s" 5 FOO2 + . cr" UDG-LINE
   SB$ ;

: UDG-OVER$ ( -- ptr u8 n )
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   s" 1 2 3 FOO2 + + + . cr" UDG-LINE
   SB$ ;

: UDG-COMPILED$ ( -- ptr u8 n )          \ checked calling checked: checker-proven, no guard
   SB-RESET
   s" : UDG-A ( n -- n ) 1 + ;" UDG-LINE
   s" : UDG-B ( n -- n ) UDG-A UDG-A ;" UDG-LINE
   s" 7 UDG-B . cr" UDG-LINE
   SB$ ;

: UDG-RAW$ ( -- ptr u8 n )               \ unchecked word: the documented boundary stays open
   SB-RESET
   s" 0 set-check" UDG-LINE
   s" : UDG-RAW 42 . cr ;" UDG-LINE
   s" UDG-RAW" UDG-LINE
   SB$ ;

: UDG-CATCH$ ( -- ptr u8 n )             \ evaluate delivers the reject as a catchable rc-70 throw
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" UDG-LINE
   s" 0 set-check" UDG-LINE
   S\" : UDG-TRY s\" FOO2\" evaluate ;" UDG-LINE
   s" ' UDG-TRY catch . cr" UDG-LINE
   S\" s\" after\" type cr" UDG-LINE
   SB$ ;

: UDG-PROBE$ ( -- ptr u8 n )             \ satisfied-depth CHECK! probing idiom still works
   SB-RESET
   S\" s\" : UDG-G ( n -- n ) 1 + ;\" CHECK! . cr" UDG-LINE
   SB$ ;

: UDG-POSITIVES ( -- )
   s" exact declared depth still runs" T-LABEL
   UDG-EXACT$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 10" UDG-ASSERT-OUT
   s" surplus depth still runs" T-LABEL
   UDG-OVER$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 9" UDG-ASSERT-OUT
   s" compiled checked-to-checked calls carry no guard" T-LABEL
   UDG-COMPILED$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 9" UDG-ASSERT-OUT
   s" unchecked user word stays executable (documented boundary)" T-LABEL
   UDG-RAW$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 42" UDG-ASSERT-OUT
   s" evaluate delivers a catchable rc-70 reject and execution continues" T-LABEL
   UDG-CATCH$ UDG-RUN-LOAD UDG-ASSERT-OK
   s" 70" UDG-ASSERT-OUT
   s" after" UDG-ASSERT-OUT
   s" satisfied-depth CHECK! probe still works" T-LABEL
   UDG-PROBE$ UDG-RUN-LOAD UDG-ASSERT-OK ;

: UDG-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-udg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u UDG-ROOT-BUF UDG-ROOT-U UDG-COPY!
   UDG-ROOT CLEANUP-TREE+
   UDG-ROOT s" forge.f" UDG-CHILD-BUF JOIN-PATH UDG-CHILD-U ! ;

: UDG-CLEANUP ( -- )
   CLEANUP-RUN
   UDG-ROOT EXISTS? TFALSE ;

: UDG-MAIN ( -- )
   T-RESET
   UDG-PREPARE
   UDG-NEG-BARE
   UDG-NEG-SHAPES
   UDG-COMPILE-IMM
   UDG-NEG-PRIMS
   UDG-PF-PACKAGE
   UDG-POSITIVES
   UDG-MINIMUM:ATOMIC-SHAPE
   UDG-MINIMUM:BOUNDARY
   UDG-CLEANUP
   T-REPORT
   s" underdepth-gate: ok" type cr ;

UDG-MAIN
