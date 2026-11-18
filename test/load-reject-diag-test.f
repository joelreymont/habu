split-str
\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself: a
\ checked load refusal exits 70 with a named diagnostic on stderr; an
\ unmodeled immediate in a TRUSTED: body is refused by the checker's preflight
\ as it is read, before any compiler sees it. Covers the direct --load,
\ require-chain and checked-body legs on generated fixtures; the child engine is
\ HABU_UNDER_TEST when set, else the running engine. A file `?TARGET` stops is
\ rejected in REJECTION's form: prose, or one JSON line under REJECTION:JSON.
\ A rejected definition prints its one diagnostic and nothing else.
\ A bare word a global and a used package both define is rejected in that form.
\ A SOURCE-ROOT word's uncaught throw on a path prints the record it carries.
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require tools/gate-json-assert-core.f
require src/lib/argv.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package LOAD-REJECT-TEST

$1000 constant CAP
3 constant TEST-OUT-U
5 constant TEST-ERR-U

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot
create BASE PATH-CAP allot
create UNDEF-P PATH-CAP allot
create BODY-P PATH-CAP allot
create OUTER-P PATH-CAP allot
create FRAG-P PATH-CAP allot
create SIDE-P PATH-CAP allot
create INC-P PATH-CAP allot
create REQ-P PATH-CAP allot
create CALLER-P PATH-CAP allot
create MISS-P PATH-CAP allot
create REBOUND-P PATH-CAP allot
create TOP-INC-P PATH-CAP allot
create TOP-REQ-P PATH-CAP allot
create RUNTIME-P PATH-CAP allot
create MODELED-P PATH-CAP allot
create TRUSTED-P PATH-CAP allot
create BPS-P PATH-CAP allot
create BPF-P PATH-CAP allot
create RES-P PATH-CAP allot
create RTYPE-P PATH-CAP allot
create RNEST-P PATH-CAP allot
create RCATCH-P PATH-CAP allot
create CF-P PATH-CAP allot
create TARGET-P PATH-CAP allot
create TARGET-JSON-P PATH-CAP allot
create TARGET-DEF-P PATH-CAP allot
create ONCE-P PATH-CAP allot
create ONCE-UNDEF-P PATH-CAP allot
create CAPTURE-P PATH-CAP allot
create ONCE-JSON-P PATH-CAP allot
create SHADOW-P PATH-CAP allot
create MARK-P PATH-CAP allot

variable BASE-U
variable UNDEF-U
variable BODY-U
variable OUTER-U
variable FRAG-U
variable SIDE-U
variable INC-U
variable REQ-U
variable CALLER-U
variable MISS-U
variable REBOUND-U
variable TOP-INC-U
variable TOP-REQ-U
variable RUNTIME-U
variable MODELED-U
variable TRUSTED-U
variable BPS-U
variable BPF-U
variable RES-U
variable RTYPE-U
variable RNEST-U
variable RCATCH-U
variable CF-U
variable TARGET-U
variable TARGET-JSON-U
variable TARGET-DEF-U
variable ONCE-U
variable ONCE-UNDEF-U
variable CAPTURE-U
variable ONCE-JSON-U
variable SHADOW-U
variable MARK-U
variable OUT-U
variable ERR-U
variable EXITED
variable RC

: BASE$ ( -- str )
   BASE BASE-U @ ;

: UNDEF$ ( -- str )
   UNDEF-P UNDEF-U @ ;

: BODY$ ( -- ^u8 n )
   BODY-P BODY-U @ ;

: OUTER$ ( -- str )
   OUTER-P OUTER-U @ ;

: FRAG$ ( -- str )
   FRAG-P FRAG-U @ ;

: SIDE$ ( -- str )
   SIDE-P SIDE-U @ ;

: INC$ ( -- str )
   INC-P INC-U @ ;

: REQ$ ( -- str )
   REQ-P REQ-U @ ;

: CALLER$ ( -- str )
   CALLER-P CALLER-U @ ;

: MISS$ ( -- str )
   MISS-P MISS-U @ ;

: REBOUND$ ( -- str )
   REBOUND-P REBOUND-U @ ;

: TOP-INC$ ( -- str )
   TOP-INC-P TOP-INC-U @ ;

: TOP-REQ$ ( -- str )
   TOP-REQ-P TOP-REQ-U @ ;

: RUNTIME$ ( -- str )
   RUNTIME-P RUNTIME-U @ ;

: MODELED$ ( -- str )
   MODELED-P MODELED-U @ ;

: TRUSTED$ ( -- str )
   TRUSTED-P TRUSTED-U @ ;

: BPS$ ( -- str )
   BPS-P BPS-U @ ;

: BPF$ ( -- str )
   BPF-P BPF-U @ ;

: RES$ ( -- str )
   RES-P RES-U @ ;

: RTYPE$ ( -- str )
   RTYPE-P RTYPE-U @ ;

: RNEST$ ( -- str )
   RNEST-P RNEST-U @ ;

: RCATCH$ ( -- str )
   RCATCH-P RCATCH-U @ ;

: CF$ ( -- str )
   CF-P CF-U @ ;

: TARGET$ ( -- str )
   TARGET-P TARGET-U @ ;

: TARGET-JSON$ ( -- str )
   TARGET-JSON-P TARGET-JSON-U @ ;

: TARGET-DEF$ ( -- str )
   TARGET-DEF-P TARGET-DEF-U @ ;

: ONCE$ ( -- str )
   ONCE-P ONCE-U @ ;

: ONCE-UNDEF$ ( -- str )
   ONCE-UNDEF-P ONCE-UNDEF-U @ ;

: CAPTURE$ ( -- str )
   CAPTURE-P CAPTURE-U @ ;

: ONCE-JSON$ ( -- str )
   ONCE-JSON-P ONCE-JSON-U @ ;

: SHADOW$ ( -- str )
   SHADOW-P SHADOW-U @ ;

: MARK$ ( -- str )
   MARK-P MARK-U @ ;

: FRAG-FIXTURE! ( str str str str -- )
   {: pre:^ preu:n frag:^ fragu:n post:^ postu:n path:^ pathu:n :}
   SB-RESET
   pre preu SB-APPEND
   frag fragu SB-APPEND
   post postu SB-APPEND
   path pathu SB$ FS:WRITE-ALL ;

: PATH! ( ^u8 n ^u8 ^n -- )
   {: name:^ nameu:n dst:^ lenp:^ :}
   BASE$ name nameu dst FS:JOIN-PATH lenp ! ;

: ROOT! ( -- )
   FS:CLEANUP-RESET
   s" habu-load-reject-diag" FS:MKTEMP-DIR {: a:^ u:n :}
   a BASE u BYTE-COPY
   u BASE-U !
   BASE$ FS:CLEANUP-TREE+ ;

: PATHS! ( -- )
   s" undef.f" UNDEF-P UNDEF-U PATH!
   s" body.f" BODY-P BODY-U PATH!
   s" outer.f" OUTER-P OUTER-U PATH!
   s" frag.f" FRAG-P FRAG-U PATH!
   s" side.f" SIDE-P SIDE-U PATH!
   s" imm-include.f" INC-P INC-U PATH!
   s" imm-require.f" REQ-P REQ-U PATH!
   s" imm-caller.f" CALLER-P CALLER-U PATH!
   s" imm-missing.f" MISS-P MISS-U PATH!
   s" imm-rebound.f" REBOUND-P REBOUND-U PATH!
   s" top-include.f" TOP-INC-P TOP-INC-U PATH!
   s" top-require.f" TOP-REQ-P TOP-REQ-U PATH!
   s" runtime.f" RUNTIME-P RUNTIME-U PATH!
   s" modeled.f" MODELED-P MODELED-U PATH!
   s" trusted.f" TRUSTED-P TRUSTED-U PATH!
   s" byte-ptr-store.f" BPS-P BPS-U PATH!
   s" byte-ptr-fetch.f" BPF-P BPF-U PATH!
   s" control-flow.f" CF-P CF-U PATH!
   s" residue.f" RES-P RES-U PATH!
   s" residue-type.f" RTYPE-P RTYPE-U PATH!
   s" residue-nest.f" RNEST-P RNEST-U PATH!
   s" residue-caught.f" RCATCH-P RCATCH-U PATH!
   s" target.f" TARGET-P TARGET-U PATH!
   s" target-json.f" TARGET-JSON-P TARGET-JSON-U PATH!
   s" target-def.f" TARGET-DEF-P TARGET-DEF-U PATH!
   s" once.f" ONCE-P ONCE-U PATH!
   s" once-undef.f" ONCE-UNDEF-P ONCE-UNDEF-U PATH!
   s" capture.f" CAPTURE-P CAPTURE-U PATH!
   s" once-json.f" ONCE-JSON-P ONCE-JSON-U PATH!
   s" shadow.f" SHADOW-P SHADOW-U PATH!
   s" mark.f" MARK-P MARK-U PATH! ;

: REJECT-FIXTURES! ( -- )
   UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" FS:WRITE-ALL
   BODY$ s" : LRD-Y ( n -- n ) drop ;" FS:WRITE-ALL
   \ The byte pointer comes from a declared cell: a raw `variable` hands out no
   \ address (E-RAW-CELL-PTR), which would refuse the fixture one definition
   \ before the token these cases pin.
   BPS$ s" TYPED-VARIABLE LRD-P ^u8 : LRD-BASE ( -- ^u8 ) LRD-P @ ; : LRD-BPS ( -- ) 0 LRD-BASE ! ;" FS:WRITE-ALL
   BPF$ s" TYPED-VARIABLE LRD-Q ^u8 : LRD-QBASE ( -- ^u8 ) LRD-Q @ ; : LRD-BPF ( -- n ) LRD-QBASE @ ;" FS:WRITE-ALL
   TARGET$ s" false ?TARGET" FS:WRITE-ALL
   TARGET-JSON$ S\" -1 REJECTION:JSON !\nfalse ?TARGET" FS:WRITE-ALL
   TARGET-DEF$ S\" : LRD-T ( -- ) false ?TARGET ;\nLRD-T" FS:WRITE-ALL
   ONCE$ s" : F ( n -- n ) drop ;" FS:WRITE-ALL
   ONCE-UNDEF$ s" : H ( -- ) FOO ;" FS:WRITE-ALL
   CAPTURE$ S\" REJECTION:CAPTURE\nCP-NOPE" FS:WRITE-ALL
   ONCE-JSON$ S\" -1 REJECTION:JSON !\n: F ( n -- n ) drop ;" FS:WRITE-ALL
   SHADOW$ S\" : LRD-SHADOW ( -- ) ;\npackage LRD-SH\npublic\n: LRD-SHADOW ( -- ) ;\n;package\nusing LRD-SH\nLRD-SHADOW\n"
   FS:WRITE-ALL
   \ A loaded file is a closed program, so the fragment shows it ran by
   \ printing: it can take no cell from its loader and leave none.
   FRAG$ s" 74 ." FS:WRITE-ALL
   SIDE$ S\" [ s\" LRD-FRAGMENT-EXECUTED\" type ]" FS:WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   UNDEF$ SB-APPEND
   OUTER$ SB$ FS:WRITE-ALL ;

: IMM-FIXTURES! ( -- )
   s" -1 REJECTION:JSON ! : LRD-IMM-I ( -- ) 73 include "
   SIDE$ s"  . ;" INC$ FRAG-FIXTURE!
   s" -1 REJECTION:JSON ! : LRD-IMM-R ( -- ) 73 require "
   SIDE$ s"  . ;" REQ$ FRAG-FIXTURE!
   CALLER$ S" : RAW-IMM ( -- ) parse-name 2drop ; immediate
-1 REJECTION:JSON !
: LRD-IMM-S ( mystery:foo -- n ) RAW-IMM 73 ;
" FS:WRITE-ALL
   MISS$ s" : LRD-IMM-M ( -- ) include lrd-missing.f ;" FS:WRITE-ALL
   REBOUND$ S" -1 REJECTION:JSON !
undefine include
: include ( -- ) parse-name 2drop ; immediate
: LRD-IMM-X ( -- ) include ;
" FS:WRITE-ALL ;

\ One definition per append: SB holds one path and its text, not three.
: RUNTIME-DEF+ ( str str -- ) {: pre:^ preu:n post:^ postu:n :}
   SB-RESET
   pre preu SB-APPEND FRAG$ SB-APPEND post postu SB-APPEND
   RUNTIME$ SB$ FS:APPEND-FILE ;

: RUNTIME-FIXTURE! ( -- )
   RUNTIME$ s" " FS:WRITE-ALL
   S\" : RUN-I ( -- ) s\" " S\" \" included ; " RUNTIME-DEF+
   S\" : RUN-R ( -- ) s\" " S\" \" required ; " RUNTIME-DEF+
   S\" : RUN-P ( -- ) s\" " S\" \" provided ; RUN-I RUN-R RUN-P" RUNTIME-DEF+ ;

: RESIDUE-FIXTURES! ( -- )
   RES$ s" 1 2" FS:WRITE-ALL
   RTYPE$ s\" NEWTYPE lrd-length\n0" FS:WRITE-ALL
   s" require " RES$ s" " RNEST$ FRAG-FIXTURE!
   S\" : LRD-T ( -- ) [: s\" " RES$ S\" \" required ;] catch . ; LRD-T"
      RCATCH$ FRAG-FIXTURE! ;

: POSITIVE-FIXTURES! ( -- )
   s" include " FRAG$ s" " TOP-INC$ FRAG-FIXTURE!
   s" require " FRAG$ s" " TOP-REQ$ FRAG-FIXTURE!
   RUNTIME-FIXTURE!
   MODELED$ S\" : PI ( -- ) ; immediate : PIM ( -- n ) PI 73 ; PIM ." FS:WRITE-ALL
   TRUSTED$ S\" -1 REJECTION:JSON ! : LRD-TRUST-IMM ( -- ) parse-name 2drop ; immediate TRUSTED: LRD-TRUST-BODY ( -- n ) LRD-TRUST-IMM 73 ;" FS:WRITE-ALL ;

: FIXTURES! ( -- )
   REJECT-FIXTURES!
   IMM-FIXTURES!
   RESIDUE-FIXTURES!
   POSITIVE-FIXTURES! ;

: SETUP ( -- )
   ROOT!
   PATHS!
   FIXTURES! ;

: STORE! ( len len PROCESS:ending str -- )
   {: outu:len erru:len oc src:^ srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT OUT-U @ ERR ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

\ Runs the engine under test on the arguments built for PATH.
: LAUNCH ( str -- ) {: path:^ pathu:n :}
   ENGINE-CANDIDATE:PATH$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   path pathu STORE! ;

: RUN ( ^u8 n -- ) {: path:^ pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   path pathu LAUNCH ;

: CHECK-RUN ( str -- ) {: path:^ pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+  path pathu >LEN PROC-ARGV+
   path pathu LAUNCH ;

: CHECK-ALL-RUN ( str -- ) {: path:^ pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+  s" --all-errors" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   path pathu LAUNCH ;

: ERR$ ( -- ^u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ^u8 n )
   OUT OUT-U @ ;

\ Every rejecting load: NON-EMPTY stderr that names the failing token, EMPTY
\ stdout, exit-kind EXIT, rc 70 — the silent-exit-70 red/green discriminator.
: ASSERT-NAMED ( str -- ) {: name:^ nameu:n :}
   ERR-U @ 0 > TTRUE
   ERR$ name nameu CONTAINS? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ 70 T= ;

: ASSERT-OK ( -- )
   EXITED @ TTRUE
   RC @ 0 T=
   ERR-U @ 0 T= ;

: ASSERT-OK-OUT ( str -- )
   {: a:^ u:n :}
   ASSERT-OK
   OUT$ a u CONTAINS? TTRUE ;

\ stderr places the record at PATH's PLACE, `:line:column`.
: ASSERT-AT ( str str -- ) {: path:^ pathu:n place:^ placeu:n :}
   SB-RESET  s"  --> " SB-APPEND  path pathu SB-APPEND  place placeu SB-APPEND
   10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE ;

\ body.f is rejected with the record HEAD at PLACE.
: BODY-REJECT-AT ( str str -- ) {: head:^ headu:n place:^ placeu:n :}
   head headu ASSERT-NAMED
   BODY$ place placeu ASSERT-AT ;

: ASSERT-IMM-DIAG ( n n str str -- )
   {: line:n col:n token:^ tokenu:n help:^ helpu:n :}
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root s" code" GJA-REQ s" E-UNMODELED-IMMEDIATE" GJA-ASSERT-STR
   root s" line" line GJA-ASSERT-INT-FIELD
   root s" column" col GJA-ASSERT-INT-FIELD
   root s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   root s" help" GJA-REQ help helpu GJA-ASSERT-STR
   root GJA-RECORD-FORM ;

: TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN PROCESS:SIGKILL PROCESS-ENDING:SIGNALED s" " STORE!
   EXITED @ TFALSE
   RC @ PROCESS:SIGKILL T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

\ The record names the undefined word and places it in undef.f, where it is
\ written.
: ASSERT-UNDEF ( -- )
   s" error[E-UNDEFINED]: cannot find word `LRD-NO-SUCH-WORD-XYZ`" ASSERT-NAMED
   UNDEF$ s" :1:16" ASSERT-AT ;

: TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word and its line" T-LABEL
   UNDEF$ RUN
   ASSERT-UNDEF ;

: TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   BODY$ RUN
   s" error[E-MISMATCH]" s" :1:20" BODY-REJECT-AT
   ERR$ s" the stack is empty after this" CONTAINS? TTRUE ;

\ Store/fetch target mismatch: `!`/`@` need
\ a cell-width `^a`; a `^u8` must name the pair, not just the token.
: TEST-BYTE-PTR-STORE ( -- )
   s" cell store into a byte pointer names expected/actual" T-LABEL
   BPS$ RUN
   s" error[E-MISMATCH]: `!` takes ^a" ASSERT-NAMED
   BPS$ s" :1:86" ASSERT-AT
   ERR$ S\" = expected: ^a\n" CONTAINS? TTRUE
   ERR$ S\" = found: ^u8\n" CONTAINS? TTRUE ;

\ Each control-flow case is a load of its own: a load stops at its first refusal.
\ Its record HEAD names the reason, placed at the token at PLACE.
: CF-RUN ( str str str -- )
   {: src:^ srcu:n head:^ headu:n place:^ placeu:n :}
   CF$ src srcu FS:WRITE-ALL
   CF$ RUN
   head headu ASSERT-NAMED
   CF$ place placeu ASSERT-AT ;

\ A depth change names the start row, or the else arm's, as expected and the
\ changed row as found.
: CF-ROWS? ( -- )
   ERR$ S\" = expected: n\n  = found: n n\n" CONTAINS? TTRUE ;

: TEST-CONTROL-FLOW ( -- )
   s" a loop body that changes depth names both rows" T-LABEL
   s" : LRD-LD ( n -- n ) 3 0 do dup loop ;"
      s" error[E-MISMATCH]: the loop body changes the stack's depth" s" :1:32" CF-RUN
   CF-ROWS?
   s" an i outside every loop says no loop is open" T-LABEL
   s" : LRD-IO ( -- ) i drop ;"
      s" error[E-NO-LOOP]: no loop is open here" s" :1:17" CF-RUN
   s" a j in a single loop says no outer loop is open" T-LABEL
   s" : LRD-JO ( -- ) 3 0 do j drop loop ;"
      s" error[E-NO-OUTER-LOOP]: no outer loop is open here" s" :1:24" CF-RUN
   s" a then with no if says it closes nothing" T-LABEL
   s" : LRD-TS ( -- ) then ;"
      s" error[E-CONTROL-ORPHAN]: this word closes nothing open here" s" :1:17" CF-RUN
   s" an if arm that changes depth names both rows" T-LABEL
   s" : LRD-IA ( n bool -- n ) if 1 then ;"
      s" error[E-MISMATCH]: the if arm changes the stack's depth" s" :1:31" CF-RUN
   CF-ROWS?
   s" if and else arms of different depths name both rows" T-LABEL
   s" : LRD-AD ( n bool -- n ) if 1 else then ;"
      s" error[E-MISMATCH]: the if and else arms leave different stack depths" s" :1:36" CF-RUN
   CF-ROWS?
   s" an exit inside a loop says it needs unloop" T-LABEL
   s" : LRD-EL ( -- ) 3 0 do exit loop ;"
      s" error[E-EXIT-IN-LOOP]: exit inside a loop needs unloop first" s" :1:24" CF-RUN
   s" a leave that changes depth names both rows" T-LABEL
   s" : LRD-LV ( n -- n ) 3 0 do 1 leave loop ;"
      s" error[E-MISMATCH]: the loop body changes the stack's depth" s" :1:30" CF-RUN
   CF-ROWS?
   s" a loop body that changes the return stack's depth says so" T-LABEL
   s" : LRD-RD ( -- ) 3 0 do 1 >r loop ;"
      s" error[E-LOOP-RETURN-DEPTH]: the loop body changes the return stack's depth" s" :1:29" CF-RUN
   s" a second else says the if already has one" T-LABEL
   s" : LRD-E2 ( bool -- ) if else else then ;"
      s" error[E-ELSE-TWICE]: this if already has an else" s" :1:30" CF-RUN
   s" an if open at ; says a structure is still open" T-LABEL
   s" : LRD-OP ( bool -- ) if ;"
      s" error[E-CONTROL-OPEN]: a control structure is still open at the end of the definition" s" :1:25" CF-RUN
   s" a +loop that meets an if inside its do says it closes nothing" T-LABEL
   s" : LRD-PI ( -- ) 3 0 do 0 0= if 1 +loop then ;"
      s" error[E-CONTROL-ORPHAN]: this word closes nothing open here" s" :1:34" CF-RUN ;

: TEST-BYTE-PTR-FETCH ( -- )
   s" cell fetch from a byte pointer names expected/actual" T-LABEL
   BPF$ RUN
   s" error[E-MISMATCH]: `@` takes ^a" ASSERT-NAMED
   BPF$ s" :1:88" ASSERT-AT
   ERR$ S\" = expected: ^a\n" CONTAINS? TTRUE
   ERR$ S\" = found: ^u8\n" CONTAINS? TTRUE ;

\ body.f holding the one line SRC is rejected with this whole record: MSG, the
\ token at column COL marked by MARK (its carets and label), then TAIL (notes
\ and help).
: PARAMETRIC-REJECT ( str str n str str -- )
   {: src:^ srcu:n msg:^ msgu:n col:n mark:^ marku:n tail:^ tailu:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   s" E-NONPARAMETRIC-EFFECT" ASSERT-NAMED
   SB-RESET  s" error[E-NONPARAMETRIC-EFFECT]: " SB-APPEND  msg msgu SB-APPEND
   S\" \n --> " SB-APPEND  BODY$ SB-APPEND  s" :1:" SB-APPEND  col FMT:SB-INT
   S\" \n  |\n1 | " SB-APPEND  src srcu SB-APPEND  S\" \n  |" SB-APPEND
   col 0 ?do 32 SB-APPEND-C loop
   mark marku SB-APPEND  S\" \n  " SB-APPEND  tail tailu SB-APPEND  S\" \n\n" SB-APPEND
   ERR$ SB$ STR= TTRUE ;

: TEST-PARAMETRIC ( -- )
   s" generic output cannot forge a pointer through an integer" T-LABEL
   s" : LRD-MINT ( -- a ) 123 ; : LRD-PTR ( -- ^u8 ) LRD-MINT ;"
   s" declared type variable `a` is specialized to a concrete type; a declared effect must stay parametric over its quantifier"
   21 s" ^^^ makes `a` a concrete type here"
   S\" = declared: -- a\n  = inferred: -- n\n  = help: declare a concrete type in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" numeric fetch requires a numeric pointee" T-LABEL
   s" : LRD-FETCH ( ^a -- n ) @ ;"
   s" declared type variable `a` is specialized to a concrete type; a declared effect must stay parametric over its quantifier"
   25 s" ^ makes `a` a concrete type here"
   S\" = declared: ^a -- n\n  = inferred: ^n -- n\n  = help: declare a concrete type in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" quotation output cannot specialize a declared variable" T-LABEL
   s" : LRD-QUOT ( [ -- a ] -- n ) execute ;"
   s" declared type variable `a` is specialized to a concrete type; a declared effect must stay parametric over its quantifier"
   30 s" ^^^^^^^ makes `a` a concrete type here"
   S\" = declared: [ -- a ] -- n\n  = inferred: [ -- n ] -- n\n  = help: declare a concrete type in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" family argument cannot specialize a declared variable" T-LABEL
   s" NEWTYPE lrd-box<a> CAST: LRD-CONCRETE ( n -- lrd-box<n> ) : LRD-PARAM ( n -- lrd-box<a> ) LRD-CONCRETE ;"
   s" declared type variable `a` is specialized to a concrete type; a declared effect must stay parametric over its quantifier"
   91 s" ^^^^^^^^^^^^ makes `a` a concrete type here"
   S\" = declared: n -- lrd-box<a>\n  = inferred: n -- lrd-box<n>\n  = help: declare a concrete type in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" raw storage cannot restrict a declared generic input" T-LABEL
   s" create LRD-RAW 1 cells allot : LRD-RAW-PUT ( a -- ) LRD-RAW ! ;"
   s" declared type variable `a` is restricted by raw storage; its declared kind must stay unchanged"
   61 s" ^ restricts `a` here"
   S\" = declared: a --\n  = inferred: a --\n  = help: declare the type the raw storage holds in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" raw storage cannot restrict a quotation output variable" T-LABEL
   s" create LRD-RAW 1 cells allot : LRD-RAW-QUOT ( [ -- a ] -- ) execute LRD-RAW ! ;"
   s" declared type variable `a` is restricted by raw storage; its declared kind must stay unchanged"
   77 s" ^ restricts `a` here"
   S\" = declared: [ -- a ] --\n  = inferred: [ -- a ] --\n  = help: declare the type the raw storage holds in place of `a`, or keep the body generic over `a`"
   PARAMETRIC-REJECT
   s" concrete storage and generic transport retain their declared effects" T-LABEL
   BODY$ s" create LRD-RAW 1 cells allot : LRD-RAW-N ( n -- ) LRD-RAW ! ; : LRD-ID ( a -- a ) ; : LRD-ROUNDTRIP ( n -- n ) LRD-ID dup LRD-RAW-N ; 41 LRD-ROUNDTRIP . cr" FS:WRITE-ALL
   BODY$ RUN s" 41" ASSERT-OK-OUT
   s" family arguments keep distinct declared variables" T-LABEL
   s" NEWTYPE lrd-pair<a,b> : LRD-ID ( lrd-pair<a,a> -- lrd-pair<a,a> ) ; : LRD-ALIAS ( lrd-pair<a,b> -- lrd-pair<a,b> ) LRD-ID ;"
   s" declared type variables `b` and `a` are unified; each declared quantifier must stay a distinct variable"
   116 s" ^^^^^^ unifies `b` and `a` here"
   S\" = declared: lrd-pair<a,b> -- lrd-pair<a,b>\n  = inferred: lrd-pair<a,a> -- lrd-pair<a,a>\n  = help: write one type variable for `b` and `a`, or keep the body from making them one type"
   PARAMETRIC-REJECT ;

\ An integer literal too big for a cell is rejected as a literal, named where
\ the body holds it, whichever loop reads the body.
: TEST-BIG-LITERAL ( -- )
   s" an integer literal too big for a cell is rejected as a literal" T-LABEL
   BODY$ s" TRUSTED: LRD-BIG ( -- n ) 99999999999999999999999 ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-LITERAL-RANGE]: the number `99999999999999999999999` is out of range" s" :1:27"
      BODY-REJECT-AT
   ERR$ s" = range: a cell holds `-9223372036854775808` to `9223372036854775807`" CONTAINS? TTRUE ;

\ A top-level `target` beside the program's own TARGET could mean either, and is refused by name.
: TEST-SECTION-CLASH ( -- )
   s" a section word beside a word of its name is refused" T-LABEL
   BODY$ S\" package LRD-CLASH\n: TARGET ( -- ) ;\ntarget\n;package\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-SECTION-WORD]: `target` is the section word, and a word of that name is in scope"
      s" :3:1" BODY-REJECT-AT ;

\ T0, arm64's tier 0, refuses a body no scan certified at its first token the
\ scan could not bind or no path reaches.
: TEST-T0-UNBOUND ( -- )
   s" T0 refuses a MATCH variant with no OF" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-NOOF ( n -- n ) construct lrd-choice scalar MATCH lrd-choice scalar 10 + ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: a `MATCH` arm's variant is not followed by `of` at `10`"
      s" :1:159" BODY-REJECT-AT
   s" T0 refuses a MATCH with no value proved under it" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-UNDER ( -- n ) MATCH lrd-choice empty OF 0 ENDOF scalar OF ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: the compiler has no family facts for `lrd-choice`"
      s" :1:112" BODY-REJECT-AT
   s" T0 refuses a MATCH over an unknown family" T-LABEL
   BODY$ s" TRUSTED: LRD-NOFAM ( n -- n ) 5 swap MATCH lrd-nofam empty OF 1 ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: the compiler has no family facts for `lrd-nofam`"
      s" :1:44" BODY-REJECT-AT
   s" T0 refuses a MATCH over an unknown variant at the variant" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-NOVAR ( lrd-choice -- n ) MATCH lrd-choice bogus OF 1 ENDOF scalar OF ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: the compiler has no family facts for `bogus`"
      s" :1:134" BODY-REJECT-AT
   s" T0 refuses dead code after exit" T-LABEL
   BODY$ s" TRUSTED: LRD-DEAD ( -- n ) 1 exit DUP ; LRD-DEAD . cr" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-CONTROL]: no path reaches `DUP`" s" :1:35" BODY-REJECT-AT
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead string literal at its opener" T-LABEL
   BODY$ S\" TRUSTED: LRD-DEADS ( -- n ) 1 exit s\" ab\" drop ;" FS:WRITE-ALL
   BODY$ RUN
   S\" error[E-T0-CONTROL]: no path reaches `s\"`" s" :1:36" BODY-REJECT-AT
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead quotation inside a quotation at its opener" T-LABEL
   BODY$ s" TRUSTED: LRD-DEADQ ( -- [ -- n ] ) [: 1 exit [: 2 ;] ;] ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-CONTROL]: no path reaches `[:`" s" :1:46" BODY-REJECT-AT
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead string holding ;] inside a quotation at its opener" T-LABEL
   BODY$ S\" TRUSTED: LRD-DEADQS ( -- [ -- n ] ) [: 1 exit s\" a ;] b\" drop ;] ;" FS:WRITE-ALL
   BODY$ RUN
   S\" error[E-T0-CONTROL]: no path reaches `s\"`" s" :1:47" BODY-REJECT-AT
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a construct of an unknown variant" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-BADVAR ( -- n ) construct lrd-choice bogus 1 ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: the compiler has no family facts for `bogus`"
      s" :1:128" BODY-REJECT-AT
   s" T0 refuses a construct of an unknown family" T-LABEL
   BODY$ s" TRUSTED: LRD-BADFAM ( -- n ) construct lrd-nofam scalar 1 ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-MATCH]: the compiler has no family facts for `lrd-nofam`"
      s" :1:40" BODY-REJECT-AT ;

\ A refused step in a TRUSTED: body lowers as the raw effect the scan splices,
\ so `2dup` over a two-cell view copies two cells.
: TEST-T0-RAW-SPLIT ( -- )
   s" T0 compiles a refused TRUSTED: step as its raw effect" T-LABEL
   BODY$ s" require src/lib/c2-memory.f require src/lib/c2-bytes.f TRUSTED: LRD-SPLIT ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> ^u8 n ) 2dup ; variable LRD-LEN variable LRD-LEN2 : LRD-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) LRD-SPLIT LRD-LEN ! drop C2-BYTES:MUT-LENGTH swap LRD-LEN2 ! ; : LRD-RUN ( -- ) C2-MEM:OWNER-SIZE [: LRD-ROOT ;] C2-MEM:WITH-MUT ; LRD-RUN LRD-LEN @ . LRD-LEN2 @ . cr" FS:WRITE-ALL
   BODY$ RUN S\" 8\n8\n" ASSERT-OK-OUT ;

\ A refused `!` of a two-cell product in a TRUSTED: body splices its own typed
\ rows and keeps their width fact, so it stores both cells.
: TEST-T0-RAW-STORE ( -- )
   s" T0 compiles a refused TRUSTED: store at its field width" T-LABEL
   BODY$ s" STRUCTURE lrd-pt FIELD x n FIELD y n ;STRUCTURE TYPED-VARIABLE LRD-PV lrd-pt TRUSTED: LRD-SET ( n n ^lrd-pt -- ) ! ; : LRD-STORE ( -- n ) 7 3 4 LRD-PV LRD-SET ; LRD-STORE . depth . cr" FS:WRITE-ALL
   BODY$ RUN S\" 7\n0\n" ASSERT-OK-OUT ;

\ A storage declaration whose definer cannot size its type is the checker's
\ refusal, named for the declared word in HEAD and placed at the unknown type.
: STORAGE-REJECT ( str str -- ) {: src:^ srcu:n head:^ headu:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   head headu ASSERT-NAMED
   ERR$ s" ^ unknown type" CONTAINS? TTRUE ;

: TEST-STORAGE ( -- )
   s" undefined storage types are named refusals" T-LABEL
   s" 4 TYPED-BUFFER LRD-STG-TB lrd-stg-none" 
      s" error[E-BAD-STORAGE]: the type of `LRD-STG-TB`" STORAGE-REJECT
   s" TYPED-VARIABLE LRD-STG-TV lrd-stg-none" 
      s" error[E-BAD-STORAGE]: the type of `LRD-STG-TV`" STORAGE-REJECT
   s" 4 LAYOUT-BUFFER LRD-STG-LB lrd-stg-none" 
      s" error[E-BAD-STORAGE]: the type of `LRD-STG-LB`" STORAGE-REJECT
   s" DYNAMIC-BUFFER LRD-STG-DB lrd-stg-none" 
      s" error[E-BAD-STORAGE]: the type of `LRD-STG-DB`" STORAGE-REJECT
   s" DEFER-LAYOUT-BUFFER LRD-STG-DL lrd-stg-none" 
      s" error[E-BAD-STORAGE]: the type of `LRD-STG-DL`" STORAGE-REJECT ;

\ SRC stores a SCRATCH-ONLY family where a build saves it: the record HEAD
\ holds HELP.
: SCRATCH-REJECT ( str str str -- )
   {: src:^ srcu:n head:^ headu:n help:^ helpu:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   ERR$ help helpu CONTAINS? TTRUE
   head headu ASSERT-NAMED ;

: TEST-SCRATCH-STORAGE ( -- )
   s" a SCRATCH-ONLY family in saved storage is a named rejection" T-LABEL
   S\" NEWTYPE lrd-scr SCRATCH-ONLY\nTYPED-VARIABLE LRD-SCR-TV lrd-scr"
   s" error[E-BAD-STORAGE]: the type of `LRD-SCR-TV` holds a value valid only in the process that made it"
   s" write `SCRATCH-ONLY` after the type of `LRD-SCR-TV`, which keeps it out of the saved image"
      SCRATCH-REJECT
   S\" NEWTYPE lrd-scr SCRATCH-ONLY\n4 LAYOUT-BUFFER LRD-SCR-LB lrd-scr"
   s" error[E-BAD-STORAGE]: the type of `LRD-SCR-LB` holds a value valid only in the process that made it"
   s" keep the values of `LRD-SCR-LB` in a TYPED-VARIABLE or TYPED-BUFFER declared `SCRATCH-ONLY`"
      SCRATCH-REJECT ;

\ SRC as the body exits RC, its stderr holding the record HEAD placed at PLACE.
: CODE-REJECT-AT ( str str str n -- )
   {: src:^ srcu:n head:^ headu:n place:^ placeu:n rc:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   ERR$ head headu CONTAINS? TTRUE
   BODY$ place placeu ASSERT-AT
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ rc T= ;

\ A definer with no signature stops at the word being defined, or at `does>`
\ for a clause.
: TEST-MISSING-SIGNATURE ( -- )
   s" a definer's missing signature names its class, token and line" T-LABEL
   s" : LRD-MK ( n -- ) create , does> @ ;"
   s" error[E-MISSING-SIGNATURE]: `does>` has no closed `( -- )` signature"
      s" :1:28" 70 CODE-REJECT-AT
   s" defer LRD-D"
   s" error[E-MISSING-SIGNATURE]: `LRD-D` has no closed `( -- )` signature"
      s" :1:7" 70 CODE-REJECT-AT ;

\ Reopening a package the engine sealed stops at the package's name.
: TEST-SEALED-PACKAGE ( -- )
   s" a sealed package's reopening names its class, package and line" T-LABEL
   S\" package NDICT\n;package"
   s" error[E-SEALED-PACKAGE]: `NDICT` would change a sealed package"
      s" :1:9" 70 CODE-REJECT-AT ;

\ A definition into a package the engine sealed stops at its whole name.
: TEST-SEALED-DEFINE ( -- )
   s" a definition into a sealed package names its class, whole name and line" T-LABEL
   s" : NDICT:FOO ( -- ) ;"
   s" error[E-SEALED-PACKAGE]: `NDICT:FOO` would change a sealed package"
      s" :1:3" 70 CODE-REJECT-AT ;

\ A name a sealed package already holds is refused as sealed, as the check
\ refuses it: the seal is asked before the duplicate.
: TEST-SEALED-DUPLICATE ( -- )
   s" a duplicate definition into a sealed package names the seal" T-LABEL
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\n;package\n: SP:A ( -- ) ;\n"
   s" error[E-SEALED-PACKAGE]: `SP:A` would change a sealed package"
      s" :6:3" 70 CODE-REJECT-AT
   s" a duplicate export into a sealed package names the seal" T-LABEL
   SB-RESET
   S\" package SP2\npublic\n: A ( -- ) ;\n" SB-APPEND
   S\" : MK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" SB-APPEND
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\nEXPORT SP2:A\n" SB-APPEND
   S\" s\" ACCEPTED\" type cr\n;package\n" SB-APPEND
   SB$ s" error[E-SEALED-PACKAGE]: `SP2:A` would change a sealed package"
      s" :10:8" 70 CODE-REJECT-AT ;

\ A string `s"` copies out of the file, which its line 3 `evaluate`s, is not
\ the file's bytes, so its rejection is placed at that `evaluate`, with the
\ string in an evaluated note.
: ASSERT-EVALUATED ( str str -- )
   {: text:^ textu:n label:^ labelu:n :}
   SB-RESET s" = evaluated: " SB-APPEND text textu SB-APPEND
   ERR$ SB$ CONTAINS? TTRUE
   SB-RESET s" ^ " SB-APPEND label labelu SB-APPEND
   ERR$ SB$ CONTAINS? TTRUE ;

: TEST-EVALUATED-PLACE ( -- )
   s" an evaluated string's rejection is placed at its evaluate" T-LABEL
   BODY$ S\" \n\ns\" lrd-zz-undefined\" evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: cannot find word `lrd-zz-undefined`" ASSERT-NAMED
   BODY$ s" :3:22" ASSERT-AT
   s" lrd-zz-undefined" s" not found" ASSERT-EVALUATED ;

\ A slice of the file's own bytes that the file `evaluate`s is file text, so a
\ refusal in it names the file's line of the refused token, whichever writer
\ refuses it.
: TEST-EVALUATED-SLICE ( -- )
   s" an evaluated slice of the file is refused at its line" T-LABEL
   BODY$ S\" \n\nparse-name lrd-zz-undefined evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: cannot find word `lrd-zz-undefined`" s" :3:12"
      BODY-REJECT-AT
   s" the checker places a refusal in an evaluated slice on the same line" T-LABEL
   BODY$ S\" \n\n5 parse-name c@ evaluate drop\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-MISMATCH]: `c@` takes ^u8" s" :3:14" BODY-REJECT-AT
   s" the native compiler places a refusal in a slice past the evaluating line" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 27 evaluate\n: LRD-QQ ( -- )\n5 c@ drop ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-MISMATCH]: `c@` takes ^u8" s" :5:3" BODY-REJECT-AT
   s" the native compiler places a refusal at the closing ; of a slice" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 29 evaluate\nTRUSTED: LRD-SF ( -- ) 0 if\n;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-CONTROL]: a control structure is still open at `;`" s" :5:1"
      BODY-REJECT-AT
   s" the checker hook places a checker-only duplicate in a slice at its line" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n ;STRUCTURE\" TYPE-DECL:REPLAY-STRUCTURE drop 2drop ;\nLRD-PREPLAY\nparse-name x drop 24 + 30 evaluate\n: LRDREP:MAKE ( n -- n ) 1 + ;\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-DUPLICATE-DEFINITION]: `LRDREP:MAKE` is already defined" CONTAINS? TTRUE
   BODY$ s" :4:3" ASSERT-AT
   EXITED @ TTRUE
   RC @ 70 T= ;

\ body.f keeps a token of its line 2 and includes mark.f, which writes a
\ record at it: the record is placed in body.f, not in the text that loaded it.
: ASSERT-OUTER-PLACE ( -- )
   EXITED @ TTRUE
   RC @ 0 T=
   SB-RESET  s"  --> " SB-APPEND  BODY$ SB-APPEND  s" :2:12" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ COUNT-SUB 2 T= ;

: TEST-OUTER-FILE-PLACE ( -- )
   MARK$ s" LRD-MARK" FS:WRITE-ALL
   SB-RESET
   S\" PTR-VARIABLE LRD-TOK-A  variable LRD-TOK-U\n" SB-APPEND
   S\" parse-name LRD-HERE LRD-TOK-U ! LRD-TOK-A !\n" SB-APPEND
   S\" : LRD-MARK ( -- )\n   s\" E-LRD-MARK\" REJECTION:OPEN  s\" a kept record\" REJECTION:EMIT-STR\n"
      SB-APPEND
   S\"   LRD-TOK-A @ LRD-TOK-U @ REJECTION:AT  s\" here\" REJECTION:EMIT-STR\n" SB-APPEND
   S\"   REJECTION:KEEP ;\nLRD-MARK\ninclude " SB-APPEND  MARK$ SB-APPEND  10 SB-APPEND-C
   BODY$ SB$ FS:WRITE-ALL
   s" a token of an outer file is placed in that file under --load" T-LABEL
   BODY$ RUN  ASSERT-OUTER-PLACE
   s" a token of an outer file is placed in that file under tools/check.f" T-LABEL
   BODY$ CHECK-RUN  ASSERT-OUTER-PLACE ;

\ A structure the checker finds open at a body's closing `;`, a plain body's or
\ a does> clause's, is placed at that `;` (column 1 of PLACE's line).
: ASSERT-CLOSE-LINE ( str -- ) {: place:^ placeu:n :}
   BODY$ RUN
   s" error[E-CONTROL-OPEN]: a control structure is still open at the end of the definition"
      place placeu BODY-REJECT-AT ;

: TEST-OPEN-AT-CLOSE ( -- )
   s" an if open at a later line's ; is refused on the line of the ;" T-LABEL
   BODY$ S\" : LRD-OI ( bool -- ) if\n\n;\n" FS:WRITE-ALL
   s" :3:1" ASSERT-CLOSE-LINE
   s" a does> clause leaving an if open is refused on the line of its ;" T-LABEL
   BODY$ S\" : LRD-MKO ( n -- ) create ,\ndoes> ( -- )\n@ 0= if\n;\n" FS:WRITE-ALL
   s" :4:1" ASSERT-CLOSE-LINE ;

\ A load while a declaration is open is rejected at the declaration's opener.
: TEST-LOAD-IN-DECLARATION ( -- )
   s" an include inside an open FUNCTION: is rejected at the FUNCTION:" T-LABEL
   BODY$ S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: LRD-FN no_symbol ( -- )\ninclude lrd-none.f\n;FUNCTION\n"
   FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNCLOSED]: `FUNCTION:` is still open when `include` loads a file"
   s" :3:1" BODY-REJECT-AT ;

\ A stored signature the declaration refuses is placed on its refused token's
\ line, not the name's.
: TEST-BAD-SIGNATURE-LINE ( -- )
   s" a refused stored signature on a later line is refused on that line" T-LABEL
   BODY$ S\" TRUSTED: LRD-BS\n  ( -- nosuch:fam )\n  1 ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `LRD-BS` cannot be recorded"
      s" :2:8" BODY-REJECT-AT ;

\ A name only the checker holds, a STRUCTURE replay with no word behind it, is
\ rejected by check-hook.f DEFINITION through src/core/check-walk.f
\ REJECT-DUPLICATE, placed at the `evaluate` that ran the string, with the
\ string in an evaluated note.
: TEST-REPLAY-DUP-PLACE ( -- )
   s" an evaluated checker-only duplicate is placed at its evaluate" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n ;STRUCTURE\" TYPE-DECL:REPLAY-STRUCTURE drop 2drop ;\nLRD-PREPLAY\ns\" : LRDREP:MAKE ( n -- n ) 1 + ;\" evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-DUPLICATE-DEFINITION]: `LRDREP:MAKE` is already defined" CONTAINS? TTRUE
   BODY$ s" :3:36" ASSERT-AT
   s" : LRDREP:MAKE ( n -- n ) 1 + ;" s" already defined" ASSERT-EVALUATED
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ 70 T= ;

: TEST-REPLAY-TASK ( -- )
   s" a replay inside a definition while a task is live is rejected" T-LABEL
   BODY$ S\" require src/lib/task.f\nrequire src/lib/test/wait.f\nrequire src/habu/verify-source.f\nTASK:MIN-STACK TASK:TASK WORKER\nvariable READY\n: SPIN ( -- ) 1 READY atomic! begin TASK:PAUSE again ;\n: GO ( -- ) ['] SPIN WORKER TASK:ACTIVATE  READY 1 WORKER TEST-WAIT:FOR-CELL ;\n: RPL ( ^u8 n -- ) EFFECT:EMPTY define-word [: s\" : RA ( -- ) ;\" VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ;\nGO\ns\" LRD-OUTER\" RPL ;\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-VERIFY-WINDOW]: the checker's replay ran inside a definition while a task is live" COUNT-SUB 1 T=
   BODY$ s" :10:15" ASSERT-AT
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ 70 T=
   s" a task started inside a replay of a definition is rejected and the definition completes" T-LABEL
   BODY$ S\" require src/lib/task.f\nrequire src/lib/test/wait.f\nrequire src/habu/verify-source.f\nTASK:MIN-STACK TASK:TASK WORKER\nvariable READY\n: SPIN ( -- ) 1 READY atomic! begin TASK:PAUSE again ;\n: GO ( -- ) ['] SPIN WORKER TASK:ACTIVATE  READY 1 WORKER TEST-WAIT:FOR-CELL ;\n: RPL ( -- ) [: GO s\" : RA ( -- ) ;\" VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ;\n: EV ( ^u8 n -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word [: RPL ;] catch . ;\ns\" OUTER\" EV 81 ;\nOUTER . cr\n: AFTER ( -- n ) 82 ;\nAFTER . cr\n" FS:WRITE-ALL
   BODY$ RUN
   OUT$ s\" -3604\n81\n\n82\n\n" STR= TTRUE
   ERR-U @ 0 T=
   EXITED @ TTRUE
   RC @ 0 T= ;

\ LRD-NEST's action starts a second verification inside the first one's
\ window; the record stands at the line that ran LRD-NEST.
: TEST-WINDOW-NESTED ( -- )
   s" a verification started inside another one's window is rejected" T-LABEL
   BODY$ S\" require src/habu/verify-source.f\n: LRD-IN ( -- ) s\" : RB ( -- ) ;\" VERIFY:SOURCE-BUF-IN-SCOPE ;\n: LRD-NEST ( -- ) s\" : RA ( -- ) ;\" [: LRD-IN ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE ;\nLRD-NEST\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-VERIFY-WINDOW]: a source verification started inside another one's window" COUNT-SUB 1 T=
   BODY$ s" :4:1" ASSERT-AT
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ 70 T= ;

\ The VERIFY:ON-CANDIDATE hook starts a composition while the one labelled
\ lrd-a is open; the record stands at the start of lrd-a.
: TEST-COMPOSE-NESTED ( -- )
   s" a composition started while another is open is rejected" T-LABEL
   BODY$ S\" require src/habu/verify-source.f\n: LRD-AGAIN ( ^u8 n ^u8 n n n -- )\n   2drop 2drop 2drop  s\" \" s\" b.f\" s\" b\" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;\n: LRD-HOOK ( -- ) ['] LRD-AGAIN is VERIFY:ON-CANDIDATE ;\nLRD-HOOK\n2 VERIFY:CURSOR!\ns\" DU\" s\" lrd-a.f\" s\" lrd-a\" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-VERIFY-WINDOW]: `VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE` ran while a source composition is open" COUNT-SUB 1 T=
   s" lrd-a" s" :1:1" ASSERT-AT
   EXITED @ TTRUE
   RC @ 70 T= ;

\ A refused `cast:` names its code, place, word and reason.
: TEST-CAST ( -- )
   s" a cast of one cell into three names its arity" T-LABEL
   BODY$ s" cast: LRD-WIDE ( n -- ^u8 n )" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-CAST-ARITY]: in `cast: LRD-WIDE`, the output is not one term over the stack below"
      s" :1:7" BODY-REJECT-AT ;

\ A checked body that calls a word acting on source or checker state is refused
\ E-UNSAFE, naming the word and why, placed at the call, with evaluate-closed as
\ the help.
: TEST-UNSAFE ( -- )
   s" a checked body calling evaluate names E-UNSAFE and why" T-LABEL
   BODY$ s" : LRD-EV ( ^u8 n -- ) evaluate ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNSAFE]: `evaluate` reads source or changes the compiler's or checker's state, which no stack effect describes"
      s" :1:23" BODY-REJECT-AT
   ERR$ s" `evaluate-closed`" CONTAINS? TTRUE ;

\ A top-level keyword in a checked body is named as a keyword, with writing it
\ at top level as the help.
: TEST-KEYWORD ( -- )
   s" a checked body using public names it a top-level keyword" T-LABEL
   BODY$ s" : LRD-KW ( -- ) public ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: `public` is a top-level keyword, not a word"
      s" :1:17" BODY-REJECT-AT
   ERR$ s" outside any definition" CONTAINS? TTRUE ;

\ A top-level parser in a checked body is named as a keyword, with its bracket
\ form as the help.
: TEST-KEYWORD-CHAR ( -- )
   s" a checked body using char names it a top-level keyword" T-LABEL
   BODY$ s" : LRD-KCH ( -- n ) char ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: `char` is a top-level keyword, not a word"
      s" :1:20" BODY-REJECT-AT
   ERR$ s" help: write `[char]` in a body" CONTAINS? TTRUE ;

: TEST-KEYWORD-TICK ( -- )
   s" a checked body using ' names it a top-level keyword" T-LABEL
   BODY$ s" : LRD-KT ( -- n ) ' ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: `'` is a top-level keyword, not a word"
      s" :1:19" BODY-REJECT-AT
   ERR$ s" help: write `[']` in a body" CONTAINS? TTRUE ;

\ A `:}` with no `{:` open is told it closes no locals list.
: TEST-LOCALS-END ( -- )
   s" a checked body's stray :} closes no locals list" T-LABEL
   BODY$ s" : LRD-LE ( -- ) :} ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: `:}` closes no locals list"
      s" :1:17" BODY-REJECT-AT
   ERR$ s" ^^ no `{:` open" CONTAINS? TTRUE
   ERR$ s" help: a locals list begins with `{:` and this `:}` closes none: delete it, or write `{:` where the list begins"
      CONTAINS? TTRUE ;

: ASSERT-CONSTANT-KEYWORD ( str -- ) {: at:^ atu:n :}
   s" error[E-UNDEFINED]: `constant` is a top-level keyword, not a word"
      at atu BODY-REJECT-AT
   ERR$ s" outside any definition" CONTAINS? TTRUE ;

\ A checked body's `constant` is named a keyword under either loader, whatever
\ inputs the body declares.
: TEST-KEYWORD-CONSTANT ( -- )
   s" a checked body using constant names it a top-level keyword" T-LABEL
   BODY$ s" : LRD-KC ( -- ) constant ;" FS:WRITE-ALL
   BODY$ RUN  s" :1:17" ASSERT-CONSTANT-KEYWORD
   s" tools/check.f names a body's constant a top-level keyword" T-LABEL
   BODY$ CHECK-RUN  s" :1:17" ASSERT-CONSTANT-KEYWORD
   s" a body whose input feeds constant names it a top-level keyword" T-LABEL
   BODY$ s" : LRD-KC ( n -- ) constant ;" FS:WRITE-ALL
   BODY$ RUN  s" :1:19" ASSERT-CONSTANT-KEYWORD ;

: TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   OUTER$ RUN
   ASSERT-UNDEF ;

\ A file that leaves cells is rejected once, by one record naming the count,
\ placed at the token that pushed the bottom cell, in the innermost file. Paths
\ are matched by their file names: the temporary root may resolve through a
\ symbolic link.
: ASSERT-RESIDUE ( str str -- ) {: msg:^ msgu:n at:^ atu:n :}
   EXITED @ TTRUE
   RC @ 70 T=
   OUT-U @ 0 T=
   ERR$ s" error[E-EVAL-RESIDUE]: " STARTS-WITH? TTRUE
   ERR$ msg msgu CONTAINS? TTRUE
   ERR$ s" E-EVAL-RESIDUE" COUNT-SUB 1 T=
   ERR$ at atu CONTAINS? TTRUE
   ERR$ s" uncaught throw" CONTAINS? TFALSE ;

: TEST-RESIDUE ( -- )
   s" residue names the cell count and the file" T-LABEL
   RES$ RUN
   s" this file leaves 2 cells" s" /residue.f:1:1" ASSERT-RESIDUE
   ERR$ s" = left: n n" CONTAINS? TTRUE
   s" a declaration's leftover cell is residue too" T-LABEL
   RTYPE$ RUN
   s" this file leaves 1 cell " s" /residue-type.f:2:1" ASSERT-RESIDUE
   s" nested residue is named once, at the innermost file" T-LABEL
   RNEST$ RUN
   s" this file leaves 2 cells" s" /residue.f:1:1" ASSERT-RESIDUE
   ERR$ s" residue-nest.f" CONTAINS? TFALSE
   s" caught residue prints its record and keeps its code" T-LABEL
   RCATCH$ RUN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ S\" 70\n" STR= TTRUE
   ERR$ s" error[E-EVAL-RESIDUE]: this file leaves 2 cells" STARTS-WITH? TTRUE
   ERR$ s" /residue.f:1:1" CONTAINS? TTRUE
   ERR$ s" E-EVAL-RESIDUE" COUNT-SUB 1 T= ;

: ASSERT-REJECTED ( n -- )
   {: rc:n :}
   ERR$ SB$ T$=
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ rc T= ;

: ASSERT-TARGET ( -- )
   70 ASSERT-REJECTED ;

: TEST-TARGET ( -- )
   s" a ?TARGET stop prints the rejection form" T-LABEL
   TARGET$ RUN
   SB-RESET
   s" error[E-TARGET]: this file does not build for " SB-APPEND
   HB-TARGET$ SB-APPEND  10 SB-APPEND-C
   s"  --> " SB-APPEND  TARGET$ SB-APPEND  S\" :1:7\n  |\n" SB-APPEND
   S\" 1 | false ?TARGET\n" SB-APPEND
   S\"   |       ^^^^^^^ the target test fails here\n" SB-APPEND
   S\"   = help: build this file only for a target its test admits, or widen the test\n\n"
   SB-APPEND
   ASSERT-TARGET
   s" a ?TARGET stop prints one JSON line under REJECTION:JSON" T-LABEL
   TARGET-JSON$ RUN
   SB-RESET
   S\" {\"severity\":\"error\",\"code\":\"E-TARGET\",\"message\":\"this file does not build for "
   SB-APPEND  HB-TARGET$ SB-APPEND
   S\" \",\"file\":\"" SB-APPEND  TARGET-JSON$ SB-APPEND
   S\" \",\"line\":2,\"column\":7,\"byte_start\":26,\"byte_end\":33" SB-APPEND
   S\" ,\"token\":\"?TARGET\",\"source_line\":\"false ?TARGET\"" SB-APPEND
   S\" ,\"label\":\"the target test fails here\"" SB-APPEND
   S\" ,\"help\":\"build this file only for a target its test admits, or widen the test\"}\n"
   SB-APPEND
   ASSERT-TARGET
   s" a ?TARGET a definition calls has no place: its token is the caller" T-LABEL
   TARGET-DEF$ RUN
   SB-RESET
   s" error[E-TARGET]: this file does not build for " SB-APPEND
   HB-TARGET$ SB-APPEND  10 SB-APPEND-C
   S\"   = help: build this file only for a target its test admits, or widen the test\n\n"
   SB-APPEND
   ASSERT-TARGET ;

\ Each case is its own child: a --load stops at its first rejection.
: TEST-DEF-ONCE ( -- )
   s" a rejected definition prints its diagnostic once" T-LABEL
   ONCE$ RUN
   SB-RESET
   s" error[E-MISMATCH]: F ends with nothing on the stack;" SB-APPEND
   S\"  its signature promises n\n --> " SB-APPEND
   ONCE$ SB-APPEND  S\" :1:16\n  |\n1 | : F ( n -- n ) drop ;\n" SB-APPEND
   S\"   |                ^^^^ the stack is empty after this\n" SB-APPEND
   S\"   = expected: n\n  = found: (nothing)\n" SB-APPEND
   S\"   = declared: n -- n\n  = inferred: n --\n" SB-APPEND
   S\"   = help: leave n after `drop`, or declare ( n -- )\n\n" SB-APPEND
   70 ASSERT-REJECTED
   s" an undefined word prints one E-UNDEFINED" T-LABEL
   ONCE-UNDEF$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `FOO`\n --> " SB-APPEND
   ONCE-UNDEF$ SB-APPEND
   S\" :1:12\n  |\n1 | : H ( -- ) FOO ;\n" SB-APPEND
   S\"   |            ^^^ not found\n" SB-APPEND
   s"   = help: define `FOO` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   70 ASSERT-REJECTED
   s" a rejected definition prints one JSON line" T-LABEL
   ONCE-JSON$ RUN
   SB-RESET
   S\" {\"severity\":\"error\",\"code\":\"E-MISMATCH\"" SB-APPEND
   S\" ,\"message\":\"F ends with nothing on the stack;" SB-APPEND
   S\"  its signature promises n\",\"file\":\"" SB-APPEND
   ONCE-JSON$ SB-APPEND
   S\" \",\"line\":2,\"column\":16,\"byte_start\":35" SB-APPEND
   S\" ,\"byte_end\":39,\"token\":\"drop\"" SB-APPEND
   S\" ,\"source_line\":\": F ( n -- n ) drop ;\"" SB-APPEND
   S\" ,\"label\":\"the stack is empty after this\"" SB-APPEND
   S\" ,\"notes\":{\"expected\":\"n\",\"found\":\"(nothing)\"" SB-APPEND
   S\" ,\"declared\":\"n -- n\",\"inferred\":\"n --\"}" SB-APPEND
   S\" ,\"help\":\"leave n after `drop`, or declare ( n -- )\"}\n" SB-APPEND
   70 ASSERT-REJECTED ;

: TEST-CAPTURE-RELEASE ( -- )
   s" a rejection under a capture that ends the run is printed" T-LABEL
   CAPTURE$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `CP-NOPE`\n --> " SB-APPEND
   CAPTURE$ SB-APPEND
   S\" :2:1\n  |\n2 | CP-NOPE\n  | ^^^^^^^ not found\n" SB-APPEND
   s"   = help: define `CP-NOPE` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   70 ASSERT-REJECTED ;

: TEST-CAPTURE-SEALED ( -- )
   s" a sealed package's rejection under a capture is printed" T-LABEL
   BODY$ S\" package LRD-SEALED\nseal-package\n;package\nREJECTION:CAPTURE\npackage LRD-SEALED\n1 . cr\n"
   FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-SEALED-PACKAGE]: `LRD-SEALED` would change a sealed package\n --> " SB-APPEND
   BODY$ SB-APPEND
   S\" :5:9\n  |\n5 | package LRD-SEALED\n  |         ^^^^^^^^^^ sealed\n" SB-APPEND
   s"   = help: put this in a package of your own:" SB-APPEND
   S\"  a sealed package takes no new words\n\n" SB-APPEND
   70 ASSERT-REJECTED ;

\ An undefined name that is the definition's own gets the `recurse` help only
\ as a call in its body: a tick of `recurse` gives no xt, and `recurse` in a
\ quotation calls the quotation.
: TEST-RECURSE-HELP ( -- )
   s" a call of the definition's own name is told to use recurse" T-LABEL
   BODY$ s" : LRD-FACT ( n -- n ) dup 1 > IF dup 1 - LRD-FACT * THEN ;" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `LRD-FACT`\n --> " SB-APPEND
   BODY$ SB-APPEND
   S\" :1:42\n  |\n1 | : LRD-FACT ( n -- n ) dup 1 > IF dup 1 - LRD-FACT * THEN ;\n" SB-APPEND
   S\"   |                                          ^^^^^^^^ not found\n" SB-APPEND
   s"   = help: `LRD-FACT` is the word this definition defines:" SB-APPEND
   S\"  call it from its own body with `recurse`\n\n" SB-APPEND
   70 ASSERT-REJECTED
   s" a tick of the definition's own name is told to define it" T-LABEL
   BODY$ s" : LRD-TICK ( -- xt ) ['] LRD-TICK ;" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `LRD-TICK`\n --> " SB-APPEND
   BODY$ SB-APPEND
   S\" :1:26\n  |\n1 | : LRD-TICK ( -- xt ) ['] LRD-TICK ;\n" SB-APPEND
   S\"   |                          ^^^^^^^^ not found\n" SB-APPEND
   s"   = help: define `LRD-TICK` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   70 ASSERT-REJECTED
   s" a quotation's call of the definition's own name is told to define it" T-LABEL
   BODY$ s" : LRD-QUOT ( -- ) [: LRD-QUOT ;] drop ;" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `LRD-QUOT`\n --> " SB-APPEND
   BODY$ SB-APPEND
   S\" :1:22\n  |\n1 | : LRD-QUOT ( -- ) [: LRD-QUOT ;] drop ;\n" SB-APPEND
   S\"   |                      ^^^^^^^^ not found\n" SB-APPEND
   s"   = help: define `LRD-QUOT` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   70 ASSERT-REJECTED ;

\ The top-level lrd-held on line 5, after LRD-HOLD's public lrd-held,
\ is told LRD-HOLD's qualified form, not `recurse`: no body is open.
: TEST-UNDEF-HOLDER ( -- )
   s" a top-level word a package holds is told its qualified form" T-LABEL
   BODY$ S\" package LRD-HOLD\npublic\n: lrd-held ( -- ) ;\n;package\nlrd-held\n"
   FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   s" error[E-UNDEFINED]: cannot find word `lrd-held`" SB-APPEND
   S\" \n --> " SB-APPEND  BODY$ SB-APPEND
   S\" :5:1\n  |\n5 | lrd-held\n  | ^^^^^^^^ not found\n" SB-APPEND
   s"   = help: `lrd-held` is public in `LRD-HOLD`: " SB-APPEND
   s" write `LRD-HOLD:lrd-held`, or call it inside " SB-APPEND
   S\" `using LRD-HOLD` … `;using`\n\n" SB-APPEND
   70 ASSERT-REJECTED ;

\ The keyword starts line 1, which holds LINE.
: BODY-ONLY-EXPECT ( str str -- ) {: word:^ wordu:n line:^ lineu:n :}
   SB-RESET
   s" error[E-UNDEFINED]: `" SB-APPEND  word wordu SB-APPEND
   s" ` works only inside a definition" SB-APPEND
   S\" \n --> " SB-APPEND  BODY$ SB-APPEND
   S\" :1:1\n  |\n1 | " SB-APPEND  line lineu SB-APPEND
   S\" \n  | " SB-APPEND
   wordu 0 ?do [char] ^ SB-APPEND-C loop
   s"  outside a definition" SB-APPEND  10 SB-APPEND-C
   s"   = help: move `" SB-APPEND  word wordu SB-APPEND
   S\" ` into a `:` … `;` body\n\n" SB-APPEND ;

: TEST-BODY-ONLY ( -- )
   s" a top-level if is told to move into a definition" T-LABEL
   BODY$ S\" if\n" FS:WRITE-ALL
   BODY$ RUN
   s" if" s" if" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f tells a top-level if to move into a definition" T-LABEL
   BODY$ CHECK-RUN
   s" if" s" if" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" an unclosed top-level {: is told to move into a definition" T-LABEL
   BODY$ S\" {: a\n" FS:WRITE-ALL
   BODY$ RUN
   s" {:" s" {: a" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f tells an unclosed top-level {: to move into a definition"
   T-LABEL
   BODY$ CHECK-RUN
   s" {:" s" {: a" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" a top-level {: require :} is told to move into a definition" T-LABEL
   BODY$ S\" {: require :}\n" FS:WRITE-ALL
   BODY$ RUN
   s" {:" s" {: require :}" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f tells a top-level {: require :} to move into a definition"
   T-LABEL
   BODY$ CHECK-RUN
   s" {:" s" {: require :}" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f tells an if before an unclosed {: to move into a definition"
   T-LABEL
   BODY$ S\" if {: a\n" FS:WRITE-ALL
   BODY$ CHECK-RUN
   s" if" s" if {: a" BODY-ONLY-EXPECT  70 ASSERT-REJECTED ;

: TEST-TOP-LOCALS-END ( -- )
   s" a top-level :} is told it closes no locals list" T-LABEL
   BODY$ S\" :}\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   s" error[E-UNDEFINED]: `:}` closes no locals list" SB-APPEND
   S\" \n --> " SB-APPEND  BODY$ SB-APPEND
   S\" :1:1\n  |\n1 | :}\n  | ^^ no `{:` open\n" SB-APPEND
   S\"   = help: delete it: a locals list is written only inside a definition\n\n"
      SB-APPEND
   70 ASSERT-REJECTED ;

\ Under --all-errors the scan goes on past the keyword, but not into its operand.
: TEST-BODY-OPERAND ( -- )
   s" tools/check.f --all-errors reads past a top-level [char]'s operand" T-LABEL
   BODY$ S\" [char] zzq\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   s" [char]" s" [char] zzq" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f --all-errors reads past a top-level {: group" T-LABEL
   BODY$ S\" {: a b :}\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   s" {:" s" {: a b :}" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f --all-errors reads past a top-level {: to the source's end" T-LABEL
   BODY$ S\" {: a\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   s" {:" s" {: a" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f --all-errors reads a top-level [type]'s operand as a type" T-LABEL
   BODY$ S\" [type] box< n >\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   s" [type]" s" [type] box< n >" BODY-ONLY-EXPECT  70 ASSERT-REJECTED
   s" tools/check.f --all-errors ends a top-level [type]'s operand before ;]" T-LABEL
   BODY$ S\" [type] ;]\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   ERR$ s" error[E-UNDEFINED]" COUNT-SUB 2 T=
   ERR$ s" `;]` works only inside a definition" CONTAINS? TTRUE
   RC @ 70 T=
   s" tools/check.f --all-errors reads a top-level is past a comment" T-LABEL
   BODY$ S\" is ( c ) zzq\n" FS:WRITE-ALL
   BODY$ CHECK-ALL-RUN
   s" is" s" is ( c ) zzq" BODY-ONLY-EXPECT  70 ASSERT-REJECTED ;

\ The record of keyword KW named at column COL of the one line SRC.
: KEYWORD-EXPECT ( str str n -- )
   {: kw:^ kwu:n src:^ srcu:n col:n :}
   SB-RESET
   s" error[E-UNDEFINED]: `" SB-APPEND  kw kwu SB-APPEND
   s" ` is a keyword, not a word" SB-APPEND
   S\" \n --> " SB-APPEND  BODY$ SB-APPEND  s" :1:" SB-APPEND  col FMT:SB-INT
   S\" \n  |\n1 | " SB-APPEND  src srcu SB-APPEND  S\" \n  |" SB-APPEND
   col 0 ?do 32 SB-APPEND-C loop
   kwu 0 ?do 94 SB-APPEND-C loop
   S\"  not a word\n" SB-APPEND
   s"   = help: name a word defined with `:` … `;` that uses `" SB-APPEND
   kw kwu SB-APPEND  S\" `\n\n" SB-APPEND ;

\ The load and tools/check.f give SRC, which names keyword KW at column COL
\ where a word is needed, KEYWORD-EXPECT's record.
: KEYWORD-REJECT ( str str n -- )
   {: kw:^ kwu:n src:^ srcu:n col:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   kw kwu src srcu col KEYWORD-EXPECT  70 ASSERT-REJECTED
   BODY$ CHECK-RUN
   kw kwu src srcu col KEYWORD-EXPECT  70 ASSERT-REJECTED ;

: TEST-KEYWORD-OPERAND ( -- )
   s" a top-level tick of if is told it is not a word" T-LABEL
   s" if" s" ' if drop" 3 KEYWORD-REJECT
   s" a postpone of if is told it is not a word" T-LABEL
   s" if" s" : X ( -- ) postpone if ;" 21 KEYWORD-REJECT
   s" a body tick of >r is told it is not a word" T-LABEL
   s" >r" s" : X ( -- ) ['] >r drop ;" 16 KEYWORD-REJECT ;

\ Under a rendering statement's mark CHECK leaves an undefined name to the run
\ (VERDICT-DEFERRED), which the engine never compiles: the definition rejects it.
: TEST-DEFERRED ( -- )
   s" a definition left to the run prints one E-UNDEFINED" T-LABEL
   BODY$ S\" package LRD-UNSEEN\ns\q \q evaluate\n: LRD-DEFER ( -- ) LRD-MADE ;\n;package\n"
   FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `LRD-MADE`\n --> " SB-APPEND
   BODY$ SB-APPEND
   S\" :3:20\n  |\n3 | : LRD-DEFER ( -- ) LRD-MADE ;\n" SB-APPEND
   S\"   |                    ^^^^^^^^ not found\n" SB-APPEND
   s"   = help: define `LRD-MADE` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   70 ASSERT-REJECTED ;

: TEST-SHADOW ( -- )
   s" a bare word a global and a used package share is rejected" T-LABEL
   SHADOW$ RUN
   SB-RESET
   s" error[E-USING-SHADOW-GLOBAL]: `LRD-SHADOW` is both a global word" SB-APPEND
   S\"  and a public of used package `LRD-SH`\n --> " SB-APPEND  SHADOW$ SB-APPEND
   S\" :7:1\n  |\n7 | LRD-SHADOW\n  | ^^^^^^^^^^ ambiguous here\n" SB-APPEND
   S\"   = global: LRD-SHADOW\n  = package: LRD-SH:LRD-SHADOW\n" SB-APPEND
   s"   = help: write `LRD-SH:LRD-SHADOW` to call the package's word," SB-APPEND
   S\"  or rename one of the two: the global word has no qualified form\n\n"
   SB-APPEND
   70 ASSERT-REJECTED ;

\ A path no file name holds is rejected by the loader word, labelled with why.
: TEST-EMPTY-PATH ( -- )
   s" an empty path is rejected at its loader word" T-LABEL
   BODY$ s\" s\" \" included" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" error[E-PATH-RANGE]: `included` got an empty path\n --> " SB-APPEND
   BODY$ SB-APPEND  S\" :1:6\n  |\n1 | s\" \" included\n" SB-APPEND
   S\"   |      ^^^^^^^^ passes a path that is empty\n  = help: pass a path of 1 to " SB-APPEND
   PATH-CAP FMT:SB-INT  S\"  bytes\n\n" SB-APPEND
   70 ASSERT-REJECTED
   s" an empty path is rejected in one JSON line under REJECTION:JSON" T-LABEL
   BODY$ S\" -1 REJECTION:JSON !\ns\" \" included" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET
   S\" {\"severity\":\"error\",\"code\":\"E-PATH-RANGE\"" SB-APPEND
   S\" ,\"message\":\"`included` got an empty path\",\"file\":\"" SB-APPEND
   BODY$ SB-APPEND
   S\" \",\"line\":2,\"column\":6,\"byte_start\":25,\"byte_end\":33" SB-APPEND
   S\" ,\"token\":\"included\",\"source_line\":\"s\\\" \\\" included\"" SB-APPEND
   S\" ,\"label\":\"passes a path that is empty\",\"help\":\"pass a path of 1 to " SB-APPEND
   PATH-CAP FMT:SB-INT  S\"  bytes\"}\n" SB-APPEND
   70 ASSERT-REJECTED ;

\ SRC as the body exits 67, its stderr opening with the line
\ SB holds and then its help: the record has no place.
: PATH-PASS-NOWHERE ( str -- ) {: src:^ srcu:n :}
   S\" \n  = help: " SB-APPEND
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   ERR$ SB$ STARTS-WITH? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ 67 T= ;

\ The record on stderr helps with a path of 1 to INCLUDE-PATH-CAP bytes.
: HELP-MOST ( -- )
   SB-RESET  s" = help: pass a path of 1 to " SB-APPEND
   INCLUDE-PATH-CAP FMT:SB-INT  S\"  bytes\n" SB-APPEND
   ERR$ SB$ CONTAINS? TTRUE ;

4220 constant LONG-QS
create LONG LONG-QS 64 + allot
variable LONG-U

: LONG+ ( str -- )
   {: a:^ u:n :}
   a LONG LONG-U @ + u BYTE-COPY  u LONG-U +! ;

\ A missing path of 4223 bytes, which CANONICAL normalizes past the cap.
: LONG$ ( -- ^u8 n )
   0 LONG-U !  S\" s\" /x/" LONG+
   LONG-QS 0 ?do [char] q LONG LONG-U @ + c!  1 LONG-U +! loop
   S\" \" SOURCE-ROOT:CANONICAL drop 2drop" LONG+
   LONG LONG-U @ ;

: TEST-PATH-PASS ( -- )
   s" CANON-OS's uncaught throw on an empty path prints its record" T-LABEL
   SB-RESET
   s" error[E-PATH-RANGE]: a path is empty" SB-APPEND
   S\" s\" \" SOURCE-ROOT:CANON-OS drop 2drop" PATH-PASS-NOWHERE
   HELP-MOST
   s" JOIN's uncaught throw on a negative root length prints its record" T-LABEL
   SB-RESET
   s" error[E-PATH-RANGE]: a path's length is -1, below 0" SB-APPEND
   S\" s\" r\" drop -1 s\" x\" SOURCE-ROOT:JOIN 2drop" PATH-PASS-NOWHERE
   HELP-MOST
   s" CANONICAL's uncaught throw on a long path prints its record" T-LABEL
   SB-RESET
   s" error[E-PATH-RANGE]: a path resolves to more than the " SB-APPEND
   PATH-CAP FMT:SB-INT  s"  bytes a path may have" SB-APPEND
   LONG$ PATH-PASS-NOWHERE ;

: TEST-IMM-INCLUDE ( -- )
   s" checked include rejects before executing its fragment" T-LABEL
   INC$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 43 s" include"
   S\" move `include` and its path out of the definition to top level, or load the file when the word runs with `s\" path\" included`"
   ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-REQUIRE ( -- )
   s" checked require rejects before executing its fragment" T-LABEL
   REQ$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 43 s" require"
   S\" move `require` and its path out of the definition to top level, or load the file when the word runs with `s\" path\" required`"
   ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-CHECK ( -- )
   s" check.f loads nothing for a body's include" T-LABEL
   MISS$ CHECK-RUN
   EXITED @ TTRUE
   RC @ 70 T=
   ERR$ s" E-UNMODELED-IMMEDIATE" COUNT-SUB 1 T=
   ERR$ s" E-MISSING-SOURCE" CONTAINS? TFALSE ;

: TEST-IMM-CALLER ( -- )
   s" an unmodeled immediate gets one canonical preflight diagnostic" T-LABEL
   CALLER$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   3 34 s" RAW-IMM"
   s" state the count with `parses: RAW-IMM n` after `RAW-IMM`"
   ASSERT-IMM-DIAG
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   \ Bytes count from the file's start, not the definition's: RAW-IMM is on line 3.
   root s" byte_start" 99 GJA-ASSERT-INT-FIELD
   root s" byte_end" 106 GJA-ASSERT-INT-FIELD ;

: TEST-IMM-REBOUND ( -- )
   s" an unmodeled immediate's help follows the word it binds, not its spelling" T-LABEL
   REBOUND$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   4 20 s" include"
   s" state the count with `parses: include n` after `include`"
   ASSERT-IMM-DIAG ;

: TEST-IMM-TRUSTED ( -- )
   s" trusted body cannot bypass unmodeled immediate refusal" T-LABEL
   TRUSTED$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 106 s" LRD-TRUST-IMM"
   s" state the count with `parses: LRD-TRUST-IMM n` after `LRD-TRUST-IMM`"
   ASSERT-IMM-DIAG ;

\ A rejection at a token a definer evaluated, which no source holds, is one record at
\ the definition's `;`, where the check runs, with the token in a `word` note.
: TEST-UNSOURCED-RECORD ( -- )
   s" a rejection at an evaluated token is placed where the check runs" T-LABEL
   BODY$ S\" true REJECTION:JSON !\n: OPEN2 ( ^u8 n -- ) EFFECT:EMPTY define-word s\" drop\" evaluate-closed ;\ns\" Q\" OPEN2 ( a comment ) ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" E-INPUT-UNDERFLOW" ASSERT-NAMED
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root s" token" GJA-REQ s" ;" GJA-ASSERT-STR
   root s" line" 3 GJA-ASSERT-INT-FIELD
   root s" column" 27 GJA-ASSERT-INT-FIELD
   root s" notes" GJA-REQ s" word" GJA-REQ s" drop" GJA-ASSERT-STR
   root GJA-RECORD-FORM ;

\ T0 refuses a body no scan certified where the checker's scan bound nothing
\ or `is` names no deferred word.
: TEST-UNCERTIFIED-MALFORMED ( -- )
   s" a trusted quotation reading its definition's local is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-QLOCAL ( n -- n ) {: a:n :} [: a ;] execute ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: cannot find word `a`" s" :1:45" BODY-REJECT-AT
   s" a trusted quotation reading its definition's numeric local is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-QNUM ( n -- n ) {: 1:n :} [: 1 ;] execute ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNDEFINED]: cannot find word `1`" s" :1:43" BODY-REJECT-AT
   s" a trusted is with no target is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-NO-TARGET ( -- ) [: ;] is ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-DEFER]: `is` names no deferred word" s" :1:37" BODY-REJECT-AT
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" a trusted is naming no word is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-NO-DEFER ( -- ) [: ;] is LRD-NO-SUCH-DEFER ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-DEFER]: cannot find deferred word `LRD-NO-SUCH-DEFER`"
      s" :1:39" BODY-REJECT-AT
   s" a trusted is naming a colon word is refused" T-LABEL
   BODY$ s" : LRD-PLAIN ( -- ) ; TRUSTED: LRD-IS-PLAIN ( -- ) [: ;] is LRD-PLAIN ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-DEFER]: `LRD-PLAIN` is not a deferred word" s" :1:60"
      BODY-REJECT-AT ;

: TEST-PREFLIGHT-POSITIVES ( -- )
   s" top-level include remains live" T-LABEL
   TOP-INC$ RUN s" 74" ASSERT-OK-OUT
   s" top-level require remains live" T-LABEL
   TOP-REQ$ RUN s" 74" ASSERT-OK-OUT
   s" runtime included required and provided remain live" T-LABEL
   RUNTIME$ RUN s" 74" ASSERT-OK-OUT
   s" an immediate that reads and writes nothing remains live" T-LABEL
   MODELED$ RUN s" 73" ASSERT-OK-OUT ;

\ A rollback with a task live retires the records it drops and keeps the
\ cursors, so the rejection it recovers from is all the load prints.
: TEST-LIVE-ROLLBACK ( -- )
   s" a rejected declaration with a task live prints only its rejection" T-LABEL
   SB-RESET
   S\" require src/lib/task.f\nrequire src/lib/test/wait.f\n" SB-APPEND
   S\" TASK:MIN-STACK TASK:TASK LRD-WORKER\nvariable LRD-READY\n" SB-APPEND
   S\" : LRD-SPIN ( -- ) 1 LRD-READY atomic! begin TASK:PAUSE again ;\n" SB-APPEND
   s" : LRD-GO ( -- ) ['] LRD-SPIN LRD-WORKER TASK:ACTIVATE" SB-APPEND
   S\"   LRD-READY 1 LRD-WORKER TEST-WAIT:FOR-CELL ;\nLRD-GO\n" SB-APPEND
   S\" ENUM lrd-col green red green ;ENUM\n" SB-APPEND
   BODY$ SB$ FS:WRITE-ALL
   BODY$ RUN
   s" E-BAD-DECLARATION" ASSERT-NAMED
   ERR$ s" E-TASK-COMPILE" CONTAINS? TFALSE ;

: MAIN ( -- )
   T-RESET
   SETUP
   TEST-STORE-SIGNALED
   TEST-UNDEF
   TEST-BODY
   TEST-PARAMETRIC
   TEST-BYTE-PTR-STORE
   TEST-BYTE-PTR-FETCH
   TEST-CONTROL-FLOW
   TEST-STORAGE
   TEST-SCRATCH-STORAGE
   TEST-MISSING-SIGNATURE
   TEST-CAST
   TEST-UNSAFE
   TEST-KEYWORD
   TEST-KEYWORD-CHAR
   TEST-KEYWORD-TICK
   TEST-LOCALS-END
   TEST-KEYWORD-CONSTANT
   TEST-SEALED-PACKAGE
   TEST-SEALED-DEFINE
   TEST-SEALED-DUPLICATE
   TEST-EVALUATED-PLACE
   TEST-EVALUATED-SLICE
   TEST-OUTER-FILE-PLACE
   TEST-REPLAY-DUP-PLACE
   TEST-REPLAY-TASK
   TEST-WINDOW-NESTED
   TEST-COMPOSE-NESTED
   TEST-OPEN-AT-CLOSE
   TEST-LOAD-IN-DECLARATION
   TEST-BAD-SIGNATURE-LINE
   TEST-REQUIRE-CHAIN
   TEST-RESIDUE
   TEST-TARGET
   TEST-DEF-ONCE
   TEST-CAPTURE-RELEASE
   TEST-CAPTURE-SEALED
   TEST-RECURSE-HELP
   TEST-UNDEF-HOLDER
   TEST-BODY-ONLY
   TEST-TOP-LOCALS-END
   TEST-BODY-OPERAND
   TEST-KEYWORD-OPERAND
   TEST-DEFERRED
   TEST-SHADOW
   TEST-EMPTY-PATH
   TEST-PATH-PASS
   TEST-IMM-INCLUDE
   TEST-IMM-REQUIRE
   TEST-IMM-CHECK
   TEST-IMM-CALLER
   TEST-IMM-REBOUND
   TEST-IMM-TRUSTED
   TEST-UNSOURCED-RECORD
   TEST-BIG-LITERAL
   TEST-SECTION-CLASH
   HB-TARGET-LINUX-X86-64? 0= if
      TEST-UNCERTIFIED-MALFORMED
      TEST-T0-UNBOUND
      TEST-T0-RAW-SPLIT
      TEST-T0-RAW-STORE
   then
   TEST-PREFLIGHT-POSITIVES
   TEST-LIVE-ROLLBACK
   FS:CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

MAIN

;package
