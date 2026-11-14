\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself: a
\ checked load refusal exits 70 with a named diagnostic on stderr; an
\ unmodeled immediate in a TRUSTED: body is refused by the checker's preflight
\ as it is read, before any compiler sees it. Covers the direct --load,
\ require-chain and checked-body legs on generated fixtures; the child engine is
\ HABU_UNDER_TEST when set, else the running engine.
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require tools/gate-json-assert-core.f
require src/lib/argv.f
require tools/repair-packet-core.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package LOAD-REJECT-TEST

$1000 constant CAP
70 constant REJECT-RC
67 constant UNCAUGHT-RC
76 constant BAD-SIG-RC
84 constant SEALED-RC
78 constant DUP-RC
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

variable BASE-U
variable UNDEF-U
variable BODY-U
variable OUTER-U
variable FRAG-U
variable SIDE-U
variable INC-U
variable REQ-U
variable CALLER-U
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
variable OUT-U
variable ERR-U
variable EXITED
variable RC

: BASE$ ( -- ptr u8 n )
   BASE BASE-U @ ;

: UNDEF$ ( -- ptr u8 n )
   UNDEF-P UNDEF-U @ ;

: BODY$ ( -- ptr u8 n )
   BODY-P BODY-U @ ;

: OUTER$ ( -- ptr u8 n )
   OUTER-P OUTER-U @ ;

: FRAG$ ( -- ptr u8 n )
   FRAG-P FRAG-U @ ;

: SIDE$ ( -- ptr u8 n )
   SIDE-P SIDE-U @ ;

: INC$ ( -- ptr u8 n )
   INC-P INC-U @ ;

: REQ$ ( -- ptr u8 n )
   REQ-P REQ-U @ ;

: CALLER$ ( -- ptr u8 n )
   CALLER-P CALLER-U @ ;

: TOP-INC$ ( -- ptr u8 n )
   TOP-INC-P TOP-INC-U @ ;

: TOP-REQ$ ( -- ptr u8 n )
   TOP-REQ-P TOP-REQ-U @ ;

: RUNTIME$ ( -- ptr u8 n )
   RUNTIME-P RUNTIME-U @ ;

: MODELED$ ( -- ptr u8 n )
   MODELED-P MODELED-U @ ;

: TRUSTED$ ( -- ptr u8 n )
   TRUSTED-P TRUSTED-U @ ;

: BPS$ ( -- ptr u8 n )
   BPS-P BPS-U @ ;

: BPF$ ( -- ptr u8 n )
   BPF-P BPF-U @ ;

: RES$ ( -- ptr u8 n )
   RES-P RES-U @ ;

: RTYPE$ ( -- ptr u8 n )
   RTYPE-P RTYPE-U @ ;

: RNEST$ ( -- ptr u8 n )
   RNEST-P RNEST-U @ ;

: RCATCH$ ( -- ptr u8 n )
   RCATCH-P RCATCH-U @ ;

: CF$ ( -- ptr u8 n )
   CF-P CF-U @ ;

: FRAG-FIXTURE! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n frag:ptr fragu:n post:ptr postu:n path:ptr pathu:n :}
   SB-RESET
   pre preu SB-APPEND
   frag fragu SB-APPEND
   post postu SB-APPEND
   path pathu SB$ FS:WRITE-ALL ;

: PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: name:ptr nameu:n dst:ptr lenp:ptr :}
   BASE$ name nameu dst FS:JOIN-PATH lenp ! ;

: ROOT! ( -- )
   FS:CLEANUP-RESET
   s" habu-load-reject-diag" FS:MKTEMP-DIR {: a:ptr u:n :}
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
   s" residue-caught.f" RCATCH-P RCATCH-U PATH! ;

: REJECT-FIXTURES! ( -- )
   UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" FS:WRITE-ALL
   BODY$ s" : LRD-Y ( n -- n ) drop ;" FS:WRITE-ALL
   \ The byte pointer comes from a declared cell: a raw `variable` hands out no
   \ address (E-RAW-CELL-PTR), which would refuse the fixture one definition
   \ before the token these cases pin.
   BPS$ s" TYPED-VARIABLE LRD-P ptr u8 : LRD-BASE ( -- ptr u8 ) LRD-P @ ; : LRD-BPS ( -- ) 0 LRD-BASE ! ;" FS:WRITE-ALL
   BPF$ s" TYPED-VARIABLE LRD-Q ptr u8 : LRD-QBASE ( -- ptr u8 ) LRD-Q @ ; : LRD-BPF ( -- n ) LRD-QBASE @ ;" FS:WRITE-ALL
   \ A loaded file is a closed program, so the fragment shows it ran by
   \ printing: it can take no cell from its loader and leave none.
   FRAG$ s" 74 ." FS:WRITE-ALL
   SIDE$ S\" [ s\" LRD-FRAGMENT-EXECUTED\" type ]" FS:WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   UNDEF$ SB-APPEND
   OUTER$ SB$ FS:WRITE-ALL ;

: IMM-FIXTURES! ( -- )
   s" -1 JSON-DIAGS ! : LRD-IMM-I ( -- ) 73 include "
   SIDE$ s"  . ;" INC$ FRAG-FIXTURE!
   s" -1 JSON-DIAGS ! : LRD-IMM-R ( -- ) 73 require "
   SIDE$ s"  . ;" REQ$ FRAG-FIXTURE!
   CALLER$ S" TRUSTED: RAW-IMM ( -- ) ; immediate
-1 JSON-DIAGS !
: LRD-IMM-S ( mystery:foo -- n ) RAW-IMM 73 ;
" FS:WRITE-ALL ;

\ One definition per append: SB holds one path and its text, not three.
: RUNTIME-DEF+ ( ptr u8 n ptr u8 n -- ) {: pre:ptr preu:n post:ptr postu:n :}
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
   RTYPE$ s" NEWTYPE lrd-length 0" FS:WRITE-ALL
   s" require " RES$ s" " RNEST$ FRAG-FIXTURE!
   S\" : LRD-T ( -- ) [: s\" " RES$ S\" \" required ;] catch . ; LRD-T"
      RCATCH$ FRAG-FIXTURE! ;

: POSITIVE-FIXTURES! ( -- )
   s" include " FRAG$ s" " TOP-INC$ FRAG-FIXTURE!
   s" require " FRAG$ s" " TOP-REQ$ FRAG-FIXTURE!
   RUNTIME-FIXTURE!
   MODELED$ S\" : PI ( -- ) ; immediate s\" PI\" 0 parse-imm : PIM ( -- n ) PI 73 ; PIM ." FS:WRITE-ALL
   TRUSTED$ S\" -1 JSON-DIAGS ! : LRD-TRUST-IMM ( -- ) ; immediate TRUSTED: LRD-TRUST-BODY ( -- n ) LRD-TRUST-IMM 73 ;" FS:WRITE-ALL ;

: FIXTURES! ( -- )
   REJECT-FIXTURES!
   IMM-FIXTURES!
   RESIDUE-FIXTURES!
   POSITIVE-FIXTURES! ;

: SETUP ( -- )
   ROOT!
   PATHS!
   FIXTURES! ;

: STORE! ( len len PROCESS:ending ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT OUT-U @ ERR ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   path pathu STORE! ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

\ Every rejecting load: NON-EMPTY stderr that names the failing token, EMPTY
\ stdout, exit-kind EXIT, rc 70 — the silent-exit-70 red/green discriminator.
: ASSERT-NAMED ( ptr u8 n -- ) {: name:ptr nameu:n :}
   ERR-U @ 0 > TTRUE
   ERR$ name nameu CONTAINS? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

: ASSERT-OK ( -- )
   EXITED @ TTRUE
   RC @ 0 T=
   ERR-U @ 0 T= ;

: ASSERT-OK-OUT ( ptr u8 n -- )
   {: a:ptr u:n :}
   ASSERT-OK
   OUT$ a u CONTAINS? TTRUE ;

: ASSERT-IMM-DIAG ( ptr u8 n ptr u8 n -- )
   {: word:ptr wordu:n token:ptr tokenu:n :}
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root GJA-DIAG-CONTRACT-ROW
   root s" code" GJA-REQ s" E-UNMODELED-IMMEDIATE" GJA-ASSERT-STR
   root s" repair_class" GJA-REQ s" model_compile_immediate" GJA-ASSERT-STR
   root s" word" GJA-REQ word wordu GJA-ASSERT-STR
   root s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   ERR$ 2dup RP-COUNT dup 1 T= >r RP-FIRST r> RP-PACKET
   {: pa:ptr pu:n :}
   pa OUT pu BYTE-COPY
   OUT pu GJA-PARSE dup GJA-OBJ {: packet:SPAN:span<u8> :}
   packet s" model_compile_immediate" GJA-REPAIR-HEAD
   packet GJA-REPAIR-DEF ;

: TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN PROCESS:SIGKILL PROCESS-ENDING:signaled s" " STORE!
   EXITED @ TFALSE
   RC @ PROCESS:SIGKILL T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

\ The checker's refusal carries no file:line; the definition's name,
\ lower-cased as the checker renders it, says where it was refused.
: ASSERT-UNDEF ( -- )
   s" E-UNDEFINED" ASSERT-NAMED
   ERR$ s" undefined word 'LRD-NO-SUCH-WORD-XYZ'" CONTAINS? TTRUE
   ERR$ s" in lrd-x" CONTAINS? TTRUE ;

: TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word and its line" T-LABEL
   UNDEF$ RUN
   ASSERT-UNDEF
   SB-RESET  s" E-UNDEFINED habu: " SB-APPEND  UNDEF$ SB-APPEND
   s" :1: undefined word 'LRD-NO-SUCH-WORD-XYZ'" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE ;

: TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   BODY$ RUN
   s" in lrd-y" ASSERT-NAMED
   ERR$ s" at 'drop'" CONTAINS? TTRUE ;

\ Store/fetch target mismatch: `!`/`@` need
\ a cell-width `ptr a`; a `ptr u8` must name the pair, not just the token.
: TEST-BYTE-PTR-STORE ( -- )
   s" cell store into a byte pointer names expected/actual" T-LABEL
   BPS$ RUN
   s" in lrd-bps" ASSERT-NAMED
   ERR$ s" at '!'" CONTAINS? TTRUE
   ERR$ s" expected: ptr a" CONTAINS? TTRUE
   ERR$ s" actual: ptr u8" CONTAINS? TTRUE ;

\ Each control-flow case is a load of its own: a load stops at its first refusal.
: CF-RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   CF$ src srcu FS:WRITE-ALL
   CF$ RUN ;

\ A control-flow refusal names its reason after the token; a depth change names
\ the start row, or the else arm's, as expected and the changed row as actual.
: TEST-CONTROL-FLOW ( -- )
   s" a loop body that changes depth names both rows" T-LABEL
   s" : LRD-LD ( n -- n ) 3 0 do dup loop ;" CF-RUN
   s" in lrd-ld" ASSERT-NAMED
   ERR$ s" at 'loop' the loop body changes the stack's depth expected: n actual: n n" CONTAINS? TTRUE
   s" an i outside every loop says no loop is open" T-LABEL
   s" : LRD-IO ( -- ) i drop ;" CF-RUN
   s" in lrd-io" ASSERT-NAMED
   ERR$ s" at 'i' no loop is open here" CONTAINS? TTRUE
   s" a j in a single loop says no outer loop is open" T-LABEL
   s" : LRD-JO ( -- ) 3 0 do j drop loop ;" CF-RUN
   s" in lrd-jo" ASSERT-NAMED
   ERR$ s" at 'j' no outer loop is open here" CONTAINS? TTRUE
   s" a then with no if says it closes nothing" T-LABEL
   s" : LRD-TS ( -- ) then ;" CF-RUN
   s" in lrd-ts" ASSERT-NAMED
   ERR$ s" at 'then' this word closes nothing open here" CONTAINS? TTRUE
   s" an if arm that changes depth names both rows" T-LABEL
   s" : LRD-IA ( n bool -- n ) if 1 then ;" CF-RUN
   s" in lrd-ia" ASSERT-NAMED
   ERR$ s" at 'then' the if arm changes the stack's depth expected: n actual: n n" CONTAINS? TTRUE
   s" if and else arms of different depths name both rows" T-LABEL
   s" : LRD-AD ( n bool -- n ) if 1 else then ;" CF-RUN
   s" in lrd-ad" ASSERT-NAMED
   ERR$ s" at 'then' the if and else arms leave different stack depths expected: n actual: n n" CONTAINS? TTRUE
   s" an exit inside a loop says it needs unloop" T-LABEL
   s" : LRD-EL ( -- ) 3 0 do exit loop ;" CF-RUN
   s" in lrd-el" ASSERT-NAMED
   ERR$ s" at 'exit' exit inside a loop needs unloop first" CONTAINS? TTRUE
   s" a leave that changes depth names both rows" T-LABEL
   s" : LRD-LV ( n -- n ) 3 0 do 1 leave loop ;" CF-RUN
   s" in lrd-lv" ASSERT-NAMED
   ERR$ s" at 'leave' the loop body changes the stack's depth expected: n actual: n n" CONTAINS? TTRUE
   s" a loop body that changes the return stack's depth says so" T-LABEL
   s" : LRD-RD ( -- ) 3 0 do 1 >r loop ;" CF-RUN
   s" in lrd-rd" ASSERT-NAMED
   ERR$ s" at 'loop' the loop body changes the return stack's depth" CONTAINS? TTRUE
   s" a second else says the if already has one" T-LABEL
   s" : LRD-E2 ( bool -- ) if else else then ;" CF-RUN
   s" in lrd-e2" ASSERT-NAMED
   ERR$ s" at 'else' this if already has an else" CONTAINS? TTRUE
   s" an if open at ; says a structure is still open" T-LABEL
   s" : LRD-OP ( bool -- ) if ;" CF-RUN
   s" in lrd-op" ASSERT-NAMED
   ERR$ s" a control structure is still open at the end of the definition" CONTAINS? TTRUE
   s" a +loop that meets an if inside its do says it closes nothing" T-LABEL
   s" : LRD-PI ( -- ) 3 0 do 0 0= if 1 +loop then ;" CF-RUN
   s" in lrd-pi" ASSERT-NAMED
   ERR$ s" at '+loop' this word closes nothing open here" CONTAINS? TTRUE ;

: TEST-BYTE-PTR-FETCH ( -- )
   s" cell fetch from a byte pointer names expected/actual" T-LABEL
   BPF$ RUN
   s" in lrd-bpf" ASSERT-NAMED
   ERR$ s" at '@'" CONTAINS? TTRUE
   ERR$ s" expected: ptr a" CONTAINS? TTRUE
   ERR$ s" actual: ptr u8" CONTAINS? TTRUE ;

: PARAMETRIC-REJECT ( ptr u8 n -- )
   BODY$ 2swap FS:WRITE-ALL
   BODY$ RUN
   s" E-NONPARAMETRIC-EFFECT" ASSERT-NAMED ;

: TEST-PARAMETRIC ( -- )
   s" generic output cannot forge a pointer through an integer" T-LABEL
   s" : LRD-MINT ( -- a ) 123 ; : LRD-PTR ( -- ptr u8 ) LRD-MINT ;" PARAMETRIC-REJECT
   s" numeric fetch requires a numeric pointee" T-LABEL
   s" : LRD-FETCH ( ptr a -- n ) @ ;" PARAMETRIC-REJECT
   s" quotation output cannot specialize a declared variable" T-LABEL
   s" : LRD-QUOT ( [ -- a ] -- n ) execute ;" PARAMETRIC-REJECT
   s" family argument cannot specialize a declared variable" T-LABEL
   s" NEWTYPE lrd-box<a> CAST: LRD-CONCRETE ( n -- lrd-box<n> ) : LRD-PARAM ( n -- lrd-box<a> ) LRD-CONCRETE ;" PARAMETRIC-REJECT
   s" raw storage cannot restrict a declared generic input" T-LABEL
   s" create LRD-RAW 1 cells allot : LRD-RAW-PUT ( a -- ) LRD-RAW ! ;" PARAMETRIC-REJECT
   ERR$ s" restricted by raw storage" CONTAINS? TTRUE
   s" raw storage cannot restrict a quotation output variable" T-LABEL
   s" create LRD-RAW 1 cells allot : LRD-RAW-QUOT ( [ -- a ] -- ) execute LRD-RAW ! ;" PARAMETRIC-REJECT
   s" concrete storage and generic transport retain their declared effects" T-LABEL
   BODY$ s" create LRD-RAW 1 cells allot : LRD-RAW-N ( n -- ) LRD-RAW ! ; : LRD-ID ( a -- a ) ; : LRD-ROUNDTRIP ( n -- n ) LRD-ID dup LRD-RAW-N ; 41 LRD-ROUNDTRIP . cr" FS:WRITE-ALL
   BODY$ RUN s" 41" ASSERT-OK-OUT
   s" family arguments keep distinct declared variables" T-LABEL
   s" NEWTYPE lrd-pair<a,b> : LRD-ID ( lrd-pair<a,a> -- lrd-pair<a,a> ) ; : LRD-ALIAS ( lrd-pair<a,b> -- lrd-pair<a,b> ) LRD-ID ;" PARAMETRIC-REJECT ;

\ An integer literal the engine cannot read is undefined, named where the body
\ holds it, whichever loop reads the body.
: TEST-BIG-LITERAL ( -- )
   s" an integer literal the engine cannot read is undefined" T-LABEL
   BODY$ s" TRUSTED: LRD-BIG ( -- n ) 99999999999999999999999 ;" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-UNDEFINED habu: " SB-APPEND  BODY$ SB-APPEND
   s" :1: undefined word '99999999999999999999999'" SB-APPEND
   SB$ ASSERT-NAMED ;

\ A top-level `target` beside the program's own TARGET could mean either, and is refused by name.
: TEST-SECTION-CLASH ( -- )
   s" a section word beside a word of its name is refused" T-LABEL
   BODY$ S\" package LRD-CLASH\n: TARGET ( -- ) ;\ntarget\n;package\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-SECTION-WORD habu: " SB-APPEND  BODY$ SB-APPEND
   s" :3: 'target' is the section word and a word of that name is in scope" SB-APPEND
   SB$ ASSERT-NAMED ;

\ T0, arm64's tier 0, refuses a body no scan certified at its first token the
\ scan could not bind or no path reaches.
: TEST-T0-UNBOUND ( -- )
   s" T0 refuses a MATCH variant with no OF" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-NOOF ( n -- n ) construct lrd-choice scalar MATCH lrd-choice scalar 10 + ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-NOOF at 10 in" ASSERT-NAMED
   s" T0 refuses a MATCH with no value proved under it" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-UNDER ( -- n ) MATCH lrd-choice empty OF 0 ENDOF scalar OF ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-UNDER at lrd-choice in" ASSERT-NAMED
   s" T0 refuses a MATCH over an unknown family" T-LABEL
   BODY$ s" TRUSTED: LRD-NOFAM ( n -- n ) 5 swap MATCH lrd-nofam empty OF 1 ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-NOFAM at lrd-nofam in" ASSERT-NAMED
   s" T0 refuses a MATCH over an unknown variant at the variant" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-NOVAR ( lrd-choice -- n ) MATCH lrd-choice bogus OF 1 ENDOF scalar OF ENDOF ;MATCH ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-NOVAR at bogus in" ASSERT-NAMED
   s" T0 refuses dead code after exit" T-LABEL
   BODY$ s" TRUSTED: LRD-DEAD ( -- n ) 1 exit DUP ; LRD-DEAD . cr" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-DEAD at DUP in" ASSERT-NAMED
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead string literal at its opener" T-LABEL
   BODY$ S\" TRUSTED: LRD-DEADS ( -- n ) 1 exit s\" ab\" drop ;" FS:WRITE-ALL
   BODY$ RUN
   S\" compiler: cannot compile LRD-DEADS at s\" in" ASSERT-NAMED
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead quotation inside a quotation at its opener" T-LABEL
   BODY$ s" TRUSTED: LRD-DEADQ ( -- [ -- n ] ) [: 1 exit [: 2 ;] ;] ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-DEADQ at [: in" ASSERT-NAMED
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a dead string holding ;] inside a quotation at its opener" T-LABEL
   BODY$ S\" TRUSTED: LRD-DEADQS ( -- [ -- n ] ) [: 1 exit s\" a ;] b\" drop ;] ;" FS:WRITE-ALL
   BODY$ RUN
   S\" compiler: cannot compile LRD-DEADQS at s\" in" ASSERT-NAMED
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" T0 refuses a construct of an unknown variant" T-LABEL
   BODY$ s" SUMTYPE lrd-choice VARIANT empty ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE TRUSTED: LRD-BADVAR ( -- n ) construct lrd-choice bogus 1 ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-BADVAR at bogus in" ASSERT-NAMED
   s" T0 refuses a construct of an unknown family" T-LABEL
   BODY$ s" TRUSTED: LRD-BADFAM ( -- n ) construct lrd-nofam scalar 1 ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-BADFAM at lrd-nofam in" ASSERT-NAMED ;

\ A refused step in a TRUSTED: body lowers as the raw effect the scan splices,
\ so `2dup` over a two-cell view copies two cells.
: TEST-T0-RAW-SPLIT ( -- )
   s" T0 compiles a refused TRUSTED: step as its raw effect" T-LABEL
   BODY$ s" require src/lib/c2-owner.f require src/lib/c2-bytes.f TRUSTED: LRD-SPLIT ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> ptr u8 n ) 2dup ; variable LRD-LEN variable LRD-LEN2 : LRD-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) LRD-SPLIT LRD-LEN ! drop C2-BYTES:MUT-LENGTH swap LRD-LEN2 ! ; : LRD-RUN ( -- ) C2-MEM:OWNER-SIZE [: LRD-ROOT ;] C2-MEM:WITH-MUT ; LRD-RUN LRD-LEN @ . LRD-LEN2 @ . cr" FS:WRITE-ALL
   BODY$ RUN S\" 8\n8\n" ASSERT-OK-OUT ;

\ A refused `!` of a two-cell product in a TRUSTED: body splices its own typed
\ rows and keeps their width fact, so it stores both cells.
: TEST-T0-RAW-STORE ( -- )
   s" T0 compiles a refused TRUSTED: store at its field width" T-LABEL
   BODY$ s" PRODUCT lrd-pt FIELD x n FIELD y n ;PRODUCT TYPED-VARIABLE LRD-PV lrd-pt TRUSTED: LRD-SET ( n n ptr lrd-pt -- ) ! ; : LRD-STORE ( -- n ) 7 3 4 LRD-PV LRD-SET ; LRD-STORE . depth . cr" FS:WRITE-ALL
   BODY$ RUN S\" 7\n0\n" ASSERT-OK-OUT ;

\ A storage declaration whose definer cannot size its type is the checker's
\ refusal, named for the declared word and the type.
: STORAGE-REJECT ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   name nameu ASSERT-NAMED
   ERR$ s" : unknown type 'lrd-stg-none'" CONTAINS? TTRUE ;

: TEST-STORAGE ( -- )
   s" undefined storage types are named refusals" T-LABEL
   s" 4 TYPED-BUFFER LRD-STG-TB lrd-stg-none" s" in LRD-STG-TB" STORAGE-REJECT
   s" TYPED-VARIABLE LRD-STG-TV lrd-stg-none" s" in LRD-STG-TV" STORAGE-REJECT
   s" 4 LAYOUT-BUFFER LRD-STG-LB lrd-stg-none" s" in LRD-STG-LB" STORAGE-REJECT
   s" DYNAMIC-BUFFER LRD-STG-DB lrd-stg-none" s" in LRD-STG-DB" STORAGE-REJECT
   s" DEFER-LAYOUT-BUFFER LRD-STG-DL lrd-stg-none" s" in LRD-STG-DL" STORAGE-REJECT ;

\ SRC as the body exits RC, its stderr holding the line
\ `<class> habu: <body>:<line>: <prose>`.
: CODE-REJECT-AT ( ptr u8 n ptr u8 n ptr u8 n n n -- )
   {: src:ptr srcu:n cls:ptr clsu:n prose:ptr proseu:n ln:n rc:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  cls clsu SB-APPEND  s"  habu: " SB-APPEND  BODY$ SB-APPEND
   s" :" SB-APPEND  ln ERROR-TEXT:DIGITS$ SB-APPEND  s" : " SB-APPEND
   prose proseu SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ rc T= ;

\ CODE-REJECT-AT located on the body's first line.
: CODE-REJECT ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: src:ptr srcu:n cls:ptr clsu:n prose:ptr proseu:n rc:n :}
   src srcu cls clsu prose proseu 1 rc CODE-REJECT-AT ;

\ A definer with no signature stops at the word being defined, or at `does>`
\ for a clause.
: TEST-MISSING-SIGNATURE ( -- )
   s" a definer's missing signature names its class, token and line" T-LABEL
   s" : LRD-MK ( n -- ) create , does> @ ;"
   s" E-MISSING-SIGNATURE" s" 'does>' has no closed ( -- ) signature" BAD-SIG-RC CODE-REJECT
   s" defer LRD-D"
   s" E-MISSING-SIGNATURE" s" 'LRD-D' has no closed ( -- ) signature" BAD-SIG-RC CODE-REJECT ;

\ Reopening a package the engine sealed stops at the package's name.
: TEST-SEALED-PACKAGE ( -- )
   s" a sealed package's reopening names its class, package and line" T-LABEL
   S\" package NDICT\n;package"
   s" E-SEALED-PACKAGE" s" 'NDICT' is sealed" SEALED-RC CODE-REJECT ;

\ A definition into a package the engine sealed stops at its whole name.
: TEST-SEALED-DEFINE ( -- )
   s" a definition into a sealed package names its class, whole name and line" T-LABEL
   s" : NDICT:FOO ( -- ) ;"
   s" E-SEALED-PACKAGE" s" 'NDICT:FOO' is sealed" SEALED-RC CODE-REJECT ;

\ A name a sealed package already holds is refused as sealed, as the check
\ refuses it: the seal is asked before the duplicate.
: TEST-SEALED-DUPLICATE ( -- )
   s" a duplicate definition into a sealed package names the seal" T-LABEL
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\n;package\n: SP:A ( -- ) ;\n"
   s" E-SEALED-PACKAGE" s" 'SP:A' is sealed" 6 SEALED-RC CODE-REJECT-AT
   s" a duplicate export into a sealed package names the seal" T-LABEL
   SB-RESET
   S\" package SP2\npublic\n: A ( -- ) ;\n" SB-APPEND
   S\" : MK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" SB-APPEND
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\nEXPORT SP2:A\n" SB-APPEND
   S\" s\" ACCEPTED\" type cr\n;package\n" SB-APPEND
   SB$ s" E-SEALED-PACKAGE" s" 'SP2:A' is sealed" 10 SEALED-RC CODE-REJECT-AT ;

\ A string `s"` copies out of the file, which its line 3 `evaluate`s, is not
\ the file's bytes, so its refusal names the file alone, with no line.
: TEST-EVALUATED-PLACE ( -- )
   s" an evaluated string's refusal names its file and no line" T-LABEL
   BODY$ S\" \n\ns\" lrd-zz-undefined\" evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-UNDEFINED habu: " SB-APPEND  BODY$ SB-APPEND
   s" : undefined word 'lrd-zz-undefined'" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ T$=
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

\ A slice of the file's own bytes that the file `evaluate`s is file text, so a
\ refusal in it names the file's line of the refused token, whichever writer
\ refuses it.
: TEST-EVALUATED-SLICE ( -- )
   s" an evaluated slice of the file is refused at its line" T-LABEL
   BODY$ S\" \n\nparse-name lrd-zz-undefined evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-UNDEFINED habu: " SB-APPEND  BODY$ SB-APPEND
   s" :3: undefined word 'lrd-zz-undefined'" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ T$=
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   s" the checker places a refusal in an evaluated slice on the same line" T-LABEL
   BODY$ S\" \n\n5 parse-name c@ evaluate drop\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" habu: " SB-APPEND  BODY$ SB-APPEND  s" :3: at 'c@'" SB-APPEND
   ERR$ SB$ CONTAINS? TTRUE
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   s" the native compiler places a refusal in a slice past the evaluating line" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 27 evaluate\n: LRD-QQ ( -- )\n5 c@ drop ;\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  10 SB-APPEND-C  s" compiler: cannot compile LRD-QQ at c@ in " SB-APPEND  BODY$ SB-APPEND
   s" :5" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   s" the native compiler places a refusal at the closing ; of a slice" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 29 evaluate\nTRUSTED: LRD-SF ( -- ) 0 if\n;\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" compiler: cannot compile LRD-SF at ; in " SB-APPEND  BODY$ SB-APPEND
   s" :5" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ T$=
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   s" the checker hook places a checker-only duplicate in a slice at its line" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n\" CHECKER-DEFPRODUCT GENERATED-DECL-CTOR:REPLAY-LEGACY ;\nLRD-PREPLAY\nparse-name x drop 24 + 30 evaluate\n: LRDREP:MAKE ( n -- n ) 1 + ;\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-DUPLICATE-DEFINITION habu: " SB-APPEND  BODY$ SB-APPEND
   s" :4: duplicate definition: LRDREP:MAKE" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE
   EXITED @ TTRUE
   RC @ DUP-RC T= ;

\ body.f is refused by the native compiler, naming definition NAME, the token
\ TOK it refused and that token's LINE.
: ASSERT-NCOMP-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n tok:ptr toku:n line:ptr lineu:n :}
   BODY$ RUN
   SB-RESET  10 SB-APPEND-C  s" compiler: cannot compile " SB-APPEND  name nameu SB-APPEND
   s"  at " SB-APPEND  tok toku SB-APPEND  s"  in " SB-APPEND  BODY$ SB-APPEND
   line lineu SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

\ The native compiler places a structure the checker finds open at a body's
\ closing `;`, a plain body's or a does> clause's, on the line of that `;`.
: ASSERT-CLOSE-LINE ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n line:ptr lineu:n :}
   name nameu s" ;" line lineu ASSERT-NCOMP-LINE ;

: TEST-OPEN-AT-CLOSE ( -- )
   s" an if open at a later line's ; is refused on the line of the ;" T-LABEL
   BODY$ S\" : LRD-OI ( bool -- ) if\n\n;\n" FS:WRITE-ALL
   s" LRD-OI" s" :3" ASSERT-CLOSE-LINE
   s" a does> clause leaving an if open is refused on the line of its ;" T-LABEL
   BODY$ S\" : LRD-MKO ( n -- ) create ,\ndoes> ( -- )\n@ 0= if\n;\n" FS:WRITE-ALL
   s" LRD-MKO" s" :4" ASSERT-CLOSE-LINE ;

\ A stored signature the declaration refuses is placed on its refused token's
\ line, not the name's.
: TEST-BAD-SIGNATURE-LINE ( -- )
   s" a refused stored signature on a later line is refused on that line" T-LABEL
   BODY$ S\" TRUSTED: LRD-BS\n  ( -- nosuch:fam )\n  1 ;\n" FS:WRITE-ALL
   s" LRD-BS" s" nosuch:fam" s" :2" ASSERT-NCOMP-LINE ;

\ A name only the checker holds, a PRODUCT replay with no word behind it, is
\ refused by check-hook.f DEFINITION through src/core/checker.f
\ REFUSE-DUPLICATE, which names an evaluated string's file alone too.
: TEST-REPLAY-DUP-PLACE ( -- )
   s" an evaluated checker-only duplicate names its file and no line" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n\" CHECKER-DEFPRODUCT GENERATED-DECL-CTOR:REPLAY-LEGACY ;\nLRD-PREPLAY\ns\" : LRDREP:MAKE ( n -- n ) 1 + ;\" evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   SB-RESET  s" E-DUPLICATE-DEFINITION habu: " SB-APPEND  BODY$ SB-APPEND
   s" : duplicate definition: LRDREP:MAKE" SB-APPEND  10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ DUP-RC T= ;

\ A refused `cast:` names its code, place, word and reason.
: TEST-CAST ( -- )
   s" a cast of one cell into three names its arity" T-LABEL
   BODY$ s" cast: LRD-WIDE ( n -- ptr u8 n )" FS:WRITE-ALL
   BODY$ RUN
   s" E-CAST-ARITY habu: " ASSERT-NAMED
   ERR$ s" :1: cast: 'LRD-WIDE': the output is not one term over the stack below" CONTAINS? TTRUE ;

\ A checked body that calls a word acting on source or checker state is refused
\ E-UNSAFE, naming the word and why; the compiler's line names the file and line.
: UNSAFE-REJECT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n head:ptr headu:n line:ptr lineu:n :}
   BODY$ src srcu FS:WRITE-ALL
   BODY$ RUN
   head headu ASSERT-NAMED
   ERR$ s" ' reads source or changes the compiler's or checker's state, which no stack effect describes, so a checked body cannot use it; suggestion: Remove it from the checked body; to evaluate text there, use evaluate-closed." CONTAINS? TTRUE
   ERR$ line lineu CONTAINS? TTRUE ;

: TEST-UNSAFE ( -- )
   s" a checked body calling evaluate names E-UNSAFE and why" T-LABEL
   s" : LRD-EV ( ptr u8 n -- ) evaluate ;"
   s" E-UNSAFE habu: in lrd-ev: 'evaluate' reads source" s" body.f:1" UNSAFE-REJECT ;

: TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   OUTER$ RUN
   ASSERT-UNDEF ;

\ A file that leaves cells is refused once, by one line naming the count and
\ the innermost file, then the engine's uncaught throw. Paths are matched by
\ their file names: the temporary root may resolve through a symbolic link.
: ASSERT-RESIDUE ( ptr u8 n ptr u8 n -- ) {: line:ptr lineu:n file:ptr fileu:n :}
   EXITED @ TTRUE
   RC @ UNCAUGHT-RC T=
   OUT-U @ 0 T=
   ERR$ line lineu STARTS-WITH? TTRUE
   ERR$ s" E-EVAL-RESIDUE" COUNT-SUB 1 T=
   ERR$ file fileu CONTAINS? TTRUE
   ERR$ s" hb: uncaught throw code -3804" CONTAINS? TTRUE ;

: TEST-RESIDUE ( -- )
   s" residue names the cell count and the file" T-LABEL
   RES$ RUN
   s" E-EVAL-RESIDUE: stack depth 2 at /" S\" /residue.f\n" ASSERT-RESIDUE
   s" a declaration's leftover cell is residue too" T-LABEL
   RTYPE$ RUN
   s" E-EVAL-RESIDUE: stack depth 1 at /" S\" /residue-type.f\n" ASSERT-RESIDUE
   s" nested residue is named once, at the innermost file" T-LABEL
   RNEST$ RUN
   s" E-EVAL-RESIDUE: stack depth 2 at /" S\" /residue.f\n" ASSERT-RESIDUE
   ERR$ s" residue-nest.f" CONTAINS? TFALSE
   s" caught residue prints its line and keeps its code" T-LABEL
   RCATCH$ RUN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ S\" -3804\n" STR= TTRUE
   ERR$ s" E-EVAL-RESIDUE: stack depth 2 at /" STARTS-WITH? TTRUE
   ERR$ S\" /residue.f\n" ENDS-WITH? TTRUE
   ERR$ 10 COUNT-CHAR 1 T= ;

: TEST-IMM-INCLUDE ( -- )
   s" checked include rejects before executing its fragment" T-LABEL
   INC$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   s" lrd-imm-i" s" include" ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-REQUIRE ( -- )
   s" checked require rejects before executing its fragment" T-LABEL
   REQ$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   s" lrd-imm-r" s" require" ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-CALLER ( -- )
   s" an unmodeled immediate gets one canonical preflight diagnostic" T-LABEL
   CALLER$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   s" lrd-imm-s" s" RAW-IMM" ASSERT-IMM-DIAG
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root s" token_index" 1 GJA-ASSERT-INT-FIELD
   \ Bytes count from the file's start, not the definition's: RAW-IMM is on line 3.
   root s" byte_start" 85 GJA-ASSERT-INT-FIELD
   root s" byte_end" 92 GJA-ASSERT-INT-FIELD ;

: TEST-IMM-TRUSTED ( -- )
   s" trusted body cannot bypass unmodeled immediate refusal" T-LABEL
   TRUSTED$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   s" lrd-trust-body" s" LRD-TRUST-IMM" ASSERT-IMM-DIAG ;

\ A refusal at a token an immediate evaluated into the body, which no source
\ holds, gets a record and a repair packet that meet the contract with no place.
: TEST-UNSOURCED-PACKET ( -- )
   s" a refusal at an evaluated token gets a repair packet with no place" T-LABEL
   BODY$ S\" true DIAG-JSON!\nTRUSTED: EV ( -- ) s\" drop\" evaluate ; immediate\ns\" EV\" 0 parse-imm\n: P ( -- ) ( a comment here ) 1 drop EV ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" E-INPUT-UNDERFLOW" ASSERT-NAMED
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root GJA-DIAG-CONTRACT-ROW
   root s" token" GJA-REQ s" drop" GJA-ASSERT-STR
   root s" line" GJA-HAS? TFALSE
   ERR$ 2dup RP-COUNT dup 1 T= >r RP-FIRST r> RP-PACKET
   {: pa:ptr pu:n :}
   pa OUT pu BYTE-COPY
   OUT pu GJA-PARSE dup GJA-OBJ {: packet:SPAN:span<u8> :}
   packet s" supply_missing_input" GJA-REPAIR-HEAD
   packet GJA-REPAIR-DEF
   packet s" line" GJA-HAS? TFALSE ;

\ T0 refuses a body no scan certified where the checker's scan bound nothing
\ or `is` names no deferred word.
: TEST-UNCERTIFIED-MALFORMED ( -- )
   s" a trusted quotation reading its definition's local is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-QLOCAL ( n -- n ) {: a:n :} [: a ;] execute ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-QLOCAL at a in" ASSERT-NAMED
   s" a trusted quotation reading its definition's numeric local is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-QNUM ( n -- n ) {: 1:n :} [: 1 ;] execute ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-QNUM at 1 in" ASSERT-NAMED
   s" a trusted is with no target is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-NO-TARGET ( -- ) [: ;] is ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-NO-TARGET at is in" ASSERT-NAMED
   ERR$ s" E-UNDEFINED" CONTAINS? TFALSE
   s" a trusted is naming no word is refused" T-LABEL
   BODY$ s" TRUSTED: LRD-NO-DEFER ( -- ) [: ;] is LRD-NO-SUCH-DEFER ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-NO-DEFER at LRD-NO-SUCH-DEFER in" ASSERT-NAMED
   s" a trusted is naming a colon word is refused" T-LABEL
   BODY$ s" : LRD-PLAIN ( -- ) ; TRUSTED: LRD-IS-PLAIN ( -- ) [: ;] is LRD-PLAIN ;" FS:WRITE-ALL
   BODY$ RUN
   s" compiler: cannot compile LRD-IS-PLAIN at LRD-PLAIN in" ASSERT-NAMED ;

: TEST-PREFLIGHT-POSITIVES ( -- )
   s" top-level include remains live" T-LABEL
   TOP-INC$ RUN s" 74" ASSERT-OK-OUT
   s" top-level require remains live" T-LABEL
   TOP-REQ$ RUN s" 74" ASSERT-OK-OUT
   s" runtime included required and provided remain live" T-LABEL
   RUNTIME$ RUN s" 74" ASSERT-OK-OUT
   s" parse-imm modeled immediate remains live" T-LABEL
   MODELED$ RUN s" 73" ASSERT-OK-OUT ;

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
   TEST-MISSING-SIGNATURE
   TEST-CAST
   TEST-UNSAFE
   TEST-SEALED-PACKAGE
   TEST-SEALED-DEFINE
   TEST-SEALED-DUPLICATE
   TEST-EVALUATED-PLACE
   TEST-EVALUATED-SLICE
   TEST-REPLAY-DUP-PLACE
   TEST-OPEN-AT-CLOSE
   TEST-BAD-SIGNATURE-LINE
   TEST-REQUIRE-CHAIN
   TEST-RESIDUE
   TEST-IMM-INCLUDE
   TEST-IMM-REQUIRE
   TEST-IMM-CALLER
   TEST-IMM-TRUSTED
   TEST-UNSOURCED-PACKET
   TEST-BIG-LITERAL
   TEST-SECTION-CLASH
   HB-TARGET-LINUX-X86-64? 0= if
      TEST-UNCERTIFIED-MALFORMED
      TEST-T0-UNBOUND
      TEST-T0-RAW-SPLIT
      TEST-T0-RAW-STORE
   then
   TEST-PREFLIGHT-POSITIVES
   FS:CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

MAIN

;package
