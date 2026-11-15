\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself: a
\ checked load refusal exits 70 with a named diagnostic on stderr; an
\ unmodeled immediate in a TRUSTED: body is refused by the checker's preflight
\ as it is read, before any compiler sees it. Covers the direct --load,
\ require-chain and checked-body legs on generated fixtures; the child engine is
\ HABU_UNDER_TEST when set, else the running engine. A file `?TARGET` stops is
\ rejected in REJECTION's form: prose, or one JSON line under REJECTION:JSON.
\ A rejected definition prints its one diagnostic and nothing else.
\ A bare word a global and a used package both define is rejected in that form.
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
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package LOAD-REJECT-TEST

$1000 constant CAP
70 constant REJECT-RC
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
create TARGET-P PATH-CAP allot
create TARGET-JSON-P PATH-CAP allot
create ONCE-P PATH-CAP allot
create ONCE-UNDEF-P PATH-CAP allot
create ONCE-JSON-P PATH-CAP allot
create SHADOW-P PATH-CAP allot

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
variable TARGET-U
variable TARGET-JSON-U
variable ONCE-U
variable ONCE-UNDEF-U
variable ONCE-JSON-U
variable SHADOW-U
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

: TARGET$ ( -- ptr u8 n )
   TARGET-P TARGET-U @ ;

: TARGET-JSON$ ( -- ptr u8 n )
   TARGET-JSON-P TARGET-JSON-U @ ;

: ONCE$ ( -- ptr u8 n )
   ONCE-P ONCE-U @ ;

: ONCE-UNDEF$ ( -- ptr u8 n )
   ONCE-UNDEF-P ONCE-UNDEF-U @ ;

: ONCE-JSON$ ( -- ptr u8 n )
   ONCE-JSON-P ONCE-JSON-U @ ;

: SHADOW$ ( -- ptr u8 n )
   SHADOW-P SHADOW-U @ ;

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
   s" residue-caught.f" RCATCH-P RCATCH-U PATH!
   s" target.f" TARGET-P TARGET-U PATH!
   s" target-json.f" TARGET-JSON-P TARGET-JSON-U PATH!
   s" once.f" ONCE-P ONCE-U PATH!
   s" once-undef.f" ONCE-UNDEF-P ONCE-UNDEF-U PATH!
   s" once-json.f" ONCE-JSON-P ONCE-JSON-U PATH!
   s" shadow.f" SHADOW-P SHADOW-U PATH! ;

: REJECT-FIXTURES! ( -- )
   UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" FS:WRITE-ALL
   BODY$ s" : LRD-Y ( n -- n ) drop ;" FS:WRITE-ALL
   \ The byte pointer comes from a declared cell: a raw `variable` hands out no
   \ address (E-RAW-CELL-PTR), which would refuse the fixture one definition
   \ before the token these cases pin.
   BPS$ s" TYPED-VARIABLE LRD-P ptr u8 : LRD-BASE ( -- ptr u8 ) LRD-P @ ; : LRD-BPS ( -- ) 0 LRD-BASE ! ;" FS:WRITE-ALL
   BPF$ s" TYPED-VARIABLE LRD-Q ptr u8 : LRD-QBASE ( -- ptr u8 ) LRD-Q @ ; : LRD-BPF ( -- n ) LRD-QBASE @ ;" FS:WRITE-ALL
   TARGET$ s" false ?TARGET" FS:WRITE-ALL
   TARGET-JSON$ S\" -1 REJECTION:JSON !\nfalse ?TARGET" FS:WRITE-ALL
   ONCE$ s" : F ( n -- n ) drop ;" FS:WRITE-ALL
   ONCE-UNDEF$ s" : H ( -- ) FOO ;" FS:WRITE-ALL
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
   CALLER$ S" TRUSTED: RAW-IMM ( -- ) ; immediate
-1 REJECTION:JSON !
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
   TRUSTED$ S\" -1 REJECTION:JSON ! : LRD-TRUST-IMM ( -- ) ; immediate TRUSTED: LRD-TRUST-BODY ( -- n ) LRD-TRUST-IMM 73 ;" FS:WRITE-ALL ;

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

\ stderr places the record at PATH's PLACE, `:line:column`.
: ASSERT-AT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n place:ptr placeu:n :}
   SB-RESET  s"  --> " SB-APPEND  path pathu SB-APPEND  place placeu SB-APPEND
   10 SB-APPEND-C
   ERR$ SB$ CONTAINS? TTRUE ;

\ body.f is rejected with the record HEAD at PLACE.
: BODY-REJECT-AT ( ptr u8 n ptr u8 n -- ) {: head:ptr headu:n place:ptr placeu:n :}
   head headu ASSERT-NAMED
   BODY$ place placeu ASSERT-AT ;

\ The immediate's record names TOKEN at LINE and COLUMN, and its help models it
\ with `parse-imm`, in the record form.
: ASSERT-IMM-DIAG ( n n ptr u8 n -- )
   {: line:n col:n token:ptr tokenu:n :}
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root s" code" GJA-REQ s" E-UNMODELED-IMMEDIATE" GJA-ASSERT-STR
   root s" line" line GJA-ASSERT-INT-FIELD
   root s" column" col GJA-ASSERT-INT-FIELD
   root s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   ERR$ s" declare it with `parse-imm`" CONTAINS? TTRUE
   root GJA-RECORD-FORM ;

: TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN PROCESS:SIGKILL PROCESS-ENDING:signaled s" " STORE!
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
\ a cell-width `ptr a`; a `ptr u8` must name the pair, not just the token.
: TEST-BYTE-PTR-STORE ( -- )
   s" cell store into a byte pointer names expected/actual" T-LABEL
   BPS$ RUN
   s" error[E-MISMATCH]: `!` takes ptr a" ASSERT-NAMED
   BPS$ s" :1:92" ASSERT-AT
   ERR$ S\" = expected: ptr a\n" CONTAINS? TTRUE
   ERR$ S\" = found: ptr u8\n" CONTAINS? TTRUE ;

\ Each control-flow case is a load of its own: a load stops at its first refusal.
\ Its record HEAD names the reason, placed at the token at PLACE.
: CF-RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n head:ptr headu:n place:ptr placeu:n :}
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
      s" error[E-REJECTED]: no loop is open here" s" :1:17" CF-RUN
   s" a j in a single loop says no outer loop is open" T-LABEL
   s" : LRD-JO ( -- ) 3 0 do j drop loop ;"
      s" error[E-REJECTED]: no outer loop is open here" s" :1:24" CF-RUN
   s" a then with no if says it closes nothing" T-LABEL
   s" : LRD-TS ( -- ) then ;"
      s" error[E-REJECTED]: this word closes nothing open here" s" :1:17" CF-RUN
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
      s" error[E-REJECTED]: exit inside a loop needs unloop first" s" :1:24" CF-RUN
   s" a leave that changes depth names both rows" T-LABEL
   s" : LRD-LV ( n -- n ) 3 0 do 1 leave loop ;"
      s" error[E-MISMATCH]: the loop body changes the stack's depth" s" :1:30" CF-RUN
   CF-ROWS?
   s" a loop body that changes the return stack's depth says so" T-LABEL
   s" : LRD-RD ( -- ) 3 0 do 1 >r loop ;"
      s" error[E-REJECTED]: the loop body changes the return stack's depth" s" :1:29" CF-RUN
   s" a second else says the if already has one" T-LABEL
   s" : LRD-E2 ( bool -- ) if else else then ;"
      s" error[E-REJECTED]: this if already has an else" s" :1:30" CF-RUN
   s" an if open at ; says a structure is still open" T-LABEL
   s" : LRD-OP ( bool -- ) if ;"
      s" error[E-REJECTED]: a control structure is still open at the end of the definition" s" :1:25" CF-RUN
   s" a +loop that meets an if inside its do says it closes nothing" T-LABEL
   s" : LRD-PI ( -- ) 3 0 do 0 0= if 1 +loop then ;"
      s" error[E-REJECTED]: this word closes nothing open here" s" :1:34" CF-RUN ;

: TEST-BYTE-PTR-FETCH ( -- )
   s" cell fetch from a byte pointer names expected/actual" T-LABEL
   BPF$ RUN
   s" error[E-MISMATCH]: `@` takes ptr a" ASSERT-NAMED
   BPF$ s" :1:94" ASSERT-AT
   ERR$ S\" = expected: ptr a\n" CONTAINS? TTRUE
   ERR$ S\" = found: ptr u8\n" CONTAINS? TTRUE ;

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
   s" error[E-UNDEFINED]: cannot find word `99999999999999999999999`" s" :1:27"
      BODY-REJECT-AT ;

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
   BODY$ s" require src/lib/c2-memory.f require src/lib/c2-bytes.f TRUSTED: LRD-SPLIT ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> ptr u8 n ) 2dup ; variable LRD-LEN variable LRD-LEN2 : LRD-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) LRD-SPLIT LRD-LEN ! drop C2-BYTES:MUT-LENGTH swap LRD-LEN2 ! ; : LRD-RUN ( -- ) C2-MEM:OWNER-SIZE [: LRD-ROOT ;] C2-MEM:WITH-MUT ; LRD-RUN LRD-LEN @ . LRD-LEN2 @ . cr" FS:WRITE-ALL
   BODY$ RUN S\" 8\n8\n" ASSERT-OK-OUT ;

\ A refused `!` of a two-cell product in a TRUSTED: body splices its own typed
\ rows and keeps their width fact, so it stores both cells.
: TEST-T0-RAW-STORE ( -- )
   s" T0 compiles a refused TRUSTED: store at its field width" T-LABEL
   BODY$ s" PRODUCT lrd-pt FIELD x n FIELD y n ;PRODUCT TYPED-VARIABLE LRD-PV lrd-pt TRUSTED: LRD-SET ( n n ptr lrd-pt -- ) ! ; : LRD-STORE ( -- n ) 7 3 4 LRD-PV LRD-SET ; LRD-STORE . depth . cr" FS:WRITE-ALL
   BODY$ RUN S\" 7\n0\n" ASSERT-OK-OUT ;

\ A storage declaration whose definer cannot size its type is the checker's
\ refusal, named for the declared word in HEAD and placed at the unknown type.
: STORAGE-REJECT ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n head:ptr headu:n :}
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

\ SRC as the body exits RC, its stderr holding the record HEAD placed at PLACE.
: CODE-REJECT-AT ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: src:ptr srcu:n head:ptr headu:n place:ptr placeu:n rc:n :}
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
      s" :1:28" REJECT-RC CODE-REJECT-AT
   s" defer LRD-D"
   s" error[E-MISSING-SIGNATURE]: `LRD-D` has no closed `( -- )` signature"
      s" :1:7" REJECT-RC CODE-REJECT-AT ;

\ Reopening a package the engine sealed stops at the package's name.
: TEST-SEALED-PACKAGE ( -- )
   s" a sealed package's reopening names its class, package and line" T-LABEL
   S\" package NDICT\n;package"
   s" error[E-SEALED-PACKAGE]: `NDICT` would change a sealed package"
      s" :1:9" REJECT-RC CODE-REJECT-AT ;

\ A definition into a package the engine sealed stops at its whole name.
: TEST-SEALED-DEFINE ( -- )
   s" a definition into a sealed package names its class, whole name and line" T-LABEL
   s" : NDICT:FOO ( -- ) ;"
   s" error[E-SEALED-PACKAGE]: `NDICT:FOO` would change a sealed package"
      s" :1:3" REJECT-RC CODE-REJECT-AT ;

\ A name a sealed package already holds is refused as sealed, as the check
\ refuses it: the seal is asked before the duplicate.
: TEST-SEALED-DUPLICATE ( -- )
   s" a duplicate definition into a sealed package names the seal" T-LABEL
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\n;package\n: SP:A ( -- ) ;\n"
   s" error[E-SEALED-PACKAGE]: `SP:A` would change a sealed package"
      s" :6:3" REJECT-RC CODE-REJECT-AT
   s" a duplicate export into a sealed package names the seal" T-LABEL
   SB-RESET
   S\" package SP2\npublic\n: A ( -- ) ;\n" SB-APPEND
   S\" : MK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" SB-APPEND
   S\" package SP\npublic\n: A ( -- ) ;\nseal-package\nEXPORT SP2:A\n" SB-APPEND
   S\" s\" ACCEPTED\" type cr\n;package\n" SB-APPEND
   SB$ s" error[E-SEALED-PACKAGE]: `SP2:A` would change a sealed package"
      s" :10:8" REJECT-RC CODE-REJECT-AT ;

\ A string `s"` copies out of the file, which its line 3 `evaluate`s, is not
\ the file's bytes, so its rejection is placed at that `evaluate`, with the
\ string in an evaluated note.
: ASSERT-EVALUATED ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n label:ptr labelu:n :}
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
   s" error[E-MISMATCH]: `c@` takes ptr u8" s" :3:14" BODY-REJECT-AT
   s" the native compiler places a refusal in a slice past the evaluating line" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 27 evaluate\n: LRD-QQ ( -- )\n5 c@ drop ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-MISMATCH]: `c@` takes ptr u8" s" :5:3" BODY-REJECT-AT
   s" the native compiler places a refusal at the closing ; of a slice" T-LABEL
   BODY$ S\" \n\nparse-name x drop 24 + 29 evaluate\nTRUSTED: LRD-SF ( -- ) 0 if\n;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-T0-CONTROL]: a control structure is still open at `;`" s" :5:1"
      BODY-REJECT-AT
   s" the checker hook places a checker-only duplicate in a slice at its line" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n\" CHECKER-DEFPRODUCT ;\nLRD-PREPLAY\nparse-name x drop 24 + 30 evaluate\n: LRDREP:MAKE ( n -- n ) 1 + ;\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-DUPLICATE-DEFINITION]: `LRDREP:MAKE` is already defined" CONTAINS? TTRUE
   BODY$ s" :4:3" ASSERT-AT
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

\ A structure the checker finds open at a body's closing `;`, a plain body's or
\ a does> clause's, is placed at that `;` (column 1 of PLACE's line).
: ASSERT-CLOSE-LINE ( ptr u8 n -- ) {: place:ptr placeu:n :}
   BODY$ RUN
   s" error[E-REJECTED]: a control structure is still open at the end of the definition"
      place placeu BODY-REJECT-AT ;

: TEST-OPEN-AT-CLOSE ( -- )
   s" an if open at a later line's ; is refused on the line of the ;" T-LABEL
   BODY$ S\" : LRD-OI ( bool -- ) if\n\n;\n" FS:WRITE-ALL
   s" :3:1" ASSERT-CLOSE-LINE
   s" a does> clause leaving an if open is refused on the line of its ;" T-LABEL
   BODY$ S\" : LRD-MKO ( n -- ) create ,\ndoes> ( -- )\n@ 0= if\n;\n" FS:WRITE-ALL
   s" :4:1" ASSERT-CLOSE-LINE ;

\ A stored signature the declaration refuses is placed on its refused token's
\ line, not the name's.
: TEST-BAD-SIGNATURE-LINE ( -- )
   s" a refused stored signature on a later line is refused on that line" T-LABEL
   BODY$ S\" TRUSTED: LRD-BS\n  ( -- nosuch:fam )\n  1 ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `LRD-BS` cannot be recorded"
      s" :2:8" BODY-REJECT-AT ;

\ A name only the checker holds, a PRODUCT replay with no word behind it, is
\ refused by check-hook.f DEFINITION through src/core/check-walk.f
\ REFUSE-DUPLICATE, placed at the `evaluate` that ran the string, with the
\ string in an evaluated note.
: TEST-REPLAY-DUP-PLACE ( -- )
   s" an evaluated checker-only duplicate is placed at its evaluate" T-LABEL
   BODY$ S\" : LRD-PREPLAY ( -- ) s\" lrdrep\" s\" FIELD x n\" CHECKER-DEFPRODUCT ;\nLRD-PREPLAY\ns\" : LRDREP:MAKE ( n -- n ) 1 + ;\" evaluate\n" FS:WRITE-ALL
   BODY$ RUN
   ERR$ s" error[E-DUPLICATE-DEFINITION]: `LRDREP:MAKE` is already defined" CONTAINS? TTRUE
   BODY$ s" :3:36" ASSERT-AT
   s" : LRDREP:MAKE ( n -- n ) 1 + ;" s" already defined" ASSERT-EVALUATED
   OUT-U @ 0 T=
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

\ A refused `cast:` names its code, place, word and reason.
: TEST-CAST ( -- )
   s" a cast of one cell into three names its arity" T-LABEL
   BODY$ s" cast: LRD-WIDE ( n -- ptr u8 n )" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-CAST-ARITY]: in `cast: LRD-WIDE`, the output is not one term over the stack below"
      s" :1:7" BODY-REJECT-AT ;

\ A checked body that calls a word acting on source or checker state is refused
\ E-UNSAFE, naming the word and why, placed at the call, with evaluate-closed as
\ the help.
: TEST-UNSAFE ( -- )
   s" a checked body calling evaluate names E-UNSAFE and why" T-LABEL
   BODY$ s" : LRD-EV ( ptr u8 n -- ) evaluate ;" FS:WRITE-ALL
   BODY$ RUN
   s" error[E-UNSAFE]: `evaluate` reads source or changes the compiler's or checker's state, which no stack effect describes"
      s" :1:26" BODY-REJECT-AT
   ERR$ s" `evaluate-closed`" CONTAINS? TTRUE ;

: TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   OUTER$ RUN
   ASSERT-UNDEF ;

\ A file that leaves cells is rejected once, by one record naming the count,
\ placed at the token that pushed the bottom cell, in the innermost file. Paths
\ are matched by their file names: the temporary root may resolve through a
\ symbolic link.
: ASSERT-RESIDUE ( ptr u8 n ptr u8 n -- ) {: msg:ptr msgu:n at:ptr atu:n :}
   EXITED @ TTRUE
   RC @ REJECT-RC T=
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
   s" this file leaves 1 cell " s" /residue-type.f:1:20" ASSERT-RESIDUE
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
   ASSERT-TARGET ;

\ Each case is its own child: a --load stops at its first rejection.
: TEST-DEF-ONCE ( -- )
   s" a rejected definition prints its diagnostic once" T-LABEL
   ONCE$ RUN
   SB-RESET
   s" error[E-MISMATCH]: the body ends with nothing on the stack;" SB-APPEND
   S\"  the signature promises n\n --> " SB-APPEND
   ONCE$ SB-APPEND  S\" :1:16\n  |\n1 | : F ( n -- n ) drop ;\n" SB-APPEND
   S\"   |                ^^^^ the stack is empty after this\n" SB-APPEND
   S\"   = expected: n\n  = found: (nothing)\n" SB-APPEND
   S\"   = declared: n -- n\n  = inferred: n --\n" SB-APPEND
   s"   = help: add the missing values after `drop` so the body" SB-APPEND
   S\"  ends with n, or declare ( n -- )\n\n" SB-APPEND
   REJECT-RC ASSERT-REJECTED
   s" an undefined word prints one E-UNDEFINED" T-LABEL
   ONCE-UNDEF$ RUN
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `FOO`\n --> " SB-APPEND
   ONCE-UNDEF$ SB-APPEND
   S\" :1:12\n  |\n1 | : H ( -- ) FOO ;\n" SB-APPEND
   S\"   |            ^^^ not found\n" SB-APPEND
   s"   = help: define `FOO` before this point," SB-APPEND
   S\"  or check its spelling\n\n" SB-APPEND
   REJECT-RC ASSERT-REJECTED
   s" a rejected definition prints one JSON line" T-LABEL
   ONCE-JSON$ RUN
   SB-RESET
   S\" {\"severity\":\"error\",\"code\":\"E-MISMATCH\"" SB-APPEND
   S\" ,\"message\":\"the body ends with nothing on the stack;" SB-APPEND
   S\"  the signature promises n\",\"file\":\"" SB-APPEND
   ONCE-JSON$ SB-APPEND
   S\" \",\"line\":2,\"column\":16,\"byte_start\":35" SB-APPEND
   S\" ,\"byte_end\":39,\"token\":\"drop\"" SB-APPEND
   S\" ,\"source_line\":\": F ( n -- n ) drop ;\"" SB-APPEND
   S\" ,\"label\":\"the stack is empty after this\"" SB-APPEND
   S\" ,\"notes\":{\"expected\":\"n\",\"found\":\"(nothing)\"" SB-APPEND
   S\" ,\"declared\":\"n -- n\",\"inferred\":\"n --\"}" SB-APPEND
   S\" ,\"help\":\"add the missing values after `drop` so the body" SB-APPEND
   S\"  ends with n, or declare ( n -- )\"}\n" SB-APPEND
   REJECT-RC ASSERT-REJECTED ;

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
   REJECT-RC ASSERT-REJECTED ;

: TEST-IMM-INCLUDE ( -- )
   s" checked include rejects before executing its fragment" T-LABEL
   INC$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 43 s" include" ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-REQUIRE ( -- )
   s" checked require rejects before executing its fragment" T-LABEL
   REQ$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 43 s" require" ASSERT-IMM-DIAG
   OUT$ s" LRD-FRAGMENT-EXECUTED" CONTAINS? TFALSE ;

: TEST-IMM-CALLER ( -- )
   s" an unmodeled immediate gets one canonical preflight diagnostic" T-LABEL
   CALLER$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   3 34 s" RAW-IMM" ASSERT-IMM-DIAG
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   \ Bytes count from the file's start, not the definition's: RAW-IMM is on line 3.
   root s" byte_start" 89 GJA-ASSERT-INT-FIELD
   root s" byte_end" 96 GJA-ASSERT-INT-FIELD ;

: TEST-IMM-TRUSTED ( -- )
   s" trusted body cannot bypass unmodeled immediate refusal" T-LABEL
   TRUSTED$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   1 89 s" LRD-TRUST-IMM" ASSERT-IMM-DIAG ;

\ A rejection at a token an immediate evaluated into the body, which no source
\ holds, gets one record in the record form with no place.
: TEST-UNSOURCED-RECORD ( -- )
   s" a rejection at an evaluated token gets a record with no place" T-LABEL
   BODY$ S\" true REJECTION:JSON !\nTRUSTED: EV ( -- ) s\" drop\" evaluate ; immediate\ns\" EV\" 0 parse-imm\n: P ( -- ) ( a comment here ) 1 drop EV ;\n" FS:WRITE-ALL
   BODY$ RUN
   s" E-INPUT-UNDERFLOW" ASSERT-NAMED
   ERR$ GJA-PARSE dup GJA-OBJ {: root:SPAN:span<u8> :}
   root s" token" GJA-REQ s" drop" GJA-ASSERT-STR
   root s" line" GJA-HAS? TFALSE
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
   TEST-TARGET
   TEST-DEF-ONCE
   TEST-SHADOW
   TEST-IMM-INCLUDE
   TEST-IMM-REQUIRE
   TEST-IMM-CALLER
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
   FS:CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

MAIN

;package
