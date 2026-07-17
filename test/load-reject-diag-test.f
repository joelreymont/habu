\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself.
\
\ Invariant (dot habu-silent-exit-70-215b2da7): every load leg that rejects a
\ definition exits 70 WITH a named diagnostic on stderr — a silent fail-closed
\ exit is the harness gap this campaign keeps paying for (cf. the seal-exit
\ labeling and the check.f E-FS-CAPACITY raw-67 fix). Pins the direct --load
\ leg, the require-chain leg, and the checked-body reject leg by spawning the
\ engine on generated rejecting fixtures and asserting stderr names the error.
\ The child engine is HABU_UNDER_TEST when the gate sets it, else bin/hb.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   tools/json.f tools/gate-json-assert-core.f tools/argv.f
\   tools/repair-packet-core.f test/load-reject-diag-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require tools/json.f
require tools/gate-json-assert-core.f
require tools/argv.f
require tools/repair-packet-core.f

$1000 constant LRD-CAP
60000 constant LRD-TIMEOUT-MS
70 constant LRD-REJECT-RC            \ the checked-reject exit class
3 constant LRD-TEST-OUT-U
5 constant LRD-TEST-ERR-U

create LRD-OUT LRD-CAP allot
create LRD-ERR LRD-CAP allot
create LRD-EMPTY 1 allot             \ zero-length stdin
create LRD-BASE FS-PATH-CAP allot
create LRD-UNDEF-PATH FS-PATH-CAP allot
create LRD-BODY-PATH FS-PATH-CAP allot
create LRD-OUTER-PATH FS-PATH-CAP allot
create LRD-FRAG-PATH FS-PATH-CAP allot
create LRD-INC-PATH FS-PATH-CAP allot
create LRD-REQ-PATH FS-PATH-CAP allot
create LRD-TOP-INC-PATH FS-PATH-CAP allot
create LRD-TOP-REQ-PATH FS-PATH-CAP allot
create LRD-RUNTIME-PATH FS-PATH-CAP allot
create LRD-MODELED-PATH FS-PATH-CAP allot
create LRD-TRUSTED-PATH FS-PATH-CAP allot

variable LRD-BASE-U
variable LRD-UNDEF-U
variable LRD-BODY-U
variable LRD-OUTER-U
variable LRD-FRAG-U
variable LRD-INC-U
variable LRD-REQ-U
variable LRD-TOP-INC-U
variable LRD-TOP-REQ-U
variable LRD-RUNTIME-U
variable LRD-MODELED-U
variable LRD-TRUSTED-U
variable LRD-OUT-U
variable LRD-ERR-U
variable LRD-EXITED
variable LRD-RC

: LRD-BASE$ ( -- ptr u8 n )
   LRD-BASE LRD-BASE-U @ ;

: LRD-UNDEF$ ( -- ptr u8 n )
   LRD-UNDEF-PATH LRD-UNDEF-U @ ;

: LRD-BODY$ ( -- ptr u8 n )
   LRD-BODY-PATH LRD-BODY-U @ ;

: LRD-OUTER$ ( -- ptr u8 n )
   LRD-OUTER-PATH LRD-OUTER-U @ ;

: LRD-FRAG$ ( -- ptr u8 n )
   LRD-FRAG-PATH LRD-FRAG-U @ ;

: LRD-INC$ ( -- ptr u8 n )
   LRD-INC-PATH LRD-INC-U @ ;

: LRD-REQ$ ( -- ptr u8 n )
   LRD-REQ-PATH LRD-REQ-U @ ;

: LRD-TOP-INC$ ( -- ptr u8 n )
   LRD-TOP-INC-PATH LRD-TOP-INC-U @ ;

: LRD-TOP-REQ$ ( -- ptr u8 n )
   LRD-TOP-REQ-PATH LRD-TOP-REQ-U @ ;

: LRD-RUNTIME$ ( -- ptr u8 n )
   LRD-RUNTIME-PATH LRD-RUNTIME-U @ ;

: LRD-MODELED$ ( -- ptr u8 n )
   LRD-MODELED-PATH LRD-MODELED-U @ ;

: LRD-TRUSTED$ ( -- ptr u8 n )
   LRD-TRUSTED-PATH LRD-TRUSTED-U @ ;

: LRD-WRITE-FRAG-FIXTURE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n post:ptr postu:n path:ptr pathu:n :}
   SB-RESET
   pre preu SB-APPEND
   LRD-FRAG$ SB-APPEND
   post postu SB-APPEND
   path pathu SB$ WRITE-ALL ;

\ Resolve the child engine (the gate-runner-entry-test pattern).
: LRD-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: LRD-SETUP ( -- )
   CLEANUP-RESET
   s" habu-load-reject-diag" TMPDIR-MKDIR {: a:ptr u:n :}
   a LRD-BASE u BYTE-COPY
   u LRD-BASE-U !
   LRD-BASE$ CLEANUP-TREE+
   LRD-BASE$ s" undef.f" LRD-UNDEF-PATH JOIN-PATH LRD-UNDEF-U !
   LRD-BASE$ s" body.f" LRD-BODY-PATH JOIN-PATH LRD-BODY-U !
   LRD-BASE$ s" outer.f" LRD-OUTER-PATH JOIN-PATH LRD-OUTER-U !
   LRD-BASE$ s" frag.f" LRD-FRAG-PATH JOIN-PATH LRD-FRAG-U !
   LRD-BASE$ s" imm-include.f" LRD-INC-PATH JOIN-PATH LRD-INC-U !
   LRD-BASE$ s" imm-require.f" LRD-REQ-PATH JOIN-PATH LRD-REQ-U !
   LRD-BASE$ s" top-include.f" LRD-TOP-INC-PATH JOIN-PATH LRD-TOP-INC-U !
   LRD-BASE$ s" top-require.f" LRD-TOP-REQ-PATH JOIN-PATH LRD-TOP-REQ-U !
   LRD-BASE$ s" runtime.f" LRD-RUNTIME-PATH JOIN-PATH LRD-RUNTIME-U !
   LRD-BASE$ s" modeled.f" LRD-MODELED-PATH JOIN-PATH LRD-MODELED-U !
   LRD-BASE$ s" trusted.f" LRD-TRUSTED-PATH JOIN-PATH LRD-TRUSTED-U !
   LRD-UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" WRITE-ALL
   LRD-BODY$ s" : LRD-Y ( n -- n ) drop ;" WRITE-ALL
   LRD-FRAG$ s" 1 +" WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   LRD-UNDEF$ SB-APPEND
   LRD-OUTER$ SB$ WRITE-ALL
   s" -1 JSON-DIAGS ! : LRD-IMM-I ( -- ) 73 include "
   s"  . ;" LRD-INC$ LRD-WRITE-FRAG-FIXTURE
   s" -1 JSON-DIAGS ! : LRD-IMM-R ( -- ) 73 require "
   s"  . ;" LRD-REQ$ LRD-WRITE-FRAG-FIXTURE
   s" 73 include " s"  ." LRD-TOP-INC$ LRD-WRITE-FRAG-FIXTURE
   s" 73 require " s"  ." LRD-TOP-REQ$ LRD-WRITE-FRAG-FIXTURE
   SB-RESET
   S\" : LRD-RUN-I ( -- ) 73 s\" " SB-APPEND LRD-FRAG$ SB-APPEND
   S\" \" included . ; : LRD-RUN-R ( -- ) 73 s\" " SB-APPEND LRD-FRAG$ SB-APPEND
   S\" \" required . ; : LRD-RUN-P ( -- ) s\" " SB-APPEND LRD-FRAG$ SB-APPEND
   S\" \" provided ; LRD-RUN-I LRD-RUN-R LRD-RUN-P" SB-APPEND
   LRD-RUNTIME$ SB$ WRITE-ALL
   LRD-MODELED$ S\" : LRD-PI ( -- ) ; immediate s\" LRD-PI\" 0 parse-imm : LRD-PIM ( -- n ) LRD-PI 73 ; LRD-PIM ." WRITE-ALL
   LRD-TRUSTED$ s" : LRD-TI ( -- ) ; immediate TRUSTED: LRD-TIM ( -- n ) LRD-TI 73 ; LRD-TIM ." WRITE-ALL ;

: LRD-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF LRD-RC ! 0 0= LRD-EXITED ! ENDOF
     signaled OF LRD-RC ! 0 0= 0= LRD-EXITED ! ENDOF
     timeout OF 0 LRD-RC ! 0 0= 0= LRD-EXITED ! ENDOF
   ;MATCH
   LEN>N LRD-ERR-U !  LEN>N LRD-OUT-U ! ;

\ Spawn `<hb> --load <fixture>` with empty stdin, capture the exit outcome.
: LRD-RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   LRD-HB$ >LEN  LRD-EMPTY 0 >LEN  LRD-OUT LRD-CAP >LEN
   LRD-ERR LRD-CAP >LEN  LRD-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   LRD-STORE! ;

: LRD-ERR$ ( -- ptr u8 n )
   LRD-ERR LRD-ERR-U @ ;

: LRD-OUT$ ( -- ptr u8 n )
   LRD-OUT LRD-OUT-U @ ;

\ Every rejecting load: exit-kind EXIT, rc 70, EMPTY stdout, NON-EMPTY stderr
\ that names the failing token — the silent-exit-70 red/green discriminator.
: LRD-ASSERT-NAMED ( ptr u8 n -- ) {: name:ptr nameu:n :}
   LRD-EXITED @ TTRUE
   LRD-RC @ LRD-REJECT-RC T=
   LRD-OUT-U @ 0 T=
   LRD-ERR-U @ 0 > TTRUE
   LRD-ERR$ name nameu CONTAINS? TTRUE ;

: LRD-ASSERT-OK ( -- )
   LRD-EXITED @ TTRUE
   LRD-RC @ 0 T=
   LRD-ERR-U @ 0 T= ;

: LRD-ASSERT-OK-OUT ( ptr u8 n -- )
   {: a:ptr u:n :}
   LRD-ASSERT-OK
   LRD-OUT$ a u CONTAINS? TTRUE ;

: LRD-ASSERT-IMM-DIAG ( ptr u8 n ptr u8 n -- )
   {: word:ptr wordu:n token:ptr tokenu:n :}
   LRD-ERR$ JSON-PARSE dup GJA-OBJ {: root:n :}
   root GJA-DIAG-CONTRACT-ROW
   root s" code" GJA-REQ s" E-UNMODELED-IMMEDIATE" GJA-ASSERT-STR
   root s" repair_class" GJA-REQ s" model_compile_immediate" GJA-ASSERT-STR
   root s" word" GJA-REQ word wordu GJA-ASSERT-STR
   root s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   LRD-ERR$ 2dup RP-COUNT >r RP-FIRST r> RP-PACKET
   {: pa:ptr pu:n :}
   pa LRD-OUT pu BYTE-COPY
   LRD-OUT pu JSON-PARSE dup GJA-OBJ {: packet:n :}
   packet s" model_compile_immediate" GJA-REPAIR-HEAD
   packet GJA-REPAIR-DEF ;

: LRD-TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   LRD-TEST-OUT-U >LEN LRD-TEST-ERR-U >LEN SIGKILL OUTCOME:SIGNALED LRD-STORE!
   LRD-EXITED @ TFALSE
   LRD-RC @ SIGKILL T=
   LRD-OUT-U @ LRD-TEST-OUT-U T=
   LRD-ERR-U @ LRD-TEST-ERR-U T= ;

: LRD-TEST-STORE-TIMEOUT ( -- )
   s" timeout outcome remains distinguishable from exit" T-LABEL
   LRD-TEST-OUT-U >LEN LRD-TEST-ERR-U >LEN OUTCOME:TIMEOUT LRD-STORE!
   LRD-EXITED @ TFALSE
   LRD-RC @ 0 T=
   LRD-OUT-U @ LRD-TEST-OUT-U T=
   LRD-ERR-U @ LRD-TEST-ERR-U T= ;

: LRD-TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word" T-LABEL
   LRD-UNDEF$ LRD-RUN
   s" E-UNDEFINED" LRD-ASSERT-NAMED
   LRD-ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: LRD-TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   LRD-BODY$ LRD-RUN
   s" in lrd-y" LRD-ASSERT-NAMED
   LRD-ERR$ s" at 'drop'" CONTAINS? TTRUE ;

: LRD-TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   LRD-OUTER$ LRD-RUN
   s" E-UNDEFINED" LRD-ASSERT-NAMED
   LRD-ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: LRD-TEST-IMM-INCLUDE ( -- )
   s" checked include rejects before executing its fragment" T-LABEL
   LRD-INC$ LRD-RUN
   s" E-UNMODELED-IMMEDIATE" LRD-ASSERT-NAMED
   s" lrd-imm-i" s" include" LRD-ASSERT-IMM-DIAG ;

: LRD-TEST-IMM-REQUIRE ( -- )
   s" checked require rejects before executing its fragment" T-LABEL
   LRD-REQ$ LRD-RUN
   s" E-UNMODELED-IMMEDIATE" LRD-ASSERT-NAMED
   s" lrd-imm-r" s" require" LRD-ASSERT-IMM-DIAG ;

: LRD-TEST-PREFLIGHT-POSITIVES ( -- )
   s" top-level include remains live" T-LABEL
   LRD-TOP-INC$ LRD-RUN s" 74" LRD-ASSERT-OK-OUT
   s" top-level require remains live" T-LABEL
   LRD-TOP-REQ$ LRD-RUN s" 74" LRD-ASSERT-OK-OUT
   s" runtime included required and provided remain live" T-LABEL
   LRD-RUNTIME$ LRD-RUN s" 74" LRD-ASSERT-OK-OUT
   s" parse-imm modeled immediate remains live" T-LABEL
   LRD-MODELED$ LRD-RUN s" 73" LRD-ASSERT-OK-OUT
   s" trusted immediate body remains live" T-LABEL
   LRD-TRUSTED$ LRD-RUN s" 73" LRD-ASSERT-OK-OUT ;

: LRD-MAIN ( -- )
   T-RESET
   LRD-SETUP
   LRD-TEST-STORE-SIGNALED
   LRD-TEST-STORE-TIMEOUT
   LRD-TEST-UNDEF
   LRD-TEST-BODY
   LRD-TEST-REQUIRE-CHAIN
   LRD-TEST-IMM-INCLUDE
   LRD-TEST-IMM-REQUIRE
   LRD-TEST-PREFLIGHT-POSITIVES
   CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

LRD-MAIN
