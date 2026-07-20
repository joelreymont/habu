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
\   tools/json.f tools/gate-json-assert-core.f lib/argv.f
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
require lib/argv.f
require tools/repair-packet-core.f

package LOAD-REJECT-TEST

$1000 constant CAP
60000 constant TIMEOUT-MS
70 constant REJECT-RC
3 constant TEST-OUT-U
5 constant TEST-ERR-U

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot
create BASE FS-PATH-CAP allot
create UNDEF-P FS-PATH-CAP allot
create BODY-P FS-PATH-CAP allot
create OUTER-P FS-PATH-CAP allot
create FRAG-P FS-PATH-CAP allot
create SIDE-P FS-PATH-CAP allot
create INC-P FS-PATH-CAP allot
create REQ-P FS-PATH-CAP allot
create SIGLESS-P FS-PATH-CAP allot
create TOP-INC-P FS-PATH-CAP allot
create TOP-REQ-P FS-PATH-CAP allot
create RUNTIME-P FS-PATH-CAP allot
create MODELED-P FS-PATH-CAP allot
create TRUSTED-P FS-PATH-CAP allot

variable BASE-U
variable UNDEF-U
variable BODY-U
variable OUTER-U
variable FRAG-U
variable SIDE-U
variable INC-U
variable REQ-U
variable SIGLESS-U
variable TOP-INC-U
variable TOP-REQ-U
variable RUNTIME-U
variable MODELED-U
variable TRUSTED-U
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

: SIGLESS$ ( -- ptr u8 n )
   SIGLESS-P SIGLESS-U @ ;

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

: FRAG-FIXTURE! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n frag:ptr fragu:n post:ptr postu:n path:ptr pathu:n :}
   SB-RESET
   pre preu SB-APPEND
   frag fragu SB-APPEND
   post postu SB-APPEND
   path pathu SB$ WRITE-ALL ;

\ Resolve the child engine (the gate-runner-entry-test pattern).
: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: name:ptr nameu:n dst:ptr lenp:ptr :}
   BASE$ name nameu dst JOIN-PATH lenp ! ;

: ROOT! ( -- )
   CLEANUP-RESET
   s" habu-load-reject-diag" TMPDIR-MKDIR {: a:ptr u:n :}
   a BASE u BYTE-COPY
   u BASE-U !
   BASE$ CLEANUP-TREE+ ;

: PATHS! ( -- )
   s" undef.f" UNDEF-P UNDEF-U PATH!
   s" body.f" BODY-P BODY-U PATH!
   s" outer.f" OUTER-P OUTER-U PATH!
   s" frag.f" FRAG-P FRAG-U PATH!
   s" side.f" SIDE-P SIDE-U PATH!
   s" imm-include.f" INC-P INC-U PATH!
   s" imm-require.f" REQ-P REQ-U PATH!
   s" imm-signatureless.f" SIGLESS-P SIGLESS-U PATH!
   s" top-include.f" TOP-INC-P TOP-INC-U PATH!
   s" top-require.f" TOP-REQ-P TOP-REQ-U PATH!
   s" runtime.f" RUNTIME-P RUNTIME-U PATH!
   s" modeled.f" MODELED-P MODELED-U PATH!
   s" trusted.f" TRUSTED-P TRUSTED-U PATH! ;

: REJECT-FIXTURES! ( -- )
   UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" WRITE-ALL
   BODY$ s" : LRD-Y ( n -- n ) drop ;" WRITE-ALL
   FRAG$ s" 1 +" WRITE-ALL
   SIDE$ S\" [ s\" LRD-FRAGMENT-EXECUTED\" type ]" WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   UNDEF$ SB-APPEND
   OUTER$ SB$ WRITE-ALL ;

: IMM-FIXTURES! ( -- )
   s" -1 JSON-DIAGS ! : LRD-IMM-I ( -- ) 73 include "
   SIDE$ s"  . ;" INC$ FRAG-FIXTURE!
   s" -1 JSON-DIAGS ! : LRD-IMM-R ( -- ) 73 require "
   SIDE$ s"  . ;" REQ$ FRAG-FIXTURE!
   SIGLESS$ S" 0 set-check
: RAW-IMM ( -- ) ; immediate
LOWER-CERT-HOOK:INSTALL
-1 JSON-DIAGS !
: LRD-IMM-S ( mystery:foo -- n ) RAW-IMM 73 ;
" WRITE-ALL ;

: RUNTIME-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   S\" : RUN-I ( -- ) 73 s\" " SB-APPEND FRAG$ SB-APPEND
   S\" \" included . ; : RUN-R ( -- ) 73 s\" " SB-APPEND FRAG$ SB-APPEND
   S\" \" required . ; : RUN-P ( -- ) s\" " SB-APPEND FRAG$ SB-APPEND
   S\" \" provided ; RUN-I RUN-R RUN-P" SB-APPEND
   SB$ ;

: POSITIVE-FIXTURES! ( -- )
   s" 73 include " FRAG$ s"  ." TOP-INC$ FRAG-FIXTURE!
   s" 73 require " FRAG$ s"  ." TOP-REQ$ FRAG-FIXTURE!
   RUNTIME$ RUNTIME-SOURCE$ WRITE-ALL
   MODELED$ S\" : PI ( -- ) ; immediate s\" PI\" 0 parse-imm : PIM ( -- n ) PI 73 ; PIM ." WRITE-ALL
   TRUSTED$ s" : OK-TI ( -- ) ; immediate TRUSTED: OK-TIM ( -- n ) OK-TI 73 ; OK-TIM ." WRITE-ALL ;

: FIXTURES! ( -- )
   REJECT-FIXTURES!
   IMM-FIXTURES!
   POSITIVE-FIXTURES! ;

: SETUP ( -- )
   ROOT!
   PATHS!
   FIXTURES! ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ Spawn `<hb> --load <fixture>` with empty stdin, capture the exit outcome.
: RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

\ Every rejecting load: exit-kind EXIT, rc 70, EMPTY stdout, NON-EMPTY stderr
\ that names the failing token — the silent-exit-70 red/green discriminator.
: ASSERT-NAMED ( ptr u8 n -- ) {: name:ptr nameu:n :}
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   OUT-U @ 0 T=
   ERR-U @ 0 > TTRUE
   ERR$ name nameu CONTAINS? TTRUE ;

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
   ERR$ JSON-PARSE dup GJA-OBJ {: root:n :}
   root GJA-DIAG-CONTRACT-ROW
   root s" code" GJA-REQ s" E-UNMODELED-IMMEDIATE" GJA-ASSERT-STR
   root s" repair_class" GJA-REQ s" model_compile_immediate" GJA-ASSERT-STR
   root s" word" GJA-REQ word wordu GJA-ASSERT-STR
   root s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   ERR$ 2dup RP-COUNT dup 1 T= >r RP-FIRST r> RP-PACKET
   {: pa:ptr pu:n :}
   pa OUT pu BYTE-COPY
   OUT pu JSON-PARSE dup GJA-OBJ {: packet:n :}
   packet s" model_compile_immediate" GJA-REPAIR-HEAD
   packet GJA-REPAIR-DEF ;

: TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN SIGKILL OUTCOME:SIGNALED STORE!
   EXITED @ TFALSE
   RC @ SIGKILL T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

: TEST-STORE-TIMEOUT ( -- )
   s" timeout outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN OUTCOME:TIMEOUT STORE!
   EXITED @ TFALSE
   RC @ 0 T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

: TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word" T-LABEL
   UNDEF$ RUN
   s" E-UNDEFINED" ASSERT-NAMED
   ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   BODY$ RUN
   s" in lrd-y" ASSERT-NAMED
   ERR$ s" at 'drop'" CONTAINS? TTRUE ;

: TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   OUTER$ RUN
   s" E-UNDEFINED" ASSERT-NAMED
   ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

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

: TEST-IMM-SIGNATURELESS ( -- )
   s" signatureless immediate gets one canonical preflight diagnostic" T-LABEL
   SIGLESS$ RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   s" lrd-imm-s" s" RAW-IMM" ASSERT-IMM-DIAG
   ERR$ JSON-PARSE dup GJA-OBJ {: root:n :}
   root s" token_index" 1 GJA-ASSERT-INT-FIELD
   root s" byte_start" 31 GJA-ASSERT-INT-FIELD
   root s" byte_end" 38 GJA-ASSERT-INT-FIELD ;

: TEST-PREFLIGHT-POSITIVES ( -- )
   s" top-level include remains live" T-LABEL
   TOP-INC$ RUN s" 74" ASSERT-OK-OUT
   s" top-level require remains live" T-LABEL
   TOP-REQ$ RUN s" 74" ASSERT-OK-OUT
   s" runtime included required and provided remain live" T-LABEL
   RUNTIME$ RUN s" 74" ASSERT-OK-OUT
   s" parse-imm modeled immediate remains live" T-LABEL
   MODELED$ RUN s" 73" ASSERT-OK-OUT
   s" trusted immediate body remains live" T-LABEL
   TRUSTED$ RUN s" 73" ASSERT-OK-OUT ;

: MAIN ( -- )
   T-RESET
   SETUP
   TEST-STORE-SIGNALED
   TEST-STORE-TIMEOUT
   TEST-UNDEF
   TEST-BODY
   TEST-REQUIRE-CHAIN
   TEST-IMM-INCLUDE
   TEST-IMM-REQUIRE
   TEST-IMM-SIGNATURELESS
   TEST-PREFLIGHT-POSITIVES
   CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

MAIN

;package
