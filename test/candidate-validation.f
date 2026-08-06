\ candidate-validation.f - digest-exact resident candidate validation worker.
\
\ Run `shared` with the exact candidate and bin/hb, then compare their evidence.
\ Run `top-row` only with the candidate: its eight loader-sensitive cases retain
\ their direct process boundary without redundantly running them on bin/hb. Each
\ worker requires HABU_UNDER_TEST to name its own executable. Source cases run
\ from one read-only snapshot in SUBJECT:RUN fork children.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/test/subject.f
require test/gate-stats.f

package CANDIDATE-VALIDATION

$40000 constant SRC-CAP
$8000 constant IO-CAP
64 constant HEX-LEN
120000 constant TIMEOUT-MS
10 constant LF

ENUM case-kind positive diagnostic negative ;ENUM
ENUM evidence-kind exited signaled timed-out ;ENUM

create SRC SRC-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
create DG 32 allot
create SRC-HEX HEX-LEN allot
create OUT-HEX HEX-LEN allot
create ERR-HEX HEX-LEN allot
create TARGET-HEX HEX-LEN allot
1 LAYOUT-BUFFER KIND-BUF evidence-kind

variable SRC-U
variable OUT-U
variable ERR-U
variable CODE
variable FAILS

: N. ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + emit ;

: KIND-PTR ( -- ptr evidence-kind )
   0 KIND-BUF ;

: KIND! ( evidence-kind -- )
   KIND-PTR ! ;

: KIND-VALUE ( -- evidence-kind )
   KIND-PTR @ ;

: HASH ( ptr u8 n ptr u8 -- ) {: a:ptr u:n hex:ptr :}
   a u DG SHA256
   DG hex SHA256>HEX ;

: TARGET$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" candidate-validation: HABU_UNDER_TEST is required" 1 die
   then ;

: CHECK-TARGET ( -- )
   TARGET$ TARGET-HEX SHA256-FILE-HEX 0 <> if E-ENGINE-KEY throw then
   ENGINE-ID:KEY$ TARGET-HEX HEX-LEN STR= 0= if
      s" candidate-validation: HABU_UNDER_TEST does not name this worker" 1 die
   then ;

: LOAD-SRC ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SRC SRC-CAP READ-ALL SRC-U !
   SRC SRC-U @ SRC-HEX HASH ;

: STORE ( len len outcome -- )
   MATCH outcome
     exited OF CODE ! construct evidence-kind exited KIND! ENDOF
     signaled OF CODE ! construct evidence-kind signaled KIND! ENDOF
     timeout OF 0 CODE ! construct evidence-kind timed-out KIND! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N OUT-U !
   OUT OUT-U @ OUT-HEX HASH
   ERR ERR-U @ ERR-HEX HASH ;

: CAPTURE ( -- )
   SRC SRC-U @ OUT IO-CAP >LEN ERR IO-CAP >LEN
   TIMEOUT-MS >MS SUBJECT:RUN STORE ;

: MARK ( -- )
   s" candidate-validation" GS-CHILD-LABEL! ;

: KIND$ ( -- ptr u8 n )
   KIND-VALUE MATCH evidence-kind
     exited OF s" exit" ENDOF
     signaled OF s" signal" ENDOF
     timed-out OF s" timeout" ENDOF
   ;MATCH ;

: TAB ( -- )
   9 emit ;

: MANIFEST ( ptr u8 n -- )
   s" manifest" type TAB type cr ;

: EVIDENCE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" case" type TAB
   path pathu type TAB
   SRC-U @ N. TAB
   SRC-HEX HEX-LEN type TAB
   KIND$ type TAB
   CODE @ N. TAB
   OUT-U @ N. TAB
   OUT-HEX HEX-LEN type TAB
   ERR-U @ N. TAB
   ERR-HEX HEX-LEN type cr ;

: FAIL ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n reason:ptr reasonu:n :}
   FAILS @ 1 + FAILS !
   s" candidate-validation: " type path pathu type
   s" : " type reason reasonu type cr ;

: CHECK ( bool ptr u8 n ptr u8 n -- )
   {: ok:bool path:ptr pathu:n reason:ptr reasonu:n :}
   ok 0= if path pathu reason reasonu FAIL then ;

: EXIT=? ( n -- bool ) {: want:n :}
   KIND-VALUE MATCH evidence-kind
     exited OF CODE @ want = ENDOF
     signaled OF 0 0= 0= ENDOF
     timed-out OF 0 0= 0= ENDOF
   ;MATCH ;

: OUT-OK? ( -- bool )
   OUT-U @ 3 < if 0 0= 0= exit then
   OUT OUT-U @ 3 - + 2 s" ok" STR=
   OUT OUT-U @ 1 - + c@ LF = and ;

: OUT-LINE= ( ptr u8 n -- bool ) {: want:ptr wantu:n :}
   OUT-U @ wantu 1 + <> if 0 0= 0= exit then
   OUT wantu want wantu STR=
   OUT wantu + c@ LF = and ;

: ERR-HAS? ( ptr u8 n -- bool ) {: needle:ptr needleu:n :}
   ERR ERR-U @ needle needleu CONTAINS? ;

: POSITIVE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu LOAD-SRC
   CAPTURE
   path pathu EVIDENCE
   0 EXIT=? path pathu s" expected exit 0" CHECK
   ERR-U @ 0= path pathu s" expected empty stderr" CHECK
   OUT-OK? path pathu s" stdout must end in ok LF" CHECK ;

: DIAGNOSTIC ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu LOAD-SRC
   CAPTURE
   path pathu EVIDENCE
   0 EXIT=? path pathu s" expected exit 0" CHECK
   OUT-OK? path pathu s" stdout must end in ok LF" CHECK
   needle needleu ERR-HAS? path pathu s" missing expected stderr" CHECK ;

: NEGATIVE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n rc:n marker:ptr markeru:n needle:ptr needleu:n :}
   path pathu LOAD-SRC
   CAPTURE
   path pathu EVIDENCE
   rc EXIT=? path pathu s" wrong negative outcome" CHECK
   marker markeru OUT-LINE= path pathu s" wrong armed stdout" CHECK
   needle needleu ERR-HAS? path pathu s" missing stderr diagnostic" CHECK ;

: RUN-CASE ( ptr u8 n case-kind n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n kind:case-kind rc:n marker:ptr markeru:n needle:ptr needleu:n :}
   kind MATCH case-kind
     positive OF path pathu POSITIVE ENDOF
     diagnostic OF path pathu needle needleu DIAGNOSTIC ENDOF
     negative OF path pathu rc marker markeru needle needleu NEGATIVE ENDOF
   ;MATCH ;

\ Expected case tally by kind, kept beside the case list so adding or removing a
\ case is a single-file edit: change the row below and bump the matching count
\ here. The whitebox test reads these back out of this source and checks them
\ against the enumerated rows, so a case dropped without adjusting its count
\ trips loudly. The lone top-row case is a positive, counted here too.
33 constant N-POSITIVE
5 constant N-DIAGNOSTIC
13 constant N-NEGATIVE

: SHARED-CASES ( -- )
   s" test/type-family-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/type-family-rollback-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/type-field-owner-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/generated-declaration-transaction-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/xt-cell-band-bad.f" construct case-kind negative 98
      s" XT-CELL-BAND-ARMED" s" hb: snapshot address cell out of range" RUN-CASE
   s" test/checker-decl-nested-bad.f" construct case-kind negative 76
      s" CHECKER-DECL-NESTED-ARMED" s" checker: declaration rollback frame mismatch" RUN-CASE
   s" test/checker-decl-depth0-bad.f" construct case-kind negative 76
      s" CHECKER-DECL-DEPTH0-ARMED" s" checker: declaration rollback frame mismatch" RUN-CASE
   s" test/decl-event-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/structure-make-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/structure-decl-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/structure-certify-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/enum-decl-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/decl-replay-verify-source.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/enum-ctor-collide-bad.f" construct case-kind negative 76
      s" ENUM-CTOR-COLLIDE-ARMED" s" sumtype: generated declaration already defined" RUN-CASE
   s" test/type-decl-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/deftype-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/deftype-dup-bad.f" construct case-kind negative 67
      s" DEFTYPE-DUP-ARMED" s" duplicate family" RUN-CASE
   s" test/cast-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/cast-negative-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/type-linear-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/type-match-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/rigid-region-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/type-layout-lower-pending.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/layout-buffer.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/field-proj-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/layout-defer.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/engine-suite.f" construct case-kind diagnostic 0 s" " s" habu: in mea1:" RUN-CASE
   s" test/type-ctor-suite.f" construct case-kind diagnostic 0 s" " s" duplicate family" RUN-CASE
   s" test/using-test.f" construct case-kind diagnostic 0 s" " s" hb: using: unknown package:" RUN-CASE
   s" test/type-export-suite.f" construct case-kind diagnostic 0 s" " s" habu: in xpu3:" RUN-CASE
   s" test/lower-cert.f" construct case-kind diagnostic 0 s" " s" habu: in lc-named-bad:" RUN-CASE
   s" test/layout-buffer-forge.f" construct case-kind negative 70
      s" LAYOUT-BUFFER-FORGE-ARMED" s" E-UNDEFINED: LBUF-PEND!" RUN-CASE
   s" test/layout-valid-w1-bad.f" construct case-kind negative 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" RUN-CASE
   s" test/layout-valid-product-bad.f" construct case-kind negative 85
      s" LAYOUT-VALID-ARMED" s" hb: bad layout tag" RUN-CASE
   s" test/layout-valid-active-bad.f" construct case-kind negative 85
      s" LAYOUT-VALID-ACTIVE-ARMED" s" hb: bad layout tag" RUN-CASE
   s" test/layout-valid-root-bad.f" construct case-kind negative 85
      s" LAYOUT-VALID-ROOT-ARMED" s" hb: bad layout tag" RUN-CASE
   s" test/layout-valid-hook-forge.f" construct case-kind negative 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: LAYOUT-VALID-RECORD-XT" RUN-CASE
   s" test/layout-valid-walk-forge.f" construct case-kind negative 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: VP-TERM-WALK-XT" RUN-CASE
   s" test/layout-valid-desc-forge.f" construct case-kind negative 70
      s" LAYOUT-VALID-FORGE-ARMED" s" E-UNDEFINED: LAYOUT-VALID-DESC-XT" RUN-CASE
   s" test/layout-buffer-depth.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/layout-valid-guards.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/layout-valid-growth.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/wide-store-seal.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/protection-span.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/lower-txn-protection.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/lower-txn-large.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/bootstrap-wide-memory-src.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/extent-product-test.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" tools/hb-build-direct-lints-test.f" construct case-kind positive 0 s" " s" " RUN-CASE
   s" test/candidate-validation-test.f" construct case-kind positive 0 s" " s" " RUN-CASE ;

: TOP-ROW-CASE ( -- )
   s" test/top-row-hook-test.f" construct case-kind positive 0 s" " s" " RUN-CASE ;

: FINISH ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   FAILS @ 0 > if
      FAILS @ N. s"  candidate-validation failures" 1 die
   then
   s" candidate-validation: " type mode modeu type s"  ok" type cr ;

: RUN-SHARED ( -- )
   MARK
   s" shared" MANIFEST
   SHARED-CASES
   s" shared" FINISH ;

: RUN-TOP-ROW ( -- )
   MARK
   s" top-row" MANIFEST
   TOP-ROW-CASE
   s" top-row" FINISH ;

: USAGE ( -- )
   s" usage: candidate-validation.f -- shared|top-row" 64 die ;

public

: MAIN ( -- )
   CHECK-TARGET
   0 FAILS !
   SCRIPT-ARGC 1 <> if USAGE then
   0 SCRIPT-ARGV$ s" shared" STR= if RUN-SHARED exit then
   0 SCRIPT-ARGV$ s" top-row" STR= if RUN-TOP-ROW exit then
   USAGE ;

;package

CANDIDATE-VALIDATION:MAIN
