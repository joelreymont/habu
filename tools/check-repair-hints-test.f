\ check-repair-hints-test.f - repair-class diagnostics no other row checks: a
\ definition too deep to check (rewrite_uncheckable), a row-typed dup mismatch,
\ a data-stack mismatch that outranks a return-stack leak, and a quotation's
\ own return-stack leak.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f
\ src/lib/vector.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ src/lib/source-lex.f
\ tools/check-all-errors-core.f tools/cli-run.f
\ tools/gate-json-assert-core.f tools/check-repair-hints-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require tools/check-all-errors-core.f
require tools/cli-run.f
require tools/gate-json-assert-core.f
require src/lib/fmt.f                        \ FMT:.INT - one-line number text

package CHECK-REPAIR-HINTS-TEST

private

$4000 constant BUF-CAP
10 constant LF-C
34 constant DEEP-IF-N

variable ROOT-U
variable SRC-U
variable DIAG-U
TYPED-VARIABLE LABEL-A ptr u8
variable LABEL-U
TYPED-VARIABLE PHASE-A ptr u8
variable PHASE-U

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create DIAG-BUF FS-PATH-CAP allot
create OUT BUF-CAP allot
create ERR BUF-CAP allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: DIAG ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

: LABEL$ ( -- ptr u8 n )
   LABEL-A @ LABEL-U @ ;

: PHASE$ ( -- ptr u8 n )
   PHASE-A @ PHASE-U @ ;

: CONTEXT! ( ptr u8 n ptr u8 n -- )
   PHASE-U !
   PHASE-A !
   LABEL-U !
   LABEL-A ! ;

: LF ( -- )
   LF-C SB-APPEND-C ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: LINE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   a u SB-APPEND
   LF
   SB$ ;

: SRC-CLEAR ( -- )
   SRC EMPTY$ WRITE-ALL ;

: SRC+ ( ptr u8 n -- )
   SRC 2swap APPEND-FILE ;

\ A quotation literal pushes a cell and returns: the return rows named are the
\ quotation's own, not the definition's around it.
: QUOT-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-QUOT-RSTACK ( i64 -- i64 ) [: >r ;] execute r> ;" LINE$ ;

\ The data row fails first, at `;`, so it names the repair, not the return row.
: MIXED-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-MIXED-RSTACK ( i64 -- ) dup >r ;" LINE$ ;

: ROW-DUP-EXTRA$ ( -- ptr u8 n )
   s" : DIAG-ROW-DUP-EXTRA ( R x -- R x ) dup ;" LINE$ ;

: DEEP-IFS ( -- )
   DEEP-IF-N 0 ?do s" 0 0= if " SB-APPEND loop ;

: DEEP-THENS ( -- )
   DEEP-IF-N 0 ?do s" then " SB-APPEND loop ;

: REWRITE-UNCHECKABLE$ ( -- ptr u8 n )
   SB-RESET
   s" : DIAG-REWRITE-UNCHECKABLE ( -- ) " SB-APPEND
   DEEP-IFS
   DEEP-THENS
   s" ;" SB-APPEND
   LF
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-repair-hints" TMPDIR-MKDIR {: a:ptr u :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-DIR+
   ROOT s" input.f" SRC-BUF SRC-U PATH!
   ROOT s" diag.jsonl" DIAG-BUF DIAG-U PATH!
   SRC CLEANUP+
   DIAG CLEANUP+ ;

: RUN-CHECK-ACT ( -- )
   LABEL$ SRC CHECK-ALL-ERRORS:FILE ;

: RUN-CHECK ( ptr u8 n -- n n n )
   2drop
   ERR BUF-CAP OUT BUF-CAP CHECK-ALL-ERRORS:BUFFERS!
   0 0= CHECK-ALL-ERRORS:JSON!
   [: RUN-CHECK-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS:OUT$ nip rc ;

: RC-NAME. ( n -- ) {: rc:n :}
   rc 60 = if s" E-PROC-SPAWN" type exit then
   rc 59 = if s" E-PROC-WAIT" type exit then
   rc 58 = if s" E-PROC-TIMEOUT" type exit then
   rc 57 = if s" E-PROC-OUTPUT" type exit then
   rc 56 = if s" E-PROC-TRUNCATED" type exit then
   rc 55 = if s" E-PROC-ENV" type exit then
   rc 54 = if s" E-PROC-PATH" type exit then
   rc 202 = if s" E-FS-OPEN" type exit then
   rc 198 = if s" E-FS-CAPACITY" type exit then
   rc 104 = if s" E-STR-BOUNDS" type exit then
   rc 103 = if s" E-STR-CAPACITY" type exit then
   s" unmapped" type ;

: DUMP-CAPTURE ( n n n n -- )
   {: outu:n erru:n code:n expect:n :}
   s" check-repair-hints boundary failure" type cr
   s" case: " type LABEL$ type cr
   s" phase: " type PHASE$ type cr
   s" exe: " type CLI-TOOLS$ type cr
   s" source: " type SRC type cr
   s" diag: " type DIAG type cr
   s" expected exit: " type expect FMT:.INT cr
   s" code: " type code dup FMT:.INT
   s"  (" type RC-NAME. s" )" type cr
   s" stdout bytes: " type outu FMT:.INT s"  / " type BUF-CAP FMT:.INT cr
   s" stderr bytes: " type erru FMT:.INT s"  / " type BUF-CAP FMT:.INT cr
   s" stdout:" type cr
   OUT outu type
   s" stderr:" type cr
   ERR erru type ;

: EXPECT-OUTCOME ( n n n n -- )
   {: outu:n erru:n code:n expect:n :}
   code expect <> if
      outu erru code expect DUMP-CAPTURE
   then
   code expect T= ;

: EXPECT-DIAG ( n n n n -- n )
   {: outu:n erru:n code:n expect:n :}
   outu erru code expect EXPECT-OUTCOME
   outu 0 <> if outu erru code expect DUMP-CAPTURE then
   OUT outu EMPTY$ T$=
   erru ;

: BAD-BATCH-SOURCE ( -- )
   SRC-CLEAR
   QUOT-RSTACK$ SRC+
   MIXED-RSTACK$ SRC+
   ROW-DUP-EXTRA$ SRC+
   REWRITE-UNCHECKABLE$ SRC+ ;

: RUN-BAD-BATCH ( -- )
   BAD-BATCH-SOURCE
   s" repair-batch" s" check" CONTEXT!
   s" repair-batch" RUN-CHECK 70 EXPECT-DIAG {: erru:n :}
   DIAG ERR erru WRITE-ALL
   s" repair-batch" s" schema" CONTEXT!
   DIAG GJA-JSON-LINES-SCHEMA ;

: ASSERT-WORD-CLASS ( ptr u8 n ptr u8 n -- ) {: word:ptr wordu:n class:ptr classu:n :}
   word wordu s" class" CONTEXT!
   DIAG word wordu class classu GJA-DIAG-WORD-REPAIR-CLASS ;

: ASSERT-WORD-RSTACK ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: word:ptr wordu exp:ptr expu act:ptr actu :}
   word wordu s" return-stack" CONTEXT!
   DIAG word wordu exp expu act actu GJA-DIAG-WORD-RETURN-STACK ;

: ASSERT-WORD-ROW-EFFECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: word:ptr wordu src:ptr srcu exp:ptr expu act:ptr actu class:ptr classu :}
   word wordu s" row-effect" CONTEXT!
   DIAG word wordu src srcu exp expu act actu class classu GJA-DIAG-WORD-ROW-EFFECT ;

: BAD-BATCH-ASSERTS ( -- )
   s" diag-quot-rstack" EMPTY$ s" i64 " ASSERT-WORD-RSTACK
   s" diag-mixed-rstack" s" remove_producer" ASSERT-WORD-CLASS
   s" diag-mixed-rstack" s" i64 --" EMPTY$ s" i64 " s" remove_producer" ASSERT-WORD-ROW-EFFECT
   s" diag-row-dup-extra" s" R x -- R x" s" a " s" a a " s" remove_producer" ASSERT-WORD-ROW-EFFECT
   s" diag-rewrite-uncheckable" s" rewrite_uncheckable" ASSERT-WORD-CLASS ;

: CASES ( -- )
   RUN-BAD-BATCH
   BAD-BATCH-ASSERTS ;

: RUN ( -- )
   T-RESET
   PREPARE
   CASES
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" check-repair-hints-test: ok" type cr ;

RUN

;package
