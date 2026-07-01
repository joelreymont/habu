\ check-repair-hints-test.f - checked fixtures for repair-class diagnostics.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/check-all-errors-core.f tools/cli-run.f tools/json.f
\ tools/gate-json-assert-core.f tools/check-repair-hints-test.f

$4000 constant CRHT-BUF-CAP
10 constant CRHT-LF-C
34 constant CRHT-DEEP-IF-N

variable CRHT-ROOT-U
variable CRHT-SRC-U
variable CRHT-DIAG-U
variable CRHT-LABEL-A
variable CRHT-LABEL-U
variable CRHT-PHASE-A
variable CRHT-PHASE-U

create CRHT-ROOT-BUF FS-PATH-CAP allot
create CRHT-SRC-BUF FS-PATH-CAP allot
create CRHT-DIAG-BUF FS-PATH-CAP allot
create CRHT-OUT CRHT-BUF-CAP allot
create CRHT-ERR CRHT-BUF-CAP allot

: CRHT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CRHT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: CRHT-ROOT ( -- ptr u8 n )
   CRHT-ROOT-BUF CRHT-ROOT-U @ ;

: CRHT-SRC ( -- ptr u8 n )
   CRHT-SRC-BUF CRHT-SRC-U @ ;

: CRHT-DIAG ( -- ptr u8 n )
   CRHT-DIAG-BUF CRHT-DIAG-U @ ;

: CRHT-LABEL$ ( -- ptr u8 n )
   CRHT-LABEL-A @ CRHT-LABEL-U @ ;

: CRHT-PHASE$ ( -- ptr u8 n )
   CRHT-PHASE-A @ CRHT-PHASE-U @ ;

: CRHT-CONTEXT! ( ptr u8 n ptr u8 n -- )
   CRHT-PHASE-U !
   CRHT-PHASE-A !
   CRHT-LABEL-U !
   CRHT-LABEL-A ! ;

: CRHT-LF ( -- )
   CRHT-LF-C SB-APPEND-C ;

: CRHT-DQ ( -- )
   34 SB-APPEND-C ;

: CRHT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CRHT-LINE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   a u SB-APPEND
   CRHT-LF
   SB$ ;

: CRHT-SRC-CLEAR ( -- )
   CRHT-SRC CRHT-EMPTY$ WRITE-ALL ;

: CRHT-SRC+ ( ptr u8 n -- )
   CRHT-SRC 2swap APPEND-FILE ;

: CRHT-REMOVE-PRODUCER$ ( -- ptr u8 n )
   s" : DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup ;" CRHT-LINE$ ;

: CRHT-ADD-PRODUCER$ ( -- ptr u8 n )
   s" : DIAG-ADD-PRODUCER ( i64 -- i64 ) drop ;" CRHT-LINE$ ;

: CRHT-FIX-TYPE$ ( -- ptr u8 n )
   s" : DIAG-FIX-TYPE ( i64 -- i64 ) 0= ;" CRHT-LINE$ ;

: CRHT-FIX-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-FIX-RSTACK ( i64 -- ) >r ;" CRHT-LINE$ ;

: CRHT-MIXED-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-MIXED-RSTACK ( i64 -- ) dup >r ;" CRHT-LINE$ ;

: CRHT-BALANCED-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-BALANCED-RSTACK ( i64 -- ) >r r> drop ;" CRHT-LINE$ ;

: CRHT-ROW-DUP-EXTRA$ ( -- ptr u8 n )
   s" : DIAG-ROW-DUP-EXTRA ( R x -- R x ) dup ;" CRHT-LINE$ ;

: CRHT-ROW-DUP-OK$ ( -- ptr u8 n )
   s" : DIAG-ROW-DUP-OK ( R x -- R x x ) dup ;" CRHT-LINE$ ;

: CRHT-TRUSTED-EVAL$ ( -- ptr u8 n )
   s" : DIAG-TRUSTED-BOUNDARY ( -- i64 ) evaluate ;" CRHT-LINE$ ;

: CRHT-TRUSTED-TRUST$ ( -- ptr u8 n )
   SB-RESET
   s" : DIAG-TRUSTED-BOUNDARY-TRUST ( -- i64 ) s" SB-APPEND CRHT-DQ
   s"  HIDDEN" SB-APPEND CRHT-DQ
   s"  s" SB-APPEND CRHT-DQ
   s"  -- i64" SB-APPEND CRHT-DQ
   s"  TRUST 42 ;" SB-APPEND
   CRHT-LF
   SB$ ;

: CRHT-TRUSTED-SET-CHECK$ ( -- ptr u8 n )
   s" : DIAG-TRUSTED-BOUNDARY-SET-CHECK ( -- i64 ) 0 set-check 42 ;" CRHT-LINE$ ;

: CRHT-SIGNATURE-SYNTAX$ ( -- ptr u8 n )
   s" : DIAG-SIGNATURE-SYNTAX ( i64 ) 1 + ;" CRHT-LINE$ ;

: CRHT-DEEP-IFS ( -- )
   CRHT-DEEP-IF-N 0 ?do s" 0 0= if " SB-APPEND loop ;

: CRHT-DEEP-THENS ( -- )
   CRHT-DEEP-IF-N 0 ?do s" then " SB-APPEND loop ;

: CRHT-REWRITE-UNCHECKABLE$ ( -- ptr u8 n )
   SB-RESET
   s" : DIAG-REWRITE-UNCHECKABLE ( -- ) " SB-APPEND
   CRHT-DEEP-IFS
   CRHT-DEEP-THENS
   s" ;" SB-APPEND
   CRHT-LF
   SB$ ;

: CRHT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-repair-hints" TMPDIR-MKDIR {: a:ptr u :}
   a u CRHT-ROOT-BUF CRHT-ROOT-U CRHT-COPY!
   CRHT-ROOT CLEANUP-DIR+
   CRHT-ROOT s" input.f" CRHT-SRC-BUF CRHT-SRC-U CRHT-PATH!
   CRHT-ROOT s" diag.jsonl" CRHT-DIAG-BUF CRHT-DIAG-U CRHT-PATH!
   CRHT-SRC CLEANUP+
   CRHT-DIAG CLEANUP+ ;

: CRHT-RUN-CHECK-ACT ( -- )
   CRHT-LABEL$ CRHT-SRC CHECK-ALL-ERRORS-FILE ;

: CRHT-RUN-CHECK ( ptr u8 n -- n n n n )
   2drop
   CRHT-ERR CRHT-BUF-CAP CRHT-OUT CRHT-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CRHT-RUN-CHECK-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip PROC-OUTCOME-EXIT rc ;

: CRHT-OUTCOME. ( n -- ) {: kind :}
   kind PROC-OUTCOME-EXIT = if s" exit" type exit then
   kind PROC-OUTCOME-SIGNAL = if s" signal" type exit then
   kind PROC-OUTCOME-TIMEOUT = if s" timeout" type exit then
   s" unknown" type ;

: CRHT-RC-NAME. ( n -- ) {: rc :}
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

: CRHT-DUMP-CAPTURE ( n n n n n -- )
   {: outu erru kind code expect :}
   s" check-repair-hints boundary failure" type cr
   s" case: " type CRHT-LABEL$ type cr
   s" phase: " type CRHT-PHASE$ type cr
   s" exe: " type CLI-TOOLS$ type cr
   s" source: " type CRHT-SRC type cr
   s" diag: " type CRHT-DIAG type cr
   s" expected exit: " type expect . cr
   s" outcome: " type kind CRHT-OUTCOME.
   s"  code: " type code . cr
   s" rc: " type kind code PROC-OUTCOME>RC RC>N dup .
   s" (" type CRHT-RC-NAME. s" )" type cr
   s" stdout bytes: " type outu . s" / " type CRHT-BUF-CAP . cr
   s" stderr bytes: " type erru . s" / " type CRHT-BUF-CAP . cr
   s" stdout:" type cr
   CRHT-OUT outu type
   s" stderr:" type cr
   CRHT-ERR erru type ;

: CRHT-EXPECT-OUTCOME ( n n n n n -- )
   {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT <> if
      outu erru kind code expect CRHT-DUMP-CAPTURE
   then
   code expect <> if
      outu erru kind code expect CRHT-DUMP-CAPTURE
   then
   kind PROC-OUTCOME-EXIT T=
   code expect T= ;

: CRHT-EXPECT-CLEAN ( n n n n n -- )
   {: outu erru kind code expect :}
   outu erru kind code expect CRHT-EXPECT-OUTCOME
   outu 0 <> if outu erru kind code expect CRHT-DUMP-CAPTURE then
   erru 0 <> if outu erru kind code expect CRHT-DUMP-CAPTURE then
   CRHT-OUT outu CRHT-EMPTY$ T$=
   CRHT-ERR erru CRHT-EMPTY$ T$= ;

: CRHT-EXPECT-DIAG ( n n n n n -- n )
   {: outu erru kind code expect :}
   outu erru kind code expect CRHT-EXPECT-OUTCOME
   outu 0 <> if outu erru kind code expect CRHT-DUMP-CAPTURE then
   CRHT-OUT outu CRHT-EMPTY$ T$=
   erru ;

: CRHT-CHECK-ACCEPTS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu body:ptr bodyu :}
   CRHT-SRC body bodyu WRITE-ALL
   label labelu s" check" CRHT-CONTEXT!
   label labelu CRHT-RUN-CHECK 0 CRHT-EXPECT-CLEAN ;

: CRHT-BAD-BATCH-SOURCE ( -- )
   CRHT-SRC-CLEAR
   CRHT-REMOVE-PRODUCER$ CRHT-SRC+
   CRHT-ADD-PRODUCER$ CRHT-SRC+
   CRHT-FIX-TYPE$ CRHT-SRC+
   CRHT-FIX-RSTACK$ CRHT-SRC+
   CRHT-MIXED-RSTACK$ CRHT-SRC+
   CRHT-ROW-DUP-EXTRA$ CRHT-SRC+
   CRHT-TRUSTED-EVAL$ CRHT-SRC+
   CRHT-TRUSTED-TRUST$ CRHT-SRC+
   CRHT-TRUSTED-SET-CHECK$ CRHT-SRC+
   CRHT-SIGNATURE-SYNTAX$ CRHT-SRC+
   CRHT-REWRITE-UNCHECKABLE$ CRHT-SRC+ ;

: CRHT-RUN-BAD-BATCH ( -- )
   CRHT-BAD-BATCH-SOURCE
   s" repair-batch" s" check" CRHT-CONTEXT!
   s" repair-batch" CRHT-RUN-CHECK 70 CRHT-EXPECT-DIAG {: erru :}
   CRHT-DIAG CRHT-ERR erru WRITE-ALL
   s" repair-batch" s" schema" CRHT-CONTEXT!
   CRHT-DIAG GJA-JSON-LINES-SCHEMA ;

: CRHT-ASSERT-WORD-CLASS ( ptr u8 n ptr u8 n -- ) {: word:ptr wordu class:ptr classu :}
   word wordu s" class" CRHT-CONTEXT!
   CRHT-DIAG word wordu class classu GJA-DIAG-WORD-REPAIR-CLASS ;

: CRHT-ASSERT-WORD-RSTACK ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: word:ptr wordu exp:ptr expu act:ptr actu :}
   word wordu s" return-stack" CRHT-CONTEXT!
   CRHT-DIAG word wordu exp expu act actu GJA-DIAG-WORD-RETURN-STACK ;

: CRHT-ASSERT-WORD-ROW-EFFECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: word:ptr wordu src:ptr srcu exp:ptr expu act:ptr actu class:ptr classu :}
   word wordu s" row-effect" CRHT-CONTEXT!
   CRHT-DIAG word wordu src srcu exp expu act actu class classu GJA-DIAG-WORD-ROW-EFFECT ;

: CRHT-BAD-BATCH-ASSERTS ( -- )
   s" diag-remove-producer" s" remove_producer" CRHT-ASSERT-WORD-CLASS
   s" diag-add-producer" s" add_producer" CRHT-ASSERT-WORD-CLASS
   s" diag-fix-type" s" fix_type" CRHT-ASSERT-WORD-CLASS
   s" diag-fix-rstack" s" fix_return_stack" CRHT-ASSERT-WORD-CLASS
   s" diag-fix-rstack" CRHT-EMPTY$ s" i64 " CRHT-ASSERT-WORD-RSTACK
   s" diag-mixed-rstack" s" fix_return_stack" CRHT-ASSERT-WORD-CLASS
   s" diag-mixed-rstack" CRHT-EMPTY$ s" i64 " CRHT-ASSERT-WORD-RSTACK
   s" diag-row-dup-extra" s" remove_producer" CRHT-ASSERT-WORD-CLASS
   s" diag-row-dup-extra" s" R x -- R x" s" a " s" a a " s" remove_producer" CRHT-ASSERT-WORD-ROW-EFFECT
   s" diag-trusted-boundary" s" trusted_boundary_required" CRHT-ASSERT-WORD-CLASS
   s" diag-trusted-boundary-trust" s" trusted_boundary_required" CRHT-ASSERT-WORD-CLASS
   s" diag-trusted-boundary-set-check" s" trusted_boundary_required" CRHT-ASSERT-WORD-CLASS
   s" diag-signature-syntax" s" fix_signature_syntax" CRHT-ASSERT-WORD-CLASS
   s" diag-rewrite-uncheckable" s" rewrite_uncheckable" CRHT-ASSERT-WORD-CLASS ;

: CRHT-CASES ( -- )
   CRHT-RUN-BAD-BATCH
   CRHT-BAD-BATCH-ASSERTS
   s" balanced-return-stack" CRHT-BALANCED-RSTACK$ CRHT-CHECK-ACCEPTS
   s" row-dup-ok" CRHT-ROW-DUP-OK$ CRHT-CHECK-ACCEPTS ;

: CRHT-MAIN ( -- )
   T-RESET
   CRHT-PREPARE
   CRHT-CASES
   CLEANUP-RUN
   CRHT-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-repair-hints-test: ok" type cr ;

CRHT-MAIN
