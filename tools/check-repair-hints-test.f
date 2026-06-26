\ check-repair-hints-test.f - checked fixtures for repair-class diagnostics.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f tools/json.f tools/gate-json-assert-core.f tools/check-repair-hints-test.f

$4000 constant CRHT-BUF-CAP
$4000 constant CRHT-ARGV-CAP
5000 constant CRHT-TIMEOUT-MS
10 constant CRHT-LF-C
32 constant CRHT-SP-C

variable CRHT-ROOT-U
variable CRHT-SRC-U
variable CRHT-DIAG-U
variable CRHT-ARGV-U
variable CRHT-LABEL-A
variable CRHT-LABEL-U
variable CRHT-PHASE-A
variable CRHT-PHASE-U

create CRHT-ROOT-BUF FS-PATH-CAP allot
create CRHT-SRC-BUF FS-PATH-CAP allot
create CRHT-DIAG-BUF FS-PATH-CAP allot
create CRHT-OUT CRHT-BUF-CAP allot
create CRHT-ERR CRHT-BUF-CAP allot
create CRHT-ARGV-BUF CRHT-ARGV-CAP allot

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

: CRHT-ARGV$ ( -- ptr u8 n )
   CRHT-ARGV-BUF CRHT-ARGV-U @ ;

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

: CRHT-ARGV-RESET ( -- )
   0 CRHT-ARGV-U !
   PROC-ARGV-RESET ;

: CRHT-ARGV-C ( n -- ) {: c :}
   CRHT-ARGV-U @ 1 + CRHT-ARGV-CAP > if E-STR-CAPACITY throw then
   c CRHT-ARGV-BUF CRHT-ARGV-U @ + c!
   CRHT-ARGV-U @ 1+ CRHT-ARGV-U ! ;

: CRHT-ARGV+ ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   CRHT-ARGV-U @ u + 3 + CRHT-ARGV-CAP > if E-STR-CAPACITY throw then
   CRHT-SP-C CRHT-ARGV-C
   CRHT-SP-C CRHT-ARGV-C
   a CRHT-ARGV-BUF CRHT-ARGV-U @ + u BYTE-COPY
   CRHT-ARGV-U @ u + CRHT-ARGV-U !
   CRHT-LF-C CRHT-ARGV-C ;

: CRHT-ARG+ ( ptr u8 n -- )
   2dup CRHT-ARGV+
   >LEN PROC-ARGV+ ;

: CRHT-WARM-LOAD ( ptr u8 n -- bool ) {: entry:ptr entryu :}
   WR-TOOLS? if
      s" --load" CRHT-ARG+
      s" HABU_WARM_TOOLS_TRUST" WR-TRUST$ CRHT-ARG+
      entry entryu CRHT-ARG+
      s" --" CRHT-ARG+
      WR-TRUE exit
   then
   WR-FALSE ;

: CRHT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CRHT-LINE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   a u SB-APPEND
   CRHT-LF
   SB$ ;

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

: CRHT-REWRITE-UNCHECKABLE$ ( -- ptr u8 n )
   s" : DIAG-REWRITE-UNCHECKABLE ( i64 -- i64 ) leave ;" CRHT-LINE$ ;

: CRHT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-repair-hints" TMPDIR-MKDIR {: a:ptr u :}
   a u CRHT-ROOT-BUF CRHT-ROOT-U CRHT-COPY!
   CRHT-ROOT CLEANUP-DIR+
   CRHT-ROOT s" input.f" CRHT-SRC-BUF CRHT-SRC-U CRHT-PATH!
   CRHT-ROOT s" diag.jsonl" CRHT-DIAG-BUF CRHT-DIAG-U CRHT-PATH!
   CRHT-SRC CLEANUP+
   CRHT-DIAG CLEANUP+ ;

: CRHT-CHECK-ARGS ( ptr u8 n -- ) {: label:ptr labelu :}
   CRHT-ARGV-RESET
   s" tools/check-all-errors.f" CRHT-WARM-LOAD if
      s" --json-errors" CRHT-ARG+
      s" --label" CRHT-ARG+
      label labelu CRHT-ARG+
      CRHT-SRC CRHT-ARG+
      exit
   then
   s" --load" CRHT-ARG+
   s" lib/errors.f" CRHT-ARG+
   s" lib/string.f" CRHT-ARG+
   s" lib/memory.f" CRHT-ARG+
   s" lib/vector.f" CRHT-ARG+
   s" lib/fs.f" CRHT-ARG+
   s" lib/process.f" CRHT-ARG+
   s" lib/process-argv.f" CRHT-ARG+
   s" tools/lint/text.f" CRHT-ARG+
   s" tools/lint/token.f" CRHT-ARG+
   s" tools/lint/lib.f" CRHT-ARG+
   s" tools/lint/json-writer.f" CRHT-ARG+
   s" tools/lint/source-lex.f" CRHT-ARG+
   s" tools/check-all-errors-core.f" CRHT-ARG+
   s" tools/argv.f" CRHT-ARG+
   s" tools/check-all-errors.f" CRHT-ARG+
   s" --" CRHT-ARG+
   s" --json-errors" CRHT-ARG+
   s" --label" CRHT-ARG+
   label labelu CRHT-ARG+
   CRHT-SRC CRHT-ARG+ ;

: CRHT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: CRHT-RUN-CHECK ( ptr u8 n -- n n n n )
   CRHT-CHECK-ARGS
   WR-TOOLS$ >LEN CRHT-OUT CRHT-BUF-CAP >LEN
   CRHT-ERR CRHT-BUF-CAP >LEN CRHT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME CRHT-CAPTURE>N ;

: CRHT-ASSERT-SCHEMA ( -- )
   CRHT-DIAG GJA-JSON-ONE-SCHEMA ;

: CRHT-ASSERT-CLASS ( ptr u8 n -- )
   CRHT-DIAG 2swap GJA-DIAG-REPAIR-CLASS ;

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
   s" exe: " type WR-TOOLS$ type cr
   s" source: " type CRHT-SRC type cr
   s" diag: " type CRHT-DIAG type cr
   s" expected exit: " type expect . cr
   s" outcome: " type kind CRHT-OUTCOME.
   s"  code: " type code . cr
   s" rc: " type kind code PROC-OUTCOME>RC RC>N dup .
   s" (" type CRHT-RC-NAME. s" )" type cr
   s" stdout bytes: " type outu . s" / " type CRHT-BUF-CAP . cr
   s" stderr bytes: " type erru . s" / " type CRHT-BUF-CAP . cr
   s" argv:" type cr
   CRHT-ARGV$ type
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

: CRHT-CHECK-HINT ( ptr u8 n ptr u8 n ptr u8 n -- ) {: label:ptr labelu class:ptr classu body:ptr bodyu :}
   CRHT-SRC body bodyu WRITE-ALL
   label labelu s" check" CRHT-CONTEXT!
   label labelu CRHT-RUN-CHECK 70 CRHT-EXPECT-DIAG {: erru :}
   CRHT-DIAG CRHT-ERR erru WRITE-ALL
   label labelu s" schema" CRHT-CONTEXT!
   CRHT-ASSERT-SCHEMA
   label labelu s" class" CRHT-CONTEXT!
   class classu CRHT-ASSERT-CLASS ;

: CRHT-ASSERT-RSTACK ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: exp:ptr expu act:ptr actu label:ptr labelu :}
   label labelu s" return-stack" CRHT-CONTEXT!
   CRHT-DIAG exp expu act actu GJA-DIAG-RETURN-STACK ;

: CRHT-ASSERT-ROW-EFFECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu exp:ptr expu act:ptr actu class:ptr classu :}
   src srcu s" row-effect" CRHT-CONTEXT!
   CRHT-DIAG src srcu exp expu act actu class classu GJA-DIAG-ROW-EFFECT ;

: CRHT-CHECK-ACCEPTS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu body:ptr bodyu :}
   CRHT-SRC body bodyu WRITE-ALL
   label labelu s" check" CRHT-CONTEXT!
   label labelu CRHT-RUN-CHECK 0 CRHT-EXPECT-CLEAN ;

: CRHT-CASES ( -- )
   s" remove-producer" s" remove_producer" CRHT-REMOVE-PRODUCER$ CRHT-CHECK-HINT
   s" add-producer" s" add_producer" CRHT-ADD-PRODUCER$ CRHT-CHECK-HINT
   s" fix-type" s" fix_type" CRHT-FIX-TYPE$ CRHT-CHECK-HINT
   s" fix-return-stack" s" fix_return_stack" CRHT-FIX-RSTACK$ CRHT-CHECK-HINT
   CRHT-EMPTY$ s" i64 " s" fix-return-stack" CRHT-ASSERT-RSTACK
   s" mixed-return-stack" s" fix_return_stack" CRHT-MIXED-RSTACK$ CRHT-CHECK-HINT
   CRHT-EMPTY$ s" i64 " s" mixed-return-stack" CRHT-ASSERT-RSTACK
   s" balanced-return-stack" CRHT-BALANCED-RSTACK$ CRHT-CHECK-ACCEPTS
   s" row-dup-extra" s" remove_producer" CRHT-ROW-DUP-EXTRA$ CRHT-CHECK-HINT
   s" R x -- R x" s" a " s" a a " s" remove_producer" CRHT-ASSERT-ROW-EFFECT
   s" row-dup-ok" CRHT-ROW-DUP-OK$ CRHT-CHECK-ACCEPTS
   s" trusted-boundary" s" trusted_boundary_required" CRHT-TRUSTED-EVAL$ CRHT-CHECK-HINT
   s" trusted-boundary-trust" s" trusted_boundary_required" CRHT-TRUSTED-TRUST$ CRHT-CHECK-HINT
   s" trusted-boundary-set-check" s" trusted_boundary_required" CRHT-TRUSTED-SET-CHECK$ CRHT-CHECK-HINT
   s" signature-syntax" s" fix_signature_syntax" CRHT-SIGNATURE-SYNTAX$ CRHT-CHECK-HINT
   s" rewrite-uncheckable" s" rewrite_uncheckable" CRHT-REWRITE-UNCHECKABLE$ CRHT-CHECK-HINT ;

: CRHT-MAIN ( -- )
   T-RESET
   CRHT-PREPARE
   CRHT-CASES
   CLEANUP-RUN
   CRHT-ROOT EXISTS? TFALSE
   T-REPORT
   s" check-repair-hints-test: ok" type cr ;

CRHT-MAIN
