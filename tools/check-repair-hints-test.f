\ check-repair-hints-test.f - checked fixtures for repair-class diagnostics.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/check-repair-hints-test.f

$4000 constant CRHT-BUF-CAP
5000 constant CRHT-TIMEOUT-MS

variable CRHT-ROOT-U
variable CRHT-SRC-U
variable CRHT-DIAG-U

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

: CRHT-LF ( -- )
   10 SB-APPEND-C ;

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

: CRHT-REMOVE-PRODUCER$ ( -- ptr u8 n )
   s" : DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup ;" CRHT-LINE$ ;

: CRHT-ADD-PRODUCER$ ( -- ptr u8 n )
   s" : DIAG-ADD-PRODUCER ( i64 -- i64 ) drop ;" CRHT-LINE$ ;

: CRHT-FIX-TYPE$ ( -- ptr u8 n )
   s" : DIAG-FIX-TYPE ( i64 -- i64 ) 0= ;" CRHT-LINE$ ;

: CRHT-FIX-RSTACK$ ( -- ptr u8 n )
   s" : DIAG-FIX-RSTACK ( i64 -- ) >r ;" CRHT-LINE$ ;

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
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/lint/json-writer.f" PROC-ARGV+
   s" tools/lint/source-lex.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check-all-errors.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" --label" PROC-ARGV+
   label labelu PROC-ARGV+
   CRHT-SRC PROC-ARGV+ ;

: CRHT-RUN-CHECK ( ptr u8 n -- n n n )
   CRHT-CHECK-ARGS
   s" bin/hb" CRHT-OUT CRHT-BUF-CAP CRHT-ERR CRHT-BUF-CAP CRHT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: CRHT-ASSERT-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/gate-json-assert.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: CRHT-RUN-SCHEMA ( -- n n n )
   CRHT-ASSERT-ARGS
   s" json-one-schema" PROC-ARGV+
   CRHT-DIAG PROC-ARGV+
   s" bin/hb" CRHT-OUT CRHT-BUF-CAP CRHT-ERR CRHT-BUF-CAP CRHT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: CRHT-RUN-CLASS ( ptr u8 n -- n n n ) {: class:ptr classu :}
   CRHT-ASSERT-ARGS
   s" diag-repair-class" PROC-ARGV+
   CRHT-DIAG PROC-ARGV+
   class classu PROC-ARGV+
   s" bin/hb" CRHT-OUT CRHT-BUF-CAP CRHT-ERR CRHT-BUF-CAP CRHT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: CRHT-ASSERT-CLEAN ( n n n -- )
   0 T=
   {: outu erru :}
   CRHT-OUT outu CRHT-EMPTY$ T$=
   CRHT-ERR erru CRHT-EMPTY$ T$= ;

: CRHT-CHECK-HINT ( ptr u8 n ptr u8 n ptr u8 n -- ) {: label:ptr labelu class:ptr classu body:ptr bodyu :}
   CRHT-SRC body bodyu WRITE-ALL
   label labelu CRHT-RUN-CHECK 70 T=
   {: outu erru :}
   CRHT-OUT outu CRHT-EMPTY$ T$=
   CRHT-DIAG CRHT-ERR erru WRITE-ALL
   CRHT-RUN-SCHEMA CRHT-ASSERT-CLEAN
   class classu CRHT-RUN-CLASS CRHT-ASSERT-CLEAN ;

: CRHT-CASES ( -- )
   s" remove-producer" s" remove_producer" CRHT-REMOVE-PRODUCER$ CRHT-CHECK-HINT
   s" add-producer" s" add_producer" CRHT-ADD-PRODUCER$ CRHT-CHECK-HINT
   s" fix-type" s" fix_type" CRHT-FIX-TYPE$ CRHT-CHECK-HINT
   s" fix-return-stack" s" fix_return_stack" CRHT-FIX-RSTACK$ CRHT-CHECK-HINT
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
