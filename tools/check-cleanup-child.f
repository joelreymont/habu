\ check-cleanup-child.f - CHECK cleanup-failure production-path fixture.
\ Run only from tools/check-test-lib.f with a test-owned TMPDIR.

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/source.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/diag-origin-core.f
require tools/json.f
require tools/json-only-core.f
require tools/signature-lint-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/trust-lint-core.f
require tools/check-all-errors-core.f
require lib/argv.f
require tools/check-core.f

package CHECK-CLEANUP-CHILD

private

5 constant PRIMARY-RC
$1000 constant PROV-OUT-CAP
\ Mode 0500 preserves search but denies the write needed by the final rmdir.

create PROV-PATH FS-PATH-CAP allot
create PROV-OUT PROV-OUT-CAP allot

variable PROV-U

: SEARCH-ONLY-DIR-MODE$ ( -- ptr u8 n )
   s" $140" ;

: TMP$ ( -- ptr u8 n )
   s" TMPDIR" GETENV dup 0= if 2drop E-FS-PATH throw then ;

: PROV$ ( -- ptr u8 n )
   PROV-PATH PROV-U @ ;

: PROV-PREPARE ( -- )
   TMP$ s" boundary.f" PROV-PATH JOIN-PATH PROV-U !
   PROV$ s" 0 set-check" WRITE-ALL ;

: PROV-CLEAN ( -- )
   [: CHECKED-BOUNDARY-LINT:FINISH ;] catch 0 T= ;

: PROV-DIRTY ( -- )
   CHECKED-BOUNDARY-LINT:RESET
   LINT-FALSE CHECKED-BOUNDARY-LINT:JSON!
   LINT-TRUE CHECKED-BOUNDARY-LINT:STRICT!
   2 >FD CHECKED-BOUNDARY-LINT:OUT-FD!
   PROV-OUT PROV-OUT-CAP LINT-OUT-BUFFER!
   PROV$ CHECKED-BOUNDARY-LINT:FILE
   LINT-OUT-BUFFER-OFF
   [: CHECKED-BOUNDARY-LINT:FINISH ;] catch 1 T= ;

: CHECK-THROW ( -- )
   CHECK:RUN dup 0 <> if throw then drop ;

: TEST-PROV-NORMAL ( -- )
   CHECK:RESET
   PROV-DIRTY
   s" " s" provider-normal.f" CHECK:SOURCE
   CHECK:RUN 0 T=
   PROV-CLEAN ;

: TEST-PROV-THROW ( -- )
   CHECK:RESET
   s" 0 set-check" s" provider-throw.f" CHECK:SOURCE
   PROV-OUT PROV-OUT-CAP LINT-OUT-BUFFER!
   [: CHECK-THROW ;] catch {: rc:n :}
   LINT-OUT$ s" CHECKER-MUTATION" CONTAINS? TTRUE
   LINT-OUT-BUFFER-OFF
   rc 1 T=
   PROV-CLEAN
   CHECK:RESET
   PROV-CLEAN ;

: TEST-PROV-RESET ( -- )
   PROV-DIRTY
   CHECK:RESET
   PROV-CLEAN ;

: PROV-CLEANUP ( -- )
   PROV$ REMOVE-FILE ;

: SOURCE-LF ( -- )
   $0A SB-APPEND-C ;

: SOURCE-DQ ( -- )
   $22 SB-APPEND-C ;

: SOURCE-TMP$ ( -- )
   s" s" SB-APPEND SOURCE-DQ
   $20 SB-APPEND-C TMP$ SB-APPEND SOURCE-DQ ;

: SOURCE-BUILD ( bool -- ptr u8 n ) {: primary:bool :}
   SB-RESET
   s" require lib/fs-mutate.f" SB-APPEND SOURCE-LF
   s" package CHECK-CLEANUP-SUBJECT" SB-APPEND SOURCE-LF
   s" : LOCK-TMP ( -- ) " SB-APPEND SOURCE-TMP$
   $20 SB-APPEND-C SEARCH-ONLY-DIR-MODE$ SB-APPEND
   s"  CHMOD-MODE" SB-APPEND
   primary if
      s"  s" SB-APPEND SOURCE-DQ
      s"  cleanup primary" SB-APPEND SOURCE-DQ
      s"  5 die" SB-APPEND
   then
   s"  ;" SB-APPEND SOURCE-LF
   s" LOCK-TMP" SB-APPEND SOURCE-LF
   s" ;package" SB-APPEND SOURCE-LF
   SB$ ;

: MODE$ ( -- ptr u8 n )
   SCRIPT-ARGC 1 <> if E-STR-BOUNDS throw then
   0 SCRIPT-ARGV$ ;

: EXPECTED-RC ( -- n )
   MODE$ s" cleanup" STR= if E-FS-IO exit then
   MODE$ s" primary" STR= if PRIMARY-RC exit then
   E-STR-BOUNDS throw ;

: SOURCE$ ( -- ptr u8 n )
   MODE$ s" cleanup" STR= if LINT-FALSE SOURCE-BUILD exit then
   MODE$ s" primary" STR= if LINT-TRUE SOURCE-BUILD exit then
   E-STR-BOUNDS throw ;

: RESTORE-TMP ( -- )
   TMP$ FS-MUT-MODE-PRIVATE-DIR CHMOD-MODE ;

: REMOVE-TMP ( -- )
   TMP$ REMOVE-DIR ;

: SELECT-PROBE ( -- )
   s" " s" selection-probe.f" CHECK:SOURCE ;

: TEST-CLEANUP ( -- )
   CHECK:RESET
   SOURCE$ s" cleanup-source.f" CHECK:SOURCE
   CHECK:RUN EXPECTED-RC T=
   RESTORE-TMP
   [: REMOVE-TMP ;] E-FS-IO TTHROWSQ
   [: SELECT-PROBE ;] catch 0 T=
   CHECK:RESET
   REMOVE-TMP
   TMP$ EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   PROV-PREPARE
   TEST-PROV-NORMAL
   TEST-PROV-THROW
   TEST-PROV-RESET
   PROV-CLEANUP
   TEST-CLEANUP
   T-REPORT ;

;package

CHECK-CLEANUP-CHILD:RUN
