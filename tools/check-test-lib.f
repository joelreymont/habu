\ check-test-lib.f - checked engine CLI/core smoke coverage library.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ lib/process-env.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/signature-lint-core.f tools/checked-boundary-lint-core.f
\ tools/reserved-name-lint-core.f
\ tools/trust-lint-core.f tools/check-all-errors-core.f tools/argv.f
\ tools/check-core.f tools/check-test.f

require tools/date.f
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
require tools/argv.f
require tools/check-core.f

$4000 constant CKT-BUF-CAP

create CKT-ROOT FS-PATH-CAP allot
create CKT-BAD-PATH FS-PATH-CAP allot
create CKT-LIST-PATH FS-PATH-CAP allot
create CKT-HB-BUF FS-PATH-CAP allot

variable CKT-OUT-A
variable CKT-ERR-A
variable CKT-ROOT-U
variable CKT-BAD-U
variable CKT-LIST-U
variable CKT-HB-U
variable CKT-START-NS

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-BAD$ ( -- ptr u8 n )
   CKT-BAD-PATH CKT-BAD-U @ ;

: CKT-LIST$ ( -- ptr u8 n )
   CKT-LIST-PATH CKT-LIST-U @ ;

: CKT-HB! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a CKT-HB-BUF u BYTE-COPY
   u CKT-HB-U ! ;

: CKT-HB-SET? ( -- bool )
   CKT-HB-U @ 0 > ;

: CKT-OUT-A-FIELD ( -- ptr ptr u8 )
   CKT-OUT-A 0 ptr-field ;

: CKT-ERR-A-FIELD ( -- ptr ptr u8 )
   CKT-ERR-A 0 ptr-field ;

: CKT-OUT-A@ ( -- ptr u8 )
   CKT-OUT-A-FIELD @ ;

: CKT-ERR-A@ ( -- ptr u8 )
   CKT-ERR-A-FIELD @ ;

: CKT-OUT-A! ( ptr u8 -- )
   CKT-OUT-A-FIELD ! ;

: CKT-ERR-A! ( ptr u8 -- )
   CKT-ERR-A-FIELD ! ;

: CKT-ALLOC-BUF ( -- ptr u8 )
   CKT-BUF-CAP MEM-ALLOC-BYTES drop ;

: CKT-OUT ( -- ptr u8 )
   CKT-OUT-A @ 0= if CKT-ALLOC-BUF CKT-OUT-A! then
   CKT-OUT-A@ ;

: CKT-ERR ( -- ptr u8 )
   CKT-ERR-A @ 0= if CKT-ALLOC-BUF CKT-ERR-A! then
   CKT-ERR-A@ ;

: CKT-CORE-ACT ( -- )
   CKT-BAD$ CKT-BAD$ CHECK-ALL-ERRORS-FILE ;

: CKT-CORE-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   CKT-BAD$ src srcu WRITE-ALL
   CKT-ERR CKT-BUF-CAP CKT-OUT CKT-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CKT-CORE-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip rc ;

: CKT-DIRECT-START ( -- )
   CHK-RESET-CFG
   CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CHK-CAPTURE-BUFFERS! ;

: CKT-DIRECT-END ( n -- n n n ) {: rc:n :}
   CHK-CAPTURE-OUT$ nip
   CHK-CAPTURE-ERR$ nip
   rc ;

: CKT-DIRECT-STDIN ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   CKT-DIRECT-START
   src srcu s" <stdin>" CHK-MATERIALIZE-BUF-AS
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-JSON-STDIN ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   CKT-DIRECT-START
   -1 CHK-JSON !
   src srcu s" <stdin>" CHK-MATERIALIZE-BUF-AS
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-SOURCE-LIST-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-MATERIALIZE-LIST-PATH
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-PREVERIFY-LIST-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-MATERIALIZE-LIST-PATH
   [: CHK-RUN-PREVERIFY ;] catch CKT-DIRECT-END ;

: CKT-DIRECT-PREVERIFY-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-ADD-POS
   CHK-MATERIALIZE
   [: CHK-RUN-PREVERIFY ;] catch CKT-DIRECT-END ;

: CKT-DIRECT-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-ADD-POS
   CHK-MATERIALIZE
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-BAD-FLAG ( -- n n n )
   CKT-DIRECT-START
   [: s" --bad-flag" CHK-PARSE-ONE ;] catch CKT-DIRECT-END ;

: CKT-CAPTURE>N ( len len rc -- n n n ) {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CKT-GOOD$ ( -- ptr u8 n )
   s" : CKT-OK ( i64 -- i64 ) dup * ;" ;

: CKT-BAD$SRC ( -- ptr u8 n )
   s" : CKT-BAD-WORD ( i64 -- i64 ) dup ;" ;

: CKT-FWDREF$ ( -- ptr u8 n )
   s" : CKT-FWDREF ( -- ) CKT-MISSING ;" ;

: CKT-HB$ ( -- ptr u8 n )
   CKT-HB-SET? if CKT-HB-BUF CKT-HB-U @ exit then
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: CKT-HB-LOAD-FWDREF ( -- n n n )
   CKT-BAD$ CKT-FWDREF$ WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CKT-BAD$ >LEN PROC-ARGV+
   CKT-HB$ >LEN CKT-OUT CKT-BUF-CAP >LEN
   CKT-ERR CKT-BUF-CAP >LEN $2710 >MS RUN-ARGV-CAPTURE
   CKT-CAPTURE>N ;

: CKT-DUP$SRC ( -- ptr u8 n )
   SB-RESET
   s" : CKT-DUP ( i64 -- i64 ) 1 + ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-DUP ( i64 -- i64 ) 2 + ;" SB-APPEND
   SB$ ;

: CKT-RESERVED$ ( -- ptr u8 n )
   s" variable I" ;

: CKT-UNDEFINED$SRC ( -- ptr u8 n )
   s" : CKT-MISS ( i64 -- i64 ) dup NOPE ;" ;

: CKT-VREC-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" DEFLINEAR own" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD box value a END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD hdl owner own raw ptr u8 END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-OWN-PASS ( own -- own ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-MAKE-POINT ( n n -- point ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TAKE-POINT ( point -- n n ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-COPY-POINT ( point -- point point ) over over ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-X ( point -- n ) drop ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-Y ( point -- n ) nip ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-X! ( n point -- point ) swap drop ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-Y! ( point n -- point ) >r drop r> ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-MAKE-BOX ( a -- box ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TAKE-BOX ( box -- a ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-HDL-PASS ( hdl -- hdl ) ;" SB-APPEND
   SB$ ;

: CKT-LINEAR-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" DEFLINEAR own" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-OWN-DUP ( own -- own own ) dup ;" SB-APPEND
   SB$ ;

: CKT-VREC-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD rect w n h n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-REC ( point -- rect ) ;" SB-APPEND
   SB$ ;

: CKT-VREC-PARTIAL$ ( -- ptr u8 n )
   SB-RESET
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-PARTIAL ( n -- point ) ;" SB-APPEND
   SB$ ;

: CKT-NOM-SCAN-BODY$ ( -- ptr u8 n )
   SB-RESET
   s" : DEFTYPE ( -- ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-NOM-BODY ( -- ) DEFTYPE ( -- ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" TRUSTED: CKT-NOM-TRUSTED ( -- ) DEFLINEAR ( -- ) ;" SB-APPEND
   SB$ ;

: CKT-SCAN-NOMINAL ( -- n )
   CHECKER-SCOPE-START
   [: CKT-LIST$ CHK-RUN-NOMINAL-FILE ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc ;

: CKT-RUN-SOURCE-LIST-RESERVED-CORE ( -- n n n )
   CKT-LIST$ CKT-RESERVED$ WRITE-ALL
   RESERVED-NAME-LINT-RESET
   CKT-ERR CKT-BUF-CAP LINT-OUT-BUFFER!
   CKT-LIST$ s" <source-list>" RESERVED-NAME-LINT-FILE-AS
   [: RESERVED-NAME-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip
   LINT-OUT-BUFFER-OFF
   0 swap rc ;

: CKT-DIE$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-BYE ( -- ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  bye" SB-APPEND
   $22 SB-APPEND-C
   s"  5 die ;" SB-APPEND
   $0a SB-APPEND-C
   s" CKT-BYE" SB-APPEND
   SB$ ;

: CKT-UNTERM-SDQ$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-UNTERM-SDQ ( -- ptr u8 n ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" bad.f" CKT-BAD-PATH JOIN-PATH CKT-BAD-U !
   CKT-ROOT$ s" local-test.f" CKT-LIST-PATH JOIN-PATH CKT-LIST-U !
   CKT-BAD$ CKT-BAD$SRC WRITE-ALL ;

: CKT-TEST-GOOD ( -- )
   [: CKT-GOOD$ VERIFY:SOURCE-BUF ;] catch 0 T= ;

: CKT-TEST-FILE-LABEL ( -- )
   CKT-BAD$SRC CKT-CORE-JSON 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru CKT-BAD$ CONTAINS? TTRUE ;

: CKT-TEST-USAGE ( -- )
   CKT-DIRECT-BAD-FLAG 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: CKT-TEST-DIE ( -- )
   CKT-DIE$ CKT-DIRECT-STDIN 5 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" bye" CONTAINS? TTRUE ;

: CKT-TEST-FWDREF-DIRECT ( -- )
   CKT-FWDREF$ CKT-DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CKT-ERR erru s" CKT-MISSING" CONTAINS? TTRUE ;

: CKT-TEST-FWDREF-JSON ( -- )
   CKT-FWDREF$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CKT-ERR erru s" CKT-MISSING" CONTAINS? TTRUE ;

: CKT-TEST-FWDREF-RAW-LOAD ( -- )
   CKT-HB-LOAD-FWDREF 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-UNDEFINED: CKT-MISSING" CONTAINS? TTRUE ;

: CKT-EXPECT-UNTERM-STRING ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu CKT-CORE-JSON 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-" CONTAINS? TTRUE ;

: CKT-TEST-UNTERM-STRING ( -- )
   CKT-UNTERM-SDQ$ CKT-EXPECT-UNTERM-STRING ;

: CKT-TEST-DUP-ALL ( -- )
   CKT-DUP$SRC CKT-CORE-JSON $4E T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   CKT-ERR erru s" duplicate-definition" CONTAINS? TTRUE ;

: CKT-TEST-SOURCE-LIST-RESERVED ( -- )
   CKT-RUN-SOURCE-LIST-RESERVED-CORE 1 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CKT-ERR erru s" <source-list>" CONTAINS? TTRUE ;

: CKT-TEST-SOURCE-LIST-AUDITED-LIB ( -- )
   s" lib/test.f" CKT-DIRECT-PREVERIFY-LIST-PATH 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-SOURCE-LIST-PREVERIFY-DIAG ( -- )
   CKT-LIST$ CKT-UNDEFINED$SRC WRITE-ALL
   CKT-LIST$ CKT-DIRECT-SOURCE-LIST-PATH 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" check.f: source preverify failed before run" CONTAINS? TTRUE
   CKT-ERR erru CKT-LIST$ CONTAINS? TTRUE
   CKT-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CKT-ERR erru s" NOPE" CONTAINS? TTRUE ;

: CKT-TEST-VALUE-RECORD-GOOD ( -- )
   CKT-VREC-GOOD$ CKT-DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-LINEAR-BAD ( -- )
   CKT-LINEAR-BAD$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-REJECTED" CONTAINS? TTRUE
   CKT-ERR erru s" dup" CONTAINS? TTRUE ;

: CKT-TEST-VALUE-RECORD-BAD ( -- )
   CKT-VREC-BAD$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CKT-ERR erru s" field<rect,w,n>" CONTAINS? TTRUE ;

: CKT-TEST-VALUE-RECORD-PARTIAL ( -- )
   CKT-VREC-PARTIAL$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CKT-ERR erru s" field<point,y,n>" CONTAINS? TTRUE ;

: CKT-TEST-NOMINAL-SCAN-TOP-LEVEL ( -- )
   CKT-LIST$ CKT-NOM-SCAN-BODY$ WRITE-ALL
   CKT-SCAN-NOMINAL 0 T= ;

: CKT-TEST-REQUIRE-FACADE ( -- )
   s" lib/test/suite-test.f" CKT-DIRECT-PREVERIFY-PATH 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   outu 0 T= ;

\ typed-local-lint: allow-bare-local - q is the test action quotation.
: CKT-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns CKT-START-NS !
   q execute
   s" PASS: " type label labelu type
   s"  (" type mono-ns CKT-START-NS @ - PROC-NS-PER-MS / . s" ms)" type cr ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-PREPARE
   s" check/good" [: CKT-TEST-GOOD ;] CKT-RUN
   s" check/file-label" [: CKT-TEST-FILE-LABEL ;] CKT-RUN
   s" check/usage-direct" [: CKT-TEST-USAGE ;] CKT-RUN
   s" check/die" [: CKT-TEST-DIE ;] CKT-RUN
   s" check/forward-ref-direct" [: CKT-TEST-FWDREF-DIRECT ;] CKT-RUN
   s" check/forward-ref-json" [: CKT-TEST-FWDREF-JSON ;] CKT-RUN
   s" check/forward-ref-raw-load" [: CKT-TEST-FWDREF-RAW-LOAD ;] CKT-RUN
   s" check/unterminated-string" [: CKT-TEST-UNTERM-STRING ;] CKT-RUN
   s" check/duplicate-all-errors" [: CKT-TEST-DUP-ALL ;] CKT-RUN
   s" check/source-list-reserved" [: CKT-TEST-SOURCE-LIST-RESERVED ;] CKT-RUN
   s" check/source-list-audited-lib" [: CKT-TEST-SOURCE-LIST-AUDITED-LIB ;] CKT-RUN
   s" check/source-list-preverify-diag" [: CKT-TEST-SOURCE-LIST-PREVERIFY-DIAG ;] CKT-RUN
   s" check/value-record-good" [: CKT-TEST-VALUE-RECORD-GOOD ;] CKT-RUN
   s" check/linear-bad" [: CKT-TEST-LINEAR-BAD ;] CKT-RUN
   s" check/value-record-bad" [: CKT-TEST-VALUE-RECORD-BAD ;] CKT-RUN
   s" check/value-record-partial" [: CKT-TEST-VALUE-RECORD-PARTIAL ;] CKT-RUN
   s" check/nominal-scan-top-level" [: CKT-TEST-NOMINAL-SCAN-TOP-LEVEL ;] CKT-RUN
   s" check/require-facade" [: CKT-TEST-REQUIRE-FACADE ;] CKT-RUN
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;
