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
create CKT-INC-DEP-PATH FS-PATH-CAP allot
create CKT-INC-ENTRY-PATH FS-PATH-CAP allot
create CKT-SUP-PATH FS-PATH-CAP allot
create CKT-USE-PATH FS-PATH-CAP allot
create CKT-HB-BUF FS-PATH-CAP allot

variable CKT-OUT-A
variable CKT-ERR-A
variable CKT-ROOT-U
variable CKT-BAD-U
variable CKT-LIST-U
variable CKT-INC-DEP-U
variable CKT-INC-ENTRY-U
variable CKT-SUP-U
variable CKT-USE-U
variable CKT-HB-U
variable CKT-START-NS
variable CKT-PAR-U

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-BAD$ ( -- ptr u8 n )
   CKT-BAD-PATH CKT-BAD-U @ ;

: CKT-LIST$ ( -- ptr u8 n )
   CKT-LIST-PATH CKT-LIST-U @ ;

: CKT-INC-DEP$ ( -- ptr u8 n )
   CKT-INC-DEP-PATH CKT-INC-DEP-U @ ;

: CKT-INC-ENTRY$ ( -- ptr u8 n )
   CKT-INC-ENTRY-PATH CKT-INC-ENTRY-U @ ;

: CKT-SUP$ ( -- ptr u8 n )
   CKT-SUP-PATH CKT-SUP-U @ ;

: CKT-USE$ ( -- ptr u8 n )
   CKT-USE-PATH CKT-USE-U @ ;

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

: CKT-DIRECT-ALL-LIST ( ptr u8 n ptr u8 n -- n n n ) {: aa:ptr au:n ba:ptr bu:n :}
   CKT-DIRECT-START
   -1 CHK-JSON !
   -1 CHK-ALL !
   -1 CHK-SOURCE-LIST !
   aa au CHK-ADD-POS
   ba bu CHK-ADD-POS
   CHK-MAKE-TEMP
   CHK-MATERIALIZE-LIST
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

: CKT-TFAM-GOOD$ ( -- ptr u8 n )   \ declarations + signature use, multi-line
   SB-RESET
   s" TYPEFAMILY tfck 1" SB-APPEND
   $0a SB-APPEND-C
   s" SUMTYPE rsck 2" SB-APPEND
   $0a SB-APPEND-C
   s"   VARIANT ok  a ;VARIANT" SB-APPEND
   $0a SB-APPEND-C
   s"   VARIANT err b ;VARIANT" SB-APPEND
   $0a SB-APPEND-C
   s" ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TF-PASS ( tfck<n> -- tfck<n> ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-RS-PASS ( rsck<n,f> -- rsck<n,f> ) ;" SB-APPEND
   SB$ ;

: CKT-TFAM-BAD$ ( -- ptr u8 n )    \ duplicate variant rejects fail-closed
   SB-RESET
   s" SUMTYPE rsbad 1 VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-BAD ( n -- n ) ;" SB-APPEND
   SB$ ;

\ S2: a declaration PRE-CHECK reject (missing arity / unterminated) must report
\ the same E-BAD-DECLARATION packet the native path emits, not a raw die.
: CKT-TFAM-NOARITY$ ( -- ptr u8 n )   \ TYPEFAMILY missing its arity token
   SB-RESET
   s" TYPEFAMILY ckfnoar" SB-APPEND
   SB$ ;

: CKT-SUM-NOEND$ ( -- ptr u8 n )      \ SUMTYPE body never reaches ;SUMTYPE
   SB-RESET
   s" SUMTYPE cksnoend 1 VARIANT ok a ;VARIANT" SB-APPEND
   SB$ ;

: CKT-ENUM-GOOD$ ( -- ptr u8 n )   \ item 14 gap: enum declaration + signature use
   SB-RESET
   s" ENUM eck red green ;ENUM" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-EN-PASS ( eck -- eck ) ;" SB-APPEND
   SB$ ;

: CKT-ENUM-BAD$ ( -- ptr u8 n )    \ duplicate enum variant rejects fail-closed
   SB-RESET
   s" ENUM ebad red red ;ENUM" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-EBAD ( n -- n ) ;" SB-APPEND
   SB$ ;

: CKT-PROD-GOOD$ ( -- ptr u8 n )   \ item 15: product declaration + signature use
   SB-RESET
   s" PRODUCT pck 0" SB-APPEND
   $0a SB-APPEND-C
   s"   FIELD x n" SB-APPEND
   $0a SB-APPEND-C
   s"   FIELD y n" SB-APPEND
   $0a SB-APPEND-C
   s" ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-PD-PASS ( pck -- pck ) ;" SB-APPEND
   SB$ ;

: CKT-PROD-BAD$ ( -- ptr u8 n )    \ duplicate product field rejects fail-closed
   SB-RESET
   s" PRODUCT pbad 1 FIELD x a FIELD x a ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-PBAD ( n -- n ) ;" SB-APPEND
   SB$ ;

: CKT-PROD-ALL$SRC ( -- ptr u8 n ) \ all-errors support replay registers products
   SB-RESET
   s" PRODUCT pae 0 FIELD v n ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-PAE-USE ( pae -- pae ) ;" SB-APPEND
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
   CKT-ROOT$ s" inc-dep.f" CKT-INC-DEP-PATH JOIN-PATH CKT-INC-DEP-U !
   CKT-ROOT$ s" inc-entry.f" CKT-INC-ENTRY-PATH JOIN-PATH CKT-INC-ENTRY-U !
   CKT-ROOT$ s" list-sup.f" CKT-SUP-PATH JOIN-PATH CKT-SUP-U !
   CKT-ROOT$ s" list-use.f" CKT-USE-PATH JOIN-PATH CKT-USE-U !
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

: CKT-TEST-TYPEFAMILY-GOOD ( -- )
   CKT-TFAM-GOOD$ CKT-DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-SUMTYPE-BAD ( -- )
   CKT-TFAM-BAD$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" duplicate variant" CONTAINS? TTRUE ;

: CKT-TEST-TFAM-NOARITY ( -- )   \ S2 parity: missing arity -> declaration packet
   CKT-TFAM-NOARITY$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" missing arity" CONTAINS? TTRUE ;

: CKT-TEST-SUM-NOEND ( -- )      \ S2 parity: unterminated sum -> declaration packet
   CKT-SUM-NOEND$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" missing ;SUMTYPE" CONTAINS? TTRUE ;

\ C2: a declaration body over TDECL-CAP ($1000) must report the same
\ E-BAD-DECLARATION packet (the length check fires ahead of variant parsing, so
\ the repeated variant names never matter).
create CKT-BIG $2000 allot   variable CKT-BIG-U
: CKT-BIG-C, ( n -- ) CKT-BIG CKT-BIG-U @ + c!  CKT-BIG-U @ 1+ CKT-BIG-U ! ;
: CKT-BIG-APP ( ptr u8 n -- ) {: a:ptr u:n :}  u 0 ?do a i + c@ CKT-BIG-C, loop ;
: CKT-OVERSIZE$ ( -- ptr u8 n )
   0 CKT-BIG-U !
   s" SUMTYPE ckbig 1 " CKT-BIG-APP
   200 0 ?do s" VARIANT vvvvvvvvvvvvvvvvvvvv n ;VARIANT " CKT-BIG-APP loop
   s" ;SUMTYPE" CKT-BIG-APP
   CKT-BIG CKT-BIG-U @ ;
: CKT-TEST-OVERSIZE ( -- )
   CKT-OVERSIZE$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" declaration too long" CONTAINS? TTRUE ;

: CKT-TEST-ENUM-GOOD ( -- )
   CKT-ENUM-GOOD$ CKT-DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-ENUM-BAD ( -- )
   CKT-ENUM-BAD$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" duplicate variant" CONTAINS? TTRUE ;

: CKT-TEST-PRODUCT-GOOD ( -- )
   CKT-PROD-GOOD$ CKT-DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-PRODUCT-BAD ( -- )
   CKT-PROD-BAD$ CKT-DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CKT-ERR erru s" duplicate field" CONTAINS? TTRUE ;

: CKT-TEST-PRODUCT-ALL-ERRORS ( -- )   \ verify-source support replay (RECORD-PRODUCT)
   CKT-PROD-ALL$SRC CKT-CORE-JSON 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

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

\ A dep loaded through a literal `s" path" included` is part of the closure:
\ the entry's use of the dep's word preverifies (the pre-producer scan captured
\ require/required only and rejected this good program).
: CKT-INC-ENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   $73 SB-APPEND-C $22 SB-APPEND-C $20 SB-APPEND-C
   CKT-INC-DEP$ SB-APPEND
   $22 SB-APPEND-C
   s"  included" SB-APPEND $0a SB-APPEND-C
   s" : CKT-USE-EIGHT ( -- n ) CKT-EIGHT ;" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: CKT-TEST-INCLUDED-DEP ( -- )
   CKT-INC-DEP$ s\" : CKT-EIGHT ( -- n ) 8 ;\n" WRITE-ALL
   CKT-INC-ENTRY$ CKT-INC-ENTRY-SRC$ WRITE-ALL
   CKT-INC-ENTRY$ CKT-DIRECT-PREVERIFY-PATH 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ `--all-errors --source-list a b` runs all-errors per ORIGINAL file with
\ prior entries replayed as support: both bad defs in b report against b's
\ path (the pre-redrive materialized temp had zero defs, so all-errors was a
\ no-op and only preverify's first error surfaced).
: CKT-TEST-SOURCE-LIST-ALL-ERRORS ( -- )
   CKT-SUP$ s\" : CKT-SEVEN ( -- n ) 7 ;\n" WRITE-ALL
   CKT-USE$ s\" : CKT-GOOD-USE ( -- n ) CKT-SEVEN ;\n: CKT-BAD-ONE ( -- n n ) CKT-SEVEN ;\n: CKT-BAD-TWO ( n -- ) CKT-SEVEN ;\n" WRITE-ALL
   CKT-SUP$ CKT-USE$ CKT-DIRECT-ALL-LIST 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru CKT-USE$ CONTAINS? TTRUE
   CKT-ERR erru s" ckt-bad-one" CONTAINS? TTRUE
   CKT-ERR erru s" ckt-bad-two" CONTAINS? TTRUE
   CKT-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE
   CKT-ERR erru s" ckt-good-use" CONTAINS? TFALSE
   CKT-ERR erru s" preverify" CONTAINS? TFALSE ;

\ Closure parity oracle: the retired token-level require/required scan
\ (the pre-producer CHK-SCAN-DEPS semantics) re-expressed here as an
\ independent check that the producer-backed closure is a superset of the old
\ closure on every entry file the gate exercises.
: CKT-PAR-WORD? ( n -- bool ) {: k:n :}
   k 0 < if LINT-FALSE exit then
   k L# @ >= if LINT-FALSE exit then
   k LK@ L-WORD = ;

: CKT-PAR=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CKT-PAR-WORD? 0= if LINT-FALSE exit then
   k LEX-TOK a u LINT-STR=CI ;

: CKT-PAR-SQUOTE? ( n -- bool ) {: k:n :}
   k CKT-PAR-WORD? 0= if LINT-FALSE exit then
   k LEX-TOK {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a c@ LINT-FOLD $73 <> if LINT-FALSE exit then
   a 1 + c@ $22 = ;

: CKT-PAR-WS? ( n -- bool )
   dup $20 = over 9 = or over $0A = or swap $0D = or ;

: CKT-PAR-SQ-SKIP ( n -- n )
   begin dup CKT-PAR-U @ < while
      CHK-EXP-BUF over + c@ CKT-PAR-WS? if 1+ else exit then
   repeat ;

: CKT-PAR-SQ-END ( n -- n )
   begin dup CKT-PAR-U @ < while
      CHK-EXP-BUF over + c@ $22 = if exit then
      1+
   repeat ;

: CKT-PAR-SQ-PATH$ ( n -- ptr u8 n bool ) {: k:n :}
   k CKT-PAR-SQUOTE? 0= if CHK-EXP-BUF 0 LINT-FALSE exit then
   k LB@ k LEX-TOK nip + CKT-PAR-SQ-SKIP {: start:n :}
   start CKT-PAR-SQ-END {: end:n :}
   end CKT-PAR-U @ >= if CHK-EXP-BUF 0 LINT-FALSE exit then
   CHK-EXP-BUF start + end start - LINT-TRUE ;

: CKT-PAR-IN-CLOSURE ( ptr u8 n -- )
   CHK-DEP-FIND 0 >= TTRUE ;

: CKT-PAR-REQUIRE ( n -- ) {: k:n :}
   k 1+ CKT-PAR-WORD? 0= if exit then
   k 1+ LEX-TOK CKT-PAR-IN-CLOSURE ;

: CKT-PAR-REQUIRED ( n -- ) {: k:n :}
   k 0 <= if exit then
   k 1- CKT-PAR-SQ-PATH$ if CKT-PAR-IN-CLOSURE else 2drop then ;

: CKT-PAR-ORACLE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHK-EXP-BUF CHK-SRC-CAP READ-ALL CKT-PAR-U !
   CHK-EXP-BUF CKT-PAR-U @ LEX-SOURCE
   0 begin dup L# @ < while
      dup s" require" CKT-PAR=CI if dup CKT-PAR-REQUIRE then
      dup s" required" CKT-PAR=CI if dup CKT-PAR-REQUIRED then
      1+
   repeat drop ;

: CKT-PAR-ENTRY ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-EXPAND-RESET
   a u CHK-EXPAND-PATH
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-DEP$ CKT-PAR-ORACLE-FILE
      1+
   repeat drop ;

: CKT-TEST-CLOSURE-PARITY ( -- )
   s" lib/errors.f" CKT-PAR-ENTRY
   s" lib/string.f" CKT-PAR-ENTRY
   s" lib/memory.f" CKT-PAR-ENTRY
   s" lib/fs.f" CKT-PAR-ENTRY
   s" lib/fs-mutate.f" CKT-PAR-ENTRY
   s" lib/process.f" CKT-PAR-ENTRY
   s" lib/process-argv.f" CKT-PAR-ENTRY
   s" lib/process-env.f" CKT-PAR-ENTRY
   s" lib/process-cwd.f" CKT-PAR-ENTRY
   s" lib/test/suite-test.f" CKT-PAR-ENTRY ;

\ Source-shape pin for the GENERATED check-runner prelude (dot
\ habu-enumerate-generated-check-4109899a): CHK-BUILD-PREFIX emits a
\ deliberate `0 set-check` window into every check-CLI child and must re-arm
\ checking by installing EXACTLY the audited CHECK-F-HOOK, whose body
\ re-enables fail-closed (70 throw). Generated source is lexer-invisible to
\ checked-boundary-lint and trusted-inventory (string literals are skipped by
\ design), so this shape regression IS the policing for the generated seam:
\ every set-check line in the generated text must be one of the two audited
\ shapes, with exactly one hook install. The doctored legs prove the scanner
\ has teeth - a rogue install name or a missing install rejects.
1024 constant CKTP-DOC-CAP
create CKTP-DOC CKTP-DOC-CAP allot
variable CKTP-I  variable CKTP-START
variable CKTP-BAD  variable CKTP-INSTALLS
variable CKTP-DOC-U

: CKTP-LINE-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" 0 set-check" LINT-STR= if LINT-TRUE exit then
   a u s" ' CHECK-F-HOOK set-check" LINT-STR= ;

: CKTP-NOTE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" set-check" CONTAINS? 0= if exit then
   a u s" ' CHECK-F-HOOK set-check" LINT-STR= if CKTP-INSTALLS @ 1+ CKTP-INSTALLS ! then
   a u CKTP-LINE-OK? 0= if -1 CKTP-BAD ! then ;

: CKTP-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CKTP-BAD !  0 CKTP-INSTALLS !
   0 CKTP-START !  0 CKTP-I !
   begin CKTP-I @ u < while
      a CKTP-I @ + c@ 10 = if
         a CKTP-START @ + CKTP-I @ CKTP-START @ - CKTP-NOTE-LINE
         CKTP-I @ 1+ CKTP-START !
      then
      CKTP-I @ 1+ CKTP-I !
   repeat
   CKTP-START @ u < if
      a CKTP-START @ + u CKTP-START @ - CKTP-NOTE-LINE
   then ;

: CKTP-SHAPE-OK? ( ptr u8 n -- bool )
   CKTP-SCAN
   CKTP-BAD @ 0=  CKTP-INSTALLS @ 1 =  and ;

: CKTP-PRELUDE ( -- ptr u8 n )
   s" prelude-shape.f" CHK-LABEL!
   0 CHK-JSON !
   CHK-RUN-RESET
   CHK-BUILD-PREFIX
   CHK-RUN-BUF CHK-RUN-U @ ;

: CKTP-DOC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CKTP-DOC-U @ u + CKTP-DOC-CAP > if E-STR-CAPACITY throw then
   a CKTP-DOC CKTP-DOC-U @ + u BYTE-COPY
   CKTP-DOC-U @ u + CKTP-DOC-U ! ;

: CKTP-DOCTORED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 CKTP-DOC-U !
   a u CKTP-DOC+
   S\" ' EVIL-HOOK set-check\n" CKTP-DOC+
   CKTP-DOC CKTP-DOC-U @ ;

: CKT-TEST-PRELUDE-HOOK ( -- )
   CKTP-PRELUDE {: pa:ptr pu:n :}
   pa pu CKTP-SHAPE-OK? TTRUE
   pa pu s" CHECK! HOOK-REPORT-UNCHECKABLE dup -1 <> IF 70 throw THEN ;" CONTAINS? TTRUE
   pa pu s" 0 set-check" CONTAINS? TTRUE
   pa pu CKTP-DOCTORED$ CKTP-SHAPE-OK? TFALSE
   pa 0 CKTP-SHAPE-OK? TFALSE ;

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
   s" check/typefamily-good" [: CKT-TEST-TYPEFAMILY-GOOD ;] CKT-RUN
   s" check/sumtype-bad" [: CKT-TEST-SUMTYPE-BAD ;] CKT-RUN
   s" check/tfam-noarity" [: CKT-TEST-TFAM-NOARITY ;] CKT-RUN
   s" check/sum-noend" [: CKT-TEST-SUM-NOEND ;] CKT-RUN
   s" check/oversize" [: CKT-TEST-OVERSIZE ;] CKT-RUN
   s" check/enum-good" [: CKT-TEST-ENUM-GOOD ;] CKT-RUN
   s" check/enum-bad" [: CKT-TEST-ENUM-BAD ;] CKT-RUN
   s" check/product-good" [: CKT-TEST-PRODUCT-GOOD ;] CKT-RUN
   s" check/product-bad" [: CKT-TEST-PRODUCT-BAD ;] CKT-RUN
   s" check/product-all-errors" [: CKT-TEST-PRODUCT-ALL-ERRORS ;] CKT-RUN
   s" check/nominal-scan-top-level" [: CKT-TEST-NOMINAL-SCAN-TOP-LEVEL ;] CKT-RUN
   s" check/require-facade" [: CKT-TEST-REQUIRE-FACADE ;] CKT-RUN
   s" check/included-dep" [: CKT-TEST-INCLUDED-DEP ;] CKT-RUN
   s" check/closure-parity" [: CKT-TEST-CLOSURE-PARITY ;] CKT-RUN
   s" check/source-list-all-errors" [: CKT-TEST-SOURCE-LIST-ALL-ERRORS ;] CKT-RUN
   s" check/prelude-hook-shape" [: CKT-TEST-PRELUDE-HOOK ;] CKT-RUN
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;
