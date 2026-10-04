\ repair-packet-test.f - checked fixture for repair packet generation.
\ Run: bin/hb --load tools/repair-packet-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/test/outcome.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require tools/json.f
require tools/gate-json-assert-core.f
require lib/argv.f
require tools/repair-packet-core.f
require test/golden.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

package REPAIR-PACKET-TEST

private

$20000 constant CAPTURE-CAP
180000 constant TIMEOUT-MS           \ includes checked compilation of the CLI

variable ROOT-U
variable SRC-U
variable DIAG-U
variable PACKET-U
variable OUT-A
variable ERR-A
variable LABEL-A
variable LABEL-U

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create DIAG-BUF FS-PATH-CAP allot
create PACKET-BUF FS-PATH-CAP allot

: PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: PTR-U8@ ( ptr a -- ptr u8 )
   PTR-U8-FIELD @ ;

: PTR-U8! ( ptr u8 ptr a -- )
   PTR-U8-FIELD ! ;

: LABEL-A@ ( -- ptr u8 )
   LABEL-A PTR-U8@ ;

: LABEL-A! ( ptr u8 -- )
   LABEL-A PTR-U8! ;

: ALLOC-BUF ( -- ptr u8 )
   CAPTURE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: BUF ( ptr a -- ptr u8 ) {: slot:ptr :}
   slot @ 0= if ALLOC-BUF slot PTR-U8! then
   slot PTR-U8@ ;

: OUT ( -- ptr u8 )
   OUT-A BUF ;

: ERR ( -- ptr u8 )
   ERR-A BUF ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: DIAG-PATH ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

: PACKET ( -- ptr u8 n )
   PACKET-BUF PACKET-U @ ;

: LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   a LABEL-A!
   u LABEL-U ! ;

: LABEL$ ( -- ptr u8 n )
   LABEL-A@ LABEL-U @ ;

: LF ( -- )
   10 SB-APPEND-C ;

: DQ ( -- )
   34 SB-APPEND-C ;

: COUNT2$ ( -- ptr u8 n )
   SB-RESET
   DQ s" diagnostic_count" SB-APPEND DQ
   s" :2" SB-APPEND
   SB$ ;

: NAME-SUFFIX$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n suffix:ptr suffixu:n :}
   SB-RESET
   name nameu SB-APPEND
   suffix suffixu SB-APPEND
   SB$ ;

: SRC! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu s" .f" NAME-SUFFIX$ {: file:ptr fileu:n :}
   ROOT file fileu SRC-BUF SRC-U PATH! ;

: DIAG! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu s" .err" NAME-SUFFIX$ {: file:ptr fileu:n :}
   ROOT file fileu DIAG-BUF DIAG-U PATH! ;

: PACKET! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu s" .packet" NAME-SUFFIX$ {: file:ptr fileu:n :}
   ROOT file fileu PACKET-BUF PACKET-U PATH! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-repair-packet" HB-TMP-MKDIR {: a:ptr u :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-TREE+ ;

: SOURCE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   a u SB-APPEND
   LF
   SB$ ;

: WRITE-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC a u SOURCE$ WRITE-ALL ;

: HB-CAPTURE ( -- len len outcome )
   ENGINE-CANDIDATE:PATH$  >LEN OUT CAPTURE-CAP >LEN
   ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME ;

: RUN-CHECK-ACT ( -- )
   LABEL$ SRC CHECK-ALL-ERRORS:FILE ;

\ The check runs in this process, so its result is the code it threw.
: RUN-CHECK ( ptr u8 n -- len len n )
   LABEL!
   ERR CAPTURE-CAP OUT CAPTURE-CAP CHECK-ALL-ERRORS:BUFFERS!
   0 0= CHECK-ALL-ERRORS:JSON!
   [: RUN-CHECK-ACT ;] catch {: rc:n :}
   0 >LEN CHECK-ALL-ERRORS:OUT$ nip >LEN rc ;

: DUMP-CAPTURE ( n n n n ptr u8 n -- )
   {: outu:n erru:n code:n expect:n ran:ptr ranu:n :}
   s" repair-packet-test failure" type cr
   s" case: " type LABEL$ type cr
   s" program: " type ran ranu type cr
   s" expected exit: " type expect FMT:.INT cr
   s" code: " type code FMT:.INT cr
   s" stdout bytes: " type outu FMT:.INT s"  / " type CAPTURE-CAP FMT:.INT cr
   s" stderr bytes: " type erru FMT:.INT s"  / " type CAPTURE-CAP FMT:.INT cr
   s" stdout:" type cr
   OUT outu type
   s" stderr:" type cr
   ERR erru type ;

: REPAIR-TOOL$ ( -- ptr u8 n )
   s" tools/repair-packet.f" ;

: CHECK-TOOL$ ( -- ptr u8 n )
   s" tools/check.f" ;

\ Each child this file spawns loads a tool; the caller names it last.
: EXPECT-EXIT ( len len outcome n ptr u8 n -- n n )
   {: outu:len erru:len oc expect:n ran:ptr ranu:n :}
   oc MATCH outcome
     exited OF dup expect <> if outu LEN>N erru LEN>N rot expect ran ranu DUMP-CAPTURE else drop then ENDOF
     signaled OF outu LEN>N erru LEN>N rot 128 + expect ran ranu DUMP-CAPTURE ENDOF
     timeout OF ENDOF
   ;MATCH
   LABEL$ T-LABEL
   ran ranu OUT outu LEN>N ERR erru LEN>N oc expect T-OUTCOME-EXITED=
   outu LEN>N erru LEN>N ;

: EXPECT-EXIT-NZ ( len len n -- n n )
   {: outu:len erru:len code:n :}
   code 0 = if outu LEN>N erru LEN>N code -1 SRC DUMP-CAPTURE then
   LABEL$ T-LABEL
   code 0 T<>
   outu LEN>N erru LEN>N ;

: WRITE-DIAG ( n -- ) {: erru:n :}
   DIAG-PATH ERR erru WRITE-ALL ;

: EXPECT-CHECK-REJECT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu RUN-CHECK EXPECT-EXIT-NZ {: outu:n erru:n :}
   label labelu T-LABEL
   outu 0 T=
   label labelu T-LABEL
   erru 0 T<>
   label labelu T-LABEL
   ERR erru s" schema_version" CONTAINS? TTRUE
   erru WRITE-DIAG ;

: PACKET$ ( -- ptr u8 n )
   DIAG-PATH RP-READ-FILE 2dup RP-COUNT >r RP-FIRST r> RP-PACKET ;

: MAKE-PACKET ( -- )
   PACKET$ {: a:ptr u:n :}
   LABEL$ T-LABEL
   u 0 T<>
   PACKET a u WRITE-ALL ;

: ASSERT-PACKET ( ptr u8 n -- ) {: class:ptr classu:n :}
   LABEL$ T-LABEL
   PACKET class classu GJA-REPAIR-PACKET ;

: GOLDEN-NAME$ ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   SB-RESET
   s" repair-" SB-APPEND
   name nameu SB-APPEND
   s" .packet" SB-APPEND
   SB$ ;

\ Byte-exact golden of the generated packet; the temp root inside the embedded
\ diagnostic is redacted to <root> so the golden stays stable across runs.
: EXPECT-GOLDEN ( ptr u8 n -- ) {: name:ptr nameu:n :}
   ROOT GOLD:REDACT!
   PACKET OUT CAPTURE-CAP READ-ALL {: pu:n :}
   LABEL$ T-LABEL
   OUT pu name nameu GOLDEN-NAME$ GOLD:CHECK TTRUE ;

: CASE-PATHS ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu SRC!
   name nameu DIAG!
   name nameu PACKET! ;

: PACKET-CASE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n class:ptr classu:n src:ptr srcu:n :}
   name nameu CASE-PATHS
   src srcu WRITE-SOURCE
   name nameu EXPECT-CHECK-REJECT
   MAKE-PACKET
   class classu ASSERT-PACKET
   name nameu EXPECT-GOLDEN ;

: TWO-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD1 ( i64 -- i64 ) dup ;" SB-APPEND LF
   s" : BAD2 ( i64 -- ) >r ;" SB-APPEND LF
   SB$ ;

: JSTR ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n :}
   key keyu JSONW-KEY
   val valu JSONW-STRING ;

: JNUM ( ptr u8 n n -- ) {: key:ptr keyu:n val:n :}
   key keyu JSONW-KEY
   val RP-U ;

: JNEXT ( -- )
   JSONW-COMMA ;

: JDONE ( -- ptr u8 n )
   JSONW-OBJECT-END
   JSON-OUT-BUF JSON-OUT-LEN @ ;

: FAMILY-DIAG$ ( -- ptr u8 n )
   JSONW-RESET  JSONW-OBJECT-START
   s" schema_version" 1 JNUM
   JNEXT s" code" s" E-MISMATCH" JSTR
   JNEXT s" repair_class" s" fix_type" JSTR
   JNEXT s" verdict" s" rejected" JSTR
   JNEXT s" word" s" diag-family" JSTR
   JNEXT s" token" s" DIAG-FAMILY" JSTR
   JNEXT s" token_index" 0 JNUM
   JNEXT s" file" s" family" JSTR
   JNEXT s" line" 1 JNUM
   JNEXT s" column" 3 JNUM
   JNEXT s" byte_start" 2 JNUM
   JNEXT s" byte_end" 13 JNUM
   JNEXT s" definition_source" s" DIAG-FAMILY ( n -- rptzrc ) " JSTR
   JNEXT s" declared_effect" s" n -- rptzrc " JSTR
   JNEXT s" declared_effect_source" s" n -- rptzrc" JSTR
   JNEXT s" inferred_effect" s" n -- n " JSTR
   JNEXT s" return_stack" JSONW-KEY JSONW-OBJECT-START
   s" expected" s" " JSTR
   JNEXT s" actual" s" " JSTR
   JSONW-OBJECT-END
   JNEXT s" expected" s" rptzrc " JSTR
   JNEXT s" actual" s" n " JSTR
   JNEXT s" family" s" rptzrc" JSTR
   JNEXT s" suggestion" s" Change the body so produced types match the signature." JSTR
   JDONE ;

: DECL-DIAG$ ( -- ptr u8 n )
   JSONW-RESET  JSONW-OBJECT-START
   s" schema_version" 1 JNUM
   JNEXT s" code" s" E-BAD-DECLARATION" JSTR
   JNEXT s" repair_class" s" fix_family_declaration" JSTR
   JNEXT s" verdict" s" rejected" JSTR
   JNEXT s" decl" s" sumtype" JSTR
   JNEXT s" family" s" badsum" JSTR
   JNEXT s" token" s" samev" JSTR
   JNEXT s" reason" s" duplicate variant" JSTR
   JNEXT s" file" s" declaration" JSTR
   JNEXT s" suggestion" s" Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks." JSTR
   JDONE ;

: TEST-REPAIR-CLASSES ( -- )
   s" remove" s" remove_producer" s" : DIAG-REMOVE ( i64 -- i64 ) dup ;" PACKET-CASE
   s" add" s" add_producer" s" : DIAG-ADD ( i64 -- i64 ) drop ;" PACKET-CASE
   s" type" s" fix_type" s" : DIAG-TYPE ( i64 -- i64 ) 0= ;" PACKET-CASE
   s" rstack" s" fix_return_stack" s" : DIAG-RSTACK ( i64 -- ) >r ;" PACKET-CASE ;

\ Source spans without a definition: a statement the checker throws out of
\ and the lexer's two defects.
: TEST-SPAN-KINDS ( -- )
   s" statement" s" unknown_rejection" s" ;using" PACKET-CASE
   s" unterminated" s" close_string" s\" : DIAG-UNTERM ( -- ) s\" abc ;" PACKET-CASE
   s" row" s" close_primitive_row" s" PRIM: DIAG-ROW PE-N PE-IN" PACKET-CASE ;

\ A storage declaration its definer refuses names the declared word and the
\ refused token, with no definition around them.
: TEST-STORAGE ( -- )
   s" storage" s" fix_storage_type" s" 4 TYPED-BUFFER DIAG-STG no-such-type" PACKET-CASE ;

: ARGV-CHECK-SOURCE ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHECK-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --all-errors" >LEN PROC-ARGV+
   s" --json-errors" >LEN PROC-ARGV+
   SRC >LEN PROC-ARGV+ ;

\ A source tools/check.f refuses, checked by the CLI in a child: the run exits
\ rc, and the packet comes from the first refusal it writes.
: CHILD-CASE ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: name:ptr nameu:n class:ptr classu:n src:ptr srcu:n rc:n :}
   name nameu CASE-PATHS
   name nameu LABEL!
   src srcu WRITE-SOURCE
   ARGV-CHECK-SOURCE
   HB-CAPTURE rc CHECK-TOOL$ EXPECT-EXIT {: outu:n erru:n :}
   name nameu T-LABEL
   outu 0 T=
   erru WRITE-DIAG
   MAKE-PACKET
   class classu ASSERT-PACKET
   name nameu EXPECT-GOLDEN ;

\ A pre-read row has a source position; one evaluated at run time has none.
: TEST-GENERATES ( -- )
   s" generates" s" fix_generates_row" s" generates: DIAG-GEN ( -- n )" PACKET-CASE
   s" generates-unplaced" s" fix_generates_row"
   s\" s\" generates: DIAG-GEN-EVAL ( -- n )\" evaluate" 67 CHILD-CASE
   DIAG-PATH GJA-DIAG-CONTRACT ;

\ The checker's pre-pass never reads a declaration that `evaluate` runs, so only
\ the run refuses it, and its record has no place. Each exits 70, the refusal
\ status: the storage declaration throws 70, and the family declaration throws
\ its own code past every handler, which the load exits 70 for because the
\ checker rendered that refusal.
: TEST-UNPLACED ( -- )
   s" storage-unplaced" s" fix_storage_type"
   s\" s\" 4 TYPED-BUFFER DIAG-STG no-such-type\" evaluate" 70 CHILD-CASE
   s" declaration-unplaced" s" fix_family_declaration"
   s\" s\" SUMTYPE badsum 0 VARIANT samev ;VARIANT VARIANT samev ;VARIANT ;SUMTYPE\" evaluate" 70 CHILD-CASE ;

\ A refused record names its token and any known position. A trust row, a
\ storage registrar called from source and a record entry called with a
\ malformed name at run time each end the load on an uncaught throw of its own
\ code, which the load exits 70 for because the checker rendered that refusal;
\ check.f keeps the load's status. A declaration's malformed name is refused
\ by the pre-pass before its statement runs. A stored signature that does not
\ parse is counted by the multi-error pre-pass (rc 70). The named definition
\ and using refusals place their own token when the checked text locates it;
\ no statement-throw packet follows them. The packet keeps each code's field.
: TEST-RECORDS ( -- )
   s" trust-row" s" fix_stale_trust_row"
   s\" s\" DIAG-NO-SUCH-WORD\" s\" -- n\" trust" 70 CHILD-CASE
   s" storage-record" s" use_storage_definer"
   s\" : DIAG-RG ( -- n ) 7 ; s\" n\" s\" DIAG-RG\" CHECKER-DEFTYPED-VARIABLE" 70 CHILD-CASE
   s" malformed-record" s" fix_qualified_name" s" defer DIAG:MAL:NAME ( -- )" PACKET-CASE
   s" malformed-run-record" s" fix_qualified_name"
   s\" s\" DIAG:MAL:RUN\" CHECKER-DEFER" 70 CHILD-CASE
   s" stored-signature" s" fix_signature_type"
   s\" s\" DIAG-SIG\" s\" -- diag-no-such-type\" trust" 70 CHILD-CASE
   s" shadowed-arity" s" match_shadowed_private_effect"
   s" package DIAG-SBA : DIAG-TWIN ( n n -- n ) + ; public : DIAG-TWIN ( n -- n ) 1 + ; ;package" 70 CHILD-CASE ;

\ A warning is no refusal: the packet comes from the refusal after it, of the
\ call it leaves undefined, and counts that one alone.
: TEST-WARNING ( -- )
   s" warning" s" unknown_rejection"
   s" : DIAG-WIDE drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop ; : DIAG-WIDE-CALL ( -- ) DIAG-WIDE ;" 70 CHILD-CASE ;

\ A bare token that resolves in a used package and in another scope as well is
\ refused at its reference, which the record places, with the used packages it
\ resolves in: a global and a used public, or two used publics.
: TEST-USING ( -- )
   s" using-shadow" s" disambiguate_using_shadow"
   s" : DIAG-SW ( n -- ) drop ; package DIAG-SP public : DIAG-SW ( n -- ) drop ; ;package using DIAG-SP : DIAG-SU ( -- ) 1 DIAG-SW ; ;using" 70 CHILD-CASE
   s" using-ambiguous" s" disambiguate_using_ambiguous"
   s" package DIAG-UA public : DIAG-UW ( n -- ) drop ; ;package package DIAG-UB public : DIAG-UW ( n -- ) drop ; ;package using DIAG-UA using DIAG-UB : DIAG-UU ( -- ) 1 DIAG-UW ; ;using ;using" 67 CHILD-CASE ;

\ A using record whose `used_packages` is no array is refused, not copied into
\ the packet.
: USING-STRING-DIAG$ ( -- ptr u8 n )
   JSONW-RESET  JSONW-OBJECT-START
   s" schema_version" 1 JNUM
   JNEXT s" code" s" E-USING-AMBIGUOUS" JSTR
   JNEXT s" repair_class" s" disambiguate_using_ambiguous" JSTR
   JNEXT s" verdict" s" rejected" JSTR
   JNEXT s" token" s" DIAG-UW" JSTR
   JNEXT s" file" s" using-string" JSTR
   JNEXT s" used_packages" s" diag-ua" JSTR
   JNEXT s" suggestion" s" Qualify the one meant as PKG:WORD, or rename the collision." JSTR
   JDONE ;

: ARGV-REPAIR-DIAG ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   REPAIR-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   DIAG-PATH >LEN PROC-ARGV+ ;

: TEST-USING-STRING ( -- )
   s" using-string" CASE-PATHS
   s" using-string" LABEL!
   DIAG-PATH USING-STRING-DIAG$ WRITE-ALL
   ARGV-REPAIR-DIAG
   HB-CAPTURE 74 REPAIR-TOOL$ EXPECT-EXIT {: outu:n erru:n :}
   s" using-string stdout" T-LABEL
   outu 0 T=
   s" using-string refusal" T-LABEL
   ERR erru s" repair-packet: used_packages is not array" CONTAINS? TTRUE ;

: TEST-TWO-DIAGS ( -- )
   s" two" CASE-PATHS
   SRC TWO-SOURCE$ WRITE-ALL
   s" two" EXPECT-CHECK-REJECT
   MAKE-PACKET
   s" remove_producer" ASSERT-PACKET
   PACKET OUT CAPTURE-CAP READ-ALL {: packetu:n :}
   s" two diagnostic count" T-LABEL
   OUT packetu COUNT2$ CONTAINS? TTRUE
   s" two" EXPECT-GOLDEN ;

: DIAG-CASE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n class:ptr classu:n diag:ptr diagu:n :}
   name nameu CASE-PATHS
   DIAG-PATH diag diagu WRITE-ALL
   name nameu LABEL!
   MAKE-PACKET
   class classu ASSERT-PACKET
   name nameu EXPECT-GOLDEN ;

: TEST-FAMILY ( -- )
   s" family" s" fix_type" FAMILY-DIAG$ DIAG-CASE ;

: TEST-DECL ( -- )
   s" declaration" s" fix_family_declaration" DECL-DIAG$ DIAG-CASE ;

\ Exercise the self-contained CLI entry; packet semantics run in-process.
: ARGV-REPAIR-NOARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   REPAIR-TOOL$ >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: RUN-REPAIR-NOARGS ( -- len len outcome )
   ARGV-REPAIR-NOARGS
   HB-CAPTURE ;

: TEST-NOARGS ( -- )
   s" noargs" LABEL!
   RUN-REPAIR-NOARGS 64 REPAIR-TOOL$ EXPECT-EXIT {: outu:n erru:n :}
   s" noargs stdout" T-LABEL
   outu 0 T=
   s" noargs usage" T-LABEL
   ERR erru s" usage: tools/repair-packet.f checker-jsonl.err" CONTAINS? TTRUE ;

\ The check.f CLI refuses a source the engine provides before checking it, so
\ its record comes from the CLI itself.
: ARGV-CHECK-ENGINE ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHECK-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --json-errors" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+ ;

: TEST-ENGINE ( -- )
   s" engine" CASE-PATHS
   s" engine" LABEL!
   ARGV-CHECK-ENGINE
   HB-CAPTURE 64 CHECK-TOOL$ EXPECT-EXIT {: outu:n erru:n :}
   s" engine stdout" T-LABEL
   outu 0 T=
   erru WRITE-DIAG
   MAKE-PACKET
   s" rebuild_engine" ASSERT-PACKET
   s" engine" EXPECT-GOLDEN ;


\ switchover wave A: GJA-U? returns option<n> (SOME parsed unsigned decimal,
\ else NONE). Both branches, directly (GJA-INT's none arm dies via GJA-FAIL, so
\ the raw parser is the testable seam).
: TEST-GJA-U ( -- )
   s" gja-u option branches" T-LABEL
   s" 12345" GJA-U? MATCH option
     none OF 0 0= 0= ENDOF
     some OF 12345 = ENDOF
   ;MATCH TTRUE
   s" 0" GJA-U? MATCH option
     none OF 0 0= 0= ENDOF
     some OF 0 = ENDOF
   ;MATCH TTRUE
   s" 12a" GJA-U? MATCH option
     none OF 0 0= ENDOF
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE
   s" " GJA-U? MATCH option
     none OF 0 0= ENDOF
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE ;

: RUN ( -- )
   T-RESET
   GOLD:INIT
   PREPARE
   TEST-REPAIR-CLASSES
   TEST-FAMILY
   TEST-DECL
   TEST-SPAN-KINDS
   TEST-STORAGE
   TEST-UNPLACED
   TEST-GENERATES
   TEST-RECORDS
   TEST-WARNING
   TEST-USING
   TEST-USING-STRING
   TEST-TWO-DIAGS
   TEST-NOARGS
   TEST-ENGINE
   TEST-GJA-U
   CLEANUP-RUN
   s" cleanup root removed" T-LABEL
   ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-packet-test: ok" type cr ;

RUN

;package
