\ repair-packet-test.f - checked fixture for repair packet generation.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/cli-run.f
\ lib/vector.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/check-all-errors-core.f tools/json.f tools/gate-json-assert-core.f
\ lib/argv.f tools/repair-packet-core.f tools/repair-packet-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/test/outcome.f
require lib/process-argv.f
require tools/cli-run.f
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

package REPAIR-PACKET-TEST

private

$20000 constant CAPTURE-CAP
10000 constant TIMEOUT-MS

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
   s" habu-repair-packet" TMPDIR-MKDIR {: a:ptr u :}
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
   CLI-TOOLS$  >LEN OUT CAPTURE-CAP >LEN
   ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME ;

: RUN-CHECK-ACT ( -- )
   LABEL$ SRC CHECK-ALL-ERRORS:FILE ;

: RUN-CHECK ( ptr u8 n -- len len outcome )
   LABEL!
   ERR CAPTURE-CAP OUT CAPTURE-CAP CHECK-ALL-ERRORS:BUFFERS!
   0 0= CHECK-ALL-ERRORS:JSON!
   [: RUN-CHECK-ACT ;] catch {: rc:n :}
   0 >LEN CHECK-ALL-ERRORS:OUT$ nip >LEN rc OUTCOME:EXITED ;

: OUTCOME-CODE ( outcome -- n bool )   \ code plus exited?
   MATCH outcome
     exited OF 0 0= ENDOF
     signaled OF 0 0= 0= ENDOF
     timeout OF 0 0 0= 0= ENDOF
   ;MATCH ;

: DUMP-CAPTURE ( n n n n -- )
   {: outu:n erru:n code:n expect:n :}
   s" repair-packet-test failure" type cr
   s" case: " type LABEL$ type cr
   s" source: " type SRC type cr
   s" diag: " type DIAG-PATH type cr
   s" packet: " type PACKET type cr
   s" expected exit: " type expect . cr
   s" code: " type code . cr
   s" stdout bytes: " type outu . s" / " type CAPTURE-CAP . cr
   s" stderr bytes: " type erru . s" / " type CAPTURE-CAP . cr
   s" stdout:" type cr
   OUT outu type
   s" stderr:" type cr
   ERR erru type ;

: EXPECT-EXIT ( len len outcome n -- n n ) {: expect:n :}
   OUTCOME-CODE {: outu:len erru:len code:n exited:bool :}
   exited 0= if outu LEN>N erru LEN>N code expect DUMP-CAPTURE then
   code expect <> if outu LEN>N erru LEN>N code expect DUMP-CAPTURE then
   LABEL$ T-LABEL
   exited TTRUE
   LABEL$ T-LABEL
   code expect T=
   outu LEN>N erru LEN>N ;

: EXPECT-EXIT-NZ ( len len outcome -- n n )
   OUTCOME-CODE {: outu:len erru:len code:n exited:bool :}
   exited 0= if outu LEN>N erru LEN>N code 0 DUMP-CAPTURE then
   code 0 = if outu LEN>N erru LEN>N code -1 DUMP-CAPTURE then
   LABEL$ T-LABEL
   exited TTRUE
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
   JNEXT s" declared_effect" s" n -- rptzrc<> " JSTR
   JNEXT s" declared_effect_source" s" n -- rptzrc" JSTR
   JNEXT s" inferred_effect" s" n -- n " JSTR
   JNEXT s" return_stack" JSONW-KEY JSONW-OBJECT-START
   s" expected" s" " JSTR
   JNEXT s" actual" s" " JSTR
   JSONW-OBJECT-END
   JNEXT s" expected" s" rptzrc<> " JSTR
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

\ Keep one warm-aware CLI no-argument smoke; packet semantics run in-process.
: ARGV-REPAIR-NOARGS ( -- )
   PROC-ARGV-RESET
   s" tools/repair-packet-core.f" s" tools/repair-packet.f" CLI-TOOLS-LOAD2 if exit then
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/argv.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/repair-packet-core.f"  >LEN PROC-ARGV+
   s" tools/repair-packet.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: RUN-REPAIR-NOARGS ( -- len len outcome )
   ARGV-REPAIR-NOARGS
   HB-CAPTURE ;

: TEST-NOARGS ( -- )
   s" noargs" LABEL!
   RUN-REPAIR-NOARGS 64 EXPECT-EXIT {: outu:n erru:n :}
   s" noargs stdout" T-LABEL
   outu 0 T=
   s" noargs usage" T-LABEL
   ERR erru s" usage: tools/repair-packet.f checker-jsonl.err" CONTAINS? TTRUE ;


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
   TEST-TWO-DIAGS
   TEST-NOARGS
   TEST-GJA-U
   CLEANUP-RUN
   s" cleanup root removed" T-LABEL
   ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-packet-test: ok" type cr ;

RUN

;package
