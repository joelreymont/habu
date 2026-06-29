\ repair-packet-test.f - checked fixture for repair packet generation.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ lib/vector.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/check-all-errors-core.f tools/json.f tools/gate-json-assert-core.f
\ tools/argv.f tools/repair-packet-core.f tools/repair-packet-test.f

$20000 constant RPT-CAPTURE-CAP
10000 constant RPT-TIMEOUT-MS

variable RPT-ROOT-U
variable RPT-SRC-U
variable RPT-DIAG-U
variable RPT-PACKET-U
variable RPT-OUT-A
variable RPT-ERR-A
variable RPT-LABEL-A
variable RPT-LABEL-U

create RPT-ROOT-BUF FS-PATH-CAP allot
create RPT-SRC-BUF FS-PATH-CAP allot
create RPT-DIAG-BUF FS-PATH-CAP allot
create RPT-PACKET-BUF FS-PATH-CAP allot

: RPT-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: RPT-PTR-U8@ ( ptr a -- ptr u8 )
   RPT-PTR-U8-FIELD @ ;

: RPT-PTR-U8! ( ptr u8 ptr a -- )
   RPT-PTR-U8-FIELD ! ;

: RPT-LABEL-A@ ( -- ptr u8 )
   RPT-LABEL-A RPT-PTR-U8@ ;

: RPT-LABEL-A! ( ptr u8 -- )
   RPT-LABEL-A RPT-PTR-U8! ;

: RPT-ALLOC-BUF ( -- ptr u8 )
   RPT-CAPTURE-CAP MEM-ALLOC-BYTES drop ;

: RPT-BUF ( ptr a -- ptr u8 ) {: slot:ptr :}
   slot @ 0= if RPT-ALLOC-BUF slot RPT-PTR-U8! then
   slot RPT-PTR-U8@ ;

: RPT-OUT ( -- ptr u8 )
   RPT-OUT-A RPT-BUF ;

: RPT-ERR ( -- ptr u8 )
   RPT-ERR-A RPT-BUF ;

: RPT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RPT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: RPT-ROOT ( -- ptr u8 n )
   RPT-ROOT-BUF RPT-ROOT-U @ ;

: RPT-SRC ( -- ptr u8 n )
   RPT-SRC-BUF RPT-SRC-U @ ;

: RPT-DIAG ( -- ptr u8 n )
   RPT-DIAG-BUF RPT-DIAG-U @ ;

: RPT-PACKET ( -- ptr u8 n )
   RPT-PACKET-BUF RPT-PACKET-U @ ;

: RPT-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   a RPT-LABEL-A!
   u RPT-LABEL-U ! ;

: RPT-LABEL$ ( -- ptr u8 n )
   RPT-LABEL-A@ RPT-LABEL-U @ ;

: RPT-LF ( -- )
   10 SB-APPEND-C ;

: RPT-DQ ( -- )
   34 SB-APPEND-C ;

: RPT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: RPT-COUNT2$ ( -- ptr u8 n )
   SB-RESET
   RPT-DQ s" diagnostic_count" SB-APPEND RPT-DQ
   s" :2" SB-APPEND
   SB$ ;

: RPT-NAME-SUFFIX$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: name:ptr nameu suffix:ptr suffixu :}
   SB-RESET
   name nameu SB-APPEND
   suffix suffixu SB-APPEND
   SB$ ;

: RPT-SRC! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu s" .f" RPT-NAME-SUFFIX$ {: file:ptr fileu :}
   RPT-ROOT file fileu RPT-SRC-BUF RPT-SRC-U RPT-PATH! ;

: RPT-DIAG! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu s" .err" RPT-NAME-SUFFIX$ {: file:ptr fileu :}
   RPT-ROOT file fileu RPT-DIAG-BUF RPT-DIAG-U RPT-PATH! ;

: RPT-PACKET! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu s" .packet" RPT-NAME-SUFFIX$ {: file:ptr fileu :}
   RPT-ROOT file fileu RPT-PACKET-BUF RPT-PACKET-U RPT-PATH! ;

: RPT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-repair-packet" TMPDIR-MKDIR {: a:ptr u :}
   a u RPT-ROOT-BUF RPT-ROOT-U RPT-COPY!
   RPT-ROOT CLEANUP-TREE+ ;

: RPT-SOURCE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   a u SB-APPEND
   RPT-LF
   SB$ ;

: RPT-WRITE-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   RPT-SRC a u RPT-SOURCE$ WRITE-ALL ;

: RPT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: RPT-HB-CAPTURE ( -- n n n n )
   WR-TOOLS$  >LEN RPT-OUT RPT-CAPTURE-CAP >LEN
   RPT-ERR RPT-CAPTURE-CAP >LEN RPT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME RPT-CAPTURE>N ;

: RPT-RUN-CHECK-ACT ( -- )
   RPT-LABEL$ RPT-SRC CHECK-ALL-ERRORS-FILE ;

: RPT-RUN-CHECK ( ptr u8 n -- n n n n )
   RPT-LABEL!
   RPT-ERR RPT-CAPTURE-CAP RPT-OUT RPT-CAPTURE-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: RPT-RUN-CHECK-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip PROC-OUTCOME-EXIT rc ;

: RPT-OUTCOME. ( n -- ) {: kind:n :}
   kind PROC-OUTCOME-EXIT = if s" exit" type exit then
   kind PROC-OUTCOME-SIGNAL = if s" signal" type exit then
   kind PROC-OUTCOME-TIMEOUT = if s" timeout" type exit then
   s" unknown" type ;

: RPT-DUMP-CAPTURE ( n n n n n -- )
   {: outu:n erru:n kind:n code:n expect:n :}
   s" repair-packet-test failure" type cr
   s" case: " type RPT-LABEL$ type cr
   s" source: " type RPT-SRC type cr
   s" diag: " type RPT-DIAG type cr
   s" packet: " type RPT-PACKET type cr
   s" expected exit: " type expect . cr
   s" outcome: " type kind RPT-OUTCOME.
   s"  code: " type code . cr
   s" stdout bytes: " type outu . s" / " type RPT-CAPTURE-CAP . cr
   s" stderr bytes: " type erru . s" / " type RPT-CAPTURE-CAP . cr
   s" stdout:" type cr
   RPT-OUT outu type
   s" stderr:" type cr
   RPT-ERR erru type ;

: RPT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT <> if outu erru kind code expect RPT-DUMP-CAPTURE then
   code expect <> if outu erru kind code expect RPT-DUMP-CAPTURE then
   RPT-LABEL$ T-LABEL
   kind PROC-OUTCOME-EXIT T=
   RPT-LABEL$ T-LABEL
   code expect T=
   outu erru ;

: RPT-EXPECT-EXIT-NZ ( n n n n -- n n ) {: outu erru kind code :}
   kind PROC-OUTCOME-EXIT <> if outu erru kind code 0 RPT-DUMP-CAPTURE then
   code 0 = if outu erru kind code -1 RPT-DUMP-CAPTURE then
   RPT-LABEL$ T-LABEL
   kind PROC-OUTCOME-EXIT T=
   RPT-LABEL$ T-LABEL
   code 0 T<>
   outu erru ;

: RPT-WRITE-DIAG ( n -- ) {: erru :}
   RPT-DIAG RPT-ERR erru WRITE-ALL ;

: RPT-EXPECT-CHECK-REJECT ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu RPT-RUN-CHECK RPT-EXPECT-EXIT-NZ {: outu erru :}
   label labelu T-LABEL
   outu 0 T=
   label labelu T-LABEL
   erru 0 T<>
   label labelu T-LABEL
   RPT-ERR erru s" schema_version" CONTAINS? TTRUE
   erru RPT-WRITE-DIAG ;

: RPT-PACKET$ ( -- ptr u8 n )
   RPT-DIAG RP-READ-FILE 2dup RP-COUNT >r RP-FIRST r> RP-PACKET ;

: RPT-MAKE-PACKET ( -- )
   RPT-PACKET$ {: a:ptr u:n :}
   RPT-LABEL$ T-LABEL
   u 0 T<>
   RPT-PACKET a u WRITE-ALL ;

: RPT-ASSERT-PACKET ( ptr u8 n -- ) {: class:ptr classu :}
   RPT-LABEL$ T-LABEL
   RPT-PACKET class classu GJA-REPAIR-PACKET ;

: RPT-CASE-PATHS ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu RPT-SRC!
   name nameu RPT-DIAG!
   name nameu RPT-PACKET! ;

: RPT-CASE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: name:ptr nameu class:ptr classu src:ptr srcu :}
   name nameu RPT-CASE-PATHS
   src srcu RPT-WRITE-SOURCE
   name nameu RPT-EXPECT-CHECK-REJECT
   RPT-MAKE-PACKET
   class classu RPT-ASSERT-PACKET ;

: RPT-TWO-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   s" : BAD1 ( i64 -- i64 ) dup ;" SB-APPEND RPT-LF
   s" : BAD2 ( i64 -- ) >r ;" SB-APPEND RPT-LF
   SB$ ;

: RPT-TEST-REPAIR-CLASSES ( -- )
   s" remove" s" remove_producer" s" : DIAG-REMOVE ( i64 -- i64 ) dup ;" RPT-CASE
   s" add" s" add_producer" s" : DIAG-ADD ( i64 -- i64 ) drop ;" RPT-CASE
   s" type" s" fix_type" s" : DIAG-TYPE ( i64 -- i64 ) 0= ;" RPT-CASE
   s" rstack" s" fix_return_stack" s" : DIAG-RSTACK ( i64 -- ) >r ;" RPT-CASE ;

: RPT-TEST-TWO-DIAGS ( -- )
   s" two" RPT-CASE-PATHS
   RPT-SRC RPT-TWO-SOURCE$ WRITE-ALL
   s" two" RPT-EXPECT-CHECK-REJECT
   RPT-MAKE-PACKET
   s" remove_producer" RPT-ASSERT-PACKET
   RPT-PACKET RPT-OUT RPT-CAPTURE-CAP READ-ALL {: packetu :}
   s" two diagnostic count" T-LABEL
   RPT-OUT packetu RPT-COUNT2$ CONTAINS? TTRUE ;

\ Keep one warm-aware CLI no-argument smoke; packet semantics run in-process.
: RPT-ARGV-REPAIR-NOARGS ( -- )
   PROC-ARGV-RESET
   s" tools/repair-packet-core.f" s" tools/repair-packet.f" WR-TOOLS-LOAD2 if exit then
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/repair-packet-core.f"  >LEN PROC-ARGV+
   s" tools/repair-packet.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: RPT-RUN-REPAIR-NOARGS ( -- n n n n )
   RPT-ARGV-REPAIR-NOARGS
   RPT-HB-CAPTURE ;

: RPT-TEST-NOARGS ( -- )
   s" noargs" RPT-LABEL!
   RPT-RUN-REPAIR-NOARGS 64 RPT-EXPECT-EXIT {: outu erru :}
   s" noargs stdout" T-LABEL
   outu 0 T=
   s" noargs usage" T-LABEL
   RPT-ERR erru s" usage: tools/repair-packet.f checker-jsonl.err" CONTAINS? TTRUE ;

: RPT-MAIN ( -- )
   T-RESET
   RPT-PREPARE
   RPT-TEST-REPAIR-CLASSES
   RPT-TEST-TWO-DIAGS
   RPT-TEST-NOARGS
   CLEANUP-RUN
   s" cleanup root removed" T-LABEL
   RPT-ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-packet-test: ok" type cr ;

RPT-MAIN
