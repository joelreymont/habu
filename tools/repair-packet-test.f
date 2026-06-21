\ repair-packet-test.f - checked fixture for repair packet generation.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/repair-packet-test.f

$20000 constant RPT-CAPTURE-CAP
5000 constant RPT-TIMEOUT-MS

variable RPT-ROOT-U
variable RPT-SRC-U
variable RPT-DIAG-U
variable RPT-PACKET-U

create RPT-ROOT-BUF FS-PATH-CAP allot
create RPT-SRC-BUF FS-PATH-CAP allot
create RPT-DIAG-BUF FS-PATH-CAP allot
create RPT-PACKET-BUF FS-PATH-CAP allot
create RPT-OUT RPT-CAPTURE-CAP allot
create RPT-ERR RPT-CAPTURE-CAP allot

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

: RPT-ARGV-CHECK ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu file:ptr fileu :}
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/memory.f" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/lint/json-writer.f" PROC-ARGV+
   s" tools/lint/source-lex.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check-all-errors.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" --label" PROC-ARGV+
   label labelu PROC-ARGV+
   file fileu PROC-ARGV+ ;

: RPT-RUN-CHECK ( ptr u8 n -- n n n ) {: label:ptr labelu :}
   label labelu RPT-SRC RPT-ARGV-CHECK
   s" bin/hb" RPT-OUT RPT-CAPTURE-CAP RPT-ERR RPT-CAPTURE-CAP
   RPT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: RPT-WRITE-DIAG ( n -- ) {: erru :}
   RPT-DIAG RPT-ERR erru WRITE-ALL ;

: RPT-EXPECT-CHECK-REJECT ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu RPT-RUN-CHECK 0 T<>
   {: outu erru :}
   outu 0 T=
   erru 0 T<>
   RPT-ERR erru s" schema_version" CONTAINS? TTRUE
   erru RPT-WRITE-DIAG ;

: RPT-ARGV-REPAIR ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/repair-packet.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RPT-DIAG PROC-ARGV+ ;

: RPT-RUN-REPAIR ( -- n n n )
   RPT-ARGV-REPAIR
   s" bin/hb" RPT-OUT RPT-CAPTURE-CAP RPT-ERR RPT-CAPTURE-CAP
   RPT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: RPT-WRITE-PACKET ( n -- ) {: outu :}
   RPT-PACKET RPT-OUT outu WRITE-ALL ;

: RPT-MAKE-PACKET ( -- )
   RPT-RUN-REPAIR 0 T=
   {: outu erru :}
   RPT-ERR erru RPT-EMPTY$ T$=
   outu 0 T<>
   outu RPT-WRITE-PACKET ;

: RPT-ARGV-ASSERT ( ptr u8 n -- ) {: class:ptr classu :}
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/gate-json-assert.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" repair-packet" PROC-ARGV+
   RPT-PACKET PROC-ARGV+
   class classu PROC-ARGV+ ;

: RPT-RUN-ASSERT ( ptr u8 n -- n n n ) {: class:ptr classu :}
   class classu RPT-ARGV-ASSERT
   s" bin/hb" RPT-OUT RPT-CAPTURE-CAP RPT-ERR RPT-CAPTURE-CAP
   RPT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: RPT-ASSERT-PACKET ( ptr u8 n -- ) {: class:ptr classu :}
   class classu RPT-RUN-ASSERT 0 T=
   {: outu erru :}
   outu 0 T=
   RPT-ERR erru RPT-EMPTY$ T$= ;

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
   RPT-OUT packetu RPT-COUNT2$ CONTAINS? TTRUE ;

: RPT-ARGV-REPAIR-NOARGS ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/repair-packet.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: RPT-RUN-REPAIR-NOARGS ( -- n n n )
   RPT-ARGV-REPAIR-NOARGS
   s" bin/hb" RPT-OUT RPT-CAPTURE-CAP RPT-ERR RPT-CAPTURE-CAP
   RPT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: RPT-TEST-NOARGS ( -- )
   RPT-RUN-REPAIR-NOARGS 64 T=
   {: outu erru :}
   outu 0 T=
   RPT-ERR erru s" usage: tools/repair-packet.f checker-jsonl.err" CONTAINS? TTRUE ;

: RPT-MAIN ( -- )
   T-RESET
   RPT-PREPARE
   RPT-TEST-REPAIR-CLASSES
   RPT-TEST-TWO-DIAGS
   RPT-TEST-NOARGS
   CLEANUP-RUN
   RPT-ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-packet-test: ok" type cr ;

RPT-MAIN
