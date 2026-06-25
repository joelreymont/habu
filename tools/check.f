\ check.f - Habu-native checked engine wrapper.
\ Load after lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f
\ lib/process.f lib/process-argv.f lib/source.f and tools/argv.f.

\ Audited hook-install boundary: this tool must install its checker hook before
\ validating generated source snippets with CHECK!.
0 set-check

: CHK-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
' CHK-CHECK-HOOK set-check

$40000 constant CHK-SRC-CAP
$50000 constant CHK-RUN-CAP
$50000 constant CHK-ORIGIN-CAP
$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
32 constant CHK-NUM-CAP
128 constant CHK-MAX-POS
120000 constant CHK-TIMEOUT-MS

10 constant CHK-LF
32 constant CHK-SP
34 constant CHK-DQ
45 constant CHK-DASH
64 constant CHK-E-USAGE
66 constant CHK-E-NOINPUT
69 constant CHK-E-UNAVAILABLE
70 constant CHK-E-CHECK

create CHK-SRC-BUF CHK-SRC-CAP allot
create CHK-RUN-BUF CHK-RUN-CAP allot
create CHK-ORIGIN-BUF CHK-ORIGIN-CAP allot
create CHK-OUT-BUF CHK-OUT-CAP allot
create CHK-ERR-BUF CHK-ERR-CAP allot
create CHK-NUM-BUF CHK-NUM-CAP allot
create CHK-ROOT-BUF FS-PATH-CAP allot
create CHK-SRC-PATH-BUF FS-PATH-CAP allot
create CHK-RUN-PATH-BUF FS-PATH-CAP allot
create CHK-ERR-PATH-BUF FS-PATH-CAP allot
create CHK-POS-A CHK-MAX-POS cells allot
create CHK-POS-U CHK-MAX-POS cells allot
create CHK-ONE 1 allot

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-JSON
variable CHK-STRICT
variable CHK-ALL
variable CHK-SOURCE-LIST
variable CHK-SRC-U
variable CHK-RUN-U
variable CHK-ORIGIN-U
variable CHK-OUT-U
variable CHK-ERR-U
variable CHK-RC
variable CHK-CHILD-RC
variable CHK-NUM-I
variable CHK-LABEL-A
variable CHK-LABEL-U
variable CHK-SRC-A
variable CHK-SRC-PATH-U
variable CHK-RUN-PATH-U
variable CHK-ROOT-U
variable CHK-ERR-PATH-U

: CHK-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: CHK-PTR-U8@ ( ptr a -- ptr u8 )
   CHK-PTR-U8-FIELD @ ;

: CHK-PTR-U8! ( ptr u8 ptr a -- )
   CHK-PTR-U8-FIELD ! ;

: CHK-PTR-U8-SLOT ( n ptr a -- ptr ptr u8 )
   swap cells + CHK-PTR-U8-FIELD ;

: CHK-PTR-U8-SLOT@ ( n ptr a -- ptr u8 )
   CHK-PTR-U8-SLOT @ ;

: CHK-PTR-U8-SLOT! ( ptr u8 n ptr a -- )
   CHK-PTR-U8-SLOT ! ;

: CHK-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: CHK-OUT ( ptr u8 n -- )
   1 -rot CHK-WRITE ;

: CHK-ERR ( ptr u8 n -- )
   2 -rot CHK-WRITE ;

: CHK-C! ( n -- )
   CHK-ONE c! ;

: CHK-ERR-C ( n -- )
   CHK-C!
   2 CHK-ONE 1 CHK-WRITE ;

: CHK-ERR-LN ( ptr u8 n -- )
   CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-USAGE ( -- )
   s" usage: tools/check.f [--json-errors] [--strict-signatures] [--all-errors] [--source-list file ... | prog.f]" CHK-ERR-LN
   CHK-E-USAGE throw ;

: CHK-THROW ( n -- )
   CLEANUP-RUN
   throw ;

: CHK-FAIL ( ptr u8 n n -- ) {: msg:ptr u code :}
   msg u CHK-ERR-LN
   code CHK-THROW ;

: CHK-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: CHK-ARG= ( n ptr u8 n -- bool ) {: idx a:ptr u :}
   idx CHK-ARG$ a u STR= ;

: CHK-DASH? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a c@ CHK-DASH = else 0 0= 0= then ;

: CHK-POS-SLOT ( n -- ptr ptr u8 )
   CHK-POS-A CHK-PTR-U8-SLOT ;

: CHK-POS-U-SLOT ( n -- ptr n )
   cells CHK-POS-U + ;

: CHK-POS$ ( n -- ptr u8 n ) {: idx :}
   idx 0 < if CHK-USAGE then
   idx CHK-POS-N @ >= if CHK-USAGE then
   idx CHK-POS-SLOT @
   idx CHK-POS-U-SLOT @ ;

: CHK-ADD-POS ( ptr u8 n -- ) {: a:ptr u :}
   CHK-POS-N @ CHK-MAX-POS >= if CHK-USAGE then
   CHK-SOURCE-LIST @ 0= if CHK-POS-N @ 0 > if CHK-USAGE then then
   a CHK-POS-N @ CHK-POS-A CHK-PTR-U8-SLOT!
   u CHK-POS-N @ CHK-POS-U-SLOT !
   CHK-POS-N @ 1+ CHK-POS-N ! ;

: CHK-PARSE-ONE ( ptr u8 n -- ) {: a:ptr u :}
   a u s" --json-errors" STR= if -1 CHK-JSON ! exit then
   a u s" --strict-signatures" STR= if -1 CHK-STRICT ! exit then
   a u s" --all-errors" STR= if -1 CHK-ALL ! exit then
   a u s" --source-list" STR= if -1 CHK-SOURCE-LIST ! exit then
   a u CHK-DASH? if CHK-USAGE then
   a u CHK-ADD-POS ;

: CHK-COLLECT-REST ( -- )
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ CHK-ARG$ CHK-ADD-POS
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-PARSE ( -- )
   0 CHK-ARG-I !
   0 CHK-POS-N !
   0 CHK-JSON !
   0 CHK-STRICT !
   0 CHK-ALL !
   0 CHK-SOURCE-LIST !
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ s" --" CHK-ARG= if
         CHK-ARG-I @ 1+ CHK-ARG-I !
         CHK-COLLECT-REST
         exit
      then
      CHK-ARG-I @ CHK-ARG$ CHK-PARSE-ONE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CHK-ROOT ( -- ptr u8 n )
   CHK-ROOT-BUF CHK-ROOT-U @ ;

: CHK-SRC-PATH ( -- ptr u8 n )
   CHK-SRC-PATH-BUF CHK-SRC-PATH-U @ ;

: CHK-ERR-PATH ( -- ptr u8 n )
   CHK-ERR-PATH-BUF CHK-ERR-PATH-U @ ;

: CHK-RUN-PATH ( -- ptr u8 n )
   CHK-RUN-PATH-BUF CHK-RUN-PATH-U @ ;

: CHK-MAKE-TEMP ( -- )
   CLEANUP-RESET
   s" habu-check" TMPDIR-MKDIR CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT CLEANUP-TREE+
   CHK-ROOT s" source.f" CHK-SRC-PATH-BUF JOIN-PATH CHK-SRC-PATH-U !
   CHK-ROOT s" run.f" CHK-RUN-PATH-BUF JOIN-PATH CHK-RUN-PATH-U !
   CHK-ROOT s" stderr.txt" CHK-ERR-PATH-BUF JOIN-PATH CHK-ERR-PATH-U ! ;

: CHK-LABEL-STDIN ( -- )
   s" <stdin>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8! ;

: CHK-LABEL-FILE ( -- )
   CHK-SRC-A CHK-PTR-U8@ CHK-LABEL-A CHK-PTR-U8!
   CHK-SRC-U @ CHK-LABEL-U ! ;

: CHK-LABEL ( -- ptr u8 n )
   CHK-LABEL-A CHK-PTR-U8@ CHK-LABEL-U @ ;

: CHK-SOURCE ( -- ptr u8 n )
   CHK-SRC-A CHK-PTR-U8@ CHK-SRC-U @ ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-SRC-BUF CHK-SRC-CAP >LEN READ-STDIN-ALL LEN>N CHK-SRC-U !
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-MATERIALIZE-FILE ( -- )
   0 CHK-POS$ CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8!
   CHK-SOURCE FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   CHK-LABEL-FILE ;

: CHK-SRC-C+ ( n -- ) {: c :}
   CHK-SRC-U @ 1+ CHK-SRC-CAP > if E-FS-CAPACITY throw then
   c CHK-SRC-BUF CHK-SRC-U @ + c!
   CHK-SRC-U @ 1+ CHK-SRC-U ! ;

: CHK-SRC-READ+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   path pathu CHK-SRC-BUF CHK-SRC-U @ + CHK-SRC-CAP CHK-SRC-U @ -
   READ-ALL {: got :}
   CHK-SRC-U @ got + CHK-SRC-U !
   CHK-LF CHK-SRC-C+ ;

: CHK-MATERIALIZE-LIST ( -- )
   CHK-POS-N @ 0= if CHK-USAGE then
   s" <source-list>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8!
   0 CHK-SRC-U !
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-SRC-READ+
      1+
   repeat drop
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-MATERIALIZE ( -- )
   s" bin/hb" FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   CHK-MAKE-TEMP
   CHK-SOURCE-LIST @ if CHK-MATERIALIZE-LIST exit then
   CHK-POS-N @ 0= if CHK-MATERIALIZE-STDIN else CHK-MATERIALIZE-FILE then ;

: CHK-LABEL-DQ? ( -- bool )
   CHK-LABEL CHK-DQ INDEX-OF 0 >= ;

: CHK-CHECK-LABEL ( -- )
   CHK-LABEL-DQ? if s" check.f: source path contains a double quote, cannot set DIAG-FILE" CHK-E-USAGE CHK-FAIL then ;

: CHK-RUN-RESET ( -- )
   0 CHK-RUN-U ! ;

: CHK-RUN+ ( ptr u8 n -- ) {: a:ptr u :}
   CHK-RUN-U @ u + CHK-RUN-CAP > if E-FS-CAPACITY throw then
   a CHK-RUN-BUF CHK-RUN-U @ + u BYTE-COPY
   CHK-RUN-U @ u + CHK-RUN-U ! ;

: CHK-RUN-C ( n -- ) {: c :}
   CHK-RUN-U @ 1+ CHK-RUN-CAP > if E-FS-CAPACITY throw then
   c CHK-RUN-BUF CHK-RUN-U @ + c!
   CHK-RUN-U @ 1+ CHK-RUN-U ! ;

: CHK-RUN-LN ( ptr u8 n -- )
   CHK-RUN+
   CHK-LF CHK-RUN-C ;

: CHK-U$ ( n -- ptr u8 n ) {: u :}
   CHK-NUM-CAP CHK-NUM-I !
   u 0= if
      CHK-NUM-I @ 1- CHK-NUM-I !
      48 CHK-NUM-BUF CHK-NUM-I @ + c!
      CHK-NUM-BUF CHK-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      CHK-NUM-I @ 1- CHK-NUM-I !
      CHK-NUM-BUF CHK-NUM-I @ + c!
      10 /
   repeat drop
   CHK-NUM-BUF CHK-NUM-I @ + CHK-NUM-CAP CHK-NUM-I @ - ;

: CHK-RUN-N ( n -- )
   CHK-U$ CHK-RUN+ ;

: CHK-BUILD-PREFIX ( -- )
   s" 0 set-check" CHK-RUN-LN
   s" s" CHK-RUN+
   CHK-DQ CHK-RUN-C
   CHK-SP CHK-RUN-C
   CHK-LABEL CHK-RUN+
   CHK-DQ CHK-RUN-C
   s"  DIAG-FILE!" CHK-RUN-LN
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-LN then
   s" : CHECK-F-HOOK ( ptr u8 n -- n )" CHK-RUN-LN
   s"    CHECK! dup -1 <> IF 70 throw THEN ;" CHK-RUN-LN
   s" ' CHECK-F-HOOK set-check" CHK-RUN-LN ;

: CHK-BUILD-RUN ( -- )
   CHK-RUN-RESET
   CHK-BUILD-PREFIX
   CHK-ORIGIN-BUF CHK-ORIGIN-U @ CHK-RUN+ ;

: CHK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CHK-FILES-END? ( ptr u8 n -- bool )
   s" ;CHK-FILES" STR= ;

: CHK-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: CHK-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup CHK-FILES-END? if 2drop 0 c, exit then
      CHK-FILES-ITEM,
   again ;

: CHK-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: CHK-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap CHK-FILES-WALK ;

: CHK-FILES: ( -- )
   create CHK-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      CHK-FILES-RUN ;

CHK-FILES: CHK-SIGNATURE-FILES
   lib/errors.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
   tools/lint/source-lex.f tools/argv.f tools/signature-lint.f
;CHK-FILES

CHK-FILES: CHK-BOUNDARY-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
   lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/argv.f
   tools/checked-boundary-lint.f
;CHK-FILES

CHK-FILES: CHK-TRUST-FILES
   tools/date.f lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/argv.f tools/trust-lint.f
;CHK-FILES

CHK-FILES: CHK-DIAG-FILES
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/diag-origin.f
;CHK-FILES

CHK-FILES: CHK-JSON-ONLY-FILES
   lib/errors.f lib/memory.f tools/argv.f tools/json.f tools/json-only.f
;CHK-FILES

CHK-FILES: CHK-ALL-ERRORS-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
   lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/argv.f tools/check-all-errors.f
;CHK-FILES

: CHK-LOAD-RESET ( -- )
   PROC-ARGV-RESET
   s" --load" CHK-ARG+ ;

: CHK-LOAD-END ( -- )
   s" --" CHK-ARG+ ;

: CHK-LOAD-SIGNATURE ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-SIGNATURE-FILES
   CHK-LOAD-END ;

: CHK-LOAD-BOUNDARY ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-BOUNDARY-FILES
   CHK-LOAD-END ;

: CHK-LOAD-TRUST ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-TRUST-FILES
   CHK-LOAD-END ;

: CHK-LOAD-DIAG ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-DIAG-FILES
   CHK-LOAD-END ;

: CHK-LOAD-JSON-ONLY ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-JSON-ONLY-FILES
   CHK-LOAD-END ;

: CHK-LOAD-ALL-ERRORS ( -- )
   CHK-LOAD-RESET
   [: CHK-ARG+ ;] CHK-ALL-ERRORS-FILES
   CHK-LOAD-END ;

: CHK-JSON-OPT ( ptr u8 n -- )
   CHK-JSON @ if CHK-ARG+ else 2drop then ;

: CHK-ADD-LABEL-SOURCE ( -- )
   s" --label" CHK-ARG+
   CHK-LABEL CHK-ARG+
   CHK-SOURCE CHK-ARG+ ;

: CHK-ARGV-SIG ( -- )
   CHK-LOAD-SIGNATURE
   s" --json" CHK-JSON-OPT
   CHK-ADD-LABEL-SOURCE ;

: CHK-ARGV-BOUNDARY ( -- )
   CHK-LOAD-BOUNDARY
   s" --json-errors" CHK-JSON-OPT
   s" --strict-boundary" CHK-ARG+
   CHK-SOURCE CHK-ARG+ ;

: CHK-ARGV-TRUST-PATH ( ptr u8 n -- ) {: path:ptr pathu :}
   CHK-LOAD-TRUST
   s" source-only" CHK-ARG+
   path pathu CHK-ARG+
   s" ." CHK-ARG+ ;

: CHK-ARGV-TRUST ( -- )
   CHK-SOURCE CHK-ARGV-TRUST-PATH ;

: CHK-ARGV-TRUST-LIST ( -- )
   CHK-LOAD-TRUST
   s" source-list" CHK-ARG+
   s" ." CHK-ARG+
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-ARG+
      1+
   repeat drop ;

: CHK-ARGV-DIAG ( -- )
   CHK-LOAD-DIAG
   CHK-SOURCE CHK-ARG+ ;

: CHK-ARGV-JSON-ONLY ( -- )
   CHK-LOAD-JSON-ONLY
   CHK-ERR-PATH CHK-ARG+ ;

: CHK-ARGV-ALL ( -- )
   CHK-LOAD-ALL-ERRORS
   s" --json-errors" CHK-JSON-OPT
   CHK-ADD-LABEL-SOURCE ;

: CHK-RUN-CAPTURE ( -- )
   s" bin/hb" >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu erru rc :}
   rc RC>N CHK-RC !
   erru LEN>N CHK-ERR-U !
   outu LEN>N CHK-OUT-U ! ;

: CHK-REPLAY ( -- )
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

: CHK-RUN-STRICT ( -- )
   CHK-STRICT @ 0= if exit then
   CHK-ARGV-SIG
   CHK-RUN-CAPTURE
   CHK-OUT-BUF CHK-OUT-U @ CHK-ERR
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   CHK-RC @ 0 <> if CHK-RC @ CHK-THROW then ;

: CHK-RUN-BOUNDARY ( -- )
   CHK-ARGV-BOUNDARY
   CHK-RUN-CAPTURE
   CHK-RC @ 0= if exit then
   CHK-OUT-BUF CHK-OUT-U @ CHK-ERR
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   CHK-RC @ CHK-THROW ;

: CHK-RUN-TRUST-CURRENT ( -- )
   CHK-RUN-CAPTURE
   CHK-RC @ 0= if exit then
   CHK-OUT-BUF CHK-OUT-U @ CHK-ERR
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   CHK-RC @ CHK-THROW ;

: CHK-RUN-TRUST-PATH ( ptr u8 n -- )
   CHK-ARGV-TRUST-PATH
   CHK-RUN-TRUST-CURRENT ;

: CHK-RUN-TRUST-LIST ( -- )
   CHK-ARGV-TRUST-LIST
   CHK-RUN-TRUST-CURRENT ;

: CHK-RUN-TRUST ( -- )
   CHK-SOURCE-LIST @ if CHK-RUN-TRUST-LIST exit then
   CHK-ARGV-TRUST
   CHK-RUN-TRUST-CURRENT ;

: CHK-RUN-ALL ( -- )
   CHK-ARGV-ALL
   CHK-RUN-CAPTURE
   CHK-REPLAY
   CHK-RC @ CHK-THROW ;

: CHK-RUN-STATIC ( -- )
   CHK-ARGV-ALL
   CHK-RUN-CAPTURE
   CHK-RC @ 0= if exit then
   CHK-REPLAY
   CHK-RC @ CHK-THROW ;

: CHK-RUN-DIAG ( -- )
   CHK-ARGV-DIAG
   s" bin/hb" >LEN CHK-ORIGIN-BUF CHK-ORIGIN-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu erru rc :}
   rc RC>N CHK-RC !
   erru LEN>N CHK-ERR-U !
   outu LEN>N CHK-ORIGIN-U !
   CHK-RC @ 0 <> if
      CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
      CHK-RC @ CHK-THROW
   then ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPTURE ;

: CHK-WRITE-ERR-FILE ( -- )
   CHK-ERR-PATH CHK-ERR-BUF CHK-ERR-U @ WRITE-ALL
;

: CHK-RUN-JSON-FILE ( -- )
   CHK-ARGV-JSON-ONLY
   CHK-RUN-CAPTURE
   CHK-OUT-BUF CHK-OUT-U @ CHK-ERR
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

: CHK-RUN-JSON-ONLY ( -- )
   CHK-WRITE-ERR-FILE
   CHK-RUN-JSON-FILE ;

: CHK-HANDLE-HB ( -- )
   CHK-RC @ 0= if
      CHK-REPLAY
      CLEANUP-RUN
      exit
   then
   CHK-RC @ CHK-CHILD-RC !
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-JSON @ if
      CHK-WRITE-ERR-FILE
      CHK-RUN-STATIC
      CHK-RUN-JSON-FILE
   else
      CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   then
   CHK-CHILD-RC @ CHK-THROW ;

: CHECK-MAIN ( -- )
   CHK-PARSE
   CHK-MATERIALIZE
   CHK-RUN-BOUNDARY
   CHK-RUN-TRUST
   CHK-RUN-STRICT
   CHK-ALL @ if CHK-RUN-ALL then
   CHK-CHECK-LABEL
   CHK-RUN-DIAG
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

CHECK-MAIN
