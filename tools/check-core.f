\ check-core.f - the checked engine core behind tools/check.f: the pre-pass
\ (tools/check-verify-core.f) and the run stage in a child on the engine
\ src/lib/engine-candidate.f names, its output captured and replayed under the
\ subject's name. --verify-only stops before the run stage.

require src/core/exit-status.f
require src/lib/date.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f                \ the run stage spawns with an environment
require src/lib/engine-candidate.f           \ and on the engine this names
require src/lib/signal.f                     \ check.f answers SIGTERM, SIGINT and SIGHUP
require src/lib/fmt.f                        \ the answer names a step that throws
require src/lib/source.f
require src/lib/argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/byte-buffer.f
require src/lib/json.f
require src/lib/source-lex.f
require tools/json-only-core.f
require tools/check-all-errors-core.f    \ loads src/habu/verify-source.f
\ The require closure, and LSP-CHECK:VERIFY-BYTES: the check that stops before the
\ run stage, which --verify-only makes.
require tools/check-verify-core.f

package LSP-CHECK
using SOURCE                             \ the shared source-string emitters
using SOURCE-ROOT

$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
\ The run stage's child, and the verifier child of the pre-pass and of
\ --verify-only, has CHK-DEADLINE-MS unless --deadline-ms gives another. A
\ capture waits in poll(2), whose wait is an int of milliseconds, so no
\ deadline is longer than CHK-DEADLINE-MAX.
120000 constant CHK-DEADLINE-MS
$7FFFFFFF constant CHK-DEADLINE-MAX

0 constant CHK-SEL-NONE
1 constant CHK-SEL-SOURCE
2 constant CHK-SEL-FILE
3 constant CHK-SEL-LIST

10 constant CHK-LF
32 constant CHK-SP
45 constant CHK-DASH

create CHK-ROOT-BUF PATH-CAP allot
create CHK-SRC-PATH-BUF PATH-CAP allot
create CHK-RUN-PATH-BUF PATH-CAP allot
create CHK-SUBJECT-PATH-BUF PATH-CAP allot
create CHK-REPORT-PATH-BUF PATH-CAP allot
create CHK-SUBJ-BUF PATH-CAP allot
create CHK-SEL-LABEL-BUF PATH-CAP allot
create CHK-ONE 1 allot

\ The lazily allocated byte buffers hold addresses, so each is a declared
\ pointer cell: a raw `variable` would launder the address through a cell of
\ unknown type.
TYPED-VARIABLE CHK-OUT-BUF-A ^u8
TYPED-VARIABLE CHK-ERR-BUF-A ^u8

\ A buffer that holds a source, or text built from one, grows to what it
\ holds: the engine loads a source file of any size (src/core/include.f), so
\ the check refuses none for its size. The run file is the subject behind a
\ prefix.
DYNAMIC-BUFFER CHK-SRC-MEM u8
DYNAMIC-BUFFER CHK-RUN-MEM u8
DYNAMIC-BUFFER CHK-SEL-SRC-MEM u8
DYNAMIC-BUFFER CHK-REPORT-MEM u8
\ Each positional path whole: CHK-POS-MEM holds its bytes, CHK-POS-ROWS its
\ offset then length. CHK-POS-SET may move a span an earlier CHK-POS$ answered.
DYNAMIC-BUFFER CHK-POS-MEM u8
DYNAMIC-BUFFER CHK-POS-ROWS n
create CHK-JSON-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE CHK-JSON-W JSON:writer
variable CHK-JSON-READY

: CHK-JSON-WRITER ( -- ^JSON:writer )
   CHK-JSON-READY @ 0= if
      CHK-JSON-BUF 1 BUF:N>BLEN BUF:INIT
      1 CHK-JSON-READY !
   then
   CHK-JSON-W CHK-JSON-BUF JSON:WRITE-OPEN-BUF ;

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-POS-U
variable CHK-JSON
variable CHK-ALL
variable CHK-VERIFY
variable CHK-STDIN-PATH-A
variable CHK-STDIN-PATH-U
variable CHK-DEADLINE                    \ the --deadline-ms value, 0 when none was given
variable CHK-SEL-MODE
variable CHK-SEL-SRC-U
variable CHK-SEL-LABEL-U
variable CHK-SRC-U
variable CHK-RUN-U
variable CHK-OUT-U
variable CHK-ERR-U
variable CHK-RC
variable CHK-LABEL-A
variable CHK-LABEL-U
variable CHK-SRC-A
variable CHK-SRC-PATH-U
variable CHK-RUN-PATH-U
variable CHK-SUBJECT-PATH-U
variable CHK-REPORT-PATH-U
variable CHK-SUBJ-U                      \ a named file's canonical path, 0 for any other input
variable CHK-ROOT-U
variable CHK-EXP-OUT-U

: CHK-PTR-U8-FIELD ( ^a -- ^^u8 )
   0 ptr-field ;

: CHK-PTR-U8@ ( ^a -- ^u8 )
   CHK-PTR-U8-FIELD @ ;

: CHK-PTR-U8! ( ^u8 ^a -- )
   CHK-PTR-U8-FIELD ! ;

: CHK-ALLOC-BUF ( n -- ^u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

\ A ROOM word makes room for n bytes and answers its buffer and that room, at
\ least one byte so the answer is always an address. Growth may move the
\ buffer, so a caller takes its address after the room it needs.
: CHK-SRC-ROOM ( n -- ^u8 n )
   1 max dup CHK-SRC-MEM-RESERVE 0 CHK-SRC-MEM swap ;

: CHK-SRC-BUF ( -- ^u8 )
   1 CHK-SRC-ROOM drop ;

\ Reads the file at a path into CHK-SRC whole. The buffer may move as it grows,
\ so a caller takes CHK-SRC-BUF after the read.
: CHK-SRC-READ ( ^u8 n -- n )
   [: CHK-SRC-ROOM drop ;] READ-WHOLE-FILE ;

: CHK-RUN-ROOM ( n -- ^u8 n )
   1 max dup CHK-RUN-MEM-RESERVE 0 CHK-RUN-MEM swap ;

: CHK-RUN-BUF ( -- ^u8 )
   1 CHK-RUN-ROOM drop ;

: CHK-OUT-BUF ( -- ^u8 )
   CHK-OUT-BUF-A @ 0= if CHK-OUT-CAP CHK-ALLOC-BUF CHK-OUT-BUF-A ! then
   CHK-OUT-BUF-A @ ;

: CHK-ERR-BUF ( -- ^u8 )
   CHK-ERR-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-ERR-BUF-A ! then
   CHK-ERR-BUF-A @ ;

: CHK-SEL-SRC-ROOM ( n -- ^u8 n )
   1 max dup CHK-SEL-SRC-MEM-RESERVE 0 CHK-SEL-SRC-MEM swap ;

: CHK-SEL-SRC-BUF ( -- ^u8 )
   1 CHK-SEL-SRC-ROOM drop ;

: CHK-WRITE ( n ^u8 n -- ) {: fd:n a:^ u:n :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: CHK-OUT ( ^u8 n -- )
   1 -rot CHK-WRITE ;

: CHK-ERR ( ^u8 n -- )
   2 -rot CHK-WRITE ;

: CHK-C! ( n -- )
   CHK-ONE c! ;

: CHK-ERR-C ( n -- )
   CHK-C!
   2 CHK-ONE 1 CHK-WRITE ;

: CHK-ERR-LN ( ^u8 n -- )
   CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-OUT-C ( n -- )
   CHK-C!
   1 CHK-ONE 1 CHK-WRITE ;

: CHK-OUT-LN ( ^u8 n -- )
   CHK-OUT
   CHK-LF CHK-OUT-C ;

\ Prose that is no diagnostic goes to stdout under --verify-only, beside the
\ verifier's report, and under --json-errors, whose stderr carries packets
\ only; else to stderr.
: CHK-PROSE-OUT? ( -- bool )
   CHK-VERIFY @ CHK-JSON @ or 0<> ;

: CHK-EXPLAIN ( ^u8 n -- )
   CHK-PROSE-OUT? if CHK-OUT else CHK-ERR then ;

: CHK-EXPLAIN-LN ( ^u8 n -- )
   CHK-PROSE-OUT? if CHK-OUT-LN else CHK-ERR-LN then ;

\ A rejected command line is one record with no place: no source holds an
\ argument. CHK-USAGE-OPEN opens it and CHK-USAGE ends it, after any notes.
: CHK-USAGE-OPEN ( -- )
   CHK-JSON @ 0<> REJECTION:JSON !
   s" E-CHECK-USAGE" REJECTION:OPEN ;

: CHK-USAGE-ARG ( ^u8 n -- )
   96 REJECTION:EMIT1  REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

\ Opens the record with the message pre, the argument a u, then post.
: CHK-USAGE-SAY ( ^u8 n ^u8 n ^u8 n -- ) {: pre:^ preu:n a:^ u:n post:^ postu:n :}
   CHK-USAGE-OPEN
   pre preu REJECTION:EMIT-STR  a u CHK-USAGE-ARG  post postu REJECTION:EMIT-STR ;

: CHK-USAGE ( -- )
   REJECTION:HELP
   s" usage: tools/check.f [--json-errors] [--all-errors] [--deadline-ms ms] [--verify-only [--stdin-path path]] [--source-list file ... | prog.f]" REJECTION:EMIT-STR
   EXIT-STATUS:USAGE REJECTION:STOP ;

: CHK-REJECT-ARG ( ^u8 n ^u8 n ^u8 n -- )
   CHK-USAGE-SAY CHK-USAGE ;

: CHK-TWICE ( ^u8 n -- )
   NULL$ 2swap s"  is given twice" CHK-REJECT-ARG ;

: CHK-NO-VALUE ( ^u8 n -- )
   NULL$ 2swap s"  takes a value, and none follows it" CHK-REJECT-ARG ;

: CHK-THROW ( n -- )
   throw ;

: CHK-ARG$ ( n -- ^u8 n )
   SCRIPT-ARGV$ ;

: CHK-ARG= ( n ^u8 n -- bool ) {: idx:n a:^ u:n :}
   idx CHK-ARG$ a u LINT-STR= ;

\ --stdin-path and --deadline-ms each consume the next token as their value.
: CHK-VALUED-ARG? ( n -- bool ) {: idx:n :}
   idx s" --stdin-path" CHK-ARG= idx s" --deadline-ms" CHK-ARG= or ;

\ Establish the output stream before parsing can reject an earlier argument:
\ whether option a u is given. A valued option's value is skipped; -- ends
\ option parsing.
: CHK-OPT? ( ^u8 n -- bool )
   {: a:^ u:n :}
   0 begin dup SCRIPT-ARGC < while
      dup s" --" CHK-ARG= if drop false exit then
      dup CHK-VALUED-ARG? if
         2 +
      else
         dup a u CHK-ARG= if drop true exit then
         1+
      then
   repeat drop false ;

: CHK-DASH? ( ^u8 n -- bool ) {: a:^ u:n :}
   u 0 > if a c@ CHK-DASH = else 0 0= 0= then ;

\ Positional path idx; every caller passes an idx from 0 below CHK-POS-N.
: CHK-POS$ ( n -- ^u8 n ) {: idx:n :}
   idx 2 * CHK-POS-ROWS @ CHK-POS-MEM
   idx 2 * 1+ CHK-POS-ROWS @ ;

: CHK-POS-SET ( ^u8 n n -- ) {: a:^ u:n idx:n :}
   CHK-POS-U @ {: off:n :}
   off u + 1+ CHK-POS-MEM-RESERVE
   a off CHK-POS-MEM u BYTE-COPY
   off u + CHK-POS-U !
   idx 2 * {: row:n :}
   row 2 + CHK-POS-ROWS-RESERVE
   off row CHK-POS-ROWS !  u row 1+ CHK-POS-ROWS ! ;

: CHK-ADD-POS ( ^u8 n -- )
   CHK-POS-N @ CHK-POS-SET
   CHK-POS-N @ 1+ CHK-POS-N ! ;

\ The program a u comes after the one selected before it.
: CHK-SECOND ( ^u8 n -- )
   s" a second program " 2swap s" ; check.f checks one, or the files a `--source-list` names"
   CHK-USAGE-SAY
   CHK-SEL-MODE @ CHK-SEL-FILE = if s" first" REJECTION:NOTE  0 CHK-POS$ REJECTION:EMIT-STR then
   CHK-SEL-MODE @ CHK-SEL-SOURCE = if
      s" first" REJECTION:NOTE  CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ REJECTION:EMIT-STR
   then
   CHK-USAGE ;

: CHK-LIST-OPT ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-FILE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-LIST of endof
      s" `--source-list` after the source text " CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ NULL$
      CHK-REJECT-ARG
   endcase ;

: CHK-OPT ( ^u8 n -- ) {: a:^ u:n :}
   a u s" json-errors" LINT-STR= if LINT-TRUE CHK-JSON ! exit then
   a u s" all-errors" LINT-STR= if LINT-TRUE CHK-ALL ! exit then
   a u s" source-list" LINT-STR= if CHK-LIST-OPT exit then
   a u s" verify-only" LINT-STR= if LINT-TRUE CHK-VERIFY ! exit then
   s" unknown option " a u NULL$ CHK-REJECT-ARG ;

public

: OPT ( ^u8 n -- )
   CHK-OPT ;

: FILE ( ^u8 n -- ) {: path:^ pathu:n :}
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of
         path pathu CHK-ADD-POS
         CHK-SEL-FILE CHK-SEL-MODE !
      endof
      CHK-SEL-LIST of path pathu CHK-ADD-POS endof
      path pathu CHK-SECOND
   endcase ;

private

: CHK-PARSE-ONE ( ^u8 n -- ) {: a:^ u:n :}
   a u s" --json-errors" LINT-STR= if s" json-errors" OPT exit then
   a u s" --all-errors" LINT-STR= if s" all-errors" OPT exit then
   a u s" --source-list" LINT-STR= if s" source-list" OPT exit then
   a u s" --verify-only" LINT-STR= if s" verify-only" OPT exit then
   a u CHK-DASH? if s" unknown flag " a u NULL$ CHK-REJECT-ARG then
   a u FILE ;

: CHK-COLLECT-REST ( -- )
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ CHK-ARG$ FILE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-STDIN-PATH$ ( -- ^u8 n )
   CHK-STDIN-PATH-A CHK-PTR-U8@ CHK-STDIN-PATH-U @ ;

\ The path stdin's text stands for under --verify-only, given once.
: CHK-PARSE-STDIN-PATH ( -- )
   CHK-ARG-I @ 1+ CHK-ARG-I !
   CHK-ARG-I @ SCRIPT-ARGC >= if s" --stdin-path" CHK-NO-VALUE then
   CHK-STDIN-PATH-U @ 0<> if s" --stdin-path" CHK-TWICE then
   CHK-ARG-I @ CHK-ARG$ {: a:^ u:n :}
   u 0= if NULL$ s" --stdin-path" s"  names an empty path" CHK-REJECT-ARG then
   a CHK-STDIN-PATH-A CHK-PTR-U8!
   u CHK-STDIN-PATH-U ! ;

: CHK-BAD-DEADLINE ( ^u8 n -- ) {: a:^ u:n :}
   CHK-USAGE-OPEN
   s" `--deadline-ms` takes a whole number of milliseconds from 1 to " REJECTION:EMIT-STR
   CHK-DEADLINE-MAX REJECTION:EMIT-U  s" , not " REJECTION:EMIT-STR  a u CHK-USAGE-ARG
   CHK-USAGE ;

: CHK-DEADLINE-SET ( ^u8 n n -- ) {: a:^ u:n ms:n :}
   ms 1 < ms CHK-DEADLINE-MAX > or if a u CHK-BAD-DEADLINE then
   ms CHK-DEADLINE ! ;

\ The child's deadline in milliseconds, given once: a whole number from 1 to
\ CHK-DEADLINE-MAX.
: CHK-PARSE-DEADLINE ( -- )
   CHK-ARG-I @ 1+ CHK-ARG-I !
   CHK-ARG-I @ SCRIPT-ARGC >= if s" --deadline-ms" CHK-NO-VALUE then
   CHK-DEADLINE @ 0<> if s" --deadline-ms" CHK-TWICE then
   CHK-ARG-I @ CHK-ARG$ {: a:^ u:n :}
   a u STR>NUMBER? MATCH option
      none OF a u CHK-BAD-DEADLINE ENDOF
      some OF {: ms:n :} a u ms CHK-DEADLINE-SET ENDOF
   ;MATCH ;

: CHK-PARSE-AT ( -- )
   CHK-ARG-I @ s" --stdin-path" CHK-ARG= if CHK-PARSE-STDIN-PATH exit then
   CHK-ARG-I @ s" --deadline-ms" CHK-ARG= if CHK-PARSE-DEADLINE exit then
   CHK-ARG-I @ CHK-ARG$ CHK-PARSE-ONE ;

: CHK-PARSE ( -- )
   0 CHK-ARG-I !
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ s" --" CHK-ARG= if
         CHK-ARG-I @ 1+ CHK-ARG-I !
         CHK-COLLECT-REST
         exit
      then
      CHK-PARSE-AT
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-SELECT-CLEAR ( -- )
   CHK-SEL-NONE CHK-SEL-MODE !
   0 CHK-SEL-SRC-U !
   0 CHK-SEL-LABEL-U !
   0 CHK-POS-N !
   0 CHK-POS-U ! ;

: CHK-RUN-TEMP-CLEAR ( -- )
   0 CHK-SRC-U !
   0 CHK-RUN-U !
   0 CHK-OUT-U !
   0 CHK-ERR-U !
   0 CHK-RC !
   0 CHK-LABEL-U !
   NULL$ drop CHK-LABEL-A CHK-PTR-U8!
   NULL$ drop CHK-SRC-A CHK-PTR-U8!
   0 CHK-EXP-OUT-U !
   0 CHK-SUBJ-U ! ;

: CHK-RESET-CFG ( -- )
   0 CHK-ARG-I !
   0 CHK-JSON !
   0 CHK-ALL !
   0 CHK-VERIFY !
   0 CHK-STDIN-PATH-U !
   0 CHK-DEADLINE !
   CHK-SELECT-CLEAR
   CHK-RUN-TEMP-CLEAR ;

: CHK-COPY! ( ^u8 n ^u8 ^n -- ) {: a:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CHK-ROOT ( -- ^u8 n )
   CHK-ROOT-BUF CHK-ROOT-U @ ;

: CHK-SRC-PATH ( -- ^u8 n )
   CHK-SRC-PATH-BUF CHK-SRC-PATH-U @ ;

: CHK-RUN-PATH ( -- ^u8 n )
   CHK-RUN-PATH-BUF CHK-RUN-PATH-U @ ;

: CHK-SUBJECT-PATH ( -- ^u8 n )
   CHK-SUBJECT-PATH-BUF CHK-SUBJECT-PATH-U @ ;

: CHK-REPORT-PATH ( -- ^u8 n )
   CHK-REPORT-PATH-BUF CHK-REPORT-PATH-U @ ;

: CHK-SUBJ$ ( -- ^u8 n )
   CHK-SUBJ-BUF CHK-SUBJ-U @ ;

: CHK-SUBJ! ( ^u8 n -- ) {: a:^ u:n :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a CHK-SUBJ-BUF u BYTE-COPY
   u CHK-SUBJ-U ! ;

: CHK-TEMP-CLEAN ( -- )
   CHK-ROOT-U @ 0= if exit then
   CHK-ROOT 2dup FS:EXISTS? if FS:REMOVE-TREE else 2drop then
   CHK-ROOT FS:CLEANUP-FORGET
   0 CHK-ROOT-U !
   0 CHK-SRC-PATH-U !
   0 CHK-RUN-PATH-U !
   0 CHK-SUBJECT-PATH-U !
   0 CHK-REPORT-PATH-U ! ;

: CHK-SESSION-CLEAR ( -- )
   CHK-RESET-CFG ;

public

: RESET ( -- )
   [: CHK-TEMP-CLEAN ;] catch {: rc:n :}
   CHK-SESSION-CLEAR
   rc 0<> if rc throw then ;

private

: CHK-LABEL-STDIN ( -- )
   s" <stdin>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8! ;

: CHK-LABEL ( -- ^u8 n )
   CHK-LABEL-A CHK-PTR-U8@ CHK-LABEL-U @ ;

: CHK-SOURCE ( -- ^u8 n )
   CHK-SRC-A CHK-PTR-U8@ CHK-SRC-U @ ;

: CHK-SOURCE-BYTES ( -- ^u8 n )
   CHK-SRC-PATH CHK-SRC-READ
   CHK-SRC-BUF swap ;

: CHK-LABEL! ( ^u8 n -- ) {: a:^ u:n :}
   u CHK-LABEL-U !
   a CHK-LABEL-A CHK-PTR-U8! ;

: CHK-SOURCE! ( ^u8 n -- ) {: a:^ u:n :}
   u CHK-SRC-U !
   a CHK-SRC-A CHK-PTR-U8! ;

: CHK-ENTRY$ ( ^u8 n -- ^u8 n )
   ENTRY-RESOLVE drop ;

\ The --all-errors core writes its records into a new report file, in the run's
\ mode; the caller closes the descriptor answered and gives the core stderr back.
: CHK-ALL-STREAM ( -- fd )
   CHK-REPORT-PATH FS:MODE-0644 FS:CREATE-NEW {: file:fd :}
   file CHECK-ALL-ERRORS:STREAM!
   CHK-JSON @ REJECTION:JSON !
   file ;

: CHK-WRITE-EXPANDED-SOURCE ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-EXP-OUT-U @ LEN>N FS:WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-EXP-APP ( ^u8 n -- ) {: a:^ u:n :}
   a u >LEN
   CHK-EXP-OUT-U @ LEN>N u + CHK-SRC-ROOM >LEN CHK-EXP-OUT-U SOURCE-APPEND-BYTES ;

: CHK-EXP-C ( n -- )
   CHK-EXP-OUT-U @ LEN>N 1+ CHK-SRC-ROOM >LEN CHK-EXP-OUT-U SOURCE-APPEND-C ;

\ A record of check.f's own stands in no source: it names the file NO-FILE$, as the loader
\ names a missing entry, and is in the run's mode.
: CHK-OPEN ( ^u8 n -- ) {: code:^ codeu:n :}
   CHK-JSON @ REJECTION:JSON !
   REJECTION:NO-FILE$ REJECTION:FILE!
   code codeu REJECTION:OPEN ;

\ Input given as bytes, on stdin or through SOURCE, has no file to name.
: CHK-RERUN ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-FILE of
         s" run the program with `bin/hb " REJECTION:EMIT-STR  CHK-LABEL REJECTION:EMIT-STR
         s" `" REJECTION:EMIT-STR
      endof
      CHK-SEL-LIST of s" run each listed file alone with `bin/hb FILE`" REJECTION:EMIT-STR endof
      s" pipe the program to `bin/hb`" REJECTION:EMIT-STR
   endcase ;

: CHK-UNSAFE-BYTE? ( ^u8 -- bool ) {: p:^ :}
   [: p 1 SOURCE-QPATH-CHECK ;] catch {: rc:n :}
   rc E-FS-PATH-UNSAFE = if true exit then
   rc 0<> if rc throw then
   false ;

\ The offset of the first byte of a u SOURCE-QPATH-CHECK rejects, else u.
: CHK-UNSAFE-AT ( ^u8 n -- n ) {: a:^ u:n :}
   u 0 ?do  a i + CHK-UNSAFE-BYTE? if i unloop exit then  loop
   u ;

: CHK-BYTE-NAME ( n -- ) {: c:n :}
   c 34 = if s" a double quote" REJECTION:EMIT-STR exit then
   c 92 = if s" a backslash" REJECTION:EMIT-STR exit then
   c 10 = if s" a LF" REJECTION:EMIT-STR exit then
   c 13 = if s" a CR" REJECTION:EMIT-STR exit then
   c 0= if s" a NUL" REJECTION:EMIT-STR exit then
   s" the byte " REJECTION:EMIT-STR  c REJECTION:EMIT-U ;

\ w wu is what was judged, a path or a label; h hu says how to change it.
: CHK-PATH-UNSAFE ( ^u8 n ^u8 n ^u8 n -- ) {: a:^ u:n w:^ wu:n h:^ hu:n :}
   a u CHK-UNSAFE-AT {: at:n :}
   s" E-FS-PATH-UNSAFE" CHK-OPEN
   s" the " REJECTION:EMIT-STR  w wu REJECTION:EMIT-STR  s"  `" REJECTION:EMIT-STR
   a u REJECTION:EMIT-ESC
   s" ` holds a double quote, backslash, CR, LF or NUL, so the run cannot quote it" REJECTION:EMIT-STR
   s" found" REJECTION:NOTE
   a at + c@ CHK-BYTE-NAME  s"  at byte " REJECTION:EMIT-STR  at REJECTION:EMIT-U
   REJECTION:HELP  h hu REJECTION:EMIT-STR
   REJECTION:KEEP
   EXIT-STATUS:USAGE CHK-THROW ;

: CHK-QPATH-JUDGE ( ^u8 n ^u8 n ^u8 n -- ) {: a:^ u:n w:^ wu:n h:^ hu:n :}
   [: a u SOURCE-QPATH-CHECK ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-PATH-UNSAFE = if a u w wu h hu CHK-PATH-UNSAFE then
   rc throw ;

: CHK-EXP-QPATH ( ^u8 n -- ) {: a:^ u:n :}
   a u s" path" s" rename or move the file so its path holds none of them" CHK-QPATH-JUDGE
   a u >LEN
   CHK-EXP-OUT-U @ LEN>N u SOURCE-QPATH-BYTES + CHK-SRC-ROOM >LEN
   CHK-EXP-OUT-U SOURCE-APPEND-QPATH ;

\ A named file is loaded the way the command line loads it: the engine turns
\ `--load PATH` into `s" PATH" script-required`, which resolves PATH as an
\ entry, so the file's own relative requires resolve against its directory. The
\ path is the canonical one the file's closure entry holds, so the child's
\ working directory does not matter. The child's own JSON diagnostics for a file
\ it loads by path carry that file's line and column, which the engine counts
\ itself.
: CHK-APPEND-ENTRY ( ^u8 n -- )
   CHK-EXP-QPATH
   CHK-SP CHK-EXP-C
   s" script-required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

\ A source list loads its files as `bin/hb --load` loads the files it names:
\ each is an entry, so a listed file's own relative requires resolve against
\ its directory. A `required` would make the working directory the root of a
\ file below it (src/core/include.f SEARCH-ROOTS), and resolve them there.
\ Boot-owned dependencies are recorded portably relative to the current tree.
\ Materializing their canonical absolute spelling makes the child miss its boot
\ row and reload a family such as option; paths outside the tree remain
\ absolute and retain their distinct application identity.
: CHK-APPEND-LISTED ( ^u8 n -- )
   SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE CHK-APPEND-ENTRY ;

\ Standard input holds a source of any size, read to its end into CHK-SRC with
\ no size to start from.
: CHK-READ-STDIN ( -- n )
   0 [: CHK-SRC-ROOM drop ;] READ-WHOLE-FD ;

\ A source given as bytes, on stdin or through SOURCE, is the file CHK-SRC-PATH
\ the run loads, so its requires resolve as they will there.
: CHK-MATERIALIZE-BYTES ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ FS:WRITE-ALL
   CHK-SRC-PATH CHK-SOURCE! ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-READ-STDIN CHK-SRC-U !
   CHK-MATERIALIZE-BYTES ;

\ Whether q holds for every position. An input set that is all sources the
\ engine provides is rejected at each input: a run loads nothing from one.
: CHK-INPUTS-ALL? ( [ ^u8 n -- bool ] -- bool ) {: q :}
   CHK-POS-N @ 0 ?do
      i CHK-POS$ q execute 0= if unloop false exit then
   loop
   true ;

\ Position idx becomes its canonical path when it exists. A missing one stays
\ as written, so the composition meets it in its place, as the load does.
: CHK-RESOLVE-POS ( n -- bool ) {: idx:n :}
   idx CHK-POS$ {: a:^ u:n :}
   NULL-PTR 0 a u [: 2drop CANONICAL ;] LOADER-RESOLVE {: path:^ pathu:n exists:bool :}
   exists if path pathu idx CHK-POS-SET then
   exists ;

\ An existing input that is no file gets the load's record: a directory's
\ EISDIR, or for any other non-file the stamping reader's.
: CHK-NO-INPUT ( ^u8 n -- ) {: a:^ u:n :}
   NULL-PTR 0 a u CHK-IRREGULAR ;

: CHK-ENGINE-PROVIDED ( ^u8 n -- ) {: a:^ u:n :}
   s" E-ENGINE-PROVIDED" CHK-OPEN
   s" the engine provides `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
   s" `, so a check loads nothing from it" REJECTION:EMIT-STR
   REJECTION:HELP
   s" rebuild bin/hb to check a change to it" REJECTION:EMIT-STR
   REJECTION:KEEP ;

: CHK-CHECK-INPUTS ( -- )
   CHK-POS-N @ 0 ?do  i CHK-RESOLVE-POS drop  loop
   [: ENGINE-PROVIDES? ;] CHK-INPUTS-ALL? 0= if exit then
   CHK-POS-N @ 0 ?do  i CHK-POS$ CHK-ENGINE-PROVIDED  loop
   EXIT-STATUS:REJECTED CHK-THROW ;

\ The run stage quotes the label into REJECTION:LABEL! only after the stages
\ that read the source, so a label the materializer takes from its caller is
\ judged as soon as it is set. stdin and a source list carry fixed labels.
: CHK-CHECK-LABEL ( -- )
   CHK-LABEL s" label" s" pass a label that holds none of them" CHK-QPATH-JUDGE ;

\ Both spellings the run quotes, the entry in the loader line and the label,
\ are the canonical path.
: CHK-MATERIALIZE-FILE ( -- )
   CHK-CHECK-INPUTS
   0 CHK-POS$ CHK-LABEL!
   0 CHK-EXP-OUT-U !
   CHK-LABEL CHK-ENTRY$ CHK-SUBJ!
   CHK-SUBJ$ CHK-APPEND-ENTRY
   CHK-JSON @ if CHK-SUBJ$ CHK-LABEL! else CHK-CHECK-LABEL then
   CHK-WRITE-EXPANDED-SOURCE ;

: CHK-MATERIALIZE-SOURCE ( -- )
   CHK-SEL-SRC-U @ CHK-SRC-ROOM drop
   {: dst:^ :}
   CHK-SEL-SRC-BUF dst CHK-SEL-SRC-U @ BYTE-COPY
   CHK-SEL-SRC-U @ CHK-SRC-U !
   CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ CHK-LABEL!
   CHK-CHECK-LABEL
   CHK-MATERIALIZE-BYTES ;

: CHK-MATERIALIZE-LIST ( -- )
   CHK-POS-N @ 0= if NULL$ s" --source-list" s"  names no file" CHK-REJECT-ARG then
   CHK-CHECK-INPUTS
   s" <source-list>" CHK-LABEL!
   0 CHK-EXP-OUT-U !
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-APPEND-LISTED
      1+
   repeat drop
   CHK-WRITE-EXPANDED-SOURCE ;

public

: SOURCE ( ^u8 n ^u8 n -- )
   {: src:^ srcu:n label:^ labelu:n :}
   CHK-SEL-MODE @ CHK-SEL-NONE <> if label labelu CHK-SECOND then
   labelu PATH-CAP > if E-FS-CAPACITY throw then
   srcu CHK-SEL-SRC-ROOM drop
   {: dst:^ :}
   src dst srcu BYTE-COPY
   label CHK-SEL-LABEL-BUF labelu BYTE-COPY
   srcu CHK-SEL-SRC-U !
   labelu CHK-SEL-LABEL-U !
   CHK-SEL-SOURCE CHK-SEL-MODE ! ;

private

\ The root is canonical, as the engine names a loaded file, and registered for
\ removal at exit when held: a `die` below never unwinds to CHK-TEMP-CLEAN.
: CHK-MAKE-TEMP ( -- )
   s" habu-check" FS:MKTEMP-DIR SOURCE-ROOT:CANON-OS 0= if E-FS-IO throw then
   CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT FS:CLEANUP-TREE+
   CHK-ROOT s" source.f" CHK-SRC-PATH-BUF FS:JOIN-PATH CHK-SRC-PATH-U !
   CHK-ROOT s" run.f" CHK-RUN-PATH-BUF FS:JOIN-PATH CHK-RUN-PATH-U !
   CHK-ROOT s" subject.f" CHK-SUBJECT-PATH-BUF FS:JOIN-PATH CHK-SUBJECT-PATH-U !
   CHK-ROOT s" report" CHK-REPORT-PATH-BUF FS:JOIN-PATH CHK-REPORT-PATH-U !
   CHK-SUBJECT-PATH s" path"
   s" set TMPDIR to a directory whose path holds none of them" CHK-QPATH-JUDGE ;

: CHK-MATERIALIZE ( -- )
   CHK-MAKE-TEMP
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-MATERIALIZE-STDIN endof
      CHK-SEL-SOURCE of CHK-MATERIALIZE-SOURCE endof
      CHK-SEL-FILE of CHK-MATERIALIZE-FILE endof
      CHK-SEL-LIST of CHK-MATERIALIZE-LIST endof
      E-TBL-BOUNDS throw
   endcase ;

: CHK-RUN-RESET ( -- )
   0 CHK-RUN-U ! ;

: CHK-RUN+ ( ^u8 n -- ) {: a:^ u:n :}
   CHK-RUN-U @ u + CHK-RUN-ROOM 2drop
   a CHK-RUN-BUF CHK-RUN-U @ + u BYTE-COPY
   CHK-RUN-U @ u + CHK-RUN-U ! ;

: CHK-RUN-C ( n -- ) {: c:n :}
   CHK-RUN-U @ 1+ CHK-RUN-ROOM 2drop
   c CHK-RUN-BUF CHK-RUN-U @ + c!
   CHK-RUN-U @ 1+ CHK-RUN-U ! ;

: CHK-RUN-SP ( ^u8 n -- )
   CHK-RUN+
   CHK-SP CHK-RUN-C ;

: CHK-RUN-LN ( ^u8 n -- )
   CHK-RUN+
   CHK-LF CHK-RUN-C ;

: CHK-RUN-QPATH+ ( ^u8 n -- ) {: a:^ u:n :}
   a u >LEN
   CHK-RUN-U @ u SOURCE-QPATH-BYTES + CHK-RUN-ROOM >LEN
   CHK-RUN-U SOURCE-APPEND-QPATH ;

: CHK-SEL-BYTES? ( -- bool )
   CHK-SEL-MODE @ dup CHK-SEL-NONE = swap CHK-SEL-SOURCE = or ;

\ The run file's prefix names the subject for the checker's diagnostics.
: CHK-BUILD-PREFIX ( -- )
   CHK-LABEL CHK-RUN-QPATH+
   CHK-SEL-BYTES? if
      CHK-SP CHK-RUN-C  CHK-SRC-PATH CHK-RUN-QPATH+
      s"  REJECTION:FILE-LABEL!" CHK-RUN-SP
   else
      s"  REJECTION:LABEL!" CHK-RUN-SP
   then
   CHK-JSON @ if s" -1 REJECTION:JSON !" CHK-RUN-SP then ;

: CHK-RUN-SOURCE+ ( ^u8 n -- )
   CHK-SRC-READ {: len:n :}
   CHK-SRC-BUF len CHK-RUN+ ;

\ A copy of the subject, which the run reads for the subject's canonical path,
\ so a rejection in the subject names its own line and column.
: CHK-WRITE-SUBJECT ( -- )
   CHK-SUBJ$ CHK-SRC-READ {: len:n :}
   CHK-SUBJECT-PATH CHK-SRC-BUF len FS:WRITE-ALL ;

: CHK-BUILD-READER ( -- )
   CHK-LF CHK-RUN-C
   s" : CHECK-F-READ ( ^u8 n -- ^u8 n ) {: p:^ pu:n :}" CHK-RUN-LN
   s" p pu " CHK-RUN+  CHK-SUBJ$ CHK-RUN-QPATH+  s"  STR= if" CHK-RUN-LN
   CHK-SUBJECT-PATH CHK-RUN-QPATH+  s"  SOURCE-ROOT:READ-OS exit then" CHK-RUN-LN
   s" p pu SOURCE-ROOT:READ-OS ;" CHK-RUN-LN
   s" : CHECK-F-INPUT ( -- ) [: SOURCE-ROOT:CANON-OS ;] [: CHECK-F-READ ;] SOURCE-INPUT:USE ;" CHK-RUN-LN
   s" CHECK-F-INPUT" CHK-RUN-LN ;

: CHK-BUILD-ACT ( -- )
   CHK-SUBJ-U @ 0 > if CHK-WRITE-SUBJECT then
   CHK-BUILD-PREFIX
   CHK-SUBJ-U @ 0 > if CHK-BUILD-READER then
   CHK-SEL-BYTES? if
      CHK-LF CHK-RUN-C
      CHK-SRC-PATH CHK-RUN-QPATH+  s"  script-required" CHK-RUN-LN
      exit
   then
   CHK-SOURCE CHK-RUN-SOURCE+ ;

: CHK-BUILD-RUN ( -- )
   CHK-RUN-RESET
   CHK-BUILD-ACT ;

: CHK-LOAD-RESET ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+ ;

: CHK-DEADLINE@ ( -- ms )
   CHK-DEADLINE @ dup 0= if drop CHK-DEADLINE-MS then >MS ;

\ The checked program runs with check.f's own environment: PATH for a TOOL
\ lookup, HOME and TMPDIR for a scratch tree. An env-less spawn hands it a
\ one-NULL envp, and a program that resolves an executable through $PATH then
\ fails in the run stage while `--load` of the same file passes.
\
\ It runs on the engine src/lib/engine-candidate.f names: a gate's HABU_UNDER_TEST,
\ else the engine running check.f, never a bin/hb of the working directory.
: CHK-RUN-CAPTURE ( -- )
   PROC-ENV-INHERIT-MISSING
   CHK-ENGINE$ >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-DEADLINE@
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
             0 CHK-RC !
             erru LEN>N CHK-ERR-U !
             outu LEN>N CHK-OUT-U ! ENDOF
     err OF PCAP-FAILED:UNMAKE  {: outu:len erru:len rc:rc :}
             rc RC>N CHK-RC !
             erru LEN>N CHK-ERR-U !
             outu LEN>N CHK-OUT-U ! ENDOF
   ;MATCH ;

: CHK-SUBJECT ( ^u8 n -- ) {: a:^ u:n :}
   CHK-LABEL REJECTION:EMIT-STR  s" : " REJECTION:EMIT-STR  a u REJECTION:EMIT-STR ;

\ The run's output is captured to be replayed, CHK-OUT-CAP bytes of standard
\ output and CHK-ERR-CAP of standard error. A run that writes past either is
\ killed by the capture, which throws E-PROC-TRUNCATED without naming the
\ stream, so the record names both bounds.
: CHK-RUN-TOO-BIG ( -- )
   s" E-PROC-TRUNCATED" CHK-OPEN
   s" the run wrote past its capture" CHK-SUBJECT
   s" stdout" REJECTION:NOTE  s" at most " REJECTION:EMIT-STR
   CHK-OUT-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   s" stderr" REJECTION:NOTE  s" at most " REJECTION:EMIT-STR
   CHK-ERR-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   REJECTION:HELP
   CHK-RERUN  s"  to see its whole output, or make it write less" REJECTION:EMIT-STR
   REJECTION:KEEP
   EXIT-STATUS:REJECTED CHK-THROW ;

\ A child still going at its deadline: what a u names, the run or the verifier.
: CHK-LATE ( ^u8 n -- ) {: a:^ u:n :}
   s" E-PROC-TIMEOUT" CHK-OPEN
   a u CHK-SUBJECT  s"  passed its deadline" REJECTION:EMIT-STR
   s" deadline" REJECTION:NOTE
   CHK-DEADLINE@ MS>N REJECTION:EMIT-U  s"  ms" REJECTION:EMIT-STR
   REJECTION:HELP
   s" check it with a longer `--deadline-ms`, up to " REJECTION:EMIT-STR
   CHK-DEADLINE-MAX REJECTION:EMIT-U
   REJECTION:KEEP ;

\ What a run still going at its deadline wrote is not replayed.
: CHK-RUN-LATE ( -- )
   s" the run" CHK-LATE
   EXIT-STATUS:REJECTED CHK-THROW ;

: CHK-RUN-CAPPED ( -- )
   [: CHK-RUN-CAPTURE ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED = if CHK-RUN-TOO-BIG then
   rc E-PROC-TIMEOUT = if CHK-RUN-LATE then
   rc throw ;

: CHK-REPLAY ( -- )
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

\ Whether the run text is entry text, a named file's or a source list's.
: CHK-ENTRY-TEXT? ( -- bool )
   CHK-SEL-MODE @ CHK-SEL-FILE =  CHK-SEL-MODE @ CHK-SEL-LIST =  or ;

\ Definition verification follows the loader composition in one checker scope
\ and one multi-error session, so a definition a loaded file makes before its
\ loader's next statement is in scope there, and a rejected definition's
\ declared signature stands for it, as at a whole-file check. The composition
\ reads each file a loader loads, so it meets each lexer defect where it lies.
: CHK-RUN-ALL ( -- )
   CHK-ENTRY-TEXT? if
      CHK-SOURCE-BYTES CHK-SRC-PATH CHECK-ALL-ERRORS:COMPOSE-ENTRY exit
   then
   CHK-SOURCE-BYTES CHK-SRC-PATH CHK-LABEL CHECK-ALL-ERRORS:COMPOSE-BUF ;

: CHK-REPORT-ROOM ( n -- ^u8 n )
   1 max dup CHK-REPORT-MEM-RESERVE 0 CHK-REPORT-MEM swap ;

: CHK-REPORT-SEND ( -- )
   CHK-REPORT-PATH [: CHK-REPORT-ROOM drop ;] READ-WHOLE-FILE {: len:n :}
   1 CHK-REPORT-ROOM drop len CHK-ERR ;

\ A verifier child that exited or died of a signal before it answered.
: CHK-VERIFIER-ENDED ( ^u8 n n -- ) {: a:^ u:n v:n :}
   s" E-VERIFIER-ENDED" CHK-OPEN
   s" the verifier ended with " CHK-SUBJECT  a u REJECTION:EMIT-STR  v REJECTION:EMIT-U
   s"  before it answered" REJECTION:EMIT-STR
   REJECTION:HELP
   s" an engine defect stopped the verifier; " REJECTION:EMIT-STR
   CHK-RERUN  s"  to see where" REJECTION:EMIT-STR
   REJECTION:KEEP ;

\ How a verifier child that gave no answer ended, closing its report.
: CHK-VERIFIER-STATUS ( PROCESS:ending -- )
   MATCH PROCESS:ending
      exited OF {: v:n :} s" exit " v CHK-VERIFIER-ENDED ENDOF
      signaled OF {: v:n :} s" signal " v CHK-VERIFIER-ENDED ENDOF
      timeout OF s" the verifier" CHK-LATE ENDOF
   ;MATCH ;

\ check.f's verifier child runs alone, never in a batch, so its stderr cap is
\ VFY-ERR-CAP.
: CHK-VERIFIER-TRUNCATED ( -- )
   s" E-PROC-TRUNCATED" CHK-OPEN
   s" the verifier wrote past its capture of standard error" CHK-SUBJECT
   s" stderr" REJECTION:NOTE  s" at most " REJECTION:EMIT-STR
   VFY-ERR-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   REJECTION:HELP
   s" an engine defect wrote it, its stderr is shown above; " REJECTION:EMIT-STR
   CHK-RERUN
   REJECTION:KEEP ;

\ The verifier child's stderr as prose, its last line ended so a record after
\ it starts a line of its own.
: CHK-VERIFIER-LOG ( -- )
   VERIFY-LOG$ {: a:^ u:n :}
   u 0= if exit then
   a u CHK-EXPLAIN
   a u + 1- c@ CHK-LF <> if NULL$ CHK-EXPLAIN-LN then ;

\ The verifier child's records on stderr, then its stderr.
: CHK-VERIFIER-RELAY ( -- )
   VERIFY-OUT$ CHK-ERR
   CHK-VERIFIER-LOG ;

\ The verifier child's stderr comes first, and its records with it when it stops
\ the check: a pass's records are warnings, which the run writes.
: CHK-PREVERIFY-REPORT ( result<n,PROCESS:ending> -- )
   MATCH result
      ok OF {: rc:n :}
         rc 0= if CHK-VERIFIER-LOG else CHK-VERIFIER-RELAY rc CHK-THROW then
      ENDOF
      err OF CHK-VERIFIER-RELAY CHK-VERIFIER-STATUS EXIT-STATUS:UNAVAILABLE CHK-THROW ENDOF
   ;MATCH ;

: CHK-PREVERIFY-ACT ( -- )
   CHK-ENTRY-TEXT? if
      CHK-SOURCE-BYTES CHK-SRC-PATH CHK-DEADLINE@ CHK-JSON @ 0<> PREVERIFY-ENTRY
      CHK-PREVERIFY-REPORT exit
   then
   CHK-SOURCE-BYTES CHK-SRC-PATH CHK-LABEL CHK-DEADLINE@
   CHK-JSON @ 0<> PREVERIFY-BYTES
   CHK-PREVERIFY-REPORT ;

\ More output than the child's capture holds leaves no answer: the records and
\ stderr received before it, and a closing record.
: CHK-RUN-PREVERIFY ( -- )
   [: CHK-PREVERIFY-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED <> if rc throw then
   CHK-VERIFIER-RELAY
   CHK-VERIFIER-TRUNCATED
   EXIT-STATUS:UNAVAILABLE CHK-THROW ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ FS:WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPPED ;

\ The run's packets on stderr; its prose, when it wrote no packet, on stdout.
: CHK-RUN-JSON-ONLY ( -- )
   FD:STDERR FD:STDOUT JSON-ONLY-FDS!
   CHK-ERR-BUF CHK-ERR-U @ JSON-ONLY-FILTER ;

\ The run's output is routed the same way whether it passed or failed: under
\ --json-errors its stderr goes through CHK-RUN-JSON-ONLY, so stderr holds only
\ packets, else both streams are replayed as written.
: CHK-HANDLE-HB ( -- )
   CHK-JSON @ if
      CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
      CHK-RUN-JSON-ONLY
   else
      CHK-REPLAY
   then
   CHK-RC @ {: rc:n :}
   rc 0<> if rc CHK-THROW then ;

\ A composition that stops the check writes its report; a passing one's report
\ holds only warnings, which the run writes.
: CHK-RUN-ALL-ERRORS ( -- )
   CHK-ALL-STREAM {: file:fd :}
   [: CHK-RUN-ALL ;] catch {: rc:n :}
   file FD>N FS:CLOSE-FD drop
   FD:STDERR CHECK-ALL-ERRORS:STREAM!
   rc 0<> if CHK-REPORT-SEND rc throw then ;

: CHK-RUN-CURRENT ( -- )
   CHK-JSON @ REJECTION:JSON !
   CHK-ALL @ if CHK-RUN-ALL-ERRORS else CHK-RUN-PREVERIFY then
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

\ Outermost boundary of one check run. Everything inside it is work on the
\ subject source, so the whole run starts at neutral top level and the caller's
\ package is restored when the run ends, however it ends.
: CHK-RUN-SCOPED ( -- )
   [: CHK-RUN-CURRENT ;] CHECK:SCOPED ;

\ --verify-only is VERIFY-BYTES over a file's or stdin's bytes, nothing more:
\ stderr gets records in the run's mode, a closing one when no verdict came;
\ stdout gets prose.

: CHK-VERIFY-READ ( -- )
   0 CHK-POS$ CHK-SRC-READ CHK-SRC-U ! ;

\ A named file the file system refuses to read is the loader's rejection at its
\ canonical path; a missing one is the load's, named as written.
: CHK-VERIFY-FILE ( -- ^u8 n )
   0 CHK-RESOLVE-POS 0= if 0 CHK-POS$ NULL-PTR 0 true SOURCE-ROOT:MISSING-SOURCE then
   0 CHK-POS$ {: a:^ u:n :}
   a u FS:FILE? 0= if a u CHK-NO-INPUT then
   [: CHK-VERIFY-READ ;] CHK-READ-REJECTED {: rc:n e:n :}
   rc 0<> if a u rc e CHK-UNREADABLE-NAMED then
   a u ;

\ stdin's path need not exist.
: CHK-VERIFY-STDIN ( -- ^u8 n )
   CHK-STDIN-PATH-U @ 0= if
      NULL$ s" --verify-only" s"  names no program, and no `--stdin-path` for one on stdin"
      CHK-REJECT-ARG
   then
   CHK-READ-STDIN CHK-SRC-U !
   NULL-PTR 0 CHK-STDIN-PATH$ [: 2drop CANONICAL ;] LOADER-RESOLVE drop 2drop
   CHK-STDIN-PATH$ ;

\ Read the bytes into the source buffer: the path they stand for.
: CHK-VERIFY-SELECT ( -- ^u8 n )
   CHK-SEL-MODE @ CHK-SEL-FILE = if
      CHK-STDIN-PATH-U @ 0<> if
         s" both the program " 0 CHK-POS$ s"  and `--stdin-path`; `--verify-only` checks one"
         CHK-REJECT-ARG
      then
      CHK-VERIFY-FILE exit
   then
   CHK-SEL-MODE @ CHK-SEL-LIST = if
      NULL$ s" --verify-only" s"  checks one program, not the files a `--source-list` names"
      CHK-REJECT-ARG
   then
   CHK-SEL-MODE @ CHK-SEL-SOURCE = if
      NULL$ s" --verify-only" s"  checks a file or stdin, not the source text " CHK-USAGE-SAY
      CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ CHK-USAGE-ARG
      CHK-USAGE
   then
   CHK-VERIFY-STDIN ;

: CHK-VERIFY-REPORT ( verdict ^u8 n -- ) {: path:^ pathu:n :}
   CHK-VERIFIER-RELAY
   MATCH verdict
      verified OF ENDOF
      refused OF VERIFY-STATUS CHK-THROW ENDOF
      engine-provided OF
         path pathu CHK-ENGINE-PROVIDED  EXIT-STATUS:REJECTED CHK-THROW
      ENDOF
      held OF
         s" E-VERIFIER-HOLDS" CHK-OPEN
         s" the verifier's own image holds the source, so it cannot verify it" CHK-SUBJECT
         REJECTION:HELP
         CHK-RERUN
         REJECTION:KEEP
         EXIT-STATUS:UNAVAILABLE CHK-THROW
      ENDOF
      incomplete OF CHK-VERIFIER-STATUS EXIT-STATUS:UNAVAILABLE CHK-THROW ENDOF
      deferred OF
         s" check.f: a stretch or definition deferred to the run is not verified" CHK-OUT-LN
      ENDOF
   ;MATCH ;

\ The path verified is the label check.f's own records name.
: CHK-VERIFY-ACT ( -- )
   CHK-VERIFY-SELECT {: path:^ pathu:n :}
   path pathu CHK-LABEL!
   CHK-SRC-BUF CHK-SRC-U @ path pathu [: drop ;] CHK-DEADLINE@ CHK-JSON @ 0<> VERIFY-BYTES
   path pathu CHK-VERIFY-REPORT ;

\ More output than the verifier's capture holds leaves no verdict: the packets
\ received before it, the prose and a closing record.
: CHK-RUN-VERIFY ( -- )
   [: CHK-VERIFY-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED <> if rc throw then
   CHK-VERIFIER-RELAY
   CHK-VERIFIER-TRUNCATED
   EXIT-STATUS:UNAVAILABLE CHK-THROW ;

\ A record this process writes is in the run's mode.
: CHK-RUN-INNER ( -- )
   CHK-JSON @ 0<> REJECTION:JSON !
   CHK-VERIFY @ if CHK-RUN-VERIFY exit then
   CHK-STDIN-PATH-U @ 0<> if NULL$ s" --stdin-path" s"  is given without `--verify-only`" CHK-REJECT-ARG then
   CHK-MATERIALIZE
   CHK-RUN-SCOPED ;

public

\ The run between two cleans of the temporary directory; a failed final clean
\ clears the session. The earliest code is thrown on after every clean, from
\ the local its catch bound, so a rejection's saved record stays carried.
: RUN ( -- )
   CHK-RUN-TEMP-CLEAR
   [: CHK-TEMP-CLEAN ;] catch {: pre-rc:n :}
   pre-rc 0= if [: CHK-RUN-INNER ;] else [: ;] then catch {: run-rc:n :}
   [: CHK-TEMP-CLEAN ;] catch {: temp-rc:n :}
   temp-rc 0<> if CHK-SESSION-CLEAR else CHK-RUN-TEMP-CLEAR then
   pre-rc 0<> if pre-rc throw then
   run-rc 0<> if run-rc throw then
   temp-rc 0<> if temp-rc throw then ;

private

\ ---- a signal that stops check.f -----------------------------------------------
\ check.f answers SIGTERM, SIGINT and SIGHUP as the gate root does (src/lib/signal.f
\ CATCH-STOPS): under the default action its run stage's child, a process group of its own,
\ runs on under init beside the temporary directory, out of reach of a `timeout` that kills
\ check.f's group. MAIN catches the three before it makes that directory, the capture waiting
\ on the child hears them (src/lib/process.f PROCESS:STOP), and MAIN asks once more after the
\ run's cleanup. RUN catches nothing: a program that checks in process keeps its own answer.
\ The answer kills and reaps the child's processes (PROCESS:KILL-CAPTURE), removes the temporary
\ directory and dies of the signal; a step that throws is named and the answer goes on. A run
\ that ended before its child started leaves its pid 0 or PROCESS:NO-PID, and PROCESS:KILL-CAPTURE
\ kills nothing for either: kill(0) is check.f's own process group.
: CHK-SAY-THROW ( ^u8 n n -- ) {: what:^ whatu:n code:n :}
   code 0= if exit then
   s" check: " CHK-ERR what whatu CHK-ERR s"  threw " CHK-ERR
   SB-RESET code FMT:SB-INT SB$ CHK-ERR-LN ;

: CHK-SIGNAL-ANSWER ( n -- ) {: sig:n :}
   s" child kill" [: PROCESS:KILL-CAPTURE ;] catch CHK-SAY-THROW
   s" check: signal" sig SIGNAL:DIE-OF ;

: CHK-SIGNAL-CHECK ( -- )
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF CHK-SIGNAL-ANSWER ENDOF
      timeout OF ENDOF
   ;MATCH ;

: CHK-CATCH-STOPS ( -- )
   SIGNAL:CATCH-STOPS
   SIGNAL:FD FD>N PROCESS:STOP-FD !
   [: CHK-SIGNAL-CHECK ;] is PROCESS:STOP ;

: CHK-MAIN-RUN ( -- )
   RESET
   s" --verify-only" CHK-OPT? CHK-VERIFY !
   s" --json-errors" CHK-OPT? CHK-JSON !
   CHK-PARSE
   RUN ;

\ A throw that is no exit status ends the check with the engine's record of an
\ uncaught throw, in the mode the command line asks for: a run whose cleanup
\ failed clears the session's.
: CHK-MAIN-UNCAUGHT ( n -- n )
   {: rc:n :}
   rc 0 >= rc 256 < and if rc exit then
   s" --json-errors" CHK-OPT? REJECTION:JSON !
   rc ERROR:UNCAUGHT
   EXIT-STATUS:UNCAUGHT ;

public

: MAIN ( -- )
   CHK-CATCH-STOPS
   [: CHK-MAIN-RUN ;] catch CHK-MAIN-UNCAUGHT {: rc:n :}
   CHK-SIGNAL-CHECK
   rc 0 <> if rc throw then ;

;using
;using
;package
