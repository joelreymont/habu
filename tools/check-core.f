\ check-core.f - the checked engine core behind tools/check.f: the closure walk and pre-pass
\ (tools/check-verify-core.f), the lints, and the run stage in a child on the engine
\ src/lib/engine-candidate.f names, its output captured and replayed under the subject's name. --verify-only stops before the run stage.

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
require tools/diag-origin-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/check-all-errors-core.f    \ loads src/habu/verify-source.f
\ The require closure, and LSP-CHECK:VERIFY-BYTES: the check that stops before the
\ run stage, which --verify-only makes.
require tools/check-verify-core.f

package LSP-CHECK
using SOURCE                             \ the shared source-string emitters
using SOURCE-ROOT

$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
32 constant CHK-NUM-CAP
128 constant CHK-MAX-POS
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

create CHK-NUM-BUF CHK-NUM-CAP allot
create CHK-ROOT-BUF PATH-CAP allot
create CHK-SRC-PATH-BUF PATH-CAP allot
create CHK-RUN-PATH-BUF PATH-CAP allot
create CHK-MARK-PATH-BUF PATH-CAP allot
create CHK-SUBJ-BUF PATH-CAP allot
create CHK-SEL-LABEL-BUF PATH-CAP allot
create CHK-STDIN-PATH-BUF PATH-CAP allot
create CHK-POS-BUF CHK-MAX-POS PATH-CAP * allot
create CHK-POS-U CHK-MAX-POS cells allot
create CHK-ONE 1 allot

\ The lazily allocated byte buffers hold addresses, so each is a declared
\ pointer cell: a raw `variable` would launder the address through a cell of
\ unknown type.
TYPED-VARIABLE CHK-OUT-BUF-A ptr u8
TYPED-VARIABLE CHK-ERR-BUF-A ptr u8
TYPED-VARIABLE CHK-MAP-BUF-A ptr u8
TYPED-VARIABLE CHK-SCRATCH-BUF-A ptr u8

\ A buffer that holds a source, or text built from one, grows to what it
\ holds: the engine loads a source file of any size (src/core/include.f), so
\ the check refuses none for its size. The run file is the subject behind a
\ prefix, with an origin mark on each definition.
DYNAMIC-BUFFER CHK-SRC-MEM u8
DYNAMIC-BUFFER CHK-RUN-MEM u8
DYNAMIC-BUFFER CHK-SEL-SRC-MEM u8
create CHK-JSON-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE CHK-JSON-W JSON:writer
variable CHK-JSON-READY

: CHK-JSON-WRITER ( -- ptr JSON:writer )
   CHK-JSON-READY @ 0= if
      CHK-JSON-BUF 1 BUF:N>BLEN BUF:INIT
      1 CHK-JSON-READY !
   then
   CHK-JSON-W CHK-JSON-BUF JSON:WRITE-OPEN-BUF ;

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-JSON
variable CHK-ALL
variable CHK-VERIFY
variable CHK-STDIN-PATH-U
variable CHK-DEADLINE                    \ the --deadline-ms value, 0 when none was given
variable CHK-SEL-MODE
variable CHK-SEL-SRC-U
variable CHK-SEL-LABEL-U
variable CHK-SRC-U
variable CHK-RUN-U
variable CHK-OUT-U
variable CHK-ERR-U
variable CHK-MAP-U
variable CHK-RC
variable CHK-NUM-I
variable CHK-LABEL-A
variable CHK-LABEL-U
variable CHK-SRC-A
variable CHK-SRC-PATH-U
variable CHK-RUN-PATH-U
variable CHK-MARK-PATH-U
variable CHK-SUBJ-U                      \ a named file's canonical path, 0 for any other input
variable CHK-ROOT-U
variable CHK-EXP-OUT-U
variable CHK-ALL-ID
variable CHK-ALL-RC

: CHK-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: CHK-PTR-U8@ ( ptr a -- ptr u8 )
   CHK-PTR-U8-FIELD @ ;

: CHK-PTR-U8! ( ptr u8 ptr a -- )
   CHK-PTR-U8-FIELD ! ;

: CHK-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

\ A ROOM word makes room for n bytes and answers its buffer and that room, at
\ least one byte so the answer is always an address. Growth may move the
\ buffer, so a caller takes its address after the room it needs.
: CHK-SRC-ROOM ( n -- ptr u8 n )
   1 max dup CHK-SRC-MEM-RESERVE 0 CHK-SRC-MEM swap ;

: CHK-SRC-BUF ( -- ptr u8 )
   1 CHK-SRC-ROOM drop ;

\ Reads the file at a path into CHK-SRC whole, however it grows while it is
\ read (src/lib/source.f READ-WHOLE-FILE). The buffer may move as it grows, so a
\ caller takes CHK-SRC-BUF after the read.
: CHK-SRC-READ ( ptr u8 n -- n )
   [: CHK-SRC-ROOM drop ;] READ-WHOLE-FILE ;

: CHK-RUN-ROOM ( n -- ptr u8 n )
   1 max dup CHK-RUN-MEM-RESERVE 0 CHK-RUN-MEM swap ;

: CHK-RUN-BUF ( -- ptr u8 )
   1 CHK-RUN-ROOM drop ;

\ The scratch where the all-errors core renders a pass's diagnostics has that
\ core's bound (tools/check-all-errors-core.f SCRATCH-CAP).
: CHK-SCRATCH-BUF ( -- ptr u8 )
   CHK-SCRATCH-BUF-A @ 0= if
      CHECK-ALL-ERRORS:SCRATCH-CAP CHK-ALLOC-BUF CHK-SCRATCH-BUF-A !
   then
   CHK-SCRATCH-BUF-A @ ;

: CHK-OUT-BUF ( -- ptr u8 )
   CHK-OUT-BUF-A @ 0= if CHK-OUT-CAP CHK-ALLOC-BUF CHK-OUT-BUF-A ! then
   CHK-OUT-BUF-A @ ;

: CHK-ERR-BUF ( -- ptr u8 )
   CHK-ERR-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-ERR-BUF-A ! then
   CHK-ERR-BUF-A @ ;

: CHK-MAP-BUF ( -- ptr u8 )
   CHK-MAP-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-MAP-BUF-A ! then
   CHK-MAP-BUF-A @ ;

: CHK-SEL-SRC-ROOM ( n -- ptr u8 n )
   1 max dup CHK-SEL-SRC-MEM-RESERVE 0 CHK-SEL-SRC-MEM swap ;

: CHK-SEL-SRC-BUF ( -- ptr u8 )
   1 CHK-SEL-SRC-ROOM drop ;

: CHK-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
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

: CHK-OUT-C ( n -- )
   CHK-C!
   1 CHK-ONE 1 CHK-WRITE ;

: CHK-OUT-LN ( ptr u8 n -- )
   CHK-OUT
   CHK-LF CHK-OUT-C ;

\ Prose that is no diagnostic goes to stdout under --verify-only, beside the
\ verifier's report, and under --json-errors, whose stderr carries packets
\ only; else to stderr.
: CHK-PROSE-OUT? ( -- bool )
   CHK-VERIFY @ CHK-JSON @ or 0<> ;

: CHK-EXPLAIN ( ptr u8 n -- )
   CHK-PROSE-OUT? if CHK-OUT else CHK-ERR then ;

: CHK-EXPLAIN-LN ( ptr u8 n -- )
   CHK-PROSE-OUT? if CHK-OUT-LN else CHK-ERR-LN then ;

: CHK-USAGE ( -- )
   s" usage: tools/check.f [--json-errors] [--all-errors] [--deadline-ms ms] [--verify-only [--stdin-path path]] [--source-list file ... | prog.f]" CHK-EXPLAIN-LN
   CHK-E-USAGE throw ;

: CHK-THROW ( n -- )
   throw ;

: CHK-FAIL ( ptr u8 n n -- ) {: msg:ptr u:n code:n :}
   msg u CHK-EXPLAIN-LN
   code CHK-THROW ;

: CHK-PATH-TOO-BIG ( -- )
   s" check.f: source path exceeds capacity" CHK-E-USAGE CHK-FAIL ;

: CHK-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: CHK-ARG= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx CHK-ARG$ a u LINT-STR= ;

\ --stdin-path and --deadline-ms each consume the next token as their value.
: CHK-VALUED-ARG? ( n -- bool ) {: idx:n :}
   idx s" --stdin-path" CHK-ARG= idx s" --deadline-ms" CHK-ARG= or ;

\ Establish the output stream before parsing can reject an earlier argument:
\ whether option a u is given. A valued option's value is skipped; -- ends
\ option parsing.
: CHK-OPT? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   0 begin dup SCRIPT-ARGC < while
      dup s" --" CHK-ARG= if drop false exit then
      dup CHK-VALUED-ARG? if
         2 +
      else
         dup a u CHK-ARG= if drop true exit then
         1+
      then
   repeat drop false ;

: CHK-DASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ CHK-DASH = else 0 0= 0= then ;

: CHK-POS-SLOT ( n -- ptr u8 )
   PATH-CAP * CHK-POS-BUF + ;

: CHK-POS-U-SLOT ( n -- ptr n )
   cells CHK-POS-U + ;

: CHK-POS$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if CHK-USAGE then
   idx CHK-POS-N @ >= if CHK-USAGE then
   idx CHK-POS-SLOT
   idx CHK-POS-U-SLOT @ ;

: CHK-ADD-POS ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   CHK-POS-N @ CHK-MAX-POS >= if CHK-USAGE then
   a CHK-POS-N @ CHK-POS-SLOT u BYTE-COPY
   u CHK-POS-N @ CHK-POS-U-SLOT !
   CHK-POS-N @ 1+ CHK-POS-N ! ;

: CHK-LIST-OPT ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-FILE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-LIST of endof
      CHK-USAGE
   endcase ;

: CHK-OPT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" json-errors" LINT-STR= if LINT-TRUE CHK-JSON ! exit then
   a u s" all-errors" LINT-STR= if LINT-TRUE CHK-ALL ! exit then
   a u s" source-list" LINT-STR= if CHK-LIST-OPT exit then
   a u s" verify-only" LINT-STR= if LINT-TRUE CHK-VERIFY ! exit then
   CHK-USAGE ;

public

: OPT ( ptr u8 n -- )
   CHK-OPT ;

: FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of
         path pathu CHK-ADD-POS
         CHK-SEL-FILE CHK-SEL-MODE !
      endof
      CHK-SEL-LIST of path pathu CHK-ADD-POS endof
      CHK-USAGE
   endcase ;

private

: CHK-PARSE-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" --json-errors" LINT-STR= if s" json-errors" OPT exit then
   a u s" --all-errors" LINT-STR= if s" all-errors" OPT exit then
   a u s" --source-list" LINT-STR= if s" source-list" OPT exit then
   a u s" --verify-only" LINT-STR= if s" verify-only" OPT exit then
   a u CHK-DASH? if CHK-USAGE then
   a u FILE ;

: CHK-COLLECT-REST ( -- )
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ CHK-ARG$ FILE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-STDIN-PATH$ ( -- ptr u8 n )
   CHK-STDIN-PATH-BUF CHK-STDIN-PATH-U @ ;

\ The path stdin's text stands for under --verify-only, given once.
: CHK-PARSE-STDIN-PATH ( -- )
   CHK-ARG-I @ 1+ CHK-ARG-I !
   CHK-ARG-I @ SCRIPT-ARGC >= if CHK-USAGE then
   CHK-STDIN-PATH-U @ 0<> if CHK-USAGE then
   CHK-ARG-I @ CHK-ARG$ {: a:ptr u:n :}
   u 0= if CHK-USAGE then
   u PATH-CAP > if E-FS-CAPACITY throw then
   a CHK-STDIN-PATH-BUF u BYTE-COPY
   u CHK-STDIN-PATH-U ! ;

: CHK-DEADLINE-SET ( n -- ) {: ms:n :}
   ms 1 < ms CHK-DEADLINE-MAX > or if CHK-USAGE then
   ms CHK-DEADLINE ! ;

\ The child's deadline in milliseconds, given once: a whole number from 1 to
\ CHK-DEADLINE-MAX.
: CHK-PARSE-DEADLINE ( -- )
   CHK-ARG-I @ 1+ CHK-ARG-I !
   CHK-ARG-I @ SCRIPT-ARGC >= if CHK-USAGE then
   CHK-DEADLINE @ 0<> if CHK-USAGE then
   CHK-ARG-I @ CHK-ARG$ STR>NUMBER? MATCH option
      none OF CHK-USAGE ENDOF
      some OF CHK-DEADLINE-SET ENDOF
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

\ CLI arguments have two path slots; keep their library capacity throw for
\ direct LSP-CHECK:FILE callers, and explain it only at this command-line boundary.
: CHK-PARSE-CLI ( -- )
   [: CHK-PARSE ;] catch {: rc:n :}
   rc E-FS-CAPACITY = if CHK-PATH-TOO-BIG then
   rc 0<> if rc throw then ;

: CHK-POS-LENS-CLEAR ( -- )
   0 begin dup CHK-MAX-POS < while
      0 over CHK-POS-U-SLOT !
      1+
   repeat drop ;

: CHK-SELECT-CLEAR ( -- )
   CHK-SEL-NONE CHK-SEL-MODE !
   0 CHK-SEL-SRC-U !
   0 CHK-SEL-LABEL-U !
   0 CHK-POS-N !
   CHK-POS-LENS-CLEAR ;

: CHK-RUN-TEMP-CLEAR ( -- )
   0 CHK-SRC-U !
   0 CHK-RUN-U !
   0 CHK-OUT-U !
   0 CHK-ERR-U !
   0 CHK-RC !
   0 CHK-NUM-I !
   0 CHK-LABEL-U !
   NULL$ drop CHK-LABEL-A CHK-PTR-U8!
   NULL$ drop CHK-SRC-A CHK-PTR-U8!
   0 CHK-EXP-OUT-U !
   0 CHK-SUBJ-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N !
   0 CHK-DISC-ID !
   0 CHK-ALL-ID !
   0 CHK-ALL-RC ! ;

: CHK-RESET-CFG ( -- )
   0 CHK-ARG-I !
   0 CHK-JSON !
   0 CHK-ALL !
   0 CHK-VERIFY !
   0 CHK-STDIN-PATH-U !
   0 CHK-DEADLINE !
   CHK-SELECT-CLEAR
   CHK-RUN-TEMP-CLEAR ;

: CHK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CHK-ROOT ( -- ptr u8 n )
   CHK-ROOT-BUF CHK-ROOT-U @ ;

: CHK-SRC-PATH ( -- ptr u8 n )
   CHK-SRC-PATH-BUF CHK-SRC-PATH-U @ ;

: CHK-RUN-PATH ( -- ptr u8 n )
   CHK-RUN-PATH-BUF CHK-RUN-PATH-U @ ;

: CHK-MARK-PATH ( -- ptr u8 n )
   CHK-MARK-PATH-BUF CHK-MARK-PATH-U @ ;

: CHK-SUBJ$ ( -- ptr u8 n )
   CHK-SUBJ-BUF CHK-SUBJ-U @ ;

: CHK-SUBJ! ( ptr u8 n -- ) {: a:ptr u:n :}
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
   0 CHK-MARK-PATH-U ! ;

: CHK-SESSION-CLEAR ( -- )
   CHK-RESET-CFG ;

: CHK-FIRST-RC ( n n -- n ) {: rc:n next-rc:n :}
   rc 0 <> if
      \ One public operation returns one code, so simultaneous propagation is
      \ impossible; the earliest primary code has precedence.
      rc exit
   then
   next-rc ;

public

: RESET ( -- )
   false CHK-ENGINE-REFUSED !
   [: CHK-TEMP-CLEAN ;] catch {: rc:n :}
   CHK-SESSION-CLEAR
   [: CHECKED-BOUNDARY-LINT:RESET ;] catch {: provider-rc:n :}
   rc provider-rc CHK-FIRST-RC
   dup 0 <> if throw then drop ;

private

\ The root is spelled canonically: the engine names a file it loads by its
\ canonical path, so the run file's name in the run's diagnostics is the one
\ CHK-RUN-PATH holds (CHK-ERR-NAME-SUBJECT). It is registered for removal at
\ process exit as soon as it is held: a `die` below - a lint library's, the
\ verifier's, the engine's - ends the process without unwinding to
\ CHK-TEMP-CLEAN, which forgets the root once it has removed it.
: CHK-MAKE-TEMP ( -- )
   s" habu-check" FS:MKTEMP-DIR SOURCE-ROOT:CANON-OS 0= if E-FS-IO throw then
   CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT FS:CLEANUP-TREE+
   CHK-ROOT s" source.f" CHK-SRC-PATH-BUF FS:JOIN-PATH CHK-SRC-PATH-U !
   CHK-ROOT s" run.f" CHK-RUN-PATH-BUF FS:JOIN-PATH CHK-RUN-PATH-U !
   CHK-ROOT s" subject.f" CHK-MARK-PATH-BUF FS:JOIN-PATH CHK-MARK-PATH-U ! ;

: CHK-LABEL-STDIN ( -- )
   s" <stdin>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8! ;

: CHK-LABEL ( -- ptr u8 n )
   CHK-LABEL-A CHK-PTR-U8@ CHK-LABEL-U @ ;

: CHK-SOURCE ( -- ptr u8 n )
   CHK-SRC-A CHK-PTR-U8@ CHK-SRC-U @ ;

: CHK-SOURCE-BYTES ( -- ptr u8 n )
   CHK-SRC-PATH CHK-SRC-READ
   CHK-SRC-BUF swap ;

: CHK-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-LABEL-U !
   a CHK-LABEL-A CHK-PTR-U8! ;

: CHK-SOURCE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-SRC-U !
   a CHK-SRC-A CHK-PTR-U8! ;

: CHK-SINGLE-FILE? ( -- bool )
   CHK-SEL-MODE @ CHK-SEL-FILE = ;

\ The lints read a named file where it lies, and any other input from the copy
\ CHK-MATERIALIZE wrote.
: CHK-LINT-SOURCE ( -- ptr u8 n )
   CHK-SINGLE-FILE? if 0 CHK-POS$ exit then
   CHK-SOURCE ;

: CHK-DISC-FAIL ( n -- )
   s" check.f: " CHK-ERR
   CHK-DISC-MSG$ CHK-E-CHECK CHK-FAIL ;

\ The --all-errors core writes its records to standard error in the run's mode.
: CHK-ALL-STREAM ( -- )
   FD:STDERR CHK-SCRATCH-BUF CHECK-ALL-ERRORS:SCRATCH-CAP CHECK-ALL-ERRORS:STREAM!
   CHK-JSON @ CHECK-ALL-ERRORS:JSON! ;

\ The pre-pass or discovery stops in one file of the composition: the subject,
\ by its label, or any other file, by its canonical path. Its bytes are the
\ subject's, or the file's own, read into CHK-SRC-BUF.
: CHK-STOP-BYTES ( ptr u8 n bool -- ptr u8 n )
   {: a:ptr u:n subj:bool :}
   subj if CHK-SOURCE-BYTES exit then
   a u CHK-SRC-READ CHK-SRC-BUF swap ;

\ What stopped the pre-pass or discovery with the given code, at the given byte
\ of the given bytes of the file the label names, by its record in the run's
\ mode (STOP-RECORD$): the check fails as a refusal when the record is one.
: CHK-STOP-WRITE ( n n ptr u8 n ptr u8 n -- )
   CHK-JSON @ STOP-RECORD$ {: rec:ptr recu:n refusal:bool :}
   recu 0<> if rec recu CHK-ERR-LN then
   refusal if CHK-E-CHECK CHK-THROW then ;

\ Discovery stops at a string or a locals group a file never closes, at its
\ opener in the file that ended the walk (CHK-DISC-ID), the subject's bytes
\ under their label among them, and is reported there: a string by the lexer's
\ record, and an end the lexer does not see, such as a `{:` group left open, by
\ the record of a statement that throws discovery's code. The check fails as a
\ refusal.
: CHK-DISC-STOPPED ( -- )
   CHK-DISC-ID @ {: id:n :}
   E-DISC-UNTERM DISCOVER:OPENER-AT id CHK-FILE-NAME$
   id CHK-DEP$ id CHK-BYTES-ID @ = CHK-STOP-BYTES CHK-STOP-WRITE
   CHK-E-CHECK CHK-THROW ;

\ A closure the walk cannot follow fails the check as a refusal. Under
\ --json-errors it is the walk's packet (CHK-FAULT$), placed at the form
\ discovery refused or at the loader word naming a file that is not there or
\ cannot be read; otherwise, or for a walk that ended at its own top, it is
\ prose.
: CHK-EXPAND-JSON ( n -- )
   CHK-FAULT$ {: a:ptr u:n :}
   u 0= if exit then
   a u CHK-ERR-LN
   CHK-E-CHECK CHK-THROW ;

\ A file the file system refused to read (CHK-READ-RC), by its canonical path:
\ the one the walk was reading, or --verify-only's named file.
: CHK-READ-FAIL ( ptr u8 n -- )
   s" check.f: cannot read " CHK-EXPLAIN
   CHK-E-IOERR CHK-FAIL ;

: CHK-EXPAND-REPORT ( n -- )
   {: rc:n :}
   rc 0= if exit then
   rc E-DISC-UNTERM = if CHK-DISC-STOPPED then
   CHK-JSON @ if rc CHK-EXPAND-JSON then
   rc CHK-DISC-RC? if rc CHK-DISC-FAIL then
   rc CHK-E-NOINPUT = if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   rc CHK-E-IOERR = if CHK-DISC-ID @ CHK-DEP$ CHK-READ-FAIL then
   rc throw ;

: CHK-ENTRY-ID ( ptr u8 n -- n )
   ENTRY-RESOLVE drop RESOLVED-ROOT$ CHK-DEP-ID ;

: CHK-EXPAND-PATH ( ptr u8 n -- )
   CHK-ENTRY-ID CHK-EXPAND CHK-EXPAND-REPORT ;

: CHK-WRITE-EXPANDED-SOURCE ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-EXP-OUT-U @ LEN>N FS:WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-EXP-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u >LEN
   CHK-EXP-OUT-U @ LEN>N u + CHK-SRC-ROOM >LEN CHK-EXP-OUT-U SOURCE-APPEND-BYTES ;

: CHK-EXP-C ( n -- )
   CHK-EXP-OUT-U @ LEN>N 1+ CHK-SRC-ROOM >LEN CHK-EXP-OUT-U SOURCE-APPEND-C ;

: CHK-EXP-QPATH ( ptr u8 n -- ) {: a:ptr u:n :}
   a u >LEN
   CHK-EXP-OUT-U @ LEN>N u SOURCE-QPATH-BYTES + CHK-SRC-ROOM >LEN
   CHK-EXP-OUT-U SOURCE-APPEND-QPATH ;

\ A named file is loaded the way the command line loads it: the engine turns
\ `--load PATH` into `s" PATH" script-required`, which resolves PATH as an
\ entry, so the file's own relative requires resolve against its directory. The
\ path is the canonical one the file's closure entry holds, so the child's
\ working directory does not matter. The child's own JSON diagnostics for a file
\ it loads by path carry that file's line and column, which the engine counts
\ itself (src/core/checker.f CHECK:FILE-POS).
: CHK-APPEND-ENTRY ( ptr u8 n -- )
   CHK-EXP-QPATH
   CHK-SP CHK-EXP-C
   s" script-required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

\ A source list loads its files as `bin/hb --load` loads the files it names:
\ each is an entry, so a listed file's own relative requires resolve against its
\ directory, as discovery walks it (CHK-ENTRY-ID). A `required` would make the
\ working directory the root of a file below it (src/core/include.f
\ SEARCH-ROOTS), and resolve them there. Boot-owned dependencies are recorded
\ portably relative to the current tree. Materializing their canonical absolute
\ spelling makes the child miss its boot row and reload a family such as
\ option; paths outside the tree remain absolute and retain their distinct
\ application identity.
: CHK-APPEND-LISTED ( ptr u8 n -- )
   SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE CHK-APPEND-ENTRY ;

\ Standard input holds a source of any size, read to its end into CHK-SRC
\ (src/lib/source.f READ-WHOLE-FD), with no size to start from.
: CHK-READ-STDIN ( -- n )
   0 0 [: CHK-SRC-ROOM drop ;] READ-WHOLE-FD ;

\ A source given as bytes, on stdin or through SOURCE, is the file CHK-SRC-PATH
\ the run loads, so its requires resolve as they will there. Its closure is
\ walked as a named file's is, and the passes after the walk visit the order it
\ leaves as they visit a named file's, that file under its label.
: CHK-MATERIALIZE-BYTES ( -- )
   CHK-SRC-BUF CHK-SRC-U @ {: a:ptr u:n :}
   CHK-SRC-PATH a u FS:WRITE-ALL
   a u CHK-SRC-PATH CHK-LABEL CHK-EXPAND-BYTES CHK-EXPAND-REPORT
   CHK-SRC-PATH CHK-SOURCE! ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-READ-STDIN CHK-SRC-U !
   CHK-MATERIALIZE-BYTES ;

\ E-FS-PATH-UNSAFE: the quoting judge, SOURCE-QPATH-CHECK, refuses a double
\ quote, backslash, CR, LF or NUL. E-FFI-NUL: the file system refuses a NUL in
\ a path.
: CHK-PATH-UNSAFE ( -- )
   s" check.f: source path or label contains a double quote, backslash, CR, LF or NUL" CHK-E-USAGE CHK-FAIL ;

\ Every input must be a file before the engine's resolver sees it. The
\ engine's resolver refuses a path holding a NUL with E-PATH-NUL, which check.f
\ does not map; FS:FILE? refuses it first with E-FFI-NUL, which CHK-MATERIALIZE
\ reports as usage.
\
\ The engine carries its own sources, so a run loads nothing from one and checks
\ nothing there; rebuilding the engine checks it. An input set that is all such
\ sources - a single file is a set of one - is refused at each input, as the
\ input is given. Any other input is checked even when this process has loaded
\ it: the pre-pass verifies it in the verifier child and the run loads it, each
\ in an engine of its own.
: CHK-INPUTS-ALL? ( [ ptr u8 n -- bool ] -- bool ) {: q :}
   CHK-POS-N @ 0 ?do
      i CHK-POS$ q execute 0= if unloop false exit then
   loop
   true ;

: CHK-ENGINE-SUG$ ( -- ptr u8 n )
   s" The engine provides this source; rebuild bin/hb to check a change to it." ;

: CHK-ENGINE-JSON ( ptr u8 n -- ) {: path:ptr pathu:n :}
   CHK-JSON-WRITER JSON:WRITE-OBJECT-START
   s" schema_version" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" code" s" E-ENGINE-PROVIDED" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" repair_class" s" rebuild_engine" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" verdict" s" uncheckable" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" file" path pathu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" line" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" column" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" suggestion" CHK-ENGINE-SUG$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END JSON:WRITTEN$ CHK-ERR-LN ;

: CHK-ENGINE-PROSE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" E-ENGINE-PROVIDED " CHK-ERR
   path pathu CHK-ERR
   s" :1:1: " CHK-ERR
   CHK-ENGINE-SUG$ CHK-ERR-LN ;

: CHK-CHECK-INPUTS ( -- )
   [: FS:FILE? ;] CHK-INPUTS-ALL? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   [: ENGINE-PROVIDES? ;] CHK-INPUTS-ALL? 0= if exit then
   CHK-POS-N @ 0 ?do
      i CHK-POS$ CHK-JSON @ if CHK-ENGINE-JSON else CHK-ENGINE-PROSE then
   loop
   CHK-E-USAGE CHK-THROW ;

\ The run stage quotes the label into DIAG-FILE! only after the stages that
\ read the source, so a label the materializer takes from its caller is judged
\ as soon as it is set. stdin and a source list carry fixed labels.
: CHK-CHECK-LABEL ( -- )
   CHK-LABEL SOURCE-QPATH-CHECK ;

\ A JSON packet names the file by the canonical absolute path its closure entry
\ holds, the spelling every dependency's packets carry, so one run names each
\ file one way and a client can map it to a URI. Prose keeps the path as given.
\ Both spellings the run quotes, the entry in the loader line and the label,
\ are judged before discovery reads the file.
: CHK-MATERIALIZE-FILE ( -- )
   0 CHK-POS$ CHK-LABEL!
   CHK-CHECK-INPUTS
   CHK-EXPAND-RESET
   0 CHK-EXP-OUT-U !
   CHK-LABEL CHK-ENTRY-ID {: id:n :}
   id CHK-DEP$ CHK-APPEND-ENTRY
   id CHK-DEP$ CHK-SUBJ!
   CHK-JSON @ if CHK-SUBJ$ CHK-LABEL! else CHK-CHECK-LABEL then
   id CHK-EXPAND CHK-EXPAND-REPORT
   CHK-WRITE-EXPANDED-SOURCE ;

: CHK-MATERIALIZE-SOURCE ( -- )
   CHK-SEL-SRC-U @ CHK-SRC-ROOM drop
   {: dst:ptr :}
   CHK-SEL-SRC-BUF dst CHK-SEL-SRC-U @ BYTE-COPY
   CHK-SEL-SRC-U @ CHK-SRC-U !
   CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ CHK-LABEL!
   CHK-CHECK-LABEL
   CHK-MATERIALIZE-BYTES ;

\ Each listed path is quoted into its loader line before discovery reads any
\ file.
: CHK-MATERIALIZE-LIST ( -- )
   CHK-POS-N @ 0= if CHK-USAGE then
   CHK-CHECK-INPUTS
   s" <source-list>" CHK-LABEL!
   CHK-EXPAND-RESET
   0 CHK-EXP-OUT-U !
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-APPEND-LISTED
      1+
   repeat drop
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-EXPAND-PATH
      1+
   repeat drop
   CHK-WRITE-EXPANDED-SOURCE ;

public

: SOURCE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   CHK-SEL-MODE @ CHK-SEL-NONE <> if CHK-USAGE then
   labelu PATH-CAP > if E-FS-CAPACITY throw then
   srcu CHK-SEL-SRC-ROOM drop
   {: dst:ptr :}
   src dst srcu BYTE-COPY
   label CHK-SEL-LABEL-BUF labelu BYTE-COPY
   srcu CHK-SEL-SRC-U !
   labelu CHK-SEL-LABEL-U !
   CHK-SEL-SOURCE CHK-SEL-MODE ! ;

private

: CHK-MATERIALIZE-DISPATCH ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-MATERIALIZE-STDIN endof
      CHK-SEL-SOURCE of CHK-MATERIALIZE-SOURCE endof
      CHK-SEL-FILE of CHK-MATERIALIZE-FILE endof
      CHK-SEL-LIST of CHK-MATERIALIZE-LIST endof
      E-TBL-BOUNDS throw
   endcase ;

\ E-FS-PATH-UNSAFE is a path or label the run cannot quote (a named file's
\ canonical spelling or a listed one as CHK-APPEND-LISTED spells it, in its
\ loader line; the label, in the run stage's DIAG-FILE!); E-FFI-NUL is a path
\ holding a NUL, which the file system refuses to look up. Both are usage
\ errors. The materializers judge each
\ spelling after the existence check, so a missing path is still NOINPUT, and
\ before discovery or any later stage reads the source, so the answer does not
\ depend on what it holds.
: CHK-MATERIALIZE ( -- )
   CHK-MAKE-TEMP
   [: CHK-MATERIALIZE-DISPATCH ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-PATH-UNSAFE = rc E-FFI-NUL = or if CHK-PATH-UNSAFE then
   rc throw ;

: CHK-U$ ( n -- ptr u8 n ) {: u:n :}
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

: CHK-DEP-PRELOAD? ( n -- bool ) {: id:n :}
   id CHK-DEP$ RESOLVE nip nip 0= ;

: CHK-RUN-RESET ( -- )
   0 CHK-RUN-U ! ;

: CHK-RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-RUN-U @ u + CHK-RUN-ROOM 2drop
   a CHK-RUN-BUF CHK-RUN-U @ + u BYTE-COPY
   CHK-RUN-U @ u + CHK-RUN-U ! ;

: CHK-RUN-C ( n -- ) {: c:n :}
   CHK-RUN-U @ 1+ CHK-RUN-ROOM 2drop
   c CHK-RUN-BUF CHK-RUN-U @ + c!
   CHK-RUN-U @ 1+ CHK-RUN-U ! ;

: CHK-RUN-SP ( ptr u8 n -- )
   CHK-RUN+
   CHK-SP CHK-RUN-C ;

: CHK-RUN-LN ( ptr u8 n -- )
   CHK-RUN+
   CHK-LF CHK-RUN-C ;

: CHK-RUN-QPATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u >LEN
   CHK-RUN-U @ u SOURCE-QPATH-BYTES + CHK-RUN-ROOM >LEN
   CHK-RUN-U SOURCE-APPEND-QPATH ;

: CHK-ERR-NONNEG ( n -- )
   dup 0 < if drop s" <negative>" CHK-ERR exit then
   CHK-U$ CHK-ERR ;

\ The run file's prefix names the subject for the checker's diagnostics and
\ turns its warnings off. The check before the run wrote the warnings, each in
\ its own file and placed: the run loads what that check checked, so its
\ warnings would repeat them, naming the subject for every file and placing
\ none. It shares the subject's first line when the subject follows it, and the
\ origin markers add no line break, so line N of the run file is line N of the
\ subject: the engine counts the lines of the file it reads when it refuses a
\ statement.
: CHK-BUILD-PREFIX ( -- )
   CHK-LABEL CHK-RUN-QPATH+
   s"  DIAG-FILE!" CHK-RUN-SP
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-SP then
   s" 0 WARN-DIAGS !" CHK-RUN-SP ;

\ The origin pass appends the marked copy of the source file at the given path
\ to the run buffer, reading it whole through check.f's own buffer. The engine comments a leading `#!` line only at the start
\ of a file it reads, so the pass comments the source's own first, with the
\ engine's rewrite: the scan then reads that line as a comment, and the marked
\ copy carries the rewrite the run needs.
: CHK-RUN-ORIGIN+ ( ptr u8 n -- )
   CHK-SRC-READ {: len:n :}
   CHK-SRC-BUF len SOURCE-ROOT:SHEBANG-COMMENT
   CHK-SRC-BUF len [: CHK-RUN-U @ + CHK-RUN-ROOM drop CHK-RUN-U @ + ;]
   DIAG-ORIGIN-SOURCE>BUF CHK-RUN-U @ + CHK-RUN-U ! ;

\ A named file runs as the command line runs it, loaded by its canonical path
\ (CHK-APPEND-ENTRY): its own relative requires resolve against its directory,
\ and a file that requires it back finds it loaded. The engine reads the
\ subject's own bytes, its shebang commented, from CHK-MARK-PATH, which the
\ run's loader answers the subject's path with (src/core/include.f
\ SOURCE-INPUT:USE), so a refusal in the subject names its own line and column.
: CHK-WRITE-MARKED ( -- )
   CHK-SUBJ$ CHK-SRC-READ {: len:n :}
   CHK-SRC-BUF len SOURCE-ROOT:SHEBANG-COMMENT
   CHK-MARK-PATH CHK-SRC-BUF len FS:WRITE-ALL ;

: CHK-BUILD-READER ( -- )
   CHK-LF CHK-RUN-C
   s" : CHECK-F-READ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: p:ptr pu:n r:ptr ru:n :}" CHK-RUN-LN
   s" p pu " CHK-RUN+  CHK-SUBJ$ CHK-RUN-QPATH+  s"  STR= if" CHK-RUN-LN
   CHK-MARK-PATH CHK-RUN-QPATH+  s"  r ru SOURCE-ROOT:READ-OS exit then" CHK-RUN-LN
   s" p pu r ru SOURCE-ROOT:READ-OS ;" CHK-RUN-LN
   s" : CHECK-F-INPUT ( -- ) [: SOURCE-ROOT:CANON-OS ;] [: CHECK-F-READ ;] SOURCE-INPUT:USE ;" CHK-RUN-LN
   s" CHECK-F-INPUT" CHK-RUN-LN ;

: CHK-BUILD-ACT ( -- )
   CHK-SUBJ-U @ 0 > if CHK-WRITE-MARKED then
   CHK-BUILD-PREFIX
   CHK-SUBJ-U @ 0 > if CHK-BUILD-READER then
   CHK-SOURCE CHK-RUN-ORIGIN+ ;

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
\ An engine the resolver refuses is its throw, the check's status (CHK-ENGINE$).
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

\ The run's output is captured to be replayed, CHK-OUT-CAP bytes of standard
\ output and CHK-ERR-CAP of standard error. A run that writes past either is
\ killed by the capture, which throws E-PROC-TRUNCATED without naming the
\ stream, so the refusal names the subject and both bounds.
: CHK-RUN-TOO-BIG ( -- )
   SB-RESET
   s" check.f: " SB-APPEND
   CHK-LABEL SB-APPEND
   s" : the run wrote past its capture of " SB-APPEND
   CHK-OUT-CAP FMT:SB-INT
   s"  bytes of standard output or " SB-APPEND
   CHK-ERR-CAP FMT:SB-INT
   s"  of standard error" SB-APPEND
   SB$ CHK-E-CHECK CHK-FAIL ;

\ A run still going at its deadline is ended by the capture with every process
\ it started (src/lib/process.f PROCESS:KILL-CAPTURE), and refused: what it wrote is
\ not replayed, and one line names the subject and the deadline. A subject that
\ needs longer is checked with a longer --deadline-ms.
: CHK-RUN-LATE ( -- )
   SB-RESET
   s" check.f: " SB-APPEND
   CHK-LABEL SB-APPEND
   s" : the run passed its deadline of " SB-APPEND
   CHK-DEADLINE@ MS>N FMT:SB-INT
   s"  ms" SB-APPEND
   SB$ CHK-E-CHECK CHK-FAIL ;

: CHK-RUN-CAPPED ( -- )
   [: CHK-RUN-CAPTURE ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED = if CHK-RUN-TOO-BIG then
   rc E-PROC-TIMEOUT = if CHK-RUN-LATE then
   rc throw ;

: CHK-REPLAY ( -- )
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

\ A source list materializes as one `script-required` line per listed file, so
\ a lint reads the listed files themselves, in list order, except a source the
\ engine provides, which the run loads nothing from.
: CHK-LINT-LISTED ( [ ptr u8 n -- ] -- ) {: lint :}
   CHK-POS-N @ 0 ?do
      i CHK-POS$ ENGINE-PROVIDES? 0= if i CHK-POS$ lint execute then
   loop ;

: CHK-RUN-BOUNDARY ( -- )
   FD:STDERR CHECKED-BOUNDARY-LINT:OUT-FD!
   CHK-JSON @ CHECKED-BOUNDARY-LINT:JSON!
   LINT-TRUE CHECKED-BOUNDARY-LINT:STRICT!
   CHK-SEL-MODE @ CHK-SEL-LIST = if
      [: CHECKED-BOUNDARY-LINT:FILE ;] CHK-LINT-LISTED
   else
      CHK-LINT-SOURCE CHK-LABEL CHECKED-BOUNDARY-LINT:FILE-AS
   then
   CHECKED-BOUNDARY-LINT:FINISH ;

: CHK-RUN-RESERVED-NAMES ( -- )
   RESERVED-NAME-LINT:RESET
   FD:STDERR RESERVED-NAME-LINT:OUT-FD!
   CHK-JSON @ RESERVED-NAME-LINT:JSON!
   CHK-SEL-MODE @ CHK-SEL-LIST = if
      [: RESERVED-NAME-LINT:FILE ;] CHK-LINT-LISTED
   else
      CHK-LINT-SOURCE CHK-LABEL RESERVED-NAME-LINT:FILE-AS
   then
   RESERVED-NAME-LINT:FINISH ;

\ The lexer's file-local diagnostics still visit discovered files. Definition
\ verification then follows the loader composition in one checker scope and
\ one multi-error session, so a definition a loaded file makes before its
\ loader's next statement is in scope there, and a refused definition's
\ declared signature stands for it, as at a whole-file check.

: CHK-ALL-ID-ACT ( -- )
   CHK-ALL-ID @ {: id:n :}
   id CHK-FILE-NAME$ id CHK-DEP$ CHECK-ALL-ERRORS:LEX-FILE ;

: CHK-ALL-RC-NOTE ( n -- ) {: rc:n :}
   CHK-ALL-RC @ 0= if rc CHK-ALL-RC ! then ;

: CHK-RUN-ALL-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-ALL-ID !
   [: CHK-ALL-ID-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc CHK-E-CHECK = rc CHECK-ALL-ERRORS:DUP-RC = or if rc CHK-ALL-RC-NOTE exit then
   rc throw ;

: CHK-RUN-ALL-ORDER ( -- )
   0 CHK-ALL-RC !
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup CHK-DEP-ORDER@ CHK-RUN-ALL-ID
      1+
   repeat drop
   CHK-ALL-RC @ 0 <> if CHK-ALL-RC @ throw then ;

: CHK-RUN-ALL-COMPOSE ( -- )
   CHK-SOURCE-BYTES CHK-SRC-PATH CHK-LABEL CHECK-ALL-ERRORS:COMPOSE-BUF ;

\ The composition hands up a reader with nothing after it: the missing-name
\ record at that token of the file's lex (CHK-NONAME-AT$), or with no token of
\ the lex there the statement's throw record. The check fails as a refusal.
: CHK-ALL-NONAME ( -- )
   VERIFY:TOKEN-BYTE@ VERIFY:SOURCE-COMPOSE-STOPPED$
   2dup VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? CHK-STOP-BYTES
   {: at:n label:ptr labelu:n src:ptr srcu:n :}
   at label labelu src srcu CHK-JSON @ CHK-NONAME-AT$
   dup 0= if
      2drop VERIFY:E-MISSING-NAME at label labelu src srcu CHECK-ALL-ERRORS:THROW-RECORD$
   then
   CHK-ERR-LN
   CHK-E-CHECK CHK-THROW ;

\ The report goes to standard error as the core makes it: every record of every
\ file, however many and however long, with no buffer to outgrow.
: CHK-RUN-ALL ( -- )
   CHK-ALL-STREAM
   CHK-RUN-ALL-ORDER
   [: CHK-RUN-ALL-COMPOSE ;] catch {: rc:n :}
   rc VERIFY:E-MISSING-NAME = if CHK-ALL-NONAME then
   rc 0 <> if rc CHK-THROW then ;

: CHK-SOURCE-LIST-REPORT ( -- )
   CHK-SEL-MODE @ CHK-SEL-LIST <> if exit then
   s" check.f: source-list entries:" CHK-ERR-LN
   0 begin dup CHK-POS-N @ < while
      s"   " CHK-ERR
      dup CHK-POS$ CHK-ERR
      CHK-LF CHK-ERR-C
      1+
   repeat drop ;

: CHK-PREVERIFY-FAIL ( n -- ) {: rc:n :}
   CHK-JSON @ if rc CHK-THROW then
   s" check.f: source preverify failed before run" CHK-ERR-LN
   s" check.f: label " CHK-ERR  CHK-LABEL CHK-ERR  CHK-LF CHK-ERR-C
   s" check.f: throw code " CHK-ERR  rc CHK-ERR-NONNEG  CHK-LF CHK-ERR-C
   CHK-SOURCE-LIST-REPORT
   rc CHK-THROW ;

: CHK-STOPPED-SOURCE ( -- ptr u8 n )
   VERIFY-STOPPED$ VERIFY-STOP-SUBJECT? CHK-STOP-BYTES ;

\ The pre-pass stopped with the given code. A statement that threw is reported
\ by its record where it stopped (CHK-STOP-WRITE), which fails the check as a
\ refusal when the record is one; any other stop returns.
: CHK-STOP-RECORD ( n -- ) {: rc:n :}
   rc CHECK-ALL-ERRORS:THREW? 0= if exit then
   rc VERIFY-STOP-AT VERIFY-STOPPED$ CHK-STOPPED-SOURCE CHK-STOP-WRITE ;

\ The checker reports nothing for a duplicate definition, so it is reported by
\ the record --all-errors writes in the run's mode, naming the file that defined
\ the name again and the name and line read from that file where the pre-pass
\ refused it (PREVERIFY-DUPLICATE), and fails the run with the duplicate's
\ status, as it fails the load.
: CHK-PREVERIFY-DUP ( -- )
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   PREVERIFY-DUPLICATE VERIFY-STOPPED$ CHK-STOPPED-SOURCE
   CHECK-ALL-ERRORS:DUP-RECORD$ CHK-ERR-LN ;

\ The pre-pass stopped with the given code: the record of its defect, or of a
\ duplicate, then the failure. A statement that threw and a refusal the checker
\ reported itself both fail with a refusal's status, so --json-errors writes
\ the refusal's packet and nothing else.
: CHK-PREVERIFY-STOPPED ( n -- ) {: rc:n :}
   rc CHK-STOP-RECORD
   rc CHECK-ALL-ERRORS:THREW?  rc REPORTED-ERROR? or if
      CHK-E-CHECK CHK-PREVERIFY-FAIL
   then
   rc CHECK-ALL-ERRORS:DUP-RC = if CHK-PREVERIFY-DUP then
   rc CHK-PREVERIFY-FAIL ;

\ How a verifier child that gave no answer ended, as the closing line of its
\ report, where CHK-EXPLAIN-LN puts prose.
: CHK-STATUS-LN ( PROCESS:ending -- )
   SB-RESET
   s" check.f: the verifier did not complete: " SB-APPEND
   MATCH PROCESS:ending
      exited OF s" exit " SB-APPEND CHK-U$ SB-APPEND ENDOF
      signaled OF s" signal " SB-APPEND CHK-U$ SB-APPEND ENDOF
      timeout OF s" deadline of " SB-APPEND CHK-DEADLINE@ MS>N CHK-U$ SB-APPEND s"  ms passed" SB-APPEND ENDOF
   ;MATCH
   SB$ CHK-EXPLAIN-LN ;

: CHK-TRUNCATED-LN ( -- )
   s" check.f: the verifier did not complete: its output exceeded the capture" CHK-EXPLAIN-LN ;

\ The pre-pass's packets on stderr, then its child's prose.
: CHK-PREVERIFY-RELAY ( -- )
   VERIFY-OUT$ CHK-ERR
   VERIFY-LOG$ CHK-EXPLAIN ;

\ The pre-pass runs in the verifier child (PREVERIFY-BYTES), resolving only the
\ run's words; its packets and the child's prose precede what its stop leaves.
: CHK-PREVERIFY-REPORT ( result<n,PROCESS:ending> -- )
   CHK-PREVERIFY-RELAY
   MATCH result
      ok OF {: rc:n :} rc 0<> if rc CHK-PREVERIFY-STOPPED then ENDOF
      err OF CHK-STATUS-LN CHK-E-UNAVAILABLE CHK-THROW ENDOF
   ;MATCH ;

: CHK-PREVERIFY-ACT ( -- )
   CHK-SOURCE-BYTES CHK-SRC-PATH CHK-LABEL CHK-DEADLINE@ PREVERIFY-BYTES
   CHK-PREVERIFY-REPORT ;

\ More output than the child's capture holds leaves no answer: the packets and
\ prose received before it, and a closing line.
: CHK-RUN-PREVERIFY ( -- )
   [: CHK-PREVERIFY-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED <> if rc throw then
   CHK-PREVERIFY-RELAY
   CHK-TRUNCATED-LN
   CHK-E-UNAVAILABLE CHK-THROW ;

: CHK-MAP+ ( ptr u8 n -- )
   >LEN CHK-MAP-BUF CHK-ERR-CAP >LEN CHK-MAP-U SOURCE-APPEND-BYTES ;

\ Where the run file's name next starts in the given bytes, or -1.
: CHK-RUN-NAMED-AT ( ptr u8 n -- n )
   CHK-RUN-PATH FIND-SUB MATCH option
     some OF IDX>N ENDOF
     none OF -1 ENDOF
   ;MATCH ;

\ The bytes before the run file's name at at, then the label in its place;
\ the bytes after the name remain.
: CHK-MAP-NAME ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n at:n :}
   a at CHK-MAP+
   CHK-LABEL CHK-MAP+
   at CHK-RUN-PATH-U @ + {: past:n :}
   a past +  u past - ;

\ An inlined subject is the run file line for line (CHK-BUILD-PREFIX), so where
\ the run's diagnostics name the run file, as the engine's refusal of a
\ statement does, they name the subject at that line.
: CHK-ERR-NAME-SUBJECT ( -- )
   0 CHK-MAP-U !
   CHK-ERR-BUF CHK-ERR-U @
   begin 2dup CHK-RUN-NAMED-AT dup 0 >= while
      CHK-MAP-NAME
   repeat drop
   CHK-MAP+
   CHK-MAP-BUF CHK-ERR-BUF CHK-MAP-U @ LEN>N BYTE-COPY
   CHK-MAP-U @ LEN>N CHK-ERR-U ! ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ FS:WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPPED
   CHK-ERR-NAME-SUBJECT ;

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

: CHK-RUN-STATIC-LINTS ( -- )
   \ Zero-byte input traverses every phase; a bypass is publicly unobservable.
   CHK-RUN-RESERVED-NAMES
   CHK-RUN-BOUNDARY
   CHK-ALL @ if CHK-RUN-ALL then ;

: CHK-RUN-CURRENT ( -- )
   CHK-RUN-STATIC-LINTS
   CHK-ALL @ 0= if CHK-RUN-PREVERIFY then
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

\ Outermost boundary of one check run. Everything inside it is work on the
\ subject source, so the whole run starts at neutral top level and the caller's
\ package is restored when the run ends, however it ends.
: CHK-RUN-SCOPED ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [: CHK-RUN-CURRENT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

\ --verify-only is VERIFY-BYTES over a file's or stdin's bytes, nothing more:
\ stderr gets packets; stdout gets prose and, with no verdict, a closing line.

: CHK-VERIFY-READ ( -- )
   0 CHK-POS$ CHK-SRC-READ CHK-SRC-U ! ;

\ The named file is read as the closure walk reads a file of the closure: one
\ the file system refuses to read fails the check as it fails the walk.
: CHK-VERIFY-FILE ( -- ptr u8 n )
   0 CHK-POS$ {: a:ptr u:n :}
   a u FS:FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   [: CHK-VERIFY-READ ;] CHK-READ-RC 0<> if a u CANONICAL drop CHK-READ-FAIL then
   a u ;

\ stdin's path need not exist.
: CHK-VERIFY-STDIN ( -- ptr u8 n )
   CHK-STDIN-PATH-U @ 0= if CHK-USAGE then
   CHK-READ-STDIN CHK-SRC-U !
   CHK-STDIN-PATH$ ;

\ Read the bytes into the source buffer: the path they stand for.
: CHK-VERIFY-SELECT ( -- ptr u8 n )
   CHK-SEL-MODE @ CHK-SEL-FILE = if
      CHK-STDIN-PATH-U @ 0<> if CHK-USAGE then
      CHK-VERIFY-FILE exit
   then
   CHK-SEL-MODE @ CHK-SEL-NONE <> if CHK-USAGE then
   CHK-VERIFY-STDIN ;

\ The verifier's packets on stderr, a stop's record last among them, its prose
\ on stdout.
: CHK-VERIFY-RELAY ( -- )
   VERIFY-OUT$ CHK-ERR
   VERIFY-LOG$ CHK-OUT ;

: CHK-VERIFY-REPORT ( verdict -- )
   CHK-VERIFY-RELAY
   MATCH verdict
      verified OF ENDOF
      refused OF CHK-E-CHECK CHK-THROW ENDOF
      engine-provided OF
         s" check.f: the engine provides the source; it is not verified" CHK-OUT-LN
         CHK-E-USAGE CHK-THROW
      ENDOF
      held OF
         s" check.f: the verifier's own image holds the source; it cannot be verified there" CHK-OUT-LN
         CHK-E-UNAVAILABLE CHK-THROW
      ENDOF
      incomplete OF CHK-STATUS-LN CHK-E-UNAVAILABLE CHK-THROW ENDOF
      deferred OF
         s" check.f: a stretch or definition deferred to the run is not verified" CHK-OUT-LN
      ENDOF
   ;MATCH ;

: CHK-VERIFY-ACT ( -- )
   CHK-VERIFY-SELECT {: path:ptr pathu:n :}
   CHK-SRC-BUF CHK-SRC-U @ path pathu [: drop ;] CHK-DEADLINE@ VERIFY-BYTES
   CHK-VERIFY-REPORT ;

\ More output than the verifier's capture holds leaves no verdict: the packets
\ received before it, the prose and a closing line.
: CHK-RUN-VERIFY ( -- )
   [: CHK-VERIFY-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TRUNCATED <> if rc throw then
   CHK-VERIFY-RELAY
   CHK-TRUNCATED-LN
   CHK-E-UNAVAILABLE CHK-THROW ;

: CHK-RUN-INNER ( -- )
   CHK-VERIFY @ if CHK-RUN-VERIFY exit then
   CHK-STDIN-PATH-U @ 0<> if CHK-USAGE then
   CHECKED-BOUNDARY-LINT:RESET
   CHK-MATERIALIZE
   CHK-RUN-SCOPED ;

: CHK-IF-CLEAN ( n [ -- ] -- n ) {: rc:n q :}
   rc 0 <> if rc exit then
   q catch ;

: CHK-FINAL-CLEAN ( n -- n n ) {: rc:n :}
   [: CHK-TEMP-CLEAN ;] catch {: temp-rc:n :}
   [: CHECKED-BOUNDARY-LINT:RESET ;] catch {: provider-rc:n :}
   temp-rc provider-rc CHK-FIRST-RC {: clean-rc:n :}
   rc clean-rc CHK-FIRST-RC
   clean-rc ;

: CHK-RUN-ACT ( -- n n )
   [: CHK-TEMP-CLEAN ;] catch
   [: CHK-RUN-INNER ;] CHK-IF-CLEAN
   CHK-FINAL-CLEAN ;

: CHK-RUN-FINISH ( n n -- n ) {: rc:n clean-rc:n :}
   clean-rc 0 <> if CHK-SESSION-CLEAR else CHK-RUN-TEMP-CLEAR then
   rc ;

public

: RUN ( -- n )
   CHK-RUN-TEMP-CLEAR
   CHK-RUN-ACT CHK-RUN-FINISH ;

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
: CHK-SAY-THROW ( ptr u8 n n -- ) {: what:ptr whatu:n code:n :}
   code 0= if exit then
   s" check: " CHK-ERR what whatu CHK-ERR s"  threw " CHK-ERR
   SB-RESET code FMT:SB-INT SB$ CHK-ERR-LN ;

: CHK-SIGNAL-ANSWER ( n -- ) {: sig:n :}
   s" child kill" [: PROCESS:KILL-CAPTURE ;] catch CHK-SAY-THROW
   s" cleanup" [: CHK-TEMP-CLEAN ;] catch CHK-SAY-THROW
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
   CHK-PARSE-CLI
   RUN dup 0 <> if throw then drop ;

\ hb exits by the code of a throw that reaches the top when it is 1 to 255,
\ writing nothing, and reports any other as `hb: uncaught throw code N` on
\ stderr, exiting UNCAUGHT-RC. Under --json-errors stderr holds packets only,
\ so the command line says such a throw on stdout, with the same status: an
\ engine selection the resolver refused (src/lib/engine-candidate.f: a path that
\ names no executable, E-FS-OPEN, or one the file system refuses, E-FS-PATH),
\ which CHK-ENGINE$ records where a child starts, as that, and any other, such
\ as a scratch directory TMPDIR will not hold, by its code. The option is read
\ from the command line: a run whose cleanup failed clears the session's.
: CHK-MAIN-UNCAUGHT ( n -- n )
   {: rc:n :}
   rc 0 >= rc 256 < and if rc exit then
   s" --json-errors" CHK-OPT? 0= if rc exit then
   CHK-ENGINE-REFUSED @ if
      s" check.f: the selected engine (HABU_UNDER_TEST, else the running engine)" CHK-OUT
      s"  is not a usable executable" CHK-OUT-LN
   else
      s" check.f: uncaught throw code " CHK-OUT
      SB-RESET rc FMT:SB-INT SB$ CHK-OUT-LN
   then
   UNCAUGHT-RC ;

public

: MAIN ( -- )
   CHK-CATCH-STOPS
   [: CHK-MAIN-RUN ;] catch CHK-MAIN-UNCAUGHT {: rc:n :}
   CHK-SIGNAL-CHECK
   rc 0 <> if rc throw then ;

;using
;using
;package
