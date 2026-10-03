\ check-core.f - reusable Habu-native checked engine core.

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f                \ the run stage spawns with an environment
require lib/engine-candidate.f           \ and on the engine this names
require lib/signal.f                     \ check.f answers SIGTERM, SIGINT and SIGHUP
require lib/fmt.f                        \ the answer names a step that throws
require lib/source.f
require lib/argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/json.f
require tools/json-only-core.f
require tools/diag-origin-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/check-all-errors-core.f    \ loads src/habu/verify-source.f
\ The require closure, and CHECK:VERIFY-BYTES: the check that stops before the
\ run stage, which --verify-only makes.
require tools/check-verify-core.f
require src/core/checker-owner-guard.f

\ Checker axioms. Retirement: habu-campaign-c2-mem-c3d7662b.
\ CHECK! certifies snippets so the fail-closed source hook compiles checked.
\ TYPE-RESERVED? is the DEFLINEAR and VALUE-RECORD name rule.
\ CHECKER-DEFLINEAR publishes parsed linearity metadata in the child scope.
\ CHECKER-TRY-RECORD publishes a parsed record, or answers the field it refuses.
\ CHECKER-SCOPE-START/DONE isolate, then roll back, generated dependency effects.
s" CHECK!" s" ptr u8 n -- n" TRUST
s" TYPE-RESERVED?" s" ptr u8 n -- bool" TRUST
s" CHECKER-DEFLINEAR" s" ptr u8 n --" TRUST
s" CHECKER-TRY-RECORD" s" ptr u8 n ptr u8 n -- n ptr u8 n" TRUST
s" CHECKER-SCOPE-START" s" --" TRUST
s" CHECKER-SCOPE-START-NEUTRAL" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK
using SOURCE                             \ the shared source-string emitters
using SOURCE-ROOT

: CHK-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
LOWER-CERT-HOOK:INSTALL
' CHK-CHECK-HOOK set-check

\ A source is at most what the engine loads from one file; tools/diag-origin.f
\ reads to the same bound.
INCLUDE-BUF-CAP constant CHK-SRC-CAP
\ The run file is the subject behind a prefix, with an origin mark on each
\ definition, and the run loads it with `--load`: it holds what the engine
\ loads from one file.
INCLUDE-BUF-CAP constant CHK-RUN-CAP
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
67 constant CHK-E-CAPACITY
\ A throw nothing handles ends a load: a code outside 1..255 is named on the
\ last line of standard error, `hb: uncaught throw code N`, and exits
\ UNCAUGHT-RC (docs/debugging.md). A checker record refused at the load throws
\ its own code (docs/repair-diagnostics.md).
UNCAUGHT-RC constant CHK-UNCAUGHT-RC
E-TRUST-UNRESOLVED constant CHK-E-TRUST-ROW
E-PKG-CONTEXT constant CHK-E-PKG-RECORD
E-BAD-QUALIFIED constant CHK-E-QUALIFIED-RECORD

0 constant CHK-SEL-NONE
1 constant CHK-SEL-SOURCE
2 constant CHK-SEL-FILE
3 constant CHK-SEL-LIST

10 constant CHK-LF
13 constant CHK-CR
32 constant CHK-SP
45 constant CHK-DASH

create CHK-NUM-BUF CHK-NUM-CAP allot
create CHK-ROOT-BUF FS-PATH-CAP allot
create CHK-SRC-PATH-BUF FS-PATH-CAP allot
create CHK-RUN-PATH-BUF FS-PATH-CAP allot
create CHK-MARK-PATH-BUF FS-PATH-CAP allot
create CHK-SUBJ-BUF FS-PATH-CAP allot
create CHK-SEL-LABEL-BUF FS-PATH-CAP allot
create CHK-STDIN-PATH-BUF FS-PATH-CAP allot
create CHK-POS-BUF CHK-MAX-POS FS-PATH-CAP * allot
create CHK-POS-U CHK-MAX-POS cells allot
create CHK-ONE 1 allot

\ The lazily allocated byte buffers hold addresses, so each is a declared
\ pointer cell: a raw `variable` would launder the address through a cell of
\ unknown type.
TYPED-VARIABLE CHK-SRC-BUF-A ptr u8
TYPED-VARIABLE CHK-RUN-BUF-A ptr u8
TYPED-VARIABLE CHK-OUT-BUF-A ptr u8
TYPED-VARIABLE CHK-ERR-BUF-A ptr u8
TYPED-VARIABLE CHK-MAP-BUF-A ptr u8
TYPED-VARIABLE CHK-EXP-BUF-A ptr u8
TYPED-VARIABLE CHK-SEL-SRC-BUF-A ptr u8

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
variable CHK-NOM-I
variable CHK-NOM-U
variable CHK-EXP-U
variable CHK-EXP-OUT-U
variable CHK-ALL-ID
variable CHK-ALL-RC
variable CHK-NOM-BAD                     \ the nominal pass reported a finding
variable CHK-TFAM-NAME-I

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

: CHK-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: CHK-SRC-BUF ( -- ptr u8 )
   CHK-SRC-BUF-A @ 0= if CHK-SRC-CAP CHK-ALLOC-BUF CHK-SRC-BUF-A ! then
   CHK-SRC-BUF-A @ ;

: CHK-RUN-BUF ( -- ptr u8 )
   CHK-RUN-BUF-A @ 0= if CHK-RUN-CAP CHK-ALLOC-BUF CHK-RUN-BUF-A ! then
   CHK-RUN-BUF-A @ ;

: CHK-OUT-BUF ( -- ptr u8 )
   CHK-OUT-BUF-A @ 0= if CHK-OUT-CAP CHK-ALLOC-BUF CHK-OUT-BUF-A ! then
   CHK-OUT-BUF-A @ ;

: CHK-ERR-BUF ( -- ptr u8 )
   CHK-ERR-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-ERR-BUF-A ! then
   CHK-ERR-BUF-A @ ;

: CHK-MAP-BUF ( -- ptr u8 )
   CHK-MAP-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-MAP-BUF-A ! then
   CHK-MAP-BUF-A @ ;

: CHK-EXP-BUF ( -- ptr u8 )
   CHK-EXP-BUF-A @ 0= if CHK-SRC-CAP CHK-ALLOC-BUF CHK-EXP-BUF-A ! then
   CHK-EXP-BUF-A @ ;

: CHK-SEL-SRC-BUF ( -- ptr u8 )
   CHK-SEL-SRC-BUF-A @ 0= if
      CHK-SRC-CAP CHK-ALLOC-BUF CHK-SEL-SRC-BUF-A !
   then
   CHK-SEL-SRC-BUF-A @ ;

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

: CHK-EXPLAIN-LN ( ptr u8 n -- )
   CHK-VERIFY @ if CHK-OUT-LN else CHK-ERR-LN then ;

: CHK-USAGE ( -- )
   s" usage: tools/check.f [--json-errors] [--all-errors] [--deadline-ms ms] [--verify-only [--stdin-path path]] [--source-list file ... | prog.f]" CHK-EXPLAIN-LN
   CHK-E-USAGE throw ;

: CHK-THROW ( n -- )
   throw ;

: CHK-FAIL ( ptr u8 n n -- ) {: msg:ptr u:n code:n :}
   msg u CHK-EXPLAIN-LN
   code CHK-THROW ;

: CHK-PATH-TOO-BIG ( -- )
   s" check.f: source path exceeds capacity" CHK-E-CAPACITY CHK-FAIL ;

: CHK-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: CHK-ARG= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx CHK-ARG$ a u LINT-STR= ;

\ --stdin-path and --deadline-ms each consume the next token as their value.
: CHK-VALUED-ARG? ( n -- bool ) {: idx:n :}
   idx s" --stdin-path" CHK-ARG= idx s" --deadline-ms" CHK-ARG= or ;

\ Establish the output stream before parsing can reject an earlier argument.
\ A valued option's value is skipped; -- ends option parsing.
: CHK-VERIFY-ARG? ( -- bool )
   0 begin dup SCRIPT-ARGC < while
      dup s" --" CHK-ARG= if drop false exit then
      dup CHK-VALUED-ARG? if
         2 +
      else
         dup s" --verify-only" CHK-ARG= if drop true exit then
         1+
      then
   repeat drop false ;

: CHK-DASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ CHK-DASH = else 0 0= 0= then ;

: CHK-POS-SLOT ( n -- ptr u8 )
   FS-PATH-CAP * CHK-POS-BUF + ;

: CHK-POS-U-SLOT ( n -- ptr n )
   cells CHK-POS-U + ;

: CHK-POS$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if CHK-USAGE then
   idx CHK-POS-N @ >= if CHK-USAGE then
   idx CHK-POS-SLOT
   idx CHK-POS-U-SLOT @ ;

: CHK-ADD-POS ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
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
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
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

\ The argument at CHK-ARG-I, and a valued option's value with it.
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
\ direct CHECK:FILE callers, and explain it only at this command-line boundary.
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
   0 CHK-NOM-I !
   0 CHK-NOM-U !
   LINT-FALSE CHK-NOM-BAD !
   0 CHK-TFAM-NAME-I !
   0 CHK-EXP-U !
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
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
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
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a CHK-SUBJ-BUF u BYTE-COPY
   u CHK-SUBJ-U ! ;

: CHK-TEMP-CLEAN ( -- )
   CHK-ROOT-U @ 0= if exit then
   CHK-ROOT 2dup EXISTS? if REMOVE-TREE else 2drop then
   CHK-ROOT CLEANUP-FORGET
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
   s" habu-check" HB-TMP-MKDIR SOURCE-ROOT:CANON-OS 0= if E-FS-IO throw then
   CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT CLEANUP-TREE+
   CHK-ROOT s" source.f" CHK-SRC-PATH-BUF JOIN-PATH CHK-SRC-PATH-U !
   CHK-ROOT s" run.f" CHK-RUN-PATH-BUF JOIN-PATH CHK-RUN-PATH-U !
   CHK-ROOT s" subject.f" CHK-MARK-PATH-BUF JOIN-PATH CHK-MARK-PATH-U ! ;

: CHK-LABEL-STDIN ( -- )
   s" <stdin>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8! ;

: CHK-LABEL-FILE ( -- )
   CHK-SRC-A CHK-PTR-U8@ CHK-LABEL-A CHK-PTR-U8!
   CHK-SRC-U @ CHK-LABEL-U ! ;

: CHK-LABEL ( -- ptr u8 n )
   CHK-LABEL-A CHK-PTR-U8@ CHK-LABEL-U @ ;

: CHK-SOURCE ( -- ptr u8 n )
   CHK-SRC-A CHK-PTR-U8@ CHK-SRC-U @ ;

: CHK-SOURCE-BYTES ( -- ptr u8 n )
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-CAP READ-ALL
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

\ Discovery stops at a string the file never closes without saying where. Under
\ --json-errors the lexer reports the defect where it stands, in the file that
\ ended the walk (CHK-DISC-ID), by the record --all-errors writes for it, and
\ the check fails as a refusal. An end the lexer does not see, such as a `{:`
\ group left open, keeps discovery's line.
: CHK-DISC-LEX-ACT ( -- )
   CHK-DISC-ID @ CHK-DEP$ 2dup CHECK-ALL-ERRORS:LEX-FILE ;

: CHK-DISC-LEX ( -- )
   2 >FD CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS:STREAM!
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   [: CHK-DISC-LEX-ACT ;] catch {: rc:n :}
   rc 0 <> if rc CHK-THROW then ;

\ The command line reports a closure it cannot follow as it always has.
: CHK-EXPAND-REPORT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc E-DISC-UNTERM = if CHK-JSON @ if CHK-DISC-LEX then then
   rc CHK-DISC-RC? if rc CHK-DISC-FAIL then
   rc CHK-E-NOINPUT = if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   rc throw ;

: CHK-ENTRY-ID ( ptr u8 n -- n )
   ENTRY-RESOLVE drop RESOLVED-ROOT$ CHK-DEP-ID ;

: CHK-EXPAND-PATH ( ptr u8 n -- )
   CHK-ENTRY-ID CHK-EXPAND CHK-EXPAND-REPORT ;

: CHK-WRITE-EXPANDED-SOURCE ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-EXP-OUT-U @ LEN>N WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-EXP-APP ( ptr u8 n -- )
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-BYTES ;

: CHK-EXP-C ( n -- )
   CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-C ;

: CHK-APPEND-REQUIRED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   \ Boot-owned dependencies are recorded portably relative to the current
   \ tree.  Materializing their canonical absolute spelling makes the child
   \ miss its boot row and reload a family such as option; paths outside the
   \ tree remain absolute and retain their distinct application identity.
   path pathu SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-QPATH
   CHK-SP CHK-EXP-C
   s" required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

\ A named file is loaded the way the command line loads it: the engine turns
\ `--load PATH` into `s" PATH" script-required`, which resolves PATH as an
\ entry, so the file's own relative requires resolve against its directory. The
\ path is the canonical one the file's closure entry holds, so the child's
\ working directory does not matter. DIAG-ORIGIN! markers exist only in inlined
\ text: the child's own JSON diagnostics for a file it loads by path carry the
\ engine's coordinates, which count from each definition's name.
: CHK-APPEND-ENTRY ( ptr u8 n -- )
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-QPATH
   CHK-SP CHK-EXP-C
   s" script-required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-SRC-BUF CHK-SRC-CAP >LEN READ-STDIN-ALL LEN>N CHK-SRC-U !
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-SOURCE-TOO-BIG ( -- )
   s" check.f: source exceeds capacity" CHK-E-NOINPUT CHK-FAIL ;

\ A capacity fault while the tool builds the file the run loads is the clean
\ NOINPUT diagnostic, not an uncaught E-FS-CAPACITY.
: CHK-CAPPED ( [ -- ] -- ) {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-CAPACITY = if CHK-SOURCE-TOO-BIG then
   rc throw ;

\ E-FS-PATH-UNSAFE: the quoting judge, SOURCE-QPATH-CHECK, refuses a double
\ quote, backslash, CR, LF or NUL, and the file system refuses a NUL in a path.
: CHK-PATH-UNSAFE ( -- )
   s" check.f: source path or label contains a double quote, backslash, CR, LF or NUL" CHK-E-USAGE CHK-FAIL ;

\ Every input must be a file before the engine's resolver sees it. The
\ resolver refuses a path holding a NUL with a raw range code, and FILE?
\ refuses it with E-FS-PATH-UNSAFE instead.
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
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-ENGINE-PROVIDED" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" rebuild_engine" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" uncheckable" LJW-STRING LJW-COMMA
   s" file" LJW-KEY path pathu LJW-STRING LJW-COMMA
   s" line" LJW-KEY 1 LJW-U LJW-COMMA
   s" column" LJW-KEY 1 LJW-U LJW-COMMA
   s" suggestion" LJW-KEY CHK-ENGINE-SUG$ LJW-STRING
   LJW-OBJECT-END
   LJW$ CHK-ERR-LN ;

: CHK-ENGINE-PROSE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" E-ENGINE-PROVIDED " CHK-ERR
   path pathu CHK-ERR
   s" :1:1: " CHK-ERR
   CHK-ENGINE-SUG$ CHK-ERR-LN ;

: CHK-CHECK-INPUTS ( -- )
   [: FILE? ;] CHK-INPUTS-ALL? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
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

\ The bound is checked on the file itself: discovery sizes its own scratch to
\ the source, so a source over CHK-SRC-CAP no longer refuses there, and the
\ later read into the source buffer sits outside CHK-MATERIALIZE's catch.
\
\ A JSON packet names the file by the canonical absolute path its closure entry
\ holds, the spelling every dependency's packets carry, so one run names each
\ file one way and a client can map it to a URI. Prose keeps the path as given.
\ Both spellings the run quotes, the entry in the loader line and the label,
\ are judged before discovery reads the file.
: CHK-MATERIALIZE-FILE ( -- )
   0 CHK-POS$ CHK-LABEL!
   CHK-CHECK-INPUTS
   CHK-LABEL FILE-SIZE CHK-SRC-CAP > if CHK-SOURCE-TOO-BIG then
   CHK-EXPAND-RESET
   0 CHK-EXP-OUT-U !
   CHK-LABEL CHK-ENTRY-ID {: id:n :}
   id CHK-DEP$ CHK-APPEND-ENTRY
   id CHK-DEP$ CHK-SUBJ!
   CHK-JSON @ if id CHK-DEP$ CHK-LABEL! else CHK-CHECK-LABEL then
   id CHK-EXPAND CHK-EXPAND-REPORT
   CHK-WRITE-EXPANDED-SOURCE ;

: CHK-MATERIALIZE-SOURCE ( -- )
   CHK-SEL-SRC-BUF CHK-SRC-BUF CHK-SEL-SRC-U @ BYTE-COPY
   CHK-SEL-SRC-U @ CHK-SRC-U !
   CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ CHK-LABEL!
   CHK-CHECK-LABEL
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SOURCE! ;

\ Each listed path is quoted into its loader line before discovery reads any
\ file.
: CHK-MATERIALIZE-LIST ( -- )
   CHK-POS-N @ 0= if CHK-USAGE then
   CHK-CHECK-INPUTS
   s" <source-list>" CHK-LABEL!
   CHK-EXPAND-RESET
   0 CHK-EXP-OUT-U !
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-APPEND-REQUIRED
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
   srcu CHK-SRC-CAP > if CHK-SOURCE-TOO-BIG then
   labelu FS-PATH-CAP > if E-FS-CAPACITY throw then
   src CHK-SEL-SRC-BUF srcu BYTE-COPY
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

\ A source (or its facade expansion) over CHK-SRC-CAP fails closed with the
\ clean NOINPUT diagnostic instead of an uncaught E-FS-CAPACITY from the read
\ layer (dot habu-tfam-13-c2-checkcore-cap). E-FS-PATH-UNSAFE is a path or
\ label the run cannot quote (a named file's canonical spelling or a listed
\ one as CHK-APPEND-REQUIRED spells it, in its loader line; the label, in the
\ run stage's DIAG-FILE!) or a path holding a NUL, which the file system
\ refuses to look up. The materializers judge each spelling after the existence
\ check, so a missing path is still NOINPUT, and before discovery or any later
\ stage reads the source, so the answer does not depend on what it holds.
: CHK-MATERIALIZE ( -- )
   CHK-MAKE-TEMP
   [: CHK-MATERIALIZE-DISPATCH ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-CAPACITY = if CHK-SOURCE-TOO-BIG then
   rc E-FS-PATH-UNSAFE = if CHK-PATH-UNSAFE then
   rc throw ;

: CHK-WORD-TOK? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: CHK-TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CHK-WORD-TOK? 0= IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ A raw operand is data: `[char] ;` ends no definition.
: CHK-TOK-SEMI? ( n -- bool ) {: k:n :}
   k LINT-LEX:OPERAND? if LINT-FALSE exit then
   k s" ;" CHK-TOK=CI ;

\ A definer reads its name with parse-name, the next whitespace-delimited token
\ whatever it spells: `DEFLINEAR (` names the type `(`, which TYPE-RESERVED?
\ refuses, and `: \` defines the word `\`, as the loader reads them. On a match
\ the lexer reads the token after the definer again by that rule, so no scan
\ takes a comment, or the token after one, for the name.
: CHK-DEFINER-TOK? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u CHK-TOK=CI 0= IF LINT-FALSE exit THEN
   k LINT-LEX:OPERAND
   LINT-TRUE ;

\ The lexer marks a parsing keyword's operand, and leaves a local of the
\ keyword's name unmarked with nothing taken, so the `;` that ends a definition
\ is the first unmarked one.
: CHK-WALK-DEF ( n -- n )                \ from past the opener to past its `;`
   begin dup LINT-LEX:COUNT < while
      dup CHK-TOK-SEMI? if 1+ exit then
      1+
   repeat ;

: CHK-TOK-END ( n -- n ) {: k:n :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

: CHK-NOM-SRC$ ( n n -- ptr u8 n ) {: def:n name:n :}
   CHK-SRC-BUF def LINT-LEX:BYTE@ +
   name CHK-TOK-END def LINT-LEX:BYTE@ - ;

: CHK-NOM-BAD-SUG$ ( -- ptr u8 n )
   s" Choose a unique non-reserved nominal type name." ;

: CHK-NOM-JSTR ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n :}
   key keyu LJW-KEY val valu LJW-STRING LJW-COMMA ;

: CHK-NOM-JU ( n ptr u8 n -- )
   LJW-KEY LJW-U LJW-COMMA ;

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

\ A declaration packet up to its suggestion: the declaration from def to token
\ tok, which the packet names and locates.
: CHK-PACKET-START ( n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: def:n tok:n code:ptr codeu:n class:ptr classu:n word:ptr wordu:n :}
   LJW-RESET
   LJW-OBJECT-START
   1 s" schema_version" CHK-NOM-JU
   s" code" code codeu CHK-NOM-JSTR
   s" repair_class" class classu CHK-NOM-JSTR
   s" verdict" s" rejected" CHK-NOM-JSTR
   s" word" word wordu CHK-NOM-JSTR
   s" token" LJW-KEY tok LINT-LEX:TOKEN LJW-STRING LJW-COMMA
   tok s" token_index" CHK-NOM-JU
   s" file" LJW-KEY CHK-LABEL LJW-STRING LJW-COMMA
   tok LINT-LEX:LINE@ s" line" CHK-NOM-JU
   tok LINT-LEX:COL@ s" column" CHK-NOM-JU
   tok LINT-LEX:BYTE@ s" byte_start" CHK-NOM-JU
   tok CHK-TOK-END s" byte_end" CHK-NOM-JU
   s" definition_source" LJW-KEY def tok CHK-NOM-SRC$ LJW-STRING LJW-COMMA
   s" declared_effect" s" unknown " CHK-NOM-JSTR
   s" declared_effect_source" s" unknown" CHK-NOM-JSTR
   s" inferred_effect" s" unknown " CHK-NOM-JSTR
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" LJW-KEY s" " LJW-STRING LJW-COMMA
   s" actual" LJW-KEY s" " LJW-STRING
   LJW-OBJECT-END
   LJW-COMMA ;

: CHK-PACKET-END ( ptr u8 n -- ) {: sug:ptr sugu:n :}
   s" suggestion" LJW-KEY sug sugu LJW-STRING
   LJW-OBJECT-END
   LJW$ CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-TYPE-JSON ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   def name s" E-BAD-NOMINAL-TYPE" s" fix_nominal_type" word wordu CHK-PACKET-START
   CHK-NOM-BAD-SUG$ CHK-PACKET-END ;

: CHK-NOM-PROSE ( n -- ) {: name:n :}
   s" check.f: bad nominal type '" CHK-ERR
   name LINT-LEX:TOKEN CHK-ERR
   39 CHK-ERR-C
   CHK-LF CHK-ERR-C ;

\ A nominal finding is reported and the scan goes on to the next declaration;
\ CHK-RUN-NOMINAL fails the run once every file is read, so one run reports
\ every finding the pass can make.
: CHK-NOM-FOUND ( -- )
   LINT-TRUE CHK-NOM-BAD ! ;

: CHK-TYPE-FAIL ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   CHK-JSON @ IF def name word wordu CHK-TYPE-JSON ELSE name CHK-NOM-PROSE THEN
   CHK-NOM-FOUND ;

: CHK-NOM-FAIL ( n n -- )
   s" deftype" CHK-TYPE-FAIL ;

: CHK-LIN-FAIL ( n n -- )
   s" deflinear" CHK-TYPE-FAIL ;

\ Prose located at token k: label, line and column.
: CHK-AT-PROSE ( n -- ) {: k:n :}
   s" check.f: " CHK-ERR
   CHK-LABEL CHK-ERR
   58 CHK-ERR-C
   k LINT-LEX:LINE@ CHK-U$ CHK-ERR
   58 CHK-ERR-C
   k LINT-LEX:COL@ CHK-U$ CHK-ERR
   s" : " CHK-ERR ;

: CHK-NONAME-SUG$ ( -- ptr u8 n )
   s" Give the definer a name: the next whitespace-delimited token." ;

\ Definer k has nothing after it, so it has no name to read: the loader refuses
\ it, and the check does at the definer.
: CHK-NONAME-FAIL ( n -- ) {: k:n :}
   CHK-JSON @ IF
      k k s" E-MISSING-NAME" s" fix_missing_name" k LINT-LEX:TOKEN CHK-PACKET-START
      CHK-NONAME-SUG$ CHK-PACKET-END
   ELSE
      k CHK-AT-PROSE
      s" missing name after '" CHK-ERR
      k LINT-LEX:TOKEN CHK-ERR
      39 CHK-ERR-C
      CHK-LF CHK-ERR-C
   THEN
   CHK-NOM-FOUND ;

\ The definer's name is read as CHK-DEFINER-TOK? reads it. A definer with no
\ token after it is refused here and answers false, so a caller answered true
\ finds the name at the next token.
: CHK-DEFINER? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u CHK-DEFINER-TOK? 0= IF LINT-FALSE exit THEN
   k 1+ LINT-LEX:COUNT < IF LINT-TRUE exit THEN
   k CHK-NONAME-FAIL
   LINT-FALSE ;

\ DEFTYPE NAME folds the UPPER-CASE surface name to the lowercase family tail
\ (SERIAL -> serial) and mints the tail with CHECKER-DEFFAMILY, as
\ lib/type/deftype.f does. The bad-name diagnostic still reports the surface
\ token the user wrote.
128 constant CHK-NOM-TAIL-CAP
create CHK-NOM-TAIL-BUF CHK-NOM-TAIL-CAP allot

: CHK-NOM-TAIL$ ( n -- ptr u8 n ) {: name:n :}
   name LINT-LEX:TOKEN {: a:ptr u:n :}
   u CHK-NOM-TAIL-CAP > IF E-FS-CAPACITY throw THEN
   a u CHK-NOM-TAIL-BUF FOLD-TO
   CHK-NOM-TAIL-BUF u ;

\ A DEFLINEAR or VALUE-RECORD name is spelled in effects as written, unfolded,
\ and its loader refuses it by TYPE-RESERVED? on those bytes; asking the same
\ word first is what lets a refusal become a diagnostic instead of the
\ registration's die.
: CHK-NOM-NAME-BAD? ( n -- bool )
   LINT-LEX:TOKEN TYPE-RESERVED? ;

: CHK-LIN-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-LIN-FAIL EXIT THEN
   name LINT-LEX:TOKEN CHECKER-DEFLINEAR ;

: CHK-VREC-FAIL ( n n -- )
   s" value-record" CHK-TYPE-FAIL ;

: CHK-FIELD-SUG$ ( -- ptr u8 n )
   s" Declare at least one field, each with a unique name and a known type." ;

\ A value-record field the registration refuses, at field token tok of the
\ declaration from def: the packet carries the refusal as its reason, and the
\ prose gives it with the field.
: CHK-FIELD-FAIL ( n n ptr u8 n -- ) {: def:n tok:n msg:ptr msgu:n :}
   CHK-JSON @ IF
      def tok s" E-BAD-RECORD-FIELD" s" fix_record_field" s" value-record" CHK-PACKET-START
      s" reason" msg msgu CHK-NOM-JSTR
      CHK-FIELD-SUG$ CHK-PACKET-END
   ELSE
      tok CHK-AT-PROSE
      msg msgu CHK-ERR
      s"  '" CHK-ERR
      tok LINT-LEX:TOKEN CHK-ERR
      39 CHK-ERR-C
      CHK-LF CHK-ERR-C
   THEN
   CHK-NOM-FOUND ;
\ The rows of CHK-EXP-BUF, two cells each: the text offset a run starts at and
\ the source offset the lexer read it from, which CHK-EXP$ hands to the checker
\ (src/core/checker.f DIAG-MAP!) so a declaration packet locates its token in
\ the file. A run takes at least one byte and each later run a separator too,
\ so CHK-SRC-CAP + 2 cells hold every row the buffer can have.
TYPED-VARIABLE CHK-EXP-ROW-A ptr n
variable CHK-EXP-ROWS

: CHK-EXP-ROW ( -- ptr n )
   CHK-EXP-ROW-A @ 0= if
      CHK-SRC-CAP 2 + MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS CHK-EXP-ROW-A !
   then
   CHK-EXP-ROW-A @ ;

: CHK-VREC-RESET ( -- )
   0 CHK-EXP-U !
   0 CHK-EXP-ROWS ! ;

: CHK-VREC-ROOM ( n -- )
   CHK-EXP-U @ + CHK-SRC-CAP > IF E-FS-CAPACITY throw THEN ;

: CHK-VREC-C ( n -- ) {: c:n :}
   1 CHK-VREC-ROOM
   c CHK-EXP-BUF CHK-EXP-U @ + c!
   CHK-EXP-U @ 1+ CHK-EXP-U ! ;

: CHK-VREC-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-VREC-ROOM
   a CHK-EXP-BUF CHK-EXP-U @ + u BYTE-COPY
   CHK-EXP-U @ u + CHK-EXP-U ! ;

: CHK-VREC-ROW! ( n -- )        \ the next run, read from source offset n
   CHK-EXP-ROW {: src:n rows:ptr :}
   CHK-EXP-U @  CHK-EXP-ROWS @ 2 * cells rows + !
   src  CHK-EXP-ROWS @ 2 * 1 + cells rows + !
   CHK-EXP-ROWS @ 1 + CHK-EXP-ROWS ! ;

: CHK-VREC-TOKEN+ ( ptr u8 n n -- )   \ a token and its source offset
   {: a:ptr u:n src:n :}
   CHK-EXP-U @ 0 > IF CHK-SP CHK-VREC-C THEN
   src CHK-VREC-ROW!
   a u CHK-VREC-APP ;

: CHK-VREC-LEX+ ( n -- )        \ lexer token k
   {: k:n :}
   k LINT-LEX:TOKEN  k LINT-LEX:BYTE@  CHK-VREC-TOKEN+ ;

\ The rebuilt declaration body, with its rows armed for the packet it may write.
: CHK-EXP$ ( -- ptr u8 n )
   CHK-EXP-BUF CHK-EXP-U @ CHK-EXP-ROW CHK-EXP-ROWS @ DIAG-MAP!
   CHK-EXP-BUF CHK-EXP-U @ ;

: CHK-VREC-END? ( n -- bool )
   s" END-VALUE-RECORD" CHK-TOK=CI ;

\ The token holding byte at of the field text CHK-VREC-TOKEN+ joined, one space
\ apart, from the tokens after the record name. The end of an empty text is the
\ END-VALUE-RECORD token.
: CHK-VREC-FIELD-TOKEN ( n n -- n ) {: name:n at:n :}
   name 1+ 0
   begin over LINT-LEX:TOKEN nip over + at < while
      over LINT-LEX:TOKEN nip + 1+
      swap 1+ swap
   repeat drop ;

\ The registration answers the field it refuses instead of dying, and leaves
\ no part of the record behind, so the scan reports it and goes on.
: CHK-VREC-DEFRECORD ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-VREC-FAIL EXIT THEN
   name LINT-LEX:TOKEN CHK-EXP-BUF CHK-EXP-U @ CHECKER-TRY-RECORD
   {: at:n msg:ptr msgu:n :}
   msgu 0= IF EXIT THEN
   def name at CHK-VREC-FIELD-TOKEN msg msgu CHK-FIELD-FAIL ;

: CHK-VREC-REGISTER ( n n -- n ) {: def:n name:n :}
   CHK-VREC-RESET
   name 1+
   begin dup LINT-LEX:COUNT < while
      dup CHK-VREC-END? if
         def name CHK-VREC-DEFRECORD
         1+ exit
      then
      dup CHK-VREC-LEX+
      1+
   repeat
   s" check.f: missing END-VALUE-RECORD" CHK-E-CHECK CHK-FAIL ;

: CHK-TFAM-DO-DEF ( -- )         \ arity token, or empty when absent (missing-arity packet)
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-TFAM-NAME-I @ 1+ dup LINT-LEX:COUNT < IF LINT-LEX:TOKEN ELSE drop s" " THEN
   CHECKER-DEFFAMILY ;

: CHK-SUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP$
   CHECKER-DEFSUM ;

: CHK-SUM-DO-NOEND ( -- )        \ unterminated: declaration packet from name + partial body
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP$
   CHECKER-DEFSUM-NOEND ;

\ A failed NEWTYPE/SUMTYPE declaration already reported through the
\ checker's declaration diagnostics (TDECL-DIAG, declaration-shaped packet);
\ capture that packet into the check error stream (the preverify pattern) and
\ map the registration throw to the check rc without a second packet. The
\ packet names the file the nominal pass is reading, as every other report does.
: CHK-DECL-CAPTURE ( -- )
   CHK-LABEL DIAG-FILE!
   CHK-JSON @ DIAG-JSON!
   CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER! ;

: CHK-DECL-FLUSH ( -- )
   DIAG-BUFFER$ CHK-ERR
   DIAG-BUFFER-OFF ;

: CHK-DECL-FAIL ( n -- )
   dup 0= IF drop EXIT THEN
   drop CHK-E-CHECK CHK-THROW ;

\ Missing arity is reported by CHECKER-DEFFAMILY through the declaration packet
\ (§24), matching native/verify-source -- not a raw pre-check throw.
: CHK-TFAM-REGISTER ( n -- n ) {: k:n :}   \ k at 'newtype'; next scan index
   k 1+ CHK-TFAM-NAME-I !
   CHK-DECL-CAPTURE
   [: CHK-TFAM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   k 3 + ;

: CHK-NOM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ CHK-NOM-TAIL$ s" 0" CHECKER-DEFFAMILY ;

\ A DEFTYPE name is refused exactly when CHECKER-DEFFAMILY refuses its tail,
\ which is the loader's rule. TYPE-RESERVED? is not that rule: it refuses any
\ tail a family in any package claims, including a global family's or another
\ package's that a package family may share (TDECL-REQUIRE-FAMILY-NAME), and
\ it passes control words and sum variants. With the arity fixed at 0 a refusal
\ can only be about the name, so the bad-nominal diagnostic replaces the
\ declaration packet.
: CHK-NOM-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-TFAM-NAME-I !
   CHK-DECL-CAPTURE
   [: CHK-NOM-DO-DEF ;] catch {: rc:n :}
   DIAG-BUFFER-OFF
   rc 0= IF EXIT THEN
   def name CHK-NOM-FAIL ;

\ shared block-declaration collector: name at k+1, body tokens buffered from
\ k+2 to the end token. Returns the next scan index and true, or k and false
\ when the input ends unterminated (caller reports its own terminator).
: CHK-BLOCK-COLLECT ( n ptr u8 n -- n bool ) {: k:n ea:ptr eu:n :}
   k 1+ CHK-TFAM-NAME-I !
   CHK-VREC-RESET
   k 2 +
   begin dup LINT-LEX:COUNT < while
      dup ea eu CHK-TOK=CI if 1+ LINT-TRUE exit then
      dup CHK-VREC-LEX+
      1+
   repeat drop k LINT-FALSE ;

\ Unterminated SUMTYPE is reported by CHECKER-DEFSUM-NOEND through the declaration packet
\ (§24), matching native/verify-source -- not a raw pre-check throw.
: CHK-SUM-REGISTER ( n -- n ) {: k:n :}    \ k at 'sumtype'; next scan index
   k s" ;SUMTYPE" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-SUM-DO-NOEND ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-SUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

\ ENUM/PRODUCT/STRUCTURE block declarations (items 14/15) register through the
\ same collector so signature uses of the family later in the file resolve. The
\ enum arm also closes the item-14 gap where an enum-declaring file failed
\ the nominal pass with unknown-family rejects.
\
\ ENUM and STRUCTURE register through the unified front ends' replay entries
\ (ENUM-DECL:ED-REPLAY, STRUCTURE-DECL:SD-REPLAY), which run the real
\ declaration grammar over the collected tokens and define no word. ENUM used to
\ drive sumtype.f's CHECKER-DEFENUM, which the type-DSL cutover deletes and
\ which only ever understood the compact form; the replay entry accepts both
\ modes. STRUCTURE had no arm at all, so any file declaring one left that family
\ unregistered and the next declaration naming it as a payload type rejected
\ with "unknown payload type".
\ PRODUCT stays on the legacy definer; the cutover retires that arm with the
\ definer itself.
\
\ CHK-BLOCK-COLLECT stops ON the terminator and does not buffer it, so each
\ replayed body re-appends the exact terminator the collector just matched. The
\ front ends parse their own terminator, and a body missing one is what their
\ "missing ;ENUM" / "missing ;STRUCTURE" gates exist to report.
\
\ COMMENTS MUST SURVIVE THE CAPTURE. The engine reads a declaration body with
\ `parse-name`, which has no comment rule, so a `\` or `(` inside one is an
\ ordinary token that hits the name gate: `ENUM c red \ note` rejects 7101
\ "name must be a lowercase tail at '\'". The lexer above disagrees — it drops a
\ `\` line comment without emitting any token at all (a `( .. )` comment does
\ become one, which is why the paren case already rejected). Rebuilding the body
\ from those tokens would therefore LAUNDER a `\` comment out of the source and
\ let the replay register a family the engine refuses.
\
\ So these two arms rebuild the body from the RAW SOURCE BYTES of the
\ declaration window instead of from the token list: from the first body token's
\ byte offset up to the terminator token's. That is byte-for-byte what the live
\ keyword would have read, comments included. The span is bounded by the two
\ token offsets the collector already found, so it is scoped strictly to one
\ declaration; every other scan in this file, and the legacy SUMTYPE/PRODUCT
\ arms, keep the token-joined body unchanged.
: CHK-DECL-RAW-BODY ( n n ptr u8 n -- ) {: k:n nxt:n ea:ptr eu:n :}
   CHK-VREC-RESET
   k 2 + {: b:n :}                     \ first body token
   nxt 1 - {: t:n :}                   \ the terminator the collector matched
   b t < IF
      b LINT-LEX:BYTE@ {: s:n :}
      t LINT-LEX:BYTE@ {: e:n :}
      s CHK-VREC-ROW!
      CHK-SRC-BUF s +  e s -  CHK-VREC-APP
   THEN
   ea eu  t LINT-LEX:BYTE@  CHK-VREC-TOKEN+ ;
: CHK-ENUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP$
   ENUM-DECL:ED-REPLAY ;

: CHK-STRUCT-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP$
   STRUCTURE-DECL:SD-REPLAY ;

: CHK-PROD-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP$
   CHECKER-DEFPRODUCT ;

: CHK-ENUM-REGISTER ( n -- n ) {: k:n :}   \ k at 'enum'; next scan index
   k s" ;ENUM" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-ENUM-DO-DEF ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   k nxt s" ;ENUM" CHK-DECL-RAW-BODY
   CHK-DECL-CAPTURE
   [: CHK-ENUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

: CHK-STRUCT-REGISTER ( n -- n ) {: k:n :}   \ k at 'structure'; next scan index
   k s" ;STRUCTURE" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-STRUCT-DO-DEF ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   k nxt s" ;STRUCTURE" CHK-DECL-RAW-BODY
   CHK-DECL-CAPTURE
   [: CHK-STRUCT-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

: CHK-PROD-REGISTER ( n -- n ) {: k:n :}   \ k at 'product'; next scan index
   k s" ;PRODUCT" CHK-BLOCK-COLLECT 0= if
      drop s" check.f: missing ;PRODUCT" CHK-E-CHECK CHK-FAIL
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-PROD-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

\ Package blocks (dot habu-tools-check-scanner-685b735e) mirror verify-source's
\ RECORD-PACKAGE/-PUBLIC/-PRIVATE/-END-PACKAGE at the checker level, so the
\ NEWTYPE/SUMTYPE/ENUM/PRODUCT registrations above land in the declaring
\ package under the live visibility mode (TDECL reads CHECKER-PACKAGE-*), and
\ qualified pkg:tail signature uses resolve public-only exactly as native.
\ The CHECKER-SCOPE frame wrapping the nominal pass saves/restores package
\ state (RBF.PKGMODE/PKGU). Boundary: the name is the token parse-name takes
\ after `package`, and CHK-DEFINER? refuses a missing one, while
\ native-loader misuse (nesting, public outside a package, ':' in a name)
\ stays fail-closed through preverify and the child run.
: CHK-PKG-REGISTER ( n -- n ) {: k:n :}   \ k at 'package'; next scan index
   k 1+ LINT-LEX:TOKEN CHECKER-PACKAGE
   k 2 + ;

: CHK-PKG-STEP ( n -- n bool ) {: k:n :}   \ package-word dispatch: next index, handled
   k s" package" CHK-DEFINER? if k CHK-PKG-REGISTER LINT-TRUE exit then
   k s" public" CHK-TOK=CI if CHECKER-PUBLIC k 1+ LINT-TRUE exit then
   k s" private" CHK-TOK=CI if CHECKER-PRIVATE k 1+ LINT-TRUE exit then
   k s" ;package" CHK-TOK=CI if CHECKER-END-PACKAGE k 1+ LINT-TRUE exit then
   k LINT-FALSE ;

: CHK-DEF-OPENER? ( n -- bool ) {: k:n :}
   k s" :" CHK-DEFINER? IF LINT-TRUE exit THEN
   k s" TRUSTED:" CHK-DEFINER? ;

\ A raw operand is data, never a definer: `' :` starts no definition. A
\ definition ends where the expansion walker ends it, so `[char] ;` in a body
\ does not end it here either. `undefine` takes its name as a definer does, so
\ a missing one is refused here, at `undefine`, before the pre-verifier, which
\ dies on it with no location (src/habu/verify-source.f UNDEFINE-WORD).
: CHK-NOM-STEP ( n -- n ) {: k:n :}
   k LINT-LEX:OPERAND? if k 1+ exit then
   k CHK-DEF-OPENER? if k 1+ CHK-WALK-DEF exit then
   k s" undefine" CHK-DEFINER? if k 2 + exit then
   k CHK-PKG-STEP if exit then drop
   k s" deftype" CHK-DEFINER? if
      k k 1+ CHK-NOM-REGISTER
      k 2 + exit
   then
   k s" deflinear" CHK-DEFINER? if
      k k 1+ CHK-LIN-REGISTER
      k 2 + exit
   then
   k s" VALUE-RECORD" CHK-DEFINER? if
      k k 1+ CHK-VREC-REGISTER exit
   then
   k s" NEWTYPE" CHK-DEFINER? if
      k CHK-TFAM-REGISTER exit
   then
   k s" SUMTYPE" CHK-DEFINER? if
      k CHK-SUM-REGISTER exit
   then
   k s" ENUM" CHK-DEFINER? if
      k CHK-ENUM-REGISTER exit
   then
   k s" STRUCTURE" CHK-DEFINER? if
      k CHK-STRUCT-REGISTER exit
   then
   k s" PRODUCT" CHK-DEFINER? if
      k CHK-PROD-REGISTER exit
   then
   k 1 + ;

\ The file is lexed whole and every declaration in it registered, in its own
\ name, so each report names the file it reads.
: CHK-RUN-NOMINAL-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-NOM-U !
   CHK-SRC-BUF CHK-NOM-U @ 1 1 0 DIAG-SOURCE!
   CHK-SRC-BUF CHK-NOM-U @ LINT-LEX:SOURCE
   0 CHK-NOM-I !
   begin CHK-NOM-I @ LINT-LEX:COUNT < while
      CHK-NOM-I @ CHK-NOM-STEP CHK-NOM-I !
   repeat ;

: CHK-RUN-NOMINAL-AS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   label labelu CHK-LABEL!
   path pathu CHK-RUN-NOMINAL-FILE ;

: CHK-DEP-PRELOAD? ( n -- bool ) {: id:n :}
   id CHK-DEP$ RESOLVE nip nip 0= ;

: CHK-RUN-NOMINAL-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-DEP$ 2dup CHK-RUN-NOMINAL-AS ;

: CHK-RUN-NOMINAL-ORDER ( -- )
   CHK-LABEL {: old:ptr oldu:n :}
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-RUN-NOMINAL-ID
      1+
   repeat drop
   old oldu CHK-LABEL! ;

: CHK-RUN-NOMINAL-FILES ( -- )
   CHK-DEP-ORDER-N @ 0 > if CHK-RUN-NOMINAL-ORDER exit then
   CHK-SOURCE CHK-RUN-NOMINAL-FILE ;

: CHK-RUN-NOMINAL ( -- )
   LINT-FALSE CHK-NOM-BAD !
   CHK-RUN-NOMINAL-FILES
   CHK-NOM-BAD @ if CHK-E-CHECK CHK-THROW then ;

: CHK-VERIFIER-XT ( n -- n ) {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF E-NCOMP-OWNER throw THEN ;

TRUSTED: CHK-VERIFIER-ACTION ( n -- [ -- ] ) ;

TRUSTED: CHK-RUN-NOMINAL-AUTH ( -- )
   CHECKER-OWNER-ABI:VERIFY-START-OFF CHK-VERIFIER-XT CHK-VERIFIER-ACTION execute
   [: CHK-RUN-NOMINAL ;] catch
   DIAG-SOURCE-OFF
   CHECKER-OWNER-ABI:VERIFY-DONE-OFF CHK-VERIFIER-XT CHK-VERIFIER-ACTION execute
   dup 0= if drop exit then
   throw ;

: CHK-RUN-RESET ( -- )
   0 CHK-RUN-U ! ;

: CHK-RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-RUN-U @ u + CHK-RUN-CAP > if E-FS-CAPACITY throw then
   a CHK-RUN-BUF CHK-RUN-U @ + u BYTE-COPY
   CHK-RUN-U @ u + CHK-RUN-U ! ;

: CHK-RUN-C ( n -- ) {: c:n :}
   CHK-RUN-U @ 1+ CHK-RUN-CAP > if E-FS-CAPACITY throw then
   c CHK-RUN-BUF CHK-RUN-U @ + c!
   CHK-RUN-U @ 1+ CHK-RUN-U ! ;

: CHK-RUN-SP ( ptr u8 n -- )
   CHK-RUN+
   CHK-SP CHK-RUN-C ;

: CHK-RUN-LN ( ptr u8 n -- )
   CHK-RUN+
   CHK-LF CHK-RUN-C ;

: CHK-RUN-QPATH+ ( ptr u8 n -- )
   >LEN CHK-RUN-BUF CHK-RUN-CAP >LEN CHK-RUN-U SOURCE-APPEND-QPATH ;

: CHK-ERR-NONNEG ( n -- )
   dup 0 < if drop s" <negative>" CHK-ERR exit then
   CHK-U$ CHK-ERR ;

\ The run file's prefix turns checking off, names the subject for the checker's
\ diagnostics, turns its warnings off and builds the hook the run is checked
\ with (CHK-HOOK-ON turns it on). The check before the run wrote the warnings,
\ each in its own file and placed: the run loads what that check checked, so
\ its warnings would repeat them, naming the subject for every file and placing
\ none. It shares the subject's first line when the subject follows it, and the
\ origin markers add no line break, so line N of the run file is line N of the
\ subject: the engine counts the lines of the file it reads when it refuses a
\ statement.
: CHK-BUILD-HOOK ( -- )
   s" 0 set-check" CHK-RUN-SP
   CHK-LABEL CHK-RUN-QPATH+
   s"  DIAG-FILE!" CHK-RUN-SP
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-SP then
   s" 0 WARN-DIAGS !" CHK-RUN-SP
   s" : CHECK-F-HOOK ( ptr u8 n -- n ) LOWER-CERT-HOOK:HOOK ;" CHK-RUN-SP
   s" LOWER-CERT-HOOK:INSTALL" CHK-RUN-SP ;

: CHK-HOOK-ON ( -- )
   s" ' CHECK-F-HOOK set-check" CHK-RUN-SP ;

\ The origin pass appends the marked copy of the source file at the given path
\ to the run buffer, reading it through check.f's own buffer and cap, as the
\ nominal pass does. The engine comments a leading `#!` line only at the start
\ of a file it reads, so the pass comments the source's own first, with the
\ engine's rewrite: the scan then reads that line as a comment, and the marked
\ copy carries the rewrite the run needs.
: CHK-RUN-ORIGIN+ ( ptr u8 n -- )
   CHK-SRC-BUF CHK-SRC-CAP READ-ALL {: len:n :}
   CHK-SRC-BUF len SOURCE-ROOT:SHEBANG-COMMENT
   CHK-SRC-BUF len
   CHK-RUN-BUF CHK-RUN-U @ +  CHK-RUN-CAP CHK-RUN-U @ - >LEN
   DIAG-ORIGIN-SOURCE>BUF LEN>N CHK-RUN-U @ + CHK-RUN-U ! ;

\ A named file runs as the command line runs it, loaded by its canonical path
\ (CHK-APPEND-ENTRY): its own relative requires resolve against its directory,
\ and a file that requires it back finds it loaded. The engine still reads the
\ copy the origin pass marked: the marked copy lies at CHK-MARK-PATH, and the
\ run's loader answers the subject's path with it (src/core/include.f
\ SOURCE-INPUT:USE), so a refusal in the subject names its own line and column.
: CHK-WRITE-MARKED ( -- )
   CHK-RUN-RESET
   CHK-SUBJ$ CHK-RUN-ORIGIN+
   CHK-MARK-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-RUN-RESET ;

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
   CHK-BUILD-HOOK
   CHK-SUBJ-U @ 0 > if CHK-BUILD-READER then
   CHK-HOOK-ON
   CHK-SOURCE CHK-RUN-ORIGIN+ ;

\ A source the read admits can still outgrow the run file once the prefix and
\ the origin marks join it; it is refused here, before the run.
: CHK-BUILD-RUN ( -- )
   CHK-RUN-RESET
   [: CHK-BUILD-ACT ;] CHK-CAPPED ;

: CHK-LOAD-RESET ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+ ;

\ The deadline the child is given.
: CHK-DEADLINE@ ( -- ms )
   CHK-DEADLINE @ dup 0= if drop CHK-DEADLINE-MS then >MS ;

\ The checked program runs with check.f's own environment: PATH for a TOOL
\ lookup, HOME and HB_TMP for a scratch tree. An env-less spawn hands it a
\ one-NULL envp, and a program that resolves an executable through $PATH then
\ fails in the run stage while `--load` of the same file passes.
\
\ It runs on the engine lib/engine-candidate.f names: a gate's HABU_UNDER_TEST,
\ else the engine running check.f, never a bin/hb of the working directory.
\ An engine the resolver refuses is its E-FS-OPEN, the check's status.
: CHK-RUN-CAPTURE ( -- )
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
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
\ it started (lib/process.f PROC-KILL-CAPTURE), and refused: what it wrote is
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

\ A source list materializes as one `required` line per listed file, so a lint
\ reads the listed files themselves, in list order, except a source the engine
\ provides, which the run loads nothing from.
: CHK-LINT-LISTED ( [ ptr u8 n -- ] -- ) {: lint :}
   CHK-POS-N @ 0 ?do
      i CHK-POS$ ENGINE-PROVIDES? 0= if i CHK-POS$ lint execute then
   loop ;

: CHK-RUN-BOUNDARY ( -- )
   2 >FD CHECKED-BOUNDARY-LINT:OUT-FD!
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
   2 >FD RESERVED-NAME-LINT:OUT-FD!
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
   CHK-ALL-ID @ CHK-DEP$ 2dup CHECK-ALL-ERRORS:LEX-FILE ;

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
      dup cells CHK-DEP-ORDER + @ CHK-RUN-ALL-ID
      1+
   repeat drop
   CHK-ALL-RC @ 0 <> if CHK-ALL-RC @ throw then ;

\ The report goes to standard error as the core makes it: every record of every
\ file, however many and however long, with no buffer to outgrow.
: CHK-RUN-ALL ( -- )
   2 >FD CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS:STREAM!
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   CHK-DEP-ORDER-N @ 0 > if CHK-RUN-ALL-ORDER then
   CHK-SOURCE-BYTES CHK-SRC-PATH CHK-LABEL CHECK-ALL-ERRORS:COMPOSE-BUF ;

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

\ The pre-pass stops in one file of the composition, the one
\ PREVERIFY-STOPPED$ names: the subject by its label, any other file by its
\ canonical path. Its bytes are the subject's, or the file's own.
: CHK-STOPPED-SOURCE ( -- ptr u8 n )
   PREVERIFY-SUBJECT? if CHK-SOURCE-BYTES exit then
   PREVERIFY-STOPPED$ CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-SRC-BUF swap ;

\ A statement that threw while it was checked is reported where it stood, by
\ the record --all-errors writes in the run's mode, and the check fails with a
\ refusal's status, which this answers.
: CHK-PREVERIFY-THREW ( n -- n ) {: rc:n :}
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   rc PREVERIFY-AT PREVERIFY-STOPPED$ CHK-STOPPED-SOURCE
   CHECK-ALL-ERRORS:THROW-RECORD$ CHK-ERR-LN
   CHK-E-CHECK ;

\ The checker reports nothing for a duplicate definition, so it is reported by
\ the record --all-errors writes in the run's mode, naming the file that defined
\ the name again and the name and line read from that file where the pre-pass
\ refused it (PREVERIFY-DUPLICATE), and fails the run with the duplicate's
\ status, as it fails the load.
: CHK-PREVERIFY-DUP ( -- )
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   PREVERIFY-DUPLICATE PREVERIFY-STOPPED$ CHK-STOPPED-SOURCE
   CHECK-ALL-ERRORS:DUP-RECORD$ CHK-ERR-LN ;

\ The pre-pass stopped with the given code: the record a throw or a duplicate
\ leaves, then the failure.
: CHK-PREVERIFY-STOPPED ( n -- ) {: rc:n :}
   rc CHECK-ALL-ERRORS:THREW? if rc CHK-PREVERIFY-THREW CHK-PREVERIFY-FAIL then
   rc CHECK-ALL-ERRORS:DUP-RC = if CHK-PREVERIFY-DUP then
   rc CHK-PREVERIFY-FAIL ;

\ How a verifier child that gave no answer ended, as the closing line of its
\ report: on stdout under --verify-only, with the rest of its prose, and on
\ stderr for the pre-pass.
: CHK-STATUS-LN ( outcome -- )
   SB-RESET
   s" check.f: the verifier did not complete: " SB-APPEND
   MATCH outcome
      exited OF s" exit " SB-APPEND CHK-U$ SB-APPEND ENDOF
      signaled OF s" signal " SB-APPEND CHK-U$ SB-APPEND ENDOF
      timeout OF s" deadline of " SB-APPEND CHK-DEADLINE@ MS>N CHK-U$ SB-APPEND s"  ms passed" SB-APPEND ENDOF
   ;MATCH
   SB$ CHK-EXPLAIN-LN ;

: CHK-TRUNCATED-LN ( -- )
   s" check.f: the verifier did not complete: its output exceeded the capture" CHK-EXPLAIN-LN ;

\ The pre-pass's packets and its child's prose, both on stderr.
: CHK-PREVERIFY-RELAY ( -- )
   VERIFY-OUT$ CHK-ERR
   VERIFY-LOG$ CHK-ERR ;

\ The pre-pass runs in the verifier child, on the engine's image
\ (CHECK:PREVERIFY-BYTES), so the words it resolves are the engine's and the
\ ones the subject loads, the words the run will have, never a word only this
\ process loaded: lib/fs.f's FILE-SIZE for check.f, lib/test.f's T= for a
\ harness that checks in process. Its packets and the child's prose come
\ first, then what its stop leaves.
: CHK-PREVERIFY-REPORT ( result<n,outcome> -- )
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

\ An inlined subject is the run file line for line (CHK-BUILD-HOOK), so where
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
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPPED
   CHK-ERR-NAME-SUBJECT ;

: CHK-RUN-JSON-ONLY ( -- )
   2 >FD 2 >FD JSON-ONLY-FDS!
   CHK-ERR-BUF CHK-ERR-U @ JSON-ONLY-FILTER ;

\ Whether the run ended on an uncaught throw of code.
: CHK-RUN-THREW? ( n -- bool ) {: code:n :}
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   code FMT:SB-INT
   CHK-LF SB-APPEND-C
   CHK-ERR-BUF CHK-ERR-U @ SB$ ENDS-WITH? ;

\ check.f exits with its run's status, but a run that ends on a refused checker
\ record's throw exits UNCAUGHT-RC as any unhandled throw does, and as check.f
\ exits for an overlong path (CHK-E-CAPACITY). That run is refused, so it exits
\ as a refusal.
: CHK-RUN-STATUS ( n -- n ) {: rc:n :}
   rc CHK-UNCAUGHT-RC <> if rc exit then
   CHK-E-TRUST-ROW CHK-RUN-THREW? if CHK-E-CHECK exit then
   CHK-E-PKG-RECORD CHK-RUN-THREW? if CHK-E-CHECK exit then
   CHK-E-QUALIFIED-RECORD CHK-RUN-THREW? if CHK-E-CHECK exit then
   rc ;

: CHK-HANDLE-HB ( -- )
   CHK-RC @ 0= if
      CHK-REPLAY
      exit
   then
   CHK-RC @ CHK-RUN-STATUS {: rc:n :}
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-JSON @ if
      CHK-RUN-JSON-ONLY
   else
      CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   then
   rc CHK-THROW ;

\ The nominal pass registers the declarations it finds in the subject source.
\ Those declarations belong to the packages that source declares, so the scope
\ starts at neutral top level instead of adopting the caller's package.
: CHK-RUN-NOMINAL-LINTS ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [:
      CHK-RUN-NOMINAL-AUTH
      CHK-RUN-RESERVED-NAMES
      CHK-RUN-BOUNDARY
   ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

: CHK-RUN-STATIC-LINTS ( -- )
   \ Zero-byte input traverses every phase; a bypass is publicly unobservable.
   CHK-RUN-NOMINAL-LINTS
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

\ --verify-only is CHECK:VERIFY-BYTES over a named file's bytes, or over stdin's
\ under --stdin-path, and nothing more: no lint, no in-process verify, no run.
\ stderr carries its packets and nothing else; stdout carries its prose and, for
\ an outcome that is not the checker's verdict, a closing line.

: CHK-VERIFY-FILE ( -- ptr u8 n )
   0 CHK-POS$ {: a:ptr u:n :}
   a u FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   a u FILE-SIZE CHK-SRC-CAP > if CHK-SOURCE-TOO-BIG then
   a u CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-SRC-U !
   a u ;

: CHK-VERIFY-STDIN-ACT ( -- )
   CHK-SRC-BUF CHK-SRC-CAP >LEN READ-STDIN-ALL LEN>N CHK-SRC-U ! ;

\ stdin's path need not exist.
: CHK-VERIFY-STDIN ( -- ptr u8 n )
   CHK-STDIN-PATH-U @ 0= if CHK-USAGE then
   [: CHK-VERIFY-STDIN-ACT ;] catch {: rc:n :}
   rc E-FS-CAPACITY = if CHK-SOURCE-TOO-BIG then
   rc 0<> if rc throw then
   CHK-STDIN-PATH$ ;

\ Read the bytes into the source buffer: the path they stand for.
: CHK-VERIFY-SELECT ( -- ptr u8 n )
   CHK-SEL-MODE @ CHK-SEL-FILE = if
      CHK-STDIN-PATH-U @ 0<> if CHK-USAGE then
      CHK-VERIFY-FILE exit
   then
   CHK-SEL-MODE @ CHK-SEL-NONE <> if CHK-USAGE then
   CHK-VERIFY-STDIN ;

\ The verifier's packets on stderr, its prose on stdout.
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
   ;MATCH ;

: CHK-VERIFY-ACT ( -- )
   CHK-VERIFY-SELECT {: path:ptr pathu:n :}
   CHK-SRC-BUF CHK-SRC-U @ path pathu CHK-DEADLINE@ VERIFY-BYTES
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
\
\ check.f ANSWERS SIGTERM, SIGINT AND SIGHUP AS THE GATE ROOT DOES (lib/signal.f
\ CATCH-STOPS). Under the default action it ends where it stands, and its run
\ stage's child, a process group of its own, runs on under init beside the
\ temporary directory: a `timeout` that kills check.f's group does not reach
\ it. MAIN catches the three before it makes that directory, the capture that
\ waits on the child hears them (lib/process.f PROC-STOP), and MAIN asks once
\ more when the run's own cleanup is done, for one that arrived while no
\ capture waited. RUN catches nothing: a program that runs a check in process
\ keeps its own answer.
\
\ The answer kills the child with every process under it and reaps it
\ (lib/process.f PROC-KILL-CAPTURE), removes the temporary directory, and dies
\ of the signal. A step that throws is named and the answer goes on. A run that
\ ended before its child started - a refused program, a usage error, resident
\ inputs - leaves the row's pid at 0 or PROC-NO-PID, and PROC-KILL-CAPTURE
\ kills nothing for either: kill(0) is check.f's own process group.
: CHK-SAY-THROW ( ptr u8 n n -- ) {: what:ptr whatu:n code:n :}
   code 0= if exit then
   s" check: " CHK-ERR what whatu CHK-ERR s"  threw " CHK-ERR
   SB-RESET code FMT:SB-INT SB$ CHK-ERR-LN ;

: CHK-SIGNAL-ANSWER ( n -- ) {: sig:n :}
   s" child kill" [: PROC-KILL-CAPTURE ;] catch CHK-SAY-THROW
   s" cleanup" [: CHK-TEMP-CLEAN ;] catch CHK-SAY-THROW
   s" check: signal" sig SIGNAL:DIE-OF ;

: CHK-SIGNAL-CHECK ( -- )
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF CHK-SIGNAL-ANSWER ENDOF
      timeout OF ENDOF
   ;MATCH ;

: CHK-CATCH-STOPS ( -- )
   SIGNAL:CATCH-STOPS
   SIGNAL:FD FD>N PROC-STOP-FD !
   [: CHK-SIGNAL-CHECK ;] is PROC-STOP ;

: CHK-MAIN-RUN ( -- )
   RESET
   CHK-VERIFY-ARG? CHK-VERIFY !
   CHK-PARSE-CLI
   RUN dup 0 <> if throw then drop ;

public

: MAIN ( -- )
   CHK-CATCH-STOPS
   [: CHK-MAIN-RUN ;] catch {: rc:n :}
   CHK-SIGNAL-CHECK
   rc 0 <> if rc throw then ;

;using
;using
;package
