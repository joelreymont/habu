\ check-verify-child-lib.f - the verifier child's body. tools/check-verify-child.f
\ states the contract, loads this file from its own tree and runs
\ CHECK-VERIFY-CHILD:MAIN.

\ The observation must precede every verifier require: those files may change the
\ compiler or replace a checker in this child, while the hypothetical subject has
\ not reached any of them at its own load boundary.
tick-order@
package CHECK-VERIFY-CHILD
variable ENTRY-ORDER
PTR-VARIABLE ENTRY-OWNER
ENTRY-OWNER !
ENTRY-ORDER !
;package

\ hb carries the verifier. tools/check-verify-child.f loads this file under its
\ own tree's root, so on an engine that does not carry it the verifier comes from
\ that tree, not the client's.
require src/habu/verify-source.f

package CHECK-VERIFY-CHILD

ENTRY-ORDER @ ENTRY-OWNER @ VERIFY:ENTRY-TICK-ORDER!

$10000 constant CHUNK                   \ bytes asked of one read
32 constant NUM-CAP
10 constant LF
1 constant OUT-FD

DYNAMIC-BUFFER IN u8                    \ stdin's bytes: the open documents, then the subject's
variable IN-U
variable POS                            \ where the next of the open documents' lines starts in IN
DYNAMIC-BUFFER OPEN-AT n                \ each open document's path's offset in IN and length, then its text's
variable OPEN-N
variable SUBJECT-AT                     \ where the subject's bytes start in IN
variable SUBJECT-U
variable SUBJECT-PATH-AT
variable SUBJECT-PATH-U
variable BATCH?                         \ framed subjects share this isolated child
variable REF-BATCH?                     \ only the selected tail's use lines are needed
variable BATCH-INDEX
variable RD
variable FAILED
variable NUM-I
create NUM NUM-CAP allot
create NL 1 allot
create ESC 6 allot                      \ one \u00XX escape
variable RUN-AT                         \ where the bytes not yet written start
DYNAMIC-BUFFER DEF-PKG u8
variable DEF-PKG-U
DYNAMIC-BUFFER IDX-EVENTS n              \ symbol, start, end for each declaration
variable IDX-N
TYPED-VARIABLE OF-A ^u8              \ the fourth form's TAIL,
variable OF-U                           \ 0 for the other forms
variable JSON?   true JSON? !           \ records are JSON, false after `--prose`


: WRITE ( n ^u8 n -- ) {: fd:n a:^ u:n :}
   u 0= if exit then
   fd a u write u <> if s" check-verify: write failed" 74 die then ;


: NEWLINE ( n -- ) {: fd:n :}
   LF NL c!
   fd NL 1 WRITE ;


: U$ ( n -- ^u8 n )
   NUM-CAP NUM-I !
   begin
      dup 10 mod 48 + NUM-I @ 1- dup NUM-I ! NUM + c!
      10 /
   dup 0= until drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - ;


: FD-N ( n n -- ) {: fd:n n:n :}
   n 0 < if fd s" -" WRITE fd 0 n - U$ WRITE exit then
   fd n U$ WRITE ;


: READ-IN ( -- )
   0 IN-U !
   begin
      IN-U @ CHUNK + IN-RESERVE
      0 IN-U @ IN CHUNK read RD !
      RD @ 0 < if s" check-verify: cannot read the open documents and the subject" 74 die then
      RD @ 0 >
   while
      IN-U @ RD @ + IN-U !
   repeat ;


: TABLE-FAULT ( -- )
   s" check-verify: malformed open documents" 74 die ;


\ The count in IN from POS up to the byte C, POS moved past C. READ-IN keeps
\ room past IN-U, so POS at IN-U is an index of IN.
: COUNT-TO ( n -- n ) {: c:n :}
   POS @ IN IN-U @ POS @ - c INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: k:n :}
   k 0 < if TABLE-FAULT then
   POS @ IN k STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH {: count:n :}
   count 0 < if TABLE-FAULT then
   POS @ k + 1+ POS !
   count ;


\ The open document whose line starts at POS, into OPEN-AT, POS moved past its
\ text.
: OPEN-ENTRY ( -- )
   STR-SPACE COUNT-TO {: pu:n :}
   LF COUNT-TO {: tu:n :}
   POS @ pu + tu + IN-U @ > if TABLE-FAULT then
   OPEN-N @ 4 * {: at:n :}
   at 4 + OPEN-AT-RESERVE
   POS @ at OPEN-AT !
   pu at 1+ OPEN-AT !
   POS @ pu + at 2 + OPEN-AT !
   tu at 3 + OPEN-AT !
   POS @ pu + tu + POS !
   OPEN-N @ 1+ OPEN-N ! ;


\ The open documents IN starts with, up to the empty line the subject's bytes
\ follow.
: READ-OPEN ( -- )
   0 POS !
   0 OPEN-N !
   begin
      POS @ IN-U @ >= if TABLE-FAULT then
      POS @ IN c@ LF <>
   while
      OPEN-ENTRY
   repeat
   POS @ 1+ SUBJECT-AT ! ;


: OPEN-PATH$ ( n -- ^u8 n )
   4 * {: at:n :}
   at OPEN-AT @ IN at 1+ OPEN-AT @ ;

: OPEN-TEXT$ ( n -- ^u8 n )
   4 * {: at:n :}
   at 2 + OPEN-AT @ IN at 3 + OPEN-AT @ ;


\ The text of the first open document at this canonical path and true, else
\ false (VERIFY:SUPPLIED).
: OPEN-TEXT ( ^u8 n -- ^u8 n bool ) {: a:^ u:n :}
   OPEN-N @ 0 ?do
      i OPEN-PATH$ a u CORE-STR= if i OPEN-TEXT$ true unloop exit then
   loop
   NULL$ false ;


: SUBJECT$ ( -- ^u8 n )
   SUBJECT-AT @ IN SUBJECT-U @ ;

: SUBJECT-PATH$ ( -- ^u8 n )
   BATCH? @ if SUBJECT-PATH-AT @ IN SUBJECT-PATH-U @ exit then
   0 SCRIPT-ARGV$ ;


\ stdin read, and a load of an open document's file given its text.
: READ-STDIN ( -- )
   READ-IN
   READ-OPEN
   IN-U @ SUBJECT-AT @ - SUBJECT-U !
   ['] OPEN-TEXT is VERIFY:SUPPLIED ;


: VERIFY-CUR-ACT ( -- )
   SUBJECT$ SUBJECT-PATH$ SUBJECT-PATH$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: VERIFY-CUR ( -- )
   SUBJECT-PATH$ SOURCE-ROOT:DIRNAME
   [: VERIFY-CUR-ACT ;] SOURCE-ROOT:WITH ;


: HEX-DIGIT ( n -- n )
   dup 10 < if 48 + exit then 87 + ;


: CONTROL-ESC ( n -- ^u8 n )
   {: c:n :}
   92 ESC c!  117 ESC 1 + c!  48 ESC 2 + c!  48 ESC 3 + c!
   c 16 / HEX-DIGIT ESC 4 + c!
   c 15 and HEX-DIGIT ESC 5 + c!
   ESC 6 ;


\ RFC 8259's string escapes: quotation mark, reverse solidus and control bytes;
\ empty for none.
: ESCAPE$ ( n -- ^u8 n )
   {: c:n :}
   c 34 = if s\" \\\"" exit then
   c 92 = if s\" \\\\" exit then
   c 32 < if c CONTROL-ESC exit then
   NULL$ ;


: JSON-BYTE ( ^u8 n -- )
   {: a:^ i:n :}
   a i + c@ ESCAPE$
   {: e:^ eu:n :}
   eu 0= if exit then
   OUT-FD a RUN-AT @ + i RUN-AT @ - WRITE
   OUT-FD e eu WRITE
   i 1+ RUN-AT ! ;


: JSON-STR ( ^u8 n -- )
   {: a:^ u:n :}
   OUT-FD s\" \"" WRITE
   0 RUN-AT !
   u 0 ?do a i JSON-BYTE loop
   OUT-FD a RUN-AT @ + u RUN-AT @ - WRITE
   OUT-FD s\" \"" WRITE ;


\ The package span, copied out of the checker's symbol pool before anything
\ else runs: the next intern can move it. The storage holds a byte at least,
\ so a global's empty package has an address.
: DEF-PKG! ( ^u8 n -- )
   {: pkg:^ pkgu:n :}
   pkgu 1 max DEF-PKG-RESERVE
   pkg 0 DEF-PKG pkgu BYTE-COPY
   pkgu DEF-PKG-U ! ;


: DEF-PKG$ ( -- ^u8 n )
   0 DEF-PKG DEF-PKG-U @ ;


: VISIBILITY$ ( n -- ^u8 n )
   {: vis:n :}
   vis SYM-GLOBAL = if s" global" exit then
   vis SYM-PRIVATE = if s" private" exit then
   vis SYM-PUBLIC = if s" public" exit then
   s" check-verify: unknown visibility" 74 die ;


: CLASS$ ( n -- ^u8 n )
   {: class:n :}
   class VERIFY:DEF-WORD = if s" word" exit then
   class VERIFY:DEF-CONSTANT = if s" constant" exit then
   class VERIFY:DEF-STORAGE = if s" storage" exit then
   class VERIFY:DEF-EXPORT = if s" export" exit then
   class VERIFY:DEF-CONSTRUCTOR = if s" constructor" exit then
   s" check-verify: unknown definition class" 74 die ;


\ A definition the first form retained, as its definition line: the name as
\ the event gives it, the package and visibility the checker recorded it
\ under. The tail it recorded is its lookup key, not a name to show.
: DEFINITION-LINE ( ^u8 n ^u8 n n ^u8 n n n n n -- )
   {: kind:^ kindu:n name:^ nameu:n sym:n eff:^ effu:n at:n end:n class:n visit:n :}
   REF-BATCH? @ if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:^ pkgu:n tail:^ tailu:n vis:n :}
   pkg pkgu DEF-PKG!
   OUT-FD s\" check-verify: definition {\"kind\":" WRITE
   kind kindu JSON-STR
   OUT-FD s\" ,\"class\":" WRITE
   class CLASS$ JSON-STR
   OUT-FD s\" ,\"word\":" WRITE
   name nameu JSON-STR
   OUT-FD s\" ,\"decl_visit\":" WRITE
   OUT-FD visit FD-N
   OUT-FD s\" ,\"decl_name\":" WRITE
   tail tailu JSON-STR
   OUT-FD s\" ,\"package\":" WRITE
   DEF-PKG$ JSON-STR
   OUT-FD s\" ,\"visibility\":" WRITE
   vis VISIBILITY$ JSON-STR
   effu 0<> if
      OUT-FD s\" ,\"effect\":" WRITE
      eff effu JSON-STR
   then
   OUT-FD s\" ,\"file\":" WRITE
   VERIFY:FILE$ JSON-STR
   OUT-FD s\" ,\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;

\ Index records are selected before the candidate scope destroys its symbols.
: IDX-DEFINITION ( ^u8 n ^u8 n n ^u8 n n n n n -- )
   {: kind:^ kindu:n name:^ nameu:n sym:n eff:^ effu:n at:n end:n class:n visit:n :}
   IDX-N @ 1+ 3 * IDX-EVENTS-RESERVE
   IDX-N @ 3 * {: row:n :}
   sym row IDX-EVENTS !
   at row 1+ IDX-EVENTS !
   end row 2 + IDX-EVENTS !
   1 IDX-N +! ;

: IDX-LINE ( n n n -- )
   {: sym:n at:n end:n :}
   sym VERIFY:SYM-IDENTITY {: pkg:^ pkgu:n tail:^ tailu:n vis:n :}
   tailu 0= if exit then
   pkg pkgu DEF-PKG!
   OUT-FD s\" check-verify: index {\"decl_name\":" WRITE
   tail tailu JSON-STR
   OUT-FD s\" ,\"package\":" WRITE
   DEF-PKG$ JSON-STR
   OUT-FD s\" ,\"visibility\":" WRITE
   vis VISIBILITY$ JSON-STR
   OUT-FD s\" ,\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;

: IDX-SELECT ( -- )
   IDX-N @ 0 ?do
      i 3 * {: row:n :}
      row IDX-EVENTS @ row 1+ IDX-EVENTS @ row 2 + IDX-EVENTS @ IDX-LINE
   loop ;

: INDEX-CUR ( -- )
   SUBJECT$ [: IDX-SELECT ;] VERIFY:SOURCE-INDEX-BUF-IN-SCOPE ;


\ A file visit, with the source of the bytes ON-FILE was handed. Disk metadata
\ comes from the quiet reader, never from a stat after verification.
: FILE-LINE ( ^u8 n ^u8 n -- )
   {: f:^ fu:n a:^ u:n :}
   f fu SUBJECT-PATH$ CORE-STR= {: subject:bool :}
   f fu OPEN-TEXT {: open:bool :} 2drop
   f fu VERIFY:QUIET-READ-META drop
   {: size:n msec:n mns:n csec:n cns:n :}
   OUT-FD s\" check-verify: file {\"file\":" WRITE
   f fu JSON-STR
   subject if
      OUT-FD s\" ,\"source\":\"subject\"" WRITE
   else
      open if
         OUT-FD s\" ,\"source\":\"document\"" WRITE
      else
         OUT-FD s\" ,\"source\":\"disk\",\"size\":" WRITE
         OUT-FD size FD-N
         OUT-FD s\" ,\"mtime_sec\":" WRITE OUT-FD msec FD-N
         OUT-FD s\" ,\"mtime_ns\":" WRITE OUT-FD mns FD-N
         OUT-FD s\" ,\"ctime_sec\":" WRITE OUT-FD csec FD-N
         OUT-FD s\" ,\"ctime_ns\":" WRITE OUT-FD cns FD-N
      then
   then
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


\ Uses with a source path in the subject have a line in every form. Other
\ uses have a line only in the fourth form, for the requested tail; this includes an
\ unlocated binding in the subject, needed to compare shared source tokens.
: USE-WRITTEN? ( n bool -- bool )
   {: sym:n located:bool :}
   REF-BATCH? @ 0= if
      located VERIFY:SUBJECT? and if true exit then
   then
   OF-U @ 0= if false exit then
   sym VERIFY:SYM-IDENTITY drop {: tail:^ tailu:n :} 2drop
   REF-BATCH? @ if
      tail tailu OF-A @ OF-U @ STR=CI
   else
      tail tailu OF-A @ OF-U @ CORE-STR=
   then ;


: USE-LINE ( n n ^u8 n n n n n -- )
   {: at:n end:n path:^ pathu:n tat:n tend:n sym:n visit:n :}
   sym pathu 0<> USE-WRITTEN? 0= if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:^ pkgu:n tail:^ tailu:n vis:n :}
   pkg pkgu DEF-PKG!
   OUT-FD s\" check-verify: use {\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s\" ,\"in\":" WRITE
   VERIFY:FILE$ JSON-STR
   OUT-FD s\" ,\"file\":" WRITE
   path pathu JSON-STR
   OUT-FD s\" ,\"target_start\":" WRITE
   OUT-FD tat FD-N
   OUT-FD s\" ,\"target_end\":" WRITE
   OUT-FD tend FD-N
   OUT-FD s\" ,\"decl_visit\":" WRITE
   OUT-FD visit FD-N
   OUT-FD s\" ,\"decl_name\":" WRITE
   tail tailu JSON-STR
   OUT-FD s\" ,\"package\":" WRITE
   DEF-PKG$ JSON-STR
   OUT-FD s\" ,\"visibility\":" WRITE
   vis VISIBILITY$ JSON-STR
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


: LOAD-OUTCOME$ ( n -- ^u8 n )
   {: outcome:n :}
   outcome VERIFY:LOAD-READ = if s" read" exit then
   outcome VERIFY:LOAD-HELD = if s" held" exit then
   s" check-verify: unknown load outcome" 74 die ;


\ A top-level loader of the first form's subject whose load reached a file, as
\ its load line.
: LOAD-LINE ( n n ^u8 n n -- )
   {: at:n end:n path:^ pathu:n outcome:n :}
   OUT-FD s\" check-verify: load {\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s\" ,\"path\":" WRITE
   path pathu JSON-STR
   OUT-FD s\" ,\"outcome\":" WRITE
   outcome LOAD-OUTCOME$ JSON-STR
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


\ A spelling the third form's cursor offers, as its candidate line.
: CANDIDATE-LINE ( ^u8 n ^u8 n n n -- )
   {: w:^ wu:n path:^ pathu:n tat:n tend:n :}
   OUT-FD s\" check-verify: candidate {\"word\":" WRITE
   w wu JSON-STR
   pathu 0 > if
      OUT-FD s\" ,\"file\":" WRITE
      path pathu JSON-STR
      OUT-FD s\" ,\"target_start\":" WRITE
      OUT-FD tat FD-N
      OUT-FD s\" ,\"target_end\":" WRITE
      OUT-FD tend FD-N
   then
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


\ One multi-error window covers the complete load composition, and goes past
\ a duplicate as past any rejected definition. A throw that ends it goes on
\ after the window closes.
: VERIFY-ALL ( -- )
   SUBJECT-PATH$ REJECTION:FILE!
   ['] FILE-LINE is VERIFY:ON-FILE
   ['] DEFINITION-LINE is VERIFY:ON-DEFINITION
   ['] USE-LINE is VERIFY:ON-USE
   ['] LOAD-LINE is VERIFY:ON-LOADER
   ['] CANDIDATE-LINE is VERIFY:ON-CANDIDATE
   CHECK:MULTI-ERR-BEGIN
   [: VERIFY-CUR ;] catch {: rc:n :}
   CHECK:MULTI-ERR-END {: rejects:n :}
   rejects 0<> if -1 FAILED ! then
   rc 0<> if rc throw then ;


\ The engine provides the path, or this child loaded it: what `require` skips.
: HELD? ( ^u8 n -- bool )
   SOURCE-ROOT:RESOLVE nip nip ;


: RESULT ( ^u8 n -- ) {: a:^ u:n :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD a u WRITE
   OUT-FD NEWLINE ;

\ ANSWER, then after a space the file the composition could not load
\ (VERIFY:FAULT-TARGET$), when it names one.
: TARGET-RESULT ( ^u8 n -- ) {: a:^ u:n :}
   VERIFY:FAULT-TARGET$ {: t:^ tu:n :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD a u WRITE
   tu 0 > if
      OUT-FD s"  " WRITE
      OUT-FD t tu WRITE
   then
   OUT-FD NEWLINE ;

\ The answer to a throw RC that ended the composition after its rejection wrote
\ its record: `refused` for a rejection, `missing` when the file a loader names
\ is not there, `unread` for a file found but not readable, and `full` for a
\ full dictionary. Any other code goes on with no answer.
: STOP-RESULT ( n -- ) {: rc:n :}
   rc INCLUDE-IO-RC = if s" unread" TARGET-RESULT exit then
   rc REJECTION:DICT-FULL-RC = if s" full" RESULT exit then
   rc CHECKER-REJECT-RC = 0= if rc throw then
   VERIFY:FAULT-TARGET$ nip 0 > if s" missing" TARGET-RESULT exit then
   s" refused" RESULT ;

: INDEX-SUBJECT ( -- )
   0 IDX-N !
   true REJECTION:JSON !
   OUT-FD REJECTION:FD!
   ['] IDX-DEFINITION is VERIFY:ON-DEFINITION
   CHECK:MULTI-ERR-BEGIN
   [: INDEX-CUR ;] catch {: rc:n :}
   CHECK:MULTI-ERR-END {: rejects:n :}
   rc 0<> if rc STOP-RESULT exit then
   VERIFY:DEFERRED? if s" deferred" RESULT exit then
   rejects 0<> if s" refused" else s" verified" then RESULT ;


\ Run the verification in one neutral checker scope, every diagnostic the
\ checker renders on stdout, a JSON record unless `--prose` asked for a prose
\ one: the code it threw, 0 for none.
: SCOPED ( [ -- ] -- n ) {: q :}
   JSON? @ REJECTION:JSON !
   OUT-FD REJECTION:FD!
   [: q CHECK:SCOPED ;] catch ;


: VERIFY-CLOSURE ( -- )
   0 FAILED !
   true VERIFY:REPORT-DEFERRALS
   [: VERIFY-ALL ;] SCOPED {: rc:n :}
   rc 0<> if rc STOP-RESULT exit then
   FAILED @ 0<> if s" refused" RESULT exit then
   VERIFY:DEFERRED? if s" deferred" RESULT exit then
   s" verified" RESULT ;


: PREVERIFY-CUR-ACT ( -- )
   SUBJECT$ 0 SCRIPT-ARGV$ 2 SCRIPT-ARGV$
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: PREVERIFY-CUR ( -- )
   0 SCRIPT-ARGV$ SOURCE-ROOT:DIRNAME
   [: PREVERIFY-CUR-ACT ;] SOURCE-ROOT:WITH ;


\ This pre-pass leaves deferred source to the run; only VERIFY-CLOSURE warns.
: PREVERIFY ( -- )
   [: PREVERIFY-CUR ;] SCOPED {: rc:n :}
   rc 0<> if rc STOP-RESULT exit then
   s" verified" RESULT ;

public

: USAGE ( -- )
   s" usage: check-verify-child.f -- SUBJECT [--prose | --label LABEL [--prose] | --at N | --of TAIL | --index]" 64 die ;

\ N in the third form's `--at N`; -1 for none.
: CURSOR-ARG ( -- n )
   SCRIPT-ARGC 3 < if -1 exit then
   1 SCRIPT-ARGV$ s" --at" CORE-STR= 0= if -1 exit then
   2 SCRIPT-ARGV$ STR>NUMBER? MATCH option
      some OF ENDOF
      none OF -1 ENDOF
   ;MATCH ;

\ The fourth form's `--of TAIL`, TAIL kept for USE-WRITTEN?: whether it is
\ that form.
: OF-ARG ( -- bool )
   SCRIPT-ARGC 3 < if false exit then
   1 SCRIPT-ARGV$ s" --of" CORE-STR= 0= if false exit then
   2 SCRIPT-ARGV$ OF-U ! OF-A !
   true ;

\ Each frame owns its subject and its open-document snapshot. The zero path
\ length ends the stream. A new scope and fresh output deduplication follow.
: BATCH-NEXT ( -- bool )
   STR-SPACE COUNT-TO {: pu:n :}
   LF COUNT-TO {: su:n :}
   pu 0= if su 0<> POS @ IN-U @ <> or if TABLE-FAULT then false exit then
   POS @ pu + su + IN-U @ > if TABLE-FAULT then
   POS @ SUBJECT-PATH-AT !
   pu SUBJECT-PATH-U !
   POS @ pu + SUBJECT-AT !
   su SUBJECT-U !
   POS @ pu + su + POS !
   0 OPEN-N !
   begin
      POS @ IN-U @ >= if TABLE-FAULT then
      POS @ IN c@ LF <>
   while OPEN-ENTRY repeat
   POS @ 1+ POS !
   true ;

: MAIN ( -- )
   SCRIPT-ARGC 2 = if
      0 SCRIPT-ARGV$ s" --batch-ref" CORE-STR= dup REF-BATCH? !
      0 SCRIPT-ARGV$ s" --batch" CORE-STR= or if
         true BATCH? !
         1 SCRIPT-ARGV$ OF-U ! OF-A !
         READ-IN
         ['] OPEN-TEXT is VERIFY:SUPPLIED
         0 POS !
         0 BATCH-INDEX !
         begin BATCH-NEXT while
            VERIFY:SOURCE-RESET-ISOLATED-VISITS
            SUBJECT-PATH$ HELD? if s" held" RESULT else VERIFY-CLOSURE then
            OUT-FD s" check-verify: batch-end " WRITE
            OUT-FD BATCH-INDEX @ FD-N
            OUT-FD NEWLINE
            1 BATCH-INDEX +!
         repeat
         exit
      then
   then
   SCRIPT-ARGC 2 = if
      1 SCRIPT-ARGV$ s" --index" CORE-STR= if READ-STDIN INDEX-SUBJECT exit then
      1 SCRIPT-ARGV$ s" --prose" CORE-STR= if
         false JSON? !
         READ-STDIN
         0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
         VERIFY-CLOSURE exit
      then
   then
   SCRIPT-ARGC 3 = if
      1 SCRIPT-ARGV$ s" --label" CORE-STR= if READ-STDIN PREVERIFY exit then
   then
   SCRIPT-ARGC 4 = if
      1 SCRIPT-ARGV$ s" --label" CORE-STR=
      3 SCRIPT-ARGV$ s" --prose" CORE-STR= and if
         false JSON? !  READ-STDIN PREVERIFY exit
      then
   then
   CURSOR-ARG {: at:n :}
   at 0 >= if at VERIFY:CURSOR! then
   at 0 >= OF-ARG or if 3 else 1 then SCRIPT-ARGC <> if USAGE then
   READ-STDIN
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package
