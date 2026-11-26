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
require src/core/exit-status.f
require src/lib/string-option.f
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


: WRITE ( n str -- ) {: fd:n s:str :}
   s STR:LEN 0= if exit then
   fd s write s STR:LEN <> if s" check-verify: write failed" EXIT-STATUS:IOERR die then ;


: NEWLINE ( n -- ) {: fd:n :}
   LF NL c!
   fd NL 1 >str WRITE ;


: U$ ( n -- str )
   NUM-CAP NUM-I !
   begin
      dup 10 mod 48 + NUM-I @ 1- dup NUM-I ! NUM + c!
      10 /
   dup 0= until drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - >str ;


: FD-N ( n n -- ) {: fd:n n:n :}
   n 0 < if fd s" -" WRITE fd 0 n - U$ WRITE exit then
   fd n U$ WRITE ;


: READ-IN ( -- )
   0 IN-U !
   begin
      IN-U @ CHUNK + IN-RESERVE
      0 IN-U @ IN CHUNK SPAN:MAKE read RD !
      RD @ 0 < if
         s" check-verify: cannot read the open documents and the subject" EXIT-STATUS:IOERR die
      then
      RD @ 0 >
   while
      IN-U @ RD @ + IN-U !
   repeat ;


: BATCH-HELP ( -- )
   s" stdin is frames, each a line `PU " REJECTION:EMIT-STR
   s" SU` then PU bytes of a subject's " REJECTION:EMIT-STR
   s" path and SU bytes of its text, " REJECTION:EMIT-STR
   s" then its open documents (each a " REJECTION:EMIT-STR
   s" line `PU U` then PU bytes of PATH " REJECTION:EMIT-STR
   s" and U bytes of TEXT) and an empty " REJECTION:EMIT-STR
   s" line; a `0 0` line ends it" REJECTION:EMIT-STR ;

: OPEN-HELP ( -- )
   s" stdin is the open documents, each " REJECTION:EMIT-STR
   s" a line `PU U` then PU bytes of " REJECTION:EMIT-STR
   s" PATH and U bytes of TEXT, the " REJECTION:EMIT-STR
   s" counts in decimal; then an empty " REJECTION:EMIT-STR
   s" line; then BYTES, to the end" REJECTION:EMIT-STR ;


\ Places the record at the len bytes from IN's byte at. It goes to stderr:
\ stdout is the subject's records.
: FRAME-REJECT ( n n str -- ) {: at:n len:n want:str :}
   false REJECTION:JSON !
   2 REJECTION:FD!
   NULL$ REJECTION:FILE!
   0 IN IN-U @ >str 1 1 0 REJECTION:SOURCE!
   s" E-CHECK-FRAME" REJECTION:OPEN
   s" stdin does not follow the frame" REJECTION:EMIT-STR
   IN-U @ 0 > if
      at IN len >str REJECTION:AT
      s" expected " REJECTION:EMIT-STR  want REJECTION:EMIT-STR
   then
   REJECTION:HELP
   BATCH? @ if BATCH-HELP else OPEN-HELP then
   EXIT-STATUS:DATAERR REJECTION:STOP ;


: COUNT-WANT ( n -- str )
   LF = if s" a decimal count, then a newline" exit then
   s" a decimal count, then a space" ;


\ The count in IN from POS up to the byte C, POS moved past C. READ-IN keeps
\ room past IN-U, so POS at IN-U is an index of IN.
: COUNT-TO ( n -- n ) {: c:n :}
   POS @ IN IN-U @ POS @ - >str c STR:INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: k:n :}
   k 0 < if
      POS @  IN-U @ POS @ -
      c COUNT-WANT FRAME-REJECT
   then
   POS @ IN k >str STR:>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH {: count:n :}
   count 0 < if POS @ k s" a decimal count" FRAME-REJECT then
   POS @ k + 1+ POS !
   count ;


\ The open document whose line starts at POS, into OPEN-AT, POS moved past its
\ text.
: OPEN-ENTRY ( -- )
   POS @ {: line:n :}
   STR:SPACE COUNT-TO {: pu:n :}
   LF COUNT-TO {: tu:n :}
   IN-U @ POS @ - {: rest:n :}
   pu rest >  tu rest pu - >  or if
      line  POS @ line - 1-  s" PU + U bytes after this line" FRAME-REJECT
   then
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
      POS @ IN-U @ >= if
         IN-U @ 1-  1  s" an empty line before the subject's bytes" FRAME-REJECT
      then
      POS @ IN c@ LF <>
   while
      OPEN-ENTRY
   repeat
   POS @ 1+ SUBJECT-AT ! ;


: OPEN-PATH$ ( n -- str )
   4 * {: at:n :}
   at OPEN-AT @ IN at 1+ OPEN-AT @ >str ;

: OPEN-TEXT$ ( n -- str )
   4 * {: at:n :}
   at 2 + OPEN-AT @ IN at 3 + OPEN-AT @ >str ;


\ The text of the first open document at this canonical path and true, else
\ false (VERIFY:SUPPLIED).
: OPEN-TEXT ( str -- str bool ) {: s:str :}
   OPEN-N @ 0 ?do
      i OPEN-PATH$ s STR:= if i OPEN-TEXT$ true unloop exit then
   loop
   NULL$ false ;


: SUBJECT$ ( -- str )
   SUBJECT-AT @ IN SUBJECT-U @ >str ;

: SUBJECT-PATH$ ( -- str )
   BATCH? @ if SUBJECT-PATH-AT @ IN SUBJECT-PATH-U @ >str exit then
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


: CONTROL-ESC ( n -- str )
   {: c:n :}
   92 ESC c!  117 ESC 1 + c!  48 ESC 2 + c!  48 ESC 3 + c!
   c 16 / HEX-DIGIT ESC 4 + c!
   c 15 and HEX-DIGIT ESC 5 + c!
   ESC 6 >str ;


\ RFC 8259's string escapes: quotation mark, reverse solidus and control bytes;
\ empty for none.
: ESCAPE$ ( n -- str )
   {: c:n :}
   c 34 = if s\" \\\"" exit then
   c 92 = if s\" \\\\" exit then
   c 32 < if c CONTROL-ESC exit then
   NULL$ ;


: JSON-BYTE ( ^u8 n -- )
   {: a:^ i:n :}
   a i + c@ ESCAPE$
   {: e:str :}
   e STR:LEN 0= if exit then
   OUT-FD a RUN-AT @ + i RUN-AT @ - >str WRITE
   OUT-FD e WRITE
   i 1+ RUN-AT ! ;


: JSON-STR ( str -- )
   {: s:str :}
   OUT-FD s\" \"" WRITE
   0 RUN-AT !
   s STR:LEN 0 ?do s STR:DATA i JSON-BYTE loop
   OUT-FD s STR:DATA RUN-AT @ + s STR:LEN RUN-AT @ - >str WRITE
   OUT-FD s\" \"" WRITE ;


\ The package span, copied out of the checker's symbol pool before anything
\ else runs: the next intern can move it. The storage holds a byte at least,
\ so a global's empty package has an address.
: DEF-PKG! ( str -- )
   {: pkg:str :}
   pkg STR:LEN 1 max DEF-PKG-RESERVE
   pkg STR:DATA 0 DEF-PKG pkg STR:LEN BYTE-COPY
   pkg STR:LEN DEF-PKG-U ! ;


: DEF-PKG$ ( -- str )
   0 DEF-PKG DEF-PKG-U @ >str ;


: VISIBILITY$ ( n -- str )
   {: vis:n :}
   vis SYM-GLOBAL = if s" global" exit then
   vis SYM-PRIVATE = if s" private" exit then
   vis SYM-PUBLIC = if s" public" exit then
   s" check-verify: unknown visibility" EXIT-STATUS:INTERNAL die ;


: CLASS$ ( n -- str )
   {: class:n :}
   class VERIFY:DEF-WORD = if s" word" exit then
   class VERIFY:DEF-CONSTANT = if s" constant" exit then
   class VERIFY:DEF-STORAGE = if s" storage" exit then
   class VERIFY:DEF-EXPORT = if s" export" exit then
   class VERIFY:DEF-CONSTRUCTOR = if s" constructor" exit then
   s" check-verify: unknown definition class" EXIT-STATUS:INTERNAL die ;


\ A definition the first form retained, as its definition line: the name as
\ the event gives it, the package and visibility the checker recorded it
\ under. The tail it recorded is its lookup key, not a name to show.
: DEFINITION-LINE ( str str n str n n n n -- )
   {: kind:str name:str sym:n eff:str at:n end:n class:n visit:n :}
   REF-BATCH? @ if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:str tail:str vis:n :}
   pkg DEF-PKG!
   OUT-FD s\" check-verify: definition {\"kind\":" WRITE
   kind JSON-STR
   OUT-FD s\" ,\"class\":" WRITE
   class CLASS$ JSON-STR
   OUT-FD s\" ,\"word\":" WRITE
   name JSON-STR
   OUT-FD s\" ,\"decl_visit\":" WRITE
   OUT-FD visit FD-N
   OUT-FD s\" ,\"decl_name\":" WRITE
   tail JSON-STR
   OUT-FD s\" ,\"package\":" WRITE
   DEF-PKG$ JSON-STR
   OUT-FD s\" ,\"visibility\":" WRITE
   vis VISIBILITY$ JSON-STR
   eff STR:LEN 0<> if
      OUT-FD s\" ,\"effect\":" WRITE
      eff JSON-STR
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
: IDX-DEFINITION ( str str n str n n n n -- )
   {: kind:str name:str sym:n eff:str at:n end:n class:n visit:n :}
   IDX-N @ 1+ 3 * IDX-EVENTS-RESERVE
   IDX-N @ 3 * {: row:n :}
   sym row IDX-EVENTS !
   at row 1+ IDX-EVENTS !
   end row 2 + IDX-EVENTS !
   1 IDX-N +! ;

: IDX-LINE ( n n n -- )
   {: sym:n at:n end:n :}
   sym VERIFY:SYM-IDENTITY {: pkg:str tail:str vis:n :}
   tail STR:LEN 0= if exit then
   pkg DEF-PKG!
   OUT-FD s\" check-verify: index {\"decl_name\":" WRITE
   tail JSON-STR
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
: FILE-LINE ( str str -- )
   {: f:str s:str :}
   f SUBJECT-PATH$ STR:= {: subject:bool :}
   f OPEN-TEXT {: open:bool :} drop
   f VERIFY:QUIET-READ-META drop
   {: size:n msec:n mns:n csec:n cns:n :}
   OUT-FD s\" check-verify: file {\"file\":" WRITE
   f JSON-STR
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
   sym VERIFY:SYM-IDENTITY drop {: tail:str :} drop
   REF-BATCH? @ if
      tail OF-A @ OF-U @ >str STR:=CI
   else
      tail OF-A @ OF-U @ >str STR:=
   then ;


: USE-LINE ( n n str n n n n -- )
   {: at:n end:n path:str tat:n tend:n sym:n visit:n :}
   sym path STR:LEN 0<> USE-WRITTEN? 0= if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:str tail:str vis:n :}
   pkg DEF-PKG!
   OUT-FD s\" check-verify: use {\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s\" ,\"in\":" WRITE
   VERIFY:FILE$ JSON-STR
   OUT-FD s\" ,\"file\":" WRITE
   path JSON-STR
   OUT-FD s\" ,\"target_start\":" WRITE
   OUT-FD tat FD-N
   OUT-FD s\" ,\"target_end\":" WRITE
   OUT-FD tend FD-N
   OUT-FD s\" ,\"decl_visit\":" WRITE
   OUT-FD visit FD-N
   OUT-FD s\" ,\"decl_name\":" WRITE
   tail JSON-STR
   OUT-FD s\" ,\"package\":" WRITE
   DEF-PKG$ JSON-STR
   OUT-FD s\" ,\"visibility\":" WRITE
   vis VISIBILITY$ JSON-STR
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


: LOAD-OUTCOME$ ( n -- str )
   {: outcome:n :}
   outcome VERIFY:LOAD-READ = if s" read" exit then
   outcome VERIFY:LOAD-HELD = if s" held" exit then
   s" check-verify: unknown load outcome" EXIT-STATUS:INTERNAL die ;


\ A top-level loader of the first form's subject whose load reached a file, as
\ its load line.
: LOAD-LINE ( n n str n -- )
   {: at:n end:n path:str outcome:n :}
   OUT-FD s\" check-verify: load {\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s\" ,\"path\":" WRITE
   path JSON-STR
   OUT-FD s\" ,\"outcome\":" WRITE
   outcome LOAD-OUTCOME$ JSON-STR
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


\ A spelling the third form's cursor offers, as its candidate line.
: CANDIDATE-LINE ( str str n n -- )
   {: w:str path:str tat:n tend:n :}
   OUT-FD s\" check-verify: candidate {\"word\":" WRITE
   w JSON-STR
   path STR:LEN 0 > if
      OUT-FD s\" ,\"file\":" WRITE
      path JSON-STR
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
: HELD? ( str -- bool )
   SOURCE-ROOT:RESOLVE nip ;


: RESULT ( str -- ) {: s:str :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD s WRITE
   OUT-FD NEWLINE ;

\ ANSWER, then after a space the file the composition could not load
\ (VERIFY:FAULT-TARGET$), when it names one.
: TARGET-RESULT ( str -- ) {: s:str :}
   VERIFY:FAULT-TARGET$ {: t:str :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD s WRITE
   t STR:LEN 0 > if
      OUT-FD s"  " WRITE
      OUT-FD t WRITE
   then
   OUT-FD NEWLINE ;

\ The answer to a throw RC that ended the composition after its rejection wrote
\ its record: `rejected` for a rejection, `missing` when the file a loader names
\ is not there, `unread` for a file found but not readable, and `full` for a
\ full dictionary. Any other code goes on with no answer.
: STOP-RESULT ( n -- ) {: rc:n :}
   rc EXIT-STATUS:IOERR = if s" unread" TARGET-RESULT exit then
   rc EXIT-STATUS:FULL = if s" full" RESULT exit then
   rc EXIT-STATUS:REJECTED = 0= if rc throw then
   VERIFY:FAULT-TARGET$ STR:LEN 0 > if s" missing" TARGET-RESULT exit then
   s" rejected" RESULT ;

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
   rejects 0<> if s" rejected" else s" verified" then RESULT ;


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
   FAILED @ 0<> if s" rejected" RESULT exit then
   VERIFY:STOPPED? if s" stopped" RESULT exit then
   VERIFY:DEFERRED? if s" deferred" RESULT exit then
   s" verified" RESULT ;


: PREVERIFY-LABELED ( -- )
   SUBJECT$ 0 SCRIPT-ARGV$ 2 SCRIPT-ARGV$
   VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: PREVERIFY-ENTRY ( -- )
   SUBJECT$ 0 SCRIPT-ARGV$ VERIFY:SOURCE-COMPOSE-ENTRY-IN-SCOPE ;

: PREVERIFY-CUR ( [ -- ] -- ) {: q :}
   0 SCRIPT-ARGV$ SOURCE-ROOT:DIRNAME q SOURCE-ROOT:WITH ;


\ This pre-pass leaves deferred source to the run; only VERIFY-CLOSURE warns.
\ Q composes the subject.
: PREVERIFY ( [ -- ] -- ) {: q :}
   [: q PREVERIFY-CUR ;] SCOPED {: rc:n :}
   rc 0<> if rc STOP-RESULT exit then
   s" verified" RESULT ;

public

: USAGE ( -- )
   s" usage: check-verify-child.f -- SUBJECT [--prose | --label LABEL [--prose] | --entry [--prose] | --at N | --of TAIL | --index]"
   EXIT-STATUS:USAGE die ;

\ N in the third form's `--at N`; -1 for none.
: CURSOR-ARG ( -- n )
   SCRIPT-ARGC 3 < if -1 exit then
   1 SCRIPT-ARGV$ s" --at" STR:= 0= if -1 exit then
   2 SCRIPT-ARGV$ STR:>NUMBER? MATCH option
      some OF ENDOF
      none OF -1 ENDOF
   ;MATCH ;

\ The fourth form's `--of TAIL`, TAIL kept for USE-WRITTEN?: whether it is
\ that form.
: OF-ARG ( -- bool )
   SCRIPT-ARGC 3 < if false exit then
   1 SCRIPT-ARGV$ s" --of" STR:= 0= if false exit then
   2 SCRIPT-ARGV$ dup STR:LEN OF-U ! STR:DATA OF-A !
   true ;

\ Each frame owns its subject and its open-document snapshot. The zero path
\ length ends the stream. A new scope and fresh output deduplication follow.
: BATCH-NEXT ( -- bool )
   POS @ {: line:n :}
   STR:SPACE COUNT-TO {: pu:n :}
   LF COUNT-TO {: su:n :}
   pu 0= if
      su 0<> if
         line  POS @ line - 1-
         s" a `0 0` line, or a path length above 0" FRAME-REJECT
      then
      POS @ IN-U @ <> if
         line  POS @ line - 1-
         s" nothing after the `0 0` line" FRAME-REJECT
      then
      false exit
   then
   IN-U @ POS @ - {: rest:n :}
   pu rest >  su rest pu - >  or if
      line  POS @ line - 1-  s" PU + SU bytes after this line" FRAME-REJECT
   then
   POS @ SUBJECT-PATH-AT !
   pu SUBJECT-PATH-U !
   POS @ pu + SUBJECT-AT !
   su SUBJECT-U !
   POS @ pu + su + POS !
   0 OPEN-N !
   begin
      POS @ IN-U @ >= if
         IN-U @ 1-  1
         s" an empty line after the frame's open documents" FRAME-REJECT
      then
      POS @ IN c@ LF <>
   while OPEN-ENTRY repeat
   POS @ 1+ POS !
   true ;

: MAIN ( -- )
   SCRIPT-ARGC 2 = if
      0 SCRIPT-ARGV$ s" --batch-ref" STR:= dup REF-BATCH? !
      0 SCRIPT-ARGV$ s" --batch" STR:= or if
         true BATCH? !
         1 SCRIPT-ARGV$ dup STR:LEN OF-U ! STR:DATA OF-A !

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
      1 SCRIPT-ARGV$ s" --index" STR:= if READ-STDIN INDEX-SUBJECT exit then
      1 SCRIPT-ARGV$ s" --entry" STR:= if READ-STDIN ['] PREVERIFY-ENTRY PREVERIFY exit then
      1 SCRIPT-ARGV$ s" --prose" STR:= if
         false JSON? !
         READ-STDIN
         0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
         VERIFY-CLOSURE exit
      then
   then
   SCRIPT-ARGC 3 = if
      1 SCRIPT-ARGV$ s" --label" STR:= if READ-STDIN ['] PREVERIFY-LABELED PREVERIFY exit then
      1 SCRIPT-ARGV$ s" --entry" STR:=
      2 SCRIPT-ARGV$ s" --prose" STR:= and if
         false JSON? !  READ-STDIN ['] PREVERIFY-ENTRY PREVERIFY exit
      then
   then
   SCRIPT-ARGC 4 = if
      1 SCRIPT-ARGV$ s" --label" STR:=
      3 SCRIPT-ARGV$ s" --prose" STR:= and if
         false JSON? !  READ-STDIN ['] PREVERIFY-LABELED PREVERIFY exit
      then
   then
   CURSOR-ARG {: at:n :}
   at 0 >= if at VERIFY:CURSOR! then
   at 0 >= OF-ARG or if 3 else 1 then SCRIPT-ARGC <> if USAGE then
   READ-STDIN
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package
