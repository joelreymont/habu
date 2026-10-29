\ check-verify-child-lib.f - the verifier child's body. tools/check-verify-child.f
\ states the contract, loads this file from its own tree and runs
\ CHECK-VERIFY-CHILD:MAIN.

\ The observation must precede every verifier require: those files may select a
\ tier or replace a checker in this child, while the hypothetical subject has
\ not reached any of them at its own load boundary.
tick-order@
package CHECK-VERIFY-CHILD
variable ENTRY-ORDER
PTR-VARIABLE ENTRY-OWNER
ENTRY-OWNER !
ENTRY-ORDER !
;package

\ tools/check-verify-child.f loads this file under its own tree's root, so the
\ verifier comes from that tree, not the client's.
require src/habu/verify-source.f

\ Engine words with no checked effect; tools/check-core.f declares them alike.
s" CHECKER-SCOPE-START-NEUTRAL" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK-VERIFY-CHILD

ENTRY-ORDER @ ENTRY-OWNER @ VERIFY:ENTRY-TICK-ORDER!

$10000 constant CHUNK                   \ bytes asked of one read
32 constant NUM-CAP
10 constant LF
1 constant OUT-FD
2 constant ERR-FD

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
variable REF-BATCH?                     \ only the selected-tail packets are needed
variable BATCH-INDEX
variable RD
variable FAILED
variable STOP                           \ the code a throw ended the composition with, 0 for none
variable NUM-I
create NUM NUM-CAP allot
create NL 1 allot
create ESC 6 allot                      \ one \u00XX escape
variable RUN-AT                         \ where the bytes not yet written start
DYNAMIC-BUFFER DEF-PKG u8
variable DEF-PKG-U
DYNAMIC-BUFFER IDX-EVENTS n              \ symbol, start, end for each declaration
variable IDX-N
TYPED-VARIABLE OF-A ptr u8              \ the fourth form's TAIL,
variable OF-U                           \ 0 for the other forms


: WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= if exit then
   fd a u write u <> if s" check-verify: write failed" 74 die then ;


: NEWLINE ( n -- ) {: fd:n :}
   LF NL c!
   fd NL 1 WRITE ;


: U$ ( n -- ptr u8 n )
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


: OPEN-PATH$ ( n -- ptr u8 n )
   4 * {: at:n :}
   at OPEN-AT @ IN at 1+ OPEN-AT @ ;

: OPEN-TEXT$ ( n -- ptr u8 n )
   4 * {: at:n :}
   at 2 + OPEN-AT @ IN at 3 + OPEN-AT @ ;


\ The text of the first open document at this canonical path and true, else
\ false (VERIFY:SUPPLIED).
: OPEN-TEXT ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}
   OPEN-N @ 0 ?do
      i OPEN-PATH$ a u CORE-STR= if i OPEN-TEXT$ true unloop exit then
   loop
   NULL$ false ;


: SUBJECT$ ( -- ptr u8 n )
   SUBJECT-AT @ IN SUBJECT-U @ ;

: SUBJECT-PATH$ ( -- ptr u8 n )
   BATCH? @ if SUBJECT-PATH-AT @ IN SUBJECT-PATH-U @ exit then
   0 SCRIPT-ARGV$ ;


\ stdin read, and a load of an open document's file given its text.
: READ-STDIN ( -- )
   READ-IN
   READ-OPEN
   IN-U @ SUBJECT-AT @ - SUBJECT-U !
   ['] OPEN-TEXT is VERIFY:SUPPLIED ;


\ The definitions after the throw went unverified, and a throw the checker
\ rendered no packet for has nothing else to show it.
: STOPPED ( ptr u8 n n n -- ) {: label:ptr labelu:n rc:n rejects:n :}
   ERR-FD label labelu WRITE
   ERR-FD s" : verification stopped by throw " WRITE
   ERR-FD rc FD-N
   ERR-FD s"  after " WRITE
   ERR-FD rejects FD-N
   ERR-FD s"  rejected definitions" WRITE
   ERR-FD NEWLINE ;


\ A loader's fault, for the parent to place its packet and status line: the
\ loader token's length and the file the loader could not read, if any.
: LOADER-LINE ( -- )
   VERIFY:FAULT-TARGET$ {: t:ptr tu:n :}
   OUT-FD s" check-verify: loader " WRITE
   OUT-FD VERIFY:FAULT-LEN@ FD-N
   tu 0 > if
      OUT-FD s"  " WRITE
      OUT-FD t tu WRITE
   then
   OUT-FD NEWLINE ;


: VERIFY-CUR-ACT ( -- )
   SUBJECT$ SUBJECT-PATH$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: VERIFY-CUR ( -- )
   SUBJECT-PATH$ SOURCE-ROOT:DIRNAME
   [: VERIFY-CUR-ACT ;] SOURCE-ROOT:WITH ;


: STOP-FIELDS ( n n ptr u8 n bool n -- )
   {: at:n u:n file:ptr fileu:n subj:bool rc:n :}
   OUT-FD rc FD-N
   OUT-FD s"  " WRITE
   OUT-FD VERIFY:TOKEN-BYTE@ FD-N
   OUT-FD s"  " WRITE
   OUT-FD at FD-N
   OUT-FD s"  " WRITE
   OUT-FD u FD-N
   OUT-FD subj if s"  1 " else s"  0 " then WRITE
   OUT-FD file fileu WRITE
   OUT-FD NEWLINE ;


: STOP-RESULT ( n -- )
   {: rc:n :}
   OUT-FD s" check-verify: stopped " WRITE
   VERIFY:DUPLICATE VERIFY:SOURCE-COMPOSE-STOPPED$
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? rc STOP-FIELDS ;


\ A duplicate the first form goes past, which the checker writes no packet for.
: DUPLICATE-LINE ( n n ptr u8 n bool -- )
   OUT-FD s" check-verify: duplicate " WRITE
   E-DUP-DEFINITION STOP-FIELDS ;


: HEX-DIGIT ( n -- n )
   dup 10 < if 48 + exit then 87 + ;


: CONTROL-ESC ( n -- ptr u8 n )
   {: c:n :}
   92 ESC c!  117 ESC 1 + c!  48 ESC 2 + c!  48 ESC 3 + c!
   c 16 / HEX-DIGIT ESC 4 + c!
   c 15 and HEX-DIGIT ESC 5 + c!
   ESC 6 ;


\ RFC 8259's string escapes: quotation mark, reverse solidus and control bytes;
\ empty for none.
: ESCAPE$ ( n -- ptr u8 n )
   {: c:n :}
   c 34 = if s\" \\\"" exit then
   c 92 = if s\" \\\\" exit then
   c 32 < if c CONTROL-ESC exit then
   NULL$ ;


: JSON-BYTE ( ptr u8 n -- )
   {: a:ptr i:n :}
   a i + c@ ESCAPE$
   {: e:ptr eu:n :}
   eu 0= if exit then
   OUT-FD a RUN-AT @ + i RUN-AT @ - WRITE
   OUT-FD e eu WRITE
   i 1+ RUN-AT ! ;


: JSON-STR ( ptr u8 n -- )
   {: a:ptr u:n :}
   OUT-FD s\" \"" WRITE
   0 RUN-AT !
   u 0 ?do a i JSON-BYTE loop
   OUT-FD a RUN-AT @ + u RUN-AT @ - WRITE
   OUT-FD s\" \"" WRITE ;


\ The package span, copied out of the checker's symbol pool before anything
\ else runs: the next intern can move it. The storage holds a byte at least,
\ so a global's empty package has an address.
: DEF-PKG! ( ptr u8 n -- )
   {: pkg:ptr pkgu:n :}
   pkgu 1 max DEF-PKG-RESERVE
   pkg 0 DEF-PKG pkgu BYTE-COPY
   pkgu DEF-PKG-U ! ;


: DEF-PKG$ ( -- ptr u8 n )
   0 DEF-PKG DEF-PKG-U @ ;


: VISIBILITY$ ( n -- ptr u8 n )
   {: vis:n :}
   vis SYM-GLOBAL = if s" global" exit then
   vis SYM-PRIVATE = if s" private" exit then
   vis SYM-PUBLIC = if s" public" exit then
   s" check-verify: unknown visibility" 74 die ;


: CLASS$ ( n -- ptr u8 n )
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
: DEFINITION-LINE ( ptr u8 n ptr u8 n n ptr u8 n n n n n -- )
   {: kind:ptr kindu:n name:ptr nameu:n sym:n eff:ptr effu:n at:n end:n class:n visit:n :}
   REF-BATCH? @ if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
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
: IDX-DEFINITION ( ptr u8 n ptr u8 n n ptr u8 n n n n n -- )
   {: kind:ptr kindu:n name:ptr nameu:n sym:n eff:ptr effu:n at:n end:n class:n visit:n :}
   IDX-N @ 1+ 3 * IDX-EVENTS-RESERVE
   IDX-N @ 3 * {: row:n :}
   sym row IDX-EVENTS !
   at row 1+ IDX-EVENTS !
   end row 2 + IDX-EVENTS !
   1 IDX-N +! ;

: IDX-DUPLICATE ( n n ptr u8 n bool -- ) drop 2drop 2drop ;

: IDX-LINE ( n n n -- )
   {: sym:n at:n end:n :}
   sym VERIFY:SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
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
: FILE-LINE ( ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n a:ptr u:n :}
   f fu SUBJECT-PATH$ CORE-STR= {: subject:bool :}
   f fu OPEN-TEXT {: open:bool :} 2drop
   f fu VERIFY:QUIET-READ-META
   {: size:n msec:n mns:n csec:n cns:n valid:bool :}
   \ Refuse unstamped disk bytes before publishing any part of their record.
   subject open or 0= valid 0= and if f fu VERIFY:REFUSE-SOURCE-READ then
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
   sym VERIFY:SYM-IDENTITY drop {: tail:ptr tailu:n :} 2drop
   REF-BATCH? @ if
      tail tailu OF-A @ OF-U @ STR=CI
   else
      tail tailu OF-A @ OF-U @ CORE-STR=
   then ;


: USE-LINE ( n n ptr u8 n n n n n -- )
   {: at:n end:n path:ptr pathu:n tat:n tend:n sym:n visit:n :}
   sym pathu 0<> USE-WRITTEN? 0= if exit then
   sym VERIFY:SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
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


: LOAD-OUTCOME$ ( n -- ptr u8 n )
   {: outcome:n :}
   outcome VERIFY:LOAD-READ = if s" read" exit then
   outcome VERIFY:LOAD-HELD = if s" held" exit then
   s" check-verify: unknown load outcome" 74 die ;


\ A top-level loader of the first form's subject whose load reached a file, as
\ its load line.
: LOAD-LINE ( n n ptr u8 n n -- )
   {: at:n end:n path:ptr pathu:n outcome:n :}
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
: CANDIDATE-LINE ( ptr u8 n ptr u8 n n n -- )
   {: w:ptr wu:n path:ptr pathu:n tat:n tend:n :}
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
\ a duplicate as past any refused definition. A duplicate it cannot go past,
\ a name the definer generates, stops it with its stopped line. A loader's fault
\ stops it with its loader line, from which the parent writes its packet and
\ its status line.
: VERIFY-ALL ( -- )
   SUBJECT-PATH$ DIAG-FILE!
   1 1 0 DIAG-ORIGIN!
   ['] DUPLICATE-LINE is VERIFY:ON-DUPLICATE
   ['] FILE-LINE is VERIFY:ON-FILE
   ['] DEFINITION-LINE is VERIFY:ON-DEFINITION
   ['] USE-LINE is VERIFY:ON-USE
   ['] LOAD-LINE is VERIFY:ON-LOADER
   ['] CANDIDATE-LINE is VERIFY:ON-CANDIDATE
   MULTI-ERR-BEGIN
   [: VERIFY-CUR ;] catch {: rc:n :}
   MULTI-ERR-END {: rejects:n :}
   rejects 0<> if -1 FAILED ! then
   rc STOP !
   rc 0= if exit then
   rc VERIFY:LOADER-FAULT? if LOADER-LINE exit then
   rc E-DUP-DEFINITION = if
      VERIFY:DUPLICATE VERIFY:SOURCE-COMPOSE-STOPPED$
      VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? DUPLICATE-LINE
   then
   VERIFY:SOURCE-COMPOSE-STOPPED$ rc rejects STOPPED ;


\ The engine provides the path, or this child loaded it: what `require` skips.
: HELD? ( ptr u8 n -- bool )
   SOURCE-ROOT:RESOLVE nip nip ;


: RESULT ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD a u WRITE
   OUT-FD NEWLINE ;

: INDEX-SUBJECT ( -- )
   0 IDX-N !
   true DIAG-JSON!
   OUT-FD DIAG-FD!
   ['] IDX-DEFINITION is VERIFY:ON-DEFINITION
   ['] IDX-DUPLICATE is VERIFY:ON-DUPLICATE
   MULTI-ERR-BEGIN
   [: INDEX-CUR ;] catch {: rc:n :}
   MULTI-ERR-END {: rejects:n :}
   rc 0<> if
      ERR-FD s" check-verify: stopped by throw " WRITE ERR-FD rc FD-N ERR-FD NEWLINE
      rc throw
   then
   VERIFY:DEFERRED? if s" deferred" RESULT exit then
   rejects 0<> if s" refused" else s" verified" then RESULT ;


\ Run the verification in one neutral checker scope, every diagnostic the
\ checker renders a packet on stdout: the code it threw, 0 for none.
: SCOPED ( [ -- ] -- n ) {: q :}
   0 0= DIAG-JSON!
   OUT-FD DIAG-FD!
   CHECKER-SCOPE-START-NEUTRAL
   q catch
   CHECKER-SCOPE-DONE ;


: VERIFY-CLOSURE ( -- )
   0 FAILED !
   0 STOP !
   VERIFY:REPORT-DEFERRALS
   [: VERIFY-ALL ;] SCOPED {: rc:n :}
   rc 0<> if
      ERR-FD s" check-verify: stopped by throw " WRITE ERR-FD rc FD-N ERR-FD NEWLINE
      rc throw
   then
   STOP @ 0<> if STOP @ STOP-RESULT exit then
   FAILED @ 0<> if s" refused" RESULT exit then
   VERIFY:DEFERRED? if s" deferred" RESULT exit then
   s" verified" RESULT ;


: PREVERIFY-CUR-ACT ( -- )
   SUBJECT$ 0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: PREVERIFY-CUR ( -- )
   0 SCRIPT-ARGV$ SOURCE-ROOT:DIRNAME
   [: PREVERIFY-CUR-ACT ;] SOURCE-ROOT:WITH ;


70 constant REFUSED-RC                  \ a refused definition's stop (verify-source.f BODY-VERDICT)

\ A definition or top-level token refused by the throw that rendered its packet
\ (checker.f CHECKER-DEF-STOPPED?) stops the pre-pass as a definition its verdict refuses
\ does; any other stop keeps its code.
: STOP-CODE ( n -- n )
   dup CHECKER-DEF-STOPPED? if drop REFUSED-RC then ;

\ This pre-pass leaves deferred source to the run; only VERIFY-CLOSURE warns.
: PREVERIFY ( -- )
   [: PREVERIFY-CUR ;] SCOPED {: rc:n :}
   rc 0<> if rc STOP-CODE STOP-RESULT exit then
   s" verified" RESULT ;

public

: USAGE ( -- )
   s" usage: check-verify-child.f -- SUBJECT [LABEL | --at N | --of TAIL | --index]" 64 die ;

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
      READ-STDIN PREVERIFY exit
   then
   CURSOR-ARG {: at:n :}
   at 0 >= if at VERIFY:CURSOR! then
   at 0 >= OF-ARG or if 3 else 1 then SCRIPT-ARGC <> if USAGE then
   READ-STDIN
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package
