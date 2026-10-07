\ check-verify-child.f - the verifier child CHECK:VERIFY-BYTES,
\ CHECK:VERIFY-BYTES-AT and CHECK:PREVERIFY-BYTES run.
\
\ tools/check-verify-core.f spawns it on the engine lib/engine-candidate.f
\ names, by its absolute path in the tree that file was loaded from, in the
\ caller's working directory; nothing else runs it:
\
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT [--lenient PATH...] < BYTES
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT LABEL < BYTES
\    ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT --at N [--lenient PATH...] < BYTES
\
\ BYTES is the subject's text and SUBJECT the canonical absolute path it is
\ checked as. The image is the engine's boot prefix, the verifier and
\ this file, so no tool's word stands in for, or collides with, a word of the
\ subject. Nothing of SUBJECT or its closure runs: the verifier scans source and
\ records what it declares.
\
\ The verifier follows top-level loader acts in one neutral checker scope, with
\ each file's path and positions while it is open. `require` skips held paths;
\ `include` verifies every occurrence. The first and third forms compose
\ quietly (VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE), so no walk of the closure
\ runs before them: the composition reads each file a loader loads and refuses
\ the loader forms tools/source-discovery.f refuses where it meets them, but in
\ a file --lenient names, each PATH canonical and absolute (the entries of
\ tools/dynamic-tail-manifest.f).
\
\ stdout is the schema-1 JSON packets, one per line in verification order, each
\ written as the checker makes it, so a child that dies has passed on every
\ packet made before; then one result line. The first form verifies with all
\ errors, going past a duplicate definition as past any refused definition,
\ and writes for each duplicate, which the checker writes no packet for, a
\ diagnostic line among the packets, for each file it reads a file line, once
\ however often it reads the file and before anything in it, for each
\ definition it retains a definition line, for each use in the subject that
\ the checker binds to a located declaration a use line, and for each
\ top-level loader of the subject whose load reached a file a load line, one
\ JSON object each. It answers
\
\    check-verify: verified | refused | deferred | held
\                | stopped RC BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
\    check-verify: duplicate 78 BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
\    check-verify: file {"file":F}
\    check-verify: definition {"kind":K,"class":C,"word":W,"package":P,
\       "visibility":V,"effect":E,"file":F,"byte_start":S,"byte_end":N}
\    check-verify: use {"byte_start":S,"byte_end":N,"file":F,
\       "target_start":TS,"target_end":TN}
\    check-verify: load {"byte_start":S,"byte_end":N,"path":P,"outcome":O}
\    check-verify: candidate {"word":W,"file":F,"target_start":TS,
\       "target_end":TN}
\    check-verify: loader LEN TARGET
\
\ A loader line comes right before the stopped line of a stop at a loader's
\ fault (VERIFY:LOADER-FAULT?): LEN is the length of the loader token at the
\ stopped line's BYTE, 0 for a string or locals group a file never closes, and
\ TARGET, the rest of the line and absent when empty, the file the loader could
\ not read, as it was resolved.
\
\ A file line names a file src/habu/verify-source.f ON-FILE reports, F as
\ its packets name it. A definition line names what ON-DEFINITION reports:
\ K the statement token that declared it, C its class as that token's path
\ knows it (word, constant, storage, or export: a re-export, whatever the word
\ it names is), W its name as the source writes it
\ (a name its definer generates, as spelled), P and V the package (empty for a
\ global) and visibility (global, private or public) the checker recorded it
\ under, E its declared effect, absent when it declared none, F the file as
\ its packets name it, and S and N where the token that declared it starts and
\ ends there.
\
\ A use line names what src/habu/verify-source.f ON-USE reports, a use in the
\ subject that the checker bound to a located declaration: S and N where the
\ use starts and ends in the subject, F the path the declaration's file was
\ resolved to, and TS and TN where the token that declared it starts and ends
\ there.
\
\ A load line names what src/habu/verify-source.f ON-LOADER reports, a
\ top-level loader of the subject whose load reached a file, in the subject's
\ order and before anything in that file: S and N where its operand starts and
\ ends in the subject (the token after include or require, a string loader's
\ literal between its quotes as written), P the canonical path, and O read,
\ the file's bytes were acquired for this load, or held, a require of a path
\ the engine provides or that was recorded before, which read nothing.
\
\ The third form is the first with a completion cursor at byte N of BYTES. It
\ writes as well, for each spelling the checker offers at the cursor's place, a
\ candidate line naming what src/habu/verify-source.f ON-CANDIDATE reports: W
\ the spelling, F the path the file of the declaration it binds was resolved
\ to, and TS and TN where the token that declared it starts and ends there; F,
\ TS and TN are absent for a declaration in no file the composition read, or
\ with no location. A cursor the scan passes in a comment, a string or a
\ signature, or never reaches, has none.
\
\ deferred: nothing is refused, but a stretch of top-level source or a
\ definition was deferred to the run, and a W-CHECK-DEFERRED packet locates each
\ (src/habu/verify-source.f TOP-TOKEN, REPORT-DEFERRED): those tokens are not
\ verified.
\ held is answered before anything is verified: this image holds SUBJECT though
\ the engine does not provide it (the verifier's source and this file), so it
\ cannot be verified here. The parent answers engine-provided itself. stopped
\ is a throw that ended the composition, whatever was refused before it.
\
\ The second form is check.f's pre-pass. The first definition the checker
\ refuses stops the composition, as it stops the load, and the subject's packets
\ name it LABEL. SUBJECT is check.f's own copy of the text, which nothing holds.
\ It answers
\
\    check-verify: verified | stopped RC BYTE DUP-AT DUP-LEN IN-SUBJECT FILE
\
\ RC is the code the composition stopped with, 70 for a definition or top-level
\ token the checker refused by the throw that rendered its packet, BYTE where
\ the token the verifier read last starts, DUP-AT and DUP-LEN where the name it
\ refused as a duplicate starts and its length, DUP-LEN 0 when it kept no name
\ (VERIFY:DUPLICATE), IN-SUBJECT 1 when the stop is in BYTES and 0 when it is
\ in a file they load, and FILE, the rest of the line, the name of the file it
\ is in: for BYTES, SUBJECT in the first form and LABEL in the second.
\
\ stderr is prose: for the first form, a line for each file whose verification
\ a throw other than a loader's fault stopped, and whatever else the engine
\ writes there, a `die`'s message among it. A child that ends any other way, the verifier's own `die` included,
\ writes no result line.

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

DYNAMIC-BUFFER SUBJECT u8               \ the subject's bytes, from stdin
variable SUBJECT-U
variable RD
variable FAILED
variable STOP                           \ the code a throw ended the composition with, 0 for none
variable NUM-I
create NUM NUM-CAP allot
create NL 1 allot
create ESC 6 allot                      \ one \u00XX escape
variable RUN-AT                         \ where the bytes not yet written start
DYNAMIC-BUFFER DEF-PKG u8               \ a definition's package
variable DEF-PKG-U
DYNAMIC-BUFFER SEEN u8                  \ the files file lines named, end to end,
variable SEEN-U
DYNAMIC-BUFFER SEEN-AT n                \ each one's offset there and length
variable SEEN-N
variable LENIENT-FIRST                  \ the argument the --lenient paths start at, SCRIPT-ARGC for none


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


: READ-SUBJECT ( -- )
   0 SUBJECT-U !
   begin
      SUBJECT-U @ CHUNK + SUBJECT-RESERVE
      0 SUBJECT-U @ SUBJECT CHUNK read RD !
      RD @ 0 < if s" check-verify: cannot read the subject" 74 die then
      RD @ 0 >
   while
      SUBJECT-U @ RD @ + SUBJECT-U !
   repeat ;


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


: VERIFY-CUR ( -- )
   0 SUBJECT SUBJECT-U @ 0 SCRIPT-ARGV$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;


\ Fields shared by a duplicate diagnostic and a terminal stop.
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


\ The digit, 0 to 15, as a lowercase hexadecimal byte.
: HEX-DIGIT ( n -- n )
   dup 10 < if 48 + exit then 87 + ;


\ A control byte's escape, \u00 and its two hexadecimal digits.
: CONTROL-ESC ( n -- ptr u8 n )
   {: c:n :}
   92 ESC c!  117 ESC 1 + c!  48 ESC 2 + c!  48 ESC 3 + c!
   c 16 / HEX-DIGIT ESC 4 + c!
   c 15 and HEX-DIGIT ESC 5 + c!
   ESC 6 ;


\ The escape RFC 8259 requires in a string for the byte, empty for none: a
\ quotation mark, a reverse solidus and a control character.
: ESCAPE$ ( n -- ptr u8 n )
   {: c:n :}
   c 34 = if s\" \\\"" exit then
   c 92 = if s\" \\\\" exit then
   c 32 < if c CONTROL-ESC exit then
   NULL$ ;


\ Byte I of the bytes at A: when it needs an escape, the bytes before it not
\ yet written, then its escape.
: JSON-BYTE ( ptr u8 n -- )
   {: a:ptr i:n :}
   a i + c@ ESCAPE$
   {: e:ptr eu:n :}
   eu 0= if exit then
   OUT-FD a RUN-AT @ + i RUN-AT @ - WRITE
   OUT-FD e eu WRITE
   i 1+ RUN-AT ! ;


\ The bytes as a JSON string, written in runs between the escapes.
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
   s" check-verify: unknown definition class" 74 die ;


\ A definition the first form retained, as its definition line: the name as
\ the event gives it, the package and visibility the checker recorded it
\ under. The tail it recorded is its lookup key, not a name to show.
: DEFINITION-LINE ( ptr u8 n ptr u8 n n ptr u8 n n n n -- )
   {: kind:ptr kindu:n name:ptr nameu:n sym:n eff:ptr effu:n at:n end:n class:n :}
   sym VERIFY:SYM-IDENTITY
   {: vis:n :}
   2drop DEF-PKG!
   OUT-FD s\" check-verify: definition {\"kind\":" WRITE
   kind kindu JSON-STR
   OUT-FD s\" ,\"class\":" WRITE
   class CLASS$ JSON-STR
   OUT-FD s\" ,\"word\":" WRITE
   name nameu JSON-STR
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


\ The file the Kth file line named.
: SEEN$ ( n -- ptr u8 n )
   {: k:n :}
   k 2 * SEEN-AT @ SEEN  k 2 * 1+ SEEN-AT @ ;


\ Whether a file line named the file.
: SEEN? ( ptr u8 n -- bool )
   {: f:ptr fu:n :}
   SEEN-N @ 0 ?do
      i SEEN$ f fu CORE-STR= if unloop true exit then
   loop
   false ;


\ The file kept as one a file line named. A path is never empty, so its
\ offset lies inside SEEN.
: SEEN+ ( ptr u8 n -- )
   {: f:ptr fu:n :}
   SEEN-U @ {: at:n :}
   SEEN-N @ {: k:n :}
   at fu + SEEN-RESERVE
   f at SEEN fu BYTE-COPY
   k 1+ 2 * SEEN-AT-RESERVE
   at k 2 * SEEN-AT !
   fu k 2 * 1+ SEEN-AT !
   at fu + SEEN-U !
   1 SEEN-N +! ;


\ A file the first form starts reading, as its file line the first time.
: FILE-LINE ( ptr u8 n -- )
   {: f:ptr fu:n :}
   f fu SEEN? if exit then
   f fu SEEN+
   OUT-FD s\" check-verify: file {\"file\":" WRITE
   f fu JSON-STR
   OUT-FD s" }" WRITE
   OUT-FD NEWLINE ;


\ A use the first form's subject bound to a located declaration, as its use
\ line.
: USE-LINE ( n n ptr u8 n n n -- )
   {: at:n end:n path:ptr pathu:n tat:n tend:n :}
   OUT-FD s\" check-verify: use {\"byte_start\":" WRITE
   OUT-FD at FD-N
   OUT-FD s\" ,\"byte_end\":" WRITE
   OUT-FD end FD-N
   OUT-FD s\" ,\"file\":" WRITE
   path pathu JSON-STR
   OUT-FD s\" ,\"target_start\":" WRITE
   OUT-FD tat FD-N
   OUT-FD s\" ,\"target_end\":" WRITE
   OUT-FD tend FD-N
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


\ Whether the file at PATH, canonical and absolute, is one --lenient names
\ (VERIFY:LENIENT-FILE?).
: LENIENT-ARG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC LENIENT-FIRST @ ?do
      a u i SCRIPT-ARGV$ CORE-STR= if true unloop exit then
   loop
   false ;


\ One multi-error window covers the complete load composition, and goes past
\ a duplicate as past any refused definition. A duplicate it cannot go past,
\ a name the definer generates, stops it with its stopped line. A loader's fault
\ stops it with its loader line, from which the parent writes its packet and
\ its status line.
: VERIFY-ALL ( -- )
   0 SCRIPT-ARGV$ DIAG-FILE!
   1 1 0 DIAG-ORIGIN!
   ['] DUPLICATE-LINE is VERIFY:ON-DUPLICATE
   ['] FILE-LINE is VERIFY:ON-FILE
   ['] DEFINITION-LINE is VERIFY:ON-DEFINITION
   ['] USE-LINE is VERIFY:ON-USE
   ['] LOAD-LINE is VERIFY:ON-LOADER
   ['] CANDIDATE-LINE is VERIFY:ON-CANDIDATE
   ['] LENIENT-ARG? is VERIFY:LENIENT-FILE?
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


: PREVERIFY-CUR ( -- )
   0 SUBJECT SUBJECT-U @ 0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;


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
   s" usage: check-verify-child.f -- SUBJECT [LABEL | [--at N] [--lenient PATH...]]" 64 die ;

\ N in the third form's `--at N`; -1 for none.
: CURSOR-ARG ( -- n )
   SCRIPT-ARGC 3 < if -1 exit then
   1 SCRIPT-ARGV$ s" --at" CORE-STR= 0= if -1 exit then
   2 SCRIPT-ARGV$ STR>NUMBER? MATCH option
      some OF ENDOF
      none OF -1 ENDOF
   ;MATCH ;

\ The arguments from the K-th on, after SUBJECT and a cursor: none, or
\ `--lenient` and the paths LENIENT-ARG? answers for.
: LENIENT-ARGS ( n -- ) {: k:n :}
   k SCRIPT-ARGC >= if SCRIPT-ARGC LENIENT-FIRST ! exit then
   k SCRIPT-ARGV$ s" --lenient" CORE-STR= 0= if USAGE then
   k 1+ LENIENT-FIRST ! ;

: MAIN ( -- )
   SCRIPT-ARGC 2 = if READ-SUBJECT PREVERIFY exit then
   CURSOR-ARG {: at:n :}
   at 0 >= if at VERIFY:CURSOR! 3 LENIENT-ARGS else 1 LENIENT-ARGS then
   READ-SUBJECT
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package

CHECK-VERIFY-CHILD:MAIN
