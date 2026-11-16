\ check-all-errors-core.f - the --all-errors checker core: checks sources as a
\ load runs them, in one checker scope and one multi-error session, and reports
\ every rejected definition and lexer defect, one record each through package
\ REJECTION, in the mode REJECTION:JSON holds, to standard error. A run's
\ records are captured by REJECTION, which grows its capture to hold them, and
\ handed to the report when the run ends.
\
\ A definition of a name its scope already held is rejected at the name, and
\ the scan goes on past it as past any rejected definition.
\
\ A rejection stops its definition or statement with CHECKER-REJECT-RC after its
\ record and the scan goes on at the next; any other code is thrown on. A
\ lexer defect, which runs to the end of its file, or a loader form discovery
\ rejects ends the scan where it lies, after every record before it.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/source.f
require src/lib/process.f
require src/lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f

\ VERIFY opens its own package, which a file required inside a package cannot,
\ so verify-source.f is required here at top level; every checker scope below
\ replays source through VERIFY.
\ A replayed `constant` gets VERIFY's `-- a` effect.
require src/habu/verify-source.f

package CHECK-ALL-ERRORS

private

TYPED-VARIABLE CA-SRC-A ^u8
variable CA-SRC-U
variable CA-SRC-CAP

TYPED-VARIABLE CA-FILE-A ^u8
variable CA-FILE-U
TYPED-VARIABLE CA-COMPOSE-PATH-A ^u8
variable CA-COMPOSE-PATH-U
TYPED-VARIABLE CA-COMPOSE-LABEL-A ^u8
variable CA-COMPOSE-LABEL-U
TYPED-VARIABLE CA-COMPOSE-ENTRY bool    \ the composed bytes are entry text

: CA-SRC-A@ ( -- ^u8 )
   CA-SRC-A @ ;

: CA-SRC-A! ( ^u8 -- )
   CA-SRC-A ! ;

: CA-FILE-A@ ( -- ^u8 )
   CA-FILE-A @ ;

: CA-FILE-A! ( ^u8 -- )
   CA-FILE-A ! ;

: CA-ERR ( ^u8 n -- ) {: a:^ u:n :}
   u 0= IF exit THEN
   FD:STDERR FD>N a u write u <> IF E-FS-IO throw THEN ;

\ Runs the action with the records it writes captured, then hands them to the
\ report; the capture ends on the throwing path too.
: CA-RECORDS ( [ -- ] -- ) {: q :}
   REJECTION:CAPTURE
   q catch {: rc:n :}
   REJECTION:BUFFER-OFF
   REJECTION:BUFFER$ CA-ERR
   rc 0 <> IF rc throw THEN ;

\ ---- the bytes of the file a record names ----------------------------------

\ Room for n bytes of the source being read, in a region of the read's own: a
\ larger region takes the bytes the last one holds, and the last is released
\ when this read made it.
: CA-SRC-ROOM ( n -- ^u8 ) {: need:n :}
   need CA-SRC-CAP @ > if
      CA-SRC-A@ CA-SRC-CAP @ {: old:^ oldcap:n :}
      need MEM-ALLOC-64K-SPAN CA-SRC-CAP ! {: fresh:^ :}
      old fresh oldcap BYTE-COPY
      oldcap 0 > if old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES then
      fresh CA-SRC-A!
   then
   CA-SRC-A@ ;

\ Each read takes a region of its own, so the bytes an earlier read or the
\ caller left (CA-SOURCE-BUF!) stay where they are.
: CA-READ-SOURCE ( ^u8 n -- ) {: path:^ pu:n :}
   0 CA-SRC-CAP !
   path pu  path pu FS:FILE-SIZE  [: CA-SRC-ROOM ;] SOURCE:READ-WHOLE-SAMPLED CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ^u8 n -- ) {: a:^ u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

: CA-START ( ^u8 n -- ) {: labela:^ labelu:n :}
   labelu CA-FILE-U !
   labela CA-FILE-A! ;

: CA-WORD-END ( n -- n )                \ the byte past the source word at byte n
   begin dup CA-SRC-U @ < if CA-SRC-A@ over + c@ 32 > else false then while
      1+
   repeat ;

\ ---- records through REJECTION ----------------------------------------------

\ A record of this core places its token in the bytes read, as the file the
\ label names.
: CA-ARM ( -- )
   CA-FILE-A@ CA-FILE-U @ REJECTION:FILE!
   CA-SRC-A@ CA-SRC-U @ 1 1 0 REJECTION:SOURCE! ;

\ The token a lexer defect sits at, read out of the source at its own width: a
\ string's opener, the token a row's rule rejects, or empty at the source's end.
: CA-LEX-TOKEN$ ( -- ^u8 n )
   LINT-LEX:ERROR-BYTE@ {: at:n :}
   CA-SRC-A@ at +  at CA-WORD-END at - ;

: CA-LEX-ROW? ( -- bool )
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = ;

\ A string defect gets the load's record, a malformed row its own code and help:
\ a caller told to close a string would look for a quote that is not there.
: CA-LEX-RECORD ( -- )
   CA-ARM
   CA-LEX-ROW? IF
      CA-LEX-TOKEN$ LINT-LEX:ROW-LINE@ LINT-LEX:ROW-COL@ VERIFY:ROW-REJECT
   ELSE CA-LEX-TOKEN$ INTERPRETER:LITERAL-REJECT THEN
   REJECTION:KEEP
   REJECTION:SOURCE-OFF ;

\ The record of the bytes read when the lexer finds a defect in them; true when
\ it wrote one.
: CA-LEX ( -- bool )
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= IF false exit THEN
   CA-LEX-RECORD true ;

\ ---- a run ------------------------------------------------------------------

variable CA-COMPOSING                   \ the run is a composition

\ What the scan's end leaves to record: a clean scan's lexer defect.
: CA-STOPPED ( n -- ) {: rc:n :}
   rc 0= IF CA-LEX drop THEN ;

\ The status the run fails with: its stop's, else a rejection's when the run
\ counted one, else none.
: CA-STATUS ( n n -- n ) {: rc:n rejects:n :}
   rc 0 <> IF rc exit THEN
   rejects 0 > IF CHECKER-REJECT-RC exit THEN
   0 ;

: CA-CHECK-SOURCE ( -- )
   CA-SRC-A@ CA-SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-CHECK-COMPOSE-VERIFY ( -- )
   CA-SRC-A@ CA-SRC-U @ CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @
   CA-COMPOSE-ENTRY @ if VERIFY:SOURCE-COMPOSE-ENTRY-IN-SCOPE exit then
   CA-COMPOSE-LABEL-A @ CA-COMPOSE-LABEL-U @
   VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: CA-CHECK-COMPOSE ( -- )
   CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @ SOURCE-ROOT:DIRNAME
   [: CA-CHECK-COMPOSE-VERIFY ;] SOURCE-ROOT:WITH ;

: CA-CHECK ( -- )
   CA-COMPOSING @ IF CA-CHECK-COMPOSE ELSE CA-CHECK-SOURCE THEN ;

\ The scan's records, captured from the first to the last; a stop other than a
\ rejection's is thrown on.
: CA-RUN-CAPTURED ( -- n )
   CA-FILE-A@ CA-FILE-U @ REJECTION:FILE!
   [: CA-CHECK ;] catch {: rc:n :}
   rc 0 <> rc CHECKER-REJECT-RC <> and IF rc throw THEN
   rc CA-STOPPED
   rc ;

variable CA-RC                          \ how the last run's check ended

: CA-RUN-BODY ( bool -- ) {: report:bool :}
   report VERIFY:REPORT-DEFERRALS
   REJECTION:TALLY @ {: before:n :}
   [: CA-RUN-CAPTURED CA-RC ! ;] CA-RECORDS
   CA-RC @  REJECTION:TALLY @ before -  CA-STATUS {: status:n :}
   status 0 <> IF status throw THEN ;

: CA-WARNED ( [ -- ] -- ) {: q :}
   CHECK:WARN-DIAGS @ {: warn:n :}
   -1 CHECK:WARN-DIAGS !   \ the session's warnings, whatever the caller's switch
   q catch {: rc:n :}
   warn CHECK:WARN-DIAGS !
   rc 0 <> IF rc throw THEN ;

public

\ Check what the given word checks in one checker scope and one multi-error
\ session, as a load runs it: a definition sees the clean definitions before
\ it and the declared signature of every definition rejected before it. The
\ checked sources are files, not a continuation of whatever package the caller
\ has open, so the scope opens at neutral top level, and closing it restores
\ the caller's package on the clean and the throwing path alike.
: SESSION ( [ -- ] -- ) {: q :}
   CHECK:MULTI-ERR-BEGIN
   [: [: q CA-WARNED ;] CHECK:SCOPED ;] catch {: rc:n :}
   CHECK:MULTI-ERR-END drop
   rc 0 <> IF rc throw THEN ;

private

: CA-RUN ( bool bool -- ) {: compose:bool report:bool :}
   compose CA-COMPOSING !
   [: report CA-RUN-BODY ;] SESSION ;

: CA-COMPOSE-AT ( ^u8 n bool -- ) {: path:^ pathu:n entry:bool :}
   entry CA-COMPOSE-ENTRY !
   path CA-COMPOSE-PATH-A !  pathu CA-COMPOSE-PATH-U !
   CA-FILE-A@ CA-COMPOSE-LABEL-A !  CA-FILE-U @ CA-COMPOSE-LABEL-U ! ;

public

\ Check the source file at the given path under the given label as a load runs
\ it, every file a top-level loader statement loads included (COMPOSE-BUF). No
\ run of the program follows, so the check reports what it leaves to one.
: FILE ( ^u8 n ^u8 n -- ) {: labela:^ labelu:n patha:^ pathu:n :}
   [: labela labelu CA-START
      patha pathu CA-READ-SOURCE
      patha pathu false CA-COMPOSE-AT
      true true CA-RUN ;] REJECTION:PRESERVE-LABEL ;

\ Check the given source bytes as the file at the given path, reporting them
\ under the given label: the composition verifies every file a top-level
\ loader statement loads where it stands, under its own path, in one session,
\ and rejects a loader form discovery rejects, or a file it cannot read, where
\ it meets it.
: COMPOSE-BUF ( ^u8 n ^u8 n ^u8 n -- )
   {: src:^ srcu:n path:^ pathu:n label:^ labelu:n :}
   [: label labelu CA-START
      src srcu CA-SOURCE-BUF!
      path pathu false CA-COMPOSE-AT
      true false CA-RUN ;] REJECTION:PRESERVE-LABEL ;

\ COMPOSE-BUF of entry text, whose records name REJECTION:NO-FILE$.
: COMPOSE-ENTRY ( ^u8 n ^u8 n -- )
   {: src:^ srcu:n path:^ pathu:n :}
   [: REJECTION:NO-FILE$ CA-START
      src srcu CA-SOURCE-BUF!
      path pathu true CA-COMPOSE-AT
      true false CA-RUN ;] REJECTION:PRESERVE-LABEL ;

\ Check the given source bytes under the given label as one unit: a loader
\ statement in them loads nothing, and a top-level token is not resolved.
: BUF ( ^u8 n ^u8 n -- ) {: labela:^ labelu:n srca:^ srcu:n :}
   [: labela labelu CA-START
      srca srcu CA-SOURCE-BUF!
      false true CA-RUN ;] REJECTION:PRESERVE-LABEL ;

;package
