\ check-all-errors-core.f - the --all-errors checker core: checks sources as a
\ load runs them, in one checker scope and one multi-error session, and reports
\ every rejected definition, statement throw and lexer defect, one record each
\ through package REJECTION, in the mode REJECTION:JSON holds. The report
\ buffer is the caller's and never grows (BUFFERS!); a streamed report has no
\ bound (STREAM!). A run's records are captured by REJECTION, which grows its
\ capture to hold them, and handed to the report when the run ends.
\
\ A definition of a name its scope already held is rejected at the name, and
\ the scan goes on past it as past any rejected definition.
\
\ The checker reports a definition it rejects and returns, but a statement can
\ also throw out of it. A status in 1..255 follows the record its rejection
\ printed, as a `;using` with no `using` open throws 70 after
\ E-USING-UNBALANCED, and fails the source with that status (THREW?). Any other
\ code was thrown with nothing reported: its record is written at the token the
\ checker read last, after whatever it reported before it, and the source fails
\ as rejected. The rest of the source is not checked. A source the scan read to
\ its end is lexed then, so a lexer defect, which runs to the end of its file,
\ is reported after every record before it.

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
\ replays source through VERIFY. It is not in the engine's baked prefix: the
\ require registry's dedupe of co-loaded sites is the only one this line has.
\ A replayed `constant` gets VERIFY's `-- a` effect.
require src/habu/verify-source.f

package CHECK-ALL-ERRORS

private

variable CA-OUT-LEN
TYPED-VARIABLE CA-OUT-A ptr u8
variable CA-OUT-CAP
variable CA-OUT-FD                      \ where the report goes, or -1 for the buffer
-1 CA-OUT-FD !
TYPED-VARIABLE CA-SRC-A ptr u8
variable CA-SRC-U
variable CA-SRC-CAP
variable CA-THROW-RC                    \ what a statement threw while it was checked
variable CA-THROW-AT                    \ and the byte of the token the checker read last

TYPED-VARIABLE CA-FILE-A ptr u8
variable CA-FILE-U
TYPED-VARIABLE CA-COMPOSE-PATH-A ptr u8
variable CA-COMPOSE-PATH-U
TYPED-VARIABLE CA-COMPOSE-LABEL-A ptr u8
variable CA-COMPOSE-LABEL-U

: CA-SRC-A@ ( -- ptr u8 )
   CA-SRC-A @ ;

: CA-SRC-A! ( ptr u8 -- )
   CA-SRC-A ! ;

: CA-FILE-A@ ( -- ptr u8 )
   CA-FILE-A @ ;

: CA-FILE-A! ( ptr u8 -- )
   CA-FILE-A ! ;

: CA-OUT-A@ ( -- ptr u8 )
   CA-OUT-A @ ;

: CA-OUT-A! ( ptr u8 -- )
   CA-OUT-A ! ;

\ The report buffer is the caller's and cannot grow, so a run's records that do
\ not fit are refused whole by a throw, after every run's before them. A
\ streamed report has no buffer to outgrow.
: CA-OUT-ROOM ( n -- )
   CA-OUT-FD @ 0 >= IF drop exit THEN
   CA-OUT-LEN @ + CA-OUT-CAP @ > IF E-DIAG-CAPACITY throw THEN ;

: CA-ERR ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF exit THEN
   CA-OUT-FD @ 0 >= IF
      CA-OUT-FD @ a u write u <> IF E-FS-IO throw THEN
      exit
   THEN
   u CA-OUT-ROOM
   a CA-OUT-A@ CA-OUT-LEN @ + u BYTE-COPY
   CA-OUT-LEN @ u + CA-OUT-LEN ! ;

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
: CA-SRC-ROOM ( n -- ptr u8 ) {: need:n :}
   need CA-SRC-CAP @ > if
      CA-SRC-A@ CA-SRC-CAP @ {: old:ptr oldcap:n :}
      need MEM-ALLOC-64K-SPAN CA-SRC-CAP ! {: fresh:ptr :}
      old fresh oldcap BYTE-COPY
      oldcap 0 > if old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES then
      fresh CA-SRC-A!
   then
   CA-SRC-A@ ;

\ Each read takes a region of its own, so the bytes an earlier read or the
\ caller left (CA-SOURCE-BUF!) stay where they are. The file is read to its end
\ however it grows while it is read: its size, whose refusal (E-FS-STAT) stays
\ a missing or irregular file's, is only the first room.
: CA-READ-SOURCE ( ptr u8 n -- ) {: path:ptr pu:n :}
   0 CA-SRC-CAP !
   path pu  path pu FS:FILE-SIZE  [: CA-SRC-ROOM ;] SOURCE:READ-WHOLE-SAMPLED CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

: CA-START ( ptr u8 n -- ) {: labela:ptr labelu:n :}
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

: CA-SAY ( ptr u8 n -- )
   REJECTION:EMIT-STR ;

: CA-TICKS ( ptr u8 n -- )
   96 REJECTION:EMIT1  CA-SAY  96 REJECTION:EMIT1 ;

\ Each lexer defect sits at an opener word, a string opener or a row opener, so
\ the record's token is that word read out of the source, at its own width.
: CA-LEX-TOKEN$ ( -- ptr u8 n )
   LINT-LEX:ERROR-BYTE@ {: at:n :}
   CA-SRC-A@ at +  at CA-WORD-END at - ;

: CA-LEX-ROW? ( -- bool )
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = ;

\ An incomplete primitive-axiom row stops the scan like an open string but gets
\ its own code and help: a caller told to close a string literal would look for
\ a quote that is not there.
: CA-LEX-HELP ( -- )
   s" close the row: `PRIM:` ends at `PRIM;`, `EPRIM:` at `EPRIM;`, " CA-SAY
   s" `PPRIM:` at `PPRIM;` or `CLOSE-PRIVATE`, `EPPRIM:` at `ECLOSE-PRIVATE`" CA-SAY ;

: CA-LEX-ROW-RECORD ( -- )
   s" E-MALFORMED-REGISTRY-ROW" REJECTION:OPEN
   s" the primitive-axiom row opened at " CA-SAY  CA-LEX-TOKEN$ CA-TICKS
   s"  does not close" CA-SAY
   CA-LEX-TOKEN$ REJECTION:AT  s" opened here" CA-SAY
   REJECTION:HELP  CA-LEX-HELP ;

\ A string defect gets the load's record.
: CA-LEX-RECORD ( -- )
   CA-ARM
   CA-LEX-ROW? IF CA-LEX-ROW-RECORD
   ELSE CA-LEX-TOKEN$ INTERPRETER:LITERAL-REJECT THEN
   REJECTION:KEEP
   REJECTION:SOURCE-OFF ;

\ The record of the bytes read when the lexer finds a defect in them; true when
\ it wrote one.
: CA-LEX ( -- bool )
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= IF false exit THEN
   CA-LEX-RECORD true ;

: CA-THROW-TOKEN$ ( -- ptr u8 n )
   CA-THROW-AT @ {: at:n :}
   CA-SRC-A@ at +  at CA-WORD-END at - ;

\ A statement's throw: the code it threw, and why the scan rejected the
\ declaration, when it knows.
: CA-THROW-RECORD ( -- )
   CA-ARM
   s" E-STATEMENT-THROW" REJECTION:OPEN
   s" the statement threw " CA-SAY  CA-THROW-RC @ REJECTION:EMIT-N
   CA-THROW-TOKEN$ REJECTION:AT  s" the check stopped here" CA-SAY
   VERIFY:REASON$ {: why:ptr whyu:n :}
   whyu 0<> IF s" reason" REJECTION:NOTE  why whyu CA-SAY THEN
   REJECTION:HELP
   s" the statement threw the code shown: catch it, or fix what threw it" CA-SAY
   REJECTION:KEEP
   REJECTION:SOURCE-OFF ;

public

\ True for the status of a check a statement threw out of unreported: a code
\ outside 0..255. A code in 1..255 is a status a rejection threw after printing
\ its record, so it gets no throw record.
: THREW? ( n -- bool ) {: rc:n :}
   rc 0 < rc 255 > or ;

private

\ ---- a run ------------------------------------------------------------------

variable CA-COMPOSING                   \ the run is a composition

\ A composition stops in one of its files: the subject, by its label, or any
\ other, which is read, by the name its records give it.
: CA-COMPOSE-STOPPED ( -- )
   VERIFY:SOURCE-COMPOSE-STOPPED$ {: a:ptr u:n :}
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? 0= IF a u CA-READ-SOURCE THEN
   u CA-FILE-U !  a CA-FILE-A! ;

\ The record of a statement's throw, at the token where the scan stopped.
: CA-THREW ( n -- ) {: rc:n :}
   rc CA-THROW-RC !
   VERIFY:TOKEN-BYTE@ CA-THROW-AT !
   CA-COMPOSING @ IF CA-COMPOSE-STOPPED THEN
   CA-THROW-RECORD ;

\ What the scan's end leaves to record: a clean scan's lexer defect, or an
\ unreported throw's record. A status in 1..255 has its record out.
: CA-STOPPED ( n -- ) {: rc:n :}
   rc 0= IF CA-LEX drop exit THEN
   rc THREW? IF rc CA-THREW THEN ;

\ The status the run fails with: a stop's in 1..255, else a rejection's when
\ the run counted one, else none.
: CA-STATUS ( n n -- n ) {: rc:n rejects:n :}
   rc THREW? 0= rc 0 <> and IF rc exit THEN
   rejects 0 > IF CHECKER-REJECT-RC exit THEN
   0 ;

: CA-CHECK-SOURCE ( -- )
   CA-SRC-A@ CA-SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-CHECK-COMPOSE-VERIFY ( -- )
   CA-SRC-A@ CA-SRC-U @ CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @
   CA-COMPOSE-LABEL-A @ CA-COMPOSE-LABEL-U @
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: CA-CHECK-COMPOSE ( -- )
   CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @ SOURCE-ROOT:DIRNAME
   [: CA-CHECK-COMPOSE-VERIFY ;] SOURCE-ROOT:WITH ;

: CA-CHECK ( -- )
   CA-COMPOSING @ IF CA-CHECK-COMPOSE ELSE CA-CHECK-SOURCE THEN ;

\ The scan's records and the stop's, captured from the first to the last.
: CA-RUN-CAPTURED ( -- n )
   CA-FILE-A@ CA-FILE-U @ REJECTION:FILE!
   1 1 0 REJECTION:ORIGIN!
   [: CA-CHECK ;] catch {: rc:n :}
   rc CA-STOPPED
   rc ;

variable CA-RC                          \ how the last run's check ended
variable CA-LEXED                       \ the last lex wrote a record

: CA-RUN-BODY ( bool -- ) {: report:bool :}
   report VERIFY:REPORT-DEFERRALS
   REJECTION:TALLY @ {: before:n :}
   [: CA-RUN-CAPTURED CA-RC ! ;] CA-RECORDS
   CA-RC @  REJECTION:TALLY @ before -  CA-STATUS {: status:n :}
   status 0 <> IF status throw THEN ;

public

\ The caller's report buffer, which this core appends to and OUT$ hands back.
\ Also clears the recorded report length.
: BUFFERS! ( ptr u8 n -- ) {: outa:ptr outcap:n :}
   outcap CA-OUT-CAP !
   outa CA-OUT-A!
   0 CA-OUT-LEN !
   -1 CA-OUT-FD ! ;

\ The report the last run accumulated in the caller's first buffer.
: OUT$ ( -- ptr u8 n )
   CA-OUT-A@ CA-OUT-LEN @ ;

\ BUFFERS! for a report written to the given file descriptor as it is made, so
\ it holds as many records, each as long, as the checked sources make. OUT$ is
\ then empty.
: STREAM! ( fd -- ) {: fd:fd :}
   s" " BUFFERS!
   fd FD>N CA-OUT-FD ! ;

\ Check what the given word checks in one checker scope and one multi-error
\ session, as a load runs it: a definition sees the clean definitions before
\ it and the declared signature of every definition rejected before it. The
\ checked sources are files, not a continuation of whatever package the caller
\ has open, so the scope opens at neutral top level, and closing it restores
\ the caller's package on the clean and the throwing path alike.
: SESSION ( [ -- ] -- ) {: q :}
   MULTI-ERR-BEGIN
   CHECKER-SCOPE-START-NEUTRAL
   WARN-DIAGS @ {: warn:n :}
   -1 WARN-DIAGS !   \ the session's warnings, whatever the caller's switch
   q catch {: rc:n :}
   warn WARN-DIAGS !
   CHECKER-SCOPE-DONE
   MULTI-ERR-END drop
   rc 0 <> IF rc throw THEN ;

private

: CA-RUN ( bool bool -- ) {: compose:bool report:bool :}
   compose CA-COMPOSING !
   [: report CA-RUN-BODY ;] SESSION ;

: CA-COMPOSE-AT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path CA-COMPOSE-PATH-A !  pathu CA-COMPOSE-PATH-U !
   CA-FILE-A@ CA-COMPOSE-LABEL-A !  CA-FILE-U @ CA-COMPOSE-LABEL-U ! ;

public

\ Check the source file at the given path under the given label as a load runs
\ it, every file a top-level loader statement loads included (COMPOSE-BUF). No
\ run of the program follows, so the check reports what it leaves to one.
: FILE ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n patha:ptr pathu:n :}
   [: labela labelu CA-START
      patha pathu CA-READ-SOURCE
      patha pathu CA-COMPOSE-AT
      true true CA-RUN ;] REJECTION:PRESERVE-LABEL ;

\ Report the lexer defect of the source file at the given path under the given
\ label, by its record, and throw the rejection status; return when the file
\ lexes clean. Nothing is checked, so a caller that cannot check the file
\ still reports the defect where it stands.
: LEX-FILE ( ptr u8 n ptr u8 n -- )
   {: labela:ptr labelu:n patha:ptr pathu:n :}
   [: labela labelu CA-START
      patha pathu CA-READ-SOURCE
      [: CA-LEX CA-LEXED ! ;] CA-RECORDS ;] REJECTION:PRESERVE-LABEL
   CA-LEXED @ IF CHECKER-REJECT-RC throw THEN ;

\ Check the given source bytes as the file at the given path, reporting them
\ under the given label: the composition verifies every file a top-level
\ loader statement loads where it stands, under its own path, in one session.
: COMPOSE-BUF ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n :}
   [: label labelu CA-START
      src srcu CA-SOURCE-BUF!
      path pathu CA-COMPOSE-AT
      true false CA-RUN ;] REJECTION:PRESERVE-LABEL ;

\ Check the given source bytes under the given label as one unit: a loader
\ statement in them loads nothing, and a top-level token is not resolved.
: BUF ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n srca:ptr srcu:n :}
   [: labela labelu CA-START
      srca srcu CA-SOURCE-BUF!
      false true CA-RUN ;] REJECTION:PRESERVE-LABEL ;

;package
