\ check-all-errors-core.f - the --all-errors checker core: checks sources as a
\ load runs them, in one checker scope and one multi-error session, and reports
\ every rejected definition, statement throw and lexer defect, one record each
\ through package REJECTION, in the mode REJECTION:JSON holds. The report
\ buffer is the caller's and never grows (BUFFERS!); a streamed report has no
\ bound (STREAM!). A run's records are captured by REJECTION, which grows its
\ capture to hold them, and handed to the report when the run ends.
\
\ A definition of a name its scope already held is rejected at the name, and
\ the scan goes on past it as past any rejected definition (CA-ON-DUP).
\
\ The checker reports a definition it rejects and returns, but a statement can
\ also throw out of it. A status in 1..255 follows the record its rejection
\ printed, as a `;using` with no `using` open throws 93 after
\ E-USING-UNBALANCED, and fails the source with that status (THREW?). Any other
\ code was thrown with nothing reported: its record is written at the token the
\ checker read last, after whatever it reported before it, or the lexer's
\ record when the lexer reads a defect there (LEX-FIRST?), and the source fails
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
require src/lib/byte-buffer.f
require src/lib/json.f
require src/lib/source-lex.f
require src/lib/verify-diagnostics.f

\ VERIFY opens its own package, which a file required inside a package cannot,
\ so verify-source.f is required here at top level; every checker scope below
\ replays source through VERIFY. It is not in the engine's baked prefix: the
\ require registry's dedupe of co-loaded sites is the only one this line has.
\ A replayed `constant` gets VERIFY's `-- a` effect.
require src/habu/verify-source.f

package CHECK-ALL-ERRORS

public

\ The status a duplicate definition stops a load with.
$4E constant DUP-RC

private

10 constant CA-LF

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
   CA-LEX-ROW? IF
      s" close the row: `PRIM:` ends at `PRIM;`, `EPRIM:` at `EPRIM;`, " CA-SAY
      s" `PPRIM:` at `PPRIM;` or `CLOSE-PRIVATE`, `EPPRIM:` at `ECLOSE-PRIVATE`" CA-SAY
      exit
   THEN
   s" close the string literal before the file ends" CA-SAY ;

: CA-LEX-RECORD ( -- )
   CA-ARM
   CA-LEX-ROW? IF s" E-MALFORMED-REGISTRY-ROW" ELSE s" E-UNTERMINATED-STRING" THEN
   REJECTION:OPEN
   CA-LEX-ROW? IF s" the primitive-axiom row" ELSE s" the string literal" THEN CA-SAY
   s"  opened at " CA-SAY  CA-LEX-TOKEN$ CA-TICKS  s"  does not close" CA-SAY
   CA-LEX-TOKEN$ REJECTION:AT  s" opened here" CA-SAY
   REJECTION:HELP  CA-LEX-HELP
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

\ A statement's throw: its code and message, its throw number standing in for
\ an empty message, and why the scan refused the declaration, when it knows.
: CA-THROW-RECORD ( -- )
   CA-ARM
   CA-THROW-RC @ VERIFY-DIAGNOSTICS:THROW-CODE$
   {: code:ptr cu:n class:ptr clu:n sug:ptr sugu:n msg:ptr msgu:n :}
   code cu REJECTION:OPEN
   msgu 0<> IF msg msgu CA-SAY ELSE
      s" the statement threw " CA-SAY  CA-THROW-RC @ REJECTION:EMIT-N
   THEN
   CA-THROW-TOKEN$ REJECTION:AT  s" the check stopped here" CA-SAY
   VERIFY:REASON$ {: why:ptr whyu:n :}
   whyu 0<> IF s" reason" REJECTION:NOTE  why whyu CA-SAY THEN
   REJECTION:HELP  sug sugu CA-SAY
   REJECTION:KEEP
   REJECTION:SOURCE-OFF ;

: CA-NAME-END ( ptr u8 n n -- n ) {: b:ptr bu:n at:n :}
   at begin dup bu < if b over + c@ 32 > else false then while 1+ repeat ;

\ VERIFY:ON-DUPLICATE: the definition's name, which starts at the given byte of
\ the bytes the scan has armed and has the given length, or the statement's
\ word there when the definer generates the name. The record returns, so the
\ scan skips the definition and goes on.
: CA-ON-DUP ( n n ptr u8 n bool -- )
   drop 2drop {: at:n u:n :}
   REJECTION:SOURCE$ {: b:ptr bu:n :}
   u 0= IF b bu at CA-NAME-END at - ELSE u THEN {: n:n :}
   s" E-DUPLICATE-DEFINITION" REJECTION:OPEN
   u 0= IF s" the name this statement makes is already defined" CA-SAY
   ELSE b at + n CA-TICKS  s"  is already defined" CA-SAY THEN
   b at + n REJECTION:AT  s" already defined" CA-SAY
   REJECTION:HELP  s" rename this word, or remove the earlier definition" CA-SAY
   REJECTION:KEEP ;

public

\ True for the status of a check a statement threw out of unreported: a code
\ outside 0..255. A code in 1..255 is a status a rejection threw after printing
\ its record, so it gets no throw record.
: THREW? ( n -- bool ) {: rc:n :}
   rc 0 < rc 255 > or ;

private

\ True for the code the pre-verifier stops with at a string or a
\ primitive-axiom row the file never closes, and discovery at a string or a
\ locals group, a defect the lexer reads too but for the group.
: LEX-STOP? ( n -- bool ) {: rc:n :}
   rc VERIFY:E-UNTERMINATED-STRING =
   rc VERIFY:E-MALFORMED-REGISTRY-ROW = or
   rc E-DISC-UNTERM = or ;

public

\ True for the code of a stop that the lexer's record of the stopped file goes
\ before, as it goes before any error in the file: a defect the lexer reads
\ too (LEX-STOP?), and a bad escape, which it does not read. With no defect
\ for the lexer to read, the stop's own record stands.
: LEX-FIRST? ( n -- bool ) {: rc:n :}
   rc LEX-STOP? rc VERIFY:E-BAD-ESCAPE = or ;

private

\ ---- a run ------------------------------------------------------------------

variable CA-COMPOSING                   \ the run is a composition

\ A composition stops in one of its files: the subject, by its label, or any
\ other, which is read, by the name its records give it.
: CA-COMPOSE-STOPPED ( -- )
   VERIFY:SOURCE-COMPOSE-STOPPED$ {: a:ptr u:n :}
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? 0= IF a u CA-READ-SOURCE THEN
   u CA-FILE-U !  a CA-FILE-A! ;

\ The record of a statement's throw where the scan stopped: the lexer's when it
\ reads a defect the stop's code says comes first, else the throw's.
: CA-THREW ( n -- ) {: rc:n :}
   rc CA-THROW-RC !
   VERIFY:TOKEN-BYTE@ CA-THROW-AT !
   CA-COMPOSING @ IF CA-COMPOSE-STOPPED THEN
   rc LEX-FIRST? IF CA-LEX IF exit THEN THEN
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
\ A reader with no token after it is the caller's to report (COMPOSE-BUF).
\ The duplicate hook stays after the run: VERIFY has no word that puts its
\ stop back.
: CA-RUN-CAPTURED ( -- n )
   CA-FILE-A@ CA-FILE-U @ REJECTION:FILE!
   1 1 0 REJECTION:ORIGIN!
   ['] CA-ON-DUP is VERIFY:ON-DUPLICATE
   [: CA-CHECK ;] catch {: rc:n :}
   rc VERIFY:E-MISSING-NAME = IF rc throw THEN
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
   labela labelu CA-START
   patha pathu CA-READ-SOURCE
   patha pathu CA-COMPOSE-AT
   true true CA-RUN ;

\ Report the lexer defect of the source file at the given path under the given
\ label, by its record, and throw the rejection status; return when the file
\ lexes clean. Nothing is checked, so a caller that cannot check the file
\ still reports the defect where it stands.
: LEX-FILE ( ptr u8 n ptr u8 n -- )
   {: labela:ptr labelu:n patha:ptr pathu:n :}
   labela labelu CA-START
   patha pathu CA-READ-SOURCE
   [: CA-LEX CA-LEXED ! ;] CA-RECORDS
   CA-LEXED @ IF CHECKER-REJECT-RC throw THEN ;

\ Check the given source bytes as the file at the given path, reporting them
\ under the given label: the composition verifies every file a top-level
\ loader statement loads where it stands, under its own path, in one session.
\ A reader with no token after it throws VERIFY:E-MISSING-NAME to the caller,
\ which reports it at VERIFY:TOKEN-BYTE@ in the file
\ VERIFY:SOURCE-COMPOSE-STOPPED$ names.
: COMPOSE-BUF ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n :}
   label labelu CA-START
   src srcu CA-SOURCE-BUF!
   path pathu CA-COMPOSE-AT
   true false CA-RUN ;

\ Check the given source bytes under the given label as one unit: a loader
\ statement in them loads nothing, and a top-level token is not resolved.
: BUF ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n srca:ptr srcu:n :}
   labela labelu CA-START
   srca srcu CA-SOURCE-BUF!
   false true CA-RUN ;

private

\ ---- old-form record lines ---------------------------------------------------
\ Each is a whole record line returned to the caller in the mode JSON! selected.

variable CA-JSON
create CA-JSON-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE CA-JSON-W JSON:writer
variable CA-JSON-READY

: CA-JSON? ( -- bool )
   CA-JSON @ 0 <> ;

: CA-JSON-WRITER ( -- ptr JSON:writer )
   CA-JSON-READY @ 0= if
      CA-JSON-BUF 1 BUF:N>BLEN BUF:INIT
      1 CA-JSON-READY !
   then
   CA-JSON-W CA-JSON-BUF JSON:WRITE-OPEN-BUF ;

\ The 1-based line and column of byte at in the buffer that starts at a.
: BYTE-ORIGIN ( ptr u8 n -- n n ) {: a:ptr at:n :}
   1 0 at 0 ?do
      a i + c@ CA-LF = if drop 1+ i 1+ then
   loop
   at swap - 1+ ;

: CA-JSON-EMPTY-FIELD ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   s" " JSON:WRITE-FIELD-S ;

\ ---- a definition of a name its scope already held ---------------------------

: CA-JSON-DUP ( ptr u8 n n n n n -- )
   {: wa:ptr wu:n ti:n line:n col:n byte:n :}
   CA-JSON-WRITER JSON:WRITE-OBJECT-START
   s" schema_version" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" code" s" E-DUPLICATE-DEFINITION" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" repair_class" s" rename_duplicate" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" verdict" s" rejected" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" word" wa wu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token" wa wu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token_index" ti JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" file" CA-FILE-A@ CA-FILE-U @ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" line" line JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" column" col JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" byte_start" byte JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" byte_end" byte wu + JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" definition_source" wa wu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" declared_effect" s" unknown" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" declared_effect_source" s" unknown" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" inferred_effect" s" unknown" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" return_stack" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" expected" CA-JSON-EMPTY-FIELD JSON:WRITE-COMMA
   s" actual" CA-JSON-EMPTY-FIELD
   JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" suggestion" s" Rename the word or undefine the old definition before redefining it." JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop ;

\ A session checks several files, so the line names the one that defined the
\ word again, as the JSON record does.
: CA-PROSE-DUP ( -- )
   CA-JSON-WRITER
   s" checker: duplicate definition in " JSON:WRITE-RAW
   CA-FILE-A@ CA-FILE-U @ JSON:WRITE-RAW drop ;

\ The words the engine's wall writes under --load, for the name and its line.
: CA-PROSE-DUP-AT ( ptr u8 n n -- ) {: na:ptr nu:n line:n :}
   CA-JSON-WRITER  s" E-DUPLICATE-DEFINITION habu: " JSON:WRITE-RAW
   CA-FILE-A@ CA-FILE-U @ JSON:WRITE-RAW  s" :" JSON:WRITE-RAW  line JSON:WRITE-U
   s" : duplicate definition: " JSON:WRITE-RAW  na nu JSON:WRITE-RAW drop ;

: CA-DUP-AT ( n n -- ) {: at:n u:n :}
   CA-SRC-A@ at BYTE-ORIGIN {: line:n col:n :}
   CA-SRC-A@ at + {: na:ptr :}
   CA-JSON? IF na u 0 line col at CA-JSON-DUP ELSE na u line CA-PROSE-DUP-AT THEN ;

\ The record of the name the scan refused, which starts at the given byte and
\ has the given length, 0 for no name kept. Both renderings
\ are built in the JSON writer's buffer.
: CA-DUP-RECORD$ ( n n -- ptr u8 n ) {: at:n u:n :}
   u 0 > at u + CA-SRC-U @ <= and IF
      at u CA-DUP-AT
   ELSE
      CA-JSON? IF s" duplicate-definition" 1 1 1 0 CA-JSON-DUP ELSE CA-PROSE-DUP THEN
   THEN
   CA-JSON-W JSON:WRITTEN$ ;

\ ---- malformed primitive-axiom row --------------------------------------------
\ An incomplete primitive-axiom row stops the scan like an open string but gets
\ its own code and repair text: a caller told to close a string literal would
\ look for a quote that is not there.
: CA-PROSE-LEX ( ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n what:ptr whatu:n :}
   CA-JSON-WRITER
   code codeu JSON:WRITE-RAW
   s"  " JSON:WRITE-RAW
   CA-FILE-A@ CA-FILE-U @ JSON:WRITE-RAW
   s" :" JSON:WRITE-RAW LINT-LEX:ERROR-LINE@ JSON:WRITE-U
   s" :" JSON:WRITE-RAW LINT-LEX:ERROR-COL@ JSON:WRITE-U
   s" : " JSON:WRITE-RAW what whatu JSON:WRITE-RAW
   s"  opened at '" JSON:WRITE-RAW CA-LEX-TOKEN$ JSON:WRITE-RAW
   s" ' does not close" JSON:WRITE-RAW drop ;

: CA-PROSE-LEX-ROW ( -- )
   s" E-MALFORMED-REGISTRY-ROW" s" primitive-axiom row" CA-PROSE-LEX ;

: CA-PROSE-LEX-UNTERM ( -- )
   s" E-UNTERMINATED-STRING" s" string literal" CA-PROSE-LEX ;

\ The record of the defect the lexer hit, in the selected mode, built in the
\ JSON writer's buffer.
: CA-LEX-RECORD$ ( -- ptr u8 n )
   CA-JSON? IF
      CA-FILE-A@ CA-FILE-U @ CA-SRC-A@ CA-SRC-U @
      VERIFY-DIAGNOSTICS:LEX-RECORD$ EXIT
   THEN
   CA-LEX-ROW? IF CA-PROSE-LEX-ROW ELSE CA-PROSE-LEX-UNTERM THEN
   CA-JSON-W JSON:WRITTEN$ ;

: CA-THROW-ORIGIN ( -- n n )
   CA-SRC-A@ CA-THROW-AT @ BYTE-ORIGIN ;

: CA-PROSE-THROW ( -- )
   CA-THROW-ORIGIN {: line:n col:n :}
   CA-JSON-WRITER
   CA-THROW-RC @ VERIFY-DIAGNOSTICS:THROW-CODE$ 2swap 2drop 2swap 2drop
   {: code:ptr codeu:n msg:ptr msgu:n :}
   code codeu JSON:WRITE-RAW
   s"  " JSON:WRITE-RAW
   CA-FILE-A@ CA-FILE-U @ JSON:WRITE-RAW
   s" :" JSON:WRITE-RAW line JSON:WRITE-U
   s" :" JSON:WRITE-RAW col JSON:WRITE-U
   s" : " JSON:WRITE-RAW
   msgu 0<> IF
      msg msgu JSON:WRITE-RAW
   ELSE
      s" throw " JSON:WRITE-RAW CA-THROW-RC @ JSON:WRITE-INT
   THEN
   s"  at '" JSON:WRITE-RAW CA-THROW-TOKEN$ JSON:WRITE-RAW
   s" '" JSON:WRITE-RAW drop ;

: CA-THROW-RECORD$ ( -- ptr u8 n )
   CA-JSON? IF
      CA-THROW-RC @ CA-THROW-AT @ CA-FILE-A@ CA-FILE-U @
      CA-SRC-A@ CA-SRC-U @ VERIFY:REASON$ VERIFY-DIAGNOSTICS:THROW-RECORD$ EXIT
   THEN
   CA-PROSE-THROW
   CA-JSON-W JSON:WRITTEN$ ;

public

\ True selects one JSON diagnostic record per rejected definition; false
\ selects the prose rendering.
: JSON! ( bool -- )
   CA-JSON ! ;

\ The record line LEX-FILE writes for the lexer defect of the given bytes, which
\ stand for the file the label names, in the mode JSON! selected, with no line
\ feed; empty when they lex clean.
: LEX-RECORD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: labela:ptr labelu:n srca:ptr srcu:n :}
   labela labelu CA-START
   srca srcu CA-SOURCE-BUF!
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF CA-LEX-RECORD$ EXIT THEN
   s" " ;

\ The record line --all-errors writes for a statement that threw the given code,
\ for a caller that had the checker run over the given source under the given
\ label: at the token that starts at the given byte, the one the checker read
\ last, in the mode JSON! selected, with no line feed.
: THROW-RECORD$ ( n n ptr u8 n ptr u8 n -- ptr u8 n )
   {: rc:n at:n labela:ptr labelu:n srca:ptr srcu:n :}
   labela labelu CA-START
   srca srcu CA-SOURCE-BUF!
   rc CA-THROW-RC !
   at CA-THROW-AT !
   CA-THROW-RECORD$ ;

\ The record line --all-errors writes for a duplicate definition, for a caller
\ that had the checker run over the given source under the given label: at the
\ name the scan refused, which starts at the given byte and has the given
\ length, 0 for no name kept (VERIFY:DUPLICATE), in the mode JSON! selected,
\ with no line feed.
: DUP-RECORD$ ( n n ptr u8 n ptr u8 n -- ptr u8 n )
   {: at:n u:n labela:ptr labelu:n srca:ptr srcu:n :}
   labela labelu CA-START
   srca srcu CA-SOURCE-BUF!
   at u CA-DUP-RECORD$ ;

;package
