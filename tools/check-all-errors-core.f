\ check-all-errors-core.f - reusable all-errors checker core.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f

\ The checked source verifier is a load-time dependency: every checker scope
\ opened below replays source through VERIFY. It is required here, at top
\ level, because VERIFY opens its own package and packages cannot nest;
\ the require registry makes co-loaded `require src/habu/verify-source.f`
\ sites a no-op after the first, which is the sole dedupe protecting this
\ line (verify-source is NOT in the engine's baked startup prefix).
require src/habu/verify-source.f

package CHECK-ALL-ERRORS

public

\ Exit code the checker reports for a duplicate definition. It is published
\ because this core both raises it and classifies it, so a caller comparing its
\ own run against the same value must read it from here.
$4E constant DUP-RC

private

10 constant CA-LF
123 constant CA-LBRACE
70 constant CA-REFUSED                  \ the status of a refusal the checker reported

\ The 1-based line and column of byte at in the buffer that starts at a.
: BYTE-ORIGIN ( ptr u8 n -- n n ) {: a:ptr at:n :}
   1 0 at 0 ?do
      a i + c@ CA-LF = if drop 1+ i 1+ then
   loop
   at swap - 1+ ;

create CA-LF-BUF 1 allot


variable CA-FAILED
variable CA-RAW-FAILURE
variable CA-JSON-FOUND
TYPED-VARIABLE CA-SRC-A ptr u8
variable CA-SRC-U
variable CA-SRC-CAP
variable CA-ERR-LEN
TYPED-VARIABLE CA-ERR-A ptr u8
variable CA-ERR-CAP
variable CA-OUT-LEN
TYPED-VARIABLE CA-OUT-A ptr u8
variable CA-OUT-CAP
variable CA-OUT-FD                      \ where the report goes, or -1 for the buffer
-1 CA-OUT-FD !
variable CA-LS
variable CA-LE
variable CA-THROW-RC                    \ what a statement threw while it was checked
variable CA-THROW-AT                    \ and the byte of the token the checker read last

TYPED-VARIABLE CA-FILE-A ptr u8
variable CA-FILE-U
variable CA-JSON
TYPED-VARIABLE CA-COMPOSE-PATH-A ptr u8
variable CA-COMPOSE-PATH-U
TYPED-VARIABLE CA-COMPOSE-LABEL-A ptr u8
variable CA-COMPOSE-LABEL-U

: CA-TRUE ( -- bool )
   0 0= ;

: CA-FALSE ( -- bool )
   CA-TRUE 0= ;


: CA-SRC-A@ ( -- ptr u8 )
   CA-SRC-A @ ;

: CA-SRC-A! ( ptr u8 -- )
   CA-SRC-A ! ;




: CA-FILE-A@ ( -- ptr u8 )
   CA-FILE-A @ ;

: CA-FILE-A! ( ptr u8 -- )
   CA-FILE-A ! ;

: CA-ERR-A@ ( -- ptr u8 )
   CA-ERR-A @ ;

: CA-ERR-A! ( ptr u8 -- )
   CA-ERR-A ! ;

: CA-OUT-A@ ( -- ptr u8 )
   CA-OUT-A @ ;

: CA-OUT-A! ( ptr u8 -- )
   CA-OUT-A ! ;

: CA-JSON? ( -- bool )
   CA-JSON @ 0 <> ;

















\ The report buffer is the caller's and cannot grow, so a record that does not
\ fit is refused whole by a throw, after every record before it. A streamed
\ report has no buffer to outgrow.
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

: CA-LF$ ( -- ptr u8 n )
   CA-LF CA-LF-BUF c!
   CA-LF-BUF 1 ;

\ A record goes in with its line feed or not at all, so a report a full buffer
\ cut short ends on a whole line.
: CA-ERR-LN ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ CA-OUT-ROOM
   a u CA-ERR
   CA-LF$ CA-ERR ;



















\ Captures the whole `<value> constant NAME` line segment so replay defines the
\ constant; the replay funnels through verify-source, whose one-cell `-- a`
\ model is the PERMANENT constant contract (TFAM 12 verdict 2026-07-09: the
\ interpret stack is untyped by design, no sound shape source exists, and
\ wider-than-cell layout values never land there — DNAME-WIDE dispatch gate).
\ A layout USE of the constant fails closed downstream; parity locked by the
\ const-layout-narrow fixture.






















: CA-JSON-LINE? ( ptr u8 n -- bool )
   LINT-TRIM dup 0= IF 2drop CA-FALSE exit THEN
   drop c@ CA-LBRACE = ;

: CA-ERR-LINE ( n n -- ptr u8 n ) {: start:n end:n :}
   CA-ERR-A@ start + end start - ;

: CA-EMIT-ERR-LINE ( n n -- ) {: start:n end:n :}
   start end CA-ERR-LINE LINT-TRIM CA-ERR-LN ;











: CA-JSON-EMPTY-FIELD ( ptr u8 n -- )
   LJW-KEY s" " LJW-STRING ;


: CA-DUP-WORD$ ( -- ptr u8 n )
   s" duplicate-definition" ;

: CA-JSON-DUP ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" rename_duplicate" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" word" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY 1 LJW-U LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY 1 LJW-U LJW-COMMA
   s" column" LJW-KEY 1 LJW-U LJW-COMMA
   s" byte_start" LJW-KEY 0 LJW-U LJW-COMMA
   s" byte_end" LJW-KEY CA-DUP-WORD$ nip LJW-U LJW-COMMA
   s" definition_source" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" declared_effect" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" declared_effect_source" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" inferred_effect" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" CA-JSON-EMPTY-FIELD LJW-COMMA
   s" actual" CA-JSON-EMPTY-FIELD
   LJW-OBJECT-END LJW-COMMA
   s" suggestion" LJW-KEY s" Rename the word or undefine the old definition before redefining it." LJW-STRING
   LJW-OBJECT-END ;

\ A session checks several files, so the line names the one that defined the
\ word again, as the JSON record does.
: CA-PROSE-DUP ( -- )
   LJW-RESET
   s" checker: duplicate definition in " LJW-RAW
   CA-FILE-A@ CA-FILE-U @ LJW-RAW ;

\ Both renderings are built in the JSON writer's buffer.
: CA-DUP-RECORD$ ( -- ptr u8 n )
   CA-JSON? IF CA-JSON-DUP ELSE CA-PROSE-DUP THEN
   LJW$ ;

: CA-HANDLE-DUP ( -- )
   CA-TRUE CA-FAILED !
   CA-DUP-RECORD$ CA-ERR-LN
   DUP-RC CA-RAW-FAILURE ! ;

: CA-WORD-END ( n -- n )                \ the byte past the source word at byte n
   begin dup CA-SRC-U @ < if CA-SRC-A@ over + c@ 32 > else CA-FALSE then while
      1+
   repeat ;

\ Each lexer defect sits at an opener word, a string opener or a row opener, so
\ the reported token is that word read out of the source, at its own width.
: CA-LEX-TOKEN-U ( -- n )
   LINT-LEX:ERROR-BYTE@ dup CA-WORD-END swap - ;

: CA-LEX-TOKEN$ ( -- ptr u8 n )
   CA-SRC-A@ LINT-LEX:ERROR-BYTE@ + CA-LEX-TOKEN-U ;

: CA-JSON-LEX-UNTERM ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-UNTERMINATED-STRING" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" close_string" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-LEX-TOKEN$ LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY LINT-LEX:ERROR-LINE@ LJW-U LJW-COMMA
   s" column" LJW-KEY LINT-LEX:ERROR-COL@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY LINT-LEX:ERROR-BYTE@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY LINT-LEX:ERROR-BYTE@ CA-LEX-TOKEN-U + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY s" Close the string literal before the definition ends." LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR-LN ;

\ ---- malformed primitive-axiom row --------------------------------------------
\ The lexer's second diagnostic. An incomplete `PRIM:`/`PPRIM:` row stops the scan
\ exactly like an open string does, but it needs its own code and its own repair
\ text: a caller told to close a string literal will look for a quote that is not
\ there.
: CA-ROW-SUGGESTION$ ( -- ptr u8 n )
   s" Close the primitive-axiom row opened at this token: a bare row reads PRIM: name effect... PRIM;, and a package row reads PPRIM: package name effect... PPRIM; or CLOSE-PRIVATE." ;

: CA-JSON-LEX-ROW ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-MALFORMED-REGISTRY-ROW" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" close_primitive_row" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-LEX-TOKEN$ LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY LINT-LEX:ERROR-LINE@ LJW-U LJW-COMMA
   s" column" LJW-KEY LINT-LEX:ERROR-COL@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY LINT-LEX:ERROR-BYTE@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY LINT-LEX:ERROR-BYTE@ CA-LEX-TOKEN-U + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY CA-ROW-SUGGESTION$ LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR-LN ;

: CA-PROSE-LEX-ROW ( -- )
   s" E-MALFORMED-REGISTRY-ROW" CA-ERR-LN ;

: CA-PROSE-LEX-UNTERM ( -- )
   s" E-UNTERMINATED-STRING" CA-ERR-LN ;

: CA-EMIT-LEX-ROW ( -- )
   CA-JSON? IF CA-JSON-LEX-ROW ELSE CA-PROSE-LEX-ROW THEN ;

: CA-EMIT-LEX-UNTERM ( -- )
   CA-JSON? IF CA-JSON-LEX-UNTERM ELSE CA-PROSE-LEX-UNTERM THEN ;

: CA-LEX-ROW? ( -- bool )
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = ;

\ The lexer reports more than one defect now, so name the one it hit.
: CA-HANDLE-LEX-DEFECT ( -- )
   LINT-LEX:ERROR? 0= IF exit THEN
   CA-LEX-ROW? IF CA-EMIT-LEX-ROW ELSE CA-EMIT-LEX-UNTERM THEN
   CA-REFUSED throw ;



: CA-FILTER-JSON ( -- )
   CA-FALSE CA-JSON-FOUND !
   0 CA-LS !
   0 CA-LE !
   begin CA-LE @ CA-ERR-LEN @ < while
      CA-ERR-A@ CA-LE @ + c@ CA-LF = IF
         CA-LS @ CA-LE @ CA-ERR-LINE CA-JSON-LINE? IF
            CA-LS @ CA-LE @ CA-EMIT-ERR-LINE
            CA-TRUE CA-JSON-FOUND !
         THEN
         CA-LE @ 1+ CA-LS !
      THEN
      CA-LE @ 1+ CA-LE !
   repeat
   CA-LS @ CA-ERR-LEN @ < IF
      CA-LS @ CA-ERR-LEN @ CA-ERR-LINE CA-JSON-LINE? IF
         CA-LS @ CA-ERR-LEN @ CA-EMIT-ERR-LINE
         CA-TRUE CA-JSON-FOUND !
      THEN
   THEN ;


: CA-RESET-CAPTURE ( -- )
   0 CA-ERR-LEN ! ;



: CA-DIAG-FINISH ( -- )
   DIAG-BUFFER$ nip CA-ERR-LEN !
   DIAG-BUFFER-OFF ;

: CA-DIAG-FULL-START ( -- )
   CA-FILE-A@ CA-FILE-U @ DIAG-FILE!
   CA-JSON? DIAG-JSON!
   1 1 0 DIAG-ORIGIN!
   CA-ERR-A@ CA-ERR-CAP @ DIAG-BUFFER! ;

: CA-CHECK-FULL-ACT ( -- )
   CA-SRC-A@ CA-SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-CHECK-FULL ( -- n )
   CA-RESET-CAPTURE
   CA-DIAG-FULL-START
   [: CA-CHECK-FULL-ACT ;] catch
   CA-DIAG-FINISH ;

\ A source checked as the loader runs it: each top-level loader statement
\ verifies the file it loads where it stands (VERIFY:SOURCE-COMPOSE-LABELED-IN-
\ SCOPE), in the session's one checker scope.
: CA-CHECK-COMPOSE-ACT ( -- )
   CA-SRC-A@ CA-SRC-U @ CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @
   CA-COMPOSE-LABEL-A @ CA-COMPOSE-LABEL-U @
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: CA-CHECK-COMPOSE ( -- n )
   CA-RESET-CAPTURE
   CA-DIAG-FULL-START
   [: CA-CHECK-COMPOSE-ACT ;] catch
   CA-DIAG-FINISH ;






: CA-RESET-RESULTS ( -- )
   CA-FALSE CA-FAILED !
   0 CA-RAW-FAILURE ! ;


: CA-EMIT-CAPTURED ( n -- ) {: rc:n :}
   CA-TRUE CA-FAILED !
   CA-JSON? IF
      CA-FILTER-JSON
      CA-JSON-FOUND @ 0= IF
         CA-ERR-A@ CA-ERR-LEN @ CA-ERR
         rc 0 <> IF rc CA-RAW-FAILURE ! THEN
      THEN
   ELSE
      CA-ERR-A@ CA-ERR-LEN @ CA-ERR
   THEN ;

\ ---- a statement that throws while it is checked ----------------------------
\ The checker reports a definition it refuses and returns, but a statement can
\ also throw out of it with nothing reported: a `;using` with no `using` open
\ throws E-USING-UNBALANCED. That throw is reported as E-STATEMENT-THROW at the
\ token the checker read last, after whatever it reported before it, and fails
\ the source like a refusal, so the run ends with the checker's status. The
\ rest of the source is not checked. Both renderings are built in the JSON
\ writer's buffer.
: CA-THROW-END ( -- n )
   CA-THROW-AT @ CA-WORD-END ;

: CA-THROW-TOKEN$ ( -- ptr u8 n )
   CA-SRC-A@ CA-THROW-AT @ + CA-THROW-END CA-THROW-AT @ - ;

: CA-THROW-ORIGIN ( -- n n )
   CA-SRC-A@ CA-THROW-AT @ BYTE-ORIGIN ;

: CA-JSON-THROW ( -- )
   CA-THROW-ORIGIN {: line:n col:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-STATEMENT-THROW" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" unknown_rejection" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-THROW-TOKEN$ LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY line LJW-U LJW-COMMA
   s" column" LJW-KEY col LJW-U LJW-COMMA
   s" byte_start" LJW-KEY CA-THROW-AT @ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY CA-THROW-END LJW-U LJW-COMMA
   s" throw_code" LJW-KEY CA-THROW-RC @ LJW-INT LJW-COMMA
   s" suggestion" LJW-KEY s" Inspect the token, signature, and raw stack evidence." LJW-STRING
   LJW-OBJECT-END ;

: CA-PROSE-THROW ( -- )
   CA-THROW-ORIGIN {: line:n col:n :}
   LJW-RESET
   s" E-STATEMENT-THROW " LJW-RAW
   CA-FILE-A@ CA-FILE-U @ LJW-RAW
   s" :" LJW-RAW line LJW-U
   s" :" LJW-RAW col LJW-U
   s" : throw " LJW-RAW CA-THROW-RC @ LJW-INT
   s"  at '" LJW-RAW CA-THROW-TOKEN$ LJW-RAW
   s" '" LJW-RAW ;

: CA-THROW-RECORD$ ( -- ptr u8 n )
   CA-JSON? IF CA-JSON-THROW ELSE CA-PROSE-THROW THEN
   LJW$ ;

: CA-THROW! ( n -- )                     \ what threw, at the token read last
   CA-THROW-RC !
   VERIFY:TOKEN-BYTE@ CA-THROW-AT ! ;

public

\ True for the status of a check that a statement threw out of: neither clean
\ nor a refusal or duplicate the checker reported.
: THREW? ( n -- bool ) {: rc:n :}
   rc 0 <> rc CA-REFUSED <> and rc DUP-RC <> and ;

private

: CA-HANDLE-THROW ( n -- )
   CA-THROW!
   0 CA-EMIT-CAPTURED
   CA-THROW-RECORD$ CA-ERR-LN ;

: CA-ALLOC-SOURCE ( n -- )
   MEM-ALLOC-64K-SPAN CA-SRC-CAP ! CA-SRC-A! ;

: CA-READ-SOURCE ( ptr u8 n -- ) {: path:ptr pu:n :}
   path pu FILE-SIZE CA-ALLOC-SOURCE
   path pu CA-SRC-A@ CA-SRC-CAP @ READ-ALL CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

\ Whole-buffer multi-error drive (Option-A no-cascade ruling on
\ habu-multi-err-checking-42db26f4): ONE verify pass in MULTI-ERR mode emits a
\ file-relative diagnostic for every rejected definition, records each
\ reject's declared signature so later callers check against it (no phantom
\ E-UNDEFINED cascade), and continues to the next definition - the native
\ load path and this tool now share the same machinery. A duplicate
\ definition throws DUP-RC (reported exactly as before), and a verdict-1
\ uncheckable still aborts fail-closed at its definition: uncheckables are
\ not counted by MULTI-ERR-N, so continuing past them would let an
\ all-uncheckable file read as clean.
\ The session is the caller's (SESSION), so a source's rejects are the ones
\ counted while it was checked.
: CA-RUN-DEFS ( -- )
   CA-RESET-RESULTS
   MULTI-ERR-N @ {: before:n :}
   CA-CHECK-FULL {: rc:n :}
   MULTI-ERR-N @ before - {: rejects:n :}
   rc DUP-RC = IF CA-HANDLE-DUP exit THEN
   rc THREW? IF rc CA-HANDLE-THROW exit THEN
   rc 0 <> rejects 0 > or IF rc CA-EMIT-CAPTURED THEN ;

\ A duplicate or a statement's throw stops the composition in one of its files,
\ the one VERIFY:SOURCE-COMPOSE-STOPPED$ names, and its record names that file
\ and reads the token out of its bytes. Every diagnostic the checker made before
\ it is reported first.
: CA-COMPOSE-STOPPED ( -- )
   VERIFY:SOURCE-COMPOSE-STOPPED$ {: a:ptr u:n :}
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? 0= IF a u CA-READ-SOURCE THEN
   u CA-FILE-U !  a CA-FILE-A! ;

: CA-HANDLE-COMPOSE-THROW ( n -- )
   CA-THROW!
   0 CA-EMIT-CAPTURED
   CA-COMPOSE-STOPPED
   CA-THROW-RECORD$ CA-ERR-LN ;

: CA-RUN-COMPOSE-DEFS ( -- )
   CA-RESET-RESULTS
   MULTI-ERR-N @ {: before:n :}
   CA-CHECK-COMPOSE {: rc:n :}
   MULTI-ERR-N @ before - {: rejects:n :}
   rc DUP-RC = IF 0 CA-EMIT-CAPTURED CA-COMPOSE-STOPPED CA-HANDLE-DUP exit THEN
   rc THREW? IF rc CA-HANDLE-COMPOSE-THROW exit THEN
   rc 0 <> rejects 0 > or IF rc CA-EMIT-CAPTURED THEN ;

: CA-START ( ptr u8 n -- ) {: labela:ptr labelu:n :}
   labelu CA-FILE-U !
   labela CA-FILE-A! ;

: CA-LEX ( -- )
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   CA-HANDLE-LEX-DEFECT ;

: CA-RUN-SOURCE ( -- )
   CA-LEX
   CA-RUN-DEFS
   CA-RAW-FAILURE @ 0 <> IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ 0 <> IF CA-REFUSED throw THEN ;

: CA-RUN-COMPOSE ( -- )
   CA-LEX
   CA-RUN-COMPOSE-DEFS
   CA-RAW-FAILURE @ 0 <> IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ 0 <> IF CA-REFUSED throw THEN ;

public

\ Both capture buffers belong to the caller. The first pair is the report
\ buffer this core appends to and OUT$ hands back; the second pair is the
\ scratch buffer the checker renders its raw diagnostics into. Also clears the
\ recorded report length.
: BUFFERS! ( ptr u8 n ptr u8 n -- ) {: outa:ptr outcap:n erra:ptr errcap:n :}
   outcap CA-OUT-CAP !
   outa CA-OUT-A!
   errcap CA-ERR-CAP !
   erra CA-ERR-A!
   0 CA-OUT-LEN !
   -1 CA-OUT-FD ! ;

\ The report the last run accumulated in the caller's first buffer.
: OUT$ ( -- ptr u8 n )
   CA-OUT-A@ CA-OUT-LEN @ ;

\ BUFFERS! for a report written to the given file descriptor as it is made, so
\ it holds as many records, each as long, as the checked sources make; the
\ buffer is the scratch. OUT$ is then empty.
: STREAM! ( fd ptr u8 n -- ) {: fd:fd erra:ptr errcap:n :}
   s" " erra errcap BUFFERS!
   fd FD>N CA-OUT-FD ! ;

\ True selects one JSON diagnostic record per rejected definition; false
\ selects the prose rendering.
: JSON! ( bool -- )
   CA-JSON ! ;

\ Check what the given word checks in one checker scope and one multi-error
\ session, as a load runs it: a definition sees the clean definitions before
\ it and the declared signature of every definition refused before it. The
\ checked sources are files, not a continuation of whatever package the caller
\ has open, so the scope opens at neutral top level, and closing it restores
\ the caller's package on the clean and the throwing path alike.
: SESSION ( [ -- ] -- ) {: q :}
   MULTI-ERR-BEGIN
   CHECKER-SCOPE-START-NEUTRAL
   q catch {: rc:n :}
   CHECKER-SCOPE-DONE
   MULTI-ERR-END drop
   rc 0 <> IF rc throw THEN ;

\ Check the source file at the given path, reporting it under the given label.
: FILE ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n patha:ptr pathu:n :}
   labela labelu CA-START
   patha pathu CA-READ-SOURCE
   [: CA-RUN-SOURCE ;] SESSION ;

\ Report the lexer defect of the source file at the given path under the given
\ label, by the record FILE writes for it, and throw the refusal status; return
\ when the file lexes clean. Nothing is checked, so a caller that cannot check
\ the file still reports the defect where it stands.
: LEX-FILE ( ptr u8 n ptr u8 n -- )
   {: labela:ptr labelu:n patha:ptr pathu:n :}
   labela labelu CA-START
   patha pathu CA-READ-SOURCE
   CA-LEX ;

\ Check the given source bytes as the file at the given path, reporting them under
\ the given label: the composition verifies every file a top-level loader
\ statement loads where it stands, under its own path, in one session.
\ Lexical defects stay each file's own (LEX-FILE).
: COMPOSE-BUF ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n :}
   path CA-COMPOSE-PATH-A !  pathu CA-COMPOSE-PATH-U !
   label CA-COMPOSE-LABEL-A !  labelu CA-COMPOSE-LABEL-U !
   label labelu CA-START
   src srcu CA-SOURCE-BUF!
   [: CA-RUN-COMPOSE ;] SESSION ;

\ Check an in-memory source buffer, reporting it under the given label.
: BUF ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n srca:ptr srcu:n :}
   labela labelu CA-START
   srca srcu CA-SOURCE-BUF!
   [: CA-RUN-SOURCE ;] SESSION ;

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
\ that had the checker run over the source it reports under the given label:
\ in the mode JSON! selected, with no line feed.
: DUP-RECORD$ ( ptr u8 n -- ptr u8 n )
   CA-START
   CA-DUP-RECORD$ ;

;package
