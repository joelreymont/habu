\ check-all-errors-core.f - reusable all-errors checker core.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/source.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require lib/source-lex.f
require lib/verify-diagnostics.f

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

\ The scratch a caller gives BUFFERS! or STREAM!. The checker renders a whole
\ pass's diagnostics into it and src/core/render.f cannot grow a caller's
\ buffer, so a pass whose diagnostics outgrow it reports those that fit, each
\ whole, then E-DIAG-CAPACITY (tools/check-test-lib.f, "a report past the
\ scratch"). A JSON record is a few hundred bytes: this holds thousands.
$100000 constant SCRATCH-CAP

private

10 constant CA-LF
123 constant CA-LBRACE
70 constant CA-REFUSED                  \ the status of a refusal the checker reported

public

\ The 1-based line and column of byte at in the buffer that starts at a.
: BYTE-ORIGIN ( ptr u8 n -- n n ) {: a:ptr at:n :}
   1 0 at 0 ?do
      a i + c@ CA-LF = if drop 1+ i 1+ then
   loop
   at swap - 1+ ;

private

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


\ ---- a definition of a name its scope already held ---------------------------
\ The scan refuses it at its name and keeps where the name starts and its length
\ (src/habu/verify-source.f VERIFY:DUPLICATE), not the name: a required file's
\ bytes are unmapped before the report. Its record reads the name out of the
\ reporter's own read of the file the composition stopped in, as a statement
\ throw's record reads its token, and names it and its place as --load does.
\ The record is the placeholder `duplicate-definition` at line 1 for a name the
\ definer generates instead of writing (a DYNAMIC-BUFFER's NAME-RESERVE or
\ NAME-RELEASE, a DEFER-LAYOUT-BUFFER's NAME-BIND or NAME-GROW), which the
\ checker's own guard refuses with nothing kept, and for a kept name that ends
\ past that read, as when the file shrank after the scan.

\ The word, its token index, and the line, column and byte where it starts.
: CA-JSON-DUP ( ptr u8 n n n n n -- )
   {: wa:ptr wu:n ti:n line:n col:n byte:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" rename_duplicate" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" word" LJW-KEY wa wu LJW-STRING LJW-COMMA
   s" token" LJW-KEY wa wu LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY ti LJW-U LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY line LJW-U LJW-COMMA
   s" column" LJW-KEY col LJW-U LJW-COMMA
   s" byte_start" LJW-KEY byte LJW-U LJW-COMMA
   s" byte_end" LJW-KEY byte wu + LJW-U LJW-COMMA
   s" definition_source" LJW-KEY wa wu LJW-STRING LJW-COMMA
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

\ The words the engine's wall writes under --load, for the name and its line.
: CA-PROSE-DUP-AT ( ptr u8 n n -- ) {: na:ptr nu:n line:n :}
   LJW-RESET
   s" duplicate definition: " LJW-RAW
   na nu LJW-RAW
   s"  at " LJW-RAW
   CA-FILE-A@ CA-FILE-U @ LJW-RAW
   s" :" LJW-RAW line LJW-U ;

: CA-DUP-AT ( n n -- ) {: at:n u:n :}
   CA-SRC-A@ at BYTE-ORIGIN {: line:n col:n :}
   CA-SRC-A@ at + {: na:ptr :}
   CA-JSON? IF na u 0 line col at CA-JSON-DUP ELSE na u line CA-PROSE-DUP-AT THEN ;

\ The record of the name the scan refused, which starts at the given byte and
\ has the given length, 0 for no name kept (VERIFY:DUPLICATE). Both renderings
\ are built in the JSON writer's buffer.
: CA-DUP-RECORD$ ( n n -- ptr u8 n ) {: at:n u:n :}
   u 0 > at u + CA-SRC-U @ <= and IF
      at u CA-DUP-AT
   ELSE
      CA-JSON? IF s" duplicate-definition" 1 1 1 0 CA-JSON-DUP ELSE CA-PROSE-DUP THEN
   THEN
   LJW$ ;

: CA-HANDLE-DUP ( -- )
   CA-TRUE CA-FAILED !
   VERIFY:DUPLICATE CA-DUP-RECORD$ CA-ERR-LN
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

\ ---- malformed primitive-axiom row --------------------------------------------
\ The lexer's second diagnostic. An incomplete `PRIM:`/`PPRIM:` row stops the scan
\ exactly like an open string does, but it needs its own code and its own repair
\ text: a caller told to close a string literal will look for a quote that is not
\ there.
\ The prose names the code, the file, line and column of the opener, what it
\ opens, and the opener as written.
: CA-PROSE-LEX ( ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n what:ptr whatu:n :}
   LJW-RESET
   code codeu LJW-RAW
   s"  " LJW-RAW
   CA-FILE-A@ CA-FILE-U @ LJW-RAW
   s" :" LJW-RAW LINT-LEX:ERROR-LINE@ LJW-U
   s" :" LJW-RAW LINT-LEX:ERROR-COL@ LJW-U
   s" : " LJW-RAW what whatu LJW-RAW
   s"  opened at '" LJW-RAW CA-LEX-TOKEN$ LJW-RAW
   s" ' does not close" LJW-RAW ;

: CA-PROSE-LEX-ROW ( -- )
   s" E-MALFORMED-REGISTRY-ROW" s" primitive-axiom row" CA-PROSE-LEX ;

: CA-PROSE-LEX-UNTERM ( -- )
   s" E-UNTERMINATED-STRING" s" string literal" CA-PROSE-LEX ;

: CA-LEX-ROW? ( -- bool )
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = ;

\ The record of the defect the lexer hit, in the selected mode, built in the
\ JSON writer's buffer. The lexer reports more than one defect, so the record
\ names the one it hit.
: CA-LEX-RECORD$ ( -- ptr u8 n )
   CA-JSON? IF
      CA-FILE-A@ CA-FILE-U @ CA-SRC-A@ CA-SRC-U @
      VERIFY-DIAGNOSTICS:LEX-RECORD$ EXIT
   THEN
   CA-LEX-ROW? IF CA-PROSE-LEX-ROW ELSE CA-PROSE-LEX-UNTERM THEN
   LJW$ ;

: CA-HANDLE-LEX-DEFECT ( -- )
   LINT-LEX:ERROR? 0= IF exit THEN
   CA-LEX-RECORD$ CA-ERR-LN
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
: CA-CHECK-COMPOSE-VERIFY ( -- )
   CA-SRC-A@ CA-SRC-U @ CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @
   CA-COMPOSE-LABEL-A @ CA-COMPOSE-LABEL-U @
   VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: CA-CHECK-COMPOSE-ACT ( -- )
   CA-COMPOSE-PATH-A @ CA-COMPOSE-PATH-U @ SOURCE-ROOT:DIRNAME
   [: CA-CHECK-COMPOSE-VERIFY ;] SOURCE-ROOT:WITH ;

: CA-CHECK-COMPOSE ( -- n )
   CA-RESET-CAPTURE
   CA-DIAG-FULL-START
   [: CA-CHECK-COMPOSE-ACT ;] catch
   CA-DIAG-FINISH ;






: CA-RESET-RESULTS ( -- )
   CA-FALSE CA-FAILED !
   0 CA-RAW-FAILURE ! ;


: CA-WRITE-CAPTURED ( -- )
   CA-JSON? IF
      CA-FILTER-JSON
      CA-JSON-FOUND @ 0= IF
         CA-ERR-A@ CA-ERR-LEN @ CA-ERR
      THEN
   ELSE
      CA-ERR-A@ CA-ERR-LEN @ CA-ERR
   THEN ;

: CA-EMIT-CAPTURED ( n -- ) {: rc:n :}
   CA-TRUE CA-FAILED !
   CA-WRITE-CAPTURED
   rc 0 <> IF rc CA-RAW-FAILURE ! THEN ;

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
   CA-JSON? IF
      CA-THROW-RC @ CA-THROW-AT @ CA-FILE-A@ CA-FILE-U @
      CA-SRC-A@ CA-SRC-U @ VERIFY-DIAGNOSTICS:THROW-RECORD$ EXIT
   THEN
   CA-PROSE-THROW
   LJW$ ;

: CA-THROW! ( n -- )                     \ what threw, at the token read last
   CA-THROW-RC !
   VERIFY:TOKEN-BYTE@ CA-THROW-AT ! ;

public

\ True for a checker refusal that renders its own diagnostic before throwing.
\ It keeps that packet instead of acquiring a statement-throw record, and the
\ check fails as for any refusal (check-core.f CHK-PREVERIFY-STOPPED).
: REPORTED-THROW? ( n -- bool )
   {: rc:n :}
   rc E-USING-SHADOW-GLOBAL =
   rc E-USING-AMBIGUOUS = or
   rc E-TRUST-UNRESOLVED = or
   rc E-SHADOWED-ARITY = or
   rc E-GENERATES-ROW = or
   rc E-PARSES-ROW = or
   rc E-NAMES-ROW = or ;

\ True for the status of a check that a statement threw out of without
\ reporting its own refusal.
: THREW? ( n -- bool ) {: rc:n :}
   rc 0 <> rc CA-REFUSED <> and rc DUP-RC <> and
   rc REPORTED-THROW? 0= and ;

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

: CA-HANDLE-THROW ( n -- )
   CA-THROW!
   0 CA-EMIT-CAPTURED
   CA-THROW-RECORD$ CA-ERR-LN ;

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
   path pu  path pu FILE-SIZE  [: CA-SRC-ROOM ;] SOURCE:READ-WHOLE-SAMPLED CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

\ Whole-buffer multi-error drive (Option-A no-cascade ruling on
\ habu-multi-err-checking-42db26f4): ONE verify pass in MULTI-ERR mode emits a
\ file-relative diagnostic for every refused definition, rejected or
\ uncheckable, counts it in MULTI-ERR-N, records its declared signature so
\ later callers check against it (no phantom E-UNDEFINED cascade), and
\ continues to the next definition - the native load path and this tool now
\ share the same machinery. A duplicate definition throws DUP-RC (reported
\ exactly as before).
\ The session is the caller's (SESSION), so a source's rejects are the ones
\ counted while it was checked. A source with none still gets what the checker
\ wrote of it, its warnings.
: CA-RUN-DEFS ( -- )
   CA-RESET-RESULTS
   MULTI-ERR-N @ {: before:n :}
   CA-CHECK-FULL {: rc:n :}
   MULTI-ERR-N @ before - {: rejects:n :}
   rc DUP-RC = IF CA-HANDLE-DUP exit THEN
   rc THREW? IF rc CA-HANDLE-THROW exit THEN
   rc 0 <> rejects 0 > or IF rc CA-EMIT-CAPTURED exit THEN
   CA-WRITE-CAPTURED ;

\ A duplicate or a statement's throw stops the composition in one of its files,
\ the one VERIFY:SOURCE-COMPOSE-STOPPED$ names, and its record names that file
\ and reads the token out of its bytes. Every diagnostic the checker made before
\ it is reported first.
: CA-COMPOSE-STOPPED ( -- )
   VERIFY:SOURCE-COMPOSE-STOPPED$ {: a:ptr u:n :}
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? 0= IF a u CA-READ-SOURCE THEN
   u CA-FILE-U !  a CA-FILE-A! ;

: CA-LEX ( -- )
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   CA-HANDLE-LEX-DEFECT ;

\ The lexer reads a file the composition loads only when the pre-verifier
\ reaches it, so a stop its record goes before (LEX-FIRST?) is reported by the
\ lexer's record for the file it stopped in; a lex that finds no defect leaves
\ the statement's throw record.
: CA-HANDLE-COMPOSE-THROW ( n -- ) {: rc:n :}
   rc CA-THROW!
   0 CA-EMIT-CAPTURED
   CA-COMPOSE-STOPPED
   rc LEX-FIRST? IF CA-LEX THEN
   CA-THROW-RECORD$ CA-ERR-LN ;

\ A reader with no token after it is the refusal tools/check-core.f writes for
\ the nominal pass (CHK-NONAME-FAIL), so its code goes up to COMPOSE-BUF's
\ caller after what the checker reported before it.
: CA-RUN-COMPOSE-DEFS ( -- )
   CA-RESET-RESULTS
   MULTI-ERR-N @ {: before:n :}
   CA-CHECK-COMPOSE {: rc:n :}
   MULTI-ERR-N @ before - {: rejects:n :}
   rc VERIFY:E-MISSING-NAME = IF 0 CA-EMIT-CAPTURED rc throw THEN
   rc DUP-RC = IF 0 CA-EMIT-CAPTURED CA-COMPOSE-STOPPED CA-HANDLE-DUP exit THEN
   rc THREW? IF rc CA-HANDLE-COMPOSE-THROW exit THEN
   rc 0 <> rejects 0 > or IF rc CA-EMIT-CAPTURED exit THEN
   CA-WRITE-CAPTURED ;

: CA-START ( ptr u8 n -- ) {: labela:ptr labelu:n :}
   labelu CA-FILE-U !
   labela CA-FILE-A! ;

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

\ Check the given source bytes as the file at the given path, reporting them under
\ the given label: the composition verifies every file a top-level loader
\ statement loads where it stands, under its own path, in one session.
\ Lexical defects stay each file's own (LEX-FILE): a loaded file is lexed when
\ the pre-verifier stops in it at an open string or row or a bad escape
\ (LEX-FIRST?). A reader with no token after it throws VERIFY:E-MISSING-NAME
\ to the caller, which reports it at VERIFY:TOKEN-BYTE@ in the file
\ VERIFY:SOURCE-COMPOSE-STOPPED$ names.
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
