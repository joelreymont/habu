\ check-all-errors-core.f - reusable all-errors checker core.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/process.f, lib/process-argv.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, and tools/lint/source-lex.f.

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

\ Checker-internal multi-error mode control; the checker registry does not
\ publish these to later checked loads (same boundary class as verify-source's
\ CHECK-BODY / MULTI-ERR-MODE?). CA-MULTI-BEGIN arms the whole-buffer pass;
\ CA-MULTI-END reads its reject count and clears it for fail-closed exit.
\ Retire both with habu-multi-err-checking-42db26f4.
TRUSTED: CA-MULTI-BEGIN ( -- )
   MULTI-ERR-BEGIN ;
TRUSTED: CA-MULTI-END ( -- n )
   MULTI-ERR-END ;

10 constant CA-LF
123 constant CA-LBRACE

create CA-LF-BUF 1 allot


variable CA-FULL-R
variable CA-FAILED
variable CA-RAW-FAILURE
variable CA-JSON-FOUND
variable CA-SRC-A
variable CA-SRC-U
variable CA-SRC-CAP
variable CA-ERR-LEN
variable CA-ERR-A
variable CA-ERR-CAP
variable CA-OUT-LEN
variable CA-OUT-A
variable CA-OUT-CAP
variable CA-LS
variable CA-LE
variable CA-TOKU                        \ length of the source word at a lexer diagnostic

variable CA-FILE-A
variable CA-FILE-U
variable CA-JSON

: CA-TRUE ( -- bool )
   0 0= ;

: CA-FALSE ( -- bool )
   CA-TRUE 0= ;


: CA-SRC-A-FIELD ( -- ptr ptr u8 )
   CA-SRC-A 0 ptr-field ;

: CA-SRC-A@ ( -- ptr u8 )
   CA-SRC-A-FIELD @ ;

: CA-SRC-A! ( ptr u8 -- )
   CA-SRC-A-FIELD ! ;




: CA-FILE-A-FIELD ( -- ptr ptr u8 )
   CA-FILE-A 0 ptr-field ;

: CA-FILE-A@ ( -- ptr u8 )
   CA-FILE-A-FIELD @ ;

: CA-FILE-A! ( ptr u8 -- )
   CA-FILE-A-FIELD ! ;

: CA-ERR-A-FIELD ( -- ptr ptr u8 )
   CA-ERR-A 0 ptr-field ;

: CA-ERR-A@ ( -- ptr u8 )
   CA-ERR-A-FIELD @ ;

: CA-ERR-A! ( ptr u8 -- )
   CA-ERR-A-FIELD ! ;

: CA-OUT-A-FIELD ( -- ptr ptr u8 )
   CA-OUT-A 0 ptr-field ;

: CA-OUT-A@ ( -- ptr u8 )
   CA-OUT-A-FIELD @ ;

: CA-OUT-A! ( ptr u8 -- )
   CA-OUT-A-FIELD ! ;

: CA-JSON? ( -- bool )
   CA-JSON @ 0 <> ;

: CA-FAIL ( ptr u8 n n -- )
   die ;

















: CA-OUT-ROOM ( n -- )
   CA-OUT-LEN @ + CA-OUT-CAP @ > IF s" check-all-errors: output buffer full" 76 CA-FAIL THEN ;

: CA-ERR ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF exit THEN
   u CA-OUT-ROOM
   a CA-OUT-A@ CA-OUT-LEN @ + u BYTE-COPY
   CA-OUT-LEN @ u + CA-OUT-LEN ! ;

: CA-LF$ ( -- ptr u8 n )
   CA-LF CA-LF-BUF c!
   CA-LF-BUF 1 ;

\ ---- Cross-file support (source-list redrive) ----------------------------
\ A caller checking an ordered file list registers each already-verified file
\ here; every checker scope opened for the current file first replays the
\ registered files with VERIFY:SOURCE-BUF-IN-SCOPE, so cross-file prefix state
\ (types, packages, definitions) is in scope exactly as at runtime. A support
\ replay failure is annotated and rethrown - never swallowed.

$80 constant CA-XSUP-MAX

create CA-XSUP-PATHS CA-XSUP-MAX FS-PATH-CAP * allot
create CA-XSUP-US CA-XSUP-MAX cells allot
variable CA-XSUP-N
variable CA-XSUP-I
variable CA-XSUP-RC
variable CA-XSUP-BUF-A
variable CA-XSUP-BUF-CAP

: CA-XSUP-BUF-A-FIELD ( -- ptr ptr u8 )
   CA-XSUP-BUF-A 0 ptr-field ;

: CA-XSUP-BUF-A@ ( -- ptr u8 )
   CA-XSUP-BUF-A-FIELD @ ;

: CA-XSUP-BUF-A! ( ptr u8 -- )
   CA-XSUP-BUF-A-FIELD ! ;

: CA-XSUP$ ( n -- ptr u8 n ) {: i:n :}
   CA-XSUP-PATHS i FS-PATH-CAP * +
   CA-XSUP-US i cells + @ ;

: CA-XSUP-BUF ( n -- ptr u8 n ) {: need:n :}
   need CA-XSUP-BUF-CAP @ > IF
      need MEM-ALLOC-64K-SPAN CA-XSUP-BUF-CAP ! CA-XSUP-BUF-A!
   THEN
   CA-XSUP-BUF-A@ CA-XSUP-BUF-CAP @ ;

: CA-XSUP-REPLAY-ONE ( n -- ) {: i:n :}
   i CA-XSUP$ {: pa:ptr pu:n :}
   pa pu FILE-SIZE CA-XSUP-BUF {: buf:ptr cap:n :}
   pa pu buf cap READ-ALL {: u:n :}
   buf u VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-XSUP-NOTE ( n n -- ) {: i:n rc:n :}
   rc 0= IF exit THEN
   rc CA-XSUP-RC !
   s" all-errors: support replay failed: " CA-ERR
   i CA-XSUP$ CA-ERR
   CA-LF$ CA-ERR
   rc throw ;

: CA-XSUP-REPLAY-CUR ( -- )
   CA-XSUP-I @ CA-XSUP-REPLAY-ONE ;

: CA-XSUP-REPLAY ( -- )
   0 CA-XSUP-I !
   begin CA-XSUP-I @ CA-XSUP-N @ < while
      [: CA-XSUP-REPLAY-CUR ;] catch CA-XSUP-I @ swap CA-XSUP-NOTE
      CA-XSUP-I @ 1+ CA-XSUP-I !
   repeat ;


















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
   start end CA-ERR-LINE LINT-TRIM CA-ERR
   CA-LF$ CA-ERR ;











: CA-JSON-EMPTY-FIELD ( ptr u8 n -- )
   LJW-KEY s" " LJW-STRING ;


: CA-DUP-WORD$ ( -- ptr u8 n )
   s" duplicate-definition" ;

: CA-JSON-DUP ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" fix_source" LJW-STRING LJW-COMMA
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
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

: CA-PROSE-DUP ( -- )
   s" checker: duplicate definition" CA-ERR
   CA-LF$ CA-ERR ;

: CA-HANDLE-DUP ( -- )
   CA-TRUE CA-FAILED !
   CA-JSON? IF CA-JSON-DUP ELSE CA-PROSE-DUP THEN
   DUP-RC CA-RAW-FAILURE ! ;

: CA-JSON-LEX-UNTERM ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-UNTERMINATED-STRING" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" fix_source" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-SRC-A@ LINT-LEX:ERROR-BYTE@ + 2 LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY LINT-LEX:ERROR-LINE@ LJW-U LJW-COMMA
   s" column" LJW-KEY LINT-LEX:ERROR-COL@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY LINT-LEX:ERROR-BYTE@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY LINT-LEX:ERROR-BYTE@ 2 + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY s" Close the string literal before the definition ends." LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

\ ---- malformed primitive-axiom row --------------------------------------------
\ The lexer's second diagnostic. An incomplete `PRIM:`/`PPRIM:` row stops the scan
\ exactly like an open string does, but it needs its own code and its own repair
\ text: a caller told to close a string literal will look for a quote that is not
\ there. The diagnostic site is the row OPENER, so the reported token is the opener
\ word read out of the source.
: CA-ROW-TOKEN-U ( -- n )
   0 CA-TOKU !
   begin
      LINT-LEX:ERROR-BYTE@ CA-TOKU @ + CA-SRC-U @ <
      CA-SRC-A@ LINT-LEX:ERROR-BYTE@ + CA-TOKU @ + c@ 32 > and
   while
      CA-TOKU @ 1+ CA-TOKU !
   repeat
   CA-TOKU @ ;

: CA-ROW-TOKEN$ ( -- ptr u8 n )
   CA-SRC-A@ LINT-LEX:ERROR-BYTE@ + CA-ROW-TOKEN-U ;

: CA-ROW-SUGGESTION$ ( -- ptr u8 n )
   s" Close the primitive-axiom row opened at this token: a bare row reads PRIM: name effect... PRIM;, and a package row reads PPRIM: package name effect... PPRIM; or CLOSE-PRIVATE." ;

: CA-JSON-LEX-ROW ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-MALFORMED-REGISTRY-ROW" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" fix_source" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-ROW-TOKEN$ LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY LINT-LEX:ERROR-LINE@ LJW-U LJW-COMMA
   s" column" LJW-KEY LINT-LEX:ERROR-COL@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY LINT-LEX:ERROR-BYTE@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY LINT-LEX:ERROR-BYTE@ CA-ROW-TOKEN-U + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY CA-ROW-SUGGESTION$ LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

: CA-PROSE-LEX-ROW ( -- )
   s" E-MALFORMED-REGISTRY-ROW" CA-ERR
   CA-LF$ CA-ERR ;

: CA-PROSE-LEX-UNTERM ( -- )
   s" E-UNTERMINATED-STRING" CA-ERR
   CA-LF$ CA-ERR ;

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
   70 throw ;



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

\ The rollback frame CHECKER-SCOPE-START pushes SAVES the caller's checker
\ package mode, name, and length, but it does not clear them, so the replayed
\ source would keep checking as if it were still inside the caller's package -
\ a top-level EXPORT directive in that source then reads as an in-package
\ re-export and the run exits DUP-RC. The replayed file is standalone source,
\ so enter neutral top-level package state before the replay; the matching
\ CHECKER-SCOPE-DONE pops the frame and restores the caller's exact package on
\ both the clean and the throwing path.
: CA-CHECK-FULL-SCOPE ( -- n )
   0 CA-FULL-R !
   CHECKER-SCOPE-START
   CHECKER-END-PACKAGE
   [: CA-XSUP-REPLAY CA-CHECK-FULL CA-FULL-R ! ;] catch
   CHECKER-SCOPE-DONE
   dup 0= IF drop CA-FULL-R @ exit THEN ;






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
: CA-RUN-DEFS ( -- )
   CA-RESET-RESULTS
   CA-MULTI-BEGIN
   CA-CHECK-FULL-SCOPE {: rc:n :}
   CA-MULTI-END {: rejects:n :}
   CA-XSUP-RC @ 0 <> IF CA-XSUP-RC @ throw THEN
   rc DUP-RC = IF CA-HANDLE-DUP exit THEN
   rc 0 <> rejects 0 > or IF rc CA-EMIT-CAPTURED THEN ;

: CA-ALLOC-SOURCE ( n -- )
   MEM-ALLOC-64K-SPAN CA-SRC-CAP ! CA-SRC-A! ;

: CA-READ-SOURCE ( ptr u8 n -- ) {: path:ptr pu:n :}
   path pu FILE-SIZE CA-ALLOC-SOURCE
   path pu CA-SRC-A@ CA-SRC-CAP @ READ-ALL CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

: CA-RUN-SOURCE ( -- )
   CA-SRC-A@ CA-SRC-U @ LINT-LEX:SOURCE
   CA-HANDLE-LEX-DEFECT
   CA-RUN-DEFS
   CA-RAW-FAILURE @ 0 <> IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ 0 <> IF 70 throw THEN ;

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
   0 CA-OUT-LEN ! ;

\ The report the last run accumulated in the caller's first buffer.
: OUT$ ( -- ptr u8 n )
   CA-OUT-A@ CA-OUT-LEN @ ;

\ True selects one JSON diagnostic record per rejected definition; false
\ selects the prose rendering.
: JSON! ( bool -- )
   CA-JSON ! ;

\ Empty the ordered list of already-verified files replayed before each run.
: SUPPORT-RESET ( -- )
   0 CA-XSUP-N ! ;

\ Append one already-verified file path to that replay list.
: SUPPORT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CA-XSUP-N @ CA-XSUP-MAX >= IF E-TBL-BOUNDS throw THEN
   u FS-PATH-CAP > IF E-FS-CAPACITY throw THEN
   a CA-XSUP-PATHS CA-XSUP-N @ FS-PATH-CAP * + u BYTE-COPY
   u CA-XSUP-US CA-XSUP-N @ cells + !
   CA-XSUP-N @ 1+ CA-XSUP-N ! ;

\ Check the source file at the given path, reporting it under the given label.
: FILE ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n patha:ptr pathu:n :}
   0 CA-XSUP-RC !
   labelu CA-FILE-U !
   labela CA-FILE-A!
   patha pathu CA-READ-SOURCE
   CA-RUN-SOURCE ;

\ Check an in-memory source buffer, reporting it under the given label.
: BUF ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n srca:ptr srcu:n :}
   0 CA-XSUP-RC !
   labelu CA-FILE-U !
   labela CA-FILE-A!
   srca srcu CA-SOURCE-BUF!
   CA-RUN-SOURCE ;

;package
