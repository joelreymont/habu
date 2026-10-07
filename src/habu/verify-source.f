\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

require lib/errors.f
require src/core/checker-owner-guard.f
require lib/string.f
require src/habu/layout.f

package VERIFY

public

\ A source the scan cannot read on stops it with one of these, TOKEN-BYTE@ at
\ the word that cannot finish: a reader (a definer, a parsing word) with no
\ token after it, a string opener with no closing quote, a PRIM: or PPRIM: row
\ with no closer, a top-level escaped literal holding an escape the engine
\ refuses. tools/check.f reports the first three where they stand, by the
\ record it writes when it finds the same defect itself. Its lexer reads no
\ escape, so it reports a bad escape by the record of a statement that throws,
\ at the literal's opener, under a code of its own that names the defect; as
\ before any error, a defect its lexer reads anywhere in the file is reported
\ instead, in its place (CHECK-ALL-ERRORS:LEX-FIRST?).
7187 constant E-MISSING-NAME
7188 constant E-UNTERMINATED-STRING
7189 constant E-MALFORMED-REGISTRY-ROW
7194 constant E-BAD-ESCAPE

\ A file a quiet composition (SOURCE-COMPOSE-QUIET-IN-SCOPE) loads that the
\ system does not open or read stops it with this, TOKEN-BYTE@ at the loader
\ that loads the file and FAULT-TARGET$ naming the file as it was resolved.
7201 constant E-SOURCE-READ

\ Captured by the fresh child before it loads verifier tooling. A direct
\ caller that does not supply an entry observation retains checking order.
variable ENTRY-TICK-ORDER
PTR-VARIABLE ENTRY-TICK-OWNER

: ENTRY-TICK-ORDER! ( n ptr u8 -- )
   ENTRY-TICK-OWNER !  ENTRY-TICK-ORDER ! ;

\ Whether a quiet composition keeps, in the file it is scanning, the loader
\ forms it refuses (the quiet composition's section below). The file is named
\ by its canonical path, the subject's as the caller supplied it; the path is
\ borrowed. By default no file is.
defer LENIENT-FILE? ( ptr u8 n -- bool )

private

\ A statement the source ends inside, one that lacks a part it must have, or one
\ past a bound the engine sets stops the scan with one of these at its opener
\ (STATEMENT-STOP). tools/check.f reports each by the record of a statement
\ that throws.
7155 constant E-VS-UNTERMINATED-DEFINITION \ a definition, signature or group
7157 constant E-VS-MISSING-SIGNATURE       \ a definer's, absent or unclosed
7158 constant E-VS-BARE-TRUST              \ TRUST with no name and signature
7199 constant E-VS-EFFECT-SIZE             \ a generates: effect past GENR-SIG-CAP

\ A declaration the checker refuses stops the scan with one of these where the
\ refusal stands, in the order tools/check.f's nominal pass asks
\ (CHK-VREC-DEFRECORD); the checker's registration, which a load reaches, dies
\ with the refusal instead.
7200 constant E-VS-TYPE-NAME               \ a DEFLINEAR or VALUE-RECORD name no type may take
7198 constant E-VS-RECORD-FIELD            \ a VALUE-RECORD field, or a record with none
TYPE-DECL:E-TDECL-SYNTAX constant E-VS-DECL-SYNTAX \ a declaration never ends

PTR-VARIABLE SOURCE-A
variable SOURCE-U
variable SCAN-I
variable SKIP-STRINGS
variable FOUND
variable TOKEN-START
\ The stopped token survives scanner restoration after a nested file throws.
variable TOKEN-BYTE
PTR-VARIABLE TOKEN-A
variable TOKEN-U
variable BODY-U
variable BASE-LINE
variable BASE-COL
variable BASE-BYTE
PTR-VARIABLE STR-PREV-A
variable STR-PREV-U
PTR-VARIABLE STR-LAST-A
variable STR-LAST-U
variable STR-LAST-KIND
PTR-VARIABLE TOP-PREV-A
variable TOP-PREV-U
PTR-VARIABLE TOP-REFUSED                 \ the top-level token refused last (TOP-RESOLVE)
PTR-VARIABLE TOP-CUR-A
variable TOP-CUR-U
create BODY-BUF BODYBUF-CAP allot
\ Each run BODY-APPEND copies into BODY-BUF is a row of two cells: the body
\ offset it starts at and the source offset it was read from. BODY$ hands the
\ rows to the checker (DIAG-MAP!), so a packet locates its token at the file
\ bytes the scanner read. A run takes at least one byte and its separator, so
\ BODYBUF-CAP cells hold every row a full body can have.
create BODY-ROW BODYBUF-CAP cells allot
variable BODY-ROWS

\ Composition is a fixed scanner operation. The subject stays pinned across
\ nested loads so a require back to it observes these bytes, and an include of
\ it scans these bytes again.
variable COMPOSE-ON
PTR-VARIABLE COMPOSE-SUBJ-A
variable COMPOSE-SUBJ-U
create COMPOSE-SUBJ-PATH PATH-CAP allot
variable COMPOSE-SUBJ-PATH-U
create COMPOSE-SUBJ-LABEL PATH-CAP allot
variable COMPOSE-SUBJ-LABEL-U
PTR-VARIABLE COMPOSE-CUR-PATH-A
variable COMPOSE-CUR-PATH-U
PTR-VARIABLE COMPOSE-PEND-A
variable COMPOSE-PEND-U
PTR-VARIABLE COMPOSE-PEND-PATH-A
variable COMPOSE-PEND-PATH-U
variable COMPOSE-REQ0
create COMPOSE-STOP-PATH PATH-CAP allot
variable COMPOSE-STOP-U
variable COMPOSE-STOP-SUBJ               \ the stop was in the supplied bytes

\ A quiet composition (SOURCE-COMPOSE-QUIET-IN-SCOPE) and the load fault it
\ stopped at: the loader token's length, 0 when no loader stopped it, and the
\ file it could not read, empty for none.
TYPED-VARIABLE QUIET bool
variable FAULT-LEN
create FAULT-TARGET PATH-CAP allot
variable FAULT-U

\ ---- completion: the cursor --------------------------------------------------
\ A completion names a byte of the subject (CURSOR!). The subject's scan finds
\ the token the byte is in, or the blanks before it, as the scanner reads
\ tokens (CSR-HIT), and the reader that takes that token answers: a top-level
\ statement, or a top-level tick's operand, has the checker offer the spellings
\ that bind there before the token is consumed (CSR-TOP); a run a definition's
\ body appends places the cursor in the body's text, where that body's check
\ offers them (CSR-PLACE, CSR-ARM). A token any other reader takes - a
\ definer's name, a loader's or `using`'s operand, a TRUSTED: body, type
\ syntax - offers nothing, nor does a byte the scan passed inside a comment, a
\ string or a signature, nor one it never reaches. Blanks before a comment or
\ string the scan skips belong to the token after it. One answer at most.
0 constant CSR-WAIT                      \ not reached yet
1 constant CSR-SPACE                     \ in the blanks before the token at CSR-TOK
2 constant CSR-PART                      \ inside the token at CSR-TOK, past its first byte
3 constant CSR-PLACED                    \ at CSR-BODY in the body being read
4 constant CSR-DONE                      \ answered, or nothing to answer
variable CSR-AT                          \ the subject byte, -1 for none
variable CSR-STATE
variable CSR-TOK                         \ where that token starts; -1 for the next one read
variable CSR-BODY
variable CSR-SUBJ                        \ the file being scanned is the subject

: CSR-RESET ( -- )
   -1 CSR-AT !  CSR-DONE CSR-STATE !  0 CSR-SUBJ ! ;

CSR-RESET

\ The cursor waits on the token read last.
: CSR-HIT? ( -- bool )
   CSR-STATE @ CSR-SPACE =  CSR-STATE @ CSR-PART =  or ;

\ NEXT-RAW read from PREV past blanks to the token from START to END, START =
\ END at the source's end. A waiting cursor is in those blanks or that token,
\ or was passed; one the token before waited on, which no reader took, offers
\ nothing.
: CSR-HIT ( n n n -- )
   {: prev:n start:n end:n :}
   CSR-SUBJ @ 0= IF EXIT THEN
   BASE-BYTE @ {: b:n :}
   CSR-STATE @ CSR-SPACE =  CSR-TOK @ 0 <  and IF b start + CSR-TOK ! EXIT THEN
   CSR-HIT? IF CSR-DONE CSR-STATE ! EXIT THEN
   CSR-STATE @ CSR-WAIT <> IF EXIT THEN
   CSR-AT @  b end +  > IF EXIT THEN
   CSR-AT @  b prev +  < IF CSR-DONE CSR-STATE ! EXIT THEN
   b start + CSR-TOK !
   CSR-AT @ CSR-TOK @ > IF CSR-PART ELSE CSR-SPACE THEN CSR-STATE ! ;

\ The scan skipped a comment or a string. Blanks before its opener belong to
\ the token after it; a cursor in the opener offers nothing, nor does one
\ still waiting, past the opener, when the skip ran out of source (FOUND = 0):
\ it is inside the comment or string the file ends in.
: CSR-SKIPPED ( -- )
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-STATE @ CSR-SPACE = IF -1 CSR-TOK ! EXIT THEN
   CSR-STATE @ CSR-PART = IF CSR-DONE CSR-STATE ! EXIT THEN
   CSR-STATE @ CSR-WAIT =  FOUND @ 0=  and IF CSR-DONE CSR-STATE ! THEN ;

\ A body run read from source offset SRC starts at BODY-U: the token the cursor
\ waits on places it in the body's text.
: CSR-PLACE ( n -- )
   {: src:n :}
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-HIT? 0= IF EXIT THEN
   CSR-TOK @  BASE-BYTE @ src +  <> IF EXIT THEN
   CSR-STATE @ CSR-PART = IF CSR-AT @ CSR-TOK @ - ELSE 0 THEN
   BODY-U @ + CSR-BODY !
   CSR-PLACED CSR-STATE ! ;

1 constant TICK-GATE
2 constant TICK-UNKNOWN
3 constant TICK-PARENT-GATE
4 constant TICK-PARENT-UNKNOWN
variable TICK-CONTEXT-ORDER
PTR-VARIABLE TICK-CONTEXT-OWNER
variable DEF-TICK-ORDER
PTR-VARIABLE DEF-TICK-OWNER
TYPED-VARIABLE TICK-REMAINDER bool

: TICK-CONTEXT-RESET ( -- )
   ENTRY-TICK-ORDER @ TICK-CONTEXT-ORDER !
   ENTRY-TICK-OWNER @ TICK-CONTEXT-OWNER ! ;

: TICK-CONTEXT-UNKNOWN ( -- )
   TICK-UNKNOWN TICK-CONTEXT-ORDER !
   NULL-PTR TICK-CONTEXT-OWNER ! ;

: TICK-DEF-LATCH ( -- )
   TICK-CONTEXT-ORDER @ DEF-TICK-ORDER !
   TICK-CONTEXT-OWNER @ DEF-TICK-OWNER ! ;

defer COMPOSE-FILE ( ptr u8 n ptr u8 n -- )

: SOURCE@ ( -- ptr u8 )
   SOURCE-A @ ;

: BASE-RESET ( -- )
   1 BASE-LINE !
   1 BASE-COL !
   0 BASE-BYTE ! ;

: SOURCE! ( ptr u8 n -- )
   BASE-RESET
   SOURCE-U !
   SOURCE-A !
   0 TOKEN-BYTE ! ;

: SOURCE-AT! ( ptr u8 n n n n -- ) {: a:ptr u:n line:n col:n byte:n :}
   a u SOURCE!
   line BASE-LINE !
   col BASE-COL !
   byte BASE-BYTE !
   byte TOKEN-BYTE ! ;

: SCAN-RESET ( -- )
   0 SCAN-I ! ;

: SCAN-C@ ( -- n )
   SOURCE@ SCAN-I @ + c@ ;

: SCAN-C+ ( -- n )
   SCAN-C@
   SCAN-I @ 1 + SCAN-I ! ;

: SKIP-WS ( -- )
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 33 < else 0 0= 0= then while
      SCAN-C+ drop
   repeat ;

: SKIP-PAST ( n -- ) {: ch:n :}
   0 FOUND !
   begin SCAN-I @ SOURCE-U @ < while
      SCAN-C+ ch = if -1 FOUND ! exit then
   repeat ;

: NEXT-RAW ( -- ptr u8 n )
   SCAN-I @ {: prev:n :}
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= if prev SOURCE-U @ dup CSR-HIT SOURCE@ 0 exit then
   SCAN-I @ TOKEN-START !
   BASE-BYTE @ SCAN-I @ + TOKEN-BYTE !
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
   prev TOKEN-START @ SCAN-I @ CSR-HIT
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - ;

\ The scan stops at the statement it is in, at its opener: the top-level token
\ it read last.
: STATEMENT-STOP ( n -- )
   {: code:n :}
   TOP-CUR-A @ SOURCE@ - BASE-BYTE @ + TOKEN-BYTE !
   code throw ;

: SC-LEAD? ( n -- bool )
   dup $73 = over $53 = or over $63 = or swap $43 = or ;

: STRING-LEAD? ( n -- bool )
   dup SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $5C <> IF 0 0= 0= EXIT THEN
   a 2 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: STRING-OPENER? ( ptr u8 n -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop 0 0= EXIT THEN
   ESCAPED-STRING-OPENER? ;

: PRINT-OPENER? ( ptr u8 n -- bool )
   s" .(" CORE-STR= ;

: SKIP-ESCAPED-QUOTE ( -- )
   0 FOUND !
   begin SCAN-I @ SOURCE-U @ < while
      SCAN-C+
      dup 92 = if
         drop
         SCAN-I @ SOURCE-U @ < if SCAN-C+ drop then
      else
         34 = if -1 FOUND ! exit then
      then
   repeat ;

\ Skipped top-level string literals feed a two-slot ring so the strings the
\ scanner would otherwise discard reach the rows that replay them: a bare
\ top-level `s" NAME" s" SIG" TRUST` (RECORD-TRUST) and a string loader's path,
\ `s" FILE" included` (COMPOSE-STRING-PATH). A slot holds the bytes the engine's
\ literal makes: a plain literal's source span, an escaped one's decoded payload
\ (RECORD-ESCAPED-STRING). The ring resets per NEXT-SCAN call, so at a TRUST
\ token it holds exactly the two preceding literals from the same statement.
\ The last literal's kind is kept too, 0 when there is none: a quiet
\ composition refuses a string loader by it (QUIET-STRING-LOAD).
: STR-RING-RESET ( -- )
   NULL-PTR STR-PREV-A !  0 STR-PREV-U !
   NULL-PTR STR-LAST-A !  0 STR-LAST-U !  0 STR-LAST-KIND ! ;

: STR-RING-PUSH ( ptr u8 n n -- ) {: a:ptr u:n kind:n :}
   STR-LAST-A @ STR-PREV-A !
   STR-LAST-U @ STR-PREV-U !
   a STR-LAST-A !
   u STR-LAST-U !
   kind STR-LAST-KIND ! ;

\ The kinds of literal discovery tells apart before a loader: an `s"` or `S"`
\ one, plain or escaped, is a path; a `c"`, `C"` or `."` one is no string a
\ loader takes; a body literal holding an escape the engine refuses is the
\ checker's to refuse.
1 constant LIT-PATH
2 constant LIT-OTHER
3 constant LIT-BAD

\ The kind of literal a string opener opens.
: LIT-KIND ( ptr u8 n -- n )
   drop c@ dup $73 = swap $53 = or IF LIT-PATH EXIT THEN
   LIT-OTHER ;

\ A literal the source ends inside stops a quiet composition at its opener, as
\ discovery's lexer stops there (E-DISC-UNTERM).
: QUIET-UNTERM ( -- )
   FOUND @ 0= QUIET @ and IF E-DISC-UNTERM throw THEN ;

\ The payload of the literal just skipped: after its opener and delimiter, pfx
\ bytes, and before the quote SCAN-I stands one past. The length is negative
\ when the source ends before the payload starts.
: SKIPPED-PAYLOAD ( n -- ptr u8 n ) {: pfx:n :}
   SOURCE@ TOKEN-START @ + pfx +
   SCAN-I @ TOKEN-START @ - pfx - 1 - ;

\ A plain literal of the given kind, its payload pfx bytes after its opener.
: RECORD-SKIPPED-STRING ( n n -- ) {: kind:n pfx:n :}
   QUIET-UNTERM
   pfx SKIPPED-PAYLOAD dup 0 < IF 2drop EXIT THEN
   kind STR-RING-PUSH ;

\ An escaped literal's decoded bytes are not in the source, so they live in one
\ of two buffers. STR-DEC-TURN names the one the next decode writes, and a
\ decode that answers its buffer flips it, so the last answer is never in the
\ buffer the next decode writes, or that a reserve for it moves. No reader
\ reads an older answer after the next decode. A top-level literal's answer
\ goes to the ring, which the handler of the token NEXT-SCAN answers reads
\ before the next NEXT-SCAN resets it: only STR-PREV may hold an older answer,
\ and the push after the next decode drops it. Only `trust`, the string
\ loaders and a quiet composition's `UNDEFINE-IF-DEFINED` read the ring, and
\ none of them reads a body. A body literal's answer is
\ BODY-LIT, which the body's next token clears or consumes (PEND-PUSH keeps a
\ copy), and a body token is read with SKIP-STRINGS off, so no decode comes
\ between.
DYNAMIC-BUFFER STR-DEC0 u8
DYNAMIC-BUFFER STR-DEC1 u8
variable STR-DEC-TURN

\ The turn's buffer, with room for u bytes and an address for none.
: STR-DEC-ROOM ( n -- ptr u8 ) {: u:n :}
   STR-DEC-TURN @ 0= IF u 1 + STR-DEC0-RESERVE  0 STR-DEC0 EXIT THEN
   u 1 + STR-DEC1-RESERVE  0 STR-DEC1 ;

\ The bytes the engine's `s\"` makes of an escaped literal's payload, and
\ whether every escape in it is one the engine reads: the payload decoded by
\ the checker's escape table (src/core/checker.f ESC-DECODE), the engine
\ decoder's table (src/habu/habu2.f C-ESC-DECODE-BASIC). An escape is spelt
\ with more bytes than it decodes to, so a decode as long as its payload met no
\ escape: the answer is the source span, and a diagnostic at the literal keeps
\ its position.
: ESC-BYTES ( ptr u8 n -- ptr u8 n bool ) {: src:ptr u:n :}
   u STR-DEC-ROOM {: dst:ptr :}
   src u dst ESC-DECODE {: k:n ok:bool :}
   k u = ok 0= or IF src u ok EXIT THEN
   1 STR-DEC-TURN @ - STR-DEC-TURN !
   dst k ok ;

\ A top-level escaped literal's slot holds its ESC-BYTES. A bad escape is
\ refused at the opener (E-BAD-ESCAPE), where the engine refuses it as a bad
\ string literal; tools/check.f reports it as the throw of the statement
\ there, unless its lexer finds a defect in the file. An unterminated literal
\ is left to the lexer, as a plain one is, except in a quiet composition
\ (QUIET-UNTERM).
: RECORD-ESCAPED-STRING ( n -- ) {: kind:n :}
   QUIET-UNTERM
   FOUND @ 0= IF EXIT THEN
   4 SKIPPED-PAYLOAD ESC-BYTES 0= IF E-BAD-ESCAPE throw THEN
   kind STR-RING-PUSH ;

\ ---- the locals a body declares -----------------------------------------------
\ The engine and the checker look a body token up among the live locals before
\ every keyword but `;` (src/habu/habu2.f EM-COMPILE-LOCAL, src/core/checker.f
\ LOC-REF?), byte for byte, so a local named `char`, `[']`, `s"`, `.(` or
\ `does>` is that local: one ordinary body token. Only the `\` and `(` comments
\ come first, so NEXT looks a body token up between them and `.(`. Only a body
\ reader consults the table, and each resets it first: between bodies it still
\ holds the last body's names, which point into a source that may be gone. A
\ `{: … :}` group reads its names raw (C-LBRACE-PARSE-NAMES), and a name ends at
\ its first `:`. A local lives to the end of the control block that declared it,
\ as the engine's control-flow stack restores the count (LCFPUSH, LCFPOP), and
\ `else` drops the true arm's. An enclosing local is still that local inside a
\ quotation, where the engine and the checker refuse it at that token.
\ lib/source.f BLOCK-OPENER? and BLOCK-CLOSER? hold the same block words for the
\ tools' scanners. Blocks are counted only while a local lives: one opened with
\ none live drops none, and a local only needs the count to change from its own
\ group on.
LOC-RECS TYPED-BUFFER LOCAL-A ptr u8          \ a live local's name
LOC-RECS TYPED-BUFFER LOCAL-U n               \ and its length
LOC-RECS TYPED-BUFFER LOCAL-D n               \ the block depth that declared it
variable LOCAL-N
variable LOCAL-DEPTH                          \ blocks open, counted while a local lives

: LOCALS-RESET ( -- )
   0 LOCAL-N !  0 LOCAL-DEPTH ! ;

: LOCAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LOCAL-N @ 0 ?do
      a u i LOCAL-A @ i LOCAL-U @ CORE-STR= if 0 0= unloop exit then
   loop
   0 0= 0= ;

\ A file whose first two bytes are `#!` names its interpreter on that line, and
\ the loader reads the line as a comment (src/core/include.f SHEBANG-COMMENT):
\ a token at the first byte of scanned bytes that start their file.
: SHEBANG? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   BASE-BYTE @ 0<>  a SOURCE@ <>  or  u 2 <  or IF 0 0= 0= EXIT THEN
   a c@ $23 =  a 1 + c@ $21 =  and ;

\ The byte that ends the comment a token opens, as the loader reads it: the
\ line's end after `\`, `)` after `(`; 0 for any other token.
: COMMENT-END ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 1 <> IF 0 EXIT THEN
   a c@ 92 = IF 10 EXIT THEN
   a c@ 40 = IF 41 EXIT THEN
   0 ;

: NEXT ( -- ptr u8 n )
   BEGIN
      NEXT-RAW
      dup 0= IF EXIT THEN
      2dup SHEBANG? IF 2drop 10 SKIP-PAST ELSE
      2dup COMMENT-END dup 0<> IF nip nip SKIP-PAST ELSE drop
      SKIP-STRINGS @ 0= IF 2dup LOCAL? IF EXIT THEN THEN
      2dup PRINT-OPENER? IF 2drop 41 SKIP-PAST ELSE
      SKIP-STRINGS @ 0= 0= IF
         2dup ESCAPED-STRING-OPENER? IF LIT-KIND SKIP-ESCAPED-QUOTE RECORD-ESCAPED-STRING ELSE
         2dup NORMAL-STRING-OPENER? IF LIT-KIND 34 SKIP-PAST 3 RECORD-SKIPPED-STRING ELSE EXIT THEN THEN
      ELSE EXIT THEN
      THEN THEN THEN
      CSR-SKIPPED
   AGAIN ;

: NEXT-SCAN ( -- ptr u8 n )
   -1 SKIP-STRINGS !
   STR-RING-RESET
   NEXT ;

: NEXT-BODY ( -- ptr u8 n )
   0 SKIP-STRINGS !
   NEXT ;

: RAW! ( -- )
   NEXT-RAW  TOKEN-U !  TOKEN-A ! ;

: BODY! ( -- )
   NEXT-BODY  TOKEN-U !  TOKEN-A ! ;

\ A body buffer that cannot represent its input RAISES; it never truncates.
\
\ This used to skip the one token that would not fit and keep appending the
\ shorter ones after it, on the reasoning that an over-cap body would be caught
\ downstream by the engine's own TDECL-CAP anyway. That reasoning died with the
\ registration-only replay entries: they parse whatever tokens arrive and have no
\ length gate, so a dropped token produces a declaration that is WELL-FORMED and
\ WRONG. Measured on the previous commit, a 1302-variant compact ENUM whose body
\ exceeds BODYBUF-CAP replayed with rc 0 and registered 1142 variants — 160
\ silently missing, and every tag after the first gap shifted, which is exactly
\ the kind of quiet registry divergence the parity suite exists to prevent.
\
\ The code is the declaration layer's own "declaration too long", sumtype.f
\ E-TDECL-CAP read by name, because that is precisely the condition: this
\ source is too long for the path that carries it. Source that trips this bound
\ also trips the engine's TDECL-CAP, so both paths answer the same code.
TYPE-DECL:E-TDECL-CAP constant E-VS-BODY-CAP

\ A body read and never checked as a definition's (a TRUSTED: or type body)
\ drops the cursor it placed.
: BODY-RESET ( -- )
   CSR-STATE @ CSR-PLACED = IF CSR-DONE CSR-STATE ! THEN
   0 BODY-U !
   0 BODY-ROWS ! ;

: BODY-ROW! ( n -- )
   BODY-ROW {: src:n rows:ptr :}
   BODY-U @  BODY-ROWS @ 2 * cells rows + !
   src  BODY-ROWS @ 2 * 1 + cells rows + !
   BODY-ROWS @ 1 + BODY-ROWS ! ;

\ Every run is read out of the source, and its row says where.
: BODY-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   BODY-U @ u + 1 + BODYBUF-CAP > IF E-VS-BODY-CAP throw THEN
   a SOURCE@ - {: src:n :}
   src 0 <  src u + SOURCE-U @ >  or IF s" verify-source: body run outside the source" 74 die THEN
   src CSR-PLACE
   src BODY-ROW!
   0 BEGIN dup u < WHILE
      dup a + c@  BODY-BUF BODY-U @ + c!
      BODY-U @ 1 + BODY-U !
      1 +
   REPEAT drop
   32 BODY-BUF BODY-U @ + c!  BODY-U @ 1 + BODY-U ! ;

\ The body for the checker, with its rows armed for the packets it writes.
: BODY$ ( -- ptr u8 n )
   BODY-BUF BODY-U @ BODY-ROW BODY-ROWS @ DIAG-MAP!
   BODY-BUF BODY-U @ ;

\ The body of the definition the current token closes, its `;` or the `does>`
\ ending a definer's body. The body holds no closer, so the checker is told
\ where it stands (DIAG-CLOSER!) and refuses a structure still open there.
: DEF-BODY$ ( -- ptr u8 n )
   BODY$  TOKEN-A @ TOKEN-U @ DIAG-CLOSER! ;

\ The signature ahead of the scan, read as the engine reads a definition head's
\ (checker.f CHECKER-SIG-SPAN): from its `(` through its `)`, and whether one
\ opens there. The scan passes it. One that never closes stops the statement
\ with the given code.
: SCAN-SIG ( n -- ptr u8 n bool )
   {: unclosed:n :}
   SOURCE@ SCAN-I @ +  SOURCE-U @ SCAN-I @ -
   {: at:ptr left:n :}
   at left CHECKER-SIG-SPAN
   {: open:n close:n :}
   open left = IF at 0 0 0= 0= EXIT THEN
   close 0= IF unclosed STATEMENT-STOP THEN
   SCAN-I @ close + SCAN-I !
   at open +  close open -  0 0= ;

\ A signature that opens here is appended to the body; what it declares,
\ inside the parentheses, is answered, empty for none.
: MAYBE-SIGNATURE ( -- ptr u8 n )
   E-VS-UNTERMINATED-DEFINITION SCAN-SIG 0= IF EXIT THEN
   2dup BODY-APPEND
   {: sig:ptr sigu:n :}
   sig 1 +  sigu 2 - ;

\ The effect inside the parentheses.
: REQUIRE-SIGNATURE ( -- ptr u8 n )
   E-VS-MISSING-SIGNATURE SCAN-SIG 0= IF E-VS-MISSING-SIGNATURE STATEMENT-STOP THEN
   {: sig:ptr sigu:n :}
   sig 1 +  sigu 2 - ;

: STRING-REST ( ptr u8 n -- ptr u8 n ) {: opener:ptr openeru:n :}
   SCAN-I @ {: start:n :}
   opener openeru ESCAPED-STRING-OPENER? IF
      SKIP-ESCAPED-QUOTE
   ELSE
      34 SKIP-PAST
   THEN
   QUIET-UNTERM
   FOUND @ 0= IF E-UNTERMINATED-STRING throw THEN
   SOURCE@ start + SCAN-I @ start - ;

: FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C
      over b + c@ FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

\ The tokens a straight line has none of. The checker's own classifier
\ (src/core/checker.f CF-TOK?) cannot be reused for the question: it is the
\ control-flow DISPATCHER and pushes a frame for every token it recognises, so
\ asking it would move the checker's state. These are its token list, plus the
\ compile-time brackets a definer call (WRAP-TOKEN) or a loader (BODY-TOKEN-SEEN)
\ must not hide behind. Each matches case-folded, as the checker reads a token
\ after TOKFOLD and the engine's keywords ignore case, so `EXIT` ends the line
\ as `exit` does.
: WRAP-COND-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI
   a u s" else" STR=CI or
   a u s" then" STR=CI or
   a u s" case" STR=CI or
   a u s" of" STR=CI or
   a u s" endof" STR=CI or
   a u s" endcase" STR=CI or ;

: WRAP-LOOP-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" begin" STR=CI
   a u s" while" STR=CI or
   a u s" repeat" STR=CI or
   a u s" until" STR=CI or
   a u s" again" STR=CI or
   a u s" do" STR=CI or
   a u s" ?do" STR=CI or
   a u s" loop" STR=CI or
   a u s" +loop" STR=CI or
   a u s" leave" STR=CI or
   a u s" exit" STR=CI or ;

: WRAP-BRACKET-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" STR=CI
   a u s" ;]" STR=CI or
   a u s" [" STR=CI or
   a u s" ]" STR=CI or
   a u s" postpone" STR=CI or
   a u s" recurse" STR=CI or ;

: WRAP-CTL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u WRAP-COND-TOK?
   a u WRAP-LOOP-TOK? or
   a u WRAP-BRACKET-TOK? or ;

\ ---- a quiet composition ------------------------------------------------------
\ A quiet composition (SOURCE-COMPOSE-QUIET-IN-SCOPE) refuses the loader forms
\ discovery refuses (tools/source-discovery.f), where it refuses them, so that
\ walk need not run before it: a loader whose path is no literal
\ (E-DISC-DYNAMIC), a literal no loader takes (E-DISC-OPENER), a path past
\ PATH-CAP (E-DISC-CAPACITY), a declaration of a loader's name (E-DISC-SHADOW)
\ and a retirement of one, or of a word it cannot read (E-DISC-RETIRE). A
\ refusal stands at TOKEN-BYTE@, FAULT-LEN bytes long, unless the file being
\ scanned is lenient (LENIENT-FILE?): such a file keeps the form, and a loader
\ it would refuse loads nothing.
: LENIENT? ( -- bool )
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ LENIENT-FILE? ;

: QUIET-REJECT ( n n -- ) {: code:n len:n :}
   LENIENT? IF EXIT THEN
   len FAULT-LEN !
   code throw ;

\ The loader names discovery reserves (SD-RESERVED$?); `script-required` is
\ not one.
: RESERVED-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" STR=CI
   a u s" included" STR=CI or
   a u s" require" STR=CI or
   a u s" required" STR=CI or
   a u s" provided" STR=CI or ;

\ The name a declaration has just read, where it stands: a loader's is refused.
: QUIET-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   QUIET @ 0= IF EXIT THEN
   a u RESERVED-NAME? IF E-DISC-SHADOW u QUIET-REJECT THEN ;

\ `UNDEFINE-IF-DEFINED` (src/habu/xref.f) retires the word the literal before
\ it names, a literal of the given kind: one that is no path literal, or that
\ names a loader, is refused; one holding a bad escape is the checker's.
: QUIET-RETIRE ( ptr u8 n n ptr u8 n -- ) {: a:ptr u:n kind:n lit:ptr litu:n :}
   QUIET @ 0= IF EXIT THEN
   a u s" UNDEFINE-IF-DEFINED" STR=CI 0= IF EXIT THEN
   kind LIT-BAD = IF EXIT THEN
   kind LIT-PATH <>  lit litu RESERVED-NAME?  or IF E-DISC-RETIRE u QUIET-REJECT THEN ;

\ A string loader u bytes long, after a literal of the given kind and length:
\ refused after none, after one no loader takes and after a path past
\ PATH-CAP. True when it may load the literal's path.
: LITERAL-LOAD? ( n n n -- bool ) {: kind:n litu:n u:n :}
   kind 0= IF E-DISC-DYNAMIC u QUIET-REJECT false EXIT THEN
   kind LIT-OTHER = IF E-DISC-OPENER u QUIET-REJECT false EXIT THEN
   kind LIT-PATH <> IF false EXIT THEN
   litu PATH-CAP > IF E-DISC-CAPACITY u QUIET-REJECT false EXIT THEN
   true ;

\ ---- a loader in a definition -------------------------------------------------
\ `s" PATH" required` or `s" PATH" included` in a definition loads PATH when the
\ word runs, after the definition at the earliest. A loader the body reaches on
\ a straight line, with no control word, quotation or bracket before it
\ (WRAP-CTL-TOK?), loads whenever the word runs, so the composition loads its
\ file at the first top-level statement boundary after the definition where the
\ file being read has closed every package and file-level `using` it opened, in
\ the order of the loaders, and at the end of that file at the latest
\ (VERIFY-SOURCE, PEND-RELEASE). A loader under a condition may not run - a
\ target's layout loads only when the image lacks one (tools/imgdump.f) - so
\ its file is the run's to load. A target predicate right before an `if` is no
\ such condition: the engine answers HB-TARGET-LINUX?, HB-TARGET-MACOS? and
\ HB-TARGET-LINUX-X86-64? one way, so the arm its answer runs stays on the
\ straight line and the other arm never runs (ARM-TOKEN?, DEAD-TOKEN).
\ tools/object-image.f loads its target's sys.f that way, and the three
\ targets' files define the same words. A source cannot define a predicate's
\ spelling (tools/reserved-name-lint-core.f reserves it), so the answer is the
\ engine's. The path is the literal right before the loader word, the bytes the
\ engine's literal makes of it (BODY-LIT!); any other loader form in a body is
\ refused by discovery (tools/source-discovery.f) or by a quiet composition. An
\ entry keeps a copy of its path: a decoded one lives only until the decode
\ after next, and the release comes after later bodies, top-level statements
\ and nested files have decoded theirs. A file's entries sit above its
\ loader's, and their paths go with them. The entries grow with the loads
\ waiting at once.
\
\ A quiet composition loads none of these: a word's run is no part of the
\ check, so its string loaders, called or not, are only refused where
\ discovery refuses them (QUIET-BODY). What it loads from a body is what the
\ engine loads while it compiles one: `include` and `require` are immediate
\ (src/core/include.f), so the file each names in a body waits here as a
\ string loader's does in an ordinary composition, and a fault loading it
\ stands at that loader word (IMM-OPERAND, CLAIMED-LOAD).
DYNAMIC-BUFFER PEND-PATH u8                   \ the waiting loads' paths, end to end
DYNAMIC-BUFFER PEND-AT n                      \ a waiting load's path's offset
DYNAMIC-BUFFER PEND-U n                       \ and its length
DYNAMIC-BUFFER PEND-INC n                     \ 1 to include its file, 0 to require it
DYNAMIC-BUFFER PEND-WORD-AT n                 \ where its loader word starts
DYNAMIC-BUFFER PEND-WORD-U n                  \ and that word's length
variable PEND-N
variable PEND-BASE                            \ the first entry of the file being read
PTR-VARIABLE BODY-LIT-A  variable BODY-LIT-U  \ the literal the body token before closed, or 0
variable BODY-LIT-KIND                        \ its kind (LIT-KIND or LIT-BAD), 0 for none
variable BODY-BENT                            \ the body read so far is no straight line
0 constant GUARD-NONE                         \ the body token before is no target predicate
1 constant GUARD-LIVE                         \ it is one the engine answers true
2 constant GUARD-DEAD                         \ it is one the engine answers false
variable BODY-GUARD                           \ which of the three
variable BODY-ARMS                            \ the target `if`s whose running arm the line is in
variable BODY-DEAD                            \ in an arm that never runs: 1 + the `if`s open in it

\ Neither a literal nor a target predicate is right before the next body token.
: BODY-PREV-CLEAR ( -- )
   0 BODY-LIT-U !  0 BODY-LIT-KIND !  GUARD-NONE BODY-GUARD ! ;

: BODY-LOAD-RESET ( -- )
   0 BODY-BENT !  BODY-PREV-CLEAR  0 BODY-ARMS !  0 BODY-DEAD ! ;

\ The end of the waiting entries' paths.
: PEND-END ( -- n )
   PEND-N @ 0= IF 0 EXIT THEN
   PEND-N @ 1 - {: last:n :}
   last PEND-AT @ last PEND-U @ + ;

\ A load waiting for the path at a, u bytes, to include (inc 1) or require,
\ its loader word wu bytes from byte w.
: PEND-PUSH ( ptr u8 n n n n -- ) {: a:ptr u:n inc:n w:n wu:n :}
   PEND-N @ PEND-END
   {: at:n off:n :}
   off u + 1 + PEND-PATH-RESERVE
   a off PEND-PATH u BYTE-COPY
   at 1 + PEND-AT-RESERVE  at 1 + PEND-U-RESERVE  at 1 + PEND-INC-RESERVE
   at 1 + PEND-WORD-AT-RESERVE  at 1 + PEND-WORD-U-RESERVE
   off at PEND-AT !  u at PEND-U !  inc at PEND-INC !
   w at PEND-WORD-AT !  wu at PEND-WORD-U !
   at 1 + PEND-N ! ;

\ The text of a literal from the rest STRING-REST read after its opener, a
\ string opener: past the one delimiting space, short of the closing quote, as
\ the engine's literal makes it, decoded when the opener is an escaped one
\ (ESC-BYTES), and its kind. A literal holding a bad escape leaves no text, of
\ kind LIT-BAD: the checker refuses its body.
: BODY-LIT! ( ptr u8 n ptr u8 n -- ) {: o:ptr ou:n s:ptr su:n :}
   BODY-PREV-CLEAR
   su 2 < IF EXIT THEN
   s 1 + su 2 -
   o ou NORMAL-STRING-OPENER? 0= IF
      ESC-BYTES 0= IF 2drop LIT-BAD BODY-LIT-KIND ! EXIT THEN
   THEN
   BODY-LIT-U !  BODY-LIT-A !
   o ou LIT-KIND BODY-LIT-KIND ! ;

: >GUARD ( bool -- n )
   IF GUARD-LIVE EXIT THEN GUARD-DEAD ;

\ What a body token tells an `if` right after it.
: TARGET-GUARD ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" HB-TARGET-LINUX?" STR=CI IF HB-TARGET-LINUX? >GUARD EXIT THEN
   a u s" HB-TARGET-MACOS?" STR=CI IF HB-TARGET-MACOS? >GUARD EXIT THEN
   a u s" HB-TARGET-LINUX-X86-64?" STR=CI IF
      HB-TARGET-LINUX-X86-64? >GUARD EXIT
   THEN
   GUARD-NONE ;

\ `if`, `else` and `then` on the straight line, true when the token is one. A
\ target predicate right before an `if` opens a target `if`, whose running arm
\ stays on the line and whose other arm is read only for its end (DEAD-TOKEN).
\ Any other `if`, and an `else` or `then` no target `if` opened, ends the line.
\ The three match case-folded, as the engine's keywords do.
: ARM-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI IF
      BODY-GUARD @ GUARD-NONE = IF 1 BODY-BENT ! 0 0= EXIT THEN
      BODY-ARMS @ 1 + BODY-ARMS !
      BODY-GUARD @ GUARD-DEAD = IF 1 BODY-DEAD ! THEN
      0 0= EXIT
   THEN
   a u s" else" STR=CI a u s" then" STR=CI or 0= IF 0 0= 0= EXIT THEN
   BODY-ARMS @ 0= IF 1 BODY-BENT ! 0 0= EXIT THEN
   a u s" else" STR=CI IF 1 BODY-DEAD ! 0 0= EXIT THEN
   BODY-ARMS @ 1 - BODY-ARMS !
   0 0= ;

\ A token in an arm that never runs, read only for the arm's end: the `else`
\ of its own target `if` starts the arm that runs, and its `then` closes it.
: DEAD-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" if" STR=CI IF BODY-DEAD @ 1 + BODY-DEAD ! EXIT THEN
   a u s" else" STR=CI BODY-DEAD @ 1 = and IF 0 BODY-DEAD ! EXIT THEN
   a u s" then" STR=CI 0= IF EXIT THEN
   BODY-DEAD @ 1 - BODY-DEAD !
   BODY-DEAD @ 0= IF BODY-ARMS @ 1 - BODY-ARMS ! THEN ;

\ A body token a quiet composition refuses where discovery refuses it, by the
\ literal before it, in any arm and whether the body runs or not: a string
\ loader word, whose file it never loads, and `UNDEFINE-IF-DEFINED`.
: QUIET-BODY ( ptr u8 n -- ) {: a:ptr u:n :}
   QUIET @ 0= IF EXIT THEN
   a u s" included" STR=CI  a u s" required" STR=CI or  a u s" provided" STR=CI or IF
      BODY-LIT-KIND @ BODY-LIT-U @ u LITERAL-LOAD? drop EXIT
   THEN
   a u BODY-LIT-KIND @ BODY-LIT-A @ BODY-LIT-U @ QUIET-RETIRE ;

\ A body token that is no string literal: a loader word right after one, on a
\ straight line, waits while an ordinary composition reads the source.
: BODY-TOKEN-SEEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u QUIET-BODY
   BODY-DEAD @ 0 > IF a u DEAD-TOKEN BODY-PREV-CLEAR EXIT THEN
   BODY-BENT @ 0= IF a u ARM-TOKEN? IF BODY-PREV-CLEAR EXIT THEN THEN
   a u WRAP-CTL-TOK? IF 1 BODY-BENT ! THEN
   COMPOSE-ON @ 0<> BODY-LIT-U @ 0 > and BODY-BENT @ 0= and QUIET @ 0= and IF
      a u s" required" STR=CI IF BODY-LIT-A @ BODY-LIT-U @ 0 TOKEN-BYTE @ u PEND-PUSH THEN
      a u s" included" STR=CI IF BODY-LIT-A @ BODY-LIT-U @ 1 TOKEN-BYTE @ u PEND-PUSH THEN
   THEN
   BODY-PREV-CLEAR
   a u TARGET-GUARD BODY-GUARD ! ;

: APPEND-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BODY-APPEND
   a u STRING-REST {: s:ptr su:n :}
   s su BODY-APPEND
   a u s su BODY-LIT! ;

: SKIP-STRING-REST ( ptr u8 n -- ) {: a:ptr u:n :}
   a u a u STRING-REST BODY-LIT! ;

\ ---- the token a parsing keyword takes ----------------------------------------
\ A parsing keyword takes the next whitespace-delimited token as its operand, by
\ parse-name's rule, so a `:`, a definer, a digit, a `\`, a `(` or a string
\ opener there is data and never a token of the program. At top level the
\ keywords are the engine's interpret-state ones (src/habu/habu2.f
\ EM-INTERPRET-DEFINE-KEYWORDS, C-TICK and C-CHAR); in a body they are the ones
\ the checker's body reader takes (src/core/checker.f PARSE-LIT? and
\ BTICK-CAND?). `char` is both. Each matches case-folded and ahead of any
\ dictionary lookup, as the engine's keyword compare (LKWCMP) and the checker's
\ TOKFOLD do, so no definition can shadow one; in a body a live local is looked
\ up first (LOCAL? above).
\
\ A dictionary word that parses is not one of these. Which word `require`
\ (src/core/include.f) or `SEE` (src/habu/xref.f) names is a scope question,
\ and the tree defines words that take no operand under both spellings
\ (test/native-unit-compile-e2e.f, test/compiler/tic6x-facts.f), so the token
\ after one is read as an ordinary token.
: CHAR-KEYWORD? ( ptr u8 n -- bool )
   s" char" STR=CI ;

: TOP-PARSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHAR-KEYWORD? IF 0 0= EXIT THEN
   a u s" '" STR=CI ;

: BODY-PARSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHAR-KEYWORD? IF 0 0= EXIT THEN
   a u s" [']" STR=CI IF 0 0= EXIT THEN
   a u s" [char]" STR=CI ;

: OPERAND ( -- ptr u8 n )
   NEXT-RAW dup 0= IF E-MISSING-NAME throw THEN ;

\ A definer takes its name by the same rule: the engine reads it with LTOK or
\ parse-name, so `: \`, `DEFLINEAR (` and `create s"` name a word `\`, a type
\ `(` (which the registration then refuses) and a word `s"`. A comment or string
\ rule there would hand the definer a later token instead. With no token left
\ TOKEN-BYTE stays at the definer, and a caller throws E-MISSING-NAME there;
\ NEWTYPE and SUMTYPE leave the refusal to their declaration packet.
: NAME-TOKEN ( -- ptr u8 n )
   NEXT-RAW ;

\ ---- recording a body's locals ------------------------------------------------
: LOCAL-TOKEN? ( -- bool )
   TOKEN-A @ TOKEN-U @ LOCAL? ;

\ The engine refuses a live local past its LOC-RECS records, and the checker
\ names that local (E-TOO-MANY-LOCALS) when this body registers, so the table
\ only stops recording. A dropped name spelled as a parsing keyword or string
\ opener and then used still hides the `;`: with nothing after it to close the
\ scan the scan stops at the string or the definition the source ends inside,
\ else the merged body registers and the checker names the local; the loader
\ refuses that body at the name.
: LOCAL-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   LOCAL-N @ LOC-RECS >= IF EXIT THEN
   0 BEGIN dup u < IF a over + c@ $3A <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   LOCAL-N @ LOCAL-U !
   a LOCAL-N @ LOCAL-A !
   LOCAL-DEPTH @ LOCAL-N @ LOCAL-D !
   LOCAL-N @ 1 + LOCAL-N ! ;

: BLOCK-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI
   a u s" begin" STR=CI or
   a u s" do" STR=CI or
   a u s" ?do" STR=CI or
   a u s" case" STR=CI or
   a u s" of" STR=CI or
   a u s" match" STR=CI or
   a u s" [:" CORE-STR= or ;

: BLOCK-CLOSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" then" STR=CI
   a u s" until" STR=CI or
   a u s" repeat" STR=CI or
   a u s" again" STR=CI or
   a u s" loop" STR=CI or
   a u s" +loop" STR=CI or
   a u s" endof" STR=CI or
   a u s" endcase" STR=CI or
   a u s" ;match" STR=CI or
   a u s" ;]" CORE-STR= or ;

\ Drop the locals the innermost open block declared.
: BLOCK-DROP ( -- )
   BEGIN
      LOCAL-N @ 0 > IF LOCAL-N @ 1 - LOCAL-D @ LOCAL-DEPTH @ >= ELSE 0 0= 0= THEN
   WHILE
      LOCAL-N @ 1 - LOCAL-N !
   REPEAT ;

: BLOCK-STEP ( ptr u8 n -- ) {: a:ptr u:n :}
   LOCAL-N @ 0= IF EXIT THEN
   a u BLOCK-OPENER? IF LOCAL-DEPTH @ 1 + LOCAL-DEPTH ! EXIT THEN
   a u s" else" STR=CI IF BLOCK-DROP EXIT THEN
   a u BLOCK-CLOSER? IF BLOCK-DROP LOCAL-DEPTH @ 1 - LOCAL-DEPTH ! THEN ;

\ One token of a group: its closer ends it, and any other token is a name.
: GROUP-TOKEN ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" :}" CORE-STR= IF 0 0= EXIT THEN
   a u LOCAL-ADD
   0 0= 0= ;

\ The byte where the group being read opens, at its `{:`.
variable GROUP-AT

\ The next token of a group. The source ending inside one stops the statement
\ it is in, and a quiet composition at the group's `{:`, as discovery's lexer
\ stops there (E-DISC-UNTERM).
: GROUP-NEXT ( -- ptr u8 n )
   NEXT-RAW dup 0= IF
      QUIET @ IF GROUP-AT @ TOKEN-BYTE !  E-DISC-UNTERM throw THEN
      E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP
   THEN ;

: APPEND-GROUP ( -- )
   BEGIN GROUP-NEXT 2dup BODY-APPEND GROUP-TOKEN UNTIL ;

: SKIP-GROUP ( -- )
   BEGIN GROUP-NEXT GROUP-TOKEN UNTIL ;

\ The path `include` or `require` takes, the next token as the engine reads it
\ (parse-name), the word wu bytes long and standing at TOKEN-BYTE: none is
\ refused at the word, one past PATH-CAP at itself, and true when the path may
\ load. A lenient file keeps the form and loads nothing.
: LOADER-OPERAND ( n -- ptr u8 n bool ) {: wu:n :}
   NEXT-RAW {: p:ptr pu:n :}
   pu 0= IF E-DISC-DYNAMIC wu QUIET-REJECT p pu false EXIT THEN
   pu PATH-CAP > IF E-DISC-CAPACITY pu QUIET-REJECT p pu false EXIT THEN
   p pu true ;

\ `include` or `require` in a body, which the engine runs while it compiles the
\ body (src/core/include.f): a quiet composition loads its file (PEND-PUSH).
: BODY-LOADER-IMM? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   QUIET @ 0= IF false EXIT THEN
   a u s" include" STR=CI  a u s" require" STR=CI or ;

\ The path the body's immediate loader word, the current token, takes: the
\ file waits for the statement's end.
: IMM-OPERAND ( -- ptr u8 n )
   TOKEN-A @ TOKEN-U @ TOKEN-BYTE @ {: w:ptr wu:n at:n :}
   wu LOADER-OPERAND {: p:ptr pu:n load:bool :}
   load IF
      p pu  w wu s" include" STR=CI IF 1 ELSE 0 THEN  at wu PEND-PUSH
   THEN
   p pu ;

\ A local, a group and a block word in a body, ahead of the parsing keywords and
\ string openers the body token readers below take. A local or a group skips
\ BODY-TOKEN-SEEN, so it clears what the token before it told an `if`: a local,
\ even one spelled as a target predicate, opens no target `if`. An immediate
\ loader's path joins the body as its token, which the engine's loader takes.
: APPEND-BODY-TOKEN ( -- )
   LOCAL-TOKEN? IF
      BODY-PREV-CLEAR
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ s" {:" CORE-STR= IF
      BODY-PREV-CLEAR
      TOKEN-BYTE @ GROUP-AT !
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      APPEND-GROUP
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ BLOCK-STEP
   TOKEN-A @ TOKEN-U @ BODY-PARSER? IF
      BODY-PREV-CLEAR
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      OPERAND BODY-APPEND
      exit
   THEN
   TOKEN-A @ TOKEN-U @ BODY-LOADER-IMM? IF
      TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      IMM-OPERAND dup 0 > IF BODY-APPEND ELSE 2drop THEN
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN
      TOKEN-A @ TOKEN-U @ BODY-APPEND
   THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ BODY-PARSER? IF OPERAND 2drop BODY-PREV-CLEAR exit THEN
   TOKEN-A @ TOKEN-U @ BODY-LOADER-IMM? IF
      TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN  IMM-OPERAND 2drop EXIT
   THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST exit THEN
   TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN ;

\ SKIP-BODY-TOKEN for a definition body, which declares locals; a primitive row
\ declares none.
: SKIP-DEF-TOKEN ( -- )
   LOCAL-TOKEN? IF BODY-PREV-CLEAR EXIT THEN
   TOKEN-A @ TOKEN-U @ s" {:" CORE-STR= IF
      BODY-PREV-CLEAR  TOKEN-BYTE @ GROUP-AT !  SKIP-GROUP EXIT
   THEN
   TOKEN-A @ TOKEN-U @ BLOCK-STEP
   SKIP-BODY-TOKEN ;

: MULTI-ERR-MODE? ( -- bool ) MULTI-ERR @ 0<> ;

\ Verifier trust rows below cover recursive checker entrypoints, checker-owned
\ mode state, dynamic signature publication, raw-definer mode, and the scope's
\ own name resolution.
\ Retirement: habu-sweep-trusted-out-41e973ce.
\ An uncheckable verdict is rendered here unless CHECK rendered it: as JSON, or
\ in a multi-error load.
TRUSTED: CHECK-BODY ( ptr u8 n -- n )
   CHECK! dup 1 = JSON-DIAGS @ 0= and MULTI-ERR-MODE? 0= and DIAG-QUIET @ 0= and
   IF DIAGXT THEN ;

\ The scope's two questions about a name, asked of the checker that owns the
\ scope: RECORD-SYM? names the symbol a definition in THIS source is recorded
\ under (0 when it never was) and FIND-SYM resolves a USE through the open
\ package's private and public wordlists, the global wordlist and the used
\ publics - the same chain a body token resolves through, so a qualified
\ `CODEGEN:BUFFER-E` and a bare `BUFFER-E` under `using CODEGEN` answer one
\ symbol.
\ Replay-only checker queries use the live declaration owner. The verifier is
\ source loaded, so the sealed image need not publish these checker names.
: OWNER-XT ( n -- n ) {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF E-NCOMP-OWNER throw THEN ;

\ The owner record stores raw execution tokens; each view states the signature
\ of the field it calls.
CAST: SYM-ACTION ( n -- [ ptr u8 n -- n ] )
CAST: IDENTITY-ACTION ( n -- [ n -- ptr u8 n ptr u8 n n ] )
CAST: CREATES-ACTION ( n -- [ n -- n ] )
CAST: CREATED-ACTION ( n -- [ ptr u8 n n -- bool ] )
CAST: DOES-ACTION ( n -- [ ptr u8 n ptr u8 n ptr u8 n -- n ] )
CAST: RENDERS-ACTION ( n -- [ ptr u8 n n -- bool ] )
CAST: REACH-ACTION ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: CLAUSE-ACTION ( n -- [ ptr u8 n -- ] )
CAST: DECL-ACTION ( n -- [ ptr u8 n ptr u8 n -- bool ] )
CAST: TICK-ORDER-ACTION ( n -- [ ptr u8 n n ptr u8 [ -- ] -- n ] )

: WITH-BODY-TICK-ORDER ( ptr u8 n [ -- ] -- bool )
   {: a:ptr u:n q :}
   a u DEF-TICK-ORDER @ DEF-TICK-OWNER @ q
   NCOMP-DISPATCH:DECL-WITH-TICK-ORDER-OFF OWNER-XT TICK-ORDER-ACTION execute 0<> ;

CAST: VERIFIER-ACTION ( n -- [ -- ] )
CAST: ARM-ACTION ( n -- [ ptr u8 n n n n -- ] )
CAST: USES-ACTION ( n -- [ [ n n n n n -- ] [ -- ] -- ] )

\ ---- navigation: where a declaration was written, what a use bound ----------
\ Each file the composition scans is a visit, numbered from one counter that
\ never goes back: no number names two files in one process, so a declaration
\ an earlier composition located names no file of this one. A visit keeps a
\ copy of the path its file was resolved to, because the loader releases its
\ own when the file ends while a use in the subject may bind a declaration in
\ a file whose visit has ended. The composition's first visit is the
\ subject's. The copies go when the composition ends.
variable VISIT-NEXT                     \ the last visit number given
variable VISIT-CUR                      \ the file being scanned, 0 outside a visit
variable VISIT-FIRST                    \ this composition's first visit
variable VISIT-N                        \ the visits this composition made
DYNAMIC-BUFFER VISIT-PATHS u8           \ their paths, end to end
variable VISIT-PATHS-U
DYNAMIC-BUFFER VISIT-ROWS n             \ per visit, its path's offset and length

\ Open a visit for the file resolved to PATH and make it the current one.
: VISIT-OPEN ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   VISIT-PATHS-U @ {: off:n :}
   VISIT-N @ 0= IF VISIT-NEXT @ 1 + VISIT-FIRST ! THEN
   off pathu + 1 + VISIT-PATHS-RESERVE
   path off VISIT-PATHS pathu BYTE-COPY
   off pathu + VISIT-PATHS-U !
   VISIT-N @ 1 + 2 * VISIT-ROWS-RESERVE
   off VISIT-N @ 2 * VISIT-ROWS !
   pathu VISIT-N @ 2 * 1 + VISIT-ROWS !
   VISIT-N @ 1 + VISIT-N !
   VISIT-NEXT @ 1 + VISIT-NEXT !
   VISIT-NEXT @ VISIT-CUR ! ;

\ Did this composition make visit V?
: VISIT-IN? ( n -- bool )
   VISIT-FIRST @ - {: row:n :}
   row 0 >= row VISIT-N @ < and ;

\ The path visit V's file was resolved to, for a visit this composition made.
: VISIT-PATH ( n -- ptr u8 n )
   VISIT-FIRST @ - 2 * {: row:n :}
   row VISIT-ROWS @ VISIT-PATHS  row 1 + VISIT-ROWS @ ;

\ The composition's end: its visits and their paths go.
: VISITS-DROP ( -- )
   VISIT-PATHS-RELEASE  VISIT-ROWS-RELEASE
   0 VISIT-PATHS-U !  0 VISIT-N !  0 VISIT-CUR ! ;

\ Arm the checker with NAME, declared by the token from byte AT, U long, in the
\ file being scanned: the record the next named registrar retains takes that
\ spelling and location
\ (src/core/checker.f CHECKER-DECL-AT!), until DISARM. Outside a visit there
\ is no file to name, and the checker's armed state stays its caller's.
: ARM ( ptr u8 n n n -- )
   {: name:ptr nameu:n at:n u:n :}
   VISIT-CUR @ 0= IF EXIT THEN
   name nameu VISIT-CUR @ at at u +
   NCOMP-DISPATCH:DECL-VERIFY-DECL-ARM-OFF OWNER-XT ARM-ACTION execute ;

: DISARM ( -- )
   VISIT-CUR @ 0= IF EXIT THEN
   NCOMP-DISPATCH:DECL-VERIFY-DECL-DISARM-OFF OWNER-XT VERIFIER-ACTION execute ;

public

\ A spelling the cursor's place offers (CURSOR!), and where the declaration it
\ binds was written: the path its file was resolved to when this composition
\ visited it, and where its declaring token starts and ends there; an empty
\ path and 0 0 for a declaration in no file the composition visited, or with no
\ location. The strings are borrowed: a word installed here consumes them
\ before it returns.
defer ON-CANDIDATE ( ptr u8 n ptr u8 n n n -- )

private

: CANDIDATE-NONE ( ptr u8 n ptr u8 n n n -- )
   2drop 2drop 2drop ;

: CANDIDATE-INIT ( -- )
   ['] CANDIDATE-NONE is ON-CANDIDATE ;

CANDIDATE-INIT

CAST: CURSOR-ACTION ( n -- [ n [ ptr u8 n n n n -- ] -- ] )
CAST: VISIBLE-ACTION ( n -- [ ptr u8 n n [ ptr u8 n n n n -- ] -- ] )

\ The checker's candidate (src/core/checker.f CHECKER-CURSOR!, EACH-VISIBLE):
\ its spelling and the declaration's visit and span, 0 0 0 for none.
: CSR-VISIT ( ptr u8 n n n n -- )
   {: a:ptr u:n v:n s:n e:n :}
   v VISIT-IN? IF a u v VISIT-PATH s e ON-CANDIDATE EXIT THEN
   a u NULL$ 0 0 ON-CANDIDATE ;

\ The definition body of U bytes about to be checked holds the cursor: arm the
\ check at it, its place or the body's end when it waits before the closer,
\ and answer whether it was armed. One arm per composition, so a does>
\ parent checked twice offers once.
: CSR-ARM ( n -- bool )
   {: u:n :}
   CSR-SUBJ @ 0= IF false EXIT THEN
   CSR-STATE @ CSR-PLACED =  CSR-STATE @ CSR-SPACE =  or 0= IF false EXIT THEN
   CSR-STATE @ CSR-PLACED = IF CSR-BODY @ ELSE u THEN
   CSR-DONE CSR-STATE !
   ['] CSR-VISIT CHECKER-OWNER-ABI:VERIFY-CURSOR-OFF OWNER-XT CURSOR-ACTION execute
   true ;

\ After the armed check, on either exit; the check that took the arm released
\ it already.
: CSR-DISARM ( -- )
   -1 ['] CSR-VISIT CHECKER-OWNER-ABI:VERIFY-CURSOR-OFF OWNER-XT CURSOR-ACTION execute ;

\ Run Q, the check of a definition body of U bytes, armed at the cursor the
\ body holds immediately before it, so that no other check (a TRUSTED: body's)
\ takes the arm, and disarmed after it on either exit.
: CSR-CHECK ( n [ -- ] -- )
   {: u:n q :}
   u CSR-ARM 0= IF q execute EXIT THEN
   q catch {: rc:n :}
   CSR-DISARM
   rc 0<> IF rc throw THEN ;

\ Run Q with H receiving each use Q's checks bind (src/core/checker.f
\ CHECKER-WITH-USES): one scope at a time, closed on either exit.
: WITH-USES ( [ n n n n n -- ] [ -- ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-USES-OFF OWNER-XT USES-ACTION execute ;

\ The checked dispatchers name their offsets through layout.f's
\ NCOMP-DISPATCH:DECL-VERIFY-* mirrors. CHECKER-OWNER-ABI loads before the
\ checker, so a source boot has no checked row for its constants.
: RECORD-SYM? ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-RECORD-SYM-OFF OWNER-XT SYM-ACTION execute ;
\ FIND-SYM is the QUIET resolver: this scan asks it of tokens it is only
\ classifying, and the two refusals the authoritative resolver owns (a used
\ public shadowing a global, a tail two used packages both export) belong to the
\ definition's own check, which resolves the same token straight after.
: FIND-SYM ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-FIND-SYM-OFF OWNER-XT SYM-ACTION execute ;

public

\ The selected symbol's package, tail and visibility are borrowed from its
\ checker owner until that owner next interns a symbol.
: SYM-IDENTITY ( n -- ptr u8 n ptr u8 n n )
   NCOMP-DISPATCH:DECL-VERIFY-SYM-IDENTITY-OFF OWNER-XT IDENTITY-ACTION execute ;

private

\ The same two questions about a definer this pre-pass never read - one compiled
\ in THIS process, whose clause the checker certified at its `;` and whose
\ created-word effect it kept (src/core/checker.f DOES-EFF-LATCH! and the NORETS
\ entry's CREATES cell), or a straight-line wrapper of such a definer, which the
\ checker's own body walk records against the wrapper's symbol the same way. The
\ effect stays where the checker built it: it is handed over as a record, not as
\ text, so the type variables the clause declared keep the raw-definer seal the
\ engine's own `trust-raw` gives them.
: CREATES-SYM? ( n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-CREATES-SYM-OFF OWNER-XT CREATES-ACTION execute ;
: RECORD-CREATED ( ptr u8 n n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RECORD-CREATED-OFF OWNER-XT CREATED-ACTION execute ;

\ Does the token run a word that may define words no source text spells, by one
\ of the facts MASK names (src/core/checker.f CTL-RENDERS, CTL-CREATES,
\ CHECKER-VERIFY-RENDERS)? When it does, the checker marks the wordlist the
\ statement runs in, and a definition or a top-level token read after it that
\ names a word nothing in scope defines is left to the run (CHECK's verdict 2,
\ or a stretch deferred at the token) instead of refused E-UNDEFINED.
: RENDERS-MARK? ( ptr u8 n n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RENDERS-OFF OWNER-XT RENDERS-ACTION execute ;

\ The two masks it is asked with: a text renderer (RECORD-DEFINER?), and a text
\ renderer or a word that calls `create` (TOP-TOKEN). They are bound at top
\ level because the checker loads before the check hook, so a source boot has
\ no checked row for its constants (test/cold-naming-test.f).
CTL-RENDERS constant MARK-RENDERS
CTL-RENDERS CTL-CREATES or constant MARK-UNSEEN
CTL-RENDERS CTL-NOMINAL or constant MARK-NOMINAL

\ A call in the body of the TRUSTED: word NAME: the checker adds what the word
\ the token names may do when it runs to what NAME may do
\ (src/core/checker.f CHECKER-VERIFY-REACH).
: TRUSTED-REACH ( ptr u8 n ptr u8 n -- )
   NCOMP-DISPATCH:DECL-VERIFY-REACH-OFF OWNER-XT REACH-ACTION execute ;

\ The does> clause record of the TRUSTED: definer NAME, published as the
\ engine's `does>` makes it (src/core/checker.f CHECKER-SOURCE-CLAUSE).
: TRUSTED-CLAUSE ( ptr u8 n -- )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-CLAUSE-OFF OWNER-XT CLAUSE-ACTION execute ;

\ ---- the definers this pre-pass learns from the sources it reads -------------
\ A `create … does>` definition IS a definer, and the effect of every word it
\ creates is the clause's declared one - a `TRUSTED:` definer included, whose
\ body is asserted but whose clause is still a declaration (SCAN-TRUSTED-BODY).
\ That is the row the ENGINE publishes for such a word at run time:
\ src/habu/habu2.f DOESPATCH:EMIT hands the parsed
\ clause signature (CRSIG) to LASTC-TRUST:PUBLISH, which registers it through
\ the checker's `trust-raw` (src/core/checker.f TRUST-RAW) - the raw-variable
\ seal RAW-TRUST-NEXT below brackets its own registration with. So a row learned
\ here is the row the load path records, not a permissive default.
\
\ WHY LEARNED AND NOT LISTED. RECORD-DEFINER?'s table names the CORE definers.
\ lib owns nine `does>` definers of its own (lib/codegen.f BUFFER-E, lib/queue.f,
\ lib/span.f twice, lib/aio.f, lib/string.f, lib/task.f three times); adding rows
\ for them would put library names in the engine and miss the next one. Measured
\ before this table existed: `tools/check.f lib/process-env-test.f` refused with
\ E-UNDEFINED for PROC-ENV-DIAG, created at lib/process-env.f:93 by
\ `PROC-ENV-DIAG-CAP CODEGEN:BUFFER PROC-ENV-DIAG`, and `tools/check.f
\ lib/queue.f` refused NG-BUFFER, created at lib/type/deftype.f:60.
\ A third class is a definer whose product text the scan never reads because
\ it is rendered and evaluated at load (FUNCTION:, COMMAND, +USER). The checker
\ learns that it renders from its body (CTL-RENDERS), and RECORD-DEFINER?, or
\ TOP-TOKEN for a statement no arm takes, marks the statement's wordlist,
\ leaving what the text defines to the run. What the source states is still
\ read: a `generates:` row declares the word its definer makes from the next
\ token (RECORD-GENERATES), and FUNCTION:'s declaration group is its word's
\ effect (RECORD-FFI-FUNCTION), so those names are checked here and only the
\ rest is left to the run.
\
\ A ROW IS KEYED BY THE CHECKER'S SYMBOL ID for the definer's name, so every
\ spelling that names the definer resolves through the scope chain the checker
\ already owns (FIND-SYM above) instead of through a second name table here.
\ Those ids are the checker's, and a rewound scope truncates them, so the row
\ count is one of the marks the checker's rollback frame rewinds
\ (src/core/checker.f VERIFY-DEFINER-N): every scope releases the rows recorded
\ inside it, SOURCE-BUF's own candidate scope and the scope a caller opens
\ around SOURCE-COMPOSE-LABELED-IN-SCOPE or another IN-SCOPE word alike, and no
\ row outlives the ids it names. A frame that finalizes keeps its ids and the
\ rows that name them.
\ `undefine` retires a definer's row as the checker retires its created-word
\ effect (CHECKER-UNDEFINE): it appends a row with no effect, DEFINER-FIND reads
\ the newest row for a name, and a rewound scope releases a retirement with the
\ rest of its rows.
\ The rows grow with the scope's definers, and their effects with what they
\ create: a preverified require closure holds several sources in one checker
\ scope. Each row's effect lies in DEFINER-SIG, a byte row: a newer effect is
\ appended, so the bytes past the last effect a live row holds are free, and a
\ rewound scope's are written again. Growth may move DEFINER-SIG, so an effect
\ is read where it lies only until the next one is recorded.
DYNAMIC-BUFFER DEFINER-SYM n
DYNAMIC-BUFFER DEFINER-LEN n               \ its effect's length, 0 for a retired row
DYNAMIC-BUFFER DEFINER-AT n                \ where its effect starts in DEFINER-SIG
DYNAMIC-BUFFER DEFINER-SIG u8

: DEFINER-SIG@ ( n -- ptr u8 n ) {: row:n :}
   row DEFINER-AT @ DEFINER-SIG  row DEFINER-LEN @ ;

\ Where the next effect goes: past the last one a live row holds.
: DEFINER-SIG-END ( -- n )
   0 VERIFY-DEFINER-N @
   BEGIN dup 0 > WHILE
      1 -
      dup DEFINER-AT @ over DEFINER-LEN @ +  rot max swap
   REPEAT drop ;

\ The checker's own "offset+1, 0 = none" answer shape, for the same reason: a
\ row index of 0 is a real row. The newest row for sym answers, a retired one
\ as none.
: DEFINER-FIND ( n -- n ) {: sym:n :}         \ sym's row + 1, 0 = no such definer
   sym 0= IF 0 EXIT THEN
   VERIFY-DEFINER-N @ BEGIN dup 0 > WHILE
      1 -
      dup DEFINER-SYM @ sym = IF
         dup DEFINER-LEN @ 0= IF drop 0 EXIT THEN
         1 + EXIT
      THEN
   REPEAT ;

: DEFINER-APPEND ( n -- n )                   \ a new row for sym, with no effect
   {: sym:n :}
   VERIFY-DEFINER-N @
   {: row:n :}
   row 1 + DEFINER-SYM-RESERVE  row 1 + DEFINER-LEN-RESERVE  row 1 + DEFINER-AT-RESERVE
   sym row DEFINER-SYM !  0 row DEFINER-LEN !  0 row DEFINER-AT !
   row 1 + VERIFY-DEFINER-N !
   row ;

: DEFINER-ROW ( n -- n ) {: sym:n :}          \ sym's row, appended when it is new
   sym DEFINER-FIND dup 0<> IF 1 - EXIT THEN drop
   sym DEFINER-APPEND ;

\ Room for U more effect bytes, and where they go.
: DEFINER-ROOM ( n -- n )
   {: u:n :}
   DEFINER-SIG-END
   {: at:n :}
   at u + DEFINER-SIG-RESERVE
   at ;

\ Record `sig` as the effect the definer named by `sym` creates. A name with a
\ live row keeps that row and takes the newer effect, which is what the run
\ time does: a replacement clause replaces the old created-word effect. An
\ empty effect holds no byte: its row's length is 0, which DEFINER-FIND reads
\ as retired, and no place in DEFINER-SIG, which may hold no byte yet, is
\ formed for it.
: DEFINER-ADD ( ptr u8 n n -- ) {: sig:ptr sigu:n sym:n :}
   sym 0= IF EXIT THEN                        \ never recorded: nothing to hang it on
   sym DEFINER-ROW
   {: row:n :}
   sigu 0= IF 0 row DEFINER-AT !  0 row DEFINER-LEN !  EXIT THEN
   sigu DEFINER-ROOM
   {: at:n :}
   sig  at DEFINER-SIG  sigu BYTE-COPY
   at row DEFINER-AT !  sigu row DEFINER-LEN ! ;

\ Give sym the effect row FROM holds, read where it lies once the room is made.
: DEFINER-INHERIT ( n n -- ) {: from:n sym:n :}
   sym 0= IF EXIT THEN
   sym DEFINER-ROW
   {: row:n :}
   from DEFINER-LEN @
   {: u:n :}
   u DEFINER-ROOM
   {: at:n :}
   from DEFINER-SIG@ drop  at DEFINER-SIG  u BYTE-COPY
   at row DEFINER-AT !  u row DEFINER-LEN ! ;

: DEFINER-RETIRE ( n -- )
   {: sym:n :}
   sym DEFINER-FIND 0= IF EXIT THEN
   sym DEFINER-APPEND drop ;

\ The row of a token's definer + 1, 0 when it is no learned definer. The empty
\ table answers before asking the scope anything, so a source that uses no such
\ definer pays one cell read per token.
: DEFINER-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   VERIFY-DEFINER-N @ 0= IF 0 EXIT THEN
   a u FIND-SYM DEFINER-FIND ;

\ The effect a token's definer gives the word it creates, answered as a string
\ whose ZERO LENGTH means "not a learned definer" - the same shape NEXT-RAW ends
\ a source with.
: DEFINER-EFFECT ( ptr u8 n -- ptr u8 n )
   DEFINER-OF dup 0= IF drop SOURCE@ 0 EXIT THEN
   1 - DEFINER-SIG@ ;

variable DEFER-REPORT                            \ the child reports deferrals
variable DEFER-SEEN                              \ and has reported one

CAST: STRETCH-ACTION ( n -- [ ptr u8 n -- ] )
: REPORT-STRETCH ( ptr u8 n -- )
   NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-OFF OWNER-XT STRETCH-ACTION execute ;

: QUIET-TYPE-CLEAR ( -- )
   CHECKER-SIG-UNRES-TAKE drop 2drop ;
: QUIET-TYPE-REPORT ( -- )
   CHECKER-SIG-UNRES-TAKE {: a:ptr u:n found:bool :}
   found DEFER-REPORT @ and IF a u REPORT-STRETCH
      -1 DEFER-SEEN ! THEN ;

\ A body the checker deferred to the run (verdict 2) is reported where the
\ checker's judgment of it stopped (CHECKER-VERIFY-DEFERRED-BODY), when the
\ child reports deferrals, as a deferred stretch is at the token that opened it.
CAST: DEFERRED-BODY-ACTION ( n -- [ -- ptr u8 n ] )
: DEFERRED-BODY$ ( -- ptr u8 n )
   NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-BODY-OFF OWNER-XT DEFERRED-BODY-ACTION execute ;
: REPORT-DEFERRED ( n -- )
   2 <> IF EXIT THEN
   DEFER-REPORT @ 0= IF EXIT THEN
   DEFERRED-BODY$ REPORT-STRETCH
   -1 DEFER-SEEN ! ;

\ A body's verdict as this scan acts on it: -1 certified, 2 deferred to the run
\ (see RENDERS-MARK?), 0 refused. In MULTI-ERR mode a refusal, rejected (0) or
\ uncheckable (1), RETURNS 0 instead of throwing: the checker has rendered it,
\ counted it in MULTI-ERR-N, which keeps the run failing, and recorded the
\ declared signature (no-cascade), so the scan continues at the next
\ definition. Outside MULTI-ERR mode every refusal throws.
\ The verdict is answered rather than swallowed because a created effect is a
\ fact about a definition the checker did not refuse: a refused body records
\ nothing.
: BODY-VERDICT ( n -- n ) {: v:n :}
   v -1 = v 2 = or IF v EXIT THEN
   MULTI-ERR-MODE? IF 0 EXIT THEN
   70 throw ;

\ The colon definitions this run judged, each counted once its verdict is
\ final (CENSUS): a `:` or `kernel:` statement (COLON?) is certified when its
\ body, and its does> clause if it has one, certified; it is deferred to the
\ run when neither was refused and one was deferred, or when the scan stopped
\ inside it (TICK-REMAINDER). A refused one counts in neither, and the scan
\ reads nothing after a stop.
variable CERTIFIED-N
variable DEFERRED-N

\ Count one definition by its verdict, as BODY-VERDICT answers one.
: TALLY ( n -- )
   dup -1 = IF drop 1 CERTIFIED-N +! EXIT THEN
   2 = IF 1 DEFERRED-N +! THEN ;

PTR-VARIABLE TICK-BODY-A
variable TICK-BODY-U
variable TICK-BODY-VERDICT

: TICK-BODY-CHECK ( -- )
   TICK-BODY-A @ TICK-BODY-U @ CHECK-BODY TICK-BODY-VERDICT ! ;

: TICK-BODY-RUN ( -- )
   TICK-BODY-U @ ['] TICK-BODY-CHECK CSR-CHECK ;

: VERIFY-BODY ( -- n )
   DEF-BODY$ {: ba:ptr bu:n :}
   ba TICK-BODY-A !  bu TICK-BODY-U !
   ba bu [: TICK-BODY-RUN ;] WITH-BODY-TICK-ORDER
   IF true TICK-REMAINDER ! 2 ELSE TICK-BODY-VERDICT @ THEN BODY-VERDICT ;

\ The pre-pass's own does>-clause entry point. It is not the engine's
\ CHECK-DOES!: this scan reaches a clause AFTER the definer's own body has been
\ checked and recorded, so the checker must not latch the created effect here -
\ the next record belongs to the next definition. What this scan learns about a
\ definer it READ goes into the table above instead (src/core/checker.f
\ CHECKER-SOURCE-DOES! carries the reason). It takes the definer's name, which
\ names the clause in the diagnostic of a refused one.
: CHECK-DOES-BODY ( ptr u8 n ptr u8 n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-DOES-OFF OWNER-XT DOES-ACTION execute ;

PTR-VARIABLE TICK-DOES-SA
variable TICK-DOES-SU
PTR-VARIABLE TICK-DOES-NA
variable TICK-DOES-NU
variable TICK-DOES-VERDICT

\ The native compiler can refuse an earlier parent token before it reaches a
\ does> clause, then checks the clause before the parent's closing check.
\ Keep the parent's collected text and source map while judging the clause.
create DOES-PARENT-BUF BODYBUF-CAP allot
create DOES-PARENT-ROW BODYBUF-CAP cells allot
variable DOES-PARENT-U
variable DOES-PARENT-ROWS
PTR-VARIABLE DOES-CLOSER-A
variable DOES-CLOSER-U
variable DOES-DEF-VERDICT
TYPED-VARIABLE DOES-PARENT-REFUSED bool
PTR-VARIABLE DOES-CLAUSE-A
variable DOES-CLAUSE-U

: REPORT-DOES-CLAUSE ( -- )
   DOES-CLAUSE-U @ 0= DEFER-REPORT @ 0= or IF EXIT THEN
   DOES-CLAUSE-A @ DOES-CLAUSE-U @ REPORT-STRETCH
   -1 DEFER-SEEN ! ;

: TICK-DOES-CHECK ( -- )
   TICK-BODY-A @ TICK-BODY-U @
   TICK-DOES-SA @ TICK-DOES-SU @ TICK-DOES-NA @ TICK-DOES-NU @
   CHECK-DOES-BODY TICK-DOES-VERDICT ! ;

: TICK-DOES-RUN ( -- )
   TICK-BODY-U @ ['] TICK-DOES-CHECK CSR-CHECK ;

: VERIFY-DOES-BODY ( ptr u8 n ptr u8 n -- n ) {: sig:ptr sigu:n na:ptr nu:n :}
   DEF-BODY$ {: ba:ptr bu:n :}
   ba TICK-BODY-A !  bu TICK-BODY-U !
   sig TICK-DOES-SA !  sigu TICK-DOES-SU !
   na TICK-DOES-NA !  nu TICK-DOES-NU !
   ba bu [: TICK-DOES-RUN ;] WITH-BODY-TICK-ORDER
   IF true TICK-REMAINDER ! 2 ELSE TICK-DOES-VERDICT @ THEN BODY-VERDICT ;

\ ---- the two rules that put a definition in the table above ------------------
\ The definition's own name, pinned by VERIFY-DEFINITION before its body is
\ scanned: a created effect is recorded on the definition's own entry, so the
\ recorders below ask the scope for that entry once the body has certified.
\ RECORD-EXPORT pins the name it re-exports here for the quotation it catches.
PTR-VARIABLE DEF-NAME-A
variable DEF-NAME-U
variable DEF-NAME-BYTE                        \ where the name starts
PTR-VARIABLE DEF-SIG-A                        \ its signature, inside the parens
variable DEF-SIG-U
variable WRAP-DEFINERS                        \ definer calls in this body …
variable WRAP-CTL                             \ … and whether the line ever bent
variable WRAP-ROW                             \ the definer row the last call names

: DEF-NAME! ( -- )
   TOKEN-U @ DEF-NAME-U !  TOKEN-A @ DEF-NAME-A !  TOKEN-BYTE @ DEF-NAME-BYTE ! ;

\ The body of the definition DEF-NAME! pinned, checked with its name armed.
: VERIFY-NAMED-BODY ( -- n )
   DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ DEF-NAME-U @ ARM
   VERIFY-BODY
   DISARM ;

: DOES-PARENT-RUN ( -- )
   VERIFY-NAMED-BODY DOES-DEF-VERDICT ! ;

: WRAP-RESET ( -- )
   0 WRAP-DEFINERS !  0 WRAP-CTL !  0 WRAP-ROW ! ;

: DEFINER-RECORD-AS ( ptr u8 n ptr u8 n -- ) {: sig:ptr sigu:n na:ptr nu:n :}
   sig sigu na nu RECORD-SYM? DEFINER-ADD ;

: DEFINER-RECORD ( ptr u8 n -- )
   DEF-NAME-A @ DEF-NAME-U @ DEFINER-RECORD-AS ;

\ Observe one body token for the straight-line-wrapper rule. A body whose tokens
\ hold EXACTLY ONE definer call and no control flow, quotation or bracket creates
\ whatever that definer creates - `: BUFFER ( n -- ) E-CG-CAP E-CG-VALUE
\ BUFFER-E ;` (lib/codegen.f) is the shape. Two definer calls, a conditional
\ definer or a definer inside a quotation record nothing, and the created word
\ then stays unknown exactly as it is today.
\
\ ONLY A DEFINER THIS PRE-PASS READ is counted here, and that is the whole split
\ between this rule and the checker's. A wrapper of a RESIDENT definer is learned
\ by the body walk instead (src/core/checker.f WRAPN/WRAPC/WRAPBENT), which sees
\ this very body - VERIFY-BODY checks it under the wrapper's own symbol - and
\ every other certified body besides, read or not. A definer this pre-pass read
\ has nothing for that walk to find: its clause goes through
\ CHECKER-SOURCE-DOES!, which latches no created effect, so its symbol carries no
\ CREATES cell and only the table below knows what it makes. The two rules meet
\ at neither end: the walk answers for resident definers, the text for read ones.
: WRAP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   WRAP-CTL @ IF EXIT THEN
   a u WRAP-CTL-TOK? IF -1 WRAP-CTL ! EXIT THEN
   a u DEFINER-OF dup 0<> IF
      1 - WRAP-ROW !
      WRAP-DEFINERS @ 1 + WRAP-DEFINERS !  EXIT
   THEN
   drop ;

\ Only a certified body is learned. One deferred to the run names a word only the
\ run can see, which may itself be a definer, so its definer calls are not
\ counted.
: VERIFY-WRAPPER ( -- )
   WRAP-CTL @ IF EXIT THEN
   WRAP-DEFINERS @ 1 <> IF EXIT THEN
   WRAP-ROW @  DEF-NAME-A @ DEF-NAME-U @ RECORD-SYM?  DEFINER-INHERIT ;

\ The live probe follows interpreter syntax. A selected public word can be
\ probed by its qualified spelling even when the verifier's using mirror does
\ not match the interpreter's; a private word has no qualified spelling and
\ cannot prove it will not execute at compile time.
DYNAMIC-BUFFER TICK-QUAL u8

\ A package's word as a caller outside it spells it, PKG:TAIL, copied into
\ TICK-QUAL, so it outlives the borrowed bytes of a symbol's identity.
: QUAL-SPELLING ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pkg:ptr pkgu:n tail:ptr tailu:n :}
   pkgu tailu + 1+ TICK-QUAL-RESERVE
   pkg 0 TICK-QUAL pkgu BYTE-COPY
   $3A 0 TICK-QUAL pkgu + c!
   tail 0 TICK-QUAL pkgu 1+ + tailu BYTE-COPY
   0 TICK-QUAL pkgu tailu + 1+ ;

: TICK-RESIDENT-IMM? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u tok-imm? 0<> IF true EXIT THEN
   a u FIND-SYM dup 0= IF drop false EXIT THEN
   SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   vis 1 = IF true EXIT THEN
   pkgu 0= IF false EXIT THEN
   pkg pkgu tail tailu QUAL-SPELLING tok-imm? 0<> ;

\ The created effect is the clause's declaration, recorded unless the definer's
\ body or its clause was refused. One deferred to the run keeps it, as a
\ deferred colon definition keeps its declared signature (src/core/checker.f
\ CHECK, verdict 2), and the run judges the deferred text. The definition reports
\ one deferral: its clause's only when the definer's body was not deferred.
\ A compile-time action inside the current definition can change the tier or
\ replace a checker before its later trusted tick. The source scan does not run
\ that action, so this definition and the following source lose the answer.
: TICK-BODY-STEP ( -- )
   DEF-TICK-ORDER @ TICK-UNKNOWN = IF EXIT THEN
   LOCAL-TOKEN? IF EXIT THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF EXIT THEN
   TOKEN-A @ TOKEN-U @ s" [']" CORE-STR= IF EXIT THEN
   TOKEN-A @ TOKEN-U @ s" [" CORE-STR=
   TOKEN-A @ TOKEN-U @ TICK-RESIDENT-IMM? or IF
      TICK-UNKNOWN DEF-TICK-ORDER !  NULL-PTR DEF-TICK-OWNER !
      TICK-CONTEXT-UNKNOWN
   THEN ;

: VERIFY-DOES ( -- bool )
   DEF-TICK-ORDER @ {: order:n :}
   order TICK-GATE = {: gate:bool :}
   order TICK-UNKNOWN = {: unknown:bool :}
   false DOES-PARENT-REFUSED !
   0 DOES-DEF-VERDICT !
   0 DOES-CLAUSE-U !
   gate unknown or IF
      gate IF TICK-PARENT-GATE ELSE TICK-PARENT-UNKNOWN THEN DEF-TICK-ORDER !
      VERIFY-NAMED-BODY 2 <> DOES-PARENT-REFUSED !
      order DEF-TICK-ORDER !
      TICK-REMAINDER @ IF 2 REPORT-DEFERRED false EXIT THEN
   THEN
   gate unknown or IF
      TOKEN-A @ DOES-CLOSER-A !  TOKEN-U @ DOES-CLOSER-U !
      BODY-U @ DOES-PARENT-U !  BODY-ROWS @ DOES-PARENT-ROWS !
      BODY-BUF DOES-PARENT-BUF DOES-PARENT-U @ BYTE-COPY
      BODY-ROW DOES-PARENT-ROW DOES-PARENT-ROWS @ 2 * cells BYTE-COPY
   ELSE
      VERIFY-NAMED-BODY dup DOES-DEF-VERDICT ! REPORT-DEFERRED
   THEN
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   BODY-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF
         TICK-REMAINDER @ DOES-PARENT-REFUSED @ or IF false EXIT THEN
         sig sigu DEF-NAME-A @ DEF-NAME-U @ VERIFY-DOES-BODY
         {: clause:n :}
         TICK-REMAINDER @ IF
            DOES-DEF-VERDICT @ 2 <> IF clause REPORT-DEFERRED THEN
            false EXIT
         THEN
         clause 0= IF false EXIT THEN
         gate unknown or IF
            clause 2 = DEFER-REPORT @ 0<> and IF
               DEFERRED-BODY$ DOES-CLAUSE-U ! DOES-CLAUSE-A !
            THEN
            TOKEN-A @ {: end:ptr :}  TOKEN-U @ {: endu:n :}
            DOES-PARENT-BUF BODY-BUF DOES-PARENT-U @ BYTE-COPY
            DOES-PARENT-ROW BODY-ROW DOES-PARENT-ROWS @ 2 * cells BYTE-COPY
            DOES-PARENT-U @ BODY-U !  DOES-PARENT-ROWS @ BODY-ROWS !
            DOES-CLOSER-A @ TOKEN-A !  DOES-CLOSER-U @ TOKEN-U !
            [: DOES-PARENT-RUN ;] catch {: rc:n :}
            rc 0<> IF REPORT-DOES-CLAUSE rc throw THEN
            DOES-DEF-VERDICT @ 2 = IF 2 REPORT-DEFERRED ELSE REPORT-DOES-CLAUSE THEN
            end TOKEN-A !  endu TOKEN-U !
            TICK-REMAINDER @ IF false EXIT THEN
         ELSE
            DOES-DEF-VERDICT @ 2 <> IF clause REPORT-DEFERRED THEN
         THEN
         DOES-DEF-VERDICT @ 0<> IF
            sig sigu DEFINER-RECORD
            DOES-DEF-VERDICT @ -1 =  clause -1 =  and IF -1 ELSE 2 THEN TALLY
         THEN
         true EXIT
      THEN
      TICK-BODY-STEP
      APPEND-BODY-TOKEN
   AGAIN ;

\ The two registrars, kept apart here for the same reason the engine keeps them
\ apart (src/core/checker.f TRUST-DECL, dot habu-make-trust-refuse-cc8e19de).
\ This scanner is a PRE-PASS: it reads a whole source buffer before any of it is
\ compiled, so a definer it replays names a word that does not exist in this
\ process and cannot be asked to prove otherwise. A bare `s" NAME" s" SIG" trust`
\ row is the opposite - it asserts an effect for a word it CLAIMS already exists,
\ which is a claim the dictionary can answer and now does.
\ DECL-SIGNATURE asks the declaration owner's registrar (src/core/checker.f
\ TRUST-DECL?), which registers the row as TRUST-DECL does and answers whether
\ it retained it. A name its scope already holds keeps that record either way,
\ so only this answer tells a redeclaration the registrar retained from one it
\ refused.
: DECL-SIGNATURE ( ptr u8 n ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-OFF OWNER-XT DECL-ACTION execute ;

TRUSTED: TRUST-SIGNATURE ( ptr u8 n ptr u8 n -- )
   TRUST ;

\ The cast declarer's registrar, on the same boundary and for the same reason as
\ TRUST-SIGNATURE above: UNSAFE-TOK? rejects `checker-defcast` inside a checked
\ body, so the one place allowed to name it is a declared boundary word. The
\ refusals it runs are the engine's own (checker.f CAST-CERTIFY), so a cast this
\ pre-pass accepts is exactly a cast the engine accepts.
TRUSTED: DEFCAST-SIGNATURE ( ptr u8 n ptr u8 n -- )
   CHECKER-DEFCAST ;

\ The `generates:` row's checks, on the same boundary for the same reason:
\ UNSAFE-TOK? rejects `checker-generates` inside a checked body. The refusals
\ are the engine's own (checker.f CHECKER-GENERATES), so a row this pre-pass
\ accepts is a row the engine accepts.
TRUSTED: GENERATES-SIGNATURE ( ptr u8 n ptr u8 n bool -- n n )
   CHECKER-GENERATES ;

\ A `parses:` row's checks, on the same boundary for the same reason:
\ UNSAFE-TOK? rejects `checker-parses-row` inside a checked body. The refusals
\ are the engine's own (checker.f CHECKER-PARSES-ROW), so a row this pre-pass
\ keeps is a row the load accepts.
TRUSTED: PARSES-CHECK ( ptr u8 n ptr u8 n ptr u8 n bool -- n n n )
   CHECKER-PARSES-ROW ;

: CAST-TRUST ( -- bool )
   DTC-NAME$ DTC-SIG$ DECL-SIGNATURE ;

\ ---- a definition of a name the scope already holds ---------------------------
\ The checker's own guard (src/core/checker.f CHECKER-CERT-DUP?) refuses a colon
\ definition, a cast, a typed storage definition and a re-export whose name its
\ scope already holds, but a colon definition only once its body is checked, a
\ storage definition once its type is read, and it keeps no place. Those rows
\ ask the guard's question as soon as they have scanned the name, as the
\ engine's wall and the check hook (src/core/check-hook.f) refuse it under
\ --load, a name only the checker holds included, such as a constructor the
\ PRODUCT replay publishes. The refusal keeps where the name starts and its
\ length, not the name (tools/check-all-errors-core.f CA-DUP-RECORD$ says why).
\ A definer the checker only trusts (variable, defer, TRUSTED:, ...) is left to
\ the run, which names its duplicate as --load does, or first refuses an
\ earlier definition whose text the engine cannot hold.
variable DUPLICATE-AT
variable DUPLICATE-U

\ Whether the path is the subject's, whose bytes are the supplied ones.
: COMPOSE-SUBJ? ( ptr u8 n -- bool )
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ CORE-STR= ;

\ A file's name as its packets give it: the label, for the subject.
: COMPOSE-DIAG$ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu COMPOSE-SUBJ? IF COMPOSE-SUBJ-LABEL COMPOSE-SUBJ-LABEL-U @ EXIT THEN
   path pathu ;

public

\ What the refusal does, given where the name starts, its length, the file it
\ is in, named as its packets name it, and whether that is the supplied bytes.
\ This throws E-DUP-DEFINITION, which stops the scan as the duplicate stops the
\ load, and DUPLICATE answers the name. A word installed in its place that
\ returns has reported the duplicate: the scan counts it in MULTI-ERR-N, as a
\ refused definition, skips the definition and goes on.
defer ON-DUPLICATE ( n n ptr u8 n bool -- )

\ What a reported definition is, as the path that declared it knows it: a word
\ that runs code (`:`, `TRUSTED:`, `defer`, a cast, a field, `FUNCTION:`), a
\ constant (`constant`, a structure's size), storage whose word answers its
\ address (`variable`, `create`, a buffer, a word a definer creates when it
\ runs), or a re-export, which is what the word it names is.
0 constant DEF-WORD
1 constant DEF-CONSTANT
2 constant DEF-STORAGE
3 constant DEF-EXPORT

\ A definition the scan retained, once its registrar returned and its name's
\ recording scope holds a live record: its kind, the statement token that
\ declared it (`:`, `constant`, a definer's name); its name as the source
\ writes it, or a name the definer generates as spelled; the symbol the
\ checker recorded it under; its declared effect, empty for none;
\ where the token that declared it starts and ends in the file FILE$
\ names; and its class, one of the four above. The strings are borrowed: a
\ word installed here consumes them before it returns.
defer ON-DEFINITION ( ptr u8 n ptr u8 n n ptr u8 n n n n -- )

\ A use in the subject that the checker bound to a located declaration
\ (src/core/checker.f CHECKER-ON-USE): where the use starts and ends in the
\ subject; the path the declaration's file was resolved to when the
\ composition visited it; and where the token that declared it starts and
\ ends there, all at the base TOKEN-BYTE@ counts from. The path is borrowed: a
\ word installed here consumes it before it returns. A use in a file the
\ subject loads, and one bound to a declaration with no location, are not
\ reported.
defer ON-USE ( n n ptr u8 n n n -- )

\ The file being scanned, named as its packets name it.
: FILE$ ( -- ptr u8 n )
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ COMPOSE-DIAG$ ;

\ A file the scan starts, named as FILE$ names it, and the bytes the scan reads
\ there, which every position it reports in the file counts in: the bytes
\ supplied for the subject, the file's bytes as this composition read them for
\ any other. It runs each time the scan starts a file, the subject first,
\ before anything in it is reported. Both strings are borrowed and read-only:
\ a word installed here consumes them before it returns and writes neither.
defer ON-FILE ( ptr u8 n ptr u8 n -- )

private

\ These declaration arms are dispatched by EM-INTERPRET-DEFINE-KEYWORDS before
\ dictionary lookup. Their scanner counterparts do no arbitrary execution.
\ A word-backed loader or library definer has no such structural guarantee:
\ even the original include/require entry can call replaced source providers.
: TICK-NATIVE-DEFINER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" package" STR=CI  a u s" ;package" STR=CI or
   a u s" public" STR=CI or  a u s" private" STR=CI or
   a u s" using" STR=CI or  a u s" ;using" STR=CI or IF 0 0= EXIT THEN
   a u s" export" STR=CI  a u s" trusted:" STR=CI or
   a u s" cast:" STR=CI or  a u s" linear:" STR=CI or
   a u s" defer" STR=CI or  a u s" create" STR=CI or
   a u s" variable" STR=CI or  a u s" constant" STR=CI or ;

: DEFINITION-NONE ( ptr u8 n ptr u8 n n ptr u8 n n n n -- )
   drop 2drop 2drop drop 2drop 2drop ;

: DEFINITION-INIT ( -- )
   ['] DEFINITION-NONE is ON-DEFINITION ;

DEFINITION-INIT

: FILE-NONE ( ptr u8 n ptr u8 n -- )  2drop 2drop ;

: FILE-INIT ( -- )
   ['] FILE-NONE is ON-FILE ;

FILE-INIT

: USE-NONE ( n n ptr u8 n n n -- )
   2drop 2drop 2drop ;

: USE-INIT ( -- )
   ['] USE-NONE is ON-USE ;

USE-INIT

: LENIENT-NONE ( ptr u8 n -- bool )  2drop false ;

: LENIENT-INIT ( -- )
   ['] LENIENT-NONE is LENIENT-FILE? ;

LENIENT-INIT

\ A use the checker published, at the subject's base: reported when it is in
\ the subject and its declaration lies in a file this composition visited.
: USE-SEEN ( n n n n n -- )
   {: s:n e:n v:n ds:n de:n :}
   VISIT-CUR @ VISIT-FIRST @ <> IF EXIT THEN
   v VISIT-IN? 0= IF EXIT THEN
   s BASE-BYTE @ +  e BASE-BYTE @ +  v VISIT-PATH  ds de ON-USE ;

: DUPLICATE-STOP ( n n ptr u8 n bool -- )
   drop 2drop
   DUPLICATE-U !  DUPLICATE-AT !
   E-DUP-DEFINITION throw ;

: DUPLICATE-INIT ( -- )
   ['] DUPLICATE-STOP is ON-DUPLICATE ;

DUPLICATE-INIT

\ Refuse the definition whose name, n bytes long, starts at TOKEN-BYTE.
: DUPLICATE! ( n -- )
   {: u:n :}
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @
   {: path:ptr pathu:n :}
   TOKEN-BYTE @ u path pathu COMPOSE-DIAG$ path pathu COMPOSE-SUBJ? ON-DUPLICATE
   1 MULTI-ERR-N +! ;

\ Whether the scan refused the definition the name names, and goes on past it.
: REFUSE-DUPLICATE ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u CHECKER-CERT-DUP? dup IF u DUPLICATE! THEN ;

\ Whether the scan refused the definition NAME, written from byte AT, now that
\ it knows whether the definition makes a does> clause (CLAUSE): a colon
\ definition at the `;` or `does>` that ends its body, before the body is
\ checked, and a TRUSTED: one once its body is read, before its clause is
\ published. A record of the name from before the replay that has the other
\ shape is no loaded twin of it (src/core/checker.f CHECKER-CERT-SHAPE-DUP?),
\ and the live engine refuses the definition there. The refusal names the
\ definition's name, as REFUSE-DUPLICATE does.
: REFUSE-SHAPE ( ptr u8 n n bool -- bool )
   {: a:ptr u:n at:n clause:bool :}
   a u clause CHECKER-CERT-SHAPE-DUP? 0= IF false EXIT THEN
   at TOKEN-BYTE !
   u DUPLICATE!
   true ;

\ The symbol the name's recording scope holds a live record under, 0 for none.
\ Where a guard refused a live record before the registrar ran, one there now
\ is the record that registrar retained: certified, deferred or kept with a
\ refused body (src/core/checker.f CHECK). A declaration DECL-SIGNATURE
\ registers has no guard: the record found for its name is that declaration's
\ only when DECL-SIGNATURE answered true, so its callers ask only then.
: RETAINED-SYM ( ptr u8 n -- n )
   {: name:ptr nameu:n :}
   name nameu CHECKER-CERT-DUP? 0= IF 0 EXIT THEN
   name nameu RECORD-SYM? ;

\ Report the definition the name names, of the class given, declared by the
\ token from byte at to byte end, when its registrar retained a record.
: DEFINED ( ptr u8 n ptr u8 n ptr u8 n n n n -- )
   {: kind:ptr kindu:n name:ptr nameu:n eff:ptr effu:n at:n end:n class:n :}
   name nameu RETAINED-SYM
   {: sym:n :}
   sym 0= IF EXIT THEN
   kind kindu name nameu sym eff effu TRIM at end class ON-DEFINITION ;

\ The same, for the name the statement token declares, written from byte at.
: DEFINED-HERE ( ptr u8 n ptr u8 n n n -- )
   {: name:ptr nameu:n eff:ptr effu:n at:n class:n :}
   TOP-CUR-A @ TOP-CUR-U @ name nameu eff effu at at nameu + class DEFINED ;

: SIG-RAW-MODE! ( n -- ) SIG-RAW-MODE ! ;

\ RAW-TRUST-NEXT: registers the given effect for the word the next token names,
\ with TVK-RAW type vars (SIG-RAW-MODE! brackets the checker's signature parse).
\ Used for the raw storage definers create/variable/constant/PTR-VARIABLE and
\ PERSISTED-PTR-VARIABLE so a
\ fetch from their raw cell yields a RAW value that cannot launder into a nominal
\ atom or family (habu-nominal-storage-raw, VALUE side). The class is the
\ definer's: DEF-CONSTANT for `constant`, DEF-STORAGE for the rest.
: RAW-TRUST-NEXT ( ptr u8 n n -- )
   {: sig:ptr sigu:n class:n :}
   NAME-TOKEN
   dup 0= IF E-MISSING-NAME throw THEN
   TOKEN-BYTE @
   {: name:ptr nameu:n at:n :}
   name nameu QUIET-NAME
   -1 SIG-RAW-MODE!
   name nameu at nameu ARM
   name nameu sig sigu [: DECL-SIGNATURE ;] [: 0 SIG-RAW-MODE! DISARM ;] finally
   IF name nameu sig sigu at class DEFINED-HERE THEN ;

\ CREATED-TRUST-NEXT?: RAW-TRUST-NEXT's twin for a definer the checker knows and
\ this pre-pass never read. The row is the checker's own certified one, so there
\ is no signature text to re-parse and no seal to re-apply here; what is left is
\ the same shape - the created word is the NEXT token - and the same answer.
\ THE TOKEN IS TESTED BEFORE THE NAME IS TAKEN: NAME-TOKEN consumes a token, and
\ a token that is not a definer must leave the scan exactly where it was.
: CREATED-TRUST-NEXT? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u FIND-SYM {: dsym:n :}
   dsym CREATES-SYM? 0= IF 0 0= 0= EXIT THEN
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   TOKEN-BYTE @ {: at:n :}
   name nameu at nameu ARM
   name nameu dsym RECORD-CREATED
   DISARM
   0= IF 0 0= 0= EXIT THEN
   name nameu s" " at DEF-STORAGE DEFINED-HERE
   0 0= ;

: TRUST-DEFER-SIGNATURE ( ptr u8 n n -- )
   {: name:ptr nameu:n at:n :}
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DECL-SIGNATURE
   {: kept:bool :}
   kept IF name nameu CHECKER-DEFER THEN
   DISARM
   kept IF name nameu sig sigu at DEF-WORD DEFINED-HERE
   ELSE QUIET-TYPE-REPORT THEN ;

: TRUST-DEFER ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   name nameu TOKEN-BYTE @ TRUST-DEFER-SIGNATURE ;

variable TRUSTED-DOES                         \ the trusted body's `does>` was read

\ A token of the trusted body NAME before its `does>`, asked of the checker
\ (TRUSTED-REACH) when it may call a word. A local, a group, a parsing keyword
\ with its operand and a string call none, and SKIP-DEF-TOKEN takes them; nor
\ do a number, a control word, a loop index, or `is` with the deferred word it
\ sets.
: TRUSTED-CALL ( ptr u8 n -- ) {: na:ptr nu:n :}
   TRUSTED-DOES @ LOCAL-TOKEN? or IF EXIT THEN
   TOKEN-A @ TOKEN-U @ {: a:ptr u:n :}
   a u s" {:" CORE-STR=  a u BODY-PARSER? or  a u STRING-OPENER? or IF EXIT THEN
   a u num-parse nip nip IF EXIT THEN
   a u WRAP-CTL-TOK? IF EXIT THEN
   a u s" i" STR=CI  a u s" j" STR=CI or  a u s" unloop" STR=CI or IF EXIT THEN
   a u s" is" STR=CI IF OPERAND 2drop EXIT THEN
   a u na nu TRUSTED-REACH ;

\ A trusted body is ASSERTED, never verified - but its `does>` clause is a
\ DECLARATION, and what it declares is the effect the load path gives every word
\ the definer creates: habu2.f EM-COMPILE-PUBLISH-TRUSTED runs CHECK-DOES! at the
\ `;` of a trusted definition, ahead of the trusted branch, and the clause it
\ certifies lands on the definer's CREATES cell (src/core/checker.f TRUST-DECL).
\ So this scan takes the clause signature and learns the definer from it, exactly
\ as VERIFY-DOES learns a checked one, and skips every token of the body as it
\ always has: nothing inside a trusted body is checked here. Measured on the
\ previous commit, with a module holding `TRUSTED: TD ( n -- ) create , does>
\ ( -- ptr n ) ;`, a source requiring it and writing `5 MOD:TD W` left W
\ E-UNDEFINED at its first typed use; it is the effect mismatch it deserves now.
\ A `does>` inside a string is no clause: the string opener leaves through
\ SKIP-BODY-TOKEN below, which consumes its rest (the four trusted DOES-*
\ bodies in src/compiler/native/checker-owner.f name fields as `s" does> …"`).
\ What running the word may do is still what its calls may do, and the tokens
\ before its `does>` run when it does (TRUSTED-CALL).
: SCAN-TRUSTED-BODY ( ptr u8 n bool -- ) {: na:ptr nu:n kept:bool :}
   BODY-LOAD-RESET
   LOCALS-RESET
   0 TRUSTED-DOES !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      LOCAL-TOKEN? 0=  TOKEN-A @ TOKEN-U @ s" does>" STR=CI  and IF
         REQUIRE-SIGNATURE
         kept IF na nu DEFINER-RECORD-AS ELSE 2drop THEN
         -1 TRUSTED-DOES !
      ELSE
         kept IF na nu TRUSTED-CALL THEN
         SKIP-DEF-TOKEN
      THEN
   AGAIN ;

\ The engine's `does>` makes the clause record `<definer>;does` for a TRUSTED:
\ definer as for a checked one, so a definer whose `does>` the scan read gets it
\ at its `;` (TRUSTED-CLAUSE), as a checked definer gets it from the check of
\ its clause (CHECK-DOES-BODY): a later definition of that name is refused and a
\ `trust` of it binds, as in the live load. A loaded twin of the other shape
\ refuses the definition first (REFUSE-SHAPE).
: TRUSTED-DEFINITION ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   TOKEN-BYTE @ {: at:n :}
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DECL-SIGNATURE
   {: kept:bool :}
   DISARM
   kept IF name nameu sig sigu at DEF-WORD DEFINED-HERE
   ELSE QUIET-TYPE-REPORT THEN
   name nameu kept SCAN-TRUSTED-BODY
   name nameu at TRUSTED-DOES @ 0<> REFUSE-SHAPE IF EXIT THEN
   TRUSTED-DOES @ kept and IF name nameu TRUSTED-CLAUSE THEN ;

\ A cast has no body and no `;`, so unlike TRUSTED-DEFINITION above there is
\ nothing to skip: the declaration ends at its closing paren. Registration goes
\ through the certifying registrar, not DECL-SIGNATURE, so an illegal retype is
\ refused here too and not merely recorded.
: CAST-DECLARATION ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   TOKEN-BYTE @ {: at:n :}
   name nameu REFUSE-DUPLICATE IF REQUIRE-SIGNATURE 2drop EXIT THEN
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DEFCAST-SIGNATURE
   DISARM
   name nameu sig sigu at DEF-WORD DEFINED-HERE ;

\ A `linear:` row has the same shape and is certified by the engine's own
\ registrar, which package VERIFY calls through its private row, so a mint or
\ erase this pre-pass accepts is exactly one the engine accepts.
: LINEAR-DECLARATION ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   TOKEN-BYTE @ {: at:n :}
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu CHECKER-LINEAR
   DISARM
   name nameu sig sigu at DEF-WORD DEFINED-HERE ;

: IDENTITY-OUTCOME ( n -- ) {: rc:n :}
   rc 0= IF EXIT THEN
   DISARM
   rc E-CHECKER-IDENTITY-UNRESOLVED <> IF rc throw THEN
   QUIET-TYPE-REPORT ;

: UNDEFINE-WORD ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   name nameu CHECKER-UNDEFINE
   name nameu RECORD-SYM? DEFINER-RETIRE ;

: RECORD-PACKAGE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu CHECKER-PACKAGE ;

: RECORD-PUBLIC ( -- )
   CHECKER-PUBLIC ;

: RECORD-PRIVATE ( -- )
   CHECKER-PRIVATE ;

: RECORD-END-PACKAGE ( -- )
   CHECKER-END-PACKAGE ;

\ `using NAME` and `;using` are source events exactly like `package` and
\ `;package`, and they were the one scope word this table never had a row for
\ (dot habu-own-pkg-state-acf7086c). Without them a replayed file's own imports
\ did nothing -- `using RB-SUPPLIER : RB-USE ( -- n ) WIDGET ;` loaded fine
\ through the engine and was refused on replay with E-UNDEFINED for WIDGET --
\ while the CALLER's imports stayed live over the whole replay instead. The
\ checker owns the replay's using depth, so these two rows are the only thing
\ that moves it.
: RECORD-USING ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu CHECKER-USING-PUSH ;

: RECORD-END-USING ( -- )
   CHECKER-USING-POP ;

\ DEFTYPE NAME declares a value nominal (lib/type/deftype.f): a
\ package-scoped arity-0 type family whose lowercase tail is the surface name
\ folded (SERIAL -> serial) and whose converter pair >NAME ( n -- tail ) /
\ NAME>N ( tail -- n ) keeps a plain n from standing in for the nominal. The
\ static recorder mirrors the runtime mint: register the family, then trust the
\ two derived converter signatures so later definitions that use the tail and
\ the converters verify without loading deftype.f.
DYNAMIC-BUFFER NOM-TAIL u8                    \ the folded tail, as long as the name

\ MANGLE ( ptr u8 n -- ptr u8 n ) folds the UPPER-CASE surface name to the
\ lowercase family tail, matching deftype.f's ASCII-LOWER fold.
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u NOM-TAIL-RESERVE
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C  over NOM-TAIL c!
      1+
   REPEAT drop
   0 NOM-TAIL u ;

\ The cast CAST-TRUST just declared, by the DEFTYPE name from byte at, u long.
: CAST-DEFINED ( bool n n -- )
   {: kept:bool at:n u:n :}
   kept 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ DTC-NAME$ DTC-SIG$ at at u + DEF-WORD DEFINED ;

: RECORD-DEFTYPE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   TOKEN-BYTE @ {: at:n :}
   name nameu MANGLE {: tail:ptr tailu:n :}
   tail tailu s" 0" CHECKER-DEFFAMILY
   name nameu tail tailu DTC-BUILD-IN
   DTC-NAME$ at nameu ARM              \ the converter's own name, the TYPE's span
   CAST-TRUST
   DISARM
   at nameu CAST-DEFINED
   name nameu tail tailu DTC-BUILD-OUT
   DTC-NAME$ at nameu ARM
   CAST-TRUST
   DISARM
   at nameu CAST-DEFINED ;

: RECORD-DEFLINEAR ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu TYPE-RESERVED? IF E-VS-TYPE-NAME throw THEN
   name nameu CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" STR=CI ;

: SUMTYPE-END? ( ptr u8 n -- bool )
   s" ;SUMTYPE" STR=CI ;

\ Missing name/arity are reported by CHECKER-DEFFAMILY through the declaration
\ packet (E-BAD-DECLARATION), matching the native path -- no raw pre-check die (§24).
: RECORD-NEWTYPE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   NEXT-SCAN {: ar:ptr aru:n :}
   name nameu ar aru CHECKER-DEFFAMILY ;

\ A SUMTYPE defines its constructors when it loads, so after the registration
\ the scan replays their checked effects: a later definition that calls one, as
\ lib/aio.f's OUTCOME-OF calls AIO-OUTCOME:ready, resolves here as in the load.
: RECORD-SUMTYPE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   BODY-RESET
   BEGIN
      NEXT-SCAN
      dup 0= IF                        \ EOF before ;SUMTYPE -> declaration packet (§24)
         2drop
         name nameu BODY$ CHECKER-DEFSUM-NOEND
         EXIT
      THEN
      2dup SUMTYPE-END? IF
         2drop
         name nameu BODY$ CHECKER-DEFSUM
         GENERATED-DECL-CTOR:REPLAY-LEGACY
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: ENUM-END? ( ptr u8 n -- bool )
   s" ;ENUM" STR=CI ;

\ The token reader for a REPLAYED declaration window, and the reason it is not
\ NEXT-SCAN.
\
\ NEXT-SCAN launders comments: a bare `\` makes it skip to the newline and a bare
\ `(` makes it skip to the `)`. That is right for scanning a FILE, where comments
\ are inert between definitions. It is wrong inside a declaration body, because
\ the engine does not scan a declaration body — the live keyword reads it with
\ `parse-name`, which has no comment rule at all, so `\` and `(` arrive as
\ ordinary tokens and hit the name gate. Measured: `ENUM c red \ note` through
\ the live front end rejects 7101 "name must be a lowercase tail at '\'".
\ Stripping them here would let a replay ACCEPT source the engine refuses, and
\ register a family that can never exist.
\
\ NEXT-RAW is exactly `parse-name`'s rule — the next whitespace-delimited token,
\ no comment or string interpretation — so the replayed body is the same token
\ sequence the live keyword would have read. The substitution is scoped to the
\ two replay windows below and to the definer names NAME-TOKEN reads; every
\ other scan in this file keeps NEXT-SCAN, since outside a declaration comments
\ really are inert.
: DECL-TOKEN ( -- ptr u8 n ) NEXT-RAW ;

\ Registration-only replay of `ENUM name .. ;ENUM` (mirrors RECORD-SUMTYPE):
\ buffer the body through ;ENUM and register the family from it.
\
\ This drives the unified ENUM front end's replay entry rather than sumtype.f's
\ CHECKER-DEFENUM, which the type-DSL cutover deletes. It also widens what this
\ arm understands: the legacy entry only ever read a compact list of bare
\ variant names, while the replay entry runs the real grammar and so accepts the
\ full `arity VARIANT name FIELD f t ;VARIANT` form too. The terminator is
\ buffered with the body because the front end parses its own terminator.
: RECORD-ENUM ( -- )
   DECL-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup ENUM-END? IF
         BODY-APPEND
         name nameu BODY$ ENUM-DECL:ED-REPLAY-OUTCOME
         IF
            DEFER-REPORT @ IF REPORT-STRETCH -1 DEFER-SEEN !
            ELSE 2drop THEN
         ELSE 2drop THEN
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: STRUCTURE-DECL-END? ( ptr u8 n -- bool )
   s" ;STRUCTURE" STR=CI ;

\ Registration-only replay of the unified `STRUCTURE name arity FIELD f t ..
\ ;STRUCTURE` (mirrors RECORD-ENUM). Distinct from RECORD-STRUCTURE above, which
\ handles the Forth-standard `BEGIN-STRUCTURE .. END-STRUCTURE` layout facility;
\ these are different declarations that happen to share a word stem.
\
\ Without this arm a STRUCTURE family was never registered on this path, so a
\ later signature or payload type naming it could not resolve. Registration
\ includes the family's MAKE/UNMAKE variant rows and constructor package, so
\ `FAMILY:MAKE` in the same source resolves; no dictionary word is defined.
: RECORD-STRUCTURE-DECL ( -- )
   DECL-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup STRUCTURE-DECL-END? IF
         BODY-APPEND
         name nameu BODY$ STRUCTURE-DECL:SD-REPLAY-OUTCOME
         IF
            DEFER-REPORT @ IF REPORT-STRETCH -1 DEFER-SEEN !
            ELSE 2drop THEN
         ELSE 2drop THEN
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: PRODUCT-END? ( ptr u8 n -- bool )
   s" ;PRODUCT" STR=CI ;

\ Registration-only replay of `PRODUCT name arity FIELD f t .. ;PRODUCT` (mirrors
\ RECORD-SUMTYPE): buffer the `arity FIELD ..` body through ;PRODUCT, register
\ the TK-PRODUCT family + its generated-word metadata rows so later signatures
\ in this source resolve the family, and replay the MAKE/UNMAKE effects. No
\ dictionary words are generated on this path (engine-definer-only, sum parity).
: RECORD-PRODUCT ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   BODY-RESET
   BEGIN
      NEXT-SCAN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup PRODUCT-END? IF
         2drop
         name nameu BODY$ CHECKER-DEFPRODUCT
         GENERATED-DECL-CTOR:REPLAY-LEGACY
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: LEGACY-TYPE-OUTCOME ( n -- ) {: rc:n :}
   rc 0= IF EXIT THEN
   rc TYPE-DECL:E-TDECL-UNRESOLVED <> IF rc throw THEN
   QUIET-TYPE-REPORT ;

\ Every storage definer's gate registration reads its declaration here, as its
\ definer does (src/core/layout-buffer.f STORAGE-PARSE-TYPE). A stored type may
\ be `ptr* base`, a family application or a spaced quotation or scheme, so the
\ type is a contiguous multi-token span from the scanner buffer, ended where the
\ checker ends one (CHECKER-TYPE-SPAN-STEP) or with its line. The declaration
\ ends with its line: the name is read on the definer's own line and the type's
\ first token on the name's, raw, as parse-name reads them, so a `(` or `\`
\ there is the name or type the definer refuses, not a comment. With no name
\ there the definer is refused at its own token; with no first token the span
\ is empty, and the checker refuses that by name.
PTR-VARIABLE STG-A
variable STG-U
PTR-VARIABLE STG-START
PTR-VARIABLE STG-TYPE-A
variable STG-TYPE-U

\ The next token, as the definer's parse-name reads it, when it stands on the
\ scanner's line: the bytes before it hold no line feed
\ (CHECKER-TYPE-SPAN-BREAK?). A token on a later line starts the next
\ statement, and none is read.
: SCAN-LINE-TOKEN ( -- ptr u8 n )
   SCAN-I @ {: end:n :}
   SKIP-WS
   SOURCE@ end +  SCAN-I @ end -  CHECKER-TYPE-SPAN-BREAK? IF SOURCE@ 0 EXIT THEN
   NEXT-RAW ;

\ Whether the spelling goes on past its last token: not when that token ended
\ it, and not past its line.
: SCAN-STORAGE-MORE? ( bool -- bool ) {: ended:bool :}
   ended IF 0 0= 0= EXIT THEN
   SCAN-LINE-TOKEN {: a:ptr u:n :}
   u 0= IF 0 0= 0= EXIT THEN
   a STG-A !  u STG-U !
   0 0= ;

: SCAN-STORAGE-TYPE ( -- ptr u8 n )
   SCAN-LINE-TOKEN STG-U !  STG-A !
   STG-A @ STG-START !
   0 BEGIN STG-A @ STG-U @ CHECKER-TYPE-SPAN-STEP SCAN-STORAGE-MORE? 0= UNTIL drop
   STG-START @  STG-A @ STG-U @ + STG-START @ -
   2dup STG-TYPE-U ! STG-TYPE-A ! ;

\ The declared name. With none on the definer's line it is empty, and the
\ definer's token is refused as its run refuses it.
\ A refused name, malformed or a duplicate, is empty and still owns its type
\ span; consume it before resuming the statement scan so a type token cannot
\ be interpreted as a new statement.
: SCAN-STORAGE-NAME ( -- ptr u8 n )
   SCAN-LINE-TOKEN
   dup 0= IF TOP-CUR-A @ TOP-CUR-U @ CHECKER-STORAGE-NAME-REFUSE EXIT THEN
   2dup QUIET-NAME
   2dup CHECKER-LBUF-NAME-OK? IF
      2dup REFUSE-DUPLICATE 0= IF EXIT THEN
   THEN
   2drop SCAN-STORAGE-TYPE 2drop SOURCE@ 0 ;

\ The load stops at a count token it refuses to resolve, refused there as a
\ top-level token (TOP-RESOLVE), so the declaration after it is read but not
\ judged: its count is refused once.
: COUNT-REFUSED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0<>  a TOP-REFUSED @ =  and ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFLAYOUT-BUFFER
   DISARM
   name nameu s" " at DEF-STORAGE DEFINED-HERE ;

\ DEFER-LAYOUT-BUFFER publishes its accessor and NAME-BIND and NAME-GROW from
\ one line, so a later definition calling one is E-UNDEFINED without this row,
\ and a type it cannot size goes unreported until the run. No count token: the
\ count arrives at the bind.
: RECORD-DEFER-LAYOUT-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFDEFER-LAYOUT-BUFFER
   DISARM
   name nameu s" " at DEF-STORAGE DEFINED-HERE ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFTYPED-BUFFER
   DISARM
   name nameu s" " at DEF-STORAGE DEFINED-HERE ;

: RECORD-TYPED-VARIABLE ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFTYPED-VARIABLE
   DISARM
   name nameu s" " at DEF-STORAGE DEFINED-HERE ;

\ DYNAMIC-BUFFER (src/core/layout-buffer.f) publishes THREE words from one line -
\ the accessor, NAME-RESERVE and NAME-RELEASE - so the whole triple is registered
\ here. Certification never runs the definer, and without this row a later
\ definition in the same source calling one of the three is E-UNDEFINED:
\ src/habu/aot-decl.f's AOT-NAMES-RESERVE was, which took the stage2 certify pass
\ with it. No count token: a dynamic buffer's extent is set at run time.
: RECORD-DYNAMIC-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFDYNAMIC-BUFFER
   DISARM
   name nameu s" " at DEF-STORAGE DEFINED-HERE ;

\ Storage registrars resolve their complete type before publishing a word.
\ A missing nominal outcome therefore leaves only the scoped uncertainty mark.
: STORAGE-OUTCOME ( n -- ) {: rc:n :}
   rc 0= IF EXIT THEN
   DISARM
   rc E-CHECKER-STORAGE-UNRESOLVED <> IF rc throw THEN
   DEFER-REPORT @ IF STG-TYPE-A @ STG-TYPE-U @ REPORT-STRETCH
      -1 DEFER-SEEN ! THEN
   QUIET-TYPE-CLEAR ;

\ The source byte body byte at was read from, in the newest run BODY-ROW!
\ started at or before it.
: BODY>BYTE ( n -- n )
   {: at:n :}
   BODY-ROW {: rows:ptr :}
   0 BODY-ROWS @ 0 ?do
      i 2 * cells rows + @ at <= IF drop i THEN
   loop
   {: row:n :}
   row 2 * 1 + cells rows + @  at +  row 2 * cells rows + @ -  BASE-BYTE @ + ;

\ The record the body holds registers, or the scan stops at the name, at the
\ field the checker refuses, or at END-VALUE-RECORD, the token read last, for a
\ record with no field.
: VALUE-RECORD-ADD ( ptr u8 n n -- )
   {: name:ptr nameu:n byte:n :}
   name nameu TYPE-RESERVED? IF byte TOKEN-BYTE !  E-VS-TYPE-NAME throw THEN
   name nameu BODY$ CHECKER-TRY-RECORD
   {: at:n msg:ptr msgu:n :}
   msgu 0= IF EXIT THEN
   at BODY-U @ < IF at BODY>BYTE TOKEN-BYTE ! THEN
   E-VS-RECORD-FIELD throw ;

: RECORD-VALUE-RECORD ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   TOKEN-BYTE @ {: byte:n :}
   BODY-RESET
   BEGIN
      NEXT-SCAN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup VALUE-RECORD-END? IF
         2drop
         name nameu byte VALUE-RECORD-ADD
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: VALUE-RECORD-OUTCOME ( n -- ) {: rc:n :}
   rc 0= IF EXIT THEN
   rc E-CHECKER-RECORD-UNRESOLVED <> IF rc throw THEN
   QUIET-TYPE-REPORT ;

\ A row the checker leaves to the run (src/core/checker.f CHECKER-VERIFY-TRUST):
\ it names no word where its record lands, and a rendering statement before it
\ marked that wordlist, so the run may define the word or the name may be
\ misspelt, and only the load can tell. The row is reported at its name, as a
\ deferred stretch is at its token, and records nothing, so each use of the
\ name stays the run's too.
CAST: TRUST-ACTION ( n -- [ ptr u8 n -- bool ] )
: TRUST-UNSEEN? ( ptr u8 n -- bool )
   CHECKER-OWNER-ABI:VERIFY-TRUST-OFF OWNER-XT TRUST-ACTION execute ;

: RECORD-TRUST ( -- )
   STR-LAST-U @ 0= IF E-VS-BARE-TRUST STATEMENT-STOP THEN
   STR-PREV-U @ 0= IF E-VS-BARE-TRUST STATEMENT-STOP THEN
   STR-PREV-A @ STR-PREV-U @ TRUST-UNSEEN? IF
      DEFER-REPORT @ IF STR-PREV-A @ STR-PREV-U @ REPORT-STRETCH  -1 DEFER-SEEN ! THEN
      EXIT
   THEN
   QUIET-TYPE-CLEAR
   STR-PREV-A @ STR-PREV-U @
   STR-LAST-A @ STR-LAST-U @
   TRUST-SIGNATURE
   QUIET-TYPE-REPORT ;

\ ---- parses: rows -----------------------------------------------------------
\ A ROW BOUNDS WHAT A WORD THAT READS THE SOURCE TAKES: `parses: W n` n raw
\ tokens, `parses-through: W n ( E1 E2 )` n and then every token through the
\ first one equal to a listed terminator, byte for byte, inclusive. The row is
\ trusted, as parse-imm's is, and never compared with W's body: the call is
\ still the run's (W-CHECK-DEFERRED at it), but the scan knows where it ends
\ and goes on after it. Only this scan keeps rows, for the source it checks;
\ the load checks them and keeps nothing (src/core/cell-effects.f parses:).
\ A row is keyed by the checker owner whose scope selected it and by the
\ binding the top-level find selects for W - its symbol and its visible
\ record's offset + 1 (CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF) - never by
\ spelling or symbol alone: a new definition of W, an `undefine` or another
\ owner selects another binding, which no row names. A later row for the same
\ binding is appended and wins; the rows are counted in a checker cell the
\ rollback frame rewinds (src/core/checker.f VERIFY-PARSES-N), as the learned
\ definers' are, so a rewound scope releases its rows. A row's terminators lie
\ in PRS-TERMS, each followed by a blank, past the bytes of the rows before it,
\ copied there as the row is read, before its source can be released; the
\ bytes past the last live row's are free. Growth may move PRS-TERMS.
DYNAMIC-BUFFER PRS-OWNER n                 \ the checker owner whose scope keyed it
DYNAMIC-BUFFER PRS-SYM n                   \ the binding's symbol
DYNAMIC-BUFFER PRS-EFF n                   \ and its visible record's offset + 1
DYNAMIC-BUFFER PRS-COUNT n                 \ the tokens it reads first
DYNAMIC-BUFFER PRS-AT n                    \ where its terminators start in PRS-TERMS
DYNAMIC-BUFFER PRS-LEN n                   \ their bytes; 0 for a `parses:` row
DYNAMIC-BUFFER PRS-TERMS u8

: PRS-OWNER-BASE ( -- n )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ BYTE-VIEW NULL-PTR BYTE-VIEW - ;

CAST: BINDING-ACTION ( n -- [ ptr u8 n -- n n n ] )
\ The binding the load's top-level find selects for a token, asked quietly of
\ the checker: its symbol, its visible record's offset + 1 and its control
\ word, all 0 when the load refuses the token or nothing live binds it.
: TOP-BINDING ( ptr u8 n -- n n n )
   CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF OWNER-XT BINDING-ACTION execute ;

\ The id of the engine word a control word names (checker.f CTL-INTRINSIC).
: BINDING-ID ( n -- n )
   CHECKER-OWNER-ABI:BINDING-ID-MASK and CHECKER-OWNER-ABI:BINDING-ID-SHIFT rshift ;

\ Where the next row's terminators go: past the last ones a live row holds.
: PRS-TERMS-END ( -- n )
   0 VERIFY-PARSES-N @
   BEGIN dup 0 > WHILE
      1 -
      dup PRS-AT @ over PRS-LEN @ +  rot max swap
   REPEAT drop ;

\ The newest row for a binding of this owner + 1, 0 when none.
: PRS-FIND ( n n -- n )
   {: sym:n eff1:n :}
   sym 0=  eff1 0=  or IF 0 EXIT THEN
   PRS-OWNER-BASE {: own:n :}
   VERIFY-PARSES-N @ BEGIN dup 0 > WHILE
      1 -
      dup PRS-SYM @ sym =  over PRS-EFF @ eff1 = and  over PRS-OWNER @ own = and
      IF 1 + EXIT THEN
   REPEAT ;

\ One terminator, copied after the LEN bytes this row has kept so far, with
\ the blank that ends it; the bytes kept.
: PRS-TERM-ADD ( ptr u8 n n -- n )
   {: a:ptr u:n len:n :}
   PRS-TERMS-END len + {: at:n :}
   at u + 1 + PRS-TERMS-RESERVE
   a  at PRS-TERMS  u BYTE-COPY
   32  at u + PRS-TERMS  c!
   len u + 1 + ;

\ One token of a row's list: kept when it is a terminator. The bytes kept, and
\ what the token is: 0 a terminator, 1 the closing `)`, 2 the source's end.
: PRS-LIST-TOKEN ( n -- n n )
   {: len:n :}
   NEXT-RAW {: a:ptr u:n :}
   u 0= IF len 2 EXIT THEN
   a u s" )" CORE-STR= IF len 1 EXIT THEN
   a u len PRS-TERM-ADD 0 ;

\ The list as the engine word reads it (checker.f PARSES-LIST-LOAD): `(`, then
\ tokens through a standalone `)`, at least one before it. The bytes kept, and
\ true when it is malformed.
: PRS-LIST-READ ( -- n bool )
   NEXT-RAW s" (" CORE-STR= 0= IF 0 0 0= EXIT THEN
   0 BEGIN PRS-LIST-TOKEN dup 0= WHILE drop REPEAT
   {: len:n end:n :}
   len  end 2 =  len 0=  or ;

\ `parses: W n` and `parses-through: W n ( E1 E2 )`, known by the identity of
\ the word the token selects (BINDING-ID), never by its spelling. The row is
\ read whole as the engine word reads it (cell-effects.f parses:), checked by
\ the engine's own checks, and kept when it bounds W. The scan knows every
\ token the declarer reads, so nothing of it is the run's.
: PARSES-ROW ( ptr u8 n bool -- )
   {: ka:ptr ku:n through:bool :}
   NEXT-RAW {: ta:ptr tu:n :}
   NEXT-RAW {: ca:ptr cu:n :}
   through IF PRS-LIST-READ ELSE 0 0 0= 0= THEN {: len:n bad:bool :}
   ka ku ta tu ca cu bad PARSES-CHECK {: sym:n eff1:n count:n :}
   sym 0= IF EXIT THEN
   VERIFY-PARSES-N @ {: row:n :}
   row 1 + PRS-OWNER-RESERVE  row 1 + PRS-SYM-RESERVE  row 1 + PRS-EFF-RESERVE
   row 1 + PRS-COUNT-RESERVE  row 1 + PRS-AT-RESERVE  row 1 + PRS-LEN-RESERVE
   PRS-TERMS-END row PRS-AT !
   PRS-OWNER-BASE row PRS-OWNER !  sym row PRS-SYM !  eff1 row PRS-EFF !
   count row PRS-COUNT !  len row PRS-LEN !
   row 1 + VERIFY-PARSES-N ! ;

\ From byte I of PRS-TERMS to END: the blank that ends the terminator at I.
: PRS-TERM-END ( n n -- n )
   {: end:n :}
   BEGIN dup end < IF dup PRS-TERMS c@ 32 <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT ;

\ Is the token the terminator from byte I to J?
: PRS-TERM-AT? ( ptr u8 n n n -- bool )
   {: a:ptr u:n i:n j:n :}
   j i - u <> IF 0 0= 0= EXIT THEN
   a u  i PRS-TERMS u  CORE-STR= ;

\ Is the token one of the row's terminators?
: PRS-TERM? ( ptr u8 n n -- bool )
   {: a:ptr u:n row:n :}
   row PRS-AT @ row PRS-LEN @ + {: end:n :}
   row PRS-AT @
   BEGIN dup end < WHILE
      dup end PRS-TERM-END
      2dup a u 2swap PRS-TERM-AT? IF 2drop 0 0= EXIT THEN
      nip 1 +
   REPEAT drop 0 0= 0= ;

\ What a bounded word reads, consumed as the run reads it, before the scan
\ reads on: the row's count of raw tokens, then, for a through row, every token
\ through the first terminator. The source's end ends it: what is there is
\ consumed, and the word stays the run's.
: PRS-CONSUME ( n -- )
   {: row:n :}
   row PRS-COUNT @ 0 ?do NEXT-RAW nip 0= IF unloop EXIT THEN loop
   row PRS-LEN @ 0= IF EXIT THEN
   BEGIN NEXT-RAW dup 0= IF 2drop EXIT THEN row PRS-TERM? UNTIL ;

\ EXPORT has two documented roles split by package context (dot
\ habu-compiler-pkg-re-688212c1): inside an open package it is the re-export
\ declaration (CHECKER-EXPORT aliases the source's checked effect under its
\ tail); at top level it is the hb-build --repl export directive, which the
\ build strips via COMMENT-EXPORTS before engine load — replay consumes the
\ name and records nothing, exactly like the engine never seeing the line.
\ A re-export duplicates its tail in the current section. The checker asks that
\ only once the name has resolved (src/core/checker.f EXPORT-RECORD), as --load
\ does, so its refusal is caught here and kept at the name as written.
\ The tail a token names: past its one non-edge colon when it is qualified.
: TOKEN-TAIL ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 1 > IF
      u 1 - 1 ?do
         a i + c@ 58 = IF a i + 1 +  u i - 1 -  unloop EXIT THEN
      loop
   THEN
   a u ;

\ An export is the same word under another tail, so the row of the word it
\ re-exports, FROM (its row + 1, 0 for none), is the export's too, as the
\ checker copies the word's other facts (checker.f EXPORT-META-COPY): appended
\ under the binding of the record the export made, as the top-level find
\ selects it by its package and tail. Its bare tail is no such selection: in
\ the package it can still name the package's own private word, the one an
\ `EXPORT` of that word publishes. A word that only calls a bounded word takes
\ no row.
: EXPORT-PARSES ( n ptr u8 n -- )
   {: from:n a:ptr u:n :}
   from 0= IF EXIT THEN
   a u TOKEN-TAIL RETAINED-SYM {: rec:n :}
   rec 0= IF EXIT THEN
   rec SYM-IDENTITY drop QUAL-SPELLING TOP-BINDING drop {: sym:n eff1:n :}
   sym rec <>  eff1 0=  or IF EXIT THEN
   from 1 - {: src:n :}
   VERIFY-PARSES-N @ {: row:n :}
   row 1 + PRS-OWNER-RESERVE  row 1 + PRS-SYM-RESERVE  row 1 + PRS-EFF-RESERVE
   row 1 + PRS-COUNT-RESERVE  row 1 + PRS-AT-RESERVE  row 1 + PRS-LEN-RESERVE
   PRS-OWNER-BASE row PRS-OWNER !  sym row PRS-SYM !  eff1 row PRS-EFF !
   src PRS-COUNT @ row PRS-COUNT !
   src PRS-AT @ row PRS-AT !  src PRS-LEN @ row PRS-LEN !
   row 1 + VERIFY-PARSES-N ! ;

\ The record an export made is the open section's, under the tail the checker
\ recorded the re-exported name under; the token that declared it is that
\ name as written, from byte at.
: EXPORT-DEFINED ( ptr u8 n n -- )
   {: name:ptr nameu:n at:n :}
   name nameu RECORD-SYM? SYM-IDENTITY drop 2swap 2drop RETAINED-SYM
   {: sym:n :}
   sym 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu sym s" " at at nameu + DEF-EXPORT ON-DEFINITION ;

: RECORD-EXPORT ( -- )
   NAME-TOKEN TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF EXIT THEN
   name nameu TOP-BINDING drop PRS-FIND {: from:n :}
   name DEF-NAME-A !  nameu DEF-NAME-U !
   name nameu at nameu ARM   \ the checker spells the record by the tail it keeps
   [: DEF-NAME-A @ DEF-NAME-U @ CHECKER-EXPORT ;] catch
   DISARM
   {: rc:n :}
   rc E-DUP-DEFINITION = IF nameu DUPLICATE! EXIT THEN
   rc 0<> IF rc throw THEN
   name nameu at EXPORT-DEFINED
   from name nameu EXPORT-PARSES ;

\ The core resolver answers the canonical path and require-known state. A
\ require publishes that path before descending, so recursive requires stop at
\ the same point as the native loader. An include always descends.
: COMPOSE-PENDING ( -- )
   COMPOSE-PEND-A @ COMPOSE-PEND-U @
   COMPOSE-PEND-PATH-A @ COMPOSE-PEND-PATH-U @ COMPOSE-FILE ;

\ The owner record's file field holds a raw execution token.
CAST: FILE-ACTION ( n -- [ [ -- ] -- ] )

: COMPOSE-LOADED ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n path:ptr pathu:n :}
   a COMPOSE-PEND-A !  u COMPOSE-PEND-U !
   path COMPOSE-PEND-PATH-A !  pathu COMPOSE-PEND-PATH-U !
   [: COMPOSE-PENDING ;]
   CHECKER-OWNER-ABI:VERIFY-FILE-OFF OWNER-XT FILE-ACTION execute ;

: COMPOSE-OPEN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ CORE-STR= IF
      path pathu COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @
      [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-SUPPLIED EXIT
   THEN
   path pathu [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-BYTES ;

: COMPOSE-INCLUDED ( ptr u8 n -- )
   SOURCE-ROOT:RESOLVE drop COMPOSE-OPEN ;

: COMPOSE-REQUIRED ( ptr u8 n -- )
   SOURCE-ROOT:RESOLVE IF 2drop EXIT THEN
   REQUIRE-STORE COMPOSE-OPEN ;

: COMPOSE-SCRIPT-REQUIRED ( ptr u8 n -- )
   SOURCE-ROOT:ENTRY-RESOLVE IF 2drop EXIT THEN
   REQUIRE-STORE COMPOSE-OPEN ;

: COMPOSE-PROVIDED ( ptr u8 n -- )
   SOURCE-ROOT:RESOLVE IF 2drop EXIT THEN
   REQUIRE-STORE 2drop ;

: COMPOSE-STRING-PATH ( -- ptr u8 n )
   STR-LAST-U @ 0= IF E-DISC-DYNAMIC throw THEN
   STR-LAST-A @ STR-LAST-U @ ;

: COMPOSE-RAW-PATH ( -- ptr u8 n )
   NEXT-RAW dup 0= IF E-DISC-DYNAMIC throw THEN ;

\ A quiet composition loads the file a loader word names under the word's
\ claim (CLAIMED-LOAD): a fault no loader in a file below claimed stands at the
\ word, when the file is not there or does not read (E-SOURCE-READ) and when
\ its path resolves past PATH-CAP (E-PATH-RANGE), which discovery refuses as a
\ capacity exceeded (E-DISC-CAPACITY) and for which a lenient file loads
\ nothing. Any other fault keeps the place where it was made.
PTR-VARIABLE LOAD-A                           \ the path the claimed load loads
variable LOAD-U

: LOAD$ ( -- ptr u8 n )
   LOAD-A @ LOAD-U @ ;

\ The load of LOAD$ that includes (true) or requires its file.
: FILE-LOAD ( bool -- [ -- ] )
   IF [: LOAD$ COMPOSE-INCLUDED ;] EXIT THEN
   [: LOAD$ COMPOSE-REQUIRED ;] ;

\ Run Q, the load a loader word wu bytes long at byte w makes.
: CLAIMED-LOAD ( [ -- ] n n -- ) {: q w:n wu:n :}
   q catch {: rc:n :}
   rc E-PATH-RANGE = IF
      LENIENT? IF EXIT THEN
      E-DISC-CAPACITY
   ELSE
      rc
   THEN {: code:n :}
   code E-SOURCE-READ =  code E-DISC-CAPACITY =  or  FAULT-LEN @ 0=  and IF
      w TOKEN-BYTE !  wu FAULT-LEN !
   THEN
   code 0<> IF code throw THEN ;

\ `include PATH` (inc true) or `require PATH` at top level, its word u bytes
\ long at TOKEN-BYTE.
: QUIET-WORD-LOAD ( n bool -- ) {: u:n inc:bool :}
   TOKEN-BYTE @ {: at:n :}
   u LOADER-OPERAND {: p:ptr pu:n load:bool :}
   load 0= IF EXIT THEN
   p LOAD-A !  pu LOAD-U !
   inc FILE-LOAD at u CLAIMED-LOAD ;

\ A string loader word at top level, u bytes long at TOKEN-BYTE: refused by
\ the literal before it as discovery refuses it (LITERAL-LOAD?), else Q loads
\ that literal's path.
: QUIET-STRING-LOAD ( n [ -- ] -- ) {: u:n q :}
   STR-LAST-KIND @ STR-LAST-U @ u LITERAL-LOAD? 0= IF EXIT THEN
   STR-LAST-A @ LOAD-A !  STR-LAST-U @ LOAD-U !
   q TOKEN-BYTE @ u CLAIMED-LOAD ;

\ A loader word at top level in a quiet composition, true when the token is
\ one: a load it makes is claimed, and `provided` records its path as loaded
\ and reads no file.
: QUIET-TOP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" STR=CI IF u true QUIET-WORD-LOAD true EXIT THEN
   a u s" require" STR=CI IF u false QUIET-WORD-LOAD true EXIT THEN
   a u s" included" STR=CI IF u true FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   a u s" required" STR=CI IF u false FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   a u s" script-required" STR=CI IF
      u [: LOAD$ COMPOSE-SCRIPT-REQUIRED ;] QUIET-STRING-LOAD true EXIT
   THEN
   a u s" provided" STR=CI IF
      u [: LOAD$ COMPOSE-PROVIDED ;] QUIET-STRING-LOAD true EXIT
   THEN
   false ;

\ The loads that wait in this file, in the order of their loaders. A file one
\ of them loads keeps its own entries above them, and they end with it. Its
\ entries may grow PEND-PATH and move it, so an entry's path is fetched when its
\ turn comes, and the resolver copies it before the file is read.
: PEND-RELEASE ( -- )
   PEND-N @ PEND-BASE @ ?do
      TICK-REMAINDER @ IF leave THEN
      i PEND-AT @ PEND-PATH i PEND-U @
      QUIET @ IF
         LOAD-U !  LOAD-A !
         i PEND-INC @ 0<> FILE-LOAD  i PEND-WORD-AT @  i PEND-WORD-U @  CLAIMED-LOAD
      ELSE
         i PEND-INC @ 0<> IF COMPOSE-INCLUDED ELSE COMPOSE-REQUIRED THEN
      THEN
   loop
   PEND-BASE @ PEND-N ! ;

\ The scope the file being read opened itself: a `package`, and the `using`s it
\ opened outside one. `;package` closes the package with the usings inside it;
\ `;using` closes the last using outside it.
variable FILE-PKG
variable FILE-USE

: FILE-SCOPE-STEP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" package" STR=CI IF 1 FILE-PKG ! EXIT THEN
   a u s" ;package" STR=CI IF 0 FILE-PKG ! EXIT THEN
   FILE-PKG @ 0<> IF EXIT THEN
   a u s" using" STR=CI IF FILE-USE @ 1 + FILE-USE ! EXIT THEN
   a u s" ;using" STR=CI FILE-USE @ 0 > and IF FILE-USE @ 1 - FILE-USE ! THEN ;

: FILE-NEUTRAL? ( -- bool )
   FILE-PKG @ 0= FILE-USE @ 0= and ;

: COMPOSE-TOP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   COMPOSE-ON @ 0= IF 0 0= 0= EXIT THEN
   a u s" include" STR=CI  a u s" require" STR=CI or
   a u s" included" STR=CI or  a u s" required" STR=CI or
   a u s" script-required" STR=CI or  a u s" provided" STR=CI or IF
      TICK-CONTEXT-UNKNOWN
   THEN
   QUIET @ IF a u QUIET-TOP? EXIT THEN
   a u s" include" STR=CI IF COMPOSE-RAW-PATH COMPOSE-INCLUDED 0 0= EXIT THEN
   a u s" require" STR=CI IF COMPOSE-RAW-PATH COMPOSE-REQUIRED 0 0= EXIT THEN
   a u s" included" STR=CI IF COMPOSE-STRING-PATH COMPOSE-INCLUDED 0 0= EXIT THEN
   a u s" required" STR=CI IF COMPOSE-STRING-PATH COMPOSE-REQUIRED 0 0= EXIT THEN
   a u s" script-required" STR=CI IF COMPOSE-STRING-PATH COMPOSE-SCRIPT-REQUIRED 0 0= EXIT THEN
   a u s" provided" STR=CI IF COMPOSE-STRING-PATH COMPOSE-PROVIDED 0 0= EXIT THEN
   0 0= 0= ;

\ A package primitive row has two closers, and this verifier models them exactly
\ as the source lexer does:
\ `PPRIM;` interns the axiom into the package public wordlist and `CLOSE-PRIVATE`
\ interns it into the package private one. Visibility is part of the row, not a
\ different row shape, so either token ends a `PPRIM:` row. A bare `PRIM:` row has
\ no package to be private in, so it declares no alternate closer and
\ `CLOSE-PRIVATE` stays an ordinary effect token there.
: ROW-CLOSER? ( ptr u8 n ptr u8 n -- bool ) {: end:ptr endu:n alt:ptr altu:n :}
   TOKEN-A @ TOKEN-U @ end endu STR=CI IF 0 0= EXIT THEN
   altu 0= IF 0 0= 0= EXIT THEN
   TOKEN-A @ TOKEN-U @ alt altu STR=CI ;

\ PRIM:/PPRIM: bodies use the canonical body scanner so parsing words consume
\ their comments, strings, and raw operands before a live closer is considered.
\ A row declares no locals, so none of the last body's stay live in it, and as
\ every body reader it starts clear of the last body's literal and line: that
\ literal may lie in decoded bytes a decode since has moved. Discovery refuses a
\ loader word with no literal before it, so tools/check.f never reads one that
\ opens a row. A row the source ends inside, before its name, package or
\ closer, stops the scan at its opener, the byte RECORD-PRIM or RECORD-PPRIM was
\ entered at.
: ROW-UNCLOSED ( n -- )
   TOKEN-BYTE !
   E-MALFORMED-REGISTRY-ROW throw ;

: RECORD-PRIM-ROW ( n ptr u8 n ptr u8 n -- )
   {: at:n end:ptr endu:n alt:ptr altu:n :}
   BODY-LOAD-RESET
   LOCALS-RESET
   NEXT-RAW dup 0= IF at ROW-UNCLOSED THEN
   2drop
   BEGIN
      BODY!
      TOKEN-U @ 0= IF at ROW-UNCLOSED THEN
      end endu alt altu ROW-CLOSER? IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: RECORD-PRIM ( -- )
   TOKEN-BYTE @ s" PRIM;" s" " RECORD-PRIM-ROW ;

: RECORD-PPRIM ( -- )
   TOKEN-BYTE @ {: at:n :}
   NEXT-RAW dup 0= IF at ROW-UNCLOSED THEN
   2drop
   at s" PPRIM;" s" CLOSE-PRIVATE" RECORD-PRIM-ROW ;

: STRUCTURE-END? ( ptr u8 n -- bool )
   s" END-STRUCTURE" STR=CI ;

: STRUCTURE-PTR-FIELD? ( ptr u8 n -- bool )
   s" PTR-FIELD:" STR=CI ;

: STRUCTURE-CFIELD? ( ptr u8 n -- bool )
   s" CFIELD:" STR=CI ;

: STRUCTURE-CELL-FIELD? ( ptr u8 n -- bool )
   s" +FIELD" STR=CI ;

: TRUST-STRUCTURE-FIELD ( ptr u8 n ptr u8 n -- bool )
   DECL-SIGNATURE ;

\ A field, declared by its definer's token kind and named by the next token.
: RECORD-STRUCTURE-FIELD ( ptr u8 n ptr u8 n -- )
   {: kind:ptr kindu:n sig:ptr sigu:n :}
   NAME-TOKEN TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   name nameu at nameu ARM
   name nameu sig sigu TRUST-STRUCTURE-FIELD
   DISARM
   IF kind kindu name nameu sig sigu at at nameu + DEF-WORD DEFINED THEN ;

\ Record the size word (`-- n`) then each field accessor with its runtime effect
\ so BEGIN-STRUCTURE layouts self-certify their field uses.
: RECORD-STRUCTURE ( -- )
   NAME-TOKEN TOKEN-BYTE @ {: name:ptr nameu:n at:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   name nameu at nameu ARM
   name nameu s" -- n" DECL-SIGNATURE
   DISARM
   IF name nameu s" -- n" at DEF-CONSTANT DEFINED-HERE THEN
   BEGIN
      NEXT-SCAN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup STRUCTURE-END? IF 2drop EXIT THEN
      \ A pointer field's POINTEE is independent of the record's element type: a
      \ cell record may hold a byte pointer. `ptr ptr a` tied the two together,
      \ so a record read as cells forced every pointer field to point at cells —
      \ the `ptr-field` primitive itself is `( ptr a n -- ptr ptr b )`.
      2dup STRUCTURE-PTR-FIELD? IF s" ptr a -- ptr ptr b" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CFIELD? IF s" ptr a -- ptr u8" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CELL-FIELD? IF s" ptr a -- ptr a" RECORD-STRUCTURE-FIELD ELSE
      2drop
      THEN THEN THEN
   AGAIN ;

\ `generates: D ( effect )` - the row for a definer that writes its word as text
\ (checker.f CHECKER-GENERATES). The row is read as the engine word reads it:
\ the name by parse-name (NAME-TOKEN), the effect as a definition head's
\ signature (REQUIRE-SIGNATURE), within the engine's bound. The registrar
\ resolves D itself and raises the refusals FIND-SYM defers; FIND-SYM keys this
\ scan's table, and a row it already holds for D - its clause or an earlier
\ row - is part of what the checker means by D already stating what it makes.
: RECORD-GENERATES ( -- )
   NAME-TOKEN
   {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   REQUIRE-SIGNATURE
   {: sig:ptr sigu:n :}
   sigu GENR-SIG-CAP > IF E-VS-EFFECT-SIZE STATEMENT-STOP THEN
   name nameu FIND-SYM
   {: sym:n :}
   name nameu sig sigu sym DEFINER-FIND 0<> GENERATES-SIGNATURE nip 0= IF EXIT THEN
   sig sigu sym DEFINER-ADD ;

\ `FUNCTION: NAME symbol ( effect ) ... ;FUNCTION` (lib/ffi-abi.f) makes NAME
\ from its declaration group, so the group is NAME's effect with the one rewrite
\ the declarer applies (ffi-abi.f OUT-TOKEN): an `i32` result is a cell. Inputs
\ stay verbatim. The group is read raw, because NEXT-SCAN skips it as a comment.
\ The live declaration pins this reading: test/certify-does-definer.f section 10.
\ The effect is rebuilt in a byte row of its own, which grows with the group:
\ BODY-BUF holds only runs read out of the source (BODY-APPEND), and the
\ rewritten `n` is not one.
variable FFI-OUT
DYNAMIC-BUFFER FFI-SIG u8
variable FFI-SIG-U

: FFI-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   FFI-SIG-U @
   {: at:n :}
   at u + 1 + FFI-SIG-RESERVE
   a  at FFI-SIG  u BYTE-COPY
   32  at u + FFI-SIG  c!
   at u + 1 + FFI-SIG-U ! ;

: FFI-TOKEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" --" CORE-STR= IF -1 FFI-OUT ! THEN
   FFI-OUT @ 0<> a u s" i32" CORE-STR= and IF s" n" FFI-APPEND EXIT THEN
   a u FFI-APPEND ;

: FFI-SIGNATURE ( -- ptr u8 n )
   NEXT-RAW s" (" CORE-STR= 0= IF E-VS-MISSING-SIGNATURE STATEMENT-STOP THEN
   1 FFI-SIG-RESERVE                          \ an empty group answers a string in the row
   0 FFI-SIG-U !
   0 FFI-OUT !
   BEGIN
      NEXT-RAW
      dup 0= IF E-VS-MISSING-SIGNATURE STATEMENT-STOP THEN
      2dup s" )" CORE-STR= IF 2drop 0 FFI-SIG FFI-SIG-U @ EXIT THEN
      FFI-TOKEN
   AGAIN ;

: RECORD-FFI-FUNCTION ( -- )
   NEXT-SCAN TOKEN-BYTE @
   {: name:ptr nameu:n at:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu QUIET-NAME
   NEXT-SCAN nip 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
   FFI-SIGNATURE {: sig:ptr sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DECL-SIGNATURE
   DISARM
   IF name nameu sig sigu at DEF-WORD DEFINED-HERE THEN ;

: RECORD-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" STR=CI IF RECORD-PACKAGE 0 0= EXIT THEN
   a u s" public" STR=CI IF RECORD-PUBLIC 0 0= EXIT THEN
   a u s" private" STR=CI IF RECORD-PRIVATE 0 0= EXIT THEN
   a u s" ;package" STR=CI IF RECORD-END-PACKAGE 0 0= EXIT THEN
   a u s" using" STR=CI IF RECORD-USING 0 0= EXIT THEN
   a u s" ;using" STR=CI IF RECORD-END-USING 0 0= EXIT THEN
   a u s" deftype" STR=CI IF RECORD-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" STR=CI IF RECORD-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" STR=CI IF
      [: RECORD-VALUE-RECORD ;] catch VALUE-RECORD-OUTCOME 0 0= EXIT THEN
   a u s" begin-structure" STR=CI IF RECORD-STRUCTURE 0 0= EXIT THEN
   a u s" structure" STR=CI IF RECORD-STRUCTURE-DECL 0 0= EXIT THEN
   a u s" newtype" STR=CI IF RECORD-NEWTYPE 0 0= EXIT THEN
   a u s" sumtype" STR=CI IF
      [: RECORD-SUMTYPE ;] catch LEGACY-TYPE-OUTCOME 0 0= EXIT THEN
   a u s" enum" STR=CI IF RECORD-ENUM 0 0= EXIT THEN
   a u s" product" STR=CI IF
      [: RECORD-PRODUCT ;] catch LEGACY-TYPE-OUTCOME 0 0= EXIT THEN
   a u s" LAYOUT-BUFFER" STR=CI IF
      [: RECORD-LAYOUT-BUFFER ;] catch STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" DEFER-LAYOUT-BUFFER" STR=CI IF
      [: RECORD-DEFER-LAYOUT-BUFFER ;] catch STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" TYPED-BUFFER" STR=CI IF
      [: RECORD-TYPED-BUFFER ;] catch STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" TYPED-VARIABLE" STR=CI IF
      [: RECORD-TYPED-VARIABLE ;] catch STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" DYNAMIC-BUFFER" STR=CI IF
      [: RECORD-DYNAMIC-BUFFER ;] catch STORAGE-OUTCOME 0 0= EXIT THEN
   \ `constant` bakes one physical cell, so its trust is the one-cell `-- a`
   \ model — identical to native C-CONSTANT, all-errors (which funnels here),
   \ and public-signatures. This is the PERMANENT contract (TFAM 12 verdict
   \ 2026-07-09, habu-tfam-12-layout): the interpret stack is untyped by
   \ design, so no path has a sound shape source, and a wider-than-cell layout
   \ value never lands there (DNAME-WIDE dispatch gate). Any layout USE of the
   \ constant fails closed downstream; parity locked by check-all-errors-test
   \ const-layout-narrow.
   a u s" constant" STR=CI IF s" -- a" DEF-CONSTANT RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" create" STR=CI IF s" -- ptr a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" STR=CI IF s" -- ptr a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PTR-VARIABLE" STR=CI IF s" -- ptr ptr a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PERSISTED-PTR-VARIABLE" STR=CI IF s" -- ptr ptr a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   \ The declared-pointee forms: the clause names the pointee, so the effect has
   \ no type variable and the raw registration seals nothing. Same word, same row
   \ as the native path publishes through `trust-raw`.
   a u s" PTR-U8-TABLE" STR=CI IF s" -- ptr ptr u8" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PERSISTED-PTR-U8-TABLE-VARIABLE" STR=CI IF s" -- ptr ptr ptr u8" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   \ The reserved-offset cell: the offset token precedes the name, as the table
   \ count does, so the created word is still the NEXT token and the row is the
   \ definer's declared clause.
   a u s" RESERVED-PTR-U8-CELL" STR=CI IF s" -- ptr ptr u8" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" STR=CI IF TRUST-DEFER 0 0= EXIT THEN
   a u s" PRIM:" STR=CI IF RECORD-PRIM 0 0= EXIT THEN
   a u s" PPRIM:" STR=CI IF RECORD-PPRIM 0 0= EXIT THEN
   a u s" trusted:" STR=CI IF TRUSTED-DEFINITION 0 0= EXIT THEN
   a u s" cast:" STR=CI IF
      [: CAST-DECLARATION ;] catch IDENTITY-OUTCOME 0 0= EXIT THEN
   a u s" linear:" STR=CI IF
      [: LINEAR-DECLARATION ;] catch IDENTITY-OUTCOME 0 0= EXIT THEN
   a u s" undefine" STR=CI IF TICK-CONTEXT-UNKNOWN UNDEFINE-WORD 0 0= EXIT THEN
   a u s" trust" STR=CI IF RECORD-TRUST 0 0= EXIT THEN
   a u s" generates:" STR=CI IF RECORD-GENERATES 0 0= EXIT THEN
   a u s" FUNCTION:" STR=CI IF RECORD-FFI-FUNCTION 0 0= EXIT THEN
   a u s" immediate" STR=CI IF TICK-CONTEXT-UNKNOWN 0 0= EXIT THEN
   a u s" export" STR=CI IF RECORD-EXPORT 0 0= EXIT THEN
   \ … then a word that renders source when it runs (`;FUNCTION`, CMD:COMMAND,
   \ TASK:+USER): what it defines is text this scan never reads, so the checker
   \ marks the statement's wordlist and leaves a later definition or top-level
   \ token naming a word nothing resolves to the run (src/core/checker.f
   \ CTL-RENDERS, UNSEEN-MARK$). It marks whether or not an arm below then
   \ records the product a `generates:` row declares (RECORD-GENERATES): that
   \ name resolves and is checked against the row, and the mark covers what no
   \ row declares, such as COMMAND's NAME#VEC and NAME#BUF. A statement no arm
   \ takes is TOP-TOKEN's, which marks it again and resolves it.
   a u MARK-RENDERS RENDERS-MARK? drop
   \ … and last, a definer this pre-pass learned from a `does>` definition or a
   \ `generates:` row earlier in the closure. The created word is the NEXT
   \ token, as it is for `constant` above - the definer's own arguments precede
   \ it - and the effect is the clause's or the row's, registered with the same
   \ raw seal the storage definers use.
   a u DEFINER-EFFECT dup 0<> IF
      TICK-CONTEXT-UNKNOWN DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT
   THEN 2drop
   \ … and last of all, a definer this pre-pass never read: one compiled in the
   \ checking process itself, whose clause the checker certified and kept. The
   \ token resolves through the same FIND-SYM every other name does, so the
   \ qualified and the bare-under-`using` spelling reach the one row.
   a u CREATED-TRUST-NEXT? IF TICK-CONTEXT-UNKNOWN 0 0= EXIT THEN
   0 0= 0= ;

\ The colon definition just scanned, once its body was judged.
: COLON-DEFINED ( -- )
   TOP-CUR-A @ TOP-CUR-U @  DEF-NAME-A @ DEF-NAME-U @  DEF-SIG-A @ DEF-SIG-U @
   DEF-NAME-BYTE @ dup DEF-NAME-U @ + DEF-WORD DEFINED ;

\ A refused definition's signature and body, to its `;`, skipped as a trusted
\ body is, so nothing of it is checked or recorded.
: SKIP-DEFINITION ( -- )
   E-VS-UNTERMINATED-DEFINITION SCAN-SIG drop 2drop
   BODY-LOAD-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      SKIP-DEF-TOKEN
   AGAIN ;

: VERIFY-DEFINITION ( -- )
   TICK-DEF-LATCH
   BODY-RESET
   NAME-TOKEN TOKEN-U !  TOKEN-A !
   TOKEN-U @ 0= if E-MISSING-NAME throw then
   DEF-NAME!
   DEF-NAME-A @ DEF-NAME-U @ QUIET-NAME
   DEF-NAME-A @ DEF-NAME-U @ REFUSE-DUPLICATE IF SKIP-DEFINITION EXIT THEN
   WRAP-RESET
   BODY-LOAD-RESET
   LOCALS-RESET
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE DEF-SIG-U !  DEF-SIG-A !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF
         DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ false REFUSE-SHAPE IF EXIT THEN
         VERIFY-NAMED-BODY dup REPORT-DEFERRED dup -1 = IF VERIFY-WRAPPER THEN TALLY
         TICK-REMAINDER @ 0= IF COLON-DEFINED THEN EXIT
      THEN
      LOCAL-TOKEN? 0= IF
         TOKEN-A @ TOKEN-U @ s" does>" STR=CI IF
            DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ true REFUSE-SHAPE IF
               SKIP-DEFINITION EXIT
            THEN
            VERIFY-DOES IF COLON-DEFINED THEN
            TICK-REMAINDER @ IF 2 TALLY THEN EXIT
         THEN
         TOKEN-A @ TOKEN-U @ WRAP-TOKEN
      THEN
      TICK-BODY-STEP
      APPEND-BODY-TOKEN
   AGAIN ;

\ ---- top-level tokens --------------------------------------------------------
\ A token no arm of the scan acts on is one the load runs, and `'` ticks its
\ operand. Under composition - the verifier's child, --all-errors and check.f's
\ pre-pass - the checker answers what the load does with each
\ (CHECKER-VERIFY-TOP): a number the engine's reader takes (num-parse) is done,
\ and any other token resolves as the load resolves it, in source order, over
\ the store this scan has filled; `char`'s operand is no name. None of it runs.
\
\ A word that may read the source after it - one that parses or is deferred -
\ takes tokens no scan can know without running it, and any token after it may
\ be one, so the scan stops at it (TOP-OPAQUE). A name a rendering statement
\ may define is the run's to judge: from it to the next token the scan acts on
\ - a definition, a loader, a definer of the table above, where the scan
\ already takes a statement to start - the stretch is deferred to the run, and
\ nothing in it is resolved. A word that renders source opens no stretch: it
\ reads only the text it renders. The verifier's child, which opts in
\ (REPORT-DEFERRALS), has each stop and each such stretch reported once, at its
\ token (W-CHECK-DEFERRED, verdict deferred), and answers `deferred` when
\ nothing is refused. A refusal opens a stretch too, unreported: the load stops
\ at it, so the operands of a misspelt parsing word, or the rest of a
\ declaration its definer misread, are not refused after it.
CAST: TOP-ACTION ( n -- [ ptr u8 n bool -- n ] )
: TOP-VERDICT ( ptr u8 n bool -- n )
   NCOMP-DISPATCH:DECL-VERIFY-TOP-OFF OWNER-XT TOP-ACTION execute ;
variable TOP-DEFER                               \ a stretch is open
PTR-VARIABLE TOP-DEFER-A  variable TOP-DEFER-U   \ its opener while its report is due, else 0

: TOP-CLOSE ( -- )
   0 TOP-DEFER !  0 TOP-DEFER-U ! ;

\ The open stretch's report, made at the scan's next token or at the source's
\ end: its opener is a token the run reads.
: TOP-DUE ( -- )
   TOP-DEFER-U @ 0= IF EXIT THEN
   DEFER-REPORT @ IF TOP-DEFER-A @ TOP-DEFER-U @ REPORT-STRETCH  -1 DEFER-SEEN ! THEN
   0 TOP-DEFER-U ! ;

\ A word that may read the source after it and states no operand shape: what
\ it reads may be any of the rest, and may change the scope of what follows, so
\ the scan reports it at once and stops, in this file and in each file whose
\ load reached it (TICK-REMAINDER), keeping what it found before the word.
: TOP-OPAQUE ( ptr u8 n -- )
   {: a:ptr u:n :}
   DEFER-REPORT @ IF a u REPORT-STRETCH  -1 DEFER-SEEN ! THEN
   true TICK-REMAINDER ! ;

\ A token that runs a word that may read the source after it (TOP-VERDICT 2):
\ a declarer reads its row; a word a row bounds is reported at once, as the
\ run's, and what it reads is consumed; any other stops the scan (TOP-OPAQUE).
\ A deferred word stays opaque whatever row names it.
: TOP-PARSER ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u TOP-BINDING {: sym:n eff1:n ctl:n :}
   ctl BINDING-ID {: id:n :}
   id CHECKER-OWNER-ABI:BINDING-PARSES-ID = IF a u 0 0= 0= PARSES-ROW EXIT THEN
   id CHECKER-OWNER-ABI:BINDING-THROUGH-ID = IF a u 0 0= PARSES-ROW EXIT THEN
   ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0<> IF a u TOP-OPAQUE EXIT THEN
   sym eff1 PRS-FIND {: row:n :}
   row 0= IF a u TOP-OPAQUE EXIT THEN
   DEFER-REPORT @ IF a u REPORT-STRETCH  -1 DEFER-SEEN ! THEN
   row 1 - PRS-CONSUME ;

\ The checker's answer for a token: a word that may read on is TOP-PARSER's,
\ and a refusal, or a name the run must judge, opens the stretch at the token.
: TOP-RESOLVE ( ptr u8 n bool -- )
   {: a:ptr u:n runs:bool :}
   a u runs TOP-VERDICT {: v:n :}
   v -1 = IF EXIT THEN
   v 2 = IF a u TOP-PARSER EXIT THEN
   -1 TOP-DEFER !
   v 0= IF a TOP-REFUSED ! EXIT THEN
   a TOP-DEFER-A !  u TOP-DEFER-U ! ;

\ The token the cursor waits on is a top-level statement, or a top-level tick's
\ operand: the checker offers the spellings that bind there now, before the
\ token is consumed, unless a stretch left to the run holds it.
: CSR-TOP ( -- )
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-HIT? 0= IF EXIT THEN
   CSR-STATE @ CSR-PART = IF CSR-AT @ CSR-TOK @ - ELSE 0 THEN {: n:n :}
   CSR-DONE CSR-STATE !
   TOP-DEFER @ 0<> IF EXIT THEN
   SOURCE@ CSR-TOK @ BASE-BYTE @ - +  n  CHECKER-OWNER-ABI:VISIBLE-TOP
   ['] CSR-VISIT
   NCOMP-DISPATCH:DECL-VERIFY-EACH-VISIBLE-OFF OWNER-XT VISIBLE-ACTION execute ;

\ `'` and `char` take the next token. `'` resolves it as the load does; `char`'s
\ is no name.
: TOP-OPERAND ( ptr u8 n -- )
   {: a:ptr u:n :}
   OPERAND {: o:ptr ou:n :}
   a u CHAR-KEYWORD? IF EXIT THEN
   CSR-TOP
   COMPOSE-ON @ 0=  TOP-DEFER @ 0<>  or IF EXIT THEN
   o ou 0 0= 0= TOP-RESOLVE ;

\ Any other token: a number the engine's reader takes is done. A word that may
\ define words this scan never reads when it runs - it renders source
\ (`;FUNCTION`, CMD:COMMAND, TASK:+USER, `evaluate`), calls `create`, or is
\ deferred - has the checker mark the statement's wordlist, with or without
\ composition, and a name that resolves nowhere after it, in a definition or at
\ top level, is left to the run (src/core/checker.f CTL-RENDERS, CTL-CREATES,
\ UNSEEN-MARK$). Only here does a word that calls `create` mark: a definer arm
\ that takes the statement (RECORD-DEFINER?) records the name it makes.
: TOP-TOKEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u num-parse nip nip IF EXIT THEN
   a u STRING-OPENER? 0= IF TICK-CONTEXT-UNKNOWN THEN
   a u MARK-UNSEEN RENDERS-MARK? drop
   COMPOSE-ON @ 0=  TOP-DEFER @ 0<>  or IF EXIT THEN
   a u 0 0= TOP-RESOLVE ;

\ A quiet reader cannot run this reached top-level word. Ask the selected
\ binding for registration/rendering effects before a definer consumes the
\ statement; a tick or an uncalled body never reaches this path.
: QUIET-NOMINAL ( ptr u8 n -- ) {: a:ptr u:n :}
   QUIET @ 0= IF EXIT THEN
   a u num-parse nip nip IF EXIT THEN
   a u STRING-OPENER? IF EXIT THEN
   a u MARK-NOMINAL RENDERS-MARK? drop ;

\ A declaration a definer of the table above reads closes the stretch, unless
\ the checker counted a refusal while it was read (MULTI-ERR-N, where refusals
\ do not throw): then the stretch opens, unreported.
: TOP-DEFINER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   MULTI-ERR-N @ {: before:n :}
   a u TICK-NATIVE-DEFINER? {: native:bool :}
   a u RECORD-DEFINER? 0= IF 0 0= 0= EXIT THEN
   native 0= IF TICK-CONTEXT-UNKNOWN THEN
   TOP-CLOSE
   MULTI-ERR-N @ before <> IF -1 TOP-DEFER ! THEN
   0 0= ;

\ `kernel:` is the engine's synonym for `:` (src/habu/definers.f DEFINE?),
\ matched case-folded as the engine's keywords are.
: COLON? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" :" CORE-STR=  a u s" kernel:" STR=CI or ;

\ A parsing keyword and its operand are one value on the interpret stack, and
\ the keyword stays the token before whatever follows: `char 0 TYPED-BUFFER B n`
\ hands the count reader `char`, which names the count's word. A load a
\ definition makes waits for the first statement after which the file has
\ closed every scope it opened, and for the file's end at the latest. A quiet
\ composition refuses a top-level `UNDEFINE-IF-DEFINED` by the literal before
\ it, as in a body (QUIET-RETIRE).
: VERIFY-SOURCE ( -- )
   SCAN-RESET
   0 DUPLICATE-U !
   SOURCE@ SOURCE-U @ BASE-LINE @ BASE-COL @ BASE-BYTE @ CHECKER-VERIFY-SOURCE!
   NULL-PTR TOP-PREV-A !  0 TOP-PREV-U !  NULL-PTR TOP-REFUSED !
   0 FILE-PKG !  0 FILE-USE !  PEND-N @ PEND-BASE !
   TOP-CLOSE
   BEGIN
      NEXT-SCAN dup 0 > TICK-REMAINDER @ 0= and WHILE
      TOP-DUE
      2dup TOP-CUR-U ! TOP-CUR-A !
      CSR-TOP
      2dup FILE-SCOPE-STEP
      2dup STR-LAST-KIND @ STR-LAST-A @ STR-LAST-U @ QUIET-RETIRE
      2dup TOP-PARSER? IF TOP-OPERAND ELSE
      2dup COLON? IF 2drop VERIFY-DEFINITION TOP-CLOSE ELSE
      2dup COMPOSE-TOP? IF 2drop TOP-CLOSE ELSE
      2dup QUIET-NOMINAL
      2dup TOP-DEFINER? IF 2drop ELSE TOP-TOKEN THEN THEN THEN THEN
      FILE-NEUTRAL? TICK-REMAINDER @ 0= and IF PEND-RELEASE THEN
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT 2drop
   TICK-REMAINDER @ IF EXIT THEN
   CSR-TOP                                      \ a cursor in the blanks the source ends with
   TOP-DUE
   PEND-RELEASE ;

\ The checker locates the packets it writes in the bytes being scanned, whose
\ first byte lies at the base's file line, column and byte.
: SOURCE-ARM ( -- )
   SOURCE@ SOURCE-U @ BASE-LINE @ BASE-COL @ BASE-BYTE @ DIAG-SOURCE! ;

\ Nested files share the checker window but not the scanner cursor. The saved
\ source, token and open-stretch context, and whether the file scanned is the
\ subject a completion's cursor is in, belong to the caller; declarations,
\ learned definers and the fact that a stretch was reported belong to the
\ entire composition. A file's packets locate in that file, and on return or
\ throw the checker is armed with the caller's bytes
\ again, or disarmed when the subject's scan ends, so no nested file's bytes,
\ which its loader frame releases, stay armed. The scan of each file starts
\ with ON-FILE, handed the bytes SOURCE! has just made the source.
: COMPOSE-FILE-SCAN ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n :}
   path pathu COMPOSE-DIAG$ {: diag:ptr diagu:n :}
   SOURCE-A @ SOURCE-U @ SCAN-I @
   BASE-LINE @ BASE-COL @ BASE-BYTE @
   TOP-PREV-A @ TOP-PREV-U @ TOP-CUR-A @ TOP-CUR-U @
   TOP-DEFER @ TOP-DEFER-A @ TOP-DEFER-U @ TOP-REFUSED @
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @
   FILE-PKG @ FILE-USE @ PEND-BASE @ VISIT-CUR @ CSR-SUBJ @
   {: olda:ptr oldu:n oldi:n oldbl:n oldbc:n oldbb:n
      oldprev:ptr oldprevu:n oldcur:ptr oldcuru:n
      olddefer:n olddefa:ptr olddefu:n oldrefused:ptr
      oldpath:ptr oldpathu:n
      oldpkg:n olduse:n oldbase:n oldvisit:n oldsubj:n :}
   src srcu SOURCE!
   SOURCE-ARM
   path COMPOSE-CUR-PATH-A !  pathu COMPOSE-CUR-PATH-U !
   diag diagu DIAG-FILE!
   path pathu VISIT-OPEN
   VISIT-CUR @ VISIT-FIRST @ = CSR-SUBJ !
   [: FILE$ SOURCE@ SOURCE-U @ ON-FILE VERIFY-SOURCE ;] catch {: rc:n :}
   DISARM
   oldvisit VISIT-CUR !
   oldsubj CSR-SUBJ !
   rc 0<> COMPOSE-STOP-U @ 0= and IF
      diag COMPOSE-STOP-PATH diagu BYTE-COPY
      diagu COMPOSE-STOP-U !
      path pathu COMPOSE-SUBJ? COMPOSE-STOP-SUBJ !
   THEN
   olda SOURCE-A !  oldu SOURCE-U !  oldi SCAN-I !
   oldbl BASE-LINE !  oldbc BASE-COL !  oldbb BASE-BYTE !
   oldprev TOP-PREV-A !  oldprevu TOP-PREV-U !
   oldcur TOP-CUR-A !  oldcuru TOP-CUR-U !
   olddefer TOP-DEFER !  olddefa TOP-DEFER-A !
   olddefu TOP-DEFER-U !  oldrefused TOP-REFUSED !
   oldpath COMPOSE-CUR-PATH-A !  oldpathu COMPOSE-CUR-PATH-U !
   oldpkg FILE-PKG !  olduse FILE-USE !  oldbase PEND-BASE !
   oldpathu 0 > IF
      oldpath oldpathu COMPOSE-DIAG$ DIAG-FILE!
      SOURCE-ARM
   ELSE
      DIAG-SOURCE-OFF
   THEN
   \ The loader's own file goes on, so a storage refusal in it is located there.
   oldu 0 > IF olda oldu oldbl oldbc oldbb CHECKER-VERIFY-SOURCE! THEN
   rc 0<> IF rc throw THEN ;

: COMPOSE-INIT ( -- )
   [: COMPOSE-FILE-SCAN ;] is COMPOSE-FILE ;

COMPOSE-INIT

: THROW-RESULT ( n -- )
   dup 0= IF drop exit THEN
   throw ;

\ One action inside the verifier's package scope: the owner's start opens the
\ checker's engine overlay (src/core/checker.f CHECKER-OVERLAY), so every name
\ the action replays binds through the engine dictionary, and the done closes
\ the overlay and restores the caller's scope on the clean and the throwing
\ path alike.
: RUN-IN-SCOPE ( [ -- ] -- )
   TICK-CONTEXT-RESET
   false TICK-REMAINDER !
   0 CERTIFIED-N !  0 DEFERRED-N !
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF OWNER-XT VERIFIER-ACTION execute
   catch
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF OWNER-XT VERIFIER-ACTION execute
   THROW-RESULT ;

\ The checker locates the packets this scan writes in the bytes it reads
\ (SOURCE-ARM), and is disarmed after the scan on the clean and the throwing
\ path alike.
: VERIFY-ARMED ( -- )
   SOURCE-ARM
   [: VERIFY-SOURCE ;] catch
   DIAG-SOURCE-OFF
   THROW-RESULT ;

: RUN ( -- )
   [: VERIFY-ARMED ;] RUN-IN-SCOPE ;

: COMPOSE-SUBJECT ( -- )
   COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ COMPOSE-FILE ;

: RUN-COMPOSE ( -- )
   [: COMPOSE-SUBJECT ;] RUN-IN-SCOPE ;

: COMPOSE-WITH-ROOT ( -- )
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ SOURCE-ROOT:DIRNAME
   [: RUN-COMPOSE ;] SOURCE-ROOT:WITH ;

\ The composition's checks report each use they bind to USE-SEEN.
: COMPOSE-WITH-USES ( -- )
   ['] USE-SEEN ['] COMPOSE-WITH-ROOT WITH-USES ;

\ Stash-and-body, the shape src/core/checker.f CHECK-QUIET-CANDIDATE! takes: a
\ quotation cannot read its caller's locals.
PTR-VARIABLE CAND-A
variable CAND-U
variable CAND-VERDICT

: CANDIDATE-BODY ( -- )
   CAND-A @ CAND-U @ CHECK-QUIET-CANDIDATE! CAND-VERDICT ! ;

\ The action SOURCE-BUF-THEN-IN-SCOPE runs once its scan is through.
TYPED-VARIABLE THEN-ACTION [ -- ]

: VERIFY-THEN ( -- )
   VERIFY-ARMED
   THEN-ACTION @ execute ;

\ ---- a quiet composition's reader ---------------------------------------------
\ A quiet composition reads each file a loader loads itself, as the loader's
\ SOURCE-INPUT:READ: a file the system does not open, or does not read to its
\ end, stops it with E-SOURCE-READ, and FAULT-TARGET$ names the file as it was
\ resolved; whether that file is missing or unreadable is the caller's to ask
\ of the file system. Any other refusal, memory's among them, stays itself.
\ The loader refuses a resolved path past PATH-CAP before it reads one
\ (src/core/include.f INCLUDE-CHECK-PATH), so the path fits both buffers.
create READ-PATHZ PATH-CAP 1+ allot
DYNAMIC-BUFFER READ-BUF u8                    \ the file's bytes, READ-U of them
variable READ-U
variable READ-FD
65536 constant READ-STEP                      \ the bytes each read asks for

: FAULT-TARGET! ( ptr u8 n -- ) {: a:ptr u:n :}
   a FAULT-TARGET u BYTE-COPY
   u FAULT-U ! ;

: READ-FILL ( -- )
   0 READ-U !
   BEGIN
      READ-U @ READ-STEP + READ-BUF-RESERVE
      READ-FD @  READ-U @ READ-BUF  READ-STEP  read
      dup 0 < IF E-SOURCE-READ throw THEN
      dup 0 >
   WHILE
      READ-U @ + READ-U !
   REPEAT drop ;

: QUIET-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   2drop {: path:ptr pathu:n :}
   path READ-PATHZ pathu BYTE-COPY
   0 READ-PATHZ pathu + c!
   READ-PATHZ open-rd {: fd:n :}
   fd 0 < IF path pathu FAULT-TARGET!  E-SOURCE-READ throw THEN
   fd READ-FD !
   [: READ-FILL ;] catch {: rc:n :}
   fd close
   rc E-SOURCE-READ = IF path pathu FAULT-TARGET! THEN
   rc 0<> IF rc throw THEN
   0 READ-BUF READ-U @ ;

\ The source a quiet composition verifies and its path, for the composition
\ it runs under catch: a quotation cannot read its caller's locals.
PTR-VARIABLE QUIET-SRC-A
variable QUIET-SRC-U
PTR-VARIABLE QUIET-PATH-A
variable QUIET-PATH-U

public

\ The verifier's child has the deferred stretches and definitions of its
\ composition reported (tools/check-verify-child.f); DEFERRED? says whether one
\ was.
: REPORT-DEFERRALS ( -- )
   -1 DEFER-REPORT !  0 DEFER-SEEN ! ;

: DEFERRED? ( -- bool )
   DEFER-SEEN @ 0<> ;

\ The colon definitions the last run certified, and those it deferred to the
\ run, up to where it stopped (TALLY): the build's self-check census reports
\ them after each certify.
: CENSUS ( -- n n )
   CERTIFIED-N @ DEFERRED-N @ ;

\ The next composition's completion cursor, subject byte N: the spellings its
\ place offers go to ON-CANDIDATE (the cursor's section, at the top of this
\ file). A negative N is none, and the composition's end drops it.
: CURSOR! ( n -- )
   dup CSR-AT !
   0 < IF CSR-DONE ELSE CSR-WAIT THEN CSR-STATE ! ;

\ The byte where the token the scan read last starts, at the base the source
\ was given, or that base before the scan reads one: after a throw out of a
\ statement, where the statement stood.
: TOKEN-BYTE@ ( -- n )
   TOKEN-BYTE @ ;

\ The name of the definition the last scan stopped at because its scope
\ already held it (ON-DUPLICATE): the byte where it starts, at the same base as
\ TOKEN-BYTE@, and its length, which is 0 when the scan refused no written
\ name.
: DUPLICATE ( -- n n )
   DUPLICATE-AT @ DUPLICATE-U @ ;

\ Verify one supplied source through loader composition at each top-level
\ loader token. The registry suffix and the caller's checker scope survive both
\ success and throw; the supplied bytes remain authoritative for this path.
: SOURCE-COMPOSE-LABELED-IN-SCOPE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n :}
   COMPOSE-ON @ 0<> IF E-PKG-CONTEXT throw THEN
   pathu 0= pathu PATH-CAP > or labelu PATH-CAP > or IF E-PATH-RANGE throw THEN
   path COMPOSE-SUBJ-PATH pathu BYTE-COPY
   pathu COMPOSE-SUBJ-PATH-U !
   label COMPOSE-SUBJ-LABEL labelu BYTE-COPY
   labelu COMPOSE-SUBJ-LABEL-U !
   src COMPOSE-SUBJ-A !  srcu COMPOSE-SUBJ-U !
   NULL-PTR COMPOSE-CUR-PATH-A !  0 COMPOSE-CUR-PATH-U !
   0 COMPOSE-STOP-U !
   0 PEND-N !
   REQUIRE-REG:COUNT COMPOSE-REQ0 !
   COMPOSE-SUBJ-PATH pathu REQUIRE-KNOWN? 0= IF
      COMPOSE-SUBJ-PATH pathu REQUIRE-STORE 2drop
   THEN
   -1 COMPOSE-ON !
   [: COMPOSE-WITH-USES ;] catch {: rc:n :}
   VISITS-DROP
   CSR-RESET
   0 COMPOSE-ON !
   COMPOSE-REQ0 @ REQUIRE-REG:TRUNCATE
   rc 0<> IF rc throw THEN ;

: SOURCE-COMPOSE-IN-SCOPE ( ptr u8 n ptr u8 n -- )
   2dup SOURCE-COMPOSE-LABELED-IN-SCOPE ;

\ SOURCE-COMPOSE-IN-SCOPE, quiet: the composition itself refuses the loader
\ forms discovery refuses (tools/source-discovery.f), where it meets them and
\ after the packets it made before, and reads each file a loader loads, so no
\ walk need run first. A file LENIENT-FILE? names keeps its forms. A stop at a
\ loader (LOADER-FAULT?) stands at TOKEN-BYTE@, FAULT-LEN@ bytes long, and
\ FAULT-TARGET$ names a file it could not read.
: SOURCE-COMPOSE-QUIET-IN-SCOPE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n :}
   COMPOSE-ON @ 0<> IF E-PKG-CONTEXT throw THEN
   src QUIET-SRC-A !  srcu QUIET-SRC-U !
   path QUIET-PATH-A !  pathu QUIET-PATH-U !
   0 FAULT-LEN !  0 FAULT-U !
   [: SOURCE-ROOT:CANON-OS ;] [: QUIET-READ ;] SOURCE-INPUT:USE
   true QUIET !
   [: QUIET-SRC-A @ QUIET-SRC-U @ QUIET-PATH-A @ QUIET-PATH-U @ SOURCE-COMPOSE-IN-SCOPE ;]
   catch {: rc:n :}
   false QUIET !
   SOURCE-INPUT:RESET
   rc THROW-RESULT ;

: SOURCE-COMPOSE-STOPPED$ ( -- ptr u8 n )
   COMPOSE-STOP-U @ 0 > IF COMPOSE-STOP-PATH COMPOSE-STOP-U @ EXIT THEN
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ ;

\ The file SOURCE-COMPOSE-STOPPED$ names is the supplied bytes themselves, not a
\ file a loader statement read: a caller that reports where the composition
\ stopped reads the token from its own bytes there, and from the named file
\ otherwise.
: SOURCE-COMPOSE-STOPPED-SUBJECT? ( -- bool )
   COMPOSE-STOP-U @ 0= COMPOSE-STOP-SUBJ @ or ;

\ Whether N, the code a quiet composition stopped with, is a loader's fault: a
\ form discovery refuses (E-DISC-FIRST..E-DISC-LAST) or a file not read
\ (E-SOURCE-READ).
: LOADER-FAULT? ( n -- bool ) {: rc:n :}
   rc E-SOURCE-READ =
   rc E-DISC-FIRST <= rc E-DISC-LAST >= and or ;

\ The length of the loader token the last quiet composition stopped at, 0 when
\ it stopped at none.
: FAULT-LEN@ ( -- n )
   FAULT-LEN @ ;

\ The file the last quiet composition could not read, as it was resolved, or
\ empty. The string is borrowed until the next quiet composition.
: FAULT-TARGET$ ( -- ptr u8 n )
   FAULT-TARGET FAULT-U @ ;

: SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   SOURCE!
   RUN ;

\ Verify one supplied source, then run ACTION in the same scope. A name the
\ source declares binds through the record the checker's overlay publishes for
\ it (src/core/checker.f CHECKER-OVERLAY), which this scope's close takes back
\ unless a neutral checker scope around it holds the overlay open, so a caller
\ that reads what the scan recorded by name reads it here, as the scan's own
\ checks do. No second verifier scope opens inside this one (E-PKG-CONTEXT). A
\ scan that throws runs no action.
: SOURCE-BUF-THEN-IN-SCOPE ( ptr u8 n [ -- ] -- )
   THEN-ACTION !
   SOURCE!
   [: VERIFY-THEN ;] RUN-IN-SCOPE ;

\ What the certify path says about one candidate definition, `NAME ( effect )
\ body`: -1 certified, 0 refused, 1 unresolvable, as CHECK-QUIET-CANDIDATE!
\ answers. A name this scanner registered has no engine record, so a live
\ candidate cannot bind it; here it binds where the scan recorded it.
: CANDIDATE-IN-SCOPE ( ptr u8 n -- n )
   CAND-U !  CAND-A !
   [: CANDIDATE-BODY ;] RUN-IN-SCOPE
   CAND-VERDICT @ ;

: SOURCE-BUF-AT-IN-SCOPE ( ptr u8 n n n n -- )
   SOURCE-AT!
   RUN ;

\ Verify a source in a candidate scope of its own: what it defines, and the
\ definer rows it learns, go when CHECKER-CANDIDATE-SCOPE-DONE rewinds the scope.
: SOURCE-BUF ( ptr u8 n -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   [: RUN ;] catch
   CHECKER-CANDIDATE-SCOPE-DONE
   THROW-RESULT ;

;package
