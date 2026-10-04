\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

require lib/errors.f
require src/core/checker-owner-guard.f
require src/habu/layout.f

package VERIFY

public

\ A source the scan cannot read on stops it with one of these, TOKEN-BYTE@ at
\ the word that cannot finish: a reader (a definer, a parsing word) with no
\ token after it, a string opener with no closing quote, a PRIM: or PPRIM: row
\ with no closer. tools/check.f reports each where it stands, by the record it
\ writes when it finds the same defect itself.
7187 constant E-MISSING-NAME
7188 constant E-UNTERMINATED-STRING
7189 constant E-MALFORMED-REGISTRY-ROW

private

\ A statement the source ends inside, or one that lacks a part it must have,
\ stops the scan with one of these at its opener (STATEMENT-STOP), and a table
\ of this scan's that is full stops it at the token read last. tools/check.f
\ reports each by the record of a statement that throws.
7155 constant E-VS-UNTERMINATED-DEFINITION \ a definition, signature or group
7157 constant E-VS-MISSING-SIGNATURE       \ a definer's, absent or unclosed
7158 constant E-VS-BARE-TRUST              \ TRUST with no name and signature
7194 constant E-VS-CAPACITY                \ a table of this scan is full
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
PTR-VARIABLE TOP-PREV-A
variable TOP-PREV-U
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
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= if SOURCE@ 0 exit then
   SCAN-I @ TOKEN-START !
   BASE-BYTE @ SCAN-I @ + TOKEN-BYTE !
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
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

\ Skipped top-level string literals feed a two-slot ring so a bare top-level
\ `s" NAME" s" SIG" TRUST` (strings the scanner would otherwise discard) can be
\ replayed as a trust. The ring resets per NEXT-SCAN call, so at a TRUST token it
\ holds exactly the two preceding literals from the same statement.
: STR-RING-RESET ( -- )
   NULL-PTR STR-PREV-A !  0 STR-PREV-U !
   NULL-PTR STR-LAST-A !  0 STR-LAST-U ! ;

: STR-RING-PUSH ( ptr u8 n -- ) {: a:ptr u:n :}
   STR-LAST-A @ STR-PREV-A !
   STR-LAST-U @ STR-PREV-U !
   a STR-LAST-A !
   u STR-LAST-U ! ;

: RECORD-SKIPPED-STRING ( n -- ) {: pfx:n :}
   SCAN-I @ TOKEN-START @ - pfx - 1 - {: vlen:n :}
   vlen 0 < IF EXIT THEN
   SOURCE@ TOKEN-START @ + pfx + vlen STR-RING-PUSH ;

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

: NEXT ( -- ptr u8 n )
   BEGIN
      NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 SKIP-PAST ELSE
      SKIP-STRINGS @ 0= IF 2dup LOCAL? IF EXIT THEN THEN
      2dup PRINT-OPENER? IF 2drop 41 SKIP-PAST ELSE
      SKIP-STRINGS @ 0= 0= IF
         2dup ESCAPED-STRING-OPENER? IF 2drop SKIP-ESCAPED-QUOTE 4 RECORD-SKIPPED-STRING ELSE
         2dup NORMAL-STRING-OPENER? IF 2drop 34 SKIP-PAST 3 RECORD-SKIPPED-STRING ELSE EXIT THEN THEN
      ELSE EXIT THEN
      THEN THEN THEN
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

: BODY-RESET ( -- )
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

: MAYBE-SIGNATURE ( -- )
   E-VS-UNTERMINATED-DEFINITION SCAN-SIG IF BODY-APPEND ELSE 2drop THEN ;

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
\ engine's. The path is the literal right before the loader word; any other
\ loader form in a body is discovery's to refuse (tools/source-discovery.f).
\ Each entry's path lies in the bytes of the file being read, which live until
\ that file ends.
16 constant PEND-MAX
PEND-MAX TYPED-BUFFER PEND-A ptr u8           \ a waiting load's path
PEND-MAX TYPED-BUFFER PEND-U n                \ and its length
PEND-MAX TYPED-BUFFER PEND-INC n              \ 1 for `included`, 0 for `required`
variable PEND-N
variable PEND-BASE                            \ the first entry of the file being read
PTR-VARIABLE BODY-LIT-A  variable BODY-LIT-U  \ the literal the body token before closed, or 0
variable BODY-BENT                            \ the body read so far is no straight line
0 constant GUARD-NONE                         \ the body token before is no target predicate
1 constant GUARD-LIVE                         \ it is one the engine answers true
2 constant GUARD-DEAD                         \ it is one the engine answers false
variable BODY-GUARD                           \ which of the three
variable BODY-ARMS                            \ the target `if`s whose running arm the line is in
variable BODY-DEAD                            \ in an arm that never runs: 1 + the `if`s open in it

\ Neither a literal nor a target predicate is right before the next body token.
: BODY-PREV-CLEAR ( -- )
   0 BODY-LIT-U !  GUARD-NONE BODY-GUARD ! ;

: BODY-LOAD-RESET ( -- )
   0 BODY-BENT !  BODY-PREV-CLEAR  0 BODY-ARMS !  0 BODY-DEAD ! ;

: PEND-PUSH ( ptr u8 n n -- ) {: a:ptr u:n inc:n :}
   PEND-N @ PEND-MAX >= IF E-VS-CAPACITY throw THEN
   a PEND-N @ PEND-A !  u PEND-N @ PEND-U !  inc PEND-N @ PEND-INC !
   PEND-N @ 1 + PEND-N ! ;

\ The text of a `s"` literal from the rest STRING-REST read: past the one
\ delimiting space, short of the closing quote. Any other opener leaves none.
: BODY-LIT! ( ptr u8 n ptr u8 n -- ) {: o:ptr ou:n s:ptr su:n :}
   BODY-PREV-CLEAR
   o ou NORMAL-STRING-OPENER? su 2 >= and IF
      s 1 + BODY-LIT-A !  su 2 - BODY-LIT-U !
   THEN ;

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

\ A body token that is no string literal: a loader word right after one, on a
\ straight line, waits while a composition reads the source.
: BODY-TOKEN-SEEN ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-DEAD @ 0 > IF a u DEAD-TOKEN BODY-PREV-CLEAR EXIT THEN
   BODY-BENT @ 0= IF a u ARM-TOKEN? IF BODY-PREV-CLEAR EXIT THEN THEN
   a u WRAP-CTL-TOK? IF 1 BODY-BENT ! THEN
   COMPOSE-ON @ 0<> BODY-LIT-U @ 0 > and BODY-BENT @ 0= and IF
      a u s" required" STR=CI IF BODY-LIT-A @ BODY-LIT-U @ 0 PEND-PUSH THEN
      a u s" included" STR=CI IF BODY-LIT-A @ BODY-LIT-U @ 1 PEND-PUSH THEN
   THEN
   0 BODY-LIT-U !
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

: GROUP-NEXT ( -- ptr u8 n )
   NEXT-RAW dup 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN ;

: APPEND-GROUP ( -- )
   BEGIN GROUP-NEXT 2dup BODY-APPEND GROUP-TOKEN UNTIL ;

: SKIP-GROUP ( -- )
   BEGIN GROUP-NEXT GROUP-TOKEN UNTIL ;

\ A local, a group and a block word in a body, ahead of the parsing keywords and
\ string openers the body token readers below take. A local or a group skips
\ BODY-TOKEN-SEEN, so it clears what the token before it told an `if`: a local,
\ even one spelled as a target predicate, opens no target `if`.
: APPEND-BODY-TOKEN ( -- )
   LOCAL-TOKEN? IF
      BODY-PREV-CLEAR
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ s" {:" CORE-STR= IF
      BODY-PREV-CLEAR
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
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN
      TOKEN-A @ TOKEN-U @ BODY-APPEND
   THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ BODY-PARSER? IF OPERAND 2drop BODY-PREV-CLEAR exit THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST exit THEN
   TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN ;

\ SKIP-BODY-TOKEN for a definition body, which declares locals; a primitive row
\ declares none.
: SKIP-DEF-TOKEN ( -- )
   LOCAL-TOKEN? IF BODY-PREV-CLEAR EXIT THEN
   TOKEN-A @ TOKEN-U @ s" {:" CORE-STR= IF BODY-PREV-CLEAR SKIP-GROUP EXIT THEN
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
CAST: CREATES-ACTION ( n -- [ n -- n ] )
CAST: CREATED-ACTION ( n -- [ ptr u8 n n -- bool ] )
CAST: DOES-ACTION ( n -- [ ptr u8 n ptr u8 n ptr u8 n -- n ] )
CAST: RENDERS-ACTION ( n -- [ n -- bool ] )

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

\ Does the symbol name a word that renders source when it runs (src/core/checker.f
\ CTL-RENDERS)? When it does, the checker marks the wordlist the statement runs
\ in, and a definition read after it that names a word nothing in scope defines
\ is left to the run (CHECK's verdict 2) instead of refused E-UNDEFINED.
: RENDERS-MARK? ( n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RENDERS-OFF OWNER-XT RENDERS-ACTION execute ;

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
\ learns that it renders from its body (CTL-RENDERS), and RECORD-DEFINER? marks
\ the statement's wordlist, leaving what the text defines to the run. What the
\ source states is still read: a `generates:` row declares the word its definer
\ makes from the next token (RECORD-GENERATES), and FUNCTION:'s declaration
\ group is its word's effect (RECORD-FFI-FUNCTION), so those names are checked
\ here and only the rest is left to the run.
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
\ The signature table stays `create … allot`: its elements are bytes, which a
\ TYPED-BUFFER cannot hold, and BUFFER: lives in lib/string.f, outside this
\ file's require closure. A slot holds the longest effect a `generates:` row
\ states (checker.f GENR-SIG-CAP), so every row the engine takes fits.
\ The bound is a scope's, not a file's: a preverified require closure holds
\ several sources in one checker scope, and the largest single file in the tree
\ carries 28 `does>` today.
128 constant DEFINER-CAP                   \ definer rows
GENR-SIG-CAP constant DEFINER-SIG-SLOT     \ one effect's bytes
DEFINER-CAP TYPED-BUFFER DEFINER-SYM n
DEFINER-CAP TYPED-BUFFER DEFINER-LEN n     \ its effect's length, 0 for a retired row
create DEFINER-SIG DEFINER-CAP DEFINER-SIG-SLOT * allot

: DEFINER-SLOT ( n -- ptr u8 ) {: row:n :}
   DEFINER-SIG BYTE-VIEW row DEFINER-SIG-SLOT * + ;

: DEFINER-SIG@ ( n -- ptr u8 n ) {: row:n :}
   row DEFINER-SLOT  row DEFINER-LEN @ ;

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

: DEFINER-APPEND ( n -- n )                   \ a new row for sym
   {: sym:n :}
   VERIFY-DEFINER-N @ DEFINER-CAP >= IF E-VS-CAPACITY throw THEN
   VERIFY-DEFINER-N @
   {: row:n :}
   sym row DEFINER-SYM !
   row 1 + VERIFY-DEFINER-N !
   row ;

: DEFINER-ROW ( n -- n ) {: sym:n :}          \ sym's row, appended when it is new
   sym DEFINER-FIND dup 0<> IF 1 - EXIT THEN drop
   sym DEFINER-APPEND ;

\ Record `sig` as the effect the definer named by `sym` creates. A name with a
\ live row keeps that row and takes the newer effect, which is what the run
\ time does: a replacement clause replaces the old created-word effect.
: DEFINER-ADD ( ptr u8 n n -- ) {: sig:ptr sigu:n sym:n :}
   sym 0= IF EXIT THEN                        \ never recorded: nothing to hang it on
   sigu DEFINER-SIG-SLOT > IF E-VS-CAPACITY throw THEN
   sym DEFINER-ROW
   {: row:n :}
   sigu row DEFINER-LEN !
   row DEFINER-SLOT
   {: slot:ptr :}
   0 BEGIN dup sigu < WHILE
      dup sig + c@  over slot + c!
      1 +
   REPEAT drop ;

: DEFINER-RETIRE ( n -- )
   {: sym:n :}
   sym DEFINER-FIND 0= IF EXIT THEN
   0  sym DEFINER-APPEND DEFINER-LEN ! ;

\ The effect a token's definer gives the word it creates, answered as a string
\ whose ZERO LENGTH means "not a learned definer" - the same shape NEXT-RAW ends
\ a source with. The empty table answers before asking the scope anything, so a
\ source that uses no such definer pays one cell read per token.
: DEFINER-EFFECT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   VERIFY-DEFINER-N @ 0= IF SOURCE@ 0 EXIT THEN
   a u FIND-SYM DEFINER-FIND dup 0= IF drop SOURCE@ 0 EXIT THEN
   1 - DEFINER-SIG@ ;

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

: VERIFY-BODY ( -- n )
   BODY$ CHECK-BODY BODY-VERDICT ;

\ The pre-pass's own does>-clause entry point. It is not the engine's
\ CHECK-DOES!: this scan reaches a clause AFTER the definer's own body has been
\ checked and recorded, so the checker must not latch the created effect here -
\ the next record belongs to the next definition. What this scan learns about a
\ definer it READ goes into the table above instead (src/core/checker.f
\ CHECKER-SOURCE-DOES! carries the reason). It takes the definer's name, which
\ names the clause in the diagnostic of a refused one.
: CHECK-DOES-BODY ( ptr u8 n ptr u8 n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-DOES-OFF OWNER-XT DOES-ACTION execute ;

: VERIFY-DOES-BODY ( ptr u8 n ptr u8 n -- n ) {: sig:ptr sigu:n na:ptr nu:n :}
   BODY$ sig sigu na nu CHECK-DOES-BODY BODY-VERDICT ;

\ ---- the two rules that put a definition in the table above ------------------
\ The definition's own name, pinned by VERIFY-DEFINITION before its body is
\ scanned: a created effect is recorded on the definition's own entry, so the
\ recorders below ask the scope for that entry once the body has certified.
\ RECORD-EXPORT pins the name it re-exports here for the quotation it catches.
PTR-VARIABLE DEF-NAME-A
variable DEF-NAME-U
variable WRAP-DEFINERS                        \ definer calls in this body …
variable WRAP-CTL                             \ … and whether the line ever bent
PTR-VARIABLE WRAP-SIG-A
variable WRAP-SIG-U

: DEF-NAME! ( -- )
   TOKEN-U @ DEF-NAME-U !  TOKEN-A @ DEF-NAME-A ! ;

: WRAP-RESET ( -- )
   0 WRAP-DEFINERS !  0 WRAP-CTL !
   NULL-PTR WRAP-SIG-A !  0 WRAP-SIG-U ! ;

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
   a u DEFINER-EFFECT dup 0<> IF
      WRAP-SIG-U !  WRAP-SIG-A !
      WRAP-DEFINERS @ 1 + WRAP-DEFINERS !  EXIT
   THEN
   2drop ;

\ Only a certified body is learned. One deferred to the run names a word only the
\ run can see, which may itself be a definer, so its definer calls are not
\ counted.
: VERIFY-WRAPPER ( -- )
   WRAP-CTL @ IF EXIT THEN
   WRAP-DEFINERS @ 1 <> IF EXIT THEN
   WRAP-SIG-A @ WRAP-SIG-U @ DEFINER-RECORD ;

\ The created effect is the clause's declaration, recorded unless the definer's
\ body or its clause was refused. One deferred to the run keeps it, as a
\ deferred colon definition keeps its declared signature (src/core/checker.f
\ CHECK, verdict 2), and the run judges the deferred text.
: VERIFY-DOES ( -- )
   VERIFY-BODY {: def:n :}
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   BODY-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF
         sig sigu DEF-NAME-A @ DEF-NAME-U @ VERIFY-DOES-BODY
         0<>  def 0<> and IF sig sigu DEFINER-RECORD THEN EXIT
      THEN
      APPEND-BODY-TOKEN
   AGAIN ;

\ The two registrars, kept apart here for the same reason the engine keeps them
\ apart (src/core/checker.f TRUST-DECL, dot habu-make-trust-refuse-cc8e19de).
\ This scanner is a PRE-PASS: it reads a whole source buffer before any of it is
\ compiled, so a definer it replays names a word that does not exist in this
\ process and cannot be asked to prove otherwise. A bare `s" NAME" s" SIG" trust`
\ row is the opposite - it asserts an effect for a word it CLAIMS already exists,
\ which is a claim the dictionary can answer and now does.
TRUSTED: DECL-SIGNATURE ( ptr u8 n ptr u8 n -- )
   TRUST-DECL ;

TRUSTED: TRUST-SIGNATURE ( ptr u8 n ptr u8 n -- )
   TRUST ;

\ The cast declarer's registrar, on the same boundary and for the same reason as
\ DECL-SIGNATURE above: UNSAFE-TOK? rejects `checker-defcast` inside a checked
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

: CAST-TRUST ( -- )
   DTC-NAME$ DTC-SIG$ DECL-SIGNATURE ;

: RECORD-CAST-IN ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-IN
   CAST-TRUST ;

: RECORD-CAST-OUT ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-OUT
   CAST-TRUST ;

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

private

: DUPLICATE-STOP ( n n ptr u8 n bool -- )
   drop 2drop
   DUPLICATE-U !  DUPLICATE-AT !
   E-DUP-DEFINITION throw ;

: DUPLICATE-INIT ( -- )
   ['] DUPLICATE-STOP is ON-DUPLICATE ;

DUPLICATE-INIT

\ Refuse the definition named by the token just scanned, n bytes long.
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

: SIG-RAW-MODE! ( n -- ) SIG-RAW-MODE ! ;

\ RAW-TRUST-NEXT: registers the given effect for the word the next token names,
\ with TVK-RAW type vars (SIG-RAW-MODE! brackets the checker's signature parse).
\ Used for the raw storage definers create/variable/constant/PTR-VARIABLE and
\ PERSISTED-PTR-VARIABLE so a
\ fetch from their raw cell yields a RAW value that cannot launder into a nominal
\ atom or family (habu-nominal-storage-raw, VALUE side).
: RAW-TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NAME-TOKEN
   dup 0= IF E-MISSING-NAME throw THEN
   -1 SIG-RAW-MODE!
   sig sigu [: DECL-SIGNATURE ;] [: 0 SIG-RAW-MODE! ;] finally ;

\ CREATED-TRUST-NEXT?: RAW-TRUST-NEXT's twin for a definer the checker knows and
\ this pre-pass never read. The row is the checker's own certified one, so there
\ is no signature text to re-parse and no seal to re-apply here; what is left is
\ the same shape - the created word is the NEXT token - and the same answer.
\ THE TOKEN IS TESTED BEFORE THE NAME IS TAKEN: NAME-TOKEN consumes a token, and
\ a token that is not a definer must leave the scan exactly where it was.
: CREATED-TRUST-NEXT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FIND-SYM {: dsym:n :}
   dsym CREATES-SYM? 0= IF 0 0= 0= EXIT THEN
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu dsym RECORD-CREATED ;

: TRUST-DEFER-SIGNATURE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu REQUIRE-SIGNATURE DECL-SIGNATURE
   name nameu CHECKER-DEFER ;

: TRUST-DEFER ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu TRUST-DEFER-SIGNATURE ;

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
: SCAN-TRUSTED-BODY ( ptr u8 n -- ) {: na:ptr nu:n :}
   BODY-LOAD-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      LOCAL-TOKEN? 0=  TOKEN-A @ TOKEN-U @ s" does>" STR=CI  and IF
         REQUIRE-SIGNATURE na nu DEFINER-RECORD-AS
      ELSE
         SKIP-DEF-TOKEN
      THEN
   AGAIN ;

: TRUSTED-DEFINITION ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu REQUIRE-SIGNATURE DECL-SIGNATURE
   name nameu SCAN-TRUSTED-BODY ;

\ A cast has no body and no `;`, so unlike TRUSTED-DEFINITION above there is
\ nothing to skip: the declaration ends at its closing paren. Registration goes
\ through the certifying registrar, not DECL-SIGNATURE, so an illegal retype is
\ refused here too and not merely recorded.
: CAST-DECLARATION ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu REFUSE-DUPLICATE IF REQUIRE-SIGNATURE 2drop EXIT THEN
   name nameu REQUIRE-SIGNATURE DEFCAST-SIGNATURE ;

: UNDEFINE-WORD ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
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
$40 constant NOM-TAIL-CAP
create NOM-TAIL-BUF NOM-TAIL-CAP allot
variable NOM-TAIL-U

\ MANGLE ( ptr u8 n -- ptr u8 n ) folds the UPPER-CASE surface name to the
\ lowercase family tail, matching deftype.f's ASCII-LOWER fold.
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u NOM-TAIL-CAP > IF E-VS-CAPACITY throw THEN
   0 NOM-TAIL-U !
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C  NOM-TAIL-BUF NOM-TAIL-U @ + c!
      NOM-TAIL-U @ 1 + NOM-TAIL-U !  1+
   REPEAT drop
   NOM-TAIL-BUF NOM-TAIL-U @ ;

: RECORD-DEFTYPE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu MANGLE {: tail:ptr tailu:n :}
   tail tailu s" 0" CHECKER-DEFFAMILY
   name nameu tail tailu RECORD-CAST-IN
   name nameu tail tailu RECORD-CAST-OUT ;

: RECORD-DEFLINEAR ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
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
         name nameu BODY$ ENUM-DECL:ED-REPLAY
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
         name nameu BODY$ STRUCTURE-DECL:SD-REPLAY
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
   STG-START @  STG-A @ STG-U @ + STG-START @ - ;

\ The declared name. With none on the definer's line it is empty, and the
\ definer's token is refused as its run refuses it.
\ A refused name, malformed or a duplicate, is empty and still owns its type
\ span; consume it before resuming the statement scan so a type token cannot
\ be interpreted as a new statement.
: SCAN-STORAGE-NAME ( -- ptr u8 n )
   SCAN-LINE-TOKEN
   dup 0= IF TOP-CUR-A @ TOP-CUR-U @ CHECKER-STORAGE-NAME-REFUSE EXIT THEN
   2dup CHECKER-LBUF-NAME-OK? IF
      2dup REFUSE-DUPLICATE 0= IF EXIT THEN
   THEN
   2drop SCAN-STORAGE-TYPE 2drop SOURCE@ 0 ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   SCAN-STORAGE-NAME {: name:ptr nameu:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFLAYOUT-BUFFER ;

\ DEFER-LAYOUT-BUFFER publishes its accessor and NAME-BIND and NAME-GROW from
\ one line, so a later definition calling one is E-UNDEFINED without this row,
\ and a type it cannot size goes unreported until the run. No count token: the
\ count arrives at the bind.
: RECORD-DEFER-LAYOUT-BUFFER ( -- )
   SCAN-STORAGE-NAME {: name:ptr nameu:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFDEFER-LAYOUT-BUFFER ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   SCAN-STORAGE-NAME {: name:ptr nameu:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFTYPED-BUFFER ;

: RECORD-TYPED-VARIABLE ( -- )
   SCAN-STORAGE-NAME {: name:ptr nameu:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFTYPED-VARIABLE ;

\ DYNAMIC-BUFFER (src/core/layout-buffer.f) publishes THREE words from one line -
\ the accessor, NAME-RESERVE and NAME-RELEASE - so the whole triple is registered
\ here. Certification never runs the definer, and without this row a later
\ definition in the same source calling one of the three is E-UNDEFINED:
\ src/habu/aot-decl.f's AOT-NAMES-RESERVE was, which took the stage2 certify pass
\ with it. No count token: a dynamic buffer's extent is set at run time.
: RECORD-DYNAMIC-BUFFER ( -- )
   SCAN-STORAGE-NAME {: name:ptr nameu:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFDYNAMIC-BUFFER ;

: RECORD-VALUE-RECORD ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   BODY-RESET
   BEGIN
      NEXT-SCAN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup VALUE-RECORD-END? IF
         2drop
         name nameu BODY$ CHECKER-DEFRECORD
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-TRUST ( -- )
   STR-LAST-U @ 0= IF E-VS-BARE-TRUST STATEMENT-STOP THEN
   STR-PREV-U @ 0= IF E-VS-BARE-TRUST STATEMENT-STOP THEN
   STR-PREV-A @ STR-PREV-U @
   STR-LAST-A @ STR-LAST-U @
   TRUST-SIGNATURE ;

\ EXPORT has two documented roles split by package context (dot
\ habu-compiler-pkg-re-688212c1): inside an open package it is the re-export
\ declaration (CHECKER-EXPORT aliases the source's checked effect under its
\ tail); at top level it is the hb-build --repl export directive, which the
\ build strips via COMMENT-EXPORTS before engine load — replay consumes the
\ name and records nothing, exactly like the engine never seeing the line.
\ A re-export duplicates its tail in the current section. The checker asks that
\ only once the name has resolved (src/core/checker.f EXPORT-RECORD), as --load
\ does, so its refusal is caught here and kept at the name as written.
: RECORD-EXPORT ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF EXIT THEN
   name DEF-NAME-A !  nameu DEF-NAME-U !
   [: DEF-NAME-A @ DEF-NAME-U @ CHECKER-EXPORT ;] catch {: rc:n :}
   rc E-DUP-DEFINITION = IF nameu DUPLICATE! EXIT THEN
   rc 0<> IF rc throw THEN ;

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

\ The loads that wait in this file, in the order of their loaders. A file one
\ of them loads keeps its own entries above them, and they end with it.
: PEND-RELEASE ( -- )
   PEND-N @ PEND-BASE @ ?do
      i PEND-A @ i PEND-U @
      i PEND-INC @ 0<> IF COMPOSE-INCLUDED ELSE COMPOSE-REQUIRED THEN
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
\ A row declares no locals, so none of the last body's stay live in it. A row
\ the source ends inside, before its name, package or closer, stops the scan at
\ its opener, the byte RECORD-PRIM or RECORD-PPRIM was entered at.
: ROW-UNCLOSED ( n -- )
   TOKEN-BYTE !
   E-MALFORMED-REGISTRY-ROW throw ;

: RECORD-PRIM-ROW ( n ptr u8 n ptr u8 n -- )
   {: at:n end:ptr endu:n alt:ptr altu:n :}
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

: TRUST-STRUCTURE-FIELD ( ptr u8 n ptr u8 n -- )
   DECL-SIGNATURE ;

: RECORD-STRUCTURE-FIELD ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu sig sigu TRUST-STRUCTURE-FIELD ;

\ Record the size word (`-- n`) then each field accessor with its runtime effect
\ so BEGIN-STRUCTURE layouts self-certify their field uses.
: RECORD-STRUCTURE ( -- )
   NAME-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF E-MISSING-NAME throw THEN
   name nameu s" -- n" DECL-SIGNATURE
   BEGIN
      NEXT-SCAN
      dup 0= IF E-VS-DECL-SYNTAX STATEMENT-STOP THEN
      2dup STRUCTURE-END? IF 2drop EXIT THEN
      \ A pointer field's POINTEE is independent of the record's element type: a
      \ cell record may hold a byte pointer. `ptr ptr a` tied the two together,
      \ so a record read as cells forced every pointer field to point at cells —
      \ the `ptr-field` primitive itself is `( ptr a n -- ptr ptr b )`.
      2dup STRUCTURE-PTR-FIELD? IF 2drop s" ptr a -- ptr ptr b" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CFIELD? IF 2drop s" ptr a -- ptr u8" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CELL-FIELD? IF 2drop s" ptr a -- ptr a" RECORD-STRUCTURE-FIELD ELSE
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
   nameu 0= IF s" verify-source: missing generates: definer name" 74 die THEN
   REQUIRE-SIGNATURE
   {: sig:ptr sigu:n :}
   sigu GENR-SIG-CAP > IF s" verify-source: generates: effect too long" 74 die THEN
   name nameu FIND-SYM
   {: sym:n :}
   name nameu sig sigu sym DEFINER-FIND 0<> GENERATES-SIGNATURE nip 0= IF EXIT THEN
   sig sigu sym DEFINER-ADD ;

\ `FUNCTION: NAME symbol ( effect ) ... ;FUNCTION` (lib/ffi-abi.f) makes NAME
\ from its declaration group, so the group is NAME's effect with the one rewrite
\ the declarer applies (ffi-abi.f OUT-TOKEN): an `i32` result is a cell. Inputs
\ stay verbatim. The group is read raw, because NEXT-SCAN skips it as a comment.
\ The live declaration pins this reading: test/certify-does-definer.f section 10.
\ The effect is rebuilt in a buffer of its own: BODY-BUF holds only runs read
\ out of the source (BODY-APPEND), and the rewritten `n` is not one. Its bound is
\ the engine's for a `generates:` effect.
variable FFI-OUT
GENR-SIG-CAP constant FFI-SIG-CAP
create FFI-SIG FFI-SIG-CAP allot
variable FFI-SIG-U

: FFI-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   FFI-SIG-U @ u + 1 + FFI-SIG-CAP > IF s" verify-source: FUNCTION: group too long" 74 die THEN
   a  FFI-SIG FFI-SIG-U @ +  u BYTE-COPY
   32  FFI-SIG FFI-SIG-U @ u + +  c!
   FFI-SIG-U @ u + 1 + FFI-SIG-U ! ;

: FFI-TOKEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" --" CORE-STR= IF -1 FFI-OUT ! THEN
   FFI-OUT @ 0<> a u s" i32" CORE-STR= and IF s" n" FFI-APPEND EXIT THEN
   a u FFI-APPEND ;

: FFI-SIGNATURE ( -- ptr u8 n )
   NEXT-RAW s" (" CORE-STR= 0= IF s" verify-source: FUNCTION: needs a declaration group" 74 die THEN
   0 FFI-SIG-U !
   0 FFI-OUT !
   BEGIN
      NEXT-RAW
      dup 0= IF s" verify-source: unterminated FUNCTION: group" 74 die THEN
      2dup s" )" CORE-STR= IF 2drop FFI-SIG FFI-SIG-U @ EXIT THEN
      FFI-TOKEN
   AGAIN ;

: RECORD-FFI-FUNCTION ( -- )
   NEXT-SCAN
   {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing FUNCTION: name" 74 die THEN
   NEXT-SCAN nip 0= IF s" verify-source: missing FUNCTION: symbol" 74 die THEN
   name nameu FFI-SIGNATURE DECL-SIGNATURE ;

: RECORD-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" STR=CI IF RECORD-PACKAGE 0 0= EXIT THEN
   a u s" public" STR=CI IF RECORD-PUBLIC 0 0= EXIT THEN
   a u s" private" STR=CI IF RECORD-PRIVATE 0 0= EXIT THEN
   a u s" ;package" STR=CI IF RECORD-END-PACKAGE 0 0= EXIT THEN
   a u s" using" STR=CI IF RECORD-USING 0 0= EXIT THEN
   a u s" ;using" STR=CI IF RECORD-END-USING 0 0= EXIT THEN
   a u s" deftype" STR=CI IF RECORD-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" STR=CI IF RECORD-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" STR=CI IF RECORD-VALUE-RECORD 0 0= EXIT THEN
   a u s" begin-structure" STR=CI IF RECORD-STRUCTURE 0 0= EXIT THEN
   a u s" structure" STR=CI IF RECORD-STRUCTURE-DECL 0 0= EXIT THEN
   a u s" newtype" STR=CI IF RECORD-NEWTYPE 0 0= EXIT THEN
   a u s" sumtype" STR=CI IF RECORD-SUMTYPE 0 0= EXIT THEN
   a u s" enum" STR=CI IF RECORD-ENUM 0 0= EXIT THEN
   a u s" product" STR=CI IF RECORD-PRODUCT 0 0= EXIT THEN
   a u s" LAYOUT-BUFFER" STR=CI IF RECORD-LAYOUT-BUFFER 0 0= EXIT THEN
   a u s" DEFER-LAYOUT-BUFFER" STR=CI IF RECORD-DEFER-LAYOUT-BUFFER 0 0= EXIT THEN
   a u s" TYPED-BUFFER" STR=CI IF RECORD-TYPED-BUFFER 0 0= EXIT THEN
   a u s" TYPED-VARIABLE" STR=CI IF RECORD-TYPED-VARIABLE 0 0= EXIT THEN
   a u s" DYNAMIC-BUFFER" STR=CI IF RECORD-DYNAMIC-BUFFER 0 0= EXIT THEN
   \ `constant` bakes one physical cell, so its trust is the one-cell `-- a`
   \ model — identical to native C-CONSTANT, all-errors (which funnels here),
   \ and public-signatures. This is the PERMANENT contract (TFAM 12 verdict
   \ 2026-07-09, habu-tfam-12-layout): the interpret stack is untyped by
   \ design, so no path has a sound shape source, and a wider-than-cell layout
   \ value never lands there (DNAME-WIDE dispatch gate). Any layout USE of the
   \ constant fails closed downstream; parity locked by check-all-errors-test
   \ const-layout-narrow.
   a u s" constant" STR=CI IF s" -- a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" create" STR=CI IF s" -- ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" STR=CI IF s" -- ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PTR-VARIABLE" STR=CI IF s" -- ptr ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PERSISTED-PTR-VARIABLE" STR=CI IF s" -- ptr ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   \ The declared-pointee forms: the clause names the pointee, so the effect has
   \ no type variable and the raw registration seals nothing. Same word, same row
   \ as the native path publishes through `trust-raw`.
   a u s" PTR-U8-TABLE" STR=CI IF s" -- ptr ptr u8" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PERSISTED-PTR-U8-TABLE-VARIABLE" STR=CI IF s" -- ptr ptr ptr u8" RAW-TRUST-NEXT 0 0= EXIT THEN
   \ The reserved-offset cell: the offset token precedes the name, as the table
   \ count does, so the created word is still the NEXT token and the row is the
   \ definer's declared clause.
   a u s" RESERVED-PTR-U8-CELL" STR=CI IF s" -- ptr ptr u8" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" STR=CI IF TRUST-DEFER 0 0= EXIT THEN
   a u s" PRIM:" STR=CI IF RECORD-PRIM 0 0= EXIT THEN
   a u s" PPRIM:" STR=CI IF RECORD-PPRIM 0 0= EXIT THEN
   a u s" trusted:" STR=CI IF TRUSTED-DEFINITION 0 0= EXIT THEN
   a u s" cast:" STR=CI IF CAST-DECLARATION 0 0= EXIT THEN
   a u s" undefine" STR=CI IF UNDEFINE-WORD 0 0= EXIT THEN
   a u s" trust" STR=CI IF RECORD-TRUST 0 0= EXIT THEN
   a u s" generates:" STR=CI IF RECORD-GENERATES 0 0= EXIT THEN
   a u s" FUNCTION:" STR=CI IF RECORD-FFI-FUNCTION 0 0= EXIT THEN
   a u s" immediate" STR=CI IF 0 0= EXIT THEN
   a u s" export" STR=CI IF RECORD-EXPORT 0 0= EXIT THEN
   \ … then a word that renders source when it runs (`;FUNCTION`, CMD:COMMAND,
   \ TASK:+USER): what it defines is text this scan never reads, so the checker
   \ marks the statement's wordlist and leaves a later definition naming a word
   \ nothing resolves to the run (src/core/checker.f CTL-RENDERS, UNSEEN-MARK$).
   \ It marks whether or not an arm below then records the product a
   \ `generates:` row declares (RECORD-GENERATES): that name resolves and is
   \ checked against the row, and the mark covers what no row declares, such as
   \ COMMAND's NAME#VEC and NAME#BUF.
   a u FIND-SYM RENDERS-MARK?
   {: renders:bool :}
   \ … and last, a definer this pre-pass learned from a `does>` definition or a
   \ `generates:` row earlier in the closure. The created word is the NEXT
   \ token, as it is for `constant` above - the definer's own arguments precede
   \ it - and the effect is the clause's or the row's, registered with the same
   \ raw seal the storage definers use.
   a u DEFINER-EFFECT dup 0<> IF RAW-TRUST-NEXT 0 0= EXIT THEN 2drop
   \ … and last of all, a definer this pre-pass never read: one compiled in the
   \ checking process itself, whose clause the checker certified and kept. The
   \ token resolves through the same FIND-SYM every other name does, so the
   \ qualified and the bare-under-`using` spelling reach the one row.
   a u CREATED-TRUST-NEXT? IF 0 0= EXIT THEN
   renders ;

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
   BODY-RESET
   NAME-TOKEN TOKEN-U !  TOKEN-A !
   TOKEN-U @ 0= if E-MISSING-NAME throw then
   DEF-NAME!
   DEF-NAME-A @ DEF-NAME-U @ REFUSE-DUPLICATE IF SKIP-DEFINITION EXIT THEN
   WRAP-RESET
   BODY-LOAD-RESET
   LOCALS-RESET
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE
   BEGIN
      BODY!
      TOKEN-U @ 0= IF E-VS-UNTERMINATED-DEFINITION STATEMENT-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF VERIFY-BODY -1 = IF VERIFY-WRAPPER THEN EXIT THEN
      LOCAL-TOKEN? 0= IF
         TOKEN-A @ TOKEN-U @ s" does>" STR=CI IF VERIFY-DOES EXIT THEN
         TOKEN-A @ TOKEN-U @ WRAP-TOKEN
      THEN
      APPEND-BODY-TOKEN
   AGAIN ;

\ A parsing keyword and its operand are one value on the interpret stack, and
\ the keyword stays the token before whatever follows: `char 0 TYPED-BUFFER B n`
\ hands the count reader `char`, which names the count's word. A load a
\ definition makes waits for the first statement after which the file has
\ closed every scope it opened, and for the file's end at the latest.
: VERIFY-SOURCE ( -- )
   SCAN-RESET
   0 DUPLICATE-U !
   SOURCE@ SOURCE-U @ BASE-LINE @ BASE-COL @ BASE-BYTE @ CHECKER-VERIFY-SOURCE!
   NULL-PTR TOP-PREV-A !  0 TOP-PREV-U !
   0 FILE-PKG !  0 FILE-USE !  PEND-N @ PEND-BASE !
   BEGIN
      NEXT-SCAN dup 0 > WHILE
      2dup TOP-CUR-U ! TOP-CUR-A !
      2dup FILE-SCOPE-STEP
      2dup TOP-PARSER? IF 2drop OPERAND 2drop ELSE
      2dup s" :" CORE-STR= IF 2drop VERIFY-DEFINITION ELSE
      2dup COMPOSE-TOP? IF 2drop ELSE
      2dup RECORD-DEFINER? IF 2drop ELSE 2drop THEN THEN THEN THEN
      FILE-NEUTRAL? IF PEND-RELEASE THEN
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT 2drop
   PEND-RELEASE ;

\ The checker locates the packets it writes in the bytes being scanned, whose
\ first byte lies at the base's file line, column and byte.
: SOURCE-ARM ( -- )
   SOURCE@ SOURCE-U @ BASE-LINE @ BASE-COL @ BASE-BYTE @ DIAG-SOURCE! ;

\ Nested files share the checker window but not the scanner cursor. The saved
\ source and token context belongs to the caller; declarations and learned
\ definers belong to the entire composition. A file's packets locate in that
\ file, and on return or throw the checker is armed with the caller's bytes
\ again, or disarmed when the subject's scan ends, so no nested file's bytes,
\ which its loader frame releases, stay armed.
: COMPOSE-FILE-SCAN ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n :}
   path pathu COMPOSE-DIAG$ {: diag:ptr diagu:n :}
   SOURCE-A @ SOURCE-U @ SCAN-I @
   BASE-LINE @ BASE-COL @ BASE-BYTE @
   TOP-PREV-A @ TOP-PREV-U @ TOP-CUR-A @ TOP-CUR-U @
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @
   FILE-PKG @ FILE-USE @ PEND-BASE @
   {: olda:ptr oldu:n oldi:n oldbl:n oldbc:n oldbb:n
      oldprev:ptr oldprevu:n oldcur:ptr oldcuru:n oldpath:ptr oldpathu:n
      oldpkg:n olduse:n oldbase:n :}
   src srcu SOURCE!
   SOURCE-ARM
   path COMPOSE-CUR-PATH-A !  pathu COMPOSE-CUR-PATH-U !
   diag diagu DIAG-FILE!
   [: VERIFY-SOURCE ;] catch {: rc:n :}
   rc 0<> COMPOSE-STOP-U @ 0= and IF
      diag COMPOSE-STOP-PATH diagu BYTE-COPY
      diagu COMPOSE-STOP-U !
      path pathu COMPOSE-SUBJ? COMPOSE-STOP-SUBJ !
   THEN
   olda SOURCE-A !  oldu SOURCE-U !  oldi SCAN-I !
   oldbl BASE-LINE !  oldbc BASE-COL !  oldbb BASE-BYTE !
   oldprev TOP-PREV-A !  oldprevu TOP-PREV-U !
   oldcur TOP-CUR-A !  oldcuru TOP-CUR-U !
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

CAST: VERIFIER-ACTION ( n -- [ -- ] )

\ One action inside the verifier's package scope: the owner's start puts the
\ checker under mirror authority, so every check the action runs binds a name
\ over the checker's own records (src/core/checker.f REPLAY-BIND), and the done
\ restores the caller's scope on the clean and the throwing path alike.
: RUN-IN-SCOPE ( [ -- ] -- )
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

\ Stash-and-body, the shape src/core/checker.f CHECK-QUIET-CANDIDATE! takes: a
\ quotation cannot read its caller's locals.
PTR-VARIABLE CAND-A
variable CAND-U
variable CAND-VERDICT

: CANDIDATE-BODY ( -- )
   CAND-A @ CAND-U @ CHECK-QUIET-CANDIDATE! CAND-VERDICT ! ;

public

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
   [: COMPOSE-WITH-ROOT ;] catch {: rc:n :}
   0 COMPOSE-ON !
   COMPOSE-REQ0 @ REQUIRE-REG:TRUNCATE
   rc 0<> IF rc throw THEN ;

: SOURCE-COMPOSE-IN-SCOPE ( ptr u8 n ptr u8 n -- )
   2dup SOURCE-COMPOSE-LABELED-IN-SCOPE ;

: SOURCE-COMPOSE-STOPPED$ ( -- ptr u8 n )
   COMPOSE-STOP-U @ 0 > IF COMPOSE-STOP-PATH COMPOSE-STOP-U @ EXIT THEN
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ ;

\ The file SOURCE-COMPOSE-STOPPED$ names is the supplied bytes themselves, not a
\ file a loader statement read: a caller that reports where the composition
\ stopped reads the token from its own bytes there, and from the named file
\ otherwise.
: SOURCE-COMPOSE-STOPPED-SUBJECT? ( -- bool )
   COMPOSE-STOP-U @ 0= COMPOSE-STOP-SUBJ @ or ;

: SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   SOURCE!
   RUN ;

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
