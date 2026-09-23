\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

package VERIFY

PTR-VARIABLE SOURCE-A
variable SOURCE-U
variable SCAN-I
variable SKIP-STRINGS
variable FOUND
variable TOKEN-START
PTR-VARIABLE TOKEN-A
variable TOKEN-U
variable BODY-U
variable LINE-N
variable LINE-START
variable TOKEN-LINE
variable TOKEN-COL
variable TOKEN-BYTE
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

: SOURCE@ ( -- ptr u8 )
   SOURCE-A @ ;

: BASE-RESET ( -- )
   1 BASE-LINE !
   1 BASE-COL !
   0 BASE-BYTE ! ;

: SOURCE! ( ptr u8 n -- )
   BASE-RESET
   SOURCE-U !
   SOURCE-A ! ;

: SOURCE-AT! ( ptr u8 n n n n -- ) {: a:ptr u:n line:n col:n byte:n :}
   a u SOURCE!
   line BASE-LINE !
   col BASE-COL !
   byte BASE-BYTE ! ;

: SCAN-RESET ( -- )
   0 SCAN-I !
   1 LINE-N !
   0 LINE-START ! ;

: SCAN-C@ ( -- n )
   SOURCE@ SCAN-I @ + c@ ;

: SCAN-C+ ( -- n )
   SCAN-C@ {: c:n :}
   SCAN-I @ 1 + SCAN-I !
   c 10 = if
      LINE-N @ 1 + LINE-N !
      SCAN-I @ LINE-START !
   then
   c ;

: TOKEN-START! ( -- )
   SCAN-I @ TOKEN-START !
   BASE-LINE @ LINE-N @ + 1 - TOKEN-LINE !
   SCAN-I @ LINE-START @ - 1 + {: col:n :}
   LINE-N @ 1 = if BASE-COL @ col + 1 - else col then TOKEN-COL !
   BASE-BYTE @ SCAN-I @ + TOKEN-BYTE ! ;

: TOKEN-ORIGIN! ( -- )
   TOKEN-LINE @ TOKEN-COL @ TOKEN-BYTE @ DIAG-ORIGIN! ;

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
   TOKEN-START!
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - ;

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

: NEXT ( -- ptr u8 n )
   BEGIN
      NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 SKIP-PAST ELSE
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
\ The code is the declaration layer's own "declaration too long" (sumtype.f
\ E-TDECL-CAP), re-declared locally the way structure-decl.f and enum-decl.f
\ re-declare their reject codes, because that is precisely the condition: this
\ source is too long for the path that carries it. Source that trips this bound
\ also trips the engine's TDECL-CAP, so both paths answer the same code.
7118 constant E-VS-BODY-CAP

: BODY-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-U @ u + 1 + BODYBUF-CAP > IF E-VS-BODY-CAP throw THEN
   0 BEGIN dup u < WHILE
      dup a + c@  BODY-BUF BODY-U @ + c!
      BODY-U @ 1 + BODY-U !
      1 +
   REPEAT drop
   32 BODY-BUF BODY-U @ + c!  BODY-U @ 1 + BODY-U ! ;

: MAYBE-SIGNATURE ( -- )
   SKIP-WS
   SCAN-I @ SOURCE-U @ < IF
      SOURCE@ SCAN-I @ + c@ 40 = IF
         SCAN-I @ TOKEN-START !
         41 SKIP-PAST
         FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
         SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ -  BODY-APPEND
      THEN
   THEN ;

: REQUIRE-SIGNATURE ( -- ptr u8 n )
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= IF s" verify-source: missing signature" 74 die THEN
   SOURCE@ SCAN-I @ + c@ 40 <> IF s" verify-source: missing signature" 74 die THEN
   SCAN-I @ 1+ TOKEN-START !
   41 SKIP-PAST
   FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
   SOURCE@ TOKEN-START @ + SCAN-I @ TOKEN-START @ - 1 - ;

: STRING-REST ( ptr u8 n -- ptr u8 n ) {: opener:ptr openeru:n :}
   SCAN-I @ {: start:n :}
   opener openeru ESCAPED-STRING-OPENER? IF
      SKIP-ESCAPED-QUOTE
   ELSE
      34 SKIP-PAST
   THEN
   FOUND @ 0= IF s" verify-source: unterminated string" 74 die THEN
   SOURCE@ start + SCAN-I @ start - ;

: APPEND-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BODY-APPEND
   a u STRING-REST BODY-APPEND ;

: SKIP-STRING-REST ( ptr u8 n -- )
   STRING-REST 2drop ;

: PARSE-NEXT? ( ptr u8 n -- bool )
   2dup s" char" CORE-STR= IF 2drop 0 0= exit THEN
   s" [char]" CORE-STR= ;

: APPEND-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN
   TOKEN-A @ TOKEN-U @ BODY-APPEND ;

: APPEND-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      APPEND-NEXT-BODY
      exit
   THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-APPEND
   THEN ;

: SKIP-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF SKIP-NEXT-BODY exit THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST THEN ;

\ Verifier trust rows below cover recursive checker entrypoints, checker-owned
\ mode state, dynamic signature publication, raw-definer mode, and the scope's
\ own name resolution.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: CHECK-BODY ( ptr u8 n -- n )
   CHECK! dup 1 = JSON-DIAGS @ 0= and DIAG-QUIET @ 0= and IF DIAGXT THEN ;

\ The scope's two questions about a name, asked of the checker that owns the
\ scope: RECORD-SYM? names the symbol a definition in THIS source is recorded
\ under (0 when it never was) and FIND-SYM resolves a USE through the open
\ package's private and public wordlists, the global wordlist and the used
\ publics - the same chain a body token resolves through, so a qualified
\ `CODEGEN:BUFFER-E` and a bare `BUFFER-E` under `using CODEGEN` answer one
\ symbol.
TRUSTED: RECORD-SYM? ( ptr u8 n -- n ) CHECKER-RECORD-SYM? ;
\ FIND-SYM is the QUIET resolver: this scan asks it of tokens it is only
\ classifying, and the two refusals the authoritative resolver owns (a used
\ public shadowing a global, a tail two used packages both export) belong to the
\ definition's own check, which resolves the same token straight after.
TRUSTED: FIND-SYM ( ptr u8 n -- n ) CHECKER-FIND-QUIET-SYM ;

\ The same three questions about a definer this pre-pass never read - one
\ compiled in THIS process, whose clause the checker certified at its `;` and
\ whose created-word effect it kept (src/core/checker.f DOES-EFF-LATCH! and the
\ NORETS entry's CREATES cell). The effect stays where the checker built it: it
\ is handed over as a record, not as text, so the type variables the clause
\ declared keep the raw-definer seal the engine's own `trust-raw` gives them.
TRUSTED: CREATES-SYM? ( n -- n ) CHECKER-CREATES-SYM? ;
TRUSTED: RECORD-CREATED ( ptr u8 n n -- bool ) CHECKER-RECORD-CREATED ;
TRUSTED: CREATES-COPY ( n n -- ) CHECKER-CREATES-COPY ;

\ ---- the definers this pre-pass learns from the sources it reads -------------
\ A `create … does>` definition IS a definer, and the effect of every word it
\ creates is the clause's declared one. That is the row the ENGINE publishes for
\ such a word at run time: src/habu/habu2.f DOESPATCH:EMIT hands the parsed
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
\
\ A ROW IS KEYED BY THE CHECKER'S SYMBOL ID for the definer's name, so every
\ spelling that names the definer resolves through the scope chain the checker
\ already owns (FIND-SYM above) instead of through a second name table here.
\ Those ids are the checker's, and a rewound scope truncates them, so the
\ candidate scope this file opens (SOURCE-BUF) releases the rows recorded inside
\ it: no row outlives the ids it names.
\ BOTH TABLES ARE `create … allot`, AND THE ROW TABLE IS NOT A TYPED-BUFFER. This
\ file is itself preverified - tools/build-fixpoint-test.f certifies it through
\ VERIFY:SOURCE-BUF - and the count of a `TYPED-BUFFER` line is read from the
\ TEXT by RECORD-TYPED-BUFFER above, which hands it to the checker's
\ CHECKER-LBUF-COUNT?: decimal digits only. Measured, `DEFINER-CAP TYPED-BUFFER
\ DEFINER-SYM n` certifies as E-CHECKER-LAYOUT-BUFFER (7121) because the token is
\ a constant's name. A decimal literal would certify and then state the capacity
\ twice; one `constant` and two allots state it once.
\ The bound is a scope's, not a file's: a preverified require closure holds
\ several sources in one candidate scope, and the largest single file in the tree
\ carries 28 `does>` today.
128 constant DEFINER-CAP                   \ definer rows
64 constant DEFINER-SIG-SLOT               \ one clause signature: [len][bytes]
create DEFINER-SYM DEFINER-CAP cells allot
create DEFINER-SIG DEFINER-CAP DEFINER-SIG-SLOT * allot
variable DEFINER-N

: DEFINER-SYM@ ( n -- n ) DEFINER-SYM {: row:n a:ptr :}
   row cells a + @ ;

: DEFINER-SYM! ( n n -- ) DEFINER-SYM {: sym:n row:n a:ptr :}
   sym row cells a + ! ;

: DEFINER-MARK ( -- n ) DEFINER-N @ ;

: DEFINER-RELEASE ( n -- ) DEFINER-N ! ;

: DEFINER-SLOT ( n -- ptr u8 ) {: row:n :}
   DEFINER-SIG BYTE-VIEW row DEFINER-SIG-SLOT * + ;

: DEFINER-SIG@ ( n -- ptr u8 n ) {: row:n :}
   row DEFINER-SLOT 1 +  row DEFINER-SLOT c@ ;

\ The checker's own "offset+1, 0 = none" answer shape, for the same reason: a
\ row index of 0 is a real row.
: DEFINER-FIND ( n -- n ) {: sym:n :}         \ sym's row + 1, 0 = no such definer
   sym 0= IF 0 EXIT THEN
   0 BEGIN dup DEFINER-N @ < WHILE
      dup DEFINER-SYM@ sym = IF 1 + EXIT THEN
      1 +
   REPEAT drop 0 ;

: DEFINER-ROW ( n -- n ) {: sym:n :}          \ sym's row, appended when it is new
   sym DEFINER-FIND dup 0<> IF 1 - EXIT THEN drop
   DEFINER-N @ DEFINER-CAP >= IF s" verify-source: too many does> definers" 74 die THEN
   DEFINER-N @ {: row:n :}
   sym row DEFINER-SYM!
   row 1 + DEFINER-N !
   row ;

\ Record `sig` as the effect the definer named by `sym` creates. A name already
\ in the table keeps one row and takes the newer effect, which is what the run
\ time does: a replacement clause replaces the old created-word effect.
: DEFINER-ADD ( ptr u8 n n -- ) {: sig:ptr sigu:n sym:n :}
   sym 0= IF EXIT THEN                        \ never recorded: nothing to hang it on
   sigu DEFINER-SIG-SLOT 1 - > IF s" verify-source: does> signature too long" 74 die THEN
   sym DEFINER-ROW DEFINER-SLOT {: slot:ptr :}
   sigu slot c!
   0 BEGIN dup sigu < WHILE
      dup sig + c@  over slot 1 + + c!
      1 +
   REPEAT drop ;

\ The effect a token's definer gives the word it creates, answered as a string
\ whose ZERO LENGTH means "not a learned definer" - the same shape NEXT-RAW ends
\ a source with. The empty table answers before asking the scope anything, so a
\ source that uses no such definer pays one cell read per token.
: DEFINER-EFFECT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   DEFINER-N @ 0= IF SOURCE@ 0 EXIT THEN
   a u FIND-SYM DEFINER-FIND dup 0= IF drop SOURCE@ 0 EXIT THEN
   1 - DEFINER-SIG@ ;

: MULTI-ERR-MODE? ( -- bool ) MULTI-ERR @ 0<> ;

\ In MULTI-ERR mode a verdict-0 reject RETURNS instead of throwing: CHECK has
\ already emitted the diagnostic, counted MULTI-ERR-N, and recorded the
\ declared signature (no-cascade), so the scan continues at the next
\ definition. Verdict-1 (uncheckable) still throws in BOTH modes: MULTI-ERR-N
\ counts verdict-0 only, so continuing past uncheckables would let an
\ all-uncheckable file exit 0 - fail-open.
\ The verdict is answered rather than swallowed because a created effect is a
\ fact about a definition the checker ACCEPTED: a refused body records nothing.
: VERIFY-BODY ( -- bool )                     \ true = this body certified
   BODY-BUF BODY-U @ CHECK-BODY {: v:n :}
   v -1 = IF 0 0= EXIT THEN
   v 0 = MULTI-ERR-MODE? and IF 0 0= 0= EXIT THEN
   70 throw ;

\ The pre-pass's own does>-clause entry point. It is not the engine's
\ CHECK-DOES!: this scan reaches a clause AFTER the definer's own body has been
\ checked and recorded, so the checker must not latch the created effect here -
\ the next record belongs to the next definition. What this scan learns about a
\ definer it READ goes into the table above instead (src/core/checker.f
\ CHECKER-SOURCE-DOES! carries the reason).
TRUSTED: CHECK-DOES-BODY ( ptr u8 n ptr u8 n -- n )
   CHECKER-SOURCE-DOES! ;

: VERIFY-DOES-BODY ( ptr u8 n -- bool ) {: sig:ptr sigu:n :}
   BODY-BUF BODY-U @ sig sigu CHECK-DOES-BODY {: v:n :}
   v -1 = IF 0 0= EXIT THEN
   v 0 = MULTI-ERR-MODE? and IF 0 0= 0= EXIT THEN
   70 throw ;

\ ---- the two rules that put a definition in the table above ------------------
\ The definition's own name, pinned by VERIFY-DEFINITION before its body is
\ scanned: a created effect is recorded on the definition's own entry, so the
\ recorders below ask the scope for that entry once the body has certified.
PTR-VARIABLE DEF-NAME-A
variable DEF-NAME-U
variable WRAP-DEFINERS                        \ definer calls in this body …
variable WRAP-CTL                             \ … and whether the line ever bent
PTR-VARIABLE WRAP-SIG-A
variable WRAP-SIG-U
variable WRAP-DEF-SYM                         \ … or, for a resident definer, its symbol

: DEF-NAME! ( -- )
   TOKEN-U @ DEF-NAME-U !  TOKEN-A @ DEF-NAME-A ! ;

: WRAP-RESET ( -- )
   0 WRAP-DEFINERS !  0 WRAP-CTL !
   NULL-PTR WRAP-SIG-A !  0 WRAP-SIG-U !  0 WRAP-DEF-SYM ! ;

: DEFINER-RECORD ( ptr u8 n -- )
   DEF-NAME-A @ DEF-NAME-U @ RECORD-SYM? DEFINER-ADD ;

\ The tokens a straight line has none of. The checker's own classifier
\ (src/core/checker.f CF-TOK?) cannot be reused for the question: it is the
\ control-flow DISPATCHER and pushes a frame for every token it recognises, so
\ asking it would move the checker's state. These are its token list, plus the
\ compile-time brackets a definer call must not hide behind.
: WRAP-COND-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" CORE-STR=
   a u s" else" CORE-STR= or
   a u s" then" CORE-STR= or
   a u s" case" CORE-STR= or
   a u s" of" CORE-STR= or
   a u s" endof" CORE-STR= or
   a u s" endcase" CORE-STR= or ;

: WRAP-LOOP-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" begin" CORE-STR=
   a u s" while" CORE-STR= or
   a u s" repeat" CORE-STR= or
   a u s" until" CORE-STR= or
   a u s" again" CORE-STR= or
   a u s" do" CORE-STR= or
   a u s" ?do" CORE-STR= or
   a u s" loop" CORE-STR= or
   a u s" +loop" CORE-STR= or
   a u s" leave" CORE-STR= or
   a u s" exit" CORE-STR= or ;

: WRAP-BRACKET-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" CORE-STR=
   a u s" ;]" CORE-STR= or
   a u s" [" CORE-STR= or
   a u s" ]" CORE-STR= or
   a u s" postpone" CORE-STR= or
   a u s" recurse" CORE-STR= or ;

: WRAP-CTL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u WRAP-COND-TOK?
   a u WRAP-LOOP-TOK? or
   a u WRAP-BRACKET-TOK? or ;

\ Observe one body token for the straight-line-wrapper rule. A body whose tokens
\ hold EXACTLY ONE definer call and no control flow, quotation or bracket creates
\ whatever that definer creates - `: BUFFER ( n -- ) E-CG-CAP E-CG-VALUE
\ BUFFER-E ;` (lib/codegen.f) is the shape. Two definer calls, a conditional
\ definer or a definer inside a quotation record nothing, and the created word
\ then stays unknown exactly as it is today.
\ A RESIDENT definer counts here exactly like a read one. It is remembered by
\ SYMBOL rather than by text because that is how the checker holds what it
\ creates, and the wrapper then inherits the same record instead of a copy of a
\ copy of a signature.
: WRAP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   WRAP-CTL @ IF EXIT THEN
   a u WRAP-CTL-TOK? IF -1 WRAP-CTL ! EXIT THEN
   a u DEFINER-EFFECT dup 0<> IF
      WRAP-SIG-U !  WRAP-SIG-A !
      WRAP-DEFINERS @ 1 + WRAP-DEFINERS !  EXIT
   THEN
   2drop
   a u FIND-SYM {: dsym:n :}
   dsym CREATES-SYM? 0= IF EXIT THEN
   dsym WRAP-DEF-SYM !
   WRAP-DEFINERS @ 1 + WRAP-DEFINERS ! ;

: VERIFY-WRAPPER ( -- )
   WRAP-CTL @ IF EXIT THEN
   WRAP-DEFINERS @ 1 <> IF EXIT THEN
   WRAP-SIG-U @ 0<> IF WRAP-SIG-A @ WRAP-SIG-U @ DEFINER-RECORD EXIT THEN
   DEF-NAME-A @ DEF-NAME-U @ RECORD-SYM? WRAP-DEF-SYM @ CREATES-COPY ;

: VERIFY-DOES ( -- )
   VERIFY-BODY {: ok:bool :}
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   0 BODY-U !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated does body" 74 die THEN
      BODY-U @ 0= if TOKEN-ORIGIN! then
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF
         sig sigu VERIFY-DOES-BODY ok and IF sig sigu DEFINER-RECORD THEN EXIT
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

: CAST-TRUST ( -- )
   DTC-NAME$ DTC-SIG$ DECL-SIGNATURE ;

: RECORD-CAST-IN ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-IN
   CAST-TRUST ;

: RECORD-CAST-OUT ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-OUT
   CAST-TRUST ;

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

: TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   sig sigu DECL-SIGNATURE ;

: SIG-RAW-MODE! ( n -- ) SIG-RAW-MODE ! ;

\ RAW-TRUST-NEXT: like TRUST-NEXT, but registers the created word's effect with
\ TVK-RAW type vars (SIG-RAW-MODE! brackets the checker's signature parse).
\ Used for the raw storage definers create/variable/constant/PTR-VARIABLE and
\ PERSISTED-PTR-VARIABLE so a
\ fetch from their raw cell yields a RAW value that cannot launder into a nominal
\ atom or family (habu-nominal-storage-raw, VALUE side).
: RAW-TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   -1 SIG-RAW-MODE!
   sig sigu DECL-SIGNATURE
   0 SIG-RAW-MODE! ;

\ CREATED-TRUST-NEXT?: RAW-TRUST-NEXT's twin for a definer the checker knows and
\ this pre-pass never read. The row is the checker's own certified one, so there
\ is no signature text to re-parse and no seal to re-apply here; what is left is
\ the same shape - the created word is the NEXT token - and the same answer.
\ THE TOKEN IS TESTED BEFORE THE NAME IS TAKEN: NEXT-SCAN consumes a token, and
\ a token that is not a definer must leave the scan exactly where it was.
: CREATED-TRUST-NEXT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FIND-SYM {: dsym:n :}
   dsym CREATES-SYM? 0= IF 0 0= 0= EXIT THEN
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing defining-word name" 74 die THEN
   name nameu dsym RECORD-CREATED ;

: TRUST-DEFER-SIGNATURE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu REQUIRE-SIGNATURE DECL-SIGNATURE
   name nameu CHECKER-DEFER ;

: TRUST-DEFER ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing defer name" 74 die THEN
   name nameu TRUST-DEFER-SIGNATURE ;

: SKIP-TRUSTED-BODY ( -- )
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated trusted definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: TRUSTED-DEFINITION ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing trusted name" 74 die THEN
   name nameu REQUIRE-SIGNATURE DECL-SIGNATURE
   SKIP-TRUSTED-BODY ;

\ A cast has no body and no `;`, so unlike TRUSTED-DEFINITION above there is
\ nothing to skip: the declaration ends at its closing paren. Registration goes
\ through the certifying registrar, not DECL-SIGNATURE, so an illegal retype is
\ refused here too and not merely recorded.
: CAST-DECLARATION ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing cast name" 74 die THEN
   name nameu REQUIRE-SIGNATURE DEFCAST-SIGNATURE ;

: UNDEFINE-WORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing undefine name" 74 die THEN
   name nameu CHECKER-UNDEFINE ;

: RECORD-PACKAGE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing package name" 74 die THEN
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
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing using name" 74 die THEN
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
   u NOM-TAIL-CAP > IF s" verify-source: nominal name too long" 74 die THEN
   0 NOM-TAIL-U !
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C  NOM-TAIL-BUF NOM-TAIL-U @ + c!
      NOM-TAIL-U @ 1 + NOM-TAIL-U !  1+
   REPEAT drop
   NOM-TAIL-BUF NOM-TAIL-U @ ;

: RECORD-DEFTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing nominal name" 74 die THEN
   name nameu MANGLE {: tail:ptr tailu:n :}
   tail tailu s" 0" CHECKER-DEFFAMILY
   name nameu tail tailu RECORD-CAST-IN
   name nameu tail tailu RECORD-CAST-OUT ;

: RECORD-DEFLINEAR ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deflinear name" 74 die THEN
   name nameu CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" STR=CI ;

: SUMTYPE-END? ( ptr u8 n -- bool )
   s" ;SUMTYPE" STR=CI ;

\ Missing name/arity are reported by CHECKER-DEFFAMILY through the declaration
\ packet (E-BAD-DECLARATION), matching the native path -- no raw pre-check die (§24).
: RECORD-NEWTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   NEXT-SCAN {: ar:ptr aru:n :}
   name nameu ar aru CHECKER-DEFFAMILY ;

: RECORD-SUMTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF                        \ EOF before ;SUMTYPE -> declaration packet (§24)
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFSUM-NOEND
         EXIT
      THEN
      2dup SUMTYPE-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFSUM
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
\ two replay windows below; every other scan in this file keeps NEXT-SCAN, since
\ outside a declaration comments really are inert.
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
   nameu 0= IF s" verify-source: missing enum name" 74 die THEN
   0 BODY-U !
   BEGIN
      DECL-TOKEN
      dup 0= IF s" verify-source: missing ;ENUM" 74 die THEN
      2dup ENUM-END? IF
         BODY-APPEND
         name nameu BODY-BUF BODY-U @ ENUM-DECL:ED-REPLAY
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
   nameu 0= IF s" verify-source: missing structure name" 74 die THEN
   0 BODY-U !
   BEGIN
      DECL-TOKEN
      dup 0= IF s" verify-source: missing ;STRUCTURE" 74 die THEN
      2dup STRUCTURE-DECL-END? IF
         BODY-APPEND
         name nameu BODY-BUF BODY-U @ STRUCTURE-DECL:SD-REPLAY
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: PRODUCT-END? ( ptr u8 n -- bool )
   s" ;PRODUCT" STR=CI ;

\ Metadata-only replay of `PRODUCT name arity FIELD f t .. ;PRODUCT` (mirrors
\ RECORD-SUMTYPE): buffer the `arity FIELD ..` body through ;PRODUCT and
\ register the TK-PRODUCT family + its generated-word metadata rows so later
\ signatures in this source resolve the family. No dictionary words are
\ generated on this path (engine-definer-only, sum parity).
: RECORD-PRODUCT ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing product name" 74 die THEN
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing ;PRODUCT" 74 die THEN
      2dup PRODUCT-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFPRODUCT
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   NEXT-SCAN {: name:ptr nameu:n :}
   NEXT-SCAN {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFLAYOUT-BUFFER ;

\ TYPED-BUFFER / TYPED-VARIABLE gate registration (dot habu-nominal-storage-typed).
\ A stored type may be `ptr* base` or a spaced `[ in -- out ]` xt<effect> quotation
\ (dot habu-typed-xt-storage-ddad4af8), so the type is a contiguous multi-token
\ span from the scanner buffer, not one token.
PTR-VARIABLE STG-A
variable STG-U
PTR-VARIABLE STG-START

: STG-PTR-TOK? ( ptr u8 n -- bool )
   s" ptr" CORE-STR= ;

: STG-QUOT-OPEN? ( ptr u8 n -- bool )
   s" [" CORE-STR= ;

: STG-QUOT-CLOSE? ( ptr u8 n -- bool )
   s" ]" CORE-STR= ;

: SCAN-STORAGE-QUOT ( -- )   \ consume `[ in -- out ]` through the closer
   BEGIN STG-A @ STG-U @ STG-QUOT-CLOSE? 0= WHILE
      NEXT-SCAN STG-U !  STG-A !
      STG-U @ 0= IF s" verify-source: missing storage ]" 74 die THEN
   REPEAT ;

: SCAN-STORAGE-TYPE ( -- ptr u8 n )
   NEXT-SCAN STG-U !  STG-A !
   STG-U @ 0= IF s" verify-source: missing storage type" 74 die THEN
   STG-A @ STG-START !
   BEGIN STG-A @ STG-U @ STG-PTR-TOK? WHILE
      NEXT-SCAN STG-U !  STG-A !
      STG-U @ 0= IF s" verify-source: missing storage pointee" 74 die THEN
   REPEAT
   STG-A @ STG-U @ STG-QUOT-OPEN? IF SCAN-STORAGE-QUOT THEN
   STG-START @  STG-A @ STG-U @ + STG-START @ - ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   NEXT-SCAN {: name:ptr nameu:n :}
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFTYPED-BUFFER ;

: RECORD-TYPED-VARIABLE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFTYPED-VARIABLE ;

\ DYNAMIC-BUFFER (src/core/layout-buffer.f) publishes THREE words from one line -
\ the accessor, NAME-RESERVE and NAME-RELEASE - so the whole triple is registered
\ here. Certification never runs the definer, and without this row a later
\ definition in the same source calling one of the three is E-UNDEFINED:
\ src/habu/aot-decl.f's AOT-NAMES-RESERVE was, which took the stage2 certify pass
\ with it. No count token: a dynamic buffer's extent is set at run time.
: RECORD-DYNAMIC-BUFFER ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFDYNAMIC-BUFFER ;

: RECORD-VALUE-RECORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing value-record name" 74 die THEN
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing END-VALUE-RECORD" 74 die THEN
      2dup VALUE-RECORD-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFRECORD
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-TRUST ( -- )
   STR-LAST-U @ 0= IF s" verify-source: TRUST missing signature string" 74 die THEN
   STR-PREV-U @ 0= IF s" verify-source: TRUST missing name string" 74 die THEN
   STR-PREV-A @ STR-PREV-U @
   STR-LAST-A @ STR-LAST-U @
   TRUST-SIGNATURE ;

\ EXPORT has two documented roles split by package context (dot
\ habu-compiler-pkg-re-688212c1): inside an open package it is the re-export
\ declaration (CHECKER-EXPORT aliases the source's checked effect under its
\ tail); at top level it is the hb-build --repl export directive, which the
\ build strips via COMMENT-EXPORTS before engine load — replay consumes the
\ name and records nothing, exactly like the engine never seeing the line.
: RECORD-EXPORT ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing EXPORT name" 74 die THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? IF name nameu CHECKER-EXPORT THEN ;

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
: RECORD-PRIM-ROW ( ptr u8 n ptr u8 n -- ) {: end:ptr endu:n alt:ptr altu:n :}
   NEXT-RAW dup 0= IF s" verify-source: missing primitive name" 74 die THEN
   2drop
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: missing primitive row closer" 74 die THEN
      end endu alt altu ROW-CLOSER? IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: RECORD-PRIM ( -- )
   s" PRIM;" s" " RECORD-PRIM-ROW ;

: RECORD-PPRIM ( -- )
   NEXT-RAW dup 0= IF s" verify-source: missing primitive package" 74 die THEN
   2drop
   s" PPRIM;" s" CLOSE-PRIVATE" RECORD-PRIM-ROW ;

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
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing structure field name" 74 die THEN
   name nameu sig sigu TRUST-STRUCTURE-FIELD ;

\ Record the size word (`-- n`) then each field accessor with its runtime effect
\ so BEGIN-STRUCTURE layouts self-certify their field uses.
: RECORD-STRUCTURE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing structure name" 74 die THEN
   name nameu s" -- n" DECL-SIGNATURE
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing END-STRUCTURE" 74 die THEN
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
   a u s" immediate" STR=CI IF 0 0= EXIT THEN
   a u s" export" STR=CI IF RECORD-EXPORT 0 0= EXIT THEN
   \ … and last, a definer this pre-pass learned from a `does>` definition
   \ earlier in the closure. The created word is the NEXT token, as it is for
   \ `constant` above - the definer's own arguments precede it - and the effect
   \ is the clause's, registered with the same raw seal the storage definers use.
   a u DEFINER-EFFECT dup 0<> IF RAW-TRUST-NEXT 0 0= EXIT THEN 2drop
   \ … and last of all, a definer this pre-pass never read: one compiled in the
   \ checking process itself, whose clause the checker certified and kept. The
   \ token resolves through the same FIND-SYM every other name does, so the
   \ qualified and the bare-under-`using` spelling reach the one row.
   a u CREATED-TRUST-NEXT? IF 0 0= EXIT THEN
   0 0= 0= ;

: VERIFY-DEFINITION ( -- )
   0 BODY-U !
   BODY!
   TOKEN-U @ 0= if s" verify-source: missing word name" 74 die then
   TOKEN-ORIGIN!
   DEF-NAME!
   WRAP-RESET
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF VERIFY-BODY IF VERIFY-WRAPPER THEN EXIT THEN
      TOKEN-A @ TOKEN-U @ s" does>" CORE-STR= IF VERIFY-DOES EXIT THEN
      TOKEN-A @ TOKEN-U @ WRAP-TOKEN
      APPEND-BODY-TOKEN
   AGAIN ;

: VERIFY-SOURCE ( -- )
   SCAN-RESET
   NULL-PTR TOP-PREV-A !  0 TOP-PREV-U !
   BEGIN
      NEXT-SCAN dup 0 > WHILE
      2dup TOP-CUR-U ! TOP-CUR-A !
      2dup s" :" CORE-STR= IF 2drop VERIFY-DEFINITION ELSE
      2dup RECORD-DEFINER? IF 2drop ELSE 2drop THEN THEN
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT 2drop ;

: THROW-RESULT ( n -- )
   dup 0= IF drop exit THEN
   throw ;

TRUSTED: RUN ( -- )
   CHECKER-VERIFY-PKG-START
   [: VERIFY-SOURCE ;] catch
   CHECKER-VERIFY-PKG-DONE
   THROW-RESULT ;

public

: SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   SOURCE!
   RUN ;

: SOURCE-BUF-AT-IN-SCOPE ( ptr u8 n n n n -- )
   SOURCE-AT!
   RUN ;

\ The definer rows recorded inside this scope go with it: CHECKER-CANDIDATE-
\ SCOPE-DONE rewinds the checker's symbol table, and a row names a symbol by id.
: SOURCE-BUF ( ptr u8 n -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   DEFINER-MARK
   [: RUN ;] catch
   swap DEFINER-RELEASE
   CHECKER-CANDIDATE-SCOPE-DONE
   THROW-RESULT ;

;package
