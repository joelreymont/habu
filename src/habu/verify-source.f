split-str
\ verify-source.f - the pre-compile verifier of checked source: it scans a file,
\ checks each colon definition with CHECK! and records the top-level defining
\ words the checker needs before the native compiler compiles them. Loads after
\ the checker, render and hook support.
\
\ A completion names a byte of the subject. The subject's scan finds the token
\ the byte is in, or the blanks before it, as the scanner reads tokens, and the
\ reader that takes that token answers: a top-level statement, or a top-level
\ tick's operand, has the checker offer the spellings that bind there before
\ the token is consumed; a run a definition's body appends places the cursor in
\ the body's text, where that body's check offers them. A token any other
\ reader takes - a definer's name, a loader's or `using`'s operand, a TRUSTED:
\ body, type syntax - offers nothing, nor does a byte the scan passed inside a
\ comment, a string or a signature, nor one it never reaches. Blanks before a
\ comment or string the scan skips belong to the token after it. One answer at
\ most.
\
\ Each *-OUTCOME word runs a record's reader under its own catch, so a code it
\ throws on keeps the record its throw carried.
\
\ A skipped top-level string literal is kept in one slot so the string the
\ scanner would otherwise discard reaches the row that replays it: a string
\ loader's path and a name before a call of a word a names: row marks. The
\ slot holds the bytes the engine's literal makes: a plain literal's source
\ span, an escaped one's decoded payload. It resets per NEXT-SCAN call, so at a
\ token it holds the literal before it in the same statement, if any. The
\ literal's kind is kept too, 0 when there is none: a composition rejects a
\ string loader by it. So is where its text between the quotes starts and ends
\ in the file, as written, escapes and all, the end 0 when there is none: the
\ span a string loader's load reports, and the text a names: call compares.
\
\ An escaped literal's decoded bytes are not in the source, so they live in one
\ of two buffers. STR-DEC-TURN names the one the next decode writes, and a
\ decode that answers its buffer flips it, so the last answer is never in the
\ buffer the next decode writes, or that a reserve for it moves. No reader
\ reads an older answer after the next decode. A top-level literal's answer
\ goes to the slot, which the handler of the token NEXT-SCAN answers reads
\ before the next NEXT-SCAN resets it. Only the string loaders and a call of a
\ word a names: row marks read the slot; a body literal keeps only its kind.
\
\ A composition rejects the top-level loader forms discovery rejects, where it
\ meets them, so that walk need not run first: a path that is no literal, a
\ literal no loader takes, or a path the loader rejects - one holding a NUL
\ byte, or one the resolver rejects. A declaration or `undefine` of a loader
\ word's name rejects nothing, as the walk rejects nothing for it, but a later
\ loader form with that word the walk reads - `include` or `require`, or a word
\ right after a path literal - at top level or in a body, is rejected at the
\ word as the walk rejects it.
\
\ A composition loads no file a definition names: a word's run is no part of
\ the check, so the ordinary effect checker judges its runtime loader calls.
\
\ Definer rows: a `create … does>` definition is a definer, and every word it
\ creates takes the clause's declared effect, a `TRUSTED:` definer's included,
\ whose body is asserted but whose clause is still a declaration. The rows are
\ learned from the source, not listed, so library definers put no names in the
\ engine; a definer that makes definitions at load, built or from evaluated text,
\ marks the statement's wordlist instead, leaving what it defines to the run,
\ while a `generates:` row and FUNCTION:'s declaration group are still read. A
\ row is keyed by the checker's symbol id for the definer's name, so every
\ spelling resolves through the checker's scope chain. A rewound scope
\ truncates those ids, so the row count is one of the marks the checker's
\ rollback frame rewinds: every scope releases the rows recorded inside it, and
\ no row outlives the ids it names. `undefine` appends a row with no effect,
\ DEFINER-FIND reads the newest row for a name, and a rewound scope releases a
\ retirement with the rest of its rows. Each row's effect lies in DEFINER-SIG,
\ a byte row: a newer effect is appended, so the bytes past the last effect a
\ live row holds are free. Growth may move DEFINER-SIG, so an effect is read
\ where it lies only until the next one is recorded.
\
\ Straight-line wrappers: a body whose tokens hold exactly one definer call and
\ no control flow, quotation or bracket creates whatever that definer creates.
\ Two definer calls, a conditional definer or a definer inside a quotation
\ record nothing, and the created word then stays unknown. Only a definer this
\ pre-pass read is counted: the checker's body walk learns a wrapper of a
\ resident definer, while a definer read here leaves the checker no created
\ effect, so only the definer rows know what it makes. Only a certified body is
\ learned: one deferred to the run names a word only the run can see, which may
\ itself be a definer.
\
\ A storage definer's declaration is read as its definer reads it, on one line:
\ the name on the definer's line and the type's first token on the name's, raw
\ as parse-name reads them, so a `(` or `\` there is a name or type the definer
\ refuses, not a comment, and a token on a later line starts the next
\ statement. The type is a span of tokens (`ptr* base`, a family application,
\ a spaced quotation or scheme) that ends where the checker ends one or with
\ its line. With no name the definer is refused at its own token; with no first
\ token the span is empty and the checker refuses it by name.
\
\ `parses: W n` bounds W to n raw tokens; `parses-through: W n ( E1 E2 )` to n
\ and then every token through the first one equal byte for byte to a listed
\ terminator, inclusive. A row is trusted and never compared with W's body: the
\ call stays the run's, but the scan goes on after it. For a binding none of
\ this scan's rows names, it reads the row the load kept for the symbol the
\ find selects, which a word's facts text gives back: an engine-provided file,
\ which it never reads, states its rows there. A row of this scan is keyed by
\ the checker owner whose scope selected it and by the binding the top-level find selects
\ for W, its symbol and visible record's offset + 1, never by spelling or
\ symbol alone: a redefinition, an `undefine` or another owner selects a
\ binding no row names. A later row for a binding wins.
\
\ A top-level loader's load in the subject's first visit arms its operand span
\ right before it runs; the load takes the span before it descends, and
\ RUN-LOADER clears it however the load ends, so no other load reports it: not
\ a file the load reads, nor the load after it.
\
\ A primitive row is read as its opener reads it: the name, and a package row's
\ package before it, are read raw, so either may be spelled as a definer or
\ parsing word (`EPRIM: create EPRIM;`). The body is read raw too: it holds only
\ atoms, spelled `PE-...` or `;PE-...`, and `REF` clauses, `REF` and the token
\ after it; any other body token makes the row malformed. prims.f's `EPRIM:` and
\ `EPPRIM:` rows end at the first `EPRIM;` or `ECLOSE-PRIVATE` that is not a
\ `REF` operand. A row is known by its opener's
\ spelling: a binding cannot tell prims.f's opener from its own definition when
\ the scan reads prims.f. No `parses-through:` row can bound an engine row:
\ prims.f loads before that word is defined.
\
\ A top-level token no arm acts on is one the load runs; `'` ticks its operand.
\ Under composition (the verifier's child, --all-errors and the check tool's
\ pre-pass) the checker answers what the load does with each: a number is done,
\ any other token resolves as the load resolves it, in source order, over the
\ store this scan filled; `char`'s operand is no name. None of it runs. A word
\ that may read the source after it (it parses or is deferred) takes tokens no
\ scan can know, so the scan stops at it. A name a rendering statement may
\ define is the run's: from it to the next token the scan acts on (a
\ definition, a loader, a definer) the stretch is deferred and nothing in it
\ resolves. A renderer opens no stretch: it reads only its own text and is
\ reported at its token when no row or group declares that text. The
\ verifier's child reports each stop and stretch once, at its token, and
\ answers `deferred` when nothing is refused. A refusal opens a stretch too,
\ unreported: the load stops at it, so the operands of a misspelt parsing word
\ or the rest of a misread declaration are not refused after it.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/source-syntax.f
require src/core/checker-owner-guard.f
require src/core/checker.f
require src/core/check-walk.f
require src/core/rejection.f
require src/core/render.f
require src/lib/string.f
require src/lib/fs.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/workspace.f
require src/habu/primitive-source.f
require src/habu/engine-origin.f
require src/habu/escape.f
require src/lib/ffi-abi.f

package VERIFY
EXPORT CHECK:CHECKER-SIG-SPAN
EXPORT CHECK:DUPLICATE-RECORD
EXPORT CHECK:VERIFY-DEFINER-N
EXPORT CHECK:CHECKER-DEFCAST
EXPORT CHECK:CHECKER-GENERATES
EXPORT CHECK:CHECKER-PARSES-ROW
EXPORT CHECK:CHECKER-NAMES-ROW
EXPORT CHECK:CHECKER-NAMED-USE
EXPORT CHECK:CHECKER-LINEAR
EXPORT CHECK:VERIFY-PARSES-N
EXPORT CHECK:CHECKER-CANDIDATE-SCOPE-START
EXPORT CHECK:CHECKER-CANDIDATE-SCOPE-DONE
EXPORT CHECK:RQM
EXPORT CHECK:REND-RECORD-EFFECT
EXPORT ROLES:DTC-NAME$
EXPORT ROLES:DTC-SIG$
EXPORT ROLES:DTC-BUILD-IN
EXPORT ROLES:DTC-BUILD-OUT
EXPORT ROLES:VRDEF-UNCLOSED
EXPORT TYPE-DECL:CHECKER-DEFFAMILY
EXPORT TYPE-DECL:CHECKER-NOMINAL-REPLAY

public

\ Never thrown by the verifier: a loaded file the system does not read to its
\ end is recorded where the read fails, which then stops with EXIT-STATUS:IOERR.
7201 s" a source file changed while a tool read it" ERROR:CODE E-SOURCE-READ

\ The E-DISC kind of the discovery rejection that stopped the last walk or
\ composition, 0 for none.
variable DISCOVERY-STOPPED

\ A loader form discovery rejects, its record written: the stop, with KIND.
: DISCOVERY-STOP ( n -- )
   DISCOVERY-STOPPED !  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Captured by the verifier child's body as it starts loading. A direct caller
\ that does not supply an entry observation retains checking order.
variable ENTRY-TICK-ORDER
PERSISTED-PTR-VARIABLE ENTRY-TICK-OWNER

: ENTRY-TICK-ORDER! ( n ^u8 -- )
   ENTRY-TICK-OWNER !  ENTRY-TICK-ORDER ! ;

private

PTR-VARIABLE SOURCE-A
variable SOURCE-U
variable SCAN-I
variable SKIP-STRINGS
variable FOUND
variable TOKEN-START
variable TOKEN-BYTE
PTR-VARIABLE TOKEN-A
variable TOKEN-U
variable BODY-U
PTR-VARIABLE STR-LAST-A
variable STR-LAST-U
variable STR-LAST-KIND
variable STR-LAST-AT
variable STR-LAST-END
PTR-VARIABLE TOP-PREV-A
variable TOP-PREV-U
PTR-VARIABLE TOP-REFUSED
PTR-VARIABLE TOP-CUR-A
variable TOP-CUR-U
DYNAMIC-BUFFER BODY-BUF u8
\ Each run BODY-APPEND copies into BODY-BUF is a row of two cells: the body
\ offset it starts at and the source offset it was read from. BODY$ hands the
\ rows to the checker, so a record places its token at the file bytes the
\ scanner read.
DYNAMIC-BUFFER BODY-ROW n
variable BODY-ROWS

\ The subject stays pinned across nested loads so a require back to it observes
\ these bytes, and an include of it scans these bytes again.
variable COMPOSE-ON
\ A standalone index judges top-level calls but follows no loader dependencies.
variable INDEX-ON
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

\ The file the composition's load fault could not read, empty for none.
create FAULT-TARGET PATH-CAP allot
variable FAULT-U

: FAULT-TARGET! ( str -- ) {: a:^ u:n :}
   a FAULT-TARGET u BYTE-COPY
   u FAULT-U ! ;

\ Where the path the composition's load loads is written in the file
\ being scanned, as the program wrote it.
PTR-VARIABLE LOAD-PLACE-A
variable LOAD-PLACE-U
\ The subject is entry text.
TYPED-VARIABLE ENTRY-TEXT bool

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

: CSR-HIT? ( -- bool )
   CSR-STATE @ CSR-SPACE =  CSR-STATE @ CSR-PART =  or ;

\ NEXT-RAW read from PREV past blanks to the token from START to END, START =
\ END at the source's end.
: CSR-HIT ( n n n -- )
   {: prev:n start:n end:n :}
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-STATE @ CSR-SPACE =  CSR-TOK @ 0 <  and IF start CSR-TOK ! EXIT THEN
   CSR-HIT? IF CSR-DONE CSR-STATE ! EXIT THEN
   CSR-STATE @ CSR-WAIT <> IF EXIT THEN
   CSR-AT @ end > IF EXIT THEN
   CSR-AT @ prev < IF CSR-DONE CSR-STATE ! EXIT THEN
   start CSR-TOK !
   CSR-AT @ CSR-TOK @ > IF CSR-PART ELSE CSR-SPACE THEN CSR-STATE ! ;

\ A cursor in the opener offers nothing, nor does one still waiting, past the
\ opener, when the skip ran out of source (FOUND = 0): it is inside the comment
\ or string the file ends in.
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
   CSR-TOK @ src <> IF EXIT THEN
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

defer COMPOSE-FILE ( str str -- renders parses nominal )

: SOURCE@ ( -- ^u8 )
   SOURCE-A @ ;

: SOURCE! ( ^u8 n -- )
   SOURCE-U !
   SOURCE-A !
   0 TOKEN-BYTE ! ;

\ The flags the top-level scan knows a `[if]` may take, the top last (FLAG-STEP).
8 constant FLAG-CAP
FLAG-CAP TYPED-BUFFER FLAGS bool
variable FLAG-N

: SCAN-RESET ( -- )
   0 SCAN-I !  0 FLAG-N ! ;

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

: NEXT-RAW ( -- str )
   SCAN-I @ {: prev:n :}
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= if prev SOURCE-U @ dup CSR-HIT SOURCE@ 0 exit then
   SCAN-I @ TOKEN-START !
   SCAN-I @ TOKEN-BYTE !
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
   prev TOKEN-START @ SCAN-I @ CSR-HIT
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - ;

\ The token at or after byte at, read without moving the scan; empty at the
\ source's end.
: TOKEN-FROM ( n -- str ) {: at:n :}
   SOURCE@ {: s:^ :}
   at BEGIN dup SOURCE-U @ < IF dup s + c@ 33 < ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: b:n :}
   b BEGIN dup SOURCE-U @ < IF dup s + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: e:n :}
   s b +  e b - ;

: STRUCTURE-PTR-FIELD? ( str -- bool )
   s" PTR-FIELD:" STR=CI ;

: STRUCTURE-CFIELD? ( str -- bool )
   s" CFIELD:" STR=CI ;

: STRUCTURE-CELL-FIELD? ( str -- bool )
   s" +FIELD" STR=CI ;

: STRUCTURE-FIELD? ( str -- bool ) {: a:^ u:n :}
   a u STRUCTURE-PTR-FIELD?  a u STRUCTURE-CFIELD? or
   a u STRUCTURE-CELL-FIELD? or ;

\ The definer t tu has no name after it: the record the loader writes for
\ that definer, else the general one, written through its help.
: NO-NAME-RECORD ( str -- ) {: t:^ tu:n :}
   t tu s" defer" STR=CI IF t tu INTERPRETER:DEFER-NO-NAME EXIT THEN
   t tu s" undefine" STR=CI IF t tu UNDEFINE-NO-NAME EXIT THEN
   t tu STRUCTURE-FIELD? IF t tu t tu STRUCT-NO-NAME EXIT THEN
   t tu t tu s" no name follows" REJECTION:MISSING-NAME ;

\ The definer the scan read last, at TOKEN-BYTE, has no name after it.
: NAME-STOP ( -- )
   TOKEN-BYTE @ TOKEN-FROM NO-NAME-RECORD
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The source ends inside the statement's definition, named by the token after
\ its opener.
: UNCLOSED-STOP ( -- )
   TOP-CUR-A @ SOURCE@ - TOP-CUR-U @ + TOKEN-FROM {: t:^ tu:n :}
   t tu t tu INTERPRETER:UNCLOSED-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The declaration t tu opened, which c cu closes, is still open at the token o ou,
\ which may not come inside it, or, with ou 0, when the source ends: the load's
\ record.
: UNCLOSED-DECL-STOP ( str str str -- )
   {: t:^ tu:n c:^ cu:n o:^ ou:n :}
   t tu c cu o ou SOURCE-ROOT:UNCLOSED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Places the empty token at the statement the scan is in, labelled l lu, when
\ a source the record knows holds it.
: STATEMENT-AT ( str -- ) {: l:^ lu:n :}
   TOP-CUR-A @ REJECTION:PLACES? 0= IF EXIT THEN
   TOP-CUR-A @ 0 REJECTION:AT  l lu REJECTION:EMIT-STR ;

: SC-LEAD? ( n -- bool )
   dup $73 = over $53 = or over $63 = or swap $43 = or ;

: STRING-LEAD? ( n -- bool )
   dup SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( str -- bool ) {: a:^ u:n :}
   u 2 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( str -- bool ) {: a:^ u:n :}
   u 3 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $5C <> IF 0 0= 0= EXIT THEN
   a 2 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: STRING-OPENER? ( str -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop 0 0= EXIT THEN
   ESCAPED-STRING-OPENER? ;

: PRINT-OPENER? ( str -- bool )
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

: STR-LAST-RESET ( -- )
   NULL-PTR STR-LAST-A !  0 STR-LAST-U !  0 STR-LAST-KIND !
   0 STR-LAST-AT !  0 STR-LAST-END ! ;

\ Record the bytes A U a literal of the given kind makes and its text as
\ written, the RAWU bytes at RAW in the bytes being scanned.
: STR-LAST! ( str n str -- )
   {: a:^ u:n kind:n raw:^ rawu:n :}
   a STR-LAST-A !
   u STR-LAST-U !
   kind STR-LAST-KIND !
   raw SOURCE@ - STR-LAST-AT !
   STR-LAST-AT @ rawu + STR-LAST-END ! ;

\ The kinds of literal discovery tells apart before a loader: an `s"` or `S"`
\ one, plain or escaped, is a path; a `c"`, `C"` or `."` one is no string a
\ loader takes.
1 constant LIT-PATH
2 constant LIT-OTHER

: LIT-KIND ( str -- n )
   drop c@ dup $73 = swap $53 = or IF LIT-PATH EXIT THEN
   LIT-OTHER ;

\ A literal the source ends inside stops a composition or index at its
\ opener, as discovery's lexer stops there.
: QUIET-UNTERM ( -- )
   FOUND @ 0= COMPOSE-ON @ INDEX-ON @ or 0<> and IF
      TOKEN-START @ TOKEN-FROM INTERPRETER:LITERAL-REJECT
      E-DISC-UNTERM DISCOVERY-STOP
   THEN ;

\ The payload of the literal just skipped: after its opener and delimiter, pfx
\ bytes, and before the quote SCAN-I stands one past. The length is negative
\ when the source ends before the payload starts.
: SKIPPED-PAYLOAD ( n -- str ) {: pfx:n :}
   SOURCE@ TOKEN-START @ + pfx +
   SCAN-I @ TOKEN-START @ - pfx - 1 - ;

\ A plain literal of the given kind, its payload pfx bytes after its opener:
\ the bytes it makes are its text as written.
: RECORD-SKIPPED-STRING ( n n -- )
   {: kind:n pfx:n :}
   QUIET-UNTERM
   pfx SKIPPED-PAYLOAD dup 0 < IF 2drop EXIT THEN
   {: raw:^ rawu:n :}
   raw rawu kind raw rawu STR-LAST! ;

DYNAMIC-BUFFER STR-DEC0 u8
DYNAMIC-BUFFER STR-DEC1 u8
variable STR-DEC-TURN

\ The turn's buffer, with room for u bytes and an address for none.
: STR-DEC-ROOM ( n -- ^u8 ) {: u:n :}
   STR-DEC-TURN @ 0= IF u 1 + STR-DEC0-RESERVE  0 STR-DEC0 EXIT THEN
   u 1 + STR-DEC1-RESERVE  0 STR-DEC1 ;

EXPORT CHECK:CHECKER-ARENA-INFO
EXPORT CHECK:CHECKER-AUTH-PACKAGE$
EXPORT CHECK:CHECKER-AUTH-PACKAGE-ACTIVE?
EXPORT CHECK:CHECKER-AUTH-PACKAGE-MODE@
EXPORT CHECK:CHECKER-CERT-DUP?
EXPORT CHECK:CHECKER-CERT-SHAPE-DUP?
EXPORT CHECK:CHECKER-DEFARENA
EXPORT CHECK:CHECKER-DEFBYTE-BUFFER
EXPORT CHECK:CHECKER-DEFCWORK-SPAN
EXPORT CHECK:CHECKER-DEFCWORK-VARIABLE
EXPORT CHECK:CHECKER-DEFDEFER-LAYOUT-BUFFER
EXPORT CHECK:CHECKER-DEFDYNAMIC-BUFFER
EXPORT CHECK:CHECKER-DEFER
EXPORT CHECK:CHECKER-DEFER-WHY!
EXPORT CHECK:CHECKER-DEFHEAP
EXPORT CHECK:CHECKER-DEFLAYOUT-BUFFER
EXPORT CHECK:CHECKER-DEFLINEAR
EXPORT CHECK:CHECKER-DEFSLOT
EXPORT CHECK:CHECKER-DEFTYPED-BUFFER
EXPORT CHECK:CHECKER-DEFTYPED-VARIABLE
EXPORT CHECK:CHECKER-DYNAMIC-INFO
EXPORT CHECK:CHECKER-END-PACKAGE
EXPORT CHECK:CHECKER-EXPORT-ROW
EXPORT CHECK:CHECKER-EXPORT-SOURCE
EXPORT CHECK:CHECKER-FIND-QUIET-SYM
EXPORT CHECK:CHECKER-GLOBAL
EXPORT CHECK:CHECKER-IMMEDIATE
EXPORT CHECK:CHECKER-LBUF-NAME-OK?
EXPORT CHECK:CHECKER-LBUF-SUFFIXED$
EXPORT CHECK:CHECKER-LINEAR-TAKEN
EXPORT CHECK:CHECKER-PACKAGE
EXPORT CHECK:CHECKER-PRIVATE
EXPORT CHECK:CHECKER-PUBLIC
EXPORT CHECK:CHECKER-QPKG$
EXPORT CHECK:CHECKER-QTAIL$
EXPORT CHECK:CHECKER-QUALIFIED?
EXPORT CHECK:CHECKER-RECORD-REJECT
EXPORT CHECK:CHECKER-SIG-UNRES-TAKE
EXPORT CHECK:CHECKER-SPLIT-STR
EXPORT CHECK:CHECKER-STORAGE-NAME-REFUSE
EXPORT CHECK:CHECKER-STORAGE-TYPE-REJECT
EXPORT CHECK:CHECKER-TRY-RECORD
EXPORT CHECK:CHECKER-TYPE-SPAN-BREAK?
EXPORT CHECK:CHECKER-TYPE-SPAN-STEP
EXPORT CHECK:CHECKER-UNDEFINE
EXPORT CHECK:CLOSE-USING
EXPORT CHECK:DEF-NAME-REJECT?
EXPORT CHECK:DOES-UNMADE-OPEN
EXPORT CHECK:IMM-READS
EXPORT CHECK:MULTI-ERR?
EXPORT CHECK:OPEN-USING
EXPORT CHECK:PROW-ADD-TEXT
EXPORT CHECK:PROW-FIND
EXPORT CHECK:SIG-RAW-MODE
EXPORT CHECK:SIG-STATES-FACTS
EXPORT CHECK:SPLIT-STR-FILE?
EXPORT CHECK:STG-SCRATCH-SAVED
EXPORT CHECK:STG-SCRATCH-TYPE
EXPORT CHECK:TYPE-RESERVED?
EXPORT CHECK:TYPE-SPAN-END?
EXPORT CHECK:UNSEEN-COVERS?
EXPORT CHECK:USIG-NEWEST
EXPORT CHECK:VREC-TOKEN
EXPORT TYPE-DECL:SCRATCH-MARK
\ The bytes the engine's `s\"` makes of an escaped literal's payload, and
\ whether every escape in it is one the engine reads. An escape is spelt
\ with more bytes than it decodes to, so a decode as long as its payload met no
\ escape: the answer is the source span, and a diagnostic at
\ the literal keeps its position.
: ESC-BYTES ( str -- str bool ) {: src:^ u:n :}
   u STR-DEC-ROOM {: dst:^ :}
   src u dst ESCAPE:DECODE {: k:n ok:bool :}
   k u = ok 0= or IF src u ok EXIT THEN
   1 STR-DEC-TURN @ - STR-DEC-TURN !
   dst k ok ;

\ A bad escape is rejected at the opener with the engine's record of a bad
\ string literal. An unterminated literal is left to the lexer, as a plain one
\ is, except in a composition or index.
: RECORD-ESCAPED-STRING ( n -- )
   {: kind:n :}
   QUIET-UNTERM
   FOUND @ 0= IF EXIT THEN
   4 SKIPPED-PAYLOAD {: raw:^ rawu:n :}
   raw rawu ESC-BYTES 0= IF
      SOURCE@ TOKEN-START @ + 3 INTERPRETER:LITERAL-REJECT
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN
   2dup raw rawu SRCLOC:LITERAL!
   kind raw rawu STR-LAST! ;

\ A body token is looked up among the live locals, byte for byte, after
\ comments and before every keyword but `;`. Each body reader resets the table;
\ a block's end retires its locals.
DYNAMIC-BUFFER LOCAL-A ^u8
DYNAMIC-BUFFER LOCAL-U n
DYNAMIC-BUFFER LOCAL-D n                      \ the block depth that declared it
variable LOCAL-N
variable LOCAL-DEPTH                          \ blocks open, counted while a local lives

: LOCALS-RESET ( -- )
   0 LOCAL-N !  0 LOCAL-DEPTH ! ;

: LOCAL? ( str -- bool ) {: a:^ u:n :}
   LOCAL-N @ 0 ?do
      a u i LOCAL-A @ i LOCAL-U @ CORE-STR= if 0 0= unloop exit then
   loop
   0 0= 0= ;

\ A file whose first two bytes are `#!` names its interpreter on that line, and
\ the loader reads the line as a comment: a token at the source's first byte.
: SHEBANG? ( str -- bool )
   {: a:^ u:n :}
   a SOURCE@ <>  u 2 <  or IF 0 0= 0= EXIT THEN
   a c@ $23 =  a 1 + c@ $21 =  and ;

\ The byte that ends the comment a token opens, as the loader reads it: the
\ line's end after `\`, `)` after `(`; 0 for any other token.
: COMMENT-END ( str -- n )
   {: a:^ u:n :}
   u 1 <> IF 0 EXIT THEN
   a c@ 92 = IF 10 EXIT THEN
   a c@ 40 = IF 41 EXIT THEN
   0 ;

: NEXT ( -- ^u8 n )
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

: NEXT-SCAN ( -- str )
   -1 SKIP-STRINGS !
   STR-LAST-RESET
   NEXT ;

: NEXT-BODY ( -- str )
   0 SKIP-STRINGS !
   NEXT ;

: RAW! ( -- )
   NEXT-RAW  TOKEN-U !  TOKEN-A ! ;

: BODY! ( -- )
   NEXT-BODY  TOKEN-U !  TOKEN-A ! ;

\ A body read and never checked as a definition's (a TRUSTED: or type body)
\ drops the cursor it placed.
: BODY-RESET ( -- )
   CSR-STATE @ CSR-PLACED = IF CSR-DONE CSR-STATE ! THEN
   0 BODY-U !
   0 BODY-ROWS ! ;

: BODY-ROW! ( n -- ) {: src:n :}
   BODY-ROWS @ 2 * {: at:n :}
   BODY-U @  at BODY-ROW !
   src  at 1 + BODY-ROW !
   BODY-ROWS @ 1 + BODY-ROWS ! ;

\ The buffers grow to hold every run, so a body is never truncated: a dropped
\ token would make a well-formed, wrong declaration. Both are reserved before
\ the run changes any state, so a refused reservation leaves the body as it was.
: BODY-APPEND ( str -- )
   {: a:^ u:n :}
   a SOURCE@ - {: src:n :}
   src 0 <  src u + SOURCE-U @ >  or IF
      s" verify-source: body run outside the source" EXIT-STATUS:INTERNAL die
   THEN
   BODY-U @ u + 1 + {: end:n :}
   end BODY-BUF-RESERVE
   BODY-ROWS @ 1 + 2 * BODY-ROW-RESERVE
   src CSR-PLACE
   src BODY-ROW!
   a  0 BODY-BUF BODY-U @ +  u BYTE-COPY
   32  BODY-U @ u + BODY-BUF c!
   end BODY-U ! ;

\ The armed addresses hold until the next BODY-APPEND, which may move both
\ buffers; every check calls BODY$ again before it reads them.
: BODY$ ( -- ^u8 n )
   BODY-U @ 1 max BODY-BUF-RESERVE
   BODY-ROWS @ 2 * 1 max BODY-ROW-RESERVE
   0 BODY-BUF BODY-U @ 0 BODY-ROW BODY-ROWS @ REJECTION:MAP!
   0 BODY-BUF BODY-U @ ;

\ The body holds no closer, so the checker is told where it stands and refuses
\ a structure still open there.
: DEF-BODY$ ( -- str )
   BODY$ {: a:^ u:n :}
   TOKEN-A @ TOKEN-U @ a REJECTION:CLOSER!  a u ;

\ The signature ahead of the scan, read as the engine reads a definition head's:
\ from its `(` through its `)`, whether one opens there and whether it closes.
\ The scan passes a closed one.
: SIG-GROUP ( -- str bool bool )
   SOURCE@ SCAN-I @ +  SOURCE-U @ SCAN-I @ -
   {: at:^ left:n :}
   at left CHECKER-SIG-SPAN
   {: open:n close:n :}
   open left = IF at 0 false false EXIT THEN
   close 0= IF at 0 true false EXIT THEN
   SCAN-I @ close + SCAN-I !
   at open +  close open -  true true ;

\ SIG-GROUP, and whether one opens; one that never closes leaves the statement's
\ definition unclosed.
: SCAN-SIG ( -- str bool )
   SIG-GROUP {: open:bool closed:bool :}
   open closed 0= and IF UNCLOSED-STOP THEN
   open ;

: MAYBE-SIGNATURE ( -- str )
   SCAN-SIG 0= IF EXIT THEN
   2dup BODY-APPEND
   {: sig:^ sigu:n :}
   sig 1 +  sigu 2 - ;

\ The record of t tu, a definition's name or `does>`, with no closed signature
\ after it.
: SIGNATURE-STOP ( str -- ) {: t:^ tu:n :}
   t tu INTERPRETER:SIGNATURE-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The signature that must follow t tu; with none the scan stops.
: SIGNATURE-AT ( str -- str )
   {: t:^ tu:n :}
   SIG-GROUP nip {: sig:^ sigu:n closed:bool :}
   closed 0= IF t tu SIGNATURE-STOP THEN
   sig 1 +  sigu 2 - ;

: STRING-REST ( str -- str ) {: opener:^ openeru:n :}
   SCAN-I @ {: start:n :}
   opener openeru ESCAPED-STRING-OPENER? IF
      SKIP-ESCAPED-QUOTE
   ELSE
      34 SKIP-PAST
   THEN
   QUIET-UNTERM
   FOUND @ 0= IF
      opener openeru INTERPRETER:LITERAL-REJECT
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN
   SOURCE@ start + SCAN-I @ start - ;

: FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: STR=CI ( str str -- bool ) {: a:^ u:n b:^ v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C
      over b + c@ FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

\ The tokens a straight line has none of. The checker's own classifier cannot
\ be reused for the question: it pushes a frame for every token it recognises,
\ so asking it would move the checker's state. These are its token list, plus
\ the compile-time brackets a definer call must not hide behind. Each matches
\ case-folded, as the engine's keywords ignore case, so `EXIT` ends the line as
\ `exit` does.
: WRAP-COND-TOK? ( str -- bool ) {: a:^ u:n :}
   a u s" if" STR=CI
   a u s" else" STR=CI or
   a u s" then" STR=CI or
   a u s" case" STR=CI or
   a u s" of" STR=CI or
   a u s" endof" STR=CI or
   a u s" endcase" STR=CI or ;

: WRAP-LOOP-TOK? ( str -- bool ) {: a:^ u:n :}
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

: WRAP-BRACKET-TOK? ( str -- bool ) {: a:^ u:n :}
   a u s" [:" STR=CI
   a u s" ;]" STR=CI or
   a u s" [" STR=CI or
   a u s" ]" STR=CI or
   a u s" postpone" STR=CI or
   a u s" recurse" STR=CI or ;

: WRAP-CTL-TOK? ( str -- bool ) {: a:^ u:n :}
   a u WRAP-COND-TOK?
   a u WRAP-LOOP-TOK? or
   a u WRAP-BRACKET-TOK? or ;

\ The record of a loader form at t tu that discovery cannot follow, for the
\ message m mu, written through the help HELP writes for t tu: the caller stops
\ it.
: FORM-REJECT ( str str [ str -- ] -- ) {: t:^ tu:n m:^ mu:n help :}
   s" E-LOADER-FORM" REJECTION:OPEN  m mu REJECTION:EMIT-STR
   t tu REJECTION:AT
   s" discovery cannot follow `" REJECTION:EMIT-STR  t tu REJECTION:EMIT-STR
   s" `" REJECTION:EMIT-STR
   REJECTION:HELP
   t tu help execute ;

: TICKED ( ^u8 n -- )
   s" `" REJECTION:EMIT-STR  REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR ;

public
\ Discovery and a composition write the record of a loader form they reject,
\ at its token t tu, with these, and stop it with DISCOVERY-STOP.
: SHADOW-REJECT ( str -- )
   s" loader word used after the file defined its name"
   [: s" load files before the definition that reuses " REJECTION:EMIT-STR  TICKED
      s" 's name, or rename that definition" REJECTION:EMIT-STR ;] FORM-REJECT ;
: RETIRE-REJECT ( str -- )
   s" loader word used after the file retired it"
   [: s" load files before `undefine` retires " REJECTION:EMIT-STR  TICKED ;] FORM-REJECT ;
: DYNAMIC-REJECT ( str -- )
   s" dynamic (non-literal) loader path"
   [: 2drop S\" write the path as a literal: `require path/file.f` or `s\" path/file.f\" required`" REJECTION:EMIT-STR ;]
   FORM-REJECT ;
: OPENER-REJECT ( str -- )
   s" unsupported string opener before a loader word"
   [: 2drop S\" write the path with `s\"`: `s\" path/file.f\" required`" REJECTION:EMIT-STR ;]
   FORM-REJECT ;
: CAPACITY-REJECT ( str -- )
   s" capacity exceeded"
   [: 2drop s" shorten the path to at most " REJECTION:EMIT-STR  PATH-CAP REJECTION:EMIT-U
      s"  bytes" REJECTION:EMIT-STR ;] FORM-REJECT ;
private

\ A loader word u bytes long at TOKEN-BYTE with no literal path: its record,
\ stopped as E-DISC-DYNAMIC.
: DYNAMIC-STOP ( n -- ) {: u:n :}
   SOURCE@ TOKEN-BYTE @ + u DYNAMIC-REJECT  E-DISC-DYNAMIC DISCOVERY-STOP ;

\ A loader word's bit in SHADOWED and RETIRED, 0 for any other name: the words
\ discovery reads as loaders, `script-required` not one.
: LOADER-BIT ( str -- n ) {: a:^ u:n :}
   a u s" include" STR=CI IF $01 EXIT THEN
   a u s" included" STR=CI IF $02 EXIT THEN
   a u s" require" STR=CI IF $04 EXIT THEN
   a u s" required" STR=CI IF $08 EXIT THEN
   a u s" provided" STR=CI IF $10 EXIT THEN
   0 ;

\ The loader words the file being read has declared a word of, and those it has
\ retired: past either, the walk, which holds no wordlists, cannot tell which
\ word the spelling names.
variable SHADOWED
variable RETIRED

\ The symbol of the newest definition, which an `immediate` marks: 0 from its
\ name until it is recorded, so one after a refused definition marks nothing.
\ A definer that can end without DEFINED clears it as it starts.
variable NEWEST-SYM

: NAME-DECLARED ( str -- )
   0 NEWEST-SYM !
   LOADER-BIT SHADOWED @ or SHADOWED ! ;

: NAME-RETIRED ( str -- )
   LOADER-BIT RETIRED @ or RETIRED ! ;

\ A loader form the walk reads, its word u bytes long at TOKEN-BYTE.
: QUIET-USE ( str -- ) {: a:^ u:n :}
   a u LOADER-BIT {: bit:n :}
   SOURCE@ TOKEN-BYTE @ + {: t:^ :}
   bit SHADOWED @ and 0<> IF
      t u SHADOW-REJECT  E-DISC-SHADOW DISCOVERY-STOP
   THEN
   bit RETIRED @ and 0<> IF
      t u RETIRE-REJECT  E-DISC-RETIRE DISCOVERY-STOP
   THEN ;

\ A string loader u bytes long, after a literal of the given kind. True when it
\ may load the literal's path, which the resolver alone may still refuse.
: LITERAL-LOAD? ( n n -- bool ) {: kind:n u:n :}
   kind 0= IF u DYNAMIC-STOP THEN
   kind LIT-OTHER = IF
      SOURCE@ TOKEN-BYTE @ + u OPENER-REJECT  E-DISC-OPENER DISCOVERY-STOP
   THEN
   kind LIT-PATH = ;

\ The kind (LIT-KIND) of the literal the body token before closed, 0 for none.
variable BODY-LIT-KIND

: BODY-PREV-CLEAR ( -- )
   0 BODY-LIT-KIND ! ;

\ Whether the scan models the load of a sealed engine: this one, composing. A stage source
\ certified without composing keeps its packages open until its capture seals them.
: SEALED-LOAD? ( -- bool )
   COMPOSE-ON @ 0<>  SEAL-NDICT@ 0<>  and ;

\ The record names a u and is placed at the TU-byte source token written at
\ byte AT, which is not a u when the name is generated.
: SEAL-STOP ( str n n -- ) {: a:^ u:n at:n tu:n :}
   a u  SOURCE@ at + tu  REJECTION:SEALED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

EXPORT CHECKER-RESOLVE:SOURCE-SEALED?
EXPORT CHECKER-RESOLVE:SOURCE-SEAL
EXPORT CHECKER-RESOLVE:SOURCE-SEALS-CLEAR

: OPEN-SEALED? ( -- bool )
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF false EXIT THEN
   CHECKER-AUTH-PACKAGE$ SOURCE-SEALED? ;

\ Whether a part is open and the source sealed the package it is a part of.
: OWNER-SEALED? ( -- bool )
   data-base ENCLOSING:REC + @ {: owner:n :}
   owner 0= IF false EXIT THEN
   owner XREF-ADDR>REC XREF-NAME$ SOURCE-SEALED? ;

: SEALED-OPEN ( str n n -- ) {: a:^ u:n at:n tu:n :}
   OPEN-SEALED? IF a u at tu SEAL-STOP THEN
   OWNER-SEALED?  CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-GLOBAL =  and IF
      a u at tu SEAL-STOP
   THEN ;

EXPORT CHECKER-RESOLVE:HEAD-ROW

\ A head resolves as the definition's does: a part is asked by its path. A
\ qualified name goes where its qualifier says, whatever the section: while a
\ part's owner is sealed, only the open part's own head takes it.
: SEALED-NAME ( str n n -- ) {: a:^ u:n at:n tu:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q 0 < IF a u at tu SEALED-OPEN EXIT THEN
   a q HEAD-ROW {: row:^ :}
   row NULL-PTR = IF a q ELSE row XREF-NAME$ THEN SOURCE-SEALED? IF a u at tu SEAL-STOP THEN
   row NULL-PTR <> IF row XREF:SEALED? IF a u at tu SEAL-STOP THEN THEN
   OPEN-SEALED? IF a u at tu SEAL-STOP THEN
   row NULL-PTR = IF false ELSE row XREF-PKG-PUBLIC data-base PKG-PUB-CELL + @ = THEN
   0=  OWNER-SEALED?  and IF a u at tu SEAL-STOP THEN
   SEALED-LOAD? 0= IF EXIT THEN
   row NULL-PTR = IF EXIT THEN
   row XREF-PKG-PUBLIC XREF-WID-PROTECTED? IF a u at tu SEAL-STOP THEN ;

\ The family name a declaration writes at byte AT: its token up to the
\ first `<`, rejected as SEALED-NAME rejects a name. An empty one is left to
\ the declaration's missing-name rejection.
: SEALED-FAMILY ( str n -- ) {: a:^ u:n at:n :}
   a u TYPE-DECL:DECL-FAMILY-NAME nip {: cut:n :}
   cut 0= IF EXIT THEN
   a cut at cut SEALED-NAME ;

\ The kind of a literal from the rest STRING-REST read after its string opener.
\ An escaped one's payload is decoded (ESC-BYTES) only to reject a bad escape
\ at the opener.
: BODY-LIT! ( str str -- ) {: o:^ ou:n s:^ su:n :}
   BODY-PREV-CLEAR
   su 2 < IF EXIT THEN
   s 1 + su 2 -
   o ou NORMAL-STRING-OPENER? 0= IF
      ESC-BYTES 0= IF
         o ou INTERPRETER:LITERAL-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
   THEN
   2drop
   o ou LIT-KIND BODY-LIT-KIND ! ;

\ The record of a declaration-owner record with no operation for a step the
\ scan takes, at the statement it is in when a source the record knows holds
\ it. The scan knows the field's offset only, so no operation is named.
: OWNER-STOP ( -- )
   TOP-CUR-A @ {: p:^ :}
   p REJECTION:PLACES? IF p ELSE NULL-PTR THEN {: t:^ :}
   NULL-PTR 0 t 0 CHECKER-OWNER-GUARD:MISSING-OP ;

EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
NCOMP-DISPATCH:DECL-CELL RESERVED-PTR-U8-CELL SOURCE-OWNER-A

\ Replay-only checker queries use the live declaration owner.
: OWNER-XT ( n -- n ) {: off:n :}
   SOURCE-OWNER-A @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF OWNER-STOP THEN ;

CAST: BINDING-ACTION ( n -- [ ^u8 n -- n n n ] )
: TOP-BINDING ( str -- n n n )
   CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF OWNER-XT BINDING-ACTION execute ;

: CORE-OP? ( str -- bool )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-CORE-OP and 0<> ;

\ The answer F of the engine's predicate the token a u spells, and whether
\ the token binds that word rather than a program's.
: ENGINE-GUARD ( str bool -- bool bool ) {: a:^ u:n f:bool :}
   a u CORE-OP? 0= IF false false EXIT THEN
   f true ;

\ The flag the engine's target predicate the token a u binds pushes, and
\ whether the token binds one.
: TARGET-GUARD ( str -- bool bool ) {: a:^ u:n :}
   a u s" HB-TARGET-LINUX?" STR=CI IF
      a u HB-TARGET-LINUX? ENGINE-GUARD EXIT
   THEN
   a u s" HB-TARGET-MACOS?" STR=CI IF
      a u HB-TARGET-MACOS? ENGINE-GUARD EXIT
   THEN
   a u s" HB-TARGET-LINUX-X86-64?" STR=CI IF
      a u HB-TARGET-LINUX-X86-64? ENGINE-GUARD EXIT
   THEN
   a u s" HB-TARGET-LINUX-KERNEL?" STR=CI IF
      a u HB-TARGET-LINUX-KERNEL? ENGINE-GUARD EXIT
   THEN
   a u s" HB-TARGET-WASM?" STR=CI IF
      a u HB-TARGET-WASM? ENGINE-GUARD EXIT
   THEN
   false false ;

: BODY-USE ( str -- ) {: a:^ u:n :}
   COMPOSE-ON @ 0= IF EXIT THEN
   a u s" include" STR=CI  a u s" require" STR=CI or
   BODY-LIT-KIND @ LIT-PATH = or IF a u QUIET-USE THEN ;

: BODY-TOKEN-SEEN ( str -- )
   BODY-USE BODY-PREV-CLEAR ;

\ The rest starts with the opener's one delimiter, which the blank BODY-APPEND
\ writes after the opener stands for, so it joins past that delimiter: the
\ checker reads the literal's bytes as the engine makes them, and a names: row
\ binds the word they name.
: APPEND-STRING ( ^u8 n -- ) {: a:^ u:n :}
   a u BODY-APPEND
   a u STRING-REST {: s:^ su:n :}
   s 1 + su 1 - BODY-APPEND
   a u s su BODY-LIT! ;

: SKIP-STRING-REST ( str -- ) {: a:^ u:n :}
   a u a u STRING-REST BODY-LIT! ;

\ A parsing keyword's operand is its next token, data whatever it spells; a body
\ local is looked up first.
: CHAR-KEYWORD? ( str -- bool )
   s" char" STR=CI ;

: TOP-PARSER? ( str -- bool ) {: a:^ u:n :}
   a u CHAR-KEYWORD? IF 0 0= EXIT THEN
   a u s" '" STR=CI ;

: BODY-PARSER? ( str -- bool ) {: a:^ u:n :}
   a u s" [']" STR=CI IF 0 0= EXIT THEN
   a u s" postpone" STR=CI IF 0 0= EXIT THEN
   a u s" [char]" STR=CI ;

: OPERAND ( -- ^u8 n )
   NEXT-RAW dup 0= IF NAME-STOP THEN ;

\ A definer takes its name as the next raw token, so `create s"` names a word
\ `s"`; every definer rejects a `(` or `\` name (REJECTION:NOT-A-NAME). A
\ comment or string rule there would hand the definer a later token instead.
\ With no token left TOKEN-BYTE stays at the definer.
: NAME-TOKEN ( -- str )
   NEXT-RAW ;

: LOCAL-TOKEN? ( -- bool )
   TOKEN-A @ TOKEN-U @ LOCAL? ;

: LOCAL-ADD ( str -- ) {: a:^ u:n :}
   LOCAL-N @ 1 + LOCAL-A-RESERVE
   LOCAL-N @ 1 + LOCAL-U-RESERVE
   LOCAL-N @ 1 + LOCAL-D-RESERVE
   0 BEGIN dup u < IF a over + c@ $3A <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   LOCAL-N @ LOCAL-U !
   a LOCAL-N @ LOCAL-A !
   LOCAL-DEPTH @ LOCAL-N @ LOCAL-D !
   LOCAL-N @ 1 + LOCAL-N ! ;

: BLOCK-OPENER? ( str -- bool )
   SOURCE-SYNTAX:BLOCK-OPENER? ;

: BLOCK-CLOSER? ( str -- bool )
   SOURCE-SYNTAX:BLOCK-CLOSER? ;

: BLOCK-DROP ( -- )
   BEGIN
      LOCAL-N @ 0 > IF LOCAL-N @ 1 - LOCAL-D @ LOCAL-DEPTH @ >= ELSE 0 0= 0= THEN
   WHILE
      LOCAL-N @ 1 - LOCAL-N !
   REPEAT ;

: BLOCK-STEP ( str -- ) {: a:^ u:n :}
   LOCAL-N @ 0= IF EXIT THEN
   a u BLOCK-OPENER? IF LOCAL-DEPTH @ 1 + LOCAL-DEPTH ! EXIT THEN
   a u s" else" STR=CI IF BLOCK-DROP EXIT THEN
   a u BLOCK-CLOSER? IF BLOCK-DROP LOCAL-DEPTH @ 1 - LOCAL-DEPTH ! THEN ;

: GROUP-TOKEN ( str -- bool ) {: a:^ u:n :}
   a u s" :}" CORE-STR= IF 0 0= EXIT THEN
   a u LOCAL-ADD
   0 0= 0= ;

variable GROUP-AT

\ The next token of a group. The source ending inside one is rejected at the
\ group's `{:`; a composition or index stops as discovery's lexer stops there
\ (E-DISC-UNTERM).
: GROUP-NEXT ( -- str )
   NEXT-RAW dup 0= IF
      GROUP-AT @ TOKEN-FROM INTERPRETER:GROUP-REJECT
      COMPOSE-ON @ INDEX-ON @ or 0<> IF E-DISC-UNTERM DISCOVERY-STOP THEN
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN ;

: APPEND-GROUP ( -- )
   BEGIN GROUP-NEXT 2dup BODY-APPEND GROUP-TOKEN UNTIL ;

: SKIP-GROUP ( -- )
   BEGIN GROUP-NEXT GROUP-TOKEN UNTIL ;

\ The path `include` or `require` takes, the word wu bytes long and standing at
\ TOKEN-BYTE: none is rejected at the word.
: LOADER-OPERAND ( n -- str ) {: wu:n :}
   NEXT-RAW {: p:^ pu:n :}
   pu 0= IF
      SOURCE@ TOKEN-BYTE @ + wu INCLUDE-NO-PATH  E-DISC-DYNAMIC DISCOVERY-STOP
   THEN
   p pu ;

\ The tokens an admitted immediate's `parses:` count names are read raw, across
\ line ends, and never join the body; a using rejection is left to the definition's check.
: SKIP-PAYLOAD ( str -- ) {: a:^ u:n :}
   a u a u CHECKER-FIND-QUIET-SYM IMM-READS {: cnt:n :}
   cnt 0 > IF cnt 0 ?DO NEXT-RAW 2drop LOOP THEN ;

\ A local or a group skips BODY-TOKEN-SEEN, so it clears the literal before it:
\ a token after it is no word right after a path literal.
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
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      TOKEN-A @ TOKEN-U @ SKIP-PAYLOAD
   THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ BODY-PARSER? IF OPERAND 2drop BODY-PREV-CLEAR exit THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST exit THEN
   TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN
   TOKEN-A @ TOKEN-U @ SKIP-PAYLOAD ;

\ SKIP-BODY-TOKEN for a definition body, which declares locals.
: SKIP-DEF-TOKEN ( -- )
   LOCAL-TOKEN? IF BODY-PREV-CLEAR EXIT THEN
   TOKEN-A @ TOKEN-U @ s" {:" CORE-STR= IF
      BODY-PREV-CLEAR  TOKEN-BYTE @ GROUP-AT !  SKIP-GROUP EXIT
   THEN
   TOKEN-A @ TOKEN-U @ BLOCK-STEP
   SKIP-BODY-TOKEN ;

\ The owner record stores raw execution tokens; each view states the signature
\ of the field it calls.
CAST: SYM-ACTION ( n -- [ ^u8 n -- n ] )
CAST: IDENTITY-ACTION ( n -- [ n -- ^u8 n ^u8 n n ] )
CAST: SOURCE-ACTION ( n -- [ n -- n ] )
CAST: CREATES-ACTION ( n -- [ n -- n ] )
CAST: CREATED-ACTION ( n -- [ ^u8 n n -- bool ] )
CAST: DOES-ACTION ( n -- [ ^u8 n ^u8 n ^u8 n -- n ] )
CAST: RENDERS-ACTION ( n -- [ ^u8 n n -- bool ] )
CAST: REACH-ACTION ( n -- [ ^u8 n ^u8 n -- ] )
CAST: CLAUSE-ACTION ( n -- [ ^u8 n -- ] )
CAST: DECL-ACTION ( n -- [ ^u8 n ^u8 n -- bool ] )
CAST: TICK-ORDER-ACTION ( n -- [ ^u8 n n ^u8 [ -- nominal ] -- n nominal ] )

: WITH-BODY-TICK-ORDER ( str [ -- nominal ] -- bool )
   {: a:^ u:n q :}
   a u DEF-TICK-ORDER @ DEF-TICK-OWNER @ q
   NCOMP-DISPATCH:DECL-WITH-TICK-ORDER-OFF OWNER-XT TICK-ORDER-ACTION execute 0<> ;

CAST: VERIFIER-ACTION ( n -- [ -- ] )
CAST: ARM-ACTION ( n -- [ ^u8 n n n n -- ] )
CAST: CTOR-DECL-ACTION ( n -- [ n ^u8 n n n n -- ] )
CAST: USES-ACTION ( n -- [ [ n n n n n n -- ] [ -- renders parses nominal ] -- renders parses nominal ] )

\ Each file the composition scans is a visit, numbered from one counter that
\ never goes back: no number names two files in one process, so a declaration
\ an earlier composition located names no file of this one. A visit keeps a
\ copy of the path its file was resolved to, because the loader releases its
\ own when the file ends while a later use may bind a declaration in a file
\ whose visit has ended. The composition's first visit is the subject's. The
\ copies go when the composition ends.
variable VISIT-NEXT                     \ the last visit number given
variable VISIT-CUR                      \ the file being scanned, 0 outside a visit
variable VISIT-FIRST
variable VISIT-N
DYNAMIC-BUFFER VISIT-PATHS u8           \ their paths, end to end
variable VISIT-PATHS-U
DYNAMIC-BUFFER VISIT-ROWS n             \ per visit, its path's offset and length

: VISIT-OPEN ( str -- )
   {: path:^ pathu:n :}
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

: VISIT-IN? ( n -- bool )
   VISIT-FIRST @ - {: row:n :}
   row 0 >= row VISIT-N @ < and ;

\ The path visit V's file was resolved to, for a visit this composition made.
: VISIT-PATH ( n -- str )
   VISIT-FIRST @ - 2 * {: row:n :}
   row VISIT-ROWS @ VISIT-PATHS  row 1 + VISIT-ROWS @ ;

: VISITS-DROP ( -- )
   VISIT-PATHS-RELEASE  VISIT-ROWS-RELEASE
   0 VISIT-PATHS-U !  0 VISIT-N !  0 VISIT-CUR ! ;

\ Arm the checker with NAME, declared by the token from byte AT, U long, in the
\ file being scanned: the record the next named registrar retains takes that
\ spelling and location, until DISARM. Outside a visit there is no file to
\ name, and the checker's armed state stays its caller's.
: ARM-UNSEALED ( str n n -- )
   {: name:^ nameu:n at:n u:n :}
   VISIT-CUR @ 0= IF EXIT THEN
   name nameu VISIT-CUR @ at at u +
   NCOMP-DISPATCH:DECL-VERIFY-DECL-ARM-OFF OWNER-XT ARM-ACTION execute ;

: ARM ( ^u8 n n n -- )
   {: name:^ nameu:n at:n u:n :}
   name nameu at u SEALED-NAME
   name nameu at u ARM-UNSEALED ;

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
defer ON-CANDIDATE ( ^u8 n ^u8 n n n -- )

private

: CANDIDATE-NONE ( str str n n -- )
   2drop 2drop 2drop ;

: CANDIDATE-INIT ( -- )
   ['] CANDIDATE-NONE is ON-CANDIDATE ;

CANDIDATE-INIT

CAST: CURSOR-ACTION ( n -- [ n [ ^u8 n n n n -- ] -- ] )
CAST: VISIBLE-ACTION ( n -- [ ^u8 n n [ ^u8 n n n n -- ] -- ] )

\ The checker's candidate: its spelling and the declaration's visit and span,
\ 0 0 0 for none.
: CSR-VISIT ( str n n n -- )
   {: a:^ u:n v:n s:n e:n :}
   v VISIT-IN? IF a u v VISIT-PATH s e ON-CANDIDATE EXIT THEN
   a u NULL$ 0 0 ON-CANDIDATE ;

\ One arm per composition, so a does> parent checked twice offers once.
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

\ Run Q with H receiving each use Q's checks bind: one scope at a time, closed
\ on either exit.
: WITH-USES ( [ n n n n n n -- ] [ -- renders parses nominal ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-USES-OFF OWNER-XT USES-ACTION execute ;

\ The scope's two questions about a name, asked of the checker that owns the
\ scope: RECORD-SYM? names the symbol a definition in THIS source is recorded
\ under (0 when it never was) and FIND-SYM resolves a USE through the open
\ package's private and public wordlists, the global wordlist and the used
\ publics - the same chain a body token resolves through.
: RECORD-SYM? ( str -- n )
   NCOMP-DISPATCH:DECL-VERIFY-RECORD-SYM-OFF OWNER-XT SYM-ACTION execute ;
\ FIND-SYM reports nothing: this scan asks it of tokens it is only
\ classifying, and the two rejections the authoritative resolver owns (a used
\ public shadowing a global, a tail two used packages both export) belong to the
\ definition's own check, which resolves the same token straight after.
: FIND-SYM ( str -- n )
   NCOMP-DISPATCH:DECL-VERIFY-FIND-SYM-OFF OWNER-XT SYM-ACTION execute ;

public

\ The selected symbol's package, tail and visibility are borrowed from its
\ checker owner until that owner next interns a symbol.
: SYM-IDENTITY ( n -- str str n )
   NCOMP-DISPATCH:DECL-VERIFY-SYM-IDENTITY-OFF OWNER-XT IDENTITY-ACTION execute ;

private

\ The word a symbol is: for an export, the symbol of the word it names, through
\ any re-exports, or the export itself once that word is undefined; otherwise
\ the symbol itself. A source the capture swept, such as a private word of a
\ sealed package, keeps its symbol with an empty SYM-IDENTITY: no word.
: SYM-SOURCE ( n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-SYM-SOURCE-OFF OWNER-XT SOURCE-ACTION execute ;

\ The same two questions about a definer this pre-pass never read - one compiled
\ in THIS process, whose clause the checker certified at its `;` and whose
\ created-word effect it kept, or a straight-line wrapper of such a definer,
\ which the checker's own body walk records against the wrapper's symbol the
\ same way. The effect stays where the checker built it: it is handed over as
\ a record, not as text, so the type variables the clause declared keep the
\ raw-definer seal the engine's own `trust-raw` gives them.
: CREATES-SYM? ( n -- n )
   NCOMP-DISPATCH:DECL-VERIFY-CREATES-SYM-OFF OWNER-XT CREATES-ACTION execute ;
: RECORD-CREATED ( str n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RECORD-CREATED-OFF OWNER-XT CREATED-ACTION execute ;

\ Does the token run a word that may define words no source text spells, by one
\ of the facts MASK names? When it does, the checker marks the wordlist the
\ statement runs in, and a definition or a top-level token read after it that
\ names a word nothing in scope defines is left to the run (CHECK's verdict 2,
\ or a stretch deferred at the token) instead of refused E-UNDEFINED.
: RENDERS-MARK? ( str n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RENDERS-OFF OWNER-XT RENDERS-ACTION execute ;

CTL-RENDERS CTL-CREATES or constant MARK-UNSEEN
CTL-RENDERS CTL-NOMINAL or constant MARK-NOMINAL

\ Only a reached call whose outputs this reader does not model can introduce
\ nominal types absent from the source. A modeled declaration checks its own
\ complete type before it publishes anything.
: NOMINAL-MARK ( str -- )
   {: a:^ u:n :}
   a u num-parse nip nip IF EXIT THEN
   a u STRING-OPENER? IF EXIT THEN
   a u MARK-NOMINAL RENDERS-MARK? drop ;

\ A call in the body of the TRUSTED: word NAME: the checker adds what the word
\ the token names may do when it runs to what NAME may do.
: TRUSTED-REACH ( str str -- )
   NCOMP-DISPATCH:DECL-VERIFY-REACH-OFF OWNER-XT REACH-ACTION execute ;

\ The does> clause record of the TRUSTED: definer NAME, published as the
\ engine's `does>` makes it.
: TRUSTED-CLAUSE ( str -- )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-CLAUSE-OFF OWNER-XT CLAUSE-ACTION execute ;

DYNAMIC-BUFFER DEFINER-SYM n
DYNAMIC-BUFFER DEFINER-LEN n               \ its effect's length, 0 for a retired row
DYNAMIC-BUFFER DEFINER-AT n
DYNAMIC-BUFFER DEFINER-GEN bool            \ its own `generates:` row states it
DYNAMIC-BUFFER DEFINER-SIG u8

: DEFINER-SIG@ ( n -- str ) {: row:n :}
   row DEFINER-AT @ DEFINER-SIG  row DEFINER-LEN @ ;

: DEFINER-SIG-END ( -- n )
   0 VERIFY-DEFINER-N @
   BEGIN dup 0 > WHILE
      1 -
      dup DEFINER-AT @ over DEFINER-LEN @ +  rot max swap
   REPEAT drop ;

: DEFINER-FIND ( n -- n ) {: sym:n :}         \ sym's row + 1, 0 = no such definer
   sym 0= IF 0 EXIT THEN
   VERIFY-DEFINER-N @ BEGIN dup 0 > WHILE
      1 -
      dup DEFINER-SYM @ sym = IF
         dup DEFINER-LEN @ 0= IF drop 0 EXIT THEN
         1 + EXIT
      THEN
   REPEAT ;

: DEFINER-APPEND ( n -- n )
   {: sym:n :}
   VERIFY-DEFINER-N @
   {: row:n :}
   row 1 + DEFINER-SYM-RESERVE  row 1 + DEFINER-LEN-RESERVE  row 1 + DEFINER-AT-RESERVE
   row 1 + DEFINER-GEN-RESERVE
   sym row DEFINER-SYM !  0 row DEFINER-LEN !  0 row DEFINER-AT !  false row DEFINER-GEN !
   row 1 + VERIFY-DEFINER-N !
   row ;

: DEFINER-ROW ( n -- n ) {: sym:n :}
   sym DEFINER-FIND dup 0<> IF 1 - EXIT THEN drop
   sym DEFINER-APPEND ;

: DEFINER-ROOM ( n -- n )
   {: u:n :}
   DEFINER-SIG-END
   {: at:n :}
   at u + DEFINER-SIG-RESERVE
   at ;

\ A name with a live row keeps that row and takes the newer effect, which is
\ what the run time does: a replacement clause replaces the old created-word
\ effect. An empty effect holds no byte: its row's length is 0, which
\ DEFINER-FIND reads as retired, and no place in DEFINER-SIG, which may hold no
\ byte yet, is formed for it.
: DEFINER-ADD ( str bool n -- ) {: sig:^ sigu:n gen:bool sym:n :}
   sym 0= IF EXIT THEN                        \ never recorded: nothing to hang it on
   sym DEFINER-ROW
   {: row:n :}
   gen row DEFINER-GEN !
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

\ The row of a token's definer + 1, 0 when it is no learned definer.
: DEFINER-OF ( str -- n ) {: a:^ u:n :}
   VERIFY-DEFINER-N @ 0= IF 0 EXIT THEN
   a u FIND-SYM DEFINER-FIND ;

\ The effect a token's definer gives the word it creates, answered as a string
\ whose ZERO LENGTH means "not a learned definer".
: DEFINER-EFFECT ( str -- str )
   DEFINER-OF dup 0= IF drop SOURCE@ 0 EXIT THEN
   1 - DEFINER-SIG@ ;

variable DEFER-REPORT                            \ the child reports deferrals
\ The last run reached uncertain source. Only DEFER-AT and REPORT-DEFERRED set
\ it, and each writes the packet that locates it when the child asks for one.
variable DEFER-SEEN

\ WHY is the deferral's cause, which picks its warning's message and help; 0
\ when the site cannot tell.
CAST: STRETCH-ACTION ( n -- [ ^u8 n -- ] )
: REPORT-STRETCH ( str n -- )
   CHECKER-DEFER-WHY!
   NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-OFF OWNER-XT STRETCH-ACTION execute ;

: DEFER-AT ( str n -- )
   {: a:^ u:n why:n :}
   FFI:SKIP-FUNCTION
   -1 DEFER-SEEN !
   DEFER-REPORT @ IF a u why REPORT-STRETCH THEN ;

: QUIET-TYPE-CLEAR ( -- )
   CHECKER-SIG-UNRES-TAKE drop 2drop ;
: QUIET-TYPE-REPORT ( -- )
   CHECKER-SIG-UNRES-TAKE {: a:^ u:n found:bool :}
   found IF a u DW-TYPE DEFER-AT THEN ;

\ A body the checker deferred to the run (verdict 2) is reported where the
\ checker's judgment of it stopped, when the child reports deferrals, as a
\ deferred stretch is at the token that opened it.
CAST: DEFERRED-BODY-ACTION ( n -- [ -- ^u8 n ] )
: DEFERRED-BODY$ ( -- str )
   NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-BODY-OFF OWNER-XT DEFERRED-BODY-ACTION execute ;
: REPORT-DEFERRED ( n -- )
   2 <> IF EXIT THEN
   -1 DEFER-SEEN !
   DEFER-REPORT @ IF DEFERRED-BODY$ 0 REPORT-STRETCH THEN ;

\ A body's verdict as this scan acts on it: -1 certified, 2 deferred to the run,
\ 0 rejected. The check wrote the record of a rejected (0) or uncheckable (1)
\ body; under MULTI-ERR? the scan answers 0 and goes on, so the body records no
\ created effect, else it stops with the record's status.
: BODY-VERDICT ( n -- n ) {: v:n :}
   v -1 = v 2 = or IF v EXIT THEN
   MULTI-ERR? IF 0 EXIT THEN
   EXIT-STATUS:REJECTED throw ;

\ The colon definitions this run judged, each counted once its verdict is
\ final (CENSUS): a `:` or `kernel:` statement (COLON?) is certified when its
\ body, and its does> clause if it has one, certified; it is deferred to the
\ run when neither was refused and one was deferred, or when the scan stopped
\ inside it (TICK-REMAINDER). A refused one counts in neither, and the scan
\ reads nothing after a stop.
variable CERTIFIED-N
variable DEFERRED-N

: TALLY ( n -- )
   dup -1 = IF drop 1 CERTIFIED-N +! EXIT THEN
   2 = IF 1 DEFERRED-N +! THEN ;

PTR-VARIABLE TICK-BODY-A
variable TICK-BODY-U
variable TICK-BODY-VERDICT

: TICK-BODY-CHECK ( -- )
   TICK-BODY-A @ TICK-BODY-U @ CHECK! TICK-BODY-VERDICT ! ;

: TICK-BODY-RUN ( -- )
   TICK-BODY-U @ ['] TICK-BODY-CHECK CSR-CHECK ;

: VERIFY-BODY ( -- n )
   DEF-BODY$ {: ba:^ bu:n :}
   ba TICK-BODY-A !  bu TICK-BODY-U !
   ba bu [: TICK-BODY-RUN ;] WITH-BODY-TICK-ORDER
   IF true TICK-REMAINDER ! 2 ELSE TICK-BODY-VERDICT @ THEN BODY-VERDICT ;

\ The pre-pass's own does>-clause entry point. It is not the engine's
\ CHECK-DOES!: this scan reaches a clause AFTER the definer's own body has been
\ checked and recorded, so the checker must not latch the created effect here -
\ the next record belongs to the next definition. What this scan learns about a
\ definer it READ goes into the table above instead. It takes the definer's
\ name, which names the clause in the diagnostic of a refused one.
: CHECK-DOES-BODY ( str str str -- n )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-DOES-OFF OWNER-XT DOES-ACTION execute ;

PTR-VARIABLE TICK-DOES-SA
variable TICK-DOES-SU
PTR-VARIABLE TICK-DOES-NA
variable TICK-DOES-NU
variable TICK-DOES-VERDICT

\ The native compiler can refuse an earlier parent token before it reaches a
\ does> clause, then checks the clause before the parent's closing check.
\ Keep the parent's collected text and source map while judging the clause.
DYNAMIC-BUFFER DOES-PARENT-BUF u8
DYNAMIC-BUFFER DOES-PARENT-ROW n
variable DOES-PARENT-U
variable DOES-PARENT-ROWS
PTR-VARIABLE DOES-CLOSER-A
variable DOES-CLOSER-U
variable DOES-DEF-VERDICT
variable DOES-ACCEPTED                  \ both parent and clause certified
TYPED-VARIABLE DOES-PARENT-REFUSED bool
PTR-VARIABLE DOES-CLAUSE-A
variable DOES-CLAUSE-U

\ The rows move cell by cell as the cells they are.
: DOES-PARENT-ROOM ( n n -- ) {: u:n c:n :}
   u 1 max BODY-BUF-RESERVE  u 1 max DOES-PARENT-BUF-RESERVE
   c 2 * 1 max BODY-ROW-RESERVE  c 2 * 1 max DOES-PARENT-ROW-RESERVE ;

: DOES-PARENT-KEEP ( -- )
   BODY-U @ DOES-PARENT-U !  BODY-ROWS @ DOES-PARENT-ROWS !
   DOES-PARENT-U @ DOES-PARENT-ROWS @ DOES-PARENT-ROOM
   0 BODY-BUF 0 DOES-PARENT-BUF DOES-PARENT-U @ BYTE-COPY
   DOES-PARENT-ROWS @ 2 * 0 ?do i BODY-ROW @ i DOES-PARENT-ROW ! loop ;

: DOES-PARENT-BACK ( -- )
   DOES-PARENT-U @ DOES-PARENT-ROWS @ DOES-PARENT-ROOM
   0 DOES-PARENT-BUF 0 BODY-BUF DOES-PARENT-U @ BYTE-COPY
   DOES-PARENT-ROWS @ 2 * 0 ?do i DOES-PARENT-ROW @ i BODY-ROW ! loop
   DOES-PARENT-U @ BODY-U !  DOES-PARENT-ROWS @ BODY-ROWS ! ;

: REPORT-DOES-CLAUSE ( -- )
   DOES-CLAUSE-U @ 0= IF EXIT THEN
   DOES-CLAUSE-A @ DOES-CLAUSE-U @ 0 DEFER-AT ;

: TICK-DOES-CHECK ( -- )
   TICK-BODY-A @ TICK-BODY-U @
   TICK-DOES-SA @ TICK-DOES-SU @ TICK-DOES-NA @ TICK-DOES-NU @
   CHECK-DOES-BODY TICK-DOES-VERDICT ! ;

: TICK-DOES-RUN ( -- )
   TICK-BODY-U @ ['] TICK-DOES-CHECK CSR-CHECK ;

: VERIFY-DOES-BODY ( str str -- n ) {: sig:^ sigu:n na:^ nu:n :}
   DEF-BODY$ {: ba:^ bu:n :}
   ba TICK-BODY-A !  bu TICK-BODY-U !
   sig TICK-DOES-SA !  sigu TICK-DOES-SU !
   na TICK-DOES-NA !  nu TICK-DOES-NU !
   ba bu [: TICK-DOES-RUN ;] WITH-BODY-TICK-ORDER
   IF true TICK-REMAINDER ! 2 ELSE TICK-DOES-VERDICT @ THEN BODY-VERDICT ;

\ The definition's own name, pinned by VERIFY-DEFINITION before its body is
\ scanned: a created effect is recorded on the definition's own entry, so the
\ recorders below ask the scope for that entry once the body has certified.
\ RECORD-EXPORT pins the name it re-exports here for the quotation it catches.
PTR-VARIABLE DEF-NAME-A
variable DEF-NAME-U
variable DEF-NAME-BYTE
PTR-VARIABLE DEF-SIG-A                        \ its signature, inside the parens
variable DEF-SIG-U
variable WRAP-DEFINERS                        \ definer calls in this body …
variable WRAP-CTL                             \ … and whether the line ever bent
variable WRAP-ROW                             \ the definer row the last call names
variable WRAP-MAKES                           \ a `create` or definer call before any bend

: DEF-NAME! ( -- )
   TOKEN-U @ DEF-NAME-U !  TOKEN-A @ DEF-NAME-A !  TOKEN-BYTE @ DEF-NAME-BYTE ! ;

\ The body of the definition DEF-NAME! pinned, checked with its name armed;
\ VERIFY-DEFINITION asked the seal at the name.
: VERIFY-NAMED-BODY ( -- n )
   DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ DEF-NAME-U @ ARM-UNSEALED
   VERIFY-BODY
   DISARM ;

: DOES-PARENT-RUN ( -- )
   VERIFY-NAMED-BODY DOES-DEF-VERDICT ! ;

: WRAP-RESET ( -- )
   0 WRAP-DEFINERS !  0 WRAP-CTL !  0 WRAP-ROW !  0 WRAP-MAKES ! ;

: DEFINER-RECORD-AS ( str str -- ) {: sig:^ sigu:n na:^ nu:n :}
   sig sigu false na nu RECORD-SYM? DEFINER-ADD ;

: DEFINER-RECORD ( str -- )
   DEF-NAME-A @ DEF-NAME-U @ DEFINER-RECORD-AS ;

\ A definer this pre-pass read is in its table; one compiled in the checking
\ process the checker answers.
: WRAP-TOKEN ( str -- ) {: a:^ u:n :}
   WRAP-CTL @ IF EXIT THEN
   a u WRAP-CTL-TOK? IF -1 WRAP-CTL ! EXIT THEN
   a u s" create" STR=CI IF -1 WRAP-MAKES ! EXIT THEN
   a u DEFINER-OF dup 0<> IF
      1 - WRAP-ROW !
      WRAP-DEFINERS @ 1 + WRAP-DEFINERS !  -1 WRAP-MAKES !  EXIT
   THEN
   drop
   WRAP-MAKES @ IF EXIT THEN
   a u FIND-SYM CREATES-SYM? 0<> IF -1 WRAP-MAKES ! THEN ;

\ A head that reaches `does>` with no `create` or definer call before any bend
\ is rejected at its `does>`, the token t tu, as the load's checker rejects it.
: DOES-UNMADE-STOP ( str -- ) {: t:^ tu:n :}
   t tu DOES-UNMADE-OPEN
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN ;

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
: QUAL-SPELLING ( str str -- str )
   {: pkg:^ pkgu:n tail:^ tailu:n :}
   pkgu tailu + 1+ TICK-QUAL-RESERVE
   pkg 0 TICK-QUAL pkgu BYTE-COPY
   $3A 0 TICK-QUAL pkgu + c!
   tail 0 TICK-QUAL pkgu 1+ + tailu BYTE-COPY
   0 TICK-QUAL pkgu tailu + 1+ ;

: TICK-RESIDENT-IMM? ( str -- bool )
   {: a:^ u:n :}
   a u tok-imm? 0<> IF true EXIT THEN
   a u FIND-SYM dup 0= IF drop false EXIT THEN
   SYM-IDENTITY {: pkg:^ pkgu:n tail:^ tailu:n vis:n :}
   vis 1 = IF true EXIT THEN
   pkgu 0= IF false EXIT THEN
   pkg pkgu tail tailu QUAL-SPELLING tok-imm? 0<> ;

\ A compile-time action inside the current definition can change the compiler or
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

\ The created effect is the clause's declaration, recorded unless the definer's
\ body or its clause was refused. One deferred to the run keeps it, as a
\ deferred colon definition keeps its declared signature, and the run judges
\ the deferred text. The definition reports one deferral: its clause's only
\ when the definer's body was not deferred.
: VERIFY-DOES ( -- bool )
   TOKEN-A @ TOKEN-U @ {: opener:^ openeru:n :}
   false DOES-ACCEPTED !
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
      DOES-PARENT-KEEP
   ELSE
      VERIFY-NAMED-BODY dup DOES-DEF-VERDICT ! REPORT-DEFERRED
   THEN
   \ The rule is checked once the head's own rejections are: the load's checker
   \ checks it at the end of the head's walk.
   WRAP-MAKES @ 0=  TICK-REMAINDER @ 0=  and IF
      gate unknown or IF DOES-PARENT-REFUSED @ 0= ELSE DOES-DEF-VERDICT @ 0<> THEN IF
         opener openeru DOES-UNMADE-STOP
         true DOES-PARENT-REFUSED !
      THEN
   THEN
   opener openeru SIGNATURE-AT {: sig:^ sigu:n :}
   BODY-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
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
            clause 2 = IF
               DEFERRED-BODY$ DOES-CLAUSE-U ! DOES-CLAUSE-A !
            THEN
            TOKEN-A @ {: end:^ :}  TOKEN-U @ {: endu:n :}
            DOES-PARENT-BACK
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
         DOES-DEF-VERDICT @ -1 =  clause -1 = and DOES-ACCEPTED !
         true EXIT
      THEN
      TICK-BODY-STEP
      APPEND-BODY-TOKEN
   AGAIN ;

\ This scanner is a PRE-PASS: it reads a whole source buffer before any of it is
\ compiled, so a definer it replays names a word that does not exist in this
\ process and cannot be asked to prove otherwise.
\ DECL-SIGNATURE asks the declaration owner's registrar, which registers the row
\ and answers whether it retained it. A name its scope already holds keeps that
\ record either way, so only this answer tells a redeclaration the registrar
\ retained from one it refused.
: DECL-SIGNATURE ( ^u8 n str -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-OFF OWNER-XT DECL-ACTION execute ;

\ The checker's own guard refuses a colon definition, a cast, a typed storage
\ definition and a re-export whose name its scope already holds, but a colon
\ definition only once its body is checked, a storage definition once its type
\ is read, and it keeps no place. Those rows ask the guard's question as soon as
\ they have scanned the name, as the engine's wall and the check hook refuse it
\ under --load, a name only the checker holds included.
\ A definer the checker only trusts (variable, defer, TRUSTED:, ...) is left to
\ the run, which names its duplicate as --load does, or first refuses an
\ earlier definition whose text the engine cannot hold.

\ Whether the path is the subject's, whose bytes are the supplied ones.
: COMPOSE-SUBJ? ( str -- bool )
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ CORE-STR= ;

: COMPOSE-DIAG$ ( str -- str )
   {: path:^ pathu:n :}
   path pathu COMPOSE-SUBJ? IF COMPOSE-SUBJ-LABEL COMPOSE-SUBJ-LABEL-U @ EXIT THEN
   path pathu ;

public

\ What a reported definition is, as the path that declared it knows it: a word
\ that runs code (`:`, `TRUSTED:`, `defer`, a cast, a field, `FUNCTION:`), a
\ constant (`constant`, a structure's size), storage whose word answers its
\ address (`variable`, `create`, a buffer, a word a definer creates when it
\ runs), a re-export, which is what the word it names is, or a generated
\ SUMTYPE constructor whose variant name MATCH also uses.
0 constant DEF-WORD
1 constant DEF-CONSTANT
2 constant DEF-STORAGE
3 constant DEF-EXPORT
4 constant DEF-CONSTRUCTOR

\ A definition the scan retained, once its registrar returned and its name's
\ recording scope holds a live record: its kind, the statement token that
\ declared it (`:`, `constant`, a definer's name); its name as the source
\ writes it, or a name the definer generates as spelled; the symbol the
\ checker recorded it under; its declared effect, empty for none;
\ where the token that declared it starts and ends in the file FILE$
\ names; its class, one of the five above; and its file visit. The strings are
\ borrowed: a word installed here consumes them before it returns.
defer ON-DEFINITION ( str str n str n n n n -- )

\ Per read file, each bound use: span, resolved declaration path (borrowed) and
\ span, symbol, visit; spans at the source's base; a primitive's visit is -2
\ minus its PRIM-SPEC row, its declaration span 0 0 when unplaced; any other
\ unplaced use: "", 0 0, visit -1.
defer ON-USE ( n n str n n n n -- )

\ A top-level loader of the subject whose load reached a file, once the file
\ was selected: where its operand starts and ends in the subject, at the base
\ the subject was given (the token after `include` or `require`, a string
\ loader's literal between its quotes as written); the canonical path; and
\ what became of it. Read: the file's source bytes were acquired for this
\ load, from disk or as the bytes supplied for the subject or for the file
\ (SUPPLIED), whether or not their scan then completes. Held: a `require`,
\ `required` or `script-required` of a path already held, which the engine
\ provides or which was recorded before, so nothing was read. Nothing else is
\ reported: not `provided`, a file that could not be read, a loader in a
\ definition or in a file the subject loads. The path is borrowed: a word
\ installed here consumes it before it returns.
0 constant LOAD-READ
1 constant LOAD-HELD
defer ON-LOADER ( n n str n -- )

\ The file being scanned, named as its packets name it.
: FILE$ ( -- str )
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ COMPOSE-DIAG$ ;

\ Whether the file being scanned is the subject, the composition's first
\ visit: a later read of the subject's path, an include of it, is not.
: SUBJECT? ( -- bool )
   VISIT-CUR @ VISIT-FIRST @ = ;

\ A file the scan starts, named as FILE$ names it, and the bytes the scan reads
\ there, which every position it reports in the file counts in: the bytes
\ supplied for the subject, or for the file (SUPPLIED), else the file's bytes
\ as this composition read them. It runs each time the scan starts a file, the
\ subject first, before anything in it is reported. Both strings are borrowed
\ and read-only: a word installed here consumes them before it returns and
\ writes neither.
defer ON-FILE ( str str -- )

\ The bytes supplied for the file other than the subject at this canonical
\ path, as the loader resolved it, and true: a load of the file scans them in
\ place of its bytes on disk, which it still needs to resolve. Else false, and
\ the load reads the disk. The bytes are borrowed and read-only, and stay so
\ while the composition runs.
defer SUPPLIED ( ^u8 n -- ^u8 n bool )

private

\ A load's place is where the file being scanned writes its path; one entry
\ text makes has none, as the load's command-line text's has none.
: LOAD-PLACE! ( str -- ) {: a:^ u:n :}
   ENTRY-TEXT @ SUBJECT? and IF
      NULL-PTR LOAD-PLACE-A !  0 LOAD-PLACE-U ! EXIT
   THEN
   a LOAD-PLACE-A !  u LOAD-PLACE-U ! ;

\ These declaration arms are dispatched before dictionary lookup. Their scanner
\ counterparts do no arbitrary execution.
\ A word-backed loader or library definer has no such structural guarantee:
\ even the original include/require entry can call replaced source providers.
: TICK-NATIVE-DEFINER? ( str -- bool )
   {: a:^ u:n :}
   a u s" package" STR=CI  a u s" ;package" STR=CI or
   a u s" public" STR=CI or  a u s" private" STR=CI or  a u s" global" STR=CI or
   a u s" using" STR=CI or  a u s" ;using" STR=CI or
   a u s" seal-package" STR=CI or  a u s" host" STR=CI or
   a u s" target" STR=CI or IF 0 0= EXIT THEN
   a u s" export" STR=CI  a u s" trusted:" STR=CI or
   a u s" cast:" STR=CI or  a u s" linear:" STR=CI or
   a u s" defer" STR=CI or  a u s" create" STR=CI or
   a u s" variable" STR=CI or  a u s" constant" STR=CI or ;

: DEFINITION-NONE ( str str n str n n n n -- )
   2drop 2drop 2drop drop 2drop 2drop ;

: DEFINITION-INIT ( -- )
   ['] DEFINITION-NONE is ON-DEFINITION ;

DEFINITION-INIT

: FILE-NONE ( str str -- )  2drop 2drop ;

: FILE-INIT ( -- )
   ['] FILE-NONE is ON-FILE ;

FILE-INIT

: SUPPLIED-NONE ( str -- str bool )  2drop NULL$ false ;

: SUPPLIED-INIT ( -- )
   ['] SUPPLIED-NONE is SUPPLIED ;

SUPPLIED-INIT

: USE-NONE ( n n str n n n n -- )
   2drop 2drop 2drop 2drop ;

: USE-INIT ( -- )
   ['] USE-NONE is ON-USE ;

USE-INIT

: LOADER-NONE ( n n str n -- )
   2drop 2drop drop ;

: LOADER-INIT ( -- )
   ['] LOADER-NONE is ON-LOADER ;

LOADER-INIT

variable PRIM-OPEN-READY
defer ENGINE-TARGET ( n -- str n n bool )

: ENGINE-TARGET-NONE ( n -- str n n bool )
   drop NULL$ 0 0 false ;

: ENGINE-TARGET-INIT ( -- ) ['] ENGINE-TARGET-NONE is ENGINE-TARGET ;
ENGINE-TARGET-INIT

: PRIM-TARGET ( n -- str n n bool )
   {: row:n :}
   PRIM-SOURCE:ENGINE-PATH$ {: path:^ pathu:n ready:bool :}
   ready 0= if NULL$ 0 0 false exit then
   path pathu SUPPLIED {: a:^ u:n open:bool :}
   open if
      PRIM-OPEN-READY @ 0= if
         1 PRIM-OPEN-READY !
         a u row PRIM-SOURCE:TARGET-IN
      else
         row PRIM-SOURCE:TARGET-ROW
      then
      {: at:n end:n found:bool :}
      path pathu at end found exit
   then
   row PRIM-SOURCE:TARGET ;

\ The checker replays a colon's header while checking its body. Its name can
\ still bind an older word there, but that token declares the new word; it is
\ not a use of the older one.
: COLON-NAME-USE? ( n n -- bool )
   {: s:n e:n :}
   TOP-CUR-A @ TOP-CUR-U @ s" :" STR=CI
   TOP-CUR-A @ TOP-CUR-U @ s" kernel:" STR=CI or
   s DEF-NAME-BYTE @ = and
   e DEF-NAME-BYTE @ DEF-NAME-U @ + = and ;

: USE-SEEN ( n n n n n n -- )
   {: s:n e:n sym:n v:n ds:n de:n :}
   s e COLON-NAME-USE? IF EXIT THEN
   v -2 <= IF
      -2 v - PRIM-TARGET drop {: path:^ pathu:n at:n end:n :}
      s e
      path pathu at end sym v ON-USE
      EXIT
   THEN
   v 0= IF
      sym ENGINE-TARGET {: path:^ pathu:n at:n end:n found:bool :}
      found IF
         s e
         path pathu at end sym -1 ON-USE
         EXIT
      THEN
   THEN
   v VISIT-IN? IF
      s e v VISIT-PATH  ds de sym v ON-USE
   ELSE
      s e NULL$ 0 0 sym -1 ON-USE
   THEN ;

\ The record of a duplicate named a u, placed at the source token t tu. Under
\ MULTI-ERR? it is kept and the scan skips the definition and goes on;
\ otherwise it stops the scan as the duplicate stops the load.
: DUPLICATE-AT! ( str str -- )
   DUPLICATE-RECORD
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN ;

\ The record of a duplicate whose name, u bytes long, starts at TOKEN-BYTE in
\ the scanned source, or of the statement's word there when the definer
\ generates the name (u = 0).
: DUPLICATE! ( n -- )
   {: u:n :}
   u 0= IF TOKEN-BYTE @ TOKEN-FROM ELSE SOURCE@ TOKEN-BYTE @ + u THEN
   2dup DUPLICATE-AT! ;

\ Whether a u, written from byte AT, is a keyword or number name; the record
\ ends as a duplicate's.
: NAME-REJECT? ( str n -- bool )
   {: a:^ u:n at:n :}
   a u SOURCE@ at + u DEF-NAME-REJECT? 0= IF false EXIT THEN
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN
   true ;

\ A keyword or number name stops first, then one that lands in a sealed package.
: REJECT-DUPLICATE ( str n -- bool )
   {: a:^ u:n at:n :}
   a u at NAME-REJECT? IF true EXIT THEN
   a u at u SEALED-NAME
   a u CHECKER-CERT-DUP? dup IF u DUPLICATE! THEN ;

\ Whether the scan refused the definition NAME, written from byte AT, now that
\ it knows whether the definition makes a does> clause (CLAUSE): a colon
\ definition at the `;` or `does>` that ends its body, before the body is
\ checked, and a TRUSTED: one once its body is read, before its clause is
\ published. A record of the name from before the replay that has the other
\ shape is no loaded twin of it, and the live engine refuses the definition
\ there.
: REFUSE-SHAPE ( str n bool -- bool )
   {: a:^ u:n at:n clause:bool :}
   a u clause CHECKER-CERT-SHAPE-DUP? 0= IF false EXIT THEN
   at TOKEN-BYTE !
   u DUPLICATE!
   true ;

: PUBLIC-WID ( str -- n )
   BOOT:NAMESPACE-WL XREF-FIND-WL {: row:^ :}
   row XREF-FOUND? 0= IF -1 EXIT THEN
   row XREF-PKG-PUBLIC ;

: ROW-SEALED? ( str -- bool )
   BOOT:NAMESPACE-WL XREF-FIND-WL {: row:^ :}
   row XREF-FOUND? 0= IF false EXIT THEN
   row XREF:SEALED? ;

\ The package path a u, named NAME as written at byte AT.
: SEALED-PACKAGE ( str str n -- )
   {: a:^ u:n name:^ nameu:n at:n :}
   a u SOURCE-SEALED? IF name nameu at nameu SEAL-STOP THEN
   a u ROW-SEALED? IF name nameu at nameu SEAL-STOP THEN
   SEALED-LOAD? 0= IF EXIT THEN
   a u PUBLIC-WID XREF-WID-PROTECTED? IF name nameu at nameu SEAL-STOP THEN ;

\ Where a guard refused a live record before the registrar ran, one there now
\ is the record that registrar retained. A declaration DECL-SIGNATURE registers
\ has no guard: the record found for its name is that declaration's only when
\ DECL-SIGNATURE answered true, so its callers ask only then.
: RETAINED-SYM ( str -- n )
   {: name:^ nameu:n :}
   name nameu CHECKER-CERT-DUP? 0= IF 0 EXIT THEN
   name nameu RECORD-SYM? ;

: DEFINED ( ^u8 n ^u8 n ^u8 n n n n -- )
   {: kind:^ kindu:n name:^ nameu:n eff:^ effu:n at:n end:n class:n :}
   name nameu RETAINED-SYM
   {: sym:n :}
   sym NEWEST-SYM !
   sym 0= IF EXIT THEN
   kind kindu name nameu sym eff effu TRIM at end class VISIT-CUR @ ON-DEFINITION ;

: DEFINED-HERE ( str str n n -- )
   {: name:^ nameu:n eff:^ effu:n at:n class:n :}
   TOP-CUR-A @ TOP-CUR-U @ name nameu eff effu at at nameu + class DEFINED ;

\ The effect of NAME's retained newest record, or empty when there is none or
\ TEXT cannot say it; it lies past TEXT-LEN, where the next TEXT write lands.
: MADE-EFFECT ( str -- str )
   RETAINED-SYM {: sym:n :}
   sym 0= IF s" " EXIT THEN
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= IF s" " EXIT THEN
   REJECTION:TEXT-LEN @ RQM @ REJECTION:DEST @ {: base:n qm:n dst:n :}
   rec1 1 - REND-RECORD-EFFECT {: a:^ u:n :}
   RQM @ {: unsaid:n :}
   base REJECTION:TEXT-LEN !  qm RQM !  dst REJECTION:DEST !
   unsaid 0<> IF s" " EXIT THEN
   a base +  u base - ;

\ DEFINED-HERE for a word a definer made, with the effect its record carries.
: DEFINED-MADE ( str n n -- )
   {: name:^ nameu:n at:n class:n :}
   name nameu  name nameu MADE-EFFECT  at class DEFINED-HERE ;

: SIG-RAW-MODE! ( n -- ) SIG-RAW-MODE ! ;

\ The keyword the engine's rejection names for the storage statement's name:
\ `constant` and `variable` read their own, every other definer's reaches
\ `create`.
: STORAGE-KEYWORD ( -- str )
   TOP-CUR-A @ TOP-CUR-U @ {: a:^ u:n :}
   a u s" constant" STR=CI IF s" constant" EXIT THEN
   a u s" variable" STR=CI IF s" variable" EXIT THEN
   s" create" ;

\ Registers the effect for the word the next token names with TVK-RAW type
\ vars, so its cell cannot launder into a nominal atom or family.
: RAW-TRUST-NEXT ( str n -- )
   {: sig:^ sigu:n class:n :}
   NAME-TOKEN
   dup 0= IF NAME-STOP THEN
   TOKEN-BYTE @
   {: name:^ nameu:n at:n :}
   STORAGE-KEYWORD name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   name nameu at REJECT-DUPLICATE IF EXIT THEN
   name nameu at nameu ARM
   -1 SIG-RAW-MODE!
   name nameu sig sigu [: DECL-SIGNATURE ;] [: 0 SIG-RAW-MODE! DISARM ;] finally
   IF name nameu sig sigu at class DEFINED-HERE THEN ;

\ DSYM is the definer's symbol, which its caller tested before the name is
\ taken: NAME-TOKEN consumes a token, and a token that is not a definer must
\ leave the scan exactly where it was.
: CREATED-TRUST-NEXT? ( n -- bool )
   {: dsym:n :}
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   STORAGE-KEYWORD name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name nameu at REJECT-DUPLICATE IF 0 0= EXIT THEN
   name nameu at nameu ARM
   name nameu dsym RECORD-CREATED
   DISARM
   0= IF 0 0= 0= EXIT THEN
   name nameu at DEF-STORAGE DEFINED-MADE
   0 0= ;

: TRUST-DEFER-SIGNATURE ( str n -- )
   {: name:^ nameu:n at:n :}
   name nameu SIGNATURE-AT {: sig:^ sigu:n :}
   name nameu at REJECT-DUPLICATE IF EXIT THEN
   name nameu at nameu ARM
   -1 SIG-STATES-FACTS !
   name nameu sig sigu [: DECL-SIGNATURE ;] [: 0 SIG-STATES-FACTS ! ;] finally
   {: kept:bool :}
   kept IF name nameu CHECKER-DEFER THEN
   DISARM
   kept IF name nameu sig sigu at DEF-WORD DEFINED-HERE
   ELSE QUIET-TYPE-REPORT THEN ;

: TRUST-DEFER ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   name nameu TOKEN-BYTE @ TRUST-DEFER-SIGNATURE ;

variable TRUSTED-DOES

\ A token of the trusted body NAME before its `does>`, asked of the checker
\ when it may call a word.
: TRUSTED-CALL ( str -- ) {: na:^ nu:n :}
   TRUSTED-DOES @ LOCAL-TOKEN? or IF EXIT THEN
   TOKEN-A @ TOKEN-U @ {: a:^ u:n :}
   a u s" {:" CORE-STR=  a u BODY-PARSER? or  a u STRING-OPENER? or IF EXIT THEN
   a u num-parse nip nip IF EXIT THEN
   a u WRAP-CTL-TOK? IF EXIT THEN
   a u s" i" STR=CI  a u s" j" STR=CI or  a u s" unloop" STR=CI or IF EXIT THEN
   a u s" is" STR=CI IF OPERAND 2drop EXIT THEN
   a u s" [package]" STR=CI IF EXIT THEN
   a u na nu TRUSTED-REACH ;

\ A trusted body is ASSERTED, never verified, but its `does>` clause is a
\ DECLARATION. So this scan learns the definer from the clause signature, and
\ checks no token of the body. A `does>` inside a string is no clause. What
\ running the word may do is still what its calls may do, and the tokens before
\ its `does>` run when it does.
: SCAN-TRUSTED-BODY ( str bool -- ) {: na:^ nu:n kept:bool :}
   BODY-PREV-CLEAR
   LOCALS-RESET
   0 TRUSTED-DOES !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      LOCAL-TOKEN? 0=  TOKEN-A @ TOKEN-U @ s" does>" STR=CI  and IF
         \ This scan skips the clause body, so its acceptance is unknown.
         INDEX-ON @ IF
            TOKEN-A @ TOKEN-U @ 0 DEFER-AT
            true TICK-REMAINDER ! EXIT
         THEN
         TOKEN-A @ TOKEN-U @ SIGNATURE-AT
         kept IF na nu DEFINER-RECORD-AS ELSE 2drop THEN
         -1 TRUSTED-DOES !
      ELSE
         kept IF na nu TRUSTED-CALL THEN
         SKIP-DEF-TOKEN
      THEN
   AGAIN ;

\ The engine's `does>` makes the clause record `<definer>;does` for a TRUSTED:
\ definer as for a checked one, so a definer whose `does>` the scan read gets it
\ at its `;`: a later definition of that name is refused, as in the live load.
: TRUSTED-DEFINITION ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   s" trusted:" name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name nameu at REJECT-DUPLICATE IF
      name nameu SIGNATURE-AT 2drop  name nameu false SCAN-TRUSTED-BODY EXIT
   THEN
   name nameu SIGNATURE-AT {: sig:^ sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DECL-SIGNATURE
   {: kept:bool :}
   DISARM
   kept 0= IF QUIET-TYPE-REPORT THEN
   name nameu kept SCAN-TRUSTED-BODY
   TICK-REMAINDER @ IF EXIT THEN
   name nameu at TRUSTED-DOES @ 0<> REFUSE-SHAPE IF EXIT THEN
   TRUSTED-DOES @ kept and IF name nameu TRUSTED-CLAUSE THEN
   kept IF name nameu sig sigu at DEF-WORD DEFINED-HERE THEN ;

\ Registration goes through the certifying registrar, not DECL-SIGNATURE, so
\ an illegal retype is refused here too and not merely recorded.
: CAST-DECLARATION ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name nameu at REJECT-DUPLICATE IF name nameu SIGNATURE-AT 2drop EXIT THEN
   name nameu SIGNATURE-AT {: sig:^ sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu CHECKER-DEFCAST
   DISARM
   name nameu sig sigu at DEF-WORD DEFINED-HERE ;

\ A `linear:` row is certified by the engine's own registrar, so a mint or
\ erase this pre-pass accepts is exactly one the engine accepts.
: LINEAR-DECLARATION ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   s" linear:" name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name nameu at NAME-REJECT? IF name nameu SIGNATURE-AT 2drop EXIT THEN
   name nameu SIGNATURE-AT {: sig:^ sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu CHECKER-LINEAR
   DISARM
   name nameu sig sigu at DEF-WORD DEFINED-HERE ;

: IDENTITY-OUTCOME ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   DISARM
   rc E-CHECKER-IDENTITY-UNRESOLVED = IF QUIET-TYPE-REPORT EXIT THEN
   MULTI-ERR? IF rc EXIT-STATUS:REJECTED <> IF rc throw THEN EXIT THEN
   rc throw ;

\ A name only rendered text may define is left to the run
\ to retire.
: UNDEFINE-WORD ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu XREF:UNDEFINE-FIND 0< IF
      name nameu UNSEEN-COVERS? IF  name nameu DW-RENDERED DEFER-AT  EXIT  THEN
      name nameu XREF:UNDEFINE-INDEX drop
   THEN
   name nameu NAME-RETIRED
   name nameu CHECKER-UNDEFINE
   name nameu RECORD-SYM? {: sym:n :}
   sym NEWEST-SYM @ = IF 0 NEWEST-SYM ! THEN
   sym DEFINER-RETIRE ;

\ Rejects a package keyword with no package open, as `--load` does, unless
\ rendered text the run alone reads may have opened one; then the run judges it.
: PACKAGE-KEYWORD-GUARD ( -- bool )
   CHECKER-AUTH-PACKAGE-ACTIVE? IF true EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ {: a:^ u:n :}
   a u UNSEEN-COVERS? IF a u DW-RENDERED DEFER-AT  false EXIT THEN
   a u INTERPRETER:PKG-CONTEXT-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: SEAL-ROW ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-AUTH-PACKAGE$ SOURCE-SEAL THEN ;

EXPORT CHECKER-RESOLVE:PART-GUARD
EXPORT CHECKER-RESOLVE:PART-PATH

\ The engine's order: the part guard at `package`, then the name and its path,
\ the checker, the seal on the path.
: RECORD-PACKAGE ( -- )
   PART-GUARD
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu PART-PATH {: pa:^ pu:n :}
   pa pu name nameu CHECKER-PACKAGE
   pa pu name nameu at SEALED-PACKAGE ;

: RECORD-PUBLIC ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-PUBLIC THEN ;

: RECORD-PRIVATE ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-PRIVATE THEN ;

: RECORD-GLOBAL ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-GLOBAL THEN ;

: RECORD-END-PACKAGE ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-END-PACKAGE THEN ;

\ `using NAME` and `;using` are source events like `package` and `;package`:
\ without these rows a replayed file's own
\ imports do nothing and a name it imports is refused E-UNDEFINED. The checker
\ owns the replay's using depth, so these two rows are the only thing that
\ moves it.
: RECORD-USING ( -- )
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu OPEN-USING ;

\ Takes the `;using` token.
: RECORD-END-USING ( str -- )
   CLOSE-USING ;

\ DEFTYPE NAME declares a value nominal: a package-scoped arity-0 type family
\ whose lowercase tail is the surface name folded (SERIAL -> serial) and whose
\ converter pair >NAME ( n -- tail ) / NAME>N ( tail -- n ) keeps a plain n from
\ standing in for the nominal.
DYNAMIC-BUFFER NOM-TAIL u8

: MANGLE ( str -- str ) {: a:^ u:n :}
   u NOM-TAIL-RESERVE
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C  over NOM-TAIL c!
      1+
   REPEAT drop
   0 NOM-TAIL u ;

: CAST-DEFINED ( n n -- )
   {: at:n u:n :}
   TOP-CUR-A @ TOP-CUR-U @ DTC-NAME$ DTC-SIG$ at at u + DEF-WORD DEFINED ;

\ Under MULTI-ERR? the scan goes on and the caller registers nothing, so
\ TOP-DEFINER? opens an unreported stretch.
: DECLARATION-END ( -- )
   MULTI-ERR? IF REJECTION:KEEP EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: RECORD-DEFLINEAR ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu name nameu DEF-NAME-REJECT? IF DECLARATION-END EXIT THEN
   name nameu TYPE-RESERVED? IF
      name nameu CHECKER-LINEAR-TAKEN  DECLARATION-END EXIT
   THEN
   name nameu CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( str -- bool )
   s" END-VALUE-RECORD" STR=CI ;

: SUMTYPE-END? ( str -- bool )
   s" ;SUMTYPE" STR=CI ;

\ A SUMTYPE defines its constructors when it loads; the scan replays its body
\ through the same front end, which publishes their checked effects, so a later
\ definition that calls one resolves here as in the load.
DYNAMIC-BUFFER SUM-VAR-AT n
variable SUM-VAR-N
variable SUM-VAR-NEXT

: SUM-VAR-STEP ( str -- )
   {: a:^ u:n :}
   SUM-VAR-NEXT @ IF
      SUM-VAR-N @ 1+ SUM-VAR-AT-RESERVE
      TOKEN-BYTE @ SUM-VAR-N @ SUM-VAR-AT !
      SUM-VAR-N @ 1+ SUM-VAR-N !
   THEN
   a u s" VARIANT" STR=CI SUM-VAR-NEXT ! ;

\ Constructor symbols belong to the variants just registered. Give each its
\ source name token, in the same order the declaration registered the variants.
: SUM-VAR-DEFINED ( n -- )
   {: fam:n :}
   fam TFAM:TFAM-VAR-START@ {: start:n :}
   SUM-VAR-N @ 0 ?DO
      start i + {: vid:n :}
      vid TFAM:SUMV-CTOR-SYM@ {: sym:n :}
      sym 0 <> IF
         vid TFAM:SUMV-NAME$ {: name:^ nameu:n :}
         i SUM-VAR-AT @ {: at:n :}
         sym name nameu VISIT-CUR @ at at nameu +
            NCOMP-DISPATCH:DECL-VERIFY-CTOR-DECL-OFF OWNER-XT CTOR-DECL-ACTION execute
         s" VARIANT" name nameu sym NULL$ at at nameu + DEF-CONSTRUCTOR VISIT-CUR @ ON-DEFINITION
      THEN
   LOOP ;

\ A declaration name or body token, as its definer's parse-name reads one: `\` and `(`
\ are tokens there, not comments (`ENUM colr red \ note` refuses 7101 at `\`).
: DECL-TOKEN ( -- str ) NEXT-RAW ;

EXPORT TYPE-DECL:TDECL-SUMTYPE

: RECORD-SUMTYPE-BODY ( str -- )
   {: name:^ nameu:n :}
   0 SUM-VAR-N !  0 SUM-VAR-NEXT !
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup 0= IF 2drop 0 0= ELSE
         2dup BODY-APPEND
         2dup SUMTYPE-END? IF 2drop 0 0= ELSE SUM-VAR-STEP 0 0= 0= THEN
      THEN
   UNTIL
   TFAM:TFAM-N@ {: before:n :}
   \ The front reads its keyword as the token read last, as on the native path.
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   TOP-CUR-A @ TOP-CUR-U @ SRCLOC:TOKEN!
   name nameu BODY$
   [: [: TDECL-SUMTYPE ;] DECL-REPLAY:RP-RUN ;]
   [: oa ou SRCLOC:TOKEN! ;] finally
   TFAM:TFAM-N@ before > IF
      TFAM:TFAM-N@ 1- SUM-VAR-DEFINED
   THEN ;

: RECORD-SUMTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu at SEALED-FAMILY
   name nameu [: RECORD-SUMTYPE-BODY ;] [: SUM-VAR-AT-RELEASE ;] finally ;

: ENUM-END? ( str -- bool )
   s" ;ENUM" STR=CI ;

\ The body keeps its ;ENUM, which the front end parses itself; one the input
\ ends inside replays without it, and the front end refuses "missing ;ENUM".
: RECORD-ENUM ( -- )
   0 NEWEST-SYM !
   DECL-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu at SEALED-FAMILY
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup 0= IF 2drop 0 0= ELSE 2dup BODY-APPEND ENUM-END? THEN
   UNTIL
   name nameu BODY$ TYPE-DECL:REPLAY-ENUM
   IF DW-TYPE DEFER-AT ELSE 2drop THEN ;

: STRUCTURE-DECL-END? ( str -- bool )
   s" ;STRUCTURE" STR=CI ;

\ The replay registers MAKE/UNMAKE, so `FAMILY:MAKE` resolves later in this
\ source; an unclosed body replays as RECORD-ENUM's does: "missing ;STRUCTURE".
: RECORD-STRUCTURE-DECL ( -- )
   0 NEWEST-SYM !
   DECL-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu at SEALED-FAMILY
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup 0= IF 2drop 0 0= ELSE 2dup BODY-APPEND STRUCTURE-DECL-END? THEN
   UNTIL
   name nameu BODY$ TYPE-DECL:REPLAY-STRUCTURE
   IF DW-TYPE DEFER-AT ELSE 2drop THEN ;

: MUTUAL-END? ( str -- bool )
   s" ;MUTUAL" STR=CI ;

\ Whether a MUTUAL's first token names a family to seal: a member's STRUCTURE,
\ ;MUTUAL and a nameless header do not. The replay judges every later entry.
: MUTUAL-LISTED? ( str -- bool ) {: a:^ u:n :}
   a u TYPE-DECL:DECL-FAMILY-NAME nip 0<>
   a u s" STRUCTURE" STR=CI 0= and  a u MUTUAL-END? 0= and ;

\ Appends the next MUTUAL body token; answers whether the body ended.
: MUTUAL-TOKEN ( -- bool )
   DECL-TOKEN {: a:^ u:n :}
   u 0= IF true EXIT THEN
   a u BODY-APPEND
   a u MUTUAL-END? ;

\ The replay registers every member's MAKE/UNMAKE and accessors, so
\ `NODE:MAKE` resolves later in this source. The first list token is the
\ replay's head; an unclosed body replays as RECORD-STRUCTURE-DECL's does:
\ "missing ;MUTUAL".
: RECORD-MUTUAL ( -- )
   DECL-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu MUTUAL-LISTED? IF name nameu at SEALED-FAMILY THEN
   BODY-RESET
   name nameu MUTUAL-END? 0= IF
      BEGIN MUTUAL-TOKEN UNTIL
   THEN
   name nameu BODY$ TYPE-DECL:REPLAY-MUTUAL
   IF DW-TYPE DEFER-AT ELSE 2drop THEN ;

: LEGACY-TYPE-OUTCOME ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   rc TYPE-DECL:E-TDECL-UNRESOLVED <> IF rc throw THEN
   QUIET-TYPE-REPORT ;

PTR-VARIABLE STG-A
variable STG-U
PTR-VARIABLE STG-START
PTR-VARIABLE STG-TYPE-A
variable STG-TYPE-U

: SCAN-LINE-TOKEN ( -- str )
   SCAN-I @ {: end:n :}
   SKIP-WS
   SOURCE@ end +  SCAN-I @ end -  CHECKER-TYPE-SPAN-BREAK? IF SOURCE@ 0 EXIT THEN
   NEXT-RAW ;

: SCAN-STORAGE-MORE? ( n bool -- n bool ) {: depth:n ended:bool :}
   ended IF depth 0 0= 0= EXIT THEN
   depth 0 > IF NEXT-RAW ELSE SCAN-LINE-TOKEN THEN {: a:^ u:n :}
   u 0= IF depth 0 0= 0= EXIT THEN
   a STG-A !  u STG-U !
   depth 0 0= ;

: SCAN-STORAGE-TYPE ( -- str )
   SCAN-LINE-TOKEN STG-U !  STG-A !
   STG-A @ STG-START !
   0 BEGIN STG-A @ STG-U @ CHECKER-TYPE-SPAN-STEP SCAN-STORAGE-MORE? 0= UNTIL drop
   STG-START @  STG-A @ STG-U @ + STG-START @ -
   2dup STG-TYPE-U ! STG-TYPE-A ! ;

\ Whether the next token on the line is the SCRATCH-ONLY clause, consumed; any
\ other is left unread.
: SCAN-SCRATCH-CLAUSE? ( -- bool )
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN s" scratch-only" STR=CI IF 0 0= EXIT THEN
   mark SCAN-I !  0 0= 0= ;

: NEWTYPE-CELL ( str bool -- ) {: name:^ nameu:n scratch:bool :}
   name nameu 2dup s" newtype" CHECKER-DEFFAMILY  scratch SCRATCH-MARK ;

\ CHECKER-DEFFAMILY's record rejects a missing name, at the keyword, or a
\ malformed header; a base on the name's line replays as NEWTYPE registers it.
: RECORD-NEWTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   nameu 0= IF name nameu TOP-CUR-A @ TOP-CUR-U @ s" newtype" CHECKER-DEFFAMILY EXIT THEN
   name nameu at SEALED-FAMILY
   0 0= 0= nameu 0 ?do name i + c@ 60 = or loop
   IF name nameu SCAN-SCRATCH-CLAUSE? NEWTYPE-CELL EXIT THEN
   SCAN-SCRATCH-CLAUSE? IF name nameu 0 0= NEWTYPE-CELL EXIT THEN
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN {: ta:^ tu:n :}
   mark SCAN-I !
   tu 0= IF name nameu 0 0= 0= NEWTYPE-CELL EXIT THEN
   tu 1 = IF ta c@ 92 = IF name nameu 0 0= 0= NEWTYPE-CELL EXIT THEN THEN
   name nameu 2dup SCAN-STORAGE-TYPE SCAN-SCRATCH-CLAUSE? {: scratch:bool :}
   s" newtype" TYPE-DECL:SD-WRAP-OUTCOME
   IF DW-TYPE DEFER-AT EXIT THEN 2drop
   scratch SCRATCH-MARK ;

\ A DEFTYPE base is the one name on the name's line, consumed; none, or a `\`
\ left unread, is `n`, and the bool says whether one was spelled.
: DEFTYPE-BASE ( -- str bool )
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN {: ba:^ bu:n :}
   bu 0= IF mark SCAN-I !  s" n" 0 0= 0= EXIT THEN
   bu 1 = IF ba c@ 92 = IF mark SCAN-I !  s" n" 0 0= 0= EXIT THEN THEN
   ba bu 0 0= ;

\ A base replays as DEFTYPE registers it; a deferred or rejected family
\ declares no converters.
: RECORD-DEFTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   TOKEN-BYTE @ {: at:n :}
   name nameu MANGLE {: tail:^ tailu:n :}
   DEFTYPE-BASE {: ba:^ bu:n based:bool :}
   based IF name nameu ba bu TYPE-DECL:SD-ONE-NAME 0= IF EXIT THEN THEN
   REJECTION:TALLY @ {: refused:n :}
   based IF name nameu at nameu SEALED-NAME ELSE name nameu at SEALED-FAMILY THEN
   based IF
      tail tailu name nameu ba bu s" deftype" TYPE-DECL:SD-WRAP-OUTCOME
      IF DW-TYPE DEFER-AT EXIT THEN 2drop
   ELSE
      tail tailu name nameu s" deftype" CHECKER-DEFFAMILY
   THEN
   REJECTION:TALLY @ refused <> IF EXIT THEN
   name nameu tail tailu ba bu DTC-BUILD-IN
   DTC-NAME$ at nameu ARM              \ the converter's own name, the TYPE's span
   DTC-NAME$ CHILD-ABI:NOMINAL-IN CHECKER-NOMINAL-REPLAY
   DISARM
   at nameu CAST-DEFINED
   name nameu tail tailu ba bu DTC-BUILD-OUT
   DTC-NAME$ at nameu ARM
   DTC-NAME$ CHILD-ABI:NOMINAL-OUT CHECKER-NOMINAL-REPLAY
   DISARM
   at nameu CAST-DEFINED ;

\ A name refused as malformed or a duplicate is empty and still owns its type
\ span; consume it before resuming the statement scan so a type token cannot
\ be interpreted as a new statement.
: SCAN-STORAGE-NAME ( -- str )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN
   dup 0= IF TOP-CUR-A @ TOP-CUR-U @ CHECKER-STORAGE-NAME-REFUSE EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ 2over REJECTION:NOT-A-NAME
   2dup NAME-DECLARED
   2dup CHECKER-LBUF-NAME-OK? IF
      2dup TOKEN-BYTE @ REJECT-DUPLICATE 0= IF EXIT THEN
   THEN
   2drop SCAN-STORAGE-TYPE 2drop SOURCE@ 0 ;

\ The load stops at a count token it refuses to resolve, refused there as a
\ top-level token, so the declaration after it is read but not judged: its
\ count is refused once.
: COUNT-REFUSED? ( str -- bool )
   {: a:^ u:n :}
   u 0<>  a TOP-REFUSED @ =  and ;

\ Whether NAME+SFX, a word the definer derives from NAME, read from byte AT,
\ is free: a taken one is the duplicate, rejected at NAME.
: DERIVED-FREE? ( str str n -- bool )
   {: name:^ nameu:n sfx:^ sfxu:n at:n :}
   name nameu sfx sfxu CHECKER-LBUF-SUFFIXED$ {: gen:^ genu:n :}
   gen genu CHECKER-LBUF-NAME-OK? 0= IF false EXIT THEN
   gen genu CHECKER-CERT-DUP? 0= IF true EXIT THEN
   gen genu SOURCE@ at + nameu DUPLICATE-AT!  false ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:^ countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFLAYOUT-BUFFER
   name nameu type typeu STG-SCRATCH-SAVED LBUF:SCRATCH-HELD? drop
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

\ DEFER-LAYOUT-BUFFER publishes its accessor and NAME-BIND and NAME-GROW from
\ one line, so a later definition calling one is E-UNDEFINED without this row,
\ and a type it cannot size goes unreported until the run. No count token: the
\ count arrives at the bind.
: RECORD-DEFER-LAYOUT-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu s" -BIND" at DERIVED-FREE? 0= IF EXIT THEN
   name nameu s" -GROW" at DERIVED-FREE? 0= IF EXIT THEN
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFDEFER-LAYOUT-BUFFER
   name nameu type typeu STG-SCRATCH-SAVED LBUF:SCRATCH-HELD? drop
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:^ countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF SCAN-SCRATCH-CLAUSE? drop EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   SCAN-SCRATCH-CLAUSE? {: scratch:bool :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFTYPED-BUFFER
   scratch 0= IF name nameu type typeu STG-SCRATCH-TYPE LBUF:SCRATCH-HELD? drop THEN
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

: RECORD-TYPED-VARIABLE ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF SCAN-SCRATCH-CLAUSE? drop EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   SCAN-SCRATCH-CLAUSE? {: scratch:bool :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFTYPED-VARIABLE
   scratch 0= IF name nameu type typeu STG-SCRATCH-TYPE LBUF:SCRATCH-HELD? drop THEN
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

: RECORD-BYTE-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:^ countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFBYTE-BUFFER
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;


: RECORD-CWORK-SPAN ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:^ countu:n :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   count countu COUNT-REFUSED? IF EXIT THEN
   name nameu at nameu ARM
   type typeu count countu name nameu CHECKER-DEFCWORK-SPAN
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

: RECORD-SLOT ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFSLOT
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

: RECORD-CWORK-VARIABLE ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFCWORK-VARIABLE
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

\ DYNAMIC-BUFFER publishes THREE words from one line - the accessor,
\ NAME-RESERVE and NAME-RELEASE. Certification never runs the definer, and
\ without this row a later definition in the same source calling one of the
\ three is E-UNDEFINED. No count token: a dynamic buffer's extent is set at run
\ time.
: RECORD-DYNAMIC-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu s" -RESERVE" at DERIVED-FREE? 0= IF EXIT THEN
   name nameu s" -RELEASE" at DERIVED-FREE? 0= IF EXIT THEN
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFDYNAMIC-BUFFER
   DISARM
   name nameu at DEF-STORAGE DEFINED-MADE ;

\ Whether neither of ARENA's words NAME derives is taken in the checker's
\ current section.
: ARENA-WORDS-FREE? ( str n -- bool )
   {: name:^ nameu:n at:n :}
   name nameu s" -NEW" at DERIVED-FREE? 0= IF false EXIT THEN
   name nameu s" -NEW-DISPOSE" at DERIVED-FREE? ;

\ Whether Q's private section, which the qualified NAME Q:TAIL never writes,
\ holds no TAIL+SFX; NAME is read from byte AT.
: ARENA-PRIVATE-FREE? ( str str n -- bool )
   {: name:^ nameu:n sfx:^ sfxu:n at:n :}
   name nameu CHECKER-QUALIFIED? drop
   CHECKER-QPKG$ XREF-PRIVATE-WL {: wid:n :}
   wid 0= IF true EXIT THEN
   CHECKER-QTAIL$ sfx sfxu CHECKER-LBUF-SUFFIXED$
   wid CHECKER-RESOLVE:WL-PROBE NULL-PTR = IF true EXIT THEN
   name nameu sfx sfxu CHECKER-LBUF-SUFFIXED$
   SOURCE@ at + nameu DUPLICATE-AT!  false ;

\ ARENA's row asks the definer's questions in the order its run asks them and
\ registers both words; without it the rest of the source defers to the run.
: RECORD-ARENA ( -- )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF TOP-CUR-A @ TOP-CUR-U @ CHECKER-STORAGE-NAME-REFUSE EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= IF EXIT THEN
   name nameu at nameu SEALED-NAME
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   name nameu at ARENA-WORDS-FREE? 0= IF EXIT THEN
   name nameu CHECKER-QUALIFIED? IF
      name nameu s" -NEW" at ARENA-PRIVATE-FREE? 0= IF EXIT THEN
      name nameu s" -NEW-DISPOSE" at ARENA-PRIVATE-FREE? 0= IF EXIT THEN
   ELSE
      \ A `global` section's words land in neither section of its package.
      mode CHECKER-PACKAGE-GLOBAL <> IF
         mode CHECKER-PACKAGE-PUBLIC = {: pub:bool :}
         pub IF CHECKER-PRIVATE ELSE CHECKER-PUBLIC THEN
         true [: drop name nameu at ARENA-WORDS-FREE? ;] catch {: rc:n :}
         pub IF CHECKER-PUBLIC ELSE CHECKER-PRIVATE THEN
         rc 0 <> IF rc throw THEN
         0= IF EXIT THEN
      THEN
   THEN
   type typeu CHECKER-ARENA-INFO 0= IF
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REJECT EXIT
   THEN drop
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFARENA
   DISARM ;

\ HEAP's row asks the definer's questions in the order its run asks them and
\ registers both words; without it the rest of the source defers to the run.
: RECORD-HEAP ( -- )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF TOP-CUR-A @ TOP-CUR-U @ CHECKER-STORAGE-NAME-REFUSE EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu NAME-DECLARED
   SCAN-STORAGE-TYPE {: type:^ typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= IF EXIT THEN
   name nameu at nameu SEALED-NAME
   name nameu s" -NEW" at DERIVED-FREE? 0= IF EXIT THEN
   name nameu s" -RELEASE" at DERIVED-FREE? 0= IF EXIT THEN
   type typeu CHECKER-DYNAMIC-INFO 0= IF
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REJECT EXIT
   THEN drop
   name nameu at nameu ARM
   type typeu name nameu CHECKER-DEFHEAP
   DISARM ;

\ Registrars resolve the whole type before publishing a word, so an unresolved
\ type leaves only the scoped uncertainty mark.
: STORAGE-OUTCOME ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   DISARM
   rc E-CHECKER-STORAGE-UNRESOLVED <> IF rc throw THEN
   STG-TYPE-A @ STG-TYPE-U @ DW-TYPE DEFER-AT
   QUIET-TYPE-CLEAR ;

\ The field list is the source from its first token f to END-VALUE-RECORD at
\ e, so the field token the checker rejects is placed; a record taken by name
\ or with no field is rejected at its name.
: VALUE-RECORD-ADD ( str ^u8 ^u8 -- )
   {: name:^ nameu:n f:^ e:^ :}
   e f - {: fu:n :}
   name nameu f fu CHECKER-TRY-RECORD {: at:n why:n :}
   why 0= IF EXIT THEN
   name nameu  f fu at VREC-TOKEN  why CHECKER-RECORD-REJECT
   DECLARATION-END ;

\ A record the input ends inside is rejected at its name, as the loader
\ rejects it.
: RECORD-VALUE-RECORD ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu REJECTION:NOT-A-NAME
   name nameu name nameu DEF-NAME-REJECT? IF DECLARATION-END EXIT THEN
   DECL-TOKEN  over {: f:^ :}
   BEGIN
      dup 0= IF
         name nameu VRDEF-UNCLOSED
         MULTI-ERR? IF 2drop REJECTION:KEEP EXIT THEN
         EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
      2dup VALUE-RECORD-END? IF
         drop {: e:^ :}
         name nameu f e VALUE-RECORD-ADD
         EXIT
      THEN
      2drop DECL-TOKEN
   AGAIN ;

: VALUE-RECORD-OUTCOME ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   rc E-CHECKER-RECORD-UNRESOLVED <> IF rc throw THEN
   QUIET-TYPE-REPORT ;

\ The row count is a checker cell the rollback frame rewinds, so a rewound
\ scope releases its rows. PRS-TERMS holds each row's terminators, each followed
\ by a blank, copied as the row is read, before its source can be released;
\ bytes past the last live row's are free. Growth may move PRS-TERMS.
DYNAMIC-BUFFER PRS-OWNER n                 \ the checker owner whose scope keyed it
DYNAMIC-BUFFER PRS-SYM n                   \ the binding's symbol
DYNAMIC-BUFFER PRS-EFF n                   \ and its visible record's offset + 1
DYNAMIC-BUFFER PRS-COUNT n                 \ the tokens it reads first
DYNAMIC-BUFFER PRS-AT n                    \ where its terminators start in PRS-TERMS
DYNAMIC-BUFFER PRS-LEN n                   \ their bytes; 0 for a `parses:` row
DYNAMIC-BUFFER PRS-TERMS u8

: PRS-OWNER-BASE ( -- n )
   SOURCE-OWNER-A @ BYTE-VIEW NULL-PTR BYTE-VIEW - ;

CAST: TOP-ACTION ( n -- [ ^u8 n bool -- n ] )
: TOP-VERDICT ( str bool -- n )
   NCOMP-DISPATCH:DECL-VERIFY-TOP-OFF OWNER-XT TOP-ACTION execute ;

\ A replaced or unlocated engine word has no live origin and names no use.
: ENGINE-ORIGIN-USE ( str n n -- )
   {: a:^ u:n at:n sym:n :}
   sym 0= IF EXIT THEN
   sym ENGINE-ORIGIN:TARGET
   {: path:^ pathu:n start:n end:n name:^ nameu:n found:bool :}
   found IF at dup u + sym 0 0 0 USE-SEEN THEN ;

\ A definer's own `generates:` row declares the text its calls render: the row
\ this scan read or, for a definer it never read, the engine's. A wrapper has
\ none: it states nothing about the rest of its body.
: DECLARED-RENDER? ( str -- bool )
   {: a:^ u:n :}
   a u DEFINER-OF dup 0<> IF 1 - DEFINER-GEN @ EXIT THEN drop
   a u TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-GENERATES and 0<> ;

\ Rendered text no row declares is source this scan never reads.
: RENDER-GAP ( str -- )
   {: a:^ u:n :}
   \ `;` is reserved: it ends a definition its opener began, and its gap is the opener's.
   a u s" ;" STR=CI IF EXIT THEN
   a u DECLARED-RENDER? IF EXIT THEN
   a u TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-DEFER and 0<>
   IF a u DW-DEFER DEFER-AT EXIT THEN
   a u DW-RENDERER DEFER-AT ;

\ A call of a `names:` word right after an `s"` literal is a use of the word
\ the literal names, at the bytes between its quotes. A literal written with an
\ escape binds nothing: its slot's bytes are shorter than its text.
: TOP-NAMED ( str -- )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-NAMES and 0= IF EXIT THEN
   STR-LAST-KIND @ LIT-PATH <> IF EXIT THEN
   STR-LAST-END @ STR-LAST-AT @ -  STR-LAST-U @ <> IF EXIT THEN
   STR-LAST-A @ STR-LAST-U @ CHECKER-NAMED-USE ;

\ An unmodelled call makes the rest of the source uncertain, declarations the
\ source index has already seen included.
: TOP-OPAQUE ( str n -- )
   {: a:^ u:n why:n :}
   a u why DEFER-AT
   true TICK-REMAINDER ! ;

public

\ Whether a call of the word a u names may move the source root.
: ROOT-MOVE? ( str -- bool )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-ROOTS and 0<> ;

private

: BOUND-CALL ( str bool -- )
   {: a:^ u:n renders:bool :}
   a u ROOT-MOVE? IF a u DW-ROOTS TOP-OPAQUE EXIT THEN
   renders IF a u RENDER-GAP THEN
   a u TOP-NAMED
   \ Literal openers only push the scanned string. Other calls may alter bindings.
   INDEX-ON @ IF a u STRING-OPENER? 0= IF a u 0 TOP-OPAQUE THEN THEN ;

: BINDING-ID ( n -- n )
   CHECKER-OWNER-ABI:BINDING-ID-MASK and CHECKER-OWNER-ABI:BINDING-ID-SHIFT rshift ;

: PRS-TERMS-END ( -- n )
   0 VERIFY-PARSES-N @
   BEGIN dup 0 > WHILE
      1 -
      dup PRS-AT @ over PRS-LEN @ +  rot max swap
   REPEAT drop ;

\ The newest row of this owner for the binding, + 1; 0 when none.
: PRS-FIND ( n n -- n )
   {: sym:n eff1:n :}
   sym 0=  eff1 0=  or IF 0 EXIT THEN
   PRS-OWNER-BASE {: own:n :}
   VERIFY-PARSES-N @ BEGIN dup 0 > WHILE
      1 -
      dup PRS-SYM @ sym =  over PRS-EFF @ eff1 = and  over PRS-OWNER @ own = and
      IF 1 + EXIT THEN
   REPEAT ;

: PRS-TERM-ADD ( str n -- n )
   {: a:^ u:n len:n :}
   PRS-TERMS-END len + {: at:n :}
   at u + 1 + PRS-TERMS-RESERVE
   a  at PRS-TERMS  u BYTE-COPY
   32  at u + PRS-TERMS  c!
   len u + 1 + ;

\ The bytes kept and the token's kind: 0 a terminator, 1 the closing `)`, 2 the
\ source's end.
: PRS-LIST-TOKEN ( n -- n n )
   {: len:n :}
   NEXT-RAW {: a:^ u:n :}
   u 0= IF len 2 EXIT THEN
   a u s" )" CORE-STR= IF len 1 EXIT THEN
   a u len PRS-TERM-ADD 0 ;

\ As the engine word reads it: `(`, then tokens through a standalone `)`, at
\ least one before it. The bytes kept, and true when malformed.
: PRS-LIST-READ ( -- n bool )
   NEXT-RAW s" (" CORE-STR= 0= IF 0 0 0= EXIT THEN
   0 BEGIN PRS-LIST-TOKEN dup 0= WHILE drop REPEAT
   {: len:n end:n :}
   len  end 2 =  len 0=  or ;

\ Known by the identity of the word the token selects, never by its spelling.
\ The row is read as the engine word reads it and checked by the engine's own
\ checks. The scan knows every token the declarer reads, so none of it is the
\ run's.
: PARSES-ROW ( str bool -- )
   {: ka:^ ku:n through:bool :}
   NEXT-RAW {: ta:^ tu:n :}
   NEXT-RAW {: ca:^ cu:n :}
   through IF PRS-LIST-READ ELSE 0 0 0= 0= THEN {: len:n bad:bool :}
   ka ku ta tu ca cu bad CHECKER-PARSES-ROW {: sym:n eff1:n count:n :}
   sym 0= IF EXIT THEN
   VERIFY-PARSES-N @ {: row:n :}
   row 1 + PRS-OWNER-RESERVE  row 1 + PRS-SYM-RESERVE  row 1 + PRS-EFF-RESERVE
   row 1 + PRS-COUNT-RESERVE  row 1 + PRS-AT-RESERVE  row 1 + PRS-LEN-RESERVE
   PRS-TERMS-END row PRS-AT !
   PRS-OWNER-BASE row PRS-OWNER !  sym row PRS-SYM !  eff1 row PRS-EFF !
   count row PRS-COUNT !  len row PRS-LEN !
   row 1 + VERIFY-PARSES-N !
   sym count  row PRS-AT @ PRS-TERMS  len PROW-ADD-TEXT ;

\ Read and checked as the engine word does.
: NAMES-ROW ( str -- )
   NEXT-RAW CHECKER-NAMES-ROW ;

: PRS-TERM-END ( n str -- n )
   {: t:^ tu:n :}
   BEGIN dup tu < IF dup t + c@ 32 <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT ;

: PRS-TERM-AT? ( n n str ^u8 -- bool )
   {: i:n j:n a:^ u:n t:^ :}
   j i - u <> IF 0 0= 0= EXIT THEN
   a u  t i +  u  CORE-STR= ;

\ A U is one of the terminators T TU holds, each followed by a blank.
: PRS-TERM? ( str str -- bool )
   {: a:^ u:n t:^ tu:n :}
   0
   BEGIN dup tu < WHILE
      dup t tu PRS-TERM-END
      2dup a u t PRS-TERM-AT? IF 2drop 0 0= EXIT THEN
      nip 1 +
   REPEAT drop 0 0= 0= ;

\ A row of this scan as a kept row reads: its count and terminators' bytes.
: PRS-ROW ( n -- n str )
   {: row:n :}
   row PRS-COUNT @  row PRS-AT @ PRS-TERMS  row PRS-LEN @ ;

\ Consumed as the run reads it. The source's end ends it: what is there is
\ consumed, and the word stays the run's.
: PRS-CONSUME ( n str -- )
   {: cnt:n t:^ tu:n :}
   cnt 0 ?do NEXT-RAW nip 0= IF unloop EXIT THEN loop
   tu 0= IF EXIT THEN
   BEGIN NEXT-RAW dup 0= IF 2drop EXIT THEN t tu PRS-TERM? UNTIL ;

\ Structure bodies take this path too.
: TOP-PARSER ( str -- )
   {: a:^ u:n :}
   a u TOP-BINDING {: sym:n eff1:n ctl:n :}
   ctl BINDING-ID {: id:n :}
   id CHECKER-OWNER-ABI:BINDING-PARSES-ID = IF a u 0 0= 0= PARSES-ROW EXIT THEN
   id CHECKER-OWNER-ABI:BINDING-THROUGH-ID = IF a u 0 0= PARSES-ROW EXIT THEN
   id CHECKER-OWNER-ABI:BINDING-NAMES-ID = IF a u NAMES-ROW EXIT THEN
   ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0<> IF a u DW-DEFER TOP-OPAQUE EXIT THEN
   sym eff1 PRS-FIND {: row:n :}
   row 0 <> IF a u DW-PARSES DEFER-AT  row 1 - PRS-ROW PRS-CONSUME EXIT THEN
   sym PROW-FIND {: cnt:n t:^ tu:n kept:bool :}
   kept 0= IF a u DW-PARSER TOP-OPAQUE EXIT THEN
   a u DW-PARSES DEFER-AT
   cnt t tu PRS-CONSUME ;

: TOKEN-TAIL ( str -- str )
   {: a:^ u:n :}
   u 1 > IF
      u 1 - 1 ?do
         a i + c@ 58 = IF a i + 1 +  u i - 1 -  unloop EXIT THEN
      loop
   THEN
   a u ;

\ An export is the same word under another tail, so it takes the row of the
\ word it re-exports, FROM (its row + 1, 0 for none), under the binding the
\ top-level find selects by package and tail. The bare tail is no such
\ selection: in the package it can name the package's own private word. A word
\ that only calls a bounded word takes no row.
: EXPORT-PARSES ( n str -- )
   {: from:n a:^ u:n :}
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
   row 1 + VERIFY-PARSES-N !
   sym row PRS-ROW PROW-ADD-TEXT ;

\ The export's record is the open section's, under the tail the checker
\ recorded; its declaring token is the name as written, from byte at.
: EXPORT-DEFINED ( str n -- )
   {: name:^ nameu:n at:n :}
   name nameu RECORD-SYM? SYM-IDENTITY drop 2swap 2drop RETAINED-SYM
   {: sym:n :}
   sym NEWEST-SYM !
   sym 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ name nameu sym s" " at at nameu + DEF-EXPORT
   VISIT-CUR @ ON-DEFINITION ;

\ Whether the export's tail, from the u bytes written at byte at, is already
\ defined here; its record stands at the tail.
: EXPORT-DUPLICATE? ( n n -- bool ) {: u:n at:n :}
   SOURCE@ at + {: a:^ :}
   a u CHECKER-QUALIFIED? IF CHECKER-QTAIL$ ELSE a u THEN {: ta:^ tu:n :}
   ta tu CHECKER-CERT-DUP? 0= IF false EXIT THEN
   ta tu ta tu DUPLICATE-AT!  true ;

\ Whether the export a u, written from byte at, is recorded: its source
\ resolves before its tail is judged a duplicate, as the load judges them.
: EXPORT-CHECKED? ( str n -- bool ) {: a:^ u:n at:n :}
   a u CHECKER-EXPORT-SOURCE {: src:n priv:^ :}
   u at EXPORT-DUPLICATE? IF false EXIT THEN
   a u src priv RES-FALSE CHECKER-EXPORT-ROW  true ;

\ The catch only disarms before a stop passes on.
: RECORD-EXPORT ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF EXIT THEN
   name nameu at nameu SEALED-OPEN
   name nameu TOP-BINDING drop PRS-FIND {: from:n :}
   name nameu at nameu ARM-UNSEALED   \ the checker spells the record by the tail it keeps
   false [: drop name nameu at EXPORT-CHECKED? ;] catch {: rc:n :}
   DISARM
   rc 0<> IF rc throw THEN
   0= IF EXIT THEN
   name nameu at EXPORT-DEFINED
   from name nameu EXPORT-PARSES ;

\ A require publishes the canonical path before descending, so recursive
\ requires stop where the native loader stops, and a path already held reports
\ held. An include always descends. A file whose bytes were acquired for a load
\ reports read before it is scanned.
: COMPOSE-PENDING ( -- )
   COMPOSE-PEND-A @ COMPOSE-PEND-U @
   COMPOSE-PEND-PATH-A @ COMPOSE-PEND-PATH-U @ COMPOSE-FILE ;

\ The owner record's file field holds a raw execution token.
CAST: FILE-ACTION ( n -- [ [ -- renders parses nominal ] -- renders parses nominal ] )

\ The operand span LOAD-AT to LOAD-END of the subject's top-level loader whose
\ load runs; LOAD-END 0 for none (an operand ends past the file's first byte).
variable LOAD-AT
variable LOAD-END

: ARM-LOAD ( n n -- )
   {: at:n end:n :}
   VISIT-CUR @ VISIT-FIRST @ <> IF EXIT THEN
   at LOAD-AT !  end LOAD-END ! ;

: LOADED ( ^u8 n n -- )
   {: path:^ pathu:n outcome:n :}
   LOAD-END @ 0= IF EXIT THEN
   LOAD-AT @ LOAD-END @  0 LOAD-END !
   path pathu outcome ON-LOADER ;

: COMPOSE-LOADED ( str str -- )
   {: a:^ u:n path:^ pathu:n :}
   path pathu LOAD-READ LOADED
   a COMPOSE-PEND-A !  u COMPOSE-PEND-U !
   path COMPOSE-PEND-PATH-A !  pathu COMPOSE-PEND-PATH-U !
   [: COMPOSE-PENDING ;]
   CHECKER-OWNER-ABI:VERIFY-FILE-OFF OWNER-XT FILE-ACTION execute ;

\ A composition rejects a load of a file no root holds as the loader does: its
\ record names the request a u and stands at LOAD-PLACE, and FAULT-TARGET$
\ names the file the read would have opened. No read runs.
: COMPOSE-MISSING ( str str -- )
   {: a:^ u:n path:^ pathu:n :}
   path pathu FAULT-TARGET!
   a u LOAD-PLACE-A @ LOAD-PLACE-U @ false SOURCE-ROOT:MISSING-SOURCE ;

\ Load PATH, the file the request a u resolved to under ROOT.
: COMPOSE-OPEN ( str str str -- )
   {: a:^ u:n path:^ pathu:n root:^ rootu:n :}
   path pathu COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ CORE-STR= IF
      root rootu path pathu COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @
      [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-SUPPLIED EXIT
   THEN
   path pathu FS:EXISTS? 0= IF a u path pathu COMPOSE-MISSING THEN
   path pathu SUPPLIED {: t:^ tu:n open:bool :}
   open IF root rootu path pathu t tu [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-SUPPLIED EXIT THEN
   root rootu path pathu [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-BYTES ;

\ The loads loader word w, the token the source wrote, makes of the path a u:
\ the resolver's answer comes through the loader's, which writes its records.
: COMPOSE-INCLUDED ( str str -- )
   {: w:^ wu:n a:^ u:n :}
   w wu a u [: SOURCE-ROOT:RESOLVE-AT ;] LOADER-RESOLVE drop {: path:^ pathu:n :}
   SOURCE-ROOT:RESOLVED-ROOT$ {: root:^ rootu:n :}
   a u path pathu root rootu COMPOSE-OPEN ;

: COMPOSE-REQUIRED ( str str -- )
   {: w:^ wu:n a:^ u:n :}
   w wu a u [: SOURCE-ROOT:RESOLVE-AT ;] LOADER-RESOLVE IF LOAD-HELD LOADED EXIT THEN
   SOURCE-ROOT:RESOLVED-ROOT$ {: root:^ rootu:n :}
   REQUIRE-STORE {: path:^ pathu:n :}
   a u path pathu root rootu COMPOSE-OPEN ;

: COMPOSE-SCRIPT-REQUIRED ( str -- )
   {: a:^ u:n :}
   NULL-PTR 0 a u [: 2drop SOURCE-ROOT:ENTRY-RESOLVE ;] LOADER-RESOLVE IF LOAD-HELD LOADED EXIT THEN
   SOURCE-ROOT:RESOLVED-ROOT$ {: root:^ rootu:n :}
   REQUIRE-STORE {: path:^ pathu:n :}
   a u path pathu root rootu COMPOSE-OPEN ;

: COMPOSE-PROVIDED ( str str -- )
   [: SOURCE-ROOT:RESOLVE-AT ;] LOADER-RESOLVE IF 2drop EXIT THEN
   REQUIRE-STORE 2drop ;

PTR-VARIABLE LOAD-A                           \ the path a top-level load loads
variable LOAD-U

: LOAD$ ( -- str )
   LOAD-A @ LOAD-U @ ;

: FILE-LOAD ( bool -- [ str -- str renders parses nominal ] )
   IF [: 2dup LOAD$ COMPOSE-INCLUDED ;] EXIT THEN
   [: 2dup LOAD$ COMPOSE-REQUIRED ;] ;

\ Run Q, the load a loader word wu bytes long at byte w makes, given the word;
\ the load's operand span ends with it.
: RUN-LOADER ( [ str -- str ] n n -- )
   {: q w:n wu:n :}
   SOURCE@ w + wu q [: 0 LOAD-END ! ;] finally 2drop ;

\ `include PATH` (inc true) or `require PATH` at top level, its word u bytes
\ long at TOKEN-BYTE; PATH's token is the operand its load reports.
: QUIET-WORD-LOAD ( n bool -- )
   {: u:n inc:bool :}
   TOKEN-BYTE @ {: at:n :}
   u LOADER-OPERAND {: p:^ pu:n :}
   TOKEN-BYTE @ dup pu + ARM-LOAD
   p LOAD-A !  pu LOAD-U !
   p pu LOAD-PLACE!
   inc FILE-LOAD at u RUN-LOADER ;

\ A string loader word at top level, u bytes long at TOKEN-BYTE: refused by
\ the literal before it as discovery refuses it, else Q loads
\ that literal's path, and the literal is the operand its load reports.
: QUIET-STRING-LOAD ( n [ str -- str ] -- )
   {: u:n q :}
   STR-LAST-KIND @ u LITERAL-LOAD? 0= IF EXIT THEN
   STR-LAST-AT @ STR-LAST-END @ ARM-LOAD
   STR-LAST-A @ LOAD-A !  STR-LAST-U @ LOAD-U !
   SOURCE@ STR-LAST-AT @ +  STR-LAST-END @ STR-LAST-AT @ -  LOAD-PLACE!
   q TOKEN-BYTE @ u RUN-LOADER ;

: QUIET-TOP? ( str -- bool ) {: a:^ u:n :}
   a u s" include" STR=CI IF u true QUIET-WORD-LOAD true EXIT THEN
   a u s" require" STR=CI IF u false QUIET-WORD-LOAD true EXIT THEN
   a u s" included" STR=CI IF u true FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   a u s" required" STR=CI IF u false FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   a u s" script-required" STR=CI IF
      u [: LOAD$ COMPOSE-SCRIPT-REQUIRED ;] QUIET-STRING-LOAD true EXIT
   THEN
   a u s" provided" STR=CI IF
      u [: 2dup LOAD$ COMPOSE-PROVIDED ;] QUIET-STRING-LOAD true EXIT
   THEN
   false ;

\ Runs the loader call with its loader word a u as the token read last, as on
\ the native path, until the call returns or throws.
: LOADER-AT ( R [ R -- S ] str -- S )
   {: a:^ u:n :}
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   a u SRCLOC:TOKEN!
   [: oa ou SRCLOC:TOKEN! ;] finally ;

: LOADER-WORD? ( str -- bool ) {: a:^ u:n :}
   a u s" include" STR=CI  a u s" require" STR=CI or
   a u s" included" STR=CI or  a u s" required" STR=CI or
   a u s" script-required" STR=CI or  a u s" provided" STR=CI or ;

\ The source now scanned opened the FUNCTION: group still open. Nested files
\ share FFI's open group, as they share the load's open declaration.
: FFI-OPEN-HERE? ( -- bool )
   FFI:OPENER$ {: t:^ tu:n :}
   tu 0= IF false EXIT THEN
   t SOURCE@ - {: at:n :}
   at 0 < 0=  at SOURCE-U @ > 0= and ;

\ The load words; `provided` loads nothing.
: LOADS? ( str -- bool ) {: a:^ u:n :}
   a u LOADER-WORD?  a u s" provided" STR=CI 0= and ;

\ The load word w wu is called while the declaration t tu opened, which c cu
\ closes, is open: the load's record.
: UNCLOSED-LOAD-STOP ( str str str -- )
   SOURCE-ROOT:UNCLOSED-LOAD  EXIT-STATUS:REJECTED REJECTION:STOP ;

: COMPOSE-TOP? ( str -- bool ) {: a:^ u:n :}
   COMPOSE-ON @ 0= IF 0 0= 0= EXIT THEN
   a u LOADER-WORD? 0= IF 0 0= 0= EXIT THEN
   TICK-CONTEXT-UNKNOWN
   a u QUIET-USE
   a u TOKEN-BYTE @ a u TOP-BINDING drop drop ENGINE-ORIGIN-USE
   a u LOADS? FFI-OPEN-HERE? and IF FFI:OPENER$ s" ;FUNCTION" a u UNCLOSED-LOAD-STOP THEN
   a u [: QUIET-TOP? ;] a u LOADER-AT ;

public
\ Writes, open after its help, the record of the row opened at line l, column c
\ that the token t tu breaks or, with tu 0, that the source ends inside.
: ROW-REJECT ( str n n -- ) {: t:^ tu:n l:n c:n :}
   s" E-MALFORMED-REGISTRY-ROW" REJECTION:OPEN
   tu 0= IF s" found the end of the source" REJECTION:EMIT-STR
   ELSE s" found " REJECTION:EMIT-STR  t tu TICKED THEN
   s"  in the primitive-axiom row opened at " REJECTION:EMIT-STR
   l REJECTION:EMIT-U  s" :" REJECTION:EMIT-STR  c REJECTION:EMIT-U
   t tu REJECTION:AT
   tu 0= IF s" the source ends before the row closes"
   ELSE s" breaks the row" THEN REJECTION:EMIT-STR
   REJECTION:HELP
   s" a row's name and package are neither an opener nor a closer, "
   REJECTION:EMIT-STR
   s" its body holds only `PE-` or `;PE-` atoms and `REF` clauses, "
   REJECTION:EMIT-STR
   s" and it ends at `EPRIM;` or `ECLOSE-PRIVATE`: " REJECTION:EMIT-STR
   tu 0= IF s" close the row"
   ELSE s" close the row before this token or replace it" THEN REJECTION:EMIT-STR ;
private

\ The token t tu breaks the row the top-level token opened.
: ROW-MALFORMED ( str -- ) {: t:^ tu:n :}
   TOP-CUR-A @ REJECTION:LOCATE drop drop {: l:n c:n :}
   t tu l c ROW-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: ROW-ENDED ( -- )
   SOURCE@ SOURCE-U @ + 0 ROW-MALFORMED ;

\ The row's next raw token; none left makes the row malformed at the
\ source's end.
: ROW-TOKEN! ( -- )
   RAW!
   TOKEN-U @ 0= IF ROW-ENDED THEN ;

: ROW-TOKEN-SEEN ( -- )
   TOKEN-A @ TOKEN-U @ BODY-TOKEN-SEEN ;

\ A header token names the row's primitive or package. None, an opener or a
\ closer is rejected as a missing header, though the opener takes either as a name.
: ROW-HEADER ( -- )
   NEXT-RAW {: a:^ u:n :}
   u 0= IF ROW-ENDED THEN
   a u SOURCE-SYNTAX:ROW-OPENER? IF a u ROW-MALFORMED THEN
   a u SOURCE-SYNTAX:ROW-CLOSER? IF a u ROW-MALFORMED THEN ;

\ A row starts clear of the last body's literal, as every body reader does.
: RECORD-PRIM-ROW ( -- )
   BODY-PREV-CLEAR
   ROW-HEADER
   BEGIN
      ROW-TOKEN!
      TOKEN-A @ TOKEN-U @ SOURCE-SYNTAX:ROW-CLOSER? IF EXIT THEN
      TOKEN-A @ TOKEN-U @ SOURCE-SYNTAX:ROW-REF? IF
         ROW-TOKEN-SEEN  ROW-TOKEN!
      ELSE
         TOKEN-A @ TOKEN-U @ SOURCE-SYNTAX:ROW-ATOM? 0= IF
            TOKEN-A @ TOKEN-U @ ROW-MALFORMED
         THEN
      THEN
      ROW-TOKEN-SEEN
   AGAIN ;

: RECORD-PPRIM ( -- )
   ROW-HEADER
   RECORD-PRIM-ROW ;

: STRUCTURE-END? ( str -- bool )
   s" END-STRUCTURE" STR=CI ;

: TRUST-STRUCTURE-FIELD ( str str -- bool )
   DECL-SIGNATURE ;

: RECORD-STRUCTURE-FIELD ( str str -- )
   {: kind:^ kindu:n sig:^ sigu:n :}
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu NAME-DECLARED
   name nameu at REJECT-DUPLICATE IF EXIT THEN
   name nameu at nameu ARM
   name nameu sig sigu TRUST-STRUCTURE-FIELD
   DISARM
   IF kind kindu name nameu sig sigu at at nameu + DEF-WORD DEFINED THEN ;

: STRUCTURE-SIZE ( str n -- ) {: name:^ nameu:n at:n :}
   name nameu at REJECT-DUPLICATE IF EXIT THEN
   name nameu at nameu ARM
   name nameu s" -- n" DECL-SIGNATURE
   DISARM
   IF name nameu s" -- n" at DEF-CONSTANT DEFINED-HERE THEN ;

\ The structure scanner takes these words' operands itself, though their
\ run-time call may read source.
: STRUCTURE-SYNTAX? ( str -- bool )
   2dup STRUCTURE-END? IF 2drop true EXIT THEN
   2dup STRUCTURE-PTR-FIELD? IF 2drop true EXIT THEN
   2dup STRUCTURE-CFIELD? IF 2drop true EXIT THEN
   STRUCTURE-CELL-FIELD? ;

\ The body executes its bound words though this scanner reads their tokens.
\ `char` takes data and `'` names a word; both consume their next raw token. A
\ word that renders source is deferred at its token, as at top level.
: STRUCTURE-USE ( str -- )
   2dup TOP-PARSER? IF
      2dup CHAR-KEYWORD? IF 2drop OPERAND 2drop EXIT THEN
      2drop OPERAND
      2dup TOP-BINDING drop drop 0= IF 2drop EXIT THEN
      false TOP-VERDICT drop EXIT
   THEN
   2dup TOP-BINDING drop drop 0= IF 2drop EXIT THEN
   2dup CTL-RENDERS RENDERS-MARK? IF 2dup RENDER-GAP THEN
   2dup 2dup STRUCTURE-SYNTAX? 0= TOP-VERDICT 2 =
   IF TOP-PARSER ELSE 2drop THEN ;

\ The group open in this source is rejected when the token o ou opens another
\ declaration or, with ou 0, when the source ends.
: FFI-UNCLOSED ( str -- ) {: o:^ ou:n :}
   FFI-OPEN-HERE? 0= IF EXIT THEN
   FFI:OPENER$ s" ;FUNCTION" o ou UNCLOSED-DECL-STOP ;

\ A declaration opening, or a package scope opening or closing, inside the
\ structure rejects the structure.
: STRUCTURE-OPENED ( str -- ) {: o:^ ou:n :}
   o ou s" BEGIN-STRUCTURE" STR=CI  o ou s" FUNCTION:" STR=CI or
   o ou s" CALLBACK:" STR=CI or
   o ou s" package" STR=CI or  o ou s" ;package" STR=CI or
   o ou s" using" STR=CI or  o ou s" ;using" STR=CI or
   o ou s" seal-package" STR=CI or 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ s" END-STRUCTURE" o ou UNCLOSED-DECL-STOP ;

\ A load word inside the structure rejects the structure.
: STRUCTURE-LOADS ( str -- ) {: w:^ wu:n :}
   w wu LOADS? 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ s" END-STRUCTURE" w wu UNCLOSED-LOAD-STOP ;

\ A section word inside the structure switches the section, as at top level.
: STRUCTURE-SECTION ( str -- ) {: w:^ wu:n :}
   w wu s" public" STR=CI IF RECORD-PUBLIC EXIT THEN
   w wu s" private" STR=CI IF RECORD-PRIVATE EXIT THEN
   w wu s" global" STR=CI IF RECORD-GLOBAL THEN ;

: RECORD-STRUCTURE ( -- )
   TOP-CUR-A @ TOP-CUR-U @ FFI-UNCLOSED
   NAME-TOKEN TOKEN-BYTE @ {: name:^ nameu:n at:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu NAME-DECLARED
   name nameu at STRUCTURE-SIZE
   BEGIN
      NEXT-SCAN
      dup 0= IF TOP-CUR-A @ TOP-CUR-U @ s" END-STRUCTURE" NULL-PTR 0 UNCLOSED-DECL-STOP THEN
      2dup STRUCTURE-OPENED
      2dup STRUCTURE-LOADS
      2dup STRUCTURE-SECTION
      2dup STRUCTURE-USE
      TICK-REMAINDER @ IF 2drop EXIT THEN
      2dup STRUCTURE-END? IF
         2drop
         EXIT
      THEN
      \ A pointer field's pointee is independent of the record's element type:
      \ a cell record may hold a byte pointer.
      2dup STRUCTURE-PTR-FIELD? IF s" ^a -- ^^b" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CFIELD? IF s" ^a -- ^u8" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CELL-FIELD? IF s" ^a -- ^a" RECORD-STRUCTURE-FIELD ELSE
      2drop
      THEN THEN THEN
   AGAIN ;

\ `generates: D ( effect )`, read as the engine word reads it: the name by
\ parse-name, the effect as a definition head's signature. The registrar
\ resolves D and raises the refusals FIND-SYM defers; a row this scan's table
\ already holds for D, its clause or an earlier row, counts as D already stating
\ what it makes.
: RECORD-GENERATES ( -- )
   NAME-TOKEN
   {: name:^ nameu:n :}
   nameu 0= IF NAME-STOP THEN
   name nameu SIGNATURE-AT
   {: sig:^ sigu:n :}
   name nameu FIND-SYM
   {: sym:n :}
   name nameu sig sigu sym DEFINER-FIND 0<> CHECKER-GENERATES nip 0= IF EXIT THEN
   sig sigu true sym DEFINER-ADD ;

\ FFI's declaration machine reads `FUNCTION: … ;FUNCTION` from the raw tokens
\ fed to it. FFI-SIG holds the effect: BODY-BUF holds only runs of the source.
DYNAMIC-BUFFER FFI-SIG u8
variable FFI-SIG-U
variable FFI-LIT-N                          \ literals read last, at most two:
variable FFI-LIT-A                          \ the older
variable FFI-LIT-B                          \ and the newer

: FFI-APPEND ( str -- )
   {: a:^ u:n :}
   FFI-SIG-U @
   {: at:n :}
   at u + 1 + FFI-SIG-RESERVE
   a  at FFI-SIG  u BYTE-COPY
   32  at u + FFI-SIG  c!
   at u + 1 + FFI-SIG-U ! ;

\ A call into FFI's declaration machine placed at the scan's token A U, which
\ stands in as the token read last until it returns or throws.
: FFI-AT ( R [ R -- S ] str -- S )
   {: a:^ u:n :}
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   a u SRCLOC:TOKEN!
   a SOURCE@ - TOKEN-BYTE !
   [: oa ou SRCLOC:TOKEN! ;] finally ;

: RECORD-FFI-FUNCTION ( str -- )
   {: a:^ u:n :}
   a u FFI-UNCLOSED
   [: [: NEXT-RAW dup 0<> IF 2dup SRCLOC:TOKEN! THEN ;]
      FFI:READ-FUNCTION ;] a u FFI-AT
   {: name:^ nameu:n :}
   name SOURCE@ - {: at:n :}
   at TOKEN-BYTE !
   name nameu NAME-DECLARED
   name nameu at REJECT-DUPLICATE IF FFI:SKIP-FUNCTION EXIT THEN
   0 FFI-SIG-U !
   [: FFI-APPEND ;] FFI:DECLARED-TEXTS
   0 FFI-SIG FFI-SIG-U @ {: sig:^ sigu:n :}
   name nameu at nameu ARM
   name nameu sig sigu DECL-SIGNATURE
   DISARM
   IF name nameu sig sigu at DEF-WORD DEFINED-HERE
   ELSE FFI:SKIP-FUNCTION THEN ;

: FFI-CLOSER? ( str -- bool )
   {: a:^ u:n :}
   a u s" ;FUNCTION" STR=CI ;

: FFI-LITERAL ( str -- )
   {: a:^ u:n :}
   a u num-parse {: v:n flt:bool num:bool :}
   num flt 0= and 0= IF 0 FFI-LIT-N ! EXIT THEN
   FFI-LIT-B @ FFI-LIT-A !  v FFI-LIT-B !
   FFI-LIT-N @ 1+ 2 min FFI-LIT-N ! ;

: FFI-CLAUSE ( str -- )
   {: a:^ u:n :}
   a u TOKEN-TAIL {: t:^ tu:n :}
   FFI-LIT-N @ {: lits:n :}
   t tu s" STRUCT-ARG" STR=CI IF
      lits 2 < IF FFI:SKIP-FUNCTION EXIT THEN
      FFI-LIT-A @ FFI-LIT-B @ [: FFI:SET-STRUCT-ARG ;] a u FFI-AT EXIT
   THEN
   t tu s" STRUCT-RESULT" STR=CI IF
      lits 1 < IF FFI:SKIP-FUNCTION EXIT THEN
      FFI-LIT-B @ [: FFI:SET-STRUCT-RESULT ;] a u FFI-AT EXIT
   THEN
   t tu s" VARIADIC" STR=CI 0= IF EXIT THEN
   lits 1 < IF FFI:SKIP-FUNCTION EXIT THEN
   FFI-LIT-B @ [: FFI:SET-VARIADIC ;] a u FFI-AT ;

\ The library's storage definers are words, not keywords: a package's word of
\ the spelling, or a global one an `undefine` made room for, is the word the
\ load calls. An arm below reads a name only when the record the engine's
\ lookup binds the token to runs the library word's code; any other word of the
\ spelling is TOP-TOKEN's call. The symbol cannot tell: a global word defined
\ after `undefine` keeps the library word's. Each code address is kept as this
\ file loads, before any source can give the spelling to another word (checked
\ code may not take a layout definer's xt: E-UNSAFE). A record index would not
\ do: a prefix rewind keeps code but reuses discarded records. xt! registers
\ these cells for
\ relocation when the verifier is captured.
: LIBRARY-XT! ( str ^n -- )
   {: a:^ u:n cell:^ :}
   a u XREF-FIND {: rec:^ :}
   rec XREF-FOUND? 0= IF
      2 s" verify-source: library word not loaded: " write drop  a u EXIT-STATUS:INTERNAL die
   THEN
   rec XREF-START cell xt! ;

variable LAYOUT-BUFFER-XT s" LAYOUT-BUFFER" LAYOUT-BUFFER-XT LIBRARY-XT!
variable DEFER-LAYOUT-BUFFER-XT s" DEFER-LAYOUT-BUFFER" DEFER-LAYOUT-BUFFER-XT LIBRARY-XT!
variable TYPED-BUFFER-XT s" TYPED-BUFFER" TYPED-BUFFER-XT LIBRARY-XT!
variable TYPED-VARIABLE-XT s" TYPED-VARIABLE" TYPED-VARIABLE-XT LIBRARY-XT!
variable DYNAMIC-BUFFER-XT s" DYNAMIC-BUFFER" DYNAMIC-BUFFER-XT LIBRARY-XT!
variable ARENA-XT s" ARENA" ARENA-XT LIBRARY-XT!
variable HEAP-XT s" HEAP" HEAP-XT LIBRARY-XT!
variable CWORK-BUFFER-XT s" CWORK-BUFFER" CWORK-BUFFER-XT LIBRARY-XT!
variable CWORK-SPAN-XT s" CWORK-SPAN" CWORK-SPAN-XT LIBRARY-XT!
variable CWORK-VARIABLE-XT s" CWORK-VARIABLE" CWORK-VARIABLE-XT LIBRARY-XT!

\ A re-export of the library word is that word: the load binds its alias to the
\ library word's code. An export keeps its source's tail, so only a token whose
\ tail is the word's name, in any case, can bind it. A live export's record
\ runs that code; one this scan's replay made has none, so its symbol is
\ followed to the word its package and tail name, the library word only when
\ that word's newest record still runs its code.
\ Known gap: an alias exported before its source is undefined, redefined or
\ not, is followed to the current global word, so the check can certify a use
\ the load refuses.
: LIBRARY-DEFINER? ( str n str -- bool )
   {: a:^ u:n code:n name:^ nameu:n :}
   a u TOKEN-TAIL name nameu STR=CI 0= IF false EXIT THEN
   a u scope-find drop drop drop {: rec:^ :}
   rec XREF-FOUND? 0= IF false EXIT THEN
   rec XREF-START code = IF true EXIT THEN
   a u FIND-SYM {: sym:n :}
   sym 0= IF false EXIT THEN
   sym SYM-SOURCE {: src:n :}
   src sym = IF false EXIT THEN
   src SYM-IDENTITY drop {: pkg:^ pkgu:n tail:^ tailu:n :}
   tailu 0= IF false EXIT THEN
   pkgu 0<> IF pkg pkgu tail tailu QUAL-SPELLING
   ELSE tail tailu THEN
   XREF-FIND {: word:^ :}
   word XREF-FOUND? 0= IF false EXIT THEN
   word XREF-START code = ;

\ src/lib/task.f loads after this file, so a TASK definer's code is read where
\ its row is reached; Q is its qualified name.
: TASK-DEFINER? ( str str str -- bool )
   {: a:^ u:n q:^ qu:n name:^ nameu:n :}
   a u TOKEN-TAIL name nameu STR=CI 0= IF false EXIT THEN
   q qu XREF-FIND {: rec:^ :}
   rec XREF-FOUND? 0= IF false EXIT THEN
   a u rec XREF-START name nameu LIBRARY-DEFINER? ;

\ A token is a clause only where the load's lookup binds it to the declarer's
\ word (LIBRARY-DEFINER?); operands are literals read right before it.
variable STRUCT-ARG-XT s" STRUCT-ARG" STRUCT-ARG-XT LIBRARY-XT!
variable STRUCT-RESULT-XT s" STRUCT-RESULT" STRUCT-RESULT-XT LIBRARY-XT!
variable VARIADIC-XT s" VARIADIC" VARIADIC-XT LIBRARY-XT!

: FFI-CLAUSE? ( str -- bool )
   {: a:^ u:n :}
   a u STRUCT-ARG-XT @ s" STRUCT-ARG" LIBRARY-DEFINER? IF true EXIT THEN
   a u STRUCT-RESULT-XT @ s" STRUCT-RESULT" LIBRARY-DEFINER? IF true EXIT THEN
   a u VARIADIC-XT @ s" VARIADIC" LIBRARY-DEFINER? ;

: RECORD-DEFINER? ( str -- bool ) {: a:^ u:n :}
   \ A program's own word spelled like a declaration is not one.
   a u TOP-BINDING nip nip BINDING-ID {: id:n :}
   id CHECKER-OWNER-ABI:BINDING-DECLARATION-ID =
   id CHECKER-OWNER-ABI:BINDING-TOP-ONLY-ID = or {: decl:bool :}
   a u s" package" STR=CI IF RECORD-PACKAGE 0 0= EXIT THEN
   a u s" public" STR=CI IF RECORD-PUBLIC 0 0= EXIT THEN
   a u s" private" STR=CI IF RECORD-PRIVATE 0 0= EXIT THEN
   a u s" global" STR=CI IF RECORD-GLOBAL 0 0= EXIT THEN
   a u s" host" STR=CI IF 0 0= EXIT THEN
   a u s" target" STR=CI IF 0 0= EXIT THEN
   a u s" ;package" STR=CI IF RECORD-END-PACKAGE 0 0= EXIT THEN
   a u s" seal-package" STR=CI IF SEAL-ROW 0 0= EXIT THEN
   a u s" using" STR=CI IF RECORD-USING 0 0= EXIT THEN
   a u s" ;using" STR=CI IF a u RECORD-END-USING 0 0= EXIT THEN
   a u s" split-str" STR=CI IF
      CHECKER-SPLIT-STR @ 0= IF
         a u INTERPRETER:SPLIT-STR-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
      0 0= EXIT
   THEN
   a u s" deftype" STR=CI decl and IF RECORD-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" STR=CI decl and IF RECORD-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" STR=CI decl and IF
      [: RECORD-VALUE-RECORD ;] VALUE-RECORD-OUTCOME 0 0= EXIT THEN
   a u s" begin-structure" STR=CI IF RECORD-STRUCTURE 0 0= EXIT THEN
   a u s" structure" STR=CI decl and IF RECORD-STRUCTURE-DECL 0 0= EXIT THEN
   a u s" mutual" STR=CI decl and IF RECORD-MUTUAL 0 0= EXIT THEN
   a u s" newtype" STR=CI decl and IF RECORD-NEWTYPE 0 0= EXIT THEN
   a u s" sumtype" STR=CI decl and IF
      [: RECORD-SUMTYPE ;] LEGACY-TYPE-OUTCOME 0 0= EXIT THEN
   a u s" enum" STR=CI decl and IF RECORD-ENUM 0 0= EXIT THEN
   a u LAYOUT-BUFFER-XT @ s" LAYOUT-BUFFER" LIBRARY-DEFINER? IF
      [: RECORD-LAYOUT-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u DEFER-LAYOUT-BUFFER-XT @ s" DEFER-LAYOUT-BUFFER" LIBRARY-DEFINER? IF
      [: RECORD-DEFER-LAYOUT-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u TYPED-BUFFER-XT @ s" TYPED-BUFFER" LIBRARY-DEFINER? IF
      [: RECORD-TYPED-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u TYPED-VARIABLE-XT @ s" TYPED-VARIABLE" LIBRARY-DEFINER? IF
      [: RECORD-TYPED-VARIABLE ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" TASK:+USERS" s" +USERS" TASK-DEFINER? IF
      [: RECORD-BYTE-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" TASK:+USER" s" +USER" TASK-DEFINER? IF
      [: RECORD-SLOT ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u DYNAMIC-BUFFER-XT @ s" DYNAMIC-BUFFER" LIBRARY-DEFINER? IF
      [: RECORD-DYNAMIC-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u ARENA-XT @ s" ARENA" LIBRARY-DEFINER? IF
      [: RECORD-ARENA ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u HEAP-XT @ s" HEAP" LIBRARY-DEFINER? IF
      [: RECORD-HEAP ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u CWORK-BUFFER-XT @ s" CWORK-BUFFER" LIBRARY-DEFINER? IF
      [: RECORD-BYTE-BUFFER ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u CWORK-SPAN-XT @ s" CWORK-SPAN" LIBRARY-DEFINER? IF
      [: RECORD-CWORK-SPAN ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u CWORK-VARIABLE-XT @ s" CWORK-VARIABLE" LIBRARY-DEFINER? IF
      [: RECORD-CWORK-VARIABLE ;] STORAGE-OUTCOME 0 0= EXIT THEN
   a u s" constant" STR=CI IF s" -- a" DEF-CONSTANT RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" create" STR=CI IF s" -- ^a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" STR=CI IF s" -- ^a" DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" STR=CI IF TRUST-DEFER 0 0= EXIT THEN
   a u s" EPRIM:" STR=CI IF RECORD-PRIM-ROW 0 0= EXIT THEN
   a u s" EPPRIM:" STR=CI IF RECORD-PPRIM 0 0= EXIT THEN
   a u s" trusted:" STR=CI IF TRUSTED-DEFINITION 0 0= EXIT THEN
   a u s" cast:" STR=CI IF
      [: CAST-DECLARATION ;] IDENTITY-OUTCOME 0 0= EXIT THEN
   a u s" linear:" STR=CI IF
      [: LINEAR-DECLARATION ;] IDENTITY-OUTCOME 0 0= EXIT THEN
   a u s" undefine" STR=CI IF TICK-CONTEXT-UNKNOWN UNDEFINE-WORD 0 0= EXIT THEN
   a u s" generates:" STR=CI decl and IF RECORD-GENERATES 0 0= EXIT THEN
   a u s" FUNCTION:" STR=CI decl and IF a u RECORD-FFI-FUNCTION 0 0= EXIT THEN
   a u FFI-CLAUSE? IF a u FFI-CLAUSE 0 0= EXIT THEN
   a u s" immediate" STR=CI IF
      TICK-CONTEXT-UNKNOWN  NEWEST-SYM @ CHECKER-IMMEDIATE 0 0= EXIT
   THEN
   a u s" export" STR=CI IF RECORD-EXPORT 0 0= EXIT THEN
   \ A word that makes definitions when it runs (`;FUNCTION`) defines words this
   \ scan never reads: the checker marks the statement's wordlist and leaves a
   \ later name nothing resolves to the run. It marks even when an arm below
   \ records what a `generates:` row declares (RECORD-GENERATES); the mark
   \ covers what no row declares. A `;FUNCTION` is taken here; any other
   \ undeclared renderer is deferred at its token (RENDER-GAP), by BOUND-CALL
   \ when a definer arm below takes it, else by TOP-TOKEN.
   a u NOMINAL-MARK
   a u CTL-RENDERS RENDERS-MARK? {: renders:bool :}
   renders a u FFI-CLOSER? and IF
      [: FFI:PLAN-FUNCTION ;] a u FFI-AT 0 0= EXIT
   THEN
   \ A declaration no arm above reads and that renders no source (`evaluate`
   \ renders, as `;FUNCTION` does) leaves the rest of its statement to the run.
   decl renders 0= and IF a u DW-PARSER TOP-OPAQUE 0 0= EXIT THEN
   \ A definer learned from a `does>` definition or a `generates:` row earlier
   \ in the closure. The created word is the next token, as for `constant` (the
   \ definer's own arguments precede it); the effect is the clause's or the
   \ row's, under the storage definers' raw seal.
   a u DEFINER-EFFECT dup 0<> IF
      a u renders BOUND-CALL
      TICK-CONTEXT-UNKNOWN DEF-STORAGE RAW-TRUST-NEXT 0 0= EXIT
   THEN 2drop
   \ A definer this pre-pass never read: one compiled in the checking process,
   \ whose clause the checker certified and kept, or a wrapper of one the
   \ checker's body walk recorded. FIND-SYM resolves it, so the qualified and
   \ the bare-under-`using` spelling reach the one row.
   a u FIND-SYM {: dsym:n :}
   dsym CREATES-SYM? 0= IF 0 0= 0= EXIT THEN
   a u renders BOUND-CALL
   dsym CREATED-TRUST-NEXT? IF TICK-CONTEXT-UNKNOWN 0 0= EXIT THEN
   0 0= 0= ;

: COLON-DEFINED ( -- )
   TOP-CUR-A @ TOP-CUR-U @  DEF-NAME-A @ DEF-NAME-U @  DEF-SIG-A @ DEF-SIG-U @
   DEF-NAME-BYTE @ dup DEF-NAME-U @ + DEF-WORD DEFINED ;

: SKIP-DEFINITION ( -- )
   SCAN-SIG drop 2drop
   BODY-PREV-CLEAR
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      SKIP-DEF-TOKEN
   AGAIN ;

: VERIFY-DEFINITION ( -- )
   TICK-DEF-LATCH
   BODY-RESET
   NAME-TOKEN TOKEN-U !  TOKEN-A !
   TOKEN-U @ 0= if NAME-STOP then
   TOP-CUR-A @ TOP-CUR-U @ TOKEN-A @ TOKEN-U @ REJECTION:NOT-A-NAME
   DEF-NAME!
   DEF-NAME-A @ DEF-NAME-U @ NAME-DECLARED
   DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ REJECT-DUPLICATE IF SKIP-DEFINITION EXIT THEN
   WRAP-RESET
   BODY-PREV-CLEAR
   LOCALS-RESET
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE DEF-SIG-U !  DEF-SIG-A !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF
         DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ false REFUSE-SHAPE IF EXIT THEN
         VERIFY-NAMED-BODY dup REPORT-DEFERRED dup -1 = IF VERIFY-WRAPPER THEN
         dup TALLY
         TICK-REMAINDER @ 0= IF
            -1 = INDEX-ON @ 0= or IF COLON-DEFINED THEN
         ELSE drop THEN EXIT
      THEN
      LOCAL-TOKEN? 0= IF
         TOKEN-A @ TOKEN-U @ s" does>" STR=CI IF
            DEF-NAME-A @ DEF-NAME-U @ DEF-NAME-BYTE @ true REFUSE-SHAPE IF
               SKIP-DEFINITION EXIT
            THEN
            VERIFY-DOES IF
               DOES-ACCEPTED @ INDEX-ON @ 0= or IF COLON-DEFINED THEN
            THEN
            TICK-REMAINDER @ IF 2 TALLY THEN EXIT
         THEN
         TOKEN-A @ TOKEN-U @ WRAP-TOKEN
      THEN
      TICK-BODY-STEP
      APPEND-BODY-TOKEN
   AGAIN ;

variable TOP-DEFER                               \ a stretch is open
PTR-VARIABLE TOP-DEFER-A  variable TOP-DEFER-U   \ its opener while its report is due, else 0

: TOP-CLOSE ( -- )
   0 TOP-DEFER !  0 TOP-DEFER-U ! ;

\ No cause: TOP-VERDICT answers 3 both for a name only rendered text may define
\ and for a word whose effect names a type that may arrive.
: TOP-DUE ( -- )
   TOP-DEFER-U @ 0= IF EXIT THEN
   TOP-DEFER-A @ TOP-DEFER-U @ 0 DEFER-AT
   0 TOP-DEFER-U ! ;

\ Reads past the operand body keyword a u reads, as a body does; the keyword
\ has its record, so the source ending inside the operand adds none.
: SKIP-OPERAND ( str -- ) {: a:^ u:n :}
   a u s" {:" CORE-STR= IF
      BEGIN NEXT-RAW dup 0= IF 2drop EXIT THEN s" :}" CORE-STR= UNTIL EXIT
   THEN
   a u s" [type]" STR=CI IF
      0 BEGIN
         SCAN-I @ TOKEN-FROM TYPE-SPAN-END? IF drop EXIT THEN
         NEXT-RAW dup 0= IF 2drop drop EXIT THEN
         CHECKER-TYPE-SPAN-STEP
      UNTIL drop EXIT
   THEN
   a u s" is" STR=CI IF NEXT-BODY 2drop EXIT THEN
   a u BODY-PARSER? IF NEXT-RAW 2drop THEN ;

\ Every answer but a binding reports the token itself, so BOUND-CALL reports a
\ gap only where no other report is, and a token has one packet at most.
: TOP-RESOLVE ( str bool bool -- )
   {: a:^ u:n runs:bool renders:bool :}
   a u runs TOP-VERDICT {: v:n :}
   v -1 = IF
      runs IF a u renders BOUND-CALL THEN
      EXIT
   THEN
   v 2 = IF
      a u ROOT-MOVE? IF a u DW-ROOTS TOP-OPAQUE ELSE a u TOP-PARSER THEN
      EXIT
   THEN
   \ A stretch holds names rendered text may define; an undefined name defines
   \ nothing, so the names after it are still resolved and rejected one by one.
   v 0= IF a TOP-REFUSED !  runs IF a u SKIP-OPERAND THEN  EXIT THEN
   -1 TOP-DEFER !
   a TOP-DEFER-A !  u TOP-DEFER-U ! ;

\ The checker offers the spellings that bind at the cursor's token before it is
\ consumed, unless a stretch left to the run holds it.
: CSR-TOP ( -- )
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-HIT? 0= IF EXIT THEN
   CSR-STATE @ CSR-PART = IF CSR-AT @ CSR-TOK @ - ELSE 0 THEN {: n:n :}
   CSR-DONE CSR-STATE !
   TOP-DEFER @ 0<> IF EXIT THEN
   SOURCE@ CSR-TOK @ +  n  CHECKER-OWNER-ABI:VISIBLE-TOP
   ['] CSR-VISIT
   NCOMP-DISPATCH:DECL-VERIFY-EACH-VISIBLE-OFF OWNER-XT VISIBLE-ACTION execute ;

: TOP-OPERAND ( str -- )
   {: a:^ u:n :}
   OPERAND {: o:^ ou:n :}
   a u CHAR-KEYWORD? IF EXIT THEN
   CSR-TOP
   COMPOSE-ON @ INDEX-ON @ or 0= IF EXIT THEN
   \ A mover's token may be executed, so the tick ends the walk as a call would.
   o ou ROOT-MOVE? IF o ou DW-ROOTS TOP-OPAQUE EXIT THEN
   TOP-DEFER @ 0<> IF EXIT THEN
   o ou false false TOP-RESOLVE ;

\ A word that may define words this scan never reads (it renders source, as
\ `;FUNCTION` and `evaluate` do, calls `create`, or is deferred) has the
\ checker mark the statement's wordlist, with or without composition. Only here
\ does a word that calls `create` mark: a definer arm that takes the statement
\ records the name it makes. An undeclared renderer is deferred at its token
\ even when no later name uses it, inside an open stretch too.
: TOP-TOKEN ( str -- )
   {: a:^ u:n :}
   a u num-parse nip nip IF EXIT THEN
   a u s" [package]" STR=CI IF EXIT THEN   \ pushes a name, calls nothing
   a u STRING-OPENER? 0= IF TICK-CONTEXT-UNKNOWN THEN
   a u NOMINAL-MARK
   a u CTL-RENDERS RENDERS-MARK? {: renders:bool :}
   renders 0= IF a u MARK-UNSEEN RENDERS-MARK? drop THEN
   \ Index safety still needs every call after a rejected declaration: an open
   \ diagnostic stretch must not hide a call that can change earlier bindings.
   COMPOSE-ON @ 0= TOP-DEFER @ 0<> or INDEX-ON @ 0= and IF
      \ A composition inside an open stretch still follows the requires after it.
      COMPOSE-ON @ IF a u ROOT-MOVE? IF a u DW-ROOTS TOP-OPAQUE EXIT THEN THEN
      renders IF a u RENDER-GAP THEN
      EXIT
   THEN
   a u true renders TOP-RESOLVE ;

\ A declaration a definer of the table above reads closes the stretch. One the
\ checker rejected while it was read (where rejections do not throw) opens an
\ unreported stretch instead: the scan recovers at the next definer or `:`, so
\ the tokens of a broken declaration are not reported as names.
: TOP-DEFINER? ( str -- bool )
   {: a:^ u:n :}
   REJECTION:TALLY @ {: before:n :}
   a u TICK-NATIVE-DEFINER? {: native:bool :}
   \ A library definer is a replaceable dictionary call, not structural syntax.
   INDEX-ON @ native 0= and IF false EXIT THEN
   TOKEN-BYTE @ {: at:n :}
   a u TOP-BINDING nip {: sym:n ctl:n :}
   a u s" host" STR=CI  a u s" target" STR=CI or {: section:bool :}
   \ A refused name goes on as an ordinary word, refused as one.
   ctl CHECKER-OWNER-ABI:BINDING-REFUSED and 0<>
   section and IF false EXIT THEN
   section sym 0<> and IF a u INTERPRETER:SECTION-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP THEN
   a u RECORD-DEFINER? 0= IF 0 0= 0= EXIT THEN
   native 0= IF TICK-CONTEXT-UNKNOWN THEN
   native 0= IF a u at sym ENGINE-ORIGIN-USE THEN
   TOP-CLOSE
   REJECTION:TALLY @ before <> IF -1 TOP-DEFER ! THEN
   0 0= ;

\ `kernel:` is the engine's synonym for `:`.
: COLON? ( str -- bool ) {: a:^ u:n :}
   a u s" :" CORE-STR=  a u s" kernel:" STR=CI or ;

: FLAG-TOP ( -- ^bool )
   FLAG-N @ 1 - FLAGS ;

: FLAG-PUSH ( bool -- )
   FLAG-N @ FLAG-CAP = IF drop 0 FLAG-N ! EXIT THEN
   FLAG-N @ FLAGS !  FLAG-N @ 1 + FLAG-N ! ;

: FLAG-STEP ( str -- ) {: a:^ u:n :}
   TOP-DEFER @ 0<> IF 0 FLAG-N ! EXIT THEN
   a u TARGET-GUARD IF FLAG-PUSH EXIT THEN drop
   a u s" 0=" STR=CI IF a u CORE-OP? ELSE false THEN IF
      FLAG-N @ 0= IF EXIT THEN
      FLAG-TOP @ 0= FLAG-TOP ! EXIT
   THEN
   0 FLAG-N ! ;

: SKIP-ARM ( -- bool )
   1 BEGIN dup 0 > WHILE
      NEXT-RAW dup 0= IF 2drop drop  false EXIT THEN
      2dup s" [if]" STR=CI IF 2drop 1 + ELSE
         s" [then]" STR=CI IF 1 - THEN
      THEN
   REPEAT drop
   true ;

\ A top-level `[if]` whose flag a target predicate settles, under any `0=`s,
\ reads only the text that flag chooses.
: KNOWN-IF? ( str -- bool ) {: a:^ u:n :}
   STR-LAST-KIND @ 0<> IF 0 FLAG-N ! THEN
   a u s" [if]" STR=CI 0= FLAG-N @ 1 <> or IF false EXIT THEN
   0 FLAGS @ IF true EXIT THEN
   SKIP-ARM 0= IF a u 0 TOP-OPAQUE THEN
   true ;

\ A parsing keyword and its operand are one value on the interpret stack, and
\ the keyword stays the token before whatever follows: `char 0 TYPED-BUFFER B n`
\ hands the count reader `char`, which names the count's word.
: SCAN-TOP ( ^u8 n -- )
   CSR-TOP
   2dup KNOWN-IF? IF 2drop ELSE
   2dup TOP-PARSER? IF TOP-OPERAND ELSE
   2dup COLON? IF 2drop VERIFY-DEFINITION TOP-CLOSE ELSE
   2dup COMPOSE-TOP? IF 2drop TOP-CLOSE ELSE
   2dup TOP-DEFINER? IF 2drop ELSE TOP-TOKEN THEN THEN THEN THEN THEN
   NULL-PTR 0 NULL-PTR 0 SRCLOC:LITERAL!   \ a literal's record lasts the token after it
   TOP-CUR-A @ TOP-CUR-U @ FFI-LITERAL
   TOP-CUR-A @ TOP-CUR-U @ FLAG-STEP ;     \ after any file this token loaded

\ A record no source places is placed at the top-level token.
: VERIFY-SOURCE ( -- )
   SCAN-RESET
   SOURCE@ SOURCE-U @ SPLIT-STR-FILE? CHECKER-SPLIT-STR !
   NULL-PTR TOP-PREV-A !  0 TOP-PREV-U !  NULL-PTR TOP-REFUSED !
   0 SHADOWED !  0 RETIRED !  0 NEWEST-SYM !
   TOP-CLOSE
   BEGIN
      NEXT-SCAN dup 0 > TICK-REMAINDER @ 0= and WHILE
      TOP-DUE
      2dup TOP-CUR-U ! TOP-CUR-A !
      [: TOP-CUR-A @ TOP-CUR-U @ SCAN-TOP ;] REJECTION:READING
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT 2drop
   TICK-REMAINDER @ IF EXIT THEN
   CSR-TOP                                      \ a cursor in the blanks the source ends with
   TOP-DUE
   NULL-PTR 0 FFI-UNCLOSED ;

: SOURCE-ARM ( -- )
   SOURCE@ SOURCE-U @ 1 1 0 REJECTION:SOURCE! ;

\ Nested files share the checker window but not the scanner cursor. The source,
\ token and open-stretch context, the loader words the file replaced, and
\ whether the file is the cursor's subject belong to the caller; declarations,
\ learned definers and whether a stretch was reported belong to the whole
\ composition. On return or throw the checker is re-armed with the caller's
\ bytes, or disarmed when the subject's scan ends, so no nested file's bytes,
\ which its loader frame releases, stay armed.
: COMPOSE-FILE-SCAN ( str str -- )
   {: src:^ srcu:n path:^ pathu:n :}
   path pathu COMPOSE-DIAG$ {: diag:^ diagu:n :}
   SOURCE-A @ SOURCE-U @ SCAN-I @
   TOP-PREV-A @ TOP-PREV-U @ TOP-CUR-A @ TOP-CUR-U @
   TOP-DEFER @ TOP-DEFER-A @ TOP-DEFER-U @ TOP-REFUSED @
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @
   VISIT-CUR @ CSR-SUBJ @
   SHADOWED @ RETIRED @ CHECKER-SPLIT-STR @
   {: olda:^ oldu:n oldi:n
      oldprev:^ oldprevu:n oldcur:^ oldcuru:n
      olddefer:n olddefa:^ olddefu:n oldrefused:^
      oldpath:^ oldpathu:n
      oldvisit:n oldsubj:n
      oldshadowed:n oldretired:n oldsplit:n :}
   src srcu SOURCE!
   path COMPOSE-CUR-PATH-A !  pathu COMPOSE-CUR-PATH-U !
   path pathu VISIT-OPEN
   VISIT-CUR @ VISIT-FIRST @ = CSR-SUBJ !
   \ ON-FILE runs while the requesting file stays armed, so a reader policy's
   \ rejection of the acquired bytes stands at the loader that loads them.
   [: FILE$ SOURCE@ SOURCE-U @ ON-FILE ;] catch
   dup 0= IF
      drop  SOURCE-ARM  diag diagu REJECTION:FILE!
      [: VERIFY-SOURCE ;] catch
   THEN
   {: rc:n :}
   DISARM
   oldvisit VISIT-CUR !
   oldsubj CSR-SUBJ !
   olda SOURCE-A !  oldu SOURCE-U !  oldi SCAN-I !
   oldprev TOP-PREV-A !  oldprevu TOP-PREV-U !
   oldcur TOP-CUR-A !  oldcuru TOP-CUR-U !
   olddefer TOP-DEFER !  olddefa TOP-DEFER-A !
   olddefu TOP-DEFER-U !  oldrefused TOP-REFUSED !
   oldpath COMPOSE-CUR-PATH-A !  oldpathu COMPOSE-CUR-PATH-U !
   oldshadowed SHADOWED !  oldretired RETIRED !  oldsplit CHECKER-SPLIT-STR !
   oldpathu 0 > IF
      oldpath oldpathu COMPOSE-DIAG$ REJECTION:FILE!
      SOURCE-ARM
   ELSE
      REJECTION:SOURCE-OFF
   THEN
   rc 0<> IF rc throw THEN ;

: COMPOSE-INIT ( -- )
   [: COMPOSE-FILE-SCAN ;] is COMPOSE-FILE ;

COMPOSE-INIT

\ The owner's start opens the checker's engine overlay, so every name the
\ action replays binds through the engine dictionary; the done closes it and
\ restores the caller's scope on the clean and the throwing path alike. With no
\ overlay open before, the done also takes back every checker row the action
\ recorded; inside an open one the rows stay in the caller's scope.
: RUN-IN-SCOPE ( [ -- ] -- )
   CHECKER-SPLIT-STR @ {: split:n :}
   TICK-CONTEXT-RESET
   false TICK-REMAINDER !  FFI:ABANDON-FUNCTION
   0 CERTIFIED-N !  0 DEFERRED-N !  0 DEFER-SEEN !  SOURCE-SEALS-CLEAR
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF OWNER-XT VERIFIER-ACTION execute
   catch {: rc:n :}
   SOURCE-SEALS-CLEAR
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF OWNER-XT VERIFIER-ACTION execute
   split CHECKER-SPLIT-STR !
   rc 0<> IF rc throw THEN ;

: VERIFY-ARMED ( -- )
   SOURCE-ARM
   [: VERIFY-SOURCE ;] catch {: rc:n :}
   REJECTION:SOURCE-OFF
   rc 0<> IF rc throw THEN ;

: RUN ( -- )
   [: VERIFY-ARMED ;] RUN-IN-SCOPE ;

: COMPOSE-SUBJECT ( -- )
   COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ COMPOSE-FILE ;

: RUN-COMPOSE ( -- )
   [: COMPOSE-SUBJECT ;] RUN-IN-SCOPE ;

: COMPOSE-WITH-USES ( -- )
   ['] USE-SEEN ['] RUN-COMPOSE WITH-USES ;

variable CAND-VERDICT

TYPED-VARIABLE THEN-ACTION [ -- nominal ]

: VERIFY-THEN ( -- )
   VERIFY-ARMED
   THEN-ACTION @ execute ;

\ A composition reads each file a loader loads itself, once COMPOSE-MISSING has
\ rejected a missing one: a file the system does not open, or does not read to
\ its end, is the loader's rejection at LOAD-PLACE, FAULT-TARGET$ naming the
\ file as resolved. Any other rejection, memory's among them, stays itself. The
\ loader rejects a resolved path past PATH-CAP before reading, so the path fits
\ both buffers.
create READ-PATHZ PATH-CAP 1+ allot
DYNAMIC-BUFFER READ-BUF u8                    \ the file's bytes, READ-U of them
variable READ-U
variable READ-FD
variable READ-SIZE
variable READ-MSEC
variable READ-MNS
variable READ-CSEC
variable READ-CNS
variable READ-PATH-U
variable READ-META-VALID
65536 constant READ-STEP

\ PATH, resolved, will not open, with errno E.
: COMPOSE-UNREADABLE ( str n -- ) {: path:^ pathu:n e:n :}
   path pathu FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ path pathu e SOURCE-ROOT:UNREADABLE-SOURCE ;

\ PATH, resolved, changed while it was read.
: COMPOSE-CHANGED ( str -- ) {: path:^ pathu:n :}
   path pathu FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ path pathu SOURCE-ROOT:CHANGED-SOURCE ;

\ Reads the open file READ-PATHZ names to its end; a read the system fails
\ rejects the file with its errno. A read stops past SIZE, the stamp's size.
: READ-FILL ( n -- ) {: size:n :}
   0 READ-U !
   BEGIN
      READ-U @ READ-STEP + READ-BUF-RESERVE
      READ-FD @  READ-U @ READ-BUF  READ-STEP  read
      dup 0 < IF READ-PATHZ READ-PATH-U @ FS:ERRNO COMPOSE-UNREADABLE THEN
      dup 0 >
   WHILE
      READ-U @ + READ-U !
      READ-U @ size > IF READ-PATHZ READ-PATH-U @ COMPOSE-CHANGED THEN
   REPEAT drop ;

\ Rejects PATH, a stream no stamp names, with the loader's record, placed at the
\ loader that loads it.
: REFUSE-SOURCE-READ ( str -- ) {: path:^ pathu:n :}
   path pathu FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ path pathu SOURCE-ROOT:UNSTAMPED-SOURCE ;

\ A stream (a FIFO, device or socket) is rejected before its open, which would
\ block on a FIFO; a changed file once it outgrows its size, else after the read.
: QUIET-READ ( str -- str )
   {: path:^ pathu:n :}
   0 READ-META-VALID !
   path pathu READ-PATHZ PATH-CAP 1+ PATHZ
   pathu READ-PATH-U !
   path pathu FS:TRY-FILE-META
   {: size:n msec:n mns:n csec:n cns:n valid:bool :}
   READ-PATHZ COMPILE-READS:STAMP COMPILE-READS:STABLE? 0= IF path pathu REFUSE-SOURCE-READ THEN
   READ-PATHZ open-rd {: fd:n :}
   fd 0 < IF path pathu FS:ERRNO COMPOSE-UNREADABLE THEN
   fd READ-FD !
   [: size READ-FILL ;] catch {: rc:n :}
   fd close
   rc 0<> IF rc throw THEN
   path pathu FS:TRY-FILE-META
   {: after:n amsec:n amns:n acsec:n acns:n still:bool :}
   size after = msec amsec = and mns amns = and
   csec acsec = and cns acns = and valid and still and 0= IF path pathu COMPOSE-CHANGED THEN
   size READ-SIZE ! msec READ-MSEC ! mns READ-MNS !
   csec READ-CSEC ! cns READ-CNS !
   true READ-META-VALID !
   0 READ-BUF READ-U @ ;

public

\ The stamp of the disk bytes the quiet reader just handed to SOURCE-INPUT.
\ ON-FILE runs before another acquisition can replace it.
: QUIET-READ-META ( str -- n n n n n bool )
   {: path:^ pathu:n :}
   READ-META-VALID @ 0<> pathu READ-PATH-U @ = and
   path pathu READ-PATHZ pathu CORE-STR= and 0= IF 0 0 0 0 0 false EXIT THEN
   READ-SIZE @ READ-MSEC @ READ-MNS @ READ-CSEC @ READ-CNS @ true ;

private

create ENGINE-PATH PATH-CAP allot

: ENGINE-FULL-PATH ( str -- str bool )
   {: rel:^ relu:n :}
   SOURCE-ROOT:ENGINE$ {: root:^ rootu:n :}
   rootu relu + 1+ PATH-CAP > if NULL$ false exit then
   root ENGINE-PATH rootu BYTE-COPY
   47 ENGINE-PATH rootu + c!
   rel ENGINE-PATH rootu 1+ + relu BYTE-COPY
   ENGINE-PATH rootu relu + 1+ true ;

: ENGINE-TARGET-READ ( n -- str n n bool )
   ENGINE-ORIGIN:TARGET
   {: rel:^ relu:n at:n end:n name:^ nameu:n found:bool :}
   found 0= if NULL$ 0 0 false exit then
   rel relu ENGINE-FULL-PATH {: path:^ pathu:n fits:bool :}
   path pathu at end fits ;

: ENGINE-TARGET-INSTALL ( -- ) ['] ENGINE-TARGET-READ is ENGINE-TARGET ;
ENGINE-TARGET-INSTALL

public

\ ACTION runs while both checker scopes still own the declaration symbols; a
\ completed multi-error scan keeps surviving declarations. A thrown scan skips
\ the action and propagates after both scopes close.
: SOURCE-INDEX-BUF-IN-SCOPE ( str [ -- ] -- )
   THEN-ACTION !
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   true INDEX-ON !
   [: [: VERIFY-THEN ;] RUN-IN-SCOPE ;] catch {: rc:n :}
   false INDEX-ON !
   CHECKER-CANDIDATE-SCOPE-DONE
   rc 0<> IF rc throw THEN ;

\ A framed verifier child makes each neutral scope stand for an independent
\ child. The previous scope has retired every declaration before this reset.
: SOURCE-RESET-ISOLATED-VISITS ( -- )
   0 VISIT-NEXT ! ;

\ The caller says whether this run's deferrals are reported as packets.
\ DEFERRED? reports coverage for the last run whether or not packets were asked
\ for; RUN-IN-SCOPE clears that fact before each run.
: REPORT-DEFERRALS ( bool -- )
   bool>n DEFER-REPORT !  0 DEFER-SEEN ! ;

: DEFERRED? ( -- bool )
   DEFER-SEEN @ 0<> ;

\ The colon definitions the last run certified, and those it deferred to the
\ run, up to where it stopped.
: CENSUS ( -- n n )
   CERTIFIED-N @ DEFERRED-N @ ;

\ The next composition's completion cursor, subject byte N: the spellings its
\ place offers go to ON-CANDIDATE. A negative N is none, and the composition's
\ end drops it.
: CURSOR! ( n -- )
   dup CSR-AT !
   0 < IF CSR-DONE ELSE CSR-WAIT THEN CSR-STATE ! ;

private

\ The public word a caller composes through: the entry one for entry text.
: COMPOSE-WORD$ ( bool -- str )
   IF s" VERIFY:SOURCE-COMPOSE-ENTRY-IN-SCOPE" EXIT THEN
   s" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE" ;

\ The record of a composition called while another is open, at the statement
\ the open one is in; entry says which public word was called.
: COMPOSE-NESTED ( bool -- ) {: entry:bool :}
   s" E-VERIFY-WINDOW" REJECTION:OPEN
   96 REJECTION:EMIT1  entry COMPOSE-WORD$ REJECTION:EMIT-STR
   s" ` ran while a source composition is open" REJECTION:EMIT-STR
   s" the open composition is here" STATEMENT-AT
   REJECTION:HELP
   s" compose one source at a time: let the open composition end first"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The record of a path or label a u, which a composition cannot hold. It
\ stands at a u where a source holds it, else at the token the engine read last.
: COMPOSE-PATH-STOP ( str -- ) {: a:^ u:n :}
   ENTRY-TEXT @ COMPOSE-WORD$ u PATH-CAP REJECTION:PATH-RANGE
   a REJECTION:PLACES? IF a u ELSE SRCLOC:TOKEN$ THEN REJECTION:AT
   u 0= IF s" empty path" ELSE s" too long" THEN REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass a path of 1 to " REJECTION:EMIT-STR  PATH-CAP REJECTION:EMIT-U
   s"  bytes" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The registry suffix and the caller's checker scope survive the composition's
\ success and throw.
: SOURCE-COMPOSE-LABELED-IN-SCOPE ( str str str -- )
   {: src:^ srcu:n path:^ pathu:n label:^ labelu:n :}
   pathu 0= pathu PATH-CAP > or IF path pathu COMPOSE-PATH-STOP THEN
   labelu PATH-CAP > IF label labelu COMPOSE-PATH-STOP THEN
   path COMPOSE-SUBJ-PATH pathu BYTE-COPY
   pathu COMPOSE-SUBJ-PATH-U !
   label COMPOSE-SUBJ-LABEL labelu BYTE-COPY
   labelu COMPOSE-SUBJ-LABEL-U !
   src COMPOSE-SUBJ-A !  srcu COMPOSE-SUBJ-U !
   NULL-PTR COMPOSE-CUR-PATH-A !  0 COMPOSE-CUR-PATH-U !
   0 PRIM-OPEN-READY !
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

: COMPOSE-QUIET ( str str str bool -- )
   {: src:^ srcu:n path:^ pathu:n label:^ labelu:n entry:bool :}
   COMPOSE-ON @ 0<> IF entry COMPOSE-NESTED THEN
   0 FAULT-U !  0 DISCOVERY-STOPPED !
   [: SOURCE-ROOT:CANON-OS ;] [: QUIET-READ ;] SOURCE-INPUT:USE
   entry ENTRY-TEXT !
   [: src srcu path pathu label labelu SOURCE-COMPOSE-LABELED-IN-SCOPE ;]
   catch {: rc:n :}
   false ENTRY-TEXT !
   SOURCE-INPUT:RESET
   rc 0<> IF rc throw THEN ;
public

\ Verify src srcu, the bytes supplied for path, composing each file a loader
\ loads under the caller's SOURCE-ROOT owner; its records name label, and a
\ bad loader form is rejected after the packets made before it.
: SOURCE-COMPOSE-QUIET-IN-SCOPE ( ^u8 n ^u8 n ^u8 n -- )
   false COMPOSE-QUIET ;

\ SOURCE-COMPOSE-QUIET-IN-SCOPE of entry text, the bytes the load's command
\ line makes of its files: no file holds them, so their records name
\ REJECTION:NO-FILE$ and a load they make stands nowhere.
: SOURCE-COMPOSE-ENTRY-IN-SCOPE ( ^u8 n ^u8 n -- )
   REJECTION:NO-FILE$ true COMPOSE-QUIET ;

\ The file the last composition could not read, as it was resolved, or empty.
\ The string is borrowed until the next composition.
: FAULT-TARGET$ ( -- str )
   FAULT-TARGET FAULT-U @ ;

: SOURCE-BUF-IN-SCOPE ( ^u8 n -- )
   SOURCE!
   RUN ;

\ Verify one supplied source, then run ACTION in the same scope. A name the
\ source declares binds through the record the checker's overlay publishes,
\ which this scope's close takes back unless a neutral checker scope around it
\ holds the overlay open, so a caller reading what the scan recorded by name
\ reads it here. No second verifier scope opens inside this one
\ (E-VERIFY-WINDOW). A scan that throws runs no action.
: SOURCE-BUF-THEN-IN-SCOPE ( ^u8 n [ -- nominal ] -- )
   THEN-ACTION !
   SOURCE!
   [: VERIFY-THEN ;] RUN-IN-SCOPE ;

\ What the certify path says about one candidate definition, `NAME ( effect )
\ body`: -1 certified, 0 refused, 1 uncheckable. A name this scanner
\ registered has no engine record, so a live candidate cannot bind it; here it
\ binds where the scan recorded it.
: CANDIDATE-IN-SCOPE ( str -- n )
   {: a:^ u:n :}
   [: a u CHECK-QUIET-CANDIDATE! CAND-VERDICT ! ;] RUN-IN-SCOPE
   CAND-VERDICT @ ;

\ Verify a source in a candidate scope of its own: what it defines, and the
\ definer rows it learns, go when CHECKER-CANDIDATE-SCOPE-DONE rewinds the scope.
: SOURCE-BUF ( ^u8 n -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   [: RUN ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc 0<> IF rc throw THEN ;

;package
