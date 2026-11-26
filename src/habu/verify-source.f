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
\ literal's kind is kept too, 0 when there
\ is none: a composition rejects a string loader by it. So is where its text
\ between the quotes starts and ends in the file, as written, escapes and all,
\ the end 0 when there is none: the span a string loader's load reports, and
\ the text a names: call compares.
\
\ An escaped literal's decoded bytes are not in the source, so they live in one
\ of two buffers. STR-DEC-TURN names the one the next decode writes, and a
\ decode that answers its buffer flips it, so the last answer is never in the
\ buffer the next decode writes, or that a reserve for it moves. No reader
\ reads an older answer after the next decode. A top-level literal's answer
\ goes to the slot, which the handlers of the tokens after it read before the
\ next NEXT-SCAN resets it. Only the string loaders and a call of a word a
\ names: row marks read the slot; a body literal keeps only its kind.
\
\ A composition rejects the top-level loader forms discovery rejects, where it
\ meets them, so that walk need not run first: a path that is no literal, a
\ literal no loader takes, or a path the loader rejects - one holding a NUL
\ byte, or one the resolver rejects. A declaration or `undefine` of a loader
\ word's name rejects nothing, as the walk rejects nothing for it, but a later
\ loader form with that word the walk reads - `include` or `require`, or a word
\ right after a path literal - at top level or in a body, is
\ rejected at the word as the walk rejects it.
\
\ A composition loads no file a definition names: a word's run is no part of
\ the check, so the ordinary effect checker judges its runtime loader calls.
\
\ Definer rows: a `create … does>` definition is a definer, and every word it
\ creates takes the clause's declared effect, a `TRUSTED:` definer's included,
\ whose head is asserted but whose clause is checked. The rows are
\ learned from the source, not listed, so library definers put no names in the
\ engine; a definer that makes definitions at load, built or from evaluated text,
\ marks the statement's wordlist instead, leaving what it defines to the run,
\ while a `generates:` row and a FUNCTION: or CALLBACK: group are still read. A
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
\ rejects, not a comment, and a token on a later line starts the next
\ statement. The type is a span of tokens (`ptr* base`, a family application,
\ a spaced quotation or scheme) that ends where the checker ends one or with
\ its line. With no name the definer is rejected at its own token; with no first
\ token the span is empty and the checker rejects it by name.
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
\ A primitive row opens only in a file the engine provides, prims.f, which the
\ LSP's definition index scans; in any other file `EPRIM:` and `EPPRIM:` are
\ ordinary words. A row is read as its opener reads it: the name, and a package
\ row's package before it, are read raw, so either may be spelled as a definer
\ or parsing word (`EPRIM: create EPRIM;`). The body is read raw too: it holds
\ only atoms, spelled `PE-...` or `;PE-...`, the flags `ETRUSTED-ONLY`,
\ `EIMMEDIATE` and `EFIXED-ABI`, and `REF` clauses, `REF` and the token after
\ it; any other body token makes the row malformed. prims.f's
\ `EPRIM:` and `EPPRIM:` rows end at the first `EPRIM;` or `ECLOSE-PRIVATE`
\ that is not a `REF` operand. A row is known by its opener's spelling: a
\ binding cannot tell prims.f's opener from its own definition when the scan
\ reads prims.f. No `parses-through:` row can bound an engine row: prims.f
\ loads before that word is defined.
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
\ answers `deferred` when nothing is rejected. A rejection opens a stretch too,
\ unreported: the load stops at it, so the operands of a misspelt parsing word
\ or the rest of a misread declaration are not rejected after it.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/source-syntax.f
require src/core/checker-owner-guard.f
require src/core/checker.f
require src/core/check-walk.f
require src/core/rejection.f
require src/core/requires.f
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
require src/lib/ffi-callback.f
require src/habu/interpreter.f

package VERIFY
EXPORT CHECK:CHECKER-SIG-SPAN
EXPORT CHECK:DUPLICATE-RECORD
EXPORT CHECK:VERIFY-DEFINER-N
EXPORT CHECK:CHECKER-DEFCAST
EXPORT CHECK:CHECKER-GENERATES
EXPORT CHECK:CHECKER-PARSES-ROW
EXPORT CHECK:CHECKER-NAMES-ROW
EXPORT CHECK:CHECKER-NAMED-USE
EXPORT CHECK:CHECKER-VERIFY-REQUIRED
EXPORT CHECK:CHECKER-REQUIRED-BODY
EXPORT CHECK:CHECKER-TRUSTED-DOES!
EXPORT CHECK:TOP-KEYWORD?
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
EXPORT TYPE-DECL:DECL-FAMILY-NAME
EXPORT TYPE-DECL:REPLAY-ENUM
EXPORT TYPE-DECL:REPLAY-MUTUAL
EXPORT TYPE-DECL:REPLAY-STRUCTURE
EXPORT TYPE-DECL:SD-ONE-NAME
EXPORT TYPE-DECL:SD-WRAP-OUTCOME
EXPORT TFAM:SUMV-CTOR-SYM@
EXPORT TFAM:SUMV-NAME$
EXPORT TFAM:TFAM-N@
EXPORT TFAM:TFAM-VAR-START@
EXPORT FFI:OPENER$
EXPORT FFI:READ-FUNCTION
EXPORT FFI:PLAN-FUNCTION
EXPORT FFI:SKIP-FUNCTION
EXPORT FFI:LEAVE-FUNCTION
EXPORT FFI:LEAVE-OPEN-FUNCTION
EXPORT FFI:ABANDON-FUNCTION
EXPORT FFI:DECLARED-TEXTS

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
PTR-VARIABLE TOP-REJECTED
PTR-VARIABLE TOP-CUR-A
variable TOP-CUR-U
variable TOP-DEFER                               \ a stretch is open
PTR-VARIABLE TOP-DEFER-A  variable TOP-DEFER-U   \ its opener while its report is due, else 0
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

: FAULT-TARGET! ( str -- ) {: s:str :}
   s STR:DATA FAULT-TARGET s STR:LEN BYTE-COPY
   s STR:LEN FAULT-U ! ;

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
variable CSR-NEXT                        \ the next composition's subject byte, -1 for none
variable CSR-STATE
variable CSR-TOK                         \ where that token starts; -1 for the next one read
variable CSR-BODY
variable CSR-SUBJ                        \ the file being scanned is the subject

: CSR-RESET ( -- )
   -1 CSR-AT !  CSR-DONE CSR-STATE !  0 CSR-SUBJ ! ;

: CSR-TAKE ( -- )
   CSR-NEXT @ dup CSR-AT !
   0 < IF CSR-DONE ELSE CSR-WAIT THEN CSR-STATE !
   -1 CSR-NEXT ! ;

CSR-RESET
-1 CSR-NEXT !

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

: SOURCE! ( str -- )
   dup STR:LEN SOURCE-U !
   STR:DATA SOURCE-A !
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
   SCAN-I @ SOURCE-U @ >= if prev SOURCE-U @ dup CSR-HIT SOURCE@ 0 >str exit then
   SCAN-I @ TOKEN-START !
   SCAN-I @ TOKEN-BYTE !
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
   prev TOKEN-START @ SCAN-I @ CSR-HIT
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - >str ;

\ The token at or after byte at, read without moving the scan; empty at the
\ source's end.
: TOKEN-FROM ( n -- str ) {: at:n :}
   SOURCE@ {: s:^ :}
   at BEGIN dup SOURCE-U @ < IF dup s + c@ 33 < ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: b:n :}
   b BEGIN dup SOURCE-U @ < IF dup s + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: e:n :}
   s b +  e b - >str ;

: STRUCTURE-PTR-FIELD? ( str -- bool )
   s" PTR-FIELD:" STR:=CI ;

: STRUCTURE-CFIELD? ( str -- bool )
   s" CFIELD:" STR:=CI ;

: STRUCTURE-CELL-FIELD? ( str -- bool )
   s" +FIELD" STR:=CI ;

: STRUCTURE-FIELD? ( str -- bool ) {: s:str :}
   s STRUCTURE-PTR-FIELD?  s STRUCTURE-CFIELD? or
   s STRUCTURE-CELL-FIELD? or ;

\ The definer t tu has no name after it: the record the loader writes for
\ that definer, else the general one, written through its help.
: NO-NAME-RECORD ( str -- ) {: t:str :}
   t s" defer" STR:=CI IF t INTERPRETER:DEFER-NO-NAME EXIT THEN
   t s" undefine" STR:=CI IF t UNDEFINE-NO-NAME EXIT THEN
   t STRUCTURE-FIELD? IF t t STRUCT-NO-NAME EXIT THEN
   t t s" no name follows" REJECTION:MISSING-NAME ;

\ The definer the scan read last, at TOKEN-BYTE, has no name after it.
: NAME-STOP ( -- )
   TOKEN-BYTE @ TOKEN-FROM NO-NAME-RECORD
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The source ends inside the statement's definition, named by the token after
\ its opener.
: UNCLOSED-STOP ( -- )
   TOP-CUR-A @ SOURCE@ - TOP-CUR-U @ + TOKEN-FROM {: t:str :}
   t t INTERPRETER:UNCLOSED-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The declaration t tu opened, which c cu closes, is still open at the token o ou,
\ which may not come inside it, or, with ou 0, when the source ends: the load's
\ record.
: UNCLOSED-DECL-STOP ( str str str -- )
   {: t:str c:str o:str :}
   t c o SOURCE-ROOT:UNCLOSED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Places the empty token at the statement the scan is in, labelled l lu, when
\ a source the record knows holds it.
: STATEMENT-AT ( str -- ) {: l:str :}
   TOP-CUR-A @ REJECTION:PLACES? 0= IF EXIT THEN
   TOP-CUR-A @ 0 >str REJECTION:AT  l REJECTION:EMIT-STR ;

: SC-LEAD? ( n -- bool )
   dup $73 = over $53 = or over $63 = or swap $43 = or ;

: STRING-LEAD? ( n -- bool )
   dup SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( str -- bool ) {: s:str :}
   s STR:LEN 2 <> IF 0 0= 0= EXIT THEN
   s STR:DATA 1 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   s STR:DATA 0 BYTE@ STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( str -- bool ) {: s:str :}
   s STR:LEN 3 <> IF 0 0= 0= EXIT THEN
   s STR:DATA 1 BYTE@ $5C <> IF 0 0= 0= EXIT THEN
   s STR:DATA 2 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   s STR:DATA 0 BYTE@ STRING-LEAD? ;

: STRING-OPENER? ( str -- bool )
   dup NORMAL-STRING-OPENER? IF drop 0 0= EXIT THEN
   ESCAPED-STRING-OPENER? ;

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
   {: s:str kind:n raw:str :}
   s STR:DATA STR-LAST-A !
   s STR:LEN STR-LAST-U !
   kind STR-LAST-KIND !
   raw STR:DATA SOURCE@ - STR-LAST-AT !
   STR-LAST-AT @ raw STR:LEN + STR-LAST-END ! ;

\ The kinds of literal discovery tells apart before a loader: an `s"` or `S"`
\ one, plain or escaped, is a path; a `c"`, `C"` or `."` one is no string a
\ loader takes.
1 constant LIT-PATH
2 constant LIT-OTHER

: LIT-KIND ( str -- n )
   STR:DATA c@ dup $73 = swap $53 = or IF LIT-PATH EXIT THEN
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
   SCAN-I @ TOKEN-START @ - pfx - 1 - >str ;

\ A plain literal of the given kind, its payload pfx bytes after its opener:
\ the bytes it makes are its text as written.
: RECORD-SKIPPED-STRING ( n n -- )
   {: kind:n pfx:n :}
   QUIET-UNTERM
   pfx SKIPPED-PAYLOAD dup STR:LEN 0 < IF drop EXIT THEN
   {: raw:str :}
   raw kind raw STR-LAST! ;

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
EXPORT CHECK:CHECKER-STORAGE-NAME-REJECT
EXPORT CHECK:CHECKER-DERIVED-TYPE-REJECT
EXPORT CHECK:CHECKER-TRY-RECORD
EXPORT CHECK:CHECKER-TYPE-SPAN-BREAK?
EXPORT CHECK:CHECKER-TYPE-SPAN-STEP
EXPORT CHECK:CHECKER-UNDEFINE
EXPORT CHECK:CHECKER-UNSEEN-NAME
EXPORT CHECK:CLOSE-USING
EXPORT CHECK:DEF-NAME-REJECT?
EXPORT CHECK:DOES-UNMADE-OPEN
EXPORT CHECK:IMM-READS
EXPORT CHECK:LEFT-AT-CALL?
EXPORT CHECK:MULTI-ERR?
EXPORT CHECK:OPEN-USING
EXPORT CHECK:PROW-ADD-TEXT
EXPORT CHECK:PROW-FIND
EXPORT CHECK:SIG-EXACT-MODE
EXPORT CHECK:SIG-RAW-MODE
EXPORT CHECK:SIG-STATES-FACTS
EXPORT CHECK:SPLIT-STR-FILE?
EXPORT CHECK:TYPE-RESERVED?
EXPORT CHECK:TYPE-SPAN-END?
EXPORT CHECK:UNSEEN-COVERS?
EXPORT CHECK:USIG-NEWEST
EXPORT CHECK:VREC-TOKEN
\ The bytes the engine's `s\"` makes of an escaped literal's payload, and
\ whether every escape in it is one the engine reads. An escape is spelt
\ with more bytes than it decodes to, so a decode as long as its payload met no
\ escape: the answer is the source span, and a diagnostic at
\ the literal keeps its position.
: ESC-BYTES ( str -- str bool ) {: src:str :}
   src STR:LEN STR-DEC-ROOM {: dst:^ :}
   src dst ESCAPE:DECODE {: k:n ok:bool :}
   k src STR:LEN = ok 0= or IF src ok EXIT THEN
   1 STR-DEC-TURN @ - STR-DEC-TURN !
   dst k >str ok ;

\ A bad escape is rejected at the opener with the engine's record of a bad
\ string literal. An unterminated literal is left to the lexer, as a plain one
\ is, except in a composition or index.
: RECORD-ESCAPED-STRING ( n -- )
   {: kind:n :}
   QUIET-UNTERM
   FOUND @ 0= IF EXIT THEN
   4 SKIPPED-PAYLOAD {: raw:str :}
   raw ESC-BYTES 0= IF
      SOURCE@ TOKEN-START @ + 3 >str INTERPRETER:LITERAL-REJECT
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN
   dup raw SRCLOC:LITERAL!
   kind raw STR-LAST! ;

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

: LOCAL? ( str -- bool ) {: s:str :}
   LOCAL-N @ 0 ?do
      s i LOCAL-A @ i LOCAL-U @ >str STR:= if 0 0= unloop exit then
   loop
   0 0= 0= ;

\ A file whose first two bytes are `#!` names its interpreter on that line, and
\ the loader reads the line as a comment: a token at the source's first byte.
: SHEBANG? ( str -- bool )
   {: s:str :}
   s STR:DATA SOURCE@ <>  s STR:LEN 2 <  or IF 0 0= 0= EXIT THEN
   s STR:DATA c@ $23 =  s STR:DATA 1 + c@ $21 =  and ;

\ The byte that ends the comment a token opens, as the loader reads it: the
\ line's end after `\`, `)` after `(`; 0 for any other token.
: COMMENT-END ( str -- n )
   {: s:str :}
   s STR:LEN 1 <> IF 0 EXIT THEN
   s STR:DATA c@ 92 = IF 10 EXIT THEN
   s STR:DATA c@ 40 = IF 41 EXIT THEN
   0 ;

: NEXT ( -- str )
   BEGIN
      NEXT-RAW
      dup STR:LEN 0= IF EXIT THEN
      dup SHEBANG? IF drop 10 SKIP-PAST ELSE
      dup COMMENT-END dup 0<> IF nip SKIP-PAST ELSE drop
      SKIP-STRINGS @ 0= IF dup LOCAL? IF EXIT THEN THEN
      SKIP-STRINGS @ 0= 0= IF
         dup ESCAPED-STRING-OPENER? IF LIT-KIND SKIP-ESCAPED-QUOTE RECORD-ESCAPED-STRING ELSE
         dup NORMAL-STRING-OPENER? IF LIT-KIND 34 SKIP-PAST 3 RECORD-SKIPPED-STRING ELSE EXIT THEN THEN
      ELSE EXIT THEN
      THEN THEN
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
   NEXT-RAW  dup STR:LEN TOKEN-U !  STR:DATA TOKEN-A ! ;

: BODY! ( -- )
   NEXT-BODY  dup STR:LEN TOKEN-U !  STR:DATA TOKEN-A ! ;

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
\ the run changes any state, so a rejected reservation leaves the body as it was.
: BODY-APPEND ( str -- )
   {: s:str :}
   s STR:DATA SOURCE@ - {: src:n :}
   BODY-U @ s STR:LEN + 1 + {: end:n :}
   end BODY-BUF-RESERVE
   BODY-ROWS @ 1 + 2 * BODY-ROW-RESERVE
   src CSR-PLACE
   src BODY-ROW!
   s STR:DATA  0 BODY-BUF BODY-U @ +  s STR:LEN BYTE-COPY
   32  BODY-U @ s STR:LEN + BODY-BUF c!
   end BODY-U ! ;

\ The armed addresses hold until the next BODY-APPEND, which may move both
\ buffers; every check calls BODY$ again before it reads them.
: BODY$ ( -- str )
   BODY-U @ 1 max BODY-BUF-RESERVE
   BODY-ROWS @ 2 * 1 max BODY-ROW-RESERVE
   0 BODY-BUF BODY-U @ >str 0 BODY-ROW BODY-ROWS @ REJECTION:MAP!
   0 BODY-BUF BODY-U @ >str ;

\ The body holds no closer, so the checker is told where it stands and rejects
\ a structure still open there.
: DEF-BODY$ ( -- str )
   BODY$ {: b:str :}
   TOKEN-A @ TOKEN-U @ >str b STR:DATA REJECTION:CLOSER!  b ;

\ The signature ahead of the scan, read as the engine reads a definition head's:
\ from its `(` through its `)`, whether one opens there and whether it closes.
\ The scan passes a closed one.
: SIG-GROUP ( -- str bool bool )
   SOURCE@ SCAN-I @ +  SOURCE-U @ SCAN-I @ -
   {: at:^ left:n :}
   at left >str CHECKER-SIG-SPAN
   {: open:n close:n :}
   open left = IF at 0 >str false false EXIT THEN
   close 0= IF at 0 >str true false EXIT THEN
   SCAN-I @ close + SCAN-I !
   at open +  close open - >str  true true ;

\ SIG-GROUP, and whether one opens; one that never closes leaves the statement's
\ definition unclosed.
: SCAN-SIG ( -- str bool )
   SIG-GROUP {: open:bool closed:bool :}
   open closed 0= and IF UNCLOSED-STOP THEN
   open ;

: MAYBE-SIGNATURE ( -- str )
   SCAN-SIG 0= IF EXIT THEN
   dup BODY-APPEND
   {: sig:str :}
   sig STR:DATA 1 +  sig STR:LEN 2 - >str ;

\ The record of t tu, a definition's name or `does>`, with no closed signature
\ after it.
: SIGNATURE-STOP ( str -- ) {: t:str :}
   t INTERPRETER:SIGNATURE-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The signature that must follow t tu; with none the scan stops.
: SIGNATURE-AT ( str -- str )
   {: t:str :}
   SIG-GROUP nip {: sig:str closed:bool :}
   closed 0= IF t SIGNATURE-STOP THEN
   sig STR:DATA 1 +  sig STR:LEN 2 - >str ;

: STRING-REST ( str -- str ) {: opener:str :}
   SCAN-I @ {: start:n :}
   opener ESCAPED-STRING-OPENER? IF
      SKIP-ESCAPED-QUOTE
   ELSE
      34 SKIP-PAST
   THEN
   QUIET-UNTERM
   FOUND @ 0= IF
      opener INTERPRETER:LITERAL-REJECT
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN
   SOURCE@ start + SCAN-I @ start - >str ;

: FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

\ The tokens a straight line has none of. The checker's own classifier cannot
\ be reused for the question: it pushes a frame for every token it recognises,
\ so asking it would move the checker's state. These are its token list, plus
\ the compile-time brackets a definer call must not hide behind. Each matches
\ case-folded, as the engine's keywords ignore case, so `EXIT` ends the line as
\ `exit` does.
: WRAP-COND-TOK? ( str -- bool ) {: s:str :}
   s s" if" STR:=CI
   s s" else" STR:=CI or
   s s" then" STR:=CI or
   s s" case" STR:=CI or
   s s" of" STR:=CI or
   s s" endof" STR:=CI or
   s s" endcase" STR:=CI or ;

: WRAP-LOOP-TOK? ( str -- bool ) {: s:str :}
   s s" begin" STR:=CI
   s s" while" STR:=CI or
   s s" repeat" STR:=CI or
   s s" until" STR:=CI or
   s s" again" STR:=CI or
   s s" do" STR:=CI or
   s s" ?do" STR:=CI or
   s s" loop" STR:=CI or
   s s" +loop" STR:=CI or
   s s" leave" STR:=CI or
   s s" exit" STR:=CI or ;

: WRAP-BRACKET-TOK? ( str -- bool ) {: s:str :}
   s s" [:" STR:=CI
   s s" ;]" STR:=CI or
   s s" [" STR:=CI or
   s s" ]" STR:=CI or
   s s" postpone" STR:=CI or
   s s" recurse" STR:=CI or ;

: WRAP-CTL-TOK? ( str -- bool ) {: s:str :}
   s WRAP-COND-TOK?
   s WRAP-LOOP-TOK? or
   s WRAP-BRACKET-TOK? or ;

\ The record of a loader form at t tu that discovery cannot follow, for the
\ message m mu, written through the help HELP writes for t tu: the caller stops
\ it.
: FORM-REJECT ( str str [ str -- ] -- ) {: t:str m:str help :}
   s" E-LOADER-FORM" REJECTION:OPEN  m REJECTION:EMIT-STR
   t REJECTION:AT
   s" discovery cannot follow `" REJECTION:EMIT-STR  t REJECTION:EMIT-STR
   s" `" REJECTION:EMIT-STR
   REJECTION:HELP
   t help execute ;

: TICKED ( str -- )
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
   [: drop
      s" write the path as a literal: `require path/file.f`" REJECTION:EMIT-STR
      S\"  or `s\" path/file.f\" required`" REJECTION:EMIT-STR ;]
   FORM-REJECT ;
: OPENER-REJECT ( str -- )
   s" unsupported string opener before a loader word"
   [: drop S\" write the path with `s\"`: `s\" path/file.f\" required`" REJECTION:EMIT-STR ;]
   FORM-REJECT ;
: CAPACITY-REJECT ( str -- )
   s" capacity exceeded"
   [: drop s" shorten the path to at most " REJECTION:EMIT-STR  PATH-CAP REJECTION:EMIT-U
      s"  bytes" REJECTION:EMIT-STR ;] FORM-REJECT ;
private

\ A loader word's bit in SHADOWED and RETIRED, 0 for any other name: the words
\ discovery reads as loaders, `script-required` not one.
: LOADER-BIT ( str -- n ) {: s:str :}
   s s" include" STR:=CI IF $01 EXIT THEN
   s s" included" STR:=CI IF $02 EXIT THEN
   s s" require" STR:=CI IF $04 EXIT THEN
   s s" required" STR:=CI IF $08 EXIT THEN
   s s" provided" STR:=CI IF $10 EXIT THEN
   0 ;

\ Loader words the file declared or retired by WALK-NAMES's forms: past either,
\ the walk, holding no wordlists, cannot tell which word a spelling names.
variable SHADOWED
variable RETIRED

\ The loader word u bytes long at TOKEN-BYTE has no literal path.
: DYNAMIC-STOP ( n -- ) {: u:n :}
   SOURCE@ TOKEN-BYTE @ + u >str DYNAMIC-REJECT
   E-DISC-DYNAMIC DISCOVERY-STOP ;

\ The symbol of the newest definition, which an `immediate` marks: 0 from its
\ name until it is recorded, so one after a rejected definition marks nothing.
\ A definer that can end without DEFINED clears it as it starts.
variable NEWEST-SYM

: NAME-DECLARED ( -- )
   0 NEWEST-SYM ! ;

\ A loader form the walk reads, its word u bytes long at TOKEN-BYTE.
: QUIET-USE ( str -- ) {: s:str :}
   s LOADER-BIT {: bit:n :}
   SOURCE@ TOKEN-BYTE @ + {: t:^ :}
   bit SHADOWED @ and 0<> IF
      t s STR:LEN >str SHADOW-REJECT  E-DISC-SHADOW DISCOVERY-STOP
   THEN
   bit RETIRED @ and 0<> IF
      t s STR:LEN >str RETIRE-REJECT  E-DISC-RETIRE DISCOVERY-STOP
   THEN ;

\ A string loader u bytes long, after a literal of the given kind. True when it
\ may load the literal's path, which the resolver alone may still reject.
: LITERAL-LOAD? ( n n -- bool ) {: kind:n u:n :}
   kind 0= IF u DYNAMIC-STOP THEN
   kind LIT-OTHER = IF
      SOURCE@ TOKEN-BYTE @ + u >str OPENER-REJECT  E-DISC-OPENER DISCOVERY-STOP
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
: SEAL-STOP ( str n n -- ) {: s:str at:n tu:n :}
   s  SOURCE@ at + tu >str  REJECTION:SEALED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

EXPORT BOOT:data-base
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

: SEALED-OPEN ( str n n -- ) {: s:str at:n tu:n :}
   OPEN-SEALED? IF s at tu SEAL-STOP THEN
   OWNER-SEALED?  CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-GLOBAL =  and IF
      s at tu SEAL-STOP
   THEN ;

EXPORT CHECKER-RESOLVE:HEAD-ROW

\ A head resolves as the definition's does: a part is asked by its path. A
\ qualified name goes where its qualifier says, whatever the section: while a
\ part's owner is sealed, only the open part's own head takes it.
: SEALED-NAME ( str n n -- ) {: s:str at:n tu:n :}
   s XREF-QUAL-INDEX {: q:n :}
   q 0 < IF s at tu SEALED-OPEN EXIT THEN
   s STR:DATA q >str HEAD-ROW {: row:^ :}
   row NULL-PTR = IF s STR:DATA q >str ELSE row XREF-NAME$ THEN SOURCE-SEALED? IF s at tu SEAL-STOP THEN
   row NULL-PTR <> IF row XREF:SEALED? IF s at tu SEAL-STOP THEN THEN
   OPEN-SEALED? IF s at tu SEAL-STOP THEN
   row NULL-PTR = IF false ELSE row XREF-PKG-PUBLIC data-base PKG-PUB-CELL + @ = THEN
   0=  OWNER-SEALED?  and IF s at tu SEAL-STOP THEN
   SEALED-LOAD? 0= IF EXIT THEN
   row NULL-PTR = IF EXIT THEN
   row XREF-PKG-PUBLIC XREF-WID-PROTECTED? IF s at tu SEAL-STOP THEN ;

\ The family name a declaration writes at byte AT: its token up to the
\ first `<`, rejected as SEALED-NAME rejects a name. An empty one is left to
\ the declaration's missing-name rejection.
: SEALED-FAMILY ( str n -- ) {: s:str at:n :}
   s DECL-FAMILY-NAME STR:LEN {: cut:n :}
   cut 0= IF EXIT THEN
   s STR:DATA cut >str at cut SEALED-NAME ;

\ The kind of a literal from the rest STRING-REST read after its string opener.
\ An escaped one's payload is decoded (ESC-BYTES) only to reject a bad escape
\ at the opener.
: BODY-LIT! ( str str -- ) {: o:str s:str :}
   BODY-PREV-CLEAR
   s STR:LEN 2 < IF EXIT THEN
   s STR:DATA 1 + s STR:LEN 2 - >str
   o NORMAL-STRING-OPENER? 0= IF
      ESC-BYTES 0= IF
         o INTERPRETER:LITERAL-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
   THEN
   drop
   o LIT-KIND BODY-LIT-KIND ! ;

\ The record of a declaration-owner record with no operation for a step the
\ scan takes, at the statement it is in when a source the record knows holds
\ it. The scan knows the field's offset only, so no operation is named.
: OWNER-STOP ( -- )
   TOP-CUR-A @ {: p:^ :}
   p REJECTION:PLACES? IF p ELSE NULL-PTR THEN {: t:^ :}
   NULL-PTR 0 >str t 0 >str CHECKER-OWNER-GUARD:MISSING-OP ;

EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
NCOMP-DISPATCH:DECL-CELL RESERVED-PTR-U8-CELL SOURCE-OWNER-A

\ Replay-only checker queries use the live declaration owner.
: OWNER-XT ( n -- n ) {: off:n :}
   SOURCE-OWNER-A @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF OWNER-STOP THEN ;

CAST: BINDING-ACTION ( n -- [ str -- n n n ] )
: TOP-BINDING ( str -- n n n )
   CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF OWNER-XT BINDING-ACTION execute ;

: CORE-OP? ( str -- bool )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-CORE-OP and 0<> ;

\ The answer F of the engine's predicate the token a u spells, and whether
\ the token binds that word rather than a program's.
: ENGINE-GUARD ( str bool -- bool bool ) {: s:str f:bool :}
   s CORE-OP? 0= IF false false EXIT THEN
   f true ;

\ The flag the engine's target predicate the token a u binds pushes, and
\ whether the token binds one.
: TARGET-GUARD ( str -- bool bool ) {: s:str :}
   s s" HB-TARGET-LINUX?" STR:=CI IF
      s HB-TARGET-LINUX? ENGINE-GUARD EXIT
   THEN
   s s" HB-TARGET-MACOS?" STR:=CI IF
      s HB-TARGET-MACOS? ENGINE-GUARD EXIT
   THEN
   s s" HB-TARGET-LINUX-X86-64?" STR:=CI IF
      s HB-TARGET-LINUX-X86-64? ENGINE-GUARD EXIT
   THEN
   s s" HB-TARGET-LINUX-ARM64?" STR:=CI IF
      s HB-TARGET-LINUX-ARM64? ENGINE-GUARD EXIT
   THEN
   s s" HB-TARGET-WASM?" STR:=CI IF
      s HB-TARGET-WASM? ENGINE-GUARD EXIT
   THEN
   false false ;

: BODY-USE ( str -- ) {: s:str :}
   COMPOSE-ON @ 0= IF EXIT THEN
   s s" include" STR:=CI  s s" require" STR:=CI or
   BODY-LIT-KIND @ LIT-PATH = or IF s QUIET-USE THEN ;

: BODY-TOKEN-SEEN ( str -- ) {: s:str :}
   s BODY-USE
   BODY-PREV-CLEAR ;

\ The rest starts with the opener's one delimiter, which the blank BODY-APPEND
\ writes after the opener stands for, so it joins past that delimiter: the
\ checker reads the literal's bytes as the engine makes them, and a names: row
\ binds the word they name.
: APPEND-STRING ( str -- ) {: t:str :}
   t BODY-APPEND
   t STRING-REST {: r:str :}
   r STR:DATA 1 + r STR:LEN 1 - >str BODY-APPEND
   t r BODY-LIT! ;

: SKIP-STRING-REST ( str -- ) {: s:str :}
   s s STRING-REST BODY-LIT! ;

\ A parsing keyword's operand is its next token, data whatever it spells; a body
\ local is looked up first.
: CHAR-KEYWORD? ( str -- bool )
   s" char" STR:=CI ;

: TOP-PARSER? ( str -- bool ) {: s:str :}
   s CHAR-KEYWORD? IF 0 0= EXIT THEN
   s s" '" STR:=CI ;

: BODY-PARSER? ( str -- bool ) {: s:str :}
   s s" [']" STR:=CI IF 0 0= EXIT THEN
   s s" postpone" STR:=CI IF 0 0= EXIT THEN
   s s" [char]" STR:=CI ;

: OPERAND ( -- str )
   NEXT-RAW dup STR:LEN 0= IF NAME-STOP THEN ;

\ A definer takes its name as the next raw token, so `create s"` names a word
\ `s"`; every definer rejects a `(` or `\` name (REJECTION:NOT-A-NAME). A
\ comment or string rule there would hand the definer a later token instead.
\ With no token left TOKEN-BYTE stays at the definer.
: NAME-TOKEN ( -- str )
   NEXT-RAW ;

: LOCAL-TOKEN? ( -- bool )
   TOKEN-A @ TOKEN-U @ >str LOCAL? ;

: LOCAL-ADD ( str -- ) {: s:str :}
   LOCAL-N @ 1 + LOCAL-A-RESERVE
   LOCAL-N @ 1 + LOCAL-U-RESERVE
   LOCAL-N @ 1 + LOCAL-D-RESERVE
   0 BEGIN dup s STR:LEN < IF s STR:DATA over + c@ $3A <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   LOCAL-N @ LOCAL-U !
   s STR:DATA LOCAL-N @ LOCAL-A !
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

: BLOCK-STEP ( str -- ) {: s:str :}
   LOCAL-N @ 0= IF EXIT THEN
   s BLOCK-OPENER? IF LOCAL-DEPTH @ 1 + LOCAL-DEPTH ! EXIT THEN
   s s" else" STR:=CI IF BLOCK-DROP EXIT THEN
   s BLOCK-CLOSER? IF BLOCK-DROP LOCAL-DEPTH @ 1 - LOCAL-DEPTH ! THEN ;

: GROUP-TOKEN ( str -- bool ) {: s:str :}
   s s" :}" STR:= IF 0 0= EXIT THEN
   s LOCAL-ADD
   0 0= 0= ;

variable GROUP-AT

\ The next token of a group. The source ending inside one is rejected at the
\ group's `{:`; a composition or index stops as discovery's lexer stops there
\ (E-DISC-UNTERM).
: GROUP-NEXT ( -- str )
   NEXT-RAW dup STR:LEN 0= IF
      GROUP-AT @ TOKEN-FROM INTERPRETER:GROUP-REJECT
      COMPOSE-ON @ INDEX-ON @ or 0<> IF E-DISC-UNTERM DISCOVERY-STOP THEN
      EXIT-STATUS:REJECTED REJECTION:STOP
   THEN ;

: APPEND-GROUP ( -- )
   BEGIN GROUP-NEXT dup BODY-APPEND GROUP-TOKEN UNTIL ;

: SKIP-GROUP ( -- )
   BEGIN GROUP-NEXT GROUP-TOKEN UNTIL ;

\ The path `include` or `require` takes, the word wu bytes long and standing at
\ TOKEN-BYTE: none is rejected at the word.
: LOADER-OPERAND ( n -- str ) {: wu:n :}
   NEXT-RAW {: p:str :}
   p STR:LEN 0= IF
      SOURCE@ TOKEN-BYTE @ + wu >str LOADER:NO-PATH  E-DISC-DYNAMIC DISCOVERY-STOP
   THEN
   p ;

\ The tokens an admitted immediate's `parses:` count names are read raw, across
\ line ends, and never join the body; a using rejection is left to the definition's check.
: SKIP-PAYLOAD ( str -- ) {: s:str :}
   s s CHECKER-FIND-QUIET-SYM IMM-READS {: cnt:n :}
   cnt 0 > IF cnt 0 ?DO NEXT-RAW drop LOOP THEN ;

\ A local or a group skips BODY-TOKEN-SEEN, so it clears the literal before it:
\ a token after it is no word right after a path literal.
: APPEND-BODY-TOKEN ( -- )
   LOCAL-TOKEN? IF
      BODY-PREV-CLEAR
      TOKEN-A @ TOKEN-U @ >str BODY-APPEND
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ >str s" {:" STR:= IF
      BODY-PREV-CLEAR
      TOKEN-BYTE @ GROUP-AT !
      TOKEN-A @ TOKEN-U @ >str BODY-APPEND
      APPEND-GROUP
      EXIT
   THEN
   TOKEN-A @ TOKEN-U @ >str BLOCK-STEP
   TOKEN-A @ TOKEN-U @ >str BODY-PARSER? IF
      BODY-PREV-CLEAR
      TOKEN-A @ TOKEN-U @ >str BODY-APPEND
      OPERAND BODY-APPEND
      exit
   THEN
   TOKEN-A @ TOKEN-U @ >str STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ >str APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ >str BODY-TOKEN-SEEN
      TOKEN-A @ TOKEN-U @ >str BODY-APPEND
      TOKEN-A @ TOKEN-U @ >str SKIP-PAYLOAD
   THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ >str BODY-PARSER? IF OPERAND drop BODY-PREV-CLEAR exit THEN
   TOKEN-A @ TOKEN-U @ >str STRING-OPENER? IF TOKEN-A @ TOKEN-U @ >str SKIP-STRING-REST exit THEN
   TOKEN-A @ TOKEN-U @ >str BODY-TOKEN-SEEN
   TOKEN-A @ TOKEN-U @ >str SKIP-PAYLOAD ;

\ SKIP-BODY-TOKEN for a definition body, which declares locals.
: SKIP-DEF-TOKEN ( -- )
   LOCAL-TOKEN? IF BODY-PREV-CLEAR EXIT THEN
   TOKEN-A @ TOKEN-U @ >str s" {:" STR:= IF
      BODY-PREV-CLEAR  TOKEN-BYTE @ GROUP-AT !  SKIP-GROUP EXIT
   THEN
   TOKEN-A @ TOKEN-U @ >str BLOCK-STEP
   SKIP-BODY-TOKEN ;

\ The owner record stores raw execution tokens; each view states the signature
\ of the field it calls.
CAST: SYM-ACTION ( n -- [ str -- n ] )
CAST: IDENTITY-ACTION ( n -- [ n -- str str n ] )
CAST: SOURCE-ACTION ( n -- [ n -- n ] )
CAST: CREATES-ACTION ( n -- [ n -- n ] )
CAST: CREATED-ACTION ( n -- [ str n -- bool ] )
CAST: DOES-ACTION ( n -- [ str str str -- n ] )
CAST: RENDERS-ACTION ( n -- [ str n -- bool ] )
CAST: REACH-ACTION ( n -- [ str str -- ] )
CAST: CLAUSE-ACTION ( n -- [ str -- ] )
CAST: DECL-ACTION ( n -- [ str str -- n ] )
CAST: SHADOW-ACTION ( n -- [ str str -- ] )
CAST: TICK-ORDER-ACTION ( n -- [ str n ^u8 [ -- nominal ] -- n nominal ] )

: WITH-BODY-TICK-ORDER ( str [ -- nominal ] -- bool )
   {: s:str q :}
   s DEF-TICK-ORDER @ DEF-TICK-OWNER @ q
   NCOMP-DISPATCH:DECL-WITH-TICK-ORDER-OFF OWNER-XT TICK-ORDER-ACTION execute 0<> ;

CAST: VERIFIER-ACTION ( n -- [ -- ] )
CAST: ARM-ACTION ( n -- [ str n n n -- ] )
CAST: CTOR-DECL-ACTION ( n -- [ n n n n -- ] )
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
   {: path:str :}
   VISIT-PATHS-U @ {: off:n :}
   VISIT-N @ 0= IF VISIT-NEXT @ 1 + VISIT-FIRST ! THEN
   off path STR:LEN + 1 + VISIT-PATHS-RESERVE
   path STR:DATA off VISIT-PATHS path STR:LEN BYTE-COPY
   off path STR:LEN + VISIT-PATHS-U !
   VISIT-N @ 1 + 2 * VISIT-ROWS-RESERVE
   off VISIT-N @ 2 * VISIT-ROWS !
   path STR:LEN VISIT-N @ 2 * 1 + VISIT-ROWS !
   VISIT-N @ 1 + VISIT-N !
   VISIT-NEXT @ 1 + VISIT-NEXT !
   VISIT-NEXT @ VISIT-CUR ! ;

: VISIT-IN? ( n -- bool )
   VISIT-FIRST @ - {: row:n :}
   row 0 >= row VISIT-N @ < and ;

\ The path visit V's file was resolved to, for a visit this composition made.
: VISIT-PATH ( n -- str )
   VISIT-FIRST @ - 2 * {: row:n :}
   row VISIT-ROWS @ VISIT-PATHS  row 1 + VISIT-ROWS @ >str ;

: VISITS-DROP ( -- )
   VISIT-PATHS-RELEASE  VISIT-ROWS-RELEASE
   0 VISIT-PATHS-U !  0 VISIT-N !  0 VISIT-CUR ! ;

\ Arm the checker with NAME, declared by the token from byte AT, U long, in the
\ file being scanned: the record the next named registrar retains takes that
\ spelling and location, until DISARM. Outside a visit there is no file to
\ name, and the checker's armed state stays its caller's.
: ARM-UNSEALED ( str n n -- )
   {: name:str at:n u:n :}
   VISIT-CUR @ 0= IF EXIT THEN
   name VISIT-CUR @ at at u +
   NCOMP-DISPATCH:DECL-VERIFY-DECL-ARM-OFF OWNER-XT ARM-ACTION execute ;

: ARM ( str n n -- )
   {: name:str at:n u:n :}
   name at u SEALED-NAME
   name at u ARM-UNSEALED ;

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
defer ON-CANDIDATE ( str str n n -- )

private

: CANDIDATE-NONE ( str str n n -- )
   2drop 2drop ;

: CANDIDATE-INIT ( -- )
   ['] CANDIDATE-NONE is ON-CANDIDATE ;

CANDIDATE-INIT

CAST: CURSOR-ACTION ( n -- [ n [ str n n n -- ] -- ] )
CAST: VISIBLE-ACTION ( n -- [ str n [ str n n n -- ] -- ] )

\ The checker's candidate: its spelling and the declaration's visit and span,
\ 0 0 0 for none.
: CSR-VISIT ( str n n n -- )
   {: name:str v:n s:n e:n :}
   v VISIT-IN? IF name v VISIT-PATH s e ON-CANDIDATE EXIT THEN
   name NULL$ 0 0 ON-CANDIDATE ;

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
\ or a stretch deferred at the token) instead of rejected E-UNDEFINED.
: RENDERS-MARK? ( str n -- bool )
   NCOMP-DISPATCH:DECL-VERIFY-RENDERS-OFF OWNER-XT RENDERS-ACTION execute ;

CTL-RENDERS CTL-CREATES or constant MARK-UNSEEN
CTL-RENDERS CTL-NOMINAL or constant MARK-NOMINAL

\ Only a reached call whose outputs this reader does not model can introduce
\ nominal types absent from the source. A modeled declaration checks its own
\ complete type before it publishes anything.
: NOMINAL-MARK ( str -- )
   {: s:str :}
   s num-parse nip nip IF EXIT THEN
   s STRING-OPENER? IF EXIT THEN
   s MARK-NOMINAL RENDERS-MARK? drop ;

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
   row DEFINER-AT @ DEFINER-SIG  row DEFINER-LEN @ >str ;

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
: DEFINER-ADD ( str bool n -- ) {: sig:str gen:bool sym:n :}
   sym 0= IF EXIT THEN                        \ never recorded: nothing to hang it on
   sym DEFINER-ROW
   {: row:n :}
   gen row DEFINER-GEN !
   sig STR:LEN 0= IF 0 row DEFINER-AT !  0 row DEFINER-LEN !  EXIT THEN
   sig STR:LEN DEFINER-ROOM
   {: at:n :}
   sig STR:DATA  at DEFINER-SIG  sig STR:LEN BYTE-COPY
   at row DEFINER-AT !  sig STR:LEN row DEFINER-LEN ! ;

\ Give sym the effect row FROM holds, read where it lies once the room is made.
: DEFINER-INHERIT ( n n -- ) {: from:n sym:n :}
   sym 0= IF EXIT THEN
   sym DEFINER-ROW
   {: row:n :}
   from DEFINER-LEN @
   {: u:n :}
   u DEFINER-ROOM
   {: at:n :}
   from DEFINER-SIG@ STR:DATA  at DEFINER-SIG  u BYTE-COPY
   at row DEFINER-AT !  u row DEFINER-LEN ! ;

: DEFINER-RETIRE ( n -- )
   {: sym:n :}
   sym DEFINER-FIND 0= IF EXIT THEN
   sym DEFINER-APPEND drop ;

\ The row of a token's definer + 1, 0 when it is no learned definer.
: DEFINER-OF ( str -- n ) {: s:str :}
   VERIFY-DEFINER-N @ 0= IF 0 EXIT THEN
   s FIND-SYM DEFINER-FIND ;

\ The effect a token's definer gives the word it creates, answered as a string
\ whose ZERO LENGTH means "not a learned definer".
: DEFINER-EFFECT ( str -- str )
   DEFINER-OF dup 0= IF drop SOURCE@ 0 >str EXIT THEN
   1 - DEFINER-SIG@ ;

variable DEFER-REPORT                            \ the child reports deferrals
\ The last run reached uncertain source. Only REPORT-DEFERRAL and REPORT-DEFERRED
\ set it, and each writes the packet that locates it when the child asks for one.
variable DEFER-SEEN

\ WHY, the deferral's cause or 0, picks its warning's message and help, which
\ name the call NA NU, empty for none; a site that stops the scan sets
\ TICK-REMAINDER before it reports.
CAST: STRETCH-ACTION ( n -- [ str -- ] )
: REPORT-STRETCH ( str n str -- ) {: s:str why:n ns:str :}
   why ns TICK-REMAINDER @ CHECKER-DEFER-WHY!
   s NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-OFF OWNER-XT STRETCH-ACTION execute ;

\ A deferral of these causes runs no code the check does not follow, so it
\ opens or closes no declaration.
: FOLLOWED? ( n -- bool )
   {: why:n :}
   why DW-TYPE =  why DW-RENDERED = or  why DW-COUNT = or ;

\ The CALLBACK: group the scan read, with its opener and where its name lies;
\ skipped when a word it claims was rejected, so `;CALLBACK` defines nothing;
\ left when code the check does not follow may close it: the run decides.
ENUM callback-group closed open skipped left ;ENUM
TYPED-VARIABLE CB-GROUP callback-group
PTR-VARIABLE CB-OPENER-A
variable CB-OPENER-U
variable CB-AT
variable CB-NAME-U
: CB-CLOSED! ( -- ) construct callback-group closed CB-GROUP ! ;
CB-CLOSED!

\ A skipped group's words are rejected whether or not the run closes it.
: LEAVE-CALLBACK ( -- )
   CB-GROUP @ MATCH callback-group
      closed OF ENDOF
      open OF construct callback-group left CB-GROUP ! ENDOF
      skipped OF CB-CLOSED! ENDOF
      left OF ENDOF
   ;MATCH ;

: DEFER-DECL ( n -- )
   {: why:n :}
   why FOLLOWED? IF SKIP-FUNCTION EXIT THEN
   LEAVE-CALLBACK
   why DW-RENDERER = IF LEAVE-OPEN-FUNCTION EXIT THEN
   LEAVE-FUNCTION ;

: REPORT-DEFERRAL ( str n str -- )
   {: s:str why:n ns:str :}
   -1 DEFER-SEEN !
   DEFER-REPORT @ IF s why ns REPORT-STRETCH THEN ;

: DEFER-AT ( str n -- ) {: s:str why:n :}
   why DEFER-DECL  s why NULL-PTR 0 >str REPORT-DEFERRAL ;

: QUIET-TYPE-CLEAR ( -- )
   CHECKER-SIG-UNRES-TAKE drop drop ;
: QUIET-TYPE-REPORT ( -- )
   CHECKER-SIG-UNRES-TAKE {: t:str found:bool :}
   found IF t DW-TYPE DEFER-AT THEN ;

\ A body the checker deferred to the run (verdict 2) is reported where the
\ checker's judgment of it stopped, when the child reports deferrals, as a
\ deferred stretch is at the token that opened it.
CAST: DEFERRED-BODY-ACTION ( n -- [ -- str n str ] )
: DEFERRED-BODY ( -- str n str )
   NCOMP-DISPATCH:DECL-VERIFY-DEFERRED-BODY-OFF OWNER-XT DEFERRED-BODY-ACTION execute ;
: REPORT-DEFERRED ( n -- )
   2 <> IF EXIT THEN
   -1 DEFER-SEEN !
   DEFER-REPORT @ IF DEFERRED-BODY REPORT-STRETCH THEN ;

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
\ run when neither was rejected and one was deferred, or when the scan stopped
\ inside it (TICK-REMAINDER). A rejected one counts in neither, and the scan
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
   TICK-BODY-A @ TICK-BODY-U @ >str CHECK! TICK-BODY-VERDICT ! ;

: TICK-BODY-RUN ( -- )
   TICK-BODY-U @ ['] TICK-BODY-CHECK CSR-CHECK ;

: VERIFY-BODY ( -- n )
   DEF-BODY$ {: b:str :}
   b STR:DATA TICK-BODY-A !  b STR:LEN TICK-BODY-U !
   b [: TICK-BODY-RUN ;] WITH-BODY-TICK-ORDER
   IF true TICK-REMAINDER ! 2 ELSE TICK-BODY-VERDICT @ THEN BODY-VERDICT ;

\ The pre-pass's own does>-clause entry point. It is not the engine's
\ CHECK-DOES!: this scan reaches a clause AFTER the definer's own body has been
\ checked and recorded, so the checker must not latch the created effect here -
\ the next record belongs to the next definition. What this scan learns about a
\ definer it READ goes into the table above instead. It takes the definer's
\ name, which names the clause in the diagnostic of a rejected one.
: CHECK-DOES-BODY ( str str str -- n )
   NCOMP-DISPATCH:DECL-VERIFY-SOURCE-DOES-OFF OWNER-XT DOES-ACTION execute ;

PTR-VARIABLE TICK-DOES-SA
variable TICK-DOES-SU
PTR-VARIABLE TICK-DOES-NA
variable TICK-DOES-NU
variable TICK-DOES-VERDICT

\ The native compiler can reject an earlier parent token before it reaches a
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
TYPED-VARIABLE DOES-PARENT-REJECTED bool
PTR-VARIABLE DOES-CLAUSE-A
variable DOES-CLAUSE-U
variable DOES-CLAUSE-WHY
PTR-VARIABLE DOES-CLAUSE-NAME-A
variable DOES-CLAUSE-NAME-U

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
   DOES-CLAUSE-A @ DOES-CLAUSE-U @ >str DOES-CLAUSE-WHY @
   DOES-CLAUSE-NAME-A @ DOES-CLAUSE-NAME-U @ >str REPORT-DEFERRAL ;

: TICK-DOES-CHECK ( -- )
   TICK-BODY-A @ TICK-BODY-U @ >str
   TICK-DOES-SA @ TICK-DOES-SU @ >str TICK-DOES-NA @ TICK-DOES-NU @ >str
   CHECK-DOES-BODY TICK-DOES-VERDICT ! ;

: TICK-DOES-RUN ( -- )
   TICK-BODY-U @ ['] TICK-DOES-CHECK CSR-CHECK ;

: VERIFY-DOES-BODY ( str str -- n ) {: sig:str ns:str :}
   DEF-BODY$ {: b:str :}
   b STR:DATA TICK-BODY-A !  b STR:LEN TICK-BODY-U !
   sig STR:DATA TICK-DOES-SA !  sig STR:LEN TICK-DOES-SU !
   ns STR:DATA TICK-DOES-NA !  ns STR:LEN TICK-DOES-NU !
   b [: TICK-DOES-RUN ;] WITH-BODY-TICK-ORDER
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
   DEF-NAME-A @ DEF-NAME-U @ >str DEF-NAME-BYTE @ DEF-NAME-U @ ARM-UNSEALED
   VERIFY-BODY
   DISARM ;

: DOES-PARENT-RUN ( -- )
   VERIFY-NAMED-BODY DOES-DEF-VERDICT ! ;

: WRAP-RESET ( -- )
   0 WRAP-DEFINERS !  0 WRAP-CTL !  0 WRAP-ROW !  0 WRAP-MAKES ! ;

: DEFINER-RECORD-AS ( str str -- ) {: sig:str ns:str :}
   sig false ns RECORD-SYM? DEFINER-ADD ;

: DEFINER-RECORD ( str -- )
   DEF-NAME-A @ DEF-NAME-U @ >str DEFINER-RECORD-AS ;

\ A definer this pre-pass read is in its table; one compiled in the checking
\ process the checker answers.
: WRAP-TOKEN ( str -- ) {: s:str :}
   WRAP-CTL @ IF EXIT THEN
   s WRAP-CTL-TOK? IF -1 WRAP-CTL ! EXIT THEN
   s s" create" STR:=CI IF -1 WRAP-MAKES ! EXIT THEN
   s DEFINER-OF dup 0<> IF
      1 - WRAP-ROW !
      WRAP-DEFINERS @ 1 + WRAP-DEFINERS !  -1 WRAP-MAKES !  EXIT
   THEN
   drop
   WRAP-MAKES @ IF EXIT THEN
   s FIND-SYM CREATES-SYM? 0<> IF -1 WRAP-MAKES ! THEN ;

\ A head that reaches `does>` with no `create` or definer call before any bend
\ is rejected at its `does>`, the token t tu, as the load's checker rejects it.
: DOES-UNMADE-STOP ( str -- ) {: t:str :}
   t DOES-UNMADE-OPEN
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN ;

: VERIFY-WRAPPER ( -- )
   WRAP-CTL @ IF EXIT THEN
   WRAP-DEFINERS @ 1 <> IF EXIT THEN
   WRAP-ROW @  DEF-NAME-A @ DEF-NAME-U @ >str RECORD-SYM?  DEFINER-INHERIT ;

\ The live probe follows interpreter syntax. A selected public word can be
\ probed by its qualified spelling even when the verifier's using mirror does
\ not match the interpreter's; a private word has no qualified spelling and
\ cannot prove it will not execute at compile time.
DYNAMIC-BUFFER TICK-QUAL u8

\ A package's word as a caller outside it spells it, PKG:TAIL, copied into
\ TICK-QUAL, so it outlives the borrowed bytes of a symbol's identity.
: QUAL-SPELLING ( str str -- str )
   {: pkg:str tail:str :}
   pkg STR:LEN tail STR:LEN + 1+ TICK-QUAL-RESERVE
   pkg STR:DATA 0 TICK-QUAL pkg STR:LEN BYTE-COPY
   $3A 0 TICK-QUAL pkg STR:LEN + c!
   tail STR:DATA 0 TICK-QUAL pkg STR:LEN 1+ + tail STR:LEN BYTE-COPY
   0 TICK-QUAL pkg STR:LEN tail STR:LEN + 1+ >str ;

: TICK-RESIDENT-IMM? ( str -- bool )
   {: s:str :}
   s tok-imm? 0<> IF true EXIT THEN
   s FIND-SYM dup 0= IF drop false EXIT THEN
   SYM-IDENTITY {: pkg:str tail:str vis:n :}
   vis 1 = IF true EXIT THEN
   pkg STR:LEN 0= IF false EXIT THEN
   pkg tail QUAL-SPELLING tok-imm? 0<> ;

\ A compile-time action inside the current definition can change the compiler or
\ replace a checker before its later trusted tick. The source scan does not run
\ that action, so this definition and the following source lose the answer.
: TICK-BODY-STEP ( -- )
   DEF-TICK-ORDER @ TICK-UNKNOWN = IF EXIT THEN
   LOCAL-TOKEN? IF EXIT THEN
   TOKEN-A @ TOKEN-U @ >str STRING-OPENER? IF EXIT THEN
   TOKEN-A @ TOKEN-U @ >str s" [']" STR:= IF EXIT THEN
   TOKEN-A @ TOKEN-U @ >str s" [" STR:=
   TOKEN-A @ TOKEN-U @ >str TICK-RESIDENT-IMM? or IF
      TICK-UNKNOWN DEF-TICK-ORDER !  NULL-PTR DEF-TICK-OWNER !
      TICK-CONTEXT-UNKNOWN
   THEN ;

: DOES-TOKEN? ( -- bool )
   LOCAL-TOKEN? IF false EXIT THEN
   TOKEN-A @ TOKEN-U @ >str s" does>" STR:=CI ;

\ The load rejects a second `does>` as it reads it, before it checks any of the
\ definition: from the first, the read token, the rest is read for one, which is
\ then the read token. Else the scan ends where it stood.
: DOES-AGAIN? ( -- bool )
   SCAN-I @ TOKEN-A @ TOKEN-U @ >str TOKEN-BYTE @ CSR-SUBJ @
   {: at:n s:str byte:n subj:n :}
   0 CSR-SUBJ !
   LOCALS-RESET  BODY-PREV-CLEAR
   BEGIN
      BODY!
      DOES-TOKEN? IF subj CSR-SUBJ !  true EXIT THEN
      TOKEN-U @ 0=  TOKEN-A @ TOKEN-U @ >str s" ;" STR:=  or 0=
   WHILE SKIP-DEF-TOKEN REPEAT
   at SCAN-I !  s STR:DATA TOKEN-A !  s STR:LEN TOKEN-U !  byte TOKEN-BYTE !  subj CSR-SUBJ !
   LOCALS-RESET  BODY-PREV-CLEAR
   false ;

: DOES-TWICE-STOP ( -- )
   TOKEN-A @ TOKEN-U @ >str REJECTION:DOES-TWICE
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN ;

\ The clause's declaration is recorded unless body or clause stops or is
\ rejected. A body deferral hides the clause's, never the clause's stop.
: VERIFY-DOES ( -- bool )
   TOKEN-A @ TOKEN-U @ >str {: opener:str :}
   false DOES-ACCEPTED !
   DEF-TICK-ORDER @ {: order:n :}
   order TICK-GATE = {: gate:bool :}
   order TICK-UNKNOWN = {: unknown:bool :}
   false DOES-PARENT-REJECTED !
   0 DOES-DEF-VERDICT !
   0 DOES-CLAUSE-U !
   gate unknown or IF
      gate IF TICK-PARENT-GATE ELSE TICK-PARENT-UNKNOWN THEN DEF-TICK-ORDER !
      VERIFY-NAMED-BODY 2 <> DOES-PARENT-REJECTED !
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
      gate unknown or IF DOES-PARENT-REJECTED @ 0= ELSE DOES-DEF-VERDICT @ 0<> THEN IF
         opener DOES-UNMADE-STOP
         true DOES-PARENT-REJECTED !
      THEN
   THEN
   opener SIGNATURE-AT {: sig:str :}
   BODY-RESET
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ >str s" ;" STR:= IF
         TICK-REMAINDER @ DOES-PARENT-REJECTED @ or IF false EXIT THEN
         sig DEF-NAME-A @ DEF-NAME-U @ >str VERIFY-DOES-BODY
         {: clause:n :}
         TICK-REMAINDER @ IF clause REPORT-DEFERRED  false EXIT THEN
         clause 0= IF false EXIT THEN
         gate unknown or IF
            clause 2 = IF
               DEFERRED-BODY {: cs:str why:n cn:str :}
               cn STR:DATA DOES-CLAUSE-NAME-A !  cn STR:LEN DOES-CLAUSE-NAME-U !
               why DOES-CLAUSE-WHY !  cs STR:DATA DOES-CLAUSE-A !  cs STR:LEN DOES-CLAUSE-U !
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
            sig DEFINER-RECORD
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
\ The registrar's CHECKER-ROW outcome, which alone tells a kept redeclaration
\ from a rejected one. A quiet row's word is left to the run: it marks its name.
: DECL-SIGNATURE ( str str -- n ) {: name:str sig:str :}
   name sig
   NCOMP-DISPATCH:DECL-VERIFY-DECL-OFF OWNER-XT DECL-ACTION execute
   dup CHECKER-ROW:QUIET = IF name CHECKER-UNSEEN-NAME THEN ;

\ The shadow rule the load asks of a colon definition as it publishes it, for
\ NAME declared by SIG.
: DECL-SHADOW ( str str -- )
   NCOMP-DISPATCH:DECL-VERIFY-SHADOW-OFF OWNER-XT SHADOW-ACTION execute ;

\ The checker's own guard rejects a colon definition, a cast, a typed storage
\ definition and a re-export whose name its scope already holds, but a colon
\ definition only once its body is checked, a storage definition once its type
\ is read, and it keeps no place. Those rows ask the guard's question as soon as
\ they have scanned the name, as the engine's wall and the check hook reject it
\ under --load, a name only the checker holds included.
\ A definer the checker only trusts (variable, defer, TRUSTED:, ...) is left to
\ the run, which names its duplicate as --load does, or first rejects an
\ earlier definition whose text the engine cannot hold.

\ Whether the path is the subject's, whose bytes are the supplied ones.
: COMPOSE-SUBJ? ( str -- bool )
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ >str STR:= ;

: COMPOSE-DIAG$ ( str -- str )
   {: path:str :}
   path COMPOSE-SUBJ? IF COMPOSE-SUBJ-LABEL COMPOSE-SUBJ-LABEL-U @ >str EXIT THEN
   path ;

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
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ >str COMPOSE-DIAG$ ;

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
defer SUPPLIED ( str -- str bool )

private

\ A load's place is where the file being scanned writes its path; one entry
\ text makes has none, as the load's command-line text's has none.
: LOAD-PLACE! ( str -- ) {: s:str :}
   ENTRY-TEXT @ SUBJECT? and IF
      NULL-PTR LOAD-PLACE-A !  0 LOAD-PLACE-U ! EXIT
   THEN
   s STR:DATA LOAD-PLACE-A !  s STR:LEN LOAD-PLACE-U ! ;

\ These declaration arms are dispatched before dictionary lookup. Their scanner
\ counterparts do no arbitrary execution.
\ A word-backed loader or library definer has no such structural guarantee:
\ even the original include/require entry can call replaced source providers.
: TICK-NATIVE-DEFINER? ( str -- bool )
   {: s:str :}
   s s" package" STR:=CI  s s" ;package" STR:=CI or
   s s" public" STR:=CI or  s s" private" STR:=CI or  s s" global" STR:=CI or
   s s" seal-package" STR:=CI or  s s" host" STR:=CI or
   s s" target" STR:=CI or IF 0 0= EXIT THEN
   s s" export" STR:=CI  s s" trusted:" STR:=CI or
   s s" cast:" STR:=CI or  s s" linear:" STR:=CI or
   s s" defer" STR:=CI or  s s" create" STR:=CI or
   s s" variable" STR:=CI or  s s" constant" STR:=CI or ;

: DEFINITION-NONE ( str str n str n n n n -- )
   2drop 2drop 2drop 2drop ;

: DEFINITION-INIT ( -- )
   ['] DEFINITION-NONE is ON-DEFINITION ;

DEFINITION-INIT

: FILE-NONE ( str str -- )  2drop ;

: FILE-INIT ( -- )
   ['] FILE-NONE is ON-FILE ;

FILE-INIT

: SUPPLIED-NONE ( str -- str bool )  drop NULL$ false ;

: SUPPLIED-INIT ( -- )
   ['] SUPPLIED-NONE is SUPPLIED ;

SUPPLIED-INIT

: USE-NONE ( n n str n n n n -- )
   2drop 2drop 2drop drop ;

: USE-INIT ( -- )
   ['] USE-NONE is ON-USE ;

USE-INIT

: LOADER-NONE ( n n str n -- )
   2drop 2drop ;

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
   PRIM-SOURCE:ENGINE-PATH$ {: path:str ready:bool :}
   ready 0= if NULL$ 0 0 false exit then
   path SUPPLIED {: src:str open:bool :}
   open if
      PRIM-OPEN-READY @ 0= if
         1 PRIM-OPEN-READY !
         src row PRIM-SOURCE:TARGET-IN
      else
         row PRIM-SOURCE:TARGET-ROW
      then
      {: at:n end:n found:bool :}
      path at end found exit
   then
   row PRIM-SOURCE:TARGET ;

\ The checker replays a colon's header while checking its body. Its name can
\ still bind an older word there, but that token declares the new word; it is
\ not a use of the older one.
: COLON-NAME-USE? ( n n -- bool )
   {: s:n e:n :}
   TOP-CUR-A @ TOP-CUR-U @ >str s" :" STR:=CI
   TOP-CUR-A @ TOP-CUR-U @ >str s" kernel:" STR:=CI or
   s DEF-NAME-BYTE @ = and
   e DEF-NAME-BYTE @ DEF-NAME-U @ + = and ;

: USE-SEEN ( n n n n n n -- )
   {: s:n e:n sym:n v:n ds:n de:n :}
   s e COLON-NAME-USE? IF EXIT THEN
   v -2 <= IF
      -2 v - PRIM-TARGET drop {: path:str at:n end:n :}
      s e
      path at end sym v ON-USE
      EXIT
   THEN
   v 0= IF
      sym ENGINE-TARGET {: path:str at:n end:n found:bool :}
      found IF
         s e
         path at end sym -1 ON-USE
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
   u 0= IF TOKEN-BYTE @ TOKEN-FROM ELSE SOURCE@ TOKEN-BYTE @ + u >str THEN
   dup DUPLICATE-AT! ;

\ Whether a u, written from byte AT, is a keyword or number name; the record
\ ends as a duplicate's.
: NAME-REJECT? ( str n -- bool )
   {: s:str at:n :}
   s SOURCE@ at + s STR:LEN >str DEF-NAME-REJECT? 0= IF false EXIT THEN
   MULTI-ERR? IF REJECTION:KEEP ELSE EXIT-STATUS:REJECTED REJECTION:STOP THEN
   true ;

\ A keyword or number name stops first, then one that lands in a sealed package.
: REJECT-DUPLICATE ( str n -- bool )
   {: s:str at:n :}
   s at NAME-REJECT? IF true EXIT THEN
   s at s STR:LEN SEALED-NAME
   s CHECKER-CERT-DUP? dup IF s STR:LEN DUPLICATE! THEN ;

\ Whether the scan rejected the definition NAME, written from byte AT, now that
\ it knows whether the definition makes a does> clause (CLAUSE): a colon
\ definition at the `;` or `does>` that ends its body, before the body is
\ checked, and a TRUSTED: one once its body is read, before its clause is
\ published. A record of the name from before the replay that has the other
\ shape is no loaded twin of it, and the live engine rejects the definition
\ there.
: REJECT-SHAPE ( str n bool -- bool )
   {: s:str at:n clause:bool :}
   s clause CHECKER-CERT-SHAPE-DUP? 0= IF false EXIT THEN
   at TOKEN-BYTE !
   s STR:LEN DUPLICATE!
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
   {: s:str name:str at:n :}
   s SOURCE-SEALED? IF name at name STR:LEN SEAL-STOP THEN
   s ROW-SEALED? IF name at name STR:LEN SEAL-STOP THEN
   SEALED-LOAD? 0= IF EXIT THEN
   s PUBLIC-WID XREF-WID-PROTECTED? IF name at name STR:LEN SEAL-STOP THEN ;

\ A guarded declaration's live record is the one its registrar kept; an
\ unguarded one's only once DECL-SIGNATURE answered CHECKER-ROW:KEPT.
: RETAINED-SYM ( str -- n )
   {: name:str :}
   name CHECKER-CERT-DUP? 0= IF 0 EXIT THEN
   name RECORD-SYM? ;

: DEFINED ( str str str n n n -- )
   {: kind:str name:str eff:str at:n end:n class:n :}
   name RETAINED-SYM
   {: sym:n :}
   sym NEWEST-SYM !
   sym 0= IF EXIT THEN
   kind name sym eff STR:TRIM at end class VISIT-CUR @ ON-DEFINITION ;

: DEFINED-HERE ( str str n n -- )
   {: name:str eff:str at:n class:n :}
   TOP-CUR-A @ TOP-CUR-U @ >str name eff at at name STR:LEN + class DEFINED ;

\ The effect of NAME's retained newest record, or empty when there is none or
\ TEXT cannot say it; it lies past TEXT-LEN, where the next TEXT write lands.
: MADE-EFFECT ( str -- str )
   RETAINED-SYM {: sym:n :}
   sym 0= IF s" " EXIT THEN
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= IF s" " EXIT THEN
   REJECTION:TEXT-LEN @ RQM @ REJECTION:DEST @ {: base:n qm:n dst:n :}
   rec1 1 - REND-RECORD-EFFECT {: e:str :}
   RQM @ {: unsaid:n :}
   base REJECTION:TEXT-LEN !  qm RQM !  dst REJECTION:DEST !
   unsaid 0<> IF s" " EXIT THEN
   e STR:DATA base +  e STR:LEN base - >str ;

\ DEFINED-HERE for a word a definer made, with the effect its record carries.
: DEFINED-MADE ( str n n -- )
   {: name:str at:n class:n :}
   name  name MADE-EFFECT  at class DEFINED-HERE ;

: SIG-RAW-MODE! ( n -- ) SIG-RAW-MODE ! ;

\ The keyword the engine's rejection names for the storage statement's name:
\ `constant` and `variable` read their own, every other definer's reaches
\ `create`.
: STORAGE-KEYWORD ( -- str )
   TOP-CUR-A @ TOP-CUR-U @ >str {: s:str :}
   s s" constant" STR:=CI IF s" constant" EXIT THEN
   s s" variable" STR:=CI IF s" variable" EXIT THEN
   s" create" ;

\ Registers the effect for the word the next token names with TVK-RAW type
\ vars, so its cell cannot launder into a nominal atom or family. Answers the
\ name and whether the registrar retained it.
: RAW-TRUST-NAME ( str n -- str bool )
   {: sig:str class:n :}
   NAME-TOKEN
   dup STR:LEN 0= IF NAME-STOP THEN
   TOKEN-BYTE @
   {: name:str at:n :}
   STORAGE-KEYWORD name REJECTION:NOT-A-NAME
   NAME-DECLARED
   name at REJECT-DUPLICATE IF name false EXIT THEN
   name at name STR:LEN ARM
   -1 SIG-RAW-MODE!
   name sig [: DECL-SIGNATURE ;] [: 0 SIG-RAW-MODE! DISARM ;] finally
   CHECKER-ROW:KEPT = {: kept:bool :}
   kept IF name sig at class DEFINED-HERE THEN
   name kept ;

: RAW-TRUST-NEXT ( str n -- ) RAW-TRUST-NAME drop drop ;

EXPORT CHECKER-OVERLAY:LANDS-REC
EXPORT CHECKER-OVERLAY:VALUE
EXPORT CHECKER-OVERLAY:VALUE!
EXPORT CHECKER-OVERLAY:UNSTATED!

\ The value a token gives and true: an integer literal or the value of the
\ constant it names; false for anything else.
: STATED-VALUE ( str -- n bool ) {: s:str :}
   s num-parse {: v:n flt:bool ok:bool :}
   ok IF v flt 0= EXIT THEN
   s scope-find drop drop drop VALUE ;

\ A `constant`'s value is stated when the token before it gives one and no
\ skipped string literal or open stretch can have pushed another.
: RECORD-CONSTANT ( -- )
   STR-LAST-KIND @ 0=  TOP-DEFER @ 0= and  TOP-PREV-U @ 0<> and {: plain:bool :}
   plain IF TOP-PREV-A @ TOP-PREV-U @ >str STATED-VALUE ELSE 0 false THEN
   {: v:n stated:bool :}
   s" -- a" DEF-CONSTANT RAW-TRUST-NAME {: name:str kept:bool :}
   kept 0= IF EXIT THEN
   name LANDS-REC {: rec:^ :}
   stated IF rec v VALUE! EXIT THEN
   rec UNSTATED! ;

: CREATED-TRUST-NEXT ( n -- )
   {: dsym:n :}
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   STORAGE-KEYWORD name REJECTION:NOT-A-NAME
   NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name at REJECT-DUPLICATE IF EXIT THEN
   name at name STR:LEN ARM
   name dsym RECORD-CREATED
   DISARM
   IF name at DEF-STORAGE DEFINED-MADE THEN ;

: TRUST-DEFER-SIGNATURE ( str n -- )
   {: name:str at:n :}
   name SIGNATURE-AT {: sig:str :}
   name at REJECT-DUPLICATE IF EXIT THEN
   name at name STR:LEN ARM
   -1 SIG-STATES-FACTS !
   name sig [: DECL-SIGNATURE ;] [: 0 SIG-STATES-FACTS ! ;] finally
   {: row:n :}
   row CHECKER-ROW:KEPT = {: kept:bool :}
   kept IF name CHECKER-DEFER THEN
   DISARM
   kept IF name sig at DEF-WORD DEFINED-HERE THEN
   row CHECKER-ROW:QUIET = IF QUIET-TYPE-REPORT THEN ;

: TRUST-DEFER ( -- )
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   NAME-DECLARED
   name TOKEN-BYTE @ TRUST-DEFER-SIGNATURE ;

variable TRUSTED-DOES

\ A token of the trusted body NAME before its `does>`, asked of the checker
\ when it may call a word.
: TRUSTED-CALL ( str -- ) {: ns:str :}
   TRUSTED-DOES @ LOCAL-TOKEN? or IF EXIT THEN
   TOKEN-A @ TOKEN-U @ >str {: s:str :}
   s s" {:" STR:=  s BODY-PARSER? or  s STRING-OPENER? or IF EXIT THEN
   s num-parse nip nip IF EXIT THEN
   s WRAP-CTL-TOK? IF EXIT THEN
   s s" i" STR:=CI  s s" j" STR:=CI or  s s" unloop" STR:=CI or IF EXIT THEN
   s s" is" STR:=CI IF OPERAND drop EXIT THEN
   s s" [package]" STR:=CI IF EXIT THEN
   s ns TRUSTED-REACH ;

variable TRUSTED-CUT              \ where BODY-BUF's clause body starts, 0 for none
PTR-VARIABLE TRUSTED-SIG-A
variable TRUSTED-SIG-U

\ The clause's signature is read and its body joins with locals of its own.
: APPEND-CLAUSE ( -- )
   TOKEN-A @ TOKEN-U @ >str SIGNATURE-AT dup STR:LEN TRUSTED-SIG-U !  STR:DATA TRUSTED-SIG-A !
   LOCALS-RESET  BODY-PREV-CLEAR
   BODY-U @ TRUSTED-CUT ! ;

\ Appends a TRUSTED: definition's body, its clause's after it, up to the `;`:
\ false at a second `does>`, which SCAN-TRUSTED-BODY rejects.
: APPEND-TRUSTED ( -- bool )
   BODY-PREV-CLEAR  LOCALS-RESET
   0 TRUSTED-CUT !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ >str s" ;" STR:= IF true EXIT THEN
      DOES-TOKEN? IF
         TRUSTED-CUT @ 0<> IF false EXIT THEN
         APPEND-CLAUSE
      ELSE
         APPEND-BODY-TOKEN
      THEN
   AGAIN ;

\ The require checks of the appended text before the clause.
: TRUSTED-REQUIRES ( -- )
   REQUIRES:CURRENT@ 0 <= IF EXIT THEN
   BODY$ {: s:str :}
   TRUSTED-CUT @ {: cut:n :}
   s STR:DATA  cut 0= IF s STR:LEN ELSE cut THEN >str  CHECKER-REQUIRED-BODY ;

\ The verdict on the appended clause of NAME, whose `;` is the read token; -1
\ for no clause.
: TRUSTED-DOES-CHECK ( str -- n ) {: ns:str :}
   TRUSTED-CUT @ {: cut:n :}
   cut 0= IF -1 EXIT THEN
   BODY$ {: s:str :}
   TOKEN-A @ TOKEN-U @ >str s STR:DATA cut + REJECTION:CLOSER!
   s STR:DATA cut +  s STR:LEN cut - >str  TRUSTED-SIG-A @ TRUSTED-SIG-U @ >str ns
   CHECKER-TRUSTED-DOES! BODY-VERDICT ;

\ The verdict on NAME's clause, after the head's require checks, as the load
\ makes both before it declares NAME's row: 0 for a second does>, -1 for no
\ clause or in an index. The scan ends where it stood, since the body is read
\ again.
: TRUSTED-TEXT-CHECK ( str str -- n )
   {: ns:str sig:str :}
   INDEX-ON @ IF -1 EXIT THEN
   SCAN-I @ {: mark:n :}
   BODY-RESET
   ns BODY-APPEND  sig STR:DATA 1 - sig STR:LEN 2 + >str BODY-APPEND
   APPEND-TRUSTED IF TRUSTED-REQUIRES  ns TRUSTED-DOES-CHECK ELSE 0 THEN
   dup REPORT-DEFERRED
   BODY-RESET
   mark SCAN-I ! ;

\ A `does>` outside a string declares the clause. No judgement is made of the
\ tokens here, but what those before it may do when run, the word may do: KEPT
\ says whether its row stands.
: SCAN-TRUSTED-BODY ( str bool -- ) {: ns:str kept:bool :}
   BODY-PREV-CLEAR
   LOCALS-RESET
   0 TRUSTED-DOES !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ >str s" ;" STR:= IF EXIT THEN
      DOES-TOKEN? IF
         TRUSTED-DOES @ 0<> IF DOES-TWICE-STOP THEN
         \ An index checks no TRUSTED: clause, so this one's acceptance is unknown.
         INDEX-ON @ IF
            true TICK-REMAINDER !
            TOKEN-A @ TOKEN-U @ >str 0 DEFER-AT EXIT
         THEN
         TOKEN-A @ TOKEN-U @ >str SIGNATURE-AT
         kept IF ns DEFINER-RECORD-AS ELSE drop THEN
         -1 TRUSTED-DOES !
      ELSE
         kept IF ns TRUSTED-CALL THEN
         SKIP-DEF-TOKEN
      THEN
   AGAIN ;

\ The engine's `does>` makes the clause record `<definer>;does` for a TRUSTED:
\ definer as for a checked one, so a definer whose `does>` the scan read gets it
\ at its `;`: a later definition of that name is rejected, as in the live load.
: TRUSTED-DEFINITION ( -- )
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   s" trusted:" name REJECTION:NOT-A-NAME
   NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name at REJECT-DUPLICATE IF
      name SIGNATURE-AT drop  name false SCAN-TRUSTED-BODY EXIT
   THEN
   name SIGNATURE-AT {: sig:str :}
   name sig TRUSTED-TEXT-CHECK 0= IF
      name false SCAN-TRUSTED-BODY EXIT
   THEN
   name at name STR:LEN ARM
   name sig DECL-SIGNATURE
   {: row:n :}
   row CHECKER-ROW:KEPT = {: kept:bool :}
   DISARM
   row CHECKER-ROW:QUIET = IF QUIET-TYPE-REPORT THEN
   name kept SCAN-TRUSTED-BODY
   TICK-REMAINDER @ IF EXIT THEN
   name at TRUSTED-DOES @ 0<> REJECT-SHAPE IF EXIT THEN
   TRUSTED-DOES @ kept and IF name TRUSTED-CLAUSE THEN
   kept IF name sig at DEF-WORD DEFINED-HERE THEN ;

\ Registration goes through the certifying registrar, not DECL-SIGNATURE, so
\ an illegal retype is rejected here too and not merely recorded.
: CAST-DECLARATION ( -- )
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name at REJECT-DUPLICATE IF name SIGNATURE-AT drop EXIT THEN
   name SIGNATURE-AT {: sig:str :}
   name at name STR:LEN ARM
   name sig CHECKER-DEFCAST
   DISARM
   name sig at DEF-WORD DEFINED-HERE ;

\ A `linear:` row is certified by the engine's own registrar, so a mint or
\ erase this pre-pass accepts is exactly one the engine accepts.
: LINEAR-DECLARATION ( -- )
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   s" linear:" name REJECTION:NOT-A-NAME
   NAME-DECLARED
   TOKEN-BYTE @ {: at:n :}
   name at NAME-REJECT? IF name SIGNATURE-AT drop EXIT THEN
   name SIGNATURE-AT {: sig:str :}
   name at name STR:LEN ARM
   name sig CHECKER-LINEAR
   DISARM
   name sig at DEF-WORD DEFINED-HERE ;

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
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   name XREF:UNDEFINE-FIND 0< IF
      name UNSEEN-COVERS? IF  name DW-RENDERED DEFER-AT  EXIT  THEN
      name XREF:UNDEFINE-INDEX drop
   THEN
   name CHECKER-UNDEFINE
   name RECORD-SYM? {: sym:n :}
   sym NEWEST-SYM @ = IF 0 NEWEST-SYM ! THEN
   sym DEFINER-RETIRE ;

\ Rejects a package keyword with no package open, as `--load` does, unless
\ rendered text the run alone reads may have opened one; then the run judges it.
: PACKAGE-KEYWORD-GUARD ( -- bool )
   CHECKER-AUTH-PACKAGE-ACTIVE? IF true EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str {: s:str :}
   s UNSEEN-COVERS? IF s DW-RENDERED DEFER-AT  false EXIT THEN
   s INTERPRETER:PKG-CONTEXT-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: SEAL-ROW ( -- )
   PACKAGE-KEYWORD-GUARD IF CHECKER-AUTH-PACKAGE$ SOURCE-SEAL THEN ;

EXPORT CHECKER-RESOLVE:PART-GUARD
EXPORT CHECKER-RESOLVE:PART-PATH

\ The engine's order: the part guard at `package`, then the name and its path,
\ the checker, the seal on the path.
: RECORD-PACKAGE ( -- )
   PART-GUARD
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   name PART-PATH {: ps:str :}
   ps name CHECKER-PACKAGE
   ps name at SEALED-PACKAGE ;

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
\ imports do nothing and a name it imports is rejected E-UNDEFINED. The checker
\ owns the replay's using depth, so these two rows are the only thing that
\ moves it.
: RECORD-USING ( -- )
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   name OPEN-USING ;

\ Takes the `;using` token.
: RECORD-END-USING ( str -- )
   CLOSE-USING ;

\ DEFTYPE NAME declares a value nominal: a package-scoped arity-0 type family
\ whose lowercase tail is the surface name folded (SERIAL -> serial) and whose
\ converter pair >NAME ( n -- tail ) / NAME>N ( tail -- n ) keeps a plain n from
\ standing in for the nominal.
DYNAMIC-BUFFER NOM-TAIL u8

: MANGLE ( str -- str ) {: s:str :}
   s STR:LEN NOM-TAIL-RESERVE
   0 BEGIN dup s STR:LEN < WHILE
      dup s STR:DATA + c@ FOLD-C  over NOM-TAIL c!
      1+
   REPEAT drop
   0 NOM-TAIL s STR:LEN >str ;

: CAST-DEFINED ( n n -- )
   {: at:n u:n :}
   TOP-CUR-A @ TOP-CUR-U @ >str DTC-NAME$ DTC-SIG$ at at u + DEF-WORD DEFINED ;

\ Under MULTI-ERR? the scan goes on and the caller registers nothing, so
\ TOP-DEFINER? opens an unreported stretch.
: DECLARATION-END ( -- )
   MULTI-ERR? IF REJECTION:KEEP EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: RECORD-DEFLINEAR ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   name name DEF-NAME-REJECT? IF DECLARATION-END EXIT THEN
   name TYPE-RESERVED? IF
      name CHECKER-LINEAR-TAKEN  DECLARATION-END EXIT
   THEN
   name CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( str -- bool )
   s" END-VALUE-RECORD" STR:=CI ;

: SUMTYPE-END? ( str -- bool )
   s" ;SUMTYPE" STR:=CI ;

\ A SUMTYPE defines its constructors when it loads; the scan replays its body
\ through the same front end, which publishes their checked effects, so a later
\ definition that calls one resolves here as in the load.
DYNAMIC-BUFFER SUM-VAR-AT n
variable SUM-VAR-N
variable SUM-VAR-NEXT

: SUM-VAR-STEP ( str -- )
   {: s:str :}
   SUM-VAR-NEXT @ IF
      SUM-VAR-N @ 1+ SUM-VAR-AT-RESERVE
      TOKEN-BYTE @ SUM-VAR-N @ SUM-VAR-AT !
      SUM-VAR-N @ 1+ SUM-VAR-N !
   THEN
   s s" VARIANT" STR:=CI SUM-VAR-NEXT ! ;

\ Constructor symbols belong to the variants just registered. Give each its
\ source name token, in the same order the declaration registered the variants.
: SUM-VAR-DEFINED ( n -- )
   {: fam:n :}
   fam TFAM-VAR-START@ {: start:n :}
   SUM-VAR-N @ 0 ?DO
      start i + {: vid:n :}
      vid SUMV-CTOR-SYM@ {: sym:n :}
      sym 0 <> IF
         vid SUMV-NAME$ {: name:str :}
         i SUM-VAR-AT @ {: at:n :}
         sym VISIT-CUR @ at at name STR:LEN +
            NCOMP-DISPATCH:DECL-VERIFY-CTOR-DECL-OFF OWNER-XT CTOR-DECL-ACTION execute
         s" VARIANT" name sym NULL$ at at name STR:LEN + DEF-CONSTRUCTOR VISIT-CUR @ ON-DEFINITION
      THEN
   LOOP ;

\ A declaration body token, comments skipped as NEXT skips them, so a replayed
\ body holds none.
: DECL-TOKEN ( -- str )
   BEGIN
      NEXT-RAW dup COMMENT-END dup 0<>
   WHILE
      nip SKIP-PAST CSR-SKIPPED
   REPEAT
   drop ;

EXPORT TYPE-DECL:TDECL-SUMTYPE

: RECORD-SUMTYPE-BODY ( str -- )
   {: name:str :}
   0 SUM-VAR-N !  0 SUM-VAR-NEXT !
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup STR:LEN 0= IF drop 0 0= ELSE
         dup BODY-APPEND
         dup SUMTYPE-END? IF drop 0 0= ELSE SUM-VAR-STEP 0 0= 0= THEN
      THEN
   UNTIL
   TFAM-N@ {: before:n :}
   \ The front reads its keyword as the token read last, as on the native path.
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   TOP-CUR-A @ TOP-CUR-U @ SRCLOC:TOKEN!
   name BODY$
   [: [: TDECL-SUMTYPE ;] DECL-REPLAY:RP-RUN ;]
   [: oa ou SRCLOC:TOKEN! ;] finally
   TFAM-N@ before > IF
      TFAM-N@ 1- SUM-VAR-DEFINED
   THEN ;

: RECORD-SUMTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   name at SEALED-FAMILY
   name [: RECORD-SUMTYPE-BODY ;] [: SUM-VAR-AT-RELEASE ;] finally ;

: ENUM-END? ( str -- bool )
   s" ;ENUM" STR:=CI ;

\ The body keeps its ;ENUM, which the front end parses itself; one the input
\ ends inside replays without it, and the front end rejects "missing ;ENUM".
: RECORD-ENUM ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   name at SEALED-FAMILY
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup STR:LEN 0= IF drop 0 0= ELSE dup BODY-APPEND ENUM-END? THEN
   UNTIL
   name BODY$ REPLAY-ENUM
   dup 0 <> IF DEFER-AT ELSE drop drop THEN ;

: STRUCTURE-DECL-END? ( str -- bool )
   s" ;STRUCTURE" STR:=CI ;

\ The replay registers MAKE/UNMAKE, so `FAMILY:MAKE` resolves later in this
\ source; an unclosed body replays as RECORD-ENUM's does: "missing ;STRUCTURE".
: RECORD-STRUCTURE-DECL ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   name at SEALED-FAMILY
   BODY-RESET
   BEGIN
      DECL-TOKEN
      dup STR:LEN 0= IF drop 0 0= ELSE dup BODY-APPEND STRUCTURE-DECL-END? THEN
   UNTIL
   name BODY$ REPLAY-STRUCTURE
   dup 0 <> IF DEFER-AT ELSE drop drop THEN ;

: MUTUAL-END? ( str -- bool )
   s" ;MUTUAL" STR:=CI ;

\ Whether a MUTUAL's first token names a family to seal: a member's STRUCTURE,
\ ;MUTUAL and a nameless header do not. The replay judges every later entry.
: MUTUAL-LISTED? ( str -- bool ) {: s:str :}
   s DECL-FAMILY-NAME STR:LEN 0<>
   s s" STRUCTURE" STR:=CI 0= and  s MUTUAL-END? 0= and ;

\ Appends the next MUTUAL body token; answers whether the body ended.
: MUTUAL-TOKEN ( -- bool )
   DECL-TOKEN {: s:str :}
   s STR:LEN 0= IF true EXIT THEN
   s BODY-APPEND
   s MUTUAL-END? ;

\ The replay registers every member's MAKE/UNMAKE and accessors, so
\ `NODE:MAKE` resolves later in this source. The first list token is the
\ replay's head; an unclosed body replays as RECORD-STRUCTURE-DECL's does:
\ "missing ;MUTUAL".
: RECORD-MUTUAL ( -- )
   DECL-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   name MUTUAL-LISTED? IF name at SEALED-FAMILY THEN
   BODY-RESET
   name MUTUAL-END? 0= IF
      BEGIN MUTUAL-TOKEN UNTIL
   THEN
   name BODY$ REPLAY-MUTUAL
   dup 0 <> IF DEFER-AT ELSE drop drop THEN ;

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
   SOURCE@ end +  SCAN-I @ end - >str  CHECKER-TYPE-SPAN-BREAK? IF SOURCE@ 0 >str EXIT THEN
   NEXT-RAW ;

: SCAN-STORAGE-MORE? ( n bool -- n bool ) {: depth:n ended:bool :}
   ended IF depth 0 0= 0= EXIT THEN
   depth 0 > IF NEXT-RAW ELSE SCAN-LINE-TOKEN THEN {: s:str :}
   s STR:LEN 0= IF depth 0 0= 0= EXIT THEN
   s STR:DATA STG-A !  s STR:LEN STG-U !
   depth 0 0= ;

: SCAN-STORAGE-TYPE ( -- str )
   SCAN-LINE-TOKEN dup STR:LEN STG-U !  STR:DATA STG-A !
   STG-A @ STG-START !
   0 BEGIN STG-A @ STG-U @ >str CHECKER-TYPE-SPAN-STEP SCAN-STORAGE-MORE? 0= UNTIL drop
   STG-START @  STG-A @ STG-U @ + STG-START @ - >str
   dup dup STR:LEN STG-TYPE-U !  STR:DATA STG-TYPE-A ! ;

\ Whether the next token on the line is the SCRATCH-ONLY clause, consumed; any
\ other is left unread.
: SCAN-SCRATCH-CLAUSE? ( -- bool )
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN s" scratch-only" STR:=CI IF 0 0= EXIT THEN
   mark SCAN-I !  0 0= 0= ;

: NEWTYPE-CELL ( str -- ) {: name:str :}
   name name s" newtype" CHECKER-DEFFAMILY ;

\ CHECKER-DEFFAMILY's record rejects a missing name, at the keyword, or a
\ malformed header; a base on the name's line replays as NEWTYPE registers it.
: RECORD-NEWTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   name STR:LEN 0= IF name TOP-CUR-A @ TOP-CUR-U @ >str s" newtype" CHECKER-DEFFAMILY EXIT THEN
   name at SEALED-FAMILY
   0 0= 0= name STR:LEN 0 ?do name STR:DATA i + c@ 60 = or loop
   IF name NEWTYPE-CELL EXIT THEN
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN {: ts:str :}
   mark SCAN-I !
   ts STR:LEN 0= IF name NEWTYPE-CELL EXIT THEN
   ts STR:LEN 1 = IF ts STR:DATA c@ 92 = IF name NEWTYPE-CELL EXIT THEN THEN
   name name SCAN-STORAGE-TYPE
   s" newtype" SD-WRAP-OUTCOME
   dup 0 <> IF DEFER-AT EXIT THEN drop drop ;

\ A DEFTYPE base is the one name on the name's line, consumed; none, or a `\`
\ left unread, is `n`, and the bool says whether one was spelled.
: DEFTYPE-BASE ( -- str bool )
   SCAN-I @ {: mark:n :}
   SCAN-LINE-TOKEN {: bs:str :}
   bs STR:LEN 0= IF mark SCAN-I !  s" n" 0 0= 0= EXIT THEN
   bs STR:LEN 1 = IF bs STR:DATA c@ 92 = IF mark SCAN-I !  s" n" 0 0= 0= EXIT THEN THEN
   bs 0 0= ;

\ A family left to the run declares no converters, so each is marked where it
\ would land: a use defers.
: CONVERTERS-LEFT ( str str str -- ) {: ns:str ts:str bs:str :}
   ns ts bs DTC-BUILD-IN   DTC-NAME$ CHECKER-UNSEEN-NAME
   ns ts bs DTC-BUILD-OUT  DTC-NAME$ CHECKER-UNSEEN-NAME ;

\ A base replays as DEFTYPE registers it; a rejected family declares no
\ converters.
: RECORD-DEFTYPE ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   TOKEN-BYTE @ {: at:n :}
   name MANGLE {: tail:str :}
   DEFTYPE-BASE {: bs:str based:bool :}
   based IF name bs SD-ONE-NAME 0= IF EXIT THEN THEN
   REJECTION:TALLY {: rejected:n :}
   based IF name at name STR:LEN SEALED-NAME ELSE name at SEALED-FAMILY THEN
   based IF
      tail name bs s" deftype" SD-WRAP-OUTCOME
      dup 0 <> IF name tail bs CONVERTERS-LEFT DEFER-AT EXIT THEN
      drop drop
   ELSE
      tail name s" deftype" CHECKER-DEFFAMILY
   THEN
   REJECTION:TALLY rejected <> IF EXIT THEN
   name tail bs DTC-BUILD-IN
   DTC-NAME$ at name STR:LEN ARM              \ the converter's own name, the TYPE's span
   DTC-NAME$ CHILD-ABI:NOMINAL-IN CHECKER-NOMINAL-REPLAY
   DISARM
   at name STR:LEN CAST-DEFINED
   name tail bs DTC-BUILD-OUT
   DTC-NAME$ at name STR:LEN ARM
   DTC-NAME$ CHILD-ABI:NOMINAL-OUT CHECKER-NOMINAL-REPLAY
   DISARM
   at name STR:LEN CAST-DEFINED ;

\ A name rejected as malformed or a duplicate is empty and still owns its type
\ span; consume it before resuming the statement scan so a type token cannot
\ be interpreted as a new statement.
: SCAN-STORAGE-NAME ( -- str )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN
   dup STR:LEN 0= IF TOP-CUR-A @ TOP-CUR-U @ >str CHECKER-STORAGE-NAME-REJECT EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str over REJECTION:NOT-A-NAME
   dup CHECKER-LBUF-NAME-OK? IF
      dup TOKEN-BYTE @ REJECT-DUPLICATE 0= IF EXIT THEN
   THEN
   drop SCAN-STORAGE-TYPE drop SOURCE@ 0 >str ;

\ The load stops at a count token it does not resolve, rejected there as a
\ top-level token, so the declaration after it is read but not judged: its
\ count is rejected once.
: COUNT-REJECTED? ( str -- bool )
   {: s:str :}
   s STR:LEN 0<>  s STR:DATA TOP-REJECTED @ =  and ;

\ Whether NAME+SFX, a word the definer derives from NAME, read from byte AT,
\ is free: a taken one is the duplicate, rejected at NAME.
: DERIVED-FREE? ( str str n -- bool )
   {: name:str sfx:str at:n :}
   name sfx CHECKER-LBUF-SUFFIXED$ {: gen:str :}
   gen CHECKER-LBUF-NAME-OK? 0= IF false EXIT THEN
   gen CHECKER-CERT-DUP? 0= IF true EXIT THEN
   gen SOURCE@ at + name STR:LEN >str DUPLICATE-AT!  false ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ >str {: count:str :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   count COUNT-REJECTED? IF EXIT THEN
   name at name STR:LEN ARM
   type count name CHECKER-DEFLAYOUT-BUFFER
   name type LBUF:MIXED-SAVED? drop
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

\ DEFER-LAYOUT-BUFFER publishes its accessor and NAME-BIND and NAME-GROW from
\ one line, so a later definition calling one is E-UNDEFINED without this row,
\ and a type it cannot size goes unreported until the run. No count token: the
\ count arrives at the bind.
: RECORD-DEFER-LAYOUT-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   name s" -BIND" at DERIVED-FREE? 0= IF EXIT THEN
   name s" -GROW" at DERIVED-FREE? 0= IF EXIT THEN
   name at name STR:LEN ARM
   type name CHECKER-DEFDEFER-LAYOUT-BUFFER
   name type LBUF:MIXED-SAVED? drop
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ >str {: count:str :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF SCAN-SCRATCH-CLAUSE? drop EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   SCAN-SCRATCH-CLAUSE? {: scratch:bool :}
   count COUNT-REJECTED? IF EXIT THEN
   name at name STR:LEN ARM
   type count name CHECKER-DEFTYPED-BUFFER
   scratch 0= IF
      name type LBUF:MIXED-SAVED? drop
   THEN
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

: RECORD-TYPED-VARIABLE ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF SCAN-SCRATCH-CLAUSE? drop EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   SCAN-SCRATCH-CLAUSE? {: scratch:bool :}
   name at name STR:LEN ARM
   type name CHECKER-DEFTYPED-VARIABLE
   scratch 0= IF
      name type LBUF:MIXED-SAVED? drop
   THEN
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

: RECORD-BYTE-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ >str {: count:str :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   count COUNT-REJECTED? IF EXIT THEN
   name at name STR:LEN ARM
   type count name CHECKER-DEFBYTE-BUFFER
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;


: RECORD-CWORK-SPAN ( -- )
   TOP-PREV-A @ TOP-PREV-U @ >str {: count:str :}
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   count COUNT-REJECTED? IF EXIT THEN
   name at name STR:LEN ARM
   type count name CHECKER-DEFCWORK-SPAN
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

: RECORD-SLOT ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   name at name STR:LEN ARM
   type name CHECKER-DEFSLOT
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

: RECORD-CWORK-VARIABLE ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   name at name STR:LEN ARM
   type name CHECKER-DEFCWORK-VARIABLE
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

\ DYNAMIC-BUFFER publishes THREE words from one line - the accessor,
\ NAME-RESERVE and NAME-RELEASE. Certification never runs the definer, and
\ without this row a later definition in the same source calling one of the
\ three is E-UNDEFINED. No count token: a dynamic buffer's extent is set at run
\ time.
: RECORD-DYNAMIC-BUFFER ( -- )
   SCAN-STORAGE-NAME TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF EXIT THEN
   SCAN-STORAGE-TYPE {: type:str :}
   name s" -RESERVE" at DERIVED-FREE? 0= IF EXIT THEN
   name s" -RELEASE" at DERIVED-FREE? 0= IF EXIT THEN
   name at name STR:LEN ARM
   type name CHECKER-DEFDYNAMIC-BUFFER
   DISARM
   name at DEF-STORAGE DEFINED-MADE ;

\ Whether neither of ARENA's words NAME derives is taken in the checker's
\ current section.
: ARENA-WORDS-FREE? ( str n -- bool )
   {: name:str at:n :}
   name s" -NEW" at DERIVED-FREE? 0= IF false EXIT THEN
   name s" -NEW-DISPOSE" at DERIVED-FREE? ;

\ Whether Q's private section, which the qualified NAME Q:TAIL never writes,
\ holds no TAIL+SFX; NAME is read from byte AT.
: ARENA-PRIVATE-FREE? ( str str n -- bool )
   {: name:str sfx:str at:n :}
   name CHECKER-QUALIFIED? drop
   CHECKER-QPKG$ XREF-PRIVATE-WL {: wid:n :}
   wid 0= IF true EXIT THEN
   CHECKER-QTAIL$ sfx CHECKER-LBUF-SUFFIXED$
   wid CHECKER-RESOLVE:WL-PROBE NULL-PTR = IF true EXIT THEN
   name sfx CHECKER-LBUF-SUFFIXED$
   SOURCE@ at + name STR:LEN >str DUPLICATE-AT!  false ;

\ ARENA's row asks the definer's questions in the order its run asks them and
\ registers both words; without it the rest of the source defers to the run.
: RECORD-ARENA ( -- )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF TOP-CUR-A @ TOP-CUR-U @ >str CHECKER-STORAGE-NAME-REJECT EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   SCAN-STORAGE-TYPE {: type:str :}
   name CHECKER-LBUF-NAME-OK? 0= IF EXIT THEN
   name at name STR:LEN SEALED-NAME
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   name at ARENA-WORDS-FREE? 0= IF EXIT THEN
   name CHECKER-QUALIFIED? IF
      name s" -NEW" at ARENA-PRIVATE-FREE? 0= IF EXIT THEN
      name s" -NEW-DISPOSE" at ARENA-PRIVATE-FREE? 0= IF EXIT THEN
   ELSE
      \ A `global` section's words land in neither section of its package.
      mode CHECKER-PACKAGE-GLOBAL <> IF
         mode CHECKER-PACKAGE-PUBLIC = {: pub:bool :}
         pub IF CHECKER-PRIVATE ELSE CHECKER-PUBLIC THEN
         [: name at ARENA-WORDS-FREE? ;] catch {: rc:n :}
         pub IF CHECKER-PUBLIC ELSE CHECKER-PRIVATE THEN
         rc 0<> IF rc throw THEN
         0= IF EXIT THEN
      THEN
   THEN
   type CHECKER-ARENA-INFO 0= IF
      drop
      name s" -NEW" s" -NEW-DISPOSE" type
      CHECKER-DERIVED-TYPE-REJECT EXIT
   THEN drop
   name at name STR:LEN ARM
   type name CHECKER-DEFARENA
   DISARM ;

\ HEAP's row asks the definer's questions in the order its run asks them and
\ registers both words; without it the rest of the source defers to the run.
: RECORD-HEAP ( -- )
   0 NEWEST-SYM !
   SCAN-LINE-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF TOP-CUR-A @ TOP-CUR-U @ >str CHECKER-STORAGE-NAME-REJECT EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   SCAN-STORAGE-TYPE {: type:str :}
   name CHECKER-LBUF-NAME-OK? 0= IF EXIT THEN
   name at name STR:LEN SEALED-NAME
   name s" -NEW" at DERIVED-FREE? 0= IF EXIT THEN
   name s" -RELEASE" at DERIVED-FREE? 0= IF EXIT THEN
   type CHECKER-DYNAMIC-INFO 0= IF
      drop
      name s" -NEW" s" -RELEASE" type
      CHECKER-DERIVED-TYPE-REJECT EXIT
   THEN drop
   name at name STR:LEN ARM
   type name CHECKER-DEFHEAP
   DISARM ;

\ Registrars resolve the whole type before publishing a word, so an unresolved
\ type marks only the words the definer would make.
: STORAGE-OUTCOME ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   DISARM
   rc E-CHECKER-STORAGE-UNRESOLVED <> IF rc throw THEN
   STG-TYPE-A @ STG-TYPE-U @ >str DW-TYPE DEFER-AT
   QUIET-TYPE-CLEAR ;

\ The field list is the source from its first token f to END-VALUE-RECORD at
\ e, so the field token the checker rejects is placed; a record taken by name
\ or with no field is rejected at its name.
: VALUE-RECORD-ADD ( str ^u8 ^u8 -- )
   {: name:str f:^ e:^ :}
   e f - {: fu:n :}
   name f fu >str CHECKER-TRY-RECORD {: at:n why:n :}
   why 0= IF EXIT THEN
   name  f fu >str at VREC-TOKEN  why CHECKER-RECORD-REJECT
   DECLARATION-END ;

\ A record the input ends inside is rejected at its name, as the loader
\ rejects it.
: RECORD-VALUE-RECORD ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name REJECTION:NOT-A-NAME
   name name DEF-NAME-REJECT? IF DECLARATION-END EXIT THEN
   \ VALUE-RECORD reads its fields with parse-name: a comment opener is a token.
   NEXT-RAW  dup STR:DATA {: f:^ :}
   BEGIN
      dup STR:LEN 0= IF
         name VRDEF-UNCLOSED
         MULTI-ERR? IF drop REJECTION:KEEP EXIT THEN
         EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
      dup VALUE-RECORD-END? IF
         STR:DATA {: e:^ :}
         name f e VALUE-RECORD-ADD
         EXIT
      THEN
      drop NEXT-RAW
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

CAST: TOP-ACTION ( n -- [ str bool -- n ] )
: TOP-VERDICT ( str bool -- n )
   NCOMP-DISPATCH:DECL-VERIFY-TOP-OFF OWNER-XT TOP-ACTION execute ;

\ A replaced or unlocated engine word has no live origin and names no use.
: ENGINE-ORIGIN-USE ( str n n -- )
   {: s:str at:n sym:n :}
   sym 0= IF EXIT THEN
   sym ENGINE-ORIGIN:TARGET
   {: path:str start:n end:n name:str found:bool :}
   found IF at dup s STR:LEN + sym 0 0 0 USE-SEEN THEN ;

\ A definer's own `generates:` row declares the text its calls render: the row
\ this scan read or, for a definer it never read, the engine's. A wrapper has
\ none: it states nothing about the rest of its body.
: DECLARED-RENDER? ( str -- bool )
   {: s:str :}
   s DEFINER-OF dup 0<> IF 1 - DEFINER-GEN @ EXIT THEN drop
   s TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-GENERATES and 0<> ;

\ Rendered text no row declares is source this scan never reads.
: RENDER-GAP ( str -- )
   {: s:str :}
   \ `;` is reserved: it ends a definition its opener began, and its gap is the opener's.
   s s" ;" STR:=CI IF EXIT THEN
   s DECLARED-RENDER? IF EXIT THEN
   s TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-DEFER and 0<>
   IF s DW-DEFER DEFER-AT EXIT THEN
   s DW-RENDERER DEFER-AT ;

\ A call of a `names:` word right after an `s"` literal is a use of the word
\ the literal names, at the bytes between its quotes. A literal written with an
\ escape binds nothing: its slot's bytes are shorter than its text.
: TOP-NAMED ( str -- )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-NAMES and 0= IF EXIT THEN
   STR-LAST-KIND @ LIT-PATH <> IF EXIT THEN
   STR-LAST-END @ STR-LAST-AT @ -  STR-LAST-U @ <> IF EXIT THEN
   STR-LAST-A @ STR-LAST-U @ >str CHECKER-NAMED-USE ;

\ An unmodelled call makes the rest of the source uncertain, declarations the
\ source index has already seen included.
: TOP-OPAQUE ( str n -- )
   {: s:str why:n :}
   true TICK-REMAINDER !
   s why DEFER-AT ;

public

\ Whether a call of the word a u names may move the source root.
: ROOT-MOVE? ( str -- bool )
   TOP-BINDING nip nip CHECKER-OWNER-ABI:BINDING-ROOTS and 0<> ;

private

: BOUND-CALL ( str bool -- )
   {: s:str renders:bool :}
   s ROOT-MOVE? IF s DW-ROOTS TOP-OPAQUE EXIT THEN
   renders IF s RENDER-GAP THEN
   s TOP-NAMED
   \ Literal openers only push the scanned string. Other calls may alter bindings.
   INDEX-ON @ IF s STRING-OPENER? 0= IF s 0 TOP-OPAQUE THEN THEN ;

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
   {: s:str len:n :}
   PRS-TERMS-END len + {: at:n :}
   at s STR:LEN + 1 + PRS-TERMS-RESERVE
   s STR:DATA  at PRS-TERMS  s STR:LEN BYTE-COPY
   32  at s STR:LEN + PRS-TERMS  c!
   len s STR:LEN + 1 + ;

\ The bytes kept and the token's kind: 0 a terminator, 1 the closing `)`, 2 the
\ source's end.
: PRS-LIST-TOKEN ( n -- n n )
   {: len:n :}
   NEXT-RAW {: s:str :}
   s STR:LEN 0= IF len 2 EXIT THEN
   s s" )" STR:= IF len 1 EXIT THEN
   s len PRS-TERM-ADD 0 ;

\ As the engine word reads it: `(`, then tokens through a standalone `)`, at
\ least one before it. The bytes kept, and true when malformed.
: PRS-LIST-READ ( -- n bool )
   NEXT-RAW s" (" STR:= 0= IF 0 0 0= EXIT THEN
   0 BEGIN PRS-LIST-TOKEN dup 0= WHILE drop REPEAT
   {: len:n end:n :}
   len  end 2 =  len 0=  or ;

\ Known by the identity of the word the token selects, never by its spelling.
\ The row is read as the engine word reads it and checked by the engine's own
\ checks. The scan knows every token the declarer reads, so none of it is the
\ run's.
: PARSES-ROW ( str bool -- )
   {: ks:str through:bool :}
   NEXT-RAW {: ts:str :}
   NEXT-RAW {: cs:str :}
   through IF PRS-LIST-READ ELSE 0 0 0= 0= THEN {: len:n bad:bool :}
   ks ts cs bad CHECKER-PARSES-ROW {: sym:n eff1:n count:n :}
   sym 0= IF EXIT THEN
   VERIFY-PARSES-N @ {: row:n :}
   row 1 + PRS-OWNER-RESERVE  row 1 + PRS-SYM-RESERVE  row 1 + PRS-EFF-RESERVE
   row 1 + PRS-COUNT-RESERVE  row 1 + PRS-AT-RESERVE  row 1 + PRS-LEN-RESERVE
   PRS-TERMS-END row PRS-AT !
   PRS-OWNER-BASE row PRS-OWNER !  sym row PRS-SYM !  eff1 row PRS-EFF !
   count row PRS-COUNT !  len row PRS-LEN !
   row 1 + VERIFY-PARSES-N !
   sym count  row PRS-AT @ PRS-TERMS  len >str PROW-ADD-TEXT ;

\ Read and checked as the engine word does.
: NAMES-ROW ( str -- )
   NEXT-RAW CHECKER-NAMES-ROW ;

: PRS-TERM-END ( n str -- n )
   {: t:str :}
   BEGIN dup t STR:LEN < IF dup t STR:DATA + c@ 32 <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT ;

: PRS-TERM-AT? ( n n str ^u8 -- bool )
   {: i:n j:n s:str t:^ :}
   j i - s STR:LEN <> IF 0 0= 0= EXIT THEN
   s  t i +  s STR:LEN >str  STR:= ;

\ A U is one of the terminators T TU holds, each followed by a blank.
: PRS-TERM? ( str str -- bool )
   {: s:str t:str :}
   0
   BEGIN dup t STR:LEN < WHILE
      dup t PRS-TERM-END
      2dup s t STR:DATA PRS-TERM-AT? IF 2drop 0 0= EXIT THEN
      nip 1 +
   REPEAT drop 0 0= 0= ;

\ A row of this scan as a kept row reads: its count and terminators' bytes.
: PRS-ROW ( n -- n str )
   {: row:n :}
   row PRS-COUNT @  row PRS-AT @ PRS-TERMS  row PRS-LEN @ >str ;

\ Consumed as the run reads it. The source's end ends it: what is there is
\ consumed, and the word stays the run's.
: PRS-CONSUME ( n str -- )
   {: cnt:n t:str :}
   cnt 0 ?do NEXT-RAW STR:LEN 0= IF unloop EXIT THEN loop
   t STR:LEN 0= IF EXIT THEN
   BEGIN NEXT-RAW dup STR:LEN 0= IF drop EXIT THEN t PRS-TERM? UNTIL ;

\ Structure bodies take this path too.
: TOP-PARSER ( str -- )
   {: s:str :}
   s TOP-BINDING {: sym:n eff1:n ctl:n :}
   ctl BINDING-ID {: id:n :}
   id CHECKER-OWNER-ABI:BINDING-PARSES-ID = IF s 0 0= 0= PARSES-ROW EXIT THEN
   id CHECKER-OWNER-ABI:BINDING-THROUGH-ID = IF s 0 0= PARSES-ROW EXIT THEN
   id CHECKER-OWNER-ABI:BINDING-NAMES-ID = IF s NAMES-ROW EXIT THEN
   ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0<> IF s DW-DEFER TOP-OPAQUE EXIT THEN
   sym eff1 PRS-FIND {: row:n :}
   row 0 <> IF s DW-PARSES DEFER-AT  row 1 - PRS-ROW PRS-CONSUME EXIT THEN
   sym PROW-FIND {: cnt:n t:str kept:bool :}
   kept 0= IF s DW-PARSER TOP-OPAQUE EXIT THEN
   s DW-PARSES DEFER-AT
   cnt t PRS-CONSUME ;

: TOKEN-TAIL ( str -- str )
   {: s:str :}
   s STR:LEN 1 > IF
      s STR:LEN 1 - 1 ?do
         s STR:DATA i + c@ 58 = IF s STR:DATA i + 1 +  s STR:LEN i - 1 - >str  unloop EXIT THEN
      loop
   THEN
   s ;

\ An export is the same word under another tail, so it takes the row of the
\ word it re-exports, FROM (its row + 1, 0 for none), under the binding the
\ top-level find selects by package and tail. The bare tail is no such
\ selection: in the package it can name the package's own private word. A word
\ that only calls a bounded word takes no row.
: EXPORT-PARSES ( n str -- )
   {: from:n s:str :}
   from 0= IF EXIT THEN
   s TOKEN-TAIL RETAINED-SYM {: rec:n :}
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
   {: name:str at:n :}
   name RECORD-SYM? SYM-IDENTITY drop swap drop RETAINED-SYM
   {: sym:n :}
   sym NEWEST-SYM !
   sym 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str name sym s" " at at name STR:LEN + DEF-EXPORT
   VISIT-CUR @ ON-DEFINITION ;

\ Whether the export's tail, from the u bytes written at byte at, is already
\ defined here; its record stands at the tail.
: EXPORT-DUPLICATE? ( n n -- bool ) {: u:n at:n :}
   SOURCE@ at + {: a:^ :}
   a u >str CHECKER-QUALIFIED? IF CHECKER-QTAIL$ ELSE a u >str THEN {: ts:str :}
   ts CHECKER-CERT-DUP? 0= IF false EXIT THEN
   ts ts DUPLICATE-AT!  true ;

\ Whether the export a u, written from byte at, is recorded: its source
\ resolves before its tail is judged a duplicate, as the load judges them.
: EXPORT-CHECKED? ( str n -- bool ) {: s:str at:n :}
   s CHECKER-EXPORT-SOURCE {: src:n priv:^ :}
   s STR:LEN at EXPORT-DUPLICATE? IF false EXIT THEN
   s src priv RES-FALSE CHECKER-EXPORT-ROW  true ;

\ The catch only disarms before a stop passes on.
: RECORD-EXPORT ( -- )
   0 NEWEST-SYM !
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF EXIT THEN
   name at name STR:LEN SEALED-OPEN
   name TOP-BINDING drop PRS-FIND {: from:n :}
   name at name STR:LEN ARM-UNSEALED   \ the checker spells the record by the tail it keeps
   [: name at EXPORT-CHECKED? ;] catch {: rc:n :}
   DISARM
   rc 0<> IF rc throw THEN
   0= IF EXIT THEN
   name at EXPORT-DEFINED
   from name EXPORT-PARSES ;

\ A require publishes the canonical path before descending, so recursive
\ requires stop where the native loader stops, and a path already held reports
\ held. An include always descends. A file whose bytes were acquired for a load
\ reports read before it is scanned.
: COMPOSE-PENDING ( -- )
   COMPOSE-PEND-A @ COMPOSE-PEND-U @ >str
   COMPOSE-PEND-PATH-A @ COMPOSE-PEND-PATH-U @ >str COMPOSE-FILE ;

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

: LOADED ( str n -- )
   {: path:str outcome:n :}
   LOAD-END @ 0= IF EXIT THEN
   LOAD-AT @ LOAD-END @  0 LOAD-END !
   path outcome ON-LOADER ;

: COMPOSE-LOADED ( str str -- )
   {: s:str path:str :}
   path LOAD-READ LOADED
   s STR:DATA COMPOSE-PEND-A !  s STR:LEN COMPOSE-PEND-U !
   path STR:DATA COMPOSE-PEND-PATH-A !  path STR:LEN COMPOSE-PEND-PATH-U !
   [: COMPOSE-PENDING ;]
   CHECKER-OWNER-ABI:VERIFY-FILE-OFF OWNER-XT FILE-ACTION execute ;

\ A composition rejects a load of a file no root holds as the loader does: its
\ record names the request a u and stands at LOAD-PLACE, and FAULT-TARGET$
\ names the file the read would have opened. No read runs.
: COMPOSE-MISSING ( str str -- )
   {: s:str path:str :}
   path FAULT-TARGET!
   s LOAD-PLACE-A @ LOAD-PLACE-U @ >str false SOURCE-ROOT:MISSING-SOURCE ;

\ Load PATH, the file the request a u resolved to under ROOT, read as file id.
: COMPOSE-OPEN ( str str str n -- )
   {: s:str path:str root:str id:n :}
   path COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ >str STR:= IF
      root path COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @ >str id
      [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-SUPPLIED EXIT
   THEN
   path FS:EXISTS? 0= IF s path COMPOSE-MISSING THEN
   path SUPPLIED {: t:str open:bool :}
   open IF root path t id [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-SUPPLIED EXIT THEN
   root path id [: COMPOSE-LOADED ;] SOURCE-ROOT:WITH-BYTES ;

\ The loads loader word w, the token the source wrote, makes of the path a u:
\ the resolver's answer comes through the loader's, which writes its records.
: COMPOSE-INCLUDED ( str str -- )
   {: w:str s:str :}
   w s [: SOURCE-ROOT:RESOLVE-AT ;] LOADER:RESOLVE drop {: path:str :}
   SOURCE-ROOT:RESOLVED-ROOT$ {: root:str :}
   s path root REQUIRES:CURRENT@ COMPOSE-OPEN ;

\ A require of a u that resolved to path under root: a held path is not
\ loaded; either way the current file requires its file.
: COMPOSE-REQUIRE ( str str bool str -- )
   {: s:str path:str held:bool root:str :}
   held IF s path SOURCE-ROOT:REQUIRE-HELD  path LOAD-HELD LOADED EXIT THEN
   REQUIRE-REG:MARK {: row:n :}
   path REQUIRE-REG:STORE {: rp:str :}
   s row root SOURCE-ROOT:REQUIRE-STORED {: id:n :}
   s rp root id COMPOSE-OPEN ;

: COMPOSE-REQUIRED ( str str -- )
   {: w:str s:str :}
   s  w s [: SOURCE-ROOT:RESOLVE-AT ;] LOADER:RESOLVE
   SOURCE-ROOT:RESOLVED-ROOT$ COMPOSE-REQUIRE ;

: COMPOSE-SCRIPT-REQUIRED ( str -- )
   {: s:str :}
   s  NULL-PTR 0 >str s [: drop SOURCE-ROOT:ENTRY-RESOLVE ;] LOADER:RESOLVE
   SOURCE-ROOT:RESOLVED-ROOT$ COMPOSE-REQUIRE ;

: COMPOSE-PROVIDED ( str str -- )
   [: SOURCE-ROOT:RESOLVE-AT ;] LOADER:RESOLVE IF drop EXIT THEN
   REQUIRE-REG:STORE drop ;

PTR-VARIABLE LOAD-A                           \ the path a top-level load loads
variable LOAD-U

: LOAD$ ( -- str )
   LOAD-A @ LOAD-U @ >str ;

: FILE-LOAD ( bool -- [ str -- str renders parses nominal ] )
   IF [: dup LOAD$ COMPOSE-INCLUDED ;] EXIT THEN
   [: dup LOAD$ COMPOSE-REQUIRED ;] ;

\ Run Q, the load a loader word wu bytes long at byte w makes, given the word;
\ the load's operand span ends with it.
: RUN-LOADER ( [ str -- str ] n n -- )
   {: q w:n wu:n :}
   SOURCE@ w + wu >str q [: 0 LOAD-END ! ;] finally drop ;

\ `include PATH` (inc true) or `require PATH` at top level, its word u bytes
\ long at TOKEN-BYTE; PATH's token is the operand its load reports.
: QUIET-WORD-LOAD ( n bool -- )
   {: u:n inc:bool :}
   TOKEN-BYTE @ {: at:n :}
   u LOADER-OPERAND {: p:str :}
   TOKEN-BYTE @ dup p STR:LEN + ARM-LOAD
   p STR:DATA LOAD-A !  p STR:LEN LOAD-U !
   p LOAD-PLACE!
   inc FILE-LOAD at u RUN-LOADER ;

\ A string loader word at top level, u bytes long at TOKEN-BYTE: rejected by
\ the literal before it as discovery rejects it, else Q loads
\ that literal's path, and the literal is the operand its load reports.
: QUIET-STRING-LOAD ( n [ str -- str ] -- )
   {: u:n q :}
   STR-LAST-KIND @ u LITERAL-LOAD? 0= IF EXIT THEN
   STR-LAST-AT @ STR-LAST-END @ ARM-LOAD
   STR-LAST-A @ LOAD-A !  STR-LAST-U @ LOAD-U !
   SOURCE@ STR-LAST-AT @ +  STR-LAST-END @ STR-LAST-AT @ - >str  LOAD-PLACE!
   q TOKEN-BYTE @ u RUN-LOADER ;

: QUIET-TOP? ( str -- bool ) {: s:str :}
   s s" include" STR:=CI IF s STR:LEN true QUIET-WORD-LOAD true EXIT THEN
   s s" require" STR:=CI IF s STR:LEN false QUIET-WORD-LOAD true EXIT THEN
   s s" included" STR:=CI IF s STR:LEN true FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   s s" required" STR:=CI IF s STR:LEN false FILE-LOAD QUIET-STRING-LOAD true EXIT THEN
   s s" script-required" STR:=CI IF
      s STR:LEN [: LOAD$ COMPOSE-SCRIPT-REQUIRED ;] QUIET-STRING-LOAD true EXIT
   THEN
   s s" provided" STR:=CI IF
      s STR:LEN [: dup LOAD$ COMPOSE-PROVIDED ;] QUIET-STRING-LOAD true EXIT
   THEN
   false ;

\ Runs the loader call with its loader word a u as the token read last, as on
\ the native path, until the call returns or throws.
: LOADER-AT ( R [ R -- S ] str -- S )
   {: s:str :}
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   s STR:DATA s STR:LEN SRCLOC:TOKEN!
   [: oa ou SRCLOC:TOKEN! ;] finally ;

: LOADER-WORD? ( str -- bool ) {: s:str :}
   s s" include" STR:=CI  s s" require" STR:=CI or
   s s" included" STR:=CI or  s s" required" STR:=CI or
   s s" script-required" STR:=CI or  s s" provided" STR:=CI or ;

: CB-OPEN? ( -- bool )
   CB-GROUP @ MATCH callback-group
      closed OF false ENDOF
      open OF true ENDOF
      skipped OF true ENDOF
      left OF false ENDOF
   ;MATCH ;

\ Whether the token t tu lies in the source now scanned.
: SCANNED? ( str -- bool ) {: t:str :}
   t STR:LEN 0= IF false EXIT THEN
   t STR:DATA SOURCE@ - {: at:n :}
   at 0 < 0=  at SOURCE-U @ > 0= and ;

\ The opener and closer of the FUNCTION: or CALLBACK: group this source opened,
\ else empty: nested files share FFI's open group, as the load's declaration.
: OPEN-GROUP ( -- str str )
   OPENER$ SCANNED? IF OPENER$ s" ;FUNCTION" EXIT THEN
   CB-OPEN?  CB-OPENER-A @ CB-OPENER-U @ >str SCANNED? and IF
      CB-OPENER-A @ CB-OPENER-U @ >str s" ;CALLBACK" EXIT
   THEN
   NULL-PTR 0 >str NULL-PTR 0 >str ;

\ The load words; `provided` loads nothing.
: LOADS? ( str -- bool ) {: s:str :}
   s LOADER-WORD?  s s" provided" STR:=CI 0= and ;

\ The load word w wu is called while the declaration t tu opened, which c cu
\ closes, is open: the load's record.
: UNCLOSED-LOAD-STOP ( str str str -- )
   SOURCE-ROOT:UNCLOSED-LOAD  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The load word w wu is called while a group this source opened is open.
: GROUP-LOAD-STOP ( str -- ) {: w:str :}
   OPEN-GROUP {: t:str c:str :}
   t STR:LEN 0= IF EXIT THEN
   t c w UNCLOSED-LOAD-STOP ;

: COMPOSE-TOP? ( str -- bool ) {: s:str :}
   COMPOSE-ON @ 0= IF 0 0= 0= EXIT THEN
   s LOADER-WORD? 0= IF 0 0= 0= EXIT THEN
   TICK-CONTEXT-UNKNOWN
   s QUIET-USE
   s TOKEN-BYTE @ s TOP-BINDING drop drop ENGINE-ORIGIN-USE
   s LOADS? IF s GROUP-LOAD-STOP THEN
   s [: QUIET-TOP? ;] s LOADER-AT ;

public
\ Writes, open after its help, the record of the row opened at line l, column c
\ that the token t tu breaks or, with tu 0, that the source ends inside.
: ROW-REJECT ( str n n -- ) {: t:str l:n c:n :}
   s" E-MALFORMED-REGISTRY-ROW" REJECTION:OPEN
   t STR:LEN 0= IF s" found the end of the source" REJECTION:EMIT-STR
   ELSE s" found " REJECTION:EMIT-STR  t TICKED THEN
   s"  in the primitive-axiom row opened at " REJECTION:EMIT-STR
   l REJECTION:EMIT-U  s" :" REJECTION:EMIT-STR  c REJECTION:EMIT-U
   t REJECTION:AT
   t STR:LEN 0= IF s" the source ends before the row closes"
   ELSE s" breaks the row" THEN REJECTION:EMIT-STR
   REJECTION:HELP
   s" a row's name and package are neither an opener nor a closer, "
   REJECTION:EMIT-STR
   s" its body holds only `PE-` or `;PE-` atoms, `REF` clauses "
   REJECTION:EMIT-STR
   s" and the flags `ETRUSTED-ONLY`, `EIMMEDIATE` and `EFIXED-ABI`, "
   REJECTION:EMIT-STR
   s" and it ends at `EPRIM;` or `ECLOSE-PRIVATE`: " REJECTION:EMIT-STR
   t STR:LEN 0= IF s" close the row"
   ELSE s" close the row before this token or replace it" THEN REJECTION:EMIT-STR ;
private

\ The token t tu breaks the row the top-level token opened.
: ROW-MALFORMED ( str -- ) {: t:str :}
   TOP-CUR-A @ REJECTION:LOCATE drop drop {: l:n c:n :}
   t l c ROW-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: ROW-ENDED ( -- )
   SOURCE@ SOURCE-U @ + 0 >str ROW-MALFORMED ;

\ The row's next raw token; none left makes the row malformed at the
\ source's end.
: ROW-TOKEN! ( -- )
   RAW!
   TOKEN-U @ 0= IF ROW-ENDED THEN ;

: ROW-TOKEN-SEEN ( -- )
   TOKEN-A @ TOKEN-U @ >str BODY-TOKEN-SEEN ;

\ A header token names the row's primitive or package. None, an opener or a
\ closer is rejected as a missing header, though the opener takes either as a name.
: ROW-HEADER ( -- )
   NEXT-RAW {: s:str :}
   s STR:LEN 0= IF ROW-ENDED THEN
   s SOURCE-SYNTAX:ROW-OPENER? IF s ROW-MALFORMED THEN
   s SOURCE-SYNTAX:ROW-CLOSER? IF s ROW-MALFORMED THEN ;

\ A row starts clear of the last body's literal, as every body reader does.
: RECORD-PRIM-ROW ( -- )
   BODY-PREV-CLEAR
   ROW-HEADER
   BEGIN
      ROW-TOKEN!
      TOKEN-A @ TOKEN-U @ >str SOURCE-SYNTAX:ROW-CLOSER? IF EXIT THEN
      TOKEN-A @ TOKEN-U @ >str SOURCE-SYNTAX:ROW-REF? IF
         ROW-TOKEN-SEEN  ROW-TOKEN!
      ELSE
         TOKEN-A @ TOKEN-U @ >str SOURCE-SYNTAX:ROW-ATOM? 0= IF
            TOKEN-A @ TOKEN-U @ >str ROW-MALFORMED
         THEN
      THEN
      ROW-TOKEN-SEEN
   AGAIN ;

: RECORD-PPRIM ( -- )
   ROW-HEADER
   RECORD-PRIM-ROW ;

: ENGINE-FILE? ( -- bool )
   COMPOSE-CUR-PATH-U @ 0= IF false EXIT THEN
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ >str REQUIRE-REG:ENGINE-PROVIDES? ;

: STRUCTURE-END? ( str -- bool )
   s" END-STRUCTURE" STR:=CI ;

: TRUST-STRUCTURE-FIELD ( str str -- bool )
   DECL-SIGNATURE CHECKER-ROW:KEPT = ;

: RECORD-STRUCTURE-FIELD ( str str -- )
   {: kind:str sig:str :}
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   NAME-DECLARED
   name at REJECT-DUPLICATE IF EXIT THEN
   name at name STR:LEN ARM
   name sig TRUST-STRUCTURE-FIELD
   DISARM
   IF kind name sig at at name STR:LEN + DEF-WORD DEFINED THEN ;

: STRUCTURE-SIZE ( str n -- ) {: name:str at:n :}
   name at REJECT-DUPLICATE IF EXIT THEN
   name at name STR:LEN ARM
   name s" -- n" DECL-SIGNATURE CHECKER-ROW:KEPT =
   DISARM
   IF name s" -- n" at DEF-CONSTANT DEFINED-HERE THEN ;

\ The structure scanner takes these words' operands itself, though their
\ run-time call may read source.
: STRUCTURE-SYNTAX? ( str -- bool )
   dup STRUCTURE-END? IF drop true EXIT THEN
   dup STRUCTURE-PTR-FIELD? IF drop true EXIT THEN
   dup STRUCTURE-CFIELD? IF drop true EXIT THEN
   STRUCTURE-CELL-FIELD? ;

\ The body executes its bound words though this scanner reads their tokens.
\ `char` takes data and `'` names a word; both consume their next raw token. A
\ word that renders source is deferred at its token, as at top level.
: STRUCTURE-USE ( str -- )
   dup TOP-PARSER? IF
      dup CHAR-KEYWORD? IF drop OPERAND drop EXIT THEN
      drop OPERAND
      dup TOP-BINDING drop drop 0= IF drop EXIT THEN
      false TOP-VERDICT drop EXIT
   THEN
   dup TOP-BINDING drop drop 0= IF drop EXIT THEN
   dup CTL-RENDERS RENDERS-MARK? IF dup RENDER-GAP THEN
   dup dup STRUCTURE-SYNTAX? 0= TOP-VERDICT 2 =
   IF TOP-PARSER ELSE drop THEN ;

\ The group open in this source is rejected when the token o ou opens another
\ declaration or, with ou 0, when the source ends.
: FFI-UNCLOSED ( str -- ) {: o:str :}
   OPEN-GROUP {: t:str c:str :}
   t STR:LEN 0= IF EXIT THEN
   t c o UNCLOSED-DECL-STOP ;

\ A declaration opening, or a package scope opening or closing, inside the
\ structure rejects the structure.
: STRUCTURE-OPENED ( str -- ) {: o:str :}
   o s" BEGIN-STRUCTURE" STR:=CI  o s" FUNCTION:" STR:=CI or
   o s" CALLBACK:" STR:=CI or
   o s" package" STR:=CI or  o s" ;package" STR:=CI or
   o s" seal-package" STR:=CI or 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str s" END-STRUCTURE" o UNCLOSED-DECL-STOP ;

\ A load word inside the structure rejects the structure.
: STRUCTURE-LOADS ( str -- ) {: w:str :}
   w LOADS? 0= IF EXIT THEN
   TOP-CUR-A @ TOP-CUR-U @ >str s" END-STRUCTURE" w UNCLOSED-LOAD-STOP ;

\ A section word inside the structure switches the section, as at top level.
: STRUCTURE-SECTION ( str -- ) {: w:str :}
   w s" public" STR:=CI IF RECORD-PUBLIC EXIT THEN
   w s" private" STR:=CI IF RECORD-PRIVATE EXIT THEN
   w s" global" STR:=CI IF RECORD-GLOBAL THEN ;

: RECORD-STRUCTURE ( -- )
   TOP-CUR-A @ TOP-CUR-U @ >str FFI-UNCLOSED
   NAME-TOKEN TOKEN-BYTE @ {: name:str at:n :}
   name STR:LEN 0= IF NAME-STOP THEN
   NAME-DECLARED
   name at STRUCTURE-SIZE
   BEGIN
      NEXT-SCAN
      dup STR:LEN 0= IF TOP-CUR-A @ TOP-CUR-U @ >str s" END-STRUCTURE" NULL-PTR 0 >str UNCLOSED-DECL-STOP THEN
      dup STRUCTURE-OPENED
      dup STRUCTURE-LOADS
      dup STRUCTURE-SECTION
      dup STRUCTURE-USE
      TICK-REMAINDER @ IF drop EXIT THEN
      dup STRUCTURE-END? IF
         drop
         EXIT
      THEN
      \ A pointer field's pointee is independent of the record's element type:
      \ a cell record may hold a byte pointer.
      dup STRUCTURE-PTR-FIELD? IF s" ^a -- ^^b" RECORD-STRUCTURE-FIELD ELSE
      dup STRUCTURE-CFIELD? IF s" ^a -- ^u8" RECORD-STRUCTURE-FIELD ELSE
      dup STRUCTURE-CELL-FIELD? IF s" ^a -- ^a" RECORD-STRUCTURE-FIELD ELSE
      drop
      THEN THEN THEN
   AGAIN ;

\ `generates: D ( effect )`, read as the engine word reads it: the name by
\ parse-name, the effect as a definition head's signature. The registrar
\ resolves D and raises the rejections FIND-SYM defers; a row this scan's table
\ already holds for D, its clause or an earlier row, counts as D already stating
\ what it makes.
: RECORD-GENERATES ( -- )
   NAME-TOKEN
   {: name:str :}
   name STR:LEN 0= IF NAME-STOP THEN
   name SIGNATURE-AT
   {: sig:str :}
   name FIND-SYM
   {: sym:n :}
   name sig sym DEFINER-FIND 0<> CHECKER-GENERATES nip 0= IF EXIT THEN
   sig true sym DEFINER-ADD ;

\ A FUNCTION: declaration's effect or a callback body's type, from FFI's texts:
\ BODY-BUF holds only runs of the source.
DYNAMIC-BUFFER FFI-SIG u8
variable FFI-SIG-U
\ The kept declaration's name, empty while none is kept: a copy lies in no
\ source, so the record is placed at the `;FUNCTION` token, as the load's is.
DYNAMIC-BUFFER FFI-NAME u8
variable FFI-NAME-U
variable FFI-LIT-N                          \ literals read last, at most two:
variable FFI-LIT-A                          \ the older
variable FFI-LIT-B                          \ and the newer

: FFI-APPEND ( str -- )
   {: s:str :}
   FFI-SIG-U @
   {: at:n :}
   at s STR:LEN + 1 + FFI-SIG-RESERVE
   s STR:DATA  at FFI-SIG  s STR:LEN BYTE-COPY
   32  at s STR:LEN + FFI-SIG  c!
   at s STR:LEN + 1 + FFI-SIG-U ! ;

\ A call into FFI's declaration machine placed at the scan's token A U, which
\ stands in as the token read last until it returns or throws.
: FFI-AT ( R [ R -- S ] str -- S )
   {: s:str :}
   SRCLOC:TOKEN$ {: oa:^ ou:n :}
   s STR:DATA s STR:LEN SRCLOC:TOKEN!
   s STR:DATA SOURCE@ - TOKEN-BYTE !
   [: oa ou SRCLOC:TOKEN! ;] finally ;

\ `FUNCTION: … ;FUNCTION`, read by FFI's declaration machine from the raw
\ tokens fed to it.
: RECORD-FFI-FUNCTION ( str -- )
   {: s:str :}
   0 FFI-NAME-U !
   s FFI-UNCLOSED
   [: [: NEXT-RAW dup STR:LEN 0<> IF dup dup STR:DATA swap STR:LEN SRCLOC:TOKEN! THEN ;]
      [: drop ;] READ-FUNCTION ;] s FFI-AT
   {: name:str :}
   name STR:DATA SOURCE@ - {: at:n :}
   at TOKEN-BYTE !
   NAME-DECLARED
   name at REJECT-DUPLICATE IF SKIP-FUNCTION EXIT THEN
   0 FFI-SIG-U !
   [: FFI-APPEND ;] DECLARED-TEXTS
   0 FFI-SIG FFI-SIG-U @ >str {: sig:str :}
   name at name STR:LEN ARM
   name sig DECL-SIGNATURE CHECKER-ROW:KEPT =
   DISARM
   IF name sig at DEF-WORD DEFINED-HERE
      name STR:LEN FFI-NAME-RESERVE  name STR:DATA 0 FFI-NAME name STR:LEN BYTE-COPY  name STR:LEN FFI-NAME-U !
   ELSE SKIP-FUNCTION THEN ;

\ Whether `;CALLBACK` defines the group's words.
: CB-DEFINES? ( -- bool )
   CB-GROUP @ MATCH callback-group
      closed OF false ENDOF
      open OF true ENDOF
      skipped OF false ENDOF
      left OF true ENDOF
   ;MATCH ;

\ A word w of the open callback group: a taken one is the duplicate, rejected
\ at the source token t, and the group is skipped.
: CALLBACK-TAKEN ( str str -- ) {: w:str t:str :}
   CB-DEFINES? 0= IF EXIT THEN
   w CHECKER-CERT-DUP? IF
      w t DUPLICATE-AT!
      construct callback-group skipped CB-GROUP !
   THEN ;

\ A word the open callback group defines, as FFI-CB:PRODUCTS gives it, claimed
\ at the group's name, as the run claims it.
: CALLBACK-CLAIM ( str str bool -- )
   drop drop SOURCE@ CB-AT @ + CB-NAME-U @ >str CALLBACK-TAKEN ;

\ The same word asked again at `;CALLBACK`, where the run defines it: a
\ definition inside the group may have taken it since.
: CALLBACK-RECLAIM ( str str bool -- )
   drop drop TOKEN-BYTE @ TOKEN-FROM CALLBACK-TAKEN ;

\ `CALLBACK: NAME ( in -- out )`, read by FFI-CB's own reader. Its words are
\ claimed there and defined at `;CALLBACK`, as the run claims and defines them.
: RECORD-FFI-CALLBACK ( str -- )
   {: s:str :}
   s FFI-UNCLOSED
   [: [: NEXT-RAW dup STR:LEN 0<> IF dup dup STR:DATA swap STR:LEN SRCLOC:TOKEN! THEN ;]
      FFI-CB:READ-CALLBACK ;] s FFI-AT
   {: name:str :}
   name STR:DATA SOURCE@ - {: at:n :}
   at TOKEN-BYTE !
   NAME-DECLARED
   s STR:DATA CB-OPENER-A !  s STR:LEN CB-OPENER-U !  at CB-AT !  name STR:LEN CB-NAME-U !
   construct callback-group open CB-GROUP !
   name at REJECT-DUPLICATE IF
      construct callback-group skipped CB-GROUP !
   THEN
   [: CALLBACK-CLAIM ;] FFI-CB:PRODUCTS ;

\ One word of the open callback group, as FFI-CB:PRODUCTS gives it, placed at
\ the group's name.
: CALLBACK-DEF ( str str bool -- )
   {: name:str row:str var:bool :}
   NAME-DECLARED
   CB-AT @ CB-NAME-U @ {: at:n u:n :}
   CB-OPENER-A @ CB-OPENER-U @ >str {: t:str :}
   name at u ARM
   var IF
      0 FFI-SIG-U !  s" [" FFI-APPEND  row FFI-APPEND  s" ]" FFI-APPEND
      0 FFI-SIG FFI-SIG-U @ 1 - >str name CHECKER-DEFTYPED-VARIABLE
      DISARM
      t name name MADE-EFFECT at at u + DEF-STORAGE DEFINED
      EXIT
   THEN
   name row DECL-SIGNATURE CHECKER-ROW:KEPT =
   DISARM
   IF t name row at at u + DEF-WORD DEFINED THEN ;

\ A qualified type in the row names the package it spells, even where the
\ open package has a part of that name.
: CALLBACK-WORD ( str str bool -- )
   -1 SIG-EXACT-MODE !
   [: CALLBACK-DEF ;] [: 0 SIG-EXACT-MODE ! ;] finally ;

\ `;CALLBACK` claims and defines the open group's words; one with none open
\ is left to the run, which rejects it.
: CLOSE-FFI-CALLBACK ( -- )
   CB-DEFINES? IF [: CALLBACK-RECLAIM ;] FFI-CB:PRODUCTS THEN
   CB-DEFINES? IF [: CALLBACK-WORD ;] FFI-CB:PRODUCTS THEN
   CB-CLOSED! ;

\ The load judges the word `;FUNCTION` builds as it publishes it, after the
\ closer's own rejections.
: FFI-CLOSE ( -- )
   PLAN-FUNCTION
   FFI-NAME-U @ 0= IF EXIT THEN
   0 FFI-NAME FFI-NAME-U @ >str {: name:str :}
   0 FFI-NAME-U !
   name 0 FFI-SIG FFI-SIG-U @ >str DECL-SHADOW ;

: FFI-CLOSER? ( str -- bool )
   {: s:str :}
   s s" ;FUNCTION" STR:=CI ;

: FFI-LITERAL ( str -- )
   {: s:str :}
   s num-parse {: v:n flt:bool num:bool :}
   num flt 0= and 0= IF 0 FFI-LIT-N ! EXIT THEN
   FFI-LIT-B @ FFI-LIT-A !  v FFI-LIT-B !
   FFI-LIT-N @ 1+ 2 min FFI-LIT-N ! ;

: FFI-CLAUSE ( str -- )
   {: s:str :}
   s TOKEN-TAIL {: t:str :}
   FFI-LIT-N @ {: lits:n :}
   t s" STRUCT-ARG" STR:=CI IF
      lits 2 < IF SKIP-FUNCTION EXIT THEN
      FFI-LIT-A @ FFI-LIT-B @ [: FFI:SET-STRUCT-ARG ;] s FFI-AT EXIT
   THEN
   t s" STRUCT-RESULT" STR:=CI IF
      lits 1 < IF SKIP-FUNCTION EXIT THEN
      FFI-LIT-B @ [: FFI:SET-STRUCT-RESULT ;] s FFI-AT EXIT
   THEN
   t s" VARIADIC" STR:=CI 0= IF EXIT THEN
   lits 1 < IF SKIP-FUNCTION EXIT THEN
   FFI-LIT-B @ [: FFI:SET-VARIADIC ;] s FFI-AT ;

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
   {: s:str cell:^ :}
   s XREF-FIND {: rec:^ :}
   rec XREF-FOUND? 0= IF
      2 s" verify-source: library word not loaded: " write drop  s EXIT-STATUS:INTERNAL die
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
variable USERS-XT s" TASK:+USERS" USERS-XT LIBRARY-XT!
variable USER-XT s" TASK:+USER" USER-XT LIBRARY-XT!

\ A re-export of the library word is that word: the load binds its alias to the
\ library word's code. An export keeps its source's tail, so only a token whose
\ tail is the word's name, in any case, can bind it. A live export's record
\ runs that code; one this scan's replay made has none, so its symbol is
\ followed to the word its package and tail name, the library word only when
\ that word's newest record still runs its code.
\ Known gap: an alias exported before its source is undefined, redefined or
\ not, is followed to the current global word, so the check can certify a use
\ the load rejects.
: LIBRARY-DEFINER? ( str n str -- bool )
   {: s:str code:n name:str :}
   s TOKEN-TAIL name STR:=CI 0= IF false EXIT THEN
   s scope-find drop drop drop {: rec:^ :}
   rec XREF-FOUND? 0= IF false EXIT THEN
   rec XREF-START code = IF true EXIT THEN
   s FIND-SYM {: sym:n :}
   sym 0= IF false EXIT THEN
   sym SYM-SOURCE {: src:n :}
   src sym = IF false EXIT THEN
   src SYM-IDENTITY drop {: pkg:str tail:str :}
   tail STR:LEN 0= IF false EXIT THEN
   pkg STR:LEN 0<> IF pkg tail QUAL-SPELLING
   ELSE tail THEN
   XREF-FIND {: word:^ :}
   word XREF-FOUND? 0= IF false EXIT THEN
   word XREF-START code = ;

\ A token is a clause only where the load's lookup binds it to the declarer's
\ word (LIBRARY-DEFINER?); operands are literals read right before it.
variable STRUCT-ARG-XT s" STRUCT-ARG" STRUCT-ARG-XT LIBRARY-XT!
variable STRUCT-RESULT-XT s" STRUCT-RESULT" STRUCT-RESULT-XT LIBRARY-XT!
variable VARIADIC-XT s" VARIADIC" VARIADIC-XT LIBRARY-XT!

: FFI-CLAUSE? ( str -- bool )
   {: s:str :}
   s STRUCT-ARG-XT @ s" STRUCT-ARG" LIBRARY-DEFINER? IF true EXIT THEN
   s STRUCT-RESULT-XT @ s" STRUCT-RESULT" LIBRARY-DEFINER? IF true EXIT THEN
   s VARIADIC-XT @ s" VARIADIC" LIBRARY-DEFINER? ;

\ The arm that reads a top-level token; no arm reads an `other` one.
ENUM definer-arm DERIVE eq
   other
   package-open public-section private-section global-section target-section
   package-close package-seal using-open using-close split-mark
   type-decl linear-type value-record raw-structure structure-decl mutual-decl
   newtype-decl sumtype-decl enum-decl
   layout-buffer defer-layout-buffer typed-buffer typed-variable
   byte-buffer user-slot dynamic-buffer arena-def heap-def
   cwork-span cwork-variable
   constant-def address-def defer-def prim-row pprim-row trusted-def
   cast-decl linear-decl undefine-name generates-row immediate-mark export-def
   function-open callback-open function-clause function-close callback-close
   unread learned created
;ENUM

: KEYWORD-ARM ( str -- definer-arm )
   {: s:str :}
   s s" package" STR:=CI IF construct definer-arm package-open EXIT THEN
   s s" public" STR:=CI IF construct definer-arm public-section EXIT THEN
   s s" private" STR:=CI IF construct definer-arm private-section EXIT THEN
   s s" global" STR:=CI IF construct definer-arm global-section EXIT THEN
   s s" host" STR:=CI IF construct definer-arm target-section EXIT THEN
   s s" target" STR:=CI IF construct definer-arm target-section EXIT THEN
   s s" ;package" STR:=CI IF construct definer-arm package-close EXIT THEN
   s s" seal-package" STR:=CI IF construct definer-arm package-seal EXIT THEN
   s s" using" STR:=CI IF construct definer-arm using-open EXIT THEN
   s s" ;using" STR:=CI IF construct definer-arm using-close EXIT THEN
   s s" split-str" STR:=CI IF construct definer-arm split-mark EXIT THEN
   construct definer-arm other ;

\ Read only where the token binds the engine's word: a program's own word of
\ the spelling is a call.
: DECLARATION-ARM ( str -- definer-arm )
   {: s:str :}
   s s" deftype" STR:=CI IF construct definer-arm type-decl EXIT THEN
   s s" deflinear" STR:=CI IF construct definer-arm linear-type EXIT THEN
   s s" value-record" STR:=CI IF construct definer-arm value-record EXIT THEN
   s s" structure" STR:=CI IF construct definer-arm structure-decl EXIT THEN
   s s" mutual" STR:=CI IF construct definer-arm mutual-decl EXIT THEN
   s s" newtype" STR:=CI IF construct definer-arm newtype-decl EXIT THEN
   s s" sumtype" STR:=CI IF construct definer-arm sumtype-decl EXIT THEN
   s s" enum" STR:=CI IF construct definer-arm enum-decl EXIT THEN
   s s" generates:" STR:=CI IF construct definer-arm generates-row EXIT THEN
   s s" FUNCTION:" STR:=CI IF construct definer-arm function-open EXIT THEN
   s s" CALLBACK:" STR:=CI IF construct definer-arm callback-open EXIT THEN
   \ `;FUNCTION` defines only the word the `FUNCTION:` arm recorded, so it marks
   \ nothing.
   s FFI-CLOSER? IF construct definer-arm function-close EXIT THEN
   s s" ;CALLBACK" STR:=CI IF construct definer-arm callback-close EXIT THEN
   construct definer-arm other ;

: STORAGE-ARM ( str -- definer-arm )
   {: s:str :}
   s LAYOUT-BUFFER-XT @ s" LAYOUT-BUFFER" LIBRARY-DEFINER?
   IF construct definer-arm layout-buffer EXIT THEN
   s DEFER-LAYOUT-BUFFER-XT @ s" DEFER-LAYOUT-BUFFER" LIBRARY-DEFINER?
   IF construct definer-arm defer-layout-buffer EXIT THEN
   s TYPED-BUFFER-XT @ s" TYPED-BUFFER" LIBRARY-DEFINER?
   IF construct definer-arm typed-buffer EXIT THEN
   s TYPED-VARIABLE-XT @ s" TYPED-VARIABLE" LIBRARY-DEFINER?
   IF construct definer-arm typed-variable EXIT THEN
   s USERS-XT @ s" +USERS" LIBRARY-DEFINER?
   IF construct definer-arm byte-buffer EXIT THEN
   s USER-XT @ s" +USER" LIBRARY-DEFINER?
   IF construct definer-arm user-slot EXIT THEN
   s DYNAMIC-BUFFER-XT @ s" DYNAMIC-BUFFER" LIBRARY-DEFINER?
   IF construct definer-arm dynamic-buffer EXIT THEN
   s ARENA-XT @ s" ARENA" LIBRARY-DEFINER?
   IF construct definer-arm arena-def EXIT THEN
   s HEAP-XT @ s" HEAP" LIBRARY-DEFINER?
   IF construct definer-arm heap-def EXIT THEN
   s CWORK-BUFFER-XT @ s" CWORK-BUFFER" LIBRARY-DEFINER?
   IF construct definer-arm byte-buffer EXIT THEN
   s CWORK-SPAN-XT @ s" CWORK-SPAN" LIBRARY-DEFINER?
   IF construct definer-arm cwork-span EXIT THEN
   s CWORK-VARIABLE-XT @ s" CWORK-VARIABLE" LIBRARY-DEFINER?
   IF construct definer-arm cwork-variable EXIT THEN
   construct definer-arm other ;

: WORD-ARM ( str -- definer-arm )
   {: s:str :}
   s s" constant" STR:=CI IF construct definer-arm constant-def EXIT THEN
   s s" create" STR:=CI IF construct definer-arm address-def EXIT THEN
   s s" variable" STR:=CI IF construct definer-arm address-def EXIT THEN
   s s" defer" STR:=CI IF construct definer-arm defer-def EXIT THEN
   s s" begin-structure" STR:=CI
   IF construct definer-arm raw-structure EXIT THEN
   s s" EPRIM:" STR:=CI
   IF ENGINE-FILE? IF construct definer-arm prim-row EXIT THEN THEN
   s s" EPPRIM:" STR:=CI
   IF ENGINE-FILE? IF construct definer-arm pprim-row EXIT THEN THEN
   s s" trusted:" STR:=CI IF construct definer-arm trusted-def EXIT THEN
   s s" cast:" STR:=CI IF construct definer-arm cast-decl EXIT THEN
   s s" linear:" STR:=CI IF construct definer-arm linear-decl EXIT THEN
   s s" undefine" STR:=CI IF construct definer-arm undefine-name EXIT THEN
   s s" immediate" STR:=CI IF construct definer-arm immediate-mark EXIT THEN
   s s" export" STR:=CI IF construct definer-arm export-def EXIT THEN
   construct definer-arm other ;

: CALL-RENDERS? ( str -- bool ) CTL-RENDERS RENDERS-MARK? ;

\ A declaration no other arm reads and that renders no source (`evaluate`
\ renders) leaves the rest of the source to the run.
: CALL-ARM ( str bool -- definer-arm )
   {: s:str decl:bool :}
   s NOMINAL-MARK
   s CALL-RENDERS? {: renders:bool :}
   decl renders 0= and IF construct definer-arm unread EXIT THEN
   s DEFINER-EFFECT STR:LEN 0<> IF construct definer-arm learned EXIT THEN
   s FIND-SYM CREATES-SYM? 0<> IF construct definer-arm created EXIT THEN
   construct definer-arm other ;

: DEFINER? ( definer-arm -- bool )
   construct definer-arm other DEFINER-ARM-EQ 0= ;

: ELSE-ARM ( definer-arm [ -- definer-arm ] -- definer-arm )
   {: arm:definer-arm next :}
   arm DEFINER? IF arm EXIT THEN
   next execute ;

: FFI-CLAUSE-ARM ( str -- definer-arm )
   FFI-CLAUSE? IF construct definer-arm function-clause EXIT THEN
   construct definer-arm other ;

: SELECT-ARM ( str -- definer-arm )
   {: s:str :}
   s TOP-BINDING nip nip BINDING-ID {: id:n :}
   id CHECKER-OWNER-ABI:BINDING-DECLARATION-ID =
   id CHECKER-OWNER-ABI:BINDING-TOP-ONLY-ID = or {: decl:bool :}
   s KEYWORD-ARM
   [: decl IF s DECLARATION-ARM ELSE construct definer-arm other THEN ;]
   ELSE-ARM
   [: s STORAGE-ARM ;] ELSE-ARM
   [: s WORD-ARM ;] ELSE-ARM
   [: s FFI-CLAUSE-ARM ;] ELSE-ARM
   [: s decl CALL-ARM ;] ELSE-ARM ;

\ The load's require check of a definer's token: the load reads a top-level
\ keyword before it looks a word up.
: DEFINER-USE ( str -- )
   {: s:str :}
   s TOP-KEYWORD? IF EXIT THEN
   s CHECKER-VERIFY-REQUIRED ;

: SPLIT-MARK ( str -- )
   CHECKER-SPLIT-STR @ 0<> IF drop EXIT THEN
   INTERPRETER:SPLIT-STR-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The effect is the `does>` clause's or the `generates:` row's.
: RUN-LEARNED ( str -- )
   {: s:str :}
   s  s CALL-RENDERS?  BOUND-CALL
   TICK-CONTEXT-UNKNOWN
   s DEFINER-EFFECT DEF-STORAGE RAW-TRUST-NEXT ;

: RUN-CREATED ( str -- )
   {: s:str :}
   s FIND-SYM {: dsym:n :}
   s  s CALL-RENDERS?  BOUND-CALL
   dsym CREATED-TRUST-NEXT  TICK-CONTEXT-UNKNOWN ;

: RUN-ARM ( str definer-arm -- )
   {: s:str arm:definer-arm :}
   arm MATCH definer-arm
      other OF ENDOF
      package-open OF RECORD-PACKAGE ENDOF
      public-section OF RECORD-PUBLIC ENDOF
      private-section OF RECORD-PRIVATE ENDOF
      global-section OF RECORD-GLOBAL ENDOF
      target-section OF ENDOF
      package-close OF RECORD-END-PACKAGE ENDOF
      package-seal OF SEAL-ROW ENDOF
      using-open OF RECORD-USING ENDOF
      using-close OF s RECORD-END-USING ENDOF
      split-mark OF s SPLIT-MARK ENDOF
      type-decl OF RECORD-DEFTYPE ENDOF
      linear-type OF RECORD-DEFLINEAR ENDOF
      value-record OF [: RECORD-VALUE-RECORD ;] VALUE-RECORD-OUTCOME ENDOF
      raw-structure OF RECORD-STRUCTURE ENDOF
      structure-decl OF RECORD-STRUCTURE-DECL ENDOF
      mutual-decl OF RECORD-MUTUAL ENDOF
      newtype-decl OF RECORD-NEWTYPE ENDOF
      sumtype-decl OF [: RECORD-SUMTYPE ;] LEGACY-TYPE-OUTCOME ENDOF
      enum-decl OF RECORD-ENUM ENDOF
      layout-buffer OF [: RECORD-LAYOUT-BUFFER ;] STORAGE-OUTCOME ENDOF
      defer-layout-buffer OF
         [: RECORD-DEFER-LAYOUT-BUFFER ;] STORAGE-OUTCOME
      ENDOF
      typed-buffer OF [: RECORD-TYPED-BUFFER ;] STORAGE-OUTCOME ENDOF
      typed-variable OF [: RECORD-TYPED-VARIABLE ;] STORAGE-OUTCOME ENDOF
      byte-buffer OF [: RECORD-BYTE-BUFFER ;] STORAGE-OUTCOME ENDOF
      user-slot OF [: RECORD-SLOT ;] STORAGE-OUTCOME ENDOF
      dynamic-buffer OF [: RECORD-DYNAMIC-BUFFER ;] STORAGE-OUTCOME ENDOF
      arena-def OF [: RECORD-ARENA ;] STORAGE-OUTCOME ENDOF
      heap-def OF [: RECORD-HEAP ;] STORAGE-OUTCOME ENDOF
      cwork-span OF [: RECORD-CWORK-SPAN ;] STORAGE-OUTCOME ENDOF
      cwork-variable OF [: RECORD-CWORK-VARIABLE ;] STORAGE-OUTCOME ENDOF
      constant-def OF RECORD-CONSTANT ENDOF
      address-def OF s" -- ^a" DEF-STORAGE RAW-TRUST-NEXT ENDOF
      defer-def OF TRUST-DEFER ENDOF
      prim-row OF RECORD-PRIM-ROW ENDOF
      pprim-row OF RECORD-PPRIM ENDOF
      trusted-def OF TRUSTED-DEFINITION ENDOF
      cast-decl OF [: CAST-DECLARATION ;] IDENTITY-OUTCOME ENDOF
      linear-decl OF [: LINEAR-DECLARATION ;] IDENTITY-OUTCOME ENDOF
      undefine-name OF TICK-CONTEXT-UNKNOWN UNDEFINE-WORD ENDOF
      generates-row OF RECORD-GENERATES ENDOF
      immediate-mark OF
         TICK-CONTEXT-UNKNOWN  NEWEST-SYM @ CHECKER-IMMEDIATE
      ENDOF
      export-def OF RECORD-EXPORT ENDOF
      function-open OF s RECORD-FFI-FUNCTION ENDOF
      callback-open OF s RECORD-FFI-CALLBACK ENDOF
      function-clause OF s FFI-CLAUSE ENDOF
      function-close OF [: FFI-CLOSE ;] s FFI-AT ENDOF
      callback-close OF CLOSE-FFI-CALLBACK ENDOF
      unread OF s DW-PARSER TOP-OPAQUE ENDOF
      learned OF s RUN-LEARNED ENDOF
      created OF s RUN-CREATED ENDOF
   ;MATCH ;

: COLON-DEFINED ( -- )
   TOP-CUR-A @ TOP-CUR-U @ >str  DEF-NAME-A @ DEF-NAME-U @ >str  DEF-SIG-A @ DEF-SIG-U @ >str
   DEF-NAME-BYTE @ dup DEF-NAME-U @ + DEF-WORD DEFINED ;

: SKIP-DEFINITION ( -- )
   SCAN-SIG drop drop
   BODY-PREV-CLEAR
   LOCALS-RESET
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ >str s" ;" STR:= IF EXIT THEN
      SKIP-DEF-TOKEN
   AGAIN ;

: VERIFY-DEFINITION ( -- )
   TICK-DEF-LATCH
   BODY-RESET
   NAME-TOKEN dup STR:LEN TOKEN-U !  STR:DATA TOKEN-A !
   TOKEN-U @ 0= if NAME-STOP then
   TOP-CUR-A @ TOP-CUR-U @ >str TOKEN-A @ TOKEN-U @ >str REJECTION:NOT-A-NAME
   DEF-NAME!
   NAME-DECLARED
   DEF-NAME-A @ DEF-NAME-U @ >str DEF-NAME-BYTE @ REJECT-DUPLICATE IF SKIP-DEFINITION EXIT THEN
   WRAP-RESET
   BODY-PREV-CLEAR
   LOCALS-RESET
   TOKEN-A @ TOKEN-U @ >str BODY-APPEND
   MAYBE-SIGNATURE dup STR:LEN DEF-SIG-U !  STR:DATA DEF-SIG-A !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF UNCLOSED-STOP THEN
      TOKEN-A @ TOKEN-U @ >str s" ;" STR:= IF
         DEF-NAME-A @ DEF-NAME-U @ >str DEF-NAME-BYTE @ false REJECT-SHAPE IF EXIT THEN
         VERIFY-NAMED-BODY dup REPORT-DEFERRED
         dup -1 =  LEFT-AT-CALL? or IF VERIFY-WRAPPER THEN
         dup TALLY
         TICK-REMAINDER @ 0= IF
            -1 = INDEX-ON @ 0= or IF COLON-DEFINED THEN
         ELSE drop THEN EXIT
      THEN
      LOCAL-TOKEN? 0= IF
         TOKEN-A @ TOKEN-U @ >str s" does>" STR:=CI IF
            DEF-NAME-A @ DEF-NAME-U @ >str DEF-NAME-BYTE @ true REJECT-SHAPE IF
               SKIP-DEFINITION EXIT
            THEN
            DOES-AGAIN? IF DOES-TWICE-STOP  SKIP-DEFINITION EXIT THEN
            VERIFY-DOES IF
               DOES-ACCEPTED @ INDEX-ON @ 0= or IF COLON-DEFINED THEN
            THEN
            TICK-REMAINDER @ IF 2 TALLY THEN EXIT
         THEN
         TOKEN-A @ TOKEN-U @ >str WRAP-TOKEN
      THEN
      TICK-BODY-STEP
      APPEND-BODY-TOKEN
   AGAIN ;

: TOP-CLOSE ( -- )
   0 TOP-DEFER !  0 TOP-DEFER-U ! ;

\ No cause: TOP-VERDICT answers 3 both for a name only rendered text may define
\ and for a word whose effect names a type that may arrive.
: TOP-DUE ( -- )
   TOP-DEFER-U @ 0= IF EXIT THEN
   TOP-DEFER-A @ TOP-DEFER-U @ >str 0 DEFER-AT
   0 TOP-DEFER-U ! ;

\ Reads past the operand body keyword a u reads, as a body does; the keyword
\ has its record, so the source ending inside the operand adds none.
: SKIP-OPERAND ( str -- ) {: s:str :}
   s s" {:" STR:= IF
      BEGIN NEXT-RAW dup STR:LEN 0= IF drop EXIT THEN s" :}" STR:= UNTIL EXIT
   THEN
   s s" [type]" STR:=CI IF
      0 BEGIN
         SCAN-I @ TOKEN-FROM TYPE-SPAN-END? IF drop EXIT THEN
         NEXT-RAW dup STR:LEN 0= IF drop drop EXIT THEN
         CHECKER-TYPE-SPAN-STEP
      UNTIL drop EXIT
   THEN
   s s" is" STR:=CI IF NEXT-BODY drop EXIT THEN
   s BODY-PARSER? IF NEXT-RAW drop THEN ;

\ Every answer but a binding reports the token itself, so BOUND-CALL reports a
\ gap only where no other report is, and a token has one packet at most.
: TOP-RESOLVE ( str bool bool -- )
   {: s:str runs:bool renders:bool :}
   s runs TOP-VERDICT {: v:n :}
   v -1 = IF
      runs IF s renders BOUND-CALL THEN
      EXIT
   THEN
   v 2 = IF
      s ROOT-MOVE? IF s DW-ROOTS TOP-OPAQUE ELSE s TOP-PARSER THEN
      EXIT
   THEN
   \ A stretch holds names rendered text may define; an undefined name defines
   \ nothing, so the names after it are still resolved and rejected one by one.
   v 0= IF s STR:DATA TOP-REJECTED !  runs IF s SKIP-OPERAND THEN  EXIT THEN
   -1 TOP-DEFER !
   s STR:DATA TOP-DEFER-A !  s STR:LEN TOP-DEFER-U ! ;

\ The checker offers the spellings that bind at the cursor's token before it is
\ consumed, unless a stretch left to the run holds it.
: CSR-TOP ( -- )
   CSR-SUBJ @ 0= IF EXIT THEN
   CSR-HIT? 0= IF EXIT THEN
   CSR-STATE @ CSR-PART = IF CSR-AT @ CSR-TOK @ - ELSE 0 THEN {: n:n :}
   CSR-DONE CSR-STATE !
   TOP-DEFER @ 0<> IF EXIT THEN
   SOURCE@ CSR-TOK @ +  n >str  CHECKER-OWNER-ABI:VISIBLE-TOP
   ['] CSR-VISIT
   NCOMP-DISPATCH:DECL-VERIFY-EACH-VISIBLE-OFF OWNER-XT VISIBLE-ACTION execute ;

: TOP-OPERAND ( str -- )
   {: s:str :}
   OPERAND {: o:str :}
   s CHAR-KEYWORD? IF EXIT THEN
   CSR-TOP
   COMPOSE-ON @ INDEX-ON @ or 0= IF EXIT THEN
   \ A mover's token may be executed, so the tick ends the walk as a call would.
   o ROOT-MOVE? IF o DW-ROOTS TOP-OPAQUE EXIT THEN
   TOP-DEFER @ 0<> IF EXIT THEN
   o false false TOP-RESOLVE ;

\ A word that may define words this scan never reads (it renders source, as
\ `;FUNCTION` and `evaluate` do, calls `create`, or is deferred) has the
\ checker mark the statement's wordlist, with or without composition. Only here
\ does a word that calls `create` mark: a definer arm that takes the statement
\ records the name it makes. An undeclared renderer is deferred at its token
\ even when no later name uses it, inside an open stretch too.
: TOP-TOKEN ( str -- )
   {: s:str :}
   s num-parse nip nip IF EXIT THEN
   s s" [package]" STR:=CI IF EXIT THEN   \ pushes a name, calls nothing
   s STRING-OPENER? 0= IF TICK-CONTEXT-UNKNOWN THEN
   s NOMINAL-MARK
   s CTL-RENDERS RENDERS-MARK? {: renders:bool :}
   renders 0= IF s MARK-UNSEEN RENDERS-MARK? drop THEN
   \ Index safety still needs every call after a rejected declaration: an open
   \ diagnostic stretch must not hide a call that can change earlier bindings.
   COMPOSE-ON @ 0= TOP-DEFER @ 0<> or INDEX-ON @ 0= and IF
      \ A composition inside an open stretch still follows the requires after it.
      COMPOSE-ON @ IF s ROOT-MOVE? IF s DW-ROOTS TOP-OPAQUE EXIT THEN THEN
      renders IF s RENDER-GAP THEN
      EXIT
   THEN
   s true renders TOP-RESOLVE ;

\ A declaration a definer of the table above reads closes the stretch. One the
\ checker rejected while it was read (where rejections do not throw) opens an
\ unreported stretch instead: the scan recovers at the next definer or `:`, so
\ the tokens of a broken declaration are not reported as names.
: TOP-DEFINER? ( str -- bool )
   {: s:str :}
   s TICK-NATIVE-DEFINER? {: native:bool :}
   \ A library definer is a replaceable dictionary call, not structural syntax.
   INDEX-ON @ native 0= and IF false EXIT THEN
   TOKEN-BYTE @ {: at:n :}
   s TOP-BINDING nip {: sym:n ctl:n :}
   s s" host" STR:=CI  s s" target" STR:=CI or {: section:bool :}
   \ A rejected name goes on as an ordinary word, rejected as one.
   ctl CHECKER-OWNER-ABI:BINDING-REJECTED and 0<>
   section and IF false EXIT THEN
   section sym 0<> and IF s INTERPRETER:SECTION-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP THEN
   s SELECT-ARM {: arm:definer-arm :}
   arm DEFINER? 0= IF false EXIT THEN
   \ TOP-TOKEN judges no token an arm reads, so it is judged here.
   s DEFINER-USE
   REJECTION:TALLY {: before:n :}
   s arm RUN-ARM
   native 0= IF TICK-CONTEXT-UNKNOWN THEN
   native 0= IF s at sym ENGINE-ORIGIN-USE THEN
   TOP-CLOSE
   REJECTION:TALLY before <> IF -1 TOP-DEFER ! THEN
   0 0= ;

\ `kernel:` is the engine's synonym for `:`.
: COLON? ( str -- bool ) {: s:str :}
   s s" :" STR:=  s s" kernel:" STR:=CI or ;

: FLAG-TOP ( -- ^bool )
   FLAG-N @ 1 - FLAGS ;

: FLAG-PUSH ( bool -- )
   FLAG-N @ FLAG-CAP = IF drop 0 FLAG-N ! EXIT THEN
   FLAG-N @ FLAGS !  FLAG-N @ 1 + FLAG-N ! ;

: FLAG-STEP ( str -- ) {: s:str :}
   TOP-DEFER @ 0<> IF 0 FLAG-N ! EXIT THEN
   s TARGET-GUARD IF FLAG-PUSH EXIT THEN drop
   s s" 0=" STR:=CI IF s CORE-OP? ELSE false THEN IF
      FLAG-N @ 0= IF EXIT THEN
      FLAG-TOP @ 0= FLAG-TOP ! EXIT
   THEN
   0 FLAG-N ! ;

\ The name after the token the scan stands past, as the walk reads it: its tail.
: NEXT-NAME ( -- str )
   SCAN-I @ TOKEN-FROM TOKEN-TAIL ;

\ The walk's declaring forms: past a definition opener or an engine definer the
\ next token is a name the file declares, past `undefine` one it retires.
: WALK-NAMES ( str -- ) {: s:str :}
   s SOURCE-SYNTAX:BODY-OPENER?
   s SOURCE-SYNTAX:DEFINER-KEYWORD? or IF
      NEXT-NAME LOADER-BIT SHADOWED @ or SHADOWED ! EXIT
   THEN
   s s" undefine" STR:=CI IF
      NEXT-NAME LOADER-BIT RETIRED @ or RETIRED !
   THEN ;

\ The byte past the comment or string the walk reads the token a u as opening,
\ 0 when it opens neither; the scan stays where it is.
: WALK-SKIP-END ( str -- n ) {: s:str :}
   s COMMENT-END {: ch:n :}
   ch 0=  s STRING-OPENER? 0=  and IF 0 EXIT THEN
   SCAN-I @ {: mark:n :}
   s ESCAPED-STRING-OPENER? IF SKIP-ESCAPED-QUOTE ELSE
      ch 0= IF 34 ELSE ch THEN SKIP-PAST
   THEN
   SCAN-I @  mark SCAN-I ! ;

\ The byte a skipped arm's walk reads on from, past its last comment or string.
variable ARM-RESUME

\ The part of the raw token just read that the walk reads as a token: empty
\ inside a comment or string, else from ARM-RESUME on.
: ARM-TOKEN ( -- str )
   TOKEN-START @ ARM-RESUME @ max {: at:n :}
   SOURCE@ at +  SCAN-I @ at - 0 max >str ;

: ARM-STEP ( -- )
   ARM-TOKEN {: s:str :}
   s STR:LEN 0= IF EXIT THEN
   s WALK-SKIP-END {: end:n :}
   end 0<> IF end ARM-RESUME ! EXIT THEN
   s WALK-NAMES ;

\ The walk reads every arm, so WALK-NAMES reads a skipped one as the walk does;
\ `[if]` and `[then]` count over every raw token, as the engine's skip does.
: SKIP-ARM ( -- bool )
   0 ARM-RESUME !
   1 BEGIN dup 0 > WHILE
      NEXT-RAW dup STR:LEN 0= IF drop drop  false EXIT THEN
      ARM-STEP
      dup s" [if]" STR:=CI IF drop 1 + ELSE
         s" [then]" STR:=CI IF 1 - THEN
      THEN
   REPEAT drop
   true ;

\ A top-level `[if]` whose flag a target predicate settles, under any `0=`s,
\ reads only the text that flag chooses.
: KNOWN-IF? ( str -- bool ) {: s:str :}
   STR-LAST-KIND @ 0<> IF 0 FLAG-N ! THEN
   s s" [if]" STR:=CI 0= FLAG-N @ 1 <> or IF false EXIT THEN
   0 FLAGS @ IF true EXIT THEN
   SKIP-ARM 0= IF s 0 TOP-OPAQUE THEN
   true ;

\ A parsing keyword and its operand are one value on the interpret stack, and
\ the keyword stays the token before whatever follows: `char 0 TYPED-BUFFER B n`
\ hands the count reader `char`, which names the count's word.
: SCAN-TOP ( str -- )
   CSR-TOP
   dup WALK-NAMES
   dup KNOWN-IF? IF drop ELSE
   dup TOP-PARSER? IF TOP-OPERAND ELSE
   dup COLON? IF drop VERIFY-DEFINITION TOP-CLOSE ELSE
   dup COMPOSE-TOP? IF drop TOP-CLOSE ELSE
   dup TOP-DEFINER? IF drop ELSE TOP-TOKEN THEN THEN THEN THEN THEN
   \ A literal's record lasts the token after it.
   NULL-PTR 0 >str NULL-PTR 0 >str SRCLOC:LITERAL!
   TOP-CUR-A @ TOP-CUR-U @ >str FFI-LITERAL
   TOP-CUR-A @ TOP-CUR-U @ >str FLAG-STEP ;     \ after any file this token loaded

\ A record no source places is placed at the top-level token.
: VERIFY-SOURCE ( -- )
   SCAN-RESET
   SOURCE@ SOURCE-U @ >str SPLIT-STR-FILE? CHECKER-SPLIT-STR !
   NULL-PTR TOP-PREV-A !  0 TOP-PREV-U !  NULL-PTR TOP-REJECTED !
   0 SHADOWED !  0 RETIRED !  0 NEWEST-SYM !
   TOP-CLOSE
   BEGIN
      NEXT-SCAN dup STR:LEN 0 > TICK-REMAINDER @ 0= and WHILE
      TOP-DUE
      dup dup STR:LEN TOP-CUR-U !  STR:DATA TOP-CUR-A !
      [: TOP-CUR-A @ TOP-CUR-U @ >str SCAN-TOP ;] REJECTION:READING
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT drop
   TICK-REMAINDER @ IF EXIT THEN
   CSR-TOP                                      \ a cursor in the blanks the source ends with
   TOP-DUE
   NULL-PTR 0 >str FFI-UNCLOSED ;

: SOURCE-ARM ( -- )
   SOURCE@ SOURCE-U @ >str 1 1 0 REJECTION:SOURCE! ;

\ Nested files share the checker window but not the scanner cursor. The source,
\ token and open-stretch context, the loader words the file replaced, and
\ whether the file is the cursor's subject belong to the caller; declarations,
\ learned definers and whether a stretch was reported belong to the whole
\ composition. On return or throw the checker is re-armed with the caller's
\ bytes, or disarmed when the subject's scan ends, so no nested file's bytes,
\ which its loader frame releases, stay armed.
: COMPOSE-FILE-SCAN ( str str -- )
   {: src:str path:str :}
   path COMPOSE-DIAG$ {: diag:str :}
   SOURCE-A @ SOURCE-U @ >str SCAN-I @
   TOP-PREV-A @ TOP-PREV-U @ >str TOP-CUR-A @ TOP-CUR-U @ >str
   TOP-DEFER @ TOP-DEFER-A @ TOP-DEFER-U @ >str TOP-REJECTED @
   COMPOSE-CUR-PATH-A @ COMPOSE-CUR-PATH-U @ >str
   VISIT-CUR @ CSR-SUBJ @
   SHADOWED @ RETIRED @ CHECKER-SPLIT-STR @
   {: olds:str oldi:n
      oldprev:str oldcur:str
      olddefer:n olddefs:str oldrejected:^
      oldpath:str
      oldvisit:n oldsubj:n
      oldshadowed:n oldretired:n oldsplit:n :}
   src SOURCE!
   path STR:DATA COMPOSE-CUR-PATH-A !  path STR:LEN COMPOSE-CUR-PATH-U !
   path VISIT-OPEN
   VISIT-CUR @ VISIT-FIRST @ = CSR-SUBJ !
   \ ON-FILE runs while the requesting file stays armed, so a reader policy's
   \ rejection of the acquired bytes stands at the loader that loads them.
   [: FILE$ SOURCE@ SOURCE-U @ >str ON-FILE ;] catch
   dup 0= IF
      drop  SOURCE-ARM  diag REJECTION:FILE!
      [: VERIFY-SOURCE ;] catch
   THEN
   {: rc:n :}
   DISARM
   oldvisit VISIT-CUR !
   oldsubj CSR-SUBJ !
   olds STR:DATA SOURCE-A !  olds STR:LEN SOURCE-U !  oldi SCAN-I !
   oldprev STR:DATA TOP-PREV-A !  oldprev STR:LEN TOP-PREV-U !
   oldcur STR:DATA TOP-CUR-A !  oldcur STR:LEN TOP-CUR-U !
   olddefer TOP-DEFER !  olddefs STR:DATA TOP-DEFER-A !
   olddefs STR:LEN TOP-DEFER-U !  oldrejected TOP-REJECTED !
   oldpath STR:DATA COMPOSE-CUR-PATH-A !  oldpath STR:LEN COMPOSE-CUR-PATH-U !
   oldshadowed SHADOWED !  oldretired RETIRED !  oldsplit CHECKER-SPLIT-STR !
   oldpath STR:LEN 0 > IF
      oldpath COMPOSE-DIAG$ REJECTION:FILE!
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
   false TICK-REMAINDER !  ABANDON-FUNCTION  CB-CLOSED!
   0 CERTIFIED-N !  0 DEFERRED-N !  0 DEFER-SEEN !  SOURCE-SEALS-CLEAR
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF OWNER-XT VERIFIER-ACTION execute
   catch {: rc:n :}
   SOURCE-SEALS-CLEAR
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF OWNER-XT VERIFIER-ACTION execute
   split CHECKER-SPLIT-STR !
   rc 0<> IF rc throw THEN ;

: VERIFY-ARMED ( -- )
   SOURCE-ARM
   [: 0 [: VERIFY-SOURCE ;] REQUIRES:WITH-FILE ;] catch {: rc:n :}
   REJECTION:SOURCE-OFF
   rc 0<> IF rc throw THEN ;

: RUN ( -- )
   [: VERIFY-ARMED ;] RUN-IN-SCOPE ;

: COMPOSE-SUBJECT ( -- )
   COMPOSE-SUBJ-A @ COMPOSE-SUBJ-U @ >str
   COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ >str COMPOSE-FILE ;

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
: COMPOSE-UNREADABLE ( str n -- ) {: path:str e:n :}
   path FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ >str path e SOURCE-ROOT:UNREADABLE-SOURCE ;

\ PATH, resolved, changed while it was read.
: COMPOSE-CHANGED ( str -- ) {: path:str :}
   path FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ >str path SOURCE-ROOT:CHANGED-SOURCE ;

\ Reads the open file READ-PATHZ names to its end; a read the system fails
\ rejects the file with its errno. A read stops past SIZE, the stamp's size.
: READ-FILL ( n -- ) {: size:n :}
   0 READ-U !
   BEGIN
      READ-U @ READ-STEP + READ-BUF-RESERVE
      READ-FD @  READ-U @ READ-BUF  READ-STEP >span read
      dup 0 < IF READ-PATHZ READ-PATH-U @ >str FS:ERRNO COMPOSE-UNREADABLE THEN
      dup 0 >
   WHILE
      READ-U @ + READ-U !
      READ-U @ size > IF READ-PATHZ READ-PATH-U @ >str COMPOSE-CHANGED THEN
   REPEAT drop ;

\ Rejects PATH, a stream no stamp names, with the loader's record, placed at the
\ loader that loads it.
: REJECT-SOURCE-READ ( str -- ) {: path:str :}
   path FAULT-TARGET!
   LOAD-PLACE-A @ LOAD-PLACE-U @ >str path SOURCE-ROOT:UNSTAMPED-SOURCE ;

\ A stream (a FIFO, device or socket) is rejected before its open, which would
\ block on a FIFO; a changed file once it outgrows its size, else after the read.
: QUIET-READ ( str -- str )
   {: path:str :}
   0 READ-META-VALID !
   path READ-PATHZ PATH-CAP 1+ PATHZ
   path STR:LEN READ-PATH-U !
   path FS:TRY-FILE-META
   {: size:n msec:n mns:n csec:n cns:n valid:bool :}
   READ-PATHZ COMPILE-READS:STAMP COMPILE-READS:STABLE? 0= IF path REJECT-SOURCE-READ THEN
   READ-PATHZ open-rd {: fd:n :}
   fd 0 < IF path FS:ERRNO COMPOSE-UNREADABLE THEN
   fd READ-FD !
   [: size READ-FILL ;] catch {: rc:n :}
   fd close
   rc 0<> IF rc throw THEN
   path FS:TRY-FILE-META
   {: after:n amsec:n amns:n acsec:n acns:n still:bool :}
   size after = msec amsec = and mns amns = and
   csec acsec = and cns acns = and valid and still and 0= IF path COMPOSE-CHANGED THEN
   size READ-SIZE ! msec READ-MSEC ! mns READ-MNS !
   csec READ-CSEC ! cns READ-CNS !
   true READ-META-VALID !
   0 READ-BUF READ-U @ >str ;

public

\ The stamp of the disk bytes the quiet reader just handed to SOURCE-INPUT.
\ ON-FILE runs before another acquisition can replace it.
: QUIET-READ-META ( str -- n n n n n bool )
   {: path:str :}
   READ-META-VALID @ 0<> path STR:LEN READ-PATH-U @ = and
   path READ-PATHZ path STR:LEN >str STR:= and 0= IF 0 0 0 0 0 false EXIT THEN
   READ-SIZE @ READ-MSEC @ READ-MNS @ READ-CSEC @ READ-CNS @ true ;

private

create ENGINE-PATH PATH-CAP allot

: ENGINE-FULL-PATH ( str -- str bool )
   {: rel:str :}
   SOURCE-ROOT:ENGINE$ {: root:str :}
   root STR:LEN rel STR:LEN + 1+ PATH-CAP > if NULL$ false exit then
   root STR:DATA ENGINE-PATH root STR:LEN BYTE-COPY
   47 ENGINE-PATH root STR:LEN + c!
   rel STR:DATA ENGINE-PATH root STR:LEN 1+ + rel STR:LEN BYTE-COPY
   ENGINE-PATH root STR:LEN rel STR:LEN + 1+ >str true ;

: ENGINE-TARGET-READ ( n -- str n n bool )
   ENGINE-ORIGIN:TARGET
   {: rel:str at:n end:n name:str found:bool :}
   found 0= if NULL$ 0 0 false exit then
   rel ENGINE-FULL-PATH {: path:str fits:bool :}
   path at end fits ;

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

\ The last run stopped where it could not follow the source, and checked
\ nothing after it.
: STOPPED? ( -- bool )
   TICK-REMAINDER @ ;

\ The colon definitions the last run certified, and those it deferred to the
\ run, up to where it stopped.
: CENSUS ( -- n n )
   CERTIFIED-N @ DEFERRED-N @ ;

\ The completion cursor of the composition that starts next, which takes it:
\ subject byte N, whose place's spellings go to ON-CANDIDATE. A negative N is
\ none; the composition's end drops the cursor it took.
: CURSOR! ( n -- )
   CSR-NEXT ! ;

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
: COMPOSE-PATH-STOP ( str -- ) {: s:str :}
   ENTRY-TEXT @ COMPOSE-WORD$ s STR:LEN PATH-CAP REJECTION:PATH-RANGE
   s STR:DATA REJECTION:PLACES? IF s ELSE SRCLOC:TOKEN$ >str THEN REJECTION:AT
   s STR:LEN 0= IF s" empty path" ELSE s" too long" THEN REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass a path of 1 to " REJECTION:EMIT-STR  PATH-CAP REJECTION:EMIT-U
   s"  bytes" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Copies the subject's path and answers its registry row, or -1: the path is
\ the row's name for an engine file the registry holds, else canonical.
: COMPOSE-SUBJ! ( str -- n )
   SOURCE-ROOT:CANONICAL drop {: p:str :}
   p SOURCE-ROOT:ENGINE$ SOURCE-ROOT:RELATIVE {: q:str :}
   q REQUIRE-REG:ROW-OF 0 < IF p ELSE q THEN {: path:str :}
   path STR:DATA COMPOSE-SUBJ-PATH path STR:LEN BYTE-COPY
   path STR:LEN COMPOSE-SUBJ-PATH-U !
   COMPOSE-SUBJ-PATH path STR:LEN >str REQUIRE-REG:ROW-OF ;

\ The registry suffix and the caller's checker scope survive the composition's
\ success and throw.
: SOURCE-COMPOSE-LABELED-IN-SCOPE ( str str str -- )
   {: src:str path:str label:str :}
   path STR:LEN 0= path STR:LEN PATH-CAP > or IF path COMPOSE-PATH-STOP THEN
   label STR:LEN PATH-CAP > IF label COMPOSE-PATH-STOP THEN
   path COMPOSE-SUBJ! {: row:n :}
   label STR:DATA COMPOSE-SUBJ-LABEL label STR:LEN BYTE-COPY
   label STR:LEN COMPOSE-SUBJ-LABEL-U !
   src STR:DATA COMPOSE-SUBJ-A !  src STR:LEN COMPOSE-SUBJ-U !
   NULL-PTR COMPOSE-CUR-PATH-A !  0 COMPOSE-CUR-PATH-U !
   0 PRIM-OPEN-READY !
   REQUIRE-REG:COUNT COMPOSE-REQ0 !
   row 0 < IF
      COMPOSE-SUBJ-PATH COMPOSE-SUBJ-PATH-U @ >str REQUIRE-REG:STORE drop
      COMPOSE-REQ0 @ SOURCE-ROOT:CURRENT$ REQUIRE-REG:MINT
   ELSE
      row REQUIRE-REG:ID
   THEN {: subj:n :}
   \ Entry text is no file's, as on the load.
   ENTRY-TEXT @ IF 0 ELSE subj THEN {: id:n :}
   REQUIRES:CURRENT@ {: was:n :}
   CSR-TAKE
   -1 COMPOSE-ON !
   id REQUIRES:CURRENT!
   [: COMPOSE-WITH-USES ;] catch {: rc:n :}
   was REQUIRES:CURRENT!
   VISITS-DROP
   CSR-RESET
   0 COMPOSE-ON !
   COMPOSE-REQ0 @ REQUIRE-REG:TRUNCATE
   rc 0<> IF rc throw THEN ;

: COMPOSE-QUIET ( str str str bool -- )
   {: src:str path:str label:str entry:bool :}
   COMPOSE-ON @ 0<> IF entry COMPOSE-NESTED THEN
   0 FAULT-U !  0 DISCOVERY-STOPPED !
   [: SOURCE-ROOT:CANON-OS ;] [: QUIET-READ ;] SOURCE-INPUT:USE
   entry ENTRY-TEXT !
   [: src path label SOURCE-COMPOSE-LABELED-IN-SCOPE ;]
   catch {: rc:n :}
   false ENTRY-TEXT !
   SOURCE-INPUT:RESET
   rc 0<> IF rc throw THEN ;
public

\ Verify src srcu, the bytes supplied for path, composing each file a loader
\ loads under the caller's SOURCE-ROOT owner; its records name label, and a
\ bad loader form is rejected after the packets made before it.
: SOURCE-COMPOSE-QUIET-IN-SCOPE ( str str str -- )
   false COMPOSE-QUIET ;

\ SOURCE-COMPOSE-QUIET-IN-SCOPE of entry text, the bytes the load's command
\ line makes of its files: no file holds them, so their records name
\ REJECTION:NO-FILE$ and a load they make stands nowhere.
: SOURCE-COMPOSE-ENTRY-IN-SCOPE ( str str -- )
   REJECTION:NO-FILE$ true COMPOSE-QUIET ;

\ The file the last composition could not read, as it was resolved, or empty.
\ The string is borrowed until the next composition.
: FAULT-TARGET$ ( -- str )
   FAULT-TARGET FAULT-U @ >str ;

: SOURCE-BUF-IN-SCOPE ( str -- )
   SOURCE!
   RUN ;

\ Verify one supplied source, then run ACTION in the same scope. A name the
\ source declares binds through the record the checker's overlay publishes,
\ which this scope's close takes back unless a neutral checker scope around it
\ holds the overlay open, so a caller reading what the scan recorded by name
\ reads it here. No second verifier scope opens inside this one
\ (E-VERIFY-WINDOW). A scan that throws runs no action.
: SOURCE-BUF-THEN-IN-SCOPE ( str [ -- nominal ] -- )
   THEN-ACTION !
   SOURCE!
   [: VERIFY-THEN ;] RUN-IN-SCOPE ;

\ What the certify path says about one candidate definition, `NAME ( effect )
\ body`: -1 certified, 0 rejected, 1 uncheckable. A name this scanner
\ registered has no engine record, so a live candidate cannot bind it; here it
\ binds where the scan recorded it.
: CANDIDATE-IN-SCOPE ( str -- n )
   {: s:str :}
   [: s CHECK-QUIET-CANDIDATE! CAND-VERDICT ! ;] RUN-IN-SCOPE
   CAND-VERDICT @ ;

\ Verify a source in a candidate scope of its own: what it defines, and the
\ definer rows it learns, go when CHECKER-CANDIDATE-SCOPE-DONE rewinds the scope.
: SOURCE-BUF ( str -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   [: RUN ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc 0<> IF rc throw THEN ;

;package
