split-str
\ generated-declaration.f: the declaration layer's shared parts, loaded before
\ structure-decl.f: DECL-REJECT (the rejection record), DECL-REPLAY (a pre-lexed
\ token stream), the checker-frame and constructor-generation participants, and
\ DECL-FRONT (the driver the STRUCTURE and ENUM front ends wrap). The runner,
\ DECLARATION-TRANSACTION:RUN, is in generated-declaration-protection.f.

\ DECL-REJECT holds the one rejection record the STRUCTURE and ENUM front ends
\ raise through. RENDER prints it through package REJECTION.
\ The reason and fix are TYPE-DECL's armed reason (TDECL-ARM), the one store
\ every declaration catcher reads; a front end arms it with REJECT or EXPECT. A
\ code raised with no reason armed for it arrives bare, and PICK-REASON reads the
\ rows each code's owner lays with EXPLAIN. Declarations never nest, so the
\ record is one frame.
using TFAM
using TYPE-DECL

package DECL-REJECT
EXPORT CHECK:TDECL-DIAG
EXPORT CHECK:DEF-NAME-REJECT?

\ The slots hold copies: the family name is read on a declaration's first
\ line but rendered after a reject on a later one, and a REPL refills its
\ input buffer per line. Each slot also keeps the span it was copied from,
\ which a located token renders instead (TOKEN-SPAN@), until the declaration
\ ends.
3 constant SLOTS
96 constant SPAN-CAP        \ bytes a slot keeps; a longer span is capped, never overruns
3 constant MARK-LEN         \ bytes the truncation marker occupies inside SPAN-CAP
46 constant MARK-BYTE       \ ASCII '.', repeated MARK-LEN times

0 constant S-KIND           \ declaration kind word: "structure" / "enum"
1 constant S-FAMILY         \ declared family name ("" before the name is parsed)
2 constant S-TOKEN          \ offending token ("" when the fault has no token)

create SPAN-BUF  SLOTS SPAN-CAP * allot
create SPAN-LEN  SLOTS cells allot
SLOTS PTR-U8-TABLE SLOT-SRC-A        \ the span a slot was copied from: its bytes
create SLOT-SRC-U  SLOTS cells allot  \ and its length

: SRC! ( str n -- )   \ span, slot
   {: a:^ u:n s:n :}
   a SLOT-SRC-A s ptr-field !
   u s cells SLOT-SRC-U + ! ;
: SRC@ ( n -- str )
   {: s:n :}
   SLOT-SRC-A s ptr-field @  s cells SLOT-SRC-U + @ ;

public

\ Every exit of a declaration forgets the borrowed spans: one left behind is
\ read stale or baked into an image with its build-time address, so engine
\ generations differ.
: FORGET-SRC ( -- )
   SLOTS 0 ?do
      NULL-PTR SLOT-SRC-A i ptr-field !
      0 i cells SLOT-SRC-U + !
   loop ;

private

: CAPPED ( n -- n ) SPAN-CAP min ;

: COPY-BYTES ( ^u8 n ^u8 -- ) {: a:^ u:n d:^ :}
   0 BEGIN dup u < WHILE
      dup a + c@  over d + c!
      1 +
   REPEAT drop ;

\ Copies a span's first CAPPED bytes to d; a capped copy's last MARK-LEN bytes
\ become "...", so it never presents a prefix as the whole name.
: CAP-COPY ( str ^u8 -- ) {: a:^ u:n d:^ :}
   u CAPPED {: len:n :}
   a len d COPY-BYTES
   u SPAN-CAP > IF
      len MARK-LEN - BEGIN dup len < WHILE
         MARK-BYTE over d + c!
         1 +
      REPEAT drop
   THEN ;

: SLOT! ( ^u8 n n -- ) {: a:^ u:n s:n :}   \ span, slot
   u CAPPED s cells SPAN-LEN + !
   a u s SPAN-CAP * SPAN-BUF + CAP-COPY
   a u s SRC! ;
: SLOT@ ( n -- str ) {: s:n :}
   s SPAN-CAP * SPAN-BUF +  s cells SPAN-LEN + @ ;

\ The offending token: the span it was copied from while its source still
\ holds it, so the rejection places it; else the copy. SPAN-CAP bounds only
\ the copy, so a placed token renders whole.
: TOKEN-SPAN@ ( -- str )
   S-TOKEN SRC@  S-TOKEN SLOT@  REJECTION:KEPT ;

\ A reason row is the explaining file's data, laid by EXPLAIN where it runs:
\ the previous row, the code, the reason's and the fix's lengths, then
\ the reason padded to a cell, then the fix. Each
\ owner explains its codes reading its own constants, so a renumbered code
\ keeps its text: owners loaded before this file are explained at the end of
\ this package, later ones beside their constants. NEWEST and each row's first
\ cell are declared address cells, NULL-PTR for none, so the linker relocates
\ them.
PERSISTED-PTR-VARIABLE NEWEST
NEWEST ROLLBACK-CELLS:MARK

0 constant NO-CODE          \ no reason is armed

: NOTHING$ ( -- str ) s" " ;

\ The bytes a text of n bytes takes in a row.
: PADDED ( n -- n ) 1 cells 1- + 1 cells / cells ;

: ROW-PREV ( ^n -- ^n ) 0 ptr-field @ ;
: ROW-CODE ( ^n -- n ) cell+ @ ;
: ROW-REASON$ ( ^n -- str ) {: r:^ :}
   r 4 cells + BYTE-VIEW  r 2 cells + @ ;
: ROW-FIX$ ( ^n -- str ) {: r:^ :}
   r 4 cells +  r 2 cells + @ PADDED +  BYTE-VIEW  r 3 cells + @ ;

\ The newest row that explains a code, NULL-PTR when none does.
: REASON-ROW ( n -- ^n ) {: code:n :}
   NEWEST @
   BEGIN dup NULL-PTR <> WHILE
      dup ROW-CODE code = IF EXIT THEN
      ROW-PREV
   REPEAT ;

\ Whether a reason explains the code: one armed for it, or its row.
: REASON? ( n -- bool ) {: code:n :}
   TDECL-ARMED code =  code REASON-ROW NULL-PTR <>  or ;

\ The reason and fix for the code: the armed ones, else its row's, else empty.
: PICK-REASON ( n -- str str ) {: code:n :}
   TDECL-ARMED code = IF TDECL-WHY@ EXIT THEN
   code REASON-ROW {: r:^ :}
   r NULL-PTR = IF NOTHING$ NOTHING$ EXIT THEN
   r ROW-REASON$  r ROW-FIX$ ;

\ A rejection's label is composed here, grown to fit, and read when the record
\ prints, so the next rejection may reuse the buffer.
$80 constant LABEL-FIRST                \ bytes of the first mapping
PERSISTED-PTR-VARIABLE LABEL-P   NULL-PTR LABEL-P !
variable LABEL-CAP   0 LABEL-CAP !      \ bytes mapped at LABEL-P, 0 for none
variable LABEL-U

public

\ OPEN ( kind -- ): every field is cleared, so nothing from the previous
\ declaration is reported against this one.
: OPEN ( ^u8 n -- )
   S-KIND SLOT!
   NOTHING$ S-FAMILY SLOT!
   NOTHING$ S-TOKEN SLOT!
   NOTHING$ NOTHING$ NO-CODE TDECL-ARM ;

: FAMILY! ( str -- ) S-FAMILY SLOT! ;

\ Front ends record every body token here, so a fault raised inside a registry
\ or a participant names the token that provoked it. A new token disarms any
\ armed reason: an arming describes a fault expected at one token.
: TOKEN! ( ^u8 n -- )
   S-TOKEN SLOT!
   NOTHING$ NOTHING$ NO-CODE TDECL-ARM ;

\ A close-stage fault belongs to the whole declaration, so it is anchored on the
\ family, as sumtype.f's definers anchor it.
: AT-FAMILY ( -- )
   S-FAMILY SLOT@ TOKEN!
   S-FAMILY SRC@ S-TOKEN SRC! ;

\ EXPECT ( reason fix code -- ) : arm the reason and fix for a fault a deeper
\ owner may raise, immediately before the call that can raise it.
: EXPECT ( ^u8 n ^u8 n n -- ) TDECL-ARM ;

\ REJECT ( reason fix code -- code ) answers the code instead of throwing it, so
\ each reject site spells its own `throw` and the thrown value is visible there.
: REJECT ( ^u8 n ^u8 n n -- n ) {: wa:^ wu:n fa:^ fu:n code:n :}
   wa wu fa fu code EXPECT
   code ;

\ A label: LABEL-START empties it, LABEL+ appends text, LABEL$ is the text.
: LABEL-START ( -- ) 0 LABEL-U ! ;
: LABEL+ ( str -- ) {: a:^ u:n :}
   LABEL-P LABEL-CAP LABEL-FIRST LABEL-U @ u + ARENA-ROOM
   a LABEL-P @ LABEL-U @ + u ARENA-COPY
   LABEL-U @ u + LABEL-U ! ;
: LABEL$ ( -- str ) LABEL-P @ LABEL-U @ ;

\ Reflection for the suites: what the record would report right now.
: KIND$ ( -- ^u8 n ) S-KIND SLOT@ ;
: FAMILY$ ( -- str ) S-FAMILY SLOT@ ;
: TOKEN$ ( -- ^u8 n ) S-TOKEN SLOT@ ;
: REASON$ ( n -- str ) PICK-REASON 2drop ;

private

\ A sealed record's word note is read before AT-FAMILY, whose TOKEN! disarms it.
\ A variant or field name is read only after the family name passed the name rule.
: RENDER ( n -- ) {: code:n :}
   TOKEN-SPAN@ {: ta:^ tu:n :}
   code E-USING-AMBIGUOUS = IF KIND$ FAMILY$ ta tu TDECL-AMBIG-DIAG EXIT THEN
   code E-TDECL-SEALED = IF FAMILY$ TDECL-SEALED-WORD AT-FAMILY TOKEN-SPAN@ REJECTION:SEALED EXIT THEN
   code E-TDECL-DEF-NAME = IF FAMILY$ ta tu DEF-NAME-REJECT? 0= IF ta tu ta tu DEF-NAME-REJECT? drop THEN EXIT THEN
   KIND$ FAMILY$ ta tu  code PICK-REASON  ta tu code TDECL-WORD-NOTE  TDECL-DIAG ;

EXPORT CHECK:MULTI-ERR?
EXPORT TYPE-DECL:TDECL-NAME-PREFLIGHT-XT
public

\ The seal of the declaration whose header is a u, judged at its
\ family name alone, before the header's parameters are parsed.
: SEAL-FAMILY ( str -- ) {: a:^ u:n :}
   a u TYPE-DECL:DECL-FAMILY-NAME {: ca:^ cu:n :}
   cu 0 > IF ca cu FAMILY!  ca cu TDECL-NAME-PREFLIGHT-XT  a u FAMILY! THEN ;

\ Whether a declaration's catch throws code n on: a status in 1..255 was printed
\ where it arose, and a code no reason explains is no declaration's.
: THROWN-ON? ( n -- bool ) {: rc:n :}
   rc 1 >=  rc 255 <= and  rc REASON? 0= or ;

\ End one declaration whose code is 0 or one a reason explains. That code
\ renders, then counts in a multi-error load (DRIVE skipped to its terminator)
\ or stops the load.
: GUARD-CODE ( n -- )
   {: rc:n :}
   rc 0= IF FORGET-SRC EXIT THEN
   rc RENDER
   FORGET-SRC
   MULTI-ERR? IF REJECTION:KEEP EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

7182 constant E-REASON-DUP  \ the code already has a reason

\ EXPLAIN ( reason fix code -- ): the code's owner calls it once, at load,
\ reading its own constant. Each copy follows its allot: allot zeroes reused
\ space it hands out, padding included.
: EXPLAIN ( str str n -- ) {: a:^ u:n fa:^ fu:n code:n :}
   code REASON-ROW NULL-PTR <> IF E-REASON-DUP throw THEN
   datum here CELL-VIEW {: row:^ :}
   here ptr-cell-mark 0 ,
   NEWEST @ row 0 ptr-field !
   code ,  u ,  fu ,
   here BYTE-VIEW {: d:^ :}
   u PADDED allot
   a u d COPY-BYTES
   here BYTE-VIEW {: fd:^ :}
   fu PADDED allot
   fa fu fd COPY-BYTES
   row NEWEST ! ;

private

\ Reasons for codes whose owners load before EXPLAIN exists.
s" malformed declaration"
s" write `STRUCTURE name` or `ENUM name`, then its clauses, then `;STRUCTURE` or `;ENUM`"
E-TDECL-SYNTAX EXPLAIN
s" invalid type parameter header"
s" name each type parameter once in the header, as in `box<a>` or `result<a,b>`"
E-TDECL-ARITY EXPLAIN
s" unknown payload type"
s" give each field a type declared before it: `n`, `^u8`, a `STRUCTURE` or an `ENUM`"
E-TDECL-PAYLOAD EXPLAIN
s" unknown layout policy"
s" name `stack-cell-tag`, `packed-tag` or `niche-null` after `POLICY`"
E-TDECL-POLICY EXPLAIN
s" invalid layout policy for recursive sum"
s" drop the payload that names the family being declared: no family can hold itself"
E-TDECL-RECURSIVE EXPLAIN
s" record too wide to make"
s" give the record at most 4096 items: one per `FIELD` and one per buffer cell"
E-TDECL-CAP EXPLAIN
s" derive requirement not met"
s" derive a feature only when every payload type has it, or drop it from `DERIVE`"
E-TDECL-DERIVE EXPLAIN
\ Both front ends arm "reserved name" at their own name gates, so E-TDECL-NAME
\ reaching the table was raised by a deeper owner: the variant-name gate (a
\ reserved tail, a family-name collision or a word the DERIVE clause generates)
\ or the constructor planner (a generated word already defined, or two sharing a
\ name). The text has to be true of both.
s" name is reserved or already taken"
s" rename it so it is no reserved word and no word this declaration generates"
E-TDECL-NAME EXPLAIN
s" name must be a lowercase tail"
s" write the name as a lowercase tail with no package qualifier, as in `point`"
E-TFAM-CASE EXPLAIN
s" duplicate name in this package"
s" give each family, variant and field a name no other declaration in this package uses"
E-TFAM-DUP EXPLAIN
s" reserved field name"
s" rename the field: a declaration word such as `field` or a generated word such as `make`, `tag` or `eq` is taken"
E-PF-NAME EXPLAIN
s" name is a keyword or reads as a number"
s" rename it: a keyword or a number names no definition"
E-TDECL-DEF-NAME EXPLAIN

;package

\ DECL-REPLAY: one installed token stream plus one mode flag.
\ src/habu/verify-source.f's declaration pre-pass registers declarations it
\ already lexed: a front end reads the next token here instead
\ of `parse-name`, and the constructor generators read RP-ACTIVE? to define no
\ word. One state carries both facts, so no caller can ask for a replay that
\ generates or a live declaration that does not. The front ends' grammar,
\ registry calls and records are those of a live declaration, so a replay
\ registers the same rows.
\ The stream is HEAD (the family name the callers lexed first) and BODY (the
\ space-separated rest). A zero-length HEAD reaches the front end's "missing
\ name" gate instead of promoting the first body token.
package DECL-REPLAY

32 constant RP-SPACE
9 constant RP-TAB
10 constant RP-LF
13 constant RP-CR

variable RP-OPEN?      \ a replay stream is installed (0 = tokens come from live input)
variable RP-HEAD?      \ the leading (family name) token has not been read yet
variable RP-HEAD-U     PTR-VARIABLE RP-HEAD-A
variable RP-BODY-U     PTR-VARIABLE RP-BODY-A
variable RP-SCAN-I     \ scan cursor within the body buffer

: RP-HEAD! ( str -- ) RP-HEAD-U ! RP-HEAD-A ! ;
: RP-HEAD@ ( -- str ) RP-HEAD-A @ RP-HEAD-U @ ;
: RP-BODY! ( str -- ) RP-BODY-U ! RP-BODY-A ! ;
: RP-BODY@ ( -- str ) RP-BODY-A @ RP-BODY-U @ ;

: RP-SEP? ( n -- bool ) {: c:n :}
   c RP-SPACE = c RP-TAB = or c RP-LF = or c RP-CR = or ;

: RP-SKIP ( str -- n ) {: a:^ u:n :}   \ first non-separator offset at/after the cursor
   RP-SCAN-I @
   BEGIN dup u < WHILE
      dup a + c@ RP-SEP? 0= IF EXIT THEN
      1 +
   REPEAT ;

: RP-TOKEN-END ( str n -- n ) {: a:^ u:n s:n :}   \ offset one past the token starting at s
   s
   BEGIN dup u < WHILE
      dup a + c@ RP-SEP? IF EXIT THEN
      1 +
   REPEAT ;

: RP-BODY-NEXT ( -- ^u8 n )         \ next body token; zero length once the buffer is spent
   RP-BODY@ {: a:^ u:n :}
   a u RP-SKIP {: s:n :}
   a u s RP-TOKEN-END {: e:n :}
   e RP-SCAN-I !
   a s +  e s - ;

public

: RP-ACTIVE? ( -- bool ) RP-OPEN? @ 0 <> ;

\ RP-CLAIM ( name body -- ): install one declaration's lexed token stream. It
\ never copies and cannot see a token already dropped, so a caller's buffer
\ builder must hold the whole declaration or raise, never truncate
\ (verify-source.f BODY-APPEND grows its buffers to every run). Comments pass through raw: the live
\ keyword reads `\` or `(` as an ordinary token and refuses it, so replay must
\ too.
: RP-CLAIM ( ^u8 n ^u8 n -- )
   RP-BODY!  RP-HEAD!
   0 RP-SCAN-I !
   -1 RP-HEAD? !
   -1 RP-OPEN? ! ;

\ Callers release on both paths, so a rejected replay cannot leave the next live
\ declaration reading a spent buffer.
: RP-RELEASE ( -- )
   0 RP-OPEN? !  0 RP-HEAD? !
   NULL-PTR 0 RP-HEAD!
   NULL-PTR 0 RP-BODY!
   0 RP-SCAN-I ! ;

\ RP-NEXT: the family name once, the body tokens, then zero-length forever, the
\ end-of-input signal `parse-name` gives, so the "missing ;STRUCTURE" / "missing
\ ;ENUM" gates fire on a truncated stream.
: RP-NEXT ( -- str )
   RP-HEAD? @ IF 0 RP-HEAD? ! RP-HEAD@ EXIT THEN
   RP-BODY-NEXT ;

: RP-RUN ( str str [ -- ] -- )
   {: na:^ nu:n ba:^ bu:n body :}
   na nu ba bu RP-CLAIM
   body catch {: rc:n :}
   RP-RELEASE
   rc 0 <> IF rc throw THEN ;

private
;package

\ The checker-frame participant. DECL-MARK is the owner mark START stores in
\ the declaration's rollback frame (checker.f RBF.COORD) and PREPARE and
\ ROLLBACK match. Any value but RBF-NO-COORDINATOR marks a declaration frame,
\ and declarations never nest, so one constant serves.
package CHECKER-DECL-FRAME

1 constant DECL-MARK

public

: PART-SNAPSHOT ( -- ) DECL-MARK START ;

: PART-PREPARE ( -- )
   DECL-MARK PREPARE 0= IF
      s" the declaration left the checker's frame stack unbalanced" EXIT-STATUS:INTERNAL die
   THEN ;

: PART-ROLLBACK ( -- ) DECL-MARK ROLLBACK ;

\ Total: RELEASE is throw-free by contract.
: PART-RELEASE ( -- ) RELEASE ;

seal-package
;package

\ One family's checked generated constructors, made from ARM's family inside
\ the transaction; a throw mid-set rolls back and publishes none.
package TYPE-DECL
private
package GENERATED-DECL-CTOR

-1 constant NO-FAMILY

variable ARMED   NO-FAMILY ARMED !   \ the family this declaration armed

: ARMED-FAM ( -- n ) ARMED @ ;
: DISARM ( -- ) NO-FAMILY ARMED ! ;

\ The one gate: a family owns generated constructors when it has a variant
\ and is a sum or an enum, the two ENUM front-end kinds; its visibility picks
\ only their spelling. A product is excluded: structure-make.f generates its
\ own make/unmake.
: GEN-OK? ( n -- bool ) {: fam:n :}
   fam TFAM-VAR-COUNT@ 0 <= IF 0 0= 0= EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or ;

public

: PART-SNAPSHOT ( -- )
   DISARM ;

\ Replay builds and checks the same words to publish their effects and emits no
\ runtime code: a call needs the checked effect, not only the package stamp.
: PART-COMMIT ( -- )
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY = IF EXIT THEN
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: count:n :}
   fam vstart count TDECL-CTOR-PUBLISH
   DECL-REPLAY:RP-ACTIVE? IF fam TDECL-CTOR-REPLAY EXIT THEN
   fam TDECL-CTOR-WORDS-BODY ;

: PART-ROLLBACK ( -- )
   DISARM ;

\ Total: publication has already happened, and the only thing left is this
\ owner's one armed-family cell.
: PART-RELEASE ( -- )
   DISARM ;

: ARM ( n -- ) ARMED ! ;

: OWNS? ( n -- bool ) GEN-OK? ;

private

seal-package

;package
;package

\ DECL-FRONT: what the STRUCTURE and ENUM front ends share around their
\ grammar: reject codes, the token source with one-token pushback and a
\ lookahead span, per-declaration parse cells, a replay's field-name window, the
\ guarded run with resynchronization, and the replay entries; SUMTYPE reads
\ only the token source. One set of cells serves every declaration: a front end
\ reads its body as tokens and runs none, so declarations never nest. Every
\ borrowed span is forgotten as the declaration ends (RESET and MISS-CLEAR in
\ DRIVE, REPLAY-END for an E-UNRESOLVED replay's span).
\ A swallowed reject (multi-error load) leaves the tokens up to the terminator
\ ahead of the interpreter, which would run them: `ENUM m7bad red red blue ;ENUM
\ NEWTYPE m7cont<a>` would die `E-UNDEFINED` at `blue`. DRIVE skips to the terminator,
\ except once NEXT has handed it out, in whatever role a front end read it
\ (SEEN-END: that skip would eat the next declaration), or at end of input.
\ NAME-TOKEN reads a STRUCTURE's or an ENUM's own name and never counts it
\ as the terminator: the verifier's scan reads that place as the name. A
\ MUTUAL member's name counts, since the scan ends a MUTUAL at its first
\ `;MUTUAL`.
\ The skip reads through RAW, not NEXT, so the record keeps naming the token.
\ QUOT-ROLLBACK after a reject is unconditional: it zeroes sumtype.f TDV-QUOT-N
\ and TDV-QUOT-MISS, which only a quotation field dirties.
package TYPE-DECL
private
package DECL-FRONT

public

7192 constant E-UNRESOLVED          \ private replay outcome after rollback

: REQUIRE-DERIVABLE ( str n n -- )
   {: ta:^ tu:n fam:n node:n :}
   fam TFAM-DERIVE-EQ? fam TFAM-DERIVE-HASH? or 0= IF EXIT THEN
   fam node TDECL-DERIVE-WHY {: wa:^ wu:n fa:^ fu:n :}
   wu 0= IF EXIT THEN
   ta tu DECL-REJECT:TOKEN!
   wa wu fa fu E-TDECL-DERIVE DECL-REJECT:REJECT throw ;

110 constant ASCII-N
102 constant ASCII-F
114 constant ASCII-R

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

variable FIELD-MISS          \ the field under resolution named a type that may still arrive
variable OUTCOME-NAMES       \ a replay wants the field-name window checked (REPLAY-OUTCOME)

private

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span in a
\ declared pointer cell.
variable PEND-U   PTR-VARIABLE PEND-A
\ The terminator DRIVE runs to. NEXT handing it out sets SEEN-END and UNGET
\ taking it back clears it, whichever front end read it, so the
\ resynchronization a reject asks for knows whether it was consumed.
PTR-VARIABLE TERM-A   variable TERM-U
variable SEEN-END            \ this declaration's terminator has been consumed
public
\ TERMINATOR? ( token -- bool ) : the token is the running declaration's
\ terminator; a span a front end reads ahead stops there (SD-SPAN-REST).
: TERMINATOR? ( str -- bool ) TERM-A @ TERM-U @ CORE-STR=CI ;
private
: PEND! ( str -- ) PEND-U ! PEND-A ! ;
: PEND@ ( -- str ) PEND-A @ PEND-U @ ;
PTR-VARIABLE HEAD-A   variable HEAD-U   variable HEAD-I   \ a span read ahead, re-read token by token before the source
: HEAD-CLEAR ( -- ) NULL-PTR HEAD-A ! 0 HEAD-U ! 0 HEAD-I ! ;

PTR-VARIABLE TYPE-A   variable TYPE-U     \ the field type token under resolution
PTR-VARIABLE MISS-A   variable MISS-U     \ the first unresolved type span, kept for REPLAY-END
\ A replay's field names, each after its count cell: one structure member's, or
\ one variant's, since a front end's SEEN-CLEAR.
PTR-VARIABLE SEEN-P
SCRATCH-VARIABLE SEEN-CAP                 \ SEEN-P's room, in cells
: SEEN ( -- ^u8 ) SEEN-P @ ;
: SEEN-LEN ( n -- ^n ) SEEN-P @ + ;       \ the count cell at byte offset n
: SEEN-REC ( n -- n ) CELL + 1 - CELL / cells CELL + ;   \ a name's record bytes, whole cells
variable SEEN-U
variable SEEN-I

: MISS-CLEAR ( -- ) NULL-PTR MISS-A ! 0 MISS-U ! ;

public

: TYPE! ( str -- ) TYPE-U ! TYPE-A ! ;
: TYPE@ ( -- str ) TYPE-A @ TYPE-U @ ;

\ MISSING! ( span -- ) : the type under resolution may still arrive. The first
\ such span is what a replay reports (REPLAY-OUTCOME); later ones are dropped.
: MISSING! ( str -- )
   -1 FIELD-MISS !
   MISS-U @ 0= IF MISS-U ! MISS-A ! ELSE 2drop THEN ;
: MISSING? ( -- bool ) MISS-U @ 0 <> ;

: SEEN-CLEAR ( -- ) 0 SEEN-U ! ;

\ HEAD! ( span -- ) : retain a span the front end read ahead of its grammar;
\ RAW hands it back token by token before reading the source again (ENUM's
\ mode selection reads to the first variant before registering the family).
: HEAD! ( str -- ) HEAD-U ! HEAD-A !  0 HEAD-I ! ;

\ Tokens come from the live input or a replay stream (DECL-REPLAY); RAW checks
\ the pushback and the lookahead span first, so lookahead is the same on both.
: SOURCE-NEXT ( -- str )
   DECL-REPLAY:RP-ACTIVE? IF DECL-REPLAY:RP-NEXT EXIT THEN
   parse-name ;

private

: HEAD-NEXT ( -- str )
   HEAD-A @ {: a:^ :}
   BEGIN HEAD-I @ HEAD-U @ < IF
      a HEAD-I @ + c@ 32 <= ELSE NO THEN WHILE
      HEAD-I @ 1+ HEAD-I !
   REPEAT
   HEAD-I @ {: start:n :}
   BEGIN HEAD-I @ HEAD-U @ < IF
      a HEAD-I @ + c@ 32 > ELSE NO THEN WHILE
      HEAD-I @ 1+ HEAD-I !
   REPEAT
   HEAD-I @ start - {: size:n :}
   HEAD-I @ HEAD-U @ = IF HEAD-CLEAR THEN
   a start + size ;

: RAW ( -- ^u8 n )                  \ next body token: pushback, lookahead, source
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   HEAD-U @ 0 <> IF HEAD-NEXT EXIT THEN
   SOURCE-NEXT ;

public

: NEXT ( -- ^u8 n )                 \ next body token, recorded for diagnostics
   RAW 2dup TERMINATOR? IF -1 SEEN-END ! THEN
   2dup DECL-REJECT:TOKEN! ;
: UNGET ( str -- ) 2dup TERMINATOR? IF 0 SEEN-END ! THEN PEND! ;
: NAME-TOKEN ( -- str )           \ a name, recorded, never the terminator
   RAW 2dup DECL-REJECT:TOKEN! ;

: POLICY-CODE ( str -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop s" missing layout policy name"
      s" name a policy after `POLICY`: `stack-cell-tag`, `packed-tag` or `niche-null`"
      E-TDECL-POLICY DECL-REJECT:REJECT throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop TL-STACK-CELL-TAG EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop TL-PACKED-TAG EXIT THEN
   2dup s" niche-null" CORE-STR=CI IF 2drop TL-NICHE EXIT THEN
   2drop s" unknown layout policy"
   s" name `stack-cell-tag`, `packed-tag` or `niche-null` after `POLICY`"
   E-TDECL-POLICY DECL-REJECT:REJECT throw ;

private

: SEEN? ( str -- bool )
   {: a:^ u:n :}
   0 SEEN-I !
   BEGIN SEEN-I @ SEEN-U @ < WHILE
      SEEN-I @ SEEN-LEN @ {: len:n :}
      len u = IF
         SEEN SEEN-I @ + CELL + len a u CORE-STR= IF YES EXIT THEN
      THEN
      SEEN-I @ len SEEN-REC + SEEN-I !
   REPEAT NO ;

EXPORT CHECK:ARENA-ROWS-ENSURE
EXPORT CHECK:MULTI-ERR?
public

\ NAME-CHECK ( name fix -- ) : a replay's field-name gate. The name is
\ validated, rejected as a duplicate of one recorded since SEEN-CLEAR with the
\ caller's help text, the one its loader gives, and recorded.
: NAME-CHECK ( str str -- )
   {: a:^ u:n f:^ fu:n :}
   a u DECL-REJECT:TOKEN!
   s" duplicate field name" f fu E-TFAM-DUP DECL-REJECT:EXPECT
   a u PF-NAME-REQUIRE
   a u SEEN? IF E-TFAM-DUP throw THEN
   SEEN-P SEEN-CAP  SEEN-U @ u SEEN-REC + CELL /  1 ARENA-ROWS-ENSURE
   u SEEN-U @ SEEN-LEN !
   0 SEEN-I !
   BEGIN SEEN-I @ u < WHILE
      a SEEN-I @ + c@
      SEEN SEEN-U @ + CELL + SEEN-I @ + c!
      SEEN-I @ 1+ SEEN-I !
   REPEAT
   SEEN-U @ u SEEN-REC + SEEN-U ! ;

private

EXPORT TYPE-DECL:TDECL-TXN-XT

: RESET ( -- )                          \ base state, on both exits of DRIVE and at load
   NULL-PTR 0 PEND!  HEAD-CLEAR  NULL-PTR 0 TYPE!
   NULL-PTR TERM-A !  0 TERM-U !  0 SEEN-END !  0 FIELD-MISS !  SEEN-CLEAR ;

: SKIP-BODY ( str -- )             \ to the terminator, or end of input
   {: ta:^ tu:n :}
   BEGIN
      RAW dup 0= IF 2drop EXIT THEN
      2dup ta tu CORE-STR=CI IF 2drop EXIT THEN
      2drop
   AGAIN ;
: RESYNC ( str -- )
   SEEN-END @ IF 2drop EXIT THEN
   MULTI-ERR? 0= IF 2drop EXIT THEN
   SKIP-BODY ;

\ DRIVE ( terminator body -- code ) : one declaration through the transaction,
\ resynchronized and reset on the way out; 0 or a reject code a reason explains,
\ and any other code thrown on. The body quotation rides the data stack through
\ the catch (docs/forth.md, checked catch): the quotation is stack-preserving,
\ so the xt is dropped after it.
: DRIVE ( str [ -- renders parses nominal ] -- n )
   {: ta:^ tu:n body :}
   MISS-CLEAR RESET
   ta TERM-A !  tu TERM-U !
   body [: dup TDECL-TXN-XT ;] catch {: rc:n :} drop
   rc 0= IF RESET 0 EXIT THEN
   QUOT-ROLLBACK
   ta tu RESYNC
   RESET
   rc E-UNRESOLVED <> IF MISS-CLEAR THEN
   rc DECL-REJECT:THROWN-ON? IF DECL-REJECT:FORGET-SRC rc throw THEN
   rc ;

: REPLAY-END ( n -- str bool )    \ direct missing span after rollback
   dup E-UNRESOLVED = IF
      drop MISS-A @ MISS-U @
      0 DECL-REJECT:GUARD-CODE
      YES MISS-CLEAR EXIT THEN
   DECL-REJECT:GUARD-CODE
   NULL-PTR 0 NO ;

public

\ RUN ( kind terminator body -- ) : one live declaration.
: RUN ( ^u8 n ^u8 n [ -- renders parses nominal ] -- )
   {: ka:^ ku:n ta:^ tu:n body :}
   ka ku DECL-REJECT:OPEN
   ta tu body DRIVE DECL-REJECT:GUARD-CODE ;

\ REPLAY-OUTCOME ( name body-text kind terminator body -- span bool ) : register
\ a declaration from tokens a tool has already lexed, defining no word, and
\ answer whether a type it names may still arrive, and which. DRIVE throws
\ E-UNRESOLVED on: no reason explains it.
: REPLAY-OUTCOME ( str str str str [ -- renders parses nominal ] -- str bool )
   {: na:^ nu:n ba:^ bu:n ka:^ ku:n ta:^ tu:n body :}
   na nu ba bu DECL-REPLAY:RP-CLAIM
   ka ku DECL-REJECT:OPEN
   YES OUTCOME-NAMES !
   0 [: drop ta tu body DRIVE ;] catch {: code rc:n :}
   0 OUTCOME-NAMES !
   DECL-REPLAY:RP-RELEASE
   rc 0= IF code REPLAY-END EXIT THEN
   rc E-UNRESOLVED = IF E-UNRESOLVED REPLAY-END EXIT THEN
   rc throw ;

private

RESET MISS-CLEAR

;package
;package

;using
;using
