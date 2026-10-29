\ generated-declaration.f: the declaration layer's shared parts, loaded before
\ structure-decl.f: DECL-REJECT (the reject packet), DECL-REPLAY (a pre-lexed
\ token stream), the checker-frame and constructor-generation participants, and
\ DECL-FRONT (the driver both front ends wrap). The runner,
\ DECLARATION-TRANSACTION:RUN, is in generated-declaration-protection.f.

\ DECL-REJECT is the one reject packet both front ends raise through. RENDER
\ forwards to render.f TDECL-DIAG, which also feeds the capture buffer
\ (RDIAG-APPEND) tools/check-core.f's CHK-DECL-CAPTURE / CHK-DECL-FLUSH read.
\ A front end arms its own reason (REJECT, EXPECT); a code raised deeper arrives
\ bare and PICK-REASON reads the table each code's owner fills with EXPLAIN. An
\ armed reason counts only for the code it was armed for. Declarations never
\ nest, so the packet is one frame.
using TFAM
using TYPE-DECL

package DECL-REJECT

\ The slots hold copies: a declaration body spans several input lines, the
\ engine refills its input buffer per line from a stream, and the family name is
\ read on the first line but rendered after a reject on a later one. Each slot
\ also keeps the span it was copied from, which a located token renders instead
\ (TOKEN-SPAN@), until GUARD-CODE ends the declaration.
4 constant SLOTS
35 constant REASON-CAP      \ reason-table rows: the codes the engine's owners explain
96 constant SPAN-CAP        \ per-slot bytes; a longer copy is capped, never overruns
3 constant MARK-LEN         \ bytes the truncation marker occupies inside SPAN-CAP
46 constant MARK-BYTE       \ ASCII '.', repeated MARK-LEN times

0 constant S-KIND           \ declaration kind word: "structure" / "enum"
1 constant S-FAMILY         \ declared family name ("" before the name is parsed)
2 constant S-TOKEN          \ offending token ("" when the fault has no token)
3 constant S-REASON         \ short reason text

create SPAN-BUF  SLOTS REASON-CAP + SPAN-CAP * allot
create SPAN-LEN  SLOTS REASON-CAP + cells allot
SLOTS PTR-U8-TABLE SLOT-SRC-A        \ the span a slot was copied from: its bytes
create SLOT-SRC-U  SLOTS cells allot  \ and its length

\ SLOT! clamps to SPAN-CAP and marks a capped copy: its last MARK-LEN bytes
\ become "...", so the packet never presents a prefix as the whole name.
: SRC! ( ptr u8 n n -- )   \ span, slot
   {: a:ptr u:n s:n :}
   a SLOT-SRC-A s ptr-field !
   u s cells SLOT-SRC-U + ! ;
: SRC@ ( n -- ptr u8 n )
   {: s:n :}
   SLOT-SRC-A s ptr-field @  s cells SLOT-SRC-U + @ ;
: FORGET-SRC ( -- )
   SLOTS 0 ?do
      NULL-PTR SLOT-SRC-A i ptr-field !
      0 i cells SLOT-SRC-U + !
   loop ;
: SLOT! ( ptr u8 n n -- ) {: a:ptr u:n s:n :}   \ span, slot
   u SPAN-CAP > IF SPAN-CAP ELSE u THEN {: len:n :}
   len s cells SPAN-LEN + !
   s SPAN-CAP * SPAN-BUF + {: d:ptr :}
   0 BEGIN dup len < WHILE
      dup a + c@  over d + c!
      1 +
   REPEAT drop
   u SPAN-CAP > IF
      len MARK-LEN - BEGIN dup len < WHILE
         MARK-BYTE over d + c!
         1 +
      REPEAT drop
   THEN
   s SLOTS < IF a u s SRC! THEN ;
: SLOT@ ( n -- ptr u8 n ) {: s:n :}
   s SPAN-CAP * SPAN-BUF +  s cells SPAN-LEN + @ ;

\ The offending token: the span it was copied from when that lies in text a
\ driver armed for locating packets (checker.f DIAG>SRC), so the packet carries
\ its file position; else the copy. An armed text stays put while its
\ declaration runs; a span in an input buffer the engine refills lies in no
\ armed text, so stale bytes are never read. SPAN-CAP bounds only the copy, so a
\ located token renders whole.
: TOKEN-SPAN@ ( -- ptr u8 n )
   S-TOKEN SRC@
   {: a:ptr u:n :}
   a DIAG>SRC nip IF a u EXIT THEN
   S-TOKEN SLOT@ ;

\ The reason table: row r is a code in REASON-CODES and its text in slot
\ SLOTS + r. Each code's owner explains it with EXPLAIN, reading its own
\ constant by name, so a renumbered code keeps its text: owners loaded before
\ this file are explained at the end of this package, later ones beside their
\ constants. REASON-CAP is the number of codes the engine's owners explain;
\ EXPLAIN refuses a code past it, or one explained twice, by name.
create REASON-CODES  REASON-CAP cells allot
variable REASONS            \ rows explained so far

0 constant NO-CODE          \ no reason is armed

variable ARMED              \ the code the armed reason explains (NO-CODE = none)

: NOTHING$ ( -- ptr u8 n ) s" " ;
: FALLBACK$ ( -- ptr u8 n ) s" declaration failed" ;

: CODE-AT ( n -- n ) cells REASON-CODES + @ ;

\ The row that explains a code, or REASONS when none does: an unexplained code
\ stays distinguishable from an explained one instead of reading as the default.
: REASON-ROW ( n -- n ) {: code:n :}
   REASONS @ 0 ?do
      i CODE-AT code = IF i unloop EXIT THEN
   loop
   REASONS @ ;

: CODE-REASON ( n -- ptr u8 n ) {: code:n :}
   code REASON-ROW {: row:n :}
   row REASONS @ < IF SLOTS row + SLOT@ EXIT THEN
   FALLBACK$ ;

: PICK-REASON ( n -- ptr u8 n ) {: code:n :}
   ARMED @ code = IF S-REASON SLOT@ EXIT THEN
   code CODE-REASON ;

: RENDER ( n -- ) {: code:n :}
   S-KIND SLOT@  S-FAMILY SLOT@  TOKEN-SPAN@  code PICK-REASON  TDECL-DIAG ;

public

\ OPEN ( kind -- ): every field is cleared, so nothing from the previous
\ declaration is reported against this one.
: OPEN ( ptr u8 n -- )
   S-KIND SLOT!
   NOTHING$ S-FAMILY SLOT!
   NOTHING$ S-TOKEN SLOT!
   FALLBACK$ S-REASON SLOT!
   NO-CODE ARMED ! ;

: FAMILY! ( ptr u8 n -- ) S-FAMILY SLOT! ;

\ Front ends record every body token here, so a fault raised inside a registry
\ or a participant names the token that provoked it. A new token disarms any
\ armed reason: an arming describes a fault expected at one token.
: TOKEN! ( ptr u8 n -- )
   S-TOKEN SLOT!
   NO-CODE ARMED ! ;

\ A close-stage fault belongs to the whole declaration, so it is anchored on the
\ family, as sumtype.f's definers anchor it.
: AT-FAMILY ( -- )
   S-FAMILY SLOT@ TOKEN!
   S-FAMILY SRC@ S-TOKEN SRC! ;

\ EXPECT ( reason code -- ) : arm the reason for a fault a deeper owner may
\ raise, immediately before the call that can raise it.
: EXPECT ( ptr u8 n n -- ) {: wa:ptr wu:n code:n :}
   wa wu S-REASON SLOT!
   code ARMED ! ;

\ REJECT ( reason code -- code ) answers the code instead of throwing it, so
\ each reject site spells its own `throw` and the thrown value is visible there.
: REJECT ( ptr u8 n n -- n ) {: wa:ptr wu:n code:n :}
   wa wu code EXPECT
   code ;

\ GUARD-CODE ( code -- ): end one declaration DECL-FRONT ran. 0 forgets the
\ borrowed spans; a reject renders after the runner rolled back, then rethrows
\ its exact code (CHECKER-REFUSE). Under `--all-errors` (MULTI-ERR?) the reject
\ is counted and the load continues, as sumtype.f TDECL-RUN does; DRIVE has
\ already skipped the stream to the terminator, which only the front end knows.
\ The spans are forgotten on both exits: one left behind would be read stale or
\ baked into an image with its build-time address, so engine generations differ.
: GUARD-CODE ( n -- )
   {: rc:n :}
   rc 0= IF FORGET-SRC EXIT THEN
   rc RENDER
   FORGET-SRC
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   rc CHECKER-REFUSE ;

\ Reflection for the suites: what the packet would report right now.
: KIND$ ( -- ptr u8 n ) S-KIND SLOT@ ;
: FAMILY$ ( -- ptr u8 n ) S-FAMILY SLOT@ ;
: TOKEN$ ( -- ptr u8 n ) S-TOKEN SLOT@ ;
: REASON$ ( n -- ptr u8 n ) PICK-REASON ;

7181 constant E-REASON-CAP  \ every reason-table row is taken: widen REASON-CAP
7182 constant E-REASON-DUP  \ the code already has a reason

\ EXPLAIN ( reason code -- ): the code's owner calls it once, at load, reading
\ its own constant.
: EXPLAIN ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   code REASON-ROW REASONS @ < IF E-REASON-DUP throw THEN
   REASONS @ REASON-CAP < 0= IF E-REASON-CAP throw THEN
   code REASONS @ cells REASON-CODES + !
   a u SLOTS REASONS @ + SLOT!
   1 REASONS +! ;

\ The last declaration's packet slots back to zero at a file boundary or image
\ capture (generated-declaration-protection.f SNAPSHOT-RESET), so a cached load
\ and a fresh one capture the same bytes. The reason rows past SLOTS are load
\ state and stay.
: SNAPSHOT-RESET ( -- )
   SLOTS SPAN-CAP * 0 ?do 0 SPAN-BUF i + c! loop
   SLOTS 0 ?do 0 i cells SPAN-LEN + ! loop
   NO-CODE ARMED ! ;

private

\ The codes of the owners loaded before this file: sumtype.f's grammar codes,
\ type-family.f's name and field-record codes, and the declaration
\ transaction's.  A post-hook checked body cannot name the pre-hook constants
\ on a from-source build, but a top-level read can.
s" malformed declaration" E-TDECL-SYNTAX EXPLAIN
s" invalid type parameter header" E-TDECL-ARITY EXPLAIN
s" unknown payload type" E-TDECL-PAYLOAD EXPLAIN
s" unknown layout policy" E-TDECL-POLICY EXPLAIN
s" invalid layout policy for recursive sum" E-TDECL-RECURSIVE EXPLAIN
s" declaration too long" E-TDECL-CAP EXPLAIN
\ E-TDECL-DERIVE reaching the table (rather than an armed front-end reason)
\ means the derive requirement failed later, in the constructor participant's
\ payload role and equality checks, not that the feature token was unknown.
s" a payload type or role has no derived equality" E-TDECL-DERIVE EXPLAIN
\ Both front ends arm "reserved name" at their own name gates, so E-TDECL-NAME
\ reaching the table was raised by a deeper owner: the variant-name gate (a
\ reserved tail or a family-name collision) or the constructor collide check (a
\ variant spelled like a word the DERIVE clause generates). The text has to be
\ true of both.
s" name is reserved or already taken" E-TDECL-NAME EXPLAIN
s" name must be a lowercase tail" E-TFAM-CASE EXPLAIN
s" duplicate name in this package" E-TFAM-DUP EXPLAIN
s" constructor package name exceeds 255 bytes" E-PACKAGE-NAME-CAP EXPLAIN
s" invalid or uncommitted field id" E-PF-ID EXPLAIN
s" stale or out-of-order field transaction" E-PF-TX EXPLAIN
s" invalid field owner family or variant" E-PF-OWNER EXPLAIN
s" reserved field name" E-PF-NAME EXPLAIN
s" malformed or owner-incompatible field schema" E-PF-SCHEMA EXPLAIN
s" invalid field layout metadata" E-PF-LAYOUT EXPLAIN
s" undefined field flag bits" E-PF-FLAGS EXPLAIN
s" a declaration is already open" DECLARATION-TRANSACTION:E-ACTIVE EXPLAIN
s" declaration checker frame mismatch" DECLARATION-TRANSACTION:E-FRAME EXPLAIN

;package

\ DECL-REPLAY: one installed token stream plus one mode flag.
\ tools/check-core.f's nominal pass and src/habu/verify-source.f register
\ declarations they already lexed: a front end reads the next token here instead
\ of `parse-name`, and the constructor generators read RP-ACTIVE? to define no
\ word. One state carries both facts, so no caller can ask for a replay that
\ generates or a live declaration that does not. The front ends' grammar,
\ registry calls and packets are unchanged, so a replay registers the same rows.
\ The stream is HEAD (the family name the callers lexed first) and BODY (the
\ space-separated rest). A zero-length HEAD reaches the front end's "missing
\ name" gate instead of promoting the first body token.
\ Every tail carries an `RP-` stem: test/engine-error-package.f corrupts the
\ baked `checker-package` name, package scope goes, and every package-local tail
\ becomes a bare global; an `OPEN` here collided with DECL-REJECT's and the
\ patched engine exited 78 instead of the pinned 70.
package DECL-REPLAY

\ Re-entry is refused with a code the transaction can roll back; production
\ callers never nest, since a replayed body is never evaluated.
7177 constant E-REPLAY-BUSY   \ a replayed declaration is already open
s" a replayed declaration is already open" E-REPLAY-BUSY DECL-REJECT:EXPLAIN

32 constant RP-SPACE
9 constant RP-TAB
10 constant RP-LF
13 constant RP-CR

variable RP-OPEN?      \ a replay stream is installed (0 = tokens come from live input)
variable RP-HEAD?      \ the leading (family name) token has not been read yet
variable RP-HEAD-U     PTR-VARIABLE RP-HEAD-A
variable RP-BODY-U     PTR-VARIABLE RP-BODY-A
variable RP-SCAN-I     \ scan cursor within the body buffer

: RP-HEAD! ( ptr u8 n -- ) RP-HEAD-U ! RP-HEAD-A ! ;
: RP-HEAD@ ( -- ptr u8 n ) RP-HEAD-A @ RP-HEAD-U @ ;
: RP-BODY! ( ptr u8 n -- ) RP-BODY-U ! RP-BODY-A ! ;
: RP-BODY@ ( -- ptr u8 n ) RP-BODY-A @ RP-BODY-U @ ;

: RP-SEP? ( n -- bool ) {: c:n :}
   c RP-SPACE = c RP-TAB = or c RP-LF = or c RP-CR = or ;

: RP-SKIP ( ptr u8 n -- n ) {: a:ptr u:n :}   \ first non-separator offset at/after the cursor
   RP-SCAN-I @
   BEGIN dup u < WHILE
      dup a + c@ RP-SEP? 0= IF EXIT THEN
      1 +
   REPEAT ;

: RP-TOKEN-END ( ptr u8 n n -- n ) {: a:ptr u:n s:n :}   \ offset one past the token starting at s
   s
   BEGIN dup u < WHILE
      dup a + c@ RP-SEP? IF EXIT THEN
      1 +
   REPEAT ;

: RP-BODY-NEXT ( -- ptr u8 n )         \ next body token; zero length once the buffer is spent
   RP-BODY@ {: a:ptr u:n :}
   a u RP-SKIP {: s:n :}
   a u s RP-TOKEN-END {: e:n :}
   e RP-SCAN-I !
   a s +  e s - ;

public

: RP-ACTIVE? ( -- bool ) RP-OPEN? @ 0 <> ;

\ RP-CLAIM ( name body -- ): install one declaration's lexed token stream. It
\ never copies and cannot see a token already dropped, so a caller's buffer
\ builder must raise when the declaration does not fit, never truncate
\ (verify-source.f BODY-APPEND throws E-VS-BODY-CAP, check-core.f CHK-VREC-ROOM
\ E-FS-CAPACITY). Comments pass through raw: the live keyword reads `\` or `(`
\ as an ordinary token and refuses it, so replay must too.
: RP-CLAIM ( ptr u8 n ptr u8 n -- )
   RP-ACTIVE? IF 2drop 2drop E-REPLAY-BUSY throw THEN
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
: RP-NEXT ( -- ptr u8 n )
   RP-HEAD? @ IF 0 RP-HEAD? ! RP-HEAD@ EXIT THEN
   RP-BODY-NEXT ;

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
   DECL-MARK PREPARE 0= IF DECLARATION-TRANSACTION:E-FRAME throw THEN ;

: PART-ROLLBACK ( -- ) DECL-MARK ROLLBACK ;

\ Total: RELEASE is throw-free by contract (src/core/checker.f RELEASE).
: PART-RELEASE ( -- ) RELEASE ;

private
;package

\ GENERATED-DECL-CTOR renders, evaluates, certifies and publishes one family's
\ checked FAMILY:VARIANT constructors inside the transaction. Commit order
\ (generated-declaration-protection.f COMMITS): checker | DECL-EVENT | here |
\ address surface | dictionary | protection. DECL-EVENT's commit has advanced
\ PF-COMMIT-N over this declaration's field rows (TYPE-FIELD-OWNER:COMMIT), the
\ watermark SUMV-NAMED-PAYLOAD? / SUMV-PAY-N read, so the generator runs from
\ the committed provider (TDECL-SUMV-PROVIDER). A throw here rolls every
\ participant back; the dictionary savepoint predates the body, so a failure
\ mid-set publishes no constructor. The family comes from ARM, never an ambient
\ register.
package GENERATED-DECL-CTOR

7176 constant E-CTOR-ARM   \ armed family is not a public ENUM family with variants
s" family cannot own generated constructors" E-CTOR-ARM DECL-REJECT:EXPLAIN

-1 constant NO-FAMILY

variable ARMED   NO-FAMILY ARMED !   \ the family this declaration armed

: ARMED-FAM ( -- n ) ARMED @ ;
: DISARM ( -- ) NO-FAMILY ARMED ! ;

\ The one gate: a family owns generated constructors when it is public (a
\ private family exports nothing), has a variant, and is a sum or an enum, the
\ two ENUM front-end kinds: the compact payloadless mode publishes constructors
\ as sumtype.f's definer does. A product is excluded: structure-make.f generates
\ its own make/unmake.
: GEN-OK? ( n -- bool ) {: fam:n :}
   fam TFAM-PUBLIC? 0= IF 0 0= 0= EXIT THEN
   fam TFAM-VAR-COUNT@ 0 <= IF 0 0= 0= EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or ;

\ An existence check on the registry, not a name lookup: planning records the
\ constructor symbol on each variant row, so a non-zero symbol on the first row
\ means the words are live. Kept out of GEN-OK?, which ED-CLOSE and OWNS? read
\ for kind alone. sumtype.f TDPLAN-NAME+ answers a second plan row with
\ E-TDECL-NAME, so an already-generated family is refused here by a named
\ throw; production never trips it, test/enum-decl-suite.f §20g drives it.
: GENERATED? ( n -- bool ) {: fam:n :}
   fam TFAM-VAR-START@ SUMV-CTOR-SYM@ 0 <> ;

\ Order matters: GEN-OK? proves the variant range is non-empty, so the first
\ variant row GENERATED? reads exists.
: GEN-REQUIRE ( n -- ) {: fam:n :}
   fam GEN-OK? 0= IF E-CTOR-ARM throw THEN
   fam GENERATED? IF E-CTOR-ARM throw THEN ;

public

\ The pending-constructor authority queue is plan scratch shared with the
\ generator. Its only legal value at the start of a declaration is empty, so
\ SNAPSHOT establishes that value rather than copying one, and ROLLBACK restores
\ it after a failure that left entries queued.
: PART-SNAPSHOT ( -- )
   DISARM
   CTOR-PEND-CLEAR ;

\ A non-mutating re-proof of the gate ARM checked: a family made ineligible
\ since then fails before any word is rendered.
: PART-PREPARE ( -- )
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY <> IF fam GEN-REQUIRE THEN ;

\ Mirrors sumtype.f CHECKER-DEFSUM-BODY's close. Both role gates read committed
\ rows, so they cannot run in the body phase. Replay renders and checks the same
\ bodies to publish their effects and emits no runtime code: a call needs the
\ checked effect, not only the package stamp.
: PART-COMMIT ( -- )
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY = IF EXIT THEN
   fam GEN-REQUIRE
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: count:n :}
   fam vstart count TDECL-DERIVE-COLLIDE
   fam vstart count TDECL-DERIVE-REQUIRE
   fam vstart count TDECL-CTOR-PUBLISH
   DECL-REPLAY:RP-ACTIVE? IF fam TDECL-CTOR-REPLAY EXIT THEN
   TDECL-SUMV-PROVIDER fam TDECL-CTOR-WORDS-BODY drop ;

: PART-ROLLBACK ( -- )
   CTOR-PEND-CLEAR
   DISARM ;

\ Total: publication has already happened, and the only thing left is this
\ owner's one armed-family cell.
: PART-RELEASE ( -- )
   DISARM ;

\ Legal only inside an open declaration transaction and for a family that passes
\ the gate, so a refused kind fails at ARM, not three phases later.
: ARM ( n -- ) {: fam:n :}
   DECLARATION-TRANSACTION:ACTIVE? 0= IF E-CTOR-ARM throw THEN
   fam GEN-REQUIRE
   fam ARMED ! ;

: OWNS? ( n -- bool ) GEN-OK? ;

\ SUMTYPE and PRODUCT still generate their constructors in sumtype.f, outside
\ this participant. A tool that registered one from tokens it had lexed
\ (src/habu/verify-source.f) replays that family's constructors here: their
\ checked effects, for the family sumtype.f last announced, and no word.
: REPLAY-LEGACY ( -- ) TDECL-CTOR-WORDS-REPLAY ;

private

get-current prot-wid-add

;package

\ DECL-FRONT: what both front ends (structure-decl.f, enum-decl.f) share around
\ their grammar: reject codes, the token source with one-token pushback and a
\ lookahead span, per-declaration parse cells, a replay's field-name window, the
\ guarded run with resynchronization, and the replay entries. One set of cells
\ serves both: declarations never nest (the runner refuses a second with
\ E-ACTIVE, DECL-REPLAY a second stream with E-REPLAY-BUSY). Every borrowed span
\ is forgotten as the declaration ends (RESET and MISS-CLEAR in DRIVE,
\ REPLAY-END for an E-UNRESOLVED replay's span).
\ A swallowed reject (multi-error load) leaves the tokens up to the terminator
\ ahead of the interpreter, which would run them: `ENUM m7bad red red blue ;ENUM
\ NEWTYPE m7cont<a>` died on `E-UNDEFINED: blue`. DRIVE skips to the terminator,
\ except after SEEN-END (that would eat the next declaration) or at end of input.
\ The skip reads through RAW, not NEXT, so the packet keeps naming the token.
\ A replay claims the stream before the packet opens: a failed claim belongs to
\ the caller, keeps the live declaration's packet and stream, and propagates with
\ no packet. Only a successful claim reaches REPLAY-END, which releases the
\ stream on both exits.
\ QUOT-ROLLBACK after a reject is unconditional: it zeroes sumtype.f TDV-QUOT-N
\ and TDV-QUOT-MISS, which only a quotation field dirties.
package DECL-FRONT

public

\ Local names for the shared declaration codes, read at top level from their
\ owners (sumtype.f E-TDECL-*, type-family.f E-TFAM-*): a post-hook checked body
\ cannot name a pre-hook constant on a from-source build. Each code keeps one
\ numeric owner (tools/error-code-lint.f). E-PF-NAME, E-TFAM-DUP, E-TFAM-CASE and
\ E-PF-SCHEMA, raised by TFAM-DECL, SUMV-ADD and the field-event path, pass
\ through unchanged.
E-TDECL-SYNTAX constant E-SYNTAX    \ malformed: missing name/terminator, token out of place
E-TDECL-ARITY constant E-ARITY      \ malformed type parameter header
E-TDECL-PAYLOAD constant E-PAYLOAD  \ unresolved / unknown field type
E-TDECL-NAME constant E-NAME        \ reserved or colliding family name
E-TDECL-POLICY constant E-POLICY    \ unknown or not-yet-supported layout policy
E-TDECL-DERIVE constant E-DERIVE    \ unknown or not-yet-supported derive feature
E-TDECL-CAP constant E-CAP          \ a family, variant or field name longer than NAME-MAX
E-TDECL-UNCHECKED constant E-UNCHECKED \ a constructor-generating declaration while checking is suspended
E-TFAM-CASE constant E-CASE         \ family name is not a lowercase canonical tail
E-TFAM-DUP constant E-DUP           \ duplicate family, variant or field tail, raised by
                                    \ TFAM-DECL / SUMV-ADD / the field-event path; named
                                    \ here only so a reason can be armed before those calls
7192 constant E-UNRESOLVED          \ private replay outcome after rollback

110 constant ASCII-N
102 constant ASCII-F
114 constant ASCII-R

\ The longest family, variant or field name, which every spelling derived from
\ it has room for (src/core/type-family.f TF-NAME-MAX, read here at top level).
TF-NAME-MAX constant NAME-MAX

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

\ The single-letter con codes are src/core/checker.f constants created before it
\ claims the source, so a from-source boot gives them no row: they are read
\ here at top level, as the reject codes are.
CC-N constant CON-N          \ single-letter n : signed cell
CC-BOOL constant CON-BOOL    \ single-letter f : boolean/flag
CC-R constant CON-R          \ single-letter r : real/float

variable FIELD-MISS          \ the field under resolution named a type that may still arrive
variable SEEN-END            \ this declaration's terminator has been consumed
variable OUTCOME-NAMES       \ a replay wants the field-name window checked (REPLAY-OUTCOME)

private

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span in a
\ declared pointer cell.
variable PEND-U   PTR-VARIABLE PEND-A
: PEND! ( ptr u8 n -- ) PEND-U ! PEND-A ! ;
: PEND@ ( -- ptr u8 n ) PEND-A @ PEND-U @ ;
PTR-VARIABLE HEAD-A   variable HEAD-U   variable HEAD-I   \ a span read ahead, re-read token by token before the source
: HEAD-CLEAR ( -- ) NULL-PTR HEAD-A ! 0 HEAD-U ! 0 HEAD-I ! ;

PTR-VARIABLE TYPE-A   variable TYPE-U     \ the field type token under resolution
PTR-VARIABLE MISS-A   variable MISS-U     \ the first unresolved type span, kept for REPLAY-END
$2000 constant SEEN-CAP                   \ the field-name window a replay checks
create SEEN SEEN-CAP allot
variable SEEN-U
variable SEEN-I

: MISS-CLEAR ( -- ) NULL-PTR MISS-A ! 0 MISS-U ! ;

public

: TYPE! ( ptr u8 n -- ) TYPE-U ! TYPE-A ! ;
: TYPE@ ( -- ptr u8 n ) TYPE-A @ TYPE-U @ ;

\ MISSING! ( span -- ) : the type under resolution may still arrive. The first
\ such span is what a replay reports (REPLAY-OUTCOME); later ones are dropped.
: MISSING! ( ptr u8 n -- )
   -1 FIELD-MISS !
   MISS-U @ 0= IF MISS-U ! MISS-A ! ELSE 2drop THEN ;
: MISSING? ( -- bool ) MISS-U @ 0 <> ;

: SEEN-CLEAR ( -- ) 0 SEEN-U ! ;

\ HEAD! ( span -- ) : retain a span the front end read ahead of its grammar;
\ RAW hands it back token by token before reading the source again (ENUM's
\ mode selection reads to the first variant before registering the family).
: HEAD! ( ptr u8 n -- ) HEAD-U ! HEAD-A !  0 HEAD-I ! ;

\ Tokens come from the live input or a replay stream (DECL-REPLAY); RAW checks
\ the pushback and the lookahead span first, so lookahead is the same on both.
: SOURCE-NEXT ( -- ptr u8 n )
   DECL-REPLAY:RP-ACTIVE? IF DECL-REPLAY:RP-NEXT EXIT THEN
   parse-name ;

private

: HEAD-NEXT ( -- ptr u8 n )
   HEAD-A @ {: a:ptr :}
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

: RAW ( -- ptr u8 n )                  \ next body token: pushback, lookahead, source
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   HEAD-U @ 0 <> IF HEAD-NEXT EXIT THEN
   SOURCE-NEXT ;

public

: NEXT ( -- ptr u8 n )                 \ next body token, recorded for diagnostics
   RAW 2dup DECL-REJECT:TOKEN! ;
: UNGET ( ptr u8 n -- ) PEND! ;

: NAME-LONG? ( ptr u8 n -- bool ) nip NAME-MAX > ;

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop s" missing layout policy name" E-POLICY DECL-REJECT:REJECT throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop TL-STACK-CELL-TAG EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop TL-PACKED-TAG EXIT THEN
   2drop s" unknown layout policy" E-POLICY DECL-REJECT:REJECT throw ;

: VIS ( -- n )                          \ declaration visibility (public at top level)
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF CHECKER-VIS-PUBLIC EXIT THEN CHECKER-AUTH-PACKAGE-MODE@ ;

private

: SEEN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   0 SEEN-I !
   BEGIN SEEN-I @ SEEN-U @ < WHILE
      SEEN SEEN-I @ + c@ {: len:n :}
      len u = IF
         SEEN BYTE-VIEW SEEN-I @ + 1+ len a u CORE-STR= IF YES EXIT THEN
      THEN
      SEEN-I @ len 1+ + SEEN-I !
   REPEAT NO ;

public

\ NAME-CHECK ( name reason -- ) : a replay's field-name gate. The name is
\ validated, refused as a duplicate of one already in this window, and recorded;
\ `reason` is the front end's text for a window overflow.
: NAME-CHECK ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n wa:ptr wu:n :}
   a u DECL-REJECT:TOKEN!
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   a u PF-NAME-REQUIRE
   a u SEEN? IF E-DUP throw THEN
   SEEN-U @ u + 1+ SEEN-CAP > IF wa wu E-CAP DECL-REJECT:REJECT throw THEN
   u SEEN SEEN-U @ + c!
   0 SEEN-I !
   BEGIN SEEN-I @ u < WHILE
      a SEEN-I @ + c@
      SEEN SEEN-U @ + 1+ SEEN-I @ + c!
      SEEN-I @ 1+ SEEN-I !
   REPEAT
   SEEN-U @ u + 1+ SEEN-U ! ;

private

: RESET ( -- )                          \ base state, on both exits of DRIVE and at load
   NULL-PTR 0 PEND!  HEAD-CLEAR  NULL-PTR 0 TYPE!
   0 SEEN-END !  0 FIELD-MISS !  SEEN-CLEAR ;

: SKIP-BODY ( ptr u8 n -- )             \ to the terminator, or end of input
   {: ta:ptr tu:n :}
   BEGIN
      RAW dup 0= IF 2drop EXIT THEN
      2dup ta tu CORE-STR=CI IF 2drop EXIT THEN
      2drop
   AGAIN ;
: RESYNC ( ptr u8 n -- )
   SEEN-END @ IF 2drop EXIT THEN
   MULTI-ERR? 0= IF 2drop EXIT THEN
   SKIP-BODY ;

\ DRIVE ( terminator body -- code ) : one declaration through the transaction,
\ resynchronized and reset on the way out; 0 or the reject code. The body
\ quotation rides the data stack through the catch (docs/forth.md, checked
\ catch): the quotation is stack-preserving, so the xt is dropped after it.
: DRIVE ( ptr u8 n [ -- ] -- n )
   {: ta:ptr tu:n body :}
   MISS-CLEAR RESET
   body [: dup TDECL-TXN-XT ;] catch {: rc:n :} drop
   rc 0= IF RESET 0 EXIT THEN
   QUOT-ROLLBACK
   ta tu RESYNC
   RESET
   rc E-UNRESOLVED <> IF MISS-CLEAR THEN
   rc ;

: REPLAY-END ( n -- ptr u8 n bool )    \ direct missing span after rollback
   DECL-REPLAY:RP-RELEASE
   dup E-UNRESOLVED = IF
      drop MISS-A @ MISS-U @
      0 DECL-REJECT:GUARD-CODE
      YES MISS-CLEAR EXIT THEN
   DECL-REJECT:GUARD-CODE
   NULL-PTR 0 NO ;

: REPLAY-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n [ -- ] bool -- ptr u8 n bool )
   {: na:ptr nu:n ba:ptr bu:n ka:ptr ku:n ta:ptr tu:n body names:bool :}
   na nu ba bu DECL-REPLAY:RP-CLAIM
   ka ku DECL-REJECT:OPEN
   names OUTCOME-NAMES !
   ta tu body DRIVE
   0 OUTCOME-NAMES !
   REPLAY-END ;

\ The body DRIVE runs in place of the front end's while checking is suspended
\ (sumtype.f TDECL-UNCHECKED$): it names the declaration and refuses it before
\ the front end registers anything, and DRIVE skips to the terminator when a
\ multi-error load swallows the reject. A replay defines no word and is not
\ refused.
: UNCHECKED ( -- )
   NEXT DECL-REJECT:FAMILY!
   TDECL-UNCHECKED$ E-UNCHECKED DECL-REJECT:REJECT throw ;

public

\ RUN ( kind terminator body -- ) : one live declaration.
: RUN ( ptr u8 n ptr u8 n [ -- ] -- )
   {: ka:ptr ku:n ta:ptr tu:n body :}
   ka ku DECL-REJECT:OPEN
   check@ 0= IF ta tu [: UNCHECKED ;] DRIVE DECL-REJECT:GUARD-CODE EXIT THEN
   ta tu body DRIVE DECL-REJECT:GUARD-CODE ;

\ REPLAY-OUTCOME ( name body-text kind terminator body -- span bool ) : register
\ a declaration from tokens a tool has already lexed, defining no word, and
\ answer whether a type it names may still arrive, and which.
: REPLAY-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n [ -- ] -- ptr u8 n bool )
   YES REPLAY-WITH ;
\ REPLAY ( name body-text kind terminator body -- ) : the same, raising
\ E-UNRESOLVED instead of answering.
: REPLAY ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n [ -- ] -- )
   NO REPLAY-WITH IF 2drop E-UNRESOLVED throw THEN 2drop ;

private

RESET MISS-CLEAR

;package

;using
;using
