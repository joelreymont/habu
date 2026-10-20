\ generated-declaration.f - declaration participants loaded before the runner.
\
\ This file holds the checker-frame participant and the constructor-generation
\ participant of the declaration transaction.  The runner that calls them in a
\ straight line, DECLARATION-TRANSACTION:RUN, is defined in
\ src/core/generated-declaration-protection.f once the last participant has
\ loaded.  The checker participant's release is total: by the time the runner
\ releases, publication has already happened and nothing may reject.
\
\ It also owns DECL-REJECT, the reject side of the same transaction (what a
\ declaration reports when RUN rolls it back), defined first, and DECL-FRONT,
\ the driver the declaration front ends wrap around the transaction, last.
\
\ ---------------------------------------------------------------------------
\ DECL-REJECT — the shared reject packet for the unified declaration front ends.
\
\ Why it exists.  The pre-unified NEWTYPE/SUMTYPE/ENUM/PRODUCT definers keep a
\ declaration context (sumtype.f TDK/TDN/TDT/TDW) and report it through
\ render.f's TDECL-DIAG before the named code propagates, so a bad pre-unified
\ declaration prints
\     habu: bad enum declaration 'colour': duplicate variant at 'red'
\ and, under --json-errors, the matching E-BAD-DECLARATION object.  The unified
\ front ends (STRUCTURE-DECL:SD-RUN, ENUM-DECL:ED-RUN) threw the same codes with
\ no message at all.  This package is the one surface both front ends raise
\ through, so both produce that line from the SAME renderer.
\
\ It deliberately owns no renderer.  RENDER forwards to render.f's TDECL-DIAG,
\ which is also what writes into the diagnostic capture buffer (RDIAG-APPEND).
\ That is how tools/check-core.f's CHK-DECL-CAPTURE / CHK-DECL-FLUSH pair
\ collects legacy declaration packets today, so a front-end packet reaches that
\ capture through the same channel, in both the prose and the JSON leg, with no
\ change to the check tool.
\
\ Scope of that claim, precisely.  What is proven here is the CHANNEL: the suites
\ arm the identical DIAG-JSON! / DIAG-BUFFER! pair the check tool arms and read
\ the rendered packet back out of DIAG-BUFFER$.  When this was written that was
\ all it could be, because check-core could not reach these front ends at all: it
\ drove the legacy CHECKER-DEFENUM and did not scan STRUCTURE.  The buffer-driven
\ registration entry below (DECL-REPLAY) closed that, and the check tool now
\ registers both kinds through the front ends, so the end-to-end leg is covered
\ by test/decl-replay-verify-source.f and tools/check-test.f as well.
\
\ The code is never laundered.  GUARD-CODE renders the packet and then rethrows
\ the code it was handed, unchanged, so every reject class keeps the exact value
\ the suites pin.
\
\ Why this file owns it.  The packet must be defined before its first consumer,
\ structure-decl.f, and it needs nothing from the declaration-event transaction
\ or the runner — only render.f, loaded far earlier in the checker prefix.
\ This is the earliest packaged file in the declaration layer, so the whole
\ layer below (decl-event.f, structure-make.f, both front ends) can arm reasons.
\
\ Reason selection.  A front end that detects the fault itself passes its own
\ reason text with REJECT.  A fault raised deeper — by the family registry, the
\ variant registry, the field record, or a transaction participant — arrives as
\ a bare code, so REASON falls back to a text table keyed by that code, which
\ each code's owner fills with EXPLAIN.  An armed reason is honoured only when
\ it was armed FOR the code that actually escaped; that is what stops a stale
\ reason from being printed against an unrelated failure.
\
\ Nesting.  STRUCTURE and ENUM are top-level, interpret-only declaration
\ keywords and never nest: one declaration is open at a time, so the packet is a
\ single frame.  OPEN clears every field, which is what keeps a previous
\ declaration's family name from leaking into the next declaration's diagnostic.
\ Clause nesting WITHIN one declaration (a FIELD inside a VARIANT inside an
\ ENUM) is not packet nesting: the family stays the declaration's family and
\ only the offending token moves.
\ ---------------------------------------------------------------------------
using TFAM
using TYPE-DECL

package DECL-REJECT

\ --- spans.  Four short byte slots hold the packet text, and one more per
\ reason-table row holds that row's text.  They are copies, not
\ borrowed spans: a declaration body spans several input lines, the engine
\ refills its input buffer per line when the source is a stream, and the family
\ name is read on the first line but rendered after a reject on a later one.  A
\ borrowed span would render whatever bytes later occupied that buffer.  Each
\ slot also keeps the span it was copied from, which a token that locates
\ renders instead (TOKEN-SPAN@), until GUARD-CODE ends the declaration.
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

\ SLOT! clamps to SPAN-CAP, so no caller can write past its slot regardless of
\ the span it was handed.
\
\ A capped span is MARKED, not silently shortened: the last MARK-LEN bytes of the
\ slot become "..." so the rendered packet says the value continued rather than
\ presenting a prefix as if it were the whole name.  Silent truncation would
\ report a different identifier than the source contains, which is strictly worse
\ than the legacy definer's borrowed span; the marker keeps the bounded copy
\ honest while still refusing to read memory the declaration no longer owns.
\
\ SRC! and SRC@ hold the span a slot was copied from; FORGET-SRC drops them all.
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

\ The offending token RENDER hands the writer, which locates a token by its
\ pointer: the span it was copied from when that lies in text a driver armed
\ for locating packets (checker.f DIAG>SRC), so the packet carries its file
\ position; else the copy, which has none. Those borrowed bytes are intact: an
\ armed text stays put while its declaration runs, as DECL-REPLAY borrows the
\ caller's buffers. A span in an input buffer the engine refills per line lies
\ in no armed text, so its stale bytes are never read. The borrowed span is
\ whole: SPAN-CAP and its marker bound only the copy, so a located token renders
\ in full and names the same bytes as its byte range.
: TOKEN-SPAN@ ( -- ptr u8 n )
   S-TOKEN SRC@
   {: a:ptr u:n :}
   a DIAG>SRC nip IF a u EXIT THEN
   S-TOKEN SLOT@ ;

\ --- the reason table: what a reject of a deeper owner's code reports when no
\ front end armed a reason for it.  Row r is a code in REASON-CODES and its text
\ in slot SLOTS + r.  Each code's owner explains it with EXPLAIN, reading its own
\ constant by name, so a renumbered code keeps its text: the owners loaded
\ before this file are explained at the end of this package, each later one
\ beside its constants.  The table is a closed world: REASON-CAP is the number
\ of codes the engine's owners explain, and EXPLAIN refuses a code past it, or
\ one explained twice, by name.
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

\ An armed reason describes ONE code.  If a different code escaped, the arming
\ was for a fault that did not happen and the table answers instead.
: PICK-REASON ( n -- ptr u8 n ) {: code:n :}
   ARMED @ code = IF S-REASON SLOT@ EXIT THEN
   code CODE-REASON ;

\ render.f's declaration-diagnostic writer is the SAME producer the legacy
\ definers report through, so prose, JSON, and the check tool's packet capture
\ all behave identically for a unified declaration.
: RENDER ( n -- ) {: code:n :}
   S-KIND SLOT@  S-FAMILY SLOT@  TOKEN-SPAN@  code PICK-REASON  TDECL-DIAG ;

public

\ OPEN ( kind -- ) : start one declaration's packet.  Every field is cleared, so
\ nothing from the previous declaration can be reported against this one.
: OPEN ( ptr u8 n -- )
   S-KIND SLOT!
   NOTHING$ S-FAMILY SLOT!
   NOTHING$ S-TOKEN SLOT!
   FALLBACK$ S-REASON SLOT!
   NO-CODE ARMED ! ;

\ The declared family name, as soon as the front end has validated it.
: FAMILY! ( ptr u8 n -- ) S-FAMILY SLOT! ;

\ The token the front end is currently acting on.  Front ends record every body
\ token here, so a fault raised inside a registry or a participant still names
\ the token that provoked it.
\
\ Recording a new token also disarms any armed reason.  An arming describes a
\ fault expected AT one token; once the front end has moved on, that reason can
\ no longer explain a failure, so it is retired by construction rather than
\ left to be printed against something unrelated later in the declaration.
: TOKEN! ( ptr u8 n -- )
   S-TOKEN SLOT!
   NO-CODE ARMED ! ;

\ Point the offending token back at the family name.  A close-stage fault is a
\ property of the whole declaration, not of the terminator token that happened
\ to be read last, and the legacy definers anchor those on the family too.
: AT-FAMILY ( -- )
   S-FAMILY SLOT@ TOKEN!
   S-FAMILY SRC@ S-TOKEN SRC! ;

\ EXPECT ( reason code -- ) : arm the reason for a fault a deeper owner may
\ raise, immediately before the call that can raise it.
: EXPECT ( ptr u8 n n -- ) {: wa:ptr wu:n code:n :}
   wa wu S-REASON SLOT!
   code ARMED ! ;

\ REJECT ( reason code -- code ) : the front end's own reject, for a fault at the
\ token it has already recorded.  It ANSWERS the code instead of throwing it, so
\ the call site spells `throw` itself: the reject reads as one line, and the
\ thrown value is visible in the front-end source at every reject site rather
\ than hidden behind a helper that could substitute a different one.
: REJECT ( ptr u8 n n -- n ) {: wa:ptr wu:n code:n :}
   wa wu code EXPECT
   code ;

\ GUARD-CODE ( code -- ) : end one declaration DECL-FRONT ran.  0 forgets the
\ borrowed spans; a reject is rendered through the shared declaration diagnostic
\ and then rethrown with its exact code (CHECKER-REFUSE), so the transaction's
\ rollback, the caller's error handling, and every pinned reject value are
\ unchanged; only the missing message is added.
\
\ Under a multi-error load (`--all-errors`) the reject is counted and the load
\ continues instead — the declaration is already rolled back by the time this
\ runs, so there is nothing half-registered to carry forward, and the point of
\ that mode is to report every fault in a file rather than the first one. This is
\ exactly what sumtype.f's TDECL-RUN does for the legacy definers; the branch was
\ latent here until the global ENUM keyword became a front end, because the only
\ multi-error consumer wrapped declarations in a catch of its own.
\
\ Swallowing a reject only continues the LOAD; it does not by itself continue the
\ INPUT. The legacy definers collected a whole declaration before parsing it, so
\ by the time TDECL-RUN swallowed anything the stream was already past the
\ terminator. A front end reads the stream as it parses, so it stops wherever the
\ fault was and the interpreter would meet the rest of the declaration as if it
\ were code. Resynchronizing needs the terminator, which only the front end
\ knows, so DECL-FRONT's driver skips to the terminator the front end hands it
\ before this reports, when MULTI-ERR? says this load will swallow.
\
\ The spans the declaration borrowed are forgotten on both exits: they are good
\ only while it runs, and one left behind would be read stale or baked into an
\ image with its build-time address, so that successive engine generations
\ differ.
\
\ Under `--all-errors` (the checker's multi-error load state) a rejected
\ definition is counted and the load continues instead of stopping at the first
\ fault, as the legacy definers' reporter TDECL-RUN answers. Otherwise the
\ reject is rethrown with its own code; the checker publishes rendered refusals
\ before throwing, so an uncaught load exits as a refusal.
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

\ EXPLAIN ( reason code -- ) : the text a reject of `code` reports when no front
\ end armed a reason for it.  The code's owner calls it once, at load, reading
\ its own constant (the reason table above).
: EXPLAIN ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   code REASON-ROW REASONS @ < IF E-REASON-DUP throw THEN
   REASONS @ REASON-CAP < 0= IF E-REASON-CAP throw THEN
   code REASONS @ cells REASON-CODES + !
   a u SLOTS REASONS @ + SLOT!
   1 REASONS +! ;

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

\ ---------------------------------------------------------------------------
\ DECL-REPLAY — where a declaration's tokens come from, and whether that
\ declaration is allowed to define words.
\
\ Why it exists.  Two tools need to register a declaration they have ALREADY
\ lexed: tools/check-core.f's nominal pass and src/habu/verify-source.f both
\ scan a file token by token and must register each family they meet, so that a
\ later signature in the same file can resolve it.  Neither is interpreting the
\ file, so neither can let a front end call `parse-name`, and neither wants the
\ constructor words a real declaration defines — check is reading source, not
\ building a program.  They used to drive sumtype.f's CHECKER-DEFENUM, the legacy
\ metadata-only registration, which the type-DSL cutover has since deleted; and
\ STRUCTURE had no such entry at all, which is why the check tool could not
\ check any file that declared one (a STRUCTURE family stayed unregistered, so
\ the next declaration that named it as a payload type rejected with "unknown
\ payload type").
\
\ What it is.  One installed token stream plus one mode flag, read by both
\ front ends and by both constructor generators.  A front end asks it for the
\ next token instead of calling `parse-name`; the generators ask it whether
\ this declaration may define words.  The two facts are deliberately ONE piece
\ of state with one name: a replayed declaration is exactly the one that must
\ not define words, so the pair cannot drift apart and no caller can ask for a
\ replay that generates, or a live declaration that does not.
\
\ The stream is in two segments because that is the shape both callers already
\ have: they lexed the family name as its own token before they knew what kind
\ of declaration it was, then buffered the rest of the body.  HEAD is that name
\ token, BODY the buffered remainder (space-separated, which is what both
\ callers' buffer builders emit).  Handing a zero-length HEAD through unchanged
\ matters: it reaches the front end's own "missing name" gate rather than
\ silently promoting the first body token to the family name.
\
\ What it is NOT.  It is not a second parser.  The front ends' grammar loops,
\ validation, registry calls, and reject packets are entirely unchanged; only
\ the source of their tokens moves.  That is the whole point — a replayed
\ declaration must register the same family identity, tags, variant and field
\ rows, and policy/derive metadata as a real one, because the checker resolves
\ types through exactly those rows.
\
\ Why it lives in THIS file rather than its own. It must be defined before its
\ first consumer and visible both to GENERATED-DECL-CTOR below and to
\ structure-make.f.
\
\ Why every tail carries an `RP-` stem.  The neighbouring declaration-layer
\ packages do the same (`SD-` in structure-decl.f, `ED-` in enum-decl.f, `SM-`
\ in structure-make.f, `DEV-` in decl-event.f), and it is not decoration:
\ test/engine-error-package.f corrupts the baked `checker-package` name to prove
\ the engine still fails closed with no checker, and in that image package scope
\ is gone, so every package-local tail is a bare global.  A tail spelled like
\ another prelude package's tail then duplicates instead — measured, an `OPEN`
\ here collided with DECL-REJECT's `OPEN` and the patched engine exited 78
\ (E-DUP-DEFINITION, silently, during boot) instead of the pinned 70 with its
\ "undefined word 'START'" diagnostic.  The stem keeps the fail-closed ORDER
\ that test pins, and `CLAIM`/`RELEASE` say what they do better than
\ `OPEN`/`CLOSE` anyway: the stream is taken from a caller and handed back.
\ ---------------------------------------------------------------------------
package DECL-REPLAY

\ Re-entry is refused with a named code the transaction can roll back, rather
\ than silently retargeting a live stream.  The production callers never nest —
\ a replayed body is never evaluated, so it cannot re-enter a front end — but
\ the guard makes that a proven property instead of an assumed one.
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

\ The two borrowed spans, each held in a declared pointer cell; the token scan
\ below runs entirely on the typed span these answer.
: RP-HEAD! ( ptr u8 n -- ) RP-HEAD-U ! RP-HEAD-A ! ;
: RP-HEAD@ ( -- ptr u8 n ) RP-HEAD-A @ RP-HEAD-U @ ;
: RP-BODY! ( ptr u8 n -- ) RP-BODY-U ! RP-BODY-A ! ;
: RP-BODY@ ( -- ptr u8 n ) RP-BODY-A @ RP-BODY-U @ ;

: RP-SEP? ( n -- bool ) {: c:n :}      \ a token separator byte
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

\ Is a replayed declaration open?  The front ends read this to choose their
\ token source; the constructor generators read it to decide whether this
\ declaration publishes words.
: RP-ACTIVE? ( -- bool ) RP-OPEN? @ 0 <> ;

\ RP-CLAIM ( name body -- ) : install one declaration's already-lexed token stream.
\
\ CAPACITY IS THE CALLER'S CONTRACT. This package holds two spans and never
\ copies, so it imposes no length bound of its own and cannot detect one that was
\ already violated: a body buffer that dropped a token arrives indistinguishable
\ from a complete one, and the grammar loop parses it happily. The caller's
\ buffer builder must therefore RAISE when the declaration does not fit, never
\ truncate — src/habu/verify-source.f's BODY-APPEND throws E-VS-BODY-CAP and
\ tools/check-core.f's CHK-VREC-ROOM throws E-FS-CAPACITY. A builder that
\ silently shortens its input turns a too-long declaration into a well-formed and
\ WRONG one, and nothing downstream of this point can tell.
\
\ COMMENTS ARE NOT LAUNDERED, for the same reason. The engine reads a declaration
\ body with `parse-name`, so a `\` or `(` inside it is an ordinary token and a
\ syntax error. A capture arm that strips comments before buffering would let
\ replay accept source the live keyword refuses, so the arms feed their raw
\ declaration-window tokens through unchanged and the front end rejects at the
\ comment token exactly as it does live.
: RP-CLAIM ( ptr u8 n ptr u8 n -- )
   RP-ACTIVE? IF 2drop 2drop E-REPLAY-BUSY throw THEN
   RP-BODY!  RP-HEAD!
   0 RP-SCAN-I !
   -1 RP-HEAD? !
   -1 RP-OPEN? ! ;

\ RP-RELEASE ( -- ) : retire the stream and leave replay mode.  Callers close on both
\ the accepting and the rejecting path, so a rejected replay cannot leave the
\ next live declaration reading a spent buffer.
: RP-RELEASE ( -- )
   0 RP-OPEN? !  0 RP-HEAD? !
   NULL-PTR 0 RP-HEAD!
   NULL-PTR 0 RP-BODY!
   0 RP-SCAN-I ! ;

\ RP-NEXT ( -- token ) : the family name once, then the body tokens in order, then
\ zero-length forever — the same end-of-input signal `parse-name` gives the
\ front ends, so their "missing ;STRUCTURE" / "missing ;ENUM" gates fire
\ unchanged on a truncated stream.
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

\ The frame word this forwards to is already throw-free by contract; see the
\ RELEASE comment in src/core/checker.f.
: PART-RELEASE ( -- ) RELEASE ;

private
;package

\ ---------------------------------------------------------------------------
\ The constructor-generation participant.
\
\ It renders, evaluates, certifies and publishes one declared family's checked
\ FAMILY:VARIANT constructors from inside the declaration transaction. The order
\ is the whole design:
\
\   checker | DECL-EVENT | here | address surface | dictionary | protection
\
\ The runner commits in that order (generated-declaration-protection.f COMMITS),
\ so by the time this participant commits, DECL-EVENT's commit has already run
\ TYPE-FIELD-OWNER:COMMIT, which advances PF-COMMIT-N over this declaration's
\ field rows. That is exactly the watermark SUMV-NAMED-PAYLOAD? / SUMV-PAY-N
\ read, so the shared generator can be driven from the ordinary COMMITTED
\ provider (TDECL-SUMV-PROVIDER) and needs no provisional reader and no
\ live-token provider of its own.
\
\ Generation is fallible by design. Commit is reversible: a throw here rolls
\ every participant back in reverse order, and because the dictionary owner took
\ its savepoint before the body ran, every word this participant already
\ evaluated is truncated with the rest of the declaration. A failure mid-set
\ therefore publishes no constructor at all.
\
\ This participant reads no ambient last-registered-family register: the family
\ it acts on is armed by the declaration front end into the one cell owned here.
\ ---------------------------------------------------------------------------
package GENERATED-DECL-CTOR

7176 constant E-CTOR-ARM   \ armed family is not a public ENUM family with variants
s" family cannot own generated constructors" E-CTOR-ARM DECL-REJECT:EXPLAIN

-1 constant NO-FAMILY

variable ARMED   NO-FAMILY ARMED !   \ the family this declaration armed

: ARMED-FAM ( -- n ) ARMED @ ;
: DISARM ( -- ) NO-FAMILY ARMED ! ;

\ THE GATE, and the one place that spells it.
\
\ A family owns generated constructors when it is PUBLIC (a private family
\ exports nothing and TDECL-CTOR-PUBLISH would leave the constructor package
\ empty), has at least one variant, and is one of the two ENUM-front-end kinds.
\
\ Both kinds, deliberately. The global-token cutover's acceptance is that every
\ existing plain ENUM behaves identically through the new front end, and the
\ legacy sumtype.f definer already publishes constructors for a compact
\ payloadless enum (a declared `LGC:RED` resolves; the same family through
\ ENUM-DECL:ED-RUN was E-UNDEFINED before this participant). Gating on TK-SUM
\ alone would leave the compact mode a permanent parity gap the cutover could
\ never close. TK-PRODUCT is excluded because the STRUCTURE front end owns its
\ own make/unmake generation in structure-make.f; admitting it here would
\ generate a second, conflicting set.
: GEN-OK? ( n -- bool ) {: fam:n :}
   fam TFAM-PUBLIC? 0= IF 0 0= 0= EXIT THEN
   fam TFAM-VAR-COUNT@ 0 <= IF 0 0= 0= EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or ;

\ Has this family's set already been generated? Constructor planning records the
\ constructor symbol on each variant row as it renders, so a non-zero symbol on
\ the first row means the words are live. This is an EXISTENCE check on the
\ registry, deliberately not a name lookup, and it is kept out of GEN-OK? so that
\ predicate keeps its single meaning — which kinds own constructors — for
\ ED-CLOSE's gate and the public OWNS?.
\
\ It matters because sumtype.f's TDPLAN-NAME+ answers a second plan row for a
\ live constructor with E-TDECL-NAME, and a caller that arms an already-generated
\ family must be refused here, by a named throw the transaction can roll back,
\ rather than reaching the duplicate row. The production path never trips it —
\ ED-CLOSE arms a family
\ whose variant rows were created moments earlier — so this is a boundary guard,
\ and test/enum-decl-suite.f §20g drives it directly.
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

\ Non-mutating re-proof only. The front end proved this gate when it armed, in
\ the body phase; nothing between then and here may have made the family
\ ineligible, and if something did, the declaration fails before any word is
\ rendered rather than publishing a set nobody validated.
: PART-PREPARE ( -- )
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY <> IF fam GEN-REQUIRE THEN ;

\ The work. Mirrors the legacy sum definer's close sequence (sumtype.f
\ CHECKER-DEFSUM-BODY): reject a variant spelled like a derived word, prove every
\ payload role is derivably comparable, stamp the constructor package on each
\ variant row, then render/evaluate/certify/seal the whole set through the shared
\ generator. Both role gates read committed rows, which is why they work here and
\ could not work in the body phase.
\
\ Replay renders and checks the same constructor bodies, publishing their
\ effects for later source definitions. It emits no runtime code. The package
\ stamp alone is insufficient: a call needs the checked constructor effect.
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

\ The declaration front end names the family whose constructors this transaction
\ owns. Legal only inside an open declaration transaction, and only for a family
\ that passes the gate, so a caller cannot arm a kind this participant refuses to
\ generate for and discover it three phases later.
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

\ ---------------------------------------------------------------------------
\ DECL-FRONT — the driver the declaration front ends wrap.
\
\ A front end (structure-decl.f, enum-decl.f) owns only its grammar. Everything
\ around the grammar is the same for every front end and lives here once: the
\ reject codes, the token source with its one-token pushback and its lookahead
\ span, the per-declaration parse cells, the field-name window a replay checks,
\ the guarded transaction run with its resynchronization, and the replay
\ entries. A front end passes what varies as arguments: its kind word for the
\ packet, its terminator for resynchronization, and its body quotation, which
\ resets the front end's own grammar scalars and runs its grammar.
\
\ One set of cells serves every front end: STRUCTURE and ENUM are top-level
\ interpret-only keywords that never nest (DECL-REJECT "Nesting" above; the
\ runner refuses a second open declaration with E-ACTIVE and DECL-REPLAY a
\ second stream with E-REPLAY-BUSY). Every source span a front end borrows —
\ the pushback, ENUM's lookahead span, the type token under resolution, the
\ missing-type span — lives here and is forgotten as the declaration ends:
\ RESET and MISS-CLEAR in DRIVE, and REPLAY-END for the span an E-UNRESOLVED
\ replay answers. DECL-REJECT forgets its spans for the same reason: one left
\ behind would be read stale or baked into an image with its build-time
\ address, so engine generations differ.
\
\ Resynchronization matters only when the reject will be swallowed — a
\ multi-error load, where the interpreter carries on with whatever the stream
\ holds next. The legacy definers got this for free: they copied every token
\ through the terminator into a buffer before parsing any of it. A front end
\ reads as it parses and stops at the fault, so the tokens between the fault
\ and the terminator are still ahead of the interpreter, which would try to
\ execute them. Measured before the front ends resynchronized:
\ `ENUM m7bad red red blue ;ENUM NEWTYPE m7cont<a>` reported the duplicate
\ variant, counted it, then died on `E-UNDEFINED: blue`, and
\ `STRUCTURE m7sbad FIELD x n FIELD x n ;STRUCTURE NEWTYPE m7scont<a>` died on
\ `E-UNDEFINED: ;STRUCTURE`. The terminator is the declaration's own boundary,
\ so skipping to it consumes exactly what belongs to it. Two cases need no
\ skip and get none: a reject raised after the terminator was consumed
\ (SEEN-END — skipping then would eat the NEXT declaration), and a reject
\ raised because the input ended without one (the scan meets end of input).
\ The skip reads through RAW, not NEXT, so the packet keeps naming the token
\ that provoked the reject.
\
\ A reject is rendered AFTER the runner has rolled everything back, then
\ rethrown with its exact code (DECL-REJECT:GUARD-CODE), the order the legacy
\ definers use (sumtype.f TDECL-RUN: restore, report, rethrow). Live and
\ replayed declarations share DRIVE, so both report through the same renderer.
\
\ A replay claims the stream BEFORE the packet is opened. Claiming can fail —
\ another replay is already installed — and that failure belongs to the caller
\ that misused the entry, not to any declaration: it must not clear the packet
\ of the declaration that owns the live stream, and it must not close that
\ stream on the way out. So it propagates as its own code with no packet, and
\ only a claim that SUCCEEDED reaches REPLAY-END, which retires the stream on
\ BOTH exits: closing only on success would leave a rejected replay installed,
\ and the next live declaration would read its tokens from a spent buffer.
\
\ QUOT-ROLLBACK after a reject is unconditional: it zeroes the quotation-field
\ parser's scratch (sumtype.f TDV-QUOT-N / TDV-QUOT-MISS), which only a
\ quotation field dirties, and is a no-op for a body that never entered
\ PARSE-QUOT.
\ ---------------------------------------------------------------------------
package DECL-FRONT

public

\ --- named reject codes: local names for the shared declaration codes, read at
\ load time from their owners, sumtype.f E-TDECL-* and type-family.f E-TFAM-*.
\ A post-hook checked body cannot name a pre-hook constant on a from-source
\ build, but a top-level read can, and the alias leaves each code one numeric
\ owner (tools/error-code-lint.f). The field record's E-PF-NAME / E-TFAM-DUP /
\ E-TFAM-CASE / E-PF-SCHEMA name+schema gate codes are raised by TFAM-DECL,
\ SUMV-ADD and the field-event path and pass through unchanged.
E-TDECL-SYNTAX constant E-SYNTAX    \ malformed: missing name/terminator, token out of place
E-TDECL-ARITY constant E-ARITY      \ malformed type parameter header
E-TDECL-PAYLOAD constant E-PAYLOAD  \ unresolved / unknown field type
E-TDECL-NAME constant E-NAME        \ reserved or colliding family name
E-TDECL-POLICY constant E-POLICY    \ unknown or not-yet-supported layout policy
E-TDECL-DERIVE constant E-DERIVE    \ unknown or not-yet-supported derive feature
E-TDECL-CAP constant E-CAP          \ a family, variant or field name longer than NAME-MAX
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

\ Tokens come from the live input source, or — when a tool is replaying a
\ declaration it has already lexed (REPLAY below) — from that tool's token
\ stream. The pushback and the lookahead span are checked first either way
\ (RAW), so lookahead behaves identically on both sources.
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

\ Every body token is recorded as the packet's offending token as it is read, so
\ a reject raised inside the family registry, the variant registry, the field
\ record, or a transaction participant still names the token that provoked it
\ without those owners knowing anything about the packet.
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

public

\ RUN ( kind terminator body -- ) : one live declaration.
: RUN ( ptr u8 n ptr u8 n [ -- ] -- )
   {: ka:ptr ku:n ta:ptr tu:n body :}
   ka ku DECL-REJECT:OPEN
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
