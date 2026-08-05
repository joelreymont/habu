\ generated-declaration.f - production generated-declaration transaction.
\
\ This file owns one sealed DECLARATION-TRANSACTION instance.  The checker is
\ the first participant; DECL-EVENT enrolls later in the checked declaration
\ layer; the native dictionary and protection owners enroll after xref; and the
\ protection owner seals the participant set last. RUN refuses to start before
\ that final seal.
\
\ The checker participant's release callback is total, like every other one: by
\ the time the coordinator releases, publication has already happened and no
\ participant is allowed to reject.
\
\ It also owns DECL-REJECT, the reject side of the same transaction: what a
\ declaration reports when RUN rolls it back.  That package is defined first,
\ because it depends on nothing else here.
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
\ The code is never laundered.  GUARD renders the packet and then rethrows the
\ code it caught, unchanged, so every reject class keeps the exact value the
\ suites pin.
\
\ Why this file owns it.  The packet must be defined before its first consumer,
\ structure-decl.f, and it needs nothing from the declaration-event transaction
\ or the coordinator — only render.f, loaded far earlier in the checker prefix.
\ This is the earliest packaged file in the declaration layer and the one that
\ owns GENERATED-DECL:RUN, the exact boundary both front ends wrap, so the
\ reject-reporting half of that boundary lives beside it and the whole layer
\ below (decl-event.f, structure-make.f, both front ends) can arm reasons.
\
\ Reason selection.  A front end that detects the fault itself passes its own
\ reason text with REJECT.  A fault raised deeper — by the family registry, the
\ variant registry, the field record, or a transaction participant — arrives as
\ a bare code, so REASON falls back to a text table keyed by that code.  An
\ armed reason is honoured only when it was armed FOR the code that actually
\ escaped; that is what stops a stale reason from being printed against an
\ unrelated failure.
\
\ Nesting.  STRUCTURE and ENUM are top-level, interpret-only declaration
\ keywords and never nest: one declaration is open at a time, so the packet is a
\ single frame.  OPEN clears every field, which is what keeps a previous
\ declaration's family name from leaking into the next declaration's diagnostic.
\ Clause nesting WITHIN one declaration (a FIELD inside a VARIANT inside an
\ ENUM) is not packet nesting: the family stays the declaration's family and
\ only the offending token moves.
\ ---------------------------------------------------------------------------
package DECL-REJECT

\ --- spans.  Four short byte slots hold the packet text.  They are copies, not
\ borrowed spans: a declaration body spans several input lines, the engine
\ refills its input buffer per line when the source is a stream, and the family
\ name is read on the first line but rendered after a reject on a later one.  A
\ borrowed span would render whatever bytes later occupied that buffer.
4 constant SLOTS
96 constant SPAN-CAP        \ per-slot bytes; a longer span is capped, never overruns
3 constant MARK-LEN         \ bytes the truncation marker occupies inside SPAN-CAP
46 constant MARK-BYTE       \ ASCII '.', repeated MARK-LEN times

0 constant S-KIND           \ declaration kind word: "structure" / "enum"
1 constant S-FAMILY         \ declared family name ("" before the name is parsed)
2 constant S-TOKEN          \ offending token ("" when the fault has no token)
3 constant S-REASON         \ short reason text

create SPAN-BUF  SLOTS SPAN-CAP * allot
create SPAN-LEN  SLOTS cells allot

\ The one raw-memory boundary in this package.  A checked body cannot type the
\ address arithmetic from a `create` region to a `ptr u8` span, exactly as
\ structure-decl.f's PEND! / PEND@ pushback cells cannot be typed; every other
\ word here is ordinary checked Habu.  SLOT! clamps to SPAN-CAP, so no caller can
\ write past its slot regardless of the span it was handed.
\
\ A capped span is MARKED, not silently shortened: the last MARK-LEN bytes of the
\ slot become "..." so the rendered packet says the value continued rather than
\ presenting a prefix as if it were the whole name.  Silent truncation would
\ report a different identifier than the source contains, which is strictly worse
\ than the legacy definer's borrowed span; the marker keeps the bounded copy
\ honest while still refusing to read memory the declaration no longer owns.
TRUSTED: SLOT! ( ptr u8 n n -- ) {: a:ptr u:n s:n :}   \ span, slot
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
   THEN ;
TRUSTED: SLOT@ ( n -- ptr u8 n ) {: s:n :}
   s SPAN-CAP * SPAN-BUF +  s cells SPAN-LEN + @ ;

\ render.f's declaration-diagnostic writer: the SAME producer the legacy
\ definers report through, so prose, JSON, and the check tool's packet capture
\ all behave identically for a unified declaration.
TRUSTED: DIAG ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) TDECL-DIAG ;

\ Multi-error load state, the checker's own (checker.f). Under `--all-errors` a
\ rejected definition is counted and the load continues instead of stopping at
\ the first fault; the legacy definers' reporter TDECL-RUN has answered that way
\ since multi-error mode existed, and GUARD below is the unified front ends'
\ reporter, so it answers the same way.
TRUSTED: MULTI? ( -- bool ) MULTI-ERR? ;
TRUSTED: MULTI-COUNT+ ( -- ) 1 MULTI-ERR-N +! ;

\ --- reject codes this package can be asked to explain.  Re-declared package-
\ locally with the owning name in the comment, exactly as structure-decl.f and
\ enum-decl.f re-declare theirs: the global pre-hook constants that own these
\ values are removed by the type-DSL cutover and do not survive the checked
\ engine's fixpoint self-rebuild.
7101 constant C-CASE        \ type-family.f E-TFAM-CASE
7102 constant C-DUP         \ type-family.f E-TFAM-DUP
7107 constant C-SYNTAX      \ sumtype.f E-TDECL-SYNTAX
7108 constant C-ARITY       \ sumtype.f E-TDECL-ARITY
7109 constant C-PAYLOAD     \ sumtype.f E-TDECL-PAYLOAD
7110 constant C-NAME        \ sumtype.f E-TDECL-NAME
7116 constant C-POLICY      \ sumtype.f E-TDECL-POLICY
7117 constant C-RECURSIVE   \ sumtype.f E-TDECL-RECURSIVE
7118 constant C-CAP         \ sumtype.f E-TDECL-CAP
7119 constant C-DERIVE      \ sumtype.f E-TDECL-DERIVE
7122 constant C-PF-ID       \ type-family.f E-PF-ID
7123 constant C-PF-TX       \ type-family.f E-PF-TX
7124 constant C-PF-OWNER    \ type-family.f E-PF-OWNER
7125 constant C-PF-NAME     \ type-family.f E-PF-NAME
7126 constant C-PF-SCHEMA   \ type-family.f E-PF-SCHEMA
7127 constant C-PF-LAYOUT   \ type-family.f E-PF-LAYOUT
7128 constant C-PF-FLAGS    \ type-family.f E-PF-FLAGS
7161 constant C-EVENT-TX    \ decl-event.f E-DEV-TX
7162 constant C-EVENT-STATE \ decl-event.f E-DEV-STATE
7163 constant C-DUP-POLICY  \ decl-event.f E-DEV-DUP-POLICY
7164 constant C-DUP-DERIVE  \ decl-event.f E-DEV-DUP-DERIVE
7170 constant C-SEALED      \ declaration-transaction.f E-REGISTRATION-SEALED
7171 constant C-PART-DEPTH  \ declaration-transaction.f E-PARTICIPANT-DEPTH
7172 constant C-FIELD-SCOPE \ decl-event.f E-DEV-FIELD-SCOPE
7173 constant C-FAMILY-SCOPE \ decl-event.f E-DEV-FAMILY-SCOPE
7174 constant C-DICT-TX     \ generated-declaration-dictionary.f E-DICTIONARY-TX
7175 constant C-DICT-CAP    \ generated-declaration-dictionary.f E-DICTIONARY-CAP
7176 constant C-CTOR-ARM    \ E-CTOR-ARM, below in this file
7177 constant C-REPLAY-BUSY \ DECL-REPLAY:E-REPLAY-BUSY, below in this file
7169 constant C-PROTECTION-CAP \ generated-declaration-protection.f E-PROTECTION-CAP
7190 constant C-MAKE-FAM    \ structure-make.f E-SM-FAM
7191 constant C-MAKE-EMPTY  \ structure-make.f E-SM-EMPTY

0 constant NO-CODE          \ no reason is armed

variable ARMED              \ the code the armed reason explains (NO-CODE = none)

: FOUND ( -- bool ) 0 0= ;
: MISSING ( -- bool ) 0 0= 0= ;
: NOTHING$ ( -- ptr u8 n ) s" " ;
: FALLBACK$ ( -- ptr u8 n ) s" declaration failed" ;

\ --- reason table, grouped by the registry that raises each code.  Each group
\ answers the text and a found flag, so an unmapped code stays distinguishable
\ from a mapped one instead of silently reading as the default.
: REASON-GRAMMAR ( n -- ptr u8 n bool ) {: code:n :}
   code C-SYNTAX    = IF s" malformed declaration"        FOUND EXIT THEN
   code C-ARITY     = IF s" arity must be a decimal, at most 23 parameters" FOUND EXIT THEN
   code C-PAYLOAD   = IF s" unknown payload type"         FOUND EXIT THEN
   code C-POLICY    = IF s" unknown layout policy"        FOUND EXIT THEN
   code C-RECURSIVE = IF s" invalid layout policy for recursive sum" FOUND EXIT THEN
   code C-CAP       = IF s" declaration too long"         FOUND EXIT THEN
   \ 7119 reaching the table (rather than an armed front-end reason) means the
   \ derive requirement failed later, in the constructor participant's payload
   \ role and equality checks, not that the feature token was unknown.
   code C-DERIVE    = IF s" a payload type or role has no derived equality" FOUND EXIT THEN
   NOTHING$ MISSING ;

: REASON-NAME ( n -- ptr u8 n bool ) {: code:n :}
   \ Both front ends arm "reserved name" at their own name gates, so 7110
   \ reaching this table was raised by a deeper owner: the variant-name gate
   \ (a reserved tail or a family-name collision) or the constructor collide
   \ check (a variant spelled like a word the DERIVE clause generates). The text
   \ has to be true of both.
   code C-NAME = IF s" name is reserved or already taken" FOUND EXIT THEN
   code C-CASE = IF s" name must be a lowercase tail"     FOUND EXIT THEN
   code C-DUP  = IF s" duplicate name in this package"    FOUND EXIT THEN
   NOTHING$ MISSING ;

: REASON-FIELD ( n -- ptr u8 n bool ) {: code:n :}
   code C-PF-ID     = IF s" invalid or uncommitted field id"         FOUND EXIT THEN
   code C-PF-TX     = IF s" stale or out-of-order field transaction" FOUND EXIT THEN
   code C-PF-OWNER  = IF s" invalid field owner family or variant"   FOUND EXIT THEN
   code C-PF-NAME   = IF s" reserved field name"                     FOUND EXIT THEN
   code C-PF-SCHEMA = IF s" malformed or owner-incompatible field schema" FOUND EXIT THEN
   code C-PF-LAYOUT = IF s" invalid field layout metadata"           FOUND EXIT THEN
   code C-PF-FLAGS  = IF s" undefined field flag bits"               FOUND EXIT THEN
   NOTHING$ MISSING ;

: REASON-TXN ( n -- ptr u8 n bool ) {: code:n :}
   code C-EVENT-TX     = IF s" stale or out-of-order declaration event" FOUND EXIT THEN
   code C-EVENT-STATE  = IF s" field publication broke field-id contiguity" FOUND EXIT THEN
   code C-DUP-POLICY   = IF s" a second POLICY clause in one declaration" FOUND EXIT THEN
   code C-DUP-DERIVE   = IF s" the same DERIVE feature twice in one declaration" FOUND EXIT THEN
   code C-SEALED       = IF s" declaration registration is sealed"   FOUND EXIT THEN
   code C-PART-DEPTH   = IF s" declaration participant depth mismatch" FOUND EXIT THEN
   code C-FIELD-SCOPE  = IF s" field is outside this declaration"    FOUND EXIT THEN
   code C-FAMILY-SCOPE = IF s" declaration does not own this family" FOUND EXIT THEN
   code C-DICT-TX      = IF s" generated-name transaction is out of order" FOUND EXIT THEN
   code C-DICT-CAP     = IF s" generated-name table is full"         FOUND EXIT THEN
   code C-CTOR-ARM     = IF s" family cannot own generated constructors" FOUND EXIT THEN
   code C-REPLAY-BUSY  = IF s" a replayed declaration is already open"   FOUND EXIT THEN
   code C-MAKE-FAM     = IF s" family cannot own generated make/unmake" FOUND EXIT THEN
   code C-MAKE-EMPTY   = IF s" a constructed family needs at least one field" FOUND EXIT THEN
   NOTHING$ MISSING ;

\ The protection owner raises one code for one condition: there is no room left to
\ protect the constructor wordlists this declaration needs.  It went unmapped for a
\ long time, so an exhausted registry printed the FALLBACK$ text against whatever
\ family happened to declare next -- the maki competitive-evidence red was
\ misattributed to an innocent enum for two days because of exactly that.  The
\ reason names the registry, so the reader looks at capacity instead of at the
\ declaration in front of them.
: REASON-PROTECTION ( n -- ptr u8 n bool ) {: code:n :}
   code C-PROTECTION-CAP = IF s" the protected-wordlist registry is full" FOUND EXIT THEN
   NOTHING$ MISSING ;

: CODE-REASON ( n -- ptr u8 n ) {: code:n :}
   code REASON-GRAMMAR    IF EXIT THEN 2drop
   code REASON-NAME       IF EXIT THEN 2drop
   code REASON-FIELD      IF EXIT THEN 2drop
   code REASON-TXN        IF EXIT THEN 2drop
   code REASON-PROTECTION IF EXIT THEN 2drop
   FALLBACK$ ;

\ An armed reason describes ONE code.  If a different code escaped, the arming
\ was for a fault that did not happen and the table answers instead.
: PICK-REASON ( n -- ptr u8 n ) {: code:n :}
   ARMED @ code = IF S-REASON SLOT@ EXIT THEN
   code CODE-REASON ;

: RENDER ( n -- ) {: code:n :}
   S-KIND SLOT@  S-FAMILY SLOT@  S-TOKEN SLOT@  code PICK-REASON  DIAG ;

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
: AT-FAMILY ( -- ) S-FAMILY SLOT@ TOKEN! ;

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

\ GUARD ( body -- ) : run one declaration.  A reject is rendered through the
\ shared declaration diagnostic and then rethrown with its exact code, so the
\ transaction's rollback, the caller's error handling, and every pinned reject
\ value are unchanged; only the missing message is added.
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
\ were code. Resynchronizing is the front end's job — only it knows its own
\ terminator — so each one skips to its own before raising; MULTI-ERROR? below is
\ how it asks whether this load will swallow.
: GUARD ( [ -- ] -- )
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   rc RENDER
   MULTI? IF MULTI-COUNT+ EXIT THEN
   rc throw ;

\ Will a reject be swallowed rather than raised? The front ends ask this to
\ decide whether they still owe the interpreter a resynchronized input stream.
: MULTI-ERROR? ( -- bool ) MULTI? ;

\ Reflection for the suites: what the packet would report right now.
: KIND$ ( -- ptr u8 n ) S-KIND SLOT@ ;
: FAMILY$ ( -- ptr u8 n ) S-FAMILY SLOT@ ;
: TOKEN$ ( -- ptr u8 n ) S-TOKEN SLOT@ ;
: REASON$ ( n -- ptr u8 n ) PICK-REASON ;

private
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
\ Why it lives in THIS file rather than its own.  It has to be defined before
\ its first consumer and needs nothing from the layer below — the same argument
\ that puts DECL-REJECT here.  It also has to be visible to GENERATED-DECL-CTOR
\ further down this file and to structure-make.f, which is why it sits above
\ both.  A separate src/core/decl-replay.f would be the tidier home, but adding
\ any engine-prefix source file requires editing the file list in
\ tools/build-fixpoint.f, which is wholly unpackaged and not allowlisted, so
\ that edit fails the package-ownership gate (measured: adding one
\ BF-APPEND-SOURCE line reports "`BF-APPEND-DECL-FILES` defines a changed
\ module word outside a package").  That veto is already tracked as its own
\ dot; when it lifts, this package moves out unchanged.
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

32 constant RP-SPACE
9 constant RP-TAB
10 constant RP-LF
13 constant RP-CR

variable RP-OPEN?      \ a replay stream is installed (0 = tokens come from live input)
variable RP-HEAD?      \ the leading (family name) token has not been read yet
variable RP-HEAD-U     variable RP-HEAD-A
variable RP-BODY-U     variable RP-BODY-A
variable RP-SCAN-I     \ scan cursor within the body buffer

\ The two raw-memory boundaries, the same idiom as structure-decl.f's PEND! /
\ PEND@ pushback cells: a checked body cannot store a `ptr u8` through a plain
\ cell and read it back.  Every other word in this package is checked Habu, and
\ the token scan below runs entirely on the typed span these answer.
TRUSTED: RP-HEAD! ( ptr u8 n -- ) RP-HEAD-U ! RP-HEAD-A ! ;
TRUSTED: RP-HEAD@ ( -- ptr u8 n ) RP-HEAD-A @ RP-HEAD-U @ ;
TRUSTED: RP-BODY! ( ptr u8 n -- ) RP-BODY-U ! RP-BODY-A ! ;
TRUSTED: RP-BODY@ ( -- ptr u8 n ) RP-BODY-A @ RP-BODY-U @ ;

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
   0 RP-OPEN? !  0 RP-HEAD? !  0 RP-HEAD-U !  0 RP-BODY-U !  0 RP-SCAN-I ! ;

\ RP-NEXT ( -- token ) : the family name once, then the body tokens in order, then
\ zero-length forever — the same end-of-input signal `parse-name` gives the
\ front ends, so their "missing ;STRUCTURE" / "missing ;ENUM" gates fire
\ unchanged on a truncated stream.
: RP-NEXT ( -- ptr u8 n )
   RP-HEAD? @ IF 0 RP-HEAD? ! RP-HEAD@ EXIT THEN
   RP-BODY-NEXT ;

private
;package

\ The checker participant lives in this file, so it owns its identity and order
\ outright. The three orders published below belong to participant modules that
\ enroll from their own files and have to read the value from somewhere.
package CHECKER-DECL-FRAME

1 constant PARTICIPANT
100 constant ORDER

: PART-SNAPSHOT ( n -- n ) {: depth:n :}
   depth START
   depth ;

: PART-PREPARE ( n -- n ) {: depth:n :}
   depth PREPARE 0=
      IF DECLARATION-TRANSACTION:E-PARTICIPANT-DEPTH throw THEN
   depth ;

: PART-COMMIT ( n -- n ) ;

: PART-ROLLBACK ( n -- n ) {: depth:n :}
   depth ROLLBACK
   depth ;

\ The frame word this forwards to is already throw-free by contract; see the
\ RELEASE comment in src/core/checker.f.
: PART-RELEASE ( -- )
   RELEASE ;

public

: INSTALL ( ptr a -- )
   PARTICIPANT ORDER
   [: PART-SNAPSHOT ;]
   [: PART-PREPARE ;]
   [: PART-COMMIT ;]
   [: PART-ROLLBACK ;]
   [: PART-RELEASE ;]
   DECLARATION-TRANSACTION:REGISTER ;

private
;package

package GENERATED-DECL-OWNER

\ Five sealed participants enroll: the checker frame (100), DECL-EVENT (800),
\ constructor generation (820), the native dictionary (850), and protection
\ (900). The boot arena is sized to hold all five, so no ordinary boot performs
\ a participant-table grow; the growth path stays live for a future sixth.
5 constant PARTICIPANT-CAP-INIT

create PARTICIPANT-BOOT
   PARTICIPANT-CAP-INIT DECLARATION-TRANSACTION:ROW-CELLS * cells allot
create STATE DECLARATION-TRANSACTION:STATE-CELLS cells allot

: INIT ( -- )
   STATE PARTICIPANT-BOOT PARTICIPANT-CAP-INIT
   [: DECLARATION-TRANSACTION:DEFAULT-ALLOCATOR ;]
   [: DECLARATION-TRANSACTION:DEFAULT-DIAGNOSTIC ;]
   DECLARATION-TRANSACTION:INIT
   STATE CHECKER-DECL-FRAME:INSTALL ;

public

: REGISTER
   ( n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ -- ] -- )
   {: id:n order:n snapshot prepare commit rollback release :} \ typed-local-lint: allow-bare-local
   STATE id order snapshot prepare commit rollback release
   DECLARATION-TRANSACTION:REGISTER ;

: REGISTER-LAST
   ( n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ -- ] -- )
   REGISTER
   STATE DECLARATION-TRANSACTION:SEAL ;

: RUN ( [ -- ] -- )
   STATE DECLARATION-TRANSACTION:SEALED? 0=
      IF DECLARATION-TRANSACTION:E-REGISTRATION-SEALED throw THEN
   STATE swap DECLARATION-TRANSACTION:RUN ;

: COUNT ( -- n ) STATE DECLARATION-TRANSACTION:COUNT ;
: DEPTH ( -- n ) STATE DECLARATION-TRANSACTION:DEPTH ;
: SEALED? ( -- bool ) STATE DECLARATION-TRANSACTION:SEALED? ;
: POISONED? ( -- bool ) STATE DECLARATION-TRANSACTION:POISONED? ;
: LAST-FAILURE-PHASE ( -- n ) STATE DECLARATION-TRANSACTION:LAST-FAILURE-PHASE ;
: LAST-FAILURE-PARTICIPANT ( -- n ) STATE DECLARATION-TRANSACTION:LAST-FAILURE-PARTICIPANT ;
: LAST-CLEANUP-PARTICIPANT ( -- n ) STATE DECLARATION-TRANSACTION:LAST-CLEANUP-PARTICIPANT ;

private

INIT
get-current prot-wid-add

;package

package GENERATED-DECL

public

DECLARATION-TRANSACTION:E-PARTICIPANT-DUP constant E-PARTICIPANT-DUP
DECLARATION-TRANSACTION:E-REGISTRATION-ACTIVE constant E-REGISTRATION-ACTIVE
DECLARATION-TRANSACTION:E-TRANSACTION-POISONED constant E-TRANSACTION-POISONED
DECLARATION-TRANSACTION:E-REGISTRATION-SEALED constant E-REGISTRATION-SEALED

800 constant ORDER-EVENT
820 constant ORDER-CONSTRUCTOR
850 constant ORDER-DICTIONARY
900 constant ORDER-PROTECTION

DECLARATION-TRANSACTION:PHASE-SNAPSHOT constant PHASE-SNAPSHOT
DECLARATION-TRANSACTION:PHASE-BODY constant PHASE-BODY
DECLARATION-TRANSACTION:PHASE-PREPARE constant PHASE-PREPARE
DECLARATION-TRANSACTION:PHASE-COMMIT constant PHASE-COMMIT
DECLARATION-TRANSACTION:PHASE-ROLLBACK constant PHASE-ROLLBACK

: RUN ( [ -- ] -- ) GENERATED-DECL-OWNER:RUN ;
: COUNT ( -- n ) GENERATED-DECL-OWNER:COUNT ;
: DEPTH ( -- n ) GENERATED-DECL-OWNER:DEPTH ;
: SEALED? ( -- bool ) GENERATED-DECL-OWNER:SEALED? ;
: POISONED? ( -- bool ) GENERATED-DECL-OWNER:POISONED? ;
: LAST-FAILURE-PHASE ( -- n ) GENERATED-DECL-OWNER:LAST-FAILURE-PHASE ;
: LAST-FAILURE-PARTICIPANT ( -- n ) GENERATED-DECL-OWNER:LAST-FAILURE-PARTICIPANT ;
: LAST-CLEANUP-PARTICIPANT ( -- n ) GENERATED-DECL-OWNER:LAST-CLEANUP-PARTICIPANT ;

private

TRUSTED: INSTALL-DECLARATION-RUNNER ( -- )
   [: GENERATED-DECL-OWNER:RUN ;] is TDECL-TXN-XT
   -1 TDECL-TXN-ARMED ! ;

INSTALL-DECLARATION-RUNNER
get-current prot-wid-add

;package

\ ---------------------------------------------------------------------------
\ The constructor-generation participant (ORDER 820).
\
\ It renders, evaluates, certifies and publishes one declared family's checked
\ FAMILY:VARIANT constructors from inside the declaration transaction. The order
\ is the whole design:
\
\   100 checker | 800 DECL-EVENT | 820 here | 850 dictionary | 900 protection
\
\ Commit runs in ascending order (declaration-transaction.f INSERT-AT keeps the
\ table sorted and RUN-FORWARD walks it upward), so by the time this participant
\ commits, DECL-EVENT's commit has already run TYPE-FIELD-OWNER:COMMIT, which
\ advances PF-COMMIT-N over this declaration's field rows. That is exactly the
\ watermark SUMV-NAMED-PAYLOAD? / SUMV-PAY-N read, so the shared generator can be
\ driven from the ordinary COMMITTED provider (TDECL-SUMV-PROVIDER) and needs no
\ provisional reader and no live-token provider of its own.
\
\ Generation is fallible by design. PHASE-COMMIT is the reversible commit phase:
\ a throw here rolls every participant back in reverse order, and because the
\ dictionary owner took its savepoint during PHASE-SNAPSHOT — before the body ran
\ — every word this participant already evaluated is truncated with the rest of
\ the declaration. A failure mid-set therefore publishes no constructor at all.
\
\ This participant reads no ambient last-registered-family register: the family
\ it acts on is armed by the declaration front end into a slot owned here and
\ indexed by this transaction's own nesting depth.
\ ---------------------------------------------------------------------------
package GENERATED-DECL-CTOR

7176 constant E-CTOR-ARM   \ armed family is not a public ENUM family with variants

5 constant PARTICIPANT
-1 constant NO-FAMILY

\ One cell per nesting level. Room is ensured in SNAPSHOT, which is allowed to
\ allocate; every later phase only reads and writes an existing cell, which is
\ what lets RELEASE stay total. Same shape as DECL-EVENT's DEV-PART-* slots.
4 constant CAP-INIT
create ARM-BOOT CAP-INIT cells allot
PTR-VARIABLE ARM-P   ARM-BOOT ARM-P !
variable ARM-CAP     CAP-INIT ARM-CAP !

\ Trusted forwarders to the pre-hook registry and generator words. sumtype.f and
\ type-family.f load before the checker hook, so a post-hook checked body reaches
\ them exactly the way enum-decl.f and structure-decl.f reach their registry
\ seams.
TRUSTED: ARM-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: FAM-PUBLIC? ( n -- bool ) TFAM-PUBLIC? ;
TRUSTED: FAM-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: FAM-ENUM? ( n -- bool ) TFAM-ENUM? ;
TRUSTED: FAM-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: CTOR-COLLIDE ( n n n -- ) TDECL-DERIVE-COLLIDE ;
TRUSTED: CTOR-REQUIRE ( n n n -- ) TDECL-DERIVE-REQUIRE ;
TRUSTED: CTOR-PUBLISH ( n n n -- ) TDECL-CTOR-PUBLISH ;
TRUSTED: CTOR-PROVIDER ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   TDECL-SUMV-PROVIDER ;
TRUSTED: CTOR-GEN ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n )
   TDECL-CTOR-WORDS-BODY ;
TRUSTED: PEND-CLEAR ( -- ) CTOR-PEND-CLEAR ;
TRUSTED: VAR-CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;

: ARM-BASE ( -- ptr a ) ARM-P @ ;
: ARM-SLOT ( -- ptr a )                    \ this nesting level's armed-family cell
   GENERATED-DECL:DEPTH 1 - cells ARM-BASE + ;
: ARM-GROW1 ( -- )
   ARM-CAP @ 2 * {: nc:n :}
   ARM-P @ ARM-CAP @ cells nc cells ARM-GROW ARM-P !
   nc ARM-CAP ! ;
: ARM-ENSURE ( -- )
   GENERATED-DECL:DEPTH ARM-CAP @ <= IF EXIT THEN
   ARM-GROW1 ;

: ARMED-FAM ( -- n ) ARM-SLOT @ ;
: DISARM ( -- ) NO-FAMILY ARM-SLOT ! ;

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
   fam FAM-PUBLIC? 0= IF 0 0= 0= EXIT THEN
   fam FAM-VAR-COUNT 0 <= IF 0 0= 0= EXIT THEN
   fam FAM-SUM? fam FAM-ENUM? or ;

\ Has this family's set already been generated? TDPLAN-CTOR+ records the
\ constructor symbol on each variant row as it renders, so a non-zero symbol on
\ the first row means the words are live. This is an EXISTENCE check on the
\ registry, deliberately not a name lookup, and it is kept out of GEN-OK? so that
\ predicate keeps its single meaning — which kinds own constructors — for
\ ED-CLOSE's gate and the public OWNS?.
\
\ It matters because sumtype.f's TDPLAN-NAME+ answers a second plan row for a
\ live constructor with `76 die`, which kills the process outright: no throw, no
\ unwind, no rollback. A caller that arms an already-generated family must be
\ refused here, by a named throw the transaction can roll back, rather than
\ reaching that die. The production path never trips it — ED-CLOSE arms a family
\ whose variant rows were created moments earlier — so this is a boundary guard,
\ and test/enum-decl-suite.f §20g drives it directly.
: GENERATED? ( n -- bool ) {: fam:n :}
   fam FAM-VAR-START VAR-CTOR-SYM 0 <> ;

\ Order matters: GEN-OK? proves the variant range is non-empty, so the first
\ variant row GENERATED? reads exists.
: GEN-REQUIRE ( n -- ) {: fam:n :}
   fam GEN-OK? 0= IF E-CTOR-ARM throw THEN
   fam GENERATED? IF E-CTOR-ARM throw THEN ;

\ The pending-constructor authority queue is plan scratch shared with the
\ generator. Its only legal value at the start of a declaration is empty, so
\ SNAPSHOT establishes that value rather than copying one, and ROLLBACK restores
\ it after a failure that left entries queued.
: PART-SNAPSHOT ( n -- n ) {: depth:n :}
   ARM-ENSURE
   DISARM
   PEND-CLEAR
   depth ;

\ Non-mutating re-proof only. The front end proved this gate when it armed, in
\ the body phase; nothing between then and here may have made the family
\ ineligible, and if something did, the declaration fails before any word is
\ rendered rather than publishing a set nobody validated.
: PART-PREPARE ( n -- n ) {: depth:n :}
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY <> IF fam GEN-REQUIRE THEN
   depth ;

\ The work. Mirrors the legacy sum definer's close sequence (sumtype.f
\ CHECKER-DEFSUM-BODY): reject a variant spelled like a derived word, prove every
\ payload role is derivably comparable, stamp the constructor package on each
\ variant row, then render/evaluate/certify/seal the whole set through the shared
\ generator. Both role gates read committed rows, which is why they work here and
\ could not work in the body phase.
\
\ The last step, and ONLY the last step, is what a replayed declaration skips.
\ The split is not a shortcut, it is where the legacy definer already drew the
\ line: sumtype.f's global ENUM keyword was CHECKER-DEFENUM followed by
\ TDECL-CTOR-WORDS, and CHECKER-DEFENUM — the metadata-only entry check-core and
\ verify-source drove before this one existed — ended with TDECL-CTOR-PUBLISH and
\ defined no word.
\ So the constructor PACKAGE STAMP on each variant row is registration, not
\ generation: it is how a later `FAMILY:VARIANT` in the same source resolves at
\ all. Skipping the arming instead of the rendering would have left every
\ constructor use in a replayed file undefined, which is the whole reason the
\ legacy entry published those rows in the first place.
: PART-COMMIT ( n -- n ) {: depth:n :}
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY = IF depth EXIT THEN
   fam GEN-REQUIRE
   fam FAM-VAR-START {: vstart:n :}
   fam FAM-VAR-COUNT {: count:n :}
   fam vstart count CTOR-COLLIDE
   fam vstart count CTOR-REQUIRE
   fam vstart count CTOR-PUBLISH
   DECL-REPLAY:RP-ACTIVE? IF depth EXIT THEN
   CTOR-PROVIDER fam CTOR-GEN drop
   depth ;

: PART-ROLLBACK ( n -- n ) {: depth:n :}
   PEND-CLEAR
   DISARM
   depth ;

\ Total, like every release callback. Publication has already happened; the only
\ thing left is this owner's one armed-family cell, whose slot arithmetic cannot
\ fail because SNAPSHOT ensured the row exists at this depth and the depth is
\ unchanged until the coordinator leaves the transaction.
: PART-RELEASE ( -- )
   DISARM ;

: INSTALL ( -- )
   PARTICIPANT GENERATED-DECL:ORDER-CONSTRUCTOR
   [: PART-SNAPSHOT ;]
   [: PART-PREPARE ;]
   [: PART-COMMIT ;]
   [: PART-ROLLBACK ;]
   [: PART-RELEASE ;]
   GENERATED-DECL-OWNER:REGISTER ;

public

\ The declaration front end names the family whose constructors this transaction
\ owns. Legal only inside an open declaration transaction, and only for a family
\ that passes the gate, so a caller cannot arm a kind this participant refuses to
\ generate for and discover it three phases later.
: ARM ( n -- ) {: fam:n :}
   GENERATED-DECL:DEPTH 0 <= IF E-CTOR-ARM throw THEN
   fam GEN-REQUIRE
   fam ARM-SLOT ! ;

: OWNS? ( n -- bool ) GEN-OK? ;

private

INSTALL
get-current prot-wid-add

;package
