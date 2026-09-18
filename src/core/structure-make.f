\ structure-make.f — STRUCTURE constructor generator (package STRUCTURE-MAKE).
\
\ ONE concern: given a validated product-kind family id in the current
\ declaration transaction, generate the
\ sealed FAMILY:MAKE and FAMILY:UNMAKE checked words from that family's
\ declaration-order provisional field schemas. A STRUCTURE is a single-shape
\ record (docs/type-families.md §9.4, §18) — exactly the PRODUCT kind — so this
\ reuses the shared sum/product constructor-seal machinery (sumtype.f
\ TDECL-CTOR-PUBLISH / TDECL-PROD-WORDS, the ordered constructor plan, the one
\ audited complete-plan eval crossing, the CHECKER-RECORD-SYM records, and the
\ closed-but-callable word-id protection) verbatim rather than re-implementing
\ the security-critical publication discipline a second time. The only new
\ logic here is the STRUCTURE adapter: validate the family the shared
\ DECL-EVENT/TYPE-FIELD transaction produced (src/core/decl-event.f,
\ src/core/type-family.f) and drive the shared generator over its provisional
\ field rows.
\
\ SEAM the parse front end calls (reconciled at merge): after a STRUCTURE body
\ has registered its family
\ (TFAM-DECL), its layout width recorded (TFAM-SLOTS!), its field range recorded
\ (TFAM-FLD-RANGE!), and its fields staged (TYPE-FIELD) — the front end's
\ ;STRUCTURE calls STRUCTURE-MAKE:GENERATE with the family id. This file adds NO
\ front end, NO parser state, and NO grammar: it owns MAKE/UNMAKE generation
\ only. The reconciliation (dot habu-structure-generate-make-872a6e75) wired that
\ ;STRUCTURE call; the front end gates it on a family WITH fields, so GENERATE is
\ only reached when its whole contract already holds (see
\ src/core/structure-decl.f's constructor-generation seam note). This module now
\ loads baked in the post-hook DECL group immediately BEFORE
\ src/core/structure-decl.f — the front end references STRUCTURE-MAKE:GENERATE, so
\ the generator must be defined first. test/structure-make-suite.f still drives
\ GENERATE directly over hand-built declarations for the field-kind and rollback
\ matrix.
\
\ Generation reads layout truth (field schemas, generic parameter slots, the
\ product width, hidden-field expansion) straight from the shared transaction
\ metadata; nothing is recomputed here. The generated MAKE effect is
\ (declaration-order field values -- family value); UNMAKE is (family value --
\ declaration-order field values). A product bundle IS its field cells in slot
\ order (no tag, docs §18), so both bodies are physical no-ops the checker
\ certifies against the field-derived metadata row; the generated text never
\ contains TRUST, TRUSTED:, or set-check.
\
\ Atomic publication after validation. GENERATE decides every local condition
\ in CHECKED code first — family liveness, product kind, at least one field,
\ every field row present in the current transaction, and MAKE/UNMAKE not
\ already generated — and only then runs SM-EMIT. Evaluation and
\ checker certification may still reject while SM-EMIT generates the words; the
\ enclosing GENERATED-DECL transaction retains every registry and dictionary
\ savepoint until that work succeeds, so either all metadata and both words
\ publish or every participant rolls back.
\
\ TRUSTED boundary. The checkable DECISIONS stay checked; the trusted set is the
\ decl-event.f idiom (sealed pre-hook colon words the checker cannot type from a
\ post-hook checked body, reached through named forwarders): six thin read
\ forwarders the validation reads, plus one SM-EMIT that performs the sealed
\ mutation — build the field schema run, add the two variant rows,
\ set the variant range, derive the constructor package, generate both words.
\ Provisional field-schema reads go through DECL-EVENT with the exact live
\ declaration token and family, so no uncommitted field row is globally readable.
\ When the type-DSL cutover factors the shared
\ generator into its own module, SM-EMIT re-points there; nothing else changes.

using SCHEMA-REG
using TFAM
using TYPE-DECL

\ --- named reject codes (7101-7128 = tfam/schema/pf; 7161-7164 = decl-event;
\ 7161/7172/7173 are the event token, field-range, and family-scope rejects for
\ provisional reads). E-SM-DUP re-declares the
\ field record's duplicate code (E-TFAM-DUP 7102) by value: that pre-hook global
\ is not visible from a post-hook checked body, so it is re-stated here rather
\ than referenced — the src/core/decl-event.f idiom for E-DEV-ARITY. 7190-7191
\ are this module's own.
7102 constant E-SM-DUP        \ MAKE/UNMAKE already generated (mirrors E-TFAM-DUP)
7190 constant E-SM-FAM        \ family id is not a live, public, product-kind family
7191 constant E-SM-EMPTY      \ product family declares no fields

package STRUCTURE-MAKE

\ --- pre-hook boundary forwarders. TRUSTED: because they call sealed pre-hook
\ registry / generation words the checker cannot type here (decl-event.f idiom).
TRUSTED: SM-FAM-LIVE? ( n -- bool ) PF-FAM-LIVE? ;
TRUSTED: SM-PRODUCT? ( n -- bool ) TFAM-PRODUCT? ;
TRUSTED: SM-FLD-START ( n -- n ) TFAM-FLD-START@ ;
TRUSTED: SM-FLD-COUNT ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: SM-SUMV-FIND ( n ptr u8 n -- n bool ) SUMV-FIND ;

: SM-FIELD-SCHEMA@ ( n n n n -- n ) {: idx:n tok:n fam:n fs:n :}
   tok fam fs idx + DECL-EVENT:FIELD-SCHEMA@ ;

\ SM-EMIT-ROWS: the sealed REGISTRATION mutation for a validated provisional
\ product. Rebuilds the current transaction's field schema nodes into one contiguous
\ declaration-order root run (the payload-schema range the two generated variant rows
\ carry — exactly the run shape the PRODUCT ctor path builds while parsing its fields),
\ adds the make(0)/unmake(1) variant rows over it at the family's product width,
\ sets the variant range, and derives the constructor package.
\ TRUSTED: because every word it calls is a sealed pre-hook registry word.
\ GENERATE has already validated its local preconditions; any evaluator or checker
\ rejection propagates to GENERATED-DECL for full rollback.
variable SM-RSTART   variable SM-VSTART
TRUSTED: SM-EMIT-ROWS ( n n n n -- ) {: tok:n fam:n fs:n fc:n :}
   SCHEMA-ROOT-N@ SM-RSTART !
   0 BEGIN dup fc < WHILE
      dup tok fam fs SM-FIELD-SCHEMA@ SCHEMA-ROOT@ SCHEMA-ROOT+ drop
      1 +
   REPEAT drop
   SUMV-N@ SM-VSTART !
   fam s" make"   0 SM-RSTART @ fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam s" unmake" 1 SM-RSTART @ fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam SM-VSTART @ 2 TFAM-VAR-RANGE!
   fam SM-VSTART @ 2 TDECL-CTOR-PUBLISH ;

\ SM-EMIT-WORDS: the GENERATION half — render, evaluate, certify and seal the
\ MAKE/UNMAKE pair. Replay uses the same plan to register checked effects only.
TRUSTED: SM-EMIT-WORDS ( n -- ) {: fam:n :}
   fam TDECL-PROD-WORDS ;

TRUSTED: SM-REPLAY-WORDS ( n -- ) TDECL-CTOR-REPLAY ;

\ --- validation pass (checked; no registry write). A reject here leaves every
\ registry byte-identical, so publication is attempted only after it wholly passes.
\ Live and product-kind. Visibility is NOT a condition: a private product
\ publishes the same two words into its declaring package's private wordlist
\ (dot habu-generate-a-private-80272413), so the only thing left to require is
\ that the family is the single-shape record this generator knows how to build.
: SM-REQUIRE-FAMILY ( n -- ) {: fam:n :}   \ live, product-kind
   fam SM-FAM-LIVE? 0= IF E-SM-FAM throw THEN
   fam SM-PRODUCT? 0= IF E-SM-FAM throw THEN ;

: SM-REQUIRE-READABLE ( n n n -- ) {: tok:n fam:n fc:n :}   \ every field row is live in this transaction
   0 BEGIN dup fc < WHILE
      dup tok fam fam SM-FLD-START SM-FIELD-SCHEMA@ drop
      1 +
   REPEAT drop ;

: SM-REQUIRE-UNGENERATED ( n -- ) {: fam:n :}   \ MAKE/UNMAKE not already generated
   fam s" make"   SM-SUMV-FIND IF drop E-SM-DUP throw THEN drop
   fam s" unmake" SM-SUMV-FIND IF drop E-SM-DUP throw THEN drop ;

public

\ GENERATE ( fam -- ) : define the sealed MAKE ( fields -- family ) and
\ UNMAKE ( family -- fields ) checked words for a product family, from its
\ declaration-order schemas in the current field transaction. A PUBLIC family
\ gets FAMILY:MAKE / FAMILY:UNMAKE in its reserved constructor namespace; a
\ PRIVATE one gets FAMILY-MAKE / FAMILY-UNMAKE in the declaring package's private
\ wordlist (src/core/type-family.f TF-CTOR-PRIV$ owns both spellings).
\ Throws E-SM-FAM (not a live product family), E-SM-EMPTY (no fields),
\ a DECL-EVENT scope error (the token does not own that family/field), or E-SM-DUP
\ (MAKE/UNMAKE already generated) — every reject
\ before any registry write, so a rejected call publishes nothing.
\
\ A REPLAYED declaration (tools/check-core.f's nominal pass, verify-source)
\ registers the make/unmake rows and checks their generated bodies. Those
\ effects allow later source to call the constructors without emitting code
\ or adding runtime dictionary entries during verification.
: GENERATE ( n n -- ) {: tok:n fam:n :}
   fam SM-REQUIRE-FAMILY
   fam SM-FLD-COUNT {: fc:n :}
   fc 1 < IF E-SM-EMPTY throw THEN
   fam SM-REQUIRE-UNGENERATED
   tok fam fc SM-REQUIRE-READABLE
   tok fam  fam SM-FLD-START  fc  SM-EMIT-ROWS
   DECL-REPLAY:RP-ACTIVE? IF fam SM-REPLAY-WORDS EXIT THEN
   fam SM-EMIT-WORDS ;

private

\ ---------------------------------------------------------------------------
\ The ADDRESS-SURFACE participant (ORDER 830), for `DERIVE addr`.
\
\ MAKE / UNMAKE are generated from the still-provisional field schemas while the
\ body parses, which is why GENERATE above reads them through DECL-EVENT. The
\ accessors CANNOT be: each one is armed with a COMMITTED field id, and the
\ checker's field-projection window reads that id through the committed
\ reflection reader — a provisional id fails closed there, by design, because the
\ id is the projection's whole authority. So this work runs one phase later, in
\ the reversible commit phase, where the order does the arranging:
\
\   100 checker | 800 DECL-EVENT | 820 constructors | 830 here | 850 dictionary
\
\ DECL-EVENT's commit has already advanced PF-COMMIT-N over this declaration's
\ field rows by the time this participant commits, and the dictionary owner took
\ its savepoint before the body ran, so a reject here truncates every accessor
\ with the rest of the declaration and the registry stays byte-identical.
\
\ One armed family per nesting level, the slot shape GENERATED-DECL-CTOR uses.
\ ---------------------------------------------------------------------------
6 constant SM-PARTICIPANT
-1 constant SM-NO-FAMILY
4 constant SM-ARM-CAP-INIT

create SM-ARM-BOOT SM-ARM-CAP-INIT cells allot
PERSISTED-PTR-VARIABLE SM-ARM-P   SM-ARM-BOOT SM-ARM-P !
variable SM-ARM-CAP   SM-ARM-CAP-INIT SM-ARM-CAP !

TRUSTED: SM-ARM-GROW ( ptr n n n -- ptr n ) ARENA-BYTES-GROW ;
TRUSTED: SM-ADDR? ( n -- bool ) TFAM-DERIVE-ADDR? ;
TRUSTED: SM-ADDR-WORDS ( n -- ) TDECL-ADDR-WORDS ;
TRUSTED: SM-ADDR-REPLAY ( n -- ) TDECL-ADDR-REPLAY ;

: SM-ARM-BASE ( -- ptr n ) SM-ARM-P @ ;
: SM-ARM-SLOT ( -- ptr n )
   GENERATED-DECL:DEPTH 1 - cells SM-ARM-BASE + ;
: SM-ARM-GROW1 ( -- )
   SM-ARM-CAP @ 2 * {: nc:n :}
   SM-ARM-P @ SM-ARM-CAP @ cells nc cells SM-ARM-GROW SM-ARM-P !
   nc SM-ARM-CAP ! ;
: SM-ARM-ENSURE ( -- )
   GENERATED-DECL:DEPTH SM-ARM-CAP @ <= IF EXIT THEN
   SM-ARM-GROW1 ;

: SM-ARMED-FAM ( -- n ) SM-ARM-SLOT @ ;
: SM-DISARM ( -- ) SM-NO-FAMILY SM-ARM-SLOT ! ;

\ THE GATE. A family owns an address surface when it is a live product that
\ declared `DERIVE addr` and has at least one field. Visibility is not a
\ condition here either: it picks the spelling and the wordlist.
: SM-ADDR-OK? ( n -- bool ) {: fam:n :}
   fam SM-FAM-LIVE? 0= IF 0 0= 0= EXIT THEN
   fam SM-PRODUCT? 0= IF 0 0= 0= EXIT THEN
   fam SM-ADDR? 0= IF 0 0= 0= EXIT THEN
   fam SM-FLD-COUNT 0 > ;

: SM-ADDR-REQUIRE ( n -- ) {: fam:n :}
   fam SM-ADDR-OK? 0= IF E-SM-FAM throw THEN ;

: SM-PART-SNAPSHOT ( n -- n ) {: depth:n :}
   SM-ARM-ENSURE
   SM-DISARM
   depth ;

\ Non-mutating re-proof: the front end proved this gate when it armed, and if
\ anything since made the family ineligible the declaration fails before a word
\ is rendered rather than publishing a surface nobody validated.
: SM-PART-PREPARE ( n -- n ) {: depth:n :}
   SM-ARMED-FAM {: fam:n :}
   fam SM-NO-FAMILY <> IF fam SM-ADDR-REQUIRE THEN
   depth ;

\ Replay registers the accessors' checked effects from the same plan and emits
\ no code, the way the constructor participant (820) replays MAKE / UNMAKE — so
\ a tool that pre-scans a source sees the record's own accessors.
: SM-PART-COMMIT ( n -- n ) {: depth:n :}
   SM-ARMED-FAM {: fam:n :}
   fam SM-NO-FAMILY = IF depth EXIT THEN
   fam SM-ADDR-REQUIRE
   DECL-REPLAY:RP-ACTIVE? IF fam SM-ADDR-REPLAY depth EXIT THEN
   fam SM-ADDR-WORDS
   depth ;

: SM-PART-ROLLBACK ( n -- n ) {: depth:n :}
   SM-DISARM
   depth ;

: SM-PART-RELEASE ( -- ) SM-DISARM ;

: SM-INSTALL ( -- )
   SM-PARTICIPANT GENERATED-DECL:ORDER-ADDRESS
   [: SM-PART-SNAPSHOT ;]
   [: SM-PART-PREPARE ;]
   [: SM-PART-COMMIT ;]
   [: SM-PART-ROLLBACK ;]
   [: SM-PART-RELEASE ;]
   GENERATED-DECL-OWNER:REGISTER ;

public

\ ARM ( fam -- ) : the declaration front end names the family whose address
\ surface this transaction owns. Legal only inside an open declaration
\ transaction and only for a family that passes the gate, so a caller cannot arm
\ a family this participant refuses and discover it two phases later.
: ARM ( n -- ) {: fam:n :}
   GENERATED-DECL:DEPTH 0 <= IF E-SM-FAM throw THEN
   fam SM-ADDR-REQUIRE
   fam SM-ARM-SLOT ! ;

private

SM-INSTALL

;package

\ ---------------------------------------------------------------------------
\ The two runtime rows a generated address surface calls. Both are documented
\ global language surface (the package-first exception the STRUCTURE opener
\ itself takes) because a generated body names them unqualified from whatever
\ package declared the record.
\
\ `field-project` is the accessor body's op. OUTSIDE the checker's armed window
\ its row is exactly what it computes — a byte offset added to a pointer, no
\ retype, layout-fenced like any other `+` — so the word is not a capability and
\ user code gains nothing by calling it. INSIDE the window (armed only by the
\ generator, per accessor, with a committed field id) the checker replaces that
\ row with the schema-aware projection `ptr family<args> -- ptr field-type`.
\
\ `record-at` needs no window: it consumes `ptr F` and a CELL COUNT and answers
\ `ptr F`, preserving the family. It forges nothing — bounds stay the caller's,
\ exactly the contract `cells +` has on every other pointer — and it exists only
\ because `+` and `cell+` refuse a layout pointee outright, which is what makes
\ a record pointer safe in the first place. The generated `F:AT` bakes the
\ family's committed width as the multiplier; the row itself scales nothing.
\ ---------------------------------------------------------------------------
: field-project ( ptr a n -- ptr a ) + ;
: record-at ( ptr a n -- ptr a ) cells + ;

;using
;using
;using
