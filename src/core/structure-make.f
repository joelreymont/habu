\ structure-make.f — STRUCTURE constructor generator (package STRUCTURE-MAKE).
\
\ ONE concern: given a validated product-kind family id in the current
\ declaration transaction, generate the
\ sealed FAMILY:MAKE and FAMILY:UNMAKE checked words from that family's
\ declaration-order provisional field schemas. A STRUCTURE is a single-shape
\ record (docs/type-families.md §9.4, §18) — exactly the PRODUCT kind — so this
\ reuses the shared sum/product constructor-seal machinery (sumtype.f
\ TDECL-CTOR-PUBLISH / TDECL-PROD-WORDS, the CTOR-PEND! pending window, the one
\ audited generated-word eval crossing, the CHECKER-RECORD-SYM record, and the
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
\ ;STRUCTURE call; the front end gates it on a PUBLIC family WITH fields, so
\ GENERATE is only reached when its whole contract already holds (see
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
\ in CHECKED code first — family liveness, product kind, public visibility, at
\ least one field, every field row present in the current transaction, and
\ MAKE/UNMAKE not already generated — and only then runs SM-EMIT. Evaluation and
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
TRUSTED: SM-PUBLIC? ( n -- bool ) TFAM-PUBLIC? ;
TRUSTED: SM-FLD-START ( n -- n ) TFAM-FLD-START@ ;
TRUSTED: SM-FLD-COUNT ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: SM-SUMV-FIND ( n ptr u8 n -- n bool ) SUMV-FIND ;

: SM-FIELD-SCHEMA@ ( n n n n -- n ) {: idx:n tok:n fam:n fs:n :}
   tok fam fs idx + DECL-EVENT:FIELD-SCHEMA@ ;

\ SM-EMIT: the sealed mutation for a validated provisional product.
\ Rebuilds the current transaction's field schema nodes into one contiguous declaration-order
\ root run (the payload-schema range the two generated variant rows carry —
\ exactly the run shape the PRODUCT ctor path builds while parsing its fields),
\ adds the make(0)/unmake(1) variant rows over it at the family's product width,
\ sets the variant range, derives the constructor package, and drives the shared
\ generator. TRUSTED: because every word it calls is a sealed pre-hook registry /
\ generation word. GENERATE has already validated its local preconditions; any
\ evaluator or checker rejection propagates to GENERATED-DECL for full rollback.
variable SM-RSTART   variable SM-VSTART
TRUSTED: SM-EMIT ( n n n n -- ) {: tok:n fam:n fs:n fc:n :}
   SCHEMA-ROOT-N@ SM-RSTART !
   0 BEGIN dup fc < WHILE
      dup tok fam fs SM-FIELD-SCHEMA@ SCHEMA-ROOT@ SCHEMA-ROOT+ drop
      1 +
   REPEAT drop
   SUMV-N@ SM-VSTART !
   fam s" make"   0 SM-RSTART @ fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam s" unmake" 1 SM-RSTART @ fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam SM-VSTART @ 2 TFAM-VAR-RANGE!
   fam SM-VSTART @ 2 TDECL-CTOR-PUBLISH
   fam TDECL-PROD-WORDS ;

\ --- validation pass (checked; no registry write). A reject here leaves every
\ registry byte-identical, so publication is attempted only after it wholly passes.
: SM-REQUIRE-FAMILY ( n -- ) {: fam:n :}   \ live, public, product-kind
   fam SM-FAM-LIVE? 0= IF E-SM-FAM throw THEN
   fam SM-PRODUCT? 0= IF E-SM-FAM throw THEN
   fam SM-PUBLIC? 0= IF E-SM-FAM throw THEN ;

: SM-REQUIRE-READABLE ( n n n -- ) {: tok:n fam:n fc:n :}   \ every field row is live in this transaction
   0 BEGIN dup fc < WHILE
      dup tok fam fam SM-FLD-START SM-FIELD-SCHEMA@ drop
      1 +
   REPEAT drop ;

: SM-REQUIRE-UNGENERATED ( n -- ) {: fam:n :}   \ MAKE/UNMAKE not already generated
   fam s" make"   SM-SUMV-FIND IF drop E-SM-DUP throw THEN drop
   fam s" unmake" SM-SUMV-FIND IF drop E-SM-DUP throw THEN drop ;

public

\ GENERATE ( fam -- ) : define the sealed FAMILY:MAKE ( fields -- family ) and
\ FAMILY:UNMAKE ( family -- fields ) checked words for a public product family,
\ from its declaration-order schemas in the current field transaction.
\ Throws E-SM-FAM (not a live/public/product family), E-SM-EMPTY (no fields),
\ a DECL-EVENT scope error (the token does not own that family/field), or E-SM-DUP
\ (MAKE/UNMAKE already generated) — every reject
\ before any registry write, so a rejected call publishes nothing.
: GENERATE ( n n -- ) {: tok:n fam:n :}
   fam SM-REQUIRE-FAMILY
   fam SM-FLD-COUNT {: fc:n :}
   fc 1 < IF E-SM-EMPTY throw THEN
   fam SM-REQUIRE-UNGENERATED
   tok fam fc SM-REQUIRE-READABLE
   tok fam  fam SM-FLD-START  fc  SM-EMIT ;

;package
