\ declaration-release-inventory.f - the release phase of the generated-declaration
\ transaction is total, proved from the production SOURCE.
\ Run: bin/hb --load test/declaration-release-inventory.f
\
\ src/core/declaration-transaction.f runs RELEASE after every reversible commit
\ has published, so a release callback has no error channel: it is executed
\ directly, never caught, never diagnosed, never allowed to poison the
\ coordinator. Nothing a declaration body can do reaches a release callback in a
\ state where it would want to reject, so that contract has no behavioural
\ witness - a validating release word survives every other suite in this
\ repository. This file is the witness it does have: tools/release-inventory.f
\ reads the eight production sources that host the release chain, finds each
\ participant registration and takes the word in its fifth callback slot, closes
\ over everything those words reach (including through deferred vectors, in both
\ their quotation and tick binding forms), and requires every reached word to be
\ either another definition in those sources or a member of a small allowlist of
\ total words. `throw`, `catch`, `die`, `allot`, `search-wl`, `prot-wid-add` and
\ the coordinator's own failure machinery are not on that allowlist, so
\ reintroducing any of them is red.
\
\ The second half drives the same analyser over hostile fixtures, so a change
\ that guts the analyser instead of the production code is red too.

require lib/test.f
require tools/release-inventory.f

T-RESET

package DECL-RELEASE-INV-TEST

\ ---------------------------------------------------------------------------
\ the production sources that host the release chain
\ ---------------------------------------------------------------------------

: READ-SOURCES ( -- )
   s" src/core/checker.f" RELEASE-INV:READ+
   s" src/core/type-schema.f" RELEASE-INV:READ+
   s" src/core/type-family.f" RELEASE-INV:READ+
   s" src/core/declaration-transaction.f" RELEASE-INV:READ+
   s" src/core/generated-declaration.f" RELEASE-INV:READ+
   s" src/core/decl-event.f" RELEASE-INV:READ+
   s" src/core/generated-declaration-dictionary.f" RELEASE-INV:READ+
   s" src/core/generated-declaration-protection.f" RELEASE-INV:READ+ ;

\ RELEASE-ALL is the coordinator's own release runner. It is reached through the
\ participant row table, not by name, so the registration walk cannot find it;
\ pinning it here is what makes a `catch` or a cleanup diagnostic put back around
\ the release loop fail this test.
: PIN-COORDINATOR ( -- )
   s" DECLARATION-TRANSACTION" s" RELEASE-ALL" RELEASE-INV:PIN-ROOT ;

: PRODUCTION-RUN ( -- n )
   RELEASE-INV:RESET
   5 RELEASE-INV:ROOTS-EXPECT!
   READ-SOURCES
   RELEASE-INV:INDEX
   RELEASE-INV:SEED
   PIN-COORDINATOR
   RELEASE-INV:RUN ;

\ Five sealed production participants register a release callback: the checker
\ frame, DECL-EVENT, constructor generation, the native dictionary, and
\ protection. The sixth registration call is the forwarder in
\ GENERATED-DECL-OWNER, which passes its own locals rather than quotations and is
\ therefore not a registration site. Constructor generation's release callback
\ lives in src/core/generated-declaration.f, already in the scanned set, so the
\ source list does not grow with it.
: TEST-PRODUCTION ( -- )
   PRODUCTION-RUN 0 T=
   RELEASE-INV:ROOTS 5 T=
   RELEASE-INV:SITES 6 T=
   RELEASE-INV:QUOTED-SITES 5 T=
   RELEASE-INV:DEFS 0 > TTRUE
   RELEASE-INV:BINDINGS 0 > TTRUE ;

\ ---------------------------------------------------------------------------
\ fixtures: one source built a line at a time, so the analyser is driven over
\ exactly the shapes that must not slip through
\ ---------------------------------------------------------------------------

$4000 constant FX-CAP
create FX-BUF FX-CAP allot
variable FX-U

: FX-ROOM ( n -- ) {: u:n :}
   FX-U @ u + FX-CAP > IF
      s" release inventory fixture buffer full" 78 die
   THEN ;

: FX-BEGIN ( -- )
   0 FX-U ! ;

: FX-RAW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u FX-ROOM
   a FX-BUF FX-U @ + u LINT-BMOVE
   FX-U @ u + FX-U ! ;

: FX-BYTE+ ( n -- ) {: c:n :}
   1 FX-ROOM
   c FX-BUF FX-U @ + c!
   FX-U @ 1 + FX-U ! ;

: FX-LF+ ( -- )
   10 FX-BYTE+ ;

: FX-DQ+ ( -- )
   34 FX-BYTE+ ;

: FX+ ( ptr u8 n -- )
   FX-RAW+ FX-LF+ ;

: FX$ ( -- ptr u8 n )
   FX-BUF FX-U @ ;

: FX-RUN ( n -- n ) {: expect:n :}
   RELEASE-INV:RESET
   expect RELEASE-INV:ROOTS-EXPECT!
   s" fixture" FX$ RELEASE-INV:SOURCE+
   RELEASE-INV:INDEX
   RELEASE-INV:SEED
   RELEASE-INV:RUN ;

\ Shared prefix: four ordinary callbacks, one of which rejects, plus the state a
\ real release callback is allowed to touch.
: FX-PREFIX ( -- )
   FX-BEGIN
   s" package FXP" FX+
   s" variable FX-DEPTH" FX+
   s" : FX-SNAP ( n -- n ) ;" FX+
   s" : FX-PREP ( n -- n ) {: d:n :} d 0 = IF 7 throw THEN d ;" FX+
   s" : FX-COMMIT ( n -- n ) ;" FX+
   s" : FX-ROLL ( n -- n ) ;" FX+ ;

: FX-REGISTER ( -- )
   s" : FX-INSTALL ( -- )" FX+
   s"    1 2 [: FX-SNAP ;] [: FX-PREP ;] [: FX-COMMIT ;] [: FX-ROLL ;]" FX+
   s"    [: FX-REL ;] GENERATED-DECL-OWNER:REGISTER ;" FX+
   s" ;package" FX+ ;

\ 1. a release callback that only drops a savepoint is clean.
: FX-CLEAN ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   FX-REGISTER ;

\ 2. a release callback that can reject is not.
: FX-THROWS ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ 0= IF 9 throw THEN ;" FX+
   FX-REGISTER ;

\ 3. comments are inert: the words below appear only inside them.
: FX-COMMENTS ( -- )
   FX-PREFIX
   s" : FX-REL ( -- throw catch die )" FX+
   s"    \ throw catch die allot search-wl prot-wid-add" FX+
   s"    FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   FX-REGISTER ;

\ 4. string bodies are inert too. The opener itself is a real call and is not on
\ the allowlist, so exactly one finding is correct here; four would mean the
\ string body leaked into the token stream.
: FX-STRING ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) s" FX-RAW+ FX-DQ+
   s"  throw catch die" FX-RAW+ FX-DQ+
   s"  2drop ;" FX+
   FX-REGISTER ;

\ 5. wrong role: the callbacks are reordered so the rejecting one lands in the
\ release slot. The inventory follows the slot, not the name.
: FX-WRONG-ROLE ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-INSTALL ( -- )" FX+
   s"    1 2 [: FX-SNAP ;] [: FX-REL ;] [: FX-COMMIT ;] [: FX-ROLL ;]" FX+
   s"    [: FX-PREP ;] GENERATED-DECL-OWNER:REGISTER ;" FX+
   s" ;package" FX+ ;

\ 6. a registration that no longer passes five callbacks is a shape failure, not
\ a silently skipped participant.
: FX-FOUR-SLOTS ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-INSTALL ( -- )" FX+
   s"    1 2 [: FX-SNAP ;] [: FX-PREP ;] [: FX-COMMIT ;]" FX+
   s"    [: FX-REL ;] GENERATED-DECL-OWNER:REGISTER ;" FX+
   s" ;package" FX+ ;

\ 7. two definitions of the release callback make the registration ambiguous.
: FX-DUPLICATE ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-REL ( -- ) 9 throw ;" FX+
   FX-REGISTER ;

\ 8. a deferred vector is followed through its binding site.
: FX-DEFER ( -- )
   FX-PREFIX
   s" defer FX-XT ( -- )" FX+
   s" : FX-BAD ( -- ) 9 throw ;" FX+
   s" : FX-BIND ( -- ) [: FX-BAD ;] is FX-XT ;" FX+
   s" : FX-REL ( -- ) FX-XT ;" FX+
   FX-REGISTER ;

\ 9. a name that merely contains a forbidden word is an ordinary call, and a
\ registration written inside a comment registers nothing.
: FX-SUBSTRING ( -- )
   FX-PREFIX
   s" : FX-THROWLESS ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-REL ( -- ) FX-THROWLESS ;" FX+
   s" \ [: FX-PREP ;] [: FX-PREP ;] [: FX-PREP ;] [: FX-PREP ;]" FX+
   s" \ [: FX-PREP ;] GENERATED-DECL-OWNER:REGISTER" FX+
   FX-REGISTER ;

\ 10. an unrecognised call is not admitted just because it looks harmless: the
\ allowlist is the whole admission rule.
: FX-UNKNOWN ( -- )
   FX-PREFIX
   s" : FX-REL ( -- ) FX-DEPTH @ SOME-UNSCANNED-WORD ;" FX+
   FX-REGISTER ;

\ 11. the release slot may name a deferred word, and then the roots are followed
\ the same way an interior reference is. Marking a root reachable and stopping
\ would admit a defer whose binding was never looked at.
: FX-DEFER-ROOT ( -- )
   FX-PREFIX
   s" defer FX-REL ( -- )" FX+
   s" : FX-BAD ( -- ) 9 throw ;" FX+
   s" : FX-BIND ( -- ) [: FX-BAD ;] is FX-REL ;" FX+
   FX-REGISTER ;

\ 12. a deferred word the closure reaches with no binding in the scanned sources
\ is a hole, not an absence of work.
: FX-UNBOUND ( -- )
   FX-PREFIX
   s" defer FX-XT ( -- )" FX+
   s" : FX-REL ( -- ) FX-XT ;" FX+
   FX-REGISTER ;

\ 13. `['] WORD is VECTOR` binds exactly what `[: WORD ;] is VECTOR` binds, so a
\ tick-form binding must be followed too. The good quotation binding is present
\ as well, so this fixture isolates the tick form from fixture 12.
: FX-TICK-BINDING ( -- )
   FX-PREFIX
   s" defer FX-XT ( -- )" FX+
   s" : FX-GOOD ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-BAD ( -- ) 9 throw ;" FX+
   s" : FX-BIND ( -- ) [: FX-GOOD ;] is FX-XT ['] FX-BAD is FX-XT ;" FX+
   s" : FX-REL ( -- ) FX-XT ;" FX+
   FX-REGISTER ;

\ 14. a binding whose target this scan cannot name - an execution token read out
\ of a cell - is recorded as opaque and reported, rather than dropped. The
\ residual limitation is narrower and is stated in the header: an xt that reaches
\ a release callback without passing through any `is` at all, for instance one
\ stored in a cell and run by `execute`, is outside what a source scan can see.
: FX-OPAQUE-BINDING ( -- )
   FX-PREFIX
   s" defer FX-XT ( -- )" FX+
   s" variable FX-CELL" FX+
   s" : FX-GOOD ( -- ) FX-DEPTH @ 1 - FX-DEPTH ! ;" FX+
   s" : FX-BIND ( -- ) [: FX-GOOD ;] is FX-XT FX-CELL @ is FX-XT ;" FX+
   s" : FX-REL ( -- ) FX-XT ;" FX+
   FX-REGISTER ;

: TEST-FIXTURES ( -- )
   FX-CLEAN 1 FX-RUN 0 T=
   RELEASE-INV:ROOTS 1 T=
   FX-THROWS 1 FX-RUN 1 T=
   FX-COMMENTS 1 FX-RUN 0 T=
   FX-STRING 1 FX-RUN 1 T=
   FX-WRONG-ROLE 1 FX-RUN 1 T=
   FX-FOUR-SLOTS 0 FX-RUN 1 T=
   RELEASE-INV:ROOTS 0 T=
   FX-DUPLICATE 1 FX-RUN 1 T=
   FX-DEFER 1 FX-RUN 1 T=
   FX-SUBSTRING 1 FX-RUN 0 T=
   RELEASE-INV:ROOTS 1 T=
   FX-UNKNOWN 1 FX-RUN 1 T=
   FX-DEFER-ROOT 1 FX-RUN 1 T=
   FX-UNBOUND 1 FX-RUN 1 T=
   FX-TICK-BINDING 1 FX-RUN 1 T=
   FX-OPAQUE-BINDING 1 FX-RUN 1 T= ;

public

: RUN ( -- )
   TEST-FIXTURES
   TEST-PRODUCTION
   T-REPORT ;

;package

DECL-RELEASE-INV-TEST:RUN
