\ rigid-region-suite.f — checker fixtures for the rigid host-allocation identity
\ domains (dot habu-define-rigid-host). A host allocation is stamped by three
\ FRESH, RIGID, monotonic-nonreuse identities that `ptr T` and ordinary type
\ variables cannot name: its host REGION (which allocation), its EXTENT (bounds),
\ and its mutation GENERATION (epoch). Each is a `fresh-region-*`/`fresh-extent-*`/
\ `fresh-gen-*` template atom minted per constructor CALL and shared across that
\ call's outputs, so two accesses to ONE allocation certify while two allocations
\ (or a recreated owner, or a stale generation) reject with a NAMED reason.
\
\ Run standalone:  bin/hb < test/rigid-region-suite.f   (exit 0 + "ok"; dies on miss)
\ Routed positive case in test/candidate-validation.f.
\
\ Modeling note: consumer type-var binding across 3+ co-resident fresh atoms in
\ one family application is SOUND — a matrix carrying region+extent+generation on
\ one owner certifies pairwise (case 8) and rejects each domain mismatch by name
\ (cases 9-11). The one real constraint on an identity variable is its SPELLING:
\ the single letters n/f/r are the reserved int/bool/float cons (CT-INIT /
\ TOK-TYPE checker.f), resolved as concrete types BEFORE the single-letter type-
\ var branch, so `r` (the natural mnemonic for a region) is silently the float
\ type and never binds a fresh-region atom (case 12). Identity vars must avoid
\ n/f/r; the domains themselves are independent, and space-global stays concrete.

require test/checker-assert.f

\ The whole suite is one package, and that is what lets the fixture words below
\ be DEFINITIONS at all. A test file may not publish new global names (the
\ package diff lint reports every one), and the abstract boundaries this suite
\ invents have to be reachable by BARE name from the candidates it hands the
\ checker - a candidate is certified with this package still open, so bare
\ lookup finds the private tails here before it reaches the global wordlist.
\ Nothing is exported: the report is called below, inside the package.
package RIGID-REGION

\ Trusted host/index constructors and equality consumers. `space-global` is a
\ concrete device atom; the rigid identities sit in the following early slots.
\
\ THEY ARE `TRUSTED:` DEFINITIONS AND NOT `trust` ROWS. Each one is an abstract
\ boundary this suite invents - a producer of a rigid matrix out of nothing has
\ no checked body, which is the whole reason it is declared rather than written.
\ A bare row would ASSERT that the word already exists somewhere, and nothing
\ defines these; that claim is now refused at the row (src/core/checker.f
\ TRUST-RESOLVES?, dot habu-make-trust-refuse-cc8e19de). `TRUSTED:` says exactly
\ what was meant all along - here is a word, here is its effect, its body is not
\ checkable - and the stub bodies are never run: every case below certifies a
\ CANDIDATE against these effects and none of them executes.
\
\ THEY SIT ABOVE THE `0 set-check` WINDOW, AND THAT POSITION IS LOAD-BEARING.
\ A declared signature is registered with the checker by the definer's own
\ publish tail (src/habu/habu2.f EM-COMPILE-PUBLISH, which reaches
\ EM-COMPILE-PUBLISH-TRUSTED -> DEF-TRUST:REGISTER only when HOOK-CELL is
\ non-zero). `0 set-check` zeroes that cell, so a `TRUSTED:` inside the window
\ publishes the WORD and registers NO EFFECT, and every case below then answers
\ 1 (uncheckable) instead of -1/0. A bare `trust` row was immune because it is
\ an ordinary call that records the row itself. The harness below still needs
\ the window - it reads checker-internal state that has no charted effect - so
\ the declarations moved above it rather than the window moving.
TRUSTED: RR-UEQ ( matrix<space-global,a,b,f32> matrix<space-global,a,b,f32> -- ) drop drop ;
TRUSTED: RR-UONE ( matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> -- ) drop drop ;
TRUSTED: RR-UBOX ( span<space-global,f32,x> span<space-global,f32,x> -- ) drop drop ;
\ one call, two outputs sharing region AND generation
TRUSTED: RR-SHARE ( -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-g,f32> ) 0 0 ;
\ equal extent (shared), distinct region
TRUSTED: RR-XRGN ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-b,fresh-extent-x,f32> ) 0 0 ;
\ shared region, distinct extent
TRUSTED: RR-XEXT ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-a,fresh-extent-y,f32> ) 0 0 ;
\ shared region, distinct generation
TRUSTED: RR-XGEN ( -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-h,f32> ) 0 0 ;
\ region-only owner (each call is a fresh allocation)
TRUSTED: RR-OWN ( -- matrix<space-global,fresh-region-a,f32,f32> ) 0 ;
\ region+extent host, one identity per call (used by the exhaustion probe)
TRUSTED: RR-MK1 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> ) 0 ;
\ a region and a generation carried in the SAME slot of two boxes
TRUSTED: RR-BOXR ( -- span<space-global,f32,fresh-region-a> ) 0 ;
TRUSTED: RR-BOXG ( -- span<space-global,f32,fresh-gen-a> ) 0 ;
\ a mask identity (fresh-mask-*): the LEGACY shared RIGID-FRESH domain, the
\ catch-all every non-region/extent/gen fresh atom still mints from.
TRUSTED: RR-BOXM ( -- span<space-global,f32,fresh-mask-a> ) 0 ;
\ one call, two outputs sharing ONE mask id (the equal-id ⇒ unify anchor).
TRUSTED: RR-SHM ( -- span<space-global,f32,fresh-mask-a> span<space-global,f32,fresh-mask-a> ) 0 0 ;

\ THREE co-resident identities on ONE owner (region+extent+generation), the shape
\ habu-add-unique-bounded needs. Consumer binds all three with non-reserved vars
\ a/b/c; each producer is one allocation whose two outputs share all three ids.
\ (TRUSTED probe rows owner = habu-add-bounded-host-b40b048f.)
TRUSTED: RR-UEQ3 ( matrix<space-global,a,b,c> matrix<space-global,a,b,c> -- ) drop drop ;
TRUSTED: RR-SHARE3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> ) 0 0 ;
TRUSTED: RR-XRGN3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-b,fresh-extent-x,fresh-gen-g> ) 0 0 ;
TRUSTED: RR-XEXT3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-y,fresh-gen-g> ) 0 0 ;
TRUSTED: RR-XGEN3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-h> ) 0 0 ;
\ SAME shape as RR-UEQ3 but the region slot is spelled `r` — the reserved FLOAT con,
\ not a type var, so it can never bind a fresh-region atom (the sole reason the
\ >=3-identity owner looked "unbindable"; the fix is a non-reserved letter).
TRUSTED: RR-U3R ( matrix<space-global,r,b,c> matrix<space-global,r,b,c> -- ) drop drop ;

0 set-check

variable #FAIL
variable #CASE
: T-FAIL ( -- ) [char] F emit #CASE @ . cr  #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if T-FAIL then ;

\ Non-quiet candidate check that captures the rendered diagnostic into a buffer
\ (kept off stderr) so a reject's NAMED reason can be asserted.
create RR-DIAG 16384 allot
: RR-CHECK ( ptr u8 n -- n ) RR-DIAG 16384 DIAG-BUFFER! CHECK-CANDIDATE! ;
: RR-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   nu 0= if -1 exit then
   hu nu < if 0 exit then
   0 begin dup hu nu - <= while
      ha over + nu  na nu  CORE-STR= if drop -1 exit then
      1 +
   repeat drop 0 ;
: RR-DIAG? ( ptr u8 n -- bool ) DIAG-BUFFER$ 2swap RR-CONTAINS? ;

\ Exhaustion probe: shrink the shared domain bound so the SECOND region mint of
\ this check wraps — it must throw E-RIGID-EXHAUST (7140) instead of reusing id 1.
variable RR-EC
: RR-WRAP ( -- )
   RIGID-MAX @  2 RIGID-MAX !
   [: s" RRW ( -- ) RR-MK1 drop RR-MK1 drop" CHECK-CANDIDATE! drop ;] catch RR-EC !
   RIGID-MAX ! ;
\ Same probe for the LEGACY shared RIGID-FRESH counter (fresh-mask-* and every
\ non-domain fresh atom mint from it): two mask mints in one check, bound to 2,
\ so the second mint must throw E-RIGID-EXHAUST instead of wrapping to a live id.
: RR-WRAPM ( -- )
   RIGID-MAX @  2 RIGID-MAX !
   [: s" RRWM ( -- ) RR-BOXM drop RR-BOXM drop" CHECK-CANDIDATE! drop ;] catch RR-EC !
   RIGID-MAX ! ;

\ (1) two accesses within one region+generation certify.
s" C-CERT ( -- ) RR-SHARE RR-UEQ"  RR-CHECK -1 T=
\ (2) equal-sized cross-region unification rejects — named region mismatch.
s" C-XRGN ( -- ) RR-XRGN RR-UEQ"   RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (3) extent mismatch rejects — named extent mismatch.
s" C-XEXT ( -- ) RR-XEXT RR-UEQ"   RR-CHECK 0 T=
s" rigid host: extent mismatch" RR-DIAG? -1 T=
\ (4) generation mismatch rejects — named stale generation.
s" C-XGEN ( -- ) RR-XGEN RR-UEQ"   RR-CHECK 0 T=
s" rigid host: stale mutation generation" RR-DIAG? -1 T=
\ (5) numeric-handle authority: a region id and a generation id that are
\ NUMERICALLY EQUAL (both the first mint of their domain) must NOT unify.
s" C-XDOM ( -- ) RR-BOXR RR-BOXG RR-UBOX"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? -1 T=
\ (6) recreated-owner reuse rejects — a fresh allocation gets a fresh region.
s" C-REUSE ( -- ) RR-OWN RR-OWN RR-UONE"   RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (7) exhaustion-before-wrap: the domain counter throws rather than reusing an id.
RR-WRAP  RR-EC @ E-RIGID-EXHAUST T=
\ (8) THREE co-resident identities bind: two accesses to one owner certify — the
\ >=3-fresh-atom case is SOUND, no unifier limit.
s" C-CERT3 ( -- ) RR-SHARE3 RR-UEQ3"  RR-CHECK -1 T=
\ (9) region mismatch among three identities still rejects, named region.
s" C-XRGN3 ( -- ) RR-XRGN3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (10) extent mismatch among three identities rejects, named extent.
s" C-XEXT3 ( -- ) RR-XEXT3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: extent mismatch" RR-DIAG? -1 T=
\ (11) generation mismatch among three identities rejects, named stale generation.
s" C-XGEN3 ( -- ) RR-XGEN3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: stale mutation generation" RR-DIAG? -1 T=
\ (12) reserved-letter footgun: RR-U3R is C-CERT3's shape with the region var
\ spelled `r`. `r` parses as the float con, so it can't bind the fresh-region
\ atom and the SAME allocation now rejects — the naming trap, not a binding limit.
s" C-RSVD3 ( -- ) RR-SHARE3 RR-U3R"    RR-CHECK 0 T=
\ (13) LEGACY mask domain, equal-id anchor: one call's two outputs share ONE
\ mask id, so the equal-id consumer certifies — the mask domain unifies ON id.
s" C-MSK= ( -- ) RR-SHM RR-UBOX"           RR-CHECK -1 T=
\ (14) two SEPARATE mask allocations get distinct ids and reject: no wrap-reuse,
\ no false unify. A wrapped id colliding with the still-live first would flip
\ THIS reject to a certify — the exact soundness hole the guard closes.
s" C-MSKX ( -- ) RR-BOXM RR-BOXM RR-UBOX"  RR-CHECK 0 T=
\ (15) exhaustion-before-wrap on the LEGACY RIGID-FRESH counter: the shared
\ counter throws E-RIGID-EXHAUST rather than wrapping into a reused id, the
\ same guard the per-domain counters got (case 7).
RR-WRAPM  RR-EC @ E-RIGID-EXHAUST T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" rigid-region-suite: failures" 1 die ;
REPORT
;package
