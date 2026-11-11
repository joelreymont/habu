\ rigid-region-suite.f - checker fixtures for the rigid host-allocation identity
\ domains: a host allocation's REGION, EXTENT and GENERATION are fresh atoms
\ (fresh-region-*, fresh-extent-*, fresh-gen-*) minted per constructor call and
\ shared across that call's outputs, which `ptr T` and ordinary type variables
\ cannot name. Two accesses to one allocation certify; two allocations, a
\ recreated owner or a stale generation reject with a named reason.

\ Run standalone:  bin/hb < test/rigid-region-suite.f   (exit 0 + "test: ok"; dies on miss)

require src/lib/string.f
require src/lib/test.f

\ The whole suite is one package, and that is what lets the fixture words below
\ be DEFINITIONS at all. A test file may not publish new global names (the
\ package diff lint reports every one), and the abstract boundaries this suite
\ invents have to be reachable by BARE name from the candidates it hands the
\ checker - a candidate is certified with this package still open, so bare
\ lookup finds the private tails here before it reaches the global wordlist.
\ Nothing is exported: the report is called below, inside the package.
package RIGID-REGION

\ Host/index constructors and equality consumers. `space-global` is a concrete
\ device atom; the rigid identities sit in the following early slots.
\
\ The constructors are `defer` declarations: each is an abstract boundary this
\ suite invents, with no checked body. A `trust` row would claim the word
\ exists, which src/core/checker.f TRUST-RESOLVES? refuses; a `defer` states the
\ word and its effect with no implementation. None is bound or run: every case
\ certifies a candidate, and a call would die with `defer: unset execution
\ vector` (rc 76).
: RR-UEQ ( matrix<space-global,a,b,f32> matrix<space-global,a,b,f32> -- ) drop drop ;
: RR-UONE ( matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> -- ) drop drop ;
: RR-UBOX ( span<space-global,f32,x> span<space-global,f32,x> -- ) drop drop ;
\ one call, two outputs sharing region AND generation
defer RR-SHARE ( -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-g,f32> )
\ equal extent (shared), distinct region
defer RR-XRGN ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-b,fresh-extent-x,f32> )
\ shared region, distinct extent
defer RR-XEXT ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-a,fresh-extent-y,f32> )
\ shared region, distinct generation
defer RR-XGEN ( -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-h,f32> )
\ region-only owner (each call is a fresh allocation)
defer RR-OWN ( -- matrix<space-global,fresh-region-a,f32,f32> )
\ region+extent host, one identity per call (used by the exhaustion probe)
defer RR-MK1 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> )
\ a region and a generation carried in the SAME slot of two boxes
defer RR-BOXR ( -- span<space-global,f32,fresh-region-a> )
defer RR-BOXG ( -- span<space-global,f32,fresh-gen-a> )
\ a mask identity (fresh-mask-*): the shared RIGID-FRESH domain every fresh atom
\ other than region, extent and generation mints from.
defer RR-BOXM ( -- span<space-global,f32,fresh-mask-a> )
\ one call, two outputs sharing ONE mask id (the equal-id ⇒ unify anchor).
defer RR-SHM ( -- span<space-global,f32,fresh-mask-a> span<space-global,f32,fresh-mask-a> )

\ Three co-resident identities on one owner (region, extent, generation); the
\ consumer binds them with non-reserved vars a/b/c, and each producer is one
\ allocation whose two outputs share all three ids.
: RR-UEQ3 ( matrix<space-global,a,b,c> matrix<space-global,a,b,c> -- ) drop drop ;
defer RR-SHARE3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> )
defer RR-XRGN3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-b,fresh-extent-x,fresh-gen-g> )
defer RR-XEXT3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-y,fresh-gen-g> )
defer RR-XGEN3 ( -- matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-g> matrix<space-global,fresh-region-a,fresh-extent-x,fresh-gen-h> )
\ RR-UEQ3's shape with the region slot spelled `r`: the single letters n, f and
\ r are the reserved int, bool and float cons (checker.f CT-INIT, TOK-TYPE),
\ resolved before the type-variable branch, so `r` never binds a fresh-region
\ atom. Identity variables avoid n, f and r.
: RR-U3R ( matrix<space-global,r,b,c> matrix<space-global,r,b,c> -- ) drop drop ;

\ Region-generic words, for a body whose own inputs declare a fresh region. A
\ mut-view is exclusive, so the two-view consumer is a `defer` (a body that drops
\ one is refused).
: RR-MPASS ( matrix<space-global,a,f32,f32> -- matrix<space-global,a,f32,f32> ) ;
: RR-VPASS ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) ;
: RR-VTAKE ( [ mut-view<p,q,a,u8> -- ] -- ) drop ;
defer RR-VSAME ( mut-view<p,q,a,u8> mut-view<p,q,a,u8> -- )

\ Words with fresh inputs: a caller hands each the allocation it holds.
defer RR-MFRESH ( matrix<space-global,fresh-region-b,f32,f32> -- matrix<space-global,fresh-region-b,f32,f32> )
defer RR-MF2 ( matrix<space-global,fresh-region-b,f32,f32> matrix<space-global,fresh-region-c,f32,f32> -- )
defer RR-MMIX ( matrix<space-global,a,f32,f32> matrix<space-global,fresh-region-b,f32,f32> -- )
defer RR-SFRESH ( span<space-global,f32,fresh-region-b> -- )
defer RR-GFRESH ( span<space-global,f32,fresh-gen-b> -- )
\ A mut-view's third argument is a region, so a caller's quantifier there may
\ be passed to a fresh input; a matrix's is any type.
defer RR-VFRESH ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-b,u8> )
defer RR-VMF2 ( mut-view<p,q,fresh-region-b,u8> matrix<space-global,fresh-region-c,f32,f32> -- )
\ An inferred word that forwards its two inputs to RR-MF2's fresh inputs.
: RR-FWD2 RR-MF2 ;

\ Region-generic callees of a quotation, and one whose quotation's output region
\ is its own fresh input's.
: RR-QTW ( [ -- matrix<space-global,a,f32,f32> ] -- matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> ) dup execute swap execute ;
: RR-QPAIR ( matrix<space-global,a,f32,f32> [ -- matrix<space-global,a,f32,f32> ] -- matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> ) execute ;
: RR-QTWX ( [ -- x ] -- x x ) dup execute swap execute ;
: RR-QRUN ( [ -- x ] -- x ) execute ;
: RR-QTAKE ( [ matrix<space-global,a,f32,f32> -- matrix<space-global,a,f32,f32> ] -- ) drop ;
defer RR-QB ( matrix<space-global,fresh-region-b,f32,f32> [ -- matrix<space-global,fresh-region-b,f32,f32> ] -- matrix<space-global,fresh-region-b,f32,f32> matrix<space-global,fresh-region-b,f32,f32> )
\ A word that mints an allocation, hands it to a callback and returns it
\ (ALLOC-DISPOSE's shape), and a region-generic callee that applies a callback.
defer RR-ADISP ( [ matrix<space-global,fresh-region-b,f32,f32> -- ] -- matrix<space-global,fresh-region-b,f32,f32> )
: RR-QAPP ( matrix<space-global,a,f32,f32> [ matrix<space-global,a,f32,f32> -- matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> ] -- matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> ) execute ;
\ A word and a region-generic callee whose region leaves only through what the
\ callback it is handed returns.
defer RR-GIVEX ( [ matrix<space-global,fresh-region-b,f32,f32> -- x ] -- x )
: RR-APPX ( matrix<space-global,a,f32,f32> [ matrix<space-global,a,f32,f32> -- x ] -- x ) execute ;
\ Words whose region sits two quotations deep: in what a callback is handed,
\ and in what a returned quotation returns.
defer RR-GIVE2 ( [ [ -- matrix<space-global,fresh-region-b,f32,f32> ] -- x ] -- x )
defer RR-GIVEP ( [ [ -- matrix<space-global,fresh-region-a,f32,f32> ] -- x y ] -- x y )
defer RR-MKQF ( -- [ -- matrix<space-global,fresh-region-b,f32,f32> ] )
\ An inferred word that returns a quotation minting a region.
: RR-MKL [: RR-OWN ;] ;

T-RESET

\ Non-quiet candidate check that captures the rendered diagnostic into a buffer
\ (kept off stderr) so a reject's NAMED reason can be asserted.
create RR-DIAG 16384 allot
\ The cases read prose, and a run under `tools/check.f --json-errors` starts in
\ JSON.
variable RR-JSON-WAS   JSON-DIAGS @ RR-JSON-WAS !   false DIAG-JSON!
: RR-CHECK ( ptr u8 n -- n ) RR-DIAG 16384 DIAG-BUFFER! CHECK-CANDIDATE! ;
: RR-DIAG? ( ptr u8 n -- bool ) DIAG-BUFFER$ 2swap CONTAINS? ;

\ A refused does> clause's code: test/certify-does-definer.f CDD-RES-BAD.
: RR-REFUSED ( -- n ) tier@ 0= IF 70 ELSE E-COMP-VERDICT THEN ;

\ Exhaustion probe: shrink the shared domain bound so the SECOND region mint of
\ this check wraps — it must throw E-RIGID-EXHAUST (7140) instead of reusing id 1.
variable RR-EC
: RR-WRAP ( -- )
   RIGID-MAX @  2 RIGID-MAX !
   [: s" RRW ( -- ) RR-MK1 drop RR-MK1 drop" CHECK-CANDIDATE! drop ;] catch RR-EC !
   RIGID-MAX ! ;
\ Same probe for the shared RIGID-FRESH counter (fresh-mask-* and every
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
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (3) extent mismatch rejects — named extent mismatch.
s" C-XEXT ( -- ) RR-XEXT RR-UEQ"   RR-CHECK 0 T=
s" rigid host: extent mismatch" RR-DIAG? TTRUE
\ (4) generation mismatch rejects — named stale generation.
s" C-XGEN ( -- ) RR-XGEN RR-UEQ"   RR-CHECK 0 T=
s" rigid host: stale mutation generation" RR-DIAG? TTRUE
\ (5) numeric-handle authority: a region id and a generation id that are
\ NUMERICALLY EQUAL (both the first mint of their domain) must NOT unify.
s" C-XDOM ( -- ) RR-BOXR RR-BOXG RR-UBOX"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
\ (6) recreated-owner reuse rejects — a fresh allocation gets a fresh region.
s" C-REUSE ( -- ) RR-OWN RR-OWN RR-UONE"   RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (7) exhaustion-before-wrap: the domain counter throws rather than reusing an id.
RR-WRAP  RR-EC @ E-RIGID-EXHAUST T=
\ (8) THREE co-resident identities bind: two accesses to one owner certify — the
\ >=3-fresh-atom case is SOUND, no unifier limit.
s" C-CERT3 ( -- ) RR-SHARE3 RR-UEQ3"  RR-CHECK -1 T=
\ (9) region mismatch among three identities still rejects, named region.
s" C-XRGN3 ( -- ) RR-XRGN3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (10) extent mismatch among three identities rejects, named extent.
s" C-XEXT3 ( -- ) RR-XEXT3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: extent mismatch" RR-DIAG? TTRUE
\ (11) generation mismatch among three identities rejects, named stale generation.
s" C-XGEN3 ( -- ) RR-XGEN3 RR-UEQ3"    RR-CHECK 0 T=
s" rigid host: stale mutation generation" RR-DIAG? TTRUE
\ (12) RR-U3R is C-CERT3's shape with the region variable spelled `r`, the float
\ con, so the same allocation rejects: the naming trap, not a binding limit.
s" C-RSVD3 ( -- ) RR-SHARE3 RR-U3R"    RR-CHECK 0 T=
\ (13) mask domain, equal-id anchor: one call's two outputs share ONE
\ mask id, so the equal-id consumer certifies — the mask domain unifies ON id.
s" C-MSK= ( -- ) RR-SHM RR-UBOX"           RR-CHECK -1 T=
\ (14) two SEPARATE mask allocations get distinct ids and reject: no wrap-reuse,
\ no false unify. A wrapped id colliding with the still-live first would flip
\ THIS reject to a certify — the exact soundness hole the guard closes.
s" C-MSKX ( -- ) RR-BOXM RR-BOXM RR-UBOX"  RR-CHECK 0 T=
\ (15) exhaustion-before-wrap on the shared RIGID-FRESH counter: it throws
\ E-RIGID-EXHAUST rather than wrapping into a reused id, as case 7 does for the
\ per-domain counters.
RR-WRAPM  RR-EC @ E-RIGID-EXHAUST T=
\ (16) a body holds a fresh region its inputs declare as one identity: it passes
\ to a region-generic word, directly or as a callback's input, and one name in
\ two inputs is one region.
s" C-FPASS ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-b,u8> ) RR-VPASS"  RR-CHECK -1 T=
s" C-FCB ( [ mut-view<p,q,fresh-region-b,u8> -- ] -- ) RR-VTAKE"  RR-CHECK -1 T=
s" C-FSELF ( mut-view<p,q,fresh-region-b,u8> mut-view<p,q,fresh-region-b,u8> -- ) RR-VSAME"  RR-CHECK -1 T=
\ (17) two declared fresh regions are two allocations.
s" C-F2 ( mut-view<p,q,fresh-region-b,u8> mut-view<p,q,fresh-region-c,u8> -- ) RR-VSAME"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (18) an input's region is not a new one: an output-only fresh region pairs
\ only with a region the body mints (CHECK-FRESH-OUTPUT).
s" C-FNEW ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-c,u8> ) RR-VPASS"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (19) an input's fresh region is not one the body mints.
s" C-FOWN ( matrix<space-global,fresh-region-b,f32,f32> -- ) RR-OWN RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (20) a declared region quantifier cannot be specialized to a fresh region.
s" C-FQNT ( mut-view<p,q,a,u8> mut-view<p,q,fresh-region-b,u8> -- ) RR-VSAME"  RR-CHECK 0 T=
s" must stay parametric" RR-DIAG? TTRUE
\ (21) declared fresh identities keep their domains.
s" C-FDOM ( span<space-global,f32,fresh-region-b> span<space-global,f32,fresh-gen-b> -- ) RR-UBOX"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
\ (22) a fresh region the body supplies, in a callback's input, is one the body
\ mints; handed an input's region instead, it is refused.
s" C-FMINT ( [ matrix<space-global,fresh-region-b,f32,f32> -- ] -- matrix<space-global,fresh-region-b,f32,f32> ) drop RR-OWN"  RR-CHECK -1 T=
s" C-FCBIN ( matrix<space-global,fresh-region-b,f32,f32> [ matrix<space-global,fresh-region-c,f32,f32> -- ] -- ) execute"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" fresh-region-c" RR-DIAG? TTRUE
\ (23) two fresh regions the body supplies are two allocations.
s" C-F2MINT ( -- matrix<space-global,fresh-region-b,f32,f32> matrix<space-global,fresh-region-c,f32,f32> ) RR-OWN dup"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" fresh-region-c" RR-DIAG? TTRUE
\ (24) a body-supplied fresh name of another domain holds no identity a call
\ minted, and a declared quantifier does not become a body-supplied region.
s" C-FGCB ( [ span<space-global,f32,fresh-gen-b> -- ] -- ) RR-BOXG swap execute"  RR-CHECK 0 T=
s" C-FQCB ( matrix<space-global,a,f32,f32> [ matrix<space-global,fresh-region-b,f32,f32> -- ] -- ) execute"  RR-CHECK 0 T=
s" must stay parametric" RR-DIAG? TTRUE
\ (25) a does> clause holds fresh regions as a colon body does: (16)'s pass is
\ admitted and takes a checked caller's allocation, a minted output region
\ reaches a checked caller, and (18)'s new output region is refused.
s" : RR-DFPASS ( n -- ) create , does> ( matrix<space-global,fresh-region-b,f32,f32> -- matrix<space-global,fresh-region-b,f32,f32> ) drop RR-MPASS ;"  TEST-EVAL:RC  0 T=
s" 0 RR-DFPASS RR-DFP"  TEST-EVAL:RC  0 T=
s" : RR-DFCALL ( -- matrix<space-global,fresh-region-c,f32,f32> ) RR-OWN RR-DFP ;"  TEST-EVAL:RC  0 T=
s" : RR-DFOWN ( n -- ) create , does> ( -- matrix<space-global,fresh-region-b,f32,f32> ) drop RR-OWN ;"  TEST-EVAL:RC  0 T=
s" 0 RR-DFOWN RR-DFO"  TEST-EVAL:RC  0 T=
s" : RR-DFUSE ( -- matrix<space-global,fresh-region-b,f32,f32> ) RR-DFO ;"  TEST-EVAL:RC  0 T=
RR-DIAG 16384 DIAG-BUFFER!
s" : RR-DFNEW ( n -- ) create , does> ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-c,u8> ) drop RR-VPASS ;"  TEST-EVAL:RC  RR-REFUSED T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ A clause passes a caller's quantifier to a fresh input as a colon body does
\ (30): not a generation's, nor a region slot's without a region kind.
RR-DIAG 16384 DIAG-BUFFER!
s" : RR-DFGEN ( n -- ) create , does> ( span<space-global,f32,a> -- ) drop RR-GFRESH ;"  TEST-EVAL:RC  RR-REFUSED T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
RR-DIAG 16384 DIAG-BUFFER!
s" : RR-DFANY ( n -- ) create , does> ( matrix<space-global,a,f32,f32> -- matrix<space-global,a,f32,f32> ) drop RR-MFRESH ;"  TEST-EVAL:RC  RR-REFUSED T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
\ (26) a call's fresh input is the allocation its caller passes, and so is the
\ name on its output: the caller's own allocation, its input, its quantifier.
s" C-CALLOWN ( -- ) RR-OWN dup RR-MFRESH RR-UONE"  RR-CHECK -1 T=
s" C-CALLREC ( matrix<space-global,fresh-region-b,f32,f32> -- matrix<space-global,fresh-region-b,f32,f32> ) RECURSE"  RR-CHECK -1 T=
s" C-CALLGEN ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) RR-VFRESH"  RR-CHECK -1 T=
\ (27) two fresh inputs take two allocations; a quantifier beside a fresh input
\ may take its allocation, since a body admits nothing from the two differing.
s" C-CALL2 ( -- ) RR-OWN RR-OWN RR-MF2"  RR-CHECK -1 T=
s" C-CALLMIX ( -- ) RR-OWN dup RR-MMIX"  RR-CHECK -1 T=
\ (28) one allocation into two fresh inputs is refused, and so is a caller's
\ quantifier, which may be the other input's allocation.
s" C-CALLSAME ( -- ) RR-OWN dup RR-MF2"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-CALLQ2 ( mut-view<p,q,a,u8> -- ) RR-OWN RR-VMF2"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (29) an inferred input cannot hold an allocation its body made: no caller has it.
s" C-INFOWN RR-OWN RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (30) a fresh input takes an identity of its own domain only, as (21): not a
\ generation, nor a caller's quantifier unless a region kind makes it a region.
s" C-CALLXDOM ( -- ) RR-BOXG RR-SFRESH"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
s" C-CALLANY ( matrix<space-global,a,f32,f32> -- matrix<space-global,a,f32,f32> ) RR-MFRESH"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
s" C-CALLQGEN ( span<space-global,f32,a> -- ) RR-GFRESH"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? TTRUE
\ (31) an argument whose type is an undeclared variable takes a new identity, so
\ an inferred forwarder has fresh inputs: two allocations, never one.
s" C-INFFWD ( -- ) RR-OWN RR-OWN RR-FWD2"  RR-CHECK -1 T=
s" C-INFFWD1 ( -- ) RR-OWN dup RR-FWD2"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-INFDUP dup RR-MF2"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
\ (32) a quotation's fresh region, new at each execution, at a direct call, a tick,
\ recursion and an inferred effect (forth.md "Rigid host-allocation identity domains").
s" C-QONCE ( -- ) [: RR-OWN ;] execute RR-MFRESH drop"  RR-CHECK -1 T=
s" C-QSHARE ( -- ) [: RR-OWN dup ;] execute RR-UONE"  RR-CHECK -1 T=
s" C-QDUP2 ( -- ) [: RR-OWN ;] dup execute swap execute RR-MF2"  RR-CHECK -1 T=
s" C-QHELP ( -- ) [: [: dup ;] RR-OWN over execute drop ;] execute swap execute 2drop"  RR-CHECK -1 T=
s" C-QTAKE ( -- ) [: RR-MFRESH ;] RR-QTAKE"  RR-CHECK -1 T=
s" C-QDIR ( -- ) [: RR-OWN ;] dup execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QTICK ( -- ) ['] RR-OWN dup execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QLOCAL ( -- ) [: RR-OWN ;] {: q :} q execute q execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QRS ( -- ) [: RR-OWN ;] >r r@ execute r> execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QNEST ( -- ) [: [: RR-OWN ;] ;] execute dup execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QPAIR ( -- ) [: RR-OWN ;] dup execute swap RR-QPAIR RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QB1 ( -- ) [: RR-OWN ;] dup execute swap RR-QB RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QTW ( -- ) [: RR-OWN ;] RR-QTW RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QTWX ( -- ) [: RR-OWN ;] RR-QTWX RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QRUN2 ( -- ) [: RR-OWN ;] dup RR-QRUN swap RR-QRUN RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBTICK ( -- ) ['] RR-ADISP dup [: drop ;] swap execute swap [: drop ;] swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBLIT ( -- ) [: RR-OWN swap RR-QAPP drop ;] dup [: dup ;] swap execute swap [: dup ;] swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBINTICK ( -- ) ['] RR-GIVEX dup [: ;] swap execute swap [: ;] swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBINLIT ( -- ) [: RR-OWN swap RR-APPX ;] dup [: ;] swap execute swap [: ;] swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBGEN ( -- ) ['] RR-QTWX RR-GIVEP RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QCBREC ( [ [ -- matrix<space-global,fresh-region-a,f32,f32> ] -- x y ] -- x y ) ['] RR-QTWX RECURSE RR-UONE RECURSE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QDEEPTICK ( -- ) ['] RR-GIVE2 dup ['] RR-QRUN swap execute swap ['] RR-QRUN swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QRETTICK ( -- ) ['] RR-MKQF dup execute swap execute execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QRETTWICE ( -- ) RR-MKQF dup execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QINFTWICE ( -- ) RR-MKL dup execute swap execute RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? TTRUE
s" C-QFRESHOUT ( -- [ -- matrix<space-global,fresh-region-b,f32,f32> ] ) [: RR-OWN ;]"  RR-CHECK -1 T=

T-REPORT
\ Diagnostics leave RR-DIAG in the format found, so a file loaded after this one
\ in the same process reports as usual.
DIAG-BUFFER-OFF  RR-JSON-WAS @ JSON-DIAGS !
;package
