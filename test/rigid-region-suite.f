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
\
\ They sit above the `0 set-check` window, and for the colon consumers that is
\ load-bearing: a colon definition's effect is registered only while checking is
\ on (RR-UEQ inside the window is published with no effect and seven cases
\ fail), while a `defer` registers its declared effect either way. The harness
\ needs the window because it reads checker-internal state with no charted
\ effect.
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

\ Region-generic view words, for a body whose own inputs declare a fresh region.
\ A mut-view is exclusive, so the two-view consumer is a `defer` (a body that
\ drops one is refused).
: RR-VPASS ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) ;
: RR-VTAKE ( [ mut-view<p,q,a,u8> -- ] -- ) drop ;
defer RR-VSAME ( mut-view<p,q,a,u8> mut-view<p,q,a,u8> -- )

\ The window holds the suspended hook on the stack. After the report the hook
\ resumes and diagnostics leave RR-DIAG, so a file loaded after this one in the
\ same process is checked and reports as usual.
check@ 0 set-check

T-RESET

\ Non-quiet candidate check that captures the rendered diagnostic into a buffer
\ (kept off stderr) so a reject's NAMED reason can be asserted.
create RR-DIAG 16384 allot
: RR-CHECK ( ptr u8 n -- n ) RR-DIAG 16384 DIAG-BUFFER! CHECK-CANDIDATE! ;
: RR-DIAG? ( ptr u8 n -- bool ) DIAG-BUFFER$ 2swap CONTAINS? ;

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
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (18) an input's region is not a new one: an output-only fresh region pairs
\ only with a region the body mints (CHECK-FRESH-OUTPUT).
s" C-FNEW ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-c,u8> ) RR-VPASS"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (19) an input's fresh region is not one the body mints.
s" C-FOWN ( matrix<space-global,fresh-region-b,f32,f32> -- ) RR-OWN RR-UONE"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
\ (20) a declared region quantifier cannot be specialized to a fresh region.
s" C-FQNT ( mut-view<p,q,a,u8> mut-view<p,q,fresh-region-b,u8> -- ) RR-VSAME"  RR-CHECK 0 T=
s" must stay parametric" RR-DIAG? -1 T=
\ (21) declared fresh identities keep their domains.
s" C-FDOM ( span<space-global,f32,fresh-region-b> span<space-global,f32,fresh-gen-b> -- ) RR-UBOX"  RR-CHECK 0 T=
s" rigid host: identity domain confusion" RR-DIAG? -1 T=
\ (22) a fresh region the body supplies, in a callback's input, is one the body
\ mints; handed an input's region instead, it is refused.
s" C-FMINT ( [ matrix<space-global,fresh-region-b,f32,f32> -- ] -- matrix<space-global,fresh-region-b,f32,f32> ) drop RR-OWN"  RR-CHECK -1 T=
s" C-FCBIN ( matrix<space-global,fresh-region-b,f32,f32> [ matrix<space-global,fresh-region-c,f32,f32> -- ] -- ) execute"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
s" fresh-region-c" RR-DIAG? -1 T=
\ (23) two fresh regions the body supplies are two allocations.
s" C-F2MINT ( -- matrix<space-global,fresh-region-b,f32,f32> matrix<space-global,fresh-region-c,f32,f32> ) RR-OWN dup"  RR-CHECK 0 T=
s" rigid host: region mismatch" RR-DIAG? -1 T=
s" fresh-region-c" RR-DIAG? -1 T=
\ (24) a body-supplied fresh name of another domain holds no identity a call
\ minted, and a declared quantifier does not become a body-supplied region.
s" C-FGCB ( [ span<space-global,f32,fresh-gen-b> -- ] -- ) RR-BOXG swap execute"  RR-CHECK 0 T=
s" C-FQCB ( matrix<space-global,a,f32,f32> [ matrix<space-global,fresh-region-b,f32,f32> -- ] -- ) execute"  RR-CHECK 0 T=
s" must stay parametric" RR-DIAG? -1 T=

T-REPORT
DIAG-BUFFER-OFF set-check
;package
