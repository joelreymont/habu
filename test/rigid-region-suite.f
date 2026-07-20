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
\ Modeling note: the checker binds a consumer var to at most a small number of
\ co-resident fresh atoms per family application, so each fixture carries the one
\ or two identities it exercises in a device family's early argument slots
\ (space-global stays concrete). The domains themselves are independent.

require test/checker-assert.f
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

\ Trusted host/index constructors and equality consumers. `space-global` is a
\ concrete device atom; the rigid identities sit in the following early slots.
s" RR-UEQ"  s" matrix<space-global,a,b,f32> matrix<space-global,a,b,f32> --" TRUST
s" RR-UONE" s" matrix<space-global,a,f32,f32> matrix<space-global,a,f32,f32> --" TRUST
s" RR-UBOX" s" span<space-global,f32,x> span<space-global,f32,x> --" TRUST
\ one call, two outputs sharing region AND generation
s" RR-SHARE" s" -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-g,f32>" TRUST
\ equal extent (shared), distinct region
s" RR-XRGN"  s" -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-b,fresh-extent-x,f32>" TRUST
\ shared region, distinct extent
s" RR-XEXT"  s" -- matrix<space-global,fresh-region-a,fresh-extent-x,f32> matrix<space-global,fresh-region-a,fresh-extent-y,f32>" TRUST
\ shared region, distinct generation
s" RR-XGEN"  s" -- matrix<space-global,fresh-region-a,fresh-gen-g,f32> matrix<space-global,fresh-region-a,fresh-gen-h,f32>" TRUST
\ region-only owner (each call is a fresh allocation)
s" RR-OWN"  s" -- matrix<space-global,fresh-region-a,f32,f32>" TRUST
\ region+extent host, one identity per call (used by the exhaustion probe)
s" RR-MK1"  s" -- matrix<space-global,fresh-region-a,fresh-extent-x,f32>" TRUST
\ a region and a generation carried in the SAME slot of two boxes
s" RR-BOXR" s" -- span<space-global,f32,fresh-region-a>" TRUST
s" RR-BOXG" s" -- span<space-global,f32,fresh-gen-a>" TRUST

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

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" rigid-region-suite: failures" 1 die ;
REPORT
