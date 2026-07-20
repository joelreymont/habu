\ extent-product-test.f — BTC-7 permanent regression for the checker half of the
\ extent-role product/factorization capability (dot habu-extent-role-product-
\ 8e364885, docs/batch-sequence-design.md §5 BTC-7, docs/extent-substrate.md). The
\ standalone decision-record probe (test/extent-substrate-probe.f) demonstrated the
\ CHOSEN substrate on raw stand-in families; this routes the permanent gate
\ regression over the REAL built-in `extprod`/`redx` families and the free-vs-inner
\ contraction rule. The candidate-B SURFACE half (EXTPROD:/>RED, the generated
\ fold/split/join, the E-EXT-FACTOR value reject) is exercised maki-side in
\ maki/extent-test.f — the one-way habu<-maki dependency guard forbids a test/ file
\ from importing maki/extent.f, so the surface fixtures live where the surface does.
\
\ Soundness goal: a cross-sequence contraction (+Σ that sums the batch axis of a
\ folded (B,T) row) is UNREPRESENTABLE in a checked program. The folded row is typed
\ over the ordered product former extprod<free,inner>; a contraction accepts an inner
\ extent (#T, #k) and REJECTS the free #B or the whole product. redx<E> names a
\ contraction axis; the checker rejects it forming over a free extent or a product.
\
\ Stand-ins (like the probe, since ix/#B live in maki): xidx = the ix index family,
\ xpb = free/batch #B, xpt = in-block/sequence #T, xpk = a head-dim inner extent,
\ xpflat = a plain flat folded-rows extent. extprod/redx are the real families.
\ CHECK-QUIET-CANDIDATE! : -1 accept, 0 reject. Routed positive suite: exit 0, empty
\ stderr, stdout ends "ok\n" (REPORT dies exit 1 on any miss).

require test/checker-assert.f

create EP-DIAG 8192 allot
EP-DIAG 8192 DIAG-BUFFER!

variable #FAIL
variable #CASE
: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

TYPEFAMILY xidx 1
TYPEFAMILY xpb 0
TYPEFAMILY xpt 0
TYPEFAMILY xpk 0
TYPEFAMILY xpflat 0

\ mark the free (outer / batch) factor, exactly as EXTPROD: does at declaration.
s" xpb" EXT-MARK-FREE-TAIL

\ --- (a) product former: a folded row index is the ordered product type ----------
s" XA  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpb,xpt>> )" CHECK-QUIET-CANDIDATE! -1 T=

\ --- (e) mismatched factor: <#B,#T> is distinct from <#B,#K> ----------------------
s" XF  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpb,xpk>> )"  CHECK-QUIET-CANDIDATE!  0 T=
\ ordered factors: <free,inner> distinct from <inner,free> — the free vs in-block
\ position is representable (what the split/contraction rule reads).
s" XO  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpt,xpb>> )"  CHECK-QUIET-CANDIDATE!  0 T=
\ split is a REAL re-typing: a folded product index does not silently equal a bare
\ factor index.
s" XS  ( xidx<extprod<xpb,xpt>> -- xidx<xpb> )"               CHECK-QUIET-CANDIDATE!  0 T=

\ --- (c) contraction rule: inner accepted, free / product rejected ---------------
\ redx<E> is well-formed at signature parse iff E is an inner extent, so an identity
\ over redx<E> isolates the rule (a free/product E rejects before the body matters).
\ baseline (the leak as it is representable WITHOUT the product): a plain flat
\ folded-rows extent is freely contractable.
s" XCFLAT ( redx<xpflat> -- redx<xpflat> )"                   CHECK-QUIET-CANDIDATE! -1 T=
\ positive: the in-block (inner) extent is contractable.
s" XCI ( redx<xpt> -- redx<xpt> )"                            CHECK-QUIET-CANDIDATE! -1 T=
\ positive: another inner extent (a head dim #K) is contractable too.
s" XCK ( redx<xpk> -- redx<xpk> )"                            CHECK-QUIET-CANDIDATE! -1 T=
\ NEGATIVE (the leak, closed): contracting the FREE (batch) factor is rejected.
s" XCB ( redx<xpb> -- redx<xpb> )"                            CHECK-QUIET-CANDIDATE!  0 T=
\ NEGATIVE: contracting the WHOLE product (summing across sequences) is rejected.
s" XCP ( redx<extprod<xpb,xpt>> -- redx<extprod<xpb,xpt>> )"  CHECK-QUIET-CANDIDATE!  0 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" extent-product-test: failures" 1 die ;
REPORT
