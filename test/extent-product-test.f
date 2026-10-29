\ extent-product-test.f - the checker half of extent-role product/factorization
\ (docs/design.md "Type substrate"): a contraction may sum an inner extent of a
\ folded extprod<free,inner> row, never the free extent or the whole product, so
\ a cross-sequence contraction is unrepresentable in a checked program. The
\ surface half is maki/extent-test.f's.

require src/lib/test.f

create EP-DIAG 8192 allot
EP-DIAG 8192 DIAG-BUFFER!

T-RESET

\ Stand-ins for maki's families: xidx the index family, xpb free #B, xpt inner
\ #T, xpk an inner head dim, xpflat a flat extent.
NEWTYPE xidx<a>
NEWTYPE xpb
NEWTYPE xpt
NEWTYPE xpk
NEWTYPE xpflat

\ mark the free (outer / batch) factor, exactly as EXTPROD: does at declaration.
s" xpb" EXT-MARK-FREE-TAIL

\ CHECK-QUIET-CANDIDATE! answers -1 to accept, 0 to reject.
\ A folded row index is the ordered product type.
s" XA  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpb,xpt>> )" CHECK-QUIET-CANDIDATE! -1 T=

\ Mismatched factors: <#B,#T> is distinct from <#B,#K>.
s" XF  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpb,xpk>> )"  CHECK-QUIET-CANDIDATE!  0 T=
\ ordered factors: <free,inner> distinct from <inner,free> — the free vs in-block
\ position is representable (what the split/contraction rule reads).
s" XO  ( xidx<extprod<xpb,xpt>> -- xidx<extprod<xpt,xpb>> )"  CHECK-QUIET-CANDIDATE!  0 T=
\ split is a REAL re-typing: a folded product index does not silently equal a bare
\ factor index.
s" XS  ( xidx<extprod<xpb,xpt>> -- xidx<xpb> )"               CHECK-QUIET-CANDIDATE!  0 T=

\ redx<E> is well-formed at signature parse only when E is an inner extent, so
\ an identity over redx<E> isolates the rule. A flat folded-rows extent is
\ freely contractable: the leak as it is without the product.
s" XCFLAT ( redx<xpflat> -- redx<xpflat> )"                   CHECK-QUIET-CANDIDATE! -1 T=
\ positive: the in-block (inner) extent is contractable.
s" XCI ( redx<xpt> -- redx<xpt> )"                            CHECK-QUIET-CANDIDATE! -1 T=
\ positive: another inner extent (a head dim #K) is contractable too.
s" XCK ( redx<xpk> -- redx<xpk> )"                            CHECK-QUIET-CANDIDATE! -1 T=
\ NEGATIVE (the leak, closed): contracting the FREE (batch) factor is rejected.
s" XCB ( redx<xpb> -- redx<xpb> )"                            CHECK-QUIET-CANDIDATE!  0 T=
\ NEGATIVE: contracting the WHOLE product (summing across sequences) is rejected.
s" XCP ( redx<extprod<xpb,xpt>> -- redx<extprod<xpb,xpt>> )"  CHECK-QUIET-CANDIDATE!  0 T=

T-REPORT
