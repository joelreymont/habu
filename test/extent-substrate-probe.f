\ extent-substrate-probe.f — decision-record demonstration for
\ habu-choose-extent-nominal-f61dac3e. Shows the CHOSEN substrate (TFAM
\ parametric families) typing candidate-B's idx<#M> with two extents distinct,
\ and — the deciding reason — hosting BTC-7's #B*#T product/factorization
\ structure on existing parametric-family unification with NO new registry.
\ It also proves the COMPARATIVE case (why the decision, not just adequacy):
\ the atom substrate silently ACCEPTS a typo'd extent (XP-ATOM-HAZARD) where TFAM
\ rejects it (XP-TYPO), and a mis-cased tail is rejected (XP-CASE, E-TFAM-CASE).
\
\ Run standalone (NOT a routed gate case; BTC-2/BTC-7 own the permanent
\ regressions):
\     bin/hb < test/extent-substrate-probe.f
\ Exit 0 + "ok" = every verdict as designed; REPORT dies (exit 1) on any miss.
\
\ Names: `idx`/`#M` are candidate-B surface sugar owned by
\ habu-extent-typed-tensor-bde435dc. Type/family tails are lowercase (the TFAM
\ rule E-TFAM-CASE; all type names — n, idx, span — are lowercase, distinct from
\ the UPPER-CASE rule for executable words). `#M` is not a legal bare family
\ tail, and `idx` is A1's flat CT-role, so this fixture uses xidx/xm/xn/xk/xprod
\ to exercise the underlying TFAM substrate the sugar will desugar onto.

require test/checker-assert.f

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

\ silence the expected rejection diagnostics (verdicts are asserted, not printed).
create XP-DIAG 8192 allot
XP-DIAG 8192 DIAG-BUFFER!

\ CHOSEN SUBSTRATE = TFAM families.
\   xidx  = candidate-B's parametric index family idx<extent>  (arity 1, value nominal)
\   xm xn xk = flat extent roles #M #N #K            (arity-0 nominal tags)
\   xprod = BTC-7 product former  #B*#T              (arity-2 parametric family)
TYPEFAMILY xidx 1
TYPEFAMILY xm 0
TYPEFAMILY xn 0
TYPEFAMILY xk 0
TYPEFAMILY xprod 2

\ --- (a) flat idx<#M>: two extents distinct (the candidate-B guarantee) --------
\ index over the same extent unifies (identity flow).
s" XP-ID ( xidx<xm> -- xidx<xm> )"        CHECK-QUIET-CANDIDATE! -1 T=
\ flipping the extent is an author-time type error: idx<#N> where idx<#M> demanded.
s" XP-FLIP ( xidx<xm> -- xidx<xn> )"      CHECK-QUIET-CANDIDATE!  0 T=
s" XP-FLIP2 ( xidx<xk> -- xidx<xm> )"     CHECK-QUIET-CANDIDATE!  0 T=
\ an extent index is a distinct nominal, not bare n (no silent collapse).
s" XP-NOUT ( xidx<xm> -- n )"             CHECK-QUIET-CANDIDATE!  0 T=
s" XP-NIN ( n -- xidx<xm> )"              CHECK-QUIET-CANDIDATE!  0 T=
\ typo protection: an undeclared extent name is rejected (unknown signature type).
s" XP-TYPO ( xidx<xundeclared> -- xidx<xundeclared> )" CHECK-QUIET-CANDIDATE! 0 T=
\ COMPARATIVE (the atom hazard that makes atoms the WRONG substrate): the SAME
\ undeclared name spelled as an atom (`extent-*`) is silently ACCEPTED — any
\ extent-* parses, so a typo'd extent is a live distinct extent, not a reject.
\ TFAM (XP-TYPO) and CT-roles reject it; atoms do not. Verdict -1 = accept.
s" XP-ATOM-HAZARD ( xidx<extent-neverdeclared> -- xidx<extent-neverdeclared> )" CHECK-QUIET-CANDIDATE! -1 T=
\ COMPARATIVE (case discipline): an upper-case tail is rejected — type/family
\ tails are lowercase (E-TFAM-CASE), so a mis-cased extent cannot resolve.
s" XP-CASE ( xidx<XM> -- xidx<XM> )" CHECK-QUIET-CANDIDATE! 0 T=

\ --- (b) BTC-7 product/factorization structure on existing TFAM unification ----
\ a folded row index typed as the product #B*#T (xprod<xm,xn>) is a first-class arg.
s" XP-PROD ( xidx<xprod<xm,xn>> -- xidx<xprod<xm,xn>> )"   CHECK-QUIET-CANDIDATE! -1 T=
\ factors are ORDERED: <#B,#T> is distinct from <#T,#B>, so the free (outer) vs
\ in-block (inner) factor is representable — the hook BTC-7's split/contraction
\ rule reads (free #B rejected, inner #T accepted).
s" XP-ORDER ( xidx<xprod<xm,xn>> -- xidx<xprod<xn,xm>> )"  CHECK-QUIET-CANDIDATE!  0 T=
\ a folded product index does NOT implicitly equal a bare-factor index: the
\ factorization/split is a real re-typing step (BTC-7), not silent coercion.
s" XP-SPLIT ( xidx<xprod<xm,xn>> -- xidx<xm> )"            CHECK-QUIET-CANDIDATE!  0 T=
\ mismatched factor product: <#M,#N> distinct from <#M,#K> (BTC-7 mismatch reject).
s" XP-FACT ( xidx<xprod<xm,xn>> -- xidx<xprod<xm,xk>> )"   CHECK-QUIET-CANDIDATE!  0 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" extent-substrate-probe: failures" 1 die ;
REPORT
