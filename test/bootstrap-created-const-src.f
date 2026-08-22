\ bootstrap-created-const-src.f - a constant cannot mint a nominal family.
\
\ The sibling of bootstrap-created-raw-src.f for the `constant` publisher. Its
\ own row matters here: `PRIM: constant` already answers `-- a` through hook
\ inference, so only the raw seal separates a published constant from an
\ inferred one, and only a nominal read-back can see the difference.

package BCK
NEWTYPE kon-id 0
7 constant KONST
s" BOOTSTRAP-CREATED-ARMED" type cr
public
: LEAK ( -- kon-id ) KONST ;
;package
