\ tier.f - choose the compiler that later definitions are compiled by.
\
\ `set-tier` is package TIER's private primitive (src/habu/prims.f): its one row
\ is TIER's, so a body compiled inside TIER calls it checked and a checked body
\ anywhere else does not find the name. SELECT is that body and the way every
\ other scope selects a tier. Top-level source is unchecked, so a build driver
\ still writes `1 set-tier` on its first line, ahead of any require.

package TIER

public

\ Every definition compiled after this call uses the selected compiler: 0 the
\ tier-0 JIT, 1 the tier-1 IR pipeline (src/habu/layout.f
\ NCOMP-DISPATCH:TIER-CELL). Any other value exits 70; x86-64 selects 1 only,
\ and tier 0 inside EXECUTABLE-BUILD:WITH throws 70.
: SELECT ( n -- )
   set-tier ;

;package
