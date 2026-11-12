\ squat-main.f - built by tools/hb-build.f: it reopens CHECKER-RESOLVE, an
\ engine package the build sealed, to publish its private xref-search-wl.

package CHECKER-RESOLVE
public
: SQUAT-SEARCH ( ptr u8 n n -- ptr n ) xref-search-wl ;
;package

: MAIN ( -- ) ;
