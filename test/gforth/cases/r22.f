\ An underdepth no catch receives runs the exit hook, then exits 70.
package XP
private
CAST: >VECTOR ( n -- ptr [ -- ] )
: VECTOR ( -- ptr [ -- ] ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - EXIT-HOOK-CELL + >VECTOR ;
public
: HOOK ( -- ) ." hook" cr ;
: ARM ( -- ) ['] HOOK VECTOR ! ;
;package
XP:ARM
drop
