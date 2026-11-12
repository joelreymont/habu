\ src/core/source-producer.f — the kernel digest an image keeps.
\ Loaded after include.f and the target layout in the engine prefix.

package SOURCE-PRODUCER
private

CAST: KERNEL-XT ( n -- [ -- ptr u8 n bool ] )

\ A source definition on an older host must not impersonate its absent kernel
\ metadata primitive. The text extent is the running engine's, as its boot
\ published it: a build window for another OS loads that OS's header offsets, so
\ no layout constant may enter this test.
: ENGINE-TEXT? ( n -- bool )
   {: xt:n :}
   data-base RBASE-CELL + @ {: base:n :}
   data-base TEXT-SIZE-CELL + @ {: size:n :}
   xt base >= xt base - size < and ;

public

\ The kernel digest comes from immutable metadata in the running host; a host
\ built before that primitive was added has no kernel fact to report.
: KERNEL$ ( -- ptr u8 n bool )
   s" kernel-producer$" WL:PRIMITIVES search-wl {: xt:n :}
   xt 0= if NULL$ 0 0= 0= exit then
   xt ENGINE-TEXT? 0= if NULL$ 0 0= 0= exit then
   xt KERNEL-XT execute ;

;package
