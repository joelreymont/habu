\ data-floor.f - raising the DATA floor to `here`. A caller that publishes
\ storage allots it first, then raises the floor; the package holds no
\ storage of its own.

require src/habu/layout.f

package DATA-FLOOR
public

\ Everything allotted so far outlives any rewind of DATA: a failed evaluation
\ or REPL line, a rolled-back declaration.
: RAISE ( -- )
   here PTR>N data-base PTR>N -  data-base DATA-FLOOR-CELL + ! ;

;package
