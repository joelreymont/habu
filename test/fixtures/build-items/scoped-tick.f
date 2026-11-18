split-str
\ scoped-tick.f - a program whose definition ticks C2-MEM:WITH-INIT, a scoped
\ entry its build made: the build rejects it, rc 70.

require src/lib/c2-memory.f

package BITEM-SCOPED-TICK
: TICK ( -- ) ['] C2-MEM:WITH-INIT drop ;
;package

: MAIN ( -- ) ;
