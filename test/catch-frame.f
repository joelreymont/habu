\ catch-frame.f - a caught throw restores the COMPLETE caller execution frame
\ (dot habu-restore-complete-exec-abb8baca): data SP, user return-stack depth
\ (RSP-CELL), loop-stack depth (LOOPSP-CELL), and the handler chain. The checker
\ models `catch` as restoring both typed stacks (src/core/checker.f RSCATCH), so a
\ throwing quotation that leaves >r litter or an open ?do frame would otherwise
\ clobber caller return/loop state with no type mismatch.
\
\ Native gates run it; tools/bootstrap.sh runs the same file against the
\ Gforth-recovered candidate, so native and recovery must agree. The cases assert
\ the restored VALUES a checked r>/loop-index sees.

require src/lib/test.f

package CATCH-FRAME-TEST
private

variable ACC

\ ---- in-process: a caught throw restores caller return/loop state ----

: CALLER-RSTK-PRESERVED ( -- n )       \ r> after a throwing >r quotation yields the caller's value
   42 >r [: 99 >r 7 throw ;] catch drop r> ;

: RSP-DRIFT ( -- n )                   \ user return-stack depth drift across a throwing >r quotation
   data-base RSP-CELL + @ {: r0:n :}
   [: 88 >r 7 throw ;] catch drop
   data-base RSP-CELL + @ r0 - ;

: LOOPSP-DRIFT ( -- n )                \ loop-stack depth drift across a throw inside ?do
   data-base LOOPSP-CELL + @ {: l0:n :}
   [: 4 0 ?do 9 throw loop ;] catch drop
   data-base LOOPSP-CELL + @ l0 - ;

: LOOP-INDEX-INTACT ( -- n )           \ caller's ?do index survives a throwing inner ?do
   0 ACC !
   3 0 ?do
      [: 5 0 ?do 9 throw loop ;] catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;                             \ 0 + 1 + 2 = 3

: INNER-CATCH ( -- )                   \ a nested catch that returns normally (return-balanced)
   [: 55 >r 3 throw ;] catch drop ;

: NESTED-RSP-DRIFT ( -- n )            \ repeated throws + a nested catch: return depth does not drift
   data-base RSP-CELL + @ {: r0:n :}
   100 0 ?do [: 42 >r 7 throw ;] catch drop loop
   [: INNER-CATCH  99 >r 7 throw ;] catch drop
   data-base RSP-CELL + @ r0 - ;

: NESTED-LOOPSP-DRIFT ( -- n )         \ repeated throws + a nested catch: loop depth does not drift
   data-base LOOPSP-CELL + @ {: l0:n :}
   100 0 ?do [: 4 0 ?do 8 throw loop ;] catch drop loop
   [: INNER-CATCH  99 >r 7 throw ;] catch drop
   data-base LOOPSP-CELL + @ l0 - ;

: IN-PROCESS ( -- )
   s" caught throw preserves caller >r value" T-LABEL
   CALLER-RSTK-PRESERVED 42 T=
   s" return-stack depth does not drift" T-LABEL
   RSP-DRIFT 0 T=
   s" loop-stack depth does not drift" T-LABEL
   LOOPSP-DRIFT 0 T=
   s" caller ?do index survives inner throw" T-LABEL
   LOOP-INDEX-INTACT 3 T=
   s" nested catch + repeated throws: no return drift" T-LABEL
   NESTED-RSP-DRIFT 0 T=
   s" nested catch + repeated throws: no loop drift" T-LABEL
   NESTED-LOOPSP-DRIFT 0 T= ;

public

: RUN ( -- )
   T-RESET
   IN-PROCESS
   T-REPORT
   s" catch-frame: ok" type cr ;

;package

CATCH-FRAME-TEST:RUN
