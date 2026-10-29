\ catch-frame.f - a caught throw restores the whole caller frame: data SP, user
\ return-stack depth (RSP-CELL), loop-stack depth (LOOPSP-CELL) and the handler
\ chain. tools/bootstrap.sh also runs this file on the Gforth-recovered
\ candidate: native and recovery agree.

require src/lib/test.f

package CATCH-FRAME-TEST
private

variable ACC

\ The checker models `catch` as restoring both typed stacks (src/core/checker.f
\ RSCATCH), so a throwing quotation's >r litter or open ?do frame would clobber
\ caller state with no type mismatch.
\ These actions have a truthful empty normal interface. Each throws after
\ changing the live frame; a tick gives catch a fixed callable effect.
TRUSTED: THROW-R99-7 ( -- ) 99 >r 7 throw ;
TRUSTED: THROW-R88-7 ( -- ) 88 >r 7 throw ;
TRUSTED: THROW-R55-3 ( -- ) 55 >r 3 throw ;
TRUSTED: THROW-R42-7 ( -- ) 42 >r 7 throw ;
TRUSTED: THROW-LOOP4-9 ( -- ) 4 0 ?do 9 throw loop ;
TRUSTED: THROW-LOOP5-9 ( -- ) 5 0 ?do 9 throw loop ;
TRUSTED: THROW-LOOP4-8 ( -- ) 4 0 ?do 8 throw loop ;

: CALLER-RSTK-PRESERVED ( -- n )       \ r> after a throwing action yields the caller's value
   42 >r ['] THROW-R99-7 catch drop r> ;

: RSP-DRIFT ( -- n )                   \ user return-stack depth drift across a throwing >r quotation
   data-base RSP-CELL + @ {: r0:n :}
   ['] THROW-R88-7 catch drop
   data-base RSP-CELL + @ r0 - ;

: LOOPSP-DRIFT ( -- n )                \ loop-stack depth drift across a throw inside ?do
   data-base LOOPSP-CELL + @ {: l0:n :}
   ['] THROW-LOOP4-9 catch drop
   data-base LOOPSP-CELL + @ l0 - ;

: LOOP-INDEX-INTACT ( -- n )           \ caller's ?do index survives a throwing inner ?do
   0 ACC !
   3 0 ?do
      ['] THROW-LOOP5-9 catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;                             \ 0 + 1 + 2 = 3

: INNER-CATCH ( -- )                   \ a nested catch that returns normally (return-balanced)
   ['] THROW-R55-3 catch drop ;

TRUSTED: THROW-INNER-R99-7 ( -- ) INNER-CATCH 99 >r 7 throw ;

: NESTED-RSP-DRIFT ( -- n )            \ repeated throws + a nested catch: return depth does not drift
   data-base RSP-CELL + @ {: r0:n :}
   100 0 ?do ['] THROW-R42-7 catch drop loop
   ['] THROW-INNER-R99-7 catch drop
   data-base RSP-CELL + @ r0 - ;

: NESTED-LOOPSP-DRIFT ( -- n )         \ repeated throws + a nested catch: loop depth does not drift
   data-base LOOPSP-CELL + @ {: l0:n :}
   100 0 ?do ['] THROW-LOOP4-8 catch drop loop
   ['] THROW-INNER-R99-7 catch drop
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
