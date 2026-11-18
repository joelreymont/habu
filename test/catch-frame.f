split-str
\ catch-frame.f - a caught throw restores the whole caller frame: data SP,
\ return-stack and loop-stack depth and the handler chain.

require src/habu/stack-abi.f
require src/lib/test.f

package CATCH-FRAME-TEST
private

variable ACC

\ The checker models `catch` restoring both stacks (src/core/checker.f RSCATCH),
\ so a throw's >r cell or open ?do frame would clobber the caller unrefused.
: THROW-R99-7 ( -- ) 99 >r 7 throw ;
: THROW-R88-7 ( -- ) 88 >r 7 throw ;
: THROW-R55-3 ( -- ) 55 >r 3 throw ;
: THROW-LOOP4-9 ( -- ) 4 0 ?do 9 throw loop ;
: THROW-LOOP5-9 ( -- ) 5 0 ?do 9 throw loop ;
: THROW-LOOP4-8 ( -- ) 4 0 ?do 8 throw loop ;

: CALLER-RSTK-PRESERVED ( -- n )       \ r> after a throwing action yields the caller's value
   42 >r ['] THROW-R99-7 catch drop r> ;

: RETURN-THROWS ( -- n )              \ more >r throws than the return stack holds
   0 STACK-ABI:RETURN-CELLS 1 + 0 ?do ['] THROW-R88-7 catch + loop ;

: LOOP-THROWS ( -- n )                \ more ?do throws than the loop stack holds
   0 ACC !
   STACK-ABI:LOOP-FRAMES 1 + 0 ?do
      ['] THROW-LOOP4-9 catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;

: LOOP-INDEX-INTACT ( -- n )           \ caller's ?do index survives a throwing inner ?do
   0 ACC !
   3 0 ?do
      ['] THROW-LOOP5-9 catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;                             \ 0 + 1 + 2 = 3

: INNER-CATCH ( -- )                   \ a nested catch that returns normally (return-balanced)
   ['] THROW-R55-3 catch drop ;

: THROW-INNER-R99-7 ( -- ) INNER-CATCH 99 >r 7 throw ;

: NESTED-RETURN-THROWS ( -- n )       \ the same through a nested catch
   0 STACK-ABI:RETURN-CELLS 1 + 0 ?do ['] THROW-INNER-R99-7 catch + loop ;

: NESTED-LOOP-THROWS ( -- n )
   0 ACC !
   STACK-ABI:LOOP-FRAMES 1 + 0 ?do
      ['] THROW-LOOP4-8 catch drop
      ['] THROW-INNER-R99-7 catch drop
      ACC @ i + ACC !
   loop
   ACC @ ;

\ 0 + 1 + ... + LOOP-FRAMES: every pass ran with its own index.
: LOOP-SUM ( -- n ) STACK-ABI:LOOP-FRAMES 1 + STACK-ABI:LOOP-FRAMES * 2 / ;

: IN-PROCESS ( -- )
   s" caught throw preserves caller >r value" T-LABEL
   CALLER-RSTK-PRESERVED 42 T=
   s" more caught >r throws than the return stack holds" T-LABEL
   RETURN-THROWS STACK-ABI:RETURN-CELLS 1 + 7 * T=
   s" more caught ?do throws than the loop stack holds" T-LABEL
   LOOP-THROWS LOOP-SUM T=
   s" caller ?do index survives inner throw" T-LABEL
   LOOP-INDEX-INTACT 3 T=
   s" nested catch: more >r throws than the return stack holds" T-LABEL
   NESTED-RETURN-THROWS STACK-ABI:RETURN-CELLS 1 + 7 * T=
   s" nested catch: more ?do throws than the loop stack holds" T-LABEL
   NESTED-LOOP-THROWS LOOP-SUM T= ;

public

: RUN ( -- )
   T-RESET
   IN-PROCESS
   T-REPORT
   s" catch-frame: ok" type cr ;

;package

CATCH-FRAME-TEST:RUN
