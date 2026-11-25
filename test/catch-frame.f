\ catch-frame.f - a caught throw restores the whole caller frame: data SP,
\ return-stack and loop-stack depth and the handler chain. A caught failed
\ text keeps the storage older data points into and the files it required
\ load again.

require src/habu/stack-abi.f
require src/lib/test/assert.f
require src/lib/test/eval.f

package CATCH-FRAME-TEST
private

variable ACC

\ The checker models `catch` restoring both stacks (src/core/checker.f RSCATCH),
\ so a throw's >r cell or open ?do frame would clobber the caller with no rejection.
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

\ A catch's body leaves more, fewer or the same cells it took, and throws 7 on
\ a nonzero top. On both paths the catch leaves its row's depth over 77.
: GROW ( n -- n n ) dup 0<> if 7 throw then dup ;
: SHRINK ( n n -- n ) 0<> if 7 throw then ;
: KEEP ( n -- n ) dup 0<> if 7 throw then ;

\ ( cells over the depth at entry, the sentinel ) after the catch.
: GROW-FRAME ( n -- n n )
   {: f:n :}
   depth {: d:n :}
   77 f ['] GROW catch drop 2drop
   depth d -  swap ;

: SHRINK-FRAME ( n -- n n )
   {: f:n :}
   depth {: d:n :}
   77 1 f ['] SHRINK catch 2drop
   depth d -  swap ;

: KEEP-FRAME ( n -- n n )
   {: f:n :}
   depth {: d:n :}
   77 f ['] KEEP catch 2drop
   depth d -  swap ;

\ A runner whose quotation's output row is open: the step alone binds it.
: RUN-OPEN ( R n [ R n -- S ] -- S ) execute ;

\ On a throw the two cells the body was given are stale: they are dropped and
\ the code thrown on to the frame's own catch.
: OPEN-DROP ( n -- )
   ['] GROW [: RUN-OPEN ;] catch {: c:n :}
   c 0<> if 2drop c throw then 2drop ;

: OPEN-FRAME ( n -- n n )
   {: f:n :}
   depth {: d:n :}
   77 1 f ['] OPEN-DROP catch 2drop
   depth d -  swap ;

\ A runner whose quotation's input row is bare: the caller's xt names its own
\ row, so the body's rows end on that row's tail.
: RUN-ROW ( R [ R -- S ] -- S ) execute ;

variable ROW-SHIFT

\ The cells over the depth at entry once the code is popped, stored on both
\ paths before a code is thrown on.
: ROW-STORE ( R n [ R n -- R n n ] -- R n n )
   depth {: d:n :}
   [: RUN-ROW ;] catch {: c:n :}
   depth d - ROW-SHIFT !
   2drop  1000 c + throw ;

: ROW-RUN ( n -- ) ['] GROW ROW-STORE ;

: ROW-FRAME ( n -- n ) ['] ROW-RUN catch drop  ROW-SHIFT @ ;

\ A body that never returns leaves its catch the depth it found.
: DROP-THROW ( n -- ) drop 7311 throw ;

\ ( cells over the depth at entry, the sentinel, the code ) once the catch's
\ code and the cells its body took are popped.
: TAKE-FRAME ( -- n n n )
   depth {: d:n :}
   77 5 [: DROP-THROW ;] catch {: c:n :} drop
   depth d -  swap c ;

: PUSH-FRAME ( -- n n n )
   depth {: d:n :}
   77 [: 1 2 9 throw ;] catch {: c:n :}
   depth d -  swap c ;

: FRAME= ( n n -- ) 77 T=  1 T= ;

: DEPTH-EVENED ( -- )
   s" a catch whose body leaves more keeps the row's depth" T-LABEL
   0 GROW-FRAME FRAME=  1 GROW-FRAME FRAME=
   s" a catch whose body leaves fewer keeps the row's depth" T-LABEL
   0 SHRINK-FRAME FRAME=  1 SHRINK-FRAME FRAME=
   s" a catch whose body leaves the same keeps the row's depth" T-LABEL
   0 KEEP-FRAME FRAME=  1 KEEP-FRAME FRAME=
   s" a catch whose body's output row is open keeps the row's depth" T-LABEL
   0 OPEN-FRAME FRAME=  1 OPEN-FRAME FRAME=
   s" a catch whose body's xt names the caller's row keeps the row's depth" T-LABEL
   0 ROW-FRAME 0 T=  1 ROW-FRAME 0 T=
   s" a catch whose body takes a cell and never returns keeps it" T-LABEL
   TAKE-FRAME 7311 T=  FRAME=
   s" a catch whose body pushes and never returns keeps the row's depth" T-LABEL
   PUSH-FRAME 9 T=  FRAME= ;

public

\ The texts run outside the package and store into these, so they are public.
variable CF-HEAD
defer CF-ACT ( -- n )
variable CF-ANSWER

private

\ A row is two cells: the next row as a distance from NULL-PTR, 0 at the end,
\ then its value.
CAST: >ROW ( n -- ^n )

: ROW-SUM ( -- n )
   0 CF-HEAD @
   begin dup 0<> while
      >ROW dup 1 cells + @ rot + swap @
   repeat drop ;

: LIST-KEPT ( -- )
   s" a failed text's row in an older list survives new data" T-LABEL
   ROW-SUM {: before:n :}
   s" datum here NULL-PTR - CATCH-FRAME-TEST:CF-HEAD @ , 7 , CATCH-FRAME-TEST:CF-HEAD ! 12345 throw"
   TEST-EVAL:RC 12345 T=
   s" create CF-PAD 0 , 0 , 0 , 0 , 0 , 0 ," TEST-EVAL:RC 0 T=
   ROW-SUM before 7 + T= ;

: DEFER-KEPT ( -- )
   s" a defer a failed text set runs its word after new definitions" T-LABEL
   s" : CF-SEVEN ( -- n ) 7 ; : CF-SET ( -- ) [: CF-SEVEN ;] is CATCH-FRAME-TEST:CF-ACT ; CF-SET 12345 throw"
   TEST-EVAL:RC 12345 T=
   s" : CF-NINE ( -- n ) 9 ; : CF-TEN ( -- n ) 10 ;" TEST-EVAL:RC 0 T=
   CF-ACT 7 T= ;

: REQUIRE-AGAIN ( -- )
   s" a file a failed text required loads on a later require" T-LABEL
   s" require test/fixtures/catch-frame-require.f 12345 throw" TEST-EVAL:RC 12345 T=
   s" require test/fixtures/catch-frame-require.f CATCH-FRAME-FIXTURE:ANSWER CATCH-FRAME-TEST:CF-ANSWER !"
   TEST-EVAL:RC 0 T=
   CF-ANSWER @ 42 T= ;

public

: RUN ( -- )
   T-RESET
   IN-PROCESS
   DEPTH-EVENED
   LIST-KEPT
   DEFER-KEPT
   REQUIRE-AGAIN
   T-REPORT
   s" catch-frame: ok" type cr ;

;package

CATCH-FRAME-TEST:RUN
