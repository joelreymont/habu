\ Production AOT subject for frame slots of values copied through loops and for
\ data-stack moves merged across link-frame restoration. CASES checks the loop
\ shapes by throw; SHAPES prints each word's name and then its results, one per
\ line, and the caller compares the output byte for byte.
package FRAME-SLOT-IMAGE
private
variable COUNT
variable SNAP

: HIT ( ptr u8 n -- )
   2drop 1 COUNT +! ;

: EARLY ( n -- n )
   begin dup 0 > while 1- repeat ;

: REPLACE ( -- ) 29 SNAP ! ;

: SNAPSHOT ( bool -- n )
   SNAP @ {: saved:n :}
   if REPLACE then
   saved ;

variable TICKS
: TICK ( -- ) 1 TICKS +! ;
: TWICE ( n -- n ) 2 * ;
: SUM ( n n -- n ) + 1+ ;
: THREE ( n -- n n n ) dup 1+ over 2 + ;
: EXCH ( n n -- n n ) swap ;
: CALLEE ( n -- n ) 1 + ;

\ A pending publish move carried across link restoration into a tail call.
: PAIR ( n n -- n ) SUM TWICE ;
: MERGE ( n -- n ) THREE drop EXCH drop CALLEE ;

\ A catch frame, unconditional or on one arm, then a tail call.
: CT ( n -- n ) [: 1+ ;] catch drop TWICE ;
: CT2 ( n -- n ) dup 0 < if [: 7 throw ;] catch drop then TWICE ;


\ The carried item starts as a copy of a and is replaced by b on odd rounds.
: ALT ( n n n -- n )
   {: a:n b:n n:n :}
   a n 0 ?do i 1 and 0<> if drop b then TICK loop a + b + ;


\ A carried copy of a local live across a loop whose body holds a catch.
: CL ( n n -- n )
   {: a:n n:n :}
   a n 0 ?do [: TICK ;] catch drop loop a + ;


\ A conditional join whose arms disagree, the local read after both.
: JN ( n bool -- n )
   {: a:n f:bool :}
   f if a TWICE else a then TICK a + ;


\ A copy forwarded through nested loops, returned alongside the local.
: NEST ( n n -- n n )
   {: a:n n:n :}
   a n 0 ?do n 0 ?do TICK loop loop a ;


\ An early exit from the loop with the carried copy.
: EXIT-EARLY ( n n -- n )
   {: a:n n:n :}
   a n 0 ?do TICK i 2 = if unloop exit then loop ;


: SHAPES ( -- )
   s" PAIR" type cr 73 4 5 PAIR . .
   s" MERGE" type cr 74 6 MERGE . .
   s" CT" type cr 5 CT .
   s" CT2" type cr -5 CT2 . 5 CT2 .
   s" ALT" type cr 10 20 4 ALT . 10 20 3 ALT . 10 20 0 ALT .
   s" CL" type cr 0 TICKS ! 7 3 CL . TICKS @ .
   s" JN" type cr 9 true JN . 9 false JN .
   s" NEST" type cr 0 TICKS ! 4 3 NEST . . TICKS @ .
   s" EXIT-EARLY" type cr
   0 TICKS ! 8 5 EXIT-EARLY . TICKS @ .
   0 TICKS ! 8 2 EXIT-EARLY . TICKS @ . ;

public
: BB ( ptr u8 n -- n )
   {: p:ptr u:n :}
   0 COUNT !
   0 begin dup u < while
      p over + c@ 47 = if p over HIT then
      1+
   repeat drop COUNT @ ;

: CASES ( -- )
   3 EARLY 0 <> if 76 throw then
   17 SNAP ! false SNAPSHOT 17 <> if 77 throw then
   17 SNAP ! true SNAPSHOT 17 <> if 78 throw then
   SNAP @ 29 <> if 79 throw then
   s" " BB 0 <> if 71 throw then
   s" /" BB 1 <> if 72 throw then
   s" abc" BB 0 <> if 73 throw then
   s" a/b/c" BB 2 <> if 74 throw then
   s" /a//b/" BB 4 <> if 75 throw then
   SHAPES
   s" frame-slots: ok" type cr ;
;package

: MAIN ( -- ) FRAME-SLOT-IMAGE:CASES ;
