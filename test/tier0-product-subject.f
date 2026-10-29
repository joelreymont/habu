\ tier0-product-subject.f - one program over the forms a product compiles:
\ loops with leave and unloop, begin/while/repeat, if/else, catch and throw,
\ nested quotations, a create ... does> definer, variables, a deferred word
\ rebound at run time, floats, and every string literal form.
package TIER0-SUBJECT

variable BUILT
tier@ BUILT !

variable TOTAL
variable ARG
variable RES

\ The sum of i*j over j in 0..3 and i in 0..2: 18.
: GRID ( -- n )
   0 4 0 ?do 3 0 ?do i j * + loop loop ;


\ The first i whose square exceeds LIM, through leave.
: FIRST-OVER ( n -- n )
   {: lim:n :}
   -1 10 0 ?do i i * lim > if drop i leave then loop ;


\ The index of 7, through unloop exit.
: FIND-7 ( -- n )
   10 0 ?do i 7 = if i unloop exit then loop -1 ;


\ Collatz steps from N to 1, through begin/while/repeat and if/else.
: STEPS ( n -- n )
   0 swap begin dup 1 <> while
      dup 2 mod 0= if 2 / else 3 * 1+ then
      swap 1+ swap
   repeat drop ;


: CHECK-EVEN ( n -- n ) dup 2 mod 0<> if -9081 throw then ;

\ The code a check of ARG throws, RES holding the value it passed.
: TRY ( n -- n )
   ARG ! [: ARG @ CHECK-EVEN RES ! ;] catch ;


: APPLY ( n [ n -- n ] -- n ) execute ;

\ A quotation holding a quotation: (n+1)*2.
: NEST ( n -- n ) [: [: 1+ ;] APPLY 2 * ;] APPLY ;

: SCALE: ( n -- ) create , does> ( n -- n ) @ * ;
3 SCALE: TRIPLE

defer STEP ( n -- n )
: BIND-ADD ( -- ) [: 3 + ;] is STEP ;
: BIND-MUL ( -- ) [: 10 * ;] is STEP ;
BIND-ADD

: FLOATS ( -- )
   7 s>f 2.0 f/ f. cr
   1.5 2.0 f* f>s . cr ;


\ Each literal repeats `0 4 5 6 7 P T p` four times, so those bytes land at
\ offsets 4k+3 whatever the alignment: the top byte of an ARM64 word, decoding
\ as ADR, CBZ/CBNZ, TBZ/TBNZ and B.cond; inline in the code band they make the
\ stripped link refuse the image.
: STRINGS ( -- )
   s" 00004444555566667777PPPPTTTTpppp" type cr
   ." 00004444555566667777PPPPTTTTpppp" cr
   s\" 00004444555566667777\tPPPPTTTTpppp" type cr
   c" 00004444555566667777PPPPTTTTpppp" count type cr
   c\" 00004444555566667777\tPPPPTTTTpppp" count type cr
   .\" 00004444555566667777\tPPPPTTTTpppp" cr ;


public

\ RUN's last line is the tier it loaded at. tools/hb-build-test-lib.f
\ HBT-SUBJECT-OUT holds the output; tools/hb-build-aot-test.f BUILD-AOT-TIERS
\ and tools/hb-build-repl-image-test.f HBT-REPL-TIER-PROGRAM build it at both
\ tiers.
: RUN ( -- )
   GRID . cr
   20 FIRST-OVER . cr
   FIND-7 . cr
   6 STEPS . cr
   4 TRY . cr RES @ . cr
   5 TRY . cr
   4 NEST . cr
   5 TRIPLE . cr
   0 TOTAL ! 5 0 ?do i TOTAL +! loop TOTAL @ . cr
   4 STEP . cr BIND-MUL 4 STEP . cr BIND-ADD
   FLOATS
   STRINGS
   BUILT @ . cr ;

;package

: MAIN ( -- ) TIER0-SUBJECT:RUN ;
