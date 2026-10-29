\ Persistent cells declared to hold an execution token, bound at load time and
\ read at run time. A stripped image omits the builder's code addresses from
\ DATA, so each cell needs the row src/habu/aot-link-arm.f EMIT-XT-ROWS writes
\ and the startup patch that applies it.
package AOT-XT-CELL-SUBJECT

$4A constant FAILURE-RC

: EXPECT ( bool -- )
   0= if s" aot-xt-cells: mismatch" FAILURE-RC die then ;

: BUMP ( n -- n ) 1+ ;
: TWICE ( n -- n ) 2 * ;

defer STEP ( n -- n )            \ bound to a named word's entry
defer QSTEP ( n -- n )           \ bound to an anonymous body
defer QSTEP2 ( n -- n )          \ ... and a second one, from the same emission
TYPED-VARIABLE RUNTIME-STEP [ n -- n ]

\ This cell straddles two encoder grid cells. Its neighbors are ordinary DATA
\ bytes: clearing either whole grid cell loses one of these marks.
create UNALIGNED 17 c, 0 , 34 c,
CAST: >STEP-SLOT ( ptr n -- ptr [ n -- n ] )
: UNALIGNED-SLOT ( -- ptr [ n -- n ] )
   UNALIGNED byte-view 1+ cell-view >STEP-SLOT ;

\ Load time only: BIND declares address cells through `xt!`, which a stripped
\ image has no registrar for, so the closure carries the quotation bodies, not
\ BIND; pulling BIND in fails the link. The two bodies differ in length, so a
\ body whose extent is measured wrong reaches the other one's code.
: BIND ( -- )
   ['] BUMP is STEP
   [: BUMP BUMP ;] is QSTEP
   [: TWICE ;] is QSTEP2
   [: BUMP ;] UNALIGNED-SLOT xt! ;
BIND

public

: RUN ( -- )
   41 STEP 42 = EXPECT
   40 QSTEP 42 = EXPECT
   21 QSTEP2 42 = EXPECT
   41 UNALIGNED-SLOT @ execute 42 = EXPECT
   UNALIGNED byte-view c@ 17 = EXPECT
   UNALIGNED byte-view 9 + c@ 34 = EXPECT
   ['] BUMP RUNTIME-STEP xt!
   41 RUNTIME-STEP @ execute 42 = EXPECT
   s" aot-xt-cells: ok" type cr ;

;package
