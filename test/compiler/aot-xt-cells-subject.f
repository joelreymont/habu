\ Persistent cells DECLARED to hold an execution token, bound at LOAD time and
\ read at run time. A stripped image restores the capture-time BYTES of these
\ cells, which are the BUILDER's code addresses, so each one needs the row
\ src/habu/aot-lib.f EMIT-XT-ROWS writes and the startup patch that applies it.
\
\ BIND IS NEVER CALLED AT RUN TIME. It is the load-time initializer, and its own
\ code declares address cells through `xt!`, which a stripped image has no
\ registrar for - so the closure must carry the quotation BODIES and not the word
\ that bound them. Pulling BIND in instead fails the link outright.
package AOT-XT-CELL-SUBJECT

$4A constant FAILURE-RC

: EXPECT ( bool -- )
   0= if s" aot-xt-cells: mismatch" FAILURE-RC die then ;

: BUMP ( n -- n ) 1+ ;
: TWICE ( n -- n ) 2 * ;

defer STEP ( n -- n )            \ bound to a named word's entry
defer QSTEP ( n -- n )           \ bound to an anonymous body
defer QSTEP2 ( n -- n )          \ ... and a second one, from the same emission

\ The two bodies have different lengths, so a body whose extent is measured
\ wrong reaches the other one's code instead of its own.
: BIND ( -- )
   ['] BUMP is STEP
   [: BUMP BUMP ;] is QSTEP
   [: TWICE ;] is QSTEP2 ;
BIND

public

: RUN ( -- )
   41 STEP 42 = EXPECT
   40 QSTEP 42 = EXPECT
   21 QSTEP2 42 = EXPECT
   s" aot-xt-cells: ok" type cr ;

;package

: MAIN ( -- )
   AOT-XT-CELL-SUBJECT:RUN ;
