\ clobber-host-redef.f - fixture for a package word that emits nothing,
\ spelled like a global host word, read as text by clobber-lint-test.f
\ (never loaded).

: SCRATCH ( n -- ) {: r:n :}
   r 0 MOVZ, ;

package REDEF

: SCRATCH ( -- n )
   12 ;

variable LREDEF-CALLEE

: EMIT-REDEF-CALLEE ( -- )
   LREDEF-CALLEE LABEL@ LBL,
   0 0 MOVZ,  RET, ;                \ answers in x0

\ REDEF's SCRATCH hides the global one, so the LDR writes x9 and x0 holds the
\ answer: no finding.
: EMIT-REDEF ( -- )
   9 SCRATCH 0 LDR,
   LREDEF-CALLEE LABEL@ BL,
   4 0 0 LDRB, ;

;package
