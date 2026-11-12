\ postpone-define.f - a definer builds a definition with define-word,
\ body-append and postpone, and each postponed word binds the word the
\ definer found when it was compiled, whatever the building package defines:
\ a global, the definer package's private word and its public one. The same
\ body evaluated as text binds the building package's words, and body-append
\ with no definition open is refused. The file is also a build source: MAIN
\ prints the three answers.

require src/lib/test.f

package POSTPONE-DEFINER
private
: BUMP ( n -- n ) 1 + ;
public
: DOUBLE ( n -- n ) 2 * ;

\ : NAME ( n -- n ) 3 * ;
: MAKE-TRIPLER ( ptr u8 n -- )
   s" ( n -- n )" define-word
   s" 3" body-append
   postpone *
   postpone ; ;

\ : NAME ( n -- n ) BUMP DOUBLE ;
: MAKE-STEPPER ( ptr u8 n -- )
   s" ( n -- n )" define-word
   postpone BUMP
   postpone DOUBLE
   postpone ; ;
;package

package POSTPONE-USER
private
: * ( n n -- n ) + ;
: BUMP ( n -- n ) 100 + ;
: DOUBLE ( n -- n ) drop 0 ;

s" TRIPLE" POSTPONE-DEFINER:MAKE-TRIPLER
s" STEP" POSTPONE-DEFINER:MAKE-STEPPER
s" : TRIPLE-TEXT ( n -- n ) 3 * ;" evaluate-closed

7 TRIPLE 21 T=
7 STEP 16 T=
7 TRIPLE-TEXT 10 T=

: APPEND-NONE ( -- n ) [: s" 1" body-append ;] catch ;
APPEND-NONE 0 T<>

public
: ANSWERS ( -- ) 7 TRIPLE . 7 STEP . 7 TRIPLE-TEXT . cr ;
;package

: MAIN ( -- ) POSTPONE-USER:ANSWERS ;
