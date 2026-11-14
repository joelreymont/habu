\ postpone-refuse.f - a definition a definer builds is refused for a `;` in
\ appended text, for compile, of a quotation literal and for a postponed word
\ since undefined, and the next definition compiles; a definer is refused for
\ postponing a word its tick is refused for.

require src/lib/test.f

package POSTPONE-REFUSE
private

\ : NAME ( -- n ) 5 ; with the `;` appended as text.
: MAKE-TEXT-END ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word
   s" 5 ;" body-append
   postpone ; ;

\ : NAME ( n -- n ) 1 + ; through compile, of a quotation.
: MAKE-QUOT-CALL ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   [: 1 + ;] compile,
   postpone ; ;

: ONE ( -- n ) 1 ;
\ : NAME ( -- n ) ONE ;
: MAKE-ONE ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word
   postpone ONE
   postpone ; ;
undefine ONE
: ONE ( -- n ) 2 ;

: TEXT-END ( -- n ) [: s" TEXT-ENDED" MAKE-TEXT-END ;] catch ;
TEXT-END 70 T=

: TRUSTED-POSTPONE ( -- n )
   [: s\" : MAKE-TRUSTED ( ptr u8 n -- ) EFFECT:NEW EFFECT:N EFFECT:OUT define-word postpone DYNAMIC-STORAGE:RECORD-NEW postpone ; ;"
      evaluate-closed ;] catch ;
TRUSTED-POSTPONE 70 T=

: QUOT-CALL ( -- n ) [: s" QUOT-CALLED" MAKE-QUOT-CALL ;] catch ;
QUOT-CALL 70 T=

: RETIRED ( -- n ) [: s" ONE-AGAIN" MAKE-ONE ;] catch ;
RETIRED 70 T=
: AFTER-RETIRED ( -- n ) 8 ;
AFTER-RETIRED 8 T=
;package

T-REPORT
