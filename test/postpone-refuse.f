\ postpone-refuse.f - a definition a definer builds is refused for a `;` in
\ appended text, for compile, of a quotation literal and for a postponed word
\ since undefined, and the next definition compiles; a definer is refused for
\ postponing a word its tick is refused for; a catch abandons a definition
\ opened inside it and keeps one opened before it.

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

\ A throw between define-word and `;` leaves no definition open past its catch.
: MAKE-THROWN ( ptr u8 n -- ) EFFECT:NEW EFFECT:N EFFECT:OUT define-word 99 throw ;
: THROWN ( -- n ) [: s" THROWN-DEF" MAKE-THROWN ;] catch ;
THROWN 99 T=

\ A refusal inside an evaluate the definer runs abandons the definer's definition.
: NESTED-IMM ( -- ) [: 1 + ;] compile, ; immediate
s" NESTED-IMM" 0 parse-imm
: MAKE-NESTED ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" NESTED-IMM" evaluate-closed
   postpone ; ;
: NESTED ( -- n ) [: s" NESTED-DEF" MAKE-NESTED ;] catch ;
NESTED 70 T=

\ An immediate's catch of its own refusal keeps the source definition it runs in.
: KEPT-IMM ( -- ) [: [: 1 + ;] compile, ;] catch 70 T= ; immediate
s" KEPT-IMM" 0 parse-imm
: KEPT ( -- n ) KEPT-IMM 5 ;
KEPT 5 T=

: AFTER-RETIRED ( -- n ) 8 ;
AFTER-RETIRED 8 T=
;package

T-REPORT
