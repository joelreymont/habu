\ postpone-refuse.f - a definition a definer builds is refused for a `;` in
\ appended text, for compile, of a quotation literal and for a postponed word
\ since undefined, and the next definition compiles; a definer is refused for
\ postponing a word its tick is refused for; a catch abandons a definition
\ opened inside it and keeps one opened before it, and the lines after it run;
\ a catch of a throw from an evaluate the definer runs keeps the definition,
\ and a word postponed after it binds.

require src/lib/string.f
require src/lib/test/ending.f
require src/lib/test/subject.f
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
   [: s\" : MAKE-TRUSTED ( ptr u8 n -- ) EFFECT:NEW EFFECT:N EFFECT:OUT define-word postpone reloc-maps-clear postpone ; ;"
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

\ The part keeps CUT-ONE out of sight where CUT-DEF ends: only its item binds it.
package CUT
: CUT-ONE ( n -- n ) 1 + ;
: CUT-TWO ( n -- n ) 2 + ;
public
: CUT-IMM ( -- ) postpone CUT-TWO postpone CUT-TWO 99 throw ; immediate
s" CUT-IMM" 0 parse-imm
: MAKE-CUT ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   [: s" CUT:CUT-IMM" evaluate-closed ;] catch 99 T=
   postpone CUT-ONE
   postpone ; ;
;package
s" CUT-DEF" CUT:MAKE-CUT
5 CUT-DEF 6 T=

: AFTER-RETIRED ( -- n ) 8 ;
AFTER-RETIRED 8 T=

$400 constant RAW-CAP
create RAW-OUT RAW-CAP allot
create RAW-ERR RAW-CAP allot

public
\ Abandoning a definition that awaits `[char]`'s operand skips no later line.
: RAW-CHILD ( -- )
   s\" : MAKE-RAW ( ptr u8 n -- ) EFFECT:NEW EFFECT:N EFFECT:OUT define-word  s\" [char]\" evaluate-closed  99 throw ;\n: RUN-RAW ( -- n ) [: s\" PR-RAW\" MAKE-RAW ;] catch ;\nRUN-RAW .  s\" raw: end\" type"
   {: src:ptr srcu:n :}
   src srcu RAW-OUT RAW-CAP >LEN RAW-ERR RAW-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   RAW-OUT outu LEN>N s\" 99\nraw: end" T$=
   src srcu RAW-OUT outu LEN>N RAW-ERR erru LEN>N oc 0 T-ENDING-EXITED= ;
;package

\ A skipped line fails nothing in its own process, so the case runs in a child.
POSTPONE-REFUSE:RAW-CHILD

T-REPORT
