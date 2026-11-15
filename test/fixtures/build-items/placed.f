\ placed.f - a definer whose built body reads a local after its `{:` block
\ ended, called on line 19. The built body is spelled from the definer's
\ strings, so its rejection is placed at that call: placed.f:19.

package BITEM-PLACED
private

: KW ( ^u8 n -- ) EFFECT:KEYWORD ;
: GROUP ( -- n ) s" {:" KW  EFFECT:N EFFECT:LOCAL  s" :}" KW ;

\ : NAME ( n bool -- n ) if {: a:n :} a then {: b:n :} a ;
: MAKE-PLACED ( ^u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN s" bool" EFFECT:NAMED EFFECT:IN
   EFFECT:N EFFECT:OUT define-word
   s" if" KW  GROUP {: a:n :}  a EFFECT:LOCAL@  s" then" KW
   GROUP drop  a EFFECT:LOCAL@
   postpone ; ;

s" BI-PLACED" MAKE-PLACED

;package
