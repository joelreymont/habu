\ evaluated.f - a definer opens a word from this file's text, then evaluates a
\ string whose immediate word appends a call of drop, which the empty word
\ cannot feed, called on line 19. The call's bytes lie in the string, whose
\ leading spaces end at an offset that lies on line 1 of this file, so the
\ rejection names no line of this file but the definer's call.

package BITEM-EVALUATED
private

: DROP-ITEM ( -- ) s" drop" EFFECT:CALL ; immediate
s" DROP-ITEM" 0 parse-imm

\ : NAME ( -- ) drop ;
: MAKE-EVALUATED ( ^u8 n -- )
   EFFECT:EMPTY define-word
   s"           DROP-ITEM" evaluate-closed
   postpone ; ;

s" BI-EVALUATED" MAKE-EVALUATED

;package
