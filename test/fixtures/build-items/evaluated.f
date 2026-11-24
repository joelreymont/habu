\ evaluated.f - a definer opens a word from this file's text, then evaluates a
\ string that appends a call of drop, which the empty word cannot feed, called
\ on line 16. The call's bytes lie in the string, whose leading spaces end at
\ an offset that lies on line 1 of this file, so the rejection names no line of
\ this file but the definer's call.

package BITEM-EVALUATED
private

\ : NAME ( -- ) drop ;
: MAKE-EVALUATED ( str -- )
   EFFECT:EMPTY define-word
   s"           drop" evaluate-closed
   postpone ; ;

s" BI-EVALUATED" MAKE-EVALUATED

;package
