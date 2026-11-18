split-str
\ A definition that leaves a quotation whose own row holds more values than a
\ diagnostic row holds: its rejection says how many it left out.
s" LONG-QUOT-ARMED" type cr
: MAKE-LONG-QUOT ( -- )
   s" LONG-QUOT" EFFECT:EMPTY define-word
   s" [:" EFFECT:KEYWORD
   5000 0 do 1 literal loop
   s" ;]" EFFECT:KEYWORD
   postpone ; ;
MAKE-LONG-QUOT
