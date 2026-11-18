split-str
\ A definition that leaves more values than a diagnostic row holds: its
\ rejection says how many it left out.
s" LONG-ROW-ARMED" type cr
: MAKE-LONG-ROW ( -- )
   s" LONG-ROW" EFFECT:EMPTY define-word
   5000 0 do 1 literal loop
   postpone ; ;
MAKE-LONG-ROW
