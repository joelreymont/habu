\ ' of a word whose effect is wider than a cell is refused (habu2.f:5772 C-TICK).
ENUM shape 0
   VARIANT dot ;VARIANT
   VARIANT circle FIELD r n ;VARIANT
;ENUM
: CIRC ( -- shape ) 3 construct shape circle ;
' CIRC drop
." after" cr
