\ Both engines publish src/habu/layout.f's DATA-BANDS table, which their span
\ guards walk: each row's offset and length, and the hull [LO, HI), answer the
\ same on each.
: ROWS ( -- )
   0 BEGIN dup DATA-BANDS:LEN 0 <> WHILE
      dup DATA-BANDS:OFF . dup DATA-BANDS:LEN .  1+
   REPEAT drop ;
ROWS DATA-BANDS:LO . DATA-BANDS:HI .
