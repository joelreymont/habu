\ bytes.f - core byte-buffer helpers every engine loads at startup, so
\ low-level code needs no library require. BYTE-VIEW ( ^a -- ^u8 ) is an engine word: checked code rejects
\ a cell @ or ! on its view (src/lib/string-test.f BAD-BYTE-VIEW-LOAD).

: BYTE+ ( ^u8 n -- ^u8 )
   + ;

: BYTE@ ( ^u8 n -- n )
   BYTE+ c@ ;

\ Forward byte semantics also define overlapping copies: a destination inside
\ the source repeats the bytes already written. This is not memmove.
: BYTE-COPY-LEN ( ^u8 ^u8 len -- ) {: src:^ dst:^ u:len :}
   u LEN>N {: count :}
   count 0 <= if exit then
   0
   \ CELL-VIEW keeps the addresses checked. Never read or write beyond the
   \ complete-cell prefix.
   \ A forward overlap narrower than a cell needs the original byte sequence.
   dst src > dst src CELL + < and 0= if
      count CELL / CELL * {: bulk :}
      begin dup bulk < while
         dup src + CELL-VIEW @ over dst + CELL-VIEW ! CELL +
      repeat
   then
   begin dup count < while
      dup src + c@ over dst + c! 1+
   repeat drop ;

: BYTE-COPY ( ^u8 ^u8 n -- )
   >LEN BYTE-COPY-LEN ;
