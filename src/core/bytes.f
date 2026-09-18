\ bytes.f - core byte-buffer helpers.

$4C constant E-BYTE-RANGE

: BYTE-CHECK-N ( n -- )
   dup 0 < if s" byte: negative length" E-BYTE-RANGE die then
   drop ;

: BYTE+ ( ptr u8 n -- ptr u8 )
   + ;

: BYTE@ ( ptr u8 n -- n )
   BYTE+ c@ ;

\ Forward byte semantics also define overlapping copies: a destination inside
\ the source repeats the bytes already written. This is not memmove.
: BYTE-COPY-LEN ( ptr u8 ptr u8 len -- ) {: src:ptr dst:ptr u:len :}
   u LEN>N {: count :}
   count 0 <= if exit then
   0
   \ CELL-VIEW keeps the addresses checked. The supported targets admit
   \ unaligned cell access; never read or write beyond the complete-cell prefix.
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

: BYTE-COPY ( ptr u8 ptr u8 n -- )
   dup BYTE-CHECK-N >LEN BYTE-COPY-LEN ;
