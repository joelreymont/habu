\ bytes.f - core byte-buffer helpers.

$4C constant E-BYTE-RANGE

: BYTE-CHECK-N ( n -- )
   dup 0 < if s" byte: negative length" E-BYTE-RANGE die then
   drop ;

: BYTE+ ( ptr u8 n -- ptr u8 )
   + ;

: BYTE@ ( ptr u8 n -- n )
   BYTE+ c@ ;

: BYTE-COPY-LEN ( ptr u8 ptr u8 len -- ) {: src:ptr dst:ptr u:len :}
   0 begin dup u LEN>N < while
      dup src + c@ over dst + c!
      1+
   repeat drop ;

: BYTE-COPY ( ptr u8 ptr u8 n -- )
   dup BYTE-CHECK-N >LEN BYTE-COPY-LEN ;
