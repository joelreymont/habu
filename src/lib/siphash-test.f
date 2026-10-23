\ siphash-test.f - SIPHASH against the SipHash-2-4 reference vectors, and
\ ENTROPY:FILL against the OS generator.
\
\ SIPT-VEC is vectors_sip64 of the reference implementation
\ (github.com/veorq/SipHash, vectors.h): entry i is SipHash-2-4 of the bytes
\ 00 01 .. i-1 under the key 00 01 .. 0f, its eight output bytes read
\ little-endian. Every length 0..63 is checked through BYTES, through
\ ABSORB-BYTE alone, and through ABSORB-WORD at every alignment.
\
\ SIPT-LONG-VEC holds the reference siphash.c's outputs, read the same way,
\ for lengths 255, 256 and 257 of the bytes (i*37+$80) & $FF under that key:
\ high-bit bytes, and lengths whose low byte, the only one the final block
\ carries, wraps.

require src/lib/errors.f
require src/lib/test.f
require src/lib/siphash.f
require src/lib/entropy.f

$0706050403020100 constant SIPT-K0
$0F0E0D0C0B0A0908 constant SIPT-K1
64 constant SIPT-N
255 constant SIPT-LONG
3 constant SIPT-LONG-COUNT
SIPT-LONG SIPT-LONG-COUNT + 1- constant SIPT-MSG-N

create SIPT-VEC
   $726FDB47DD0E0E31 , $74F839C593DC67FD , $0D6C8009D9A94F5A , $85676696D7FB7E2D ,
   $CF2794E0277187B7 , $18765564CD99A68D , $CBC9466E58FEE3CE , $AB0200F58B01D137 ,
   $93F5F5799A932462 , $9E0082DF0BA9E4B0 , $7A5DBBC594DDB9F3 , $F4B32F46226BADA7 ,
   $751E8FBC860EE5FB , $14EA5627C0843D90 , $F723CA908E7AF2EE , $A129CA6149BE45E5 ,
   $3F2ACC7F57C29BDB , $699AE9F52CBE4794 , $4BC1B3F0968DD39C , $BB6DC91DA77961BD ,
   $BED65CF21AA2EE98 , $D0F2CBB02E3B67C7 , $93536795E3A33E88 , $A80C038CCD5CCEC8 ,
   $B8AD50C6F649AF94 , $BCE192DE8A85B8EA , $17D835B85BBB15F3 , $2F2E6163076BCFAD ,
   $DE4DAAACA71DC9A5 , $A6A2506687956571 , $AD87A3535C49EF28 , $32D892FAD841C342 ,
   $7127512F72F27CCE , $A7F32346F95978E3 , $12E0B01ABB051238 , $15E034D40FA197AE ,
   $314DFFBE0815A3B4 , $027990F029623981 , $CADCD4E59EF40C4D , $9ABFD8766A33735C ,
   $0E3EA96B5304A7D0 , $AD0C42D6FC585992 , $187306C89BC215A9 , $D4A60ABCF3792B95 ,
   $F935451DE4F21DF2 , $A9538F0419755787 , $DB9ACDDFF56CA510 , $D06C98CD5C0975EB ,
   $E612A3CB9ECBA951 , $C766E62CFCADAF96 , $EE64435A9752FE72 , $A192D576B245165A ,
   $0A8787BF8ECB74B2 , $81B3E73D20B49B6F , $7FA8220BA3B2ECEA , $245731C13CA42499 ,
   $B78DBFAF3A8D83BD , $EA1AD565322A1A0B , $60E61C23A3795013 , $6606D7E446282B93 ,
   $6CA4ECB15C5F91E1 , $9F626DA15C9625F3 , $E51B38608EF25F57 , $958A324CEB064572 ,

create SIPT-LONG-VEC
   $AEE2708C19355DBA , $957DC0A55D8B7D45 , $34DB33BC2A621FFD ,

create SIPT-MSG SIPT-MSG-N allot
create SIPT-STATE SIPHASH:STATE-CELLS cells allot

: SIPT-WANT ( n -- n ) cells SIPT-VEC + @ ;

: SIPT-FILL-MSG ( -- ) SIPT-N 0 ?do i SIPT-MSG i + c! loop ;

: SIPT-FILL-HIGH ( -- )
   SIPT-MSG-N 0 ?do i 37 * $80 + $FF and SIPT-MSG i + c! loop ;

: SIPT-START ( -- ) SIPT-STATE SIPT-K0 SIPT-K1 SIPHASH:INIT ;

\ Append message bytes [from, to) one at a time.
: SIPT-BYTES+ ( n n -- )
   {: from:n to:n :}
   to from ?do SIPT-STATE SIPT-MSG i + c@ SIPHASH:ABSORB-BYTE loop ;


\ The message's eight bytes at off as one little-endian cell.
: SIPT-WORD@ ( n -- n )
   {: off:n :}
   0 8 0 ?do SIPT-MSG off + i + c@ i 8 * lshift or loop ;


\ Hash the first len bytes: lead bytes singly, then as many whole words as fit,
\ then the rest singly. A lead of 1..7 puts every word off the block boundary.
: SIPT-MIXED ( n n -- n )
   {: len:n lead:n :}
   SIPT-START
   0 lead SIPT-BYTES+
   len lead - 8 / {: words:n :}
   words 0 ?do SIPT-STATE  lead i 8 * + SIPT-WORD@  SIPHASH:ABSORB-WORD loop
   lead words 8 * + len SIPT-BYTES+
   SIPT-STATE SIPHASH:FINISH ;


: SIPT-CHECK ( n n -- )
   {: len:n want:n :}
   SIPT-K0 SIPT-K1 SIPT-MSG len SIPHASH:BYTES  want T=
   SIPT-START  0 len SIPT-BYTES+  SIPT-STATE SIPHASH:FINISH  want T=
   len 1+ 8 min 0 ?do len i SIPT-MIXED  want T= loop ;


\ ENTROPY fixtures. SIPT-BIG spans more than one getentropy chunk; the bytes
\ past the first 256 come only from the second.
32 constant SIPT-SMALL
300 constant SIPT-BIG-N
256 constant SIPT-CHUNK
create SIPT-A SIPT-SMALL allot
create SIPT-B SIPT-SMALL allot
create SIPT-BIG SIPT-BIG-N allot

: SIPT-ZERO ( ptr u8 n -- )
   {: p len:n :}
   len 0 ?do 0 p i + c! loop ;


\ How many of the first n bytes differ between two spans.
: SIPT-DIFFS ( ptr u8 ptr u8 n -- n )
   {: p q len:n :}
   0 len 0 ?do p i + c@ q i + c@ <> if 1+ then loop ;


: SIPT-NONZERO ( ptr u8 n -- n )
   {: p len:n :}
   0 len 0 ?do p i + c@ 0<> if 1+ then loop ;


: SIPT-RUN ( -- )
   T-RESET
   SIPT-FILL-MSG
   SIPT-N 0 ?do i  i SIPT-WANT  SIPT-CHECK loop
   \ the key is two distinct halves: swapping them changes the answer
   SIPT-K1 SIPT-K0 SIPT-MSG 8 SIPHASH:BYTES  8 SIPT-WANT T<>
   SIPT-FILL-HIGH
   SIPT-LONG-COUNT 0 ?do
      SIPT-LONG i +  i cells SIPT-LONG-VEC + @  SIPT-CHECK
   loop
   [: SIPT-K0 SIPT-K1 SIPT-MSG -1 SIPHASH:BYTES drop ;] E-SPAN-LENGTH TTHROWSQ
   \ two fills of 32 bytes from the OS generator differ
   SIPT-A SIPT-SMALL SIPT-ZERO  SIPT-B SIPT-SMALL SIPT-ZERO
   SIPT-A SIPT-SMALL ENTROPY:FILL  SIPT-B SIPT-SMALL ENTROPY:FILL
   SIPT-A SIPT-B SIPT-SMALL SIPT-DIFFS 0 T<>
   \ 300 bytes cross the 256-byte chunk: the second chunk's 44 bytes and the
   \ last 64 are not left zero
   SIPT-BIG SIPT-BIG-N SIPT-ZERO
   SIPT-BIG SIPT-BIG-N ENTROPY:FILL
   SIPT-BIG SIPT-CHUNK +  SIPT-BIG-N SIPT-CHUNK -  SIPT-NONZERO 0 T<>
   SIPT-BIG SIPT-BIG-N 64 - +  64 SIPT-NONZERO 0 T<>
   [: SIPT-A -1 ENTROPY:FILL ;] E-SPAN-LENGTH TTHROWSQ ;

SIPT-RUN
T-REPORT
