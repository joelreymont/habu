\ sha256.fs — SHA-256 in Forth (32-bit math masked into 64-bit cells). Used to
\ self-sign Mach-O binaries (ad-hoc CodeDirectory) so habu needs no external
\ codesign. Verified against the FIPS-180 "abc" vector. TRUSTED (bit math).
\ Names are distinct under gforth's case-folding: big sigma BSIG0/1, small SSIG0/1.
$FFFFFFFF constant W32

: M32 ( x -- x ) W32 and ;

: ROTR ( x n -- x ) {: n :} dup n rshift swap 32 n - lshift or M32 ;

: SHR ( x n -- x ) rshift ;

: CH  ( x y z -- w ) {: x y z :} x y and  x invert z and  xor ;

: MAJ ( x y z -- w ) {: x y z :} x y and  x z and  y z and  xor xor ;

: BSIG0 ( x -- w ) dup 2 ROTR  over 13 ROTR  rot 22 ROTR  xor xor ;

: BSIG1 ( x -- w ) dup 6 ROTR  over 11 ROTR  rot 25 ROTR  xor xor ;

: SSIG0 ( x -- w ) dup 7 ROTR  over 18 ROTR  rot 3 SHR  xor xor ;

: SSIG1 ( x -- w ) dup 17 ROTR  over 19 ROTR  rot 10 SHR  xor xor ;
create KK
$428a2f98 , $71374491 , $b5c0fbcf , $e9b5dba5 , $3956c25b , $59f111f1 , $923f82a4 , $ab1c5ed5 ,
$d807aa98 , $12835b01 , $243185be , $550c7dc3 , $72be5d74 , $80deb1fe , $9bdc06a7 , $c19bf174 ,
$e49b69c1 , $efbe4786 , $0fc19dc6 , $240ca1cc , $2de92c6f , $4a7484aa , $5cb0a9dc , $76f988da ,
$983e5152 , $a831c66d , $b00327c8 , $bf597fc7 , $c6e00bf3 , $d5a79147 , $06ca6351 , $14292967 ,
$27b70a85 , $2e1b2138 , $4d2c6dfc , $53380d13 , $650a7354 , $766a0abb , $81c2c92e , $92722c85 ,
$a2bfe8a1 , $a81a664b , $c24b8b70 , $c76c51a3 , $d192e819 , $d6990624 , $f40e3585 , $106aa070 ,
$19a4c116 , $1e376c08 , $2748774c , $34b0bcb5 , $391c0cb3 , $4ed8aa4a , $5b9cca4f , $682e6ff3 ,
$748f82ee , $78a5636f , $84c87814 , $8cc70208 , $90befffa , $a4506ceb , $bef9a3f7 , $c67178f2 ,
create HH0 $6a09e667 , $bb67ae85 , $3c6ef372 , $a54ff53a , $510e527f , $9b05688c , $1f83d9ab , $5be0cd19 ,
create H 8 cells allot   create WS 64 cells allot   create ST 8 cells allot

: BE32@ ( a -- w ) dup c@ 24 lshift  over 1+ c@ 16 lshift or  over 2 + c@ 8 lshift or  swap 3 + c@ or ;

: STV ( i -- w )  cells ST + @ ;

\ one compression round for schedule index i (locals at word scope, not in a loop)
: SHA-ROUND ( ri -- ) {: ri :}
   7 STV  4 STV BSIG1 +  4 STV 5 STV 6 STV CH +  KK ri cells + @ +  WS ri cells + @ +  M32  {: t1 :}
   0 STV BSIG0  0 STV 1 STV 2 STV MAJ +  M32  {: t2 :}
   7 0 ?do  ST 6 i - cells + @  ST 7 i - cells + !  loop    \ h=g g=f f=e d=c c=b b=a
   4 STV t1 + M32  ST 4 cells + !                           \ e = d + t1
   t1 t2 + M32  ST 0 cells + ! ;                            \ a = t1 + t2

\ process one 64-byte block at addr
: SHA-BLOCK ( a -- )
   16 0 ?do  dup i 4 * + BE32@  WS i cells + !  loop drop
   64 16 ?do
     WS i 2 - cells + @ SSIG1  WS i 7 - cells + @ +  WS i 15 - cells + @ SSIG0 +  WS i 16 - cells + @ +  M32
     WS i cells + !  loop
   8 0 ?do  H i cells + @  ST i cells + !  loop
   64 0 ?do  i SHA-ROUND  loop
   8 0 ?do  H i cells + @  ST i cells + @ +  M32  H i cells + !  loop ;

: SHA-INIT ( -- ) 8 0 ?do  HH0 i cells + @  H i cells + !  loop ;

\ big-endian 32-bit store
: BE32! ( w a -- ) {: w a :}
   w 24 rshift 255 and a c!  w 16 rshift 255 and a 1+ c!
   w 8 rshift 255 and a 2 + c!  w 255 and a 3 + c! ;

\ big-endian 64-bit store
: BE64! ( x a -- ) {: x a :}  8 0 ?do  x 56 i 8 * - rshift 255 and  a i + c!  loop ;
\ pad the tail [tail,tail+tl) of a u-byte message into PBLK; returns block count (1|2)
create PBLK 128 allot

: SHA-PAD ( tail tl ubytes -- nblk ) {: tail tl ub :}
   PBLK 128 0 fill  tail PBLK tl move  $80 PBLK tl + c!
   tl 56 < if 64 else 128 then {: blen :}
   ub 8 * PBLK blen 8 - + BE64!
   blen 64 / ;

\ SHA-256 of [a,u) -> writes 32 bytes to dst
: SHA256 ( a u dst -- ) {: a u dst :}  SHA-INIT
   u 64 / {: nb :}
   nb 0 ?do  a i 64 * +  SHA-BLOCK  loop
   a nb 64 * +  u nb 64 * -  u  SHA-PAD {: nblk :}
   PBLK SHA-BLOCK  nblk 1 > if PBLK 64 + SHA-BLOCK then
   8 0 ?do  H i cells + @  dst i 4 * + BE32!  loop ;
