\ sha256.fs — SHA-256 in Forth (32-bit math masked into 64-bit cells). Used to
\ self-sign Mach-O binaries (ad-hoc CodeDirectory) so caf needs no external
\ codesign. Verified against the FIPS-180 "abc" vector. TRUSTED (bit math).
\ Names are distinct under gforth's case-folding: big sigma BSIG0/1, small SSIG0/1.
$FFFFFFFF constant W32
: m32 ( x -- x ) W32 and ;
: rotr ( x n -- x ) {: n :} dup n rshift swap 32 n - lshift or m32 ;
: shr ( x n -- x ) rshift ;
: ch  ( x y z -- w ) {: x y z :} x y and  x invert z and  xor ;
: maj ( x y z -- w ) {: x y z :} x y and  x z and  y z and  xor xor ;
: BSIG0 ( x -- w ) dup 2 rotr  over 13 rotr  rot 22 rotr  xor xor ;
: BSIG1 ( x -- w ) dup 6 rotr  over 11 rotr  rot 25 rotr  xor xor ;
: SSIG0 ( x -- w ) dup 7 rotr  over 18 rotr  rot 3 shr  xor xor ;
: SSIG1 ( x -- w ) dup 17 rotr  over 19 rotr  rot 10 shr  xor xor ;
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
: be32@ ( a -- w ) dup c@ 24 lshift  over 1+ c@ 16 lshift or  over 2 + c@ 8 lshift or  swap 3 + c@ or ;
: STv ( i -- w )  cells ST + @ ;
\ one compression round for schedule index i (locals at word scope, not in a loop)
: sha-round ( ri -- ) {: ri :}
   7 STv  4 STv BSIG1 +  4 STv 5 STv 6 STv ch +  KK ri cells + @ +  WS ri cells + @ +  m32  {: t1 :}
   0 STv BSIG0  0 STv 1 STv 2 STv maj +  m32  {: t2 :}
   7 0 ?do  ST 6 i - cells + @  ST 7 i - cells + !  loop    \ h=g g=f f=e d=c c=b b=a
   4 STv t1 + m32  ST 4 cells + !                           \ e = d + t1
   t1 t2 + m32  ST 0 cells + ! ;                            \ a = t1 + t2
\ process one 64-byte block at addr
: sha-block ( a -- )
   16 0 ?do  dup i 4 * + be32@  WS i cells + !  loop drop
   64 16 ?do
     WS i 2 - cells + @ SSIG1  WS i 7 - cells + @ +  WS i 15 - cells + @ SSIG0 +  WS i 16 - cells + @ +  m32
     WS i cells + !  loop
   8 0 ?do  H i cells + @  ST i cells + !  loop
   64 0 ?do  i sha-round  loop
   8 0 ?do  H i cells + @  ST i cells + @ +  m32  H i cells + !  loop ;
: sha-init ( -- ) 8 0 ?do  HH0 i cells + @  H i cells + !  loop ;
\ big-endian 32-bit store
: be32! ( w a -- ) {: w a :}
   w 24 rshift 255 and a c!  w 16 rshift 255 and a 1+ c!
   w 8 rshift 255 and a 2 + c!  w 255 and a 3 + c! ;
\ big-endian 64-bit store
: be64! ( x a -- ) {: x a :}  8 0 ?do  x 56 i 8 * - rshift 255 and  a i + c!  loop ;
\ pad the tail [tail,tail+tl) of a u-byte message into PBLK; returns block count (1|2)
create PBLK 128 allot
: sha-pad ( tail tl ubytes -- nblk ) {: tail tl ub :}
   PBLK 128 0 fill  tail PBLK tl move  $80 PBLK tl + c!
   tl 56 < if 64 else 128 then {: blen :}
   ub 8 * PBLK blen 8 - + be64!
   blen 64 / ;
\ SHA-256 of [a,u) -> writes 32 bytes to dst
: sha256 ( a u dst -- ) {: a u dst :}  sha-init
   u 64 / {: nb :}
   nb 0 ?do  a i 64 * +  sha-block  loop
   a nb 64 * +  u nb 64 * -  u  sha-pad {: nblk :}
   PBLK sha-block  nblk 1 > if PBLK 64 + sha-block then
   8 0 ?do  H i cells + @  dst i 4 * + be32!  loop ;
