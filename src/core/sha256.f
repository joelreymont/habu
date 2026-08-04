\ src/core/sha256.f — SHA-256 in the STANDALONE's Forth ($hex constants, DO/LOOP,
\ no move/fill/hex). Lets the standalone self-sign its
\ own Mach-O with zero gforth and zero external codesign. Verified against FIPS-180.
$FFFFFFFF constant W32

: M32 ( n -- n )
   W32 and ;

: ROTR ( n n -- n )
   2dup rshift >r
   32 swap - lshift r> or M32 ;

: SHR ( n n -- n )
   rshift ;

: CH ( n n n -- n )
   {: x:n y:n z:n :}
   x y and  x invert z and  xor ;

: MAJ ( n n n -- n )
   {: x:n y:n z:n :}
   x y and  x z and  y z and  xor xor ;

: BSIG0 ( n -- n )
   dup 2 ROTR  over 13 ROTR  rot 22 ROTR  xor xor ;

: BSIG1 ( n -- n )
   dup 6 ROTR  over 11 ROTR  rot 25 ROTR  xor xor ;

: SSIG0 ( n -- n )
   dup 7 ROTR  over 18 ROTR  rot 3 SHR  xor xor ;

: SSIG1 ( n -- n )
   dup 17 ROTR  over 19 ROTR  rot 10 SHR  xor xor ;
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
create H $40 allot   create WS $200 allot

$1000 constant SHA-IO-CAP
-1 constant SHA-E-OPEN
-2 constant SHA-E-READ

create SHA-TAIL $40 allot
create SHA-IO SHA-IO-CAP allot
create SHA-DIGEST $20 allot

variable SHA-TAIL-U
variable SHA-TOTAL
variable SHA-A
variable SHA-U
variable SHA-NEED
variable SHA-NBLK
variable SHA-FD
variable SHA-RD
variable SHA-BLEN
variable SHA-BLOCK-A
variable SHA-P
variable SHA-SRC
variable SHA-DST
variable SHA-OUT
variable SHA-W
variable SHA-N
variable SHA-TL
variable SHA-UB

: SHA-A-FIELD ( -- ptr ptr u8 )
   SHA-A 0 ptr-field ;

: SHA-A@ ( -- ptr u8 )
   SHA-A-FIELD @ ;

: SHA-A! ( ptr u8 -- )
   SHA-A-FIELD ! ;

: BE32@ ( ptr u8 -- n )
   dup 0 ZBYTE@ 24 lshift  over 1 ZBYTE@ 16 lshift or
   over 2 ZBYTE@ 8 lshift or  swap 3 ZBYTE@ or ;

: SHA-P-FIELD ( -- ptr ptr u8 )
   SHA-P 0 ptr-field ;

: SHA-P@ ( -- ptr u8 )
   SHA-P-FIELD @ ;

: SHA-P! ( ptr u8 -- )
   SHA-P-FIELD ! ;

: SHA-SRC-FIELD ( -- ptr ptr u8 )
   SHA-SRC 0 ptr-field ;

: SHA-SRC@ ( -- ptr u8 )
   SHA-SRC-FIELD @ ;

: SHA-SRC! ( ptr u8 -- )
   SHA-SRC-FIELD ! ;

: SHA-DST-FIELD ( -- ptr ptr u8 )
   SHA-DST 0 ptr-field ;

: SHA-DST@ ( -- ptr u8 )
   SHA-DST-FIELD @ ;

: SHA-DST! ( ptr u8 -- )
   SHA-DST-FIELD ! ;

: SHA-OUT-FIELD ( -- ptr ptr u8 )
   SHA-OUT 0 ptr-field ;

: SHA-OUT@ ( -- ptr u8 )
   SHA-OUT-FIELD @ ;

: SHA-OUT! ( ptr u8 -- )
   SHA-OUT-FIELD ! ;

: SHA-BLOCK-A-FIELD ( -- ptr ptr u8 )
   SHA-BLOCK-A 0 ptr-field ;

: SHA-BLOCK-A@ ( -- ptr u8 )
   SHA-BLOCK-A-FIELD @ ;

: SHA-BLOCK-A! ( ptr u8 -- )
   SHA-BLOCK-A-FIELD ! ;

: SHA-H@ ( n -- n )
   cells H + @ ;

: SHA-H! ( n n -- )
   cells H + ! ;

: SHA-BLOCK ( ptr u8 -- )
   SHA-BLOCK-A!
   16 0 DO  SHA-BLOCK-A@ i 4 * ZPTR+ BE32@  WS i cells + !  LOOP
   64 16 DO
      WS i 2 - cells + @ {: x:n :}
      x 17 rshift x 15 lshift or
      x 19 rshift x 13 lshift or xor
      x 10 rshift xor W32 and
      WS i 7 - cells + @ +
      WS i 15 - cells + @ {: y:n :}
      y 7 rshift y 25 lshift or
      y 18 rshift y 14 lshift or xor
      y 3 rshift xor W32 and +
      WS i 16 - cells + @ + W32 and
      WS i cells + !
   LOOP
   0 SHA-H@ 1 SHA-H@ 2 SHA-H@ 3 SHA-H@
   4 SHA-H@ 5 SHA-H@ 6 SHA-H@ 7 SHA-H@
   64 0 DO
      {: a:n b:n c:n d:n e:n f:n g:n h:n :}
      e 6 rshift e 26 lshift or
      e 11 rshift e 21 lshift or xor
      e 25 rshift e 7 lshift or xor W32 and
      h + f g xor e and g xor +
      KK i cells + @ + WS i cells + @ + {: t1:n :}
      a 2 rshift a 30 lshift or
      a 13 rshift a 19 lshift or xor
      a 22 rshift a 10 lshift or xor W32 and
      a b and a b or c and or + {: t2:n :}
      t1 t2 + W32 and  a  b  c  d t1 + W32 and  e  f  g
   LOOP
   {: a:n b:n c:n d:n e:n f:n g:n h:n :}
   0 SHA-H@ a + W32 and 0 SHA-H!
   1 SHA-H@ b + W32 and 1 SHA-H!
   2 SHA-H@ c + W32 and 2 SHA-H!
   3 SHA-H@ d + W32 and 3 SHA-H!
   4 SHA-H@ e + W32 and 4 SHA-H!
   5 SHA-H@ f + W32 and 5 SHA-H!
   6 SHA-H@ g + W32 and 6 SHA-H!
   7 SHA-H@ h + W32 and 7 SHA-H! ;

: SHA-INIT ( -- )
   8 0 DO  HH0 i cells + @  H i cells + !  LOOP ;

: SHA256-RESET ( -- )
   SHA-INIT
   0 SHA-TAIL-U !
   0 SHA-TOTAL ! ;

: BE32! ( n ptr u8 -- )
   SHA-P! SHA-W !
   SHA-W @ 24 rshift $FF and SHA-P@ 0 ZBYTE!
   SHA-W @ 16 rshift $FF and SHA-P@ 1 ZBYTE!
   SHA-W @ 8 rshift $FF and SHA-P@ 2 ZBYTE!
   SHA-W @ $FF and SHA-P@ 3 ZBYTE! ;

: BE64! ( n ptr u8 -- )
   SHA-P! SHA-W !
   8 0 DO  SHA-W @ 56 i 8 * - rshift $FF and SHA-P@ i ZBYTE!  LOOP ;

: ZFILL ( ptr u8 n -- )
   SHA-N ! SHA-P!
   SHA-N @ 0 DO  0 SHA-P@ i ZBYTE!  LOOP ;

\ NB: the standalone's plain DO is do-while (0 0 DO runs once), so every loop that
\ can have zero trips is guarded with `0 > if ... then`.
: BMOVE ( ptr u8 ptr u8 n -- )
   SHA-N ! SHA-DST! SHA-SRC!
   SHA-N @ 0 > if
      SHA-N @ 0 DO  SHA-SRC@ i ZBYTE@ SHA-DST@ i ZBYTE!  LOOP
   then ;
create PBLK $80 allot

: SHA-TAKE-TAIL ( n -- )
   SHA-N !
   SHA-A@ SHA-TAIL SHA-TAIL-U @ ZPTR+ SHA-N @ BMOVE
   SHA-TAIL-U @ SHA-N @ + SHA-TAIL-U !
   SHA-A@ SHA-N @ ZPTR+ SHA-A!
   SHA-U @ SHA-N @ - SHA-U ! ;

\ pad tail [tail,tail+tl) of a ub-byte message into PBLK; returns block count (1|2)
: SHA-PAD ( ptr u8 n n -- n )
   SHA-UB ! SHA-TL ! SHA-SRC!
   PBLK $80 ZFILL  SHA-SRC@ PBLK SHA-TL @ BMOVE  $80 PBLK SHA-TL @ ZBYTE!
   SHA-TL @ $38 < if $40 else $80 then SHA-BLEN !
   SHA-UB @ 8 *  PBLK SHA-BLEN @ 8 - ZPTR+  BE64!
   SHA-BLEN @ $40 / ;

\ SHA-256 of [a,u) -> writes 32 bytes to dst
: SHA256-UPDATE ( ptr u8 n -- )
   SHA-U ! SHA-A!
   SHA-TOTAL @ SHA-U @ + SHA-TOTAL !
   SHA-TAIL-U @ 0 > if
      SHA-U @ 0 > if
         $40 SHA-TAIL-U @ - SHA-NEED !
         SHA-U @ SHA-NEED @ < if SHA-U @ SHA-TAKE-TAIL exit then
         SHA-NEED @ SHA-TAKE-TAIL
         SHA-TAIL SHA-BLOCK
         0 SHA-TAIL-U !
      then
   then
   begin SHA-U @ $40 >= while
      SHA-A@ SHA-BLOCK
      SHA-A@ $40 ZPTR+ SHA-A!
      SHA-U @ $40 - SHA-U !
   repeat
   SHA-U @ 0 > if
      SHA-A@ SHA-TAIL SHA-U @ BMOVE
      SHA-U @ SHA-TAIL-U !
   then ;

: SHA256-FINAL ( ptr u8 -- )
   SHA-OUT!
   SHA-TAIL SHA-TAIL-U @ SHA-TOTAL @ SHA-PAD SHA-NBLK !
   PBLK SHA-BLOCK
   SHA-NBLK @ 1 > if PBLK $40 ZPTR+ SHA-BLOCK then
   8 0 DO  H i cells + @  SHA-OUT@ i 4 * ZPTR+ BE32!  LOOP ;

: SHA256 ( ptr u8 n ptr u8 -- )
   SHA-OUT! SHA-U ! SHA-A!
   SHA256-RESET
   SHA-A@ SHA-U @ SHA256-UPDATE
   SHA-OUT@ SHA256-FINAL ;

: NIB>HEX ( n -- n )
   dup 10 < if $30 + else $57 + then ;

: BYTE>HEX ( n ptr u8 -- )
   SHA-P! SHA-W !
   SHA-W @ 4 rshift $F and NIB>HEX SHA-P@ 0 ZBYTE!
   SHA-W @ $F and NIB>HEX SHA-P@ 1 ZBYTE! ;

: SHA256>HEX ( ptr u8 ptr u8 -- )
   SHA-DST! SHA-SRC!
   $20 0 DO  SHA-SRC@ i ZBYTE@  SHA-DST@ i 2 * ZPTR+  BYTE>HEX  LOOP ;

: SHA-CLOSE ( -- )
   SHA-FD @ 0 >= if SHA-FD @ close then ;

: SHA256-FILE ( ptr u8 n ptr u8 -- n )
   SHA-OUT! SHA-U ! SHA-P!
   SHA256-RESET
   SHA-P@ SHA-U @ path0 open-rd SHA-FD !
   SHA-FD @ 0 < if SHA-E-OPEN exit then
   begin
      SHA-FD @ SHA-IO SHA-IO-CAP read SHA-RD !
      SHA-RD @ 0 > while
      SHA-IO SHA-RD @ SHA256-UPDATE
   repeat
   SHA-RD @ 0 < if SHA-CLOSE SHA-E-READ exit then
   SHA-CLOSE
   SHA-OUT@ SHA256-FINAL
   0 ;

: SHA256-FILE-HEX ( ptr u8 n ptr u8 -- n )
   >r
   SHA-DIGEST SHA256-FILE dup 0 <> if r> drop exit then
   drop
   SHA-DIGEST r> SHA256>HEX
   0 ;
