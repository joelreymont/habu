\ src/core/sha256.f — SHA-256 in the STANDALONE's Forth (decimal constants, DO/LOOP,
\ no move/fill/hex). Lets the standalone self-sign its
\ own Mach-O with zero gforth and zero external codesign. Verified against FIPS-180.
4294967295 constant W32

: M32 W32 and ;

: ROTR {: n :} dup n rshift swap 32 n - lshift or M32 ;

: SHR rshift ;

: CH {: x y z :} x y and  x invert z and  xor ;

: MAJ {: x y z :} x y and  x z and  y z and  xor xor ;

: BSIG0 dup 2 ROTR  over 13 ROTR  rot 22 ROTR  xor xor ;

: BSIG1 dup 6 ROTR  over 11 ROTR  rot 25 ROTR  xor xor ;

: SSIG0 dup 7 ROTR  over 18 ROTR  rot 3 SHR  xor xor ;

: SSIG1 dup 17 ROTR  over 19 ROTR  rot 10 SHR  xor xor ;
create KK
  1116352408 , 1899447441 , 3049323471 , 3921009573 , 961987163 , 1508970993 , 2453635748 , 2870763221 ,
  3624381080 , 310598401 , 607225278 , 1426881987 , 1925078388 , 2162078206 , 2614888103 , 3248222580 ,
  3835390401 , 4022224774 , 264347078 , 604807628 , 770255983 , 1249150122 , 1555081692 , 1996064986 ,
  2554220882 , 2821834349 , 2952996808 , 3210313671 , 3336571891 , 3584528711 , 113926993 , 338241895 ,
  666307205 , 773529912 , 1294757372 , 1396182291 , 1695183700 , 1986661051 , 2177026350 , 2456956037 ,
  2730485921 , 2820302411 , 3259730800 , 3345764771 , 3516065817 , 3600352804 , 4094571909 , 275423344 ,
  430227734 , 506948616 , 659060556 , 883997877 , 958139571 , 1322822218 , 1537002063 , 1747873779 ,
  1955562222 , 2024104815 , 2227730452 , 2361852424 , 2428436474 , 2756734187 , 3204031479 , 3329325298 ,
create HH0
  1779033703 , 3144134277 , 1013904242 , 2773480762 , 1359893119 , 2600822924 , 528734635 , 1541459225 ,
create H 64 allot   create WS 512 allot   create ST 64 allot

4096 constant SHA-IO-CAP
-1 constant SHA-E-OPEN
-2 constant SHA-E-READ

create SHA-TAIL 64 allot
create SHA-IO SHA-IO-CAP allot
create SHA-DIGEST 32 allot

variable SHA-TAIL-U
variable SHA-TOTAL
variable SHA-A
variable SHA-U
variable SHA-NEED
variable SHA-NBLK
variable SHA-FD
variable SHA-RD
variable SHA-T1
variable SHA-T2
variable SHA-BLEN

: BE32@ ( ptr u8 -- n )
   dup c@ 24 lshift  over 1 + c@ 16 lshift or  over 2 + c@ 8 lshift or  swap 3 + c@ or ;

: STV cells ST + @ ;

\ one compression round for schedule index ri (local named ri, not i, to keep loop-i)
: SHA-ROUND {: ri :}
   7 STV  4 STV BSIG1 +  4 STV 5 STV 6 STV CH +  KK ri cells + @ +  WS ri cells + @ +  M32  SHA-T1 !
   0 STV BSIG0  0 STV 1 STV 2 STV MAJ +  M32  SHA-T2 !
   7 0 DO  ST 6 i - cells + @  ST 7 i - cells + !  LOOP   \ h=g g=f f=e d=c c=b b=a
   4 STV SHA-T1 @ + M32  ST 4 cells + !                     \ e = d + t1
   SHA-T1 @ SHA-T2 @ + M32  ST 0 cells + ! ;                \ a = t1 + t2

: SHA-BLOCK ( ptr u8 -- )
   {: a:ptr :}
   16 0 DO  a i 4 * + BE32@  WS i cells + !  LOOP
   64 16 DO
     WS i 2 - cells + @ SSIG1  WS i 7 - cells + @ +  WS i 15 - cells + @ SSIG0 +  WS i 16 - cells + @ +  M32
     WS i cells + !  LOOP
   8 0 DO  H i cells + @  ST i cells + !  LOOP
   64 0 DO  i SHA-ROUND  LOOP
   8 0 DO  H i cells + @  ST i cells + @ +  M32  H i cells + !  LOOP ;

: SHA-INIT 8 0 DO  HH0 i cells + @  H i cells + !  LOOP ;

: SHA256-RESET ( -- )
   SHA-INIT
   0 SHA-TAIL-U !
   0 SHA-TOTAL ! ;

: BE32! ( n ptr u8 -- )
   {: w a:ptr :}
   w 24 rshift 255 and a c!  w 16 rshift 255 and a 1 + c!
   w 8 rshift 255 and a 2 + c!  w 255 and a 3 + c! ;

: BE64! ( n ptr u8 -- )
   {: x a:ptr :}  8 0 DO  x 56 i 8 * - rshift 255 and  a i + c!  LOOP ;

: ZFILL ( ptr u8 n -- )
   {: a:ptr n :}  n 0 DO  0 a i + c!  LOOP ;

\ NB: the standalone's plain DO is do-while (0 0 DO runs once), so every loop that
\ can have zero trips is guarded with `0 > if ... then`.
: BMOVE ( ptr u8 ptr u8 n -- )
   {: src:ptr dst:ptr n :}  n 0 > if  n 0 DO  src i + c@ dst i + c!  LOOP  then ;
create PBLK 128 allot

: SHA-TAKE-TAIL ( n -- )
   {: n :}
   SHA-A @ SHA-TAIL SHA-TAIL-U @ + n BMOVE
   SHA-TAIL-U @ n + SHA-TAIL-U !
   SHA-A @ n + SHA-A !
   SHA-U @ n - SHA-U ! ;

\ pad tail [tail,tail+tl) of a ub-byte message into PBLK; returns block count (1|2)
: SHA-PAD ( ptr u8 n n -- n )
   {: tail:ptr tl ub :}
   PBLK 128 ZFILL  tail PBLK tl BMOVE  128 PBLK tl + c!
   tl 56 < if 64 else 128 then SHA-BLEN !
   ub 8 *  PBLK SHA-BLEN @ 8 - +  BE64!
   SHA-BLEN @ 64 / ;

\ SHA-256 of [a,u) -> writes 32 bytes to dst
: SHA256-UPDATE ( ptr u8 n -- )
   {: a:ptr u :}
   a SHA-A !
   u SHA-U !
   SHA-TOTAL @ u + SHA-TOTAL !
   SHA-TAIL-U @ 0 > if
      SHA-U @ 0 > if
         64 SHA-TAIL-U @ - SHA-NEED !
         SHA-U @ SHA-NEED @ < if SHA-U @ SHA-TAKE-TAIL exit then
         SHA-NEED @ SHA-TAKE-TAIL
         SHA-TAIL SHA-BLOCK
         0 SHA-TAIL-U !
      then
   then
   begin SHA-U @ 64 >= while
      SHA-A @ SHA-BLOCK
      SHA-A @ 64 + SHA-A !
      SHA-U @ 64 - SHA-U !
   repeat
   SHA-U @ 0 > if
      SHA-A @ SHA-TAIL SHA-U @ BMOVE
      SHA-U @ SHA-TAIL-U !
   then ;

: SHA256-FINAL ( ptr u8 -- )
   {: dst:ptr :}
   SHA-TAIL SHA-TAIL-U @ SHA-TOTAL @ SHA-PAD SHA-NBLK !
   PBLK SHA-BLOCK
   SHA-NBLK @ 1 > if PBLK 64 + SHA-BLOCK then
   8 0 DO  H i cells + @  dst i 4 * + BE32!  LOOP ;

: SHA256 ( ptr u8 n ptr u8 -- )
   {: a:ptr u dst:ptr :}
   SHA256-RESET
   a u SHA256-UPDATE
   dst SHA256-FINAL ;

: NIB>HEX ( n -- n )
   dup 10 < if 48 + else 87 + then ;

: BYTE>HEX ( n ptr u8 -- )
   {: b dst:ptr :}
   b 4 rshift 15 and NIB>HEX dst c!
   b 15 and NIB>HEX dst 1 + c! ;

: SHA256>HEX ( ptr u8 ptr u8 -- )
   {: dg:ptr dst:ptr :}
   32 0 DO  dg i + c@  dst i 2 * +  BYTE>HEX  LOOP ;

: SHA-CLOSE ( -- )
   SHA-FD @ 0 >= if SHA-FD @ close then ;

: SHA256-FILE ( ptr u8 n ptr u8 -- n )
   {: pa:ptr pu dst:ptr :}
   SHA256-RESET
   pa pu path0 open-rd SHA-FD !
   SHA-FD @ 0 < if SHA-E-OPEN exit then
   begin
      SHA-FD @ SHA-IO SHA-IO-CAP read SHA-RD !
      SHA-RD @ 0 > while
      SHA-IO SHA-RD @ SHA256-UPDATE
   repeat
   SHA-RD @ 0 < if SHA-CLOSE SHA-E-READ exit then
   SHA-CLOSE
   dst SHA256-FINAL
   0 ;

: SHA256-FILE-HEX ( ptr u8 n ptr u8 -- n )
   {: pa:ptr pu dst:ptr :}
   pa pu SHA-DIGEST SHA256-FILE dup 0 <> if exit then
   drop
   SHA-DIGEST dst SHA256>HEX
   0 ;
