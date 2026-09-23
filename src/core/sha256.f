\ src/core/sha256.f — SHA-256 in the STANDALONE's Forth ($hex constants, DO/LOOP,
\ no move/fill/hex). Lets the standalone self-sign its
\ own Mach-O with zero gforth and zero external codesign. Verified against FIPS-180.
\
\ A DIGEST IN PROGRESS IS THE CALLER'S BYTES. SHA256-BEGIN, SHA256-FEED and
\ SHA256-END work on a context the caller owns: any writable span of
\ SHA256-CTX-BYTES bytes, one per digest. Nothing on that path is process-wide,
\ so two tasks (docs/threads.md — tasks are pthreads) hash at the same time by
\ holding a context each, and a context is a plain span: copying its bytes
\ copies the digest state, and the copy finishes on its own.
\
\ THE LAYOUT, by byte offset into the span:
\   H       $40 at $00     the eight hash words, a cell each
\   TAIL    $40 at $40     the block being filled
\   TAIL-U    8 at $80     how many of those tail bytes are live
\   TOTAL     8 at $88     message bytes fed so far
\   WS     $200 at $90     the sixty-four word message schedule, a cell each
\   PBLK    $80 at $290    the padding block SHA256-END compresses
\ THE HASH STATE IS CELLS, reached through CELL-VIEW (the declared view, as in
\ src/core/checker.f SYM.PKG-U and lib/codegen.f CB-DATA): a word is one `@` or
\ one `!` at a cell index, and a word that touches several takes the view once
\ into a local. TAIL and PBLK stay byte regions reached with ZPTR+, BE32@ reads
\ the caller's block words and BE32! writes the digest out, because the input
\ and the output are byte spans by nature. Reading the schedule byte-wise cost
\ 4.4x on a 4 MiB digest, which is what this accessor is for.
\
\ A CONTEXT MUST THEREFORE BE CELL-ALIGNED. `create` aligns, whatever was
\ allotted before it, so `create CTX SHA256-CTX-BYTES allot` is aligned and so
\ is a cell-offset field of a cell-aligned record; SHA256-CTX-BYTES is itself a
\ multiple of the cell, so an array of contexts stays aligned. A misaligned span
\ is NOT diagnosed: the checker sees a `ptr u8` and AArch64 user code does not
\ fault on an unaligned cell access, so nothing would catch it at run time
\ either. The rule is stated here because it can only be stated.
\
\ SHA256-END may be called once per SHA256-BEGIN. A second END on the same
\ context reads a state the first one already padded; the result is undefined
\ and nothing diagnoses it.
\
\ A FILE CONTEXT is the caller's bytes the same way: a span of
\ SHA256-FILE-CTX-BYTES holding a digest context, the digest, the read buffer
\ and the path scratch, so SHA256-FILE-IN and SHA256-FILE-HEX-IN hash two files
\ in two tasks at once by holding one each.
\
\ SHA256-RESET / SHA256-UPDATE / SHA256-FINAL and SHA256 are the process-wide
\ one-shot form over the one static context SHA-CTX0, and SHA256-FILE /
\ SHA256-FILE-HEX are the one-shot form of the file words over the one static
\ file context SHA-FCTX0: they are for a caller that hashes one thing at a time
\ in one thread.
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

\ The cell fields are cell INDICES into the context's CELL-VIEW; the byte
\ regions are byte offsets into the span itself.
$00 constant SHA-H-CELL
$40 constant SHA-TAIL-OFF
$10 constant SHA-TAIL-U-CELL
$11 constant SHA-TOTAL-CELL
$12 constant SHA-WS-CELL
$290 constant SHA-PBLK-OFF
$310 constant SHA256-CTX-BYTES

$1000 constant SHA-IO-CAP
-1 constant SHA-E-OPEN
-2 constant SHA-E-READ

\ A FILE CONTEXT, by byte offset: the digest context at 0 (so the file context
\ is cell-aligned by the same rule), the 32-byte binary digest, the read buffer,
\ and the NUL-terminated path the open is handed — PATHZ writes it here because
\ PATH0's own scratch is process-wide and two tasks would race on it. The path
\ region is a whole number of cells so the total is one too.
SHA256-CTX-BYTES constant SHA-FILE-DG-OFF
SHA-FILE-DG-OFF $20 + constant SHA-FILE-IO-OFF
SHA-FILE-IO-OFF SHA-IO-CAP + constant SHA-FILE-PATH-OFF
SHA-FILE-PATH-OFF PATH-CAP + CELL + constant SHA256-FILE-CTX-BYTES

\ TF-SHA16 (src/core/type-family-sha.f) hashes into this one: it renders the
\ first eight bytes of a one-shot digest, so it is process-wide like SHA256.
create SHA-DIGEST $20 allot

: BE32@ ( ptr u8 -- n )
   dup 0 ZBYTE@ 24 lshift  over 1 ZBYTE@ 16 lshift or
   over 2 ZBYTE@ 8 lshift or  swap 3 ZBYTE@ or ;

: BE32! ( n ptr u8 -- ) {: w:n p :}
   w 24 rshift $FF and  p 0 ZBYTE!
   w 16 rshift $FF and  p 1 ZBYTE!
   w 8 rshift $FF and   p 2 ZBYTE!
   w $FF and            p 3 ZBYTE! ;

: BE64! ( n ptr u8 -- ) {: w:n p :}
   8 0 DO  w 56 i 8 * - rshift $FF and  p i ZBYTE!  LOOP ;

: ZFILL ( ptr u8 n -- ) {: p n:n :}
   n 0 DO  0 p i ZBYTE!  LOOP ;

\ NB: the standalone's plain DO is do-while (0 0 DO runs once), so every loop that
\ can have zero trips is guarded with `0 > if ... then`.
: BMOVE ( ptr u8 ptr u8 n -- ) {: src dst n:n :}
   n 0 > if
      n 0 DO  src i ZBYTE@  dst i ZBYTE!  LOOP
   then ;

\ The cell accessors take the context's CELL-VIEW, not the span: a caller that
\ touches several cells views it once.
: SHA-H@ ( ptr n n -- n )
   SHA-H-CELL + cells + @ ;

: SHA-H! ( n ptr n n -- )
   SHA-H-CELL + cells + ! ;

: SHA-TAIL-A ( ptr u8 -- ptr u8 )
   SHA-TAIL-OFF ZPTR+ ;

: SHA-PBLK-A ( ptr u8 -- ptr u8 )
   SHA-PBLK-OFF ZPTR+ ;

: SHA-TAIL-U@ ( ptr n -- n )
   SHA-TAIL-U-CELL cells + @ ;

: SHA-TAIL-U! ( n ptr n -- )
   SHA-TAIL-U-CELL cells + ! ;

: SHA-TOTAL@ ( ptr n -- n )
   SHA-TOTAL-CELL cells + @ ;

: SHA-TOTAL! ( n ptr n -- )
   SHA-TOTAL-CELL cells + ! ;

\ compress one $40-byte block into the context's hash words
: SHA-BLOCK ( ptr u8 ptr u8 -- ) {: ctx blk :}
   ctx CELL-VIEW {: cv :}
   \ The schedule is read and written 190 times a block, inline on purpose: the
   \ same 4 MiB pass took 72 ms through a one-line SHA-WS@/SHA-WS! pair against
   \ 42 ms with the index arithmetic here, both pinned to one P-core.
   cv SHA-WS-CELL cells + {: ws :}
   16 0 DO  blk i 4 * ZPTR+ BE32@  ws i cells + !  LOOP
   64 16 DO
      ws i 2 - cells + @ {: x:n :}
      x 17 rshift x 15 lshift or
      x 19 rshift x 13 lshift or xor
      x 10 rshift xor W32 and
      ws i 7 - cells + @ +
      ws i 15 - cells + @ {: y:n :}
      y 7 rshift y 25 lshift or
      y 18 rshift y 14 lshift or xor
      y 3 rshift xor W32 and +
      ws i 16 - cells + @ + W32 and
      ws i cells + !
   LOOP
   cv 0 SHA-H@ cv 1 SHA-H@ cv 2 SHA-H@ cv 3 SHA-H@
   cv 4 SHA-H@ cv 5 SHA-H@ cv 6 SHA-H@ cv 7 SHA-H@
   64 0 DO
      {: a:n b:n c:n d:n e:n f:n g:n h:n :}
      e 6 rshift e 26 lshift or
      e 11 rshift e 21 lshift or xor
      e 25 rshift e 7 lshift or xor W32 and
      h + f g xor e and g xor +
      KK i cells + @ + ws i cells + @ + {: t1:n :}
      a 2 rshift a 30 lshift or
      a 13 rshift a 19 lshift or xor
      a 22 rshift a 10 lshift or xor W32 and
      a b and a b or c and or + {: t2:n :}
      t1 t2 + W32 and  a  b  c  d t1 + W32 and  e  f  g
   LOOP
   {: a:n b:n c:n d:n e:n f:n g:n h:n :}
   cv 0 SHA-H@ a + W32 and  cv 0 SHA-H!
   cv 1 SHA-H@ b + W32 and  cv 1 SHA-H!
   cv 2 SHA-H@ c + W32 and  cv 2 SHA-H!
   cv 3 SHA-H@ d + W32 and  cv 3 SHA-H!
   cv 4 SHA-H@ e + W32 and  cv 4 SHA-H!
   cv 5 SHA-H@ f + W32 and  cv 5 SHA-H!
   cv 6 SHA-H@ g + W32 and  cv 6 SHA-H!
   cv 7 SHA-H@ h + W32 and  cv 7 SHA-H! ;

: SHA256-BEGIN ( ptr u8 -- )
   CELL-VIEW {: cv :}
   8 0 DO  HH0 i cells + @  cv i SHA-H!  LOOP
   0 cv SHA-TAIL-U!
   0 cv SHA-TOTAL! ;

\ append n bytes of [a,…) to the context tail, raising the live count
: SHA-TAKE-TAIL ( ptr u8 ptr u8 n -- ) {: ctx a n:n :}
   ctx CELL-VIEW {: cv :}
   a  ctx SHA-TAIL-A cv SHA-TAIL-U@ ZPTR+  n BMOVE
   cv SHA-TAIL-U@ n + cv SHA-TAIL-U! ;

\ top the tail up from [a,u) and compress it when it fills; answers what is left
: SHA-FILL-TAIL ( ptr u8 ptr u8 n -- ptr u8 n ) {: ctx a u:n :}
   ctx CELL-VIEW {: cv :}
   cv SHA-TAIL-U@ 0 = if a u exit then
   u 0 = if a u exit then
   $40 cv SHA-TAIL-U@ - {: need:n :}
   u need < if
      ctx a u SHA-TAKE-TAIL
      a u ZPTR+ 0 exit
   then
   ctx a need SHA-TAKE-TAIL
   ctx ctx SHA-TAIL-A SHA-BLOCK
   0 cv SHA-TAIL-U!
   a need ZPTR+  u need - ;

\ compress every whole block of [a,u); answers the short span left over
: SHA-BLOCKS ( ptr u8 ptr u8 n -- ptr u8 n ) {: ctx a u:n :}
   a u
   begin dup $40 >= while
      over ctx swap SHA-BLOCK
      swap $40 ZPTR+ swap $40 -
   repeat ;

: SHA256-FEED ( ptr u8 ptr u8 n -- ) {: ctx a u:n :}
   ctx CELL-VIEW {: cv :}
   cv SHA-TOTAL@ u + cv SHA-TOTAL!
   ctx a u SHA-FILL-TAIL {: a1 u1:n :}
   ctx a1 u1 SHA-BLOCKS {: a2 u2:n :}
   u2 0 > if ctx a2 u2 SHA-TAKE-TAIL then ;

\ pad the context's live tail, as the end of a ub-byte message, into its PBLK;
\ returns the block count (1|2)
: SHA-PAD ( ptr u8 n -- n ) {: ctx ub:n :}
   ctx CELL-VIEW SHA-TAIL-U@ {: tl:n :}
   ctx SHA-PBLK-A {: pblk :}
   pblk $80 ZFILL  ctx SHA-TAIL-A pblk tl BMOVE  $80 pblk tl ZBYTE!
   tl $38 < if $40 else $80 then {: blen:n :}
   ub 8 *  pblk blen 8 - ZPTR+  BE64!
   blen $40 / ;

\ finish the digest into dst, which is $20 bytes of the caller's
: SHA256-END ( ptr u8 ptr u8 -- ) {: ctx out :}
   ctx CELL-VIEW {: cv :}
   ctx cv SHA-TOTAL@ SHA-PAD {: nblk:n :}
   ctx ctx SHA-PBLK-A SHA-BLOCK
   nblk 1 > if ctx ctx SHA-PBLK-A $40 ZPTR+ SHA-BLOCK then
   8 0 DO  cv i SHA-H@  out i 4 * ZPTR+ BE32!  LOOP ;

\ THE PROCESS-WIDE ONE-SHOT CONTEXT. SHA256-RESET/UPDATE/FINAL and SHA256 hash
\ through it, so one digest at a time in one thread; a caller that needs two
\ holds two contexts of its own and calls BEGIN/FEED/END.
create SHA-CTX0 SHA256-CTX-BYTES allot

: SHA256-RESET ( -- )
   SHA-CTX0 SHA256-BEGIN ;

: SHA256-UPDATE ( ptr u8 n -- ) {: a u:n :}
   SHA-CTX0 a u SHA256-FEED ;

: SHA256-FINAL ( ptr u8 -- )
   SHA-CTX0 swap SHA256-END ;

: SHA256 ( ptr u8 n ptr u8 -- ) {: a u:n out :}
   SHA-CTX0 SHA256-BEGIN
   SHA-CTX0 a u SHA256-FEED
   SHA-CTX0 out SHA256-END ;

: NIB>HEX ( n -- n )
   dup 10 < if $30 + else $57 + then ;

: BYTE>HEX ( n ptr u8 -- ) {: w:n p :}
   w 4 rshift $F and NIB>HEX  p 0 ZBYTE!
   w $F and NIB>HEX           p 1 ZBYTE! ;

: SHA256>HEX ( ptr u8 ptr u8 -- ) {: src dst :}
   $20 0 DO  src i ZBYTE@  dst i 2 * ZPTR+  BYTE>HEX  LOOP ;

\ THE FILE CONTEXT'S PARTS.
: SHA-FILE-DG ( ptr u8 -- ptr u8 )
   SHA-FILE-DG-OFF ZPTR+ ;

: SHA-FILE-IO ( ptr u8 -- ptr u8 )
   SHA-FILE-IO-OFF ZPTR+ ;

: SHA-FILE-PATH ( ptr u8 -- ptr u8 )
   SHA-FILE-PATH-OFF ZPTR+ ;

\ Hash the file at [a,u) into out through the caller's file context. Nothing on
\ this path is process-wide: the descriptor and the byte count are locals, and
\ the digest context, the read buffer and the path the open is handed are the
\ file context's, so two tasks run this at once on two contexts of their own.
: SHA256-FILE-IN ( ptr u8 ptr u8 n ptr u8 -- n ) {: fctx a u:n out :}
   fctx SHA-FILE-IO {: io :}
   fctx SHA-FILE-PATH {: pz :}
   fctx SHA256-BEGIN
   a u pz PATHZ
   pz open-rd {: fd:n :}
   fd 0 < if SHA-E-OPEN exit then
   begin
      fd io SHA-IO-CAP read
      dup 0 > while
      fctx io rot SHA256-FEED
   repeat
   0 < if fd close SHA-E-READ exit then
   fd close
   fctx out SHA256-END
   0 ;

: SHA256-FILE-HEX-IN ( ptr u8 ptr u8 n ptr u8 -- n ) {: fctx a u:n out :}
   fctx SHA-FILE-DG {: dg :}
   fctx a u dg SHA256-FILE-IN {: rc:n :}
   rc 0 <> if rc exit then
   dg out SHA256>HEX
   0 ;

\ THE PROCESS-WIDE ONE-SHOT FILE CONTEXT, what SHA-IO and the shared digest
\ scratch were: one file at a time in one thread.
create SHA-FCTX0 SHA256-FILE-CTX-BYTES allot

: SHA256-FILE ( ptr u8 n ptr u8 -- n ) {: a u:n out :}
   SHA-FCTX0 a u out SHA256-FILE-IN ;

: SHA256-FILE-HEX ( ptr u8 n ptr u8 -- n ) {: a u:n out :}
   SHA-FCTX0 a u out SHA256-FILE-HEX-IN ;

\ SHA is process-wide scratch. A completed operation owns no caller buffer, and
\ no cell here holds one any more - every cursor the words once kept is a local
\ now - so capture has only the one-shot context to return to its empty start.
: SHA256-SNAPSHOT-PREPARE ( -- )
   SHA256-RESET ;
