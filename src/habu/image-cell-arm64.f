\ ARM64 startup projection of IMAGE-CELLS. The callers supply their framing
\ and free registers; all image kinds use this same group/bitmap/value walk.
\ Load after asm.f and icode.f, like the engine emitter that includes this
\ file: the recovery builders inline those two before requiring any emitter.
require src/habu/image-cells.f

package IMAGE-CELL-ARM64
using A64ASM
using IMAGE-CELLS

: VALUE, ( n n n n n bool label -- )
   {: acc:n cur:n byte:n group:n shift:n checked:bool bad:label :}
   LBL {: top:label :}
   acc 0 MOVZ, shift 0 MOVZ,
   top LBL,
      checked IF cur 0 CMP, C-CS bad BCOND, THEN
      byte cur 0 LDRB, cur cur 1 ADDI,
      checked IF
         LBL {: narrow:label :}
         shift 63 CMPI, C-NE narrow BCOND,
         byte 1 CMPI, C-HI bad BCOND,
         narrow LBL,
      THEN
      group byte $7F ANDI, group group shift LSLV, acc acc group ORR,
      shift shift 7 ADDI,
      group byte $80 ANDI, group top CBNZ,
   checked IF
      LBL {: minimal:label :}
      acc bad CBZ,
      shift 7 CMPI, C-EQ minimal BCOND,
      byte bad CBZ,
      minimal LBL,
   THEN ;

: STORE, ( n n bool label -- ) {: acc:n dst:n checked:bool bad:label :}
   checked IF
      LBL LBL LBL {: full:label tail:label done:label :}
      dst 1 CMP, C-CS bad BCOND,
      2 dst CELL-BYTES ADDI, 2 1 CMP, C-LS full BCOND,
      2 1 dst SUB, 5 2 3 LSLI, 5 acc 5 LSRV, 5 bad CBNZ,
      5 dst 0 ADDI,
      tail LBL,
         acc 5 0 STRB, acc acc 8 LSRI, 5 5 1 ADDI,
         2 2 1 SUBI, 2 tail CBNZ,
      done B,
      full LBL, acc dst 0 STR,
      done LBL,
   ELSE acc dst 0 STR, THEN ;

: GROUP, ( n n n n n n n n n n n bool label -- )
   {: bm:n values:n dst:n leftb:n bits:n left:n test:n acc:n
      byte:n group:n shift:n checked:bool bad:label :}
   LBL LBL LBL LBL LBL LBL
   {: bmtop:label bmdone:label bittop:label bitdone:label bitnext:label empty:label :}
   checked IF
      2 bm GROUP-BYTES ADDI, 2 6 CMP, C-HI bad BCOND,
      4 0 MOVZ,
   THEN
   leftb GROUP-BYTES MOVZ,
   bmtop LBL,
      leftb bmdone CBZ,
      bits bm 0 LDRB, bm bm 1 ADDI,
      checked IF 4 4 bits ORR, THEN
      leftb leftb 1 SUBI,
      bits empty CBZ,
      left CELL-BITS MOVZ,
      bittop LBL,
         left bitdone CBZ,
         test bits 1 ANDI,
         test bitnext CBZ,
            acc values byte group shift checked bad VALUE,
            acc dst checked bad STORE,
         bitnext LBL,
         bits bits 1 LSRI, dst dst CELL-BYTES ADDI, left left 1 SUBI,
         bittop B,
      bitdone LBL,
      bmtop B,
   empty LBL,
      dst dst BM-BYTE-SPAN ADDI,
      bmtop B,
   bmdone LBL,
   checked IF 4 bad CBZ, THEN ;

\ Input registers: presence cursor/end, stored-group cursor, value cursor,
\ destination. Scratch registers follow in loop order. Cursors advance, other
\ registers are untouched. The destination has already been zeroed.
: COPY-INNER, ( n n n n n n n n n n n n n n n bool label -- )
   {: map:n end:n bm:n values:n dst:n groups:n leftg:n leftb:n
      bits:n left:n test:n acc:n byte:n group:n shift:n checked:bool bad:label :}
   LBL LBL LBL LBL LBL
   {: gtop:label gdone:label ptop:label pnext:label absent:label :}
   checked IF 6 values 0 ADDI, THEN
   gtop LBL,
      map end CMP, C-CS gdone BCOND,
      groups map 0 LDRB, map map 1 ADDI,
      leftg CELL-BITS MOVZ,
      ptop LBL,
         leftg gtop CBZ,
         test groups 1 ANDI,
         test absent CBZ,
            bm values dst leftb bits left test acc byte group shift checked bad GROUP,
         pnext LBL,
         groups groups 1 LSRI, leftg leftg 1 SUBI,
         ptop B,
   absent LBL,
      test GROUP-SPAN MOVZ, dst dst test ADD,
      pnext B,
   gdone LBL,
   checked IF bm 6 CMP, C-NE bad BCOND, THEN ;

public
\ Baked and stripped streams are emitted by the same build that emits their
\ reader. Keep their instruction sequence unchanged by the checked projection.
: COPY, ( n n n n n n n n n n n n n n n -- )
   false LBL COPY-INNER, ;

\ Snapshot input is untrusted. x0 bounds encoded bytes, x1 bounds scratch DATA;
\ x2/x4/x5/x6 are additional scratch and must not be any supplied register.
\ Header/map framing is checked by the caller; this walk checks every group,
\ varint and store, including the partial final cell. A failure branches to bad.
: CHECKED-COPY, ( n n n n n n n n n n n n n n n label -- )
   true swap COPY-INNER, ;

;using
;using
;package
