\ regalloc.f - the register allocator; load after mnem.f and sys.f, before jit.f.
\ VRPACK is the pool table, one byte per slot with idx 0 low: widen the pool or
\ port an arch by editing it alone. VRTAB/VRITAB map idx->reg and reg->idx,
\ filled by LVRINIT; VRFREE-CELL is the free mask by pool index (regalloc-abi.f).
\ Other files edit the mask: jit.f LVSPILL LVDROP LVNIPX LVBINPREP LVPUSHR LVSETF;
\ habu2.f the definition-start and abort resets, CFB-ENTRY around LVSNAP, mark restore.

require src/habu/regalloc-abi.f

\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
using REGALLOC-ABI

variable LVRALLOC   variable LVBIT   variable LVRINIT   variable LFRALLOC

\ LIDX holds the innermost tier 0 `do`/`?do` index between rows, a copy of the
\ loop frame's index cell (habu2.f J-FRAME, J-I). It is out of the pool: no
\ pool value lives in it, and no row's code, inlinable primitive body or
\ syscall stencil writes it.
29 constant LIDX
$0F0E0D0C0B0A09 constant VRPACK     \ x9..x15 (byte per slot, idx 0 low)
$0807061615181719 constant VRPACK2  \ overflow: x25,x23,x24,x21,x22 (tokenizer state in DATA
                                    \ cells), x6,x7,x8 (no row's code uses them while pool
                                    \ values are live)
16 constant SNAP-SLOTS              \ a snapshot frame's two pack cells hold 16 slots
: PK# ( n -- n ) 0 BEGIN over 0= 0= WHILE 1 + swap 8 rshift swap REPEAT nip ;
VRPACK PK# constant #POOL1
#POOL1 VRPACK2 PK# + constant #POOL
1 #POOL lshift 1 - constant VRALL

24 constant #FPOOL              \ d8..d31 (d0-d7 stay prim scratch; contiguous,
                                \ so idx+8 math replaces a table here)
1 #FPOOL lshift 1 - constant FRALL

\ LVRINIT ( -- ) : fill VRTAB/VRITAB from the VRPACK literal. Run once at startup
\ (idempotent; snapshot images carry the tables but re-running is harmless).
: EMIT-VRINIT ( -- )
   LVRINIT LABEL@ LBL,
   LBL {: pl :}  LBL {: pd :}  LBL {: fl :}  LBL {: fd :}
   5 0 MOVZ,
   pl LBL,                                   \ poison the inverse table
      5 VRTAB-BYTES CMPI,  C-GE pd BCOND,
      6 $FF MOVZ,  7 VRITAB-OFF LIT64,  7 DATA 7 ADD,  7 7 5 ADD,  6 7 0 STRB,
      5 5 1 ADDI,  pl B,
   pd LBL,
   8 VRPACK LIT64,  5 0 MOVZ,
   fl LBL,                                   \ walk the packed tables a byte at a time
      5 #POOL1 CMPI,  C-GE fd BCOND,
      6 8 $FF ANDI,
      7 VRTAB-OFF LIT64,  7 DATA 7 ADD,  7 7 5 ADD,  6 7 0 STRB,
      7 VRITAB-OFF LIT64,  7 DATA 7 ADD,  7 7 6 ADD,  5 7 0 STRB,
      8 8 8 LSRI,  5 5 1 ADDI,  fl B,
   fd LBL,
   LBL {: fl2 :}  LBL {: fd2 :}
   8 VRPACK2 LIT64,
   fl2 LBL,                                  \ the overflow table continues the index
      5 #POOL CMPI,  C-GE fd2 BCOND,
      6 8 $FF ANDI,
      7 VRTAB-OFF LIT64,  7 DATA 7 ADD,  7 7 5 ADD,  6 7 0 STRB,
      7 VRITAB-OFF LIT64,  7 DATA 7 ADD,  7 7 6 ADD,  5 7 0 STRB,
      8 8 8 LSRI,  5 5 1 ADDI,  fl2 B,
   fd2 LBL,  RET, ;

\ LVRALLOC ( -- x14=reg | 0 ) : grab a free register from the pool bitmask
: EMIT-VRALLOC ( -- )
   LVRALLOC LABEL@ LBL,
   LBL LBL LBL {: rl rgot rno :}
   6 DATA VRFREE-CELL LDR,  5 0 MOVZ,
   rl LBL,
      5 #POOL CMPI,  C-GE rno BCOND,
      7 6 5 LSRV,  7 7 1 ANDI,  7 rgot CBNZ,
      5 5 1 ADDI,  rl B,
   rno LBL,  14 0 MOVZ,  RET,
   rgot LBL,
      7 1 MOVZ,  7 7 5 LSLV,  6 6 7 EOR,  6 DATA VRFREE-CELL STR,
      14 VRTAB-OFF LIT64,  14 DATA 14 ADD,  14 14 5 ADD,  14 14 0 LDRB,  RET, ;

\ LFRALLOC ( -- x14=dreg | 0 ) : grab a free FLOAT register (d8..d31)
: EMIT-FRALLOC ( -- )
   LFRALLOC LABEL@ LBL,
   LBL LBL LBL {: rl rgot rno :}
   6 DATA FRFREE-CELL LDR,  5 0 MOVZ,
   rl LBL,
      5 #FPOOL CMPI,  C-GE rno BCOND,
      7 6 5 LSRV,  7 7 1 ANDI,  7 rgot CBNZ,
      5 5 1 ADDI,  rl B,
   rno LBL,  14 0 MOVZ,  RET,
   rgot LBL,
      7 1 MOVZ,  7 7 5 LSLV,  6 6 7 EOR,  6 DATA FRFREE-CELL STR,
      14 5 8 ADDI,  RET, ;

\ LVBIT ( x7=reg -- x8=its free-mask bit ) : reg -> 1<<idx via the inverse
\ table. Preserves x7; clobbers x10. Callers or (free) / eor (claim) the mask.
: EMIT-VBIT ( -- )
   LVBIT LABEL@ LBL,
   10 VRITAB-OFF LIT64,  10 DATA 10 ADD,  10 10 7 ADD,  10 10 0 LDRB,
   8 1 MOVZ,  8 8 10 LSLV,  RET, ;

;using
;using
;using
