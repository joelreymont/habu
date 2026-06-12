\ regalloc.f — THE register allocator, transcribed from bootstrap/cg/regalloc.fs
\ (lockstep; parity-linted). One place to look:
\   - VRPACK: the pool TABLE — one byte per slot, idx 0 at the low byte. To
\     widen the pool or port an arch, edit this literal (and only this file).
\   - #POOL / VRALL are DERIVED from VRPACK (byte count / low bitmask).
\   - VRFREE-CELL: the free bitmask in the DATA header, bit = pool INDEX.
\   - VRTAB/VRITAB: DATA byte tables idx->reg and reg->idx, filled at startup
\     by LVRINIT from VRPACK; LVRALLOC and LVBIT read them at JIT time.
\ Allocator-state TOUCHPOINTS elsewhere (they or/eor the mask directly):
\   jit.f: LVSPILL (reset to VRALL), LVDROP/LVNIPX/LVBINPREP/LVPUSHR/LVRECON
\   (free/claim via LVBIT)   habu2.f: the WHILE and J-REPEAT VRALL stores
\ Load after mnem.f/sys.f, before jit.f.

variable LVRALLOC   variable LVBIT   variable LVRINIT

$1D0F0E0D0C0B0A09 constant VRPACK   \ x9..x15, x29 (byte per slot, idx 0 low)
$1615181719 constant VRPACK2        \ overflow: x25,x23,x24,x21,x22 (tokenizer state in DATA cells)
: PK# ( x -- n ) 0 BEGIN over WHILE 1 + swap 8 rshift swap REPEAT nip ;
VRPACK PK# constant #POOL1
#POOL1 VRPACK2 PK# + constant #POOL
1 #POOL lshift 1 - constant VRALL

$208  constant VRFREE-CELL      \ free-register bitmask, bit = pool index
$3600 constant VRTAB-OFF        \ 32 B: idx -> register number   (after LOCNAMES)
$3620 constant VRITAB-OFF       \ 32 B: register number -> idx ($FF = not pooled)

\ LVRINIT ( -- ) : fill VRTAB/VRITAB from the VRPACK literal. Run once at startup
\ (idempotent; the warm snapshot carries the tables but re-running is harmless).
: EMIT-VRINIT
   LVRINIT @ LBL,
   LBL {: pl :}  LBL {: pd :}  LBL {: fl :}  LBL {: fd :}
   5 0 MOVZ,
   pl LBL,                                   \ poison the inverse table
      5 32 CMPI,  C-GE pd BCOND,
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
: EMIT-VRALLOC
   LVRALLOC @ LBL,
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

\ LVBIT ( x7=reg -- x8=its free-mask bit ) : reg -> 1<<idx via the inverse
\ table. Preserves x7; clobbers x10. Callers or (free) / eor (claim) the mask.
: EMIT-VBIT
   LVBIT @ LBL,
   10 VRITAB-OFF LIT64,  10 DATA 10 ADD,  10 10 7 ADD,  10 10 0 LDRB,
   8 1 MOVZ,  8 8 10 LSLV,  RET, ;
