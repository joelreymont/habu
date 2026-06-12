\ regalloc.fs — THE register allocator: the JIT's runtime register pool. One
\ place to look (and the one file the pool-widening work touches):
\   - the pool: x9..x15 today (VRALL bits, bit r-9); x16/x17 stay scratch
\   - VRFREE-CELL: the free bitmask in the DATA header
\   - Lvralloc: grab a free register ( -- x14=reg | 0 )
\ Allocator-state TOUCHPOINTS elsewhere (they or/eor the mask directly):
\   vsjit.fs: Lvspill (reset to VRALL), Lvdrop/Lvnipx (free), Lvbinprep (free
\   rm), Lvpushr (re-claim after spill), Lvrecon (rebuild from snapshot)
\   forth.fs: the `:` reset and j-repeat's exit-path reset (VRALL store)

require asm.fs

variable Lvralloc
$208 constant VRFREE-CELL       \ free-register bitmask, bit r-9 for x9..x15
$7F  constant VRALL             \ all seven pool registers free

\ Lvralloc ( -- x14=reg | 0 ) : grab a free register from the pool bitmask
: emit-vralloc
   Lvralloc @ LBL,
   NEWLBL NEWLBL NEWLBL {: rl rgot rno :}
   6 DATA VRFREE-CELL LDR,  5 0 MOVZ,
   rl LBL,
      5 7 CMPI,  C-GE rno BCOND,
      7 6 5 LSRV,  7 7 1 ANDI,  7 rgot CBNZ,
      5 5 1 ADDI,  rl B,
   rno LBL,  14 0 MOVZ,  RET,
   rgot LBL,
      7 1 MOVZ,  7 7 5 LSLV,  6 6 7 EOR,  6 DATA VRFREE-CELL STR,
      14 5 9 ADDI,  RET, ;
