\ Native engine helpers for retained-code provenance. The table is sorted and
\ normalized; ordinary forward publication appends or extends its last row.
\ Only an interior overwrite moves a suffix. Queries use binary search.
package TIER-PROV

variable LSET  variable LSET-END
variable LQUERY  variable LQUERY-END
variable CAPTURE-ORIGIN  -1 CAPTURE-ORIGIN !

public
: LABELS ( -- )
   LBL LSET !  LBL LSET-END !  LBL LQUERY !  LBL LQUERY-END ! ;

private

\ dst must differ from ix. A row has three cells, not a power-of-two stride.
: ROW, ( n n n -- ) {: dst:n ix:n base:n :}
   dst ix 1 LSLI,  dst dst ix ADD,  dst dst 3 LSLI,
   dst base dst ADD, ;

: SAVE, ( -- )
   SP SP 208 SUBI,
   18 2 do i SP i 2 - cells STR, loop
   30 SP 128 STR, ;

: RESTORE, ( -- )
   18 2 do i SP i 2 - cells LDR, loop
   30 SP 128 LDR,  SP SP 208 ADDI,  RET, ;

\ x2 table, x3 count cell, x4 count, x7 first, x8 end, x9 origin.
\ Find [x5,x6), the rows that overlap the new interval.
: SEARCH, ( -- )
   LBL LBL LBL LBL LBL LBL {: left:label lhi:label lend:label right:label rhi:label rend:label :}
   10 0 MOVZ,  11 4 0 ADDI,
   left LBL,
      10 11 CMP,  C-GE lend BCOND,
      12 10 11 ADD,  12 12 1 LSRI,  13 12 2 ROW,
      16 13 8 LDR,  16 7 CMP,  C-GT lhi BCOND,
      10 12 1 ADDI,  left B,
      lhi LBL,  11 12 0 ADDI,  left B,
   lend LBL,  5 10 0 ADDI,
   11 4 0 ADDI,
   right LBL,
      10 11 CMP,  C-GE rend BCOND,
      12 10 11 ADD,  12 12 1 LSRI,  13 12 2 ROW,
      16 13 0 LDR,  16 8 CMP,  C-GE rhi BCOND,
      10 12 1 ADDI,  right B,
      rhi LBL,  11 12 0 ADDI,  right B,
   rend LBL,  6 10 0 ADDI, ;

\ Preserve at most one head and tail, then merge equal-origin neighbours.
\ Head/tail flags are x14/x15; their saved endpoints/kinds are on SP.
: EDGES, ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL
   {: nohead:label headkeep:label notail:label tailkeep:label noprev:label prevdone:label nonext:label nextdone:label :}
   14 0 MOVZ,  15 0 MOVZ,
   5 6 CMP,  C-GE nohead BCOND,
   13 5 2 ROW,
   10 13 0 LDR,  10 7 CMP,  C-GE nohead BCOND,
   11 13 16 LDR,  11 9 CMP,  C-NE headkeep BCOND,
      7 10 0 ADDI,  nohead B,
   headkeep LBL,
      14 1 MOVZ,  10 SP 144 STR,  11 SP 152 STR,
   nohead LBL,
   5 6 CMP,  C-GE notail BCOND,
   12 6 1 SUBI,  13 12 2 ROW,
   10 13 8 LDR,  10 8 CMP,  C-LE notail BCOND,
   11 13 16 LDR,  11 9 CMP,  C-NE tailkeep BCOND,
      8 10 0 ADDI,  notail B,
   tailkeep LBL,
      15 1 MOVZ,  10 SP 160 STR,  11 SP 168 STR,
   notail LBL,
   14 noprev CBNZ,  5 noprev CBZ,
   12 5 1 SUBI,  13 12 2 ROW,
   10 13 8 LDR,  10 7 CMP,  C-NE prevdone BCOND,
   11 13 16 LDR,  11 9 CMP,  C-NE prevdone BCOND,
      7 13 0 LDR,  5 5 1 SUBI,
   prevdone LBL,  noprev LBL,
   15 nonext CBNZ,  6 4 CMP,  C-GE nonext BCOND,
   13 6 2 ROW,
   10 13 0 LDR,  10 8 CMP,  C-NE nextdone BCOND,
   11 13 16 LDR,  11 9 CMP,  C-NE nextdone BCOND,
      8 13 8 LDR,  6 6 1 ADDI,
   nextdone LBL,  nonext LBL, ;

\ Copy one row from index x12 to index x12+x17. x14/x15 were saved.
: MOVE-ROW, ( -- )
   13 12 2 ROW,
   16 12 17 ADD,  14 16 2 ROW,
   10 13 0 LDR,  11 13 8 LDR,  16 13 16 LDR,
   10 14 0 STR,  11 14 8 STR,  16 14 16 STR, ;

: MOVE, ( label -- ) {: full:label :}
   LBL LBL LBL LBL {: left:label right:label more:label done:label :}
   14 SP 184 STR,  15 SP 192 STR,
   17 14 15 ADD,  17 17 1 ADDI,
   16 6 5 SUB,  17 17 16 SUB,                 \ replacement count - removed count
   16 4 17 ADD,
   10 SPANS LIT64,  16 10 CMP,  C-GT full BCOND,
   16 SP 176 STR,
   17 done CBZ,
   17 0 CMPI,  C-LT left BCOND,
   12 4 0 ADDI,
   right LBL,
      12 6 CMP,  C-LE done BCOND,
      12 12 1 SUBI,  MOVE-ROW,  right B,
   left LBL,
   12 6 0 ADDI,
   more LBL,
      12 4 CMP,  C-GE done BCOND,
      MOVE-ROW,  12 12 1 ADDI,  more B,
   done LBL,
   14 SP 184 LDR,  15 SP 192 LDR, ;

: PUT, ( -- )
   LBL LBL {: nohead:label notail:label :}
   12 5 0 ADDI,
   14 nohead CBZ,
      13 12 2 ROW,  10 SP 144 LDR,  11 SP 152 LDR,
      10 13 0 STR,  7 13 8 STR,  11 13 16 STR,
      12 12 1 ADDI,
   nohead LBL,
   13 12 2 ROW,
   7 13 0 STR,  8 13 8 STR,  9 13 16 STR,
   12 12 1 ADDI,
   15 notail CBZ,
      13 12 2 ROW,  10 SP 160 LDR,  11 SP 168 LDR,
      8 13 0 STR,  10 13 8 STR,  11 13 16 STR,
   notail LBL,
   16 SP 176 LDR,  16 3 0 STR, ;

: EMIT-SET ( -- )
   LBL LBL LBL LBL LBL {: append:label interior:label done:label full:label msg:label :}
   LSET LABEL@ LBL,
   SAVE,
   9 10 CMP,  C-CS done BCOND,
   7 9 DBASE SUB,  8 10 DBASE SUB,  9 11 0 ADDI,
   7 8 CMP,  C-GE done BCOND,
   3 N-CELL LIT64,  3 DATA 3 ADD,  4 3 0 LDR,
   10 SPANS LIT64,  4 10 CMP,  C-HI full BCOND,
   2 TABLE-OFF LIT64,  2 DATA 2 ADD,
   4 append CBZ,
   12 4 1 SUBI,  13 12 2 ROW,
   10 13 8 LDR,  10 7 CMP,  C-GT interior BCOND,
   C-LT append BCOND,
   11 13 16 LDR,  11 9 CMP,  C-NE append BCOND,
      8 13 8 STR,  done B,
   append LBL,
      10 SPANS LIT64,  4 10 CMP,  C-GE full BCOND,
      13 4 2 ROW,
      7 13 0 STR,  8 13 8 STR,  9 13 16 STR,
      4 4 1 ADDI,  4 3 0 STR,  done B,
   interior LBL,
      SEARCH,  EDGES,  full MOVE,  PUT,  done B,
   full LBL,
      0 2 MOVZ,  1 msg ADR,  2 31 MOVZ,  NR-WRITE SYS,
      0 101 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  S\" hb: code-origin table capacity\n" BYTES,
   done LBL,  RESTORE,
   LSET-END LABEL@ LBL,
   s" engine-code-origin-set" LSET @ LSET-END @ ENGINE-HELPER:REGISTER ;

: EMIT-QUERY ( -- )
   LBL LBL LBL LBL LBL {: search:label hi:label found:label unknown:label done:label :}
   LQUERY LABEL@ LBL,
   \ Returns x9; preserves the other kernel scratch registers.
   SAVE,
   9 10 CMP,  C-CS unknown BCOND,
   7 9 DBASE SUB,  8 10 DBASE SUB,
   7 8 CMP,  C-GE unknown BCOND,
   2 TABLE-OFF LIT64,  2 DATA 2 ADD,
   3 N-CELL LIT64,  3 DATA 3 ADD,
   10 0 MOVZ,  11 3 0 LDR,  4 11 0 ADDI,
   12 SPANS LIT64,  11 12 CMP,  C-HI unknown BCOND,
   search LBL,
      10 11 CMP,  C-GE found BCOND,
      12 10 11 ADD,  12 12 1 LSRI,  13 12 2 ROW,
      16 13 8 LDR,  16 7 CMP,  C-GT hi BCOND,
      10 12 1 ADDI,  search B,
      hi LBL,  11 12 0 ADDI,  search B,
   found LBL,
   10 4 CMP,  C-GE unknown BCOND,
   13 10 2 ROW,
   16 13 0 LDR,  16 7 CMP,  C-GT unknown BCOND,
   16 13 8 LDR,  16 8 CMP,  C-LT unknown BCOND,
   9 13 16 LDR,  done B,
   unknown LBL,  9 0 MOVN,
   done LBL,
   9 SP 56 STR,                            \ replace saved x9 with the answer
   RESTORE,
   LQUERY-END LABEL@ LBL,
   s" engine-code-origin-query" LQUERY @ LQUERY-END @ ENGINE-HELPER:REGISTER ;

public

: EMIT-HELPERS ( -- ) EMIT-SET EMIT-QUERY ;

\ Runtime addresses enter here; stored coordinates remain relocation-relative.
\ Keep all caller registers intact, including overlapping argument registers.
: RANGE, ( n n n -- ) {: first:n end:n origin:n :}
   SP SP 64 SUBI,
   9 SP 0 STR,  10 SP 8 STR,  11 SP 16 STR,  30 SP 24 STR,
   first SP 32 STR,  end SP 40 STR,
   9 SP 32 LDR,  10 SP 40 LDR,  11 origin LIT64,
   LSET LABEL@ BL,
   9 SP 0 LDR,  10 SP 8 LDR,  11 SP 16 LDR,  30 SP 24 LDR,
   SP SP 64 ADDI, ;

: NATIVE-RANGE, ( n n -- ) 1 RANGE, ;
: UNKNOWN-RANGE, ( n n -- ) -1 RANGE, ;

\ This value belongs to one emitter invocation. Only the owned capture's
\ admission can supply 1; the ordinary artifact/source entry supplies unknown.
: CAPTURE-ORIGIN! ( n -- )
   dup -1 <> over 1 <> and if
      s" engine: captured code lacks native provenance" ENGINE-ERROR:IMAGE-CODE-ORIGIN die
   then
   CAPTURE-ORIGIN ! ;

: CAPTURE-RANGE, ( n n -- ) CAPTURE-ORIGIN @ RANGE, ;

: QUERY, ( -- ) LQUERY LABEL@ BL, ;

: OPEN, ( -- )
   SP SP 16 SUBI,  9 SP 0 STR,
   9 OPEN-CELL LIT64,  9 DATA 9 ADD,  CP 9 0 STR,
   9 SP 0 LDR,  SP SP 16 ADDI, ;

\ Close before any cursor rollback. Failure is explicitly unknown; only the
\ native compiler's successful return can publish positive native evidence.
: CLOSE, ( n -- ) {: origin:n :}
   LBL {: none:label :}
   SP SP 32 SUBI,  9 SP 0 STR,  10 SP 8 STR,  11 SP 16 STR,
   11 OPEN-CELL LIT64,  11 DATA 11 ADD,
   9 11 0 LDR,  9 none CBZ,
   10 0 MOVZ,  10 11 0 STR,
   9 CP origin RANGE,
   none LBL,
   9 SP 0 LDR,  10 SP 8 LDR,  11 SP 16 LDR,  SP SP 32 ADDI, ;

\ Only the region is relocated by a snapshot. Discard its old engine-text
\ coordinates once at boot, retaining the sorted region rows without changes.
: ABANDON, ( -- )
   LBL LBL {: native:label done:label :}
   SP SP 16 SUBI,  9 SP 0 STR,
   9 DATA NCOMP-DISPATCH:DEF-TIER-CELL LDR,  9 native CBNZ,
   0 CLOSE,  done B,
   native LBL,  -1 CLOSE,
   done LBL,
   9 SP 0 LDR,  SP SP 16 ADDI, ;

: RESTORE-REGION, ( -- )
   LBL LBL LBL {: more:label next:label done:label :}
   2 TABLE-OFF LIT64,  2 DATA 2 ADD,
   3 N-CELL LIT64,  3 DATA 3 ADD,  4 3 0 LDR,
   5 SPANS LIT64,  4 5 CMP,  C-LS more BCOND,
   LBL {: msg:label :}
   0 2 MOVZ,  1 msg ADR,  2 31 MOVZ,  NR-WRITE SYS,
   0 101 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL, S\" hb: code-origin table capacity\n" BYTES,
   more LBL,
   5 0 MOVZ,  6 0 MOVZ,
   LBL {: loop:label :}
   loop LBL,
      5 4 CMP,  C-GE done BCOND,
      13 5 2 ROW,
      10 13 0 LDR,  11 13 8 LDR,  12 13 16 LDR,
      14 DICT-SIZE LIT64,  10 14 CMP,  C-LT next BCOND,
      14 REGION LIT64,  11 14 CMP,  C-GT next BCOND,
      13 6 2 ROW,
      10 13 0 STR,  11 13 8 STR,  12 13 16 STR,
      6 6 1 ADDI,
   next LBL,  5 5 1 ADDI,  loop B,
   done LBL,  6 3 0 STR, ;

;package
