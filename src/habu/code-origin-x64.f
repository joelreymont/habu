\ code-origin-x64.f - the x86-64 twin of src/habu/code-origin.f, package
\ X64PROV: the retained-code provenance band over the same DATA layout
\ (src/habu/layout.f TIER-PROV). A row is (first, end, origin), its coordinates
\ relative to DBASE; unknown is -1, JIT 0 and positively native 1, and missing
\ coverage is unknown. The rows stay sorted, disjoint and normalized: ordinary
\ forward publication appends or extends the last row, only an interior
\ overwrite moves a suffix, and a query searches by halves.
\
\ THE CALL CONTRACT. code-origin.f's call sites keep every ARM64 register;
\ these keep every VM register and clobber rax rcx rdx rsi rdi and r8-r11, as
\ the kernel's other helpers do (src/habu/kernel-x64.f), and OPEN, keeps every
\ register. A span is two absolute addresses: the first byte in rdi and the
\ end, exclusive, in rsi.
require lib/byte-buffer.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/primitive-registry.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f

package X64PROV
using X64ASM
using X64CODE

2 constant STDERR

\ A row's cells.
0 constant R-FIRST
8 constant R-END
16 constant R-ORIGIN

\ The replacement's frame on the machine stack: the kept head and tail with a
\ flag each, the count MOVE, leaves for PUT, and the origin while rdx carries
\ moved cells.
0 constant HEAD-FIRST
8 constant HEAD-ORIGIN
16 constant TAIL-END
24 constant TAIL-ORIGIN
32 constant HEAD-KEPT
40 constant TAIL-KEPT
48 constant NEW-COUNT
56 constant KEEP-ORIGIN
64 constant SET-FRAME

\ The helpers' labels, made by EMIT-HELPERS in the stream it emits them into.
variable SET-CELL
variable QUERY-CELL
: SET-LBL ( -- label ) SET-CELL @ >LABEL ;
: QUERY-LBL ( -- label ) QUERY-CELL @ >LABEL ;

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: CP-REG ( -- r64 ) ENGINE-GPR:X64-CP >R64 ;

: LOAD, ( r64 mem -- ) ASM-SINK ENC-MOV-RM ;
: STORE, ( r64 mem -- ) ASM-SINK ENC-MOV-MR ;
: COPY, ( r64 r64 -- ) ASM-SINK ENC-MOV-RR ;
: CMP-REG, ( r64 r64 -- ) ASM-SINK ENC-CMP-RR ;
: CMP-MEM, ( r64 mem -- ) ASM-SINK ENC-CMP-RM ;
: TEST, ( r64 -- ) dup ASM-SINK ENC-TEST-RR ;

: FRAME-CELL ( n -- mem ) RSP swap MEM-OFF ;
: COUNT-MEM ( -- mem ) DATA-REG TIER-PROV:N-CELL MEM-OFF ;
: OPEN-MEM ( -- mem ) DATA-REG TIER-PROV:OPEN-CELL MEM-OFF ;

\ A cell of the row whose byte offset in the table the register holds.
: TABLE-CELL ( r64 n -- mem ) {: off:r64 field:n :}
   DATA-REG off 1 TIER-PROV:TABLE-OFF field + MEM-IDX ;

\ dst = the byte offset of the row whose index the register holds.
: ROW-OFF, ( r64 r64 -- ) {: dst:r64 ix:r64 :}
   dst ix TIER-PROV:SPAN-BYTES >IMM8 ASM-SINK ENC-IMUL-RRI8 ;

: NEXT-ROW, ( r64 -- ) TIER-PROV:SPAN-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ rax = the index halfway between two registers.
: MID, ( r64 r64 -- ) {: lo:r64 hi:r64 :}
   RAX lo hi 1 0 MEM-IDX ASM-SINK ENC-LEA
   RAX 1 >IMM8 ASM-SINK ENC-SHR-RI8 ;

\ Set the frame's flag at an offset.
: KEPT!, ( n -- ) {: at:n :}
   RAX 1 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX at FRAME-CELL STORE, ;

: FULL$ ( -- ptr u8 n ) S\" hb: code-origin table capacity\n" ;

\ ---- the search -------------------------------------------------------------
\ r9 = the first of the r8 rows whose end passes rdi. Clobbers rax rcx r11.
: FIRST-PAST, ( -- )
   LBL LBL LBL {: more:label high:label done:label :}
   R9 ZERO-REG,  R11 R8 COPY,
   more LBL,
      R9 R11 CMP-REG,  C-GE done JCC,
      R9 R11 MID,  RCX RAX ROW-OFF,
      RDI RCX R-END TABLE-CELL CMP-MEM,  C-L high JCC,
      R9 RAX 1 MEM-OFF ASM-SINK ENC-LEA  more JMP,
   high LBL,  R11 RAX COPY,  more JMP,
   done LBL, ;

\ r10 = the first row from r9 on that starts at or past rsi. Clobbers rax rcx
\ r11.
: FIRST-AT, ( -- )
   LBL LBL LBL {: more:label high:label done:label :}
   R10 R9 COPY,  R11 R8 COPY,
   more LBL,
      R10 R11 CMP-REG,  C-GE done JCC,
      R10 R11 MID,  RCX RAX ROW-OFF,
      RSI RCX R-FIRST TABLE-CELL CMP-MEM,  C-LE high JCC,
      R10 RAX 1 MEM-OFF ASM-SINK ENC-LEA  more JMP,
   high LBL,  R11 RAX COPY,  more JMP,
   done LBL, ;

\ ---- the replacement ---------------------------------------------------------
\ The span [rdi, rsi) with origin rdx replaces the rows [r9, r10) it overlaps.
\ Keep at most one head and one tail of them, then merge equal-origin
\ neighbours: the twin of code-origin.f EDGES,. A head [first, rdi) or tail
\ [rsi, end) of another origin waits in the frame; one of rdx's origin widens
\ the span instead, and so does an adjacent neighbour of rdx's origin.
: EDGES, ( -- )
   LBL LBL LBL LBL {: nohead:label headkeep:label notail:label tailkeep:label :}
   LBL LBL {: noprev:label nonext:label :}
   RAX ZERO-REG,
   RAX HEAD-KEPT FRAME-CELL STORE,  RAX TAIL-KEPT FRAME-CELL STORE,
   R9 R10 CMP-REG,  C-GE nohead JCC,
   RCX R9 ROW-OFF,
   RAX RCX R-FIRST TABLE-CELL LOAD,
   RAX RDI CMP-REG,  C-GE nohead JCC,
   R11 RCX R-ORIGIN TABLE-CELL LOAD,
   R11 RDX CMP-REG,  C-NE headkeep JCC,
      RDI RAX COPY,  nohead JMP,
   headkeep LBL,
      RAX HEAD-FIRST FRAME-CELL STORE,  R11 HEAD-ORIGIN FRAME-CELL STORE,
      HEAD-KEPT KEPT!,
   nohead LBL,
   R9 R10 CMP-REG,  C-GE notail JCC,
   RCX R10 -1 MEM-OFF ASM-SINK ENC-LEA  RCX RCX ROW-OFF,
   RAX RCX R-END TABLE-CELL LOAD,
   RAX RSI CMP-REG,  C-LE notail JCC,
   R11 RCX R-ORIGIN TABLE-CELL LOAD,
   R11 RDX CMP-REG,  C-NE tailkeep JCC,
      RSI RAX COPY,  notail JMP,
   tailkeep LBL,
      RAX TAIL-END FRAME-CELL STORE,  R11 TAIL-ORIGIN FRAME-CELL STORE,
      TAIL-KEPT KEPT!,
   notail LBL,
   RAX HEAD-KEPT FRAME-CELL LOAD,  RAX TEST,  C-NE noprev JCC,
   R9 TEST,  C-E noprev JCC,
   RCX R9 -1 MEM-OFF ASM-SINK ENC-LEA  RCX RCX ROW-OFF,
   RDI RCX R-END TABLE-CELL CMP-MEM,  C-NE noprev JCC,
   RDX RCX R-ORIGIN TABLE-CELL CMP-MEM,  C-NE noprev JCC,
      RDI RCX R-FIRST TABLE-CELL LOAD,  R9 ASM-SINK ENC-DEC
   noprev LBL,
   RAX TAIL-KEPT FRAME-CELL LOAD,  RAX TEST,  C-NE nonext JCC,
   R10 R8 CMP-REG,  C-GE nonext JCC,
   RCX R10 ROW-OFF,
   RSI RCX R-FIRST TABLE-CELL CMP-MEM,  C-NE nonext JCC,
   RDX RCX R-ORIGIN TABLE-CELL CMP-MEM,  C-NE nonext JCC,
      RSI RCX R-END TABLE-CELL LOAD,  R10 ASM-SINK ENC-INC
   nonext LBL, ;

\ Copy the row whose index the register holds rax bytes along the table,
\ through rdx. Clobbers rcx r11.
: MOVE-ROW, ( r64 -- ) {: ix:r64 :}
   RCX ix ROW-OFF,
   R11 RCX RAX 1 0 MEM-IDX ASM-SINK ENC-LEA
   RDX RCX R-FIRST TABLE-CELL LOAD,  RDX R11 R-FIRST TABLE-CELL STORE,
   RDX RCX R-END TABLE-CELL LOAD,  RDX R11 R-END TABLE-CELL STORE,
   RDX RCX R-ORIGIN TABLE-CELL LOAD,  RDX R11 R-ORIGIN TABLE-CELL STORE, ;

\ Make room for what PUT, writes: the kept head and tail and the span replace
\ the rows [r9, r10), so the rows from r10 on move by the difference, and a
\ table that would pass SPANS rows branches to the label. The twin of
\ code-origin.f MOVE,. The new count waits in the frame, and so does the
\ origin while rdx carries the moved cells.
: MOVE, ( label -- ) {: full:label :}
   LBL LBL LBL {: left:label right:label done:label :}
   \ rax = the rows the table gains or loses: the pieces less the rows replaced
   RAX HEAD-KEPT FRAME-CELL LOAD,
   RAX TAIL-KEPT FRAME-CELL ASM-SINK ENC-ADD-RM
   RAX ASM-SINK ENC-INC
   RAX R10 ASM-SINK ENC-SUB-RR  RAX R9 ASM-SINK ENC-ADD-RR
   R11 R8 RAX 1 0 MEM-IDX ASM-SINK ENC-LEA
   R11 TIER-PROV:SPANS >IMM32 ASM-SINK ENC-CMP-RI32  C-G full JCC,
   R11 NEW-COUNT FRAME-CELL STORE,
   RDX KEEP-ORIGIN FRAME-CELL STORE,
   RAX RAX TIER-PROV:SPAN-BYTES >IMM8 ASM-SINK ENC-IMUL-RRI8
   RAX TEST,  C-E done JCC,  C-L left JCC,
   right LBL,                                        \ down from the last row
      R8 R10 CMP-REG,  C-LE done JCC,
      R8 ASM-SINK ENC-DEC  R8 MOVE-ROW,  right JMP,
   left LBL,                                         \ up from row r10
      R10 R8 CMP-REG,  C-GE done JCC,
      R10 MOVE-ROW,  R10 ASM-SINK ENC-INC  left JMP,
   done LBL,
   RDX KEEP-ORIGIN FRAME-CELL LOAD, ;

\ Write the kept head, the span and the kept tail from row r9 on, and publish
\ the count MOVE, left in the frame: the twin of code-origin.f PUT,.
: PUT, ( -- )
   LBL LBL {: nohead:label notail:label :}
   RCX R9 ROW-OFF,
   RAX HEAD-KEPT FRAME-CELL LOAD,  RAX TEST,  C-E nohead JCC,
      RAX HEAD-FIRST FRAME-CELL LOAD,  RAX RCX R-FIRST TABLE-CELL STORE,
      RDI RCX R-END TABLE-CELL STORE,
      RAX HEAD-ORIGIN FRAME-CELL LOAD,  RAX RCX R-ORIGIN TABLE-CELL STORE,
      RCX NEXT-ROW,
   nohead LBL,
   RDI RCX R-FIRST TABLE-CELL STORE,
   RSI RCX R-END TABLE-CELL STORE,
   RDX RCX R-ORIGIN TABLE-CELL STORE,
   RAX TAIL-KEPT FRAME-CELL LOAD,  RAX TEST,  C-E notail JCC,
      RCX NEXT-ROW,
      RSI RCX R-FIRST TABLE-CELL STORE,
      RAX TAIL-END FRAME-CELL LOAD,  RAX RCX R-END TABLE-CELL STORE,
      RAX TAIL-ORIGIN FRAME-CELL LOAD,  RAX RCX R-ORIGIN TABLE-CELL STORE,
   notail LBL,
   RAX NEW-COUNT FRAME-CELL LOAD,  RAX COUNT-MEM STORE, ;

\ ---- the helpers -------------------------------------------------------------
\ (engine-code-origin-set) ( rdi = first, rsi = end, rdx = origin, rcx = 0,
\ or nonzero to write only where rows lie ): the twin of code-origin.f
\ EMIT-SET. An empty or wrapping span changes nothing. rcx nonzero asks for
\ invalidation only where earlier evidence exists: generic patch32 also writes
\ DATA and dictionary words, which need no code row. A table that cannot take
\ the rows writes `hb: code-origin table capacity` on fd 2 and exits
\ ENGINE-ERROR:CODE-ORIGIN-FULL.
: EMIT-SET ( -- )
   SET-LBL {: start:label :}
   LBL LBL LBL LBL {: append:label interior:label normal:label replace:label :}
   LBL LBL LBL LBL {: full:label msg:label done:label end:label :}
   s" engine-code-origin-set" start LABEL>N end LABEL>N
   ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   RDI RSI CMP-REG,  C-AE done JCC,
   RDI DBASE-REG ASM-SINK ENC-SUB-RR  RSI DBASE-REG ASM-SINK ENC-SUB-RR
   RDI RSI CMP-REG,  C-GE done JCC,
   R8 COUNT-MEM LOAD,
   R8 TIER-PROV:SPANS >IMM32 ASM-SINK ENC-CMP-RI32  C-A full JCC,
   RCX TEST,  C-E normal JCC,
      FIRST-PAST,  FIRST-AT,
      R9 R10 CMP-REG,  C-GE done JCC,  replace JMP,
   normal LBL,
   R8 TEST,  C-E append JCC,
   RCX R8 -1 MEM-OFF ASM-SINK ENC-LEA  RCX RCX ROW-OFF,
   RAX RCX R-END TABLE-CELL LOAD,
   RAX RDI CMP-REG,  C-G interior JCC,  C-L append JCC,
   RDX RCX R-ORIGIN TABLE-CELL CMP-MEM,  C-NE append JCC,
      RSI RCX R-END TABLE-CELL STORE,  done JMP,     \ extend the last row
   append LBL,
      R8 TIER-PROV:SPANS >IMM32 ASM-SINK ENC-CMP-RI32  C-GE full JCC,
      RCX R8 ROW-OFF,
      RDI RCX R-FIRST TABLE-CELL STORE,
      RSI RCX R-END TABLE-CELL STORE,
      RDX RCX R-ORIGIN TABLE-CELL STORE,
      R8 ASM-SINK ENC-INC  R8 COUNT-MEM STORE,  done JMP,
   interior LBL,
      FIRST-PAST,  FIRST-AT,
   replace LBL,
      RSP SET-FRAME >IMM8 ASM-SINK ENC-SUB-RI8
      EDGES,  full MOVE,  PUT,
      RSP SET-FRAME >IMM8 ASM-SINK ENC-ADD-RI8
      done JMP,
   full LBL,
      RDI STDERR >IMM32 ASM-SINK ENC-MOV-RI32
      RSI msg MOVABS,
      RDX FULL$ nip >IMM32 ASM-SINK ENC-MOV-RI32
      NR-WRITE SYS,
      RDI ENGINE-ERROR:CODE-ORIGIN-FULL >IMM32 ASM-SINK ENC-MOV-RI32
      NR-EXIT-GROUP SYS,
   msg LBL,  FULL$ BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   done LBL,
   ASM-SINK ENC-RET
   end LBL, ;

\ (engine-code-origin-query) ( rdi = first, rsi = end -- rax = origin ): the
\ twin of code-origin.f EMIT-QUERY. The answer is the origin of the one row
\ that covers the whole span, and -1, unknown, for an empty or wrapping span,
\ a span no one row covers and a count past SPANS.
: EMIT-QUERY ( -- )
   QUERY-LBL {: start:label :}
   LBL LBL {: unknown:label end:label :}
   s" engine-code-origin-query" start LABEL>N end LABEL>N
   ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   RDI RSI CMP-REG,  C-AE unknown JCC,
   RDI DBASE-REG ASM-SINK ENC-SUB-RR  RSI DBASE-REG ASM-SINK ENC-SUB-RR
   RDI RSI CMP-REG,  C-GE unknown JCC,
   R8 COUNT-MEM LOAD,
   R8 TIER-PROV:SPANS >IMM32 ASM-SINK ENC-CMP-RI32  C-A unknown JCC,
   FIRST-PAST,
   R9 R8 CMP-REG,  C-GE unknown JCC,
   RCX R9 ROW-OFF,
   RDI RCX R-FIRST TABLE-CELL CMP-MEM,  C-L unknown JCC,   \ starts past rdi
   RSI RCX R-END TABLE-CELL CMP-MEM,  C-G unknown JCC,     \ ends short of rsi
   RAX RCX R-ORIGIN TABLE-CELL LOAD,
   ASM-SINK ENC-RET
   unknown LBL,
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   ASM-SINK ENC-RET
   end LBL, ;

public

\ Make both helpers' labels and emit them; every call site follows in the same
\ stream. src/habu/kernel-x64.f HELPERS, calls it.
: EMIT-HELPERS ( -- )
   LBL SET-CELL !  LBL QUERY-CELL !
   EMIT-SET  EMIT-QUERY ;

private

\ Record [rdi, rsi) with the origin; the flag asks for a write only where rows
\ already lie. The twin of code-origin.f RANGE-MODE,.
: RANGE-MODE, ( n bool -- ) {: origin:n existing?:bool :}
   RDX origin >IMM32 ASM-SINK ENC-MOV-RI32
   existing? if RCX 1 >IMM32 ASM-SINK ENC-MOV-RI32 else RCX ZERO-REG, then
   SET-LBL CALL, ;

public

\ The span [rdi, rsi) is positively native, or of unknown origin.
: NATIVE-RANGE, ( -- ) 1 false RANGE-MODE, ;
: UNKNOWN-RANGE, ( -- ) -1 false RANGE-MODE, ;

\ A generic instruction write carries no optimizer proof: [rdi, rsi) turns
\ unknown where rows lie, and a write where none lies records nothing.
: INVALIDATE, ( -- ) -1 true RANGE-MODE, ;

\ rax = the origin of [rdi, rsi).
: QUERY, ( -- ) QUERY-LBL CALL, ;

\ Open the window at CP. It keeps every register.
: OPEN, ( -- ) CP-REG OPEN-MEM STORE, ;

\ Close the window: [the CP OPEN, saw, CP) takes the origin n and the window
\ clears; with none open nothing changes. Close before any cursor rollback.
\ Failure is explicitly unknown; only the native compiler's successful return
\ may supply 1.
: CLOSE, ( n -- ) {: origin:n :}
   LBL {: none:label :}
   RDI OPEN-MEM LOAD,  RDI TEST,  C-E none JCC,
   RAX ZERO-REG,  RAX OPEN-MEM STORE,
   RSI CP-REG COPY,
   origin false RANGE-MODE,
   none LBL, ;

\ Mark the text between two labels native, as habu2.f
\ EM-STARTUP-RUNTIME-STATE marks the engine text at boot.
: TEXT-NATIVE, ( label label -- ) {: first:label end:label :}
   RDI first MOVABS,  RSI end MOVABS,  NATIVE-RANGE, ;

;using
;using
;package
