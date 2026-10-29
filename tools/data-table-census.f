\ data-table-census.f - attributes every byte of the boot DATA heap [0, DP0) to
\ the `create`d or `variable` word that owns it: an AOT image carries every
\ non-zero byte in full, whether or not anything reads it.
\   printf 'require tools/data-table-census.f\nDATA-CENSUS:RUN\n' | bin/hb
\ RUN is not called on load, so tools/check.f can carry this file. Rows are
\ tab-separated: offset, extent, fill, nonzero, cells, cost, wid, owner.

\ Latched before this tool's own requires allot: the census covers [0, DP0).
package DATA-CENSUS
here data-base - constant DP0
;package

require src/lib/fmt.f
require src/lib/sort.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package DATA-CENSUS

\ DATA offset in the high half, dictionary index in the low half, so one sort
\ orders both.
DYNAMIC-BUFFER T-ROW n
variable T-N
variable NZ        variable FILL       variable RUNS
variable PAY       variable ROWB
variable SUM-NZ    variable SUM-FILL   variable SUM-RUNS   variable SUM-PAY
variable SUM-ROWB

32 constant OFF-SHIFT
$FFFFFFFF constant IDX-MASK

: HEAP@ ( n -- ptr u8 ) {: off:n :}
   data-base BYTE-VIEW off + ;

: TAB ( -- ) 9 emit ;

: ROW-OFF ( n -- n ) OFF-SHIFT rshift ;
: ROW-IDX ( n -- n ) IDX-MASK and ;

\ No record slot holds the address: it is decoded from EMIT-CREATE's MOVZ/MOVK
\ carrier (src/habu/habu2.f) with ADDRESS-CARRIER:CHAIN-VALUE. The DKIND:ADDR
\ stamp says the body is still the definer's own; `does>` clears it.
: REC-OFF ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p p rec XREF-CODE-BYTES + ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size 0= if s" data-table-census: malformed DKIND:ADDR body" 74 die then
   p size ADDRESS-CARRIER:CHAIN-VALUE DATA-VA VA>N - {: off:n :}
   off 0 < off DATA-SIZE >= or if
      s" data-table-census: DKIND:ADDR body decodes outside DATA" 74 die then
   off ;

: OWNER? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-RETIRED? if false exit then
   rec XREF-START 0= if false exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR = ;

\ Records stamped DKIND:ADDR (src/habu/definers.f) are the only owners of DATA.
\ Sorted by address they partition it: each base runs to the next, the last to
\ DP0.
: COLLECT ( -- )
   0 T-N !
   ndict@ T-ROW-RESERVE
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec OWNER? if
         rec REC-OFF {: off:n :}
         off DP0 < if
            off OFF-SHIFT lshift i or  T-N @ T-ROW !
            T-N @ 1+ T-N !
         then
      then
   loop
   0 T-ROW T-N @ [: < ;] SORT:SORT! ;

\ Bytes past the extent read as zero, as the capture leaves them.
variable CV

: CELL@ ( n n n -- n ) {: base:n c:n len:n :}
   0 CV !
   AOT-WINDOW:CELL-BYTES 0 ?do
      c AOT-WINDOW:CELL-BYTES * i + {: off:n :}
      off len < if
         base off + HEAP@ c@  i 8 * lshift  CV @ or  CV !
      then
   loop
   CV @ ;

\ Cells walk in ascending order, so a group is charged when the walk first
\ enters it.
variable GRP-LAST

: SCAN ( n n -- ) {: base:n len:n :}
   0 NZ !  0 FILL !  0 RUNS !  0 PAY !  0 ROWB !  -1 GRP-LAST !
   len 0 ?do
      base i + HEAP@ c@ 0<> if  NZ @ 1+ NZ !  i 1+ FILL !  then
   loop
   len AOT-WINDOW:CELL-BYTES 1- + AOT-WINDOW:CELL-BYTES / {: cells:n :}
   cells 0 ?do
      base i len CELL@ {: v:n :}
      v 0<> if
         RUNS @ 1+ RUNS !
         PAY @ v AOT-WINDOW:CELL-VLEN + PAY !
         i AOT-WINDOW:CELL-BYTES * AOT-WINDOW:GROUP-SPAN / {: g:n :}
         g GRP-LAST @ <> if
            ROWB @ AOT-WINDOW:GROUP-BYTES + ROWB !
            g GRP-LAST !
         then
      then
   loop ;

\ extent runs to the next owner; fill is one past the last non-zero byte. cells
\ and cost follow the capture's scanners (aot-capture.f ACAP-SCAN-SEG,
\ aot-link-arm.f BUILD-SPARSE-DATA): a non-zero cell costs the LEB128 of its
\ value, and each GROUP-SPAN holding one costs GROUP-BYTES of bitmap (aot-decl.f
\ BM-COMPACT). Sort on cost, not nonzero. Cells and groups count from the
\ extent's own base, so neighbours sharing one are both charged.
: ROW ( n n ptr u8 n n -- ) {: off:n len:n name:ptr nameu:n wid:n :}
   off len SCAN
   off FMT:.U TAB  len FMT:.U TAB  FILL @ FMT:.U TAB  NZ @ FMT:.U TAB
   RUNS @ FMT:.U TAB  PAY @ ROWB @ + FMT:.U TAB
   wid FMT:.INT TAB  name nameu type cr
   SUM-NZ @ NZ @ + SUM-NZ !  SUM-FILL @ FILL @ + SUM-FILL !
   SUM-RUNS @ RUNS @ + SUM-RUNS !  SUM-PAY @ PAY @ + SUM-PAY !
   SUM-ROWB @ ROWB @ + SUM-ROWB ! ;

: END-OF ( n -- n ) {: k:n :}
   k 1+ T-N @ >= if DP0 exit then
   k 1+ T-ROW @ ROW-OFF ;

: ROW-AT ( n -- ) {: k:n :}
   k T-ROW @ ROW-OFF {: off:n :}
   k T-ROW @ ROW-IDX XREF-REC {: rec:ptr :}
   off  k END-OF off -  rec XREF-NAME$  rec XREF-WORDLIST ROW ;

\ No record owns the span below the first base: the engine's fixed cells below
\ DATA-START, and what the seed and boot prefix allotted above it.
: HEAD-ROWS ( n -- ) {: first:n :}
   first DATA-START <= if  0 first s" (engine-cells)" -1 ROW  exit  then
   0 DATA-START s" (engine-cells)" -1 ROW
   DATA-START  first DATA-START -  s" (unowned-heap)" -1 ROW ;

: REPORT ( -- )
   0 SUM-NZ !  0 SUM-FILL !  0 SUM-RUNS !  0 SUM-PAY !  0 SUM-ROWB !
   s" offset" type TAB s" extent" type TAB s" fill" type TAB
   s" nonzero" type TAB s" cells" type TAB s" cost" type TAB
   s" wid" type TAB s" owner" type cr
   T-N @ 0 > if 0 T-ROW @ ROW-OFF HEAD-ROWS then
   T-N @ 0 ?do i ROW-AT loop
   s" census: owners " type T-N @ FMT:.U
   s"  dp " type DP0 FMT:.U
   s"  data-start " type DATA-START FMT:.U
   s"  nonzero " type SUM-NZ @ FMT:.U
   s"  cells " type SUM-RUNS @ FMT:.U
   s"  cost " type SUM-PAY @ SUM-ROWB @ + FMT:.U
   s"  fill " type SUM-FILL @ FMT:.U cr ;

public

: RUN ( -- )
   COLLECT REPORT T-ROW-RELEASE ;

;package
