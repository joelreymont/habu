\ data-table-census.f - what every byte of the captured DATA heap belongs to.
\
\ WHY IT EXISTS. An AOT engine carries its boot DATA heap inside its own __text
\ and copies it back at every start. The image stores that heap as its non-zero
\ runs, so a table's zero tail costs no image bytes - but it still costs address
\ space and copy time - while every byte that is NOT zero travels in full,
\ whether or not the table's consumer will ever read it. A fill pointer, a
\ sentinel row and a tail of entries left over from the build are all non-zero,
\ and none of them is visible in a total. This census attributes every byte of
\ [0, boot DP) to the word that owns it, so an offender can be named.
\
\ WHAT IT WALKS AND WHY THAT IS THE WHOLE HEAP. A `create`d or `variable` word
\ is stamped DKIND:ADDR by its definer (src/habu/layout.f) and no other kind of
\ record owns DATA, so the stamped records ARE the heap's owners. Sorted by
\ address they partition the heap exactly: `allot` only moves DP forward, so
\ each base runs to the next one and the last runs to DP. The span below the
\ first base has no owner and is reported as two rows, split at DATA-START: the
\ engine's own fixed cells below it, and the heap the seed and the boot prefix
\ allotted above it before the first `create`. Both carry wid -1.
\
\ HOW A BASE IS READ. No record slot holds the address a `create`d word pushes.
\ Its body is EMIT-CREATE's own (src/habu/habu2.f): a recorded MOVZ/MOVK
\ carrier, then the push stencil and RET. So the address is read back
\ out of that chain with the decoder its emitter's relocation pass uses,
\ SNAP-RELOC:CHAIN-VALUE. The DKIND:ADDR stamp is what makes that read sound - it
\ says the body is still the definer's own, and `does>` clears it in the same
\ window it patches the RET into a clause branch. A decoded value outside the
\ DATA region ends the census by name rather than naming a wrong owner.
\
\ Run it in any engine; the rows are tab-separated for sorting. The entry is
\ not called on load, so `tools/check.f` can carry this file through its child
\ without the table filling the captured output:
\   printf 'require tools/data-table-census.f\nDATA-CENSUS:RUN\n' | bin/hb
\ Columns: offset, extent, fill, non-zero, runs, cost, wid, owner. `extent` is
\ the bytes the owner holds up to the next owner, `fill` is one past its last
\ non-zero byte (what a capture sized to content would carry) and `non-zero`
\ counts the bytes inside the extent that are not zero. `runs` and `cost` model
\ what the capture would charge for the extent, so they follow the scanners
\ (aot-capture.f ACAP-SCAN-SEG, aot-lib.f BUILD-SPARSE-DATA) rather than the raw
\ content: a cell that holds anything costs the unsigned LEB128 of its value, and
\ the bitmap travels in groups - AOT-WINDOW:GROUP-SPAN bytes of the extent whose
\ cells are all zero cost nothing at all, and one that holds any cell costs its
\ whole AOT-WINDOW:GROUP-BYTES of bitmap (src/habu/aot-decl.f BM-COMPACT).
\ `cost` is those value bytes plus the groups the extent keeps. A table of cells
\ holding small numbers still costs more than its non-zero bytes;
\ sort on this column rather than on non-zero.
\
\ Runs are counted inside one owner's extent, so an owner boundary splits a run
\ the capture would keep whole. That over-counts by at most one row per owner.
\ Groups are counted from the extent's own base rather than from the capture
\ base the image's map is aligned to, so two neighbours sharing a group are each
\ charged for it: at most one group per owner boundary.

\ The boot DP, latched before this tool's own requires allocate anything. The
\ census covers [0, DP0); everything the tool itself allots lands above it.
package DATA-CENSUS
here data-base - constant DP0
;package

require lib/fmt.f
require lib/sort.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package DATA-CENSUS

\ One row per owner: the DATA offset in the high half, the dictionary index in
\ the low half, so the two travel through one comparator.
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

\ The address the record's body pushes, as a DATA offset.
: REC-OFF ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p p rec XREF-CODE-BYTES + SNAP-RELOC:CHAIN-SIZE {: size:n :}
   size 0= if s" data-table-census: malformed DKIND:ADDR body" 74 die then
   p size SNAP-RELOC:CHAIN-VALUE DATA-VA VA>N - {: off:n :}
   off 0 < off DATA-SIZE >= or if
      s" data-table-census: DKIND:ADDR body decodes outside DATA" 74 die then
   off ;

: OWNER? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-RETIRED? if false exit then
   rec XREF-START 0= if false exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR = ;

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

\ One cell of the extent, read as the unsigned number its bytes spell. Bytes
\ past the extent read as the zeros a capture would leave there.
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

\ The group a present cell puts in the image, charged once: the cells walk in
\ ascending order, so a group is charged when the walk first enters it.
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

\ The span no record owns, split where the engine's fixed cells end.
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
