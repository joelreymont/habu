\ heap-churn.f - test-only: an allocator takes 100000 blocks of 100 B to 1 MiB,
\ an octave as likely as another, in an order a seeded draw of 512 slots sets:
\ an empty slot takes a block, and a full one frees its block or resizes it to
\ a new size, each as likely. Halfway, just before a block is taken, a request
\ over 4 GiB is made. Each block holds its own mark in its first and last cells
\ and one cell each 4 KiB; a zero check reads the same cells, or every byte of
\ a span under a cell. RUN answers the blocks taken or grown with a nonzero
\ byte, the blocks freed or resized with a kept mark changed, the over-4 GiB
\ request's throw code and the highest live bytes. test/wasm/heap-test.f runs
\ it in a Wasm module, where test.f does not build, so it counts and its
\ caller judges.
\ STORAGE CLASS. PROCESS-WIDE: the slots are static; one task runs it.

require src/lib/allocator.f
require src/lib/property.f

package HEAP-CHURN
private

100000 constant BLOCKS
512 constant SLOTS
100 constant LEAST
$100000 constant LARGEST
14 constant OCTAVES                 \ LEAST doubled 13 times is under LARGEST
7 constant SEED
4096 constant STRIDE                \ bytes from one marked cell to the next
$100000001 constant HUGE            \ a byte past Wasm memory32's 4 GiB

TYPED-VARIABLE AL ^ALLOC:allocator
SLOTS TYPED-BUFFER PTRS ^u8
SLOTS TYPED-BUFFER SIZES n
SLOTS TYPED-BUFFER MARKS n
variable TAKEN
variable STAMPS                     \ marks written so far
variable UNZEROED
variable CHANGED
variable REFUSAL
variable LIVE                       \ bytes the slots hold
variable HIGHEST


: SIZE ( -- n )
   OCTAVES PROP:RND% {: o:n :}
   LEAST o lshift {: lo:n :}
   lo 2 * LARGEST 1+ min {: hi:n :}
   lo  hi lo - PROP:RND%  + ;


\ The bound of the STRIDE-th cells that end at or before the last cell: the
\ last cell's mark would overwrite part of one that overlapped it. At least 0,
\ as ?do ... +loop skips only equal bounds.
: CLEAR ( n -- n ) CELL 2 * - 1+ 0 max ;


\ Writes a mark into those STRIDE-th cells and the last cell.
: MARK ( ^u8 n n -- )
   {: p:^ len:n w:n :}
   len CLEAR 0 ?do w p i + CELL-VIEW ! STRIDE +loop
   w p len + CELL - CELL-VIEW ! ;


\ Whether the STRIDE-th cells MARK writes for n bytes hold the mark.
: STRIDED? ( str n -- bool )
   {: s:str w:n :}
   s STR:LEN CLEAR 0 ?do
      s STR:DATA i + CELL-VIEW @ w <> if unloop false exit then
   STRIDE +loop true ;


: MARKED? ( str n -- bool )
   {: s:str w:n :}
   s STR:DATA s STR:LEN + CELL - CELL-VIEW @ w =  s w STRIDED?  and ;


: ZEROED? ( str -- bool )
   {: s:str :}
   s STR:LEN CELL >= if s 0 MARKED? exit then
   true  s STR:LEN 0 ?do s STR:DATA i + c@ 0= and loop ;


\ Whether a block resized from the first size to the second keeps its marks.
: KEPT? ( str n n -- bool )
   {: s:str new:n w:n :}
   new s STR:LEN >= if s w MARKED? exit then
   s STR:DATA new >str w STRIDED? ;


: LIVE+ ( n -- ) LIVE +!  LIVE @ HIGHEST @ max HIGHEST ! ;


\ Marks a block with a new mark and puts it in a slot.
: SET-SLOT ( str n -- )
   {: s:str i:n :}
   1 STAMPS +!  STAMPS @ {: w:n :}
   s STR:DATA s STR:LEN w MARK
   s STR:DATA i PTRS !  s STR:LEN i SIZES !  w i MARKS ! ;


: REFUSE ( -- ) [: AL @ HUGE ALLOC:BYTES drop drop ;] catch REFUSAL ! ;


: TAKE ( n -- )
   {: i:n :}
   TAKEN @ BLOCKS 2 / = if REFUSE then
   SIZE {: len:n :}
   AL @ len ALLOC:BYTES drop {: p:^ :}
   p len >str ZEROED? 0= if 1 UNZEROED +! then
   1 TAKEN +!  len LIVE+
   p len >str i SET-SLOT ;


: GIVE ( n -- )
   {: i:n :}
   i PTRS @ {: p:^ :}
   p i SIZES @ >str i MARKS @ MARKED? 0= if 1 CHANGED +! then
   AL @ p ALLOC:FREE
   i SIZES @ negate LIVE+
   NULL-PTR i PTRS ! ;


\ The bytes a block gains, past its old size, come zeroed.
: GROWN ( ^u8 n n -- )
   {: q:^ old:n new:n :}
   new old <= if exit then
   q old + new old - >str ZEROED? 0= if 1 UNZEROED +! then ;


: RESIZE-SLOT ( n -- )
   {: i:n :}
   i SIZES @ {: old:n :}
   SIZE {: new:n :}
   AL @ i PTRS @ old new ALLOC:RESIZE drop {: q:^ :}
   q old >str new i MARKS @ KEPT? 0= if 1 CHANGED +! then
   q old new GROWN
   new old - LIVE+
   q new >str i SET-SLOT ;


: STEP ( -- )
   SLOTS PROP:RND% {: i:n :}
   i PTRS @ NULL-PTR = if i TAKE exit then
   2 PROP:RND% 0= if i GIVE else i RESIZE-SLOT then ;


: RESET ( -- )
   0 TAKEN !  0 STAMPS !  0 UNZEROED !  0 CHANGED !
   0 REFUSAL !  0 LIVE !  0 HIGHEST !
   SLOTS 0 ?do NULL-PTR i PTRS ! loop ;

public

: RUN ( ^ALLOC:allocator -- n n n n )
   AL !
   RESET
   SEED PROP:SEED!
   begin TAKEN @ BLOCKS < while STEP repeat
   SLOTS 0 ?do i PTRS @ NULL-PTR <> if i GIVE then loop
   UNZEROED @ CHANGED @ REFUSAL @ HIGHEST @ ;

;package
