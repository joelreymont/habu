\ ALLOC through an arena's allocator: NEW answers zeroed, distinct, cell-aligned
\ records of a structure, `n` and `r` that keep their fields; BYTES answers
\ zeroed bytes past the first chunk too; the allocator outlives a reset; a child
\ arena has its own; DELETE and FREE leave earlier records readable; BYTES and
\ FREE call a recording allocator's functions only for bytes, and INIT fills a
\ record once.
require src/lib/test.f
require src/lib/allocator.f
require src/lib/arena.f

package ALLOC-TEST

STRUCTURE turn DERIVE addr FIELD id n FIELD step n ;STRUCTURE

\ Two records' address distances: cell-aligned and distinct. A caller takes
\ each at its own type: a `ptr t` input that reaches `-` is a cell pointer.
: APART ( n n -- )
   {: x:n y:n :}
   x CELL mod 0 T=  y CELL mod 0 T=  x y <> TTRUE ;


: ZEROED ( ptr u8 n -- )
   {: p:ptr len:n :}
   0 len 0 ?do p i + c@ or loop 0 T= ;


: NEW-TURN ( ptr ALLOC:allocator -- ptr turn ) [type] turn ALLOC:NEW ;


: TURN-ZERO ( ptr turn -- )
   {: t:ptr :}
   t TURN-ID @ 0 T=  t TURN-STEP @ 0 T= ;


: TURNS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" ALLOC:NEW answers zeroed, distinct, cell-aligned records" T-LABEL
   al NEW-TURN {: t1:ptr :}
   al NEW-TURN {: t2:ptr :}
   t1 TURN-ZERO  t2 TURN-ZERO  t1 NULL-PTR - t2 NULL-PTR - APART
   s" that keep the fields written" T-LABEL
   1 t1 TURN-ID !  2 t1 TURN-STEP !  3 t2 TURN-ID !  4 t2 TURN-STEP !
   t1 TURN-ID @ 1 T=  t1 TURN-STEP @ 2 T=
   t2 TURN-ID @ 3 T=  t2 TURN-STEP @ 4 T= ;


: CELLS-N ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so for n" T-LABEL
   al [type] n ALLOC:NEW {: p1:ptr :}
   al [type] n ALLOC:NEW {: p2:ptr :}
   p1 @ 0 T=  p2 @ 0 T=  p1 NULL-PTR - p2 NULL-PTR - APART
   5 p1 !  6 p2 !  p1 @ 5 T=  p2 @ 6 T= ;


: CELLS-R ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so for r" T-LABEL
   al [type] r ALLOC:NEW {: p1:ptr :}
   al [type] r ALLOC:NEW {: p2:ptr :}
   p1 @ 0.0 f= TTRUE  p2 @ 0.0 f= TTRUE  p1 NULL-PTR - p2 NULL-PTR - APART
   2.5 p1 !  -1.5 p2 !  p1 @ 2.5 f= TTRUE  p2 @ -1.5 f= TTRUE ;


\ $8000 bytes pass the first chunk, so they come from a later one.
: BYTES ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" ALLOC:BYTES answers zeroed bytes and the length asked" T-LABEL
   al 100 ALLOC:BYTES dup 100 T= ZEROED
   s" and so past the first chunk" T-LABEL
   al $8000 ALLOC:BYTES dup $8000 T= ZEROED
   s" and no bytes as NULL-PTR, which ALLOC:FREE frees as nothing" T-LABEL
   al 0 ALLOC:BYTES 0 T= {: z:ptr :}
   z NULL-PTR = TTRUE
   al z ALLOC:FREE ;


: RECORDS ( -- )
   ARENA:NEW {: a:ptr :}
   a ARENA:ALLOCATOR {: al:ptr :}
   al TURNS  al CELLS-N  al CELLS-R  al BYTES
   a ARENA:RELEASE ;


\ The record after a reset sits where a written one was, so a header or a
\ chunk the reset left unzeroed shows in it.
: RESETS ( -- )
   s" after ARENA:RESET the allocator answers a fresh zeroed record" T-LABEL
   ARENA:NEW {: a:ptr :}
   a ARENA:ALLOCATOR {: al:ptr :}
   al NEW-TURN {: t1:ptr :}
   7 t1 TURN-ID !  8 t1 TURN-STEP !
   a ARENA:RESET
   a ARENA:ALLOCATOR NULL-PTR - al NULL-PTR - T=
   al NEW-TURN TURN-ZERO
   a ARENA:RELEASE ;


: CHILDREN ( -- )
   s" a child from ARENA:NEW-IN has its own allocator" T-LABEL
   ARENA:NEW {: a:ptr :}
   a ARENA:NEW-IN {: c:ptr :}
   c ARENA:ALLOCATOR {: cl:ptr :}
   cl NULL-PTR - a ARENA:ALLOCATOR NULL-PTR - <> TTRUE
   cl NEW-TURN TURN-ZERO
   a ARENA:RELEASE ;


: FREES ( -- )
   s" ALLOC:DELETE and ALLOC:FREE leave earlier records readable" T-LABEL
   ARENA:NEW {: a:ptr :}
   a ARENA:ALLOCATOR {: al:ptr :}
   al NEW-TURN {: t1:ptr :}
   al NEW-TURN {: t2:ptr :}
   1 t1 TURN-ID !  2 t2 TURN-ID !
   al 16 ALLOC:BYTES drop {: b:ptr :}
   al t2 ALLOC:DELETE
   al b ALLOC:FREE
   t1 TURN-ID @ 1 T=  t2 TURN-ID @ 2 T=
   a ARENA:RELEASE ;


4096 BUFFER: POOL
variable TOP
variable ALLOCS
variable FREES-MADE
variable LAST-N
variable FREED-STATE
variable FREED-AT
TYPED-VARIABLE AL ALLOC:allocator

: OFFSET ( ptr u8 -- n ) NULL-PTR - POOL NULL-PTR - - ;


\ A bump allocator over POOL that counts its calls and keeps the last length.
: P-ALLOC ( ptr u8 n -- ptr u8 )
   {: st:ptr len:n :}
   1 ALLOCS +!  len LAST-N !
   st TOP @ +  len 7 + -8 and TOP +! ;


: P-FREE ( ptr u8 ptr u8 -- )
   {: st:ptr p:ptr :}
   1 FREES-MADE +!  st OFFSET FREED-STATE !  p OFFSET FREED-AT ! ;


: FILL ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   POOL [: P-ALLOC ;] [: P-FREE ;] al ALLOC:INIT ;


: NO-CALLS ( -- )
   AL 0 ALLOC:BYTES 0 T= {: z:ptr :}
   z NULL-PTR = TTRUE
   AL z ALLOC:FREE
   ALLOCS @ 0 T=  FREES-MADE @ 0 T= ;


: ONE-CALL ( -- )
   AL 16 ALLOC:BYTES 16 T= {: b:ptr :}
   ALLOCS @ 1 T=  LAST-N @ 16 T=
   AL b ALLOC:FREE
   FREES-MADE @ 1 T=  FREED-STATE @ 0 T=  FREED-AT @ b OFFSET T= ;


: REFUSED ( -- )
   [: AL -1 ALLOC:BYTES drop drop ;] E-MEM-SIZE TTHROWSQ
   ALLOCS @ 1 T= ;


: REFILLS ( -- )
   [: AL FILL ;] E-ALLOC-STATE TTHROWSQ
   ARENA:NEW {: a:ptr :}
   [: a ARENA:ALLOCATOR FILL ;] E-ALLOC-STATE TTHROWSQ
   a ARENA:RELEASE ;


\ TOP starts a cell in, so no pointer is the state itself.
: CALLS ( -- )
   s" ALLOC:BYTES asks the function only for bytes; ALLOC:FREE hands it the state and the pointer" T-LABEL
   CELL TOP !  AL FILL
   NO-CALLS ONE-CALL REFUSED REFILLS ;


: RUN ( -- )
   T-RESET
   RECORDS RESETS CHILDREN FREES CALLS ;

RUN
T-REPORT

;package
