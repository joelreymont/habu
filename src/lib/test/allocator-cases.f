\ allocator-cases.f - test-only: the cases every allocator meets, in the
\ allocator test's package; another allocator's test calls TURNS and ZEROED.
\ STORAGE CLASS. None.

require src/lib/test.f
require src/lib/allocator.f

package ALLOC-TEST

STRUCTURE turn DERIVE addr FIELD id n FIELD step n ;STRUCTURE

\ Two records' address distances: cell-aligned and distinct. A caller takes
\ each at its own type: a `ptr t` input that reaches `-` is a cell pointer.
: APART ( n n -- )
   {: x:n y:n :}
   x CELL mod 0 T=  y CELL mod 0 T=  x y <> TTRUE ;


: NEW-TURN ( ptr ALLOC:allocator -- ptr turn ) [type] turn ALLOC:NEW ;


: TURN-ZERO ( ptr turn -- )
   {: t:ptr :}
   t TURN-ID @ 0 T=  t TURN-STEP @ 0 T= ;

public

: ZEROED ( ptr u8 n -- )
   {: p:ptr len:n :}
   0 len 0 ?do p i + c@ or loop 0 T= ;


: TURNS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" ALLOC:NEW answers zeroed, distinct, cell-aligned records" T-LABEL
   al NEW-TURN {: t1:ptr :}
   al NEW-TURN {: t2:ptr :}
   t1 TURN-ZERO  t2 TURN-ZERO  t1 NULL-PTR - t2 NULL-PTR - APART
   s" that keep the fields written" T-LABEL
   1 t1 TURN-ID !  2 t1 TURN-STEP !  3 t2 TURN-ID !  4 t2 TURN-STEP !
   t1 TURN-ID @ 1 T=  t1 TURN-STEP @ 2 T=
   t2 TURN-ID @ 3 T=  t2 TURN-STEP @ 4 T=
   al t1 ALLOC:DELETE  al t2 ALLOC:DELETE ;

;package
