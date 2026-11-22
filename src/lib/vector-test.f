split-str
\ vector-test.f - focused tests for the typed vector<t>: its elements, its
\ growth through its allocator, and the frees DELETE and a refused NEW make.
\ Run: bin/hb --load src/lib/vector-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/allocator.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/arena.f

: VECT-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

DEFLINEAR vect-lin

package VECT-TYPED

STRUCTURE turn DERIVE addr FIELD id n FIELD step n ;STRUCTURE

: CELLS-N ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:NEW makes zeroed elements of n that keep what is written" T-LABEL
   al 3 [type] n VEC:NEW {: v:^ :}
   v VEC:COUNT 3 T=
   v 0 VEC:AT @ 0 T=  v 2 VEC:AT @ 0 T=
   5 v 0 VEC:AT !  6 v 2 VEC:AT !
   v 0 VEC:AT @ 5 T=  v 1 VEC:AT @ 0 T=  v 2 VEC:AT @ 6 T=
   v VEC:DELETE ;

: CELLS-R ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" and so of r" T-LABEL
   al 2 [type] r VEC:NEW {: v:^ :}
   2.5 v 1 VEC:AT !
   v 0 VEC:AT @ 0.0 f= TTRUE  v 1 VEC:AT @ 2.5 f= TTRUE ;

: RECORDS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" and so of a record" T-LABEL
   al 4 [type] turn VEC:NEW {: v:^ :}
   7 v 3 VEC:AT TURN-ID !  8 v 3 VEC:AT TURN-STEP !
   v 3 VEC:AT TURN-ID @ 7 T=  v 3 VEC:AT TURN-STEP @ 8 T=
   v 2 VEC:AT TURN-STEP @ 0 T= ;

: EMPTY ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" a vector of no elements has COUNT 0; a negative count is refused" T-LABEL
   al 0 [type] n VEC:NEW {: v:^ :}
   v VEC:COUNT 0 T=
   v VEC:DELETE
   [: al -1 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $2000000000000001 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $7FFFFFFFFFFFFFFF [type] u8 VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ ;

: GROWS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:PUSH answers a zeroed new element; 1000 keep their values" T-LABEL
   al 0 [type] n VEC:NEW {: v:^ :}
   1000 0 ?do v VEC:PUSH dup @ 0 T= i swap ! loop
   v VEC:COUNT 1000 T=
   1000 0 ?do v i VEC:AT @ i T= loop
   v VEC:DELETE ;

: PUSH-TURNS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" and pushes records whose fields read back through VEC:AT" T-LABEL
   al 0 [type] turn VEC:NEW {: v:^ :}
   v VEC:PUSH {: a:^ :}  7 a TURN-ID !  8 a TURN-STEP !
   v VEC:PUSH {: b:^ :}  9 b TURN-ID !  10 b TURN-STEP !
   v 0 VEC:AT TURN-ID @ 7 T=  v 0 VEC:AT TURN-STEP @ 8 T=
   v 1 VEC:AT TURN-ID @ 9 T=  v 1 VEC:AT TURN-STEP @ 10 T=
   v VEC:DELETE ;

: ENSURED ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:ENSURE keeps the elements, zeroes new ones, never shrinks" T-LABEL
   al 3 [type] n VEC:NEW {: v:^ :}
   1 v 0 VEC:AT !  2 v 1 VEC:AT !  3 v 2 VEC:AT !
   v 10 VEC:ENSURE
   v VEC:COUNT 10 T=
   3 0 ?do v i VEC:AT @ i 1 + T= loop
   10 3 ?do v i VEC:AT @ 0 T= loop
   v 5 VEC:ENSURE  v VEC:COUNT 10 T=
   v VEC:DELETE ;

: RESIZED ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:RESIZE cuts elements and grows back to zeroed ones" T-LABEL
   al 10 [type] n VEC:NEW {: v:^ :}
   10 0 ?do i 1 + v i VEC:AT ! loop
   v 4 VEC:RESIZE
   v VEC:COUNT 4 T=
   v 10 VEC:RESIZE
   4 0 ?do v i VEC:AT @ i 1 + T= loop
   10 4 ?do v i VEC:AT @ 0 T= loop
   v 0 VEC:RESIZE  v VEC:COUNT 0 T=
   v VEC:DELETE ;

: SPANS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:SPAN covers the elements in place" T-LABEL
   al 0 [type] n VEC:NEW {: e:^ :}
   e VEC:SPAN SPAN:LEN 0 T=
   al 3 [type] n VEC:NEW {: v:^ :}
   9 v 0 VEC:AT !
   v VEC:SPAN {: s :}
   s SPAN:LEN 3 cells T=
   s 0 SPAN:CELL@ 9 T=
   al 2 [type] turn VEC:NEW {: t:^ :}
   t VEC:SPAN SPAN:LEN  [type] turn width>n 2 *  T=
   e VEC:DELETE  v VEC:DELETE  t VEC:DELETE ;

: BAD-COUNTS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:ENSURE, VEC:RESIZE, VEC:RESERVE, VEC:TRIM and VEC:APPEND refuse a negative count or one too big" T-LABEL
   al 1 [type] n VEC:NEW {: v:^ :}
   [: v -1 VEC:ENSURE ;] E-VEC-CAPACITY TTHROWSQ
   [: v -1 VEC:RESIZE ;] E-VEC-CAPACITY TTHROWSQ
   [: v $2000000000000001 VEC:ENSURE ;] E-VEC-CAPACITY TTHROWSQ
   [: v -1 VEC:RESERVE ;] E-VEC-CAPACITY TTHROWSQ
   [: v $2000000000000001 VEC:RESERVE ;] E-VEC-CAPACITY TTHROWSQ
   [: v -1 VEC:TRIM ;] E-VEC-CAPACITY TTHROWSQ
   [: v $2000000000000001 VEC:TRIM ;] E-VEC-CAPACITY TTHROWSQ
   [: v 0 VEC:AT -1 v VEC:APPEND ;] E-VEC-CAPACITY TTHROWSQ
   [: v 0 VEC:AT $2000000000000001 v VEC:APPEND ;] E-VEC-CAPACITY TTHROWSQ
   v VEC:DELETE ;

TYPED-VARIABLE NO-VEC ^VEC:vector<n>

: DELETE-NULL ( -- )
   s" VEC:DELETE of NULL-PTR does nothing" T-LABEL
   [: NO-VEC @ VEC:DELETE ;] 0 TTHROWSQ ;

\ An allocator bumping through POOL that records its calls; each resize moves
\ the block and fills the old one with $A5, so a read of it shows.
4096 constant POOL-BYTES
POOL-BYTES BUFFER: POOL
variable TOP
variable ALLOCS
variable RESIZE-CALLS
variable FREES
variable LAST-N
10 TYPED-BUFFER FREED n
TYPED-VARIABLE AL ALLOC:allocator

: P-ALLOC ( str -- ^u8 )
   {: st:^ len:n :}
   1 ALLOCS +!  len LAST-N !
   len 2000 >  TOP @ len + POOL-BYTES >  or if E-MEM-SIZE throw then
   st TOP @ +  len 7 + -8 and TOP +! ;

: P-RESIZE ( ^u8 str n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   1 RESIZE-CALLS +!
   st new P-ALLOC {: moved:^ :}
   block moved old new min BYTE-COPY
   $A5 block old SPAN:MAKE SPAN:FILL  moved ;

: P-FREE ( ^u8 ^u8 -- )
   {: st:^ p:^ :}
   p NULL-PTR - st NULL-PTR - - FREES @ FREED !  1 FREES +! ;

: DELETE-SOME ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 3 [type] n VEC:NEW {: v:^ :}
   ALLOCS @ a0 - 2 T=  LAST-N @ 3 cells T=
   v VEC:DELETE  FREES @ f0 - 2 T=
   f0 FREED @  h [type] VEC:vector<n> width>n +  T=
   f0 1 + FREED @ h T= ;

: DELETE-NONE ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 0 [type] n VEC:NEW {: v:^ :}
   ALLOCS @ a0 - 1 T=
   v VEC:DELETE  FREES @ f0 - 1 T=  f0 FREED @ h T= ;

: DELETES ( -- )
   s" VEC:DELETE frees the data, then the header, through the vector's allocator" T-LABEL
   DELETE-SOME  DELETE-NONE ;

: REFUSED ( -- )
   s" a refused data request frees the header" T-LABEL
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   [: AL 1000 [type] n VEC:NEW drop ;] E-MEM-SIZE TTHROWSQ
   ALLOCS @ a0 - 2 T=  FREES @ f0 - 1 T=  f0 FREED @ h T= ;

: REFUSED-GROWTH ( -- )
   s" a growth the allocator refuses leaves the vector as it was" T-LABEL
   AL 3 [type] n VEC:NEW {: v:^ :}
   1 v 0 VEC:AT !  2 v 1 VEC:AT !  3 v 2 VEC:AT !
   [: v 1000 VEC:ENSURE ;] E-MEM-SIZE TTHROWSQ
   v VEC:COUNT 3 T=
   3 0 ?do v i VEC:AT @ i 1 + T= loop
   v VEC:DELETE ;

: RESERVED ( -- )
   s" VEC:RESERVE keeps COUNT; growth within it asks for no bytes and moves nothing" T-LABEL
   AL 2 [type] n VEC:NEW {: v:^ :}
   7 v 0 VEC:AT !
   v 40 VEC:RESERVE
   v VEC:COUNT 2 T=  v 0 VEC:AT @ 7 T=
   v 0 VEC:AT {: first:^ :}  ALLOCS @ {: a0:n :}
   38 0 ?do v VEC:PUSH drop loop
   v 3 VEC:RESIZE  v 0 VEC:AT 2 v VEC:APPEND  v 3 VEC:AT @ 7 T=
   v 40 VEC:ENSURE  v 40 VEC:RESERVE
   ALLOCS @ a0 T=  v 0 VEC:AT first = TTRUE
   v VEC:COUNT 40 T=  v 0 VEC:AT @ 7 T=
   v VEC:DELETE ;

: TRIMMED ( -- )
   s" VEC:TRIM sets the room to n, or COUNT when that is more" T-LABEL
   AL 10 [type] n VEC:NEW {: v:^ :}
   10 0 ?do i v i VEC:AT ! loop
   v VEC:CAP 10 T=
   v 100 VEC:RESERVE  v VEC:CAP 100 T=
   v 20 VEC:TRIM  v VEC:CAP 20 T=  LAST-N @ 20 cells T=  v VEC:COUNT 10 T=
   10 0 ?do v i VEC:AT @ i T= loop
   v 0 VEC:TRIM  v VEC:CAP 10 T=  LAST-N @ 10 cells T=
   v 30 VEC:TRIM  LAST-N @ 30 cells T=
   v VEC:DELETE ;

\ The source's byte offset, 16, passes the room of 3.
: LAST-APPEND ( -- )
   s" VEC:APPEND reads its own elements where growth moved them" T-LABEL
   AL 3 [type] n VEC:NEW {: v:^ :}
   3 0 ?do i 1 + v i VEC:AT ! loop
   v 2 VEC:AT 1 v VEC:APPEND
   v VEC:COUNT 4 T=  v 3 VEC:AT @ 3 T=
   v VEC:DELETE ;

\ The source's block lies below the vector's, which the append moves.
: OUTSIDE-APPEND ( -- )
   s" VEC:APPEND copies elements from outside the vector into a moved block" T-LABEL
   AL 3 [type] u8 VEC:NEW {: u:^ :}
   3 0 ?do i 1 + u i VEC:AT c! loop
   AL 1 [type] u8 VEC:NEW {: v:^ :}
   u VEC:SPAN SPAN:$ v VEC:APPEND
   v VEC:COUNT 4 T=  v 0 VEC:AT c@ 0 T=
   4 1 ?do v i VEC:AT c@ i T= loop
   u VEC:DELETE  v VEC:DELETE ;

\ Last on POOL: growth that does not double fills it.
: AMORTIZED ( -- )
   s" sixty-four pushes double the room in at most eight resize calls" T-LABEL
   RESIZE-CALLS @ {: r0:n :}
   AL 0 [type] n VEC:NEW {: v:^ :}
   [: 64 0 ?do v VEC:PUSH drop loop ;] 0 TTHROWSQ
   RESIZE-CALLS @ r0 - 8 <= TTRUE
   v VEC:DELETE ;

public

: RUN ( -- )
   POOL [: P-ALLOC ;] [: P-RESIZE ;] [: P-FREE ;] AL ALLOC:INIT
   ARENA:NEW {: a:^ :}
   a ARENA:ALLOCATOR {: al:^ :}
   al CELLS-N  al CELLS-R  al RECORDS  al EMPTY
   al GROWS  al PUSH-TURNS  al ENSURED  al RESIZED
   al SPANS  al BAD-COUNTS  DELETE-NULL
   DELETES  REFUSED  REFUSED-GROWTH  RESERVED  OUTSIDE-APPEND  LAST-APPEND  TRIMMED
   AMORTIZED
   a ARENA:RELEASE ;

;package

: VECT-RUN ( -- )
   T-RESET
   VECT-TYPED:RUN
   s" VOK-TAT ( ^VEC:vector<n> -- ^n ) 0 VEC:AT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-PUSH ( ^VEC:vector<n> -- ^n ) VEC:PUSH"
   CHECK-QUIET-CANDIDATE! -1 T=
   s" VLIN-AT ( ^VEC:vector<vect-lin> -- ^vect-lin ) 0 VEC:AT" VECT-CHECK-REJECTS
   T-REPORT ;

VECT-RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
: VECT-BOUNDS ( ^ALLOC:allocator -- )
   {: al:^ :}
   s" VEC:AT refuses an index outside 0 to COUNT - 1" T-LABEL
   al 4 [type] n VEC:NEW {: v:^ :}
   [: v 4 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   [: v -1 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v 2 VEC:RESIZE
   [: v 2 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v 0 VEC:RESIZE
   [: v 0 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v VEC:DELETE ;
[then]
