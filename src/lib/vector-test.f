\ vector-test.f - focused tests for the typed vector<t>: its elements, its
\ growth through its allocator, and the frees DELETE and a refused NEW make.
\ Run: bin/hb --load src/lib/vector-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/allocator.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/arena.f

: VECT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

DEFLINEAR vect-lin

package VECT-TYPED

STRUCTURE turn DERIVE addr FIELD id n FIELD step n ;STRUCTURE

: CELLS-N ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:NEW makes zeroed elements of n that keep what is written" T-LABEL
   al 3 [type] n VEC:NEW {: v:ptr :}
   v VEC:COUNT 3 T=
   v 0 VEC:AT @ 0 T=  v 2 VEC:AT @ 0 T=
   5 v 0 VEC:AT !  6 v 2 VEC:AT !
   v 0 VEC:AT @ 5 T=  v 1 VEC:AT @ 0 T=  v 2 VEC:AT @ 6 T=
   v VEC:DELETE ;

: CELLS-R ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so of r" T-LABEL
   al 2 [type] r VEC:NEW {: v:ptr :}
   2.5 v 1 VEC:AT !
   v 0 VEC:AT @ 0.0 f= TTRUE  v 1 VEC:AT @ 2.5 f= TTRUE ;

: RECORDS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so of a record, past an index's error" T-LABEL
   al 4 [type] turn VEC:NEW {: v:ptr :}
   7 v 3 VEC:AT TURN-ID !  8 v 3 VEC:AT TURN-STEP !
   v 3 VEC:AT TURN-ID @ 7 T=  v 3 VEC:AT TURN-STEP @ 8 T=
   v 2 VEC:AT TURN-STEP @ 0 T=
   [: v 4 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   [: v -1 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ ;

: EMPTY ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" a vector of no elements has no index; a negative count is refused" T-LABEL
   al 0 [type] n VEC:NEW {: v:ptr :}
   v VEC:COUNT 0 T=
   [: v 0 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v VEC:DELETE
   [: al -1 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $2000000000000001 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $7FFFFFFFFFFFFFFF [type] u8 VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ ;

: GROWS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:PUSH answers a zeroed new element; 1000 keep their values" T-LABEL
   al 0 [type] n VEC:NEW {: v:ptr :}
   1000 0 ?do v VEC:PUSH dup @ 0 T= i swap ! loop
   v VEC:COUNT 1000 T=
   1000 0 ?do v i VEC:AT @ i T= loop
   v VEC:DELETE ;

: PUSH-TURNS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and pushes records whose fields read back through VEC:AT" T-LABEL
   al 0 [type] turn VEC:NEW {: v:ptr :}
   v VEC:PUSH {: a:ptr :}  7 a TURN-ID !  8 a TURN-STEP !
   v VEC:PUSH {: b:ptr :}  9 b TURN-ID !  10 b TURN-STEP !
   v 0 VEC:AT TURN-ID @ 7 T=  v 0 VEC:AT TURN-STEP @ 8 T=
   v 1 VEC:AT TURN-ID @ 9 T=  v 1 VEC:AT TURN-STEP @ 10 T=
   v VEC:DELETE ;

: ENSURED ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:ENSURE keeps the elements, zeroes new ones, never shrinks" T-LABEL
   al 3 [type] n VEC:NEW {: v:ptr :}
   1 v 0 VEC:AT !  2 v 1 VEC:AT !  3 v 2 VEC:AT !
   v 10 VEC:ENSURE
   v VEC:COUNT 10 T=
   3 0 ?do v i VEC:AT @ i 1 + T= loop
   10 3 ?do v i VEC:AT @ 0 T= loop
   v 5 VEC:ENSURE  v VEC:COUNT 10 T=
   v VEC:DELETE ;

: RESIZED ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:RESIZE cuts elements and grows back to zeroed ones" T-LABEL
   al 10 [type] n VEC:NEW {: v:ptr :}
   10 0 ?do i 1 + v i VEC:AT ! loop
   v 4 VEC:RESIZE
   v VEC:COUNT 4 T=
   [: v 4 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v 10 VEC:RESIZE
   4 0 ?do v i VEC:AT @ i 1 + T= loop
   10 4 ?do v i VEC:AT @ 0 T= loop
   v 0 VEC:RESIZE  v VEC:COUNT 0 T=
   v VEC:DELETE ;

: SPANS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:SPAN covers the elements in place" T-LABEL
   al 0 [type] n VEC:NEW {: e:ptr :}
   e VEC:SPAN SPAN:LEN 0 T=
   al 3 [type] n VEC:NEW {: v:ptr :}
   9 v 0 VEC:AT !
   v VEC:SPAN {: s :}
   s SPAN:LEN 3 cells T=
   s 0 SPAN:CELL@ 9 T=
   al 2 [type] turn VEC:NEW {: t:ptr :}
   t VEC:SPAN SPAN:LEN  [type] turn width>n 2 *  T=
   e VEC:DELETE  v VEC:DELETE  t VEC:DELETE ;

: BAD-COUNTS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:ENSURE and VEC:RESIZE refuse a negative count or one too big" T-LABEL
   al 1 [type] n VEC:NEW {: v:ptr :}
   [: v -1 VEC:ENSURE ;] E-VEC-CAPACITY TTHROWSQ
   [: v -1 VEC:RESIZE ;] E-VEC-CAPACITY TTHROWSQ
   [: v $2000000000000001 VEC:ENSURE ;] E-VEC-CAPACITY TTHROWSQ
   v VEC:DELETE ;

TYPED-VARIABLE NO-VEC ptr VEC:vector<n>

: DELETE-NULL ( -- )
   s" VEC:DELETE of NULL-PTR does nothing" T-LABEL
   [: NO-VEC @ VEC:DELETE ;] 0 TTHROWSQ ;

\ An allocator bumping through POOL that counts its calls, resizes and frees,
\ keeps the last length asked, records each freed offset in FREED, refuses over
\ 2000 bytes or past POOL's end and moves each resized block.
4096 constant POOL-BYTES
POOL-BYTES BUFFER: POOL
variable TOP
variable ALLOCS
variable RESIZE-CALLS
variable FREES
variable LAST-N
8 TYPED-BUFFER FREED n
TYPED-VARIABLE AL ALLOC:allocator

: P-ALLOC ( ptr u8 n -- ptr u8 )
   {: st:ptr len:n :}
   1 ALLOCS +!  len LAST-N !
   len 2000 >  TOP @ len + POOL-BYTES >  or if E-MEM-SIZE throw then
   st TOP @ +  len 7 + -8 and TOP +! ;

: P-RESIZE ( ptr u8 ptr u8 n n -- ptr u8 )
   {: st:ptr block:ptr old:n new:n :}
   1 RESIZE-CALLS +!
   st new P-ALLOC {: moved:ptr :}
   block moved old new min BYTE-COPY moved ;

: P-FREE ( ptr u8 ptr u8 -- )
   {: st:ptr p:ptr :}
   p NULL-PTR - st NULL-PTR - - FREES @ FREED !  1 FREES +! ;

: DELETE-SOME ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 3 [type] n VEC:NEW {: v:ptr :}
   ALLOCS @ a0 - 2 T=  LAST-N @ 3 cells T=
   v VEC:DELETE  FREES @ f0 - 2 T=
   f0 FREED @  h [type] VEC:vector<n> width>n +  T=
   f0 1 + FREED @ h T= ;

: DELETE-NONE ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 0 [type] n VEC:NEW {: v:ptr :}
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
   AL 3 [type] n VEC:NEW {: v:ptr :}
   1 v 0 VEC:AT !  2 v 1 VEC:AT !  3 v 2 VEC:AT !
   [: v 1000 VEC:ENSURE ;] E-MEM-SIZE TTHROWSQ
   v VEC:COUNT 3 T=
   3 0 ?do v i VEC:AT @ i 1 + T= loop
   v VEC:DELETE ;

\ Last on POOL: growth that does not double fills it.
: AMORTIZED ( -- )
   s" sixty-four pushes double the room in at most eight resize calls" T-LABEL
   RESIZE-CALLS @ {: r0:n :}
   AL 0 [type] n VEC:NEW {: v:ptr :}
   [: 64 0 ?do v VEC:PUSH drop loop ;] 0 TTHROWSQ
   RESIZE-CALLS @ r0 - 8 <= TTRUE
   v VEC:DELETE ;

public

: RUN ( -- )
   POOL [: P-ALLOC ;] [: P-RESIZE ;] [: P-FREE ;] AL ALLOC:INIT
   ARENA:NEW {: a:ptr :}
   a ARENA:ALLOCATOR {: al:ptr :}
   al CELLS-N  al CELLS-R  al RECORDS  al EMPTY
   al GROWS  al PUSH-TURNS  al ENSURED  al RESIZED
   al SPANS  al BAD-COUNTS  DELETE-NULL
   DELETES  REFUSED  REFUSED-GROWTH  AMORTIZED
   a ARENA:RELEASE ;

;package

: VECT-RUN ( -- )
   T-RESET
   VECT-TYPED:RUN
   s" VOK-TAT ( ptr VEC:vector<n> -- ptr n ) 0 VEC:AT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-PUSH ( ptr VEC:vector<n> -- ptr n ) VEC:PUSH"
   CHECK-QUIET-CANDIDATE! -1 T=
   s" VLIN-AT ( ptr VEC:vector<vect-lin> -- ptr vect-lin ) 0 VEC:AT" VECT-CHECK-REJECTS
   T-REPORT ;

VECT-RUN
