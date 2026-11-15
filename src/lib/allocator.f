\ allocator.f - an allocator: a record of functions over bytes and the state
\ they act on, each function taking the state first. A container takes one when
\ made and keeps it, so the same container code runs on any allocator. Every
\ allocator answers zeroed bytes and throws a memory error on a refused request.
\ Only RESIZE copies bytes, a block's own as it moves: a program writes bytes
\ once, into the storage they live in, and a request to copy them in points at
\ the producer.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f

package ALLOC
public

STRUCTURE allocator OPAQUE DERIVE addr
   FIELD state ^u8
   FIELD allocate [ ^u8 n -- ^u8 ]
   FIELD resize [ ^u8 ^u8 n n -- ^u8 ]
   FIELD free [ ^u8 ^u8 -- ]
;STRUCTURE

private

CAST: >ELEM ( ^u8 -- ^t )
CAST: ELEM> ( ^t -- ^u8 )
CAST: ALLOCATE>N ( [ ^u8 n -- ^u8 ] -- n )

public

\ Fills a zeroed record once; a filled one is E-ALLOC-STATE, so a
\ container frees through the functions that gave it its bytes.
: INIT ( ^u8 [ ^u8 n -- ^u8 ] [ ^u8 ^u8 n n -- ^u8 ]
         [ ^u8 ^u8 -- ] ^allocator -- )
   {: state:^ alloc resize release al:^ :}
   al ALLOCATOR-ALLOCATE @ ALLOCATE>N 0<> if E-ALLOC-STATE throw then
   state al ALLOCATOR-STATE !
   alloc al ALLOCATOR-ALLOCATE !
   resize al ALLOCATOR-RESIZE !
   release al ALLOCATOR-FREE ! ;


\ Answers n as asked; no bytes are NULL-PTR, taken from no allocator, and a
\ negative n is E-MEM-SIZE, so a function is asked only for more than zero.
: BYTES ( ^allocator n -- ^u8 n )
   {: al:^ len:n :}
   len 0= if NULL-PTR 0 exit then
   len 0 < if E-MEM-SIZE throw then
   al ALLOCATOR-STATE @ len al ALLOCATOR-ALLOCATE @ execute len ;


: FREE ( ^allocator ^u8 -- )
   {: al:^ p:^ :}
   p NULL-PTR = if exit then
   al ALLOCATOR-STATE @ p al ALLOCATOR-FREE @ execute ;


\ Keeps a block's first bytes, up to the smaller count, and zeroes added ones;
\ the block may move, and the old pointer is then dead to the caller.
: RESIZE ( ^allocator ^u8 n n -- ^u8 n )
   {: al:^ block:^ old:n new:n :}
   old 0 < new 0 < or if E-MEM-SIZE throw then
   new 0= if al block FREE NULL-PTR 0 exit then
   block NULL-PTR = if al new BYTES exit then
   new old = if block new exit then
   al ALLOCATOR-STATE @ block old new al ALLOCATOR-RESIZE @ execute new ;



: NEW ( ^allocator width<t> -- ^t ) width>n BYTES drop >ELEM ;
: DELETE ( ^allocator ^t -- ) ELEM> FREE ;

seal-package
;package
