\ Elements of one width in segments of SEG-N; no element moves, so at most
\ one segment's room is unused.
\ STORAGE CLASS. CALLER-OWNED: every segment lives in the allocator its
\ caller passes.

require src/lib/allocator.f
require src/lib/vector.f

package SPARQL
private

32 constant SEG-N

STRUCTURE segs<t> DERIVE addr
   FIELD allocator ^ALLOC:allocator
   FIELD segments ^VEC:vector<^u8>
   FIELD count n
   FIELD width n
;STRUCTURE

CAST: >SEG-ELEM ( ^u8 -- ^t )
CAST: >SEGS ( ^u8 -- ^segs<t> )

: SEG-NEW ( ^ALLOC:allocator width<t> -- ^segs<t> )
   {: al:^ w :}
   al [type] segs<u8> width>n ALLOC:BYTES drop >SEGS {: s:^ :}
   al s SEGS-ALLOCATOR !
   al 0 [type] ^u8 VEC:NEW s SEGS-SEGMENTS !
   w width>n s SEGS-WIDTH !
   s ;

: SEG-COUNT ( ^segs<t> -- n ) SEGS-COUNT @ ;

\ Trusts its index: the caller keeps it below SEG-COUNT.
: SEG-AT ( ^segs<t> n -- ^t )
   {: s:^ i:n :}
   s SEGS-SEGMENTS @ i SEG-N / VEC:AT @
   i SEG-N mod s SEGS-WIDTH @ * +  >SEG-ELEM ;

\ Elements past the count are zero: a segment comes zeroed, and the count
\ never falls.
: SEG-ENSURE ( ^segs<t> n -- )
   {: s:^ k:n :}
   k s SEGS-COUNT @ <= if exit then
   s SEGS-SEGMENTS @ {: dir:^ :}
   k SEG-N 1- + SEG-N / {: need:n :}
   begin dir VEC:COUNT need < while
      s SEGS-ALLOCATOR @  SEG-N s SEGS-WIDTH @ *  ALLOC:BYTES drop
      dir VEC:PUSH !
   repeat
   k s SEGS-COUNT ! ;

: SEG-PUSH ( ^segs<t> -- ^t )
   {: s:^ :}
   s SEGS-COUNT @ {: i:n :}
   s i 1+ SEG-ENSURE
   s i SEG-AT ;

;package
