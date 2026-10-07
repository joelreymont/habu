\ Bounded operations on borrowed C2 byte views.
require lib/c2-memory.f

package C2-BYTES
public

\ A read view is (base, byte bound). Return its bound without exposing base.
: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> ) C2-MEM:LENGTH ;

: MUT-LENGTH ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> ) C2-MEM:MUT-LENGTH ;

: COPY$ ( mut-view<p,q,a,u8> ptr u8 n -- mut-view<p,q,a,u8> n )
   {: source:ptr size:n :}
   MUT-LENGTH swap {: cap:n :}
   size 0 < if E-SPAN-RANGE throw then
   size cap > if E-SPAN-CAPACITY throw then
   size 0 ?do i source + c@ i swap C2-MEM:MUT-BYTE! loop
   size ;

: SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> ) C2-MEM:SLICE ;

: PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> ) C2-MEM:PREFIX ;

\ A shared source is read synchronously, before its caller resumes or closes
\ its loan. The destination keeps the same unique authority and byte bound.
: COPY ( mut-view<d,e,a,u8> read-view<p,q,u8> -- mut-view<d,e,a,u8> n )
   {: source :}
   source LENGTH {: size:n kept :}
   MUT-LENGTH swap {: cap:n :}
   size cap > if E-SPAN-CAPACITY throw then
   size 0 ?do
      kept i C2-MEM:BYTE@ drop
      i swap C2-MEM:MUT-BYTE!
   loop
   size ;

: EQUAL? ( read-view<p,q,u8> read-view<x,y,u8> -- bool )
   {: left right :}
   left LENGTH {: left-size:n a :}
   right LENGTH {: right-size:n b :}
   left-size right-size <> if false exit then
   left-size 0 ?do
      a i C2-MEM:BYTE@ drop
      b i C2-MEM:BYTE@ drop
      <> if false unloop exit then
   loop
   true ;

;package
