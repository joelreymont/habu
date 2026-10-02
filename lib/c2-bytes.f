\ Bounded operations on borrowed C2 byte views.
require lib/c2-memory.f

package C2-BYTES
private

TRUSTED: HIDE ( n -- ) int-mark ;
ndict@ 1- constant HIDE-ID
TRUSTED: READ-UNPACK ( read-view<p,q,u8> -- ptr u8 n ) ;
ndict@ 1- constant READ-UNPACK-ID
TRUSTED: MUT-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n ) ;
ndict@ 1- constant MUT-UNPACK-ID

: CHECK-SLICE ( n n n -- ) {: off:n count:n bound:n :}
   \ Subtract only after the offset is inside the bound; off + count may wrap.
   off 0 < count 0 < or off bound > or if E-SPAN-RANGE throw then
   count bound off - > if E-SPAN-RANGE throw then ;

TRUSTED: C-SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> )
   {: off:n count:n :} READ-UNPACK {: base:ptr bound:n :}
   off count bound CHECK-SLICE
   base off + count ;
ndict@ 1- constant SLICE-ID

TRUSTED: C-PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> )
   {: count:n :} MUT-UNPACK {: base:ptr bound:n :}
   0 count bound CHECK-SLICE
   base count ;
ndict@ 1- constant PREFIX-ID

public

\ A read view is (base, byte bound). Return its bound without exposing base.
TRUSTED: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   READ-UNPACK dup -rot ;

TRUSTED: MUT-LENGTH ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   MUT-UNPACK dup -rot ;

: COPY$ ( mut-view<p,q,a,u8> ptr u8 n -- mut-view<p,q,a,u8> n )
   {: source:ptr size:n :}
   MUT-LENGTH swap {: cap:n :}
   size 0 < if E-SPAN-RANGE throw then
   size cap > if E-SPAN-CAPACITY throw then
   size 0 ?do i source + c@ i swap C2-MEM:MUT-BYTE! loop
   size ;

: SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> )
   C-SLICE ;

: PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> )
   C-PREFIX ;

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

private
SLICE-ID HIDE
PREFIX-ID HIDE
READ-UNPACK-ID HIDE
MUT-UNPACK-ID HIDE
HIDE-ID HIDE

;package
