\ Scoped C2 allocation and nonowning loans. The public entries are raw until
\ the exact native image entries receive the checker-owned scope kinds.
\ This hidden runtime is source composition, not a separately provided module,
\ so it lives below lib/c2-memory/ and not among the flat modules: loaded alone
\ on a product it would reopen the sealed C2-MEM and exit 84.
include lib/c2-memory/owner-runtime.f
require lib/memory.f
require lib/num-types.f

package C2-MEM
private

CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

: ALLOC-RUN ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U )
   {: body :}
   [: MEM:RELEASE-BYTES ;] ACQUIRE-BYTES {: resource:ptr size:NUM:alloc-byte-len :}
   resource size ALLOC-LEN>N body execute 2drop ;
ndict@ 1- constant ALLOC-RUN-ID

\ A child callback's returned control cells carry no disposal authority.
: LOAN-BODY ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n ] -- S ptr u8 n )
   execute ;

: LOAN-RUN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U )
   LOAN-ENTER
   [: LOAN-BODY ;] [: LOAN-LEAVE ;] finally ;
ndict@ 1- constant LOAN-RUN-ID

: CHECK-INDEX ( n n -- ) {: idx:n bound:n :}
   idx 0 < idx bound >= or if E-SPAN-RANGE throw then ;

: CHECK-SLICE ( n n n -- ) {: off:n count:n bound:n :}
   \ Subtract only after the offset is inside the bound; off + count may wrap.
   off 0 < count 0 < or off bound > or if E-SPAN-RANGE throw then
   count bound off - > if E-SPAN-RANGE throw then ;

\ The view representation casts: a view is two cells, base and byte bound.
\ Only C2-MEM's private section may pack, and every pack below keeps or
\ narrows the cells it unpacked.
CAST: READ-UNPACK ( read-view<p,q,u8> -- ptr u8 n )
ndict@ 1- constant READ-UNPACK-ID
CAST: MUT-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n )
ndict@ 1- constant MUT-UNPACK-ID
CAST: READ-PACK ( ptr u8 n -- read-view<p,q,u8> )
ndict@ 1- constant READ-PACK-ID
CAST: MUT-PACK ( ptr u8 n -- mut-view<p,q,a,u8> )
ndict@ 1- constant MUT-PACK-ID

TRUSTED: HIDE-REP ( n -- ) int-mark ;
ndict@ 1- constant HIDE-REP-ID

public

: WITH-MUT ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U )
   [: ALLOC-RUN ;] RUN ;

: WITH-READ ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U )
   {: parent:ptr bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

: WITH-MUT-LOAN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U )
   {: parent:ptr bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

TRUSTED: WITH-INIT ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] stow-layout | U -- S ptr u8 n | U )
   INIT-RUN ;

TRUSTED: WITH-RECORDS ( R ptr u8 n n n [ R ptr u8 n -- S ptr u8 n | U -- U ] stow-layout | U -- S ptr u8 n | U )
   RECORDS-RUN ;

: WITH-RECORD ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   {: width:n stride:n align:n :}
   {: base:ptr count:n index:n callback :}
   index count CHECK-INDEX
   base index stride * + stride callback LOAN-RUN 2drop
   base count ;

\ The field offset and extent are emitted from the checker's committed layout
\ fact for this exact call. The callback owns only that span; the parent's
\ original base and bound return unchanged even when the field was edited.
: WITH-FIELD ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n | U -- S ptr u8 n | U )
   {: off:n extent:n :}
   {: base:ptr bound:n callback :}
   off 0 < extent 0 <= or off bound > or if E-SPAN-RANGE throw then
   extent bound off - > if E-SPAN-RANGE throw then
   base off + extent callback LOAN-RUN 2drop
   base bound ;

\ These three bodies unpack a view's base and byte bound, check the index
\ before access, and return the view with the cells it came with.
: BYTE@ ( read-view<p,q,u8> n -- u8 read-view<p,q,u8> )
   {: idx:n :}
   dup READ-UNPACK {: base:ptr bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ swap ;

: MUT-BYTE@ ( mut-view<p,q,a,u8> n -- u8 mut-view<p,q,a,u8> )
   swap MUT-UNPACK {: idx:n base:ptr bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ base bound MUT-PACK ;

: MUT-BYTE! ( mut-view<p,q,a,u8> n u8 -- mut-view<p,q,a,u8> )
   rot MUT-UNPACK {: idx:n value:u8 base:ptr bound:n :}
   idx bound CHECK-INDEX
   value base idx + c!
   base bound MUT-PACK ;

\ SLICE and PREFIX narrow a view inside its bound; LENGTH and MUT-LENGTH
\ return the bound without exposing the base.
: SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> )
   {: off:n count:n :}
   READ-UNPACK {: base:ptr bound:n :}
   off count bound CHECK-SLICE
   base off + count READ-PACK ;

: PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> )
   {: count:n :}
   MUT-UNPACK {: base:ptr bound:n :}
   0 count bound CHECK-SLICE
   base count MUT-PACK ;

: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   dup READ-UNPACK nip swap ;

: MUT-LENGTH ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   MUT-UNPACK {: base:ptr bound:n :}
   bound base bound MUT-PACK ;

private
HIDE-RUNTIME
HIDE-RUNTIME-ID HIDE-REP
ALLOC-RUN-ID HIDE-REP
LOAN-RUN-ID HIDE-REP
READ-UNPACK-ID HIDE-REP
MUT-UNPACK-ID HIDE-REP
READ-PACK-ID HIDE-REP
MUT-PACK-ID HIDE-REP
HIDE-REP-ID HIDE-REP

;package
