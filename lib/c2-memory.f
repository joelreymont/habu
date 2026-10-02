\ Scoped C2 allocation and nonowning loans. The public entries are raw until
\ the exact native image entries receive the checker-owned scope kinds.
\ This hidden runtime is source composition, not a separately provided module.
include lib/c2-owner-runtime.f
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

\ Reinterpret the two physical cells only inside this trusted package boundary.
TRUSTED: READ-UNPACK ( read-view<p,q,u8> -- ptr u8 n ) ;
TRUSTED: MUT-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n ) ;

TRUSTED: HIDE-REP ( n -- ) int-mark ;
ndict@ 1- constant HIDE-REP-ID
ndict@ 4 - HIDE-REP
ndict@ 3 - HIDE-REP

public

TRUSTED: WITH-MUT ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U )
   [: ALLOC-RUN ;] RUN ;

TRUSTED: WITH-READ ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U )
   {: parent:ptr bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

TRUSTED: WITH-MUT-LOAN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U )
   {: parent:ptr bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

TRUSTED: WITH-INIT ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   INIT-RUN ;

TRUSTED: WITH-RECORDS ( R ptr u8 n n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   RECORDS-RUN ;

TRUSTED: WITH-RECORD ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   {: width:n stride:n align:n :}
   {: base:ptr count:n index:n callback :}
   index count CHECK-INDEX
   base index stride * + stride callback LOAN-RUN 2drop
   base count ;

\ The field offset and extent are emitted from the checker's committed layout
\ fact for this exact call. The callback owns only that span; the parent's
\ original base and bound return unchanged even when the field was edited.
TRUSTED: WITH-FIELD ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n | U -- S ptr u8 n | U )
   {: off:n extent:n :}
   {: base:ptr bound:n callback :}
   off 0 < extent 0 <= or off bound > or if E-SPAN-RANGE throw then
   extent bound off - > if E-SPAN-RANGE throw then
   base off + extent callback LOAN-RUN 2drop
   base bound ;

\ A view is two cells: base and byte bound. These three narrow trusted bodies
\ unpack that ABI, check the index before access, and return the same view.
TRUSTED: BYTE@ ( read-view<p,q,u8> n -- u8 read-view<p,q,u8> )
   swap READ-UNPACK {: idx:n base:ptr bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ base bound ;

TRUSTED: MUT-BYTE@ ( mut-view<p,q,a,u8> n -- u8 mut-view<p,q,a,u8> )
   swap MUT-UNPACK {: idx:n base:ptr bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ base bound ;

TRUSTED: MUT-BYTE! ( mut-view<p,q,a,u8> n u8 -- mut-view<p,q,a,u8> )
   rot MUT-UNPACK {: idx:n value:u8 base:ptr bound:n :}
   idx bound CHECK-INDEX
   value base idx + c!
   base bound ;

private
HIDE-RUNTIME
HIDE-RUNTIME-ID HIDE-REP
ALLOC-RUN-ID HIDE-REP
LOAN-RUN-ID HIDE-REP
HIDE-REP-ID HIDE-REP

;package
