\ Append-only byte allocations owned by an explicit checked owner handle.
require lib/c2-memory.f

package C2-MEM
public

STRUCTURE owner-state 0 FIELD node ptr n ;STRUCTURE
STRUCTURE owner 3 FIELD state mut-view<a,b,c,init<b,owner-state>> ;STRUCTURE

private

TRUSTED: HIDE-OWNER ( n -- ) int-mark ;
ndict@ 1- constant HIDE-OWNER-ID
s" C2--MEM-OWNER--STATE:MAKE" XREF-FIND-INDEX HIDE-OWNER
s" C2--MEM-OWNER--STATE:UNMAKE" XREF-FIND-INDEX HIDE-OWNER
s" C2--MEM-OWNER:MAKE" XREF-FIND-INDEX HIDE-OWNER
s" C2--MEM-OWNER:UNMAKE" XREF-FIND-INDEX HIDE-OWNER

TRUSTED: SEED-RAW ( -- owner-state ) 0 ;
ndict@ 1- constant SEED-RAW-ID

TRUSTED: BIND-RAW ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> )
   BIND-STATE ;
ndict@ 1- constant BIND-RAW-ID

TRUSTED: UNBIND-RAW ( owner<p,i,a> -- mut-view<p,i,a,init<i,owner-state>> ) ;
ndict@ 1- constant UNBIND-RAW-ID

TRUSTED: OWNER-UNPACK ( owner<p,i,a> -- ptr u8 n ) ;
ndict@ 1- constant OWNER-UNPACK-ID

TRUSTED: PUBLISH-RAW ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) ;
ndict@ 1- constant PUBLISH-RAW-ID

public

: OWNER-SIZE ( -- NUM:alloc-byte-len ) 8 MEM:BYTES-ALLOC-LEN ;
: SEED-OWNER ( -- owner-state ) SEED-RAW ;
: BIND ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> ) BIND-RAW ;
: UNBIND ( owner<p,i,a> -- mut-view<p,i,a,init<i,owner-state>> ) UNBIND-RAW ;

\ The checked disposer consumes the unique view. Its two physical cells are
\ the pointer and allocation length accepted by APPEND's private callback ABI.
TRUSTED: ALLOC-DISPOSE ( owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,fresh-region-b,u8> -- ] -- owner<p,i,a> mut-view<p,p,fresh-region-b,u8> )
   {: size:NUM:alloc-byte-len dispose :}
   OWNER-UNPACK {: base:ptr bound:n :}
   base OWNER-FRAME size dispose APPEND {: resource:ptr extent:n :}
   base bound resource extent ;

: PUBLISH ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) PUBLISH-RAW ;

: ALLOC ( owner<p,i,a> NUM:alloc-byte-len -- owner<p,i,a> mut-view<p,p,fresh-region-a,u8> )
   [: PUBLISH drop ;] ALLOC-DISPOSE ;

private
SEED-RAW-ID HIDE-OWNER
BIND-RAW-ID HIDE-OWNER
UNBIND-RAW-ID HIDE-OWNER
OWNER-UNPACK-ID HIDE-OWNER
PUBLISH-RAW-ID HIDE-OWNER
HIDE-OWNER-ID HIDE-OWNER

;package
