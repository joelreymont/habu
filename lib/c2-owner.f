\ Append-only byte allocations owned by an explicit checked owner handle.
require lib/c2-memory.f

package C2-MEM
public

STRUCTURE owner-state 0 OPAQUE FIELD node ptr n ;STRUCTURE
STRUCTURE owner 3 OPAQUE FIELD state mut-view<a,b,c,init<b,owner-state>> ;STRUCTURE

private

CAST: OWNER-VIEW-UNPACK ( mut-view<p,i,a,init<i,owner-state>> -- ptr u8 n )
CAST: OWNER-VIEW-PACK ( ptr u8 n -- mut-view<p,i,a,init<i,owner-state>> )

public

: OWNER-SIZE ( -- NUM:alloc-byte-len ) 8 MEM:BYTES-ALLOC-LEN ;
: SEED-OWNER ( -- owner-state ) NULL$ drop CELL-VIEW OWNER-STATE-MAKE ;
: BIND ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> ) OWNER-VIEW-UNPACK BIND-STATE OWNER-VIEW-PACK OWNER-MAKE ;
: UNBIND ( owner<p,i,a> -- mut-view<p,i,a,init<i,owner-state>> ) OWNER-UNMAKE ;

\ The checked disposer consumes the unique view. Its two physical cells are
\ the pointer and allocation length accepted by APPEND's private callback ABI.
TRUSTED: ALLOC-DISPOSE ( owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,fresh-region-b,u8> -- ] -- owner<p,i,a> mut-view<p,p,fresh-region-b,u8> )
   {: size:NUM:alloc-byte-len dispose :}
   OWNER-UNMAKE OWNER-VIEW-UNPACK {: base:ptr bound:n :}
   base OWNER-FRAME size dispose APPEND {: resource:ptr extent:n :}
   base bound resource extent ;

: PUBLISH ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) MUT-UNPACK READ-PACK ;

: ALLOC ( owner<p,i,a> NUM:alloc-byte-len -- owner<p,i,a> mut-view<p,p,fresh-region-a,u8> )
   [: PUBLISH drop ;] ALLOC-DISPOSE ;

;package
