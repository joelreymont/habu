\ Native lowering calls this for a checker-proven quotation store. The physical
\ store has the ordinary polymorphic cell effect; only the elaborator selects
\ it for code values. Mapped runtime buffers are transient and need no DATA row.
package QUOTATION-STORAGE
private

\ DATA-VA is the image's fixed mapping on both native targets. data-base is
\ task-local: a worker has only a 64 KiB context, not the image's DATA extent.
\ The cast types the target image's DATA-VA; it does not translate
\ that address to the host's mapping in a foreign window.
CAST: N>IMAGE ( n -- ptr u8 )
: IMAGE-BASE ( -- ptr u8 ) DATA-VA N>IMAGE ;

public

: STORE ( a ptr a -- )
   dup byte-view IMAGE-BASE -
   dup 0 >= swap SNAP-RELOC:XTCELL-OFF-MAX <= and
   if xt! else ! then ;

;package
