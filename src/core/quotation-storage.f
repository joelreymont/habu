\ Native lowering calls this for a checker-proven quotation store. The physical
\ store has the ordinary polymorphic cell effect; only the elaborator selects
\ it for code values. Mapped runtime buffers are transient and need no DATA row.
package QUOTATION-STORAGE
public

\ DATA-VA is the target image's fixed mapping, not translated to the host in a foreign window.
: STORE ( a ptr a -- )
   dup NULL-PTR - DATA-VA -
   dup 0 >= swap SNAP-RELOC:XTCELL-OFF-MAX <= and
   if xt! else ! then ;

;package
