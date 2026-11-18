split-str
\ A fixed-segment x86-64 ELF writer. The caller owns the descriptor and the
\ three initialized spans; this file owns the ELF header, dynamic tail and
\ zero padding between the segments.
require src/core/exit-status.f
require src/os/linux-x86-64/elf.f
require src/habu/fdio.f

package X64IMAGE

private

4096 constant ZERO-BYTES
create ZEROS ZERO-BYTES allot

: CLEAR-ZEROS ( -- )
   ZERO-BYTES 0 ?do 0 ZEROS i + c! loop ;

: PAD ( n n -- ) {: fd:n count:n :}
   count ZERO-BYTES / 0 ?do fd ZEROS ZERO-BYTES FDIO:WALL loop
   fd ZEROS count ZERO-BYTES mod FDIO:WALL ;

: PAD-TO ( n n n -- ) {: fd:n from:n to:n :}
   from to > if s" x64image: segment spans overlap" EXIT-STATUS:IMAGE die then
   fd to from - PAD ;

public

\ The text and region land at their fixed addresses; DATA's bytes ride the
\ read-only carrier the boot copies into DATA, so they must fit both.
: WRITE-FD ( ^u8 n ^u8 n ^u8 n n -- )
   {: text:^ text-len:n region:^ region-len:n data:^ data-len:n fd:n :}
   text-len 0 <
   region-len REGION > or
   region-len 0 < or
   data-len LINUX-LAYOUT:DATA-SIZE > or
   data-len ELF-CARRIER-CAP > or
   data-len 0 < or if
      s" x64image: initialized span exceeds segment" EXIT-STATUS:IMAGE die
   then
   region-len ELF-REGION-BYTES !
   data-len ELF-DATA-BYTES !
   text-len ELF-HEADER-FOR
   ELF-REGION-AT REGION-OFF > if
      s" x64image: text overlaps fixed region" EXIT-STATUS:IMAGE die
   then
   CLEAR-ZEROS
   fd MBUF MLEN@ FDIO:WALL
   fd text text-len FDIO:WALL
   fd LINUX-LAYOUT:CODE-OFF text-len + ELF-TEXT-SIZE @ PAD-TO
   ELF-RW-TAIL
   fd MBUF MLEN@ FDIO:WALL
   fd ELF-TEXT-SIZE @ ELF-RW-SZ + ELF-REGION-AT PAD-TO
   fd region region-len FDIO:WALL
   fd ELF-REGION-AT region-len + ELF-DATA-AT PAD-TO
   fd data data-len FDIO:WALL
   0 ELF-REGION-BYTES !  0 ELF-DATA-BYTES ! ;

;package
