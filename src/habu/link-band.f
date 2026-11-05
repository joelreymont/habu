\ link-band.f - every arm64 shadow emission laid out back to back after the
\ kernel in A64ICODE's code buffer and placed at its link address, TEXT-VA plus
\ its offset there. Words visible only inside LINKER.

require src/lib/hashmap.f
require src/habu/xref.f
require src/compiler/native/shadow.f
require src/arch/arm64/icode.f
require src/arch/arm64/link.f
require src/arch/arm64/link-kernel.f

package LINKER
private

\ The address __text is linked at, where offset 0 of A64ICODE's code runs.
: TEXT-VA ( -- n ) VMBASE CODE-OFF + ;

\ Emission e's byte offset in A64ICODE's code buffer.
DYNAMIC-BUFFER OFFSETS n
: OFFSET@ ( n -- n ) OFFSETS @ ;

\ The host-entry table over HM: a shadow record's host entry to its link address.
DYNAMIC-BUFFER ENTRY-KEYS n
DYNAMIC-BUFFER ENTRY-USED n
DYNAMIC-BUFFER ENTRY-LINKS n
variable ENTRY-CAP
variable PLACING                     \ the emission being placed


: ENTRY-SLOT ( n -- n )
   {: key:n :}
   0 ENTRY-KEYS  0 ENTRY-USED  ENTRY-CAP @  key HM:PROBE ;


\ The name of the record entering emission e at its first byte.
: ROUTINE. ( n -- )
   {: e:n :}
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ e =  i NSHADOW:ENTRY@ 0= and if
         i NSHADOW:RECORD@ XREF-REC XREF-NAME$ type unloop exit
      then
   loop
   s" emission " type e . ;


\ The name of the dictionary record entering at a, else a itself.
: TARGET. ( n -- )
   {: a:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-START a = if i XREF-REC XREF-NAME$ type unloop exit then
   loop
   a . ;


\ A does> companion enters its emission at its clause.
: FILE-ENTRY ( n -- )
   {: r:n :}
   r NSHADOW:RECORD@ XREF-REC XREF-START {: key:n :}
   TEXT-VA  r NSHADOW:EMISSION@ OFFSET@ +  r NSHADOW:ENTRY@ + {: link:n :}
   key ENTRY-SLOT {: s:n :}
   s ENTRY-USED @ 0<> if
      s ENTRY-LINKS @ link = if exit then
      s" link: " type r NSHADOW:RECORD@ XREF-REC XREF-NAME$ type
      s"  enters at two link addresses" type cr
      s" link: one host entry at two link addresses" 74 die
   then
   1 s ENTRY-USED !  key s ENTRY-KEYS !  link s ENTRY-LINKS ! ;


\ HM:PROBE needs a power-of-two cap; at least twice the records keeps the table
\ at most half full.
: ENTRY-TABLE ( -- )
   NSHADOW:RECORDS 2 * 16 max  1 begin 2dup > while 2 * repeat nip {: cap:n :}
   cap ENTRY-CAP !
   cap ENTRY-KEYS-RESERVE  cap ENTRY-USED-RESERVE  cap ENTRY-LINKS-RESERVE
   0 ENTRY-USED cap HM:CLEAR
   NSHADOW:RECORDS 0 ?do i FILE-ENTRY loop ;


\ Each emission from A64ICODE:ASM-LEN on, in emission order.
: LAYOUT ( -- )
   NSHADOW:EMISSIONS OFFSETS-RESERVE
   A64ICODE:ASM-LEN
   NSHADOW:EMISSIONS 0 ?do
      i NSHADOW:SIZE 3 and 0<> if
         s" link: " type i ROUTINE. s"  is not whole 4-byte words" type cr
         s" link: a routine is not whole 4-byte words" 74 die
      then
      dup i OFFSETS !  i NSHADOW:SIZE +
   loop
   drop ;


\ A host address's link address: a product routine's entry, a kernel
\ primitive's, or -1. PLACE-ROUTINES builds the table first.
: LINK-ADDRESS ( n -- n )
   {: a:n :}
   a ENTRY-SLOT {: s:n :}
   s ENTRY-USED @ 0<> if s ENTRY-LINKS @ exit then
   a ARM64:KERNEL-ENTRY {: off:n :}
   off 0 >= if TEXT-VA off + exit then
   -1 ;


: TARGET-ADDRESS ( n -- n )
   {: a:n :}
   a LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   s" link: " type PLACING @ ROUTINE. s"  names " type a TARGET.
   s" , neither a product word nor a kernel primitive" type cr
   s" link: a product routine names a word outside the product" 74 die ;


: PLACE-ONE ( n [ n -- n ] -- )
   {: e:n data :}
   e NSHADOW:SIZE 4 / {: words:n :}
   words A64ICODE:ASM-CP?
   e PLACING !
   e  A64ICODE:CODE e OFFSET@ +  TEXT-VA e OFFSET@ +  data  ['] TARGET-ADDRESS
   ARM64:PLACE
   words A64ICODE:ASM-CP +! ;


\ Every emission of the open shadow, after the kernel ARM64:KERNEL wrote, its
\ data addresses through data.
: PLACE-ROUTINES ( [ n -- n ] -- )
   {: data :}
   LAYOUT
   ENTRY-TABLE
   NSHADOW:EMISSIONS 0 ?do i data PLACE-ONE loop ;

;package
