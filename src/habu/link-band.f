\ link-band.f - the arm64 shadow emissions a program's roots reach, or every one
\ for an engine, laid out back to back after the kernel in A64ICODE's code
\ buffer and placed at TEXT-VA plus their offset there. Words visible only
\ inside LINKER.

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

\ Emission e's byte offset in A64ICODE's code buffer, -1 when it is not placed.
DYNAMIC-BUFFER OFFSETS n
: OFFSET@ ( n -- n ) OFFSETS @ ;

\ The host-entry table over HM: a shadow record's host entry to the record.
DYNAMIC-BUFFER ENTRY-KEYS n
DYNAMIC-BUFFER ENTRY-USED n
DYNAMIC-BUFFER ENTRY-RECS n
variable ENTRY-CAP
variable PLACING                     \ the emission being placed

\ Each emission's records, chained from EM-FIRST through REC-NEXT; -1 ends it.
DYNAMIC-BUFFER EM-FIRST n
DYNAMIC-BUFFER REC-NEXT n

\ PLACED per emission and LIVE per record, 1 once marked; MAKE-LIVE and
\ PLACE-EM push each index they mark once.
DYNAMIC-BUFFER PLACED n
DYNAMIC-BUFFER LIVE n
DYNAMIC-BUFFER PLACE-WORK n
variable PLACE-COUNT
DYNAMIC-BUFFER LIVE-WORK n
variable LIVE-COUNT
variable WALKING                     \ the live record whose range is read

\ The dictionary records named for a kernel word a stripped program cannot run.
DYNAMIC-BUFFER UNSAFE-RECS n
variable UNSAFE-COUNT


: ENTRY-SLOT ( n -- n )
   {: key:n :}
   0 ENTRY-KEYS  0 ENTRY-USED  ENTRY-CAP @  key HM:PROBE ;


\ The shadow record whose host entry is a, or -1.
: REC-AT ( n -- n )
   ENTRY-SLOT {: s:n :}
   s ENTRY-USED @ 0= if -1 exit then
   s ENTRY-RECS @ ;


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


\ Two records share a host entry only where they enter one emission at one
\ offset; a does> companion enters its emission at its clause.
: FILE-ENTRY ( n -- )
   {: r:n :}
   r NSHADOW:RECORD@ XREF-REC XREF-START {: key:n :}
   key ENTRY-SLOT {: s:n :}
   s ENTRY-USED @ 0<> if
      s ENTRY-RECS @ {: q:n :}
      q NSHADOW:EMISSION@ r NSHADOW:EMISSION@ =
      q NSHADOW:ENTRY@ r NSHADOW:ENTRY@ = and if exit then
      s" link: " type r NSHADOW:RECORD@ XREF-REC XREF-NAME$ type
      s"  enters at two link addresses" type cr
      s" link: one host entry at two link addresses" 74 die
   then
   1 s ENTRY-USED !  key s ENTRY-KEYS !  r s ENTRY-RECS ! ;


\ HM:PROBE needs a power-of-two cap; at least twice the records keeps the table
\ at most half full.
: ENTRY-TABLE ( -- )
   NSHADOW:RECORDS 2 * 16 max  1 begin 2dup > while 2 * repeat nip {: cap:n :}
   cap ENTRY-CAP !
   cap ENTRY-KEYS-RESERVE  cap ENTRY-USED-RESERVE  cap ENTRY-RECS-RESERVE
   0 ENTRY-USED cap HM:CLEAR
   NSHADOW:RECORDS 0 ?do i FILE-ENTRY loop ;


: CHAINS ( -- )
   NSHADOW:EMISSIONS EM-FIRST-RESERVE  NSHADOW:RECORDS REC-NEXT-RESERVE
   NSHADOW:EMISSIONS 0 ?do -1 i EM-FIRST ! loop
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ {: e:n :}
      e EM-FIRST @ i REC-NEXT !
      i e EM-FIRST !
   loop ;


\ Where record r's code ends: the smallest entry above its own among its
\ emission's records, else the emission's end.
: RANGE-END ( n -- n )
   {: r:n :}
   r NSHADOW:ENTRY@ {: from:n :}
   r NSHADOW:EMISSION@ {: e:n :}
   e NSHADOW:SIZE  e EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:ENTRY@ dup from > if rot min swap else drop then
      REC-NEXT @
   repeat
   drop ;


: UNSAFE-NAME? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" create" XREF-STR=CI
   a u s" patch32" XREF-STR=CI or
   a u s" code-publish" XREF-STR=CI or
   a u s" callmap-set" XREF-STR=CI or
   a u s" addrmap-set" XREF-STR=CI or
   a u s" xref-retarget" XREF-STR=CI or
   a u s" evaluate" XREF-STR=CI or
   a u s" evaluate-range" XREF-STR=CI or
   a u s" evaluate-closed" XREF-STR=CI or ;


\ The words UNSAFE-NAME? names: a stripped program has no dictionary and no
\ writable code for the first six, and no interpreter for the evaluate words.
: UNSAFE-TABLE ( -- )
   0 UNSAFE-COUNT !
   ndict@ 0 ?do
      i XREF-REC XREF-NAME$ UNSAFE-NAME? if
         UNSAFE-COUNT @ 1+ UNSAFE-RECS-RESERVE
         i UNSAFE-COUNT @ UNSAFE-RECS !
         1 UNSAFE-COUNT +!
      then
   loop ;


\ The kept dictionary record entering at host address a, or -1.
: UNSAFE@ ( n -- n )
   {: a:n :}
   UNSAFE-COUNT @ 0 ?do
      i UNSAFE-RECS @ {: d:n :}
      d XREF-REC XREF-START a = if d unloop exit then
   loop
   -1 ;


\ The tables a link reads, its marks and worklists cleared: once per link,
\ before ROOT or REACH-ALL.
: ROUTINES ( -- )
   ENTRY-TABLE  CHAINS  UNSAFE-TABLE
   NSHADOW:EMISSIONS {: ems:n :}
   NSHADOW:RECORDS {: recs:n :}
   ems PLACED-RESERVE  ems PLACE-WORK-RESERVE
   recs LIVE-RESERVE  recs LIVE-WORK-RESERVE
   ems 0 ?do 0 i PLACED ! loop
   recs 0 ?do 0 i LIVE ! loop
   0 PLACE-COUNT !  0 LIVE-COUNT ! ;


\ Code address site k's chain value, read from the shadow's bytes as
\ ARM64:PLACE reads it.
: CHAIN-AT ( n n -- n )
   {: e:n k:n :}
   e NSHADOW:BYTES  e k NSHADOW:ADDR-SITE@ +
   ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDRESS-CARRIER:CHAIN-VALUE ;


\ The host address each call and code address site of emission e at an offset
\ in [from, to) names, handed to visit; data addresses name none.
: EACH-TARGET ( n n n [ n -- ] -- )
   {: e:n from:n to:n visit :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      s from >=  s to < and if e i NSHADOW:CALL-TARGET@ visit execute then
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-CODE = if
         e i NSHADOW:ADDR-SITE@ {: at:n :}
         at from >=  at to < and if e i CHAIN-AT visit execute then
      then
   loop ;


: PLACE-EM ( n -- )
   {: e:n :}
   e PLACED @ 0<> if exit then
   1 e PLACED !
   e PLACE-COUNT @ PLACE-WORK !
   1 PLACE-COUNT +! ;


: MAKE-LIVE ( n -- )
   {: r:n :}
   r LIVE @ 0<> if exit then
   1 r LIVE !
   r LIVE-COUNT @ LIVE-WORK !
   1 LIVE-COUNT +!
   r NSHADOW:EMISSION@ PLACE-EM ;


: CANNOT-RUN ( n -- )
   XREF-REC XREF-NAME$ type
   s" , which a stripped program cannot run" type cr
   s" link: a stripped program reaches a word it cannot run" 74 die ;


\ A target naming no record and no UNSAFE-NAME? word is a kernel primitive,
\ whose code PLACE-TARGET keeps, or a foreign word TARGET-ADDRESS refuses.
: LIVE-TARGET ( n -- )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if r MAKE-LIVE exit then
   a UNSAFE@ {: d:n :}
   d 0< if exit then
   s" link: " type WALKING @ NSHADOW:RECORD@ XREF-REC XREF-NAME$ type
   s"  calls " type d CANNOT-RUN ;


: PLACE-TARGET ( n -- )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if r NSHADOW:EMISSION@ PLACE-EM exit then
   a ARM64:ROOT-ENTRY drop ;


: LIVE-STEP ( -- )
   -1 LIVE-COUNT +!
   LIVE-COUNT @ LIVE-WORK @ {: r:n :}
   r WALKING !
   r NSHADOW:EMISSION@  r NSHADOW:ENTRY@  r RANGE-END  ['] LIVE-TARGET EACH-TARGET ;


\ Every site of a placed emission resolves at placement, its dead parts too.
: PLACE-STEP ( -- )
   -1 PLACE-COUNT +!
   PLACE-COUNT @ PLACE-WORK @ {: e:n :}
   e 0 e NSHADOW:SIZE ['] PLACE-TARGET EACH-TARGET ;


: DRAIN ( -- )
   begin LIVE-COUNT @ PLACE-COUNT @ + 0 > while
      LIVE-COUNT @ 0 > if LIVE-STEP else PLACE-STEP then
   repeat ;


\ Emission e's record whose range [ENTRY@, RANGE-END) holds byte off, or -1.
: RANGE-REC ( n n -- n )
   {: e:n off:n :}
   e EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:ENTRY@ off <=  over RANGE-END off > and if exit then
      REC-NEXT @
   repeat ;


\ A quotation's entry a makes the record holding it live, and NSHADOW:PUBLISH
\ files each emission under a record at offset 0; XT-VALUE refuses any other a.
: QUOTE-ROOT ( n -- )
   {: a:n :}
   a NSHADOW:SOURCE-FUNCTION {: e:n f:n :}
   e 0< if exit then
   e  e f NSHADOW:FUNCTION-OFFSET@  RANGE-REC {: r:n :}
   r 0< if
      s" link: no record holds the function at " type a .
      s" link: a function lies outside every record" 74 die
   then
   r MAKE-LIVE DRAIN ;


\ Host address a the program starts from: a record's or QUOTE-ROOT's entry makes
\ live the code it reaches; a kernel primitive's entry keeps its kernel code.
: ROOT ( n -- )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if r MAKE-LIVE DRAIN exit then
   a UNSAFE@ {: d:n :}
   d 0< if
      a ARM64:ROOT-ENTRY if exit then
      a QUOTE-ROOT exit
   then
   s" link: the program starts at " type d CANNOT-RUN ;


\ An engine ships every routine.
: REACH-ALL ( -- )
   NSHADOW:EMISSIONS 0 ?do 1 i PLACED ! loop ;


\ Each placed emission from A64ICODE:ASM-LEN on, in emission order.
: LAYOUT ( -- )
   NSHADOW:EMISSIONS OFFSETS-RESERVE
   A64ICODE:ASM-LEN
   NSHADOW:EMISSIONS 0 ?do
      i PLACED @ 0= if
         -1 i OFFSETS !
      else
         i NSHADOW:SIZE 3 and 0<> if
            s" link: " type i ROUTINE. s"  is not whole 4-byte words" type cr
            s" link: a routine is not whole 4-byte words" 74 die
         then
         dup i OFFSETS !  i NSHADOW:SIZE +
      then
   loop
   drop ;


\ A host address's link address: a placed product routine's entry, a kernel
\ primitive's, a placed quotation's, or -1. ROUTINES builds the table and
\ PLACE-ROUTINES the offsets.
: LINK-ADDRESS ( n -- n )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if
      r NSHADOW:EMISSION@ {: e:n :}
      e PLACED @ 0= if -1 exit then
      TEXT-VA  e OFFSET@ +  r NSHADOW:ENTRY@ + exit
   then
   a ARM64:KERNEL-ENTRY {: off:n :}
   off 0 >= if TEXT-VA off + exit then
   \ Only a value no record or kernel entry matched scans every emission's
   \ functions: (quotation xt cells) x (functions), here and in QUOTE-ROOT.
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0< if -1 exit then
   qe PLACED @ 0= if -1 exit then
   TEXT-VA  qe OFFSET@ +  qe qf NSHADOW:FUNCTION-OFFSET@ + ;


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


\ Each placed emission, after the kernel ARM64:KERNEL wrote, its data addresses
\ through data.
: PLACE-ROUTINES ( [ n -- n ] -- )
   {: data :}
   LAYOUT
   NSHADOW:EMISSIONS 0 ?do i PLACED @ 0<> if i data PLACE-ONE then loop ;

;package
