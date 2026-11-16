\ link-band.f - the shadow emissions a program's roots reach, or every one for
\ an engine, on arm64 or Wasm. An arm64 link lays them after the kernel in
\ A64ICODE's code buffer (LAYOUT) and places them at TEXT-VA plus their offset
\ there. Words visible only inside LINKER.

require src/lib/hashmap.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/dict.f
require src/compiler/native/shadow.f
require src/arch/arm64/icode.f
require src/arch/arm64/link.f
require src/arch/arm64/link-kernel.f
require src/arch/wasm/link-kernel.f
require src/arch/wasm/link-site.f

package LINKER
private

\ Emission e's byte offset in A64ICODE's code buffer, -1 when it is not placed.
DYNAMIC-BUFFER OFFSETS n
: OFFSET@ ( n -- n ) OFFSETS @ ;

\ Emission e's defer trailer: the host dispatch cell it names, 0 for none (LAYOUT).
DYNAMIC-BUFFER TRAILERS n
2 cells constant TRAILER-BYTES

\ The band's first byte, where the kernel ends, and the byte after its last (LAYOUT).
variable BAND-AT
variable BAND-END

\ The host-entry table over HM: a shadow record's host entry to the record.
DYNAMIC-BUFFER ENTRY-KEYS n
DYNAMIC-BUFFER ENTRY-USED n
DYNAMIC-BUFFER ENTRY-RECS n
variable ENTRY-CAP
variable ENTRY-N                     \ the table holds shadow records [0, ENTRY-N)
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


\ The name of the record entering emission e at its first byte, empty for none.
: ROUTINE-NAME$ ( n -- ^u8 n )
   {: e:n :}
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ e =  i NSHADOW:ENTRY@ 0= and if
         i NSHADOW:RECORD@ XREF-REC XREF-NAME$ unloop exit
      then
   loop
   s" " ;


: ROUTINE. ( n -- )
   {: e:n :}
   e ROUTINE-NAME$ {: name:^ nameu:n :}
   nameu 0<> if name nameu SAY exit then
   s" emission " SAY e SAY-INT ;


\ The dictionary record entering at a, or -1.
: ENTRY-REC ( n -- n )
   {: a:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-START a = if i unloop exit then
   loop
   -1 ;

\ The name of the dictionary record entering at a, else a itself, written into
\ the open record.
: TARGET-STR ( n -- )
   {: a:n :}
   a ENTRY-REC {: r:n :}
   r 0 < if s" the code at " REJECTION:EMIT-STR  a REJECTION:EMIT-N exit then
   s" `" REJECTION:EMIT-STR  r XREF-REC XREF-NAME$ REJECTION:EMIT-STR
   s" `" REJECTION:EMIT-STR ;


\ Two records share a host entry only where they enter one emission at one
\ offset; a does> companion enters its emission at its clause. The first filed
\ stands.
: FILE-ENTRY ( n -- )
   {: r:n :}
   r NSHADOW:RECORD@ XREF-REC XREF-START {: key:n :}
   key ENTRY-SLOT {: s:n :}
   s ENTRY-USED @ 0<> if exit then
   1 s ENTRY-USED !  key s ENTRY-KEYS !  r s ENTRY-RECS ! ;


\ An empty table for n records. HM:PROBE needs a power-of-two cap; at least
\ twice the records keeps the table at most half full.
: EMPTY-ENTRIES ( n -- )
   2 * 16 max  1 begin 2dup > while 2 * repeat nip {: cap:n :}
   cap ENTRY-CAP !
   cap ENTRY-KEYS-RESERVE  cap ENTRY-USED-RESERVE  cap ENTRY-RECS-RESERVE
   0 ENTRY-USED cap HM:CLEAR
   0 ENTRY-N ! ;

\ Adds the records published since the last call, re-adding every record into
\ a larger table once they would fill more than half of it.
: ENTRIES ( -- )
   NSHADOW:RECORDS {: recs:n :}
   recs ENTRY-N @ < if E-COMP-STATE throw then
   recs 2 * ENTRY-CAP @ > if recs EMPTY-ENTRIES then
   recs ENTRY-N @ ?do i FILE-ENTRY loop
   recs ENTRY-N ! ;

: ENTRY-TABLE ( -- )
   NSHADOW:RECORDS EMPTY-ENTRIES  ENTRIES ;


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


\ The kernel words a stripped program cannot run: it has no dictionary to read
\ or write, no writable code and no interpreter for the evaluate words,
\ literal, compile, and ;.
: UNSAFE-NAME? ( ^u8 n -- bool )
   {: a:^ u:n :}
   a u s" create" XREF-STR=CI
   a u s" patch32" XREF-STR=CI or
   a u s" code-publish" XREF-STR=CI or
   a u s" callmap-set" XREF-STR=CI or
   a u s" addrmap-set" XREF-STR=CI or
   a u s" xref-retarget" XREF-STR=CI or
   a u s" search-wl" XREF-STR=CI or
   a u s" xref-search-wl" XREF-STR=CI or
   a u s" scope-find" XREF-STR=CI or
   a u s" tok-imm?" XREF-STR=CI or
   a u s" policy-admit" XREF-STR=CI or
   a u s" policy-seal" XREF-STR=CI or
   a u s" def-occ-resolve" XREF-STR=CI or
   a u s" prof-on" XREF-STR=CI or
   a u s" ndict!" XREF-STR=CI or
   a u s" wide-mark" XREF-STR=CI or
   a u s" record-facts!" XREF-STR=CI or
   a u s" evaluate" XREF-STR=CI or
   a u s" evaluate-range" XREF-STR=CI or
   a u s" evaluate-closed" XREF-STR=CI or
   a u s" literal" XREF-STR=CI or
   a u s" compile," XREF-STR=CI or
   a u s" ;" XREF-STR=CI or ;


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


\ The open shadow's machine is Wasm.
: WASM? ( -- bool )
   NSHADOW:BINDING CBIND:TARGET@ CTARGET:ARCH@  CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;


\ The machines a part of the linker places, which NO-PART's text names.
: PLACES? ( CTARGET:arch -- bool ) {: a:CTARGET:arch :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ  a CTARGET-ARCH:WASM CTARGET-ARCH:EQ  or ;


\ A build for the target t tu names, whose machine the linker has no part for, such
\ as an x86-64 target. The linker holds no source place, so the record is unplaced.
: NO-PART ( ^u8 n -- ) {: t:^ tu:n :}
   s" E-LINK-MACHINE" REJECTION:OPEN
   s" the build's code is for a machine the linker cannot place: it places arm64 and Wasm code"
   REJECTION:EMIT-STR
   s" found" REJECTION:NOTE  t tu REJECTION:EMIT-STR
   REJECTION:HELP
   s" build for macos-aarch64, linux-aarch64 or wasm" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;


\ The address value machine a's emission bytes b, s of them, hold at site for a row of
\ kind, and whether they hold one there, read by a's part of LINKER.
: ADDRESS@ ( CTARGET:arch ^u8 n n n -- n bool )
   {: a:CTARGET:arch b:^ s:n site:n kind:n :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ if
      b s site kind ARM64:ADDRESS@
   else
      b s site kind WASM:ADDRESS@
   then ;


\ v written as ADDRESS@ reads it, by a's part of LINKER; false, with nothing written,
\ where ADDRESS@ answers false.
: ADDRESS! ( n CTARGET:arch ^u8 n n n -- bool )
   {: v:n a:CTARGET:arch b:^ s:n site:n kind:n :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ if
      v b s site kind ARM64:ADDRESS!
   else
      v b s site kind WASM:ADDRESS!
   then ;


\ Emission e's address site k's value and whether it holds one, read as the emission's
\ machine encodes it.
: SITE-VALUE ( n n -- n bool )
   {: e:n k:n :}
   e NSHADOW:ARCH@  e NSHADOW:BYTES  e NSHADOW:SIZE  e k NSHADOW:ADDR-SITE@
   e k NSHADOW:ADDR-SITE-KIND@  ADDRESS@ ;


\ Emission e's function at offset v, or -1: a site of a Wasm emission naming that
\ emission holds the offset, an arm64 site a host address.
: OWN-FUNCTION ( n n -- n )
   {: e:n v:n :}
   e NSHADOW:ARCH@ CTARGET-ARCH:WASM CTARGET-ARCH:EQ 0= if -1 exit then
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ v = if i unloop exit then
   loop
   -1 ;


: VISIT-OUTSIDE ( n n [ n -- ] -- )
   {: e:n v:n visit :}
   e v OWN-FUNCTION 0 >= if exit then
   v visit execute ;


\ The host address each call and code address site of emission e at an offset
\ in [from, to) names outside e, handed to visit; data addresses name none.
: EACH-TARGET ( n n n [ n -- ] -- )
   {: e:n from:n to:n visit :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      s from >=  s to < and if e  e i NSHADOW:CALL-TARGET@  visit VISIT-OUTSIDE then
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-CODE = if
         e i NSHADOW:ADDR-SITE@ {: at:n :}
         at from >=  at to < and if
            e i SITE-VALUE {: v:n has:bool :}
            has if e v visit VISIT-OUTSIDE then
         then
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


\ Live record r, or -1 for the program's start, reaches the UNSAFE-NAME? word
\ at dictionary index d. No record keeps its source, so the record is unplaced.
: CANNOT-RUN ( n n -- )
   {: r:n d:n :}
   d XREF-REC XREF-NAME$ {: name:^ nameu:n :}
   s" E-CANNOT-RUN-STRIPPED" REJECTION:OPEN
   r 0< if
      s" the program starts at `" REJECTION:EMIT-STR
   else
      s" `" REJECTION:EMIT-STR  r NSHADOW:RECORD@ XREF-REC XREF-NAME$ REJECTION:EMIT-STR
      s" ` calls `" REJECTION:EMIT-STR
   then
   name nameu REJECTION:EMIT-STR
   s" `, which a stripped program cannot run" REJECTION:EMIT-STR
   REJECTION:HELP
   s" a stripped program has no dictionary, writable code or interpreter: run `"
   REJECTION:EMIT-STR  name nameu REJECTION:EMIT-STR
   s" ` while the program builds, never from code its entry reaches" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;


\ The shadow record host address a names, or -1. A Wasm module has no hb
\ kernel: a kernel word a kernel-words.f word provides names that word's record.
: TARGET-REC ( n -- n )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if r exit then
   WASM? 0= if -1 exit then
   a WASM:PROVIDER {: p:n :}
   p 0= if -1 exit then
   p REC-AT ;


\ Keeps the kernel code entering at host address a, true when a is a kernel
\ primitive's entry. A Wasm module has no hb kernel and keeps none.
: KEEP-KERNEL ( n -- bool )
   WASM? if drop false exit then
   ARM64:ROOT-ENTRY ;


\ A target with no record and no UNSAFE-NAME? word is a kernel primitive or a
\ foreign word the write step refuses.
: LIVE-TARGET ( n -- )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if r MAKE-LIVE exit then
   a UNSAFE@ {: d:n :}
   d 0< if exit then
   WALKING @ d CANNOT-RUN ;


\ A placed emission's target places the emission holding its record, else
\ KEEP-KERNEL keeps its kernel code.
: PLACE-TARGET ( n -- )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if r NSHADOW:EMISSION@ PLACE-EM exit then
   a KEEP-KERNEL drop ;


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
   e  e f NSHADOW:FUNCTION-OFFSET@  RANGE-REC MAKE-LIVE DRAIN ;


\ Host address a the program starts from: a record's or quotation's entry makes
\ its code live; an arm64 kernel primitive keeps its code.
: ROOT ( n -- )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if r MAKE-LIVE DRAIN exit then
   a UNSAFE@ {: d:n :}
   d 0< if
      a KEEP-KERNEL if exit then
      a QUOTE-ROOT exit
   then
   -1 d CANNOT-RUN ;


\ An engine ships every routine.
: REACH-ALL ( -- )
   NSHADOW:RECORDS 0 ?do 1 i NSHADOW:EMISSION@ PLACED ! loop ;


\ The dispatch cell of the defer whose record emission e holds, or 0. A
\ defer's emission holds its record alone, so its trailer follows the emission.
: EM-TRAILER ( n -- n )
   {: e:n :}
   e EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:RECORD@ XREF-REC {: rec:^ :}
      rec NDICT:TRAILER-CELL {: cell:n :}
      cell 0<> if drop cell exit then
      REC-NEXT @
   repeat
   drop 0 ;

\ Each placed emission from A64ICODE:ASM-LEN on, in emission order, a defer's
\ with room for its trailer when the output keeps a dictionary. An arm64
\ emission is whole instruction words, so every offset stays word-aligned.
: LAYOUT ( bool -- )
   {: dict:bool :}
   NSHADOW:EMISSIONS OFFSETS-RESERVE  NSHADOW:EMISSIONS TRAILERS-RESERVE
   A64ICODE:ASM-LEN  dup BAND-AT !
   NSHADOW:EMISSIONS 0 ?do
      0 i TRAILERS !
      i PLACED @ 0= if
         -1 i OFFSETS !
      else
         dup i OFFSETS !  i NSHADOW:SIZE +
         dict if i EM-TRAILER else 0 then {: cell:n :}
         cell i TRAILERS !
         cell 0<> if TRAILER-BYTES + then
      then
   loop
   BAND-END ! ;


\ A host address's link address: a placed target routine's entry, a kernel
\ primitive's, a placed quotation's, or -1. ROUTINES builds the table and
\ PLACE-ROUTINES the offsets.
: LINK-ADDRESS ( n -- n )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if
      r NSHADOW:EMISSION@ {: e:n :}
      e PLACED @ 0= if -1 exit then
      ARM64:TEXT-VA  e OFFSET@ +  r NSHADOW:ENTRY@ + exit
   then
   a ARM64:KERNEL-ENTRY {: off:n :}
   off 0 >= if ARM64:TEXT-VA off + exit then
   \ Only a value no record or kernel entry matched scans every emission's
   \ functions: (quotation xt cells) x (functions), here and in QUOTE-ROOT.
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0< if -1 exit then
   qe PLACED @ 0= if -1 exit then
   ARM64:TEXT-VA  qe OFFSET@ +  qe qf NSHADOW:FUNCTION-OFFSET@ + ;


\ A placed routine names code the target holds no copy of. Unplaced, as
\ CANNOT-RUN's record is.
: TARGET-ADDRESS ( n -- n )
   {: a:n :}
   a LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   PLACING @ ROUTINE-NAME$ {: name:^ nameu:n :}
   s" E-LINK-FOREIGN-WORD" REJECTION:OPEN
   nameu 0<> if
      s" `" REJECTION:EMIT-STR  name nameu REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   else
      s" emission " REJECTION:EMIT-STR  PLACING @ REJECTION:EMIT-N
   then
   s"  calls " REJECTION:EMIT-STR  a TARGET-STR
   s" , which is neither a word the build defines nor a primitive of the target's kernel"
   REJECTION:EMIT-STR
   REJECTION:HELP
   s" call only words the build's files define or the target's kernel provides"
   REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;


\ Emission e's code, then a defer's trailer: DEFER-MAGIC and its dispatch cell
\ through data.
: PLACE-ONE ( n [ n -- n ] -- )
   {: e:n data :}
   e NSHADOW:SIZE 4 / {: words:n :}
   words A64ICODE:ASM-CP?
   e PLACING !
   e  A64ICODE:CODE e OFFSET@ +  ARM64:TEXT-VA e OFFSET@ +  data  ['] TARGET-ADDRESS
   ARM64:PLACE
   words A64ICODE:ASM-CP +!
   e TRAILERS @ {: cell:n :}
   cell 0<> if DEFER-MAGIC A64ICODE:DCQ,  cell data execute A64ICODE:DCQ, then ;


\ Each placed emission, after the kernel ARM64:ADOPT laid, its data addresses
\ through data; dict as LAYOUT takes it.
: PLACE-ROUTINES ( bool [ n -- n ] -- )
   {: dict:bool data :}
   dict LAYOUT
   NSHADOW:EMISSIONS 0 ?do i PLACED @ 0<> if i data PLACE-ONE then loop ;

;package
