\ link-band.f - the shadow emissions a program's roots reach, or every one for
\ an engine. A native link lays them after the kernel in NATIVE's code buffer
\ (LAYOUT) and places them at TEXT-VA plus their offset there. Words visible
\ only inside LINKER.

require src/core/exit-status.f
require src/lib/hashmap.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/dict.f
require src/compiler/native/shadow.f
require src/arch/arm64/icode.f
require src/arch/arm64/link.f
require src/habu/link-kernel.f
require src/arch/wasm/link-kernel.f
require src/arch/wasm/link-site.f
require src/arch/x86-64/link.f
require src/compiler/native/emission.f
require src/habu/link-code.f

package LINKER
private

\ Emission e's byte offset in NATIVE's code buffer, -1 when it is not placed.
DYNAMIC-BUFFER OFFSETS n
: OFFSET@ ( n -- n ) OFFSETS @ ;

\ Emission e's defer trailer: the host dispatch cell it names, 0 for none (LAYOUT).
DYNAMIC-BUFFER TRAILERS n
2 cells constant TRAILER-BYTES

\ Emission e's copy word, whose steps trailer follows its code, -1 for none (LAYOUT).
DYNAMIC-BUFFER STEPPED n

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

\ PLACED per emission and LIVE per record, 1 once marked; MAKE-LIVE pushes
\ each record it marks once.
DYNAMIC-BUFFER PLACED n
DYNAMIC-BUFFER LIVE n
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
: ROUTINE-NAME$ ( n -- str )
   {: e:n :}
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ e =  i NSHADOW:ENTRY@ 0= and if
         i NSHADOW:RECORD@ XREF-REC XREF-NAME$ unloop exit
      then
   loop
   s" " ;


: ROUTINE. ( n -- )
   {: e:n :}
   e ROUTINE-NAME$ {: name:str :}
   name STR:LEN 0<> if name REJECTION:EMIT-STR exit then
   s" emission " REJECTION:EMIT-STR  e REJECTION:EMIT-N ;


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
: UNSAFE-NAME? ( str -- bool )
   {: s:str :}
   s s" create" STR:=CI
   s s" patch32" STR:=CI or
   s s" code-publish" STR:=CI or
   s s" xref-retarget" STR:=CI or
   s s" bare-search-wl" STR:=CI or
   s s" xref-search-wl" STR:=CI or
   s s" bare-scope-find" STR:=CI or
   s s" bare-tok-imm?" STR:=CI or
   s s" policy-admit" STR:=CI or
   s s" policy-seal" STR:=CI or
   s s" def-occ-resolve" STR:=CI or
   s s" prof-on" STR:=CI or
   s s" ndict!" STR:=CI or
   s s" wide-mark" STR:=CI or
   s s" record-facts!" STR:=CI or
   s s" evaluate" STR:=CI or
   s s" evaluate-range" STR:=CI or
   s s" evaluate-closed" STR:=CI or
   s s" literal" STR:=CI or
   s s" compile," STR:=CI or
   s s" ;" STR:=CI or ;


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


\ The tables a link reads, its marks and worklist cleared: once per link,
\ before ROOT or REACH-ALL.
: ROUTINES ( -- )
   ENTRY-TABLE  CHAINS  UNSAFE-TABLE
   NSHADOW:EMISSIONS {: ems:n :}
   NSHADOW:RECORDS {: recs:n :}
   ems PLACED-RESERVE
   recs LIVE-RESERVE  recs LIVE-WORK-RESERVE
   ems 0 ?do 0 i PLACED ! loop
   recs 0 ?do 0 i LIVE ! loop
   0 LIVE-COUNT ! ;


\ The open shadow's machine is Wasm.
: WASM? ( -- bool )
   NSHADOW:BINDING CBIND:TARGET@ CTARGET:ARCH@  CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;



\ The address site at site, a row of kind's, in machine a's emission bytes b, s
\ of them, loads v, or 0 where the machine's writer says it cannot use v.
: ADDRESS! ( n CTARGET:arch ^u8 n n n -- )
   {: v:n a:CTARGET:arch b:^ s:n site:n kind:n :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ if v b s site kind ARM64:ADDRESS! exit then
   a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ if v b s site kind X64:ADDRESS! exit then
   v b s site kind WASM:ADDRESS! ;


\ Emission e's address site k's value.
: SITE-VALUE ( n n -- n )
   NSHADOW:ADDR-SITE-VALUE@ ;


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
            e  e i SITE-VALUE  visit VISIT-OUTSIDE
         then
      then
   loop ;


: MAKE-LIVE ( n -- )
   {: r:n :}
   r LIVE @ 0<> if exit then
   1 r LIVE !
   r LIVE-COUNT @ LIVE-WORK !
   1 LIVE-COUNT +!
   1 r NSHADOW:EMISSION@ PLACED ! ;


\ Live record r, or -1 for the program's start, reaches the UNSAFE-NAME? word
\ at dictionary index d. No record keeps its source, so the record is unplaced.
: CANNOT-RUN ( n n -- )
   {: r:n d:n :}
   d XREF-REC XREF-NAME$ {: name:str :}
   s" E-CANNOT-RUN-STRIPPED" REJECTION:OPEN
   r 0< if
      s" the program starts at `" REJECTION:EMIT-STR
   else
      s" `" REJECTION:EMIT-STR  r NSHADOW:RECORD@ XREF-REC XREF-NAME$ REJECTION:EMIT-STR
      s" ` calls `" REJECTION:EMIT-STR
   then
   name REJECTION:EMIT-STR
   s" `, which a stripped program cannot run" REJECTION:EMIT-STR
   REJECTION:HELP
   s" a stripped program has no dictionary, writable code or interpreter: run `"
   REJECTION:EMIT-STR  name REJECTION:EMIT-STR
   s" ` while the program builds, never from code its entry reaches" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


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
   NATIVE:ROOT-ENTRY ;


\ A target with no record that is no UNSAFE-NAME? word keeps its kernel code;
\ the write step rejects one the kernel does not hold.
: LIVE-TARGET ( n -- )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if r MAKE-LIVE exit then
   a UNSAFE@ {: d:n :}
   d 0< if a KEEP-KERNEL drop exit then
   WALKING @ d CANNOT-RUN ;


: LIVE-STEP ( -- )
   -1 LIVE-COUNT +!
   LIVE-COUNT @ LIVE-WORK @ {: r:n :}
   r WALKING !
   r NSHADOW:EMISSION@  r NSHADOW:ENTRY@  r RANGE-END  ['] LIVE-TARGET EACH-TARGET ;


: DRAIN ( -- )
   begin LIVE-COUNT @ 0 > while LIVE-STEP repeat ;


\ Emission e's record whose range [ENTRY@, RANGE-END) holds byte off, or -1.
: RANGE-REC ( n n -- n )
   {: e:n off:n :}
   e EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:ENTRY@ off <=  over RANGE-END off > and if exit then
      REC-NEXT @
   repeat ;


\ Whether byte off of placed emission e is a live record's: its records' ranges
\ tile it, and a live one's targets and data are kept.
: LIVE-AT? ( n n -- bool ) RANGE-REC LIVE @ 0<> ;


\ A quotation's entry a makes the record holding it live, and NSHADOW:PUBLISH
\ files each emission under a record at offset 0; XT-VALUE rejects any other a.
: QUOTE-ROOT ( n -- )
   {: a:n :}
   a NSHADOW:SOURCE-FUNCTION {: e:n f:n :}
   e 0< if exit then
   e  e f NSHADOW:FUNCTION-OFFSET@  RANGE-REC MAKE-LIVE DRAIN ;


\ Host address a the program starts from: a record's or quotation's entry makes
\ its code live; a kernel primitive keeps its code, except on Wasm.
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
   NSHADOW:RECORDS 0 ?do
      1 i LIVE !  1 i NSHADOW:EMISSION@ PLACED !
   loop ;


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

\ The copy word whose record emission e holds, or -1. A copy word's emission
\ holds its record alone, so its steps follow the emission.
: EM-STEPS ( n -- n )
   {: e:n :}
   e EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:RECORD@ {: r:n :}
      r XREF-REC XREF-FLAGS DNAME-COPY and 0<> if drop r exit then
      REC-NEXT @
   repeat ;

\ Each placed emission from NATIVE:SIZE on, in emission order, a defer's or a
\ copy word's with room for its trailer when the output keeps a dictionary,
\ the only reader of either.
: LAYOUT ( bool -- )
   {: dict:bool :}
   NSHADOW:EMISSIONS OFFSETS-RESERVE  NSHADOW:EMISSIONS TRAILERS-RESERVE
   NSHADOW:EMISSIONS STEPPED-RESERVE
   NATIVE:SIZE  dup BAND-AT !
   NSHADOW:EMISSIONS 0 ?do
      0 i TRAILERS !  -1 i STEPPED !
      i PLACED @ 0= if
         -1 i OFFSETS !
      else
         dup i OFFSETS !  i NSHADOW:SIZE +
         dict if i EM-TRAILER else 0 then {: cell:n :}
         cell i TRAILERS !
         cell 0<> if TRAILER-BYTES + then
         dict if i EM-STEPS else -1 then {: r:n :}
         r i STEPPED !
         r 0 >= if r XREF-REC NDICT:COPY-TRAILER STR:LEN + then
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
      NATIVE:TEXT-VA  e OFFSET@ +  r NSHADOW:ENTRY@ + exit
   then
   a NATIVE:KERNEL-ENTRY {: off:n :}
   off 0 >= if NATIVE:TEXT-VA off + exit then
   \ Only a value no record or kernel entry matched scans every emission's
   \ functions: (quotation xt cells) x (functions), here and in QUOTE-ROOT.
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0< if -1 exit then
   qe PLACED @ 0= if -1 exit then
   NATIVE:TEXT-VA  qe OFFSET@ +  qe qf NSHADOW:FUNCTION-OFFSET@ + ;


\ A placed routine names code the target holds no copy of. Unplaced, as
\ CANNOT-RUN's record is.
: TARGET-ADDRESS ( n -- n )
   {: a:n :}
   a LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   PLACING @ ROUTINE-NAME$ {: name:str :}
   s" E-LINK-FOREIGN-WORD" REJECTION:OPEN
   name STR:LEN 0<> if
      s" `" REJECTION:EMIT-STR  name REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   else
      s" emission " REJECTION:EMIT-STR  PLACING @ REJECTION:EMIT-N
   then
   s"  calls " REJECTION:EMIT-STR  a TARGET-STR
   s" , which is neither a word the build defines nor a primitive of the target's kernel"
   REJECTION:EMIT-STR
   REJECTION:HELP
   s" call only words the build's files define or the target's kernel provides"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


\ Emission e's code, then a defer's trailer, DEFER-MAGIC and its dispatch cell
\ through data, or a copy word's, its host record's bytes.
: PLACE-ONE ( n [ n -- n ] -- )
   {: e:n data :}
   e NSHADOW:SIZE NATIVE:SPACE {: dst:^ :}
   e PLACING !
   e dst NATIVE:TEXT-VA e OFFSET@ + data ['] TARGET-ADDRESS
   ['] LIVE-AT? NATIVE:PLACE
   e TRAILERS @ {: cell:n :}
   cell 0<> if DEFER-MAGIC NATIVE:U64,  cell data execute NATIVE:U64, then
   e STEPPED @ {: r:n :}
   r 0 >= if r XREF-REC NDICT:COPY-TRAILER NATIVE:BYTES, then ;


\ Each placed emission, after the kernel NATIVE:ADOPT laid, its data addresses
\ through data; dict as LAYOUT takes it.
: PLACE-ROUTINES ( bool [ n -- n ] -- )
   {: dict:bool data :}
   dict LAYOUT
   NSHADOW:EMISSIONS 0 ?do i PLACED @ 0<> if i data PLACE-ONE then loop ;

;package
