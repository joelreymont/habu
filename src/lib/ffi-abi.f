\ ffi-abi.f - checked AAPCS64 and SysV FFI calls, package FFI. AAPCS64 passes
\ eight integer and eight float registers, SysV six and eight. FUNCTION: is the
\ only checked call surface: its word names a row, never an address.
\ STORAGE CLASS. TASK-LOCAL: marshalling buffers, kernel params and the
\ DLSYM loader block; calls nest only through a C callback, which shares
\ them. PROCESS-WIDE: rows, whose addresses, handles and failures are scratch.
\
\ FUNCTION: names a row here and its generated word carries the row's index, so
\ a checked caller reaches only a symbol some declaration published. Exact
\ declarations share one row. A row resolves its symbol on the first call and
\ caches the address. Library 0 is the process (RTLD_DEFAULT, no reference);
\ any other index is a path dlopened once, at the first call; one that does not
\ open passes the call to the next library its row names. Rows may move when a
\ table grows; indices stay stable.
\
\ A declaration with a C struct is planned by AAPCS64 over its C arguments in
\ order; the STRUCT-RESULT destination, the last input, is not one. Floats take
\ v0-v7. An integer argument takes the next of x0-x7, else the next stack
\ cell. A record of up to 16 bytes takes its registers whole, the last one's
\ unused high bytes zero, or when too few remain its stack cells whole, and the
\ next register becomes x8. A larger record is passed as a pointer, an integer
\ argument, to a private copy in the native call frame that lives exactly as
\ long as the call; the copy follows every stack argument from an even cell,
\ 16-byte aligned on the 16-byte aligned native stack. A planned register is
\ kept as its number and a stack cell as -1-cell; a record of 16 bytes or fewer
\ has no copy, kept as 0.

HB-TARGET-MACOS? HB-TARGET-LINUX-KERNEL? or ?TARGET
s" src/lib/error.f" required
s" src/core/rollback-cells.f" required
s" src/lib/span.f" required
s" src/lib/string.f" required
s" src/lib/le.f" required
s" src/lib/float32.f" required
s" src/lib/adt/option.f" required
s" src/lib/adt/nullable.f" required
s" src/lib/codegen.f" required
s" src/lib/memory.f" required

package FFI

8 constant FFI-REG-ARGS
16 constant FFI-MAX-ARGS
16 constant FFI-MAX-KPARAMS
2 constant FFI-RTLD-NOW
$0A constant LF-C

$3A00 constant FFI-BUF-OFF
$3A80 constant FFI-FBUF-OFF
$3AC0 constant FFI-STACK-BUF-OFF
$3B40 constant FFI-KPARAM-PBUF-OFF
$3BC0 constant FFI-KPARAM-VBUF-OFF
$3C40 constant FFI-DLBUF-OFF
$3C80 constant FFI-KPARAM#-OFF

: FFI-BUF ( -- ptr n )
   data-base FFI-BUF-OFF + ;

: FFI-FBUF ( -- ptr r )
   data-base FFI-FBUF-OFF + ;

\ A C float occupies the low half of the same FP-register slot as a double.
: FFI-F32BUF ( -- ptr n )
   data-base FFI-FBUF-OFF + ;

\ A spill slot holds an integer or a float as the marshalled signature says, so
\ FFI-STACK-BUF and FFI-STACK-FBUF declare one offset at the two pointees rather
\ than one `( -- ptr a )` that would let a caller pick engine DATA's type.
: FFI-STACK-BUF ( -- ptr n )
   data-base FFI-STACK-BUF-OFF + ;

: FFI-STACK-FBUF ( -- ptr r )
   data-base FFI-STACK-BUF-OFF + ;

: FFI-KPARAM-PBUF ( -- ptr n )
   data-base FFI-KPARAM-PBUF-OFF + ;

: FFI-KPARAM-VBUF ( -- ptr n )
   data-base FFI-KPARAM-VBUF-OFF + ;

: FFI-DLBUF ( -- ptr n )
   data-base FFI-DLBUF-OFF + ;

: FFI-KPARAM# ( -- ptr n )
   data-base FFI-KPARAM#-OFF + ;

\ CELL>PTR stays private: a foreign address carries no length, so only a word
\ that knows the callee's width may publish one.
: PTR>CELL ( ptr a -- n ) NULL-PTR - ;
CAST: CELL>PTR ( n -- ptr u8 )

\ ---- argument marshalling -------------------------------------------------
\ Integer slot 8 is x8, the AAPCS64 indirect-result register. Stack slots are
\ copied to the C stack exactly as prepacked by the caller on either ABI.
: FFI-CHECK-INDEX ( n n -- ) {: idx:n cap:n :}
   idx 0 < if E-FFI-ARITY throw then
   idx cap >= if E-FFI-ARITY throw then ;
: FFI-CHECK-COUNT ( n n -- ) {: count:n cap:n :}
   count 0 < if E-FFI-ARITY throw then
   count cap > if E-FFI-ARITY throw then ;
: FFI-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-BUF idx cells + ;
: FFI-FSLOT ( n -- ptr r ) {: idx:n :}
   idx FFI-REG-ARGS FFI-CHECK-INDEX
   FFI-FBUF idx cells + ;
: FFI-F32SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-REG-ARGS FFI-CHECK-INDEX
   FFI-F32BUF idx cells + ;
: FFI-STACK-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-BUF idx cells + ;
: FFI-STACK-FSLOT ( n -- ptr r ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-FBUF idx cells + ;
: FFI-ARG! ( n n -- ) {: v:n idx:n :}
   v idx FFI-SLOT ! ;
: FFI-PTR-ARG! ( ptr a n -- ) {: p:ptr idx:n :}
   p PTR>CELL idx FFI-ARG! ;
: FFI-FARG! ( r n -- ) {: v:r idx:n :}
   v idx FFI-FSLOT ! ;
: FFI-F32ARG! ( n n -- ) {: bits:n idx:n :}
   bits idx FFI-F32SLOT ! ;
: FFI-STACK! ( n n -- ) {: v:n idx:n :}
   v idx FFI-STACK-SLOT ! ;
: FFI-FSTACK! ( r n -- ) {: v:r idx:n :}
   v idx FFI-STACK-FSLOT ! ;
: FFI-X8! ( n -- )
   8 FFI-ARG! ;

\ ---- out-params -----------------------------------------------------------
: FFI-OUT@ ( ptr n -- n )
   @ ;
: FFI-OUT! ( n ptr n -- )
   ! ;

\ ---- kernelParams ---------------------------------------------------------
\ `cuLaunchKernel` wants void**: each array element points to caller-owned or
\ FFI-owned param storage. FFI-owned value cells are stable until the next
\ FFI-KPARAM-RESET or overwrite by another FFI-KPARAM-N+ sequence.
: FFI-KPARAM-COUNT ( -- n )
   FFI-KPARAM# @ ;
: FFI-KPARAM-RESET ( -- )
   0 FFI-KPARAM# ! ;
: FFI-KPARAM-CHECK ( n -- ) {: idx:n :}
   idx FFI-MAX-KPARAMS FFI-CHECK-INDEX ;
: FFI-KPARAM-PTR-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-KPARAM-CHECK
   FFI-KPARAM-PBUF idx cells + ;
: FFI-KPARAM-VAL-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-KPARAM-CHECK
   FFI-KPARAM-VBUF idx cells + ;
: FFI-KPARAM-BUMP ( n -- )
   1 + FFI-KPARAM# ! ;
: FFI-KPARAM+ ( ptr a -- ) {: p:ptr :}
   FFI-KPARAM-COUNT {: idx:n :}
   p PTR>CELL idx FFI-KPARAM-PTR-SLOT !
   idx FFI-KPARAM-BUMP ;
: FFI-KPARAM-N+ ( n -- ) {: v:n :}
   FFI-KPARAM-COUNT {: idx:n :}
   v idx FFI-KPARAM-VAL-SLOT !
   idx FFI-KPARAM-VAL-SLOT FFI-KPARAM+ ;
: FFI-KPARAMS ( -- ptr n n )
   FFI-KPARAM-PBUF FFI-KPARAM-COUNT ;
: FFI-KPARAMS>N ( -- n )
   FFI-KPARAM-PBUF PTR>CELL ;

: DLOPEN-RAW ( ptr u8 n -- n ) {: path:ptr flags:n :}
   path PTR>CELL FFI-DLBUF !
   flags FFI-DLBUF CELL + !
   FFI-DLBUF 2 dlopen-entry ffi-call-n ;

: DLSYM-RAW ( n ptr u8 -- n ) {: handle:n name:ptr :}
   handle FFI-DLBUF !
   name PTR>CELL FFI-DLBUF CELL + !
   FFI-DLBUF 2 dlsym-entry ffi-call-n ;

\ ---- declared foreign functions -------------------------------------------

$30 constant FN-NAME-CAP
PATH-CAP 1 + constant LIB-PATH-CAP
$20 constant LIB-BASE-CAP
9999 constant LIB-VERSION-MAX

0 cells constant FN-ADDR-OFF
1 cells constant FN-LIB-OFF
2 cells constant FN-ARGC-OFF
3 cells constant FN-SPILL-OFF
4 cells constant FN-RELOC-OFF
5 cells constant FN-NAME-OFF
FN-NAME-OFF FN-NAME-CAP + constant FN-ROW-BYTES
0 cells constant LIB-LEN-OFF
1 cells constant LIB-HANDLE-OFF
2 cells constant LIB-FAILED-OFF
3 cells constant LIB-NEXT-OFF             \ the library index tried when this path does not open, or 0
4 cells constant LIB-PATH-OFF
LIB-PATH-OFF LIB-PATH-CAP + 7 + 8 / 8 * constant LIB-ROW-BYTES
PERSISTED-PTR-VARIABLE FN-TABLE
FN-TABLE ROLLBACK-CELLS:MARK
PERSISTED-PTR-VARIABLE LIB-TABLE
LIB-TABLE ROLLBACK-CELLS:MARK
variable FN-CAP
FN-CAP ROLLBACK-CELLS:MARK
variable LIB-CAP
LIB-CAP ROLLBACK-CELLS:MARK
variable FN-N
FN-N ROLLBACK-CELLS:MARK
variable LIB-N
LIB-N ROLLBACK-CELLS:MARK

: ALIGNED-ROWS ( n -- ptr u8 )
   {: size:n :}
   here data-base - negate 7 and allot
   here BYTE-VIEW {: rows:ptr :}
   size allot
   rows ;

\ An old block stays allotted: a callback the loader runs may grow the table
\ while the loader still reads a row's name or path there.
: FN-GROW ( -- )
   FN-CAP @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   next FN-ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   next 0 ?do rows i FN-ROW-BYTES * + FN-ADDR-OFF + scratch-cell-mark loop
   oldcap 0 > if
      FN-TABLE @ {: old:ptr :}
      old rows FN-N @ FN-ROW-BYTES * BYTE-COPY
      FN-N @ 0 ?do 0 old i FN-ROW-BYTES * + FN-ADDR-OFF + CELL-VIEW ! loop
   then
   rows FN-TABLE !
   next FN-CAP ! ;

: LIB-GROW ( -- )
   LIB-CAP @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   next LIB-ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   next 0 ?do
      rows i LIB-ROW-BYTES * + LIB-HANDLE-OFF + scratch-cell-mark
      rows i LIB-ROW-BYTES * + LIB-FAILED-OFF + scratch-cell-mark
   loop
   oldcap 0 > if
      LIB-TABLE @ {: old:ptr :}
      old rows LIB-N @ LIB-ROW-BYTES * BYTE-COPY
      LIB-N @ 0 ?do
         old i LIB-ROW-BYTES * + {: row:ptr :}
         0 row LIB-HANDLE-OFF + CELL-VIEW !
         0 row LIB-FAILED-OFF + CELL-VIEW !
      loop
   then
   rows LIB-TABLE !
   next LIB-CAP ! ;

: FN-NEW ( -- )
   FN-N @ FN-CAP @ >= if FN-GROW then ;

: LIB-NEW ( -- )
   LIB-N @ LIB-CAP @ >= if LIB-GROW then ;

: FN-ROW ( n -- ptr u8 )
   {: idx:n :}
   FN-TABLE @ idx FN-ROW-BYTES * + ;

: LIB-ROW ( n -- ptr u8 )
   {: idx:n :}
   LIB-TABLE @ idx LIB-ROW-BYTES * + ;

: FN-FIELD ( n n -- ptr n )
   {: idx:n off:n :}
   idx FN-ROW off + CELL-VIEW ;

: LIB-FIELD ( n n -- ptr n )
   {: idx:n off:n :}
   idx LIB-ROW off + CELL-VIEW ;

\ A new row's string field, read before the row is counted: the name or path is
\ copied first, so a refused one leaves the row count as it was.
: NAME-FIELD ( n -- SPAN:span<u8> )
   FN-ROW FN-NAME-OFF + FN-NAME-CAP SPAN:MAKE ;

: PATH-FIELD ( n -- SPAN:span<u8> )
   LIB-ROW LIB-PATH-OFF + LIB-PATH-CAP SPAN:MAKE ;

: FN-CHECK ( n -- )
   {: idx:n :}
   idx FN-N @ FFI-CHECK-INDEX ;

: FN-NAME ( n -- ptr u8 )
   {: idx:n :}
   idx FN-CHECK
   idx FN-ROW FN-NAME-OFF + ;

: NAME-LEN ( n -- n )
   {: idx:n :}
   idx FN-NAME {: name:ptr :}
   0 begin dup FN-NAME-CAP < over name + c@ 0<> and while 1+ repeat ;

: FN-ADDR@ ( n -- n )
   {: idx:n :}
   idx FN-CHECK
   idx FN-ADDR-OFF FN-FIELD @ ;

: FN-ADDR! ( n n -- )
   {: value:n idx:n :}
   idx FN-CHECK
   value idx FN-ADDR-OFF FN-FIELD ! ;

: FN-LIB@ ( n -- n )
   {: idx:n :}
   idx FN-CHECK
   idx FN-LIB-OFF FN-FIELD @ ;

: FN-SPILL@ ( n -- n )
   {: idx:n :}
   idx FN-CHECK
   idx FN-SPILL-OFF FN-FIELD @ ;

: FN-ARGC@ ( n -- n )
   {: idx:n :}
   idx FN-CHECK
   idx FN-ARGC-OFF FN-FIELD @ ;

: FN-RELOC@ ( n -- n )
   {: idx:n :}
   idx FN-CHECK
   idx FN-RELOC-OFF FN-FIELD @ ;

: LIB-CHECK ( n -- )
   {: idx:n :}
   idx LIB-N @ FFI-CHECK-INDEX ;

: LIB-PATH ( n -- ptr u8 )
   {: idx:n :}
   idx LIB-CHECK
   idx LIB-ROW LIB-PATH-OFF + ;

: LIB-LEN@ ( n -- n )
   {: idx:n :}
   idx LIB-CHECK
   idx LIB-LEN-OFF LIB-FIELD @ ;

: LIB-LEN! ( n n -- )
   {: u:n idx:n :}
   idx LIB-CHECK
   u idx LIB-LEN-OFF LIB-FIELD ! ;

: LIB-HANDLE@ ( n -- n )
   {: idx:n :}
   idx LIB-CHECK
   idx LIB-HANDLE-OFF LIB-FIELD @ ;

: LIB-HANDLE! ( n n -- )
   {: value:n idx:n :}
   idx LIB-CHECK
   value idx LIB-HANDLE-OFF LIB-FIELD ! ;

: LIB-FAILED? ( n -- bool )
   {: idx:n :}
   idx LIB-CHECK
   idx LIB-FAILED-OFF LIB-FIELD @ 0 <> ;

: LIB-FAILED! ( n n -- )
   {: value:n idx:n :}
   idx LIB-CHECK
   value idx LIB-FAILED-OFF LIB-FIELD ! ;

: LIB-NEXT@ ( n -- n )
   {: idx:n :}
   idx LIB-CHECK
   idx LIB-NEXT-OFF LIB-FIELD @ ;

: LIB-NEXT! ( n n -- )
   {: value:n idx:n :}
   idx LIB-CHECK
   value idx LIB-NEXT-OFF LIB-FIELD ! ;

public
\ The handle FFI:DLSYM takes for the process's own symbols.
: PROCESS-HANDLE ( -- n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if 0 exit then
   HB-TARGET-MACOS? if -2 exit then
   E-FFI-LIBRARY throw ;

private
\ Library index idx's path, as LIBRARY-PATH and LIB-INDEX number it (from 1).
: LIB-ROW@ ( n -- ptr u8 n ) 1- {: row:n :} row LIB-PATH row LIB-LEN@ ;

: LINE-BODY ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 NUM:alloc-byte-len -- )
   {: p:ptr pu:n a:ptr au:n s:ptr su:n b:ptr bu:n line:ptr extent:NUM:alloc-byte-len :}
   p line pu BYTE-COPY
   a line pu + au BYTE-COPY
   s line pu + au + su BYTE-COPY
   b line pu + au + su + bu BYTE-COPY
   pu au + su + bu + {: len:n :}
   LF-C line len + c!
   line len 1+ ERROR-TEXT:TYPE ;

\ The four parts and a line feed go out in one write, so lines from concurrent
\ tasks never interleave.
: REPORT-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n a:ptr au:n s:ptr su:n b:ptr bu:n :}
   p pu a au s su b bu
   pu au + su + bu + 1+ MEM:BYTES-ALLOC-LEN [: LINE-BODY ;] MEM:WITH-BYTES ;

\ why, the loader's reason, goes on tried's line only: a later loader call
\ replaces it.
: REPORT-NAMES ( n n n -- )
   {: why:n tried:n lib:n :}
   lib 0= if exit then
   s" E-FFI-DLOPEN: " lib LIB-ROW@
   why 0 <> lib tried = and if s" : " why CELL>PTR dup ZLEN else s" " s" " then
   REPORT-LINE
   why tried lib 1 - LIB-NEXT@ RECURSE ;

\ dlerror's NULL means the loader kept no reason.
: OPEN-REPORT ( n n n -- )
   {: dlerr:n tried:n lib:n :}
   FFI-BUF 0 dlerr ffi-call-n
   tried lib REPORT-NAMES ;

\ The first handle from lib along next and the library that holds it, or 0 and
\ tried: the last library this walk failed to open, else tried as given. A
\ failed row is skipped without the loader, and next always names an earlier
\ row, so the walk ends.
: OPEN-WALK ( n n -- n n )
   {: tried:n lib:n :}
   lib 0= if tried 0 exit then
   lib 1 - {: slot:n :}
   slot LIB-HANDLE@ dup 0 <> if lib swap exit then
   drop
   slot LIB-FAILED? if tried slot LIB-NEXT@ RECURSE exit then
   slot LIB-PATH FFI-RTLD-NOW DLOPEN-RAW
   dup 0 <> if dup slot LIB-HANDLE! lib swap exit then
   drop
   1 slot LIB-FAILED!
   lib slot LIB-NEXT@ RECURSE ;

\ The handle for lib and the library that holds it. dlerr is dlerror's address,
\ resolved before the walk so that no loader call runs between the last failed
\ open and OPEN-REPORT's read of its reason. A walk that opened nothing reports
\ only when it failed an open of its own.
: LIB-RESOLVE ( n n -- n n )
   {: dlerr:n lib:n :}
   lib 0= if PROCESS-HANDLE 0 exit then
   0 lib OPEN-WALK {: tried:n handle:n :}
   handle 0 <> if handle tried exit then
   tried 0 <> if dlerr tried lib OPEN-REPORT then
   E-FFI-DLOPEN throw ;

: SYMBOL-REPORT ( n n -- ) {: idx:n lib:n :}
   s" E-FFI-DLSYM: " idx FN-NAME idx NAME-LEN
   lib 0 <> if s"  in " lib LIB-ROW@ else s" " s" " then
   REPORT-LINE ;

variable ERRNO-FN-CELL
variable DLERROR-FN-CELL

: ERRNO-FN ( -- n ) ERRNO-FN-CELL @ ;
: DLERROR-FN ( -- n ) DLERROR-FN-CELL @ ;

\ The errno accessor resolves first because the loader may change errno even
\ when it succeeds, and nothing may run it between a call and its errno read.
: FN-LOOKUP ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   idx ERRNO-FN <> if ERRNO-FN RECURSE drop then
   idx FN-LIB@ {: lib:n :}
   lib 0 <> if DLERROR-FN RECURSE else 0 then lib LIB-RESOLVE {: handle:n held:n :}
   handle idx FN-NAME DLSYM-RAW
   dup 0= if idx held SYMBOL-REPORT E-FFI-DLSYM throw then
   dup idx FN-ADDR! ;

\ A rejection of the declaration is SYNTAX-OPEN, its message, then SYNTAX-HELP
\ with the fix; it places the record at the token read last.
: SYNTAX-OPEN ( -- )
   s" E-FFI-SYNTAX" REJECTION:OPEN ;

\ The throw keeps the code a catch reads; uncaught, the load exits 70.
: SYNTAX-HELP ( ptr u8 n -- ) {: h:ptr hu:n :}
   CHECK:DEFINER-PLACE
   REJECTION:HELP  h hu REJECTION:EMIT-STR
   REJECTION:KEEP  E-FFI-SYNTAX CHECKER-REFUSE ;

: SYNTAX-REJECT ( ptr u8 n ptr u8 n -- )
   {: m:ptr mu:n h:ptr hu:n :}
   SYNTAX-OPEN  m mu REJECTION:EMIT-STR  h hu SYNTAX-HELP ;

: NAME-ROOM ( n -- )
   {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u FN-NAME-CAP 1 - > if E-FFI-SYNTAX throw then ;

: PATH-ROOM ( n -- )
   {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u LIB-PATH-CAP 1 - > if E-FFI-SYNTAX throw then ;

: BASE-CHECK ( n -- ) {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u LIB-BASE-CAP > if E-FFI-SYNTAX throw then ;

: VERSION-CHECK ( n -- ) {: version:n :}
   version 0 < if E-FFI-SYNTAX throw then
   version LIB-VERSION-MAX > if E-FFI-SYNTAX throw then ;

public

: BUF-OFF ( -- n ) FFI-BUF-OFF ;
: KPARAM-END-OFF ( -- n ) FFI-KPARAM#-OFF CELL + ;
: NOW ( -- n ) FFI-RTLD-NOW ;

\ FFI:NOW alone is RTLD_LOCAL on Linux, so PROCESS-SYMBOLS cannot see the
\ library; `FFI:NOW FFI:GLOBAL or` joins its symbols to RTLD_DEFAULT. macOS
\ opens global either way. <dlfcn.h> spells RTLD_GLOBAL 0x100 in glibc, 0x8 on
\ macOS.
: GLOBAL ( -- n ) HB-TARGET-MACOS? if $8 else $100 then ;

: >CELL ( ptr a -- n ) PTR>CELL ;

: VALUE! ( n n -- ) FFI-ARG! ;
: POINTER! ( ptr a n -- ) FFI-PTR-ARG! ;

: FLOAT! ( r n -- ) FFI-FARG! ;
: FLOAT32! ( r n -- ) {: value:r idx:n :}
   value F32:NARROW idx FFI-F32ARG! ;

: STACK-VALUE! ( n n -- ) FFI-STACK! ;
: STACK-FLOAT! ( r n -- ) FFI-FSTACK! ;
: STACK-POINTER! ( ptr a n -- ) {: value:ptr idx:n :}
   value PTR>CELL idx FFI-STACK! ;

: ARGS ( -- ptr n ) FFI-BUF ;
: FLOATS ( -- ptr r ) FFI-FBUF ;
: STACK ( -- ptr n ) FFI-STACK-BUF ;

: OUT@ ( ptr n -- n ) FFI-OUT@ ;
: OUT! ( n ptr n -- ) FFI-OUT! ;
: KPARAM-COUNT ( -- n ) FFI-KPARAM-COUNT ;
: KPARAM-RESET ( -- ) FFI-KPARAM-RESET ;
: KPARAM+ ( ptr a -- ) FFI-KPARAM+ ;
: KPARAM-VALUE+ ( n -- ) FFI-KPARAM-N+ ;
: KPARAMS ( -- ptr n n ) FFI-KPARAMS ;
: KPARAMS>CELL ( -- n ) FFI-KPARAMS>N ;

\ FFI is the only library code that writes a terminator.
private
\ A 0 byte among the u bytes at b would end the C string early.
: NUL-CHECK ( ptr u8 n -- )
   {: b u :}
   u 0 ?do b i + c@ 0= if E-FFI-NUL throw then loop ;

\ The span's base once u bytes and their 0 fit in it: u < reach, which cannot
\ overflow as u+1 can.
: CSTR-FIT ( n SPAN:span<u8> -- ptr u8 )
   {: u s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ {: b l :}
   u l >= if E-SPAN-CAPACITY throw then
   b ;

public
: CSTR-ROOM ( n -- n ) 1+ ;

: CSTR! ( n SPAN:span<u8> -- ptr u8 )
   {: u s :}
   u s CSTR-FIT {: b :}
   b u NUL-CHECK
   0 b u + c!
   b ;

\ Every refusal comes before a byte is written, so a refused destination keeps
\ what it held.
: CSTR ( ptr u8 n SPAN:span<u8> -- ptr u8 )
   {: src u s :}
   u s CSTR-FIT {: b :}
   src u NUL-CHECK
   src u s SPAN:COPY
   0 b u + c!
   b ;

private

: ERRNO-SYMBOL$ ( -- ptr u8 n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if s" __errno_location" exit then
   HB-TARGET-MACOS? if s" __error" exit then
   E-FFI-LIBRARY throw ;

public

: DLOPEN ( ptr u8 n -- n ) DLOPEN-RAW ;
: DLSYM ( n ptr u8 -- n ) DLSYM-RAW ;

\ The C entry of engine callback stub n: an immutable code address, never a
\ Habu xt.
: CALLBACK-ENTRY ( n -- n ) callback-entry ;

\ ---- the declared-function surface ----------------------------------------
\ The declarer below is DECLARE-ROW's only intended caller.
: PROCESS ( -- n ) 0 ;

: LIBRARY-PATH-CAP ( -- n ) LIB-PATH-CAP ;

\ The cells a declaration's stack arguments and private record copies share.
: STACK-CELLS ( -- n ) FFI-MAX-ARGS ;

\ Render into the caller's CODEGEN buffer; the caller owns the returned span.
: LIBRARY-NAME$ ( ptr u8 n n ptr n -- ptr u8 n ) {: base:ptr u:n version:n buf:ptr :}
   u BASE-CHECK
   version VERSION-CHECK
   buf CODEGEN:RESET
   s" lib" buf CODEGEN:APPEND-STRING
   base u buf CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      s" ." buf CODEGEN:APPEND-STRING
      version buf CODEGEN:APPEND-DECIMAL
      s" .dylib" buf CODEGEN:APPEND-STRING
   else HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      s" .so." buf CODEGEN:APPEND-STRING
      version buf CODEGEN:APPEND-DECIMAL
   else E-FFI-LIBRARY throw then then
   buf CODEGEN:CONTENTS ;


: LIB-INDEX ( ptr u8 n n -- n )
   {: path:ptr u:n next:n :}
   LIB-N @ 0 ?do
      i LIB-LEN@ u = i LIB-NEXT@ next = and if
         path u i LIB-PATH u STR= if i 1+ unloop exit then
      then
   loop
   0 ;

private

: LIBRARY-PATH-OR ( ptr u8 n n -- n )
   {: path:ptr u:n next:n :}
   u PATH-ROOM
   path u next LIB-INDEX dup 0<> if exit then drop
   LIB-NEW
   path u LIB-N @ PATH-FIELD CSTR drop
   LIB-N @ 1+ LIB-N !
   LIB-N @ {: row:n :}
   u row 1- LIB-LEN!
   0 row 1- LIB-HANDLE!
   0 row 1- LIB-FAILED!
   next row 1- LIB-NEXT!
   row ;

public

: LIBRARY-PATH ( ptr u8 n -- n ) 0 LIBRARY-PATH-OR ;

: CALL ( n -- n ) {: idx:n :}
   idx FN-LOOKUP {: fn:n :}
   FFI-BUF idx FN-ARGC@ fn ffi-call-n ;

: CALL-PTR ( n -- ptr u8 ) CALL CELL>PTR ;

: CALL-ABI ( n -- n ) {: idx:n :}
   idx FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF idx FN-SPILL@ fn ffi-call-abi ;

: CALL-ABI-PTR ( n -- ptr u8 ) CALL-ABI CELL>PTR ;

: CALL-ABI-R ( n -- r ) {: idx:n :}
   idx FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF idx FN-SPILL@ fn ffi-call-abi-r ;

: CALL-ABI-F32 ( n -- r )
   CALL-ABI-R IEEE754:F64>BITS $FFFFFFFF and F32:WIDEN ;

\ A struct-returning declaration resolves its row before staging: a first-call
\ dlopen can run constructors that call C through this task's scratch.
: RESOLVE-ROW ( n -- ) FN-LOOKUP drop ;

\ The caller's result storage rides x8, AAPCS64's indirect-result register. A
\ record returned in x0/x1 ignores x8. Storage C writes through x8 is
\ 16-byte aligned, the strictest a supported record can need; a register record
\ stores into eight-byte-aligned storage.
: RESULT-DEST! ( ptr u8 n -- ) {: dst:ptr len:n :}
   dst PTR>CELL {: addr:n :}
   len 16 > if 15 else 7 then {: mask:n :}
   addr 0= addr mask and 0<> or if E-FFI-SYNTAX throw then
   dst 8 POINTER! ;

\ The raw x0 and x1 after the call, in address order. The row's relocation
\ mask names the staged cells that hold a private struct copy's address in
\ the stack buffer: bits 0-7 are x0-x7 and bits 8-23 stack cells 0-15. The
\ engine copies the stack buffer into the native call frame and moves each
\ named cell to the copy there, so C receives storage that lives exactly as
\ long as the call and that no other call reuses.
: CALL-STRUCT ( n -- n n ) {: idx:n :}
   idx FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF idx FN-SPILL@ idx FN-RELOC@ fn ffi-call-struct ;

: CALL-STRUCT-PTR ( n -- ptr u8 ) CALL-STRUCT drop CELL>PTR ;

private

: REG-STORE ( n ptr u8 n -- ) {: v:n dst:ptr len:n :}
   len 0 ?do v i 8 * rshift $FF and dst i + c! loop ;

\ A by-value record is read at exactly its width: cell k holds bytes
\ 8k..8k+7, little-endian, and the last cell's bytes past len are zero.
: RECORD-CELL ( ptr u8 n n -- n ) {: src:ptr len:n k:n :}
   0 len k 8 * - 8 min 0 ?do src k 8 * + i + c@ i 8 * lshift or loop ;

: RECORD-CELLS ( ptr u8 n -- n ) {: src:ptr len:n :}
   src PTR>CELL 0= len 0 <= or if E-FFI-SYNTAX throw then
   len 1- 8 / 1+ ;

: RECORD-COPY ( ptr u8 n n -- n ) {: src:ptr len:n pay:n :}
   src len RECORD-CELLS 0 ?do src len i RECORD-CELL pay i + STACK-VALUE! loop
   pay FFI-STACK-SLOT PTR>CELL ;

public

\ STRUCT-ARG staging. A record of 16 bytes or fewer goes whole in consecutive
\ integer registers from reg, or in stack cells from pos; a larger one is
\ copied into stack cells from pay and passed by address in register reg or
\ stack cell pos.
: STRUCT-REGS! ( ptr u8 n n -- ) {: src:ptr len:n reg:n :}
   src len RECORD-CELLS 0 ?do src len i RECORD-CELL reg i + VALUE! loop ;

: STRUCT-STACK! ( ptr u8 n n -- ) {: src:ptr len:n pos:n :}
   src len RECORD-CELLS 0 ?do src len i RECORD-CELL pos i + STACK-VALUE! loop ;

: STRUCT-COPY-REG! ( ptr u8 n n n -- ) {: src:ptr len:n pay:n reg:n :}
   src len pay RECORD-COPY reg VALUE! ;

: STRUCT-COPY-STACK! ( ptr u8 n n n -- ) {: src:ptr len:n pay:n pos:n :}
   src len pay RECORD-COPY pos STACK-VALUE! ;

\ A record of len bytes returned in x0 then x1 lands at exactly its width;
\ one larger than 16 bytes is already in place through x8.
: STORE-RESULT ( n n ptr u8 n -- ) {: lo:n hi:n dst:ptr len:n :}
   len 16 > if exit then
   lo dst len 8 min REG-STORE
   len 8 > if hi dst 8 + len 8 - REG-STORE then ;

: I32-RESULT ( n -- n )
   $FFFFFFFF and $80000000 xor $80000000 - ;
\ The stored symbol's length decides along with its bytes, so neither a prefix
\ nor a name holding a 0 byte matches a row.
private

: SAME-FN? ( ptr u8 n n n n n n -- bool )
   {: name:ptr u:n lib:n argc:n spills:n reloc:n idx:n :}
   idx FN-LIB@ lib <> if false exit then
   idx FN-ARGC@ argc <> if false exit then
   idx FN-SPILL@ spills <> if false exit then
   idx FN-RELOC@ reloc <> if false exit then
   idx NAME-LEN u <> if false exit then
   name u idx FN-NAME u STR= ;

\ A relocation mask may name only the row's own cells: registers among its
\ first argc, at most x0-x7, and stack cells among its spills, so the engine
\ never moves a cell past the native frame it copies.
: RELOC-OWNED? ( n n n -- bool ) {: reloc:n argc:n spills:n :}
   1 argc 8 min lshift 1-  1 spills lshift 1- 8 lshift or {: own:n :}
   reloc own invert and 0= ;

public

: FN-INDEX ( ptr u8 n n n n n -- n )
   {: name:ptr u:n lib:n argc:n spills:n reloc:n :}
   u NAME-ROOM
   FN-N @ 0 ?do
      name u lib argc spills reloc i SAME-FN? if i unloop exit then
   loop
   -1 ;

\ Validation precedes reading a row's name.
: DECLARE-ROW ( ptr u8 n n n n n -- n )
   {: name:ptr u:n lib:n argc:n spills:n reloc:n :}
   lib 0 < if E-FFI-LIBRARY throw then
   lib LIB-N @ > if E-FFI-LIBRARY throw then
   argc FFI-MAX-ARGS FFI-CHECK-COUNT
   spills FFI-MAX-ARGS FFI-CHECK-COUNT
   reloc argc spills RELOC-OWNED? 0= if E-FFI-ARITY throw then
   name u lib argc spills reloc FN-INDEX dup 0 >= if exit then drop
   FN-NEW
   FN-N @ {: idx:n :}
   name u idx NAME-FIELD CSTR drop
   FN-N @ 1+ FN-N !
   spills idx FN-SPILL-OFF FN-FIELD !
   reloc idx FN-RELOC-OFF FN-FIELD !
   lib idx FN-LIB-OFF FN-FIELD !
   argc idx FN-ARGC-OFF FN-FIELD !
   0 idx FN-ADDR!
   idx ;

: DECLARE ( ptr u8 n n n -- n ) 0 0 DECLARE-ROW ;

\ errno is this package's own binding, written by hand because the declarer is
\ defined after this package closes. libc's location is thread-local and the
\ value is a four-byte C int.
: ERRNO ( -- n )
   ERRNO-FN CALL-PTR LE:U32@ ;

ERRNO-SYMBOL$ PROCESS 0 DECLARE ERRNO-FN-CELL !

s" dlerror" PROCESS 0 DECLARE DLERROR-FN-CELL !


\ FUNCTION: - declare a foreign function, get a checked word.
\
\     VERSIONED-LIBRARY z 1,2                 \ libz.so.1, else libz.so.2 (.1.dylib, else .2.dylib on macOS)
\     FUNCTION: CRC-CALL crc32 ( n ptr u8 n -- n ) ;FUNCTION
\     LIBRARY /usr/lib/libSystem.B.dylib      \ a literal, or PROCESS-SYMBOLS (RTLD_DEFAULT)
\     FUNCTION: SQRT-CALL sqrt ( r -- r ) ;FUNCTION
\     FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- i32 )
\        1 $10 WRITES-BYTES                   \ sockaddr_in
\        2 $04 WRITES-BYTES                   \ socklen_t
\     ;FUNCTION
\
\ The Habu word name and the C symbol are separate tokens. A symbol is often not
\ a legal Habu name (__errno_location).
\
\ The declared effect becomes the generated word's effect, with `f32` mapped
\ to Habu `r` on either side and an `i32` result mapped to `n`. A clause names a
\ `ptr u8` the callee writes: `idx len WRITES-BYTES` for a fixed width, or
\ `idx arg WRITES-ARG` when another argument carries the length. The clauses
\ are ordinary words the
\ interpreter runs between the two keywords, so their numbers are literals and
\ comments sit among them.
\ Convert a nominal at the call site: a declaration speaks the C function's own
\ types.
\
\ VERSIONED-LIBRARY, LIBRARY or PROCESS-SYMBOLS selects the library for the
\ declarations that follow. VERSIONED-LIBRARY takes a base name and a
\ comma-separated list of soname versions, renders the spelling this target
\ loads for each, and the first call opens them in the order written until one
\ opens.
\ LIBRARY takes one literal, an absolute path or an unversioned name, and
\ refuses one spelled in the other target's convention. THE SELECTION BELONGS TO
\ THE SCOPE THAT STATES IT: the package section or global scope the declarations
\ land in. There is no default: a declaration with no selection in its own scope
\ is E-FFI-LIBRARY, so a file cannot inherit a library an earlier file selected
\ and resolve its symbols through that handle's dependency tree.
\
\ The result is the call's, at the C prototype's width: `n` (or a widening
\ token) for a cell-wide integer (`long`, `size_t`, `ssize_t`, `off_t`, an
\ address kept as a number), `i32` for a C `int` (an enum, `pid_t`,
\ `kern_return_t`), `u32` for an `unsigned int` (`mach_port_t`), `r` for C
\ double, `f32` for C float, `ptr u8` for a foreign-owned address, or nothing,
\ which drops the return cell. A 32-bit result fills only the low half of the
\ return register and neither AAPCS64 nor SysV defines the high half, so a C
\ `int` declared `n` can read -1 as $FFFFFFFF. `i32` sign-extends the low half
\ and reads `n` to the checker, which has no signed 32-bit type; `u32` masks it
\ and keeps its own type.
\
\ `bytes STRUCT-RESULT` declares a C struct of `bytes`, any positive count,
\ returned by value. The last input is a `ptr u8` to caller-owned storage of at
\ least `bytes` that must not alias memory the callee reaches; it is no C
\ argument, and the output row is empty. The record has integer, pointer or
\ mixed members: an HFA (one to four members of one float type) or an HVA (one
\ to four 8- or 16-byte vectors) returns in v0-v3, which no byte count reveals,
\ and must not be declared. AAPCS64 returns up to 8 bytes in x0 and 9..16 in x0
\ then x1, stored at exactly `bytes`; C writes a larger record through x8. The
\ storage is eight-byte aligned, 16-byte aligned for a record over 16 bytes; a
\ record type aligned beyond 16 bytes is excluded.
\ The destination is one of the sixteen inputs, so at most fifteen C arguments
\ remain, placed as STRUCT-ARG's are. A null or misaligned destination is
\ E-FFI-SYNTAX at the call.
\
\ `idx bytes STRUCT-ARG` declares input idx, a `ptr u8`, the address of a C
\ struct of `bytes`, any positive count, passed by value: one C argument,
\ read at exactly `bytes` when the call is staged, so the callee never sees
\ the source. The record is neither an HFA nor an HVA, as for STRUCT-RESULT;
\ either travels in v registers and must not be declared. A declaration may
\ have up to eight float arguments beside its records; its integer and pointer
\ inputs must be cell-wide C types, since Apple arm64 packs two spilled C ints
\ into one stack cell. The stack cells, the copies and the padding that aligns
\ each copy share sixteen cells, refused past that at ;FUNCTION
\ (E-FFI-ARITY). Excluded, because a byte count cannot
\ reveal them, are records aligned beyond 16 bytes and 16-byte aligned ones
\ of 16 bytes or fewer: Apple arm64 may start such a record in an odd
\ register like any other, but on the stack it starts at a 16-byte aligned
\ slot, so after nine longs an ordinary 16-byte record is at SP+8 and a
\ 16-byte aligned one at SP+16. It combines with STRUCT-RESULT:
\     FUNCTION: DEPARSE pg_query_deparse_protobuf ( ptr u8 ptr u8 -- )
\        0 16 STRUCT-ARG  16 STRUCT-RESULT
\     ;FUNCTION
\
\ A float result reads only the low 32 bits of its FP slot and widens to Habu
\ `r`. Variadic positions use C default promotions, so declare a promoted float
\ as `r`, never `f32`.
\ A declaration carrying a float rides the ABI call, which packs no spill
\ there, so it is refused past eight of either kind (E-FFI-ARITY); an
\ integer-only declaration rides ffi-call-n, which spills past x7 itself.
\ Every SysV overflow call rides the ABI trampoline so mixed kinds keep their
\ own stack positions; a C float past the eighth
\ float register has no stack staging and is refused (E-FFI-ARITY).
\
\ ;FUNCTION builds the declared word with define-word: its
\ effect from the types read where the declaration stands, its body from
\ literals and the words found here, which no word of the declaring package
\ can capture.

private

$40 constant TOK-CAP
$10 constant ARG-MAX
8 constant REG-MAX                        \ AAPCS64 integer / both ABIs' float slots
6 constant X86-INT-REG-MAX                \ SysV rdi, rsi, rdx, rcx, r8, r9

PATH-CAP 128 + constant DIAG-CAP

public

\ An input's kind, as STRUCT-FITS? reads it: a cell, a `ptr u8`, an `r` or an
\ `f32`.
0 constant K-VALUE
1 constant K-POINTER
2 constant K-FLOAT
3 constant K-FLOAT32

private

0 constant R-NONE
1 constant R-VALUE
2 constant R-FLOAT
3 constant R-POINTER
4 constant R-I32
5 constant R-U32
6 constant R-FLOAT32

0 constant C-INT
1 constant C-POINTER
2 constant C-ABI
3 constant C-ABI-R
4 constant C-STRUCT

-1 constant NO-EXT

LIBRARY-PATH-CAP 1 - CODEGEN:BUFFER LIB-NAME
DIAG-CAP CODEGEN:BUFFER DIAG

\ One declaration's working state: a declaration word writes each cell before
\ it reads it. While no declaration is open (OPEN @ 0=) every cell rests at
\ zero, and the token cells rest outside every declaration word. Each word that
\ can write a cell keeps this on every exit, a throw included (KEEP-REST); a
\ throw while a declaration stays open keeps that declaration.
create NAME-BUF TOK-CAP allot
create SYM-BUF TOK-CAP allot
create TOK-BUF TOK-CAP allot
create BASE-BUF TOK-CAP allot
variable NAME-U
variable SYM-U
variable TOK-U
variable BASE-U

create ARG-KIND ARG-MAX cells allot
create ARG-REG ARG-MAX cells allot
create ARG-EXT ARG-MAX cells allot
create ARG-EXT-ARG ARG-MAX cells allot
create ARG-SBYTES ARG-MAX cells allot         \ STRUCT-ARG's record width; 0 when none
create ARG-PAY ARG-MAX cells allot            \ planned: a large record's first copy cell
variable ARG-N
variable RES-KIND
variable RES-NULLABLE                      \ 1 for a nullable<u8> result
variable VAR-START
variable STRUCT-BYTES                      \ STRUCT-RESULT's record width; 0 when none
TYPED-VARIABLE EFF-TYPE EFFECT:type        \ the effect, built as it is read
variable CUR-LIB
CUR-LIB ROLLBACK-CELLS:MARK
variable OPEN                              \ 0 closed, else 1 + the open declaration's clauses
variable SCOPE-WID                         \ the wordlist LIBRARY/PROCESS-SYMBOLS was stated in
variable SCOPE-SET
SCOPE-WID scratch-cell-mark  SCOPE-SET scratch-cell-mark
SCOPE-WID ROLLBACK-CELLS:MARK  SCOPE-SET ROLLBACK-CELLS:MARK

\ The rest value of EFF-TYPE, a node no effect has.
CAST: >EFFECT ( n -- EFFECT:type )

\ The token cells a library selection shares with a declaration, at rest.
: REST-TOKENS ( -- )
   TOK-CAP 0 ?do 0 TOK-BUF i + c! 0 BASE-BUF i + c! loop
   0 TOK-U ! 0 BASE-U ! ;

: REST ( -- )
   TOK-CAP 0 ?do 0 NAME-BUF i + c! 0 SYM-BUF i + c! loop
   0 NAME-U ! 0 SYM-U !
   REST-TOKENS
   ARG-MAX 0 ?do
      0 ARG-KIND i cells + ! 0 ARG-REG i cells + ! 0 ARG-EXT i cells + !
      0 ARG-EXT-ARG i cells + ! 0 ARG-SBYTES i cells + ! 0 ARG-PAY i cells + !
   loop
   0 ARG-N ! 0 RES-KIND ! 0 RES-NULLABLE !
   0 VAR-START ! 0 STRUCT-BYTES ! 0 >EFFECT EFF-TYPE ! ;

: KEEP-REST ( R [ R -- S ] -- S )
   [: OPEN @ 0= if REST else REST-TOKENS then ;] finally ;

\ ---- the declaration token stream -----------------------------------------
\ parse-name's span lasts while its line is read, so every token is copied
\ as it is read and every comparison reads the copy. A declaration's effect
\ is on its line, so its types are read from their spans.

: TOK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if
      s" the line ended before the declaration did"
      s" write FUNCTION: NAME symbol ( inputs -- output ) on one line"
      SYNTAX-REJECT
   then
   u TOK-CAP > if
      SYNTAX-OPEN  s" a declaration token is at most " REJECTION:EMIT-STR
      TOK-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" use a shorter name" SYNTAX-HELP
   then
   a TOK-BUF u BYTE-COPY
   u TOK-U ! ;

: TOK@ ( -- )
   parse-name TOK! ;

: TOK-SPAN ( -- ptr u8 n )
   parse-name 2dup TOK! ;

: TOK$ ( -- ptr u8 n )
   TOK-BUF TOK-U @ ;

: TOK-IS? ( ptr u8 n -- bool )
   TOK$ STR= ;

: NAME! ( -- )
   TOK$ {: a:ptr u:n :}
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: SYMBOL-WRITTEN ( n -- )
   {: u:n :}
   u FN-NAME-CAP 1 - > if
      SYNTAX-OPEN  s" a C symbol name is at most " REJECTION:EMIT-STR
      FN-NAME-CAP 1 - REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" call a longer symbol through a C function with a shorter name"
      SYNTAX-HELP
   then ;

: SYM! ( -- )
   TOK$ {: a:ptr u:n :}
   u SYMBOL-WRITTEN
   a SYM-BUF u BYTE-COPY
   u SYM-U ! ;

: BASE-WRITTEN ( n -- )
   {: u:n :}
   u LIB-BASE-CAP > if
      SYNTAX-OPEN  s" a library base name is at most " REJECTION:EMIT-STR
      LIB-BASE-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" write the name without its lib prefix and suffix: z for libz.so.1"
      SYNTAX-HELP
   then ;

\ The base name is copied out of the token buffer because reading the version
\ token overwrites it.
: BASE! ( -- )
   TOK$ {: a:ptr u:n :}
   u BASE-WRITTEN
   a BASE-BUF u BYTE-COPY
   u BASE-U ! ;

: VERSION-WRITTEN ( n -- )
   {: version:n :}
   version LIB-VERSION-MAX > if
      SYNTAX-OPEN  s" a library version is at most " REJECTION:EMIT-STR
      LIB-VERSION-MAX REJECTION:EMIT-U
      s" write the library's major version: 1 for libz.so.1"
      SYNTAX-HELP
   then ;

: >VERSION ( ptr u8 n -- n )
   STR-PARSE-POS MATCH option
      none OF
         s" a library version is not a number"
         s" write versions as numbers split by commas: VERSIONED-LIBRARY z 1,2"
         SYNTAX-REJECT
      ENDOF
      some OF dup VERSION-WRITTEN ENDOF
   ;MATCH ;

\ A version list's rows, made from the last version to the first, each next
\ naming the version written after it; the answer is the first version's row.
\ Every piece is parsed on the way in, so a refused list adds no row.
: VERSION-ROWS ( ptr u8 n -- n )
   {: list:ptr u:n :}
   list u [char] , INDEX-OF MATCH option
      none OF u ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: cut:n :}
   list cut >VERSION {: version:n :}
   cut u = if 0 else list cut + 1+ u cut - 1- RECURSE then {: next:n :}
   BASE-BUF BASE-U @ version LIB-NAME LIBRARY-NAME$ next LIBRARY-PATH-OR ;

\ ---- the declared effect ---------------------------------------------------

: ARG-KIND@ ( n -- n ) {: i:n :} ARG-KIND i cells + @ ;
: ARG-REG@ ( n -- n ) {: i:n :} ARG-REG i cells + @ ;
: ARG-EXT@ ( n -- n ) {: i:n :} ARG-EXT i cells + @ ;
: ARG-EXT-ARG@ ( n -- n ) {: i:n :} ARG-EXT-ARG i cells + @ ;
: ARG-EXT! ( n n -- ) {: v:n i:n :} v ARG-EXT i cells + ! ;
: ARG-EXT-ARG! ( n n -- ) {: v:n i:n :} v ARG-EXT-ARG i cells + ! ;
: ARG-SBYTES@ ( n -- n ) {: i:n :} ARG-SBYTES i cells + @ ;
: ARG-SBYTES! ( n n -- ) {: v:n i:n :} v ARG-SBYTES i cells + ! ;
: ARG-PAY@ ( n -- n ) {: i:n :} ARG-PAY i cells + @ ;
: ARG-PAY! ( n n -- ) {: v:n i:n :} v ARG-PAY i cells + ! ;

\ The effect with T as its next input, or its output.
: IN+ ( EFFECT:type -- ) EFF-TYPE @ swap EFFECT:IN EFF-TYPE ! ;
: OUT+ ( EFFECT:type -- ) EFF-TYPE @ swap EFFECT:OUT EFF-TYPE ! ;

\ A catch reads E-FFI-ARITY; uncaught, the load exits 70.
: ARITY-REJECT ( -- )
   s" E-FFI-ARITY" REJECTION:OPEN
   s" a FUNCTION: declaration has at most " REJECTION:EMIT-STR
   ARG-MAX REJECTION:EMIT-U  s"  inputs" REJECTION:EMIT-STR
   CHECK:DEFINER-PLACE
   REJECTION:HELP
   s" call it through a C function that takes the inputs in a record"
   REJECTION:EMIT-STR
   REJECTION:KEEP  E-FFI-ARITY CHECKER-REFUSE ;

: ARG+ ( n -- )
   {: kind:n :}
   ARG-N @ ARG-MAX >= if ARITY-REJECT then
   kind ARG-KIND ARG-N @ cells + !
   NO-EXT ARG-N @ ARG-EXT!
   NO-EXT ARG-N @ ARG-EXT-ARG!
   0 ARG-N @ ARG-SBYTES!
   ARG-N @ 1 + ARG-N ! ;

\ A pointer is `ptr u8`: a byte span with a length the declaration states.
\ Answers the span from `ptr`, at A, through `u8`.
: PTR-TAIL ( ptr u8 -- ptr u8 n ) {: a:ptr :}
   TOK-SPAN {: b:ptr u:n :}
   s" u8" TOK-IS? 0= if
      s" expected 'u8' after 'ptr'"
      s" declare a pointer as ptr u8, a byte span, whatever C points at"
      SYNTAX-REJECT
   then
   a  b u + a - ;

: IN-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   s" ptr" TOK-IS? if
      a PTR-TAIL EFFECT:READ-TYPE IN+ K-POINTER ARG+ exit
   then
   s" f32" TOK-IS? if s" r" EFFECT:NAMED IN+ K-FLOAT32 ARG+ exit then
   a u EFFECT:READ-TYPE IN+
   s" r" TOK-IS? if K-FLOAT ARG+ exit then
   K-VALUE ARG+ ;

: OUT-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   RES-KIND @ R-NONE <> if
      s" a FUNCTION: declaration has at most one output"
      s" C returns one value: pass others as ptr u8 inputs with WRITES-BYTES"
      SYNTAX-REJECT
   then
   s" ptr" TOK-IS? if
      a PTR-TAIL EFFECT:READ-TYPE OUT+ R-POINTER RES-KIND ! exit
   then
   s" f32" TOK-IS? if s" r" EFFECT:NAMED OUT+ R-FLOAT32 RES-KIND ! exit then
   s" i32" TOK-IS? if EFFECT:N OUT+ R-I32 RES-KIND ! exit then
   a u EFFECT:READ-TYPE OUT+
   s" nullable<u8>" TOK-IS? if R-POINTER RES-KIND ! 1 RES-NULLABLE ! exit then
   s" r" TOK-IS? if R-FLOAT RES-KIND ! exit then
   s" u32" TOK-IS? if R-U32 RES-KIND ! exit then
   R-VALUE RES-KIND ! ;

: INPUTS ( -- )
   begin
      TOK-SPAN
      s" --" TOK-IS? if 2drop exit then
      s" )" TOK-IS? if
         s" expected '--' before ')'"
         s" write the effect as ( inputs -- output ), '--' even with no output"
         SYNTAX-REJECT
      then
      IN-TOKEN
   again ;

: OUTPUTS ( -- )
   begin
      TOK-SPAN
      s" )" TOK-IS? if 2drop exit then
      s" --" TOK-IS? if
         s" a second '--' in the effect"
         s" write one '--', between the inputs and the output"
         SYNTAX-REJECT
      then
      OUT-TOKEN
   again ;

: EFFECT ( -- )
   TOK@
   s" (" TOK-IS? 0= if
      s" expected '(' after the C symbol"
      s" write FUNCTION: NAME symbol ( inputs -- output ) ;FUNCTION"
      SYNTAX-REJECT
   then
   EFFECT:EMPTY EFF-TYPE !
   INPUTS
   OUTPUTS ;

\ ---- which call the declaration rides --------------------------------------

: REG-ROOM ( n n -- )
   {: int:n flt:n :}
   int HB-TARGET-LINUX-X86-64? if X86-INT-REG-MAX else REG-MAX then
      > if E-FFI-ARITY throw then
   flt REG-MAX > if E-FFI-ARITY throw then ;

: FLOAT-ARG? ( n -- bool )
   ARG-KIND@ {: kind:n :}
   kind K-FLOAT = kind K-FLOAT32 = or ;

: PLACE-REG ( n n n -- n n )
   {: int:n flt:n i:n :}
   i FLOAT-ARG? if flt ARG-REG i cells + ! int flt 1+ exit then
   int ARG-REG i cells + ! int 1+ flt ;

\ Answers the integer and float registers taken.
: PLAN-REGS ( n -- n n )
   {: argc:n :}
   0 0 argc 0 ?do i PLACE-REG loop ;

: X86-SPILL ( n n -- n ) {: stk:n i:n :}
   stk 1+ negate ARG-REG i cells + !
   stk 1+ ;

: X86-PLACE ( n n n n -- n n n )
   {: int:n flt:n stk:n i:n :}
   i FLOAT-ARG? if
      flt REG-MAX < if flt ARG-REG i cells + ! int flt 1+ stk exit then
      i ARG-KIND@ K-FLOAT32 = if E-FFI-ARITY throw then
      int flt stk i X86-SPILL exit
   then
   int X86-INT-REG-MAX < if int ARG-REG i cells + ! int 1+ flt stk exit then
   int flt stk i X86-SPILL ;

: PLAN-X86 ( -- n n n )
   0 0 0 ARG-N @ 0 ?do i X86-PLACE loop ;

\ A macOS variadic argument takes a stack cell, so the registers taken are the
\ fixed arguments'.
: PLAN-SPILLS ( -- n n n )
   ARG-N @ PLAN-REGS
   VAR-START @ 0 < HB-TARGET-MACOS? 0= or if 0 exit then
   2drop
   VAR-START @ PLAN-REGS
   2dup REG-ROOM
   ARG-N @ VAR-START @ - ;

: C-ARGS ( -- n )
   ARG-N @ STRUCT-BYTES @ 0 > if 1- then ;

: RECORD-WORDS ( n -- n ) 1- 8 / 1+ ;

: INT-WORDS ( n -- n ) {: bytes:n :}
   bytes 0 > bytes 16 <= and if bytes RECORD-WORDS exit then
   1 ;

: PLACE-ARG ( n n n n n -- n n n n ) {: int:n flt:n stack:n kind:n bytes:n :}
   kind K-FLOAT = kind K-FLOAT32 = or if int flt 1+ stack flt exit then
   bytes INT-WORDS {: words:n :}
   int words + REG-MAX <= if int words + flt stack int exit then
   REG-MAX flt stack words + stack 1+ negate ;

: PLACE-COPY ( n n -- n n ) {: pos:n bytes:n :}
   bytes 16 <= if pos 0 exit then
   pos 1+ 2 / 2 * {: pay:n :}
   \ A width past the capacity left makes the plan unfit at once, so no sum of
   \ payloads can wrap.
   STACK-CELLS pay - 8 * bytes < if STACK-CELLS 1+ pay exit then
   pay bytes RECORD-WORDS + pay ;

\ The plan of n C arguments, each with its kind in kinds and its record width,
\ 0 for none, in sbytes: each one's place goes to regs and its copy's to pays.
\ Answers the integer registers, float registers and stack cells it takes.
: PLAN-RECORDS ( ptr n ptr n n ptr n ptr n -- n n n )
   {: kinds:ptr sbytes:ptr argc:n regs:ptr pays:ptr :}
   0 0 0 argc 0 ?do
      kinds i cells + @ sbytes i cells + @ PLACE-ARG
      regs i cells + !
   loop
   argc 0 ?do
      sbytes i cells + @ PLACE-COPY
      pays i cells + !
   loop ;

: PLAN-FITS? ( n n -- bool ) {: flt:n stk:n :}
   flt REG-MAX <= stk STACK-CELLS <= and ;

public

\ Whether n C arguments, kinds and record widths as PLAN-RECORDS reads them,
\ fit the struct call; past it the declaration is refused (E-FFI-ARITY).
\ regs and pays take the plan, n cells each. The source checker holds a
\ declaration's clauses to the rule this way.
: STRUCT-FITS? ( ptr n ptr n n ptr n ptr n -- bool )
   PLAN-RECORDS {: flt:n stk:n :}
   drop flt stk PLAN-FITS? ;

private

: PLAN-STRUCT ( -- n n )
   ARG-KIND ARG-SBYTES C-ARGS ARG-REG ARG-PAY PLAN-RECORDS
   {: int:n flt:n stk:n :}
   flt stk PLAN-FITS? 0= if E-FFI-ARITY throw then
   int stk ;

: RECORDS? ( -- bool )
   false ARG-N @ 0 ?do i ARG-SBYTES@ 0 > or loop ;

: PLAN-TARGET ( -- n n n )
   HB-TARGET-LINUX-X86-64? if PLAN-X86 exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if PLAN-SPILLS exit then
   E-FFI-LIBRARY throw ;

\ Answers the integer registers and stack cells the call takes, which its row
\ keeps, and the call it rides.
: PLAN-CALL ( -- n n n )
   STRUCT-BYTES @ 0 > RECORDS? or if PLAN-STRUCT C-STRUCT exit then
   PLAN-TARGET {: int:n flt:n stk:n :}
   RES-KIND @ R-FLOAT = RES-KIND @ R-FLOAT32 = or {: fres:bool :}
   stk 0 > if int stk fres if C-ABI-R else C-ABI then exit then
   fres if int flt REG-ROOM int stk C-ABI-R exit then
   flt 0 > if int flt REG-ROOM int stk C-ABI exit then
   RES-KIND @ R-POINTER = if int stk C-POINTER exit then
   int stk C-INT ;

\ ---- the built definition -------------------------------------------------

\ The type of argument I's local, by its kind.
: LOCAL-TYPE ( n -- EFFECT:type ) {: i:n :}
   i ARG-KIND@ K-VALUE = if EFFECT:N exit then
   i ARG-KIND@ K-POINTER = if s" u8" EFFECT:NAMED EFFECT:PTR exit then
   s" r" EFFECT:NAMED ;

\ One local per argument: the definition declares no other, so argument I's
\ local has index I.
: LOCALS ( -- )
   ARG-N @ 0= if exit then
   s" {:" EFFECT:KEYWORD
   ARG-N @ 0 ?do i LOCAL-TYPE EFFECT:LOCAL drop loop
   s" :}" EFFECT:KEYWORD ;

: STAGE-VALUE ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone VALUE! ;

: STAGE-FLOAT ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone FLOAT! ;

: STAGE-FLOAT32 ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone FLOAT32! ;

: STAGE-POINTER ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone POINTER! ;

: STACK-POS ( n -- ) {: i:n :}
   i ARG-REG@ 0 < if i ARG-REG@ negate 1- literal exit then
   i VAR-START @ - literal ;
: STAGE-SPILL ( n -- ) {: i:n :}
   i ARG-KIND@ K-FLOAT = if
      i STACK-POS postpone STACK-FLOAT! exit then
   i ARG-KIND@ K-VALUE = if
      i STACK-POS postpone STACK-VALUE! exit then
   i STACK-POS postpone STACK-POINTER! ;

: STAGE-REG ( n -- ) {: i:n :}
   i ARG-KIND@ K-FLOAT = if i STAGE-FLOAT exit then
   i ARG-KIND@ K-FLOAT32 = if i STAGE-FLOAT32 exit then
   i ARG-KIND@ K-VALUE = if i STAGE-VALUE exit then
   i STAGE-POINTER ;

: STAGE-ONE ( n -- ) {: i:n :}
   HB-TARGET-LINUX-X86-64? i ARG-REG@ 0 < and if i STAGE-SPILL exit then
   HB-TARGET-MACOS? VAR-START @ 0 >= and i VAR-START @ >= and if
      i STAGE-SPILL exit
   then
   i STAGE-REG ;

: STAGE ( -- )
   ARG-N @ 0 ?do ARG-N @ i 1+ - STAGE-ONE loop ;

: CALL-WORD ( n -- )
   {: kind:n :}
   RES-KIND @ R-FLOAT32 = if postpone CALL-ABI-F32 exit then
   kind C-ABI-R = if postpone CALL-ABI-R exit then
   kind C-ABI = if
      RES-KIND @ R-POINTER = if postpone CALL-ABI-PTR
      else postpone CALL-ABI then exit
   then
   kind C-POINTER = if postpone CALL-PTR exit then
   postpone CALL ;

\ ---- the row this declaration takes ----------------------------------------

: DIAG+ ( ptr u8 n -- )
   DIAG CODEGEN:APPEND-STRING ;

\ A library name spelled in the other target's convention is refused where it
\ is stated, not at the first call in whatever module declared it. A rendered
\ name passes by construction.
: TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if s" linux" exit then
   HB-TARGET-MACOS? if s" macos" exit then
   E-FFI-LIBRARY throw ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: path:ptr u:n :}
   0 u 0 ?do path i + c@ [char] / = if drop i 1+ then loop
   {: start:n :} path start + u start - ;

: FOREIGN-SPELLING? ( ptr u8 n -- bool )
   BASENAME {: path:ptr u:n :}
   HB-TARGET-MACOS? if
      path u s" .dylib" ENDS-WITH? if false exit then
      path u s" .so" ENDS-WITH? path u s" .so." CONTAINS? or exit
   then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      path u s" .dylib" ENDS-WITH? exit
   then
   E-FFI-LIBRARY throw ;

: REPORT-LIBRARY-TARGET ( ptr u8 n -- ) {: path:ptr u:n :}
   DIAG CODEGEN:RESET
   s" ffi: library " DIAG+
   path u DIAG+
   s"  is spelled for the other target; this build loads for " DIAG+
   TARGET$ DIAG+
   s" : name it with VERSIONED-LIBRARY" DIAG+
   LF-C DIAG CODEGEN:APPEND-BYTE
   DIAG CODEGEN:CONTENTS ERROR-TEXT:TYPE ;

: CHECK-SPELLING ( ptr u8 n -- ) {: path:ptr u:n :}
   path u FOREIGN-SPELLING? 0= if exit then
   path u REPORT-LIBRARY-TARGET
   E-FFI-LIBRARY throw ;


\ The cells holding a larger record's copy address, which the call moves to
\ the copy in its native frame: bit reg for x0-x7, bit 8+cell for a stack
\ cell. The row keeps this mask; no caller supplies one.
: RELOC-MASK ( -- n )
   0 C-ARGS 0 ?do
      i ARG-SBYTES@ 16 > if
         i ARG-REG@ 0 < if 1 i ARG-REG@ negate 1- 8 + lshift
         else 1 i ARG-REG@ lshift then
         or
      then
   loop ;

: SLOT ( n n -- n )
   {: int:n stk:n :}
   SYM-BUF SYM-U @ CUR-LIB @ int stk RELOC-MASK DECLARE-ROW ;

\ A niche value's zero cell is none, so `some` of the returned cell reads NULL as none.
: RESULT-TAIL ( -- )
   RES-NULLABLE @ 0<> if postpone NULLABLE:SOME exit then
   RES-KIND @ R-NONE = if postpone drop exit then
   RES-KIND @ R-I32 = if postpone I32-RESULT exit then
   RES-KIND @ R-U32 = if $FFFFFFFF literal postpone and then ;

: STAGE-RECORD ( n -- ) {: i:n :}
   i ARG-SBYTES@ literal
   i ARG-SBYTES@ 16 <= if
      i ARG-REG@ 0 < if i STACK-POS postpone STRUCT-STACK! exit then
      i ARG-REG@ literal postpone STRUCT-REGS! exit
   then
   i ARG-PAY@ literal
   i ARG-REG@ 0 < if i STACK-POS postpone STRUCT-COPY-STACK! exit then
   i ARG-REG@ literal postpone STRUCT-COPY-REG! ;

: STAGE-C-ARG ( n -- ) {: i:n :}
   i EFFECT:LOCAL@
   i ARG-SBYTES@ 0 > if i STAGE-RECORD exit then
   i ARG-REG@ 0 < if i STAGE-SPILL exit then
   i STAGE-REG ;

\ The STRUCT-RESULT destination, the last argument, and its width.
: DEST ( -- )
   ARG-N @ 1- EFFECT:LOCAL@ STRUCT-BYTES @ literal ;

: STRUCT-BODY ( n -- ) {: slot:n :}
   LOCALS
   slot literal postpone RESOLVE-ROW
   C-ARGS 0 ?do i STAGE-C-ARG loop
   STRUCT-BYTES @ 0 > if DEST postpone RESULT-DEST! then
   slot literal
   STRUCT-BYTES @ 0 > if
      postpone CALL-STRUCT DEST postpone STORE-RESULT exit
   then
   RES-KIND @ R-POINTER = if
      postpone CALL-STRUCT-PTR RESULT-TAIL exit
   then
   postpone CALL-STRUCT postpone drop
   RESULT-TAIL ;

: EMIT ( n n n -- )
   {: int:n stk:n kind:n :}
   int stk SLOT {: slot:n :}
   NAME-BUF NAME-U @ EFF-TYPE @ define-word
   kind C-STRUCT = if slot STRUCT-BODY postpone ; exit then
   STAGE
   slot literal
   kind CALL-WORD
   RESULT-TAIL
   postpone ; ;

\ The selection scope is the current wordlist, not the loading file:
\ INCLUDE-PATH names the last file opened, so every consumer would share one
\ identity. A wordlist is saved and restored with the package scope and
\ separates two consumers whose requires end at one file. A selection belongs
\ to the source load that made it, and a wordlist id holds only in the process
\ that made it, so both cells are scratch: every saved program, a cross-built
\ one included, starts with none.
: RECORD-SCOPE ( -- )
   get-current SCOPE-WID !
   1 SCOPE-SET ! ;

: CHECK-SCOPE ( -- )
   SCOPE-SET @ 0= if E-FFI-LIBRARY throw then
   get-current SCOPE-WID @ <> if E-FFI-LIBRARY throw then ;

public

\ Nothing is published before ;FUNCTION, and the row is registered as the word
\ is built, so a refused declaration leaves no half-built word.
: OPEN-FUNCTION ( -- )
   [: CHECK-SCOPE
      OPEN @ 0 <> if
         s" FUNCTION: inside an open declaration"
         s" end the declaration before it with ;FUNCTION"
         SYNTAX-REJECT
      then
      0 ARG-N ! -1 VAR-START ! 0 STRUCT-BYTES !
      R-NONE RES-KIND ! 0 RES-NULLABLE !
      TOK@ NAME!
      TOK@ SYM!
      EFFECT
      1 OPEN ! ;] KEEP-REST ;

: CLOSE-FUNCTION ( -- )
   [: OPEN @ 0= if
         s" ;FUNCTION with no declaration open"
         s" open one with FUNCTION: NAME symbol ( inputs -- output )"
         SYNTAX-REJECT
      then
      0 OPEN !
      PLAN-CALL
      EMIT ;] KEEP-REST ;

private

\ A refused clause ABANDONS the declaration rather than leaving it half open:
\ the next FUNCTION: in the file would otherwise refuse too, and the first
\ diagnostic would name the wrong declaration.
: REJECT ( ptr u8 n ptr u8 n -- )
   0 OPEN !
   SYNTAX-REJECT ;

: NOT-OPEN ( -- )
   s" a clause outside a FUNCTION: declaration"
   s" write the clause between FUNCTION: and ;FUNCTION" REJECT ;

: NO-STRUCT-CALL ( -- )
   s" this target has no struct call"
   s" pass the record by its address through a C function" REJECT ;

: STRUCT-VARIADIC ( -- )
   s" a struct call with VARIADIC"
   s" a struct call has no variadic arguments: declare a fixed effect"
   REJECT ;

: NO-INPUT ( -- )
   s" the clause names no input of the declaration"
   s" name an input by its place in the effect, the first 0" REJECT ;

: NOT-SPAN ( -- )
   s" the clause names an input that is not ptr u8"
   s" name a ptr u8 input" REJECT ;

: TAKEN ( -- )
   s" a second clause names the same input"
   s" give each ptr u8 input one WRITES-BYTES, WRITES-ARG or STRUCT-ARG"
   REJECT ;

: DEST-TAKEN ( -- )
   s" the clause names STRUCT-RESULT's destination, the last input"
   s" name another input: C writes the result record to the last one"
   REJECT ;

: CLAUSE-TARGET ( n -- ) {: idx:n :}
   OPEN @ 0= if NOT-OPEN then
   idx 0 < idx ARG-N @ >= or if NO-INPUT then
   idx ARG-KIND@ K-POINTER <> if NOT-SPAN then
   idx ARG-EXT@ NO-EXT <> if TAKEN then
   idx ARG-EXT-ARG@ NO-EXT <> if TAKEN then
   idx ARG-SBYTES@ 0 <> if TAKEN then
   STRUCT-BYTES @ 0 <> idx ARG-N @ 1- = and if DEST-TAKEN then ;

: VARIADIC-TWICE ( -- )
   s" a second VARIADIC"
   s" declare VARIADIC once, with the count of fixed inputs" REJECT ;

: FIXED-RANGE ( -- )
   s" VARIADIC's fixed count is not from 1 to the inputs declared"
   s" write n VARIADIC with n the inputs before the first variadic one"
   REJECT ;

: VARIADIC-F32 ( -- )
   s" an f32 variadic argument"
   s" C promotes a variadic float to double: declare it r" REJECT ;

: VARIADIC-CLAUSE ( n -- ) {: fixed:n :}
   OPEN @ 0= if NOT-OPEN then
   VAR-START @ -1 <> if VARIADIC-TWICE then
   STRUCT-BYTES @ 0 <> RECORDS? or if STRUCT-VARIADIC then
   fixed 1 < fixed ARG-N @ > or if FIXED-RANGE then
   ARG-N @ fixed ?do
      i ARG-KIND@ K-FLOAT32 = if VARIADIC-F32 then
   loop
   fixed VAR-START !
   1 OPEN +! ;

\ Argument idx is written by the callee over the span named here, a fixed width
\ or the argument that carries the length.
: EXTENT-CLAUSE ( n n -- ) {: idx:n len:n :}
   idx CLAUSE-TARGET
   len 0 <= if
      s" WRITES-BYTES's byte count is not positive"
      s" write idx n WRITES-BYTES with n the bytes C writes, at least 1"
      REJECT
   then
   len idx ARG-EXT!
   1 OPEN +! ;

: LENGTH-CLAUSE ( n n -- ) {: idx:n src:n :}
   idx CLAUSE-TARGET
   src 0 < src ARG-N @ >= or if
      s" WRITES-ARG's length names no input of the declaration"
      s" write idx len WRITES-ARG with len the input holding the byte count"
      REJECT
   then
   src ARG-KIND@ K-VALUE <> if
      s" WRITES-ARG's length input is not an integer"
      s" name an n input that holds the byte count" REJECT
   then
   src idx ARG-EXT-ARG!
   1 OPEN +! ;

: RESULT-CHECK ( n -- ) {: bytes:n :}
   STRUCT-BYTES @ 0 <> if
      s" a second STRUCT-RESULT" s" declare STRUCT-RESULT once" REJECT
   then
   bytes 0 <= if
      s" STRUCT-RESULT's byte count is not positive"
      s" write n STRUCT-RESULT with n the record's size in bytes" REJECT
   then
   RES-KIND @ R-NONE <> if
      s" STRUCT-RESULT with a declared output"
      s" declare no output: C writes the record to the last input" REJECT
   then ;

: STRUCT-RESULT-CLAUSE ( n -- ) {: bytes:n :}
   OPEN @ 0= if NOT-OPEN then
   HB-TARGET-LINUX-X86-64? if NO-STRUCT-CALL then
   bytes RESULT-CHECK
   VAR-START @ -1 <> if STRUCT-VARIADIC then
   ARG-N @ 0= if
      s" STRUCT-RESULT with no input to write to"
      s" declare the record's address as the last input, a ptr u8" REJECT
   then
   ARG-N @ 1- CLAUSE-TARGET
   bytes STRUCT-BYTES !
   1 OPEN +! ;

\ The struct call returns x0 and x1 only, so a float result has no shape here.
: STRUCT-ARG-CLAUSE ( n n -- ) {: idx:n bytes:n :}
   OPEN @ 0= if NOT-OPEN then
   HB-TARGET-LINUX-X86-64? if NO-STRUCT-CALL then
   idx CLAUSE-TARGET
   bytes 0 <= if
      s" STRUCT-ARG's byte count is not positive"
      s" write idx n STRUCT-ARG with n the record's size in bytes" REJECT
   then
   VAR-START @ -1 <> if STRUCT-VARIADIC then
   RES-KIND @ R-FLOAT = RES-KIND @ R-FLOAT32 = or if
      s" STRUCT-ARG with a float result"
      s" the struct call returns x0 and x1: declare an integer result" REJECT
   then
   bytes idx ARG-SBYTES!
   1 OPEN +! ;

: PATH-WRITTEN ( n -- )
   {: u:n :}
   u 0 <= if
      s" the library path is empty"
      s" write the library's path or file name after LIBRARY, on its line"
      SYNTAX-REJECT
   then
   u LIB-PATH-CAP 1 - > if
      SYNTAX-OPEN  s" a library path is at most " REJECTION:EMIT-STR
      LIB-PATH-CAP 1 - REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" name the library by its file name, which the loader searches for"
      SYNTAX-HELP
   then ;
public

: SET-VARIADIC ( n -- ) [: VARIADIC-CLAUSE ;] KEEP-REST ;
: WRITE-EXTENT ( n n -- ) [: EXTENT-CLAUSE ;] KEEP-REST ;
: WRITE-LENGTH-ARG ( n n -- ) [: LENGTH-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-RESULT ( n -- ) [: STRUCT-RESULT-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-ARG ( n n -- ) [: STRUCT-ARG-CLAUSE ;] KEEP-REST ;

: SELECT-LIBRARY ( ptr u8 n -- ) {: path:ptr u:n :}
   u PATH-ROOM
   path u CHECK-SPELLING
   path u LIBRARY-PATH CUR-LIB !
   RECORD-SCOPE ;

\ LIBRARY's path as written: a bad length is rejected with a record placed at it.
: SELECT-WRITTEN-LIBRARY ( ptr u8 n -- ) {: path:ptr u:n :}
   u PATH-WRITTEN
   path u SELECT-LIBRARY ;

: SELECT-VERSIONED-LIBRARY ( -- )
   [: TOK@ BASE!
      TOK@ TOK$ VERSION-ROWS CUR-LIB !
      RECORD-SCOPE ;]
   KEEP-REST ;

: SELECT-PROCESS ( -- )
   PROCESS CUR-LIB !
   RECORD-SCOPE ;

\ Both wordlists are protected here, after FFI's last definition: an engine
\ that loads this file after its seal refuses any definition into a protected
\ wordlist and exits 84.
seal-package

;package

\ The declaration keywords are global: a package word ending in a colon cannot
\ be reached through a qualifier, and a declarer reads bare from any package
\ anyway. All three parse the live input stream, which their ( -- ) rows do
\ not model, so they are top-level-interpret-only.
: LIBRARY ( -- )
   parse-name FFI:SELECT-WRITTEN-LIBRARY ;
parses: LIBRARY 1

: VERSIONED-LIBRARY ( -- )
   FFI:SELECT-VERSIONED-LIBRARY ;

: PROCESS-SYMBOLS ( -- )
   FFI:SELECT-PROCESS ;

: FUNCTION: ( -- )
   FFI:OPEN-FUNCTION ;

: WRITES-BYTES ( n n -- )
   FFI:WRITE-EXTENT ;

: WRITES-ARG ( n n -- )
   FFI:WRITE-LENGTH-ARG ;

: VARIADIC ( n -- ) FFI:SET-VARIADIC ;

: STRUCT-RESULT ( n -- ) FFI:SET-STRUCT-RESULT ;

: STRUCT-ARG ( n n -- ) FFI:SET-STRUCT-ARG ;

: ;FUNCTION ( -- )
   FFI:CLOSE-FUNCTION ;
