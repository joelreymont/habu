split-str
\ ffi-abi.f - checked AAPCS64 and SysV FFI calls, package FFI. AAPCS64 passes
\ eight integer and eight float registers, SysV six and eight. FUNCTION: is the
\ only call surface: its word names a row, never an address, and the row and
\ staging words are private.
\ STORAGE CLASS. TASK-LOCAL: marshalling buffers and the DLSYM loader block;
\ calls nest only through a C callback, which shares them. PROCESS-WIDE: rows,
\ whose addresses, handles and failures are scratch.
\
\ FUNCTION: names a row here and its generated word carries the row's index, so
\ a checked caller reaches only a symbol some declaration published. Exact
\ declarations share one row. A row resolves its symbol on the first call and
\ caches the address. Library 0 is the process (RTLD_DEFAULT, no reference);
\ any other index is a path dlopened once, at the first call; one that does not
\ open passes the call to the next library its row names. Rows may move when a
\ table grows; indices stay stable.
\
\ Every call is planned over its C arguments in order; the STRUCT-RESULT
\ destination, the last input, is not one. A float takes the next free float
\ register and an integer the next free integer register, else a stack slot.
\ A record of up to 16 bytes takes its registers whole, the last one's unused
\ high bytes zero, or when too few remain its stack words whole. After that
\ spill AAPCS64 places no later input in an integer register (C.13; LLVM does
\ the same on Darwin); SysV leaves its unused registers for later arguments.
\ A larger record takes stack words on SysV. AAPCS64 passes it as a pointer,
\ an integer argument, to a private copy in the native call frame that lives
\ exactly as long as the call; the copy follows every stack argument from an
\ even cell, 16-byte aligned on the 16-byte aligned native stack.
\ SysV reserves its first integer register for a result larger than 16 bytes;
\ AAPCS64 uses x8 without consuming an argument register. A planned register
\ is its number, a stack slot is -1 less its byte offset, and a private
\ copy's place is a cell index, 0 for none.

require src/lib/ieee754.f

HB-TARGET-MACOS? HB-TARGET-LINUX-KERNEL? or ?TARGET
s" src/core/exit-status.f" required
s" src/lib/error.f" required
s" src/lib/span.f" required
s" src/lib/string-option.f" required
s" src/lib/le.f" required
s" src/lib/float32.f" required
s" src/lib/adt/option.f" required
s" src/lib/adt/nullable.f" required
s" src/lib/codegen.f" required
s" src/lib/memory.f" required

package FFI

EXPORT BOOT:data-base
EXPORT CHECK:DEFINER-PLACE
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-TOP-ONLY
EXPORT SOURCE-ROOT:OPENED
EXPORT SOURCE-ROOT:CLOSED
EXPORT SOURCE-ROOT:NESTED-REJECT

FFI-ABI:STACK-BUF-BYTES 1 cells / constant FFI-MAX-ARGS
\ AAPCS64's integer argument registers, and both ABIs' float slots.
FFI-ABI:FBUF-BYTES 1 cells / constant REG-MAX
2 constant FFI-RTLD-NOW

: FFI-BUF ( -- ^n )
   data-base FFI-ABI:BUF-OFF + ;

: FFI-FBUF ( -- ^r )
   data-base FFI-ABI:FBUF-OFF + ;

\ A C float occupies the low half of the same FP-register slot as a double.
: FFI-F32BUF ( -- ^n )
   data-base FFI-ABI:FBUF-OFF + ;

\ A spill slot holds an integer, a float or narrower C bytes as the marshalled
\ signature says, so FFI-STACK-BUF, FFI-STACK-FBUF and FFI-STACK-BYTES declare
\ one offset at three pointees rather than one `( -- ^a )` that would let a
\ caller pick engine DATA's type.
: FFI-STACK-BUF ( -- ^n )
   data-base FFI-ABI:STACK-BUF-OFF + ;

: FFI-STACK-FBUF ( -- ^r )
   data-base FFI-ABI:STACK-BUF-OFF + ;

: FFI-STACK-BYTES ( -- ^u8 )
   data-base FFI-ABI:STACK-BUF-OFF + ;

: FFI-DLBUF ( -- ^n )
   data-base FFI-ABI:DLBUF-OFF + ;

\ CELL>PTR stays private: a foreign address carries no length, so only a word
\ that knows the callee's width may publish one.
: PTR>CELL ( ^a -- n ) NULL-PTR - ;
CAST: CELL>PTR ( n -- ^u8 )

\ ---- argument marshalling -------------------------------------------------
\ Integer slot 8 is x8, the AAPCS64 indirect-result register. The stack buffer
\ is addressed by byte offset and copied to the C stack exactly as prepacked.
\ Every slot and offset is a literal FUNCTION:'s planner bounds at ;FUNCTION, so
\ none is checked here.
: FFI-SLOT ( n -- ^n ) {: idx:n :}
   FFI-BUF idx cells + ;
: FFI-FSLOT ( n -- ^r ) {: idx:n :}
   FFI-FBUF idx cells + ;
: FFI-F32SLOT ( n -- ^n ) {: idx:n :}
   FFI-F32BUF idx cells + ;
: FFI-STACK-SLOT ( n -- ^n )
   {: off:n :}
   FFI-STACK-BUF off + ;
: FFI-STACK-FSLOT ( n -- ^r )
   {: off:n :}
   FFI-STACK-FBUF off + ;
: FFI-STACK-AT ( n -- ^u8 )
   {: off:n :}
   FFI-STACK-BYTES off + ;
: VALUE! ( n n -- ) {: v:n idx:n :}
   v idx FFI-SLOT ! ;
: POINTER! ( ^a n -- ) {: p:^ idx:n :}
   p PTR>CELL idx VALUE! ;
: FLOAT! ( r n -- ) {: v:r idx:n :}
   v idx FFI-FSLOT ! ;
: FLOAT32! ( r n -- ) {: value:r idx:n :}
   value F32:NARROW idx FFI-F32SLOT ! ;
: STACK-VALUE! ( n n -- )
   {: v:n off:n :}
   v off FFI-STACK-SLOT ! ;
: STACK-FLOAT! ( r n -- )
   {: v:r off:n :}
   v off FFI-STACK-FSLOT ! ;
: STACK-U8! ( n n -- )
   {: v:n off:n :}
   v $FF and off FFI-STACK-AT c! ;
: STACK-U16! ( n n -- )
   {: v:n off:n :}
   v off FFI-STACK-AT LE:U16! ;
: STACK-U32! ( n n -- )
   {: v:n off:n :}
   v off FFI-STACK-AT LE:U32! ;
: STACK-FLOAT32! ( r n -- )
   {: value:r off:n :}
   value F32:NARROW off STACK-U32! ;
: STACK-POINTER! ( ^a n -- )
   {: value:^ off:n :}
   value PTR>CELL off STACK-VALUE! ;

: DLOPEN-RAW ( ^u8 n -- n ) {: path:^ flags:n :}
   path PTR>CELL FFI-DLBUF !
   flags FFI-DLBUF CELL + !
   FFI-DLBUF 2 dlopen-entry ffi-call-n ;

: DLSYM-RAW ( n ^u8 -- n ) {: handle:n name:^ :}
   handle FFI-DLBUF !
   name PTR>CELL FFI-DLBUF CELL + !
   FFI-DLBUF 2 dlsym-entry ffi-call-n ;

\ ---- declared foreign functions -------------------------------------------

PATH-CAP 1 + constant LIB-PATH-CAP

0 cells constant FN-ADDR-OFF
1 cells constant FN-LIB-OFF
2 cells constant FN-SPILL-OFF
3 cells constant FN-RELOC-OFF
4 cells constant FN-NAME-OFF              \ the symbol's address, a pointer cell
5 cells constant FN-NAME-LEN-OFF          \ the symbol's length, its 0 byte not counted
6 cells constant FN-ROW-BYTES
0 cells constant LIB-LEN-OFF
1 cells constant LIB-HANDLE-OFF
2 cells constant LIB-FAILED-OFF
3 cells constant LIB-NEXT-OFF             \ the library index tried when this path does not open, or 0
4 cells constant LIB-PATH-OFF
LIB-PATH-OFF LIB-PATH-CAP + 7 + 8 / 8 * constant LIB-ROW-BYTES
PERSISTED-PTR-VARIABLE FN-TABLE
PERSISTED-PTR-VARIABLE LIB-TABLE
variable FN-CAP
variable LIB-CAP
variable FN-N
variable LIB-N

: ALIGNED-ROWS ( n -- ^u8 )
   {: size:n :}
   align
   here BYTE-VIEW {: rows:^ :}
   size allot
   rows ;

\ An old block stays allotted: a callback the loader runs may grow a table
\ while the loader still reads a library row's path there.
: FN-GROW ( -- )
   FN-CAP @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   next FN-ROW-BYTES * ALIGNED-ROWS {: rows:^ :}
   next 0 ?do
      rows i FN-ROW-BYTES * + {: row:^ :}
      row FN-ADDR-OFF + scratch-cell-mark
      row FN-NAME-OFF + ptr-cell-mark
   loop
   oldcap 0 > if
      FN-TABLE @ {: old:^ :}
      old rows FN-N @ FN-ROW-BYTES * BYTE-COPY
      FN-N @ 0 ?do 0 old i FN-ROW-BYTES * + FN-ADDR-OFF + CELL-VIEW ! loop
   then
   rows FN-TABLE !
   next FN-CAP ! ;

: LIB-GROW ( -- )
   LIB-CAP @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   next LIB-ROW-BYTES * ALIGNED-ROWS {: rows:^ :}
   next 0 ?do
      rows i LIB-ROW-BYTES * + LIB-HANDLE-OFF + scratch-cell-mark
      rows i LIB-ROW-BYTES * + LIB-FAILED-OFF + scratch-cell-mark
   loop
   oldcap 0 > if
      LIB-TABLE @ {: old:^ :}
      old rows LIB-N @ LIB-ROW-BYTES * BYTE-COPY
      LIB-N @ 0 ?do
         old i LIB-ROW-BYTES * + {: row:^ :}
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

: FN-ROW ( n -- ^u8 )
   {: idx:n :}
   FN-TABLE @ idx FN-ROW-BYTES * + ;

: LIB-ROW ( n -- ^u8 )
   {: idx:n :}
   LIB-TABLE @ idx LIB-ROW-BYTES * + ;

: FN-FIELD ( n n -- ^n )
   {: idx:n off:n :}
   idx FN-ROW off + CELL-VIEW ;

: LIB-FIELD ( n n -- ^n )
   {: idx:n off:n :}
   idx LIB-ROW off + CELL-VIEW ;

\ A new library row's path field, written before the row is counted, so a
\ rejected path leaves the row count as it was.
: PATH-FIELD ( n -- SPAN:span<u8> )
   LIB-ROW LIB-PATH-OFF + LIB-PATH-CAP SPAN:MAKE ;

: NAME-CELL ( n -- ^^u8 )
   {: idx:n :}
   idx FN-ROW FN-NAME-OFF + CELL-VIEW 0 ptr-field ;

: FN-NAME ( n -- ^u8 )
   {: idx:n :}
   idx NAME-CELL @ ;

: NAME-LEN ( n -- n )
   {: idx:n :}
   idx FN-NAME-LEN-OFF FN-FIELD @ ;

: FN-ADDR@ ( n -- n )
   {: idx:n :}
   idx FN-ADDR-OFF FN-FIELD @ ;

: FN-ADDR! ( n n -- )
   {: value:n idx:n :}
   value idx FN-ADDR-OFF FN-FIELD ! ;

: FN-LIB@ ( n -- n )
   {: idx:n :}
   idx FN-LIB-OFF FN-FIELD @ ;

: FN-SPILL@ ( n -- n )
   {: idx:n :}
   idx FN-SPILL-OFF FN-FIELD @ ;

: FN-RELOC@ ( n -- n )
   {: idx:n :}
   idx FN-RELOC-OFF FN-FIELD @ ;

: LIB-PATH ( n -- ^u8 )
   {: idx:n :}
   idx LIB-ROW LIB-PATH-OFF + ;

: LIB-LEN@ ( n -- n )
   {: idx:n :}
   idx LIB-LEN-OFF LIB-FIELD @ ;

: LIB-LEN! ( n n -- )
   {: u:n idx:n :}
   u idx LIB-LEN-OFF LIB-FIELD ! ;

: LIB-HANDLE@ ( n -- n )
   {: idx:n :}
   idx LIB-HANDLE-OFF LIB-FIELD @ ;

: LIB-HANDLE! ( n n -- )
   {: value:n idx:n :}
   value idx LIB-HANDLE-OFF LIB-FIELD ! ;

: LIB-FAILED? ( n -- bool )
   {: idx:n :}
   idx LIB-FAILED-OFF LIB-FIELD @ 0 <> ;

: LIB-FAILED! ( n n -- )
   {: value:n idx:n :}
   value idx LIB-FAILED-OFF LIB-FIELD ! ;

: LIB-NEXT@ ( n -- n )
   {: idx:n :}
   idx LIB-NEXT-OFF LIB-FIELD @ ;

: LIB-NEXT! ( n n -- )
   {: value:n idx:n :}
   value idx LIB-NEXT-OFF LIB-FIELD ! ;

public
\ The handle FFI:DLSYM takes for the process's own symbols.
: PROCESS-HANDLE ( -- n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if 0 exit then
   HB-TARGET-MACOS? if -2 exit then
   E-FFI-LIBRARY throw ;

private
\ Library index idx's path, as LIBRARY-PATH and LIB-INDEX number it (from 1).
: LIB-ROW@ ( n -- str ) 1- {: row:n :} row LIB-PATH row LIB-LEN@ ;

\ A load failure reports when a call first resolves its symbol, in any task:
\ one write of a record built in the caller's own bytes.
: PUT ( ^u8 n str -- ^u8 n )
   {: line:^ at:n s:^ su:n :}
   s line at + su BYTE-COPY
   line at su + ;

: HELP-LEAD$ ( -- str ) s\" \n  = help: " ;

\ The help line h ends a record; HELP-LEN is its length.
: HELP-PUT ( ^u8 n str -- str )
   {: line:^ at:n h:^ hu:n :}
   line at HELP-LEAD$ PUT h hu PUT s\" \n" PUT ;

: HELP-LEN ( n -- n ) HELP-LEAD$ nip + 1+ ;

: RECORD-BODY ( str str str str str str ^u8 NUM:alloc-byte-len -- )
   {: a:^ au:n b:^ bu:n c:^ cu:n d:^ du:n e:^ eu:n h:^ hu:n line:^ extent:NUM:alloc-byte-len :}
   line 0 a au PUT b bu PUT c cu PUT d du PUT e eu PUT
   h hu HELP-PUT ERROR-TEXT:TYPE ;

\ A record whose header line is the parts a to e, then the help h.
: REPORT ( str str str str str str -- )
   {: a:^ au:n b:^ bu:n c:^ cu:n d:^ du:n e:^ eu:n h:^ hu:n :}
   a au b bu c cu d du e eu h hu
   au bu + cu + du + eu + hu HELP-LEN + MEM:BYTES-ALLOC-LEN
   [: RECORD-BODY ;] MEM:WITH-BYTES ;

: OPEN-HEAD$ ( -- str )
   s" error[E-FFI-DLOPEN]: cannot open library '" ;

: NOTE$ ( -- str ) s\" \n  = note: tried '" ;

: OPEN-HELP$ ( -- str )
   s" install the library, or name a path the loader finds" ;

\ why, the loader's reason, goes on tried's note only: a later loader call
\ replaces it, and dlerror's NULL means it kept none.
: REASON$ ( n n n -- str str )
   {: why:n tried:n lib:n :}
   why 0 <> lib tried = and if s" : " why CELL>PTR dup ZLEN exit then
   s" " s" " ;

: NOTE-LEN ( n n n -- n )
   {: why:n tried:n lib:n :}
   why tried lib REASON$ {: s:^ su:n r:^ ru:n :}
   NOTE$ nip lib LIB-ROW@ nip + 1+ su + ru + ;

\ The notes' length from lib along next.
: NOTES-LEN ( n n n -- n )
   {: why:n tried:n lib:n :}
   lib 0= if 0 exit then
   why tried lib NOTE-LEN
   why tried lib 1 - LIB-NEXT@ RECURSE + ;

: NOTE-PUT ( ^u8 n n n n -- str )
   {: line:^ at:n why:n tried:n lib:n :}
   why tried lib REASON$ {: s:^ su:n r:^ ru:n :}
   line at NOTE$ PUT lib LIB-ROW@ PUT s" '" PUT s su PUT r ru PUT ;

\ One note per library from lib along next.
: NOTES-PUT ( ^u8 n n n n -- str )
   {: line:^ at:n why:n tried:n lib:n :}
   lib 0= if line at exit then
   line at why tried lib NOTE-PUT
   why tried lib 1 - LIB-NEXT@ RECURSE ;

: OPEN-BODY ( n n n ^u8 NUM:alloc-byte-len -- )
   {: why:n tried:n lib:n line:^ extent:NUM:alloc-byte-len :}
   line 0 OPEN-HEAD$ PUT lib LIB-ROW@ PUT s" '" PUT
   why tried lib NOTES-PUT OPEN-HELP$ HELP-PUT
   ERROR-TEXT:TYPE ;

\ lib heads the chain the record names.
: OPEN-REPORT ( n n n -- )
   {: dlerr:n tried:n lib:n :}
   FFI-BUF 0 dlerr ffi-call-n {: why:n :}
   why tried lib
   OPEN-HEAD$ nip lib LIB-ROW@ nip + 1+
   why tried lib NOTES-LEN + OPEN-HELP$ nip HELP-LEN + MEM:BYTES-ALLOC-LEN
   [: OPEN-BODY ;] MEM:WITH-BYTES ;

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

: SYMBOL-HEAD$ ( -- str )
   s" error[E-FFI-DLSYM]: no C symbol '" ;

: PROCESS-HELP$ ( -- str )
   s" name the library that defines it with LIBRARY, or open it with FFI:PROCESS-WIDE" ;

: LIBRARY-HELP$ ( -- str )
   s" check the symbol's spelling against the library's exports" ;

\ lib 0 is the process's own symbols.
: SYMBOL-REPORT ( n n -- ) {: idx:n lib:n :}
   SYMBOL-HEAD$ idx FN-NAME idx NAME-LEN
   lib 0= if
      s" ' among the process's symbols" s" " s" " PROCESS-HELP$
      REPORT exit
   then
   s" ' in '" lib LIB-ROW@ s" '" LIBRARY-HELP$ REPORT ;

variable ERRNO-FN-CELL
variable DLERROR-FN-CELL

: ERRNO-FN ( -- n ) ERRNO-FN-CELL @ ;
: DLERROR-FN ( -- n ) DLERROR-FN-CELL @ ;

\ The row's address, resolved through its library at its first call.
: FN-RESOLVE ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   idx FN-LIB@ {: lib:n :}
   lib 0 <> if DLERROR-FN RECURSE else 0 then lib LIB-RESOLVE {: handle:n held:n :}
   handle idx FN-NAME DLSYM-RAW
   dup 0= if idx held SYMBOL-REPORT E-FFI-DLSYM throw then
   dup idx FN-ADDR! ;

\ The errno accessor resolves first, so a lookup gives back errno as the caller
\ left it: the loader may change it even when it succeeds.
: FN-LOOKUP ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   idx ERRNO-FN = if idx FN-RESOLVE exit then
   FFI-BUF 0 ERRNO-FN FN-RESOLVE ffi-call-n CELL>PTR {: cell:^ :}
   cell LE:U32@ {: saved:n :}
   idx FN-RESOLVE {: fn:n :}
   saved cell LE:U32!
   fn ;

\ The row's address, its cached cell read in line, as a call reads it every
\ time; FN-LOOKUP resolves it only while the cell is 0.
: CALLEE ( n -- n ) {: idx:n :}
   FN-TABLE @ idx FN-ROW-BYTES * + FN-ADDR-OFF + CELL-VIEW @
   dup 0 <> if exit then
   drop idx FN-LOOKUP ;

public

: NOW ( -- n ) FFI-RTLD-NOW ;

\ FFI:NOW alone is RTLD_LOCAL on Linux, so PROCESS-SYMBOLS cannot see the
\ library; `FFI:NOW FFI:PROCESS-WIDE or` joins its symbols to RTLD_DEFAULT.
\ macOS opens global either way. <dlfcn.h> spells RTLD_GLOBAL 0x100 in glibc,
\ 0x8 on macOS.
: PROCESS-WIDE ( -- n ) HB-TARGET-MACOS? if $8 else $100 then ;

: >CELL ( ^a -- n ) PTR>CELL ;

\ FFI is the only library code that writes a terminator.
private
\ A 0 byte among the u bytes at b would end the C string early.
: NUL-CHECK ( str -- )
   {: b u :}
   u 0 ?do b i + c@ 0= if E-FFI-NUL throw then loop ;

\ The span's base once u bytes and their 0 fit in it: u < reach, which cannot
\ overflow as u+1 can.
: CSTR-FIT ( n SPAN:span<u8> -- ^u8 )
   {: u s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ {: b l :}
   u l >= if E-SPAN-CAPACITY throw then
   b ;

public
: CSTR-ROOM ( n -- n ) 1+ ;

: CSTR! ( n SPAN:span<u8> -- ^u8 )
   {: u s :}
   u s CSTR-FIT {: b :}
   b u NUL-CHECK
   0 b u + c!
   b ;

\ Every rejection comes before a byte is written, so a rejected destination keeps
\ what it held.
: CSTR ( str SPAN:span<u8> -- ^u8 )
   {: src u s :}
   u s CSTR-FIT {: b :}
   src u NUL-CHECK
   src u s SPAN:COPY
   0 b u + c!
   b ;

private

\ ?TARGET, at the head of this file, admits only macOS and the Linux kernel.
: ERRNO-SYMBOL$ ( -- str )
   HB-TARGET-MACOS? if s" __error" exit then
   s" __errno_location" ;

public

: DLOPEN ( ^u8 n -- n ) DLOPEN-RAW ;
: DLSYM ( n ^u8 -- n ) DLSYM-RAW ;

\ The shared C thunk every callback stub branches to: an immutable code address,
\ never a Habu xt.
: CALLBACK-ENTRY ( -- n ) callback-entry ;

private

\ ---- the declared-function surface ----------------------------------------

\ The cells a declaration's stack arguments and private record copies share.
: STACK-CELLS ( -- n ) FFI-MAX-ARGS ;

\ Render into the caller's CODEGEN buffer; the caller owns the returned span.
: LIBRARY-NAME$ ( str n ^n -- str ) {: base:^ u:n version:n buf:^ :}
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

: PROCESS ( -- n ) 0 ;

: LIB-INDEX ( str n -- n )
   {: path:^ u:n next:n :}
   LIB-N @ 0 ?do
      i LIB-LEN@ u = i LIB-NEXT@ next = and if
         path u i LIB-PATH u STR:= if i 1+ unloop exit then
      then
   loop
   0 ;

: LIBRARY-PATH-OR ( str n -- n )
   {: path:^ u:n next:n :}
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

: LIBRARY-PATH ( str -- n ) 0 LIBRARY-PATH-OR ;

\ CALL-k ( x1 .. xk n -- n ): row n's function called with the k integer
\ arguments x1..xk, x1 the first, from the data stack. A FUNCTION: word
\ calls these, never ffi-call-k, which is undefined outside FFI.
: CALL-0 ( n -- n ) CALLEE ffi-call-0 ;
: CALL-1 ( n n -- n ) CALLEE ffi-call-1 ;
: CALL-2 ( n n n -- n ) CALLEE ffi-call-2 ;
: CALL-3 ( n n n n -- n ) CALLEE ffi-call-3 ;
: CALL-4 ( n n n n n -- n ) CALLEE ffi-call-4 ;
: CALL-5 ( n n n n n n -- n ) CALLEE ffi-call-5 ;
: CALL-6 ( n n n n n n n -- n ) CALLEE ffi-call-6 ;
: CALL-7 ( n n n n n n n n -- n ) CALLEE ffi-call-7 ;
: CALL-8 ( n n n n n n n n n -- n ) CALLEE ffi-call-8 ;

\ Row idx's function called through the full ABI, with spill stack cells;
\ spill is the row's own count, compiled in by the declaration.
: CALL-ABI ( n n -- n ) {: idx:n spill:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF spill idx CALLEE ffi-call-abi ;

: CALL-ABI-PTR ( n n -- ^u8 ) CALL-ABI CELL>PTR ;

: CALL-ABI-R ( n n -- r ) {: idx:n spill:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF spill idx CALLEE ffi-call-abi-r ;

: CALL-ABI-F32 ( n n -- r )
   CALL-ABI-R IEEE754:F64>BITS $FFFFFFFF and F32:WIDEN ;

\ A struct-returning declaration resolves its row before staging: a first-call
\ dlopen can run constructors that call C through this task's scratch.
: RESOLVE-ROW ( n -- ) FN-LOOKUP drop ;

\ The two integer result registers, in address order.
: CALL-STRUCT ( n -- n n ) {: idx:n :}
   idx FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF idx FN-SPILL@ idx FN-RELOC@ fn ffi-call-struct ;

: CALL-STRUCT-PTR ( n -- ^u8 ) CALL-STRUCT drop CELL>PTR ;

: REG-STORE ( n str -- ) {: v:n dst:^ len:n :}
   len 0 ?do v i 8 * rshift $FF and dst i + c! loop ;

\ A by-value record is read at exactly its width: cell k holds bytes
\ 8k..8k+7, little-endian, and the last cell's bytes past len are zero.
: RECORD-CELL ( str n -- n ) {: src:^ len:n k:n :}
   0 len k 8 * - 8 min 0 ?do src k 8 * + i + c@ i 8 * lshift or loop ;

: RECORD-WORDS ( n -- n ) 1- 8 / 1+ ;

: RECORD-COPY ( str n -- n )
   {: src:^ len:n pay:n :}
   len RECORD-WORDS 0 ?do
      src len i RECORD-CELL pay i + cells STACK-VALUE!
   loop
   pay cells FFI-STACK-SLOT PTR>CELL ;

\ STRUCT-ARG staging. A record of 16 bytes or fewer goes whole in consecutive
\ integer registers from reg, or in stack cells from byte offset pos, and so
\ does a larger one on SysV, always in stack cells. A larger one on AAPCS64 is
\ copied into stack cells from cell pay and passed by address in register reg
\ or the stack cell at byte offset pos.
: STRUCT-REGS! ( str n -- ) {: src:^ len:n reg:n :}
   len RECORD-WORDS 0 ?do src len i RECORD-CELL reg i + VALUE! loop ;

: STRUCT-STACK! ( str n -- )
   {: src:^ len:n pos:n :}
   len RECORD-WORDS 0 ?do
      src len i RECORD-CELL pos i cells + STACK-VALUE!
   loop ;

: STRUCT-COPY-REG! ( str n n -- ) {: src:^ len:n pay:n reg:n :}
   src len pay RECORD-COPY reg VALUE! ;

: STRUCT-COPY-STACK! ( str n n -- ) {: src:^ len:n pay:n pos:n :}
   src len pay RECORD-COPY pos STACK-VALUE! ;

\ A record of len bytes returned in two registers lands at exactly its width;
\ one larger than 16 bytes is already in place through the hidden pointer.
: STORE-RESULT ( n n n ^u8 -- ) {: lo:n hi:n len:n dst:^ :}
   len 16 > if exit then
   lo dst len 8 min REG-STORE
   len 8 > if hi dst 8 + len 8 - REG-STORE then ;

\ Whether row idx is the declaration of name in library lib with spills stack
\ cells and relocation mask reloc, a row's identity: no call reads an
\ argument count from its row. The stored symbol's length decides along with
\ its bytes, so neither a prefix nor a name holding a 0 byte matches a row.
: SAME-FN? ( str n n n n -- bool )
   {: name:^ u:n lib:n spills:n reloc:n idx:n :}
   idx FN-LIB@ lib <> if false exit then
   idx FN-SPILL@ spills <> if false exit then
   idx FN-RELOC@ reloc <> if false exit then
   idx NAME-LEN u <> if false exit then
   name u idx FN-NAME u STR:= ;

\ The symbol and its 0 byte are allotted where the declaration is accepted and
\ never move; the row's pointer cell reaches them across a save or a link.
: NAME-STORE ( str n -- ) {: name:^ u:n idx:n :}
   name u NUL-CHECK
   here BYTE-VIEW {: b:^ :}
   u 1+ allot
   name b u BYTE-COPY
   0 b u + c!
   b idx NAME-CELL !
   u idx FN-NAME-LEN-OFF FN-FIELD ! ;

\ The row of name in library lib with spills and reloc, or -1 for none.
: FN-INDEX ( str n n n -- n )
   {: name:^ u:n lib:n spills:n reloc:n :}
   FN-N @ 0 ?do
      name u lib spills reloc i SAME-FN? if i unloop exit then
   loop
   -1 ;

\ The row of name in library lib with spills and reloc: the existing one, else
\ a new unresolved row.
: DECLARE-ROW ( str n n n -- n )
   {: name:^ u:n lib:n spills:n reloc:n :}
   name u lib spills reloc FN-INDEX dup 0 >= if exit then drop
   FN-NEW
   FN-N @ {: idx:n :}
   name u idx NAME-STORE
   FN-N @ 1+ FN-N !
   spills idx FN-SPILL-OFF FN-FIELD !
   reloc idx FN-RELOC-OFF FN-FIELD !
   lib idx FN-LIB-OFF FN-FIELD !
   0 idx FN-ADDR!
   idx ;

public
\ n as the C int a foreign call reads or answers: its low 32 bits, sign-extended.
: >I32 ( n -- n )
   $FFFFFFFF and $80000000 xor $80000000 - ;

\ errno is this package's own binding, written by hand because the declarer is
\ defined below it, in this package. libc's location is thread-local and the
\ value is a four-byte C int.
: ERRNO ( -- n )
   ERRNO-FN CALL-0 CELL>PTR LE:U32@ ;

\ Sets errno to 0, for a call whose failure only errno tells.
: ERRNO-CLEAR ( -- )
   0 ERRNO-FN CALL-0 CELL>PTR LE:U32! ;

ERRNO-SYMBOL$ PROCESS 0 0 DECLARE-ROW ERRNO-FN-CELL !

s" dlerror" PROCESS 0 0 DECLARE-ROW DLERROR-FN-CELL !


\ FUNCTION: - declare a foreign function, get a checked word.
\
\     VERSIONED-LIBRARY z 1,2                 \ libz.so.1, else libz.so.2 (.1.dylib, else .2.dylib on macOS)
\     FUNCTION: CRC-CALL crc32 ( n str -- n ) ;FUNCTION
\     LIBRARY /usr/lib/libSystem.B.dylib      \ a literal, or PROCESS-SYMBOLS (RTLD_DEFAULT)
\     FUNCTION: SQRT-CALL sqrt ( r -- r ) ;FUNCTION
\     FUNCTION: LOCAL-CALL getsockname ( i32 ^u8 ^u8 -- i32 ) ;FUNCTION
\
\ The Habu word name and the C symbol are separate tokens. A symbol is often not
\ a legal Habu name (__errno_location).
\
\ The declared effect becomes the generated word's effect, with `f32` mapped
\ to Habu `r` and `i32` to `n` on either side. A `str` input is two C
\ arguments, its base then its byte length as a C size_t; a clause names an
\ input by its place in the effect. The clauses STRUCT-ARG, STRUCT-RESULT and
\ VARIADIC are ordinary words the interpreter runs between the two keywords,
\ so their numbers are literals and comments sit among them.
\ Convert a nominal at the call site: a declaration speaks the C function's own
\ types.
\
\ VERSIONED-LIBRARY, LIBRARY or PROCESS-SYMBOLS selects the library for the
\ declarations that follow. VERSIONED-LIBRARY takes a base name and a
\ comma-separated list of soname versions, renders the spelling this target
\ loads for each, and the first call opens them in the order written until one
\ opens.
\ LIBRARY takes one literal, an absolute path or an unversioned name, and
\ rejects one spelled in the other target's convention. THE SELECTION BELONGS TO
\ THE SCOPE THAT STATES IT: the package section or global scope the declarations
\ land in. There is no default: a declaration with no selection in its own scope
\ is E-FFI-LIBRARY, so a file cannot inherit a library an earlier file selected
\ and resolve its symbols through that handle's dependency tree.
\
\ The result is the call's, at the C prototype's width: `n` (or a widening
\ token) for a cell-wide integer (`long`, `size_t`, `ssize_t`, `off_t`, an
\ address kept as a number), `i32` for a C `int` (an enum, `pid_t`,
\ `kern_return_t`), `u32` for an `unsigned int` (`mach_port_t`), `r` for C
\ double, `f32` for C float, `^u8` for a foreign-owned address, or nothing,
\ which drops the return cell. A 32-bit result fills only the low half of the
\ return register and neither AAPCS64 nor SysV defines the high half, so a C
\ `int` declared `n` can read -1 as $FFFFFFFF. `i32` sign-extends the low half
\ and reads `n` to the checker, which has no signed 32-bit type; `u32` masks it
\ and keeps its own type.
\
\ `bytes STRUCT-RESULT` declares a C struct of `bytes`, any positive count,
\ returned by value. The last input is a `^u8` to caller-owned storage of at
\ least `bytes` that must not alias memory the callee reaches; it is no C
\ argument, and the output row is empty. AAPCS64 permits integer, pointer or
\ mixed members, excluding an HFA (one to four members of one float type) or
\ an HVA (one to four 8- or 16-byte vectors), which return in v0-v3.
\ SysV permits naturally aligned
\ integer and pointer members with alignment at most 8, excluding floating,
\ packed and over-aligned records. A byte count cannot establish these classes.
\ Up to 8 bytes return in x0 or rax and 9..16 in x0/x1 or rax/rdx, stored at
\ exactly `bytes`. C writes a larger record through x8 or rdi, so its storage
\ is aligned as the C record is. A `create … allot` block is cell-aligned;
\ a record holding a 16-byte aligned member needs 16. A record of 16 bytes or
\ fewer is stored a byte at a time, at any address.
\ The destination counts toward the sixteen inputs, a str counting two, so at
\ most fifteen C arguments remain, placed as STRUCT-ARG's are.
\
\ `idx bytes STRUCT-ARG` declares input idx, a `^u8`, the address of a C
\ struct of `bytes`, any positive count, passed by value: one C argument,
\ read at exactly `bytes` when the call is staged, so the callee never sees
\ the source. The record has the same member restrictions as STRUCT-RESULT.
\ A struct call places its other inputs as any call does. The stack cells,
\ copies and alignment padding share sixteen cells, rejected past that at
\ ;FUNCTION (E-FFI-ARITY). AAPCS64 also excludes, because a byte count cannot
\ reveal them, records aligned beyond 16 bytes and 16-byte aligned ones of 16
\ bytes or fewer: Apple arm64 may start such a record in an odd register like
\ any other, but on the stack it starts at a 16-byte aligned slot, so after
\ nine longs an ordinary 16-byte record is at SP+8 and a 16-byte aligned one at
\ SP+16. It combines with STRUCT-RESULT:
\     FUNCTION: DEPARSE pg_query_deparse_protobuf ( ^u8 ^u8 -- )
\        0 16 STRUCT-ARG  16 STRUCT-RESULT
\     ;FUNCTION
\
\ A float result reads only the low 32 bits of its FP slot and widens to Habu
\ `r`. Variadic positions use C default promotions, so declare a promoted float
\ as `r`, never `f32`.
\ A scalar input past its kind's registers takes the next 8-byte stack slot,
\ the value in its low bytes; Apple arm64 packs a fixed one at its C width and
\ alignment and puts every variadic input in a slot.
\
\ ;FUNCTION builds the declared word with define-word: its
\ effect from the types read where the declaration stands, its body from
\ literals and the words found here, which no word of the declaring package
\ can capture.

private

\ Each C argument but a record takes at most one 8-byte stack slot, so ARG-MAX
\ arguments fit FFI's stack buffer.
FFI-MAX-ARGS constant ARG-MAX
\ The longest type text IN-TOKEN or OUT-TOKEN records, nullable<u8>, fits.
$10 constant TYPE-CAP
6 constant X86-INT-REG-MAX                \ SysV rdi, rsi, rdx, rcx, r8, r9

\ An input's kind: a cell, a `^u8`, an `r` or an `f32`.
0 constant K-VALUE
1 constant K-POINTER
2 constant K-FLOAT
3 constant K-FLOAT32

\ An input type's C size in bytes.
: TYPE-SIZE ( str -- n )
   {: a:^ u:n :}
   a u s" u8" STR:= a u s" char" STR:= or if 1 exit then
   a u s" u16" STR:= if 2 exit then
   a u s" u32" STR:= a u s" i32" STR:= or a u s" f32" STR:= or if 4 exit then
   8 ;

0 constant R-NONE
1 constant R-VALUE
2 constant R-FLOAT
3 constant R-POINTER
4 constant R-I32
5 constant R-U32
6 constant R-FLOAT32
7 constant R-U16
8 constant R-U8

\ C-REG: every argument in an integer register, the result not a float.
0 constant C-REG
1 constant C-ABI
2 constant C-ABI-R
3 constant C-STRUCT

CODEGEN:BUFFER LIB-NAME

\ One declaration's working state: a declaration word writes each cell before
\ it reads it. Outside the open state every cell rests at zero and every buffer
\ holds nothing, and the token cells and buffers rest outside every declaration
\ word. Each word that can write one keeps this on every exit, a throw included
\ (KEEP-REST); a throw while a declaration stays open keeps that declaration. A
\ buffer grows to the token it holds.
DYNAMIC-BUFFER NAME-BUF u8
DYNAMIC-BUFFER SYM-BUF u8
DYNAMIC-BUFFER TOK-BUF u8
DYNAMIC-BUFFER BASE-BUF u8
variable NAME-U
variable SYM-U
variable TOK-U
variable BASE-U

create ARG-KIND ARG-MAX cells allot
create ARG-WIDTH ARG-MAX cells allot          \ each input's C width in bytes
create ARG-REG ARG-MAX cells allot
create ARG-SBYTES ARG-MAX cells allot         \ STRUCT-ARG's record width; 0 when none
create ARG-PAY ARG-MAX cells allot            \ planned: a large record's first copy cell
\ Each effect input's type as DECLARED-EFFECT reads it, a str's at its base;
\ empty at a str's length, which the effect does not name.
create ARG-TEXT ARG-MAX TYPE-CAP * allot
create ARG-TEXT-U ARG-MAX cells allot
variable ARG-N
variable RES-KIND
variable RES-NULLABLE                      \ 1 for a nullable<u8> result
variable VAR-START
variable STRUCT-BYTES                      \ STRUCT-RESULT's record width; 0 when none
create RES-TEXT TYPE-CAP allot             \ the result's type as DECLARED-EFFECT reads it
variable RES-TEXT-U
variable CUR-LIB
\ skipped: the driver cannot tell what the declaration holds, so its clauses
\ are ignored and it closes without a plan. The load never skips or leaves.
\ left: the run decides how it closes, from source the driver cannot read.
ENUM decl-state closed open skipped left ;ENUM
TYPED-VARIABLE DECL decl-state
PTR-VARIABLE OPENER-A
variable OPENER-U
: CLOSED! ( -- )
   construct decl-state closed DECL !  NULL-PTR OPENER-A !  0 OPENER-U ! ;
CLOSED!
SCRATCH-VARIABLE SCOPE-WID   \ the wordlist LIBRARY/PROCESS-SYMBOLS was stated in
SCRATCH-VARIABLE SCOPE-SET

: OPEN? ( -- bool )
   DECL @ MATCH decl-state
      closed OF false ENDOF
      open OF true ENDOF
      skipped OF false ENDOF
      left OF false ENDOF
   ;MATCH ;

\ The token cells a library selection shares with a declaration, at rest.
: REST-TOKENS ( -- )
   TOK-BUF-RELEASE BASE-BUF-RELEASE
   0 TOK-U ! 0 BASE-U ! ;

: REST ( -- )
   NAME-BUF-RELEASE SYM-BUF-RELEASE
   TYPE-CAP 0 ?do 0 RES-TEXT i + c! loop
   0 NAME-U ! 0 SYM-U ! 0 RES-TEXT-U !
   REST-TOKENS
   ARG-MAX TYPE-CAP * 0 ?do 0 ARG-TEXT i + c! loop
   ARG-MAX 0 ?do
      0 ARG-KIND i cells + ! 0 ARG-WIDTH i cells + !
      0 ARG-REG i cells + !
      0 ARG-SBYTES i cells + ! 0 ARG-PAY i cells + !
      0 ARG-TEXT-U i cells + !
   loop
   0 ARG-N ! 0 RES-KIND ! 0 RES-NULLABLE !
   0 VAR-START ! 0 STRUCT-BYTES ! ;

: KEEP-REST ( R [ R -- S ] -- S )
   [: OPEN? if REST-TOKENS else REST then ;] finally ;

\ The loader runs this on a declaration still open when its file ends.
: DISCARD ( -- )
   CLOSED!  REST ;

\ ---- a bad declaration's rejection ----------------------------------------
\ A rejection is SYNTAX-OPEN or ARITY-OPEN, its message, then HELP-STOP with the
\ fix, which places the record at the token read last. The stop abandons the
\ open declaration, so the next FUNCTION: in the file is not rejected for it.
: STOP-REJECTION ( -- )
   OPEN? if CLOSED then  CLOSED!
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: HELP-END ( str -- )
   {: h:^ hu:n :}
   REJECTION:HELP  h hu REJECTION:EMIT-STR  STOP-REJECTION ;

public

: SYNTAX-OPEN ( -- )
   s" E-FFI-SYNTAX" REJECTION:OPEN ;

: ARITY-OPEN ( -- )
   s" E-FFI-ARITY" REJECTION:OPEN ;

: HELP-STOP ( str -- )
   DEFINER-PLACE HELP-END ;

: SYNTAX-REJECT ( str str -- )
   {: m:^ mu:n h:^ hu:n :}
   SYNTAX-OPEN  m mu REJECTION:EMIT-STR  h hu HELP-STOP ;

private

\ ---- the declaration token stream -----------------------------------------
\ Tokens come from the caller's source, and a span lasts while the source's
\ line is read, so every token is copied as it is read and every comparison
\ reads the copy. A declaration's effect is on its line, so its types are
\ read from their spans.

: TOK$ ( -- str )
   0 TOK-BUF TOK-U @ ;

: TOK! ( str -- ) {: a:^ u:n :}
   u 0= if
      s" the line ended before the declaration did"
      s" write FUNCTION: NAME symbol ( inputs -- output ) on one line"
      SYNTAX-REJECT
   then
   u TOK-BUF-RESERVE
   a 0 TOK-BUF u BYTE-COPY
   u TOK-U ! ;

: TOK@ ( [ -- str ] -- )
   execute TOK! ;

: TOK-SPAN ( [ -- str ] -- str )
   execute 2dup TOK! ;

: TOK-IS? ( str -- bool )
   TOK$ STR:= ;

: NAME! ( -- )
   TOK$ {: a:^ u:n :}
   u NAME-BUF-RESERVE
   a 0 NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: SYM! ( -- )
   TOK$ {: a:^ u:n :}
   u SYM-BUF-RESERVE
   a 0 SYM-BUF u BYTE-COPY
   u SYM-U ! ;

\ The base's length before a target's suffix in it: `.dylib` or `.so` at its
\ end, else `.so.` within it; its whole length when it holds none.
: SUFFIX-CUT ( str -- n ) {: a:^ u:n :}
   a u s" .dylib" STR:ENDS-WITH? if u 6 - exit then
   a u s" .so" STR:ENDS-WITH? if u 3 - exit then
   a u s" .so." STR:FIND-SUB MATCH option
      none OF u ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ The base the source spells t tu is copied out of the token buffer because
\ reading the version token overwrites it.
: BASE! ( str -- ) {: t:^ tu:n :}
   TOK$ {: a:^ u:n :}
   a u SUFFIX-CUT {: cut:n :}
   cut u < if
      s" E-FFI-LIBRARY" REJECTION:OPEN
      s" the base `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
      s" ` is already spelled for a target" REJECTION:EMIT-STR
      t tu SRCLOC:TOKEN$ REJECTION:KEPT REJECTION:AT  s" a target's suffix" REJECTION:EMIT-STR
      REJECTION:HELP  s" give VERSIONED-LIBRARY the bare name, `" REJECTION:EMIT-STR
      a cut REJECTION:EMIT-STR  96 REJECTION:EMIT1  STOP-REJECTION
   then
   u BASE-BUF-RESERVE
   a 0 BASE-BUF u BYTE-COPY
   u BASE-U ! ;

: >VERSION ( str -- n )
   STR:PARSE-POS MATCH option
      none OF
         s" a library version is not a number"
         s" write versions as numbers split by commas: VERSIONED-LIBRARY z 1,2"
         SYNTAX-REJECT
      ENDOF
      some OF ENDOF
   ;MATCH ;

\ The name the base and version make, held in LIB-NAME until the next is made.
: VERSION-NAME$ ( n -- str ) {: version:n :}
   0 BASE-BUF BASE-U @ version LIB-NAME LIBRARY-NAME$ ;

\ Rejects the version's name, at the base the source spells t tu, when a
\ library path's field cannot hold it.
: CHECK-NAME ( str n -- ) {: t:^ tu:n version:n :}
   version VERSION-NAME$ {: name:^ nu:n :}
   nu LIB-PATH-CAP 1 - > if
      s" E-FFI-SYNTAX" s" the library name made from this base is longer than a path holds"
      t tu SRCLOC:TOKEN$ REJECTION:KEPT s" too long" REJECTION:OPEN-AT
      LIB-PATH-CAP 1 - s" bytes" REJECTION:LIMIT
      s" found" REJECTION:NOTE  nu REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" name" REJECTION:NOTE  name nu REJECTION:EMIT-STR
      s" shorten the base name, so `lib`, the base, the target's suffix and a version fit the limit"
      HELP-END
   then ;

\ The list's rows, made from the last version to the first; the answer is the
\ first version's row. Every name is checked before a row is added.
: VERSION-ROWS ( str str -- n )
   {: t:^ tu:n list:^ u:n :}
   list u [char] , STR:INDEX-OF MATCH option
      none OF u ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: cut:n :}
   list cut >VERSION {: version:n :}
   t tu version CHECK-NAME
   cut u = if 0 else t tu list cut + 1+ u cut - 1- RECURSE then {: next:n :}
   version VERSION-NAME$ next LIBRARY-PATH-OR ;

\ ---- the declared effect ---------------------------------------------------

: ARG-KIND@ ( n -- n ) {: i:n :} ARG-KIND i cells + @ ;
: ARG-WIDTH@ ( n -- n )
   {: i:n :}
   ARG-WIDTH i cells + @ ;
: ARG-REG@ ( n -- n ) {: i:n :} ARG-REG i cells + @ ;
: ARG-SBYTES@ ( n -- n ) {: i:n :} ARG-SBYTES i cells + @ ;
: ARG-SBYTES! ( n n -- ) {: v:n i:n :} v ARG-SBYTES i cells + ! ;
: ARG-PAY@ ( n -- n ) {: i:n :} ARG-PAY i cells + @ ;
: ARG-PAY! ( n n -- ) {: v:n i:n :} v ARG-PAY i cells + ! ;

: ARG-TEXT$ ( n -- str ) {: i:n :}
   ARG-TEXT i TYPE-CAP * +  ARG-TEXT-U i cells + @ ;

\ Records the type text of the input ARG+ added last.
: IN-TEXT ( str -- ) {: a:^ u:n :}
   ARG-N @ 1 - {: i:n :}
   a ARG-TEXT i TYPE-CAP * + u BYTE-COPY
   u ARG-TEXT-U i cells + ! ;

: RES-TEXT! ( str -- ) {: a:^ u:n :}
   a RES-TEXT u BYTE-COPY
   u RES-TEXT-U ! ;

: INPUTS-FULL ( -- )
   ARITY-OPEN
   s" a FUNCTION: declaration has at most " REJECTION:EMIT-STR
   ARG-MAX REJECTION:EMIT-U  s"  inputs, a str counting two" REJECTION:EMIT-STR
   s" call it through a C function that takes the inputs in a record"
   HELP-STOP ;

: ARG+ ( n n -- )
   {: kind:n width:n :}
   ARG-N @ ARG-MAX >= if INPUTS-FULL then
   kind ARG-KIND ARG-N @ cells + !
   width ARG-WIDTH ARG-N @ cells + !
   0 ARG-N @ ARG-SBYTES!
   ARG-N @ 1 + ARG-N ! ;

\ A pointer is `^u8`, a byte span: a token that opens with `^` names one.
: PTR-TOK? ( -- bool )  TOK$ 1 min s" ^" STR:= ;

\ Rejects a pointer token other than `^u8`.
: PTR-TAIL ( -- )
   s" ^u8" TOK-IS? 0= if
      s" expected 'u8' after '^'"
      s" declare a pointer as ^u8, a byte span, whatever C points at"
      SYNTAX-REJECT
   then ;

\ The integer types and addr an input or an output keeps as spelled.
: SCALAR? ( -- bool )
   s" n" TOK-IS? s" i64" TOK-IS? or s" cell" TOK-IS? or
   s" addr" TOK-IS? or s" u8" TOK-IS? or s" u16" TOK-IS? or
   s" u32" TOK-IS? or s" char" TOK-IS? or ;

\ A declaration spells the C prototype's types; a role, a bool, an enum or a
\ sum is Habu's, so the caller converts it.
: TYPE-REJECT ( str str -- )
   {: w:^ wu:n l:^ lu:n :}
   SYNTAX-OPEN  s" '" REJECTION:EMIT-STR  TOK$ REJECTION:EMIT-STR
   s" ' is not " REJECTION:EMIT-STR  w wu REJECTION:EMIT-STR
   s"  type: " REJECTION:EMIT-STR  w wu REJECTION:EMIT-STR
   s"  is " REJECTION:EMIT-STR  l lu REJECTION:EMIT-STR
   s" declare the C prototype's type and convert the value where the word is called"
   HELP-STOP ;

: IN-REJECT ( -- )
   s" an input"
   s" ^u8, str, f32, r, i32, n, i64, cell, addr, u8, u16, u32 or char"
   TYPE-REJECT ;

: OUT-REJECT ( -- )
   s" an output"
   s" ^u8, f32, i32, nullable<u8>, r, n, i64, cell, addr, u8, u16, u32 or char"
   TYPE-REJECT ;

\ Each type is read where it is written, so a rejected one names its token;
\ DECLARED-EFFECT reads the recorded text again.
: IN-TOKEN ( str -- )
   {: a:^ u:n :}
   a u TYPE-SIZE {: size:n :}
   PTR-TOK? if
      PTR-TAIL  a u EFFECT:READ-TYPE drop
      K-POINTER size ARG+ s" ^u8" IN-TEXT exit
   then
   s" str" TOK-IS? if
      a u EFFECT:READ-TYPE drop
      K-POINTER 8 ARG+ s" str" IN-TEXT
      K-VALUE 8 ARG+ s" " IN-TEXT exit
   then
   s" f32" TOK-IS? if
      K-FLOAT32 size ARG+ s" r" IN-TEXT exit
   then
   s" i32" TOK-IS? if
      K-VALUE size ARG+ s" n" IN-TEXT exit
   then
   s" r" TOK-IS? SCALAR? or 0= if IN-REJECT then
   a u EFFECT:READ-TYPE drop
   s" r" TOK-IS? if K-FLOAT else K-VALUE then size ARG+
   a u IN-TEXT ;

: SCALAR-KIND ( -- n )
   s" u32" TOK-IS? if R-U32 exit then
   s" u16" TOK-IS? if R-U16 exit then
   s" u8" TOK-IS? s" char" TOK-IS? or if R-U8 exit then
   R-VALUE ;

: OUT-TOKEN ( str -- )
   {: a:^ u:n :}
   RES-KIND @ R-NONE <> if
      s" a FUNCTION: declaration has at most one output"
      s" C returns one value: return the others through ^u8 inputs C writes"
      SYNTAX-REJECT
   then
   PTR-TOK? if
      PTR-TAIL  a u EFFECT:READ-TYPE drop
      s" ^u8" RES-TEXT! R-POINTER RES-KIND ! exit
   then
   s" f32" TOK-IS? if s" r" RES-TEXT! R-FLOAT32 RES-KIND ! exit then
   s" i32" TOK-IS? if s" n" RES-TEXT! R-I32 RES-KIND ! exit then
   s" nullable<u8>" TOK-IS? s" r" TOK-IS? or SCALAR? or 0= if
      OUT-REJECT
   then
   a u EFFECT:READ-TYPE drop  a u RES-TEXT!
   s" nullable<u8>" TOK-IS? if R-POINTER RES-KIND ! 1 RES-NULLABLE ! exit then
   s" r" TOK-IS? if R-FLOAT RES-KIND ! exit then
   SCALAR-KIND RES-KIND ! ;

: INPUTS ( [ -- str ] -- )
   {: src :}
   begin
      src TOK-SPAN
      s" --" TOK-IS? if 2drop exit then
      s" )" TOK-IS? if
         s" expected '--' before ')'"
         s" write the effect as ( inputs -- output ), '--' even with no output"
         SYNTAX-REJECT
      then
      IN-TOKEN
   again ;

: OUTPUTS ( [ -- str ] -- )
   {: src :}
   begin
      src TOK-SPAN
      s" )" TOK-IS? if 2drop exit then
      s" --" TOK-IS? if
         s" a second '--' in the effect"
         s" write one '--', between the inputs and the output"
         SYNTAX-REJECT
      then
      OUT-TOKEN
   again ;

: EFFECT ( [ -- str ] -- )
   {: src :}
   src TOK@
   s" (" TOK-IS? 0= if
      s" expected '(' after the C symbol"
      s" write FUNCTION: NAME symbol ( inputs -- output ) ;FUNCTION"
      SYNTAX-REJECT
   then
   src INPUTS
   src OUTPUTS ;

\ ---- which call the declaration rides --------------------------------------

: STAGING-FULL ( -- )
   ARITY-OPEN
   s" a struct call stages at most " REJECTION:EMIT-STR
   STACK-CELLS REJECTION:EMIT-U
   s"  cells of stack inputs and record copies" REJECTION:EMIT-STR
   s" pass the large records by address through a C function"
   HELP-STOP ;

: FLOAT-KIND? ( n -- bool )
   {: kind:n :}
   kind K-FLOAT = kind K-FLOAT32 = or ;

: ALIGN-UP ( n n -- n )
   {: v:n a:n :}
   v a + 1- a / a * ;

\ Whether input i is variadic when the variadic inputs start at start, -1
\ for none.
: VARIADIC? ( n n -- bool )
   {: i:n start:n :}
   start 0 >= i start >= and ;

: INT-REGS ( -- n )
   HB-TARGET-LINUX-X86-64? if X86-INT-REG-MAX exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if REG-MAX exit then
   E-FFI-LIBRARY throw ;

: RESULT-REGS ( n -- n )
   16 > HB-TARGET-LINUX-X86-64? and if 1 exit then
   0 ;

: SLOT-BYTES ( n bool -- n )
   {: size:n var:bool :}
   HB-TARGET-MACOS? var 0= and if size exit then
   8 ;

\ Places an input of bytes at the next stack offset aligned to them, at most
\ 8, and answers the offset past it and its place.
: SPILL-ARG ( n n n n -- n n n n )
   {: int:n flt:n off:n bytes:n :}
   off bytes 8 min ALIGN-UP {: at:n :}
   int flt at bytes + at 1+ negate ;

: PLACE-SCALAR ( n n n n n bool -- n n n n )
   {: int:n flt:n off:n kind:n size:n var:bool :}
   size var SLOT-BYTES {: bytes:n :}
   HB-TARGET-MACOS? var and if int flt off bytes SPILL-ARG exit then
   kind FLOAT-KIND? if
      flt REG-MAX < if int flt 1+ off flt exit then
   else
      int INT-REGS < if int 1+ flt off int exit then
   then
   int flt off bytes SPILL-ARG ;

\ Check remaining cells before multiplying a record's width, so a sum of
\ payloads cannot wrap into a plan that fits.
: SPILL-RECORD ( n n n n -- n n n n )
   {: int:n flt:n off:n words:n :}
   off 8 ALIGN-UP {: at:n :}
   STACK-CELLS at 8 / - words < if
      int flt STACK-CELLS 1+ cells at 1+ negate exit
   then
   int flt at words cells SPILL-ARG ;

: PLACE-RECORD ( n n n n -- n n n n )
   {: int:n flt:n off:n bytes:n :}
   bytes RECORD-WORDS {: words:n :}
   bytes 16 <= int words + INT-REGS <= and if
      int words + flt off int exit
   then
   HB-TARGET-LINUX-X86-64? if int else INT-REGS then
   flt off words SPILL-RECORD ;

\ The call rule: places one C argument of kind and C size, a record of sbytes
\ (0 for none), variadic or not, past int integer and flt float registers and
\ off stack bytes, and answers them past it and its place.
: PLACE-NEXT ( n n n n n n bool -- n n n n )
   {: int:n flt:n off:n kind:n size:n sbytes:n var:bool :}
   sbytes 0 > sbytes 16 <= HB-TARGET-LINUX-X86-64? or and if
      int flt off sbytes PLACE-RECORD exit
   then
   int flt off kind size var PLACE-SCALAR ;

\ Places argc inputs in argument order, after the registers a larger result
\ reserves: input i's kind, C size and record bytes at cell i of kinds, sizes
\ and sbytes, each place into regs. Answers the integer registers, float
\ registers and stack bytes taken.
: PLAN-IN-ORDER ( ^n ^n ^n n n n ^n -- n n n )
   {: kinds:^ sizes:^ sbytes:^ argc:n start:n result:n regs:^ :}
   result RESULT-REGS 0 0 argc 0 ?do
      kinds i cells + @ sizes i cells + @ sbytes i cells + @
      i start VARIADIC? PLACE-NEXT
      regs i cells + !
   loop ;

: PLACE-COPY ( n n -- n n ) {: pos:n bytes:n :}
   bytes 16 <= if pos 0 exit then
   pos 1+ 2 / 2 * {: pay:n :}
   \ A width past the capacity left makes the plan unfit at once, so no sum of
   \ payloads can wrap.
   STACK-CELLS pay - 8 * bytes < if STACK-CELLS 1+ pay exit then
   pay bytes RECORD-WORDS + pay ;

\ Places each larger record's copy from stack cell pos into pays, and answers
\ the cells past the last.
: PLAN-COPIES ( ^n n n ^n -- n )
   {: sbytes:^ argc:n pos:n pays:^ :}
   pos argc 0 ?do
      sbytes i cells + @ PLACE-COPY
      pays i cells + !
   loop ;

\ The plan of argc C arguments, variadic from start (-1 for none), into regs
\ and pays. Answers the float registers and the stack cells taken.
: PLAN-INPUTS ( ^n ^n ^n n n n ^n ^n -- n n )
   {: kinds:^ sizes:^ sbytes:^ argc:n start:n result:n regs:^ pays:^ :}
   kinds sizes sbytes argc start result regs PLAN-IN-ORDER rot drop {: flt:n off:n :}
   off 8 ALIGN-UP 8 / {: pos:n :}
   HB-TARGET-LINUX-X86-64? if flt pos exit then
   flt sbytes argc pos pays PLAN-COPIES ;

: PLAN-ROOM ( n -- )
   {: stk:n :}
   stk STACK-CELLS <= if exit then
   STAGING-FULL ;

: C-ARGS ( -- n )
   ARG-N @ STRUCT-BYTES @ 0 > if 1- then ;

: RECORDS? ( -- bool )
   false ARG-N @ 0 ?do i ARG-SBYTES@ 0 > or loop ;

: CALL-KIND ( n n -- n )
   {: flt:n stk:n :}
   RES-KIND @ R-FLOAT = RES-KIND @ R-FLOAT32 = or if C-ABI-R exit then
   stk 0 > flt 0 > or if C-ABI exit then
   C-REG ;

\ Answers the stack cells the call takes and the call it rides.
: PLAN-CALL ( -- n n )
   ARG-KIND ARG-WIDTH ARG-SBYTES C-ARGS VAR-START @ STRUCT-BYTES @ ARG-REG ARG-PAY
   PLAN-INPUTS {: flt:n stk:n :}
   STRUCT-BYTES @ 0 > RECORDS? or if
      stk PLAN-ROOM
      stk C-STRUCT exit
   then
   stk  flt stk CALL-KIND ;

\ ---- the built definition -------------------------------------------------

\ The type of argument I's local, by its kind.
: LOCAL-TYPE ( n -- EFFECT:type ) {: i:n :}
   i ARG-KIND@ K-VALUE = if EFFECT:N exit then
   i ARG-KIND@ K-POINTER = if s" u8" EFFECT:NAMED EFFECT:PTR exit then
   s" r" EFFECT:NAMED ;

\ One local per argument, in order; answers the
\ first one's index, argument I's being it + I.
: LOCALS ( -- n )
   s" {:" EFFECT:KEYWORD
   0 LOCAL-TYPE EFFECT:LOCAL
   ARG-N @ 1 ?do i LOCAL-TYPE EFFECT:LOCAL drop loop
   s" :}" EFFECT:KEYWORD ;

: STAGE-VALUE ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone VALUE! ;

: STAGE-FLOAT ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone FLOAT! ;

: STAGE-FLOAT32 ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone FLOAT32! ;

: STAGE-POINTER ( n -- ) {: i:n :}
   i ARG-REG@ literal postpone POINTER! ;

: STACK-POS ( n -- )
   {: i:n :}
   i ARG-REG@ negate 1- literal ;

\ C promotes a variadic integer narrower than an int, so its slot is written
\ whole.
: STAGE-INT-SPILL ( n -- )
   {: i:n :}
   i VAR-START @ VARIADIC? if postpone STACK-VALUE! exit then
   i ARG-WIDTH@ {: width:n :}
   width 1 = if postpone STACK-U8! exit then
   width 2 = if postpone STACK-U16! exit then
   width 4 = if postpone STACK-U32! exit then
   postpone STACK-VALUE! ;

: STAGE-SPILL ( n -- ) {: i:n :}
   i STACK-POS
   i ARG-KIND@ K-FLOAT = if postpone STACK-FLOAT! exit then
   i ARG-KIND@ K-FLOAT32 = if postpone STACK-FLOAT32! exit then
   i ARG-KIND@ K-POINTER = if postpone STACK-POINTER! exit then
   i STAGE-INT-SPILL ;

: STAGE-REG ( n -- ) {: i:n :}
   i ARG-KIND@ K-FLOAT = if i STAGE-FLOAT exit then
   i ARG-KIND@ K-FLOAT32 = if i STAGE-FLOAT32 exit then
   i ARG-KIND@ K-VALUE = if i STAGE-VALUE exit then
   i STAGE-POINTER ;

: STAGE-ONE ( n -- ) {: i:n :}
   i ARG-REG@ 0 < if i STAGE-SPILL exit then
   i STAGE-REG ;

: STAGE ( -- )
   ARG-N @ 0 ?do ARG-N @ i 1+ - STAGE-ONE loop ;

: CALL-WORD ( n -- )
   {: kind:n :}
   RES-KIND @ R-FLOAT32 = if postpone CALL-ABI-F32 exit then
   kind C-ABI-R = if postpone CALL-ABI-R exit then
   RES-KIND @ R-POINTER = if postpone CALL-ABI-PTR exit then
   postpone CALL-ABI ;

\ The deepest pointer argument's index, ARG-N when none is.
: FIRST-POINTER ( -- n )
   ARG-N @ 0 ?do
      i ARG-KIND@ K-POINTER = if i unloop exit then
   loop
   ARG-N @ ;

\ Leaves every argument a cell in place: the arguments from the deepest
\ pointer up are bound to locals and pushed back in order, each pointer as
\ its cell; those below it stay where they are, with no local.
: ARG-CELLS ( -- )
   FIRST-POINTER {: first:n :}
   first ARG-N @ = if exit then
   s" {:" EFFECT:KEYWORD
   first LOCAL-TYPE EFFECT:LOCAL {: base:n :}
   ARG-N @ first 1+ ?do i LOCAL-TYPE EFFECT:LOCAL drop loop
   s" :}" EFFECT:KEYWORD
   ARG-N @ first ?do
      i first - base + EFFECT:LOCAL@
      i ARG-KIND@ K-POINTER = if postpone PTR>CELL then
   loop ;

\ Appends CALL-k for argc arguments, at most INT-REGS in a C-REG call.
: REG-CALL-WORD ( n -- ) {: argc:n :}
   argc 0 = if postpone CALL-0 exit then
   argc 1 = if postpone CALL-1 exit then
   argc 2 = if postpone CALL-2 exit then
   argc 3 = if postpone CALL-3 exit then
   argc 4 = if postpone CALL-4 exit then
   argc 5 = if postpone CALL-5 exit then
   argc 6 = if postpone CALL-6 exit then
   argc 7 = if postpone CALL-7 exit then
   postpone CALL-8 ;

\ A C-REG call's body: its arguments as cells in order, then CALL-k of
\ row slot; a pointer result is the cell CELL>PTR retypes.
: REG-BODY ( n -- ) {: slot:n :}
   ARG-CELLS
   slot literal
   ARG-N @ REG-CALL-WORD
   RES-KIND @ R-POINTER = if postpone CELL>PTR then ;

\ ---- the row this declaration takes ----------------------------------------

\ A library name spelled in the other target's convention is rejected where it
\ is stated, not at the first call in whatever module declared it. A rendered
\ name passes by construction.
: TARGET-HELP$ ( -- str )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      s" name it with VERSIONED-LIBRARY, which spells it for linux" exit
   then
   HB-TARGET-MACOS? if
      s" name it with VERSIONED-LIBRARY, which spells it for macos" exit
   then
   E-FFI-LIBRARY throw ;

: SPELLED$ ( -- str ) s" ' is spelled for the other target" ;

: BASENAME ( str -- str ) {: path:^ u:n :}
   0 u 0 ?do path i + c@ [char] / = if drop i 1+ then loop
   {: start:n :} path start + u start - ;

: FOREIGN-SPELLING? ( str -- bool )
   BASENAME {: path:^ u:n :}
   HB-TARGET-MACOS? if
      path u s" .dylib" STR:ENDS-WITH? if false exit then
      path u s" .so" STR:ENDS-WITH? path u s" .so." STR:CONTAINS? or exit
   then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      path u s" .dylib" STR:ENDS-WITH? exit
   then
   E-FFI-LIBRARY throw ;

\ A path in the other target's spelling.
: SPELLING-REJECT ( str -- ) {: path:^ u:n :}
   s" E-FFI-LIBRARY" REJECTION:OPEN
   s" library '" REJECTION:EMIT-STR  path u REJECTION:EMIT-STR
   SPELLED$ REJECTION:EMIT-STR
   TARGET-HELP$ HELP-STOP ;

: CHECK-SPELLING ( str -- ) {: path:^ u:n :}
   path u FOREIGN-SPELLING? if path u SPELLING-REJECT then ;

\ A path stands where `LIBRARY` read it. One the source does not hold, which a
\ caller passed, stands at the token being run, and its bytes are not read.
: CHECK-PATH ( str -- ) {: path:^ u:n :}
   u 0 <= if
      s" E-FFI-SYNTAX" s" the library path is empty"
      SRCLOC:TOKEN$ s" expected a library path after this" REJECTION:OPEN-AT
      s" write the library's path or file name after LIBRARY, on its line"
      HELP-END
   then
   u LIB-PATH-CAP >= if
      SYNTAX-OPEN  s" the library path is longer than a path can be" REJECTION:EMIT-STR
      path u SRCLOC:TOKEN$ REJECTION:KEPT REJECTION:AT  s" too long" REJECTION:EMIT-STR
      LIB-PATH-CAP 1 - s" bytes" REJECTION:LIMIT
      s" found" REJECTION:NOTE  u REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
      s" name the library by its file name, which the loader searches for"
      HELP-END
   then
   path u CHECK-SPELLING ;


\ The cells holding a larger record's copy address, which the call moves to
\ the copy in its native frame: bit reg for x0-x7, bit 8+cell for a stack
\ cell. Each bit is one of this plan's own cells, so the engine never moves a
\ cell past the native frame it copies; no caller supplies a mask.
: RELOC-MASK ( -- n )
   HB-TARGET-LINUX-X86-64? if 0 exit then
   0 C-ARGS 0 ?do
      i ARG-SBYTES@ 16 > if
         i ARG-REG@ 0 < if 1 i ARG-REG@ negate 1- 8 / 8 + lshift
         else 1 i ARG-REG@ lshift then
         or
      then
   loop ;

\ The row of the declared symbol in the current library, with stk spilled
\ stack cells and this plan's relocation mask.
: SLOT ( n -- n )
   {: stk:n :}
   0 SYM-BUF SYM-U @ CUR-LIB @ stk RELOC-MASK DECLARE-ROW ;

\ A niche value's zero cell is none, so `some` of the returned cell reads NULL as none.
\ AAPCS64 and SysV leave the register bits above a u8, u16 or u32 result
\ unspecified.
: RESULT-TAIL ( -- )
   RES-NULLABLE @ 0<> if postpone NULLABLE:SOME exit then
   RES-KIND @ R-NONE = if postpone drop exit then
   RES-KIND @ R-I32 = if postpone >I32 exit then
   RES-KIND @ R-U32 = if $FFFFFFFF literal postpone and exit then
   RES-KIND @ R-U16 = if $FFFF literal postpone and exit then
   RES-KIND @ R-U8 = if $FF literal postpone and then ;

: STAGE-RECORD ( n -- ) {: i:n :}
   i ARG-SBYTES@ literal postpone >str
   i ARG-SBYTES@ 16 <= HB-TARGET-LINUX-X86-64? or if
      i ARG-REG@ 0 < if i STACK-POS postpone STRUCT-STACK! exit then
      i ARG-REG@ literal postpone STRUCT-REGS! exit
   then
   i ARG-PAY@ literal
   i ARG-REG@ 0 < if i STACK-POS postpone STRUCT-COPY-STACK! exit then
   i ARG-REG@ literal postpone STRUCT-COPY-REG! ;

: STAGE-C-ARG ( n n -- ) {: base:n i:n :}
   i base + EFFECT:LOCAL@
   i ARG-SBYTES@ 0 > if i STAGE-RECORD exit then
   i ARG-REG@ 0 < if i STAGE-SPILL exit then
   i STAGE-REG ;

\ The STRUCT-RESULT width, then its destination, the last argument.
: DEST ( n -- ) {: base:n :}
   STRUCT-BYTES @ literal
   ARG-N @ 1- base + EFFECT:LOCAL@ ;

: STAGE-DEST ( n -- ) {: base:n :}
   STRUCT-BYTES @ 16 <= if exit then
   ARG-N @ 1- base + EFFECT:LOCAL@
   HB-TARGET-LINUX-X86-64? if 0 else 8 then literal postpone POINTER! ;

: STRUCT-BODY ( n -- ) {: slot:n :}
   LOCALS {: base:n :}
   slot literal postpone RESOLVE-ROW
   C-ARGS 0 ?do base i STAGE-C-ARG loop
   base STAGE-DEST
   slot literal
   STRUCT-BYTES @ 0 > if
      postpone CALL-STRUCT base DEST postpone STORE-RESULT exit
   then
   RES-KIND @ R-POINTER = if
      postpone CALL-STRUCT-PTR RESULT-TAIL exit
   then
   postpone CALL-STRUCT postpone drop
   RESULT-TAIL ;

\ An effect node ends with the load that made it, so the effect is built here
\ from the recorded texts, never kept from the FUNCTION: line.
: DECLARED-EFFECT ( -- EFFECT:type )
   EFFECT:EMPTY
   ARG-N @ 0 ?do
      i ARG-TEXT$ dup 0 <> if EFFECT:READ-TYPE EFFECT:IN else 2drop then
   loop
   RES-KIND @ R-NONE = if exit then
   RES-TEXT RES-TEXT-U @ EFFECT:READ-TYPE EFFECT:OUT ;

\ Splits a str based at argument B into ^u8 n; a generated body takes no
\ >r, so the one-cell arguments above it wait in a `{:` group.
: SPLIT-ARG ( n -- ) {: b:n :}
   b ARG-TEXT$ s" str" STR:= 0= if exit then
   b 2 + {: top:n :}
   top ARG-N @ = if postpone str> exit then
   s" {:" EFFECT:KEYWORD
   top LOCAL-TYPE EFFECT:LOCAL {: base:n :}
   ARG-N @ top 1+ ?do i LOCAL-TYPE EFFECT:LOCAL drop loop
   s" :}" EFFECT:KEYWORD
   postpone str>
   ARG-N @ top - 0 ?do base i + EFFECT:LOCAL@ loop ;

\ The effect takes a str as one value and the stages after it take each
\ argument as one cell, so each str is split, the topmost first.
: SPLIT-STRS ( -- )
   ARG-N @ 0 ?do ARG-N @ i 1+ - SPLIT-ARG loop ;

\ A struct call reads its spill count and relocation mask from its row; a
\ staged call has stk, its spilled stack cells, compiled in.
: BODY ( n n n -- )
   {: stk:n kind:n slot:n :}
   SPLIT-STRS
   kind C-STRUCT = if slot STRUCT-BODY postpone ; exit then
   kind C-REG = if
      slot REG-BODY  RESULT-TAIL  postpone ; exit
   then
   STAGE
   slot literal
   stk literal
   kind CALL-WORD
   RESULT-TAIL
   postpone ; ;

: EMIT ( n n -- )
   {: stk:n kind:n :}
   stk SLOT {: slot:n :}
   0 NAME-BUF NAME-U @ DECLARED-EFFECT define-word
   stk kind slot BODY ;

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

\ The record is placed at the declaration's FUNCTION: token, which the
\ declaration was read past.
: SCOPE-REJECT ( str -- )
   {: m:^ mu:n :}
   OPENER-A @ OPENER-U @ SRCLOC:TOKEN!
   s" E-FFI-LIBRARY" REJECTION:OPEN  m mu REJECTION:EMIT-STR
   s" select one before it in its own package section, or at top level for a top-level declaration, with LIBRARY, VERSIONED-LIBRARY or PROCESS-SYMBOLS"
   HELP-STOP ;

: CHECK-SCOPE ( -- )
   SCOPE-SET @ 0= if s" FUNCTION: with no library selected" SCOPE-REJECT then
   get-current SCOPE-WID @ <> if
      s" the library selected last belongs to another package section or to top level"
      SCOPE-REJECT
   then ;

: CLOSE-STATE ( -- bool )
   DECL @ MATCH decl-state
      closed OF
         s" ;FUNCTION with no declaration open"
         s" open one with FUNCTION: NAME symbol ( inputs -- output )"
         SYNTAX-REJECT
      ENDOF
      open OF true ENDOF
      skipped OF false ENDOF
      left OF false ENDOF
   ;MATCH
   CLOSED! ;

: LEFT! ( -- )
   construct decl-state left DECL !
   NULL-PTR OPENER-A !  0 OPENER-U ! ;

\ The token that opened the declaration still open or skipped, as its reader
\ gave it; empty when none is.
: OPENER$ ( -- str )
   OPENER-A @ OPENER-U @ ;

\ The read-head transition from the FUNCTION: token at SRCLOC:TOKEN$: answers the
\ name token, which `named` gets once the declaration is read, before it opens.
: READ-FUNCTION ( [ -- str ] [ str -- ] -- str )
   {: src named :}
   SRCLOC:TOKEN$ {: t:^ tu:n :}
   [: OPEN? if
         OPENER$ s" ;FUNCTION" t tu SOURCE-ROOT:UNCLOSED
         CLOSED  CLOSED!
         EXIT-STATUS:REJECTED REJECTION:STOP
      then
      CLOSED!
      0 ARG-N ! -1 VAR-START ! 0 STRUCT-BYTES !
      R-NONE RES-KIND ! 0 RES-NULLABLE !
      src TOK-SPAN NAME!
      src TOK@ SYM!
      src EFFECT
      2dup named execute
      construct decl-state open DECL !  t OPENER-A !  tu OPENER-U ! ;] KEEP-REST ;

CAST: AS-TARGET ( n -- [ str -- str n ] )

public

\ Read before its library is checked, so an unreadable one is a syntax record;
\ a taken name is rejected at its source token before anything opens.
: OPEN-FUNCTION ( -- )
   SRCLOC:TOKEN$ [: ;] NESTED-REJECT
   [: parse-name ;]
   [: data-base PROVIDED-XT:TARGET-CELL + @ AS-TARGET
      execute drop 2drop ;]
   READ-FUNCTION 2drop
   OPENER$ s" ;FUNCTION" ['] DISCARD OPENED
   CHECK-SCOPE ;

\ The word and its row are made only here, after the plan's rejections, so a
\ rejected declaration leaves no half-built word.
: CLOSE-FUNCTION ( -- )
   [: CLOSE-STATE if CLOSED PLAN-CALL EMIT then ;] KEEP-REST ;

private

\ The close transition without the word: the plan and its rejections run.
: PLAN-FUNCTION ( -- )
   [: CLOSE-STATE if PLAN-CALL 2drop then ;] KEEP-REST ;

\ Sent by a driver that cannot tell what the declaration holds.
: SKIP-FUNCTION ( -- )
   [: DECL @ MATCH decl-state
         closed OF ENDOF
         open OF construct decl-state skipped DECL ! ENDOF
         skipped OF ENDOF
         left OF ENDOF
      ;MATCH ;] KEEP-REST ;

\ Sent by a driver after a stretch that may close or open a declaration: how it
\ closes is left to the run.
: LEAVE-FUNCTION ( -- )
   [: LEFT! ;] KEEP-REST ;

\ Sent by a driver after rendered text: the text may close the open declaration,
\ and the load rejects text that ends with one it opened.
: LEAVE-OPEN-FUNCTION ( -- )
   [: DECL @ MATCH decl-state
         closed OF ENDOF
         open OF LEFT! ENDOF
         skipped OF LEFT! ENDOF
         left OF ENDOF
      ;MATCH ;] KEEP-REST ;

: ABANDON-FUNCTION ( -- )
   [: CLOSED! ;] KEEP-REST ;

\ The texts DECLARED-EFFECT reads, in its order, with -- before the output.
: DECLARED-TEXTS ( [ str -- ] -- )
   {: put :}
   ARG-N @ 0 ?do
      i ARG-TEXT$ dup 0 <> if put execute else 2drop then
   loop
   s" --" put execute
   RES-KIND @ R-NONE = if exit then
   RES-TEXT RES-TEXT-U @ put execute ;

: NOT-OPEN ( -- )
   s" a clause outside a FUNCTION: declaration"
   s" write the clause between FUNCTION: and ;FUNCTION" SYNTAX-REJECT ;

: CLAUSE? ( -- bool )
   DECL @ MATCH decl-state
      closed OF NOT-OPEN ENDOF
      open OF true ENDOF
      skipped OF false ENDOF
      left OF false ENDOF
   ;MATCH ;

: STRUCT-VARIADIC ( -- )
   s" a struct call with VARIADIC"
   s" a struct call has no variadic arguments: declare a fixed effect"
   SYNTAX-REJECT ;

: NO-INPUT ( -- )
   s" the clause names no input of the declaration"
   s" name an input by its place in the effect, the first 0" SYNTAX-REJECT ;

: NOT-SPAN ( -- )
   s" the clause names an input that is not ^u8"
   s" name a ^u8 input" SYNTAX-REJECT ;

: TAKEN ( -- )
   s" a second clause names the same input"
   s" give a ^u8 input one STRUCT-ARG, or make it STRUCT-RESULT's destination, not both"
   SYNTAX-REJECT ;

: DEST-TAKEN ( -- )
   s" the clause names STRUCT-RESULT's destination, the last input"
   s" name another input: C writes the result record to the last one"
   SYNTAX-REJECT ;

\ The effect's input at place as an index into the ARG- arrays, a str's at its
\ base; ARG-N past the last.
: PLACE>ARG ( n -- n ) {: place:n :}
   place 0 < if place exit then
   place ARG-N @ 0 ?do
      ARG-TEXT-U i cells + @ 0 <> if
         dup 0= if drop i unloop exit then
         1-
      then
   loop
   drop ARG-N @ ;

\ The effect's input count, a str one.
: EFFECT-INPUTS ( -- n )
   0 ARG-N @ 0 ?do ARG-TEXT-U i cells + @ 0 <> if 1+ then loop ;

: CLAUSE-TARGET ( n -- ) {: idx:n :}
   idx 0 < idx ARG-N @ >= or if NO-INPUT then
   idx ARG-TEXT$ s" ^u8" STR:= 0= if NOT-SPAN then
   idx ARG-SBYTES@ 0 <> if TAKEN then
   STRUCT-BYTES @ 0 <> idx ARG-N @ 1- = and if DEST-TAKEN then ;

: VARIADIC-TWICE ( -- )
   s" a second VARIADIC"
   s" declare VARIADIC once, with the count of fixed inputs" SYNTAX-REJECT ;

: FIXED-RANGE ( -- )
   s" VARIADIC's fixed count is not from 1 to the inputs declared"
   s" write n VARIADIC with n the inputs before the first variadic one"
   SYNTAX-REJECT ;

: VARIADIC-F32 ( -- )
   s" an f32 variadic argument"
   s" C promotes a variadic float to double: declare it r" SYNTAX-REJECT ;

: VARIADIC-CLAUSE ( n -- ) {: fixed:n :}
   CLAUSE? 0= if exit then
   VAR-START @ -1 <> if VARIADIC-TWICE then
   STRUCT-BYTES @ 0 <> RECORDS? or if STRUCT-VARIADIC then
   fixed 1 < fixed EFFECT-INPUTS > or if FIXED-RANGE then
   fixed PLACE>ARG {: start:n :}
   ARG-N @ start ?do
      i ARG-KIND@ K-FLOAT32 = if VARIADIC-F32 then
   loop
   start VAR-START ! ;

: RESULT-CHECK ( n -- ) {: bytes:n :}
   STRUCT-BYTES @ 0 <> if
      s" a second STRUCT-RESULT" s" declare STRUCT-RESULT once" SYNTAX-REJECT
   then
   bytes 0 <= if
      s" STRUCT-RESULT's byte count is not positive"
      s" write n STRUCT-RESULT with n the record's size in bytes" SYNTAX-REJECT
   then
   RES-KIND @ R-NONE <> if
      s" STRUCT-RESULT with a declared output"
      s" declare no output: C writes the record to the last input" SYNTAX-REJECT
   then ;

: STRUCT-RESULT-CLAUSE ( n -- ) {: bytes:n :}
   CLAUSE? 0= if exit then
   bytes RESULT-CHECK
   VAR-START @ -1 <> if STRUCT-VARIADIC then
   ARG-N @ 0= if
      s" STRUCT-RESULT with no input to write to"
      s" declare the record's address as the last input, a ^u8" SYNTAX-REJECT
   then
   ARG-N @ 1- CLAUSE-TARGET
   bytes STRUCT-BYTES ! ;

\ The struct call returns two integer registers, so a float result has no shape here.
: STRUCT-ARG-CLAUSE ( n n -- ) {: place:n bytes:n :}
   CLAUSE? 0= if exit then
   place PLACE>ARG {: idx:n :}
   idx CLAUSE-TARGET
   bytes 0 <= if
      s" STRUCT-ARG's byte count is not positive"
      s" write idx n STRUCT-ARG with n the record's size in bytes" SYNTAX-REJECT
   then
   VAR-START @ -1 <> if STRUCT-VARIADIC then
   RES-KIND @ R-FLOAT = RES-KIND @ R-FLOAT32 = or if
      s" STRUCT-ARG with a float result"
      s" the struct call returns integer registers: declare an integer result" SYNTAX-REJECT
   then
   bytes idx ARG-SBYTES! ;

\ VERSIONED-LIBRARY's next operand from src, copied; with none left the record
\ stands at the token read last, which why labels.
: LIB-OPERAND ( [ -- str ] str -- str ) {: src why:^ whyu:n :}
   src execute {: a:^ u:n :}
   u 0= if
      s" VERSIONED-LIBRARY" s" a base and a version list"
      SRCLOC:TOKEN$ why whyu REJECTION:MISSING-OPERAND
      s" write the base and its versions after VERSIONED-LIBRARY, as `VERSIONED-LIBRARY z 1,2`"
      REJECTION:EMIT-STR  STOP-REJECTION
   then
   a u TOK!  a u ;

public

: SET-VARIADIC ( n -- ) [: VARIADIC-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-RESULT ( n -- ) [: STRUCT-RESULT-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-ARG ( n n -- ) [: STRUCT-ARG-CLAUSE ;] KEEP-REST ;

: SELECT-LIBRARY ( str -- ) {: path:^ u:n :}
   path u [: CHECK-PATH ;] KEEP-REST
   path u LIBRARY-PATH CUR-LIB !
   RECORD-SCOPE ;

: SELECT-VERSIONED-LIBRARY ( -- )
   [: parse-name ;] {: src :}
   [: src s" no base follows" LIB-OPERAND 2dup BASE!
      src s" no version list follows" LIB-OPERAND 2drop TOK$ VERSION-ROWS CUR-LIB !
      RECORD-SCOPE ;]
   KEEP-REST ;

: SELECT-PROCESS ( -- )
   PROCESS CUR-LIB !
   RECORD-SCOPE ;

global

\ The declaration keywords are global: a package word ending in a colon cannot
\ be reached through a qualifier, and a declarer reads bare from any package
\ anyway. All three parse the live input stream, which their ( -- ) rows do
\ not model, so they are top-level-interpret-only.
: LIBRARY ( -- )
   parse-name FFI:SELECT-LIBRARY ;
parses: LIBRARY 1

: VERSIONED-LIBRARY ( -- )
   FFI:SELECT-VERSIONED-LIBRARY ;
parses: VERSIONED-LIBRARY 2

: PROCESS-SYMBOLS ( -- )
   FFI:SELECT-PROCESS ;

: FUNCTION: ( -- )
   FFI:OPEN-FUNCTION ;
INTRINSIC-TOP-ONLY INTRINSIC

: VARIADIC ( n -- ) FFI:SET-VARIADIC ;

: STRUCT-RESULT ( n -- ) FFI:SET-STRUCT-RESULT ;

: STRUCT-ARG ( n n -- ) FFI:SET-STRUCT-ARG ;

: ;FUNCTION ( -- )
   FFI:CLOSE-FUNCTION ;
INTRINSIC-TOP-ONLY INTRINSIC

;package
