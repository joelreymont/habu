\ ffi-abi.f - checked AAPCS64 and SysV FFI calls, package FFI. AAPCS64 passes
\ eight integer and eight float registers, SysV six and eight. FUNCTION: is the
\ only checked call surface: its word names a row, never an address.
\ STORAGE CLASS. TASK-LOCAL: marshalling buffers, extent tables, kernel params
\ and the DLSYM loader block; calls nest only through a C callback, which shares
\ them (src/lib/ffi-callback.f). PROCESS-WIDE: rows, cleared before each capture.

s" src/lib/errors.f" required
s" src/lib/span.f" required                   \ CSTR's destination carries its reach
s" src/lib/string.f" required
s" src/lib/image-lifecycle.f" required
s" src/lib/le.f" required                     \ the four-byte C int this package reads back
s" src/lib/float32.f" required                \ binary32 conversion at the C ABI boundary
s" src/lib/adt/option.f" required             \ option<n> - the version token VERSIONED-LIBRARY parses
s" src/lib/codegen.f" required                \ the rendered library name and the declarer's buffers
s" src/lib/memory.f" required                 \ an error line's buffer, sized to the line

package FFI

8 constant FFI-REG-ARGS
16 constant FFI-MAX-ARGS
16 constant FFI-MAX-KPARAMS
2 constant FFI-RTLD-NOW                   \ dlopen RTLD_NOW: bind every symbol at load
$0A constant LF-C                         \ an error line's terminator

$3A00 constant FFI-BUF-OFF
$3A80 constant FFI-FBUF-OFF
$3AC0 constant FFI-STACK-BUF-OFF
$3B40 constant FFI-KPARAM-PBUF-OFF
$3BC0 constant FFI-KPARAM-VBUF-OFF
$3C40 constant FFI-DLBUF-OFF
$3C80 constant FFI-KPARAM#-OFF
$40C8 constant FFI-REG-LEN-BUF-OFF
$4148 constant FFI-STACK-LEN-BUF-OFF
$41C8 constant FFI-SCRATCH-END

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

: FFI-REG-LEN-BUF ( -- ptr n )
   data-base FFI-REG-LEN-BUF-OFF + ;

: FFI-STACK-LEN-BUF ( -- ptr n )
   data-base FFI-STACK-LEN-BUF-OFF + ;

\ The identity axioms FFI-PTR>CELL / FFI-CELL>PTR are declared in
\ src/core/checker.f with an owner-private row admitting this package alone:
\ `( ptr a -- n )` is no CAST: (a type-variable pointee is E-CAST-LINEAR). CELL>PTR
\ stays private: a foreign address carries no length, so only a word that knows
\ the callee's width may publish one.
: PTR>CELL ( ptr a -- n ) FFI-PTR>CELL ;
: CELL>PTR ( n -- ptr u8 ) FFI-CELL>PTR ;

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

: REG-LEN-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-REG-LEN-BUF idx cells + ;

: STACK-LEN-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-LEN-BUF idx cells + ;

: REG-LEN! ( n n -- ) {: len:n idx:n :}
   len 0 < if E-FFI-ARITY throw then
   len idx REG-LEN-SLOT ! ;

: STACK-LEN! ( n n -- ) {: len:n idx:n :}
   len 0 < if E-FFI-ARITY throw then
   len idx STACK-LEN-SLOT ! ;

: WRITABLE-LEN ( n -- n ) {: len:n :}
   len 0 <= if E-FFI-ARITY throw then
   len ;

\ Checked bodies over the owner-private ffi-call-bounded row; the loader's own
\ two GOT slots, DLOPEN-SLOT and DLSYM-SLOT, hold the addresses.
: DLOPEN-RAW ( ptr u8 n -- n ) {: path:ptr flags:n :}
   path PTR>CELL FFI-DLBUF !
   flags FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLOPEN-SLOT @ ffi-call-bounded ;

: DLSYM-RAW ( n ptr u8 -- n ) {: handle:n name:ptr :}
   handle FFI-DLBUF !
   name PTR>CELL FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLSYM-SLOT @ ffi-call-bounded ;

\ ---- declared foreign functions -------------------------------------------
\ FUNCTION: names a row here and its generated word carries the row's index, so
\ a checked caller reaches only a symbol some declaration published. Exact
\ declarations share one row. A row resolves its symbol on the first call and
\ caches the address. Library 0 is the process (RTLD_DEFAULT, no reference);
\ any other index is a path dlopened once. Rows may move when a bank grows;
\ indices stay stable. The two banks reuse descriptors independently.

$30 constant FN-NAME-CAP                  \ NUL-terminated C symbol
PATH-CAP 1 + constant LIB-PATH-CAP      \ a library path of PATH-CAP bytes and its NUL
$20 constant LIB-BASE-CAP                 \ the base name LIBRARY-NAME$ renders around
9999 constant LIB-VERSION-MAX             \ ... and its soname version

0 cells constant FN-ADDR-OFF
1 cells constant FN-LIB-OFF
2 cells constant FN-ARGC-OFF
3 cells constant FN-SPILL-OFF
4 cells constant FN-RELOC-OFF
5 cells constant FN-NAME-OFF
FN-NAME-OFF FN-NAME-CAP + constant FN-ROW-BYTES
0 cells constant LIB-LEN-OFF
1 cells constant LIB-HANDLE-OFF
2 cells constant LIB-FAILED-OFF           \ 1 once the path failed to open; cleared with the handle
3 cells constant LIB-PATH-OFF
LIB-PATH-OFF LIB-PATH-CAP + 7 + 8 / 8 * constant LIB-ROW-BYTES
PERSISTED-PTR-VARIABLE FN-TABLE
PERSISTED-PTR-VARIABLE LIB-TABLE
variable FN-CAP
variable LIB-CAP
variable FN-N
variable LIB-N
\ The builder's declarations are a second bank: their call sites carry MAKER-TAG
\ in the index, so a callback into the application selects its own rows while
\ the builder's foreign call is still on the stack.
PERSISTED-PTR-VARIABLE MAKER-FN-TABLE
PERSISTED-PTR-VARIABLE MAKER-LIB-TABLE
variable MAKER-FN-CAP
variable MAKER-LIB-CAP
variable MAKER-FN-N
variable MAKER-LIB-N
variable MAKER-DEPTH
$4000000000000000 constant MAKER-TAG
\ 0, or the xt OBSERVE stored.
variable OBSERVER
CAST: OBSERVER-FN ( n -- [ n n -- ] )
CAST: OBSERVER-N ( [ n n -- ] -- n )
1 constant OBS-LIB
2 constant OBS-FN
3 constant OBS-SCOPE

: NOTIFY ( n n -- ) {: kind:n idx:n :}
   OBSERVER @ dup 0<> if kind idx rot OBSERVER-FN execute else drop then ;
variable FN-REGISTERED
PERSISTED-PTR-VARIABLE PIN-FN
PERSISTED-PTR-VARIABLE PIN-LIB
variable PIN-FN-CAP
variable PIN-LIB-CAP
DYNAMIC-BUFFER RETIRED-BASE ptr u8
DYNAMIC-BUFFER RETIRED-SIZE n
variable RETIRED-COUNT

: MAKER-TOKEN? ( n -- bool ) MAKER-TAG and 0<> ;
: ROW-INDEX ( n -- n ) MAKER-TAG invert and ;
: DECL-TAG ( -- n ) MAKER-DEPTH @ 0<> if MAKER-TAG else 0 then ;
: FN-TABLE-CELL ( n -- ptr ptr u8 ) MAKER-TOKEN? if MAKER-FN-TABLE else FN-TABLE then ;
: LIB-TABLE-CELL ( n -- ptr ptr u8 ) MAKER-TOKEN? if MAKER-LIB-TABLE else LIB-TABLE then ;
: FN-CAP-CELL ( n -- ptr n ) MAKER-TOKEN? if MAKER-FN-CAP else FN-CAP then ;
: LIB-CAP-CELL ( n -- ptr n ) MAKER-TOKEN? if MAKER-LIB-CAP else LIB-CAP then ;
: FN-N-CELL ( n -- ptr n ) MAKER-TOKEN? if MAKER-FN-N else FN-N then ;
: LIB-N-CELL ( n -- ptr n ) MAKER-TOKEN? if MAKER-LIB-N else LIB-N then ;

\ Only declarations use the ambient scope. Every call and every lookup instead
\ chooses the bank from its own token; callbacks can therefore reenter FFI.
public
: WITH-MAKER-DECLARATIONS ( [ -- ] -- ) {: load :}
   1 MAKER-DEPTH +!
   load [: -1 MAKER-DEPTH +! ;] finally ;
private

: ALIGNED-ROWS ( n -- ptr u8 )
   {: size:n :}
   here data-base - negate 7 and allot
   here BYTE-VIEW {: rows:ptr :}
   size allot
   rows ;

\ The loader may still read a row's inline name or path when a callback grows a
\ table. Keep retired declarations until the next quiescent cleanup.
: RETIRE-ROOM ( -- )
   RETIRED-COUNT @ 1+ dup RETIRED-BASE-RESERVE RETIRED-SIZE-RESERVE ;

: RETIRE ( ptr u8 n -- ) {: base:ptr size:n :}
   base RETIRED-COUNT @ RETIRED-BASE !
   size RETIRED-COUNT @ RETIRED-SIZE !
   1 RETIRED-COUNT +! ;

: FN-GROW ( -- )
   DECL-TAG {: token:n :}
   token FN-CAP-CELL @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   oldcap 0 > if RETIRE-ROOM then
   next FN-ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   oldcap 0 > if
      token FN-TABLE-CELL @ {: old:ptr :}
      old rows token FN-N-CELL @ FN-ROW-BYTES * BYTE-COPY
      old oldcap FN-ROW-BYTES * RETIRE
      token FN-N-CELL @ 0 ?do 0 old i FN-ROW-BYTES * + FN-ADDR-OFF + CELL-VIEW ! loop
   then
   rows token FN-TABLE-CELL !
   next token FN-CAP-CELL ! ;

: LIB-GROW ( -- )
   DECL-TAG {: token:n :}
   token LIB-CAP-CELL @ {: oldcap:n :}
   oldcap 0= if 1 else oldcap 2 * then {: next:n :}
   oldcap 0 > if RETIRE-ROOM then
   next LIB-ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   oldcap 0 > if
      token LIB-TABLE-CELL @ {: old:ptr :}
      old rows token LIB-N-CELL @ LIB-ROW-BYTES * BYTE-COPY
      old oldcap LIB-ROW-BYTES * RETIRE
      token LIB-N-CELL @ 0 ?do
         old i LIB-ROW-BYTES * + {: row:ptr :}
         0 row LIB-HANDLE-OFF + CELL-VIEW !
         0 row LIB-FAILED-OFF + CELL-VIEW !
      loop
   then
   rows token LIB-TABLE-CELL !
   next token LIB-CAP-CELL ! ;

: FN-NEW ( -- )
   DECL-TAG dup FN-N-CELL @ swap FN-CAP-CELL @ >= if FN-GROW then ;

: LIB-NEW ( -- )
   DECL-TAG dup LIB-N-CELL @ swap LIB-CAP-CELL @ >= if LIB-GROW then ;

: FN-ROW ( n -- ptr u8 )
   {: idx:n :}
   idx FN-TABLE-CELL @ idx ROW-INDEX FN-ROW-BYTES * + ;

: LIB-ROW ( n -- ptr u8 )
   {: idx:n :}
   idx LIB-TABLE-CELL @ idx ROW-INDEX LIB-ROW-BYTES * + ;

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
   idx ROW-INDEX idx FN-N-CELL @ FFI-CHECK-INDEX ;

: FN-NAME ( n -- ptr u8 )
   {: idx:n :}
   idx FN-CHECK
   idx FN-ROW FN-NAME-OFF + ;

\ Row idx's symbol length: the bytes before its terminator.
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
   idx ROW-INDEX idx LIB-N-CELL @ FFI-CHECK-INDEX ;

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

\ The register, spill, dlopen/dlsym and length buffers keep the last call's
\ values, which made two --repl builds of one program differ
\ (tools/hb-build-repl-twin-test.f). Kernel params stay until FFI-KPARAM-RESET.
: FORGET-STAGED ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

: PROCESS-HANDLE ( -- n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if 0 exit then
   HB-TARGET-MACOS? if -2 exit then
   E-FFI-LIBRARY throw ;

public
\ Library index idx's path, as LIBRARY-PATH and LIB-INDEX number it (from 1).
: LIB-ROW@ ( n -- ptr u8 n ) 1- {: row:n :} row LIB-PATH row LIB-LEN@ ;
private

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

\ dlerror's NULL means the loader kept no reason.
: OPEN-REPORT ( n n -- ) {: dlerr:n lib:n :}
   FFI-BUF FFI-REG-LEN-BUF 0 dlerr ffi-call-bounded {: why:n :}
   s" E-FFI-DLOPEN: " lib LIB-ROW@
   why 0 <> if s" : " why CELL>PTR dup ZLEN else s" " s" " then
   REPORT-LINE ;

\ dlerr is dlerror's address, resolved before the dlopen so that no loader call
\ runs between the failure and OPEN-REPORT's read of its reason. A failed open
\ is reported once; the slot's mark answers later calls without the loader.
: LIB-RESOLVE ( n n -- n ) {: dlerr:n lib:n :}
   lib 0= if PROCESS-HANDLE exit then
   lib 1 - {: slot:n :}
   slot LIB-HANDLE@ dup 0 <> if exit then
   drop
   slot LIB-FAILED? if E-FFI-DLOPEN throw then
   slot LIB-PATH FFI-RTLD-NOW DLOPEN-RAW
   dup 0= if 1 slot LIB-FAILED! dlerr lib OPEN-REPORT E-FFI-DLOPEN throw then
   dup slot LIB-HANDLE! ;

: SYMBOL-REPORT ( n n -- ) {: idx:n lib:n :}
   s" E-FFI-DLSYM: " idx FN-NAME idx NAME-LEN
   lib 0 <> if s"  in " lib LIB-ROW@ else s" " s" " then
   REPORT-LINE ;

variable ERRNO-FN-CELL
variable DLCLOSE-FN-CELL
variable DLERROR-FN-CELL

: ERRNO-FN ( -- n ) ERRNO-FN-CELL @ ;
: DLERROR-FN ( -- n ) DLERROR-FN-CELL @ ;

\ Resolution is lazy: a declaration never calls the loader, and a missing symbol
\ is E-FFI-DLSYM at the first call. The errno accessor resolves first because
\ the loader may change errno even when it succeeds, and nothing may run it
\ between a call and its errno read. This lookup registers nothing.
: FN-LOOKUP ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   idx ERRNO-FN <> if ERRNO-FN RECURSE drop then
   idx FN-LIB@ {: lib:n :}
   lib 0 <> if DLERROR-FN RECURSE else 0 then lib LIB-RESOLVE
   idx FN-NAME DLSYM-RAW
   dup 0= if idx lib SYMBOL-REPORT E-FFI-DLSYM throw then
   dup idx FN-ADDR! ;

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
: SCRATCH-END ( -- n ) FFI-SCRATCH-END ;
: NOW ( -- n ) FFI-RTLD-NOW ;

\ FFI:NOW alone is RTLD_LOCAL on Linux, so PROCESS-SYMBOLS cannot see the
\ library; `FFI:NOW FFI:GLOBAL or` joins its symbols to RTLD_DEFAULT. macOS
\ opens global either way. <dlfcn.h> spells RTLD_GLOBAL 0x100 in glibc, 0x8 on
\ macOS.
: GLOBAL ( -- n ) HB-TARGET-MACOS? if $8 else $100 then ;

: >CELL ( ptr a -- n ) PTR>CELL ;

: RESET ( -- )
   FFI-MAX-ARGS 0 ?do
      0 i REG-LEN!
      0 i STACK-LEN!
   loop ;

: VALUE! ( n n -- ) {: value:n idx:n :}
   value idx FFI-ARG!
   0 idx REG-LEN! ;

: READABLE! ( ptr a n -- ) {: value:ptr idx:n :}
   value idx FFI-PTR-ARG!
   0 idx REG-LEN! ;

: WRITABLE! ( ptr a n n -- ) {: value:ptr len:n idx:n :}
   value idx FFI-PTR-ARG!
   len WRITABLE-LEN idx REG-LEN! ;

: FLOAT! ( r n -- ) FFI-FARG! ;
: FLOAT32! ( r n -- ) {: value:r idx:n :}
   value F32:NARROW idx FFI-F32ARG! ;

: STACK-VALUE! ( n n -- ) {: value:n idx:n :}
   value idx FFI-STACK!
   0 idx STACK-LEN! ;

: STACK-FLOAT! ( r n -- ) {: value:r idx:n :}
   value idx FFI-FSTACK!
   0 idx STACK-LEN! ;

: STACK-READABLE! ( ptr a n -- ) {: value:ptr idx:n :}
   value PTR>CELL idx FFI-STACK!
   0 idx STACK-LEN! ;

: STACK-WRITABLE! ( ptr a n n -- ) {: value:ptr len:n idx:n :}
   value PTR>CELL idx FFI-STACK!
   len WRITABLE-LEN idx STACK-LEN! ;

: X8-VALUE! ( n -- ) 8 VALUE! ;
: X8-READABLE! ( ptr a -- ) 8 READABLE! ;
: X8-WRITABLE! ( ptr a n -- ) 8 WRITABLE! ;

: ARGS ( -- ptr n ) FFI-BUF ;
: FLOATS ( -- ptr r ) FFI-FBUF ;
: STACK ( -- ptr n ) FFI-STACK-BUF ;
: REG-LENS ( -- ptr n ) FFI-REG-LEN-BUF ;
: STACK-LENS ( -- ptr n ) FFI-STACK-LEN-BUF ;

: OUT@ ( ptr n -- n ) FFI-OUT@ ;
: OUT! ( n ptr n -- ) FFI-OUT! ;
: KPARAM-COUNT ( -- n ) FFI-KPARAM-COUNT ;
: KPARAM-RESET ( -- ) FFI-KPARAM-RESET ;
: KPARAM+ ( ptr a -- ) FFI-KPARAM+ ;
: KPARAM-VALUE+ ( n -- ) FFI-KPARAM-N+ ;
: KPARAMS ( -- ptr n n ) FFI-KPARAMS ;
: KPARAMS>CELL ( -- n ) FFI-KPARAMS>N ;

\ A string holding a 0 byte cannot cross to C and is E-FFI-NUL; FFI is the only
\ library code that writes a terminator.
private
\ A 0 byte among the u bytes at b would end the C string early.
: NUL-CHECK ( ptr u8 n -- )
   {: b u :}
   u 0 ?do b i + c@ 0= if E-FFI-NUL throw then loop ;

\ The span's base once u bytes and their 0 fit in it: u < reach, which cannot
\ overflow as u+1 can. Refused before any byte is read or written.
: CSTR-FIT ( n SPAN:span<u8> -- ptr u8 )
   {: u s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN:$ {: b l :}
   u l >= if E-SPAN-CAPACITY throw then
   b ;

public
\ The bytes a C copy of an n-byte string occupies: the string and its 0.
: CSTR-ROOM ( n -- n ) 1+ ;

\ Terminate the n bytes already at the front of the span and answer its base
\ as the C string.
: CSTR! ( n SPAN:span<u8> -- ptr u8 )
   {: u s :}
   u s CSTR-FIT {: b :}
   b u NUL-CHECK
   0 b u + c!
   b ;

\ Copy a Habu string into the span as a C string. Every refusal comes before
\ a byte is written, so a refused destination keeps what it held.
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
\ Habu xt. src/lib/ffi-callback.f answers it to C once the slot is bound.
: CALLBACK-ENTRY ( n -- n ) callback-entry ;

\ ---- the declared-function surface ----------------------------------------
\ The declarer below is DECLARE-ROW's only intended caller; DECLARE is the row of
\ a call with no spilled arguments and nothing to relocate.
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


\ Path identity is the complete byte span, including its length. The selected
\ bank's library index of the path, or 0 when no row holds it.
: LIB-INDEX ( ptr u8 n -- n ) {: path:ptr u:n :}
   DECL-TAG {: tag:n :}
   tag LIB-N-CELL @ 0 ?do
      i tag or LIB-LEN@ u = if
         path u i tag or LIB-PATH u STR= if i 1+ tag or unloop exit then
      then
   loop
   0 ;

\ Reusing an occupied row leaves its resolved handle in place; only a new path
\ consumes a row.
: LIBRARY-PATH ( ptr u8 n -- n )
   {: path:ptr u:n :}
   u PATH-ROOM
   DECL-TAG {: tag:n :}
   path u LIB-INDEX dup 0<> if OBS-LIB over NOTIFY exit then drop
   LIB-NEW
   path u tag LIB-N-CELL @ tag or PATH-FIELD CSTR drop
   tag LIB-N-CELL @ 1+ tag LIB-N-CELL !
   tag LIB-N-CELL @ {: row:n :}
   u row 1- tag or LIB-LEN!
   0 row 1- tag or LIB-HANDLE!
   0 row 1- tag or LIB-FAILED!
   \ Keep the row: an observer that declares rows of its own moves the count.
   OBS-LIB row tag or NOTIFY
   row tag or ;

private

\ dlclose answers a C int, which fills only the low half of the return register,
\ and zero is success. A refused close keeps the handle for the next PREPARE to
\ retry. This runs only inside FFI's registered hook, so the dlclose row is
\ looked up without registering.
: CLOSE-SLOT ( n -- bool ) {: slot:n :}
   RESET slot LIB-HANDLE@ 0 VALUE!
   DLCLOSE-FN-CELL @ {: row:n :}
   row FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-REG-LEN-BUF row FN-ARGC@ fn ffi-call-bounded
   $FFFFFFFF and 0 <> if false exit then
   0 slot LIB-HANDLE!
   true ;

\ Library handles are owned references, released here; function addresses are
\ borrowed and only cleared, so the first call after a restore re-resolves. This
\ runs after every one-shot cleanup (IMAGE-LIFECYCLE:PREPARE runs persistent
\ hooks last, in reverse order, and FFI registers first), so module cleanup that
\ calls into a library has finished before the library closes.
: CLOSE-BANK ( n -- bool ) {: tag:n :}
   true
   tag LIB-N-CELL @ 0 ?do
      i tag or LIB-HANDLE@ 0 <> if i tag or CLOSE-SLOT and then
   loop ;

: CLEAR-BANK ( n -- ) {: tag:n :}
   tag FN-N-CELL @ 0 ?do 0 i tag or FN-ADDR! loop
   tag LIB-N-CELL @ 0 ?do 0 i tag or LIB-FAILED! loop ;

: ZERO-ROWS ( ptr u8 n -- ) {: rows:ptr bytes:n :}
   bytes 0 ?do 0 rows i + c! loop ;

: DRAIN-RETIRED ( -- )
   RETIRED-COUNT @ 0 ?do
      i RETIRED-BASE @ {: base:ptr :}
      base PIN-FN @ <> base PIN-LIB @ <> and if
         base i RETIRED-SIZE @ ZERO-ROWS
      then
   loop
   RETIRED-BASE-RELEASE RETIRED-SIZE-RELEASE
   0 RETIRED-COUNT ! ;

: FORGET-SYMBOLS ( -- )
   0 CLOSE-BANK MAKER-TAG CLOSE-BANK and {: closed:bool :}
   \ Closing a maker library can resolve the ordinary dlclose row again.
   \ Clear both banks only after all handles have been attempted.
   0 CLEAR-BANK MAKER-TAG CLEAR-BANK
   FFI-BUF BYTE-VIEW FFI-KPARAM-PBUF-OFF FFI-BUF-OFF - FORGET-STAGED
   FFI-DLBUF BYTE-VIEW FFI-KPARAM#-OFF FFI-DLBUF-OFF - FORGET-STAGED
   FFI-REG-LEN-BUF BYTE-VIEW FFI-SCRATCH-END FFI-REG-LEN-BUF-OFF - FORGET-STAGED
   closed 0= if E-FFI-DLCLOSE throw then
   DRAIN-RETIRED ;

\ The registered flag is set only after REGISTER completes, so a throwing
\ registration leaves flag and registry consistent for a retry (the rule above
\ src/lib/image-lifecycle.f PREPARE).
: REGISTER-CLEANUP ( -- )
   FN-REGISTERED @ 0= if
      [: FORGET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT
      1 FN-REGISTERED !
   then ;

\ The unresolved path arms FFI's cleanup before the lookup acquires an address
\ or a handle. A stripped image needs this: it starts with an empty registry and
\ the flag clear (AOT-CONTROLS) and replays no declaration, so its first
\ resolution registers, and FFI's hook is its first persistent one unless the
\ program registered one at run time before its first foreign call. Registering
\ here, not in the CALL words, places this after REGISTER-CLEANUP and the hook's
\ dlclose call, which uses FN-LOOKUP.
: FN-RESOLVE ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   REGISTER-CLEANUP
   idx FN-LOOKUP ;

public

\ A source import/export boundary has no foreign call in flight, so the cache is
\ released there before declarations are copied.
: FORGET-CACHE ( -- ) FORGET-SYMBOLS ;

\ An AOT window retains the application-close allocation even if later linker
\ declarations grow the live table. Release it only after LINK has serialized
\ the image, or when a new window abandons the prior one.
: IMAGE-PIN ( ptr u8 n ptr u8 n -- )
   {: fn:ptr fncap:n lib:ptr libcap:n :}
   fn FN-TABLE @ <> fncap FN-CAP @ <> or
   lib LIB-TABLE @ <> libcap LIB-CAP @ <> or or if E-FFI-SYNTAX throw then
   fn PIN-FN ! fncap PIN-FN-CAP !
   lib PIN-LIB ! libcap PIN-LIB-CAP ! ;

: IMAGE-UNPIN ( -- )
   PIN-FN-CAP @ 0 > PIN-FN @ FN-TABLE @ <> and if
      PIN-FN @ PIN-FN-CAP @ FN-ROW-BYTES * ZERO-ROWS
   then
   PIN-LIB-CAP @ 0 > PIN-LIB @ LIB-TABLE @ <> and if
      PIN-LIB @ PIN-LIB-CAP @ LIB-ROW-BYTES * ZERO-ROWS
   then
   NULL-PTR PIN-FN ! NULL-PTR PIN-LIB !
   0 PIN-FN-CAP ! 0 PIN-LIB-CAP ! ;

: CALL ( n -- n ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-REG-LEN-BUF idx FN-ARGC@ fn ffi-call-bounded ;

: CALL-PTR ( n -- ptr u8 ) CALL CELL>PTR ;

: CALL-ABI ( n -- n ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   idx FN-SPILL@ fn ffi-call-abi-bounded ;

: CALL-ABI-PTR ( n -- ptr u8 ) CALL-ABI CELL>PTR ;

: CALL-ABI-R ( n -- r ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   idx FN-SPILL@ fn ffi-call-abi-r-bounded ;

: CALL-ABI-F32 ( n -- r )
   CALL-ABI-R IEEE754:F64>BITS $FFFFFFFF and F32:WIDEN ;

\ A struct-returning declaration resolves its row before staging: a first-call
\ dlopen can run constructors that call C through this task's scratch.
: RESOLVE-ROW ( n -- ) FN-RESOLVE drop ;

\ The caller's result storage rides x8, AAPCS64's indirect-result register,
\ with its whole extent guarded. A record returned in x0/x1 ignores x8, and
\ the guard still covers the stores made from them. Storage C writes through
\ x8 is 16-byte aligned, the strictest a supported record can need; a
\ register record stores into eight-byte-aligned storage.
: RESULT-DEST! ( ptr u8 n -- ) {: dst:ptr len:n :}
   dst PTR>CELL {: addr:n :}
   len 16 > if 15 else 7 then {: mask:n :}
   addr 0= addr mask and 0<> or if E-FFI-SYNTAX throw then
   dst len X8-WRITABLE! ;

\ The raw x0 and x1 after the call, in address order. The row's relocation
\ mask names the staged cells that hold a private struct copy's address in
\ the stack buffer: bits 0-7 are x0-x7 and bits 8-23 stack cells 0-15. The
\ engine copies the stack buffer into the native call frame and moves each
\ named cell to the copy there, so C receives storage that lives exactly as
\ long as the call and that no other call reuses.
: CALL-STRUCT ( n -- n n ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   idx FN-SPILL@ idx FN-RELOC@ fn ffi-call-struct-bounded ;

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

\ A record over 16 bytes is copied into stack cells from payload on, and its
\ address there is what the call relocates.
: RECORD-COPY ( ptr u8 n n -- n ) {: src:ptr len:n pay:n :}
   src len RECORD-CELLS 0 ?do src len i RECORD-CELL pay i + STACK-VALUE! loop
   pay FFI-STACK-SLOT PTR>CELL ;

public

\ STRUCT-ARG staging. A record of 16 bytes or fewer goes whole in consecutive
\ integer registers from reg, or in stack cells from pos; a larger one is
\ copied into stack cells from pay and passed by address in register reg or
\ stack cell pos. Every staged cell has extent 0: C reads the record's
\ copy, never the source.
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
\ An exact descriptor includes the library index, complete C symbol, integer
\ argument count, spill count and relocation mask. The stored symbol's length
\ decides along with its bytes, so neither a prefix nor a name holding a 0 byte
\ matches a row.
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

\ The selected bank's index of the first row matching the descriptor, or -1
\ when none does.
: FN-INDEX ( ptr u8 n n n n n -- n )
   {: name:ptr u:n lib:n argc:n spills:n reloc:n :}
   u NAME-ROOM
   DECL-TAG {: tag:n :}
   tag FN-N-CELL @ 0 ?do
      name u lib argc spills reloc i tag or SAME-FN? if i tag or unloop exit then
   loop
   -1 ;

\ Validation precedes reading a row's name. A match keeps its cached address;
\ only a new descriptor appends to the selected bank. Declaring registers FFI's
\ cleanup: loading this file declares the errno row before any module can
\ register a persistent hook, so FFI's hook is the first and runs last, and a
\ full image keeps the entry and the flag (tools/aot-build-core.f registers
\ PROC-MAPS:RELOAD, which calls Mach through this table, after it).
: DECLARE-ROW ( ptr u8 n n n n n -- n )
   {: name:ptr u:n lib:n argc:n spills:n reloc:n :}
   DECL-TAG {: tag:n :}
   lib 0 < if E-FFI-LIBRARY throw then
   lib 0<> if
      lib MAKER-TOKEN? if
         tag 0= if E-FFI-LIBRARY throw then
      else
         tag 0<> if E-FFI-LIBRARY throw then
      then
      lib ROW-INDEX dup 0= if E-FFI-LIBRARY throw then
      tag LIB-N-CELL @ > if E-FFI-LIBRARY throw then
   then
   argc FFI-MAX-ARGS FFI-CHECK-COUNT
   spills FFI-MAX-ARGS FFI-CHECK-COUNT
   reloc argc spills RELOC-OWNED? 0= if E-FFI-ARITY throw then
   name u lib argc spills reloc FN-INDEX dup 0 >= if REGISTER-CLEANUP OBS-FN over NOTIFY exit then drop
   REGISTER-CLEANUP
   FN-NEW
   tag FN-N-CELL @ tag or {: idx:n :}
   name u idx NAME-FIELD CSTR drop
   tag FN-N-CELL @ 1+ tag FN-N-CELL !
   spills idx FN-SPILL-OFF FN-FIELD !
   reloc idx FN-RELOC-OFF FN-FIELD !
   lib idx FN-LIB-OFF FN-FIELD !
   argc idx FN-ARGC-OFF FN-FIELD !
   0 idx FN-ADDR!
   \ Keep the row: an observer that declares rows of its own moves the count.
   OBS-FN idx NOTIFY
   idx ;

: DECLARE ( ptr u8 n n n -- n ) 0 0 DECLARE-ROW ;

\ The rows every bank has room for; a declaration that grows a bank's table
\ moves it (test/native-file-frame-e2e.f FFI-FIXTURES declares until it does).
: MARK ( -- n )
   FN-CAP @ LIB-CAP @ + MAKER-FN-CAP @ + MAKER-LIB-CAP @ + ;

\ The index the selected bank's next new function or library row gets.
: NEXT-FN ( -- n ) DECL-TAG dup FN-N-CELL @ or ;
: NEXT-LIB ( -- n ) DECL-TAG dup LIB-N-CELL @ 1+ or ;

\ Row idx's descriptor: symbol, library index, argument and spill counts and
\ relocation mask, in DECLARE-ROW's order.
: FN-ROW@ ( n -- ptr u8 n n n n n )
   {: idx:n :}
   idx FN-NAME idx NAME-LEN idx FN-LIB@ idx FN-ARGC@ idx FN-SPILL@ idx FN-RELOC@ ;
: MAX-ARGS ( -- n ) FFI-MAX-ARGS ;
\ The bit a builder-support row's index carries.
: MAKER-BANK ( -- n ) MAKER-TAG ;
: SYMBOL-CAP ( -- n ) FN-NAME-CAP 1- ;

\ Hand every completed declaration, reused or appended, to an xt as
\ `( kind idx )`: 1 a library (LIBRARY-PATH), 2 a function (DECLARE), 3 a
\ selection write, unchanged ones included, with idx 0.
: OBSERVE ( [ n n -- ] -- ) OBSERVER-N OBSERVER ! ;
: UNOBSERVE ( -- ) 0 OBSERVER ! ;
: NOTIFY-SCOPE ( -- ) OBS-SCOPE 0 NOTIFY ;
\ The observer's cell.
: OBSERVER-CELL ( -- ptr n ) OBSERVER ;

\ errno is this package's own binding, written by hand because the declarer is
\ defined after this package closes. libc's location is thread-local and the
\ value is a four-byte C int.
: ERRNO ( -- n )
   RESET ERRNO-FN CALL-PTR LE:U32@ ;

ERRNO-SYMBOL$ PROCESS 0 DECLARE ERRNO-FN-CELL !

\ FORGET-SYMBOLS closes the library handles through this row.
s" dlclose" PROCESS 1 DECLARE DLCLOSE-FN-CELL !

\ OPEN-REPORT reads a failed dlopen's reason through this row.
s" dlerror" PROCESS 0 DECLARE DLERROR-FN-CELL !

\ A stripped image whose host includes FFI carries the declarations made while
\ loading the application in its DATA window, with foreign addresses unresolved
\ for the new process; the linker gets the table's exact storage through the two
\ callbacks. Retired rows drain before capture; each dynamic buffer's #base and
\ #ctl start empty in a fresh runtime. A stripped entry publishes the two table
\ bases after restoring DATA: their addresses vary with the maker's window,
\ while the descriptor bytes and counts stay identical for one application
\ source.
: AOT-CONTROLS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: carry fresh :}
   FN-N BYTE-VIEW CELL carry execute
   LIB-N BYTE-VIEW CELL carry execute
   FN-CAP BYTE-VIEW CELL carry execute
   LIB-CAP BYTE-VIEW CELL carry execute
   ERRNO-FN-CELL BYTE-VIEW CELL carry execute
   DLCLOSE-FN-CELL BYTE-VIEW CELL carry execute
   DLERROR-FN-CELL BYTE-VIEW CELL carry execute
   FN-TABLE BYTE-VIEW CELL fresh execute
   LIB-TABLE BYTE-VIEW CELL fresh execute
   PIN-FN BYTE-VIEW CELL fresh execute
   PIN-LIB BYTE-VIEW CELL fresh execute
   PIN-FN-CAP BYTE-VIEW CELL fresh execute
   PIN-LIB-CAP BYTE-VIEW CELL fresh execute
   RETIRED-BASE#base BYTE-VIEW CELL fresh execute
   RETIRED-BASE#ctl BYTE-VIEW DYNAMIC-STORAGE:CONTROL-CELLS cells fresh execute
   RETIRED-SIZE#base BYTE-VIEW CELL fresh execute
   RETIRED-SIZE#ctl BYTE-VIEW DYNAMIC-STORAGE:CONTROL-CELLS cells fresh execute
   RETIRED-COUNT BYTE-VIEW CELL fresh execute
   OBSERVER BYTE-VIEW CELL fresh execute
   FN-REGISTERED BYTE-VIEW CELL fresh execute ;


\ Stripped application runtime starts with no maker declarations. The
\ conditional lookup branch can still name these cells, so claim them fresh.
: MAKER-FRESH-CONTROLS ( [ ptr u8 n -- ] -- ) {: fresh :}
   MAKER-FN-N BYTE-VIEW CELL fresh execute
   MAKER-LIB-N BYTE-VIEW CELL fresh execute
   MAKER-FN-CAP BYTE-VIEW CELL fresh execute
   MAKER-LIB-CAP BYTE-VIEW CELL fresh execute
   MAKER-FN-TABLE BYTE-VIEW CELL fresh execute
   MAKER-LIB-TABLE BYTE-VIEW CELL fresh execute
   MAKER-DEPTH BYTE-VIEW CELL fresh execute ;

\ The fixed row headers contain cells; symbol names and library paths are
\ bytes. AOT sizes and copies these same extents before scanning header cells.
: AOT-TABLES-AT ( ptr u8 n ptr u8 n [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: fn:ptr fncap:n lib:ptr libcap:n cells bytes :}
   fncap 0 ?do
      fn i FN-ROW-BYTES * + {: row:ptr :}
      row FN-NAME-OFF cells execute
      row FN-NAME-OFF + FN-NAME-CAP bytes execute
   loop
   libcap 0 ?do
      lib i LIB-ROW-BYTES * + {: row:ptr :}
      row LIB-PATH-OFF cells execute
      row LIB-PATH-OFF + LIB-ROW-BYTES LIB-PATH-OFF - bytes execute
   loop ;

private

: TABLE-SPANS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: cells bytes :}
   cells cells AOT-CONTROLS
   cells MAKER-FRESH-CONTROLS
   FN-TABLE @ FN-CAP @ LIB-TABLE @ LIB-CAP @
      cells bytes AOT-TABLES-AT
   MAKER-FN-TABLE @ MAKER-FN-CAP @
      MAKER-LIB-TABLE @ MAKER-LIB-CAP @ cells bytes AOT-TABLES-AT
   PIN-FN @ PIN-FN-CAP @ PIN-LIB @ PIN-LIB-CAP @
      cells bytes AOT-TABLES-AT ;

public

: OWNED-BASES ( [ ptr u8 -- ] -- )
   {: claim :}
   FN-TABLE BYTE-VIEW claim execute
   LIB-TABLE BYTE-VIEW claim execute ;

: OWNED-CAPS ( [ ptr u8 -- ] -- )
   {: claim :}
   FN-CAP BYTE-VIEW claim execute
   LIB-CAP BYTE-VIEW claim execute ;

\ A stripped build loads its linker after the application's DATA span closes.
\ Remember the application's declaration boundary and current table allocation.
: IMAGE-MARK ( -- n n ptr u8 ptr u8 )
   FN-N @ LIB-N @ FN-TABLE @ LIB-TABLE @ ;

: IMAGE-CAPS ( -- n n )
   FN-CAP @ LIB-CAP @ ;

: OWNED-COUNTS ( [ ptr u8 -- ] -- )
   {: claim :}
   FN-N BYTE-VIEW claim execute
   LIB-N BYTE-VIEW claim execute ;


\ FUNCTION: - declare a foreign function, get a checked word.
\
\     VERSIONED-LIBRARY z 1                   \ libz.so.1 here, libz.1.dylib on macOS
\     FUNCTION: CRC-CALL crc32 ( n ptr u8 n -- n ) ;FUNCTION
\     LIBRARY /usr/lib/libSystem.B.dylib      \ a literal, or PROCESS-SYMBOLS (RTLD_DEFAULT)
\     FUNCTION: SQRT-CALL sqrt ( r -- r ) ;FUNCTION
\     FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- i32 )
\        1 $10 WRITES-BYTES                   \ sockaddr_in
\        2 $04 WRITES-BYTES                   \ socklen_t
\     ;FUNCTION
\
\ The Habu word name and the C symbol are separate tokens. A symbol is often not
\ a legal Habu name (__errno_location), and a package's own verb is usually not
\ the C one: UDP4:BIND is the module's API, bind is libc's.
\
\ The declared effect becomes the generated word's effect, with `f32` mapped
\ to Habu `r` on either side and an `i32` result mapped to `n`. It decides
\ every argument's staging: `n` (and any token that widens to it) is a VALUE!,
\ `r` is a C double FLOAT!, `f32` is a C float FLOAT32!, and `ptr u8` is a
\ READABLE! - read-only, extent 0 - unless a clause names it written:
\ `idx len WRITES-BYTES` for a fixed width, or `idx arg WRITES-ARG` when another
\ argument carries the length. A written pointer must state its extent because
\ the bounded call guards exactly the span it is given. The clauses are ordinary
\ words the interpreter runs between the two keywords, so their numbers are
\ literals and comments sit among them.
\ Convert a nominal at the call site: a declaration speaks the C function's own
\ types.
\
\ VERSIONED-LIBRARY, LIBRARY or PROCESS-SYMBOLS selects the library for the
\ declarations that follow. VERSIONED-LIBRARY takes a base name and a soname
\ version and renders the spelling this target loads (FFI:LIBRARY-NAME$).
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
\ record type aligned beyond 16 bytes is excluded. The whole extent is guarded.
\ The destination is one of the sixteen inputs, so at most fifteen C arguments
\ remain, placed as STRUCT-ARG's are. No VARIADIC, no write clause on the
\ storage, no x86-64. A null or misaligned destination is E-FFI-SYNTAX at the
\ call.
\
\ `idx bytes STRUCT-ARG` declares input idx, a `ptr u8`, the address of a C
\ struct of `bytes`, any positive count, passed by value: one C argument,
\ read at exactly `bytes` when the call is staged, so the callee never sees
\ the source. The record is neither an HFA nor an HVA, as for STRUCT-RESULT;
\ either travels in v registers and must not be declared. One of up to 16
\ bytes goes whole in the next integer registers, the last one's unused high
\ bytes zero; when too few of x0-x7 remain it goes whole on the stack and the
\ later integer arguments follow it there (AAPCS64 sets the next register to
\ x8). A larger record is passed as a pointer to a private, 16-byte aligned
\ copy in the native call frame, which lives exactly as long as the call. A
\ declaration may have up to eight float arguments beside its records; its
\ integer and pointer inputs must be cell-wide C types, since Apple arm64
\ packs two spilled C ints into one stack cell. The stack cells, the copies
\ and the padding that aligns each copy share sixteen cells, refused past
\ that at ;FUNCTION (E-FFI-ARITY). Excluded, because a byte count cannot
\ reveal them, are records aligned beyond 16 bytes and 16-byte aligned ones
\ of 16 bytes or fewer: Apple arm64 may start such a record in an odd
\ register like any other, but on the stack it starts at a 16-byte aligned
\ slot, so after nine longs an ordinary 16-byte record is at SP+8 and a
\ 16-byte aligned one at SP+16. The clause refuses, E-FFI-SYNTAX, outside a
\ declaration, twice on one input, on any input that is not a `ptr u8`, the
\ STRUCT-RESULT destination or the target of a write clause, beside VARIADIC
\ or an `r` or `f32` result, and on x86-64. It combines with STRUCT-RESULT:
\     FUNCTION: DEPARSE pg_query_deparse_protobuf ( ptr u8 ptr u8 -- )
\        0 16 STRUCT-ARG  16 STRUCT-RESULT
\     ;FUNCTION
\
\ Arguments take integer and float registers in declaration order.
\ A float result reads only the low 32 bits of its FP slot and widens to Habu
\ `r`. Variadic positions use C default promotions, so declare a promoted float
\ as `r`, never `f32`.
\ AAPCS64 takes integers in x0..x7 and floats in d0..d7. A declaration carrying
\ a float rides the ABI call, which packs no spill there, so it is refused past
\ eight of either kind (E-FFI-ARITY); an integer-only declaration rides
\ ffi-call-bounded, which spills past x7 itself. SysV takes six integer and
\ eight float registers, then prepacked stack slots in argument order. Every
\ SysV overflow call rides the ABI trampoline so mixed kinds and bounded
\ pointer extents keep their own stack positions; a C float past the eighth
\ float register has no stack staging and is refused (E-FFI-ARITY).
\
\ THE GENERATED TEXT DEFINES WORDS AND LEAVES NOTHING ON THE STACK: it runs
\ through the loader's evaluate-closed INCLUDE-EVALUATE, where residue is
\ E-EVAL-RESIDUE, so each declaration is one complete colon definition with no
\ interpreted residue (src/lib/ffi-test.f asserts DEPTH across one).

private

$800 constant GEN-CAP
$100 constant EFF-CAP
$40 constant TOK-CAP
$10 constant ARG-MAX
8 constant REG-MAX                        \ AAPCS64 integer / both ABIs' float slots
6 constant X86-INT-REG-MAX                \ SysV rdi, rsi, rdx, rcx, r8, r9
10 constant DEC-BASE
16 constant HEX-BASE
$20 constant SP-C
$24 constant HEX-C                        \ '$' - the hex literal prefix

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
4 constant R-I32                          \ a C int: sign-extend the low half
5 constant R-U32                          \ a C unsigned int: mask the low half
6 constant R-FLOAT32                      \ a C float: widen the low half

0 constant C-BOUNDED
1 constant C-POINTER
2 constant C-ABI
3 constant C-ABI-R
4 constant C-STRUCT

-1 constant NO-EXT

LIBRARY-PATH-CAP 1 - CODEGEN:BUFFER LIB-NAME
GEN-CAP CODEGEN:BUFFER GEN
EFF-CAP CODEGEN:BUFFER EFF
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
variable VAR-START
variable STRUCT-BYTES                      \ STRUCT-RESULT's record width; 0 when none
variable CUR-LIB
variable OPEN                              \ 0 closed, else 1 + the open declaration's clauses
variable SCOPE-WID                         \ the wordlist LIBRARY/PROCESS-SYMBOLS was stated in
variable SCOPE-SET                         \ ... and whether one was stated at all
variable SCOPE-HOOK

\ The token cells a library selection shares with a declaration, at rest.
: REST-TOKENS ( -- )
   TOK-CAP 0 ?do 0 TOK-BUF i + c! 0 BASE-BUF i + c! loop
   0 TOK-U ! 0 BASE-U ! ;

\ The declaration's working state at rest: every cell from NAME-BUF through
\ STRUCT-BYTES at zero.
: REST ( -- )
   TOK-CAP 0 ?do 0 NAME-BUF i + c! 0 SYM-BUF i + c! loop
   0 NAME-U ! 0 SYM-U !
   REST-TOKENS
   ARG-MAX 0 ?do
      0 ARG-KIND i cells + ! 0 ARG-REG i cells + ! 0 ARG-EXT i cells + !
      0 ARG-EXT-ARG i cells + ! 0 ARG-SBYTES i cells + ! 0 ARG-PAY i cells + !
   loop
   0 ARG-N ! 0 RES-KIND !
   0 VAR-START ! 0 STRUCT-BYTES ! ;

: KEEP-REST ( R [ R -- S ] -- S )
   [: OPEN @ 0= if REST else REST-TOKENS then ;] finally ;

\ ---- the declaration token stream -----------------------------------------
\ parse-name's span is transient, so every token is copied before the next one
\ is read and every comparison reads the copy.

: TOK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FFI-SYNTAX throw then
   u TOK-CAP > if E-FFI-SYNTAX throw then
   a TOK-BUF u BYTE-COPY
   u TOK-U ! ;

: TOK@ ( -- )
   parse-name TOK! ;

: TOK$ ( -- ptr u8 n )
   TOK-BUF TOK-U @ ;

: TOK-IS? ( ptr u8 n -- bool )
   TOK$ STR= ;

: NAME! ( -- )
   TOK$ {: a:ptr u:n :}
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: SYM! ( -- )
   TOK$ {: a:ptr u:n :}
   a SYM-BUF u BYTE-COPY
   u SYM-U ! ;

\ VERSIONED-LIBRARY's two tokens. The base name is copied out of the token
\ buffer because reading the version token overwrites it.
: BASE! ( -- )
   TOK$ {: a:ptr u:n :}
   a BASE-BUF u BYTE-COPY
   u BASE-U ! ;

: TOK>VERSION ( -- n )
   TOK$ STR-PARSE-POS MATCH option
      none OF E-FFI-SYNTAX throw ENDOF
      some OF ENDOF
   ;MATCH ;

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

: EFF+ ( ptr u8 n -- )
   EFF CODEGEN:APPEND-STRING
   SP-C EFF CODEGEN:APPEND-BYTE ;

: TOK-KEEP ( -- )
   TOK$ EFF+ ;

: ARG+ ( n -- )
   {: kind:n :}
   ARG-N @ ARG-MAX >= if E-FFI-ARITY throw then
   kind ARG-KIND ARG-N @ cells + !
   NO-EXT ARG-N @ ARG-EXT!
   NO-EXT ARG-N @ ARG-EXT-ARG!
   0 ARG-N @ ARG-SBYTES!
   ARG-N @ 1 + ARG-N ! ;

\ A pointer is `ptr u8`: a byte span with a length the declaration states. `ptr`
\ alone, or over any other pointee, has no extent this side can guard.
: PTR-TAIL ( -- )
   TOK@
   s" u8" TOK-IS? 0= if E-FFI-SYNTAX throw then
   TOK-KEEP ;

: IN-TOKEN ( -- )
   s" ptr" TOK-IS? if TOK-KEEP PTR-TAIL K-POINTER ARG+ exit then
   s" r" TOK-IS? if TOK-KEEP K-FLOAT ARG+ exit then
   s" f32" TOK-IS? if s" r" EFF+ K-FLOAT32 ARG+ exit then
   TOK-KEEP K-VALUE ARG+ ;

: OUT-TOKEN ( -- )
   RES-KIND @ R-NONE <> if E-FFI-SYNTAX throw then
   s" ptr" TOK-IS? if TOK-KEEP PTR-TAIL R-POINTER RES-KIND ! exit then
   s" r" TOK-IS? if TOK-KEEP R-FLOAT RES-KIND ! exit then
   s" f32" TOK-IS? if s" r" EFF+ R-FLOAT32 RES-KIND ! exit then
   s" i32" TOK-IS? if s" n" EFF+ R-I32 RES-KIND ! exit then
   s" u32" TOK-IS? if TOK-KEEP R-U32 RES-KIND ! exit then
   TOK-KEEP R-VALUE RES-KIND ! ;

: INPUTS ( -- )
   begin
      TOK@
      s" --" TOK-IS? if TOK-KEEP exit then
      s" )" TOK-IS? if E-FFI-SYNTAX throw then
      IN-TOKEN
   again ;

: OUTPUTS ( -- )
   begin
      TOK@
      s" )" TOK-IS? if exit then
      s" --" TOK-IS? if E-FFI-SYNTAX throw then
      OUT-TOKEN
   again ;

: EFFECT ( -- )
   TOK@
   s" (" TOK-IS? 0= if E-FFI-SYNTAX throw then
   EFF CODEGEN:RESET
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

\ The first n arguments take registers in order, floats and integers numbered
\ apart. Answers the integer and float registers taken.
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

\ A struct-bearing declaration is planned by AAPCS64 over its C arguments in
\ order; the STRUCT-RESULT destination, the last input, is none. Floats take
\ v0-v7. An integer argument takes the next of x0-x7, else the next stack
\ cell. A record of up to 16 bytes takes its registers whole, or when too few
\ remain its stack cells whole, and the next register becomes x8. A larger
\ record's pointer is an integer argument, and its copy follows every stack
\ argument from an even cell, 16-byte aligned on the 16-byte aligned native
\ stack. A register is kept as its number and a stack cell as -1-cell, as on
\ x86-64; a record of 16 bytes or fewer has no copy, kept as 0.
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
\ declaration's clauses to the rule this way (src/habu/verify-source.f).
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
   int stk C-BOUNDED ;

\ ---- the generated definition ----------------------------------------------

: GEN+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;
: GEN-N ( n -- ) GEN CODEGEN:APPEND-DECIMAL ;
: GEN-SP ( -- ) SP-C GEN CODEGEN:APPEND-BYTE ;
: GEN-ARG ( n -- ) s" A" GEN+ GEN-N ;

: NEEDS-LOCALS? ( -- bool )
   false ARG-N @ 0 ?do
      i ARG-EXT-ARG@ NO-EXT <> or
   loop ;

: LOCALS ( -- )
   ARG-N @ 0= if exit then
   s" {: " GEN+
   ARG-N @ 0 ?do
      i GEN-ARG
      i ARG-KIND@ K-VALUE = if s" :n" GEN+ then
      i ARG-KIND@ K-FLOAT = if s" :r" GEN+ then
      i ARG-KIND@ K-FLOAT32 = if s" :r" GEN+ then
      GEN-SP
   loop
   s" :} " GEN+ ;

: STAGE-VALUE ( n -- ) {: i:n :}
   i ARG-REG@ GEN-N s"  FFI:VALUE! " GEN+ ;

: STAGE-FLOAT ( n -- ) {: i:n :}
   i ARG-REG@ GEN-N s"  FFI:FLOAT! " GEN+ ;

: STAGE-FLOAT32 ( n -- ) {: i:n :}
   i ARG-REG@ GEN-N s"  FFI:FLOAT32! " GEN+ ;

: STAGE-READABLE ( n -- ) {: i:n :}
   i ARG-REG@ GEN-N s"  FFI:READABLE! " GEN+ ;

: STAGE-WRITABLE ( n -- ) {: i:n :}
   i ARG-EXT@ NO-EXT <> if i ARG-EXT@ GEN-N else i ARG-EXT-ARG@ GEN-ARG then
   GEN-SP i ARG-REG@ GEN-N s"  FFI:WRITABLE! " GEN+ ;

: STAGE-POINTER ( n -- ) {: i:n :}
   i ARG-EXT@ NO-EXT <> if i STAGE-WRITABLE exit then
   i ARG-EXT-ARG@ NO-EXT <> if i STAGE-WRITABLE exit then
   i STAGE-READABLE ;

\ A planned stack cell is -1-cell in ARG-REG; a macOS variadic argument's
\ cell is its position past the fixed ones.
: STACK-POS ( n -- ) {: i:n :}
   i ARG-REG@ 0 < if i ARG-REG@ negate 1- GEN-N exit then
   i VAR-START @ - GEN-N ;
: STAGE-SPILL ( n -- ) {: i:n :}
   i ARG-KIND@ K-FLOAT = if
      i STACK-POS s"  FFI:STACK-FLOAT! " GEN+ exit then
   i ARG-KIND@ K-VALUE = if
      i STACK-POS s"  FFI:STACK-VALUE! " GEN+ exit then
   i ARG-EXT@ NO-EXT <> i ARG-EXT-ARG@ NO-EXT <> or if
      i ARG-EXT@ NO-EXT <> if i ARG-EXT@ GEN-N else i ARG-EXT-ARG@ GEN-ARG then
      GEN-SP i STACK-POS s"  FFI:STACK-WRITABLE! " GEN+ exit
   then
   i STACK-POS s"  FFI:STACK-READABLE! " GEN+ ;

: STAGE-REG ( n -- ) {: i:n :}
   i ARG-KIND@ K-FLOAT = if i STAGE-FLOAT exit then
   i ARG-KIND@ K-FLOAT32 = if i STAGE-FLOAT32 exit then
   i ARG-KIND@ K-VALUE = if i STAGE-VALUE exit then
   i STAGE-POINTER ;

: STAGE-ONE ( n bool -- ) {: i:n named:bool :}
   named if i GEN-ARG GEN-SP then
   HB-TARGET-LINUX-X86-64? i ARG-REG@ 0 < and if i STAGE-SPILL exit then
   HB-TARGET-MACOS? VAR-START @ 0 >= and i VAR-START @ >= and if
      i STAGE-SPILL exit
   then
   i STAGE-REG ;

: STAGE ( bool -- ) {: named:bool :}
   ARG-N @ 0 ?do
      named if i else ARG-N @ i 1+ - then named STAGE-ONE
   loop ;

: CALL-WORD ( n -- )
   {: kind:n :}
   RES-KIND @ R-FLOAT32 = if s" FFI:CALL-ABI-F32" GEN+ exit then
   kind C-ABI-R = if s" FFI:CALL-ABI-R" GEN+ exit then
   kind C-ABI = if
      RES-KIND @ R-POINTER = if s" FFI:CALL-ABI-PTR"
      else s" FFI:CALL-ABI" then GEN+ exit
   then
   kind C-POINTER = if s" FFI:CALL-PTR" GEN+ exit then
   s" FFI:CALL" GEN+ ;

\ ---- the row this declaration takes ----------------------------------------

: DIAG+ ( ptr u8 n -- )
   DIAG CODEGEN:APPEND-STRING ;

\ A library name spelled in the other target's convention is refused where it
\ is stated, not at the first call in whatever module declared it; the line
\ names the path and the target this build loads for. A rendered name passes by
\ construction.
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

: RESULT-TAIL ( -- )
   RES-KIND @ R-NONE = if s"  drop" GEN+ exit then
   RES-KIND @ R-I32 = if s"  FFI:I32-RESULT" GEN+ exit then
   RES-KIND @ R-U32 = if s"  $FFFFFFFF and" GEN+ then ;

: STAGE-RECORD ( n -- ) {: i:n :}
   i ARG-SBYTES@ GEN-N GEN-SP
   i ARG-SBYTES@ 16 <= if
      i ARG-REG@ 0 < if i STACK-POS s"  FFI:STRUCT-STACK! " GEN+ exit then
      i ARG-REG@ GEN-N s"  FFI:STRUCT-REGS! " GEN+ exit
   then
   i ARG-PAY@ GEN-N GEN-SP
   i ARG-REG@ 0 < if i STACK-POS s"  FFI:STRUCT-COPY-STACK! " GEN+ exit then
   i ARG-REG@ GEN-N s"  FFI:STRUCT-COPY-REG! " GEN+ ;

: STAGE-C-ARG ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP
   i ARG-SBYTES@ 0 > if i STAGE-RECORD exit then
   i ARG-REG@ 0 < if i STAGE-SPILL exit then
   i STAGE-REG ;

: STRUCT-BODY ( n n -- )
   SLOT {: slot:n :}
   LOCALS
   slot GEN-N s"  FFI:RESOLVE-ROW FFI:RESET " GEN+
   C-ARGS 0 ?do i STAGE-C-ARG loop
   STRUCT-BYTES @ 0 > if
      ARG-N @ 1- GEN-ARG GEN-SP STRUCT-BYTES @ GEN-N s"  FFI:RESULT-DEST! " GEN+
   then
   slot GEN-N
   STRUCT-BYTES @ 0 > if
      s"  FFI:CALL-STRUCT " GEN+
      ARG-N @ 1- GEN-ARG GEN-SP STRUCT-BYTES @ GEN-N s"  FFI:STORE-RESULT" GEN+ exit
   then
   RES-KIND @ R-POINTER = if s"  FFI:CALL-STRUCT-PTR" GEN+ exit then
   s"  FFI:CALL-STRUCT drop" GEN+
   RESULT-TAIL ;

: EMIT ( n n n -- )
   {: int:n stk:n kind:n :}
   GEN CODEGEN:RESET
   s" : " GEN+ NAME-BUF NAME-U @ GEN+
   s"  ( " GEN+ EFF CODEGEN:CONTENTS GEN+ s" ) " GEN+
   kind C-STRUCT = if int stk STRUCT-BODY s"  ;" GEN+ exit then
   NEEDS-LOCALS? {: named:bool :}
   named if LOCALS then
   \ The mixed ABI guard visits all nine register extents. Bounded calls guard
   \ only the row's argc live slots, whose staging also writes each extent.
   kind C-ABI = kind C-ABI-R = or if s" FFI:RESET " GEN+ then
   named STAGE
   int stk SLOT GEN-N GEN-SP
   kind CALL-WORD
   RESULT-TAIL
   s"  ;" GEN+ ;

\ The selection scope is the current wordlist, not the loading file:
\ INCLUDE-PATH names the last file opened (src/lib/codegen.f after `require
\ src/lib/ffi-abi.f`), so every consumer would share one identity. A wordlist is
\ saved and restored with the package scope and separates two consumers whose
\ requires end at one file. A selection belongs to the source load that made it:
\ the source closes before capture, and its host wordlist id is not an image id.
: CLEAR-SCOPE ( -- )
   0 SCOPE-WID !
   0 SCOPE-SET !
   NOTIFY-SCOPE ;

: ENSURE-SCOPE-HOOK ( -- )
   SCOPE-HOOK @ 0= if
      [: CLEAR-SCOPE ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT
      1 SCOPE-HOOK !
   then ;

: RECORD-SCOPE ( -- )
   ENSURE-SCOPE-HOOK
   get-current SCOPE-WID !
   1 SCOPE-SET !
   NOTIFY-SCOPE ;

: CHECK-SCOPE ( -- )
   SCOPE-SET @ 0= if E-FFI-LIBRARY throw then
   get-current SCOPE-WID @ <> if E-FFI-LIBRARY throw then ;

\ These context cells survive in a baked engine even when their private
\ variable records do not. FILE replay and admission use the same extents.
: CONTEXT-CELLS ( [ ptr u8 n -- ] -- ) {: keep :}
   CUR-LIB BYTE-VIEW CELL keep execute
   SCOPE-WID BYTE-VIEW CELL keep execute
   SCOPE-SET BYTE-VIEW CELL keep execute
   OPEN BYTE-VIEW CELL keep execute
   NAME-U BYTE-VIEW CELL keep execute
   SYM-U BYTE-VIEW CELL keep execute
   ARG-KIND ARG-MAX cells keep execute
   ARG-EXT ARG-MAX cells keep execute
   ARG-EXT-ARG ARG-MAX cells keep execute
   ARG-SBYTES ARG-MAX cells keep execute
   ARG-N BYTE-VIEW CELL keep execute
   RES-KIND BYTE-VIEW CELL keep execute
   VAR-START BYTE-VIEW CELL keep execute
   STRUCT-BYTES BYTE-VIEW CELL keep execute ;

public

\ A cold FILE import can restore a selection made before a child FILE and then
\ continue with FUNCTION:. Install the matching cleanup before capture; the
\ hook flag itself belongs to the live registry, not the serialized bytes.
: ARM-SCOPE ( -- ) ENSURE-SCOPE-HOOK ;

\ The declarer's selection as a file object carries it: the library index,
\ the wordlist it was stated in, and whether one is stated. SELECT restores
\ it at an install as RECORD-SCOPE made it, hook included. Like
\ RECORD-SCOPE it reports the write, an unchanged one included: a file's
\ object must record the selection a write replaced.
: SELECTION ( -- n n bool ) CUR-LIB @ SCOPE-WID @ SCOPE-SET @ 0<> ;
: SELECT ( n n bool -- )
   {: lib:n wid:n set:bool :}
   lib CUR-LIB ! wid SCOPE-WID !
   set if ENSURE-SCOPE-HOOK 1 else 0 then SCOPE-SET !
   NOTIFY-SCOPE ;

\ The declaration between FUNCTION: and ;FUNCTION: 0 when none is open, else
\ one more than the clauses it holds. A top-level run that changes it is
\ admitted by that change (tools/native-unit-dependency.f RUN-JUDGE); a
\ segment that opens or closes with one pending refuses
\ (tools/native-unit-object.f DECL-CLOSED-CK).
: PENDING ( -- n ) OPEN @ ;

\ Nothing is published before ;FUNCTION, and the row is registered as the text
\ is rendered, so a refused declaration leaves no half-built word.
: OPEN-FUNCTION ( -- )
   [: CHECK-SCOPE
      OPEN @ 0 <> if E-FFI-SYNTAX throw then
      0 ARG-N ! -1 VAR-START ! 0 STRUCT-BYTES !
      R-NONE RES-KIND !
      TOK@ NAME!
      TOK@ SYM!
      EFFECT
      1 OPEN ! ;] KEEP-REST ;

\ The rendered text calls only FFI words, which no scratch cell reaches, so
\ the scratch rests before the text runs.
: CLOSE-FUNCTION ( -- )
   [: OPEN @ 0= if E-FFI-SYNTAX throw then
      0 OPEN !
      PLAN-CALL
      EMIT ;] KEEP-REST
   GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

private

: PARSER-SPANS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: cells bytes :}
   cells CONTEXT-CELLS
   LIB-NAME cells CODEGEN:SCAN-SPANS
   GEN cells CODEGEN:SCAN-SPANS
   EFF cells CODEGEN:SCAN-SPANS
   DIAG cells CODEGEN:SCAN-SPANS
   NAME-BUF TOK-CAP bytes execute
   SYM-BUF TOK-CAP bytes execute
   TOK-BUF TOK-CAP bytes execute
   BASE-BUF TOK-CAP bytes execute ;

\ A refused clause ABANDONS the declaration rather than leaving it half open:
\ the next FUNCTION: in the file would otherwise refuse too, and the first
\ diagnostic would name the wrong declaration.
: REFUSE ( -- )
   0 OPEN !
   E-FFI-SYNTAX throw ;

: CLAUSE-TARGET ( n -- ) {: idx:n :}
   OPEN @ 0= if REFUSE then
   idx 0 < if REFUSE then
   idx ARG-N @ >= if REFUSE then
   idx ARG-KIND@ K-POINTER <> if REFUSE then
   idx ARG-EXT@ NO-EXT <> if REFUSE then
   idx ARG-EXT-ARG@ NO-EXT <> if REFUSE then
   idx ARG-SBYTES@ 0 <> if REFUSE then
   STRUCT-BYTES @ 0 <> idx ARG-N @ 1- = and if REFUSE then ;

: VARIADIC-CLAUSE ( n -- ) {: fixed:n :}
   OPEN @ 0= if REFUSE then
   VAR-START @ -1 <> if REFUSE then
   STRUCT-BYTES @ 0 <> RECORDS? or if REFUSE then
   fixed 1 < fixed ARG-N @ > or if REFUSE then
   ARG-N @ fixed ?do
      i ARG-KIND@ K-FLOAT32 = if REFUSE then
   loop
   fixed VAR-START !
   1 OPEN +! ;

\ Argument idx is written by the callee: the bounded call guards exactly the
\ span named here, a fixed width or the argument that carries the length.
: EXTENT-CLAUSE ( n n -- ) {: idx:n len:n :}
   idx CLAUSE-TARGET
   len 0 <= if REFUSE then
   len idx ARG-EXT!
   1 OPEN +! ;

: LENGTH-CLAUSE ( n n -- ) {: idx:n src:n :}
   idx CLAUSE-TARGET
   src 0 < if REFUSE then
   src ARG-N @ >= if REFUSE then
   src ARG-KIND@ K-VALUE <> if REFUSE then
   src idx ARG-EXT-ARG!
   1 OPEN +! ;

\ The last input, a `ptr u8` no write clause names, receives a C struct of
\ bytes returned by value; the declaration has no output. AAPCS64 only.
: STRUCT-RESULT-CLAUSE ( n -- ) {: bytes:n :}
   OPEN @ 0= if REFUSE then
   HB-TARGET-LINUX-X86-64? if REFUSE then
   STRUCT-BYTES @ 0 <> if REFUSE then
   bytes 0 <= if REFUSE then
   RES-KIND @ R-NONE <> if REFUSE then
   VAR-START @ -1 <> if REFUSE then
   ARG-N @ 0= if REFUSE then
   ARG-N @ 1- CLAUSE-TARGET
   bytes STRUCT-BYTES !
   1 OPEN +! ;

\ Input idx, a `ptr u8` no other clause names, is the address of a C struct of
\ bytes passed by value. The struct call returns x0 and x1 only, so a float
\ result has no shape here. AAPCS64 only.
: STRUCT-ARG-CLAUSE ( n n -- ) {: idx:n bytes:n :}
   OPEN @ 0= if REFUSE then
   HB-TARGET-LINUX-X86-64? if REFUSE then
   idx CLAUSE-TARGET
   bytes 0 <= if REFUSE then
   VAR-START @ -1 <> if REFUSE then
   RES-KIND @ R-FLOAT = RES-KIND @ R-FLOAT32 = or if REFUSE then
   bytes idx ARG-SBYTES!
   1 OPEN +! ;
public

: SET-VARIADIC ( n -- ) [: VARIADIC-CLAUSE ;] KEEP-REST ;
: WRITE-EXTENT ( n n -- ) [: EXTENT-CLAUSE ;] KEEP-REST ;
: WRITE-LENGTH-ARG ( n n -- ) [: LENGTH-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-RESULT ( n -- ) [: STRUCT-RESULT-CLAUSE ;] KEEP-REST ;
: SET-STRUCT-ARG ( n n -- ) [: STRUCT-ARG-CLAUSE ;] KEEP-REST ;

: SELECT-LIBRARY ( ptr u8 n -- ) {: path:ptr u:n :}
   u 0 <= u LIBRARY-PATH-CAP >= or if E-FFI-SYNTAX throw then
   path u CHECK-SPELLING
   path u LIBRARY-PATH CUR-LIB !
   RECORD-SCOPE ;

\ The two tokens are rendered for this target and take the literal form's path,
\ so the scope recording and the table guard are shared.
: SELECT-VERSIONED-LIBRARY ( -- )
   [: TOK@ BASE!
      BASE-BUF BASE-U @ TOK@ TOK>VERSION LIB-NAME LIBRARY-NAME$ SELECT-LIBRARY ;]
   KEEP-REST ;

: SELECT-PROCESS ( -- )
   PROCESS CUR-LIB !
   RECORD-SCOPE ;

\ The storage FFI restores itself: the registry tables and the declaration
\ parser's cells and buffers.
: SCAN-SPANS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: cells bytes :}
   cells bytes TABLE-SPANS
   cells bytes PARSER-SPANS ;

\ Both wordlists are protected here, after FFI's last definition: an engine
\ that loads this file after its seal (tools/build-fixpoint.f's capture host)
\ refuses any definition into a protected wordlist and exits 84.
private

get-current prot-wid-add

public

get-current prot-wid-add

;package

\ The declaration keywords are global: a package word ending in a colon cannot
\ be reached through a qualifier, and a declarer reads bare from any package
\ anyway, like DEFTYPE and CAST:. All three parse the live input stream, which
\ their ( -- ) rows do not model, so they are top-level-interpret-only.
: LIBRARY ( -- )
   parse-name FFI:SELECT-LIBRARY ;

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
