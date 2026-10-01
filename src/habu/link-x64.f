\ link-x64.f - the captured dictionary laid out for an x86-64 image, package
\ X64LINK. It is the write-time twin of the ARM64 seed's boot passes (src/habu/
\ habu2.f EM-SEED-DICT, EM-AOT-REGISTER-RECS and EMIT-AOT-PROT-RESTORE): an
\ x86-64 image maps its code region and DATA at fixed addresses (docs/x86-64.md
\ "Fixed segments"), so what the ARM64 seed computes at every boot is computed
\ here once, into byte rows the image writer places.
\
\ - THE RECORDS. dict[0, PRIMS) are the kernel's bodies in ENGINE-PRIMS order
\   (src/habu/primitive-registry.f, filled by src/habu/kernel-x64.f KERNEL,):
\   each entry is its first label's address in the text, its span exact, its
\   flags ENGINE-PRIMS:DNAME with the primitive's min-in. dict[PRIMS, RECORDS)
\   are the capture's shipped records in capture order. Record n lies at REC-VA,
\   n * DREC bytes into the region, where the boot points r13.
\ - THE ROUTINES. A code record's routine is its emission in the capture's
\   shadow (src/habu/aot-decl.f AOT-SHADOW), copied as the capture left it to a
\   CODE-SLOT of the region's code band and filled to the next slot with int3,
\   as code-publish fills. A `does>` companion enters its definer's emission at
\   the clause. The long names lie ahead of the routines, zero-filled to a slot.
\   The sites stay unlinked: the next leaf links them in CODE$, through PLACED
\   and REC-VA, and builds or refuses the routines a capture has none for.
\ - THE WIDS. A captured wid is a window coordinate (WID-REL-BASE): 0 stays the
\   global wordlist and any other one becomes T0 + (wid - WID-REL-BASE), the
\   image's numbering from FIRST-DYNAMIC-WID, as AOT-WINDOW:REBASE-WID, rebases
\   against the booting engine's WIDN. The image's WIDN is T0 plus the window's
\   span, which counts the wordlists no record names.
\ - THE BITMAP. Each protected-wid row, a zero-based window offset, sets the bit
\   of T0 + row in BITS$, the PROT-BITS band, as AOT-WINDOW:SEAL-WIDS, does.
\ - THE NAME INDEX. The writer builds it, into INDEX$: HIDX-SLOTS u32 slots
\   keyed and probed exactly as kernel-x64.f REBUILD-HELPER, fills them from the
\   live records [0, RECORDS), with CLAIMS one claim per record. It is a
\   function of the records alone, and they of the capture and the kernel: no
\   host address, wid or record number reaches it. Measured: the window of
\   test/x86-64-link-records.f captured by a host with one record, one
\   wordlist and one DATA cell more ahead of it, which moves the window's first
\   record, first wordlist and DATA base, lays out the same index, records, code
\   band and bitmap, by digest. So the boot rebuilds nothing: the image carries
\   the table at INDEX-OFF in DATA, the heap floor moves to HEAP-FLOOR above it,
\   and HIDXP-CELL holds INDEX-VA. The kernel's own rows keep it from there.
\
\ Every refusal is by name and comes before the first byte is laid: a code
\ record with no routine, a routine that names no shipped code record, a wid
\ outside the capture window, a protected wid at or past PROT-WID-MAX, more
\ records than DICT-CAP, and routines that pass the code ceiling.
\
\ LOAD ORDER. The x86-64 files bind `using X64CODE`, whose public tails
\ src/arch/arm64/icode.f also defines as globals (CODE, LBL, ASM-LEN), and
\ src/habu/aot-decl.f loads behind that file, for AOT-SECTION-CAP. So a loader
\ brings the x86-64 side in first, elf.f behind src/os/image-bytes.f under
\ `using X64CODE` as elf.f's own header asks, then the ARM64 code layer and the
\ capture's declarations, and this file last; it reads X64CODE qualified.
require lib/le.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/elf.f
require src/os/linux-x86-64/target-layout.f
require src/habu/layout.f
require src/habu/code-span.f
require src/habu/primitive-registry.f
require src/habu/kernel-x64.f
require src/habu/aot-decl.f

package X64LINK
using AOT-BUF

\ The rc of a capture this layout cannot place: the capture's own refusals'.
74 constant REFUSE-RC
$CC constant INT3
$FFFFFFFF constant PKG-MARK            \ a compact row's wid word on a package row
\ The twin of kernel-x64.f HASH, and of habu1.f C-HIDX-HASH: FNV-1a over the
\ name folded to lower case.
$CBF29CE484222325 constant FNV-BASIS
$100000001B3 constant FNV-PRIME


DYNAMIC-BUFFER DICT-STORAGE n
DYNAMIC-BUFFER CODE-STORAGE n
DYNAMIC-BUFFER INDEX-STORAGE n
DYNAMIC-BUFFER PLACE-STORAGE n         \ shadow record row -> its code band offset
PROT-BITS-BYTES BUFFER: BITS
variable PRIM-N
variable REC-TOTAL
variable NAMES-LEN                     \ long-name bytes ahead of the routines
variable NAME-AT                       \ the next long name's code band offset
variable CODE-END                      \ code band bytes laid, a CODE-SLOT multiple
variable CUR                           \ the shadow row a walk of the records is at
variable PREV                          \ the emission a walk of the rows copied last
variable LO
variable HI

: REFUSE ( ptr u8 n -- ) REFUSE-RC die ;
: SLOT-UP ( n -- n ) X64KERNEL:CODE-SLOT 1- + X64KERNEL:CODE-SLOT negate and ;

\ ---- the capture's rows ---------------------------------------------------------
: CREC ( n -- ptr u8 ) {: k:n :} AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;
: CREC@ ( n n -- n ) {: k:n f:n :} k CREC f + LE:U32@ ;
: PKG? ( n -- bool ) 16 CREC@ PKG-MARK = ;
: POOL ( n -- ptr u8 ) {: k:n :} AOT-NAMES-BUF@ k 8 CREC@ + ;
: CREC-NAME$ ( n -- ptr u8 n ) POOL {: e:ptr :} e 1+ e c@ ;
: PWIN@ ( n -- n ) {: r:n :} AOT-PWIN-BUF@ r 4 * + LE:U32@ ;
: SH@ ( n n -- n ) {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + LE:U32@ ;
: SH-REC ( n -- n ) 0 SH@ ;
: SH-AT ( n -- n ) 4 SH@ ;
: SH-LEN ( n -- n ) 8 SH@ ;
: SH-ENTRY ( n -- n ) 12 SH@ ;

: WINDOW. ( n -- ) {: k:n :}
   s" x64link: window record " type k CREC-NAME$ type ;

\ ---- refusals --------------------------------------------------------------------
: NO-ROUTINE ( n -- )
   WINDOW. s"  has no x86-64 routine" type cr
   s" x64link: a code record the capture's shadow carries no routine for" REFUSE ;

: STRAY-ROUTINE ( n -- ) {: r:n :}
   s" x64link: the shadow routine of window record " type r SH-REC .
   r SH-REC AOT-REC-N @ < if
      s" lands on package row " type r SH-REC CREC-NAME$ type
   else
      s" names none of the " type AOT-REC-N @ . s" shipped records" type
   then cr
   s" x64link: the shadow's routines do not match the shipped records" REFUSE ;

: WID-OUT ( n n -- ) {: k:n w:n :}
   k WINDOW. s"  carries wid " type w .
   s" outside the window's " type AOT-WID-SPAN @ . s" wordlists" type cr
   s" x64link: a wid outside the capture window" REFUSE ;

: PROT-OUT ( n -- ) {: row:n :}
   s" x64link: protected-wid row " type row .
   s" is window wordlist " type row PWIN@ . s" of " type AOT-WID-SPAN @ . cr
   s" x64link: a protected wid outside the window or at PROT-WID-MAX" REFUSE ;

\ ---- the wids -------------------------------------------------------------------
: WID-IN? ( n -- bool ) {: w:n :}
   w 0= if true exit then
   w WID-REL-BASE - {: o:n :}
   o 0 >=  o AOT-WID-SPAN @ <  and ;

: ?WID ( n n -- ) {: k:n w:n :} w WID-IN? 0= if k w WID-OUT then ;

\ The image's id for a captured wid the checks admitted.
: IMAGE-WID ( n -- n ) {: w:n :}
   w 0= if 0 exit then
   w WID-REL-BASE - FIRST-DYNAMIC-WID + ;

\ ---- the checks, before any byte --------------------------------------------------
\ The shadow's rows ascend by record, one per routine (src/habu/aot-file.f
\ ?SH-RECS), so one walk beside the shipped rows pairs each code record with its
\ routine and finds a routine on a package row or past the last row.
: ?ROUTINES ( -- )
   0 CUR !
   AOT-REC-N @ 0 ?do
      CUR @ AOT-SHADOW:REC-N @ < if CUR @ SH-REC i = else false then {: on:bool :}
      i PKG? if
         on if CUR @ STRAY-ROUTINE then
      else
         on 0= if i NO-ROUTINE then
         1 CUR +!
      then
   loop
   CUR @ AOT-SHADOW:REC-N @ < if CUR @ STRAY-ROUTINE then ;

: ?WIDS ( -- )
   AOT-REC-N @ 0 ?do
      i PKG? if
         i i 0 CREC@ ?WID  i i 4 CREC@ ?WID
      else
         i i 16 CREC@ ?WID
      then
   loop
   AOT-PWIN-N @ 0 ?do
      i PWIN@ {: o:n :}
      o AOT-WID-SPAN @ >=  o FIRST-DYNAMIC-WID + PROT-WID-MAX >= or if i PROT-OUT then
   loop ;

\ An out-of-line name lies in the code band, ahead of the routines: a
\ primitive's past DNAME-INL, as ENGINE-EMIT:EMIT-DICT keeps it, and a window
\ record's whose flags say DNAME-EXT. A `does>` companion's is one of those at
\ any length: does-record writes the parent's name and `;does` at CP.
: EXT? ( n -- bool ) 12 CREC@ 2 and 0<> ;

: NAMES-SIZE ( -- n )
   0
   ENGINE-PRIMS:COUNT 0 ?do
      i ENGINE-PRIMS:NAME-LEN {: len:n :}
      len DNAME-INL > if len + then
   loop
   AOT-REC-N @ 0 ?do i EXT? if i POOL c@ + then loop ;

\ The code band's bytes: the names, then each emission from a slot.
: CODE-SIZE ( -- n )
   NAMES-SIZE SLOT-UP
   -1 PREV !
   AOT-SHADOW:REC-N @ 0 ?do
      i SH-AT PREV @ <> if i SH-LEN SLOT-UP +  i SH-AT PREV ! then
   loop ;

: ?FITS ( -- )
   ENGINE-PRIMS:COUNT AOT-REC-N @ + {: n:n :}
   n DICT-CAP > if
      s" x64link: " type n . s" records against DICT-CAP " type DICT-CAP . cr
      s" x64link: the records do not fit the dictionary" REFUSE
   then
   CODE-SIZE {: size:n :}
   size X64KERNEL:CODE-CEILING DICT-SIZE - > if
      s" x64link: " type size . s" bytes of names and routines past the code ceiling" type cr
      s" x64link: the routines do not fit the code band" REFUSE
   then ;

public

\ ---- where things land --------------------------------------------------------------
: T0 ( -- n ) FIRST-DYNAMIC-WID ;
: WIDN ( -- n ) T0 AOT-WID-SPAN @ + ;
: PRIMS ( -- n ) PRIM-N @ ;
: RECORDS ( -- n ) REC-TOTAL @ ;
: TEXT-VA ( -- n ) VMBASE CODE-OFF + ;           \ where elf.f ASM-CODE links the stream
: REGION-VA ( -- n ) ELF-REGION-VA ;
: REC-VA ( n -- n ) DREC * REGION-VA + ;
: CODE-VA ( -- n ) REGION-VA DICT-SIZE + ;
: DICT$ ( -- ptr u8 n ) 0 DICT-STORAGE BYTE-VIEW RECORDS DREC * ;
: CODE$ ( -- ptr u8 n ) 0 CODE-STORAGE BYTE-VIEW CODE-END @ ;
: BITS$ ( -- ptr u8 n ) BITS PROT-BITS-BYTES ;
: INDEX$ ( -- ptr u8 n ) 0 INDEX-STORAGE BYTE-VIEW HIDX-BYTES ;
: INDEX-OFF ( -- n ) DATA-START ;
: INDEX-VA ( -- n ) X64LAYOUT:DATA-VA VA>N INDEX-OFF + ;
: HEAP-FLOOR ( -- n ) INDEX-OFF HIDX-BYTES + ;
: CLAIMS ( -- n ) RECORDS ;

\ The code band offset where shadow code byte n landed: the last record row
\ whose emission starts at or below it, since the rows' starts ascend.
: PLACED ( n -- n ) {: n:n :}
   0 LO !  AOT-SHADOW:REC-N @ HI !
   begin HI @ LO @ - 1 > while
      LO @ HI @ + 2 / {: mid:n :}
      mid SH-AT n <= if mid LO ! else mid HI ! then
   repeat
   LO @ PLACE-STORAGE @  n LO @ SH-AT -  + ;

\ ---- reading a laid-out record ----------------------------------------------------
: REC ( n -- ptr u8 ) {: k:n :} 0 DICT-STORAGE BYTE-VIEW k DREC * + ;
: REC@ ( n n -- n ) {: k:n f:n :} k REC f + LE:U64@ ;

: REC-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k 16 REC@ {: flags:n :}
   flags DNAME-LEN-MASK and {: len:n :}
   flags DNAME-EXT and 0= if k REC X64KERNEL:REC-NAME + len exit then
   0 CODE-STORAGE BYTE-VIEW  k X64KERNEL:REC-NAME REC@ CODE-VA -  +  len ;

private

\ ---- the name index --------------------------------------------------------------
: FOLD ( n -- n ) {: c:n :}
   c $41 >=  c $5A <= and if c $20 or exit then
   c ;

: HASH ( ptr u8 n -- n ) {: p:ptr len:n :}
   FNV-BASIS
   len 0 ?do p i + c@ FOLD xor FNV-PRIME * loop ;

: SLOT ( ptr u8 n n -- n ) {: p:ptr len:n w:n :}
   p len HASH w xor HIDX-SLOTS 1- and ;

: SLOT-AT ( n -- ptr u8 ) {: s:n :} 0 INDEX-STORAGE BYTE-VIEW s 4 * + ;

: INSERT ( n -- ) {: k:n :}
   k REC-NAME$ k X64KERNEL:REC-WID REC@ SLOT
   begin dup SLOT-AT LE:U32@ 0<> while 1+ HIDX-SLOTS 1- and repeat
   k 1+ swap SLOT-AT LE:U32! ;

: INDEX ( -- )
   HIDX-BYTES CELL / {: cells:n :}
   cells INDEX-STORAGE-RESERVE
   cells 0 ?do 0 i INDEX-STORAGE ! loop
   RECORDS 0 ?do i INSERT loop ;

\ ---- laying the code band ------------------------------------------------------------
: FILL ( ptr u8 n n n -- ) {: band:ptr lo:n hi:n byte:n :}
   hi lo ?do byte band i + c! loop ;

\ Emission r from code band offset `at`, int3 to the next slot, which it answers.
: PLACE ( n n -- n ) {: at:n r:n :}
   0 CODE-STORAGE BYTE-VIEW {: band:ptr :}
   at r PLACE-STORAGE !
   AOT-SHADOW:CODE-BUF@ r SH-AT +  band at +  r SH-LEN BYTE-COPY
   at r SH-LEN + SLOT-UP {: next:n :}
   band  at r SH-LEN +  next  INT3 FILL
   next ;

\ The names' span zero-filled to a slot, then each emission once.
: ROUTINES ( -- )
   NAMES-LEN @ SLOT-UP {: start:n :}
   0 CODE-STORAGE BYTE-VIEW  NAMES-LEN @  start  0 FILL
   start
   -1 PREV !
   AOT-SHADOW:REC-N @ 0 ?do
      i SH-AT PREV @ = if
         i 1- PLACE-STORAGE @ i PLACE-STORAGE !
      else
         i SH-AT PREV !  i PLACE
      then
   loop
   CODE-END ! ;

\ ---- laying the records ------------------------------------------------------------
\ A long name's bytes at the next name offset, and its address in the record.
: LONG-NAME ( ptr u8 n ptr u8 -- ) {: a:ptr len:n rec:ptr :}
   a  0 CODE-STORAGE BYTE-VIEW NAME-AT @ +  len BYTE-COPY
   CODE-VA NAME-AT @ + rec X64KERNEL:REC-NAME + LE:U64!
   NAME-AT @ len + NAME-AT ! ;

: NAME! ( ptr u8 n bool ptr u8 -- ) {: a:ptr len:n ext:bool rec:ptr :}
   0 rec X64KERNEL:REC-NAME + LE:U64!  0 rec X64KERNEL:REC-NAME + 8 + LE:U64!
   ext if a len rec LONG-NAME exit then
   a rec X64KERNEL:REC-NAME + len BYTE-COPY ;

: PRIM! ( n -- ) {: p:n :}
   p REC {: rec:ptr :}
   p ENGINE-PRIMS:FIRST-LABEL X64CODE:LABEL-AT {: first:n :}
   p ENGINE-PRIMS:LAST-LABEL X64CODE:LABEL-AT {: last:n :}
   TEXT-VA first + rec LE:U64!
   last first - CODE-SPAN:EXACT rec 8 + LE:U64!
   p ENGINE-PRIMS:DNAME
   p ENGINE-PRIMS:NAME-LEN DNAME-INL > if DNAME-EXT or then  rec 16 + LE:U64!
   p ENGINE-PRIMS:NAME$ dup DNAME-INL > rec NAME!
   p ENGINE-PRIMS:HELPER-WID rec X64KERNEL:REC-WID + LE:U64! ;

\ The flags cell from the row's flags, min-in and kind bytes and its name length.
: FLAGS ( n -- n ) {: k:n :}
   k 12 CREC@ {: w:n :}
   w $FF and 60 lshift
   w 8 rshift $FF and 52 lshift or
   w 16 rshift $FF and 50 lshift or
   k POOL c@ or ;

\ A code record enters its routine; a does> companion at the clause.
: CODE-REC! ( n n ptr u8 -- ) {: k:n r:n rec:ptr :}
   CODE-VA r PLACE-STORAGE @ + r SH-ENTRY + rec LE:U64!
   r SH-LEN r SH-ENTRY - CODE-SPAN:EXACT rec 8 + LE:U64!
   k 16 CREC@ IMAGE-WID rec X64KERNEL:REC-WID + LE:U64! ;

\ A package row holds its public and private wids where code would be.
: PKG-REC! ( n ptr u8 -- ) {: k:n rec:ptr :}
   k 0 CREC@ IMAGE-WID rec LE:U64!
   k 4 CREC@ IMAGE-WID rec 8 + LE:U64!
   DICT-WL:NAMESPACE rec X64KERNEL:REC-WID + LE:U64! ;

: WINDOW-RECS ( -- )
   0 CUR !
   AOT-REC-N @ 0 ?do
      PRIMS i + REC {: rec:ptr :}
      i FLAGS rec 16 + LE:U64!
      i CREC-NAME$ i EXT? rec NAME!
      i PKG? if i rec PKG-REC! else i CUR @ rec CODE-REC!  1 CUR +! then
   loop ;

: PROTECT ( -- )
   BITS 0 PROT-BITS-BYTES 0 FILL
   AOT-PWIN-N @ 0 ?do
      i PWIN@ T0 + {: w:n :}
      BITS w 3 rshift + {: p:ptr :}
      p c@  1 w 7 and lshift or  p c!
   loop ;

: RESERVE ( -- )
   RECORDS DREC * CELL / DICT-STORAGE-RESERVE
   CODE-SIZE CELL / 1+ CODE-STORAGE-RESERVE
   AOT-SHADOW:REC-N @ 1 max PLACE-STORAGE-RESERVE ;

public

\ Lay the kernel's records and the capture's out: after the kernel's rows are
\ emitted into the X64CODE stream and before elf.f ASM-CODE links it, and after
\ a capture or an import has filled the capture's tables.
: LAYOUT ( -- )
   ?ROUTINES
   ?WIDS
   ?FITS
   ENGINE-PRIMS:COUNT PRIM-N !
   PRIMS AOT-REC-N @ + REC-TOTAL !
   NAMES-SIZE NAMES-LEN !
   0 NAME-AT !
   RESERVE
   ROUTINES
   PRIMS 0 ?do i PRIM! loop
   WINDOW-RECS
   PROTECT
   INDEX ;

\ The image record of a name in one wordlist, or -1, folded as every name
\ compare is: the probe kernel-x64.f FIND-HELPER makes, against INDEX$.
: FIND ( ptr u8 n n -- n ) {: a:ptr len:n w:n :}
   a len w SLOT
   HIDX-SLOTS 0 ?do
      dup SLOT-AT LE:U32@ {: v:n :}
      v 0= if drop -1 unloop exit then
      v 1- {: k:n :}
      k X64KERNEL:REC-WID REC@ w = if
         k REC-NAME$ a len CORE-STR=CI if drop k unloop exit then
      then
      1+ HIDX-SLOTS 1- and
   loop
   drop -1 ;

;using
;package
