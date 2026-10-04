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
\   The target compiler emits fixed and definer bodies into the shadow too.
\   A shipped code record with no routine is refused by name.
\ - THE SITES. Each site of the shadow is linked in CODE$ after its routine is
\   copied: a CALL or TAIL rel32 to its target's entry, a CODE MOVABS of that
\   entry, a DATA MOVABS of the window's DATA where DATA-AT lands it, a FUN
\   MOVABS of its function inside its own routine (a does> definer's clause is
\   its companion's entry). A target is a shipped record's routine, or a kernel
\   body found by the name the site carries among the kernel's global rows, the
\   first row of that name, which is the one the image's index finds.
\ - THE CODE CELLS. CELL-XT answers the image xt a declared code cell holds: the
\   entry of the record its xt row names, or of the kernel body it names.
\ - THE WIDS. A captured wid is a window coordinate (WID-REL-BASE): 0 stays the
\   global wordlist and any other one becomes T0 + (wid - WID-REL-BASE), the
\   image's numbering from FIRST-DYNAMIC-WID, as AOT-WINDOW:REBASE-WID, rebases
\   against the booting engine's WIDN. The image's WIDN is T0 plus the window's
\   span, which counts the wordlists no record names.
\ - THE BITMAP. Each protected-wid row, a zero-based window offset, sets the bit
\   of T0 + row in BITS$, the PROT-BITS band, as AOT-WINDOW:SEAL-WIDS, does.
\ - THE NAME INDEX. The writer builds it, into INDEX$: HIDX-SLOTS u32 slots
\   keyed and filled as kernel-x64.f REBUILD-HELPER fills them, from the live
\   records [0, RECORDS), with CLAIMS one claim per record. The kernel's own
\   probe reads it: test/x86-64-link-records.f stages it in a booted image,
\   where xref-search-wl finds every record from it. It is a
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
\ records than DICT-CAP, routines that pass the code ceiling, a site naming a
\ record with no routine, a site or code cell naming a word no kernel body
\ carries, a call or branch whose displacement does not fit its rel32, a code
\ cell no xt row keys (a quotation's entry, which the capture refuses), and a
\ code cell whose xt row names a record with no routine. A refusal dies, and
\ LAYOUT writes only this file's buffers, so no byte leaves the process first.
\
\ LOAD ORDER. The x86-64 files bind `using X64CODE`, whose public tails
\ src/arch/arm64/icode.f also defines as globals (CODE, LBL, ASM-LEN), and
\ src/habu/aot-decl.f reads that file's AOT-SECTION-CAP. So this file requires
\ the x86-64 side first, then the ARM64 code layer and the capture's
\ declarations; it reads X64CODE qualified. A loader that loads
\ src/arch/arm64/icode.f ahead of this file loads the x86-64 side before that.
require lib/le.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/elf.f
require src/os/linux-x86-64/target-layout.f
require src/habu/layout.f
require src/habu/code-span.f
require src/habu/primitive-registry.f
require src/habu/kernel-x64.f
require src/habu/aot-decl.f
require src/habu/address-cells.f

package X64LINK
using AOT-BUF
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

\ The rc of a capture this layout cannot place: the capture's own refusals'.
74 constant REFUSE-RC
$CC constant INT3
\ A call or branch is E8 or E9 cd: its displacement counts from the end, four
\ bytes past where the field starts, and a field holds a signed 32-bit number.
X64ASM:CALL-REL32-OFF 4 + constant REL32-END
$7FFFFFFF constant REL32-MAX
REL32-MAX negate 1- constant REL32-MIN
$FFFFFFFF constant PKG-MARK            \ a compact row's wid word on a package row
\ The twin of kernel-x64.f HASH, and of habu1.f C-HIDX-HASH: FNV-1a over the
\ name folded to lower case.
$CBF29CE484222325 constant FNV-BASIS
$100000001B3 constant FNV-PRIME


DYNAMIC-BUFFER DICT-STORAGE n
DYNAMIC-BUFFER CODE-STORAGE n
DYNAMIC-BUFFER REGION-STORAGE n
DYNAMIC-BUFFER INDEX-STORAGE n
DYNAMIC-BUFFER PLACE-STORAGE n         \ shadow record row -> its code band offset
DYNAMIC-BUFFER REC-ROUTINE n           \ shipped record row -> shadow routine row
DYNAMIC-BUFFER DATA-STORAGE n
variable DATA-END
DYNAMIC-BUFFER REG-STORAGE n
PROT-BITS-BYTES BUFFER: BITS
variable PRIM-N
variable REC-TOTAL
variable NAMES-LEN                     \ long-name bytes ahead of the routines
variable NAME-AT                       \ the next long name's code band offset
variable CODE-END                      \ code band bytes, a CODE-SLOT multiple
variable CUR                           \ the shadow row a walk of the records is at
variable PREV                          \ the emission a walk of the rows copied last
variable LO
variable HI
variable VAL-AT
variable REG-N
variable REG-ROOT
variable REG-ROWS
variable REG-CHILD
variable REG-HI
variable REG-OUT
variable CELL-CAP

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
   s" x64link: the shadow routine of shipped row " type r SH-REC .INT
   r SH-REC AOT-REC-N @ < if
      s"  lands on package row " type r SH-REC CREC-NAME$ type
   else
      s"  names none of the " type AOT-REC-N @ .INT s"  shipped records" type
   then cr
   s" x64link: the shadow's routines do not match the shipped records" REFUSE ;

: WID-OUT ( n n -- ) {: k:n w:n :}
   k WINDOW. s"  carries wid " type w .INT
   s"  outside the window's " type AOT-WID-SPAN @ .INT s"  wordlists" type cr
   s" x64link: a wid outside the capture window" REFUSE ;

: PROT-OUT ( n -- ) {: row:n :}
   s" x64link: protected-wid row " type row .INT
   s"  is window wordlist " type row PWIN@ .INT s"  of " type AOT-WID-SPAN @ .INT cr
   s" x64link: a protected wid outside the window or at PROT-WID-MAX" REFUSE ;

\ The shadow row whose emission holds shadow code byte n: the first row of the
\ last emission that starts at or below it, since the rows' starts ascend. A
\ `does>` companion follows its definer at the definer's start, so the row is
\ the definer's.
: ROW-AT ( n -- n ) {: n:n :}
   0 LO !  AOT-SHADOW:REC-N @ HI !
   begin HI @ LO @ - 1 > while
      LO @ HI @ + 2 / {: mid:n :}
      mid SH-AT n <= if mid LO ! else mid HI ! then
   repeat
   begin LO @ 0 > if LO @ 1- SH-AT LO @ SH-AT = else false then while -1 LO +! repeat
   LO @ ;

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
\ A named record owns one routine. Anonymous routines can interleave in the
\ shadow's source order, so retain the named row lookup separately from code
\ placement, which follows the shadow's original emission order.
: ?ROUTINES ( -- )
   AOT-REC-N @ 1 max REC-ROUTINE-RESERVE
   AOT-REC-N @ 0 ?do -1 i REC-ROUTINE ! loop
   AOT-SHADOW:REC-N @ 0 ?do
      i SH-REC {: k:n :}
      k AOT-SHADOW:ANON-REC and 0= if
         k AOT-REC-N @ >= if i STRAY-ROUTINE then
         k PKG? if i STRAY-ROUTINE then
         k REC-ROUTINE @ 0 >= if
            s" x64link: two routines for one record" REFUSE then
         i k REC-ROUTINE !
      then
   loop
   AOT-REC-N @ 0 ?do
      i PKG? 0= if i REC-ROUTINE @ 0 < if i NO-ROUTINE then then
   loop ;

: ROUTINE-OF ( n -- n ) REC-ROUTINE @ ;

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

: ?FITS ( -- )
   ENGINE-PRIMS:COUNT AOT-REC-N @ + {: n:n :}
   n DICT-CAP > if
      s" x64link: " type n .INT s"  records against DICT-CAP " type DICT-CAP .INT cr
      s" x64link: the records do not fit the dictionary" REFUSE
   then
   CODE-END @ {: size:n :}
   size X64KERNEL:CODE-CEILING DICT-SIZE - > if
      s" x64link: " type size .INT s"  bytes of names and routines past the code ceiling" type cr
      s" x64link: the routines do not fit the code band" REFUSE
   then ;

public

\ ---- where things land --------------------------------------------------------------
: T0 ( -- n ) FIRST-DYNAMIC-WID ;
: WIDN ( -- n ) T0 AOT-WID-SPAN @ + ;
: PRIMS ( -- n ) PRIM-N @ ;
: RECORDS ( -- n ) REC-TOTAL @ ;
: TEXT-VA ( -- n ) VMBASE X64LAYOUT:CODE-OFF + ;  \ where elf.f ASM-CODE links the stream
: REGION-VA ( -- n ) ELF-REGION-VA ;
: REC-VA ( n -- n ) DREC * REGION-VA + ;
: CODE-VA ( -- n ) REGION-VA DICT-SIZE + ;
: CP-VA ( -- n ) CODE-VA CODE-END @ + ;
: DICT$ ( -- ptr u8 n ) 0 DICT-STORAGE BYTE-VIEW RECORDS DREC * ;
: CODE$ ( -- ptr u8 n ) 0 CODE-STORAGE BYTE-VIEW CODE-END @ ;
: BITS$ ( -- ptr u8 n ) BITS PROT-BITS-BYTES ;
: INDEX$ ( -- ptr u8 n ) 0 INDEX-STORAGE BYTE-VIEW HIDX-BYTES ;
: INDEX-OFF ( -- n ) DATA-START ;
: INDEX-VA ( -- n ) X64LAYOUT:DATA-VA VA>N INDEX-OFF + ;
: INDEX-END ( -- n ) INDEX-OFF HIDX-BYTES + ;
: CLAIMS ( -- n ) RECORDS ;

\ The code band offset where shadow code byte n landed.
: PLACED ( n -- n ) {: n:n :}
   n ROW-AT {: r:n :}
   r PLACE-STORAGE @  n r SH-AT -  + ;

\ Where the window's DATA lands, as a DATA offset: the heap floor moved up to the
\ capture base's own 8-residue, as habu2.f EM-AOT-RELOC-DATA moves the seed's DP,
\ so every captured cell keeps its alignment. The window spans AOT-DATA-SIZE
\ bytes from it.
: CELL-BASE ( -- n )
   CELL-CAP @ ADDRESS-CELLS:BOOT-CAP > if INDEX-END
      else ADDRESS-CELLS:BOOT-OFF then ;
: CELL-END ( -- n )
   CELL-CAP @ ADDRESS-CELLS:BOOT-CAP > if INDEX-END CELL-CAP @ cells +
      else INDEX-END then ;
: DATA-AT ( -- n ) CELL-END dup AOT-DATA-D0 @ rot - 7 and + ;
: HEAP-FLOOR ( -- n ) DATA-AT AOT-DATA-SIZE @ + ;
: CELL-AT ( n -- n ) {: c:n :}
   AOT-WINDOW:XTOFF-BUF@ c AOT-WINDOW:XTOFF-ROW * + LE:U32@ {: loc:n :}
   loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if loc exit then
   DATA-AT loc AOT-WINDOW:XTOFF-LOC-MASK and + ;
: DATA$ ( -- ptr u8 n ) 0 DATA-STORAGE BYTE-VIEW DATA-END @ ;

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

public

: REGION$ ( -- ptr u8 n )
   DICT-SIZE CODE-END @ + {: len:n :}
   len CELL / 1+ REGION-STORAGE-RESERVE
   0 REGION-STORAGE BYTE-VIEW {: out:ptr :}
   out 0 len 0 FILL
   DICT$ {: recs:ptr rec-len:n :}
   recs out rec-len BYTE-COPY
   CODE$ {: code:ptr code-len:n :}
   code out DICT-SIZE + code-len BYTE-COPY
   out len ;

private

\ An emission may include a defer trailer after its routine. The next distinct
\ record start, or the shadow's end, bounds the bytes copied into its slot.
: EXTENT ( n -- n ) {: r:n :}
   AOT-SHADOW:CODE-LEN @
   AOT-SHADOW:REC-N @ r 1+ ?do
      i SH-AT r SH-AT <> if drop i SH-AT leave then
   loop
   r SH-AT - ;

\ Each row's code band offset, laying nothing: the names' span to a slot, then
\ each emission once from a slot, a row over the emission before it at its offset.
: PLACE-ALL ( -- )
   AOT-SHADOW:REC-N @ 1 max PLACE-STORAGE-RESERVE
   NAMES-LEN @ SLOT-UP
   -1 PREV !
   AOT-SHADOW:REC-N @ 0 ?do
      i SH-AT PREV @ = if
         i 1- PLACE-STORAGE @ i PLACE-STORAGE !
      else
         i SH-AT PREV !
         dup i PLACE-STORAGE !
         i EXTENT + SLOT-UP
      then
   loop
   CODE-END ! ;

\ Emission r at its offset, int3 to the next slot.
: COPY-ROUTINE ( n -- ) {: r:n :}
   0 CODE-STORAGE BYTE-VIEW {: band:ptr :}
   r PLACE-STORAGE @ {: at:n :}
   AOT-SHADOW:CODE-BUF@ r SH-AT +  band at +  r EXTENT BYTE-COPY
   band  at r EXTENT +  dup SLOT-UP  INT3 FILL ;

\ The names' span zero-filled to a slot, then each emission once.
: ROUTINES ( -- )
   0 CODE-STORAGE BYTE-VIEW  NAMES-LEN @  NAMES-LEN @ SLOT-UP  0 FILL
   -1 PREV !
   AOT-SHADOW:REC-N @ 0 ?do
      i SH-AT PREV @ <> if i SH-AT PREV !  i COPY-ROUTINE then
   loop ;

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
   AOT-REC-N @ 0 ?do
      PRIMS i + REC {: rec:ptr :}
      i FLAGS rec 16 + LE:U64!
      i CREC-NAME$ i EXT? rec NAME!
      i PKG? if i rec PKG-REC! else i i ROUTINE-OF rec CODE-REC! then
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
   CODE-END @ CELL / 1+ CODE-STORAGE-RESERVE ;

\ ---- the sites -------------------------------------------------------------------
\ A site row is (shadow code byte, kind, target), src/habu/aot-decl.f AOT-SHADOW.
: SITE@ ( n n -- n ) {: s:n f:n :}
   AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + f + LE:U32@ ;
: SITE-AT ( n -- n ) 0 SITE@ ;
: SITE-KIND ( n -- n ) 4 SITE@ ;
: REL32? ( n -- bool ) {: kind:n :} kind AOT-SHADOW:CALL =  kind AOT-SHADOW:TAIL = or ;

: POOL-NAME$ ( n -- ptr u8 n ) {: off:n :} AOT-NAMES-BUF@ off + {: e:ptr :} e 1+ e c@ ;
: SITE-NAME$ ( n -- ptr u8 n ) 8 SITE@ SITE-TARGET-MASK and POOL-NAME$ ;

\ The kernel body a name of the engine's own prefix names: the first global row
\ of that name, folded, the record the image's index finds; or -1.
: KERNEL-BODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   ENGINE-PRIMS:COUNT 0 ?do
      i ENGINE-PRIMS:HELPER-WID 0= if
         i ENGINE-PRIMS:NAME$ a u CORE-STR=CI if i unloop exit then
      then
   loop
   -1 ;

: BODY-VA ( n -- n ) ENGINE-PRIMS:FIRST-LABEL X64CODE:LABEL-AT TEXT-VA + ;
: SHADOW-VA ( n -- n ) {: r:n :}
   CODE-VA r PLACE-STORAGE @ + r SH-ENTRY + ;

: SITE. ( n -- ) {: s:n :}
   s SITE-AT ROW-AT {: r:n :}
   s" x64link: the shadow routine of " type
   r SH-REC AOT-SHADOW:ANON-REC and 0<> if
      s" anonymous row " type r .INT
   else
      r SH-REC CREC-NAME$ type
   then
   s"  at code byte " type s SITE-AT .INT ;

: UNCARRIED ( n -- ) {: s:n :}
   s SITE. s"  names " type s SITE-NAME$ type
   s" , which no x86-64 kernel body carries" type cr
   s" x64link: a shadow site names a word the x86-64 kernel does not carry" REFUSE ;

: ROUTINELESS ( n -- ) {: s:n :}
   s 8 SITE@ SITE-TARGET-MASK and {: k:n :}
   s SITE. s"  names shipped row " type k .INT space k CREC-NAME$ type
   s" , which has no x86-64 routine" type cr
   s" x64link: a shadow site names a record with no x86-64 routine" REFUSE ;

: OUT-OF-REACH ( n n -- ) {: s:n d:n :}
   s SITE. s"  lies " type d .INT s"  bytes from its target, past a rel32" type cr
   s" x64link: a shadow call or branch does not reach its target" REFUSE ;

\ The image address a site's target enters: a shipped record's routine at its
\ entry, or a kernel body by name.
: TARGET-VA ( n -- n ) {: s:n :}
   s 8 SITE@ {: t:n :}
   t SITE-SHADOW-TAG and SITE-SHADOW-TAG = if
      t SITE-TARGET-MASK and SHADOW-VA exit
   then
   t SITE-NAME-TAG and 0<> if
      s SITE-NAME$ KERNEL-BODY {: p:n :}
      p 0 < if s UNCARRIED then
      p BODY-VA exit
   then
   t SITE-TARGET-MASK and ROUTINE-OF {: r:n :}
   r 0 < if s ROUTINELESS then
   r SHADOW-VA ;

\ What the capture left in a MOVABS site's immediate.
: FIELD-OFF ( n -- n ) {: kind:n :}
   kind REL32? if X64ASM:CALL-REL32-OFF exit then
   kind AOT-SHADOW:DCELL = if 0 exit then
   X64ASM:MOV-RI64-IMM-OFF ;

: CAPTURED ( n -- n ) {: s:n :}
   AOT-SHADOW:CODE-BUF@ s SITE-AT + s SITE-KIND FIELD-OFF + LE:U64@ ;

\ The field a site's instruction carries in the image: a call's or branch's
\ displacement from its end, or a MOVABS's address. The reader admits five kinds
\ (src/habu/aot-file.f ?SH-SITES), so the one left after four is FUN, whose
\ immediate the capture left as its function's offset in its own emission.
: SITE-VALUE ( n -- n ) {: s:n :}
   s SITE-KIND {: kind:n :}
   kind REL32? if s TARGET-VA  CODE-VA s SITE-AT PLACED + REL32-END +  - exit then
   kind AOT-SHADOW:CODE = if s TARGET-VA exit then
   kind AOT-SHADOW:DATA =  kind AOT-SHADOW:DCELL = or if
      X64LAYOUT:DATA-VA VA>N DATA-AT +  s CAPTURED AOT-DATA-D0 @ -  + exit
   then
   CODE-VA s SITE-AT ROW-AT PLACE-STORAGE @ +  s CAPTURED + ;

: ?SITES ( -- )
   AOT-SHADOW:SITE-N @ 0 ?do
      i SITE-VALUE {: v:n :}
      i SITE-KIND REL32? if
         v REL32-MIN <  v REL32-MAX > or if i v OUT-OF-REACH then
      then
   loop ;

: LINK-SITES ( -- )
   0 CODE-STORAGE BYTE-VIEW {: band:ptr :}
   AOT-SHADOW:SITE-N @ 0 ?do
      i SITE-VALUE {: v:n :}
      band i SITE-AT PLACED +  i SITE-KIND FIELD-OFF + {: at:ptr :}
      i SITE-KIND REL32? if v at LE:U32! else v at LE:U64! then
   loop ;

\ The same small sort publishes both persistent registries. A key is the
\ placed byte offset shifted left by eight for SITES, or by one for cells;
\ its low bits hold the kind. Capture rows need not arrive in placement order.
: REG-KEY ( n -- n ) REG-STORAGE @ ;
: REG-SWAP ( n n -- ) {: a:n b:n :}
   a REG-KEY {: value:n :}
   b REG-KEY a REG-STORAGE !  value b REG-STORAGE ! ;
: REG-SIFT ( n n -- ) {: root:n rows:n :}
   root REG-ROOT !  rows REG-ROWS !
   begin REG-ROOT @ 2 * 1+ REG-ROWS @ < while
      REG-ROOT @ 2 * 1+ REG-CHILD !
      REG-CHILD @ 1+ REG-ROWS @ < if
         REG-CHILD @ REG-KEY REG-CHILD @ 1+ REG-KEY < if
            REG-CHILD @ 1+ REG-CHILD ! then then
      REG-ROOT @ REG-KEY REG-CHILD @ REG-KEY < 0= if exit then
      REG-ROOT @ REG-CHILD @ REG-SWAP
      REG-CHILD @ REG-ROOT !
   repeat ;
: REG-SORT ( -- )
   REG-N @ 2 / 1- REG-HI !
   begin REG-HI @ 0 >= while
      REG-HI @ REG-N @ REG-SIFT  REG-HI @ 1- REG-HI ! repeat
   REG-N @ 1- REG-HI !
   begin REG-HI @ 0 > while
      0 REG-HI @ REG-SWAP
      0 REG-HI @ REG-SIFT
      REG-HI @ 1- REG-HI ! repeat ;
: REG-ADD ( n -- ) {: value:n :}
   REG-N @ 1+ REG-STORAGE-RESERVE
   value REG-N @ REG-STORAGE !
   1 REG-N +! ;
: REG-CLEAR ( -- ) 0 REG-N ! ;
: DATA-CELL! ( n n -- ) {: v:n off:n :}
   v 0 DATA-STORAGE BYTE-VIEW off + LE:U64! ;

: SITE-REGISTRY ( -- )
   REG-CLEAR
   AOT-SHADOW:SITE-N @ 0 ?do
      i SITE-KIND AOT-SHADOW:DCELL <> if
         i SITE-KIND REL32? if SNAP-RELOC:SITE-CALL
            else SNAP-RELOC:SITE-ADDR then {: kind:n :}
         DICT-SIZE i SITE-AT PLACED + {: off:n :}
         off REGION >= if s" x64link: placed site outside REGION" REFUSE then
         off 8 lshift kind or REG-ADD
      then
   loop
   REG-N @ SNAP-RELOC:SITE-CAP > if s" x64link: too many placed sites" REFUSE then
   REG-SORT
   REG-N @ SNAP-RELOC:SITE-N-CELL DATA-CELL!
   REG-N @ 0 ?do
      i REG-KEY {: key:n :}
      i 0 > if
         i 1- REG-KEY 8 rshift key 8 rshift = if
            s" x64link: duplicate placed site" REFUSE then then
      SNAP-RELOC:SITE-ROWS-OFF i SNAP-RELOC:SITE-ROW-BYTES * + {: at:n :}
      key 8 rshift 0 DATA-STORAGE BYTE-VIEW at + LE:U32!
      key $FF and 0 DATA-STORAGE BYTE-VIEW at SNAP-RELOC:SITE-KIND-OFF + + c!
   loop ;

\ ---- the code cells --------------------------------------------------------------
\ An address-cell row's target word (src/habu/aot-decl.f AOT-WINDOW): its two high
\ bits are CODE (00), named CODE (01) or DATA (10); its low bits are 0 for null.
: CELL-META ( n -- n ) {: c:n :}
   AOT-WINDOW:XTOFF-BUF@ c AOT-WINDOW:XTOFF-ROW * + 4 + LE:U32@ ;
: CODE-CELL? ( n -- bool ) CELL-META {: m:n :}
   m AOT-WINDOW:XTOFF-KIND-MASK and 0=  m AOT-WINDOW:XTOFF-VALUE-MASK and 0<> and ;
: NAMED-CELL? ( n -- bool )
   CELL-META AOT-WINDOW:XTOFF-KIND-MASK and AOT-WINDOW:XTOFF-NAME-TAG = ;
: CELL-NAME$ ( n -- ptr u8 n ) CELL-META AOT-WINDOW:XTOFF-VALUE-MASK and 1- POOL-NAME$ ;
: XT@ ( n n -- n ) {: x:n f:n :}
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + f + LE:U32@ ;
: XT-TARGET-VA ( n -- n ) {: target:n :}
   target SITE-SHADOW-TAG and SITE-SHADOW-TAG = if
      target SITE-TARGET-MASK and SHADOW-VA exit
   then
   PRIMS target + 0 REC@ ;

: NO-XT ( n -- ) {: c:n :}
   s" x64link: address-cell row " type c .INT
   s"  holds window code no shipped record enters" type cr
   s" x64link: a code cell targets code no shipped record enters" REFUSE ;

: CELL-UNCARRIED ( n -- ) {: c:n :}
   s" x64link: address-cell row " type c .INT s"  holds " type c CELL-NAME$ type
   s" , which no x86-64 kernel body carries" type cr
   s" x64link: a code cell names a word the x86-64 kernel does not carry" REFUSE ;

: CELL-ROUTINELESS ( n n -- ) {: c:n k:n :}
   s" x64link: address-cell row " type c .INT s"  holds shipped row " type k .INT space k CREC-NAME$ type
   s" , which has no x86-64 routine" type cr
   s" x64link: a code cell names a record with no x86-64 routine" REFUSE ;

\ The xt rows ascend by address-cell row, one per code cell (src/habu/aot-file.f
\ ?SH-XTS), so one walk beside the cells pairs each with its row.
: ?CELLS ( -- )
   0 CUR !
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i CODE-CELL? if
         CUR @ AOT-SHADOW:XT-N @ < if CUR @ 0 XT@ i = else false then
         0= if i NO-XT then
         CUR @ 4 XT@ {: target:n :}
         target SITE-SHADOW-TAG and SITE-SHADOW-TAG <> if
            target ROUTINE-OF 0 < if i target CELL-ROUTINELESS then
         then
         1 CUR +!
      then
      i NAMED-CELL? if i CELL-NAME$ KERNEL-BODY 0 < if i CELL-UNCARRIED then then
   loop ;

\ The captured DATA follows the index and must stay below the kernel's heap
\ ceiling. This is the write-time twin of the ARM64 boot relocation bound.
: ?DATA ( -- )
   HEAP-FLOOR X64KERNEL:DP-CEILING > if
      s" x64link: the window DATA does not fit under the DP ceiling" REFUSE
   then
   HEAP-FLOOR DATA-END !
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i CELL-AT {: off:n :}
      off 0 < off X64LAYOUT:DATA-SIZE CELL - > or if
         s" x64link: address cell outside DATA" REFUSE then
      off CELL + DATA-END @ max DATA-END !
   loop ;

: DATA-BYTES! ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   a 0 DATA-STORAGE BYTE-VIEW off + u BYTE-COPY ;

: WINDOW-CELLS ( -- )
   0 VAL-AT !
   AOT-WINDOW:BM-LEN @ AOT-WINDOW:CELL-BITS * 0 ?do
      AOT-WINDOW:BM-BUF@ i AOT-WINDOW:CELL-BITS / + c@
      i AOT-WINDOW:CELL-BITS mod rshift 1 and 0<> if
         AOT-WINDOW:VAL-BUF@ VAL-AT @ +  AOT-WINDOW:VAL-LEN @ VAL-AT @ -
         AOT-WINDOW:CELL-V@ {: v:n w:n :}
         v DATA-AT i AOT-WINDOW:CELL-BYTES * + DATA-CELL!
         VAL-AT @ w + VAL-AT !
      then
   loop ;

: CELL-DECL ( n bool -- ) {: off:n data?:bool :}
   off 0 < off X64LAYOUT:DATA-SIZE CELL - > or if
      s" x64link: address cell outside DATA" REFUSE then
   off 1 lshift  data? if 1 or then REG-ADD ;

: CELL-ROW! ( n -- ) {: key:n :}
   key 1 rshift  key 1 and 0<> if SNAP-RELOC:XTCELL-DATA-TAG or then
   REG-OUT @ cells CELL-BASE + DATA-CELL!
   1 REG-OUT +! ;

: FIXED-CELLS ( -- )
   HOOK-CELL false CELL-DECL
   COMPILE-PREFLIGHT-CELL false CELL-DECL
   TOP-HOOK-CELL false CELL-DECL
   EXIT-HOOK-CELL false CELL-DECL
   NCOMP-DISPATCH:XT-CELL false CELL-DECL
   NCOMP-DISPATCH:FIXED-SHADOW-CELL false CELL-DECL
   NCOMP-DISPATCH:DOES-SHADOW-CELL false CELL-DECL
   TASK-CHAIN-CELL true CELL-DECL
   CREATEP-CELL false CELL-DECL
   APP-ENTRY:XT-CELL false CELL-DECL
   LASTC-CELL false CELL-DECL
   ENGINE-MAIN:XT-CELL false CELL-DECL
   PROVIDED-XT:EVALUATE-CELL false CELL-DECL
   NCOMP-DISPATCH:DECL-CELL true CELL-DECL
   NCOMP-DISPATCH:TARGET-DECL-CELL true CELL-DECL ;

\ Reserve a DATA backing for the largest possible deduplicated declaration
\ set before relocating the captured window. The ordinary small set stays in
\ the fixed boot rows; a larger set lives directly after the name index.
: CELL-PLAN ( -- )
   REG-CLEAR FIXED-CELLS
   REG-N @ AOT-WINDOW:XTOFF-N @ + CELL-CAP ! ;

: CELL-REGISTRY ( -- )
   REG-CLEAR FIXED-CELLS
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i CELL-AT
      i CELL-META AOT-WINDOW:XTOFF-DATA-TAG and 0<> CELL-DECL
   loop
   REG-SORT
   0 REG-OUT !
   REG-N @ 0 ?do
      i REG-KEY {: key:n :}
      i 0 > if
         i 1- REG-KEY 1 rshift key 1 rshift = if
            i 1- REG-KEY key <> if
               s" x64link: address cell has two kinds" REFUSE then
         else key CELL-ROW! then
      else key CELL-ROW! then
   loop
   REG-OUT @ SNAP-RELOC:XTCELL-N-CELL DATA-CELL!
   ADDRESS-CELLS:MAGIC SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:MAGIC-FIELD + DATA-CELL!
   CELL-BASE SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD + DATA-CELL!
   CELL-CAP @ ADDRESS-CELLS:BOOT-CAP max
      SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:CAP-FIELD + DATA-CELL!
   0 SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:MODE-FIELD + DATA-CELL! ;

: ROW-VALUE ( n -- n ) {: c:n :}
   c CODE-CELL? if CUR @ 4 XT@ XT-TARGET-VA  1 CUR +! exit then
   c NAMED-CELL? if c CELL-NAME$ KERNEL-BODY BODY-VA exit then
   c CELL-META AOT-WINDOW:XTOFF-VALUE-MASK and {: v:n :}
   v 0= if 0 exit then
   X64LAYOUT:DATA-VA VA>N DATA-AT + v + 1- ;

: DATA-BAND ( -- )
   DATA-END @ CELL 1- + CELL / {: ncells:n :}
   ncells 1 max DATA-STORAGE-RESERVE
   ncells 0 ?do 0 i DATA-STORAGE ! loop
   X64LAYOUT:DATA-VA VA>N HEAP-FLOOR + DP-CELL DATA-CELL!
   DATA-START BOOT-LAYOUT:HEAP-START-CELL DATA-CELL!
   WIDN WIDN-CELL DATA-CELL!
   T0 AOT-CELLS:T0-CELL DATA-CELL!
   INDEX-VA HIDXP-CELL DATA-CELL!
   CLAIMS HIDX:CLAIMS DATA-CELL!
   BITS$ PROT-BITS-OFF DATA-BYTES!
   INDEX$ INDEX-OFF DATA-BYTES!
   WINDOW-CELLS
   0 CUR !
   AOT-WINDOW:XTOFF-N @ 0 ?do i ROW-VALUE i CELL-AT DATA-CELL! loop
   CELL-REGISTRY
   SITE-REGISTRY
   DICT-SIZE TIER-PROV:TABLE-OFF DATA-CELL!
   DICT-SIZE CODE-END @ + TIER-PROV:TABLE-OFF CELL + DATA-CELL!
   1 TIER-PROV:TABLE-OFF 2 cells + DATA-CELL!
   1 TIER-PROV:N-CELL DATA-CELL! ;

public

\ Lay the kernel's records and the capture's out: after the kernel's rows are
\ emitted into the X64CODE stream and before elf.f ASM-CODE links it, and after
\ a capture or an import has filled the capture's tables.
: LAYOUT ( -- )
   ?ROUTINES
   ?WIDS
   ENGINE-PRIMS:COUNT PRIM-N !
   PRIMS AOT-REC-N @ + REC-TOTAL !
   NAMES-SIZE NAMES-LEN !
   0 NAME-AT !
   PLACE-ALL
   ?FITS
   CELL-PLAN
   ?DATA
   ?SITES
   ?CELLS
   RESERVE
   ROUTINES
   PRIMS 0 ?do i PRIM! loop
   WINDOW-RECS
   PROTECT
   INDEX
   LINK-SITES
   DATA-BAND ;

\ The image xt address-cell row n's cell holds when it holds code: the entry of
\ the shipped record its xt row names, or of the kernel body it names; -1 for a
\ cell that holds DATA or nothing. LAYOUT refused a code cell neither resolves.
: CELL-XT ( n -- n ) {: c:n :}
   c NAMED-CELL? if c CELL-NAME$ KERNEL-BODY BODY-VA exit then
   c CODE-CELL? 0= if -1 exit then
   AOT-SHADOW:XT-N @ 0 ?do
      i 0 XT@ c = if i 4 XT@ XT-TARGET-VA unloop exit then
   loop
   -1 ;

\ The .names sidecar uses offsets within the linked REGION code band, the
\ same coordinate its reader uses for a code blob. Anonymous captured routines
\ have no dictionary row, but retain their shadow row and can still be named
\ by the build-side sidecar.
: NAMES-SPAN ( n -- n n ) {: r:n :}
   r PLACE-STORAGE @ r SH-ENTRY +
   r SH-LEN r SH-ENTRY - CODE-SPAN:EXACT ;

;using   \ X64LAYOUT
;using
;package
