split-str
\ link-data.f - a build's DATA: the link's copy of the open space's cells, each
\ declared cell given its value at DATA-VA, appended to A64ICODE's code buffer
\ after the routine band. A Mach-O image holds the copy as its non-zero cell
\ runs, then the DATA list, the offset from DATA-VA of every cell holding a
\ code address, which start-up adds the load offset to. An ELF image never
\ moves: it holds a table of the copy's runs of ELF:PAGE-BYTES pages that hold a
\ non-zero cell, whose bytes the writer's carrier load holds, and an empty
\ list. The link also writes the header cells HEADER-CELLS names, and a Mach-O
\ image lists the code ones. An engine's link adds the records: the target
\ kernel's primitives, then the build's, each build record and wordlist id
\ renumbered for the target (RECORD-IMAGE), and the record list. LINKER:WRITE
\ writes the linked program; the other words are LINKER's own.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/le.f
require src/lib/sort.f
require src/lib/fs-mutate.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/address-cells.f
require src/arch/arm64/icode.f
require src/habu/link-kernel.f
require src/habu/fdio.f
require src/habu/sign-id.f
require src/os/macos/macho.f
require src/os/macos/sign2.f
require src/os/linux/elf.f
require src/os/linux/sign.f
require src/habu/driver-io.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-copy.f
require src/arch/wasm/link-module.f
require src/habu/link-map.f
using DRIVER

package LINKER
private

\ The kernel object's labels, which ADOPT-LABELS sets. The tail's, which the
\ start-up reads: the DATA image's start and end, the DATA list's start and end.
variable LIMAGE
variable LIMAGE-END
variable LLIST
variable LLIST-END
\ An engine's: the record image's start and end, the record list's start and end.
variable LRECS
variable LRECS-END
variable LRLIST
variable LRLIST-END
\ The code's end, before the pad that aligns the DATA image.
variable CODE-END
\ An engine's name pool's end, which no label marks: the records start after it
\ at an 8-byte boundary.
variable NAMES-END
\ The kernel's uncaught-throw reporter, signal handler, primitive count and
\ dictionary table; an engine's underdepth throw.
variable LUNCAUGHT
variable LSIGH
variable LNCOUNT
variable LDICT
variable LFLOORREC
\ The kernel's code end, in bytes from the text's start: the target's words follow.
variable KERNEL-END

\ The record list's offsets from the dictionary band, ascending as the records
\ are laid; the code offset of each build record's out-of-line name.
DYNAMIC-BUFFER RLISTED n
variable RLISTED-N
DYNAMIC-BUFFER NAME-AT n
\ Each kernel primitive's facts text, as its offset from the space, 0 for none.
DYNAMIC-BUFFER PRIM-FACTS-AT n


\ A bound label's byte offset in the product's code buffer.
: LABEL-OFFSET ( ^n -- n ) NATIVE:LABEL@ NATIVE:OFFSET ;


\ A host address in the open space at its DATA-VA address: the resolver code's
\ data address sites get.
: DATA-ADDRESS ( n -- n ) DATA-OFFSET  NATIVE:DATA-VA + ;


: PAD8 ( -- ) NATIVE:PAD8 ;



\ The link address xt cell off's value v gives the image.
: XT-VALUE ( n n -- n )
   {: off:n v:n :}
   v 0= if 0 exit then
   v LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   v ENTRY-REC {: r:n :}
   s" E-LINK-XT-CELL" REJECTION:OPEN
   s" the xt cell at offset " REJECTION:EMIT-STR  off REJECTION:EMIT-N
   r 0< if
      s"  holds " REJECTION:EMIT-STR  v REJECTION:EMIT-N
      s" , which is no code the program holds" REJECTION:EMIT-STR
   else
      s"  holds the xt of `" REJECTION:EMIT-STR  r XREF-REC XREF-NAME$ REJECTION:EMIT-STR
      s" `, which is no word the program holds" REJECTION:EMIT-STR
   then
   REJECTION:HELP
   s" store in an xt cell only 0 or the xt of a word the program defines" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


: COPY-ROW ( n -- )
   {: row:n :}
   row IMAGE-OFF {: off:n :}
   off CELL-FITS
   row CELL-VALUE {: v:n :}
   row SCRATCH-ROW? if 0 off COPY-AT LE:U64! exit then
   row DATA-ROW? if
      v 0<> if v DATA-ADDRESS off COPY-AT LE:U64! then exit
   then
   off v XT-VALUE off COPY-AT LE:U64! ;


\ A cell holding 0 holds no code address and is not listed. LIST, sorts the list
\ and SORT-ROWS the copied rows by cell: neither follows the build's mark order.
: LIST-ROW ( n -- )
   {: row:n :}
   row XT-ROW? 0= if exit then
   row CELL-VALUE 0= if exit then
   LISTED-N @ 1 + LISTED-RESERVE
   row IMAGE-OFF LISTED-N @ LISTED !  1 LISTED-N +! ;

: LIST, ( -- )
   LISTED-N @ {: n:n :}
   n 0 > if 0 LISTED n [: < ;] SORT:SORT! then
   n 0 ?do i LISTED @ NATIVE:U64, loop ;


\ A cell the build stored into would be written and slid twice.
: HEADER! ( n n -- )
   {: v:n off:n :}
   off SPACE-AT @ 0<> if
      s" E-LINK-HEADER-CELL" REJECTION:OPEN
      s" the build stored into the header cell at offset " REJECTION:EMIT-STR
      off REJECTION:EMIT-N  s" , which the link writes" REJECTION:EMIT-STR
      REJECTION:HELP
      s" leave the header cells to the link: store into a cell the build allots"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   v off COPY-AT LE:U64! ;


\ Header cells valued as the target's boot leaves them, at the kernel object's
\ offsets, and the cells it presets.
: HEADER-CELLS ( -- )
   NATIVE:DATA-VA {: va:n :}
   s" DATA-START" NATIVE:SYM {: start:n :}
   va DATA-IMAGE-LEN +  s" DP-CELL" NATIVE:SYM HEADER!
   va start +  s" DP-FLOOR-CELL" NATIVE:SYM HEADER!
   va s" DP-CEILING-OFF" NATIVE:SYM +  s" DP-CEILING-CELL" NATIVE:SYM HEADER!
   start s" HEAP-START-CELL" NATIVE:SYM HEADER!
   NATIVE:PRESET-N 0 ?do i NATIVE:PRESET@ HEADER! loop
   va s" FD-CELL" NATIVE:SYM +
      s" FD-PTR-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA s" RBASE-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA LUNCAUGHT LABEL-OFFSET +  s" UNCGH-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA LSIGH LABEL-OFFSET +  s" STUB-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA KERNEL-END @ +  s" CODE-END-CELL" NATIVE:SYM HEADER!
   \ The build's MARK left its own index mapping's address here; the target's
   \ first MARK builds one from 0.
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;


\ An engine's throw recovery: its kernel branches through this cell after a
\ data-stack underdepth. A stripped program's cut drops it.
: RECOVERY-CELLS ( -- )
   NATIVE:TEXT-VA LFLOORREC LABEL-OFFSET +  s" FLOORREC-CELL" NATIVE:SYM HEADER! ;

\ The header cells HEADER-CELLS and RECOVERY-CELLS give a code address.
: LIST-HEADER ( -- )
   s" RBASE-CELL" NATIVE:SYM NATIVE:U64,
   s" UNCGH-CELL" NATIVE:SYM NATIVE:U64,
   s" STUB-CELL" NATIVE:SYM NATIVE:U64,
   s" CODE-END-CELL" NATIVE:SYM NATIVE:U64,
   ENGINE? @ if
      s" FLOORREC-CELL" NATIVE:SYM NATIVE:U64,
   then ;


\ ---- an engine's records ------------------------------------------------------------
\ A record index holds only in the process that made it: the target numbers its
\ kernel's primitives [0, P) and each build record r at P + (r - FIRST-REC).

: TARGET-WIDN ( -- n ) FIRST-DYNAMIC-WID  WIDN-CELL HOST@ +  FIRST-WID - ;


\ The target kernel's primitive count and record k's cell: [0] and [8] the
\ entry's and the last word's code offsets, [24] an out-of-line name's.
: PRIMS ( -- n ) NATIVE:CODE LNCOUNT LABEL-OFFSET + LE:U64@ ;

: PRIM@ ( n n -- n )
   {: k:n slot:n :}
   NATIVE:CODE LDICT LABEL-OFFSET +  k DREC * +  slot cells +  LE:U64@ ;

: PRIM-NAME$ ( n -- str )
   {: k:n :}
   k 2 PRIM@ {: flags:n :}
   flags DNAME-EXT and 0<> if
      NATIVE:CODE k 3 PRIM@ +
   else
      NATIVE:CODE LDICT LABEL-OFFSET +  k DREC * +  24 +
   then
   flags DNAME-LEN-MASK and ;


\ A built-in primitive's facts text, laid in the space; 0 for a package row, an
\ owned primitive or a primitive no text says.
: PRIM-TEXT ( n -- n )
   {: k:n :}
   k 5 PRIM@ WL:PRIMITIVES <> if 0 exit then
   k PRIM-NAME$ PRIMITIVE-FACTS {: a:^ u:n :}
   u 0= if 0 exit then
   a u INTERN:TEXT BASE @ - ;

\ After the kernel and before DATA-IMAGE copies the space.
: PRIM-TEXTS ( -- )
   PRIMS PRIM-FACTS-AT-RESERVE
   PRIMS 0 ?do i PRIM-TEXT i PRIM-FACTS-AT ! loop ;

: RLIST+ ( n -- )
   RLISTED-N @ 1 + RLISTED-RESERVE
   RLISTED-N @ RLISTED !  1 RLISTED-N +! ;

\ A cell holding a code address at band offset off: listed, so start-up slides it.
: CODE-CELL, ( n n -- )
   {: v:n off:n :}
   v NATIVE:U64,  off RLIST+ ;


: PRIM-LEN ( n -- n )
   dup 1 PRIM@ swap 0 PRIM@ -
   NATIVE:X64? if CODE-SPAN:EXACT else 4 - then ;

\ A primitive's record: [8] the body's length, a package row's
\ [0] and [8] its wordlist ids; [32] the facts text PRIM-TEXTS laid.
: PRIM-REC, ( n -- )
   {: k:n :}
   k DREC * {: at:n :}
   k 0 PRIM@ {: first:n :}
   k 5 PRIM@ {: wid:n :}
   wid WL:NAMESPACE = if
      first NATIVE:U64,  k 1 PRIM@ NATIVE:U64,
   else
      NATIVE:TEXT-VA first + at CODE-CELL,
      k PRIM-LEN NATIVE:U64,
   then
   k 2 PRIM@ {: flags:n :}
   flags NATIVE:U64,
   flags DNAME-EXT and 0<> if
      NATIVE:TEXT-VA k 3 PRIM@ +  at 24 + CODE-CELL,
   else
      k 3 PRIM@ NATIVE:U64,
   then
   k PRIM-FACTS-AT @ NATIVE:U64,
   wid NATIVE:U64, ;


\ The packed emission's span, without the running image's slot padding.
\ A defer ends exactly at its trailer; a definer includes its does> clause.
: TARGET-LEN ( n -- n )
   {: s:n :}
   s NSHADOW:EMISSION@ {: e:n :}
   e NSHADOW:SIZE s NSHADOW:ENTRY@ - {: size:n :}
   e TRAILERS @ 0<> if size CODE-SPAN:EXACT exit then
   size e NSHADOW:RET-BYTES CODE-SPAN:RECORDED ;

\ A linked record names the target kernel or its packed emission.
\ A record without either keeps its own span.
: BUILD-LEN ( n -- n )
   {: r:n :}
   r XREF-REC {: rec:^ :}
   rec XREF-START {: a:n :}
   a NATIVE:KERNEL-ROW {: k:n :}
   k 0 >= if k PRIM-LEN exit then
   a 0<> if
      a REC-AT {: s:n :}
      s 0 >= if s TARGET-LEN exit then
   then
   rec XREF-RAW-LEN ;


\ Build record r's entry at the target's link address.
: ENTRY-ADDRESS ( n -- n )
   {: r:n :}
   r XREF-REC XREF-START {: a:n :}
   a 0= if 0 exit then
   a LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   r XREF-REC XREF-NAME$ {: name:^ nameu:n :}
   s" E-LINK-RECORD-ENTRY" REJECTION:OPEN
   s" `" REJECTION:EMIT-STR  name nameu REJECTION:EMIT-STR
   s" ` enters at " REJECTION:EMIT-STR  a REJECTION:EMIT-N
   s" , which is no code the engine holds" REJECTION:EMIT-STR
   REJECTION:HELP
   s" define `" REJECTION:EMIT-STR  name nameu REJECTION:EMIT-STR
   s" ` in a file the engine build loads" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


\ Build record r's source when r is an alias but not of a does> clause, else -1; an owned
\ primitive's record has a facts text, an alias none. Asked in index order from SHADOW-AT 0.
: EXPORT-SOURCE ( n -- n )
   {: r:n :}
   r XREF-REC {: rec:^ :}
   rec CK-REC-FACTS-SLOT XREF-CELL@ 0<> if -1 exit then
   r SHADOW-ROW  rec XREF-WORDLIST  KIND  KIND-ALIAS <> if -1 exit then
   r SAME-START {: src:n :}
   src 0 < if -1 exit then
   src 1 - XREF-DOES-COMPANION? if -1 exit then
   src ;

\ Build record r at target index k: [32], the facts text's offset from the image's DATA,
\ holds in the target's, as does an export's source index.
: BUILD-REC, ( n n -- )
   {: r:n k:n :}
   r XREF-REC {: rec:^ :}
   k DREC * {: at:n :}
   rec XREF-WORDLIST {: wid:n :}
   wid XREF:PACKAGE-WL? if -1 else r EXPORT-SOURCE then {: src:n :}
   wid XREF:PACKAGE-WL? if
      rec XREF-PKG-PUBLIC r WID-MAP NATIVE:U64,
      rec XREF-PKG-PRIVATE r WID-MAP NATIVE:U64,
   else
      r ENTRY-ADDRESS {: entry:n :}
      entry 0<> if entry at CODE-CELL, else 0 NATIVE:U64, then
      r BUILD-LEN NATIVE:U64,
   then
   rec XREF-FLAGS NATIVE:U64,
   rec XREF-EXT? if
      NATIVE:TEXT-VA  r FIRST-REC @ - NAME-AT @ +  at 24 + CODE-CELL,
   else
      rec XREF-NAME-SLOT XREF-CELL@ NATIVE:U64,
   then
   src 0 >= if  -1  PRIMS src +  FIRST-REC @ -  -
   else  rec CK-REC-FACTS-SLOT XREF-CELL@  then  NATIVE:U64,
   wid r WID-MAP NATIVE:U64, ;


: NAME-POOL ( -- )
   ndict@ FIRST-REC @ - NAME-AT-RESERVE
   ndict@ FIRST-REC @ ?do
      i XREF-REC {: rec:^ :}
      rec XREF-EXT? if
         NATIVE:SIZE  i FIRST-REC @ - NAME-AT !
         rec XREF-NAME$ NATIVE:BYTES,
      then
   loop ;


: TARGET-NDICT ( -- n ) PRIMS  ndict@ +  FIRST-REC @ - ;


\ After DATA-IMAGE: the names, then at an 8-byte boundary the records, then
\ the record list.
: RECORD-IMAGE ( -- )
   NAME-POOL
   NATIVE:SIZE NAMES-END !
   PAD8
   0 RLISTED-N !
   LRECS NATIVE:LABEL@ NATIVE:BIND
   PRIMS 0 ?do i PRIM-REC, loop
   0 SHADOW-AT !
   ndict@ FIRST-REC @ ?do  i  PRIMS i + FIRST-REC @ -  BUILD-REC,  loop
   LRECS-END NATIVE:LABEL@ NATIVE:BIND
   LRLIST NATIVE:LABEL@ NATIVE:BIND
   RLISTED-N @ 0 ?do i RLISTED @ NATIVE:U64, loop
   LRLIST-END NATIVE:LABEL@ NATIVE:BIND
   RLISTED-RELEASE  NAME-AT-RELEASE  PRIM-FACTS-AT-RELEASE ;


\ An engine's header cells beside HEADER-CELLS': the record count the seal
\ covers, the wordlist counter and the types wordlist, as the target numbers them.
: ENGINE-CELLS ( -- )
   TARGET-NDICT s" SEAL-NDICT-CELL" NATIVE:SYM HEADER!
   TARGET-WIDN s" WIDN-CELL" NATIVE:SYM HEADER!
   FIRST-DYNAMIC-WID TYPES-WL FIRST-WID - +  s" TYPES-WL-CELL" NATIVE:SYM HEADER! ;


: SET? ( n -- bool ) COPY-CELLS @ 0<> ;

\ The first non-zero cell at or after cell i, or COPY-N.
: NEXT-SET ( n -- n )
   begin dup COPY-N < if dup SET? 0= else false then while 1+ repeat ;

\ The cell after the run whose first cell is i. A run's header costs two
\ cells, so a gap of one or two zero cells stays inside it and three end it.
: RUN-END ( n -- n )
   begin dup 1+ NEXT-SET  2dup swap - 3 <=  over COPY-N < and while nip repeat
   drop 1+ ;

: RUN, ( n n -- )
   {: first:n end:n :}
   first CELL * NATIVE:U64,  end first - NATIVE:U64,
   end first ?do i COPY-CELLS @ NATIVE:U64, loop ;

\ A Mach-O image's copy as runs [off u64][n u64][n cells], off the run's byte
\ offset from DATA; zero cells outside a run are not written.
: RUNS, ( -- )
   0 NEXT-SET
   begin dup COPY-N < while dup RUN-END tuck RUN, NEXT-SET repeat drop ;


: CARRIER$ ( -- str ) 0 CARRIER CARRIER-N @ ;

\ The copy in pages, the last one zero past the copy's end: the boot moves whole
\ pages, and one image runs on every page size its machine's kernels have.
: PAGE-BYTES ( -- n ) NATIVE:X64? ELF:PAGE-BYTES ;
: PAGE-CELLS ( -- n ) PAGE-BYTES CELL / ;
: PAGES ( -- n ) COPY-N PAGE-CELLS 1- + PAGE-CELLS / ;

\ Whether page p holds a non-zero cell.
: PAGE-SET? ( n -- bool )
   PAGE-CELLS * {: first:n :}
   first PAGE-CELLS + COPY-N min {: end:n :}
   end first ?do i SET? if true unloop exit then loop false ;

\ The page holding the first non-zero cell at or after cell i, or PAGES.
: NEXT-PAGE ( n -- n )
   NEXT-SET dup COPY-N < if PAGE-CELLS / else drop PAGES then ;

\ The page after the run of carried pages whose first is p.
: PAGE-RUN-END ( n -- n )
   begin 1+ dup PAGES < if dup PAGE-SET? else false then while repeat ;

\ Page p's bytes appended to the carrier.
: CARRY-PAGE ( n -- )
   PAGE-BYTES * {: from:n :}
   CARRIER-N @ {: at:n :}
   at PAGE-BYTES + CARRIER-RESERVE
   COPY-N CELL * from - PAGE-BYTES min {: u:n :}
   from COPY-AT  at CARRIER  u BYTE-COPY
   PAGE-BYTES u ?do 0 at i + CARRIER c! loop
   PAGE-BYTES CARRIER-N +! ;

: PAGE-RUN, ( n n -- )
   {: first:n end:n :}
   first PAGE-BYTES * NATIVE:U64,
   end first - PAGE-BYTES * NATIVE:U64,
   end first ?do i CARRY-PAGE loop ;

\ An ELF image's copy as a table of runs [off u64][bytes u64], both multiples
\ of PAGE-BYTES, each a maximal run of pages holding a non-zero cell; the
\ pages go to CARRIER, and a page of zero cells is not carried.
: PAGE-RUNS, ( -- )
   0 CARRIER-N !
   0 NEXT-PAGE
   begin dup PAGES < while
      dup PAGE-RUN-END tuck PAGE-RUN,  PAGE-CELLS * NEXT-PAGE
   repeat drop ;


\ The cells holding a code address, the code header cells last.
: DATA-LIST ( -- )
   0 LISTED-N !  ['] LIST-ROW EACH-KEPT
   LIST,
   LIST-HEADER ;

\ After PLACE-ROUTINES: at an 8-byte boundary the copy with the header cells
\ written, as cell runs then the DATA list when macho, else as page runs then
\ an empty list.
: DATA-IMAGE ( bool -- )
   {: macho:bool :}
   NATIVE:SIZE CODE-END !
   PAD8
   LIMAGE NATIVE:LABEL@ NATIVE:BIND
   KEPT-COPY
   ['] COPY-ROW EACH-KEPT
   HEADER-CELLS
   ENGINE? @ if ENGINE-CELLS RECOVERY-CELLS then
   macho if RUNS, else PAGE-RUNS, then
   LIMAGE-END NATIVE:LABEL@ NATIVE:BIND
   LLIST NATIVE:LABEL@ NATIVE:BIND
   macho if DATA-LIST then
   LLIST-END NATIVE:LABEL@ NATIVE:BIND ;


\ Region kind k spans labels a to b.
: LABEL-REGION+ ( n ^n ^n -- )
   {: k:n a:^ b:^ :}
   k  a LABEL-OFFSET  b LABEL-OFFSET  REGION+ ;

\ What DATA-IMAGE and RECORD-IMAGE appended, for the map.
: REGIONS ( -- )
   DATA-KIND LIMAGE LIMAGE-END LABEL-REGION+
   DATA-LIST-KIND LLIST LLIST-END LABEL-REGION+
   ENGINE? @ 0= if exit then
   NAMES-KIND  LLIST-END LABEL-OFFSET  NAMES-END @  REGION+
   RECORDS-KIND LRECS LRECS-END LABEL-REGION+
   RECORD-LIST-KIND LRLIST LRLIST-END LABEL-REGION+ ;


: ADOPT-LABELS ( -- )
   NATIVE:ADOPT
   s" IMAGE" NATIVE:SYM-LABEL LIMAGE NATIVE:LABEL!
   s" IMAGE-END" NATIVE:SYM-LABEL LIMAGE-END NATIVE:LABEL!
   s" LIST" NATIVE:SYM-LABEL LLIST NATIVE:LABEL!
   s" LIST-END" NATIVE:SYM-LABEL LLIST-END NATIVE:LABEL!
   s" UNCAUGHT" NATIVE:SYM-LABEL LUNCAUGHT NATIVE:LABEL!
   s" SIGH" NATIVE:SYM-LABEL LSIGH NATIVE:LABEL!
   s" NCOUNT" NATIVE:SYM-LABEL LNCOUNT NATIVE:LABEL!
   s" DICT" NATIVE:SYM-LABEL LDICT NATIVE:LABEL!
   ENGINE? @ 0= if exit then
   s" RECS" NATIVE:SYM-LABEL LRECS NATIVE:LABEL!
   s" RECS-END" NATIVE:SYM-LABEL LRECS-END NATIVE:LABEL!
   s" RLIST" NATIVE:SYM-LABEL LRLIST NATIVE:LABEL!
   s" RLIST-END" NATIVE:SYM-LABEL LRLIST-END NATIVE:LABEL!
   s" FLOORREC" NATIVE:SYM-LABEL LFLOORREC NATIVE:LABEL! ;


\ A stripped program's code: the start-up, the kernel code the program reaches,
\ the routines and the data it reaches, for a Mach-O image when macho.
\ HEADER-CELLS reads LUNCAUGHT and LSIGH after the cut.
: LINK-STRIPPED ( bool -- )
   {: macho:bool :}
   ADOPT-LABELS
   ROUTINES
   REACH-DATA
   LUNCAUGHT NATIVE:LABEL@ NATIVE:ROOT-LABEL
   LSIGH NATIVE:LABEL@ NATIVE:ROOT-LABEL
   NATIVE:CUT
   NATIVE:SIZE KERNEL-END !
   false ['] DATA-ADDRESS PLACE-ROUTINES
   macho DATA-IMAGE ;


\ Kind k's chain in the kernel becomes adrp+add of the entry of k's word in
\ C2-MEM's public wordlist wid. An engine build loads and seals C2-MEM's seven
\ words before its link, which places every routine.
: SCOPE-ENTRY! ( n n -- )
   {: wid:n k:n :}
   k CHECK:SCOPE-WORD$ wid CHECKER-RESOLVE:WL-PROBE {: rec:^ :}
   rec XREF-START LINK-ADDRESS {: to:n :}
   k NATIVE:SCOPE-SITE NATIVE:OFFSET {: at:n :}
   to  NATIVE:TEXT-VA at +  NATIVE:CODE at +  NATIVE:CODE-ADDRESS! ;


\ After PLACE-ROUTINES, scope-kind?'s chains name C2-MEM's entries; a target
\ without C2-MEM leaves them 0.
: SCOPE-ENTRIES ( -- )
   s" C2-MEM" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:^ :}
   pkg XREF-FOUND? 0= if exit then
   pkg XREF-PKG-PUBLIC {: wid:n :}
   9 2 ?do wid i SCOPE-ENTRY! loop ;


\ An engine's code: the start-up, the whole kernel, every routine, DATA, then
\ the records, for a Mach-O image when macho.
: LINK-ENGINE ( bool -- )
   {: macho:bool :}
   ADOPT-LABELS
   NATIVE:SIZE KERNEL-END !
   PRIM-TEXTS
   ROUTINES REACH-ALL
   true ['] DATA-ADDRESS PLACE-ROUTINES
   SCOPE-ENTRIES
   macho DATA-IMAGE
   RECORD-IMAGE ;


\ Write the linked image and sign it, a Mach-O one when macho, else an ELF one.
: EMIT-IMAGE ( str str bool -- )
   {: sig:^ sigu:n path:^ pathu:n macho:bool :}
   macho if
      sig sigu MACHO:SET-SIGID
      MACHO:ASM-CODE NATIVE:TEXT-VA NATIVE:MACHO-IMPORTS$ MACHO:BUILD-IMAGE
      MACHO:CODESIG2
   else
      sig sigu ELF:SET-SIGID
      NATIVE:X64? if
         NATIVE:TEXT-VA X64CODE:FIXUPS!
         NATIVE:CODE NATIVE:SIZE NATIVE:TEXT-VA
         NATIVE:ELF-IMPORTS$ CARRIER$ ELF:BUILD-X64
      else
         ELF:ASM-CODE NATIVE:TEXT-VA NATIVE:ELF-IMPORTS$ CARRIER$
         ELF:BUILD-IMAGE
      then
      ELF:CODESIG2
   then
   path pathu DRV-WRITE-IMAGE ;


\ Link the open space's program and write it, signed, to the path, as Mach-O
\ when macho, else as ELF; the caller first reads the kernel object of the
\ build's kind and stores the program's entry. Only an engine's records read
\ facts texts, so only an engine's link places those the build's body held,
\ after every file, where no file's literal can name one. The program goes to
\ a sibling renamed over the path: macOS kills a signed program rewritten in
\ place.
: WRITE-PROGRAM ( str bool -- )
   {: path:^ pathu:n macho:bool :}
   ENGINE? @ if 0 PLACE-FACTS then
   macho ENGINE? @ if LINK-ENGINE else LINK-STRIPPED then
   path pathu [: 2dup ENGINE? @ if SIGN-ID:ENGINE$ else SIGN-ID:PROG$ then 2swap
      macho EMIT-IMAGE  FS:CHMOD-X ;] FS:REPLACE-CACHED
   REGIONS  path pathu CODE-END @ WRITE-MAP ;

public

\ Link the open space's program and write it to the path: a Wasm target's as a
\ module, a Mach-O or ELF target's as WRITE-PROGRAM does. GOING comes first:
\ outside a build no target is open for the match to read.
: WRITE ( str -- )
   {: path:^ pathu:n :}
   false GOING
   TARGET @ RTARGET:IMAGE@ MATCH RTARGET:image
      macho OF path pathu true WRITE-PROGRAM ENDOF
      elf OF path pathu false WRITE-PROGRAM ENDOF
      pe OF
         s" E-LINK-NO-WRITER" REJECTION:OPEN
         s" the link has no file writer for a PE target" REJECTION:EMIT-STR
         REJECTION:HELP
         s" build for a Linux or macOS target" REJECTION:EMIT-STR
         EXIT-STATUS:REJECTED REJECTION:STOP
      ENDOF
      wasm OF path pathu WASM:WRITE-MODULE ENDOF
   ;MATCH ;

;package
;using
