\ link-data.f - a build's DATA: the link's copy of the open space's cells, each
\ declared cell given its value at DATA-VA, appended to A64ICODE's code buffer
\ after the routine band as its non-zero runs, then the DATA list, the offset
\ from DATA-VA of every cell holding a code address, which start-up adds the
\ load offset to. The link also writes the header cells HEADER-CELLS names and
\ lists the code ones. An engine's link adds the records: the target kernel's
\ primitives, then the build's, each build record and wordlist id renumbered for
\ the target (RECORD-IMAGE), and the record list. LINKER:WRITE writes the
\ linked program; the other words are LINKER's own.

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
\ dictionary table; an engine's throw recoveries.
variable LUNCAUGHT
variable LSIGH
variable LNCOUNT
variable LDICT
variable LEVALREC
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


\ A bound label's byte offset in A64ICODE's code buffer.
: LABEL-OFFSET ( ptr n -- n ) NATIVE:LABEL@ NATIVE:OFFSET ;


\ The image is the space's runs: its length is the space's, not the distance
\ between its labels.
: DATA-IMAGE-AT ( -- n ) LIMAGE LABEL-OFFSET ;
: DATA-LIST-AT ( -- n ) LLIST LABEL-OFFSET ;
: DATA-LIST-N ( -- n ) LLIST-END LABEL-OFFSET DATA-LIST-AT - 8 / ;


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
   s" link: the xt cell at offset " SAY off SAY-INT s"  holds " SAY v SAY-INT SAY-CR
   s" link: an xt cell names a routine the target does not place" 74 die ;


: COPY-ROW ( n -- )
   {: row:n :}
   row ROW-OFF {: off:n :}
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
   row ROW-OFF LISTED-N @ LISTED !  1 LISTED-N +! ;

: LIST, ( -- )
   LISTED-N @ {: n:n :}
   n 0 > if 0 LISTED n [: < ;] SORT:SORT! then
   n 0 ?do i LISTED @ NATIVE:U64, loop ;


\ A cell the build stored into would be written and slid twice.
: HEADER! ( n n -- )
   {: v:n off:n :}
   off SPACE-AT @ 0<> if
      s" link: the build stored into the header cell at offset " SAY off SAY-INT SAY-CR
      s" link: a build stored into a header cell the link writes" 74 die
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
   va s" FD-CELL" NATIVE:SYM +  s" FD-PTR-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA s" RBASE-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA LUNCAUGHT LABEL-OFFSET +  s" UNCGH-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA LSIGH LABEL-OFFSET +  s" STUB-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA KERNEL-END @ +  s" CODE-END-CELL" NATIVE:SYM HEADER!
   \ The build's MARK left its own index mapping's address here; the target's
   \ first MARK builds one from 0.
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;


\ An engine's throw recoveries: its kernel branches through these cells inside an
\ evaluate and after a data-stack underdepth. A stripped program's cut keeps neither.
: RECOVERY-CELLS ( -- )
   NATIVE:X64? if 0 else NATIVE:TEXT-VA LEVALREC LABEL-OFFSET + then
   s" EVALREC-CELL" NATIVE:SYM HEADER!
   NATIVE:TEXT-VA LFLOORREC LABEL-OFFSET +  s" FLOORREC-CELL" NATIVE:SYM HEADER! ;

\ The header cells HEADER-CELLS and RECOVERY-CELLS give a code address.
: LIST-HEADER ( -- )
   s" RBASE-CELL" NATIVE:SYM NATIVE:U64,
   s" UNCGH-CELL" NATIVE:SYM NATIVE:U64,
   s" STUB-CELL" NATIVE:SYM NATIVE:U64,
   s" CODE-END-CELL" NATIVE:SYM NATIVE:U64,
   ENGINE? @ if
      NATIVE:X64? 0= if s" EVALREC-CELL" NATIVE:SYM NATIVE:U64, then
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

: PRIM-NAME$ ( n -- ptr u8 n )
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
   k PRIM-NAME$ CHECK:PRIMITIVE-FACTS {: a:ptr u:n :}
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


\ Shadow record s's [8] cell as native publication records it: a definer's span
\ runs over its does> clause to its emission's end.
: TARGET-LEN ( n -- n )
   {: s:n :}
   s NSHADOW:EMISSION@ {: e:n :}
   e NSHADOW:SIZE s NSHADOW:ENTRY@ -  e NSHADOW:RET-BYTES CODE-SPAN:RECORDED ;

\ Build record r's length: the target kernel's for a kernel primitive's, in a
\ cross-build its target emission's, else its own: a target word's host routine
\ is the error routine.
: BUILD-LEN ( n -- n )
   {: r:n :}
   r XREF-REC {: rec:ptr :}
   rec XREF-START {: a:n :}
   a NATIVE:KERNEL-ROW {: k:n :}
   k 0 >= if k PRIM-LEN exit then
   CROSS? a 0<> and if
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
   s" link: record " SAY r XREF-REC XREF-NAME$ SAY s"  enters at " SAY a SAY-INT
   s" , which the target does not place" SAY SAY-CR
   s" link: a record enters code the target does not place" 74 die ;


\ Build record r's source when r is an alias but not of a does> clause, else -1; an owned
\ primitive's record has a facts text, an alias none. Asked in index order from SHADOW-AT 0.
: EXPORT-SOURCE ( n -- n )
   {: r:n :}
   r XREF-REC {: rec:ptr :}
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
   r XREF-REC {: rec:ptr :}
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
      i XREF-REC {: rec:ptr :}
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
   FIRST-DYNAMIC-WID BOOT:TYPES-WL FIRST-WID - +  s" TYPES-WL-CELL" NATIVE:SYM HEADER! ;


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

\ The copy as runs [off u64][n u64][n cells], off the run's byte offset from
\ DATA; zero cells outside a run are not written.
: RUNS, ( -- )
   0 NEXT-SET
   begin dup COPY-N < while dup RUN-END tuck RUN, NEXT-SET repeat drop ;


\ After PLACE-ROUTINES: at an 8-byte boundary the copy's runs with the header
\ cells written, then the DATA list, the code header cells last.
: DATA-IMAGE ( -- )
   NATIVE:SIZE CODE-END !
   PAD8
   LIMAGE NATIVE:LABEL@ NATIVE:BIND
   KEPT-COPY
   ['] COPY-ROW EACH-KEPT
   HEADER-CELLS
   ENGINE? @ if ENGINE-CELLS RECOVERY-CELLS then
   RUNS,
   LIMAGE-END NATIVE:LABEL@ NATIVE:BIND
   LLIST NATIVE:LABEL@ NATIVE:BIND
   0 LISTED-N !  ['] LIST-ROW EACH-KEPT
   LIST,
   LIST-HEADER
   LLIST-END NATIVE:LABEL@ NATIVE:BIND ;


\ Region kind k spans labels a to b.
: LABEL-REGION+ ( n ptr n ptr n -- )
   {: k:n a:ptr b:ptr :}
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
   NATIVE:X64? 0= if s" EVALREC" NATIVE:SYM-LABEL LEVALREC NATIVE:LABEL! then
   s" FLOORREC" NATIVE:SYM-LABEL LFLOORREC NATIVE:LABEL! ;


\ A stripped program's code: the start-up, the kernel code the program reaches,
\ the routines and the data it reaches. HEADER-CELLS reads LUNCAUGHT and LSIGH
\ after the cut.
: LINK-STRIPPED ( -- )
   ADOPT-LABELS
   ROUTINES
   REACH-DATA
   LUNCAUGHT NATIVE:LABEL@ NATIVE:ROOT-LABEL
   LSIGH NATIVE:LABEL@ NATIVE:ROOT-LABEL
   NATIVE:CUT
   NATIVE:SIZE KERNEL-END !
   ['] DATA-ADDRESS PLACE-ROUTINES
   DATA-IMAGE ;


\ C2-MEM's word whose entry scope-kind? answers kind k for.
: SCOPE-WORD$ ( n -- ptr u8 n )
   {: k:n :}
   k 2 = if s" WITH-READ" exit then
   k 3 = if s" WITH-MUT" exit then
   k 4 = if s" WITH-MUT-LOAN" exit then
   k 5 = if s" WITH-INIT" exit then
   k 6 = if s" WITH-RECORDS" exit then
   k 7 = if s" WITH-RECORD" exit then
   k 8 = if s" WITH-FIELD" exit then
   s" link: scope-kind? has no kind past 8" 74 die ;


\ Kind k's chain in the kernel becomes adrp+add of the entry of k's word in
\ C2-MEM's public wordlist wid.
: SCOPE-ENTRY! ( n n -- )
   {: wid:n k:n :}
   k SCOPE-WORD$ wid CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if
      s" link: C2-MEM has no " SAY k SCOPE-WORD$ SAY SAY-CR
      s" link: C2-MEM lacks a scope entry" 74 die
   then
   rec XREF-START LINK-ADDRESS {: to:n :}
   to -1 = if s" link: a C2-MEM scope entry is code the target does not place" 74 die then
   k NATIVE:SCOPE-SITE NATIVE:OFFSET {: at:n :}
   to  NATIVE:TEXT-VA at +  NATIVE:CODE at +  NATIVE:CODE-ADDRESS! ;


\ After PLACE-ROUTINES, scope-kind?'s chains name C2-MEM's entries; a target
\ without C2-MEM leaves them 0.
: SCOPE-ENTRIES ( -- )
   s" C2-MEM" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if exit then
   pkg XREF-PKG-PUBLIC {: wid:n :}
   9 2 ?do wid i SCOPE-ENTRY! loop ;


\ An engine's code: the start-up, the whole kernel, every routine, DATA, then
\ the records.
: LINK-ENGINE ( -- )
   ADOPT-LABELS
   NATIVE:SIZE KERNEL-END !
   PRIM-TEXTS
   ROUTINES REACH-ALL
   ['] DATA-ADDRESS PLACE-ROUTINES
   SCOPE-ENTRIES
   DATA-IMAGE
   RECORD-IMAGE ;


: OBJECT-KIND ( -- )
   KERNEL? @ if s" link: a kernel build writes no program" 74 die then
   NATIVE:READ? 0= if s" link: no kernel object was read for this build" 74 die then
   TARGET @ RTARGET:ARCH@ CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ
   NATIVE:X64? <> if
      s" link: the kernel object is for another architecture" 74 die
   then
   ENGINE? @ if 1 else 0 then  s" ENGINE" NATIVE:SYM <> if
      s" link: the kernel object is another kind of build's" 74 die
   then ;


: EMIT-IMAGE ( ptr u8 n ptr u8 n -- )
   {: sig:ptr sigu:n path:ptr pathu:n :}
   TARGET @ RTARGET:IMAGE@ MATCH RTARGET:image
      macho OF
         MACHO:ASM-CODE NATIVE:TEXT-VA MACHO:BUILD-IMAGE
         sig sigu MACHO:SET-SIGID MACHO:CODESIG2
      ENDOF
      elf OF
         NATIVE:X64? if
            NATIVE:TEXT-VA X64CODE:FIXUPS!
            NATIVE:CODE NATIVE:SIZE NATIVE:TEXT-VA ELF:BUILD-X64
         else ELF:ASM-CODE NATIVE:TEXT-VA ELF:BUILD-IMAGE then
         sig sigu ELF:SET-SIGID ELF:CODESIG2
      ENDOF
      pe OF s" link: no file writer for a PE target" 74 die ENDOF
      wasm OF s" link: a Wasm target's module is written by WASM:WRITE-MODULE" 74 die ENDOF
   ;MATCH
   path pathu DRV-WRITE-IMAGE ;


\ Link the open space's program and write it, signed, to the path; the caller
\ first stores its entry. Only an engine's records read facts texts, so only an
\ engine's link places those the build's body held, after every file, where no
\ file's literal can name one. The program goes to a sibling renamed over the
\ path: macOS kills a signed program rewritten in place.
: WRITE-PROGRAM ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   OBJECT-KIND
   ENGINE? @ if 0 CHECK:PLACE-FACTS then
   s" ENTRY-CELL" NATIVE:SYM {: entry:n :}
   entry SPACE-AT @ 0= if
      s" link: the build stored no entry for the program" 74 die
   then
   ENGINE? @ if LINK-ENGINE else LINK-STRIPPED then
   path pathu [: 2dup ENGINE? @ if SIGN-ID:ENGINE$ else SIGN-ID:PROG$ then 2swap EMIT-IMAGE
      FS:CHMOD-X ;] FS:REPLACE-STAGED
   REGIONS  path pathu CODE-END @ WRITE-MAP ;

public

\ Link the open space's program and write it to the path: a Wasm target's as a
\ module, any other's as WRITE-PROGRAM does. GOING comes first: outside a build
\ no shadow is open for WASM? to read.
: WRITE ( ptr u8 n -- )
   false GOING
   WASM? if WASM:WRITE-MODULE exit then
   WRITE-PROGRAM ;

;package
;using
