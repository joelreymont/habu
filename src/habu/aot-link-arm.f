\ aot-lib.f - stripped AOT linker words, over src/habu/aot-closure.f's members.
\
\ tools/aot-build.f loads the application through the retained native compiler.
\ LINK writes the selected entry's reachable closure into a standalone image,
\ relocates its code and emits the minimal runtime entry. tools/hb-build.f owns
\ the output paths; the default application entry is MAIN.
package AOT-LINK
private
: LOAD-X64-WRITER ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/habu/image-x64.f" required
      s" src/habu/boot-x64.f" required
   then ;
' LOAD-X64-WRITER
;package
execute
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

\ No static require can name a target's file, so the emission that needs one
\ selects it, as src/habu/app-image-core.f does. The
\ system calls come first: this file's startup code, src/habu/rt.f and
\ src/habu/crash.f emit through SYS, and the target's call numbers.
package AOT-LINK
private

: LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   HB-TARGET-LINUX-X86-64? if s" src/os/linux-x86-64/sys.f" required exit then
   s" aot: unsupported target" 76 die ;

' LOAD-SYS
;package
execute
require src/os/env-base.f
require src/habu/stack-abi.f
require src/habu/layout.f
require src/habu/rt.f
require src/habu/crash.f
require src/habu/xref.f
require src/habu/fdio.f
require src/lib/le.f
require src/habu/address-carrier.f
require src/habu/aot-decl.f
require src/habu/aot-window-latch.f
require src/habu/aot-owned-cells.f
require src/habu/aot-closure.f

\ Full-image emission owns its signer and driver. This linker loads only after
\ the application span is latched; snapshot support needs neither dependency.
\ Each signer patches its target's image format. The selected writer loads
\ src/os/image-bytes.f under its own code-package scope before using the buffer.
package AOT-LINK
private

: LOAD-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/elf.f" required
      s" src/os/linux-x86-64/sign.f" required
      exit
   then
   s" aot: unsupported target" 76 die ;

' LOAD-IMAGE
;package
execute
require src/habu/sign-id.f
require src/habu/driver-io.f
require src/habu/aot-common.f

\ The AOT relocation core compiles checked. It works over CLOSURE MEMBERS - a
\ code entry and a length, held in the parallel arrays src/habu/aot-closure.f
\ fills - and not over dictionary records, because the image ships no record for
\ a word nothing can name and the payload's span table is what accounts for one.
\ A member is named by its index, so every read here is a cell of an owned array
\ and the record-shaped TRUSTED: boundary this file used to carry is gone.

package AOT-LINK
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM
using A64ICODE


\ --- emit the image: minimal entry + compacted, relocated blobs.
variable MLBL
variable CODE-END
\ The label at text offset zero: the image's own code base, which the declared
\ xt cells are restored against. It costs no bytes and is placed by EMIT-ENTRY.
variable LTEXT
variable LDPREFUSE  variable LDPREFUSE-NUM
variable LDPREFUSE-MSG  variable LDPREFUSE-OF  variable LDPREFUSE-UNIT
variable LUNCREP  variable LUNCREP-MSG  variable LUNCREP-SIGN  variable LUNCREP-NL

\ ADDRESS A LABEL THE STARTUP CANNOT REACH WITH ADR. `adr` carries a signed
\ 21-bit byte delta (src/arch/arm64/icode.f ?ADR, ±1 MiB), and the startup sits
\ at text offset zero while the labels below bind past the whole copied code
\ band: the entry word (MLBL, inside the band), the crash handler, the signal
\ stub and the sparse data blob (all after it). Tender's standalone closure is
\ 1.36 MB of code, so those four are the only sites in an emitted image that
\ cross the window - a copied member's own body never enters ?ADR (COPY-COMPACT-BLOB
\ emits its words raw and RELOC-W32 re-encodes a member's ADR with its own
\ refusal), and DATA and string references travel as movz/movk chains.
\ THE BASE IS LTEXT, NOT THE TEXT-BASE CLAIM. src/habu/habu2.f TADR, reads the
\ running image's code base out of DATA RBASE-CELL, which the startup itself has
\ to publish first: EMIT-OWNED-CELLS stores the claims in claim order and the
\ ENTRY-XT claim (aot-owned-cells.f:279) comes before the TEXT-BASE one (:280),
\ so at the first of these four sites the cell is still zero. LTEXT is bound at
\ text offset zero, behind every startup site, so `ADR` reaches it by
\ construction and it holds exactly the value site EMIT-OWNED-CELLS publishes
\ into RBASE-CELL. LOFF, then carries the target's own byte offset from that
\ base in two 16-bit lanes, a non-negative absolute position whose bound is the
\ pair's 32 bits and not the distance from the site. Four words, 16 bytes,
\ per converted site: 48 bytes more per image than the four `ADR`s were.
\ NO FIXTURE BUILDS AN IMAGE PAST THE WINDOW, and the LINK is no longer what
\ stops one: MEMBER-ORDER below sorts the closure members by entry once per link
\ and both lookups binary-search that order, so OLD>NEW and MEMBER-AT no longer
\ scan every member per relocated instruction. THE COMPILE is what a >1 MiB
\ fixture costs. Measured on a generated chain of 27,000 words, 1,082,700 bytes
\ of code in 27,004 closure members: hb-build 51.9 s cold, of which the AOT
\ maker alone is 42.8 s, and an hb-build of those same 27,000 words behind a
\ one-member closure - the same compile, no closure to link - is 39.3 s. The
\ member scans this replaced were 2.4 s of that maker run (45.2 s before) and
\ 0.5 s of the 13,504-member half of it: four times the cost for twice the
\ members is the quadratic term, and it is the term that is gone.
\ tools/hb-build-stripped-chain-test.f HBT-STRIPPED-CHAIN runs the converted
\ startup end to end at 1,104 members, and Tender's standalone build is the
\ proof at the scale that needed it.
: TEXT-ADR, ( n n label -- )                      \ ( rd rt label -- ) rd = runtime address of label
   {: rd:n rt:n l:label :}
   rd rt = IF s" aot: TEXT-ADR, destination and scratch are one register" 74 die THEN
   rd l LOFF,                                     \ rd = the label's byte offset from text offset zero
   rt LTEXT LABEL@ ADR,                           \ rt = this image's code base
   rd rt rd ADD, ;

: EMIT-DATA-REGION-MAP ( -- )
   LBL {: dvok:label :}
   0 DATA-VA VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-DATA-RESERVE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   dvok LBL,
   DATA 0 0 ADDI,                                \ x20 = DATA-VA (mmap result, verified)
   XDS DATA STACK-ABI:BASE-CELL STR,
   7 STACK-ABI:BOOT-BYTES LIT64,  7 DATA STACK-ABI:CAP-CELL STR, ;

\ ---- the engine runtime cells this entry owns ---------------------------------
\ THE KERNEL'S INITIAL STACK, read the way src/habu/habu2.f EM-ENTRY-ARGS reads
\ it, because a stripped image's entry IS a process entry: argc at [sp], argv one
\ cell above, envp one past argv's NULL terminator. THIS RUNS FIRST, before any
\ other emitted instruction: it is the only point at which SP still names the
\ kernel's frame (G-INSTALL-CRASH-X11 builds a frame of its own below it) and x0-x2
\ still hold the macOS entry's own three arguments. Nothing between here and
\ EMIT-OWNED-CELLS touches x13-x15 - crash.f C-SIGNAL-STACK works in x0/x1/x12,
\ STACK-GUARD:EMIT-MAP in x0-x6/x9/x10, and a Linux syscall returns in x0 and
\ preserves the rest.
: EMIT-ENTRY-ARGS ( -- )
   HB-TARGET-LINUX? IF
      13 SP 0 LDR,  14 SP 8 ADDI,
      15 13 1 ADDI,  15 15 3 LSLI,  15 14 15 ADD,
      exit
   THEN
   HB-TARGET-MACOS? IF
      13 0 0 ADDI,  14 1 0 ADDI,  15 2 0 ADDI,
      exit
   THEN
   s" aot: unknown target" 74 die ;

\ THE LIST IS src/habu/aot-owned-cells.f, the one src/habu/aot-closure.f
\ CLAIMED-CELL? admits from, so a cell this publishes and a cell the walker lets
\ through are the same cell by construction.
\ A FRESH claim emits nothing: EMIT-DATA-REGION-MAP just mapped DATA anonymously,
\ so the cell already holds the zero its claim says is correct. A CARRIED claim
\ emits nothing either, for the opposite reason: CARRY-CELLS below already copied
\ its bytes into the window, so EMIT-DATA-COPY restores them with the rest of the
\ application's data and there is no cell here to publish. An IMAGE-BASE
\ claim gets one store of x20 - this image's own DATA base, which is the value the
\ declaring file wrote into the cell when the engine loaded it.
\ The three fixed startup cells go with them, and are named by their layout
\ constants rather than by a claim because no code spells out their address:
\ src/os/env-base.f reaches them through the IMAGE-BASE cell, and habu2.f
\ EM-DATA-INIT publishes them for the engine out of these same three registers.
\ They sit below DATA-START, so EMIT-DATA-COPY's restore cannot reach them.
\ An ENTRY-XT claim gets the address of the word this image starts, which is the
\ label the root call below branches to, and a TEXT-BASE claim this image's own
\ code base (LTEXT, the label at text offset zero - the value the engine's entry
\ stores with EM-DATA-INIT). The entry word binds inside the copied code band, so
\ it goes through TEXT-ADR, with x12 as the scratch - dead here, because
\ EMIT-OWNED-CELLS stores the argc/argv/envp registers out before its loop and
\ holds nothing else across an iteration. LTEXT is the base TEXT-ADR, itself
\ reaches, so its own site keeps ADR,.
: EMIT-OWNED-CELLS ( -- )
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   AOT-OWNED:N 0 ?do
      i OWNED-PUBLISHED? IF
         9 i AOT-OWNED:AT DATA-VA VA>N - LIT64,   \ x9 = the cell's DATA offset
         9 DATA 9 ADD,                            \ ... the cell itself
         i AOT-OWNED:ENTRY-XT? IF
            11 12 MLBL LABEL@ TEXT-ADR,           \ x11 = this image's entry word
            11 9 0 STR,                           \ *cell = the entry it starts at
         ELSE i AOT-OWNED:TEXT-BASE? IF
            11 LTEXT LABEL@ ADR,                  \ x11 = this image's code base
            11 9 0 STR,                           \ *cell = the text base `rbase` answers
         ELSE
            DATA 9 0 STR,                         \ *cell = x20, this image's DATA base
         THEN THEN
      THEN
   loop ;

\ A carried FFI table is at a different absolute DATA address in each maker's
\ application window. The entry installs that mapped address into the fresh
\ engine control cell; the DATA blob carries only the identical table bytes.
variable FFI-BASE-INDEX

: FFI-BASE-MARK@ ( -- ptr u8 )
   FFI-BASE-INDEX @ 0= if FFI-FN-BASE-MARK @ else FFI-LIB-BASE-MARK @ then ;

: EMIT-FFI-BASE ( ptr u8 -- ) {: source:ptr :}
   FFI-BASE-MARK@ FFI:>CELL {: table:n :}
   table 0= if 0 else
      table MAPPED-DATA {: target:n ok:bool :}
      ok 0= if s" aot: FFI table base has no restored destination" 74 die then
      target
   then {: target:n :}
   9 source data-base BYTE-VIEW - LIT64,
   9 DATA 9 ADD,
   11 target LIT64,
   11 9 0 STR,
   1 FFI-BASE-INDEX +! ;

: EMIT-FFI-BASES ( -- )
   0 FFI-BASE-INDEX !
   [: EMIT-FFI-BASE ;] FFI:OWNED-BASES ;

\ ONE UNSIGNED LEB128 VARINT, INLINE: seven bits a byte from the cursor `cur`,
\ low group first, until a byte arrives with its high bit clear. `acc` answers
\ the value, `cur` is advanced past it, and `b`, `g` and `sh` are clobbered.
\ This is src/habu/aot-decl.f AOT-WINDOW:CELL-V! read backwards, and the same
\ grammar src/habu/habu2.f AOT-WINDOW:APPLY-CELLS decodes for the baked window.
: EMIT-VGET ( n n n n n -- ) {: acc:n cur:n b:n g:n sh:n :}
   LBL {: vtop:label :}
   acc 0 MOVZ,  sh 0 MOVZ,
   vtop LBL,
      b cur 0 LDRB,  cur cur 1 ADDI,
      g b $7F ANDI,  g g sh LSLV,  acc acc g ORR,
      sh sh 7 ADDI,
      g b $80 ANDI,  g vtop CBNZ, ;


\ The blob's header: the group count and the stored bitmap bytes, as two u32
\ (BUILD-SPARSE-DATA below writes them last, over room reserved first). The
\ reader it frames is emitted above the writer, so the width is named here.
8 constant SPARSE-HDR

\ A BIT A CELL, IN CELL ORDER, so the destination cursor x13 is the decoder's
\ whole state between cells: it steps one cell for every bit the bitmap carries,
\ present or not, and the value cursor x10 advances only where a bit is set. An
\ all-clear bitmap byte therefore costs one branch and one add for eight cells,
\ which is what makes a window of mostly `allot`ed room cheap to lay down.
\ THE BITMAP ITSELF IS GROUPED (src/habu/aot-decl.f AOT-WINDOW:BM-COMPACT): an
\ all-clear GROUP-BYTES bytes of it is not in the image, and costs one branch and
\ one add for GROUP-SPAN bytes of DATA. The outer loop walks the presence map's
\ bits; a set bit runs the byte walk over that group's stored GROUP-BYTES bytes,
\ a clear one steps the destination past the group. This is the shape
\ src/habu/habu2.f AOT-WINDOW:APPLY-CELLS runs for the baked window; the two
\ differ only in where the frame comes from - three labels there, the blob's own
\ two-u32 header here - and in which registers are free.
\ x10 ends one past the blob's last value byte, which is where EMIT-XT-CELLS
\ expects its rows.
: EMIT-DATA-COPY ( -- )
   BLOB-LEN @ 0= IF
      7 BLOB-END @ LIT64,  7 DATA DP-CELL STR,  exit         \ DP = data base (no user data)
   THEN
   9 12 BLOB-LBL LABEL@ TEXT-ADR,                 \ x9 = sparse header in __text (x12: the loop reloads it below)
   11 9 0 LDRW,                                   \ x11 = groups the presence map covers
   10 9 4 LDRW,                                   \ x10 = bytes of stored bitmap groups
   9 9 SPARSE-HDR ADDI,                           \ x9 = presence map cursor
   11 11 7 ADDI,  11 11 3 LSRI,                   \ x11 = ceil(groups / 8) map bytes
   11 9 11 ADD,                                   \ x11 = one past the presence map
   17 11 0 ADDI,                                  \ x17 = the stored groups, after the map
   10 17 10 ADD,                                  \ x10 = value cursor, after the stored groups
   13 BLOB-SRC @ LIT64,                           \ x13 = destination cursor, at the span's base VA
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: gtop:label gdone:label ptop:label pnext:label absent:label
      bmtop:label bmdone:label bittop:label bitdone:label bitnext:label empty:label :}
   gtop LBL,
      9 11 CMP,  C-CS gdone BCOND,
      23 9 0 LDRB,  9 9 1 ADDI,                   \ x23 = this byte's eight groups
      24 8 MOVZ,                                  \ x24 = groups left in the byte
      ptop LBL,
         24 gtop CBZ,
         21 23 1 ANDI,
         21 absent CBZ,
            25 AOT-WINDOW:GROUP-BYTES MOVZ,       \ x25 = bitmap bytes left in the group
            bmtop LBL,
               25 bmdone CBZ,
               14 17 0 LDRB,  17 17 1 ADDI,       \ x14 = this byte's eight cells
               25 25 1 SUBI,
               14 empty CBZ,
               12 AOT-WINDOW:CELL-BITS MOVZ,      \ x12 = cells left in the byte
               bittop LBL,
                  12 bitdone CBZ,
                  21 14 1 ANDI,
                  21 bitnext CBZ,
                     15 10 16 22 7 EMIT-VGET      \ x15 = the present cell's value
                     15 13 0 STR,
                  bitnext LBL,
                  14 14 1 LSRI,  13 13 AOT-WINDOW:CELL-BYTES ADDI,  12 12 1 SUBI,
                  bittop B,
               bitdone LBL,
               bmtop B,
            empty LBL,
               13 13 AOT-WINDOW:BM-BYTE-SPAN ADDI, \ eight absent cells: step the destination
               bmtop B,
            bmdone LBL,
         pnext LBL,
         23 23 1 LSRI,  24 24 1 SUBI,
         ptop B,
   absent LBL,
      21 AOT-WINDOW:GROUP-SPAN MOVZ,  13 13 21 ADD,  \ an absent group: step the destination
      pnext B,
   gdone LBL,
   7 BLOB-SRC @ SPAN-CELLS AOT-WINDOW:CELL-BYTES * + LIT64,
   7 DATA DP-CELL STR, ;                           \ DP = user-end (runtime here/allot base)

\ ---- the declared xt cells: what the copy alone cannot restore -----------------
\ The copy above restores DATA with declared XT bytes zeroed. The rows
\ EMIT-XT-ROWS places after the blob say where each XT cell is and where its word
\ ended up in THIS image, in the shape src/habu/aot-file.f's XTOFF rows carry: a
\ u32 location and a u32 target in one eight-byte row.
\ THE ROWS NEED NO ADDRESS OF THEIR OWN. The copy loop's byte cursor x10 ends one
\ past the blob's last payload byte, which is where the rows begin (BYTES, pads
\ the blob to the four-byte boundary they sit on), so the startup adds no second
\ address-of-the-blob sequence ahead of the root call: test/gate-aot-image.f
\ admits exactly one of them in the startup and ends the reported code range at
\ the blob, and rows placed past the blob are outside that range rather than read
\ as instructions.
\ The cell is x20-relative (DATA is mapped at a fixed address, so its offset is the same
\ number the declaration carries) and the value is relative to the image's own
\ code base, which is the label at text offset zero.
: EMIT-XT-CELLS ( -- )
   XTC-N @ 0= IF exit THEN
   BLOB-LEN @ 0= IF s" aot: declared xt cells without a restored data span" 74 die THEN
   10 10 3 ADDI,  10 10 2 LSRI,  10 10 2 LSLI,     \ x10 = the rows, at the 4-byte boundary
   11 XTC-N @ LIT64,                               \ x11 = rows remaining
   12 LTEXT LABEL@ ADR,                            \ x12 = this image's code base
   LBL LBL {: xtop:label xdone:label :}
   xtop LBL,
      11 xdone CBZ,
      13 10 0 LDRW,                                \ x13 = the cell's DATA offset
      14 10 4 LDRW,                                \ x14 = its word's code offset
      13 DATA 13 ADD,                              \ ... the cell
      14 12 14 ADD,                                \ ... and the token it takes
      14 13 0 STR,
      10 10 8 ADDI,  11 11 1 SUBI,  xtop B,
   xdone LBL, ;

\ --- sparse encoding: the captured span travels as a presence bitmap over its
\ CELLS and one varint per present cell, not as the span. A table `allot`ed at
\ declared capacity but only partly filled left its unused tail as literal zero
\ bytes in every earlier image; the restore above maps an anonymous (already
\ zero) region, so a zero byte never has to travel. Format:
\ [groups u32][stored bitmap bytes u32][presence map, one bit a group, low bit
\ first][the present groups, AOT-WINDOW:GROUP-BYTES bytes each in group order,
\ the last zero-padded][one unsigned LEB128 per present cell, in cell order] -
\ one cursor decodes the values with no stored cell->value offset, and the two
\ header numbers are what say where they begin. The bitmap, its grouping and the
\ varint ARE the AOT-WINDOW encoding aot-capture.f writes for the metabuild seed
\ (src/habu/aot-decl.f package AOT-WINDOW, BM-COMPACT for the grouping), so the
\ blob and the baked window carry one format and one decoder shape.
\ SPARSE-CAP bounds this file's own encoding buffer and nothing else: it is
\ generous headroom over the row/byte overhead of a captured DATA span, which is
\ set by how much data the application declares and not by how long its code is.
\ A span that still overflows it dies closed by name instead of corrupting the
\ buffer.
$1E0000 constant SPARSE-CAP
create SPARSE-BUF SPARSE-CAP allot   variable SPARSE-LEN
variable AOT-CODE-N
variable AOT-PAYLOAD-OFF

: SPARSE-ROOM? ( n -- )
   SPARSE-LEN @ + SPARSE-CAP > IF
      s" aot: sparse data blob exceeds buffer" 74 die THEN ;

\ The header, and the only fixed-width field the blob has. It is written last,
\ over room reserved first, because its value is how long the rows turned out.
: SPARSE-U32! ( n n -- ) {: v:n at:n :}
   v         $FF and SPARSE-BUF at + c!
   v 8  rshift $FF and SPARSE-BUF at + 1 + c!
   v 16 rshift $FF and SPARSE-BUF at + 2 + c!
   v 24 rshift $FF and SPARSE-BUF at + 3 + c! ;

: SPARSE-BYTE! ( n -- ) {: v:n :}
   1 SPARSE-ROOM?
   v SPARSE-BUF SPARSE-LEN @ + c!
   SPARSE-LEN @ 1 + SPARSE-LEN ! ;


\ Exact byte coverage of the XT rows overlapping this grid cell. The sorted
\ cursor stays on the first overlapping row: an unaligned XT also covers bytes
\ in the next grid cell, and multiple XT rows may meet one grid cell.
: BLOB-XT-MASK ( n -- n )
   {: at:n :}
   at XTC-DECLARED? 0= IF 0 exit THEN
   0 XTC-CX @
   BEGIN dup XTC-N @ < IF dup XTC-CELL at 8 + < ELSE false THEN WHILE
      {: mask:n k:n :}
      k XTC-CELL at - {: start:n :}
      start 0 max {: lo:n :}
      start 8 + 8 min {: hi:n :}
      mask 1 hi lo - lshift 1- lo lshift or  k 1+
   REPEAT drop ;

\ ONE CELL OF [0, BLOB-LEN), read as the unsigned number its bytes spell. The
\ partial tail reads as the fresh mapping's zero bytes, like ACAP-CELL@. An XT
\ row is restored by EMIT-XT-CELLS, so its obsolete producer address contributes
\ only zero bytes here. DATA-kind cells and neighboring bytes stay intact.
variable BLOB-CV

: BLOB-CELL@ ( n -- n )
   {: c:n :}
   c AOT-WINDOW:CELL-BYTES * BLOB-SRC @ + BLOB-XT-MASK {: mask:n :}
   0 BLOB-CV !
   AOT-WINDOW:CELL-BYTES 0 ?do
      c AOT-WINDOW:CELL-BYTES * i + {: off:n :}
      off BLOB-LEN @ <  mask 1 i lshift and 0= and IF
         BLOB-SRC@ off + c@  i 8 * lshift  BLOB-CV @ or  BLOB-CV !
      THEN
   loop
   BLOB-CV @ ;

\ The bitmap is written in ascending cell order, so extending it is appending
\ zero bytes, and it stops at the highest present cell.
: BLOB-BIT! ( n -- ) {: c:n :}
   c AOT-WINDOW:CELL-BITS / SPARSE-HDR + {: at:n :}
   BEGIN SPARSE-LEN @ at <= WHILE 0 SPARSE-BYTE! REPEAT
   SPARSE-BUF at + c@
   1 c AOT-WINDOW:CELL-BITS mod lshift or
   SPARSE-BUF at + c! ;

: BLOB-V! ( n -- ) {: v:n :}
   AOT-WINDOW:VMAX SPARSE-ROOM?
   v  SPARSE-BUF SPARSE-LEN @ +  AOT-WINDOW:CELL-V! {: w:n :}
   SPARSE-LEN @ w + SPARSE-LEN ! ;

\ Two passes over the grid, because where the values start is not known until
\ the bitmap is built and grouped. The first pass leaves the FLAT bitmap in the
\ buffer behind the header, BM-COMPACT reads it there and answers the grouped
\ form, and that form is copied back over it: the flat bytes are this word's
\ working state and never reach the image.
: BUILD-SPARSE-DATA ( -- )
   BLOB-SRC @ AOT-WINDOW:CELL-BYTES mod 0<> IF
      s" aot: the captured DATA span does not start on a cell" 74 die THEN
   0 SPARSE-LEN !
   SPARSE-HDR SPARSE-ROOM?  SPARSE-HDR SPARSE-LEN !
   XTC-REWIND
   SPAN-CELLS 0 ?do
      i BLOB-CELL@ 0<> IF i BLOB-BIT! THEN
   loop
   SPARSE-BUF SPARSE-HDR +  SPARSE-LEN @ SPARSE-HDR -  AOT-WINDOW:BM-COMPACT
   AOT-WINDOW:CBM-GROUPS @  0 SPARSE-U32!
   AOT-WINDOW:CBM-STORED @  4 SPARSE-U32!
   SPARSE-HDR SPARSE-LEN !
   AOT-WINDOW:CBM-LEN 0 ?do
      AOT-WINDOW:CBM-BUF i + c@ SPARSE-BYTE!
   loop
   XTC-REWIND
   SPAN-CELLS 0 ?do
      i BLOB-CELL@ {: v:n :}
      v 0<> IF v BLOB-V! THEN
   loop ;

\ The declared xt-cell rows follow this blob immediately (LINK emits them next),
\ at the four-byte boundary BYTES, pads to. On macOS the label names the file's
\ __DATA_CONST address after its two GOT cells. The assembler advances through
\ the virtual gap for its existing label fixups; the object and image writers
\ omit that gap from their code slice.
: AOT-PAYLOAD-PLACE ( -- )
   ASM-LEN AOT-CODE-N !
   ASM-LEN CODELEN !
   TEXTSZ CODE-OFF - IMG-PAYLOAD-PREFIX @ + AOT-PAYLOAD-OFF !
   begin ASM-LEN AOT-PAYLOAD-OFF @ < while 0 EMITW repeat ;

: EMIT-DATA-BLOB ( -- )
   BLOB-LEN @ 0= IF exit THEN
   BUILD-SPARSE-DATA
   HB-TARGET-MACOS? if AOT-PAYLOAD-PLACE then
   BLOB-LBL LABEL@ LBL,
   SPARSE-BUF SPARSE-LEN @ BYTES, ;

: AOT-OBJ-CODE-N ( -- n )
   AOT-CODE-N @ 0 > if AOT-CODE-N @ else ASM-LEN then ;

: AOT-OBJ-PAYLOAD-N ( -- n )
   AOT-CODE-N @ 0 > if ASM-LEN AOT-PAYLOAD-OFF @ - else 0 then ;

create AOT-OBJ-HDR 8 allot

: AOT-WRITE-OBJ ( -- )
   AOT-OBJ-CODE-N AOT-OBJ-HDR LE:U32!
   AOT-OBJ-PAYLOAD-N AOT-OBJ-HDR 4 + LE:U32!
   AOT-OBJ PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" aot: cannot open object output" 74 die THEN
   DRV-WFD @ AOT-OBJ-HDR 8 FDIO:WALL
   DRV-WFD @ CODE AOT-OBJ-CODE-N FDIO:WALL
   AOT-OBJ-PAYLOAD-N 0 > if
      DRV-WFD @ CODE AOT-PAYLOAD-OFF @ + AOT-OBJ-PAYLOAD-N FDIO:WALL
   then
   DRV-WFD @ close ;

: AOT-FRESH-PAYLOAD! ( -- )
   AOT-OBJ-PAYLOAD-N 0 > if
      AOT-OBJ-CODE-N CODE AOT-PAYLOAD-OFF @ + AOT-OBJ-PAYLOAD-N IMG-PAYLOAD!
   then ;

\ The stripped image carries the signal stub too, and publishes it the way
\ habu2.f EM-STARTUP-RUNTIME-STATE does, with this image's own addresses.
\ IT CLEARS NO FD WORD, where the engine's boot has to: this startup's DATA is a
\ fresh MAP_ANON mapping at DATA-VA and EMIT-DATA-COPY restores only [heap mark,
\ here), which begins at DATA-START and cannot reach a header cell -- so the fd
\ word this image starts with is the mapping's own zero. A SNAPSHOT copies DATA
\ from offset zero (snap-lib.f SND-COPY), which is why the engine clears it.
\ The stub binds after the copied code and after the crash handler, so its
\ address comes from TEXT-ADR, (x12 is the scratch; nothing in this word holds
\ it).
: EMIT-SIGNAL-PUBLISH ( -- )
   11 12 LSIGH LABEL@ TEXT-ADR,  11 DATA SIGNAL-ABI:STUB-CELL STR,
   11 DATA-VA VA>N SIGNAL-ABI:FD-CELL + LIT64,  11 DATA SIGNAL-ABI:FD-PTR-CELL STR, ;

\ A stripped image runs the same three guarded VM stacks as the engine, and
\ installs the same crash handler. Both matter here and not only in the engine:
\ a stripped application has no interpreter to name an overflow, so before guard
\ pages its only diagnostic WAS the per-transfer check. Without the handler the
\ guard page would be a bare SIGSEGV; with it, the overflow is the same named
\ `hb: stack bounds exceeded (<which>)` and STACK-BOUNDS exit the engine gives.
\ The process-exit vector (src/habu/layout.f EXIT-HOOK-CELL), inline. A stripped
\ image has no engine label to call - habu2.f EMIT-EXITHOOK's trampoline is
\ engine text - so this entry and EMIT-UNCAUGHT each emit it. After the root
\ call every register but the VM's is dead, so the sequence needs neither a
\ frame nor a saved exit code; the reporter saves its code itself. THE CELL IS
\ CLEARED BEFORE THE CALL, exactly as the engine's trampoline clears it: a hook
\ that dies re-enters an exit path and must find the vector empty. An image that
\ arms nothing pays one load and a CBZ. No gate reads this sequence: the image
\ gate's walk (test/gate-aot-image.f SCAN-STARTUP) stops at the root call. Its
\ behaviour is test/stripped-image.f RUN-EXITS, one run per exit, each of which
\ must remove the directory its hook registered.
: EMIT-EXIT-HOOK ( -- )
   LBL {: nohk:label :}
   9 DATA EXIT-HOOK-CELL LDR,  9 nohk CBZ,
   10 0 MOVZ,  10 DATA EXIT-HOOK-CELL STR,
   9 BLR,
   nohk LBL, ;

: EMIT-ENTRY
   LTEXT LABEL@ LBL,                             \ text offset zero: this image's code base
   EMIT-ENTRY-ARGS                               \ argc/argv/envp into x13/x14/x15, off the untouched kernel frame
   C-SIGNAL-STACK                                \ the crash handler's stack, carved below the kernel frame (crash.f)
   STACK-ABI:BOOT-BYTES XDS STACK-GUARD:EMIT-MAP
   EMIT-DATA-REGION-MAP                          \ map DATA-VA, set x20/S0
   EMIT-OWNED-CELLS                              \ the engine runtime cells this entry owns
   STACK-ABI:RETURN-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:RETURN-BASE-CELL STR,
   STACK-ABI:LOOP-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:LOOP-BASE-CELL STR,
   \ Name a guard-page fault instead of dumping SIGSEGV. The handler follows the
   \ copied code (EMIT-CRASH-CODE below), so its address comes from TEXT-ADR,;
   \ x12 is dead here, the two mapped stack bases above travel in x10.
   11 12 LCRASHH LABEL@ TEXT-ADR,  G-INSTALL-CRASH-X11
   EMIT-DATA-COPY                                \ restore persistent data + DP
   EMIT-FFI-BASES                                \ select this image's two FFI table destinations
   EMIT-XT-CELLS                                 \ ... and the tokens its declared cells hold
   EMIT-SIGNAL-PUBLISH                           \ this image's stub address and fd word
   11 12 LUNCREP LABEL@ TEXT-ADR,  11 DATA UNCGH-CELL STR,   \ the uncaught-throw reporter (EMIT-UNCAUGHT)
   MLBL LABEL@ BL,                              \ bl <entry root> (resolved when MLBL is placed)
   EMIT-EXIT-HOOK                                \ whatever the application armed on its way out
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;               \ exit(0)

\ The handler body and its hex printer follow the copied code, so the root
\ closure record stays the first word after the startup (test/gate-aot-image.f
\ pins that). Nothing bounds how far past the startup they land: the entry
\ addresses them with TEXT-ADR,, whose LOFF, lanes span the whole code buffer.
\ DP-CHECK still uses its engine diagnostic and evaluate recovery in a native
\ process. In a stripped image its exact (LDPBAD) branch is redirected here:
\ the same candidate and ceiling are printed, then the process exits 76.
\ No compiler/evaluator state is part of this handler.
: DP-REFUSE-MSG$ ( -- ptr u8 n ) s" hb: data space out of range: DP " ;
: DP-REFUSE-OF$ ( -- ptr u8 n ) s"  of " ;
: DP-REFUSE-UNIT$ ( -- ptr u8 n ) S\"  bytes\n" ;

: EMIT-DP-REFUSAL ( -- )
   LDPREFUSE LABEL@ LBL,
   0 2 MOVZ,  1 LDPREFUSE-MSG LABEL@ ADR,  2 DP-REFUSE-MSG$ nip MOVZ,  NR-WRITE SYS,
   9 15 DATA SUB,  LDPREFUSE-NUM LABEL@ BL,
   0 2 MOVZ,  1 LDPREFUSE-OF LABEL@ ADR,  2 DP-REFUSE-OF$ nip MOVZ,  NR-WRITE SYS,
   9 DATA-SIZE PROF-CNT-BYTES - LIT64,  LDPREFUSE-NUM LABEL@ BL,
   0 2 MOVZ,  1 LDPREFUSE-UNIT LABEL@ ADR,  2 DP-REFUSE-UNIT$ nip MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS,
   LDPREFUSE-NUM LABEL@ LBL,
   LBL {: digit:label :}
   SP SP 32 SUBI,
   12 SP 32 ADDI,
   11 $A MOVZ,
   digit LBL,
      13 9 11 UDIV,  10 13 11 MUL,  10 9 10 SUB,
      10 10 $30 ADDI,  12 12 1 SUBI,  10 12 0 STRB,
      9 13 0 ADDI,  9 digit CBNZ,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,  NR-WRITE SYS,
   SP SP 32 ADDI,  RET,
   LDPREFUSE-MSG LABEL@ LBL,  DP-REFUSE-MSG$ BYTES,
   LDPREFUSE-OF LABEL@ LBL,  DP-REFUSE-OF$ BYTES,
   LDPREFUSE-UNIT LABEL@ LBL,  DP-REFUSE-UNIT$ BYTES, ;

\ A throw no handler catches reaches BTHROW's THROW-NOREC branch (habu1.f) with
\ the code in x9, and that branch jumps through UNCGH-CELL. The engine's boot
\ stores its own reporter there (habu2.f LUNCAUGHT); a stripped image has no
\ engine text, so EMIT-ENTRY stores this one, which keeps the same contract
\ (docs/debugging.md "Reading an exit status"). It runs the exit vector first,
\ as every exit does. A code from 1 to 255 is then the exit status, with no
\ output; any other code would be kernel-masked, so it is named on fd 2 as
\ `hb: uncaught throw code <n>` and the process exits UNCAUGHT-RC. The engine's
\ RC-REJECT leg has no counterpart: only the checker renders a refusal, and no
\ stripped closure can write REFUSAL-ABI:CODE-CELL, which aot-owned-cells.f
\ does not claim. The code waits out the hook on the machine stack and stays
\ in x15 across the writes, which the kernel preserves; the digits are
\ LDPREFUSE-NUM's, printed from the magnitude, so MIN-N prints exactly.
: UNCAUGHT-MSG$ ( -- ptr u8 n ) s" hb: uncaught throw code " ;

: EMIT-UNCAUGHT ( -- )
   LBL LBL {: report:label pos:label :}
   LUNCREP LABEL@ LBL,
   SP SP $10 SUBI,  9 SP 0 STR,
   EMIT-EXIT-HOOK
   15 SP 0 LDR,  SP SP $10 ADDI,
   15 1 CMPI,    C-LT report BCOND,
   15 255 CMPI,  C-GT report BCOND,
   0 15 0 ADDI,  NR-EXIT-GROUP SYS,
   report LBL,
   0 2 MOVZ,  1 LUNCREP-MSG LABEL@ ADR,  2 UNCAUGHT-MSG$ nip MOVZ,  NR-WRITE SYS,
   9 15 0 ADDI,
   15 0 CMPI,  C-GE pos BCOND,
      0 2 MOVZ,  1 LUNCREP-SIGN LABEL@ ADR,  2 1 MOVZ,  NR-WRITE SYS,
      9 SP 15 SUB,                               \ x9 = 0 - code (register 31 reads zero)
   pos LBL,
   LDPREFUSE-NUM LABEL@ BL,
   0 2 MOVZ,  1 LUNCREP-NL LABEL@ ADR,  2 1 MOVZ,  NR-WRITE SYS,
   0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS,
   LUNCREP-MSG LABEL@ LBL,  UNCAUGHT-MSG$ BYTES,
   LUNCREP-SIGN LABEL@ LBL,  s" -" BYTES,
   LUNCREP-NL LABEL@ LBL,  S\" \n" BYTES, ;

: EMIT-CRASH-CODE ( -- )
   EMIT-CRASH-HANDLER  EMIT-SIGNAL-HANDLER  EMIT-HEX ;
PTR-VARIABLE CP2  PTR-VARIABLE CEND   \ the copy walk's cursor and its one-past end
\ Whether a recorded address site starts at p: the one reader of the record
\ (src/habu/sites.f), through aot-closure.f EACH-SITE over the one byte.
variable AS-HIT
: AS-SITE ( n n -- ) {: off:n kind:n :}
   kind SNAP-RELOC:SITE-ADDR = if 1 AS-HIT ! then ;
: ADDRESS-SITE? ( ptr u8 -- bool ) {: p:ptr :}
   0 AS-HIT !
   p p 1 + [: AS-SITE ;] EACH-SITE
   AS-HIT @ 0<> ;
variable DP-REF-OFF
: BCOND? {: w:n :}  w $FF000010 and $54000000 = ;
: CBZIMM? {: w:n :}  w $7E000000 and $34000000 = ;
: TBZIMM? {: w:n :}  w $7E000000 and $36000000 = ;
: ADR? {: w:n :}  w $9F000000 and $10000000 = ;
: ADRP? {: w:n :}  w $9F000000 and $90000000 = ;
\ PLAN-BLOBS follows the pooling pass below, which uses the relocation target
\ decoders and the final DATA-TARGET mapping before it sets member lengths.
\ Relocation math for direct branches. The binary is PIE (arm64 macOS requires it),
\ so absolute targets would be wrong under the ASLR slide — instead each direct
\ branch is rewritten to a PC-RELATIVE form whose offset within __text is
\ slide-independent (no runtime relocation needed). Range checking makes linker
\ corruption fail at build time if the AOT __text ever outgrows the branch reach.
variable BDELTA
: BITS {: w:n lo:n width:n :}  w lo rshift  1 width lshift 1 - and ;
: SX {: f:n width:n :}  f 1 width 1 - lshift xor  1 width 1 - lshift - ;
: REL26 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -33554432 <  BDELTA @ 33554431 > or IF s" aot: B/BL target out of range" 74 die THEN
   BDELTA @ $3FFFFFF and ;
: REL19 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -262144 <  BDELTA @ 262143 > or IF s" aot: rel19 target out of range" 74 die THEN
   BDELTA @ $7FFFF and ;
: REL14 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -8192 <  BDELTA @ 8191 > or IF s" aot: rel14 target out of range" 74 die THEN
   BDELTA @ $3FFF and ;
\ ADR carries a BYTE delta. Its target remains inside the same member, but a
\ shortened DATA site between it and the target changes that delta.
: ADRD32 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ -1048576 <  BDELTA @ 1048575 > or IF s" aot: ADR target out of range" 74 die THEN
   BDELTA @ 3 and 29 lshift  BDELTA @ 2 rshift $7FFFF and 5 lshift or ;

\ A target at a member's end is the adjacent member's start, not this one's.
: BTGT19 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 BITS 19 SX 4 * + ;
: BTGT14 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 14 BITS 14 SX 4 * + ;
: ADRTGT {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 BITS 2 lshift  w 29 2 BITS or 21 SX + ;
\ AN ADR IS RESOLVED IN ITS OWN MEMBER OR NOT AT ALL, which is the one arm that
\ does not fall back to OLD>NEW. The only ADR a compiled body carries is a
\ quotation's code address (src/compiler/native/emit.f PUT-CODEADDR), and its
\ delta runs from the site to a LATER function of the SAME emission, because the
\ front end numbers a quotation after the function its `[:` stands in
\ (src/compiler/native/elaborate.f QOPEN-ROW, QBUILD). One dictionary record
\ covers a whole emission and a `;does` companion runs from its clause to that
\ emission's end (src/compiler/native/publish.f PUBLISH-PENDING-DOES), so a
\ member added through a record holds both ends of every ADR inside it. The only
\ narrower member is an anonymous body, and it too runs to its record's end,
\ because a tier-1 quotation reference is an ADR and not an address chain, so
\ nothing marks a function boundary for aot-closure.f BODY-END-SCAN to stop at.
\ A target in ANOTHER member is therefore a defect in the emitter or in the walk
\ and not a program to relocate, and it is named here rather than mapped through
\ OLD>NEW into a delta the copy was never asked to keep. THE LONG FORM IS NO
\ ANSWER to such a target: ADRP+ADD is two words where the ADR was one. ITS
\ TARGET IS NAMED THE WAY MAP-TARGET!'s is, and an ADR
\ is the writer that meets an UNALIGNED target: its delta is in bytes where a
\ branch target is always 4-byte aligned, and ADDRESS-OWNER answers no owner for
\ an unaligned address, so the record below it is all there is to name.
: ADR-TARGET! {: i:n p:ptr w:n :} ( n ptr u8 n -- )
   p w ADRTGT {: t:ptr :}
   i t MAP-IN-MEMBER TNEW !
   TNEW @ -1 = IF
      s" aot: ADR target outside its member site=" AETXT
      i CLO-REC@ AEREC-TXT
      s"  target=" AETXT t CODE-N AEJNUM
      s"  target-word=" AETXT t CODE-N ADDRESS-OWNER t CODE-ADDR-TXT
      10 AE1
      s" " 74 die THEN ;
\ A DIRECT BRANCH TO A DECLARATION-ONLY RECORD IS DROPPED RATHER THAN RELOCATED.
\ The target is resolved here the way the closure walk resolved it - by exact
\ record entry - so the branch this rewrites is exactly the one the walk left
\ out of the closure, and the instruction becomes a NOP. Today that record is
\ (MARK), the address-cell registrar `xt!` calls to declare the cell it just
\ stored into: a stripped image carries no reader for that table, so the store
\ stands and the declaration goes away (aot-closure.f AOT-DECLARATION? holds the
\ rule and the evidence). Every other unmapped target still dies by name in
\ MAP-TARGET!, and the drop is keyed on the target, never on the calling member.
: RELOC-DIRECT ( n ptr u8 n -- n ) {: i:n p:ptr w:n :}
   p w TARGET {: t:ptr :}
   t PTR-MARK-TARGET? IF 9 XDS -8 ENC-LDRPRE EXIT THEN
   t DECLARATION-TARGET? IF ENC-NOP EXIT THEN
   t DP-REFUSAL-TARGET? IF
      w $FC000000 and  ASM-LEN DP-REF-OFF @ REL26 or EXIT THEN
   i t MAP-TARGET!
   w $FC000000 and  ASM-LEN TNEW @ REL26 or ;
: RELOC-W32 {: i:n p:ptr w:n :} ( n ptr u8 n -- n )
   w DIRECT? IF i p w RELOC-DIRECT EXIT THEN
   w BCOND? IF
      i p w BTGT19 MAP-TARGET!
      w $FF00001F and  ASM-LEN TNEW @ REL19 5 lshift or EXIT THEN
   w CBZIMM? IF
      i p w BTGT19 MAP-TARGET!
      w $FF00001F and  ASM-LEN TNEW @ REL19 5 lshift or EXIT THEN
   w TBZIMM? IF
      i p w BTGT14 MAP-TARGET!
      w $FFF8001F and  ASM-LEN TNEW @ REL14 5 lshift or EXIT THEN
   w ADR? IF
      i p w ADR-TARGET!
      w $9F00001F and  ASM-LEN TNEW @ ADRD32 or EXIT THEN
   w ADRP? IF s" aot: ADRP relocation unsupported" 74 die THEN
   w ;

\ --- fail-closed abs-chain guard. The AOT linker contract is DIRECT-BL-ONLY: no
\ native emitter produces the absolute movz/movk/movk x16 + blr x16 call form (only
\ the gforth seed ever did, and seed output never reaches AOT closure/link). ABS-CHAIN?
\ is the minimal 4-word pattern match; the copier and relocator die loudly with a named
\ error if one is ever present, instead of silently carrying it into the shipped image.
: ABS-CHAIN? ( ptr u8 ptr u8 -- bool ) {: p:ptr e:ptr :}
   p 16 + e <= IF
      p AOT-W32@ $FFE0001F and $D2800010 =
      p 4 + AOT-W32@ $FFE0001F and $F2A00010 = and
      p 8 + AOT-W32@ $FFE0001F and $F2C00010 = and
      p 12 + AOT-W32@ $D63F0200 = and
   ELSE AOT-FALSE THEN ;
: ABS-CHAIN-JSON ( -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-ABS-CHAIN" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" reason" AEJKEY s" stripped AOT linker input carries an absolute movz/movk/movk/blr call chain; the contract is direct-BL only" AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" rebuild the AOT input with the native engine (every call a direct BL); the absolute call-chain form is not linkable" AEJSTR
   125 AE1 10 AE1 ;
: ABS-CHAIN-PROSE ( -- )
   s" hb-build: stripped AOT linker input carries an absolute movz/movk/movk/blr call chain (direct-BL only)" AETXT 10 AE1 ;
: ABS-CHAIN-DIE ( -- )
   JSON-DIAGS @ IF ABS-CHAIN-JSON ELSE ABS-CHAIN-PROSE THEN
   s" aot: absolute call chain in linker input" 74 die ;

\ Replace a code-address literal without changing the blob's size. ELF and
\ Mach-O place CODE at a 4 KiB-aligned VA; ADRP's signed 21-bit page delta plus
\ ADDI's low twelve bits remains valid when the entire image moves.
: EMIT-CODE-ADDRESS ( n n -- ) {: target:n rd:n :}
   rd 31 = if s" aot: code address cannot use the zero register" 74 die then
   CODE-OFF $FFF and 0<> if s" aot: code origin is not page aligned" 74 die then
   target 12 rshift ASM-LEN 12 rshift - {: pages:n :}
   pages -1048576 < pages 1048575 > or if s" aot: code address is outside ADRP reach" 74 die then
   \ ADRP Xd: immlo[30:29], immhi[23:5], Rd[4:0].
   $90000000 pages 3 and 29 lshift or
   pages 2 rshift $7FFFF and 5 lshift or rd or EMITW
   rd rd target $FFF and ADDI,
   NOP, NOP, ;


: XT-ROW-FIELD ( n -- n ) {: v:n :}
   v 0 < v $FFFFFFFF > or IF s" aot: a declared xt cell row field is not a u32" 74 die THEN
   v ;

\ One row per declared cell, (location u32, target u32) little-endian, which is
\ one DCQ, of target<<32 or location.
: EMIT-XT-ROWS ( -- )
   XTC-N @ 0 ?do
      i XTC-OFF@ XT-ROW-FIELD
      i XT-CELL-TARGET XT-ROW-FIELD 32 lshift or DCQ,
   loop ;

\ Inventory only the source region's recorded sites. DATA-TARGET is the final
\ answer after the copied window, carried claims and literal pool have settled;
\ grouping an earlier compiler address would merge independently mapped cells.
: POOL-COUNT ( -- )
   0 PN !
   NCLO @ 0 ?do
      i CLO-AT CP2 !  i CLO-AT i CLO-BYTES + CEND !
      BEGIN CP2 @ CEND @ < WHILE
         CP2 @ ADDRESS-SITE? IF
            CP2 @ CEND @ ADDRESS-VALUE {: value:n :}
            CP2 @ CEND @ ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
            size ADDRESS-CARRIER:DATA-CHAIN-BYTES =
               value DATA-ADDRESS? and IF 1 PN +! THEN
            size
         ELSE 4 THEN CP2 @ + CP2 !
      REPEAT
   loop ;

: GROUP-FIND ( n -- n ) {: target:n :}
   GN @ 0 ?do i GTARGET @ target = IF i unloop EXIT THEN loop
   -1 ;

variable ROW-GROUP
: POOL-ROW ( n ptr u8 n -- ) {: m:n p:ptr value:n :}
   m CLO-REC@ p value DATA-TARGET {: target:n :}
   target GROUP-FIND ROW-GROUP !
   ROW-GROUP @ -1 = IF
      GN @ {: fresh:n :}
      target fresh GTARGET !  0 fresh GSAVE !  0 fresh GON !
      fresh ROW-GROUP !
      1 GN +!
   THEN
   p PN @ PSITE !
   0 PN @ PMODE !  0 PN @ PHOLD !  0 PN @ PCUM !
   ROW-GROUP @ PN @ PGROUP !
   1 PN +! ;

: POOL-FILL ( -- )
   0 PN !  0 GN !
   NCLO @ 0 ?do
      PN @ i PSTART !
      i CLO-AT CP2 !  i CLO-AT i CLO-BYTES + CEND !
      BEGIN CP2 @ CEND @ < WHILE
         CP2 @ ADDRESS-SITE? IF
            CP2 @ CEND @ ADDRESS-VALUE {: value:n :}
            CP2 @ CEND @ ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
            size ADDRESS-CARRIER:DATA-CHAIN-BYTES =
            value DATA-ADDRESS? and IF i CP2 @ value POOL-ROW THEN
            size
         ELSE 4 THEN CP2 @ + CP2 !
      REPEAT
      PN @ i PEND !
   loop ;

\ A code target inside a site being shortened would name one of its deleted
\ words. Preserve that site's original carrier. Exact source-site starts remain
\ valid targets; bytes at or after its end map through the cumulative removal.
: POOL-HOLD-TARGET ( ptr u8 -- ) {: target:ptr :}
   target MORD-BELOW {: pos:n :}
   pos 0 < IF EXIT THEN
   pos MORD-AT {: m:n :}
   target m CLO-AT m CLO-BYTES + >= IF EXIT THEN
   m PEND @ m PSTART @ ?do
      target i PSITE @ >
      target i PSITE @ ADDRESS-CARRIER:DATA-CHAIN-BYTES + < and IF
         1 i PHOLD !  unloop EXIT
      THEN
   loop ;

: POOL-HOLD-SOURCE ( -- )
   NCLO @ 0 ?do
      i CLO-AT CP2 !  i CLO-AT i CLO-BYTES + CEND !
      BEGIN CP2 @ CEND @ < WHILE
         CP2 @ ADDRESS-SITE? IF
            CP2 @ CEND @ ADDRESS-VALUE {: value:n :}
            value DATA-ADDRESS? 0= IF value CODE-PTR POOL-HOLD-TARGET THEN
            CP2 @ CEND @ ADDRESS-CARRIER:CHAIN-SIZE
         ELSE
            CP2 @ AOT-W32@ {: w:n :}
            w DIRECT? IF CP2 @ w TARGET POOL-HOLD-TARGET THEN
            w BCOND? w CBZIMM? or IF CP2 @ w BTGT19 POOL-HOLD-TARGET THEN
            w TBZIMM? IF CP2 @ w BTGT14 POOL-HOLD-TARGET THEN
            w ADR? IF CP2 @ w ADRTGT POOL-HOLD-TARGET THEN
            4
         THEN CP2 @ + CP2 !
      REPEAT
   loop
   XTC-N @ 0 ?do i XTC-VAL@ CODE-PTR POOL-HOLD-TARGET loop ;

variable POOL-WORST-END
variable POOL-ORIG-OFF
variable POOL-SAVED
variable POOL-CUM

\ Classify a load against the unshortened member sequence and the largest
\ possible pool, including its branch and alignment. Shortening any site can
\ only bring that pool closer to a site left in the image.
: POOL-FORMS ( -- )
   ASM-LEN POOL-WORST-END !
   NCLO @ 0 ?do i CLO-BYTES POOL-WORST-END +! loop
   11 GN @ 8 * + POOL-WORST-END +!
   ASM-LEN POOL-ORIG-OFF !
   NCLO @ 0 ?do
      i PEND @ i PSTART @ ?do
         i PSITE @ AOT-W32@ ADDRESS-CARRIER:ADDR-RD-MASK and 31 <>
         i PHOLD @ 0= and IF
            POOL-WORST-END @ POOL-ORIG-OFF @
               i PSITE @ j CLO-AT - + -
            1048572 <= IF 1 ELSE 2 THEN
            dup i PMODE !
            1 = IF 8 ELSE 4 THEN
            i PGROUP @ {: group:n :}
            group GSAVE @ + group GSAVE !
         THEN
      loop
      i CLO-BYTES POOL-ORIG-OFF +!
   loop ;

: POOL-SELECT ( -- )
   0 POOL-SAVED !  0 POOL-N !
   GN @ 0 ?do
      i GSAVE @ 8 > IF
         1 i GON !
         i GSAVE @ 8 - POOL-SAVED +!
         1 POOL-N +!
      THEN
   loop
   \ At most four branch bytes and seven alignment bytes frame one pool.
   POOL-SAVED @ 11 <= IF
      GN @ 0 ?do 0 i GON ! loop
      0 POOL-N !
   THEN
   NCLO @ 0 ?do
      0 POOL-CUM !
      i PEND @ i PSTART @ ?do
         i PGROUP @ GON @ 0= IF 0 i PMODE ! THEN
         i PMODE @ dup 1 = IF drop 8 POOL-CUM +! ELSE
            2 = IF 4 POOL-CUM +! THEN
         THEN
         POOL-CUM @ i PCUM !
      loop
   loop ;

: POOL-PLAN ( -- )
   MEMBER-ORDER
   POOL-COUNT
   PN @ POOL-TABLES
   POOL-FILL
   PN @ 0 > IF POOL-HOLD-SOURCE POOL-FORMS POOL-SELECT ELSE 0 POOL-N ! THEN ;

variable POOL-CELL-OFF
: PLAN-BLOBS ( -- )
   POOL-PLAN
   NCLO @ PLAN-TABLES
   ASM-LEN NEXT-OFF !
   NCLO @ 0 ?do
      NEXT-OFF @ i NEWOFF !
      i CLO-BYTES {: bytes:n :}
      i PSTART @ i PEND @ < IF bytes i PEND @ 1- PCUM @ - ELSE bytes THEN
         dup i BLEN ! NEXT-OFF +!
   loop
   NEXT-OFF @ DP-REF-OFF !
   POOL-N @ 0 > IF
      NEXT-OFF @ 11 + 8 / 8 * POOL-OFF !
      POOL-OFF @ POOL-N @ 8 * + DP-REF-OFF !
      POOL-OFF @ POOL-CELL-OFF !
      GN @ 0 ?do
         i GON @ 0<> IF
            POOL-CELL-OFF @ i GOFF !
            8 POOL-CELL-OFF +!
         THEN
      loop
   THEN ;

: EMIT-POOL-LOAD ( n -- ) {: row:n :}
   row PSITE @ AOT-W32@ ADDRESS-CARRIER:ADDR-RD-MASK and {: rd:n :}
   row PGROUP @ GOFF @ {: at:n :}
   row PMODE @ 1 = IF
      at ASM-LEN - {: delta:n :}
      delta 3 and 0<> delta -1048576 < or delta 1048572 > or IF
         s" aot: pooled literal out of range" 74 die
      THEN
      $58000000 delta 4 / $7FFFF and 5 lshift or rd or EMITW
      EXIT
   THEN
   at 12 rshift ASM-LEN 12 rshift - {: pages:n :}
   pages -1048576 < pages 1048575 > or IF
      s" aot: pooled page out of range" 74 die
   THEN
   $90000000 pages 3 and 29 lshift or
   pages 2 rshift $7FFFF and 5 lshift or rd or EMITW
   rd rd at $FFF and LDR, ;

: EMIT-POOL ( -- )
   POOL-N @ 0= IF EXIT THEN
   LBL {: after:label :}
   after B,
   BEGIN ASM-LEN 7 and 0<> WHILE NOP, REPEAT
   GN @ 0 ?do i GON @ 0<> IF i GTARGET @ DCQ, THEN loop
   after LBL,
   ASM-LEN DP-REF-OFF @ <> IF s" aot: pool layout changed" 74 die THEN ;

: COPY-ADDRESS ( ptr n ptr u8 ptr u8 -- ) {: owner:ptr p:ptr e:ptr :}
   p e ADDRESS-VALUE {: v:n :}
   v DATA-ADDRESS? if
      owner p v DATA-TARGET {: target:n :}
      p e ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
      size 4 / {: halves:n :}
      \ Preserve geometry and scaffold; DATA halves run high to low.
      halves 0 ?do
         p i 4 * + AOT-W32@ ADDRESS-CARRIER:ADDR-OPC-MASK and
         size ADDRESS-CARRIER:DATA-CHAIN-BYTES = if 2 i - else i then
         16 * target swap rshift $FFFF and 5 lshift or EMITW
      loop
      exit
   then
   v CODE-PTR OLD>NEW dup 0 < if
      s" aot: code address is outside the closure" 74 die
   then
   p AOT-W32@ ADDRESS-CARRIER:ADDR-RD-MASK and EMIT-CODE-ADDRESS ;

variable PCOPY
: COPY-COMPACT-BLOB {: i:n :} ( n -- )
   i CLO-AT CP2 !  i CLO-AT i CLO-BYTES + CEND !
   i PSTART @ PCOPY !
   BEGIN CP2 @ CEND @ < WHILE
      CP2 @ CEND @ ABS-CHAIN? IF ABS-CHAIN-DIE THEN
      CP2 @ ADDRESS-SITE? if
         PCOPY @ i PEND @ < IF
            CP2 @ PCOPY @ PSITE @ = IF
               PCOPY @ PMODE @ 0 > IF
                  PCOPY @ EMIT-POOL-LOAD
               ELSE i CLO-REC@ CP2 @ CEND @ COPY-ADDRESS THEN
               1 PCOPY +!
            ELSE i CLO-REC@ CP2 @ CEND @ COPY-ADDRESS THEN
         ELSE i CLO-REC@ CP2 @ CEND @ COPY-ADDRESS THEN
         CP2 @ CEND @ ADDRESS-CARRIER:CHAIN-SIZE
      else
         i CP2 @ CP2 @ AOT-W32@ RELOC-W32 EMITW 4
      then CP2 @ + CP2 !
   REPEAT ;
: COPY-PLANNED-BLOBS
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ 0= IF MLBL LABEL@ LBL, THEN          \ MAIN is closure word 0 -> place its label
      WI @ COPY-COMPACT-BLOB
      ASM-LEN WI @ MEMBER-NEWOFF WI @ BLEN @ + <> IF
         s" aot: planned member " AETXT WI @ AEJNUM
         s"  ends at=" AETXT WI @ MEMBER-NEWOFF WI @ BLEN @ + AEJNUM
         s"  actual=" AETXT ASM-LEN AEJNUM 10 AE1
         s" aot: planned member length differs from copy" 74 die THEN
      WI @ 1+ WI ! REPEAT ;
: COPY-BLOBS  PLAN-BLOBS  COPY-PLANNED-BLOBS  EMIT-POOL ;
variable RP  variable RE
\ Fail-closed output verification. COPY-COMPACT-BLOB (via RELOC-W32) already relocated
\ every direct branch as it emitted the blobs, so under the direct-BL-only contract
\ there is nothing left to relocate. This second pass re-walks the emitted __text and
\ dies with the named error if any absolute call chain survived into the shipped image.
: RELOCATE
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ NEWOFF @ RP !
      RP @ WI @ BLEN @ + RE !
      BEGIN RP @ RE @ < WHILE
         CODE RP @ + CODE RE @ + ABS-CHAIN? IF ABS-CHAIN-DIE THEN
         RP @ 4 + RP !
      REPEAT
      WI @ 1+ WI ! REPEAT ;

\ The span is already latched: tools/aot-build-open.f calls AOT-DATA-SPAN the moment
\ the application has loaded, because this file - and every lib module the linker
\ needs - is loaded AFTER that and allots above BLOB-END. LINK therefore reads the
\ bounds it is given and never latches them itself; latching here would put the
\ whole linker inside the span.
: LINK-ARM ( -- )
   0 AOT-CODE-N !  0 AOT-PAYLOAD-OFF !
   ASM-INIT  LBL MLBL !  LBL BLOB-LBL !  LBL LTEXT !
   LBL LCRASHH !  LBL LSIGH !  LBL LHEX !  LBL LHDR !
   LBL LDPREFUSE !  LBL LDPREFUSE-NUM !
   LBL LDPREFUSE-MSG !  LBL LDPREFUSE-OF !  LBL LDPREFUSE-UNIT !
   LBL LUNCREP !  LBL LUNCREP-MSG !  LBL LUNCREP-SIGN !  LBL LUNCREP-NL !
   EMIT-ENTRY  COPY-BLOBS  RELOCATE  EMIT-DP-REFUSAL  EMIT-UNCAUGHT  EMIT-CRASH-CODE
   ASM-LEN CODE-END !
   EMIT-DATA-BLOB  EMIT-XT-ROWS
   AOT-WRITE-OBJ
   AOT-FRESH-PAYLOAD!
   SIGN-ID:PROG$ AOT-OUT DRV-EMIT-IMAGE ;

: INSTALL-ARM ( -- ) [: LINK-ARM ;] is LINK-TARGET ;
INSTALL-ARM

;using
;using
;package
