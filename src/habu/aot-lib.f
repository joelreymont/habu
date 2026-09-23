\ aot-lib.f - stripped AOT linker words. Load after src/habu/aot-closure.f.
\
\ tools/aot-build.f loads the application through the retained native compiler.
\ LINK writes the selected entry's reachable closure into a standalone image,
\ relocates its code and emits the minimal runtime entry. tools/hb-build.f owns
\ the output paths; the default application entry is MAIN.
require src/habu/stack-abi.f
require src/habu/rt.f
require src/habu/crash.f
require src/habu/aot-decl.f
require src/habu/aot-window-latch.f
require src/habu/aot-owned-cells.f

\ The AOT relocation core compiles checked. It works over CLOSURE MEMBERS - a
\ code entry and a length, held in the parallel arrays src/habu/aot-closure.f
\ fills - and not over dictionary records, because the image ships no record for
\ a word nothing can name and the payload's span table is what accounts for one.
\ A member is named by its index, so every read here is a cell of an owned array
\ and the record-shaped TRUSTED: boundary this file used to carry is gone.

package AOT-LINK
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM

: AOT-OUT  s" hb-aot-got" TMP-PATH ;
: AOT-OBJ ( -- ptr u8 n )  s" hb-aot-obj" TMP-PATH ;

: AOT-FALSE ( -- bool ) 0 0= 0= ;

\ --- emit the image: minimal entry + compacted, relocated blobs.
variable MLBL
\ The label at text offset zero: the image's own code base, which the declared
\ xt cells are restored against. It costs no bytes and is placed by EMIT-ENTRY.
variable LTEXT

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
\ tools/hb-build-test.f HBT-STRIPPED-CHAIN runs the converted startup end to end
\ at 1,104 members, and Tender's standalone build is the proof at the scale that
\ needed it.
: TEXT-ADR, ( n n label -- )                      \ ( rd rt label -- ) rd = runtime address of label
   {: rd:n rt:n l:label :}
   rd rt = IF s" aot: TEXT-ADR, destination and scratch are one register" 74 die THEN
   rd l LOFF,                                     \ rd = the label's byte offset from text offset zero
   rt LTEXT LABEL@ ADR,                           \ rt = this image's code base
   rd rt rd ADD, ;

\ The planning columns, parallel to aot-closure.f's closure tables: where each
\ member lands in the compacted __text and how many bytes were planned for it.
\ Both hold counts. PLAN-BLOBS allocates them for the members the walk actually
\ found (NCLO), which is why they carry no capacity of their own.
DYNAMIC-BUFFER NEWOFF n   \ each member's offset in the emitted __text
DYNAMIC-BUFFER BLEN n     \ ... and the bytes planned for it
: PLAN-TABLES ( n -- ) {: rows:n :}
   rows 1 < IF s" aot: no closure members to plan" 74 die THEN
   rows NEWOFF-RESERVE  rows BLEN-RESERVE ;

\ --- the persistent data region this file emits is the span
\ src/habu/aot-window-latch.f latched: AOT-DATA-START and AOT-DATA-SPAN live there
\ because the window must open before the application's `require` runs, which is
\ long before this file is loaded. The scan cursor below is this file's own, and
\ it is a DATA address as an integer like the bounds it walks between - nothing
\ dereferences it, the one cell it reads goes through aot-closure.f DATA-CELL@,
\ which is where a DATA address becomes a pointer again.
variable DSCAN

\ THE CARRIED CELLS, COPIED INTO THE WINDOW BEFORE ANYTHING READS OR MAPS IT.
\ Each claim named CARRIED in src/habu/aot-owned-cells.f is copied, by its
\ declared byte length, into the run aot-window-latch.f CARRY-RESERVE reserved
\ inside the span, and the destination is recorded so aot-closure.f
\ CARRIED-TARGET can map a spelled address to the copy at the same interior
\ offset. Runs are placed in claim order and cell-aligned, because a carried table
\ is read with `@` (sha256's KK is 64 cells); the copies go in BEFORE
\ AOT-DATA-TEXTPTR-CHECK and the closure walk, so carried bytes face the same
\ window checks the application's own data does and no pass can meet a
\ half-carried window.
variable CARRY-USED

: CARRY-CELL ( n -- ) {: i:n :}
   i AOT-OWNED:LEN {: len:n :}
   \ The source is read through DATA-PTR, whose every caller bounds-checks the
   \ address it hands over: a claim's declared length is the only number here
   \ the linker did not compute itself.
   i AOT-OWNED:AT DATA-ADDRESS? i AOT-OWNED:AT len + DATA-ADDRESS? and 0= IF
      s" aot: a carried claim reaches outside the DATA mapping" 74 die THEN
   \ A CARRIED CLAIM IS ENGINE DATA, which is all below the window: the image
   \ restores the window itself, so a claim reaching into it would be copied into
   \ the run and never consulted - aot-closure.f MAPPED-DATA answers an in-window
   \ address with itself before it ever asks a claim - and the same bytes would
   \ ship twice. The bound above admits the whole mapping and cannot see this.
   i AOT-OWNED:AT len + BLOB-SRC @ > IF
      s" aot: a carried claim reaches into the capture window" 74 die THEN
   CARRY-USED @ len + CARRY-BYTES > IF
      s" aot: carried engine cells exceed the window's carried run" 74 die THEN
   i AOT-OWNED:AT DATA-PTR  CARRY-BASE$ CARRY-USED @ +  len  BYTE-COPY
   CARRY-BASE @ CARRY-USED @ +  i AOT-OWNED:DEST!
   CARRY-USED @ len + 7 + 8 / 8 *  CARRY-USED ! ;

: CARRY-CELLS ( -- )
   0 CARRY-USED !
   AOT-OWNED:N 0 ?do
      i AOT-OWNED:CARRIED? IF i CARRY-CELL THEN
   loop ;

\ Every cell the capture window covers that NOTHING DECLARED, classified by the
\ ONE predicate aot-closure.f publishes (CELL-TEXTPTR?, by live engine extents).
\ A declared xt cell is relocated instead (COLLECT-XT-CELLS above it, EMIT-XT-ROWS
\ below), and it is skipped here by its declaration and not by its value, so the
\ two answers cannot disagree about one cell. A STRING LITERAL'S BODY is the other
\ declaration the window carries: its bytes are characters by the row the compiler
\ wrote when it placed them (aot-closure.f CELL-LITERAL?), so they are skipped
\ here for the same reason and by the same rule. What is left is a code or
\ dictionary pointer no declaration accounts for - a `' word ,` table - and the
\ scan reports the first one with the three facts a program has to be edited by:
\ its owning word, its DATA offset and the pointer it holds.
\ RAW AND TYPED STORAGE BOTH STAY UNDER THE VALUE SCAN. Inside a definition the
\ checker refuses a pointer or a token in either kind of cell, but the top level
\ is not certified: `variable V ' FOO V !` and `TYPED-VARIABLE V n ' FOO V !`
\ both bake a code address (measured on this engine), so no storage declaration
\ says what a baked cell holds until the top level checks typed stores. A
\ literal body has no such door: nothing stores into it.
\ ONE CELL, JUDGED BY WHAT DECLARED ITS BYTES. The value decides only WHICH
\ refusal a cell would draw; whether it draws one at all is the declaration's
\ answer, so the second declaration - the compiler's literal rows
\ (aot-closure.f CELL-LITERAL?) - is asked before either refusal and not after.
\ It is asked SECOND because the row walk costs a pass over the pools while the
\ two extent tests are arithmetic: a cell whose value is neither a live engine
\ extent nor a mapping is data on any reading and needs no declaration to say so.
: SCAN-DATA-CELL ( n -- ) {: at:n :}
   at DATA-CELL@ {: v:n :}
   v CELL-TEXTPTR? v CELL-MAPPED? or 0= IF exit THEN
   at CELL-LITERAL? IF exit THEN
   v CELL-TEXTPTR? IF at v REFUSE-UNDECLARED-CELL THEN
   v CELL-MAPPED? IF at v REFUSE-MAPPED-CELL THEN ;

: AOT-DATA-TEXTPTR-CHECK ( -- )
   XTC-REWIND
   BLOB-SRC @ DSCAN !
   BEGIN DSCAN @ 8 + BLOB-END @ <= WHILE
      DSCAN @ XTC-DECLARED? 0= IF DSCAN @ SCAN-DATA-CELL THEN
      DSCAN @ 8 + DSCAN !
   REPEAT ;

: EMIT-DATA-REGION-MAP ( -- )
   LBL {: dvok:label :}
   0 DATA-VA VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
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
\ EMIT-OWNED-CELLS touches x13-x15 - STACK-GUARD:EMIT-MAP works in x0-x6/x9/x10,
\ and a Linux syscall returns in x0 and preserves the rest.
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
: OWNED-PUBLISHED? ( n -- bool ) {: k:n :}
   k AOT-OWNED:IMAGE-BASE?  k AOT-OWNED:ENTRY-XT? or  k AOT-OWNED:TEXT-BASE? or ;
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

\ The span in whole cells: the grid the bitmap covers. The last cell may reach
\ above the latched span end, and the writer reads those bytes as the zeros they
\ are, so the image's own DP is this rounded end and nothing is ever stored
\ above it.
: SPAN-CELLS ( -- n )
   BLOB-LEN @ AOT-WINDOW:CELL-BYTES 1- + AOT-WINDOW:CELL-BYTES / ;

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
\ The copy above restores every cell's capture-time BYTES, and for a cell declared
\ to hold an execution token those bytes are the BUILDER's code address. The rows
\ EMIT-XT-ROWS places after the blob say where such a cell is and where its word
\ ended up in THIS image, in the shape src/habu/aot-file.f's XTOFF rows carry: a
\ u32 location and a u32 target in one eight-byte row.
\ THE ROWS NEED NO ADDRESS OF THEIR OWN. The copy loop's byte cursor x10 ends one
\ past the blob's last payload byte, which is where the rows begin (BYTES, pads
\ the blob to the four-byte boundary they sit on), so the startup adds no second
\ address-of-the-blob sequence ahead of the root call: test/gate-aot-image.f
\ admits exactly one of them in the startup and ends the reported code range at
\ the blob, and rows placed past the blob are outside that range rather than read
\ as instructions.
\ The cell is x20-relative (DATA is mapped MAP_FIXED, so its offset is the same
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

\ BLOB-SRC is a plain address cell; pin its byte-pointer role once here so
\ every scan/copy site below reads it as a span, not a bare number.
: BLOB-SRC@ ( -- ptr u8 ) BLOB-SRC @ DATA-PTR ;

\ ONE CELL OF [0, BLOB-LEN), read as the unsigned number its bytes spell. Mirrors
\ aot-capture.f ACAP-CELL@, rounding included: the last cell of the grid may
\ reach above the latched span, and those bytes read as the zeros the fresh
\ mapping leaves there. The stripped span has no declared address cells to
\ exclude - EMIT-XT-CELLS rewrites each one after the copy - so every cell of the
\ grid is offered.
variable BLOB-CV

: BLOB-CELL@ ( n -- n ) {: c:n :}
   0 BLOB-CV !
   AOT-WINDOW:CELL-BYTES 0 ?do
      c AOT-WINDOW:CELL-BYTES * i + {: off:n :}
      off BLOB-LEN @ < IF
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
   SPAN-CELLS 0 ?do
      i BLOB-CELL@ {: v:n :}
      v 0<> IF v BLOB-V! THEN
   loop ;

\ The declared xt-cell rows follow this blob immediately (LINK emits them next),
\ at the four-byte boundary BYTES, pads to.
: EMIT-DATA-BLOB ( -- )                            \ place the sparse blob after all code
   BLOB-LEN @ 0= IF exit THEN
   BUILD-SPARSE-DATA
   BLOB-LBL LABEL@ LBL,
   SPARSE-BUF SPARSE-LEN @ BYTES, ;

: AOT-WRITE-OBJ ( -- )
   AOT-OBJ PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" aot: cannot open object output" 74 die THEN
   DRV-WFD @ CODE ASM-LEN FDIO:WALL
   DRV-WFD @ close ;

\ --- preseeded test entry: raw physical value-stack cells materialized before
\ the selected root is called. A preseeded bad-tag object/AOT entry
\ (tools/hb-build.f --preseed-entry) pushes a forged bundle (payload slots + an
\ out-of-range tag) so the matched helper reaches its inline invalid-tag die
\ (docs/type-families.md §25.5). SEED-CELLS is
\ bottom-of-stack first, tag last (top). Empty for a normal MAIN build.
64 constant SEED-MAX
create SEED-CELLS SEED-MAX cells allot   variable SEED-N
0 SEED-N !
: SEED-RESET ( -- )  0 SEED-N ! ;
: SEED+ ( n -- )
   SEED-N @ SEED-MAX >= IF s" aot: too many preseed cells" 74 die THEN
   SEED-CELLS SEED-N @ cells + !  SEED-N @ 1 + SEED-N ! ;
: EMIT-SEED ( -- )                               \ push SEED-CELLS onto the value stack (x19)
   SEED-N @ STACK-ABI:BOOT-BYTES 8 / > if s" aot: initial data stack exceeds allocation" 74 die then
   0 BEGIN dup SEED-N @ < WHILE
      9 over cells SEED-CELLS + @ LIT64,          \ x9 = seed cell value
      9 XDS 0 STR,                                \ *x19 = x9  (value-stack top)
      XDS XDS 8 ADDI,                             \ x19 += 8  (advance one cell)
      1 +
   REPEAT drop ;

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
\ engine text, and this entry is the whole of this image's exit path - and after
\ the root call every register but the VM's is dead, so the sequence needs
\ neither a frame nor a saved exit code. THE CELL IS CLEARED BEFORE THE CALL,
\ exactly as the engine's trampoline clears it: a hook that dies re-enters an
\ exit path and must find the vector empty. An image that arms nothing pays one
\ load and a CBZ. The image gate reads this sequence instruction by instruction
\ between the root call and the exit tail (test/gate-aot-image.f EXIT-HOOK-END).
: EMIT-EXIT-HOOK ( -- )
   LBL {: nohk:label :}
   9 DATA EXIT-HOOK-CELL LDR,  9 nohk CBZ,
   10 0 MOVZ,  10 DATA EXIT-HOOK-CELL STR,
   9 BLR,
   nohk LBL, ;

: EMIT-ENTRY
   LTEXT LABEL@ LBL,                             \ text offset zero: this image's code base
   EMIT-ENTRY-ARGS                               \ argc/argv/envp into x13/x14/x15, off the untouched kernel frame
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
   EMIT-XT-CELLS                                 \ ... and the tokens its declared cells hold
   EMIT-SIGNAL-PUBLISH                           \ this image's stub address and fd word
   EMIT-SEED                                     \ push preseeded value-stack cells (empty for MAIN)
   MLBL LABEL@ BL,                              \ bl <entry root> (resolved when MLBL is placed)
   EMIT-EXIT-HOOK                                \ whatever the application armed on its way out
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;               \ exit(0)

\ The handler body and its hex printer follow the copied code, so the root
\ closure record stays the first word after the startup (test/gate-aot-image.f
\ pins that). Nothing bounds how far past the startup they land: the entry
\ addresses them with TEXT-ADR,, whose LOFF, lanes span the whole code buffer.
: EMIT-CRASH-CODE ( -- )
   EMIT-CRASH-HANDLER  EMIT-SIGNAL-HANDLER  EMIT-HEX ;
PTR-VARIABLE CP2  PTR-VARIABLE CEND   \ the copy walk's cursor and its one-past end
variable NEXT-OFF
\ --- THE MEMBERS IN ENTRY ORDER, the third per-member column. The closure walk
\ discovers members in CALL order (aot-closure.f ADD-CLO appends what it reaches),
\ so the entries in CLO are unordered, and the two lookups below - the exact-entry
\ one and the one that finds the member covering an address - scanned the whole
\ closure per relocated instruction, which made the link quadratic in NCLO. This
\ index is filled once per link, beside NEWOFF and BLEN and for the same NCLO
\ rows, and both lookups binary-search it.
\ THE MEMBERS ARE DISJOINT BY CONSTRUCTION, which is what lets ONE position
\ answer for an address. Two anonymous bodies of one record are the only members
\ that can nest - under tier 1 the lower one runs to the record's end and holds
\ the higher one's code - and aot-closure.f DROP-NESTED-CLO drops the contained
\ one after the walk, at the source of the overlap and with the offset arithmetic
\ that makes the container answer for it; the closure this file plans never
\ overlaps. MORD-DISJOINT is the INVARIANT CHECK on that, run once per link over
\ the sorted order - each member's end at or below the next one's start - and it
\ names both members' records, entries and lengths rather than picking one of
\ them, the way the scans it replaced did (they answered the LOWEST index
\ covering a target, so the discovery order decided). Adjacency is admitted and
\ is what MAP-IN-MEMBER's >= boundary is about: a target at a member's end
\ belongs to the next member. A WALK CANNOT REACH THE REFUSAL, so what reaches it
\ is a table filled by hand - test/gate-aot-negative-lib.f fills two rows, the
\ second inside the first, and expects exit 74 with the named line. Over real
\ links: zero overlapping pairs and 27,000 touching ones over the sorted order of
\ the 27,004-member chain this file's head measures, and every link
\ tools/hb-build-test.f and the gate's AOT suites make is green with the check in
\ place.
DYNAMIC-BUFFER MORD n         \ member indices, ascending by CLO-AT
variable MOI                                  \ the fill and disjointness cursor
variable MSR  variable MSC  variable MSN      \ a sift's root, child and heap size
variable MHI  variable MHE                    \ the heapify and extract cursors
variable MBLO  variable MBHI  variable MBMID  \ a search's window and its probe
variable MBP                                  \ ... and the best position it saw
: MORD-AT ( n -- n ) MORD @ ;                 \ the member at order position p
: MORD-KEY ( n -- ptr u8 ) MORD-AT CLO-AT ;   \ ... and that member's entry
: MORD-SWAP ( n n -- ) {: a:n b:n :}
   a MORD-AT {: x:n :}
   b MORD-AT a MORD !  x b MORD ! ;
\ Heapsort: one column, no scratch, no recursion, O(N log N) whatever the walk
\ order is. THE WALK ORDER IS NOT NEARLY SORTED, and on a chain it is the reverse
\ - the worst case an insertion sort has. Measured over the closures this file
\ links: a 27,004-member chain of words each calling the previous descends at
\ 27,001 of its 27,003 adjacent index pairs (the walk enters the chain at its
\ tail and works back), and a nine-member library program at 6 of 8. An insertion
\ sort would be the quadratic this pass exists to remove.
: MORD-CHILD ( -- )           \ MSC = the larger of the root's two children
   MSC @ 1+ MSN @ < IF
      MSC @ MORD-KEY  MSC @ 1+ MORD-KEY  < IF MSC @ 1+ MSC ! THEN THEN ;
: MORD-SIFT ( n n -- ) {: root:n rows:n :}
   root MSR !  rows MSN !
   BEGIN MSR @ 2 * 1+ MSN @ < WHILE
      MSR @ 2 * 1+ MSC !
      MORD-CHILD
      MSR @ MORD-KEY  MSC @ MORD-KEY  < 0= IF EXIT THEN
      MSR @ MSC @ MORD-SWAP
      MSC @ MSR !
   REPEAT ;
: MORD-SORT ( n -- ) {: rows:n :}
   rows 2 / 1- MHI !
   BEGIN MHI @ 0 >= WHILE  MHI @ rows MORD-SIFT  MHI @ 1- MHI !  REPEAT
   rows 1- MHE !
   BEGIN MHE @ 0 > WHILE
      0 MHE @ MORD-SWAP
      0 MHE @ MORD-SIFT
      MHE @ 1- MHE !  REPEAT ;
: MORD-OVERLAP-DIE ( n n -- ) {: a:n b:n :}
   s" aot: closure members overlap site=" AETXT
   a CLO-REC@ AEREC-TXT
   s"  at=" AETXT a CLO-AT CODE-N AEJNUM
   s"  bytes=" AETXT a CLO-BYTES AEJNUM
   s"  and=" AETXT b CLO-REC@ AEREC-TXT
   s"  at=" AETXT b CLO-AT CODE-N AEJNUM
   s"  bytes=" AETXT b CLO-BYTES AEJNUM
   10 AE1
   s" aot: closure members overlap" 74 die ;
: MORD-DISJOINT ( -- )
   0 MOI !
   BEGIN MOI @ 1+ NCLO @ < WHILE
      MOI @ MORD-KEY MOI @ MORD-AT CLO-BYTES +  MOI @ 1+ MORD-KEY > IF
         MOI @ MORD-AT  MOI @ 1+ MORD-AT  MORD-OVERLAP-DIE THEN
      MOI @ 1+ MOI ! REPEAT ;
\ Fill, sort, assert - once per link, from PLAN-BLOBS, because the closure is
\ final there and nothing before it asks where a member is or where it lands. A
\ test that fills the closure tables by hand calls this the way it calls
\ PLAN-TABLES: the lookups below read no other order.
\ NOTHING MAY ASK FOR A MEMBER BEFORE THIS RUNS, and nothing in a link does - the
\ first lookup of a link is in COPY-BLOBS, which is PLAN-BLOBS and then the copy.
\ A whitebox that asks earlier gets E-LAYOUT-BOUNDS off the unfilled order rather
\ than an answer, which is the fail-closed direction and how the order is proved
\ to be what the lookups read (test/compiler/aot-nested-body.f plans first for
\ that reason; test/compiler/native-code-span.f already did).
: MEMBER-ORDER ( -- )
   NCLO @ 1 < IF s" aot: no closure members to order" 74 die THEN
   NCLO @ MORD-RESERVE
   0 MOI ! BEGIN MOI @ NCLO @ < WHILE  MOI @ MOI @ MORD !  MOI @ 1+ MOI ! REPEAT
   NCLO @ MORD-SORT
   MORD-DISJOINT ;
: MORD-FIND ( ptr u8 -- n ) {: start:ptr :}   \ the position of this exact entry, or -1
   0 MBLO !  NCLO @ 1- MBHI !
   BEGIN MBLO @ MBHI @ <= WHILE
      MBLO @ MBHI @ + 2 / MBMID !
      MBMID @ MORD-KEY start = IF MBMID @ EXIT THEN
      MBMID @ MORD-KEY start < IF MBMID @ 1+ MBLO ! ELSE MBMID @ 1- MBHI ! THEN
   REPEAT  -1 ;
: MORD-BELOW ( ptr u8 -- n ) {: t:ptr :}      \ the last position at or below t, or -1
   0 MBLO !  NCLO @ 1- MBHI !  -1 MBP !
   BEGIN MBLO @ MBHI @ <= WHILE
      MBLO @ MBHI @ + 2 / MBMID !
      t MBMID @ MORD-KEY < IF MBMID @ 1- MBHI !
      ELSE MBMID @ MBP !  MBMID @ 1+ MBLO ! THEN
   REPEAT  MBP @ ;
\ The closure member whose entry this is, or -1. The entry is a member's
\ identity (aot-closure.f ADD-CLO), so this is what a record pointer or a span
\ row is resolved through before anything asks where the member is going.
: MEMBER-AT {: start:ptr :} ( ptr u8 -- n )
   start MORD-FIND {: p:n :}
   p 0 < IF -1 EXIT THEN
   p MORD-AT ;
: MEMBER-NEWOFF ( n -- n ) NEWOFF @ ;
: CLO-AT-N ( n -- n ) CLO-AT CODE-N ;      \ the same entry, for value-domain arithmetic
: BCOND? {: w:n :}  w $FF000010 and $54000000 = ;
: CBZIMM? {: w:n :}  w $7E000000 and $34000000 = ;
: TBZIMM? {: w:n :}  w $7E000000 and $36000000 = ;
: ADR? {: w:n :}  w $9F000000 and $10000000 = ;
: ADRP? {: w:n :}  w $9F000000 and $90000000 = ;
\ Compacted blob length. Under the direct-BL-only contract every word maps 1:1
\ (no movz/movk/movk/blr chain is ever collapsed), so the compacted length is the
\ member's own length.
: PLAN-BLOBS
   NCLO @ PLAN-TABLES
   MEMBER-ORDER
   ASM-LEN NEXT-OFF !
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      NEXT-OFF @       WI @ NEWOFF !
      WI @ CLO-BYTES dup WI @ BLEN !
      NEXT-OFF @ + NEXT-OFF !
      WI @ 1+ WI ! REPEAT ;
\ Relocation math for direct branches. The binary is PIE (arm64 macOS requires it),
\ so absolute targets would be wrong under the ASLR slide — instead each direct
\ branch is rewritten to a PC-RELATIVE form whose offset within __text is
\ slide-independent (no runtime relocation needed). Range checking makes linker
\ corruption fail at build time if the AOT __text ever outgrows the branch reach.
variable BDELTA  variable TNEW
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
\ ADR carries a BYTE delta, and its range check is the ENCODER's guard: no
\ relocated ADR can trip it, because ADR-TARGET! below resolves every site inside
\ its own member and the plan preserves that member's length, so the delta
\ arriving here is the one the compiler already fitted.
: ADRD32 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ -1048576 <  BDELTA @ 1048575 > or IF s" aot: ADR target out of range" 74 die THEN
   BDELTA @ 3 and 29 lshift  BDELTA @ 2 rshift $7FFFF and 5 lshift or ;

\ A target at a member's end is the adjacent member's start, not this one's.
: MAP-IN-MEMBER {: i:n t:ptr :} ( n ptr u8 -- n )
   t i CLO-AT < IF -1 EXIT THEN
   t i CLO-AT i CLO-BYTES + >= IF -1 EXIT THEN
   i MEMBER-NEWOFF  t i CLO-AT -  + ;
\ THE ONE MEMBER THAT CAN COVER t is the last one whose entry is at or below it
\ (MEMBER-ORDER above asserted the members are disjoint), so the search answers a
\ position and MAP-IN-MEMBER answers whether t is inside that member at all.
: OLD>NEW {: t:ptr :} ( ptr u8 -- n )
   t MORD-BELOW {: p:n :}
   p 0 < IF -1 EXIT THEN
   p MORD-AT t MAP-IN-MEMBER ;
: MAP-TARGET {: i:n t:ptr :} ( n ptr u8 -- n )
   i t MAP-IN-MEMBER dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
\ A TARGET THE COMPACTED IMAGE HAS NO ADDRESS FOR NAMES ITSELF. The member being
\ relocated is the site, so its record is the word to edit; the target is the
\ address the closure walk never reached, and the word that owns it - when the
\ building dictionary still knows one - says what was not carried. Without those
\ three facts the refusal is a bare sentence and the next case costs a bisection.
\ A target NO record owns is named by the record below it, `NAME+off`, the rule
\ the span refusal's site is named by (aot-closure.f CODE-ADDR-TXT): the target of
\ a branch into a stripped word, and any target an exact owner cannot be found
\ for, is otherwise a bare address the reader has nothing to open.
: MAP-TARGET! {: i:n t:ptr :} ( n ptr u8 -- )
   i t MAP-TARGET TNEW !
   TNEW @ -1 = IF
      s" aot: PC-relative target removed or outside closure site=" AETXT
      i CLO-REC@ AEREC-TXT
      s"  target=" AETXT t CODE-N AEJNUM
      s"  target-word=" AETXT t CODE-N ADDRESS-OWNER t CODE-ADDR-TXT
      10 AE1
      s" " 74 die THEN ;
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
\ ANSWER to such a target: the planner maps every word 1:1 (PLAN-BLOBS above,
\ "the compacted length is the member's own length"), and ADRP+ADD is two words
\ where the ADR was one. ITS TARGET IS NAMED THE WAY MAP-TARGET!'s is, and an ADR
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
: DECLARATION-TARGET? ( ptr u8 -- bool ) {: t:ptr :}
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? 0= IF AOT-FALSE EXIT THEN
   callee AOT-DECLARATION? ;
: RELOC-DIRECT ( n ptr u8 n -- n ) {: i:n p:ptr w:n :}
   p w TARGET {: t:ptr :}
   t DECLARATION-TARGET? IF ENC-NOP EXIT THEN
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

\ The closure member whose span holds this code address, or -1 when nothing in
\ the closure covers it. The owner record answers for a word the image still
\ names; the payload's span table answers for one it does not, and an address
\ neither can place is refused here rather than relocated to a guess.
: ADDRESS-MEMBER ( n -- n ) {: v:n :}
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if owner REC-CODE-PTR@ MEMBER-AT exit then
   v SPAN-OWNER {: k:n :}
   k 0 < if s" aot: code address has no dictionary owner" 74 die then
   k SPAN-START MEMBER-AT ;

\ A declared xt cell's row target is the new offset of the code it names, from
\ the SAME mapping a relocated branch target goes through (OLD>NEW: the member
\ that covers the address, plus the address's own distance into it, which
\ COPY-COMPACT-BLOB's instruction-for-instruction copy preserves). An address no
\ member covers dies by name here: the emit-time proof that this image carries
\ the code the cell will point at, after the root pass put it in the closure.
: XT-CELL-TARGET ( n -- n ) {: k:n :}
   k XTC-VAL@ CODE-PTR OLD>NEW {: t:n :}
   t 0 < IF s" aot: declared xt cell target is outside the closure" 74 die THEN
   t ;

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

: COPY-ADDRESS ( ptr n ptr u8 ptr u8 -- ) {: owner:ptr p:ptr e:ptr :}
   p e ADDRESS-VALUE {: v:n :}
   v DATA-ADDRESS? if
      owner p v DATA-TARGET {: target:n :}
      \ Keep all four instructions and their Rd/shift/opcode fields. A variable
      \ length literal encoder would invalidate the planned code offsets.
      SNAP-RELOC:ADDR-CHAIN-BYTES 4 / 0 ?do
         p i 4 * + AOT-W32@ SNAP-RELOC:ADDR-OPC-MASK and
         target i 16 * rshift $FFFF and 5 lshift or EMITW
      loop
      exit
   then
   v ADDRESS-MEMBER {: m:n :}
   m 0 < if s" aot: code address is outside the closure" 74 die then
   m MEMBER-NEWOFF  v m CLO-AT-N -  +
   p AOT-W32@ SNAP-RELOC:ADDR-RD-MASK and EMIT-CODE-ADDRESS ;

: COPY-COMPACT-BLOB {: i:n :} ( n -- )
   i CLO-AT CP2 !  i CLO-AT i CLO-BYTES + CEND !
   BEGIN CP2 @ CEND @ < WHILE
      CP2 @ CEND @ ABS-CHAIN? IF ABS-CHAIN-DIE THEN
      CP2 @ ADDRESS-SITE? if
         i CLO-REC@ CP2 @ CEND @ COPY-ADDRESS
         SNAP-RELOC:ADDR-CHAIN-BYTES
      else
         i CP2 @ CP2 @ AOT-W32@ RELOC-W32 EMITW 4
      then CP2 @ + CP2 !
   REPEAT ;
: COPY-PLANNED-BLOBS
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ 0= IF MLBL LABEL@ LBL, THEN          \ MAIN is closure word 0 -> place its label
      WI @ COPY-COMPACT-BLOB
      WI @ 1+ WI ! REPEAT ;
: COPY-BLOBS  PLAN-BLOBS  COPY-PLANNED-BLOBS ;
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

public

\ The span is already latched: tools/aot-build-core.f calls AOT-DATA-SPAN the moment
\ the application has loaded, because this file - and every lib module the linker
\ needs - is loaded AFTER that and allots above BLOB-END. LINK therefore reads the
\ bounds it is given and never latches them itself; latching here would put the
\ whole linker inside the span.
: LINK ( -- )
   CARRY-CELLS                                      \ the engine constants this image carries
   COLLECT-XT-CELLS                                 \ the window's DECLARED cells: xt rows out, DATA cells mapped
   AOT-DATA-TEXTPTR-CHECK                           \ ... and no undeclared code pointer beside them
   CLOSURE  ASM-INIT  LBL MLBL !  LBL BLOB-LBL !  LBL LTEXT !
   LBL LCRASHH !  LBL LSIGH !  LBL LHEX !  LBL LHDR !   \ the stripped image carries both handlers too
   EMIT-ENTRY  COPY-BLOBS  RELOCATE  EMIT-CRASH-CODE  EMIT-DATA-BLOB  EMIT-XT-ROWS
   AOT-WRITE-OBJ
   s" hb-prog" AOT-OUT DRV-EMIT-IMAGE ;

;using
;package
