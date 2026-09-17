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
create NEWOFF MAX-CLO cells allot   create BLEN MAX-CLO cells allot

\ --- persistent data region: the program's compile-time create/variable/allot/
\ ,/s" data lives contiguously from the DATA pointer latched before user
\ compilation to `here`; the source buffer and assembler CODE buffer are separate
\ mmaps, so AOT-LINK never allots either into DATA. We emit that span
\ into __text and the entry maps DATA-VA and copies it back to the SAME absolute
\ VA (DATA-VA is a fixed MAP_FIXED VA, so those addresses are load-stable). All
\ other runtime cells stay zero from the fresh anonymous mmap; only x20, S0-CELL,
\ and DP-CELL need explicit init.
variable DSCAN
$F0000 constant AOT-DATA-BLOB-MAX          \ keep the blob within ADR ±1MB range

: AOT-DATA-START ( -- )
   here BLOB-SRC !
   \ The retained compiler must intern this application's strings and trap
   \ messages inside the span the stripped image restores.
   NSTR:WINDOW-OPEN ;

: AOT-DATA-SPAN ( -- )
   here  BLOB-END !
   BLOB-END @ BLOB-SRC @ - dup 0 < IF s" aot: negative data span" 74 die THEN BLOB-LEN ! ;

\ Every cell the capture window covers that NOTHING DECLARED, classified by the
\ ONE predicate aot-closure.f publishes (CELL-TEXTPTR?, by live engine extents).
\ A declared xt cell is relocated instead (COLLECT-XT-CELLS above it, EMIT-XT-ROWS
\ below), and it is skipped here by its declaration and not by its value, so the
\ two answers cannot disagree about one cell. What is left is a code or dictionary
\ pointer no declaration accounts for - a `' word ,` table - and the scan reports
\ the first one with the three facts a program has to be edited by: its owning
\ word, its DATA offset and the pointer it holds.
: AOT-DATA-TEXTPTR-CHECK ( -- )
   XTC-REWIND
   BLOB-SRC @ DSCAN !
   BEGIN DSCAN @ 8 + BLOB-END @ <= WHILE
      DSCAN @ XTC-DECLARED? 0= IF
         DSCAN @ @ CELL-TEXTPTR? IF DSCAN @ DSCAN @ @ REFUSE-UNDECLARED-CELL THEN
      THEN
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

\ ONE UNSIGNED LEB128 VARINT, INLINE: seven bits a byte from the cursor `cur`,
\ low group first, until a byte arrives with its high bit clear. `acc` answers
\ the value, `cur` is advanced past it, and `b`, `g` and `sh` are clobbered.
\ This is src/habu/aot-decl.f AOT-WINDOW:RUN-V! read backwards, and the same
\ grammar src/habu/habu2.f AOT-WINDOW:APPLY-RUNS decodes for the baked window.
: EMIT-VGET ( n n n n n -- ) {: acc:n cur:n b:n g:n sh:n :}
   LBL {: vtop:label :}
   acc 0 MOVZ,  sh 0 MOVZ,
   vtop LBL,
      b cur 0 LDRB,  cur cur 1 ADDI,
      g b $7F ANDI,  g g sh LSLV,  acc acc g ORR,
      sh sh 7 ADDI,
      g b $80 ANDI,  g vtop CBNZ, ;

\ A row is (gap from the last run's end, length), so the destination cursor x13
\ is the decoder's whole state between rows: it ends each run one past that
\ run's last byte, which is exactly what the next row's gap counts from.
: EMIT-DATA-COPY ( -- )
   BLOB-LEN @ 0= IF
      7 BLOB-END @ LIT64,  7 DATA DP-CELL STR,  exit         \ DP = data base (no user data)
   THEN
   9 BLOB-LBL LABEL@ ADR,                         \ x9 = sparse header in __text
   11 9 0 LDRW,                                   \ x11 = encoded row byte length
   9 9 4 ADDI,                                    \ x9 = row cursor
   11 9 11 ADD,                                   \ x11 = one past the last row == byte payload start
   10 11 0 ADDI,                                  \ x10 = byte payload cursor
   13 BLOB-SRC @ LIT64,                           \ x13 = destination cursor, at the span's base VA
   LBL LBL LBL LBL {: rowtop:label rowdone:label innertop:label outerback:label :}
   rowtop LBL,
      9 11 CMP,  C-CS rowdone BCOND,
      14 9 15 16 12 EMIT-VGET                     \ x14 = gap from the last run's end
      13 13 14 ADD,
      14 9 15 16 12 EMIT-VGET                     \ x14 = this run's length
      innertop LBL,
      14 outerback CBZ,
      15 10 0 LDRB,  15 13 0 STRB,
      10 10 1 ADDI,  13 13 1 ADDI,  14 14 1 SUBI,
      innertop B,
   outerback LBL,
      rowtop B,
   rowdone LBL,
   7 BLOB-END @ LIT64,  7 DATA DP-CELL STR, ;      \ DP = user-end (runtime here/allot base)

\ ---- the declared xt cells: what the copy alone cannot restore -----------------
\ The copy above restores every cell's capture-time BYTES, and for a cell declared
\ to hold an execution token those bytes are the BUILDER's code address. The rows
\ EMIT-XT-ROWS places after the blob say where such a cell is and where its word
\ ended up in THIS image, in the shape src/habu/aot-file.f's XTOFF rows carry: a
\ u32 location and a u32 target in one eight-byte row.
\ THE ROWS NEED NO ADDRESS OF THEIR OWN. The copy loop's byte cursor x10 ends one
\ past the blob's last payload byte, which is where the rows begin (BYTES, pads
\ the blob to the four-byte boundary they sit on), so the startup adds no second
\ ADR ahead of the root call: test/gate-aot-image.f admits exactly one ADR x9 in
\ the startup and ends the reported code range at the blob, and rows placed past
\ the blob are outside that range rather than read as instructions.
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

\ --- sparse encoding: the captured span travels as its non-zero byte extents
\ (plus the zero gaps under AOT-WINDOW:RUN-GAP-MIN, which are cheaper to carry
\ than to split around), not as the span. A table `allot`ed at declared capacity
\ but only partly filled left its unused tail as literal zero bytes in every
\ earlier image; the restore above maps an anonymous (already zero) region, so a
\ zero byte never has to travel. Format: [row bytes u32] [(gap varint, length
\ varint) until those bytes are spent] [bytes, row order, concatenated] - one
\ cursor decodes it with no stored row->byte offset, and the header is a byte
\ length rather than a row count because a varint row has no fixed width. The
\ row IS the AOT-WINDOW run row aot-capture.f writes for the metabuild seed
\ (src/habu/aot-decl.f package AOT-WINDOW).
\ SPARSE-CAP is generous headroom over the row/byte overhead of a span already
\ expected to stay near AOT-DATA-BLOB-MAX; a span that still overflows it dies
\ closed by name instead of corrupting the buffer.
AOT-DATA-BLOB-MAX 2 * constant SPARSE-CAP
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
: BLOB-SRC@ ( -- ptr u8 ) BLOB-SRC @ ;

\ The non-zero byte extents of [0, BLOB-LEN), visited in declaration order.
\ Mirrors aot-capture.f ACAP-SCAN-SEG/ACAP-RUN-CLOSE, gap rule included: a run
\ ends at its last non-zero byte and reopens only after AOT-WINDOW:RUN-GAP-MIN
\ zeros, because a shorter gap costs less to carry than the row that splitting
\ it buys. The stripped span has no declared address cells to exclude, so the
\ whole span is one gap.
variable BLOB-RUN-AT   variable BLOB-RUN-OPEN   variable BLOB-RUN-END

: BLOB-RUN-CLOSE ( [ n n -- ] n -- ) {: body at:n :}
   BLOB-RUN-OPEN @ 0 < IF exit THEN
   BLOB-RUN-OPEN @  at BLOB-RUN-OPEN @ -  body execute
   -1 BLOB-RUN-OPEN ! ;

: EACH-BLOB-RUN ( [ n n -- ] -- ) {: body :}
   -1 BLOB-RUN-OPEN !  0 BLOB-RUN-END !  0 BLOB-RUN-AT !
   BEGIN BLOB-RUN-AT @ BLOB-LEN @ < WHILE
      BLOB-SRC@ BLOB-RUN-AT @ + c@ 0= IF
         BLOB-RUN-AT @ BLOB-RUN-END @ - AOT-WINDOW:RUN-GAP-MIN >= IF
            body BLOB-RUN-END @ BLOB-RUN-CLOSE THEN
      ELSE
         BLOB-RUN-OPEN @ 0 < IF BLOB-RUN-AT @ BLOB-RUN-OPEN ! THEN
         BLOB-RUN-AT @ 1+ BLOB-RUN-END !
      THEN
      BLOB-RUN-AT @ 1+ BLOB-RUN-AT !
   REPEAT
   body BLOB-RUN-END @ BLOB-RUN-CLOSE ;

\ One past the last row's run: what the next row's gap counts from.
variable BLOB-LAST-END

: BLOB-V! ( n -- ) {: v:n :}
   v  SPARSE-BUF SPARSE-LEN @ +  AOT-WINDOW:RUN-V! {: w:n :}
   w 0= IF s" aot: a sparse data row field is not a u32 varint" 74 die THEN
   SPARSE-LEN @ w + SPARSE-LEN ! ;

: BLOB-ROW! ( n n -- ) {: start:n len:n :}
   AOT-WINDOW:RUN-VMAX 2 * SPARSE-ROOM?
   start BLOB-LAST-END @ - BLOB-V!
   len BLOB-V!
   start len + BLOB-LAST-END ! ;

: BLOB-BYTES! ( n n -- ) {: start:n len:n :}
   len 0 ?do  BLOB-SRC@ start + i + c@ SPARSE-BYTE!  loop ;

: BUILD-SPARSE-DATA ( -- )
   0 SPARSE-LEN !  0 BLOB-LAST-END !
   4 SPARSE-ROOM?  4 SPARSE-LEN !
   [: BLOB-ROW! ;] EACH-BLOB-RUN
   SPARSE-LEN @ 4 -  0 SPARSE-U32!
   [: BLOB-BYTES! ;] EACH-BLOB-RUN ;

\ The declared xt-cell rows follow this blob immediately (LINK emits them next),
\ at the four-byte boundary BYTES, pads to.
: EMIT-DATA-BLOB ( -- )                            \ place the sparse blob after all code
   BLOB-LEN @ 0= IF exit THEN
   ASM-LEN AOT-DATA-BLOB-MAX > IF
      s" aot: data blob too far for ADR (program too large); split program" 74 die THEN
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
\ The address travels in x11 for the reason G-INSTALL-CRASH's does -
\ test/gate-aot-image.f finds the DATA restore by its ADR x9 and admits exactly
\ one in the startup.
: EMIT-SIGNAL-PUBLISH ( -- )
   11 LSIGH LABEL@ ADR,  11 DATA SIGNAL-ABI:STUB-CELL STR,
   11 DATA-VA VA>N SIGNAL-ABI:FD-CELL + LIT64,  11 DATA SIGNAL-ABI:FD-PTR-CELL STR, ;

\ A stripped image runs the same three guarded VM stacks as the engine, and
\ installs the same crash handler. Both matter here and not only in the engine:
\ a stripped application has no interpreter to name an overflow, so before guard
\ pages its only diagnostic WAS the per-transfer check. Without the handler the
\ guard page would be a bare SIGSEGV; with it, the overflow is the same named
\ `hb: stack bounds exceeded (<which>)` and STACK-BOUNDS exit the engine gives.
: EMIT-ENTRY
   LTEXT LABEL@ LBL,                             \ text offset zero: this image's code base
   STACK-ABI:BOOT-BYTES XDS STACK-GUARD:EMIT-MAP
   EMIT-DATA-REGION-MAP                          \ map DATA-VA, set x20/S0
   STACK-ABI:RETURN-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:RETURN-BASE-CELL STR,
   STACK-ABI:LOOP-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:LOOP-BASE-CELL STR,
   G-INSTALL-CRASH                               \ name a guard-page fault instead of dumping SIGSEGV
   EMIT-DATA-COPY                                \ restore persistent data + DP
   EMIT-XT-CELLS                                 \ ... and the tokens its declared cells hold
   EMIT-SIGNAL-PUBLISH                           \ this image's stub address and fd word
   EMIT-SEED                                     \ push preseeded value-stack cells (empty for MAIN)
   MLBL LABEL@ BL,                              \ bl <entry root> (resolved when MLBL is placed)
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;               \ exit(0)

\ The handler body and its hex printer follow the copied code, so the root
\ closure record stays the first word after the startup (test/gate-aot-image.f
\ pins that) and the entry's ADR to the handler spans at most the code, which
\ EMIT-DATA-BLOB already bounds for the data blob placed after them.
: EMIT-CRASH-CODE ( -- )
   EMIT-CRASH-HANDLER  EMIT-SIGNAL-HANDLER  EMIT-HEX ;
variable CP2  variable CEND  variable NEXT-OFF
\ The closure member whose entry this is, or -1. The entry is a member's
\ identity (aot-closure.f ADD-CLO), so this is what a record pointer or a span
\ row is resolved through before anything asks where the member is going.
: MEMBER-AT {: start:ptr :} ( ptr u8 -- n )
   0 CLO-CX !
   BEGIN CLO-CX @ NCLO @ < WHILE
      CLO-CX @ CLO-AT start = IF CLO-CX @ EXIT THEN
      CLO-CX @ 1+ CLO-CX ! REPEAT  -1 ;
: MEMBER-NEWOFF ( n -- n ) cells NEWOFF + @ ;
: CLO-AT-N ( n -- n ) cells CLO + @ ;      \ the same cell, for value-domain arithmetic
: BCOND? {: w:n :}  w $FF000010 and $54000000 = ;
: CBZIMM? {: w:n :}  w $7E000000 and $34000000 = ;
: TBZIMM? {: w:n :}  w $7E000000 and $36000000 = ;
: ADR? {: w:n :}  w $9F000000 and $10000000 = ;
: ADRP? {: w:n :}  w $9F000000 and $90000000 = ;
\ Compacted blob length. Under the direct-BL-only contract every word maps 1:1
\ (no movz/movk/movk/blr chain is ever collapsed), so the compacted length is the
\ member's own length.
: PLAN-BLOBS
   ASM-LEN NEXT-OFF !
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      NEXT-OFF @       NEWOFF WI @ cells + !
      WI @ CLO-BYTES dup BLEN WI @ cells + !
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
: ADRD32 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ -1048576 <  BDELTA @ 1048575 > or IF s" aot: ADR target out of range" 74 die THEN
   BDELTA @ 3 and 29 lshift  BDELTA @ 2 rshift $7FFFF and 5 lshift or ;

\ A target at a member's end is the adjacent member's start, not this one's.
: MAP-IN-MEMBER {: i:n t:ptr :} ( n ptr u8 -- n )
   t i CLO-AT < IF -1 EXIT THEN
   t i CLO-AT i CLO-BYTES + >= IF -1 EXIT THEN
   i MEMBER-NEWOFF  t i CLO-AT -  + ;
: OLD>NEW {: t:ptr :} ( ptr u8 -- n )
   0 CLO-CX !
   BEGIN CLO-CX @ NCLO @ < WHILE
      CLO-CX @ t MAP-IN-MEMBER dup -1 <> IF EXIT THEN drop
      CLO-CX @ 1+ CLO-CX ! REPEAT  -1 ;
: MAP-TARGET {: i:n t:ptr :} ( n ptr u8 -- n )
   i t MAP-IN-MEMBER dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
: MAP-TARGET! {: i:n t:ptr :} ( n ptr u8 -- )
   i t MAP-TARGET TNEW !
   TNEW @ -1 = IF s" aot: PC-relative target removed or outside closure" 74 die THEN ;
: BTGT19 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 BITS 19 SX 4 * + ;
: BTGT14 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 14 BITS 14 SX 4 * + ;
: ADRTGT {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 BITS 2 lshift  w 29 2 BITS or 21 SX + ;
: RELOC-W32 {: i:n p:ptr w:n :} ( n ptr u8 n -- n )
   w DIRECT? IF
      i p w TARGET MAP-TARGET!
      w $FC000000 and  ASM-LEN TNEW @ REL26 or EXIT THEN
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
      i p w ADRTGT MAP-TARGET!
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
      then CP2 +!
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
      NEWOFF WI @ cells + @ RP !
      RP @ BLEN WI @ cells + @ + RE !
      BEGIN RP @ RE @ < WHILE
         CODE RP @ + CODE RE @ + ABS-CHAIN? IF ABS-CHAIN-DIE THEN
         RP @ 4 + RP !
      REPEAT
      WI @ 1+ WI ! REPEAT ;

public

: LINK ( -- )
   AOT-DATA-SPAN
   COLLECT-XT-CELLS                                 \ the window's DECLARED xt cells
   AOT-DATA-TEXTPTR-CHECK                           \ ... and no undeclared code pointer beside them
   CLOSURE  ASM-INIT  LBL MLBL !  LBL BLOB-LBL !  LBL LTEXT !
   LBL LCRASHH !  LBL LSIGH !  LBL LHEX !  LBL LHDR !   \ the stripped image carries both handlers too
   EMIT-ENTRY  COPY-BLOBS  RELOCATE  EMIT-CRASH-CODE  EMIT-DATA-BLOB  EMIT-XT-ROWS
   AOT-WRITE-OBJ
   s" hb-prog" AOT-OUT DRV-EMIT-IMAGE ;

;using
;package
