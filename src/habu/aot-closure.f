\ aot-closure.f - stripped AOT closure analysis and diagnostics.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/address-cells.f
require src/habu/code-span.f
\ The span cells and HERE-N come from the lib-free latch file, which the build
\ driver loads on its own and much earlier - before the application's require.
require src/habu/aot-window-latch.f
require src/habu/aot-owned-cells.f

\ This file compiles checked, with raw-pointer boundaries as explicit TRUST rows.

\ The checker registry does not publish its diagnostic-mode cell to later
\ checked loads, so the AOT diagnostic reader types that boundary here.
\ Retirement: habu-primitive-effect-axiom-1119f176.
s" JSON-DIAGS" s" -- ptr a" TRUST

package AOT-LINK
using SNAP-RELOC

\ These views expose mixed dictionary fields and the live code/dictionary extent
\ used to classify stripped-image cells. Retirement:
\ habu-builder-trust-rows-c5d41af6.
: AOT-DBASE@ dbase@ ;
s" AOT-DBASE@" s" -- ptr a" TRUST
\ Live-extent bounds as integers, for value-domain range tests (CELL-TEXTPTR?):
\ the dict-record region base and the emitted-code high-water.
: AOT-DBASE-N dbase@ ;
s" AOT-DBASE-N" s" -- n" TRUST
: AOT-CP-N cp@ ;
s" AOT-CP-N" s" -- n" TRUST
\ ... and the DATA cursor as one, which src/habu/aot-window-latch.f HERE-N
\ computes, because the window is latched with it before this file exists.
: AOT-PTR@ {: a:ptr :} ( ptr a -- ptr a )
   a @ ;
s" AOT-PTR@" s" ptr a -- ptr a" TRUST

\ --- read a little-endian 32-bit instruction word from a code pointer. Used by the
\ direct-branch closure scan and the linker's address-chain validation.
: AOT-W32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

\ The map declares address sites, so an ordinary numeric literal with the same
\ bits is never relocated. Engine-text records are outside this region map.
: ADDRESS-SITE? ( ptr u8 -- bool ) {: p:ptr :}
   p AOT-DBASE@ BYTE-VIEW - {: off:n :}
   off DICT-SIZE < off REGION >= or if false exit then
   data-base ADDRMAP-OFF + off 5 rshift + BYTE-VIEW c@
   off 2 rshift 7 and rshift 1 and 0<> ;

\ MOVZ #lo; MOVK #hi,lsl16/32/48. Keep opcode, shift and register bits;
\ only the four immediate fields may differ. CHAINV itself is only a decoder.
: ADDRESS-CHAIN? ( ptr u8 ptr u8 -- bool ) {: p:ptr e:ptr :}
   p e > if false exit then
   e p - ADDR-CHAIN-BYTES < if false exit then
   p AOT-W32@ ADDR-RD-MASK and {: rd:n :}
   p AOT-W32@ ADDR-OPC-MASK and $D2800000 rd or =
   p 4 + AOT-W32@ ADDR-OPC-MASK and $F2A00000 rd or = and
   p 8 + AOT-W32@ ADDR-OPC-MASK and $F2C00000 rd or = and
   p 12 + AOT-W32@ ADDR-OPC-MASK and $F2E00000 rd or = and ;

: ADDRESS-VALUE ( ptr u8 ptr u8 -- n ) {: p:ptr e:ptr :}
   p e ADDRESS-CHAIN? 0= if s" aot: malformed recorded address chain" 74 die then
   p CHAINV ;

: DATA-ADDRESS? ( n -- bool ) {: v:n :}
   \ The outer mapping has the same stable one-past address as a captured span.
   v DATA-VA VA>N >= v DATA-VA VA>N DATA-SIZE + <= and ;

\ Only the user DATA span is copied by stripped startup. A fixed address into
\ the compiler's earlier heap would otherwise silently read a zeroed replacement.
\ DATA-ADDRESS! and DATA-TARGET, which enforce that, are defined with the other
\ refusals below, after the record accessors their diagnostics name a cell with.
\ BLOB-SRC, BLOB-END and BLOB-LEN are the latch file's; this is the emitted
\ blob's label, which only aot-lib.f places and reads.
variable BLOB-LBL

\ Decode an AArch64 direct branch (B / BL). Both share opcode bits: masking off
\ the link bit leaves $14000000, so DIRECT? recognizes B and BL and excludes the
\ conditional/compare branches (BCOND/CBZ/TBZ), which stay intra-record and are
\ never followed as calls. TARGET returns the branch's absolute code address =
\ site + sign-extended(imm26) * 4.
$7C000000 constant MASK
$14000000 constant OPCODE
$3FFFFFF constant DELTA-MASK
$2000000 constant SIGN

: SIGNED ( n -- n ) SIGN xor SIGN - ;

: DIRECT? ( n -- bool ) MASK and OPCODE = ;

: TARGET ( ptr u8 n -- ptr u8 ) {: p:ptr w:n :}
   p w DELTA-MASK and SIGNED 4 * + ;

: REC {: k:n :} ( n -- ptr a )
   AOT-DBASE@ k 48 * + ;          \ dict record k  (0:addr 8:len 16:name-len|flags 24:name|ptr)
: REC-BYTES ( ptr n -- n ) 8 + @ CODE-SPAN:BYTES ;
: AOT-FOLD {: c:n :}  c 64 > c 91 < and IF c 32 + ELSE c THEN ;
\ A dict record is one raw 48-byte block that holds BOTH cells (code address,
\ code length, name-length|flags) and — for a short name — the name BYTES. A
\ single pointer cannot be a cell pointer and a byte pointer at once, so each
\ view is minted separately: the name accessors work from the record INDEX and
\ call REC once per view. REC>IX recovers the index for the callers that carry a
\ record pointer. `REC-NAME-PTR ( ptr a -- ptr a )` previously declared the name
\ pointer to have the record's element type, which is what let a cell pointer be
\ read with c@.
: REC>IX {: r:ptr :} ( ptr a -- n )
   r AOT-DBASE@ - 48 / ;
: IX-NAME-LEN {: k:n :} ( n -- n )
   k REC 16 + @ DNAME-LEN-MASK and ;
: IX-NAME-PTR {: k:n :} ( n -- ptr u8 )
   k REC 16 + @ DNAME-EXT and 0= IF k REC 24 + ELSE k REC 24 + AOT-PTR@ THEN ;

: REC-NAME-LEN {: r:ptr :} ( ptr a -- n )
   r REC>IX IX-NAME-LEN ;
: REC-NAME-PTR {: r:ptr :} ( ptr a -- ptr u8 )
   r REC>IX IX-NAME-PTR ;
: REC-NAME@ {: r:ptr :} ( ptr a -- ptr u8 n )
   r REC-NAME-PTR  r REC-NAME-LEN ;

: REC-NAME-C@ {: r:ptr idx:n :} ( ptr a n -- n )
   r REC-NAME-PTR idx + c@ ;

: REC-NAME= {: r:ptr a:ptr u:n :} ( ptr a ptr u8 n -- bool )
   r REC-NAME-LEN u = IF
      0 BEGIN dup u < WHILE
         dup r swap REC-NAME-C@ AOT-FOLD
         over a + c@ AOT-FOLD = 0= IF drop 0 0= 0= EXIT THEN
         1 +
      REPEAT drop 0 0=
   ELSE 0 0= 0= THEN ;
\ Selected AOT entry word. Defaults to MAIN (the zero-argument process entry);
\ a preseeded test entry (tools/hb-build.f --preseed-entry) sets it to a matched
\ helper so the stripped image starts at a non-MAIN root. Bare names select
\ global words; package entries use their public qualified token.
create ENTRY-NAME-BUF 64 allot   variable ENTRY-NAME-U
: ENTRY-NAME$ ( -- ptr u8 n )
   ENTRY-NAME-BUF ENTRY-NAME-U @ ;
: ENTRY-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1 < IF s" aot: empty entry name" 74 die THEN
   u 64 > IF s" aot: entry name too long" 74 die THEN
   a ENTRY-NAME-BUF u BYTE-COPY
   u ENTRY-NAME-U ! ;
s" MAIN" ENTRY-NAME!
\ Data-space words (@ ! c@ c! here allot , c,) are now supported: the AOT entry
\ maps the fixed DATA region and restores the program's persistent data (see
\ aot-lib.f). Only words that need machinery the stripped binary does not carry
\ stay rejected — the closure walk distinguishes runtime use from compile-time
\ definition, so `create` here means runtime dictionary creation (no dictionary).
\ The five writers below all reach the code region or a live dictionary record
\ (stripped __text is r-x and not at RBASE-VA). `patch32` is the isolated
\ single-word poke;
\ `code-publish` is the bulk publication window, `xref-retarget` points a record
\ at a routine, `callmap-set` records a call site's relocation class, and
\ `addrmap-set` records an address chain's.
: AOT-UNSAFE? {: r:ptr :} ( ptr a -- bool )
   r s" create" REC-NAME= IF 0 0= EXIT THEN
   r s" patch32" REC-NAME= IF 0 0= EXIT THEN
   r s" code-publish" REC-NAME= IF 0 0= EXIT THEN
   r s" callmap-set" REC-NAME= IF 0 0= EXIT THEN
   r s" addrmap-set" REC-NAME= IF 0 0= EXIT THEN
   r s" xref-retarget" REC-NAME= IF 0 0= EXIT THEN
   0 0= 0= ;
create AECH 1 allot
: AE1 {: c:n :}  c AECH c!  2 AECH 1 write drop ;
: AETXT {: a:ptr u:n :} ( ptr u8 n -- )
   2 a u write drop ;
: AEREC-TXT {: r:ptr :} ( ptr a -- )
   r 0= IF s" <unknown>" AETXT ELSE r REC-NAME@ AETXT THEN ;
: AEJCHAR {: c:n :}
   c 10 = IF 92 AE1 110 AE1 EXIT THEN
   c 13 = IF 92 AE1 114 AE1 EXIT THEN
   c 9 = IF 92 AE1 116 AE1 EXIT THEN
   c 34 =  c 92 = or IF 92 AE1 THEN  c AE1 ;
: AEJSTR {: a:ptr u:n :} ( ptr u8 n -- )
   34 AE1  0 BEGIN dup u < WHILE dup a + c@ AEJCHAR 1 + REPEAT drop 34 AE1 ;
: AEJKEY {: a:ptr u:n :} ( ptr u8 n -- )
   a u AEJSTR 58 AE1 ;
: AEJREC {: r:ptr :} ( ptr a -- )
   r 0= IF s" <unknown>" AEJSTR ELSE r REC-NAME@ AEJSTR THEN ;
create AENB 20 allot  variable AENV  variable AENN
: AEJNUM
   AENV !  0 AENN !
   AENV @ 0= IF 48 AE1 EXIT THEN
   BEGIN AENV @ 0 > WHILE
      AENV @ 10 mod 48 +  AENB AENN @ + c!
      AENN @ 1 + AENN !
      AENV @ 10 / AENV !
   REPEAT
   AENN @ BEGIN dup 0 > WHILE 1 - dup AENB + c@ AE1 REPEAT drop ;
: AOT-UNSAFE-JSON {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY caller AEJREC 44 AE1
   s" token" AEJKEY callee AEJREC 44 AE1
   s" reason" AEJKEY s" stripped AOT has no runtime compiler, dictionary, or writable code" AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" stripped AOT cannot run create/patch32 at runtime; use --repl or remove the word from the runtime path" AEJSTR
   125 AE1 10 AE1 ;
: AOT-UNSAFE-PROSE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   s" hb-build: stripped AOT unsupported word '" AETXT
   callee AEREC-TXT
   s" ' called by '" AETXT
   caller AEREC-TXT
   s" '" AETXT 10 AE1 ;
: AOT-UNSAFE-DIE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   JSON-DIAGS @ IF caller callee AOT-UNSAFE-JSON ELSE caller callee AOT-UNSAFE-PROSE THEN
   s" hb-build: AOT unsupported word" 70 die ;

variable FX
: REC-CODE-PTR ( ptr a -- ptr ptr u8 ) {: r:ptr :}  r 0 ptr-field ;
: REC-CODE-PTR@ ( ptr a -- ptr u8 )  REC-CODE-PTR @ ;
: REC-WID@ ( ptr n -- n ) {: r:ptr :}  r 40 + @ ;

\ ---- the payload's code-span table --------------------------------------------
\ THE IMAGE SHIPS NO RECORD FOR A WORD NOTHING CAN NAME, and the walk below still
\ has to account for that word's code: a displacement landing in a span nothing
\ accounts for has nowhere to go. So the payload carries the span instead of the
\ record - an 8-byte row of (blob offset u32, raw CODE-SPAN u32), which is exactly
\ the two numbers this file asks a record for - and the seed publishes the table,
\ its row count and the address it copied the blob to (habu2.f AOT-SPAN:PUBLISH,).
\ All three cells are zero in an engine that captured nothing and in a whitebox
\ image that kept every name, and a zero count is simply no rows.
\ THE BASE IS READ TWICE, as a pointer for the spans this file walks and as a
\ number for the value-domain range test, the same two readings CELL-TEXTPTR?
\ already makes of the live extents above.
: SPAN-TABLE ( -- ptr u8 )
   data-base AOT-SPAN:TABLE-CELL CELL / ptr-field @ ;
: SPAN-BASE ( -- ptr u8 )
   data-base AOT-SPAN:BASE-CELL CELL / ptr-field @ ;
: SPAN-BASE-N ( -- n ) data-base AOT-SPAN:BASE-CELL + @ ;
: SPAN-N ( -- n ) data-base AOT-SPAN:N-CELL + @ ;
: SPAN-ROW ( n -- ptr u8 ) {: k:n :}
   SPAN-TABLE k AOT-SPAN:ROW * + ;
: SPAN-OFF ( n -- n ) SPAN-ROW AOT-W32@ ;
: SPAN-START ( n -- ptr u8 ) {: k:n :} SPAN-BASE k SPAN-OFF + ;
: SPAN-START-N ( n -- n ) {: k:n :} SPAN-BASE-N k SPAN-OFF + ;
: SPAN-BYTES ( n -- n ) SPAN-ROW 4 + AOT-W32@ CODE-SPAN:BYTES ;

\ The span that STARTS here, or -1: FINDADDR-PTR's question, asked of the words
\ the image ships no record for.
: SPAN-AT-ENTRY ( ptr u8 -- n ) {: t:ptr :}
   SPAN-N 0 ?do
      i SPAN-START t = if i unloop exit then
   loop  -1 ;

\ ... and the span that CONTAINS this address, which is ADDRESS-OWNER's second
\ question: an anonymous body is interior to the word that emitted it.
: SPAN-OWNER ( n -- n ) {: t:n :}
   SPAN-N 0 ?do
      t i SPAN-START-N >= if
         t i SPAN-START-N - i SPAN-BYTES < if i unloop exit then
      then
   loop  -1 ;

: FINDMAIN ( -- ptr n )
   ENTRY-NAME$ XREF-FIND ;

\ --- closure: BFS from MAIN over the native call graph. CLO and the parallel
\ COPY/RELOCATE arrays (NEWOFF/BLEN) are all sized by MAX-CLO; ADD-CLO fails
\ closed at the cap so a large closure can never write past the tables.
\
\ A MEMBER IS A CODE SPAN, and a record is what NAMED it rather than what it is.
\ The words the image still names arrive with one and the stripped ones arrive
\ from the span table above; everything past this point - the plan, the copy, the
\ branch retarget - asks a member only for its entry and its length, which is why
\ the table needs to carry nothing else. CLO-REC is XREF-NULL for a member no
\ record names, and only the unsupported-word check and the diagnostics read it;
\ both already answer for a record that is not there.
\ THE ENTRY IS ALSO THE IDENTITY. Two records can share one entry - `EXPORT`
\ publishes a second name for the same execution token - and one span is one
\ member however many names reach it, so the blob is copied once.
\ Two of the three columns hold ADDRESSES, so they are declared tables and not
\ `create ... cells allot`: a member's entry is a code pointer and its record is
\ a dictionary pointer, while its length is a count.
1024 constant MAX-CLO
MAX-CLO TYPED-BUFFER CLO ptr u8      \ each member's code entry
create CLO-LEN MAX-CLO cells allot   \ ... its code length in bytes
MAX-CLO TYPED-BUFFER CLO-REC ptr n   \ ... and the record that named it, or XREF-NULL
variable NCLO  variable CLO-CX
PTR-VARIABLE ROOTREC
variable CLO-LIMIT
: CLO-LIMIT! {: n:n :}
   n 1 < IF s" aot: CLO-LIMIT below 1" 74 die THEN
   n MAX-CLO > IF s" aot: CLO-LIMIT above MAX-CLO" 74 die THEN
   n CLO-LIMIT ! ;
MAX-CLO CLO-LIMIT!
: CLO-AT ( n -- ptr u8 ) CLO @ ;
: CLO-BYTES ( n -- n ) cells CLO-LEN + @ ;
: CLO-REC@ ( n -- ptr n ) CLO-REC @ ;
: IN-CLO? {: start:ptr :} ( ptr u8 -- bool )
   0 CLO-CX ! BEGIN CLO-CX @ NCLO @ < WHILE CLO-CX @ CLO-AT start = IF 0 0= exit THEN CLO-CX @ 1+ CLO-CX ! REPEAT 0 0= 0= ;
: CLO-OVERFLOW-JSON {: r:ptr :} ( ptr a -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-CLOSURE-LIMIT" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" reachable_count" AEJKEY NCLO @ AEJNUM 44 AE1
   s" max_closure" AEJKEY CLO-LIMIT @ AEJNUM 44 AE1
   s" root_word" AEJKEY ROOTREC @ AEJREC 44 AE1
   s" last_added_word" AEJKEY r AEJREC 44 AE1
   s" suggestion" AEJKEY
   s" split program, use --repl/snapshot, or raise MAX-CLO with a gate that proves the larger closure" AEJSTR
   125 AE1 10 AE1 ;
: CLO-OVERFLOW-PROSE {: r:ptr :} ( ptr a -- )
   s" aot: closure exceeds MAX-CLO reachable_count=" AETXT NCLO @ AEJNUM
   s"  max_closure=" AETXT CLO-LIMIT @ AEJNUM
   s"  root_word='" AETXT ROOTREC @ AEREC-TXT
   s" ' last_added_word='" AETXT r AEREC-TXT
   s" ' suggestion='split program, use --repl/snapshot, or raise MAX-CLO with a gate that proves the larger closure'" AETXT
   10 AE1 ;
: CLO-OVERFLOW-DIE {: r:ptr :} ( ptr a -- )
   JSON-DIAGS @ IF r CLO-OVERFLOW-JSON ELSE r CLO-OVERFLOW-PROSE THEN
   s" aot: closure exceeds MAX-CLO" 74 die ;
: ADD-CLO ( ptr n ptr u8 n -- ) {: r:ptr start:ptr len:n :}
   start IN-CLO? IF exit THEN
   NCLO @ CLO-LIMIT @ >= IF r CLO-OVERFLOW-DIE THEN
   start NCLO @ CLO !
   len NCLO @ cells CLO-LEN + !
   r NCLO @ CLO-REC !
   NCLO @ 1+ NCLO ! ;
PTR-VARIABLE SP2  PTR-VARIABLE SEND   \ a member's scan cursor and its one-past end
: ADD-REC-CLO ( ptr n -- ) {: r:ptr :}
   r  r REC-CODE-PTR@  r REC-BYTES  ADD-CLO ;
: ADD-SPAN-CLO ( n -- ) {: k:n :}
   XREF-NULL  k SPAN-START  k SPAN-BYTES  ADD-CLO ;
: SCAN-CALLEE ( ptr n ptr n -- ) {: caller:ptr callee:ptr :}
   callee XREF-FOUND? 0= if exit then
   callee AOT-UNSAFE? if caller callee AOT-UNSAFE-DIE then
   callee ADD-REC-CLO ;

\ Resolve a call target (a code address) to its record by EXACT code entry: scan the
\ dict records and match on the code-entry pointer directly (REC-CODE-PTR@) so a
\ direct-BL target needs no address-to-cell cast. Ordinary words and registered engine
\ helpers both carry a record; a non-entry address matches nothing (fails closed later).
: FINDADDR-PTR ( ptr u8 -- ptr n ) {: t:ptr :}  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC REC-CODE-PTR@ t = IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  XREF-NULL ;

\ Ticks may name engine-text entries; anonymous bodies can be interior to their
\ owner's emission. Use exact entry identity first, then the recorded span
\ (including the explicit full-span bit), never a nearest-address heuristic.
\ Namespace records hold wordlist IDs in those fields, not code spans.
: ADDRESS-OWNER ( n -- ptr n ) {: t:n :}
   t 3 and 0<> if XREF-NULL exit then
   ndict@ 0 ?do
      i REC REC-WID@ -1 <> if
         i REC @ t = if i REC unloop exit then
      then
   loop
   ndict@ 0 ?do
      i REC {: r:ptr :}
      r REC-WID@ -1 <> if
         t r @ >= if t r @ - r REC-BYTES < if r unloop exit then then
      then
   loop XREF-NULL ;

\ A cell holds a code/dict pointer iff its value lands in a LIVE engine extent the
\ dictionary records: the dict-record array [AOT-DBASE@, +ndict@*DREC) or the emitted
\ code span [AOT-DBASE@+DICT-SIZE, AOT-CP@). Both bounds are recorded live extents, so a
\ plain datum in free space (above the code high-water, or a free dict slot) is data and
\ the classification survives the code region moving near ordinary integer magnitudes. The
\ former [RBASE-VA, RBASE-VA+REGION) window was a MAGNITUDE HEURISTIC that misclassified any
\ datum in the 8 MiB window as a pointer (dot habu-identify-code-pointers-b973e6cc).
\ THE TWO CHECKS THAT MEET SUCH A CELL SHARE THIS ONE PREDICATE: the span scan in
\ aot-lib.f, which reads every cell the capture window covers, and DATA-ADDRESS!
\ below, which meets the same kind of cell through a recorded address naming one
\ the window never opened over. They must not answer differently.
\ THE TWO EXTENTS ARE ASKED FOR SEPARATELY, because the stripped image answers
\ for them differently: a code address a declared cell holds is relocated against
\ this image's own code base, and a dictionary-record address is refused however
\ it was declared - the image carries no records for one to point at.
: CELL-DICTPTR? ( n -- bool ) {: v:n :}                                      \ in live dict records
   v AOT-DBASE-N >=  v AOT-DBASE-N ndict@ DREC * + < and ;
: CELL-CODEPTR? ( n -- bool ) {: v:n :}                                      \ in emitted code
   v AOT-DBASE-N DICT-SIZE + >=  v AOT-CP-N < and ;
: CELL-TEXTPTR? ( n -- bool ) {: v:n :}     \ code/dict pointer, by live extents (not magnitude)
   v CELL-DICTPTR? IF true EXIT THEN
   v CELL-CODEPTR? ;

\ A DATA address as a pointer. An address arrives here as a plain integer (the
\ value a recorded chain spells out, a scan cursor, a span bound), and DATA is
\ ONE mapping based at data-base, so the pointer is that base plus the checked
\ offset. THIS IS THE ONLY PLACE IN THE LINKER WHERE A DATA ADDRESS BECOMES A
\ POINTER AGAIN - the cell read below and the blob byte reads in aot-lib.f both
\ come through here, so no recorded value reaches DATA by a route of its own.
\ Every caller bounds-checks first - DATA-CELL? for a recorded value,
\ XTC-IN-WINDOW? for a declared cell, the span bounds for the scan and the copy -
\ so the offset is inside the mapping.
: DATA-PTR ( n -- ptr u8 ) {: at:n :}
   data-base BYTE-VIEW at DATA-VA VA>N - + ;

\ The cell at a DATA address. The span scan in aot-lib.f reads its cells through
\ this word too, so the two checks that meet such a cell cannot read one
\ differently.
: DATA-CELL@ ( n -- n ) DATA-PTR CELL-VIEW @ ;

\ A WHOLE cell at this address is inside the mapped DATA region, so reading it to
\ see what it holds cannot fault. DATA-ADDRESS? admits the region's one-past end,
\ which is a valid address to relocate and not a readable cell.
: DATA-CELL? ( n -- bool ) {: v:n :}
   v DATA-VA VA>N >= v 8 + DATA-VA VA>N DATA-SIZE + <= and ;

\ Does this record's code spell out that DATA address? `variable`, `create` and
\ `defer` each compile a recorded address chain for their own body, so the site is
\ the structural link from a persistent cell back to the word that owns it. A cell
\ reached by arithmetic from some other body has no site of its own and stays
\ unnamed rather than being attributed to the nearest word below it.
: REC-CELL-SITE? ( ptr n n -- bool ) {: r:ptr v:n :}
   r REC-CODE-PTR@ {: p:ptr :}
   r REC-BYTES ADDR-CHAIN-BYTES - 4 / 1+ 0 max 0 ?do
      p i 4 * + ADDRESS-SITE? if
         p i 4 * +  p i 4 * + ADDR-CHAIN-BYTES + ADDRESS-CHAIN? if
            p i 4 * + CHAINV v = if true unloop exit then
         then
      then
   loop false ;

\ Namespace records hold wordlist IDs where a word record holds its code span.
: DATA-CELL-OWNER ( n -- ptr n ) {: v:n :}
   ndict@ 0 ?do
      i REC REC-WID@ -1 <> if
         i REC v REC-CELL-SITE? if i REC unloop exit then
      then
   loop XREF-NULL ;

\ A PERSISTENT CELL THE STRIPPED IMAGE CANNOT CARRY, named the way dot
\ habu-name-the-cell-740feb52 settled it: the word whose data the cell is, the
\ cell's DATA offset and the value it holds. Only the reason and the suggestion
\ differ between the cases below, so those travel as arguments and every case
\ renders through this one pair of writers - a program is edited by the three
\ facts, and they must not depend on which check met the cell.
: DATA-CELL-JSON ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY cell DATA-CELL-OWNER AEJREC 44 AE1
   s" data_off" AEJKEY cell DATA-VA VA>N - AEJNUM 44 AE1
   s" value" AEJKEY v AEJNUM 44 AE1
   s" reason" AEJKEY ra ru AEJSTR 44 AE1
   s" suggestion" AEJKEY sa su AEJSTR
   125 AE1 10 AE1 ;

\ The suggestion travels in the prose rendering too, the way CLO-OVERFLOW-PROSE
\ carries its own: the person reading stderr is the one who has to edit the
\ program, and which form to reach for is the whole answer for a declared cell.
: DATA-CELL-PROSE ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   s" hb-build: " AETXT  ra ru AETXT
   s"  word=" AETXT cell DATA-CELL-OWNER AEREC-TXT
   s"  data-off=" AETXT cell DATA-VA VA>N - AEJNUM
   s"  value=" AETXT v AEJNUM
   s"  suggestion='" AETXT sa su AETXT 39 AE1
   10 AE1 ;

: REFUSE-DATA-CELL ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   JSON-DIAGS @ IF cell v ra ru sa su DATA-CELL-JSON ELSE cell v ra ru sa su DATA-CELL-PROSE THEN
   s" hb-build: AOT unsupported persistent data" 70 die ;

\ A code pointer in a cell NOTHING DECLARED. The image relocates a declared xt
\ cell (the table below), and a declaration is the only authority for reading a
\ cell as a code address: an ordinary integer can hold any value at all, so a
\ value-range guess would rewrite a datum that merely looks like an address.
\ `defer`, `is` and `xt!` declare; `' word ,` stores an untyped cell and does not.
: REFUSE-UNDECLARED-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data holds an undeclared code/dict pointer"
   s" a stripped image relocates only DECLARED xt cells (defer/is/xt!); ' word , declares nothing - bind the cell with a defer or xt!, or use --repl"
   REFUSE-DATA-CELL ;

\ A dictionary-record pointer, declared or not. The image ships code, never
\ records, so there is nothing for this value to be rebased onto.
: REFUSE-DICT-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data holds a dictionary-record pointer"
   s" a stripped image carries no dictionary records; remove the record pointer from persistent data, or use --repl"
   REFUSE-DATA-CELL ;

\ A declared xt cell whose value is not a word's code at all.
: REFUSE-XT-TARGET ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT declared xt cell holds an address no word in the image owns"
   s" the cell is declared to hold an execution token; store a real token in it, or use --repl"
   REFUSE-DATA-CELL ;

\ A cell below the capture window: a preloaded module's data, which the stripped
\ image never restores. The declaration cannot help a cell that is not there.
: REFUSE-UNRESTORED-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data outside the restored span holds a code/dict pointer"
   s" the image restores only the program's own DATA window; define the cell in the program's own source, or use --repl"
   REFUSE-DATA-CELL ;

\ A genuine data pointer the restored span does not cover. Named the way
\ REFUSE-ADDRESS-SITE names its site - the owning word and the region offset of
\ the recorded cell - plus the value, the word whose data that value is, and the
\ span it had to fall in. Without the target name the value is an address the
\ program has no way to recognise.
: REFUSE-DATA-SPAN ( ptr n ptr u8 n -- ) {: owner:ptr site:ptr v:n :}
   s" aot: address refers to data outside the restored span caller=" AETXT
   owner AEREC-TXT
   s"  region-off=" AETXT site AOT-DBASE@ BYTE-VIEW - AEJNUM
   s"  value=" AETXT v AEJNUM
   s"  target=" AETXT v DATA-CELL-OWNER AEREC-TXT
   s"  span=[" AETXT BLOB-SRC @ AEJNUM
   44 AE1 BLOB-END @ AEJNUM
   93 AE1 10 AE1
   s" " 74 die ;

\ The cell this address names, refused as unsupported persistent data when it
\ holds a code or dictionary pointer - named with the same three facts, from the
\ same predicate, that the span scan reaches such a cell with. The reason differs
\ because the fault does: this cell is outside the span the image restores, so
\ even a declared xt cell has nothing to be patched in.
: CHECK-DATA-CELL ( n -- ) {: cell:n :}
   cell DATA-CELL@ {: v:n :}
   v CELL-TEXTPTR? if cell v REFUSE-UNRESTORED-CELL then ;

\ AN ENGINE RUNTIME CELL THE STRIPPED ENTRY OWNS, ADMITTED BY ITS DECLARATION.
\ src/habu/aot-owned-cells.f is the list; a cell is on it because it is named
\ there. The value has to BE a claimed cell's address - one that merely lands in
\ the engine's DATA below the window, or in the same file as a claimed cell, is
\ refused exactly as before. Relocation preserves such an address because DATA is
\ one MAP_FIXED mapping at DATA-VA in the engine and in the image alike, and
\ src/habu/aot-lib.f EMIT-OWNED-CELLS reads this same table to publish what each
\ claim declares - so the two cannot disagree about one cell.
\ A CARRIED claim is not owned in place - its bytes move - so its base address is
\ admitted by the mapping below and never by this predicate.
: OWNED-AT? ( n n -- bool ) {: i:n v:n :}
   i AOT-OWNED:CARRIED? if false exit then
   v i AOT-OWNED:AT = ;

: OWNED-CELL? ( n -- bool ) {: v:n :}
   AOT-OWNED:N 0 ?do
      i v OWNED-AT? if true unloop exit then
   loop false ;

\ AN ADDRESS INSIDE A CARRIED CLAIM'S BYTES NAMES THE COPY. src/habu/aot-lib.f
\ CARRY-CELLS has already copied [cell, cell+length) into the window's carried run
\ and recorded where, so the image's code reads the bytes it restores and not the
\ engine DATA it was compiled against. The interior offset is preserved, which is
\ what makes the mapping answer for `KK i cells +` as well as for `KK` - the whole
\ declared range maps, one address at a time, through the same target the closure
\ walk hands a re-interned literal.
: CARRIED-IN? ( n n -- bool ) {: i:n v:n :}
   i AOT-OWNED:CARRIED? 0= if false exit then
   v i AOT-OWNED:AT >=  v i AOT-OWNED:AT i AOT-OWNED:LEN + < and ;

: CARRIED-TARGET ( n -- n ) {: v:n :}                 \ the copy's address, or -1
   AOT-OWNED:N 0 ?do
      i v CARRIED-IN? if i AOT-OWNED:DEST v i AOT-OWNED:AT - + unloop exit then
   loop -1 ;

: DATA-ADDRESS! ( ptr n ptr u8 n -- ) {: owner:ptr site:ptr v:n :}
   \ The end is a valid one-past pointer for a zero-length buffer. Relocation
   \ preserves the address; it does not certify a later memory access.
   v BLOB-SRC @ >= v BLOB-END @ <= and if exit then
   v OWNED-CELL? if exit then
   v DATA-CELL? if v CHECK-DATA-CELL then
   owner site v REFUSE-DATA-SPAN ;

\ A carried cell's declared bytes and an immutable literal row can both be copied
\ into the window - the first into the carried run by name, the second into the
\ capture's active pool - and the address is rewritten to the copy either way.
\ Other pre-window DATA has no ownership proof and must keep the link refusal.
: DATA-TARGET ( ptr n ptr u8 n -- n ) {: owner:ptr site:ptr v:n :}
   v BLOB-SRC @ >= v BLOB-END @ <= and if v exit then
   v CARRIED-TARGET {: c:n :}
   c 0 >= if c exit then
   v NSTR:REINTERN-OWNED drop {: w:n :}
   owner site w DATA-ADDRESS!  w ;

\ ---- the program's DECLARED xt cells -------------------------------------------
\ A PERSISTED CELL HOLDS A CODE ADDRESS BECAUSE IT WAS DECLARED TO, never because
\ its value looks like one. The engine's address-cell table (src/habu/layout.f
\ package SNAP-RELOC, src/habu/address-cells.f) is where `defer` registers a
\ dispatch cell, `is` the cell it stores into and `xt!` a cell a checked word
\ worked out at run time - the same authority the snapshot writer and the AOT
\ capture relocate from, and the same reason both of them refuse to scan DATA for
\ values in an address band. This pass reads the XT-kind rows that fall inside the
\ capture window; the two things it answers are what the span scan must NOT refuse
\ and what the image must carry.
\ A row whose value is zero is an unbound cell: the sparse copy restores the zero
\ and there is nothing to relocate. A DATA-kind row needs nothing either - DATA is
\ mapped MAP_FIXED at DATA-VA and the window is restored to the same addresses.
1024 constant MAX-XTCELL
create XTC-OFF MAX-XTCELL cells allot     \ each declared cell's DATA offset
create XTC-VAL MAX-XTCELL cells allot     \ ... and the code address it holds
variable XTC-N  variable XTC-CX  variable XTC-I  variable XTC-J

: XTC-OFF@ ( n -- n ) cells XTC-OFF + @ ;
: XTC-VAL@ ( n -- n ) cells XTC-VAL + @ ;
: XTC-CELL ( n -- n ) XTC-OFF@ DATA-VA VA>N + ;
: XTC-SET ( n n n -- ) {: k:n off:n v:n :}
   off k cells XTC-OFF + !  v k cells XTC-VAL + ! ;

\ Ascending by DATA offset, which is the order the span scan meets cells in, so
\ its membership test is a cursor rather than a search. Rows arrive in
\ declaration order; insertion sort keeps the two arrays in step with no scratch.
: XTC-PLACE ( n -- ) {: i:n :}
   i XTC-OFF@ {: off:n :}
   i XTC-VAL@ {: v:n :}
   i XTC-J !
   BEGIN XTC-J @ 0 > IF XTC-J @ 1 - XTC-OFF@ off > ELSE false THEN WHILE
      XTC-J @  XTC-J @ 1 - XTC-OFF@  XTC-J @ 1 - XTC-VAL@  XTC-SET
      XTC-J @ 1 - XTC-J !
   REPEAT
   XTC-J @ off v XTC-SET ;

: XTC-SORT ( -- )
   1 XTC-I ! BEGIN XTC-I @ XTC-N @ < WHILE
      XTC-I @ XTC-PLACE  XTC-I @ 1+ XTC-I !
   REPEAT ;

: XTC+ ( n n -- ) {: off:n v:n :}
   XTC-N @ MAX-XTCELL >= IF
      s" aot: more declared xt cells in the data window than MAX-XTCELL" 74 die THEN
   XTC-N @ off v XTC-SET
   XTC-N @ 1+ XTC-N ! ;

\ The WHOLE cell is inside the span the image restores. A declared cell that
\ straddles the window's end is not one this image can carry, and it keeps the
\ unrestored-cell refusal the span bound already gives such a cell.
: XTC-IN-WINDOW? ( n -- bool ) {: at:n :}
   at BLOB-SRC @ >= at 8 + BLOB-END @ <= and ;

: XTC-ROW ( n -- ) {: k:n :}
   k ADDRESS-CELLS:ROW@ {: raw:n :}
   raw XTCELL-DATA-TAG and 0<> IF exit THEN
   raw XTCELL-OFF-MASK and {: off:n :}
   DATA-VA VA>N off + {: at:n :}
   at XTC-IN-WINDOW? 0= IF exit THEN
   at DATA-CELL@ {: v:n :}
   v 0= IF exit THEN
   v CELL-DICTPTR? IF at v REFUSE-DICT-CELL THEN
   off v XTC+ ;

: COLLECT-XT-CELLS ( -- )
   0 XTC-N !  0 XTC-CX !
   ADDRESS-CELLS:LIVE-SPAN {: base:ptr rows:n :}
   rows 0 ?do i XTC-ROW loop
   XTC-SORT ;

\ Does a declared cell overlap the eight bytes at this address? The span scan asks
\ once per cell in ascending order, so the cursor only moves forward. Containment
\ is the declaration's whole rule (layout.f XTCELL-OFF-MAX: a cell reached through
\ `allot` after byte data keeps its residue), so this is an overlap test and not
\ an equality one.
: XTC-REWIND ( -- ) 0 XTC-CX ! ;
: XTC-PASSED? ( n -- bool ) {: at:n :}
   XTC-CX @ XTC-N @ >= IF false EXIT THEN
   XTC-CX @ XTC-CELL 8 + at <= ;
: XTC-DECLARED? ( n -- bool ) {: at:n :}
   BEGIN at XTC-PASSED? WHILE XTC-CX @ 1+ XTC-CX ! REPEAT
   XTC-CX @ XTC-N @ >= IF false EXIT THEN
   XTC-CX @ XTC-CELL at 8 + < ;

: REFUSE-ADDRESS-SITE ( ptr n ptr u8 ptr u8 -- ) {: caller:ptr p:ptr e:ptr :}
   s" aot: malformed recorded address chain caller=" AETXT
   caller AEJREC
   s"  region-off=" AETXT p AOT-DBASE@ BYTE-VIEW - AEJNUM
   s"  words=" AETXT
   e p - 4 / 4 min 0 max 0 ?do
      i 0<> if 32 AE1 then
      p i 4 * + AOT-W32@ AEJNUM
   loop
   10 AE1 s" " 74 die ;

: SCAN-ADDRESS ( ptr n ptr u8 ptr u8 -- ) {: caller:ptr p:ptr e:ptr :}
   p ADDRESS-SITE? 0= if exit then
   p e ADDRESS-CHAIN? 0= if caller p e REFUSE-ADDRESS-SITE then
   p CHAINV {: v:n :}
   v DATA-ADDRESS? if
      caller p v DATA-TARGET drop exit
   then
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if caller owner SCAN-CALLEE exit then
   v SPAN-OWNER {: k:n :}
   k 0 < if s" aot: code address has no dictionary owner" 74 die then
   k ADD-SPAN-CLO ;

\ A BRANCH OUT OF THE MEMBER IS A CALL for the closure's purposes, so a direct
\ branch - BL or B alike - whose target lies outside the member being scanned is
\ followed to its callee, and one whose target lands back inside that member is
\ intra-record control flow (a loop, an `if` arm, a refusal's jump) and is left
\ alone. Two emitters plant the outward B, and both aim at a callee's entry base:
\ habu2.f DOESPATCH:EMIT overwrites a created word's RET with a `b` to its
\ parent's `;does` companion record (elaborate.f DOES-NAME), and the native
\ compiler's tail call (emit.f PUT-TAILCALL) leaves through the callee. Either
\ way the target has to join the closure or aot-lib.f MAP-TARGET! refuses the
\ branch it cannot rewrite. Conditional and compare branches are not DIRECT? and
\ are never followed.
\ A callee the image ships no record for answers from the span table instead,
\ and an entry neither table knows is left where the relocation pass will refuse
\ it by name.
: SCAN-DIRECT ( ptr n ptr u8 ptr u8 ptr u8 -- ) {: caller:ptr p:ptr mstart:ptr mend:ptr :}
   p AOT-W32@ dup DIRECT? 0= if drop exit then
   p swap TARGET {: t:ptr :}
   t mstart >= t mend < and if exit then
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? if caller callee SCAN-CALLEE exit then
   t SPAN-AT-ENTRY {: k:n :}
   k 0 >= if k ADD-SPAN-CLO then ;

: SCAN-MEMBER {: i:n :} ( n -- )
   i CLO-AT SP2 !  i CLO-AT i CLO-BYTES + SEND !
   BEGIN SP2 @ SEND @ < WHILE
      i CLO-REC@ SP2 @ SEND @ SCAN-ADDRESS
      i CLO-REC@ SP2 @ i CLO-AT SEND @ SCAN-DIRECT
      SP2 @ 4 + SP2 !
   REPEAT ;
variable WI
: NO-ENTRY-DIE ( -- )
   s" aot: entry word not found: " AETXT  ENTRY-NAME$ AETXT  10 AE1
   s" aot: no entry" 74 die ;

\ A record's code span as integers, the domain interior addresses arrive in, and
\ the code pointer for one address inside it. A record carries its entry as a
\ pointer and the walk carries addresses as integers, so the crossing happens
\ here, once, as the entry plus the address's distance from it.
\ The crossing is the dictionary base - the one address published in both domains
\ (AOT-DBASE@ and AOT-DBASE-N above) - plus the distance from it. The emitted
\ code is mapped above that base, so this is checked arithmetic and not a cast,
\ and the closure table's entries cross here too (aot-lib.f CLO-AT-N).
: CODE-N ( ptr u8 -- n ) {: p:ptr :}
   p AOT-DBASE@ BYTE-VIEW -  AOT-DBASE-N + ;
: REC-ENTRY-N ( ptr n -- n ) {: r:ptr :}
   r REC-CODE-PTR@ CODE-N ;
: REC-END-N ( ptr n -- n ) {: r:ptr :}  r REC-ENTRY-N r REC-BYTES + ;
: CODE-AT ( ptr n n -- ptr u8 ) {: r:ptr v:n :}
   r REC-CODE-PTR@  v r REC-ENTRY-N -  + ;

\ The same crossing for an address whose owner is not known yet: through the
\ record that names it, or through the payload's span table for a word the image
\ ships no record for. An address neither can place is refused here rather than
\ turned into a pointer to a guess.
: CODE-PTR ( n -- ptr u8 ) {: v:n :}
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if owner v CODE-AT exit then
   v SPAN-OWNER {: s:n :}
   s 0 < if s" aot: code address has no dictionary owner" 74 die then
   s SPAN-START  v s SPAN-START-N -  + ;

\ WHERE AN ANONYMOUS BODY ENDS. One emission is the word's own function followed
\ by its quotations' functions, laid out in order (src/compiler/native/emit.f
\ FUNCTION-OFFSET@), and one record covers all of them - so an anonymous body
\ ends where the next function begins, and the last one ends with the record.
\ THE NEXT FUNCTION IS FOUND BY THE ADDRESS MAP: the compiler records every
\ literal it decides IS a code address (src/habu/layout.f ADDRMAP-OFF, the
\ quotation entry a `[: ;]` pushes among them), so the next entry above this one
\ is the smallest recorded chain value above it inside this record. Nothing here
\ decides what a body looks like by decoding bytes.
variable BODY-END
: BODY-END-SCAN ( ptr n n -- ) {: r:ptr v:n :}
   r REC-END-N BODY-END !
   r REC-CODE-PTR@ {: p:ptr :}
   r REC-BYTES ADDR-CHAIN-BYTES - 4 / 1+ 0 max 0 ?do
      p i 4 * + ADDRESS-SITE? if
         p i 4 * +  p i 4 * + ADDR-CHAIN-BYTES + ADDRESS-CHAIN? if
            p i 4 * + CHAINV dup v > over BODY-END @ < and if BODY-END ! else drop then
         then
      then
   loop ;

\ The anonymous body at v, as a member in its own right. The word that DEFINED it
\ is not pulled in with it: a `[: ... ;] is X` initializer runs at load time and
\ its own code declares an address cell, which a stripped image has no registrar
\ for - the image needs the quotation, not the word that bound it. The record
\ still travels with the member, because it is what a diagnostic has to say about
\ a body: no record NAMES it, and the word it was compiled inside is the only
\ thing a program can be edited by. Nothing resolves an address through it.
: ADD-BODY-CLO ( ptr n n -- ) {: r:ptr v:n :}
   r v BODY-END-SCAN
   r  r v CODE-AT  BODY-END @ v -  ADD-CLO ;

\ A DECLARED XT CELL IS A CLOSURE ROOT. Its value is resolved exactly the way
\ SCAN-ADDRESS resolves a code literal met inside a word - the record whose ENTRY
\ it is, else the record whose span CONTAINS it (an anonymous body, added above
\ without the word that defined it), else the payload's span table. The target
\ joins the closure BEFORE the walk below, so the strip cannot drop the code a
\ persistent cell points at, and a target no table can place is refused by name
\ rather than relocated to a guess.
: XT-CELL-ROOT ( n -- ) {: k:n :}
   k XTC-VAL@ {: v:n :}
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if
      v owner REC-ENTRY-N = if
         owner AOT-UNSAFE? if k XTC-CELL DATA-CELL-OWNER owner AOT-UNSAFE-DIE then
         owner ADD-REC-CLO exit
      then
      owner v ADD-BODY-CLO exit
   then
   v SPAN-OWNER {: s:n :}
   s 0 < if k XTC-CELL v REFUSE-XT-TARGET then
   s ADD-SPAN-CLO ;

: XT-CELL-ROOTS ( -- )  XTC-N @ 0 ?do i XT-CELL-ROOT loop ;

: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop NO-ENTRY-DIE THEN  dup ROOTREC !  ADD-REC-CLO
   XT-CELL-ROOTS
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ SCAN-MEMBER  WI @ 1+ WI ! REPEAT ;

;using
;package
