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
\ THE ONE RECORD A CALL IS DROPPED TO RATHER THAN FOLLOWED. (MARK) is the
\ address-cell registrar (habu2.f EMIT-MARK): `xt!` stores the token into the
\ cell and BLs it to DECLARE that cell as holding a code address, for readers a
\ stripped image does not have - the snapshot writer, the loader's relocation
\ pass (habu2.f EMIT-XT) and the AOT capture (aot-capture.f) all read the
\ address-cell table, and a stripped image's only restore pass is the
\ linker-computed EMIT-XT-CELLS (aot-lib.f). The registrar would die on the zero
\ address-cell header of its fresh DATA in any case (habu2.f MARK-HEADER). So
\ the declaration half of `xt!` has nothing to declare to in such an image and
\ is dropped at link (aot-lib.f RELOC-W32 writes a NOP), while the store half -
\ BSTORE's store, protection guard and all - is carried unchanged. A
\ declaration-only record is never a closure member, so the walk below does not
\ follow the branch. The rule is keyed on the TARGET being the registrar, never
\ on the name of the member that calls it.
: AOT-DECLARATION? {: r:ptr :} ( ptr a -- bool )
   r s" (MARK)" REC-NAME= ;
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
\ COPY/RELOCATE arrays (NEWOFF/BLEN) are sized from the program being linked;
\ ADD-CLO fails closed at that capacity so a walk can never write past the
\ tables, and no constant decides which programs can be stripped.
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
\ THE COLUMNS ARE DYNAMIC-BUFFERS because their capacity is a runtime number:
\ the tables are sized from the program being linked (CLO-CAPACITY and CLO-TABLES
\ below, at the head of the walk), not cut to a constant. The definer's control
\ head is a declared pointer cell, so the two address columns keep their pointer
\ types - raw `create ... cells allot` storage may hold no address at all. The
\ heads are allotted above BLOB-END like every other linker cell and the mappings
\ register in the host's DYNAMIC-STORAGE registry, which a stripped image
\ publishes FRESH (src/habu/aot-owned-cells.f), so a link-time table reaches
\ neither the captured window nor the image.
DYNAMIC-BUFFER CLO ptr u8      \ each member's code entry
DYNAMIC-BUFFER CLO-LEN n       \ ... its code length in bytes
DYNAMIC-BUFFER CLO-REC ptr n   \ ... and the record that named it, or XREF-NULL
variable NCLO  variable CLO-CX
PTR-VARIABLE ROOTREC
variable CLO-CAP     \ rows allocated, 0 until the tables are sized
variable CLO-REQ     \ a limit a program lowered by hand, or 0 for the capacity
variable CLO-LIMIT   \ ... and the one ADD-CLO enforces, resolved at the sizing
: CLO-AT ( n -- ptr u8 ) CLO @ ;
: CLO-BYTES ( n -- n ) CLO-LEN @ ;
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
   s" split the program into smaller entry points, or run it with --repl or a snapshot image" AEJSTR
   125 AE1 10 AE1 ;
: CLO-OVERFLOW-PROSE {: r:ptr :} ( ptr a -- )
   s" aot: closure exceeds the closure limit reachable_count=" AETXT NCLO @ AEJNUM
   s"  max_closure=" AETXT CLO-LIMIT @ AEJNUM
   s"  root_word='" AETXT ROOTREC @ AEREC-TXT
   s" ' last_added_word='" AETXT r AEREC-TXT
   s" ' suggestion='split the program into smaller entry points, or run it with --repl or a snapshot image'" AETXT
   10 AE1 ;
\ The tables are sized from the program, so a closure cannot outgrow them by
\ construction: this refusal is reachable only where CLO-LIMIT! lowered the limit
\ below the capacity, and the suggestion says what a user can do about a program
\ that is genuinely too large rather than naming a number to raise.
: CLO-OVERFLOW-DIE {: r:ptr :} ( ptr a -- )
   JSON-DIAGS @ IF r CLO-OVERFLOW-JSON ELSE r CLO-OVERFLOW-PROSE THEN
   s" aot: closure exceeds the closure limit" 74 die ;
: ADD-CLO ( ptr n ptr u8 n -- ) {: r:ptr start:ptr len:n :}
   start IN-CLO? IF exit THEN
   NCLO @ CLO-LIMIT @ >= IF r CLO-OVERFLOW-DIE THEN
   start NCLO @ CLO !
   len NCLO @ CLO-LEN !
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

\ THE RECORD WHOSE CODE STARTS NEAREST BELOW AN ADDRESS - A READING AID, NEVER
\ OWNERSHIP. ADDRESS-OWNER above stays exact-entry-then-recorded-span, because a
\ walk that guessed would hand one word's code to another and the closure would
\ carry the wrong bytes. A REFUSAL is read by a person who has to find the
\ instruction, and the site of a span refusal is often code no record names at
\ all: an engine word whose name the build stripped arrives as a span member with
\ XREF-NULL for its record (ADD-SPAN-CLO), and `caller=<unknown>` leaves the
\ reader nothing to open. So the span refusal - and only it - names the record
\ that starts closest below the site and spells the distance in bytes,
\ `NAME+off`: the `+off` is what says this is the neighbour above which the site
\ lies and not the word that owns it. Namespace records hold wordlist ids where a
\ word record holds its code span, so they are skipped here as they are above.
variable NB-IX
: CODE-NEIGHBOUR ( ptr u8 -- ptr n ) {: site:ptr :}
   -1 NB-IX !
   ndict@ 0 ?do
      i REC {: r:ptr :}
      r REC-WID@ -1 <> if
         site r REC-CODE-PTR@ >= if
            NB-IX @ 0 < if i NB-IX ! else
               r REC-CODE-PTR@ NB-IX @ REC REC-CODE-PTR@ > if i NB-IX ! then
            then
         then
      then
   loop
   NB-IX @ 0 < if XREF-NULL exit then
   NB-IX @ REC ;

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

\ The linker writes a window cell in exactly two places, and both write bytes the
\ image is about to capture: src/habu/aot-lib.f CARRY-CELLS copies a carried
\ claim into the carried run, and XTD-ROW below stores a declared DATA cell's
\ mapped value into the cell itself. The blob is read out of live DATA after
\ both, so the image ships what they wrote.
: DATA-CELL! ( n n -- ) {: at:n v:n :} v at DATA-PTR CELL-VIEW ! ;

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

\ THE GREATEST DATA ADDRESS THIS RECORD'S CODE SPELLS AT OR BELOW A VALUE, or -1
\ for a record that spells none. The same chain walk REC-CELL-SITE? makes, asked
\ the other question: not "is this the cell" but "how close from below does this
\ record get", which is the only name an address NO record spells exactly can be
\ given. A chain that spells a code address is not an answer here - the question
\ is which data the value is interior to - so the value domain is checked before
\ the distance.
variable CB-AT
: REC-CELL-BELOW ( ptr n n -- n ) {: r:ptr v:n :}
   -1 CB-AT !
   r REC-CODE-PTR@ {: p:ptr :}
   r REC-BYTES ADDR-CHAIN-BYTES - 4 / 1+ 0 max 0 ?do
      p i 4 * + ADDRESS-SITE? if
         p i 4 * +  p i 4 * + ADDR-CHAIN-BYTES + ADDRESS-CHAIN? if
            p i 4 * + CHAINV {: w:n :}
            w DATA-ADDRESS?  w v <= and  w CB-AT @ > and if w CB-AT ! then
         then
      then
   loop CB-AT @ ;

\ ... and the record that gets nearest, with the address it spells. The span
\ refusal names an unspelled target `NAME+off` from this pair, for the same
\ reason CODE-NEIGHBOUR names its site: a buffer reached at a field offset, or a
\ buffer whose own record the build stripped, is data with no exact spelling
\ anywhere and `target=<unknown>` says nothing a program can be edited by. The
\ CELL refusals keep DATA-CELL-OWNER's exact answer - they name the word whose
\ data the cell IS, which is a claim about ownership and not a direction to look
\ in.
variable DN-IX  variable DN-AT
: DATA-NEIGHBOUR ( n -- ptr n n ) {: v:n :}
   -1 DN-IX !  -1 DN-AT !
   ndict@ 0 ?do
      i REC {: r:ptr :}
      r REC-WID@ -1 <> if
         r v REC-CELL-BELOW {: w:n :}
         w DN-AT @ > if w DN-AT !  i DN-IX ! then
      then
   loop
   DN-IX @ 0 < if XREF-NULL -1 exit then
   DN-IX @ REC DN-AT @ ;

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

\ A DECLARED DATA CELL HOLDING AN ENGINE ADDRESS NO CLAIM NAMES. The cell itself
\ is inside the window and travels; the address it holds is engine DATA the image
\ never restores, so the image would read the fresh mapping's zero through it.
\ The cell is named rather than a code site, because the fault is the value the
\ program stored and not an instruction the compiler emitted.
: REFUSE-UNCLAIMED-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT declared data cell holds an engine address outside the restored span"
   s" the image restores only the window this program owns and the cells src/habu/aot-owned-cells.f claims; take the address at run time, claim the target carried, or use --repl"
   REFUSE-DATA-CELL ;

\ A genuine data pointer the restored span does not cover. Named the way
\ REFUSE-ADDRESS-SITE names its site - the owning word and the region offset of
\ the recorded cell - plus the value, the word whose data that value is, and the
\ span it had to fall in. Without the target name the value is an address the
\ program has no way to recognise.
\ THE SITE, NAMED EVEN WHERE NO RECORD NAMES IT. The record that named the member
\ answers when there is one; otherwise the neighbour below it does, as `NAME+off`.
: CALLER-TXT ( ptr n ptr u8 -- ) {: owner:ptr site:ptr :}
   owner XREF-FOUND? if owner AEREC-TXT exit then
   site CODE-NEIGHBOUR {: near:ptr :}
   near XREF-FOUND? 0= if near AEREC-TXT exit then
   near AEREC-TXT  43 AE1  site near REC-CODE-PTR@ - AEJNUM ;

\ ... and the target the same way: the word whose data the value is when a record
\ spells it exactly, else the nearest spelled data below it as `NAME+off`. A
\ value no record spells at all - nothing in the dictionary reaches that low -
\ stays `<unknown>`, which is then the whole truth about it.
: TARGET-TXT ( n -- ) {: v:n :}
   v DATA-CELL-OWNER {: exact:ptr :}
   exact XREF-FOUND? if exact AEREC-TXT exit then
   v DATA-NEIGHBOUR {: near:ptr at:n :}
   near XREF-FOUND? 0= if near AEREC-TXT exit then
   near AEREC-TXT  43 AE1  v at - AEJNUM ;

: REFUSE-DATA-SPAN ( ptr n ptr u8 n -- ) {: owner:ptr site:ptr v:n :}
   s" aot: address refers to data outside the restored span caller=" AETXT
   owner site CALLER-TXT
   s"  region-off=" AETXT site AOT-DBASE@ BYTE-VIEW - AEJNUM
   s"  value=" AETXT v AEJNUM
   s"  target=" AETXT v TARGET-TXT
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

\ THE SPAN THE IMAGE RESTORES, as a test on a value. The end is a valid one-past
\ address for a zero-length buffer, so the bound admits it; relocation preserves
\ an address and does not certify a later memory access.
: IN-WINDOW? ( n -- bool ) {: v:n :}
   v BLOB-SRC @ >= v BLOB-END @ <= and ;

\ A CLAIM'S DECLARED BYTES, WHATEVER ITS KIND. src/habu/aot-owned-cells.f is the
\ list; a cell is on it because it is NAMED there, and the length it declares is
\ its extent: [cell, cell + length), one cell for the kinds that name a single
\ cell and the declared span for a carried table or a fresh buffer. A BUFFER IS
\ REACHED AT AN INTERIOR OFFSET AS OFTEN AS AT ITS HEAD - src/core/util.f PATHZ
\ writes the NUL at PZB + u - so a claim that admitted only its first address
\ would refuse the same buffer one instruction later. A value that merely lands
\ in the engine's DATA below the window, or in the same file as a claimed cell,
\ or in the bytes NEXT to one, is refused exactly as before: with PZB claimed
\ for PATH-CAP + 1 bytes, a program spelling PZB + 8 links and one spelling RDP
\ - util.f's next record, at PZB + 1032 - is still refused by that name.
\ Relocation preserves
\ a claimed address because DATA is one MAP_FIXED mapping at DATA-VA in the
\ engine and in the image alike, and src/habu/aot-lib.f EMIT-OWNED-CELLS reads
\ this same table to publish what each claim declares - so the two cannot
\ disagree about one cell.
\ ONE PREDICATE FOR EVERY ROAD BELOW, because the carried map answers an address
\ with the copy and the refusal check must admit exactly what the map answered
\ for.
: CLAIMED-AT? ( n n -- bool ) {: i:n v:n :}
   v i AOT-OWNED:AT >=  v i AOT-OWNED:AT i AOT-OWNED:LEN + < and ;

\ AN ADDRESS INSIDE A CARRIED CLAIM'S BYTES NAMES THE COPY. src/habu/aot-lib.f
\ CARRY-CELLS has already copied [cell, cell+length) into the window's carried run
\ and recorded where, so the image's code reads the bytes it restores and not the
\ engine DATA it was compiled against. The interior offset is preserved, which is
\ what makes the mapping answer for `KK i cells +` as well as for `KK` - the whole
\ declared range maps, one address at a time, through the same target the closure
\ walk hands a re-interned literal.
: CARRIED-IN? ( n n -- bool ) {: i:n v:n :}
   i AOT-OWNED:CARRIED? 0= if false exit then
   i v CLAIMED-AT? ;

: CARRIED-TARGET ( n -- n ) {: v:n :}                 \ the copy's address, or -1
   AOT-OWNED:N 0 ?do
      i v CARRIED-IN? if i AOT-OWNED:DEST v i AOT-OWNED:AT - + unloop exit then
   loop -1 ;

\ AN ENGINE ADDRESS THE STRIPPED ENTRY OWNS OR CARRIES, ADMITTED BY ITS
\ DECLARATION: any claim whose declared bytes cover it, by the one predicate
\ above.
: CLAIMED-CELL? ( n -- bool ) {: v:n :}
   AOT-OWNED:N 0 ?do
      i v CLAIMED-AT? if true unloop exit then
   loop false ;

\ THE IMAGE'S ANSWER FOR A DATA ADDRESS, and whether any claim covers it. BOTH
\ ROADS INTO A BELOW-WINDOW ADDRESS GO THROUGH THIS ONE MAP - the address a
\ record's code spells out (SCAN-ADDRESS, aot-lib.f COPY-ADDRESS) and the value a
\ declared DATA cell holds (XTD-ROW) - so an address one road rewrites is never
\ an address the other ships verbatim. An in-window value answers itself, because
\ the window is restored at the addresses it was captured from; a value inside a
\ carried claim answers with the copy at the same interior offset; a string
\ literal interned in another pool is re-interned into the application's and
\ answers with that copy. What is left is engine DATA the image does not restore
\ and no claim names, and each road refuses it by the site it has: a code site,
\ or the cell.
: MAPPED-DATA ( n -- n bool ) {: v:n :}
   v IN-WINDOW? if v true exit then
   v CARRIED-TARGET {: c:n :}
   c 0 >= if c true exit then
   v NSTR:REINTERN-OWNED drop {: w:n :}
   w  w IN-WINDOW? w CLAIMED-CELL? or ;

: DATA-ADDRESS! ( ptr n ptr u8 n -- ) {: owner:ptr site:ptr v:n :}
   v IN-WINDOW? if exit then
   v CLAIMED-CELL? if exit then
   v DATA-CELL? if v CHECK-DATA-CELL then
   owner site v REFUSE-DATA-SPAN ;

\ A carried cell's declared bytes and an immutable literal row can both be copied
\ into the window - the first into the carried run by name, the second into the
\ capture's active pool - and the address is rewritten to the copy either way.
\ Other pre-window DATA has no ownership proof and must keep the link refusal,
\ which DATA-ADDRESS! raises with this site's three facts.
: DATA-TARGET ( ptr n ptr u8 n -- n ) {: owner:ptr site:ptr v:n :}
   v MAPPED-DATA {: w:n ok:bool :}
   ok 0= if owner site w DATA-ADDRESS! then
   w ;

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
\ and there is nothing to relocate. A DATA-kind row carries no code address, so
\ it is not collected here - its cell is mapped in place by XTD-ROW below and
\ travels in the image's data blob like any other window byte.
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

\ A DECLARED DATA CELL HOLDS THE ADDRESS IT WAS DECLARED FOR, and the image has
\ to be able to read it. The window is restored at the addresses it was captured
\ from, so an in-window value needs nothing - which is all a DATA-kind row ever
\ needed while every declared pointer named the program's own data. A value below
\ the window is the engine's own DATA, which a stripped image does not restore:
\ it goes through the SAME map a code-spelled address goes through, and an
\ address no claim covers is refused by the cell that holds it. THE REWRITE IS
\ THE CELL'S CAPTURED VALUE: it is stored into the live window cell here, before
\ aot-lib.f reads the window out into the image's data blob, so the image ships
\ the mapped address in the cell itself and no second pass patches it.
: XTD-ROW ( n -- ) {: at:n :}
   at DATA-CELL@ {: v:n :}
   v DATA-ADDRESS? 0= IF exit THEN
   v MAPPED-DATA {: w:n ok:bool :}
   ok 0= IF at v REFUSE-UNCLAIMED-CELL THEN
   w v <> IF at w DATA-CELL! THEN ;

: XTC-ROW ( n -- ) {: k:n :}
   k ADDRESS-CELLS:ROW@ {: raw:n :}
   raw XTCELL-OFF-MASK and {: off:n :}
   DATA-VA VA>N off + {: at:n :}
   at XTC-IN-WINDOW? 0= IF exit THEN
   raw XTCELL-DATA-TAG and 0<> IF at XTD-ROW exit THEN
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
\ it by name. A target that resolves to a declaration-only record is neither: it
\ is never a member, because the relocation pass drops the call (AOT-DECLARATION?
\ above).
: SCAN-DIRECT ( ptr n ptr u8 ptr u8 ptr u8 -- ) {: caller:ptr p:ptr mstart:ptr mend:ptr :}
   p AOT-W32@ dup DIRECT? 0= if drop exit then
   p swap TARGET {: t:ptr :}
   t mstart >= t mend < and if exit then
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? if
      callee AOT-DECLARATION? if exit then
      caller callee SCAN-CALLEE exit
   then
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
\ UNDER TIER 1 NO CHAIN MARKS A FUNCTION BOUNDARY - a quotation reference is an
\ ADR, not a movz/movk chain - so a body runs to its record's end and carries
\ every function above it. Measured on a word installing two quotations at load
\ time (`: INSTALL ( -- ) [: INC ;] is A  [: DEC ;] is B ;`): the first body is a
\ 32-byte member in a 116-byte record, holding its own 16-byte function and the
\ second quotation's 16 bytes as well.
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

\ A NESTED BODY IS CARRIED ONCE, WHICH IS WHAT MAKES THE MEMBERS DISJOINT. No two
\ members share a byte, so ONE member answers for an address and aot-lib.f
\ OLD>NEW maps a target by containment alone. Two anonymous bodies of ONE record
\ are what would break that under tier 1: the lower one runs to the record's end
\ (ADD-BODY-CLO above) and so HOLDS the higher one's code, and both are members
\ when two declared xt cells name them. This pass drops the contained one. The
\ cell that named it resolves through the container at its own offset into it,
\ which is a real instruction in the copy COPY-COMPACT-BLOB emits for the
\ container - the same bytes the dropped copy would have held. Keeping both put
\ that code in the image twice and left the inner cell's row pointing into
\ whichever copy the walk reached first, which is the discovery order and not a
\ decision. Dropping the CONTAINER is never the answer: the code between the two
\ bodies would be missing from the image altogether.
\ WHAT THE PASS VISITS IS BOUNDED, because the link is linear in the closure's
\ size. A body is added by XT-CELL-ROOT alone - before the walk, at most one row
\ per declared xt cell (the capacity comment below) - so a contained body is one
\ of the rows 1..XTC-N, row 0 being MAIN's record, and dropping a row only moves
\ rows down. Its container NAMES THE SAME RECORD, whether it is that record's own
\ member or a sibling body of it, because a body and a record both arrive with
\ the record the address resolved through (ADD-BODY-CLO, ADD-REC-CLO). One scan
\ of the closure per candidate row is therefore enough: O(XTC-N x NCLO), never
\ quadratic in NCLO.
\ A PARTIAL OVERLAP CANNOT ARISE, so containment is the whole rule: BODY-END-SCAN
\ ends a body at the smallest recorded chain value above it INSIDE its record, so
\ two bodies of one record are disjoint or share their end; a record contains its
\ own bodies; and a stripped span names a word the image ships no record for.
\ ONE RECORD IS CONTAINED IN ANOTHER, and no linkable closure holds both: a
\ `;does` companion runs from its clause to its emission's end (measured: a
\ `create , does>` definer's record is [26247860,26247960) and its companion's
\ [26247936,26247960)), and reaching the definer means calling `create`, which
\ the link refuses - an entry calling such a definer dies exit 70 "stripped AOT
\ unsupported word 'create' called by ...", while an entry reaching only the
\ created word carries the companion alone. Any overlap this pass leaves is
\ therefore a defect in the table rather than a shape to compact away, and
\ aot-lib.f MEMBER-ORDER refuses it by name.
variable CLO-NI  variable CLO-NJ  variable CLO-NK   \ the candidate row, the scan, the shift
: CLO-INSIDE? ( n n -- bool ) {: b:n c:n :}         \ row b's extent inside row c's?
   b c = IF false EXIT THEN
   b CLO-REC@ c CLO-REC@ = IF
      b CLO-AT c CLO-AT >=
      b CLO-AT b CLO-BYTES +  c CLO-AT c CLO-BYTES +  <=  and
   ELSE false THEN ;
: CLO-CONTAINED? ( n -- bool ) {: b:n :}            \ ... inside any other member's
   0 CLO-NJ !
   BEGIN CLO-NJ @ NCLO @ < WHILE
      b CLO-NJ @ CLO-INSIDE? IF true EXIT THEN
      CLO-NJ @ 1+ CLO-NJ ! REPEAT  false ;
: DROP-CLO-ROW ( n -- ) {: b:n :}                   \ remove row b, compacting the columns
   b CLO-NK !
   BEGIN CLO-NK @ 1+ NCLO @ < WHILE
      CLO-NK @ 1+ CLO-AT     CLO-NK @ CLO !
      CLO-NK @ 1+ CLO-BYTES  CLO-NK @ CLO-LEN !
      CLO-NK @ 1+ CLO-REC@   CLO-NK @ CLO-REC !
      CLO-NK @ 1+ CLO-NK ! REPEAT
   NCLO @ 1- NCLO ! ;
: DROP-NESTED-CLO ( -- )
   1 CLO-NI !
   BEGIN CLO-NI @ NCLO @ <  CLO-NI @ XTC-N @ <=  and WHILE
      CLO-NI @ CLO-CONTAINED? IF CLO-NI @ DROP-CLO-ROW ELSE CLO-NI @ 1+ CLO-NI ! THEN
   REPEAT ;

\ THE CAPACITY IS A BOUND THE LINKER ALREADY KNOWS, which is why the tables can
\ be allocated per program. A member IS its entry (ADD-CLO dedups on it), and
\ every entry the walk can offer is one of three: a dictionary record's code
\ entry (ADD-REC-CLO, at most `ndict@` of them), a stripped span's start
\ (ADD-SPAN-CLO, at most SPAN-N) or an anonymous body interior to a record
\ (ADD-BODY-CLO, reached only from XT-CELL-ROOT above and so at most one per
\ declared xt cell, XTC-N). Their sum bounds the closure, and all three counts
\ are final before the walk starts: the application has finished loading, so the
\ dictionary is closed and the seed's span table is published, and aot-lib.f LINK
\ runs COLLECT-XT-CELLS before CLOSURE. One allocation of that many rows per
\ column, no growth and no constant.
: CLO-CAPACITY ( -- n ) ndict@ SPAN-N + XTC-N @ + ;
: CLO-LIMIT-ABOVE-DIE ( n n -- ) {: req:n cap:n :}
   s" aot: CLO-LIMIT " AETXT req AEJNUM
   s"  above the closure capacity " AETXT cap AEJNUM
   s"  = records " AETXT ndict@ AEJNUM
   s"  + stripped spans " AETXT SPAN-N AEJNUM
   s"  + declared xt cells " AETXT XTC-N @ AEJNUM
   10 AE1
   s" aot: CLO-LIMIT above the closure capacity" 74 die ;
\ The capacity is the limit unless a program asked for a lower one, and a request
\ above the capacity is refused where the two first meet: at the sizing for a
\ request made before it (a fixture lowers the limit while its source loads), at
\ the call for one made after.
: CLO-LIMIT-RESOLVE ( -- )
   CLO-REQ @ 0= IF CLO-CAP @ CLO-LIMIT ! exit THEN
   CLO-REQ @ CLO-CAP @ > IF CLO-REQ @ CLO-CAP @ CLO-LIMIT-ABOVE-DIE THEN
   CLO-REQ @ CLO-LIMIT ! ;
: CLO-LIMIT! {: n:n :}
   n 1 < IF s" aot: CLO-LIMIT below 1" 74 die THEN
   n CLO-REQ !
   CLO-CAP @ 0<> IF CLO-LIMIT-RESOLVE THEN ;
\ Allocate the three columns for one link. CLOSURE asks for CLO-CAPACITY rows; a
\ test that fills the tables by hand instead of walking asks for the rows it is
\ about to write.
: CLO-TABLES ( n -- ) {: rows:n :}
   rows 1 < IF s" aot: closure capacity below 1" 74 die THEN
   rows CLO-RESERVE  rows CLO-LEN-RESERVE  rows CLO-REC-RESERVE
   rows CLO-CAP !
   CLO-LIMIT-RESOLVE ;

: CLOSURE  CLO-CAPACITY CLO-TABLES
   0 NCLO !  FINDMAIN dup 0= IF drop NO-ENTRY-DIE THEN  dup ROOTREC !  ADD-REC-CLO
   XT-CELL-ROOTS
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ SCAN-MEMBER  WI @ 1+ WI ! REPEAT
   DROP-NESTED-CLO ;

;using
;package
