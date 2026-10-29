\ aot-closure.f - the stripped-image closure: the walk from MAIN over direct
\ calls and recorded address sites into the member tables CLO, CLO-LEN and
\ CLO-REC, and the refusals a member can raise. aot-common.f LINK runs CLOSURE;
\ aot-link-arm.f and aot-x64.f copy and relocate the members.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/address-carrier.f
require src/habu/address-cells.f
require src/habu/sites.f
require src/habu/text-sites-x64.f
require src/habu/code-span.f
\ The span cells and HERE-N come from the latch file, which the build driver
\ loads before the application's require.
require src/habu/aot-window-latch.f
require src/habu/aot-owned-cells.f
\ The live-cell classifier and diagnostic are shared with FILE admission.
require src/habu/aot-data-cell.f

\ This file compiles checked, with raw-pointer boundaries as explicit TRUST rows.
package AOT-LINK
using SNAP-RELOC
using ADDRESS-CARRIER

\ The linker keeps its diagnostic words. Strict FILE export uses the same
\ refusal; automatic FILE caching declines unsafe storage and continues source.
: AE1 ( n -- ) AOT-CELL:AE1 ;
: AETXT ( ptr u8 n -- ) AOT-CELL:AETXT ;
: AEREC-TXT ( ptr n -- ) AOT-CELL:AEREC-TXT ;
: AEJSTR ( ptr u8 n -- ) AOT-CELL:AEJSTR ;
: AEJKEY ( ptr u8 n -- ) AOT-CELL:AEJKEY ;
: AEJREC ( ptr n -- ) AOT-CELL:AEJREC ;
: AEJNUM ( n -- ) AOT-CELL:AEJNUM ;

\ Raw views of mixed dictionary fields and the live code and dictionary extent.
: AOT-DBASE@ dbase@ ;
s" AOT-DBASE@" s" -- ptr u8" TRUST
\ The dictionary base also names code offsets in diagnostics.
: AOT-DBASE-N dbase@ ;
s" AOT-DBASE-N" s" -- n" TRUST
\ The DATA cursor as a pointer: aot-window-latch.f HERE-N computes it, because
\ the window is latched before this file loads.
: AOT-PTR@ {: a:ptr :} ( ptr a -- ptr a )
   a @ ;
s" AOT-PTR@" s" ptr a -- ptr a" TRUST

: AOT-W32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

\ src/habu/sites.f is the one reader of the record of
\ address-literal first bytes, so a numeric literal with the same bits is never
\ relocated. Only the live code region (offsets DICT-SIZE up to REGION) carries
\ recorded sites; an engine-text record yields none. A walk's per-site word runs
\ inside SITES:EACH-IN-SPAN's quotation, which cannot read the walker's locals,
\ so each walk keeps its span's end and its question in cells.
: SITE-OFF ( ptr u8 -- n ) {: p:ptr :}
   p AOT-DBASE@ - ;
: SITE-PTR ( n -- ptr u8 ) {: off:n :}
   AOT-DBASE@ off + ;

\ Sites whose first byte lies in [p, e), in ascending order.
: EACH-SITE ( ptr u8 ptr u8 [ n n -- ] -- ) {: p:ptr e:ptr q :}
   p SITE-OFF DICT-SIZE max {: lo:n :}
   e SITE-OFF REGION min {: hi:n :}
   lo hi >= if exit then
   lo hi lo - q SITES:EACH-IN-SPAN ;

TYPED-VARIABLE MEMBER-Q [ ptr u8 n -- ]
: MEMBER-REGION-SITE ( n n -- ) {: off:n kind:n :}
   off SITE-PTR
   kind SNAP-RELOC:SITE-CALL = if 1+ else 2 + then
   kind MEMBER-Q @ execute ;

\ A closure member's recorded sites, whether it came from live JIT REGION or
\ from the immutable engine text. Text kinds 3/4/5 are rel8/rel32/abs64;
\ REGION kinds 1/2 are call/address. Both yield the original field pointer.
: EACH-MEMBER-SITE ( ptr u8 ptr u8 [ ptr u8 n -- ] -- )
   {: p:ptr e:ptr q :}
   p AOT-DBASE@ >=
   e AOT-DBASE@ REGION + <= and if
      q MEMBER-Q !
      p e [: MEMBER-REGION-SITE ;] EACH-SITE exit
   then
   HB-TARGET-LINUX-X86-64? if p e q X64TEXT:EACH-IN-SPAN then ;

\ The address literal at a site, as its size in bytes and the value it carries,
\ never read past e; size 0 for a truncated or malformed literal. One arm per
\ target spelling: AArch64 moves the address in halves, a compact DATA chain or
\ a full four-half one; x86-64 moves it whole with one `mov r64, imm64`.
: CHAIN-LITERAL ( ptr u8 ptr u8 -- n n ) {: p:ptr e:ptr :}
   p e CHAIN-SIZE {: size:n :}
   size 0= if 0 0 exit then
   size  p size CHAIN-VALUE ;
: MOVABS-LITERAL ( ptr u8 ptr u8 -- n n ) {: p:ptr e:ptr :}
   e p - MOVABS-BYTES < if 0 0 exit then
   p MOVABS-SITE? 0= if 0 0 exit then
   MOVABS-BYTES  p MOVABSV ;
: SITE-LITERAL ( ptr u8 ptr u8 -- n n ) {: p:ptr e:ptr :}
   HB-TARGET-LINUX-X86-64? if p e MOVABS-LITERAL exit then
   p e CHAIN-LITERAL ;

: ADDRESS-VALUE ( ptr u8 ptr u8 -- n ) {: p:ptr e:ptr :}
   p e SITE-LITERAL {: size:n v:n :}
   size 0= if s" aot: malformed recorded address chain" 74 die then
   v ;

: DATA-ADDRESS? ( n -- bool ) AOT-CELL:DATA-ADDRESS? ;

\ Only the user DATA span is copied by stripped startup: a fixed address into
\ the compiler's earlier heap would read a zeroed replacement. DATA-ADDRESS! and
\ DATA-TARGET enforce that below, after the record accessors their diagnostics
\ use. BLOB-SRC, BLOB-END and BLOB-LEN are the latch file's; BLOB-LBL is the
\ emitted blob's label, placed and read only by aot-link-arm.f.
variable BLOB-LBL

\ B and BL match MASK/OPCODE; conditional and compare branches (B.cond, CBZ,
\ TBZ) stay intra-record and are never followed as calls.
$7C000000 constant MASK
$14000000 constant OPCODE
$3FFFFFF constant DELTA-MASK
$2000000 constant SIGN

: SIGNED ( n -- n ) SIGN xor SIGN - ;

: DIRECT? ( n -- bool ) MASK and OPCODE = ;

: TARGET ( ptr u8 n -- ptr u8 ) {: p:ptr w:n :}
   p w DELTA-MASK and SIGNED 4 * + ;

: REC {: k:n :} ( n -- ptr n )
   AOT-DBASE@ k 48 * + CELL-VIEW ;   \ dict record k  (0:addr 8:len 16:name-len|flags 24:name|ptr)
: REC-BYTES ( ptr n -- n ) 8 + @ CODE-SPAN:BYTES ;
: AOT-FOLD {: c:n :}  c 64 > c 91 < and IF c 32 + ELSE c THEN ;
\ A dict record is one raw 48-byte block holding both cells (code address, code
\ length, name-length|flags) and, for a short name, the name bytes. One pointer
\ cannot be a cell pointer and a byte pointer at once, so REC yields the record
\ as cells, IX-NAME-PTR views the name field as bytes, and the name accessors
\ work from the record index.
: REC>IX {: r:ptr :} ( ptr a -- n )
   r NULL-PTR - AOT-DBASE-N - 48 / ;
: IX-NAME-LEN {: k:n :} ( n -- n )
   k REC 16 + @ DNAME-LEN-MASK and ;
: IX-NAME-PTR {: k:n :} ( n -- ptr u8 )
   k REC 16 + @ DNAME-EXT and 0= IF k REC 24 + BYTE-VIEW ELSE k REC 24 + AOT-PTR@ BYTE-VIEW THEN ;

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
\ MAIN, the zero-argument process root, looked up bare so it selects the global
\ word.
: ENTRY-NAME$ ( -- ptr u8 n ) s" MAIN" ;
\ Data-space words (@ ! c@ c! here allot , c,) are supported: the AOT entry maps
\ the fixed DATA region and restores the program's data (aot-link-arm.f
\ EMIT-DATA-COPY). The closure walk tells runtime use from compile-time
\ definition, so `create` here means runtime dictionary creation. The five
\ writers reach the code region or a live dictionary record (stripped __text is
\ r-x and not at RBASE-VA): `patch32` pokes one word, `code-publish` opens the
\ publication window, `xref-retarget` points a record at a routine,
\ `callmap-set` and `addrmap-set` record a site's relocation class.
: AOT-UNSAFE? {: r:ptr :} ( ptr a -- bool )
   r s" create" REC-NAME= IF 0 0= EXIT THEN
   r s" patch32" REC-NAME= IF 0 0= EXIT THEN
   r s" code-publish" REC-NAME= IF 0 0= EXIT THEN
   r s" callmap-set" REC-NAME= IF 0 0= EXIT THEN
   r s" addrmap-set" REC-NAME= IF 0 0= EXIT THEN
   r s" xref-retarget" REC-NAME= IF 0 0= EXIT THEN
   0 0= 0= ;
\ DECLARATIONS WHOSE REGISTRIES A STRIPPED IMAGE NEVER READS. (MARK) is the
\ address-cell registrar (habu2.f EMIT-MARK): `xt!` stores the token and BLs it
\ to DECLARE the cell as a code address. Its readers (the snapshot writer,
\ habu2.f EMIT-XT, aot-capture.f) are absent from a stripped image, whose only
\ restore pass is the linker-computed aot-link-arm.f EMIT-XT-CELLS, and the
\ registrar would die on the zero address-cell header of fresh DATA (habu2.f
\ MARK-HEADER). So link drops the declaration half of `xt!` (aot-link-arm.f
\ RELOC-W32 writes a NOP) and keeps the store half, BSTORE's guard included. A
\ declaration-only record is never a closure member. ptr-cell-mark's primitive
\ first pops the address; the ARM64 linker keeps that pop when it drops the
\ registry call (aot-common.f PTR-MARK-TARGET?). The x86-64 linker drops only
\ xt!'s recorded registrar call: ptr-cell-mark calls the registrar at runtime
\ there and is refused if reachable (aot-x64.f). Both rules key on the target,
\ never on the calling member.
: AOT-DECLARATION? {: r:ptr :} ( ptr a -- bool )
   r s" (MARK)" REC-NAME= IF 0 0= EXIT THEN
   r s" ptr-cell-mark" REC-NAME= ;

\ DP-CHECK branches to a compiler diagnostic/recovery tail in the engine.
\ A stripped image replaces that exact helper with its own bounded refusal.
: AOT-DP-REFUSAL? {: r:ptr :} ( ptr a -- bool )
   r s" (LDPBAD)" REC-NAME= ;
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

\ The image ships no record for a word nothing can name, but a displacement into
\ its code must still land in an accounted span. The payload carries 8-byte rows
\ (blob offset u32, raw CODE-SPAN u32); the seed publishes the table, its row
\ count and the blob's copy address (habu2.f AOT-SPAN:PUBLISH,). All three are
\ zero in an engine that captured nothing and in a whitebox image that kept
\ every name. The base is read both as a pointer (the spans walked here) and as
\ a number (the value-domain range test), as CELL-TEXTPTR? reads the extents.
: SPAN-TABLE ( -- ptr u8 )
   data-base AOT-CELLS:SPAN-TABLE-CELL CELL / ptr-field @ ;
: SPAN-BASE ( -- ptr u8 )
   data-base AOT-CELLS:SPAN-BASE-CELL CELL / ptr-field @ ;
: SPAN-BASE-N ( -- n ) data-base AOT-CELLS:SPAN-BASE-CELL + @ ;
: SPAN-N ( -- n ) data-base AOT-CELLS:SPAN-N-CELL + @ ;
: SPAN-ROW ( n -- ptr u8 ) {: k:n :}
   SPAN-TABLE k AOT-SPAN:ROW * + ;
: SPAN-OFF ( n -- n ) SPAN-ROW AOT-W32@ ;
: SPAN-START ( n -- ptr u8 ) {: k:n :} SPAN-BASE k SPAN-OFF + ;
: SPAN-START-N ( n -- n ) {: k:n :} SPAN-BASE-N k SPAN-OFF + ;
: SPAN-BYTES ( n -- n ) SPAN-ROW 4 + AOT-W32@ CODE-SPAN:BYTES ;

\ The span that starts here, or -1: FINDADDR-PTR's question for the words the
\ image ships no record for.
: SPAN-AT-ENTRY ( ptr u8 -- n ) {: t:ptr :}
   SPAN-N 0 ?do
      i SPAN-START t = if i unloop exit then
   loop  -1 ;

\ The span that contains this address, ADDRESS-OWNER's second question: an
\ anonymous body is interior to the word that emitted it.
: SPAN-OWNER ( n -- n ) {: t:n :}
   SPAN-N 0 ?do
      t i SPAN-START-N >= if
         t i SPAN-START-N - i SPAN-BYTES < if i unloop exit then
      then
   loop  -1 ;

: FINDMAIN ( -- ptr n )
   ENTRY-NAME$ XREF-FIND ;

\ The closure is a BFS from MAIN over the native call graph. The member tables are
\ sized from the program being linked (CLO-CAPACITY, CLO-TABLES); ADD-CLO fails
\ closed at that capacity, and no constant decides which programs can be
\ stripped.
\ A member is a code span: past this point only its entry and length are read.
\ CLO-REC is XREF-NULL for a member no record names (a span-table row); only the
\ unsupported-word check and the diagnostics read it. The entry is the
\ identity: `EXPORT` gives one execution token two records, and one span is one
\ member, copied once.
\ The columns are DYNAMIC-BUFFERs, whose declared pointer head keeps the address
\ columns typed. The heads sit above BLOB-END and the mappings register in
\ DYNAMIC-STORAGE, which a stripped image publishes fresh
\ (src/habu/aot-owned-cells.f), so a link-time table reaches neither the
\ captured window nor the image.
DYNAMIC-BUFFER CLO ptr u8      \ each member's code entry
DYNAMIC-BUFFER CLO-LEN n       \ ... its code length in bytes
DYNAMIC-BUFFER CLO-REC ptr n   \ ... and the record that named it, or XREF-NULL
variable NCLO
PTR-VARIABLE ROOTREC
variable CLO-CAP     \ rows allocated, 0 until the tables are sized
variable CLO-REQ     \ a limit a program lowered by hand, or 0 for the capacity
variable CLO-LIMIT   \ ... and the one ADD-CLO enforces, resolved at the sizing
: CLO-AT ( n -- ptr u8 ) CLO @ ;
: CLO-BYTES ( n -- n ) CLO-LEN @ ;
: CLO-REC@ ( n -- ptr n ) CLO-REC @ ;

\ Both walk questions are indexed: is this entry a member (IN-CLO?, once per
\ followed call) and which record starts here (FINDADDR-PTR, once per member
\ instruction). As scans, a 27,000-word chain cost IN-CLO? 364,594,512
\ comparisons over 27,005 calls and FINDADDR-PTR 587,777,056 over 27,004;
\ indexed, 1.3 and 2.7 slots per call, and the maker went from 43.0 s to 32.1 s,
\ the time the same words take behind a one-member closure.
\ An index holds row numbers and no key column: a slot's key is read back
\ through its row (CLO-AT, REC-CODE-PTR@), so it cannot disagree with the
\ column. Linear probing, no deletion; DROP-NESTED-CLO, the one pass that
\ removes rows, rebuilds the set.
\ Capacity is the power of two at or above twice the row bound (the closure
\ capacity, ndict@), so the load stays at most a half; a wrap is a defect in
\ this file and refuses by name, as CLO-OVERFLOW-DIE does, instead of looping.
\ The hash decides probe order, never an answer, and holds no per-run value.
-1 constant IX-FREE                    \ a slot naming no row
$9E3779B97F4A7C15 constant IX-MUL      \ odd: the 64-bit golden-ratio multiplier
variable IX-CX
: IX-CAPACITY ( n -- n ) {: rows:n :}  \ a power of two, at least twice the rows
   1 IX-CX !
   BEGIN IX-CX @ rows 2 * < WHILE IX-CX @ 2 * IX-CX ! REPEAT
   IX-CX @ ;
: IX-SLOT ( ptr u8 n -- n ) {: e:ptr mask:n :}   \ the first slot for a code entry
   e AOT-DBASE@ - 2 rshift  IX-MUL *  32 rshift  mask and ;

\ The members, indexed by entry: a second record naming one span finds the
\ member already there. A fixture that writes rows by hand leaves the set empty.
DYNAMIC-BUFFER CLO-SET n       \ the member row whose entry lands in each slot
variable CLO-MASK              \ ... the set's capacity minus one
variable CLO-SX                \ ... the probe's slot
variable CLO-ROW               \ ... and the row that slot named
variable CLO-PROBE  variable CLO-RI
: CLO-SET-EMPTY ( -- )
   0 CLO-SX !
   BEGIN CLO-SX @ CLO-MASK @ <= WHILE  IX-FREE CLO-SX @ CLO-SET !  CLO-SX @ 1+ CLO-SX ! REPEAT ;
: CLO-SET-SIZE ( n -- ) {: rows:n :}
   rows IX-CAPACITY 1- CLO-MASK !
   CLO-MASK @ 1+ CLO-SET-RESERVE
   CLO-SET-EMPTY ;
: IN-CLO? {: start:ptr :} ( ptr u8 -- bool )
   start CLO-MASK @ IX-SLOT CLO-SX !
   BEGIN CLO-SX @ CLO-SET @ dup CLO-ROW ! IX-FREE <> WHILE
      CLO-ROW @ CLO-AT start = IF true exit THEN
      CLO-SX @ 1+ CLO-MASK @ and CLO-SX !
   REPEAT false ;
: CLO-SET+ ( n -- ) {: row:n :}        \ index a row whose entry the set lacks
   row CLO-AT CLO-MASK @ IX-SLOT CLO-SX !
   0 CLO-PROBE !
   BEGIN CLO-SX @ CLO-SET @ IX-FREE <> WHILE
      CLO-PROBE @ 1+ dup CLO-PROBE ! CLO-MASK @ > IF
         s" aot: closure member index full" 74 die THEN
      CLO-SX @ 1+ CLO-MASK @ and CLO-SX !
   REPEAT
   row CLO-SX @ CLO-SET ! ;
: CLO-SET-REBUILD ( -- )               \ ... every row again, after rows moved
   CLO-SET-EMPTY
   0 CLO-RI !
   BEGIN CLO-RI @ NCLO @ < WHILE  CLO-RI @ CLO-SET+  CLO-RI @ 1+ CLO-RI ! REPEAT ;
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
\ Reachable only where CLO-LIMIT! lowered the limit below the capacity; the
\ suggestion says what a user can do about a program that is too large.
: CLO-OVERFLOW-DIE {: r:ptr :} ( ptr a -- )
   JSON-DIAGS @ IF r CLO-OVERFLOW-JSON ELSE r CLO-OVERFLOW-PROSE THEN
   s" aot: closure exceeds the closure limit" 74 die ;
\ A MEMBER LOADED AHEAD OF THE MAKER IS REFUSED, BY NAME. The engine's records
\ end at its seal watermark, and tools/aot-build-open.f latches OPENER-NDICT
\ before it loads anything an application can name, so a record between the
\ two was defined ahead of the maker: on the engine only the latch file's
\ private records, on the keyed linker image (test/preloaded-engine.f rule 3)
\ the image's whole load. No production build carries one: the engine's maker
\ compiles a module the application requires inside the window, and refuses any
\ other name the application never required.
: PRE-WINDOW? ( ptr n -- bool ) {: r:ptr :}
   r BYTE-VIEW AOT-DBASE@ - DREC / {: k:n :}
   k data-base SEAL-NDICT-CELL + @ >= k OPENER-NDICT @ < and ;
\ The remedy: the program's own require loads the module inside the window,
\ which works only on an engine that has not loaded it already.
: PRE-WINDOW-HINT$ ( -- ptr u8 n )
   s" require the module that defines the word and link on an engine that has not loaded it, so it compiles inside the capture window, or build with --repl" ;
: PRE-WINDOW-JSON {: r:ptr :} ( ptr a -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-PRE-WINDOW" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY r AEJREC 44 AE1
   s" suggestion" AEJKEY PRE-WINDOW-HINT$ AEJSTR
   125 AE1 10 AE1 ;
: PRE-WINDOW-PROSE {: r:ptr :} ( ptr a -- )
   s" aot: closure reaches a word defined before the capture window opened word=" AETXT
   r AEREC-TXT
   s"  suggestion='" AETXT PRE-WINDOW-HINT$ AETXT 39 AE1
   10 AE1 ;
\ The rendering is the whole diagnostic, so the die adds no line of its own.
: PRE-WINDOW-DIE {: r:ptr :} ( ptr a -- )
   JSON-DIAGS @ IF r PRE-WINDOW-JSON ELSE r PRE-WINDOW-PROSE THEN
   s" " 74 die ;
: ADD-CLO ( ptr n ptr u8 n -- ) {: r:ptr start:ptr len:n :}
   start IN-CLO? IF exit THEN
   r XREF-FOUND? IF r PRE-WINDOW? IF r PRE-WINDOW-DIE THEN THEN
   NCLO @ CLO-LIMIT @ >= IF r CLO-OVERFLOW-DIE THEN
   start NCLO @ CLO !
   len NCLO @ CLO-LEN !
   r NCLO @ CLO-REC !
   NCLO @ CLO-SET+
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

\ A call target's record by exact code entry (REC-CODE-PTR@), so a direct-BL
\ target needs no address-to-cell cast; a non-entry address matches nothing and
\ fails closed later. The first record wins: `EXPORT` gives one token two
\ records, and the walk must reach the same one either way.
\ The index is valid for exactly ndict@ rows: aot-common.f LINK runs with the
\ dictionary closed, so one build serves a link, and a caller that defines
\ words between questions (the gate fixtures ask FINDADDR-PTR with no closure
\ tables sized) gets a rebuild. REC-IX-N starts at 0, so the first question
\ builds. Only public `ndict!`, owned by the property-test harness, lowers the
\ count, and it never links.
DYNAMIC-BUFFER REC-BY-ENTRY n  \ the first record row whose entry lands in each slot
variable REC-IX-MASK           \ ... the index's capacity minus one
variable REC-IX-N              \ ... the record count it was built at, 0 until built
variable REC-SX  variable REC-ROW  variable REC-PROBE
: REC-IX+ ( n -- ) {: row:n :}
   row REC REC-CODE-PTR@ {: e:ptr :}
   e REC-IX-MASK @ IX-SLOT REC-SX !
   0 REC-PROBE !
   BEGIN REC-SX @ REC-BY-ENTRY @ dup REC-ROW ! IX-FREE <> WHILE
      REC-ROW @ REC REC-CODE-PTR@ e = IF exit THEN
      REC-PROBE @ 1+ dup REC-PROBE ! REC-IX-MASK @ > IF
         s" aot: closure record index full" 74 die THEN
      REC-SX @ 1+ REC-IX-MASK @ and REC-SX !
   REPEAT
   row REC-SX @ REC-BY-ENTRY ! ;
: REC-IX-BUILD ( -- )
   ndict@ IX-CAPACITY 1- REC-IX-MASK !
   REC-IX-MASK @ 1+ REC-BY-ENTRY-RESERVE
   0 REC-SX !
   BEGIN REC-SX @ REC-IX-MASK @ <= WHILE
      IX-FREE REC-SX @ REC-BY-ENTRY !  REC-SX @ 1+ REC-SX ! REPEAT
   0 FX ! BEGIN FX @ ndict@ < WHILE  FX @ REC-IX+  FX @ 1+ FX ! REPEAT
   ndict@ REC-IX-N ! ;
: FINDADDR-PTR ( ptr u8 -- ptr n ) {: t:ptr :}
   REC-IX-N @ ndict@ <> IF REC-IX-BUILD THEN
   t REC-IX-MASK @ IX-SLOT REC-SX !
   BEGIN REC-SX @ REC-BY-ENTRY @ dup REC-ROW ! IX-FREE <> WHILE
      REC-ROW @ REC REC-CODE-PTR@ t = IF REC-ROW @ REC exit THEN
      REC-SX @ 1+ REC-IX-MASK @ and REC-SX !
   REPEAT  XREF-NULL ;

\ Exact entry first, then the recorded span (including the explicit full-span
\ bit), never a nearest-address guess: ticks may name engine-text entries and
\ anonymous bodies are interior to their owner's emission. Namespace records
\ hold wordlist IDs in those fields, not code spans.
: ADDRESS-OWNER ( n -- ptr n ) {: t:n :}
   HB-TARGET-LINUX-X86-64? 0= if
      t 3 and 0<> if XREF-NULL exit then
   then
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

\ THE RECORD WHOSE CODE STARTS NEAREST BELOW AN ADDRESS: a reading aid for a
\ refusal, never ownership (a walk that guessed would carry the wrong bytes). A
\ refused address is often in a span member with XREF-NULL for its record
\ (ADD-SPAN-CLO), so a refusal names `NAME+off`, the `+off` marking a neighbour
\ and not an owner. CODE-ADDR-TXT applies it to the span refusal's site and to
\ the code refusals' target (aot-common.f MAP-TARGET!, aot-link-arm.f
\ ADR-TARGET!). Namespace records are skipped.
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

\ Whether any record starts above the address: a stripped word's code sits in
\ the gap between two records, so the record below names an address only when
\ another starts above it. An address above every record (the DATA buffer a
\ hand-built member aims an ADR at) stays `<unknown>`.
: CODE-ABOVE? ( ptr u8 -- bool ) {: addr:ptr :}
   ndict@ 0 ?do
      i REC {: r:ptr :}
      r REC-WID@ -1 <> if
         r REC-CODE-PTR@ addr > if true unloop exit then
      then
   loop false ;

\ A cell holds a code or dict pointer iff its value lands in a live extent the
\ dictionary records (src/habu/aot-data-cell.f): the record array [dbase,
\ +ndict@*DREC) or the emitted code [dbase+DICT-SIZE, cp@); free space is data.
\ The span scan (aot-common.f
\ AOT-DATA-TEXTPTR-CHECK) and DATA-ADDRESS! below share this predicate and must
\ not answer differently. The two extents are asked separately: a declared code
\ address is relocated against the image's code base, and a record address is
\ refused however declared, since the image carries no records.
: CELL-DICTPTR? ( n -- bool ) AOT-CELL:DICT-PTR? ;
: CELL-TEXTPTR? ( n -- bool ) AOT-CELL:TEXT-PTR? ;

: CELL-MAPPED? ( n -- bool ) AOT-CELL:MAPPED? ;

\ The eight bytes at this address are a string literal's body, by the
\ compiler's row table (src/compiler/native/string.f, one row per interned body),
\ which MAPPED-DATA below also re-interns through. This is a declaration, not a
\ reading of the value: `s"` bodies are immutable characters no form puts an
\ address in, yet `s" {pm}"` and a 01 byte read as 0x017D6D707B and can land in
\ the emitted code span. Every byte of the
\ cell is asked: the scan starts a cell at every byte, so a cell can begin
\ before a body and still cover its characters.
: LITERAL-BYTE? ( n -- bool ) {: at:n :}
   at NSTR:OWNER-ROW {: body:ptr size:n owned:bool :}
   owned ;
: CELL-LITERAL? ( n -- bool ) {: at:n :}
   at [: LITERAL-BYTE? ;] AOT-CELL:LITERAL-OVERLAP? ;

\ The only place in the linker where a DATA address becomes a pointer again:
\ DATA is one mapping at data-base, and the cell reads here and the blob reads
\ in aot-common.f (BLOB-SRC@) come through it. Every caller bounds-checks first
\ (DATA-CELL? for a recorded value, XTC-IN-WINDOW? for a declared cell, the span
\ bounds for the scan and the copy).
: DATA-PTR ( n -- ptr u8 ) {: at:n :}
   data-base BYTE-VIEW at DATA-VA VA>N - + ;

\ The span scan (aot-common.f SCAN-DATA-CELL) reads cells through this word too.
: DATA-CELL@ ( n -- n ) DATA-PTR CELL-VIEW @ ;

\ The linker writes a window cell in exactly two places: aot-common.f
\ CARRY-CELLS copies a carried claim into the carried run, and XTD-ROW below
\ stores a declared DATA cell's mapped value. The blob is read out of live DATA
\ after both.
: DATA-CELL! ( n n -- ) {: at:n v:n :} v at DATA-PTR CELL-VIEW ! ;

\ A whole cell at this address is inside mapped DATA, so reading it cannot
\ fault. DATA-ADDRESS? admits the region's one-past end, which is not a readable
\ cell.
: DATA-CELL? ( n -- bool ) {: v:n :}
   v DATA-VA VA>N >= v 8 + DATA-VA VA>N DATA-SIZE + <= and ;

\ A per-site word reads the literal at each site up to the record's end (a
\ literal the span cuts short is no answer) and keeps its answer in a cell.
PTR-VARIABLE RW-END  variable RW-V
: REC-WALK ( ptr n n [ n n -- ] -- ) {: r:ptr v:n q :}
   r REC-CODE-PTR@ {: p:ptr :}
   p r REC-BYTES + RW-END !  v RW-V !
   p RW-END @ q EACH-SITE ;
\ Size 0 for a call site or a malformed one.
: RW-LITERAL ( n n -- n n ) {: off:n kind:n :}
   kind SNAP-RELOC:SITE-ADDR <> if 0 0 exit then
   off SITE-PTR RW-END @ SITE-LITERAL ;

\ `variable`, `create` and `defer` compile a recorded address chain for their
\ own body, so a site links a persistent cell back to its owning word. A cell
\ reached by arithmetic from another body has no site and stays unnamed rather
\ than attributed to the nearest word below it.
variable CS-HIT
: CELL-SITE ( n n -- )
   RW-LITERAL {: size:n w:n :}
   size 0<> w RW-V @ = and if 1 CS-HIT ! then ;
: REC-CELL-SITE? ( ptr n n -- bool ) {: r:ptr v:n :}
   0 CS-HIT !
   r v [: CELL-SITE ;] REC-WALK
   CS-HIT @ 0<> ;

\ Namespace records hold wordlist IDs where a word record holds its code span.
\ The walk uses the recorded sites (src/habu/sites.f), which both targets keep;
\ AOT-CELL:OWNER (src/habu/aot-data-cell.f) asks the ARM64 address bitmap, which
\ an x86-64 engine does not keep.
: DATA-CELL-OWNER ( n -- ptr n ) {: v:n :}
   ndict@ 0 ?do
      i REC REC-WID@ -1 <> if
         i REC v REC-CELL-SITE? if i REC unloop exit then
      then
   loop XREF-NULL ;

\ The greatest DATA address this record spells at or below a value, or -1: the
\ only name an address no record spells exactly can be given. A chain spelling a
\ code address is no answer, so the value domain is checked before the distance.
variable CB-AT
: CELL-BELOW ( n n -- )
   RW-LITERAL {: size:n w:n :}
   size 0= if exit then
   w DATA-ADDRESS?  w RW-V @ <= and  w CB-AT @ > and if w CB-AT ! then ;
: REC-CELL-BELOW ( ptr n n -- n ) {: r:ptr v:n :}
   -1 CB-AT !
   r v [: CELL-BELOW ;] REC-WALK
   CB-AT @ ;

\ The span refusal names an unspelled target `NAME+off` from this pair: a buffer
\ reached at a field offset, or one whose record the build stripped, has no
\ exact spelling. The CELL refusals keep DATA-CELL-OWNER's exact answer, a claim
\ about ownership.
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

: REFUSE-DATA-CELL ( n n ptr u8 n ptr u8 n -- ) AOT-CELL:REFUSE ;

\ A declaration is the only authority for reading a cell as a code address: a
\ value-range guess would rewrite a datum that looks like an address. `defer`,
\ `is` and `xt!` declare; `' word ,` stores an untyped cell and does not.
: REFUSE-UNDECLARED-CELL ( n n -- ) AOT-CELL:REFUSE-UNDECLARED ;

: REFUSE-MAPPED-CELL ( n n -- ) AOT-CELL:REFUSE-MAPPED ;

\ The image ships code, never records, so a record pointer has nothing to be
\ rebased onto.
: REFUSE-DICT-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data holds a dictionary-record pointer"
   s" a stripped image carries no dictionary records; remove the record pointer from persistent data, or use --repl"
   REFUSE-DATA-CELL ;

: REFUSE-XT-TARGET ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT declared xt cell holds an address no word in the image owns"
   s" the cell is declared to hold an execution token; store a real token in it, or use --repl"
   REFUSE-DATA-CELL ;

\ A cell below the capture window: a preloaded module's data, which the stripped
\ image never restores.
: REFUSE-UNRESTORED-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data outside the restored span holds a code/dict pointer"
   s" the image restores only the program's own DATA window; define the cell in the program's own source, or use --repl"
   REFUSE-DATA-CELL ;

\ A declared DATA cell holding an engine address no claim names: the cell
\ travels, but the address is engine DATA the image never restores, so the image
\ would read zero through it. The cell is named, not a code site, because the
\ fault is the stored value.
: REFUSE-UNCLAIMED-CELL ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT declared data cell holds an engine address outside the restored span"
   s" the image restores only the window this program owns and the cells src/habu/aot-owned-cells.f claims; take the address at run time, claim the target carried, or use --repl"
   REFUSE-DATA-CELL ;

\ A data pointer the restored span does not cover, named like
\ REFUSE-ADDRESS-SITE (owning word, region offset) plus the value, the word whose
\ data it is, and the span it had to fall in.
\ The owning record names a code address when there is one (the member's record
\ for a site, ADDRESS-OWNER for a target); otherwise the record below does, as
\ `NAME+off`, only where recorded code also starts above it; else `<unknown>`.
: CODE-ADDR-TXT ( ptr n ptr u8 -- ) {: owner:ptr addr:ptr :}
   owner XREF-FOUND? if owner AEREC-TXT exit then
   addr CODE-ABOVE? if addr CODE-NEIGHBOUR else XREF-NULL then {: near:ptr :}
   near XREF-FOUND? 0= if near AEREC-TXT exit then
   near AEREC-TXT  43 AE1  addr near REC-CODE-PTR@ - AEJNUM ;

\ A DATA address: the word whose data the value is when a record spells it
\ exactly, else the nearest spelled data below as `NAME+off`, else `<unknown>`.
: TARGET-TXT ( n -- ) {: v:n :}
   v DATA-CELL-OWNER {: exact:ptr :}
   exact XREF-FOUND? if exact AEREC-TXT exit then
   v DATA-NEIGHBOUR {: near:ptr at:n :}
   near XREF-FOUND? 0= if near AEREC-TXT exit then
   near AEREC-TXT  43 AE1  v at - AEJNUM ;

: REFUSE-DATA-SPAN ( ptr n ptr u8 n -- ) {: owner:ptr site:ptr v:n :}
   s" aot: address refers to data outside the restored span caller=" AETXT
   owner site CODE-ADDR-TXT
   s"  region-off=" AETXT site AOT-DBASE@ - AEJNUM
   s"  value=" AETXT v AEJNUM
   s"  target=" AETXT v TARGET-TXT
   s"  span=[" AETXT BLOB-SRC @ AEJNUM
   44 AE1 BLOB-END @ AEJNUM
   93 AE1 10 AE1
   s" " 74 die ;

\ Refused with the span scan's three facts from the same predicate; the reason
\ differs because this cell is outside the restored span, so even a declared xt
\ cell has nothing to be patched in.
: CHECK-DATA-CELL ( n -- ) {: cell:n :}
   cell DATA-CELL@ {: v:n :}
   v CELL-TEXTPTR? if cell v REFUSE-UNRESTORED-CELL then ;

\ The end is a valid one-past address for a zero-length buffer, so the bound
\ admits it; relocation preserves an address and does not certify an access.
: IN-WINDOW? ( n -- bool ) {: v:n :}
   v BLOB-SRC @ >= v BLOB-END @ <= and ;

\ A claim's declared bytes [cell, cell + length), whatever its kind:
\ src/habu/aot-owned-cells.f is the list. A buffer is reached at interior
\ offsets as often as at its head (src/core/util.f PATHZ writes the NUL at PZB +
\ u); a value merely next to a claim is refused: with PZB claimed for PATH-CAP +
\ 1 bytes, PZB + 8 links and RDP at PZB + 1032 is refused by that name.
\ Relocation preserves a claimed address because DATA is one mapping at DATA-VA
\ in engine and image, and aot-link-arm.f EMIT-OWNED-CELLS reads this table.
\ The carried map and the refusal check share this predicate, so the check
\ admits exactly what the map answered for.
: CLAIMED-AT? ( n n -- bool ) {: i:n v:n :}
   v i AOT-OWNED:AT >=  v i AOT-OWNED:AT i AOT-OWNED:LEN + < and ;

\ An address inside a carried claim names the copy: aot-common.f CARRY-CELLS
\ has copied [cell, cell+length) into the carried run, and the interior offset
\ is preserved, so `KK i cells +` maps as well as `KK`.
: CARRIED-IN? ( n n -- bool ) {: i:n v:n :}
   i AOT-OWNED:CARRIED? 0= if false exit then
   i v CLAIMED-AT? ;

: CARRIED-TARGET ( n -- n ) {: v:n :}                 \ the copy's address, or -1
   AOT-OWNED:N 0 ?do
      i v CARRIED-IN? if i AOT-OWNED:DEST v i AOT-OWNED:AT - + unloop exit then
   loop -1 ;
\ Any claim whose declared bytes cover the address admits it.
\ above.
: CLAIMED-CELL? ( n -- bool ) {: v:n :}
   AOT-OWNED:N 0 ?do
      i v CLAIMED-AT? if true unloop exit then
   loop false ;

\ The image's answer for a DATA address, and whether any claim covers it. Both
\ roads into a below-window address use this one map (a code-spelled address:
\ SCAN-ADDRESS, aot-link-arm.f COPY-ADDRESS; a declared DATA cell: XTD-ROW), so
\ one road never ships verbatim what the other rewrites. An in-window value
\ answers itself (the window is restored where captured); a value in a carried
\ claim answers with the copy at the same offset; a literal interned in another
\ pool is re-interned into the application's. The rest is refused by its road's
\ site: a code site or the cell.
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

\ A carried claim's bytes and an immutable literal row are copied into the
\ window (the carried run, the capture's active pool) and the address rewritten
\ to the copy. Other pre-window DATA has no ownership proof: DATA-ADDRESS!
\ refuses it with this site's three facts.
: DATA-TARGET ( ptr n ptr u8 n -- n ) {: owner:ptr site:ptr v:n :}
   v MAPPED-DATA {: w:n ok:bool :}
   ok 0= if owner site w DATA-ADDRESS! then
   w ;

\ A persisted cell holds a code address because it was declared to, never
\ because its value looks like one. The address-cell table (src/habu/layout.f
\ package SNAP-RELOC, src/habu/address-cells.f) holds `defer`'s dispatch cell,
\ `is`'s target and `xt!`'s run-time cell, the authority the snapshot writer and
\ the AOT capture relocate from. This pass reads the XT-kind rows inside the
\ capture window: what the span scan must not refuse and what the image carries.
\ A zero row is an unbound cell with nothing to relocate. A DATA-kind row is not
\ collected: XTD-ROW maps its cell in place and it travels in the data blob.
1024 constant MAX-XTCELL
create XTC-OFF MAX-XTCELL cells allot     \ each declared cell's DATA offset
create XTC-VAL MAX-XTCELL cells allot     \ ... and the code address it holds
variable XTC-N  variable XTC-CX  variable XTC-I  variable XTC-J

: XTC-OFF@ ( n -- n ) cells XTC-OFF + @ ;
: XTC-VAL@ ( n -- n ) cells XTC-VAL + @ ;
: XTC-CELL ( n -- n ) XTC-OFF@ DATA-VA VA>N + ;
: XTC-SET ( n n n -- ) {: k:n off:n v:n :}
   off k cells XTC-OFF + !  v k cells XTC-VAL + ! ;

\ Ascending by DATA offset, the span scan's order, so its membership test is a
\ cursor. Insertion sort keeps the two arrays in step with no scratch.
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

\ The whole cell is inside the restored span; a cell straddling the window's end
\ keeps the unrestored-cell refusal.
: XTC-IN-WINDOW? ( n -- bool ) {: at:n :}
   at BLOB-SRC @ >= at 8 + BLOB-END @ <= and ;

\ A declared DATA cell's value: in-window needs nothing (restored where
\ captured); below the window (engine DATA, not restored) goes through
\ MAPPED-DATA, and an address no claim covers is refused by its cell. The rewrite
\ is stored into the live window cell before aot-common.f BLOB-SRC@ reads the
\ blob, so no second pass patches it. A value that is not a DATA address is a
\ zero, a scalar or a code or dict address; the last is refused here by the span
\ scan's predicate, because the scan (aot-common.f AOT-DATA-TEXTPTR-CHECK) skips
\ declared cells.
: XTD-ROW ( n -- ) {: at:n :}
   at DATA-CELL@ {: v:n :}
   v DATA-ADDRESS? 0= IF
      v CELL-MAPPED? IF at v REFUSE-MAPPED-CELL THEN
      exit THEN
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

\ The span scan asks in ascending order, so the cursor only moves forward. The
\ test is overlap, not equality (layout.f XTCELL-OFF-MAX: a cell reached through
\ `allot` after byte data keeps its residue).
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
   s"  region-off=" AETXT p AOT-DBASE@ - AEJNUM
   s"  words=" AETXT
   e p - 4 / 4 min 0 max 0 ?do
      i 0<> if 32 AE1 then
      p i 4 * + AOT-W32@ AEJNUM
   loop
   10 AE1 s" " 74 die ;

: SCAN-ADDRESS ( ptr n ptr u8 ptr u8 -- ) {: caller:ptr p:ptr e:ptr :}
   p e SITE-LITERAL {: size:n v:n :}
   size 0= if caller p e REFUSE-ADDRESS-SITE then
   v DATA-ADDRESS? if
      caller p v DATA-TARGET drop exit
   then
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if caller owner SCAN-CALLEE exit then
   v SPAN-OWNER {: k:n :}
   k 0 < if s" aot: code address has no dictionary owner" 74 die then
   k ADD-SPAN-CLO ;

\ A direct branch (BL or B) whose target lies outside the member is a call and
\ is followed; one landing inside is intra-record control flow. Two emitters
\ plant an outward B at a callee's entry: habu2.f DOESPATCH:EMIT (a created
\ word's RET becomes a `b` to its `;does` companion, elaborate.f DOES-NAME) and
\ the tail call (emit.f PUT-TAILCALL). The target must join the closure or
\ aot-common.f MAP-TARGET! refuses the branch. A callee with no record answers
\ from the span table; an entry neither table knows is left for the relocation
\ pass to refuse by name. A declaration-only target is never a member
\ (AOT-DECLARATION?).
: SCAN-TARGET ( ptr n ptr u8 ptr u8 ptr u8 -- ) {: caller:ptr t:ptr mstart:ptr mend:ptr :}
   t mstart >= t mend < and if exit then
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? if
      callee AOT-DECLARATION? callee AOT-DP-REFUSAL? or if exit then
      caller callee SCAN-CALLEE exit
   then
   t SPAN-AT-ENTRY {: k:n :}
   k 0 >= if k ADD-SPAN-CLO then ;

: SCAN-DIRECT ( ptr n ptr u8 ptr u8 ptr u8 -- ) {: caller:ptr p:ptr mstart:ptr mend:ptr :}
   p AOT-W32@ dup DIRECT? 0= if drop exit then
   p swap TARGET caller swap mstart mend SCAN-TARGET ;

: SIGNED32 ( n -- n )
   dup $80000000 and 0<> if $100000000 - then ;
: SIGNED8 ( n -- n )
   dup $80 and 0<> if 256 - then ;

variable SM-I

\ x86 sites point at their displacement or immediate field and the site table
\ states their widths, so no instruction-byte walk is needed for inline data.
: SCAN-X64-SITE ( ptr u8 n -- ) {: field:ptr kind:n :}
   kind SNAP-RELOC:SITE-ADDR = kind 5 = or if
      SM-I @ CLO-REC@ field 2 - SEND @ SCAN-ADDRESS exit
   then
   kind SNAP-RELOC:SITE-CALL = kind 4 = or if
      field LE:U32@ SIGNED32 field 4 + + {: target:ptr :}
      SM-I @ CLO-REC@ target SM-I @ CLO-AT SEND @ SCAN-TARGET exit
   then
   kind 3 = if
      field c@ SIGNED8 field 1+ + {: target:ptr :}
      SM-I @ CLO-REC@ target SM-I @ CLO-AT SEND @ SCAN-TARGET exit
   then
   s" aot: unknown x86 text site kind" 74 die ;

\ A site's literal is scanned before its word's branch. SM-I names the member to
\ the per-site word.
: SCAN-DIRECT-TO ( ptr u8 -- ) {: p:ptr :}
   BEGIN SP2 @ p < WHILE
      SM-I @ CLO-REC@ SP2 @ SM-I @ CLO-AT SEND @ SCAN-DIRECT
      SP2 @ 4 + SP2 !
   REPEAT ;
: MEMBER-SITE ( n n -- ) {: off:n kind:n :}
   kind SNAP-RELOC:SITE-ADDR <> if exit then
   off SITE-PTR {: p:ptr :}
   p SCAN-DIRECT-TO
   SM-I @ CLO-REC@ p SEND @ SCAN-ADDRESS ;
: SCAN-MEMBER {: i:n :} ( n -- )
   i SM-I !
   i CLO-AT SP2 !  i CLO-AT i CLO-BYTES + SEND !
   HB-TARGET-LINUX-X86-64? if
      i CLO-AT SEND @ [: SCAN-X64-SITE ;] EACH-MEMBER-SITE exit
   then
   i CLO-AT SEND @ [: MEMBER-SITE ;] EACH-SITE
   SEND @ SCAN-DIRECT-TO ;
variable WI
: NO-ENTRY-DIE ( -- )
   s" aot: entry word not found: " AETXT  ENTRY-NAME$ AETXT  10 AE1
   s" aot: no entry" 74 die ;

\ A record carries its entry as a pointer and the walk carries addresses as
\ integers: the crossing is the dictionary base, published in both domains
\ (AOT-DBASE@, AOT-DBASE-N), plus the distance from it, checked arithmetic and
\ not a cast. aot-common.f CLO-AT-N crosses here too.
: CODE-N ( ptr u8 -- n ) {: p:ptr :}
   p AOT-DBASE@ -  AOT-DBASE-N + ;
: REC-ENTRY-N ( ptr n -- n ) {: r:ptr :}
   r REC-CODE-PTR@ CODE-N ;
: REC-END-N ( ptr n -- n ) {: r:ptr :}  r REC-ENTRY-N r REC-BYTES + ;
: CODE-AT ( ptr n n -- ptr u8 ) {: r:ptr v:n :}
   r REC-CODE-PTR@  v r REC-ENTRY-N -  + ;

\ The same crossing through the record that names the address or the span
\ table; an address neither can place is refused rather than guessed.
: CODE-PTR ( n -- ptr u8 ) {: v:n :}
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if owner v CODE-AT exit then
   v SPAN-OWNER {: s:n :}
   s 0 < if s" aot: code address has no dictionary owner" 74 die then
   s SPAN-START  v s SPAN-START-N -  + ;

\ Where an anonymous body ends. One record covers the word's function followed
\ by its quotations' functions (src/compiler/native/emit.f FUNCTION-OFFSET@), so
\ a body ends at the smallest recorded code-address literal above it inside the
\ record (src/habu/sites.f records each quotation entry a `[: ;]` pushes), or
\ at the record's end; no byte decoding. Under tier 1 a quotation reference is
\ an ADR, not a chain, so a body runs to its record's end: measured, `: INSTALL (
\ -- ) [: INC ;] is A  [: DEC ;] is B ;` gives a 32-byte first body in a
\ 116-byte record, holding the second quotation's 16 bytes too.
variable BODY-END
: BODY-SITE ( n n -- )
   RW-LITERAL {: size:n w:n :}
   size 0= if exit then
   w RW-V @ > w BODY-END @ < and if w BODY-END ! then ;
: BODY-END-SCAN ( ptr n n -- ) {: r:ptr v:n :}
   r REC-END-N BODY-END !
   r v [: BODY-SITE ;] REC-WALK ;

\ The defining word is not pulled in: a `[: ... ;] is X` initializer runs at
\ load time and declares an address cell, which a stripped image has no
\ registrar for. The record travels with the member only for diagnostics; no
\ address resolves through it.
: ADD-BODY-CLO ( ptr n n -- ) {: r:ptr v:n :}
   r v BODY-END-SCAN
   r  r v CODE-AT  BODY-END @ v -  ADD-CLO ;

\ A declared xt cell is a closure root, resolved like SCAN-ADDRESS resolves a
\ code literal: entry, else containing span (an anonymous body), else the span
\ table. It joins before the walk, so the strip keeps the code a persistent cell
\ points at; a target no table can place is refused by name.
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

\ A nested body is carried once, which keeps members disjoint: one member
\ answers for an address and aot-common.f OLD>NEW maps a target by containment.
\ Under tier 1 the lower of two bodies of one record runs to the record's end
\ and holds the higher one; this pass drops the contained one, whose cell
\ resolves through the container's copy (aot-link-arm.f COPY-COMPACT-BLOB).
\ Keeping both put the code in twice with the inner row in whichever copy the
\ walk reached first; dropping the container would lose the code between them.
\ Bounded: bodies come only from XT-CELL-ROOT before the walk, so a contained
\ body is one of rows 1..XTC-N (row 0 is MAIN's record), and its container names
\ the same record (ADD-BODY-CLO, ADD-REC-CLO): O(XTC-N x NCLO).
\ No partial overlap: BODY-END-SCAN makes two bodies of one record disjoint or
\ share their end. A record inside another (a `;does` companion runs to its
\ emission's end: measured [26247860,26247960) and [26247936,26247960)) needs
\ `create`, which the link refuses (exit 70 "stripped AOT unsupported word
\ 'create' called by ..."); any overlap left is a table defect, refused by
\ aot-common.f MEMBER-ORDER.
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
\ The compaction moves rows, so the pass ends by reindexing the survivors.
: DROP-NESTED-CLO ( -- )
   1 CLO-NI !
   BEGIN CLO-NI @ NCLO @ <  CLO-NI @ XTC-N @ <=  and WHILE
      CLO-NI @ CLO-CONTAINED? IF CLO-NI @ DROP-CLO-ROW ELSE CLO-NI @ 1+ CLO-NI ! THEN
   REPEAT
   CLO-SET-REBUILD ;

\ The capacity bounds the closure: a member is its entry (ADD-CLO dedups on it),
\ and an entry is a record's (ADD-REC-CLO, at most ndict@), a stripped span's
\ start (ADD-SPAN-CLO, at most SPAN-N) or an anonymous body (ADD-BODY-CLO, only
\ from XT-CELL-ROOT, at most XTC-N). All three are final before the walk: the
\ dictionary is closed, the span table published, and aot-common.f LINK runs
\ COLLECT-XT-CELLS before CLOSURE.
: CLO-CAPACITY ( -- n ) ndict@ SPAN-N + XTC-N @ + ;
: CLO-LIMIT-ABOVE-DIE ( n n -- ) {: req:n cap:n :}
   s" aot: CLO-LIMIT " AETXT req AEJNUM
   s"  above the closure capacity " AETXT cap AEJNUM
   s"  = records " AETXT ndict@ AEJNUM
   s"  + stripped spans " AETXT SPAN-N AEJNUM
   s"  + declared xt cells " AETXT XTC-N @ AEJNUM
   10 AE1
   s" aot: CLO-LIMIT above the closure capacity" 74 die ;
\ A request above the capacity is refused where the two first meet: at the
\ sizing for a request made before it (a fixture lowers the limit while its
\ source loads), at the call for one made after.
: CLO-LIMIT-RESOLVE ( -- )
   CLO-REQ @ 0= IF CLO-CAP @ CLO-LIMIT ! exit THEN
   CLO-REQ @ CLO-CAP @ > IF CLO-REQ @ CLO-CAP @ CLO-LIMIT-ABOVE-DIE THEN
   CLO-REQ @ CLO-LIMIT ! ;
: CLO-LIMIT! {: n:n :}
   n 1 < IF s" aot: CLO-LIMIT below 1" 74 die THEN
   n CLO-REQ !
   CLO-CAP @ 0<> IF CLO-LIMIT-RESOLVE THEN ;
\ A test that fills the tables by hand asks for the rows it writes.
: CLO-TABLES ( n -- ) {: rows:n :}
   rows 1 < IF s" aot: closure capacity below 1" 74 die THEN
   rows CLO-RESERVE  rows CLO-LEN-RESERVE  rows CLO-REC-RESERVE
   rows CLO-SET-SIZE
   rows CLO-CAP !
   CLO-LIMIT-RESOLVE ;

: CLOSURE  CLO-CAPACITY CLO-TABLES
   0 NCLO !  FINDMAIN dup 0= IF drop NO-ENTRY-DIE THEN  dup ROOTREC !  ADD-REC-CLO
   XT-CELL-ROOTS
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ SCAN-MEMBER  WI @ 1+ WI ! REPEAT
   DROP-NESTED-CLO ;

;using
;using
;package
