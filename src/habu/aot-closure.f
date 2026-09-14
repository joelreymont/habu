\ aot-closure.f - stripped AOT closure analysis and diagnostics.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

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
: AOT-PTR@ {: a:ptr :} ( ptr a -- ptr a )
   a @ ;
s" AOT-PTR@" s" ptr a -- ptr a" TRUST

\ --- read a little-endian 32-bit instruction word from a code pointer. Used by the
\ direct-BL closure scan and the linker's address-chain validation.
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
variable BLOB-SRC  variable BLOB-END  variable BLOB-LEN  variable BLOB-LBL
: DATA-ADDRESS! ( n -- ) {: v:n :}
   \ The end is a valid one-past pointer for a zero-length buffer. Relocation
   \ preserves the address; it does not certify a later memory access.
   v BLOB-SRC @ < v BLOB-END @ > or if
      s" aot: address refers to data outside the restored span" 74 die
   then ;

\ Immutable literal rows can be copied into the capture's active pool. Other
\ pre-window DATA has no ownership proof and must keep the link refusal.
: DATA-TARGET ( n -- n ) {: v:n :}
   v BLOB-SRC @ >= v BLOB-END @ <= and if v exit then
   v NSTR:REINTERN-OWNED if dup DATA-ADDRESS! exit then
   dup DATA-ADDRESS! ;

\ Decode an AArch64 direct branch (B / BL). Both share opcode bits: masking off
\ the link bit leaves $14000000, so DIRECT? recognizes B and BL and excludes the
\ conditional/compare branches (BCOND/CBZ/TBZ), which stay intra-record and are
\ never followed as calls. TARGET returns the branch's absolute code address =
\ site + sign-extended(imm26) * 4.
$7C000000 constant MASK
$14000000 constant OPCODE
$3FFFFFF constant DELTA-MASK
$2000000 constant SIGN
$FC000000 constant CALL-MASK
$94000000 constant CALL-OP

: SIGNED ( n -- n ) SIGN xor SIGN - ;

: DIRECT? ( n -- bool ) MASK and OPCODE = ;
\ A `BL imm26` — the one native call form (habu2.f LCEMITBL). Distinguished from a
\ plain `B` (same opcode minus the link bit) so intra-record control flow is skipped.
: CALL? ( n -- bool ) CALL-MASK and CALL-OP = ;

: TARGET ( ptr u8 n -- ptr u8 ) {: p:ptr w:n :}
   p w DELTA-MASK and SIGNED 4 * + ;

: REC {: k:n :} ( n -- ptr a )
   AOT-DBASE@ k 48 * + ;          \ dict record k  (0:addr 8:len 16:name-len|flags 24:name|ptr)
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
   s" word" AEJKEY caller REC-NAME@ AEJSTR 44 AE1
   s" token" AEJKEY callee REC-NAME@ AEJSTR 44 AE1
   s" reason" AEJKEY s" stripped AOT has no runtime compiler, dictionary, or writable code" AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" stripped AOT cannot run create/patch32 at runtime; use --repl or remove the word from the runtime path" AEJSTR
   125 AE1 10 AE1 ;
: AOT-UNSAFE-PROSE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   s" hb-build: stripped AOT unsupported word '" AETXT
   callee REC-NAME@ AETXT
   s" ' called by '" AETXT
   caller REC-NAME@ AETXT
   s" '" AETXT 10 AE1 ;
: AOT-UNSAFE-DIE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   JSON-DIAGS @ IF caller callee AOT-UNSAFE-JSON ELSE caller callee AOT-UNSAFE-PROSE THEN
   s" hb-build: AOT unsupported word" 70 die ;

variable FX
: REC-CODE-PTR ( ptr a -- ptr ptr u8 ) {: r:ptr :}  r 0 ptr-field ;
: REC-CODE-PTR@ ( ptr a -- ptr u8 )  REC-CODE-PTR @ ;
: REC-WID@ ( ptr n -- n ) {: r:ptr :}  r 40 + @ ;

: FINDMAIN ( -- ptr n )
   ENTRY-NAME$ XREF-FIND ;

\ --- closure: BFS from MAIN over the native call graph. CLO and the parallel
\ COPY/RELOCATE arrays (NEWOFF/BLEN) are all sized by MAX-CLO; ADD-CLO fails
\ closed at the cap so a large closure can never write past the tables.
1024 constant MAX-CLO
create CLO MAX-CLO cells allot   variable NCLO  variable CLO-CX
variable ROOTREC
variable CLO-LIMIT
: CLO-LIMIT! {: n:n :}
   n 1 < IF s" aot: CLO-LIMIT below 1" 74 die THEN
   n MAX-CLO > IF s" aot: CLO-LIMIT above MAX-CLO" 74 die THEN
   n CLO-LIMIT ! ;
MAX-CLO CLO-LIMIT!
: IN-CLO? {: r:ptr :} ( ptr a -- bool )
   0 CLO-CX ! BEGIN CLO-CX @ NCLO @ < WHILE CLO-CX @ cells CLO + @ r = IF 0 0= exit THEN CLO-CX @ 1+ CLO-CX ! REPEAT 0 0= 0= ;
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
: ADD-CLO {: r:ptr :} ( ptr a -- )
   r IN-CLO? IF exit THEN
   NCLO @ CLO-LIMIT @ >= IF r CLO-OVERFLOW-DIE THEN
   r NCLO @ cells CLO + !  NCLO @ 1+ NCLO ! ;
variable SP2  variable SEND
: SCAN-CALLEE ( ptr n ptr n -- ) {: caller:ptr callee:ptr :}
   callee XREF-FOUND? 0= if exit then
   callee dup AOT-UNSAFE? if caller swap AOT-UNSAFE-DIE then
   ADD-CLO ;

\ Resolve a call target (a code address) to its record by EXACT code entry: scan the
\ dict records and match on the code-entry pointer directly (REC-CODE-PTR@) so a
\ direct-BL target needs no address-to-cell cast. Ordinary words and registered engine
\ helpers both carry a record; a non-entry address matches nothing (fails closed later).
: FINDADDR-PTR ( ptr u8 -- ptr n ) {: t:ptr :}  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC REC-CODE-PTR@ t = IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  XREF-NULL ;

\ Ticks may name engine-text entries; anonymous bodies can be interior to their
\ owner's emission. Use exact entry identity first, then the recorded span
\ (record length excludes its final RET), never a nearest-address heuristic.
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
         t r @ >= if t r @ - r 8 + @ 4 + < if r unloop exit then then
      then
   loop XREF-NULL ;

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
      v DATA-TARGET drop exit
   then
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? 0= if s" aot: code address has no dictionary owner" 74 die then
   caller owner SCAN-CALLEE ;

\ Follow a direct BL (the one native call form) to its callee; leave everything
\ else (a plain B, conditional/compare branches, intra-record jumps) untouched.
: SCAN-DIRECT ( ptr n ptr u8 -- ) {: caller:ptr p:ptr :}
   p AOT-W32@ dup CALL? if
      p swap TARGET FINDADDR-PTR caller swap SCAN-CALLEE
   else
      drop
   then ;

: SCAN-REC {: r:ptr :} ( ptr a -- )
   r @ SP2 !  r @ r 8 + @ + 4 + SEND !
   BEGIN SP2 @ SEND @ < WHILE
      r SP2 @ SEND @ SCAN-ADDRESS
      r SP2 @ SCAN-DIRECT
      SP2 @ 4 + SP2 !
   REPEAT ;
variable WI
: NO-ENTRY-DIE ( -- )
   s" aot: entry word not found: " AETXT  ENTRY-NAME$ AETXT  10 AE1
   s" aot: no entry" 74 die ;
: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop NO-ENTRY-DIE THEN  dup ROOTREC !  ADD-CLO
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ cells CLO + @ SCAN-REC  WI @ 1+ WI ! REPEAT ;

;using
;package
