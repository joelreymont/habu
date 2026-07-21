\ aot-closure.f - stripped AOT closure analysis and diagnostics.

\ The toolchain hook is on when this file is appended and stays on: this file
\ compiles checked, with its raw-pointer boundaries as explicit TRUST rows.
\ The driver installs USER-HOOK below for user source only.

\ Checker-internal surface used by AOT diagnostics and the driver hook: the
\ checker registry does not publish its own words to later checked loads
\ (same boundary class as verify-source.f CHECK-BODY), so the two entrypoints
\ the AOT tail needs are typed here as axioms.
s" JSON-DIAGS" s" -- ptr a" TRUST
s" CHECK!" s" ptr u8 n -- n" TRUST

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
\ direct-BL closure scan (SCAN-DIRECT) and the linker's fail-closed abs-chain guard
\ (aot-lib.f ABS-CHAIN?). The absolute movz/movk/movk x16 + blr x16 call form is no
\ longer recognized here: every native call is a direct BL, so the closure walks BL
\ only and the linker narrows to direct-BL-only input.
: AOT-W32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

\ Decode an AArch64 direct branch (B / BL). Both share opcode bits: masking off
\ the link bit leaves $14000000, so DIRECT? recognizes B and BL and excludes the
\ conditional/compare branches (BCOND/CBZ/TBZ), which stay intra-record and are
\ never followed as calls. TARGET returns the branch's absolute code address =
\ site + sign-extended(imm26) * 4.
package AOT-BRANCH

$7C000000 constant MASK
$14000000 constant OPCODE
$3FFFFFF constant DELTA-MASK
$2000000 constant SIGN
$FC000000 constant CALL-MASK
$94000000 constant CALL-OP

: SIGNED ( n -- n ) SIGN xor SIGN - ;

public

: DIRECT? ( n -- bool ) MASK and OPCODE = ;
\ A `BL imm26` — the one native call form (habu2.f LCEMITBL). Distinguished from a
\ plain `B` (same opcode minus the link bit) so intra-record control flow is skipped.
: CALL? ( n -- bool ) CALL-MASK and CALL-OP = ;

: TARGET ( ptr u8 n -- ptr u8 ) {: p:ptr w:n :}
   p w DELTA-MASK and SIGNED 4 * + ;

;package

: REC {: k:n :} ( n -- ptr a )
   AOT-DBASE@ k 48 * + ;          \ dict record k  (0:addr 8:len 16:name-len|flags 24:name|ptr)
: AOT-FOLD {: c:n :}  c 64 > c 91 < and IF c 32 + ELSE c THEN ;
: REC-NAME-LEN {: r:ptr :} ( ptr a -- n )
   r 16 + @ DNAME-LEN-MASK and ;
: REC-NAME-PTR {: r:ptr :} ( ptr a -- ptr a )
   r 16 + @ DNAME-EXT and 0= IF r 24 + ELSE r 24 + AOT-PTR@ THEN ;
: REC-NAME@ {: r:ptr :} ( ptr a -- ptr a n )
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
\ helper so the stripped image starts at a non-MAIN root (docs/census-tfam-10.md
\ Category 1/6: identity is resolved by record here, not carried as a bare name).
create ENTRY-NAME-BUF 64 allot   variable ENTRY-NAME-U
: ENTRY-NAME$ ( -- ptr u8 n )
   ENTRY-NAME-BUF ENTRY-NAME-U @ ;
: ENTRY-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1 < IF s" aot: empty entry name" 74 die THEN
   u 64 > IF s" aot: entry name too long" 74 die THEN
   a ENTRY-NAME-BUF u BYTE-COPY
   u ENTRY-NAME-U ! ;
s" MAIN" ENTRY-NAME!
: MAIN? {: r:ptr :} ( ptr a -- bool )
   r ENTRY-NAME$ REC-NAME= ;
\ Data-space words (@ ! c@ c! here allot , c,) are now supported: the AOT entry
\ maps the fixed DATA region and restores the program's persistent data (see
\ aot-lib.f). Only words that need machinery the stripped binary does not carry
\ stay rejected — the closure walk distinguishes runtime use from compile-time
\ definition, so `create` here means runtime dictionary creation (no dictionary),
\ `compile,` means runtime compilation (no compiler), `patch32` writes the code
\ region (stripped __text is r-x and not at RBASE-VA).
: AOT-UNSAFE? {: r:ptr :} ( ptr a -- bool )
   r s" create" REC-NAME= IF 0 0= EXIT THEN
   r s" compile," REC-NAME= IF 0 0= EXIT THEN
   r s" patch32" REC-NAME= IF 0 0= EXIT THEN
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
   s" stripped AOT cannot run create/compile,/patch32 at runtime; use --repl or remove the word from the runtime path" AEJSTR
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
: REC-WID@ ( ptr a -- n ) {: r:ptr :}  r 40 + @ ;

: FINDMAIN ( -- ptr a )  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC MAIN? IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  XREF-NULL ;

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
: SCAN-CALLEE ( ptr a ptr a -- ) {: caller:ptr callee:ptr :}
   callee XREF-FOUND? 0= if exit then
   callee dup AOT-UNSAFE? if caller swap AOT-UNSAFE-DIE then
   ADD-CLO ;

\ Resolve a call target (a code address) to its record by EXACT code entry: scan the
\ dict records and match on the code-entry pointer directly (REC-CODE-PTR@) so a
\ direct-BL target needs no address-to-cell cast. Ordinary words and registered engine
\ helpers both carry a record; a non-entry address matches nothing (fails closed later).
: FINDADDR-PTR ( ptr u8 -- ptr a ) {: t:ptr :}  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC REC-CODE-PTR@ t = IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  XREF-NULL ;

\ Follow a direct BL (the one native call form) to its callee; leave everything
\ else (a plain B, conditional/compare branches, intra-record jumps) untouched.
: SCAN-DIRECT ( ptr a ptr u8 -- ) {: caller:ptr p:ptr :}
   p AOT-W32@ dup AOT-BRANCH:CALL? if
      p swap AOT-BRANCH:TARGET FINDADDR-PTR caller swap SCAN-CALLEE
   else
      drop
   then ;

: SCAN-REC {: r:ptr :} ( ptr a -- )
   r @ SP2 !  r @ r 8 + @ + SEND !
   BEGIN SP2 @ SEND @ < WHILE
      r SP2 @ SCAN-DIRECT
      SP2 @ 4 + SP2 !
   REPEAT ;
variable WI
: NO-ENTRY-DIE ( -- )
   s" aot: entry word not found: " AETXT  ENTRY-NAME$ AETXT  10 AE1
   s" aot: no entry" 74 die ;
: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop NO-ENTRY-DIE THEN  dup ROOTREC !  ADD-CLO
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ cells CLO + @ SCAN-REC  WI @ 1+ WI ! REPEAT ;
