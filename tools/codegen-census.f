\ codegen-census.f - how many of each ARM64 shape the code-size fixes target.
\
\ Run: <engine> --load tools/codegen-census.f -- <image> <source-commit> <report>
\
\ Walks the whole captured code blob of a tools/native-build.f product in one
\ process - every dictionary record, every aot/code-spans row, and the unowned
\ bytes between them - decodes each instruction and counts the shapes the
\ ARM64 code-size fixes target, per record and in total. <report> is written
\ whole at the end (codegen-census.txt by convention):
\
\   E <sha256> <source-commit> <image-bytes>
\   C <blob-bytes> <records> <spans> <unowned-bytes> <overlapping-owners>
\   P <pattern> <sites> <bytes> <estimated-saving>      one row per pattern
\   I data-carrier <island> <sites> <distinct>         one row per 1 MiB island
\   W <pattern> <name-or-offset> <sites>               top forty records a pattern
\
\ The data-carrier P row carries two more columns, its distinct targets summed
\ over records and summed over islands, and its W rows the record's own
\ distinct count. A record is `PKG:NAME` (a package's public or private word)
\ or `NAME`; an aot/code-spans row is `NAME@<blob offset>`, named by
\ native-build's <image>.names sidecar when one sits beside the image and
\ `@<blob offset>` otherwise; unowned code is `gap@<blob offset>`. The source
\ commit is the caller's statement: the image does not carry one.
\
\ WHY ONE PROCESS OVER THE PRODUCT. tools/tier-dump.f reloads a corpus for
\ every word and cannot name a duplicated private word, and a corpus load is
\ not what ships. The product's own blob is: every byte a later slice changes
\ is in it, and one walk over it answers every pattern at once.
\
\ WHY THIS FILE IS A MEMBER OF PACKAGE IMAGE-SIZE. tools/image-size-lib.f is
\ the one reader of the AOT payload: it finds the payload, proves its framing
\ and decodes the packed record, span and site rows. A second decoder here
\ would drift from it, so this file reopens that package and reads the rows
\ its walk has already bounded (CREC-*, SPAN-*, DSITE-OFF, CHAIN-VALUE, the
\ seeded primitive records). Its own words carry the CG- prefix so they cannot
\ collide with the walker's; a collision is refused at load.
\
\ HOW A SHAPE IS RECOGNISED. An opcode is the tree's own assembler
\ (src/arch/arm64/asm.f) applied to zero operands, compared under the mask of
\ its form's operand fields, and operands are read with the field decoders of
\ src/arch/arm64/disasm.f; so the census matches exactly what the emitter
\ writes. Dataflow is followed inside one basic block only, except for the
\ call-crossing spill below: a block starts at a record's first instruction, at
\ every direct branch target and after every branch or return, and a def search
\ also stops at a call, because the ARM64 description declares the whole
\ register pool destroyed by one. Declared address sites and code-resident DATA
\ cells are never decoded as code.
\
\ THE PATTERNS, in report order. Bytes are what the shape occupies now; the
\ saving is what its fix removes.
\   guarded-division     cbnz xb over a cold refusal into sdiv xq,xa,xb: the
\                        movn, the push and bl throw (20 bytes) today, one
\                        bl (DIV-ZERO) (12) after the fix. Saving: above 12.
\   terminal-only-frame  a record that saves the link (str x30,[sp,#-k]! or a
\                        slot store) and whose every call is bl throw or bl die.
\                        Bytes: the link transfers. Saving: the same, unless a
\                        fused frame also holds other slots, when the push and
\                        pop only become the frame's reserve and release.
\   mask-chain           movz or movn plus at least one movk to its register,
\                        not a declared site, whose value is a logical
\                        immediate. Saving: every instruction but one (orr).
\   constant-shift       lslv or lsrv whose count this block set with one movz
\                        or movn. Bytes: the shifts and their distinct constants.
\                        Saving: the distinct constants, an upper bound: a
\                        constant the region memo shares with another use stays.
\   scaled-index         madd with an addend, one factor of which this block
\                        set to 8 with one movz. Bytes and saving as above.
\   remainder            sdiv q,a,b; mul t,q,b; sub r,a,t in one block. Saving:
\                        4 (msub).
\   signed-maximum       eor t,a,b; cmp a,b; csetm m; and u,t,m; eor r,u,b.
\                        Saving: 12 (cmp; csel).
\   call-crossing-spill  a store to a frame slot, a bl into another routine in
\                        the blob, then a reload of the slot, with no other call
\                        between: not to engine text, through a register or to
\                        the routine's own entry, since a self-call keeps the
\                        whole pool destroyed under clobber summaries too. The
\                        blob is tier-1 code: src/habu/aot-owned.f refuses a
\                        capture window without native provenance. Slot state
\                        follows a block and the fall-through out of a
\                        conditional branch into a block no branch, adr or code
\                        literal names, so every path to a counted reload
\                        passes the store and the call; a br in the blob is
\                        refused. Sites: slot reloads, a floor, since a reload
\                        past a branch target is not followed. Bytes: the
\                        distinct stores and reloads. Saving: the same, reached
\                        only when the callee's clobber summary frees a
\                        register.
\   data-carrier         a declared DATA address site (aot/data-sites, not a
\                        code-resident cell): movz lsl 32; movk lsl 16; movk.
\                        Saving: 12 - 4 per site less an 8-byte pool slot per
\                        distinct target per 1 MiB island, the ldr-literal reach.
\   no-return-fallback   bl die after a declared DATA carrier and the status
\                        ENGINE-ERROR:CODE-CERT since the block's last call: the
\                        DEAD-END trap. Bytes: the carrier through the bl.
\                        Saving: above 24 (carrier, two moves, bl).
\   wide-store-run       w >= 2 bl ! in one block with no other call between,
\                        each after the first preceded, since the one before,
\                        by add #8k (k its position in the run) into a
\                        register other than the data-stack pointer. Bytes:
\                        the measured span, from the first instruction after
\                        the call or block start that precedes the first bl !
\                        through the last one, rather than a fixed 16 bytes a
\                        cell. Saving: the span less the w + 2 instructions one
\                        helper call leaves (w publishes, a movz, a bl), never
\                        below 0.
\
\ docs/engine-size.md names this tool beside tools/engine-size.f.

require lib/fs.f
require lib/sort.f
require src/arch/arm64/asm.f
require src/arch/arm64/disasm.f
require tools/image-size-lib.f

package IMAGE-SIZE
private

64 constant CG-USAGE-RC                         \ sysexits EX_USAGE

\ ---- the patterns ---------------------------------------------------------------
0 constant CG-P-DIV
1 constant CG-P-FRAME
2 constant CG-P-MASK
3 constant CG-P-SHIFT
4 constant CG-P-INDEX
5 constant CG-P-REM
6 constant CG-P-MAX
7 constant CG-P-SPILL
8 constant CG-P-DATA
9 constant CG-P-DEAD
10 constant CG-P-WIDE
11 constant CG-P-N
40 constant CG-TOP

: CG-PATTERN$ ( n -- ptr u8 n ) {: p:n :}
   p CG-P-DIV = if s" guarded-division" exit then
   p CG-P-FRAME = if s" terminal-only-frame" exit then
   p CG-P-MASK = if s" mask-chain" exit then
   p CG-P-SHIFT = if s" constant-shift" exit then
   p CG-P-INDEX = if s" scaled-index" exit then
   p CG-P-REM = if s" remainder" exit then
   p CG-P-MAX = if s" signed-maximum" exit then
   p CG-P-SPILL = if s" call-crossing-spill" exit then
   p CG-P-DATA = if s" data-carrier" exit then
   p CG-P-DEAD = if s" no-return-fallback" exit then
   s" wide-store-run" ;

\ ---- opcodes: the assembler's own words under their forms' masks ----------------
$FFE0FC00 constant CG-RRR-MASK                  \ all but rd, rn, rm (no shift)
$FFE08000 constant CG-RRRA-MASK                 \ ... and the addend
$FF800000 constant CG-MOVW-MASK                 \ all but rd, imm16, hw
$FC000000 constant CG-B26-MASK                  \ all but imm26
$FFFFFC1F constant CG-BLR-MASK                  \ all but rn
$FF000000 constant CG-CB-MASK                   \ all but imm19 and rt
$FFFF0FE0 constant CG-CSET-MASK                 \ all but cond and rd
$FFE0FC1F constant CG-CMP-MASK                  \ all but rn and rm; rd stays xzr
$FFC003E0 constant CG-LSU-MASK                  \ all but rt, rt2 and the offset
$FFE00FFF constant CG-WB-MASK                   \ all but imm9
$FFFFFFE0 constant CG-RT-MASK                   \ all but rt
$FFC00000 constant CG-ADDI-MASK                 \ all but imm12, rn, rd
$F000 constant CG-COND-FIELD

0 0 0 A64ASM:ENC-SDIV constant CG-SDIV
0 0 0 A64ASM:ENC-LSLV constant CG-LSLV
0 0 0 A64ASM:ENC-LSRV constant CG-LSRV
0 0 0 0 A64ASM:ENC-MADD constant CG-MADD
0 0 0 A64ASM:ENC-MUL constant CG-MUL
0 0 0 A64ASM:ENC-SUB constant CG-SUB
0 0 0 A64ASM:ENC-EOR constant CG-EOR
0 0 0 A64ASM:ENC-AND constant CG-AND
0 0 A64ASM:ENC-CMP constant CG-CMP
0 A64ASM:C-EQ A64ASM:ENC-CSETM CG-COND-FIELD invert and constant CG-CSETM
0 A64ASM:C-EQ A64ASM:ENC-CSET CG-COND-FIELD invert and constant CG-CSET
0 0 0 A64ASM:MOVZHW constant CG-MOVZ
0 0 0 A64ASM:MOVKHW constant CG-MOVK
0 0 0 A64ASM:MOVNHW constant CG-MOVN
0 A64ASM:ENC-BL constant CG-BL
0 A64ASM:ENC-BLR constant CG-BLR
0 0 A64ASM:ENC-CBNZ constant CG-CBNZ
0 0 0 A64ASM:ENC-ADDI constant CG-ADDI
0 A64M:DSTACK-GPR CELL A64ASM:ENC-STRPOST constant CG-PUSH
A64M:LINK-GPR A64M:SP-GPR 0 A64ASM:ENC-STRPRE constant CG-LINK-PUSH
A64M:LINK-GPR A64M:SP-GPR 0 A64ASM:ENC-LDRPOST constant CG-LINK-POP
0 A64M:SP-GPR 0 A64ASM:ENC-STR constant CG-SP-STR
0 A64M:SP-GPR 0 A64ASM:ENC-LDR constant CG-SP-LDR
0 A64M:SP-GPR 0 A64ASM:ENC-STRD constant CG-SP-STRD
0 A64M:SP-GPR 0 A64ASM:ENC-LDRD constant CG-SP-LDRD
0 0 A64M:SP-GPR 0 A64ASM:ENC-STP constant CG-SP-STP
0 1 A64M:SP-GPR 0 A64ASM:ENC-LDP CG-LSU-MASK and constant CG-SP-LDP

\ The branch kinds that end a block. B.cond, cbz/cbnz and tbz/tbnz of either
\ width; the assembler writes the 64-bit compare-branch only, but a block
\ boundary is architectural.
$FC000000 constant CG-B-MASK        $14000000 constant CG-B-OP
$FF000010 constant CG-BCOND-MASK    $54000000 constant CG-BCOND-OP
$7E000000 constant CG-CBX-MASK      $34000000 constant CG-CBX-OP
$7E000000 constant CG-TBX-MASK      $36000000 constant CG-TBX-OP
$9F000000 constant CG-ADR-MASK      $10000000 constant CG-ADR-OP

\ ---- fields -----------------------------------------------------------------------
: CG-SX ( n n -- n ) {: v:n bits:n :}
   v 1 bits 1- lshift and 0<> if v 1 bits lshift - else v then ;
: CG-IMM26 ( n -- n ) $3FFFFFF and 26 CG-SX ;
: CG-IMM19 ( n -- n ) 5 rshift $7FFFF and 19 CG-SX ;
: CG-IMM14 ( n -- n ) 5 rshift $3FFF and 14 CG-SX ;
\ adr's byte offset: immhi (bits 23:5) above immlo (bits 30:29), signed.
: CG-ADR-OFF ( n -- n ) {: w:n :}
   w 5 rshift $7FFFF and 2 lshift  w 29 rshift 3 and or  21 CG-SX ;
: CG-IMM7 ( n -- n ) 15 rshift $7F and 7 CG-SX ;
: CG-RA ( n -- n ) 10 rshift 31 and ;
: CG-HW ( n -- n ) 21 rshift 3 and ;

: CG-FORM? ( n n n -- bool ) {: w:n mask:n op:n :} w mask and op = ;
: CG-MOVW? ( n n -- bool ) CG-MOVW-MASK swap CG-FORM? ;
: CG-RRR? ( n n -- bool ) CG-RRR-MASK swap CG-FORM? ;
: CG-BL? ( n -- bool ) CG-B26-MASK CG-BL CG-FORM? ;
: CG-CALL? ( n -- bool ) dup CG-BL? swap CG-BLR-MASK CG-BLR CG-FORM? or ;

\ A load or store: op0 (bits 28:25) is x1x0.
: CG-LS? ( n -- bool ) 25 rshift 5 and 4 = ;

: CG-SAME2? ( n n n n -- bool ) {: a:n b:n c:n d:n :}
   a c = b d = and  a d = b c = and or ;

\ ---- the blob as instruction words -------------------------------------------------
DYNAMIC-BUFFER CG-W n               \ instruction words, index = blob offset / 4
DYNAMIC-BUFFER CG-F u8              \ flag bits per instruction
variable CG-N

1 constant CG-DATA                  \ a declared DATA carrier starts here
2 constant CG-CODE                  \ a declared CODE carrier starts here
4 constant CG-LEAD                  \ a basic block starts here
8 constant CG-HELD                  \ a carrier or a code-resident DATA cell covers this word
16 constant CG-SPILLED              \ a store already counted as a call-crossing spill
32 constant CG-FED-SHIFT            \ a constant already counted as a shift count
64 constant CG-FED-INDEX            \ a constant already counted as a cell scale
128 constant CG-LAND                \ a branch, an adr or a code literal names this word

: CG-W@ ( n -- n ) CG-W @ ;
: CG-F? ( n n -- bool ) {: i:n bit:n :} i CG-F c@ bit and 0<> ;
: CG-F+ ( n n -- ) {: i:n bit:n :} i CG-F c@ bit or i CG-F c! ;

: CG-MARK ( n n -- ) {: t:n bits:n :}
   t 0 >= t CG-N @ < and if t bits CG-F+ then ;
: CG-LEAD+ ( n -- ) CG-LEAD CG-MARK ;

\ A blob offset that names an instruction starts a block no slot state is
\ carried into (CG-FALLS-IN?); one between instructions names data.
: CG-LAND-AT ( n -- ) {: at:n :}
   at 3 and 0= if at 4 / CG-LEAD CG-LAND or CG-MARK then ;

: CG-LOAD ( -- )
   BLOB-LEN @ 3 and 0<> if s" codegen-census: the code blob is not whole instructions" RC die then
   BLOB-LEN @ 4 / CG-N !
   CG-N @ 1+ CG-W-RESERVE  CG-N @ 1+ CG-F-RESERVE
   CG-N @ 0 ?do i 4 * BLOB-W32@ i CG-W !  0 i CG-F c! loop ;

\ ---- declared sites -------------------------------------------------------------------
$80000000 constant CG-CELL-TAG      \ src/habu/aot-decl.f AOT-DSITE-CELL
$7FFFFFFF constant CG-OFF-MASK

: CG-IN-BLOB ( n n -- ) {: off:n bytes:n :}
   off 3 and 0<> off 0 < or off bytes + BLOB-LEN @ > or if
      s" codegen-census: a declared site lies outside the code blob" RC die then ;

: CG-HOLD ( n n -- ) {: off:n bytes:n :}
   off bytes CG-IN-BLOB
   bytes 4 / 0 ?do off 4 / i + CG-HELD CG-F+ loop ;

\ The site's first instruction is proved inside the blob before its carrier is
\ decoded, so an offset past the blob is refused as one, not as a non-carrier.
: CG-CODE-SITE ( n -- ) {: off:n :}
   off 4 CG-IN-BLOB
   BLOB-OFF @ off + CHAIN-SIZE {: size:n :}
   size 0= if s" codegen-census: a declared code site holds no address carrier" RC die then
   off size CG-HOLD  off 4 / CG-CODE CG-F+
   BLOB-OFF @ off + CHAIN-VALUE CODE-B0 @ - CG-LAND-AT ;

: CG-DATA-SITE ( n -- ) {: row:n :}
   row CG-OFF-MASK and {: off:n :}
   row CG-CELL-TAG and 0<> if off CELL CG-HOLD exit then
   off SNAP-RELOC:DATA-CHAIN-BYTES CG-HOLD  off 4 / CG-DATA CG-F+ ;

: CG-MARK-SITES ( -- )
   DSITE-N @ 0 ?do i DSITE-OFF CG-DATA-SITE loop
   CSITE-N @ 0 ?do CSITE0 @ i 4 * + U32@ CG-CODE-SITE loop
   XTSITE-N @ 0 ?do XTSITE0 @ i XTSITE-ROW * + U32@ CG-CODE-SITE loop ;

\ ---- basic blocks -------------------------------------------------------------------
: CG-JUMP? ( n -- bool ) {: w:n :}
   w CG-B-MASK CG-B-OP CG-FORM?  w CG-BCOND-MASK CG-BCOND-OP CG-FORM? or
   w CG-CBX-MASK CG-CBX-OP CG-FORM? or  w CG-TBX-MASK CG-TBX-OP CG-FORM? or ;

\ The instruction a direct non-link branch at i goes to; only called on a jump.
: CG-TARGET ( n n -- n ) {: i:n w:n :}
   w CG-B-MASK CG-B-OP CG-FORM? if i w CG-IMM26 + exit then
   w CG-TBX-MASK CG-TBX-OP CG-FORM? if i w CG-IMM14 + exit then
   i w CG-IMM19 + ;

\ A branch that may fall through: b.cond other than al and nv, cbz, cbnz, tbz
\ or tbnz.
: CG-FORK? ( n -- bool ) {: w:n :}
   w CG-BCOND-MASK CG-BCOND-OP CG-FORM? if w 14 and 14 <> exit then
   w CG-CBX-MASK CG-CBX-OP CG-FORM?  w CG-TBX-MASK CG-TBX-OP CG-FORM? or ;

\ An adr outside a carrier names its target, which may be entered through a
\ register. A br would enter code through one, so the carry across a
\ fall-through (CG-FALLS-IN?) needs a blob without it.
: CG-MARK-LEADERS ( -- )
   CG-N @ 0 ?do
      i CG-HELD CG-F? 0= if
         i CG-W@ {: w:n :}
         w RET-MASK and BR-REG-OP = if
            s" codegen-census: the code blob holds a br, which enters code through a register" RC die
         then
         w CG-ADR-MASK CG-ADR-OP CG-FORM? if i 4 * w CG-ADR-OFF + CG-LAND-AT then
         w CG-JUMP? if i w CG-TARGET CG-LEAD CG-LAND or CG-MARK then
         w CG-JUMP? w TERMINAL-W32? or if i 1+ CG-LEAD+ then
      then
   loop ;

\ ---- owners: records, spans and the gaps between them ------------------------------
DYNAMIC-BUFFER CG-SS n              \ owner start, blob offset
DYNAMIC-BUFFER CG-SE n              \ owner end
DYNAMIC-BUFFER CG-SK n              \ code-index node, or -1 for unowned code
variable CG-NSEG  variable CG-POS  variable CG-UNOWNED  variable CG-OVER

: CG-SEG+ ( n n n -- ) {: at:n end:n k:n :}
   at 3 and 0<> if s" codegen-census: a code owner starts between instructions" RC die then
   at CG-NSEG @ CG-SS !  end CG-NSEG @ CG-SE !  k CG-NSEG @ CG-SK !
   at 4 / CG-LEAD+
   CG-NSEG @ 1+ CG-NSEG ! ;

: CG-GAP ( n -- ) {: at:n :}
   at CG-POS @ > if
      CG-POS @ at -1 CG-SEG+
      at CG-POS @ - CG-UNOWNED +!
      at CG-POS !
   then ;

\ The sorted code index puts a record before a span with the same entry, so an
\ EXPORT alias or a record/span pair is one owner, the record. A body that
\ starts inside another is counted as an overlap; any part of it past the
\ earlier owner's end is its own.
: CG-SEGMENTS ( -- )
   BUILD-CODE-INDEX
   CODE-N @ 2 * 1+ {: cap:n :}
   cap CG-SS-RESERVE  cap CG-SE-RESERVE  cap CG-SK-RESERVE
   0 CG-NSEG !  0 CG-POS !  0 CG-UNOWNED !  0 CG-OVER !
   CODE-N @ 0 ?do
      i RSTART @ {: at:n :}
      i REND @ {: end:n :}
      at CG-POS @ < if
         CG-OVER @ 1+ CG-OVER !
         end CG-POS @ > if CG-POS @ end i RIDX @ CG-SEG+  end CG-POS ! then
      else
         at CG-GAP
         at end i RIDX @ CG-SEG+  end CG-POS !
      then
   loop
   BLOB-LEN @ CG-GAP ;

\ ---- the three engine words a call is classified against ---------------------------
variable CG-THROW  variable CG-DIE  variable CG-STORE

: CG-PRIM ( ptr u8 n -- n ) {: a:ptr u:n :}
   PDICT-N @ 0 ?do
      PDICT @ i PREC * + {: r:n :}
      r PREC-WID 0= if
         r PREC-NAME a u IMAGE-STR= if r PREC-START unloop exit then
      then
   loop
   s" codegen-census: the engine's seeded dictionary lacks throw, die or !" RC die ;

: CG-ENTRIES ( -- )
   s" throw" CG-PRIM CG-THROW !
   s" die" CG-PRIM CG-DIE !
   s" !" CG-PRIM CG-STORE ! ;

0 constant CG-K-TIER1               \ into another routine in the blob
1 constant CG-K-SELF                \ into the scanned owner's own entry
2 constant CG-K-THROW
3 constant CG-K-DIE
4 constant CG-K-STORE
5 constant CG-K-ENGINE              \ any other engine-text word
6 constant CG-K-REG                 \ blr

variable CG-ENTRY                   \ the scanned owner's entry, or -1 for unowned code

\ A call out of the blob names the engine's canonical text coordinates, the
\ ones src/habu/aot-capture.f binds a call site in (BOUND-SITE-TARGET).
: CG-CALLEE ( n n -- n ) {: i:n w:n :}
   w CG-BL? 0= if CG-K-REG exit then
   i w CG-IMM26 + 4 * {: tgt:n :}
   tgt CG-ENTRY @ = if CG-K-SELF exit then
   tgt 0 >= tgt BLOB-LEN @ < and if CG-K-TIER1 exit then
   tgt REGION-OFF + DICT-SIZE + {: canon:n :}
   canon CG-THROW @ = if CG-K-THROW exit then
   canon CG-DIE @ = if CG-K-DIE exit then
   canon CG-STORE @ = if CG-K-STORE exit then
   CG-K-ENGINE ;

\ ---- the tallies --------------------------------------------------------------------
DYNAMIC-BUFFER CG-SITES n
DYNAMIC-BUFFER CG-BYTES n
DYNAMIC-BUFFER CG-SAVE n
DYNAMIC-BUFFER CG-OWN n             \ owner * CG-P-N + pattern: sites
variable CG-SEG                     \ the owner being scanned
variable CG-SEND                    \ its end, as an instruction index

: CG-HIT ( n n n n -- ) {: p:n sites:n bytes:n save:n :}
   sites p CG-SITES +!  bytes p CG-BYTES +!  save p CG-SAVE +!
   sites CG-SEG @ CG-P-N * p + CG-OWN +! ;

\ ---- dataflow inside a block --------------------------------------------------------
variable CG-BLK                     \ the instruction this block starts at
variable CG-CALL0                   \ the first instruction after this block's last call

\ Loads and stores write their transfer register (two for a pair) unless they
\ store or move a vector register, and the base when they write it back.
: CG-LS-WRITES? ( n n -- bool ) {: w:n reg:n :}
   w 27 rshift 7 and {: cls:n :}
   w 26 rshift 1 and 0<> {: vec:bool :}
   cls 5 = if
      w 23 rshift 1 and 0<> w FRN reg = and if true exit then
      w 22 rshift 1 and 0= vec or if false exit then
      w FRD reg =  w CG-RA reg = or exit
   then
   cls 3 = w 24 rshift 1 and 0= and if vec 0= w FRD reg = and exit then
   cls 7 = w 24 rshift 1 and 0= and w 21 rshift 1 and 0= and w 10 rshift 1 and 0<> and
      w FRN reg = and if true exit then
   w 22 rshift 3 and 0= vec or if false exit then
   w FRD reg = ;

\ Conservative: a SIMD or FP instruction counts as writing its rd field.
: CG-WRITES? ( n n -- bool ) {: w:n reg:n :}
   reg 31 = if false exit then
   w 25 rshift 15 and {: op0:n :}
   op0 $E and 8 = if w FRD reg = exit then
   op0 7 and 5 = if w FRD reg = exit then
   op0 7 and 7 = if w FRD reg = exit then
   w CG-LS? if w reg CG-LS-WRITES? exit then
   false ;

\ The instruction of this block that last wrote reg before i, or -1 when the
\ block starts or a call intervenes first.
: CG-DEF ( n n -- n ) {: i:n reg:n :}
   reg 31 = if -1 exit then
   i 1- begin dup CG-BLK @ >= while
      dup CG-W@ {: w:n :}
      w CG-CALL? if drop -1 exit then
      w reg CG-WRITES? if exit then
      1-
   repeat drop -1 ;

: CG-MOVW-VALUE ( n -- n ) {: w:n :} w FI16 w CG-HW 16 * lshift ;

\ The value instruction j sets with one move-wide, and whether it is one.
: CG-CONST1 ( n -- n bool ) {: j:n :}
   j CG-W@ {: w:n :}
   j 1+ CG-N @ < if
      j 1+ CG-W@ {: nx:n :}
      nx CG-MOVK CG-MOVW? nx FRD w FRD = and if 0 false exit then
   then
   w CG-MOVZ CG-MOVW? if w CG-MOVW-VALUE true exit then
   w CG-MOVN CG-MOVW? if w CG-MOVW-VALUE invert true exit then
   0 false ;

\ True the first time a constant feeds this pattern.
: CG-FED ( n n -- bool ) {: j:n bit:n :}
   j bit CG-F? if false exit then
   j bit CG-F+ true ;

\ ---- guarded division ---------------------------------------------------------------
: CG-DIV ( n n -- ) {: i:n w:n :}
   w CG-CB-MASK CG-CBNZ CG-FORM? 0= if exit then
   w CG-IMM19 {: k:n :}
   k 2 <> k 4 <> and if exit then
   i k + CG-SEND @ >= if exit then
   i k + CG-W@ {: d:n :}
   d CG-SDIV CG-RRR? 0= d FRM w FRD <> or if exit then
   i k + 1- CG-W@ CG-BL? 0= if exit then
   k 4 = if
      i 1+ CG-W@ CG-MOVN CG-MOVW? 0= if exit then
      i 2 + CG-W@ CG-RT-MASK CG-PUSH CG-FORM? 0= if exit then
   then
   k 1+ 4 * {: bytes:n :}
   CG-P-DIV 1 bytes bytes 12 - CG-HIT ;

\ ---- terminal-only frame -------------------------------------------------------------
variable CG-LINKS                   \ link transfers in this record
variable CG-LSAVE                   \ ... that save it
variable CG-FUSED                   \ a pre-index link push was seen
variable CG-SPT                     \ other loads and stores off sp
variable CG-NBL                     \ bl instructions
variable CG-NBAD                    \ calls that return: anything but bl throw or bl die

\ 0 not a link transfer, 1 the fused push, 2 a slot save, 3 a restore.
: CG-LINK-KIND ( n -- n ) {: w:n :}
   w CG-WB-MASK CG-LINK-PUSH CG-FORM? if 1 exit then
   w CG-WB-MASK CG-LINK-POP CG-FORM? if 3 exit then
   w FRD A64M:LINK-GPR <> if 0 exit then
   w CG-LSU-MASK CG-SP-STR CG-FORM? if 2 exit then
   w CG-LSU-MASK CG-SP-LDR CG-FORM? if 3 exit then
   0 ;

: CG-FRAME-STEP ( n -- ) {: w:n :}
   w CG-LS? w FRN A64M:SP-GPR = and 0= if exit then
   w CG-LINK-KIND {: k:n :}
   k 0= if 1 CG-SPT +! exit then
   1 CG-LINKS +!
   k 1 = if -1 CG-FUSED ! then
   k 3 <> if 1 CG-LSAVE +! then ;

: CG-FRAME-END ( -- )
   CG-LSAVE @ 0= CG-NBL @ 0= or CG-NBAD @ 0<> or if exit then
   CG-LINKS @ 4 * {: bytes:n :}
   CG-FUSED @ 0<> CG-SPT @ 0<> and if 0 else bytes then {: save:n :}
   CG-P-FRAME 1 bytes save CG-HIT ;

\ ---- mask chain ------------------------------------------------------------------------
variable CG-V  variable CG-J

: CG-MOVK-OF? ( n n -- bool ) {: j:n rd:n :}
   j CG-SEND @ >= if false exit then
   j CG-LEAD CG-F? j CG-HELD CG-F? or if false exit then
   j CG-W@ {: w:n :}
   w CG-MOVK CG-MOVW? w FRD rd = and ;

: CG-LANE ( n n -- n ) {: v:n w:n :}
   w CG-HW 16 * {: sh:n :}
   v $FFFF sh lshift invert and  w FI16 sh lshift or ;

: CG-MASK ( n n -- ) {: i:n w:n :}
   w CG-MOVZ CG-MOVW? w CG-MOVN CG-MOVW? or 0= if exit then
   w CG-MOVW-VALUE  w CG-MOVN CG-MOVW? if invert then CG-V !
   i 1+ CG-J !
   begin CG-J @ w FRD CG-MOVK-OF? while
      CG-V @ CG-J @ CG-W@ CG-LANE CG-V !
      1 CG-J +!
   repeat
   CG-J @ i - {: len:n :}
   len 2 < if exit then
   CG-V @ A64ASM:LIMM? 0= if exit then
   CG-P-MASK 1 len 4 * len 1- 4 * CG-HIT ;

\ ---- constant shift and scaled index -----------------------------------------------
: CG-FEEDS ( n n n -- ) {: p:n j:n bit:n :}
   j bit CG-FED if p 1 8 4 CG-HIT else p 1 4 0 CG-HIT then ;

: CG-SHIFT ( n n -- ) {: i:n w:n :}
   w CG-LSLV CG-RRR? w CG-LSRV CG-RRR? or 0= if exit then
   i w FRM CG-DEF {: j:n :}
   j 0 < if exit then
   j CG-CONST1 nip 0= if exit then
   CG-P-SHIFT j CG-FED-SHIFT CG-FEEDS ;

\ The one-instruction 8 this block feeds reg from, or -1.
: CG-EIGHT ( n n -- n ) {: i:n reg:n :}
   i reg CG-DEF {: j:n :}
   j 0 < if -1 exit then
   j CG-CONST1 if 8 = if j else -1 then else drop -1 then ;

: CG-INDEX ( n n -- ) {: i:n w:n :}
   w CG-RRRA-MASK CG-MADD CG-FORM? 0= w CG-RA 31 = or if exit then
   i w FRM CG-EIGHT {: jm:n :}
   jm 0 >= if jm else i w FRN CG-EIGHT then {: j:n :}
   j 0 < if exit then
   CG-P-INDEX j CG-FED-INDEX CG-FEEDS ;

\ ---- remainder ------------------------------------------------------------------------
\ Both scans stay in the block, stop at a call, and fail when an operand they
\ still need is overwritten first.
: CG-FIND-MUL ( n n n n -- n ) {: i:n q:n b:n a:n :}
   i 1+ begin dup CG-SEND @ < while
      dup CG-LEAD CG-F? if drop -1 exit then
      dup CG-W@ {: x:n :}
      x CG-CALL? if drop -1 exit then
      x CG-MUL CG-RRR? if x FRN x FRM q b CG-SAME2? if exit then then
      x q CG-WRITES? x b CG-WRITES? or x a CG-WRITES? or if drop -1 exit then
      1+
   repeat drop -1 ;

: CG-FIND-SUB ( n n n -- n ) {: j:n t:n a:n :}
   j 1+ begin dup CG-SEND @ < while
      dup CG-LEAD CG-F? if drop -1 exit then
      dup CG-W@ {: x:n :}
      x CG-CALL? if drop -1 exit then
      x CG-SUB CG-RRR? x FRN a = and x FRM t = and if exit then
      x a CG-WRITES? x t CG-WRITES? or if drop -1 exit then
      1+
   repeat drop -1 ;

: CG-REM ( n n -- ) {: i:n w:n :}
   w CG-SDIV CG-RRR? 0= if exit then
   w FRD {: q:n :}
   w FRN {: a:n :}
   w FRM {: b:n :}
   q a = q b = or if exit then
   i q b a CG-FIND-MUL {: j:n :}
   j 0 < if exit then
   j CG-W@ FRD {: t:n :}
   t a = if exit then
   j t a CG-FIND-SUB 0 < if exit then
   CG-P-REM 1 12 4 CG-HIT ;

\ ---- signed maximum -------------------------------------------------------------------
\ PUT-FLAG (src/compiler/native/emit.f) writes the compare immediately before
\ the csetm it feeds, so the flags a mask reads come from the instruction before.
: CG-MAX-AND ( n n n n -- bool ) {: j:n t:n m:n b:n :}
   j t CG-DEF {: k:n :}
   k 0 < if false exit then
   k CG-W@ {: ew:n :}
   ew CG-EOR CG-RRR? 0= if false exit then
   j m CG-DEF {: l:n :}
   l CG-BLK @ <= if false exit then
   l CG-W@ {: sw:n :}
   sw CG-CSET-MASK CG-CSETM CG-FORM? sw CG-CSET-MASK CG-CSET CG-FORM? or 0= if false exit then
   l 1- CG-W@ {: cw:n :}
   cw CG-CMP-MASK CG-CMP CG-FORM? 0= if false exit then
   ew FRN ew FRM cw FRN cw FRM CG-SAME2? 0= if false exit then
   b cw FRN = b cw FRM = or ;

\ u is the masked difference and b the kept operand of `eor r, u, b`.
: CG-MAX-FROM ( n n n -- bool ) {: i:n u:n b:n :}
   i u CG-DEF {: j:n :}
   j 0 < if false exit then
   j CG-W@ {: aw:n :}
   aw CG-AND CG-RRR? 0= if false exit then
   j aw FRN aw FRM b CG-MAX-AND if true exit then
   j aw FRM aw FRN b CG-MAX-AND ;

: CG-MAX ( n n -- ) {: i:n w:n :}
   w CG-EOR CG-RRR? 0= if exit then
   i w FRN w FRM CG-MAX-FROM  i w FRM w FRN CG-MAX-FROM or 0= if exit then
   CG-P-MAX 1 20 12 CG-HIT ;

\ ---- call-crossing spill --------------------------------------------------------------
\ A slot's store is remembered with the epoch it was made in and the calls the
\ epoch had made by then; a reload crosses when a tier-1 call and no other
\ call came between. Epochs are numbered, so nothing is cleared between them.
\ Each owner and each block a branch can land on starts a new epoch; a block
\ entered only by falling out of a conditional branch continues the one
\ before (CG-BLOCK), since every path into it runs through that block. That
\ holds while nothing enters it through a register: CG-MARK-LEADERS refuses a
\ br and makes every adr and code-literal target a landing.
4096 constant CG-SLOTS              \ an unsigned scaled offset names at most 4096 cells
DYNAMIC-BUFFER CG-SL-EPOCH n
DYNAMIC-BUFFER CG-SL-AT n
DYNAMIC-BUFFER CG-SL-CALLS n
DYNAMIC-BUFFER CG-SL-OTHER n
variable CG-EPOCH  variable CG-CALLS  variable CG-OTHER
variable CG-RSITES  variable CG-RBYTES

\ 0 none, 1 a slot store, 2 a slot reload, 3 a pair store, 4 a pair reload.
\ The link's own slot is the frame's, not a spill.
: CG-SLOT-FORM ( n -- n ) {: w:n :}
   w CG-LSU-MASK and {: m:n :}
   w FRD A64M:LINK-GPR = {: link:bool :}
   m CG-SP-STR = if link if 0 else 1 then exit then
   m CG-SP-LDR = if link if 0 else 2 then exit then
   m CG-SP-STRD = if 1 exit then
   m CG-SP-LDRD = if 2 exit then
   m CG-SP-STP = if 3 exit then
   m CG-SP-LDP = if 4 exit then
   0 ;

: CG-SLOT? ( n -- bool ) {: s:n :} s 0 >= s CG-SLOTS < and ;

: CG-SLOT-STORE ( n n -- ) {: i:n s:n :}
   s CG-SLOT? 0= if exit then
   CG-EPOCH @ s CG-SL-EPOCH !  i s CG-SL-AT !
   CG-CALLS @ s CG-SL-CALLS !  CG-OTHER @ s CG-SL-OTHER ! ;

: CG-SLOT-LOAD ( n -- ) {: s:n :}
   s CG-SLOT? 0= if exit then
   s CG-SL-EPOCH @ CG-EPOCH @ <> if exit then
   CG-CALLS @ s CG-SL-CALLS @ <=  CG-OTHER @ s CG-SL-OTHER @ <> or if exit then
   1 CG-RSITES +!
   s CG-SL-AT @ {: st:n :}
   st CG-SPILLED CG-F? 0= if st CG-SPILLED CG-F+  4 CG-RBYTES +! then ;

: CG-SPILL ( n n -- ) {: i:n w:n :}
   w CG-SLOT-FORM {: k:n :}
   k 0= if exit then
   k 1 = if i w FI12 CG-SLOT-STORE exit then
   k 3 = if i w CG-IMM7 CG-SLOT-STORE  i w CG-IMM7 1+ CG-SLOT-STORE exit then
   0 CG-RSITES !  0 CG-RBYTES !
   k 2 = if w FI12 CG-SLOT-LOAD else w CG-IMM7 CG-SLOT-LOAD  w CG-IMM7 1+ CG-SLOT-LOAD then
   CG-RSITES @ 0= if exit then
   CG-RBYTES @ 4 + {: bytes:n :}
   CG-P-SPILL CG-RSITES @ bytes bytes CG-HIT ;

\ ---- DATA carriers ---------------------------------------------------------------------
20 constant CG-ISLAND-BITS          \ 1 MiB, the reach of an ldr literal
48 constant CG-ISLAND-SHIFT         \ above the 48 bits a DATA carrier holds
DYNAMIC-BUFFER CG-SV n              \ this owner's carrier targets
DYNAMIC-BUFFER CG-IV n              \ every carrier target, tagged with its island
DYNAMIC-BUFFER CG-DD n              \ distinct targets per owner
variable CG-SVN  variable CG-IVN  variable CG-RDIST

: CG-CARRIER ( n -- ) {: i:n :}
   BLOB-OFF @ i 4 * + {: at:n :}
   at CHAIN-SIZE SNAP-RELOC:DATA-CHAIN-BYTES <> if
      s" codegen-census: a declared DATA site is not a DATA carrier" RC die then
   at CHAIN-VALUE {: v:n :}
   v CG-SVN @ CG-SV !  1 CG-SVN +!
   i 4 * CG-ISLAND-BITS rshift CG-ISLAND-SHIFT lshift v or CG-IVN @ CG-IV !  1 CG-IVN +!
   CG-P-DATA 1 SNAP-RELOC:DATA-CHAIN-BYTES 8 CG-HIT ;

: CG-OWNER-DISTINCT ( -- n )
   CG-SVN @ 0= if 0 exit then
   0 CG-SV CG-SVN @ [: < ;] SORT:SORT!
   1 CG-SVN @ 1 ?do i CG-SV @ i 1- CG-SV @ <> if 1+ then loop ;

\ ---- no-return fallback -----------------------------------------------------------------
variable CG-C  variable CG-ST

: CG-DEAD ( n -- ) {: i:n :}
   -1 CG-C !  0 CG-ST !
   i 1- begin dup CG-CALL0 @ >= while
      dup CG-DATA CG-F? if dup CG-C ! then
      dup CG-HELD CG-F? 0= if
         dup CG-CONST1 swap ENGINE-ERROR:CODE-CERT = and if -1 CG-ST ! then
      then
      1-
   repeat drop
   CG-C @ 0 < CG-ST @ 0= or if exit then
   i CG-C @ - 1+ 4 * {: bytes:n :}
   CG-P-DEAD 1 bytes bytes 24 - 0 max CG-HIT ;

\ ---- wide-store run ---------------------------------------------------------------------
variable CG-RUN  variable CG-RUN0  variable CG-RUNEND

: CG-RUN-CLOSE ( -- )
   CG-RUN @ 2 >= if
      CG-RUNEND @ CG-RUN0 @ - 1+ 4 * {: bytes:n :}
      CG-P-WIDE 1 bytes  bytes CG-RUN @ 2 + 4 * - 0 max  CG-HIT
   then
   0 CG-RUN ! ;

\ The address step the next cell of the run needs, somewhere in [from, to).
: CG-STEP8? ( n n -- bool ) {: from:n to:n :}
   CG-RUN @ CELL * {: step:n :}
   from begin dup to < while
      dup CG-W@ {: x:n :}
      x CG-ADDI-MASK CG-ADDI CG-FORM?  x FI12 step = and
      x FRD A64M:DSTACK-GPR <> and if drop true exit then
      1+
   repeat drop false ;

: CG-STORE-CALL ( n -- ) {: i:n :}
   CG-RUN @ 0 > if
      CG-CALL0 @ i CG-STEP8? if 1 CG-RUN +!  i CG-RUNEND !  exit then
   then
   CG-RUN-CLOSE
   1 CG-RUN !  CG-CALL0 @ CG-RUN0 !  i CG-RUNEND ! ;

\ ---- the walk -------------------------------------------------------------------------
: CG-CALL ( n n -- ) {: i:n w:n :}
   i w CG-CALLEE {: k:n :}
   k CG-K-TIER1 = if 1 CG-CALLS +! else 1 CG-OTHER +! then
   k CG-K-REG <> if 1 CG-NBL +! then
   k CG-K-THROW <> k CG-K-DIE <> and if 1 CG-NBAD +! then
   k CG-K-DIE = if i CG-DEAD then
   k CG-K-STORE = if i CG-STORE-CALL else CG-RUN-CLOSE then
   i 1+ CG-CALL0 ! ;

\ Whether the block at i is entered only from the conditional branch before it
\ in the same owner: not the owner's first instruction, not a branch target,
\ and after a branch that may fall through.
: CG-FALLS-IN? ( n -- bool ) {: i:n :}
   i CG-SEG @ CG-SS @ 4 / <= if false exit then
   i CG-LAND CG-F?  i 1- CG-HELD CG-F? or if false exit then
   i 1- CG-W@ CG-FORK? ;

\ The spill epoch outlives a block entered only by falling in; every other
\ dataflow state is the block's own.
: CG-BLOCK ( n -- ) {: i:n :}
   CG-RUN-CLOSE
   i CG-BLK !  i CG-CALL0 !
   i CG-FALLS-IN? if exit then
   1 CG-EPOCH +!  0 CG-CALLS !  0 CG-OTHER ! ;

: CG-STEP ( n -- ) {: i:n :}
   i CG-LEAD CG-F? if i CG-BLOCK then
   i CG-DATA CG-F? if i CG-CARRIER then
   i CG-HELD CG-F? if exit then
   i CG-W@ {: w:n :}
   w CG-CALL? if i w CG-CALL exit then
   w CG-FRAME-STEP
   i w CG-SPILL
   i w CG-DIV  i w CG-MASK  i w CG-SHIFT  i w CG-INDEX  i w CG-REM  i w CG-MAX ;

: CG-SCAN-OWNER ( n -- ) {: s:n :}
   s CG-SEG !
   s CG-SE @ 4 / CG-SEND !
   s CG-SK @ {: k:n :}
   k 0 < if -1 else k NODE-START then CG-ENTRY !
   0 CG-LINKS !  0 CG-LSAVE !  0 CG-FUSED !  0 CG-SPT !  0 CG-NBL !  0 CG-NBAD !
   0 CG-SVN !  0 CG-RUN !
   s CG-SS @ 4 / begin dup CG-SEND @ < while dup CG-STEP 1+ repeat drop
   CG-RUN-CLOSE
   CG-FRAME-END
   CG-OWNER-DISTINCT dup s CG-DD !  CG-RDIST +! ;

: CG-ZERO ( n -- ) {: k:n :} 0 k CG-SITES !  0 k CG-BYTES !  0 k CG-SAVE ! ;

: CG-SCAN ( -- )
   CG-P-N dup CG-SITES-RESERVE dup CG-BYTES-RESERVE CG-SAVE-RESERVE
   CG-P-N 0 ?do i CG-ZERO loop
   CG-NSEG @ CG-P-N * {: cells:n :}
   cells CG-OWN-RESERVE  cells 0 ?do 0 i CG-OWN ! loop
   CG-NSEG @ CG-DD-RESERVE
   CG-SLOTS dup CG-SL-EPOCH-RESERVE dup CG-SL-AT-RESERVE
   dup CG-SL-CALLS-RESERVE CG-SL-OTHER-RESERVE
   CG-SLOTS 0 ?do -1 i CG-SL-EPOCH ! loop
   DSITE-N @ 1+ dup CG-SV-RESERVE CG-IV-RESERVE
   0 CG-EPOCH !  0 CG-RDIST !  0 CG-IVN !
   CG-NSEG @ 0 ?do i CG-SCAN-OWNER loop ;

\ ---- islands ---------------------------------------------------------------------------
DYNAMIC-BUFFER CG-ISITES n
DYNAMIC-BUFFER CG-IDIST n
variable CG-NISL  variable CG-IDSUM

: CG-ISLANDS ( -- )
   BLOB-LEN @ 1- CG-ISLAND-BITS rshift 1+ CG-NISL !
   CG-NISL @ dup CG-ISITES-RESERVE CG-IDIST-RESERVE
   CG-NISL @ 0 ?do 0 i CG-ISITES ! 0 i CG-IDIST ! loop
   CG-IVN @ 0 > if 0 CG-IV CG-IVN @ [: < ;] SORT:SORT! then
   CG-IVN @ 0 ?do
      i CG-IV @ {: v:n :}
      v CG-ISLAND-SHIFT rshift {: isl:n :}
      1 isl CG-ISITES +!
      i 0= if true else i 1- CG-IV @ v <> then if 1 isl CG-IDIST +! then
   loop
   0 CG-NISL @ 0 ?do i CG-IDIST @ + loop CG-IDSUM !
   CG-IDSUM @ CELL * negate CG-P-DATA CG-SAVE +! ;

\ ---- the report -------------------------------------------------------------------------
DYNAMIC-BUFFER CG-OUT u8
variable CG-OUT-N  variable CG-OUT-CAP
DYNAMIC-BUFFER CG-RANK n
create CG-SHA-CTX SHA256-CTX-BYTES allot
create CG-DG 32 allot
create CG-HEX 64 allot

: CG-C, ( n -- ) {: c:n :}
   CG-OUT-N @ CG-OUT-CAP @ >= if
      CG-OUT-CAP @ 2 * $10000 max CG-OUT-CAP !  CG-OUT-CAP @ CG-OUT-RESERVE then
   c CG-OUT-N @ CG-OUT c!  1 CG-OUT-N +! ;

: CG-S, ( ptr u8 n -- ) {: a:ptr u:n :} u 0 ?do a i + c@ CG-C, loop ;
: CG-BL, ( -- ) 32 CG-C, ;
: CG-LF, ( -- ) 10 CG-C, ;
: CG-N, ( n -- ) CG-BL, SB-RESET FMT:SB-INT SB$ CG-S, ;
: CG-IMAGE, ( n n -- ) {: at:n len:n :} len 0 ?do at i + U8@ CG-C, loop ;

: CG-NAME, ( n -- ) {: k:n :}
   k REC-NAME {: at:n len:n :}
   at 0 < if s" ?" CG-S, exit then
   at len CG-IMAGE, ;

: CG-AT, ( n -- ) {: s:n :} s" @" CG-S, s CG-SS @ SB-RESET FMT:SB-INT SB$ CG-S, ;

\ A span's name comes from the sidecar row with its exact start and length, so
\ a stale sidecar names nothing.
: CG-OWNER, ( n -- ) {: s:n :}
   CG-BL,
   s CG-SK @ {: k:n :}
   k 0 < if s" gap" CG-S, s CG-AT, exit then
   k REC-N @ >= if
      s CG-SS @ s CG-SE @ s CG-SS @ - IMAGE-NAMES:SPAN-NAME$ CG-S,  s CG-AT, exit
   then
   k REC-ROLE {: role:n :}
   role ROLE-PUBLIC = role ROLE-PRIVATE = or if
      k REC-WID WPKG @ CG-NAME, s" :" CG-S,
   then
   k CG-NAME, ;

: CG-E, ( -- )
   CG-SHA-CTX IMG@ ILEN @ CG-DG SHA256-IN
   CG-DG CG-HEX SHA256>HEX
   s" E " CG-S, CG-HEX 64 CG-S, CG-BL, 1 SCRIPT-ARGV$ CG-S, ILEN @ CG-N, CG-LF, ;

: CG-C-LINE, ( -- )
   s" C" CG-S, BLOB-LEN @ CG-N,
   0 CG-NSEG @ 0 ?do i CG-SK @ dup 0 >= swap REC-N @ < and if 1+ then loop CG-N,
   0 CG-NSEG @ 0 ?do i CG-SK @ REC-N @ >= if 1+ then loop CG-N,
   CG-UNOWNED @ CG-N,  CG-OVER @ CG-N,  CG-LF, ;

: CG-P, ( n -- ) {: p:n :}
   s" P " CG-S, p CG-PATTERN$ CG-S,
   p CG-SITES @ CG-N,  p CG-BYTES @ CG-N,  p CG-SAVE @ CG-N,
   p CG-P-DATA = if CG-RDIST @ CG-N,  CG-IDSUM @ CG-N, then
   CG-LF, ;

: CG-I, ( -- )
   CG-NISL @ 0 ?do
      s" I data-carrier" CG-S, i CG-N, i CG-ISITES @ CG-N, i CG-IDIST @ CG-N, CG-LF,
   loop ;

\ Owners with sites of pattern p, most first; ties keep blob order.
: CG-RANK-BUILD ( n -- n ) {: p:n :}
   0 CG-NSEG @ 0 ?do
      i CG-P-N * p + CG-OWN @ {: c:n :}
      c 0 > if c 32 lshift CG-NSEG @ i - or over CG-RANK !  1+ then
   loop
   dup 0 > if 0 CG-RANK over [: > ;] SORT:SORT! then ;

: CG-W, ( n -- ) {: p:n :}
   p CG-RANK-BUILD CG-TOP min 0 ?do
      i CG-RANK @ {: packed:n :}
      CG-NSEG @ packed $FFFFFFFF and - {: s:n :}
      s" W " CG-S, p CG-PATTERN$ CG-S, s CG-OWNER, packed 32 rshift CG-N,
      p CG-P-DATA = if s CG-DD @ CG-N, then
      CG-LF,
   loop ;

: CG-REPORT ( -- )
   0 CG-OUT-N !  0 CG-OUT-CAP !
   CG-E,  CG-C-LINE,
   CG-P-N 0 ?do i CG-P, loop
   CG-I,
   CG-NSEG @ CG-RANK-RESERVE
   CG-P-N 0 ?do i CG-W, loop ;

\ ---- the command -------------------------------------------------------------------------
: CG-REFUSE-CLASS ( -- )
   SB-RESET s" codegen-census: " SB-APPEND 0 SCRIPT-ARGV$ SB-APPEND
   s"  is a " SB-APPEND CLASS$ SB-APPEND
   s"  image, not a tools/native-build.f engine" SB-APPEND
   SB$ RC die ;

: CG-COMMAND ( -- )
   SCRIPT-ARGC 3 <> if
      s" usage: <engine> --load tools/codegen-census.f -- <image> <source-commit> <report>"
      CG-USAGE-RC die
   then
   0 SCRIPT-ARGV$ MEASURE
   CLASS$ s" engine" STR= 0= if CG-REFUSE-CLASS then
   BUILD-WID-MAP
   CG-LOAD
   CG-ENTRIES
   CG-MARK-SITES
   CG-MARK-LEADERS
   CG-SEGMENTS
   CG-SCAN
   CG-ISLANDS
   CG-REPORT
   2 SCRIPT-ARGV$ 0 CG-OUT CG-OUT-N @ WRITE-ALL ;

\ Loading with no argument defines the tool without running it.
: CG-MAIN? ( -- )
   SCRIPT-ARGC 0 > if CG-COMMAND then ;

CG-MAIN?

;package
