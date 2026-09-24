\ reloc-cases.f - Execute relocation vectors against emitted instruction sequences.
\ Selected examples do not establish complete relocation coverage.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/habu/address-carrier.f
require test/compiler/ir-id-source.f
require test/compiler/reloc-schema.f
require test/compiler/reloc-vm.f

package RELOC-CASES
using RELOC-PROOF
private

\ Where the machine sees the two bands. Any two addresses far enough apart to be
\ distinct spans; the arithmetic under test never depends on them, which is
\ itself part of what the rows show.
$10000000 constant VM-DATA
$20000000 constant VM-REGION

\ Windows inside the machine's own store.
0 constant MAP-AT         64 constant MAP-BYTES
64 constant IMG-AT        256 constant IMG-BYTES
512 constant TAB-AT       128 constant TAB-BYTES
768 constant CELLS-AT     128 constant CELLS-BYTES
1024 constant AMAP-AT     64 constant AMAP-BYTES

$3FFFFFF constant IMM-MASK
26 constant IMM-BITS
$2000000 constant IMM-HALF

variable BLOP
variable ROLE-SEEN
variable IMG-OK
variable SCAN-AT
variable MOVED-N
create SCAF CHAIN-WORDS cells allot   \ the four scaffold words, read out of habu1.f

\ ---- 1. the pinned band constants --------------------------------------------

: PIN-CHECK ( n -- ) {: k:n :}
   k PIN-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" the shipped source declares that band constant exactly once" T-LABEL
   k PIN-NAME$ COMPILER-ID-SRC:CONSTS 1 T=
   s" the shipped band constant is the number the shared table froze" T-LABEL
   k PIN-NAME$ COMPILER-ID-SRC:CONST@ k PIN-VALUE T= ;

: PHASE-PINS ( -- )
   PIN-COUNT 0 ?do i PIN-CHECK loop ;

\ ---- 4. the shape of the vector table ----------------------------------------

: ROLE-SEE ( n -- ) {: role:n :}
   role 0 < role ROLE-COUNT >= or if E-CRL-ROW throw then
   ROLE-SEEN @ 1 role lshift or ROLE-SEEN ! ;

: IN-REACH? ( n -- bool ) {: d:n :}
   d IMM-HALF negate >= d IMM-HALF < and ;

: SITE-SHAPE ( n n -- ) {: row:n j:n :}
   row ROW-BASE@ j + {: s:n :}
   s" every word of the region is listed once, in order" T-LABEL
   s SITE-IDX@ j T=
   s SITE-KIND@ KIND-BL = if
      s" every call displacement in the row is inside BL's reach" T-LABEL
      s SITE-V0@ IN-REACH? s SITE-V1@ IN-REACH? and s SITE-V2@ IN-REACH? and TTRUE
   then ;

: ROW-SHAPE ( n -- ) {: row:n :}
   row ROW-ROLE@ ROLE-SEE
   s" the row lists exactly as many words as its region holds" T-LABEL
   row ROW-LEN@ row ROW-WORDS@ T=
   s" the region fits the image window the machine gives it" T-LABEL
   row ROW-WORDS@ 4 * IMG-BYTES <= TTRUE
   s" both region bases sit a whole instruction from the canonical offset" T-LABEL
   row ROW-WOFF@ REGION-OFF - 3 and row ROW-LOFF@ REGION-OFF - 3 and or 0 T=
   row ROW-LEN@ 0 ?do row i SITE-SHAPE loop ;

: XROW-SHAPE ( n -- ) {: row:n :}
   row XROW-ROLE@ ROLE-SEE
   s" the address-cell row fits the cell window the machine gives it" T-LABEL
   row XROW-LEN@ 8 * CELLS-BYTES <= TTRUE ;

\ How many slots of a chain row the writer's pass has to change. A row where
\ that is zero would pass however little the pass did, so it is refused here.
: AMOVED ( n -- n ) {: row:n :}
   0 MOVED-N !
   row AROW-LEN@ 0 ?do
      row AROW-BASE@ i + {: s:n :}
      s ASITE-V0@ s ASITE-V1@ <> if MOVED-N @ 1+ MOVED-N ! then
   loop
   MOVED-N @ ;

: BANDS-APART? ( n n n -- bool ) {: a:n b:n blen:n :}
   a blen + b <= b blen + a <= or ;

: ASITE-SHAPE ( n n -- ) {: row:n j:n :}
   s" every slot of the chain region is listed once, in order" T-LABEL
   row AROW-BASE@ j + ASITE-IDX@ j CHAIN-WORDS * T= ;

: AROW-SHAPE ( n -- ) {: row:n :}
   row AROW-ROLE@ ROLE-SEE
   s" the chain row lists exactly as many words as its region holds" T-LABEL
   row AROW-LEN@ CHAIN-WORDS * row AROW-WORDS@ T=
   s" the chain region fits the image window the machine gives it" T-LABEL
   row AROW-WORDS@ 4 * IMG-BYTES <= TTRUE
   s" the writer's band and the canonical band are disjoint" T-LABEL
   row AROW-WB@ row AROW-CB@ row AROW-BLEN@ BANDS-APART? TTRUE
   s" the loader's band and the canonical band are disjoint" T-LABEL
   row AROW-LB@ row AROW-CB@ row AROW-BLEN@ BANDS-APART? TTRUE
   s" the chain row asks the writer's pass to move at least one slot" T-LABEL
   row AMOVED 0 > TTRUE
   row AROW-LEN@ 0 ?do row i ASITE-SHAPE loop ;

: PHASE-SHAPE ( -- )
   0 ROLE-SEEN !
   ROWS 0 ?do i ROW-SHAPE loop
   XROWS 0 ?do i XROW-SHAPE loop
   AROWS 0 ?do i AROW-SHAPE loop
   s" every role the schema names is covered by a row" T-LABEL
   ROLE-SEEN @ 1 ROLE-COUNT lshift 1- T= ;

\ ---- the machine's symbol table ----------------------------------------------
\ Every bare name the two shipped passes use, bound to the value the shipped
\ source gives it. The condition codes and the register alias come out of the
\ sources that declare them, so a renumbering there changes what the machine
\ runs rather than being absorbed by a copy kept here.

: COND-SYM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u a u COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+ ;

: LOAD-COND-SYMS ( -- )
   COND-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" C-EQ" COND-SYM
   s" C-NE" COND-SYM
   s" C-GE" COND-SYM
   s" C-LT" COND-SYM
   s" C-GT" COND-SYM
   s" C-LE" COND-SYM
   s" C-CC" COND-SYM
   s" C-CS" COND-SYM
   s" C-HI" COND-SYM ;

: LOAD-EMIT-SYMS ( -- )
   EMIT-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" BL-OP-HI" COMPILER-ID-SRC:CONST@ BLOP !
   s" BL-OP-HI" BLOP @ RELOC-VM:SYM+
   s" CALLMSG-LEN" s" CALLMSG-LEN" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" XTMSG-LEN" s" XTMSG-LEN" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" ADDRMSG-LEN" s" ADDRMSG-LEN" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" XTBANDMSG-LEN" s" XTBANDMSG-LEN" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+ ;

\ The carrier's SHAPE is declared in src/habu/address-carrier.f, because a capture
\ running inside bin/hb has to recognise the same chain this emitter's relocation
\ pass reads back. The machine binds those five from the file that declares them,
\ so the scan follows the definition rather than a copy kept here.
: LOAD-DECL-SYMS ( -- )
   DECL-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" ADDR-OPC-MASK" s" ADDR-OPC-MASK" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" ADDR-IMM-MASK" s" ADDR-IMM-MASK" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" ADDR-CHAIN-BYTES" s" ADDR-CHAIN-BYTES" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" DATA-CHAIN-BYTES" s" DATA-CHAIN-BYTES" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" DATA-MOVZ2" SNAP-RELOC:DATA-MOVZ2 RELOC-VM:SYM+
   s" DATA-MOVK0" SNAP-RELOC:DATA-MOVK0 RELOC-VM:SYM+
   s" ADDR-RD-MASK" s" ADDR-RD-MASK" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+
   s" ADDR-RD-BITS" s" ADDR-RD-BITS" COMPILER-ID-SRC:CONST@ RELOC-VM:SYM+ ;

\ The chain's four scaffold words are declared in src/habu/habu1.f. They are
\ bound as machine symbols so the shipped check runs against the shipped words,
\ and kept here as well so the fixture builds its chains out of the same four.
: SCAFFOLD-SYM ( n ptr u8 n -- ) {: j:n a:ptr u:n :}
   a u COMPILER-ID-SRC:CONST@ {: w:n :}
   a u w RELOC-VM:SYM+
   w SCAF j cells + ! ;

: LOAD-SCAFFOLD-SYMS ( -- )
   SCAFFOLD-FILE$ COMPILER-ID-SRC:SCAN-FILE
   0 s" W-MOVZ0" SCAFFOLD-SYM
   1 s" W-MOVK1" SCAFFOLD-SYM
   2 s" W-MOVK2" SCAFFOLD-SYM
   3 s" W-MOVK3" SCAFFOLD-SYM ;

\ The band offsets and the register alias come from the loaded layout, because
\ they are derived rather than literal. CALLMAP-RC is a literal and is pinned as
\ one above; it is bound here as well so the refusal the machine reports is the
\ shipped status.
: LOAD-LAYOUT-SYMS ( -- )
   s" DATA" DATA RELOC-VM:SYM+
   s" CALLMAP-OFF" SNAP-RELOC:CALLMAP-OFF RELOC-VM:SYM+
   s" CALLMAP-RC" SNAP-RELOC:CALLMAP-RC RELOC-VM:SYM+
   s" XTCELL-N-CELL" SNAP-RELOC:XTCELL-N-CELL RELOC-VM:SYM+
   s" ADDRESS-CELLS:BASE-FIELD" ADDRESS-CELLS:BASE-FIELD RELOC-VM:SYM+
   s" XTCELL-CAP" SNAP-RELOC:XTCELL-CAP RELOC-VM:SYM+
   s" XTCELL-RC" SNAP-RELOC:XTCELL-RC RELOC-VM:SYM+
   s" XTCELL-OFF-MAX" SNAP-RELOC:XTCELL-OFF-MAX RELOC-VM:SYM+
   s" XTCELL-DATA-TAG" SNAP-RELOC:XTCELL-DATA-TAG RELOC-VM:SYM+
   s" XTCELL-OFF-MASK" SNAP-RELOC:XTCELL-OFF-MASK RELOC-VM:SYM+
   s" XTBAND-RC" SNAP-RELOC:XTBAND-RC RELOC-VM:SYM+
   s" XTKIND-RC" SNAP-RELOC:XTKIND-RC RELOC-VM:SYM+
   s" ADDRMAP-OFF" SNAP-RELOC:ADDRMAP-OFF RELOC-VM:SYM+
   s" ADDRMAP-RC" SNAP-RELOC:ADDRMAP-RC RELOC-VM:SYM+ ;

: LOAD-GLOBAL-LABELS ( -- )
   s" LCALLS" RELOC-VM:GLABEL+
   s" LXT" RELOC-VM:GLABEL+
   s" LMARK" RELOC-VM:GLABEL+
   s" LPTRMARK" RELOC-VM:GLABEL+
   s" LCALLMSG" RELOC-VM:GLABEL+
   s" LXTMSG" RELOC-VM:GLABEL+
   s" LXTBANDMSG" RELOC-VM:GLABEL+
   s" LXTKINDMSG" RELOC-VM:GLABEL+
   s" LADDRS" RELOC-VM:GLABEL+
   s" LADDRMSG" RELOC-VM:GLABEL+ ;

\ Leaves the emitter source scanned, so a pass can be decoded straight after.
: TEACH-MACHINE ( -- )
   RELOC-VM:RESET
   LOAD-COND-SYMS
   LOAD-LAYOUT-SYMS
   LOAD-GLOBAL-LABELS
   LOAD-SCAFFOLD-SYMS
   LOAD-DECL-SYMS
   LOAD-EMIT-SYMS ;

: CLEAR-REGS ( -- )
   32 0 ?do 0 i RELOC-VM:R! loop ;

\ ---- 4. driving the call rows through the shipped pass -----------------------

: BL-WORD ( n -- n ) {: d:n :}
   BLOP @ IMM-BITS lshift d IMM-MASK and or ;

: SITE-WORD ( n n -- n ) {: s:n v:n :}
   s SITE-KIND@ KIND-BL = if v BL-WORD exit then
   v ;

: MAP-BIT ( n -- ) {: idx:n :}
   VM-DATA SNAP-RELOC:CALLMAP-OFF + idx 3 rshift + {: at:n :}
   at 1 RELOC-VM:PEEK 1 idx 7 and lshift or at 1 RELOC-VM:POKE ;

: PLACE-SITE ( n -- ) {: s:n :}
   s SITE-V0@ {: v:n :}
   s v SITE-WORD VM-REGION s SITE-IDX@ 4 * + 4 RELOC-VM:POKE
   s SITE-REC@ 0<> if s SITE-IDX@ MAP-BIT then ;

: CALL-SEGMENTS ( n -- ) {: row:n :}
   RELOC-VM:SEG-RESET
   VM-DATA SNAP-RELOC:CALLMAP-OFF + MAP-AT MAP-BYTES RELOC-VM:SEG+
   VM-REGION IMG-AT row ROW-WORDS@ 4 * RELOC-VM:SEG+ ;

: BUILD-IMAGE ( n -- ) {: row:n :}
   row CALL-SEGMENTS
   row ROW-LEN@ 0 ?do row ROW-BASE@ i + PLACE-SITE loop ;

: RUN-CALL-PASS ( n n -- n ) {: row:n delta:n :}
   CLEAR-REGS
   VM-REGION 8 RELOC-VM:R!
   delta 10 RELOC-VM:R!
   row ROW-WORDS@ 4 * 11 RELOC-VM:R!
   VM-DATA DATA RELOC-VM:R!
   RELOC-VM:RUN
   RELOC-VM:HALT-CODE ;

: WORD-AT ( n -- n ) {: idx:n :}
   VM-REGION idx 4 * + 4 RELOC-VM:PEEK ;

: SITE-EXPECT ( n n -- n ) {: s:n phase:n :}
   phase 0 = if s s SITE-V1@ SITE-WORD exit then
   s s SITE-V2@ SITE-WORD ;

: SITE-MATCH ( n n -- ) {: s:n phase:n :}
   s SITE-IDX@ WORD-AT s phase SITE-EXPECT = if exit then
   false IMG-OK ! ;

: IMAGE-MATCH ( n n -- bool ) {: row:n phase:n :}
   true IMG-OK !
   row ROW-LEN@ 0 ?do row ROW-BASE@ i + phase SITE-MATCH loop
   IMG-OK @ ;

: WRITER-DELTA ( n -- n ) {: row:n :}
   row ROW-WOFF@ REGION-OFF - ;

: LOADER-DELTA ( n -- n ) {: row:n :}
   REGION-OFF row ROW-LOFF@ - ;

: CANON-PHASE ( n -- ) {: row:n :}
   row BUILD-IMAGE
   s" the writer's pass over the shipped instruction sequence ends as the row records" T-LABEL
   row row WRITER-DELTA RUN-CALL-PASS row ROW-RC@ T=
   s" the canonical image is the one the shared vector row records" T-LABEL
   row 0 IMAGE-MATCH TTRUE ;

: REBASE-PHASE ( n -- ) {: row:n :}
   s" the loader's pass over the shipped instruction sequence returns cleanly" T-LABEL
   row row LOADER-DELTA RUN-CALL-PASS 0 T=
   s" the restored image is the one the shared vector row records" T-LABEL
   row 1 IMAGE-MATCH TTRUE ;

: CALL-ROW ( n -- ) {: row:n :}
   row CANON-PHASE
   row ROW-RC@ 0<> if exit then
   row REBASE-PHASE ;

\ Two writing runs at different region bases, the same two callees, and
\ therefore the same canonical words: the portability claim, compared directly
\ rather than left to a reader of the table.
: CANON-IMAGE-EQUAL? ( n n -- bool ) {: ra:n rb:n :}
   ra ROW-LEN@ rb ROW-LEN@ <> if false exit then
   true IMG-OK !
   ra ROW-LEN@ 0 ?do
      ra ROW-BASE@ i + 0 SITE-EXPECT rb ROW-BASE@ i + 0 SITE-EXPECT <> if
         false IMG-OK !
      then
   loop
   IMG-OK @ ;

: ROW-OF-ROLE ( n -- n ) {: role:n :}
   ROWS 0 ?do i ROW-ROLE@ role = if i unloop exit then loop
   E-CRL-ROW throw ;

: PHASE-BASE-FREE ( -- )
   s" a second writing base canonicalizes to the very same image" T-LABEL
   ROLE-REBASE-UP ROW-OF-ROLE ROLE-BASE-FREE ROW-OF-ROLE CANON-IMAGE-EQUAL? TTRUE ;

: PHASE-CALLS ( -- )
   s" the shipped call pass decodes into instructions the machine can run" T-LABEL
   s" EMIT-CALLS" RELOC-VM:DECODE
   RELOC-VM:INSTRUCTIONS 0 > TTRUE
   ROWS 0 ?do i CALL-ROW loop ;

\ ---- 5. driving the address-cell rows ----------------------------------------
\ XT-CANON constructs the loader's input using the model's canonicalization.
\ This does not execute the writer. test/snapshot-writer.f and test/app-image.f
\ cover actual writer/restore use; the loader below is shipped EMIT-XT.

: XT-CANON ( n n -- n ) {: c:n db:n :}
   c 0= if 0 exit then
   c db - RBASE-VA + ;

: XT-SEGMENTS ( n -- ) {: row:n :}
   RELOC-VM:SEG-RESET
   VM-DATA SNAP-RELOC:XTCELL-N-CELL +
      TAB-AT ADDRESS-CELLS:BOOT-OFF SNAP-RELOC:XTCELL-N-CELL - row XROW-LEN@ 8 * +
      RELOC-VM:SEG+
   VM-DATA CELLS-AT row XROW-LEN@ 8 * RELOC-VM:SEG+ ;

: XT-DECLARE ( n n -- ) {: row:n j:n :}
   j 8 * {: off:n :}
   off VM-DATA ADDRESS-CELLS:BOOT-OFF + j 8 * + 8 RELOC-VM:POKE
   row XROW-BASE@ j + CELL-V0@ row XROW-DBW@ XT-CANON VM-DATA off + 8 RELOC-VM:POKE ;

: XT-BUILD ( n -- ) {: row:n :}
   row XT-SEGMENTS
   ADDRESS-CELLS:BOOT-OFF VM-DATA SNAP-RELOC:XTCELL-N-CELL +
      ADDRESS-CELLS:BASE-FIELD + 8 RELOC-VM:POKE
   row XROW-LEN@ VM-DATA SNAP-RELOC:XTCELL-N-CELL + 8 RELOC-VM:POKE
   row XROW-LEN@ 0 ?do row i XT-DECLARE loop ;

: XT-CANON-MATCH ( n n -- ) {: row:n j:n :}
   row XROW-BASE@ j + {: c:n :}
   s" the writer folds the declared cell onto the canonical sentinel" T-LABEL
   c CELL-V0@ row XROW-DBW@ XT-CANON c CELL-V1@ T= ;

: XT-CELL-MATCH ( n n -- ) {: row:n j:n :}
   s" the restored cell is the one the shared vector row records" T-LABEL
   VM-DATA j 8 * + 8 RELOC-VM:PEEK row XROW-BASE@ j + CELL-V2@ T= ;

: XT-ROW ( n -- ) {: row:n :}
   row XROW-LEN@ 0 ?do row i XT-CANON-MATCH loop
   row XT-BUILD
   CLEAR-REGS
   row XROW-DBL@ RBASE-VA - 10 RELOC-VM:R!
   VM-DATA DATA RELOC-VM:R!
   RELOC-VM:RUN
   s" the loader's address-cell pass returns cleanly" T-LABEL
   RELOC-VM:HALT-CODE 0 T=
   row XROW-LEN@ 0 ?do row i XT-CELL-MATCH loop ;

: PHASE-XT ( -- )
   s" the shipped address-cell pass decodes into instructions the machine can run" T-LABEL
   s" EMIT-XT" RELOC-VM:DECODE
   RELOC-VM:INSTRUCTIONS 0 > TTRUE
   XROWS 0 ?do i XT-ROW loop ;

\ ---- 6. driving the address-literal chain rows -------------------------------
\ The shipped `SNAP-RELOC:EMIT-ADDRS` is decoded and run the same way the call
\ pass is, once per leg: the writer's leg moves the live band onto the canonical
\ sentinel and the loader's moves the sentinel onto the band this run got. Only
\ the fixture supplies the address; the four words of every slot are built from it
\ and the scaffold words read out of src/habu/habu1.f.

\ The scaffold for lane j, read out of src/habu/habu1.f, with its destination
\ register replaced by the one this lane is meant to name. The four shipped
\ scaffolds all name x9, so a slot that names x9 gets its words back unchanged.
: LANE-SCAFFOLD ( n n -- n ) {: j:n rd:n :}
   SCAF j cells + @ {: w:n :}
   w w RD-MASK and - rd + ;

: CHAIN-WORD ( n n n n -- n ) {: v:n j:n bad:n rd:n :}
   j bad = if CHAIN-BAD-WORD exit then
   v j 16 * rshift IMM16-MASK and IMM-SCALE * j rd LANE-SCAFFOLD + ;

: SLOT-WORD ( n n n -- n ) {: s:n j:n v:n :}
   v j s ASITE-BAD@ s j ASITE-LANE-RD CHAIN-WORD ;

: AMAP-BIT ( n -- ) {: idx:n :}
   VM-DATA SNAP-RELOC:ADDRMAP-OFF + idx 3 rshift + {: at:n :}
   at 1 RELOC-VM:PEEK 1 idx 7 and lshift or at 1 RELOC-VM:POKE ;

: PLACE-ASITE ( n -- ) {: s:n :}
   CHAIN-WORDS 0 ?do
      s i s ASITE-V0@ SLOT-WORD
      VM-REGION s ASITE-IDX@ i + 4 * + 4 RELOC-VM:POKE
   loop
   s ASITE-REC@ 0<> if s ASITE-IDX@ AMAP-BIT then ;

: ADDR-SEGMENTS ( n -- ) {: row:n :}
   RELOC-VM:SEG-RESET
   VM-DATA SNAP-RELOC:ADDRMAP-OFF + AMAP-AT AMAP-BYTES RELOC-VM:SEG+
   VM-REGION IMG-AT row AROW-WORDS@ 4 * RELOC-VM:SEG+ ;

: BUILD-CHAIN-IMAGE ( n -- ) {: row:n :}
   row ADDR-SEGMENTS
   row AROW-LEN@ 0 ?do row AROW-BASE@ i + PLACE-ASITE loop ;

\ x8/x11 are the image the pass scans; x21/x22/x25 are the band it is moving.
: RUN-ADDR-PASS ( n n n n -- n ) {: row:n base:n blen:n tgt:n :}
   CLEAR-REGS
   VM-REGION 8 RELOC-VM:R!
   row AROW-WORDS@ 4 * 11 RELOC-VM:R!
   base 21 RELOC-VM:R!
   blen 22 RELOC-VM:R!
   tgt 25 RELOC-VM:R!
   VM-DATA DATA RELOC-VM:R!
   RELOC-VM:RUN
   RELOC-VM:HALT-CODE ;

: ASITE-EXPECT ( n n n -- n ) {: s:n phase:n j:n :}
   phase 0 = if s j s ASITE-V1@ SLOT-WORD exit then
   s j s ASITE-V2@ SLOT-WORD ;

: ASITE-MATCH ( n n -- ) {: s:n phase:n :}
   CHAIN-WORDS 0 ?do
      VM-REGION s ASITE-IDX@ i + 4 * + 4 RELOC-VM:PEEK
      s phase i ASITE-EXPECT <> if false IMG-OK ! then
   loop ;

: CHAIN-IMAGE-MATCH ( n n -- bool ) {: row:n phase:n :}
   true IMG-OK !
   row AROW-LEN@ 0 ?do row AROW-BASE@ i + phase ASITE-MATCH loop
   IMG-OK @ ;

: ACANON-PHASE ( n -- ) {: row:n :}
   row BUILD-CHAIN-IMAGE
   s" the writer's address pass over the shipped instruction sequence ends as the row records" T-LABEL
   row row AROW-WB@ row AROW-BLEN@ row AROW-CB@ RUN-ADDR-PASS row AROW-RC@ T=
   s" the canonical chain image is the one the shared vector row records" T-LABEL
   row 0 CHAIN-IMAGE-MATCH TTRUE ;

: AREBASE-PHASE ( n -- ) {: row:n :}
   s" the loader's address pass over the shipped instruction sequence returns cleanly" T-LABEL
   row row AROW-CB@ row AROW-BLEN@ row AROW-LB@ RUN-ADDR-PASS 0 T=
   s" the restored chain image is the one the shared vector row records" T-LABEL
   row 1 CHAIN-IMAGE-MATCH TTRUE ;

: CHAIN-ROW ( n -- ) {: row:n :}
   row ACANON-PHASE
   row AROW-RC@ 0<> if exit then
   row AREBASE-PHASE ;

: PHASE-ADDRS ( -- )
   s" the shipped address pass decodes into instructions the machine can run" T-LABEL
   s" EMIT-ADDRS" RELOC-VM:DECODE
   RELOC-VM:INSTRUCTIONS 0 > TTRUE
   AROWS 0 ?do i CHAIN-ROW loop ;

\ The DATA carrier is validated by the shipped pass and left unchanged even
\ when its immediate fields numerically lie inside the moving CODE band.
: DATA-CHAIN-CASE ( n n -- ) {: rd:n bad:n :}
   RELOC-VM:SEG-RESET
   VM-DATA SNAP-RELOC:ADDRMAP-OFF + AMAP-AT AMAP-BYTES RELOC-VM:SEG+
   VM-REGION IMG-AT 12 RELOC-VM:SEG+
   $D2C00000 rd or VM-REGION 4 RELOC-VM:POKE
   $F2A00000 rd or VM-REGION 4 + 4 RELOC-VM:POKE
   $F2800400 rd or bad xor {: last:n :}
   last VM-REGION 8 + 4 RELOC-VM:POKE
   0 AMAP-BIT
   CLEAR-REGS
   VM-REGION 8 RELOC-VM:R! 12 11 RELOC-VM:R!
   0 21 RELOC-VM:R! $10000 22 RELOC-VM:R! $80000 25 RELOC-VM:R!
   VM-DATA DATA RELOC-VM:R!
   RELOC-VM:RUN
   bad 0= if
      RELOC-VM:HALT-CODE 0 T=
      VM-REGION 4 RELOC-VM:PEEK $D2C00000 rd or T=
      VM-REGION 8 + 4 RELOC-VM:PEEK last T=
   else RELOC-VM:HALT-CODE SNAP-RELOC:ADDRMAP-RC T= then ;

: DATA-CHAINS ( -- )
   s" DATA carriers ending at the image boundary are not CODE-relocated" T-LABEL
   19 0 ?do i 0 DATA-CHAIN-CASE loop
   s" wrong DATA carrier register, halfword and opcode are refused" T-LABEL
   9 1 DATA-CHAIN-CASE 9 $200000 DATA-CHAIN-CASE 9 $20000000 DATA-CHAIN-CASE ;

public

: HABU-SIDE ( -- )
   PHASE-PINS
   PHASE-SHAPE
   TEACH-MACHINE
   PHASE-CALLS
   PHASE-BASE-FREE
   PHASE-XT
   PHASE-ADDRS
   DATA-CHAINS ;

;using
;package
