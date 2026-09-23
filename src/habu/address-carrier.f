\ address-carrier.f - pure address shapes and value codecs.
\
\ Capture, link and file inspection share this grammar without loading capture
\ buffers or assembler state. A matching shape grants no address provenance:
\ callers still need the declared site map or a created-owner record.

package SNAP-RELOC
public

\ The shape of the four-instruction address chain habu2.f C-ADDR-RAW emits, as the
\ relocation pass has to read it back. ADDR-OPC-MASK keeps everything in a MOVZ or
\ MOVK word except its 16-bit immediate, so masking a site word leaves the opcode,
\ the shift and the destination register to compare. ADDR-IMM-MASK is that
\ immediate once it has been shifted down by five, and ADDR-CHAIN-BYTES is the
\ whole chain.
\ W-MOVZ0/W-MOVK1/W-MOVK2/W-MOVK3 all name x9, because that is the register the ONE
\ carrier C-ADDR-RAW writes into, but they are not the only chains that reach the
\ map: a chain the native compiler emits names whichever register its allocator
\ chose. So the pass takes the register from the SITE's own first word and requires
\ the other three lanes to name that same one. ADDR-RD-MASK is the destination-
\ register field of a move-wide word and ADDR-RD-BITS its width, so shifting a
\ scaffold down by that width and back clears the register out of it and leaves the
\ opcode and the shift, which is what the site's register is then put back into.
\ Accepting any register without the agreement requirement would accept four words
\ that name four different registers, whose four immediates spell out no address at
\ all; formal/Common/Reloc.v states both halves.
$FFE0001F constant ADDR-OPC-MASK
$FFFF constant ADDR-IMM-MASK
16 constant ADDR-CHAIN-BYTES
12 constant DATA-CHAIN-BYTES
$1F constant ADDR-RD-MASK
5 constant ADDR-RD-BITS
$D2C00000 constant DATA-MOVZ2
$F2800000 constant DATA-MOVK0

\ ONE READER AND ONE WRITER OF A CHAIN'S VALUE, beside the shape they read it
\ with. Three passes need them - the capture's site scan, the merge that rebases
\ a read-back capture into a host's coordinates, and any later consumer - and a
\ private copy in each is exactly the drift the shape constants moved here to
\ stop. The relocation pass in src/habu/habu2.f is not one of the three: it is
\ EMITTED machine code and consumes the constants directly.
private

\ The chain's own instruction word. Owner-prefixed like every other reader of
\ this shape in the tree (AOT-W32@, ACAP-W32@; see tools/jitdump-core.f).
: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;

public

\ The four immediate fields as one value, whatever register the chain names: the
\ immediate sits just above the destination-register field, which is what
\ ADDR-RD-BITS is the width of.
: CHAINV ( ptr u8 -- n ) {: p:ptr :}
   p W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and
   p 4 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 16 lshift or
   p 8 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 32 lshift or
   p 12 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 48 lshift or ;

\ Re-encode a full 64-bit value into an existing chain, keeping each word's
\ opcode AND its destination register - it never reads the register it
\ preserves, so a chain the native compiler emitted into an allocator's register
\ rewrites as readily as one the engine emitted into x9.
: SET-CHAIN ( ptr u8 n -- ) {: p:ptr val:n :}
   4 0 ?do
      p i 4 * + W32@ ADDR-OPC-MASK and
      val i 16 * rshift ADDR-IMM-MASK and ADDR-RD-BITS lshift or
      p i 4 * + W32!
   loop ;

\ Shared DATA uses MOVZ lsl32; MOVK lsl16; MOVK lsl0. Both fixed DATA
\ mappings fit 48 bits. It must not use x20: that base is private to each task.
\ CODE addresses retain the four-half carrier. Only declared address sites
\ use this grammar: a numeric lookalike never acquires pointer provenance.
\ Return zero on a truncated or malformed carrier, without reading past end.
: CHAIN-SIZE ( ptr u8 ptr u8 -- n ) {: p:ptr end:ptr :}
   end p - DATA-CHAIN-BYTES < if 0 exit then
   p W32@ ADDR-RD-MASK and {: rd:n :}
   p W32@ ADDR-OPC-MASK and DATA-MOVZ2 rd or = if
      p 4 + W32@ ADDR-OPC-MASK and $F2A00000 rd or <>
      p 8 + W32@ ADDR-OPC-MASK and DATA-MOVK0 rd or <> or if 0 exit then
      DATA-CHAIN-BYTES exit
   then
   p W32@ ADDR-OPC-MASK and $D2800000 rd or <>
   p 4 + W32@ ADDR-OPC-MASK and $F2A00000 rd or <> or if 0 exit then
   end p - ADDR-CHAIN-BYTES < if 0 exit then
   p 8 + W32@ ADDR-OPC-MASK and $F2C00000 rd or <>
   p 12 + W32@ ADDR-OPC-MASK and $F2E00000 rd or <> or if 0 exit then
   ADDR-CHAIN-BYTES ;

\ Live carriers hold absolute addresses; captured DATA sites hold window
\ coordinates until the boot rebase. The same fields carry either number.
: CHAIN-VALUE ( ptr u8 n -- n ) {: p:ptr size:n :}
   size ADDR-CHAIN-BYTES = if p CHAINV exit then
   size DATA-CHAIN-BYTES <> if s" address chain: invalid size" 74 die then
   0 3 0 ?do
      p i 4 * + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and
      2 i - 16 * lshift or
   loop ;

: SET-CHAIN-VALUE ( ptr u8 n n -- ) {: p:ptr val:n size:n :}
   size ADDR-CHAIN-BYTES = if p val SET-CHAIN exit then
   size DATA-CHAIN-BYTES <> if s" address chain: invalid size" 74 die then
   val 0 < val $FFFFFFFFFFFF > or if
      s" address chain: DATA address exceeds 48 bits" 74 die
   then
   3 0 ?do
      p i 4 * + W32@ ADDR-OPC-MASK and
      val 2 i - 16 * rshift ADDR-IMM-MASK and ADDR-RD-BITS lshift or
      p i 4 * + W32!
   loop ;

\ ---- the same three questions for the x86-64 address site --------------------
\ AArch64 patches three DATA or four full move-wide halves; x86-64 spells the
\ address as ONE instruction, `mov r64, imm64`, and
\ patches the whole value at once: REX.W (with B naming the high register bank),
\ then B8+rd, then the eight immediate bytes. So the site is ten bytes wide and
\ its patch is eight bytes at offset two, which is what MOVABS-BYTES,
\ MOVABS-IMM-OFF and MOVABS-IMM-BYTES say, and what the model in
\ formal/Common/Reloc.v is asked about.
\
\ REX.R and REX.X have nothing to name here - the form carries no ModRM and no
\ SIB byte - so a prefix that sets either is some other instruction, not this
\ site with spare bits. MOVABS-SITE? therefore admits exactly $48 and $49, and
\ exactly the eight opcodes B8..BF, which is the same fail-closed reading the
\ chain's scaffold compare makes: the register may vary, the shape may not.
$FE constant MOVABS-REX-MASK
$48 constant MOVABS-REX-W
$F8 constant MOVABS-OP-MASK
$B8 constant MOVABS-OP
10 constant MOVABS-BYTES
2 constant MOVABS-IMM-OFF
8 constant MOVABS-IMM-BYTES

: MOVABS-SITE? ( ptr u8 -- bool ) {: p:ptr :}
   p c@ MOVABS-REX-MASK and MOVABS-REX-W =
   p 1+ c@ MOVABS-OP-MASK and MOVABS-OP = and ;

\ The immediate as one value, whatever register the instruction names.
: MOVABSV ( ptr u8 -- n ) {: p:ptr :}
   0
   MOVABS-IMM-BYTES 0 ?do
      p MOVABS-IMM-OFF + i + c@  i 8 * lshift  or
   loop ;

\ Re-encode a full 64-bit value into an existing site, keeping the prefix and
\ the opcode - and so the destination register - exactly as they were.
: SET-MOVABS ( ptr u8 n -- ) {: p:ptr val:n :}
   MOVABS-IMM-BYTES 0 ?do
      val i 8 * rshift $FF and  p MOVABS-IMM-OFF + i + c!
   loop ;
;package
