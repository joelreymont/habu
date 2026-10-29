\ address-carrier.f - pure address shapes and value codecs, shared by capture,
\ link and file inspection without capture buffers or assembler state. A matching
\ shape grants no provenance: callers still need the declared site map or a
\ created-owner record. Its own package, not SNAP-RELOC: the engine bakes
\ layout.f, which owns SNAP-RELOC, and the image tools that load this file on a
\ product engine may not reopen it (docs/forth-card.md § 6).

package ADDRESS-CARRIER
public

\ The four-instruction address chain habu2.f C-ADDR-RAW emits. ADDR-OPC-MASK keeps
\ a MOVZ or MOVK word without its 16-bit immediate; ADDR-IMM-MASK is that
\ immediate shifted down by five. W-MOVZ0..W-MOVK3 name x9, C-ADDR-RAW's register,
\ but a native-compiled chain names its allocator's register: the pass takes the
\ register from the site's first word and requires the other three to name it,
\ since four words naming four registers spell no address. ADDR-RD-MASK and
\ ADDR-RD-BITS are the move-wide destination field and its width.
$FFE0001F constant ADDR-OPC-MASK
$FFFF constant ADDR-IMM-MASK
16 constant ADDR-CHAIN-BYTES
12 constant DATA-CHAIN-BYTES
$1F constant ADDR-RD-MASK
5 constant ADDR-RD-BITS
$D2C00000 constant DATA-MOVZ2
$F2800000 constant DATA-MOVK0

\ The one reader and writer of a chain's value, for the capture's site scan, the
\ capture merge's rebase and any later consumer. The relocation pass in
\ src/habu/habu2.f is emitted machine code and reads the constants directly.
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

\ The same three questions for the x86-64 address site.
\ x86-64 spells the address as one `mov r64, imm64`: REX.W (B names the high
\ bank), B8+rd, eight immediate bytes, so the site is ten bytes and its patch
\ eight bytes at offset two. REX.R and REX.X have nothing to name without ModRM
\ or SIB, so MOVABS-SITE? admits exactly $48 and $49 and opcodes B8..BF: the
\ register may vary, the shape may not. test/x86-64-seam.f exercises it.
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
