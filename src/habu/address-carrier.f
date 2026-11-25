split-str
\ address-carrier.f - the arm64 data address chain shape and its writer. A matching
\ shape grants no provenance: callers still need an address site row or a
\ created-owner record. Its own package, not RELOC: the engine bakes
\ layout.f, which owns RELOC, and the image tools that load this file on a
\ target engine may not reopen it (docs/forth-card.md § 6).

package ADDRESS-CARRIER
public

\ ADDR-OPC-MASK keeps a MOVZ or MOVK word without its 16-bit immediate;
\ ADDR-RD-MASK is its destination field, ADDR-RD-BITS wide.
$FFE0001F constant ADDR-OPC-MASK
$FFFF constant ADDR-IMM-MASK
$1F constant ADDR-RD-MASK
5 constant ADDR-RD-BITS

private

\ The chain's own instruction word, a private copy as every reader of this
\ shape keeps one.
: W32@ ( ^u8 -- n ) {: p:^ :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: W32! ( n ^u8 -- ) {: w:n p:^ :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;

public

\ Write val, below 2^48, into the chain at p: MOVZ lsl32; MOVK lsl16; MOVK lsl0
\ in one register, never x20, the base private to each task.
: SET-CHAIN-VALUE ( n ^u8 -- ) {: val:n p:^ :}
   3 0 ?do
      p i 4 * + W32@ ADDR-OPC-MASK and
      val 2 i - 16 * rshift ADDR-IMM-MASK and ADDR-RD-BITS lshift or
      p i 4 * + W32!
   loop ;
;package
