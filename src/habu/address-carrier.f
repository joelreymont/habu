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
12 constant DATA-CHAIN-BYTES
$1F constant ADDR-RD-MASK
5 constant ADDR-RD-BITS
$D2C00000 constant DATA-MOVZ2
$F2800000 constant DATA-MOVK0

private

\ The chain's own instruction word, a private copy as every reader of this
\ shape keeps one.
: W32@ ( ^u8 -- n ) {: p:^ :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: W32! ( n ^u8 -- ) {: w:n p:^ :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;

public

\ A data address is MOVZ lsl32; MOVK lsl16; MOVK lsl0: every data address a
\ target holds fits 48 bits. It must not use x20: that base is private to each
\ task. Only declared address sites use this grammar: a numeric lookalike never
\ acquires pointer provenance. Return zero on a truncated or malformed carrier,
\ without reading past end.
: CHAIN-SIZE ( ^u8 ^u8 -- n ) {: p:^ end:^ :}
   end p - DATA-CHAIN-BYTES < if 0 exit then
   p W32@ ADDR-RD-MASK and {: rd:n :}
   p W32@ ADDR-OPC-MASK and DATA-MOVZ2 rd or <>
   p 4 + W32@ ADDR-OPC-MASK and $F2A00000 rd or <> or
   p 8 + W32@ ADDR-OPC-MASK and DATA-MOVK0 rd or <> or if 0 exit then
   DATA-CHAIN-BYTES ;

: SET-CHAIN-VALUE ( str n -- ) {: p:^ val:n size:n :}
   3 0 ?do
      p i 4 * + W32@ ADDR-OPC-MASK and
      val 2 i - 16 * rshift ADDR-IMM-MASK and ADDR-RD-BITS lshift or
      p i 4 * + W32!
   loop ;
;package
