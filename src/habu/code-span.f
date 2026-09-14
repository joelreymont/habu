\ Dictionary code lengths retain the legacy body-before-final-slot convention.
\ Bit 31 marks an exact span instead: no instruction follows the recorded body.
\ The bit survives the compact u32 dictionary unchanged. Namespace [8] cells
\ are wordlist IDs and must never pass through these code-only accessors.
package CODE-SPAN
public

$80000000 constant FULL
$7FFFFFFF constant MASK
$FFFFFFFF constant RAW-MAX
4 constant INSN-BYTES

: FULL? ( n -- bool ) FULL and 0= 0= ;
: BODY ( n -- n ) MASK and ;

: SIZE? ( n -- bool ) {: size:n :}
   size INSN-BYTES < size MASK > or if 0 0= 0= exit then
   size INSN-BYTES mod 0 = ;

: VALID? ( n -- bool ) {: raw:n :}
   raw 0 < raw RAW-MAX > or if 0 0= 0= exit then
   raw BODY INSN-BYTES mod 0 <> if 0 0= 0= exit then
   raw FULL? if raw BODY 0 > else 0 0= then ;

: CHECK ( n -- )
   VALID? 0= if s" hb: malformed dictionary code length" 74 die then ;

: BYTES ( n -- n ) {: raw:n :}
   raw CHECK
   raw FULL? if raw BODY else raw BODY INSN-BYTES + then ;

: EXACT ( n -- n ) {: size:n :}
   size SIZE? 0= if s" hb: malformed dictionary code size" 74 die then
   size FULL or ;

;package
