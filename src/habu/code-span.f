split-str
\ Dictionary code lengths retain the legacy body-before-final-slot convention.
\ Bit 31 marks an exact span instead: no instruction follows the recorded body.
\ An exact span is any byte count from 1 to MASK, because an x86-64 routine may
\ end on any byte. A legacy span is whole ARM64 instructions, and INSN-BYTES is
\ only the final slot a legacy body is recorded before.
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
   size 1 >= size MASK <= and ;

: VALID? ( n -- bool ) {: raw:n :}
   raw 0 < raw RAW-MAX > or if 0 0= 0= exit then
   raw FULL? if raw BODY 0 > exit then
   raw BODY INSN-BYTES mod 0 = ;

private
\ Rejects V given to the public word W: WHAT says what V is not, OK the values
\ the word takes, TEST the CODE-SPAN word that answers whether a value is one.
: BAD ( n ^u8 n ^u8 n ^u8 n ^u8 n -- )
   {: v:n w:^ wu:n what:^ whatu:n ok:^ oku:n test:^ testu:n :}
   s" E-CODE-SPAN" REJECTION:OPEN
   s" `CODE-SPAN:" REJECTION:EMIT-STR  w wu REJECTION:EMIT-STR
   s" ` was given " REJECTION:EMIT-STR  v REJECTION:EMIT-N
   s" , which is not " REJECTION:EMIT-STR  what whatu REJECTION:EMIT-STR
   s" takes" REJECTION:NOTE  ok oku REJECTION:EMIT-STR
   REJECTION:HELP
   s" test the value with `CODE-SPAN:" REJECTION:EMIT-STR  test testu REJECTION:EMIT-STR
   s" ` first and pass only one it accepts" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: LENGTH-CK ( n str -- ) {: raw:n w:^ wu:n :}
   raw VALID? 0= if
      raw w wu s" a dictionary code length"
      s" whole 4-byte instructions, or bit 31 set over 1 to 2147483647 bytes"
      s" VALID?" BAD
   then ;

public
: CHECK ( n -- ) s" CHECK" LENGTH-CK ;

: BYTES ( n -- n ) {: raw:n :}
   raw s" BYTES" LENGTH-CK
   raw FULL? if raw BODY else raw BODY INSN-BYTES + then ;

: EXACT ( n -- n ) {: size:n :}
   size SIZE? 0= if
      size s" EXACT" s" a code size" s" 1 to 2147483647 bytes" s" SIZE?" BAD
   then
   size FULL or ;

\ A record's length: size less the emission's trailing return of ret bytes; with
\ none, the whole span exactly, so a reader never borrows the next record's bytes.
: RECORDED ( n n -- n ) {: size:n ret:n :}
   ret 0= if size EXACT exit then
   size ret - ;

;package
