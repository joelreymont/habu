\ A dictionary code length is the routine's whole byte count, 1 to MASK: an
\ x86-64 routine may end on any byte. A record an hb before this one laid
\ carries bit 31 over the count, which BODY drops. Namespace [8] cells are
\ wordlist IDs and must never pass through these code-only accessors.
package CODE-SPAN
public

$80000000 constant FULL
$7FFFFFFF constant MASK
$FFFFFFFF constant RAW-MAX

: FULL? ( n -- bool ) FULL and 0= 0= ;
: BODY ( n -- n ) MASK and ;

: SIZE? ( n -- bool ) {: size:n :}
   size 1 >= size MASK <= and ;

: VALID? ( n -- bool ) {: raw:n :}
   raw 0 >= raw RAW-MAX <= and  raw BODY SIZE? and ;

private
\ Rejects V given to the public word W: WHAT says what V is not, OK the values
\ the word takes, TEST the CODE-SPAN word that answers whether a value is one.
: BAD ( n str str str str -- )
   {: v:n w:str what:str ok:str test:str :}
   s" E-CODE-SPAN" REJECTION:OPEN
   s" `CODE-SPAN:" REJECTION:EMIT-STR  w REJECTION:EMIT-STR
   s" ` was given " REJECTION:EMIT-STR  v REJECTION:EMIT-N
   s" , which is not " REJECTION:EMIT-STR  what REJECTION:EMIT-STR
   s" takes" REJECTION:NOTE  ok REJECTION:EMIT-STR
   REJECTION:HELP
   s" test the value with `CODE-SPAN:" REJECTION:EMIT-STR  test REJECTION:EMIT-STR
   s" ` first and pass only one it accepts" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: LENGTH-CK ( n str -- ) {: raw:n w:str :}
   raw VALID? 0= if
      raw w s" a dictionary code length"
      s" 1 to 2147483647 bytes, bare or under bit 31"
      s" VALID?" BAD
   then ;

public
: CHECK ( n -- ) s" CHECK" LENGTH-CK ;

: EXACT ( n -- n ) {: size:n :}
   size SIZE? 0= if
      size s" EXACT" s" a code size" s" 1 to 2147483647 bytes" s" SIZE?" BAD
   then
   size ;

;package
