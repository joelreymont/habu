\ code-bytes.f - what the code-byte boundary admits and what it refuses.
\
\ Every address here is the running image's own. The bound exists to be measured
\ against what this process actually mapped, so a fixture that invented its own
\ extents would prove nothing about the word under test: the two accepted spans
\ are read out of live dictionary records, and the refused ones are built from
\ the same live `rbase`, `dbase@` and `cp@` the boundary reads.

require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/habu/code-bytes.f

package CODE-BYTES-TEST

$100 constant CAP
create OUT CAP allot
create ERR CAP allot

4 constant INSN-BYTES

\ A word this file defines, so one of the accepted spans is in the region's code
\ band rather than the baked text. Public, because XREF-FIND reaches a private
\ word by no spelling.
public
: LIVE-SUBJECT ( n -- n ) 1+ ;
private

: REC ( ptr u8 n -- ptr n )
   XREF-FIND dup XREF-FOUND? 0= if
      s" code-bytes-test: subject not published" 74 die
   then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-BYTES ( ptr u8 n -- n )
   REC XREF-CODE-BYTES ;

\ A package name's record reuses a word's two code-span cells for the two
\ wordlist ids it publishes, so its "start" is a small integer and the span it
\ would name is not code at all. That is the miss this bound exists to catch,
\ and it is taken from the live dictionary rather than invented.
: NAMESPACE-START ( -- n )
   ndict@ 1 - begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-NAMESPACE-WL = if
         XREF-REC XREF-PKG-PUBLIC exit
      then
      1-
   repeat
   drop s" code-bytes-test: no namespace record" 74 die ;

\ The out-of-region refusal exits the process, so it is proven in a child.
: REFUSES ( -- )
   s" : CB-OUT-OF-REGION ( -- ) 12 4 CODE-BYTES:AT drop drop ; CB-OUT-OF-REGION"
   OUT CAP >LEN ERR CAP >LEN 1000 >MS SUBJECT:RUN
   74 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   ERR erru LEN>N S\" hb: span outside the code region\n" T$= ;

: RUN ( -- )
   T-RESET

   s" a published record's own span is code" T-LABEL
   s" XREF-START" REC-START s" XREF-START" REC-BYTES CODE-BYTES:IN-CODE? TTRUE
   s" CODE-BYTES-TEST:LIVE-SUBJECT" REC-START
      s" CODE-BYTES-TEST:LIVE-SUBJECT" REC-BYTES CODE-BYTES:IN-CODE? TTRUE
   cp@ INSN-BYTES - INSN-BYTES CODE-BYTES:IN-CODE? TTRUE
   rbase INSN-BYTES CODE-BYTES:IN-CODE? TTRUE

   s" a wordlist id is not a code address" T-LABEL
   NAMESPACE-START INSN-BYTES CODE-BYTES:IN-CODE? TFALSE

   s" an address outside both bands is refused" T-LABEL
   0 INSN-BYTES CODE-BYTES:IN-CODE? TFALSE
   -1 INSN-BYTES CODE-BYTES:IN-CODE? TFALSE
   rbase INSN-BYTES - INSN-BYTES CODE-BYTES:IN-CODE? TFALSE
   dbase@ INSN-BYTES CODE-BYTES:IN-CODE? TFALSE
   dbase@ DICT-SIZE + INSN-BYTES - INSN-BYTES CODE-BYTES:IN-CODE? TFALSE
   cp@ INSN-BYTES CODE-BYTES:IN-CODE? TFALSE

   s" a span inside no single band is refused, however it starts" T-LABEL
   dbase@ DICT-SIZE + INSN-BYTES - INSN-BYTES 2 * CODE-BYTES:IN-CODE? TFALSE
   rbase dbase@ rbase - INSN-BYTES + CODE-BYTES:IN-CODE? TFALSE
   cp@ INSN-BYTES - INSN-BYTES 2 * CODE-BYTES:IN-CODE? TFALSE

   s" a length no code span could have is refused" T-LABEL
   rbase -1 CODE-BYTES:IN-CODE? TFALSE
   rbase REGION 1+ CODE-BYTES:IN-CODE? TFALSE
   \ Large enough that start + bytes wraps negative. Without the length bound
   \ the sum lands below the band top and the span is admitted.
   rbase $7FFFFFFFFFFFFFFF CODE-BYTES:IN-CODE? TFALSE
   cp@ INSN-BYTES - $7FFFFFFFFFFFFFFF CODE-BYTES:IN-CODE? TFALSE

   s" an admitted span answers its own bytes and its own length" T-LABEL
   s" XREF-START" REC-START s" XREF-START" REC-BYTES CODE-BYTES:AT
      {: p:ptr got:n :}
   got s" XREF-START" REC-BYTES T=
   p c@ s" XREF-START" REC-START INSN-BYTES CODE-BYTES:AT drop c@ T=

   s" a span outside the code region refuses by name" T-LABEL
   REFUSES

   T-REPORT ;

' RUN
;package
execute
