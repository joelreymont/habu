\ jitdump-core.f - decode the exact live code span of a dictionary word.

require src/habu/code-bytes.f
require lib/fmt.f

package JITDUMP-LOAD
: TARGET ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" tools/jitdump-x64.f" required
   else
      s" tools/jitdump-arm.f" required
   then ;
' TARGET
;package
execute

\ CLI: bin/hb --load tools/jitdump.f -- '<program>' WORD
\ Inline: <program> ' WORD JITDUMP:JD. A record supplies the terminal address;
\ a branch, early return or immediate byte that looks like RET does not.
package JITDUMP
private

: REC-FOR ( n -- ptr n ) {: xt:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-RETIRED? 0=
      rec XREF-WORDLIST XREF-NAMESPACE-WL <> and
      rec XREF-START xt = and if
         rec XREF-CODE-BYTES 0 > if rec unloop exit then
      then
   loop
   s" jitdump: no live record begins at this xt" 74 die ;

: STEP ( ptr u8 n n -- n ) {: a:ptr u:n pc:n :}
   pc FMT:.INT s" : " type
   a u pc JIT-DIS:STEP {: bytes:n :}
   bytes 0 <= bytes u > or if s" jitdump: decoder escaped recorded span" 74 die then
   cr bytes ;

: WALK ( n ptr u8 n -- ) {: xt:n a:ptr u:n :}
   0 begin dup u < while
      {: offset:n :}
      a offset + u offset - xt offset + STEP
      offset +
   repeat drop ;

: JIT-USAGE ( -- )
   s" usage: bin/hb --load tools/jitdump.f -- '<program>' WORD" 64 die ;

public

: JD ( n -- ) {: xt:n :}
   xt REC-FOR XREF-CODE-BYTES {: bytes:n :}
   xt bytes CODE-BYTES:AT drop {: a:ptr :}
   xt a bytes WALK ;

: JIT-FIND ( ptr u8 n -- n )
   get-current XREF-FIND-WL
   dup XREF-FOUND? 0= if drop s" jitdump: target word not found" 74 die then
   XREF-START ;

\ Evaluates caller source through the real compiler before lookup. The program
\ defines words and leaves nothing: a cell it leaves is E-EVAL-RESIDUE.
: JIT-EVALUATE ( ptr u8 n -- )
   evaluate-closed ;

: JIT-MAIN ( -- )
   SCRIPT-ARGC 2 <> if JIT-USAGE then
   0 SCRIPT-ARGV$ JIT-EVALUATE
   1 SCRIPT-ARGV$ JIT-FIND JD ;

: JIT-AUTO ( -- )
   SCRIPT-ARGC 0 > if JIT-MAIN then ;

;package
