\ code-dump-arm64.f - every live kernel record decodes through the arm64 code
\ dump. Needs a native arm64 engine.

require src/lib/test/assert.f
require test/gate-common.f
require src/habu/boot-cells.f
require src/habu/code-bytes.f
require src/lib/fmt.f
require tools/code-dump-arm.f
require src/lib/test/runner.f

package CODE-DUMP-ARM64-TEST
private

\ A kernel record lies in the engine's text, below dbase@.
: KERNEL-REC? ( ^n -- bool ) {: rec:^ :}
   rec XREF-RETIRED? if false exit then
   rec XREF-WORDLIST BOOT:NAMESPACE-WL = if false exit then
   rec XREF-CODE-BYTES 0 > rec XREF-START dbase@ < and ;

: REC-BYTES ( ^n -- str ) {: rec:^ :}
   rec XREF-START rec XREF-CODE-BYTES CODE-BYTES:AT ;

\ One line per word: the dump's text for each word of rec, in order.
: WALK-REC ( ^n -- ) {: rec:^ :}
   rec REC-BYTES {: s:str :}
   0 begin dup s STR:LEN < while
      {: off:n :}
      s STR:DATA off + s STR:LEN off - >str rec XREF-START off + DUMP-DECODE:STEP cr
      off +
   repeat drop ;

: WALK-ALL ( -- )
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec KERNEL-REC? if rec WALK-REC then
   loop ;

\ The offset in the captured walk of the next word's line.
variable LINE-AT

: NEXT-LINE ( -- str )
   GT-OUT$ {: out:str :}
   out LINE-AT @ T-LINE-END {: end:n :}
   out STR:DATA LINE-AT @ + end LINE-AT @ - >str
   end 1+ LINE-AT ! ;

\ The offset of rec's first word whose text has '?', else -1. Every arm64
\ word is 4 bytes, the walk's step for each.
: UNKNOWN-AT ( ^n -- n ) {: rec:^ :}
   rec REC-BYTES STR:LEN {: len:n :}
   0 begin dup len < while
      NEXT-LINE s" ?" STR:CONTAINS? if exit then
      4 +
   repeat drop -1 ;

\ The record's name, the word's offset and its 4 bytes.
: UNKNOWN$ ( ^n n -- str ) {: rec:^ off:n :}
   rec REC-BYTES {: s:str :}
   SB-RESET rec XREF-NAME$ SB-APPEND s"  +" SB-APPEND
   off FMT:INT$ SB-APPEND s" : " SB-APPEND
   4 0 ?do s STR:DATA off + i + c@ 2 FMT:HEX-WIDTH$ SB-APPEND loop
   SB$ ;

\ The first kernel record word the dump prints as '?', empty when none does.
: FIRST-UNKNOWN ( -- str )
   0 LINE-AT !
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec KERNEL-REC? if
         rec UNKNOWN-AT {: off:n :}
         off 0< 0= if rec off UNKNOWN$ unloop exit then
      then
   loop
   s" " ;

: CHECK-RECORDS ( -- )
   [: WALK-ALL ;] GE-CAPTURE-ACTION
   s" kernel record walk" GE-EXPECT-OK
   FIRST-UNKNOWN s" " T$= ;

public
: RUN ( -- )
   T-RESET
   s" code-dump-arm64" GT-START
   CHECK-RECORDS
   GT-CLEANUP
   T-REPORT ;
;package

CODE-DUMP-ARM64-TEST:RUN
