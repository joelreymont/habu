split-str
\ code-dump-core.f - decode the exact live code span of a dictionary word.

require src/habu/boot-cells.f
require src/habu/code-bytes.f
require src/lib/fmt.f
require src/core/exit-status.f

HB-TARGET-LINUX-X86-64? [if] require tools/code-dump-x64.f [then]
HB-TARGET-LINUX-X86-64? 0= [if] require tools/code-dump-arm.f [then]

\ CLI: bin/hb --load tools/code-dump.f -- '<program>' WORD
\ Inline: <program> ' WORD CODE-DUMP:DECODE. A record supplies the terminal
\ address; a branch, early return or immediate byte that looks like RET does
\ not. The target's decoder (tools/code-dump-arm.f, tools/code-dump-x64.f)
\ reads one instruction per step.
package CODE-DUMP
private

: REC-FOR ( n -- ^n ) {: xt:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec XREF-RETIRED? 0=
      rec XREF-WORDLIST BOOT:NAMESPACE-WL <> and
      rec XREF-START xt = and if
         rec XREF-CODE-BYTES 0 > if rec unloop exit then
      then
   loop
   s" code-dump: no live record begins at this xt" EXIT-STATUS:NOINPUT die ;

: STEP ( ^u8 n n -- n ) {: a:^ u:n pc:n :}
   pc FMT:.INT s" : " type
   a u pc DUMP-DECODE:STEP {: bytes:n :}
   bytes 0 <= bytes u > or if
      s" code-dump: decoder escaped recorded span" EXIT-STATUS:INTERNAL die
   then
   cr bytes ;

: WALK ( n ^u8 n -- ) {: xt:n a:^ u:n :}
   0 begin dup u < while
      {: offset:n :}
      a offset + u offset - xt offset + STEP
      offset +
   repeat drop ;

: USAGE ( -- )
   s" usage: bin/hb --load tools/code-dump.f -- '<program>' WORD" EXIT-STATUS:USAGE die ;

public

: DECODE ( n -- ) {: xt:n :}
   xt REC-FOR XREF-CODE-BYTES {: bytes:n :}
   xt bytes CODE-BYTES:AT drop {: a:^ :}
   xt a bytes WALK ;

: FIND-XT ( str -- n )
   get-current XREF-FIND-WL
   dup XREF-FOUND? 0= if drop s" code-dump: target word not found" EXIT-STATUS:NOINPUT die then
   XREF-START ;

\ Evaluates caller source through the real compiler before lookup. The program
\ defines words and leaves nothing: a text that leaves or takes cells is
\ rejected with a record and 70.
: LOAD-PROGRAM ( str -- )
   evaluate-closed ;

: MAIN ( -- )
   SCRIPT-ARGC 2 <> if USAGE then
   0 SCRIPT-ARGV$ LOAD-PROGRAM
   1 SCRIPT-ARGV$ FIND-XT DECODE ;

: AUTO ( -- )
   SCRIPT-ARGC 0 > if MAIN then ;

;package
