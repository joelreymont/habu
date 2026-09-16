\ perf-map-core.f - rewrite the addresses in `perf script` output as Habu words.
\
\ External perf keeps the exact call chains the in-binary profiler's conservative
\ walk can only approximate, but it prints the engine's code addresses as
\ [unknown]: every Habu word lives in one anonymous mapping perf has no symbols
\ for. This filter puts the names back, so
\
\   perf record -g <engine> --load prog.f
\   perf script | <engine> --load tools/perf-map.f
\
\ reads as words and packages rather than hex.
\
\ THE INDEX IS THE PROFILER'S OWN. `prof-on` builds the pc-sorted live-range
\ index and `prof-off` stops the clock again before the report needs a sample, so
\ an address is resolved here by exactly the code the SIGALRM handler runs
\ (prof-pc>rec), not by a second implementation that could disagree with it.
\
\ ONLY A CALL-CHAIN LINE CARRIES AN ADDRESS. perf script writes a sample as an
\ unindented header line - "comm pid timestamp: period event:" - followed by one
\ indented line per frame, "<hex> <symbol> (<dso>)". The header's pid and period
\ are decimal runs that are also valid hex, so a filter that mapped "anything
\ shaped like an address" would rewrite them; this one maps only the first token
\ of an indented line, which is the one column perf documents as an address.
\
\ WHICH ADDRESSES RESOLVE: the engine is position-dependent, so a code address
\ from another run of the SAME binary maps directly. Words the profiled program
\ compiled for itself are not in this process's dictionary and stay hex, exactly
\ as they do in the internal profiler's deferred bucket. The dso column is not
\ consulted: every Habu word lives in one anonymous mapping, so the address is
\ the only thing that can decide, and an address outside this dictionary's code
\ range - every shared library on this machine sits far above it - resolves to
\ nothing and is left alone.
require lib/errors.f
require lib/string.f
require lib/render.f

package PERF-MAP
private

7401 constant E-IO
$10000 constant IN-CAP           \ one read chunk
4096 constant LINE-CAP
512 constant PKG-CAP             \ package rows the qualifier table holds
4 constant ADDR-MIN              \ hex digits before a token is taken for an address
16 constant ADDR-MAX
9 constant TAB

create IN-BUF IN-CAP allot
create LINE-BUF LINE-CAP allot
create PKG-WID PKG-CAP cells allot
create PKG-REC PKG-CAP cells allot

variable IN-U      variable IN-I
variable LINE-U
variable PKG-N
variable CUR       variable TOK
variable ACC
variable OK
variable MAPPED    \ has this line's address column been rewritten already

\ ---- the package qualifier table ---------------------------------------------
\ A namespace row publishes its public wid in the cell a word uses for its code
\ start and its private wid in the cell a word uses for its length; XREF names
\ both. Collected once: a dictionary scan per address would cost more than the
\ whole rewrite.
: PKG+ ( n n -- ) {: wid:n rec:n :}
   wid 0= if exit then
   PKG-N @ PKG-CAP >= if exit then
   wid PKG-WID PKG-N @ cells + !
   rec PKG-REC PKG-N @ cells + !
   PKG-N @ 1+ PKG-N ! ;

: PKG-SCAN ( -- )
   0 PKG-N !
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC i PKG+
         rec XREF-PKG-PRIVATE i PKG+
      then
   loop ;

: PKG-OF ( n -- n ) {: wid:n :}         \ the package record for a wid, -1 when none
   wid 0= if -1 exit then
   -1 ACC !
   PKG-N @ 0 ?do
      PKG-WID i cells + @ wid = if PKG-REC i cells + @ ACC ! then
   loop
   ACC @ ;

: QNAME+ ( n -- ) {: idx:n :}           \ append the record's qualified name
   idx XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST PKG-OF {: p:n :}
   p 0 >= if p XREF-REC XREF-NAME$ RENDER:RB+ s" :" RENDER:RB+ then
   rec XREF-NAME$ RENDER:RB+ ;

\ ---- hex tokens ---------------------------------------------------------------
: HEX-DIGIT ( n -- n ) {: c:n :}        \ the digit's value, or -1
   c [char] 0 >= c [char] 9 <= and if c [char] 0 - exit then
   c [char] a >= c [char] f <= and if c [char] a - 10 + exit then
   c [char] A >= c [char] F <= and if c [char] A - 10 + exit then
   -1 ;

: HEX@ ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 ACC !
   u 0 ?do ACC @ 16 * a i + c@ HEX-DIGIT + ACC ! loop
   ACC @ ;

: HEX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u ADDR-MIN < if STR-FALSE exit then
   u ADDR-MAX > if STR-FALSE exit then
   1 OK !
   u 0 ?do a i + c@ HEX-DIGIT 0 < if 0 OK ! then loop
   OK @ 0<> ;

: ADDR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u HEX@ {: addr:n :}
   addr prof-pc>rec {: idx:n :}
   idx 0 < if a u RENDER:RB+ exit then
   idx QNAME+
   addr idx XREF-REC XREF-START - {: off:n :}
   off 0 > if s" +" RENDER:RB+ off RENDER:RB# then ;

\ ---- one line ------------------------------------------------------------------
: SEP? ( n -- bool ) {: c:n :}
   c STR-SPACE = c TAB = or ;

: INDENTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if STR-FALSE exit then
   a 0 + c@ SEP? ;

\ The address column is the first token of an indented line, and only that one.
: ADDR-HERE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   MAPPED @ 0<> if STR-FALSE exit then
   a u HEX? ;

public
\ The rewritten line, in RENDER's buffer. Separated from the stdin driver so a
\ fixture can hand it a line and read the answer back.
: LINE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   RENDER:RESET
   0 CUR !
   a u INDENTED? 0= if 1 else 0 then MAPPED !
   begin CUR @ u < while
      a CUR @ + c@ SEP? if
         a CUR @ + 1 RENDER:RB+  CUR @ 1+ CUR !
      else
         CUR @ TOK !
         begin CUR @ u < if a CUR @ + c@ SEP? 0= else STR-FALSE then while
            CUR @ 1+ CUR !
         repeat
         a TOK @ +  CUR @ TOK @ -  2dup ADDR-HERE? if
            ADDR+  1 MAPPED !
         else
            RENDER:RB+  1 MAPPED !
         then
      then
   repeat
   RENDER:RB$ ;
private

: LINE. ( ptr u8 n -- )
   LINE$ type cr ;

\ ---- streaming stdin ----------------------------------------------------------
: REFILL ( -- bool )
   0 IN-BUF IN-CAP read {: got:n :}
   got 0 < if s" perf-map: cannot read stdin" E-IO die then
   got IN-U !  0 IN-I !
   got 0 > ;

: NEXT-BYTE ( -- n )                    \ -1 at end of input
   IN-I @ IN-U @ >= if
      REFILL 0= if -1 exit then
   then
   IN-BUF IN-I @ + c@  IN-I @ 1+ IN-I ! ;

: LINE+ ( n -- ) {: c:n :}
   LINE-U @ LINE-CAP >= if exit then    \ a line past the buffer keeps its tail unmapped
   c LINE-BUF LINE-U @ + c!
   LINE-U @ 1+ LINE-U ! ;

public
: ARM ( -- )
   0 prof-on prof-off                   \ build the index; the clock stops before it matters
   PKG-SCAN ;

: RUN ( -- )
   ARM
   0 LINE-U !
   begin NEXT-BYTE dup 0 >= while
      dup STR-LF = if
         drop
         LINE-BUF LINE-U @ LINE.  0 LINE-U !
      else
         LINE+
      then
   repeat
   drop
   LINE-U @ 0 > if LINE-BUF LINE-U @ LINE. then ;

: MAIN ( -- ) RUN ;
;package
