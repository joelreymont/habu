\ tier-dump.f - write one word's baked code span, as a chosen tier compiled it:
\     bin/hb --load tools/tier-dump.f -- <tier> <[[PKG]:]WORD> <out> <corpus.f>...
\     objdump -b binary -m aarch64 -D <out>     (GNU; on macOS aarch64-elf-objdump)
\ XREF-CODE-BYTES writes a body with an early exit whole. tools/jitdump.f decodes
\ the same span in process. Only src/habu/code-bytes.f is required (`set-tier` is
\ an engine word), for the reason tools/tier-census.f gives.

require src/habu/code-bytes.f

package TIER-DUMP
private

64 constant USAGE-RC                \ sysexits EX_USAGE
74 constant IO-RC                   \ sysexits EX_IOERR
48 constant ZERO-C
1537 constant O-WRITE-NEW           \ the engine's portable O_WRONLY|O_CREAT|O_TRUNC
420 constant MODE-0644

: USAGE ( -- )
   s" usage: bin/hb --load tools/tier-dump.f -- <tier> <[[PKG]:]WORD> <out> <corpus.f>..."
   USAGE-RC die ;

: TIER-ARG ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 <> if USAGE then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if USAGE then
   t ;

: LOAD-CORPUS ( -- )
   script-argc 3 ?do i script-argv$ required loop ;

\ A bare name must be one live word, a private one included; PKG:NAME is
\ PKG's public word, else its private one; :NAME is the global one.

variable HITS
variable HIT-IX
variable HIT-START

\ Rows that share a code start are one word (an `EXPORT` keeps its xt).
: HIT ( n -- ) {: ix:n :}
   ix XREF-REC XREF-START {: start:n :}
   HITS @ 0= if
      1 HITS !  ix HIT-IX !  start HIT-START !  exit
   then
   start HIT-START @ <> if HITS @ 1 + HITS ! then ;

\ A type entry is no word.
: WORD-ROW? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-WORDLIST {: wid :}
   BOOT:TYPES-WL {: types :}
   rec XREF-RETIRED? 0=
   wid XREF:PACKAGE-WL? 0=  and
   types 0<>  wid types =  and 0=  and ;

: SCAN-NAME ( ptr u8 n -- ) {: a:ptr u :}
   0 HITS !  -1 HIT-IX !
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec WORD-ROW? if
         rec a u XREF-MATCH? if i HIT then
      then
   loop ;

\ The last inner colon splits (`COMP:LABEL:HEAD`); a trailing one, or one after
\ a colon (`A::X` is A's `:X`), is part of the name; a leading one means global.
: QUAL-SPLIT ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 >  a c@ $3A =  and if 0 exit then
   -1  u 1 - 1 ?do
      a i + c@ $3A =  a i + 1 - c@ $3A <>  and if drop i then
   loop ;

: QUAL-INDEX ( ptr u8 n ptr n -- n ) {: t:ptr tu ns:ptr :}
   t tu ns XREF-PKG-PUBLIC XREF-FIND-WL-INDEX
   dup 0 >= if exit then drop
   ns XREF-PKG-PRIVATE {: pri :}
   pri 0= if -1 exit then
   t tu pri XREF-FIND-WL-INDEX ;

: SCAN-QUALIFIED ( ptr u8 n n -- ) {: a:ptr u k :}
   0 HITS !
   a k BOOT:NAMESPACE-WL XREF-FIND-WL {: ns:ptr :}
   ns XREF-FOUND? 0= if exit then
   a k + 1 +  u k - 1 -  ns QUAL-INDEX {: ix:n :}
   ix 0 >= if ix HIT then ;

: SCAN-GLOBAL ( ptr u8 n -- ) {: a:ptr u :}
   0 HITS !
   a 1 +  u 1 -  BOOT:GLOBAL-WL XREF-FIND-WL-INDEX {: ix:n :}
   ix 0 >= if ix HIT then ;

: AMBIGUOUS ( -- )
   s" tier-dump: name is defined more than once; name the word as PKG:NAME, or :NAME for the global one"
   USAGE-RC die ;

: FIND-REC ( ptr u8 n -- ptr n ) {: a:ptr u :}
   a u QUAL-SPLIT {: k :}
   k 0 < if a u SCAN-NAME then
   k 0 = if a u SCAN-GLOBAL then
   k 0 > if a u k SCAN-QUALIFIED then
   HITS @ 0= if s" tier-dump: word not found" USAGE-RC die then
   HITS @ 1 > if AMBIGUOUS then
   HIT-IX @ XREF-REC ;

\ CODE-BYTES:AT refuses a span outside the running image's code, so a corrupt
\ record cannot make this tool write arbitrary memory.
: SPAN ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u FIND-REC {: rec:ptr :}
   rec XREF-CODE-BYTES {: bytes :}
   bytes 0= if s" tier-dump: word has no code" USAGE-RC die then
   rec XREF-START bytes CODE-BYTES:AT ;

\ The path goes through the engine's PATH0, not FS: this file may require
\ nothing the corpus could want to weigh (see the header).
: WRITE-SPAN ( ptr u8 n ptr u8 n -- )
   {: path:ptr pu code:ptr bytes :}
   path pu PATH0 O-WRITE-NEW MODE-0644 open {: fd :}
   fd 0 < if s" tier-dump: cannot open output" IO-RC die then
   fd code bytes write bytes <> if
      fd close
      s" tier-dump: short write" IO-RC die
   then
   fd close ;

public

: MAIN ( -- )
   script-argc 4 < if USAGE then
   0 script-argv$ TIER-ARG set-tier
   LOAD-CORPUS
   1 script-argv$ SPAN {: code:ptr bytes :}
   2 script-argv$ code bytes WRITE-SPAN ;

;package

TIER-DUMP:MAIN
