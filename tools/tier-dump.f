\ tier-dump.f - write one word's baked code span, as a chosen tier compiled it:
\     bin/hb --load tools/tier-dump.f -- <tier> <WORD> <out> <corpus.f>...
\     objdump -b binary -m aarch64 -D <out>
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
   s" usage: bin/hb --load tools/tier-dump.f -- <tier> <WORD> <out> <corpus.f>..."
   USAGE-RC die ;

: TIER-ARG ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 <> if USAGE then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if USAGE then
   t ;

: LOAD-CORPUS ( -- )
   script-argc 3 ?do i script-argv$ required loop ;

\ XREF-FIND misses package-private words, most of a tree, so a miss is looked for
\ across every wordlist; an ambiguous name is refused, since two packages may
\ spell a private helper the same way.

variable HITS
variable HIT-IX

: SCAN-NAME ( ptr u8 n -- ) {: a:ptr u :}
   0 HITS !  -1 HIT-IX !
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-RETIRED? 0= if
         rec a u XREF-MATCH? if
            HITS @ 1 + HITS !
            i HIT-IX !
         then
      then
   loop ;

: AMBIGUOUS ( -- )
   s" tier-dump: name is defined in more than one wordlist; no qualified spelling reaches a private word"
   USAGE-RC die ;

: FIND-REC ( ptr u8 n -- ptr n ) {: a:ptr u :}
   a u XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? if rec exit then
   a u SCAN-NAME
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
