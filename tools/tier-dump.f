\ tier-dump.f - write one word's baked code span, as a chosen tier compiled it.
\
\ Run:
\     bin/hb --load tools/tier-dump.f -- <tier> <WORD> <out> <corpus.f>...
\     objdump -b binary -m aarch64 -D <out>
\
\ Selects the tier, loads the corpus through `required`, and writes the named
\ word's exact code span to <out> as raw bytes. Two runs that differ only in the
\ tier put the same source side by side, which is the only way to see what an
\ optimization pass did to a body rather than to a total.
\
\ The word is looked up with XREF-FIND, so a package-qualified spelling
\ (`ARRAY:A-LEN`) works and a private word is reachable, and the span comes from
\ XREF-CODE-BYTES, so a body with an early exit is written whole. tools/jitdump.f
\ is the sibling that decodes in process with Habu's own ARM64 decoder; it walks
\ from an xt to the first `ret` and finds only the current wordlist, and its
\ output prints one operand per line, which a side-by-side reading cannot use.
\
\ This file requires one thing only, for the reason tools/tier-census.f gives:
\ the string and formatting libraries are corpus a tier comparison wants to
\ weigh, and a tool that loaded them for its own use could not. The one require
\ is src/habu/code-bytes.f, the boundary that turns a record's code address into
\ bytes. It requires nothing itself, and the only comparison it disturbs is one
\ whose corpus names that file, which is already loaded by then.

require src/habu/code-bytes.f

package TIER-DUMP
private

1024 constant PATH-CAP
64 constant USAGE-RC                \ sysexits EX_USAGE
74 constant IO-RC                   \ sysexits EX_IOERR
48 constant ZERO-C
1537 constant O-WRITE-NEW           \ the engine's portable O_WRONLY|O_CREAT|O_TRUNC
420 constant MODE-0644

create PATHZ PATH-CAP 1 + allot

\ `set-tier` is refused inside a plain checked body; one row, nothing else in it.
TRUSTED: SELECT-TIER ( n -- ) set-tier ;

: USAGE ( -- )
   s" usage: bin/hb --load tools/tier-dump.f -- <tier> <WORD> <out> <corpus.f>..."
   USAGE-RC die ;

: TIER-ARG ( ptr u8 n -- n ) {: a:ptr u :}
   u 1 <> if USAGE then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if USAGE then
   t ;

: ZPATH ( ptr u8 n -- ) {: a:ptr u :}
   u PATH-CAP > if s" tier-dump: path too long" IO-RC die then
   u 0 ?do a i + c@ PATHZ i + c! loop
   0 PATHZ u + c! ;

: LOAD-CORPUS ( -- )
   script-argc 3 ?do i script-argv$ required loop ;

\ XREF-FIND resolves a top-level name and a `PKG:NAME` whose tail is public;
\ a package's private words sit in a wordlist that the qualified path does not
\ reach, and most of a tree's words are private. So a name XREF-FIND misses is
\ looked for across every wordlist, and an ambiguous one is refused rather than
\ resolved by position - two packages may spell a private helper the same way,
\ and picking one silently would disassemble a word the caller did not name.

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

\ The record's start is an address the engine hands back as a number;
\ CODE-BYTES:AT is the one place it becomes bytes, and it refuses a span that is
\ not inside the running image's code, so a corrupt record cannot make this tool
\ write whatever happens to sit at the number.
: SPAN ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u FIND-REC {: rec:ptr :}
   rec XREF-CODE-BYTES {: bytes :}
   bytes 0= if s" tier-dump: word has no code" USAGE-RC die then
   rec XREF-START bytes CODE-BYTES:AT ;

: WRITE-SPAN ( ptr u8 n ptr u8 n -- ) {: path:ptr pu code:ptr bytes :}
   path pu ZPATH
   PATHZ O-WRITE-NEW MODE-0644 open {: fd :}
   fd 0 < if s" tier-dump: cannot open output" IO-RC die then
   fd code bytes write bytes <> if
      fd close
      s" tier-dump: short write" IO-RC die
   then
   fd close ;

public

: MAIN ( -- )
   script-argc 4 < if USAGE then
   0 script-argv$ TIER-ARG SELECT-TIER
   LOAD-CORPUS
   1 script-argv$ SPAN {: code:ptr bytes :}
   2 script-argv$ code bytes WRITE-SPAN ;

;package

TIER-DUMP:MAIN
