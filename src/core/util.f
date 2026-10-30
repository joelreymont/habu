\ util.f — shared helpers for the checked core. Load before src/core/checker.f.

\ First source-prefix record. Its dictionary index is resolved
\ by CORE-PREFIX in the running engine: a captured ndict value would describe
\ the build host, whose primitive/helper record count can differ from ours.
\ This identifies the prefix; earlier DATA allocations can still be live.
variable IMK-NDICT0

\ The cell of a dictionary record (src/habu/layout.f DREC) that holds the
\ wordlist it was published into, 0 for the global one. src/habu/xref.f reads
\ records by it, and src/core/checker.f, which loads before xref.f, maps the
\ record the engine's lookup binds to a scope by it.
5 constant DICT-WORDLIST-SLOT

\ The scope-find flag bit set when two used publics hold the tail.
2 constant SCOPE-FIND-AMBIGUOUS

\ --- checker registry write-protection. A registry control cell is a din=0 data
\ record, which the seal pass (src/core/internal-mark.f) exempts, so its bare
\ name would stay executable (`99 PF-COMMIT-N !` corrupted TYPE-FIELD:COUNT).
\ REG-PROTECT records the last-defined data record's index and internal-mark.f
\ IMK-SEAL-REGISTRY sets DNAME-INT on each after the cold prefix loads; compiled
\ core callers keep working. Read a registry through its certified accessor; the
\ laundered ['] -> execute route is docs/design.md "Registry protection". A
\ package's public data or defer record needs REG-PROTECT too: internal-mark.f
\ classifies only colon records. An index holds only in the loading process, so
\ the pass retires the table (internal-mark.f IMK-PASS).
192 constant REG-PROT-CAP
create REG-PROT-IDX  REG-PROT-CAP cells allot
variable REG-PROT-N   0 REG-PROT-N !
: REG-PROTECT ( -- )   \ tag the just-defined data record for seal-time internal-marking
   REG-PROT-N @ REG-PROT-CAP >= IF s" registry protect overflow" 76 die THEN
   ndict@ 1 -  REG-PROT-IDX REG-PROT-N @ cells + !
   1 REG-PROT-N +! ;
\ The seal pass reads the registrations through these and retires them.
: REG-PROT-COUNT ( -- n ) REG-PROT-N @ ;
: REG-PROT-RECORD ( n -- n ) cells REG-PROT-IDX + @ ;
: REG-PROT-RETIRE ( -- )
   REG-PROT-N @ 0 ?do 0 i cells REG-PROT-IDX + ! loop
   0 REG-PROT-N ! ;

\ Set by the build that loads this prefix as its target when the product keeps
\ its internal words reachable; every other load leaves it 0.
variable ENGINE-UNSEAL
REG-PROTECT
\ The seal pass reads the request through this: a checked body on a cold boot
\ cannot name the cell, which has no row.
: ENGINE-UNSEAL? ( -- bool ) ENGINE-UNSEAL @ 0= 0= ;

: CORE-STR= {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) byte-wise string equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@  b i + c@  <> if 0 0= 0= unloop exit then  loop 0 0= ;

\ ASCII case fold + case-insensitive equality (declaration keyword matching).
: CORE-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CORE-STR=CI {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) folded equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@ CORE-FOLD-C  b i + c@ CORE-FOLD-C  <> if 0 0= 0= unloop exit then  loop 0 0= ;

\ PATHZ is the engine's one C-path copier: it copies (a,u) into the room bytes
\ at d and appends the NUL, so src/core and src/habu write no terminator of
\ their own. A path holding a 0 byte, which the OS would read short, is
\ E-PATH-NUL. Every guard throws, never dies: every file reader opens through
\ here and a caller must get a catchable refusal (docs/forth.md § Errors). The
\ negative check is the memory guard: for u < 0 the copy loop never runs and
\ `0 d u + c!` writes before the buffer. The checker models PATHZ/PATH0 with a
\ stack effect only, so it does not see these throws.
\ PATH-CAP is the tree's one path capacity, in bytes without the NUL: a path one
\ layer accepts every layer accepts. src/core/cell-effects.f gives it,
\ E-PATH-RANGE and E-PATH-NUL their rows (this file loads before the checker).
\ The value is PATH_MAX - 1 of the smallest target: Darwin's PATH_MAX is 1024
\ with the NUL, and lstat refused a 1024-byte path (src/lib/fs-test.f
\ FS-TEST-PATH-CAP-EDGE); Linux keeps 1023 because the per-task FS-ABI and
\ FS-MUT-ABI bands (src/habu/layout.f) hold path buffers of this size inside
\ USER-REGION-END. Buffers of another size are fixed by an OS limit
\ (src/lib/pty.f SLAVE-PATH-CAP, src/habu/proc-maps.f MAPS-PATH-MAX), a foreign
\ ABI field (src/habu/habu1.f SPAWN-CHDIR-PATH-CAP) or a bootstrap stage that
\ cannot see this file (the Gforth fixture's NF-PATH-CAP).
1023 constant PATH-CAP
7134 constant E-PATH-RANGE   \ path length negative, or no room for it and its NUL
7165 constant E-PATH-NUL     \ a path holds a 0 byte, which would end it early
: PATHZ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u d:ptr room :}
   u 0 < IF E-PATH-RANGE throw THEN
   u room >= IF E-PATH-RANGE throw THEN
   0 BEGIN dup u < WHILE
      dup a + c@  dup 0= IF E-PATH-NUL throw THEN  over d + c!  1 +
   REPEAT drop  0 d u + c! ;
create PZB PATH-CAP 1 + allot

: PATH0 ( ptr u8 n -- ptr u8 )
   {: a:ptr u :}
   a u PZB PATH-CAP 1 + PATHZ  PZB ;     \ shared scratch
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

\ A source line and its number as the engine's refusal tail (habu2.f
\ LCOMPILEDIE) counts and prints them.
package SOURCE-TEXT

private

20 constant DIGITS-CAP
create DIGITS DIGITS-CAP allot
variable DIGIT-AT

: DIGIT+ ( n -- ) {: c:n :}
   DIGIT-AT @ 1 - DIGIT-AT !
   c DIGITS DIGIT-AT @ + c! ;

public

: LINE ( ptr u8 ptr u8 -- n )
   {: b:ptr p:ptr :}
   1 p b - 0 ?do b i + c@ 10 = if 1 + then loop ;

\ A non-negative number's decimal digits, written from the end of DIGITS.
: DIGITS$ ( n -- ptr u8 n )
   DIGITS-CAP DIGIT-AT !
   begin dup 10 mod $30 + DIGIT+ 10 / dup 0= until drop
   DIGITS DIGIT-AT @ +  DIGITS-CAP DIGIT-AT @ - ;
;package
