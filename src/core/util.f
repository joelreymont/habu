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

\ --- checker registry write-protection (dots habu-protect-type-field-04d91409,
\ habu-protect-sibling-type-44eec932). A registry control cell (variable/create)
\ is a din=0 data record, so the seal-time internal-word pass
\ (src/core/internal-mark.f) EXEMPTS it and its bare name would stay executable at
\ top level — a bare `<cell> !` then mutates the registry past the public API
\ (confirmed exploit: `99 PF-COMMIT-N !` corrupts TYPE-FIELD:COUNT). REG-PROTECT
\ records the most-recently-defined data record's dictionary index; the pass word
\ IMK-SEAL-REGISTRY (internal-mark.f) sets DNAME-INT on each after the whole cold
\ prefix loads, so a bare `<cell> @`/`<cell> !` or `' <cell>` fails closed
\ (`hb: internal engine word`, rc 70) on both --load and stdin. Core compiled
\ callers resolved before that pass keep working, and checked user code already
\ rejects the non-certified raw name (E-UNDEFINED). Read a registry through its
\ certified accessor (TYPE-FIELD:COUNT, TFAM-N@, TF-STR-U@, SCHEMA-N@, …), never
\ the raw cell. The laundered ['] -> execute route this does not close is
\ docs/design.md "Registry protection". Defined here in the first prefix source so every later prefix file
\ (type-schema.f and type-family.f both load before internal-mark.f) can tag its
\ cells at their definition site with a single REG-PROTECT.
\ An index holds only in the process that loads the prefix - a build host
\ numbers its window's records after its own primitives - so the pass retires
\ the table once it has run, and no image carries one (internal-mark.f IMK-PASS).
\ The cap is a real budget, not a round number: 47 of 64 slots were taken when
\ dot habu-tfam-2b-sealed-1b77662c sealed src/core/sumtype.f, and that seal alone
\ needs 14 more (11 public `defer` hooks and 3 public control cells - a package
\ PUBLIC data or defer record is still reachable under its qualified spelling,
\ and internal-mark.f classifies only COLON records, so REG-PROTECT is what
\ closes it). Three free slots are not a budget, and the seal campaign has one
\ registry left to convert, so the table is sized for it: 192 cells is 1.5 KB of
\ prefix data and the die above still names the overflow if it is ever reached.
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
\ at d and appends the NUL every OS path call reads, so src/core and src/habu
\ write no terminator of their own. A path the OS would read short, one holding
\ a 0 byte, is E-PATH-NUL rather than a silent truncation. Every guard throws
\ and none may be a die: PATHZ is on the open path of every engine file reader
\ (SHA256-FILE-IN, include, the REPL file loader), so a caller handed a bad path
\ must get a catchable named refusal, not a process exit (docs/forth.md §
\ Errors). The negative check is the memory-safety guard: the copy loop never
\ runs for u < 0 and cannot bound the terminator write `0 d u + c!`. The
\ checker models PATHZ/PATH0 as primitives with a stack effect only, so these
\ throws are invisible to it.
\ The tree's one path capacity, in path bytes without the NUL. A buffer that
\ holds a whole path, core, library or tool, is sized from this constant and
\ holds a path of this many bytes, plus a NUL where the OS reads it, so a path
\ one layer accepts is a path every layer accepts. src/core/cell-effects.f
\ gives this constant, E-PATH-RANGE and E-PATH-NUL a PRIM: row: this file loads
\ before the checker, and without the row a checked body that names one is
\ E-UNDEFINED on an engine that boots its prefix from source
\ (test/cold-naming-test.f).
\ The value is PATH_MAX - 1 of the smallest supported target, so every path
\ this gate admits is one the OS accepts: Darwin's PATH_MAX is 1024 counting
\ the NUL, and a 1024-byte path passed a cap of 1024 only for lstat to answer
\ ENAMETOOLONG (src/lib/fs-test.f FS-TEST-PATH-CAP-EDGE). Linux's PATH_MAX is
\ 4096, but its builds keep 1023 too: the per-task FS-ABI and FS-MUT-ABI bands
\ (src/habu/layout.f) hold path buffers of this size inside USER-REGION-END,
\ which has no room for 4095-byte ones.
\ The buffers of another size are the ones something else fixes:
\   - an OS limit: a pty slave name (src/lib/pty.f SLAVE-PATH-CAP, Darwin's
\     TIOCPTYGNAME answer) and a /proc/self/maps pathname
\     (src/habu/proc-maps.f MAPS-PATH-MAX, PATH_MAX);
\   - a foreign ABI field: the chdir path inside a posix_spawn file action
\     (src/habu/habu1.f SPAWN-CHDIR-PATH-CAP);
\   - a bootstrap stage that cannot see this file: the Gforth fixture's
\     NF-PATH-CAP (docs/gate.md "Separate Gforth recovery checks").
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
\ read a little-endian u32 from byte addr p
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
