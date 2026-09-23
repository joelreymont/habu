\ util.f — shared helpers for the checked core. Load before src/core/checker.f.

\ First source-prefix record. Its dictionary index is resolved
\ by CORE-PREFIX in the running engine: a captured ndict value would describe
\ the build host, whose primitive/helper record count can differ from ours.
\ This identifies the prefix; earlier DATA allocations can still be live.
variable IMK-NDICT0

variable SEQ

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
\ the raw cell. Defined here in the first prefix source so every later prefix file
\ (type-schema.f and type-family.f both load before internal-mark.f) can tag its
\ cells at their definition site with a single REG-PROTECT.
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

: CORE-STR= {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) byte-wise string equality
   u v = IF
     -1 SEQ !
     0 BEGIN dup u < WHILE
       dup a + c@  over b + c@  <> IF 0 SEQ ! THEN
       1 + REPEAT drop
	   ELSE 0 SEQ ! THEN
	   SEQ @ 0 <> ;

\ ASCII case fold + case-insensitive equality (declaration keyword matching).
: CORE-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CORE-STR=CI {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) folded equality
   u v = IF
     -1 SEQ !
     0 BEGIN dup u < WHILE
       dup a + c@ CORE-FOLD-C  over b + c@ CORE-FOLD-C  <> IF 0 SEQ ! THEN
       1 + REPEAT drop
	   ELSE 0 SEQ ! THEN
	   SEQ @ 0 <> ;

\ NUL-terminated path helper for open: copy (a,u) to d, append NUL.
\ Both guards below are load-bearing and neither may be a die. This is a library
\ path primitive on the open path of every file reader (SHA256-FILE, the makers,
\ the REPL file loader), so a caller handed a bad length must get a catchable
\ named refusal, not a process exit: die belongs to build-time makers and CLI
\ boundaries (docs/forth.md § Errors), and an uncatchable exit here made every
\ SHA256-FILE caller unable to report a too-long path. The negative check is the
\ memory-safety guard - without it `0 d u + c!` writes the NUL BEFORE the buffer
\ (u = -1 corrupts one byte; a large negative u faults), because the copy loop
\ `0 BEGIN dup u <` never runs for u < 0 and cannot bound the terminator write.
\ The checker models PATHZ/path0 as primitives with a stack effect only, so this
\ throw is invisible to it; callers that want a process exit still get one from
\ an uncaught throw at their top level.
\ The tree's one path capacity, in path bytes without the NUL: every path
\ buffer, core or library (lib/fs.f derives FS-PATH-CAP from it), holds a
\ path of this many bytes plus its NUL, so a path one layer accepts is a path
\ every layer accepts.
1024 constant PATH-CAP
7134 constant E-PATH-RANGE   \ path length negative, or longer than PATH-CAP bytes
: PATHZ {: a:ptr u d:ptr :} ( ptr u8 n ptr u8 -- )
   u 0 < IF E-PATH-RANGE throw THEN
   u PATH-CAP > IF E-PATH-RANGE throw THEN
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
create PZB PATH-CAP 1 + allot

: PATH0 {: a:ptr u :} ( ptr u8 n -- ptr u8 )
   a u PZB PATHZ  PZB ;     \ shared scratch
\ read a little-endian u32 from byte addr p
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
