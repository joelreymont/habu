\ util.fs — shared in-subset helpers for the selfhost layer. Load FIRST (before
\ walk.fs / checker.fs / vs.fs, which all use CORE-STR=).

\ Prim-boundary dictionary watermark for the seal-time internal-word marking
\ pass (src/core/internal-mark.f, dot habu-hb-crash-bare-c5be6634). util.f is
\ the first prefix source file, so ndict here = the baked primitive record
\ count; the pass walks [IMK-NDICT0, ndict). Defined FIRST so the refresh
\ truncation (tools/build-fixpoint.f BF-STAGE2-HIDE-DEFS) can retire and
\ replay it with the rest of the file.
variable IMK-NDICT0
ndict@ IMK-NDICT0 !

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

\ --- implementation surfaces: publication is an act, not a default -----------
\ internal-mark.f refuses a global colon word the checker cannot type and lets
\ every one it CAN type stay top-level executable, because for a file whose
\ definitions record no signature those two questions are the same question.
\ They stop being the same question the moment an implementation file becomes
\ checker-known: its internals acquire effects and the pass then admits all of
\ them to the top-level name universe (measured: 102 global names, four of them
\ taking the process down with a register dump, dot habu-route-3-the-64078d43).
\ A file wraps itself in IMPLEMENTATION ... ;IMPLEMENTATION to say that its
\ definitions are internals whatever the checker knows of them, and marks each
\ definition that IS interface with a trailing API. Unmarked means closed, so a
\ definition added anywhere in a declared file is refused at top level with no
\ other edit - which is the property a list of protected names cannot have.
\ Nothing here changes what a COMPILED caller or the checker resolves: DNAME-INT
\ is read by interpret dispatch and interpret tick only.
\ These control cells are data records, so the span walk below never reaches
\ them: each carries its own REG-PROTECT or the mechanism would hand its own
\ state to the top level it exists to close.
16 constant IMPL-SPAN-CAP   REG-PROTECT
create IMPL-SPAN-FROM IMPL-SPAN-CAP cells allot   REG-PROTECT
create IMPL-SPAN-TO   IMPL-SPAN-CAP cells allot   REG-PROTECT
variable IMPL-SPAN-N   0 IMPL-SPAN-N !   REG-PROTECT
variable IMPL-OPEN     0 IMPL-OPEN !   REG-PROTECT

\ Sized for the surfaces the route-3 landing declares (checker.f 134, render.f 3,
\ layout-buffer.f 6) with room for the files the seal campaign has left.
512 constant IMPL-API-CAP   REG-PROTECT
create IMPL-API-IDX IMPL-API-CAP cells allot   REG-PROTECT
variable IMPL-API-N   0 IMPL-API-N !   REG-PROTECT

: IMPLEMENTATION ( -- )   \ open an implementation region at the current dictionary end
   IMPL-OPEN @ 0 <> IF s" implementation region already open" 76 die THEN
   IMPL-SPAN-N @ IMPL-SPAN-CAP >= IF s" implementation span overflow" 76 die THEN
   ndict@ IMPL-SPAN-FROM IMPL-SPAN-N @ cells + !
   -1 IMPL-OPEN ! ;

: ;IMPLEMENTATION ( -- )
   IMPL-OPEN @ 0 = IF s" implementation region not open" 76 die THEN
   ndict@ IMPL-SPAN-TO IMPL-SPAN-N @ cells + !
   1 IMPL-SPAN-N +!
   0 IMPL-OPEN ! ;

: API ( -- )   \ publish the just-defined record out of the open implementation region
   IMPL-OPEN @ 0 = IF s" api outside an implementation region" 76 die THEN
   IMPL-API-N @ IMPL-API-CAP >= IF s" implementation api overflow" 76 die THEN
   ndict@ 1 -  IMPL-API-IDX IMPL-API-N @ cells + !
   1 IMPL-API-N +! ;

\ Everything util.f defines below this line is internal to the prefix unless its
\ closing line is followed by API. CORE-FOLD-C is a case-fold helper the moved
\ type foundation calls, not a top-level name.
IMPLEMENTATION

: CORE-STR= {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) byte-wise string equality
   u v = IF
     -1 SEQ !
     0 BEGIN dup u < WHILE
       dup a + c@  over b + c@  <> IF 0 SEQ ! THEN
       1 + REPEAT drop
	   ELSE 0 SEQ ! THEN
	   SEQ @ 0 <> ;
API   \ CORE-STR=

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
API   \ CORE-STR=CI

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
256 constant PATH-CAP
7134 constant E-PATH-RANGE   \ path length negative, or longer than PATH-CAP holds with its NUL
: PATHZ {: a:ptr u d:ptr :} ( ptr u8 n ptr u8 -- )
   u 0 < IF E-PATH-RANGE throw THEN
   u 1 + PATH-CAP > IF E-PATH-RANGE throw THEN
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
API   \ PATHZ
create PZB PATH-CAP allot

: PATH0 {: a:ptr u :} ( ptr u8 n -- ptr u8 )
   a u PZB PATHZ  PZB ;     \ shared scratch
API   \ PATH0
\ read a little-endian u32 from byte addr p
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
API   \ RD32
;IMPLEMENTATION
