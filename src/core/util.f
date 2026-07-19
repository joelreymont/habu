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
64 constant REG-PROT-CAP
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
256 constant PATH-CAP
: PATHZ {: a:ptr u d:ptr :} ( ptr u8 n ptr u8 -- )
   u 1 + PATH-CAP > IF s" path too long" 76 die THEN
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
create PZB PATH-CAP allot

: PATH0 {: a:ptr u :} ( ptr u8 n -- ptr u8 )
   a u PZB PATHZ  PZB ;     \ shared scratch
\ read a little-endian u32 from byte addr p
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
