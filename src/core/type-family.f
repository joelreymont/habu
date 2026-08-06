\ type-family.f — package-scoped type-family (TFAM), sum-variant (SUMV),
\ product-field, and logical-layout registries for the checker (package TFAM).
\ See docs/type-families.md §6-7, §21. Records key on (package name, canonical
\ lowercase tail), so two packages may define the same tail without aliasing.
\ All names are interned as byte offsets into one growable string pool, so record
\ arrays hold only integers: a grow is a plain cell copy and snapshot persist
\ bakes stores verbatim with no rebase. Loaded unchecked in the checker prefix,
\ right after type-schema.f, mirroring the VREC value-record registry. Mutators
\ are package-private implementation words; the read-only query surface (find /
\ arity / kind predicates) is the only part meant to leave the package once
\ sealing (dot 2b) lands. The declaration boundary rejects uppercase/mixed-case
\ tails BEFORE storage, so the registry only ever stores already-canonical tails.

\ --- kind of a family (what its values are).
0 constant TK-CELL          \ scalar-cell family (no ADT layout)
1 constant TK-PRODUCT       \ record / struct
2 constant TK-SUM           \ tagged sum
3 constant TK-ENUM          \ payload-free sum
4 constant TK-EVIDENCE      \ compile-only evidence family
4 constant TK-MAX

\ --- layout policy (physical representation). Default is the universal
\ M-payload-cells + 1-tag-cell stack representation (docs §22.1).
0 constant TL-STACK-CELL-TAG
1 constant TL-PACKED-TAG
2 constant TL-NICHE
3 constant TL-BOXED
4 constant TL-CUSTOM
4 constant TL-MAX

\ --- parameter kind (kind of each of a family's `arity` parameters).
0 constant PK-CELL
1 constant PK-LAYOUT
2 constant PK-TYPE
3 constant PK-EVIDENCE
3 constant PK-MAX

CELL constant TAGW-CELL     \ default tag width: one stack cell

\ Visibility reuses the checker's package modes so registry and package scope
\ never diverge: CHECKER-PACKAGE-PRIVATE (1) / CHECKER-PACKAGE-PUBLIC (2).

\ --- named reject codes (thrown, caught by parser/CHECK path or unit `catch`).
7101 constant E-TFAM-CASE     \ uppercase/mixed-case or non-canonical tail token
7102 constant E-TFAM-DUP      \ duplicate tail within the same package
7106 constant E-TFAM-AMBIG    \ two other-package public families tie on a tail (7103 = E-SCHEMA-BAD)
7104 constant E-TFAM-ARITY    \ negative arity
7105 constant E-TFAM-KIND     \ unknown kind
7122 constant E-PF-ID         \ invalid or provisional (uncommitted) product-field id
7123 constant E-PF-TX         \ stale or non-LIFO field transaction, or reset while a field transaction is open
7124 constant E-PF-OWNER      \ invalid family / optional-variant ownership
7125 constant E-PF-NAME       \ reserved field tail
7126 constant E-PF-SCHEMA     \ malformed or owner-incompatible schema
7127 constant E-PF-LAYOUT     \ invalid field layout metadata / policy
7128 constant E-PF-FLAGS      \ undefined field flag bits
7132 constant E-TFAM-PAYLOAD  \ malformed or mixed unified payload metadata

variable TF-I                 \ private scan/copy index
variable TF-PUB               \ private first-public-match accumulator (-1 = none)

\ ---------------------------------------------------------------------------
\ declaration parameter spelling.  This is the one reversible alphabet used by
\ every declaration parser and signature generator.  The concrete scalar tokens
\ f, n, and r are deliberately absent, so their meaning never depends on arity.
\ The registry may store wider internal schemas; this count bounds only the
\ source-language spelling of positional declaration parameters.
\ ---------------------------------------------------------------------------
23 constant TFAM-DECL-PARAM-COUNT
create TFAM-DECL-PARAM-ALPHABET
   97 c,  98 c,  99 c,  100 c, 101 c,
   103 c, 104 c, 105 c, 106 c, 107 c,
   108 c, 109 c, 111 c, 112 c, 113 c,
   115 c, 116 c, 117 c, 118 c, 119 c,
   120 c, 121 c, 122 c,

: TFAM-DECL-PARAM>CHAR ( n -- n bool ) {: index:n :}
   index 0 < index TFAM-DECL-PARAM-COUNT >= or IF 0 RES-FALSE EXIT THEN
   TFAM-DECL-PARAM-ALPHABET index + c@ RES-TRUE ;

: TFAM-DECL-CHAR>PARAM ( n -- n bool ) {: char:n :}
   0 BEGIN dup TFAM-DECL-PARAM-COUNT < WHILE
      TFAM-DECL-PARAM-ALPHABET over + c@ char = IF RES-TRUE EXIT THEN
      1 +
   REPEAT
   drop 0 RES-FALSE ;

\ ---------------------------------------------------------------------------
\ shared string pool. Names are interned as byte offsets; offsets stay valid
\ across a pool grow, so no stored offset ever needs rebasing.
\ ---------------------------------------------------------------------------
32 constant TF-STR-INIT          \ small seed byte pool; grows (doubles) on demand
variable TF-STR-CAP-V   TF-STR-INIT TF-STR-CAP-V !   REG-PROTECT
: TF-STR-CAP ( -- n ) TF-STR-CAP-V @ ;
create TF-STR-BOOT   TF-STR-INIT allot   REG-PROTECT
variable TF-STR-P   TF-STR-BOOT TF-STR-P !   REG-PROTECT
: TF-STR ( -- ptr u8 ) TF-STR-P @ ;
variable TF-STR-U   0 TF-STR-U !   REG-PROTECT

: TF-STR-GROW ( n -- ) {: need:n :}
   need TF-STR-CAP-V @ 2 * max {: nc:n :}
   TF-STR-P  TF-STR-CAP-V @  nc  REG-GROW1
   nc TF-STR-CAP-V ! ;
: TF-STR-ENSURE ( n -- ) {: add:n :}      \ room for `add` more bytes
   TF-STR-U @ add + TF-STR-CAP-V @ <= IF exit THEN
   TF-STR-U @ add + TF-STR-GROW ;
: TF-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}   \ copy bytes into the pool -> offset
   u TF-STR-ENSURE                        \ grow first, then cache a stable base
   TF-STR-U @ {: off:n :}
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@   TF-STR off + TF-I @ + c!
      TF-I @ 1 + TF-I !
   REPEAT
   TF-STR-U @ u + TF-STR-U !
   off ;
: TF-OFF$ ( n n -- ptr u8 n ) {: off:n u:n :}   \ interned (offset,len) -> string
   TF-STR off + u ;

\ ---------------------------------------------------------------------------
\ canonical tail validation. Declarations accept only lowercase tokens; the
\ registry never folds case, so an uppercase/mixed-case token is rejected here.
\ ---------------------------------------------------------------------------
: TF-LOWER? ( n -- bool ) {: c:n :} c 97 >= c 122 <= and ;   \ a-z
: TF-DIGIT? ( n -- bool ) {: c:n :} c 48 >= c 57 <= and ;    \ 0-9
: TF-UPPER? ( n -- bool ) {: c:n :} c 65 >= c 90 <= and ;    \ A-Z
: TF-TAILBYTE? ( n -- bool ) {: c:n :}   \ lowercase, digit, or internal hyphen
   c TF-LOWER? IF RES-TRUE EXIT THEN
   c TF-DIGIT? IF RES-TRUE EXIT THEN
   c 45 = ;                                                  \ '-'
: TF-HIDDEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ compaction-hidden field name
   u 0= IF RES-FALSE EXIT THEN
   a c@ 64 = ;                                              \ leading '@'
: TF-HYPHEN-BAD? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}   \ '-' at an edge or doubled
   i 0 = IF RES-TRUE EXIT THEN                      \ leading '-a'
   i u 1 - = IF RES-TRUE EXIT THEN                  \ trailing 'a-'
   a i + 1 - c@ 45 = ;                              \ previous byte also '-' -> 'a--b'
: TF-CANON? ( ptr u8 n -- bool ) {: a:ptr u:n :}    \ tailbytes + internal single '-' + >=1 letter
   u 0= IF RES-FALSE EXIT THEN
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@ TF-TAILBYTE? 0= IF RES-FALSE EXIT THEN
      a TF-I @ + c@ 45 = IF
         a u TF-I @ TF-HYPHEN-BAD? IF RES-FALSE EXIT THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 TF-I !
   BEGIN TF-I @ u < WHILE                            \ require at least one letter
      a TF-I @ + c@ TF-LOWER? IF RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   RES-FALSE ;
: TF-REQUIRE-CANON ( ptr u8 n -- )   \ reject uppercase/mixed-case at declaration
   TF-CANON? 0= IF E-TFAM-CASE throw THEN ;

\ ---------------------------------------------------------------------------
\ param-kind pool (one PK-* cell per parameter of every family, contiguous).
\ ---------------------------------------------------------------------------
4 constant TF-PK-INIT            \ small seed param-kind pool; grows on demand
variable TF-PK-CAP-V   TF-PK-INIT TF-PK-CAP-V !   REG-PROTECT
: TF-PK-CAP ( -- n ) TF-PK-CAP-V @ ;
create TF-PK-BOOT   TF-PK-INIT cells allot   REG-PROTECT
variable TF-PK-P   TF-PK-BOOT TF-PK-P !   REG-PROTECT
: TF-PK-BASE ( -- ptr a ) TF-PK-P @ ;
variable TF-PK-N   0 TF-PK-N !   REG-PROTECT

: TF-PK-GROW ( n -- ) {: need:n :}
   need TF-PK-CAP-V @ 2 * max {: nc:n :}
   TF-PK-P  TF-PK-CAP-V @ cells  nc cells  REG-GROW1
   nc TF-PK-CAP-V ! ;
: TF-PK-ENSURE ( -- )
   TF-PK-N @ TF-PK-CAP-V @ < IF exit THEN
   TF-PK-N @ 1 + TF-PK-GROW ;
: TF-PK+ ( n -- ) {: k:n :}             \ append one param-kind slot
   TF-PK-ENSURE
   k TF-PK-N @ cells TF-PK-BASE + !
   TF-PK-N @ 1 + TF-PK-N ! ;

\ ---------------------------------------------------------------------------
\ TFAM record arena.
\ ---------------------------------------------------------------------------
\ Bit i in a PTR-MASK marks slot i as a relocating pointer. Every registry
\ record below contains only scalar ids, counts, codes, or interned offsets.
: TF-LAYOUT= ( n n -- )
   <> if s" type-family: layout drift" CORE-LAYOUT-RC die then ;

0 cells constant TF.PKG-OFF-AT
1 cells constant TF.PKG-U-OFF
2 cells constant TF.VIS-OFF
3 cells constant TF.NAME-OFF-AT
4 cells constant TF.NAME-U-OFF
5 cells constant TF.ARITY-OFF
6 cells constant TF.KIND-OFF
7 cells constant TF.PK-START-OFF
8 cells constant TF.LAYOUT-OFF
9 cells constant TF.SLOTS-OFF
10 cells constant TF.VAR-START-OFF
11 cells constant TF.VAR-COUNT-OFF
12 cells constant TF.FLD-START-OFF
13 cells constant TF.FLD-COUNT-OFF
14 cells constant TF.TAGW-OFF
15 cells constant TF.SCHEMA-ROOT-OFF
16 cells constant TF.SPAN-OFF-AT
17 cells constant TF.SPAN-U-OFF
18 cells constant TF.DERIVE-OFF
\ TF.TAILNEXT: next family id (+1) in this row's tail bucket, 0 at the end of the
\ chain. It is the registry's own lookup index (TFX-* below), which is why it
\ lives in the record rather than in a side table that could drift from it.
19 cells constant TF.TAILNEXT-OFF
20 cells constant TF-REC
CELL constant TF-REC-ALIGN
0 constant TF-REC-PTR-MASK

: TF.PKG-OFF ( ptr a -- ptr a ) TF.PKG-OFF-AT + ;
: TF.PKG-U ( ptr a -- ptr a ) TF.PKG-U-OFF + ;
: TF.VIS ( ptr a -- ptr a ) TF.VIS-OFF + ;
: TF.NAME-OFF ( ptr a -- ptr a ) TF.NAME-OFF-AT + ;
: TF.NAME-U ( ptr a -- ptr a ) TF.NAME-U-OFF + ;
: TF.ARITY ( ptr a -- ptr a ) TF.ARITY-OFF + ;
: TF.KIND ( ptr a -- ptr a ) TF.KIND-OFF + ;
: TF.PK-START ( ptr a -- ptr a ) TF.PK-START-OFF + ;
: TF.LAYOUT ( ptr a -- ptr a ) TF.LAYOUT-OFF + ;
: TF.SLOTS ( ptr a -- ptr a ) TF.SLOTS-OFF + ;
: TF.VAR-START ( ptr a -- ptr a ) TF.VAR-START-OFF + ;
: TF.VAR-COUNT ( ptr a -- ptr a ) TF.VAR-COUNT-OFF + ;
: TF.FLD-START ( ptr a -- ptr a ) TF.FLD-START-OFF + ;
: TF.FLD-COUNT ( ptr a -- ptr a ) TF.FLD-COUNT-OFF + ;
: TF.TAGW ( ptr a -- ptr a ) TF.TAGW-OFF + ;
: TF.SCHEMA-ROOT ( ptr a -- ptr a ) TF.SCHEMA-ROOT-OFF + ;
: TF.SPAN-OFF ( ptr a -- ptr a ) TF.SPAN-OFF-AT + ;
: TF.SPAN-U ( ptr a -- ptr a ) TF.SPAN-U-OFF + ;
: TF.DERIVE ( ptr a -- ptr a ) TF.DERIVE-OFF + ;
: TF.TAILNEXT ( ptr a -- ptr a ) TF.TAILNEXT-OFF + ;

TF.PKG-OFF-AT 0 cells TF-LAYOUT=
TF.PKG-U-OFF 1 cells TF-LAYOUT=
TF.VIS-OFF 2 cells TF-LAYOUT=
TF.NAME-OFF-AT 3 cells TF-LAYOUT=
TF.NAME-U-OFF 4 cells TF-LAYOUT=
TF.ARITY-OFF 5 cells TF-LAYOUT=
TF.KIND-OFF 6 cells TF-LAYOUT=
TF.PK-START-OFF 7 cells TF-LAYOUT=
TF.LAYOUT-OFF 8 cells TF-LAYOUT=
TF.SLOTS-OFF 9 cells TF-LAYOUT=
TF.VAR-START-OFF 10 cells TF-LAYOUT=
TF.VAR-COUNT-OFF 11 cells TF-LAYOUT=
TF.FLD-START-OFF 12 cells TF-LAYOUT=
TF.FLD-COUNT-OFF 13 cells TF-LAYOUT=
TF.TAGW-OFF 14 cells TF-LAYOUT=
TF.SCHEMA-ROOT-OFF 15 cells TF-LAYOUT=
TF.SPAN-OFF-AT 16 cells TF-LAYOUT=
TF.SPAN-U-OFF 17 cells TF-LAYOUT=
TF.DERIVE-OFF 18 cells TF-LAYOUT=
TF.TAILNEXT-OFF 19 cells TF-LAYOUT=
TF-REC 20 cells TF-LAYOUT=
TF-REC-ALIGN CELL TF-LAYOUT=
TF-REC TF-REC-ALIGN mod 0 TF-LAYOUT=
TF-REC-PTR-MASK 0 TF-LAYOUT=
0 TF.PKG-OFF TF.PKG-OFF-AT TF-LAYOUT=
0 TF.PKG-U TF.PKG-U-OFF TF-LAYOUT=
0 TF.VIS TF.VIS-OFF TF-LAYOUT=
0 TF.NAME-OFF TF.NAME-OFF-AT TF-LAYOUT=
0 TF.NAME-U TF.NAME-U-OFF TF-LAYOUT=
0 TF.ARITY TF.ARITY-OFF TF-LAYOUT=
0 TF.KIND TF.KIND-OFF TF-LAYOUT=
0 TF.PK-START TF.PK-START-OFF TF-LAYOUT=
0 TF.LAYOUT TF.LAYOUT-OFF TF-LAYOUT=
0 TF.SLOTS TF.SLOTS-OFF TF-LAYOUT=
0 TF.VAR-START TF.VAR-START-OFF TF-LAYOUT=
0 TF.VAR-COUNT TF.VAR-COUNT-OFF TF-LAYOUT=
0 TF.FLD-START TF.FLD-START-OFF TF-LAYOUT=
0 TF.FLD-COUNT TF.FLD-COUNT-OFF TF-LAYOUT=
0 TF.TAGW TF.TAGW-OFF TF-LAYOUT=
0 TF.SCHEMA-ROOT TF.SCHEMA-ROOT-OFF TF-LAYOUT=
0 TF.SPAN-OFF TF.SPAN-OFF-AT TF-LAYOUT=
0 TF.SPAN-U TF.SPAN-U-OFF TF-LAYOUT=
0 TF.DERIVE TF.DERIVE-OFF TF-LAYOUT=
0 TF.TAILNEXT TF.TAILNEXT-OFF TF-LAYOUT=

4 constant TF-CAP-INIT
variable TF-CAP-V   TF-CAP-INIT TF-CAP-V !   REG-PROTECT
: TF-CAP ( -- n ) TF-CAP-V @ ;
create TF-A-BOOT   TF-CAP-INIT TF-REC * allot   REG-PROTECT
variable TF-A-P   TF-A-BOOT TF-A-P !   REG-PROTECT
: TF-BASE ( -- ptr a ) TF-A-P @ ;
variable TFAM-N   0 TFAM-N !   REG-PROTECT

: TF-GROW ( n -- ) {: need:n :}
   need TF-CAP-V @ 2 * max {: nc:n :}
   TF-A-P  TF-CAP-V @ TF-REC *  nc TF-REC *  REG-GROW1
   nc TF-CAP-V ! ;
: TF-ENSURE ( -- )
   TFAM-N @ TF-CAP-V @ < IF exit THEN
   TFAM-N @ 1 + TF-GROW ;
: TF-REC@ ( n -- ptr a ) {: id:n :}      \ address of family record `id`
   id 0 < IF s" tfam: bad family id" 76 die THEN
   id TFAM-N @ >= IF s" tfam: bad family id" 76 die THEN
   id TF-REC * TF-BASE + ;

: TFAM-N@ ( -- n ) TFAM-N @ ;            \ family high-water (rollback/tests)
: TF-STR-U@ ( -- n ) TF-STR-U @ ;        \ interned string-pool high-water
: TF-PK-N@ ( -- n ) TF-PK-N @ ;          \ param-kind pool high-water

\ --- read-only queries.
: TFAM-PKG$ ( n -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.PKG-OFF @ r TF.PKG-U @ TF-OFF$ ;
: TFAM-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.NAME-OFF @ r TF.NAME-U @ TF-OFF$ ;
: TFAM-VIS@ ( n -- n ) TF-REC@ TF.VIS @ ;
: TFAM-ARITY@ ( n -- n ) TF-REC@ TF.ARITY @ ;
: TFAM-KIND@ ( n -- n ) TF-REC@ TF.KIND @ ;
: TFAM-LAYOUT-POLICY@ ( n -- n ) TF-REC@ TF.LAYOUT @ ;
: TFAM-SLOTS@ ( n -- n ) TF-REC@ TF.SLOTS @ ;
: TFAM-VAR-START@ ( n -- n ) TF-REC@ TF.VAR-START @ ;
: TFAM-VAR-COUNT@ ( n -- n ) TF-REC@ TF.VAR-COUNT @ ;
: TFAM-FLD-START@ ( n -- n ) TF-REC@ TF.FLD-START @ ;
: TFAM-FLD-COUNT@ ( n -- n ) TF-REC@ TF.FLD-COUNT @ ;
: TFAM-TAGW@ ( n -- n ) TF-REC@ TF.TAGW @ ;
: TFAM-SCHEMA-ROOT@ ( n -- n ) TF-REC@ TF.SCHEMA-ROOT @ ;
: TFAM-SPAN@ ( n -- n n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.SPAN-OFF @ r TF.SPAN-U @ ;
: TFAM-PK@ ( n n -- n ) {: id:n i:n :}
   i 0 < i id TFAM-ARITY@ >= or IF s" tfam: bad param index" 76 die THEN
   id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + @ ;

: TFAM-PUBLIC? ( n -- bool ) TFAM-VIS@ CHECKER-PACKAGE-PUBLIC = ;
: TFAM-CELL? ( n -- bool ) TFAM-KIND@ TK-CELL = ;
: TFAM-PRODUCT? ( n -- bool ) TFAM-KIND@ TK-PRODUCT = ;
: TFAM-SUM? ( n -- bool ) TFAM-KIND@ TK-SUM = ;
: TFAM-ENUM? ( n -- bool ) TFAM-KIND@ TK-ENUM = ;
: TFAM-LAYOUT? ( n -- bool ) {: id:n :}   \ true when the family occupies an ADT layout
   id TFAM-PRODUCT? id TFAM-SUM? or id TFAM-ENUM? or ;

\ --- opt-in derived words (derive S1, dot habu-checker-capability-derive):
\ a `DERIVE eq` clause marks the family row; the sumtype generator then emits
\ the family's derived words and the ctor-word predicate below recognizes
\ their fixed generator-owned tails, exactly like generated constructors.
1 constant DRV-EQ
2 constant DRV-HASH
: TFAM-DERIVE@ ( n -- n ) TF-REC@ TF.DERIVE @ ;
: TFAM-DERIVE-EQ! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-EQ or swap ! ;
: TFAM-DERIVE-EQ? ( n -- bool ) TFAM-DERIVE@ DRV-EQ and 0 <> ;
: TFAM-DERIVE-HASH! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-HASH or swap ! ;
: TFAM-DERIVE-HASH? ( n -- bool ) TFAM-DERIVE@ DRV-HASH and 0 <> ;
: TFAM-DERIVE-ANY? ( n -- bool ) TFAM-DERIVE@ 0 <> ;

\ a boxed value is a single heap/DATA pointer (docs §22.4 `ptr fam-box`) and a
\ niche value is a single cell with the discriminant folded into the payload
\ (docs §22.3) — both collapse the stack width to one cell regardless of kind,
\ so the width branch below keys on the policy before the kind. Reached today
\ ONLY through the direct TFAM-LAYOUT! mutator (no declaration accepts boxed /
\ niche-null yet — both reject at the POLICY clause), so this is check-sound
\ metadata the boxed/niche accept slices consume; it never changes the width of
\ a stack-cell-tag or packed family (packed keeps the cell width, docs §22.2).
: TFAM-BOXED-OR-NICHE? ( n -- bool ) {: id:n :}   \ policy collapses the value to one cell
   id TFAM-LAYOUT-POLICY@ {: p:n :}
   p TL-BOXED = p TL-NICHE = or ;

\ logical width in stack cells (docs/type-families.md §18 WIDTH function):
\ boxed/niche = one cell; sum = max payload slots + one tag cell; enum = tag only
\ (slots 0); product = field cells, no tag; cell/evidence families are one cell.
: TFAM-WIDTH@ ( n -- n ) {: id:n :}
   id TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   id TFAM-SUM? id TFAM-ENUM? or IF id TFAM-SLOTS@ 1 + EXIT THEN
   id TFAM-PRODUCT? IF id TFAM-SLOTS@ EXIT THEN
   1 ;

\ --- friend-only field mutators (populated by later declaration passes / tests).
: TFAM-LAYOUT! ( n n -- ) {: id:n p:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   p id TF-REC@ TF.LAYOUT ! ;
: TFAM-SLOTS! ( n n -- ) swap TF-REC@ TF.SLOTS ! ;
: TFAM-VAR-RANGE! ( n n n -- ) {: id:n s:n c:n :}
   s id TF-REC@ TF.VAR-START !  c id TF-REC@ TF.VAR-COUNT ! ;
: TFAM-FLD-RANGE! ( n n n -- ) {: id:n s:n c:n :}
   s id TF-REC@ TF.FLD-START !  c id TF-REC@ TF.FLD-COUNT ! ;
: TFAM-TAGW! ( n n -- ) swap TF-REC@ TF.TAGW ! ;
: TFAM-SCHEMA-ROOT! ( n n -- ) swap TF-REC@ TF.SCHEMA-ROOT ! ;
: TFAM-SPAN! ( n n n -- ) {: id:n off:n u:n :}
   off id TF-REC@ TF.SPAN-OFF !  u id TF-REC@ TF.SPAN-U ! ;
: TFAM-PK! ( n n n -- ) {: id:n i:n k:n :}
   i 0 < i id TFAM-ARITY@ >= or IF s" tfam: bad param index" 76 die THEN
   k 0 < k PK-MAX > or IF E-TFAM-KIND throw THEN
   k id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + ! ;

\ --- matching and lookup.
: TFAM-PKG-MATCH? ( ptr u8 n n -- bool ) {: pa:ptr pu:n id:n :}
   id TFAM-PKG$ pa pu CORE-STR=CI ;
: TFAM-NAME-MATCH? ( ptr u8 n n -- bool ) {: na:ptr nu:n id:n :}
   id TFAM-NAME$ na nu CORE-STR= ;

\ --- tail index (TFX) ----------------------------------------------------------
\ Both registry lookups below ask a question about ONE tail: TFAM-FIND-IN wants
\ the row with that tail in a given package, TFAM-FIND-PUBLIC wants the public
\ rows with that tail across packages. Every other row in the registry is
\ irrelevant to both, so the walk over [0, TFAM-N) is replaced by a walk of the
\ rows that share the tail's hash bucket — a handful, and independent of how many
\ families the program declared.
\
\ Rows are pushed on their bucket in id order and retired newest-first, so a
\ retired row is always at its bucket head and TFAM-N rewinding IS the chain's
\ retirement — the property the rollback comment further down already relies on.
\ The bucket count follows the record arena's own capacity; a capacity change or
\ a rewind performed outside the retire seam rebuilds the whole index.

64 constant TFX-SLOTS-INIT              \ power of two; grown to keep load <= 1/2
variable TFX-SLOTS-V   TFX-SLOTS-INIT TFX-SLOTS-V !   REG-PROTECT
: TFX-SLOTS ( -- n ) TFX-SLOTS-V @ ;
create TFX-A-BOOT   TFX-SLOTS-INIT cells allot   REG-PROTECT
variable TFX-A-P   TFX-A-BOOT TFX-A-P !   REG-PROTECT
: TFX-BASE ( -- ptr a ) TFX-A-P @ ;
variable TFX-READY   0 TFX-READY !   REG-PROTECT
variable TFX-HI      0 TFX-HI !       REG-PROTECT
variable TFX-CAP     0 TFX-CAP !      REG-PROTECT   \ the TF-CAP the buckets were sized for
variable TFX-H                \ private hash accumulator
variable TFX-I                \ private build/retire index
variable TFX-CUR              \ private bucket-walk cursor

: TFX-BKT ( n -- ptr a ) {: slot:n :}
   slot cells TFX-BASE + ;

: TFX-H+ ( n -- )
   TFX-H @ xor HIDX-FNV-PRIME * TFX-H ! ;

\ Hashes the exact bytes, because TFAM-NAME-MATCH? compares the exact bytes:
\ registry tails are canonical lowercase, so a non-canonical token misses here
\ for the same reason it misses the comparison.
: TFX-HASH ( ptr u8 n -- n ) {: na:ptr nu:n :}
   HIDX-FNV-BASIS TFX-H !
   0 BEGIN dup nu < WHILE
      dup na + c@ TFX-H+
      1 +
   REPEAT drop
   TFX-H @ TFX-SLOTS 1 - and ;

: TFX-ROW-BKT ( n -- ptr a ) {: id:n :}
   id TFAM-NAME$ TFX-HASH TFX-BKT ;

: TFX-PUSH ( n -- ) {: id:n :}
   id TFX-ROW-BKT {: b:ptr :}
   b @ id TF-REC@ TF.TAILNEXT !
   id 1 + b ! ;

: TFX-POP ( n -- ) {: id:n :}
   id TFX-ROW-BKT {: b:ptr :}
   b @ id 1 + <> IF s" tfam: tail index corrupt" 76 die THEN
   id TF-REC@ TF.TAILNEXT @ b ! ;

: TFX-BKTS-CLEAR ( -- )
   0 BEGIN dup TFX-SLOTS < WHILE
      0 over TFX-BKT !
      1 +
   REPEAT drop ;

\ Keep the load factor at or below one half, so a bucket walk stays a handful of
\ rows however many families the program declares.
: TFX-SLOTS-NEED ( -- n )
   TFX-SLOTS-INIT
   BEGIN dup TF-CAP 2 * < WHILE 2 * REPEAT ;

: TFX-RESIZE ( -- )
   TF-CAP TFX-CAP !
   TFX-SLOTS-NEED {: need:n :}
   need TFX-SLOTS = IF EXIT THEN
   TFX-A-P  TFX-SLOTS cells  need cells  REG-GROW1
   need TFX-SLOTS-V ! ;

: TFX-BUILD ( -- )
   TFX-RESIZE
   TFX-BKTS-CLEAR
   0 TFX-I !
   BEGIN TFX-I @ TFAM-N @ < WHILE
      TFX-I @ TFX-PUSH
      TFX-I @ 1 + TFX-I !
   REPEAT
   TFAM-N @ TFX-HI !
   -1 TFX-READY ! ;

\ On every registry lookup, so the question is three cell reads: has the index
\ been built, has the store rewound under it, and has the record arena been
\ resized since the buckets were sized from it.
: TFX-ENSURE ( -- )
   TFX-READY @ 0=
   TFAM-N @ TFX-HI @ < or
   TFX-CAP @ TF-CAP <> or IF TFX-BUILD THEN ;

\ TFX-RETIRE ( n -- ) : pop rows [newn, TFAM-N) before TFAM-N rewinds to newn.
\ Newest first, so each popped row is at its bucket head.
: TFX-RETIRE ( n -- ) {: newn:n :}
   TFX-READY @ 0= IF EXIT THEN
   TFAM-N @ 1 -
   BEGIN dup newn >= WHILE
      dup TFX-POP
      1 -
   REPEAT drop
   newn TFX-HI ! ;

\ The row is already inside [0, TFAM-N) by the time its fields are written, so
\ the caller runs TFX-ENSURE BEFORE committing the slot — a rebuild here would
\ chain the new row once and this push would chain it a second time.
: TFX-ADD ( n -- ) {: id:n :}
   id TFX-PUSH
   TFAM-N @ TFX-HI ! ;

\ exact (package,tail) — the qualified-lookup and duplicate-detection primitive.
: TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   TFX-ENSURE
   na nu TFX-HASH TFX-BKT @ TFX-CUR !
   BEGIN TFX-CUR @ 0 <> WHILE
      TFX-CUR @ 1 - {: id:n :}
      pa pu id TFAM-PKG-MATCH? IF
         na nu id TFAM-NAME-MATCH? IF id RES-TRUE EXIT THEN
      THEN
      id TF-REC@ TF.TAILNEXT @ TFX-CUR !
   REPEAT
   0 RES-FALSE ;

\ TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool ) : the SPECIFICATION of what
\ the index answers — the lowest row matching (package, tail), by walking the
\ registry. test/checker-scan-index-suite.f differentials the two.
: TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ TFAM-N @ < WHILE
      pa pu TF-I @ TFAM-PKG-MATCH? IF
         na nu TF-I @ TFAM-NAME-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;

\ PUBLIC package family with this tail. Global rows are lexical and are resolved
\ separately by TFAM-RESOLVE, so they never enter this fallback set. A
\ (package,tail) pair is unique (TFAM-DECL rejects DUP), so two matches are always
\ different packages: that is a genuine unqualified ambiguity and throws
\ E-TFAM-AMBIG rather than silently picking the lowest id. Exactly one package
\ public match resolves; none is false.
: TFAM-FIND-PUBLIC ( ptr u8 n -- n bool ) {: na:ptr nu:n :}
   -1 TF-PUB !
   TFX-ENSURE
   na nu TFX-HASH TFX-BKT @ TFX-CUR !
   BEGIN TFX-CUR @ 0 <> WHILE
      TFX-CUR @ 1 - {: id:n :}
      id TFAM-PUBLIC? IF
         id TFAM-PKG$ nip 0 <> IF
            na nu id TFAM-NAME-MATCH? IF
               TF-PUB @ 0< IF id TF-PUB !
               ELSE E-TFAM-AMBIG throw THEN
            THEN
         THEN
      THEN
      id TF-REC@ TF.TAILNEXT @ TFX-CUR !
   REPEAT
   TF-PUB @ 0< IF 0 RES-FALSE ELSE TF-PUB @ RES-TRUE THEN ;

\ Unqualified lexical order is the active package's exact row (private or
\ public), the exact global row, then the unique package-public fallback
\ (E-TFAM-AMBIG if two non-lexical packages tie). At top level the first exact
\ lookup is already global. Compaction-hidden `@name` tokens never resolve.
: TFAM-RESOLVE ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   pa pu na nu TFAM-FIND-IN IF RES-TRUE EXIT THEN
   drop
   pu 0 <> IF
      s" " na nu TFAM-FIND-IN IF RES-TRUE EXIT THEN
      drop
   THEN
   na nu TFAM-FIND-PUBLIC ;

\ --- declaration. Storage only ever sees canonical lowercase tails.
: TFAM-KIND-VALID? ( n -- bool ) {: k:n :} k 0 >= k TK-MAX <= and ;
: TFAM-PK-RESERVE ( n -- ) {: k:n :}        \ default every parameter to PK-CELL
   0 TF-I !
   BEGIN TF-I @ k < WHILE
      PK-CELL TF-PK+
      TF-I @ 1 + TF-I !
   REPEAT ;
: TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n )
   {: pa:ptr pu:n vis:n na:ptr nu:n arity:n kind:n :}
   na nu TF-REQUIRE-CANON
   arity 0 < IF E-TFAM-ARITY throw THEN
   kind TFAM-KIND-VALID? 0= IF E-TFAM-KIND throw THEN
   pa pu na nu TFAM-FIND-IN IF drop E-TFAM-DUP throw THEN drop   \ FIND returns (id-or-0 flag); drop the id
   TF-ENSURE
   TFX-ENSURE                             \ before the slot is committed; see TFX-ADD
   TFAM-N @ {: id:n :}
   pa pu TF-INTERN {: poff:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + TFAM-N !                        \ commit the slot before writing it (VREC style)
   id TF-REC@ {: r:ptr :}
   poff r TF.PKG-OFF !   pu r TF.PKG-U !
   vis r TF.VIS !
   noff r TF.NAME-OFF !  nu r TF.NAME-U !
   arity r TF.ARITY !    kind r TF.KIND !
   TF-PK-N @ r TF.PK-START !
   TL-STACK-CELL-TAG r TF.LAYOUT !
   0 r TF.SLOTS !
   0 r TF.VAR-START !          0 r TF.VAR-COUNT !
   0 r TF.FLD-START !          0 r TF.FLD-COUNT !
   TAGW-CELL r TF.TAGW !
   0 r TF.SCHEMA-ROOT !
   0 r TF.SPAN-OFF !   0 r TF.SPAN-U !
   0 r TF.DERIVE !
   0 r TF.TAILNEXT !
   id TFX-ADD                             \ the row's tail is written: it can be found now
   arity TFAM-PK-RESERVE
   id ;

\ ---------------------------------------------------------------------------
\ SUMV: sum/enum variant records, keyed by (family-id, variant tail).
\ ---------------------------------------------------------------------------
0 cells constant SV.FAM-OFF
1 cells constant SV.NAME-OFF-AT
2 cells constant SV.NAME-U-OFF
3 cells constant SV.TAG-OFF
4 cells constant SV.SCH-START-OFF
5 cells constant SV.SCH-COUNT-OFF
6 cells constant SV.PAYCELLS-OFF
7 cells constant SV.CTOR-SYM-OFF
8 cells constant SV.CTOR-PKG-OFF-AT
9 cells constant SV.CTOR-PKG-U-OFF
10 cells constant SUMV-REC
CELL constant SUMV-REC-ALIGN
0 constant SUMV-REC-PTR-MASK

: SV.FAM ( ptr a -- ptr a ) SV.FAM-OFF + ;
: SV.NAME-OFF ( ptr a -- ptr a ) SV.NAME-OFF-AT + ;
: SV.NAME-U ( ptr a -- ptr a ) SV.NAME-U-OFF + ;
: SV.TAG ( ptr a -- ptr a ) SV.TAG-OFF + ;
: SV.SCH-START ( ptr a -- ptr a ) SV.SCH-START-OFF + ;
: SV.SCH-COUNT ( ptr a -- ptr a ) SV.SCH-COUNT-OFF + ;
: SV.PAYCELLS ( ptr a -- ptr a ) SV.PAYCELLS-OFF + ;
: SV.CTOR-SYM ( ptr a -- ptr a ) SV.CTOR-SYM-OFF + ;
: SV.CTOR-PKG-OFF ( ptr a -- ptr a ) SV.CTOR-PKG-OFF-AT + ;
: SV.CTOR-PKG-U ( ptr a -- ptr a ) SV.CTOR-PKG-U-OFF + ;

SV.FAM-OFF 0 cells TF-LAYOUT=
SV.NAME-OFF-AT 1 cells TF-LAYOUT=
SV.NAME-U-OFF 2 cells TF-LAYOUT=
SV.TAG-OFF 3 cells TF-LAYOUT=
SV.SCH-START-OFF 4 cells TF-LAYOUT=
SV.SCH-COUNT-OFF 5 cells TF-LAYOUT=
SV.PAYCELLS-OFF 6 cells TF-LAYOUT=
SV.CTOR-SYM-OFF 7 cells TF-LAYOUT=
SV.CTOR-PKG-OFF-AT 8 cells TF-LAYOUT=
SV.CTOR-PKG-U-OFF 9 cells TF-LAYOUT=
SUMV-REC 10 cells TF-LAYOUT=
SUMV-REC-ALIGN CELL TF-LAYOUT=
SUMV-REC SUMV-REC-ALIGN mod 0 TF-LAYOUT=
SUMV-REC-PTR-MASK 0 TF-LAYOUT=
0 SV.FAM SV.FAM-OFF TF-LAYOUT=
0 SV.NAME-OFF SV.NAME-OFF-AT TF-LAYOUT=
0 SV.NAME-U SV.NAME-U-OFF TF-LAYOUT=
0 SV.TAG SV.TAG-OFF TF-LAYOUT=
0 SV.SCH-START SV.SCH-START-OFF TF-LAYOUT=
0 SV.SCH-COUNT SV.SCH-COUNT-OFF TF-LAYOUT=
0 SV.PAYCELLS SV.PAYCELLS-OFF TF-LAYOUT=
0 SV.CTOR-SYM SV.CTOR-SYM-OFF TF-LAYOUT=
0 SV.CTOR-PKG-OFF SV.CTOR-PKG-OFF-AT TF-LAYOUT=
0 SV.CTOR-PKG-U SV.CTOR-PKG-U-OFF TF-LAYOUT=

4 constant SUMV-CAP-INIT
variable SUMV-CAP-V   SUMV-CAP-INIT SUMV-CAP-V !   REG-PROTECT
: SUMV-CAP ( -- n ) SUMV-CAP-V @ ;
create SUMV-A-BOOT   SUMV-CAP-INIT SUMV-REC * allot   REG-PROTECT
variable SUMV-A-P   SUMV-A-BOOT SUMV-A-P !   REG-PROTECT
: SUMV-BASE ( -- ptr a ) SUMV-A-P @ ;
variable SUMV-N   0 SUMV-N !   REG-PROTECT

: SUMV-GROW ( n -- ) {: need:n :}
   need SUMV-CAP-V @ 2 * max {: nc:n :}
   SUMV-A-P  SUMV-CAP-V @ SUMV-REC *  nc SUMV-REC *  REG-GROW1
   nc SUMV-CAP-V ! ;
: SUMV-ENSURE ( -- )
   SUMV-N @ SUMV-CAP-V @ < IF exit THEN
   SUMV-N @ 1 + SUMV-GROW ;
: SUMV-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < IF s" tfam: bad variant id" 76 die THEN
   id SUMV-N @ >= IF s" tfam: bad variant id" 76 die THEN
   id SUMV-REC * SUMV-BASE + ;

: SUMV-FAM@ ( n -- n ) SUMV-REC@ SV.FAM @ ;
: SUMV-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id SUMV-REC@ {: r:ptr :}  r SV.NAME-OFF @ r SV.NAME-U @ TF-OFF$ ;
: SUMV-TAG@ ( n -- n ) SUMV-REC@ SV.TAG @ ;
: SUMV-SCH-START@ ( n -- n ) SUMV-REC@ SV.SCH-START @ ;
: SUMV-SCH-COUNT@ ( n -- n ) SUMV-REC@ SV.SCH-COUNT @ ;
: SUMV-RAW-PAYCELLS@ ( n -- n ) SUMV-REC@ SV.PAYCELLS @ ;
: SUMV-N@ ( -- n ) SUMV-N @ ;

\ generated-constructor metadata (item 8). A PUBLIC sum/enum family stores its
\ derived constructor package name (interned offset+len) in every variant; the
\ checker symbol for the generated constructor word lands in SV.CTOR-SYM. Private
\ families export nothing, so both stay zero. All three cells are integers /
\ interned offsets, so the existing SUMV snapshot bake persists them verbatim.
: SUMV-CTOR-PKG! ( n n n -- ) {: id:n off:n u:n :}
   off id SUMV-REC@ SV.CTOR-PKG-OFF !   u id SUMV-REC@ SV.CTOR-PKG-U ! ;
: SUMV-CTOR-PKG$ ( n -- ptr u8 n ) {: id:n :}
   id SUMV-REC@ {: r:ptr :}  r SV.CTOR-PKG-OFF @ r SV.CTOR-PKG-U @ TF-OFF$ ;
: SUMV-CTOR-SYM@ ( n -- n ) SUMV-REC@ SV.CTOR-SYM @ ;

\ --- constructor-symbol index (SVX) --------------------------------------------
\ SUMV-FROM-CTOR-SYM runs on every resolved call token and answers "no" for
\ almost all of them, so it must not cost a walk of the variant store. The
\ answer it wants is the LOWEST variant id carrying that constructor symbol, and
\ a variant's constructor symbol is written exactly once, so a per-symbol head
\ that only ever takes its FIRST writer reproduces it exactly.
\
\ Retirement is the store's own: variant rows only leave by SUMV-N rewinding, and
\ the lowest id for a symbol is the first to go, so a head pointing at a retired
\ row means the symbol has no row left. SVX-TRUNCATE therefore needs no
\ back-link — it clears the heads the retired rows own and nothing else.
\ SVX-HI catches a rewind performed outside those seams; SVX-GEN catches the
\ shared mapping being dropped or re-laid-out under it.
variable SVX-GEN   variable SVX-HI
0 SVX-GEN !   0 SVX-HI !

: SVX@ ( n -- n ) HT-SVX IDX-HEAD@ ;
: SVX! ( n n -- ) HT-SVX IDX-HEAD! ;

: SVX-STAMP ( -- ) SUMV-N @ SVX-HI ! ;

: SVX-SYNC ( -- )
   SUMV-N @ SVX-HI @ < IF 0 SVX-GEN ! THEN ;

variable SVX-I

\ First writer wins, matching SUMV-FROM-CTOR-SYM's lowest-id answer. Under the
\ store's one-constructor-per-variant rule (SUMV-CTOR-SYM! refuses a second
\ write) a symbol can key at most one row, so no test can tell first from last;
\ the condition reproduces the specification without leaning on that rule.
: SVX-LINK ( n n -- ) {: vid:n sym:n :}
   sym 0= IF EXIT THEN
   sym IDX-SYM-OK
   sym SVX@ 0 <> IF EXIT THEN
   vid 1 + sym SVX! ;

: SVX-BUILD ( -- )
   HT-SVX IDX-HEADS-CLEAR
   0 SVX-I !
   BEGIN SVX-I @ SUMV-N @ < WHILE
      SVX-I @ dup SUMV-CTOR-SYM@ SVX-LINK
      SVX-I @ 1 + SVX-I !
   REPEAT
   SVX-STAMP
   HIDX-GEN @ SVX-GEN ! ;

: SVX-ENSURE ( -- )
   HIDX-ENSURE
   SVX-SYNC
   SVX-GEN @ HIDX-GEN @ <> IF SVX-BUILD THEN ;

\ SVX-TRUNCATE ( n -- ) : called before SUMV-N rewinds to `newn`.
: SVX-TRUNCATE ( n -- ) {: newn:n :}
   SVX-GEN @ HIDX-GEN @ <> IF EXIT THEN
   newn SVX-I !
   BEGIN SVX-I @ SUMV-N @ < WHILE
      SVX-I @ SUMV-CTOR-SYM@ {: sym:n :}
      sym 0 <> IF
         sym SVX@ SVX-I @ 1 + = IF 0 sym SVX! THEN
      THEN
      SVX-I @ 1 + SVX-I !
   REPEAT
   newn SVX-HI ! ;

\ A variant owns exactly one generated constructor. Rewriting the cell would
\ strand the old symbol's head on a row that no longer carries it, so the second
\ write is refused rather than silently indexed wrong.
: SUMV-CTOR-SYM! ( n n -- ) {: vid:n sym:n :}
   vid SUMV-CTOR-SYM@ 0 <> IF s" tfam: variant constructor symbol already set" 76 die THEN
   SVX-ENSURE
   sym vid SUMV-REC@ SV.CTOR-SYM !
   vid sym SVX-LINK ;

\ SUMV-CTOR-FIRST-LINEAR ( n -- n ) : the SPECIFICATION of what SVX answers —
\ the lowest variant id (+1) with this constructor symbol, by walking the store.
\ test/checker-scan-index-suite.f differentials the indexed answer against it.
: SUMV-CTOR-FIRST-LINEAR ( n -- n ) {: sym:n :}
   0 SVX-I !
   BEGIN SVX-I @ SUMV-N @ < WHILE
      SVX-I @ SUMV-CTOR-SYM@ sym = IF SVX-I @ 1 + EXIT THEN
      SVX-I @ 1 + SVX-I !
   REPEAT
   0 ;

\ generated-constructor protection predicates (item 8 slice 3). Names are
\ matched case-insensitively against the recorded SV.CTOR-PKG spellings, so a
\ folded alias cannot reopen a constructor package, extend it with a new
\ tail, or undefine a generated word through any case variant. Installed into
\ the checker's CTOR-*-XT friend cells at the end of this file.
variable TF-CI              \ protection scan index (TF-I stays the decl scanner's)
variable TF-CW-COL          \ first-colon split position
: SUMV-CTOR-PKG-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id SUMV-REC@ SV.CTOR-PKG-U @ 0= IF RES-FALSE EXIT THEN
   id SUMV-CTOR-PKG$ a u CORE-STR=CI ;
: TFAM-CTOR-PKG? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a recorded ctor package name?
   0 TF-CI !
   BEGIN TF-CI @ SUMV-N @ < WHILE
      a u TF-CI @ SUMV-CTOR-PKG-MATCH? IF RES-TRUE EXIT THEN
      TF-CI @ 1 + TF-CI !
   REPEAT RES-FALSE ;
: TF-CW-SPLIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ first non-edge ':' -> TF-CW-COL
   -1 TF-CW-COL !
   0 TF-CI !
   BEGIN TF-CI @ u < WHILE
      a TF-CI @ + c@ 58 = IF TF-CI @ TF-CW-COL ! u TF-CI ! ELSE TF-CI @ 1 + TF-CI ! THEN
   REPEAT
   TF-CW-COL @ 0 > TF-CW-COL @ u 1 - < and ;
: TFAM-CTOR-WORD-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}   \ split name = ctor id?
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 -  id SUMV-NAME$ CORE-STR=CI ;
: TF-CW-TAIL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ name tail after the split colon
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 - ;
: TFAM-DERIVED-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a fixed generator-owned derived tail?
   a u s" eq" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" hash" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag" CORE-STR=CI ;
: TFAM-DERIVED-KIND-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}   \ derived tail the FAMILY generates
   a u s" eq" CORE-STR=CI IF fam TFAM-DERIVE-EQ? EXIT THEN
   a u s" hash" CORE-STR=CI IF fam TFAM-DERIVE-HASH? EXIT THEN
   fam TFAM-PRODUCT? IF RES-FALSE EXIT THEN   \ products get no discriminant
   a u s" tag" CORE-STR=CI ;                  \ tag rides ANY derive on sum/enum
: TFAM-DERIVED-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}   \ split name = id-family derived word?
   id SUMV-FAM@ TFAM-DERIVE-ANY? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ id SUMV-FAM@ TFAM-DERIVED-KIND-TAIL? ;
: TFAM-CTOR-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ exact PKG:VARIANT/derived word?
   a u TF-CW-SPLIT? 0= IF RES-FALSE EXIT THEN
   0 TF-CI !
   BEGIN TF-CI @ SUMV-N @ < WHILE
      a u TF-CI @ TFAM-CTOR-WORD-AT? IF RES-TRUE EXIT THEN
      a u TF-CI @ TFAM-DERIVED-AT? IF RES-TRUE EXIT THEN
      TF-CI @ 1 + TF-CI !
   REPEAT RES-FALSE ;
: TFAM-CTOR-EXTEND? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ new tail in a ctor package?
   a u TF-CW-SPLIT? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ TFAM-CTOR-PKG? 0= IF RES-FALSE EXIT THEN
   a u TFAM-CTOR-WORD? 0= ;

: SUMV-MATCH? ( n ptr u8 n n -- bool ) {: fam:n na:ptr nu:n id:n :}
   id SUMV-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id SUMV-NAME$ na nu CORE-STR= ;
: SUMV-FIND ( n ptr u8 n -- n bool ) {: fam:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ SUMV-N @ < WHILE
      fam na nu TF-I @ SUMV-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: SUMV-ADD ( n ptr u8 n n n n n -- n )
   {: fam:n na:ptr nu:n tag:n ss:n sc:n pc:n :}
   na nu TF-REQUIRE-CANON
   fam na nu SUMV-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   SUMV-ENSURE
   SUMV-N @ {: id:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + SUMV-N !
   id SUMV-REC@ {: r:ptr :}
   fam r SV.FAM !   noff r SV.NAME-OFF !   nu r SV.NAME-U !
   tag r SV.TAG !   ss r SV.SCH-START !   sc r SV.SCH-COUNT !   pc r SV.PAYCELLS !
   0 r SV.CTOR-SYM !   0 r SV.CTOR-PKG-OFF !   0 r SV.CTOR-PKG-U !
   id ;

\ ---------------------------------------------------------------------------
\ constructor package-name derivation (Package Shape; docs/type-families.md §12,
\ PLAN "Package Shape"). Maps the defining (package, family tail) to the reserved
\ constructor package spelling — the same bytes native, habu1, and the Gforth
\ mirror must produce (all three parse this one file). Readable escaped form:
\ uppercase every joined segment (the package segment AND the family tail),
\ double a literal '-' inside each ('-' -> '--'), join with single '-'
\ separators. Escaping the tail too is what makes the map injective: canonical
\ segments never start/end with '-', so hyphen runs inside escaped segments stay
\ even-length and interior, and each single '-' separator decodes uniquely.
\ Package `a-b` family `c` derives `A--B-C`; `a`+`b-c` derives `A-B--C`; a
\ top-level `a-b-c` derives `A--B--C` — all distinct. The escaped form is
\ injective at EVERY length, and both the runtime dictionary (DNAME-EXT external
\ names, habu2.f C-STORE-NAME) and the AOT snapshot (EXT records ride the
\ kept-source path, aot-capture.f) store names past the 16-byte inline cell, so
\ TF-CTOR-NAME-LIMIT is a READABILITY cap on the generated spelling — NOT a
\ dictionary/record structural bound (audit dot habu-raise-or-alias-5d2a6b70:
\ the SHA form below is itself > 16 bytes and already stores/constructs fine).
\ Past the cap the spelling is `T` + the first 16 lowercase hex digits of SHA-256
\ over the length-prefixed unescaped segment list + '-' + the raw uppercase
\ tail (unescaped: the fixed-width hash region already delimits it); that opaque
\ fallback only bounds pathologically long names, it never protects a fixed width.
\ Top level (empty package) derives the bare escaped tail: `result` -> `RESULT`.
\ SHA-256 loads after this file in the engine prefix, so the fallback hashes
\ through the friend xt installed by type-family-sha.f.
\ 32 (not 16): the longest legitimate escaped ctor package is ~25 bytes
\ (CAD-KIND-ADDRESS--SPACE; EVID/POLICY presence-slot sums like
\ EVID-CERTIFY--SLOT=18, POLICY-PROMOTE--POLICY=22), so 32 keeps every real
\ family on the readable escaped spelling with headroom while retaining the SHA
\ fallback for anything longer.
32 constant TF-CTOR-NAME-LIMIT   \ readable-spelling cap (audit: NOT DNAME-INL)
$400 constant TF-CTOR-CAP        \ derived-name / segment-list buffer bytes
create TF-CTOR-BUF TF-CTOR-CAP allot
variable TF-CTOR-U               \ derived-name length
create TF-CTOR-SEG TF-CTOR-CAP allot   \ length-prefixed segment list (SHA input)
variable TF-CTOR-SEG-U
create TF-CTOR-HEX 16 allot       \ 16 lowercase hex digits from the SHA fallback

: TF-SHA16-UNSET ( ptr u8 n ptr u8 -- )   \ default until type-family-sha.f installs
   {: a:ptr u:n dst:ptr :}
   s" tfam: constructor sha hook not installed" 76 die ;

\ friend hook: 16 hex of SHA-256 over (ptr,n) into the 16-byte output;
\ type-family-sha.f installs TF-SHA16 once the registry and hash both exist.
defer TF-SHA16-XT ( ptr u8 n ptr u8 -- )

: TF-SHA16-DEFAULT ( -- )
   [: TF-SHA16-UNSET ;] is TF-SHA16-XT ;
TF-SHA16-DEFAULT

: TF-UPPER-C ( n -- n ) {: c:n :} c TF-LOWER? IF c 32 - EXIT THEN c ;   \ a-z -> A-Z
: TF-CTOR-C, ( n -- )            \ append one byte to the derived-name buffer
   TF-CTOR-U @ TF-CTOR-CAP >= IF s" tfam: constructor name too long" 76 die THEN
   TF-CTOR-BUF TF-CTOR-U @ + c!
   TF-CTOR-U @ 1 + TF-CTOR-U ! ;
: TF-CTOR-SEG-C, ( n -- )        \ append one byte to the SHA segment-list input
   TF-CTOR-SEG-U @ TF-CTOR-CAP >= IF s" tfam: segment list too long" 76 die THEN
   TF-CTOR-SEG TF-CTOR-SEG-U @ + c!
   TF-CTOR-SEG-U @ 1 + TF-CTOR-SEG-U ! ;

: TF-CTOR-ESC ( ptr u8 n -- ) {: a:ptr u:n :}   \ one uppercased '-'->'--' escaped segment
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@ dup 45 = IF
         drop 45 TF-CTOR-C, 45 TF-CTOR-C,
      ELSE TF-UPPER-C TF-CTOR-C, THEN
      TF-I @ 1 + TF-I !
   REPEAT ;
: TF-CTOR-TAIL ( ptr u8 n -- ) {: a:ptr u:n :}      \ raw uppercased tail (hash form)
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@ TF-UPPER-C TF-CTOR-C,
      TF-I @ 1 + TF-I !
   REPEAT ;
: TF-CTOR-BUILD-ESCAPED ( ptr u8 n ptr u8 n -- )   \ (pkg tail)
   {: pa:ptr pu:n ta:ptr tu:n :}
   0 TF-CTOR-U !
   pu 0 > IF pa pu TF-CTOR-ESC  45 TF-CTOR-C, THEN
   ta tu TF-CTOR-ESC ;

: TF-CTOR-SEG-BUILD ( ptr u8 n -- ) {: pa:ptr pu:n :}   \ length-prefixed segment list
   0 TF-CTOR-SEG-U !
   pu 0= IF EXIT THEN                     \ top level: empty segment list
   pu TF-CTOR-SEG-C,                      \ one length byte (package name <= 255)
   0 TF-I !
   BEGIN TF-I @ pu < WHILE
      pa TF-I @ + c@ TF-CTOR-SEG-C,
      TF-I @ 1 + TF-I !
   REPEAT ;
: TF-CTOR-HEX, ( -- )            \ append the 16 fallback hex digits to the buffer
   0 TF-I !
   BEGIN TF-I @ 16 < WHILE
      TF-CTOR-HEX TF-I @ + c@ TF-CTOR-C,
      TF-I @ 1 + TF-I !
   REPEAT ;
: TF-CTOR-BUILD-HASH ( ptr u8 n ptr u8 n -- )   \ (pkg tail)
   {: pa:ptr pu:n ta:ptr tu:n :}
   pa pu TF-CTOR-SEG-BUILD
   TF-CTOR-SEG TF-CTOR-SEG-U @ TF-CTOR-HEX TF-SHA16-XT
   0 TF-CTOR-U !
   [char] T TF-CTOR-C,
   TF-CTOR-HEX,
   45 TF-CTOR-C,
   ta tu TF-CTOR-TAIL ;

\ TF-CTOR-PKG$ ( pkg-a pkg-u tail-a tail-u -- ptr u8 n ) : derived constructor
\ package name in TF-CTOR-BUF. Escaped form when it fits the inline name limit,
\ else the SHA-256 fallback. The tail must already be a canonical lowercase tail.
: TF-CTOR-PKG$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n ta:ptr tu:n :}
   pa pu ta tu TF-CTOR-BUILD-ESCAPED
   TF-CTOR-U @ TF-CTOR-NAME-LIMIT > IF pa pu ta tu TF-CTOR-BUILD-HASH THEN
   TF-CTOR-BUF TF-CTOR-U @ ;

\ ---------------------------------------------------------------------------
\ shared fields, keyed by (family-id, optional-variant-id, field tail).
\ PF-N includes rows provisional in the active transaction; PF-COMMIT-N is the
\ reflection high-water. ADD never returns a row id, so an id becomes observable
\ only after the outer transaction commits.
\ ---------------------------------------------------------------------------
-1 constant PF-NO-VARIANT
0 cells constant PF.FAM-OFF
1 cells constant PF.VAR-OFF
2 cells constant PF.NAME-OFF-AT
3 cells constant PF.NAME-U-OFF
4 cells constant PF.SCH-OFF
5 cells constant PF.SLOT-OFF
6 cells constant PF.CELLS-OFF
7 cells constant PF.BYTE-OFF-AT
8 cells constant PF.BYTES-OFF
9 cells constant PF.ALIGN-OFF
10 cells constant PF.FLAGS-OFF
11 cells constant PF-REC
CELL constant PF-REC-ALIGN
0 constant PF-REC-PTR-MASK

: PF.FAM ( ptr a -- ptr a ) PF.FAM-OFF + ;
: PF.VAR ( ptr a -- ptr a ) PF.VAR-OFF + ;
: PF.NAME-OFF ( ptr a -- ptr a ) PF.NAME-OFF-AT + ;
: PF.NAME-U ( ptr a -- ptr a ) PF.NAME-U-OFF + ;
: PF.SCH ( ptr a -- ptr a ) PF.SCH-OFF + ;
: PF.SLOT ( ptr a -- ptr a ) PF.SLOT-OFF + ;
: PF.CELLS ( ptr a -- ptr a ) PF.CELLS-OFF + ;
: PF.BYTE-OFF ( ptr a -- ptr a ) PF.BYTE-OFF-AT + ;
: PF.BYTES ( ptr a -- ptr a ) PF.BYTES-OFF + ;
: PF.ALIGN ( ptr a -- ptr a ) PF.ALIGN-OFF + ;
: PF.FLAGS ( ptr a -- ptr a ) PF.FLAGS-OFF + ;

PF.FAM-OFF 0 cells TF-LAYOUT=
PF.VAR-OFF 1 cells TF-LAYOUT=
PF.NAME-OFF-AT 2 cells TF-LAYOUT=
PF.NAME-U-OFF 3 cells TF-LAYOUT=
PF.SCH-OFF 4 cells TF-LAYOUT=
PF.SLOT-OFF 5 cells TF-LAYOUT=
PF.CELLS-OFF 6 cells TF-LAYOUT=
PF.BYTE-OFF-AT 7 cells TF-LAYOUT=
PF.BYTES-OFF 8 cells TF-LAYOUT=
PF.ALIGN-OFF 9 cells TF-LAYOUT=
PF.FLAGS-OFF 10 cells TF-LAYOUT=
PF-REC 11 cells TF-LAYOUT=
PF-REC-ALIGN CELL TF-LAYOUT=
PF-REC PF-REC-ALIGN mod 0 TF-LAYOUT=
PF-REC-PTR-MASK 0 TF-LAYOUT=
0 PF.FAM PF.FAM-OFF TF-LAYOUT=
0 PF.VAR PF.VAR-OFF TF-LAYOUT=
0 PF.NAME-OFF PF.NAME-OFF-AT TF-LAYOUT=
0 PF.NAME-U PF.NAME-U-OFF TF-LAYOUT=
0 PF.SCH PF.SCH-OFF TF-LAYOUT=
0 PF.SLOT PF.SLOT-OFF TF-LAYOUT=
0 PF.CELLS PF.CELLS-OFF TF-LAYOUT=
0 PF.BYTE-OFF PF.BYTE-OFF-AT TF-LAYOUT=
0 PF.BYTES PF.BYTES-OFF TF-LAYOUT=
0 PF.ALIGN PF.ALIGN-OFF TF-LAYOUT=
0 PF.FLAGS PF.FLAGS-OFF TF-LAYOUT=

\ --- Registry cell write-protection. REG-PROTECT / IMK-SEAL-REGISTRY (defined in
\ src/core/util.f and src/core/internal-mark.f; see the util.f header) seal each
\ registry control cell DNAME-INT so a bare `<cell> @`/`<cell> !` or `' <cell>`
\ fails closed (`hb: internal engine word`, rc 70). The product-field arena is the
\ first-sealed set (confirmed exploit `99 PF-COMMIT-N !` corrupted
\ TYPE-FIELD:COUNT); the sibling family/variant/string/param/layout registries in
\ this file and the schema registries in type-schema.f are sealed the same way at
\ their own definition sites.
4 constant PF-CAP-INIT
variable PF-CAP-V   PF-CAP-INIT PF-CAP-V !   REG-PROTECT
: PF-CAP ( -- n ) PF-CAP-V @ ;
create PF-A-BOOT   PF-CAP-INIT PF-REC * allot   REG-PROTECT
variable PF-A-P   PF-A-BOOT PF-A-P !   REG-PROTECT
: PF-BASE ( -- ptr a ) PF-A-P @ ;
variable PF-N   0 PF-N !   REG-PROTECT
variable PF-COMMIT-N   0 PF-COMMIT-N !   REG-PROTECT

PF-REC CELL / constant PF-REC-CELLS   \ product-field record stride, in cells

\ PF-SCRUB ( lo hi -- ) : zero the product-field records [lo,hi) back to canonical
\ zero. Rows are pointer-free (PF-REC-PTR-MASK 0), so a zeroed row is the canonical
\ "absent" row: retired provisional or rejected rows then leave no observable bytes
\ in reflection or in snapshot/fixpoint identity. lo>=hi (nothing retired) is a
\ no-op. Operates on the live arena (PF-BASE).
: PF-SCRUB ( n n -- ) {: lo:n hi:n :}
   hi lo <= IF EXIT THEN
   PF-BASE  lo PF-REC-CELLS *  hi PF-REC-CELLS *  ARENA-CELLS-ZERO ;

: PF-GROW ( n -- ) {: need:n :}
   need PF-CAP-V @ 2 * max {: nc:n :}
   PF-A-P  PF-CAP-V @ PF-REC *  nc PF-REC *  REG-GROW1
   nc PF-CAP-V ! ;
: PF-ENSURE ( -- )
   PF-N @ PF-CAP-V @ < IF exit THEN
   PF-N @ 1 + PF-GROW ;
\ Field-id readers reject an out-of-band id with a catchable E-PF-ID throw, never
\ a process-killing `die`: the id is public reflection input (an `n` any checked
\ caller can guess), so a bad id is a caller error to surface under `catch`, not
\ an engine assert. PF-ROW admits provisional rows (bound PF-N) for in-transaction
\ machinery; PF-REC@ is the committed reflection reader (bound PF-COMMIT-N), so a
\ guessed provisional id (>= PF-COMMIT-N) rejects here before it can be read.
: PF-ROW ( n -- ptr a ) {: id:n :}
   id 0 < IF E-PF-ID throw THEN
   id PF-N @ >= IF E-PF-ID throw THEN
   id PF-REC * PF-BASE + ;
: PF-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < IF E-PF-ID throw THEN
   id PF-COMMIT-N @ >= IF E-PF-ID throw THEN
   id PF-REC * PF-BASE + ;

: PF-FAM@ ( n -- n ) PF-REC@ PF.FAM @ ;
: PF-VAR@ ( n -- n ) PF-REC@ PF.VAR @ ;
: PF-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id PF-REC@ {: r:ptr :}  r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ ;
: PF-SCH@ ( n -- n ) PF-REC@ PF.SCH @ ;
: PF-PENDING-SCH@ ( n -- n ) PF-ROW PF.SCH @ ;
: PF-SLOT@ ( n -- n ) PF-REC@ PF.SLOT @ ;
: PF-CELLS@ ( n -- n ) PF-REC@ PF.CELLS @ ;
: PF-BYTE-OFF@ ( n -- n ) PF-REC@ PF.BYTE-OFF @ ;
: PF-BYTES@ ( n -- n ) PF-REC@ PF.BYTES @ ;
: PF-ALIGN@ ( n -- n ) PF-REC@ PF.ALIGN @ ;
: PF-FLAGS@ ( n -- n ) PF-REC@ PF.FLAGS @ ;
: PF-N@ ( -- n ) PF-COMMIT-N @ ;

\ A variant has one declared-payload source. Legacy SUMTYPE declarations store
\ positional roots in the SUMV schema range. The unified ENUM front end stores
\ named rows in TYPE-FIELD and leaves that range empty. The accessors below are
\ the single semantic seam for both representations. A sum/enum family with a
\ nonzero declared field count uses TYPE-FIELD for every variant; invalid bounds,
\ ownership, or a missing named index are invariant failures, never reasons to
\ fall back into positional storage. PRODUCT keeps positional schemas for its
\ generated make/unmake variants. An all-empty family has no payload under either
\ representation and stays equivalent. Only a bounded, committed,
\ variant-owned field slice is a valid named representation.
: SUMV-NAMED-FIELD ( n n -- n bool ) {: vid:n wanted:n :}
   wanted 0 < IF 0 RES-FALSE EXIT THEN
   vid SUMV-FAM@ {: fam:n :}
   fam TFAM-FLD-START@ {: base:n :}
   fam TFAM-FLD-COUNT@ {: count:n :}
   base 0 < IF 0 RES-FALSE EXIT THEN
   count 0 < IF 0 RES-FALSE EXIT THEN
   base PF-COMMIT-N @ > IF 0 RES-FALSE EXIT THEN
   count PF-COMMIT-N @ base - > IF 0 RES-FALSE EXIT THEN
   0
   0 BEGIN dup count < WHILE
      base over + {: fid:n :}
      fid PF-FAM@ fam = fid PF-VAR@ vid = and IF
         over wanted = IF 2drop fid RES-TRUE EXIT THEN
         swap 1 + swap
      THEN
      1 +
   REPEAT 2drop 0 RES-FALSE ;

: SUMV-FAMILY-LEGACY-PAYLOAD? ( n -- bool ) {: fam:n :}
   fam TFAM-VAR-START@ {: base:n :}
   fam TFAM-VAR-COUNT@ {: count:n :}
   base 0 < count 0 < or IF E-TFAM-PAYLOAD throw THEN
   base SUMV-N @ > IF E-TFAM-PAYLOAD throw THEN
   count SUMV-N @ base - > IF E-TFAM-PAYLOAD throw THEN
   0 BEGIN dup count < WHILE
      base over + {: vid:n :}
      vid SUMV-FAM@ fam <> IF E-TFAM-PAYLOAD throw THEN
      vid SUMV-SCH-COUNT@ 0 <> IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;

: SUMV-NAMED-PAYLOAD? ( n -- bool ) {: vid:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-START@ {: base:n :}
   fam TFAM-FLD-COUNT@ {: count:n :}
   count 0 < IF E-TFAM-PAYLOAD throw THEN
   count 0= IF RES-FALSE EXIT THEN
   base 0 < IF E-TFAM-PAYLOAD throw THEN
   base PF-COMMIT-N @ > IF E-TFAM-PAYLOAD throw THEN
   count PF-COMMIT-N @ base - > IF E-TFAM-PAYLOAD throw THEN
   0 BEGIN dup count < WHILE
      base over + {: fid:n :}
      fid PF-FAM@ fam <> IF E-TFAM-PAYLOAD throw THEN
      fid PF-VAR@ {: owner:n :}
      owner PF-NO-VARIANT = IF E-TFAM-PAYLOAD throw THEN
      owner 0 < owner SUMV-N @ >= or IF E-TFAM-PAYLOAD throw THEN
      owner SUMV-FAM@ fam <> IF E-TFAM-PAYLOAD throw THEN
      1 +
   REPEAT drop
   fam SUMV-FAMILY-LEGACY-PAYLOAD? IF E-TFAM-PAYLOAD throw THEN
   RES-TRUE ;

: SUMV-PAY-FIELD ( n n -- n bool ) {: vid:n wanted:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF 0 RES-FALSE EXIT THEN
   vid wanted SUMV-NAMED-FIELD ;

: SUMV-PAY-N ( n -- n ) {: vid:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF vid SUMV-SCH-COUNT@ EXIT THEN
   vid SUMV-FAM@ {: fam:n :}
   fam TFAM-FLD-START@ {: base:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      base over + {: fid:n :}
      fid PF-FAM@ fam = fid PF-VAR@ vid = and IF swap 1 + swap THEN
      1 +
   REPEAT drop ;

: SUMV-PAY-ROOT ( n n -- n ) {: vid:n index:n :}
   index 0 < IF E-TFAM-PAYLOAD throw THEN
   vid SUMV-NAMED-PAYLOAD? IF
      vid index SUMV-NAMED-FIELD 0= IF drop E-TFAM-PAYLOAD throw THEN
      PF-SCH@ EXIT
   THEN
   index vid SUMV-SCH-COUNT@ >= IF E-TFAM-PAYLOAD throw THEN
   vid SUMV-SCH-START@ index + ;

: SUMV-PAYCELLS@ ( n -- n ) {: vid:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF vid SUMV-RAW-PAYCELLS@ EXIT THEN
   vid SUMV-PAY-N {: count:n :}
   0
   0 BEGIN dup count < WHILE
      vid over SUMV-NAMED-FIELD 0= IF drop E-TFAM-PAYLOAD throw THEN
      PF-CELLS@ rot + swap
      1 +
   REPEAT drop ;

\ --- arg-aware instantiated width (item 12 / layout-cap slice 1, docs §18). The
\ registry TFAM-WIDTH@ assumes every parameter contributes one cell; that is exact
\ WHILE family parameters stay cell-kinded, but §18's WIDTH function is defined
\ over the INSTANTIATED field/variant types. TFAM-INST-WIDTH@ walks a resolved
\ layout term's variant/product schemas and substitutes each param slot by the
\ width of the term's matching arg (T-WIDTH, checker.f), so a layout arg widens
\ the sum payload / product body. For every cell-kinded instantiation (all args
\ width 1) it equals TFAM-WIDTH@, so routing T-WIDTH through it is behaviour-
\ preserving groundwork. Nested parametric families propagate their own args in a
\ later slice; a schema SC-APP is always an arity-0 concrete payload family today,
\ whose instantiated width already equals its declared registry width.
: SCH-NODE-IWIDTH ( n n -- n ) {: node:n term:n :}   \ inst width of one schema node under term's args
   node SCHEMA-PARAM? IF term node SCHEMA-A@ PARAM>ARG T-WIDTH EXIT THEN
   node SCHEMA-APP?   IF node SCHEMA-A@ TFAM-WIDTH@ EXIT THEN
   1 ;
: SUMV-IWIDTH ( n n -- n ) {: vid:n term:n :}   \ sum of variant vid's payload field inst-widths
   0                                            \ acc
   0 BEGIN dup vid SUMV-PAY-N < WHILE             \ ( acc j )
      vid over SUMV-PAY-ROOT SCHEMA-ROOT@ term SCH-NODE-IWIDTH   \ ( acc j wj )
      rot + swap                                 \ ( acc' j )
      1 +
   REPEAT drop ;
: SUM-IWIDTH ( n -- n ) {: term:n :}            \ tag + max variant payload inst-width
   term PARAM>FAM {: fam:n :}
   fam TFAM-VAR-START@ {: vs:n :}
   0                                            \ maxpay
   0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE        \ ( maxpay j )
      vs over + term SUMV-IWIDTH                  \ ( maxpay j payj )
      rot max swap                               \ ( maxpay' j )
      1 +
   REPEAT drop
   1 + ;                                         \ + tag cell
: PRODUCT-IWIDTH ( n -- n ) {: term:n :}        \ sum of field inst-widths (no tag)
   term PARAM>FAM {: fam:n :}
   fam TFAM-FLD-START@ {: fs:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      fs over + PF-PENDING-SCH@ SCHEMA-ROOT@ term SCH-NODE-IWIDTH
      rot + swap
      1 +
   REPEAT drop ;
: TFAM-INST-WIDTH@ ( n -- n ) {: term:n :}      \ instantiated logical width of a resolved layout term
   term PARAM>FAM {: fam:n :}
   fam TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   fam TFAM-PRODUCT? IF term PRODUCT-IWIDTH EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF term SUM-IWIDTH EXIT THEN
   1 ;

: PF-ROW-OWNER? ( n n ptr n -- bool ) {: fam:n var:n r:ptr :}
   r PF.FAM @ fam = r PF.VAR @ var = and ;
: PF-MATCH? ( n n ptr u8 n n -- bool ) {: fam:n var:n na:ptr nu:n id:n :}
   id PF-REC@ {: r:ptr :}
   fam var r PF-ROW-OWNER? 0= IF RES-FALSE EXIT THEN
   r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= ;
: PF-FIND ( n n ptr u8 n -- n bool ) {: fam:n var:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ PF-COMMIT-N @ < WHILE
      fam var na nu TF-I @ PF-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: PF-EACH ( n n n -- n bool ) {: fam:n var:n start:n :}
   start 0 < IF 0 RES-FALSE EXIT THEN
   start TF-I !
   BEGIN TF-I @ PF-COMMIT-N @ < WHILE
      TF-I @ PF-REC@ {: r:ptr :}
      fam var r PF-ROW-OWNER? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;

\ Strict-LIFO transaction frames. COMMIT retains its rollback frame through
\ global finalization. Nested commit keeps rows provisional; only outer commit
\ advances PF-COMMIT-N. Every frame owns both mutable marks.
0 cells constant PFTX.PFN-OFF
1 cells constant PFTX.STRU-OFF
2 cells constant PFTX.TOK-OFF
3 cells constant PFTX.COMMITN-OFF
4 cells constant PFTX.STATE-OFF
5 cells constant PF-TX-REC
CELL constant PF-TX-REC-ALIGN
0 constant PF-TX-REC-PTR-MASK

: PFTX.PFN ( ptr a -- ptr a ) PFTX.PFN-OFF + ;
: PFTX.STRU ( ptr a -- ptr a ) PFTX.STRU-OFF + ;
: PFTX.TOK ( ptr a -- ptr a ) PFTX.TOK-OFF + ;
: PFTX.COMMITN ( ptr a -- ptr a ) PFTX.COMMITN-OFF + ;
: PFTX.STATE ( ptr a -- ptr a ) PFTX.STATE-OFF + ;

PFTX.PFN-OFF 0 cells TF-LAYOUT=
PFTX.STRU-OFF 1 cells TF-LAYOUT=
PFTX.TOK-OFF 2 cells TF-LAYOUT=
PFTX.COMMITN-OFF 3 cells TF-LAYOUT=
PFTX.STATE-OFF 4 cells TF-LAYOUT=
PF-TX-REC 5 cells TF-LAYOUT=
PF-TX-REC-ALIGN CELL TF-LAYOUT=
PF-TX-REC PF-TX-REC-ALIGN mod 0 TF-LAYOUT=
PF-TX-REC-PTR-MASK 0 TF-LAYOUT=
0 PFTX.PFN PFTX.PFN-OFF TF-LAYOUT=
0 PFTX.STRU PFTX.STRU-OFF TF-LAYOUT=
0 PFTX.TOK PFTX.TOK-OFF TF-LAYOUT=
0 PFTX.COMMITN PFTX.COMMITN-OFF TF-LAYOUT=
0 PFTX.STATE PFTX.STATE-OFF TF-LAYOUT=

4 constant PF-TX-CAP-INIT
variable PF-TX-CAP-V   PF-TX-CAP-INIT PF-TX-CAP-V !   REG-PROTECT
create PF-TX-BOOT   PF-TX-CAP-INIT PF-TX-REC * allot   REG-PROTECT
variable PF-TX-P   PF-TX-BOOT PF-TX-P !   REG-PROTECT
variable PF-TX-DEPTH   0 PF-TX-DEPTH !   REG-PROTECT
variable PF-TX-SERIAL   0 PF-TX-SERIAL !   REG-PROTECT

\ Multi-frame cleanup seam. A declaration participant that finds frames opened
\ above its own has to retire them, but only this owner may walk its own frame
\ stack, and the walk must stay unreachable from ordinary source. So the entry
\ point is one execution vector: the owner installs its private ROLLBACK-THROUGH
\ into this deferred word below, the compiled declaration-event participant calls
\ the vector, and src/core/generated-declaration-protection.f retires the name
\ once every prefix caller is compiled. The vector is deliberately NOT a checker
\ primitive row: a primitive axiom survives `undefine`, so retiring the name
\ would leave the checker certifying calls the runtime can no longer resolve.
defer TDECL-FIELD-CLEANUP-XT ( n -- )

\ Total release seam, same shape and lifetime as the cleanup vector above. The
\ coordinator's release phase runs after every reversible commit succeeded, so
\ the declaration-event participant must discard its field frame without being
\ able to reject: PREPARE already proved the frame is the live committed top.
\ RELEASE below is that discard and nothing else. It stays private to this owner
\ — a public FINALIZE-style entry would have to re-validate, which is exactly
\ what a total release may not do — so the compiled participant reaches it
\ through this one execution vector, and
\ src/core/generated-declaration-protection.f retires the name once that sole
\ caller is compiled.
defer TDECL-FIELD-RELEASE-XT ( -- )

package TYPE-FIELD-OWNER

0 constant STATE-OPEN
1 constant STATE-COMMITTED

: TX-BASE ( -- ptr a ) PF-TX-P @ ;
: TX-GROW ( -- )
   PF-TX-CAP-V @ 2 * {: nc:n :}
   PF-TX-P PF-TX-CAP-V @ PF-TX-REC * nc PF-TX-REC * REG-GROW1
   nc PF-TX-CAP-V ! ;
: TX-ENSURE ( -- )
   PF-TX-DEPTH @ PF-TX-CAP-V @ < IF EXIT THEN
   TX-GROW ;
: TX-AT ( n -- ptr a ) PF-TX-REC * TX-BASE + ;
: TX-TOP ( -- ptr a )
   PF-TX-DEPTH @ 0= IF E-PF-TX throw THEN
   PF-TX-DEPTH @ 1 - TX-AT ;
: TX-REQUIRE ( n -- ) TX-TOP PFTX.TOK @ <> IF E-PF-TX throw THEN ;
: TX-STATE-REQUIRE ( n n -- ) {: tx:n state:n :}
   tx TX-REQUIRE
   TX-TOP PFTX.STATE @ state <> IF E-PF-TX throw THEN ;
: TX-MARKS-REQUIRE ( -- )
   TX-TOP {: r:ptr :}
   r PFTX.PFN @ PF-N @ > IF E-PF-TX throw THEN
   r PFTX.STRU @ TF-STR-U @ > IF E-PF-TX throw THEN ;
: TX-OPEN-MARKS-REQUIRE ( -- )
   TX-MARKS-REQUIRE
   TX-TOP PFTX.COMMITN @ PF-COMMIT-N @ <> IF E-PF-TX throw THEN ;
: TX-PARENT-REQUIRE ( -- )
   PF-TX-DEPTH @ 0= IF EXIT THEN
   TX-TOP PFTX.STATE @ STATE-OPEN <> IF E-PF-TX throw THEN
   TX-OPEN-MARKS-REQUIRE ;
: TX-COMMITTED-MARKS-REQUIRE ( -- )
   TX-MARKS-REQUIRE
   PF-TX-DEPTH @ 1 = IF
      PF-COMMIT-N @ PF-N @ <> IF E-PF-TX throw THEN
      EXIT
   THEN
   TX-TOP PFTX.COMMITN @ PF-COMMIT-N @ <> IF E-PF-TX throw THEN ;
: RELEASE ( -- )
   PF-TX-DEPTH @ 1 - PF-TX-DEPTH ! ;

public

\ The wrap guard stays although no test drives it. Reaching it needs 2^63 OPENs
\ in one process, so the only way to observe it was the whitebox seam that poked
\ PF-TX-SERIAL directly, and this leaf's contract retired that seam rather than
\ publish a raw-state setter to keep it. Deleting the guard instead would make a
\ wrapped serial mint token 0 or a negative token, which every phase compares by
\ equality — a stale token could then alias a live frame, the one failure the
\ whole token scheme exists to prevent. It is a cheap always-on precondition on
\ the sole mutator of the serial, so it is kept unexercised and deliberately not
\ re-exposed through a TRUSTED boundary.
: OPEN ( -- n )
   TX-PARENT-REQUIRE
   PF-TX-SERIAL @ 1 + dup 0 <= IF drop E-PF-TX throw THEN
   {: tok:n :}
   TX-ENSURE
   tok PF-TX-SERIAL !
   PF-TX-DEPTH @ TX-AT {: r:ptr :}
   PF-N @ r PFTX.PFN !
   TF-STR-U @ r PFTX.STRU !
   PF-COMMIT-N @ r PFTX.COMMITN !
   STATE-OPEN r PFTX.STATE !
   tok r PFTX.TOK !
   PF-TX-DEPTH @ 1 + PF-TX-DEPTH !
   tok ;
: PREPARE ( n -- n ) {: tx:n :}
   tx STATE-OPEN TX-STATE-REQUIRE
   TX-OPEN-MARKS-REQUIRE
   PF-N @ ;
: COMMIT ( n -- ) {: tx:n :}
   tx PREPARE drop
   PF-TX-DEPTH @ 1 = IF PF-N @ PF-COMMIT-N ! THEN
   STATE-COMMITTED TX-TOP PFTX.STATE ! ;
: FINALIZE ( n -- ) {: tx:n :}
   tx STATE-COMMITTED TX-STATE-REQUIRE
   TX-COMMITTED-MARKS-REQUIRE
   RELEASE ;
\ A live frame is in exactly one of the two states OPEN sets and COMMIT moves it
\ to, so the state selects which watermark check applies; there is no third value
\ to reject.
: ROLLBACK ( n -- ) {: tx:n :}
   tx TX-REQUIRE
   TX-TOP PFTX.STATE @ STATE-OPEN =
      IF TX-OPEN-MARKS-REQUIRE ELSE TX-COMMITTED-MARKS-REQUIRE THEN
   TX-TOP {: r:ptr :}
   r PFTX.PFN @ {: keep:n :}
   keep PF-N @ PF-SCRUB              \ scrub the provisional rows this rollback retires
   keep PF-N !
   r PFTX.STRU @ TF-STR-U !
   r PFTX.COMMITN @ PF-COMMIT-N !
   RELEASE ;

\ The field owner is the only authority that can read a provisional row.  The
\ caller must present the exact live top transaction token, the row must have
\ been added by that frame, and its family must match.  DECL-EVENT wraps this
\ sealed owner seam with its own declaration token and event-range proof.
: TX-SCHEMA-FOR ( n n n -- n ) {: tx:n fam:n id:n :}
   tx STATE-OPEN TX-STATE-REQUIRE
   TX-OPEN-MARKS-REQUIRE
   id TX-TOP PFTX.PFN @ < IF E-PF-ID throw THEN
   id PF-N @ >= IF E-PF-ID throw THEN
   id PF-ROW {: r:ptr :}
   r PF.FAM @ fam <> IF E-PF-OWNER throw THEN
   r PF.SCH @ ;
: TX-CELLS-FOR ( n n n -- n ) {: tx:n fam:n id:n :}
   tx fam id TX-SCHEMA-FOR drop
   id PF-ROW PF.CELLS @ ;

private

\ Cleanup for a failed declaration that left frames open above the caller's own.
\ TX-INDEX proves the exact token is still one of the live frames BEFORE anything
\ moves, so a stale or already-released token leaves every mark untouched; the
\ retirement itself is the ordinary ROLLBACK path applied to each live frame from
\ the top down, so descendants are retired strictly last-in first-out and the
\ target frame's own marks are the last ones restored. This stays private and its
\ only caller is the vector installed below: no source-level word may ever pick a
\ frame out of the middle of this stack.
: TX-INDEX ( n -- n ) {: tx:n :}
   PF-TX-DEPTH @
   BEGIN dup 0 > WHILE
      1 -
      dup TX-AT PFTX.TOK @ tx = IF EXIT THEN
   REPEAT
   drop E-PF-TX throw ;

: ROLLBACK-THROUGH ( n -- ) {: tx:n :}
   tx TX-INDEX {: keep:n :}
   BEGIN PF-TX-DEPTH @ keep > WHILE
      TX-TOP PFTX.TOK @ ROLLBACK
   REPEAT ;

: VECTORS-INSTALL ( -- )
   [: ROLLBACK-THROUGH ;] is TDECL-FIELD-CLEANUP-XT
   [: RELEASE ;] is TDECL-FIELD-RELEASE-XT ;
VECTORS-INSTALL

;package

\ ADD validation. Field rows carry explicit logical-cell and memory-layout
\ metadata under every registered family policy; no policy implies CELL-sized
\ storage. Policy-specific lowering consumes these validated facts later.
$7FFFFFFFFFFFFFFF constant PF-MAX-N
0 constant PF-FLAGS-NONE

: TF-GRAMMAR-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" newtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" enum" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;enum" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" product" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;product" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" field" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" policy" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" derive" CORE-STR=CI ;

package TYPE-NAME

7107 constant E-SYNTAX
7110 constant E-RESERVED

public

\ CONTROL? is the ONE place the control-word list is written down. Every
\ declaration name gate in the engine asks this word rather than keeping a list
\ of its own: this package's own RESERVED? (variant names), sumtype.f's
\ TDECL-RESERVED? (legacy family names), PF-RESERVED? below (field names), and
\ the unified STRUCTURE / ENUM front ends through their CONTROL-KW? forwarders.
\ A name that spells a control word would compile into a definition body as that
\ control word, so no declaration position may take one; keeping a second copy of
\ the list is how the front ends and the legacy definers drifted apart before.
: CONTROL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" then" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" else" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" begin" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" until" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" again" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" while" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" repeat" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" case" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" of" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" endof" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" endcase" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" do" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ?do" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" loop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" +loop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" leave" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" unloop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" exit" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" recurse" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" construct" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" match" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;match" CORE-STR=CI ;

private

: RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = IF RES-TRUE EXIT THEN
   a u VREC-FIND IF drop RES-TRUE EXIT THEN drop
   a u s" field" CORE-STR= IF RES-TRUE EXIT THEN
   a u CON-OF 0 <> IF RES-TRUE EXIT THEN
   a u ATOM-TOK? IF RES-TRUE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u CONTROL? IF RES-TRUE EXIT THEN
   a u TF-GRAMMAR-KEYWORD? ;

: FAMILY-TAKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " a u TFAM-FIND-IN nip IF RES-TRUE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ a u TFAM-FIND-IN nip ;

public

: VARIANT-REQUIRE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF E-SYNTAX throw THEN
   a u TF-REQUIRE-CANON
   a u RESERVED? IF E-RESERVED throw THEN
   a u FAMILY-TAKEN? IF E-RESERVED throw THEN ;

private
get-current prot-wid-add
public
get-current prot-wid-add

;package

\ A field tail may not spell a grammar keyword, a control word, or one of the
\ generated-operation names. The control-word arm comes from TYPE-NAME:CONTROL?,
\ the single owner of that list, so a field named `if` is refused on every path
\ that registers a field row: the legacy PRODUCT definer through
\ TDECL-REQUIRE-FIELD-NAME and the unified STRUCTURE / ENUM front ends through
\ TYPE-FIELD-OWNER:ADD. Before this arm existed the two family-name gates
\ rejected `if` and the field gate accepted it.
: PF-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TF-GRAMMAR-KEYWORD? IF RES-TRUE EXIT THEN
   a u TYPE-NAME:CONTROL? IF RES-TRUE EXIT THEN
   a u s" make" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" unmake" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" eq" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" hash" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" order" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" encode" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" decode" CORE-STR=CI ;
: PF-NAME-REQUIRE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TF-REQUIRE-CANON
   a u PF-RESERVED? IF E-PF-NAME throw THEN ;

: PF-FAM-LIVE? ( n -- bool ) {: fam:n :} fam 0 >= fam TFAM-N @ < and ;
: PF-OWNER-OK? ( n n -- bool ) {: fam:n var:n :}
   fam PF-FAM-LIVE? 0= IF RES-FALSE EXIT THEN
   var PF-NO-VARIANT = IF fam TFAM-PRODUCT? EXIT THEN
   var 0 < var SUMV-N @ >= or IF RES-FALSE EXIT THEN
   var SUMV-FAM@ fam =
   fam TFAM-SUM? fam TFAM-ENUM? or and ;
: PF-FAM-VISIBLE? ( n n -- bool ) {: owner:n fam:n :}
   fam TFAM-PUBLIC? IF RES-TRUE EXIT THEN
   fam TFAM-PKG$ owner TFAM-PKG$ CORE-STR= ;
: PF-APP-KIND ( n -- n ) {: fam:n :}
   fam TFAM-KIND@ TK-EVIDENCE = IF PK-EVIDENCE EXIT THEN
   fam TFAM-LAYOUT? IF PK-LAYOUT EXIT THEN
   PK-CELL ;
: PF-KIND-OK? ( n n -- bool ) {: got:n want:n :}
   want PK-TYPE = IF got PK-EVIDENCE <> EXIT THEN
   got want = ;

\ Validate one quotation effect side: a live SCH-ROW node whose element type nodes
\ each pass PF-NODE-KIND?. Deferred so PF-NODE-KIND?'s SCH-QUOT case can reach it
\ before the impl (which calls PF-NODE-KIND? back) is defined and installed below.
defer PF-QUOT-ROW-OK? ( n n -- bool )   \ ( owner rownode -- bool )

: PF-NODE-KIND? ( n n -- n bool ) {: owner:n node:n :}
   node SCHEMA-NODE-OK? 0= IF 0 RES-FALSE EXIT THEN
   node SCHEMA-PARAM? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ {: idx:n :}
      idx 0 < idx owner TFAM-ARITY@ >= or IF 0 RES-FALSE EXIT THEN
      owner idx TFAM-PK@ dup PK-CELL <> IF drop 0 RES-FALSE EXIT THEN
      RES-TRUE EXIT
   THEN
   node SCHEMA-CON? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ CT-LIVE? IF PK-CELL RES-TRUE ELSE 0 RES-FALSE THEN EXIT
   THEN
   node SCHEMA-PTR? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ dup node >= IF drop 0 RES-FALSE EXIT THEN
      owner swap RECURSE 0= IF drop 0 RES-FALSE EXIT THEN drop
      PK-CELL RES-TRUE EXIT
   THEN
   node SCHEMA-QUOT? IF
      node SCHEMA-A@ dup 0= swap -1 = or 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-C@ SCH-QUOT-ROWS <> IF 0 RES-FALSE EXIT THEN
      node SCHEMA-B@ {: start:n :}
      start 0 < start SCH-ROOT-N @ > or IF 0 RES-FALSE EXIT THEN
      SCH-QUOT-ROWS SCH-ROOT-N @ start - > IF 0 RES-FALSE EXIT THEN
      0 BEGIN dup SCH-QUOT-ROWS < WHILE            \ each of the four sides is a valid SCH-ROW
         dup >r
         start r@ + SCHEMA-ROOT@ dup node >= IF    \ side root -> row node, must be built before node
            drop r> drop drop 0 RES-FALSE EXIT
         THEN
         owner swap PF-QUOT-ROW-OK? 0= IF
            r> drop drop 0 RES-FALSE EXIT
         THEN
         r> drop 1 +
      REPEAT drop
      PK-CELL RES-TRUE EXIT
   THEN
   node SCHEMA-APP? IF
      node SCHEMA-A@ {: fam:n :}
      fam PF-FAM-LIVE? 0= IF 0 RES-FALSE EXIT THEN
      owner fam PF-FAM-VISIBLE? 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-B@ {: start:n :}
      node SCHEMA-C@ {: count:n :}
      count fam TFAM-ARITY@ <> IF 0 RES-FALSE EXIT THEN
      count 0= start 0= 0= and IF 0 RES-FALSE EXIT THEN
      start 0 < start SCH-ROOT-N @ > or IF 0 RES-FALSE EXIT THEN
      count SCH-ROOT-N @ start - > IF 0 RES-FALSE EXIT THEN
      0 BEGIN dup count < WHILE
         dup >r
         start r@ + SCHEMA-ROOT@ dup node >= IF
            drop r> drop drop 0 RES-FALSE EXIT
         THEN
         owner swap RECURSE 0= IF drop r> drop drop 0 RES-FALSE EXIT THEN
         fam r@ TFAM-PK@ PF-KIND-OK? 0= IF r> drop drop 0 RES-FALSE EXIT THEN
         r> drop
         1 +
      REPEAT drop
      fam PF-APP-KIND RES-TRUE EXIT
   THEN
   0 RES-FALSE ;

\ A quotation side row is valid when it is a SCH-ROW node with an in-range element
\ range and every element type node passes PF-NODE-KIND?. Elements are built before
\ the row (parser order), so each must sit below the row id — the same acyclic guard
\ PF-NODE-KIND? applies to APP/PTR children.
: PF-QUOT-ROW-OK-IMPL ( n n -- bool ) {: owner:n rownode:n :}
   rownode SCHEMA-ROW? 0= IF RES-FALSE EXIT THEN
   rownode SCHEMA-ROW-COUNT@ {: cnt:n :}
   rownode SCHEMA-ROW-START@ {: estart:n :}
   cnt 0 < IF RES-FALSE EXIT THEN
   estart 0 < estart SCH-ROOT-N @ > or IF RES-FALSE EXIT THEN
   cnt SCH-ROOT-N @ estart - > IF RES-FALSE EXIT THEN
   0 BEGIN dup cnt < WHILE                          \ ( j )
      dup estart + SCHEMA-ROOT@                     \ ( j elem )
      dup rownode >= IF 2drop RES-FALSE EXIT THEN
      owner swap PF-NODE-KIND? nip 0= IF drop RES-FALSE EXIT THEN
      1 +
   REPEAT drop RES-TRUE ;
: PF-QUOT-ROW-INSTALL ( -- ) [: PF-QUOT-ROW-OK-IMPL ;] is PF-QUOT-ROW-OK? ;
PF-QUOT-ROW-INSTALL
: PF-SCHEMA-OK? ( n n -- bool ) {: owner:n sch:n :}
   sch 0 < sch SCH-ROOT-N @ >= or IF RES-FALSE EXIT THEN
   owner sch SCHEMA-ROOT@ PF-NODE-KIND? nip ;
: PF-SCHEMA-WIDTH ( n -- n ) {: sch:n :}
   sch SCHEMA-ROOT@ {: node:n :}
   node SCHEMA-APP? IF node SCHEMA-A@ TFAM-WIDTH@ ELSE 1 THEN ;

\ --- declaration-time parameter-arity walk for SUM variant payload schemas
\ (dot habu-declaration-time-arity-4c70e37c). PRODUCT fields validate their whole
\ field-schema tree at declaration through PF-SCHEMA-OK? / PF-NODE-KIND?; SUM
\ variant payloads had no equivalent authoritative whole-tree check, so the
\ invariant "every payload parameter is within the declaring family's arity" held
\ only as an emergent side effect of the per-token TDECL-LETTER arity gate
\ (sumtype.f). TFC-SCH-TERM (the construct/MATCH consumer) indexes its minted-var
\ scratch by the parameter index with no bounds guard, so a payload parameter
\ beyond arity would be a silent out-of-bounds read rather than a fail-closed
\ reject; this walk makes the arity invariant a structural whole-tree property
\ enforced at declaration and keeps the parse gate as the front line.
\
\ It is deliberately NOT PF-NODE-KIND? reused verbatim: PF-NODE-KIND?'s SCH-APP
\ branch also requires each argument's physical kind to match the applied
\ family's parameter kind (PF-KIND-OK?), which would reject a valid nested-layout
\ sum payload such as option<result<n,f>> (a layout-kind family argument in a
\ cell parameter slot, which the checker's LOGHID width coercion makes sound).
\ Sum payload validation is purely the parameter-index/arity concern, so this
\ focused walk descends the same node kinds -- SCH-APP arguments, SCH-PTR
\ pointee, SCH-QUOT effect-side rows (SCH-ROW elements) -- and resolves every
\ SCH-PARAM index against the declaring family's arity, returning the first
\ out-of-arity index or -1 when the whole subtree is in range (indices are 0..25,
\ so -1 is an unambiguous "in range" sentinel).
defer TFAM-SCH-ROW-ARITY ( n n -- n )   \ ( owner rownode -- bad-idx ) forward ref for the SCH-QUOT recursion

: TFAM-SCH-ARITY ( n n -- n ) {: owner:n node:n :}   \ first out-of-arity SCH-PARAM index in node's subtree, else -1
   node SCHEMA-NODE-OK? 0= IF -1 EXIT THEN
   node SCHEMA-PARAM? IF
      node SCHEMA-A@ dup owner TFAM-ARITY@ < IF drop -1 THEN EXIT
   THEN
   node SCHEMA-PTR? IF owner node SCHEMA-A@ RECURSE EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-C@ {: cnt:n :}
      node SCHEMA-B@ {: start:n :}
      0 BEGIN dup cnt < WHILE
         start over + SCHEMA-ROOT@ owner swap RECURSE
         dup 0 >= IF nip EXIT THEN drop
         1 +
      REPEAT drop -1 EXIT
   THEN
   node SCHEMA-QUOT? IF
      0 BEGIN dup SCH-QUOT-ROWS < WHILE
         node over SCHEMA-QUOT-ROW@ owner swap TFAM-SCH-ROW-ARITY
         dup 0 >= IF nip EXIT THEN drop
         1 +
      REPEAT drop -1 EXIT
   THEN
   -1 ;

: TFAM-SCH-ROW-ARITY-IMPL ( n n -- n ) {: owner:n rownode:n :}   \ first out-of-arity index in a SCH-ROW's elements, else -1
   rownode SCHEMA-ROW? 0= IF -1 EXIT THEN
   rownode SCHEMA-ROW-COUNT@ {: cnt:n :}
   0 BEGIN dup cnt < WHILE
      rownode over SCHEMA-ROW-ELEM@ owner swap TFAM-SCH-ARITY
      dup 0 >= IF nip EXIT THEN drop
      1 +
   REPEAT drop -1 ;
: TFAM-SCH-ROW-ARITY-INSTALL ( -- ) [: TFAM-SCH-ROW-ARITY-IMPL ;] is TFAM-SCH-ROW-ARITY ;
TFAM-SCH-ROW-ARITY-INSTALL

: PF-RANGE-OK? ( n n -- bool ) {: off:n len:n :}
   off 0 >= len 0 > and IF off PF-MAX-N len - <= ELSE RES-FALSE THEN ;
: PF-POW2? ( n -- bool ) {: n:n :}
   n 0 > IF n n 1 - and 0= ELSE RES-FALSE THEN ;
: PF-RANGE-OVERLAP? ( n n n n -- bool ) {: a:n au:n b:n bu:n :}
   a b bu + < b a au + < and ;
: PF-CELL-BYTES ( n -- n ) {: n:n :}
   n 0 < n PF-MAX-N CELL / > or IF E-PF-LAYOUT throw THEN
   n cells ;
: PF-OVERLAP? ( n n n n n n -- bool )
   {: fam:n var:n slot:n cellsn:n boff:n bytesn:n :}
   0 TF-I !
   BEGIN TF-I @ PF-N @ < WHILE
      TF-I @ PF-ROW {: r:ptr :}
      fam var r PF-ROW-OWNER? IF
         slot cellsn r PF.SLOT @ r PF.CELLS @ PF-RANGE-OVERLAP? IF RES-TRUE EXIT THEN
         boff bytesn r PF.BYTE-OFF @ r PF.BYTES @ PF-RANGE-OVERLAP? IF RES-TRUE EXIT THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   RES-FALSE ;
: PF-LAYOUT-REQUIRE ( n n n n n n n n -- )
   {: fam:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   flags PF-FLAGS-NONE <> IF E-PF-FLAGS throw THEN
   slot cellsn PF-RANGE-OK? 0= IF E-PF-LAYOUT throw THEN
   boff bytesn PF-RANGE-OK? 0= IF E-PF-LAYOUT throw THEN
   al PF-POW2? 0= IF E-PF-LAYOUT throw THEN
   boff al mod 0 <> IF E-PF-LAYOUT throw THEN
   sch PF-SCHEMA-WIDTH cellsn <> IF E-PF-LAYOUT throw THEN
   fam TFAM-LAYOUT-POLICY@ CASE
      TL-STACK-CELL-TAG OF
         slot PF-CELL-BYTES boff <> IF E-PF-LAYOUT throw THEN
         cellsn PF-CELL-BYTES bytesn <> IF E-PF-LAYOUT throw THEN
         al CELL <> IF E-PF-LAYOUT throw THEN
      ENDOF
      TL-PACKED-TAG OF
         slot PF-CELL-BYTES boff <> IF E-PF-LAYOUT throw THEN
         cellsn PF-CELL-BYTES bytesn <> IF E-PF-LAYOUT throw THEN
         al CELL <> IF E-PF-LAYOUT throw THEN
      ENDOF
      TL-NICHE OF E-PF-LAYOUT throw ENDOF
      TL-BOXED OF E-PF-LAYOUT throw ENDOF
      TL-CUSTOM OF E-PF-LAYOUT throw ENDOF
      E-PF-LAYOUT throw
   ENDCASE ;
: PF-DUP? ( n n ptr u8 n -- bool ) {: fam:n var:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ PF-N @ < WHILE
      TF-I @ PF-ROW {: r:ptr :}
      fam var r PF-ROW-OWNER? IF
         r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= IF RES-TRUE EXIT THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   RES-FALSE ;

package TYPE-FIELD-OWNER

public

: ADD ( n n n ptr u8 n n n n n n n n -- n )
   {: tx:n fam:n var:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   tx STATE-OPEN TX-STATE-REQUIRE
   TX-OPEN-MARKS-REQUIRE
   fam var PF-OWNER-OK? 0= IF E-PF-OWNER throw THEN
   na nu PF-NAME-REQUIRE
   fam var na nu PF-DUP? IF E-TFAM-DUP throw THEN
   fam sch PF-SCHEMA-OK? 0= IF E-PF-SCHEMA throw THEN
   fam sch slot cellsn boff bytesn al flags PF-LAYOUT-REQUIRE
   fam var slot cellsn boff bytesn PF-OVERLAP? IF E-PF-LAYOUT throw THEN
   PF-ENSURE
   na nu TF-INTERN {: noff:n :}
   PF-N @ {: id:n :}
   id PF-REC * PF-BASE + {: r:ptr :}
   fam r PF.FAM !   var r PF.VAR !
   noff r PF.NAME-OFF !   nu r PF.NAME-U !   sch r PF.SCH !
   slot r PF.SLOT !   cellsn r PF.CELLS !
   boff r PF.BYTE-OFF !   bytesn r PF.BYTES !
   al r PF.ALIGN !   flags r PF.FLAGS !
   id 1 + PF-N !
   tx ;

private
get-current prot-wid-add
public
get-current prot-wid-add
private

;package

package TYPE-FIELD

public

: COUNT ( -- n ) PF-N@ ;
: TX-DEPTH ( -- n ) PF-TX-DEPTH @ ;
: NO-VARIANT ( -- n ) PF-NO-VARIANT ;
: FIND ( n n ptr u8 n -- n bool ) PF-FIND ;
: EACH ( n n n -- n bool ) PF-EACH ;
: FAMILY@ ( n -- n ) PF-FAM@ ;
: VARIANT@ ( n -- n ) PF-VAR@ ;
: NAME$ ( n -- ptr u8 n ) PF-NAME$ ;
: SCHEMA@ ( n -- n ) PF-SCH@ ;
: SLOT@ ( n -- n ) PF-SLOT@ ;
: CELLS@ ( n -- n ) PF-CELLS@ ;
: BYTE-OFF@ ( n -- n ) PF-BYTE-OFF@ ;
: BYTES@ ( n -- n ) PF-BYTES@ ;
: ALIGN@ ( n -- n ) PF-ALIGN@ ;
: FLAGS@ ( n -- n ) PF-FLAGS@ ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package

\ Concrete schema linearity. Family arguments are checker terms and are
\ accounted by LAYOUT-MAYBE-LINEAR? / LAYOUT-LINEAR-COUNT; this metadata walk
\ accounts the other ownership source: concrete linear nodes embedded in sum
\ variants or product fields. Pointer and quotation nodes are non-owning
\ boundaries. An application recursively checks both its concrete arguments
\ and the referenced family's schemas, so nested field families cannot launder
\ a linear value. The declaration graph is acyclic outside pointer boundaries.
defer TFCL-NODE-XT ( n -- bool )

: TFAM-CONCRETE-LINEAR? ( n -- bool ) {: fam:n :}
   fam TFAM-PRODUCT? IF
      0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
         fam TFAM-FLD-START@ over + PF-ROW PF.SCH @ SCHEMA-ROOT@ TFCL-NODE-XT IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      RES-FALSE EXIT
   THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF
      0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
         fam TFAM-VAR-START@ over + {: vid:n :}
         0 BEGIN dup vid SUMV-PAY-N < WHILE
            vid over SUMV-PAY-ROOT SCHEMA-ROOT@ TFCL-NODE-XT IF 2drop RES-TRUE EXIT THEN
            1 +
         REPEAT drop
         1 +
      REPEAT drop
   THEN
   RES-FALSE ;

: TFCL-NODE? ( n -- bool ) {: node:n :}
   node SCHEMA-CON? IF node SCHEMA-A@ CT-LINEAR? EXIT THEN
   node SCHEMA-APP? IF
      0 BEGIN dup node SCHEMA-C@ < WHILE
         node SCHEMA-B@ over + SCHEMA-ROOT@ RECURSE IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      node SCHEMA-A@ TFAM-CONCRETE-LINEAR? EXIT
   THEN
   RES-FALSE ;

: TFCL-NODE-INSTALL ( -- )
   [: TFCL-NODE? ;] is TFCL-NODE-XT
   [: TFAM-CONCRETE-LINEAR? ;] is TFAM-CON-LIN-XT ;
TFCL-NODE-INSTALL

\ ---------------------------------------------------------------------------
\ logical layout records, one per family that has a resolved physical layout.
\ ---------------------------------------------------------------------------
0 cells constant LAY.FAM-OFF
1 cells constant LAY.POLICY-OFF
2 cells constant LAY.SIZE-OFF
3 cells constant LAY.ALIGN-OFF
4 cells constant LAY.TAGW-OFF
5 cells constant LAY-REC
CELL constant LAY-REC-ALIGN
0 constant LAY-REC-PTR-MASK

: LAY.FAM ( ptr a -- ptr a ) LAY.FAM-OFF + ;
: LAY.POLICY ( ptr a -- ptr a ) LAY.POLICY-OFF + ;
: LAY.SIZE ( ptr a -- ptr a ) LAY.SIZE-OFF + ;
: LAY.ALIGN ( ptr a -- ptr a ) LAY.ALIGN-OFF + ;
: LAY.TAGW ( ptr a -- ptr a ) LAY.TAGW-OFF + ;

LAY.FAM-OFF 0 cells TF-LAYOUT=
LAY.POLICY-OFF 1 cells TF-LAYOUT=
LAY.SIZE-OFF 2 cells TF-LAYOUT=
LAY.ALIGN-OFF 3 cells TF-LAYOUT=
LAY.TAGW-OFF 4 cells TF-LAYOUT=
LAY-REC 5 cells TF-LAYOUT=
LAY-REC-ALIGN CELL TF-LAYOUT=
LAY-REC LAY-REC-ALIGN mod 0 TF-LAYOUT=
LAY-REC-PTR-MASK 0 TF-LAYOUT=
0 LAY.FAM LAY.FAM-OFF TF-LAYOUT=
0 LAY.POLICY LAY.POLICY-OFF TF-LAYOUT=
0 LAY.SIZE LAY.SIZE-OFF TF-LAYOUT=
0 LAY.ALIGN LAY.ALIGN-OFF TF-LAYOUT=
0 LAY.TAGW LAY.TAGW-OFF TF-LAYOUT=

4 constant LAY-CAP-INIT
variable LAY-CAP-V   LAY-CAP-INIT LAY-CAP-V !   REG-PROTECT
: LAY-CAP ( -- n ) LAY-CAP-V @ ;
create LAY-A-BOOT   LAY-CAP-INIT LAY-REC * allot   REG-PROTECT
variable LAY-A-P   LAY-A-BOOT LAY-A-P !   REG-PROTECT
: LAY-BASE ( -- ptr a ) LAY-A-P @ ;
variable LAY-N   0 LAY-N !   REG-PROTECT

: LAY-GROW ( n -- ) {: need:n :}
   need LAY-CAP-V @ 2 * max {: nc:n :}
   LAY-A-P  LAY-CAP-V @ LAY-REC *  nc LAY-REC *  REG-GROW1
   nc LAY-CAP-V ! ;
: LAY-ENSURE ( -- )
   LAY-N @ LAY-CAP-V @ < IF exit THEN
   LAY-N @ 1 + LAY-GROW ;
: LAY-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < IF s" tfam: bad layout id" 76 die THEN
   id LAY-N @ >= IF s" tfam: bad layout id" 76 die THEN
   id LAY-REC * LAY-BASE + ;

: LAY-FAM@ ( n -- n ) LAY-REC@ LAY.FAM @ ;
: LAY-POLICY@ ( n -- n ) LAY-REC@ LAY.POLICY @ ;
: LAY-SIZE@ ( n -- n ) LAY-REC@ LAY.SIZE @ ;
: LAY-ALIGN@ ( n -- n ) LAY-REC@ LAY.ALIGN @ ;
: LAY-TAGW@ ( n -- n ) LAY-REC@ LAY.TAGW @ ;
: LAY-N@ ( -- n ) LAY-N @ ;

: LAY-FIND ( n -- n bool ) {: fam:n :}
   0 TF-I !
   BEGIN TF-I @ LAY-N @ < WHILE
      TF-I @ LAY-FAM@ fam = IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: LAY-ADD ( n n n n n -- n ) {: fam:n p:n sz:n al:n tw:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   fam LAY-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   LAY-ENSURE
   LAY-N @ {: id:n :}
   id 1 + LAY-N !
   id LAY-REC@ {: r:ptr :}
   fam r LAY.FAM !   p r LAY.POLICY !   sz r LAY.SIZE !
   al r LAY.ALIGN !   tw r LAY.TAGW !
   id ;

\ ---------------------------------------------------------------------------
\ packed memory ABI descriptor (docs §22.2, policy TL-PACKED-TAG). packed keeps
\ the STACK representation as cells (docs §4/§22.2 - the stack width W is
\ unchanged, identical to stack-cell-tag); it ONLY adds a memory descriptor so a
\ layout value can be marshalled into an ABI-stable buffer (arrays of ADTs, GPU
\ buffers). v1 payloads are cell-kinded (docs §4: slot0..slot(M-1) tag, M cells),
\ so the only field packed narrows is the TAG: a K-variant tag is stored in the
\ smallest of u8/u16/u32/cell holding tags [0,K). Payloads stay CELL-wide (align
\ CELL), so per-field byte offsets are implicit (slot i at byte i*CELL) and need
\ no table; SIZE/ALIGN/TAGW fully specify the v1 ABI (a mixed narrow-payload tier
\ with an explicit offset table is a later refinement). The tag sits AFTER the
\ payload, matching the stack order (slot0..slot(M-1) tag); SIZE is the aligned
\ record stride (align_up so an array is a stride*i walk). Pure compile-time
\ metadata: no heap, no runtime cost. These compute the descriptor for ANY family
\ independent of its declared policy, so the accept sub-slice can compute-then-
\ LAY-ADD; the grammar keeps rejecting POLICY packed-tag until that lands.
: PACKED-ALIGN-UP ( n n -- n ) {: v:n a:n :}   \ round v up to a multiple of pow2 a
   v a 1- + a 1- invert and ;
: PACKED-NARROW ( n -- n )   \ smallest byte width 1/2/4/8 holding tags [0,count); 0 if none
   {: count:n :}
   count 0 <= IF 0 EXIT THEN
   count 256 <= IF 1 EXIT THEN
   count 65536 <= IF 2 EXIT THEN
   count 1 32 lshift <= IF 4 EXIT THEN
   8 ;
: PACKED-TAGW ( n -- n ) {: fam:n :}   \ narrowed tag byte width (0 for tag-less products)
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF 0 EXIT THEN
   fam TFAM-VAR-COUNT@ PACKED-NARROW ;
: PACKED-ALIGN ( n n -- n ) {: pay:n tw:n :}   \ record alignment from payload bytes + tag width
   pay 0 > IF CELL EXIT THEN     \ any cell payload -> cell alignment
   tw 0 > IF tw EXIT THEN        \ tag-only -> tag alignment
   1 ;                           \ defensive empty -> byte
: PACKED-DESC ( n -- n n n ) {: fam:n :}   \ ( fam -- size align tagw ) packed ABI descriptor
   fam PACKED-TAGW {: tw:n :}
   fam TFAM-SLOTS@ CELL * {: pay:n :}
   pay tw PACKED-ALIGN {: al:n :}
   pay tw + al PACKED-ALIGN-UP  al  tw ;

\ ---------------------------------------------------------------------------
\ base-state reset. Refuses to run while a field transaction is live (depth > 0)
\ so an in-flight frame is never discarded out from under its token holder, and
\ deliberately does NOT rewind PF-TX-SERIAL: the transaction-token generation
\ counter stays monotonic for the whole process, so a token minted before a reset
\ can never equal — and therefore never alias — a transaction begun after it.
\ Depth is provably 0 past the guard, so it is not re-cleared here.
\ ---------------------------------------------------------------------------
: TFAM-RESET ( -- )
   PF-TX-DEPTH @ IF E-PF-TX throw THEN   \ live field transaction: reset would discard its frame
   0 SVX-GEN !                           \ every variant row the index points at is going
   0 TFX-READY !                         \ ... and every family row the tail index chains
   0 TFAM-N !   0 TF-STR-U !   0 TF-PK-N !
   0 SUMV-N !   0 PF-N !   0 PF-COMMIT-N !   0 LAY-N !
   -1 FIELD-FAM !     \ field family is de-registered until re-declared, so its id can't dangle
   EXT-FREE-CLEAR ;   \ BTC-7: drop free-extent marks with the families they name
TFAM-RESET

\ ---------------------------------------------------------------------------
\ rollback frame stack (TFAM half of the checker's transactional rollback).
\ Each checker scope/candidate saves the family/variant/field/layout registry
\ high-water marks plus the string-pool and param-kind pool ends; rejecting a
\ scope/candidate pops them so a rejected family declaration leaves no family,
\ variant, field, or layout row and no interned name behind. These registries use
\ scans keyed on (package, tail), so restoring the counters IS entry retirement:
\ SUMV-FIND/PF-FIND/LAY-FIND only scan [0,N), and re-adding under the same name
\ interns fresh at the restored pool end. The two lookups that now go through an
\ index — TFAM-FIND-IN and SUMV-FROM-CTOR-SYM — unchain their rows from that same
\ restore (TFX-RETIRE, SVX-TRUNCATE) so the index retires exactly with the rows.
\ Pushed/popped in lockstep with checker.f's core frame.
\ ---------------------------------------------------------------------------
0 cells constant TFRB.TFAMN-OFF
1 cells constant TFRB.STRU-OFF
2 cells constant TFRB.PKN-OFF
3 cells constant TFRB.SUMVN-OFF
4 cells constant TFRB.PFN-OFF
5 cells constant TFRB.LAYN-OFF
6 cells constant TFRB.PFCOMMITN-OFF
7 cells constant TFRB.PFTXDEPTH-OFF
8 cells constant TF-RBF-REC
CELL constant TF-RBF-REC-ALIGN
0 constant TF-RBF-REC-PTR-MASK

: TFRB.TFAMN ( ptr a -- ptr a ) TFRB.TFAMN-OFF + ;
: TFRB.STRU ( ptr a -- ptr a ) TFRB.STRU-OFF + ;
: TFRB.PKN ( ptr a -- ptr a ) TFRB.PKN-OFF + ;
: TFRB.SUMVN ( ptr a -- ptr a ) TFRB.SUMVN-OFF + ;
: TFRB.PFN ( ptr a -- ptr a ) TFRB.PFN-OFF + ;
: TFRB.LAYN ( ptr a -- ptr a ) TFRB.LAYN-OFF + ;
: TFRB.PFCOMMITN ( ptr a -- ptr a ) TFRB.PFCOMMITN-OFF + ;
: TFRB.PFTXDEPTH ( ptr a -- ptr a ) TFRB.PFTXDEPTH-OFF + ;

TFRB.TFAMN-OFF 0 cells TF-LAYOUT=
TFRB.STRU-OFF 1 cells TF-LAYOUT=
TFRB.PKN-OFF 2 cells TF-LAYOUT=
TFRB.SUMVN-OFF 3 cells TF-LAYOUT=
TFRB.PFN-OFF 4 cells TF-LAYOUT=
TFRB.LAYN-OFF 5 cells TF-LAYOUT=
TFRB.PFCOMMITN-OFF 6 cells TF-LAYOUT=
TFRB.PFTXDEPTH-OFF 7 cells TF-LAYOUT=
TF-RBF-REC 8 cells TF-LAYOUT=
TF-RBF-REC-ALIGN CELL TF-LAYOUT=
TF-RBF-REC TF-RBF-REC-ALIGN mod 0 TF-LAYOUT=
TF-RBF-REC-PTR-MASK 0 TF-LAYOUT=
0 TFRB.TFAMN TFRB.TFAMN-OFF TF-LAYOUT=
0 TFRB.STRU TFRB.STRU-OFF TF-LAYOUT=
0 TFRB.PKN TFRB.PKN-OFF TF-LAYOUT=
0 TFRB.SUMVN TFRB.SUMVN-OFF TF-LAYOUT=
0 TFRB.PFN TFRB.PFN-OFF TF-LAYOUT=
0 TFRB.LAYN TFRB.LAYN-OFF TF-LAYOUT=
0 TFRB.PFCOMMITN TFRB.PFCOMMITN-OFF TF-LAYOUT=
0 TFRB.PFTXDEPTH TFRB.PFTXDEPTH-OFF TF-LAYOUT=

16 constant TF-RBF-CAP-INIT
variable TF-RBF-CAP-V   TF-RBF-CAP-INIT TF-RBF-CAP-V !
create TF-RBF-BOOT   TF-RBF-CAP-INIT TF-RBF-REC * allot
variable TF-RBF-P    TF-RBF-BOOT TF-RBF-P !
: TF-RBF-BASE ( -- ptr a ) TF-RBF-P @ ;
variable TF-RBF-DEPTH   0 TF-RBF-DEPTH !

: TF-RBF-GROW ( -- )
   TF-RBF-CAP-V @ 2 * {: nc:n :}
   TF-RBF-P  TF-RBF-CAP-V @ TF-RBF-REC *  nc TF-RBF-REC *  REG-GROW1
   nc TF-RBF-CAP-V ! ;
: TF-RBF-ENSURE ( -- )
   TF-RBF-DEPTH @ TF-RBF-CAP-V @ < IF exit THEN
   TF-RBF-GROW ;
: TF-RBF-CUR ( -- ptr a ) TF-RBF-DEPTH @ TF-RBF-REC * TF-RBF-BASE + ;
: TF-RBF-TOP ( -- ptr a )
   TF-RBF-DEPTH @ 0= IF E-PF-TX throw THEN
   TF-RBF-DEPTH @ 1 - TF-RBF-REC * TF-RBF-BASE + ;

package CHECKER-DECL-FRAME

: PF-DEPTH= ( ptr n -- )
   TFRB.PFTXDEPTH @ PF-TX-DEPTH @ <> IF E-PF-TX throw THEN ;

: TF-RELEASE ( -- )
   TF-RBF-DEPTH @ 1 - TF-RBF-DEPTH ! ;

: TF-SAVE ( -- )
   TF-RBF-ENSURE
   TF-RBF-CUR {: r:ptr :}
   TFAM-N @ r TFRB.TFAMN !
   TF-STR-U @ r TFRB.STRU !
   TF-PK-N @ r TFRB.PKN !
   SUMV-N @ r TFRB.SUMVN !
   PF-N @ r TFRB.PFN !
   LAY-N @ r TFRB.LAYN !
   PF-COMMIT-N @ r TFRB.PFCOMMITN !
   PF-TX-DEPTH @ r TFRB.PFTXDEPTH !
   TF-RBF-DEPTH @ 1 + TF-RBF-DEPTH ! ;

: TF-RESTORE-TOP ( ptr n -- )
   TF-RELEASE
   {: r:ptr :}
   r TFRB.TFAMN @ TFX-RETIRE             \ unchain the rows before their ids go out of range
   r TFRB.TFAMN @ TFAM-N !
   r TFRB.STRU @ TF-STR-U !
   r TFRB.PKN @ TF-PK-N !
   r TFRB.SUMVN @ SVX-TRUNCATE           \ retire the constructor heads the rows own
   r TFRB.SUMVN @ SUMV-N !
   r TFRB.PFN @ PF-N @ PF-SCRUB       \ scrub product-field rows this rejected declaration retires
   r TFRB.PFN @ PF-N !
   r TFRB.PFCOMMITN @ PF-COMMIT-N !
   r TFRB.LAYN @ LAY-N ! ;

: TF-RESTORE ( -- )
   TF-RBF-TOP PF-DEPTH=
   TF-RBF-TOP TF-RESTORE-TOP ;

: TF-FINALIZE ( -- )
   TF-RBF-TOP PF-DEPTH=
   TF-RELEASE ;

;package

\ TFAM-RBF-SNAP-RESET ( -- ) : snapshot prepare — frames are transient (depth 0
\ at snapshot), so drop any grown arena back to the baked boot store.
: TFAM-RBF-SNAP-RESET ( -- )
   TF-RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   TF-RBF-BOOT TF-RBF-P !
   TF-RBF-CAP-INIT TF-RBF-CAP-V !
   0 TF-RBF-DEPTH ! ;

: PF-TX-SNAP-RESET ( -- )
   PF-TX-DEPTH @ IF s" checker: snapshot inside field transaction" 76 die THEN
   PF-TX-BOOT PF-TX-P !
   PF-TX-CAP-INIT PF-TX-CAP-V ! ;

\ combined registry rollback hooks: one SAVE/RESTORE pair the checker's core
\ RBF-PUSH/POP drives, so TFAM + SCHEMA frames stay in lockstep with core marks.
package CHECKER-DECL-FRAME

: EXT-SAVE ( -- )
   TF-SAVE
   SCHEMA-ROLLBACK-SAVE ;

: EXT-RESTORE ( -- )
   SCHEMA-ROLLBACK-RESTORE
   TF-RESTORE ;

: EXT-FINALIZE ( -- )
   SCHEMA-ROLLBACK-FINALIZE
   TF-FINALIZE ;

: TYPES-READY ( n -- bool ) {: depth:n :}
   TF-RBF-DEPTH @ depth =
   SCH-RBF-DEPTH @ depth = and ;

: TYPES-RESTORE ( -- )
   SCHEMA-ROLLBACK-RESTORE
   TF-RBF-TOP TF-RESTORE-TOP ;

: TYPES-RELEASE ( -- )
   SCHEMA-ROLLBACK-FINALIZE
   TF-RELEASE ;

: INSTALL-TYPES ( -- )
   [: EXT-SAVE ;] is REG-EXT-RB-SAVE-XT
   [: EXT-FINALIZE ;] is REG-EXT-RB-FINALIZE-XT
   [: EXT-RESTORE ;] is REG-EXT-RB-RESTORE-XT
   [: TYPES-READY ;] is TYPES-READY-XT
   [: TYPES-RESTORE ;] is TYPES-RESTORE-XT
   [: TYPES-RELEASE ;] is TYPES-RELEASE-XT ;
INSTALL-TYPES

get-current prot-wid-add
public
get-current prot-wid-add
private

;package

\ PF-PERSIST-CANONICAL ( -- ) : make the product-field capacity byte-canonical
\ before it is baked, so retired/rejected declarations cannot leak observable bytes
\ into snapshot/fixpoint identity. Persist runs at depth 0 (PF-TX-SNAP-RESET/
\ TFAM-RBF-SNAP-RESET assert it), so PF-N == PF-COMMIT-N and everything at or above
\ the committed high-water is dead capacity. Zero the live arena's unused tail
\ [PF-COMMIT-N, PF-CAP); if the arena has grown, the boot buffer is now dead DATA
\ that is still baked verbatim, so zero it whole. Only committed rows [0,PF-COMMIT-N)
\ then carry information; identical logical registries bake byte-identically.
: PF-PERSIST-CANONICAL ( -- )
   PF-COMMIT-N @ PF-CAP-V @ PF-SCRUB
   PF-BASE PF-A-BOOT <> IF
      PF-A-BOOT 0 PF-CAP-INIT PF-REC-CELLS * ARENA-CELLS-ZERO
   THEN ;

\ ---------------------------------------------------------------------------
\ snapshot persist: bake grown TFAM/SUMV/field/layout/param-kind/string stores
\ into image DATA. All record fields are integers or interned offsets, so nothing
\ rebases. Wired into CHECKER-SNAPSHOT-PREPARE through the REG-EXT-PERSIST-XT hook.
\ ---------------------------------------------------------------------------
: TFAM-SNAPSHOT-PERSIST ( -- )
   TF-A-P    TF-A-BOOT    TF-CAP-V @ TF-REC *      REG-PERSIST-BUF drop
   TF-PK-P   TF-PK-BOOT   TF-PK-CAP-V @ cells      REG-PERSIST-BUF drop
   SUMV-A-P  SUMV-A-BOOT  SUMV-CAP-V @ SUMV-REC *  REG-PERSIST-BUF drop
   PF-PERSIST-CANONICAL
   PF-A-P    PF-A-BOOT    PF-CAP-V @ PF-REC *      REG-PERSIST-BUF drop
   LAY-A-P   LAY-A-BOOT   LAY-CAP-V @ LAY-REC *    REG-PERSIST-BUF drop
   TF-STR-P  TF-STR-BOOT  TF-STR-U @               REG-PERSIST-BUF IF
      TF-STR-U @ TF-STR-CAP-V !
   THEN ;

\ TFX-SNAP-RESET ( -- ) : snapshot prepare — the tail index's bucket array is a
\ grown, process-local buffer, so it is dropped back to the baked boot store and
\ marked not-ready. The chains themselves live in the persisted records, but they
\ address buckets that no longer exist, so the restored image must rebuild rather
\ than follow them. Mirrors HIDX-RESET on the checker side.
: TFX-SNAP-RESET ( -- )
   TFX-A-BOOT TFX-A-P !
   TFX-SLOTS-INIT TFX-SLOTS-V !
   0 TFX-CAP !
   0 TFX-READY !
   0 TFX-HI ! ;

\ install the friend-only registry persist hook read by CHECKER-SNAPSHOT-PREPARE.
: REG-EXT-PERSIST ( -- )
   TFAM-SNAPSHOT-PERSIST
   SCHEMA-SNAPSHOT-PERSIST
   TFX-SNAP-RESET              \ tail-index buckets are process-local
   PF-TX-SNAP-RESET            \ field transactions are process-local
   RBF-SNAP-RESET               \ core rollback frames are process-local
   TFAM-RBF-SNAP-RESET          \ TFAM registry rollback frames
   SCHEMA-RBF-SNAP-RESET ;      \ SCHEMA registry rollback frames
: REG-EXT-PERSIST-INSTALL ( -- ) [: REG-EXT-PERSIST ;] is REG-EXT-PERSIST-XT ;
REG-EXT-PERSIST-INSTALL

\ ---------------------------------------------------------------------------
\ Built-in parametric cell families — the checker parser's parametric type
\ constructors, replacing checker.f's old hard-coded PARAM-CTOR? whitelist. Every
\ family is PUBLIC and global (empty package) so a bare `span<...>` resolves via
\ TFAM-RESOLVE from any scope. `ptr` is dual (see its line comment): registered
\ arity-2 for `ptr<space,elem>`, while bare `ptr elem` keeps the MK-PTR special
\ case in checker.f. Registration runs at prefix load in every
\ context (preverify parent + runtime child), so both see identical families.
\ ---------------------------------------------------------------------------
: TFAM-REG-CELL ( ptr u8 n n -- )   \ public global TK-CELL family
   {: na:ptr nu:n ar:n :}
   s" " CHECKER-PACKAGE-PUBLIC na nu ar TK-CELL TFAM-DECL drop ;

\ `ptr` is dual: `ptr<space,elem>` is a parametric pointer (T-PARAM, resolved
\ here), while `ptr elem` (no `<`) stays the MK-PTR plain-pointer special case in
\ checker.f SIG-TYPE. Registered arity 2 matches every `ptr<...>` in the tree.
s" ptr"        2 TFAM-REG-CELL
s" span"       3 TFAM-REG-CELL
s" vspan"      3 TFAM-REG-CELL   \ M10: global span with a PROVEN 16B base alignment (tile-v4a.f)
s" matrix"     4 TFAM-REG-CELL
s" gridctx"    3 TFAM-REG-CELL
s" fanctx"     3 TFAM-REG-CELL
s" idxctx"     4 TFAM-REG-CELL
s" uniqidxctx" 4 TFAM-REG-CELL
s" coopctx"    3 TFAM-REG-CELL
s" rowctx"     3 TFAM-REG-CELL
s" tile"       3 TFAM-REG-CELL
s" vtile"      3 TFAM-REG-CELL   \ M10: vec4 lane tile, DISTINCT from scalar tile<> (tile-v4a.f)
s" acc"        3 TFAM-REG-CELL
s" mmctx"      3 TFAM-REG-CELL
\ Pipelined register-blocked GEMM tile vocabulary (lib/ptx/tile-pipe.f):
\ <t elem, b block, l layout-atom, w mask, p parity var> on the staged families;
\ <t, b, g micro-geometry atom, w> on the micro-tile accumulator.
s" mmstage"    5 TFAM-REG-CELL   \ READY current-parity staged As+Bs tile-pair
s" mmaslice"   5 TFAM-REG-CELL   \ strided A slice of a stage (scalar loads only)
s" mmbslice"   5 TFAM-REG-CELL   \ contiguous 16B-proven B slice (v4 loads legal)
s" mmafrag"    5 TFAM-REG-CELL   \ A operand fragment (4 regs, one k column)
s" mmbfrag"    5 TFAM-REG-CELL   \ B operand fragment (4 regs, one vec4 row)
s" mmracc"     4 TFAM-REG-CELL   \ register-blocked micro-tile accumulator
\ cp.async pipeline-slot typestate (lib/ptx/cpp-slot.f, dot
\ habu-checker-cp-async-6ba788a5): a staged-buffer slot threads
\ pending<p> -> committed<p> -> ready<p> across the double-buffer protocol
\ (p = symbolic buffer parity). The distinct state families make read-before-wait,
\ missing-commit, double-wait, and parity mismatch fail-closed type errors.
s" cpp-pending"   1 TFAM-REG-CELL   \ issued: cp.async copies in flight, not yet committed
s" cpp-committed" 1 TFAM-REG-CELL   \ commit_group closed, wait_group + bar.sync not yet done
s" cpp-ready"     1 TFAM-REG-CELL   \ waited + bar.sync fenced: the staged tile is block-visible
s" attnctx"    3 TFAM-REG-CELL
s" attnacc"    3 TFAM-REG-CELL
s" attn-stage-q"       0 TFAM-REG-CELL
s" attn-stage-score"   0 TFAM-REG-CELL
s" attn-stage-softmax" 0 TFAM-REG-CELL
s" attn-stage-output"  0 TFAM-REG-CELL
s" attn-stage-done"    0 TFAM-REG-CELL
s" uniform"    1 TFAM-REG-CELL
s" rowidx"     1 TFAM-REG-CELL
\ BTC-7 extent-role product/factorization (docs/batch-sequence-design.md §5,
\ docs/extent-substrate.md). `extprod<free,inner>` is the product former: an
\ ORDERED arity-2 cell family so ix<extprod<extb,extt>> types a folded (B,T) row
\ and its ordered args already reject a swapped or mismatched factor on the
\ existing parametric unification (probe test/extent-substrate-probe.f). `redx`
\ is the arity-1 contraction/reduction index: a value of ix<e> re-typed as
\ redx<e> is an axis marked for summation. The checker's free-vs-inner rule
\ (checker.f EXT-REDX-BAD-ARG? at SIG-END-PARAM) rejects redx over a free factor,
\ which is what makes the cross-sequence contraction leak unrepresentable.
\ `ix` is the arity-1 index family the other two are written in terms of: a value
\ of ix<e> is one cell whose phantom argument names the extent it indexes. All
\ three belong to the SAME core-owned substrate and are registered together here.
\ They have to be: the checked retype ix<e> -> redx<e> (lib/type/extent-role.f
\ `>RED`) is an
\ INTRODUCTION into redx, and the checker only authorizes an introduction from
\ the destination family's declaring package (checker.f CAST-OWNER?). While `ix`
\ was declared by `package MAKI` and `redx` by the core, that retype had no legal
\ home in either package and every load of maki/extent.f threw E-CAST-OWNER.
s" ix"         1 TFAM-REG-CELL
s" extprod"    2 TFAM-REG-CELL
s" redx"       1 TFAM-REG-CELL

\ M5: capture the tile/uniform family ids into the checker's barrier-uniformity
\ cells (declared in checker.f). A collective typed ( tile<..> -- uniform<..> )
\ emits bar.sync and is only sound under block-uniform control (checker.f
\ PTX-BARRIER-SIG?/BARRIER-CUR?). Runs in every load context, like the
\ registrations above, so the parent verifier and runtime child agree.
: PTX-FAM-ID ( ptr u8 n -- n )   \ resolve a GLOBAL family name to its id (0 if none)
   {: na:ptr nu:n :}
   s" " na nu TFAM-RESOLVE IF ELSE drop 0 THEN ;
s" tile"    PTX-FAM-ID PTX-TILE-FAM !
s" uniform" PTX-FAM-ID PTX-UNIFORM-FAM !
\ cp.async pipeline-slot barrier ids: the WAIT step ( cpp-committed<p> -- cpp-ready<p> )
\ retires the copy group and bar.sync-fences it, so it composes with the M5
\ barrier model (checker.f PTX-CPWAIT-ROWS?/BARRIER-CUR?) - a WAIT reached under
\ divergent control is not block-uniform and rejects, exactly like BLOCK-MAX.
s" cpp-committed" PTX-FAM-ID PTX-CPCOMMITTED-FAM !
s" cpp-ready"     PTX-FAM-ID PTX-CPREADY-FAM !
\ BTC-7: capture the product/contraction family ids into the checker's role
\ registry cells (declared in checker.f). SIG-END-PARAM reads EXT-REDX-FAM to gate
\ redx<..> forming, and EXT-PROD-FAM to reject contracting a whole product.
s" extprod" PTX-FAM-ID EXT-PROD-FAM !
s" redx"    PTX-FAM-ID EXT-REDX-FAM !

\ Internal VREC field constructor: arity 3, PRIVATE in reserved package "@" (not a
\ spellable user package) so it never resolves from user signatures, while every
\ field<...> term still carries this reserved family-id for identity comparison.
s" @" CHECKER-PACKAGE-PRIVATE s" field" 3 TK-CELL TFAM-DECL FIELD-FAM !

\ ---------------------------------------------------------------------------
\ signature-token resolution (the checker's TFAM-RESOLVE-XT target). On top of
\ TFAM-RESOLVE's package-scope rules this adds the signature-surface concerns:
\ qualified `PKG:tail` references (fold the qualifier — package names are
\ stored case-folded — require a canonical lowercase tail, resolve public rows
\ plus the active package's own private rows), and unqualified ambiguity
\ (E-TFAM-AMBIG) mapped to an unresolved token so the signature rejects with a
\ diagnostic instead of aborting the load.
\ ---------------------------------------------------------------------------
$100 constant TFQ-CAP            \ folded qualifier bytes (CHECKER-PACKAGE-CAP)
create TFQ-BUF TFQ-CAP allot
variable TFQ-U
variable TFQ-TA   variable TFQ-TU     \ qualified tail token
variable TFQ-COLON

: TFQ-FOLD-COPY ( ptr u8 n -- ) {: a:ptr u:n :}   \ folded qualifier -> TFQ-BUF
   u TFQ-CAP > IF s" tfam: qualifier too long" 76 die THEN
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@ CORE-FOLD-C  TFQ-BUF TF-I @ + c!
      TF-I @ 1 + TF-I !
   REPEAT
   u TFQ-U ! ;

\ TFQ-SPLIT? ( ptr u8 n -- bool ) : one non-edge ':' splits qualifier/tail
\ (engine FIND parity); edge or repeated colons never split (and never resolve).
: TFQ-SPLIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   -1 TFQ-COLON !
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@ 58 = IF
         TFQ-COLON @ 0 < 0= IF RES-FALSE EXIT THEN   \ second ':' -> malformed
         TF-I @ TFQ-COLON !
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   TFQ-COLON @ 0 < IF RES-FALSE EXIT THEN
   TFQ-COLON @ 0 = IF RES-FALSE EXIT THEN            \ leading ':'
   TFQ-COLON @ u 1 - = IF RES-FALSE EXIT THEN        \ trailing ':'
   a TFQ-COLON @ TFQ-FOLD-COPY
   a TFQ-COLON @ + 1 + TFQ-TA !
   u TFQ-COLON @ - 1 - TFQ-TU !
   RES-TRUE ;

: TFAM-QUAL-RESOLVE ( ptr u8 n -- n bool ) {: pa:ptr pu:n :}
   TFQ-TA @ TFQ-TU @ TF-CANON? 0= IF 0 RES-FALSE EXIT THEN
   TFQ-BUF TFQ-U @ TFQ-TA @ TFQ-TU @ TFAM-FIND-IN 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-PUBLIC? IF id RES-TRUE EXIT THEN
   TFQ-BUF TFQ-U @ pa pu CORE-STR=CI IF id RES-TRUE EXIT THEN   \ own private rows
   0 RES-FALSE ;

\ TFAM-RESOLVE may throw E-TFAM-AMBIG; a checked `catch` needs a stack-neutral
\ quotation that cannot read locals, so buffer the (pkg,name) args and the
\ (id,flag) result through cells and run the resolve as a `( -- )` quotation.
variable TFSR-PA   variable TFSR-PU   variable TFSR-NA   variable TFSR-NU
variable TFSR-ID   variable TFSR-FLAG
: TFSR-RUN ( -- )
   TFSR-PA @ TFSR-PU @ TFSR-NA @ TFSR-NU @ TFAM-RESOLVE
   TFSR-FLAG !  TFSR-ID ! ;

: TFAM-SIG-RESOLVE ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   na nu TFQ-SPLIT? IF pa pu TFAM-QUAL-RESOLVE EXIT THEN
   pa TFSR-PA !  pu TFSR-PU !  na TFSR-NA !  nu TFSR-NU !
   [: TFSR-RUN ;] catch {: rc:n :}
   rc 0= IF TFSR-ID @ TFSR-FLAG @ EXIT THEN   \ ( id flag ) from the resolver
   rc E-TFAM-AMBIG = IF 0 RES-FALSE EXIT THEN
   rc throw ;

\ ---------------------------------------------------------------------------
\ construct form (item 9, docs §12): resolution + step effect for the checker's
\ reserved `construct family variant` token protocol. The ownership predicate
\ is package identity: the family must live in the ACTIVE checker package (top
\ level owns the global "" package), public or private — cross-package
\ construction never resolves, so private families stay package-sealed and
\ public cross-package callers use the generated constructor words. Only sum
\ and enum kinds construct. The step effect is the generated-constructor call
\ effect built inline from SUMV metadata: payload schema nodes instantiate
\ against one fresh checker var per family parameter (concrete payloads map to
\ themselves), the family output term carries those vars, and CHECKER-STEP
\ applies din/dout with the same unification, diagnostics capture, and linear
\ conservation as any word call. PUSH-LOGICAL keeps declared-sig parity:
\ resolved-arg bundles (incl. every arity-0 family) expand to hidden fields at
\ the step; open-arg parametric results stay one conservative logical cell and
\ expand at the boundary through the LOGHID coercion.
\ ---------------------------------------------------------------------------
26 constant TFC-VAR-CAP          \ internal construct scratch; not the declaration spelling cap
create TFC-VARS TFC-VAR-CAP cells allot
variable TFC-I   variable TFC-J   variable TFC-ROW

: TFC-MINT-VARS ( n -- ) {: ar:n :}       \ one fresh checker var per family param
   ar TFC-VAR-CAP > IF s" tfam: construct arity over cap" 76 die THEN
   0 TFC-I !
   BEGIN TFC-I @ ar < WHILE
      FRESH MK-VAR TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

: TFC-ARGS! ( n -- ) {: term:n :}   \ copy a resolved family term's args into TFC-VARS
   term PARAM>ARGC TFC-VAR-CAP > IF s" tfam: layout arity over cap" 76 die THEN
   0 TFC-I !
   BEGIN TFC-I @ term PARAM>ARGC < WHILE
      term TFC-I @ PARAM>ARG  TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

\ Fold one quotation effect side (a SCH-ROW node) onto a shared base row: each
\ element type node becomes a checker term (TFC-SCH-TERM, mutually recursive so a
\ nested quotation element resolves) and is pushed in declaration order (element 0
\ deepest), matching the checker's PSTACK/SIG-PARSE-QUOT bottom->top fold. Deferred
\ because the SCH-QUOT case of TFC-SCH-TERM calls it before its impl is defined.
defer TFC-QUOT-ROW ( n n -- n )   \ ( rownode base -- row )

: TFC-SCH-TERM ( n -- n ) {: node:n :}    \ payload schema node -> checker type term
   node SCHEMA-PARAM? IF node SCHEMA-A@ cells TFC-VARS + @ EXIT THEN
   node SCHEMA-CON?   IF node SCHEMA-A@ MK-CON EXIT THEN
   node SCHEMA-PTR?   IF node SCHEMA-A@ RECURSE MK-PTR EXIT THEN
   node SCHEMA-APP? IF
      PARAM-SCR-N @ {: base:n :}
      0 BEGIN dup node SCHEMA-C@ < WHILE
         node SCHEMA-B@ over + SCHEMA-ROOT@ RECURSE PARAM-SCR+
         1 +
      REPEAT drop
      node SCHEMA-A@ {: fam:n :}
      base fam TFAM-NAME$ fam MK-PARAM EXIT
   THEN
   node SCHEMA-QUOT? IF                       \ xt-carrying payload: din/dout share a
      FRESH MK-ROW {: dbase:n :}              \ data base, rin/rout a return base (row-poly)
      FRESH MK-ROW {: rbase:n :}
      node SCHEMA-QUOT-DIN@  dbase TFC-QUOT-ROW {: din:n :}
      node SCHEMA-QUOT-DOUT@ dbase TFC-QUOT-ROW {: dout:n :}
      node SCHEMA-QUOT-HASR@ 0= 0= IF
         node SCHEMA-QUOT-RIN@  rbase TFC-QUOT-ROW {: rin:n :}
         node SCHEMA-QUOT-ROUT@ rbase TFC-QUOT-ROW {: rout:n :}
         din dout rin rout MK-QUOT EXIT
      THEN
      din dout rbase rbase MK-QUOT EXIT       \ no return clause: neutral rin = rout
   THEN
   s" tfam: unsupported construct payload schema" 76 die ;

: TFC-QUOT-ROW-IMPL ( n n -- n ) {: rownode:n base:n :}
   base 0                                                \ ( row j )
   BEGIN dup rownode SCHEMA-ROW-COUNT@ < WHILE            \ ( row j )
      dup rownode swap SCHEMA-ROW-ELEM@ TFC-SCH-TERM      \ ( row j term )
      swap >r                                             \ ( row term ) R: j
      swap PUSH-LOGICAL                                   \ ( row' )     R: j
      r> 1 +                                              \ ( row' j+1 )
   REPEAT drop ;
: TFC-QUOT-ROW-INSTALL ( -- ) [: TFC-QUOT-ROW-IMPL ;] is TFC-QUOT-ROW ;
TFC-QUOT-ROW-INSTALL

\ Payload transport uses the canonical logical-value seam. PUSH-LOGICAL expands
\ every closed layout to its hidden physical fields, including a width-one enum,
\ while scalar, pointer, and open terms retain their ordinary one-cell form.
\ Construct and MATCH therefore share the same representation as signatures.
: TFC-PUSH-PAY ( n n -- n ) PUSH-LOGICAL ;
: TFC-PAY-ROW ( n n -- n ) {: vid:n row0:n :}
   row0 TFC-ROW !
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-PAY-N < WHILE
      vid TFC-J @ SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-SCH-TERM
      TFC-ROW @ TFC-PUSH-PAY TFC-ROW !
      TFC-J @ 1 + TFC-J !
   REPEAT
   TFC-ROW @ ;

: TFC-FAM-TERM ( n -- n ) {: fam:n :}     \ family<v0,..> output term over the minted vars
   PARAM-SCR-N @ {: base:n :}
   0 TFC-I !
   BEGIN TFC-I @ fam TFAM-ARITY@ < WHILE
      TFC-I @ cells TFC-VARS + @ PARAM-SCR+
      TFC-I @ 1 + TFC-I !
   REPEAT
   base fam TFAM-NAME$ fam MK-PARAM ;

\ layout-cap slice 4/5 (dot habu-checker-capability-layout-9b8540bd): width-aware
\ construct/MATCH lowering. The checker records, per genuinely-wide CLOSED instantiation
\ (a multi-cell layout arg, flat arity-0 OR nested named application), one
\ extra-pad fact keyed at the construct/ctor-call/`of` token: w = instantiated_pads -
\ declared_pads, flagged WF-XPAD-FLAG so pass-2 fires even when the difference is 1.
\ Pass 2 (native EM-ADT-CON-VAR / EM-COMPILE-CALL / EM-ADT-MATCH-OF, gforth mirror)
\ adds those extra zero cells so the physical bundle matches the arg-aware width. A
\ layout-cap slice 5 flips the flat-only gate to a recursive WIDTH-STABILITY check:
\ a nested named instantiation (option<result<n,pkg:prod>>) lowers correctly under
\ the SAME extra-pad model because every width site (TFAM-INST-WIDTH@, TFC-VAR-
\ PAYCELLS, famterm T-WIDTH) already recurses through the arg tree — the inner
\ bundle is constructed with its OWN extra pads at its own site, and the outer
\ construct/match only adds the outer delta. The one soundness requirement is that
\ every width be CLOSED (no unresolved type/row var below a family, pointer, or
\ quotation): an open inner var (option<result<n,a>>) has an unstable width and
\ MUST stay fail-closed, so TYPE-CLOSED? rejects it and CONSTRUCT-WIDE-STAGED-
\ REJECT / declared-width match hold. The checker owns that one closure authority.
: TFC-VAR-PAYCELLS ( n -- n ) {: vid:n :}   \ sum of instantiated payload cell widths for a variant
   0
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-PAY-N < WHILE
      vid TFC-J @ SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-SCH-TERM T-WIDTH +
      TFC-J @ 1 + TFC-J !
   REPEAT ;

\ Extra pads = instantiated pads - declared pads, where instantiated pads =
\ instantiated payload SLOTS - instantiated payload cells. The payload slots are the
\ bundle width MINUS the tag cell — but only a TAGGED family (SUM/ENUM) carries a tag;
\ a STRUCTURE/PRODUCT is TAGLESS, so its whole width is payload. Subtracting a tag cell
\ from a tagless family is the tagless-family regression: scg<scinr> (a parametric STRUCTURE at a width-2
\ leaf) has width 3 all payload, but `width - 1` under-counts the slots to 2 and reports
\ a spurious extra = -1, rejecting a value the width-aware lowering handles perfectly.
\ TFC-TAG-CELLS keys the subtraction off the registry family kind, so a product computes
\ extra = 0 and certifies with no exemption.
\ A POSITIVE extra means the wide instantiation needs MORE zero cells than the declared
\ family width reserves; add-only pass 2 supplies them (WF-XPAD-FLAG fact). A genuinely
\ NEGATIVE extra (a tagged SUM variant whose instantiated payload needs FEWER cells than
\ the declared family width reserves — the widest-DECLARED variant is not the
\ widest-INSTANTIATED one, OR a non-widest variant grows more than the widest does) is a
\ real contradiction: pass-1 keys pads off the DECLARED width and emits MORE pad cells
\ than the certified instantiated width has, and the add-only pass-2 fact cannot REMOVE
\ cells to correct it. The certified width and the only possible lowering permanently
\ DISAGREE, so no sound construct/MATCH of that variant exists on the current engine.
\ Such a width contradiction must NEVER certify — reject it unconditionally, including a
\ CHECK-CANDIDATE probe. SIGNED pass-2 corrections (a native emitter that removes cells,
\ not only adds) would make these lowerable; that capability is tracked by dot
\ habu-signed-pass-2-4fc2b960 and will flip these rejects to exact-width construct/MATCH.
: TFC-XPAD-NARROW-REJECT ( -- )   \ certified instantiated width contradicts add-only lowering: never certify
   0 OK ! -1 FAILSET ! ;

: TFC-TAG-CELLS ( n -- n )   \ layout tag cells for a family: 1 for a tagged SUM/ENUM, 0 for a tagless PRODUCT/STRUCTURE
   dup TFAM-SUM? IF drop 1 EXIT THEN
   TFAM-ENUM? IF 1 ELSE 0 THEN ;

: TFC-CON-XPAD-RECORD ( n n n -- ) {: fam:n vid:n famterm:n :}   \ record the wide construct's extra-pad fact, or fail closed on a genuine narrower-than-declared contradiction
   famterm T-WIDTH fam TFC-TAG-CELLS -          \ instantiated payload slots (subtract the tag cell only for a tagged family; a tagless product subtracts none)
   vid TFC-VAR-PAYCELLS -                        \ - instantiated payload cells = instantiated pads
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -        \ - declared pads = extra pads
   {: extra:n :}
   extra 0 > IF 0 fam 0 extra WF-XPAD-FLAG WF-ADD-FULL EXIT THEN
   extra 0 < IF TFC-XPAD-NARROW-REJECT THEN ;   \ genuinely narrower than declared: add-only lowering cannot remove the surplus (until signed pass-2, dot habu-signed-pass-2-4fc2b960)

\ MATCH only ever sees a TAGGED family: TFAM-MATCH-FAM and TFL-MATCH-FAM? both reject a
\ non-SUM/ENUM family (MD-FAM-KIND) before any arm is recorded, and a STRUCTURE UNMAKE
\ takes a separate field-projection path, not this one. So the tag cell is always present
\ here and `rt T-WIDTH 1 -` needs no kind guard.
: TFAM-MATCH-XPAD-RECORD ( n n -- ) {: vid:n term:n :}   \ record a wide MATCH arm's extra-pad fact, or fail closed on a genuine narrower-than-declared arm
   term T-RES {: rt:n :}
   rt TYPE-CLOSED? 0= IF EXIT THEN               \ open nested term/row: leave declared width, stay fail-closed
   rt T-WIDTH 1 -                                \ instantiated payload slots (MATCH families are always tagged: subtract the one tag cell)
   vid TFC-VAR-PAYCELLS -                        \ - instantiated payload cells = instantiated pads
   vid SUMV-FAM@ TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -   \ - declared pads = extra pads
   {: extra:n :}
   extra 0 > IF 0 vid SUMV-FAM@ 0 extra WF-XPAD-FLAG WF-ADD-FULL EXIT THEN
   extra 0 < IF TFC-XPAD-NARROW-REJECT THEN ;   \ genuinely narrower-than-declared arm: declared-width unpack would skip a pad the bundle lacks (until signed pass-2, dot habu-signed-pass-2-4fc2b960)

: TFAM-ACTIVE-PKG$ ( -- ptr u8 n )        \ authenticated package ("" at top level)
   CHECKER-AUTH-PACKAGE$ ;

: TFAM-CONSTRUCT-FAM ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ folded family token -> id
   TFAM-ACTIVE-PKG$ na nu TFAM-FIND-IN 0= IF drop MD-CON-FAM MDIAG! 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-CON-KIND MDIAG! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

\ TFC-CONSTRUCT-STEP-VID ( fam vid -- ) : apply the inline generated-constructor
\ effect for a resolved (family,variant). One fresh checker var per family param,
\ then — bidirectionally — the concrete args named by the declared output are
\ recovered over those vars (CONSTRUCT-DECL-LAYOUT), so a DIRECT closed layout
\ arg makes the payload input and the layout-bundle output PUSH-LOGICAL-expand to
\ its representation even at width 1. Open/linear/scalar/pointer args keep the
\ fresh var and ordinary boundary coercion. Shared by the reserved `construct`
\ token and the generated-constructor CALL (TFAM-CTOR-STEP?).
: TFC-CONSTRUCT-STEP-VID ( n n -- ) {: fam:n vid:n :}
   fam TFAM-ARITY@ TFC-MINT-VARS
   fam CONSTRUCT-DECL-LAYOUT {: dt:n seeded:bool :}
   seeded IF dt TFC-ARGS! THEN
   FRESH MK-ROW {: base:n :}
   vid base TFC-PAY-ROW {: din:n :}
   fam TFC-FAM-TERM {: famterm:n :}
   seeded IF                                           \ layout-cap slice 4/5: width-aware lowering for a CLOSED (stable-width) instantiation, incl. nested
      dt TYPE-CLOSED? IF fam vid famterm TFC-CON-XPAD-RECORD THEN
   THEN
   famterm base PUSH-LOGICAL {: dout:n :}
   din dout CHECKER-STEP
   seeded IF
      dt TYPE-CLOSED? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN   \ open nested term/row: stay staged fail-closed
   THEN ;

: TFAM-CONSTRUCT-STEP ( ptr u8 n n -- bool ) {: na:ptr nu:n fam:n :}
   fam na nu SUMV-FIND 0= IF drop MD-CON-VAR MDIAG! RES-FALSE EXIT THEN
   {: vid:n :}
   fam vid TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ TFAM-CTOR-STEP? ( sym -- bool ) : a generated-constructor CALL whose stored
\ var effect cannot absorb a direct logical-layout argument. Reverse the resolved
\ word symbol to its variant; if CONSTRUCT-DECL-LAYOUT finds an eligible declared
\ output, apply the bidirectionally seeded step and report handled. Otherwise
\ report unhandled so DO-TOK runs the ordinary word call.
: SUMV-FROM-CTOR-SYM ( n -- n bool ) {: sym:n :}   \ constructor word symbol -> variant id
   sym 0 <= IF 0 RES-FALSE EXIT THEN
   SVX-ENSURE
   sym SVX@ dup 0= IF RES-FALSE EXIT THEN          \ 0 head = no variant with this symbol
   1 - RES-TRUE ;
: TFAM-CTOR-STEP? ( n -- bool ) {: sym:n :}
   sym SUMV-FROM-CTOR-SYM 0= IF drop RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam CONSTRUCT-DECL-LAYOUT nip 0= IF RES-FALSE EXIT THEN
   fam vid TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ ---------------------------------------------------------------------------
\ MATCH resolution + payload instantiation (item 9 slice 3, docs §14). MATCH
\ resolution follows SIGNATURE scope, not construct's owner-only rule:
\ eliminability = nameability. You may match any family you could name in a
\ stack signature (own package private+public, else the unique public family,
\ qualified PKG:tail included), because a value of that family can only reach
\ you through such a signature — private families stay unmatchable outside
\ their package by unnameability. Only sum/enum kinds match. The branch payload
\ row instantiates the variant's schema against the SCRUTINEE's recovered arg
\ terms (copied into the TFC scratch vars, consumed immediately at OF — no
\ liveness across tokens, so construct and nested matches may interleave).
\ ---------------------------------------------------------------------------
: TFAM-MATCH-FAM ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ folded family token
   TFAM-ACTIVE-PKG$ na nu TFAM-SIG-RESOLVE 0= IF drop MD-FAM-UNKNOWN MDIAG! 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-FAM-KIND MDIAG! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFAM-MATCH-VARIANT ( ptr u8 n n -- n bool ) {: na:ptr nu:n fam:n :}
   fam na nu SUMV-FIND ;

: TFAM-MATCH-PAY ( n n n -- n ) {: vid:n term:n row:n :}   \ variant payload onto row
   term T-RES TFC-ARGS!
   vid term TFAM-MATCH-XPAD-RECORD            \ layout-cap slice 4: wide arm records its extra-pad lowering fact
   vid row TFC-PAY-ROW ;

\ --- field projection (dot habu-checker-type-structure). The registry-reflection
\ half of the checker's FIELD-PROJ-XT hook: given a committed field id, the baked
\ byte offset from the accessor body, and the resolved family term of the input
\ pointer (`ptr family<args>`), validate the projection against the committed
\ layout and return the instantiated field type. The field id is the sole
\ authority; the byte offset, family, byte extent, role, and schema all derive
\ from it via TYPE-FIELD reflection, so a forged offset, an offset past the family
\ width, a foreign family, a wrong generic arity, or a non-addressable field role
\ each fail closed. TFC-ARGS!/TFC-SCH-TERM instantiate the field's schema over the
\ input family's args, so a generic field yields the caller's substituted type.
\ Lives here because the field/schema registries and TFC-SCH-TERM (which
\ checker.f loads before) are this module's; the checker reaches it only through
\ the sealed friend hook, exactly like TFAM-MATCH-PAY / TFAM-CONSTRUCT-STEP.
variable FPRJ-TERM   variable FPRJ-OK
variable FPRJ-FID    variable FPRJ-OFF   variable FPRJ-FAM
\ Inner validation reads the request from the FPRJ-* cells (so the caught
\ quotation stays stack-neutral for `catch`), may throw E-PF-ID on an uncommitted
\ field id, and publishes the instantiated field type + verdict into FPRJ-TERM /
\ FPRJ-OK.
: TFAM-FIELD-PROJ-DO ( -- )
   0 FPRJ-OK !  0 FPRJ-TERM !
   FPRJ-FID @ TYPE-FIELD:FAMILY@ {: ffam:n :}
   ffam FPRJ-FAM @ PARAM>FAM <> IF EXIT THEN                    \ field not owned by the input family
   FPRJ-OFF @ FPRJ-FID @ TYPE-FIELD:BYTE-OFF@ <> IF EXIT THEN   \ baked offset disagrees with the committed offset (forged access)
   FPRJ-FID @ TYPE-FIELD:FLAGS@ PF-FLAGS-NONE <> IF EXIT THEN   \ non-addressable field role (niche/boxed/custom): no projection
   FPRJ-FAM @ PARAM>ARGC ffam TFAM-ARITY@ <> IF EXIT THEN       \ generic arity mismatch
   FPRJ-OFF @ FPRJ-FID @ TYPE-FIELD:BYTES@ +  ffam TFAM-SLOTS@ CELL *  > IF EXIT THEN   \ field extent past the family width
   FPRJ-FAM @ TFC-ARGS!
   FPRJ-FID @ TYPE-FIELD:SCHEMA@ SCHEMA-ROOT@ TFC-SCH-TERM FPRJ-TERM !   \ root index -> node -> instantiate the field schema over the input args
   -1 FPRJ-OK ! ;
: TFAM-FIELD-PROJ ( n n n -- n bool )   \ fid off famterm -- fieldterm ok
   FPRJ-FAM !  FPRJ-OFF !  FPRJ-FID !
   [: TFAM-FIELD-PROJ-DO ;] catch drop           \ E-PF-ID (uncommitted id) -> ok stays 0
   FPRJ-TERM @ FPRJ-OK @ 0= 0= ;                  \ ok as a boolean flag

\ ---------------------------------------------------------------------------
\ item 10 slice 1: compiler-facing lowering surface (docs §16; dot
\ habu-tfam-10-native design A). Pure resolution + metadata for the native
\ construct/MATCH emitters, called by NAME through the engine's C-FIND-GLOBAL
\ friend bridge at the captured token positions: same registry and scope rules
\ as the checker's friend XTs (owner-only construct, signature-scope match),
\ but NO diagnostic latch and NO checker-row effect — the checker still judges
\ the definition at `;` through its own construct/MATCH machinery. Raw engine
\ token spans fold here (TOKFOLD), so `construct ZRES OK` and the lowercase
\ spelling agree, exactly like checker body tokens. The other metadata the
\ emitters need (SUMV-TAG@, SUMV-PAYCELLS@, TFAM-SLOTS@, TFAM-VAR-COUNT@,
\ TFAM-NAME$) is already named public words above.
\ ---------------------------------------------------------------------------
: TFL-SUMKIND? ( n -- bool ) {: id:n :}   \ constructible/matchable kind
   id TFAM-SUM? id TFAM-ENUM? or ;

: TFL-FOLD$ ( ptr u8 n -- ptr u8 n )      \ fold a raw engine token (shared TKF buffer)
   TOKFOLD drop TKF TKFU @ ;

: TFL-CON-FAM? ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ owner-only scope (docs §12)
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-FIND-IN 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFL-MATCH-FAM? ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ signature scope (docs §14)
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFL-VAR? ( ptr u8 n n -- n bool ) {: na:ptr nu:n fam:n :}   \ variant in fam -> vid
   fam na nu TFL-FOLD$ SUMV-FIND ;

: TFL-VPADS ( n n -- n ) {: fam:n vid:n :}   \ zero pads M-p for a variant's construct
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - ;

: TFL-CVAR? ( ptr u8 n n -- n n bool )   \ variant in a resolved fam -> ( tag pads ok )
   {: va:ptr vu:n fam:n :}
   va vu fam TFL-VAR? 0= IF drop 0 0 RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-TAG@  fam vid TFL-VPADS  RES-TRUE ;

: TFL-CON? ( ptr u8 n ptr u8 n -- n n bool )   \ construct one-shot: -> tag pads ok
   {: fa:ptr fu:n va:ptr vu:n :}
   fa fu TFL-CON-FAM? 0= IF drop 0 0 RES-FALSE EXIT THEN
   {: fam:n :}
   va vu fam TFL-CVAR? ;

\ Install the checker's friend hooks: checker.f loads before this file, so it
\ resolves families / reads arities during signature parsing through these
\ defers. Wrapped in a word so the `[: ;]` quotations compile (`is` binds each
\ defer to the real query word, replacing the old raw-variable stores).
: TFAM-HOOK-INSTALL ( -- )
   [: TFAM-SIG-RESOLVE ;] is TFAM-RESOLVE-XT
   [: TFAM-CTOR-PKG? ;]    is CTOR-PKG?-XT     \ item 8: constructor-package reopen reject
   [: TFAM-CTOR-WORD? ;]   is CTOR-WORD?-XT    \ item 8: generated-word undefine reject
   [: TFAM-CTOR-EXTEND? ;] is CTOR-EXTEND?-XT  \ item 8: closed-package extra-tail reject
   [: TFAM-ARITY@ ;]  is TFAM-ARITY-XT
   [: TFAM-LAYOUT? ;] is TFAM-LAYOUT?-XT   \ item 7: checker reaches the layout kind for its fail-closed guard
   [: TFAM-CELL? ;]   is TFAM-CELL?-XT     \ nominal scalars: checker reaches the cell kind for LAYOUT-BUFFER admission + pointee governance
   [: TFAM-PKG$ ;]    is TFAM-PKG-XT       \ nominal CAST introduction belongs to the declaring package
   [: TFAM-WIDTH@ ;]  is TFAM-WIDTH-XT     \ item 12: checker reads DECLARED logical widths (params-as-cells) for the boot fallback
   [: TFAM-INST-WIDTH@ ;] is TFAM-INST-WIDTH-XT   \ layout-cap slice 1: arg-aware INSTANTIATED width for T-WIDTH / WF fact surface
   [: TFAM-CONSTRUCT-FAM ;]  is CONSTRUCT-FAM-XT   \ item 9: construct family resolution (active package only)
   [: TFAM-CONSTRUCT-STEP ;] is CONSTRUCT-STEP-XT  \ item 9: construct variant resolve + inline constructor effect
   [: TFAM-CTOR-STEP? ;]     is CTOR-STEP-XT        \ layout-cap slice 3: generated-constructor CALL on a multi-cell layout arg routes through the arg-aware step
   [: TFAM-MATCH-FAM ;]     is MATCH-FAM-XT     \ item 9: MATCH family resolution (signature scope)
   [: TFAM-MATCH-VARIANT ;] is MATCH-VAR-XT     \ item 9: MATCH branch variant resolve
   [: SUMV-TAG@ ;]          is MATCH-VTAG-XT    \ item 9: variant id -> declaration-order tag (bitset index)
   [: TFAM-VAR-COUNT@ ;]    is MATCH-VCOUNT-XT  \ item 9: exhaustiveness domain size
   [: TFAM-MATCH-PAY ;]     is MATCH-PAY-XT     \ item 9: branch payload row from the scrutinee's args
   [: TFAM-FIELD-PROJ ;]    is FIELD-PROJ-XT ;  \ dot habu-checker-type-structure: instantiated field type for the FAMILY:FIELD projection window
TFAM-HOOK-INSTALL
