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

variable TF-I                 \ private scan/copy index
variable TF-PUB               \ private first-public-match accumulator (-1 = none)

\ ---------------------------------------------------------------------------
\ shared string pool. Names are interned as byte offsets; offsets stay valid
\ across a pool grow, so no stored offset ever needs rebasing.
\ ---------------------------------------------------------------------------
32 constant TF-STR-INIT          \ small seed byte pool; grows (doubles) on demand
variable TF-STR-CAP-V   TF-STR-INIT TF-STR-CAP-V !
: TF-STR-CAP ( -- n ) TF-STR-CAP-V @ ;
create TF-STR-BOOT   TF-STR-INIT allot
variable TF-STR-P   TF-STR-BOOT TF-STR-P !
: TF-STR ( -- ptr u8 ) TF-STR-P @ ;
variable TF-STR-U   0 TF-STR-U !

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
variable TF-PK-CAP-V   TF-PK-INIT TF-PK-CAP-V !
: TF-PK-CAP ( -- n ) TF-PK-CAP-V @ ;
create TF-PK-BOOT   TF-PK-INIT cells allot
variable TF-PK-P   TF-PK-BOOT TF-PK-P !
: TF-PK-BASE ( -- ptr a ) TF-PK-P @ ;
variable TF-PK-N   0 TF-PK-N !

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
19 cells constant TF-REC
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
TF-REC 19 cells TF-LAYOUT=
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

4 constant TF-CAP-INIT
variable TF-CAP-V   TF-CAP-INIT TF-CAP-V !
: TF-CAP ( -- n ) TF-CAP-V @ ;
create TF-A-BOOT   TF-CAP-INIT TF-REC * allot
variable TF-A-P   TF-A-BOOT TF-A-P !
: TF-BASE ( -- ptr a ) TF-A-P @ ;
variable TFAM-N   0 TFAM-N !

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
   id TFAM-PKG$ pa pu CORE-STR= ;
: TFAM-NAME-MATCH? ( ptr u8 n n -- bool ) {: na:ptr nu:n id:n :}
   id TFAM-NAME$ na nu CORE-STR= ;

\ exact (package,tail) — the qualified-lookup and duplicate-detection primitive.
: TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ TFAM-N @ < WHILE
      pa pu TF-I @ TFAM-PKG-MATCH? IF
         na nu TF-I @ TFAM-NAME-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;

\ PUBLIC family with this tail across packages. A (package,tail) pair is unique
\ (TFAM-DECL rejects DUP), so two public matches are always different packages:
\ that is a genuine unqualified ambiguity and throws E-TFAM-AMBIG rather than
\ silently picking the lowest id. Exactly one public match resolves; none is false.
: TFAM-FIND-PUBLIC ( ptr u8 n -- n bool ) {: na:ptr nu:n :}
   -1 TF-PUB !
   0 TF-I !
   BEGIN TF-I @ TFAM-N @ < WHILE
      TF-I @ TFAM-PUBLIC? IF
         na nu TF-I @ TFAM-NAME-MATCH? IF
            TF-PUB @ 0< IF TF-I @ TF-PUB !
            ELSE E-TFAM-AMBIG throw THEN
         THEN
      THEN
      TF-I @ 1 + TF-I !
   REPEAT
   TF-PUB @ 0< IF 0 RES-FALSE ELSE TF-PUB @ RES-TRUE THEN ;

\ unqualified resolution from the active package: own package (private+public)
\ first, else the unique public family (E-TFAM-AMBIG if two other packages tie).
\ Compaction-hidden `@name` tokens never resolve.
: TFAM-RESOLVE ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   pa pu na nu TFAM-FIND-IN IF RES-TRUE EXIT THEN
   drop
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
variable SUMV-CAP-V   SUMV-CAP-INIT SUMV-CAP-V !
: SUMV-CAP ( -- n ) SUMV-CAP-V @ ;
create SUMV-A-BOOT   SUMV-CAP-INIT SUMV-REC * allot
variable SUMV-A-P   SUMV-A-BOOT SUMV-A-P !
: SUMV-BASE ( -- ptr a ) SUMV-A-P @ ;
variable SUMV-N   0 SUMV-N !

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
: SUMV-PAYCELLS@ ( n -- n ) SUMV-REC@ SV.PAYCELLS @ ;
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
: SUMV-CTOR-SYM! ( n n -- ) swap SUMV-REC@ SV.CTOR-SYM ! ;
: SUMV-CTOR-SYM@ ( n -- n ) SUMV-REC@ SV.CTOR-SYM @ ;

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
\ product fields, keyed by (family-id, field tail).
\ ---------------------------------------------------------------------------
0 cells constant PF.FAM-OFF
1 cells constant PF.NAME-OFF-AT
2 cells constant PF.NAME-U-OFF
3 cells constant PF.SCH-OFF
4 cells constant PF.SLOT-OFF
5 cells constant PF-REC
CELL constant PF-REC-ALIGN
0 constant PF-REC-PTR-MASK

: PF.FAM ( ptr a -- ptr a ) PF.FAM-OFF + ;
: PF.NAME-OFF ( ptr a -- ptr a ) PF.NAME-OFF-AT + ;
: PF.NAME-U ( ptr a -- ptr a ) PF.NAME-U-OFF + ;
: PF.SCH ( ptr a -- ptr a ) PF.SCH-OFF + ;
: PF.SLOT ( ptr a -- ptr a ) PF.SLOT-OFF + ;

PF.FAM-OFF 0 cells TF-LAYOUT=
PF.NAME-OFF-AT 1 cells TF-LAYOUT=
PF.NAME-U-OFF 2 cells TF-LAYOUT=
PF.SCH-OFF 3 cells TF-LAYOUT=
PF.SLOT-OFF 4 cells TF-LAYOUT=
PF-REC 5 cells TF-LAYOUT=
PF-REC-ALIGN CELL TF-LAYOUT=
PF-REC PF-REC-ALIGN mod 0 TF-LAYOUT=
PF-REC-PTR-MASK 0 TF-LAYOUT=
0 PF.FAM PF.FAM-OFF TF-LAYOUT=
0 PF.NAME-OFF PF.NAME-OFF-AT TF-LAYOUT=
0 PF.NAME-U PF.NAME-U-OFF TF-LAYOUT=
0 PF.SCH PF.SCH-OFF TF-LAYOUT=
0 PF.SLOT PF.SLOT-OFF TF-LAYOUT=

4 constant PF-CAP-INIT
variable PF-CAP-V   PF-CAP-INIT PF-CAP-V !
: PF-CAP ( -- n ) PF-CAP-V @ ;
create PF-A-BOOT   PF-CAP-INIT PF-REC * allot
variable PF-A-P   PF-A-BOOT PF-A-P !
: PF-BASE ( -- ptr a ) PF-A-P @ ;
variable PF-N   0 PF-N !

: PF-GROW ( n -- ) {: need:n :}
   need PF-CAP-V @ 2 * max {: nc:n :}
   PF-A-P  PF-CAP-V @ PF-REC *  nc PF-REC *  REG-GROW1
   nc PF-CAP-V ! ;
: PF-ENSURE ( -- )
   PF-N @ PF-CAP-V @ < IF exit THEN
   PF-N @ 1 + PF-GROW ;
: PF-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < IF s" tfam: bad field id" 76 die THEN
   id PF-N @ >= IF s" tfam: bad field id" 76 die THEN
   id PF-REC * PF-BASE + ;

: PF-FAM@ ( n -- n ) PF-REC@ PF.FAM @ ;
: PF-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id PF-REC@ {: r:ptr :}  r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ ;
: PF-SCH@ ( n -- n ) PF-REC@ PF.SCH @ ;
: PF-SLOT@ ( n -- n ) PF-REC@ PF.SLOT @ ;
: PF-N@ ( -- n ) PF-N @ ;

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
   vid SUMV-SCH-START@ {: ss:n :}
   0                                            \ acc
   0 BEGIN dup vid SUMV-SCH-COUNT@ < WHILE       \ ( acc j )
      ss over + SCHEMA-ROOT@ term SCH-NODE-IWIDTH   \ ( acc j wj )
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
      fs over + PF-SCH@ SCHEMA-ROOT@ term SCH-NODE-IWIDTH
      rot + swap
      1 +
   REPEAT drop ;
: TFAM-INST-WIDTH@ ( n -- n ) {: term:n :}      \ instantiated logical width of a resolved layout term
   term PARAM>FAM {: fam:n :}
   fam TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   fam TFAM-PRODUCT? IF term PRODUCT-IWIDTH EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF term SUM-IWIDTH EXIT THEN
   1 ;

: PF-MATCH? ( n ptr u8 n n -- bool ) {: fam:n na:ptr nu:n id:n :}
   id PF-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id PF-NAME$ na nu CORE-STR= ;
: PF-FIND ( n ptr u8 n -- n bool ) {: fam:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ PF-N @ < WHILE
      fam na nu TF-I @ PF-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: PF-ADD ( n ptr u8 n n n -- n ) {: fam:n na:ptr nu:n sch:n slot:n :}
   na nu TF-REQUIRE-CANON
   fam na nu PF-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   PF-ENSURE
   PF-N @ {: id:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + PF-N !
   id PF-REC@ {: r:ptr :}
   fam r PF.FAM !   noff r PF.NAME-OFF !   nu r PF.NAME-U !
   sch r PF.SCH !   slot r PF.SLOT !
   id ;

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
         fam TFAM-FLD-START@ over + PF-SCH@ SCHEMA-ROOT@ TFCL-NODE-XT IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      RES-FALSE EXIT
   THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF
      0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
         fam TFAM-VAR-START@ over + {: vid:n :}
         0 BEGIN dup vid SUMV-SCH-COUNT@ < WHILE
            vid SUMV-SCH-START@ over + SCHEMA-ROOT@ TFCL-NODE-XT IF 2drop RES-TRUE EXIT THEN
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
   [: TFCL-NODE? ;] is TFCL-NODE-XT ;
TFCL-NODE-INSTALL
' TFAM-CONCRETE-LINEAR? TFAM-CON-LIN-XT !

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
variable LAY-CAP-V   LAY-CAP-INIT LAY-CAP-V !
: LAY-CAP ( -- n ) LAY-CAP-V @ ;
create LAY-A-BOOT   LAY-CAP-INIT LAY-REC * allot
variable LAY-A-P   LAY-A-BOOT LAY-A-P !
: LAY-BASE ( -- ptr a ) LAY-A-P @ ;
variable LAY-N   0 LAY-N !

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
\ base-state reset.
\ ---------------------------------------------------------------------------
: TFAM-RESET ( -- )
   0 TFAM-N !   0 TF-STR-U !   0 TF-PK-N !
   0 SUMV-N !   0 PF-N !   0 LAY-N !
   -1 FIELD-FAM ! ;   \ field family is de-registered until re-declared, so its id can't dangle
TFAM-RESET

\ ---------------------------------------------------------------------------
\ rollback frame stack (TFAM half of the checker's transactional rollback).
\ Each checker scope/candidate saves the family/variant/field/layout registry
\ high-water marks plus the string-pool and param-kind pool ends; rejecting a
\ scope/candidate pops them so a rejected family declaration leaves no family,
\ variant, field, or layout row and no interned name behind. These registries use
\ linear scans keyed on (package, tail) — no separate hash index — so restoring
\ the counters IS entry retirement: TFAM-FIND-IN/SUMV-FIND/PF-FIND/LAY-FIND only
\ scan [0,N), and re-adding under the same name interns fresh at the restored
\ pool end. Pushed/popped in lockstep with checker.f's core frame.
\ ---------------------------------------------------------------------------
0 cells constant TFRB.TFAMN-OFF
1 cells constant TFRB.STRU-OFF
2 cells constant TFRB.PKN-OFF
3 cells constant TFRB.SUMVN-OFF
4 cells constant TFRB.PFN-OFF
5 cells constant TFRB.LAYN-OFF
6 cells constant TF-RBF-REC
CELL constant TF-RBF-REC-ALIGN
0 constant TF-RBF-REC-PTR-MASK

: TFRB.TFAMN ( ptr a -- ptr a ) TFRB.TFAMN-OFF + ;
: TFRB.STRU ( ptr a -- ptr a ) TFRB.STRU-OFF + ;
: TFRB.PKN ( ptr a -- ptr a ) TFRB.PKN-OFF + ;
: TFRB.SUMVN ( ptr a -- ptr a ) TFRB.SUMVN-OFF + ;
: TFRB.PFN ( ptr a -- ptr a ) TFRB.PFN-OFF + ;
: TFRB.LAYN ( ptr a -- ptr a ) TFRB.LAYN-OFF + ;

TFRB.TFAMN-OFF 0 cells TF-LAYOUT=
TFRB.STRU-OFF 1 cells TF-LAYOUT=
TFRB.PKN-OFF 2 cells TF-LAYOUT=
TFRB.SUMVN-OFF 3 cells TF-LAYOUT=
TFRB.PFN-OFF 4 cells TF-LAYOUT=
TFRB.LAYN-OFF 5 cells TF-LAYOUT=
TF-RBF-REC 6 cells TF-LAYOUT=
TF-RBF-REC-ALIGN CELL TF-LAYOUT=
TF-RBF-REC TF-RBF-REC-ALIGN mod 0 TF-LAYOUT=
TF-RBF-REC-PTR-MASK 0 TF-LAYOUT=
0 TFRB.TFAMN TFRB.TFAMN-OFF TF-LAYOUT=
0 TFRB.STRU TFRB.STRU-OFF TF-LAYOUT=
0 TFRB.PKN TFRB.PKN-OFF TF-LAYOUT=
0 TFRB.SUMVN TFRB.SUMVN-OFF TF-LAYOUT=
0 TFRB.PFN TFRB.PFN-OFF TF-LAYOUT=
0 TFRB.LAYN TFRB.LAYN-OFF TF-LAYOUT=

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

: TFAM-ROLLBACK-SAVE ( -- )
   TF-RBF-ENSURE
   TF-RBF-CUR {: r:ptr :}
   TFAM-N @ r TFRB.TFAMN !
   TF-STR-U @ r TFRB.STRU !
   TF-PK-N @ r TFRB.PKN !
   SUMV-N @ r TFRB.SUMVN !
   PF-N @ r TFRB.PFN !
   LAY-N @ r TFRB.LAYN !
   TF-RBF-DEPTH @ 1 + TF-RBF-DEPTH ! ;
: TFAM-ROLLBACK-RESTORE ( -- )
   TF-RBF-DEPTH @ 1 - TF-RBF-DEPTH !
   TF-RBF-CUR {: r:ptr :}
   r TFRB.TFAMN @ TFAM-N !
   r TFRB.STRU @ TF-STR-U !
   r TFRB.PKN @ TF-PK-N !
   r TFRB.SUMVN @ SUMV-N !
   r TFRB.PFN @ PF-N !
   r TFRB.LAYN @ LAY-N ! ;

\ TFAM-RBF-SNAP-RESET ( -- ) : snapshot prepare — frames are transient (depth 0
\ at snapshot), so drop any grown arena back to the baked boot store.
: TFAM-RBF-SNAP-RESET ( -- )
   TF-RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   TF-RBF-BOOT TF-RBF-P !
   TF-RBF-CAP-INIT TF-RBF-CAP-V !
   0 TF-RBF-DEPTH ! ;

\ combined registry rollback hooks: one SAVE/RESTORE pair the checker's core
\ RBF-PUSH/POP drives, so TFAM + SCHEMA frames stay in lockstep with core marks.
: REG-EXT-ROLLBACK-SAVE ( -- )
   TFAM-ROLLBACK-SAVE
   SCHEMA-ROLLBACK-SAVE ;
: REG-EXT-ROLLBACK-RESTORE ( -- )
   SCHEMA-ROLLBACK-RESTORE
   TFAM-ROLLBACK-RESTORE ;
: REG-EXT-RB-INSTALL ( -- )
   [: REG-EXT-ROLLBACK-SAVE ;] is REG-EXT-RB-SAVE-XT
   [: REG-EXT-ROLLBACK-RESTORE ;] is REG-EXT-RB-RESTORE-XT ;
REG-EXT-RB-INSTALL

\ ---------------------------------------------------------------------------
\ snapshot persist: bake grown TFAM/SUMV/field/layout/param-kind/string stores
\ into image DATA. All record fields are integers or interned offsets, so nothing
\ rebases. Wired into CHECKER-SNAPSHOT-PREPARE through the REG-EXT-PERSIST-XT hook.
\ ---------------------------------------------------------------------------
: TFAM-SNAPSHOT-PERSIST ( -- )
   TF-A-P    TF-A-BOOT    TF-CAP-V @ TF-REC *      REG-PERSIST-BUF drop
   TF-PK-P   TF-PK-BOOT   TF-PK-CAP-V @ cells      REG-PERSIST-BUF drop
   SUMV-A-P  SUMV-A-BOOT  SUMV-CAP-V @ SUMV-REC *  REG-PERSIST-BUF drop
   PF-A-P    PF-A-BOOT    PF-CAP-V @ PF-REC *      REG-PERSIST-BUF drop
   LAY-A-P   LAY-A-BOOT   LAY-CAP-V @ LAY-REC *    REG-PERSIST-BUF drop
   TF-STR-P  TF-STR-BOOT  TF-STR-U @               REG-PERSIST-BUF IF
      TF-STR-U @ TF-STR-CAP-V !
   THEN ;

\ install the friend-only registry persist hook read by CHECKER-SNAPSHOT-PREPARE.
: REG-EXT-PERSIST ( -- )
   TFAM-SNAPSHOT-PERSIST
   SCHEMA-SNAPSHOT-PERSIST
   RBF-SNAP-RESET               \ core rollback frames are process-local
   TFAM-RBF-SNAP-RESET          \ TFAM registry rollback frames
   SCHEMA-RBF-SNAP-RESET ;      \ SCHEMA registry rollback frames
' REG-EXT-PERSIST REG-EXT-PERSIST-XT !

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
   TFQ-BUF TFQ-U @ pa pu CORE-STR= IF id RES-TRUE EXIT THEN   \ own private rows
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
26 constant TFC-VAR-CAP          \ positional params are letters a..z (TDECL-ARITY-CAP parity)
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
   s" tfam: unsupported construct payload schema" 76 die ;

\ TFC-PUSH-PAY ( term row -- row ) : push one payload term. A genuinely
\ MULTI-CELL (T-WIDTH>1) resolved layout arg expands to its W hidden physical
\ fields (PUSH-LOGICAL) so the checker row and the runtime cells agree (docs §11)
\ and the branch/UNMAKE consumes the whole bundle. Everything else — a cell or
\ pointer payload, a W=1 layout (enum / single-field product, whose one logical
\ cell IS the value the branch uses), an OPEN param var (construct's fresh var
\ before any declared-output recovery), and a possibly-linear arg — stays one
\ logical cell exactly as MK-PUSH did, so every pre-existing construction and
\ match arm keeps its verdict AND its payload term shape.
: TFC-PUSH-PAY ( n n -- n )
   over T-WIDTH 1 > IF PUSH-LOGICAL ELSE MK-PUSH THEN ;
: TFC-PAY-ROW ( n n -- n ) {: vid:n row0:n :}
   row0 TFC-ROW !
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-SCH-COUNT@ < WHILE
      vid SUMV-SCH-START@ TFC-J @ + SCHEMA-ROOT@ TFC-SCH-TERM
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

\ layout-cap slice 4 (dot habu-checker-capability-layout-9b8540bd): width-aware
\ construct/MATCH lowering. The checker records, per genuinely-wide FLAT instantiation
\ (a multi-cell layout arg that is itself an arity-0 named product/family), one
\ extra-pad fact keyed at the construct/ctor-call/`of` token: w = instantiated_pads -
\ declared_pads, flagged WF-XPAD-FLAG so pass-2 fires even when the difference is 1.
\ Pass 2 (native EM-ADT-CON-VAR / EM-COMPILE-CALL / EM-ADT-MATCH-OF, gforth mirror)
\ adds those extra zero cells so the physical bundle matches the arg-aware width. A
\ nested parametric arg (arity>0) stays outside the flat model and keeps the slice-3
\ staged fail-closed (construct) / declared-width (match), tracked to slice 5.
variable TFC-NEST
: TFC-CON-FLAT? ( n -- bool ) {: dt:n :}   \ every multi-cell arg is a flat arity-0 family
   0 TFC-NEST !
   0 TFC-I !
   BEGIN TFC-I @ dt PARAM>ARGC < WHILE
      dt TFC-I @ PARAM>ARG dup T-WIDTH 1 > swap PARAM>ARGC 0 > and IF -1 TFC-NEST ! THEN
      TFC-I @ 1 + TFC-I !
   REPEAT
   TFC-NEST @ 0= ;

: TFC-VAR-PAYCELLS ( n -- n ) {: vid:n :}   \ sum of instantiated payload cell widths for a variant
   0
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-SCH-COUNT@ < WHILE
      vid SUMV-SCH-START@ TFC-J @ + SCHEMA-ROOT@ TFC-SCH-TERM T-WIDTH +
      TFC-J @ 1 + TFC-J !
   REPEAT ;

: TFC-CON-XPAD-RECORD ( n n n -- ) {: fam:n vid:n famterm:n :}   \ record the wide construct's extra-pad fact
   famterm T-WIDTH 1 -                          \ instantiated slots
   vid TFC-VAR-PAYCELLS -                        \ - instantiated payload cells = instantiated pads
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -        \ - declared pads = extra pads
   {: extra:n :}
   extra 0 > IF 0 fam 0 extra WF-XPAD-FLAG WF-ADD-FULL THEN ;

: TFAM-MATCH-XPAD-RECORD ( n n -- ) {: vid:n term:n :}   \ record a wide MATCH arm's extra-pad fact
   term T-RES {: rt:n :}
   rt TFC-CON-FLAT? 0= IF EXIT THEN             \ nested arm: leave declared width (slice 5)
   rt T-WIDTH 1 -                                \ instantiated slots
   vid TFC-VAR-PAYCELLS -                        \ - instantiated payload cells = instantiated pads
   vid SUMV-FAM@ TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -   \ - declared pads = extra pads
   {: extra:n :}
   extra 0 > IF 0 vid SUMV-FAM@ 0 extra WF-XPAD-FLAG WF-ADD-FULL THEN ;

: TFAM-ACTIVE-PKG$ ( -- ptr u8 n )        \ active checker package ("" at top level)
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ EXIT THEN
   s" " ;

: TFAM-CONSTRUCT-FAM ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ folded family token -> id
   TFAM-ACTIVE-PKG$ na nu TFAM-FIND-IN 0= IF drop MD-CON-FAM MDIAG! 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-CON-KIND MDIAG! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

\ TFC-CONSTRUCT-STEP-VID ( fam vid -- ) : apply the inline generated-constructor
\ effect for a resolved (family,variant). One fresh checker var per family param,
\ then — bidirectionally — the concrete args named by the declared output are
\ recovered over those vars (CONSTRUCT-DECL-TERM), so a NAMED multi-cell layout
\ arg makes the payload input and the layout-bundle output PUSH-LOGICAL-expand to
\ the arg-aware width; cell/open args keep the fresh var and the boundary
\ coercion, unchanged. Shared by the reserved `construct` token and, for a
\ multi-cell instantiation, the generated-constructor CALL (TFAM-CTOR-STEP?).
: TFC-CONSTRUCT-STEP-VID ( n n -- ) {: fam:n vid:n :}
   fam TFAM-ARITY@ TFC-MINT-VARS
   fam CONSTRUCT-DECL-MULTICELL? {: dt:n :}           \ multi-cell bundle term, else 0
   dt 0 <> IF dt TFC-ARGS! THEN                       \ recover concrete args ONLY for a multi-cell instantiation; cell/open stay fresh
   FRESH MK-ROW {: base:n :}
   vid base TFC-PAY-ROW {: din:n :}
   fam TFC-FAM-TERM {: famterm:n :}
   dt 0 <> IF                                         \ layout-cap slice 4: width-aware lowering, else fail closed
      dt TFC-CON-FLAT? IF fam vid famterm TFC-CON-XPAD-RECORD THEN
   THEN
   famterm base PUSH-LOGICAL {: dout:n :}
   din dout CHECKER-STEP
   dt 0 <> IF
      dt TFC-CON-FLAT? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN   \ nested parametric arg: slice 5 (staged fail-closed)
   THEN ;

: TFAM-CONSTRUCT-STEP ( ptr u8 n n -- bool ) {: na:ptr nu:n fam:n :}
   fam na nu SUMV-FIND 0= IF drop MD-CON-VAR MDIAG! RES-FALSE EXIT THEN
   {: vid:n :}
   fam vid TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ TFAM-CTOR-STEP? ( sym -- bool ) : a generated-constructor CALL whose stored
\ 1-cell-per-param effect cannot express the instantiation. Reverse the resolved
\ word symbol to its variant; if the declared output binds this family to a
\ multi-cell layout arg (CONSTRUCT-DECL-MULTICELL?), apply the arg-aware step and
\ report handled. Otherwise report unhandled so DO-TOK runs the ordinary word
\ call — every cell/generic/scalar constructor call is untouched.
: SUMV-FROM-CTOR-SYM ( n -- n bool ) {: sym:n :}   \ constructor word symbol -> variant id
   sym 0 <= IF 0 RES-FALSE EXIT THEN
   0 TF-I !
   BEGIN TF-I @ SUMV-N @ < WHILE
      TF-I @ SUMV-CTOR-SYM@ sym = IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: TFAM-CTOR-STEP? ( n -- bool ) {: sym:n :}
   sym SUMV-FROM-CTOR-SYM 0= IF drop RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam CONSTRUCT-DECL-MULTICELL? 0= IF RES-FALSE EXIT THEN   \ not a multi-cell instantiation: fall through to the ordinary word call
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

\ Install the checker's friend xt hooks: checker.f loads before this file, so it
\ resolves families / reads arities during signature parsing through these cells.
' TFAM-SIG-RESOLVE TFAM-RESOLVE-XT !
' TFAM-CTOR-PKG?    CTOR-PKG?-XT !     \ item 8: constructor-package reopen reject
' TFAM-CTOR-WORD?   CTOR-WORD?-XT !    \ item 8: generated-word undefine reject
' TFAM-CTOR-EXTEND? CTOR-EXTEND?-XT !  \ item 8: closed-package extra-tail reject
' TFAM-ARITY@  TFAM-ARITY-XT !
' TFAM-LAYOUT? TFAM-LAYOUT?-XT !   \ item 7: checker reaches the layout kind for its fail-closed guard
' TFAM-CELL?   TFAM-CELL?-XT !     \ nominal scalars: checker reaches the cell kind for LAYOUT-BUFFER admission + pointee governance
' TFAM-WIDTH@  TFAM-WIDTH-XT !     \ item 12: checker reads DECLARED logical widths (params-as-cells) for the boot fallback
' TFAM-INST-WIDTH@ TFAM-INST-WIDTH-XT !   \ layout-cap slice 1: arg-aware INSTANTIATED width for T-WIDTH / WF fact surface
' TFAM-CONSTRUCT-FAM  CONSTRUCT-FAM-XT !   \ item 9: construct family resolution (active package only)
' TFAM-CONSTRUCT-STEP CONSTRUCT-STEP-XT !  \ item 9: construct variant resolve + inline constructor effect
' TFAM-CTOR-STEP?     CTOR-STEP-XT !        \ layout-cap slice 3: generated-constructor CALL on a multi-cell layout arg routes through the arg-aware step
' TFAM-MATCH-FAM     MATCH-FAM-XT !     \ item 9: MATCH family resolution (signature scope)
' TFAM-MATCH-VARIANT MATCH-VAR-XT !     \ item 9: MATCH branch variant resolve
' SUMV-TAG@          MATCH-VTAG-XT !    \ item 9: variant id -> declaration-order tag (bitset index)
' TFAM-VAR-COUNT@    MATCH-VCOUNT-XT !  \ item 9: exhaustiveness domain size
' TFAM-MATCH-PAY     MATCH-PAY-XT !     \ item 9: branch payload row from the scrutinee's args
