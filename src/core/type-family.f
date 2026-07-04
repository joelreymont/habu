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
: TF-STR ( -- ptr ) TF-STR-P @ ;
variable TF-STR-U   0 TF-STR-U !

: TF-STR-GROW ( n -- ) {: need:n :}
   need TF-STR-CAP-V @ 2 * max {: nc:n :}
   TF-STR-P  TF-STR-CAP-V @  nc  REG-GROW1
   nc TF-STR-CAP-V ! ;
: TF-STR-ENSURE ( n -- ) {: add:n :}      \ room for `add` more bytes
   TF-STR-U @ add + TF-STR-CAP-V @ <= IF exit THEN
   TF-STR-U @ add + TF-STR-GROW ;
: TF-INTERN ( ptr u8 n -- off ) {: a:ptr u:n :}   \ copy bytes into the pool -> offset
   u TF-STR-ENSURE                        \ grow first, then cache a stable base
   TF-STR-U @ {: off:n :}
   0 TF-I !
   BEGIN TF-I @ u < WHILE
      a TF-I @ + c@   TF-STR off + TF-I @ + c!
      TF-I @ 1 + TF-I !
   REPEAT
   TF-STR-U @ u + TF-STR-U !
   off ;
: TF-OFF$ ( off u -- ptr u8 n ) {: off:n u:n :}   \ interned (offset,len) -> string
   TF-STR off + u ;

\ ---------------------------------------------------------------------------
\ canonical tail validation. Declarations accept only lowercase tokens; the
\ registry never folds case, so an uppercase/mixed-case token is rejected here.
\ ---------------------------------------------------------------------------
: TF-LOWER? ( c -- bool ) {: c:n :} c 97 >= c 122 <= and ;   \ a-z
: TF-DIGIT? ( c -- bool ) {: c:n :} c 48 >= c 57 <= and ;    \ 0-9
: TF-UPPER? ( c -- bool ) {: c:n :} c 65 >= c 90 <= and ;    \ A-Z
: TF-TAILBYTE? ( c -- bool ) {: c:n :}   \ lowercase, digit, or internal hyphen
   c TF-LOWER? IF RES-TRUE EXIT THEN
   c TF-DIGIT? IF RES-TRUE EXIT THEN
   c 45 = ;                                                  \ '-'
: TF-HIDDEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ compaction-hidden field name
   u 0= IF RES-FALSE EXIT THEN
   a c@ 64 = ;                                              \ leading '@'
: TF-HYPHEN-BAD? ( a u i -- bool ) {: a:ptr u:n i:n :}   \ '-' at an edge or doubled
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
: TF-PK-BASE ( -- ptr ) TF-PK-P @ ;
variable TF-PK-N   0 TF-PK-N !

: TF-PK-GROW ( n -- ) {: need:n :}
   need TF-PK-CAP-V @ 2 * max {: nc:n :}
   TF-PK-P  TF-PK-CAP-V @ cells  nc cells  REG-GROW1
   nc TF-PK-CAP-V ! ;
: TF-PK-ENSURE ( -- )
   TF-PK-N @ TF-PK-CAP-V @ < IF exit THEN
   TF-PK-N @ 1 + TF-PK-GROW ;
: TF-PK+ ( kind -- ) {: k:n :}          \ append one param-kind slot
   TF-PK-ENSURE
   k TF-PK-N @ cells TF-PK-BASE + !
   TF-PK-N @ 1 + TF-PK-N ! ;

\ ---------------------------------------------------------------------------
\ TFAM record arena.
\ ---------------------------------------------------------------------------
BEGIN-STRUCTURE TF-REC
   CELL +FIELD TF.PKG-OFF
   CELL +FIELD TF.PKG-U
   CELL +FIELD TF.VIS
   CELL +FIELD TF.NAME-OFF
   CELL +FIELD TF.NAME-U
   CELL +FIELD TF.ARITY
   CELL +FIELD TF.KIND
   CELL +FIELD TF.PK-START
   CELL +FIELD TF.LAYOUT
   CELL +FIELD TF.SLOTS
   CELL +FIELD TF.VAR-START
   CELL +FIELD TF.VAR-COUNT
   CELL +FIELD TF.FLD-START
   CELL +FIELD TF.FLD-COUNT
   CELL +FIELD TF.TAGW
   CELL +FIELD TF.SCHEMA-ROOT
   CELL +FIELD TF.SPAN-OFF
   CELL +FIELD TF.SPAN-U
END-STRUCTURE

4 constant TF-CAP-INIT
variable TF-CAP-V   TF-CAP-INIT TF-CAP-V !
: TF-CAP ( -- n ) TF-CAP-V @ ;
create TF-A-BOOT   TF-CAP-INIT TF-REC * allot
variable TF-A-P   TF-A-BOOT TF-A-P !
: TF-BASE ( -- ptr ) TF-A-P @ ;
variable TFAM-N   0 TFAM-N !

: TF-GROW ( n -- ) {: need:n :}
   need TF-CAP-V @ 2 * max {: nc:n :}
   TF-A-P  TF-CAP-V @ TF-REC *  nc TF-REC *  REG-GROW1
   nc TF-CAP-V ! ;
: TF-ENSURE ( -- )
   TFAM-N @ TF-CAP-V @ < IF exit THEN
   TFAM-N @ 1 + TF-GROW ;
: TF-REC@ ( id -- ptr ) {: id:n :}       \ address of family record `id`
   id 0 < IF s" tfam: bad family id" 76 die THEN
   id TFAM-N @ >= IF s" tfam: bad family id" 76 die THEN
   id TF-REC * TF-BASE + ;

: TFAM-N@ ( -- n ) TFAM-N @ ;            \ family high-water (rollback/tests)
: TF-STR-U@ ( -- n ) TF-STR-U @ ;        \ interned string-pool high-water
: TF-PK-N@ ( -- n ) TF-PK-N @ ;          \ param-kind pool high-water

\ --- read-only queries.
: TFAM-PKG$ ( id -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.PKG-OFF @ r TF.PKG-U @ TF-OFF$ ;
: TFAM-NAME$ ( id -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.NAME-OFF @ r TF.NAME-U @ TF-OFF$ ;
: TFAM-VIS@ ( id -- vis ) TF-REC@ TF.VIS @ ;
: TFAM-ARITY@ ( id -- n ) TF-REC@ TF.ARITY @ ;
: TFAM-KIND@ ( id -- kind ) TF-REC@ TF.KIND @ ;
: TFAM-LAYOUT-POLICY@ ( id -- policy ) TF-REC@ TF.LAYOUT @ ;
: TFAM-SLOTS@ ( id -- n ) TF-REC@ TF.SLOTS @ ;
: TFAM-VAR-START@ ( id -- n ) TF-REC@ TF.VAR-START @ ;
: TFAM-VAR-COUNT@ ( id -- n ) TF-REC@ TF.VAR-COUNT @ ;
: TFAM-FLD-START@ ( id -- n ) TF-REC@ TF.FLD-START @ ;
: TFAM-FLD-COUNT@ ( id -- n ) TF-REC@ TF.FLD-COUNT @ ;
: TFAM-TAGW@ ( id -- n ) TF-REC@ TF.TAGW @ ;
: TFAM-SCHEMA-ROOT@ ( id -- n ) TF-REC@ TF.SCHEMA-ROOT @ ;
: TFAM-SPAN@ ( id -- off u ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.SPAN-OFF @ r TF.SPAN-U @ ;
: TFAM-PK@ ( id i -- kind ) {: id:n i:n :}
   i 0 < i id TFAM-ARITY@ >= or IF s" tfam: bad param index" 76 die THEN
   id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + @ ;

: TFAM-PUBLIC? ( id -- bool ) TFAM-VIS@ CHECKER-PACKAGE-PUBLIC = ;
: TFAM-CELL? ( id -- bool ) TFAM-KIND@ TK-CELL = ;
: TFAM-PRODUCT? ( id -- bool ) TFAM-KIND@ TK-PRODUCT = ;
: TFAM-SUM? ( id -- bool ) TFAM-KIND@ TK-SUM = ;
: TFAM-ENUM? ( id -- bool ) TFAM-KIND@ TK-ENUM = ;
: TFAM-LAYOUT? ( id -- bool ) {: id:n :}   \ true when the family occupies an ADT layout
   id TFAM-PRODUCT? id TFAM-SUM? or id TFAM-ENUM? or ;

\ --- friend-only field mutators (populated by later declaration passes / tests).
: TFAM-LAYOUT! ( id policy -- ) {: id:n p:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   p id TF-REC@ TF.LAYOUT ! ;
: TFAM-SLOTS! ( id n -- ) swap TF-REC@ TF.SLOTS ! ;
: TFAM-VAR-RANGE! ( id start count -- ) {: id:n s:n c:n :}
   s id TF-REC@ TF.VAR-START !  c id TF-REC@ TF.VAR-COUNT ! ;
: TFAM-FLD-RANGE! ( id start count -- ) {: id:n s:n c:n :}
   s id TF-REC@ TF.FLD-START !  c id TF-REC@ TF.FLD-COUNT ! ;
: TFAM-TAGW! ( id w -- ) swap TF-REC@ TF.TAGW ! ;
: TFAM-SCHEMA-ROOT! ( id root -- ) swap TF-REC@ TF.SCHEMA-ROOT ! ;
: TFAM-SPAN! ( id off u -- ) {: id:n off:n u:n :}
   off id TF-REC@ TF.SPAN-OFF !  u id TF-REC@ TF.SPAN-U ! ;
: TFAM-PK! ( id i kind -- ) {: id:n i:n k:n :}
   i 0 < i id TFAM-ARITY@ >= or IF s" tfam: bad param index" 76 die THEN
   k 0 < k PK-MAX > or IF E-TFAM-KIND throw THEN
   k id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + ! ;

\ --- matching and lookup.
: TFAM-PKG-MATCH? ( ptr u8 n id -- bool ) {: pa:ptr pu:n id:n :}
   id TFAM-PKG$ pa pu CORE-STR= ;
: TFAM-NAME-MATCH? ( ptr u8 n id -- bool ) {: na:ptr nu:n id:n :}
   id TFAM-NAME$ na nu CORE-STR= ;

\ exact (package,tail) — the qualified-lookup and duplicate-detection primitive.
: TFAM-FIND-IN ( pkg-a pkg-u name-a name-u -- id true | false )
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
: TFAM-FIND-PUBLIC ( name-a name-u -- id true | false ) {: na:ptr nu:n :}
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
: TFAM-RESOLVE ( pkg-a pkg-u name-a name-u -- id true | false )
   {: pa:ptr pu:n na:ptr nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   pa pu na nu TFAM-FIND-IN IF RES-TRUE EXIT THEN
   drop
   na nu TFAM-FIND-PUBLIC ;

\ --- declaration. Storage only ever sees canonical lowercase tails.
: TFAM-KIND-VALID? ( kind -- bool ) {: k:n :} k 0 >= k TK-MAX <= and ;
: TFAM-PK-RESERVE ( arity -- ) {: k:n :}    \ default every parameter to PK-CELL
   0 TF-I !
   BEGIN TF-I @ k < WHILE
      PK-CELL TF-PK+
      TF-I @ 1 + TF-I !
   REPEAT ;
: TFAM-DECL ( pkg-a pkg-u vis name-a name-u arity kind -- id )
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
   arity TFAM-PK-RESERVE
   id ;

\ ---------------------------------------------------------------------------
\ SUMV: sum/enum variant records, keyed by (family-id, variant tail).
\ ---------------------------------------------------------------------------
BEGIN-STRUCTURE SUMV-REC
   CELL +FIELD SV.FAM
   CELL +FIELD SV.NAME-OFF
   CELL +FIELD SV.NAME-U
   CELL +FIELD SV.TAG
   CELL +FIELD SV.SCH-START
   CELL +FIELD SV.SCH-COUNT
   CELL +FIELD SV.PAYCELLS
   CELL +FIELD SV.CTOR-SYM
   CELL +FIELD SV.CTOR-PKG-OFF
   CELL +FIELD SV.CTOR-PKG-U
END-STRUCTURE

4 constant SUMV-CAP-INIT
variable SUMV-CAP-V   SUMV-CAP-INIT SUMV-CAP-V !
: SUMV-CAP ( -- n ) SUMV-CAP-V @ ;
create SUMV-A-BOOT   SUMV-CAP-INIT SUMV-REC * allot
variable SUMV-A-P   SUMV-A-BOOT SUMV-A-P !
: SUMV-BASE ( -- ptr ) SUMV-A-P @ ;
variable SUMV-N   0 SUMV-N !

: SUMV-GROW ( n -- ) {: need:n :}
   need SUMV-CAP-V @ 2 * max {: nc:n :}
   SUMV-A-P  SUMV-CAP-V @ SUMV-REC *  nc SUMV-REC *  REG-GROW1
   nc SUMV-CAP-V ! ;
: SUMV-ENSURE ( -- )
   SUMV-N @ SUMV-CAP-V @ < IF exit THEN
   SUMV-N @ 1 + SUMV-GROW ;
: SUMV-REC@ ( id -- ptr ) {: id:n :}
   id 0 < IF s" tfam: bad variant id" 76 die THEN
   id SUMV-N @ >= IF s" tfam: bad variant id" 76 die THEN
   id SUMV-REC * SUMV-BASE + ;

: SUMV-FAM@ ( id -- fam ) SUMV-REC@ SV.FAM @ ;
: SUMV-NAME$ ( id -- ptr u8 n ) {: id:n :}
   id SUMV-REC@ {: r:ptr :}  r SV.NAME-OFF @ r SV.NAME-U @ TF-OFF$ ;
: SUMV-TAG@ ( id -- tag ) SUMV-REC@ SV.TAG @ ;
: SUMV-SCH-START@ ( id -- n ) SUMV-REC@ SV.SCH-START @ ;
: SUMV-SCH-COUNT@ ( id -- n ) SUMV-REC@ SV.SCH-COUNT @ ;
: SUMV-PAYCELLS@ ( id -- n ) SUMV-REC@ SV.PAYCELLS @ ;
: SUMV-N@ ( -- n ) SUMV-N @ ;

: SUMV-MATCH? ( fam name-a name-u id -- bool ) {: fam:n na:ptr nu:n id:n :}
   id SUMV-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id SUMV-NAME$ na nu CORE-STR= ;
: SUMV-FIND ( fam name-a name-u -- id true | false ) {: fam:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ SUMV-N @ < WHILE
      fam na nu TF-I @ SUMV-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: SUMV-ADD ( fam name-a name-u tag sch-start sch-count paycells -- id )
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
\ product fields, keyed by (family-id, field tail).
\ ---------------------------------------------------------------------------
BEGIN-STRUCTURE PF-REC
   CELL +FIELD PF.FAM
   CELL +FIELD PF.NAME-OFF
   CELL +FIELD PF.NAME-U
   CELL +FIELD PF.SCH
   CELL +FIELD PF.SLOT
END-STRUCTURE

4 constant PF-CAP-INIT
variable PF-CAP-V   PF-CAP-INIT PF-CAP-V !
: PF-CAP ( -- n ) PF-CAP-V @ ;
create PF-A-BOOT   PF-CAP-INIT PF-REC * allot
variable PF-A-P   PF-A-BOOT PF-A-P !
: PF-BASE ( -- ptr ) PF-A-P @ ;
variable PF-N   0 PF-N !

: PF-GROW ( n -- ) {: need:n :}
   need PF-CAP-V @ 2 * max {: nc:n :}
   PF-A-P  PF-CAP-V @ PF-REC *  nc PF-REC *  REG-GROW1
   nc PF-CAP-V ! ;
: PF-ENSURE ( -- )
   PF-N @ PF-CAP-V @ < IF exit THEN
   PF-N @ 1 + PF-GROW ;
: PF-REC@ ( id -- ptr ) {: id:n :}
   id 0 < IF s" tfam: bad field id" 76 die THEN
   id PF-N @ >= IF s" tfam: bad field id" 76 die THEN
   id PF-REC * PF-BASE + ;

: PF-FAM@ ( id -- fam ) PF-REC@ PF.FAM @ ;
: PF-NAME$ ( id -- ptr u8 n ) {: id:n :}
   id PF-REC@ {: r:ptr :}  r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ ;
: PF-SCH@ ( id -- node ) PF-REC@ PF.SCH @ ;
: PF-SLOT@ ( id -- n ) PF-REC@ PF.SLOT @ ;
: PF-N@ ( -- n ) PF-N @ ;

: PF-MATCH? ( fam name-a name-u id -- bool ) {: fam:n na:ptr nu:n id:n :}
   id PF-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id PF-NAME$ na nu CORE-STR= ;
: PF-FIND ( fam name-a name-u -- id true | false ) {: fam:n na:ptr nu:n :}
   0 TF-I !
   BEGIN TF-I @ PF-N @ < WHILE
      fam na nu TF-I @ PF-MATCH? IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: PF-ADD ( fam name-a name-u sch slot -- id ) {: fam:n na:ptr nu:n sch:n slot:n :}
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

\ ---------------------------------------------------------------------------
\ logical layout records, one per family that has a resolved physical layout.
\ ---------------------------------------------------------------------------
BEGIN-STRUCTURE LAY-REC
   CELL +FIELD LAY.FAM
   CELL +FIELD LAY.POLICY
   CELL +FIELD LAY.SIZE
   CELL +FIELD LAY.ALIGN
   CELL +FIELD LAY.TAGW
END-STRUCTURE

4 constant LAY-CAP-INIT
variable LAY-CAP-V   LAY-CAP-INIT LAY-CAP-V !
: LAY-CAP ( -- n ) LAY-CAP-V @ ;
create LAY-A-BOOT   LAY-CAP-INIT LAY-REC * allot
variable LAY-A-P   LAY-A-BOOT LAY-A-P !
: LAY-BASE ( -- ptr ) LAY-A-P @ ;
variable LAY-N   0 LAY-N !

: LAY-GROW ( n -- ) {: need:n :}
   need LAY-CAP-V @ 2 * max {: nc:n :}
   LAY-A-P  LAY-CAP-V @ LAY-REC *  nc LAY-REC *  REG-GROW1
   nc LAY-CAP-V ! ;
: LAY-ENSURE ( -- )
   LAY-N @ LAY-CAP-V @ < IF exit THEN
   LAY-N @ 1 + LAY-GROW ;
: LAY-REC@ ( id -- ptr ) {: id:n :}
   id 0 < IF s" tfam: bad layout id" 76 die THEN
   id LAY-N @ >= IF s" tfam: bad layout id" 76 die THEN
   id LAY-REC * LAY-BASE + ;

: LAY-FAM@ ( id -- fam ) LAY-REC@ LAY.FAM @ ;
: LAY-POLICY@ ( id -- policy ) LAY-REC@ LAY.POLICY @ ;
: LAY-SIZE@ ( id -- n ) LAY-REC@ LAY.SIZE @ ;
: LAY-ALIGN@ ( id -- n ) LAY-REC@ LAY.ALIGN @ ;
: LAY-TAGW@ ( id -- n ) LAY-REC@ LAY.TAGW @ ;
: LAY-N@ ( -- n ) LAY-N @ ;

: LAY-FIND ( fam -- id true | false ) {: fam:n :}
   0 TF-I !
   BEGIN TF-I @ LAY-N @ < WHILE
      TF-I @ LAY-FAM@ fam = IF TF-I @ RES-TRUE EXIT THEN
      TF-I @ 1 + TF-I !
   REPEAT
   0 RES-FALSE ;
: LAY-ADD ( fam policy size align tagw -- id ) {: fam:n p:n sz:n al:n tw:n :}
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
BEGIN-STRUCTURE TF-RBF-REC
   CELL +FIELD TFRB.TFAMN
   CELL +FIELD TFRB.STRU
   CELL +FIELD TFRB.PKN
   CELL +FIELD TFRB.SUMVN
   CELL +FIELD TFRB.PFN
   CELL +FIELD TFRB.LAYN
END-STRUCTURE

16 constant TF-RBF-CAP-INIT
variable TF-RBF-CAP-V   TF-RBF-CAP-INIT TF-RBF-CAP-V !
create TF-RBF-BOOT   TF-RBF-CAP-INIT TF-RBF-REC * allot
variable TF-RBF-P    TF-RBF-BOOT TF-RBF-P !
: TF-RBF-BASE ( -- ptr ) TF-RBF-P @ ;
variable TF-RBF-DEPTH   0 TF-RBF-DEPTH !

: TF-RBF-GROW ( -- )
   TF-RBF-CAP-V @ 2 * {: nc:n :}
   TF-RBF-P  TF-RBF-CAP-V @ TF-RBF-REC *  nc TF-RBF-REC *  REG-GROW1
   nc TF-RBF-CAP-V ! ;
: TF-RBF-ENSURE ( -- )
   TF-RBF-DEPTH @ TF-RBF-CAP-V @ < IF exit THEN
   TF-RBF-GROW ;
: TF-RBF-CUR ( -- ptr ) TF-RBF-DEPTH @ TF-RBF-REC * TF-RBF-BASE + ;

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
' REG-EXT-ROLLBACK-SAVE    REG-EXT-RB-SAVE-XT !
' REG-EXT-ROLLBACK-RESTORE REG-EXT-RB-RESTORE-XT !

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
: TFAM-REG-CELL ( name-a name-u arity -- )   \ public global TK-CELL family
   {: na:ptr nu:n ar:n :}
   s" " CHECKER-PACKAGE-PUBLIC na nu ar TK-CELL TFAM-DECL drop ;

\ `ptr` is dual: `ptr<space,elem>` is a parametric pointer (T-PARAM, resolved
\ here), while `ptr elem` (no `<`) stays the MK-PTR plain-pointer special case in
\ checker.f SIG-TYPE. Registered arity 2 matches every `ptr<...>` in the tree.
s" ptr"        2 TFAM-REG-CELL
s" span"       3 TFAM-REG-CELL
s" matrix"     4 TFAM-REG-CELL
s" gridctx"    3 TFAM-REG-CELL
s" fanctx"     3 TFAM-REG-CELL
s" idxctx"     4 TFAM-REG-CELL
s" uniqidxctx" 4 TFAM-REG-CELL
s" coopctx"    3 TFAM-REG-CELL
s" rowctx"     3 TFAM-REG-CELL
s" tile"       3 TFAM-REG-CELL
s" acc"        3 TFAM-REG-CELL
s" mmctx"      3 TFAM-REG-CELL
s" mmacc"      3 TFAM-REG-CELL
s" uniform"    1 TFAM-REG-CELL
s" rowidx"     1 TFAM-REG-CELL

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

: TFAM-QUAL-RESOLVE ( pkg-a pkg-u -- id true | false ) {: pa:ptr pu:n :}
   TFQ-TA @ TFQ-TU @ TF-CANON? 0= IF 0 RES-FALSE EXIT THEN
   TFQ-BUF TFQ-U @ TFQ-TA @ TFQ-TU @ TFAM-FIND-IN 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-PUBLIC? IF id RES-TRUE EXIT THEN
   TFQ-BUF TFQ-U @ pa pu CORE-STR= IF id RES-TRUE EXIT THEN   \ own private rows
   0 RES-FALSE ;

: TFAM-SIG-RESOLVE ( pkg-a pkg-u name-a name-u -- id true | false )
   {: pa:ptr pu:n na:ptr nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   na nu TFQ-SPLIT? IF pa pu TFAM-QUAL-RESOLVE EXIT THEN
   pa pu na nu ['] TFAM-RESOLVE catch {: rc:n :}
   rc 0= IF EXIT THEN                         \ ( id flag ) from the resolver
   2drop 2drop                                \ catch reset the pre-call depth
   rc E-TFAM-AMBIG = IF 0 RES-FALSE EXIT THEN
   rc throw ;

\ Install the checker's friend xt hooks: checker.f loads before this file, so it
\ resolves families / reads arities during signature parsing through these cells.
' TFAM-SIG-RESOLVE TFAM-RESOLVE-XT !
' TFAM-ARITY@  TFAM-ARITY-XT !
