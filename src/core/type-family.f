\ type-family.f: package TFAM, the family (TFAM), variant (SUMV), product-field
\ and logical-layout registries for the checker; loaded unchecked in the checker
\ prefix after type-schema.f. Records key on (package, canonical lowercase tail);
\ names are byte offsets into one string pool, so records hold only integers and
\ snapshot persist bakes stores verbatim. Registries roll back through RBF-PUSH /
\ RBF-POP and persist through TFAM-SNAPSHOT-PERSIST. Five words stay global (TFAM-NAME$).

using SCHEMA-REG

\ A name referenced outside this package is public, everything else private;
\ outside includes the checker's `PPRIM: TFAM` rows and the inner packages this
\ file opens (TYPE-NAME, TYPE-FIELD, TYPE-FIELD-OWNER, CHECKER-DECL-FRAME,
\ SCHEMA-REG). Packages do not nest (habu2.f C-PACKAGE exits $4B), so each inner
\ block closes TFAM, reopens it after, and reads publics through `using TFAM`.
package TFAM

\ --- kind of a family (what its values are).

public

\ NFILE-REF installs these after the engine prefix has loaded. Family identity
\ is its exact canonical package and tail; constructor identity is an
\ occurrence in the FILE tree.
defer FAMILY-KEY-XT ( n -- ptr u8 n bool )
defer FAMILY-BY-KEY-XT ( ptr u8 n -- n bool )
defer FAMILY-KEY-MATCH-XT ( ptr u8 n ptr u8 n ptr u8 n -- bool )
defer CON-DECLARED-XT ( n -- )
defer CON-KEY-XT ( n -- ptr u8 n bool )
defer CON-BY-KEY-XT ( ptr u8 n -- n bool )
defer CON-BIND-XT ( n ptr u8 n -- )
defer UNIT-FAM-RESET-XT ( -- )
76 constant E-REG-AOT-REFUSED \ a type-registry capture or plan is refused (REG-AOT-REFUSE)

private
: FAMILY-KEY-ABSENT ( n -- ptr u8 n bool ) drop NULL-PTR 0 RES-FALSE ;
: FAMILY-BY-KEY-ABSENT ( ptr u8 n -- n bool ) 2drop 0 RES-FALSE ;
: FAMILY-KEY-MATCH-ABSENT ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   2drop 2drop 2drop RES-FALSE ;
: FAMILY-BIND-ABSENT ( n ptr u8 n -- ) 2drop drop E-REG-AOT-REFUSED throw ;
: FAMILY-HOOK-DEFAULTS ( -- )
   [: FAMILY-KEY-ABSENT ;] is FAMILY-KEY-XT
   [: FAMILY-BY-KEY-ABSENT ;] is FAMILY-BY-KEY-XT
   [: FAMILY-KEY-MATCH-ABSENT ;] is FAMILY-KEY-MATCH-XT
   [: drop ;] is CON-DECLARED-XT
   [: FAMILY-KEY-ABSENT ;] is CON-KEY-XT
   [: FAMILY-BY-KEY-ABSENT ;] is CON-BY-KEY-XT
   [: FAMILY-BIND-ABSENT ;] is CON-BIND-XT
   [: ;] is UNIT-FAM-RESET-XT ;
FAMILY-HOOK-DEFAULTS

public

: FILE-REF-RESET ( -- )
   [: FAMILY-KEY-ABSENT ;] is FAMILY-KEY-XT
   [: FAMILY-BY-KEY-ABSENT ;] is FAMILY-BY-KEY-XT
   [: FAMILY-KEY-MATCH-ABSENT ;] is FAMILY-KEY-MATCH-XT
   [: drop ;] is CON-DECLARED-XT
   [: drop ;] CHECKER-REG:CON-DECLARED-USE
   [: FAMILY-KEY-ABSENT ;] is CON-KEY-XT
   [: FAMILY-BY-KEY-ABSENT ;] is CON-BY-KEY-XT
   [: FAMILY-BIND-ABSENT ;] is CON-BIND-XT ;

: FILE-REF-USE ( [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ ptr u8 n ptr u8 n ptr u8 n -- bool ] [ n -- ] [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ n ptr u8 n -- ] -- )
   is CON-BIND-XT
   is CON-BY-KEY-XT
   is CON-KEY-XT
   dup CHECKER-REG:CON-DECLARED-USE
   is CON-DECLARED-XT
   is FAMILY-KEY-MATCH-XT
   is FAMILY-BY-KEY-XT
   is FAMILY-KEY-XT ;

0 constant TK-CELL          \ scalar-cell family (no ADT layout)
1 constant TK-PRODUCT       \ record / struct
2 constant TK-SUM           \ tagged sum
3 constant TK-ENUM          \ payload-free sum

private

4 constant TK-EVIDENCE      \ compile-only evidence family
4 constant TK-MAX

\ --- layout policy (physical representation); the default is TL-STACK-CELL-TAG.

public

0 constant TL-STACK-CELL-TAG
1 constant TL-PACKED-TAG
2 constant TL-NICHE
3 constant TL-BOXED
4 constant TL-CUSTOM

private

4 constant TL-MAX

\ --- parameter kind (kind of each of a family's `arity` parameters).

public

0 constant PK-CELL
1 constant PK-LAYOUT
2 constant PK-TYPE
4 constant PK-SCOPE        \ non-value lexical scope identity
5 constant PK-REGION       \ non-value host allocation identity

private

3 constant PK-EVIDENCE
5 constant PK-MAX

public

CELL constant TAGW-CELL     \ default tag width: one stack cell

\ Visibility reuses the checker's package modes so registry and package scope
\ never diverge: CHECKER-PACKAGE-PRIVATE (1) / CHECKER-PACKAGE-PUBLIC (2).

\ --- named reject codes (thrown, caught by parser/CHECK path or unit `catch`).
7101 constant E-TFAM-CASE     \ uppercase/mixed-case or non-canonical tail token
7102 constant E-TFAM-DUP      \ duplicate tail within the same package
7106 constant E-TFAM-AMBIG    \ two other-package public families tie on a tail (7103 = E-SCHEMA-BAD)
7209 constant E-UNIT-FAMILY-CHANGED \ a FILE family dependency is absent or its contract differs

private

7104 constant E-TFAM-ARITY    \ negative arity
7105 constant E-TFAM-KIND     \ unknown kind

public

\ 7122-7124 are layout-buffer.f's E-LAYOUT-BOUNDS / -UNBOUND / -CEIL.
7148 constant E-PF-ID         \ invalid or provisional (uncommitted) product-field id
7149 constant E-PF-TX         \ stale or non-LIFO field transaction, or reset while a field transaction is open
7150 constant E-PF-OWNER      \ invalid family / optional-variant ownership
7125 constant E-PF-NAME       \ reserved field tail
7126 constant E-PF-SCHEMA     \ malformed or owner-incompatible schema
7127 constant E-PF-LAYOUT     \ invalid field layout metadata / policy
7128 constant E-PF-FLAGS      \ undefined field flag bits
7132 constant E-TFAM-PAYLOAD  \ malformed or mixed unified payload metadata

private

variable TF-PUB               \ private first-public-match accumulator (-1 = none)

\ declaration parameter spelling.  This is the one reversible alphabet used by
\ every declaration parser and signature generator.  The concrete scalar tokens
\ f, n, and r are deliberately absent, so their meaning never depends on arity.
\ The registry may store wider internal schemas; this count bounds only the
\ source-language spelling of positional declaration parameters.

public

23 constant TFAM-DECL-PARAM-COUNT

private

create TFAM-DECL-PARAM-ALPHABET
   97 c,  98 c,  99 c,  100 c, 101 c,
   103 c, 104 c, 105 c, 106 c, 107 c,
   108 c, 109 c, 111 c, 112 c, 113 c,
   115 c, 116 c, 117 c, 118 c, 119 c,
   120 c, 121 c, 122 c,

public

: TFAM-DECL-PARAM>CHAR ( n -- n bool ) {: index:n :}
   index 0 < index TFAM-DECL-PARAM-COUNT >= or IF 0 RES-FALSE EXIT THEN
   TFAM-DECL-PARAM-ALPHABET index + c@ RES-TRUE ;

: TFAM-DECL-CHAR>PARAM ( n -- n bool ) {: char:n :}
   0 BEGIN dup TFAM-DECL-PARAM-COUNT < WHILE
      TFAM-DECL-PARAM-ALPHABET over + c@ char = IF RES-TRUE EXIT THEN
      1 +
   REPEAT
   drop 0 RES-FALSE ;

\ shared string pool. Names are interned as byte offsets; offsets stay valid
\ across a pool grow, so no stored offset ever needs rebasing.

private

32 constant TF-STR-INIT          \ small seed byte pool; grows (doubles) on demand
variable TF-STR-CAP-V   TF-STR-INIT TF-STR-CAP-V !   REG-PROTECT
: TF-STR-CAP ( -- n ) TF-STR-CAP-V @ ;
create TF-STR-BOOT   TF-STR-INIT allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TF-STR-P   TF-STR-BOOT TF-STR-P !   REG-PROTECT
: TF-STR ( -- ptr u8 ) TF-STR-P @ ;
\ The string pool's home, the DATA block it was last baked in (REG-POOL-PERSIST).
PERSISTED-PTR-VARIABLE TF-STR-HOME   TF-STR-BOOT TF-STR-HOME !   REG-PROTECT
variable TF-STR-HOME-CAP   TF-STR-INIT TF-STR-HOME-CAP !   REG-PROTECT

public

variable TF-STR-U   0 TF-STR-U !   REG-PROTECT

private

: TF-STR-GROW ( n -- ) {: need:n :}
   need TF-STR-CAP-V @ 2 * max {: nc:n :}
   TF-STR-P  TF-STR-CAP-V @  nc  REG-GROW1
   nc TF-STR-CAP-V ! ;
: TF-STR-ENSURE ( n -- ) {: add:n :}
   TF-STR-U @ add + TF-STR-CAP-V @ <= IF exit THEN
   TF-STR-U @ add + TF-STR-GROW ;
: TF-STR-SCRUB ( n n -- ) {: lo:n hi:n :} \ zero the pool bytes [lo,hi); see PF-SCRUB
   hi lo ?do 0 TF-STR i + c! loop ;

public

: TF-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u TF-STR-ENSURE                        \ grow first, then cache a stable base
   TF-STR-U @ {: off:n :}
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@   TF-STR off + i + c!
      i 1 +
   REPEAT drop
   TF-STR-U @ u + TF-STR-U !
   off ;
: TF-OFF$ ( n n -- ptr u8 n ) {: off:n u:n :}
   TF-STR off + u ;

\ canonical tail validation. Declarations accept only lowercase tokens; the
\ registry never folds case, so an uppercase/mixed-case token is rejected here.

private

: TF-LOWER? ( n -- bool ) {: c:n :} c 97 >= c 122 <= and ;   \ a-z
: TF-DIGIT? ( n -- bool ) {: c:n :} c 48 >= c 57 <= and ;    \ 0-9
: TF-UPPER? ( n -- bool ) {: c:n :} c 65 >= c 90 <= and ;    \ A-Z
: TF-TAILBYTE? ( n -- bool ) {: c:n :}   \ lowercase, digit, or internal hyphen
   c TF-LOWER? IF RES-TRUE EXIT THEN
   c TF-DIGIT? IF RES-TRUE EXIT THEN
   c 45 = ;                                                  \ '-'

public

: TF-HIDDEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ compaction-hidden field name
   u 0= IF RES-FALSE EXIT THEN
   a c@ 64 = ;                                              \ leading '@'

private

: TF-HYPHEN-BAD? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}   \ '-' at an edge or doubled
   i 0 = IF RES-TRUE EXIT THEN                      \ leading '-a'
   i u 1 - = IF RES-TRUE EXIT THEN                  \ trailing 'a-'
   a i + 1 - c@ 45 = ;                              \ previous byte also '-' -> 'a--b'

public

: TF-CANON? ( ptr u8 n -- bool ) {: a:ptr u:n :}    \ tailbytes + internal single '-' + >=1 letter
   u 0= IF RES-FALSE EXIT THEN
   a u CK-NAME-SPAN? 0= IF RES-FALSE EXIT THEN      \ a length no name has is no tail
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ TF-TAILBYTE? 0= IF RES-FALSE EXIT THEN
      a i + c@ 45 = IF
         a u i TF-HYPHEN-BAD? IF RES-FALSE EXIT THEN
      THEN
      i 1 +
   REPEAT drop
   0 BEGIN dup u < WHILE                             \ require at least one letter
      {: i:n :}
      a i + c@ TF-LOWER? IF RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   RES-FALSE ;
: TF-REQUIRE-CANON ( ptr u8 n -- )   \ reject uppercase/mixed-case at declaration
   TF-CANON? 0= IF E-TFAM-CASE throw THEN ;

\ param-kind pool (one PK-* cell per parameter of every family, contiguous).

private

4 constant TF-PK-INIT            \ small seed param-kind pool; grows on demand
variable TF-PK-CAP-V   TF-PK-INIT TF-PK-CAP-V !   REG-PROTECT
: TF-PK-CAP ( -- n ) TF-PK-CAP-V @ ;
create TF-PK-BOOT   TF-PK-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TF-PK-P   TF-PK-BOOT TF-PK-P !   REG-PROTECT
: TF-PK-BASE ( -- ptr n ) TF-PK-P @ ;

public

variable TF-PK-N   0 TF-PK-N !   REG-PROTECT

private

: TF-PK-GROW ( n -- ) {: need:n :}
   need TF-PK-CAP-V @ 2 * max {: nc:n :}
   TF-PK-P  TF-PK-CAP-V @ cells  nc cells  REG-GROW1
   nc TF-PK-CAP-V ! ;
: TF-PK-ENSURE ( -- )
   TF-PK-N @ TF-PK-CAP-V @ < IF exit THEN
   TF-PK-N @ 1 + TF-PK-GROW ;
: TF-PK-SCRUB ( n n -- ) {: lo:n hi:n :}  \ zero the param-kind cells [lo,hi); see PF-SCRUB
   TF-PK-BASE lo hi ARENA-CELLS-ZERO ;
: TF-PK+ ( n -- ) {: k:n :}
   TF-PK-ENSURE
   k TF-PK-N @ cells TF-PK-BASE + !
   TF-PK-N @ 1 + TF-PK-N ! ;

\ TFAM record arena.
\ Every registry record below contains only scalar ids, counts, codes, or
\ interned offsets, so its arena needs no pointer relocation.

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
\ TF.OPAQUE: nonzero when the family's generated words take the private-family
\ placement (the OPAQUE header clause, structure-decl.f OPAQUE-CLAUSE).
20 cells constant TF.OPAQUE-OFF

public

21 cells constant TF-REC
private

: TF.PKG-OFF ( ptr a -- ptr a ) TF.PKG-OFF-AT + ;
: TF.PKG-U ( ptr a -- ptr a ) TF.PKG-U-OFF + ;
: TF.VIS ( ptr a -- ptr a ) TF.VIS-OFF + ;

public

: TF.NAME-OFF ( ptr a -- ptr a ) TF.NAME-OFF-AT + ;
: TF.NAME-U ( ptr a -- ptr a ) TF.NAME-U-OFF + ;

private

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
: TF.OPAQUE ( ptr a -- ptr a ) TF.OPAQUE-OFF + ;


4 constant TF-CAP-INIT
variable TF-CAP-V   TF-CAP-INIT TF-CAP-V !   REG-PROTECT
: TF-CAP ( -- n ) TF-CAP-V @ ;
create TF-A-BOOT   TF-CAP-INIT TF-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TF-A-P   TF-A-BOOT TF-A-P !   REG-PROTECT
: TF-BASE ( -- ptr n ) TF-A-P @ ;

public

variable TFAM-N   0 TFAM-N !   REG-PROTECT

private

: TF-GROW ( n -- ) {: need:n :}
   need TF-CAP-V @ 2 * max {: nc:n :}
   TF-A-P  TF-CAP-V @ TF-REC *  nc TF-REC *  REG-GROW1
   nc TF-CAP-V ! ;
: TF-ENSURE ( -- )
   TFAM-N @ TF-CAP-V @ < IF exit THEN
   TFAM-N @ 1 + TF-GROW ;
: TF-SCRUB ( n n -- ) {: lo:n hi:n :}     \ zero the family records [lo,hi); see PF-SCRUB
   TF-BASE  lo TF-REC CELL / *  hi TF-REC CELL / *  ARENA-CELLS-ZERO ;

public

: TF-REC@ ( n -- ptr n ) {: id:n :}
   id 0 < IF s" tfam: bad family id" 76 die THEN
   id TFAM-N @ >= IF s" tfam: bad family id" 76 die THEN
   id TF-REC * TF-BASE + ;

: TFAM-N@ ( -- n ) TFAM-N @ ;            \ family high-water (rollback/tests)
: TF-STR-U@ ( -- n ) TF-STR-U @ ;        \ interned string-pool high-water
: TF-PK-N@ ( -- n ) TF-PK-N @ ;          \ param-kind pool high-water

\ --- read-only queries.
: TFAM-PKG$ ( n -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.PKG-OFF @ r TF.PKG-U @ TF-OFF$ ;

;package

using TFAM

\ Five names stay global because the engine needs them there. TFAM-NAME$,
\ TFL-CON-FAM?, TFL-CVAR? and TFL-MATCH-FAM? are the `construct`/`match`/`;match`
\ bridge: habu2.f resolves them through C-FIND-GLOBAL, which zeroes the open
\ package's wordlist cells so a keyword's meaning cannot depend on where it is
\ written, and bootstrap/cg/forth.fs mirrors the four spellings. TFAM-CTOR-WORD?
\ is named by an AOT-captured call site, and the boot seed re-resolves a baked
\ callee only through a global scope. Each sits between `;package` and
\ `package TFAM` and reads the registry through `using TFAM`;
\ test/internal-word-gate.f pins that the surface stops there.
: TFAM-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id TF-REC@ {: r:ptr :}  r TF.NAME-OFF @ r TF.NAME-U @ TF-OFF$ ;

package TFAM

public

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
: TFAM-LAYOUT? ( n -- bool ) {: id:n :}
   id TFAM-PRODUCT? id TFAM-SUM? or id TFAM-ENUM? or ;

\ --- opt-in derived words: a DERIVE clause marks the family row; the sumtype
\ generator emits the derived words and the ctor-word predicate below recognizes
\ their fixed tails like generated constructors.
1 constant DRV-EQ
2 constant DRV-HASH
4 constant DRV-ADDR              \ address surface: one accessor per field (sumtype.f TDECL-ADDR-FIELD-WORD)
8 constant DRV-INIT              \ initialized-record field accessors
DRV-EQ DRV-HASH or DRV-ADDR or DRV-INIT or constant DRV-ALL

private

: TFAM-DERIVE@ ( n -- n ) TF-REC@ TF.DERIVE @ ;

public

: TFAM-DERIVE-EQ! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-EQ or swap ! ;
: TFAM-DERIVE-EQ? ( n -- bool ) TFAM-DERIVE@ DRV-EQ and 0 <> ;
: TFAM-DERIVE-HASH! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-HASH or swap ! ;
: TFAM-DERIVE-HASH? ( n -- bool ) TFAM-DERIVE@ DRV-HASH and 0 <> ;
: TFAM-DERIVE-ADDR! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-ADDR or swap ! ;
: TFAM-DERIVE-ADDR? ( n -- bool ) TFAM-DERIVE@ DRV-ADDR and 0 <> ;
: TFAM-DERIVE-INIT! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-INIT or swap ! ;
: TFAM-DERIVE-INIT? ( n -- bool ) TFAM-DERIVE@ DRV-INIT and 0 <> ;
: TFAM-DERIVE-ANY? ( n -- bool ) TFAM-DERIVE@ 0 <> ;

\ The OPAQUE header clause: the family stays as visible as it was declared, and
\ every word it GENERATES takes the private-family placement (bare FAMILY-MEMBER
\ spelling in the declaring package's private wordlist, protected by name).
\ TFAM-GEN-PUBLIC? is the one predicate generation and recognition read; type
\ resolution keeps reading TFAM-PUBLIC?. No TFM-EPOCH bump, as the derive
\ setters: the TFM-STORE memo reads TFAM-OPAQUE?, but the header clause runs
\ before the field commit that moves the epoch, so no answer predates it.
: TFAM-OPAQUE! ( n -- ) TF-REC@ TF.OPAQUE -1 swap ! ;
: TFAM-OPAQUE? ( n -- bool ) TF-REC@ TF.OPAQUE @ 0 <> ;
: TFAM-GEN-PUBLIC? ( n -- bool ) {: fam:n :}
   fam TFAM-PUBLIC? fam TFAM-OPAQUE? 0= and ;

\ A boxed value is one pointer and a niche value one cell with the discriminant
\ folded into the payload, so the width branch keys on the policy before the
\ kind. Only TFAM-LAYOUT! reaches them (the POLICY clause refuses boxed and
\ niche-null).

private

: TFAM-BOXED-OR-NICHE? ( n -- bool ) {: id:n :}   \ policy collapses the value to one cell
   id TFAM-LAYOUT-POLICY@ {: p:n :}
   p TL-BOXED = p TL-NICHE = or ;

\ Logical width in stack cells. Under stack-cell-tag a sum is W = M + 1 cells, M
\ the widest variant payload: payload slots in declared order, slot0 deepest,
\ padding above a shorter payload, the tag on top. An enum is its tag alone; a
\ product is its field cells in slot order, with no tag; a cell, evidence,
\ boxed (one pointer) or niche-null (null is the empty variant) family is one
\ cell. packed keeps the stack cells and adds only a memory descriptor
\ (PACKED-DESC) that narrows the tag. TFAM-WIDTH@ is the declared width, every
\ parameter one cell; TFAM-INST-WIDTH@ is the width of an instantiated term. In
\ the checker (checker.f PUSH-LOGICAL) a layout type whose width is known,
\ linear included, expands to its W hidden physical terms and generic stack
\ words move whole bundles; one whose width reads an open argument stays one
\ logical cell. A sum whose payload may be linear is linear. render.f compacts a
\ hidden run back to family<args>.

public

: TFAM-WIDTH@ ( n -- n ) {: id:n :}
   id TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   id TFAM-SUM? id TFAM-ENUM? or IF id TFAM-SLOTS@ 1 + EXIT THEN
   id TFAM-PRODUCT? IF id TFAM-SLOTS@ EXIT THEN
   1 ;

\ TFM-EPOCH dates the family facts memo (TFM, further down). Every event that can
\ change a memoized answer moves it: the four mutators below, a variant or field
\ row appended, the field commit watermark moving, and any rewind that retires a
\ family, variant or field row. Rows never change in place otherwise, so an
\ answer stamped with the current epoch is still the answer a walk would give.
\ It starts at 1 so a zeroed memo cell never looks current.

private

variable TFM-EPOCH   1 TFM-EPOCH !   REG-PROTECT

public

: TFM-EPOCH+ ( -- ) TFM-EPOCH @ 1 + TFM-EPOCH ! ;

\ --- friend-only field mutators (populated by later declaration passes / tests).
: TFAM-LAYOUT! ( n n -- ) {: id:n p:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   p id TF-REC@ TF.LAYOUT !
   TFM-EPOCH+ ;
: TFAM-SLOTS! ( n n -- ) swap TF-REC@ TF.SLOTS !  TFM-EPOCH+ ;
\ An empty range (count 0) is stored as start 0 count 0, never at the
\ store's high-water: it names no row, so its start is the same whatever
\ an earlier file left in the store.
: TFAM-VAR-RANGE! ( n n n -- ) {: id:n s:n c:n :}
   c 0= IF 0 ELSE s THEN id TF-REC@ TF.VAR-START !
   c id TF-REC@ TF.VAR-COUNT !
   TFM-EPOCH+ ;
: TFAM-FLD-RANGE! ( n n n -- ) {: id:n s:n c:n :}
   c 0= IF 0 ELSE s THEN id TF-REC@ TF.FLD-START !
   c id TF-REC@ TF.FLD-COUNT !
   TFM-EPOCH+ ;
: TFAM-TAGW! ( n n -- ) swap TF-REC@ TF.TAGW ! ;
: TFAM-SCHEMA-ROOT! ( n n -- ) swap TF-REC@ TF.SCHEMA-ROOT ! ;
: TFAM-SPAN! ( n n n -- ) {: id:n off:n u:n :}
   off id TF-REC@ TF.SPAN-OFF !  u id TF-REC@ TF.SPAN-U ! ;
: TFAM-PK! ( n n n -- ) {: id:n i:n k:n :}
   i 0 < i id TFAM-ARITY@ >= or IF s" tfam: bad param index" 76 die THEN
   k 0 < k PK-MAX > or IF E-TFAM-KIND throw THEN
   k id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + ! ;

\ --- matching and lookup.

private

: TFAM-PKG-MATCH? ( ptr u8 n n -- bool ) {: pa:ptr pu:n id:n :}
   id TFAM-PKG$ pa pu CORE-STR=CI ;
: TFAM-NAME-MATCH? ( ptr u8 n n -- bool ) {: na:ptr nu:n id:n :}
   id TFAM-NAME$ na nu CORE-STR= ;

\ --- tail index (TFX). TFAM-FIND-IN and TFAM-FIND-PUBLIC each ask about one
\ tail, so they walk the rows sharing its hash bucket instead of [0, TFAM-N).
\ Rows are pushed on their bucket in id order and retired newest-first, so a
\ retired row is always at its bucket head and rewinding TFAM-N is the chain's
\ retirement. The bucket count follows the record arena's capacity; a capacity
\ change or a rewind outside the retire seam rebuilds the whole index.

public

64 constant TFX-SLOTS-INIT              \ power of two; grown to keep load <= 1/2

private

variable TFX-SLOTS-V   TFX-SLOTS-INIT TFX-SLOTS-V !   REG-PROTECT

public

: TFX-SLOTS ( -- n ) TFX-SLOTS-V @ ;

private

create TFX-A-BOOT   TFX-SLOTS-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TFX-A-P   TFX-A-BOOT TFX-A-P !   REG-PROTECT
: TFX-BASE ( -- ptr n ) TFX-A-P @ ;
variable TFX-READY   0 TFX-READY !   REG-PROTECT

public

variable TFX-HI      0 TFX-HI !       REG-PROTECT

private

variable TFX-CAP     0 TFX-CAP !      REG-PROTECT   \ the TF-CAP the buckets were sized for
variable TFX-H                \ private hash accumulator
variable TFX-I                \ private build/retire index
variable TFX-CUR              \ private bucket-walk cursor

: TFX-BKT ( n -- ptr n ) {: slot:n :}
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

: TFX-ROW-BKT ( n -- ptr n ) {: id:n :}
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

\ Pops rows [newn, TFAM-N) before TFAM-N rewinds to newn, newest first so each
\ popped row is at its bucket head. It dies when the counter has already
\ rewound: rows in [TFAM-N, TFX-HI) are then chained under ids the store no
\ longer holds, the loop would pop nothing, and the stamp would erase
\ TFX-ENSURE's only rebuild signal, leaving a chained row that kills TF-REC@ on
\ a later lookup. Dying here names the caller that broke the order.
: TFX-RETIRE ( n -- ) {: newn:n :}
   TFX-READY @ 0= IF EXIT THEN
   TFX-HI @ TFAM-N @ > IF s" tfam: tail index retired after its rows went" 76 die THEN
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

public

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

: FILE-IDENTITY$ ( n -- ptr u8 n ptr u8 n bool ) {: id:n :}
   id 0 < id TFAM-N @ >= or IF
      NULL-PTR 0 NULL-PTR 0 RES-FALSE EXIT THEN
   id TFAM-PKG$ id TFAM-NAME$ RES-TRUE ;

: FILE-FIND ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;

\ TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool ) : the SPECIFICATION of what
\ the index answers — the lowest row matching (package, tail), by walking the
\ registry. test/checker-scan-index-suite.f differentials the two.
: TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   0 BEGIN dup TFAM-N @ < WHILE
      {: i:n :}
      pa pu i TFAM-PKG-MATCH? IF
         na nu i TFAM-NAME-MATCH? IF i RES-TRUE EXIT THEN
      THEN
      i 1 +
   REPEAT drop
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

private

\ Explicit imports choose a public family before the implicit public fallback.
\ Repeating an import still names the same family; two distinct matches reject.
: TFAM-FIND-USED ( ptr u8 n -- n bool ) {: na:ptr nu:n :}
   -1 TF-PUB !
   CHECKER-USING-N 0 ?DO
      i CHECKER-USING$ na nu TFAM-FIND-IN IF
         {: id:n :}
         id TFAM-PUBLIC? IF
            TF-PUB @ 0< IF id TF-PUB !
            ELSE id TF-PUB @ <> IF E-TFAM-AMBIG throw THEN THEN
         THEN
      ELSE drop THEN
   LOOP
   TF-PUB @ 0< IF 0 RES-FALSE ELSE TF-PUB @ RES-TRUE THEN ;

public

\ Unqualified lexical order is the active package's exact row (private or
\ public), the exact global row, explicit imports, then the unique public
\ fallback. Multiple eligible families at either import/fallback step throw
\ E-TFAM-AMBIG. At top level the first exact
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
   na nu TFAM-FIND-USED IF RES-TRUE EXIT THEN
   drop
   na nu TFAM-FIND-PUBLIC ;

\ --- declaration. Storage only ever sees canonical lowercase tails.

private

: TFAM-KIND-VALID? ( n -- bool ) {: k:n :} k 0 >= k TK-MAX <= and ;
: TFAM-PK-RESERVE ( n -- ) {: k:n :}        \ default every parameter to PK-CELL
   0 BEGIN dup k < WHILE
      PK-CELL TF-PK+
      1 +
   REPEAT drop ;

public

: TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n )
   {: pa:ptr pu:n vis:n na:ptr nu:n arity:n kind:n :}
   WRITE-WINDOW-CK                        \ src/core/checker.f WRITE-WINDOW
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
   0 r TF.OPAQUE !
   id TFX-ADD                             \ the row's tail is written: it can be found now
   arity TFAM-PK-RESERVE
   id ;
NOMINAL-PROVIDER

\ SUMV: sum/enum variant records, keyed by (family-id, variant tail).

private

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

public

10 cells constant SUMV-REC
private

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


4 constant SUMV-CAP-INIT
variable SUMV-CAP-V   SUMV-CAP-INIT SUMV-CAP-V !   REG-PROTECT
: SUMV-CAP ( -- n ) SUMV-CAP-V @ ;
create SUMV-A-BOOT   SUMV-CAP-INIT SUMV-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE SUMV-A-P   SUMV-A-BOOT SUMV-A-P !   REG-PROTECT
: SUMV-BASE ( -- ptr n ) SUMV-A-P @ ;

public

variable SUMV-N   0 SUMV-N !   REG-PROTECT

private

: SUMV-GROW ( n -- ) {: need:n :}
   need SUMV-CAP-V @ 2 * max {: nc:n :}
   SUMV-A-P  SUMV-CAP-V @ SUMV-REC *  nc SUMV-REC *  REG-GROW1
   nc SUMV-CAP-V ! ;
: SUMV-ENSURE ( -- )
   SUMV-N @ SUMV-CAP-V @ < IF exit THEN
   SUMV-N @ 1 + SUMV-GROW ;
: SUMV-SCRUB ( n n -- ) {: lo:n hi:n :}   \ zero the variant records [lo,hi); see PF-SCRUB
   SUMV-BASE  lo SUMV-REC CELL / *  hi SUMV-REC CELL / *  ARENA-CELLS-ZERO ;
: SUMV-REC@ ( n -- ptr n ) {: id:n :}
   id 0 < IF s" tfam: bad variant id" 76 die THEN
   id SUMV-N @ >= IF s" tfam: bad variant id" 76 die THEN
   id SUMV-REC * SUMV-BASE + ;

public

: SUMV-FAM@ ( n -- n ) SUMV-REC@ SV.FAM @ ;
: SUMV-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id SUMV-REC@ {: r:ptr :}  r SV.NAME-OFF @ r SV.NAME-U @ TF-OFF$ ;
: SUMV-TAG@ ( n -- n ) SUMV-REC@ SV.TAG @ ;
: SUMV-SCH-START@ ( n -- n ) SUMV-REC@ SV.SCH-START @ ;
: SUMV-SCH-COUNT@ ( n -- n ) SUMV-REC@ SV.SCH-COUNT @ ;

private

: SUMV-RAW-PAYCELLS@ ( n -- n ) SUMV-REC@ SV.PAYCELLS @ ;

public

: SUMV-N@ ( -- n ) SUMV-N @ ;

\ generated-constructor metadata. A PUBLIC sum/enum family stores its
\ derived constructor package name (interned offset+len) in every variant; the
\ checker symbol for the generated constructor word lands in SV.CTOR-SYM. Private
\ families export nothing, so both stay zero. All three cells are integers /
\ interned offsets, so the existing SUMV snapshot bake persists them verbatim.
: SUMV-CTOR-PKG! ( n n n -- ) {: id:n off:n u:n :}
   off id SUMV-REC@ SV.CTOR-PKG-OFF !   u id SUMV-REC@ SV.CTOR-PKG-U ! ;
: SUMV-CTOR-PKG$ ( n -- ptr u8 n ) {: id:n :}
   id SUMV-REC@ {: r:ptr :}  r SV.CTOR-PKG-OFF @ r SV.CTOR-PKG-U @ TF-OFF$ ;
: SUMV-CTOR-SYM@ ( n -- n ) SUMV-REC@ SV.CTOR-SYM @ ;

\ --- constructor-symbol index (SVX). SUMV-FROM-CTOR-SYM runs on every resolved
\ call token and answers no for almost all, so it must not walk the variant
\ store. It answers the lowest variant id carrying the symbol; a symbol is
\ written once per variant, so a per-symbol head that keeps its first writer
\ reproduces it. Variant rows leave only by SUMV-N rewinding, lowest id first,
\ so a head at a retired row means the symbol has no row left and SVX-TRUNCATE
\ needs no back-link. SVX-HI is the SUMV-N the index covers; every seam that
\ moves SUMV-N moves it (SUMV-ADD, SVX-TRUNCATE, the unit imports, TFAM-RESET),
\ so a difference catches a rewind outside those seams. SVX-GEN catches the
\ shared mapping being dropped or re-laid out.

private

variable SVX-GEN

public

\ REG-PROTECT: `99999 SVX-HI !` turned a catchable duplicate-family reject into
\ `tfam: ctor index retired after its rows went`, exit 76.
variable SVX-HI   REG-PROTECT
0 SVX-GEN !   0 SVX-HI !

: SVX@ ( n -- n ) HT-SVX IDX-HEAD@ ;

private

: SVX! ( n n -- ) HT-SVX IDX-HEAD! ;

: SVX-STAMP ( -- ) SUMV-N @ SVX-HI ! ;

: SVX-SYNC ( -- )
   SUMV-N @ SVX-HI @ < IF 0 SVX-GEN ! THEN ;

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
   SUMV-N @ 0 ?do i dup SUMV-CTOR-SYM@ SVX-LINK loop
   SVX-STAMP
   HIDX-GEN @ SVX-GEN ! ;

public

: SVX-ENSURE ( -- )
   HIDX-ENSURE
   SVX-SYNC
   SVX-GEN @ HIDX-GEN @ <> IF SVX-BUILD THEN ;

\ SVX-TRUNCATE ( n -- ) : called before SUMV-N rewinds to `newn`. It refuses to
\ run after the fact for the reason TFX-RETIRE gives: SVX-HI above SUMV-N means
\ the store already shrank under the index, and stamping SVX-HI down then would
\ throw away the rewind SVX-SYNC exists to catch.
\ An index at a stale generation has nothing to retire and no rewind left for
\ SVX-SYNC to catch (SVX-GEN already forces SVX-BUILD), so its mark follows the
\ store: a cache hit leaves the index stale where a cold load leaves it current,
\ and both must end at the same SVX-HI.

private

: SVX-TRUNCATE ( n -- ) {: newn:n :}
   SVX-GEN @ HIDX-GEN @ <> IF newn SVX-HI ! EXIT THEN
   SVX-HI @ SUMV-N @ > IF s" tfam: ctor index retired after its rows went" 76 die THEN
   SUMV-N @ newn ?do
      i SUMV-CTOR-SYM@ {: sym:n :}
      sym 0 <> IF
         sym SVX@ i 1 + = IF 0 sym SVX! THEN
      THEN
   loop
   newn SVX-HI ! ;

\ A variant owns exactly one generated constructor. Rewriting the cell would
\ strand the old symbol's head on a row that no longer carries it, so the second
\ write is refused rather than silently indexed wrong.

public

: SUMV-CTOR-SYM! ( n n -- ) {: vid:n sym:n :}
   vid SUMV-CTOR-SYM@ 0 <> IF s" tfam: variant constructor symbol already set" 76 die THEN
   SVX-ENSURE
   sym vid SUMV-REC@ SV.CTOR-SYM !
   vid sym SVX-LINK ;

\ SUMV-CTOR-FIRST-LINEAR ( n -- n ) : the SPECIFICATION of what SVX answers —
\ the lowest variant id (+1) with this constructor symbol, by walking the store.
\ test/checker-scan-index-suite.f differentials the indexed answer against it.
: SUMV-CTOR-FIRST-LINEAR ( n -- n ) {: sym:n :}
   SUMV-N @ 0 ?do
      i SUMV-CTOR-SYM@ sym = IF i 1 + unloop EXIT THEN
   loop
   0 ;

\ --- variant tail index (VNX). SUMV-FIND wants one tail in one family and the
\ generated-word recognizer every row spelled like one tail in any family;
\ generating an ENUM's constructors asks each once per variant, so a store walk
\ makes the declaration quadratic in its variants. Rows chain by the hash of
\ their folded tail, which serves both: the exact comparison still rejects a
\ non-canonical spelling, and every case-insensitive match sits in one bucket.
\ Rows are pushed in id order by SUMV-ADD and retired newest-first by
\ TFAM-REWIND, so a retired row is always its bucket's head. Buckets and links
\ are process-local: a capture drops them and a capacity change rebuilds them. A
\ store that moved outside those seams is rebuilt by the next lookup, and
\ refused by a rewind that comes first (VNX-RETIRE).

private

8 constant VNX-SLOTS-INIT               \ power of two; grown to keep load <= 1/2
variable VNX-SLOTS-V   VNX-SLOTS-INIT VNX-SLOTS-V !   REG-PROTECT
: VNX-SLOTS ( -- n ) VNX-SLOTS-V @ ;
create VNX-B-BOOT   VNX-SLOTS-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE VNX-B-P   VNX-B-BOOT VNX-B-P !   REG-PROTECT
create VNX-L-BOOT   SUMV-CAP-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE VNX-L-P   VNX-L-BOOT VNX-L-P !   REG-PROTECT
variable VNX-LCAP   SUMV-CAP-INIT VNX-LCAP !   REG-PROTECT   \ rows the link array holds
variable VNX-READY   0 VNX-READY !   REG-PROTECT

public

variable VNX-HI      0 VNX-HI !      REG-PROTECT   \ SUMV-N the chains cover

private

variable VNX-CAP     0 VNX-CAP !     REG-PROTECT   \ SUMV-CAP the buckets were sized for
variable VNX-CUR              \ private bucket-walk cursor

: VNX-BKT ( n -- ptr n ) {: slot:n :}   \ bucket head: row id + 1, 0 = empty
   slot cells VNX-B-P @ + ;
: VNX-LINK ( n -- ptr n ) {: id:n :}    \ next row id + 1 in the row's bucket
   id cells VNX-L-P @ + ;

: VNX-HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   HIDX-FNV-BASIS
   0 BEGIN dup u < WHILE
      dup a + c@ CORE-FOLD-C rot xor HIDX-FNV-PRIME * swap
      1 +
   REPEAT drop
   VNX-SLOTS 1 - and ;

: VNX-ROW-BKT ( n -- ptr n ) SUMV-NAME$ VNX-HASH VNX-BKT ;

: VNX-PUSH ( n -- ) {: id:n :}
   id VNX-ROW-BKT {: b:ptr :}
   b @ id VNX-LINK !
   id 1 + b ! ;

: VNX-POP ( n -- ) {: id:n :}
   id VNX-ROW-BKT {: b:ptr :}
   b @ id 1 + <> IF s" tfam: variant tail index corrupt" 76 die THEN
   id VNX-LINK @ b ! ;

\ The links follow the variant store's capacity and the buckets keep the load
\ factor at or below one half. Both only ever grow: SUMV-CAP never shrinks.
: VNX-RESIZE ( -- )
   SUMV-CAP VNX-CAP !
   SUMV-CAP VNX-LCAP @ > IF
      VNX-L-P  VNX-LCAP @ cells  SUMV-CAP cells  REG-GROW1
      SUMV-CAP VNX-LCAP !
   THEN
   VNX-SLOTS-INIT BEGIN dup SUMV-CAP 2 * < WHILE 2 * REPEAT {: need:n :}
   need VNX-SLOTS <= IF EXIT THEN
   VNX-B-P  VNX-SLOTS cells  need cells  REG-GROW1
   need VNX-SLOTS-V ! ;

: VNX-BUILD ( -- )
   VNX-RESIZE
   VNX-B-P @ 0 VNX-SLOTS ARENA-CELLS-ZERO
   0 BEGIN dup SUMV-N @ < WHILE dup VNX-PUSH 1 + REPEAT drop
   SUMV-N @ VNX-HI !
   -1 VNX-READY ! ;

\ Three cell reads on every lookup: built, covering exactly the live rows, and
\ sized for the store's current capacity.
: VNX-ENSURE ( -- )
   VNX-READY @ 0=
   SUMV-N @ VNX-HI @ <> or
   VNX-CAP @ SUMV-CAP <> or IF VNX-BUILD THEN ;

\ Pops rows [newn, SUMV-N) before SUMV-N rewinds to newn, newest first, so each
\ popped row is at its bucket head. It dies, as TFX-RETIRE does, on chains that
\ do not cover exactly the live rows: SUMV-ADD, UNIT-FAM-TAKE and TFAM-REWIND
\ move VNX-HI with SUMV-N, and TFAM-RESET and REG-AOT-INSTALL drop the chains, so
\ unequal marks mean a caller moved the counter around those seams, and popping
\ would unlink rows the chains never held or erase the signal VNX-ENSURE rebuilds
\ on.
: VNX-RETIRE ( n -- ) {: newn:n :}
   VNX-READY @ 0= IF EXIT THEN
   VNX-HI @ SUMV-N @ <> IF s" tfam: variant tail index retired out of step with its rows" 76 die THEN
   SUMV-N @ 1 -
   BEGIN dup newn >= WHILE dup VNX-POP 1 - REPEAT drop
   newn VNX-HI ! ;

\ Capture: the grown buffers are process-local, so the image keeps the boot
\ stores and the restored process rebuilds. Mirrors TFX-SNAP-RESET.
: VNX-SNAP-RESET ( -- )
   VNX-B-BOOT VNX-B-P !   VNX-SLOTS-INIT VNX-SLOTS-V !
   VNX-L-BOOT VNX-L-P !   SUMV-CAP-INIT VNX-LCAP !   0 VNX-CAP !
   0 VNX-READY !   0 VNX-HI !   0 VNX-CUR ! ;

public

\ Generated-constructor protection: names match the recorded SV.CTOR-PKG
\ spellings case-insensitively, so a folded alias cannot reopen a constructor
\ package, extend it with a new tail, or undefine a generated word. Installed
\ into the checker's CTOR-*-XT friend cells at the end of this file.
private

variable TF-CW-COL          \ first-colon split position
: SUMV-CTOR-PKG-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id SUMV-REC@ SV.CTOR-PKG-U @ 0= IF RES-FALSE EXIT THEN
   id SUMV-CTOR-PKG$ a u CORE-STR=CI ;
: TFAM-CTOR-PKG? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a recorded ctor package name?
   SUMV-N @ 0 ?do
      a u i SUMV-CTOR-PKG-MATCH? IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

public

: TF-CW-SPLIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ first non-edge ':' -> TF-CW-COL
   -1 TF-CW-COL !
   u 0 ?do
      a i + c@ 58 = IF i TF-CW-COL ! leave THEN
   loop
   TF-CW-COL @ 0 > TF-CW-COL @ u 1 - < and ;
: TFAM-CTOR-WORD-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}   \ split name = ctor id?
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 -  id SUMV-NAME$ CORE-STR=CI ;

private

: TF-CW-TAIL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ name tail after the split colon
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 - ;

public

: TFAM-DERIVED-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a fixed generator-owned derived tail?
   a u s" eq" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" hash" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag" CORE-STR=CI ;

private

: TFAM-DERIVED-KIND-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}   \ derived tail the FAMILY generates
   a u s" eq" CORE-STR=CI IF fam TFAM-DERIVE-EQ? EXIT THEN
   a u s" hash" CORE-STR=CI IF fam TFAM-DERIVE-HASH? EXIT THEN
   fam TFAM-PRODUCT? IF RES-FALSE EXIT THEN   \ products get no discriminant
   a u s" tag" CORE-STR=CI ;                  \ tag rides ANY derive on sum/enum

public

: TFAM-DERIVED-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}   \ split name = id-family derived word?
   id SUMV-FAM@ TFAM-DERIVE-ANY? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ id SUMV-FAM@ TFAM-DERIVED-KIND-TAIL? ;

\ The QUALIFIED half of the generated-word scan. Its private half, and the two
\ recognizers that join them, sit beside TF-CTOR-PRIV$ below: a recognizer must
\ read the spelling from the derivation that generates it, never restate it.
\ A constructor word's tail is its variant's tail, so only the rows in that
\ tail's VNX bucket can be one. A derived word is recognized from its family,
\ not a row, so only the three derived tails still walk the store.
: TF-CTOR-QUAL-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ PKG:VARIANT / derived word?
   a u TF-CW-TAIL$ {: ta:ptr tu:n :}
   VNX-ENSURE
   ta tu VNX-HASH VNX-BKT @ VNX-CUR !
   BEGIN VNX-CUR @ 0 <> WHILE
      VNX-CUR @ 1 - {: id:n :}
      a u id TFAM-CTOR-WORD-AT? IF RES-TRUE EXIT THEN
      id VNX-LINK @ VNX-CUR !
   REPEAT
   ta tu TFAM-DERIVED-TAIL? 0= IF RES-FALSE EXIT THEN
   SUMV-N @ 0 ?do
      a u i TFAM-DERIVED-AT? IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

;package

package TFAM

: SUMV-MATCH? ( n ptr u8 n n -- bool ) {: fam:n na:ptr nu:n id:n :}
   id SUMV-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id SUMV-NAME$ na nu CORE-STR= ;

public

\ The lowest matching row, as the store walk answered: a chain runs newest
\ first, so the last match it meets is the lowest.
: SUMV-FIND ( n ptr u8 n -- n bool ) {: fam:n na:ptr nu:n :}
   VNX-ENSURE
   -1
   na nu VNX-HASH VNX-BKT @ VNX-CUR !
   BEGIN VNX-CUR @ 0 <> WHILE
      VNX-CUR @ 1 - {: id:n :}
      fam na nu id SUMV-MATCH? IF drop id THEN
      id VNX-LINK @ VNX-CUR !
   REPEAT
   dup 0 < IF drop 0 RES-FALSE EXIT THEN
   RES-TRUE ;

\ SUMV-FIND-LINEAR ( n ptr u8 n -- n bool ) : the SPECIFICATION of what VNX
\ answers, by walking the store. test/checker-scan-index-suite.f differentials
\ the two.
: SUMV-FIND-LINEAR ( n ptr u8 n -- n bool ) {: fam:n na:ptr nu:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: i:n :}
      fam na nu i SUMV-MATCH? IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;
: SUMV-ADD ( n ptr u8 n n n n n -- n )
   {: fam:n na:ptr nu:n tag:n ss:n sc:n pc:n :}
   WRITE-WINDOW-CK
   na nu TF-REQUIRE-CANON
   fam na nu SUMV-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   SUMV-ENSURE
   VNX-ENSURE                             \ size the index before the slot is committed
   SVX-SYNC                               \ catch an outside rewind before the stamp moves
   SUMV-N @ {: id:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + SUMV-N !
   id SUMV-REC@ {: r:ptr :}
   fam r SV.FAM !   noff r SV.NAME-OFF !   nu r SV.NAME-U !
   \ An empty schema range is start 0 count 0, as for TFAM-VAR-RANGE!.
   tag r SV.TAG !   sc 0= IF 0 ELSE ss THEN r SV.SCH-START !
   sc r SV.SCH-COUNT !   pc r SV.PAYCELLS !
   0 r SV.CTOR-SYM !   0 r SV.CTOR-PKG-OFF !   0 r SV.CTOR-PKG-U !
   id VNX-PUSH   SUMV-N @ VNX-HI !
   SVX-STAMP                              \ a new row has no constructor symbol, so no head
   TFM-EPOCH+
   id ;

\ Constructor package name (docs/forth.md "Structures And Enums"): the same bytes
\ native, habu1 and the Gforth mirror produce (all three parse this file).
\ Uppercase every segment (package and family tail), double a literal '-' inside
\ each, join with single '-'. Escaping the tail too makes the map injective:
\ canonical segments never start or end with '-', so hyphen runs inside escaped
\ segments stay even-length and interior and each single '-' decodes uniquely.
\ Package `a-b` family `c` derives `A--B-C`; `a`+`b-c` derives `A-B--C`;
\ top-level `a-b-c` derives `A--B--C`; top level derives the bare escaped tail
\ (`result` -> `RESULT`). A derived package past 255 bytes (CHECKER-PACKAGE-CAP
\ - 1) is refused like an explicit package name.

private

$400 constant TF-CTOR-CAP        \ derived-name buffer bytes
create TF-CTOR-BUF TF-CTOR-CAP allot
variable TF-CTOR-U               \ derived-name length

public

: TF-UPPER-C ( n -- n ) {: c:n :} c TF-LOWER? IF c 32 - EXIT THEN c ;   \ a-z -> A-Z

\ The longest family, variant or field name is the longest package name
\ (CHECKER-PACKAGE-CAP - 1), so every derived spelling fits TF-CTOR-BUF: at most
\ 2*255 + 1 + 2*255 = 1021 bytes escaped, 255 + 1 + 255 + 12 for a private
\ member. sumtype.f, structure-decl.f and enum-decl.f refuse a longer name with
\ E-TDECL-CAP and TF-NAME-LONG$'s reason, and ARENA (arena.f) runs STRUCTURE's
\ gate on its name first; the dies in `TF-CTOR-C,` and TFAM-INIT-MEMBER$ guard
\ only that arithmetic.
CHECKER-PACKAGE-CAP 1 - constant TF-NAME-MAX

\ The reason refusing a name longer than TF-NAME-MAX. It is static text: no
\ number formatter loads before this file, and the refusal carries the name.
: TF-NAME-LONG$ ( -- ptr u8 n ) s" name longer than 255 bytes" ;

private

: TF-CTOR-C, ( n -- )
   TF-CTOR-U @ TF-CTOR-CAP >= IF s" tfam: constructor name too long" 76 die THEN
   TF-CTOR-BUF TF-CTOR-U @ + c!
   TF-CTOR-U @ 1 + TF-CTOR-U ! ;
: TF-CTOR-ESC ( ptr u8 n -- ) {: a:ptr u:n :}   \ one uppercased '-'->'--' escaped segment
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ dup 45 = IF
         drop 45 TF-CTOR-C, 45 TF-CTOR-C,
      ELSE TF-UPPER-C TF-CTOR-C, THEN
      i 1 +
   REPEAT drop ;
: TF-CTOR-TAIL ( ptr u8 n -- ) {: a:ptr u:n :}      \ raw uppercased private tail
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ TF-UPPER-C TF-CTOR-C,
      i 1 +
   REPEAT drop ;
: TF-CTOR-BUILD-ESCAPED ( ptr u8 n ptr u8 n -- )   \ (pkg tail)
   {: pa:ptr pu:n ta:ptr tu:n :}
   0 TF-CTOR-U !
   pu 0 > IF pa pu TF-CTOR-ESC  45 TF-CTOR-C, THEN
   ta tu TF-CTOR-ESC ;

\ The tail must already be canonical lowercase.

public

: TF-CTOR-PKG$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n ta:ptr tu:n :}
   pa pu ta tu TF-CTOR-BUILD-ESCAPED
   TF-CTOR-U @ CHECKER-PACKAGE-CAP >= IF E-PACKAGE-NAME-CAP throw THEN
   TF-CTOR-BUF TF-CTOR-U @ ;

\ Two spellings, on purpose:
\   public   PKG-FAMILY:MEMBER  in the reserved constructor namespace
\                               TF-CTOR-PKG$ derives, escaped and bounded.
\   private  FAMILY-MEMBER      in the declaring package's private wordlist:
\                               uppercased family tail, '-', uppercased member.
\ The private form needs no package segment or escaping: every use is inside the
\ declaring package. It is not injective across packages and need not be: a
\ collision with an existing private word, or between two families of one
\ package, dies "generated declaration already defined".
\ A qualified private spelling is impossible: the engine routes a qualified
\ definition name to the namespace's PUBLIC wid (habu2.f C-QUALIFY-DEF), and the
\ checker records any single-non-edge-colon name as that package's public
\ symbol before it consults the open package (checker.f CHECKER-RECORD-SYM).
: TF-CTOR-PRIV$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: ta:ptr tu:n ma:ptr mu:n :}
   0 TF-CTOR-U !
   ta tu TF-CTOR-TAIL
   45 TF-CTOR-C,
   ma mu TF-CTOR-TAIL
   TF-CTOR-BUF TF-CTOR-U @ ;

private

\ The members a private family's generator publishes, and the only names the
\ by-name guard protects: MAKE and UNMAKE (structure-make.f SM-EMIT-ROWS,
\ sumtype.f TDECL-PROD-PLAN). A further private member extends this list and
\ the recognizer needs no other change. Derived tails are recognized from the
\ family's derive flags (TFAM-DERIVED-PRIV-TAIL?) and addr accessors from the
\ field registry (TFAM-ADDR-WORD?). A private SUM or ENUM publishes no word, so
\ a variant name is not a member: inside a package declaring a private `SUMTYPE
\ colour`, an ordinary `COLOUR-RED` is the package's own word and stays
\ undefinable.
2 constant TF-PRIV-MEMBER-N
: TF-PRIV-MEMBER$ ( n -- ptr u8 n ) {: i:n :}
   i 0= IF s" make" EXIT THEN
   s" unmake" ;

: TF-PRIV-NAME? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}   \ = a member of THIS family?
   0 BEGIN dup TF-PRIV-MEMBER-N < WHILE
      {: m:n :}
      a u  fam TFAM-NAME$ m TF-PRIV-MEMBER$ TF-CTOR-PRIV$  CORE-STR=CI
         IF RES-TRUE EXIT THEN
      m 1 +
   REPEAT drop RES-FALSE ;

public

\ A private family derives too, so the private scan answers for a derived tail
\ as the qualified one does through TFAM-DERIVED-AT?. The candidate spelling
\ comes from TF-CTOR-PRIV$, so the recognizer cannot drift from TDGEN-DRV-REF.
: TFAM-PRIV-MEMBER? ( ptr u8 n ptr u8 n n -- bool ) {: a:ptr u:n ma:ptr mu:n fam:n :}
   a u  fam TFAM-NAME$ ma mu TF-CTOR-PRIV$  CORE-STR=CI ;

: TFAM-DERIVED-PRIV-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}
   fam TFAM-DERIVE-EQ? IF
      a u s" eq" fam TFAM-PRIV-MEMBER? IF RES-TRUE EXIT THEN THEN
   fam TFAM-DERIVE-HASH? IF
      a u s" hash" fam TFAM-PRIV-MEMBER? IF RES-TRUE EXIT THEN THEN
   fam TFAM-PRODUCT? IF RES-FALSE EXIT THEN   \ products get no discriminant
   a u s" tag" fam TFAM-PRIV-MEMBER? ;        \ tag rides ANY derive on sum/enum

private

\ A private (or OPAQUE) family's generated word is recognized only while its
\ declaring package is open, the only scope it resolves in; outside it the name
\ belongs to whoever spells it. Before that, a family with public generated
\ words (TFAM-GEN-PUBLIC?) wears the qualified spelling, a private sum or enum
\ publishes nothing, and a fieldless product published no member pair. Members
\ are the constructor pair, the derived tails, and the addr accessors
\ (TFAM-ADDR-WORD-XT).
: TF-CTOR-PRIV-FAM? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}
   fam TFAM-GEN-PUBLIC? IF RES-FALSE EXIT THEN
   fam TFAM-PRODUCT? 0= IF RES-FALSE EXIT THEN
   fam TFAM-VAR-COUNT@ 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u fam TF-PRIV-NAME? IF RES-TRUE EXIT THEN
   a u fam TFAM-DERIVED-PRIV-TAIL? ;

public

: TF-CTOR-PRIV-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   TFAM-N @ 0 ?do
      a u i TF-CTOR-PRIV-FAM? IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

\ `DERIVE addr` accessors are recognized from the field registry, declared
\ further down, so the predicate is a friend xt bound at its definition. It
\ joins the two scans because every consumer asks one question (does the
\ generator own this name): the undefine guard, the closed-package extra-tail
\ rule, xref.f's protected-WID assertions and the generated-declaration name
\ preflight.
defer TFAM-ADDR-WORD-XT ( ptr u8 n -- bool )

;package

\ A generated word wears either the qualified public spelling or the bare private
\ one, and the colon decides which scan answers for it. A length no name has
\ (CK-NAME-SPAN?) spells no generated word; CHECKER-UNDEFINE asks this first.
: TFAM-CTOR-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ exact generated word?
   a u CK-NAME-SPAN? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-SPLIT? IF
      a u TF-CTOR-QUAL-WORD? IF RES-TRUE EXIT THEN
   ELSE
      a u TF-CTOR-PRIV-WORD? IF RES-TRUE EXIT THEN
   THEN
   a u TFAM-ADDR-WORD-XT ;

package TFAM

\ A private family reserves no namespace, so it has no package for a stray tail
\ to extend and this stays the qualified rule alone.
\ The generated-word test runs first: it answers every generated definition
\ from the tail index, while the package test walks the variant store and is
\ needed only for a name the generator does not own.
: TFAM-CTOR-EXTEND? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ new tail in a ctor package?
   a u TF-CW-SPLIT? 0= IF RES-FALSE EXIT THEN
   TF-CW-COL @ {: col:n :}
   a u TFAM-CTOR-WORD? IF RES-FALSE EXIT THEN
   a col TFAM-CTOR-PKG? ;

public

\ shared fields, keyed by (family-id, optional-variant-id, field tail).
\ PF-N includes rows provisional in the active transaction; PF-COMMIT-N is the
\ reflection high-water. ADD never returns a row id, so an id becomes observable
\ only after the outer transaction commits.
-1 constant PF-NO-VARIANT

private

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

public

11 cells constant PF-REC
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


\ --- REG-PROTECT / IMK-SEAL-REGISTRY (util.f, internal-mark.f) seal each
\ registry control cell DNAME-INT, so a bare `<cell> @`/`<cell> !` or
\ `' <cell>` fails closed (`hb: internal engine word`, rc 70). Measured on the
\ product-field arena: `99 PF-COMMIT-N !` corrupted TYPE-FIELD:COUNT.

private

4 constant PF-CAP-INIT
variable PF-CAP-V   PF-CAP-INIT PF-CAP-V !   REG-PROTECT

public

: PF-CAP ( -- n ) PF-CAP-V @ ;

private

create PF-A-BOOT   PF-CAP-INIT PF-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE PF-A-P   PF-A-BOOT PF-A-P !   REG-PROTECT

public

: PF-BASE ( -- ptr n ) PF-A-P @ ;
variable PF-N   0 PF-N !   REG-PROTECT
variable PF-COMMIT-N   0 PF-COMMIT-N !   REG-PROTECT

private

PF-REC CELL / constant PF-REC-CELLS   \ product-field record stride, in cells

\ Zeroes the product-field records [lo,hi): rows hold only scalar ids and
\ offsets, so a zeroed row is the canonical absent row and retired rows leave no
\ bytes in reflection or snapshot/fixpoint identity.

public

: PF-SCRUB ( n n -- ) {: lo:n hi:n :}
   hi lo <= IF EXIT THEN
   PF-BASE  lo PF-REC-CELLS *  hi PF-REC-CELLS *  ARENA-CELLS-ZERO ;

private

: PF-GROW ( n -- ) {: need:n :}
   need PF-CAP-V @ 2 * max {: nc:n :}
   PF-A-P  PF-CAP-V @ PF-REC *  nc PF-REC *  REG-GROW1
   nc PF-CAP-V ! ;

public

: PF-ENSURE ( -- )
   PF-N @ PF-CAP-V @ < IF exit THEN
   PF-N @ 1 + PF-GROW ;
\ Field-id readers reject an out-of-band id with a catchable E-PF-ID throw, never
\ a process-killing `die`: the id is public reflection input (an `n` any checked
\ caller can guess), so a bad id is a caller error to surface under `catch`, not
\ an engine assert. PF-ROW admits provisional rows (bound PF-N) for in-transaction
\ machinery; PF-REC@ is the committed reflection reader (bound PF-COMMIT-N), so a
\ guessed provisional id (>= PF-COMMIT-N) rejects here before it can be read.
: PF-ROW ( n -- ptr n ) {: id:n :}
   id 0 < IF E-PF-ID throw THEN
   id PF-N @ >= IF E-PF-ID throw THEN
   id PF-REC * PF-BASE + ;

private

: PF-REC@ ( n -- ptr n ) {: id:n :}
   id 0 < IF E-PF-ID throw THEN
   id PF-COMMIT-N @ >= IF E-PF-ID throw THEN
   id PF-REC * PF-BASE + ;

public

: PF-FAM@ ( n -- n ) PF-REC@ PF.FAM @ ;
: PF-VAR@ ( n -- n ) PF-REC@ PF.VAR @ ;
: PF-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id PF-REC@ {: r:ptr :}  r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ ;
: PF-SCH@ ( n -- n ) PF-REC@ PF.SCH @ ;

private

: PF-PENDING-SCH@ ( n -- n ) PF-ROW PF.SCH @ ;

public

: PF-SLOT@ ( n -- n ) PF-REC@ PF.SLOT @ ;
: PF-CELLS@ ( n -- n ) PF-REC@ PF.CELLS @ ;
: PF-BYTE-OFF@ ( n -- n ) PF-REC@ PF.BYTE-OFF @ ;
: PF-BYTES@ ( n -- n ) PF-REC@ PF.BYTES @ ;
: PF-ALIGN@ ( n -- n ) PF-REC@ PF.ALIGN @ ;
: PF-FLAGS@ ( n -- n ) PF-REC@ PF.FLAGS @ ;
: PF-N@ ( -- n ) PF-COMMIT-N @ ;

\ --- field owner index (FVX). Every question about one owner's fields (is this
\ name taken, does this layout overlap, what is this variant's payload) wants
\ the rows of one (family, variant) pair; an ENUM asks once per field or
\ variant, so a store walk is quadratic. Rows chain by the hash of their owner;
\ a bucket can hold other owners' rows, so a reader keeps only its owner's.
\ Chains run newest first. VNX's discipline: ADD pushes each row in id order,
\ PF-REWIND pops retired rows newest first while their owner cells are intact.
\ Buckets and links are process-local: a capture drops them, and a capacity
\ change or a store moved outside those seams rebuilds them.

private

8 constant FVX-SLOTS-INIT               \ power of two; grown to keep load <= 1/2
variable FVX-SLOTS-V   FVX-SLOTS-INIT FVX-SLOTS-V !   REG-PROTECT
: FVX-SLOTS ( -- n ) FVX-SLOTS-V @ ;
create FVX-B-BOOT   FVX-SLOTS-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE FVX-B-P   FVX-B-BOOT FVX-B-P !   REG-PROTECT
create FVX-L-BOOT   PF-CAP-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE FVX-L-P   FVX-L-BOOT FVX-L-P !   REG-PROTECT
variable FVX-LCAP   PF-CAP-INIT FVX-LCAP !   REG-PROTECT   \ rows the link array holds
variable FVX-READY   0 FVX-READY !   REG-PROTECT
variable FVX-HI      0 FVX-HI !      REG-PROTECT   \ PF-N the chains cover
variable FVX-CAP     0 FVX-CAP !     REG-PROTECT   \ PF-CAP the buckets were sized for

: FVX-BKT ( n -- ptr n ) {: slot:n :}   \ bucket head: row id + 1, 0 = empty
   slot cells FVX-B-P @ + ;
: FVX-LINK ( n -- ptr n ) {: id:n :}    \ next row id + 1 in the row's bucket
   id cells FVX-L-P @ + ;

: FVX-HASH ( n n -- n ) {: fam:n var:n :}
   HIDX-FNV-BASIS fam xor HIDX-FNV-PRIME *  var xor HIDX-FNV-PRIME *
   FVX-SLOTS 1 - and ;

: PF-ROW-OWNER? ( n n ptr n -- bool ) {: fam:n var:n r:ptr :}
   r PF.FAM @ fam = r PF.VAR @ var = and ;

\ Read through PF-BASE, not PF-ROW: ADD links its row before PF-N publishes it.
: FVX-ROW-BKT ( n -- ptr n ) {: id:n :}
   id PF-REC * PF-BASE + {: r:ptr :}
   r PF.FAM @ r PF.VAR @ FVX-HASH FVX-BKT ;

: FVX-PUSH ( n -- ) {: id:n :}
   id FVX-ROW-BKT {: b:ptr :}
   b @ id FVX-LINK !
   id 1 + b ! ;

: FVX-POP ( n -- ) {: id:n :}
   id FVX-ROW-BKT {: b:ptr :}
   b @ id 1 + <> IF s" tfam: field owner index corrupt" 76 die THEN
   id FVX-LINK @ b ! ;

\ The links follow the field store's capacity and the buckets keep the load
\ factor at or below one half. Both only ever grow: PF-CAP never shrinks.
: FVX-RESIZE ( -- )
   PF-CAP FVX-CAP !
   PF-CAP FVX-LCAP @ > IF
      FVX-L-P  FVX-LCAP @ cells  PF-CAP cells  REG-GROW1
      PF-CAP FVX-LCAP !
   THEN
   FVX-SLOTS-INIT BEGIN dup PF-CAP 2 * < WHILE 2 * REPEAT {: need:n :}
   need FVX-SLOTS <= IF EXIT THEN
   FVX-B-P  FVX-SLOTS cells  need cells  REG-GROW1
   need FVX-SLOTS-V ! ;

: FVX-BUILD ( -- )
   FVX-RESIZE
   FVX-B-P @ 0 FVX-SLOTS ARENA-CELLS-ZERO
   0 BEGIN dup PF-N @ < WHILE dup FVX-PUSH 1 + REPEAT drop
   PF-N @ FVX-HI !
   -1 FVX-READY ! ;

\ Three cell reads on every lookup: built, covering exactly the live rows, and
\ sized for the store's current capacity.
: FVX-ENSURE ( -- )
   FVX-READY @ 0=
   PF-N @ FVX-HI @ <> or
   FVX-CAP @ PF-CAP <> or IF FVX-BUILD THEN ;

\ Pops rows [newn, PF-N) before PF-N rewinds to newn, newest first. Chains that
\ no longer cover exactly the live rows are dropped for a rebuild instead. The
\ field store has two rewinders that need not nest: a failed declaration rolls
\ back its field transaction before it restores a checker scope its body
\ leaked, whose mark lies above those rows, and that restore moves PF-N forward
\ over scrubbed rows the chains no longer hold.
: FVX-RETIRE ( n -- ) {: newn:n :}
   FVX-READY @ 0= IF EXIT THEN
   FVX-HI @ PF-N @ <>  newn PF-N @ > or IF 0 FVX-READY ! EXIT THEN
   PF-N @ 1 -
   BEGIN dup newn >= WHILE dup FVX-POP 1 - REPEAT drop
   newn FVX-HI ! ;

\ Capture: the grown buffers are process-local, so the image keeps the boot
\ stores and the restored process rebuilds. Mirrors VNX-SNAP-RESET.
: FVX-SNAP-RESET ( -- )
   FVX-B-BOOT FVX-B-P !   FVX-SLOTS-INIT FVX-SLOTS-V !
   FVX-L-BOOT FVX-L-P !   PF-CAP-INIT FVX-LCAP !   0 FVX-CAP !
   0 FVX-READY !   0 FVX-HI ! ;

\ One owner's rows, newest first: FVX-FIRST answers the cursor of its bucket's
\ newest row and FVX-NEXT the cursor after a cursor's row, each a row id + 1
\ with 0 for the end. The cursor lives on the stack, so a reader may recurse
\ into another owner's walk.
: FVX-FIRST ( n n -- n ) {: fam:n var:n :}
   FVX-ENSURE
   fam var FVX-HASH FVX-BKT @ ;
: FVX-NEXT ( n -- n ) 1 - FVX-LINK @ ;

public

\ PF-LINK ( n -- ) : chain row id, which ADD has written and is about to
\ publish as PF-N's next value.
: PF-LINK ( n -- ) {: id:n :}
   FVX-ENSURE                             \ covers [0, id) and fits the capacity
   id FVX-PUSH
   id 1 + FVX-HI ! ;

\ THE field store rewind: TYPE-FIELD-OWNER:ROLLBACK, a checker frame's restore
\ and the prefix rewind all put the rows back to a saved mark through here.
\ The index pops the retired rows before the scrub clears the owner cells it
\ finds their buckets by.
: PF-REWIND ( n n -- ) {: pfn:n commitn:n :}
   pfn FVX-RETIRE
   pfn PF-N @ <>  commitn PF-COMMIT-N @ <> or IF TFM-EPOCH+ THEN
   pfn PF-N @ PF-SCRUB                    \ scrub the rows this rewind retires
   pfn PF-N !
   commitn PF-COMMIT-N ! ;

\ --- family facts memo (TFM). A checked use of a family's type asks
\ SUM-NAMED-PAYLOAD?, TFAM-CONCRETE-LINEAR? and TFW-FAM-IMPL (width of a family
\ with no parameter); each walks every variant, and certifying one generated
\ constructor asks several times, so without the memo an ENUM's constructors are
\ quadratic in its variants.
\ Each answer is memoized per family under TFM-EPOCH: a cell stamped with the
\ current epoch holds the answer a walk would give. A refusal is a throw and is
\ never memoized. Process-local, like VNX.

private

0 constant TFM-NAMED
1 constant TFM-LINEAR
2 constant TFM-WIDTH
3 constant TFM-STORE
8 cells constant TFM-REC                    \ (stamp, answer) per fact
create TFM-BOOT   TF-CAP-INIT TFM-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TFM-P   TFM-BOOT TFM-P !   REG-PROTECT
variable TFM-CAP   TF-CAP-INIT TFM-CAP !   REG-PROTECT   \ families TFM-P holds

\ Grown with the family store. The new rows are zeroed, and zero is never a
\ current stamp.
: TFM-ENSURE ( -- )
   TFM-CAP @ TF-CAP >= IF EXIT THEN
   TFM-P  TFM-CAP @ TFM-REC *  TF-CAP TFM-REC *  REG-GROW1
   TFM-P @  TFM-CAP @ TFM-REC * CELL /  TF-CAP TFM-REC * CELL /  ARENA-CELLS-ZERO
   TF-CAP TFM-CAP ! ;

: TFM-AT ( n n -- ptr n ) {: fam:n k:n :}   \ the fact's stamp cell; its answer follows
   TFM-ENSURE
   fam TFM-REC *  k 2 * cells +  TFM-P @ + ;

: TFM@ ( n n -- n bool ) {: fam:n k:n :}
   fam k TFM-AT {: c:ptr :}
   c @ TFM-EPOCH @ <> IF 0 RES-FALSE EXIT THEN
   c CELL + @ RES-TRUE ;

: TFM! ( n n n -- ) {: v:n fam:n k:n :}
   fam k TFM-AT {: c:ptr :}
   TFM-EPOCH @ c !
   v c CELL + ! ;

: TFM-FLAG@ ( n n -- bool bool ) TFM@ {: ok:bool :} 0 <> ok ;
: TFM-FLAG! ( bool n n -- ) {: b:bool fam:n k:n :}
   b IF 1 ELSE 0 THEN fam k TFM! ;

\ Capture: back to the zeroed boot store. Its zero stamps are invalid at epoch 1.
: TFM-SNAP-RESET ( -- )
   TFM-BOOT 0 TF-CAP-INIT TFM-REC * CELL / ARENA-CELLS-ZERO
   TFM-BOOT TFM-P !
   TF-CAP-INIT TFM-CAP !
   1 TFM-EPOCH ! ;

public

\ A variant has one declared-payload source: SUMTYPE stores positional roots in
\ the SUMV schema range; ENUM stores named rows in TYPE-FIELD and leaves that
\ range empty. The accessors below are the one seam for both. A sum/enum family
\ with a nonzero declared field count uses TYPE-FIELD for every variant; bad
\ bounds, ownership or a missing named index are invariant failures, never a
\ fallback into positional storage. PRODUCT keeps positional schemas for its
\ make/unmake variants. An all-empty family has no payload either way. Only a
\ bounded, committed, variant-owned field slice is a valid named representation.

private

\ A variant's named payload is the rows of its owner chain (FVX) that sit inside
\ its family's field slice. The chain can also hold the owner's rows outside the
\ slice, provisional ones included, which no reader counts; it runs newest
\ first, so declaration order is counted back from its far end.
: SUMV-SLICE-ROW? ( n n n -- bool ) {: fam:n vid:n id:n :}
   fam TFAM-FLD-START@ {: base:n :}
   id base <  id base fam TFAM-FLD-COUNT@ + >=  or IF RES-FALSE EXIT THEN
   fam vid id PF-ROW PF-ROW-OWNER? ;

: SUMV-SLICE-N ( n n -- n ) {: fam:n vid:n :}    \ the variant's rows in the slice
   0  fam vid FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - {: id:n :}
      fam vid id SUMV-SLICE-ROW? IF swap 1 + swap THEN
      FVX-NEXT
   REPEAT drop ;

: SUMV-NAMED-FIELD ( n n -- n bool ) {: vid:n wanted:n :}
   wanted 0 < IF 0 RES-FALSE EXIT THEN
   vid SUMV-FAM@ {: fam:n :}
   fam TFAM-FLD-START@ {: base:n :}
   fam TFAM-FLD-COUNT@ {: count:n :}
   base 0 < IF 0 RES-FALSE EXIT THEN
   count 0 < IF 0 RES-FALSE EXIT THEN
   base PF-COMMIT-N @ > IF 0 RES-FALSE EXIT THEN
   count PF-COMMIT-N @ base - > IF 0 RES-FALSE EXIT THEN
   fam vid SUMV-SLICE-N wanted - 1 -             \ the variant's rows newer than the wanted one
   dup 0 < IF drop 0 RES-FALSE EXIT THEN
   fam vid FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - {: id:n :}
      fam vid id SUMV-SLICE-ROW? IF
         over 0= IF 2drop id RES-TRUE EXIT THEN
         swap 1 - swap
      THEN
      FVX-NEXT
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

: SUM-NAMED-PAYLOAD-WALK? ( n -- bool ) {: fam:n :}
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-START@ {: base:n :}
   fam TFAM-FLD-COUNT@ {: count:n :}
   count 0 < IF E-TFAM-PAYLOAD throw THEN
   count 0= IF RES-FALSE EXIT THEN
   base 0 < IF E-TFAM-PAYLOAD throw THEN
   base PF-COMMIT-N @ > IF E-TFAM-PAYLOAD throw THEN
   count PF-COMMIT-N @ base - > IF E-TFAM-PAYLOAD throw THEN
   fam SUMV-FAMILY-LEGACY-PAYLOAD? IF E-TFAM-PAYLOAD throw THEN
   fam TFAM-VAR-START@ {: variants:n :}
   fam TFAM-VAR-COUNT@ {: variant-count:n :}
   0 BEGIN dup count < WHILE
      base over + {: fid:n :}
      fid PF-FAM@ fam <> IF E-TFAM-PAYLOAD throw THEN
      fid PF-VAR@ {: owner:n :}
      owner PF-NO-VARIANT = IF E-TFAM-PAYLOAD throw THEN
      owner 0 < owner SUMV-N @ >= or IF E-TFAM-PAYLOAD throw THEN
      owner SUMV-FAM@ fam <> IF E-TFAM-PAYLOAD throw THEN
      owner variants < owner variants - variant-count >= or IF E-TFAM-PAYLOAD throw THEN
      1 +
   REPEAT drop
   RES-TRUE ;

: SUM-NAMED-PAYLOAD? ( n -- bool ) {: fam:n :}
   fam TFM-NAMED TFM-FLAG@ IF EXIT THEN drop
   fam SUM-NAMED-PAYLOAD-WALK? dup fam TFM-NAMED TFM-FLAG! ;

: SUMV-NAMED-PAYLOAD? ( n -- bool ) SUMV-FAM@ SUM-NAMED-PAYLOAD? ;

public

: SUMV-PAY-FIELD ( n n -- n bool ) {: vid:n wanted:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF 0 RES-FALSE EXIT THEN
   vid wanted SUMV-NAMED-FIELD ;

: SUMV-PAY-N ( n -- n ) {: vid:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF vid SUMV-SCH-COUNT@ EXIT THEN
   vid SUMV-FAM@ vid SUMV-SLICE-N ;

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
   vid SUMV-FAM@ {: fam:n :}
   0  fam vid FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - {: id:n :}
      fam vid id SUMV-SLICE-ROW? IF swap id PF-CELLS@ + swap THEN
      FVX-NEXT
   REPEAT drop ;

\ --- instantiated width. A schema application's width is its family's width at
\ its arguments' widths. Under a term a parameter takes the term's argument
\ width; at declaration (TFAM-SCH-WIDTH) it counts as one cell. So a closed
\ family's declared width equals its instantiated width, and opt<res<n,n>> is as
\ wide as its arguments make it, never its family's registry width. A walk reads
\ argument widths from a frame [base, top) of the TFW scratch row, cell base+i
\ holding argument i's width. An application measures its arguments into the
\ frame above top, so frames never overlap and every cell is written before it
\ is read; base and top are passed explicitly, so a throw leaks nothing. T-WIDTH
\ (checker.f) reads TFAM-INST-WIDTH@; typed storage, generated constructors and
\ MATCH read T-WIDTH.

private

16 constant TFW-INIT
create TFW-BOOT   TFW-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE TFW-P   TFW-BOOT TFW-P !   REG-PROTECT
variable TFW-CAP   TFW-INIT TFW-CAP !   REG-PROTECT   \ cells TFW-P holds

\ Grown, never zeroed: every frame cell is written before it is read.
: TFW-ENSURE ( n -- ) {: need:n :}
   need TFW-CAP @ <= IF EXIT THEN
   need TFW-CAP @ 2 * max {: cap:n :}
   TFW-P  TFW-CAP @ cells  cap cells  REG-GROW1
   cap TFW-CAP ! ;

: TFW@ ( n -- n ) cells TFW-P @ + @ ;
: TFW! ( n n -- ) {: w:n idx:n :}
   idx 1 + TFW-ENSURE
   w idx cells TFW-P @ + ! ;

\ Capture: back to the boot row. The frames are process-local scratch, like TFM.
: TFW-SNAP-RESET ( -- )
   TFW-BOOT 0 TFW-INIT ARENA-CELLS-ZERO
   TFW-BOOT TFW-P !
   TFW-INIT TFW-CAP ! ;

defer TFW-FAM ( n n n -- n )   \ ( fam base top -- width ) forward ref for a nested application

: TFW-NODE ( n n n -- n ) {: node:n base:n top:n :}   \ width of one schema node under the frame [base, top)
   node SCHEMA-PARAM? IF base node SCHEMA-A@ + TFW@ EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-B@ {: start:n :}
      node SCHEMA-C@ {: cnt:n :}
      cnt 0 ?do                                  \ an argument's own frames live above top+cnt
         start i + SCHEMA-ROOT@ base top cnt + RECURSE  top i + TFW!
      loop
      node SCHEMA-A@ top top cnt + TFW-FAM EXIT
   THEN
   1 ;

: TFW-PRODUCT ( n n n -- n ) {: fam:n base:n top:n :}
   fam TFAM-FLD-START@ {: fs:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      fs over + PF-PENDING-SCH@ SCHEMA-ROOT@ base top TFW-NODE
      rot + swap
      1 +
   REPEAT drop ;

: TFW-SUMV ( n n n -- n ) {: vid:n base:n top:n :}   \ positional payload only
   vid SUMV-SCH-COUNT@ {: count:n :}
   vid SUMV-SCH-START@ {: start:n :}
   0
   0 BEGIN dup count < WHILE
      start over + SCHEMA-ROOT@ base top TFW-NODE
      rot + swap
      1 +
   REPEAT drop ;

\ Called after validating the family once. It reads only this variant's rows,
\ and the cursor stays on the stack while a field's width recurses.
: TFW-SUMV-NAMED ( n n n -- n ) {: vid:n base:n top:n :}
   vid SUMV-FAM@ {: fam:n :}
   0  fam vid FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - {: id:n :}
      fam vid id SUMV-SLICE-ROW? IF
         swap id PF-SCH@ SCHEMA-ROOT@ base top TFW-NODE + swap
      THEN
      FVX-NEXT
   REPEAT drop ;

: TFW-SUM ( n n n -- n ) {: fam:n base:n top:n :}   \ tag + max variant payload width
   fam SUM-NAMED-PAYLOAD? {: named:bool :}
   fam TFAM-VAR-START@ {: vs:n :}
   0
   0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
      vs over + base top named IF TFW-SUMV-NAMED ELSE TFW-SUMV THEN
      rot max swap
      1 +
   REPEAT drop
   1 + ;

: TFW-FAM-WALK ( n n n -- n ) {: fam:n base:n top:n :}
   fam TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   fam TFAM-PRODUCT? IF fam base top TFW-PRODUCT EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF fam base top TFW-SUM EXIT THEN
   1 ;

\ A family with no parameter has one width wherever it is applied, so it is
\ walked once per TFM epoch and every later application reads the TFM-WIDTH
\ memo: a chain of closed families that each carry the previous one twice is
\ walked in linear time, not 2^N. A parametric family is walked per frame.
: TFW-FAM-IMPL ( n n n -- n ) {: fam:n base:n top:n :}
   fam TFAM-ARITY@ 0 > IF fam base top TFW-FAM-WALK EXIT THEN
   fam TFM-WIDTH TFM@ IF EXIT THEN drop
   fam base top TFW-FAM-WALK dup fam TFM-WIDTH TFM! ;
: TFW-FAM-INSTALL ( -- ) [: TFW-FAM-IMPL ;] is TFW-FAM ;
TFW-FAM-INSTALL

public

: TFAM-SCH-WIDTH ( n n -- n ) {: fam:n node:n :}   \ declared width of a schema node of fam: each parameter one cell
   fam TFAM-ARITY@ {: ar:n :}
   ar 0 ?do 1 i TFW! loop
   node 0 ar TFW-NODE ;

private

\ Every argument is measured before any frame cell is written: T-WIDTH of an
\ argument re-enters this walk at cell 0.
: TERM-FRAME ( n n n -- ) {: term:n idx:n count:n :}   \ frame cells [idx, count) := term's argument widths
   idx count >= IF EXIT THEN
   term idx PARAM>ARG T-WIDTH {: w:n :}
   term idx 1 + count RECURSE
   w idx TFW! ;

\ A closed family answers from TFW-FAM-IMPL's memo; a parametric one is walked
\ per instance.
: TFAM-INST-WIDTH@ ( n -- n ) {: term:n :}      \ instantiated logical width of a resolved layout term
   term PARAM>FAM {: fam:n :}
   fam TFAM-BOXED-OR-NICHE? IF 1 EXIT THEN
   fam TFAM-LAYOUT? 0= IF 1 EXIT THEN
   term PARAM>ARGC {: count:n :}
   term 0 count TERM-FRAME
   fam 0 count TFW-FAM ;

\ --- which argument slots the width above reads. TFW-NODE reads an argument's
\ width where a schema root reads its slot: directly, a root that is that
\ parameter node, or a nested application whose family reads the slot that
\ argument fills. An occurrence under a pointer or a quotation, or in a slot its
\ application never reads, has a width the declaration fixes. These words walk
\ the same roots as TFW-PRODUCT and TFW-SUM; the checker uses the answer to
\ decide whether an instance with that slot open has a placeable width (checker.f
\ LAYOUT-WIDTH-OPEN?). A width site added to TFW-NODE is an occurrence this must
\ report.
defer TFAM-WIDTH-SLOT-FWD ( n n -- bool )   \ ( fam slot -- reads? ) forward ref for a nested application
: SCH-ROOT-WIDTH-SLOT? ( n n -- bool ) {: node:n slot:n :}
   node SCHEMA-PARAM? IF node SCHEMA-A@ slot = EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-A@ {: g:n :}
      node SCHEMA-B@ {: start:n :}
      node SCHEMA-C@ 0 ?do
         g i TFAM-WIDTH-SLOT-FWD IF
            start i + SCHEMA-ROOT@ slot RECURSE IF unloop RES-TRUE EXIT THEN
         THEN
      loop
   THEN
   RES-FALSE ;
: SUMV-WIDTH-SLOT? ( n n -- bool ) {: vid:n slot:n :}
   0 BEGIN dup vid SUMV-PAY-N < WHILE
      vid over SUMV-PAY-ROOT SCHEMA-ROOT@ slot SCH-ROOT-WIDTH-SLOT? IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;

: TFAM-WIDTH-SLOT? ( n n -- bool ) {: fam:n slot:n :}   \ the instantiated width reads argument slot `slot`
   fam TFAM-BOXED-OR-NICHE? IF RES-FALSE EXIT THEN
   fam TFAM-PRODUCT? IF
      0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
         fam TFAM-FLD-START@ over + PF-PENDING-SCH@ SCHEMA-ROOT@ slot SCH-ROOT-WIDTH-SLOT?
         IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      RES-FALSE EXIT
   THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF
      fam SUM-NAMED-PAYLOAD? IF
         fam TFAM-FLD-COUNT@ 0 ?do
            fam TFAM-FLD-START@ i + PF-SCH@ SCHEMA-ROOT@ slot SCH-ROOT-WIDTH-SLOT?
            IF unloop RES-TRUE EXIT THEN
         loop
         RES-FALSE EXIT
      THEN
      0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
         fam TFAM-VAR-START@ over + slot SUMV-WIDTH-SLOT? IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
   THEN
   RES-FALSE ;
: TFAM-WIDTH-SLOT-INSTALL ( -- ) [: TFAM-WIDTH-SLOT? ;] is TFAM-WIDTH-SLOT-FWD ;
TFAM-WIDTH-SLOT-INSTALL

\ --- which argument slots reach storage (checker.f PARAM-PAIR-SLOT). A slot
\ reaches storage when its parameter occurs beneath a pointer, or in an
\ argument a nested application's own storage slot takes (a `FIELD
\ data ptr a`); a field, a variant payload or a quotation's row elsewhere
\ holds it by value (`FIELD fn a`). An element-type slot (PK-TYPE: a C2
\ view's, init's, records' or loan-field's, or a user family's parameter that
\ feeds one) names the type of the storage the family addresses, so it
\ reaches storage whatever its fields. Only a visible product or an unboxed
\ sum holds a slot by value: an OPAQUE or boxed family, or one with no layout,
\ reaches storage in every slot. One mask per family, memoized under
\ TFM-EPOCH; the parameter kinds and OPAQUE it reads are set before the field
\ commit that moves the epoch. Only a boxed sum names its own family
\ (sumtype.f E-TDECL-RECURSIVE), and it answers before any walk, so the walk
\ ends.
-1 constant STORE-ALL
defer TFAM-STORE-MASK-FWD ( n -- n )   \ ( fam -- mask ) forward ref for a nested application


: SCH-STORE-MASK ( n bool -- n )   \ slots whose parameter node holds beneath storage
   {: node:n held:bool :}
   node SCHEMA-PARAM? IF held IF 1 node SCHEMA-A@ lshift ELSE 0 THEN EXIT THEN
   node SCHEMA-PTR? IF node SCHEMA-A@ RES-TRUE RECURSE EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-A@ TFAM-STORE-MASK-FWD {: g:n :}
      node SCHEMA-B@ {: start:n :}
      0  node SCHEMA-C@ 0 ?do
         start i + SCHEMA-ROOT@  held g 1 i lshift and 0 <> or  RECURSE or
      loop EXIT
   THEN
   node SCHEMA-QUOT? IF
      0  SCH-QUOT-ROWS 0 ?do
         node i SCHEMA-QUOT-ROW@ {: row:n :}
         row SCHEMA-ROW-OK? IF
            row SCHEMA-ROW-COUNT@ 0 ?do row i SCHEMA-ROW-ELEM@ held RECURSE or loop
         THEN
      loop EXIT
   THEN
   0 ;


: FIELDS-STORE-MASK ( n -- n )   \ the declared fields' storage slots
   {: fam:n :}
   0  fam TFAM-FLD-COUNT@ 0 ?do
      fam TFAM-FLD-START@ i + PF-PENDING-SCH@ SCHEMA-ROOT@ RES-FALSE SCH-STORE-MASK or
   loop ;


: SUMV-STORE-MASK ( n -- n )
   {: vid:n :}
   0  vid SUMV-PAY-N 0 ?do vid i SUMV-PAY-ROOT SCHEMA-ROOT@ RES-FALSE SCH-STORE-MASK or loop ;


: TFAM-STORE-WALK ( n -- n )
   {: fam:n :}
   fam TFAM-OPAQUE?  fam TFAM-LAYOUT-POLICY@ TL-BOXED =  or IF STORE-ALL EXIT THEN
   fam TFAM-PRODUCT? IF fam FIELDS-STORE-MASK EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF STORE-ALL EXIT THEN
   fam SUM-NAMED-PAYLOAD? IF fam FIELDS-STORE-MASK EXIT THEN
   0  fam TFAM-VAR-COUNT@ 0 ?do fam TFAM-VAR-START@ i + SUMV-STORE-MASK or loop ;


: TYPE-SLOT-MASK ( n -- n )   \ the element-type (PK-TYPE) slots
   {: fam:n :}
   0  fam TFAM-ARITY@ 0 ?do fam i TFAM-PK@ PK-TYPE = IF 1 i lshift or THEN loop ;


: TFAM-STORE-MASK ( n -- n )
   {: fam:n :}
   fam TFM-STORE TFM@ IF EXIT THEN drop
   fam TFAM-STORE-WALK  fam TYPE-SLOT-MASK or  dup fam TFM-STORE TFM! ;


: TFAM-STORE-MASK-INSTALL ( -- ) [: TFAM-STORE-MASK ;] is TFAM-STORE-MASK-FWD ;
TFAM-STORE-MASK-INSTALL


: TFAM-STORE-SLOT? ( n n -- bool )   \ argument slot `slot` reaches storage
   {: fam:n slot:n :}
   fam TFAM-STORE-MASK  1 slot lshift and 0 <> ;


: PF-MATCH? ( n n ptr u8 n n -- bool ) {: fam:n var:n na:ptr nu:n id:n :}
   id PF-REC@ {: r:ptr :}
   fam var r PF-ROW-OWNER? 0= IF RES-FALSE EXIT THEN
   r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= ;

public

: PF-FIND ( n n ptr u8 n -- n bool ) {: fam:n var:n na:ptr nu:n :}
   0 BEGIN dup PF-COMMIT-N @ < WHILE
      {: i:n :}
      fam var na nu i PF-MATCH? IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;
: PF-EACH ( n n n -- n bool ) {: fam:n var:n start:n :}
   start 0 < IF 0 RES-FALSE EXIT THEN
   start BEGIN dup PF-COMMIT-N @ < WHILE
      {: i:n :}
      i PF-REC@ {: r:ptr :}
      fam var r PF-ROW-OWNER? IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;

\ Strict-LIFO transaction frames. COMMIT retains its rollback frame through
\ global finalization. Nested commit keeps rows provisional; only outer commit
\ advances PF-COMMIT-N. Every frame owns PF-N and PF-COMMIT-N. The name pool is
\ the family frame's (TF-RBF, through TFAM-REWIND): family, variant and
\ constructor-package names interleave with field names in it, so no field
\ frame mark bounds only its own bytes. A field rollback no enclosing family
\ frame rewinds leaves its field names in [0, TF-STR-U), so UNIT-FAM-TILED
\ refuses the file that made them for FILE capture.

private

0 cells constant PFTX.PFN-OFF
1 cells constant PFTX.TOK-OFF
2 cells constant PFTX.COMMITN-OFF
3 cells constant PFTX.STATE-OFF

public

4 cells constant PF-TX-REC
: PFTX.PFN ( ptr a -- ptr a ) PFTX.PFN-OFF + ;
: PFTX.TOK ( ptr a -- ptr a ) PFTX.TOK-OFF + ;
: PFTX.COMMITN ( ptr a -- ptr a ) PFTX.COMMITN-OFF + ;
: PFTX.STATE ( ptr a -- ptr a ) PFTX.STATE-OFF + ;


private

4 constant PF-TX-CAP-INIT

public

variable PF-TX-CAP-V   PF-TX-CAP-INIT PF-TX-CAP-V !   REG-PROTECT

private

create PF-TX-BOOT   PF-TX-CAP-INIT PF-TX-REC * allot   REG-PROTECT

public

PERSISTED-PTR-VARIABLE PF-TX-P   PF-TX-BOOT PF-TX-P !   REG-PROTECT
variable PF-TX-DEPTH   0 PF-TX-DEPTH !   REG-PROTECT
variable PF-TX-SERIAL   0 PF-TX-SERIAL !   REG-PROTECT

\ Here because only this package sees PF-TX-BOOT.
: PF-TX-ENSURE ( -- )
   PF-TX-DEPTH @ PF-TX-CAP-V @ < IF EXIT THEN
   PF-TX-CAP-V @ 2 * {: nc:n :}
   PF-TX-P PF-TX-CAP-V @ PF-TX-REC * nc PF-TX-REC * REG-GROW1
   nc PF-TX-CAP-V ! ;

\ A declaration participant that finds frames opened above its own must retire
\ them, but only this owner may walk its frame stack, unreachable from ordinary
\ source. The owner installs its private ROLLBACK-THROUGH here, the compiled
\ declaration-event participant calls the vector, and
\ generated-declaration-protection.f retires the name once every prefix caller
\ is compiled. It is not a checker primitive row: a primitive axiom survives
\ `undefine`, so the checker would keep certifying calls the runtime can no
\ longer resolve.
defer TDECL-FIELD-CLEANUP-XT ( n -- )

\ Total release, same shape and lifetime as the cleanup vector. The runner's
\ release step runs after every reversible commit succeeded, so the
\ participant must discard its field frame without being able to reject:
\ PREPARE already proved the frame is the live committed top. RELEASE stays
\ private because a public entry would have to re-validate, which a total
\ release may not do; generated-declaration-protection.f retires the name once
\ its sole caller is compiled.
defer TDECL-FIELD-RELEASE-XT ( -- )

;package

package TYPE-FIELD-OWNER

0 constant STATE-OPEN
1 constant STATE-COMMITTED

: TX-BASE ( -- ptr n ) PF-TX-P @ ;
: TX-AT ( n -- ptr n ) PF-TX-REC * TX-BASE + ;
: TX-TOP ( -- ptr n )
   PF-TX-DEPTH @ 0= IF E-PF-TX throw THEN
   PF-TX-DEPTH @ 1 - TX-AT ;
: TX-REQUIRE ( n -- ) TX-TOP PFTX.TOK @ <> IF E-PF-TX throw THEN ;
: TX-STATE-REQUIRE ( n n -- ) {: tx:n state:n :}
   tx TX-REQUIRE
   TX-TOP PFTX.STATE @ state <> IF E-PF-TX throw THEN ;
: TX-MARKS-REQUIRE ( -- )
   TX-TOP PFTX.PFN @ PF-N @ > IF E-PF-TX throw THEN ;
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

\ The wrap guard has no test: reaching it needs 2^63 OPENs in one process, and no
\ raw setter or TRUSTED seam for PF-TX-SERIAL is published to fake it. Without
\ it a wrapped serial would mint token 0 or a negative token, and every phase
\ compares tokens by equality, so a stale token could alias a live frame. A FILE
\ import only advances the serial by a delta UNIT-FAM-PLAN-OPEN bounds, so this
\ is the one minting site.
: OPEN ( -- n )
   WRITE-WINDOW-CK
   TX-PARENT-REQUIRE
   PF-TX-SERIAL @ 1 + dup 0 <= IF drop E-PF-TX throw THEN
   {: tok:n :}
   PF-TX-ENSURE
   tok PF-TX-SERIAL !
   PF-TX-DEPTH @ TX-AT {: r:ptr :}
   PF-N @ r PFTX.PFN !
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
   WRITE-WINDOW-CK
   tx PREPARE drop
   PF-TX-DEPTH @ 1 = IF PF-N @ PF-COMMIT-N !  TFM-EPOCH+ THEN
   STATE-COMMITTED TX-TOP PFTX.STATE ! ;
: FINALIZE ( n -- ) {: tx:n :}
   WRITE-WINDOW-CK
   tx STATE-COMMITTED TX-STATE-REQUIRE
   TX-COMMITTED-MARKS-REQUIRE
   RELEASE ;
\ A live frame is in exactly one of the two states OPEN sets and COMMIT moves it
\ to; the state selects which watermark check applies.
: ROLLBACK ( n -- ) {: tx:n :}
   WRITE-WINDOW-CK
   tx TX-REQUIRE
   TX-TOP PFTX.STATE @ STATE-OPEN =
      IF TX-OPEN-MARKS-REQUIRE ELSE TX-COMMITTED-MARKS-REQUIRE THEN
   TX-TOP {: r:ptr :}
   r PFTX.PFN @ r PFTX.COMMITN @ PF-REWIND   \ retire the provisional rows
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
\ TX-INDEX proves the token is still a live frame before anything moves, so a
\ stale or released token leaves every mark untouched; each live frame then
\ takes the ordinary ROLLBACK from the top down, the target's own marks last.
\ Private; its only caller is the vector installed below, so no source-level
\ word can pick a frame out of the middle of the stack.
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
   [: ROLLBACK-THROUGH ;] is TFAM:TDECL-FIELD-CLEANUP-XT
   [: RELEASE ;] is TFAM:TDECL-FIELD-RELEASE-XT ;
VECTORS-INSTALL

;package

package TFAM

\ ADD validation. Field rows carry explicit logical-cell and memory-layout
\ metadata under every policy; no policy implies CELL-sized storage.
$7FFFFFFFFFFFFFFF constant PF-MAX-N

public

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

;package

package TYPE-NAME

\ sumtype.f's TYPE-DECL codes under their own names: that package loads after
\ this one, so the same (code, name) pair is registered here first.
7107 constant E-TDECL-SYNTAX
7110 constant E-TDECL-NAME

public

\ CONTROL? is the one place the control-word list is written. Every declaration
\ name gate asks it: this package's RESERVED? (variant names), FAMILY-RESERVED?
\ for every family definer, and PF-RESERVED? (field names). A name that spells a
\ control word would compile into a body as that control word, so no
\ declaration position may take one.
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
   a u CON-OF 0 <> IF RES-TRUE EXIT THEN
   a u ATOM-TOK? IF RES-TRUE EXIT THEN
   a u XT-TOK? IF RES-TRUE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u CONTROL? IF RES-TRUE EXIT THEN
   a u TF-GRAMMAR-KEYWORD? ;

public

\ The variant-name collision rule, asked by VARIANT-REQUIRE and by SUMTYPE's
\ variant gate (sumtype.f): a family is taken wherever the declaring scope
\ resolves it, a global family in every scope and a package family inside its
\ package.
: FAMILY-TAKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " a u TFAM-FIND-IN nip IF RES-TRUE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ a u TFAM-FIND-IN nip ;

\ The family-name gate every family definer asks: SUMTYPE, PRODUCT and NEWTYPE
\ (sumtype.f) and the STRUCTURE and ENUM front ends. A family tail must be one an
\ effect reads as that family. The effect parser reads a value record name
\ (checker.f PSTACK) and a bare `ptr` (SIG-PTR-TOK?) ahead of any family, and an
\ atom-shaped tail as an atom wherever the family does not resolve; its other
\ readings (`[`, stack syntax, row variables, split characters) are never
\ canonical tails. A package may shadow a global family, so without the `ptr`
\ row a package's `ptr` registered and its CAST: converter `( n -- ptr )` died
\ as a pointer with no pointee; without the value record row `ENUM vr` loaded
\ in a package after `VALUE-RECORD vr`, one name naming two kinds.
: FAMILY-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SIG-PTR-TOK? IF RES-TRUE EXIT THEN
   a u RESERVED? ;

: VARIANT-REQUIRE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF E-TDECL-SYNTAX throw THEN
   a u TF-REQUIRE-CANON
   a u RESERVED? IF E-TDECL-NAME throw THEN
   a u FAMILY-TAKEN? IF E-TDECL-NAME throw THEN ;

private
get-current prot-wid-add
public
get-current prot-wid-add

;package

package TFAM

\ A field tail may not spell a grammar keyword, a control word (TYPE-NAME:CONTROL?)
\ or a generated-operation name, on every path that registers a field row:
\ PRODUCT through TDECL-REQUIRE-FIELD-NAME and the STRUCTURE / ENUM front ends
\ through TYPE-FIELD-OWNER:ADD.
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

public

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

private

: PF-FAM-VISIBLE? ( n n -- bool ) {: owner:n fam:n :}
   fam TFAM-PUBLIC? IF RES-TRUE EXIT THEN
   fam TFAM-PKG$ owner TFAM-PKG$ CORE-STR= ;
: PF-APP-KIND ( n -- n ) {: fam:n :}
   fam TFAM-KIND@ TK-EVIDENCE = IF PK-EVIDENCE EXIT THEN
   fam TFAM-LAYOUT? IF PK-LAYOUT EXIT THEN
   PK-CELL ;
: PF-NONVALUE? ( n -- bool )
   dup PK-SCOPE = swap PK-REGION = or ;
: PF-FAM-NONVALUE? ( n -- bool ) {: fam:n :}
   fam TFAM-ARITY@ 0 ?do
      fam i TFAM-PK@ PF-NONVALUE? IF UNLOOP RES-TRUE EXIT THEN
   loop RES-FALSE ;
: PF-KIND-OK? ( n n -- bool ) {: got:n want:n :}
   want PK-TYPE = IF
      got PK-EVIDENCE <> got PF-NONVALUE? 0= and EXIT THEN
   got want = ;

\ Deferred so PF-NODE-KIND?'s SCH-QUOT case can reach it before its body, which
\ calls PF-NODE-KIND? back, is defined.
defer PF-QUOT-ROW-OK? ( n n -- bool )   \ ( owner rownode -- bool )

: PF-NODE-KIND? ( n n -- n bool ) {: owner:n node:n :}
   node SCHEMA-NODE-OK? 0= IF 0 RES-FALSE EXIT THEN
   node SCHEMA-PARAM? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ {: idx:n :}
      idx 0 < idx owner TFAM-ARITY@ >= or IF 0 RES-FALSE EXIT THEN
      owner idx TFAM-PK@ RES-TRUE EXIT
   THEN
   node SCHEMA-CON? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ CT-LIVE? IF PK-CELL RES-TRUE ELSE 0 RES-FALSE THEN EXIT
   THEN
   node SCHEMA-PTR? IF
      node SCHEMA-B@ node SCHEMA-C@ or 0= 0= IF 0 RES-FALSE EXIT THEN
      node SCHEMA-A@ {: child:n :}
      child node >= IF 0 RES-FALSE EXIT THEN
      owner child RECURSE 0= IF drop 0 RES-FALSE EXIT THEN
      PF-NONVALUE? IF 0 RES-FALSE EXIT THEN
      child SCHEMA-APP? IF
         child SCHEMA-A@ PF-FAM-NONVALUE? IF 0 RES-FALSE EXIT THEN
      THEN
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

\ Elements are built before their row (parser order), so each must sit below the
\ row id: the acyclic guard PF-NODE-KIND? applies to APP/PTR children.
: PF-QUOT-ROW-OK-IMPL ( n n -- bool ) {: owner:n rownode:n :}
   rownode SCHEMA-ROW? 0= IF RES-FALSE EXIT THEN
   rownode SCHEMA-ROW-COUNT@ {: cnt:n :}
   rownode SCHEMA-ROW-START@ {: estart:n :}
   cnt 0 < IF RES-FALSE EXIT THEN
   estart 0 < estart SCH-ROOT-N @ > or IF RES-FALSE EXIT THEN
   cnt SCH-ROOT-N @ estart - > IF RES-FALSE EXIT THEN
   0 BEGIN dup cnt < WHILE
      dup estart + SCHEMA-ROOT@
      dup rownode >= IF 2drop RES-FALSE EXIT THEN
      owner swap PF-NODE-KIND? IF
         PF-NONVALUE? IF drop RES-FALSE EXIT THEN
      ELSE 2drop RES-FALSE EXIT THEN
      1 +
   REPEAT drop RES-TRUE ;
: PF-QUOT-ROW-INSTALL ( -- ) [: PF-QUOT-ROW-OK-IMPL ;] is PF-QUOT-ROW-OK? ;
PF-QUOT-ROW-INSTALL

public

: PF-SCHEMA-OK? ( n n -- bool ) {: owner:n sch:n :}
   sch 0 < sch SCH-ROOT-N @ >= or IF RES-FALSE EXIT THEN
   sch SCHEMA-ROOT@ {: node:n :}
   owner node PF-NODE-KIND? IF
      {: kind:n :}
      node SCHEMA-PARAM? IF
         owner C2-INIT-FAM @ = node SCHEMA-A@ 1 = and IF kind PK-TYPE =
         ELSE kind PK-CELL = THEN EXIT
      THEN kind PF-NONVALUE? 0= EXIT
   THEN drop RES-FALSE ;

private

\ --- declaration-time parameter-arity walk for SUM variant payloads. TFC-SCH-TERM
\ (construct/MATCH) indexes its minted-var scratch by parameter index with no
\ bounds guard, so a payload parameter beyond arity would be a silent
\ out-of-bounds read; this walk makes the arity invariant structural, with the
\ per-token TDECL-LETTER gate (sumtype.f) as the front line. It is not
\ PF-NODE-KIND?: that walk's SCH-APP branch also matches each argument's
\ physical kind to the parameter kind (PF-KIND-OK?), which would reject a valid
\ nested-layout payload such as option<result<n,f>> (a layout-kind argument in a
\ cell parameter slot). This walk descends SCH-APP arguments, the SCH-PTR
\ pointee and SCH-QUOT rows, and answers the first out-of-arity index, or -1
\ when the subtree is in range (indices are 0..25).
defer TFAM-SCH-ROW-ARITY ( n n -- n )   \ ( owner rownode -- bad-idx ) forward ref for the SCH-QUOT recursion

public

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

private

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

public

: PF-OVERLAP? ( n n n n n n -- bool )
   {: fam:n var:n slot:n cellsn:n boff:n bytesn:n :}
   fam var FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - PF-ROW {: r:ptr :}
      fam var r PF-ROW-OWNER? IF
         slot cellsn r PF.SLOT @ r PF.CELLS @ PF-RANGE-OVERLAP? IF drop RES-TRUE EXIT THEN
         boff bytesn r PF.BYTE-OFF @ r PF.BYTES @ PF-RANGE-OVERLAP? IF drop RES-TRUE EXIT THEN
      THEN
      FVX-NEXT
   REPEAT
   drop RES-FALSE ;
: PF-LAYOUT-REQUIRE ( n n n n n n n n -- )
   {: fam:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   flags PF-FLAGS-NONE <> IF E-PF-FLAGS throw THEN
   slot cellsn PF-RANGE-OK? 0= IF E-PF-LAYOUT throw THEN
   boff bytesn PF-RANGE-OK? 0= IF E-PF-LAYOUT throw THEN
   al PF-POW2? 0= IF E-PF-LAYOUT throw THEN
   boff al mod 0 <> IF E-PF-LAYOUT throw THEN
   fam sch SCHEMA-ROOT@ TFAM-SCH-WIDTH cellsn <> IF E-PF-LAYOUT throw THEN
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
   fam var FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - PF-ROW {: r:ptr :}
      fam var r PF-ROW-OWNER? IF
         r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= IF drop RES-TRUE EXIT THEN
      THEN
      FVX-NEXT
   REPEAT
   drop RES-FALSE ;

;package

package TYPE-FIELD-OWNER

public

: ADD ( n n n ptr u8 n n n n n n n n -- n )
   {: tx:n fam:n var:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   WRITE-WINDOW-CK
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
   id PF-LINK                       \ chain the row under its owner before publishing it
   id 1 + PF-N !
   TFM-EPOCH+
   tx ;

private
get-current prot-wid-add
public
get-current prot-wid-add
private

;package

package TFAM

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

\ generated ADDRESS accessors (`DERIVE addr`, sumtype.f TDECL-ADDR-FIELD-WORD):
\ one word per field plus AT / BYTES / CELLS, in the spellings and wordlists the
\ family's constructors use. A public family's accessors are new tails in its
\ reserved constructor package, which TFAM-CTOR-EXTEND? would otherwise refuse,
\ and an undefine of one must answer E-CTOR-PROTECTED. This block follows the
\ field registry because a field tail is the member name. Only committed field
\ rows are read, the range the generator reads, so a name asked about while the
\ declaration is provisional (MAKE/UNMAKE generation in the body phase) sees no
\ uncommitted row.
package TFAM

private

: TFAM-ADDR-FIELD-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}   \ tail names a COMMITTED field of fam
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: id:n :}
      id PF-N@ < IF
         id PF-NAME$ a u CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

public

: TFAM-ADDR-FIXED-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a fixed addr member tail
   a u s" at" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" bytes" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" cells" CORE-STR=CI ;

: TFAM-ADDR-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}   \ member tail the FAMILY generates
   fam TFAM-DERIVE-ADDR? 0= IF RES-FALSE EXIT THEN
   a u TFAM-ADDR-FIXED-TAIL? IF RES-TRUE EXIT THEN
   a u fam TFAM-ADDR-FIELD-TAIL? ;

private

: TFAM-ADDR-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}   \ split name = id-family accessor?
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ id SUMV-FAM@ TFAM-ADDR-TAIL? ;

\ The private spelling carries no colon, so the member is recovered by deriving
\ each candidate spelling and comparing, as TFAM-PRIV-MEMBER? does.
: TFAM-ADDR-PRIV-FIXED? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}
   a u  fam TFAM-NAME$ s" at"    TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE EXIT THEN
   a u  fam TFAM-NAME$ s" bytes" TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE EXIT THEN
   a u  fam TFAM-NAME$ s" cells" TF-CTOR-PRIV$ CORE-STR=CI ;

: TFAM-ADDR-PRIV-FIELD? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: id:n :}
      id PF-N@ < IF
         a u  fam TFAM-NAME$ id PF-NAME$ TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

: TFAM-ADDR-PRIV-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-GEN-PUBLIC? IF RES-FALSE EXIT THEN
   fam TFAM-DERIVE-ADDR? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u fam TFAM-ADDR-PRIV-FIXED? IF RES-TRUE EXIT THEN
   a u fam TFAM-ADDR-PRIV-FIELD? ;

\ Both spellings, over the variant rows every addr family owns (a STRUCTURE with
\ fields always has its make/unmake pair; visiting a family twice is free).
: TFAM-ADDR-QUAL-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-ADDR-AT? IF RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop RES-FALSE ;

: TFAM-ADDR-PRIV-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-ADDR-PRIV-AT? IF RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop RES-FALSE ;

public

: TFAM-ADDR-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ exact generated accessor?
   a u TF-CW-SPLIT? IF a u TFAM-ADDR-QUAL-WORD? EXIT THEN
   a u TFAM-ADDR-PRIV-WORD? ;

private

\ A FIELD name is canonical, while the two public initialized members add a
\ final @ or !. Keep the suffix in its own buffer: TF-CTOR-PRIV$ builds into
\ TF-CTOR-BUF and must not overwrite its member argument.
create TF-INIT-TAIL TF-CTOR-CAP allot

public

: TFAM-INIT-MEMBER$ ( n bool -- ptr u8 n ) {: fid:n store:bool :}
   fid PF-NAME$ {: a:ptr u:n :}
   u 1 + TF-CTOR-CAP > IF s" tfam: initialized member too long" 76 die THEN
   u 0 ?do a i + c@ TF-UPPER-C TF-INIT-TAIL i + c! loop
   store IF 33 ELSE 64 THEN TF-INIT-TAIL u + c!
   TF-INIT-TAIL u 1 + ;

\ The helper lives in the declaring package's private wordlist even when the
\ record and its accessor are public. This spelling is shared with the
\ generated-name protection predicate below.
: TFAM-INIT-HELPER$ ( n n -- ptr u8 n ) {: fam:n fid:n :}
   fam TFAM-NAME$ fid PF-NAME$ TF-CTOR-PRIV$ 2drop
   s" -INIT-UNPACK" TF-CTOR-TAIL
   TF-CTOR-BUF TF-CTOR-U @ ;

: TFAM-INIT-RECEIVER$ ( n -- ptr u8 n ) {: fam:n :}
   fam TFAM-NAME$ s" init-view-unpack" TF-CTOR-PRIV$ 2drop
   TF-CTOR-BUF TF-CTOR-U @ ;

private

: TFAM-INIT-TAIL? ( ptr u8 n n -- bool ) {: a:ptr u:n fam:n :}
   fam TFAM-DERIVE-INIT? 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: fid:n :}
      fid PF-N@ < IF
         a u fid RES-FALSE TFAM-INIT-MEMBER$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
         a u fid RES-TRUE TFAM-INIT-MEMBER$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

: TFAM-INIT-QUAL-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-GEN-PUBLIC? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ fam TFAM-INIT-TAIL? ;

: TFAM-INIT-PRIV-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-DERIVE-INIT? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u fam TFAM-INIT-RECEIVER$ CORE-STR=CI IF RES-TRUE EXIT THEN
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: fid:n :}
      fid PF-N@ < IF
         a u fam fid TFAM-INIT-HELPER$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
         fam TFAM-GEN-PUBLIC? 0= IF
            a u fam TFAM-NAME$ fid RES-FALSE TFAM-INIT-MEMBER$
               TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
            a u fam TFAM-NAME$ fid RES-TRUE TFAM-INIT-MEMBER$
               TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
         THEN
      THEN
   loop RES-FALSE ;

: TFAM-INIT-QUAL-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-INIT-QUAL-AT? IF RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop RES-FALSE ;

: TFAM-INIT-PRIV-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-INIT-PRIV-AT? IF RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop RES-FALSE ;

public

: TFAM-INIT-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TF-CW-SPLIT? IF a u TFAM-INIT-QUAL-WORD? EXIT THEN
   a u TFAM-INIT-PRIV-WORD? ;

: TFAM-ADDR-WORD-INSTALL ( -- )
   [: 2dup TFAM-ADDR-WORD? IF 2drop RES-TRUE EXIT THEN
      TFAM-INIT-WORD? ;] is TFAM-ADDR-WORD-XT ;
TFAM-ADDR-WORD-INSTALL

;package

package TFAM

\ Conservative schema linearity for declaration and cast fences. Exact value
\ counts below instantiate fields, including repeated parameter occurrences.
\ Pointer and quotation nodes are non-owning boundaries.
defer TFCL-NODE-XT ( n -- bool )

: TFAM-CONCRETE-LINEAR-WALK? ( n -- bool ) {: fam:n :}
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

public

: TFAM-CONCRETE-LINEAR? ( n -- bool ) {: fam:n :}
   fam C2-MUT-FAM @ = IF RES-TRUE EXIT THEN
   fam C2-RECORDS-FAM @ = IF RES-TRUE EXIT THEN
   fam C2-READ-FAM @ = IF RES-FALSE EXIT THEN
   fam TFM-LINEAR TFM-FLAG@ IF EXIT THEN drop
   fam TFAM-CONCRETE-LINEAR-WALK? dup fam TFM-LINEAR TFM-FLAG! ;

: TFCL-NODE? ( n -- bool ) {: node:n :}
   node SCHEMA-CON? IF node SCHEMA-A@ CT-LINEAR? EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-A@ C2-MUT-FAM @ = IF RES-TRUE EXIT THEN
      node SCHEMA-A@ C2-RECORDS-FAM @ = IF RES-TRUE EXIT THEN
      node SCHEMA-A@ C2-READ-FAM @ = IF RES-FALSE EXIT THEN
      0 BEGIN dup node SCHEMA-C@ < WHILE
         node SCHEMA-B@ over + SCHEMA-ROOT@ RECURSE IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      node SCHEMA-A@ TFAM-CONCRETE-LINEAR? EXIT
   THEN
   RES-FALSE ;

private

: TFCL-NODE-INSTALL ( -- )
   [: TFCL-NODE? ;] is TFCL-NODE-XT
   [: TFAM-CONCRETE-LINEAR? ;] is TFAM-CON-LIN-XT ;
TFCL-NODE-INSTALL

\ logical layout records, one per family that has a resolved physical layout.
0 cells constant LAY.FAM-OFF
1 cells constant LAY.POLICY-OFF
2 cells constant LAY.SIZE-OFF
3 cells constant LAY.ALIGN-OFF
4 cells constant LAY.TAGW-OFF

public

5 cells constant LAY-REC
private

: LAY.FAM ( ptr a -- ptr a ) LAY.FAM-OFF + ;
: LAY.POLICY ( ptr a -- ptr a ) LAY.POLICY-OFF + ;
: LAY.SIZE ( ptr a -- ptr a ) LAY.SIZE-OFF + ;
: LAY.ALIGN ( ptr a -- ptr a ) LAY.ALIGN-OFF + ;
: LAY.TAGW ( ptr a -- ptr a ) LAY.TAGW-OFF + ;


4 constant LAY-CAP-INIT
variable LAY-CAP-V   LAY-CAP-INIT LAY-CAP-V !   REG-PROTECT
: LAY-CAP ( -- n ) LAY-CAP-V @ ;
create LAY-A-BOOT   LAY-CAP-INIT LAY-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE LAY-A-P   LAY-A-BOOT LAY-A-P !   REG-PROTECT
: LAY-BASE ( -- ptr n ) LAY-A-P @ ;

public

variable LAY-N   0 LAY-N !   REG-PROTECT

private

: LAY-GROW ( n -- ) {: need:n :}
   need LAY-CAP-V @ 2 * max {: nc:n :}
   LAY-A-P  LAY-CAP-V @ LAY-REC *  nc LAY-REC *  REG-GROW1
   nc LAY-CAP-V ! ;
: LAY-ENSURE ( -- )
   LAY-N @ LAY-CAP-V @ < IF exit THEN
   LAY-N @ 1 + LAY-GROW ;
: LAY-SCRUB ( n n -- ) {: lo:n hi:n :}    \ zero the layout records [lo,hi); see PF-SCRUB
   LAY-BASE  lo LAY-REC CELL / *  hi LAY-REC CELL / *  ARENA-CELLS-ZERO ;
: LAY-REC@ ( n -- ptr n ) {: id:n :}
   id 0 < IF s" tfam: bad layout id" 76 die THEN
   id LAY-N @ >= IF s" tfam: bad layout id" 76 die THEN
   id LAY-REC * LAY-BASE + ;

public

: LAY-FAM@ ( n -- n ) LAY-REC@ LAY.FAM @ ;
: LAY-POLICY@ ( n -- n ) LAY-REC@ LAY.POLICY @ ;
: LAY-SIZE@ ( n -- n ) LAY-REC@ LAY.SIZE @ ;
: LAY-ALIGN@ ( n -- n ) LAY-REC@ LAY.ALIGN @ ;
: LAY-TAGW@ ( n -- n ) LAY-REC@ LAY.TAGW @ ;
: LAY-N@ ( -- n ) LAY-N @ ;

: LAY-FIND ( n -- n bool ) {: fam:n :}
   0 BEGIN dup LAY-N @ < WHILE
      {: i:n :}
      i LAY-FAM@ fam = IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;
: LAY-ADD ( n n n n n -- n ) {: fam:n p:n sz:n al:n tw:n :}
   WRITE-WINDOW-CK
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   fam LAY-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   LAY-ENSURE
   LAY-N @ {: id:n :}
   id 1 + LAY-N !
   id LAY-REC@ {: r:ptr :}
   fam r LAY.FAM !   p r LAY.POLICY !   sz r LAY.SIZE !
   al r LAY.ALIGN !   tw r LAY.TAGW !
   id ;

\ packed memory ABI descriptor (TL-PACKED-TAG). packed keeps the stack width W
\ of stack-cell-tag and adds a memory descriptor for ABI-stable buffers. Payloads
\ are cell-kinded, so the only field packed narrows is the tag: a K-variant tag
\ is stored in the smallest of u8/u16/u32/cell holding [0,K). Payloads stay
\ CELL-wide (slot i at byte i*CELL), so SIZE/ALIGN/TAGW specify the ABI. The tag
\ sits after the payload, matching the stack order; SIZE is the aligned record
\ stride. These compute the descriptor for any family, independent of its
\ declared policy.

private

: PACKED-ALIGN-UP ( n n -- n ) {: v:n a:n :}   \ round v up to a multiple of pow2 a
   v a 1- + a 1- invert and ;

public

: PACKED-NARROW ( n -- n )   \ smallest byte width 1/2/4/8 holding tags [0,count); 0 if none
   {: count:n :}
   count 0 <= IF 0 EXIT THEN
   count 256 <= IF 1 EXIT THEN
   count 65536 <= IF 2 EXIT THEN
   count 1 32 lshift <= IF 4 EXIT THEN
   8 ;

private

: PACKED-TAGW ( n -- n ) {: fam:n :}   \ narrowed tag byte width (0 for tag-less products)
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF 0 EXIT THEN
   fam TFAM-VAR-COUNT@ PACKED-NARROW ;
: PACKED-ALIGN ( n n -- n ) {: pay:n tw:n :}   \ record alignment from payload bytes + tag width
   pay 0 > IF CELL EXIT THEN     \ any cell payload -> cell alignment
   tw 0 > IF tw EXIT THEN        \ tag-only -> tag alignment
   1 ;                           \ defensive empty -> byte

public

: PACKED-DESC ( n -- n n n ) {: fam:n :}   \ ( fam -- size align tagw ) packed ABI descriptor
   fam PACKED-TAGW {: tw:n :}
   fam TFAM-SLOTS@ CELL * {: pay:n :}
   pay tw PACKED-ALIGN {: al:n :}
   pay tw + al PACKED-ALIGN-UP  al  tw ;

\ The committed init field carries T's schema and stack width. Its field row is
\ canonical cell storage, while the value being initialized may have a narrower
\ packed memory descriptor. Keep the element-layout decision in one place for
\ the wrapper and the initialized-record query below.
private
: INIT-ELEM-LAYOUT? ( n -- n n bool ) {: term:n :}   \ T -> bytes, alignment, known?
   term T-RES {: elem:n :}
   elem TAG T-CON = IF CELL CELL RES-TRUE EXIT THEN
   elem TAG T-PARAM <> IF 0 0 RES-FALSE EXIT THEN
   elem PARAM>FAM {: fam:n :}
   fam C2-INIT-FAM @ = IF elem 1 PARAM>ARG RECURSE EXIT THEN
   fam TFAM-LAYOUT? 0= IF CELL CELL RES-TRUE EXIT THEN
   elem LAYOUT-WIDTH-OPEN? IF 0 0 RES-FALSE EXIT THEN
   fam TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG = IF
      elem T-WIDTH CELL * CELL RES-TRUE EXIT
   THEN
   fam TFAM-LAYOUT-POLICY@ TL-PACKED-TAG <> IF 0 0 RES-FALSE EXIT THEN
   elem T-WIDTH fam TFAM-WIDTH@ <> IF 0 0 RES-FALSE EXIT THEN
   fam LAY-FIND IF
      dup LAY-SIZE@ swap LAY-ALIGN@ RES-TRUE
   ELSE drop 0 0 RES-FALSE THEN ;

public
: INIT-LAYOUT? ( n -- n n bool ) {: term:n :}   \ init<I,T> -> T's bytes, alignment, known?
   term T-RES dup TAG T-PARAM <> IF drop 0 0 RES-FALSE EXIT THEN
   dup PARAM>FAM C2-INIT-FAM @ <> IF drop 0 0 RES-FALSE EXIT THEN
   1 PARAM>ARG INIT-ELEM-LAYOUT? ;

\ base-state reset. Refuses while a field transaction is live, so a frame is
\ never discarded under its token holder, and does not rewind PF-TX-SERIAL: the
\ token counter stays monotonic for the process, so a token minted before a
\ reset never aliases a transaction begun after it.
: TFAM-RESET ( -- )
   PF-TX-DEPTH @ IF E-PF-TX throw THEN   \ live field transaction: reset would discard its frame
   UNIT-FAM-RESET-XT
   0 SVX-GEN !                           \ every variant row the index points at is going
   0 TFX-READY !                         \ ... and every family row the tail index chains
   0 VNX-READY !                         \ ... and every row the variant tail index chains
   0 FVX-READY !                         \ ... and every row the field owner index chains
   TFM-EPOCH+                            \ ... and every family a memoized fact describes
   0 TFAM-N @ TF-SCRUB   0 TF-STR-U @ TF-STR-SCRUB   0 TF-PK-N @ TF-PK-SCRUB
   0 SUMV-N @ SUMV-SCRUB   0 PF-N @ PF-SCRUB   0 LAY-N @ LAY-SCRUB   \ scrub the rows it retires
   0 TFAM-N !   0 TF-STR-U !   0 TF-PK-N !
   0 SUMV-N !   0 SVX-HI !   0 PF-N !   0 PF-COMMIT-N !   0 LAY-N !
   -1 FIELD-FAM !  -1 LOAN-FIELD-FAM !  -1 C2-STOW-FAM !
   EXT-FREE-CLEAR ;   \ drop free-extent marks with the families they name
TFAM-RESET

\ The registry rewind: every caller that puts the family and variant stores or
\ the name pool back to an earlier mark goes through here. Three indexes chain
\ rows by id (TFAM-FIND-IN's tail index, SUMV-FROM-CTOR-SYM's symbol index,
\ SUMV-FIND's variant index); a chained row whose id is out of range is found by
\ the next lookup, which reads past the store and dies. So rows are unchained
\ before the counters move. A caller that wrote the counters itself and skipped
\ the retirements left the outer restore popping nothing and stamping the index
\ current; the first lookup of that tail died 76 in TF-REC@ while every
\ watermark was right.
\ The retired family, variant, param-kind and layout rows and pool bytes are
\ scrubbed to zero, as PF-SCRUB does for field rows, after the indexes pop: the
\ pops read the tail links and constructor symbols the scrub clears.
\ Product-field rows and schema nodes have their own frames; a caller that owns
\ them rewinds them after this (field rows through PF-REWIND).
: TFAM-REWIND ( n n n n n -- ) {: tfamn:n stru:n pkn:n sumvn:n layn:n :}
   tfamn TFAM-N @ < stru TF-STR-U @ < or IF UNIT-FAM-RESET-XT THEN
   C2-STOW-FAM @ tfamn >= IF -1 C2-STOW-FAM ! THEN
   tfamn TFX-RETIRE                      \ unchain the rows before their ids go out of range
   sumvn SVX-TRUNCATE                    \ and the constructor heads those rows own
   sumvn VNX-RETIRE                      \ and the tail chains
   tfamn TFAM-N @ TF-SCRUB               \ then scrub the rows this rewind retires
   sumvn SUMV-N @ SUMV-SCRUB
   pkn TF-PK-N @ TF-PK-SCRUB
   layn LAY-N @ LAY-SCRUB
   stru TF-STR-U @ TF-STR-SCRUB
   tfamn TFAM-N @ <> sumvn SUMV-N @ <> or IF TFM-EPOCH+ THEN
   tfamn TFAM-N !
   stru TF-STR-U !
   pkn TF-PK-N !
   sumvn SUMV-N !
   layn LAY-N ! ;

\ rollback frame stack, pushed and popped in lockstep with checker.f's core
\ frame. Each scope or candidate saves the family/variant/field/layout marks and
\ the string-pool and param-kind ends; a reject pops them, leaving no row or
\ interned name behind. Restoring the counters is retirement for the scans
\ (PF-FIND/LAY-FIND scan [0,N)); the indexed lookups unchain from the same
\ restore (TFX-RETIRE, SVX-TRUNCATE, VNX-RETIRE) and the field owner index from
\ the PF-REWIND that follows (FVX-RETIRE).

private

0 cells constant TFRB.TFAMN-OFF
1 cells constant TFRB.STRU-OFF
2 cells constant TFRB.PKN-OFF
3 cells constant TFRB.SUMVN-OFF
4 cells constant TFRB.PFN-OFF
5 cells constant TFRB.LAYN-OFF
6 cells constant TFRB.PFCOMMITN-OFF
7 cells constant TFRB.PFTXDEPTH-OFF

public

8 cells constant TF-RBF-REC
: TFRB.TFAMN ( ptr a -- ptr a ) TFRB.TFAMN-OFF + ;
: TFRB.STRU ( ptr a -- ptr a ) TFRB.STRU-OFF + ;
: TFRB.PKN ( ptr a -- ptr a ) TFRB.PKN-OFF + ;
: TFRB.SUMVN ( ptr a -- ptr a ) TFRB.SUMVN-OFF + ;
: TFRB.PFN ( ptr a -- ptr a ) TFRB.PFN-OFF + ;
: TFRB.LAYN ( ptr a -- ptr a ) TFRB.LAYN-OFF + ;
: TFRB.PFCOMMITN ( ptr a -- ptr a ) TFRB.PFCOMMITN-OFF + ;
: TFRB.PFTXDEPTH ( ptr a -- ptr a ) TFRB.PFTXDEPTH-OFF + ;


private

16 constant TF-RBF-CAP-INIT
variable TF-RBF-CAP-V   TF-RBF-CAP-INIT TF-RBF-CAP-V !
create TF-RBF-BOOT   TF-RBF-CAP-INIT TF-RBF-REC * allot
\ Private: `0 TF-RBF-P !` from a program took the engine down with SIGSEGV
\ (rc 134) inside the next declaration.
PERSISTED-PTR-VARIABLE TF-RBF-P    TF-RBF-BOOT TF-RBF-P !
: TF-RBF-BASE ( -- ptr n ) TF-RBF-P @ ;

public

variable TF-RBF-DEPTH   0 TF-RBF-DEPTH !   REG-PROTECT

private

: TF-RBF-GROW ( -- )
   TF-RBF-CAP-V @ 2 * {: nc:n :}
   TF-RBF-P  TF-RBF-CAP-V @ TF-RBF-REC *  nc TF-RBF-REC *  REG-GROW1
   nc TF-RBF-CAP-V ! ;

public

: TF-RBF-ENSURE ( -- )
   TF-RBF-DEPTH @ TF-RBF-CAP-V @ < IF exit THEN
   TF-RBF-GROW ;
: TF-RBF-CUR ( -- ptr n ) TF-RBF-DEPTH @ TF-RBF-REC * TF-RBF-BASE + ;
: TF-RBF-TOP ( -- ptr n )
   TF-RBF-DEPTH @ 0= IF E-PF-TX throw THEN
   TF-RBF-DEPTH @ 1 - TF-RBF-REC * TF-RBF-BASE + ;

;package

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
   r TFRB.TFAMN @  r TFRB.STRU @  r TFRB.PKN @  r TFRB.SUMVN @  r TFRB.LAYN @
   TFAM-REWIND                        \ retires the three indexes, then moves the counters
   r TFRB.PFN @ r TFRB.PFCOMMITN @ PF-REWIND ;   \ the field rows this rejected declaration added

: TF-RESTORE ( -- )
   TF-RBF-TOP PF-DEPTH=
   TF-RBF-TOP TF-RESTORE-TOP ;

: TF-FINALIZE ( -- )
   TF-RBF-TOP PF-DEPTH=
   TF-RELEASE ;

;package

package TFAM

\ Frames are transient (depth 0 at snapshot): drop any grown arena back to the
\ baked boot store.
: TFAM-RBF-SNAP-RESET ( -- )
   TF-RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   TF-RBF-BOOT TF-RBF-P !
   TF-RBF-CAP-INIT TF-RBF-CAP-V !
   TF-RBF-BOOT 0 TF-RBF-CAP-INIT TF-RBF-REC * CELL / ARENA-CELLS-ZERO
   0 TF-RBF-DEPTH ! ;

: PF-TX-SNAP-RESET ( -- )
   PF-TX-DEPTH @ IF s" checker: snapshot inside field transaction" 76 die THEN
   PF-TX-P PF-TX-BOOT PF-TX-CAP-V @ PF-TX-REC * ARENA-BOOT-RESTORE
   PF-TX-CAP-INIT PF-TX-CAP-V ! ;

\ ---- the core-prefix boundary: this file's half -------------------------------
\ checker.f CHECKER-BOUND owns the mark and the seam; these bodies hang off
\ REG-EXT-BND-SAVE-XT / REG-EXT-BND-RESTORE-XT. Without them the registries keep
\ every row the boot declared after the mark, and a later snapshot redeclares
\ one on the first `require` and dies `duplicate family` (measured, at
\ 'option'). It is a recorded set, not a frame: the SAVE/RESTORE stack moves in
\ lockstep with the checker's core frame, and an unpaired push held across the
\ boot fails the first declaration after it (measured: throw 7113). It records
\ TF-SAVE's counters and runs TF-RESTORE-TOP's body minus TF-RELEASE (the pop).
\ Both halves refuse a live field transaction or an open rollback frame: a
\ recorded set describes the registries only when nothing is half-declared.

;package

package SCHEMA-REG

private

variable BTFAM  variable BSTRU  variable BPK  variable BSUMV  variable BLAY
variable BPF    variable BPFC   variable BSCH  variable BSCHR

: AT-REST ( -- )
   PF-TX-DEPTH @ IF
      s" checker: prefix boundary inside field transaction" 76 die THEN
   TF-RBF-DEPTH @ IF
      s" checker: prefix boundary inside family rollback scope" 76 die THEN
   SCH-RBF-DEPTH @ IF
      s" checker: prefix boundary inside schema rollback scope" 76 die THEN ;

: PFX-MARK ( -- )
   AT-REST
   TFAM-N @ BTFAM !
   TF-STR-U @ BSTRU !
   TF-PK-N @ BPK !
   SUMV-N @ BSUMV !
   LAY-N @ BLAY !
   PF-N @ BPF !
   PF-COMMIT-N @ BPFC !
   COUNTS BSCHR ! BSCH ! ;

: PFX-REWIND ( -- )
   AT-REST
   BSCH @ BSCHR @ REWIND
   BTFAM @ BSTRU @ BPK @ BSUMV @ BLAY @ TFAM-REWIND
   BPF @ BPFC @ PF-REWIND ;

\ Installed, not published: the boundary's one public seam is the checker's, so
\ these registries cannot be rewound at the boundary by anything else.
: INSTALL-BOUND ( -- )
   [: PFX-MARK ;] is REG-EXT-BND-SAVE-XT
   [: PFX-REWIND ;] is REG-EXT-BND-RESTORE-XT ;
INSTALL-BOUND

;package

package TFAM

\ One SAVE/RESTORE pair the checker's core RBF-PUSH/POP drives, so TFAM and
\ SCHEMA frames stay in lockstep with core marks.

;package

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

package TFAM

\ Persist runs at depth 0 (PF-TX-SNAP-RESET / TFAM-RBF-SNAP-RESET assert it), so
\ PF-N = PF-COMMIT-N and [PF-COMMIT-N, PF-CAP) is dead capacity: zeroed so that
\ identical logical registries bake byte-identically.
: PF-PERSIST-CANONICAL ( -- )
   PF-COMMIT-N @ PF-CAP-V @ PF-SCRUB ;

\ snapshot persist: bake grown TFAM/SUMV/field/layout/param-kind/string stores
\ into image DATA. All record fields are integers or interned offsets, so nothing
\ rebases. Wired into CHECKER-CAPTURE-PREPARE through the REG-EXT-PERSIST-XT hook.

public

: TFAM-SNAPSHOT-PERSIST ( -- )
   TF-A-P    TF-CAP-V @ TF-REC *      REG-PERSIST-BUF drop
   TF-PK-P   TF-PK-CAP-V @ cells      REG-PERSIST-BUF drop
   SUMV-A-P  SUMV-CAP-V @ SUMV-REC *  REG-PERSIST-BUF drop
   PF-PERSIST-CANONICAL
   PF-A-P    PF-CAP-V @ PF-REC *      REG-PERSIST-BUF drop
   LAY-A-P   LAY-CAP-V @ LAY-REC *    REG-PERSIST-BUF drop
   TF-STR-P TF-STR-HOME TF-STR-HOME-CAP TF-STR-U TF-STR-CAP-V REG-POOL-PERSIST ;

\ The bucket array is grown and process-local: dropped back to the boot store
\ and marked not-ready. TFX-BUILD rewrites every row's TF.TAILNEXT, so each is
\ zeroed too; left in place, a row's link would depend on whether anything built
\ the index before capture, and two loads of one program would capture different
\ bytes.

private

: TFX-SNAP-RESET ( -- )
   TFX-A-BOOT TFX-A-P !
   TFX-SLOTS-INIT TFX-SLOTS-V !
   0 TFX-CAP !
   0 TFX-READY !
   0 TFX-HI !
   0 TFX-H ! 0 TFX-I ! 0 TFX-CUR !
   TFAM-N @ 0 ?do 0 i TF-REC@ TF.TAILNEXT ! loop ;

\ AOT capture: the registry delta a captured window declared, and installing it
\ in the engine seeded with that window. A seeded word's signature is text the
\ intake parses (`( IR-ARENA:view -- )` resolves `view` in IR-ARENA), so a
\ window that declared families carries them.
\ References are integer row ids and interned offsets. Partial payloads append
\ rows only after all eight captured prefixes compare equal to the live stores;
\ installed deltas must also compare equal, since a count cannot prove
\ identity. Only the rebuilt family tail link and the scrubbed constructor-symbol
\ cell are excluded. Every store, reference and schema is validated and all
\ capacity exists before the first count or row is published: a failure may
\ leave private capacity, never a partial registry.
8 constant REG-AOT-N                      \ stores, in the order below
24 constant REG-AOT-ROW                   \ base, delta count, complete-store bytes
REG-AOT-N REG-AOT-ROW * 8 + constant REG-AOT-HDR

variable REG-AOT-STATE  variable REG-AOT-J  variable REG-AOT-CUR
variable REG-AOT-CLOSED
create REG-AOT-MARK-A REG-AOT-N cells allot
create REG-AOT-END-A REG-AOT-N cells allot

: REG-AOT-WIDTH ( n -- n ) {: k:n :}
   k 0 = IF TF-REC EXIT THEN
   k 1 = IF CELL EXIT THEN
   k 2 = IF SUMV-REC EXIT THEN
   k 3 = IF PF-REC EXIT THEN
   k 4 = IF LAY-REC EXIT THEN
   k 5 = IF 1 EXIT THEN
   k 6 = IF SCH-REC EXIT THEN
   CELL ;

: REG-AOT-COUNT ( n -- n ) {: k:n :}
   k 0 = IF TFAM-N @ EXIT THEN
   k 1 = IF TF-PK-N @ EXIT THEN
   k 2 = IF SUMV-N @ EXIT THEN
   k 3 = IF PF-COMMIT-N @ EXIT THEN
   k 4 = IF LAY-N @ EXIT THEN
   k 5 = IF TF-STR-U @ EXIT THEN
   k 6 = IF SCH-N @ EXIT THEN
   SCH-ROOT-N @ ;

\ The rows a family contract covers: every live row. Inside a declaration
\ that includes its provisional field rows [PF-COMMIT-N, PF-N); a word the
\ declaration generates is captured there, and commit moves no row, so its
\ contract equals the one a later word computes over the committed rows.
: REG-LIVE-COUNT ( n -- n ) {: k:n :}
   k 3 = IF PF-N @ EXIT THEN
   k REG-AOT-COUNT ;

: REG-AOT-BASE-PTR ( n -- ptr u8 ) {: k:n :}
   k 0 = IF TF-BASE BYTE-VIEW EXIT THEN
   k 1 = IF TF-PK-BASE BYTE-VIEW EXIT THEN
   k 2 = IF SUMV-BASE BYTE-VIEW EXIT THEN
   k 3 = IF PF-BASE BYTE-VIEW EXIT THEN
   k 4 = IF LAY-BASE BYTE-VIEW EXIT THEN
   k 5 = IF TF-STR EXIT THEN
   k 6 = IF SCH-BASE BYTE-VIEW EXIT THEN
   SCH-ROOT-BASE BYTE-VIEW ;

: REG-AOT-NAME ( n -- ptr u8 n ) {: k:n :}
   k 0 = IF s" families" EXIT THEN
   k 1 = IF s" parameter kinds" EXIT THEN
   k 2 = IF s" sum variants" EXIT THEN
   k 3 = IF s" product fields" EXIT THEN
   k 4 = IF s" layouts" EXIT THEN
   k 5 = IF s" type names" EXIT THEN
   k 6 = IF s" schema nodes" EXIT THEN
   s" schema roots" ;

\ Room for `n` elements in store k, through the store's own geometric grow: each
\ step is the grow the compiled path's ENSURE takes when the store is full
\ (`cap 1+`), so a seeded delta lands in an arena of the capacity a compiled one
\ reaches rather than the grow's floor `n`. The type-name store's compiled ENSURE
\ asks for max(used + name bytes, 2*cap), which is 2*cap here: names are at most
\ TF-NAME-MAX bytes and every baked name store is larger, and persist places a
\ moved name store by its used size and home (REG-POOL-PERSIST).
: REG-AOT-ROOM ( n n -- ) {: k:n n:n :}
   k 0 = IF begin n TF-CAP-V @ > while TF-CAP-V @ 1+ TF-GROW repeat EXIT THEN
   k 1 = IF begin n TF-PK-CAP-V @ > while TF-PK-CAP-V @ 1+ TF-PK-GROW repeat EXIT THEN
   k 2 = IF begin n SUMV-CAP-V @ > while SUMV-CAP-V @ 1+ SUMV-GROW repeat EXIT THEN
   k 3 = IF begin n PF-CAP-V @ > while PF-CAP-V @ 1+ PF-GROW repeat EXIT THEN
   k 4 = IF begin n LAY-CAP-V @ > while LAY-CAP-V @ 1+ LAY-GROW repeat EXIT THEN
   k 5 = IF begin n TF-STR-CAP-V @ > while TF-STR-CAP-V @ 1+ TF-STR-GROW repeat EXIT THEN
   k 6 = IF begin n SCH-CAP-V @ > while SCH-CAP-V @ 1+ SCH-GROW repeat EXIT THEN
   begin n SCH-ROOT-CAP-V @ > while SCH-ROOT-CAP-V @ 1+ SCH-ROOT-GROW repeat ;

: REG-AOT-COUNT! ( n n -- ) {: k:n n:n :}
   k 0 = IF n TFAM-N ! EXIT THEN
   k 1 = IF n TF-PK-N ! EXIT THEN
   k 2 = IF n SUMV-N ! EXIT THEN
   k 3 = IF n PF-N !  n PF-COMMIT-N ! EXIT THEN
   k 4 = IF n LAY-N ! EXIT THEN
   k 5 = IF n TF-STR-U ! EXIT THEN
   k 6 = IF n SCH-N ! EXIT THEN
   n SCH-ROOT-N ! ;

: REG-AOT-U64! ( n ptr u8 -- ) {: v:n p:ptr :}
   8 0 ?do  v i 8 * rshift $FF and  p i + c!  loop ;

: REG-AOT-U64@ ( ptr u8 -- n ) {: p:ptr :}
   0 8 0 ?do  8 lshift  p 7 i - + c@ or  loop ;

: REG-AOT-MARK@ ( n -- n ) {: k:n :} REG-AOT-MARK-A k cells + @ ;
: REG-AOT-END@ ( n -- n ) {: k:n :} REG-AOT-END-A k cells + @ ;

\ The high-waters the window opens on, latched by src/habu/aot-arm.f OPEN from
\ the same call that arms the signature collection - one window, one base.
: REG-AOT-MARK ( -- )
   0 REG-AOT-CLOSED !
   REG-AOT-N 0 ?do  i REG-AOT-COUNT  REG-AOT-MARK-A i cells +  !  loop ;

\ The high-waters the window closes on, latched at aot-arm.f SIG-CLOSE. A capture
\ tool loads its own sources after the window shuts and those declare types; read
\ at capture time the delta carried them, and the seeded engine measured its
\ registry against a base counting types no target has (measured: the window
\ declares 70 families and the capture read more). A build that captures the
\ same window twice ends its types twice.
: REG-AOT-CLOSE ( -- )
   REG-AOT-N 0 ?do  i REG-AOT-COUNT  REG-AOT-END-A i cells +  !  loop
   -1 REG-AOT-CLOSED ! ;

\ ---- writing the prefix and delta -------------------------------------------

: REG-AOT-OVERFLOW ( -- )
   s" tfam: the captured type registry does not fit the artifact's buffer" 74 die ;

\ A store's payload is its whole contents through the closed end, prefix
\ included; a table row is (base, delta-count, bytes). The prefix is carried
\ even when the window added no row: equal counts can hold different types.

\ Schema constructor codes past the fixed primitive table are process-local:
\ a capture holding one is refused.
: REG-AOT-SCHEMA-CON-PORTABLE ( -- )
   6 REG-AOT-END@ 1 ?do
      i SCHEMA-CON? IF
         i SCHEMA-A@ dup CC-N < swap CC-MAX >= or IF
            s" tfam: captured schema constructor has process-local identity" 76 die
         THEN
      THEN
   loop ;

: REG-AOT-SAVE ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   REG-AOT-CLOSED @ 0= IF
      s" tfam: a type registry was captured from a window that never closed" 74 die
   THEN
   REG-AOT-SCHEMA-CON-PORTABLE
   cap REG-AOT-HDR < IF REG-AOT-OVERFLOW THEN
   REG-AOT-N dst REG-AOT-U64!
   REG-AOT-HDR REG-AOT-CUR !
   REG-AOT-N 0 ?do
      i REG-AOT-MARK@ {: base:n :}
      i REG-AOT-END@ base - {: cnt:n :}
      cnt 0 < IF
         s" tfam: a type registry shrank across the capture window" 74 die
      THEN
      i REG-AOT-END@ cap REG-AOT-CUR @ - i REG-AOT-WIDTH / > IF
         REG-AOT-OVERFLOW THEN
      i REG-AOT-END@ i REG-AOT-WIDTH * {: bytes:n :}
      base   dst i REG-AOT-ROW * 8 + +       REG-AOT-U64!
      cnt    dst i REG-AOT-ROW * 8 + 8 + +   REG-AOT-U64!
      bytes  dst i REG-AOT-ROW * 8 + 16 + +  REG-AOT-U64!
      bytes 0 > IF
         i REG-AOT-BASE-PTR
         dst REG-AOT-CUR @ +
         bytes ARENA-COPY
      THEN
      REG-AOT-CUR @ bytes + REG-AOT-CUR !
   loop
   REG-AOT-CUR @ ;

\ ---- putting it back ---------------------------------------------------------

PTR-VARIABLE REG-AOT-ERROR-A
variable REG-AOT-ERROR-U

: REG-AOT-REFUSE ( ptr u8 n -- )
   REG-AOT-ERROR-U ! REG-AOT-ERROR-A ! E-REG-AOT-REFUSED throw ;

: UNIT-FAM-CHANGED ( ptr u8 n -- )
   REG-AOT-ERROR-U ! REG-AOT-ERROR-A ! E-UNIT-FAMILY-CHANGED throw ;

: REG-AOT-BASE-BAD ( n n n -- )
   drop 2drop
   s" tfam: a seeded type registry does not start where its capture did" REG-AOT-REFUSE ;

\ SV.CTOR-SYM names a row of the checker's symbol store, which has no base
\ alignment between the capture engine and this one. Measured: seeded rows
\ carried ctor-sym 7750 against a boot SYM-N of 3147, and a later intern of
\ 7750 bound the stale variant and wedged the install fixpoint. The load zeroes
\ the cell; SVX-LINK and SUMV-FROM-CTOR-SYM refuse to key on 0. A seeded
\ constructor is an ordinary word whose calls certify through its signature row.
: REG-AOT-SCRUB ( n n n -- ) {: k:n base:n cnt:n :}
   k 2 <> IF EXIT THEN
   base REG-AOT-J !
   BEGIN REG-AOT-J @ base cnt + < WHILE
      0 REG-AOT-J @ SUMV-REC@ SV.CTOR-SYM !
      REG-AOT-J @ 1 + REG-AOT-J !
   REPEAT ;

: REG-AOT-ROW@ ( ptr u8 n -- n n n ) {: src:ptr k:n :}
   src k REG-AOT-ROW * 8 + + {: row:ptr :}
   row REG-AOT-U64@  row 8 + REG-AOT-U64@  row 16 + REG-AOT-U64@ ;

\ These two cells are derived runtime associations, not registry identity:
\ TAILNEXT is rebuilt by TFX; CTOR-SYM is deliberately scrubbed on import.
: REG-AOT-CANON-C@ ( ptr u8 n n -- n ) {: src:ptr off:n k:n :}
   k 0 = IF
      off TF-REC mod {: field:n :}
      field TF.TAILNEXT-OFF >= field TF.TAILNEXT-OFF CELL + < and IF 0 EXIT THEN
   THEN
   k 2 = IF
      off SUMV-REC mod {: field:n :}
      field SV.CTOR-SYM-OFF >= field SV.CTOR-SYM-OFF CELL + < and IF 0 EXIT THEN
   THEN
   src off + c@ ;

\ Validates a captured table without the active checker: merge may compare two
\ frozen windows after the live registry has changed owners.
: REG-AOT-SHAPE ( ptr u8 n -- bool ) {: src:ptr u:n :}
   u REG-AOT-HDR < IF
      s" tfam: captured registry is shorter than its table" REG-AOT-REFUSE THEN
   src REG-AOT-U64@ REG-AOT-N <> IF
      s" tfam: captured registry has an invalid store count" REG-AOT-REFUSE THEN
   0 REG-AOT-HDR
   REG-AOT-N 0 ?do
      {: delta:n used:n :}
      src i REG-AOT-ROW@ {: base:n cnt:n bytes:n :}
      base 0 < cnt 0 < or bytes 0 < or IF
         s" tfam: captured registry has a negative extent" REG-AOT-REFUSE THEN
      u used - i REG-AOT-WIDTH / {: room:n :}
      base room > IF
         s" tfam: captured registry prefix exceeds its bytes" REG-AOT-REFUSE THEN
      cnt room base - > IF
         s" tfam: captured registry delta exceeds its bytes" REG-AOT-REFUSE THEN
      base cnt + i REG-AOT-WIDTH * bytes <> IF
         s" tfam: captured registry has an invalid record extent" REG-AOT-REFUSE THEN
      delta cnt or used bytes +
   loop
   u <> IF s" tfam: captured registry does not fill its bytes" REG-AOT-REFUSE THEN
   0 <> ;

: REG-AOT-MERGE-REFUSE ( -- )
   s" tfam: captured registries have incompatible prefixes" REG-AOT-REFUSE ;

\ True iff the incoming payload is the one to retain. A prefix-only capture can
\ accompany a delta at its opening or closed boundary. Compares canonical
\ records, not byte lengths or the installed registry.
: REG-AOT-MERGE-INCOMING-BODY ( ptr u8 n ptr u8 n -- bool )
   {: host:ptr hu:n incoming:ptr iu:n :}
   hu 0= IF iu 0 <> IF incoming iu REG-AOT-SHAPE drop THEN RES-TRUE EXIT THEN
   host hu REG-AOT-SHAPE {: hd:bool :}
   iu 0= IF RES-FALSE EXIT THEN
   incoming iu REG-AOT-SHAPE {: id:bool :}
   hd id and IF
      s" tfam: two type-registry deltas cannot share one base" REG-AOT-REFUSE THEN
   REG-AOT-HDR REG-AOT-HDR
   REG-AOT-N 0 ?do
      {: ho:n io:n :}
      host i REG-AOT-ROW@ {: hb:n hc:n hbytes:n :}
      incoming i REG-AOT-ROW@ {: ib:n ic:n ibytes:n :}
      hd IF
         ib hb < ib hb hc + > or IF REG-AOT-MERGE-REFUSE THEN
      ELSE id IF
         hb ib < hb ib ic + > or IF REG-AOT-MERGE-REFUSE THEN
      ELSE
         hb ib <> IF REG-AOT-MERGE-REFUSE THEN
      THEN THEN
      hbytes ibytes min 0 ?do
         host ho + i j REG-AOT-CANON-C@
         incoming io + i j REG-AOT-CANON-C@ <> IF REG-AOT-MERGE-REFUSE THEN
      loop
      ho hbytes + io ibytes +
   loop 2drop
   hd 0= ;

variable REG-AOT-KEEP-INCOMING

: REG-AOT-MERGE-CHECK ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   2over 2over REG-AOT-MERGE-INCOMING-BODY REG-AOT-KEEP-INCOMING ! ;

public

: REG-AOT-MERGE-INCOMING? ( ptr u8 n ptr u8 n -- bool )
   0 REG-AOT-ERROR-U !
   ['] REG-AOT-MERGE-CHECK catch dup 0 <> IF
      REG-AOT-ERROR-U @ 0= IF throw THEN
      drop 2drop 2drop REG-AOT-ERROR-A @ REG-AOT-ERROR-U @ 76 die
   THEN drop 2drop 2drop REG-AOT-KEEP-INCOMING @ 0 <> ;

private

: REG-AOT-SAME? ( ptr u8 n n n -- bool ) {: src:ptr k:n base:n bytes:n :}
   k REG-AOT-BASE-PTR base k REG-AOT-WIDTH * + {: live:ptr :}
   bytes 0 ?do
      src i k REG-AOT-CANON-C@ live i k REG-AOT-CANON-C@ <> IF
         RES-FALSE unloop EXIT THEN
   loop RES-TRUE ;

PTR-VARIABLE REG-AOT-PROJECTED
variable REG-AOT-PROJECTED-U

variable UNIT-FAM-PROJECT-ON
defer UNIT-FAM-PROJECT-VIEW-XT ( n n n -- ptr u8 )
: UNIT-FAM-NO-VIEW ( n n n -- ptr u8 ) drop 2drop NULL-PTR ;
: UNIT-FAM-NO-VIEW-INSTALL ( -- )
   [: UNIT-FAM-NO-VIEW ;] is UNIT-FAM-PROJECT-VIEW-XT ;
UNIT-FAM-NO-VIEW-INSTALL

: REG-AOT-BLOB-STORE ( ptr u8 n -- ptr u8 ) {: src:ptr k:n :}
   REG-AOT-HDR
   k 0 ?do src i REG-AOT-ROW@ nip nip + loop
   src + ;

: REG-AOT-PREFIX= ( ptr u8 n n n -- bool )
   {: src:ptr k:n base:n bytes:n :}
   REG-AOT-PROJECTED-U @ 0= IF src k base bytes REG-AOT-SAME? EXIT THEN
   REG-AOT-PROJECTED @ k REG-AOT-BLOB-STORE
   base k REG-AOT-WIDTH * + {: prior:ptr :}
   bytes 0 ?do
      src i k REG-AOT-CANON-C@ prior i k REG-AOT-CANON-C@ <> IF
         RES-FALSE unloop EXIT THEN
   loop RES-TRUE ;

: REG-AOT-PROJECTED-COUNT ( n -- n ) {: k:n :}
   REG-AOT-PROJECTED-U @ 0= IF k REG-AOT-COUNT EXIT THEN
   REG-AOT-PROJECTED @ k REG-AOT-ROW@ drop + ;

\ Validate every base and byte span before reserving or publishing any store.
\ Subtraction bounds the product and endpoint before either is computed.
: REG-AOT-TABLE-CHECK ( ptr u8 n -- ) {: src:ptr u:n :}
   u 0= IF EXIT THEN
   u REG-AOT-HDR < IF
      s" tfam: a seeded type registry is shorter than its own table" REG-AOT-REFUSE THEN
   src REG-AOT-U64@ REG-AOT-N <> IF
      s" tfam: a seeded type registry names a store count this engine cannot read" REG-AOT-REFUSE THEN
   REG-AOT-HDR REG-AOT-CUR !
   0 REG-AOT-STATE !              \ nonempty stores: 1 fresh, 2 already present
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ {: base:n cnt:n bytes:n :}
      base 0 < cnt 0 < or bytes 0 < or IF
         s" tfam: a seeded type registry has a negative base, count or length" REG-AOT-REFUSE THEN
      u REG-AOT-CUR @ - i REG-AOT-WIDTH / {: room:n :}
      base room > IF
         s" tfam: a seeded type registry prefix runs past its own bytes" REG-AOT-REFUSE THEN
      cnt room base - > IF
         s" tfam: a seeded type registry section runs past its own bytes" REG-AOT-REFUSE THEN
      base cnt + i REG-AOT-WIDTH * bytes <> IF
         s" tfam: a seeded type registry section is not a whole number of records" REG-AOT-REFUSE THEN
      base $7FFFFFFFFFFFFFFF i REG-AOT-WIDTH / cnt - > IF
         s" tfam: a seeded type registry endpoint overflows" REG-AOT-REFUSE THEN
      i REG-AOT-PROJECTED-COUNT {: live:n :}
      live base <> live base cnt + <> and IF
         i base live REG-AOT-BASE-BAD THEN
      src REG-AOT-CUR @ + i 0 base i REG-AOT-WIDTH * REG-AOT-PREFIX= 0= IF
         s" tfam: a seeded type registry differs from its captured prefix" REG-AOT-REFUSE THEN
      cnt 0 > IF
         live base = IF 1 ELSE 2 THEN {: next:n :}
         REG-AOT-STATE @ 0 <> REG-AOT-STATE @ next <> and IF
            s" tfam: seeded registry stores disagree about installation" REG-AOT-REFUSE THEN
         next REG-AOT-STATE !
         next 2 = IF
            src REG-AOT-CUR @ + base i REG-AOT-WIDTH * +
            i base cnt i REG-AOT-WIDTH * REG-AOT-PREFIX= 0= IF
               s" tfam: a seeded type registry differs from the installed records" REG-AOT-REFUSE THEN
         THEN
      THEN
      REG-AOT-CUR @ bytes + REG-AOT-CUR !
   loop
   REG-AOT-CUR @ u <> IF
      s" tfam: a seeded type registry does not fill its own bytes" REG-AOT-REFUSE THEN ;

\ Reads a wholly prefix-owned or wholly incoming span without installing it;
\ table validation precedes it and no live count changes.
: REG-AOT-VIEW ( ptr u8 n n n n -- ptr u8 )
   {: src:ptr u:n k:n first:n count:n :}
   first 0 < count 0 < or IF
      s" tfam: a seeded registry reference is negative" REG-AOT-REFUSE THEN
   UNIT-FAM-PROJECT-ON @ IF
      k first count UNIT-FAM-PROJECT-VIEW-XT EXIT THEN
   u 0= IF
      first k REG-AOT-COUNT > IF
         s" tfam: a seeded registry reference exceeds the prefix" REG-AOT-REFUSE THEN
      count k REG-AOT-COUNT first - > IF
         s" tfam: a seeded registry span exceeds the prefix" REG-AOT-REFUSE THEN
      k REG-AOT-BASE-PTR first k REG-AOT-WIDTH * + EXIT
   THEN
   src k REG-AOT-ROW@ drop {: base:n cnt:n :}
   first base cnt + > IF
      s" tfam: a seeded registry reference exceeds its section" REG-AOT-REFUSE THEN
   count base cnt + first - > IF
      s" tfam: a seeded registry span exceeds its section" REG-AOT-REFUSE THEN
   src k REG-AOT-BLOB-STORE first k REG-AOT-WIDTH * + ;

: REG-AOT-NAME= ( ptr u8 n n n ptr u8 n -- bool )
   {: src:ptr u:n off:n bytes:n name:ptr nameu:n :}
   src u 5 off bytes REG-AOT-VIEW bytes name nameu CORE-STR= ;

\ A graph's family ID is meaningful only with its canonical package/tail,
\ arity and physical/logical form. Consult incoming records even if a previous
\ idempotent install has already put those IDs into the live registry.
: REG-AOT-PARAM-CHECK ( ptr u8 ptr u8 ptr u8 n -- )
   {: node:ptr graph:ptr src:ptr u:n :}
   src u 0 node EN.H @ 1 REG-AOT-VIEW CELL-VIEW {: rec:ptr :}
   rec TF.ARITY @ node EN.C @ <> IF
      s" tfam: a seeded effect has the wrong family arity" REG-AOT-REFUSE THEN
   src u rec TF.NAME-OFF @ rec TF.NAME-U @
   graph node EN.A @ + node EN.B @ REG-AOT-NAME= 0= IF
      s" tfam: a seeded effect names a different family" REG-AOT-REFUSE THEN
   src u rec TF.PKG-OFF @ rec TF.PKG-U @
   graph node EN.F @ + node EN.G @ REG-AOT-NAME= 0= IF
      s" tfam: a seeded effect names a different family package" REG-AOT-REFUSE THEN
   rec TF.KIND @ dup 0 < swap TK-MAX > or IF
      s" tfam: a seeded effect has an invalid family kind" REG-AOT-REFUSE THEN
   rec TF.LAYOUT @ dup 0 < swap TL-MAX > or IF
      s" tfam: a seeded effect has an invalid family layout" REG-AOT-REFUSE THEN
   node EN.E @ 0 < IF
      s" tfam: a seeded effect has a negative hidden field" REG-AOT-REFUSE THEN
   node EN.E @ 0 > IF
      rec TF.KIND @ TK-CELL = rec TF.KIND @ TK-EVIDENCE = or IF
         s" tfam: a seeded cell family cannot have a hidden field" REG-AOT-REFUSE THEN
   THEN
   src u 1 rec TF.PK-START @ rec TF.ARITY @ REG-AOT-VIEW drop ;

\ A graph-callback refusal dies 76 like the loader's. 2over 2over keeps the
\ quotation balanced on both paths.
: REG-AOT-PARAM? ( ptr u8 ptr u8 ptr u8 n -- )
   0 REG-AOT-ERROR-U !
   [: 2over 2over REG-AOT-PARAM-CHECK ;] catch
   dup 0 <> IF
      REG-AOT-ERROR-U @ 0= IF throw THEN
      drop 2drop 2drop REG-AOT-ERROR-A @ REG-AOT-ERROR-U @ 76 die
   THEN drop 2drop 2drop ;

\ Semantic references are checked through the uninstalled future view. No
\ count, constructor association or lookup index changes during this pass.
: REG-AOT-ITEM ( ptr u8 n n n -- ptr n )
   1 REG-AOT-VIEW CELL-VIEW ;

\ The graph validator asks the already validated future registry whether an
\ argument is a scope slot. A live family id is not authority during intake.
: REG-AOT-SCOPE? ( ptr u8 ptr u8 n n -- bool )
   {: node:ptr src:ptr u:n idx:n :}
   src u 0 node EN.H @ 1 REG-AOT-VIEW CELL-VIEW {: rec:ptr :}
   idx 0 < idx rec TF.ARITY @ >= or IF
      s" tfam: a seeded scope slot exceeds the family arity" REG-AOT-REFUSE THEN
   src u 1 rec TF.PK-START @ idx + REG-AOT-ITEM @ PK-SCOPE = ;

: REG-AOT-REGION? ( ptr u8 ptr u8 n n -- bool )
   {: node:ptr src:ptr u:n idx:n :}
   src u 0 node EN.H @ 1 REG-AOT-VIEW CELL-VIEW {: rec:ptr :}
   idx 0 < idx rec TF.ARITY @ >= or IF
      s" tfam: a seeded region slot exceeds the family arity" REG-AOT-REFUSE THEN
   src u 1 rec TF.PK-START @ idx + REG-AOT-ITEM @ PK-REGION = ;

: REG-AOT-RANGE ( ptr u8 n n n n -- ) REG-AOT-VIEW drop ;

: REG-AOT-NONNEG ( n -- )
   0 < IF s" tfam: a seeded registry scalar is negative" REG-AOT-REFUSE THEN ;

: REG-AOT-CON-VALID? ( n -- bool ) {: id:n :}
   id CC-N < IF RES-FALSE EXIT THEN
   id CC-MAX < IF RES-TRUE EXIT THEN
   UNIT-FAM-PROJECT-ON @ IF id CHECKER-REG:UNIT-CON-VALID-XT EXIT THEN
   RES-FALSE ;

: REG-AOT-LIMIT ( n n -- ) {: value:n ceiling:n :}
   value 0 < value ceiling > or IF
      s" tfam: a seeded registry scalar is outside its domain" REG-AOT-REFUSE THEN ;

: REG-AOT-STRING ( ptr u8 n n n -- ) {: src:ptr u:n off:n len:n :}
   src u 5 off len REG-AOT-RANGE ;

: REG-AOT-TAIL ( ptr u8 n n n -- ) {: src:ptr u:n off:n len:n :}
   src u 5 off len REG-AOT-VIEW len TF-CANON? 0= IF
      s" tfam: a seeded registry name is not canonical" REG-AOT-REFUSE THEN ;

: REG-AOT-FAMILY ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 0 id REG-AOT-ITEM {: row:ptr :}
   src u row TF.PKG-OFF @ row TF.PKG-U @ REG-AOT-STRING
   src u row TF.NAME-OFF @ row TF.NAME-U @ REG-AOT-TAIL
   row TF.VIS @ dup CHECKER-PACKAGE-PRIVATE <
   swap CHECKER-PACKAGE-PUBLIC > or IF
      s" tfam: a seeded family has invalid visibility" REG-AOT-REFUSE THEN
   row TF.KIND @ TK-MAX REG-AOT-LIMIT
   row TF.LAYOUT @ TL-PACKED-TAG REG-AOT-LIMIT
   row TF.SLOTS @ $7FFFFFFFFFFFFFFF CELL / 1- REG-AOT-LIMIT
   row TF.TAGW @ CELL REG-AOT-LIMIT
   row TF.DERIVE @ DRV-ALL invert and 0 <> IF
      s" tfam: a seeded family has invalid derive flags" REG-AOT-REFUSE THEN
   src u 1 row TF.PK-START @ row TF.ARITY @ REG-AOT-RANGE
   src u 2 row TF.VAR-START @ row TF.VAR-COUNT @ REG-AOT-RANGE
   src u 3 row TF.FLD-START @ row TF.FLD-COUNT @ REG-AOT-RANGE
   row TF.SCHEMA-ROOT @ 0 <> IF src u 7 row TF.SCHEMA-ROOT @ REG-AOT-ITEM drop THEN
   row TF.SPAN-OFF @ REG-AOT-NONNEG row TF.SPAN-U @ REG-AOT-NONNEG
   row TF.KIND @ TK-CELL = row TF.KIND @ TK-EVIDENCE = or IF
      row TF.SLOTS @ row TF.VAR-COUNT @ or row TF.FLD-COUNT @ or 0 <> IF
         s" tfam: a seeded cell family has layout members" REG-AOT-REFUSE THEN
   THEN
   row TF.KIND @ TK-ENUM = IF
      row TF.SLOTS @ row TF.ARITY @ or row TF.FLD-COUNT @ or 0 <> IF
         s" tfam: a seeded compact enum has payload members" REG-AOT-REFUSE THEN
   THEN ;

: REG-AOT-VARIANT ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 2 id REG-AOT-ITEM {: row:ptr :}
   src u 0 row SV.FAM @ REG-AOT-ITEM {: family:ptr :}
   family TF.VAR-START @ {: first:n :}
   id first < IF s" tfam: a seeded variant precedes its family" REG-AOT-REFUSE THEN
   id first - family TF.VAR-COUNT @ >= IF
      s" tfam: a seeded variant is outside its family" REG-AOT-REFUSE THEN
   row SV.TAG @ id first - <> IF
      s" tfam: a seeded variant has the wrong tag" REG-AOT-REFUSE THEN
   src u row SV.NAME-OFF @ row SV.NAME-U @ REG-AOT-TAIL
   src u row SV.CTOR-PKG-OFF @ row SV.CTOR-PKG-U @ REG-AOT-STRING
   src u 7 row SV.SCH-START @ row SV.SCH-COUNT @ REG-AOT-RANGE
   row SV.PAYCELLS @ family TF.SLOTS @ REG-AOT-LIMIT ;

: REG-AOT-SCHEMA-CHILD ( ptr u8 n n n -- ) {: src:ptr u:n parent:n child:n :}
   child 0 <= child parent >= or IF
      s" tfam: a seeded schema has a forward or cyclic edge" REG-AOT-REFUSE THEN
   src u 6 child REG-AOT-ITEM drop ;

: REG-AOT-SCHEMA-ROOTS ( ptr u8 n n n n bool -- )
   {: src:ptr u:n parent:n first:n count:n rows:bool :}
   src u 7 first count REG-AOT-RANGE
   count 0 ?do
      src u 7 first i + REG-AOT-ITEM @ {: child:n :}
      src u parent child REG-AOT-SCHEMA-CHILD
      src u 6 child REG-AOT-ITEM @ SCH-ROW = rows xor IF
         s" tfam: a seeded schema edge has the wrong node kind" REG-AOT-REFUSE THEN
   loop ;

: REG-AOT-SCHEMA ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 6 id REG-AOT-ITEM {: row:ptr :}
   row @ {: tag:n :} row CELL + @ {: a:n :}
   row 2 cells + @ {: b:n :} row 3 cells + @ {: c:n :}
   tag SCH-PARAM = IF
      a REG-AOT-NONNEG b c or 0 <> IF
         s" tfam: a seeded parameter schema has reserved fields" REG-AOT-REFUSE THEN EXIT
   THEN
   tag SCH-CON = IF
      a REG-AOT-CON-VALID? 0= IF
         s" tfam: captured schema constructor has process-local identity" REG-AOT-REFUSE THEN
      b c or 0 <> IF
         s" tfam: a seeded constructor schema has reserved fields" REG-AOT-REFUSE THEN EXIT
   THEN
   tag SCH-PTR = IF
      b c or 0 <> IF
         s" tfam: a seeded pointer schema has reserved fields" REG-AOT-REFUSE THEN
      src u id a REG-AOT-SCHEMA-CHILD
      src u 6 a REG-AOT-ITEM @ SCH-ROW = IF
         s" tfam: a seeded pointer schema names a row" REG-AOT-REFUSE THEN EXIT
   THEN
   tag SCH-APP = IF
      c 0= b 0 <> and IF
         s" tfam: a seeded nullary schema has a nonzero argument base" REG-AOT-REFUSE THEN
      src u 0 a REG-AOT-ITEM TF.ARITY @ c <> IF
         s" tfam: a seeded schema has the wrong family arity" REG-AOT-REFUSE THEN
      src u id b c RES-FALSE REG-AOT-SCHEMA-ROOTS EXIT
   THEN
   tag SCH-QUOT = IF
      a 0 <> a -1 <> and c SCH-QUOT-ROWS <> or IF
         s" tfam: a seeded quotation schema has invalid sides" REG-AOT-REFUSE THEN
      src u id b c RES-TRUE REG-AOT-SCHEMA-ROOTS EXIT
   THEN
   tag SCH-ROW = IF
      c 0 <> IF s" tfam: a seeded row schema has a reserved field" REG-AOT-REFUSE THEN
      src u id a b RES-FALSE REG-AOT-SCHEMA-ROOTS EXIT
   THEN
   s" tfam: a seeded schema has an invalid tag" REG-AOT-REFUSE ;

: REG-AOT-FIELD ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 3 id REG-AOT-ITEM {: row:ptr :}
   src u 0 row PF.FAM @ REG-AOT-ITEM {: family:ptr :}
   src u row PF.NAME-OFF @ row PF.NAME-U @ REG-AOT-TAIL
   src u 7 row PF.SCH @ REG-AOT-ITEM @ {: node:n :}
   src u 6 node REG-AOT-ITEM @ SCH-ROW = IF
      s" tfam: a seeded field names a row schema" REG-AOT-REFUSE THEN
   id family TF.FLD-START @ < IF
      s" tfam: a seeded field precedes its family" REG-AOT-REFUSE THEN
   id family TF.FLD-START @ - family TF.FLD-COUNT @ >= IF
      s" tfam: a seeded field is outside its family" REG-AOT-REFUSE THEN
   row PF.VAR @ -1 = IF
      family TF.KIND @ TK-PRODUCT <> IF
         s" tfam: a seeded untagged field is not product-owned" REG-AOT-REFUSE THEN
   ELSE
      family TF.KIND @ TK-SUM <> IF
         s" tfam: a seeded variant field is not sum-owned" REG-AOT-REFUSE THEN
      src u 2 row PF.VAR @ REG-AOT-ITEM SV.FAM @ row PF.FAM @ <> IF
         s" tfam: a seeded field names another family's variant" REG-AOT-REFUSE THEN
   THEN
   row PF.SLOT @ row PF.CELLS @ PF-RANGE-OK? 0= IF
      s" tfam: a seeded field has an invalid cell range" REG-AOT-REFUSE THEN
   row PF.BYTE-OFF @ row PF.BYTES @ PF-RANGE-OK? 0= IF
      s" tfam: a seeded field has an invalid byte range" REG-AOT-REFUSE THEN
   row PF.SLOT @ family TF.SLOTS @ > IF
      s" tfam: a seeded field starts outside its family" REG-AOT-REFUSE THEN
   row PF.CELLS @ family TF.SLOTS @ row PF.SLOT @ - > IF
      s" tfam: a seeded field extends outside its family" REG-AOT-REFUSE THEN
   row PF.ALIGN @ PF-POW2? 0= IF
      s" tfam: a seeded field alignment is invalid" REG-AOT-REFUSE THEN
   row PF.BYTE-OFF @ row PF.ALIGN @ mod 0 <> row PF.FLAGS @ 0 <> or IF
      s" tfam: a seeded field layout is invalid" REG-AOT-REFUSE THEN ;

: REG-AOT-LAYOUT ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 4 id REG-AOT-ITEM {: row:ptr :}
   src u 0 row LAY.FAM @ REG-AOT-ITEM {: family:ptr :}
   row LAY.POLICY @ TL-PACKED-TAG <>
   row LAY.POLICY @ family TF.LAYOUT @ <> or IF
      s" tfam: a seeded layout has an unsupported or mismatched policy" REG-AOT-REFUSE THEN
   family TF.KIND @ TK-SUM = family TF.KIND @ TK-ENUM = or IF
      family TF.VAR-COUNT @ PACKED-NARROW ELSE 0 THEN {: tagw:n :}
   family TF.SLOTS @ cells {: payload:n :}
   payload 0 > IF CELL ELSE tagw 0 > IF tagw ELSE 1 THEN THEN {: alignment:n :}
   payload tagw + alignment PACKED-ALIGN-UP row LAY.SIZE @ <>
   alignment row LAY.ALIGN @ <> or tagw row LAY.TAGW @ <> or IF
      s" tfam: a seeded layout disagrees with its family descriptor" REG-AOT-REFUSE THEN ;

: REG-AOT-SHAPE-CHECK ( ptr u8 n -- ) {: src:ptr u:n :}
   UNIT-FAM-PROJECT-ON @ 0= IF src u REG-AOT-TABLE-CHECK THEN
   u 0= IF EXIT THEN
   src 6 REG-AOT-ROW@ drop + 1 ?do
      src u 6 i REG-AOT-ITEM {: node:ptr :}
      node @ SCH-CON = IF
         node CELL + @ REG-AOT-CON-VALID? 0= IF
            s" tfam: captured schema constructor has process-local identity" REG-AOT-REFUSE THEN
      THEN
   loop
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ drop {: first:n count:n :}
      count 0 ?do
         first i + {: id:n :}
         j 0 = IF src u id REG-AOT-FAMILY
         ELSE j 1 = IF src u 1 id REG-AOT-ITEM @ PK-MAX REG-AOT-LIMIT
         ELSE j 2 = IF src u id REG-AOT-VARIANT
         ELSE j 3 = IF src u id REG-AOT-FIELD
         ELSE j 4 = IF src u id REG-AOT-LAYOUT
         ELSE j 6 = IF src u id REG-AOT-SCHEMA
         ELSE j 7 = IF
            src u 7 id REG-AOT-ITEM @ {: node:n :}
            node 0 <= IF
               s" tfam: a seeded schema root names nil" REG-AOT-REFUSE THEN
            src u 6 node REG-AOT-ITEM drop
         THEN THEN THEN THEN THEN THEN THEN
      loop
   loop ;

\ Owner-relative checks run after all node kinds and decreasing schema edges
\ have validated. A memo per (owner, strictness, node) prevents shared subgraphs
\ from expanding exponentially; its two-cell rows are private scratch only.
PTR-VARIABLE REG-AOT-MEMO
variable REG-AOT-MEMO-U

\ Cell fam of REG-AOT-FAMW holds 1 + the walked width of arity-0 family fam, 0
\ until that walk completes: computed widths only, never a persisted slot
\ count. A parametric family is walked per frame. A check maps it with the
\ memo; REG-AOT-GRAPH-WIDTH maps its own unless a check holds one.
PTR-VARIABLE REG-AOT-FAMW
variable REG-AOT-FAMW-U

\ One cell per family id the payload can name: up to row 0's base plus count,
\ or the live count for an empty payload, which REG-AOT-ITEM reads as the live
\ registry. A registry without a family maps none.
: REG-AOT-FAMW-START ( ptr u8 n -- ) {: src:ptr u:n :}
   u 0= IF 0 REG-AOT-COUNT ELSE src 0 REG-AOT-ROW@ drop + THEN
   cells {: bytes:n :}
   bytes 0 > IF bytes ARENA-ALLOC REG-AOT-FAMW ! THEN
   bytes REG-AOT-FAMW-U ! ;

: REG-AOT-FAMW-DONE ( -- )
   REG-AOT-FAMW @ REG-AOT-FAMW-U @ ASIG-RELEASE
   NULL-PTR REG-AOT-FAMW ! 0 REG-AOT-FAMW-U ! ;

: REG-AOT-MEMO-DONE ( -- )
   REG-AOT-MEMO @ REG-AOT-MEMO-U @ ASIG-RELEASE
   NULL-PTR REG-AOT-MEMO ! 0 REG-AOT-MEMO-U !
   REG-AOT-FAMW-DONE ;

\ One row per schema node id up to row 6's base plus count. The size is never
\ zero, which ARENA-ALLOC dies on: REG-AOT-CHECK's table check refuses a row 6
\ whose base and end both differ from the live SCH-N, and SCH-N is at least 1
\ (node 0 is the nil sentinel).
: REG-AOT-MEMO-START ( ptr u8 n -- ) {: src:ptr u:n :}
   src 6 REG-AOT-ROW@ drop + 2 cells * {: bytes:n :}
   bytes ARENA-ALLOC REG-AOT-MEMO ! bytes REG-AOT-MEMO-U !
   src u REG-AOT-FAMW-START ;

: REG-AOT-MEMO-SLOT ( n -- ptr n )
   2 cells * REG-AOT-MEMO @ + CELL-VIEW ;

: REG-AOT-FAM-KIND ( ptr n -- n ) {: row:ptr :}
   row TF.KIND @ {: kind:n :}
   kind TK-EVIDENCE = IF PK-EVIDENCE EXIT THEN
   kind TK-PRODUCT = kind TK-SUM = or kind TK-ENUM = or IF PK-LAYOUT EXIT THEN
   PK-CELL ;

: REG-AOT-FAM-WIDTH ( ptr n -- n ) {: row:ptr :}
   row TF.LAYOUT @ {: policy:n :}
   policy TL-BOXED = policy TL-NICHE = or IF 1 EXIT THEN
   row TF.KIND @ {: kind:n :}
   kind TK-PRODUCT = IF row TF.SLOTS @ EXIT THEN
   kind TK-SUM = kind TK-ENUM = or IF row TF.SLOTS @ 1+ EXIT THEN
   1 ;

\ Leave room for the persisted width+1 encoding. Bounds precede addition.
: REG-AOT-GRAPH-WIDTH+ ( n n -- n ) {: left:n right:n :}
   left 0 < right 0 < or IF ASIG-GRAPH-DIE THEN
   right $7FFFFFFFFFFFFFFE left - > IF ASIG-GRAPH-DIE THEN
   left right + ;

\ Portable counterpart of TFW-NODE / TFW-FAM over the seeded rows. Members are
\ validated before field schemas (REG-AOT-OWNER-CHECK), so this walk reaches
\ unvalidated roots: it refuses a parameter beyond its owner's arity and an
\ application of a family not strictly older than its owner, which bounds the
\ recursion. REG-AOT-SHAPE-CHECK pins an application's argument count to its
\ family's arity, so a nested frame holds exactly the applied family's arity.
defer REG-AOT-FAM-W ( ptr u8 n n n n -- n )   \ ( src u fam base top -- width ) forward ref for a nested application

: REG-AOT-NODE-W ( ptr u8 n n n n n -- n )
   {: src:ptr u:n owner:n node:n base:n top:n :}
   src u 6 node REG-AOT-ITEM {: row:ptr :}
   row @ {: tag:n :}
   row CELL + @ {: a:n :}
   tag SCH-PARAM = IF
      a  src u 0 owner REG-AOT-ITEM TF.ARITY @  >= IF
         s" tfam: a seeded schema parameter exceeds its owner arity" REG-AOT-REFUSE THEN
      base a + TFW@ EXIT
   THEN
   tag SCH-APP = IF
      a owner >= IF
         s" tfam: a seeded schema has a recursive or forward family" REG-AOT-REFUSE THEN
      row 2 cells + @ {: b:n :}
      row 3 cells + @ {: c:n :}
      c 0 ?do                                    \ an argument's own frames live above top+c
         src u owner  src u 7 b i + REG-AOT-ITEM @  base top c + RECURSE  top i + TFW!
      loop
      src u a top top c + REG-AOT-FAM-W EXIT
   THEN
   1 ;

: REG-AOT-FIELDS-W ( ptr u8 n n ptr n n n n -- n )
   {: src:ptr u:n owner:n family:ptr variant:n base:n top:n :}
   0 family TF.FLD-COUNT @ 0 ?do
      src u 3 family TF.FLD-START @ i + REG-AOT-ITEM {: field:ptr :}
      field PF.VAR @ variant = IF
         src u owner  src u 7 field PF.SCH @ REG-AOT-ITEM @  base top REG-AOT-NODE-W
         REG-AOT-GRAPH-WIDTH+
      THEN
   loop ;

: REG-AOT-FAM-WALK ( ptr u8 n n ptr n n n -- n )
   {: src:ptr u:n fam:n family:ptr base:n top:n :}
   family TF.LAYOUT @ {: policy:n :}
   policy TL-BOXED = policy TL-NICHE = or IF 1 EXIT THEN
   family TF.KIND @ {: kind:n :}
   kind TK-PRODUCT = IF
      src u fam family PF-NO-VARIANT base top REG-AOT-FIELDS-W EXIT
   THEN
   kind TK-SUM = kind TK-ENUM = or IF
      0 family TF.VAR-COUNT @ 0 ?do
         family TF.VAR-START @ i + {: vid:n :}
         src u 2 vid REG-AOT-ITEM {: variant:ptr :}
         family TF.FLD-COUNT @ 0 > IF
            src u fam family vid base top REG-AOT-FIELDS-W
         ELSE
            0 variant SV.SCH-COUNT @ 0 ?do
               src u fam  src u 7 variant SV.SCH-START @ i + REG-AOT-ITEM @  base top REG-AOT-NODE-W
               REG-AOT-GRAPH-WIDTH+
            loop
         THEN
         max
      loop
      1 REG-AOT-GRAPH-WIDTH+ EXIT
   THEN
   1 ;

: REG-AOT-FAM-W-IMPL ( ptr u8 n n n n -- n ) {: src:ptr u:n fam:n base:n top:n :}
   src u 0 fam REG-AOT-ITEM {: family:ptr :}
   family TF.ARITY @ 0 >  fam cells REG-AOT-FAMW-U @ >= or IF
      src u fam family base top REG-AOT-FAM-WALK EXIT THEN
   fam cells REG-AOT-FAMW @ + CELL-VIEW {: memo:ptr :}
   memo @ 0 > IF memo @ 1 - EXIT THEN
   src u fam family base top REG-AOT-FAM-WALK
   dup 1 + memo ! ;
: REG-AOT-FAM-W-INSTALL ( -- ) [: REG-AOT-FAM-W-IMPL ;] is REG-AOT-FAM-W ;
REG-AOT-FAM-W-INSTALL

: REG-AOT-NODE-WIDTH ( ptr u8 n n n -- n ) {: src:ptr u:n owner:n node:n :}   \ declared width: each parameter one cell
   src u 0 owner REG-AOT-ITEM TF.ARITY @ {: ar:n :}
   ar 0 ?do 1 i TFW! loop
   src u owner node 0 ar REG-AOT-NODE-W ;

: REG-AOT-GRAPH-WIDTH ( ptr u8 ptr u8 ptr u8 ptr u8 n -- n )
   {: term:ptr graph:ptr widths:ptr src:ptr u:n :}
   src u 0 term EN.H @ REG-AOT-ITEM {: family:ptr :}
   \ The seeded slots are validated against the declared walk
   \ (REG-AOT-FIELD-SCHEMA, REG-AOT-OWNER-WIDTH, REG-AOT-FAMILY-MEMBERS), which is
   \ this walk at an all-ones frame, so with every argument one cell
   \ REG-AOT-FAM-WIDTH is the answer, without rescanning every field for each
   \ hidden slot of a wide value.
   RES-TRUE term EN.C @ 0 ?do
      graph term EN.D @ + i cells + CELL-VIEW @ widths + CELL-VIEW @ 1 = and
   loop IF family REG-AOT-FAM-WIDTH EXIT THEN
   term EN.C @ 0 ?do                             \ the graph's memoized argument widths
      graph term EN.D @ + i cells + CELL-VIEW @ widths + CELL-VIEW @  i TFW!
   loop
   src u term EN.H @ 0 term EN.C @
   REG-AOT-FAMW-U @ 0 > IF REG-AOT-FAM-W EXIT THEN
   src u REG-AOT-FAMW-START
   [: REG-AOT-FAM-W ;] [: REG-AOT-FAMW-DONE ;] finally ;

: REG-AOT-SAME-PKG? ( ptr u8 n ptr n ptr n -- bool )
   {: src:ptr u:n left:ptr right:ptr :}
   src u 5 left TF.PKG-OFF @ left TF.PKG-U @ REG-AOT-VIEW left TF.PKG-U @
   src u 5 right TF.PKG-OFF @ right TF.PKG-U @ REG-AOT-VIEW right TF.PKG-U @ CORE-STR= ;

: REG-AOT-MEMO-RESULT ( n ptr n n -- n ) {: key:n memo:ptr kind:n :}
   key memo ! kind memo CELL + ! kind ;

: REG-AOT-FAM-NONVALUE? ( ptr u8 n n -- bool ) {: src:ptr u:n fam:n :}
   src u 0 fam REG-AOT-ITEM {: row:ptr :}
   row TF.ARITY @ 0 ?do
      src u 1 row TF.PK-START @ i + REG-AOT-ITEM @ PF-NONVALUE? IF
         UNLOOP RES-TRUE EXIT THEN
   loop RES-FALSE ;

: REG-AOT-OWNED-NODE ( ptr u8 n n n bool -- n )
   {: src:ptr u:n owner:n node:n strict:bool :}
   owner 1+ 2 * strict IF 1+ THEN {: key:n :}
   node REG-AOT-MEMO-SLOT {: memo:ptr :}
   memo @ key = IF memo CELL + @ EXIT THEN
   src u 0 owner REG-AOT-ITEM {: owning:ptr :}
   src u 6 node REG-AOT-ITEM {: row:ptr :}
   row @ {: tag:n :} row CELL + @ {: a:n :}
   row 2 cells + @ {: b:n :} row 3 cells + @ {: c:n :}
   tag SCH-PARAM = IF
      a owning TF.ARITY @ >= IF
         s" tfam: a seeded schema parameter exceeds its owner arity" REG-AOT-REFUSE THEN
      src u 1 owning TF.PK-START @ a + REG-AOT-ITEM @ {: kind:n :}
      key memo kind REG-AOT-MEMO-RESULT EXIT
   THEN
   tag SCH-CON = IF
      a REG-AOT-CON-VALID? 0= IF
         s" tfam: captured schema constructor has process-local identity" REG-AOT-REFUSE THEN
   THEN
   tag SCH-PTR = IF
      src u owner a strict TWALK-DEEPER RECURSE TWALK-SHALLOWER PF-NONVALUE? IF
         s" tfam: a seeded pointer names a non-value parameter" REG-AOT-REFUSE THEN
      src u 6 a REG-AOT-ITEM {: child:ptr :}
      child @ SCH-APP = IF
         src u child CELL + @ REG-AOT-FAM-NONVALUE? IF
            s" tfam: a seeded pointer names a borrowed family" REG-AOT-REFUSE THEN
      THEN
   THEN
   tag SCH-ROW = IF
      b 0 ?do
         src u 7 a i + REG-AOT-ITEM @ {: child:n :}
         src u owner child strict TWALK-DEEPER RECURSE TWALK-SHALLOWER PF-NONVALUE? IF
            s" tfam: a seeded effect uses a non-value parameter" REG-AOT-REFUSE THEN
      loop
   THEN
   tag SCH-QUOT = IF
      c 0 ?do
         src u 7 b i + REG-AOT-ITEM @ {: child:n :}
         src u owner child strict TWALK-DEEPER RECURSE TWALK-SHALLOWER drop
      loop
   THEN
   tag SCH-APP = IF
      \ Ordinary declarations resolve only already-declared families, including
      \ pointer-wrapped references: the dependency graph stays acyclic.
      a owner >= IF
         s" tfam: a seeded schema has a recursive or forward family" REG-AOT-REFUSE THEN
      src u 0 a REG-AOT-ITEM {: applied:ptr :}
      applied TF.VIS @ CHECKER-PACKAGE-PUBLIC <>
      src u owning applied REG-AOT-SAME-PKG? 0= and IF
         s" tfam: a seeded schema names an inaccessible family" REG-AOT-REFUSE THEN
      c 0 ?do
         src u 7 b i + REG-AOT-ITEM @ {: child:n :}
         src u owner child strict TWALK-DEEPER RECURSE TWALK-SHALLOWER {: got:n :}
         strict IF
            src u 1 applied TF.PK-START @ i + REG-AOT-ITEM @ {: want:n :}
            got want PF-KIND-OK? 0= IF
               s" tfam: a seeded schema argument has the wrong kind" REG-AOT-REFUSE THEN
         THEN
      loop
      key memo applied REG-AOT-FAM-KIND REG-AOT-MEMO-RESULT EXIT
   THEN
   key memo PK-CELL REG-AOT-MEMO-RESULT ;

: REG-AOT-OWNED-ROOT ( ptr u8 n n n bool -- n )
   {: src:ptr u:n owner:n root:n strict:bool :}
   src u 7 root REG-AOT-ITEM @ {: node:n :}
   src u 6 node REG-AOT-ITEM @ {: tag:n :}
   tag SCH-ROW = IF
      s" tfam: a seeded payload root names an effect row" REG-AOT-REFUSE THEN
   src u owner node strict REG-AOT-OWNED-NODE {: kind:n :}
   strict tag SCH-PARAM = and kind PK-CELL <> and kind PK-TYPE <> and IF
      s" tfam: a seeded field parameter is not cell-kinded" REG-AOT-REFUSE THEN
   strict kind PF-NONVALUE? and IF
      s" tfam: a seeded field uses a non-value parameter as a value" REG-AOT-REFUSE THEN
   strict tag SCH-PARAM = and kind PK-TYPE = and IF
      src u 6 node REG-AOT-ITEM CELL + @ 1 = owner C2-INIT-FAM @ = and 0= IF
         s" tfam: a seeded field directly owns a type parameter" REG-AOT-REFUSE THEN
   THEN
   src u owner node REG-AOT-NODE-WIDTH ;

: REG-AOT-FIELD-SCHEMA ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 3 id REG-AOT-ITEM {: row:ptr :}
   src u 0 row PF.FAM @ REG-AOT-ITEM {: family:ptr :}
   src u row PF.FAM @ row PF.SCH @ RES-TRUE REG-AOT-OWNED-ROOT
   row PF.CELLS @ <> IF
      s" tfam: a seeded field width disagrees with its schema" REG-AOT-REFUSE THEN
   family TF.LAYOUT @ TL-PACKED-TAG > IF
      s" tfam: a seeded field uses an unsupported layout" REG-AOT-REFUSE THEN
   row PF.SLOT @ $7FFFFFFFFFFFFFFF CELL / >
   row PF.CELLS @ $7FFFFFFFFFFFFFFF CELL / > or IF
      s" tfam: a seeded field byte width overflows" REG-AOT-REFUSE THEN
   row PF.SLOT @ cells row PF.BYTE-OFF @ <>
   row PF.CELLS @ cells row PF.BYTES @ <> or
   row PF.ALIGN @ CELL <> or IF
      s" tfam: a seeded field bytes disagree with its cell layout" REG-AOT-REFUSE THEN ;

\ Field insertion already forbids overlaps and duplicate names within one
\ owner. Check the same bounded family slice before admitting imported rows.
: REG-AOT-FIELD-PEERS ( ptr u8 n n -- ) {: src:ptr u:n id:n :}
   src u 3 id REG-AOT-ITEM {: row:ptr :}
   src u 0 row PF.FAM @ REG-AOT-ITEM TF.FLD-START @ {: first:n :}
   id first ?do
      src u 3 i REG-AOT-ITEM {: other:ptr :}
      other PF.VAR @ row PF.VAR @ = IF
         src u 5 row PF.NAME-OFF @ row PF.NAME-U @ REG-AOT-VIEW row PF.NAME-U @
         src u 5 other PF.NAME-OFF @ other PF.NAME-U @ REG-AOT-VIEW other PF.NAME-U @ CORE-STR= IF
            s" tfam: a seeded field repeats an owner-local name" REG-AOT-REFUSE THEN
         row PF.SLOT @ row PF.CELLS @ other PF.SLOT @ other PF.CELLS @ PF-RANGE-OVERLAP?
         row PF.BYTE-OFF @ row PF.BYTES @ other PF.BYTE-OFF @ other PF.BYTES @ PF-RANGE-OVERLAP? or IF
            s" tfam: a seeded field overlaps another field" REG-AOT-REFUSE THEN
      THEN
   loop ;

: REG-AOT-OWNER-WIDTH ( ptr u8 n ptr n n n -- n )
   {: src:ptr u:n family:ptr variant:n limit:n :}
   0 0 family TF.FLD-COUNT @ 0 ?do
      src u 3 family TF.FLD-START @ i + REG-AOT-ITEM {: field:ptr :}
      field PF.VAR @ variant = IF
         {: total:n highest:n :}
         field PF.CELLS @ limit total - > IF
            s" tfam: seeded fields exceed their owner's payload width" REG-AOT-REFUSE THEN
         field PF.SLOT @ limit > IF
            s" tfam: a seeded field starts outside its variant" REG-AOT-REFUSE THEN
         field PF.CELLS @ limit field PF.SLOT @ - > IF
            s" tfam: a seeded field extends outside its variant" REG-AOT-REFUSE THEN
         total field PF.CELLS @ +
         highest field PF.SLOT @ field PF.CELLS @ + max
      THEN
   loop
   2dup <> IF
      s" tfam: seeded fields leave a gap in their owner layout" REG-AOT-REFUSE THEN drop ;

: REG-AOT-FAMILY-MEMBERS ( ptr u8 n n -- ) {: src:ptr u:n fam:n :}
   src u 0 fam REG-AOT-ITEM {: row:ptr :}
   row TF.FLD-COUNT @ 0 ?do
      src u 3 row TF.FLD-START @ i + REG-AOT-ITEM PF.FAM @ fam <> IF
         s" tfam: a seeded family includes another family's field" REG-AOT-REFUSE THEN
   loop
   row TF.KIND @ TK-PRODUCT = IF
      src u row PF-NO-VARIANT row TF.SLOTS @ REG-AOT-OWNER-WIDTH
      row TF.SLOTS @ <> IF
         s" tfam: a seeded product width differs from its fields" REG-AOT-REFUSE THEN
   THEN
   0 row TF.VAR-COUNT @ 0 ?do
      row TF.VAR-START @ i + {: vid:n :}
      src u 2 vid REG-AOT-ITEM {: variant:ptr :}
      variant SV.FAM @ fam <> IF
         s" tfam: a seeded family includes another family's variant" REG-AOT-REFUSE THEN
      row TF.KIND @ TK-SUM = row TF.FLD-COUNT @ 0 > and
      variant SV.SCH-COUNT @ 0 > and IF
         s" tfam: a seeded sum mixes positional and named payloads" REG-AOT-REFUSE THEN
      0
      variant SV.SCH-COUNT @ 0 ?do
         src u fam variant SV.SCH-START @ i + RES-FALSE REG-AOT-OWNED-ROOT {: cellsn:n :}
         dup row TF.SLOTS @ swap - cellsn < IF
            s" tfam: a seeded variant schema exceeds its payload width" REG-AOT-REFUSE THEN
         cellsn +
      loop
      row TF.KIND @ TK-SUM = row TF.FLD-COUNT @ 0 > and IF
         drop
         src u row vid row TF.SLOTS @ REG-AOT-OWNER-WIDTH
      ELSE
         dup variant SV.PAYCELLS @ <> IF
            s" tfam: a seeded variant width disagrees with its schema" REG-AOT-REFUSE THEN
      THEN
      max
   loop
   row TF.KIND @ TK-SUM = row TF.KIND @ TK-ENUM = or IF
      row TF.SLOTS @ <> IF
         s" tfam: a seeded family's width differs from its maximum payload" REG-AOT-REFUSE THEN
   ELSE drop THEN ;

: REG-AOT-OWNER-CHECK ( ptr u8 n -- ) {: src:ptr u:n :}
   TWALK-RESET
   src 0 REG-AOT-ROW@ drop {: first:n count:n :}
   count 0 ?do src u first i + REG-AOT-FAMILY-MEMBERS loop
   src 3 REG-AOT-ROW@ drop {: field-first:n field-count:n :}
   field-count 0 ?do
      src u field-first i + REG-AOT-FIELD-SCHEMA
      src u field-first i + REG-AOT-FIELD-PEERS
   loop ;

: REG-AOT-CHECK ( ptr u8 n -- )
   dup 0= IF 2drop EXIT THEN
   2dup REG-AOT-SHAPE-CHECK
   2dup REG-AOT-MEMO-START
   [: 2dup REG-AOT-OWNER-CHECK ;] catch
   REG-AOT-MEMO-DONE
   dup 0 <> IF throw THEN drop 2drop ;

\ The refusal stays fatal (76) while the private REG-AOT-CHECK can be caught to
\ prove that a late refusal leaves every published store untouched.
: REG-AOT-VALIDATE ( ptr u8 n -- )
   0 REG-AOT-ERROR-U !
   [: 2dup REG-AOT-CHECK ;] catch
   dup 0 <> IF
      REG-AOT-ERROR-U @ 0= IF throw THEN
      drop 2drop REG-AOT-ERROR-A @ REG-AOT-ERROR-U @ 76 die
   THEN drop 2drop ;

: REG-AOT-PREPARE ( ptr u8 n ptr u8 n -- n )
   {: src:ptr u:n prior:ptr prioru:n :}
   prior REG-AOT-PROJECTED ! prioru REG-AOT-PROJECTED-U !
   src u [: 2dup REG-AOT-CHECK ;] catch {: rc:n :}
   2drop
   NULL-PTR REG-AOT-PROJECTED ! 0 REG-AOT-PROJECTED-U !
   rc 0 <> IF rc throw THEN
   REG-AOT-STATE @ ;

: REG-AOT-APPEND ( ptr u8 n -- ) {: src:ptr u:n :}
   u 0= IF EXIT THEN
   0
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ drop nip or
   loop
   0= IF EXIT THEN
   \ A failed allocation may leave capacity, but no visible registry member.
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ drop {: base:n cnt:n :}
      i REG-AOT-COUNT base = cnt 0 > and IF
         i base cnt + REG-AOT-ROOM THEN
   loop
   REG-AOT-HDR REG-AOT-CUR !
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ {: base:n cnt:n bytes:n :}
      i REG-AOT-COUNT base = IF
         cnt 0 > IF
            src REG-AOT-CUR @ + base i REG-AOT-WIDTH * +
            i REG-AOT-BASE-PTR base i REG-AOT-WIDTH * +
            cnt i REG-AOT-WIDTH * ARENA-COPY
         THEN
         i base cnt + REG-AOT-COUNT!
         i base cnt REG-AOT-SCRUB
      THEN
      REG-AOT-CUR @ bytes + REG-AOT-CUR !
   loop
   TFX-SNAP-RESET
   VNX-SNAP-RESET
   FVX-SNAP-RESET
   TFM-EPOCH+
   0 SVX-GEN !   SVX-STAMP ;

: REG-AOT-TAKE ( ptr u8 n n -- ) {: src:ptr u:n state:n :}
   state 1 = IF src u REG-AOT-APPEND THEN ;

: REG-AOT-INSTALL ( ptr u8 n -- ) {: src:ptr u:n :}
   u 0= IF EXIT THEN
   src u REG-AOT-CHECK
   src u REG-AOT-STATE @ REG-AOT-TAKE ;

\ REG-AOT-INSTALL throws before publication on invalid input; the artifact-load
\ boundary keeps the named process refusal (76).
: REG-AOT-LOAD ( ptr u8 n -- )
   0 REG-AOT-ERROR-U !
   [: 2dup REG-AOT-INSTALL ;] catch
   dup 0 <> IF
      REG-AOT-ERROR-U @ 0= IF throw THEN
      drop 2drop REG-AOT-ERROR-A @ REG-AOT-ERROR-U @ 76 die
   THEN drop 2drop ;

: REG-EXT-AOT-INSTALL ( -- )
   [: REG-AOT-MARK ;] is REG-EXT-AOT-MARK-XT
   [: REG-AOT-CLOSE ;] is REG-EXT-AOT-CLOSE-XT
   [: REG-AOT-SAVE ;] is REG-EXT-AOT-SAVE-XT
   [: REG-AOT-LOAD ;] is REG-EXT-AOT-LOAD-XT
   [: REG-AOT-VALIDATE ;] is REG-EXT-AOT-VALIDATE-XT
   [: REG-AOT-PARAM? ;] is REG-EXT-AOT-PARAM-XT
   [: REG-AOT-SCOPE? ;] is REG-EXT-AOT-SCOPE-XT
   [: REG-AOT-REGION? ;] is REG-EXT-AOT-REGION-XT
   [: REG-AOT-GRAPH-WIDTH ;] is REG-EXT-AOT-WIDTH-XT
   [: TFAM-NAME$ ;] is REG-EXT-AOT-FAMILY-NAME-XT ;
REG-EXT-AOT-INSTALL

\ Built-in parametric cell families: the parser's parametric type constructors.
\ Each is public and global (empty package), so a bare `span<...>` resolves
\ through TFAM-RESOLVE from any scope. Registration runs at prefix load in every
\ context (preverify parent and runtime child), so both see identical families.

public

: TFAM-REG-CELL ( ptr u8 n n -- )
   {: na:ptr nu:n ar:n :}
   s" " CHECKER-PACKAGE-PUBLIC na nu ar TK-CELL TFAM-DECL drop ;

\ `ptr` is dual: `ptr<space,elem>` is a parametric pointer (T-PARAM, resolved
\ here), while `ptr elem` (no `<`) stays the MK-PTR plain-pointer special case in
\ checker.f SIG-TYPE. Registered arity 2 matches every `ptr<...>` in the tree.
s" ptr"        2 TFAM-REG-CELL
s" span"       3 TFAM-REG-CELL
s" vspan"      3 TFAM-REG-CELL   \ global span with a proven 16-byte base alignment
s" matrix"     4 TFAM-REG-CELL
s" gridctx"    3 TFAM-REG-CELL
s" fanctx"     3 TFAM-REG-CELL
s" idxctx"     4 TFAM-REG-CELL
s" uniqidxctx" 4 TFAM-REG-CELL
s" coopctx"    3 TFAM-REG-CELL
s" rowctx"     3 TFAM-REG-CELL
s" tile"       3 TFAM-REG-CELL
s" vtile"      3 TFAM-REG-CELL   \ vec4 lane tile, distinct from scalar tile<>
s" acc"        3 TFAM-REG-CELL
s" mmctx"      3 TFAM-REG-CELL
\ Pipelined register-blocked GEMM tiles: <t elem, b block, l layout-atom, w mask,
\ p parity var> on the staged families; <t, b, g micro-geometry atom, w> on the
\ micro-tile accumulator.
s" mmstage"    5 TFAM-REG-CELL   \ READY current-parity staged As+Bs tile-pair
s" mmaslice"   5 TFAM-REG-CELL   \ strided A slice of a stage (scalar loads only)
s" mmbslice"   5 TFAM-REG-CELL   \ contiguous 16B-proven B slice (v4 loads legal)
s" mmafrag"    5 TFAM-REG-CELL   \ A operand fragment (4 regs, one k column)
s" mmbfrag"    5 TFAM-REG-CELL   \ B operand fragment (4 regs, one vec4 row)
s" mmracc"     4 TFAM-REG-CELL   \ register-blocked micro-tile accumulator
\ cp.async pipeline-slot typestate: a staged-buffer slot threads pending<p> ->
\ committed<p> -> ready<p> (p = symbolic buffer parity). Distinct families make
\ read-before-wait, missing-commit, double-wait and parity mismatch type errors.
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
\ Extent roles (docs/design.md "Type substrate"). `extprod<free,inner>` is an
\ ordered arity-2 product: ix<extprod<extb,extt>> types a folded (B,T) row and
\ ordinary unification rejects a swapped or mismatched factor. A value of
\ redx<e> is an axis marked for summation; checker.f EXT-REDX-BAD-ARG? at
\ SIG-END-PARAM rejects redx over a free factor. A value of ix<e> is one cell
\ indexing extent e. The three register together in the core: the retype
\ ix<e> -> redx<e> (src/lib/type/extent-role.f `>RED`) introduces redx, and
\ checker.f CAST-OWNER? authorizes an introduction only from the destination
\ family's declaring package.
s" ix"         1 TFAM-REG-CELL
s" extprod"    2 TFAM-REG-CELL
s" redx"       1 TFAM-REG-CELL

\ The tile/uniform family ids go into the checker's barrier-uniformity cells
\ (declared in checker.f). A collective typed ( tile<..> -- uniform<..> ) emits
\ bar.sync and is sound only under block-uniform control (checker.f
\ PTX-BARRIER-ROWS?/BARRIER-CUR?). Runs in every load context, so the parent
\ verifier and runtime child agree.

private

: PTX-FAM-ID ( ptr u8 n -- n )   \ resolve a GLOBAL family name to its id (0 if none)
   {: na:ptr nu:n :}
   s" " na nu TFAM-RESOLVE IF ELSE drop 0 THEN ;
s" tile"    PTX-FAM-ID PTX-TILE-FAM !
s" uniform" PTX-FAM-ID PTX-UNIFORM-FAM !
\ The WAIT step ( cpp-committed<p> -- cpp-ready<p> ) retires the copy group and
\ fences it with bar.sync: a WAIT under divergent control is not block-uniform
\ and is rejected (checker.f PTX-CPWAIT-ROWS?/BARRIER-CUR?).
s" cpp-committed" PTX-FAM-ID PTX-CPCOMMITTED-FAM !
s" cpp-ready"     PTX-FAM-ID PTX-CPREADY-FAM !
\ Extent-role ids for the checker's role cells (declared in checker.f):
\ SIG-END-PARAM reads EXT-REDX-FAM to gate forming redx<..>, and EXT-PROD-FAM
\ to reject contracting a whole product.
s" extprod" PTX-FAM-ID EXT-PROD-FAM !
s" redx"    PTX-FAM-ID EXT-REDX-FAM !

\ Internal VREC field constructor: arity 3, PRIVATE in reserved package "@" (not a
\ spellable user package) so it never resolves from user signatures, while every
\ field<...> term still carries this reserved family-id for identity comparison.
s" @" CHECKER-PACKAGE-PRIVATE s" field" 3 TK-CELL TFAM-DECL FIELD-FAM !
\ Region provenance for an exclusive initialized-field loan. The second
\ argument is the existing committed field descriptor, and the first is the
\ parent's allocation region; neither the constructor nor its spelling is
\ available to a user signature.
s" @" CHECKER-PACKAGE-PRIVATE s" loan-field" 2 TK-CELL TFAM-DECL LOAN-FIELD-FAM !
LOAN-FIELD-FAM @ 0 PK-REGION TFAM-PK!
LOAN-FIELD-FAM @ 1 PK-TYPE TFAM-PK!

\ C2's shared and exclusive views have the same address-and-bound stack layout.
\ They are family records rather than PRODUCT declarations: no constructor,
\ destructor, or address accessor is published. The fields describe their ABI
\ for width-aware transport and image replay; checked code cannot project them.
private
variable READ-FAM
variable MUT-FAM
variable INIT-FAM
variable RECORDS-FAM
variable STOW-FAM
variable HEAD-FAM
variable VIEW-TX
variable VIEW-FIELD-BASE

: REGISTER-VIEW-FIELDS ( n -- ) {: fam:n :}
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   TYPE-FIELD-OWNER:OPEN VIEW-TX !
   VIEW-TX @ fam TYPE-FIELD:NO-VARIANT s" base"
      CC-U8 SCHEMA-CON SCHEMA-PTR SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ fam TYPE-FIELD:NO-VARIANT s" bound"
      CC-LEN SCHEMA-CON SCHEMA-ROOT+
      1 1 CELL CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ TYPE-FIELD-OWNER:COMMIT
   fam VIEW-FIELD-BASE @ 2 TFAM-FLD-RANGE!
   fam 2 TFAM-SLOTS!
   VIEW-TX @ TYPE-FIELD-OWNER:FINALIZE ;

: REGISTER-READ-VIEW ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" read-view" 3 TK-PRODUCT TFAM-DECL READ-FAM !
   READ-FAM @ C2-READ-FAM !
   READ-FAM @ 0 PK-SCOPE TFAM-PK!
   READ-FAM @ 1 PK-SCOPE TFAM-PK!
   READ-FAM @ 2 PK-TYPE TFAM-PK!
   READ-FAM @ REGISTER-VIEW-FIELDS ;

: REGISTER-MUT-VIEW ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" mut-view" 4 TK-PRODUCT TFAM-DECL MUT-FAM !
   MUT-FAM @ C2-MUT-FAM !
   MUT-FAM @ 0 PK-SCOPE TFAM-PK!
   MUT-FAM @ 1 PK-SCOPE TFAM-PK!
   MUT-FAM @ 2 PK-REGION TFAM-PK!
   MUT-FAM @ 3 PK-TYPE TFAM-PK!
   MUT-FAM @ REGISTER-VIEW-FIELDS ;

\ A reserved initialized element is a nominal product with one transparent
\ payload schema. The field is metadata only: no make/unmake, derive, field
\ accessor, or raw address operation is generated for this family.
: REGISTER-INIT ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" init" 2 TK-PRODUCT TFAM-DECL INIT-FAM !
   INIT-FAM @ C2-INIT-FAM !
   INIT-FAM @ 0 PK-SCOPE TFAM-PK!
   INIT-FAM @ 1 PK-TYPE TFAM-PK!
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   TYPE-FIELD-OWNER:OPEN VIEW-TX !
   VIEW-TX @ INIT-FAM @ TYPE-FIELD:NO-VARIANT s" value"
      1 SCHEMA-PARAM SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ TYPE-FIELD-OWNER:COMMIT
   INIT-FAM @ VIEW-FIELD-BASE @ 1 TFAM-FLD-RANGE!
   INIT-FAM @ 1 TFAM-SLOTS!
   VIEW-TX @ TYPE-FIELD-OWNER:FINALIZE ;


\ An arena owner's head (src/core/dynamic-storage.f): one cell holding the
\ newest chunk's base. No constructor, destructor or accessor exists, and the
\ checker refuses to fetch or store it whole, or any layout holding it
\ (checker.f IN-PLACE?), and refuses any cast that reaches it outside
\ DYNAMIC-STORAGE (checker.f CAST-REACHES-HEAD?), so no typed access and no
\ other cast copies or forges a head; address arithmetic still reaches one
\ (docs/forth.md "Raw storage never holds an address"). The field records its
\ ABI only.
: REGISTER-ARENA-HEAD ( -- )
   s" DYNAMIC-STORAGE" CHECKER-PACKAGE-PUBLIC s" arena-head" 0 TK-PRODUCT TFAM-DECL HEAD-FAM !
   HEAD-FAM @ ARENA-HEAD-FAM !
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   TYPE-FIELD-OWNER:OPEN VIEW-TX !
   VIEW-TX @ HEAD-FAM @ TYPE-FIELD:NO-VARIANT s" chunk"
      CC-U8 SCHEMA-CON SCHEMA-PTR SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ TYPE-FIELD-OWNER:COMMIT
   HEAD-FAM @ VIEW-FIELD-BASE @ 1 TFAM-FLD-RANGE!
   HEAD-FAM @ 1 TFAM-SLOTS!
   VIEW-TX @ TYPE-FIELD-OWNER:FINALIZE ;


: REGISTER-RECORDS ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" records" 4 TK-PRODUCT TFAM-DECL RECORDS-FAM !
   RECORDS-FAM @ C2-RECORDS-FAM !
   RECORDS-FAM @ 0 PK-SCOPE TFAM-PK!
   RECORDS-FAM @ 1 PK-SCOPE TFAM-PK!
   RECORDS-FAM @ 2 PK-REGION TFAM-PK!
   RECORDS-FAM @ 3 PK-TYPE TFAM-PK!
   RECORDS-FAM @ REGISTER-VIEW-FIELDS ;

\ The runtime's three machine descriptors form one private value. The family
\ identity, rather than its spelling or shape, authorizes a native transfer.
: REGISTER-STOW ( -- )
   s" C2-MEM" CHECKER-PACKAGE-PRIVATE s" stow-layout" 0 TK-PRODUCT TFAM-DECL STOW-FAM !
   STOW-FAM @ C2-STOW-FAM !
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   TYPE-FIELD-OWNER:OPEN VIEW-TX !
   VIEW-TX @ STOW-FAM @ TYPE-FIELD:NO-VARIANT s" width"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ STOW-FAM @ TYPE-FIELD:NO-VARIANT s" bytes"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      1 1 CELL CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ STOW-FAM @ TYPE-FIELD:NO-VARIANT s" alignment"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      2 1 2 CELL * CELL CELL PF-FLAGS-NONE TYPE-FIELD-OWNER:ADD VIEW-TX !
   VIEW-TX @ TYPE-FIELD-OWNER:COMMIT
   STOW-FAM @ VIEW-FIELD-BASE @ 3 TFAM-FLD-RANGE!
   STOW-FAM @ 3 TFAM-SLOTS!
   VIEW-TX @ TYPE-FIELD-OWNER:FINALIZE ;

REGISTER-READ-VIEW
REGISTER-MUT-VIEW
REGISTER-INIT
REGISTER-RECORDS
REGISTER-STOW
REGISTER-ARENA-HEAD
public

\ Signature-token resolution, the checker's TFAM-RESOLVE-XT target, over
\ TFAM-RESOLVE's package-scope rules. A qualified `PKG:tail` folds the qualifier
\ (package names are stored case-folded), requires a lowercase tail, and
\ resolves public rows plus the active package's own private rows. Unqualified
\ ambiguity (E-TFAM-AMBIG) becomes an unresolved token, so the signature is
\ rejected with a diagnostic instead of aborting the load.
$100 constant TFQ-CAP            \ folded qualifier bytes (CHECKER-PACKAGE-CAP)
create TFQ-BUF TFQ-CAP allot
variable TFQ-U
PTR-VARIABLE TFQ-TA   variable TFQ-TU  \ qualified tail token
variable TFQ-COLON

: TFQ-FOLD-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   u TFQ-CAP > IF s" tfam: qualifier too long" 76 die THEN
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ CORE-FOLD-C  TFQ-BUF i + c!
      i 1 +
   REPEAT drop
   u TFQ-U ! ;

\ TFQ-SPLIT? ( ptr u8 n -- bool ) : one non-edge ':' splits qualifier/tail
\ (engine FIND parity); edge or repeated colons never split (and never resolve),
\ and neither does a qualifier longer than TFQ-BUF, which no package name is.
: TFQ-SPLIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   -1 TFQ-COLON !
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ 58 = IF
         TFQ-COLON @ 0 < 0= IF RES-FALSE EXIT THEN   \ second ':' -> malformed
         i TFQ-COLON !
      THEN
      i 1 +
   REPEAT drop
   TFQ-COLON @ 0 < IF RES-FALSE EXIT THEN
   TFQ-COLON @ 0 = IF RES-FALSE EXIT THEN            \ leading ':'
   TFQ-COLON @ u 1 - = IF RES-FALSE EXIT THEN        \ trailing ':'
   TFQ-COLON @ TFQ-CAP > IF RES-FALSE EXIT THEN      \ names no package
   a TFQ-COLON @ TFQ-FOLD-COPY
   a TFQ-COLON @ + 1 + TFQ-TA !
   u TFQ-COLON @ - 1 - TFQ-TU !
   RES-TRUE ;

variable TFSR-HIDDEN
: TFAM-QUAL-RESOLVE ( ptr u8 n -- n bool ) {: pa:ptr pu:n :}
   TFQ-TA @ TFQ-TU @ TF-CANON? 0= IF 0 RES-FALSE EXIT THEN
   TFQ-BUF TFQ-U @ TFQ-TA @ TFQ-TU @ TFAM-FIND-IN 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-PUBLIC? IF id RES-TRUE EXIT THEN
   TFQ-BUF TFQ-U @ pa pu CORE-STR=CI IF id RES-TRUE EXIT THEN   \ own private rows
   -1 TFSR-HIDDEN !
   0 RES-FALSE ;

\ TFAM-RESOLVE may throw E-TFAM-AMBIG; a checked `catch` needs a stack-neutral
\ quotation that cannot read locals, so buffer the (pkg,name) args and the
\ (id,flag) result through cells and run the resolve as a `( -- )` quotation.
PTR-VARIABLE TFSR-PA   variable TFSR-PU   PTR-VARIABLE TFSR-NA   variable TFSR-NU
variable TFSR-ID   variable TFSR-FLAG
variable TFSR-AMBIG
: TFSR-RUN ( -- )
   TFSR-PA @ TFSR-PU @ TFSR-NA @ TFSR-NU @ TFAM-RESOLVE
   TFSR-FLAG !  TFSR-ID ! ;

public

: TFAM-SIG-RESOLVE ( ptr u8 n ptr u8 n -- n bool )
   {: pa:ptr pu:n na:ptr nu:n :}
   0 TFSR-AMBIG !  0 TFSR-HIDDEN !
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   na nu TFQ-SPLIT? IF pa pu TFAM-QUAL-RESOLVE EXIT THEN
   pa TFSR-PA !  pu TFSR-PU !  na TFSR-NA !  nu TFSR-NU !
   [: TFSR-RUN ;] catch {: rc:n :}
   rc 0= IF TFSR-ID @ TFSR-FLAG @ EXIT THEN   \ ( id flag ) from the resolver
   rc E-TFAM-AMBIG = IF -1 TFSR-AMBIG ! 0 RES-FALSE EXIT THEN
   rc throw ;

\ The signature parser asks this only after its normal family and concrete
\ lookups missed. An ambiguous public tail, a malformed qualifier or a
\ noncanonical family tail is not an absent declaration a quiet producer may
\ explain.
: TFAM-MAY-ARRIVE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   TFSR-AMBIG @ TFSR-HIDDEN @ or 0 <> IF RES-FALSE EXIT THEN
   a u TFQ-SPLIT? IF TFQ-TA @ TFQ-TU @ TF-CANON?
   ELSE a u TF-CANON? THEN
   IF a u CHECKER-TYPE-MARK-COVERS? ELSE RES-FALSE THEN ;

: TFAM-MAY-ARRIVE-INSTALL ( -- )
   [: TFAM-MAY-ARRIVE? ;] is TYPE-MAY-ARRIVE-XT ;
TFAM-MAY-ARRIVE-INSTALL

private

\ TFAM-CLAIMED? ( ptr u8 n -- bool ) : some scope reads this signature token as
\ a family (the checker's TFAM-CLAIMED?-XT target). The package that declares a
\ tail reads its own row first, private or public; a global row is read
\ everywhere, and another package's public row through `using` or the unique
\ fallback. So every row of an unqualified tail claims it, ambiguous or not. A
\ qualified `PKG:tail` claims PKG's row, which PKG reads even while private.
\ The checker refuses a DEFLINEAR or VALUE-RECORD name that is claimed: the
\ mirror of a family declaration refusing a declared type's name.
: TFAM-CLAIMED? ( ptr u8 n -- bool ) {: na:ptr nu:n :}
   na nu TF-HIDDEN? IF RES-FALSE EXIT THEN
   na nu TFQ-SPLIT? IF TFQ-BUF TFQ-U @ TFQ-TA @ TFQ-TU @ TFAM-FIND-IN nip EXIT THEN
   TFX-ENSURE
   na nu TFX-HASH TFX-BKT @ TFX-CUR !
   BEGIN TFX-CUR @ 0 <> WHILE
      TFX-CUR @ 1 - {: id:n :}
      na nu id TFAM-NAME-MATCH? IF RES-TRUE EXIT THEN
      id TF-REC@ TF.TAILNEXT @ TFX-CUR !
   REPEAT
   RES-FALSE ;

create UNIT-FAM-MARKS REG-AOT-N cells allot
create UNIT-FAM-STABLE REG-AOT-N cells allot
PTR-VARIABLE UNIT-FAM-AT
\ PF-TX-SERIAL where the slice starts. Tokens outlive their declaration (see
\ TFAM-RESET), so a slice records the serials it opened and an import advances
\ PF-TX-SERIAL by that delta, as the cold compile's OPENs do.
variable UNIT-FAM-SERIAL-MARK
TFAM-N @ constant UNIT-FAM-STABLE-N
$54464D44454C5433 constant UNIT-FAM-MAGIC
\ Magic, store count, the transaction serial delta, then base, count and
\ stable count per store.
24 REG-AOT-N 24 * + constant UNIT-FAM-HDR
PTR-VARIABLE UNIT-FAM-REFS
variable UNIT-FAM-REFS-CAP
PTR-VARIABLE UNIT-CON-REFS
variable UNIT-CON-REFS-CAP
PTR-VARIABLE UNIT-FAM-SEEN
variable UNIT-FAM-SEEN-CAP
variable UNIT-FAM-SEEN-EPOCH 0 UNIT-FAM-SEEN-EPOCH !
variable UNIT-FAM-REF-N
variable UNIT-CON-REF-N

: UNIT-FAM-REFS-RESERVE ( n -- ) {: need:n :}
   UNIT-FAM-REFS UNIT-FAM-REFS-CAP need 1 ARENA-ROWS-ENSURE ;
: UNIT-CON-REFS-RESERVE ( n -- ) {: need:n :}
   UNIT-CON-REFS UNIT-CON-REFS-CAP need 1 ARENA-ROWS-ENSURE ;
: UNIT-FAM-SEEN-RESERVE ( n -- ) {: need:n :}
   UNIT-FAM-SEEN-CAP @ {: old:n :}
   UNIT-FAM-SEEN UNIT-FAM-SEEN-CAP need 1 ARENA-ROWS-ENSURE
   UNIT-FAM-SEEN-CAP @ old > IF
      UNIT-FAM-SEEN @ old cells + CELL-VIEW
      0 UNIT-FAM-SEEN-CAP @ old - ARENA-CELLS-ZERO
   THEN ;
: UNIT-FAM-REFS-RELEASE ( -- )
   UNIT-FAM-REFS UNIT-FAM-REFS-CAP 1 ARENA-ROWS-RELEASE ;
: UNIT-CON-REFS-RELEASE ( -- )
   UNIT-CON-REFS UNIT-CON-REFS-CAP 1 ARENA-ROWS-RELEASE ;
: UNIT-FAM-SEEN-RELEASE ( -- )
   UNIT-FAM-SEEN UNIT-FAM-SEEN-CAP 1 ARENA-ROWS-RELEASE ;

\ Coverage of one FILE's string-pool delta, one cell per pool byte: capture and
\ import each claim the bytes the delta's row strings name and prove that they
\ tile the delta. Staging, released with the rest of the FILE scratch.
PTR-VARIABLE UNIT-FAM-COVER
variable UNIT-FAM-COVER-CAP
: UNIT-FAM-COVER-OPEN ( n -- ) {: n:n :}
   UNIT-FAM-COVER UNIT-FAM-COVER-CAP n 1 ARENA-ROWS-ENSURE
   UNIT-FAM-COVER @ CELL-VIEW 0 n ARENA-CELLS-ZERO ;
: UNIT-FAM-COVERED? ( n -- bool ) cells UNIT-FAM-COVER @ + @ 0 <> ;
: UNIT-FAM-COVER! ( n -- ) -1 swap cells UNIT-FAM-COVER @ + ! ;
: UNIT-FAM-COVER-FULL? ( n -- bool ) {: n:n :}
   n 0 ?do i UNIT-FAM-COVERED? 0= IF RES-FALSE unloop EXIT THEN loop
   RES-TRUE ;
: UNIT-FAM-COVER-RELEASE ( -- )
   UNIT-FAM-COVER UNIT-FAM-COVER-CAP 1 ARENA-ROWS-RELEASE ;
defer UNIT-FAM-SCRATCH-RESET ( -- )
: UNIT-FAM-EXPORT-RESET ( -- )
   UNIT-FAM-MARKS 0 REG-AOT-N ARENA-CELLS-ZERO
   NULL-PTR UNIT-FAM-AT ! 0 UNIT-FAM-REF-N ! 0 UNIT-CON-REF-N !
   0 UNIT-FAM-SERIAL-MARK !
   UNIT-FAM-REFS-RELEASE UNIT-CON-REFS-RELEASE
   UNIT-FAM-SEEN-RELEASE 0 UNIT-FAM-SEEN-EPOCH !
   UNIT-FAM-SCRATCH-RESET ;
: UNIT-FAM-REF@ ( n -- n ) cells UNIT-FAM-REFS @ + @ ;
: UNIT-CON-REF@ ( n -- n ) cells UNIT-CON-REFS @ + @ ;
: UNIT-FAM-REF! ( n n -- ) cells UNIT-FAM-REFS @ + ! ;
: UNIT-CON-REF! ( n n -- ) cells UNIT-CON-REFS @ + ! ;

: UNIT-FAM-STABLE-INIT ( -- )
   REG-AOT-N 0 ?do i REG-AOT-COUNT UNIT-FAM-STABLE i cells + ! loop ;
UNIT-FAM-STABLE-INIT

: UNIT-FAM-REF+ ( n -- )
   {: id:n :}
   id 0 < id TFAM-N @ >= or IF E-TFAM-ARITY throw THEN
   id UNIT-FAM-STABLE-N < IF EXIT THEN
   id UNIT-FAM-MARKS @ >= IF EXIT THEN
   UNIT-FAM-REF-N @ 0 ?do
      i UNIT-FAM-REF@ id = IF UNLOOP EXIT THEN
   loop
   UNIT-FAM-REF-N @ 1+ UNIT-FAM-REFS-RESERVE
   id UNIT-FAM-REF-N @ UNIT-FAM-REF!
   1 UNIT-FAM-REF-N +! ;

: UNIT-CON-REF+ ( n -- )
   {: id:n :}
   id CC-MAX < IF EXIT THEN
   UNIT-CON-REF-N @ 0 ?do
      i UNIT-CON-REF@ id = IF UNLOOP EXIT THEN
   loop
   UNIT-CON-REF-N @ 1+ UNIT-CON-REFS-RESERVE
   id UNIT-CON-REF-N @ UNIT-CON-REF!
   1 UNIT-CON-REF-N +! ;

public
: UNIT-TFAM-REF ( n -- ) UNIT-FAM-REF+ ;
private

26 constant TFC-VAR-CAP          \ internal construct scratch; not the declaration spelling cap
create TFC-VARS TFC-VAR-CAP cells allot
variable TFC-I   variable TFC-J   variable TFC-ROW

\ Installed after this file's last signature-resolution scratch is declared, so
\ the same owner clears the raw token spans before DATA is baked. Every index,
\ memo, frame and transaction reset here is process-local.
: REG-EXT-PERSIST ( -- )
   \ An ordinary prepared checker unit still owns its family plans here.
   \ Its explicit prepare end releases them after installation or discard.
   UNIT-FAM-PROJECT-ON @ 0= IF UNIT-FAM-RESET-XT THEN
   TFAM-SNAPSHOT-PERSIST
   SCHEMA-SNAPSHOT-PERSIST
   TFX-SNAP-RESET
   VNX-SNAP-RESET
   FVX-SNAP-RESET
   TFM-SNAP-RESET
   TFW-SNAP-RESET
   PF-TX-SNAP-RESET
   RBF-SNAP-RESET
   TFAM-RBF-SNAP-RESET
   SCHEMA-RBF-SNAP-RESET
   UNIT-FAM-EXPORT-RESET ;

: REG-EXT-PERSIST-INSTALL ( -- ) [: REG-EXT-PERSIST ;] is REG-EXT-PERSIST-XT ;
REG-EXT-PERSIST-INSTALL

\ This file's scratch: each cell is written and read within one call, so a
\ file boundary or image capture resets it with no later read depending on it.
: TFAM-SCRATCH-RETIRE ( -- )
   \ A caught FILE dependency refusal leaves only an invocation diagnostic.
   NULL-PTR REG-AOT-ERROR-A ! 0 REG-AOT-ERROR-U !
   TFQ-CAP 0 ?do 0 TFQ-BUF i + c! loop
   0 TFQ-U !  NULL-PTR TFQ-TA !  0 TFQ-TU !  0 TFQ-COLON !
   NULL-PTR TFSR-PA !  0 TFSR-PU !
   NULL-PTR TFSR-NA !  0 TFSR-NU !
   0 TFSR-ID !  0 TFSR-FLAG !
   TFC-VARS 0 TFC-VAR-CAP ARENA-CELLS-ZERO
   0 TFC-I !  0 TFC-J !  0 TFC-ROW !
   TF-CTOR-CAP 0 ?do 0 TF-CTOR-BUF i + c!  0 TF-INIT-TAIL i + c! loop
   0 TF-CTOR-U !
   \ Rows above the open frames were released; the open frames keep theirs.
   PF-TX-P @ PF-TX-DEPTH @ PF-TX-REC * CELL /
   PF-TX-CAP-V @ PF-TX-REC * CELL / ARENA-CELLS-ZERO ;

: TFAM-SCRATCH-INSTALL ( -- ) [: TFAM-SCRATCH-RETIRE ;] is REG-EXT-SCRATCH-XT ;
TFAM-SCRATCH-INSTALL

public

\ Construct form (docs/forth.md "Structures And Enums"): the checker's reserved
\ `construct family variant` token. A family constructs only inside its own
\ active checker package (top level owns the global "" package), public or
\ private; cross-package callers use the generated constructor words. Only sum
\ and enum kinds construct. The step effect instantiates SUMV payload schemas
\ against one fresh var per family parameter and CHECKER-STEP applies it like
\ any word call. A result of known width expands to hidden fields at the step;
\ a parametric result whose width reads an open argument (checker.f
\ LAYOUT-WIDTH-OPEN?) stays one logical cell and expands at the boundary.

private

: TFC-MINT-VARS ( n -- ) {: ar:n :}
   ar TFC-VAR-CAP > IF s" tfam: construct arity over cap" 76 die THEN
   0 TFC-I !
   BEGIN TFC-I @ ar < WHILE
      FRESH MK-VAR TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

: TFC-ARGS! ( n -- ) {: term:n :}
   term PARAM>ARGC TFC-VAR-CAP > IF s" tfam: layout arity over cap" 76 die THEN
   0 TFC-I !
   BEGIN TFC-I @ term PARAM>ARGC < WHILE
      term TFC-I @ PARAM>ARG  TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

\ Folds one quotation side (a SCH-ROW node) onto a base row, element 0 deepest,
\ matching the checker's PSTACK/SIG-PARSE-QUOT fold. Deferred: the SCH-QUOT case
\ of TFC-SCH-TERM calls it before its impl is defined.
defer TFC-QUOT-ROW ( n n -- n )   \ ( rownode base -- row )


: TFC-SCH-TERM ( n -- n )    \ payload schema node -> checker type term
   {: node:n :}
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
      FRESH MK-QROW {: dbase:n :}             \ data base, rin/rout a return base: the
      FRESH MK-QROW {: rbase:n :}             \ implicit tails a signature's callable has
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
   base 0
   BEGIN dup rownode SCHEMA-ROW-COUNT@ < WHILE
      dup rownode swap SCHEMA-ROW-ELEM@ TFC-SCH-TERM
      swap >r
      swap PUSH-LOGICAL
      r> 1 +
   REPEAT drop ;
: TFC-QUOT-ROW-INSTALL ( -- ) [: TFC-QUOT-ROW-IMPL ;] is TFC-QUOT-ROW ;
TFC-QUOT-ROW-INSTALL

\ The initialized-record surface is a bounded view of committed PRODUCT
\ fields. The physical record is canonical cell storage, and each substituted
\ field must still occupy the cells and byte range its committed id describes.
\ Comparing only the total width would accept two fields whose changed widths
\ cancel while the second field's committed offset is now wrong.
: INIT-NO-RECORD ( -- n n n bool ) 0 0 0 RES-FALSE ;
: INIT-NO-FIELD ( -- n n n n bool ) 0 0 0 0 RES-FALSE ;

: INIT-FIELD-WIDTH? ( n n n -- n bool ) {: fid:n fam:n used:n :}
   fid PF-FAM@ fam <> IF 0 RES-FALSE EXIT THEN
   fid PF-VAR@ PF-NO-VARIANT <> IF 0 RES-FALSE EXIT THEN
   fid PF-FLAGS@ PF-FLAGS-NONE <> IF 0 RES-FALSE EXIT THEN
   fid PF-SLOT@ used <> IF 0 RES-FALSE EXIT THEN
   fid PF-BYTE-OFF@ used CELL * <> IF 0 RES-FALSE EXIT THEN
   fid PF-ALIGN@ CELL <> IF 0 RES-FALSE EXIT THEN
   fid PF-SCH@ SCHEMA-ROOT@ TFC-SCH-TERM T-RES {: field:n :}
   field ISVAR IF 0 RES-FALSE EXIT THEN
   field TAG T-PARAM = IF
      field LAYOUT-WIDTH-OPEN? IF 0 RES-FALSE EXIT THEN
   THEN
   field T-WIDTH {: width:n :}
   fid PF-CELLS@ width <> IF 0 RES-FALSE EXIT THEN
   fid PF-BYTES@ width CELL * <> IF 0 RES-FALSE EXIT THEN
   width RES-TRUE ;

public

: INIT-RECORD? ( n -- n n n bool ) {: term:n :}
   term T-RES {: record:n :}
   record TAG T-PARAM <> IF INIT-NO-RECORD EXIT THEN
   record PARAM>FAM {: fam:n :}
   fam TFAM-PRODUCT? 0= IF INIT-NO-RECORD EXIT THEN
   fam C2-READ-FAM @ = fam C2-MUT-FAM @ = or
   fam C2-INIT-FAM @ = or fam C2-RECORDS-FAM @ = or IF INIT-NO-RECORD EXIT THEN
   fam TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG <> IF INIT-NO-RECORD EXIT THEN
   record PARAM>ARGC fam TFAM-ARITY@ <> IF INIT-NO-RECORD EXIT THEN
   fam TFAM-FLD-COUNT@ {: count:n :}
   count 0 <= IF INIT-NO-RECORD EXIT THEN
   fam TFAM-FLD-START@ {: base:n :}
   base 0 < base PF-N@ > or IF INIT-NO-RECORD EXIT THEN
   count PF-N@ base - > IF INIT-NO-RECORD EXIT THEN
   record LAYOUT-WIDTH-OPEN? IF INIT-NO-RECORD EXIT THEN
   record INIT-ELEM-LAYOUT? 0= IF 2drop INIT-NO-RECORD EXIT THEN
   {: size:n align:n :}
   align CELL <> IF INIT-NO-RECORD EXIT THEN
   record TFC-ARGS!
   0
   count 0 ?do
      dup base i + fam rot INIT-FIELD-WIDTH?
      IF + ELSE 2drop unloop INIT-NO-RECORD EXIT THEN
   loop
   dup fam TFAM-SLOTS@ <> IF drop INIT-NO-RECORD EXIT THEN
   dup record T-WIDTH <> IF drop INIT-NO-RECORD EXIT THEN
   dup CELL * size <> IF drop INIT-NO-RECORD EXIT THEN
   size align RES-TRUE ;

: INIT-FIELD? ( n n -- n n n n bool ) {: fid:n term:n :}
   term INIT-RECORD? 0= IF drop drop drop INIT-NO-FIELD EXIT THEN
   drop drop drop
   term T-RES {: record:n :}
   record PARAM>FAM {: fam:n :}
   fam TFAM-FLD-START@ {: base:n :}
   fid base < fid base fam TFAM-FLD-COUNT@ + >= or IF INIT-NO-FIELD EXIT THEN
   record TFC-ARGS!
   fid PF-SCH@ SCHEMA-ROOT@ TFC-SCH-TERM
   fid PF-BYTE-OFF@ fid PF-CELLS@ fid PF-BYTES@ RES-TRUE ;

: INIT-FIELD-NAME? ( n ptr u8 n -- n n n bool )
   {: record:n name:ptr size:n :}
   record INIT-RECORD? 0= IF drop drop drop 0 0 0 RES-FALSE EXIT THEN
   drop drop drop
   record T-RES PARAM>FAM PF-NO-VARIANT name size PF-FIND
   0= IF drop 0 0 0 RES-FALSE EXIT THEN
   {: fid:n :}
   fid record INIT-FIELD? 0= IF 2drop 2drop 0 0 0 RES-FALSE EXIT THEN
   {: field:n off:n cells:n bytes:n :}
   PARAM-SCR-N @ {: base:n :}
   record PARAM-SCR+
   fid PF-NAME$ MK-ATOM PARAM-SCR+
   field PARAM-SCR+
   base s" field" FIELD-FAM @ MK-PARAM off bytes RES-TRUE ;

\ A value owns only the fields in its instantiated schema. Reconstruct a field
\ term with the same substitution used by MAKE/UNMAKE and MATCH; this avoids
\ charging a phantom view element or counting a repeated owning parameter once.
: TFAM-SCH-LIN-N ( n n bool -- n ) {: root:n term:n maybe:bool :}
   term TFC-ARGS!
   root SCHEMA-ROOT@ TFC-SCH-TERM maybe OWN-TYPE-N ;

: PRODUCT-LIN-N ( n bool -- n ) {: term:n maybe:bool :}
   term PARAM>FAM {: fam:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      fam TFAM-FLD-START@ over + PF-PENDING-SCH@ term maybe TFAM-SCH-LIN-N
      rot + swap
      1 +
   REPEAT drop ;

: SUMV-LIN-N ( n n bool -- n ) {: vid:n term:n maybe:bool :}
   0
   0 BEGIN dup vid SUMV-PAY-N < WHILE
      vid over SUMV-PAY-ROOT term maybe TFAM-SCH-LIN-N
      rot + swap
      1 +
   REPEAT drop ;

: SUM-LIN-N ( n bool -- n ) {: term:n maybe:bool :}
   term PARAM>FAM {: fam:n :}
   0
   0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
      fam TFAM-VAR-START@ over + term maybe SUMV-LIN-N
      rot max swap
      1 +
   REPEAT drop ;

: TFAM-INST-LIN-N ( n bool -- n ) {: term:n maybe:bool :}
   term PARAM>FAM {: fam:n :}
   fam C2-READ-FAM @ = IF 0 EXIT THEN
   fam C2-MUT-FAM @ = IF 1 EXIT THEN
   fam C2-RECORDS-FAM @ = IF 1 EXIT THEN
   fam TFAM-PRODUCT? IF term maybe PRODUCT-LIN-N EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or IF term maybe SUM-LIN-N EXIT THEN
   0 ;

: TFAM-INST-LIN ( n -- n ) RES-FALSE TFAM-INST-LIN-N ;
: TFAM-INST-MAY ( n -- bool ) RES-TRUE TFAM-INST-LIN-N 0= 0= ;

: TFAM-INST-LIN-INSTALL ( -- )
   [: TFAM-INST-LIN ;] is TFAM-LIN-N-XT
   [: TFAM-INST-MAY ;] is TFAM-MAY-LIN-XT ;
TFAM-INST-LIN-INSTALL

\ Instantiate one field schema against its owning family value. The lowering
\ certificate can run while construct still uses TFC-VARS and TFC-I, so keep
\ its substitution local to this call.
: SCHEMA-TERM-FOR ( n n -- n ) {: owner:n node:n :}
   TFC-I @ {: old-i:n :}
   PARAM-SCR-N @ {: base:n :}
   0 BEGIN dup TFC-VAR-CAP < WHILE
      dup cells TFC-VARS + @ PARAM-SCR+
      1 +
   REPEAT drop
   owner T-RES TFC-ARGS!
   node TFC-SCH-TERM {: term:n :}
   0 BEGIN dup TFC-VAR-CAP < WHILE
      dup base + cells PARAM-SCR + @
      over cells TFC-VARS + !
      1 +
   REPEAT drop
   base PARAM-SCR-N !
   old-i TFC-I !
   term ;

private

\ A layout value's members for checker.f's CAST: walks (CAST-INTRO? and
\ CAST-MAY-LINEAR?) and IN-PLACE?: every product field and every variant
\ payload, in either payload form, instantiated over the value's own arguments
\ and pushed on one row. A field projection or a MATCH arm yields exactly these
\ types. Schemas index parameters by position, so the term must carry the
\ family's argument count: the cast gate refuses any other count before its
\ walks, and IN-PLACE? answers false for such a term without asking.
: MEMBER+ ( n n n -- n ) {: term:n root:n row:n :}
   term root SCHEMA-ROOT@ SCHEMA-TERM-FOR row MK-PUSH ;
: FIELD-MEMBERS ( n n n -- n ) {: term:n fam:n row:n :}
   row
   fam TFAM-FLD-COUNT@ 0 ?do term fam TFAM-FLD-START@ i + PF-SCH@ rot MEMBER+ loop ;
: VARIANT-MEMBERS ( n n n -- n ) {: term:n vid:n row:n :}
   row
   vid SUMV-PAY-N 0 ?do term vid i SUMV-PAY-ROOT rot MEMBER+ loop ;
: TFAM-MEMBERS ( n -- n ) {: term0:n :}
   term0 T-UNSTALE {: term:n :}
   term PARAM>FAM {: fam:n :}
   term PARAM>ARGC fam TFAM-ARITY@ <> IF
      s" tfam: layout members need the family's arity" 76 die THEN
   fam TFAM-PRODUCT? IF term fam FRESH MK-ROW FIELD-MEMBERS EXIT THEN
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF
      s" tfam: members of a family with no layout" 76 die THEN
   FRESH MK-ROW
   fam TFAM-VAR-COUNT@ 0 ?do term fam TFAM-VAR-START@ i + rot VARIANT-MEMBERS loop ;
: TFAM-MEMBERS-INSTALL ( -- ) [: TFAM-MEMBERS ;] is TFAM-MEMBERS-XT ;
TFAM-MEMBERS-INSTALL

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

\ Width-aware construct/MATCH lowering. For a wide closed instantiation (a
\ multi-cell layout argument, flat or nested) the checker records an extra-pad
\ fact at the construct, constructor-call or `of` token: w = instantiated pads -
\ declared pads, flagged WF-XPAD-FLAG so pass 2 fires even when w is 1. Pass 2
\ (native EM-ADT-CON-VAR / EM-COMPILE-CALL / EM-ADT-MATCH-OF, mirrored in
\ bootstrap/cg/forth.fs) adds those zero cells. A nested instantiation
\ (option<result<n,pkg:prod>>) needs nothing more: every width site
\ (TFAM-INST-WIDTH@, TFC-VAR-PAYCELLS, T-WIDTH) recurses through the argument
\ tree, so the inner bundle gets its own pads at its own site. Every width must
\ be closed: an open inner var (option<result<n,a>>) has an unstable width, so
\ TYPE-CLOSED? rejects it and CONSTRUCT-WIDE-STAGED-REJECT holds.
: TFC-VAR-PAYCELLS ( n -- n ) {: vid:n :}   \ sum of instantiated payload cell widths for a variant
   0
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-PAY-N < WHILE
      vid TFC-J @ SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-SCH-TERM T-WIDTH +
      TFC-J @ 1 + TFC-J !
   REPEAT ;

\ Extra pads = instantiated pads - declared pads; instantiated pads =
\ instantiated payload slots - instantiated payload cells. Only a SUM or ENUM
\ has a tag cell; a PRODUCT's whole width is payload (scg<scinr>, a parametric
\ STRUCTURE at a width-2 leaf, is width 3, all payload: subtracting a tag would
\ report extra -1), so TFC-TAG-CELLS keys the subtraction off the family kind.
\ A negative extra (the widest declared variant is not the widest instantiated
\ one, or a non-widest variant grows more) is a contradiction: pass 1 emits the
\ declared pads and add-only pass 2 cannot remove cells, so no sound lowering
\ exists and it is refused unconditionally, a candidate probe included. Signed
\ pass-2 corrections would make it lowerable.
: TFC-XPAD-NARROW-REJECT ( -- )   \ certified instantiated width contradicts add-only lowering: never certify
   0 OK ! -1 FAILSET ! ;

: TFC-TAG-CELLS ( n -- n )   \ layout tag cells for a family: 1 for a tagged SUM/ENUM, 0 for a tagless PRODUCT/STRUCTURE
   dup TFAM-SUM? IF drop 1 EXIT THEN
   TFAM-ENUM? IF 1 ELSE 0 THEN ;

\ One subtraction answers both lowerings, so the number is latched here. Both
\ push what the family declares and can only add: pass 1 emits the declared
\ pads and pass 2 adds the WF row's cells at the same site; the native chain
\ calls the generated constructor (declared pads and tag, whatever the
\ instantiation) or reads the declared pads from the registry for a
\ `construct`. MWIN-CELLS! files the difference under the judged token for
\ src/compiler/native/elaborate.f; the WF row carries it to pass 2. A
\ construction that adds nothing files nothing (src/core/checker.f states the
\ absent case above the table).
: TFC-CON-XPAD-RECORD ( n n n -- ) {: fam:n vid:n famterm:n :}
   famterm T-WIDTH fam TFC-TAG-CELLS -
   vid TFC-VAR-PAYCELLS -
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -
   {: extra:n :}
   extra 0 > IF extra MWIN-CELLS!  0 fam 0 extra WF-XPAD-FLAG WF-ADD-FULL EXIT THEN
   extra 0 < IF TFC-XPAD-NARROW-REJECT THEN ;   \ add-only lowering cannot remove the surplus

\ MATCH sees only a SUM or ENUM family: TFAM-MATCH-FAM and TFL-MATCH-FAM? reject
\ any other kind (MD-FAM-KIND) before an arm is recorded, and a product's UNMAKE
\ records no pad fact. So `rt T-WIDTH 1 -` needs no kind guard.
\
\ Pass 2 needs the difference from the declared pads (its lowering emitted them
\ and can only add); a compiler that models the stack cell by cell needs the
\ pad count itself. Both come off one subtraction: MWIN-CELLS! files the count
\ under this `of` token for src/compiler/native/elaborate.f, and the WF row
\ carries the difference to pass 2. An arm whose term is not closed latches
\ nothing: the cell-accurate consumer then refuses the definition by name.
: TFAM-MATCH-XPAD-RECORD ( n n -- ) {: vid:n term:n :}
   term T-RES {: rt:n :}
   rt TYPE-REP-CLOSED? 0= IF EXIT THEN           \ open value/row: leave declared width; scope identities do not affect padding
   rt T-WIDTH 1 -
   vid TFC-VAR-PAYCELLS -
   {: ipads:n :}
   ipads MWIN-CELLS!
   ipads
   vid SUMV-FAM@ TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -
   {: extra:n :}
   extra 0 > IF 0 vid SUMV-FAM@ 0 extra WF-XPAD-FLAG WF-ADD-FULL EXIT THEN
   extra 0 < IF TFC-XPAD-NARROW-REJECT THEN ;   \ declared-width unpack would skip a pad the bundle lacks

public

: TFAM-ACTIVE-PKG$ ( -- ptr u8 n )        \ authenticated package ("" at top level)
   CHECKER-AUTH-PACKAGE$ ;

private

: TFAM-CONSTRUCT-FAM ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ folded family token -> id
   TFAM-ACTIVE-PKG$ na nu TFAM-FIND-IN 0= IF drop MD-CON-FAM MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-CON-KIND MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

\ TFC-CONSTRUCT-STEP-VID ( fam vid -- ) : the inline generated-constructor
\ effect for a resolved (family, variant). The concrete args named by the
\ declared output are recovered over fresh vars (CONSTRUCT-DECL-LAYOUT), so a
\ direct closed layout argument makes the payload input and the bundle output
\ PUSH-LOGICAL-expand even at width 1; open, linear, scalar and pointer args keep
\ the fresh var. Shared by the `construct` token and the generated-constructor
\ call (TFAM-CTOR-STEP?).
: TFC-CONSTRUCT-STEP-VID ( n n -- )
   {: fam:n vid:n :}
   \ The layout query walks ownership through TFC-VARS, so populate this
   \ constructor's arguments only after that query has finished.
   fam CONSTRUCT-DECL-LAYOUT {: dt:n seeded:bool :}
   seeded IF dt TFC-ARGS! ELSE fam TFAM-ARITY@ TFC-MINT-VARS THEN
   FRESH MK-ROW {: base:n :}
   vid base TFC-PAY-ROW {: din:n :}
   fam TFC-FAM-TERM {: famterm:n :}
   seeded IF
      dt TYPE-REP-CLOSED? IF fam vid famterm TFC-CON-XPAD-RECORD THEN
   THEN
   famterm base PUSH-LOGICAL {: dout:n :}
   din dout RECORDED-STEP
   seeded IF
      dt TYPE-REP-CLOSED? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN   \ open value/row: stay staged fail-closed
   THEN ;

: TFAM-CONSTRUCT-STEP ( ptr u8 n n -- bool ) {: na:ptr nu:n fam:n :}
   fam na nu SUMV-FIND 0= IF drop MD-CON-VAR MDIAG-COMPILE! RES-FALSE EXIT THEN
   {: vid:n :}
   fam vid TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ TFAM-CTOR-STEP? ( sym -- bool ) : a generated-constructor CALL whose stored
\ var effect cannot absorb a direct logical-layout argument. Reverse the resolved
\ word symbol to its variant; if CONSTRUCT-DECL-LAYOUT finds an eligible declared
\ output, apply the bidirectionally seeded step and report handled. Otherwise
\ report unhandled so DO-TOK runs the ordinary word call. A product's UNMAKE row
\ never takes the construct step: TFAM-UNMAKE-STEP? runs it backwards, seeded from
\ the value it takes apart.

public

: SUMV-FROM-CTOR-SYM ( n -- n bool ) {: sym:n :}   \ constructor word symbol -> variant id
   sym 0 <= IF 0 RES-FALSE EXIT THEN
   SVX-ENSURE
   sym SVX@ dup 0= IF RES-FALSE EXIT THEN          \ 0 head = no variant with this symbol
   1 - RES-TRUE ;

private

\ A product's two generated rows are make then unmake, the order sumtype.f
\ TDECL-PROD-PLAN generates their words in.
: TFAM-UNMAKE-ROW? ( n n -- bool ) {: fam:n vid:n :}
   fam TFAM-PRODUCT? 0= IF RES-FALSE EXIT THEN
   vid fam TFAM-VAR-START@ 1 + = ;

\ The construct step with its rows exchanged: the family value over the seeded
\ args is the input and the instantiated fields are the output, over one base
\ row. An open seeded term stays staged fail-closed, as it does for MAKE.
: TFAM-UNMAKE-STEP? ( n n -- bool )
   {: fam:n vid:n :}
   fam UNMAKE-SEED-LAYOUT 0= IF drop RES-FALSE EXIT THEN
   {: dt:n :}
   dt TFC-ARGS!
   fam TFC-FAM-TERM {: famterm:n :}
   FRESH MK-ROW {: base:n :}
   vid base TFC-PAY-ROW {: dout:n :}
   famterm base PUSH-LOGICAL {: din:n :}
   din dout RECORDED-STEP
   dt TYPE-REP-CLOSED? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN
   RES-TRUE ;

: TFAM-CTOR-STEP? ( n -- bool ) {: sym:n :}
   sym SUMV-FROM-CTOR-SYM 0= IF drop RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam vid TFAM-UNMAKE-ROW? IF fam vid TFAM-UNMAKE-STEP? EXIT THEN
   fam CONSTRUCT-DECL-LAYOUT nip 0= IF RES-FALSE EXIT THEN
   fam vid TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ MATCH resolution (docs/forth.md "Structures And Enums") follows signature
\ scope, not construct's owner-only rule: a family matches wherever a stack
\ signature can name it (own package private and public, else the unique public
\ family, qualified PKG:tail included), since a value of it arrives only through
\ such a signature. Only sum and enum kinds match. The arm payload row
\ instantiates the variant schema over the scrutinee's argument terms in the TFC
\ scratch vars, consumed at OF, so construct and nested matches may interleave.
: TFAM-MATCH-FAM ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ folded family token
   TFAM-ACTIVE-PKG$ na nu TFAM-SIG-RESOLVE 0= IF drop MD-FAM-UNKNOWN MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-FAM-KIND MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFAM-MATCH-VARIANT ( ptr u8 n n -- n bool ) {: na:ptr nu:n fam:n :}
   fam na nu SUMV-FIND ;

: TFAM-MATCH-PAY ( n n n -- n ) {: vid:n term:n row:n :}   \ variant payload onto row
   term T-RES TFC-ARGS!
   vid term TFAM-MATCH-XPAD-RECORD
   vid row TFC-PAY-ROW ;

\ Field projection, the registry half of the checker's FIELD-PROJ-XT hook. The
\ committed field id is the sole authority: offset, family, extent, role and
\ schema derive from it through TYPE-FIELD reflection, so a forged offset, an
\ offset past the family width, a foreign family, a wrong generic arity or a
\ non-addressable role each answer 0 and false. It lives here because the field
\ and schema registries and TFC-SCH-TERM belong to this file; the checker
\ reaches it only through the sealed friend hook.
: TFAM-FIELD-PROJ-DO ( n n n n -- n bool )   \ fid off famterm ffam -- fieldterm ok
   {: fid:n off:n fam:n ffam:n :}
   ffam fam PARAM>FAM <> IF 0 RES-FALSE EXIT THEN                \ field not owned by the input family
   off fid TYPE-FIELD:BYTE-OFF@ <> IF 0 RES-FALSE EXIT THEN      \ baked offset disagrees with the committed offset (forged access)
   fid TYPE-FIELD:FLAGS@ PF-FLAGS-NONE <> IF 0 RES-FALSE EXIT THEN   \ non-addressable field role (niche/boxed/custom): no projection
   fam PARAM>ARGC ffam TFAM-ARITY@ <> IF 0 RES-FALSE EXIT THEN   \ generic arity mismatch
   fam TFAM-INST-WIDTH@ ffam TFAM-SLOTS@ <> IF 0 RES-FALSE EXIT THEN   \ a wide instantiation moves every field past the first: the committed offsets no longer describe it (see the note below)
   off fid TYPE-FIELD:BYTES@ +  ffam TFAM-SLOTS@ CELL *  > IF 0 RES-FALSE EXIT THEN   \ field extent past the family width
   fam TFC-ARGS!
   fid TYPE-FIELD:SCHEMA@ SCHEMA-ROOT@ TFC-SCH-TERM RES-TRUE ;   \ root index -> node -> instantiate the field schema over the input args
\ Only the committed field-family lookup is caught: an E-PF-ID there means the
\ expected absent field, and every other error propagates.
: TFAM-FIELD-PROJ ( n n n -- n bool )   \ fid off famterm -- fieldterm ok
   {: fid:n off:n fam:n :}
   fid [: TYPE-FIELD:FAMILY@ ;] catch
   dup 0= IF drop {: ffam:n :} fid off fam ffam TFAM-FIELD-PROJ-DO EXIT THEN
   nip dup E-PF-ID <> IF throw THEN
   drop 0 RES-FALSE ;

\ The width condition above, like the forged-offset and role conditions, guards
\ the arming boundary: only a generator that armed a window with an id its text
\ does not describe reaches it. A family type argument does not bind an
\ accessor's parameter (measured: `expected: ptr sdagen<a> actual: ptr
\ sdagen<sdaw1>`), and every other argument is one cell. Its judgeable twin is
\ RECORD-AT-EXACT? in src/core/checker.f, which a written-out `record-at` call
\ reaches with a concrete wide pointee (test/structure-decl-suite.f case 15,
\ SDAW3); the two must keep the same condition, or the two halves of one address
\ surface describe different instantiations.

\ Fixed storage walks the same instantiated schema that field projection uses,
\ but computes its own offsets: a wide argument moves every following field.
\ The stored type has already passed CHECKER-STORAGE-INFO's closed-layout gate.
public

: TFAM-STORAGE-QUOT? ( n -- bool ) T-RES TAG T-QUOT = ;
: TFAM-STORAGE-PRODUCT? ( n -- n bool )
   T-RES dup TAG T-PARAM <> IF drop 0 RES-FALSE EXIT THEN
   PARAM>FAM dup TFAM-PRODUCT? IF RES-TRUE ELSE drop 0 RES-FALSE THEN ;
: TFAM-STORAGE-SUM? ( n -- n bool )
   T-RES dup TAG T-PARAM <> IF drop 0 RES-FALSE EXIT THEN
   PARAM>FAM dup TFAM-SUM? over TFAM-ENUM? or
   IF RES-TRUE ELSE drop 0 RES-FALSE THEN ;
: TFAM-STORAGE-FIELD ( n n -- n n ) {: term:n field:n :}
   term TFC-ARGS!
   field TYPE-FIELD:SCHEMA@ SCHEMA-ROOT@ TFC-SCH-TERM
   dup T-WIDTH ;
: TFAM-STORAGE-VARIANT ( n n -- n bool ) {: fam:n tag:n :}
   fam TFAM-VAR-COUNT@ 0 ?do
      fam TFAM-VAR-START@ i + {: vid:n :}
      vid SUMV-TAG@ tag = IF vid RES-TRUE unloop EXIT THEN
   loop
   0 RES-FALSE ;
: TFAM-STORAGE-PAY ( n n n -- n n ) {: term:n vid:n index:n :}
   term TFC-ARGS!
   vid index SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-SCH-TERM
   dup T-WIDTH ;

\ Inspect the instantiated value graph once at allocation. A pointer is a
\ scalar storage cell, even when its pointee could itself hold a quotation.
\ Every sum alternative matters because the active tag can change after this
\ declaration; capture later walks only the alternative present in DATA.
: TFAM-STORAGE-HAS-QUOT? ( n -- bool ) {: term:n :}
   term TFAM-STORAGE-QUOT? IF RES-TRUE EXIT THEN
   term TFAM-STORAGE-PRODUCT? IF
      {: fam:n :}
      fam TFAM-FLD-COUNT@ 0 ?do
         fam TFAM-FLD-START@ i + {: field:n :}
         term field TFAM-STORAGE-FIELD drop recurse IF
            RES-TRUE unloop EXIT
         THEN
      loop RES-FALSE EXIT
   THEN drop
   term TFAM-STORAGE-SUM? IF
      {: fam:n :}
      fam TFAM-VAR-COUNT@ 0 ?do
         fam TFAM-VAR-START@ i + {: vid:n :}
         vid SUMV-PAY-N 0 ?do
            term vid i TFAM-STORAGE-PAY drop recurse IF
               RES-TRUE unloop unloop EXIT
            THEN
         loop
      loop RES-FALSE EXIT
   THEN drop
   RES-FALSE ;

private

\ Compiler-facing lowering surface: resolution and metadata for the native
\ construct/MATCH emitters, called by name through the engine's C-FIND-GLOBAL
\ friend bridge at captured token positions. Same registry and scope rules as
\ the checker's friend XTs, but no diagnostic latch and no checker-row effect:
\ the checker still judges the definition at `;`. Raw engine token spans fold
\ here (TOKFOLD), so `construct ZRES OK` and its lowercase spelling agree.

public

: TFL-SUMKIND? ( n -- bool ) {: id:n :}   \ constructible/matchable kind
   id TFAM-SUM? id TFAM-ENUM? or ;

: TFL-FOLD$ ( ptr u8 n -- ptr u8 n )      \ fold a raw engine token (shared TKF buffer)
   TOKFOLD drop TKF TKFU @ ;

;package

: TFL-CON-FAM? ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ owner-only scope
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-FIND-IN 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

package TFAM

;package

: TFL-MATCH-FAM? ( ptr u8 n -- n bool ) {: na:ptr nu:n :}   \ signature scope
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

package TFAM

public

: TFL-VAR? ( ptr u8 n n -- n bool ) {: na:ptr nu:n fam:n :}   \ variant in fam -> vid
   fam na nu TFL-FOLD$ SUMV-FIND ;

: TFL-VPADS ( n n -- n ) {: fam:n vid:n :}   \ zero pads M-p for a variant's construct
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - ;

;package

: TFL-CVAR? ( ptr u8 n n -- n n bool )   \ variant in a resolved fam -> ( tag pads ok )
   {: va:ptr vu:n fam:n :}
   va vu fam TFL-VAR? 0= IF drop 0 0 RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-TAG@  fam vid TFL-VPADS  RES-TRUE ;

package TFAM

public

: TFL-CON? ( ptr u8 n ptr u8 n -- n n bool )   \ construct one-shot: -> tag pads ok
   {: fa:ptr fu:n va:ptr vu:n :}
   fa fu TFL-CON-FAM? 0= IF drop 0 0 RES-FALSE EXIT THEN
   {: fam:n :}
   va vu fam TFL-CVAR? ;

private

: UNIT-FAM-MARK ( -- )
   REG-AOT-N 0 ?do i REG-AOT-COUNT UNIT-FAM-MARKS i cells + ! loop
   PF-TX-SERIAL @ UNIT-FAM-SERIAL-MARK !
   0 UNIT-FAM-REF-N ! 0 UNIT-CON-REF-N ! ;

\ The transaction serials the slice opened since its mark.
: UNIT-FAM-SERIAL-DELTA ( -- n )
   PF-TX-SERIAL @ UNIT-FAM-SERIAL-MARK @ - dup 0 < IF
      drop s" tfam: FILE transaction serial moved backward" REG-AOT-REFUSE THEN ;

: UNIT-FAM-BASE@ ( n -- n ) cells UNIT-FAM-MARKS + @ ;

: UNIT-FAM-SCAN-NODE ( n -- ) {: node:n :}
   UNIT-FAM-SEEN @ node cells + @ UNIT-FAM-SEEN-EPOCH @ = IF EXIT THEN
   UNIT-FAM-SEEN-EPOCH @ UNIT-FAM-SEEN @ node cells + !
   node SCHEMA-APP? IF
      node SCHEMA-A@ UNIT-FAM-REF+
      node SCHEMA-C@ 0 ?do
         node SCHEMA-B@ i + SCHEMA-ROOT@ recurse
      loop EXIT
   THEN
   node SCHEMA-CON? IF node SCHEMA-A@ UNIT-CON-REF+ EXIT THEN
   node SCHEMA-PTR? IF node SCHEMA-A@ recurse EXIT THEN
   node SCHEMA-QUOT? IF
      node SCHEMA-C@ 0 ?do
         node SCHEMA-B@ i + SCHEMA-ROOT@ recurse
      loop
      EXIT
   THEN
   node SCHEMA-ROW? IF
      node SCHEMA-B@ 0 ?do
         node SCHEMA-A@ i + SCHEMA-ROOT@ recurse
      loop
   THEN ;

: UNIT-FAM-SCAN-FAMILY ( n -- ) {: fam:n :}
   fam TFAM-SCHEMA-ROOT@ dup 0 <> IF SCHEMA-ROOT@ UNIT-FAM-SCAN-NODE
      ELSE drop THEN
   fam TFAM-VAR-COUNT@ 0 ?do
      fam TFAM-VAR-START@ i + {: vid:n :}
      vid SUMV-SCH-COUNT@ 0 ?do
         vid SUMV-REC@ SV.SCH-START @ i + SCHEMA-ROOT@
         UNIT-FAM-SCAN-NODE
      loop
   loop
   fam TFAM-FLD-COUNT@ 0 ?do
      fam TFAM-FLD-START@ i + PF-PENDING-SCH@
      SCHEMA-ROOT@ UNIT-FAM-SCAN-NODE
   loop ;

: UNIT-FAM-SCAN ( -- )
   6 REG-AOT-COUNT dup UNIT-FAM-SEEN-RESERVE
   drop
   UNIT-FAM-SEEN-EPOCH @ 1+ dup 0= IF
      drop UNIT-FAM-SEEN @ CELL-VIEW 0 UNIT-FAM-SEEN-CAP @ ARENA-CELLS-ZERO
      1
   THEN UNIT-FAM-SEEN-EPOCH !
   6 REG-AOT-COUNT 6 UNIT-FAM-BASE@ ?do
      i SCHEMA-APP? IF i SCHEMA-A@ UNIT-FAM-REF+ THEN
      i SCHEMA-CON? IF i SCHEMA-A@ UNIT-CON-REF+ THEN
   loop
   0
   BEGIN dup UNIT-FAM-REF-N @ < WHILE
      dup UNIT-FAM-REF@ UNIT-FAM-SCAN-FAMILY
      1+
   REPEAT drop ;

: UNIT-FAM-KEY ( n -- ptr u8 n ) FAMILY-KEY-XT
   \ tfam: FILE family lacks declaration identity
   0= IF 2drop CHECKER-REG:UNIT-BAD THEN ;

: UNIT-CON-KEY ( n -- ptr u8 n ) CON-KEY-XT
   \ tfam: FILE constructor lacks declaration identity
   0= IF 2drop CHECKER-REG:UNIT-BAD THEN ;

defer UNIT-FAM-CONTRACT-SIZE-XT ( n -- n )
defer UNIT-FAM-CONTRACT-WRITE-XT ( n ptr u8 -- ptr u8 )
defer UNIT-FAM-CONTRACT-CHECK-XT ( n ptr u8 n -- )
defer UNIT-CON-CONTRACT-SIZE-XT ( n -- n )
defer UNIT-CON-CONTRACT-WRITE-XT ( n ptr u8 -- ptr u8 )
defer UNIT-CON-CONTRACT-CHECK-XT ( n ptr u8 n ptr u8 n -- )

: UNIT-FAM-STR-SIZE ( -- n )
   0
   0 REG-AOT-COUNT 0 UNIT-FAM-BASE@ ?do
      i TF-REC@ {: row:ptr :}
      16 row TF.PKG-U @ + +  row TF.NAME-U @ +
   loop
   2 REG-AOT-COUNT 2 UNIT-FAM-BASE@ ?do
      i SUMV-REC@ {: row:ptr :}
      16 row SV.NAME-U @ + + row SV.CTOR-PKG-U @ +
   loop
   3 REG-AOT-COUNT 3 UNIT-FAM-BASE@ ?do
      i PF-REC@ PF.NAME-U @ 8 + +
   loop ;

: UNIT-FAM-SIZE ( -- n )
   UNIT-FAM-SCAN
   UNIT-FAM-REF-N @ UNIT-CON-REF-N @ or UNIT-FAM-SERIAL-DELTA or
   REG-AOT-N 0 ?do
      i 5 <> IF i REG-AOT-COUNT i UNIT-FAM-BASE@ - or THEN
   loop
   0= IF
      \ tfam: FILE family pool delta holds bytes no row names
      5 REG-AOT-COUNT 5 UNIT-FAM-BASE@ <> IF CHECKER-REG:UNIT-BAD THEN
      0 EXIT
   THEN
   UNIT-FAM-HDR
   REG-AOT-N 0 ?do
      i 5 <> IF
         i REG-AOT-COUNT i UNIT-FAM-BASE@ -
         i REG-AOT-WIDTH * +
      THEN
   loop
   UNIT-FAM-STR-SIZE +
   0 REG-AOT-COUNT 0 UNIT-FAM-BASE@ ?do
      i UNIT-FAM-KEY nip 8 + +
   loop
   8 +
   UNIT-FAM-REF-N @ 0 ?do
      i UNIT-FAM-REF@ {: id:n :}
      id UNIT-FAM-KEY nip 24 + +
      id UNIT-FAM-CONTRACT-SIZE-XT +
   loop
   8 +
   UNIT-CON-REF-N @ 0 ?do
      i UNIT-CON-REF@ {: id:n :}
      id UNIT-CON-KEY nip 24 + +
      id UNIT-CON-CONTRACT-SIZE-XT +
   loop ;

: UNIT-FAM-U64+ ( n ptr u8 -- ptr u8 )
   {: value:n dst:ptr :}
   value dst REG-AOT-U64! dst 8 + ;

: UNIT-FAM-STR+ ( n n ptr u8 -- ptr u8 )
   {: off:n u:n dst:ptr :}
   u dst UNIT-FAM-U64+ {: bytes:ptr :}
   u 0 > IF off u TF-OFF$ drop bytes u ARENA-COPY THEN
   bytes u + ;

: UNIT-FAM-KEY+ ( ptr u8 n ptr u8 -- ptr u8 )
   {: key:ptr u:n dst:ptr :}
   u dst UNIT-FAM-U64+ {: bytes:ptr :}
   u 0 > IF key bytes u ARENA-COPY THEN
   bytes u + ;

: UNIT-FAM-OWN-KEY+ ( n ptr u8 -- ptr u8 )
   {: id:n dst:ptr :}
   id UNIT-FAM-KEY dst UNIT-FAM-KEY+ ;

: UNIT-CON-OWN-KEY+ ( n ptr u8 -- ptr u8 )
   {: id:n dst:ptr :}
   id UNIT-CON-KEY dst UNIT-FAM-KEY+ ;

\ An import rebuilds the pool delta by placing each carried string at its
\ rebased source offset (UNIT-FAM-PUT-STRING), so the capture refuses a delta
\ its row strings do not tile exactly. An empty string claims no byte; an empty
\ constructor package is absent at offset 0, any other sits inside the delta.
: UNIT-FAM-CLAIM ( n n -- ) {: off:n u:n :}
   5 UNIT-FAM-BASE@ {: base:n :}
   \ tfam: FILE family string lies outside its pool delta
   off base < off 5 REG-AOT-COUNT u - > or IF CHECKER-REG:UNIT-BAD THEN
   u 0 ?do off base - i + UNIT-FAM-COVER! loop ;

: UNIT-FAM-CTOR-CLAIM ( n n -- ) {: off:n u:n :}
   u 0 <> IF off u UNIT-FAM-CLAIM EXIT THEN
   \ tfam: FILE variant places an absent constructor package
   off 0 <> IF CHECKER-REG:UNIT-BAD THEN ;

: UNIT-FAM-TILED ( -- )
   5 REG-AOT-COUNT 5 UNIT-FAM-BASE@ - {: n:n :}
   n UNIT-FAM-COVER-OPEN
   0 REG-AOT-COUNT 0 UNIT-FAM-BASE@ ?do
      i TF-REC@ {: row:ptr :}
      row TF.PKG-OFF @ row TF.PKG-U @ UNIT-FAM-CLAIM
      row TF.NAME-OFF @ row TF.NAME-U @ UNIT-FAM-CLAIM
   loop
   2 REG-AOT-COUNT 2 UNIT-FAM-BASE@ ?do
      i SUMV-REC@ {: row:ptr :}
      row SV.NAME-OFF @ row SV.NAME-U @ UNIT-FAM-CLAIM
      row SV.CTOR-PKG-OFF @ row SV.CTOR-PKG-U @ UNIT-FAM-CTOR-CLAIM
   loop
   3 REG-AOT-COUNT 3 UNIT-FAM-BASE@ ?do
      i PF-REC@ {: row:ptr :}
      row PF.NAME-OFF @ row PF.NAME-U @ UNIT-FAM-CLAIM
   loop
   \ tfam: FILE family pool delta holds bytes no row names
   n UNIT-FAM-COVER-FULL? 0= IF CHECKER-REG:UNIT-BAD THEN ;

\ A variant's constructor symbol travels as its unit symbol ordinal + 1, the
\ wire rule for symbols (checker.f UNIT-SYM-ORD); 0 stays absent. The checker
\ maps the ordinal back to the live symbol at install (UNIT-FAM-CTORS), so a
\ reused file leaves the variant naming its constructor word as compiling it
\ does. A constructor word the file did not define has no ordinal it owns.
: UNIT-FAM-CTOR-ORDS ( ptr u8 n -- ) {: rows:ptr bytes:n :}
   bytes SUMV-REC / 0 ?do
      rows i SUMV-REC * + CELL-VIEW SV.CTOR-SYM {: cell:ptr :}
      cell @ {: sym:n :}
      sym 0 <> IF
         \ tfam: FILE variant constructor is not the file's own word
         sym CHECKER-REG:UNIT-OWN-SYM-ORD 0= IF CHECKER-REG:UNIT-BAD THEN
         1 + cell !
      THEN
   loop ;

: UNIT-FAM-SAVE ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   UNIT-FAM-SIZE {: size:n :}
   size cap > IF REG-AOT-OVERFLOW THEN
   size 0= IF 0 EXIT THEN
   UNIT-FAM-TILED
   UNIT-FAM-MAGIC dst UNIT-FAM-U64+
   REG-AOT-N swap UNIT-FAM-U64+
   UNIT-FAM-SERIAL-DELTA swap UNIT-FAM-U64+ UNIT-FAM-AT !
   REG-AOT-N 0 ?do
      i UNIT-FAM-BASE@ {: base:n :}
      i REG-AOT-COUNT {: end:n :}
      base UNIT-FAM-AT @ UNIT-FAM-U64+
      i 5 = IF 0 ELSE end base - THEN swap UNIT-FAM-U64+
      UNIT-FAM-STABLE i cells + @ swap UNIT-FAM-U64+ UNIT-FAM-AT !
   loop
   REG-AOT-N 0 ?do
      i 5 <> IF
         i REG-AOT-COUNT i UNIT-FAM-BASE@ - i REG-AOT-WIDTH * {: bytes:n :}
         bytes 0 > IF
            i REG-AOT-BASE-PTR i UNIT-FAM-BASE@ i REG-AOT-WIDTH * +
            UNIT-FAM-AT @ bytes ARENA-COPY
            i 2 = IF UNIT-FAM-AT @ bytes UNIT-FAM-CTOR-ORDS THEN
         THEN
         UNIT-FAM-AT @ bytes + UNIT-FAM-AT !
      THEN
   loop
   0 REG-AOT-COUNT 0 UNIT-FAM-BASE@ ?do
      i TF-REC@ {: row:ptr :}
      row TF.PKG-OFF @ row TF.PKG-U @ UNIT-FAM-AT @ UNIT-FAM-STR+
      row TF.NAME-OFF @ row TF.NAME-U @ rot UNIT-FAM-STR+
      UNIT-FAM-AT !
   loop
   2 REG-AOT-COUNT 2 UNIT-FAM-BASE@ ?do
      i SUMV-REC@ {: row:ptr :}
      row SV.NAME-OFF @ row SV.NAME-U @ UNIT-FAM-AT @ UNIT-FAM-STR+
      row SV.CTOR-PKG-OFF @ row SV.CTOR-PKG-U @ rot UNIT-FAM-STR+
      UNIT-FAM-AT !
   loop
   3 REG-AOT-COUNT 3 UNIT-FAM-BASE@ ?do
      i PF-REC@ {: row:ptr :}
      row PF.NAME-OFF @ row PF.NAME-U @ UNIT-FAM-AT @ UNIT-FAM-STR+
      UNIT-FAM-AT !
   loop
   0 REG-AOT-COUNT 0 UNIT-FAM-BASE@ ?do
      i UNIT-FAM-AT @ UNIT-FAM-OWN-KEY+ UNIT-FAM-AT !
   loop
   UNIT-FAM-REF-N @ UNIT-FAM-AT @ UNIT-FAM-U64+ UNIT-FAM-AT !
   UNIT-FAM-REF-N @ 0 ?do
      i UNIT-FAM-REF@ {: id:n :}
      id UNIT-FAM-AT @ UNIT-FAM-U64+
      id swap UNIT-FAM-OWN-KEY+ UNIT-FAM-AT !
      id UNIT-FAM-CONTRACT-SIZE-XT UNIT-FAM-AT @ UNIT-FAM-U64+
      id swap UNIT-FAM-CONTRACT-WRITE-XT UNIT-FAM-AT !
   loop
   UNIT-CON-REF-N @ UNIT-FAM-AT @ UNIT-FAM-U64+ UNIT-FAM-AT !
   UNIT-CON-REF-N @ 0 ?do
      i UNIT-CON-REF@ {: id:n :}
      id UNIT-FAM-AT @ UNIT-FAM-U64+
      id swap UNIT-CON-OWN-KEY+ UNIT-FAM-AT !
      id UNIT-CON-CONTRACT-SIZE-XT UNIT-FAM-AT @ UNIT-FAM-U64+
      id swap UNIT-CON-CONTRACT-WRITE-XT UNIT-FAM-AT !
   loop
   UNIT-FAM-AT @ dst - size <> IF
      s" tfam: FILE family size drift" REG-AOT-REFUSE THEN
   size ;

\ A FILE family's source indices are local to the capture engine. Keep only
\ the new records while preparation builds a read view over live rows and the
\ deltas already prepared for this FILE tree. Neither preparation nor graph
\ checking publishes a registry member.
0 constant UFP.NEXT
1 constant UFP.BLOB
2 constant UFP.CAP
3 constant UFP.SRC
4 constant UFP.U
5 constant UFP.KEYS
6 constant UFP.FMAP
7 constant UFP.FN
8 constant UFP.CMAP
9 constant UFP.CN
10 constant UFP.SBASE
18 constant UFP.COUNT
26 constant UFP.TBASE
34 constant UFP.SERIAL
35 cells constant UFP.REC
REG-AOT-N PTR-U8-TABLE UNIT-FAM-RAW
PTR-VARIABLE UNIT-FAM-FIRST
PTR-VARIABLE UNIT-FAM-LAST
PTR-VARIABLE UNIT-FAM-CUR
PTR-VARIABLE UNIT-FAM-OPEN
PTR-VARIABLE UNIT-FAM-IN
PTR-VARIABLE UNIT-FAM-LIMIT
variable UNIT-FAM-ACC
PTR-VARIABLE UNIT-FAM-MAP-CUR
PTR-VARIABLE UNIT-FAM-KEY-AT

: UFP@ ( ptr u8 n -- n ) cells + CELL-VIEW @ ;
: UFP! ( n ptr u8 n -- ) cells + CELL-VIEW ! ;
\ A FILE family plan is a cell row in mapped storage whose UFP.NEXT, UFP.BLOB,
\ UFP.SRC, UFP.KEYS, UFP.FMAP and UFP.CMAP cells hold byte addresses.
CAST: UFP-CELLS ( ptr u8 -- ptr ptr u8 )
: UFP-PTR@ ( ptr u8 n -- ptr u8 ) cells + UFP-CELLS @ ;
: UFP-PTR! ( ptr u8 ptr u8 n -- ) cells + UFP-CELLS ! ;
: UNIT-FAM-RAW@ ( n -- ptr u8 ) cells UNIT-FAM-RAW + 0 ptr-field @ ;
: UNIT-FAM-RAW! ( ptr u8 n -- ) cells UNIT-FAM-RAW + 0 ptr-field ! ;
: UFP-SBASE@ ( ptr u8 n -- n ) UFP.SBASE + UFP@ ;
: UFP-COUNT@ ( ptr u8 n -- n ) UFP.COUNT + UFP@ ;
: UFP-TBASE@ ( ptr u8 n -- n ) UFP.TBASE + UFP@ ;

: UNIT-FAM-READ ( n -- ptr u8 ) {: n:n :}
   n 0 < n UNIT-FAM-LIMIT @ UNIT-FAM-IN @ - > or IF
      s" tfam: truncated FILE family delta" REG-AOT-REFUSE THEN
   UNIT-FAM-IN @ dup n + UNIT-FAM-IN ! ;

: UNIT-FAM-CELL ( -- n ) 8 UNIT-FAM-READ REG-AOT-U64@ ;

: UNIT-FAM-STR-LEN+ ( n n -- n ) {: sum:n len:n :}
   len 0 < len UNIT-FAM-LIMIT @ UNIT-FAM-IN @ - sum - > or IF
      s" tfam: invalid FILE family string length" REG-AOT-REFUSE THEN
   sum len + ;

: UNIT-FAM-RAW-ROW ( n n -- ptr n ) {: k:n i:n :}
   k UNIT-FAM-RAW@ i k REG-AOT-WIDTH * + CELL-VIEW ;

: UNIT-FAM-STRING-BYTES ( ptr u8 -- n ) {: plan:ptr :}
   0
   plan 0 UFP-COUNT@ 0 ?do
      0 i UNIT-FAM-RAW-ROW {: row:ptr :}
      row TF.PKG-U @ UNIT-FAM-STR-LEN+
      row TF.NAME-U @ UNIT-FAM-STR-LEN+
   loop
   plan 2 UFP-COUNT@ 0 ?do
      2 i UNIT-FAM-RAW-ROW {: row:ptr :}
      row SV.NAME-U @ UNIT-FAM-STR-LEN+
      row SV.CTOR-PKG-U @ UNIT-FAM-STR-LEN+
   loop
   plan 3 UFP-COUNT@ 0 ?do
      3 i UNIT-FAM-RAW-ROW PF.NAME-U @ UNIT-FAM-STR-LEN+
   loop ;

: UNIT-FAM-EXTENT+ ( n n n n n -- n ) {: ext:n off:n len:n base:n bound:n :}
   len 0= IF ext EXIT THEN
   off base < off base - bound len - > or IF
      s" tfam: FILE family string lies outside its pool delta" REG-AOT-REFUSE THEN
   off base - len + ext max ;

\ The string section is the pool delta the carried strings tile, measured from
\ the source offsets their rows carry; the carried byte total bounds it. An
\ empty string claims no byte.
: UNIT-FAM-STRING-EXTENT ( ptr u8 -- n ) {: plan:ptr :}
   plan UNIT-FAM-STRING-BYTES {: bound:n :}
   plan 5 UFP-SBASE@ {: base:n :}
   0
   plan 0 UFP-COUNT@ 0 ?do
      0 i UNIT-FAM-RAW-ROW {: row:ptr :}
      row TF.PKG-OFF @ row TF.PKG-U @ base bound UNIT-FAM-EXTENT+
      row TF.NAME-OFF @ row TF.NAME-U @ base bound UNIT-FAM-EXTENT+
   loop
   plan 2 UFP-COUNT@ 0 ?do
      2 i UNIT-FAM-RAW-ROW {: row:ptr :}
      row SV.NAME-OFF @ row SV.NAME-U @ base bound UNIT-FAM-EXTENT+
      row SV.CTOR-PKG-OFF @ row SV.CTOR-PKG-U @ base bound UNIT-FAM-EXTENT+
   loop
   plan 3 UFP-COUNT@ 0 ?do
      3 i UNIT-FAM-RAW-ROW {: row:ptr :}
      row PF.NAME-OFF @ row PF.NAME-U @ base bound UNIT-FAM-EXTENT+
   loop ;

: UNIT-FAM-TARGET-BASE ( n -- n ) {: k:n :}
   k REG-AOT-COUNT UNIT-FAM-ACC !
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup k UFP-COUNT@ UNIT-FAM-ACC +!
      UFP.NEXT UFP-PTR@
   REPEAT drop UNIT-FAM-ACC @ ;

\ PF-TX-SERIAL once the plans prepared so far install.
: UNIT-FAM-TARGET-SERIAL ( -- n )
   PF-TX-SERIAL @ UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup UFP.SERIAL UFP@ rot + swap
      UFP.NEXT UFP-PTR@
   REPEAT drop ;

: UNIT-FAM-ROW ( ptr u8 n n -- ptr n ) {: plan:ptr k:n i:n :}
   plan UFP.BLOB UFP-PTR@ k REG-AOT-BLOB-STORE
   i k REG-AOT-WIDTH * + CELL-VIEW ;

: UNIT-FAM-PROJECT-VIEW ( n n n -- ptr u8 )
   {: k:n first:n count:n :}
   first k REG-AOT-COUNT <= IF
      count k REG-AOT-COUNT first - <= IF
         k REG-AOT-BASE-PTR first k REG-AOT-WIDTH * + EXIT
      THEN
   THEN
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup k UFP-TBASE@ {: base:n :}
      dup k UFP-COUNT@ {: n:n :}
      first base >= first base n + <= and
      count base n + first - <= and IF
         k first base - UNIT-FAM-ROW BYTE-VIEW EXIT
      THEN
      UFP.NEXT UFP-PTR@
   REPEAT drop
   s" tfam: FILE family reference has no projected row" REG-AOT-REFUSE ;

: UNIT-FAM-PLAN-HEADER ( ptr u8 -- ) {: plan:ptr :}
   plan UFP.BLOB UFP-PTR@ {: blob:ptr :}
   REG-AOT-N blob REG-AOT-U64!
   REG-AOT-N 0 ?do
      plan i UFP-TBASE@ blob i REG-AOT-ROW * 8 + + REG-AOT-U64!
      plan i UFP-COUNT@ blob i REG-AOT-ROW * 8 + 8 + + REG-AOT-U64!
      plan i UFP-COUNT@ i REG-AOT-WIDTH *
      blob i REG-AOT-ROW * 8 + 16 + + REG-AOT-U64!
   loop ;

: UNIT-FAM-PLAN-ROWS ( ptr u8 -- ) {: plan:ptr :}
   REG-AOT-N 0 ?do
      i 5 <> IF
         plan i UFP-COUNT@ i REG-AOT-WIDTH * {: bytes:n :}
         bytes 0 > IF
            i UNIT-FAM-RAW@
            plan UFP.BLOB UFP-PTR@ i REG-AOT-BLOB-STORE
            bytes ARENA-COPY
         THEN
      THEN
   loop ;

\ A FILE's strings were interned in declaration order into one pool delta, and
\ a family's constructor package once for all its variants. Each carried string
\ goes back to its source offset rebased into the target pool, so the section
\ and every offset are the cold build's; strings sharing bytes must agree.
: UNIT-FAM-PUT-STRING ( ptr n ptr u8 -- ) {: field:ptr plan:ptr :}
   UNIT-FAM-CELL {: len:n :}
   len field CELL + @ <> IF
      s" tfam: FILE family string disagrees with its row" REG-AOT-REFUSE THEN
   len UNIT-FAM-READ {: src:ptr :}
   field @ plan 5 UFP-SBASE@ - {: at:n :}
   at 0 < at plan 5 UFP-COUNT@ len - > or IF
      s" tfam: FILE family string lies outside its pool delta" REG-AOT-REFUSE THEN
   plan UFP.BLOB UFP-PTR@ 5 REG-AOT-BLOB-STORE at + {: dst:ptr :}
   len 0 ?do
      at i + UNIT-FAM-COVERED? IF
         src i + c@ dst i + c@ <> IF
            s" tfam: FILE family strings disagree where they share pool bytes"
            REG-AOT-REFUSE THEN
      ELSE
         src i + c@ dst i + c!   at i + UNIT-FAM-COVER!
      THEN
   loop
   at plan 5 UFP-TBASE@ + field ! ;

\ An empty constructor package is absent: SUMV-ADD leaves offset 0 and only a
\ public family's TDECL-CTOR-PUBLISH interns one.
: UNIT-FAM-PUT-CTOR-PKG ( ptr n ptr u8 -- ) {: field:ptr plan:ptr :}
   field CELL + @ 0 <> IF field plan UNIT-FAM-PUT-STRING EXIT THEN
   UNIT-FAM-CELL 0 <> IF
      s" tfam: FILE family string disagrees with its row" REG-AOT-REFUSE THEN
   field @ 0 <> IF
      s" tfam: FILE variant places an absent constructor package" REG-AOT-REFUSE THEN ;

: UNIT-FAM-PLAN-STRINGS ( ptr u8 -- ) {: plan:ptr :}
   plan 5 UFP-COUNT@ UNIT-FAM-COVER-OPEN
   plan 0 UFP-COUNT@ 0 ?do
      plan 0 i UNIT-FAM-ROW {: row:ptr :}
      row TF.PKG-OFF plan UNIT-FAM-PUT-STRING
      row TF.NAME-OFF plan UNIT-FAM-PUT-STRING
   loop
   plan 2 UFP-COUNT@ 0 ?do
      plan 2 i UNIT-FAM-ROW {: row:ptr :}
      row SV.NAME-OFF plan UNIT-FAM-PUT-STRING
      row SV.CTOR-PKG-OFF plan UNIT-FAM-PUT-CTOR-PKG
   loop
   plan 3 UFP-COUNT@ 0 ?do
      plan 3 i UNIT-FAM-ROW PF.NAME-OFF plan UNIT-FAM-PUT-STRING
   loop
   plan 5 UFP-COUNT@ UNIT-FAM-COVER-FULL? 0= IF
      s" tfam: FILE family strings do not fill their section" REG-AOT-REFUSE THEN ;

: UNIT-FAM-KEY-READ ( -- ptr u8 n )
   UNIT-FAM-CELL {: n:n :}
   n 0 <= IF s" tfam: empty FILE declaration key" REG-AOT-REFUSE THEN
   n UNIT-FAM-READ n ;

: UNIT-FAM-PREPARED-KEY ( ptr u8 n -- n bool ) {: key:ptr len:n :}
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup UFP.KEYS UFP-PTR@ UNIT-FAM-KEY-AT !
      dup 0 UFP-COUNT@ 0 ?do
         UNIT-FAM-KEY-AT @ REG-AOT-U64@ {: n:n :}
         UNIT-FAM-KEY-AT @ 8 + n key len CORE-STR= IF
            dup 0 UFP-TBASE@ i + nip RES-TRUE unloop EXIT
         THEN
         UNIT-FAM-KEY-AT @ 8 + n + UNIT-FAM-KEY-AT !
      loop
      UFP.NEXT UFP-PTR@
   REPEAT drop 0 RES-FALSE ;

: UNIT-FAM-LOOKUP-KEY ( ptr u8 n -- n bool ) {: key:ptr len:n :}
   key len FAMILY-BY-KEY-XT IF RES-TRUE EXIT THEN drop
   key len UNIT-FAM-PREPARED-KEY ;

: UNIT-FAM-ROW-NAME ( ptr u8 n n -- ptr u8 n )
   {: plan:ptr off:n len:n :}
   plan UFP.BLOB UFP-PTR@ 5 REG-AOT-BLOB-STORE
   off plan 5 UFP-TBASE@ - + len ;

: UNIT-FAM-PRIOR-KEY? ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr len:n keys:ptr count:n :}
   keys
   count 0 ?do
      dup REG-AOT-U64@ {: u:n :}
      dup 8 + u key len CORE-STR= IF
         drop RES-TRUE unloop EXIT THEN
      8 + u +
   loop drop RES-FALSE ;

: UNIT-FAM-READ-KEYS ( ptr u8 -- ) {: plan:ptr :}
   UNIT-FAM-IN @ plan UFP.KEYS UFP-PTR!
   plan 0 UFP-COUNT@ 0 ?do
      UNIT-FAM-KEY-READ {: key:ptr len:n :}
      key len plan UFP.KEYS UFP-PTR@ i UNIT-FAM-PRIOR-KEY? IF
         s" tfam: duplicate FILE family declaration key" REG-AOT-REFUSE
      THEN
      plan 0 i UNIT-FAM-ROW {: row:ptr :}
      key len
      plan row TF.PKG-OFF @ row TF.PKG-U @ UNIT-FAM-ROW-NAME
      plan row TF.NAME-OFF @ row TF.NAME-U @ UNIT-FAM-ROW-NAME
      FAMILY-KEY-MATCH-XT 0= IF
         s" tfam: FILE family key disagrees with its declaration" REG-AOT-REFUSE
      THEN
      key len UNIT-FAM-LOOKUP-KEY IF drop
         s" tfam: duplicate FILE family declaration key" REG-AOT-REFUSE
      THEN drop
   loop ;

: UNIT-FAM-MAP-PAIR ( n n ptr u8 n ptr u8 -- )
   {: source:n target:n key:ptr len:n plan:ptr :}
   source UNIT-FAM-MAP-CUR @ CELL-VIEW !
   target UNIT-FAM-MAP-CUR @ CELL + CELL-VIEW !
   key plan UFP.SRC UFP-PTR@ -
      UNIT-FAM-MAP-CUR @ 2 cells + CELL-VIEW !
   len UNIT-FAM-MAP-CUR @ 3 cells + CELL-VIEW !
   UNIT-FAM-MAP-CUR @ 4 cells + UNIT-FAM-MAP-CUR ! ;

: UNIT-FAM-MAP-ROOM ( ptr u8 -- ) {: plan:ptr :}
   UNIT-FAM-MAP-CUR @ 4 cells +
   plan UFP.BLOB UFP-PTR@ plan UFP.CAP UFP@ + > IF
      s" tfam: FILE family map exceeds its delta" REG-AOT-REFUSE THEN ;

: UNIT-FAM-READ-FMAP ( ptr u8 -- ) {: plan:ptr :}
   UNIT-FAM-MAP-CUR @ plan UFP.FMAP UFP-PTR!
   UNIT-FAM-CELL {: n:n :}
   n 0 < n UNIT-FAM-LIMIT @ UNIT-FAM-IN @ - 16 / > or IF
      s" tfam: FILE family reference count exceeds its bytes" REG-AOT-REFUSE THEN
   n plan UFP.FN UFP!
   n 0 ?do
      UNIT-FAM-CELL {: source:n :}
      source UNIT-FAM-STABLE-N <
      source plan 0 UFP-SBASE@ >= or IF
         s" tfam: FILE family dependency has a local or fixed source ID" REG-AOT-REFUSE
      THEN
      UNIT-FAM-KEY-READ {: key:ptr len:n :}
      key len UNIT-FAM-LOOKUP-KEY 0= IF drop
         s" tfam: FILE family dependency is absent" UNIT-FAM-CHANGED THEN
      {: target:n :}
      UNIT-FAM-CELL {: bytes:n :}
      bytes 0 < IF
         s" tfam: FILE family contract length is negative" REG-AOT-REFUSE THEN
      bytes UNIT-FAM-READ {: contract:ptr :}
      target contract bytes UNIT-FAM-CONTRACT-CHECK-XT
      plan UNIT-FAM-MAP-ROOM
      source target key len plan UNIT-FAM-MAP-PAIR
   loop ;

: UNIT-FAM-READ-CMAP ( ptr u8 -- ) {: plan:ptr :}
   UNIT-FAM-MAP-CUR @ plan UFP.CMAP UFP-PTR!
   UNIT-FAM-CELL {: n:n :}
   n 0 < n UNIT-FAM-LIMIT @ UNIT-FAM-IN @ - 16 / > or IF
      s" tfam: FILE constructor reference count exceeds its bytes" REG-AOT-REFUSE THEN
   n plan UFP.CN UFP!
   n 0 ?do
      UNIT-FAM-CELL {: source:n :}
      UNIT-FAM-KEY-READ {: key:ptr len:n :}
      key len CON-BY-KEY-XT 0= IF drop
         key len CHECKER-REG:UNIT-CON-PROJECT-XT 0= IF drop
            s" tfam: FILE constructor dependency is absent" UNIT-FAM-CHANGED
         THEN
      THEN
      {: target:n :}
      UNIT-FAM-CELL {: bytes:n :}
      bytes 0 < IF
         s" tfam: FILE constructor contract length is negative" REG-AOT-REFUSE THEN
      bytes UNIT-FAM-READ {: contract:ptr :}
      target key len contract bytes UNIT-CON-CONTRACT-CHECK-XT
      plan UNIT-FAM-MAP-ROOM
      source target key len plan UNIT-FAM-MAP-PAIR
   loop ;

: UNIT-FAM-FIND-MAP ( n ptr u8 n -- n bool )
   {: source:n pairs:ptr count:n :}
   count 0 ?do
      pairs i 4 cells * + dup CELL-VIEW @ source = IF
         CELL + CELL-VIEW @ RES-TRUE unloop EXIT THEN drop
   loop 0 RES-FALSE ;

: UNIT-FAM-MAP-ID ( n -- n ) {: source:n :}
   source UNIT-FAM-STABLE-N < IF source EXIT THEN
   UNIT-FAM-CUR @ {: plan:ptr :}
   plan NULL-PTR = IF
      s" tfam: FILE family map has no active unit" REG-AOT-REFUSE THEN
   source plan 0 UFP-SBASE@ >=
   source plan 0 UFP-SBASE@ plan 0 UFP-COUNT@ + < and IF
      source plan 0 UFP-SBASE@ - plan 0 UFP-TBASE@ + EXIT THEN
   source plan UFP.FMAP UFP-PTR@ plan UFP.FN UFP@ UNIT-FAM-FIND-MAP
   IF EXIT THEN drop
   s" tfam: FILE family ID lacks an identity" REG-AOT-REFUSE ;

: UNIT-CON-MAP-ID ( n -- n ) {: source:n :}
   source CC-MAX < IF source EXIT THEN
   UNIT-FAM-CUR @ {: plan:ptr :}
   source plan UFP.CMAP UFP-PTR@ plan UFP.CN UFP@ UNIT-FAM-FIND-MAP
   IF EXIT THEN drop
   s" tfam: FILE constructor ID lacks an identity" REG-AOT-REFUSE ;

: UNIT-FAM-SUBID ( ptr u8 n n -- n ) {: plan:ptr k:n source:n :}
   source UNIT-FAM-STABLE k cells + @ < IF source EXIT THEN
   source plan k UFP-SBASE@ >=
   source plan k UFP-SBASE@ plan k UFP-COUNT@ + <= and IF
      source plan k UFP-SBASE@ - plan k UFP-TBASE@ + EXIT THEN
   s" tfam: FILE family row has an external store reference" REG-AOT-REFUSE ;

: UNIT-FAM-ROW-ID! ( ptr u8 n ptr n -- ) {: plan:ptr k:n field:ptr :}
   plan k field @ UNIT-FAM-SUBID field ! ;

\ An empty range (count 0) names no row; its writers store start 0, which
\ needs no translation.
: UNIT-FAM-REBASE ( ptr u8 -- ) {: plan:ptr :}
   plan 0 UFP-COUNT@ 0 ?do
      plan 0 i UNIT-FAM-ROW {: row:ptr :}
      plan 1 row TF.PK-START UNIT-FAM-ROW-ID!
      row TF.VAR-COUNT @ 0 > IF
         plan 2 row TF.VAR-START UNIT-FAM-ROW-ID! THEN
      row TF.FLD-COUNT @ 0 > IF
         plan 3 row TF.FLD-START UNIT-FAM-ROW-ID! THEN
      row TF.SCHEMA-ROOT @ 0 <> IF
         plan 7 row TF.SCHEMA-ROOT UNIT-FAM-ROW-ID! THEN
      0 row TF.TAILNEXT !
   loop
   plan 2 UFP-COUNT@ 0 ?do
      plan 2 i UNIT-FAM-ROW {: row:ptr :}
      row SV.FAM @ UNIT-FAM-MAP-ID row SV.FAM !
      row SV.SCH-COUNT @ 0 > IF
         plan 7 row SV.SCH-START UNIT-FAM-ROW-ID! THEN
   loop
   plan 3 UFP-COUNT@ 0 ?do
      plan 3 i UNIT-FAM-ROW {: row:ptr :}
      row PF.FAM @ UNIT-FAM-MAP-ID row PF.FAM !
      row PF.VAR @ -1 <> IF plan 2 row PF.VAR UNIT-FAM-ROW-ID! THEN
      plan 7 row PF.SCH UNIT-FAM-ROW-ID!
   loop
   plan 4 UFP-COUNT@ 0 ?do
      plan 4 i UNIT-FAM-ROW {: row:ptr :}
      row LAY.FAM @ UNIT-FAM-MAP-ID row LAY.FAM !
   loop
   plan 6 UFP-COUNT@ 0 ?do
      plan 6 i UNIT-FAM-ROW {: row:ptr :}
      row @ {: tag:n :}
      tag SCH-PTR = IF plan 6 row CELL + UNIT-FAM-ROW-ID! THEN
      tag SCH-APP = IF
         row CELL + @ UNIT-FAM-MAP-ID row CELL + !
         row 2 cells + @ 0 <> IF
            plan 7 row 2 cells + UNIT-FAM-ROW-ID! THEN
      THEN
      tag SCH-CON = IF
         row CELL + @ UNIT-CON-MAP-ID row CELL + ! THEN
      tag SCH-QUOT = IF
         row 3 cells + @ 0 <> IF
            plan 7 row 2 cells + UNIT-FAM-ROW-ID! THEN
      THEN
      tag SCH-ROW = IF
         row 2 cells + @ 0 <> IF
            plan 7 row CELL + UNIT-FAM-ROW-ID! THEN
      THEN
   loop
   plan 7 UFP-COUNT@ 0 ?do
      plan 7 i UNIT-FAM-ROW {: row:ptr :}
      plan 6 row UNIT-FAM-ROW-ID!
   loop ;

: UNIT-FAM-PLAN-OPEN ( ptr u8 n -- ptr u8 ) {: src:ptr u:n :}
   u 0= IF NULL-PTR EXIT THEN
   u 0 <
   u $7FFFFFFFFFFFFFFF REG-AOT-HDR - UFP.REC - 2 / > or IF
      s" tfam: FILE family delta is too large" REG-AOT-REFUSE THEN
   REG-AOT-HDR u 2 * + {: cap:n :}
   UFP.REC cap + ARENA-ALLOC-BYTES {: plan:ptr :}
   plan UNIT-FAM-OPEN !
   NULL-PTR plan UFP.NEXT UFP-PTR!  src plan UFP.SRC UFP-PTR!
   u plan UFP.U UFP!
   plan UFP.REC + plan UFP.BLOB UFP-PTR!
   cap plan UFP.CAP UFP!
   src UNIT-FAM-IN ! src u + UNIT-FAM-LIMIT !
   UNIT-FAM-CELL UNIT-FAM-MAGIC <> IF
      s" tfam: FILE family delta has an invalid format" REG-AOT-REFUSE THEN
   UNIT-FAM-CELL REG-AOT-N <> IF
      s" tfam: FILE family delta has an invalid store count" REG-AOT-REFUSE THEN
   UNIT-FAM-CELL {: serial:n :}
   serial 0 < IF
      s" tfam: FILE family delta has a negative transaction serial" REG-AOT-REFUSE THEN
   serial $7FFFFFFFFFFFFFFF UNIT-FAM-TARGET-SERIAL - > IF
      s" tfam: FILE family transaction serial overflows" REG-AOT-REFUSE THEN
   serial plan UFP.SERIAL UFP!
   REG-AOT-N 0 ?do
      UNIT-FAM-CELL {: base:n :}
      UNIT-FAM-CELL {: count:n :}
      UNIT-FAM-CELL UNIT-FAM-STABLE i cells + @ <> IF
         s" tfam: FILE family stable registry changed" REG-AOT-REFUSE THEN
      base 0 < count 0 < or IF
         s" tfam: FILE family delta has a negative extent" REG-AOT-REFUSE THEN
      base UNIT-FAM-STABLE i cells + @ < IF
         s" tfam: FILE family source predates the stable registry" REG-AOT-REFUSE THEN
      base $7FFFFFFFFFFFFFFF count - > IF
         s" tfam: FILE family source extent overflows" REG-AOT-REFUSE THEN
      base plan UFP.SBASE i + UFP!
      count plan UFP.COUNT i + UFP!
      i UNIT-FAM-TARGET-BASE {: target:n :}
      target $7FFFFFFFFFFFFFFF count - > IF
         s" tfam: FILE family target extent overflows" REG-AOT-REFUSE THEN
      target plan UFP.TBASE i + UFP!
   loop
   plan 5 UFP-COUNT@ 0 <> IF
      s" tfam: FILE family delta carries raw string offsets" REG-AOT-REFUSE THEN
   REG-AOT-N 0 ?do
      i 5 <> IF
         plan i UFP-COUNT@ {: count:n :}
         count UNIT-FAM-LIMIT @ UNIT-FAM-IN @ - i REG-AOT-WIDTH / > IF
            s" tfam: FILE family rows exceed their bytes" REG-AOT-REFUSE THEN
         count i REG-AOT-WIDTH * UNIT-FAM-READ
         i UNIT-FAM-RAW!
      THEN
   loop
   plan UNIT-FAM-STRING-EXTENT plan UFP.COUNT 5 + UFP!
   plan UNIT-FAM-PLAN-HEADER
   plan UNIT-FAM-PLAN-ROWS
   plan UNIT-FAM-PLAN-STRINGS
   plan UNIT-FAM-READ-KEYS
   plan UFP.BLOB UFP-PTR@ REG-AOT-HDR +
   REG-AOT-N 0 ?do
      plan i UFP-COUNT@ i REG-AOT-WIDTH * +
   loop UNIT-FAM-MAP-CUR !
   plan UNIT-FAM-READ-FMAP
   plan UNIT-FAM-READ-CMAP
   UNIT-FAM-IN @ UNIT-FAM-LIMIT @ <> IF
      s" tfam: FILE family delta has trailing bytes" REG-AOT-REFUSE THEN
   NULL-PTR UNIT-FAM-OPEN !
   plan ;

: UNIT-FAM-PLAN-DROP ( ptr u8 -- ) {: plan:ptr :}
   plan plan UFP.CAP UFP@ UFP.REC + ASIG-RELEASE ;

: UNIT-FAM-PLAN-ADD ( ptr u8 -- ) {: plan:ptr :}
   UNIT-FAM-LAST @ NULL-PTR = IF plan UNIT-FAM-FIRST !
   ELSE plan UNIT-FAM-LAST @ UFP.NEXT UFP-PTR! THEN
   plan UNIT-FAM-LAST !
   plan UNIT-FAM-CUR ! ;

: UNIT-FAM-BEGIN ( -- )
   NULL-PTR UNIT-FAM-FIRST ! NULL-PTR UNIT-FAM-LAST !
   NULL-PTR UNIT-FAM-CUR ! NULL-PTR UNIT-FAM-OPEN !
   0 UNIT-FAM-PROJECT-ON ! ;

: UNIT-FAM-PREPARE ( ptr u8 n -- ptr u8 )
   [: 2dup UNIT-FAM-PLAN-OPEN UNIT-FAM-OPEN ! ;] catch {: rc:n :}
   2drop
   rc 0 <> IF
      UNIT-FAM-OPEN @ dup NULL-PTR <> IF UNIT-FAM-PLAN-DROP
         ELSE drop THEN
      NULL-PTR UNIT-FAM-OPEN !
      rc throw
   THEN
   UNIT-FAM-OPEN @ {: plan:ptr :}
   NULL-PTR UNIT-FAM-OPEN !
   plan NULL-PTR = IF NULL-PTR EXIT THEN
   plan UNIT-FAM-PLAN-ADD
   -1 UNIT-FAM-PROJECT-ON !
   plan UNIT-FAM-REBASE
   plan UFP.BLOB UFP-PTR@ plan UFP.CAP UFP@ REG-AOT-CHECK
   plan ;

: UNIT-FAM-VIEW ( ptr u8 -- ptr u8 n ) {: state:ptr :}
   state NULL-PTR = IF NULL-PTR 0 EXIT THEN
   state UFP.BLOB UFP-PTR@ state UFP.CAP UFP@ ;

\ Each imported variant's carried constructor cell (UNIT-FAM-CTOR-ORDS) becomes
\ what EACH answers for it with CTX: the checker checks the ordinal when it
\ prepares the unit and names the live symbol once the unit's symbols are
\ installed, before UNIT-FAM-TAKE copies the rows. 0 stays absent.
: UNIT-FAM-CTORS ( ptr u8 ptr u8 [ ptr u8 n -- n ] -- ) {: plan:ptr ctx:ptr each :}
   plan NULL-PTR = IF EXIT THEN
   plan 2 UFP-COUNT@ 0 ?do
      plan 2 i UNIT-FAM-ROW {: row:ptr :}
      row SV.CTOR-SYM @ {: v:n :}
      v 0 <> IF ctx v each execute row SV.CTOR-SYM ! THEN
   loop ;

\ The lookup indexes over the family, variant and field stores take the
\ plan's rows as a declaration does: each store the plan grows gets its index
\ built and sized for the grown capacity before its count moves, and each new
\ row pushed in id order after every store holds its rows.
: UNIT-FAM-INDEX-ENSURE ( ptr u8 -- ) {: plan:ptr :}
   plan 0 UFP-COUNT@ 0 > IF TFX-ENSURE THEN
   plan 2 UFP-COUNT@ 0 > IF VNX-ENSURE THEN
   plan 3 UFP-COUNT@ 0 > IF FVX-ENSURE THEN ;

: UNIT-FAM-INDEX-PUSH ( ptr u8 -- ) {: plan:ptr :}
   plan 0 UFP-COUNT@ 0 > IF
      TFAM-N @ plan 0 UFP-TBASE@ ?do i TFX-PUSH loop
      TFAM-N @ TFX-HI !
   THEN
   plan 2 UFP-COUNT@ 0 > IF
      SUMV-N @ plan 2 UFP-TBASE@ ?do i VNX-PUSH loop
      SUMV-N @ VNX-HI !
   THEN
   plan 3 UFP-COUNT@ 0 > IF
      PF-N @ plan 3 UFP-TBASE@ ?do i FVX-PUSH loop
      PF-N @ FVX-HI !
   THEN ;

: UNIT-FAM-TAKE ( ptr u8 -- ) {: plan:ptr :}
   plan NULL-PTR = IF EXIT THEN
   REG-AOT-N 0 ?do
      plan i UFP-TBASE@ i REG-AOT-COUNT <> IF
         s" tfam: FILE family installation order changed" REG-AOT-REFUSE THEN
      i plan i UFP-TBASE@ plan i UFP-COUNT@ + REG-AOT-ROOM
   loop
   plan UNIT-FAM-INDEX-ENSURE
   plan UFP.SERIAL UFP@ PF-TX-SERIAL +!
   REG-AOT-N 0 ?do
      plan i UFP-COUNT@ i REG-AOT-WIDTH * {: bytes:n :}
      bytes 0 > IF
         plan UFP.BLOB UFP-PTR@ i REG-AOT-BLOB-STORE
         i REG-AOT-BASE-PTR plan i UFP-TBASE@ i REG-AOT-WIDTH * +
         bytes ARENA-COPY
         \ Only a store the plan grows moves its count: the field store's
         \ setter also moves PF-N, which inside a declaration would retire
         \ that declaration's provisional rows.
         i plan i UFP-TBASE@ plan i UFP-COUNT@ + REG-AOT-COUNT!
      THEN
   loop
   plan UNIT-FAM-INDEX-PUSH
   TFM-EPOCH+ 0 SVX-GEN ! SVX-STAMP ;

: UNIT-FAM-END ( -- )
   0 UNIT-FAM-PROJECT-ON ! NULL-PTR UNIT-FAM-CUR !
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup UFP.NEXT UFP-PTR@ swap
      UNIT-FAM-PLAN-DROP
   REPEAT drop
   NULL-PTR UNIT-FAM-FIRST ! NULL-PTR UNIT-FAM-LAST !
   UNIT-FAM-SCRATCH-RESET ;

\ Exact semantic contracts for family dependencies. Source indices, interned
\ offsets, diagnostic spans and derived lookup links are intentionally absent;
\ names, parameter kinds, layout, fields and recursively expanded schemas are
\ compared byte for byte. A schema application carries the defining FILE key
\ of its family, so a different declaration cannot impersonate that edge.
PTR-VARIABLE UNIT-FAM-CON-P
variable UNIT-FAM-CON-N
variable UNIT-FAM-CON-LIMIT
variable UNIT-FAM-CON-MODE
create UNIT-FAM-CON-CELL 8 allot

: UNIT-FAM-CON-OUT ( ptr u8 n -- ) {: src:ptr len:n :}
   len 0 < IF s" tfam: negative FILE family contract extent" REG-AOT-REFUSE THEN
   UNIT-FAM-CON-MODE @ 0 <> IF
      UNIT-FAM-CON-MODE @ 2 = IF
         len UNIT-FAM-CON-LIMIT @ UNIT-FAM-CON-N @ - > IF
            s" tfam: FILE family contract is truncated" REG-AOT-REFUSE THEN
         src len UNIT-FAM-CON-P @ UNIT-FAM-CON-N @ + len CORE-STR= 0= IF
            s" tfam: FILE family dependency changed semantics" UNIT-FAM-CHANGED
         THEN
      ELSE
         src UNIT-FAM-CON-P @ UNIT-FAM-CON-N @ + len ARENA-COPY
      THEN
   THEN
   len UNIT-FAM-CON-N +! ;

: UNIT-FAM-CON-CELL+ ( n -- )
   UNIT-FAM-CON-CELL REG-AOT-U64!
   UNIT-FAM-CON-CELL 8 UNIT-FAM-CON-OUT ;

: UNIT-FAM-CON-STR ( ptr u8 n -- ) {: a:ptr u:n :}
   u UNIT-FAM-CON-CELL+
   a u UNIT-FAM-CON-OUT ;

: UNIT-FAM-CON-COUNT ( n -- n ) {: k:n :}
   k REG-AOT-COUNT UNIT-FAM-ACC !
   UNIT-FAM-PROJECT-ON @ IF
      UNIT-FAM-FIRST @
      BEGIN dup NULL-PTR <> WHILE
         dup k UFP-COUNT@ UNIT-FAM-ACC +!
         UFP.NEXT UFP-PTR@
      REPEAT drop
   THEN UNIT-FAM-ACC @ ;

: UNIT-FAM-CON-ROW ( n n -- ptr n ) {: k:n id:n :}
   UNIT-FAM-PROJECT-ON @ IF
      k id 1 UNIT-FAM-PROJECT-VIEW CELL-VIEW EXIT THEN
   id 0 < id k REG-LIVE-COUNT >= or IF
      s" tfam: FILE contract row is outside the registry" REG-AOT-REFUSE THEN
   k REG-AOT-BASE-PTR id k REG-AOT-WIDTH * + CELL-VIEW ;

: UNIT-FAM-CON-NAME ( n n -- ) {: off:n len:n :}
   UNIT-FAM-PROJECT-ON @ IF
      5 off len UNIT-FAM-PROJECT-VIEW len UNIT-FAM-CON-STR EXIT THEN
   off len TF-OFF$ UNIT-FAM-CON-STR ;

: UNIT-FAM-CON-PLAN-KEY ( n -- ptr u8 n bool ) {: id:n :}
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup 0 UFP-TBASE@ {: base:n :}
      dup 0 UFP-COUNT@ {: count:n :}
      id base >= id base count + < and IF
         dup UFP.KEYS UFP-PTR@ UNIT-FAM-KEY-AT !
         id base - 0 ?do
            UNIT-FAM-KEY-AT @ dup REG-AOT-U64@ 8 + + UNIT-FAM-KEY-AT !
         loop
         drop UNIT-FAM-KEY-AT @ dup REG-AOT-U64@ {: n:n :}
         8 + n RES-TRUE EXIT
      THEN
      UFP.NEXT UFP-PTR@
   REPEAT drop NULL-PTR 0 RES-FALSE ;

\ While checking (mode 2), an absent identity is a changed dependency.
: UNIT-FAM-CON-ABSENT ( ptr u8 n -- )
   UNIT-FAM-CON-MODE @ 2 = IF UNIT-FAM-CHANGED THEN
   2drop CHECKER-REG:UNIT-BAD ;


: UNIT-FAM-CON-FKEY ( n -- ptr u8 n ) {: id:n :}
   id TFAM-N @ < IF
      id FAMILY-KEY-XT IF EXIT THEN 2drop
   THEN
   id UNIT-FAM-CON-PLAN-KEY IF EXIT THEN 2drop
   s" tfam: FILE contract family identity is absent" UNIT-FAM-CON-ABSENT ;

: UNIT-FAM-CON-CKEY ( n -- ptr u8 n ) {: id:n :}
   id CTN @ < IF
      id CON-KEY-XT IF EXIT THEN 2drop
   THEN
   UNIT-FAM-FIRST @
   BEGIN dup NULL-PTR <> WHILE
      dup UFP.CMAP UFP-PTR@ {: map:ptr :}
      dup UFP.CN UFP@ 0 ?do
         map i 4 cells * + {: pair:ptr :}
         pair CELL + CELL-VIEW @ id = IF
            dup UFP.SRC UFP-PTR@
            pair 2 cells + CELL-VIEW @ +
            pair 3 cells + CELL-VIEW @ rot drop unloop EXIT
         THEN
      loop
      UFP.NEXT UFP-PTR@
   REPEAT drop
   s" tfam: FILE contract constructor identity is absent" UNIT-FAM-CON-ABSENT ;

: UNIT-FAM-CON-FAMREF ( n -- ) {: id:n :}
   id UNIT-FAM-STABLE-N < IF
      0 UNIT-FAM-CON-CELL+ id UNIT-FAM-CON-CELL+ EXIT THEN
   1 UNIT-FAM-CON-CELL+
   id UNIT-FAM-CON-FKEY UNIT-FAM-CON-STR ;

: UNIT-FAM-CON-CONREF ( n -- ) {: id:n :}
   id CC-MAX < IF
      0 UNIT-FAM-CON-CELL+ id UNIT-FAM-CON-CELL+ EXIT THEN
   1 UNIT-FAM-CON-CELL+
   id UNIT-FAM-CON-CKEY UNIT-FAM-CON-STR ;

: UNIT-FAM-CON-SCHEMA ( n -- ) {: id:n :}
   6 id UNIT-FAM-CON-ROW {: row:ptr :}
   row @ {: tag:n :}
   tag UNIT-FAM-CON-CELL+
   tag SCH-PARAM = IF row CELL + @ UNIT-FAM-CON-CELL+ EXIT THEN
   tag SCH-CON = IF row CELL + @ UNIT-FAM-CON-CONREF EXIT THEN
   tag SCH-PTR = IF row CELL + @ recurse EXIT THEN
   tag SCH-APP = IF
      row CELL + @ UNIT-FAM-CON-FAMREF
      row 3 cells + @ {: count:n :}
      count UNIT-FAM-CON-CELL+
      count 0 ?do
         7 row 2 cells + @ i + UNIT-FAM-CON-ROW @ recurse
      loop EXIT
   THEN
   tag SCH-QUOT = IF
      row CELL + @ UNIT-FAM-CON-CELL+
      row 3 cells + @ {: count:n :}
      count UNIT-FAM-CON-CELL+
      count 0 ?do
         7 row 2 cells + @ i + UNIT-FAM-CON-ROW @ recurse
      loop EXIT
   THEN
   tag SCH-ROW = IF
      row 2 cells + @ {: count:n :}
      count UNIT-FAM-CON-CELL+
      count 0 ?do
         7 row CELL + @ i + UNIT-FAM-CON-ROW @ recurse
      loop EXIT
   THEN
   s" tfam: FILE contract has invalid schema" REG-AOT-REFUSE ;

: UNIT-FAM-CON-FAMILY ( n -- ) {: id:n :}
   0 id UNIT-FAM-CON-ROW {: row:ptr :}
   row TF.PKG-OFF @ row TF.PKG-U @ UNIT-FAM-CON-NAME
   row TF.NAME-OFF @ row TF.NAME-U @ UNIT-FAM-CON-NAME
   row TF.VIS @ UNIT-FAM-CON-CELL+
   row TF.ARITY @ UNIT-FAM-CON-CELL+
   row TF.KIND @ UNIT-FAM-CON-CELL+
   row TF.LAYOUT @ UNIT-FAM-CON-CELL+
   row TF.SLOTS @ UNIT-FAM-CON-CELL+
   row TF.TAGW @ UNIT-FAM-CON-CELL+
   row TF.DERIVE @ UNIT-FAM-CON-CELL+
   row TF.OPAQUE @ UNIT-FAM-CON-CELL+
   row TF.SCHEMA-ROOT @ 0 <> IF
      1 UNIT-FAM-CON-CELL+
      7 row TF.SCHEMA-ROOT @ UNIT-FAM-CON-ROW @ UNIT-FAM-CON-SCHEMA
   ELSE 0 UNIT-FAM-CON-CELL+ THEN
   row TF.ARITY @ 0 ?do
      1 row TF.PK-START @ i + UNIT-FAM-CON-ROW @ UNIT-FAM-CON-CELL+
   loop
   row TF.VAR-COUNT @ UNIT-FAM-CON-CELL+
   row TF.VAR-COUNT @ 0 ?do
      2 row TF.VAR-START @ i + UNIT-FAM-CON-ROW {: variant:ptr :}
      variant SV.NAME-OFF @ variant SV.NAME-U @ UNIT-FAM-CON-NAME
      variant SV.CTOR-PKG-OFF @ variant SV.CTOR-PKG-U @ UNIT-FAM-CON-NAME
      variant SV.TAG @ UNIT-FAM-CON-CELL+
      variant SV.PAYCELLS @ UNIT-FAM-CON-CELL+
      variant SV.SCH-COUNT @ UNIT-FAM-CON-CELL+
      variant SV.SCH-COUNT @ 0 ?do
         7 variant SV.SCH-START @ i + UNIT-FAM-CON-ROW @
         UNIT-FAM-CON-SCHEMA
      loop
   loop
   row TF.FLD-COUNT @ UNIT-FAM-CON-CELL+
   row TF.FLD-COUNT @ 0 ?do
      3 row TF.FLD-START @ i + UNIT-FAM-CON-ROW {: field:ptr :}
      field PF.NAME-OFF @ field PF.NAME-U @ UNIT-FAM-CON-NAME
      field PF.VAR @ -1 = IF -1 ELSE
         field PF.VAR @ row TF.VAR-START @ - THEN UNIT-FAM-CON-CELL+
      field PF.SLOT @ UNIT-FAM-CON-CELL+
      field PF.CELLS @ UNIT-FAM-CON-CELL+
      field PF.BYTE-OFF @ UNIT-FAM-CON-CELL+
      field PF.BYTES @ UNIT-FAM-CON-CELL+
      field PF.ALIGN @ UNIT-FAM-CON-CELL+
      field PF.FLAGS @ UNIT-FAM-CON-CELL+
      7 field PF.SCH @ UNIT-FAM-CON-ROW @ UNIT-FAM-CON-SCHEMA
   loop
   0
   4 UNIT-FAM-CON-COUNT 0 ?do
      4 i UNIT-FAM-CON-ROW LAY.FAM @ id = IF 1+ THEN
   loop
   dup UNIT-FAM-CON-CELL+
   drop
   4 UNIT-FAM-CON-COUNT 0 ?do
      4 i UNIT-FAM-CON-ROW {: lay:ptr :}
      lay LAY.FAM @ id = IF
         lay LAY.POLICY @ UNIT-FAM-CON-CELL+
         lay LAY.SIZE @ UNIT-FAM-CON-CELL+
         lay LAY.ALIGN @ UNIT-FAM-CON-CELL+
         lay LAY.TAGW @ UNIT-FAM-CON-CELL+
      THEN
   loop ;

: UNIT-FAM-CONTRACT-SIZE ( n -- n )
   0 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N !
   UNIT-FAM-CON-FAMILY UNIT-FAM-CON-N @ ;

: UNIT-FAM-CONTRACT-WRITE ( n ptr u8 -- ptr u8 )
   {: id:n dst:ptr :}
   1 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N ! dst UNIT-FAM-CON-P !
   id UNIT-FAM-CON-FAMILY
   dst UNIT-FAM-CON-N @ + ;

: UNIT-FAM-CONTRACT-CHECK ( n ptr u8 n -- )
   {: id:n src:ptr len:n :}
   2 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N !
   src UNIT-FAM-CON-P ! len UNIT-FAM-CON-LIMIT !
   id UNIT-FAM-CON-FAMILY
   UNIT-FAM-CON-N @ len <> IF
      s" tfam: FILE family contract has trailing bytes" REG-AOT-REFUSE THEN ;

: UNIT-CON-CONTRACT-CONTENT ( n -- ) {: id:n :}
   id CT-NAME$ UNIT-FAM-CON-STR
   id CT-PKG$ UNIT-FAM-CON-STR
   id CT-CLASS@ UNIT-FAM-CON-CELL+
   id CT-WIDTH@ UNIT-FAM-CON-CELL+
   id CT-SIGN@ UNIT-FAM-CON-CELL+ ;

: UNIT-CON-CONTRACT-SIZE ( n -- n )
   0 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N !
   UNIT-CON-CONTRACT-CONTENT UNIT-FAM-CON-N @ ;

: UNIT-CON-CONTRACT-WRITE ( n ptr u8 -- ptr u8 )
   {: id:n dst:ptr :}
   1 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N ! dst UNIT-FAM-CON-P !
   id UNIT-CON-CONTRACT-CONTENT
   dst UNIT-FAM-CON-N @ + ;

PTR-VARIABLE UNIT-CON-CON-IN
PTR-VARIABLE UNIT-CON-CON-END

: UNIT-CON-CON-READ ( n -- ptr u8 ) {: n:n :}
   n 0 < n UNIT-CON-CON-END @ UNIT-CON-CON-IN @ - > or IF
      s" tfam: malformed FILE constructor contract" REG-AOT-REFUSE THEN
   UNIT-CON-CON-IN @ dup n + UNIT-CON-CON-IN ! ;

: UNIT-CON-CON-CELL ( -- n ) 8 UNIT-CON-CON-READ REG-AOT-U64@ ;

: UNIT-CON-CON-STR ( -- ptr u8 n )
   UNIT-CON-CON-CELL {: len:n :}
   len UNIT-CON-CON-READ len ;

: UNIT-CON-CONTRACT-CHECK ( n ptr u8 n ptr u8 n -- )
   {: id:n key:ptr keyu:n src:ptr len:n :}
   id CTN @ < IF
      2 UNIT-FAM-CON-MODE ! 0 UNIT-FAM-CON-N !
      src UNIT-FAM-CON-P ! len UNIT-FAM-CON-LIMIT !
      id UNIT-CON-CONTRACT-CONTENT
      UNIT-FAM-CON-N @ len <> IF
         s" tfam: FILE constructor contract has trailing bytes" REG-AOT-REFUSE
      THEN EXIT
   THEN
   src UNIT-CON-CON-IN ! src len + UNIT-CON-CON-END !
   UNIT-CON-CON-STR {: name:ptr nameu:n :}
   UNIT-CON-CON-STR {: pkg:ptr pkgu:n :}
   UNIT-CON-CON-CELL CT-LINEAR <> IF
      s" tfam: FILE constructor class changed" REG-AOT-REFUSE THEN
   UNIT-CON-CON-CELL 64 <> IF
      s" tfam: FILE constructor width changed" REG-AOT-REFUSE THEN
   UNIT-CON-CON-CELL CS-NONE <> IF
      s" tfam: FILE constructor signedness changed" REG-AOT-REFUSE THEN
   UNIT-CON-CON-IN @ UNIT-CON-CON-END @ <> IF
      s" tfam: FILE constructor contract has trailing bytes" REG-AOT-REFUSE THEN
   key keyu name nameu pkg pkgu
      CHECKER-REG:UNIT-CON-PROJECT-CONTRACT-XT 0= IF
      s" tfam: FILE constructor name or package changed" REG-AOT-REFUSE THEN ;

\ The copied plans outlive parser and contract staging, including a capture
\ while projection is active. Clear only the staging at its owner boundaries.
: UNIT-FAM-SCRATCH-CLEAR ( -- )
   REG-AOT-N 0 ?do NULL-PTR i UNIT-FAM-RAW! loop
   NULL-PTR UNIT-FAM-IN ! NULL-PTR UNIT-FAM-LIMIT !
   0 UNIT-FAM-ACC !
   NULL-PTR UNIT-FAM-MAP-CUR ! NULL-PTR UNIT-FAM-KEY-AT !
   NULL-PTR UNIT-FAM-CON-P ! 0 UNIT-FAM-CON-N !
   0 UNIT-FAM-CON-LIMIT ! 0 UNIT-FAM-CON-MODE !
   0 UNIT-FAM-CON-CELL REG-AOT-U64!
   NULL-PTR UNIT-CON-CON-IN ! NULL-PTR UNIT-CON-CON-END !
   UNIT-FAM-COVER-RELEASE ;

: UNIT-FAM-SCRATCH-INSTALL ( -- )
   [: UNIT-FAM-SCRATCH-CLEAR ;] is UNIT-FAM-SCRATCH-RESET ;
UNIT-FAM-SCRATCH-INSTALL

: UNIT-FAM-CONTRACT-INSTALL ( -- )
   [: UNIT-FAM-CONTRACT-SIZE ;] is UNIT-FAM-CONTRACT-SIZE-XT
   [: UNIT-FAM-CONTRACT-WRITE ;] is UNIT-FAM-CONTRACT-WRITE-XT
   [: UNIT-FAM-CONTRACT-CHECK ;] is UNIT-FAM-CONTRACT-CHECK-XT
   [: UNIT-CON-CONTRACT-SIZE ;] is UNIT-CON-CONTRACT-SIZE-XT
   [: UNIT-CON-CONTRACT-WRITE ;] is UNIT-CON-CONTRACT-WRITE-XT
   [: UNIT-CON-CONTRACT-CHECK ;] is UNIT-CON-CONTRACT-CHECK-XT
   [: UNIT-FAM-PROJECT-VIEW ;] is UNIT-FAM-PROJECT-VIEW-XT ;
UNIT-FAM-CONTRACT-INSTALL

: UNIT-FAM-RESET-INSTALL ( -- ) [: UNIT-FAM-END ;] is UNIT-FAM-RESET-XT ;
UNIT-FAM-RESET-INSTALL

: UNIT-FAMILY-NAME-IN ( ptr u8 n ptr u8 n -- bool )
   {: name:ptr nameu:n src:ptr u:n :}
   u 0= IF RES-FALSE EXIT THEN
   src 0 REG-AOT-ROW@ drop + 0 ?do
      src u 0 i REG-AOT-ITEM {: row:ptr :}
      src u 5 row TF.NAME-OFF @ row TF.NAME-U @ REG-AOT-VIEW
      row TF.NAME-U @ name nameu CORE-STR= IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

: UNIT-FAMILY-NAME? ( ptr u8 n ptr u8 n -- bool )
   {: name:ptr nameu:n src:ptr u:n :}
   UNIT-FAM-PROJECT-ON @ IF
      UNIT-FAM-CUR @ {: plan:ptr :}
      name nameu plan UFP.BLOB UFP-PTR@ plan UFP.CAP UFP@
      UNIT-FAMILY-NAME-IN EXIT
   THEN
   name nameu src u UNIT-FAMILY-NAME-IN ;

\ The checker's friend hooks: checker.f loads before this file and reaches
\ family resolution and arities through these defers during signature parsing.
: TFAM-HOOK-INSTALL ( -- )
   [: TFAM-N@ ;] is CHECKER-REG:UNIT-TFAM-N@
   [: UNIT-FAM-MARK ;] is CHECKER-REG:UNIT-TFAM-MARK
   [: UNIT-FAM-SIZE ;] is CHECKER-REG:UNIT-TFAM-SIZE
   [: UNIT-FAM-SAVE ;] is CHECKER-REG:UNIT-TFAM-SAVE
   [: UNIT-FAM-REF+ ;] is CHECKER-REG:UNIT-TFAM-REF
   [: UNIT-FAM-MAP-ID ;] is CHECKER-REG:UNIT-TFAM-MAP
   [: UNIT-FAM-BEGIN ;] is CHECKER-REG:UNIT-TFAM-BEGIN
   [: UNIT-FAM-PREPARE ;] is CHECKER-REG:UNIT-TFAM-PREPARE
   [: UNIT-FAM-VIEW ;] is CHECKER-REG:UNIT-TFAM-VIEW
   [: UNIT-FAM-TAKE ;] is CHECKER-REG:UNIT-TFAM-TAKE
   [: UNIT-FAM-CTORS ;] is CHECKER-REG:UNIT-TFAM-CTORS
   [: UNIT-FAM-EXPORT-RESET ;] is CHECKER-REG:UNIT-TFAM-EXPORT-RESET
   [: UNIT-FAM-END ;] is CHECKER-REG:UNIT-TFAM-END
   [: UNIT-FAMILY-NAME? ;] is CK-UNIT-FAMILY?
   [: TFAM-SIG-RESOLVE ;] is TFAM-RESOLVE-XT
   [: TFAM-CLAIMED? ;] is TFAM-CLAIMED?-XT
   [: TFAM-CTOR-PKG? ;]    is CTOR-PKG?-XT     \ constructor-package reopen reject
   [: TFAM-CTOR-WORD? ;]   is CTOR-WORD?-XT    \ generated-word undefine reject
   [: TFAM-CTOR-EXTEND? ;] is CTOR-EXTEND?-XT  \ closed-package extra-tail reject
   [: TFAM-ARITY@ ;]  is TFAM-ARITY-XT
   [: TFAM-LAYOUT? ;] is TFAM-LAYOUT?-XT   \ checker reaches the layout kind for its fail-closed guard
   [: TFAM-CELL? ;]   is TFAM-CELL?-XT     \ nominal scalars: checker reaches the cell kind for LAYOUT-BUFFER admission + pointee governance
   [: TFAM-PKG$ ;]    is TFAM-PKG-XT       \ nominal CAST introduction belongs to the declaring package
   [: TFAM-OPAQUE? ;] is TFAM-OPAQUE?-XT   \ so does an OPAQUE structure's
   [: TFAM-WIDTH@ ;]  is TFAM-WIDTH-XT     \ checker reads DECLARED logical widths (params-as-cells) for the boot fallback
   [: TFAM-INST-WIDTH@ ;] is TFAM-INST-WIDTH-XT   \ arg-aware instantiated width for T-WIDTH and the WF fact surface
   [: INIT-RECORD? ;] is TFAM-INIT-RECORD-XT
   [: INIT-FIELD-NAME? ;] is TFAM-INIT-FIELD-XT
   [: TFAM-WIDTH-SLOT? ;] is TFAM-WIDTH-SLOT-XT   \ which argument slots that width reads: an open slot it does not read is still placeable
   [: TFAM-PK@ PK-SCOPE = ;] is TFAM-SCOPE-SLOT-XT
   [: TFAM-PK@ PK-REGION = ;] is TFAM-REGION-SLOT-XT
   [: 2dup TFAM-PK@ PK-TYPE = >r TFAM-WIDTH-SLOT? 0= r> and ;] is TFAM-TYPE-SLOT-XT
   [: TFAM-STORE-SLOT? ;] is TFAM-STORE-SLOT-XT   \ which argument slots reach storage: a pair of one demands both sides
   [: TFAM-CONSTRUCT-FAM ;]  is CONSTRUCT-FAM-XT   \ construct family resolution (active package only)
   [: TFAM-CONSTRUCT-STEP ;] is CONSTRUCT-STEP-XT  \ construct variant resolve + inline constructor effect
   [: TFAM-CTOR-STEP? ;]     is CTOR-STEP-XT        \ a generated-constructor call on a multi-cell layout arg takes the arg-aware step
   [: SUMV-FROM-CTOR-SYM nip ;] is CTOR-ROW?-XT   \ a generated constructor row moves exactly its payload (checker.f LIN-EFF-PASS)
   [: TFAM-MATCH-FAM ;]     is MATCH-FAM-XT     \ MATCH family resolution (signature scope)
   [: TFAM-MATCH-VARIANT ;] is MATCH-VAR-XT     \ MATCH branch variant resolve
   [: SUMV-TAG@ ;]          is MATCH-VTAG-XT    \ variant id -> declaration-order tag (bitset index)
   [: TFL-VPADS ;]          is VARIANT-PADS-XT  \ family id + variant id -> declared pads (the tape's construct and arm events)
   [: TFAM-VAR-COUNT@ ;]    is MATCH-VCOUNT-XT  \ exhaustiveness domain size
   [: TFAM-MATCH-PAY ;]     is MATCH-PAY-XT     \ branch payload row from the scrutinee's args
   [: TFAM-FIELD-PROJ ;]    is FIELD-PROJ-XT ;  \ instantiated field type for the FAMILY:FIELD projection window
TFAM-HOOK-INSTALL

;package

;using
;using

\ The native lowerer and the checker must resolve names and interpret the
\ resulting family/variant ids in this same registry, including after handover.
package CHECKER-REG
' TFL-MATCH-FAM?       DECLARATIONS FAMILY-MATCH-OFF + xt!
' TFL-CON-FAM?         DECLARATIONS FAMILY-CON-OFF + xt!
' TFAM:TFL-VAR?        DECLARATIONS FAMILY-VARIANT-OFF + xt!
' TFAM:TFAM-SLOTS@     DECLARATIONS FAMILY-SLOTS-OFF + xt!
' TFAM:TFAM-VAR-COUNT@ DECLARATIONS FAMILY-VARIANTS-OFF + xt!
' TFAM-NAME$           DECLARATIONS FAMILY-NAME-OFF + xt!
' TFAM:SUMV-TAG@       DECLARATIONS VARIANT-TAG-OFF + xt!
' TFAM:TFL-VPADS       DECLARATIONS VARIANT-PADS-OFF + xt!
' TFAM:SUMV-PAYCELLS@  DECLARATIONS VARIANT-PAY-CELLS-OFF + xt!
' TFAM:SUMV-PAY-N      DECLARATIONS VARIANT-PAY-TERMS-OFF + xt!
' TFL-CVAR?           DECLARATIONS CHECKER-OWNER-ABI:FAMILY-CVAR-OFF + xt!
;package
