split-str
\ type-family.f: package TFAM, the family (TFAM), variant (SUMV), product-field
\ and logical-layout registries for the checker; loaded unchecked in the checker
\ prefix after type-schema.f. Records key on (package, canonical lowercase tail);
\ names are byte offsets into one string pool, so records hold only integers.
\ Registries roll back through RBF-PUSH / RBF-POP.

\ TFAM's words are private except its error codes; an engine package takes one
\ with `EXPORT TFAM:NAME`. The inner packages this file opens (TYPE-NAME,
\ TYPE-FIELD, CHECKER-DECL-FRAME, SCHEMA-REG) are top-level packages, not
\ parts of TFAM, since a `package` inside an open one opens a part of it: each
\ inner block closes TFAM, reopens it after, and EXPORTs what it calls.
package TFAM
private
EXPORT SCHEMA-REG:SCH-APP
EXPORT SCHEMA-REG:SCH-CON
EXPORT SCHEMA-REG:SCH-N
EXPORT SCHEMA-REG:SCH-PARAM
EXPORT SCHEMA-REG:SCH-PTR
EXPORT SCHEMA-REG:SCH-QUOT
EXPORT SCHEMA-REG:SCH-QUOT-ROWS
EXPORT SCHEMA-REG:SCH-ROOT-N
EXPORT SCHEMA-REG:SCH-ROW
EXPORT SCHEMA-REG:SCHEMA-A@
EXPORT SCHEMA-REG:SCHEMA-APP
EXPORT SCHEMA-REG:SCHEMA-APP?
EXPORT SCHEMA-REG:SCHEMA-B@
EXPORT SCHEMA-REG:SCHEMA-C@
EXPORT SCHEMA-REG:SCHEMA-CON
EXPORT SCHEMA-REG:SCHEMA-CON?
EXPORT SCHEMA-REG:SCHEMA-NODE-OK?
EXPORT SCHEMA-REG:SCHEMA-PARAM
EXPORT SCHEMA-REG:SCHEMA-PARAM?
EXPORT SCHEMA-REG:SCHEMA-PTR
EXPORT SCHEMA-REG:SCHEMA-PTR?
EXPORT SCHEMA-REG:SCHEMA-QUOT
EXPORT SCHEMA-REG:SCHEMA-QUOT-DIN@
EXPORT SCHEMA-REG:SCHEMA-QUOT-DOUT@
EXPORT SCHEMA-REG:SCHEMA-QUOT-FACTS@
EXPORT SCHEMA-REG:SCHEMA-QUOT-HASR@
EXPORT SCHEMA-REG:SCHEMA-QUOT-RIN@
EXPORT SCHEMA-REG:SCHEMA-QUOT-ROUT@
EXPORT SCHEMA-REG:SCHEMA-QUOT-ROW@
EXPORT SCHEMA-REG:SCHEMA-QUOT?
EXPORT SCHEMA-REG:SCHEMA-ROOT+
EXPORT SCHEMA-REG:SCHEMA-ROOT-N@
EXPORT SCHEMA-REG:SCHEMA-ROOT@
EXPORT SCHEMA-REG:SCHEMA-ROW
EXPORT SCHEMA-REG:SCHEMA-ROW-COUNT@
EXPORT SCHEMA-REG:SCHEMA-ROW-ELEM@
EXPORT SCHEMA-REG:SCHEMA-ROW-OK?
EXPORT SCHEMA-REG:SCHEMA-ROW-START@
EXPORT SCHEMA-REG:SCHEMA-ROW?
EXPORT SCHEMA-REG:SCHEMA-TAG@
EXPORT SCHEMA-REG:SCHEMA-XT?

\ --- kind of a family (what its values are).

0 constant TK-CELL          \ scalar-cell family (no ADT layout)
1 constant TK-PRODUCT       \ record / struct
2 constant TK-SUM           \ tagged sum
3 constant TK-ENUM          \ payload-free sum

4 constant TK-EVIDENCE      \ compile-only evidence family
4 constant TK-MAX

\ --- layout policy (physical representation); the default is TL-STACK-CELL-TAG.

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
4 constant PK-SCOPE        \ non-value lexical scope identity
5 constant PK-REGION       \ non-value host allocation identity

3 constant PK-EVIDENCE
5 constant PK-MAX

CELL constant TAGW-CELL     \ default tag width: one stack cell

\ Visibility reuses the checker's package modes so registry and package scope
\ never diverge: CHECKER-PACKAGE-PRIVATE (1) / CHECKER-PACKAGE-PUBLIC (2).

public

\ --- named reject codes (thrown, caught by parser/CHECK path or unit `catch`).
7101 constant E-TFAM-CASE     \ uppercase/mixed-case or non-canonical tail token
7102 constant E-TFAM-DUP      \ duplicate tail within the same package
7106 constant E-TFAM-AMBIG    \ two other-package public families tie on a tail
\ E-TDECL-DEF-NAME: a family, variant or public accessor field name that is
\ a keyword or reads as a number.
7123 constant E-TDECL-DEF-NAME

\ 7122 is layout-buffer.f's E-LAYOUT-BOUNDS.
7125 constant E-PF-NAME       \ reserved field tail

private

variable TF-PUB               \ private first-public-match accumulator (-1 = none)

\ declaration parameter spelling.  This is the one reversible alphabet used by
\ every declaration parser and signature generator.  The concrete scalar tokens
\ f, n, and r are deliberately absent, so their meaning never depends on arity.
\ The registry may store wider internal schemas; this count bounds only the
\ source-language spelling of positional declaration parameters.

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

\ shared string pool. Names are interned as byte offsets; offsets stay valid
\ across a pool grow, so no stored offset ever needs rebasing.

512 constant TF-STR-INIT         \ bytes; holds the built-ins' names (TF-CAP-INIT)
variable TF-STR-CAP-V   TF-STR-INIT TF-STR-CAP-V !
: TF-STR-CAP ( -- n ) TF-STR-CAP-V @ ;
create TF-STR-BOOT   TF-STR-INIT allot
PERSISTED-PTR-VARIABLE TF-STR-P   TF-STR-BOOT TF-STR-P !
: TF-STR ( -- ^u8 ) TF-STR-P @ ;

variable TF-STR-U   0 TF-STR-U !

EXPORT CHECKER-REG:GROW

: TF-STR-GROW ( n -- ) {: need:n :}
   need TF-STR-CAP-V @ 2 * max {: nc:n :}
   TF-STR-P  TF-STR-CAP-V @  nc  GROW
   nc TF-STR-CAP-V ! ;
: TF-STR-ENSURE ( n -- ) {: add:n :}
   TF-STR-U @ add + TF-STR-CAP-V @ <= IF exit THEN
   TF-STR-U @ add + TF-STR-GROW ;
: TF-STR-SCRUB ( n n -- ) {: lo:n hi:n :} \ zero the pool bytes [lo,hi); see PF-SCRUB
   hi lo ?do 0 TF-STR i + c! loop ;

: TF-INTERN ( str -- n ) {: a:^ u:n :}
   u TF-STR-ENSURE                        \ grow first, then cache a stable base
   TF-STR-U @ {: off:n :}
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@   TF-STR off + i + c!
      i 1 +
   REPEAT drop
   TF-STR-U @ u + TF-STR-U !
   off ;
: TF-OFF$ ( n n -- str ) {: off:n u:n :}
   TF-STR off + u ;

\ canonical tail validation. Declarations accept only lowercase tokens; the
\ registry never folds case, so an uppercase/mixed-case token is rejected here.

: TF-LOWER? ( n -- bool ) {: c:n :} c 97 >= c 122 <= and ;   \ a-z
: TF-DIGIT? ( n -- bool ) {: c:n :} c 48 >= c 57 <= and ;    \ 0-9
: TF-TAILBYTE? ( n -- bool ) {: c:n :}   \ lowercase, digit, or internal hyphen
   c TF-LOWER? IF RES-TRUE EXIT THEN
   c TF-DIGIT? IF RES-TRUE EXIT THEN
   c 45 = ;                                                  \ '-'

: TF-HIDDEN? ( str -- bool ) {: a:^ u:n :}   \ compaction-hidden field name
   u 0= IF RES-FALSE EXIT THEN
   a c@ 64 = ;                                              \ leading '@'

: TF-HYPHEN-BAD? ( str n -- bool ) {: a:^ u:n i:n :}   \ '-' at an edge or doubled
   i 0 = IF RES-TRUE EXIT THEN                      \ leading '-a'
   i u 1 - = IF RES-TRUE EXIT THEN                  \ trailing 'a-'
   a i + 1 - c@ 45 = ;                              \ previous byte also '-' -> 'a--b'

EXPORT CHECK:ARENA-BYTES-ENSURE
EXPORT CHECK:ARENA-ROWS-ENSURE
EXPORT CHECK:BYTE-SPAN?
EXPORT CHECK:C2-INIT-FAM
EXPORT CHECK:C2-MUT-FAM
EXPORT CHECK:C2-READ-FAM
EXPORT CHECK:C2-RECORDS-FAM
EXPORT CHECK:C2-STOW-FAM
EXPORT CHECK:CHECKER-AUTH-PACKAGE$
EXPORT CHECK:CHECKER-AUTH-PACKAGE-ACTIVE?
EXPORT CHECK:CHECKER-AUTH-PACKAGE-MODE@
EXPORT CHECK:CHECKER-COLON-SCAN
EXPORT CHECK:CHECKER-PKG-SYM
EXPORT CHECK:CHECKER-SYM-IDENTITY
EXPORT CHECK:CHECKER-TYPE-MARK-COVERS?
EXPORT CHECK:CHECKER-USING$
EXPORT CHECK:CHECKER-USING-N
EXPORT CHECK:CK-REC-WID
EXPORT CHECK:CONSTRUCT-DECL-LAYOUT
EXPORT CHECK:CONSTRUCT-WIDE-STAGED-REJECT
EXPORT CHECK:CT-FIND
EXPORT CHECK:CT-LINEAR?
EXPORT CHECK:CT-LIVE?
EXPORT CHECK:CT-NAME$
EXPORT CHECK:CTL-STATABLE
EXPORT CHECK:CTOR-EXTEND?-XT
EXPORT CHECK:CTOR-OWNER-XT
EXPORT CHECK:CTOR-ROW?-XT
EXPORT CHECK:CTOR-WORD?-XT
EXPORT CHECK:CW-MEMBER-OFFSET
EXPORT CHECK:CW-MEMBER-SPAN
EXPORT CHECK:CW-MEMBER-STRIDE
EXPORT CHECK:CWIN-ADD
EXPORT CHECK:DCUR
EXPORT CHECK:EFFECT-TYPE-FAM
EXPORT CHECK:ENTRIES-PENDING
EXPORT CHECK:EXT-PROD-FAM
EXPORT CHECK:EXT-REDX-FAM
EXPORT CHECK:FAILSET
EXPORT CHECK:FIELD-FAM
EXPORT CHECK:FRESH
EXPORT CHECK:HIDDEN-PARAM?
EXPORT CHECK:HIDDEN-SLOT@
EXPORT CHECK:HIDX-ENSURE
EXPORT CHECK:HIDX-GEN
EXPORT CHECK:IDX-HEAD!
EXPORT CHECK:IDX-HEAD@
EXPORT CHECK:IDX-HEADS-CLEAR
EXPORT CHECK:IDX-SYM-OK
EXPORT CHECK:ISVAR
EXPORT CHECK:LAYOUT-WIDTH-OPEN?
EXPORT CHECK:LOAN-FIELD-FAM
EXPORT CHECK:MDIAG-COMPILE!
EXPORT CHECK:MK-ATOM
EXPORT CHECK:MK-CON
EXPORT CHECK:MK-LOGICAL
EXPORT CHECK:MK-PARAM
EXPORT CHECK:MK-PTR
EXPORT CHECK:MK-PUSH
EXPORT CHECK:MK-QROW
EXPORT CHECK:MK-QUOT
EXPORT CHECK:MK-ROW
EXPORT CHECK:MK-VAR
EXPORT CHECK:MWIN-CELLS!
EXPORT CHECK:NEW
EXPORT CHECK:NMAP-RESET
EXPORT CHECK:NOMINAL-PROVIDER
EXPORT CHECK:NSCOPE-RAISE
EXPORT CHECK:OPAQUE-XT
EXPORT CHECK:OWN-TYPE-N
EXPORT CHECK:P>REST
EXPORT CHECK:P>TYPE
EXPORT CHECK:PARAM-SCR
EXPORT CHECK:PARAM-SCR+
EXPORT CHECK:PARAM-SCR-N
EXPORT CHECK:PARAM>ARG
EXPORT CHECK:PARAM>ARGC
EXPORT CHECK:PARAM>FAM
EXPORT CHECK:PAY
EXPORT CHECK:PTR>INNER
EXPORT CHECK:PTX-CPCOMMITTED-FAM
EXPORT CHECK:PTX-CPREADY-FAM
EXPORT CHECK:PTX-TILE-FAM
EXPORT CHECK:PTX-UNIFORM-FAM
EXPORT CHECK:PUSH-LOGICAL
EXPORT CHECK:PUSH-TERM
EXPORT CHECK:Q-FACTS!
EXPORT CHECK:Q>BLOCK
EXPORT CHECK:Q>FACTS
EXPORT CHECK:R-RES
EXPORT CHECK:REC-IX
EXPORT CHECK:REC-ON
EXPORT CHECK:RECORDED-STEP
EXPORT CHECK:REG-EXT-AOT-FAMILY-NAME-XT
EXPORT CHECK:REG-GROW1
EXPORT CHECK:SEED-TERM?
EXPORT CHECK:SIG-FACTS-MODE
EXPORT CHECK:SLOT>VAR
EXPORT CHECK:STR-FAM
EXPORT CHECK:T-RES
EXPORT CHECK:T-UNSTALE
EXPORT CHECK:T-WIDTH
EXPORT CHECK:TAG
EXPORT CHECK:TFAM-ARITY-XT
EXPORT CHECK:TFAM-CELL?-XT
EXPORT CHECK:TFAM-CLAIMED?-XT
EXPORT CHECK:TFAM-CON-LIN-XT
EXPORT CHECK:TFAM-INST-WIDTH-XT
EXPORT CHECK:TFAM-LAYOUT?-XT
EXPORT CHECK:TFAM-LIN-N-XT
EXPORT CHECK:TFAM-MAY-LIN-XT
EXPORT CHECK:TFAM-MEMBERS-XT
EXPORT CHECK:TFAM-PKG-XT
EXPORT CHECK:TFAM-REGION-SLOT-XT
EXPORT CHECK:TFAM-RESOLVE-XT
EXPORT CHECK:TFAM-SCOPE-SLOT-XT
EXPORT CHECK:TFAM-STORE-SLOT-XT
EXPORT CHECK:TFAM-TYPE-SLOT-XT
EXPORT CHECK:TFAM-WIDTH-SLOT-XT
EXPORT CHECK:TFAM-WIDTH-XT
EXPORT CHECK:TKF
EXPORT CHECK:TKFU
EXPORT CHECK:TOKFOLD
EXPORT CHECK:TOP-STEP
EXPORT CHECK:TVK-CELL!
EXPORT CHECK:TVK-REGION!
EXPORT CHECK:TVK-SCOPE!
EXPORT CHECK:TYPE-MAY-ARRIVE-XT
EXPORT CHECK:TYPE-REP-CLOSED?
EXPORT CHECK:UNMAKE-SEED-LAYOUT
EXPORT CHECK:WIDTH-FAM
EXPORT CHECK:WRITE-WINDOW-CK

: TF-CANON? ( str -- bool ) {: a:^ u:n :}    \ tailbytes + internal single '-' + >=1 letter
   u 0= IF RES-FALSE EXIT THEN
   a u BYTE-SPAN? 0= IF RES-FALSE EXIT THEN      \ a false length is no tail
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
: TF-REQUIRE-CANON ( str -- )   \ reject uppercase/mixed-case at declaration
   TF-CANON? 0= IF E-TFAM-CASE throw THEN ;

\ param-kind pool (one PK-* cell per parameter of every family, contiguous).

128 constant TF-PK-INIT          \ holds the built-ins' param kinds (TF-CAP-INIT)
variable TF-PK-CAP-V   TF-PK-INIT TF-PK-CAP-V !
: TF-PK-CAP ( -- n ) TF-PK-CAP-V @ ;
create TF-PK-BOOT   TF-PK-INIT cells allot
PERSISTED-PTR-VARIABLE TF-PK-P   TF-PK-BOOT TF-PK-P !
: TF-PK-BASE ( -- ^n ) TF-PK-P @ ;

variable TF-PK-N   0 TF-PK-N !

: TF-PK-GROW ( n -- ) {: need:n :}
   need TF-PK-CAP-V @ 2 * max {: nc:n :}
   TF-PK-P  TF-PK-CAP-V @ cells  nc cells  GROW
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
16 cells constant TF.DERIVE-OFF
\ TF.TAILNEXT: next family id (+1) in this row's tail bucket, 0 at the end of the
\ chain. It is the registry's own lookup index (TFX-* below), which is why it
\ lives in the record rather than in a side table that could drift from it.
17 cells constant TF.TAILNEXT-OFF
\ TF.OPAQUE: nonzero when the family's generated words take the private-family
\ placement (the OPAQUE header clause, structure-decl.f OPAQUE-CLAUSE).
18 cells constant TF.OPAQUE-OFF
\ TF.ENTRY: the family's type entry, its record index + 1; 0 when it has none.
19 cells constant TF.ENTRY-OFF
\ TF.AT-SYM: the checker symbol of the family's generated AT, 0 when it has none.
20 cells constant TF.AT-SYM-OFF

21 cells constant TF-REC

: TF.PKG-OFF ( ^a -- ^a ) TF.PKG-OFF-AT + ;
: TF.PKG-U ( ^a -- ^a ) TF.PKG-U-OFF + ;
: TF.VIS ( ^a -- ^a ) TF.VIS-OFF + ;

: TF.NAME-OFF ( ^a -- ^a ) TF.NAME-OFF-AT + ;
: TF.NAME-U ( ^a -- ^a ) TF.NAME-U-OFF + ;

: TF.ARITY ( ^a -- ^a ) TF.ARITY-OFF + ;
: TF.KIND ( ^a -- ^a ) TF.KIND-OFF + ;
: TF.PK-START ( ^a -- ^a ) TF.PK-START-OFF + ;
: TF.LAYOUT ( ^a -- ^a ) TF.LAYOUT-OFF + ;
: TF.SLOTS ( ^a -- ^a ) TF.SLOTS-OFF + ;
: TF.VAR-START ( ^a -- ^a ) TF.VAR-START-OFF + ;
: TF.VAR-COUNT ( ^a -- ^a ) TF.VAR-COUNT-OFF + ;
: TF.FLD-START ( ^a -- ^a ) TF.FLD-START-OFF + ;
: TF.FLD-COUNT ( ^a -- ^a ) TF.FLD-COUNT-OFF + ;
: TF.TAGW ( ^a -- ^a ) TF.TAGW-OFF + ;
: TF.SCHEMA-ROOT ( ^a -- ^a ) TF.SCHEMA-ROOT-OFF + ;
: TF.DERIVE ( ^a -- ^a ) TF.DERIVE-OFF + ;
: TF.TAILNEXT ( ^a -- ^a ) TF.TAILNEXT-OFF + ;
: TF.OPAQUE ( ^a -- ^a ) TF.OPAQUE-OFF + ;
: TF.ENTRY ( ^a -- ^a ) TF.ENTRY-OFF + ;
: TF.AT-SYM ( ^a -- ^a ) TF.AT-SYM-OFF + ;


\ Each boot store holds the built-in families this file declares, so a written
\ program keeps them in its DATA: the linker refuses a cell into mapped memory.
64 constant TF-CAP-INIT
variable TF-CAP-V   TF-CAP-INIT TF-CAP-V !
: TF-CAP ( -- n ) TF-CAP-V @ ;
create TF-A-BOOT   TF-CAP-INIT TF-REC * allot
PERSISTED-PTR-VARIABLE TF-A-P   TF-A-BOOT TF-A-P !
: TF-BASE ( -- ^n ) TF-A-P @ ;

variable TFAM-N   0 TFAM-N !

: TF-GROW ( n -- ) {: need:n :}
   need TF-CAP-V @ 2 * max {: nc:n :}
   TF-A-P  TF-CAP-V @ TF-REC *  nc TF-REC *  GROW
   nc TF-CAP-V ! ;
: TF-ENSURE ( -- )
   TFAM-N @ TF-CAP-V @ < IF exit THEN
   TFAM-N @ 1 + TF-GROW ;
: TF-SCRUB ( n n -- ) {: lo:n hi:n :}     \ zero the family records [lo,hi); see PF-SCRUB
   TF-BASE  lo TF-REC CELL / *  hi TF-REC CELL / *  ARENA-CELLS-ZERO ;

: TF-REC@ ( n -- ^n ) {: id:n :}
   id TF-REC * TF-BASE + ;

: TFAM-N@ ( -- n ) TFAM-N @ ;            \ family high-water (rollback/tests)

\ --- read-only queries.
: TFAM-PKG$ ( n -- str ) {: id:n :}
   id TF-REC@ {: r:^ :}  r TF.PKG-OFF @ r TF.PKG-U @ TF-OFF$ ;

;package

package TFAM
private

: TFAM-NAME$ ( n -- str ) {: id:n :}
   id TF-REC@ {: r:^ :}  r TF.NAME-OFF @ r TF.NAME-U @ TF-OFF$ ;

: TFAM-VIS@ ( n -- n ) TF-REC@ TF.VIS @ ;
: TFAM-ARITY@ ( n -- n ) TF-REC@ TF.ARITY @ ;
: TFAM-KIND@ ( n -- n ) TF-REC@ TF.KIND @ ;
: TFAM-LAYOUT-POLICY@ ( n -- n ) TF-REC@ TF.LAYOUT @ ;
: TFAM-NICHE? ( n -- bool ) TFAM-LAYOUT-POLICY@ TL-NICHE = ;
: TFAM-SLOTS@ ( n -- n ) TF-REC@ TF.SLOTS @ ;
: TFAM-VAR-START@ ( n -- n ) TF-REC@ TF.VAR-START @ ;
: TFAM-VAR-COUNT@ ( n -- n ) TF-REC@ TF.VAR-COUNT @ ;
: TFAM-FLD-START@ ( n -- n ) TF-REC@ TF.FLD-START @ ;
: TFAM-FLD-COUNT@ ( n -- n ) TF-REC@ TF.FLD-COUNT @ ;
: TFAM-TAGW@ ( n -- n ) TF-REC@ TF.TAGW @ ;
: TFAM-PK@ ( n n -- n ) {: id:n i:n :}
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
4 constant DRV-ADDR              \ address surface: one accessor per field
8 constant DRV-INIT              \ initialized-record field accessors
16 constant DRV-TAG              \ checked conversion from a tag, TAG>

: TFAM-DERIVE@ ( n -- n ) TF-REC@ TF.DERIVE @ ;

: TFAM-DERIVE-EQ! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-EQ or swap ! ;
: TFAM-DERIVE-EQ? ( n -- bool ) TFAM-DERIVE@ DRV-EQ and 0 <> ;
: TFAM-DERIVE-HASH! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-HASH or swap ! ;
: TFAM-DERIVE-HASH? ( n -- bool ) TFAM-DERIVE@ DRV-HASH and 0 <> ;
: TFAM-DERIVE-ADDR! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-ADDR or swap ! ;
: TFAM-DERIVE-ADDR? ( n -- bool ) TFAM-DERIVE@ DRV-ADDR and 0 <> ;
: TFAM-DERIVE-INIT! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-INIT or swap ! ;
: TFAM-DERIVE-INIT? ( n -- bool ) TFAM-DERIVE@ DRV-INIT and 0 <> ;
: TFAM-DERIVE-TAG! ( n -- ) TF-REC@ TF.DERIVE dup @ DRV-TAG or swap ! ;
: TFAM-DERIVE-TAG? ( n -- bool ) TFAM-DERIVE@ DRV-TAG and 0 <> ;
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
\ kind. Only TFAM-LAYOUT! reaches them (the POLICY clause refuses boxed).

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

variable TFM-EPOCH   1 TFM-EPOCH !

: TFM-EPOCH+ ( -- ) TFM-EPOCH @ 1 + TFM-EPOCH ! ;

\ --- field mutators.
: TFAM-LAYOUT! ( n n -- ) {: id:n p:n :}
   p 0 < p TL-MAX > or IF s" tfam: layout policy out of range" EXIT-STATUS:INTERNAL die THEN
   p id TF-REC@ TF.LAYOUT !
   TFM-EPOCH+ ;
: TFAM-SLOTS! ( n n -- ) swap TF-REC@ TF.SLOTS !  TFM-EPOCH+ ;

\ A STRUCTURE learns its kind at its close: one with no field is a cell family.
: TFAM-KIND! ( n n -- ) swap TF-REC@ TF.KIND !  TFM-EPOCH+ ;

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

EXPORT CHECKER-REG:SCRATCH{
EXPORT CHECKER-REG:}SCRATCH
\ The kind memo's current generation, 0 for none, and the last one handed out.
SCRATCH{
variable PF-GEN   variable PF-GEN-N
}SCRATCH
: PF-GEN-NEW ( -- ) PF-GEN-N @ 1 + dup PF-GEN-N !  PF-GEN ! ;

: TFAM-PK! ( n n n -- ) {: id:n i:n k:n :}
   k 0 < k PK-MAX > or IF s" tfam: parameter kind out of range" EXIT-STATUS:INTERNAL die THEN
   \ A memoized kind may read this one, so a change starts a new generation.
   PF-GEN @ 0 <> IF id i TFAM-PK@ k <> IF PF-GEN-NEW THEN THEN
   k id TF-REC@ TF.PK-START @ i + cells TF-PK-BASE + ! ;

\ --- matching and lookup.

: TFAM-PKG-MATCH? ( str n -- bool ) {: pa:^ pu:n id:n :}
   id TFAM-PKG$ pa pu CORE-STR=CI ;
: TFAM-NAME-MATCH? ( str n -- bool ) {: na:^ nu:n id:n :}
   id TFAM-NAME$ na nu CORE-STR= ;

\ --- tail index (TFX). TFAM-FIND-IN asks about one tail, so it walks the rows
\ sharing its hash bucket instead of [0, TFAM-N). Rows are pushed on their
\ bucket in id order and retired newest-first, so a retired row is always at its
\ bucket head and rewinding TFAM-N is the chain's retirement. The bucket count
\ follows the record arena's capacity; a capacity change or a rewind outside the
\ retire seam rebuilds the whole index.

128 constant TFX-SLOTS-INIT             \ power of two; grown to keep load <= 1/2

variable TFX-SLOTS-V   TFX-SLOTS-INIT TFX-SLOTS-V !

: TFX-SLOTS ( -- n ) TFX-SLOTS-V @ ;

create TFX-A-BOOT   TFX-SLOTS-INIT cells allot
PERSISTED-PTR-VARIABLE TFX-A-P   TFX-A-BOOT TFX-A-P !
: TFX-BASE ( -- ^n ) TFX-A-P @ ;
variable TFX-READY   0 TFX-READY !

variable TFX-HI      0 TFX-HI !

variable TFX-CAP     0 TFX-CAP !      \ the TF-CAP the buckets were sized for
variable TFX-I                \ private build/retire index
variable TFX-CUR              \ private bucket-walk cursor

: TFX-BKT ( n -- ^n ) {: slot:n :}
   slot cells TFX-BASE + ;

\ Hashes the exact bytes, because TFAM-NAME-MATCH? compares the exact bytes:
\ registry tails are canonical lowercase, so a non-canonical token misses here
\ for the same reason it misses the comparison.
: TFX-HASH ( str -- n ) {: na:^ nu:n :}
   FNV:BASIS na nu FNV:BYTES TFX-SLOTS 1 - and ;

: TFX-ROW-BKT ( n -- ^n ) {: id:n :}
   id TFAM-NAME$ TFX-HASH TFX-BKT ;

: TFX-PUSH ( n -- ) {: id:n :}
   id TFX-ROW-BKT {: b:^ :}
   b @ id TF-REC@ TF.TAILNEXT !
   id 1 + b ! ;

: TFX-POP ( n -- ) {: id:n :}
   id TFX-ROW-BKT {: b:^ :}
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
   TFX-A-P  TFX-SLOTS cells  need cells  GROW
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
\ popped row is at its bucket head.
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

: TFAM-FIND-IN ( str str -- n bool )
   {: pa:^ pu:n na:^ nu:n :}
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

\ --- declaration. Storage only ever sees canonical lowercase tails.

: TFAM-KIND-VALID? ( n -- bool ) {: k:n :} k 0 >= k TK-MAX <= and ;
: TFAM-PK-RESERVE ( n -- ) {: k:n :}        \ default every parameter to PK-CELL
   0 BEGIN dup k < WHILE
      PK-CELL TF-PK+
      1 +
   REPEAT drop ;

: TFAM-DECL ( str n str n n -- n )
   {: pa:^ pu:n vis:n na:^ nu:n arity:n kind:n :}
   WRITE-WINDOW-CK                        \ src/core/checker.f WRITE-WINDOW
   na nu TF-REQUIRE-CANON
   arity 0 < IF s" tfam: negative family arity" EXIT-STATUS:INTERNAL die THEN
   kind TFAM-KIND-VALID? 0= IF s" tfam: family kind out of range" EXIT-STATUS:INTERNAL die THEN
   pa pu na nu TFAM-FIND-IN IF drop E-TFAM-DUP throw THEN drop   \ FIND returns (id-or-0 flag); drop the id
   TF-ENSURE
   TFX-ENSURE                             \ before the slot is committed; see TFX-ADD
   TFAM-N @ {: id:n :}
   pa pu TF-INTERN {: poff:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + TFAM-N !                        \ commit the slot before writing it (VREC style)
   id TF-REC@ {: r:^ :}
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
   0 r TF.DERIVE !
   0 r TF.TAILNEXT !
   0 r TF.OPAQUE !
   0 r TF.ENTRY !
   0 r TF.AT-SYM !
   id TFX-ADD                             \ the row's tail is written: it can be found now
   arity TFAM-PK-RESERVE
   id ;
NOMINAL-PROVIDER

\ SUMV: sum/enum variant records, keyed by (family-id, variant tail).

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

: SV.FAM ( ^a -- ^a ) SV.FAM-OFF + ;
: SV.NAME-OFF ( ^a -- ^a ) SV.NAME-OFF-AT + ;
: SV.NAME-U ( ^a -- ^a ) SV.NAME-U-OFF + ;
: SV.TAG ( ^a -- ^a ) SV.TAG-OFF + ;
: SV.SCH-START ( ^a -- ^a ) SV.SCH-START-OFF + ;
: SV.SCH-COUNT ( ^a -- ^a ) SV.SCH-COUNT-OFF + ;
: SV.PAYCELLS ( ^a -- ^a ) SV.PAYCELLS-OFF + ;
: SV.CTOR-SYM ( ^a -- ^a ) SV.CTOR-SYM-OFF + ;
: SV.CTOR-PKG-OFF ( ^a -- ^a ) SV.CTOR-PKG-OFF-AT + ;
: SV.CTOR-PKG-U ( ^a -- ^a ) SV.CTOR-PKG-U-OFF + ;


4 constant SUMV-CAP-INIT
variable SUMV-CAP-V   SUMV-CAP-INIT SUMV-CAP-V !
: SUMV-CAP ( -- n ) SUMV-CAP-V @ ;
create SUMV-A-BOOT   SUMV-CAP-INIT SUMV-REC * allot
PERSISTED-PTR-VARIABLE SUMV-A-P   SUMV-A-BOOT SUMV-A-P !
: SUMV-BASE ( -- ^n ) SUMV-A-P @ ;

variable SUMV-N   0 SUMV-N !

: SUMV-GROW ( n -- ) {: need:n :}
   need SUMV-CAP-V @ 2 * max {: nc:n :}
   SUMV-A-P  SUMV-CAP-V @ SUMV-REC *  nc SUMV-REC *  GROW
   nc SUMV-CAP-V ! ;
: SUMV-ENSURE ( -- )
   SUMV-N @ SUMV-CAP-V @ < IF exit THEN
   SUMV-N @ 1 + SUMV-GROW ;
: SUMV-SCRUB ( n n -- ) {: lo:n hi:n :}   \ zero the variant records [lo,hi); see PF-SCRUB
   SUMV-BASE  lo SUMV-REC CELL / *  hi SUMV-REC CELL / *  ARENA-CELLS-ZERO ;
: SUMV-REC@ ( n -- ^n ) {: id:n :}
   id SUMV-REC * SUMV-BASE + ;

: SUMV-FAM@ ( n -- n ) SUMV-REC@ SV.FAM @ ;
: SUMV-NAME$ ( n -- str ) {: id:n :}
   id SUMV-REC@ {: r:^ :}  r SV.NAME-OFF @ r SV.NAME-U @ TF-OFF$ ;
: SUMV-TAG@ ( n -- n ) SUMV-REC@ SV.TAG @ ;
: SUMV-SCH-START@ ( n -- n ) SUMV-REC@ SV.SCH-START @ ;
: SUMV-SCH-COUNT@ ( n -- n ) SUMV-REC@ SV.SCH-COUNT @ ;

: SUMV-RAW-PAYCELLS@ ( n -- n ) SUMV-REC@ SV.PAYCELLS @ ;

: SUMV-N@ ( -- n ) SUMV-N @ ;

\ generated-constructor metadata. A PUBLIC family stores its derived
\ constructor package name (interned offset+len) in every variant; a private
\ family's stays zero. The checker symbol for each variant's generated word,
\ public or private, lands in SV.CTOR-SYM. All three cells are integers /
\ interned offsets, so the existing SUMV snapshot bake persists them verbatim.
: SUMV-CTOR-PKG$ ( n -- str ) {: id:n :}
   id SUMV-REC@ {: r:^ :}  r SV.CTOR-PKG-OFF @ r SV.CTOR-PKG-U @ TF-OFF$ ;
: SUMV-CTOR-SYM@ ( n -- n ) SUMV-REC@ SV.CTOR-SYM @ ;

\ --- constructor-symbol index (SVX). SUMV-FROM-CTOR-SYM runs on every resolved
\ call token and answers no for almost all, so it must not walk the variant
\ store. It answers the lowest variant id carrying the symbol; a symbol is
\ written once per variant, so a per-symbol head that keeps its first writer
\ reproduces it. Variant rows leave only by SUMV-N rewinding, lowest id first,
\ so a head at a retired row means the symbol has no row left and SVX-TRUNCATE
\ needs no back-link. SVX-HI is the SUMV-N the index covers; every seam that
\ moves SUMV-N moves it (SUMV-ADD, SVX-TRUNCATE, TFAM-RESET), so a difference
\ catches a rewind outside those seams. SVX-GEN catches the shared mapping being
\ dropped or re-laid out.

variable SVX-GEN
variable SVX-HI
0 SVX-GEN !   0 SVX-HI !

: SVX@ ( n -- n ) HT-SVX IDX-HEAD@ ;

: SVX! ( n n -- ) HT-SVX IDX-HEAD! ;

: SVX-STAMP ( -- ) SUMV-N @ SVX-HI ! ;

: SVX-SYNC ( -- )
   SUMV-N @ SVX-HI @ < IF 0 SVX-GEN ! THEN ;

\ First writer wins, so the head stays the lowest id the SVX header promises
\ even if two rows ever carried one symbol.
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

: SVX-ENSURE ( -- )
   HIDX-ENSURE
   SVX-SYNC
   SVX-GEN @ HIDX-GEN @ <> IF SVX-BUILD THEN ;

\ SVX-TRUNCATE ( n -- ) : called before SUMV-N rewinds to `newn`.
\ An index at a stale generation has nothing to retire and no rewind left for
\ SVX-SYNC to catch (SVX-GEN already forces SVX-BUILD), so its mark follows the
\ store: a cache hit leaves the index stale where a cold load leaves it current,
\ and both must end at the same SVX-HI.

: SVX-TRUNCATE ( n -- ) {: newn:n :}
   SVX-GEN @ HIDX-GEN @ <> IF newn SVX-HI ! EXIT THEN
   SUMV-N @ newn ?do
      i SUMV-CTOR-SYM@ {: sym:n :}
      sym 0 <> IF
         sym SVX@ i 1 + = IF 0 sym SVX! THEN
      THEN
   loop
   newn SVX-HI ! ;

: SUMV-CTOR-SYM! ( n n -- ) {: vid:n sym:n :}
   SVX-ENSURE
   sym vid SUMV-REC@ SV.CTOR-SYM !
   vid sym SVX-LINK ;

\ --- variant indexes (VNX, CPX). SUMV-FIND wants one tail in one family and the
\ qualified constructor recognizer one tail under one constructor package;
\ generating an ENUM's constructors asks each once per variant, so a store walk
\ makes the declaration quadratic in its variants. VNX chains rows by the hash
\ of their family and folded tail: the family is in the key because every
\ structure has the tails `make` and `unmake`, which by the tail alone put a
\ row per structure in one bucket. CPX chains the rows that have a constructor
\ package by the hash of the folded package and folded tail. The exact
\ comparisons still reject a non-canonical spelling, and every
\ case-insensitive match sits in one bucket. Rows join VNX in id order in
\ SUMV-ADD and CPX in id order as SUMV-CTOR-PKG! records their packages, and
\ TFAM-REWIND retires them newest first, so a retired row is always its
\ buckets' head. Buckets and links are process-local: a capture drops them and
\ a capacity change rebuilds them. A store that moved outside those seams is
\ rebuilt by the next lookup.

8 constant VNX-SLOTS-INIT               \ power of two; grown to keep load <= 1/2
variable VNX-SLOTS-V   VNX-SLOTS-INIT VNX-SLOTS-V !
: VNX-SLOTS ( -- n ) VNX-SLOTS-V @ ;
create VNX-B-BOOT   VNX-SLOTS-INIT cells allot
PERSISTED-PTR-VARIABLE VNX-B-P   VNX-B-BOOT VNX-B-P !
create VNX-L-BOOT   SUMV-CAP-INIT cells allot
PERSISTED-PTR-VARIABLE VNX-L-P   VNX-L-BOOT VNX-L-P !
create CPX-B-BOOT   VNX-SLOTS-INIT cells allot
PERSISTED-PTR-VARIABLE CPX-B-P   CPX-B-BOOT CPX-B-P !
create CPX-L-BOOT   SUMV-CAP-INIT cells allot
PERSISTED-PTR-VARIABLE CPX-L-P   CPX-L-BOOT CPX-L-P !
variable VNX-LCAP   SUMV-CAP-INIT VNX-LCAP !   \ rows each link array holds
variable VNX-READY   0 VNX-READY !

variable VNX-HI      0 VNX-HI !      \ SUMV-N the chains cover

variable VNX-CAP     0 VNX-CAP !     \ SUMV-CAP the buckets were sized for
variable VNX-CUR              \ private bucket-walk cursor

: VNX-BKT ( n -- ^n ) {: slot:n :}   \ bucket head: row id + 1, 0 = empty
   slot cells VNX-B-P @ + ;
: VNX-LINK ( n -- ^n ) {: id:n :}    \ next row id + 1 in the row's bucket
   id cells VNX-L-P @ + ;
: CPX-BKT ( n -- ^n ) {: slot:n :}
   slot cells CPX-B-P @ + ;
: CPX-LINK ( n -- ^n ) {: id:n :}
   id cells CPX-L-P @ + ;

: VNX-FOLD ( n str -- n ) {: h:n a:^ u:n :}   \ folded bytes into h
   h 0 BEGIN dup u < WHILE
      dup a + c@ CORE-FOLD-C rot FNV:STEP swap
      1 +
   REPEAT drop ;

: VNX-HASH ( n str -- n ) {: fam:n a:^ u:n :}   \ family, tail
   FNV:BASIS fam FNV:STEP  a u VNX-FOLD
   VNX-SLOTS 1 - and ;

: CPX-HASH ( str str -- n ) {: pa:^ pu:n ta:^ tu:n :}   \ package, tail
   FNV:BASIS pa pu VNX-FOLD  ta tu VNX-FOLD
   VNX-SLOTS 1 - and ;

: VNX-ROW-BKT ( n -- ^n ) {: id:n :}
   id SUMV-FAM@ id SUMV-NAME$ VNX-HASH VNX-BKT ;
: CPX-ROW-BKT ( n -- ^n ) {: id:n :}
   id SUMV-CTOR-PKG$ id SUMV-NAME$ CPX-HASH CPX-BKT ;
: CPX-ROW? ( n -- bool )   \ has a package, so a CPX chain holds it
   SUMV-REC@ SV.CTOR-PKG-U @ 0 <> ;

: CHAIN-PUSH ( n ^n ^n -- ) {: id:n b:^ l:^ :}   \ l: id's link
   b @ l !   id 1 + b ! ;
: CHAIN-POP ( ^n ^n -- ) {: b:^ l:^ :}
   l @ b ! ;

: VNX-PUSH ( n -- ) {: id:n :}
   id  id VNX-ROW-BKT  id VNX-LINK  CHAIN-PUSH ;
: CPX-PUSH ( n -- ) {: id:n :}
   id  id CPX-ROW-BKT  id CPX-LINK  CHAIN-PUSH ;

: VNX-JOIN ( n -- ) {: id:n :}   \ chain a row in both indexes
   id VNX-PUSH
   id CPX-ROW? IF id CPX-PUSH THEN ;
: VNX-LEAVE ( n -- ) {: id:n :}   \ unchain a row from both
   id CPX-ROW? IF  id CPX-ROW-BKT  id CPX-LINK  CHAIN-POP THEN
   id VNX-ROW-BKT  id VNX-LINK  CHAIN-POP ;

\ The links follow the variant store's capacity and the buckets keep the load
\ factor at or below one half. Both only ever grow: SUMV-CAP never shrinks.
: VNX-RESIZE ( -- )
   SUMV-CAP VNX-CAP !
   SUMV-CAP VNX-LCAP @ > IF
      VNX-L-P  VNX-LCAP @ cells  SUMV-CAP cells  GROW
      CPX-L-P  VNX-LCAP @ cells  SUMV-CAP cells  GROW
      SUMV-CAP VNX-LCAP !
   THEN
   VNX-SLOTS-INIT BEGIN dup SUMV-CAP 2 * < WHILE 2 * REPEAT {: need:n :}
   need VNX-SLOTS <= IF EXIT THEN
   VNX-B-P  VNX-SLOTS cells  need cells  GROW
   CPX-B-P  VNX-SLOTS cells  need cells  GROW
   need VNX-SLOTS-V ! ;

: VNX-BUILD ( -- )
   VNX-RESIZE
   VNX-B-P @ 0 VNX-SLOTS ARENA-CELLS-ZERO
   CPX-B-P @ 0 VNX-SLOTS ARENA-CELLS-ZERO
   0 BEGIN dup SUMV-N @ < WHILE dup VNX-JOIN 1 + REPEAT drop
   SUMV-N @ VNX-HI !
   -1 VNX-READY ! ;

\ Three cell reads on every lookup: built, covering exactly the live rows, and
\ sized for the store's current capacity.
: VNX-ENSURE ( -- )
   VNX-READY @ 0=
   SUMV-N @ VNX-HI @ <> or
   VNX-CAP @ SUMV-CAP <> or IF VNX-BUILD THEN ;

\ Pops rows [newn, SUMV-N) before SUMV-N rewinds to newn, newest first, so each
\ popped row is at its buckets' head.
: VNX-RETIRE ( n -- ) {: newn:n :}
   VNX-READY @ 0= IF EXIT THEN
   SUMV-N @ 1 -
   BEGIN dup newn >= WHILE dup VNX-LEAVE 1 - REPEAT drop
   newn VNX-HI ! ;

: SUMV-CTOR-PKG! ( n n n -- ) {: id:n off:n u:n :}
   VNX-ENSURE
   off id SUMV-REC@ SV.CTOR-PKG-OFF !   u id SUMV-REC@ SV.CTOR-PKG-U !
   u 0 <> IF id CPX-PUSH THEN ;

\ Generated-constructor protection: a split name's head matches the recorded
\ SV.CTOR-PKG spelling of the package it resolves to here, case-insensitively,
\ so neither a folded alias nor a part's own name can reopen a constructor
\ package, extend it with a new tail, or undefine a generated word. Installed
\ into the checker's CTOR-*-XT friend cells at the end of this file.

variable TF-CW-COL          \ first-colon split position
PTR-VARIABLE TF-CW-HEAD-A   variable TF-CW-HEAD-U   \ the split head, resolved
: TF-CW-HEAD$ ( -- str ) TF-CW-HEAD-A @ TF-CW-HEAD-U @ ;
: SUMV-CTOR-PKG-MATCH? ( str n -- bool ) {: a:^ u:n id:n :}
   id SUMV-REC@ SV.CTOR-PKG-U @ 0= IF RES-FALSE EXIT THEN
   id SUMV-CTOR-PKG$ a u CORE-STR=CI ;

: TFAM-OWNER ( n -- str str bool ) {: fam:n :}
   fam TFAM-PKG$ fam TFAM-NAME$ RES-TRUE ;

\ The package and name of the family whose constructor package a u is: a
\ recorded ctor package, or a part `TOP:X` where X is one of a family of TOP.
: TFAM-CTOR-OWNER ( str -- str str bool ) {: a:^ u:n :}
   a u CHECKER-COLON-SCAN {: cn:n ci:n :}
   cn 1 =  ci 0 >  and {: part:bool :}
   SUMV-N @ 0 ?do
      a u i SUMV-CTOR-PKG-MATCH? IF i SUMV-FAM@ TFAM-OWNER unloop EXIT THEN
      part IF
         a ci + 1 +  u ci - 1 -  i SUMV-CTOR-PKG-MATCH? IF
            a ci  i SUMV-FAM@ TFAM-PKG$ CORE-STR=CI IF
               i SUMV-FAM@ TFAM-OWNER unloop EXIT
            THEN
         THEN
      THEN
   loop a 0 a 0 RES-FALSE ;

: TFAM-CTOR-PKG? ( str -- bool )
   TFAM-CTOR-OWNER {: pa:^ pu:n na:^ nu:n owned:bool :} owned ;

EXPORT CHECKER-RESOLVE:CK-REC-NAME$
EXPORT CHECKER-RESOLVE:HEAD-ROW

\ The package a qualifier's head names here, as a word's qualifier does: the
\ open package's part first, then a top-level package; a head no record holds
\ stays as given.
: TFQ-HEAD$ ( str -- str ) {: a:^ u:n :}
   a u HEAD-ROW {: row:^ :}
   row NULL-PTR = IF a u EXIT THEN
   row CK-REC-NAME$ ;

: TF-CW-SPLIT? ( str -- bool ) {: a:^ u:n :}   \ first non-edge ':' -> TF-CW-COL
   -1 TF-CW-COL !
   u 0 ?do
      a i + c@ 58 = IF i TF-CW-COL ! leave THEN
   loop
   TF-CW-COL @ 0 > TF-CW-COL @ u 1 - < and
   dup IF a TF-CW-COL @ TFQ-HEAD$ TF-CW-HEAD-U ! TF-CW-HEAD-A ! THEN ;
: TFAM-CTOR-WORD-AT? ( str n -- bool ) {: a:^ u:n id:n :}   \ split name = ctor id?
   TF-CW-HEAD$ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 -  id SUMV-NAME$ CORE-STR=CI ;

: TF-CW-TAIL$ ( str -- str ) {: a:^ u:n :}   \ name tail after the split colon
   a TF-CW-COL @ + 1 +  u TF-CW-COL @ - 1 - ;

: TFAM-DERIVED-TAIL? ( str -- bool ) {: a:^ u:n :}   \ a fixed generator-owned derived tail?
   a u s" eq" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" hash" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag>" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag" CORE-STR=CI ;

: TFAM-DERIVED-KIND-TAIL? ( str n -- bool ) {: a:^ u:n fam:n :}   \ derived tail the FAMILY generates
   a u s" eq" CORE-STR=CI IF fam TFAM-DERIVE-EQ? EXIT THEN
   a u s" hash" CORE-STR=CI IF fam TFAM-DERIVE-HASH? EXIT THEN
   a u s" tag>" CORE-STR=CI IF fam TFAM-DERIVE-TAG? EXIT THEN   \ enums only
   fam TFAM-PRODUCT? IF RES-FALSE EXIT THEN   \ products get no discriminant
   a u s" tag" CORE-STR=CI ;                  \ tag rides ANY derive on sum/enum

: TFAM-DERIVED-AT? ( str n -- bool ) {: a:^ u:n id:n :}   \ split name = id-family derived word?
   id SUMV-FAM@ TFAM-DERIVE-ANY? 0= IF RES-FALSE EXIT THEN
   TF-CW-HEAD$ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ id SUMV-FAM@ TFAM-DERIVED-KIND-TAIL? ;

\ The QUALIFIED half of the generated-word scan. Its private half, and the two
\ recognizers that join them, sit beside TF-CTOR-PRIV$ below: a recognizer must
\ read the spelling from the derivation that generates it, never restate it.
\ A constructor word's head and tail are its variant's package and tail, so
\ only the rows in their CPX bucket can be one. A derived word is recognized
\ from its family, not a row, so only the derived tails still walk the store.
: TF-CTOR-QUAL-FIND ( str -- n bool ) {: a:^ u:n :}   \ family, found
   a u TF-CW-TAIL$ {: ta:^ tu:n :}
   VNX-ENSURE
   TF-CW-HEAD$ ta tu CPX-HASH CPX-BKT @ VNX-CUR !
   BEGIN VNX-CUR @ 0 <> WHILE
      VNX-CUR @ 1 - {: id:n :}
      a u id TFAM-CTOR-WORD-AT? IF id SUMV-FAM@ RES-TRUE EXIT THEN
      id CPX-LINK @ VNX-CUR !
   REPEAT
   ta tu TFAM-DERIVED-TAIL? 0= IF 0 RES-FALSE EXIT THEN
   SUMV-N @ 0 ?do
      a u i TFAM-DERIVED-AT? IF i SUMV-FAM@ RES-TRUE unloop EXIT THEN
   loop 0 RES-FALSE ;

;package

package TFAM
private

: SUMV-MATCH? ( n str n -- bool ) {: fam:n na:^ nu:n id:n :}
   id SUMV-FAM@ fam = 0= IF RES-FALSE EXIT THEN
   id SUMV-NAME$ na nu CORE-STR= ;

\ The lowest matching row: a chain runs newest first, so the last match it meets
\ is the lowest.
: SUMV-FIND ( n str -- n bool ) {: fam:n na:^ nu:n :}
   VNX-ENSURE
   -1
   fam na nu VNX-HASH VNX-BKT @ VNX-CUR !
   BEGIN VNX-CUR @ 0 <> WHILE
      VNX-CUR @ 1 - {: id:n :}
      fam na nu id SUMV-MATCH? IF drop id THEN
      id VNX-LINK @ VNX-CUR !
   REPEAT
   dup 0 < IF drop 0 RES-FALSE EXIT THEN
   RES-TRUE ;

: SUMV-ADD ( n str n n n n -- n )
   {: fam:n na:^ nu:n tag:n ss:n sc:n pc:n :}
   WRITE-WINDOW-CK
   na nu TF-REQUIRE-CANON
   fam na nu SUMV-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   SUMV-ENSURE
   VNX-ENSURE                             \ size the index before the slot is committed
   SVX-SYNC                               \ catch an outside rewind before the stamp moves
   SUMV-N @ {: id:n :}
   na nu TF-INTERN {: noff:n :}
   id 1 + SUMV-N !
   id SUMV-REC@ {: r:^ :}
   fam r SV.FAM !   noff r SV.NAME-OFF !   nu r SV.NAME-U !
   \ An empty schema range is start 0 count 0, as for TFAM-VAR-RANGE!.
   tag r SV.TAG !   sc 0= IF 0 ELSE ss THEN r SV.SCH-START !
   sc r SV.SCH-COUNT !   pc r SV.PAYCELLS !
   0 r SV.CTOR-SYM !   0 r SV.CTOR-PKG-OFF !   0 r SV.CTOR-PKG-U !
   id VNX-PUSH   SUMV-N @ VNX-HI !
   SVX-STAMP                              \ a new row has no constructor symbol, so no head
   TFM-EPOCH+
   id ;

\ Constructor package name (docs/forth.md "Structures And Enums").
\ Uppercase every segment (package and family tail), double a literal '-' inside
\ each, join with single '-'. Escaping the tail too makes the map injective:
\ canonical segments never start or end with '-', so hyphen runs inside escaped
\ segments stay even-length and interior and each single '-' decodes uniquely.
\ Package `a-b` family `c` derives `A--B-C`; `a`+`b-c` derives `A-B--C`;
\ top-level `a-b-c` derives `A--B--C`; top level derives the bare escaped tail
\ (`result` -> `RESULT`).

PTR-VARIABLE TF-CTOR-P           \ the derived name, grown on demand
SCRATCH-VARIABLE TF-CTOR-CAP     \ TF-CTOR-P's room, in cells
variable TF-CTOR-U               \ derived-name length
: TF-CTOR-BUF ( -- ^u8 ) TF-CTOR-P @ ;

: TF-UPPER-C ( n -- n ) {: c:n :} c TF-LOWER? IF c 32 - EXIT THEN c ;   \ a-z -> A-Z

: TF-CTOR-C, ( n -- )
   TF-CTOR-P TF-CTOR-CAP  TF-CTOR-U @ CELL / 1 +  1 ARENA-ROWS-ENSURE
   TF-CTOR-BUF TF-CTOR-U @ + c!
   TF-CTOR-U @ 1 + TF-CTOR-U ! ;
: TF-CTOR-ESC ( str -- ) {: a:^ u:n :}   \ one uppercased '-'->'--' escaped segment
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ dup 45 = IF
         drop 45 TF-CTOR-C, 45 TF-CTOR-C,
      ELSE TF-UPPER-C TF-CTOR-C, THEN
      i 1 +
   REPEAT drop ;
: TF-CTOR-TAIL ( str -- ) {: a:^ u:n :}      \ raw uppercased private tail
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ TF-UPPER-C TF-CTOR-C,
      i 1 +
   REPEAT drop ;
: TF-CTOR-BUILD-ESCAPED ( str str -- )   \ (pkg tail)
   {: pa:^ pu:n ta:^ tu:n :}
   0 TF-CTOR-U !
   pu 0 > IF pa pu TF-CTOR-ESC  45 TF-CTOR-C, THEN
   ta tu TF-CTOR-ESC ;

\ The tail must already be canonical lowercase.

: TF-CTOR-PKG$ ( str str -- str )
   {: pa:^ pu:n ta:^ tu:n :}
   pa pu ta tu TF-CTOR-BUILD-ESCAPED
   TF-CTOR-BUF TF-CTOR-U @ ;

\ Two spellings, on purpose:
\   public   PKG-FAMILY:MEMBER  in the reserved constructor namespace
\                               TF-CTOR-PKG$ derives, escaped.
\   private  FAMILY-MEMBER      in the declaring package's private wordlist:
\                               uppercased family tail, '-', uppercased member.
\ The private form needs no package segment or escaping: every use is inside the
\ declaring package. It is not injective across packages and need not be: a
\ collision with an existing private word, or between two families of one
\ package, dies "generated declaration already defined".
\ A qualified private spelling is impossible: the engine routes a qualified
\ definition name to the namespace's PUBLIC wid, and the
\ checker records any single-non-edge-colon name as that package's public
\ symbol before it consults the open package (checker.f CHECKER-RECORD-SYM).
: TF-CTOR-PRIV$ ( str str -- str )
   {: ta:^ tu:n ma:^ mu:n :}
   0 TF-CTOR-U !
   ta tu TF-CTOR-TAIL
   45 TF-CTOR-C,
   ma mu TF-CTOR-TAIL
   TF-CTOR-BUF TF-CTOR-U @ ;

\ A private family's generated words are its rows' private spellings, a
\ product's make and unmake rows included, and its derived words: the
\ family's private prefix and a tail TFAM-DERIVED-KIND-TAIL? accepts, the
\ qualified half's rule. A name no row or derived tail spells is the package's
\ own word: beside a private `SUMTYPE colour`, `undefine COLOUR-MAKE` succeeds.
: TF-PRIV-NAME? ( str n -- bool ) {: a:^ u:n fam:n :}   \ = a row of THIS family?
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ 0 ?do
      a u  fam TFAM-NAME$ vstart i + SUMV-NAME$ TF-CTOR-PRIV$  CORE-STR=CI
         IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

: TF-PRIV-DERIVED? ( str n -- bool ) {: a:^ u:n fam:n :}
   fam TFAM-DERIVE-ANY? 0= IF RES-FALSE EXIT THEN
   fam TFAM-NAME$ a 0 TF-CTOR-PRIV$ {: pa:^ pu:n :}   \ FAMILY-
   u pu <= IF RES-FALSE EXIT THEN
   a pu pa pu CORE-STR=CI 0= IF RES-FALSE EXIT THEN
   a pu +  u pu -  fam TFAM-DERIVED-KIND-TAIL? ;

\ A private or OPAQUE family's generated words resolve only while its package
\ is open; outside it the name belongs to whoever spells it.
: TF-CTOR-PRIV-FAM? ( str n -- bool ) {: a:^ u:n fam:n :}
   fam TFAM-GEN-PUBLIC? IF RES-FALSE EXIT THEN
   fam TFAM-VAR-COUNT@ 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u fam TF-PRIV-NAME? IF RES-TRUE EXIT THEN
   a u fam TF-PRIV-DERIVED? ;

: TF-CTOR-PRIV-FIND ( str -- n bool ) {: a:^ u:n :}   \ family, found
   TFAM-N @ 0 ?do
      a u i TF-CTOR-PRIV-FAM? IF i RES-TRUE unloop EXIT THEN
   loop 0 RES-FALSE ;

\ `DERIVE addr` and `init` members are recognized from the field registry,
\ declared further down, so the scan is an xt bound there.
defer TFAM-ADDR-FIND-XT ( str -- n bool )

\ A length that describes no memory (BYTE-SPAN?) spells no generated word, and
\ is answered before a scan reads the bytes.
: TF-GEN-FIND ( str -- n bool ) {: a:^ u:n :}
   a u BYTE-SPAN? 0= IF 0 RES-FALSE EXIT THEN
   a u TF-CW-SPLIT? IF
      a u TF-CTOR-QUAL-FIND IF RES-TRUE EXIT THEN
   ELSE
      a u TF-CTOR-PRIV-FIND IF RES-TRUE EXIT THEN
   THEN
   drop  a u TFAM-ADDR-FIND-XT ;

: TFAM-CTOR-WORD? ( str -- bool )   \ exact generated word?
   TF-GEN-FIND nip ;

\ The type whose generated word a definition of a u here would repeat, and true;
\ a bare spelling repeats one only in the package's private section.
: TFAM-GENERATOR ( str -- str bool ) {: a:^ u:n :}
   a u TF-GEN-FIND {: fam:n hit:bool :}
   hit 0= IF s" " RES-FALSE EXIT THEN
   a u TF-CW-SPLIT? 0= IF
      CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
         s" " RES-FALSE EXIT
      THEN
   THEN
   fam TFAM-NAME$ RES-TRUE ;
;package

package TFAM
private

\ A private family reserves no namespace, so it has no package for a stray tail
\ to extend and this stays the qualified rule alone.
\ The generated-word test runs first: it answers every generated definition
\ from the tail index, while the package test walks the variant store and is
\ needed only for a name the generator does not own.
: TFAM-CTOR-EXTEND? ( str -- bool ) {: a:^ u:n :}   \ new tail in a ctor package?
   a u TF-CW-SPLIT? 0= IF RES-FALSE EXIT THEN
   TF-CW-HEAD$ {: ha:^ hu:n :}
   a u TFAM-CTOR-WORD? IF RES-FALSE EXIT THEN
   ha hu TFAM-CTOR-PKG? ;

\ shared fields, keyed by (family-id, optional-variant-id, field tail).
\ PF-N includes the open declaration's provisional rows; PF-COMMIT-N is the
\ reflection high-water. PF-ADD returns no row id, so an id becomes observable
\ only after PF-COMMIT.
-1 constant PF-NO-VARIANT
\ A buffer row (STRUCTURE `BUFFER name N`) holds N bytes inline from a whole-cell
\ slot: its schema is the element, u8, and its cells, ceil(N / CELL), its width.
1 constant PF-FLAG-BUFFER

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
\ PF.ACCESSOR-SYM: the checker symbol of the field's generated accessor, 0 when
\ it has none.
11 cells constant PF.ACCESSOR-SYM-OFF

12 cells constant PF-REC
: PF.FAM ( ^a -- ^a ) PF.FAM-OFF + ;
: PF.VAR ( ^a -- ^a ) PF.VAR-OFF + ;
: PF.NAME-OFF ( ^a -- ^a ) PF.NAME-OFF-AT + ;
: PF.NAME-U ( ^a -- ^a ) PF.NAME-U-OFF + ;
: PF.SCH ( ^a -- ^a ) PF.SCH-OFF + ;
: PF.SLOT ( ^a -- ^a ) PF.SLOT-OFF + ;
: PF.CELLS ( ^a -- ^a ) PF.CELLS-OFF + ;
: PF.BYTE-OFF ( ^a -- ^a ) PF.BYTE-OFF-AT + ;
: PF.BYTES ( ^a -- ^a ) PF.BYTES-OFF + ;
: PF.ALIGN ( ^a -- ^a ) PF.ALIGN-OFF + ;
: PF.FLAGS ( ^a -- ^a ) PF.FLAGS-OFF + ;

: PF.ACCESSOR-SYM ( ^a -- ^a ) PF.ACCESSOR-SYM-OFF + ;

16 constant PF-CAP-INIT            \ holds the built-ins' fields (TF-CAP-INIT)
variable PF-CAP-V   PF-CAP-INIT PF-CAP-V !

: PF-CAP ( -- n ) PF-CAP-V @ ;

create PF-A-BOOT   PF-CAP-INIT PF-REC * allot
PERSISTED-PTR-VARIABLE PF-A-P   PF-A-BOOT PF-A-P !

: PF-BASE ( -- ^n ) PF-A-P @ ;

variable PF-N   0 PF-N !
variable PF-COMMIT-N   0 PF-COMMIT-N !

PF-REC CELL / constant PF-REC-CELLS   \ product-field record stride, in cells

\ Zeroes the product-field records [lo,hi): rows hold only scalar ids and
\ offsets, so a zeroed row is the canonical absent row and retired rows leave no
\ bytes in reflection or snapshot/fixpoint identity.

: PF-SCRUB ( n n -- ) {: lo:n hi:n :}
   hi lo <= IF EXIT THEN
   PF-BASE  lo PF-REC-CELLS *  hi PF-REC-CELLS *  ARENA-CELLS-ZERO ;

: PF-GROW ( n -- ) {: need:n :}
   need PF-CAP-V @ 2 * max {: nc:n :}
   PF-A-P  PF-CAP-V @ PF-REC *  nc PF-REC *  GROW
   nc PF-CAP-V ! ;

: PF-ENSURE ( -- )
   PF-N @ PF-CAP-V @ < IF exit THEN
   PF-N @ 1 + PF-GROW ;

: PF-REC@ ( n -- ^n ) {: id:n :}
   id PF-REC * PF-BASE + ;

: PF-FAM@ ( n -- n ) PF-REC@ PF.FAM @ ;
: PF-VAR@ ( n -- n ) PF-REC@ PF.VAR @ ;
: PF-NAME$ ( n -- str ) {: id:n :}
   id PF-REC@ {: r:^ :}  r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ ;
: PF-SCH@ ( n -- n ) PF-REC@ PF.SCH @ ;

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
\ Chains run newest first. VNX's discipline: PF-ADD pushes each row in id order,
\ PF-REWIND pops retired rows newest first while their owner cells are intact.
\ Buckets and links are process-local: a capture drops them, and a capacity
\ change or a store moved outside those seams rebuilds them.

32 constant FVX-SLOTS-INIT              \ power of two; grown to keep load <= 1/2
variable FVX-SLOTS-V   FVX-SLOTS-INIT FVX-SLOTS-V !
: FVX-SLOTS ( -- n ) FVX-SLOTS-V @ ;
create FVX-B-BOOT   FVX-SLOTS-INIT cells allot
PERSISTED-PTR-VARIABLE FVX-B-P   FVX-B-BOOT FVX-B-P !
create FVX-L-BOOT   PF-CAP-INIT cells allot
PERSISTED-PTR-VARIABLE FVX-L-P   FVX-L-BOOT FVX-L-P !
variable FVX-LCAP   PF-CAP-INIT FVX-LCAP !   \ rows the link array holds
variable FVX-READY   0 FVX-READY !
variable FVX-HI      0 FVX-HI !      \ PF-N the chains cover
variable FVX-CAP     0 FVX-CAP !     \ PF-CAP the buckets were sized for

: FVX-BKT ( n -- ^n ) {: slot:n :}   \ bucket head: row id + 1, 0 = empty
   slot cells FVX-B-P @ + ;
: FVX-LINK ( n -- ^n ) {: id:n :}    \ next row id + 1 in the row's bucket
   id cells FVX-L-P @ + ;

: FVX-HASH ( n n -- n ) {: fam:n var:n :}
   FNV:BASIS fam FNV:STEP var FNV:STEP
   FVX-SLOTS 1 - and ;

: PF-ROW-OWNER? ( n n ^n -- bool ) {: fam:n var:n r:^ :}
   r PF.FAM @ fam = r PF.VAR @ var = and ;

: FVX-ROW-BKT ( n -- ^n ) {: id:n :}
   id PF-REC@ {: r:^ :}
   r PF.FAM @ r PF.VAR @ FVX-HASH FVX-BKT ;

: FVX-PUSH ( n -- ) {: id:n :}
   id FVX-ROW-BKT {: b:^ :}
   b @ id FVX-LINK !
   id 1 + b ! ;

: FVX-POP ( n -- ) {: id:n :}
   id FVX-ROW-BKT {: b:^ :}
   id FVX-LINK @ b ! ;

\ The links follow the field store's capacity and the buckets keep the load
\ factor at or below one half. Both only ever grow: PF-CAP never shrinks.
: FVX-RESIZE ( -- )
   PF-CAP FVX-CAP !
   PF-CAP FVX-LCAP @ > IF
      FVX-L-P  FVX-LCAP @ cells  PF-CAP cells  GROW
      PF-CAP FVX-LCAP !
   THEN
   FVX-SLOTS-INIT BEGIN dup PF-CAP 2 * < WHILE 2 * REPEAT {: need:n :}
   need FVX-SLOTS <= IF EXIT THEN
   FVX-B-P  FVX-SLOTS cells  need cells  GROW
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
\ no longer cover exactly the live rows are dropped for a rebuild instead.
\ Checker frames restore last saved first, so newn never lies above PF-N.
: FVX-RETIRE ( n -- ) {: newn:n :}
   FVX-READY @ 0= IF EXIT THEN
   FVX-HI @ PF-N @ <> IF 0 FVX-READY ! EXIT THEN
   PF-N @ 1 -
   BEGIN dup newn >= WHILE dup FVX-POP 1 - REPEAT drop
   newn FVX-HI ! ;

\ One owner's rows, newest first: FVX-FIRST answers the cursor of its bucket's
\ newest row and FVX-NEXT the cursor after a cursor's row, each a row id + 1
\ with 0 for the end. The cursor lives on the stack, so a reader may recurse
\ into another owner's walk.
: FVX-FIRST ( n n -- n ) {: fam:n var:n :}
   FVX-ENSURE
   fam var FVX-HASH FVX-BKT @ ;
: FVX-NEXT ( n -- n ) 1 - FVX-LINK @ ;

\ PF-LINK ( n -- ) : chain row id, which PF-ADD has written and is about to
\ publish as PF-N's next value.
: PF-LINK ( n -- ) {: id:n :}
   FVX-ENSURE                             \ covers [0, id) and fits the capacity
   id FVX-PUSH
   id 1 + FVX-HI ! ;

\ THE field store rewind: a checker frame's restore puts the rows back to a
\ saved mark through here.
\ The index pops the retired rows before the scrub clears the owner cells it
\ finds their buckets by.
: PF-REWIND ( n n -- ) {: pfn:n commitn:n :}
   pfn FVX-RETIRE
   pfn PF-N @ <>  commitn PF-COMMIT-N @ <> or IF TFM-EPOCH+ THEN
   pfn PF-N @ PF-SCRUB                    \ scrub the rows this rewind retires
   pfn PF-N !
   commitn PF-COMMIT-N ! ;

\ --- family facts memo (TFM). A checked use of a family's type asks
\ TFAM-CONCRETE-LINEAR?, which walks every variant, and certifying one
\ generated constructor asks several times, so without the memo an ENUM's
\ constructors are quadratic in its variants.
\ Each answer is memoized per family under TFM-EPOCH: a cell stamped with the
\ current epoch holds the answer a walk would give. A refusal is a throw and is
\ never memoized. Process-local, like VNX.

0 constant TFM-LINEAR
1 constant TFM-STORE
4 cells constant TFM-REC                    \ (stamp, answer) per fact
create TFM-BOOT   TF-CAP-INIT TFM-REC * allot
PERSISTED-PTR-VARIABLE TFM-P   TFM-BOOT TFM-P !
variable TFM-CAP   TF-CAP-INIT TFM-CAP !   \ families TFM-P holds

\ Grown with the family store. The new rows are zeroed, and zero is never a
\ current stamp.
: TFM-ENSURE ( -- )
   TFM-CAP @ TF-CAP >= IF EXIT THEN
   TFM-P  TFM-CAP @ TFM-REC *  TF-CAP TFM-REC *  GROW
   TFM-P @  TFM-CAP @ TFM-REC * CELL /  TF-CAP TFM-REC * CELL /  ARENA-CELLS-ZERO
   TF-CAP TFM-CAP ! ;

: TFM-AT ( n n -- ^n ) {: fam:n k:n :}   \ the fact's stamp cell; its answer follows
   TFM-ENSURE
   fam TFM-REC *  k 2 * cells +  TFM-P @ + ;

: TFM@ ( n n -- n bool ) {: fam:n k:n :}
   fam k TFM-AT {: c:^ :}
   c @ TFM-EPOCH @ <> IF 0 RES-FALSE EXIT THEN
   c CELL + @ RES-TRUE ;

: TFM! ( n n n -- ) {: v:n fam:n k:n :}
   fam k TFM-AT {: c:^ :}
   TFM-EPOCH @ c !
   v c CELL + ! ;

: TFM-FLAG@ ( n n -- bool bool ) TFM@ {: ok:bool :} 0 <> ok ;
: TFM-FLAG! ( bool n n -- ) {: b:bool fam:n k:n :}
   b IF 1 ELSE 0 THEN fam k TFM! ;

\ A variant has one declared-payload source: SUMTYPE stores positional roots in
\ the SUMV schema range; ENUM stores named rows in TYPE-FIELD and leaves that
\ range empty. The accessors below are the one seam for both. A sum/enum family
\ with a nonzero declared field count uses TYPE-FIELD for every variant. A
\ product family keeps positional schemas
\ for its make/unmake variants. An all-empty family has no payload either way.

\ A variant's named payload is the rows of its owner chain (FVX) that sit inside
\ its family's field slice. The chain can also hold the owner's rows outside the
\ slice, provisional ones included, which no reader counts; it runs newest
\ first, so declaration order is counted back from its far end.
: SUMV-SLICE-ROW? ( n n n -- bool ) {: fam:n vid:n id:n :}
   fam TFAM-FLD-START@ {: base:n :}
   id base <  id base fam TFAM-FLD-COUNT@ + >=  or IF RES-FALSE EXIT THEN
   fam vid id PF-REC@ PF-ROW-OWNER? ;

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

: SUM-NAMED-PAYLOAD? ( n -- bool ) {: fam:n :}
   fam TFAM-SUM? fam TFAM-ENUM? or 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-COUNT@ 0 <> ;

: SUMV-NAMED-PAYLOAD? ( n -- bool ) SUMV-FAM@ SUM-NAMED-PAYLOAD? ;

: SUMV-PAY-FIELD ( n n -- n bool ) {: vid:n wanted:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF 0 RES-FALSE EXIT THEN
   vid wanted SUMV-NAMED-FIELD ;

: SUMV-PAY-N ( n -- n ) {: vid:n :}
   vid SUMV-NAMED-PAYLOAD? 0= IF vid SUMV-SCH-COUNT@ EXIT THEN
   vid SUMV-FAM@ vid SUMV-SLICE-N ;

: SUMV-PAY-ROOT ( n n -- n ) {: vid:n index:n :}
   vid SUMV-NAMED-PAYLOAD? IF
      vid index SUMV-NAMED-FIELD drop
      PF-SCH@ EXIT
   THEN
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

16 constant TFW-INIT
create TFW-BOOT   TFW-INIT cells allot
PERSISTED-PTR-VARIABLE TFW-P   TFW-BOOT TFW-P !
variable TFW-CAP   TFW-INIT TFW-CAP !   \ cells TFW-P holds

\ Grown, never zeroed: every frame cell is written before it is read.
: TFW-ENSURE ( n -- ) {: need:n :}
   need TFW-CAP @ <= IF EXIT THEN
   need TFW-CAP @ 2 * max {: cap:n :}
   TFW-P  TFW-CAP @ cells  cap cells  GROW
   cap TFW-CAP ! ;

: TFW@ ( n -- n ) cells TFW-P @ + @ ;
: TFW! ( n n -- ) {: w:n idx:n :}
   idx 1 + TFW-ENSURE
   w idx cells TFW-P @ + ! ;

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

\ A buffer's width is its row's cells: its schema names only the element.
: TFW-FIELD ( n n n -- n ) {: id:n base:n top:n :}
   id PF-REC@ {: r:^ :}
   r PF.FLAGS @ PF-FLAG-BUFFER = IF r PF.CELLS @ EXIT THEN
   r PF.SCH @ SCHEMA-ROOT@ base top TFW-NODE ;

: TFW-PRODUCT ( n n n -- n ) {: fam:n base:n top:n :}
   fam TFAM-FLD-START@ {: fs:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      fs over + base top TFW-FIELD
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

\ A family with no parameter answers from its record, so a chain of closed
\ families holding one another costs no walk. A parametric one is walked per
\ frame.
: TFW-FAM-IMPL ( n n n -- n ) {: fam:n base:n top:n :}
   fam TFAM-ARITY@ 0 > IF fam base top TFW-FAM-WALK EXIT THEN
   fam TFAM-WIDTH@ ;
: TFW-FAM-INSTALL ( -- ) [: TFW-FAM-IMPL ;] is TFW-FAM ;
TFW-FAM-INSTALL

: TFAM-SCH-WIDTH ( n n -- n ) {: fam:n node:n :}   \ declared width of a schema node of fam: each parameter one cell
   fam TFAM-ARITY@ {: ar:n :}
   ar 0 ?do 1 i TFW! loop
   node 0 ar TFW-NODE ;

\ Every argument is measured before any frame cell is written: T-WIDTH of an
\ argument re-enters this walk at cell 0.
: TERM-FRAME ( n n n -- ) {: term:n idx:n count:n :}   \ frame cells [idx, count) := term's argument widths
   idx count >= IF EXIT THEN
   term idx PARAM>ARG T-WIDTH {: w:n :}
   term idx 1 + count RECURSE
   w idx TFW! ;

\ A closed family answers from its record; a parametric one is walked per
\ instance.
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
         fam TFAM-FLD-START@ over + PF-SCH@ SCHEMA-ROOT@ slot SCH-ROOT-WIDTH-SLOT?
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

\ --- which argument slots reach storage (checker.f PARAM-PAIR-SLOT), memoized under
\ TFM-EPOCH: the kinds and OPAQUE read here are set before the field commit moves it.
-1 constant STORE-ALL
defer TFAM-STORE-MASK-FWD ( n -- n )   \ ( fam -- mask ) forward ref for a nested application


\ The walk ends: a family names itself only beneath a pointer, where held is true and
\ no nested mask is asked, or as a boxed sum, which TFAM-STORE-WALK answers first.
: SCH-STORE-MASK ( n bool -- n )   \ slots whose parameter node holds beneath storage
   {: node:n held:bool :}
   node SCHEMA-PARAM? IF held IF 1 node SCHEMA-A@ lshift ELSE 0 THEN EXIT THEN
   node SCHEMA-PTR? IF node SCHEMA-A@ RES-TRUE RECURSE EXIT THEN
   node SCHEMA-APP? IF
      held IF 0 ELSE node SCHEMA-A@ TFAM-STORE-MASK-FWD THEN {: g:n :}
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
      fam TFAM-FLD-START@ i + PF-SCH@ SCHEMA-ROOT@ RES-FALSE SCH-STORE-MASK or
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


: TYPE-SLOT-MASK ( n -- n )   \ PK-TYPE slots: each names the type of the storage its family addresses
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


: PF-MATCH? ( n n str n -- bool ) {: fam:n var:n na:^ nu:n id:n :}
   id PF-REC@ {: r:^ :}
   fam var r PF-ROW-OWNER? 0= IF RES-FALSE EXIT THEN
   r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= ;

: PF-FIND ( n n str -- n bool ) {: fam:n var:n na:^ nu:n :}
   0 BEGIN dup PF-COMMIT-N @ < WHILE
      {: i:n :}
      fam var na nu i PF-MATCH? IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;

\ PF-ADD validation. Field rows carry explicit logical-cell and memory-layout
\ metadata under every policy; no policy implies CELL-sized storage.
$7FFFFFFFFFFFFFFF constant PF-MAX-N

0 constant PF-FLAGS-NONE

: TF-GRAMMAR-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u s" variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" newtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" enum" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;enum" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" field" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" buffer" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" policy" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" derive" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" scratch-only" CORE-STR=CI ;

;package

package TYPE-NAME

\ sumtype.f's TYPE-DECL codes under their own names: that package loads after
\ this one, so the same (code, name) pair is registered here first.
7107 constant E-TDECL-SYNTAX
7110 constant E-TDECL-NAME

public

\ The text after the last colon of a name, or the whole name when there is
\ none: a package path's last segment.
: LAST-SEGMENT$ ( str -- str ) {: a:^ u:n :}
   u 0 ?do
      a u i - 1 - + c@ 58 = IF a u i - + i unloop EXIT THEN
   loop a u ;

private

EXPORT CHECK:ATOM-TOK?
EXPORT CHECK:CHECKER-AUTH-PACKAGE$
EXPORT CHECK:CHECKER-AUTH-PACKAGE-ACTIVE?
EXPORT CHECK:CON-OF
EXPORT CHECK:CT-NAME$
EXPORT CHECK:CTN
EXPORT CHECK:FRESH-ATOM-TOK?
EXPORT CHECK:DEF-NAME-BAD?
EXPORT CHECK:SIG-PTR-TOK?
EXPORT CHECK:VREC-FIND
EXPORT CHECK:VREC-N
EXPORT CHECK:VREC-NAME$
EXPORT CHECK:XT-TOK?
EXPORT TFAM:TF-GRAMMAR-KEYWORD?
EXPORT TFAM:TF-HIDDEN?
EXPORT TFAM:TF-REQUIRE-CANON
EXPORT TFAM:TFAM-FIND-IN
: RESERVED? ( str -- bool ) {: a:^ u:n :}
   u 1 = IF RES-TRUE EXIT THEN
   a u VREC-FIND IF drop RES-TRUE EXIT THEN drop
   a u CON-OF 0 <> IF RES-TRUE EXIT THEN
   a u ATOM-TOK? IF RES-TRUE EXIT THEN
   a u XT-TOK? IF RES-TRUE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u TF-GRAMMAR-KEYWORD? ;

\ Name `na nu` spells `<qa>:<ta>`, its qualifier folded as a signature folds it
\ (TFQ-SPLIT?).
: QUAL-NAME? ( str str str -- bool )
   {: na:^ nu:n qa:^ qu:n ta:^ tu:n :}
   nu qu tu + 1 + <> IF RES-FALSE EXIT THEN
   na qu + c@ 58 <> IF RES-FALSE EXIT THEN
   na qu qa qu CORE-STR=CI 0= IF RES-FALSE EXIT THEN
   na qu + 1 + tu ta tu CORE-STR= ;

\ A DEFLINEAR or VALUE-RECORD name `B:tail` exists, B the open package path's
\ last segment: the declaration TFAM-CLAIMED? refuses when the family comes first.
: QUAL-NOMINAL? ( str -- bool ) {: a:^ u:n :}
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ LAST-SEGMENT$ {: qa:^ qu:n :}
   qa qu TF-HIDDEN? IF RES-FALSE EXIT THEN
   CTN @ 1 ?do
      i CT-NAME$ qa qu a u QUAL-NAME? IF RES-TRUE unloop EXIT THEN
   loop
   VREC-N @ 0 ?do
      i VREC-NAME$ qa qu a u QUAL-NAME? IF RES-TRUE unloop EXIT THEN
   loop RES-FALSE ;

public

\ The variant-name collision rule, asked by VARIANT-REQUIRE and by SUMTYPE's
\ variant gate (sumtype.f): a family is taken wherever the declaring scope
\ resolves it, a global family in every scope and a package family inside its
\ package.
: FAMILY-TAKEN? ( str -- bool ) {: a:^ u:n :}
   s" " a u TFAM-FIND-IN nip IF RES-TRUE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ a u TFAM-FIND-IN nip ;

\ The family-name gate every family definer asks: SUMTYPE and NEWTYPE
\ (sumtype.f) and the STRUCTURE and ENUM front ends. A family tail must be one an
\ effect reads as that family. The effect parser reads a value record name
\ (checker.f PSTACK) ahead of any family, refuses a bare `ptr` (SIG-PTR-TOK?)
\ as an unknown type even where a family of that tail resolves, and reads an
\ atom-shaped tail as an atom wherever the family does not resolve; its other
\ readings (`[`, stack syntax, row variables, split characters) are never
\ canonical tails. A package may shadow a global family, so without the `ptr`
\ row a package's `ptr` registered and its CAST: converter `( n -- ptr )` died
\ as an unknown type, a family no effect could name; without the value record
\ row `ENUM vr` loaded in a package after `VALUE-RECORD vr`, one name naming
\ two kinds.
: FAMILY-RESERVED? ( str -- bool ) {: a:^ u:n :}
   a u SIG-PTR-TOK? IF RES-TRUE EXIT THEN
   a u QUAL-NOMINAL? IF RES-TRUE EXIT THEN
   a u RESERVED? ;

\ A variant defines a word in its family's package, so no keyword may name it.
: VARIANT-REQUIRE ( str -- ) {: a:^ u:n :}
   u 0= IF E-TDECL-SYNTAX throw THEN
   a u DEF-NAME-BAD? IF TFAM:E-TDECL-DEF-NAME throw THEN
   a u TF-REQUIRE-CANON
   a u RESERVED? IF E-TDECL-NAME throw THEN
   a u FAMILY-TAKEN? IF E-TDECL-NAME throw THEN ;

seal-package

;package

package TFAM
private

\ A field tail may not spell a declaration word (TF-GRAMMAR-KEYWORD?) or a
\ generated-operation name, on every path that registers a field row: the
\ STRUCTURE and ENUM front ends, through PF-ADD.
: PF-RESERVED? ( str -- bool ) {: a:^ u:n :}
   a u TF-GRAMMAR-KEYWORD? IF RES-TRUE EXIT THEN
   a u s" make" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" unmake" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" tag" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" eq" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" hash" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" order" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" encode" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" decode" CORE-STR=CI ;

: PF-NAME-REQUIRE ( str -- ) {: a:^ u:n :}
   a u TF-REQUIRE-CANON
   a u PF-RESERVED? IF E-PF-NAME throw THEN ;

: PF-FAM-LIVE? ( n -- bool ) {: fam:n :} fam 0 >= fam TFAM-N @ < and ;
: PF-OWNER-OK? ( n n -- bool ) {: fam:n var:n :}
   fam PF-FAM-LIVE? 0= IF RES-FALSE EXIT THEN
   var PF-NO-VARIANT = IF fam TFAM-PRODUCT? EXIT THEN
   var 0 < var SUMV-N @ >= or IF RES-FALSE EXIT THEN
   var SUMV-FAM@ fam =
   fam TFAM-SUM? fam TFAM-ENUM? or and ;

EXPORT CHECKER-RESOLVE:TOP$

\ A private family is visible in its own package and in that package's parts.
: PF-FAM-VISIBLE? ( n n -- bool ) {: owner:n fam:n :}
   fam TFAM-PUBLIC? IF RES-TRUE EXIT THEN
   fam TFAM-PKG$ owner TFAM-PKG$ CORE-STR= IF RES-TRUE EXIT THEN
   owner TFAM-PKG$ CHECKER-COLON-SCAN drop 0= IF RES-FALSE EXIT THEN
   fam TFAM-PKG$ owner TFAM-PKG$ TOP$ CORE-STR= ;
: PF-APP-KIND ( n -- n ) {: fam:n :}
   fam TFAM-KIND@ TK-EVIDENCE = IF PK-EVIDENCE EXIT THEN
   fam TFAM-LAYOUT? IF PK-LAYOUT EXIT THEN
   PK-CELL ;
: PF-NONVALUE? ( n -- bool )
   dup PK-SCOPE = swap PK-REGION = or ;

\ A family with a scope or region parameter.
: TFAM-NONVALUE-PARAM? ( n -- bool ) {: fam:n :}
   fam TFAM-ARITY@ 0 ?do
      fam i TFAM-PK@ PF-NONVALUE? IF UNLOOP RES-TRUE EXIT THEN
   loop RES-FALSE ;

\ A cell parameter takes a layout-kind argument: the instantiated width is
\ argument-aware (TFAM-INST-WIDTH@), so option<pair> is a valid field type as it
\ is a valid variant payload or stack item.
: PF-KIND-OK? ( n n -- bool ) {: got:n want:n :}
   want PK-TYPE = IF
      got PK-EVIDENCE <> got PF-NONVALUE? 0= and EXIT THEN
   want PK-CELL = got PK-LAYOUT = and IF RES-TRUE EXIT THEN
   got want = ;

\ Deferred so PF-NODE-KIND?'s SCH-QUOT case can reach it before its body, which
\ calls PF-NODE-KIND? back, is defined.
defer PF-QUOT-ROW-OK? ( n n -- bool )   \ ( owner rownode -- bool )

\ PF-NODE-KIND?'s answer per node id: it reads only the node, which never
\ changes, the owner and the registry, so it holds while PF-GEN is current.
SCRATCH{
PTR-VARIABLE PF-MEMO-P   variable PF-MEMO-CAP
}SCRATCH
4 constant PF-MEMO-ROW
: PF-MEMO-SLOT ( n -- ^n ) PF-MEMO-ROW * cells PF-MEMO-P @ + ;
\ Transient ids are reused after a release: never memoized.
: PF-MEMO-ID? ( n -- bool ) {: node:n :} node 0 > node SCH-N @ < and ;

: PF-MEMO-HIT? ( n n -- bool ) {: owner:n node:n :}
   PF-GEN @ 0= IF RES-FALSE EXIT THEN
   node PF-MEMO-ID? 0= IF RES-FALSE EXIT THEN
   node PF-MEMO-CAP @ >= IF RES-FALSE EXIT THEN
   node PF-MEMO-SLOT {: s:^ :}
   s @ PF-GEN @ =  s CELL + @ owner =  and ;

: PF-MEMO-ANSWER ( n -- n bool ) PF-MEMO-SLOT {: s:^ :}
   s 2 cells + @  s 3 cells + @ 0 <> ;

\ Stores node's answer while a generation is current, and answers it.
: PF-KEEP ( n bool n n -- n bool ) {: kind:n ok:bool owner:n node:n :}
   PF-GEN @ 0 <>  node PF-MEMO-ID? and IF
      PF-MEMO-P PF-MEMO-CAP node 1 + PF-MEMO-ROW ARENA-ROWS-ENSURE
      node PF-MEMO-SLOT {: s:^ :}
      PF-GEN @ s !  owner s CELL + !  kind s 2 cells + !  ok IF 1 ELSE 0 THEN s 3 cells + !
   THEN kind ok ;

\ Each child's answer goes through PF-KEEP. The recursion is as deep as the
\ type, so a frame keeps only what the walk needs on the data stack.
: PF-NODE-JUDGE ( n n -- n bool ) {: owner:n node:n :}
   owner node PF-MEMO-HIT? IF node PF-MEMO-ANSWER EXIT THEN
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
      owner child RECURSE  owner child PF-KEEP 0= IF drop 0 RES-FALSE EXIT THEN
      PF-NONVALUE? IF 0 RES-FALSE EXIT THEN
      child SCHEMA-APP? IF
         child SCHEMA-A@ TFAM-NONVALUE-PARAM? IF 0 RES-FALSE EXIT THEN
      THEN
      PK-CELL RES-TRUE EXIT
   THEN
   node SCHEMA-QUOT? IF
      node SCHEMA-A@ 1 invert and CTL-STATABLE invert and 0 <> IF 0 RES-FALSE EXIT THEN
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
         owner swap RECURSE  owner start r@ + SCHEMA-ROOT@ PF-KEEP
         0= IF drop r> drop drop 0 RES-FALSE EXIT THEN
         fam r@ TFAM-PK@ PF-KIND-OK? 0= IF r> drop drop 0 RES-FALSE EXIT THEN
         r> drop
         1 +
      REPEAT drop
      fam PF-APP-KIND RES-TRUE EXIT
   THEN
   0 RES-FALSE ;

: PF-NODE-KIND? ( n n -- n bool ) {: owner:n node:n :}
   owner node PF-NODE-JUDGE  owner node PF-KEEP ;

\ A field type's parse judges each node once: answers are kept from START to END.
: PF-KIND-MEMO-START ( -- ) PF-GEN-NEW ;
: PF-KIND-MEMO-END ( -- ) 0 PF-GEN ! ;

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

\ --- declaration-time parameter-arity walk for SUM variant payloads. TFC-SCH-TERM
\ (construct/MATCH) indexes its minted-var scratch by parameter index with no
\ bounds guard, so a payload parameter beyond arity would be a silent
\ out-of-bounds read; this walk makes the arity invariant structural, with the
\ per-token TDECL-LETTER gate (sumtype.f) as the front line. It is not
\ PF-NODE-KIND?, which answers the subtree's kind and whether it is well
\ formed, not which parameter index is out of arity. This walk descends
\ SCH-APP arguments, the SCH-PTR pointee and SCH-QUOT rows, and answers the
\ first out-of-arity index, or -1 when the subtree is in range (indices are
\ 0..22).
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
   n 0 < n PF-MAX-N CELL / > or IF
      s" pf: a field's slot or cells is too large to count in bytes" EXIT-STATUS:INTERNAL die THEN
   n cells ;
: PF-CELL-SLOTS-REQUIRE ( n n n n n n -- )   \ a field laid out in whole cells
   {: slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   slot PF-CELL-BYTES boff <> IF
      s" pf: a field's byte-off is not its slot in bytes" EXIT-STATUS:INTERNAL die THEN
   flags PF-FLAG-BUFFER = IF                  \ a buffer's bytes end in its last cell
      bytesn 1 - CELL / 1 + cellsn <> IF
         s" pf: a buffer's cells do not end with its bytes" EXIT-STATUS:INTERNAL die THEN
   ELSE
      cellsn PF-CELL-BYTES bytesn <> IF
         s" pf: a field's bytes is not its cells in bytes" EXIT-STATUS:INTERNAL die THEN
   THEN
   al CELL <> IF s" pf: a field's align is not the cell size" EXIT-STATUS:INTERNAL die THEN ;
: PF-U8? ( n -- bool ) {: node:n :}   \ a buffer's schema: the con u8
   node SCHEMA-CON? IF node SCHEMA-A@ CC-U8 = ELSE RES-FALSE THEN ;

\ A field that is no buffer is as many cells as its schema is wide.
: PF-WIDTH-REQUIRE ( n n n -- ) {: fam:n sch:n cellsn:n :}
   fam sch SCHEMA-ROOT@ TFAM-SCH-WIDTH cellsn <> IF
      s" pf: a field's cells is not its schema's width" EXIT-STATUS:INTERNAL die THEN ;

\ --- entry reads (TFAM-FIND, below). A family read from its type entry is
\ first its head (ER-HEAD: declared, with its layout policy and slots), then,
\ after every family its entry names, its body (ER-BODY: variants, fields,
\ layout rows); TFAM-FIND decides both by where it is, so a family holds no
\ read state. MUTUAL members name each other, so a member's body can be read
\ while a member it holds by value has only its head and no fields, which
\ would measure 0 cells wide. The read as a whole is one machine, ER-STATE:
\   ER-IDLE  no entry is being read; PF-ADD checks a field's width as it adds it.
\   ER-OPEN  an entry is being read, from field row ER-MARK on; PF-ADD defers
\            each field's width check.
\ IDLE -> OPEN  the outermost TFAM-FIND starts reading an entry: ER-MARK := PF-N.
\ OPEN -> OPEN  a read inside it, which adds rows past ER-MARK.
\ OPEN -> IDLE  the outermost read returns, every body read; it then checks the
\               rows from ER-MARK on (PF-WIDTHS-REQUIRE): by-value containment
\               is acyclic, so every width they read is now whole.
\ OPEN -> IDLE  a throw leaves the outermost read, which rethrows it.
0 constant ER-IDLE
1 constant ER-OPEN
variable ER-STATE   ER-IDLE ER-STATE !
variable ER-MARK    0 ER-MARK !

\ The width check ER-OPEN deferred, for every field row from mark on.
: PF-WIDTHS-REQUIRE ( n -- ) {: mark:n :}
   PF-N @ mark ?do
      i PF-REC@ {: r:^ :}
      r PF.FLAGS @ PF-FLAG-BUFFER <> IF
         r PF.FAM @ r PF.SCH @ r PF.CELLS @ PF-WIDTH-REQUIRE
      THEN
   loop ;

: PF-OVERLAP? ( n n n n n n -- bool )
   {: fam:n var:n slot:n cellsn:n boff:n bytesn:n :}
   fam var FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - PF-REC@ {: r:^ :}
      fam var r PF-ROW-OWNER? IF
         slot cellsn r PF.SLOT @ r PF.CELLS @ PF-RANGE-OVERLAP? IF drop RES-TRUE EXIT THEN
         boff bytesn r PF.BYTE-OFF @ r PF.BYTES @ PF-RANGE-OVERLAP? IF drop RES-TRUE EXIT THEN
      THEN
      FVX-NEXT
   REPEAT
   drop RES-FALSE ;
: PF-LAYOUT-REQUIRE ( n n n n n n n n -- )
   {: fam:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   flags PF-FLAGS-NONE <> flags PF-FLAG-BUFFER <> and IF
      s" pf: field flags set undefined bits" EXIT-STATUS:INTERNAL die THEN
   slot cellsn PF-RANGE-OK? 0= IF
      s" pf: a field's slot or cells is out of range" EXIT-STATUS:INTERNAL die THEN
   boff bytesn PF-RANGE-OK? 0= IF
      s" pf: a field's byte-off or bytes is out of range" EXIT-STATUS:INTERNAL die THEN
   al PF-POW2? 0= IF s" pf: a field's align is not a power of two" EXIT-STATUS:INTERNAL die THEN
   boff al mod 0 <> IF
      s" pf: a field's byte-off is not a multiple of its align" EXIT-STATUS:INTERNAL die THEN
   flags PF-FLAG-BUFFER = IF
      sch SCHEMA-ROOT@ PF-U8? 0= IF
         s" pf: a buffer's schema is not u8" EXIT-STATUS:INTERNAL die
      THEN
   ELSE
      ER-STATE @ ER-IDLE = IF fam sch cellsn PF-WIDTH-REQUIRE THEN
   THEN
   fam TFAM-LAYOUT-POLICY@ {: policy:n :}
   policy TL-STACK-CELL-TAG =  policy TL-PACKED-TAG = or  policy TL-NICHE = or
   0= IF
      s" pf: a field was added to a family whose layout policy lays out no fields"
      EXIT-STATUS:INTERNAL die
   THEN
   slot cellsn boff bytesn al flags PF-CELL-SLOTS-REQUIRE ;
: PF-DUP? ( n n str -- bool ) {: fam:n var:n na:^ nu:n :}
   fam var FVX-FIRST
   BEGIN dup 0 <> WHILE
      dup 1 - PF-REC@ {: r:^ :}
      fam var r PF-ROW-OWNER? IF
         r PF.NAME-OFF @ r PF.NAME-U @ TF-OFF$ na nu CORE-STR= IF drop RES-TRUE EXIT THEN
      THEN
      FVX-NEXT
   REPEAT
   drop RES-FALSE ;

: PF-ADD ( n n str n n n n n n n -- )
   {: fam:n var:n na:^ nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   WRITE-WINDOW-CK
   fam var PF-OWNER-OK? 0= IF
      s" pf: a field's owner is not a product, or a sum or enum variant of the family"
      EXIT-STATUS:INTERNAL die
   THEN
   na nu PF-NAME-REQUIRE
   fam var na nu PF-DUP? IF E-TFAM-DUP throw THEN
   fam sch PF-SCHEMA-OK? 0= IF
      s" pf: a field's schema does not fit its family's parameter kinds" EXIT-STATUS:INTERNAL die
   THEN
   fam sch slot cellsn boff bytesn al flags PF-LAYOUT-REQUIRE
   fam var slot cellsn boff bytesn PF-OVERLAP? IF
      s" pf: a field overlaps another field of its owner" EXIT-STATUS:INTERNAL die THEN
   PF-ENSURE
   na nu TF-INTERN {: noff:n :}
   PF-N @ {: id:n :}
   id PF-REC@ {: r:^ :}
   fam r PF.FAM !   var r PF.VAR !
   noff r PF.NAME-OFF !   nu r PF.NAME-U !   sch r PF.SCH !
   slot r PF.SLOT !   cellsn r PF.CELLS !
   boff r PF.BYTE-OFF !   bytesn r PF.BYTES !
   al r PF.ALIGN !   flags r PF.FLAGS !   0 r PF.ACCESSOR-SYM !
   id PF-LINK                       \ chain the row under its owner before publishing it
   id 1 + PF-N !
   TFM-EPOCH+ ;

: PF-COMMIT ( -- ) WRITE-WINDOW-CK  PF-N @ PF-COMMIT-N !  TFM-EPOCH+ ;

\ --- generic member symbols (MBX). A field row holds its generated accessor's
\ symbol and a family row its AT's. A symbol's head is the row it was last
\ written to, field id * 2 + 2 or family id * 2 + 1, and is believed only
\ while that row is live and still holds the symbol: a retired row is out of
\ range and a row added again starts with none, so a rewind unlinks nothing.

variable MBX-GEN   0 MBX-GEN !

: MBX-LINK ( n n -- ) {: head:n sym:n :}
   sym IDX-SYM-OK
   head sym HT-MBX IDX-HEAD! ;

: MBX-BUILD ( -- )
   HT-MBX IDX-HEADS-CLEAR
   PF-N @ 0 ?do
      i PF-REC@ PF.ACCESSOR-SYM @ {: sym:n :}
      sym 0 <> IF i 2 * 2 + sym MBX-LINK THEN
   loop
   TFAM-N @ 0 ?do
      i TF-REC@ TF.AT-SYM @ {: sym:n :}
      sym 0 <> IF i 2 * 1 + sym MBX-LINK THEN
   loop
   HIDX-GEN @ MBX-GEN ! ;

: MBX-ENSURE ( -- )
   HIDX-ENSURE
   MBX-GEN @ HIDX-GEN @ <> IF MBX-BUILD THEN ;

: FIELD-SYM! ( n n -- ) {: fid:n sym:n :}
   sym 0= IF EXIT THEN
   sym fid PF-REC@ PF.ACCESSOR-SYM !
   MBX-ENSURE
   fid 2 * 2 + sym MBX-LINK ;

: AT-SYM! ( n n -- ) {: fam:n sym:n :}
   sym 0= IF EXIT THEN
   sym fam TF-REC@ TF.AT-SYM !
   MBX-ENSURE
   fam 2 * 1 + sym MBX-LINK ;

\ Records SYM as the generated word of member SHAPE PARAM: a field's accessor
\ (FIELD, SPAN, PARAM its field id) or a family's AT (INDEX, PARAM its id). Any
\ other shape is no member.
: MEMBER-SYM! ( n n n -- ) {: shape:n param:n sym:n :}
   shape CHILD-ABI:FIELD =  shape CHILD-ABI:SPAN = or IF param sym FIELD-SYM! EXIT THEN
   shape CHILD-ABI:INDEX = IF param sym AT-SYM! THEN ;

\ The shape and param of the member whose generated word is SYM; false for
\ any other symbol.
: SYM-MEMBER ( n -- n n bool ) {: sym:n :}
   sym 0 <= IF 0 0 RES-FALSE EXIT THEN
   MBX-ENSURE
   sym HT-MBX IDX-HEAD@ {: head:n :}
   head 0= IF 0 0 RES-FALSE EXIT THEN
   head 1 and 0 <> IF
      head 2 / {: fam:n :}
      fam TFAM-N @ < IF
         fam TF-REC@ TF.AT-SYM @ sym = IF CHILD-ABI:INDEX fam RES-TRUE EXIT THEN
      THEN
      0 0 RES-FALSE EXIT
   THEN
   head 2 / 1 - {: fid:n :}
   fid PF-N @ < IF
      fid PF-REC@ PF.ACCESSOR-SYM @ sym = IF
         fid PF-REC@ PF.FLAGS @ PF-FLAG-BUFFER = IF CHILD-ABI:SPAN ELSE CHILD-ABI:FIELD THEN
         fid RES-TRUE EXIT
      THEN
   THEN
   0 0 RES-FALSE ;

;package

package TYPE-FIELD
EXPORT TFAM:PF-N@
EXPORT TFAM:PF-NO-VARIANT

public

: COUNT ( -- n ) PF-N@ ;
: NO-VARIANT ( -- n ) PF-NO-VARIANT ;

seal-package
;package

\ generated ADDRESS accessors (`DERIVE addr`):
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

: TFAM-ADDR-FIELD-TAIL? ( str n -- bool ) {: a:^ u:n fam:n :}   \ tail names a COMMITTED field of fam
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: id:n :}
      id PF-N@ < IF
         id PF-NAME$ a u CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

: TFAM-ADDR-FIXED-TAIL? ( str -- bool ) {: a:^ u:n :}   \ a fixed addr member tail
   a u s" at" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" bytes" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" cells" CORE-STR=CI ;

: TFAM-ADDR-TAIL? ( str n -- bool ) {: a:^ u:n fam:n :}   \ member tail the FAMILY generates
   fam TFAM-DERIVE-ADDR? 0= IF RES-FALSE EXIT THEN
   a u TFAM-ADDR-FIXED-TAIL? IF RES-TRUE EXIT THEN
   a u fam TFAM-ADDR-FIELD-TAIL? ;

: TFAM-ADDR-AT? ( str n -- bool ) {: a:^ u:n id:n :}   \ split name = id-family accessor?
   TF-CW-HEAD$ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ id SUMV-FAM@ TFAM-ADDR-TAIL? ;

\ The private spelling carries no colon, so the member is recovered by deriving
\ each candidate spelling and comparing, as TFAM-PRIV-MEMBER? does.
: TFAM-ADDR-PRIV-FIXED? ( str n -- bool ) {: a:^ u:n fam:n :}
   a u  fam TFAM-NAME$ s" at"    TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE EXIT THEN
   a u  fam TFAM-NAME$ s" bytes" TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE EXIT THEN
   a u  fam TFAM-NAME$ s" cells" TF-CTOR-PRIV$ CORE-STR=CI ;

: TFAM-ADDR-PRIV-FIELD? ( str n -- bool ) {: a:^ u:n fam:n :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: id:n :}
      id PF-N@ < IF
         a u  fam TFAM-NAME$ id PF-NAME$ TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

: TFAM-ADDR-PRIV-AT? ( str n -- bool ) {: a:^ u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-GEN-PUBLIC? IF RES-FALSE EXIT THEN
   fam TFAM-DERIVE-ADDR? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u fam TFAM-ADDR-PRIV-FIXED? IF RES-TRUE EXIT THEN
   a u fam TFAM-ADDR-PRIV-FIELD? ;

\ Both spellings, over the variant rows every addr family owns (a STRUCTURE with
\ fields always has its make/unmake pair; visiting a family twice is free).
: TFAM-ADDR-QUAL-FIND ( str -- n bool ) {: a:^ u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-ADDR-AT? IF j SUMV-FAM@ RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop 0 RES-FALSE ;

: TFAM-ADDR-PRIV-FIND ( str -- n bool ) {: a:^ u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-ADDR-PRIV-AT? IF j SUMV-FAM@ RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop 0 RES-FALSE ;

: TFAM-ADDR-FIND ( str -- n bool ) {: a:^ u:n :}   \ accessor family
   a u TF-CW-SPLIT? IF a u TFAM-ADDR-QUAL-FIND EXIT THEN
   a u TFAM-ADDR-PRIV-FIND ;

\ A FIELD name is canonical, while the two public initialized members add a
\ final @ or !. Keep the suffix in its own buffer: TF-CTOR-PRIV$ builds into
\ TF-CTOR-BUF and must not overwrite its member argument.
PTR-VARIABLE TF-INIT-P
SCRATCH-VARIABLE TF-INIT-CAP     \ TF-INIT-P's room, in cells
: TF-INIT-TAIL ( -- ^u8 ) TF-INIT-P @ ;

: TFAM-INIT-MEMBER$ ( n bool -- str ) {: fid:n store:bool :}
   fid PF-NAME$ {: a:^ u:n :}
   TF-INIT-P TF-INIT-CAP  u CELL / 1 +  1 ARENA-ROWS-ENSURE
   u 0 ?do a i + c@ TF-UPPER-C TF-INIT-TAIL i + c! loop
   store IF 33 ELSE 64 THEN TF-INIT-TAIL u + c!
   TF-INIT-TAIL u 1 + ;

: TFAM-INIT-TAIL? ( str n -- bool ) {: a:^ u:n fam:n :}
   fam TFAM-DERIVE-INIT? 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: fid:n :}
      fid PF-N@ < IF
         a u fid RES-FALSE TFAM-INIT-MEMBER$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
         a u fid RES-TRUE TFAM-INIT-MEMBER$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

: TFAM-INIT-QUAL-AT? ( str n -- bool ) {: a:^ u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-GEN-PUBLIC? 0= IF RES-FALSE EXIT THEN
   TF-CW-HEAD$ id SUMV-CTOR-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   a u TF-CW-TAIL$ fam TFAM-INIT-TAIL? ;

: TFAM-INIT-PRIV-AT? ( str n -- bool ) {: a:^ u:n id:n :}
   id SUMV-FAM@ {: fam:n :}
   fam TFAM-DERIVE-INIT? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? 0= IF RES-FALSE EXIT THEN
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fs i + {: fid:n :}
      fid PF-N@ < IF
         fam TFAM-GEN-PUBLIC? 0= IF
            a u fam TFAM-NAME$ fid RES-FALSE TFAM-INIT-MEMBER$
               TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
            a u fam TFAM-NAME$ fid RES-TRUE TFAM-INIT-MEMBER$
               TF-CTOR-PRIV$ CORE-STR=CI IF RES-TRUE unloop EXIT THEN
         THEN
      THEN
   loop RES-FALSE ;

: TFAM-INIT-QUAL-FIND ( str -- n bool ) {: a:^ u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-INIT-QUAL-AT? IF j SUMV-FAM@ RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop 0 RES-FALSE ;

: TFAM-INIT-PRIV-FIND ( str -- n bool ) {: a:^ u:n :}
   0 BEGIN dup SUMV-N @ < WHILE
      {: j:n :}
      a u j TFAM-INIT-PRIV-AT? IF j SUMV-FAM@ RES-TRUE EXIT THEN
      j 1 +
   REPEAT drop 0 RES-FALSE ;

: TFAM-INIT-FIND ( str -- n bool ) {: a:^ u:n :}
   a u TF-CW-SPLIT? IF a u TFAM-INIT-QUAL-FIND EXIT THEN
   a u TFAM-INIT-PRIV-FIND ;

: TFAM-ADDR-FIND-INSTALL ( -- )
   [: 2dup TFAM-ADDR-FIND IF nip nip RES-TRUE EXIT THEN
      drop TFAM-INIT-FIND ;] is TFAM-ADDR-FIND-XT ;
TFAM-ADDR-FIND-INSTALL

;package

package TFAM
private

\ Conservative schema linearity for declaration and cast fences. Exact value
\ counts below instantiate fields, including repeated parameter occurrences.
\ Pointer and quotation nodes are non-owning boundaries.
defer TFCL-NODE-XT ( n -- bool )

: TFAM-CONCRETE-LINEAR-WALK? ( n -- bool ) {: fam:n :}
   fam TFAM-SUM? fam TFAM-ENUM? or {: sum:bool :}
   fam TFAM-PRODUCT?  sum fam TFAM-FLD-COUNT@ 0 > and  or IF
      0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
         fam TFAM-FLD-START@ over + PF-REC@ PF.SCH @ SCHEMA-ROOT@ TFCL-NODE-XT IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      RES-FALSE EXIT
   THEN
   sum IF
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

5 cells constant LAY-REC

: LAY.FAM ( ^a -- ^a ) LAY.FAM-OFF + ;
: LAY.POLICY ( ^a -- ^a ) LAY.POLICY-OFF + ;
: LAY.SIZE ( ^a -- ^a ) LAY.SIZE-OFF + ;
: LAY.ALIGN ( ^a -- ^a ) LAY.ALIGN-OFF + ;
: LAY.TAGW ( ^a -- ^a ) LAY.TAGW-OFF + ;


4 constant LAY-CAP-INIT
variable LAY-CAP-V   LAY-CAP-INIT LAY-CAP-V !
: LAY-CAP ( -- n ) LAY-CAP-V @ ;
create LAY-A-BOOT   LAY-CAP-INIT LAY-REC * allot
PERSISTED-PTR-VARIABLE LAY-A-P   LAY-A-BOOT LAY-A-P !
: LAY-BASE ( -- ^n ) LAY-A-P @ ;

variable LAY-N   0 LAY-N !

: LAY-GROW ( n -- ) {: need:n :}
   need LAY-CAP-V @ 2 * max {: nc:n :}
   LAY-A-P  LAY-CAP-V @ LAY-REC *  nc LAY-REC *  GROW
   nc LAY-CAP-V ! ;
: LAY-ENSURE ( -- )
   LAY-N @ LAY-CAP-V @ < IF exit THEN
   LAY-N @ 1 + LAY-GROW ;
: LAY-SCRUB ( n n -- ) {: lo:n hi:n :}    \ zero the layout records [lo,hi); see PF-SCRUB
   LAY-BASE  lo LAY-REC CELL / *  hi LAY-REC CELL / *  ARENA-CELLS-ZERO ;
: LAY-REC@ ( n -- ^n ) {: id:n :}
   id LAY-REC * LAY-BASE + ;

: LAY-FAM@ ( n -- n ) LAY-REC@ LAY.FAM @ ;
: LAY-POLICY@ ( n -- n ) LAY-REC@ LAY.POLICY @ ;
: LAY-SIZE@ ( n -- n ) LAY-REC@ LAY.SIZE @ ;
: LAY-ALIGN@ ( n -- n ) LAY-REC@ LAY.ALIGN @ ;
: LAY-TAGW@ ( n -- n ) LAY-REC@ LAY.TAGW @ ;

: LAY-FIND ( n -- n bool ) {: fam:n :}
   0 BEGIN dup LAY-N @ < WHILE
      {: i:n :}
      i LAY-FAM@ fam = IF i RES-TRUE EXIT THEN
      i 1 +
   REPEAT drop
   0 RES-FALSE ;

: LAY-ADD ( n n n n n -- n ) {: fam:n p:n sz:n al:n tw:n :}
   WRITE-WINDOW-CK
   p 0 < p TL-MAX > or IF s" tfam: layout policy out of range" EXIT-STATUS:INTERNAL die THEN
   fam LAY-FIND IF drop E-TFAM-DUP throw THEN drop   \ drop the id from FIND's (id-or-0 flag)
   LAY-ENSURE
   LAY-N @ {: id:n :}
   id 1 + LAY-N !
   id LAY-REC@ {: r:^ :}
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

\ The committed init field carries T's schema and stack width. Its field row is
\ canonical cell storage, while the value being initialized may have a narrower
\ packed memory descriptor.
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

\ A retired family read from its type entry can be read again.
: ENTRIES-RETIRED ( n n -- ) {: lo:n hi:n :}
   hi lo ?do
      i TF-REC@ TF.ENTRY @ 0 <> IF -1 ENTRIES-PENDING ! THEN
   loop ;

: TFAM-RESET ( -- )
   0 SVX-GEN !                           \ every variant row the index points at is going
   0 TFX-READY !                         \ ... and every family row the tail index chains
   0 VNX-READY !                         \ ... and every row the variant tail index chains
   0 FVX-READY !                         \ ... and every row the field owner index chains
   TFM-EPOCH+                            \ ... and every family a memoized fact describes
   0 TFAM-N @ ENTRIES-RETIRED
   0 TFAM-N @ TF-SCRUB   0 TF-STR-U @ TF-STR-SCRUB   0 TF-PK-N @ TF-PK-SCRUB
   0 SUMV-N @ SUMV-SCRUB   0 PF-N @ PF-SCRUB   0 LAY-N @ LAY-SCRUB   \ scrub the rows it retires
   0 TFAM-N !   0 TF-STR-U !   0 TF-PK-N !
   0 SUMV-N !   0 SVX-HI !   0 PF-N !   0 PF-COMMIT-N !   0 LAY-N !
   -1 FIELD-FAM !  -1 LOAN-FIELD-FAM ! ;
TFAM-RESET

\ The registry rewind: every caller that puts the family and variant stores or
\ the name pool back to an earlier mark goes through here. Four indexes chain
\ rows by id (TFAM-FIND-IN's tail index, SUMV-FROM-CTOR-SYM's symbol index,
\ SUMV-FIND's and TF-CTOR-QUAL-FIND's variant indexes); a chained row whose id
\ is out of range is found by the next lookup, which reads past the store.
\ So rows are unchained before the counters move.
\ The retired family, variant, param-kind and layout rows and pool bytes are
\ scrubbed to zero, as PF-SCRUB does for field rows, after the indexes pop: the
\ pops read the tail links and constructor symbols the scrub clears.
\ Product-field rows and schema nodes have their own frames; a caller that owns
\ them rewinds them after this (field rows through PF-REWIND).
: TFAM-REWIND ( n n n n n -- ) {: tfamn:n stru:n pkn:n sumvn:n layn:n :}
   tfamn TFAM-N @ ENTRIES-RETIRED
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

0 cells constant TFRB.TFAMN-OFF
1 cells constant TFRB.STRU-OFF
2 cells constant TFRB.PKN-OFF
3 cells constant TFRB.SUMVN-OFF
4 cells constant TFRB.PFN-OFF
5 cells constant TFRB.LAYN-OFF
6 cells constant TFRB.PFCOMMITN-OFF

7 cells constant TF-RBF-REC
: TFRB.TFAMN ( ^a -- ^a ) TFRB.TFAMN-OFF + ;
: TFRB.STRU ( ^a -- ^a ) TFRB.STRU-OFF + ;
: TFRB.PKN ( ^a -- ^a ) TFRB.PKN-OFF + ;
: TFRB.SUMVN ( ^a -- ^a ) TFRB.SUMVN-OFF + ;
: TFRB.PFN ( ^a -- ^a ) TFRB.PFN-OFF + ;
: TFRB.LAYN ( ^a -- ^a ) TFRB.LAYN-OFF + ;
: TFRB.PFCOMMITN ( ^a -- ^a ) TFRB.PFCOMMITN-OFF + ;


16 constant TF-RBF-CAP-INIT
variable TF-RBF-CAP-V   TF-RBF-CAP-INIT TF-RBF-CAP-V !
create TF-RBF-BOOT   TF-RBF-CAP-INIT TF-RBF-REC * allot
\ Private: `0 TF-RBF-P !` from a program took the engine down with SIGSEGV
\ (rc 134) inside the next declaration.
PERSISTED-PTR-VARIABLE TF-RBF-P    TF-RBF-BOOT TF-RBF-P !
: TF-RBF-BASE ( -- ^n ) TF-RBF-P @ ;

variable TF-RBF-DEPTH   0 TF-RBF-DEPTH !

: TF-RBF-GROW ( -- )
   TF-RBF-CAP-V @ 2 * {: nc:n :}
   TF-RBF-P  TF-RBF-CAP-V @ TF-RBF-REC *  nc TF-RBF-REC *  GROW
   nc TF-RBF-CAP-V ! ;

: TF-RBF-ENSURE ( -- )
   TF-RBF-DEPTH @ TF-RBF-CAP-V @ < IF exit THEN
   TF-RBF-GROW ;
: TF-RBF-CUR ( -- ^n ) TF-RBF-DEPTH @ TF-RBF-REC * TF-RBF-BASE + ;
: TF-RBF-TOP ( -- ^n )
   TF-RBF-DEPTH @ 1 - TF-RBF-REC * TF-RBF-BASE + ;

;package

package CHECKER-DECL-FRAME
EXPORT TFAM:LAY-N
EXPORT TFAM:PF-COMMIT-N
EXPORT TFAM:PF-N
EXPORT TFAM:PF-REWIND
EXPORT TFAM:SUMV-N
EXPORT TFAM:TF-PK-N
EXPORT TFAM:TF-RBF-CUR
EXPORT TFAM:TF-RBF-DEPTH
EXPORT TFAM:TF-RBF-ENSURE
EXPORT TFAM:TF-RBF-TOP
EXPORT TFAM:TF-STR-U
EXPORT TFAM:TFAM-N
EXPORT TFAM:TFAM-REWIND
EXPORT TFAM:TFRB.LAYN
EXPORT TFAM:TFRB.PFCOMMITN
EXPORT TFAM:TFRB.PFN
EXPORT TFAM:TFRB.PKN
EXPORT TFAM:TFRB.STRU
EXPORT TFAM:TFRB.SUMVN
EXPORT TFAM:TFRB.TFAMN
EXPORT SCHEMA-REG:SCH-RBF-DEPTH
EXPORT SCHEMA-REG:SCHEMA-ROLLBACK-FINALIZE
EXPORT SCHEMA-REG:SCHEMA-ROLLBACK-RESTORE
EXPORT SCHEMA-REG:SCHEMA-ROLLBACK-SAVE

: TF-RELEASE ( -- )
   TF-RBF-DEPTH @ 1 - TF-RBF-DEPTH ! ;

: TF-SAVE ( -- )
   TF-RBF-ENSURE
   TF-RBF-CUR {: r:^ :}
   TFAM-N @ r TFRB.TFAMN !
   TF-STR-U @ r TFRB.STRU !
   TF-PK-N @ r TFRB.PKN !
   SUMV-N @ r TFRB.SUMVN !
   PF-N @ r TFRB.PFN !
   LAY-N @ r TFRB.LAYN !
   PF-COMMIT-N @ r TFRB.PFCOMMITN !
   TF-RBF-DEPTH @ 1 + TF-RBF-DEPTH ! ;

: TF-RESTORE-TOP ( ^n -- )
   TF-RELEASE
   {: r:^ :}
   r TFRB.TFAMN @  r TFRB.STRU @  r TFRB.PKN @  r TFRB.SUMVN @  r TFRB.LAYN @
   TFAM-REWIND                        \ retires the three indexes, then moves the counters
   r TFRB.PFN @ r TFRB.PFCOMMITN @ PF-REWIND ;   \ the field rows this rejected declaration added

: TF-RESTORE ( -- )
   TF-RBF-TOP TF-RESTORE-TOP ;

: TF-FINALIZE ( -- )
   TF-RELEASE ;

\ One SAVE/RESTORE pair the checker's core RBF-PUSH/POP drives, so TFAM and
\ SCHEMA frames stay in lockstep with core marks.
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

;package

package TFAM
private

: REG-EXT-AOT-INSTALL ( -- )
   [: TFAM-NAME$ ;] is REG-EXT-AOT-FAMILY-NAME-XT ;
REG-EXT-AOT-INSTALL

\ Built-in parametric cell families: the parser's parametric type constructors.
\ Each is public and global (empty package), so a bare `span<...>` resolves
\ through TFAM-RESOLVE from any scope. Registration runs at prefix load in every
\ context (preverify parent and runtime child), so both see identical families.

: TFAM-REG-CELL ( str n -- )
   {: na:^ nu:n ar:n :}
   s" " CHECKER-PACKAGE-PUBLIC na nu ar TK-CELL TFAM-DECL drop ;

\ `ptr<space,elem>` is a parametric pointer (T-PARAM, resolved here); the plain
\ pointer is `^elem` (MK-PTR, checker.f SIG-TYPE), and a `ptr` without `<` names
\ no type. Registered arity 2 matches every `ptr<...>` in the tree.
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
\ ix<e> -> redx<e> (src/lib/type/extent-role.f `>RED`) introduces redx.
s" ix"         1 TFAM-REG-CELL
s" extprod"    2 TFAM-REG-CELL
s" redx"       1 TFAM-REG-CELL

\ The tile/uniform family ids go into the checker's barrier-uniformity cells
\ (declared in checker.f). A collective typed ( tile<..> -- uniform<..> ) emits
\ bar.sync and is sound only under block-uniform control (checker.f
\ PTX-BARRIER-ROWS?/BARRIER-CUR?). Runs in every load context, so the parent
\ verifier and runtime child agree.

: PTX-FAM-ID ( str -- n )   \ resolve a GLOBAL family name to its id (0 if none)
   {: na:^ nu:n :}
   s" " na nu TFAM-FIND-IN IF ELSE drop 0 THEN ;
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
\ They are family records rather than STRUCTURE declarations: no constructor,
\ destructor, or address accessor is published. The fields describe their ABI
\ for width-aware transport and image replay; checked code cannot project them.
variable READ-FAM
variable MUT-FAM
variable INIT-FAM
variable RECORDS-FAM
variable STOW-FAM
variable STRING-FAM
variable VIEW-FIELD-BASE

: REGISTER-VIEW-FIELDS ( n -- ) {: fam:n :}
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   fam TYPE-FIELD:NO-VARIANT s" base"
      CC-U8 SCHEMA-CON SCHEMA-PTR SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE PF-ADD
   fam TYPE-FIELD:NO-VARIANT s" bound"
      CC-LEN SCHEMA-CON SCHEMA-ROOT+
      1 1 CELL CELL CELL PF-FLAGS-NONE PF-ADD
   PF-COMMIT
   fam VIEW-FIELD-BASE @ 2 TFAM-FLD-RANGE!
   fam 2 TFAM-SLOTS! ;

: REGISTER-READ-VIEW ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" read-view" 3 TK-PRODUCT TFAM-DECL READ-FAM !
   READ-FAM @ C2-READ-FAM !
   READ-FAM @ 0 PK-SCOPE TFAM-PK!
   READ-FAM @ 1 PK-SCOPE TFAM-PK!
   READ-FAM @ 2 PK-TYPE TFAM-PK!
   READ-FAM @ REGISTER-VIEW-FIELDS ;

\ str is a native two-cell product, base address below byte length; it
\ publishes no constructor, destructor or field accessor.
: REGISTER-STR ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" str" 0 TK-PRODUCT TFAM-DECL STRING-FAM !
   STRING-FAM @ STR-FAM !
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   STRING-FAM @ TYPE-FIELD:NO-VARIANT s" base"
      CC-U8 SCHEMA-CON SCHEMA-PTR SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE PF-ADD
   STRING-FAM @ TYPE-FIELD:NO-VARIANT s" len"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      1 1 CELL CELL CELL PF-FLAGS-NONE PF-ADD
   PF-COMMIT
   STRING-FAM @ VIEW-FIELD-BASE @ 2 TFAM-FLD-RANGE!
   STRING-FAM @ 2 TFAM-SLOTS! ;

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
   INIT-FAM @ TYPE-FIELD:NO-VARIANT s" value"
      1 SCHEMA-PARAM SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE PF-ADD
   PF-COMMIT
   INIT-FAM @ VIEW-FIELD-BASE @ 1 TFAM-FLD-RANGE!
   INIT-FAM @ 1 TFAM-SLOTS! ;

\ `width<t>`: one cell holding the bytes t takes in storage. Its slot is
\ PK-TYPE, so t may be a type of any width, a record's included.
: REGISTER-WIDTH ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" width" 1 TK-CELL TFAM-DECL WIDTH-FAM !
   WIDTH-FAM @ 0 PK-TYPE TFAM-PK! ;

\ `EFFECT:type`: a definer's type value, a handle to a transient schema node
\ the checker made, which no storage holds.
: REGISTER-EFFECT-TYPE ( -- )
   s" EFFECT" CHECKER-PACKAGE-PUBLIC s" type" 0 TK-CELL TFAM-DECL
   EFFECT-TYPE-FAM ! ;


: REGISTER-RECORDS ( -- )
   s" " CHECKER-PACKAGE-PUBLIC s" records" 4 TK-PRODUCT TFAM-DECL RECORDS-FAM !
   RECORDS-FAM @ C2-RECORDS-FAM !
   RECORDS-FAM @ 0 PK-SCOPE TFAM-PK!
   RECORDS-FAM @ 1 PK-SCOPE TFAM-PK!
   RECORDS-FAM @ 2 PK-REGION TFAM-PK!
   RECORDS-FAM @ 3 PK-TYPE TFAM-PK!
   RECORDS-FAM @ REGISTER-VIEW-FIELDS ;

\ The runtime's three machine descriptors form one private value.
: REGISTER-STOW ( -- )
   s" C2-MEM" CHECKER-PACKAGE-PRIVATE s" stow-layout" 0 TK-PRODUCT TFAM-DECL STOW-FAM !
   STOW-FAM @ C2-STOW-FAM !
   TYPE-FIELD:COUNT VIEW-FIELD-BASE !
   STOW-FAM @ TYPE-FIELD:NO-VARIANT s" width"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      0 1 0 CELL CELL PF-FLAGS-NONE PF-ADD
   STOW-FAM @ TYPE-FIELD:NO-VARIANT s" bytes"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      1 1 CELL CELL CELL PF-FLAGS-NONE PF-ADD
   STOW-FAM @ TYPE-FIELD:NO-VARIANT s" alignment"
      CC-N SCHEMA-CON SCHEMA-ROOT+
      2 1 2 CELL * CELL CELL PF-FLAGS-NONE PF-ADD
   PF-COMMIT
   STOW-FAM @ VIEW-FIELD-BASE @ 3 TFAM-FLD-RANGE!
   STOW-FAM @ 3 TFAM-SLOTS! ;

REGISTER-READ-VIEW
REGISTER-MUT-VIEW
REGISTER-INIT
REGISTER-WIDTH
REGISTER-EFFECT-TYPE
REGISTER-RECORDS
REGISTER-STOW
REGISTER-STR

\ --- type entries. Inside a build each family a file declares gets a record,
\ its type entry, named `pkg:tail` (a global's `tail`) in the program's types
\ wordlist BOOT:TYPES-WL; its facts text is the family's rows written out
\ (ENTRY-TEXT$). A lookup that misses reads the entry into the registry
\ (TFAM-FIND). A declaration first reads every entry the registry lacks
\ (READ-PENDING): a read inside one would land among its own rows.
\
\ The text is tokens split by one blank, `-` for an empty name, a family
\ written `pkg:tail`:
\   kind vis arity pk... layout slots tagw derive opaque scratch at-sym
\   ndeps family...   (every other family its schemas name, read first)
\   nvar (name tag paycells ctor-pkg ctor-sym nroots node...)...
\   nfld (name variant slot cells byte-off bytes align flags accessor-sym node)...
\   nlay (policy size align tagw)...
\ A ctor-sym, at-sym or accessor-sym is `-` or `vis pkg tail`; a field's variant is its ordinal, -1
\ for none. A node is `P index`, `C con`, `* node`, `A family count node...`,
\ `Q hasr node node node node` or `R count node...`.

\ The word every type entry's record runs. A type entry lives in the types
\ wordlist, which no search order holds, so only the engine could run one.
: TYPE-WORD ( -- ) s" tfam: a type entry ran as a word" EXIT-STATUS:INTERNAL die ;

: ENTRY? ( n -- bool ) TF-REC@ TF.ENTRY @ 0 <> ;
\ Family id's type entry is record ix.
: ENTRY! ( n n -- ) {: id:n ix:n :} ix 1 + id TF-REC@ TF.ENTRY ! ;
\ The linker made a type entry the registry lacks.
: ENTRY-LINKED ( -- ) -1 ENTRIES-PENDING ! ;

PTR-VARIABLE ET-BUF   SCRATCH-VARIABLE ET-CAP   variable ET-N    \ the text being written
PTR-VARIABLE EN-BUF   SCRATCH-VARIABLE EN-CAP   variable EN-N    \ an entry's name
PTR-VARIABLE ED-BUF   SCRATCH-VARIABLE ED-CAP   variable ED-N    \ a family's dependencies
PTR-VARIABLE ES-BUF   SCRATCH-VARIABLE ES-CAP   variable ES-N    \ schema nodes read, not yet rooted
create ET-CH 8 allot

\ Appends a u to the byte store pv, whose length cell is nv.
: ENTRY-S, ( str ^^u8 ^n ^n -- )
   {: a:^ u:n pv:^ capv:^ nv:^ :}
   pv capv  nv @ u +  ARENA-BYTES-ENSURE
   u 0 ?do a i + c@  pv @ nv @ i + +  c! loop
   nv @ u + nv ! ;

: ET-S, ( str -- ) ET-BUF ET-CAP ET-N ENTRY-S, ;
: ET-C, ( n -- ) ET-CH c!  ET-CH 1 ET-S, ;
: ET-SEP ( -- ) ET-N @ 0 <> IF 32 ET-C, THEN ;


: ET-DIGITS ( n -- ) {: m:n :}
   m 9 > IF m 10 / recurse THEN
   m 10 mod 48 + ET-C, ;


: ET-NUM ( n -- ) {: v:n :}
   ET-SEP
   v 0 < 0= IF v ET-DIGITS EXIT THEN
   v negate 0 < IF s" tfam: type entry number out of range" EXIT-STATUS:INTERNAL die THEN
   45 ET-C,  v negate ET-DIGITS ;


: ET-NAME ( str -- ) {: a:^ u:n :}
   ET-SEP
   u 0= IF 45 ET-C, EXIT THEN
   a u ET-S, ;


: ET-FAM ( n -- ) {: id:n :}
   ET-SEP
   id TFAM-PKG$ {: pa:^ pu:n :}
   pu 0 <> IF pa pu ET-S,  58 ET-C, THEN
   id TFAM-NAME$ ET-S, ;


: EN-NAME ( str str -- str ) {: pa:^ pu:n na:^ nu:n :}
   0 EN-N !
   pu 0 <> IF
      pa pu EN-BUF EN-CAP EN-N ENTRY-S,
      s" :" EN-BUF EN-CAP EN-N ENTRY-S,
   THEN
   na nu EN-BUF EN-CAP EN-N ENTRY-S,
   EN-BUF @ EN-N @ ;


: ED-HAS? ( n -- bool ) {: fam:n :}
   ED-N @ 0 ?do
      ED-BUF @ i cells + @ fam = IF RES-TRUE unloop EXIT THEN
   loop
   RES-FALSE ;


: ED-ADD ( n n -- ) {: fam:n self:n :}
   fam self = IF EXIT THEN
   fam ED-HAS? IF EXIT THEN
   ED-BUF ED-CAP ED-N @ 1 + cells ARENA-BYTES-ENSURE
   fam ED-BUF @ ED-N @ cells + !
   ED-N @ 1 + ED-N ! ;


\ Adds the families schema node names, but self.
: ED-NODE ( n n -- ) {: node:n self:n :}
   node SCHEMA-TAG@ {: tag:n :}
   tag SCH-PTR = IF node SCHEMA-A@ self recurse EXIT THEN
   tag SCH-APP = IF
      node SCHEMA-A@ self ED-ADD
      node SCHEMA-C@ 0 ?do node SCHEMA-B@ i + SCHEMA-ROOT@ self recurse loop EXIT
   THEN
   tag SCH-QUOT = IF
      node SCHEMA-C@ 0 ?do node SCHEMA-B@ i + SCHEMA-ROOT@ self recurse loop EXIT
   THEN
   tag SCH-ROW = IF
      node SCHEMA-B@ 0 ?do node SCHEMA-A@ i + SCHEMA-ROOT@ self recurse loop
   THEN ;


: ED-VARIANT ( n n -- ) {: v:n self:n :}
   v SUMV-SCH-COUNT@ 0 ?do v SUMV-SCH-START@ i + SCHEMA-ROOT@ self ED-NODE loop ;


: ED-COLLECT ( n -- ) {: id:n :}
   0 ED-N !
   id TFAM-VAR-COUNT@ 0 ?do id TFAM-VAR-START@ i + id ED-VARIANT loop
   id TFAM-FLD-COUNT@ 0 ?do
      id TFAM-FLD-START@ i + PF-SCH@ SCHEMA-ROOT@ id ED-NODE
   loop ;


: ET-NODE ( n -- ) {: node:n :}
   node SCHEMA-TAG@ {: tag:n :}
   tag SCH-PARAM = IF s" P" ET-NAME node SCHEMA-A@ ET-NUM EXIT THEN
   tag SCH-CON = IF s" C" ET-NAME node SCHEMA-A@ CT-NAME$ ET-NAME EXIT THEN
   tag SCH-PTR = IF s" *" ET-NAME node SCHEMA-A@ recurse EXIT THEN
   tag SCH-APP = IF
      s" A" ET-NAME node SCHEMA-A@ ET-FAM node SCHEMA-C@ ET-NUM
      node SCHEMA-C@ 0 ?do node SCHEMA-B@ i + SCHEMA-ROOT@ recurse loop EXIT
   THEN
   tag SCH-QUOT = IF
      s" Q" ET-NAME node SCHEMA-A@ ET-NUM
      node SCHEMA-C@ 0 ?do node SCHEMA-B@ i + SCHEMA-ROOT@ recurse loop EXIT
   THEN
   tag SCH-ROW = IF
      s" R" ET-NAME node SCHEMA-B@ ET-NUM
      node SCHEMA-B@ 0 ?do node SCHEMA-A@ i + SCHEMA-ROOT@ recurse loop EXIT
   THEN
   s" tfam: type entry of a bad schema node" EXIT-STATUS:INTERNAL die ;


: ET-SYM ( n -- ) {: sym:n :}
   sym 0= IF s" " ET-NAME EXIT THEN
   sym CHECKER-SYM-IDENTITY {: pa:^ pu:n ta:^ tu:n vis:n :}
   vis ET-NUM  pa pu ET-NAME  ta tu ET-NAME ;


: ET-VARIANT ( n -- ) {: v:n :}
   v SUMV-NAME$ ET-NAME  v SUMV-TAG@ ET-NUM  v SUMV-RAW-PAYCELLS@ ET-NUM
   v SUMV-CTOR-PKG$ ET-NAME  v SUMV-CTOR-SYM@ ET-SYM
   v SUMV-SCH-COUNT@ ET-NUM
   v SUMV-SCH-COUNT@ 0 ?do v SUMV-SCH-START@ i + SCHEMA-ROOT@ ET-NODE loop ;


: ET-FIELD ( n n -- ) {: f:n vstart:n :}
   f PF-NAME$ ET-NAME
   f PF-VAR@ dup PF-NO-VARIANT = IF ELSE vstart - THEN ET-NUM
   f PF-SLOT@ ET-NUM  f PF-CELLS@ ET-NUM  f PF-BYTE-OFF@ ET-NUM
   f PF-BYTES@ ET-NUM  f PF-ALIGN@ ET-NUM  f PF-FLAGS@ ET-NUM
   f PF-REC@ PF.ACCESSOR-SYM @ ET-SYM
   f PF-SCH@ SCHEMA-ROOT@ ET-NODE ;


: ET-LAY ( n -- ) {: l:n :}
   l LAY-POLICY@ ET-NUM  l LAY-SIZE@ ET-NUM  l LAY-ALIGN@ ET-NUM  l LAY-TAGW@ ET-NUM ;

\ Family id's entry name.
: ENTRY-NAME$ ( n -- str ) {: id:n :} id TFAM-PKG$ id TFAM-NAME$ EN-NAME ;


\ Family id's rows as its entry's text, held until the next text is written.
: ENTRY-TEXT$ ( n -- str ) {: id:n :}
   id ED-COLLECT
   0 ET-N !
   id TFAM-KIND@ ET-NUM  id TFAM-VIS@ ET-NUM  id TFAM-ARITY@ ET-NUM
   id TFAM-ARITY@ 0 ?do id i TFAM-PK@ ET-NUM loop
   id TFAM-LAYOUT-POLICY@ ET-NUM  id TFAM-SLOTS@ ET-NUM  id TFAM-TAGW@ ET-NUM
   id TFAM-DERIVE@ ET-NUM  id TF-REC@ TF.OPAQUE @ ET-NUM
   id TF-REC@ TF.AT-SYM @ ET-SYM
   ED-N @ ET-NUM
   ED-N @ 0 ?do ED-BUF @ i cells + @ ET-FAM loop
   id TFAM-VAR-COUNT@ ET-NUM
   id TFAM-VAR-COUNT@ 0 ?do id TFAM-VAR-START@ i + ET-VARIANT loop
   id TFAM-FLD-COUNT@ ET-NUM
   id TFAM-FLD-COUNT@ 0 ?do id TFAM-FLD-START@ i + id TFAM-VAR-START@ ET-FIELD loop
   id LAY-FIND IF 1 ET-NUM ET-LAY ELSE drop 0 ET-NUM THEN
   ET-BUF @ ET-N @ ;

PTR-VARIABLE ER-P   variable ER-U          \ the rest of the text being read
PTR-VARIABLE ER-NAME   variable ER-NAME-U  \ its entry's name
200 constant ER-MSG-CAP
create ER-MSG ER-MSG-CAP allot

\ A text this tree's writer did not write: a broken build, or a first-generation
\ engine whose host laid its entries in an older ENTRY-TEXT$ text.
: ER-BAD ( -- )
   s" tfam: bad type entry " {: a:^ u:n :}
   u 0 ?do a i + c@ ER-MSG i + c! loop
   ER-NAME-U @ ER-MSG-CAP u - min {: k:n :}
   k 0 ?do ER-NAME @ i + c@ ER-MSG u + i + c! loop
   ER-MSG u k + EXIT-STATUS:INTERNAL die ;


\ The index of the last ':' in a name, -1 for none.
: LAST-COLON ( str -- n ) {: a:^ u:n :}
   -1 0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ 58 = IF drop i THEN
      i 1 +
   REPEAT drop ;


\ `pkg:tail` as package and tail, split at the last colon; a bare tail is
\ package "". False for an edge colon.
: SPLIT-NAME ( str -- str str bool ) {: a:^ u:n :}
   a u LAST-COLON {: c:n :}
   c 0 < IF a 0 a u RES-TRUE EXIT THEN
   c 0 = c u 1 - = or IF a 0 a u RES-FALSE EXIT THEN
   a c  a c + 1 +  u c - 1 -  RES-TRUE ;


\ The bytes before the first blank.
: ER-LEN ( str -- n ) {: a:^ u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ 32 = IF EXIT THEN
      1 +
   REPEAT ;


: ER-TOKEN ( -- str )
   ER-U @ 1 < IF ER-BAD THEN
   ER-P @ ER-U @ {: a:^ u:n :}
   a u ER-LEN {: k:n :}
   k 0= IF ER-BAD THEN
   k u < IF a k + 1 + ER-P !  u k - 1 - ER-U !
   ELSE a u + ER-P !  0 ER-U ! THEN
   a k ;


: ER-DIGITS ( str -- n ) {: a:^ u:n :}
   u 1 < IF ER-BAD THEN
   0 0 BEGIN dup u < WHILE
      {: acc:n i:n :}
      a i + c@ 48 - {: d:n :}
      d 0 < d 9 > or IF ER-BAD THEN
      acc 10 * d +  i 1 +
   REPEAT drop ;


: ER-INT ( str -- n ) {: a:^ u:n :}
   a c@ 45 = IF a 1 + u 1 - ER-DIGITS negate EXIT THEN
   a u ER-DIGITS ;
: ER-NUM ( -- n ) ER-TOKEN ER-INT ;


: ER-NAME$ ( -- str )
   ER-TOKEN {: a:^ u:n :}
   u 1 = IF a c@ 45 = IF a 0 EXIT THEN THEN
   a u ;


\ A family the registry holds, by its exact name.
: ER-FAM ( -- n )
   ER-TOKEN SPLIT-NAME 0= IF ER-BAD THEN
   TFAM-FIND-IN 0= IF ER-BAD THEN ;


: ES-PUSH ( n -- ) {: node:n :}
   ES-BUF ES-CAP ES-N @ 1 + cells ARENA-BYTES-ENSURE
   node ES-BUF @ ES-N @ cells + !
   ES-N @ 1 + ES-N ! ;
: ES@ ( n -- n ) {: k:n :} ES-BUF @ k cells + @ ;


\ Roots the nodes staged from mark on, contiguously, and unstages them.
: ES-ROOTS ( n -- n ) {: mark:n :}
   SCHEMA-ROOT-N@ {: start:n :}
   ES-N @ mark ?do i ES@ SCHEMA-ROOT+ drop loop
   mark ES-N !
   start ;


\ A node's children are built and staged before its own roots are pushed, so
\ each node's roots stay contiguous. ar is the arity of the family whose entry
\ is read: a parameter indexes its type variables.
: ER-NODE ( n -- n ) {: ar:n :}
   ER-TOKEN {: a:^ u:n :}
   u 1 <> IF ER-BAD THEN
   a c@ {: c:n :}
   c 80 = IF
      ER-NUM {: ix:n :}
      ix 0 <  ix ar >=  or IF ER-BAD THEN
      ix SCHEMA-PARAM EXIT
   THEN
   c 67 = IF
      ER-TOKEN CT-FIND {: con:n :}
      con 0= IF ER-BAD THEN
      con SCHEMA-CON EXIT
   THEN
   c 42 = IF ar recurse SCHEMA-PTR EXIT THEN
   c 65 = IF
      ER-FAM {: fam:n :}
      ER-NUM {: k:n :}
      k fam TFAM-ARITY@ <> IF ER-BAD THEN
      ES-N @ {: mark:n :}
      k 0 ?do ar recurse ES-PUSH loop
      fam  mark ES-ROOTS  k SCHEMA-APP EXIT
   THEN
   c 81 = IF
      ER-NUM {: qa:n :}
      ES-N @ {: mark:n :}
      SCH-QUOT-ROWS 0 ?do ar recurse ES-PUSH loop
      mark ES@  mark 1 + ES@  mark 2 + ES@  mark 3 + ES@  qa 1 and 0 <>  qa 1 invert and
      mark ES-N !
      SCHEMA-QUOT EXIT
   THEN
   c 82 = IF
      ER-NUM {: k:n :}
      k 0 < IF ER-BAD THEN
      ES-N @ {: mark:n :}
      k 0 ?do ar recurse ES-PUSH loop
      mark ES-ROOTS k SCHEMA-ROW EXIT
   THEN
   ER-BAD ;


: ER-SYM ( -- n )
   ER-TOKEN {: a:^ u:n :}
   u 1 = IF a c@ 45 = IF 0 EXIT THEN THEN
   a u ER-INT {: vis:n :}
   ER-NAME$ {: pa:^ pu:n :}
   ER-TOKEN {: ta:^ tu:n :}
   pa pu vis ta tu CHECKER-PKG-SYM ;


\ Reads one variant of family id; answers its positional payload count.
: ER-VARIANT ( n -- n ) {: id:n :}
   ER-TOKEN {: na:^ nu:n :}
   ER-NUM {: tag:n :}
   ER-NUM {: pc:n :}
   ER-NAME$ {: ca:^ cu:n :}
   ER-SYM {: sym:n :}
   ER-NUM {: k:n :}
   k 0 < IF ER-BAD THEN
   ES-N @ {: mark:n :}
   k 0 ?do id TFAM-ARITY@ ER-NODE ES-PUSH loop
   mark ES-ROOTS {: ss:n :}
   id na nu tag ss k pc SUMV-ADD {: v:n :}
   cu 0 <> IF v ca cu TF-INTERN cu SUMV-CTOR-PKG! THEN
   sym 0 <> IF v sym SUMV-CTOR-SYM! THEN
   k ;


: ER-FIELD ( n n -- ) {: id:n vstart:n :}
   ER-TOKEN {: na:^ nu:n :}
   ER-NUM {: vo:n :}
   vo 0 < IF PF-NO-VARIANT ELSE vstart vo + THEN {: var:n :}
   var SUMV-N@ >= IF ER-BAD THEN
   ER-NUM {: slot:n :}
   ER-NUM {: nc:n :}
   ER-NUM {: boff:n :}
   ER-NUM {: nb:n :}
   ER-NUM {: al:n :}
   ER-NUM {: flags:n :}
   ER-SYM {: sym:n :}
   id TFAM-ARITY@ ER-NODE SCHEMA-ROOT+ {: root:n :}
   PF-N @ {: fid:n :}
   id var na nu root slot nc boff nb al flags PF-ADD
   fid sym FIELD-SYM! ;


\ The family's declaration and its scalar rows, before the families it names.
: ER-HEAD ( str str n -- n ) {: pa:^ pu:n ta:^ tu:n ix:n :}
   ER-NUM {: kind:n :}
   ER-NUM {: vis:n :}
   ER-NUM {: arity:n :}
   pa pu vis ta tu arity kind TFAM-DECL {: id:n :}
   id ix ENTRY!
   arity 0 ?do id i ER-NUM TFAM-PK! loop
   id ER-NUM TFAM-LAYOUT!
   ER-NUM drop                    \ slots: ER-BODY takes them from the rows
   id ER-NUM TFAM-TAGW!
   ER-NUM id TF-REC@ TF.DERIVE !
   ER-NUM id TF-REC@ TF.OPAQUE !
   id ER-SYM AT-SYM!
   id ;


\ The slots family id's committed rows give: a product's field cells, a sum's
\ or an enum's widest variant payload; any other kind keeps its own.
: ROW-SLOTS ( n -- n ) {: id:n :}
   id TFAM-PRODUCT? IF
      0  id TFAM-FLD-START@ dup id TFAM-FLD-COUNT@ + swap ?do i PF-CELLS@ + loop
      EXIT
   THEN
   id TFAM-SUM? id TFAM-ENUM? or IF
      0  id TFAM-VAR-START@ dup id TFAM-VAR-COUNT@ + swap ?do i SUMV-PAYCELLS@ max loop
      EXIT
   THEN
   id TFAM-SLOTS@ ;

\ A sum's or an enum's variants take named fields or positional payloads,
\ never both: pk is the widest variant's positional count, nf the field count.
: ER-PAYLOAD-MIX? ( n n n -- bool ) {: id:n pk:n nf:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF RES-FALSE EXIT THEN
   pk 0 >  nf 0 >  and ;

\ Variants, fields and layout, after the families they name.
: ER-BODY ( n -- ) {: id:n :}
   SUMV-N@ {: vstart:n :}
   ER-NUM {: nv:n :}
   nv 0 < IF ER-BAD THEN
   0  nv 0 ?do id ER-VARIANT max loop {: pk:n :}
   id vstart nv TFAM-VAR-RANGE!
   ER-NUM {: nf:n :}
   nf 0 < IF ER-BAD THEN
   id pk nf ER-PAYLOAD-MIX? IF ER-BAD THEN
   nf 0 > IF
      TYPE-FIELD:COUNT {: base:n :}
      nf 0 ?do id vstart ER-FIELD loop
      PF-COMMIT
      id base nf TFAM-FLD-RANGE!
   THEN
   id id ROW-SLOTS TFAM-SLOTS!
   ER-NUM 0 ?do id ER-NUM ER-NUM ER-NUM ER-NUM LAY-ADD drop loop
   ER-U @ 0 <> IF ER-BAD THEN ;

EXPORT CHECKER-RESOLVE:REC>INDEX
EXPORT CHECK:FACTS@
EXPORT CHECK:LINEAR-TEXT?
EXPORT CHECK:NMAP-SLOT
EXPORT CHECK:SGBAD-AMBIG!
EXPORT CHECK:SIG-SCOPE$
EXPORT CHECKER-RESOLVE:REC-AT
EXPORT CHECKER-RESOLVE:xref-search-wl

EXPORT BOOT:TYPES-WL

\ The record of family `pkg:tail`'s type entry while one is still to be read,
\ else NULL-PTR. The probe is no dependency of the part: a linked piece gave the entry.
: ER-PROBE ( str str -- ^n )
   {: pa:^ pu:n na:^ nu:n :}
   ENTRIES-PENDING @ 0= IF NULL-PTR EXIT THEN
   TYPES-WL {: wid:n :}
   wid 0= IF NULL-PTR EXIT THEN
   pa pu na nu EN-NAME wid xref-search-wl {: rec:^ :}
   rec NULL-PTR = IF NULL-PTR EXIT THEN
   \ A linear type's entry shares the wordlist; its text starts `linear`, which is no family.
   rec REC>INDEX FACTS@ LINEAR-TEXT? IF NULL-PTR EXIT THEN
   rec ;

\ Reads record rec's type entry into the registry: its head, then each family it
\ names that the registry lacks, then its body (ER-STATE). The rows read are
\ written out again and must equal the entry's text.
: ER-READ ( ^n -- n ) {: rec:^ :}
   rec REC>INDEX {: ix:n :}
   ER-P @ ER-U @ ER-NAME @ ER-NAME-U @ {: sp:^ su:n sn:^ snu:n :}
   rec CK-REC-NAME$ {: ra:^ ru:n :}
   ra ER-NAME !  ru ER-NAME-U !
   ix FACTS@ ER-U ! ER-P !
   ra ru SPLIT-NAME 0= IF ER-BAD THEN
   ix ER-HEAD {: id:n :}
   ER-NUM 0 ?do
      ER-TOKEN SPLIT-NAME 0= IF ER-BAD THEN
      {: qa:^ qu:n ta:^ tu:n :}
      qa qu ta tu TFAM-FIND-IN nip 0= IF
         qa qu ta tu ER-PROBE {: dep:^ :}
         dep NULL-PTR = IF ER-BAD THEN
         dep recurse drop
      THEN
   loop
   id ER-BODY
   id ENTRY-TEXT$ ix FACTS@ CORE-STR= 0= IF ER-BAD THEN
   sp ER-P !  su ER-U !  sn ER-NAME !  snu ER-NAME-U !
   id ;

\ TFAM-FIND-IN, else the family's type entry `pkg:tail` read into the registry
\ (ER-READ), the outermost read moving ER-STATE.
: TFAM-FIND ( str str -- n bool )
   {: pa:^ pu:n na:^ nu:n :}
   pa pu na nu TFAM-FIND-IN IF RES-TRUE EXIT THEN drop
   pa pu na nu ER-PROBE {: rec:^ :}
   rec NULL-PTR = IF 0 RES-FALSE EXIT THEN
   ER-STATE @ ER-OPEN = IF rec ER-READ RES-TRUE EXIT THEN
   PF-N @ ER-MARK !  ER-OPEN ER-STATE !
   [: rec ER-READ ;] [: ER-IDLE ER-STATE ! ;] finally {: id:n :}
   ER-MARK @ PF-WIDTHS-REQUIRE
   id RES-TRUE ;

\ The family a spelling names exactly, with no scope or fallback: a bare tail is
\ package "", else path:tail split at the last colon.
: TFAM-FIND-SPELLED ( str -- n bool )
   SPLIT-NAME 0= IF 2drop 2drop 0 RES-FALSE EXIT THEN
   TFAM-FIND ;


\ Reads every type entry the family registry or the type table lacks, so no
\ lookup inside a declaration that follows reads one.
: READ-PENDING ( -- )
   ENTRIES-PENDING @ 0= IF EXIT THEN
   TYPES-WL {: wid:n :}
   wid 0 <> IF
      ndict@ 0 ?do
         i REC-AT {: rec:^ :}
         rec CK-REC-WID wid = IF
            i FACTS@ LINEAR-TEXT? IF
               rec CK-REC-NAME$ CT-FIND drop
            ELSE
               rec CK-REC-NAME$ SPLIT-NAME 0= IF
                  s" tfam: bad type entry name" EXIT-STATUS:INTERNAL die THEN
               TFAM-FIND drop drop
            THEN
         THEN
      loop
   THEN
   0 ENTRIES-PENDING ! ;


\ A `using` import names its package's public families: the family tail na nu
\ names in the package of using slot ix.
: TFAM-USED-FIND ( n str -- n bool ) {: ix:n na:^ nu:n :}
   ix CHECKER-USING$ na nu TFAM-FIND 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-PUBLIC? IF id RES-TRUE ELSE 0 RES-FALSE THEN ;

\ Repeating an import still names the same family; two distinct matches throw
\ E-TFAM-AMBIG.
: TFAM-FIND-USED ( str -- n bool ) {: na:^ nu:n :}
   -1 TF-PUB !
   CHECKER-USING-N 0 ?DO
      i na nu TFAM-USED-FIND IF
         {: id:n :}
         TF-PUB @ 0< IF id TF-PUB !
         ELSE id TF-PUB @ <> IF E-TFAM-AMBIG throw THEN THEN
      ELSE drop THEN
   LOOP
   TF-PUB @ 0< IF 0 RES-FALSE ELSE TF-PUB @ RES-TRUE THEN ;

\ Bit i set when using slot i offers the family tail na nu.
: TFAM-USED-MASK ( str -- n ) {: na:^ nu:n :}
   0  CHECKER-USING-N 0 ?DO
      i na nu TFAM-USED-FIND nip IF 1 i lshift or THEN
   LOOP ;

\ A bare tail resolves as a bare word does: the active package's exact row
\ (private or public), inside a part `A:B` the row of its package A, the exact
\ global row, then the `using` imports; another package's family needs
\ `PKG:tail` or an import. At top level the first exact lookup is already
\ global. Compaction-hidden `@name` tokens never resolve.
: TFAM-RESOLVE ( str str -- n bool )
   {: pa:^ pu:n na:^ nu:n :}
   na nu TF-HIDDEN? IF 0 RES-FALSE EXIT THEN
   pa pu na nu TFAM-FIND IF RES-TRUE EXIT THEN
   drop
   pa pu CHECKER-COLON-SCAN drop 0 <> IF
      pa pu TOP$ na nu TFAM-FIND IF RES-TRUE EXIT THEN
      drop
   THEN
   pu 0 <> IF
      s" " na nu TFAM-FIND IF RES-TRUE EXIT THEN
      drop
   THEN
   na nu TFAM-FIND-USED ;

PTR-VARIABLE TFQ-P
SCRATCH-VARIABLE TFQ-CAP          \ TFQ-P's room, in cells

: TFQ-BUF ( -- ^u8 ) TFQ-P @ ;
variable TFQ-U
PTR-VARIABLE TFQ-TA   variable TFQ-TU  \ qualified tail token
variable TFQ-COLON

: TFQ-FOLD-COPY ( str -- ) {: a:^ u:n :}
   TFQ-P TFQ-CAP  u CELL / 1 +  1 ARENA-ROWS-ENSURE
   0 BEGIN dup u < WHILE
      {: i:n :}
      a i + c@ CORE-FOLD-C  TFQ-BUF i + c!
      i 1 +
   REPEAT drop
   u TFQ-U ! ;

\ One non-edge ':' splits qualifier and tail (engine FIND parity) and folds the
\ written qualifier into TFQ-BUF.
: TFQ-SPLIT? ( str -- bool ) {: a:^ u:n :}
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
   a TFQ-COLON @ TFQ-FOLD-COPY
   a TFQ-COLON @ + 1 + TFQ-TA !
   u TFQ-COLON @ - 1 - TFQ-TU !
   RES-TRUE ;

\ How a family token resolved: SR-AMBIG is a bare tail two used packages make
\ public, SR-HIDDEN a family another package keeps private.
0 constant SR-FOUND
1 constant SR-ABSENT
2 constant SR-AMBIG
3 constant SR-HIDDEN

\ The split tail's family in package qa, seen from package pa.
: TFAM-QUAL-IN ( str str -- n n )
   {: qa:^ qu:n pa:^ pu:n :}
   TFQ-TA @ TFQ-TU @ TF-CANON? 0= IF 0 SR-ABSENT EXIT THEN
   qa qu TFQ-TA @ TFQ-TU @ TFAM-FIND 0= IF drop 0 SR-ABSENT EXIT THEN
   {: id:n :}
   id TFAM-PUBLIC? IF id SR-FOUND EXIT THEN
   qa qu pa pu CORE-STR=CI IF id SR-FOUND EXIT THEN   \ own private rows
   0 SR-HIDDEN ;

: TFAM-QUAL-RESOLVE ( str -- n n ) {: pa:^ pu:n :}
   TFQ-BUF TFQ-U @ TFQ-HEAD$ pa pu TFAM-QUAL-IN ;

\ A qualified spelling's family in the package its qualifier names as
\ written, which no part of package pa shadows; a bare one answers false.
: TFAM-EXACT-RESOLVE ( str str -- n bool )
   {: pa:^ pu:n na:^ nu:n :}
   na nu TFQ-SPLIT? 0= IF 0 RES-FALSE EXIT THEN
   TFQ-BUF TFQ-U @ pa pu TFAM-QUAL-IN SR-FOUND = ;

\ The family token na nu names in package pa and how it resolved; an ambiguous
\ public tail (E-TFAM-AMBIG) answers SR-AMBIG.
: TFAM-SIG-RESOLVE ( str str -- n n )
   {: pa:^ pu:n na:^ nu:n :}
   SIG-FACTS-MODE @ IF   \ facts may start "@:"
      na nu TFAM-FIND-SPELLED IF SR-FOUND ELSE SR-ABSENT THEN EXIT
   THEN
   na nu TF-HIDDEN? IF 0 SR-ABSENT EXIT THEN
   na nu TFQ-SPLIT? IF pa pu TFAM-QUAL-RESOLVE EXIT THEN
   0 RES-FALSE [: 2drop pa pu na nu TFAM-RESOLVE ;] catch {: id flag rc:n :}
   rc 0= IF id flag IF SR-FOUND ELSE SR-ABSENT THEN EXIT THEN
   rc E-TFAM-AMBIG = IF 0 SR-AMBIG EXIT THEN
   rc throw ;

\ The checker's TFAM-RESOLVE-XT target: an ambiguous tail fails the parse as
\ SGBAD's ambiguous kind, with the using slots that offer it.
: TFAM-PARSE-RESOLVE ( str str -- n bool )
   {: pa:^ pu:n na:^ nu:n :}
   pa pu na nu TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF na nu  na nu TFAM-USED-MASK  SGBAD-AMBIG! THEN
   id  sr SR-FOUND = ;

\ The signature parser asks this only after its normal family and concrete
\ lookups missed. An ambiguous public tail, another package's private family, a
\ malformed qualifier or a noncanonical family tail is not an absent
\ declaration a quiet producer may explain.
: TFAM-MAY-ARRIVE? ( str -- bool ) {: a:^ u:n :}
   SIG-SCOPE$ a u TFAM-SIG-RESOLVE nip {: sr:n :}
   sr SR-AMBIG =  sr SR-HIDDEN = or IF RES-FALSE EXIT THEN
   a u TFQ-SPLIT? IF TFQ-TA @ TFQ-TU @ TF-CANON?
   ELSE a u TF-CANON? THEN
   IF a u CHECKER-TYPE-MARK-COVERS? ELSE RES-FALSE THEN ;

: TFAM-MAY-ARRIVE-INSTALL ( -- )
   [: TFAM-MAY-ARRIVE? ;] is TYPE-MAY-ARRIVE-XT ;
TFAM-MAY-ARRIVE-INSTALL

\ The row's package is the qualifier, or a part `A:<qualifier>`.
: TFAM-QUAL-PKG? ( str n -- bool ) {: qa:^ qu:n id:n :}
   id TFAM-PKG$ {: pa:^ pu:n :}
   pa pu qa qu CORE-STR=CI IF RES-TRUE EXIT THEN
   pu qu 1 + < IF RES-FALSE EXIT THEN
   pa pu qu - 1 - + c@ 58 <> IF RES-FALSE EXIT THEN
   pa pu qu - + qu qa qu CORE-STR=CI ;

\ Some scope reads this signature token as a family, which a DEFLINEAR or
\ VALUE-RECORD name may not be (forth.md "Declaring your own scalar types").
: TFAM-CLAIMED? ( str -- bool ) {: na:^ nu:n :}
   READ-PENDING
   na nu TF-HIDDEN? IF RES-FALSE EXIT THEN
   na nu TFQ-SPLIT? IF TFQ-TA @ TFQ-TU @ TFQ-BUF TFQ-U @
   ELSE na nu NULL-PTR 0 THEN {: ta:^ tu:n qa:^ qu:n :}
   TFX-ENSURE
   ta tu TFX-HASH TFX-BKT @ TFX-CUR !
   BEGIN TFX-CUR @ 0 <> WHILE
      TFX-CUR @ 1 - {: id:n :}
      ta tu id TFAM-NAME-MATCH? IF
         qu 0= IF RES-TRUE EXIT THEN
         qa qu id TFAM-QUAL-PKG? IF RES-TRUE EXIT THEN
      THEN
      id TF-REC@ TF.TAILNEXT @ TFX-CUR !
   REPEAT
   RES-FALSE ;

26 constant TFC-VAR-CAP          \ internal construct scratch; not the declaration spelling cap
create TFC-VARS TFC-VAR-CAP cells allot
variable TFC-I   variable TFC-J   variable TFC-ROW

\ Construct form (docs/forth.md "Structures And Enums"): the checker's reserved
\ `construct family variant` token. A family constructs only inside its own
\ active checker package (top level owns the global "" package), public or
\ private; cross-package callers use the generated constructor words. Only sum
\ and enum kinds construct. The step effect instantiates SUMV payload schemas
\ against one fresh var per family parameter and CHECKER-STEP applies it like
\ any word call. A result of known width expands to hidden fields at the step;
\ a parametric result whose width reads an open argument (checker.f
\ LAYOUT-WIDTH-OPEN?) stays one logical cell and expands at the boundary.

: TFC-MINT-VARS ( n -- ) {: ar:n :}
   ar TFC-VAR-CAP > IF s" tfam: construct arity over cap" EXIT-STATUS:INTERNAL die THEN
   0 TFC-I !
   BEGIN TFC-I @ ar < WHILE
      FRESH MK-VAR TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

: TFC-ARGS! ( n -- ) {: term:n :}
   term PARAM>ARGC TFC-VAR-CAP > IF s" tfam: layout arity over cap" EXIT-STATUS:INTERNAL die THEN
   0 TFC-I !
   BEGIN TFC-I @ term PARAM>ARGC < WHILE
      term TFC-I @ PARAM>ARG  TFC-I @ cells TFC-VARS + !
      TFC-I @ 1 + TFC-I !
   REPEAT ;

\ TFC-SCH-TERM's mode: a term for a value in use, for a value a constructor
\ stores, or for a question about an instance, whose term is thrown away.
0 constant TFC-USE
1 constant TFC-STORE
2 constant TFC-ASK

\ Folds one quotation side (a SCH-ROW node) onto a base row, element 0 deepest,
\ matching the checker's PSTACK/SIG-PARSE-QUOT fold. Deferred: the SCH-QUOT case
\ of TFC-SCH-TERM calls it before its impl is defined.
defer TFC-QUOT-ROW ( n n n -- n )   \ ( rownode base mode -- row )


: TFC-SCH-TERM ( n n -- n )    \ payload schema node -> checker type term
   {: node:n mode:n :}
   node SCHEMA-PARAM? IF node SCHEMA-A@ cells TFC-VARS + @ EXIT THEN
   node SCHEMA-CON?   IF node SCHEMA-A@ MK-CON EXIT THEN
   node SCHEMA-PTR?   IF node SCHEMA-A@ mode RECURSE MK-PTR EXIT THEN
   node SCHEMA-XT?    IF OPAQUE-XT EXIT THEN
   node SCHEMA-APP? IF
      PARAM-SCR-N @ {: base:n :}
      0 BEGIN dup node SCHEMA-C@ < WHILE
         node SCHEMA-B@ over + SCHEMA-ROOT@ mode RECURSE PARAM-SCR+
         1 +
      REPEAT drop
      node SCHEMA-A@ {: fam:n :}
      base fam TFAM-NAME$ fam MK-PARAM EXIT
   THEN
   node SCHEMA-QUOT? IF                       \ xt-carrying payload: din/dout share a
      FRESH MK-QROW {: dbase:n :}             \ data base, rin/rout a return base: the
      FRESH MK-QROW {: rbase:n :}             \ implicit tails a signature's callable has
      node SCHEMA-QUOT-DIN@  dbase mode TFC-QUOT-ROW {: din:n :}
      node SCHEMA-QUOT-DOUT@ dbase mode TFC-QUOT-ROW {: dout:n :}
      node SCHEMA-QUOT-HASR@ IF
         node SCHEMA-QUOT-RIN@  rbase mode TFC-QUOT-ROW {: rin:n :}
         node SCHEMA-QUOT-ROUT@ rbase mode TFC-QUOT-ROW {: rout:n :}
         din dout rin rout MK-QUOT
      ELSE
         din dout rbase rbase MK-QUOT         \ no return clause: neutral rin = rout
      THEN
      dup Q>FACTS node SCHEMA-QUOT-FACTS@ or over Q-FACTS!
      \ A schema's quotation in a constructor's input is stored, so it reads no local.
      mode TFC-STORE = IF dup Q>BLOCK PAY NSCOPE-RAISE THEN EXIT
   THEN
   s" tfam: unsupported construct payload schema" EXIT-STATUS:INTERNAL die ;


: TFC-QUOT-ROW-IMPL ( n n n -- n ) {: rownode:n base:n mode:n :}
   base 0
   BEGIN dup rownode SCHEMA-ROW-COUNT@ < WHILE
      dup rownode swap SCHEMA-ROW-ELEM@ mode TFC-SCH-TERM
      swap >r
      \ A question's term is thrown away, so its row leaves the instance's
      \ arguments their kinds.
      swap mode TFC-ASK = IF PUSH-TERM ELSE PUSH-LOGICAL THEN
      r> 1 +
   REPEAT drop ;
: TFC-QUOT-ROW-INSTALL ( -- ) [: TFC-QUOT-ROW-IMPL ;] is TFC-QUOT-ROW ;
TFC-QUOT-ROW-INSTALL

\ A generic member's number: the bytes a field's accessor (FIELD, SPAN) adds to
\ the record's address, or AT's stride (INDEX). The declaration fixes it with
\ every parameter one cell; an instance with a wide argument moves it.
: MEMBER-FAM ( n n -- n ) {: shape:n param:n :}
   shape CHILD-ABI:INDEX = IF param EXIT THEN
   param PF-FAM@ ;

: SLOT-OPEN? ( n n -- bool ) {: term:n slot:n :}
   term slot PARAM>ARG T-UNSTALE {: arg:n :}
   arg ISVAR IF RES-TRUE EXIT THEN
   arg TAG T-PARAM = IF arg LAYOUT-WIDTH-OPEN? EXIT THEN
   RES-FALSE ;

\ Whether member SHAPE PARAM's number reads argument slot SLOT: an offset reads
\ the widths of the fields before it, a BUFFER field's none.
: MEMBER-READS? ( n n n -- bool ) {: shape:n param:n slot:n :}
   shape param MEMBER-FAM {: fam:n :}
   shape CHILD-ABI:INDEX = IF fam slot TFAM-WIDTH-SLOT? EXIT THEN
   param fam TFAM-FLD-START@ ?do
      i PF-FLAGS@ PF-FLAG-BUFFER <> IF
         i PF-SCH@ SCHEMA-ROOT@ slot SCH-ROOT-WIDTH-SLOT? IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

\ Member SHAPE PARAM's number for the instance TERM, false when TERM is no
\ instance of the member's family or a slot the number reads is open.
: MEMBER-AT ( n n n -- n bool ) {: shape:n param:n term0:n :}
   shape param MEMBER-FAM {: fam:n :}
   term0 T-UNSTALE {: term:n :}
   term TAG T-PARAM <> IF 0 RES-FALSE EXIT THEN
   term PARAM>FAM fam <> IF 0 RES-FALSE EXIT THEN
   term PARAM>ARGC {: count:n :}
   count fam TFAM-ARITY@ <> IF 0 RES-FALSE EXIT THEN
   count 0 ?do
      shape param i MEMBER-READS? IF
         term i SLOT-OPEN? IF 0 RES-FALSE unloop EXIT THEN
      THEN
   loop
   shape CHILD-ABI:INDEX = IF term TFAM-INST-WIDTH@ CELL * RES-TRUE EXIT THEN
   term 0 count TERM-FRAME
   0  param fam TFAM-FLD-START@ ?do i 0 count TFW-FIELD + loop
   CELL * RES-TRUE ;

\ The initialized-record surface is a bounded view of a product family's
\ committed fields. The physical record is canonical cell storage, and each
\ substituted field must still occupy the cells and byte range its committed id
\ describes.
\ Comparing only the total width would accept two fields whose changed widths
\ cancel while the second field's committed offset is now wrong.
: INIT-NO-RECORD ( -- n n n bool ) 0 0 0 RES-FALSE ;
: INIT-NO-FIELD ( -- n n n n bool ) 0 0 0 0 RES-FALSE ;

: INIT-FIELD-WIDTH? ( n n n -- n bool ) {: fid:n fam:n record:n :}
   fid PF-FAM@ fam <> IF 0 RES-FALSE EXIT THEN
   fid PF-VAR@ PF-NO-VARIANT <> IF 0 RES-FALSE EXIT THEN
   fid PF-FLAGS@ PF-FLAGS-NONE <> IF 0 RES-FALSE EXIT THEN
   CHILD-ABI:FIELD fid record MEMBER-AT 0= IF drop 0 RES-FALSE EXIT THEN {: off:n :}
   fid PF-SLOT@ CELL * off <> IF 0 RES-FALSE EXIT THEN
   fid PF-BYTE-OFF@ off <> IF 0 RES-FALSE EXIT THEN
   fid PF-ALIGN@ CELL <> IF 0 RES-FALSE EXIT THEN
   fid PF-SCH@ SCHEMA-ROOT@ TFC-ASK TFC-SCH-TERM T-RES {: field:n :}
   field ISVAR IF 0 RES-FALSE EXIT THEN
   field TAG T-PARAM = IF
      field LAYOUT-WIDTH-OPEN? IF 0 RES-FALSE EXIT THEN
   THEN
   field T-WIDTH {: width:n :}
   fid PF-CELLS@ width <> IF 0 RES-FALSE EXIT THEN
   fid PF-BYTES@ width CELL * <> IF 0 RES-FALSE EXIT THEN
   width RES-TRUE ;

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
      base i + fam record INIT-FIELD-WIDTH?
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
   fid PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM
   fid PF-BYTE-OFF@ fid PF-CELLS@ fid PF-BYTES@ RES-TRUE ;

: INIT-FIELD-NAME? ( n str -- n n n bool )
   {: record:n name:^ size:n :}
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
   root SCHEMA-ROOT@ TFC-ASK TFC-SCH-TERM maybe OWN-TYPE-N ;

: PRODUCT-LIN-N ( n bool -- n ) {: term:n maybe:bool :}
   term PARAM>FAM {: fam:n :}
   0
   0 BEGIN dup fam TFAM-FLD-COUNT@ < WHILE
      fam TFAM-FLD-START@ over + PF-SCH@ term maybe TFAM-SCH-LIN-N
      rot + swap
      1 +
   REPEAT drop ;

: SUMV-LIN-N ( n n bool -- n ) {: vid:n term:n maybe:bool :}
   0 0 BEGIN dup vid SUMV-PAY-N < WHILE
      {: acc:n idx:n :}
      vid idx SUMV-PAY-ROOT term maybe TFAM-SCH-LIN-N acc +
      idx 1 +
   REPEAT drop ;

: SUM-LIN-N ( n bool -- n ) {: term:n maybe:bool :}
   term PARAM>FAM {: fam:n :}
   0 0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
      {: acc:n idx:n :}
      fam TFAM-VAR-START@ idx + term maybe SUMV-LIN-N acc max
      idx 1 +
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
: SCHEMA-TERM-FOR ( n n n -- n ) {: owner:n node:n mode:n :}
   TFC-I @ {: old-i:n :}
   PARAM-SCR-N @ {: base:n :}
   0 BEGIN dup TFC-VAR-CAP < WHILE
      dup cells TFC-VARS + @ PARAM-SCR+
      1 +
   REPEAT drop
   owner T-RES TFC-ARGS!
   node mode TFC-SCH-TERM {: term:n :}
   0 BEGIN dup TFC-VAR-CAP < WHILE
      dup base + cells PARAM-SCR + @
      over cells TFC-VARS + !
      1 +
   REPEAT drop
   base PARAM-SCR-N !
   old-i TFC-I !
   term ;

\ The type variable an effect's parameter I is, in the NMAP slot of the I-th
\ variable letter.
: EFFECT-VAR ( n -- n ) {: i:n :}
   i TFAM-DECL-PARAM>CHAR drop 97 - SLOT>VAR ;

\ The arguments EFFECT-ROWS-ON reads parameters 0 to SPAN - 1 from.
: EFFECT-OWNER ( n -- n ) {: span:n :}
   PARAM-SCR-N @ {: base:n :}
   0 BEGIN dup span < WHILE dup EFFECT-VAR PARAM-SCR+ 1 + REPEAT drop
   base s" effect" -1 MK-PARAM ;

\ One more than the largest parameter index a schema node holds, 0 for none.
: PARAM-SPAN ( n -- n ) {: node:n :}
   node SCHEMA-PARAM? IF node SCHEMA-A@ 1 + EXIT THEN
   node SCHEMA-PTR? IF node SCHEMA-A@ RECURSE EXIT THEN
   node SCHEMA-APP? IF
      0 0 BEGIN dup node SCHEMA-C@ < WHILE
         node SCHEMA-B@ over + SCHEMA-ROOT@ RECURSE  rot max swap  1 +
      REPEAT drop EXIT
   THEN
   node SCHEMA-ROW? IF
      0 0 BEGIN dup node SCHEMA-ROW-COUNT@ < WHILE
         node over SCHEMA-ROW-ELEM@ RECURSE  rot max swap  1 +
      REPEAT drop EXIT
   THEN
   node SCHEMA-QUOT? IF
      node SCHEMA-QUOT-DIN@ RECURSE  node SCHEMA-QUOT-DOUT@ RECURSE max
      node SCHEMA-QUOT-RIN@ RECURSE max  node SCHEMA-QUOT-ROUT@ RECURSE max
      EXIT
   THEN
   0 ;

\ Kinds each parameter node passes to a family's scope or region slot.
: EFFECT-KINDS ( n -- ) {: node:n :}
   node SCHEMA-PTR? IF node SCHEMA-A@ RECURSE EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-C@ 0 ?do
         node SCHEMA-B@ i + SCHEMA-ROOT@
         dup SCHEMA-PARAM? IF
            dup SCHEMA-A@ TFAM-DECL-PARAM>CHAR drop 97 - NMAP-SLOT @
            node SCHEMA-A@ i TFAM-PK@
            dup PK-SCOPE = IF over TVK-SCOPE! THEN
            PK-REGION = IF TVK-REGION! ELSE drop THEN
         THEN
         RECURSE
      loop EXIT
   THEN
   node SCHEMA-ROW? IF
      node SCHEMA-ROW-COUNT@ 0 ?do node i SCHEMA-ROW-ELEM@ RECURSE loop EXIT
   THEN
   node SCHEMA-QUOT? IF
      node SCHEMA-QUOT-DIN@ RECURSE  node SCHEMA-QUOT-DOUT@ RECURSE
      node SCHEMA-QUOT-RIN@ RECURSE  node SCHEMA-QUOT-ROUT@ RECURSE
   THEN ;

\ A side of an effect on row BASE, element 0 deepest, each type instantiated
\ over OWNER's arguments and pushed as a parsed signature's type is.
: EFFECT-SIDE ( n n n -- n ) {: owner:n side:n base:n :}
   base
   side SCHEMA-ROW-COUNT@ 0 ?do
      owner side i SCHEMA-ROW-ELEM@ TFC-USE SCHEMA-TERM-FOR swap PUSH-LOGICAL
   loop ;

\ The input and output rows of effect node NODE on row BASE.
: EFFECT-ROWS-ON ( n n -- n n ) {: node:n base:n :}
   node PARAM-SPAN EFFECT-OWNER {: owner:n :}
   node EFFECT-KINDS
   owner node SCHEMA-QUOT-DIN@ base EFFECT-SIDE
   owner node SCHEMA-QUOT-DOUT@ base EFFECT-SIDE ;

\ The term type node NODE gives, its parameters the effect's variables, as
\ EFFECT-ROWS-ON gives a side's type.
: EFFECT-TYPE ( n -- n ) {: node:n :}
   node PARAM-SPAN EFFECT-OWNER {: owner:n :}
   node EFFECT-KINDS
   owner node TFC-USE SCHEMA-TERM-FOR ;

\ A layout value's members for checker.f's CAST: walks (CAST-INTRO? and
\ CAST-MAY-LINEAR?): every product field and every variant payload, in either
\ payload form, instantiated over the value's own arguments and pushed on one
\ row. Schemas index parameters by position, so the term must carry the
\ family's argument count: the cast gate rejects any other count before its
\ walks.
: MEMBER+ ( n n n -- n ) {: term:n root:n row:n :}
   term root SCHEMA-ROOT@ TFC-ASK SCHEMA-TERM-FOR row MK-PUSH ;
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
      s" tfam: layout members need the family's arity" EXIT-STATUS:INTERNAL die THEN
   fam TFAM-PRODUCT? IF term fam FRESH MK-ROW FIELD-MEMBERS EXIT THEN
   FRESH MK-ROW
   fam TFAM-VAR-COUNT@ 0 ?do term fam TFAM-VAR-START@ i + rot VARIANT-MEMBERS loop ;
: TFAM-MEMBERS-INSTALL ( -- ) [: TFAM-MEMBERS ;] is TFAM-MEMBERS-XT ;
TFAM-MEMBERS-INSTALL

\ What an offset child's output points at, from the record's pointee A.
: CHILD-POINTEE ( n n -- n ) {: shape:n a:n :}
   shape CHILD-ABI:CELL-OFF = IF a EXIT THEN
   shape CHILD-ABI:PTR-OFF = IF FRESH MK-VAR MK-PTR EXIT THEN
   shape CHILD-ABI:BYTE-OFF = IF CC-U8 MK-CON EXIT THEN
   s" tfam: unknown child shape" EXIT-STATUS:INTERNAL die ;

\ Letters the type variable ID in NMAP as a signature spells parameter INDEX.
: FRESH-LETTER ( n n -- ) {: id:n index:n :}
   index TFAM-DECL-PARAM>CHAR 0= IF drop EXIT THEN
   97 - NMAP-SLOT id swap ! ;

\ A fresh variable for parameter INDEX of FAM, kinded as a signature kinds the
\ parameter it spells there and lettered as signature parameter LETTER.
: FRESH-LETTERED ( n n n -- n ) {: fam:n index:n letter:n :}
   FRESH {: id:n :}
   id letter FRESH-LETTER
   fam index TFAM-PK@ {: pk:n :}
   pk PK-SCOPE = IF id TVK-SCOPE! THEN
   pk PK-REGION = IF id TVK-REGION! THEN
   id MK-VAR ;

: FRESH-PARAM ( n n -- n ) dup FRESH-LETTERED ;

\ The family FAM over fresh type variables, one per parameter and lettered as
\ it: a built row's stored kinds are decided for lettered variables only.
: TFAM-FRESH-TERM ( n -- n ) {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   fam TFAM-ARITY@ 0 ?do fam i FRESH-PARAM PARAM-SCR+ loop
   base fam TFAM-NAME$ fam MK-PARAM ;

\ A field accessor's input row over a fresh row BASE, `^F<..>`, from the
\ committed field id FID, and its family term TERM, whose arguments the field's
\ type reads.
: FIELD-IN ( n -- n n n ) {: fid:n :}
   fid PF-FAM@ {: fam:n :}
   fam TFAM-FRESH-TERM {: term:n :}
   term TFC-ARGS!
   FRESH MK-ROW {: base:n :}
   term MK-PTR base MK-PUSH  base  term ;

\ A variable member SHAPE PARAM's number reads stays one cell, the width the
\ number counts it as: the stored kinds a pointer gives promote only ANY.
: MEMBER-CELLS ( n n n -- ) {: shape:n param:n term:n :}
   term PARAM>ARGC 0 ?do
      shape param i MEMBER-READS? IF term i PARAM>ARG PAY TVK-CELL! THEN
   loop ;

: SPAN-U8 ( -- n )
   s" SPAN" s" span" TFAM-FIND 0= IF s" tfam: no SPAN:span family" EXIT-STATUS:INTERNAL die THEN
   {: fam:n :}
   PARAM-SCR-N @ {: pbase:n :}
   CC-U8 MK-CON PARAM-SCR+
   pbase fam TFAM-NAME$ fam MK-PARAM ;

\ A plain field accessor's rows and byte offset, from the committed field id
\ FID: it takes `^F<..>` and answers `^T`, T the field's type over the
\ same arguments.
: TFAM-FIELD-ROWS ( n -- n n n ) {: fid:n :}
   fid FIELD-IN {: din:n base:n term:n :}
   CHILD-ABI:FIELD fid term MEMBER-CELLS
   fid PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM {: ft:n :}
   din  ft MK-PTR base MK-PUSH  fid PF-BYTE-OFF@ ;

\ A BUFFER field's accessor's rows and field id, from the committed field id
\ FID: it takes `^F<..>` and answers the field's bytes as `SPAN:span<u8>`,
\ laid base below len as SPAN:MAKE lays it.
: TFAM-SPAN-ROWS ( n -- n n n ) {: fid:n :}
   fid FIELD-IN {: din:n base:n term:n :}
   CHILD-ABI:SPAN fid term MEMBER-CELLS
   din  SPAN-U8 base PUSH-LOGICAL  fid ;

\ AT's rows and stride in bytes, from the family id FAM: it takes `^F<..> n`
\ and answers `^F<..>`, the count an integer as `record-at` takes it.
: TFAM-AT-ROWS ( n -- n n n ) {: fam:n :}
   fam TFAM-FRESH-TERM {: term:n :}
   CHILD-ABI:INDEX fam term MEMBER-CELLS
   FRESH MK-ROW {: base:n :}
   term MK-PTR base MK-PUSH  CC-N MK-CON swap MK-PUSH
   term MK-PTR base MK-PUSH
   fam TFAM-SLOTS@ CELL * ;

\ An initialized record's receiver over the family term TERM of arity AR,
\ `mut-view<p,q,a,init<i,TERM>>`, its variables lettered after TERM's.
: INIT-RECEIVER ( n n -- n ) {: term:n ar:n :}
   C2-INIT-FAM @ 0 ar 3 + FRESH-LETTERED {: scope:n :}
   PARAM-SCR-N @ {: base:n :}
   scope PARAM-SCR+ term PARAM-SCR+
   base s" init" C2-INIT-FAM @ MK-PARAM {: elem:n :}
   PARAM-SCR-N @ {: vbase:n :}
   3 0 ?do C2-MUT-FAM @ i ar i + FRESH-LETTERED PARAM-SCR+ loop
   elem PARAM-SCR+
   vbase s" mut-view" C2-MUT-FAM @ MK-PARAM ;

\ An initialized field accessor's rows and field id, from its shape and the
\ committed field id FID: INIT-GET takes the receiver and answers it and the
\ field's T over the family's arguments; INIT-SET takes both and answers the
\ receiver.
: TFAM-INIT-ROWS ( n n -- n n n ) {: shape:n fid:n :}
   fid PF-FAM@ {: fam:n :}
   fam TFAM-FRESH-TERM {: term:n :}
   term TFC-ARGS!
   fid PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM {: ft:n :}
   term fam TFAM-ARITY@ INIT-RECEIVER {: view:n :}
   view FRESH MK-ROW PUSH-LOGICAL {: bare:n :}
   ft bare PUSH-LOGICAL {: full:n :}
   shape CHILD-ABI:INIT-GET = IF bare full ELSE full bare THEN
   fid ;

\ A value child's rows: it answers its number, `( -- n )`.
: VALUE-ROWS ( n -- n n n ) {: v:n :}
   FRESH MK-ROW {: base:n :}
   base  CC-N MK-CON base MK-PUSH  v ;

\ An offset child's rows and offset, from its shape and byte offset.
: OFFSET-ROWS ( n n -- n n n ) {: shape:n off:n :}
   FRESH MK-ROW  FRESH MK-VAR {: base:n a:n :}
   a MK-PTR base MK-PUSH
   shape a CHILD-POINTEE MK-PTR base MK-PUSH
   off ;

\ A storage companion's rows: RELEASE takes nothing, `( -- )`, and BIND, GROW
\ and RESERVE take a count, `( n -- )`. Its body value is unread.
: COMPANION-ROWS ( n -- n n n ) {: shape:n :}
   FRESH MK-ROW {: base:n :}
   shape CHILD-ABI:RELEASE = IF base base 0 EXIT THEN
   CC-N MK-CON base MK-PUSH  base  0 ;

: COMPANION? ( n -- bool ) {: shape:n :}
   shape CHILD-ABI:BIND =  shape CHILD-ABI:GROW = or
   shape CHILD-ABI:RESERVE = or  shape CHILD-ABI:RELEASE = or ;

\ ROW with `^ARENA:arena` pushed.
: ARENA-ARG ( n -- n ) {: row:n :}
   s" ARENA" s" arena" TFAM-FIND 0= IF s" tfam: no ARENA:arena family" EXIT-STATUS:INTERNAL die THEN
   TFAM-FRESH-TERM MK-PTR row MK-PUSH ;

\ ROW with the action `[ ^T -- ]` pushed.
: ACTION-ARG ( n n -- n ) {: term:n row:n :}
   FRESH MK-QROW {: q:n :}
   term MK-PTR q MK-PUSH  q  FRESH MK-QROW dup MK-QUOT  row MK-PUSH ;

: STORED-IN ( n n n -- n ) {: shape:n term:n base:n :}
   shape CHILD-ABI:STORED =  shape CHILD-ABI:SLOT = or
   shape CHILD-ABI:WORK = or  shape CHILD-ABI:HEAP-NEW = or IF base EXIT THEN
   shape CHILD-ABI:ARENA-NEW = IF base ARENA-ARG EXIT THEN
   shape CHILD-ABI:ARENA-DISPOSE = IF term  base ARENA-ARG  ACTION-ARG EXIT THEN
   CC-N MK-CON base MK-PUSH ;

\ A typed storage child's input and output rows over the stored type's term T:
\ STORED, SLOT, WORK and HEAP-NEW answer `( -- ^T )`, the indexed shapes
\ `( n -- ^T )`, HEAP-RELEASE `( ^T -- )`, ARENA-NEW
\ `( ^ARENA:arena -- ^T )` and ARENA-DISPOSE
\ `( ^ARENA:arena [ ^T -- ] -- ^T )`. It takes no NEW, so T, which the
\ storage parse just made, stays live.
: TFAM-STORED-ROWS ( n n -- n n ) {: shape:n term:n :}
   FRESH MK-ROW {: base:n :}
   shape CHILD-ABI:HEAP-RELEASE = IF
      term MK-PTR base MK-PUSH  base EXIT
   THEN
   shape term base STORED-IN
   term MK-PTR base MK-PUSH ;

\ Payload transport uses the canonical logical-value seam. PUSH-LOGICAL expands
\ every closed layout to its hidden physical fields, including a width-one enum,
\ while scalar, pointer, and open terms retain their ordinary one-cell form.
\ Construct and MATCH therefore share the same representation as signatures.
: TFC-PUSH-PAY ( n n -- n ) PUSH-LOGICAL ;
: TFC-PAY-ROW ( n n bool -- n ) {: vid:n row0:n stored:bool :}
   row0 TFC-ROW !
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-PAY-N < WHILE
      vid TFC-J @ SUMV-PAY-ROOT SCHEMA-ROOT@
      stored IF TFC-STORE ELSE TFC-USE THEN TFC-SCH-TERM
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

\ A product's MAKE or UNMAKE rows over FAM's lettered variables, from its make
\ or unmake variant: MAKE takes the fields and answers the value, UNMAKE the
\ reverse. Its body is a cast, so its value is unread.
: PROD-ROWS ( n n -- n n n ) {: shape:n fam:n :}
   fam TFAM-FRESH-TERM {: term:n :}
   term TFC-ARGS!
   fam TFAM-VAR-START@
   shape CHILD-ABI:UNMAKE = IF 1 + THEN {: vid:n :}
   FRESH MK-ROW {: base:n :}
   vid base RES-FALSE TFC-PAY-ROW {: pay:n :}
   term base PUSH-LOGICAL {: value:n :}
   shape CHILD-ABI:MAKE = IF pay value ELSE value pay THEN
   0 ;

\ A DEFTYPE converter's rows over FAM, between its base (n, or the one field of
\ a family over a base) and the nominal. Its body is a cast: the value is unread.
: NOMINAL-ROWS ( n n -- n n n ) {: shape:n fam:n :}
   FRESH MK-ROW {: base:n :}
   fam TFAM-FLD-COUNT@ 1 = IF
      fam TFAM-FLD-START@ PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM
      base TFC-PUSH-PAY
   ELSE CC-N MK-CON base MK-PUSH THEN {: val:n :}
   fam TFAM-FRESH-TERM base PUSH-LOGICAL {: nom:n :}
   shape CHILD-ABI:NOMINAL-IN = IF val nom ELSE nom val THEN
   0 ;

\ A child word's input row, output row and body value, from its CHILD-ABI SHAPE
\ and PARAM, in fresh checker terms. NEW restarts the variable ids, so NMAP is
\ cleared with it: a stale letter would name a fresh variable.
: TFAM-CHILD-ROWS ( n n -- n n n ) {: shape:n param:n :}
   NEW NMAP-RESET
   shape CHILD-ABI:FIELD = IF param TFAM-FIELD-ROWS EXIT THEN
   shape CHILD-ABI:SPAN = IF param TFAM-SPAN-ROWS EXIT THEN
   shape CHILD-ABI:INDEX = IF param TFAM-AT-ROWS EXIT THEN
   shape CHILD-ABI:VALUE = IF param VALUE-ROWS EXIT THEN
   shape CHILD-ABI:INIT-GET = IF shape param TFAM-INIT-ROWS EXIT THEN
   shape CHILD-ABI:INIT-SET = IF shape param TFAM-INIT-ROWS EXIT THEN
   shape CHILD-ABI:MAKE = IF shape param PROD-ROWS EXIT THEN
   shape CHILD-ABI:UNMAKE = IF shape param PROD-ROWS EXIT THEN
   shape CHILD-ABI:NOMINAL-IN = IF shape param NOMINAL-ROWS EXIT THEN
   shape CHILD-ABI:NOMINAL-OUT = IF shape param NOMINAL-ROWS EXIT THEN
   shape COMPANION? IF shape COMPANION-ROWS EXIT THEN
   shape param OFFSET-ROWS ;

\ Width-aware construct/MATCH lowering. For a wide closed instantiation (a
\ multi-cell layout argument, flat or nested) the checker records an extra-pad
\ fact at the construct, constructor-call or `of` token: w = instantiated pads -
\ declared pads, the zero cells the lowering adds. A nested instantiation
\ (option<result<n,pkg:prod>>) needs nothing more: every width site
\ (TFAM-INST-WIDTH@, TFC-VAR-PAYCELLS, T-WIDTH) recurses through the argument
\ tree, so the inner bundle gets its own pads at its own site. Every width must
\ be closed: an open inner var (option<result<n,a>>) has an unstable width, so
\ TYPE-CLOSED? rejects it and CONSTRUCT-WIDE-STAGED-REJECT holds.
: TFC-VAR-PAYCELLS ( n -- n ) {: vid:n :}   \ sum of instantiated payload cell widths for a variant
   0
   0 TFC-J !
   BEGIN TFC-J @ vid SUMV-PAY-N < WHILE
      vid TFC-J @ SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-ASK TFC-SCH-TERM T-WIDTH +
      TFC-J @ 1 + TFC-J !
   REPEAT ;

EXPORT CHECK:MD-CONSTRUCT-WIDE
EXPORT CHECK:MD-FAM-AMBIG
EXPORT CHECK:MD-MEMBER-TOP
EXPORT CHECK:MDIAG!
EXPORT CHECK:OK
\ Extra pads = instantiated pads - declared pads; instantiated pads =
\ instantiated payload slots - instantiated payload cells. Only a SUM or ENUM
\ has a tag cell; a product family's whole width is payload (scg<scinr>, a
\ parametric STRUCTURE at a width-2 leaf, is width 3, all payload: subtracting a
\ tag would report extra -1), so TFC-TAG-CELLS keys the subtraction off the
\ family kind.
\ A negative extra on a construct (the widest declared variant is not the
\ widest instantiated one, or a non-widest variant grows more) is a
\ contradiction: its lowering pushes the declared pads and can only add cells,
\ so no sound lowering exists and it is refused unconditionally, a candidate
\ probe included. A MATCH arm reads only the instantiated pads.
: TFC-XPAD-NARROW-REJECT ( -- )   \ certified instantiated width contradicts add-only lowering: never certify
   MD-CONSTRUCT-WIDE MDIAG!  0 OK ! -1 FAILSET ! ;

: TFC-TAG-CELLS ( n -- n )   \ layout tag cells for a family: 1 for a tagged SUM/ENUM, 0 for a tagless product family
   dup TFAM-SUM? IF drop 1 EXIT THEN
   TFAM-ENUM? IF 1 ELSE 0 THEN ;

\ A niche value is one cell; MATCH and construct work on its stack-cell-tag
\ bundle, the cell and then a tag (0 for a zero cell, else 1).
: TFC-BUNDLE-CELLS ( n n -- n ) {: fam:n term:n :}
   term T-WIDTH
   fam TFAM-NICHE? IF 1 + THEN ;

\ The lowering pushes what the family declares and can only add: it calls the
\ generated constructor (declared pads and tag, whatever the instantiation) or
\ reads the declared pads from the registry for a `construct`, and pushes the
\ difference as zeros. MWIN-CELLS! files the difference under the judged token;
\ a construction that adds nothing files nothing. A niche family counts its
\ bundle, so a construct of either variant adds nothing.
: TFC-CON-XPAD-RECORD ( n n n -- ) {: fam:n vid:n famterm:n :}
   fam famterm TFC-BUNDLE-CELLS fam TFC-TAG-CELLS -
   vid TFC-VAR-PAYCELLS -
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - -
   {: extra:n :}
   extra 0 > IF extra MWIN-CELLS! EXIT THEN
   extra 0 < IF TFC-XPAD-NARROW-REJECT THEN ;   \ add-only lowering cannot remove the surplus

\ MATCH sees only a SUM or ENUM family: TFAM-MATCH-FAM and TFL-MATCH-FAM? reject
\ any other kind (MD-FAM-KIND) before an arm is recorded, and a product's UNMAKE
\ records no pad fact. So `rt T-WIDTH 1 -` needs no kind guard.
\
\ The compiler models the stack cell by cell and needs the pad count itself:
\ MWIN-CELLS! files it under this `of` token. An arm whose term is not closed
\ latches nothing: the cell-accurate consumer then refuses the definition by
\ name. A niche family counts its bundle, so its empty arm files one pad and the
\ other none.
: TFAM-MATCH-PADS-RECORD ( n n -- ) {: vid:n term:n :}
   term T-RES {: rt:n :}
   rt TYPE-REP-CLOSED? 0= IF EXIT THEN           \ open value/row: leave declared width; scope identities do not affect padding
   vid SUMV-FAM@ rt TFC-BUNDLE-CELLS 1 -
   vid TFC-VAR-PAYCELLS -
   MWIN-CELLS! ;

: TFAM-ACTIVE-PKG$ ( -- str )        \ authenticated package ("" at top level)
   CHECKER-AUTH-PACKAGE$ ;

\ The family a schema node applies, else 0 false.
: TFAM-NODE-FAM ( n -- n bool ) {: node:n :}
   node SCHEMA-NODE-OK? 0= IF 0 RES-FALSE EXIT THEN
   node SCHEMA-APP? 0= IF 0 RES-FALSE EXIT THEN
   node SCHEMA-A@ RES-TRUE ;

: TFAM-CONSTRUCT-KIND ( n -- n bool ) {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-CON-KIND MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFAM-CONSTRUCT-FAM ( str -- n bool ) {: na:^ nu:n :}   \ folded family token -> id
   TFAM-ACTIVE-PKG$ na nu TFAM-FIND 0= IF drop MD-CON-FAM MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   TFAM-CONSTRUCT-KIND ;

\ A node's family under the owner rule a spelled one meets: the active
\ package's own.
: TFAM-CONSTRUCT-NODE ( n -- n bool ) {: node:n :}
   node TFAM-NODE-FAM 0= IF drop MD-CON-FAM MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   {: id:n :}
   TFAM-ACTIVE-PKG$ id TFAM-PKG-MATCH? 0= IF MD-CON-FAM MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id TFAM-CONSTRUCT-KIND ;

\ Whether ROW's top cell is the unstale hidden slot SLOT of family FAM.
: TFC-GROUP-CELL? ( n n n -- bool )
   {: row:n fam:n slot:n :}
   row R-RES {: node:n :}
   node TAG S-PUSH <> IF RES-FALSE EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   t PARAM>FAM fam <> IF RES-FALSE EXIT THEN
   t HIDDEN-SLOT@ slot = ;


\ Whether ROW's top cells are the hidden slots TOP down to 0 of family FAM.
: TFC-GROUP-CELLS? ( n n n -- bool )
   {: row:n fam:n top:n :}
   row top
   BEGIN dup 0 >= WHILE                       \ ( row slot )
      2dup fam swap TFC-GROUP-CELL? 0= IF 2drop RES-FALSE EXIT THEN
      swap R-RES P>REST swap 1 -
   REPEAT 2drop RES-TRUE ;


\ The value DEPTH cells below the live top: a whole hidden group answers its term, width and true;
\ a non-hidden cell, open ones included, 0 1 false; a stale or missing cell or a partial group width 0.
: TFC-LIVE-GROUP ( n -- n n bool )
   {: depth:n :}
   DCUR @ depth
   BEGIN dup 0 > WHILE                        \ ( row cells-left )
      over R-RES TAG S-PUSH <> IF 2drop 0 0 RES-FALSE EXIT THEN
      swap R-RES P>REST swap 1 -
   REPEAT drop {: row:n :}
   row R-RES {: node:n :}
   node TAG S-PUSH <> IF 0 0 RES-FALSE EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t TAG T-STALE = IF 0 0 RES-FALSE EXIT THEN
   t HIDDEN-PARAM? 0= IF 0 1 RES-FALSE EXIT THEN
   t T-WIDTH {: w:n :}
   row t PARAM>FAM w 1 - TFC-GROUP-CELLS? 0= IF 0 0 RES-FALSE EXIT THEN
   t MK-LOGICAL w RES-TRUE ;


\ Payload J of VID over the value DEPTH cells down: a bare family parameter over a whole hidden
\ group takes the group's type. Answers the next payload's depth, -1 for no seed.
: TFC-LIVE-PAY ( n n n -- n )
   {: j:n depth:n vid:n :}
   depth TFC-LIVE-GROUP {: lt:n w:n group:bool :}
   w 0= IF -1 EXIT THEN
   vid j SUMV-PAY-ROOT SCHEMA-ROOT@ {: node:n :}
   group node SCHEMA-PARAM? and IF lt node SCHEMA-A@ cells TFC-VARS + ! THEN
   depth w + ;


\ The args VID's payloads on the stack give FAM, walked from the top payload down; a parameter no
\ payload sets stays fresh. A payload of width 0, or a term SEED-TERM? refuses, is no seed.
: TFC-LIVE-SEED ( n n -- n bool )
   {: fam:n vid:n :}
   fam TFAM-ARITY@ 0= IF 0 RES-FALSE EXIT THEN
   fam TFAM-ARITY@ TFC-MINT-VARS
   vid SUMV-PAY-N 1 - 0
   BEGIN over 0 >= WHILE                      \ ( payload depth )
      2dup vid TFC-LIVE-PAY nip
      dup 0 < IF 2drop 0 RES-FALSE EXIT THEN
      swap 1 - swap
   REPEAT 2drop
   fam TFC-FAM-TERM {: t:n :}
   t SEED-TERM? IF t RES-TRUE EXIT THEN
   0 RES-FALSE ;


\ The seed: FAM's first bundle in the declared output when that seeds (CONSTRUCT-DECL-LAYOUT), else
\ the payloads' (TFC-LIVE-SEED); with neither, vars stay fresh and a multi-cell value fails closed.
: TFC-SEED ( n n -- n bool )
   {: fam:n vid:n :}
   fam CONSTRUCT-DECL-LAYOUT IF RES-TRUE EXIT THEN drop
   fam vid TFC-LIVE-SEED ;


\ The construct step for (FAM, VID) over DT SEEDED, which TFC-SEED answers before TFC-ARGS! rewrites
\ the TFC-VARS it walks: a seeded closed layout arg expands payload and output even at width 1.
: TFC-CONSTRUCT-STEP-VID ( n n n bool -- )
   {: fam:n vid:n dt:n seeded:bool :}
   seeded IF dt TFC-ARGS! ELSE fam TFAM-ARITY@ TFC-MINT-VARS THEN
   FRESH MK-ROW {: base:n :}
   vid base RES-TRUE TFC-PAY-ROW {: din:n :}
   fam TFC-FAM-TERM {: famterm:n :}
   seeded IF
      dt TYPE-REP-CLOSED? IF fam vid famterm TFC-CON-XPAD-RECORD THEN
   THEN
   famterm base PUSH-LOGICAL {: dout:n :}
   din dout RECORDED-STEP
   seeded IF
      dt TYPE-REP-CLOSED? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN   \ open value/row: stay staged fail-closed
   THEN ;

: TFAM-CONSTRUCT-STEP ( str n -- bool ) {: na:^ nu:n fam:n :}
   fam na nu SUMV-FIND 0= IF drop MD-CON-VAR MDIAG-COMPILE! RES-FALSE EXIT THEN
   {: vid:n :}
   fam vid 2dup TFC-SEED TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ TFAM-CTOR-STEP? ( sym -- bool ): a generated-constructor call TFC-SEED seeds takes the construct
\ step, a product's UNMAKE the reversed one (TFAM-UNMAKE-STEP?); any other runs DO-TOK's word call.

: SUMV-FROM-CTOR-SYM ( n -- n bool ) {: sym:n :}   \ constructor word symbol -> variant id
   sym 0 <= IF 0 RES-FALSE EXIT THEN
   SVX-ENSURE
   sym SVX@ dup 0= IF RES-FALSE EXIT THEN          \ 0 head = no variant with this symbol
   1 - RES-TRUE ;

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
   vid base RES-FALSE TFC-PAY-ROW {: dout:n :}
   famterm base PUSH-LOGICAL {: din:n :}
   din dout RECORDED-STEP
   dt TYPE-REP-CLOSED? 0= IF CONSTRUCT-WIDE-STAGED-REJECT THEN
   RES-TRUE ;

: TFAM-CTOR-STEP? ( n -- bool ) {: sym:n :}
   sym SUMV-FROM-CTOR-SYM 0= IF drop RES-FALSE EXIT THEN
   {: vid:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam vid TFAM-UNMAKE-ROW? IF fam vid TFAM-UNMAKE-STEP? EXIT THEN
   fam vid TFC-SEED {: dt:n seeded:bool :}
   seeded 0= IF RES-FALSE EXIT THEN
   fam vid dt seeded TFC-CONSTRUCT-STEP-VID
   RES-TRUE ;

\ The family term of the `^F<..>` DEPTH cells below the live top; false for
\ any other cell, an open or stale one included.
: LIVE-RECEIVER ( n -- n bool ) {: depth:n :}
   DCUR @ depth 0 ?do
      R-RES dup TAG S-PUSH <> IF drop 0 RES-FALSE unloop EXIT THEN
      P>REST
   loop
   R-RES dup TAG S-PUSH <> IF drop 0 RES-FALSE EXIT THEN
   P>TYPE T-RES dup TAG T-PTR <> IF drop 0 RES-FALSE EXIT THEN
   PTR>INNER T-RES dup TAG T-PARAM <> IF drop 0 RES-FALSE EXIT THEN
   RES-TRUE ;

\ Member SHAPE PARAM's rows over the instance TERM: its published rows with the
\ family's variables replaced by TERM's arguments.
: MEMBER-ROWS ( n n n -- n n ) {: shape:n param:n term:n :}
   term TFC-ARGS!
   FRESH MK-ROW {: base:n :}
   term MK-PTR base MK-PUSH {: din:n :}
   shape CHILD-ABI:INDEX = IF
      CC-N MK-CON din MK-PUSH  term MK-PTR base MK-PUSH EXIT
   THEN
   shape CHILD-ABI:SPAN = IF din  SPAN-U8 base PUSH-LOGICAL EXIT THEN
   param PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM {: ft:n :}
   din  ft MK-PTR base MK-PUSH ;

\ Files member SHAPE PARAM's number NUM on the token, which T0 adds in place of
\ calling the member's word.
: MEMBER-FACT ( n n n -- ) {: shape:n param:n num:n :}
   REC-ON @ 0= IF EXIT THEN
   shape CHILD-ABI:INDEX = IF REC-IX @ num 0 CW-MEMBER-STRIDE CWIN-ADD EXIT THEN
   shape CHILD-ABI:SPAN = IF
      REC-IX @ num param PF-BYTES@ CW-MEMBER-SPAN CWIN-ADD EXIT
   THEN
   REC-IX @ num 0 CW-MEMBER-OFFSET CWIN-ADD ;

\ Whether NUM differs from the number member SHAPE PARAM's own word adds: a
\ field's or span's declared offset, AT's declared stride.
: MEMBER-MOVED? ( n n n -- bool ) {: shape:n param:n num:n :}
   shape CHILD-ABI:INDEX = IF param TFAM-SLOTS@ CELL * ELSE param PF-BYTE-OFF@ THEN
   num <> ;

\ Records the entry name of TERM's family and, unless its width is one cell
\ whatever its arguments, each argument's, as T-WIDTH measures them.
: WIDTH-SPELLED ( n -- ) {: t:n :}
   t T-UNSTALE {: r:n :}
   r TAG T-PARAM <> IF EXIT THEN
   r PARAM>FAM {: fam:n :}
   fam 0 < IF EXIT THEN
   fam ENTRY-NAME$ COMPILE-READS:TEXT+
   fam TFAM-BOXED-OR-NICHE? IF EXIT THEN
   fam TFAM-LAYOUT? 0= IF EXIT THEN
   r PARAM>ARGC 0 ?do r i PARAM>ARG RECURSE loop ;

: MEMBER-SPELLED ( n n n -- ) {: shape:n param:n term0:n :}
   term0 T-UNSTALE {: term:n :}
   term PARAM>ARGC 0 ?do
      shape param i MEMBER-READS? IF term i PARAM>ARG WIDTH-SPELLED THEN
   loop ;

\ At top level the word runs with its declared number, so a moved one is rejected
\ and a kept one still makes the part depend on the widths it reads.
: TFAM-MEMBER-STEP? ( n -- bool ) {: sym:n :}
   sym SYM-MEMBER 0= IF 2drop RES-FALSE EXIT THEN {: shape:n param:n :}
   shape CHILD-ABI:INDEX = IF 1 ELSE 0 THEN LIVE-RECEIVER
   0= IF drop RES-FALSE EXIT THEN {: term:n :}
   shape param term MEMBER-AT 0= IF drop RES-FALSE EXIT THEN {: num:n :}
   TOP-STEP @ 0 <>  shape param num MEMBER-MOVED?  and IF
      MD-MEMBER-TOP MDIAG!  0 OK !  -1 FAILSET !  RES-TRUE EXIT
   THEN
   shape param term MEMBER-ROWS RECORDED-STEP
   shape param term MEMBER-SPELLED
   TOP-STEP @ 0= IF shape param num MEMBER-FACT THEN
   RES-TRUE ;

\ MATCH resolution (docs/forth.md "Structures And Enums") follows signature
\ scope, not construct's owner-only rule: a family matches wherever a stack
\ signature can name it (TFAM-RESOLVE's scope, or PKG:tail), since a value of it
\ arrives only through such a signature. Only sum and enum kinds match. The arm
\ payload row instantiates the variant schema over the scrutinee's argument terms
\ in the TFC scratch vars, consumed at OF, so construct and nested matches may
\ interleave.
: TFAM-MATCH-KIND ( n -- n bool ) {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF MD-FAM-KIND MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFAM-MATCH-FAM ( str -- n bool ) {: na:^ nu:n :}   \ folded family token
   TFAM-ACTIVE-PKG$ na nu TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF MD-FAM-AMBIG MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   sr SR-FOUND <> IF MD-FAM-UNKNOWN MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   id TFAM-MATCH-KIND ;

\ The family a node applies. EFFECT:NAMED resolved it as source does, so a
\ built match needs no scope resolve.
: TFAM-MATCH-NODE ( n -- n bool )
   TFAM-NODE-FAM 0= IF drop MD-FAM-UNKNOWN MDIAG-COMPILE! 0 RES-FALSE EXIT THEN
   TFAM-MATCH-KIND ;

: TFAM-MATCH-VARIANT ( str n -- n bool ) {: na:^ nu:n fam:n :}
   fam na nu SUMV-FIND ;

: TFAM-MATCH-PAY ( n n n -- n ) {: vid:n term:n row:n :}   \ variant payload onto row
   term T-RES TFC-ARGS!
   vid term TFAM-MATCH-PADS-RECORD
   vid row RES-FALSE TFC-PAY-ROW ;

\ Fixed storage walks a field's instantiated schema but computes its own
\ offsets: a wide argument moves every following field.
\ The stored type has already passed CHECKER-STORAGE-INFO's closed-layout gate.
: TFAM-STORAGE-QUOT? ( n -- bool ) T-RES TAG T-QUOT = ;
: TFAM-STORAGE-PRODUCT? ( n -- n bool )
   T-RES dup TAG T-PARAM <> IF drop 0 RES-FALSE EXIT THEN
   PARAM>FAM dup TFAM-PRODUCT? IF RES-TRUE ELSE drop 0 RES-FALSE THEN ;
: TFAM-STORAGE-SUM? ( n -- n bool )
   T-RES dup TAG T-PARAM <> IF drop 0 RES-FALSE EXIT THEN
   PARAM>FAM dup TFAM-SUM? over TFAM-ENUM? or
   IF RES-TRUE ELSE drop 0 RES-FALSE THEN ;
: TFAM-FIELD-TERM ( n n -- n ) {: term:n field:n :}
   term TFC-ARGS!
   field PF-SCH@ SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM ;
: TFAM-STORAGE-FIELD ( n n -- n n ) {: term:n field:n :}
   term field TFAM-FIELD-TERM
   field PF-FLAGS@ PF-FLAG-BUFFER = IF field PF-CELLS@ EXIT THEN   \ a buffer: its cells
   dup T-WIDTH ;
: TFAM-PAY-TERM ( n n n -- n ) {: term:n vid:n index:n :}
   term TFC-ARGS!
   vid index SUMV-PAY-ROOT SCHEMA-ROOT@ TFC-USE TFC-SCH-TERM ;
: TFAM-STORAGE-PAY ( n n n -- n n )
   TFAM-PAY-TERM dup T-WIDTH ;

\ Whether any cell of the type holds an address: a quotation (code) or a pointer
\ (its pointee is not followed), in every sum alternative, since the tag can change.
: HAS-ADDRESS? ( n -- bool ) {: term:n :}
   term TFAM-STORAGE-QUOT? IF RES-TRUE EXIT THEN
   term T-RES TAG T-PTR = IF RES-TRUE EXIT THEN
   term TFAM-STORAGE-PRODUCT? IF
      {: fam:n :}
      fam TFAM-FLD-COUNT@ 0 ?do
         fam TFAM-FLD-START@ i + {: field:n :}
         term field TFAM-FIELD-TERM recurse IF
            RES-TRUE unloop EXIT
         THEN
      loop RES-FALSE EXIT
   THEN drop
   term TFAM-STORAGE-SUM? IF
      {: fam:n :}
      0 BEGIN dup fam TFAM-VAR-COUNT@ < WHILE
         {: idx:n :}
         fam TFAM-VAR-START@ idx + {: vid:n :}
         vid SUMV-PAY-N 0 ?do
            term vid i TFAM-PAY-TERM recurse IF
               RES-TRUE unloop EXIT
            THEN
         loop
         idx 1 +
      REPEAT drop RES-FALSE EXIT
   THEN drop
   RES-FALSE ;

\ What tier 0's construct and MATCH resolve: the checker's registry and scope
\ rules, but no diagnostic latch or checker row; the checker judges at `;`.

: TFL-SUMKIND? ( n -- bool ) {: id:n :}   \ constructible/matchable kind
   id TFAM-SUM? id TFAM-ENUM? or ;

: TFL-FOLD$ ( str -- str )      \ fold a raw engine token (shared TKF buffer)
   TOKFOLD drop TKF TKFU @ ;

: TFL-CON-FAM? ( str -- n bool ) {: na:^ nu:n :}   \ owner-only scope
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-FIND 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFL-MATCH-FAM? ( str -- n bool ) {: na:^ nu:n :}   \ signature scope
   TFAM-ACTIVE-PKG$ na nu TFL-FOLD$ TFAM-SIG-RESOLVE SR-FOUND <> IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFL-SUMKIND? 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TFL-VPADS ( n n -- n ) {: fam:n vid:n :}   \ zero pads M-p for a variant's construct
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - ;

\ The checker's friend hooks: checker.f loads before this file and reaches
\ family resolution and arities through these defers during signature parsing.
: TFAM-HOOK-INSTALL ( -- )
   [: TFAM-PARSE-RESOLVE ;] is TFAM-RESOLVE-XT
   [: TFAM-CLAIMED? ;] is TFAM-CLAIMED?-XT
   \ The constructor checks answer from the rows read: a linked family is read first.
   [: READ-PENDING TFAM-CTOR-OWNER ;]   is CTOR-OWNER-XT    \ package reopen
   [: READ-PENDING TFAM-CTOR-WORD? ;]   is CTOR-WORD?-XT    \ generated-word undefine
   [: READ-PENDING TFAM-CTOR-EXTEND? ;] is CTOR-EXTEND?-XT  \ closed-package extra tail
   [: TFAM-ARITY@ ;]  is TFAM-ARITY-XT
   [: TFAM-LAYOUT? ;] is TFAM-LAYOUT?-XT   \ checker reaches the layout kind for its fail-closed guard
   [: TFAM-CELL? ;]   is TFAM-CELL?-XT     \ nominal scalars: checker reaches the cell kind for LAYOUT-BUFFER admission + pointee governance
   [: TFAM-PKG$ ;]    is TFAM-PKG-XT       \ a signature graph's family package
   [: TFAM-WIDTH@ ;]  is TFAM-WIDTH-XT     \ checker reads DECLARED logical widths (params-as-cells) for the boot fallback
   [: TFAM-INST-WIDTH@ ;] is TFAM-INST-WIDTH-XT   \ arg-aware instantiated width for T-WIDTH and the WF fact surface
   [: TFAM-WIDTH-SLOT? ;] is TFAM-WIDTH-SLOT-XT   \ which argument slots that width reads: an open slot it does not read is still placeable
   [: TFAM-PK@ PK-SCOPE = ;] is TFAM-SCOPE-SLOT-XT
   [: TFAM-PK@ PK-REGION = ;] is TFAM-REGION-SLOT-XT
   [: 2dup TFAM-PK@ PK-TYPE = >r TFAM-WIDTH-SLOT? 0= r> and ;] is TFAM-TYPE-SLOT-XT
   [: TFAM-STORE-SLOT? ;] is TFAM-STORE-SLOT-XT   \ which argument slots reach storage: a pair of one demands both sides
   [: SUMV-FROM-CTOR-SYM nip ;] is CTOR-ROW?-XT ;   \ a generated constructor row moves exactly its payload (checker.f LIN-EFF-PASS)
TFAM-HOOK-INSTALL

;package

\ A build's registries start with the families above, which every program
\ starts with; a product declares the rest for itself (CHECK:STORES-OPEN), so
\ its SPAN:span never meets hb's. The tail indexes and the memo start empty, and
\ their first use rebuilds them.
package TFAM
private
EXPORT CHECK:STORE+
EXPORT CHECK:TABLE+
EXPORT CHECK:CELL-ROW+

TF-STR-P TF-STR-CAP-V 1 TF-STR-U STORE+
TF-PK-P TF-PK-CAP-V CELL TF-PK-N STORE+
TF-A-P TF-CAP-V TF-REC TFAM-N STORE+
TFM-P TFM-CAP TFM-REC TABLE+
TFX-A-P TFX-SLOTS-V CELL TABLE+
TFX-READY 0 CELL-ROW+
TFX-HI 0 CELL-ROW+
TFX-CAP 0 CELL-ROW+
SUMV-A-P SUMV-CAP-V SUMV-REC SUMV-N STORE+
VNX-B-P VNX-SLOTS-V CELL TABLE+
VNX-L-P VNX-LCAP CELL TABLE+
CPX-B-P VNX-SLOTS-V CELL TABLE+
CPX-L-P VNX-LCAP CELL TABLE+
VNX-READY 0 CELL-ROW+
VNX-HI 0 CELL-ROW+
VNX-CAP 0 CELL-ROW+
PF-A-P PF-CAP-V PF-REC PF-N STORE+
PF-COMMIT-N PF-COMMIT-N @ CELL-ROW+
FVX-B-P FVX-SLOTS-V CELL TABLE+
FVX-L-P FVX-LCAP CELL TABLE+
FVX-READY 0 CELL-ROW+
FVX-HI 0 CELL-ROW+
FVX-CAP 0 CELL-ROW+
LAY-A-P LAY-CAP-V LAY-REC LAY-N STORE+
;package

package SCHEMA-REG
EXPORT CHECK:STORE+

SCH-A-P SCH-CAP-V SCH-REC SCH-N STORE+
SCH-ROOT-P SCH-ROOT-CAP-V CELL SCH-ROOT-N STORE+
;package
