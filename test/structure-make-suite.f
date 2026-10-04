\ structure-make-suite.f — behavior + rollback suite for the STRUCTURE
\ constructor generator (src/core/structure-make.f, package STRUCTURE-MAKE; dot
\ habu-structure-generate-make-872a6e75). A WHITEBOX-SUITE row: the
\ transaction, registry, and generation words are engine-internal, so they
\ resolve on the whitebox engine and never on the product.
\
\ The field-kind and role/arity matrix of the generator runs from real STRUCTURE
\ syntax in test/structure-decl-suite.f (sections 10-11) and
\ test/structure-certify-suite.f, whose ;STRUCTURE calls the same
\ STRUCTURE-MAKE:GENERATE. This suite keeps the rejects no front end can
\ present: a family is registered by hand (TFAM-DECL), its fields driven through
\ a DECL-EVENT transaction, and STRUCTURE-MAKE:GENERATE called with an enum id,
\ an out-of-range id, an empty product, a rolled-back field row (the field
\ record's E-PF-ID committed-reader reject) and a second generation. Every
\ reject happens before any registry write, so the variant / product-field /
\ schema registries stay byte-identical (the canonical-zero invariant).
\ structure-make.f is baked, so this suite uses STRUCTURE-MAKE:GENERATE directly
\ (no require).
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require lib/fmt.f                        \ FMT:.INT - one-line number text

using SCHEMA-REG
using TFAM

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
   then ;

1 constant CON-N       \ CC-N  (cell)

\ scratch.
variable TOK       \ live decl-event transaction token
variable BASE      \ committed field high-water at declaration open (this family's field start)
variable NF        \ running field count for the open declaration
variable SLOTC     \ running slot / cell offset for the open declaration
variable TC        \ caught throw code
variable NE        \ enum family id
variable NEVS      \ enum variant-start
variable SUMV0 variable SCHR0 variable PF0 variable STRU0   \ registry watermarks

\ per-family ids.
variable FEN variable FEM variable FRB

\ ---------------------------------------------------------------------------
\ declaration helpers: register + drive a structure exactly as the front end
\ will, then hand the published family id to the generator.
\ ---------------------------------------------------------------------------
: S-DECL ( ptr u8 n n -- n ) {: ta:ptr tu:n ar:n :}   \ public product family in package "sm"
   s" sm" CHECKER-PACKAGE-PUBLIC ta tu ar TK-PRODUCT TFAM-DECL ;

: S-OPEN ( n -- ) {: fam:n :}   \ open a decl-event declaration for fam
   TYPE-FIELD:COUNT BASE !  0 NF !  0 SLOTC !
   DECL-EVENT:OPEN TOK !
   TOK @ fam DECL-EVENT:DECL TOK ! ;

: S-FIELD ( n ptr u8 n n -- ) {: fam:n na:ptr nu:n sch:n :}   \ one cell-wide field at the next slot
   TOK @ fam na nu sch  SLOTC @  1  SLOTC @ CELL *  CELL  CELL  0  DECL-EVENT:FIELD TOK !
   SLOTC @ 1 + SLOTC !
   NF @ 1 + NF ! ;

: S-BIND ( n -- ) {: fam:n :}   \ record layout width and field range while rows remain provisional
   fam BASE @ NF @ TFAM-FLD-RANGE!
   fam NF @ TFAM-SLOTS! ;

: S-GENERATE ( n -- ) {: fam:n :}
   fam S-BIND
   TOK @ fam STRUCTURE-MAKE:GENERATE
   TOK @ DECL-EVENT:PUBLISH ;

\ schema-root builder (each field needs its own root).
: SR-CELL  ( -- n ) CON-N SCHEMA-CON SCHEMA-ROOT+ ;

\ ---------------------------------------------------------------------------
\ clean slate + a public arity-0 enum `ne` (width 1): the non-product family
\ section 1 hands the generator. Declared through the raw registry seam (it is a
\ dependency, not the structure under test): two variants, no payload.
\ ---------------------------------------------------------------------------
TFAM-RESET
SCHEMA-RESET
DECL-EVENT:RESET
s" sm" CHECKER-PACKAGE-PUBLIC s" ne" 0 TK-ENUM TFAM-DECL NE !
SUMV-N@ NEVS !
NE @ s" red"  0 0 0 0 SUMV-ADD drop
NE @ s" blue" 1 0 0 0 SUMV-ADD drop
NE @ NEVS @ 2 TFAM-VAR-RANGE!
NE @ 0 TFAM-SLOTS!

\ ---------------------------------------------------------------------------
\ 1. Non-product / non-live family rejects (E-SM-FAM 7190), before any write.
\ ---------------------------------------------------------------------------
0 NE @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop         \ an enum is not a product
TC @ 7190 T=
0 TFAM-N@ 100 + ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop \ an out-of-range id is not live
TC @ 7190 T=

\ ---------------------------------------------------------------------------
\ 2. A live public product with no fields rejects (E-SM-EMPTY 7191).
\ ---------------------------------------------------------------------------
s" en" 0 S-DECL FEN !
0 FEN @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop
TC @ 7191 T=

\ ---------------------------------------------------------------------------
\ 3. A rolled-back / unpublished field row rejects because its stale event
\     token cannot authorize a provisional read, and rejected generation writes NO
\     registry: SUMV / schema-root / product-field / string-pool watermarks are
\     byte-identical before and after (the canonical-zero invariant).
\ ---------------------------------------------------------------------------
s" rb" 0 S-DECL FRB !
TYPE-FIELD:COUNT BASE !
DECL-EVENT:OPEN TOK !
TOK @ FRB @ DECL-EVENT:DECL TOK !
TOK @ FRB @ s" x" SR-CELL 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
TOK @ DECL-EVENT:ROLLBACK                               \ retire the field: its id stays uncommitted
FRB @ BASE @ 1 TFAM-FLD-RANGE!                          \ front-end field range points at the retired id
FRB @ 1 TFAM-SLOTS!
SUMV-N@ SUMV0 !   SCHEMA-ROOT-N@ SCHR0 !   TYPE-FIELD:COUNT PF0 !   TF-STR-U@ STRU0 !
TOK @ FRB @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop
TC @ 7161 T=                                            \ stale event token cannot authorize a provisional read
SUMV-N@ SUMV0 @ T=                                      \ no variant rows written
SCHEMA-ROOT-N@ SCHR0 @ T=                               \ no schema roots appended
TYPE-FIELD:COUNT PF0 @ T=                               \ no field rows committed
TF-STR-U@ STRU0 @ T=                                    \ no names interned

\ ---------------------------------------------------------------------------
\ 4. A second generation for the same family rejects (E-SM-DUP 7102) and again
\     leaves every registry byte-identical: MAKE/UNMAKE publish exactly once.
\ ---------------------------------------------------------------------------
s" em" 0 S-DECL FEM !
FEM @ S-OPEN
FEM @ s" x" SR-CELL S-FIELD
FEM @ S-GENERATE                                        \ first generation succeeds
SUMV-N@ SUMV0 !   SCHEMA-ROOT-N@ SCHR0 !   TYPE-FIELD:COUNT PF0 !   TF-STR-U@ STRU0 !
TOK @ FEM @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop  \ second generation rejects before any provisional read
TC @ 7102 T=
SUMV-N@ SUMV0 @ T=
SCHEMA-ROOT-N@ SCHR0 @ T=
TYPE-FIELD:COUNT PF0 @ T=
TF-STR-U@ STRU0 @ T=

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-make-suite: failures" 1 die ;
REPORT

;using
;using
