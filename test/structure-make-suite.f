\ structure-make-suite.f — behavior + rollback suite for the STRUCTURE
\ constructor generator (src/core/structure-make.f, package STRUCTURE-MAKE; dot
\ habu-structure-generate-make-872a6e75). Run BY THE ENGINE over stdin, exactly
\ like test/decl-event-suite.f (the transaction, registry, and generation words
\ resolve only at top-level interpret, never inside a checked ':' body):
\     bin/hb < test/structure-make-suite.f
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

\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): sealed pre-hook registry /
\ checker-frame colon words probed at top level go through named shims; a shim
\ stays TRUSTED: only where the name it forwards to is engine-internal and a
\ checked body cannot resolve it.
TRUSTED: TWX-TFAM-RESET ( -- ) TFAM-RESET ;
TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: TWX-SCHEMA-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: TWX-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: TWX-TFAM-SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: TWX-TFAM-FLD-RANGE! ( n n n -- ) TFAM-FLD-RANGE! ;
TRUSTED: TWX-TFAM-VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;

1 constant CON-N       \ CC-N  (cell)

\ CHECKER-PACKAGE-PUBLIC / TK-* are top-level-interpret-only checker words; bind
\ their values to plain constants so the checked declaration helpers can use them.
CHECKER-PACKAGE-PUBLIC constant PUBVIS
TK-PRODUCT constant KPROD

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
   s" sm" PUBVIS ta tu ar KPROD TWX-TFAM-DECL ;

: S-OPEN ( n -- ) {: fam:n :}   \ open a decl-event declaration for fam
   TYPE-FIELD:COUNT BASE !  0 NF !  0 SLOTC !
   DECL-EVENT:OPEN TOK !
   TOK @ fam DECL-EVENT:DECL TOK ! ;

: S-FIELD ( n ptr u8 n n -- ) {: fam:n na:ptr nu:n sch:n :}   \ one cell-wide field at the next slot
   TOK @ fam na nu sch  SLOTC @  1  SLOTC @ CELL *  CELL  CELL  0  DECL-EVENT:FIELD TOK !
   SLOTC @ 1 + SLOTC !
   NF @ 1 + NF ! ;

: S-BIND ( n -- ) {: fam:n :}   \ record layout width and field range while rows remain provisional
   fam BASE @ NF @ TWX-TFAM-FLD-RANGE!
   fam NF @ TWX-TFAM-SLOTS! ;

: S-GENERATE ( n -- ) {: fam:n :}
   fam S-BIND
   TOK @ fam STRUCTURE-MAKE:GENERATE
   TOK @ DECL-EVENT:PUBLISH ;

\ schema-root builder (each field needs its own root).
: SR-CELL  ( -- n ) CON-N TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ ;

\ ---------------------------------------------------------------------------
\ clean slate + a public arity-0 enum `ne` (width 1): the non-product family
\ section 1 hands the generator. Declared through the raw registry seam (it is a
\ dependency, not the structure under test): two variants, no payload.
\ ---------------------------------------------------------------------------
TWX-TFAM-RESET
TWX-SCHEMA-RESET
DECL-EVENT:RESET
s" sm" CHECKER-PACKAGE-PUBLIC s" ne" 0 TK-ENUM TWX-TFAM-DECL NE !
SUMV-N@ NEVS !
NE @ s" red"  0 0 0 0 TWX-SUMV-ADD drop
NE @ s" blue" 1 0 0 0 TWX-SUMV-ADD drop
NE @ NEVS @ 2 TWX-TFAM-VAR-RANGE!
NE @ 0 TWX-TFAM-SLOTS!

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
FRB @ BASE @ 1 TWX-TFAM-FLD-RANGE!                      \ front-end field range points at the retired id
FRB @ 1 TWX-TFAM-SLOTS!
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
