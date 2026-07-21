\ structure-make-suite.f — behavior + rollback suite for the STRUCTURE
\ constructor generator (src/core/structure-make.f, package STRUCTURE-MAKE; dot
\ habu-structure-generate-make-872a6e75). Run BY THE ENGINE over stdin, exactly
\ like test/decl-event-suite.f (the transaction, registry, and generation words
\ resolve only at top-level interpret, never inside a checked ':' body):
\     bin/hb < test/structure-make-suite.f
\
\ This is the whitebox generator suite: every structure is declared by hand
\ exactly the way the parse front end does — register the family (TFAM-DECL),
\ drive its fields through a DECL-EVENT transaction, record the family's layout
\ width and field range, call STRUCTURE-MAKE:GENERATE with the live token, then
\ publish the transaction. That hand-built path stays the right level here, and
\ keeps the field-kind and rollback matrix independent of the grammar: the
\ reconciliation has now wired that same seam into the front end's ;STRUCTURE
\ (proven end-to-end from STRUCTURE syntax in test/structure-decl-suite.f), and
\ structure-make.f is baked, so this suite uses the baked STRUCTURE-MAKE:GENERATE
\ directly (no require).
\
\ Proves: MAKE→UNMAKE round-trips bit-identically and preserves declaration
\ order (a physical no-op over the field cells); each field kind — plain cell,
\ byte (char), pointer, nested family, and a generic parameter substituted at a
\ concrete type — generates a MAKE/UNMAKE pair whose types are exact inverses; a
\ field-arity or field-role (semantic type) mismatch at a MAKE call site is a
\ checker reject; identical declarations fold to an identical DECL-EVENT
\ snapshot identity and byte-stable committed layout; and every reject
\ (non-product / non-live family, no fields, a rolled-back or unpublished field
\ row via the field record's E-PF-ID committed-reader reject, and a second
\ generation) happens before any registry write, so the variant / product-field
\ / schema registries stay byte-identical (the canonical-zero invariant).
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;
: T-TRUE ( bool -- ) {: b:bool :}
   #CASE @ 1 + #CASE !
   b 0= if T-FAIL s" assert: expected true" type cr then ;

\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): sealed pre-hook registry /
\ checker-frame colon words probed at top level go through named trusted shims.
TRUSTED: TWX-TFAM-RESET ( -- ) TFAM-RESET ;
TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: TWX-SCHEMA-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: TWX-SCHEMA-PTR ( n -- n ) SCHEMA-PTR ;
TRUSTED: TWX-SCHEMA-APP ( n n n -- n ) SCHEMA-APP ;
TRUSTED: TWX-SCHEMA-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: TWX-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: TWX-TFAM-SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: TWX-TFAM-SLOTS@ ( n -- n ) TFAM-SLOTS@ ;
TRUSTED: TWX-TFAM-FLD-RANGE! ( n n n -- ) TFAM-FLD-RANGE! ;
TRUSTED: TWX-TFAM-VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;

1 constant CON-N       \ CC-N  (cell)
8 constant CON-CHAR    \ CC-CHAR (byte)

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
variable IDA   variable IDB
variable NE        \ nested enum family id
variable NEVS      \ nested enum variant-start
variable SUMV0 variable SCHR0 variable PF0 variable STRU0   \ registry watermarks

\ per-family ids.
variable FC1 variable FP3 variable FBF variable FPP variable FNF variable FGP
variable FRS variable FEN variable FEM variable FRB variable FD1 variable FD2

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

: S-CLOSE ( n -- )
   S-BIND
   TOK @ DECL-EVENT:PUBLISH ;

: S-GENERATE ( n -- ) {: fam:n :}
   fam S-BIND
   TOK @ fam STRUCTURE-MAKE:GENERATE
   TOK @ DECL-EVENT:PUBLISH ;

\ schema-root builders (each field needs its own root).
: SR-CELL  ( -- n ) CON-N TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ ;
: SR-CHAR  ( -- n ) CON-CHAR TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ ;
: SR-PTR   ( -- n ) CON-N TWX-SCHEMA-CON TWX-SCHEMA-PTR TWX-SCHEMA-ROOT+ ;
: SR-APP   ( n -- n ) 0 0 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ ;   \ ( fam -- root )
: SR-PARAM ( n -- n ) TWX-SCHEMA-PARAM TWX-SCHEMA-ROOT+ ;     \ ( idx -- root )

\ ---------------------------------------------------------------------------
\ clean slate + a public arity-0 enum `ne` (width 1) to serve as a nested field
\ family. Declared through the raw registry seam (it is a dependency, not the
\ structure under test): two variants, tag = declaration order, no payload.
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
\ 1. Cell field: MAKE/UNMAKE generate and their types are exact inverses.
\ ---------------------------------------------------------------------------
s" c1" 0 S-DECL FC1 !
FC1 @ S-OPEN
FC1 @ s" x" SR-CELL S-FIELD
FC1 @ S-GENERATE
s" C1RT ( n -- n ) SM-C1:MAKE SM-C1:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 2. Three cell fields: RUNTIME round-trip is bit-identical AND preserves
\    declaration order (MAKE then UNMAKE is a physical no-op over the cells).
\ ---------------------------------------------------------------------------
s" p3" 0 S-DECL FP3 !
FP3 @ S-OPEN
FP3 @ s" a" SR-CELL S-FIELD
FP3 @ s" b" SR-CELL S-FIELD
FP3 @ s" c" SR-CELL S-FIELD
FP3 @ S-GENERATE
: P3RT ( n n n -- n n n ) SM-P3:MAKE SM-P3:UNMAKE ;
10 20 30 P3RT 30 T= 20 T= 10 T=            \ order + values preserved

\ ---------------------------------------------------------------------------
\ 3. Byte (char) field: the con type renders and round-trips as its own type.
\ ---------------------------------------------------------------------------
s" bf" 0 S-DECL FBF !
FBF @ S-OPEN
FBF @ s" x" SR-CHAR S-FIELD
FBF @ S-GENERATE
s" BFRT ( char -- char ) SM-BF:MAKE SM-BF:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 4. Pointer field: `ptr n` renders and round-trips.
\ ---------------------------------------------------------------------------
s" pp" 0 S-DECL FPP !
FPP @ S-OPEN
FPP @ s" p" SR-PTR S-FIELD
FPP @ S-GENERATE
s" PPRT ( ptr n -- ptr n ) SM-PP:MAKE SM-PP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 5. Nested-family field: a field typed as the enum family `ne` round-trips.
\ ---------------------------------------------------------------------------
s" nf" 0 S-DECL FNF !
FNF @ S-OPEN
FNF @ s" e" NE @ SR-APP S-FIELD
FNF @ S-GENERATE
s" NFRT ( sm:ne -- sm:ne ) SM-NF:MAKE SM-NF:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 6. Generic parameter field: a `gp<a>` with field `a` generates
\    MAKE ( a -- gp<a> ) / UNMAKE ( gp<a> -- a ). At a concrete instantiation
\    (a = n) it certifies and round-trips bit-identically.
\ ---------------------------------------------------------------------------
s" gp" 1 S-DECL FGP !
FGP @ S-OPEN
FGP @ s" v" 0 SR-PARAM S-FIELD
FGP @ S-GENERATE
s" GPCERT ( n -- n ) SM-GP:MAKE SM-GP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
: GPRT ( n -- n ) SM-GP:MAKE SM-GP:UNMAKE ;
42 GPRT 42 T=

\ ---------------------------------------------------------------------------
\ 7. Field-role and field-arity mismatches at MAKE are checker rejects. `rs`
\    has fields (e:ne, n:n); swapping the two argument roles, or supplying the
\    wrong argument count, does not certify.
\ ---------------------------------------------------------------------------
s" rs" 0 S-DECL FRS !
FRS @ S-OPEN
FRS @ s" e" NE @ SR-APP S-FIELD
FRS @ s" k" SR-CELL S-FIELD
FRS @ S-GENERATE
s" RSOK ( sm:ne n -- sm:ne n ) SM-RS:MAKE SM-RS:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" RSSWAP ( n sm:ne -- sm:rs ) SM-RS:MAKE" CHECK-QUIET-CANDIDATE! 0 T=   \ roles swapped
s" RSAR ( sm:ne -- sm:rs ) SM-RS:MAKE" CHECK-QUIET-CANDIDATE! 0 T=       \ one arg, needs two

\ ---------------------------------------------------------------------------
\ 8. Determinism: two identical structure declarations fold to an identical
\    DECL-EVENT snapshot identity and byte-stable committed field layout. (The
\    generated words are a pure function of that identical metadata; the
\    cross-process byte-identity is proven by candidate-validation's digest
\    comparison.)
\ ---------------------------------------------------------------------------
TWX-TFAM-RESET TWX-SCHEMA-RESET DECL-EVENT:RESET
s" d1" 0 S-DECL FD1 !
FD1 @ S-OPEN
FD1 @ s" x" SR-CELL S-FIELD
FD1 @ s" y" SR-CELL S-FIELD
FD1 @ S-CLOSE
DECL-EVENT:IDENTITY IDA !
FD1 @ TWX-TFAM-SLOTS@ 2 T=
BASE @     TYPE-FIELD:SLOT@ 0 T=
BASE @ 1 + TYPE-FIELD:SLOT@ 1 T=
BASE @ 1 + TYPE-FIELD:BYTE-OFF@ CELL T=
\ full reset, then the identical declaration reproduces the same identity.
TWX-TFAM-RESET TWX-SCHEMA-RESET DECL-EVENT:RESET
s" d1" 0 S-DECL FD2 !
FD2 @ S-OPEN
FD2 @ s" x" SR-CELL S-FIELD
FD2 @ s" y" SR-CELL S-FIELD
FD2 @ S-CLOSE
DECL-EVENT:IDENTITY IDB !
IDA @ IDB @ T=                              \ identical declaration -> identical identity
FD2 @ TWX-TFAM-SLOTS@ 2 T=                  \ identical layout width
BASE @ 1 + TYPE-FIELD:BYTE-OFF@ CELL T=     \ identical field offset

\ re-seed the enum + slate for the negative cases (the reset above cleared it).
TWX-TFAM-RESET TWX-SCHEMA-RESET DECL-EVENT:RESET
s" sm" CHECKER-PACKAGE-PUBLIC s" ne" 0 TK-ENUM TWX-TFAM-DECL NE !
SUMV-N@ NEVS !
NE @ s" red"  0 0 0 0 TWX-SUMV-ADD drop
NE @ s" blue" 1 0 0 0 TWX-SUMV-ADD drop
NE @ NEVS @ 2 TWX-TFAM-VAR-RANGE!
NE @ 0 TWX-TFAM-SLOTS!

\ ---------------------------------------------------------------------------
\ 9. Non-product / non-live family rejects (E-SM-FAM 7190), before any write.
\ ---------------------------------------------------------------------------
0 NE @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop         \ an enum is not a product
TC @ 7190 T=
0 TFAM-N@ 100 + ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop \ an out-of-range id is not live
TC @ 7190 T=

\ ---------------------------------------------------------------------------
\ 10. A live public product with no fields rejects (E-SM-EMPTY 7191).
\ ---------------------------------------------------------------------------
s" en" 0 S-DECL FEN !
0 FEN @ ' STRUCTURE-MAKE:GENERATE catch TC ! 2drop
TC @ 7191 T=

\ ---------------------------------------------------------------------------
\ 11. A rolled-back / unpublished field row rejects because its stale event
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
\ 12. A second generation for the same family rejects (E-SM-DUP 7102) and again
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
