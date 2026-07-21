\ structure-decl-suite.f — behavior + rollback suite for the STRUCTURE typed
\ declaration front end (src/core/structure-decl.f, package STRUCTURE-DECL; dot
\ habu-structure-parse-typed-c5a01e1f). Run BY THE ENGINE over stdin, exactly
\ like test/decl-event-suite.f (the definer parses the live input stream and
\ mutates the type registry, so it resolves only at top-level interpret):
\     bin/hb < test/structure-decl-suite.f
\ Proves: a successful declaration persists its family + fields; field events
\ reach TYPE-FIELD reflection with the declared names, schemas, order, slots, and
\ byte offsets; POLICY and DERIVE reach both the event stream and the family
\ record; every reject anchor (E-TDECL-* family + the field record's own name
\ gate) fires at the offending token; a mid-declaration reject leaves every
\ registry cursor byte-identical to the pre-declaration baseline; the
\ deterministic snapshot identity is reproducible for an identical declaration
\ against a fresh registry; and — the reconciliation seam — a public STRUCTURE
\ with fields generates a working sealed FAMILY:MAKE/UNMAKE ctor package that
\ round-trips bit-identically in declaration order, while a rejected, an opaque
\ zero-field, and a private declaration each generate no ctor words.
\ A failure prints F<index> + detail; REPORT exits 1.

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

\ --- boundary shims: the STRUCTURE opener, evaluate, and the sealed pre-hook
\ registry / schema reflection words are reached at top level through named
\ trusted forwarders (the same idiom test/decl-event-suite.f uses).
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;            \ evaluate under catch -> throw code
TRUSTED: FAMID ( ptr u8 n -- n ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop ;
TRUSTED: FAM-POLICY@ ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: FAM-EQ? ( n -- bool ) TFAM-DERIVE-EQ? ;
TRUSTED: FAM-HASH? ( n -- bool ) TFAM-DERIVE-HASH? ;
TRUSTED: FAM-SLOTS@ ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: SCH-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: SCH-TAG@ ( n -- n ) SCHEMA-TAG@ ;
TRUSTED: SCH-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: PACKED# ( -- n ) TL-PACKED-TAG ;
TRUSTED: STACK# ( -- n ) TL-STACK-CELL-TAG ;
TRUSTED: SCHCON# ( -- n ) SCH-CON ;
TRUSTED: SCHPARAM# ( -- n ) SCH-PARAM ;
TRUSTED: CCN# ( -- n ) CC-N ;
TRUSTED: CCF# ( -- n ) CC-BOOL ;
TRUSTED: CCR# ( -- n ) CC-R ;

\ --- registry snapshot, so a reject can be proven byte-identical and the
\ identity test can re-run an identical declaration against a fresh registry
\ (the in-process proxy for a fresh process: same family id both times).
variable RB-TFAM  variable RB-STR  variable RB-PK  variable RB-SUMV
variable RB-LAY   variable RB-SCH  variable RB-ROOT  variable RB-PFN  variable RB-PFC
TRUSTED: REG-MARK ( -- )
   TFAM-N @ RB-TFAM !  TF-STR-U @ RB-STR !  TF-PK-N @ RB-PK !
   SUMV-N @ RB-SUMV !  LAY-N @ RB-LAY !  SCH-N @ RB-SCH !  SCH-ROOT-N @ RB-ROOT !
   PF-N @ RB-PFN !  PF-COMMIT-N @ RB-PFC ! ;
TRUSTED: REG-RESTORE ( -- )
   RB-TFAM @ TFAM-N !  RB-STR @ TF-STR-U !  RB-PK @ TF-PK-N !
   RB-SUMV @ SUMV-N !  RB-LAY @ LAY-N !  RB-SCH @ SCH-N !  RB-ROOT @ SCH-ROOT-N !
   RB-PFN @ PF-N !  RB-PFC @ PF-COMMIT-N ! ;
TRUSTED: TFAMN@ ( -- n ) TFAM-N @ ;
TRUSTED: SCHN@ ( -- n ) SCH-N @ ;
TRUSTED: SUMVN@ ( -- n ) SUMV-N @ ;

variable RC   variable FID   variable B   variable NODE   variable PFB   variable DEVB
variable SV0   \ variant-cursor watermark for the ctor-generation gating checks

\ ---------------------------------------------------------------------------
\ 1. A product declaration persists its family and both field rows, reachable
\    through TYPE-FIELD reflection in declaration order with the right layout.
\ ---------------------------------------------------------------------------
TFAM-N@ FID !
TYPE-FIELD:COUNT B !
s" STRUCTURE point 0 FIELD x n FIELD y n ;STRUCTURE" EV
TFAM-N@ FID @ 1 + T=                                  \ exactly one new family
TYPE-FIELD:COUNT B @ 2 + T=                           \ two committed field rows
B @ TYPE-FIELD:NAME$ s" x" CORE-STR= T-TRUE           \ first field is x
B @ TYPE-FIELD:SLOT@ 0 T=                             \ x at cell slot 0
B @ TYPE-FIELD:BYTE-OFF@ 0 T=
B @ 1 + TYPE-FIELD:NAME$ s" y" CORE-STR= T-TRUE       \ second field is y
B @ 1 + TYPE-FIELD:SLOT@ 1 T=                         \ y at cell slot 1
B @ 1 + TYPE-FIELD:BYTE-OFF@ CELL T=                  \ y at byte offset = one cell
s" point" FAMID FAM-SLOTS@ 2 T=                       \ product width = two field cells

\ ---------------------------------------------------------------------------
\ 2. The field schema reaches reflection: field type `n` is a concrete con node.
\ ---------------------------------------------------------------------------
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=                            \ a concrete-con schema node
NODE @ SCH-A@ CCN# T=                                 \ con code = n

\ ---------------------------------------------------------------------------
\ 3. Field events reach the shared event stream in order (DECL, FIELD, FIELD).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE evt 0 FIELD a n FIELD b n ;STRUCTURE" EV
DECL-EVENT:COUNT 4 T=                                 \ DECL + ARITY + two FIELD events
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:ARITY? T-TRUE
2 DECL-EVENT:FIELD? T-TRUE
3 DECL-EVENT:FIELD? T-TRUE
2 DECL-EVENT:VAR@ DECL-EVENT:NO-VARIANT T=            \ structure fields carry no variant

\ ---------------------------------------------------------------------------
\ 4. POLICY reaches both the family record and the event stream.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE ppk 0 POLICY packed-tag FIELD x n ;STRUCTURE" EV
s" ppk" FAMID FAM-POLICY@ PACKED# T=                  \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 5. DERIVE reaches both the family record and the event stream; two features
\    on one clause are accepted, each recorded once.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE der 0 DERIVE eq hash FIELD x n ;STRUCTURE" EV
s" der" FAMID FAM-EQ? T-TRUE                          \ eq derived
s" der" FAMID FAM-HASH? T-TRUE                        \ hash derived
2 DECL-EVENT:DERIVE? T-TRUE                           \ two DERIVE events after DECL + ARITY
3 DECL-EVENT:DERIVE? T-TRUE

\ ---------------------------------------------------------------------------
\ 6. A duplicate field name rejects through the field record (E-TFAM-DUP 7102)
\    and the whole provisional declaration rolls back byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
TYPE-FIELD:COUNT PFB !
DECL-EVENT:COUNT DEVB !
s" STRUCTURE dupf 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SCHN@ RB-SCH @ T=                                     \ schema nodes retired
SUMVN@ RB-SUMV @ T=                                   \ variant cursor unchanged
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
DECL-EVENT:COUNT DEVB @ T=                            \ nothing new published

\ ---------------------------------------------------------------------------
\ 7. Grammar / name / arity / type / policy / derive / terminator rejects, each
\    at the offending token with the E-TDECL-* family code.
\ ---------------------------------------------------------------------------
s" STRUCTURE field 0 ;STRUCTURE" TRY 7110 T=                     \ reserved keyword name
s" STRUCTURE n 0 ;STRUCTURE" TRY 7110 T=                         \ single-letter type name
s" STRUCTURE Bad 0 ;STRUCTURE" TRY 7101 T=                       \ upper-case name (case)
s" STRUCTURE foo q ;STRUCTURE" TRY 7108 T=                       \ non-numeric arity
s" STRUCTURE foo 24 ;STRUCTURE" TRY 7108 T=                      \ arity above the shared 23 cap
s" STRUCTURE foo 0 FIELD a nope ;STRUCTURE" TRY 7109 T=          \ unresolved field type
s" STRUCTURE foo 0 FIELD a Q ;STRUCTURE" TRY 7109 T=             \ upper-case single-letter type
s" STRUCTURE foo 0 FIELD a a ;STRUCTURE" TRY 7109 T=             \ parameter outside declared arity
s" STRUCTURE foo 0 VARIANT q ;VARIANT ;STRUCTURE" TRY 7107 T=    \ mixed legacy token
s" STRUCTURE foo 0 FIELD x n" TRY 7107 T=                        \ missing ;STRUCTURE
s" STRUCTURE foo 0 POLICY nope FIELD x n ;STRUCTURE" TRY 7116 T= \ unknown layout policy
s" STRUCTURE foo 0 DERIVE nope FIELD x n ;STRUCTURE" TRY 7119 T= \ unknown derive feature

\ ---------------------------------------------------------------------------
\ 8. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
\ ---------------------------------------------------------------------------
s" STRUCTURE twice 0 FIELD x n ;STRUCTURE" EV
s" STRUCTURE twice 0 FIELD x n ;STRUCTURE" TRY 7102 T=

\ ---------------------------------------------------------------------------
\ 9. Deterministic snapshot identity: an identical declaration against a fresh
\    registry (family id restored, event log reset) folds to the same identity;
\    a different declaration folds to a different one. These declarations are
\    PRIVATE so the in-process registry-restore trick can re-declare the same
\    family id without regenerating (and colliding on) its sealed ctor words —
\    the snapshot identity is a DECL-EVENT property (family id + events, not
\    visibility), so private and public declarations fold identically. The
\    public ctor-generation seam is proven separately in cases 10-13.
\ ---------------------------------------------------------------------------
package IDENTTEST
private
REG-MARK
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC !                              \ RC holds identity A
REG-RESTORE                                           \ retire family; fresh registry
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC @ T=                           \ identical declaration -> same identity
REG-RESTORE
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n FIELD y n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC @ <> T-TRUE                    \ different declaration -> different identity
public
;package

\ ---------------------------------------------------------------------------
\ 10. End-to-end wiring (the ;STRUCTURE -> STRUCTURE-MAKE:GENERATE seam): a public
\     STRUCTURE with fields, declared from real syntax, generates a working sealed
\     MAKE/UNMAKE ctor package (two variant rows: make + unmake). MAKE then UNMAKE
\     is a bit-identical physical no-op that preserves declaration order.
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
s" STRUCTURE tri 0 FIELD a n FIELD b n FIELD c n ;STRUCTURE" EV
SUMVN@ SV0 @ 2 + T=                                   \ exactly two ctor variant rows generated
: TRIRT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE ;      \ callable sealed ctor words
11 22 33 TRIRT 33 T= 22 T= 11 T=                      \ declaration order + values round-trip bit-identically

\ ---------------------------------------------------------------------------
\ 11. The generated MAKE/UNMAKE are exact type inverses across a cell + byte +
\     generic field mix (the make-suite candidate whitebox check, driven here from
\     real STRUCTURE syntax): `abc` has a generic parameter field, a cell field,
\     and a byte field, and at a concrete instantiation (parameter a = n) the
\     round-trip certifies with its declaration-order field types.
\ ---------------------------------------------------------------------------
s" STRUCTURE abc 1 FIELD p a FIELD k n FIELD c char ;STRUCTURE" EV
s" ABCRT ( n n char -- n n char ) ABC:MAKE ABC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic-only structure round-trips its parameter at a concrete instantiation.
s" STRUCTURE gp 1 FIELD v a ;STRUCTURE" EV
: GPRT ( n -- n ) GP:MAKE GP:UNMAKE ;
42 GPRT 42 T=

\ The post-hook STRUCTURE parser consumes the shared declaration alphabet:
\ g is parameter 5, z is parameter 22, and f/n/r remain concrete scalar fields
\ even though the declared arity extends past their old letter positions.
TYPE-FIELD:COUNT B !
s" STRUCTURE sdmap 23 FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e FIELD p05 g FIELD flag f FIELD integer n FIELD real r FIELD last z ;STRUCTURE" EV
B @ 5 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHPARAM# T=   NODE @ SCH-A@ 5 T=
B @ 6 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=     NODE @ SCH-A@ CCF# T=
B @ 7 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=     NODE @ SCH-A@ CCN# T=
B @ 8 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=     NODE @ SCH-A@ CCR# T=
B @ 9 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHPARAM# T=   NODE @ SCH-A@ 22 T=
s" SDMAPRT ( n n n n n n bool n r char -- n n n n n n bool n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDMAPBAD ( n n n n n bool n n r char -- n n n n n bool n n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ 12. Constructor generation is gated (SD-MAKEABLE?): only a PUBLIC structure WITH
\     fields owns a ctor package. A rejected declaration and an opaque zero-field
\     declaration each generate NO ctor words (the variant cursor is unchanged).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
s" STRUCTURE badf 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=   \ duplicate field rejects
SUMVN@ SV0 @ T=                                       \ no ctor words from a rejected declaration
SUMVN@ SV0 !
s" STRUCTURE opaque 0 ;STRUCTURE" EV                  \ zero-field opaque one-cell family (docs/type-families.md §2.2)
SUMVN@ SV0 @ T=                                       \ an opaque family owns no MAKE/UNMAKE

\ ---------------------------------------------------------------------------
\ 13. A PRIVATE structure with fields owns no construction surface either:
\     SD-MAKEABLE? requires a public family, matching the shipped product
\     precedent (private products publish no MAKE/UNMAKE, fail-closed).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
package SDPRIVTEST
private
STRUCTURE hidden 0 FIELD x n ;STRUCTURE
public
;package
SUMVN@ SV0 @ T=                                       \ private structure generates no ctor words

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-decl-suite: failures" 1 die ;
REPORT
