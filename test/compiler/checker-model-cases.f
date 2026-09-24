\ checker-model-cases.f - Exercise checker vocabulary and control-flow vectors through the real checker.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require test/checker-assert.f
require test/compiler/checker-model-schema.f

package CHECKER-MODEL-CASES
using CHECKER-MODEL-PROOF
public

\ The vectors' prelude. These are the Habu realisation of the model's `wStep1`
\ .. `wKeepAny` word effects and of its `ltok` linear con: one named word per
\ effect, each certifying on its own. Nothing pins them to the model directly,
\ because the vectors already do - change either side and the verdicts move
\ apart. `deflinear` writes a row into the checker's concrete type table, which
\ is one global table and not a wordlist, so the type name it mints is reachable
\ unqualified from any signature however the declaration is scoped; the name is
\ spelled distinctly for that reason.
deflinear cmltok

\ The sum family the MATCH depth vectors eliminate. It is the model's `fmres`:
\ two variants in declaration order, each carrying one `n` of payload. A family
\ is registered in one global type registry, like a linear type, so the name is
\ reachable unqualified from a candidate signature however the declaration is
\ scoped; it is spelled distinctly for that reason.
SUMTYPE cmres 0
  VARIANT cmok  n ;VARIANT
  VARIANT cmerr n ;VARIANT
;SUMTYPE

\ A second sum family whose two variants carry DIFFERENT payloads. It is the
\ model's `fmbool`, and the construct vectors use it for the one question a
\ single-payload family cannot ask: whether the payload a construct consumes
\ comes from the variant or merely from the family. A step that read the family
\ would answer the same for both variants; these two answer differently.
SUMTYPE cmbres 0
  VARIANT cmbf bool ;VARIANT
  VARIANT cmbn n ;VARIANT
;SUMTYPE

\ The two families the scrutinee-pop vectors need, and the only ones here whose
\ bundle is more than two cells. Each variant carries TWO cells, so the bundle is
\ three: two payload slots and the tag. `cmtwin` is `cmwide` again under another
\ name - same variant count, same payloads, same width - so nothing but the
\ family identity distinguishes the two, which is what those vectors are about.
\ They are the model's `fmwide` and `fmtwin`.
SUMTYPE cmwide 0
  VARIANT cmwa n n ;VARIANT
  VARIANT cmwb n n ;VARIANT
;SUMTYPE

SUMTYPE cmtwin 0
  VARIANT cmta n n ;VARIANT
  VARIANT cmtb n n ;VARIANT
;SUMTYPE

: STEP1 ( i64 -- i64 ) ;
: MK-CELL ( -- cell ) 0 ;
: MK-BOOL ( -- bool ) 0 0< ;
: DUP1 ( i64 -- i64 i64 ) dup ;
: DROP1 ( i64 -- ) drop ;
: MK-N ( -- n ) 0 ;
: DROP-N ( n -- ) drop ;
: DUP-POLY ( a -- a a ) dup ;
: DROP-POLY ( a -- ) drop ;
: KEEP-POLY ( a -- a ) ;

\ A declared call effect moves its argument to the return row. Unlike the
\ intrinsic >r rule, a call checks conservation before transferring return rows.
\ This is the model's wToRAsWord; the model cases never execute the callee.
defer TO-R-WORD ( a | -- | a )

\ The four words the rigid host-identity vectors need. A `fresh-*` name in a
\ signature is a TEMPLATE slot the checker mints an identity for at every call
\ site, so a word that PRODUCES one cannot be written in checked Habu at all:
\ minting a host identity is exactly what a trusted host constructor does and
\ what checked code is not allowed to do. These are that boundary, in the same
\ shape `lib/ptx/tile.f` declares `MK-SPAN` and `MK-MATRIX` in, and the vectors
\ below are their test - each of the six turns on what these four declare.
\
\ `MK-REGION-PAIR` names ONE slot twice, so its two outputs carry one identity;
\ `MK-REGION` and `MK-GEN` each name one slot in a different domain, so two
\ calls are two allocations and one call of each is two domains at the same
\ number. `SAME-ID` is the consumer that asks whether two atoms are one
\ identity. They are the model's `wMkRegionPair`, `wMkRegion`, `wMkGen` and
\ `wSameId`.
TRUSTED: MK-REGION ( -- fresh-region-a ) 0 ;
TRUSTED: MK-GEN ( -- fresh-gen-a ) 0 ;
TRUSTED: MK-REGION-PAIR ( -- fresh-region-a fresh-region-a ) 0 0 ;
: SAME-ID ( x x -- ) 2drop ;

private

\ ---- the shared program vectors ----------------------------------------------
\ `CHECK-QUIET-CANDIDATE!` answers -1 certified, 1 unresolvable, 0 refused. The
\ row stores the model's three-way verdict, so the mapping is written once here
\ and an unresolvable can never be read as a refusal.

: VERDICT-OF ( n -- n ) {: answer:n :}
   answer -1 = if V-CERT exit then
   answer 1 = if V-UNCK exit then
   V-REJECT ;

: VECTOR-ROW ( n -- ) {: k:n :}
   SB-RESET s" the shipped checker answers " SB-APPEND
   k VEC-VERD@ VERDICT-NAME$ SB-APPEND s"  for " SB-APPEND
   k VEC-NAME$ SB-APPEND SB$ T-LABEL
   k VEC-SRC$ CHECK-QUIET-CANDIDATE! VERDICT-OF k VEC-VERD@ T= ;

\ ---- where these vectors have to be asked ------------------------------------
\ Every `construct` row resolves its family in the ACTIVE package -
\ TFAM-CONSTRUCT-FAM, src/core/type-family.f, which reads the very
\ CHECKER-AUTH-PACKAGE$ this word reads - and the families these rows use are
\ declared in this package. Asked from anywhere else the checker refuses every
\ one of them with `bad construct: family not declared in the active package`,
\ which is a correct refusal about a program nobody wrote.
\
\ WHAT THAT COSTS IS NOT ONLY THE RED ROWS. The three rows the model says
\ certify go red, and the four that expect a refusal go GREEN FOR THE WRONG
\ REASON: measured at top level, `a_sibling_variant_of_the_same_family_wants_its
\ _own_payload` is refused for the missing family rather than for the payload
\ its variant does not have, and so is the underflow row. Four rows that assert
\ nothing look exactly like four rows that pass.
\
\ The phase reads the package authority itself and names a wrong scope before
\ executing the vectors.
\
\ IT ASKS FOR THE PACKAGE AND NOT FOR A CERTIFYING PROBE, deliberately. A probe
\ that ran a construct and required VCert would also fire if `construct` itself
\ ever regressed, and would report that as "asked in the wrong place". The
\ environment is what this row is about; a broken rule belongs to the rows that
\ test the rule.
: OWN-PACKAGE$ ( -- ptr u8 n )
   s" CHECKER-MODEL-CASES" ;

: ASKED-IN-OWN-PACKAGE? ( -- bool )
   CHECKER-AUTH-PACKAGE$ OWN-PACKAGE$ STR= ;

public

: VECTOR-PHASE ( -- )
   s" the vectors are asked inside the package that declares their families" T-LABEL
   ASKED-IN-OWN-PACKAGE? TTRUE
   VECTORS 0 ?do i VECTOR-ROW loop ;

\ ---- the whole Habu side -----------------------------------------------------

: HABU-SIDE ( -- )
   VECTOR-PHASE ;

;using
;package
