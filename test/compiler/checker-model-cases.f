\ checker-model-cases.f - Exercise checker vocabulary and control-flow vectors through the real checker.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/test/outcome.f
require test/compiler/checker-model-schema.f

package CHECKER-MODEL-CASES
using CHECKER-MODEL-PROOF
public

\ The model's `wStep1` .. `wKeepAny` word effects and its `ltok` linear con, one
\ certifying word per effect; the vectors pin them to the model. `deflinear`
\ writes the one global concrete type table, so the name is reachable
\ unqualified however the declaration is scoped and is spelled distinctly.
deflinear cmltok

\ The model's `fmres`: two variants in declaration order, one `n` payload each.
\ Families live in one global registry like linear types: spelled distinctly.
SUMTYPE cmres
  VARIANT cmok  n ;VARIANT
  VARIANT cmerr n ;VARIANT
;SUMTYPE

\ The model's `fmbool`: variants with different payloads, so a construct step
\ that read the family instead of the variant would answer the same for both.
SUMTYPE cmbres
  VARIANT cmbf bool ;VARIANT
  VARIANT cmbn n ;VARIANT
;SUMTYPE

\ The model's `fmwide` and `fmtwin`, for the scrutinee-pop vectors: each variant
\ carries two cells, a three-cell bundle; `cmtwin` repeats `cmwide`, so only the
\ family identity distinguishes them.
SUMTYPE cmwide
  VARIANT cmwa n n ;VARIANT
  VARIANT cmwb n n ;VARIANT
;SUMTYPE

SUMTYPE cmtwin
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

\ A `fresh-*` name is a template slot the checker mints an identity for at each
\ call site, so checked Habu cannot produce one: the producers are `defer`
\ declarations, never set or run. `MK-REGION-PAIR` names one slot twice (one
\ identity); `MK-REGION` and `MK-GEN` each name one slot in a different domain.
\ They are the model's `wMkRegionPair`, `wMkRegion`, `wMkGen` and `wSameId`.
defer MK-REGION ( -- fresh-region-a )
defer MK-GEN ( -- fresh-gen-a )
defer MK-REGION-PAIR ( -- fresh-region-a fresh-region-a )
: SAME-ID ( x x -- ) 2drop ;

private

\ ---- the shared program vectors ----------------------------------------------
\ `CHECK-QUIET-CANDIDATE!` answers -1 certified, 1 unresolvable, 0 refused; the
\ mapping is written once so an unresolvable never reads as a refusal.

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
\ Every `construct` row resolves its family in the active package
\ (src/core/type-family.f TFAM-CONSTRUCT-FAM, reading CHECKER-AUTH-PACKAGE$).
\ Asked elsewhere, the certifying rows refuse and the refusing rows pass for the
\ wrong reason (`family not declared in the active package`). The phase checks
\ the package itself: a certifying probe would blame the scope for a `construct`
\ regression.
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
