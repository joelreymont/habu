\ type-match-suite.f — checked MATCH suite (docs/forth.md "Structures And
\ Enums"), run by the engine over stdin like the other type suites:
\     bin/hb < test/type-match-suite.f
\ MATCH refines each branch with its variant's payload, joins branch outputs and
\ enforces exhaustiveness over declaration-order tags (no default branch).
\ test/match-factor-pin.f owns the native CLI cases.

require src/lib/string.f
require src/lib/test.f

T-RESET

SUMTYPE mres
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
SUMTYPE mmix
  VARIANT small n ;VARIANT
  VARIANT big ptr u8 n n ;VARIANT
;SUMTYPE
SUMTYPE men
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE
SUMTYPE mptr
  VARIANT ok  ptr u8 ;VARIANT
  VARIANT err ptr u8 ;VARIANT
;SUMTYPE
SUMTYPE mpoly<a,b>
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
\ TMLIN declares the linear `mtok`, so FREE-MTOK, the abstract consumer that
\ releases one to nothing, is checked over the package's private LINEAR: erase
\ row. The cases name both by their qualified spelling and stay at top level:
\ `construct` resolves its family owner-only, so KNL3 below stops certifying the
\ moment a package is open around it.
package TMLIN
public
DEFLINEAR TMLIN:mtok
private
LINEAR: MTOK>N ( TMLIN:mtok -- n )
public
: FREE-MTOK ( TMLIN:mtok -- ) MTOK>N drop ;
;package
SUMTYPE mlin<a,b>
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
ENUM-DECL:ED-RUN mnamedlin
  VARIANT hold FIELD token TMLIN:mtok ;VARIANT
  VARIANT empty ;VARIANT
;ENUM
\ Variants spelled like stack words: the constructors MWV:DUP/MWV:SWAP are
\ qualified defs, declared before the fixtures that use bare dup/swap as prims
\ (M9, CS1, CS2, PS*). Those still certify: a qualified def publishes no
\ bare-global effect row.
SUMTYPE mwv
  VARIANT dup  n ;VARIANT
  VARIANT swap n ;VARIANT
;SUMTYPE
\ A genuine ENUM-declared family (kind TK-ENUM, not a SUMTYPE 0): MATCH must
\ eliminate it like the enum-shaped sum `men` above, since the family-kind gate
\ is sum or enum (docs/forth.md "Structures And Enums"). Its generated
\ constructors are MECOLOR:RED / MECOLOR:GREEN / MECOLOR:BLUE.
ENUM mecolor
  red
  green
  blue
;ENUM

\ Accepted: exhaustive matches with joined rows and refined payloads.
s" M1=" type s" T1 ( mres -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ payload refinement: the wide variant's ptr u8 n n row is live in its branch.
s" M2=" type s" T2 ( mmix -- n ) MATCH mmix small OF ENDOF big OF nip nip ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ refinement visible in the certified effect: both payloads type ptr u8.
s" M3=" type s" T3 ( mptr -- ptr u8 ) MATCH mptr ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ enum-shaped family (zero-payload variants, width 1).
s" M4=" type s" T4 ( men -- n ) MATCH men lit OF 1 ENDOF dark OF 2 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ parametric family: args recovered from the scrutinee's concrete expansion.
s" M5=" type s" T5 ( mpoly<n,n> -- n ) MATCH mpoly ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" M6=" type s" T6 ( mpoly<ptr u8,ptr u8> -- ptr u8 ) MATCH mpoly ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ nested match (frames stack) fed by the construct form.
s" M7=" type s" T7 ( mres -- n ) MATCH mres ok OF construct mres err MATCH mres ok OF ENDOF err OF ENDOF ;MATCH ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ ordinary control interleaves inside a branch body.
s" M8=" type s" T8 ( mres f -- n ) {: f:bool :} MATCH mres ok OF f if 1 + then ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" M9=" type s" T9 ( mres -- n ) MATCH mres ok OF case 1 of 7 endof 9 swap endcase ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" M10=" type s" T10 ( mres n -- n ) case 1 of MATCH mres ok OF ENDOF err OF ENDOF ;MATCH endof swap drop 0 swap endcase" CHECK-QUIET-CANDIDATE! -1 T=
\ a local named like a variant cannot shadow the captured variant token.
s" M11=" type s" T11 ( mres n -- n ) {: ok:n :} MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ early exit in one branch; the live branch supplies the join.
s" M13=" type s" T13 ( mres -- n ) MATCH mres ok OF exit ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ every branch exits: no normal continuation, fold-exits certifies the def.
s" M14=" type s" T14 ( mres -- n ) MATCH mres ok OF exit ENDOF err OF exit ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ balanced return-stack traffic inside a branch.
s" M15=" type s" T15 ( mres -- n ) MATCH mres ok OF >r 0 r> + ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-OK" type cr

\ Linear payloads: matching a linear sum consumes the scrutinee once (popped at
\ MATCH entry), and each arm must consume its refined payload exactly once: no
\ leak, drop, copy or double consume. ML1-ML4 consume by construct round-trips
\ (re-mint); ML5-ML12 consume to nothing through TMLIN:FREE-MTOK.
s" MLC=" type s" KLC ( TMLIN:mtok -- TMLIN:mtok TMLIN:mtok ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" MLN1=" type s" KNL1 ( mnamedlin -- mnamedlin mnamedlin ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" MLN2=" type s" KNL2 ( mnamedlin -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" ML1=" type s" K1 ( mlin<TMLIN:mtok,n> -- mlin<TMLIN:mtok,n> ) MATCH mlin ok OF construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ moving the linear payload out through the join is legitimate consumption.
SUMTYPE mlin2<a,b>
  VARIANT ok  a ;VARIANT
  VARIANT err a ;VARIANT
;SUMTYPE
s" ML2=" type s" K2 ( mlin2<TMLIN:mtok,TMLIN:mtok> -- TMLIN:mtok ) MATCH mlin2 ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ dropping or copying the linear payload in a branch rejects.
s" ML3=" type s" KB1 ( mlin<TMLIN:mtok,n> -- mlin<TMLIN:mtok,n> ) MATCH mlin ok OF drop 0 construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" ML4=" type s" KB2 ( mlin<TMLIN:mtok,n> -- mlin<TMLIN:mtok,n> ) MATCH mlin ok OF dup drop construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ Named-field construct and MATCH use the same declared payload and preserve
\ the linear token exactly once.
s" MLN3=" type s" KNL3 ( TMLIN:mtok -- mnamedlin ) construct mnamedlin hold" CHECK-QUIET-CANDIDATE! -1 T=
s" MLN4=" type s" KNL4 ( mnamedlin -- n ) MATCH mnamedlin hold OF TMLIN:FREE-MTOK 0 ENDOF empty OF 0 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MLN5=" type s" KNL5 ( mnamedlin -- n ) MATCH mnamedlin hold OF 0 ENDOF empty OF 0 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ full consume: the linear arm frees its refined payload to nothing, the err arm
\ (payload n, non-linear) passes it through -> accept.
s" ML5=" type s" K5 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF TMLIN:FREE-MTOK 0 ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ leak: the refined linear payload is never consumed (only n produced) -> reject.
s" ML6=" type s" K6 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF 0 ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ double-consume: freeing the payload twice (use-after-free) -> reject.
s" ML7=" type s" K7 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF TMLIN:FREE-MTOK TMLIN:FREE-MTOK 0 ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ leak on early exit: the payload is live when the arm exits -> reject.
s" ML8=" type s" K8 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF exit ENDOF err OF 0 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ forward the payload out through the join (both arms carry mtok) -> accept.
s" ML9=" type s" K9 ( mlin2<TMLIN:mtok,TMLIN:mtok> -- TMLIN:mtok ) MATCH mlin2 ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ forward and free the same payload (copy then consume) -> reject.
s" ML10=" type s" K10 ( mlin2<TMLIN:mtok,TMLIN:mtok> -- TMLIN:mtok ) MATCH mlin2 ok OF dup TMLIN:FREE-MTOK ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ payload moved via balanced return-stack traffic then consumed -> accept
\ (the move-class relaxation composes with match refinement).
s" ML11=" type s" K11 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF >r 0 r> TMLIN:FREE-MTOK ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ payload stranded on the return stack (>r, never r>) -> reject.
s" ML12=" type s" K12 ( mlin<TMLIN:mtok,n> -- n ) MATCH mlin ok OF >r 0 ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MATCH-LINEAR" type cr

\ ENUM-declared family under MATCH: a real TK-ENUM eliminates, round-trips
\ through its generated constructor, and enforces exhaustiveness and
\ no-duplicate arms like a sum.
s" ME1=" type s" TE1 ( mecolor -- n ) MATCH mecolor red OF 0 ENDOF green OF 1 ENDOF blue OF 2 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ constructor -> match round-trip: MECOLOR:GREEN yields a mecolor the match pops.
s" ME2=" type s" TE2 ( -- n ) MECOLOR:GREEN MATCH mecolor red OF 0 ENDOF green OF 1 ENDOF blue OF 2 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ non-exhaustive enum match rejects (blue arm missing).
s" MEB1=" type s" BE1 ( mecolor -- n ) MATCH mecolor red OF 0 ENDOF green OF 1 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ duplicate enum variant arm rejects.
s" MEB2=" type s" BE2 ( mecolor -- n ) MATCH mecolor red OF 0 ENDOF red OF 1 ENDOF green OF 2 ENDOF blue OF 3 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ a raw n is not the enum: the kind/scrutinee gate rejects.
s" MEB3=" type s" BE3 ( n -- n ) MATCH mecolor red OF 0 ENDOF green OF 1 ENDOF blue OF 2 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MATCH-ENUM" type cr

\ Rejected, a hard reject 0 and never uncheckable 1: exhaustiveness, duplicates,
\ resolution, scrutinee shape, joins, truncation, strays, depth overflow.
s" MB1=" type s" B1 ( mres -- n ) MATCH mres ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB2=" type s" B2 ( mres -- n ) MATCH mres ok OF ENDOF ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB3=" type s" B3 ( mres -- n ) MATCH mres small OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB4=" type s" B4 ( mres -- n ) MATCH nofam ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB5=" type s" B5 ( n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB6=" type s" B6 ( mmix -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB7=" type s" B7 ( mres n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ scrutinee not on top
s" MB8=" type s" B8 ( span<g,n,n> -- n ) MATCH span ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ cell family: kind-gated
\ branch-output join mismatch (E-MATCH-BRANCH-JOIN, docs/forth.md
\ "Structures And Enums").
SUMTYPE mjoin
  VARIANT ok  ptr u8 ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
s" MB9=" type s" B9 ( mjoin -- n ) MATCH mjoin ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ return-stack mismatch across branches.
s" MB10=" type s" B10 ( mres -- n ) MATCH mres ok OF >r ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ truncated forms: every prefix of the block rejects.
s" MB11=" type s" B11 ( mres -- n ) MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB12=" type s" B12 ( mres -- n ) MATCH mres" CHECK-QUIET-CANDIDATE! 0 T=
s" MB13=" type s" B13 ( mres -- n ) MATCH mres ok" CHECK-QUIET-CANDIDATE! 0 T=
s" MB14=" type s" B14 ( mres -- n ) MATCH mres ok OF ENDOF err OF ENDOF" CHECK-QUIET-CANDIDATE! 0 T=
\ strays and malformed branch heads.
s" MB15=" type s" B15 ( n -- n ) ;match" CHECK-QUIET-CANDIDATE! 0 T=
s" MB16=" type s" B16 ( mres -- n ) MATCH mres of OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ missing variant token
s" MB17=" type s" B17 ( mres -- n ) MATCH mres ok drop OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ variant without OF
s" MB18=" type s" B18 ( mres -- n ) MATCH mres ok OF ENDOF endcase" CHECK-QUIET-CANDIDATE! 0 T=   \ CASE closer cannot close a match
\ open-arg parametric scrutinee (one conservative logical cell) rejects in v1.
s" MB19=" type s" B19 ( mpoly<a,b> -- a ) MATCH mpoly ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ quotation rows are open (forward inference): a match inside [: ;] rejects.
s" MB20=" type s" B20 ( mres -- n ) [: MATCH mres ok OF ENDOF err OF ENDOF ;MATCH ;] execute" CHECK-QUIET-CANDIDATE! 0 T=
\ dead code after an all-exit match stays dead code.
s" MB21=" type s" B21 ( mres -- n ) MATCH mres ok OF exit ENDOF err OF exit ENDOF ;MATCH 0" CHECK-QUIET-CANDIDATE! 0 T=
s" MATCH-BAD" type cr

\ CASE pins beside the shared OF/ENDOF surface, which the enclosing frame kind
\ dispatches. They use bare `dup` as a prim after `mwv` published its MWV:DUP
\ constructor, so they also pin that a qualified def leaks no bare-global row.
s" CS1=" type s" C1 ( n -- n ) case 1 of 2 endof dup endcase" CHECK-QUIET-CANDIDATE! -1 T=
s" CS2=" type s" C2 ( n n -- n ) case 1 of 1 + endof 2 of 2 + endof drop dup endcase" CHECK-QUIET-CANDIDATE! -1 T=
s" CS3=" type s" C3 ( n -- n ) endof" CHECK-QUIET-CANDIDATE! 0 T=
s" CASE-GREEN" type cr

\ Variants spelled like stack words are captured operands, never word calls:
\ family and variant tokens are captured before locals, control and word lookup.
s" M12=" type s" T12 ( mwv -- n ) MATCH mwv dup OF ENDOF swap OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ A public family declared prim-named variants above, and bare dup/swap still
\ bind the prims in these later checked bodies.
s" PS1=" type s" PS1 ( n n -- n n ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" PS2=" type s" PS2 ( n -- n n ) dup"    CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-WORDVAR" type cr

\ Frame headroom: a match two CFS slots from the cap is a hard reject (verdict
\ 0, pinned diagnostics), not the silent-uncheckable CF overflow; the same shape
\ with headroom certifies.
$400 constant MD-CAP
create MD-BUF MD-CAP allot
variable MD-U
variable MD-I
: MD-CLEAR ( -- ) 0 MD-U ! ;
: MD-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   MD-U @ u + MD-CAP > if s" match-suite: depth fixture too long" 76 die then
   0 MD-I !
   begin MD-I @ u < while
      a MD-I @ + c@  MD-BUF MD-U @ + c!
      MD-U @ 1 + MD-U !
      MD-I @ 1 + MD-I !
   repeat ;
variable MD-J   \ MD-BODY's own loop index: MD-APP clobbers MD-I per append
: MD-BODY ( n -- ptr u8 n ) {: k:n :}   \ k if-frames around one full match
   MD-CLEAR
   s" MD ( f mres -- f mres ) " MD-APP
   0 MD-J !  begin MD-J @ k < while s" over if " MD-APP MD-J @ 1 + MD-J ! repeat
   s" MATCH mres ok OF construct mres ok ENDOF err OF construct mres err ENDOF ;MATCH " MD-APP
   0 MD-J !  begin MD-J @ k < while s" then " MD-APP MD-J @ 1 + MD-J ! repeat
   MD-BUF MD-U @ ;
s" MD31=" type 31 MD-BODY CHECK-QUIET-CANDIDATE! 0 T=
s" MD29=" type 29 MD-BODY CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-DEPTH" type cr

\ Scope: family resolution is signature scope (eliminability = nameability),
\ unlike construct's owner-only rule. In-package private matches; cross-package
\ public matches bare and qualified.
package mscp
private
SUMTYPE msec
  VARIANT hide n ;VARIANT
;SUMTYPE
s" MS1=" type s" S1 ( msec -- n ) MATCH msec hide OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
;package
package mpub
public
SUMTYPE mpres
  VARIANT yes n ;VARIANT
;SUMTYPE
;package
s" MS2=" type s" S2 ( mpub:mpres -- n ) MATCH mpres yes OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MS3=" type s" S3 ( mpub:mpres -- n ) MATCH mpub:mpres yes OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-SCOPE" type cr

\ Diagnostics: every match/construct reject class carries a stable code, repair
\ class, reason and suggestion; nonexhaustive lists the missing variant names.
\ Captured through the render diag buffer in JSON mode; one prose case pins the
\ `bad match: missing variants` text.
create MDG-BUF 8192 allot
: MDG< ( ptr u8 n -- )   \ run one candidate with JSON diags into the buffer
   MDG-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: MDG? ( ptr u8 n -- )
   T-NEXT
   DIAG-BUFFER$ 2swap CONTAINS? 0= if
      s" diag needle missing" T-ASSERT-DETAIL
   then ;
: MDG-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

SUMTYPE mtri
  VARIANT aa n ;VARIANT
  VARIANT bb n ;VARIANT
  VARIANT cc n ;VARIANT
;SUMTYPE

s" G1 ( mres -- n ) MATCH nofam ok OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-UNKNOWN-FAMILY\"" MDG?
s\" \"repair_class\":\"fix_family_reference\"" MDG?
s" G2 ( mres -- n ) MATCH span ok OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-FAMILY-KIND\"" MDG?
s" G3 ( n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-SCRUTINEE\"" MDG?
s\" \"reason\":\"bad match: expected sum or enum value on stack\"" MDG?
s" G4 ( mmix -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-FAMILY-MISMATCH\"" MDG?
s" G5 ( mres -- n ) MATCH mres small OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-UNKNOWN-VARIANT\"" MDG?
s\" \"repair_class\":\"fix_variant_reference\"" MDG?
s" G6 ( mres -- n ) MATCH mres ok OF ENDOF ok OF ENDOF err OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-DUPLICATE-VARIANT\"" MDG?
s\" \"repair_class\":\"remove_duplicate_branch\"" MDG?
s" G7 ( mres -- n ) MATCH mres ok drop OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-MISSING-OF\"" MDG?
s" G8 ( mtri -- n ) MATCH mtri bb OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-NONEXHAUSTIVE\"" MDG?
s\" \"repair_class\":\"add_missing_branches\"" MDG?
s\" \"missing_variants\":\"aa cc\"" MDG?
s" G9 ( n -- n ) ;match" MDG<
s\" \"code\":\"E-MATCH-STRAY\"" MDG?
s" G10 ( mres -- n ) MATCH mres ok OF ENDOF err OF ENDOF" MDG<
s\" \"code\":\"E-MATCH-UNTERMINATED\"" MDG?
s" G11 ( mpoly<a,b> -- a ) MATCH mpoly ok OF ENDOF err OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-OPEN-ARGS\"" MDG?
s" G12 ( mres -- n ) [: MATCH mres ok OF ENDOF err OF ENDOF ;MATCH ;] execute" MDG<
s\" \"code\":\"E-MATCH-QUOTATION\"" MDG?
s" G13 ( mjoin -- n ) MATCH mjoin ok OF ENDOF err OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-BRANCH-JOIN\"" MDG?
s\" \"repair_class\":\"fix_branch_outputs\"" MDG?
s" G14 ( mres -- n ) construct nofam ok" MDG<
s\" \"code\":\"E-CONSTRUCT-UNKNOWN-FAMILY\"" MDG?
s" G15 ( mres -- n ) construct span ok" MDG<
s\" \"code\":\"E-CONSTRUCT-FAMILY-KIND\"" MDG?
s" G16 ( mres -- n ) construct mres nope" MDG<
s\" \"code\":\"E-CONSTRUCT-UNKNOWN-VARIANT\"" MDG?
s" G17 ( n -- mres ) construct mres" MDG<
s\" \"code\":\"E-CONSTRUCT-UNTERMINATED\"" MDG?
\ Products are not eliminable or constructible through the token forms — the
\ family-kind gate is sum/enum only (docs/forth.md "Structures And Enums"); the
\ product surface is its generated PKG:MAKE/PKG:UNMAKE words.
PRODUCT mprod
  FIELD v n
;PRODUCT
s" GP1 ( mprod -- n ) MATCH mprod make OF ENDOF ;MATCH" MDG<
s\" \"code\":\"E-MATCH-FAMILY-KIND\"" MDG?
s" GP2 ( n -- mprod ) construct mprod make" MDG<
s\" \"code\":\"E-CONSTRUCT-FAMILY-KIND\"" MDG?
31 MD-BODY MDG<
s\" \"code\":\"E-MATCH-DEPTH\"" MDG?
s\" \"repair_class\":\"factor_match_nesting\"" MDG?
MDG-END
\ prose shape: the `bad match: missing variants` text, prose mode.
MDG-BUF 8192 DIAG-BUFFER!
s" G18 ( mtri -- n ) MATCH mtri bb OF ENDOF ;MATCH" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" bad match: missing variants: aa cc" CONTAINS? TTRUE
DIAG-BUFFER-OFF
s" MATCH-DIAG" type cr

\ MATCH on a named multi-cell layout arg: the some-branch payload expands the
\ product to its W hidden cells (type-family.f TFC-PAY-ROW), so UNMAKE and typed
\ locals receive the exact fields (ML1) and branch outputs join. Over-dropping
\ the bundle leaves a branch imbalance (ML2): the payload is the wide bundle.
PRODUCT mlw2 FIELD x n FIELD y n ;PRODUCT
SUMTYPE mlopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
s" ML1 ( mlopt<mlw2> -- n ) MATCH mlopt none OF 0 ENDOF some OF MLW2:UNMAKE {: x:n y:n :} x y + ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" ML2 ( mlopt<mlw2> -- n ) MATCH mlopt none OF 0 ENDOF some OF drop drop 1 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MATCH-LAYOUT" type cr

\ MATCH on a nested named instantiation: the some-branch payload of
\ mnopt<mnres<n,n>> is the whole 2-cell inner bundle, destructured by a second
\ MATCH (MN1). Over-dropping it leaves a branch imbalance (MN2); the arm can
\ re-wrap it into a fresh outer bundle (MN3), so the payload is not a scalar.
SUMTYPE mnres<a,b> VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE
SUMTYPE mnopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
s" MN1 ( mnopt<mnres<n,n>> -- n ) MATCH mnopt none OF 999 ENDOF some OF MATCH mnres ok OF ENDOF err OF ENDOF ;MATCH ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MN2 ( mnopt<mnres<n,n>> -- n ) MATCH mnopt none OF 0 ENDOF some OF drop drop 1 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MN3 ( mnopt<mnres<n,n>> -- mnopt<mnres<n,n>> ) MATCH mnopt none OF MNOPT:NONE ENDOF some OF MNOPT:SOME ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-NESTED" type cr

\ Unified ENUM declarations store named payload rows in TYPE-FIELD: construct,
\ MATCH, generic substitution, nested applications, payload order and
\ diagnostics consume the same declared-payload accessors as linearity.
ENUM-DECL:ED-RUN nec red green blue ;ENUM
ENUM-DECL:ED-RUN nep
  VARIANT mk FIELD x n FIELD y n ;VARIANT
;ENUM
ENUM-DECL:ED-RUN nei
  VARIANT quit ;VARIANT
  VARIANT move FIELD dx n FIELD dy n ;VARIANT
;ENUM
ENUM-DECL:ED-RUN neg<a>
  VARIANT some FIELD value a ;VARIANT
  VARIANT none ;VARIANT
;ENUM
ENUM-DECL:ED-RUN nen
  VARIANT wrap FIELD child nec ;VARIANT
;ENUM

s" NEC1=" type s" NEC1 ( -- nec ) construct nec green" CHECK-QUIET-CANDIDATE! -1 T=
s" NEC2=" type s" NEC2 ( nec -- n ) MATCH nec red OF 0 ENDOF green OF 1 ENDOF blue OF 2 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NEC3=" type s" NEC3 ( nec -- n ) MATCH nec red OF 0 ENDOF green OF 1 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" NEP1=" type s" NEP1 ( n n -- nep ) construct nep mk" CHECK-QUIET-CANDIDATE! -1 T=
s" NEP2=" type s" NEP2 ( nep -- n ) MATCH nep mk OF + ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NEP3=" type s" NEP3 ( n n -- n ) construct nep mk MATCH nep mk OF + ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NEP4=" type s" NEP4 ( n ptr u8 -- nep ) construct nep mk" CHECK-QUIET-CANDIDATE! 0 T=
s" NEI1=" type s" NEI1 ( nei -- n ) MATCH nei quit OF 0 ENDOF move OF + ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NEG1=" type s" NEG1 ( n -- neg<n> ) construct neg some" CHECK-QUIET-CANDIDATE! -1 T=
s" NEG2=" type s" NEG2 ( neg<n> -- n ) MATCH neg some OF ENDOF none OF 0 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NEN1=" type s" NEN1 ( nen -- n ) MATCH nen wrap OF MATCH nec red OF 0 ENDOF green OF 1 ENDOF blue OF 2 ENDOF ;MATCH ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=

s" NED1 ( n ptr u8 -- nep ) construct nep mk" MDG<
s\" \"variant\":\"mk\"" MDG?
s\" \"payload_pos\":1" MDG?
s\" \"field\":\"y\"" MDG?
MDG-END
s" ENUM-CONSTRUCT-MATCH" type cr

T-REPORT
