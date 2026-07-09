\ type-match-suite.f — checked MATCH eliminator suite (PLAN item 9, docs
\ /type-families.md §13-14). Run BY THE ENGINE over stdin, like the other
\ type suites:  bin/hb < test/type-match-suite.f
\ MATCH is a checker control form: `MATCH family  variant OF ... ENDOF ...
\ ;MATCH` pops the scrutinee's hidden-field bundle, refines each branch with
\ that variant's instantiated payload, joins branch outputs, and enforces
\ exhaustiveness over the declaration-order tags (v1 has no default branch).
\ Family/variant tokens are captured before locals/control/word lookup;
\ OF/ENDOF are shared with CASE and dispatched by the enclosing frame kind.
\ Family resolution is signature scope (eliminability = nameability), unlike
\ construct's owner-only rule. All fixtures are CHECK-only candidates —
\ native/Gforth lowering is item 10 (engine fail-closure gate-pinned by
\ GE-CONSTRUCT-PENDING for the token protocol family).

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

\ ---------------------------------------------------------------------------
\ families under test: scalar sum, multi-cell + padding, enum-shaped,
\ ptr payloads, parametric, and linear-arg parametric.
\ ---------------------------------------------------------------------------
SUMTYPE mres 0
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
SUMTYPE mmix 0
  VARIANT small n ;VARIANT
  VARIANT big ptr u8 n n ;VARIANT
;SUMTYPE
SUMTYPE men 0
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE
SUMTYPE mptr 0
  VARIANT ok  ptr u8 ;VARIANT
  VARIANT err ptr u8 ;VARIANT
;SUMTYPE
SUMTYPE mpoly 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
deflinear mtok
SUMTYPE mlin 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE

\ ---------------------------------------------------------------------------
\ accepted: exhaustive matches with joined rows and refined payloads.
\ ---------------------------------------------------------------------------
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

\ ---------------------------------------------------------------------------
\ linear payloads: the branch consumes its payload exactly once — proven
\ TRUST-free by construct round-trips (the only checked linear consumers).
\ ---------------------------------------------------------------------------
s" ML1=" type s" K1 ( mlin<mtok,n> -- mlin<mtok,n> ) MATCH mlin ok OF construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ moving the linear payload out through the join is legitimate consumption.
SUMTYPE mlin2 2
  VARIANT ok  a ;VARIANT
  VARIANT err a ;VARIANT
;SUMTYPE
s" ML2=" type s" K2 ( mlin2<mtok,mtok> -- mtok ) MATCH mlin2 ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ dropping or copying the linear payload in a branch rejects.
s" ML3=" type s" KB1 ( mlin<mtok,n> -- mlin<mtok,n> ) MATCH mlin ok OF drop 0 construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" ML4=" type s" KB2 ( mlin<mtok,n> -- mlin<mtok,n> ) MATCH mlin ok OF dup drop construct mlin ok ENDOF err OF construct mlin err ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MATCH-LINEAR" type cr

\ ---------------------------------------------------------------------------
\ rejected: exhaustiveness, duplicates, resolution, scrutinee shape, joins,
\ truncation, strays, depth overflow (hard reject 0 — never uncheckable 1).
\ ---------------------------------------------------------------------------
s" MB1=" type s" B1 ( mres -- n ) MATCH mres ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB2=" type s" B2 ( mres -- n ) MATCH mres ok OF ENDOF ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB3=" type s" B3 ( mres -- n ) MATCH mres small OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB4=" type s" B4 ( mres -- n ) MATCH nofam ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB5=" type s" B5 ( n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB6=" type s" B6 ( mmix -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" MB7=" type s" B7 ( mres n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ scrutinee not on top
s" MB8=" type s" B8 ( span<g,n,n> -- n ) MATCH span ok OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=   \ cell family: kind-gated
\ branch-output join mismatch (docs §25.4 rejected branch join).
SUMTYPE mjoin 0
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

\ ---------------------------------------------------------------------------
\ CASE regression pins beside the shared OF/ENDOF surface (before the mwv
\ block below: its leaked bare `dup` record would shadow the prim here).
\ ---------------------------------------------------------------------------
s" CS1=" type s" C1 ( n -- n ) case 1 of 2 endof dup endcase" CHECK-QUIET-CANDIDATE! -1 T=
s" CS2=" type s" C2 ( n n -- n ) case 1 of 1 + endof 2 of 2 + endof drop dup endcase" CHECK-QUIET-CANDIDATE! -1 T=
s" CS3=" type s" C3 ( n -- n ) endof" CHECK-QUIET-CANDIDATE! 0 T=
s" CASE-GREEN" type cr

\ ---------------------------------------------------------------------------
\ variants spelled like stack words are captured operands, never word calls.
\ DECLARED LAST among the word-shape fixtures: a public family with prim-named
\ variants leaks bare `dup`/`swap` effect records that shadow the prims in
\ every LATER checked body (pre-existing engine record-call bug, dot
\ habu-qualified-defs-leak-aadeb5c9) — keep bare-prim-using fixtures above.
\ ---------------------------------------------------------------------------
SUMTYPE mwv 0
  VARIANT dup  n ;VARIANT
  VARIANT swap n ;VARIANT
;SUMTYPE
s" M12=" type s" T12 ( mwv -- n ) MATCH mwv dup OF ENDOF swap OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-WORDVAR" type cr

\ ---------------------------------------------------------------------------
\ frame headroom: a match two CFS slots from the cap HARD-rejects (verdict 0,
\ pinned diagnostics) instead of tripping the silent-uncheckable CF overflow;
\ the same shape with headroom certifies.
\ ---------------------------------------------------------------------------
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

\ ---------------------------------------------------------------------------
\ scope: signature-scope resolution — in-package private matches; cross-package
\ public matches bare and qualified (eliminability = nameability).
\ ---------------------------------------------------------------------------
package mscp
private
SUMTYPE msec 0
  VARIANT hide n ;VARIANT
;SUMTYPE
s" MS1=" type s" S1 ( msec -- n ) MATCH msec hide OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
end-package
package mpub
public
SUMTYPE mpres 0
  VARIANT yes n ;VARIANT
;SUMTYPE
end-package
s" MS2=" type s" S2 ( mpub:mpres -- n ) MATCH mpres yes OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MS3=" type s" S3 ( mpub:mpres -- n ) MATCH mpub:mpres yes OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" MATCH-SCOPE" type cr

\ ---------------------------------------------------------------------------
\ item 9 slice 4: §24 diagnostics. Every match/construct reject class carries
\ a stable code, repair class, reason, and suggestion; nonexhaustive lists the
\ missing variant NAMES. Captured through the render diag buffer in JSON mode
\ (the type-decl-suite pattern); one prose case pins the §24 text shape.
\ ---------------------------------------------------------------------------
variable MDT-I
: MDT-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   hu nu < if 0 0= 0= exit then
   0 MDT-I !
   begin MDT-I @ nu + hu <= while
      h MDT-I @ + nu  n nu CORE-STR= if 0 0= exit then
      MDT-I @ 1 + MDT-I !
   repeat 0 0= 0= ;

create MDG-BUF 8192 allot
: MDG< ( ptr u8 n -- )   \ run one candidate with JSON diags into the buffer
   MDG-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: MDG? ( ptr u8 n -- )   \ assert the captured diag contains the needle
   #CASE @ 1 + #CASE !
   DIAG-BUFFER$ 2swap MDT-CONTAINS? 0= if
      T-FAIL s" assert: diag needle missing" type cr
   then ;
: MDG-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

SUMTYPE mtri 0
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
31 MD-BODY MDG<
s\" \"code\":\"E-MATCH-DEPTH\"" MDG?
s\" \"repair_class\":\"factor_match_nesting\"" MDG?
MDG-END
\ prose shape: the §24 text with the missing-variant names, prose mode.
MDG-BUF 8192 DIAG-BUFFER!
s" G18 ( mtri -- n ) MATCH mtri bb OF ENDOF ;MATCH" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" bad match: missing variants: aa cc" MDT-CONTAINS? -1 T=
DIAG-BUFFER-OFF
s" MATCH-DIAG" type cr

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-match-suite: failures" 1 die ;
REPORT
