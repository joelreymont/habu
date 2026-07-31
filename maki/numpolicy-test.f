\ maki/numpolicy-test.f - checked acceptance for the typed numeric-policy family
\ (maki/numpolicy.f). Covers the plan §22.6 Exit line: an approximate candidate
\ cannot satisfy an exact policy; composed error bounds are deterministic; and the
\ op-registry bridge / wire roundtrip are fail-closed. Every refusal is paired with a
\ resolving positive control (per LESSONS), so no TTHROWS is vacuous. The requested
\ policy is now DERIVED PER-OP by maki/sched-key.f REGION-POL (folding OP-DOM over the
\ region's ops), so the KEY-invalidation half of the dot (a different op mix => a
\ different honest policy => a different plan/artifact key) is proven in
\ maki/sched-key-test.f (skey's pol field + the per-op SK-KEY$ render), and the golden
\ RECORD carrying a relative result refusing an exact policy in
\ maki/evidence/policy-e2e-test.f.

require lib/test.f
require test/checker-assert.f
require maki/numpolicy.f

\ ---- compact helpers (RANK is bijective with the domain, so rank pins the value) ---
: CMP    ( dom dom -- n )       NPOL:COMPOSE NPOL:RANK ;      \ compose, then rank
: SAT    ( dom dom -- bool )    NPOL:SATISFIES? ;
: OPDOM  ( MAKI:opkind -- n )   NPOL:OP-DOM NPOL:RANK ;       \ an op's achieved domain rank
: DOMRT  ( dom -- n )           NPOL:DOM>N NPOL:N>DOM NPOL:RANK ;   \ dom -> n -> dom roundtrip

\ ---- fail-closed probes ------------------------------------------------------
: TRY-NUM-N        ( -- )  MAKI:NUM-N NPOL:NUM>DOM drop ;     \ no op carries NUM-N
: TRY-NDOM-HI      ( -- )  4 NPOL:N>DOM drop ;
: TRY-NDOM-NEG     ( -- )  -1 NPOL:N>DOM drop ;

\ ---- fixtures: the checked refusal gate (ENFORCE) over the acceptance cases ----
\ TF32-vs-FP32 (the motivating confusion): FP32 FMA is the exact reference domain,
\ TF32 tensor-core is relative-error; approximate cannot satisfy exact.
: TF32-VS-FP32-NEG ( -- )  NPOL-DOM:RELATIVE NPOL-DOM:EXACT NPOL:ENFORCE ;
: TF32-VS-FP32-POS ( -- )  NPOL-DOM:EXACT    NPOL-DOM:EXACT NPOL:ENFORCE ;
\ GELU (approximate transcendental): registry numeric class relative; RELU is exact.
: GELU-NEG ( -- )  MAKI-OPKIND:GELU NPOL:OP-DOM NPOL-DOM:EXACT    NPOL:ENFORCE ;
: GELU-POS ( -- )  MAKI-OPKIND:GELU NPOL:OP-DOM NPOL-DOM:RELATIVE NPOL:ENFORCE ;
: RELU-POS ( -- )  MAKI-OPKIND:RELU NPOL:OP-DOM NPOL-DOM:EXACT    NPOL:ENFORCE ;
\ recompute (empirical): an empirical license satisfies only an empirical policy.
: RECOMPUTE-NEG-EXACT ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     NPOL:ENFORCE ;
: RECOMPUTE-NEG-REL   ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  NPOL:ENFORCE ;
: RECOMPUTE-POS       ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL NPOL:ENFORCE ;
\ pipeline composition: a TF32 relative matmul + an exact elementwise stage yields
\ the WEAKEST domain (relative), deterministically.
: PIPE-TF32-EXACT ( -- n )
   MAKI-OPKIND:MATMUL NPOL:OP-DOM  MAKI-OPKIND:RELU NPOL:OP-DOM  NPOL:COMPOSE NPOL:RANK ;

T-RESET

\ ---- strength rank ordering (exact strongest 0 .. empirical weakest 3) ---------
NPOL-DOM:EXACT     NPOL:RANK 0 T=
NPOL-DOM:ULP       NPOL:RANK 1 T=
NPOL-DOM:RELATIVE  NPOL:RANK 2 T=
NPOL-DOM:EMPIRICAL NPOL:RANK 3 T=

\ ---- key tokens --------------------------------------------------------------
NPOL-DOM:EXACT     NPOL:NAME s" exact" T$=
NPOL-DOM:ULP       NPOL:NAME s" ulp"   T$=
NPOL-DOM:RELATIVE  NPOL:NAME s" rel"   T$=
NPOL-DOM:EMPIRICAL NPOL:NAME s" emp"   T$=

\ ---- composition table (weakest / higher-rank wins; commutative + idempotent) --
NPOL-DOM:EXACT     NPOL-DOM:EXACT     CMP 0 T=
NPOL-DOM:EXACT     NPOL-DOM:ULP       CMP 1 T=
NPOL-DOM:EXACT     NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:EXACT     NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:ULP       NPOL-DOM:EXACT     CMP 1 T=
NPOL-DOM:ULP       NPOL-DOM:ULP       CMP 1 T=
NPOL-DOM:ULP       NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:ULP       NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:RELATIVE  NPOL-DOM:EXACT     CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:ULP       CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:ULP       CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL CMP 3 T=
\ COMPOSE returns the actual weaker VALUE, not just its rank.
NPOL-DOM:RELATIVE NPOL-DOM:EXACT NPOL:COMPOSE NPOL-DOM:RELATIVE NPOL-DOM:EQ TTRUE

\ ---- satisfaction: evidence must be at least as STRONG as the requirement ------
NPOL-DOM:EXACT     NPOL-DOM:EXACT     SAT TTRUE
NPOL-DOM:EXACT     NPOL-DOM:EMPIRICAL SAT TTRUE     \ exact satisfies any requirement
NPOL-DOM:ULP       NPOL-DOM:EXACT     SAT TFALSE
NPOL-DOM:ULP       NPOL-DOM:RELATIVE  SAT TTRUE
NPOL-DOM:RELATIVE  NPOL-DOM:EXACT     SAT TFALSE    \ approximate cannot satisfy exact
NPOL-DOM:RELATIVE  NPOL-DOM:RELATIVE  SAT TTRUE
NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     SAT TFALSE
NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  SAT TFALSE
NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL SAT TTRUE

\ ---- op-registry bridge: raw NUM-* class -> typed domain = the PER-OP request ---
\ OP-DOM is both an op's ACHIEVED domain and (folded over a region by sched-key.f
\ REGION-POL) its REQUESTED policy - the per-op axis that replaced the per-class
\ table, so a pure-gelu region requests relative while a pure-relu region stays exact.
MAKI-OPKIND:RELU    OPDOM 0 T=     \ NUM-EXACT
MAKI-OPKIND:CAST    OPDOM 0 T=     \ NUM-EXACT
MAKI-OPKIND:RESHAPE OPDOM 0 T=     \ movement: exact
MAKI-OPKIND:ADD     OPDOM 1 T=     \ NUM-ULP
MAKI-OPKIND:MUL     OPDOM 1 T=     \ NUM-ULP
MAKI-OPKIND:GELU    OPDOM 2 T=     \ NUM-RELTOL (approximate transcendental)
MAKI-OPKIND:MATMUL  OPDOM 2 T=     \ NUM-RELTOL (accumulated)
MAKI-OPKIND:LAYERNORM OPDOM 2 T=   \ NUM-RELTOL

\ ---- wire roundtrip (id == rank; inverse fails closed) ------------------------
NPOL-DOM:EXACT     DOMRT 0 T=
NPOL-DOM:ULP       DOMRT 1 T=
NPOL-DOM:RELATIVE  DOMRT 2 T=
NPOL-DOM:EMPIRICAL DOMRT 3 T=

\ ---- acceptance fixtures: refusals + positive controls (non-vacuous) -----------
' TF32-VS-FP32-NEG    E-NPOL-APPROX TTHROWS    \ TF32 relative result vs FP32 exact policy: refused
' TF32-VS-FP32-POS    0             TTHROWS    \ FP32 exact result vs FP32 exact policy: satisfied
' GELU-NEG            E-NPOL-APPROX TTHROWS    \ approximate GELU vs exact policy: refused
' GELU-POS            0             TTHROWS    \ GELU vs relative policy: satisfied
' RELU-POS            0             TTHROWS    \ exact RELU vs exact policy: satisfied (flipped verdict)
' RECOMPUTE-NEG-EXACT E-NPOL-APPROX TTHROWS    \ empirical recompute vs exact policy: refused
' RECOMPUTE-NEG-REL   E-NPOL-APPROX TTHROWS    \ empirical recompute vs relative policy: refused
' RECOMPUTE-POS       0             TTHROWS    \ empirical recompute vs empirical policy: satisfied
PIPE-TF32-EXACT 2 T=                           \ TF32 matmul + exact elementwise => relative (weakest)

\ ---- fail-closed throws ------------------------------------------------------
' TRY-NUM-N        E-NPOL-DOM   TTHROWS         \ NUM-N has no domain
' TRY-NDOM-HI      E-NPOL-DOM   TTHROWS         \ wire id out of range
' TRY-NDOM-NEG     E-NPOL-DOM   TTHROWS

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing here: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types. The pins and the shape twin
\ own their own test package rather than the global scope the older legs above use.
package NPOL-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

s" NP-C-OK ( CAD-KIND:numeric-policy-id -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:OK" YES
s" NP-C-WW ( -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:WRONG-WIDTH" YES
s" NP-C-UNK ( -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:UNKNOWN" YES
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the numeric-policy id.
s" NP-C-RAW ( n -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:OK" NO
s" NP-C-BARE ( CAD-KIND:numeric-policy-id -- n ) NPOL-ID--RESULT:OK" NO
s" NP-C-NONE ( -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:OK" NO
s" NP-C-FGN ( CAD-KIND:target-id -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:OK" NO

public

\ idr-twin is NPOL:id-result's SHAPE under a different name: same arity, same three
\ variants in the same order, same named payload field. It exists only so the
\ negatives below can prove decode-result identity is NOMINAL - two identically
\ shaped ENUM families never unify, in either direction. It has to be public: a
\ private family publishes no constructors at all, and the positive control below
\ builds through the twin's own ok, so neither negative can pass by being
\ unresolvable rather than ill-typed.
ENUM idr-twin<a>
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

s" NP-C-TWIN ( CAD-KIND:numeric-policy-id -- idr-twin<CAD-KIND:numeric-policy-id> ) NPOL--TEST-IDR--TWIN:OK" YES
s" NP-C-TWIN-X1 ( CAD-KIND:numeric-policy-id -- idr-twin<CAD-KIND:numeric-policy-id> ) NPOL-ID--RESULT:OK" NO
s" NP-C-TWIN-X2 ( CAD-KIND:numeric-policy-id -- NPOL:id-result<CAD-KIND:numeric-policy-id> ) NPOL--TEST-IDR--TWIN:OK" NO

;package

\ § 23.9 numeric-policy-id: constructor + wire codec round-trip + fail-closed decode.
\ Reopen the owner package for LE-PUT / DOM-COUNT (an out-of-range wire raw is only
\ forgeable inside the owning package).
package NPOL

512 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-RT ( dom -- n )                            \ 0 = REGISTER->ID>WIRE->WIRE>ID keeps the dom
   dup {: d:dom :}
   REGISTER TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF POLICY-DOM RANK  d RANK  = if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ raw == DOM-COUNT (one past the closed range): unknown
   DOM-COUNT  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
\ Numeric-policy's content key IS its 8-byte rank (the documented § 23.9 exception), so
\ the codec coincides with ID>WIRE / WIRE>ID; prove both the dom-preserving round-trip
\ and the byte-for-byte coincidence.
create TT-KBUF TT-WCAP allot

: TT-CKEY-RT ( dom -- n )                       \ 0 = REGISTER->KEY>WIRE->WIRE>KEY keeps the dom
   dup {: d:dom :}
   REGISTER TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF POLICY-DOM RANK  d RANK  = if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-IS-RANK ( -- n )                      \ 0 iff KEY>WIRE == ID>WIRE (the 8-byte rank)
   NPOL-DOM:RELATIVE REGISTER {: id:CAD-KIND:numeric-policy-id :}
   id TT-WBUF TT-WCAP ID>WIRE {: la:n :}
   id TT-KBUF TT-WCAP KEY>WIRE {: lb:n :}
   la lb <> if 1 exit then
   la WIRE-BYTES <> if 2 exit then
   0 begin dup la < while
      dup {: k:n :}
      TT-WBUF k + c@  TT-KBUF k + c@  <> if drop 3 exit then
      1+
   repeat drop 0 ;

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The wire words above reach the arms only through a decode. These construct each
\ variant DIRECTLY through the production producers and match it straight back, so
\ the named payload FIELD is proven to bind in declaration order. The ok arm binds
\ its payload to a TYPED local and reports the recovered raw, which for this family
\ IS the dom rank (the § 23.9 minimal content key), so a payload the constructor
\ dropped or zeroed would come back as a different rank instead of passing. The id
\ under test is `relative` (rank 2), so a zeroed payload is distinguishable from a
\ live one.
\
\ Construction is factored into one typed word per variant because the checker
\ requires MATCH's scrutinee to be a concretely instantiated family value: a single
\ word that both constructs and matches is refused, and the diagnostic names the
\ family token as an undefined word. That refusal predates this migration (it
\ reproduces identically on the legacy declaration) and is reported separately.
: TT-ID ( -- CAD-KIND:numeric-policy-id )   NPOL-DOM:RELATIVE REGISTER ;

: TT-MK-OK ( CAD-KIND:numeric-policy-id -- id-result<CAD-KIND:numeric-policy-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:numeric-policy-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:numeric-policy-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:numeric-policy-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-RANK ( id-result<CAD-KIND:numeric-policy-id> -- n )   \ ok payload's rank, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:numeric-policy-id :} got NUMERIC-POLICY-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   TT-ID TT-MK-OK TT-ARM ;
: TT-RT-OK-RANK ( -- n )                        \ 0 = the registered id came back unchanged
   TT-ID dup NUMERIC-POLICY-ID>RAW {: want:n :}
   TT-MK-OK TT-OK-RANK want = if 0 else 1 then ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-RANK ( -- n ) TT-MK-WW TT-OK-RANK ;     \ a payloadless arm carries no rank

NPOL-DOM:EXACT     TT-RT 0 T=
NPOL-DOM:ULP       TT-RT 0 T=
NPOL-DOM:RELATIVE  TT-RT 0 T=
NPOL-DOM:EMPIRICAL TT-RT 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
NPOL-DOM:EXACT     TT-CKEY-RT 0 T=
NPOL-DOM:EMPIRICAL TT-CKEY-RT 0 T=
TT-CKEY-IS-RANK 0 T=
TT-ID NUMERIC-POLICY-ID>RAW 2 T=                \ the round-trip payload is the non-zero rank 2
TT-RT-OK-ARM 1 T=                               \ ok dispatches to its own arm
TT-RT-OK-RANK 0 T=                              \ and carries its payload through unchanged
TT-RT-WW 2 T=                                   \ wrong-width dispatches to its own arm
TT-RT-UNK 3 T=                                  \ unknown dispatches to its own arm
TT-WW-RANK -1 T=                                \ the no-payload arms of TT-OK-RANK are live

;package

T-REPORT
