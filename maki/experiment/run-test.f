\ maki/experiment/run-test.f - acceptance for the immutable experiment-run identity
\ (maki/experiment/run.f; dot habu-v2-experiment-run-7c1d1906).
\
\ Proves the plan:3300-3317 acceptance, each item by named test words (all id / sum values
\ are produced and consumed INSIDE colon words, never on the interpret-mode stack):
\   SAME-KEY / F-* : ACCEPTANCE - the run digest changes for EVERY semantic field (a
\          per-field flip matrix over seed, rng, dataset, split, preprocess, model,
\          optimizer, numeric, target, compiler, environment, license, authority), while
\          two identical run keys hash equally (the maki/db/diff-suite.f flip precedent).
\   INTERN-* : ACCEPTANCE - two equal builds intern to ONE run-id; a changed field mints a
\          DISTINCT id (equal keys resume the same identity).
\   WIRE-RT : the cross-process content-key round-trips (KEY>WIRE then WIRE>KEY resolves the
\          same id by content); the fail-closed decode taxonomy (wrong-width, unknown).
\   NO-LICENSE / NO-AUTHORITY / COMPLETE-OK : ACCEPTANCE - a run key missing the license or
\          the authority field SEALs `incomplete`; a complete key SEALs `ok`.
\   BATCH-* : ACCEPTANCE - the deterministic next-batch id = f(run-id, k): stable across a
\          rebuild and independent of computation order; distinct per index and per run.
\
\ The test reopens package RUN (a friend) to reach the private CKW width. Identity fixtures
\ mint real ids through their owner constructors (ARTIFACT / NPOL / TARGET / CONFIG) - never
\ a raw cast; names carry the run-test prefix (docs/forth.md § "fixtures use unique
\ test-owned names").

require lib/test.f
require lib/string.f
require maki/experiment/run.f
require maki/artifact.f
require maki/numpolicy.f
require maki/target/target.f
require maki/config.f

package RUN

create DA CKW allot                \ run content key A
create DB CKW allot                \ run content key B
create BK0 CKW allot               \ batch-0 id
create BK1 CKW allot               \ batch-1 id
create BK0B CKW allot              \ batch-0 id, recomputed

\ ---- shared identities (registered once; REGISTER interns by content) ------------
: DS-A ( -- CAD-KIND:artifact-id )     s" run-test/dataset-mnist" ARTIFACT:REGISTER ;
: DS-B ( -- CAD-KIND:artifact-id )     s" run-test/dataset-cifar" ARTIFACT:REGISTER ;
: MODEL-A ( -- CAD-KIND:artifact-id )  s" run-test/model-resnet" ARTIFACT:REGISTER ;
: MODEL-B ( -- CAD-KIND:artifact-id )  s" run-test/model-vit" ARTIFACT:REGISTER ;
: NUM-EXACT ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:EXACT NPOL:REGISTER ;
: NUM-ULP ( -- CAD-KIND:numeric-policy-id )    NPOL-DOM:ULP NPOL:REGISTER ;
: TGT-A ( -- CAD-KIND:target-id )   TARGET:SM87 ;
\ arch=100 matches the diff-suite-test cap-fill descriptor, so TARGET:REGISTER RESOLVES the
\ existing id in the full suite (no new 16-slot registry entry) and mints one only in
\ isolation (the maki/db/diff-suite-test.f T2 precedent).
: TGT-B ( -- CAD-KIND:target-id )
   s" run-test/tgt100"
   TARGET:ISA-PTX 100 32 1024 49152 TARGET:CAP-PTX TARGET:DESCRIPTOR
   TARGET:REGISTER ;
: CFG-A ( -- CAD-KIND:config-id )   s" run-test/compiler-nvcc-12" CONFIG:REGISTER ;
: CFG-B ( -- CAD-KIND:config-id )   s" run-test/compiler-nvcc-13" CONFIG:REGISTER ;
: ENV-A ( -- CAD-KIND:config-id )   s" run-test/env-orin-25w" CONFIG:REGISTER ;
: ENV-B ( -- CAD-KIND:config-id )   s" run-test/env-orin-15w" CONFIG:REGISTER ;

\ descriptor byte strings (hashed to content keys by the run-key setters)
: RNG-A ( -- ptr u8 n )    s" run-test/rng-philox" ;
: RNG-B ( -- ptr u8 n )    s" run-test/rng-pcg" ;
: SPLIT-A ( -- ptr u8 n )  s" run-test/split-80-10-10" ;
: SPLIT-B ( -- ptr u8 n )  s" run-test/split-70-15-15" ;
: PREP-A ( -- ptr u8 n )   s" run-test/prep-standardize" ;
: PREP-B ( -- ptr u8 n )   s" run-test/prep-augment" ;
: OPT-A ( -- ptr u8 n )    s" run-test/opt-adamw-3e-4" ;
: OPT-B ( -- ptr u8 n )    s" run-test/opt-sgd-1e-2" ;
: LIC-A ( -- ptr u8 n )    s" run-test/lic-cc-by" ;
: LIC-B ( -- ptr u8 n )    s" run-test/lic-proprietary" ;
: AUTH-A ( -- ptr u8 n )   s" run-test/auth-lab-grant-7" ;
: AUTH-B ( -- ptr u8 n )   s" run-test/auth-lab-grant-9" ;

\ ---- spec: canonical field values a flip test tweaks one at a time ---------------
TYPED-VARIABLE SP-DATASET  CAD-KIND:artifact-id
TYPED-VARIABLE SP-MODEL    CAD-KIND:artifact-id
TYPED-VARIABLE SP-NUMERIC  CAD-KIND:numeric-policy-id
TYPED-VARIABLE SP-TARGET   CAD-KIND:target-id
TYPED-VARIABLE SP-COMPILER CAD-KIND:config-id
TYPED-VARIABLE SP-ENV      CAD-KIND:config-id
variable SP-SEED
variable SP-RNG-ALT
variable SP-SPLIT-ALT
variable SP-PREP-ALT
variable SP-OPT-ALT
variable SP-LIC-ALT
variable SP-AUTH-ALT

: SPEC-RESET ( -- )
   7 SP-SEED !
   DS-A SP-DATASET !  MODEL-A SP-MODEL !
   NUM-EXACT SP-NUMERIC !  TGT-A SP-TARGET !
   CFG-A SP-COMPILER !  ENV-A SP-ENV !
   false SP-RNG-ALT !  false SP-SPLIT-ALT !  false SP-PREP-ALT !
   false SP-OPT-ALT !  false SP-LIC-ALT !  false SP-AUTH-ALT ! ;

\ populate the pending run key from the spec (no SEAL).
: POPULATE ( -- )
   NEW
   SP-SEED @ SEED!
   SP-RNG-ALT @ if RNG-B else RNG-A then RNG
   SP-DATASET @ DATASET
   SP-SPLIT-ALT @ if SPLIT-B else SPLIT-A then SPLIT
   SP-PREP-ALT @ if PREP-B else PREP-A then PREPROCESS
   SP-MODEL @ MODEL
   SP-OPT-ALT @ if OPT-B else OPT-A then OPTIMIZER
   SP-NUMERIC @ NUMERIC
   SP-TARGET @ TARGET
   SP-COMPILER @ COMPILER
   SP-ENV @ ENVIRONMENT
   SP-LIC-ALT @ if LIC-B else LIC-A then LICENSE
   SP-AUTH-ALT @ if AUTH-B else AUTH-A then AUTHORITY ;

\ SEAL requiring ok; the incomplete arm diverges (a build bug in a happy-path fixture).
: BUILD-ID ( -- CAD-KIND:run-id )
   SEAL MATCH seal-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
   ;MATCH ;
: BUILD-SPEC ( -- CAD-KIND:run-id )   POPULATE BUILD-ID ;

\ SEAL as a code: 0 ok, 1 incomplete.
: SEAL-CODE ( -- n )
   SEAL MATCH seal-result
      ok OF drop 0 ENDOF
      incomplete OF 1 ENDOF
   ;MATCH ;

\ ---- byte helpers ---------------------------------------------------------------
: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;
: KEY= ( ptr u8 ptr u8 -- bool )   CKW MEM= ;
: DIGEST! ( CAD-KIND:run-id ptr u8 -- )   CKW KEY>WIRE drop ;
: ZERO32 ( ptr u8 -- ) {: a:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}   0 a k + c!   1+
   repeat drop ;

\ ---- ACCEPTANCE: per-field digest flip matrix -----------------------------------
: A-DIG ( -- )   SPEC-RESET BUILD-SPEC DA DIGEST! ;
: B-DIG ( -- )   BUILD-SPEC DB DIGEST! ;             \ spec already tweaked
: FLIPS? ( -- bool )   DA DB KEY= 0= ;

: SAME-KEY ( -- bool )    A-DIG SPEC-RESET B-DIG DA DB KEY= ;
: F-SEED ( -- bool )      A-DIG SPEC-RESET 99 SP-SEED ! B-DIG FLIPS? ;
: F-RNG ( -- bool )       A-DIG SPEC-RESET true SP-RNG-ALT ! B-DIG FLIPS? ;
: F-DATASET ( -- bool )   A-DIG SPEC-RESET DS-B SP-DATASET ! B-DIG FLIPS? ;
: F-SPLIT ( -- bool )     A-DIG SPEC-RESET true SP-SPLIT-ALT ! B-DIG FLIPS? ;
: F-PREP ( -- bool )      A-DIG SPEC-RESET true SP-PREP-ALT ! B-DIG FLIPS? ;
: F-MODEL ( -- bool )     A-DIG SPEC-RESET MODEL-B SP-MODEL ! B-DIG FLIPS? ;
: F-OPT ( -- bool )       A-DIG SPEC-RESET true SP-OPT-ALT ! B-DIG FLIPS? ;
: F-NUMERIC ( -- bool )   A-DIG SPEC-RESET NUM-ULP SP-NUMERIC ! B-DIG FLIPS? ;
: F-TARGET ( -- bool )    A-DIG SPEC-RESET TGT-B SP-TARGET ! B-DIG FLIPS? ;
: F-COMPILER ( -- bool )  A-DIG SPEC-RESET CFG-B SP-COMPILER ! B-DIG FLIPS? ;
: F-ENV ( -- bool )       A-DIG SPEC-RESET ENV-B SP-ENV ! B-DIG FLIPS? ;
: F-LICENSE ( -- bool )   A-DIG SPEC-RESET true SP-LIC-ALT ! B-DIG FLIPS? ;
: F-AUTHORITY ( -- bool ) A-DIG SPEC-RESET true SP-AUTH-ALT ! B-DIG FLIPS? ;

\ ---- ACCEPTANCE: intern (equal keys resume one identity) ------------------------
: INTERN-SAME ( -- bool )
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   SPEC-RESET BUILD-SPEC {: b:CAD-KIND:run-id :}
   a b EQUAL? ;
: INTERN-DIFF ( -- bool )
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   SPEC-RESET 99 SP-SEED ! BUILD-SPEC {: b:CAD-KIND:run-id :}
   a b EQUAL? 0= ;

\ ---- cross-process content-key round-trip + fail-closed decode -------------------
: WIRE-RT ( -- bool )
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   a DA CKW KEY>WIRE drop
   DA CKW WIRE>KEY MATCH id-result
      ok OF a EQUAL? ENDOF
      wrong-width OF false ENDOF
      unknown OF false ENDOF
   ;MATCH ;
: WIRE-WRONGWIDTH ( -- bool )       \ a short buffer is a typed wrong-width reject
   DA CKW 1- WIRE>KEY MATCH id-result
      ok OF drop false ENDOF
      wrong-width OF true ENDOF
      unknown OF false ENDOF
   ;MATCH ;
: WIRE-UNKNOWN ( -- bool )          \ an unregistered 32-byte key is a typed unknown reject
   DB ZERO32                        \ all-zero content key: no run interns to it
   DB CKW WIRE>KEY MATCH id-result
      ok OF drop false ENDOF
      wrong-width OF false ENDOF
      unknown OF true ENDOF
   ;MATCH ;

\ ---- ACCEPTANCE: missing license / authority reject typed -----------------------
: POP-NO-LICENSE ( -- )                              \ every field but LICENSE
   NEW
   SP-SEED @ SEED!  RNG-A RNG  SP-DATASET @ DATASET  SPLIT-A SPLIT  PREP-A PREPROCESS
   SP-MODEL @ MODEL  OPT-A OPTIMIZER  SP-NUMERIC @ NUMERIC  SP-TARGET @ TARGET
   SP-COMPILER @ COMPILER  SP-ENV @ ENVIRONMENT  AUTH-A AUTHORITY ;
: POP-NO-AUTHORITY ( -- )                            \ every field but AUTHORITY
   NEW
   SP-SEED @ SEED!  RNG-A RNG  SP-DATASET @ DATASET  SPLIT-A SPLIT  PREP-A PREPROCESS
   SP-MODEL @ MODEL  OPT-A OPTIMIZER  SP-NUMERIC @ NUMERIC  SP-TARGET @ TARGET
   SP-COMPILER @ COMPILER  SP-ENV @ ENVIRONMENT  LIC-A LICENSE ;
: NO-LICENSE ( -- n )    SPEC-RESET POP-NO-LICENSE SEAL-CODE ;
: NO-AUTHORITY ( -- n )  SPEC-RESET POP-NO-AUTHORITY SEAL-CODE ;
: COMPLETE-OK ( -- n )   SPEC-RESET POPULATE SEAL-CODE ;
: SEAL-EMPTY ( -- n )    NEW SEAL-CODE ;

\ ---- ACCEPTANCE: deterministic next-batch identity ------------------------------
: BATCH-REBUILD ( -- bool )        \ batch 0 stable across a rebuild (same interned run)
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   a 0 BK0 BATCH-ID
   SPEC-RESET BUILD-SPEC {: b:CAD-KIND:run-id :}
   b 0 BK0B BATCH-ID
   BK0 BK0B KEY= ;
: BATCH-INDEX-DIFF ( -- bool )     \ batch 0 and batch 1 of one run differ
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   a 0 BK0 BATCH-ID  a 1 BK1 BATCH-ID
   BK0 BK1 KEY= 0= ;
: BATCH-RUN-DIFF ( -- bool )       \ same index, different run -> different batch id
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   a 0 BK0 BATCH-ID
   SPEC-RESET 99 SP-SEED ! BUILD-SPEC {: b:CAD-KIND:run-id :}
   b 0 BK0B BATCH-ID
   BK0 BK0B KEY= 0= ;
: BATCH-ORDER ( -- bool )          \ computing batch 1 between two batch-0 calls is inert
   SPEC-RESET BUILD-SPEC {: a:CAD-KIND:run-id :}
   a 0 BK0 BATCH-ID
   a 1 BK1 BATCH-ID
   a 0 BK0B BATCH-ID
   BK0 BK0B KEY= ;

T-RESET

\ per-field flip matrix: every semantic field flips the run digest
SAME-KEY TTRUE
F-SEED TTRUE
F-RNG TTRUE
F-DATASET TTRUE
F-SPLIT TTRUE
F-PREP TTRUE
F-MODEL TTRUE
F-OPT TTRUE
F-NUMERIC TTRUE
F-TARGET TTRUE
F-COMPILER TTRUE
F-ENV TTRUE
F-LICENSE TTRUE
F-AUTHORITY TTRUE

\ intern: equal keys resume one identity
INTERN-SAME TTRUE
INTERN-DIFF TTRUE

\ cross-process content-key round-trip + fail-closed decode
WIRE-RT TTRUE
WIRE-WRONGWIDTH TTRUE
WIRE-UNKNOWN TTRUE

\ missing license / authority reject typed (incomplete)
SEAL-EMPTY 1 T=
NO-LICENSE 1 T=
NO-AUTHORITY 1 T=
COMPLETE-OK 0 T=

\ deterministic next-batch identity
BATCH-REBUILD TTRUE
BATCH-INDEX-DIFF TTRUE
BATCH-RUN-DIFF TTRUE
BATCH-ORDER TTRUE

T-REPORT

;package
