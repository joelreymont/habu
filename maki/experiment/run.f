\ maki/experiment/run.f - the immutable experiment-run identity (MODEL-CAD-V2-PLAN.md
\ § 23.4 "Experiment, dataset, and model registry", plan:3300-3317; dot
\ habu-v2-experiment-run-7c1d1906).
\
\ CONCERN: the content-addressed owner of CAD-KIND:run-id. A run key BINDS the twelve
\ identity/descriptor facts the plan names ("Every run key includes model, data/split,
\ preprocessing, seed/RNG algorithm, optimizer, numeric policy, target, compiler,
\ runtime, and environment facts"; "Record licenses and authority alongside content")
\ as TYPED fields over the landed § 23.9 identities. SEAL canonically serializes the
\ fields, digests them (SHA-256), and INTERNS the digest: two equal builds yield ONE
\ run-id (content-addressed, the maki/artifact.f REGISTER precedent), so equal keys
\ resume the SAME lineage. The metric-population typing (maki/experiment/run-metric.f)
\ and the per-run lineage log (maki/experiment/run-lineage.f) are SEPARATE concerns that
\ CONSUME this identity; they are not forked here.
\
\ ---- RUN-KEY SCHEMA (field -> representation, grounded in the landed substrate) ------
\ Ascending canonical tags 1..13; every field is digest-covered (a run key's identity IS
\ the full set of facts). Fixed-width fields only, so the canonical form is length-free
\ and byte-identical across processes.
\   seed         u64 scalar (the deterministic run/RNG seed).
\   rng          32-byte SHA-256 content key over the caller's RNG-ALGORITHM descriptor
\                bytes (the "seed/RNG algorithm" pair; the maki/db/diff-suite.f
\                normalization-descriptor precedent - no RNG-algorithm-id owner exists).
\   dataset      CAD-KIND:artifact-id (datasets are immutable content-addressed objects,
\                plan:3302; serialised as ARTIFACT:KEY>WIRE - the nearest landed
\                content-addressed object identity; no dedicated dataset-id owner).
\   split        32-byte content key over the caller's split descriptor (no split-id owner).
\   preprocess   32-byte content key over the caller's preprocessing descriptor.
\   model        CAD-KIND:artifact-id (the model revision; ARTIFACT:KEY>WIRE).
\   optimizer    32-byte content key over the caller's optimizer descriptor (algorithm +
\                hyperparameters; no optimizer-id owner).
\   numeric      CAD-KIND:numeric-policy-id (NPOL:KEY>WIRE, 8B proof-domain rank).
\   target       CAD-KIND:target-id (TARGET:KEY>WIRE, 32B canonical facts).
\   compiler     CAD-KIND:config-id (CONFIG:KEY>WIRE, 32B canonical fact string) - the
\                compiler/toolchain FACTS. CONSERVATIVE READING (flagged): CAD-KIND:toolchain-id
\                is declared but has NO content-key owner yet, so the compiler facts are
\                bound as a config-id (the build/config-fact registry the plan splits
\                compiler+runtime+environment across, plan:3306; the maki/db/promotion-policy.f
\                population->config-id flagged-reading precedent). When a toolchain-id owner
\                with KEY>WIRE lands, `compiler` becomes a projection of it.
\   environment  CAD-KIND:config-id (CONFIG:KEY>WIRE) - the runtime/environment FACTS,
\                a SEPARATE tag from `compiler` even though both read as config-id.
\   license      32-byte content key over the caller's license descriptor. REQUIRED: a
\                run key missing a license SEALs `incomplete` ("missing licenses ... reject
\                before promotion", plan:3316). CONSERVATIVE READING (flagged): the plan
\                under-specifies the license model (no license-id owner), so a license is a
\                content key over its descriptor; a dedicated owner is its eventual projection.
\   authority    32-byte content key over the caller's authority descriptor. REQUIRED,
\                same conservative reading ("Record licenses and authority alongside
\                content", plan:3304): the authorizing principal/grant descriptor.
\
\ ---- IDENTITY (intern) ---------------------------------------------------------------
\ The 32-byte SHA-256 over the canonical field prefix IS the run's cross-process content
\ key. SEAL interns it: an equal digest returns the existing run-id, a fresh digest mints
\ one (RUN-CK content-key store, the maki/artifact.f content-addressed intern). So two
\ equal builds (any process) are ONE run-id; any semantic-field change flips the digest
\ and mints a distinct id. KEY>WIRE / WIRE>KEY are the durable cross-process codec.
\
\ ---- DETERMINISTIC NEXT-BATCH IDENTITY (plan:3297 "reproduce the next batch") ---------
\ BATCH-ID(run,k) = SHA-256(run-content-key || k). A pure function of the run identity and
\ the batch index (the maki/db/diff-suite.f CASE-ID precedent): the same run rederives an
\ identical batch-k id in any order or after a rebuild, and a different run or index differs.
\
\ FIRST-SLICE POOL: a bounded ring of interned run-id slots (the maki/db first-slice
\ convention); a durable run store is a later dot. maki -> habu only; run owns -5616..-5619.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f            \ CAD-KIND:artifact-id KEY>WIRE (dataset + model)
require maki/numpolicy.f           \ CAD-KIND:numeric-policy-id KEY>WIRE (numeric policy)
require maki/target/target.f       \ CAD-KIND:target-id KEY>WIRE (target)
require maki/config.f              \ CAD-KIND:config-id KEY>WIRE (compiler + environment)

-5616 constant E-RUN-CAP           \ run-id registry full (distinct interned run keys exhausted)
-5617 constant E-RUN-BUF           \ KEY>WIRE output buffer / encode scratch smaller than required
-5618 constant E-RUN-WIDE          \ an owner KEY>WIRE returned a width the fixed run-key field cannot hold
-5619 constant E-RUN-ID            \ run-id outside the interned range

package RUN
public

\ WIRE>KEY decode result (the § 23.9 id-result custom-sum idiom): `ok` carries the refined
\ nominal id; the reject arms are the fixed-width content-key refusals (wrong width,
\ unresolved content key). A bespoke per-package sum, not result<a,b>, so a total ok
\ construction leaves no free error variable.
SUMTYPE id-result 1
   VARIANT ok a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;SUMTYPE

\ SEAL outcome (custom-sum result, never value+flag): `ok` carries the interned run-id;
\ `incomplete` rejects a run key missing any required field (including license / authority).
SUMTYPE seal-result 1
   VARIANT ok a ;VARIANT
   VARIANT incomplete ;VARIANT
;SUMTYPE

private

256 constant RUN-CAP               \ max distinct interned run keys (v1 first-slice pool)
32  constant CKW                   \ content-key / digest width (SHA-256 cross-process form)
8   constant U64W                  \ fixed little-endian scalar width
8   constant NPOLW                 \ numeric-policy KEY>WIRE width (8B dom rank)
64  constant FID-CAP               \ scratch cap for one foreign-id wire form
$400 constant SCRATCH-CAP          \ canonical field-prefix scratch (13 fixed fields << this)

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public mint is
\ SEAL, bound to a real interned digest, so a raw n cannot forge a run identity.
TRUSTED: RAW>RUN-ID ( n -- CAD-KIND:run-id ) ;
TRUSTED: RUN-ID>RAW ( CAD-KIND:run-id -- n ) ;

\ ---- canonical ascending field tags (1..13, all digest-covered) -----------------
1  constant TAG-SEED
2  constant TAG-RNG
3  constant TAG-DATASET
4  constant TAG-SPLIT
5  constant TAG-PREP
6  constant TAG-MODEL
7  constant TAG-OPT
8  constant TAG-NUMERIC
9  constant TAG-TARGET
10 constant TAG-COMPILER
11 constant TAG-ENV
12 constant TAG-LICENSE
13 constant TAG-AUTHORITY

\ ---- required-field seen bits (a run key missing one -> incomplete) --------------
1    constant SEEN-SEED
2    constant SEEN-RNG
4    constant SEEN-DATASET
8    constant SEEN-SPLIT
16   constant SEEN-PREP
32   constant SEEN-MODEL
64   constant SEEN-OPT
128  constant SEEN-NUMERIC
256  constant SEEN-TARGET
512  constant SEEN-COMPILER
1024 constant SEEN-ENV
2048 constant SEEN-LICENSE
4096 constant SEEN-AUTHORITY
8191 constant SEEN-ALL              \ all thirteen required fields

\ ---- run-id registry (per-id 32-byte content key = digest) ----------------------
create RUN-CK RUN-CAP CKW * allot   \ interned per-id content keys (the run digests)
variable RUN-N                       \ interned count

\ ---- pending run-key under construction (single builder scratch) ----------------
variable CUR-SEED
create CUR-RNG   CKW allot
create CUR-SPLIT CKW allot
create CUR-PREP  CKW allot
create CUR-OPT   CKW allot
create CUR-LIC   CKW allot
create CUR-AUTH  CKW allot
TYPED-VARIABLE CUR-DATASET  CAD-KIND:artifact-id
TYPED-VARIABLE CUR-MODEL    CAD-KIND:artifact-id
TYPED-VARIABLE CUR-NUMERIC  CAD-KIND:numeric-policy-id
TYPED-VARIABLE CUR-TARGET   CAD-KIND:target-id
TYPED-VARIABLE CUR-COMPILER CAD-KIND:config-id
TYPED-VARIABLE CUR-ENV      CAD-KIND:config-id
variable CUR-SEEN

\ ---- scratch buffers ------------------------------------------------------------
create EBUF SCRATCH-CAP allot
variable EO
create DGBUF CKW allot                          \ recomputed run digest (content key)
create KEYSCRATCH FID-CAP allot                 \ one foreign-id wire form
create CASEBUF CKW U64W + allot                 \ [run-content-key][k] for BATCH-ID

\ ---- fixed little-endian scalar put (byte access) -------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

\ ---- fixed 32-byte content-key comparison (intern lookup) -----------------------
: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK@ ( n -- ptr u8 )   CKW * RUN-CK + ;         \ per-id content-key slot base

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}            \ raw id whose content key equals p, or -1
   RUN-N @ 0 ?do
      i CK@ p CK-EQ? if i unloop exit then
   loop -1 ;

: ID-CK ( CAD-KIND:run-id -- n )                 \ validate + project a run-id to its raw
   RUN-ID>RAW dup 0 < over RUN-N @ >= or if E-RUN-ID throw then ;

\ ---- semantic-prefix emitter (shared by SEAL digest, KEY>WIRE, BATCH-ID) ---------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}   EO @ k + SCRATCH-CAP > if E-RUN-BUF throw then ;
: E-U8 ( n -- ) {: c:n :}     1 E-ROOM  c EBUF EO @ + c!  EO @ 1+ EO ! ;
: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u E-ROOM  a EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;
: E-LE ( n n -- ) {: v:n w:n :}
   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;

\ a fixed-width content-key field: tag byte, then the 32 key bytes.
: E-CK-FIELD ( n ptr u8 -- ) {: tag:n buf:ptr :}   tag E-U8  buf CKW E-BYTES ;

\ a fixed u64 scalar field: tag byte, then the LE64 value.
: E-U64-FIELD ( n n -- ) {: tag:n v:n :}   tag E-U8  v U64W E-LE ;

\ foreign content-addressed id fields: serialise the stored nominal id ACROSS its owner
\ boundary via X:KEY>WIRE (the cross-process content key; total for a valid id), guard the
\ returned width against the fixed field width (fail-closed if an owner widens its key),
\ then frame it.
: E-ART-FIELD ( CAD-KIND:artifact-id n -- ) {: id:CAD-KIND:artifact-id tag:n :}
   id KEYSCRATCH FID-CAP ARTIFACT:KEY>WIRE {: w:n :}
   w CKW <> if E-RUN-WIDE throw then
   tag E-U8  KEYSCRATCH CKW E-BYTES ;
: E-CFG-FIELD ( CAD-KIND:config-id n -- ) {: id:CAD-KIND:config-id tag:n :}
   id KEYSCRATCH FID-CAP CONFIG:KEY>WIRE {: w:n :}
   w CKW <> if E-RUN-WIDE throw then
   tag E-U8  KEYSCRATCH CKW E-BYTES ;
: E-TGT-FIELD ( CAD-KIND:target-id n -- ) {: id:CAD-KIND:target-id tag:n :}
   id KEYSCRATCH FID-CAP TARGET:KEY>WIRE {: w:n :}
   w CKW <> if E-RUN-WIDE throw then
   tag E-U8  KEYSCRATCH CKW E-BYTES ;
: E-NPOL-FIELD ( CAD-KIND:numeric-policy-id n -- ) {: id:CAD-KIND:numeric-policy-id tag:n :}
   id KEYSCRATCH FID-CAP NPOL:KEY>WIRE {: w:n :}
   w NPOLW <> if E-RUN-WIDE throw then
   tag E-U8  KEYSCRATCH NPOLW E-BYTES ;

\ emit the digest-covered semantic fields (tags 1..13) into EBUF, ascending.
: EMIT-SEMANTIC ( -- )
   E-RESET
   TAG-SEED CUR-SEED @ E-U64-FIELD
   TAG-RNG CUR-RNG E-CK-FIELD
   CUR-DATASET @ TAG-DATASET E-ART-FIELD
   TAG-SPLIT CUR-SPLIT E-CK-FIELD
   TAG-PREP CUR-PREP E-CK-FIELD
   CUR-MODEL @ TAG-MODEL E-ART-FIELD
   TAG-OPT CUR-OPT E-CK-FIELD
   CUR-NUMERIC @ TAG-NUMERIC E-NPOL-FIELD
   CUR-TARGET @ TAG-TARGET E-TGT-FIELD
   CUR-COMPILER @ TAG-COMPILER E-CFG-FIELD
   CUR-ENV @ TAG-ENV E-CFG-FIELD
   TAG-LICENSE CUR-LIC E-CK-FIELD
   TAG-AUTHORITY CUR-AUTH E-CK-FIELD ;

: HASH-SEMANTIC ( -- )                           \ EBUF = tags 1..13, DGBUF = 32 digest bytes
   EMIT-SEMANTIC  EBUF EO @ DGBUF SHA256 ;

\ intern the recomputed digest (DGBUF): an equal digest returns the existing run-id, a
\ fresh digest appends a new slot and mints its id.
: INTERN-DIGEST ( -- CAD-KIND:run-id )
   DGBUF CK-FIND {: found:n :}
   found 0 >= if found RAW>RUN-ID exit then
   RUN-N @ RUN-CAP >= if E-RUN-CAP throw then
   RUN-N @ {: raw:n :}
   DGBUF raw CK@ CKW BYTE-COPY
   raw 1+ RUN-N !
   raw RAW>RUN-ID ;

: MARK-SEEN ( n -- )   CUR-SEEN @ or CUR-SEEN ! ;
: COMPLETE? ( -- bool )   CUR-SEEN @ SEEN-ALL and SEEN-ALL = ;

: SR-OK ( CAD-KIND:run-id -- seal-result<CAD-KIND:run-id> )   RUN-SEAL--RESULT:OK ;
: SR-INCOMPLETE ( -- seal-result<CAD-KIND:run-id> )           RUN-SEAL--RESULT:INCOMPLETE ;
: IDR-OK ( a -- id-result<a> )          RUN-ID--RESULT:OK ;
: IDR-WRONG-WIDTH ( -- id-result<a> )   RUN-ID--RESULT:WRONG-WIDTH ;
: IDR-UNKNOWN ( -- id-result<a> )       RUN-ID--RESULT:UNKNOWN ;

public

\ ---- staged run-key builder -----------------------------------------------------
\ NEW opens a fresh pending run key; the typed setters populate the named fields; SEAL
\ validates completeness, digests the canonical prefix, and interns it to the immutable
\ run-id (or rejects `incomplete`). Only one run key is under construction at a time.
: NEW ( -- )   0 CUR-SEEN ! ;

: SEED! ( n -- )                          CUR-SEED !  SEEN-SEED MARK-SEEN ;
: RNG ( ptr u8 n -- ) {: a:ptr u:n :}     a u CUR-RNG SHA256  SEEN-RNG MARK-SEEN ;
: DATASET ( CAD-KIND:artifact-id -- )     CUR-DATASET !  SEEN-DATASET MARK-SEEN ;
: SPLIT ( ptr u8 n -- ) {: a:ptr u:n :}   a u CUR-SPLIT SHA256  SEEN-SPLIT MARK-SEEN ;
: PREPROCESS ( ptr u8 n -- ) {: a:ptr u:n :}  a u CUR-PREP SHA256  SEEN-PREP MARK-SEEN ;
: MODEL ( CAD-KIND:artifact-id -- )       CUR-MODEL !  SEEN-MODEL MARK-SEEN ;
: OPTIMIZER ( ptr u8 n -- ) {: a:ptr u:n :}   a u CUR-OPT SHA256  SEEN-OPT MARK-SEEN ;
: NUMERIC ( CAD-KIND:numeric-policy-id -- )   CUR-NUMERIC !  SEEN-NUMERIC MARK-SEEN ;
: TARGET ( CAD-KIND:target-id -- )        CUR-TARGET !  SEEN-TARGET MARK-SEEN ;
: COMPILER ( CAD-KIND:config-id -- )      CUR-COMPILER !  SEEN-COMPILER MARK-SEEN ;
: ENVIRONMENT ( CAD-KIND:config-id -- )   CUR-ENV !  SEEN-ENV MARK-SEEN ;
: LICENSE ( ptr u8 n -- ) {: a:ptr u:n :}     a u CUR-LIC SHA256  SEEN-LICENSE MARK-SEEN ;
: AUTHORITY ( ptr u8 n -- ) {: a:ptr u:n :}   a u CUR-AUTH SHA256  SEEN-AUTHORITY MARK-SEEN ;

: SEAL ( -- seal-result<CAD-KIND:run-id> )
   COMPLETE? 0= if SR-INCOMPLETE exit then
   HASH-SEMANTIC
   INTERN-DIGEST SR-OK ;

\ ---- identity surface -----------------------------------------------------------
\ EQUAL? is same-run identity: interning makes equal keys share one raw id, so raw
\ equality is exactly run-key equality.
: EQUAL? ( CAD-KIND:run-id CAD-KIND:run-id -- bool )
   {: x:CAD-KIND:run-id y:CAD-KIND:run-id :}
   x RUN-ID>RAW y RUN-ID>RAW = ;

: VALIDATE ( CAD-KIND:run-id -- CAD-KIND:run-id )   dup ID-CK drop ;

\ KEY>WIRE writes the run's 32-byte cross-process content key (its digest) into the caller
\ buffer; total for a valid id. WIRE>KEY resolves 32 bytes against the local registry BY
\ CONTENT (never by intern order), refining to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:run-id ptr u8 n -- n ) {: id:CAD-KIND:run-id out:ptr cap:n :}
   cap CKW < if E-RUN-BUF throw then
   id ID-CK CK@  out  CKW  BYTE-COPY
   CKW ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:run-id> ) {: a:ptr u:n :}
   u CKW <> if IDR-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if IDR-UNKNOWN exit then
   raw RAW>RUN-ID IDR-OK ;

\ ---- deterministic next-batch identity (plan:3297) ------------------------------
\ BATCH-ID(run,k) = SHA-256(run-content-key || k). Since the content key covers every
\ semantic field, the same run rederives an identical batch-k id in any order or after a
\ rebuild, and a different run or index derives a different one.
: BATCH-ID ( CAD-KIND:run-id n ptr u8 -- ) {: id:CAD-KIND:run-id k:n out:ptr :}
   id ID-CK CK@  CASEBUF  CKW BYTE-COPY
   k  CASEBUF CKW +  U64W LE-PUT
   CASEBUF  CKW U64W +  out SHA256 ;

: COUNT ( -- n )   RUN-N @ ;

private

: RUN-INIT ( -- )   0 RUN-N !  0 CUR-SEEN ! ;
RUN-INIT

;package
