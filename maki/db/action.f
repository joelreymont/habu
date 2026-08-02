\ maki/db/action.f - the machine-facing action-schema registry (MODEL-CAD-V2-PLAN.md
\ § 23.9 "Machine-facing action registry", plan:3825-3839; dot
\ habu-v2-machine-action-a7357409).
\
\ CONCERN: the registry of callable protocol actions. Each action is a REGISTERED
\ declaration naming its checked input/output artifact kinds, preconditions, effects,
\ required capabilities, deterministic/cacheable flags, budget dimensions, produced
\ obligations, verifier class, emitted diagnostics, and invalidation rules (the plan's
\ field list). The registry is content-addressed by canonical action NAME (the
\ maki/producer.f intern precedent), enumerates CANONICALLY (name-sorted, registration-
\ order independent), and digests deterministically, so an agent's recorded
\ registrations REPLAY to one enumeration (plan:3855). Package ACTION. One concern per
\ file: the action-schema registry lives here; the transaction VALUE (package TX), the
\ obligation schema (OBLIG), the diagnostic IR (DIAG), and each identity registry stay
\ their own files and are CONSUMED, never forked.
\
\ DISPATCH is the PROTOCOL GATE, not an executor. Given an action-id, the actual input's
\ KIND, and the caller's granted effect/capability authority, it validates kind match,
\ effect + capability authorization, and staged availability, returning a typed outcome.
\ It NEVER invokes the underlying operation (TX:COMMIT etc.) - a thin adapter does that
\ AFTER `accepted` (plan:3831 "adapters are thin codecs over the same transaction
\ protocol"). An action whose implementation is not landed declares STAGING `declared`,
\ and DISPATCH returns `unsupported`; only a landed action declares `implemented` and can
\ reach `accepted` (plan:3830 staged exposure - no faked executable dispatch for
\ unimplemented actions).
\
\ ---- FIELD REPRESENTATIONS (grounded in the landed substrate) --------------------
\ input/output kinds  : art-kind - the closed protocol artifact-CLASS vocabulary this
\                       registry owns. A variant-closed ENUM (DERIVE eq): a MATCH is
\                       exhaustive and no out-of-vocabulary kind is constructible, so a
\                       WRONG-TYPED kind cannot even reach DISPATCH (the cad-kinds verdict
\                       pattern, proven statically by the action-test AD-K-* fixtures and
\                       cad-kinds-test CK-ACTION); a well-typed WRONG-VARIANT kind is the
\                       DYNAMIC wrong-kind reject inside DISPATCH.
\ preconditions / effects / budget-dims / invalidation : ACTION-owned closed ENUMs (the
\                       registry's own concern) held as a u64 BITMASK (1<<ordinal),
\                       canonical by construction (order-independent, duplicate-free).
\ capabilities        : opaque codes from the transaction model, OR'd into a u64 mask.
\                       Every seeded action declares an EMPTY capability set; the field
\                       and authorization mechanism are exercised with abstract bit codes
\                       in the test.
\ obligations (produced) : reuse the LANDED OBLIG:relation vocabulary (the relation an
\                       obligation asserts, maki/db/obligation.f) as a bitmask - no
\                       competing vocabulary.
\ diagnostics         : reuse the LANDED DIAG:class vocabulary (the nine § 23.9 failure
\                       classes, maki/db/diagnostic.f) as a bitmask - no competing
\                       vocabulary.
\ verifier            : reuse the LANDED OBLIG:verifier class (a scalar typed column).
\ deterministic / cacheable : flag bits (plan:3828).
\ staging             : implemented | declared (this registry's own concern).
\
\ ---- AUTHORIZATION (plan:3829 "unauthorized effects reject before execution") -----
\ DISPATCH enforces on BOTH axes: the action's declared EFFECT mask must be a subset of
\ the caller's GRANTED effect mask AND its declared CAPABILITY mask a subset of the
\ granted capability mask, else `unauthorized` - checked BEFORE `accepted`, invoking
\ nothing. Seeded capability masks are empty (see above), so effect authority is the axis
\ the seeded set exercises (TX:COMMIT declares write-store/publish-revision/read-store);
\ the test drives the capability axis with abstract codes.
\
\ FIRST-SLICE POOL: a bounded registry with fixed per-field caps (the maki/db first-slice
\ convention); a durable action store and a cross-process action-id wire codec are later
\ dots. maki -> habu only; action owns -5374..-5379.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f          \ CAD-KIND:action-id nominal identity
require maki/db/obligation.f      \ OBLIG:relation / OBLIG:verifier (reused vocabularies)
require maki/db/diagnostic.f      \ DIAG:class (reused diagnostic-class vocabulary)

-5374 constant E-ACTION-CAP       \ registry count or name-store bytes exhausted
-5375 constant E-ACTION-KEY       \ empty action name or name over the per-name byte cap
-5376 constant E-ACTION-ID        \ action-id outside the registered range (or unknown name)
-5377 constant E-ACTION-BUF       \ DIGEST output buffer smaller than the fixed digest width
-5378 constant E-ACTION-ENC       \ canonical enumeration over the internal digest scratch
-5379 constant E-ACTION-SEED      \ a built-in seed declaration was incomplete/conflicting (load bug)

package ACTION
public

\ ---- input/output artifact-KIND vocabulary (this registry owns it) --------------
\ The closed protocol artifact-CLASS vocabulary. DERIVE eq so DISPATCH compares the
\ supplied kind to the declared kind; a variant-closed ENUM, so a non-kind cannot be
\ passed where a kind is required (static kind safety, the cad-kinds verdict pattern).
ENUM art-kind DERIVE eq
   none schema artifact revision transaction diff pass-obj
;ENUM

\ ---- preconditions (plan:3722-3729 commit preconditions) ------------------------
ENUM precond
   base-fresh reads-recorded schema-valid obligations-discharged budget-available
;ENUM

\ ---- effects: the enforced authority axis (plan:3731 publish, § 12 pass) ---------
ENUM effect-class
   read-store write-store publish-revision run-pass emit-obligation emit-diagnostic
;ENUM

\ ---- budget dimensions (plan:3211 "compute, storage, and wall-time budgets") ----
ENUM budget-dim
   wall-time compute memory storage
;ENUM

\ ---- invalidation rules (plan:3748 "changed dependencies, schema, target, numeric
\ policy, verifier version, or environment invalidate exactly the affected evidence") -
ENUM invalidation
   on-dep-change on-schema-change on-target-change on-config-change
   on-verifier-change on-numeric-change
;ENUM

\ ---- staging: this registry's own concern (plan:3830 staged exposure) -----------
\ implemented: a landed named surface realizes the operation, so DISPATCH may reach
\ `accepted`. declared: the operation is registered but not yet implemented, so DISPATCH
\ returns `unsupported` (no faked executable dispatch).
ENUM staging DERIVE eq
   implemented declared
;ENUM

\ ---- REGISTER outcome (the § 23.9 art-result custom-sum idiom) ------------------
\ ok carries the interned action-id; incomplete rejects a declaration missing a REQUIRED
\ scalar field (the maki/db/diagnostic.f build-result missing-field precedent); conflict
\ rejects re-registering an existing name with a DIFFERENT declaration (an idempotent
\ re-register of an IDENTICAL declaration is ok). A bespoke per-package sum, not
\ result<a,b>, so a total ok construction leaves no free error variable.
\ Declared through the unified ENUM front end in full mode (the arity after the name
\ selects it), so the ok payload is a named FIELD rather than a positional one. It is
\ called `id` because that is what REGISTER puts there: both of its ok paths end in
\ `RAW>ACTION-ID RR-OK` (the resolved idempotent re-register and the freshly interned
\ one), and RR-OK's own effect is ( a -- register-result<a> ), instantiated at
\ CAD-KIND:action-id by REGISTER's signature. The generated ACTION-REGISTER--RESULT:OK
\ / :INCOMPLETE / :CONFLICT constructors and every MATCH site are unchanged, because
\ both spellings and payload binding order derive from the package, the family tail and
\ the declaration order, none of which the mode changes.
ENUM register-result 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT incomplete ;VARIANT
   VARIANT conflict ;VARIANT
;ENUM

\ ---- DISPATCH outcome (the protocol gate; executes nothing) ---------------------
\ accepted: every gate passed and the action is implemented, so a thin adapter may now
\ invoke the underlying operation. The reject arms each return BEFORE `accepted`.
\ Every arm carries no payload, so this is written in the COMPACT enum form (one bare
\ token per case) rather than the arity-headed full form. The type registry therefore
\ records it as an enum family and no longer as a general sum. That kind change is
\ deliberate: both spellings are one cell wide and both give the same MATCH surface, so
\ no consumer - including the cross-package MATCH in maki/db/agent-loop.f - can tell them
\ apart by behaviour, which is exactly why action-test.f pins the recorded kind live out
\ of the family registry. Writing this back as `SUMTYPE dispatch-result 0 ...`, or as the
\ arity-headed full enum form, changes the recorded kind and turns that suite red. The
\ generated ACTION-DISPATCH--RESULT:ACCEPTED / :UNKNOWN-ACTION / :WRONG-KIND /
\ :UNAUTHORIZED / :UNSUPPORTED constructors keep their exact spellings, and case order
\ still fixes the tags.
ENUM dispatch-result
   accepted unknown-action wrong-kind unauthorized unsupported
;ENUM

private

\ ---- readable wrappers over the generated constructor spellings ------------------
: RR-OK ( a -- register-result<a> )        ACTION-REGISTER--RESULT:OK ;
: RR-INCOMPLETE ( -- register-result<a> )  ACTION-REGISTER--RESULT:INCOMPLETE ;
: RR-CONFLICT ( -- register-result<a> )    ACTION-REGISTER--RESULT:CONFLICT ;

: DR-ACCEPTED ( -- dispatch-result )      ACTION-DISPATCH--RESULT:ACCEPTED ;
: DR-UNKNOWN ( -- dispatch-result )       ACTION-DISPATCH--RESULT:UNKNOWN-ACTION ;
: DR-WRONG-KIND ( -- dispatch-result )    ACTION-DISPATCH--RESULT:WRONG-KIND ;
: DR-UNAUTHORIZED ( -- dispatch-result )  ACTION-DISPATCH--RESULT:UNAUTHORIZED ;
: DR-UNSUPPORTED ( -- dispatch-result )   ACTION-DISPATCH--RESULT:UNSUPPORTED ;

\ Private representation refinement of the action identity (owner-only, TRUSTED:, never
\ public) - the maki/producer.f RAW>PRODUCER-ID precedent. REGISTER is the ONLY public
\ authority-bearing mint, bound to a real name registration, so a raw n cannot forge an
\ action identity.
TRUSTED: RAW>ACTION-ID ( n -- CAD-KIND:action-id ) ;
TRUSTED: ACTION-ID>RAW ( CAD-KIND:action-id -- n ) ;

\ ---- capacities + fixed widths --------------------------------------------------
64 constant ACT-CAP                       \ max distinct registered actions (v1)
64 constant ACT-KEY-MAX                    \ max bytes per canonical action name
ACT-CAP ACT-KEY-MAX * constant ACT-KEY-CAP
8 constant U64W                            \ fixed little-endian scalar width
4 constant U32W                            \ fixed little-endian length width
32 constant DIGEST-BYTES                   \ SHA-256 registry digest width
$4000 constant ENC-CAP                     \ canonical enumeration scratch

\ ---- required-scalar seen bits (a declaration missing one -> incomplete) --------
1  constant SEEN-INPUT
2  constant SEEN-OUTPUT
4  constant SEEN-DET
8  constant SEEN-CACHE
16 constant SEEN-VERIFIER
32 constant SEEN-STAGING
63 constant SEEN-ALL                        \ all six required scalars

\ ---- name registry columns (the maki/producer.f intern precedent) ---------------
create ACT-KEYS ACT-KEY-CAP allot           \ interned name bytes, back to back
create ACT-KO   ACT-CAP cells allot           \ per-id name offset into ACT-KEYS
create ACT-KL   ACT-CAP cells allot           \ per-id name length
variable ACT-KEY-U                             \ bytes used in ACT-KEYS
variable ACT-N                                  \ registered count

\ ---- per-action declaration columns (parallel; indexed by the action-id raw) ----
ACT-CAP TYPED-BUFFER D-INPUT    art-kind
ACT-CAP TYPED-BUFFER D-OUTPUT   art-kind
ACT-CAP TYPED-BUFFER D-VERIFIER OBLIG:verifier
ACT-CAP TYPED-BUFFER D-STAGING  staging
create D-DET     ACT-CAP cells allot           \ deterministic flag
create D-CACHE   ACT-CAP cells allot           \ cacheable flag
create D-PRECOND ACT-CAP cells allot           \ precondition bitmask
create D-EFFECT  ACT-CAP cells allot           \ effect bitmask
create D-CAP     ACT-CAP cells allot           \ capability code bitmask
create D-BUDGET  ACT-CAP cells allot           \ budget-dimension bitmask
create D-OBLIG   ACT-CAP cells allot           \ produced-obligation (OBLIG:relation) bitmask
create D-DIAG    ACT-CAP cells allot           \ emitted-diagnostic (DIAG:class) bitmask
create D-INVAL   ACT-CAP cells allot           \ invalidation-rule bitmask

\ ---- builder scratch (one declaration under construction) -----------------------
1 TYPED-BUFFER B-INPUT    art-kind
1 TYPED-BUFFER B-OUTPUT   art-kind
1 TYPED-BUFFER B-VERIFIER OBLIG:verifier
1 TYPED-BUFFER B-STAGING  staging
variable B-DET
variable B-CACHE
variable B-PRECOND
variable B-EFFECT
variable B-CAP
variable B-BUDGET
variable B-OBLIG
variable B-DIAG
variable B-INVAL
variable B-SEEN                                \ which required scalars have been set

\ ---- enumeration + digest scratch ---------------------------------------------
create EBUF ENC-CAP allot
variable EO
create DGBUF DIGEST-BYTES allot
create ORD ACT-CAP cells allot                 \ name-sorted permutation of the action raws

\ ---- fixed little-endian scalar codec -----------------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

\ ---- name interning -----------------------------------------------------------
: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u ACT-KEY-MAX > or if E-ACTION-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}           \ stored name bytes for a validated raw id
   ACT-KEYS raw cells ACT-KO + @ +  raw cells ACT-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered name, or -1
   ACT-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   ACT-KEY-U @ u + ACT-KEY-CAP > if E-ACTION-CAP throw then
   ACT-KEY-U @ {: off:n :}
   a ACT-KEYS off + u BYTE-COPY
   off raw cells ACT-KO + !
   u   raw cells ACT-KL + !
   off u + ACT-KEY-U ! ;

: ID-CK ( CAD-KIND:action-id -- n )            \ refine a nominal id to its validated raw
   ACTION-ID>RAW dup 0 < over ACT-N @ >= or if E-ACTION-ID throw then ;

\ ---- ordinal projections (stable wire codes for the digest) ---------------------
: KIND>N ( art-kind -- n )
   MATCH art-kind
      none        OF 0 ENDOF
      schema      OF 1 ENDOF
      artifact    OF 2 ENDOF
      revision    OF 3 ENDOF
      transaction OF 4 ENDOF
      diff        OF 5 ENDOF
      pass-obj    OF 6 ENDOF
   ;MATCH ;

: STAGING>N ( staging -- n )
   MATCH staging
      implemented OF 0 ENDOF
      declared    OF 1 ENDOF
   ;MATCH ;

: VERIFIER>N ( OBLIG:verifier -- n )           \ my stable projection of the reused class
   MATCH OBLIG:verifier
      static-checker    OF 0 ENDOF
      differential-exec OF 1 ENDOF
      golden-recompute  OF 2 ENDOF
      device-measure    OF 3 ENDOF
      safety-prover     OF 4 ENDOF
      perf-benchmark    OF 5 ENDOF
   ;MATCH ;

\ ---- bit projections for the set-valued builder accumulators --------------------
: PRECOND>BIT ( precond -- n )
   MATCH precond
      base-fresh             OF 1  ENDOF
      reads-recorded         OF 2  ENDOF
      schema-valid           OF 4  ENDOF
      obligations-discharged OF 8  ENDOF
      budget-available       OF 16 ENDOF
   ;MATCH ;

: EFFECT>BIT ( effect-class -- n )
   MATCH effect-class
      read-store       OF 1  ENDOF
      write-store      OF 2  ENDOF
      publish-revision OF 4  ENDOF
      run-pass         OF 8  ENDOF
      emit-obligation  OF 16 ENDOF
      emit-diagnostic  OF 32 ENDOF
   ;MATCH ;

: BUDGET>BIT ( budget-dim -- n )
   MATCH budget-dim
      wall-time OF 1 ENDOF
      compute   OF 2 ENDOF
      memory    OF 4 ENDOF
      storage   OF 8 ENDOF
   ;MATCH ;

: INVAL>BIT ( invalidation -- n )
   MATCH invalidation
      on-dep-change      OF 1  ENDOF
      on-schema-change   OF 2  ENDOF
      on-target-change   OF 4  ENDOF
      on-config-change   OF 8  ENDOF
      on-verifier-change OF 16 ENDOF
      on-numeric-change  OF 32 ENDOF
   ;MATCH ;

: REL>BIT ( OBLIG:relation -- n )              \ reused OBLIG:relation vocabulary
   MATCH OBLIG:relation
      semantic-equiv        OF 1  ENDOF
      representation-refine OF 2  ENDOF
      numeric-approx        OF 4  ENDOF
      target-compat         OF 8  ENDOF
      resource-bound        OF 16 ENDOF
      safety-property       OF 32 ENDOF
   ;MATCH ;

: DIAG>BIT ( DIAG:class -- n )                 \ reused DIAG:class vocabulary
   MATCH DIAG:class
      invariant      OF 1   ENDOF
      unsupported    OF 2   ENDOF
      invalid-input  OF 4   ENDOF
      resource       OF 8   ENDOF
      external       OF 16  ENDOF
      numeric        OF 32  ENDOF
      performance    OF 64  ENDOF
      stale-evidence OF 128 ENDOF
      authorization  OF 256 ENDOF
   ;MATCH ;

\ ---- copy the builder into a fresh interned slot -------------------------------
: STORE-DECL ( n -- ) {: s:n :}
   0 B-INPUT @    s D-INPUT !
   0 B-OUTPUT @   s D-OUTPUT !
   0 B-VERIFIER @ s D-VERIFIER !
   0 B-STAGING @  s D-STAGING !
   B-DET @     s cells D-DET + !
   B-CACHE @   s cells D-CACHE + !
   B-PRECOND @ s cells D-PRECOND + !
   B-EFFECT @  s cells D-EFFECT + !
   B-CAP @     s cells D-CAP + !
   B-BUDGET @  s cells D-BUDGET + !
   B-OBLIG @   s cells D-OBLIG + !
   B-DIAG @    s cells D-DIAG + !
   B-INVAL @   s cells D-INVAL + ! ;

\ Does the pending builder equal the declaration already stored at slot s? (idempotent
\ re-register vs conflict). Scalars compare by their DERIVE-eq family word; masks by =.
: DECL-MATCHES? ( n -- bool ) {: s:n :}
   0 B-INPUT @    s D-INPUT @    ACTION-ART--KIND:EQ
   0 B-OUTPUT @   s D-OUTPUT @   ACTION-ART--KIND:EQ and
   0 B-VERIFIER @ s D-VERIFIER @ OBLIG-VERIFIER:EQ and
   0 B-STAGING @  s D-STAGING @  ACTION-STAGING:EQ and
   B-DET @     s cells D-DET + @     = and
   B-CACHE @   s cells D-CACHE + @   = and
   B-PRECOND @ s cells D-PRECOND + @ = and
   B-EFFECT @  s cells D-EFFECT + @  = and
   B-CAP @     s cells D-CAP + @     = and
   B-BUDGET @  s cells D-BUDGET + @  = and
   B-OBLIG @   s cells D-OBLIG + @   = and
   B-DIAG @    s cells D-DIAG + @    = and
   B-INVAL @   s cells D-INVAL + @   = and ;

public

\ ---- declaration builder --------------------------------------------------------
\ DECLARE begins a fresh action declaration (clearing every field + the seen set). The
\ six required SCALAR setters each record a seen bit; the `+` accumulators build the
\ (empty-by-default) bitmask sets. REGISTER validates completeness against SEEN-ALL.
: DECLARE ( -- )
   0 B-SEEN !
   0 B-DET !  0 B-CACHE !
   0 B-PRECOND !  0 B-EFFECT !  0 B-CAP !  0 B-BUDGET !
   0 B-OBLIG !  0 B-DIAG !  0 B-INVAL ! ;

: INPUT! ( art-kind -- )            0 B-INPUT !     B-SEEN @ SEEN-INPUT or B-SEEN ! ;
: OUTPUT! ( art-kind -- )           0 B-OUTPUT !    B-SEEN @ SEEN-OUTPUT or B-SEEN ! ;
: DETERMINISTIC! ( bool -- )        B-DET !         B-SEEN @ SEEN-DET or B-SEEN ! ;
: CACHEABLE! ( bool -- )            B-CACHE !       B-SEEN @ SEEN-CACHE or B-SEEN ! ;
: VERIFIER! ( OBLIG:verifier -- )   0 B-VERIFIER !  B-SEEN @ SEEN-VERIFIER or B-SEEN ! ;
: STAGING! ( staging -- )           0 B-STAGING !   B-SEEN @ SEEN-STAGING or B-SEEN ! ;

: PRECOND+ ( precond -- )             PRECOND>BIT B-PRECOND @ or B-PRECOND ! ;
: EFFECT+ ( effect-class -- )         EFFECT>BIT B-EFFECT @ or B-EFFECT ! ;
: CAP+ ( n -- )                       B-CAP @ or B-CAP ! ;   \ opaque closed-vocabulary bit code
: BUDGET+ ( budget-dim -- )           BUDGET>BIT B-BUDGET @ or B-BUDGET ! ;
: OBLIGATION+ ( OBLIG:relation -- )   REL>BIT B-OBLIG @ or B-OBLIG ! ;
: DIAGNOSTIC+ ( DIAG:class -- )       DIAG>BIT B-DIAG @ or B-DIAG ! ;
: INVALIDATION+ ( invalidation -- )   INVAL>BIT B-INVAL @ or B-INVAL ! ;

\ REGISTER validates the pending declaration (a missing required scalar -> incomplete),
\ interns the action by its canonical name (an equal name is idempotent when its stored
\ declaration is IDENTICAL, else conflict), stores a fresh declaration, and returns the
\ action-id - the ONLY authority-bearing public mint, bound to a real name registration.
: REGISTER ( ptr u8 n -- register-result<CAD-KIND:action-id> ) {: a:ptr u:n :}
   a u KEY-CK
   B-SEEN @ SEEN-ALL and SEEN-ALL <> if RR-INCOMPLETE exit then
   a u KEY-FIND {: found:n :}
   found 0 >= if
      found DECL-MATCHES? if found RAW>ACTION-ID RR-OK else RR-CONFLICT then
      exit
   then
   ACT-N @ ACT-CAP >= if E-ACTION-CAP throw then
   ACT-N @ {: raw:n :}
   a u raw KEY-PUT
   raw STORE-DECL
   raw 1+ ACT-N !
   raw RAW>ACTION-ID RR-OK ;

\ ---- identity + declaration projections ----------------------------------------
: VALIDATE-ID ( CAD-KIND:action-id -- CAD-KIND:action-id )  dup ID-CK drop ;
: NAME$ ( CAD-KIND:action-id -- ptr u8 n )   ID-CK KEY@ ;
: EQUAL? ( CAD-KIND:action-id CAD-KIND:action-id -- bool )
   {: x:CAD-KIND:action-id y:CAD-KIND:action-id :}
   x ACTION-ID>RAW y ACTION-ID>RAW = ;
: COUNT ( -- n )   ACT-N @ ;

\ ID-OF resolves a REGISTERED action name to its id, fail-closed on an unknown name.
: ID-OF ( ptr u8 n -- CAD-KIND:action-id ) {: a:ptr u:n :}
   a u KEY-FIND {: found:n :}
   found 0 < if E-ACTION-ID throw then
   found RAW>ACTION-ID ;

: INPUT-KIND@ ( CAD-KIND:action-id -- art-kind )      ID-CK D-INPUT @ ;
: OUTPUT-KIND@ ( CAD-KIND:action-id -- art-kind )     ID-CK D-OUTPUT @ ;
: VERIFIER@ ( CAD-KIND:action-id -- OBLIG:verifier )  ID-CK D-VERIFIER @ ;
: STAGING@ ( CAD-KIND:action-id -- staging )          ID-CK D-STAGING @ ;
: DETERMINISTIC@ ( CAD-KIND:action-id -- bool )       ID-CK cells D-DET + @ ;
: CACHEABLE@ ( CAD-KIND:action-id -- bool )           ID-CK cells D-CACHE + @ ;
: PRECOND-MASK@ ( CAD-KIND:action-id -- n )     ID-CK cells D-PRECOND + @ ;
: EFFECT-MASK@ ( CAD-KIND:action-id -- n )      ID-CK cells D-EFFECT + @ ;
: CAP-MASK@ ( CAD-KIND:action-id -- n )         ID-CK cells D-CAP + @ ;
: BUDGET-MASK@ ( CAD-KIND:action-id -- n )      ID-CK cells D-BUDGET + @ ;
: OBLIGATION-MASK@ ( CAD-KIND:action-id -- n )  ID-CK cells D-OBLIG + @ ;
: DIAG-MASK@ ( CAD-KIND:action-id -- n )        ID-CK cells D-DIAG + @ ;
: INVAL-MASK@ ( CAD-KIND:action-id -- n )       ID-CK cells D-INVAL + @ ;

private

\ ---- dispatch authorization ----------------------------------------------------
: SUBSET? ( n n -- bool ) {: need:n granted:n :}
   need granted and need = ;

public

\ DISPATCH is the protocol GATE (never the executor). It resolves the action, checks the
\ supplied input KIND against the declared input kind, authorizes the declared effects AND
\ capabilities against the caller's grants, and gates on staged availability, returning a
\ typed outcome. `accepted` means every gate passed and the action is implemented, so a
\ thin adapter may now invoke the underlying operation; DISPATCH itself invokes nothing.
: DISPATCH ( CAD-KIND:action-id art-kind n n -- dispatch-result )
   {: id:CAD-KIND:action-id in:art-kind granted-eff:n granted-cap:n :}
   id ACTION-ID>RAW {: raw:n :}
   raw 0 < raw ACT-N @ >= or if DR-UNKNOWN exit then
   in id INPUT-KIND@ ACTION-ART--KIND:EQ 0= if DR-WRONG-KIND exit then
   id EFFECT-MASK@ granted-eff SUBSET? 0= if DR-UNAUTHORIZED exit then
   id CAP-MASK@ granted-cap SUBSET? 0= if DR-UNAUTHORIZED exit then
   id STAGING@ ACTION-STAGING:IMPLEMENTED ACTION-STAGING:EQ 0= if DR-UNSUPPORTED exit then
   DR-ACCEPTED ;

private

\ ---- canonical enumeration (name-sorted) ---------------------------------------
variable BLI
: BYTES< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu min {: m:n :}
   0 BLI !
   begin BLI @ m < while
      a BLI @ + c@  b BLI @ + c@  {: ca:n cb:n :}
      ca cb <> if ca cb < exit then
      BLI @ 1+ BLI !
   repeat
   au bu < ;

: NAME< ( n n -- bool ) {: x:n y:n :}          \ raw x's name lexicographically before y's
   x KEY@ y KEY@ BYTES< ;

variable SI variable SJ variable SKEY
: ORD-INIT ( -- )                              \ ORD[k] = k for every registered raw
   0 begin dup ACT-N @ < while
      dup dup cells ORD + !
      1+
   repeat drop ;

: ORD-SORT ( -- )                              \ insertion sort ORD by NAME<
   1 SI !
   begin SI @ ACT-N @ < while
      SI @ cells ORD + @ SKEY !
      SI @ SJ !
      begin SJ @ 0 > if SKEY @ SJ @ 1- cells ORD + @ NAME< else false then while
         SJ @ 1- cells ORD + @  SJ @ cells ORD + !
         SJ @ 1- SJ !
      repeat
      SKEY @ SJ @ cells ORD + !
      SI @ 1+ SI !
   repeat ;

: ORD-BUILD ( -- )   ORD-INIT ORD-SORT ;

\ ---- canonical per-action declaration emit (into EBUF) -------------------------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}   EO @ k + ENC-CAP > if E-ACTION-ENC throw then ;
: E-LE ( n n -- ) {: v:n w:n :}   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;
: E-BYTES ( ptr u8 n -- ) {: p:ptr u:n :}   u E-ROOM  p EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;

: EMIT-ONE ( n -- ) {: s:n :}                  \ slot s's canonical declaration bytes
   s KEY@ {: a:ptr u:n :}
   u U32W E-LE
   a u E-BYTES
   s D-INPUT @ KIND>N U64W E-LE
   s D-OUTPUT @ KIND>N U64W E-LE
   s cells D-DET + @ U64W E-LE
   s cells D-CACHE + @ U64W E-LE
   s cells D-PRECOND + @ U64W E-LE
   s cells D-EFFECT + @ U64W E-LE
   s cells D-CAP + @ U64W E-LE
   s cells D-BUDGET + @ U64W E-LE
   s cells D-OBLIG + @ U64W E-LE
   s cells D-DIAG + @ U64W E-LE
   s cells D-INVAL + @ U64W E-LE
   s D-VERIFIER @ VERIFIER>N U64W E-LE
   s D-STAGING @ STAGING>N U64W E-LE ;

: EMIT-ALL ( -- )                              \ every action in canonical (name-sorted) order
   E-RESET
   ORD-BUILD
   0 begin dup ACT-N @ < while
      dup cells ORD + @ EMIT-ONE
      1+
   repeat drop ;

public

\ ENUM-AT returns the k-th action-id in CANONICAL (name-sorted) enumeration order, so the
\ enumeration is registration-order independent (the replay property, plan:3855).
: ENUM-AT ( n -- CAD-KIND:action-id ) {: k:n :}
   k 0 < k ACT-N @ >= or if E-ACTION-ID throw then   \ reject before ORD-BUILD / pointer math / mint
   ORD-BUILD  k cells ORD + @ RAW>ACTION-ID ;

\ DIGEST writes the 32-byte SHA-256 over the canonical (name-sorted) enumeration of every
\ action's canonical declaration into the caller buffer and returns DIGEST-BYTES. Equal
\ registrations in ANY order digest identically (canonical + replayable).
: DIGEST ( ptr u8 n -- n ) {: out:ptr cap:n :}
   cap DIGEST-BYTES < if E-ACTION-BUF throw then
   EMIT-ALL
   EBUF EO @ out SHA256
   DIGEST-BYTES ;

\ RESET clears the whole registry (a durable store is a later dot; used by replay tests).
: RESET ( -- )   0 ACT-N !  0 ACT-KEY-U ! ;

private

\ ---- seed the plan's action set (plan:3834-3835) --------------------------------
\ Each seed declares HONEST fields reflecting the LANDED surfaces; STAGING is
\ `implemented` iff a landed named surface realizes the operation, else `declared`
\ (DISPATCH then returns `unsupported`). Capability sets are EMPTY (the vocabulary is
\ user-gated / not yet landed; see the header) - effect authority is the enforced axis.
: SEED-REG ( ptr u8 n -- )                     \ register a seed; a load-time failure is a bug
   REGISTER MATCH register-result
      ok         OF drop ENDOF
      incomplete OF E-ACTION-SEED throw ENDOF
      conflict   OF E-ACTION-SEED throw ENDOF
   ;MATCH ;

\ SCHEMA:LIST - enumerate the schema registry (package SCHEMA landed: COUNT/REGISTER).
: SEED-SCHEMA-LIST ( -- )
   DECLARE
   ACTION-ART--KIND:NONE INPUT!
   ACTION-ART--KIND:SCHEMA OUTPUT!
   true DETERMINISTIC!  true CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-INVALIDATION:ON-SCHEMA-CHANGE INVALIDATION+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" SCHEMA:LIST" SEED-REG ;

\ ARTIFACT:GET - fetch an artifact envelope (package ARTIFACT landed: registry + codec).
: SEED-ARTIFACT-GET ( -- )
   DECLARE
   ACTION-ART--KIND:ARTIFACT INPUT!
   ACTION-ART--KIND:ARTIFACT OUTPUT!
   true DETERMINISTIC!  true CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-PRECOND:SCHEMA-VALID PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-INVALIDATION:ON-DEP-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-SCHEMA-CHANGE INVALIDATION+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" ARTIFACT:GET" SEED-REG ;

\ REVISION:DIFF - compare two revisions (no diff word landed -> declared).
: SEED-REVISION-DIFF ( -- )
   DECLARE
   ACTION-ART--KIND:REVISION INPUT!
   ACTION-ART--KIND:DIFF OUTPUT!
   true DETERMINISTIC!  true CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:DECLARED STAGING!
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-BUDGET--DIM:COMPUTE BUDGET+
   ACTION-INVALIDATION:ON-DEP-CHANGE INVALIDATION+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" REVISION:DIFF" SEED-REG ;

\ TX:BEGIN - open a transaction on a base revision (TX:OPEN landed).
: SEED-TX-BEGIN ( -- )
   DECLARE
   ACTION-ART--KIND:REVISION INPUT!
   ACTION-ART--KIND:TRANSACTION OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-PRECOND:BASE-FRESH PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" TX:BEGIN" SEED-REG ;

\ TX:APPLY - accumulate reads/writes/deps into the open transaction (TX builder landed).
: SEED-TX-APPLY ( -- )
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:TRANSACTION OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-PRECOND:READS-RECORDED PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" TX:APPLY" SEED-REG ;

\ TX:VALIDATE - the typed transaction verdict (TX:VALIDATE landed).
: SEED-TX-VALIDATE ( -- )
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:DIFF OUTPUT!
   true DETERMINISTIC!  true CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-PRECOND:BASE-FRESH PRECOND+
   ACTION-PRECOND:READS-RECORDED PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-INVALIDATION:ON-DEP-CHANGE INVALIDATION+
   DIAG-CLASS:INVARIANT DIAGNOSTIC+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" TX:VALIDATE" SEED-REG ;

\ TX:COMMIT - crash-safe publish of a validated transaction (CSTORE:COMMIT landed). The
\ WRITE-STORE + PUBLISH-REVISION effects are the seeded set's authorization exercise.
: SEED-TX-COMMIT ( -- )
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:REVISION OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   ACTION-PRECOND:BASE-FRESH PRECOND+
   ACTION-PRECOND:READS-RECORDED PRECOND+
   ACTION-PRECOND:SCHEMA-VALID PRECOND+
   ACTION-PRECOND:OBLIGATIONS-DISCHARGED PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-EFFECT--CLASS:WRITE-STORE EFFECT+
   ACTION-EFFECT--CLASS:PUBLISH-REVISION EFFECT+
   ACTION-BUDGET--DIM:STORAGE BUDGET+
   DIAG-CLASS:INVARIANT DIAGNOSTIC+
   DIAG-CLASS:AUTHORIZATION DIAGNOSTIC+
   DIAG-CLASS:STALE-EVIDENCE DIAGNOSTIC+
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" TX:COMMIT" SEED-REG ;

\ TX:ABORT - discard the open transaction (no landed named abort surface -> declared).
: SEED-TX-ABORT ( -- )
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:NONE OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:DECLARED STAGING!
   DIAG-CLASS:INVALID-INPUT DIAGNOSTIC+
   s" TX:ABORT" SEED-REG ;

\ PASS:RUN - run a pass over an object (pass engine § 12 not landed -> declared). It
\ produces a semantic-equivalence obligation discharged by an independent differential
\ verifier, and is broadly invalidated - the richest declaration.
: SEED-PASS-RUN ( -- )
   DECLARE
   ACTION-ART--KIND:PASS-OBJ INPUT!
   ACTION-ART--KIND:PASS-OBJ OUTPUT!
   true DETERMINISTIC!  true CACHEABLE!
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC VERIFIER!
   ACTION-STAGING:DECLARED STAGING!
   ACTION-PRECOND:READS-RECORDED PRECOND+
   ACTION-PRECOND:BUDGET-AVAILABLE PRECOND+
   ACTION-EFFECT--CLASS:READ-STORE EFFECT+
   ACTION-EFFECT--CLASS:RUN-PASS EFFECT+
   ACTION-EFFECT--CLASS:EMIT-OBLIGATION EFFECT+
   ACTION-BUDGET--DIM:COMPUTE BUDGET+
   ACTION-BUDGET--DIM:WALL-TIME BUDGET+
   ACTION-BUDGET--DIM:MEMORY BUDGET+
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIGATION+
   DIAG-CLASS:INVARIANT DIAGNOSTIC+
   DIAG-CLASS:UNSUPPORTED DIAGNOSTIC+
   DIAG-CLASS:RESOURCE DIAGNOSTIC+
   DIAG-CLASS:NUMERIC DIAGNOSTIC+
   DIAG-CLASS:PERFORMANCE DIAGNOSTIC+
   ACTION-INVALIDATION:ON-DEP-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-SCHEMA-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-TARGET-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-CONFIG-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-VERIFIER-CHANGE INVALIDATION+
   ACTION-INVALIDATION:ON-NUMERIC-CHANGE INVALIDATION+
   s" PASS:RUN" SEED-REG ;

: SEED-ALL ( -- )
   SEED-SCHEMA-LIST SEED-ARTIFACT-GET SEED-REVISION-DIFF
   SEED-TX-BEGIN SEED-TX-APPLY SEED-TX-VALIDATE SEED-TX-COMMIT SEED-TX-ABORT
   SEED-PASS-RUN ;

: ACTION-INIT ( -- )   RESET SEED-ALL ;
ACTION-INIT

;package
