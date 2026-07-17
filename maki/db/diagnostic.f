\ maki/db/diagnostic.f - the common structured Diagnostic IR (MODEL-CAD-V2-PLAN.md
\ § 23.9 "Structured failure IR" + § 23.2 "Diagnostic and repair contract"; dot
\ habu-v2-structured-diagnostic-18d24536).
\
\ CONCERN: the ONE diagnostic value that every checker, compiler, pass, runtime,
\ benchmark, deployment, and policy failure lowers to, plus its canonical wire
\ codec (ENCODE/DECODE). The two renderers (human text + canonical JSON) that
\ consume this value live in a sibling file, maki/db/diagnostic-render.f (one
\ concern per file: the value + wire here, the presentation there). The renderer
\ file reopens `package DIAG` so it reaches these accessors by bare name.
\
\ SHAPE: a diagnostic is an opaque `diagnostic` handle (a PRODUCT over a pooled
\ slot index, the maki/db/artifact.f weight-artifact precedent). The nine failure
\ CLASSES are a real closed `class` ENUM (variant-closed, exhaustive MATCH, no
\ raw-n) - the plan's "typed variants for invariant violation, unsupported
\ capability, ...". `severity`, `phase`, and `repair` are the same closed-ENUM
\ substrate. Fallible construction and decode return CUSTOM-SUM results
\ (build-result / decode-result), never a value+flag sentinel: the checker forces
\ every caller to MATCH the reject arms.
\
\ VALIDATION (§ 23.2): a diagnostic with no owner or no reproduction command is
\ not a diagnostic. DIAG:BUILD returns a TYPED build-result reject
\ (missing-owner / missing-reproduction), never a throw - a partial failure
\ artifact cannot be constructed.
\
\ IDENTITY FIELDS use the landed nominal ids via their OWNER packages, serialized
\ across the package boundary through each owner's public codec, never a raw cast:
\   owner       -> CAD-KIND:producer-id  (PRODUCER:ID>WIRE / WIRE>ID)
\   environment -> CAD-KIND:config-id    (CONFIG:ID>WIRE / WIRE>ID)
\   subject / counterexample / dependency-cone[] -> CAD-KIND:artifact-id, carried
\                  as their public store KEY$ and re-interned by ARTIFACT:REGISTER
\                  on decode (idempotent: equal keys share one id).
\
\ CONSERVATIVE READINGS (plan under-specifies these field types; grounded in what
\ the repo can express today, flagged per the dot brief):
\   - subject-digest: the plan names a 256-bit digest, but content-digest is a
\     private multi-cell value owned by package ARTIFACT and no shared nominal
\     exposes it. We model `subject` as the failing subject's CAD-KIND:artifact-id
\     (its content-addressed IDENTITY), the strongest identity the repo publishes.
\   - environment-digest: modeled as CAD-KIND:config-id (the environment's
\     configuration identity), the landed identity nearest an env fingerprint.
\   - revision: CAD-KIND:rev-id is a declared nominal with NO owner registry
\     (nothing can mint or serialize it today), so `revision` is a dot/commit-style
\     reference STRING. Capability gap dotted: a rev-id owner registry.
\   - invalidated-evidence[]: CAD-KIND:evidence-id likewise has no owner registry,
\     so each element is an evidence-reference STRING. Same capability-gap dot.
\   - parent-diagnostic: diagnostics are not yet content-addressed artifacts in
\     this slice, so the parent link is the parent's diagnostic `code` (n). Becomes
\     an artifact-id when the diagnostic store lands.
\   - location: a token / node / region locator is heterogeneous; modeled as a
\     free-form STRING (lossless on the wire).
\   - expected-facts[] / observed-facts[]: a "typed fact" has no universal repo
\     value; each fact is stored as its rendered fact STRING, lossless on the wire.
\
\ FIRST-SLICE POOL: a bounded ring of DIAG-CAP live slots with fixed per-field
\ caps, the maki/db/artifact.f first-slice convention. A durable diagnostic store
\ is a later dot.

require lib/prelude.f
require lib/string.f
require lib/adt/option.f
require maki/cad-kinds.f
require maki/artifact.f          \ CAD-KIND:artifact-id owner: REGISTER / KEY$
require maki/producer.f          \ CAD-KIND:producer-id owner: REGISTER / ID>WIRE / WIRE>ID
require maki/config.f            \ CAD-KIND:config-id owner: REGISTER / ID>WIRE / WIRE>ID

\ ---- error codes (diagnostic owns -5354..-5358) -------------------------------
-5354 constant E-DIAG-CAP        \ a diagnostic pool / per-field array capacity exceeded
-5355 constant E-DIAG-BUF        \ ENCODE output buffer smaller than the canonical bytes
-5356 constant E-DIAG-SEM        \ encode scratch overflow (unreachable internal cap)
-5357 constant E-DIAG-ORD        \ an enum ordinal outside its closed domain (fail-closed inverse)
-5358 constant E-DIAG-FIELD      \ a per-field byte string over its fixed slot capacity

package DIAG
public

\ ---- the typed failure classes and attribute substrate ------------------------
\ The nine failure classes of § 23.9, as a closed variant-exhaustive ENUM.
ENUM class DERIVE eq
   invariant unsupported invalid-input resource external
   numeric performance stale-evidence authorization
;ENUM

ENUM severity DERIVE eq
   error warning blocked
;ENUM

\ The pipeline phase that owns the failure (checker .. runtime .. deploy).
ENUM phase DERIVE eq
   checker elaborate solve legalize plan verify codegen build
   evidence promote deploy runtime
;ENUM

\ Legal repair classes (§ 23.2): the typed action category an agent may select.
ENUM repair DERIVE eq
   checker-fix numeric-fix perf-fix resource-fix
   input-fix capability-fix external-fix retry
;ENUM

\ ---- the diagnostic value: an opaque handle over a pooled slot -----------------
PRODUCT diagnostic 0
   FIELD slot n
;PRODUCT

\ ---- fallible construction / decode outcomes (custom-sum results) --------------
SUMTYPE build-result 0
   VARIANT ok diagnostic ;VARIANT
   VARIANT missing-owner ;VARIANT
   VARIANT missing-reproduction ;VARIANT
;SUMTYPE

SUMTYPE decode-result 0
   VARIANT ok diagnostic ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT noncanonical ;VARIANT
   VARIANT bounds ;VARIANT
   VARIANT duplicate ;VARIANT
   VARIANT unknown-required ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over generated constructor spellings --------------------
: >DIAG ( n -- diagnostic )        DIAG-DIAGNOSTIC:MAKE ;
: DIAG> ( diagnostic -- n )        DIAG-DIAGNOSTIC:UNMAKE ;
: B-OK ( n -- build-result )       >DIAG DIAG-BUILD--RESULT:OK ;
: B-NO-OWNER ( -- build-result )   DIAG-BUILD--RESULT:MISSING-OWNER ;
: B-NO-REPRO ( -- build-result )   DIAG-BUILD--RESULT:MISSING-REPRODUCTION ;
: D-OK ( n -- decode-result )      >DIAG DIAG-DECODE--RESULT:OK ;
: D-R-MALFORMED ( -- decode-result )   DIAG-DECODE--RESULT:MALFORMED ;
: D-R-NONCANON ( -- decode-result )    DIAG-DECODE--RESULT:NONCANONICAL ;
: D-R-BOUNDS ( -- decode-result )      DIAG-DECODE--RESULT:BOUNDS ;
: D-R-DUP ( -- decode-result )         DIAG-DECODE--RESULT:DUPLICATE ;
: D-R-UNKNOWN ( -- decode-result )     DIAG-DECODE--RESULT:UNKNOWN-REQUIRED ;

\ ---- ordinal <-> enum bridges (stable wire ids; inverses fail closed) ----------
9  constant CLASS-VARIANTS
3  constant SEV-VARIANTS
12 constant PHASE-VARIANTS
8  constant REPAIR-VARIANTS

: CLASS>N ( class -- n )
   MATCH class
      invariant      OF 0 ENDOF
      unsupported    OF 1 ENDOF
      invalid-input  OF 2 ENDOF
      resource       OF 3 ENDOF
      external       OF 4 ENDOF
      numeric        OF 5 ENDOF
      performance    OF 6 ENDOF
      stale-evidence OF 7 ENDOF
      authorization  OF 8 ENDOF
   ;MATCH ;

: N>CLASS ( n -- class )
   case
      0 of DIAG-CLASS:INVARIANT      endof
      1 of DIAG-CLASS:UNSUPPORTED    endof
      2 of DIAG-CLASS:INVALID-INPUT  endof
      3 of DIAG-CLASS:RESOURCE       endof
      4 of DIAG-CLASS:EXTERNAL       endof
      5 of DIAG-CLASS:NUMERIC        endof
      6 of DIAG-CLASS:PERFORMANCE    endof
      7 of DIAG-CLASS:STALE-EVIDENCE endof
      8 of DIAG-CLASS:AUTHORIZATION  endof
      E-DIAG-ORD throw
   endcase ;

: SEV>N ( severity -- n )
   MATCH severity
      error   OF 0 ENDOF
      warning OF 1 ENDOF
      blocked OF 2 ENDOF
   ;MATCH ;

: N>SEV ( n -- severity )
   case
      0 of DIAG-SEVERITY:ERROR   endof
      1 of DIAG-SEVERITY:WARNING endof
      2 of DIAG-SEVERITY:BLOCKED endof
      E-DIAG-ORD throw
   endcase ;

: PHASE>N ( phase -- n )
   MATCH phase
      checker   OF 0  ENDOF
      elaborate OF 1  ENDOF
      solve     OF 2  ENDOF
      legalize  OF 3  ENDOF
      plan      OF 4  ENDOF
      verify    OF 5  ENDOF
      codegen   OF 6  ENDOF
      build     OF 7  ENDOF
      evidence  OF 8  ENDOF
      promote   OF 9  ENDOF
      deploy    OF 10 ENDOF
      runtime   OF 11 ENDOF
   ;MATCH ;

: N>PHASE ( n -- phase )
   case
      0  of DIAG-PHASE:CHECKER   endof
      1  of DIAG-PHASE:ELABORATE endof
      2  of DIAG-PHASE:SOLVE     endof
      3  of DIAG-PHASE:LEGALIZE  endof
      4  of DIAG-PHASE:PLAN      endof
      5  of DIAG-PHASE:VERIFY    endof
      6  of DIAG-PHASE:CODEGEN   endof
      7  of DIAG-PHASE:BUILD     endof
      8  of DIAG-PHASE:EVIDENCE  endof
      9  of DIAG-PHASE:PROMOTE   endof
      10 of DIAG-PHASE:DEPLOY    endof
      11 of DIAG-PHASE:RUNTIME   endof
      E-DIAG-ORD throw
   endcase ;

: REPAIR>N ( repair -- n )
   MATCH repair
      checker-fix    OF 0 ENDOF
      numeric-fix    OF 1 ENDOF
      perf-fix       OF 2 ENDOF
      resource-fix   OF 3 ENDOF
      input-fix      OF 4 ENDOF
      capability-fix OF 5 ENDOF
      external-fix   OF 6 ENDOF
      retry          OF 7 ENDOF
   ;MATCH ;

: N>REPAIR ( n -- repair )
   case
      0 of DIAG-REPAIR:CHECKER-FIX    endof
      1 of DIAG-REPAIR:NUMERIC-FIX    endof
      2 of DIAG-REPAIR:PERF-FIX       endof
      3 of DIAG-REPAIR:RESOURCE-FIX   endof
      4 of DIAG-REPAIR:INPUT-FIX      endof
      5 of DIAG-REPAIR:CAPABILITY-FIX endof
      6 of DIAG-REPAIR:EXTERNAL-FIX   endof
      7 of DIAG-REPAIR:RETRY          endof
      E-DIAG-ORD throw
   endcase ;

\ ---- pool capacities (first-slice bounded ring) -------------------------------
16  constant DIAG-CAP             \ live diagnostic slots (ring reuse)
3   constant LISTS                \ string-list kinds per slot (expected/observed/invalidated)
0   constant SL-EXP
1   constant SL-OBS
2   constant SL-INVAL
6   constant SL-CAP               \ max items per string list
96  constant SL-MAX               \ max bytes per string-list item
8   constant CONE-CAP             \ dependency-cone artifact ids per slot
8   constant REPAIR-CAP           \ legal-repair classes per slot
200 constant REPRO-MAX            \ reproduction command bytes
96  constant LOC-MAX              \ location string bytes
96  constant REV-MAX              \ revision reference bytes

\ ---- protocol constants (frozen wire) -----------------------------------------
8  constant U64W                 \ fixed little-endian scalar width
4  constant U32W                 \ fixed little-endian length width
2  constant HDR-W                \ tag byte + flags byte
1  constant FLAG-REQUIRED        \ flags bit 0
$1000 constant SCRATCH-CAP       \ semantic/encode scratch bytes
64 constant FID-WIRE-CAP         \ scratch cap for one foreign-id wire form

\ Ascending canonical field tags. 1..12 are always present (required); 13..18 are
\ optional and emitted only when their presence flag is set, still ascending.
1  constant TAG-CODE
2  constant TAG-CLASS
3  constant TAG-SEVERITY
4  constant TAG-PHASE
5  constant TAG-OWNER             \ CAD-KIND:producer-id (PRODUCER:ID>WIRE/WIRE>ID)
6  constant TAG-REPRO             \ reproduction command (string)
7  constant TAG-LOC               \ location (string)
8  constant TAG-EXP               \ expected-facts[]  (string list)
9  constant TAG-OBS               \ observed-facts[]   (string list)
10 constant TAG-INVAL             \ invalidated-evidence[] (string list)
11 constant TAG-CONE              \ dependency-cone[]  (artifact-id KEY$ list)
12 constant TAG-REPAIRS           \ legal-repairs[]    (ordinal list)
13 constant TAG-ENV               \ CAD-KIND:config-id (optional)
14 constant TAG-REV               \ revision reference (optional string)
15 constant TAG-SUBJECT           \ CAD-KIND:artifact-id KEY$ (optional)
16 constant TAG-CX                \ counterexample CAD-KIND:artifact-id KEY$ (optional)
17 constant TAG-PARENT            \ parent-diagnostic code (optional u64)
18 constant TAG-PROGRESS          \ progress-measure (optional u64)
12 constant TAG-REQUIRED-MAX      \ tags 1..12 are required-present
18 constant TAG-KNOWN-MAX         \ highest defined tag

\ taxonomy codes accumulated during decode, mapped to decode-result at the boundary
1 constant D-MALFORMED
2 constant D-NONCANON
3 constant D-BOUNDS
4 constant D-DUP
5 constant D-UNKNOWN-REQ

\ ---- envelope pool: scalar cell columns ---------------------------------------
create P-CODE     DIAG-CAP cells allot
create P-PARENT   DIAG-CAP cells allot
create P-PROGRESS DIAG-CAP cells allot
create P-CONE-N   DIAG-CAP cells allot
create P-REPAIR-N DIAG-CAP cells allot
create P-FLAGS    DIAG-CAP cells allot          \ per-slot optional-field presence bitset
variable P-NEXT                                \ ring cursor
variable CURRENT                               \ slot under construction (DIAG:BEGIN)

\ optional-field presence bits (index into the per-slot P-FLAGS bitset)
0 constant FB-OWNER
1 constant FB-ENV
2 constant FB-REV
3 constant FB-SUBJECT
4 constant FB-CX
5 constant FB-PARENT
6 constant FB-PROGRESS

\ ---- typed columns (checker-native storage; family is preserved) --------------
DIAG-CAP TYPED-BUFFER CLASS-COL   class
DIAG-CAP TYPED-BUFFER SEV-COL     severity
DIAG-CAP TYPED-BUFFER PHASE-COL   phase
DIAG-CAP TYPED-BUFFER OWNER-COL   CAD-KIND:producer-id
DIAG-CAP TYPED-BUFFER ENV-COL     CAD-KIND:config-id
DIAG-CAP TYPED-BUFFER SUBJECT-COL CAD-KIND:artifact-id
DIAG-CAP TYPED-BUFFER CX-COL      CAD-KIND:artifact-id
DIAG-CAP CONE-CAP *   TYPED-BUFFER CONE-COL   CAD-KIND:artifact-id
DIAG-CAP REPAIR-CAP * TYPED-BUFFER REPAIR-COL repair

\ ---- fixed per-slot byte buffers (single strings) -----------------------------
create B-REPRO DIAG-CAP REPRO-MAX * allot
create L-REPRO DIAG-CAP cells allot
create B-LOC   DIAG-CAP LOC-MAX * allot
create L-LOC   DIAG-CAP cells allot
create B-REV   DIAG-CAP REV-MAX * allot
create L-REV   DIAG-CAP cells allot

\ ---- string-list storage (expected / observed / invalidated share one arena) --
create SL-N     DIAG-CAP LISTS * cells allot
create SL-LEN   DIAG-CAP LISTS * SL-CAP * cells allot
create SL-BYTES DIAG-CAP LISTS * SL-CAP * SL-MAX * allot

\ ---- scratch encode buffer + cursor -------------------------------------------
create EBUF SCRATCH-CAP allot
variable EO
create IDWBUF FID-WIRE-CAP allot               \ one foreign-id wire form

\ ---- decode cursor state ------------------------------------------------------
PTR-VARIABLE DBASE
variable DLEN
variable DPOS
variable DPREV
variable DERR
variable SEEN

\ ---- scalar cell column access ------------------------------------------------
: CODE-R@ ( n -- n )      cells P-CODE + @ ;
: CODE-R! ( n n -- )      cells P-CODE + ! ;
: PARENT-R@ ( n -- n ) cells P-PARENT + @ ;
: PARENT-R! ( n n -- ) cells P-PARENT + ! ;
: PROGRESS-R@ ( n -- n ) cells P-PROGRESS + @ ;
: PROGRESS-R! ( n n -- ) cells P-PROGRESS + ! ;
: CONE-N@ ( n -- n )    cells P-CONE-N + @ ;
: CONE-N! ( n n -- )    cells P-CONE-N + ! ;
: REPAIR-N@ ( n -- n )  cells P-REPAIR-N + @ ;
: REPAIR-N! ( n n -- )  cells P-REPAIR-N + ! ;

\ per-slot optional-field presence bitset (bit index + slot)
: FLAG-CELL ( n -- ptr a )    cells P-FLAGS + ;
: FLAG-SET ( n n -- ) {: bit:n s:n :}
   1 bit lshift  s FLAG-CELL dup @ rot or swap ! ;
: FLAG-GET? ( n n -- bool ) {: bit:n s:n :}
   1 bit lshift  s FLAG-CELL @ and 0<> ;
: HAS-OWNER@ ( n -- bool )    FB-OWNER swap FLAG-GET? ;
: HAS-ENV@ ( n -- bool )      FB-ENV swap FLAG-GET? ;
: HAS-REV@ ( n -- bool )      FB-REV swap FLAG-GET? ;
: HAS-SUBJECT@ ( n -- bool )  FB-SUBJECT swap FLAG-GET? ;
: HAS-CX@ ( n -- bool )       FB-CX swap FLAG-GET? ;
: HAS-PARENT@ ( n -- bool )   FB-PARENT swap FLAG-GET? ;
: HAS-PROGRESS@ ( n -- bool ) FB-PROGRESS swap FLAG-GET? ;

\ typed enum / id column access
: CLASS-R@ ( n -- class )                   CLASS-COL @ ;
: CLASS-R! ( class n -- )                   CLASS-COL ! ;
: SEV-R@ ( n -- severity )                  SEV-COL @ ;
: SEV-R! ( severity n -- )                  SEV-COL ! ;
: PHASE-R@ ( n -- phase )                   PHASE-COL @ ;
: PHASE-R! ( phase n -- )                   PHASE-COL ! ;
: OWNER-R@ ( n -- CAD-KIND:producer-id )    OWNER-COL @ ;
: OWNER-R! ( CAD-KIND:producer-id n -- )    OWNER-COL ! ;
: ENV-R@ ( n -- CAD-KIND:config-id )        ENV-COL @ ;
: ENV-R! ( CAD-KIND:config-id n -- )        ENV-COL ! ;
: SUBJECT-R@ ( n -- CAD-KIND:artifact-id )  SUBJECT-COL @ ;
: SUBJECT-R! ( CAD-KIND:artifact-id n -- )  SUBJECT-COL ! ;
: CX-R@ ( n -- CAD-KIND:artifact-id )       CX-COL @ ;
: CX-R! ( CAD-KIND:artifact-id n -- )       CX-COL ! ;

: CONE-AT ( n n -- CAD-KIND:artifact-id ) {: s:n k:n :}   s CONE-CAP * k + CONE-COL @ ;
: CONE-AT! ( CAD-KIND:artifact-id n n -- ) {: v:CAD-KIND:artifact-id s:n k:n :}
   v  s CONE-CAP * k + CONE-COL ! ;
: REPAIR-AT ( n n -- repair ) {: s:n k:n :}   s REPAIR-CAP * k + REPAIR-COL @ ;
: REPAIR-AT! ( repair n n -- ) {: v:repair s:n k:n :}   v  s REPAIR-CAP * k + REPAIR-COL ! ;

\ ---- single-string column store/fetch -----------------------------------------
: REPRO-STORE ( ptr u8 n n -- ) {: a:ptr u:n s:n :}
   u REPRO-MAX > if E-DIAG-FIELD throw then
   a  B-REPRO s REPRO-MAX * +  u BYTE-COPY
   u s cells L-REPRO + ! ;
: REPRO-FETCH ( n -- ptr u8 n ) {: s:n :}
   B-REPRO s REPRO-MAX * +  s cells L-REPRO + @ ;
: LOC-STORE ( ptr u8 n n -- ) {: a:ptr u:n s:n :}
   u LOC-MAX > if E-DIAG-FIELD throw then
   a  B-LOC s LOC-MAX * +  u BYTE-COPY
   u s cells L-LOC + ! ;
: LOC-FETCH ( n -- ptr u8 n ) {: s:n :}
   B-LOC s LOC-MAX * +  s cells L-LOC + @ ;
: REV-STORE ( ptr u8 n n -- ) {: a:ptr u:n s:n :}
   u REV-MAX > if E-DIAG-FIELD throw then
   a  B-REV s REV-MAX * +  u BYTE-COPY
   u s cells L-REV + ! ;
: REV-FETCH ( n -- ptr u8 n ) {: s:n :}
   B-REV s REV-MAX * +  s cells L-REV + @ ;

\ ---- string-list store/fetch (shared arena, indexed by slot/list/item) --------
: SL-IDX ( n n -- n )         LISTS * + ;                     \ ( slot list -- listcell )
: SL-N@ ( n n -- n )          SL-IDX cells SL-N + @ ;
: SL-N! ( n n n -- )          SL-IDX cells SL-N + ! ;         \ ( cnt slot list -- )
: SL-ITEM-IDX ( n n n -- n ) {: s:n list:n k:n :}            \ ( slot list item -- flatidx )
   s list SL-IDX SL-CAP * k + ;
: SL-RESET ( n n -- )         0 -rot SL-N! ;                  \ ( slot list -- )
: SL-ADD ( ptr u8 n n n -- ) {: a:ptr u:n s:n list:n :}       \ ( ptr u8 n slot list -- )
   s list SL-N@ {: k:n :}
   k SL-CAP >= if E-DIAG-CAP throw then
   u SL-MAX > if E-DIAG-FIELD throw then
   s list k SL-ITEM-IDX {: fi:n :}
   a  SL-BYTES fi SL-MAX * +  u BYTE-COPY
   u fi cells SL-LEN + !
   k 1+ s list SL-N! ;
: SL-GET ( n n n -- ptr u8 n ) {: s:n list:n k:n :}           \ ( slot list item -- ptr u8 n )
   s list k SL-ITEM-IDX {: fi:n :}
   SL-BYTES fi SL-MAX * +  fi cells SL-LEN + @ ;

\ ---- slot allocation + reset --------------------------------------------------
: SLOT-ALLOC ( -- n )
   P-NEXT @  dup 1+ DIAG-CAP mod P-NEXT ! ;

: SLOT-RESET ( n -- ) {: s:n :}
   0 s CODE-R!
   0 s PARENT-R!  0 s PROGRESS-R!
   0 s CONE-N!  0 s REPAIR-N!
   0 s cells L-REPRO + !  0 s cells L-LOC + !  0 s cells L-REV + !
   s SL-EXP SL-RESET  s SL-OBS SL-RESET  s SL-INVAL SL-RESET
   0 s FLAG-CELL ! ;

\ ---- fixed little-endian scalar codec -----------------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

\ ---- semantic-prefix emitter (shared by ENCODE) -------------------------------
: E-RESET ( -- )   0 EO ! ;

: E-ROOM ( n -- ) {: k:n :}
   EO @ k + SCRATCH-CAP > if E-DIAG-SEM throw then ;

: E-U8 ( n -- ) {: c:n :}
   1 E-ROOM
   c EBUF EO @ + c!
   EO @ 1+ EO ! ;

: E-LE ( n n -- ) {: v:n w:n :}
   w E-ROOM
   v EBUF EO @ + w LE-PUT
   EO @ w + EO ! ;

: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u E-ROOM
   a EBUF EO @ + u BYTE-COPY
   EO @ u + EO ! ;

: E-HEAD ( n n -- ) {: tag:n flags:n :}
   tag E-U8  flags E-U8 ;

\ backpatched u32 length: reserve a slot, emit the payload, patch its byte count.
: E-LEN-RESERVE ( -- n )
   EO @  0 U32W E-LE ;
: E-LEN-PATCH ( n -- ) {: off:n :}
   EO @ off U32W + -  EBUF off + U32W LE-PUT ;

\ A length-delimited u64 scalar field: tag, required flag, len=8, then the LE64.
: E-U64-FIELD ( n n -- ) {: tag:n v:n :}
   tag FLAG-REQUIRED E-HEAD
   U64W U32W E-LE
   v U64W E-LE ;

\ A length-delimited byte field: tag, required flag, len, then the bytes. Serves
\ raw strings, foreign-id wire forms, and artifact KEY$ payloads alike.
: E-STR-FIELD ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   tag FLAG-REQUIRED E-HEAD
   u U32W E-LE
   a u E-BYTES ;

\ A string-list field: tag, flag, backpatched len, count, then each (u32 len + bytes).
: E-STRLIST-FIELD ( n n n -- ) {: tag:n s:n list:n :}
   tag FLAG-REQUIRED E-HEAD
   E-LEN-RESERVE {: off:n :}
   s list SL-N@ {: cnt:n :}
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s list k SL-GET {: a:ptr u:n :}
      u U32W E-LE  a u E-BYTES
      1+
   repeat drop
   off E-LEN-PATCH ;

\ dependency-cone: each element is its artifact KEY$ string.
: E-CONE-FIELD ( n -- ) {: s:n :}
   TAG-CONE FLAG-REQUIRED E-HEAD
   E-LEN-RESERVE {: off:n :}
   s CONE-N@ {: cnt:n :}
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k CONE-AT ARTIFACT:KEY$ {: a:ptr u:n :}
      u U32W E-LE  a u E-BYTES
      1+
   repeat drop
   off E-LEN-PATCH ;

\ legal-repairs: a fixed-width u64 ordinal list (count word + cnt ordinals).
: E-REPAIRS-FIELD ( n -- ) {: s:n :}
   s REPAIR-N@ {: cnt:n :}
   TAG-REPAIRS FLAG-REQUIRED E-HEAD
   cnt 1+ U64W * U32W E-LE
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k REPAIR-AT REPAIR>N U64W E-LE
      1+
   repeat drop ;

\ foreign-id fields: serialize the stored nominal ACROSS the owner boundary.
: E-OWNER-FIELD ( n -- ) {: s:n :}
   s OWNER-R@ IDWBUF FID-WIRE-CAP PRODUCER:ID>WIRE {: w:n :}
   TAG-OWNER IDWBUF w E-STR-FIELD ;
: E-ENV-FIELD ( n -- ) {: s:n :}
   s ENV-R@ IDWBUF FID-WIRE-CAP CONFIG:ID>WIRE {: w:n :}
   TAG-ENV IDWBUF w E-STR-FIELD ;
: E-SUBJECT-FIELD ( n -- ) {: s:n :}
   s SUBJECT-R@ ARTIFACT:KEY$ {: a:ptr u:n :}
   TAG-SUBJECT a u E-STR-FIELD ;
: E-CX-FIELD ( n -- ) {: s:n :}
   s CX-R@ ARTIFACT:KEY$ {: a:ptr u:n :}
   TAG-CX a u E-STR-FIELD ;

\ ---- optional-field emitters (only when the presence flag is set) --------------
: E-OPT-ENV ( n -- ) {: s:n :}      s HAS-ENV@ if s E-ENV-FIELD then ;
: E-OPT-REV ( n -- ) {: s:n :}      s HAS-REV@ if TAG-REV s REV-FETCH E-STR-FIELD then ;
: E-OPT-SUBJECT ( n -- ) {: s:n :}  s HAS-SUBJECT@ if s E-SUBJECT-FIELD then ;
: E-OPT-CX ( n -- ) {: s:n :}       s HAS-CX@ if s E-CX-FIELD then ;
: E-OPT-PARENT ( n -- ) {: s:n :}   s HAS-PARENT@ if TAG-PARENT s PARENT-R@ E-U64-FIELD then ;
: E-OPT-PROGRESS ( n -- ) {: s:n :} s HAS-PROGRESS@ if TAG-PROGRESS s PROGRESS-R@ E-U64-FIELD then ;

\ ---- full canonical envelope into EBUF ----------------------------------------
: EMIT-REQUIRED ( n -- ) {: s:n :}
   TAG-CODE     s CODE-R@              E-U64-FIELD
   TAG-CLASS    s CLASS-R@ CLASS>N     E-U64-FIELD
   TAG-SEVERITY s SEV-R@ SEV>N         E-U64-FIELD
   TAG-PHASE    s PHASE-R@ PHASE>N     E-U64-FIELD
   s E-OWNER-FIELD
   TAG-REPRO s REPRO-FETCH E-STR-FIELD
   TAG-LOC   s LOC-FETCH   E-STR-FIELD
   TAG-EXP   s SL-EXP   E-STRLIST-FIELD
   TAG-OBS   s SL-OBS   E-STRLIST-FIELD
   TAG-INVAL s SL-INVAL E-STRLIST-FIELD
   s E-CONE-FIELD
   s E-REPAIRS-FIELD ;

: EMIT-ENVELOPE ( n -- ) {: s:n :}
   E-RESET
   s EMIT-REQUIRED
   s E-OPT-ENV  s E-OPT-REV  s E-OPT-SUBJECT  s E-OPT-CX
   s E-OPT-PARENT  s E-OPT-PROGRESS ;

\ ---- decode helpers -----------------------------------------------------------
: FAIL ( n -- )      DERR ! ;
: FAILED? ( -- bool ) DERR @ 0<> ;
: REMAIN ( -- n )    DLEN @ DPOS @ - ;

: D-U8 ( -- n )
   REMAIN 1 < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + c@
   DPOS @ 1+ DPOS ! ;

: D-LE ( n -- n ) {: w:n :}
   REMAIN w < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + w LE-GET
   DPOS @ w + DPOS ! ;

\ take a span of `len` bytes at the cursor (fail-closed on short input).
: D-TAKE ( n -- ptr u8 n ) {: len:n :}
   DBASE 0 ptr-field @ {: base:ptr :}
   REMAIN len < if D-MALFORMED FAIL base 0 exit then
   base DPOS @ + {: sp:ptr :}
   DPOS @ len + DPOS !
   sp len ;

: SEEN-CLEAR ( -- )   0 SEEN ! ;
: SEEN-TAG ( n -- )   1 swap lshift SEEN @ or SEEN ! ;
: SEEN? ( n -- bool ) 1 swap lshift SEEN @ and 0<> ;

\ ---- required-field bodies ----------------------------------------------------
: TAKE-U64 ( n -- n ) {: declen:n :}
   declen U64W <> if D-BOUNDS FAIL 0 exit then
   U64W D-LE ;

: TAKE-ENUM ( n n -- n ) {: declen:n hi:n :}      \ scalar u64 ordinal, range-checked
   declen TAKE-U64 {: ord:n :}
   FAILED? if 0 exit then
   ord 0 < ord hi >= or if D-BOUNDS FAIL 0 exit then
   ord ;

: TAKE-STR ( n n n -- ) {: s:n declen:n which:n :}   \ which: 0=repro 1=loc 2=rev
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   which 0 = if u REPRO-MAX > if D-BOUNDS FAIL exit then a u s REPRO-STORE exit then
   which 1 = if u LOC-MAX  > if D-BOUNDS FAIL exit then a u s LOC-STORE   exit then
   u REV-MAX > if D-BOUNDS FAIL exit then
   a u s REV-STORE  FB-REV s FLAG-SET ;      \ revision is optional: mark present

: TAKE-STRLIST ( n n n -- ) {: s:n declen:n list:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt SL-CAP > or if D-BOUNDS FAIL exit then
   s list SL-RESET
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      U32W D-LE {: ilen:n :}
      ilen SL-MAX > if D-BOUNDS FAIL then
      ilen D-TAKE {: a:ptr u:n :}
      FAILED? 0= if a u s list SL-ADD then
      1+
   repeat drop ;

: TAKE-CONE ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt CONE-CAP > or if D-BOUNDS FAIL exit then
   0 s CONE-N!
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      U32W D-LE {: klen:n :}
      klen D-TAKE {: a:ptr u:n :}
      FAILED? 0= if a u ARTIFACT:REGISTER s k CONE-AT!  k 1+ s CONE-N! then
      1+
   repeat drop ;

: TAKE-REPAIRS ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt REPAIR-CAP > or if D-BOUNDS FAIL exit then
   declen cnt 1+ U64W * <> if D-BOUNDS FAIL exit then
   0 s REPAIR-N!
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      U64W D-LE {: ord:n :}
      ord 0 < ord REPAIR-VARIANTS >= or
      if D-BOUNDS FAIL
      else ord N>REPAIR s k REPAIR-AT!  k 1+ s REPAIR-N! then
      1+
   repeat drop ;

: TAKE-IDWIRE-OWNER ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u PRODUCER:WIRE>ID
   MATCH PRODUCER:id-result
      ok          OF s OWNER-R!  FB-OWNER s FLAG-SET ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-IDWIRE-ENV ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u CONFIG:WIRE>ID
   MATCH CONFIG:id-result
      ok          OF s ENV-R!  FB-ENV s FLAG-SET ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-SUBJECT ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u ARTIFACT:REGISTER s SUBJECT-R!  FB-SUBJECT s FLAG-SET ;

: TAKE-CX ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u ARTIFACT:REGISTER s CX-R!  FB-CX s FLAG-SET ;

: KNOWN-BODY ( n n n -- ) {: s:n tag:n declen:n :}
   tag SEEN? if D-DUP FAIL exit then
   tag SEEN-TAG
   tag TAG-CODE     = if declen TAKE-U64 s CODE-R!  exit then
   tag TAG-CLASS    = if declen CLASS-VARIANTS TAKE-ENUM N>CLASS s CLASS-R!  exit then
   tag TAG-SEVERITY = if declen SEV-VARIANTS TAKE-ENUM N>SEV s SEV-R!  exit then
   tag TAG-PHASE    = if declen PHASE-VARIANTS TAKE-ENUM N>PHASE s PHASE-R!  exit then
   tag TAG-OWNER    = if s declen TAKE-IDWIRE-OWNER  exit then
   tag TAG-REPRO    = if s declen 0 TAKE-STR  exit then
   tag TAG-LOC      = if s declen 1 TAKE-STR  exit then
   tag TAG-EXP      = if s declen SL-EXP TAKE-STRLIST  exit then
   tag TAG-OBS      = if s declen SL-OBS TAKE-STRLIST  exit then
   tag TAG-INVAL    = if s declen SL-INVAL TAKE-STRLIST  exit then
   tag TAG-CONE     = if s declen TAKE-CONE  exit then
   tag TAG-REPAIRS  = if s declen TAKE-REPAIRS  exit then
   tag TAG-ENV      = if s declen TAKE-IDWIRE-ENV  exit then
   tag TAG-REV      = if s declen 2 TAKE-STR  exit then
   tag TAG-SUBJECT  = if s declen TAKE-SUBJECT  exit then
   tag TAG-CX       = if s declen TAKE-CX  exit then
   tag TAG-PARENT   = if declen TAKE-U64 s PARENT-R!  FB-PARENT s FLAG-SET  exit then
   tag TAG-PROGRESS = if declen TAKE-U64 s PROGRESS-R!  FB-PROGRESS s FLAG-SET  exit then ;

: FIELD-STEP ( n -- ) {: s:n :}
   D-U8 {: tag:n :}    FAILED? if exit then
   D-U8 {: flags:n :}  FAILED? if exit then
   U32W D-LE {: declen:n :}  FAILED? if exit then
   declen 0 < if D-BOUNDS FAIL exit then
   tag DPREV @ = if D-DUP FAIL exit then
   tag DPREV @ < if D-NONCANON FAIL exit then
   tag DPREV !
   tag TAG-KNOWN-MAX <= tag 0 > and if
      s tag declen KNOWN-BODY exit
   then
   flags FLAG-REQUIRED and 0<> if
      REMAIN declen < if D-MALFORMED FAIL exit then
      D-UNKNOWN-REQ FAIL exit
   then
   declen D-TAKE 2drop ;              \ skip an unknown OPTIONAL field

: REQUIRED-COMPLETE? ( -- bool )
   true
   TAG-CODE SEEN? and TAG-CLASS SEEN? and TAG-SEVERITY SEEN? and
   TAG-PHASE SEEN? and TAG-OWNER SEEN? and TAG-REPRO SEEN? and
   TAG-LOC SEEN? and TAG-EXP SEEN? and TAG-OBS SEEN? and
   TAG-INVAL SEEN? and TAG-CONE SEEN? and TAG-REPAIRS SEEN? and ;

: DEC-SETUP ( ptr u8 n -- ) {: a:ptr u:n :}
   a DBASE 0 ptr-field !  u DLEN !  0 DPOS !  -1 DPREV !  0 DERR !
   SEEN-CLEAR ;

: DEC-SLOT-FILL ( n -- ) {: s:n :}
   s SLOT-RESET
   begin REMAIN 0 > FAILED? 0= and while
      s FIELD-STEP
   repeat ;

: DEC-RESULT ( n -- decode-result ) {: s:n :}
   DERR @ 0= if s D-OK exit then
   DERR @ D-MALFORMED   = if D-R-MALFORMED exit then
   DERR @ D-NONCANON    = if D-R-NONCANON exit then
   DERR @ D-BOUNDS      = if D-R-BOUNDS exit then
   DERR @ D-DUP         = if D-R-DUP exit then
   D-R-UNKNOWN ;

public

\ ---- staged builder -----------------------------------------------------------
\ DIAG:BEGIN opens a fresh diagnostic; the setters populate the required core,
\ optional identities, and the bounded lists; DIAG:BUILD validates and seals it.
: NEW ( -- )                           SLOT-ALLOC dup CURRENT ! SLOT-RESET ;
: CODE ( n -- )                        CURRENT @ CODE-R! ;
: CLASS ( class -- )                   CURRENT @ CLASS-R! ;
: SEVERITY ( severity -- )             CURRENT @ SEV-R! ;
: PHASE ( phase -- )                   CURRENT @ PHASE-R! ;
: OWNER ( CAD-KIND:producer-id -- ) {: id:CAD-KIND:producer-id :}
   CURRENT @ {: s:n :}   id s OWNER-R!  FB-OWNER s FLAG-SET ;
: ENVIRONMENT ( CAD-KIND:config-id -- ) {: id:CAD-KIND:config-id :}
   CURRENT @ {: s:n :}   id s ENV-R!  FB-ENV s FLAG-SET ;
: REPRODUCTION ( ptr u8 n -- )         CURRENT @ REPRO-STORE ;
: LOCATION ( ptr u8 n -- )             CURRENT @ LOC-STORE ;
: REVISION ( ptr u8 n -- ) {: a:ptr u:n :}
   CURRENT @ {: s:n :}   a u s REV-STORE  FB-REV s FLAG-SET ;
: SUBJECT ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   CURRENT @ {: s:n :}   id s SUBJECT-R!  FB-SUBJECT s FLAG-SET ;
: COUNTEREXAMPLE ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   CURRENT @ {: s:n :}   id s CX-R!  FB-CX s FLAG-SET ;
: PARENT ( n -- ) {: v:n :}
   CURRENT @ {: s:n :}   v s PARENT-R!  FB-PARENT s FLAG-SET ;
: PROGRESS ( n -- ) {: v:n :}
   CURRENT @ {: s:n :}   v s PROGRESS-R!  FB-PROGRESS s FLAG-SET ;
: EXPECTED+ ( ptr u8 n -- )            CURRENT @ SL-EXP SL-ADD ;
: OBSERVED+ ( ptr u8 n -- )            CURRENT @ SL-OBS SL-ADD ;
: INVALIDATED+ ( ptr u8 n -- )         CURRENT @ SL-INVAL SL-ADD ;
: CONE+ ( CAD-KIND:artifact-id -- )
   CURRENT @ {: s:n :}
   s CONE-N@ {: k:n :}
   k CONE-CAP >= if E-DIAG-CAP throw then
   s k CONE-AT!  k 1+ s CONE-N! ;
: REPAIR+ ( repair -- )
   CURRENT @ {: s:n :}
   s REPAIR-N@ {: k:n :}
   k REPAIR-CAP >= if E-DIAG-CAP throw then
   s k REPAIR-AT!  k 1+ s REPAIR-N! ;

\ DIAG:BUILD - the ONLY constructor. A missing owner or an empty reproduction
\ command is rejected with a typed build-result, never a throw (§ 23.2).
: BUILD ( -- build-result )
   CURRENT @ {: s:n :}
   s HAS-OWNER@ 0= if B-NO-OWNER exit then
   s REPRO-FETCH nip 0= if B-NO-REPRO exit then
   s B-OK ;

\ ---- typed field accessors (the value read surface) ---------------------------
: CODE@ ( diagnostic -- n )                    DIAG> CODE-R@ ;
: CLASS@ ( diagnostic -- class )               DIAG> CLASS-R@ ;
: SEVERITY@ ( diagnostic -- severity )         DIAG> SEV-R@ ;
: PHASE@ ( diagnostic -- phase )               DIAG> PHASE-R@ ;
: OWNER@ ( diagnostic -- CAD-KIND:producer-id ) DIAG> OWNER-R@ ;
: REPRODUCTION@ ( diagnostic -- ptr u8 n )     DIAG> REPRO-FETCH ;
: LOCATION@ ( diagnostic -- ptr u8 n )         DIAG> LOC-FETCH ;

: HAS-ENVIRONMENT? ( diagnostic -- bool )      DIAG> HAS-ENV@ ;
: ENVIRONMENT@ ( diagnostic -- CAD-KIND:config-id ) DIAG> ENV-R@ ;
: HAS-REVISION? ( diagnostic -- bool )         DIAG> HAS-REV@ ;
: REVISION@ ( diagnostic -- ptr u8 n )         DIAG> REV-FETCH ;
: HAS-SUBJECT? ( diagnostic -- bool )          DIAG> HAS-SUBJECT@ ;
: SUBJECT@ ( diagnostic -- CAD-KIND:artifact-id ) DIAG> SUBJECT-R@ ;
: HAS-COUNTEREXAMPLE? ( diagnostic -- bool )   DIAG> HAS-CX@ ;
: COUNTEREXAMPLE@ ( diagnostic -- CAD-KIND:artifact-id ) DIAG> CX-R@ ;
: HAS-PARENT? ( diagnostic -- bool )           DIAG> HAS-PARENT@ ;
: PARENT@ ( diagnostic -- n )                  DIAG> PARENT-R@ ;
: HAS-PROGRESS? ( diagnostic -- bool )         DIAG> HAS-PROGRESS@ ;
: PROGRESS@ ( diagnostic -- n )                DIAG> PROGRESS-R@ ;

: EXPECTED-COUNT ( diagnostic -- n )           DIAG> SL-EXP SL-N@ ;
: EXPECTED@ ( diagnostic n -- ptr u8 n ) {: d:diagnostic k:n :}   d DIAG> SL-EXP k SL-GET ;
: OBSERVED-COUNT ( diagnostic -- n )           DIAG> SL-OBS SL-N@ ;
: OBSERVED@ ( diagnostic n -- ptr u8 n ) {: d:diagnostic k:n :}   d DIAG> SL-OBS k SL-GET ;
: INVALIDATED-COUNT ( diagnostic -- n )        DIAG> SL-INVAL SL-N@ ;
: INVALIDATED@ ( diagnostic n -- ptr u8 n ) {: d:diagnostic k:n :}   d DIAG> SL-INVAL k SL-GET ;
: CONE-COUNT ( diagnostic -- n )               DIAG> CONE-N@ ;
: CONE@ ( diagnostic n -- CAD-KIND:artifact-id ) {: d:diagnostic k:n :}   d DIAG> k CONE-AT ;
: REPAIR-COUNT ( diagnostic -- n )             DIAG> REPAIR-N@ ;
: REPAIR@ ( diagnostic n -- repair ) {: d:diagnostic k:n :}   d DIAG> k REPAIR-AT ;

\ ---- enum name projections (single source of truth for both renderers) --------
: CLASS-NAME ( class -- ptr u8 n )
   MATCH class
      invariant      OF s" invariant"      ENDOF
      unsupported    OF s" unsupported"     ENDOF
      invalid-input  OF s" invalid-input"   ENDOF
      resource       OF s" resource"        ENDOF
      external       OF s" external"        ENDOF
      numeric        OF s" numeric"         ENDOF
      performance    OF s" performance"     ENDOF
      stale-evidence OF s" stale-evidence"  ENDOF
      authorization  OF s" authorization"   ENDOF
   ;MATCH ;

: SEVERITY-NAME ( severity -- ptr u8 n )
   MATCH severity
      error   OF s" error"   ENDOF
      warning OF s" warning" ENDOF
      blocked OF s" blocked" ENDOF
   ;MATCH ;

: PHASE-NAME ( phase -- ptr u8 n )
   MATCH phase
      checker   OF s" checker"   ENDOF
      elaborate OF s" elaborate" ENDOF
      solve     OF s" solve"     ENDOF
      legalize  OF s" legalize"  ENDOF
      plan      OF s" plan"      ENDOF
      verify    OF s" verify"    ENDOF
      codegen   OF s" codegen"   ENDOF
      build     OF s" build"     ENDOF
      evidence  OF s" evidence"  ENDOF
      promote   OF s" promote"   ENDOF
      deploy    OF s" deploy"    ENDOF
      runtime   OF s" runtime"   ENDOF
   ;MATCH ;

: REPAIR-NAME ( repair -- ptr u8 n )
   MATCH repair
      checker-fix    OF s" checker-fix"    ENDOF
      numeric-fix    OF s" numeric-fix"    ENDOF
      perf-fix       OF s" perf-fix"       ENDOF
      resource-fix   OF s" resource-fix"   ENDOF
      input-fix      OF s" input-fix"      ENDOF
      capability-fix OF s" capability-fix" ENDOF
      external-fix   OF s" external-fix"   ENDOF
      retry          OF s" retry"          ENDOF
   ;MATCH ;

\ ---- canonical wire codec -----------------------------------------------------
: ENCODE ( diagnostic ptr u8 n -- n ) {: d:diagnostic out:ptr cap:n :}
   d DIAG> EMIT-ENVELOPE
   EO @ cap > if E-DIAG-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

: DECODE ( ptr u8 n -- decode-result ) {: a:ptr u:n :}
   a u DEC-SETUP
   SLOT-ALLOC {: s:n :}
   s DEC-SLOT-FILL
   FAILED? 0= REQUIRED-COMPLETE? 0= and if D-MALFORMED FAIL then
   s DEC-RESULT ;

private

: DIAG-CODEC-INIT ( -- )
   0 P-NEXT !  0 CURRENT ! ;

DIAG-CODEC-INIT
;package
