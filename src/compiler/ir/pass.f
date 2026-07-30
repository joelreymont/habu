\ pass.f - the pass result and its witness header: the one authority that says
\ what a pass has to prove about its own output before anything may consume it.
\
\ docs/compiler-ir-design.md section 6.7 fixes both halves. A pass returns a
\ pass-result of "output-module, witness, metrics", every witness begins with
\ "magic, format-version, pass-id, pass-version, input-module-digest,
\ output-module-digest, target-contract-digest, numeric-policy-digest,
\ schema-digest, payload-length, payload-digest", and "a witness is rejected
\ before its pass-specific payload is read if any binding differs". Section 5.8
\ adds the consequence: a definition, object, kernel or candidate "is not visible
\ until all required witnesses pass".
\
\ WHAT A PASS RESULT IS, AND WHAT IT IS NOT. It is not a record a producer fills
\ in and a consumer believes. A witness is a CLAIM; this file re-derives every
\ bound fact from the artifacts themselves - the bound context, the two frozen
\ modules and their canonical tables, the payload bytes, the metrics bytes - and
\ compares. Nothing a producer writes is evidence of itself. That is why the
\ package that validates is not the package that ran the pass: section 5.8 says
\ the artifact is not visible until its witness passes, so the authority that
\ decides has to sit outside the stage that wants to publish. A pass calls
\ WITNESS to state its result and hands the bytes on; its consumer calls VALIDATE
\ and gets, or does not get, the sealed handle that publication needs.
\
\ THE MODULE QUESTION SECTION 6.7 ANSWERS. A pass stage consumes and produces
\ MODULES, not byte streams. Section 5.1 states the shape - "It consumes a frozen
\ module and builds a new module: PASS(input module, configuration) -> output
\ module, witness, metrics" - and section 6.7 repeats it in the result itself,
\ whose first component is "output-module". Bytes appear in one place only: the
\ witness binds the two modules by their canonical frame DIGESTS, which is how a
\ module's identity is stated, not what a pass is handed. So the staging words
\ below take (module, canonical table) pairs and re-encode them, and a decoder
\ that produced only a validated byte stream could not be presented to this stage
\ at all - it would have to rebuild a module first. That settles the open design
\ question of the decoder leaf: decoding is a module replay through IR-BUILD,
\ freeze and verify, not a stream validator.
\
\ THE ONE RESOURCE THIS FILE OWNS. A validated pass result is a resource, because
\ it is the right to publish one module exactly once. It lives in an owned
\ registry with a committed ceiling of SLOT-MAX live results per context, keyed
\ by a nonzero, monotonic, never-reused generation serial, exactly as IR-CTX,
\ IR-ARENA, IR-BUILD and IR-CANON do it. Everything else this file touches is
\ borrowed: the modules and canonical tables belong to the caller, the witness,
\ payload and metrics bytes belong to the caller, and the scratch span the module
\ frames are re-derived in belongs to the caller too. So validating a pass can
\ never be the thing that exhausts a context, and this file holds no memory of
\ its own beyond one staging area and its registry.
\
\ EXACTLY ONCE, AND WHAT EACH OUTCOME COSTS. A result handle is minted by one
\ word and consumed by exactly one of two: ACCEPT publishes the output module and
\ retires the handle, RELEASE discards the result and retires the handle. A
\ second use of either is refused by the name of the consumption that already
\ happened - E-IR-PASS-CONSUMED after ACCEPT, E-IR-PASS-RELEASED after RELEASE -
\ so a caller never learns "invalid handle" when the truth is "you already
\ published this". A refused VALIDATE mints nothing: the slot is taken and the
\ generation installed after the last comparison agreed, so a rejection costs no
\ registry slot and leaves nothing half-installed. test/compiler/ir-pass.f
\ measures that directly by filling the registry, releasing one slot, running a
\ rejected validation, and requiring the next valid one to still find the slot.
\
\ WHY REJECTION CANNOT BE LATE. Section 6.7 requires the refusal to happen before
\ the pass-specific payload is read, and here that is structural rather than
\ ordered: nothing in this file interprets a payload byte. The payload is only
\ ever measured - its length and its SHA-256 - and interpreting it needs
\ PAYLOAD-CK, which needs a live result handle, which VALIDATE mints last. So a
\ rejected witness leaves the caller with no handle and therefore no checked way
\ to read the payload it described. The cheap refusals still run first anyway:
\ framing, then the pass identity, then the binding, then the modules, then the
\ schema, then the payload and metrics measurements, then the mint.
\
\ ONE WRITER AND ONE READER OF ONE LAYOUT. WITNESS writes the header the staged
\ facts imply and VALIDATE compares a presented header against the same derived
\ facts, both through DERIVE. A writer and a validator that computed their fields
\ separately could drift; deriving once means the two can only ever disagree
\ about a presented header, never about what a field means.
\
\ FIELD WIDTH AND FIELD ORDER. Every witness field is one eight-byte
\ little-endian slot, CDIGEST's preimage slot, the same convention the canonical
\ wire frame uses, so a witness slot and a frame slot can never drift apart. The
\ first eleven fields are section 6.7's list in section 6.7's order, with
\ format-version and pass-version each read as a major and a minor slot the way
\ IR-ENCODE reads a frame's format version, and each digest occupying four slots
\ deepest word first. Section 6.7 says a witness BEGINS with that list, so the
\ metrics length and metrics digest follow it: the pass-result's third component
\ is metrics, and a metrics record no binding covered could be swapped for
\ another without the witness noticing.
\
\ WHY THE SCHEMA DIGEST IS THE OUTPUT MODULE'S. A canonical frame cannot carry a
\ schema-table digest at all - IR-SCHEMA's digest folds each record's stored
\ operand, result and attribute-key lists, and those hold module-local insertion
\ ordinals, so two equivalent modules built along two intern orders have two
\ different schema digests, which src/compiler/ir/encode.f explains and
\ test/compiler/ir-encode.f measures. A frame therefore binds only the dialect's
\ canonical name ordinal and its schema major and minor version. A witness binds
\ one pass over one module in one process, so it may use that non-canonical
\ digest, and it binds the OUTPUT module's: the input module's schema generation
\ already rides along inside the input frame digest, while the module being
\ published is the one whose operation vocabulary a consumer has to pin exactly.
\
\ WHY ONE FILE. The header layout, the validation, and the result registry are
\ one responsibility - "the pass witness and what makes it valid" - and they have
\ one consumer between them. A witness is not an independent format with other
\ readers the way the canonical frame is, its fields ARE the validation's fields,
\ and the resource the validation mints is the thing the validation exists to
\ mint. IR-CANON keeps its table registry beside canonicalization and IR-BUILD
\ keeps its builder registry beside the builder for the same reason: the registry
\ belongs with the stage that mints the resource.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/ir/context.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f
require src/compiler/ir/encode.f

package IR-PASS
public

\ The right to publish one pass's output module exactly once. Minted only by
\ VALIDATE, consumed only by ACCEPT or RELEASE.
NEWTYPE result 0

private

CAST: MINT-R ( n -- IR-PASS:result ) ;
CAST: R>N ( IR-PASS:result -- n ) ;

\ ---- the format --------------------------------------------------------------
\ "HBWT" read as four little-endian ASCII bytes: H at byte zero. Four ASCII bytes
\ rather than a small integer, so a witness's leading slot can never collide with
\ a CDIGEST domain-separation tag, and distinct from the canonical frame's magic
\ so a frame presented as a witness is refused on its first slot.
$54574248 constant MAGIC
1 constant FMT-MAJOR
0 constant FMT-MINOR

\ ---- the witness header's slots ----------------------------------------------
\ Section 6.7's field list in section 6.7's order, then the metrics binding.
\ Every digest takes four slots, deepest word first, which is the order CDIGEST
\ reads them out in.
0 constant WS-MAGIC
1 constant WS-MAJOR                  \ witness format major version
2 constant WS-MINOR                  \ witness format minor version
3 constant WS-PASS                   \ the pass name's digest: which pass
7 constant WS-PMAJOR                 \ the pass's own major version
8 constant WS-PMINOR                 \ the pass's own minor version
9 constant WS-INPUT                  \ the input module's canonical frame digest
13 constant WS-OUTPUT                \ the output module's canonical frame digest
17 constant WS-TARGET                \ the bound target contract's digest
21 constant WS-POLICY                \ the bound numeric policy's digest
25 constant WS-SCHEMA                \ the output module's schema-table digest
29 constant WS-PAYLEN                \ payload bytes
30 constant WS-PAYDIG                \ the payload's digest
34 constant WS-METLEN                \ metrics bytes
35 constant WS-METDIG                \ the metrics record's digest
39 constant WIT-SLOTS

WIT-SLOTS CDIGEST:SLOT-BYTES * constant WIT-BYTES

\ ---- the derived facts -------------------------------------------------------
\ One digest slot per bound digest field, filled by DERIVE from the artifacts and
\ read by both the writer and the validator, so the two cannot disagree about
\ what a field means.
0 constant DG-PASS
1 constant DG-INPUT
2 constant DG-OUTPUT
3 constant DG-TARGET
4 constant DG-POLICY
5 constant DG-SCHEMA
6 constant DG-PAYLOAD
7 constant DG-METRICS
8 constant DG-STAGE#

\ ---- the committed ceiling ---------------------------------------------------
\ Live validated results per context. A pipeline holds the result it is about to
\ publish and the ones it has not decided about yet; a ninth is a named refusal
\ rather than a silent allocation, the same ceiling and the same reason as
\ IR-CANON's canonical tables.
8 constant SLOT-MAX

\ Every digest this file stores: the derived facts, then two per registry slot -
\ the payload digest and the metrics digest a validated result carries.
DG-STAGE# SLOT-MAX 2 * + constant DG#

: DG-PAY-OF ( n -- n )
   2 * DG-STAGE# + ;

: DG-MET-OF ( n -- n )
   2 * DG-STAGE# + 1+ ;

\ ---- storage -----------------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable PGEN-CELL
0 PGEN-CELL !
create PGENS SLOT-MAX cells allot
create POWNERS SLOT-MAX cells allot
create PSTATES SLOT-MAX cells allot
create PPAYLEN SLOT-MAX cells allot
create PMETLEN SLOT-MAX cells allot
create DGS DG# 4 * cells allot
SLOT-MAX TYPED-BUFFER PMODS IR-BUILD:module

$7FFFFFFF constant PGEN-MAX
1 constant ST-LIVE
2 constant ST-ACCEPTED
3 constant ST-RELEASED

\ ---- the staging area --------------------------------------------------------
\ One package-owned area under the single-task compilation discipline, the same
\ shape IR-TYPE's function-type stage and IR-CTX's staging window use. Each field
\ is declared once by name; a field declared twice and a begin while one is open
\ are both E-IR-PASS-STAGE, and a check run before every field arrived is
\ E-IR-PASS-FIELD.
1 constant F-CTX
2 constant F-PASS
4 constant F-VERSION
8 constant F-INPUT
16 constant F-OUTPUT
32 constant F-PAYLOAD
64 constant F-METRICS
128 constant F-SCRATCH
255 constant F-ALL

variable STG-OPEN
0 STG-OPEN !
variable STG-HAVE
0 STG-HAVE !
variable STG-PMAJOR
variable STG-PMINOR
variable STG-PAYLEN
variable STG-METLEN
variable STG-SCRLEN
create STG-SCR 1 cells allot
1 TYPED-BUFFER STG-CTX IR-CTX:ctx
1 TYPED-BUFFER STG-IN-M IR-BUILD:module
1 TYPED-BUFFER STG-IN-T IR-CANON:table
1 TYPED-BUFFER STG-OUT-M IR-BUILD:module
1 TYPED-BUFFER STG-OUT-T IR-CANON:table

: SCR-FIELD ( -- ptr ptr u8 )
   STG-SCR 0 ptr-field ;

: SCR@ ( -- ptr u8 )
   SCR-FIELD @ ;

: SCR! ( ptr u8 -- )
   SCR-FIELD ! ;

: STG-OPEN-CK ( -- )
   STG-OPEN @ 0= if E-IR-PASS-STAGE throw then ;

\ Declaring one field: the stage must be open and the field must be new.
: FIELD-TAKE ( n -- )
   {: bit:n :}
   STG-OPEN-CK
   STG-HAVE @ bit and 0 <> if E-IR-PASS-STAGE throw then
   STG-HAVE @ bit or STG-HAVE ! ;

\ Closing the stage. A rejected check consumes the stage just as a rejected
\ IR-TYPE end does, so no half-staged result leaks into the next validation; the
\ staged cells stay readable until the next CHECK-BEGIN clears them.
: STAGE-TAKE ( -- )
   STG-OPEN-CK
   STG-HAVE @ F-ALL <> if 0 STG-OPEN ! E-IR-PASS-FIELD throw then
   0 STG-OPEN ! ;

\ ---- one digest, four cells --------------------------------------------------
: DG-AT ( n n -- n )
   {: k:n w:n :}
   k 4 * w + cells DGS + @ ;

: DG-TO ( n n n -- )
   {: v:n k:n w:n :}
   v  k 4 * w + cells DGS +  ! ;

: DG@ ( n -- CDIGEST:digest )
   {: k:n :}
   k 0 DG-AT  k 1 DG-AT  k 2 DG-AT  k 3 DG-AT  CDIGEST-DIGEST:MAKE ;

: DG! ( n CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: k:n w0:n w1:n w2:n w3:n :}
   w0 k 0 DG-TO
   w1 k 1 DG-TO
   w2 k 2 DG-TO
   w3 k 3 DG-TO ;

\ ---- one digest, four header slots -------------------------------------------
: PUT-DIGEST ( ptr u8 n CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: p at:n w0:n w1:n w2:n w3:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   w0 p at CDIGEST:SLOT!
   w1 p at 1+ CDIGEST:SLOT!
   w2 p at 2 + CDIGEST:SLOT!
   w3 p at 3 + CDIGEST:SLOT! ;

: GET-DIGEST ( ptr u8 n -- CDIGEST:digest )
   {: p at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p at CDIGEST:SLOT@
   p at 1+ CDIGEST:SLOT@
   p at 2 + CDIGEST:SLOT@
   p at 3 + CDIGEST:SLOT@
   CDIGEST-DIGEST:MAKE ;

public

\ ---- staging the facts a witness is about -----------------------------------
\ What the consumer knows independently of anything the producer wrote: the
\ context it compiled under, the pass and version it asked for, the module it
\ handed in, the module it got back, the payload and metrics bytes it was given,
\ and a scratch span the two module frames may be re-derived in.
: CHECK-BEGIN ( -- )
   STG-OPEN @ 0 <> if E-IR-PASS-STAGE throw then
   1 STG-OPEN !
   0 STG-HAVE ! ;

: CHECK-CTX ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   F-CTX FIELD-TAKE
   c 0 STG-CTX ! ;

\ The pass's identity is its name's digest rather than a small integer, because
\ there is no pass registry to allocate integers and a name digest binds the
\ identity without one.
: CHECK-PASS ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   F-PASS FIELD-TAKE
   DG-PASS  p n CDIGEST:COMPUTE  DG! ;

: CHECK-VERSION ( n n -- )
   {: major:n minor:n :}
   F-VERSION FIELD-TAKE
   major STG-PMAJOR !
   minor STG-PMINOR ! ;

: CHECK-INPUT ( IR-BUILD:module IR-CANON:table -- )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   F-INPUT FIELD-TAKE
   m IR-BUILD:FROZEN? 0= if E-IR-PASS-STALE throw then
   m 0 STG-IN-M !
   t 0 STG-IN-T ! ;

: CHECK-OUTPUT ( IR-BUILD:module IR-CANON:table -- )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   F-OUTPUT FIELD-TAKE
   m IR-BUILD:FROZEN? 0= if E-IR-PASS-STALE throw then
   m 0 STG-OUT-M !
   t 0 STG-OUT-T ! ;

\ The payload and the metrics record are measured here and never read again: this
\ file learns their length and their SHA-256 and nothing else about them.
: CHECK-PAYLOAD ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   F-PAYLOAD FIELD-TAKE
   n STG-PAYLEN !
   DG-PAYLOAD  p n CDIGEST:COMPUTE  DG! ;

: CHECK-METRICS ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   F-METRICS FIELD-TAKE
   n STG-METLEN !
   DG-METRICS  p n CDIGEST:COMPUTE  DG! ;

\ Borrowed room, not owned room: the two module frames are re-derived here and
\ nothing is retained. A span too short for a frame is IR-ENCODE:ENCODE's own
\ E-IR-ENCODE-ROOM, which is the same fact under its own owner's name.
: CHECK-SCRATCH ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   F-SCRATCH FIELD-TAKE
   p SCR!
   n STG-SCRLEN ! ;

private

\ ---- deriving the bound facts from the artifacts -----------------------------
: CTX@ ( -- IR-CTX:ctx )
   0 STG-CTX @ ;

: TARGET-DIG ( IR-CTX:ctx -- CDIGEST:digest )
   IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:DIGEST ;

: POLICY-DIG ( IR-CTX:ctx -- CDIGEST:digest )
   IR-CTX:BINDING@ CBIND:POLICY@ CNUM:DIGEST ;

: SCHEMA-DIG ( IR-BUILD:module -- CDIGEST:digest )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL  m IR-BUILD:FSCHEMA-ROWS  IR-SCHEMA:FTABLE-DIGEST ;

\ One module's identity: its canonical frame, written into the borrowed scratch
\ and digested. Presenting a canonical table another module owns leaves as
\ IR-CANON's own E-IR-CANON-OWNER inside ENCODE, before a byte is written.
: FRAME-DIG ( IR-BUILD:module IR-CANON:table -- CDIGEST:digest )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   CTX@ m t  SCR@ STG-SCRLEN @  IR-ENCODE:ENCODE {: wrote:n :}
   SCR@ wrote IR-ENCODE:DIGEST ;

\ Every bound fact, computed from the artifacts alone. The writer states these
\ and the validator compares against these, so a field means one thing.
: DERIVE ( -- )
   DG-TARGET  CTX@ TARGET-DIG  DG!
   DG-POLICY  CTX@ POLICY-DIG  DG!
   DG-SCHEMA  0 STG-OUT-M @ SCHEMA-DIG  DG!
   DG-INPUT   0 STG-IN-M @  0 STG-IN-T @  FRAME-DIG  DG!
   DG-OUTPUT  0 STG-OUT-M @ 0 STG-OUT-T @ FRAME-DIG  DG! ;

\ ---- writing a witness -------------------------------------------------------
: PUT-FORMAT ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   MAGIC p WS-MAGIC CDIGEST:SLOT!
   FMT-MAJOR p WS-MAJOR CDIGEST:SLOT!
   FMT-MINOR p WS-MINOR CDIGEST:SLOT! ;

: PUT-PASS ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-PASS DG-PASS DG@ PUT-DIGEST
   STG-PMAJOR @ p WS-PMAJOR CDIGEST:SLOT!
   STG-PMINOR @ p WS-PMINOR CDIGEST:SLOT! ;

: PUT-MODULES ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-INPUT DG-INPUT DG@ PUT-DIGEST
   p WS-OUTPUT DG-OUTPUT DG@ PUT-DIGEST ;

: PUT-BINDING ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-TARGET DG-TARGET DG@ PUT-DIGEST
   p WS-POLICY DG-POLICY DG@ PUT-DIGEST
   p WS-SCHEMA DG-SCHEMA DG@ PUT-DIGEST ;

: PUT-SPANS ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STG-PAYLEN @ p WS-PAYLEN CDIGEST:SLOT!
   p WS-PAYDIG DG-PAYLOAD DG@ PUT-DIGEST
   STG-METLEN @ p WS-METLEN CDIGEST:SLOT!
   p WS-METDIG DG-METRICS DG@ PUT-DIGEST ;

\ ---- checking a presented witness --------------------------------------------
\ Bytes too short to hold a header, or a leading slot that is not the magic, are
\ not one of our witnesses at all, so nothing else about them is believed.
: MAGIC-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   n WIT-BYTES < if E-IR-PASS-STATE throw then
   p WS-MAGIC CDIGEST:SLOT@ MAGIC <> if E-IR-PASS-STATE throw then ;

\ A different major version is a different format. A higher minor version states
\ header content this reader was not written to see, so it is refused rather than
\ read as far as it is understood.
: VERSION-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-MAJOR CDIGEST:SLOT@ FMT-MAJOR <> if E-IR-PASS-VERSION throw then
   p WS-MINOR CDIGEST:SLOT@ {: minor:n :}
   minor 0 < minor FMT-MINOR > or if E-IR-PASS-VERSION throw then ;

\ A witness is a fixed number of slots, so a span with anything after them is not
\ one witness.
: LENGTH-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   n WIT-BYTES <> if E-IR-PASS-FRAME throw then ;

: FRAME-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n MAGIC-CK
   p VERSION-CK
   p n LENGTH-CK ;

\ One presented digest field against the fact derived from the artifacts.
: DIG-SAME? ( ptr u8 n n -- bool )
   {: p at:n k:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p at GET-DIGEST  k DG@  CDIGEST-DIGEST:EQ ;

\ One comparison per bound field, so a dropped comparison is a visible line and
\ each failure carries its own name.
: IDENT-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-PASS DG-PASS DIG-SAME? 0= if E-IR-PASS-PASS throw then
   p WS-PMAJOR CDIGEST:SLOT@ STG-PMAJOR @ <> if E-IR-PASS-PASS throw then
   p WS-PMINOR CDIGEST:SLOT@ STG-PMINOR @ <> if E-IR-PASS-PASS throw then ;

: BINDING-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-TARGET DG-TARGET DIG-SAME? 0= if E-IR-PASS-TARGET throw then
   p WS-POLICY DG-POLICY DIG-SAME? 0= if E-IR-PASS-POLICY throw then ;

: MODULES-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-INPUT DG-INPUT DIG-SAME? 0= if E-IR-PASS-INPUT throw then
   p WS-OUTPUT DG-OUTPUT DIG-SAME? 0= if E-IR-PASS-OUTPUT throw then ;

: SCHEMA-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-SCHEMA DG-SCHEMA DIG-SAME? 0= if E-IR-PASS-SCHEMA throw then ;

: PAYLOAD-HDR-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-PAYLEN CDIGEST:SLOT@ STG-PAYLEN @ <> if E-IR-PASS-PAYLOAD throw then
   p WS-PAYDIG DG-PAYLOAD DIG-SAME? 0= if E-IR-PASS-PAYLOAD throw then ;

: METRICS-HDR-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-METLEN CDIGEST:SLOT@ STG-METLEN @ <> if E-IR-PASS-METRICS throw then
   p WS-METDIG DG-METRICS DIG-SAME? 0= if E-IR-PASS-METRICS throw then ;

\ ---- the registry ------------------------------------------------------------
: PGEN@ ( n -- n )
   cells PGENS + @ ;

: PGEN! ( n n -- )
   cells PGENS + ! ;

: POWNER@ ( n -- n )
   cells POWNERS + @ ;

: POWNER! ( n n -- )
   cells POWNERS + ! ;

: PSTATE@ ( n -- n )
   cells PSTATES + @ ;

: PSTATE! ( n n -- )
   cells PSTATES + ! ;

: SLOTS-CLEAR ( -- )
   SLOT-MAX 0 ?do
      0 i PGEN!
   loop ;
SLOTS-CLEAR

: PGEN-NEXT-N ( n -- n )
   dup 0 < over PGEN-MAX >= or if E-IR-PASS-SERIALS throw then
   1+ ;

: TRY-PGEN ( -- n bool )
   PGEN-CELL atomic@ {: current:n :}
   current PGEN-NEXT-N {: next:n :}
   current next PGEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-PGEN ( -- n )
   begin
      TRY-PGEN dup 0=
   while
      2drop
   repeat
   drop ;

: FIND-P ( n -- n )
   {: g:n :}
   -1
   SLOT-MAX 0 ?do
      g i PGEN@ = if drop i leave then
   loop ;

\ Retire every slot whose owning context has torn down: the module it names died
\ with that context and its generation can never resolve again.
: SWEEP ( -- )
   SLOT-MAX 0 ?do
      i PGEN@ 0 <> if
         i POWNER@ IR-CTX:SERIAL-LIVE? 0= if
            0 i PGEN!
         then
      then
   loop ;

: FREE-SLOT ( -- n )
   -1
   SLOT-MAX 0 ?do
      i PGEN@ 0= if drop i leave then
   loop
   dup 0 < if E-IR-PASS-SLOTS throw then ;

\ Resolve a handle to its slot. A generation this registry never minted and a
\ slot whose owning context has torn down are both stale; a consumed slot keeps
\ its generation, so it can still say which consumption already happened.
: LIVE-SLOT ( IR-PASS:result -- n )
   R>N FIND-P
   dup 0 < if E-IR-PASS-STALE throw then
   dup POWNER@ IR-CTX:SERIAL-LIVE? 0= if
      0 over PGEN! E-IR-PASS-STALE throw
   then
   dup PSTATE@ ST-ACCEPTED = if E-IR-PASS-CONSUMED throw then
   dup PSTATE@ ST-RELEASED = if E-IR-PASS-RELEASED throw then
   dup PSTATE@ ST-LIVE <> if E-IR-PASS-STATE throw then ;

\ The mint. It runs only after the last comparison agreed, so a rejected witness
\ costs no slot and installs nothing; the generation goes in last, so a failure
\ part way through leaves no half-installed slot either.
: MINT ( -- IR-PASS:result )
   SWEEP
   FREE-SLOT {: slot:n :}
   TAKE-PGEN {: g:n :}
   0 STG-OUT-M @ slot PMODS !
   STG-PAYLEN @ slot cells PPAYLEN + !
   STG-METLEN @ slot cells PMETLEN + !
   slot DG-PAY-OF  DG-PAYLOAD DG@  DG!
   slot DG-MET-OF  DG-METRICS DG@  DG!
   CTX@ IR-CTX:SERIAL slot POWNER!
   ST-LIVE slot PSTATE!
   g slot PGEN!
   g MINT-R ;

public

\ ---- the size of one witness -------------------------------------------------
\ Fixed, because every field is fixed width and there is no variable part: a
\ caller sizes its span before it asks for the bytes.
: WITNESS-BYTES ( -- n )
   WIT-BYTES ;

\ ---- writing the witness -----------------------------------------------------
\ What a pass calls to state its result. It consumes the stage, derives every
\ bound fact from the artifacts, and writes them; the destination is a byte span
\ the caller already owns, exactly as a canonical frame is written.
: WITNESS ( ptr u8 n -- n )
   {: p room:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STAGE-TAKE
   room WIT-BYTES < if E-IR-PASS-ROOM throw then
   DERIVE
   p PUT-FORMAT
   p PUT-PASS
   p PUT-MODULES
   p PUT-BINDING
   p PUT-SPANS
   WIT-BYTES ;

\ ---- validating a presented witness ------------------------------------------
\ What a consumer calls before it publishes anything. Every binding is compared
\ against a fact derived from the artifacts themselves, in header order, cheapest
\ refusal first; the sealed handle is minted last, so a rejection publishes
\ nothing and hands back no capability to read the payload it described.
: VALIDATE ( ptr u8 n -- IR-PASS:result )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STAGE-TAKE
   p n FRAME-CK
   DERIVE
   p IDENT-CK
   p BINDING-CK
   p MODULES-CK
   p SCHEMA-CK
   p PAYLOAD-HDR-CK
   p METRICS-HDR-CK
   MINT ;

\ ---- consuming the result exactly once ---------------------------------------
\ Publish the output module and retire the handle. This is the only way to obtain
\ a module from a pass result, so a checked consumer cannot publish one whose
\ witness did not pass.
: ACCEPT ( IR-PASS:result -- IR-BUILD:module )
   LIVE-SLOT {: slot:n :}
   slot PMODS @ {: m:IR-BUILD:module :}
   ST-ACCEPTED slot PSTATE!
   m ;

\ Give the result up without publishing it, and retire the handle. The slot
\ records that it was released, so a later use is named rather than merely
\ refused.
: RELEASE ( IR-PASS:result -- )
   LIVE-SLOT {: slot:n :}
   ST-RELEASED slot PSTATE! ;

: LIVE? ( IR-PASS:result -- bool )
   R>N FIND-P {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot POWNER@ IR-CTX:SERIAL-LIVE?
   slot PSTATE@ ST-LIVE = and ;

\ ---- reading what the result bound -------------------------------------------
\ The gate a consumer puts in front of interpreting the payload: the presented
\ span must be the payload the validated witness bound, by length and by digest,
\ and the handle must still be live. Nothing here reads inside the span beyond
\ its SHA-256. The length is compared first and it is not a second soundness
\ check - the digest is taken over exactly the presented span, so any span of
\ another length already fails the digest unless SHA-256 collides. It is there so
\ that a span the caller mis-sized is refused without hashing bytes the validator
\ was never asked to read, which is the same bounded-work discipline the
\ canonical frame's counts-before-payload rule keeps.
: PAYLOAD-CK ( IR-PASS:result ptr u8 n -- )
   {: r:IR-PASS:result p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   r LIVE-SLOT {: slot:n :}
   n  slot cells PPAYLEN + @  <> if E-IR-PASS-PAYLOAD throw then
   p n CDIGEST:COMPUTE  slot DG-PAY-OF DG@  CDIGEST-DIGEST:EQ
   0= if E-IR-PASS-PAYLOAD throw then ;

: METRICS-CK ( IR-PASS:result ptr u8 n -- )
   {: r:IR-PASS:result p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   r LIVE-SLOT {: slot:n :}
   n  slot cells PMETLEN + @  <> if E-IR-PASS-METRICS throw then
   p n CDIGEST:COMPUTE  slot DG-MET-OF DG@  CDIGEST-DIGEST:EQ
   0= if E-IR-PASS-METRICS throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
