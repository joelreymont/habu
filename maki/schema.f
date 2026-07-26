\ maki/schema.f - the schema-definition identity registry + wire codec (the
\ schema-id leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors
\ and wire codecs" per-family contract; dot habu-schema-schema-id-3a6827e9).
\
\ CONCERN: the content-addressed owner of CAD-KIND:schema-id. REGISTER interns a
\ canonical schema NAME into an append-only registry (equal names intern to ONE
\ id), the private RAW>SCHEMA-ID / SCHEMA-ID>RAW refinements stay owner-only, and
\ the ID>WIRE / WIRE>ID pair is the fixed-width wire codec (total encode; audited
\ fail-closed decode). This is a DIFFERENT concern from maki/evidence/schema.f
\ (package EVID), which owns the artifact-indexed evidence-bundle presence schema;
\ this file owns schema-DEFINITION identity, package SCHEMA (protocol op
\ SCHEMA:LIST lands on this package later, § 23.9 machine-facing action registry).
\
\ ENGINEERING DECISION - the schema-definition canonical grammar (§ 23.9 NEEDS-
\ DECISION, resolved in-dot from real usage). A schema-id interns the canonical,
\ VERSION-INDEPENDENT schema NAME, not a re-encoded field-set grammar. Grounding:
\   1. What consumes schema-id today: maki/evidence/policy.f threads it OPAQUELY as
\      the gate-set / granted `pol` field - it names the promotion-policy schema
\      and never reads a field set through the id. The schema's field-set grammar
\      is already expressed as a CHECKED TYPED PRODUCT (POLICY:gate-set) the checker
\      enforces; re-encoding that grammar as interned bytes would duplicate a
\      checker-owned shape with no consumer.
\   2. The envelope splits schema-id from schema-version (§ 23.9 Canonical typed
\      artifacts: `schema-id, schema-version, kind`), exactly as producer-id splits
\      from producer-version. schema-id is therefore the VERSION-INDEPENDENT identity
\      of a schema definition; the grammar EVOLVES with versions, so folding the
\      grammar into the id would make it version-DEPENDENT and break that split. The
\      versioned grammar lives behind schema-version; schema-id is the schema NAME.
\ So the interned content is the canonical schema name bytes; equal names are one id.
\
\ WIRE FORMS. Two audited public codecs share the private RAW>SCHEMA-ID refinement:
\   - ID>WIRE / WIRE>ID: the PROCESS-LOCAL registry raw as a fixed-width 8-byte
\     little-endian scalar. Admissible only for intra-process wire paths (e.g.
\     maki/db/diagnostic.f bundles that never leave the process); NEVER for a
\     digest-covered, cross-process, or durable identity (a raw index means a
\     different descriptor in another process).
\   - KEY>WIRE / WIRE>KEY: the CROSS-PROCESS content key - SHA-256 over the
\     interned canonical schema-name bytes, fixed 32-byte little-endian (§ 23.9
\     origin-class table, content-addressed registry intern row). Equal names
\     digest identically in every process, so this form survives process death:
\     WIRE>KEY resolves the 32 bytes against the local registry by CONTENT, never
\     by registration order. This is the digest-covered / durable wire form the
\     envelope and transaction codecs consume.
\ The per-id content key is interned alongside the name at REGISTER (SCH-CK), so it
\ costs one SHA-256 per fresh registration and a content-key column, never a rehash
\ on the wire path.
\
\ Fail closed: an empty/oversized name, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ schema owns -5330..-5333.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:schema-id nominal identity

-5330 constant E-SCHEMA-WIRE     \ ID>WIRE output buffer smaller than the fixed wire width
-5331 constant E-SCHEMA-CAP      \ registry count or name-store bytes exhausted
-5332 constant E-SCHEMA-KEY      \ empty schema name or name over the per-name byte cap
-5333 constant E-SCHEMA-ID       \ schema-id outside the registered range

package SCHEMA
public

\ WIRE>ID decode result (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ refined nominal id in its `id` field; the reject arms are the fixed-width
\ byte-decode refusals the contract names (wrong width, unresolved/out-of-range
\ raw). A bespoke per-package sum, not result<a,b>, so a total ok construction
\ leaves no free error variable. Declared through the unified ENUM front end in full
\ mode (the arity after the name selects it), so the payload is a named FIELD rather
\ than a positional one; the generated SCHEMA-ID--RESULT:OK / :WRONG-WIDTH /
\ :UNKNOWN constructors and every MATCH site are unchanged, because both spellings
\ and payload binding order derive from the package, the family tail, and the
\ declaration order, none of which the mode changes.
ENUM id-result 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

256 constant SCH-CAP                      \ max distinct registered schema definitions (v1)
256 constant SCH-KEY-MAX                    \ max bytes per canonical schema name
SCH-CAP SCH-KEY-MAX * constant SCH-KEY-CAP
8 constant WIRE-BYTES                        \ fixed little-endian wire width of the registry raw
32 constant CK-BYTES                         \ SHA-256 content-key width (cross-process wire form)

create SCH-KEYS SCH-KEY-CAP allot            \ interned name bytes, back to back
create SCH-KO   SCH-CAP cells allot           \ per-id name offset into SCH-KEYS
create SCH-KL   SCH-CAP cells allot           \ per-id name length
create SCH-CK   SCH-CAP CK-BYTES * allot      \ per-id SHA-256 content key over the name bytes
variable SCH-KEY-U                             \ bytes used in SCH-KEYS
variable SCH-N                                  \ registered count

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public
\ producer is REGISTER, bound to a real name registration, so a raw n cannot forge
\ a schema identity.
TRUSTED: RAW>SCHEMA-ID ( n -- CAD-KIND:schema-id ) ;
TRUSTED: SCHEMA-ID>RAW ( CAD-KIND:schema-id -- n ) ;

: ID-CK ( CAD-KIND:schema-id -- n )
   SCHEMA-ID>RAW dup 0 < over SCH-N @ >= or if E-SCHEMA-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u SCH-KEY-MAX > or if E-SCHEMA-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored name bytes for a validated raw id
   SCH-KEYS raw cells SCH-KO + @ +  raw cells SCH-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered name, or -1
   SCH-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: CK@ ( n -- ptr u8 )   CK-BYTES * SCH-CK + ;   \ per-id content-key slot base

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   SCH-KEY-U @ u + SCH-KEY-CAP > if E-SCHEMA-CAP throw then
   SCH-KEY-U @ {: off:n :}
   a SCH-KEYS off + u BYTE-COPY
   off raw cells SCH-KO + !
   u   raw cells SCH-KL + !
   a u  raw CK@  SHA256                        \ intern the cross-process content key
   off u + SCH-KEY-U ! ;

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}   \ fixed 32-byte content-key compare
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}   \ raw id whose content key equals p's 32 bytes, or -1
   SCH-N @ 0 ?do
      i CK@ p CK-EQ? if i unloop exit then
   loop -1 ;

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

: R-OK ( a -- id-result<a> )          SCHEMA-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   SCHEMA-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       SCHEMA-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns a schema by its canonical name: an equal name returns the same
\ id (content-addressed), a fresh name appends a new registry slot and mints its id.
\ The ONLY authority-bearing public mint, bound to a real name, never a raw cast.
: REGISTER ( ptr u8 n -- CAD-KIND:schema-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>SCHEMA-ID exit then
   SCH-N @ SCH-CAP >= if E-SCHEMA-CAP throw then
   SCH-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ SCH-N !
   raw RAW>SCHEMA-ID ;

\ NAME$ projects a schema id back to its registered canonical name.
: NAME$ ( CAD-KIND:schema-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-schema identity: interning makes equal names share one raw id, so
\ raw equality is exactly name equality.
: EQUAL? ( CAD-KIND:schema-id CAD-KIND:schema-id -- bool )
   {: x:CAD-KIND:schema-id y:CAD-KIND:schema-id :}
   x SCHEMA-ID>RAW y SCHEMA-ID>RAW = ;

: VALIDATE ( CAD-KIND:schema-id -- CAD-KIND:schema-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-SCHEMA-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED against the
\ append-only registry, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:schema-id ptr u8 n -- n )
   {: id:CAD-KIND:schema-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-SCHEMA-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:schema-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw SCH-N @ >= or if R-UNKNOWN exit then
   raw RAW>SCHEMA-ID R-OK ;

\ KEY>WIRE writes the id's cross-process content key (SHA-256 over the interned name,
\ 32 fixed bytes) into the caller buffer; total for a valid id (E-SCHEMA-WIRE if the
\ cap cannot hold CK-BYTES). WIRE>KEY is the audited cross-process boundary: it reads
\ the 32-byte content key and resolves it against the local registry BY CONTENT
\ (never by registration order), refining to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:schema-id ptr u8 n -- n )
   {: id:CAD-KIND:schema-id out:ptr cap:n :}
   cap CK-BYTES < if E-SCHEMA-WIRE throw then
   id ID-CK CK@  out  CK-BYTES  BYTE-COPY
   CK-BYTES ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:schema-id> )
   {: a:ptr u:n :}
   u CK-BYTES <> if R-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if R-UNKNOWN exit then
   raw RAW>SCHEMA-ID R-OK ;

: COUNT ( -- n )  SCH-N @ ;

;package
