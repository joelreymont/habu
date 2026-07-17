\ maki/producer.f - the producer identity registry + wire codec (the producer-id
\ leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors and wire
\ codecs" per-family contract; dot habu-producer-producer-id-5e016e1f).
\
\ CONCERN: the content-addressed owner of CAD-KIND:producer-id. REGISTER interns a
\ canonical producer NAME into an append-only registry (equal names intern to ONE
\ id), the private RAW>PRODUCER-ID / PRODUCER-ID>RAW refinements stay owner-only,
\ and the ID>WIRE / WIRE>ID pair is the fixed-width wire codec (total encode;
\ audited fail-closed decode). Package PRODUCER is the machine-facing action /
\ component registry the plan names (§ 23.9); the callable-action schema surface
\ lands on this package later.
\
\ ENGINEERING DECISION - what canonically identifies a producer (§ 23.9 NEEDS-
\ DECISION, resolved in-dot from the plan's provenance sections). A producer-id
\ interns the canonical, VERSION-INDEPENDENT producer NAME - a stable, namespaced
\ machine-facing component identifier - and NOTHING else. Grounding:
\   1. The envelope splits producer-id from producer-version (§ 23.9 Canonical typed
\      artifacts: `producer-id, producer-version`; § 9.1 "producer/pass version when
\      derived"), exactly as schema-id splits from schema-version. The identity is
\      therefore VERSION-INDEPENDENT: the version is a separate digest-covered field,
\      never folded into the id.
\   2. A producer is a "producing component" / callable action in the machine-facing
\      registry (§ 23.9); the replaceable agents and passes (search, rewrite, repair,
\      ranking) operate through the kernel. What DISTINGUISHES two producers is the
\      stable component identity - a name. The NEEDS-DECISION floats "name + class?":
\      the envelope has NO separate producer-class field, so any class distinction
\      (agent vs pass vs verifier vs importer) MUST fold into the identity. A
\      NAMESPACED name carries it (e.g. `pass/lowering`, `agent/search`) with no
\      consumer decomposing class through the id, so a separate class field would be
\      unmotivated structure. The minimal descriptor the plan implies is a stable,
\      namespaced producer name; the version rides the separate envelope field.
\ So the interned content is the canonical producer name bytes; equal names are one id.
\
\ WIRE FORMS. Two audited public codecs share the private RAW>PRODUCER-ID refinement:
\   - ID>WIRE / WIRE>ID: the PROCESS-LOCAL registry raw as a fixed-width 8-byte
\     little-endian scalar. Admissible only for intra-process wire paths (e.g.
\     maki/db/diagnostic.f bundles that never leave the process); NEVER for a
\     digest-covered, cross-process, or durable identity.
\   - KEY>WIRE / WIRE>KEY: the CROSS-PROCESS content key - SHA-256 over the interned
\     canonical producer-name bytes, fixed 32-byte little-endian (§ 23.9 origin-class
\     table, content-addressed registry intern row). Equal names digest identically in
\     every process, so this form survives process death: WIRE>KEY resolves the 32
\     bytes against the local registry by CONTENT, never by registration order. This
\     is the digest-covered / durable wire form the envelope and transaction codecs
\     consume. The per-id content key is interned at REGISTER (PRD-CK).
\
\ Fail closed: an empty/oversized name, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ producer owns -5334..-5337.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:producer-id nominal identity

-5334 constant E-PRODUCER-WIRE     \ ID>WIRE output buffer smaller than the fixed wire width
-5335 constant E-PRODUCER-CAP      \ registry count or name-store bytes exhausted
-5336 constant E-PRODUCER-KEY      \ empty producer name or name over the per-name byte cap
-5337 constant E-PRODUCER-ID       \ producer-id outside the registered range

package PRODUCER
public

\ WIRE>ID decode result (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ refined nominal id; the reject arms are the fixed-width byte-decode refusals the
\ contract names (wrong width, unresolved/out-of-range raw). A bespoke per-package
\ sum, not result<a,b>, so a total ok construction leaves no free error variable.
SUMTYPE id-result 1
   VARIANT ok a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;SUMTYPE

private

256 constant PRD-CAP                      \ max distinct registered producers (v1)
256 constant PRD-KEY-MAX                    \ max bytes per canonical producer name
PRD-CAP PRD-KEY-MAX * constant PRD-KEY-CAP
8 constant WIRE-BYTES                        \ fixed little-endian wire width of the registry raw
32 constant CK-BYTES                         \ SHA-256 content-key width (cross-process wire form)

create PRD-KEYS PRD-KEY-CAP allot            \ interned name bytes, back to back
create PRD-KO   PRD-CAP cells allot           \ per-id name offset into PRD-KEYS
create PRD-KL   PRD-CAP cells allot           \ per-id name length
create PRD-CK   PRD-CAP CK-BYTES * allot      \ per-id SHA-256 content key over the name bytes
variable PRD-KEY-U                             \ bytes used in PRD-KEYS
variable PRD-N                                  \ registered count

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public
\ producer is REGISTER, bound to a real name registration, so a raw n cannot forge
\ a producer identity.
TRUSTED: RAW>PRODUCER-ID ( n -- CAD-KIND:producer-id ) ;
TRUSTED: PRODUCER-ID>RAW ( CAD-KIND:producer-id -- n ) ;

: ID-CK ( CAD-KIND:producer-id -- n )
   PRODUCER-ID>RAW dup 0 < over PRD-N @ >= or if E-PRODUCER-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u PRD-KEY-MAX > or if E-PRODUCER-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored name bytes for a validated raw id
   PRD-KEYS raw cells PRD-KO + @ +  raw cells PRD-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered name, or -1
   PRD-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: CK@ ( n -- ptr u8 )   CK-BYTES * PRD-CK + ;   \ per-id content-key slot base

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   PRD-KEY-U @ u + PRD-KEY-CAP > if E-PRODUCER-CAP throw then
   PRD-KEY-U @ {: off:n :}
   a PRD-KEYS off + u BYTE-COPY
   off raw cells PRD-KO + !
   u   raw cells PRD-KL + !
   a u  raw CK@  SHA256                        \ intern the cross-process content key
   off u + PRD-KEY-U ! ;

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}   \ fixed 32-byte content-key compare
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}   \ raw id whose content key equals p's 32 bytes, or -1
   PRD-N @ 0 ?do
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

: R-OK ( a -- id-result<a> )          PRODUCER-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   PRODUCER-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       PRODUCER-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns a producer by its canonical name: an equal name returns the same
\ id (content-addressed), a fresh name appends a new registry slot and mints its id.
\ The ONLY authority-bearing public mint, bound to a real name, never a raw cast.
: REGISTER ( ptr u8 n -- CAD-KIND:producer-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>PRODUCER-ID exit then
   PRD-N @ PRD-CAP >= if E-PRODUCER-CAP throw then
   PRD-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ PRD-N !
   raw RAW>PRODUCER-ID ;

\ NAME$ projects a producer id back to its registered canonical name.
: NAME$ ( CAD-KIND:producer-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-producer identity: interning makes equal names share one raw id, so
\ raw equality is exactly name equality.
: EQUAL? ( CAD-KIND:producer-id CAD-KIND:producer-id -- bool )
   {: x:CAD-KIND:producer-id y:CAD-KIND:producer-id :}
   x PRODUCER-ID>RAW y PRODUCER-ID>RAW = ;

: VALIDATE ( CAD-KIND:producer-id -- CAD-KIND:producer-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-PRODUCER-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED against the
\ append-only registry, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:producer-id ptr u8 n -- n )
   {: id:CAD-KIND:producer-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-PRODUCER-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:producer-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw PRD-N @ >= or if R-UNKNOWN exit then
   raw RAW>PRODUCER-ID R-OK ;

\ KEY>WIRE writes the id's cross-process content key (SHA-256 over the interned name,
\ 32 fixed bytes); total for a valid id (E-PRODUCER-WIRE if the cap cannot hold
\ CK-BYTES). WIRE>KEY is the audited cross-process boundary: it reads the 32-byte
\ content key and resolves it against the local registry BY CONTENT (never by
\ registration order), refining to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:producer-id ptr u8 n -- n )
   {: id:CAD-KIND:producer-id out:ptr cap:n :}
   cap CK-BYTES < if E-PRODUCER-WIRE throw then
   id ID-CK CK@  out  CK-BYTES  BYTE-COPY
   CK-BYTES ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:producer-id> )
   {: a:ptr u:n :}
   u CK-BYTES <> if R-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if R-UNKNOWN exit then
   raw RAW>PRODUCER-ID R-OK ;

: COUNT ( -- n )  PRD-N @ ;

;package
