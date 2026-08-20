\ maki/db/evidence.f - the evidence-descriptor identity registry + wire codec (the
\ evidence-id leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors and
\ wire codecs" per-family contract; dot habu-v2-evidence-applicability-73ac58b9).
\
\ CONCERN: the content-addressed owner of CAD-KIND:evidence-id. REGISTER interns the
\ canonical evidence DESCRIPTOR bytes into an append-only registry (equal descriptors
\ intern to ONE id), the private RAW>EVIDENCE-ID / EVIDENCE-ID>RAW refinements stay
\ owner-only, and the ID>WIRE / WIRE>ID pair is the fixed-width wire codec (total encode;
\ audited fail-closed decode). This is a DIFFERENT concern from the proof-EVIDENCE VALUE
\ (package OBLIG, maki/db/obligation.f: subject/domain/relation/environment/verifier) and
\ from the artifact-indexed evidence CLASS families (package EVID, maki/evidence/): this
\ file owns only the durable IDENTITY of an evidence descriptor, not its structure.
\
\ WHAT CANONICALLY IDENTIFIES AN EVIDENCE (§ 23.9 origin-class table, content-addressed
\ registry intern row). An evidence-id interns the canonical, deterministically-ordered
\ SERIALIZATION of the evidence descriptor as a byte string - whatever the evidence
\ producer emits as the evidence's canonical form (e.g. the discharge-relevant axes:
\ subject / proof domain / claimed relation / environment / verifier identity + class).
\ Equal descriptors share one id. This leg fixes the OWNERSHIP BOUNDARY and the identity
\ mechanism, not a frozen descriptor field set: the concrete descriptor vocabulary is the
\ evidence producer's concern (the maki/config.f open-fact-vocabulary precedent). The
\ diagnostic IR's `invalidated-evidence[]` field (maki/db/diagnostic.f), held as strings
\ pending this owner, promotes to CAD-KIND:evidence-id through REGISTER / WIRE>ID.
\
\ WIRE FORMS. Two audited public codecs share the private RAW>EVIDENCE-ID refinement:
\   - ID>WIRE / WIRE>ID: the PROCESS-LOCAL registry raw as a fixed-width 8-byte
\     little-endian scalar. Admissible only for intra-process wire paths (e.g.
\     maki/db/diagnostic.f bundles that never leave the process); NEVER for a
\     digest-covered, cross-process, or durable identity.
\   - KEY>WIRE / WIRE>KEY: the CROSS-PROCESS content key - SHA-256 over the interned
\     canonical descriptor bytes, fixed 32-byte little-endian. Equal descriptors digest
\     identically in every process, so this form survives process death: WIRE>KEY resolves
\     the 32 bytes against the local registry BY CONTENT, never by registration order. The
\     per-id content key is interned at REGISTER (EVR-CK).
\
\ Fail closed: an empty/oversized descriptor, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ evidence owns -5366..-5369.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:evidence-id nominal identity

-5366 constant E-EVIDENCE-WIRE     \ ID>WIRE / KEY>WIRE output buffer smaller than the fixed wire width
-5367 constant E-EVIDENCE-CAP      \ registry count or descriptor-store bytes exhausted
-5368 constant E-EVIDENCE-KEY      \ empty evidence descriptor or descriptor over the per-descriptor byte cap
-5369 constant E-EVIDENCE-ID       \ evidence-id outside the registered range

package EVIDENCE
public

\ WIRE>ID / WIRE>KEY decode result (the § 23.9 art-result custom-sum idiom): `ok`
\ carries the refined nominal id in its `id` field; the reject arms are the
\ fixed-width byte-decode refusals the contract names (wrong width,
\ unresolved/out-of-range raw). A bespoke per-package sum, not result<a,b>, so a total
\ ok construction leaves no free error variable. Declared through the unified ENUM
\ front end in full mode (the arity after the name selects it), so the payload is a
\ named FIELD rather than a positional one; the generated EVIDENCE-ID--RESULT:OK /
\ :WRONG-WIDTH / :UNKNOWN constructors and every MATCH site are unchanged, because
\ both spellings and payload binding order derive from the package, the family tail,
\ and the declaration order, none of which the mode changes.
ENUM id-result 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

256 constant EVR-CAP                      \ max distinct registered evidence descriptors (v1)
256 constant EVR-KEY-MAX                    \ max bytes per canonical evidence descriptor
EVR-CAP EVR-KEY-MAX * constant EVR-KEY-CAP
8 constant WIRE-BYTES                        \ fixed little-endian wire width of the registry raw
32 constant CK-BYTES                         \ SHA-256 content-key width (cross-process wire form)

create EVR-KEYS EVR-KEY-CAP allot            \ interned descriptor bytes, back to back
create EVR-KO   EVR-CAP cells allot           \ per-id descriptor offset into EVR-KEYS
create EVR-KL   EVR-CAP cells allot           \ per-id descriptor length
create EVR-CK   EVR-CAP CK-BYTES * allot      \ per-id SHA-256 content key over the descriptor bytes
variable EVR-KEY-U                             \ bytes used in EVR-KEYS
variable EVR-N                                  \ registered count

\ Private representation refinements (owner-only, never public), the
\ maki/producer.f RAW>PRODUCER-ID / PRODUCER-ID>RAW precedent. The only public
\ producer is REGISTER, bound to a real descriptor registration, so a raw n cannot
\ forge an evidence identity.
\ Both rows stay trust rows, for two different reasons. CAST: refuses the mint
\ with 7135 E-CAST-OWNER because package CAD-KIND declares the family. The
\ projection out would certify, but its NAME repeats its owner, so the package
\ lint refuses any edit to that line until it is renamed - a cascade owned by
\ habu-cast-definer-330-1f5980b8, not by this file.
TRUSTED: RAW>EVIDENCE-ID ( n -- CAD-KIND:evidence-id ) ;
TRUSTED: EVIDENCE-ID>RAW ( CAD-KIND:evidence-id -- n ) ;

: ID-CK ( CAD-KIND:evidence-id -- n )
   EVIDENCE-ID>RAW dup 0 < over EVR-N @ >= or if E-EVIDENCE-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u EVR-KEY-MAX > or if E-EVIDENCE-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored descriptor bytes for a validated raw id
   EVR-KEYS raw cells EVR-KO + @ +  raw cells EVR-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered descriptor, or -1
   EVR-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: CK@ ( n -- ptr u8 )   CK-BYTES * EVR-CK + ;   \ per-id content-key slot base

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   EVR-KEY-U @ u + EVR-KEY-CAP > if E-EVIDENCE-CAP throw then
   EVR-KEY-U @ {: off:n :}
   a EVR-KEYS off + u BYTE-COPY
   off raw cells EVR-KO + !
   u   raw cells EVR-KL + !
   a u  raw CK@  SHA256                        \ intern the cross-process content key
   off u + EVR-KEY-U ! ;

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}   \ fixed 32-byte content-key compare
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}   \ raw id whose content key equals p's 32 bytes, or -1
   EVR-N @ 0 ?do
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

: R-OK ( a -- id-result<a> )          EVIDENCE-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   EVIDENCE-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       EVIDENCE-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns an evidence by its canonical descriptor: an equal descriptor returns
\ the same id (content-addressed), a fresh descriptor appends a new registry slot and
\ mints its id. The ONLY authority-bearing public mint, bound to a real descriptor
\ registration, never a raw cast.
: REGISTER ( ptr u8 n -- CAD-KIND:evidence-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>EVIDENCE-ID exit then
   EVR-N @ EVR-CAP >= if E-EVIDENCE-CAP throw then
   EVR-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ EVR-N !
   raw RAW>EVIDENCE-ID ;

\ DESCRIPTOR$ projects an evidence id back to its registered canonical descriptor.
: DESCRIPTOR$ ( CAD-KIND:evidence-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-evidence identity: interning makes equal descriptors share one raw id,
\ so raw equality is exactly descriptor equality.
: EQUAL? ( CAD-KIND:evidence-id CAD-KIND:evidence-id -- bool )
   {: x:CAD-KIND:evidence-id y:CAD-KIND:evidence-id :}
   x EVIDENCE-ID>RAW y EVIDENCE-ID>RAW = ;

: VALIDATE ( CAD-KIND:evidence-id -- CAD-KIND:evidence-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-EVIDENCE-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED against the
\ append-only registry, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:evidence-id ptr u8 n -- n )
   {: id:CAD-KIND:evidence-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-EVIDENCE-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:evidence-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw EVR-N @ >= or if R-UNKNOWN exit then
   raw RAW>EVIDENCE-ID R-OK ;

\ KEY>WIRE writes the id's cross-process content key (SHA-256 over the interned descriptor,
\ 32 fixed bytes); total for a valid id (E-EVIDENCE-WIRE if the cap cannot hold CK-BYTES).
\ WIRE>KEY is the audited cross-process boundary: it reads the 32-byte content key and
\ resolves it against the local registry BY CONTENT (never by registration order), refining
\ to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:evidence-id ptr u8 n -- n )
   {: id:CAD-KIND:evidence-id out:ptr cap:n :}
   cap CK-BYTES < if E-EVIDENCE-WIRE throw then
   id ID-CK CK@  out  CK-BYTES  BYTE-COPY
   CK-BYTES ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:evidence-id> )
   {: a:ptr u:n :}
   u CK-BYTES <> if R-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if R-UNKNOWN exit then
   raw RAW>EVIDENCE-ID R-OK ;

: COUNT ( -- n )  EVR-N @ ;

;package
