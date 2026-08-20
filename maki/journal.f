\ maki/journal.f - the append-only audit journal + audit-event-id wire codec (the
\ audit-event-id leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity
\ constructors and wire codecs" per-family contract; dot habu-v2-txn-journal-d0bc644f).
\
\ CONCERN: the append-only owner of CAD-KIND:audit-event-id. Unlike the five
\ content-addressed identity families (schema/producer/config/numeric-policy/target,
\ where equal content interns to ONE id), an audit event is OCCURRENCE-identified:
\ APPEND ALWAYS mints the NEXT monotonic sequence id and stores its descriptor, so
\ two appends of an identical descriptor are DISTINCT events (§ 23.9 origin-class
\ table: "the journal-assigned monotonic sequence number ... occurrence-identified,
\ never content-addressed"). The private RAW>AUDIT-EVENT-ID / AUDIT-EVENT-ID>RAW
\ refinements stay owner-only, and the ID>WIRE / WIRE>ID pair is the fixed-width wire
\ codec (total encode; audited fail-closed decode). Package JOURNAL is the
\ persistence surface the plan names (§ 23.9 "append-only audit records and
\ deterministic replay"); the object-store / replay surface lands on it later.
\
\ WIRE FORM (§ 23.9 origin-class table, append-only sequence): the journal-assigned
\ monotonic sequence number as a fixed-width 8-byte little-endian scalar. The raw IS
\ the sequence, so - unlike the content-addressed families whose eventual form is a
\ 32-byte SHA-256 content key - this 8-byte sequence IS the canonical wire form, and
\ audit-event-id is DIGEST-EXCLUDED (maki/cad-kinds.f, maki/db/artifact.f TAG-EVENT):
\ re-recording the same artifact under a new audit event never changes its digest.
\
\ audit-event-id is a PERSISTENT provenance identity and must never unify with the
\ EPHEMERAL runtime synchronization event ADAG:event-id (maki/async-dag.f); the
\ nominal separation is pinned by maki/cad-kinds-test.f CK-EV-X1/CK-EV-X2.
\
\ Fail closed: an empty/oversized descriptor, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ journal owns -5342..-5345.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:audit-event-id nominal identity

-5342 constant E-JOURNAL-WIRE    \ ID>WIRE output buffer smaller than the fixed wire width
-5343 constant E-JOURNAL-CAP     \ journal count or descriptor-store bytes exhausted
-5344 constant E-JOURNAL-KEY     \ empty event descriptor or descriptor over the per-event byte cap
-5345 constant E-JOURNAL-ID      \ audit-event-id outside the appended range

package JOURNAL
public

\ WIRE>ID decode result (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ refined nominal id in its `id` field; the reject arms are the fixed-width
\ byte-decode refusals the contract names (wrong width, unresolved/out-of-range
\ sequence). A bespoke per-package sum, not result<a,b>, so a total ok construction
\ leaves no free error variable. Declared through the unified ENUM front end in
\ full mode (the arity after the name selects it), so the payload is a named FIELD
\ rather than a positional one; the generated JOURNAL-ID--RESULT:OK /
\ :WRONG-WIDTH / :UNKNOWN constructors and every MATCH site are unchanged, because
\ both spellings and payload binding order derive from the package, the family
\ tail, and the declaration order, none of which the mode changes.
ENUM id-result 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

256 constant JRN-CAP                       \ max distinct appended audit events (v1)
256 constant JRN-KEY-MAX                     \ max bytes per event descriptor
JRN-CAP JRN-KEY-MAX * constant JRN-KEY-CAP
8 constant WIRE-BYTES                         \ fixed little-endian wire width of the sequence

create JRN-KEYS JRN-KEY-CAP allot            \ appended descriptor bytes, back to back
create JRN-KO   JRN-CAP cells allot           \ per-event descriptor offset into JRN-KEYS
create JRN-KL   JRN-CAP cells allot           \ per-event descriptor length
variable JRN-KEY-U                             \ bytes used in JRN-KEYS
variable JRN-N                                  \ appended count == next sequence

\ Private representation refinements (owner-only, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public
\ producer is APPEND, bound to a real journal append, so a raw n cannot forge an
\ audit-event identity.
\ The mint stays a trust row: CAST: refuses it with 7135 E-CAST-OWNER because
\ package CAD-KIND declares the family. The projection out is a checked cast.
TRUSTED: RAW>AUDIT-EVENT-ID ( n -- CAD-KIND:audit-event-id ) ;
CAST: AUDIT-EVENT-ID>RAW ( CAD-KIND:audit-event-id -- n )

: ID-CK ( CAD-KIND:audit-event-id -- n )
   AUDIT-EVENT-ID>RAW dup 0 < over JRN-N @ >= or if E-JOURNAL-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u JRN-KEY-MAX > or if E-JOURNAL-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored descriptor bytes for a validated raw id
   JRN-KEYS raw cells JRN-KO + @ +  raw cells JRN-KL + @ ;

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   JRN-KEY-U @ u + JRN-KEY-CAP > if E-JOURNAL-CAP throw then
   JRN-KEY-U @ {: off:n :}
   a JRN-KEYS off + u BYTE-COPY
   off raw cells JRN-KO + !
   u   raw cells JRN-KL + !
   off u + JRN-KEY-U ! ;

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

: R-OK ( a -- id-result<a> )          JOURNAL-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   JOURNAL-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       JOURNAL-ID--RESULT:UNKNOWN ;

public

\ APPEND records an audit event by its descriptor and mints the NEXT monotonic
\ sequence id (occurrence-identified: an identical descriptor appended twice yields
\ two DISTINCT ids, never one). The ONLY authority-bearing public mint, bound to a
\ real journal append, never a raw cast.
: APPEND ( ptr u8 n -- CAD-KIND:audit-event-id ) {: a:ptr u:n :}
   a u KEY-CK
   JRN-N @ JRN-CAP >= if E-JOURNAL-CAP throw then
   JRN-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ JRN-N !
   raw RAW>AUDIT-EVENT-ID ;

\ DESC$ projects an audit-event id back to its recorded descriptor.
: DESC$ ( CAD-KIND:audit-event-id -- ptr u8 n )  ID-CK KEY@ ;

\ SEQ is the monotonic sequence number the event was assigned (the raw).
: SEQ ( CAD-KIND:audit-event-id -- n )  ID-CK ;

\ EQUAL? is same-occurrence identity: each append mints a fresh sequence, so raw
\ equality is exactly same-event identity.
: EQUAL? ( CAD-KIND:audit-event-id CAD-KIND:audit-event-id -- bool )
   {: x:CAD-KIND:audit-event-id y:CAD-KIND:audit-event-id :}
   x AUDIT-EVENT-ID>RAW y AUDIT-EVENT-ID>RAW = ;

: VALIDATE ( CAD-KIND:audit-event-id -- CAD-KIND:audit-event-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-JOURNAL-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the sequence FAIL-CLOSED against
\ the appended range, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:audit-event-id ptr u8 n -- n )
   {: id:CAD-KIND:audit-event-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-JOURNAL-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:audit-event-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw JRN-N @ >= or if R-UNKNOWN exit then
   raw RAW>AUDIT-EVENT-ID R-OK ;

: COUNT ( -- n )  JRN-N @ ;

;package
