\ maki/rev.f - the revision identity registry + wire codec (the rev-id leg of the
\ MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors and wire codecs"
\ per-family contract; dot habu-v2-txn-journal-d0bc644f).
\
\ CONCERN: the content-addressed owner of CAD-KIND:rev-id. A revision is minted at
\ COMMIT and content-addressed by its CANONICAL REVISION CONTENT (§ 23.9 origin-class
\ table + rev-id row: "minted at COMMIT; content-addressed by the canonical revision
\ content (parent + write set) so deterministic replay reproduces identical
\ rev-ids"). COMMIT interns that canonical content byte string into an append-only
\ registry (equal content interns to ONE id), the private RAW>REV-ID / REV-ID>RAW
\ refinements stay owner-only, and the ID>WIRE / WIRE>ID pair is the fixed-width wire
\ codec (total encode; audited fail-closed decode).
\
\ REVISION-CONTENT CANONICAL FORM (§ 23.9 rev-id NEEDS-DECISION, "revision-content
\ canonical form (engineering; part of the TX dot)"). This leg owns rev-id IDENTITY;
\ it interns whatever canonical revision-content BYTES the committer supplies, exactly
\ as maki/config.f interns a caller-canonicalised config-fact string. The concrete
\ (parent + write set) serialisation is produced by the transaction commit path
\ (maki/db/transaction.f TX:PROPOSE), which owns the transaction data model and is the
\ sole consumer that knows a revision's parent and write set. Splitting it this way
\ keeps ONE concern per file: rev-id identity here, revision content there.
\ Determinism: equal (parent + write set) -> equal canonical bytes -> COMMIT returns
\ one rev-id, so replaying the same transaction reproduces the same revision id.
\
\ WIRE FORM (this round): the process-local registry raw as a fixed-width 8-byte
\ little-endian scalar, matching the landed maki/target/target.f, maki/numpolicy.f,
\ maki/schema.f, maki/producer.f, and maki/config.f precedents and the
\ maki/db/artifact.f id-on-wire. The § 23.9 origin-class table names the cross-process
\ content key (SHA-256, 32 bytes) as the eventual form; the plan marks reconciling the
\ process-local raw with that content key OUT OF SCOPE for this contract round (the
\ envelope implementation dot owns the migration).
\
\ Fail closed: an empty/oversized content string, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ rev owns -5346..-5349.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:rev-id nominal identity

-5346 constant E-REV-WIRE        \ ID>WIRE output buffer smaller than the fixed wire width
-5347 constant E-REV-CAP         \ registry count or content-store bytes exhausted
-5348 constant E-REV-KEY         \ empty revision-content string or content over the per-rev byte cap
-5349 constant E-REV-ID          \ rev-id outside the registered range

package REV
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

256 constant REV-CAP                       \ max distinct committed revisions (v1)
256 constant REV-KEY-MAX                     \ max bytes per canonical revision-content string
REV-CAP REV-KEY-MAX * constant REV-KEY-CAP
8 constant WIRE-BYTES                         \ fixed little-endian wire width of the registry raw

create REV-KEYS REV-KEY-CAP allot            \ interned revision-content bytes, back to back
create REV-KO   REV-CAP cells allot           \ per-id content offset into REV-KEYS
create REV-KL   REV-CAP cells allot           \ per-id content length
variable REV-KEY-U                             \ bytes used in REV-KEYS
variable REV-N                                  \ registered count

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public
\ producer is COMMIT, bound to a real revision-content registration, so a raw n cannot
\ forge a revision identity.
TRUSTED: RAW>REV-ID ( n -- CAD-KIND:rev-id ) ;
TRUSTED: REV-ID>RAW ( CAD-KIND:rev-id -- n ) ;

: ID-CK ( CAD-KIND:rev-id -- n )
   REV-ID>RAW dup 0 < over REV-N @ >= or if E-REV-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u REV-KEY-MAX > or if E-REV-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored content bytes for a validated raw id
   REV-KEYS raw cells REV-KO + @ +  raw cells REV-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered content, or -1
   REV-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   REV-KEY-U @ u + REV-KEY-CAP > if E-REV-CAP throw then
   REV-KEY-U @ {: off:n :}
   a REV-KEYS off + u BYTE-COPY
   off raw cells REV-KO + !
   u   raw cells REV-KL + !
   off u + REV-KEY-U ! ;

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

: R-OK ( a -- id-result<a> )          REV-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   REV-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       REV-ID--RESULT:UNKNOWN ;

public

\ COMMIT interns a revision by its canonical revision-content bytes: equal content
\ returns the same id (content-addressed / deterministic replay), fresh content
\ appends a new registry slot and mints its id. The ONLY authority-bearing public
\ mint, bound to a real content registration, never a raw cast.
: COMMIT ( ptr u8 n -- CAD-KIND:rev-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>REV-ID exit then
   REV-N @ REV-CAP >= if E-REV-CAP throw then
   REV-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ REV-N !
   raw RAW>REV-ID ;

\ CONTENT$ projects a rev id back to its registered canonical revision content.
: CONTENT$ ( CAD-KIND:rev-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-revision identity: interning makes equal content share one raw id,
\ so raw equality is exactly content equality.
: EQUAL? ( CAD-KIND:rev-id CAD-KIND:rev-id -- bool )
   {: x:CAD-KIND:rev-id y:CAD-KIND:rev-id :}
   x REV-ID>RAW y REV-ID>RAW = ;

: VALIDATE ( CAD-KIND:rev-id -- CAD-KIND:rev-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-REV-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED against the
\ append-only registry, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:rev-id ptr u8 n -- n )
   {: id:CAD-KIND:rev-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-REV-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:rev-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw REV-N @ >= or if R-UNKNOWN exit then
   raw RAW>REV-ID R-OK ;

: COUNT ( -- n )  REV-N @ ;

;package
